(** Conditional term-rewriting system (CTRS) representation produced from an
    elaborated + simplified IL spec.

    Only the representation, the term/rule queries and symbol slicing, the
    diagnostic printer, and the shared Maude lexical layer live here; the IL ->
    CTRS translation is in {!To_ctrs}, the checker-facing pass families are
    {!Crc_surface} (analysis normalization) and {!Scc_surface}
    (over-approximation), and the order-sorted Maude module surfaces are
    {!To_maude} (execution) and {!To_mfe} (analysis, for the Maude Formal
    Environment). *)

(* A CTRS term: either a variable, or a function symbol applied to zero or more
   argument terms. A nullary application prints as a bare [id]. *)
type term = Var of string | App of string * term list

(* A condition is an equation between two terms. *)
type cond = term * term

(* A (possibly conditional) rewrite rule [lhs -> rhs | conds]. [owise] marks a
   clause that applied "otherwise" (SpecTec [ElsePr]): it fires only when no
   earlier sibling did. The Maude surfaces ({!To_maude}/{!To_mfe}) render it as
   Maude's [owise] equation attribute. *)
type rule = { lhs : term; rhs : term; conds : cond list; owise : bool }

type t = {
  vars : string list; (* every variable used, deduplicated *)
  rules : rule list;
}

(* The IL -> CTRS translation lives in {!To_ctrs}; this module is only the data
   model and printer. *)

(* -------------------------------------------------------------------------- *)
(* CTRS identifier lexical conventions.

   Scrubbing an arbitrary string into a CTRS-safe identifier lives here at the
   data model, not in the symbol-naming layer, because BOTH that layer
   ({!Ctrs_term}, which builds every rule's symbols) and the Maude surfaces
   ({!To_mfe} analysis and {!To_maude} execution) must agree on the exact
   spelling -- so the one definition sits at the layer all of them can reach. *)

(* A readable token for a non-alphanumeric character so symbolic notations keep
   distinct, legible names (e.g. [`+`] -> "plus", not the empty string). A prime
   ['] is kept as "prime" because it distinguishes sibling definitions ([f] vs
   [f'] vs [f'']) that would otherwise collide on the same symbol; backticks,
   double quotes and whitespace are dropped; truly unknown symbols become "sym". *)
let mnemonic_of_char (c : char) : string =
  match c with
  | '+' -> "plus"
  | '-' -> "minus"
  | '*' -> "star"
  | '/' -> "slash"
  | '\\' -> "backslash"
  | '<' -> "lt"
  | '>' -> "gt"
  | '=' -> "eq"
  | '!' -> "bang"
  | '?' -> "quest"
  | '&' -> "amp"
  | '|' -> "bar"
  | '^' -> "caret"
  | '~' -> "tilde"
  | '%' -> "percent"
  | '.' -> "dot"
  | ',' -> "comma"
  | ';' -> "semi"
  | ':' -> "colon"
  | '#' -> "hash"
  | '$' -> "dollar"
  | '@' -> "at"
  | '(' -> "lparen"
  | ')' -> "rparen"
  | '[' -> "lbrack"
  | ']' -> "rbrack"
  | '{' -> "lbrace"
  | '}' -> "rbrace"
  | '\'' -> "prime"
  | '`' | '"' | ' ' | '_' -> ""
  | _ -> "sym"

(* Scrub a string into a CTRS-safe identifier: maximal [A-Za-z0-9] runs are kept,
   every other character is replaced by a mnemonic token, tokens are joined with
   [_], an alphabetic lead is guaranteed, and the result is never empty. Distinct
   inputs may still collide (a known first-cut limitation). *)
let sanitize (s : string) : string =
  let is_alnum c =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
  in
  (* Accumulate completed tokens (reversed) plus the current alphanumeric run;
     [run] is committed to [tokens] whenever a non-alphanumeric breaks it. *)
  let commit run tokens = if run = "" then tokens else run :: tokens in
  let tokens, run =
    String.fold_left
      (fun (tokens, run) c ->
        if is_alnum c then (tokens, run ^ String.make 1 c)
        else
          match mnemonic_of_char c with
          | "" -> (commit run tokens, "")
          | m -> (m :: commit run tokens, ""))
      ([], "") s
  in
  let r = String.concat "_" (List.rev (commit run tokens)) in
  if r = "" then "anon"
  else if r.[0] >= '0' && r.[0] <= '9' then "c_" ^ r
  else r

let rec string_of_term = function
  | Var id -> id
  | App (id, []) -> id
  | App (id, terms) ->
      id ^ "(" ^ String.concat ", " (List.map string_of_term terms) ^ ")"

(* A single rule for debug/error messages: [lhs -> rhs], any conditions appended
   as [ | s == t, ...]. Not a surface a tool parses -- the Maude surfaces are
   {!To_maude}/{!To_mfe}; this is only for human-readable diagnostics. *)
let string_of_rule { lhs; rhs; conds; _ } =
  let head = string_of_term lhs ^ " -> " ^ string_of_term rhs in
  match conds with
  | [] -> head
  | _ ->
      head ^ " | "
      ^ String.concat ", "
          (List.map
             (fun (l, r) -> string_of_term l ^ " == " ^ string_of_term r)
             conds)

(* -------------------------------------------------------------------------- *)
(* Term/rule queries shared by the translation ({!To_ctrs}) and slicing below. *)

(* Every variable occurring in a term. *)
let rec vars_of_term = function
  | Var v -> [ v ]
  | App (_, ts) -> List.concat_map vars_of_term ts

(* Every variable occurring in a rule (lhs, rhs and conditions). *)
let vars_of_rule (r : rule) : string list =
  vars_of_term r.lhs @ vars_of_term r.rhs
  @ List.concat_map (fun (a, b) -> vars_of_term a @ vars_of_term b) r.conds

(* Occurrences of variable [v] in a term. *)
let rec count_var (v : string) = function
  | Var u -> if u = v then 1 else 0
  | App (_, ts) -> List.fold_left (fun n t -> n + count_var v t) 0 ts

(* Substitute variables by name throughout a term (parallel, by association
   list): a variable bound in [pairs] is replaced, all others are kept. *)
let rec subst (pairs : (string * term) list) (t : term) : term =
  match t with
  | Var v -> ( match List.assoc_opt v pairs with Some t' -> t' | None -> t)
  | App (sym, ts) -> App (sym, List.map (subst pairs) ts)

(* Drop later duplicates, preserving first-occurrence order. *)
let dedup_stable (xs : string list) : string list =
  let seen = Hashtbl.create 64 in
  List.filter
    (fun x ->
      if Hashtbl.mem seen x then false
      else (
        Hashtbl.add seen x ();
        true))
    xs

(* Rebuild a system from its rules, recomputing the variable list in stable
   first-occurrence order. Every pass that filters, rewrites or extends the
   rule list closes with this. *)
let of_rules (rules : rule list) : t =
  { rules; vars = dedup_stable (List.concat_map vars_of_rule rules) }

(* Every function symbol applied anywhere in a term (the [App] heads). *)
let rec heads_of_term = function
  | Var _ -> []
  | App (head, ts) -> head :: List.concat_map heads_of_term ts

(* Symbols a rule references: every head in its lhs, rhs and conditions. The lhs
   root (the symbol the rule defines) is included too, harmless for the
   reachability closure below. *)
let refs_of_rule (r : rule) : string list =
  heads_of_term r.lhs @ heads_of_term r.rhs
  @ List.concat_map (fun (a, b) -> heads_of_term a @ heads_of_term b) r.conds

(* The symbol a rule defines: the root head of its lhs (always an [App] here). *)
let defined_head (r : rule) : string option =
  match r.lhs with App (head, _) -> Some head | Var _ -> None

(* Every symbol the system defines (the root of some rule's lhs): functions,
   relations, and prelude operations -- the symbols that should rewrite away,
   never a value constructor. A normal form still mentioning one of these is
   stuck (reduction halted mid-evaluation). *)
let defined_heads (t : t) : string list =
  dedup_stable (List.filter_map defined_head t.rules)

(* The function symbols reachable from [roots], following each reached symbol's
   defining rules in [rules] transitively (downward dependency closure). Used
   both to prune unreachable definitions and to slice the system to one
   symbol's dependencies. *)
let reachable_heads ~(roots : string list) (rules : rule list) :
    (string, unit) Hashtbl.t =
  let by_head = Hashtbl.create 256 in
  List.iter
    (fun r ->
      match defined_head r with
      | Some head ->
          let prev = try Hashtbl.find by_head head with Not_found -> [] in
          Hashtbl.replace by_head head (r :: prev)
      | None -> ())
    rules;
  let reachable = Hashtbl.create 256 in
  let worklist = ref roots in
  while !worklist <> [] do
    match !worklist with
    | [] -> ()
    | head :: rest ->
        worklist := rest;
        if not (Hashtbl.mem reachable head) then (
          Hashtbl.add reachable head ();
          match Hashtbl.find_opt by_head head with
          | Some rules ->
              worklist := List.concat_map refs_of_rule rules @ !worklist
          | None -> ())
  done;
  reachable

(* Restrict the system to the rules reachable from [roots] (each root's defining
   rules plus their transitive downward dependencies); variables are recomputed. *)
let slice (t : t) ~(roots : string list) : t =
  let reachable = reachable_heads ~roots t.rules in
  let rules =
    List.filter
      (fun r ->
        match defined_head r with
        | Some head -> Hashtbl.mem reachable head
        | None -> false)
      t.rules
  in
  of_rules rules

(* A reusable index for slicing the SAME system by many different roots (a
   sweep slices once per symbol). [make_slicer] indexes each rule under its
   defined head, keeping the rule's original position; [slice_with] then walks
   the reachability closure and GATHERS only the reachable heads' rules (sorted
   back into original order) -- it never rescans the whole rule list, so a
   153-symbol sweep costs O(sum of slice sizes) instead of O(153 * total rules).
   Its result is identical to {!slice}. *)
type slicer = { by_head : (string, (int * rule) list) Hashtbl.t }

let make_slicer (t : t) : slicer =
  let by_head = Hashtbl.create 256 in
  List.iteri
    (fun i r ->
      match defined_head r with
      | Some head ->
          let prev = try Hashtbl.find by_head head with Not_found -> [] in
          Hashtbl.replace by_head head ((i, r) :: prev)
      | None -> ())
    t.rules;
  { by_head }

let slice_with (s : slicer) ~(roots : string list) : t =
  let reachable = Hashtbl.create 256 in
  let worklist = ref roots in
  while !worklist <> [] do
    match !worklist with
    | [] -> ()
    | head :: rest ->
        worklist := rest;
        if not (Hashtbl.mem reachable head) then (
          Hashtbl.add reachable head ();
          match Hashtbl.find_opt s.by_head head with
          | Some irules ->
              worklist :=
                List.concat_map (fun (_, r) -> refs_of_rule r) irules
                @ !worklist
          | None -> ())
  done;
  let indexed =
    Hashtbl.fold
      (fun head () acc ->
        match Hashtbl.find_opt s.by_head head with
        | Some irules -> List.rev_append irules acc
        | None -> acc)
      reachable []
  in
  let sorted = List.sort (fun (i, _) (j, _) -> compare i j) indexed in
  of_rules (List.map snd sorted)
