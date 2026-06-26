(** Conditional term-rewriting system (CTRS) representation produced from an
    elaborated + simplified IL spec.

    Only the representation and the term printer live here; the IL -> CTRS
    translation is in {!To_ctrs}, and the textual surface that feeds the Maude
    Formal Environment (CRC/ChC) is {!string_of_system_maude} below. *)

(* A CTRS term: either a variable, or a function symbol applied to zero or more
   argument terms. A nullary application prints as a bare [id]. *)
type term = Var of string | App of string * term list

(* A condition is an equation between two terms. *)
type cond = term * term

(* A (possibly conditional) rewrite rule [lhs -> rhs | conds]. [owise] marks a
   clause that applied "otherwise" (SpecTec [ElsePr]): it fires only when no
   earlier sibling did. {!string_of_system_maude} renders it as Maude's [owise]
   equation attribute. *)
type rule = { lhs : term; rhs : term; conds : cond list; owise : bool }

type t = {
  vars : string list; (* every variable used, deduplicated *)
  rules : rule list;
}

(* The IL -> CTRS translation lives in {!To_ctrs}; this module is only the data
   model and printer. *)

let rec string_of_term = function
  | Var id -> id
  | App (id, []) -> id
  | App (id, terms) ->
      id ^ "(" ^ String.concat ", " (List.map string_of_term terms) ^ ")"

(* A single rule for debug/error messages: [lhs -> rhs], any conditions appended
   as [ | s == t, ...]. Not a surface a tool parses -- the executable surface is
   {!string_of_system_maude}; this is only for human-readable diagnostics. *)
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
  let vars = dedup_stable (List.concat_map vars_of_rule rules) in
  { rules; vars }

(* -------------------------------------------------------------------------- *)
(* Maude system-module surface, for the Maude Formal Environment (CRC + ChC).

   The textual surface of a CTRS: a single-sort Full Maude *system* module. The
   equational fragment prints as [eq]/[ceq]; the symbols in [rule_heads] (the
   non-input-moded relations -- genuinely non-deterministic, run via search)
   print as [rl]/[crl]. The split lets the Church-Rosser Checker decide the
   equations' confluence and the Coherence Checker the rules' coherence with
   them. Everything sits in one sort [Term]: the CTRS carries no sorts, so each
   operator is declared from its arity alone (an over-approximation -- ill-sorted
   overlaps may surface as spurious critical pairs). The wrapping [(mod ... endm)]
   parens are Full Maude's module-entry syntax. *)

(* Every (symbol, arity) pair applied in a term. *)
let rec ops_of_term acc = function
  | Var _ -> acc
  | App (sym, ts) ->
      List.fold_left ops_of_term ((sym, List.length ts) :: acc) ts

(* Every (symbol, arity) the system applies, first occurrence kept. A symbol's
   arity is fixed (the naming convention folds arity into variant/case symbols),
   so each symbol appears with a single arity. *)
let ops_of_system (t : t) : (string * int) list =
  let acc =
    List.fold_left
      (fun acc r ->
        let acc = ops_of_term (ops_of_term acc r.lhs) r.rhs in
        List.fold_left
          (fun acc (a, b) -> ops_of_term (ops_of_term acc a) b)
          acc r.conds)
      [] t.rules
  in
  let seen = Hashtbl.create 64 in
  List.filter
    (fun p ->
      if Hashtbl.mem seen p then false
      else (
        Hashtbl.add seen p ();
        true))
    (List.rev acc)

(* A Maude conditional fragment [t1 = t2 /\ t3 = t4] from COPS join conditions
   ([s == t] becomes the equational condition [s = t]). *)
let string_of_conds_maude conds =
  String.concat " /\\ "
    (List.map (fun (a, b) -> string_of_term a ^ " = " ^ string_of_term b) conds)

let string_of_system_maude ?(module_name = "SPEC") ~(rule_heads : string list)
    (t : t) : string =
  let buf = Buffer.create 512 in
  let add = Buffer.add_string buf in
  add ("(mod " ^ module_name ^ " is\n");
  add "  sort Term .\n";
  List.iter
    (fun (sym, arity) ->
      if arity = 0 then add (Printf.sprintf "  op %s : -> Term .\n" sym)
      else
        let dom = String.concat " " (List.init arity (fun _ -> "Term")) in
        add (Printf.sprintf "  op %s : %s -> Term .\n" sym dom))
    (ops_of_system t);
  (match t.vars with
  | [] -> ()
  | vs -> add (Printf.sprintf "  vars %s : Term .\n" (String.concat " " vs)));
  let is_rule r =
    match defined_head r with Some h -> List.mem h rule_heads | None -> false
  in
  List.iter
    (fun r ->
      let lhs = string_of_term r.lhs and rhs = string_of_term r.rhs in
      if is_rule r then
        match r.conds with
        | [] -> add (Printf.sprintf "  rl %s => %s .\n" lhs rhs)
        | cs ->
            add
              (Printf.sprintf "  crl %s => %s if %s .\n" lhs rhs
                 (string_of_conds_maude cs))
      else
        (* [owise] (SpecTec [ElsePr]) is an equation attribute; rules carry it
           only spuriously (relations are non-deterministic), so it is ignored
           on the [rl] branch above. *)
        let owise = if r.owise then " [owise]" else "" in
        match r.conds with
        | [] -> add (Printf.sprintf "  eq %s = %s%s .\n" lhs rhs owise)
        | cs ->
            add
              (Printf.sprintf "  ceq %s = %s if %s%s .\n" lhs rhs
                 (string_of_conds_maude cs) owise))
    t.rules;
  add "endm)\n";
  Buffer.contents buf
