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

(* -------------------------------------------------------------------------- *)
(* CTRS identifier lexical conventions.

   Scrubbing an arbitrary string into a CTRS-safe identifier lives here at the
   data model, not in the symbol-naming layer, because BOTH that layer
   ({!Ctrs_term}, which builds every rule's symbols) and the Maude surfaces (the
   analysis {!string_of_system_maude} below and the executable {!To_maude}) must
   agree on the exact spelling -- so the one definition sits at the layer all of
   them can reach. *)

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
(* Premise-binder normalization (analysis-surface confluence). *)

(* Substitute the variable [v] by [repl] throughout a term. *)
let rec subst_var (v : string) (repl : term) = function
  | Var u -> if u = v then repl else Var u
  | App (f, ts) -> App (f, List.map (subst_var v repl) ts)

(* Occurrences of variable [v] in a term. *)
let rec count_var (v : string) = function
  | Var u -> if u = v then 1 else 0
  | App (_, ts) -> List.fold_left (fun n t -> n + count_var v t) 0 ts

(* A value-constructor pattern: every applied head is a constructor (one no rule
   defines, [is_defined] false) and the leaves are variables. Folding such a
   pattern into a rule's lhs keeps the lhs a matchable value pattern. *)
let rec is_ctor_pattern (is_defined : string -> bool) = function
  | Var _ -> true
  | App (f, ts) ->
      (not (is_defined f)) && List.for_all (is_ctor_pattern is_defined) ts

(* Normalize every premise-bound variable out of the analysis system's rules so
   the MFE's Church-Rosser checker is not tripped by the spurious critical pairs
   the single-sort [prod = v] / [v = K(..)] condition rendering raises.

   The surface renders a CTRS join-condition as an equality, so the fresh
   variable a condition binds (a relation/function output, or a field a
   destructuring pattern extracts) is left FREE on the rule's right -- and the
   CRC, which never solves the (deterministic) binding, reports e.g.
   [#v# = v if prod = v /\ prod = #v#]. Per rule, a fixpoint folds each such
   binder back into the rule until none remains, applying the first applicable of:

   - {b inline} an output binder [(prod, v)] with [v] NOT head-bound: substitute
     [v := prod] into the rhs/conditions ([prod] a deterministic value, so this is
     semantics-preserving). The binder must be live and used once (or be a plain
     [Var] alias) so a producer is never duplicated;
   - {b fold} a PURE-accessor destructuring [(v, K(..))] -- [v] head-bound and
     used in no other condition, [K(..)] a constructor pattern: substitute
     [v := K(..)] EVERYWHERE, so [K]'s fresh field variables become
     lhs-pattern-bound (the binder moves from the right to the head, where Maude
     binds it by matching).

   Uniform over every rule, so a binder inside a recursive iteration helper
   ([$iterapply]/[$itercollect]/[$unzip]) is normalized the same way. Gensym
   threading (run before this) binds a [tuple(out, state)], not a bare [Var], so a
   threaded binder is skipped. Analysis-surface only: {!To_maude} keeps the [:=]
   matching condition its stuck-head guard relies on. *)
let fold_premise_binders ~(rule_heads : string list) (t : t) : t =
  let defined = Hashtbl.create 512 in
  List.iter (fun h -> Hashtbl.replace defined h ()) (defined_heads t);
  let is_defined h = Hashtbl.mem defined h in
  let rules_set = Hashtbl.create 64 in
  List.iter (fun h -> Hashtbl.replace rules_set h ()) rule_heads;
  let fold_rule (r : rule) : rule =
    (* The variable a condition binds and the term to fold it to, if any. *)
    let binding lhs_vars ~rhs ~others ((a, b) : cond) : (string * term) option =
      (* inline: [v] a non-head output bound to deterministic [prod]. *)
      let inline v prod =
        let deterministic =
          match prod with
          | App (f, _) -> not (Hashtbl.mem rules_set f)
          | Var _ -> true
        in
        if
          List.mem v lhs_vars
          || List.mem v (vars_of_term prod)
          || not deterministic
        then None
        else
          let is_alias = match prod with Var _ -> true | _ -> false in
          let uses =
            count_var v rhs
            + List.fold_left
                (fun n (l, r) -> n + count_var v l + count_var v r)
                0 others
          in
          if uses >= 1 && (is_alias || uses = 1) then Some (v, prod) else None
      in
      (* fold: [v] a head variable destructured against a constructor pattern.
         Only a PURE accessor -- [v] used in no other condition -- is folded:
         folding a guarded clause (where [v] also feeds a [match_*]/owise guard,
         e.g. [$lookup]'s [pair]) would strip the disjointness guard the CRC
         relies on and expose the clause's owise overlap (turning a YES into a
         MAYBE). *)
      let fold v pat =
        let pat_vars = vars_of_term pat in
        let used_elsewhere =
          List.exists (fun (l, r) -> count_var v l + count_var v r > 0) others
        in
        match pat with
        | App _
          when List.mem v lhs_vars && (not used_elsewhere)
               && count_var v r.lhs = 1
               && is_ctor_pattern is_defined pat
               && (not (List.mem v pat_vars))
               && not (List.exists (fun w -> List.mem w lhs_vars) pat_vars) ->
            Some (v, pat)
        | _ -> None
      in
      let binder v other =
        match inline v other with Some x -> Some x | None -> fold v other
      in
      match (a, b) with
      | Var v, _ -> (
          match binder v b with
          | Some x -> Some x
          | None -> ( match b with Var w -> binder w a | _ -> None))
      | _, Var w -> binder w a
      | _ -> None
    in
    let rec loop (r : rule) =
      let lhs_vars = vars_of_term r.lhs in
      let rec find before = function
        | [] -> None
        | c :: after -> (
            let others = List.rev_append before after in
            match binding lhs_vars ~rhs:r.rhs ~others c with
            | Some sub -> Some (sub, others)
            | None -> find (c :: before) after)
      in
      match find [] r.conds with
      | None -> r
      | Some ((v, repl), others) ->
          let sub = subst_var v repl in
          loop
            {
              r with
              lhs = sub r.lhs;
              rhs = sub r.rhs;
              conds = List.map (fun (l, rr) -> (sub l, sub rr)) others;
            }
    in
    let r = loop r in
    (* Drop conditions a substitution made trivially true ([t = t]). *)
    { r with conds = List.filter (fun (l, rr) -> l <> rr) r.conds }
  in
  let rules = List.map fold_rule t.rules in
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

(* Maude lexical layer, shared with the executable surface ({!To_maude}). *)

(* A CTRS id ([A-Za-z0-9_$]+) to a Maude-safe id: [_] is a mixfix placeholder in
   Maude, so map it to [-] (injective, since CTRS ids never contain [-]). *)
let maude_id (s : string) : string =
  String.map (fun c -> if c = '_' then '-' else c) s

(* A CTRS variable name as a valid Maude variable identifier. A variable built
   from a pretty-printed pattern (a tuple bind ["(value, id)"], an angle-bracket
   type ["pair<K, V>"], a primed name) can carry characters Maude forbids in a
   variable -- spaces, parens, commas, dots, angle brackets. Names already
   confined to [A-Za-z0-9_] (the overwhelming majority) render exactly as the
   [maude_id] mangling; only the rest are run through {!sanitize} first to become
   well-formed (and stay distinct). *)
let maude_var (v : string) : string =
  let plain =
    String.for_all
      (function
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
      v
  in
  maude_id (if plain then v else sanitize v)

(* A CTRS term in Maude's single-sort surface syntax: variables scrubbed to
   valid Maude identifiers, application heads mangled, no on-the-fly sorts (the
   module declares one global [vars ... : Term]). Distinct from {!string_of_term}
   (the raw human-readable diagnostic printer). *)
let rec string_of_term_maude = function
  | Var id -> maude_var id
  | App (id, []) -> maude_id id
  | App (id, terms) ->
      maude_id id ^ "("
      ^ String.concat ", " (List.map string_of_term_maude terms)
      ^ ")"

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
    (List.map
       (fun (a, b) -> string_of_term_maude a ^ " = " ^ string_of_term_maude b)
       conds)

let string_of_system_maude ?(module_name = "SPEC") ~(rule_heads : string list)
    (t : t) : string =
  let buf = Buffer.create 512 in
  let add = Buffer.add_string buf in
  (* The single sort [Term] declares its own [not]/[and]/[true]/[false], which
     would clash with Maude's built-in [BOOL] ("Ambiguous parsing"); turning the
     implicit import off lets the module's own boolean operators stand. *)
  add "set include BOOL off .\n\n";
  add ("(mod " ^ module_name ^ " is\n");
  add "  sort Term .\n";
  List.iter
    (fun (sym, arity) ->
      let sym = maude_id sym in
      if arity = 0 then add (Printf.sprintf "  op %s : -> Term .\n" sym)
      else
        let dom = String.concat " " (List.init arity (fun _ -> "Term")) in
        add (Printf.sprintf "  op %s : %s -> Term .\n" sym dom))
    (ops_of_system t);
  (match t.vars with
  | [] -> ()
  | vs ->
      add
        (Printf.sprintf "  vars %s : Term .\n"
           (String.concat " " (List.map maude_var vs))));
  let is_rule r =
    match defined_head r with Some h -> List.mem h rule_heads | None -> false
  in
  List.iter
    (fun r ->
      let lhs = string_of_term_maude r.lhs
      and rhs = string_of_term_maude r.rhs in
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
