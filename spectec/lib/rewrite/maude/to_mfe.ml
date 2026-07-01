open Lang.Il
module R = Rewrite_system
module MS = Maude_sorts

(** Emit a {!Rewrite_system.t} (the structural analysis system,
    {!Rewrite.rewrite_spec}) as an {b order-sorted} Full-Maude system module for
    the Maude Formal Environment (Church-Rosser + Coherence checkers).

    This is the analysis counterpart of {!To_maude}: it recovers each operator's
    IL sort via the shared {!Maude_sorts} (so the MFE reasons over the spec's
    real sorts instead of one universal [Term], which yields fewer spurious
    critical pairs), but keeps the {e structural} scalar theory -- own Peano
    nats, sign-magnitude ints, char-list texts, own booleans -- because the CTRS
    prelude rules that implement those scalars are present in the analysis
    system (unlike {!To_maude}, which drops them and delegates to Maude's
    built-ins).

    Differences from {!To_maude} (deliberately no shared emitter):
    - structural scalar constructors ([zero]/[succ]/[int_pos]/[int_neg]/[true]/
      [false]) are declared instead of the native [nat]/[int]/[bool]/[txt]
      wrappers, and nothing is imported ([set include BOOL off .] so the
      module's own [true]/[false]/[and]/[not] stand);
    - conditions stay in the CTRS join form [s = t] in source order (the MFE
      analyses the equations; it does not need {!To_maude}'s [:=]/[=>]
      operational scheduling, [isStuckHead] guards, delegation equations, or
      owise totalization);
    - the module is wrapped in Full-Maude [(mod ... endm)] parens. *)

let buf_line b s =
  Buffer.add_string b s;
  Buffer.add_char b '\n'

(* One CTRS join condition [s = t], both sides printed with on-the-fly sorts. *)
let string_of_cond vs ((l, r) : R.cond) : string =
  MS.print_term Structural vs l ^ " = " ^ MS.print_term Structural vs r

let string_of_conds vs (conds : R.cond list) : string =
  String.concat " /\\ " (List.map (string_of_cond vs) conds)

(* One rule as [eq]/[ceq] (equational fragment) or [rl]/[crl] (the [rule_heads]
   relations). [owise] (SpecTec [ElsePr]) is an equation attribute; a relation
   rule cannot carry it, so it is dropped there (as the single-sort surface did). *)
let print_rule vs (is_rel : bool) (r : R.rule) : string =
  let lhs = MS.print_term Structural vs r.R.lhs
  and rhs = MS.print_term Structural vs r.R.rhs in
  let arrow = if is_rel then " => " else " = " in
  let kw =
    if is_rel then if r.R.conds = [] then "rl" else "crl"
    else if r.R.conds = [] then "eq"
    else "ceq"
  in
  let attr = if r.R.owise && not is_rel then " [owise]" else "" in
  let head = kw ^ " " ^ lhs ^ arrow ^ rhs in
  match r.R.conds with
  | [] -> head ^ attr ^ " ."
  | cs -> head ^ " if " ^ string_of_conds vs cs ^ attr ^ " ."

(* Emit the order-sorted analysis module. [orig] is the elaborated IL spec (for
   sort recovery); [sys] is the structural CTRS ({!Rewrite.rewrite_spec}); the
   symbols in [rule_heads] print as [rl]/[crl], the rest as [eq]/[ceq]. *)
let module_of_system ?(module_name = "SPEC") ~(rule_heads : string list)
    (orig : spec) (sys : R.t) : string =
  (* [sys] is built from the defunctionalized spec ({!Pipeline}), so signature
     recovery and variable hints must read the same form. *)
  let orig = Defunctionalize.defunctionalize orig in
  let tenv = MS.type_env orig in
  let tbl, inj_subsorts = MS.recover Structural orig tenv in
  let var_hints = Var_hints.of_spec (Simplify.simplify_spec orig) in
  let sg sym arity = MS.signature tbl sym arity in
  (* Symbols to declare ops for: those used in the rules, plus all IL
     constructors (so a start term can be formed even when no rule mentions the
     case). Structural scalar constructors already occur in the prelude rules, so
     [symbol_arities] covers them. *)
  let used = MS.symbol_arities Structural sys.R.rules in
  let ctor_arities =
    List.filter_map
      (fun s ->
        match Hashtbl.find_opt tbl s with
        | Some (a, _) -> Some (s, List.length a)
        | None -> None)
      (MS.il_constructor_syms orig)
  in
  let ops = MS.dedup (used @ ctor_arities) in
  let op_sigs = MS.dedup (List.map (fun (s, n) -> (s, sg s n)) ops) in
  (* An empty text is the bare [nil] (a char [List]); a [text]-typed position
     takes sort [Text]. [List < Text] lets a char list inhabit those positions.
     Only when [Text] is actually a signature sort. *)
  let text_subsort =
    if
      List.exists
        (fun (_, (args, res)) -> List.mem "Text" (res :: args))
        op_sigs
    then [ ("List", "Text") ]
    else []
  in
  let inj_subsorts = inj_subsorts @ text_subsort in
  (* Sorts named by op signatures AND the endpoints of every injection edge (a
     union type surfaces only as a subsort super, so list it too). No built-in
     sorts here -- the structural theory imports nothing. *)
  let edge_sorts = List.concat_map (fun (a, b) -> [ a; b ]) inj_subsorts in
  let mentioned =
    MS.dedup
      (List.concat_map (fun (_, (args, res)) -> res :: args) op_sigs
      @ edge_sorts)
  in
  let edges =
    inj_subsorts
    @ List.filter_map
        (fun s -> if s = MS.val_sort then None else Some (s, MS.val_sort))
        mentioned
  in
  let sorts = MS.dedup (MS.val_sort :: mentioned) in
  let is_rel r =
    match R.defined_head r with
    | Some h -> List.mem h rule_heads
    | None -> false
  in
  let b = Buffer.create 4096 in
  (* The module declares its own [true]/[false]/[and]/[not]; turn the implicit
     [BOOL] import off so they don't clash ("Ambiguous parsing"). *)
  buf_line b "set include BOOL off .";
  buf_line b "";
  buf_line b ("(mod " ^ module_name ^ " is");
  let non_val = List.filter (fun s -> s <> MS.val_sort) sorts in
  buf_line b ("  sorts " ^ String.concat " " non_val ^ " " ^ MS.val_sort ^ " .");
  if non_val <> [] then
    buf_line b
      ("  subsorts " ^ String.concat " " non_val ^ " < " ^ MS.val_sort ^ " .");
  List.iter
    (fun (sub, super) -> buf_line b ("  subsort " ^ sub ^ " < " ^ super ^ " ."))
    (MS.dedup inj_subsorts);
  buf_line b "";
  List.iter
    (fun (sym, (args, res)) ->
      let dom = if args = [] then "" else String.concat " " args ^ " " in
      buf_line b ("  op " ^ R.maude_id sym ^ " : " ^ dom ^ "-> " ^ res ^ " ."))
    (List.sort compare op_sigs);
  buf_line b "";
  (* equations first (functions/prelude/constructors), then the relation rules;
     spec order preserved within each. *)
  let eqs, rls = List.partition (fun r -> not (is_rel r)) sys.R.rules in
  let emit r =
    let hint_types =
      match R.defined_head r with
      | Some h -> Option.value (Hashtbl.find_opt var_hints h) ~default:[]
      | None -> []
    in
    let hint v =
      Option.map (MS.sort_of_typ tenv) (List.assoc_opt v hint_types)
    in
    let vs = MS.infer_var_sorts edges sg hint r in
    buf_line b ("  " ^ print_rule vs (is_rel r) r)
  in
  List.iter emit eqs;
  if rls <> [] then buf_line b "";
  List.iter emit rls;
  buf_line b "endm)";
  Buffer.contents b
