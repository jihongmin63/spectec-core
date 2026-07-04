open Lang.Il
module R = Rewrite_system
module MS = Maude_sorts
module T = Ctrs_term

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
   symbols in [rule_heads] print as [rl]/[crl], the rest as [eq]/[ceq].
   [full_maude] (default [true], {!Mfe.check}'s use) wraps the module in Full
   Maude's [(mod ... endm)] parens, needed for the MFE's CRC/ChC loop to accept
   it as a term. [false] emits it as a plain STOCK-Maude module ([mod ... endm
   .]) instead, for {!Maude_run.run_direct}/[run_batch_direct]'s direct
   (non-reflective) execution path, which runs a bare [maude] binary with
   nothing loaded -- Full Maude's parenthesized form is not valid input there. *)
let module_of_system ?(module_name = "SPEC") ?(full_maude = true)
    ~(rule_heads : string list) (orig : spec) (sys : R.t) : string =
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
  (* Execution mode ([full_maude = false]) additionally needs
     [{!To_maude.stuck_head_sym} : Val -> Bool] declared: {!To_maude.print_rule}
     (used below in that mode) guards a bare-variable matching condition with
     it so a stuck subterm cannot silently masquerade as a bound value.
     Analysis mode never prints a [:=] matching condition (see [print_rule]
     below, the plain-[=] one), so it never needs the guard. *)
  (* Execution mode only ([full_maude = false]): [cat]/[len]'s base signatures
     ({!Maude_sorts.prelude_sigs}) are [Text]-wide (a char list is the only
     [Text] value there is, see the [text_subsort] comment below), but both are
     also used generically over any list ([to_ctrs.ml]'s [CatE]/[LenE]
     translate arbitrary sequences, not just text), and a plain [List] argument
     is not a [Text] (no subsort edge either way) -- so a [cat]/[len]
     application over e.g. a declaration list parses ill-sorted and can never
     reduce ({!To_maude}'s execution path already adds this same overload for
     the native theory). Left out of analysis mode: {!Mfe.check}'s CRC/ChC
     results for the existing spec are already established with the [Text]-only
     signature in place, and this session has no way to re-verify them, so the
     fix is scoped to the new, previously-untested execution path only. *)
  let overload_sigs =
    if full_maude then []
    else
      (if List.exists (fun (s, _) -> s = "cat") ops then
         [ ("cat", ([ "List"; "List" ], "List")) ]
       else [])
      @
      if List.exists (fun (s, _) -> s = "len") ops then
        [ ("len", ([ "List" ], "NatV")) ]
      else []
  in
  let op_sigs =
    MS.dedup
      (List.map (fun (s, n) -> (s, sg s n)) ops
      @ overload_sigs
      @
      if full_maude then []
      else [ (To_maude.stuck_head_sym, ([ MS.val_sort ], "Bool")) ])
  in
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
  (* Execution mode only: the symbols that reduce away, and (for those with a
     known arity from [used]) their [{!To_maude.stuck_head_sym}] equations --
     see the [op_sigs] comment above. *)
  let defined_heads = R.defined_heads sys in
  let stuck_arities =
    List.filter_map
      (fun h -> Option.map (fun n -> (h, n)) (List.assoc_opt h used))
      defined_heads
  in
  let b = Buffer.create 4096 in
  (* The module declares its own [true]/[false]/[and]/[not]; turn the implicit
     [BOOL] import off so they don't clash ("Ambiguous parsing"). *)
  buf_line b "set include BOOL off .";
  buf_line b "";
  buf_line b
    ((if full_maude then "(mod " else "mod ") ^ module_name ^ " is");
  let non_val = List.filter (fun s -> s <> MS.val_sort) sorts in
  buf_line b ("  sorts " ^ String.concat " " non_val ^ " " ^ MS.val_sort ^ " .");
  if non_val <> [] then
    buf_line b
      ("  subsorts " ^ String.concat " " non_val ^ " < " ^ MS.val_sort ^ " .");
  List.iter
    (fun (sub, super) -> buf_line b ("  subsort " ^ sub ^ " < " ^ super ^ " ."))
    (MS.dedup inj_subsorts);
  buf_line b "";
  (* Execution mode only: [or]/[and] are printed with only the two
     unconditional short-circuit equations (or(true,y)=true / or(false,y)=y,
     and symmetrically for [and]) -- Maude's DEFAULT strategy evaluates both
     arguments to normal form before ever trying those equations, so a stuck
     LATER disjunct/conjunct (reflect.ml's judgment/owise guards routinely
     chain dozens via [or_t]/[and_t]) blocks the WHOLE expression even once
     an EARLIER true/false has already decided it. [strat (1 0 2 0)] makes
     Maude retry the top equations right after reducing just the first
     argument, short-circuiting exactly like the equations already assume.
     But the deciding true/false is not always in argument POSITION 1 --
     [strat] alone still misses a fold where an early disjunct is the one
     that gets stuck and a LATER one is the true/false (or(stuck, true) never
     matches [or(true,y)=true], only [or(y,true)=true] would, and that
     mirror equation was never generated -- see [prelude.ml]'s [or_t]/[and_t]
     rules, deliberately kept to the one direction since [Rewrite_system]'s
     CTRS layer has no operator attributes to express the other). [comm]
     lets Maude's matcher try the argument swap itself instead, so either
     position finds the equation. Analysis mode ({!Mfe.check}) never executes
     ground terms, so it has no need for either attribute and is left as-is. *)
  List.iter
    (fun (sym, (args, res)) ->
      let dom = if args = [] then "" else String.concat " " args ^ " " in
      let attr =
        if (not full_maude) && (sym = "or" || sym = "and") then
          " [comm strat (1 0 2 0)]"
        else ""
      in
      buf_line b
        ("  op " ^ R.maude_id sym ^ " : " ^ dom ^ "-> " ^ res ^ attr ^ " ."))
    (List.sort compare op_sigs);
  (* Execution mode only: {!Maude_run.run_batch_direct} delimits each batched
     start's output with a reduce of this bare marker constant (no equations,
     so it reduces to itself) -- NOT a quoted Maude [String] literal like the
     [Native]/meta path uses, because [protecting STRING .] transitively pulls
     in [BOOL] and clashes with this module's own [true]/[false] (the reason
     for [set include BOOL off .] above in the first place). Declared with the
     exact same spelling {!Maude_run.batch_sep} is, so the bridge's line-level
     substring search for it (shared with the native path, which does wrap it
     in quotes) finds it either way. *)
  if not full_maude then
    buf_line b
      ("  op " ^ R.maude_id Maude_run.batch_sep ^ " : -> " ^ MS.val_sort
     ^ " .");
  (* Execution mode only: EVERY byte value's [chr_<code>] constructor (the
     structural char-list text encoding, {!Ctrs_term.chars_t}), not just the
     ones [used] happens to catch (a rule pattern rarely mentions a literal
     character -- text data lives in ENCODED START TERMS, built at run time
     from whatever identifiers/string literals the target program actually
     contains, which this module-text pass cannot see yet: it runs once,
     before any program is parsed). Harmless when unused (a plain declared
     constant like any other constructor). *)
  if not full_maude then (
    let already = List.map fst op_sigs in
    for code = 0 to 255 do
      let sym = T.chr_sym code in
      if not (List.mem sym already) then
        buf_line b
          ("  op " ^ R.maude_id sym ^ " : -> " ^ MS.val_sort ^ " .")
    done);
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
    let line =
      if full_maude then print_rule vs (is_rel r) r
      else
        (* [Structural]: the operational [:=]/[=>] scheduling {!To_maude} built
           for the [Native] execution module, reused as-is -- see that
           function's doc comment for why analysis mode's plain [l = r] cannot
           be [reduce]d directly. *)
        To_maude.print_rule ~scalars:Structural vs rule_heads defined_heads
          (is_rel r) r
    in
    buf_line b ("  " ^ line)
  in
  List.iter emit eqs;
  if rls <> [] then buf_line b "";
  List.iter emit rls;
  if (not full_maude) && stuck_arities <> [] then (
    buf_line b "";
    List.iter (buf_line b) (To_maude.stuck_head_eqs stuck_arities sg));
  (* [endm] is itself the complete module-closing token in STOCK Maude (no
     trailing [.] -- one was mistaken for a dangling empty top-level sentence,
     "syntax error" right after any module, verified empirically); Full Maude's
     parenthesized form closes with [)], also no [.]. *)
  buf_line b (if full_maude then "endm)" else "endm");
  Buffer.contents b


(* -------------------------------------------------------------------------- *)
(* Structural start-term encoding, for a direct (non-reflective) [reduce] of
   the analysis module ({!Maude_run.run_direct}) -- the [Structural] oracle
   leg's start-term counterpart of {!To_maude.meta_start_app} (which is
   [Native]-only: a structural value has no built-in Maude literal wrapper for
   the META-TERM grammar to reflect, so it goes through the module's real
   signature as plain object-syntax text instead, via {!Maude_sorts.print_term}
   -- no per-symbol sort suffix needed there, unlike the META-TERM printer,
   since Maude's own parser resolves sorts from the declared [op]s). *)
let start_app (orig : spec) (system : R.t) (rel : string) (args : value list) :
    string =
  let vs : (string, string) Hashtbl.t = Hashtbl.create 0 in
  let enc (v : value) : string =
    MS.print_term Structural vs (To_maude.encode_value ~scalars:Structural orig v)
  in
  let arg_terms = List.map enc args in
  (* Append the gensym seed when [system] threads [rel], exactly as
     {!To_maude.meta_start_app} does for the native path. *)
  let arg_terms =
    if List.mem (R.sanitize rel) (Gensym.effectful_syms system) then
      arg_terms
      @ [ MS.print_term Structural vs (T.text_t ~scalars:Structural Gensym.seed_text) ]
    else arg_terms
  in
  match arg_terms with
  | [] -> To_maude.maude_sym rel
  | _ -> To_maude.maude_sym rel ^ "(" ^ String.concat ", " arg_terms ^ ")"
