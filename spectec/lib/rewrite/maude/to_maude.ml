open Common.Source
open Lang.Il
module R = Rewrite_system
module T = Ctrs_term
open Maude_sorts

(** Emit a {!Rewrite_system.t} (produced by {!To_ctrs}) as an executable,
    order-sorted Maude {b system module} so the translated spec can actually be
    {e run} (via {!Maude_run}), not just analysed for confluence/termination.

    Three things the COPS/TPDB surfaces throw away are recovered here:

    - {b Sorts.} Each IL type becomes a Maude sort; every symbol gets an
      [op f : S1 .. Sn -> S .] declaration. Signatures are read from the
      original (un-simplified) spec's [TypD]/[RelD]/[DecD] using the {e same}
      naming functions {!To_ctrs} used in the rules. The container/generic
      machinery (lists, options, polymorphic functions) is order-sorted under a
      single universal supersort [Val] (every sort is [< Val]); this is the
      pragmatic resolution of the polymorphism the erased CTRS left behind.

    - {b eq vs rl.} Function/prelude/constructor rules are deterministic
      equations ([eq]/[ceq]). Input-moded relations are functional, so they also
      become equations -- this lets {!Maude_run} [reduce] them deterministically
      instead of forcing a [search] over a free output variable (see
      {!rule_relation_syms}). Only relations left as rules ([rl]/[crl]) are
      explored with [search]/[rewrite]; [relations_as_rules] forces every
      relation back to a rule.

    - {b Condition kinds.} A CTRS join condition [(left, right)] is, by the
      translation's invariant, [left] = a value to evaluate and [right] = a
      pattern/binder. It becomes a Maude equality condition [left = right] when
      [right] binds nothing new, a matching condition [right := left] when
      [right] binds fresh variables and [left] is equation-reducible, or a
      rewrite condition [left => right] when [left] is a relation kept as a
      rule.

    The module is emitted over Maude's BUILT-IN scalar theories: the system is a
    {!Pipeline.maude_system_of_spec} result, whose scalar values sit in wrapper
    constructors over the imported sorts ([nat(3) : NatV], see {!Maude_theory}),
    and the scalar operators become one-line delegation equations
    ([delegation_eqs]) instead of structural recursion. The built-in sorts
    themselves stay OUTSIDE [Val], so their kinds never merge with the spec's
    (imported operator attributes would clash; see [builtin_sorts]).

    Maude-surface notes (verified against Maude 3.5.1): [_] is a mixfix
    placeholder in Maude, so every CTRS id (which is [[A-Za-z0-9_]+]) has [_]
    rewritten to [-]; built-in literals (numerals, quoted strings) print
    verbatim; variables are written with on-the-fly sorts [name:Sort] rather
    than global [var] declarations, because the same CTRS variable name can need
    different sorts in different rules. *)

(* -------------------------------------------------------------------------- *)
(* Maude lexical layer. The id/variable scrubbing ([R.maude_id]/[R.maude_var], built
   on [sanitize]) lives in {!Rewrite_system} so the analysis surface
   ({!Rewrite_system.string_of_system_maude}) and this executable surface share
   the same spelling; referenced here as [R.maude_id]/[R.maude_var]. *)

(* -------------------------------------------------------------------------- *)
(* Condition classification. *)

let rec vars_of_term = function
  | R.Var v -> [ v ]
  | R.App (_, ts) -> List.concat_map vars_of_term ts

let is_relation rels = function
  | R.App (f, _) -> List.mem f rels
  | R.Var _ -> false

(* A wrapped bool literal [bool(true)]/[bool(false)] (the form every IL bool
   takes under the [Native] scalar theory, {!To_ctrs.of_spec}). *)
let is_bool_lit = function
  | R.App (b, [ R.App (("true" | "false"), []) ])
    when b = Maude_theory.bool_wrap_sym ->
      true
  | _ -> false

(* The name of the head-of-term value predicate (see [stuck_head_defs]). *)
let stuck_head_sym = "isStuckHead"

(* The Maude form a CTRS condition takes. One classifier decides both how the
   condition prints ([print_cond]) and which side must already be bound for it
   to run ([print_conds]'s scheduler), so the two can never disagree.
   - [`Rewrite]: the left head is a relation, which reduces only via rules
     ([rl]/[crl]), so the condition must be a rewrite condition [l => r] (an
     equality [=] only reduces with equations and would never fire).
   - [`Check]: the right side is a bool literal -- a pure test, binding nothing.
   - [`Match]: a matching condition, binding whichever side is fresh. *)
let cond_form rels ((l, r) : R.cond) : [ `Rewrite | `Check | `Match ] =
  if is_relation rels l then `Rewrite
  else if is_bool_lit r then `Check
  else `Match

(* Render one condition, threading the set of already-bound variables (Maude
   evaluates conditions left to right). Returns the condition text and the
   variables it binds. [defined] is the set of symbols that reduce away
   (functions/relations/prelude ops), used to guard bare-variable matches. *)
let print_cond vs rels defined bound ((l, r) : R.cond) : string * string list =
  let fresh_of t =
    List.filter (fun v -> not (List.mem v bound)) (vars_of_term t)
  in
  let pl = print_term Native vs l and pr = print_term Native vs r in
  (* A matching condition [pat := subj] binding [vars]: it succeeds whenever
     [subj] is structurally of [pat]'s sort -- even when a defined head in [subj]
     got stuck (no rule applied), since a stuck application still inhabits its
     declared result sort. When [pat] is a bare variable that would silently
     absorb such a stuck term, require its head to be a value constructor so the
     stuck-ness propagates to the top instead (a constructor pattern like
     [some(v)] already rejects a stuck head by failing to match). *)
  let matching ~pat ~subj ~pat_s ~subj_s vars =
    let base = pat_s ^ " := " ^ subj_s in
    match (pat, subj) with
    | R.Var _, R.App (f, _) when List.mem f defined ->
        (base ^ " /\\ " ^ stuck_head_sym ^ "(" ^ pat_s ^ ") = false", vars)
    | _ -> (base, vars)
  in
  match cond_form rels (l, r) with
  | `Rewrite -> (pl ^ " => " ^ pr, fresh_of r)
  | `Check -> (pl ^ " = " ^ pr, [])
  | `Match ->
      let fr = fresh_of r in
      if fr <> [] then matching ~pat:r ~subj:l ~pat_s:pr ~subj_s:pl fr
      else
        (* The CTRS pair usually carries the binder on the right, but a
           cast-stripped equality (`declarationIR = constantDeclarationIR`) can
           leave the fresh binder on the left; orient the match toward whichever
           side is fresh so the variable gets bound rather than emitted as an
           unsatisfiable [=]. *)
        let fl = fresh_of l in
        if fl <> [] && not (is_relation rels r) then
          matching ~pat:l ~subj:r ~pat_s:pl ~subj_s:pr fl
        else (pl ^ " = " ^ pr, [])

(* Maude evaluates conditions left to right and a variable must be bound before
   use. The CTRS order is not always binding-respecting (a cast-stripped binding
   can sit after a test that uses it), so emit conditions by readiness, taking the
   earliest source position among the ready ones -- an already-valid order is thus
   reproduced verbatim. A condition is ready when the side its [cond_form] does
   NOT bind is fully bound: a rewrite [l => r] or a check [l = r] needs [l]
   bound; a matching binds whichever side is fresh, so it needs the other side
   bound. A genuine cycle leaves nothing ready; emit the rest in source order
   rather than loop. *)
let print_conds vs rels defined (lhs_vars : string list) (conds : R.cond list) :
    string =
  let bnd bound t = List.for_all (fun v -> List.mem v bound) (vars_of_term t) in
  let ready bound ((l, r) : R.cond) =
    match cond_form rels (l, r) with
    | `Rewrite | `Check -> bnd bound l
    | `Match -> bnd bound l || bnd bound r
  in
  let emit (bound, acc) c =
    let text, newvars = print_cond vs rels defined bound c in
    (newvars @ bound, text :: acc)
  in
  let take_ready bound conds =
    let rec go seen = function
      | [] -> None
      | c :: rest when ready bound c -> Some (c, List.rev_append seen rest)
      | c :: rest -> go (c :: seen) rest
    in
    go [] conds
  in
  let rec schedule bound acc = function
    | [] -> List.rev acc
    | remaining -> (
        match take_ready bound remaining with
        | Some (c, rest) ->
            let bound, acc = emit (bound, acc) c in
            schedule bound acc rest
        | None ->
            let _, acc = List.fold_left emit (bound, acc) remaining in
            List.rev acc)
  in
  String.concat " /\\ " (schedule lhs_vars [] conds)

(* -------------------------------------------------------------------------- *)
(* Rule printing. *)

let print_rule vs rels defined (is_rel : bool) (r : R.rule) : string =
  let lhs = print_term Native vs r.R.lhs
  and rhs = print_term Native vs r.R.rhs in
  let arrow = if is_rel then " => " else " = " in
  let kw =
    if is_rel then if r.R.conds = [] then "rl" else "crl"
    else if r.R.conds = [] then "eq"
    else "ceq"
  in
  (* An [otherwise] clause maps to Maude's [owise] equation attribute. Maude
     rules ([rl]/[crl]) cannot carry it, so a relation [otherwise] is dropped
     (the historical behaviour) with a warning rather than emitting invalid
     syntax. *)
  let attr =
    if not r.R.owise then ""
    else if is_rel then (
      Printf.eprintf
        "warning: dropping `otherwise' on relation rule for %s (Maude rules \
         cannot express owise)\n"
        (match r.R.lhs with R.App (h, _) -> R.maude_id h | R.Var _ -> "?");
      "")
    else " [owise]"
  in
  let head = kw ^ " " ^ lhs ^ arrow ^ rhs in
  match r.R.conds with
  | [] -> head ^ attr ^ " ."
  | _ ->
      let lhs_vars = vars_of_term r.R.lhs in
      head ^ " if "
      ^ print_conds vs rels defined lhs_vars r.R.conds
      ^ attr ^ " ."

(* -------------------------------------------------------------------------- *)
(* Module assembly. *)

let buf_line b s =
  Buffer.add_string b s;
  Buffer.add_char b '\n'

(* The relations to keep as Maude rules ([rl]/[crl]); the rest become equations.
   Input-moded relations are functional, so by default they are emitted as
   equations -- this lets [reduce] evaluate them deterministically instead of
   forcing Maude to [search] over a free output variable. Two exceptions stay
   rules: [relations_as_rules] forces every relation back to a rule, and an
   input-moded relation whose rule conditions invoke a non-functional relation
   must stay a rule because a [ceq] cannot carry a [=>] rewrite condition (only a
   [crl] can). The latter is iterated to a fixpoint, since one such fallback can
   pull another relation down with it. *)
let rule_relation_syms ~(relations_as_rules : bool) (orig : spec) (sys : R.t) :
    string list =
  let all_rels =
    List.filter_map
      (fun def ->
        match def.it with
        | RelD { relid = id; _ } -> Some (T.rel_sym id)
        | _ -> None)
      orig
  in
  if relations_as_rules then all_rels
  else
    let candidates = To_ctrs.input_moded_rel_syms orig in
    (* Per candidate, the relations it invokes in a condition's lhs (the only
       place that would render as a [=>] rewrite condition). *)
    let invokes = Hashtbl.create 16 in
    List.iter
      (fun r ->
        match R.defined_head r with
        | Some h when List.mem h candidates ->
            let calls =
              List.filter_map
                (fun (l, _) ->
                  match l with
                  | R.App (f, _) when List.mem f all_rels -> Some f
                  | _ -> None)
                r.R.conds
            in
            Hashtbl.replace invokes h
              (calls @ Option.value (Hashtbl.find_opt invokes h) ~default:[])
        | _ -> ())
      sys.R.rules;
    let rec fixpoint functional =
      let functional' =
        List.filter
          (fun h ->
            List.for_all
              (fun c -> List.mem c functional)
              (Option.value (Hashtbl.find_opt invokes h) ~default:[]))
          functional
      in
      if List.length functional' = List.length functional then functional
      else fixpoint functional'
    in
    let functional = fixpoint candidates in
    List.filter (fun r -> not (List.mem r functional)) all_rels

(* Equations defining [isStuckHead : Val -> Bool], which is [true] exactly on a
   term whose head is a defined symbol (a stuck, not-yet-reduced application) and
   [false] on a value constructor (via [owise]). [print_cond] uses it to keep a
   bare-variable matching condition from absorbing a stuck term. [heads] are the
   defined symbols with their arities; [sg] gives each one's domain sorts. *)
let stuck_head_eqs (heads : (string * int) list) sg : string list =
  let eq_of (h, n) =
    let argsorts, _ = sg h n in
    let argsorts =
      if List.length argsorts = n then argsorts
      else List.init n (fun _ -> val_sort)
    in
    let pat =
      if n = 0 then R.maude_id h
      else
        R.maude_id h ^ "("
        ^ String.concat ", "
            (List.mapi (fun i s -> Printf.sprintf "V%d:%s" i s) argsorts)
        ^ ")"
    in
    "  eq " ^ stuck_head_sym ^ "(" ^ pat ^ ") = true ."
  in
  List.map eq_of (List.sort compare heads)
  @ [ "  eq " ^ stuck_head_sym ^ "(D:" ^ val_sort ^ ") = false [owise] ." ]

(* -------------------------------------------------------------------------- *)
(* Native-theory delegations.                                                  *)
(* The [Native] scalar theory ({!To_ctrs.of_spec}) omits the prelude rules     *)
(* that hand-implement the scalar theories; each surviving operator is          *)
(* re-emitted here as a one-                                                    *)
(* line equation over Maude's built-in Bool/Nat/Int/String. The wrapper        *)
(* constructor patterns ([nat(X:Nat)], ...) only match real values, so a stuck *)
(* argument leaves the application unreduced and stuckness propagates without  *)
(* extra guards. Variable sorts are spelled at every occurrence (Maude         *)
(* requires it for on-the-fly variables).                                      *)

let natp inner = R.maude_id Maude_theory.nat_wrap_sym ^ "(" ^ inner ^ ")"
let intp inner = R.maude_id Maude_theory.int_wrap_sym ^ "(" ^ inner ^ ")"
let boolp inner = R.maude_id Maude_theory.bool_wrap_sym ^ "(" ^ inner ^ ")"
let txtp inner = R.maude_id Maude_theory.text_wrap_sym ^ "(" ^ inner ^ ")"

let deleg_line sym args rhs =
  Printf.sprintf "  eq %s(%s) = %s ." (R.maude_id sym) (String.concat ", " args)
    rhs

(* [sym -> (arity, delegated dependencies, equation lines)]. Emitted (with the
   dependency closure) for every symbol the system still references. The
   recursive list operations are restated over [nat]-wrapped indices, their
   Peano rules being gone. Semantics match the structural prelude: [sub] is
   monus, [div]/[mod] take the dividend's sign and need a non-zero divisor
   (division by zero is stuck where the structural rules diverged), [pow_int]
   needs a non-negative exponent. The text builtins ([$int_to_text],
   [$strip_prefix]/[$strip_suffix]) are restated over [String] -- decimal
   conversion via CONVERSION's [string], stripping via [substr]/[length]. *)
let delegation_eqs : (string * (int * string list * string list)) list =
  let n2 sym op =
    deleg_line sym
      [ natp "X:Nat"; natp "Y:Nat" ]
      (natp (Printf.sprintf "X:Nat %s Y:Nat" op))
  in
  let n2b sym op =
    deleg_line sym
      [ natp "X:Nat"; natp "Y:Nat" ]
      (boolp (Printf.sprintf "X:Nat %s Y:Nat" op))
  in
  let i2 sym op =
    deleg_line sym
      [ intp "I:Int"; intp "J:Int" ]
      (intp (Printf.sprintf "I:Int %s J:Int" op))
  in
  let i2b sym op =
    deleg_line sym
      [ intp "I:Int"; intp "J:Int" ]
      (boolp (Printf.sprintf "I:Int %s J:Int" op))
  in
  (* the builtins p4-old and the new p4 spec spell differently, generated from
     the symbol so both spellings share one definition *)
  let to_bitstr sym =
    ( sym,
      ( 2,
        [],
        [
          deleg_line sym
            [ intp "W:Nat"; intp "N:Int" ]
            (intp "((2 ^ W:Nat) - 1) & N:Int");
        ] ) )
  in
  let to_int sym =
    ( sym,
      ( 2,
        [],
        [
          Printf.sprintf
            "  ceq %s(%s, %s) = %s if P:Int := 2 ^ W:Nat /\\ B:Int := ((2 ^ \
             W:Nat) - 1) & N:Int ."
            (R.maude_id sym) (intp "W:Nat") (intp "N:Int")
            (intp "if B:Int < (P:Int quo 2) then B:Int else B:Int - P:Int fi");
        ] ) )
  in
  let nat_fold sym op =
    ( sym,
      ( 1,
        [],
        [
          deleg_line sym [ "nil" ] (natp "0");
          Printf.sprintf "  ceq %s(cons(%s, L:List)) = %s if %s := %s(L:List) ."
            (R.maude_id sym) (natp "N:Nat") (natp op) (natp "M:Nat")
            (R.maude_id sym);
        ] ) )
  in
  [
    ( "not",
      (1, [], [ deleg_line "not" [ boolp "B:Bool" ] (boolp "not B:Bool") ]) );
    (* The binary connectives dispatch on the FIRST argument only, mirroring
       the structural prelude: the second side passes through un-forced, so a
       stuck right side under a deciding left (e.g. a non-member [subty_<T>],
       which has no [-> false] rules, under a false antecedent) does not stick
       the whole condition. *)
    ( "and",
      ( 2,
        [],
        [
          deleg_line "and" [ boolp "true"; "Y:Val" ] "Y:Val";
          deleg_line "and" [ boolp "false"; "Y:Val" ] (boolp "false");
        ] ) );
    ( "or",
      ( 2,
        [],
        [
          deleg_line "or" [ boolp "true"; "Y:Val" ] (boolp "true");
          deleg_line "or" [ boolp "false"; "Y:Val" ] "Y:Val";
        ] ) );
    ( "impl",
      ( 2,
        [],
        [
          deleg_line "impl" [ boolp "true"; "Y:Val" ] "Y:Val";
          deleg_line "impl" [ boolp "false"; "Y:Val" ] (boolp "true");
        ] ) );
    ( "equiv",
      ( 2,
        [],
        [
          deleg_line "equiv" [ boolp "true"; "Y:Val" ] "Y:Val";
          deleg_line "equiv"
            [ boolp "false"; "Y:Val" ]
            (R.maude_id "not" ^ "(Y:Val)");
        ] ) );
    ("add", (2, [], [ n2 "add" "+" ]));
    ( "sub",
      ( 2,
        [],
        [
          (* PARTIAL, matching the interpreter's [Xl.Num.bin] (num.ml): nat
             subtraction is defined only when [Y <= X]; an underflow has no rule
             and stays STUCK, so the violation propagates to a stuck normal form
             (a rejection) instead of saturating to 0. A total monus [else 0]
             would silently accept ill-typed programs that rely on nat underflow
             being a rejection -- e.g. a table whose [priority_delta] drives an
             entry priority negative ([$set_priorities_of_tableEntryListIR']'s
             [$(n_last - n_delta)] in 5.02.2-typing-table-context). No
             interp-PASS program reaches an underflowing [sub] (the interpreter
             would have asserted), so this never costs completeness. *)
          Printf.sprintf "  ceq sub(%s, %s) = %s if Y:Nat <= X:Nat ."
            (natp "X:Nat") (natp "Y:Nat") (natp "sd(X:Nat, Y:Nat)");
        ] ) );
    ("mul", (2, [], [ n2 "mul" "*" ]));
    ( "div",
      ( 2,
        [],
        [
          deleg_line "div"
            [ natp "X:Nat"; natp "Y:NzNat" ]
            (natp "X:Nat quo Y:NzNat");
        ] ) );
    ( "mod",
      ( 2,
        [],
        [
          deleg_line "mod"
            [ natp "X:Nat"; natp "Y:NzNat" ]
            (natp "X:Nat rem Y:NzNat");
        ] ) );
    ("pow", (2, [], [ n2 "pow" "^" ]));
    ("leq", (2, [], [ n2b "leq" "<=" ]));
    ("lt", (2, [], [ n2b "lt" "<" ]));
    ( "int_pos",
      (1, [], [ deleg_line "int_pos" [ natp "N:Nat" ] (intp "N:Nat") ]) );
    ( "int_neg",
      (1, [], [ deleg_line "int_neg" [ natp "N:Nat" ] (intp "- (N:Nat + 1)") ])
    );
    ( "negate_int",
      (1, [], [ deleg_line "negate_int" [ intp "I:Int" ] (intp "- I:Int") ]) );
    ( "abs_nat",
      (1, [], [ deleg_line "abs_nat" [ intp "I:Int" ] (natp "abs(I:Int)") ]) );
    ( "nonneg_int",
      (1, [], [ deleg_line "nonneg_int" [ intp "I:Int" ] (boolp "I:Int >= 0") ])
    );
    (* the nat<-int cast gate: the inner [N:Nat] pattern only matches a
       non-negative payload, so a negative int stays unconverted (stuck). *)
    ( "nat_of_int",
      (1, [], [ deleg_line "nat_of_int" [ intp "N:Nat" ] (natp "N:Nat") ]) );
    ( "sub_int_nat",
      ( 2,
        [],
        [
          deleg_line "sub_int_nat"
            [ natp "X:Nat"; natp "Y:Nat" ]
            (intp "X:Nat - Y:Nat");
        ] ) );
    ("add_int", (2, [], [ i2 "add_int" "+" ]));
    ("sub_int", (2, [], [ i2 "sub_int" "-" ]));
    ("mul_int", (2, [], [ i2 "mul_int" "*" ]));
    ( "div_int",
      ( 2,
        [],
        [
          deleg_line "div_int"
            [ intp "I:Int"; intp "J:NzInt" ]
            (intp "I:Int quo J:NzInt");
        ] ) );
    ( "mod_int",
      ( 2,
        [],
        [
          deleg_line "mod_int"
            [ intp "I:Int"; intp "J:NzInt" ]
            (intp "I:Int rem J:NzInt");
        ] ) );
    ( "pow_int",
      ( 2,
        [],
        [
          deleg_line "pow_int"
            [ intp "I:Int"; intp "N:Nat" ]
            (intp "I:Int ^ N:Nat");
        ] ) );
    ("leq_int", (2, [], [ i2b "leq_int" "<=" ]));
    ("lt_int", (2, [], [ i2b "lt_int" "<" ]));
    ( "sub_nat",
      ( 1,
        [],
        [
          deleg_line "sub_nat" [ natp "N:Nat" ] (boolp "true");
          deleg_line "sub_nat" [ intp "N:Nat" ] (boolp "true");
          deleg_line "sub_nat" [ intp "- N:NzNat" ] (boolp "false");
        ] ) );
    ( "len",
      ( 1,
        [],
        [
          deleg_line "len" [ "nil" ] (natp "0");
          Printf.sprintf
            "  ceq len(cons(V:Val, L:List)) = %s if %s := len(L:List) ."
            (natp "s(N:Nat)") (natp "N:Nat");
          deleg_line "len" [ txtp "S:String" ] (natp "length(S:String)");
        ] ) );
    ( "idx",
      ( 2,
        [],
        [
          deleg_line "idx" [ "cons(V:Val, L:List)"; natp "0" ] "V:Val";
          deleg_line "idx"
            [ "cons(V:Val, L:List)"; natp "s(N:Nat)" ]
            (Printf.sprintf "idx(L:List, %s)" (natp "N:Nat"));
        ] ) );
    ( "take",
      ( 2,
        [],
        [
          deleg_line "take" [ "L:List"; natp "0" ] "nil";
          deleg_line "take"
            [ "cons(V:Val, L:List)"; natp "s(N:Nat)" ]
            (Printf.sprintf "cons(V:Val, take(L:List, %s))" (natp "N:Nat"));
        ] ) );
    ( "drop",
      ( 2,
        [],
        [
          deleg_line "drop" [ "L:List"; natp "0" ] "L:List";
          deleg_line "drop"
            [ "cons(V:Val, L:List)"; natp "s(N:Nat)" ]
            (Printf.sprintf "drop(L:List, %s)" (natp "N:Nat"));
        ] ) );
    ( "slice",
      ( 3,
        [ "take"; "drop" ],
        [
          deleg_line "slice"
            [ "L:List"; natp "I:Nat"; natp "N:Nat" ]
            (Printf.sprintf "take(drop(L:List, %s), %s)" (natp "I:Nat")
               (natp "N:Nat"));
        ] ) );
    ( "upd_idx",
      ( 3,
        [],
        [
          deleg_line "upd_idx"
            [ "cons(V:Val, L:List)"; natp "0"; "W:Val" ]
            "cons(W:Val, L:List)";
          deleg_line "upd_idx"
            [ "cons(V:Val, L:List)"; natp "s(N:Nat)"; "W:Val" ]
            (Printf.sprintf "cons(V:Val, %s(L:List, %s, W:Val))"
               (R.maude_id "upd_idx") (natp "N:Nat"));
        ] ) );
    ( "upd_slice",
      ( 4,
        [ "take"; "drop" ],
        [
          deleg_line "upd_slice"
            [ "L:List"; natp "I:Nat"; natp "N:Nat"; "M:List" ]
            (Printf.sprintf
               "cat(take(L:List, %s), cat(M:List, drop(L:List, %s)))"
               (natp "I:Nat") (natp "I:Nat + N:Nat"));
        ] ) );
    ( "$int_to_text",
      ( 1,
        [],
        [
          (* Match the interpreter's [Xl.Num.string_of_num] EXACTLY: a signed
             [int] prints with an explicit sign ([+0], [+72], [-5]), an unsigned
             [nat] without one. Maude's [string(I, 10)] already prints [-] for a
             negative, so only the non-negative [int] case needs the [+] prefix.
             (Reached by every numeric table-key name via [$name_expression].) *)
          deleg_line "$int_to_text"
            [ intp "I:Int" ]
            (txtp
               "if I:Int >= 0 then \"+\" + string(I:Int, 10) else \
                string(I:Int, 10) fi");
          (* a nat argument flows in where the IL used nat <: int subsumption
             without an explicit cast (e.g. the tuple-type `$init` indices) *)
          deleg_line "$int_to_text" [ natp "N:Nat" ] (txtp "string(N:Nat, 10)");
        ] ) );
    ( "$strip_prefix",
      ( 2,
        [],
        [
          deleg_line "$strip_prefix"
            [ txtp "T:String"; txtp "P:String" ]
            (txtp "substr(T:String, length(P:String), length(T:String))");
        ] ) );
    ( "$strip_suffix",
      ( 2,
        [],
        [
          deleg_line "$strip_suffix"
            [ txtp "T:String"; txtp "S:String" ]
            (txtp "substr(T:String, 0, sd(length(T:String), length(S:String)))");
        ] ) );
    (* [$strip_all_whitespace] removes every space (the interpreter's
       [String.split_on_char ' ' |> concat], [targets/p4/builtins/texts.ml]):
       a string with no space is returned as-is, otherwise the first space
       ([find]) is excised and the result reprocessed. Reached by every table
       key ([TableKey_ok]'s [$strip_all_whitespace($name_expression(..))]). *)
    ( "$strip_all_whitespace",
      ( 1,
        [],
        [
          (* the empty text is the bare [nil] (see the empty-text-as-[nil] convention),
             not [txt("")], so it needs its own line -- reached when
             [$name_expression] of a non-name key expression returns empty *)
          deleg_line "$strip_all_whitespace" [ "nil" ] "nil";
          Printf.sprintf
            "  ceq %s(%s) = %s if find(S:String, \" \", 0) == notFound ."
            (R.maude_id "$strip_all_whitespace")
            (txtp "S:String") (txtp "S:String");
          Printf.sprintf
            "  ceq %s(%s) = %s if I:Nat := find(S:String, \" \", 0) ."
            (R.maude_id "$strip_all_whitespace")
            (txtp "S:String")
            (R.maude_id "$strip_all_whitespace"
            ^ "("
            ^ txtp
                "substr(S:String, 0, I:Nat) + substr(S:String, I:Nat + 1, \
                 length(S:String))"
            ^ ")");
        ] ) );
    (* The numeric builtins ([BuiltinDecD]s the interpreter implements in
       OCaml, [targets/p4/builtins/numerics.ml]/[nats.ml]): pure Bigint
       arithmetic, mirrored directly by GMP -- bitwise operations are
       two's-complement on both sides, [>>] is the interpreter's arithmetic
       (floor) shift, [quo] its truncated halving. A shift by a negative
       offset is the identity (the interpreter's loop does not run). Unary
       Peano could not express these at all (2^32 was unrepresentable). *)
    ( "$pow2",
      (1, [], [ deleg_line "$pow2" [ natp "N:Nat" ] (intp "2 ^ N:Nat") ]) );
    ( "$shl",
      ( 2,
        [],
        [
          deleg_line "$shl"
            [ intp "I:Int"; intp "N:Nat" ]
            (intp "I:Int << N:Nat");
          deleg_line "$shl" [ intp "I:Int"; intp "- K:NzNat" ] (intp "I:Int");
        ] ) );
    ( "$shr",
      ( 2,
        [],
        [
          deleg_line "$shr"
            [ intp "I:Int"; intp "N:Nat" ]
            (intp "I:Int quo (2 ^ N:Nat)");
          deleg_line "$shr" [ intp "I:Int"; intp "- K:NzNat" ] (intp "I:Int");
        ] ) );
    ( "$shr_arith",
      ( 3,
        [],
        [
          deleg_line "$shr_arith"
            [ intp "I:Int"; intp "0"; intp "M:Int" ]
            (intp "I:Int");
          deleg_line "$shr_arith"
            [ intp "I:Int"; intp "- K:NzNat"; intp "M:Int" ]
            (intp "I:Int");
          deleg_line "$shr_arith"
            [ intp "I:Int"; intp "s(N:Nat)"; intp "M:Int" ]
            (Printf.sprintf "%s(%s, %s, %s)" (R.maude_id "$shr_arith")
               (intp "(I:Int quo 2) + M:Int")
               (intp "N:Nat") (intp "M:Int"));
        ] ) );
    ("$bneg", (1, [], [ deleg_line "$bneg" [ intp "I:Int" ] (intp "~ I:Int") ]));
    ( "$band",
      ( 2,
        [],
        [
          deleg_line "$band"
            [ intp "I:Int"; intp "J:Int" ]
            (intp "I:Int & J:Int");
        ] ) );
    ( "$bor",
      ( 2,
        [],
        [
          deleg_line "$bor"
            [ intp "I:Int"; intp "J:Int" ]
            (intp "I:Int | J:Int");
        ] ) );
    ( "$bxor",
      ( 2,
        [],
        [
          deleg_line "$bxor"
            [ intp "I:Int"; intp "J:Int" ]
            (intp "I:Int xor J:Int");
        ] ) );
    (* [$bitacc(n, m, l)] = the bit slice [n[m:l]] = (n >> l) & (2^(m+1-l) - 1),
       defined for 0 <= l <= m (a negative or inverted slice is stuck, where
       the interpreter raises). *)
    ( "$bitacc",
      ( 3,
        [],
        [
          Printf.sprintf "  ceq %s(%s, %s, %s) = %s if L:Nat <= M:Nat = true ."
            (R.maude_id "$bitacc") (intp "N:Int") (intp "M:Nat") (intp "L:Nat")
            (intp "(N:Int >> L:Nat) & ((2 ^ sd(M:Nat + 1, L:Nat)) - 1)");
        ] ) );
    (* [$to_bitstr(w, n)] = n mod 2^w (the low w bits, non-negative);
       [$to_int(w, n)] reads those bits back as a w-bit two's-complement
       signed value. Both mirror the interpreter's iterative +-2^w wrapping.
       The new p4 spec spells them [$int_to_bitstr]/[$bitstr_to_int] (the
       interpreter renames per target, [targets/p4/p4.ml] [old_builtins]), so
       each delegation is instantiated under both spellings. *)
    to_bitstr "$to_bitstr";
    to_bitstr "$int_to_bitstr";
    to_int "$to_int";
    to_int "$bitstr_to_int";
    (* [$sum]/[$max]/[$min] ([$sum_nat]/... in the new spec) fold a nat list
       with base 0 (so [$min] of any list is 0 -- faithfully mirroring the
       interpreter's fold). *)
    nat_fold "$sum" "N:Nat + M:Nat";
    nat_fold "$sum_nat" "N:Nat + M:Nat";
    nat_fold "$max" "max(N:Nat, M:Nat)";
    nat_fold "$max_nat" "max(N:Nat, M:Nat)";
    nat_fold "$min" "min(N:Nat, M:Nat)";
    nat_fold "$min_nat" "min(N:Nat, M:Nat)";
    (* new-spec-only: decimal text to int (the interpreter's int_of_string) *)
    ( "$text_to_int",
      ( 1,
        [],
        [
          deleg_line "$text_to_int"
            [ txtp "S:String" ]
            (intp "rat(S:String, 10)");
        ] ) );
  ]

(* Equations the [eq] symbol gains over the wrapped scalars (its structural
   rules over options/lists/user types are kept as CTRS rules). Different
   wrappers never compare -- the elaborator casts both sides to one type. The
   [nil] lines bridge the empty text: a spec [TextE ""] compiles to the bare
   empty LIST, indistinguishable from one (see the empty-text-as-[nil] convention). *)
let scalar_eq_eqs () : string list =
  [
    deleg_line "eq" [ natp "X:Nat"; natp "Y:Nat" ] (boolp "X:Nat == Y:Nat");
    deleg_line "eq" [ intp "I:Int"; intp "J:Int" ] (boolp "I:Int == J:Int");
    deleg_line "eq"
      [ boolp "B1:Bool"; boolp "B2:Bool" ]
      (boolp "B1:Bool == B2:Bool");
    deleg_line "eq"
      [ txtp "S1:String"; txtp "S2:String" ]
      (boolp "S1:String == S2:String");
    deleg_line "eq" [ txtp "S:String"; "nil" ] (boolp "S:String == \"\"");
    deleg_line "eq" [ "nil"; txtp "S:String" ] (boolp "S:String == \"\"");
  ]

(* [cat] over texts (its List rules are structural and kept); the [nil] line
   bridges the empty text again. *)
let text_cat_eqs () : string list =
  [
    deleg_line "cat"
      [ txtp "S1:String"; txtp "S2:String" ]
      (txtp "S1:String + S2:String");
    "  eq cat(T:Text, nil) = T:Text .";
  ]

(* The delegated symbols present in [sys] (dependency-closed, with arities):
   they have no rules in the system, so op declaration, stuck detection and
   the bare-variable match guards must count them explicitly. *)
let native_delegated (sys : R.t) : (string * int) list =
  let used = symbol_arities Native sys.R.rules in
  let rec close acc sym =
    if List.mem_assoc sym acc then acc
    else
      match List.assoc_opt sym delegation_eqs with
      | None -> acc
      | Some (arity, deps, _) -> List.fold_left close ((sym, arity) :: acc) deps
  in
  List.fold_left
    (fun acc (sym, _) ->
      if List.mem_assoc sym delegation_eqs then close acc sym else acc)
    [] used

(* The built-in sorts our wrappers and delegations mention. They are imported,
   not declared, and they stay OUTSIDE [Val]: their kinds never merge with the
   spec's, so the imported NAT/STRING/BOOL operators cannot clash with each
   other (or with ours). *)
let builtin_sorts = [ "Bool"; "Nat"; "NzNat"; "Int"; "NzInt"; "String" ]

let module_of_system ?(module_name = "SPEC") ?(relations_as_rules = false)
    (orig : spec) (sys : R.t) : string =
  (* [sys] was built from the defunctionalized spec ({!Pipeline}), so signature
     recovery and variable hints must read the same form -- the specialized
     copies' declarations live only there (memoized, the same physical spec). *)
  let orig = Defunctionalize.defunctionalize orig in
  let tenv = type_env orig in
  let tbl, inj_subsorts = recover Native orig tenv in
  (* Declared IL types of body-rule variables, keyed by defined symbol, used to
     restore narrow variable sorts (see {!Var_hints.of_spec}). Recomputed
     from the same idempotent simplification [sys] was built from. *)
  let var_hints = Var_hints.of_spec (Simplify.simplify_spec orig) in
  let rels = rule_relation_syms ~relations_as_rules orig sys in
  let sg sym arity = signature tbl sym arity in
  (* Symbols to declare ops for: those used in the rules, the delegated
     operators (rule-less, see [native_delegated]), the wrapper constructors
     (a start term needs them even when the spec's own rules do not), plus all
     IL constructors (so encoders can build start terms). *)
  let used = symbol_arities Native sys.R.rules in
  let delegated = native_delegated sys in
  let wrapper_arities =
    [
      (Maude_theory.bool_wrap_sym, 1);
      (Maude_theory.nat_wrap_sym, 1);
      (Maude_theory.int_wrap_sym, 1);
      (Maude_theory.text_wrap_sym, 1);
      ("nil", 0);
    ]
  in
  let ctor_syms = il_constructor_syms orig in
  let ctor_arities =
    List.filter_map
      (fun s ->
        match Hashtbl.find_opt tbl s with
        | Some (a, _) -> Some (s, List.length a)
        | None -> None)
      ctor_syms
  in
  let ops = dedup (used @ delegated @ wrapper_arities @ ctor_arities) in
  let has_op sym = List.exists (fun (s, _) -> s = sym) ops in
  (* The symbols that reduce away: rule heads, plus the delegated operators.
     Used both to guard bare-variable matching conditions and to define
     [isStuckHead]. *)
  let defined_heads = dedup (R.defined_heads sys @ List.map fst delegated) in
  let stuck_arities =
    dedup
      (List.filter_map
         (fun h -> Option.map (fun n -> (h, n)) (List.assoc_opt h used))
         defined_heads
      @ delegated)
  in
  (* The [List]-precise overloads of [len]/[cat] (their base declarations are
     [Text]-wide for inference, see [prelude_sigs]): a [cat] of two lists
     parses at sort [List], so it can fill a list position statically. *)
  let overload_sigs =
    (if List.mem_assoc "len" delegated then [ ("len", ([ "List" ], "NatV")) ]
     else [])
    @ if has_op "cat" then [ ("cat", ([ "List"; "List" ], "List")) ] else []
  in
  let op_sigs =
    dedup
      (List.map (fun (s, n) -> (s, sg s n)) ops
      @ overload_sigs
      @ [ (stuck_head_sym, ([ val_sort ], "Bool")) ])
  in
  (* An empty text is the bare empty LIST ([TextE ""] has no [chr] to mark it,
     see the empty-text-as-[nil] convention), but a [text]-typed position takes sort
     [Text]. [List < Text] lets it (and any char list) inhabit those positions;
     the [eq]/[cat] bridge equations above give it text semantics. Only when
     [Text] is actually used as a signature sort. *)
  let text_subsort =
    if
      List.exists
        (fun (_, (args, res)) -> List.mem "Text" (res :: args))
        op_sigs
    then [ ("List", "Text") ]
    else []
  in
  let inj_subsorts = inj_subsorts @ text_subsort in
  (* Sorts named by op signatures, AND the endpoints of every injection subsort
     edge. A union type ([syntax baseTypeIR = primitiveTypeIR | numberTypeIR]) has
     no constructors of its own, so it surfaces only as a subsort SUPER
     ([subsort PrimitiveTypeIR < BaseTypeIR]); without listing it here it is never
     declared and Maude rejects the edge (and any variable of that sort) as an
     undeclared sort. The built-in sorts are imported, not declared, and are
     kept out of [Val] (see [builtin_sorts]). *)
  let edge_sorts = List.concat_map (fun (a, b) -> [ a; b ]) inj_subsorts in
  let mentioned =
    dedup
      (List.concat_map (fun (_, (args, res)) -> res :: args) op_sigs
      @ edge_sorts)
    |> List.filter (fun s -> not (List.mem s builtin_sorts))
  in
  let edges =
    inj_subsorts
    @ List.filter_map
        (fun s -> if s = val_sort then None else Some (s, val_sort))
        mentioned
  in
  let sorts = dedup (val_sort :: mentioned) in
  let b = Buffer.create 4096 in
  buf_line b ("mod " ^ module_name ^ " is");
  buf_line b "  protecting INT .";
  buf_line b "  protecting STRING .";
  if List.mem_assoc "$int_to_text" delegated then
    buf_line b "  protecting CONVERSION .";
  buf_line b
    ("  sorts "
    ^ String.concat " " (List.filter (fun s -> s <> val_sort) sorts)
    ^ " " ^ val_sort ^ " .");
  (* subsorts: every non-Val sort under Val, then injection edges *)
  let non_val = List.filter (fun s -> s <> val_sort) sorts in
  if non_val <> [] then
    buf_line b
      ("  subsorts " ^ String.concat " " non_val ^ " < " ^ val_sort ^ " .");
  List.iter
    (fun (sub, super) -> buf_line b ("  subsort " ^ sub ^ " < " ^ super ^ " ."))
    (dedup inj_subsorts);
  buf_line b "";
  (* op declarations, sorted for stable output *)
  List.iter
    (fun (sym, (args, res)) ->
      let dom = if args = [] then "" else String.concat " " args ^ " " in
      buf_line b ("  op " ^ R.maude_id sym ^ " : " ^ dom ^ "-> " ^ res ^ " ."))
    (List.sort compare op_sigs);
  buf_line b "";
  (* rules: equations first (functions/prelude/constructors), then relation
     rules, preserving spec order within each. *)
  let eqs, rls =
    List.partition
      (fun r ->
        match R.defined_head r with
        | Some h -> not (List.mem h rels)
        | None -> true)
      sys.R.rules
  in
  let emit r =
    let head = R.defined_head r in
    let is_rel = match head with Some h -> List.mem h rels | None -> false in
    (* The declared types of this rule's variables (by its defined symbol),
       mapped to sorts; authoritative over position inference. *)
    let hint_types =
      match head with
      | Some h -> Option.value (Hashtbl.find_opt var_hints h) ~default:[]
      | None -> []
    in
    let hint v = Option.map (sort_of_typ tenv) (List.assoc_opt v hint_types) in
    let vs = infer_var_sorts edges sg hint r in
    buf_line b ("  " ^ print_rule vs rels defined_heads is_rel r)
  in
  List.iter emit eqs;
  if rls <> [] then buf_line b "";
  List.iter emit rls;
  (* The built-in theory: delegation equations for the surviving scalar/list
     operators, the scalar [eq] equations, and the text [cat] overload. *)
  buf_line b "";
  List.iter
    (fun (sym, _) ->
      match List.assoc_opt sym delegation_eqs with
      | Some (_, _, lines) -> List.iter (buf_line b) lines
      | None -> ())
    (List.sort compare delegated);
  if has_op "eq" then List.iter (buf_line b) (scalar_eq_eqs ());
  if has_op "cat" then List.iter (buf_line b) (text_cat_eqs ());
  (* Negated judgments: an [IfNotHoldPr] premise compiles to a condition
     [R(args) == false], but a judgment's positive clauses only ever produce
     [bool(true)] -- failure is stuckness -- so the condition is unsatisfiable
     and its clause dead. Totalize each relation used under negation with a
     guarded [owise] complement, making the negation decidable. Closed-world:
     sound when the positive clauses cover every holding instance; a stuck
     application deeper than the argument heads is still absorbed to [false].
     Only no-output relations emitted as equations qualify -- a complement on
     an output-carrying or rule-mode relation would turn its failure into a
     value, breaking stuck propagation -- the rest keep the unsatisfiable
     condition (warned). *)
  let judgment_syms, all_rel_syms =
    List.fold_left
      (fun (js, rs) def ->
        match def.it with
        | RelD { relid = id; reltyp; _ } ->
            let sym = T.rel_sym id in
            let _, outs =
              Mode.partition reltyp.it (Mixfix.args (Mode.notation reltyp.it))
            in
            ((if outs = [] then sym :: js else js), sym :: rs)
        | _ -> (js, rs))
      ([], []) orig
  in
  let is_false_lit = function
    | R.App (b, [ R.App ("false", []) ]) when b = Maude_theory.bool_wrap_sym ->
        true
    | _ -> false
  in
  (* A gensym-threaded judgment ({!Gensym}) returns [tuple(bool, state)], so
     its negated premise reads [tuple(bool(false), St) := R(args, St0)] and
     its complement must pass the state through: the failed attempt issued no
     observable name. *)
  let threaded = Gensym.effectful_syms sys in
  let is_threaded_false = function
    | R.App ("tuple", [ x; _ ]) -> is_false_lit x
    | _ -> false
  in
  let negated_rels =
    dedup
      (List.concat_map
         (fun r ->
           List.filter_map
             (fun (l, rhs) ->
               match l with
               | R.App (f, _)
                 when (is_false_lit rhs || is_threaded_false rhs)
                      && List.mem f all_rel_syms ->
                   Some f
               | _ -> None)
             r.R.conds)
         sys.R.rules)
  in
  if negated_rels <> [] then buf_line b "";
  List.iter
    (fun sym ->
      if (not (List.mem sym judgment_syms)) || List.mem sym rels then
        Printf.eprintf
          "warning: negated relation %s is not an equation-mode judgment; its \
           negated premises stay unsatisfiable\n"
          (R.maude_id sym)
      else
        match List.assoc_opt sym used with
        | None -> ()
        | Some arity ->
            let args, _ = sg sym arity in
            let vars =
              List.mapi (fun i s -> Printf.sprintf "X%d:%s" i s) args
            in
            let lhs =
              if vars = [] then R.maude_id sym
              else R.maude_id sym ^ "(" ^ String.concat ", " vars ^ ")"
            in
            let false_t = R.maude_id Maude_theory.bool_wrap_sym ^ "(false)" in
            let rhs =
              (* threaded judgment: the trailing argument is the state, handed
                 back unchanged alongside the failure *)
              if List.mem sym threaded then
                Printf.sprintf "tuple(%s, %s)" false_t
                  (List.nth vars (List.length vars - 1))
              else false_t
            in
            let guards =
              List.map
                (fun v -> Printf.sprintf "%s(%s) = false" stuck_head_sym v)
                vars
            in
            buf_line b
              (if guards = [] then
                 Printf.sprintf "  eq %s = %s [owise] ." lhs rhs
               else
                 Printf.sprintf "  ceq %s = %s if %s [owise] ." lhs rhs
                   (String.concat " /\\ " guards)))
    (List.sort compare negated_rels);
  (* Structural-subtype predicates ([subty_<T>] and the [subty_tup]/[_list]/
     [_opt] helpers) carry member rules only ([defs_of_typ] never emits
     [-> false]), but the spec uses them NEGATED -- e.g. every
     wellformedness rule's `~(t <: synthesizedTypeIR)` -- so totalize each
     used one with the same guarded owise complement as the judgments above
     (closed-world: the member rules cover every member; a stuck argument
     head is not absorbed). *)
  let subty_syms =
    List.filter (fun (sym, _) -> String.starts_with ~prefix:"subty_" sym) used
  in
  if subty_syms <> [] then buf_line b "";
  List.iter
    (fun (sym, arity) ->
      let args, _ = sg sym arity in
      let vars = List.mapi (fun i s -> Printf.sprintf "X%d:%s" i s) args in
      let lhs = R.maude_id sym ^ "(" ^ String.concat ", " vars ^ ")" in
      let guards =
        List.map
          (fun v -> Printf.sprintf "%s(%s) = false" stuck_head_sym v)
          vars
      in
      buf_line b
        (Printf.sprintf "  ceq %s = %s(false) if %s [owise] ." lhs
           (R.maude_id Maude_theory.bool_wrap_sym)
           (String.concat " /\\ " guards)))
    (List.sort compare subty_syms);
  (* The value-head predicate guarding bare-variable matching conditions. *)
  buf_line b "";
  List.iter (buf_line b) (stuck_head_eqs stuck_arities sg);
  buf_line b "endm";
  Buffer.contents b

(* The one-call entry point: translate via the EXECUTION pipeline
   ({!Pipeline.maude_system_of_spec} -- the structural CTRS restated over
   Maude's built-in theories), then emit the module text. [orig] (the
   elaborated spec) supplies the sorts/signatures. The analysis (COPS) surface
   keeps the structural system; the two pipelines diverge intentionally. *)
let module_of_spec ?(module_name = "SPEC") ?(relations_as_rules = false)
    (orig : spec) : string =
  let sys = Pipeline.maude_system_of_spec orig in
  module_of_system ~module_name ~relations_as_rules orig sys

(* The module's reducible symbols (functions/relations/ops, including the
   rule-less delegated operators) in Maude spelling, for {!Maude_run}'s stuck
   check: a normal form still mentioning one of these halted mid-evaluation.
   The same system [module_of_spec] emits. *)
let maude_defined_heads (orig : spec) : string list =
  let sys = Pipeline.maude_system_of_spec orig in
  List.map R.maude_id
    (dedup (R.defined_heads sys @ List.map fst (native_delegated sys)))

(* -------------------------------------------------------------------------- *)
(* Start-term encoding: an IL [value] (e.g. a program parsed by a language
   front-end) to a ground Maude term in this module's vocabulary. *)

(* A symbol the way it appears in the emitted module (sanitized + mangled). *)
let maude_sym (s : string) : string = R.maude_id (R.sanitize s)

(* A region-independent key for a [mixop], equivalent to [Mixfix.eq_mixop]:
   [compare_mixop] compares atoms by [Xl.Atom.compare] (= [Stdlib.compare] on the
   region-free [Atom.t]) and orders [Arg] before [Atom], so dropping each atom's
   region and erasing [Arg]'s payload yields a value whose structural equality
   matches [eq_mixop]. Lets the encoder resolve a case by hash lookup instead of
   rescanning the spec. *)
let mixop_key (mixop : Lang.Il.mixop) : Xl.Atom.t option list =
  List.map (function Mixfix.Arg () -> None | Mixfix.Atom a -> Some a.it) mixop

(* Per-spec index for [encode_value]'s constructor resolution, built once: a
   CaseV node would otherwise rescan every variant def in the (large) spec, so
   the bare per-node cost was O(nodes x cases). Two tables, both keyed by the
   case's notation [mixop]:
   - [field_typs]: (declaring origin, mixop) -> the case's declared field types,
     first declaration winning (mirrors the old [List.find_map] over defs then
     typcases).
   - [origins]: mixop -> [(owning type, declaring origin)] in declaration order,
     from which the encoder picks the declaring origin by expected/noted/
     unique-all priority. (The old code also matched arity, but mixop equality
     already implies equal [Arg] count, so the mixop key subsumes it.) *)
type encode_index = {
  field_typs : (string * Xl.Atom.t option list, typ' list) Hashtbl.t;
  origins : (Xl.Atom.t option list, (string * string) list) Hashtbl.t;
}

let build_encode_index (orig : spec) : encode_index =
  let field_typs = Hashtbl.create 512 in
  let origins = Hashtbl.create 512 in
  List.iter
    (fun def ->
      match def.it with
      | TypD { synid = tid; deftyp = { it = VariantT typcases; _ }; _ } ->
          List.iter
            (fun (tc : typcase) ->
              let nottyp = tc.notation in
              let key = mixop_key (Mixfix.to_mixop nottyp.it) in
              let fkey = (tid.it, key) in
              if not (Hashtbl.mem field_typs fkey) then
                Hashtbl.replace field_typs fkey
                  (List.map (fun t -> t.it) (Mixfix.args nottyp.it));
              let origin, _ = case_origin_mixop tc in
              let prev =
                Option.value (Hashtbl.find_opt origins key) ~default:[]
              in
              Hashtbl.replace origins key ((tid.it, origin) :: prev))
            typcases
      | _ -> ())
    orig;
  (* Entries were prepended per key; restore declaration order. Snapshot the
     keys first -- replacing values while iterating the table is unspecified. *)
  let keys = Hashtbl.fold (fun k _ acc -> k :: acc) origins [] in
  List.iter
    (fun k -> Hashtbl.replace origins k (List.rev (Hashtbl.find origins k)))
    keys;
  { field_typs; origins }

(* One-slot memo (physical equality on the spec, like {!meta_sig_memo}): one
   [run] encodes every batched start term against the same spec value. *)
let encode_index_memo : (spec * encode_index) option ref = ref None

let encode_index (orig : spec) : encode_index =
  match !encode_index_memo with
  | Some (o, idx) when o == orig -> idx
  | _ ->
      let idx = build_encode_index orig in
      encode_index_memo := Some (orig, idx);
      idx

(* Encode a value to a ground term in the native theory: scalars become
   wrapped built-in literals ({!Maude_theory}), so a program identifier is a
   [String] and a numeral never builds a Peano tower (no [Bigint] overflow).
   [expected] is the type the surrounding position wants, used only to put a
   numeric leaf in the [int] vs [nat] wrapper. *)
let encode_value (orig : spec) (v : value) : R.term =
  let idx = encode_index orig in
  (* The declared field types of the variant case [origin]/[mixop], so a numeric
     leaf can be coerced to the [int]/[nat] the case expects. *)
  let case_field_typs (origin : string) (mixop : Lang.Il.mixop) :
      typ' list option =
    Hashtbl.find_opt idx.field_typs (origin, mixop_key mixop)
  in
  let rec enc (expected : typ' option) (v : value) : R.term =
    match v.it with
    | BoolV b -> Maude_theory.bool_t b
    | NumV num -> (
        let i = Xl.Num.to_int num in
        (* Respect the value's own [Num] tag first: a genuinely-[Int] numeral
           stays in the [int] wrapper even when the surrounding position's
           expected type is generic (an array index [a[0]], a bit literal's
           value, a slice bound) -- otherwise it would reduce to [nat] where the
           interpreter keeps [int] (the front-end parses bare integer literals as
           [`Int], see [lexer.mll]). [expected] still upcasts a [Nat] value
           sitting in an [int] position (nat <: int). *)
        match (num, expected) with
        | `Int _, _ | _, Some (NumT `IntT) -> Maude_theory.int_t i
        | _ -> Maude_theory.nat_t i)
    | TextV s -> Maude_theory.text_t s
    | OptV None -> T.none_t
    | OptV (Some v) -> T.some_t (enc None v)
    | ListV vs ->
        List.fold_right (fun v acc -> T.cons_t (enc None v) acc) vs T.nil_t
    | TupleV vs -> T.tuple_t (List.map (enc None) vs)
    | StructV fields ->
        let name = Option.value (typ_name_of v.note.typ) ~default:"anon" in
        T.app_t (T.struct_sym name)
          (List.map (fun (_, fv) -> enc None fv) fields)
    | CaseV vc ->
        let noted = Option.value (typ_name_of v.note.typ) ~default:"anon" in
        (* The type the PARENT field declares for this position, threaded down
           via [case_field_typs] as [expected]. This is the spec's own truth, so
           it outranks the value's note when the two disagree. *)
        let expected_name = Option.bind expected typ_name_of in
        let mixop = Mixfix.to_mixop vc in
        let args = Mixfix.args vc in
        (* The declared constructor is spelled with the case's DECLARING
           origin ([case_origin_mixop]), which the value's note need not
           name: the note may be a UNION the case is injected into
           (dataExpression for a sequenceOrRecordExpression case), or a stale
           front-end spelling the loaded spec renamed (the spec-independent
           front-end keeps `methodPrototypeList`, but the new spec renamed it
           `externConstructorOrMethodPrototypeList`; `number` vs the new spec's
           `integerLiteral`; ...). The list-cons case shape is shared by every
           `X*` type, so resolving by case shape alone is ambiguous and the old
           fallback emitted the stale note name -- an operator the spec's module
           never declares (Maude: "no parse for term"). Resolve via the EXPECTED
           type's cases first, then the noted type's; otherwise accept the
           declaring origin only if the whole spec agrees on exactly one. *)
        let entries =
          Option.value
            (Hashtbl.find_opt idx.origins (mixop_key mixop))
            ~default:[]
        in
        let expected_origins =
          List.filter_map
            (fun (owner, o) ->
              if Some owner = expected_name then Some o else None)
            entries
        in
        let noted_origins =
          List.filter_map
            (fun (owner, o) -> if owner = noted then Some o else None)
            entries
        in
        let all_origins = List.map snd entries in
        let origin =
          match
            (expected_origins, noted_origins, List.sort_uniq compare all_origins)
          with
          | o :: _, _, _ -> o
          | [], o :: _, _ -> o
          | [], [], [ o ] -> o
          | [], [], _ -> noted
        in
        let ftyps =
          match case_field_typs origin mixop with
          | Some ts when List.length ts = List.length args ->
              List.map Option.some ts
          | _ -> List.map (fun _ -> None) args
        in
        T.variant_t origin mixop (List.map2 enc ftyps args)
    | FuncV _ -> T.nil_t
    (* impty base has no function values *)
  in
  enc None v

(* -------------------------------------------------------------------------- *)
(* Meta-level (reflective) start-term encoding.

   A start term encoded in the spec's own object syntax ([Program-ok(<huge
   term>)]) is parsed by Maude through the module's giant mixfix signature
   (thousands of operators under the universal sort [Val]) -- the dominant
   per-program cost (~7s for a small P4 program, vs ~0.4s to parse the module
   and 0ms to rewrite). The reflective META-TERM grammar is fixed and tiny, so
   {!Maude_run} feeds the term to [metaReduce(upModule('SPEC, false), <meta>)]
   instead: an operator application [f(a..)] becomes the meta-term ['f[a..]], a
   0-arity constant ['f.Sort], and the heavy module reflection ([upModule]) is
   paid once for a whole batch. *)

(* One-slot memo (physical equality on the spec, like {!Pipeline.maude_memo}):
   one [run] encodes every batched start term against the same spec value, so
   the recovered signature is built once rather than per program. *)
let meta_sig_memo : (spec * (string -> int -> string list * string)) option ref
    =
  ref None

let meta_signature (orig : spec) : string -> int -> string list * string =
  match !meta_sig_memo with
  | Some (o, sg) when o == orig -> sg
  | _ ->
      (* Same form signature recovery reads in [module_of_system]: the
         defunctionalized spec, whose specialized copies' declarations the
         encoder's symbols refer to. *)
      let orig' = Defunctionalize.defunctionalize orig in
      let tbl, _ = recover Native orig' (type_env orig') in
      let sg sym arity = signature tbl sym arity in
      meta_sig_memo := Some (orig, sg);
      sg

(* A non-negative numeral's META-TERM. Maude reflects a built-in [Nat] as the
   iterated successor ['s_^N['0.Zero]] (the plain constant ['N.Nat] does NOT
   parse back through [metaReduce]); zero is ['0.Zero]. Verified with [upTerm]. *)
let meta_nat_lit (n : string) : string =
  if n = "0" then "'0.Zero" else Printf.sprintf "'s_^%s['0.Zero]" n

(* A signed numeral's META-TERM: a negative magnitude wraps the [Nat] form in
   the built-in unary minus (['-_[..]]); non-negative is the [Nat] form. *)
let meta_int_lit (n : string) : string =
  if String.length n > 0 && n.[0] = '-' then
    Printf.sprintf "'-_[%s]"
      (meta_nat_lit (String.sub n 1 (String.length n - 1)))
  else meta_nat_lit n

(* An [R.term] (a ground encoded value) as a Maude META-TERM. The four built-in
   scalar wrappers print their reflected built-in literal directly; every other
   constructor is an ['op[..]] application, or ['op.Sort] at arity 0 (the sort
   recovered from the signature, as the META-TERM grammar requires it on a
   constant). *)
let rec print_meta_term sg (t : R.term) : string =
  match t with
  | R.Var v -> "'" ^ R.maude_var v ^ ":" ^ val_sort
  | R.App (w, [ R.App (n, []) ]) when w = Maude_theory.nat_wrap_sym ->
      Printf.sprintf "'%s[%s]" (R.maude_id w) (meta_nat_lit n)
  | R.App (w, [ R.App (n, []) ]) when w = Maude_theory.int_wrap_sym ->
      Printf.sprintf "'%s[%s]" (R.maude_id w) (meta_int_lit n)
  | R.App (w, [ R.App (b, []) ]) when w = Maude_theory.bool_wrap_sym ->
      Printf.sprintf "'%s['%s.Bool]" (R.maude_id w) b
  | R.App (w, [ R.App (s, []) ]) when w = Maude_theory.text_wrap_sym ->
      Printf.sprintf "'%s['%s.String]" (R.maude_id w) s
  | R.App (f, []) -> Printf.sprintf "'%s.%s" (R.maude_id f) (snd (sg f 0))
  | R.App (f, args) ->
      Printf.sprintf "'%s[%s]" (R.maude_id f)
        (String.concat ", " (List.map (print_meta_term sg) args))

(* Encode [value] to its Maude META-TERM text (self-contained: literals carry
   their own bytes). *)
let meta_term_of_value (orig : spec) (v : value) : string =
  print_meta_term (meta_signature orig) (encode_value orig v)

(* The start application of relation [rel] on already-encoded META-TERM [args],
   as a META-TERM ['rel[args]]. When the translated system threads [rel] with the
   gensym state ({!Gensym}), the seed is appended and the run normalizes to
   [tuple(result, final-state)] instead of the bare result. *)
let meta_start_app (orig : spec) (rel : string) (args : string list) : string =
  let threaded = Gensym.effectful_syms (Pipeline.maude_system_of_spec orig) in
  let args =
    if List.mem (R.sanitize rel) threaded then
      args
      @ [
          print_meta_term (meta_signature orig)
            (Maude_theory.text_t Gensym.seed_text);
        ]
    else args
  in
  Printf.sprintf "'%s[%s]" (maude_sym rel) (String.concat ", " args)
