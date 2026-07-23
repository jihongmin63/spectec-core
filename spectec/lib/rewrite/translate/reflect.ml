(* Analysis-only owise reflection.

   A SpecTec [-- otherwise] clause fires exactly when no earlier sibling clause
   applies. The execution surface keeps that as Maude's [owise] attribute, but
   the analysis (MFE) surface cannot: the Church-Rosser checker ignores [owise]
   when it builds critical pairs, so every owise/sibling overlap surfaces as a
   spurious non-joinable pair (todo.md M1). This pass makes the semantics
   explicit instead: for each owise rule it builds, per preceding sibling, a
   total boolean term [g_i] meaning "sibling i applies", replaces the owise
   marker with the condition

     or(g_1, .. , g_k) = false

   and clears the [owise] flag. The CRC then discharges the owise/sibling
   pairs itself: under a critical-pair unifier the guard components reduce to
   the SAME subject terms the sibling's own conditions use, so the sibling
   conditions -- taken as rewrite hypotheses -- collapse the guard to
   [true = false], an infeasible pair. (Calibrated against the MFE: it
   rewrites conditions with each other as hypotheses but does NOT narrow over
   constructors, so this same-subject alignment is the load-bearing
   requirement.)

   [g_i] is built from the TRANSLATED sibling rule (its CTRS head pattern and
   conditions), never re-translated from the IL, so the guard's subterms are
   spelled exactly like the sibling's conditions:
   - a variant head/sub-pattern [K(ps)] becomes [match_<T>_K(subject)] (the
     total per-type matcher) plus recursion into [ps] over fresh projections
     [proj_K_i(subject)];
   - struct and tuple patterns project without a test (single constructor);
   - list/option patterns use the prelude's total [match_cons]/[match_nil]/
     [match_some]/[match_none];
   - a fully-ground non-variant pattern is one [eqg] test (the reflexive
     guard equality -- see [eqg_sym] for why the structural [eq] must not
     appear in a guard);
   - a condition [(l, true)] contributes [l], [(l, false)] contributes
     [not(l)], a binding condition [(l, v)] extends the substitution with
     [v := l], a destructuring [(l, K(ps))] tests the matcher on [l], and any
     other [(l, r)] becomes [eqg(l, r)].
   The prelude's short-circuit booleans ([and(false, y) = false]) make the
   projections safe: a projection only ever sits to the right of the matcher
   that guards it.

   Not every owise clause is reflectable yet. The pass SKIPS (keeping the
   owise flag; {!Mfe} warns when such a rule reaches the checker) any symbol
   whose siblings involve: a relation call (needs the R? judgment reflection,
   the planned Phase 3), an iteration helper without a success reflection
   ([$iterall]/[$itercollect] when THIS attempt could not build
   one for them; [$itermap]/[$iterproj] are pure stream transformers
   and never gated), a gensym state-threaded symbol, or a pattern/condition
   shape it cannot type against the spec. Each skip is reported on stderr
   with its reason, so the gate doubles as the follow-up worklist.

   [$iterall]/[$itercollect] each get a "does this iteration
   succeed" total-boolean reflection [holds_$iterall../$itercollect..]
   built the same way as a judgment's [holds_R] (see the
   judgment reflection section below): a base case of [true], and a step
   case that ANDs the inner premise's reflected conditions with the
   recursive call. [$itercollect]'s own conditions never
   carry a bool rhs (they always bind the collected value), so
   {!replace_cond} never rewrites them -- instead, once a helper is
   success-reflected, EVERY rule (owise sibling or not) whose conditions
   bind its result gains an explicit [holds_<helper>(args) = true] test
   immediately before that binding (see the "global success test insertion"
   step in {!owise} below). That is new: earlier reflection only respelled
   existing conditions in place, one-to-one; this inserts a condition,
   system-wide, not only for owise. It is still analysis-only (the execution
   pipeline never runs this pass) and semantically sound as an added
   condition (a successful collection implies its own success judgment).

   Matcher/accessor/projection rules referenced by a guard are (re)generated
   here when the system does not already define them -- [of_spec]'s
   [prune_unused] runs before this pass and may have dropped a matcher family
   no translated rule needed. *)

open Common.Source
open Lang.Il
module R = Rewrite_system
module T = Ctrs_term

(* -------------------------------------------------------------------------- *)
(* Tables read off the (defunctionalized) spec. *)

type tables = {
  typdefs : (string, deftyp') Hashtbl.t; (* type name -> definition *)
  ctor_types : (string, string list) Hashtbl.t; (* variant sym -> type names *)
  funcsigs : (string, typ list * typ) Hashtbl.t; (* $f -> params, result *)
  relsigs : (string, typ list) Hashtbl.t; (* Rel -> input types *)
  fieldsigs : (string, typ') Hashtbl.t; (* field_<ty>_<a> -> field type *)
  rel_outs : (string, typ list) Hashtbl.t; (* Rel -> output types *)
}

let build_tables (orig : spec) : tables =
  let tbl =
    {
      typdefs = Hashtbl.create 64;
      ctor_types = Hashtbl.create 256;
      funcsigs = Hashtbl.create 64;
      relsigs = Hashtbl.create 64;
      fieldsigs = Hashtbl.create 256;
      rel_outs = Hashtbl.create 64;
    }
  in
  List.iter
    (fun (def : def) ->
      match def.it with
      | TypD { synid; deftyp; _ } -> (
          if not (Hashtbl.mem tbl.typdefs synid.it) then
            Hashtbl.add tbl.typdefs synid.it deftyp.it;
          match deftyp.it with
          | VariantT typcases ->
              List.iter
                (fun (tc : typcase) ->
                  let { synid = oid; _ } = tc.origin.it in
                  let ctor =
                    T.variant_sym oid.it (Mixfix.to_mixop tc.notation.it)
                  in
                  let tys =
                    Option.value
                      (Hashtbl.find_opt tbl.ctor_types ctor)
                      ~default:[]
                  in
                  if not (List.mem synid.it tys) then
                    Hashtbl.replace tbl.ctor_types ctor (tys @ [ synid.it ]))
                typcases
          | StructT fields ->
              List.iter
                (fun ((a, ft) : typfield) ->
                  Hashtbl.replace tbl.fieldsigs (T.field_sym synid.it a) ft.it)
                fields
          | _ -> ())
      | DecD { defid; params; typ; _ } ->
          let exps =
            List.filter_map
              (fun p -> match p.it with ExpP t -> Some t | DefP _ -> None)
              params
          in
          if List.length exps = List.length params then
            Hashtbl.replace tbl.funcsigs (T.func_sym defid) (exps, typ)
      | RelD { relid; reltyp; _ } ->
          let typs = Mixfix.args (Mode.notation reltyp.it) in
          let idxs = List.init (List.length typs) Fun.id in
          let ins, outs = Mode.partition reltyp.it idxs in
          Hashtbl.replace tbl.relsigs (T.rel_sym relid)
            (List.map (List.nth typs) ins);
          Hashtbl.replace tbl.rel_outs (T.rel_sym relid)
            (List.map (List.nth typs) outs)
      | BuiltinDecD _ -> ())
    orig;
  tbl

(* Unwrap plain aliases down to a variant/struct/structural type. *)
let rec resolve (tbl : tables) (t : typ') : typ' =
  match t with
  | VarT { synid; _ } -> (
      match Hashtbl.find_opt tbl.typdefs synid.it with
      | Some (PlainT u) -> resolve tbl u.it
      | _ -> t)
  | _ -> t

(* The case of variant type [ty] whose generated symbol is [ctor]:
   its mixop, field types, and [ty]'s case count. *)
let variant_case (tbl : tables) (ty : string) (ctor : string) :
    (mixop * typ list * int) option =
  match Hashtbl.find_opt tbl.typdefs ty with
  | Some (VariantT typcases) ->
      List.find_map
        (fun (tc : typcase) ->
          let { synid = oid; _ } = tc.origin.it in
          let mixop = Mixfix.to_mixop tc.notation.it in
          if T.variant_sym oid.it mixop = ctor then
            Some (mixop, Mixfix.args tc.notation.it, List.length typcases)
          else None)
        typcases
  | _ -> None

(* -------------------------------------------------------------------------- *)
(* (B) discriminator hoist: respell an opaque matcher test [match_K(v) = true]
   as the structural equation [v = K(fresh..)], for a bare-variable subject
   [v] that no OTHER condition of the same rule mentions. The reverse table
   (matcher symbol -> its constructor + arity) mirrors [build_tables]'s
   [ctor_types] construction, walked backwards.

   [Crc_surface.fold_premise_binders] only recognizes a condition shaped
   [Var v = K(..)] / [K(..) = Var v], so an opaque [match_K(v) = true] guard
   never qualifies -- this respelling is what lets a head-bound discriminator
   variable fold into the constructor pattern the guard tests, turning a
   guarded multi-clause dispatch (several rules sharing one head, their
   disjointness carried only by opaque [match_*] conditions the CRC cannot
   see through) into genuinely disjoint head patterns. Whether the fold
   actually fires is otherwise entirely [fold_premise_binders]'s call.

   A clause whose head-bound [v] ALSO feeds a companion destructuring
   condition (the [let K(x, y) = v] SpecTec's elaborator emits alongside the
   [matches K] guard as two separate premises) cannot be respelled that way:
   both conditions would still mention [v], so the fresh [v = K(fresh..)]
   would be inert and [fold_premise_binders] would keep refusing the
   companion (used-elsewhere). The two passes deadlock on each other's
   leftover, and the head stays a bare variable -- costing the fold's two
   payoffs at once: disjoint head patterns (CRC) and a syntactically
   decreasing recursive argument (termination; the descent lives only in the
   premise, where a dependency-pair analysis cannot see it).

   So for that shape the matcher is DROPPED instead ([restated_by_destructure]):
   [match_K(v) = true] is implied by [v = K(..)] naming the same [K], so
   removing it preserves the clause's applicability while freeing the
   companion to fold into the head -- where the pattern [K(..)] then states
   the disjointness the matcher used to carry. Families still holding an
   [owise] rule are exempt ([owise_head_syms]): there the CRC refutes the
   negation guard {!owise} emits only by rewriting it with a sibling's
   IDENTICAL guard term, so dropping that sibling's matcher would strip the
   very hypothesis the discharge rests on (impty's [$lookup]).

   Runs over the FLAT rule list {!To_ctrs} produced, so a matcher condition
   nested inside an [IterPr] helper's step rule is covered the same way, with
   no separate recursive scan. *)

type matcher = { ctor : string; arity : int }

let build_matcher_table (orig : spec) : (string, matcher) Hashtbl.t =
  let tbl = Hashtbl.create 256 in
  List.iter
    (fun (def : def) ->
      match def.it with
      | TypD { synid; deftyp = { it = VariantT typcases; _ }; _ } ->
          List.iter
            (fun (tc : typcase) ->
              let { synid = oid; _ } = tc.origin.it in
              let mixop = Mixfix.to_mixop tc.notation.it in
              let msym = T.match_sym synid.it mixop in
              if not (Hashtbl.mem tbl msym) then
                Hashtbl.add tbl msym
                  {
                    ctor = T.variant_sym oid.it mixop;
                    arity = Mixfix.arity mixop;
                  })
            typcases
      | _ -> ())
    orig;
  List.iter
    (fun (msym, m) -> Hashtbl.replace tbl msym m)
    [
      ("match_some", { ctor = "some"; arity = 1 });
      ("match_none", { ctor = "none"; arity = 0 });
      ("match_cons", { ctor = "cons"; arity = 2 });
      ("match_nil", { ctor = "nil"; arity = 0 });
    ];
  tbl

(* The head symbols whose clause family still carries an [owise] rule. Such a
   family's disjointness is discharged by {!owise}'s negation guard, which the
   CRC only refutes when a sibling states the SAME guard term it can rewrite
   with -- so a sibling there keeps its matcher (see [drop_restated_matcher]). *)
let owise_head_syms (sys : R.t) : string list =
  List.filter_map
    (fun (r : R.rule) ->
      match (r.R.owise, r.R.lhs) with true, R.App (f, _) -> Some f | _ -> None)
    sys.R.rules
  |> R.dedup_stable

let head_sym (t : R.term) : string option =
  match t with R.App (f, _) -> Some f | R.Var _ -> None

let hoist_matchers ~(scalars : T.scalar_theory) ~(orig : spec) (sys : R.t) : R.t
    =
  let tbl = build_matcher_table orig in
  let yes = T.bool_t ~scalars true in
  let owise_heads = owise_head_syms sys in
  let hoist_rule (r : R.rule) : R.rule =
    let n = ref 0 in
    let fresh () =
      let i = !n in
      incr n;
      Printf.sprintf "hoist_%d" i
    in
    let guarded_family =
      match head_sym r.R.lhs with
      | Some f -> List.mem f owise_heads
      | None -> true
    in
    let orig_conds = Array.of_list r.R.conds in
    let mentions (v : string) ((l, rr) : R.cond) : bool =
      R.count_var v l > 0 || R.count_var v rr > 0
    in
    let others i (v : string) : R.cond list =
      Array.to_list orig_conds
      |> List.filteri (fun j _ -> j <> i)
      |> List.filter (mentions v)
    in
    (* [v]'s ONLY other occurrence destructures it against the very constructor
       the matcher tests, so [match_K(v) = true] restates what [v = K(..)]
       already says: dropping it loses nothing and frees
       {!Crc_surface.fold_premise_binders} (whose "used in no other
       condition" gate the matcher itself was tripping) to fold the destructure
       into the head. *)
    let restated_by_destructure i (v : string) (ctor : string) : bool =
      match others i v with
      | [ (R.Var u, R.App (f, _)) ] | [ (R.App (f, _), R.Var u) ] ->
          u = v && f = ctor
      | _ -> false
    in
    let hoist_cond i ((l, r) : R.cond) : R.cond option =
      match l with
      | R.App (msym, [ R.Var v ]) when r = yes -> (
          match Hashtbl.find_opt tbl msym with
          | None -> Some (l, r)
          | Some { ctor; arity } ->
              if others i v = [] then
                Some
                  ( R.Var v,
                    T.app_t ctor (List.init arity (fun _ -> T.var_t (fresh ())))
                  )
              else if (not guarded_family) && restated_by_destructure i v ctor
              then None
              else Some (l, r))
      | _ -> Some (l, r)
    in
    let conds =
      List.mapi hoist_cond (Array.to_list orig_conds) |> List.filter_map Fun.id
    in
    { r with R.conds }
  in
  let rules = List.map hoist_rule sys.R.rules in
  R.of_rules rules

(* -------------------------------------------------------------------------- *)
(* Comparison / negation guard alignment.

   The CRC discharges a conditional critical pair's infeasibility only when two
   of its conditions reduce to the SAME subject term equated to [true] versus
   [false] (it rewrites the conditions as mutual hypotheses; it does not narrow
   the subject). Sibling clauses that split on complementary comparisons -- e.g.
   [$bin_shr]'s arithmetic shift ([i < 0]) versus logical shift ([i >= 0]) --
   translate to [lt_int(X, 0) = true] in one and [leq_int(0, X) = true] in the
   other, because {!Ctrs_term.term_of_cmpop} normalizes [>=] to a swapped [leq].
   Those are two different subject terms, so the CRC keeps the pair as a
   spurious [MAYBE]. The prelude's [lt_int(x,y) = not(leq_int(y,x))] bridge does
   NOT repair it: here [X] is a [$bitstr_to_int(..)] whose recovered sort is the
   top [Val], while the bridge is declared over [IntV] (a subsort), so it never
   fires on [X] -- measured, the sign split survives as 6 critical pairs.

   This pass respells each top-level comparison guard to the canonical
   [leq]/[leq_int] predicate at inverted polarity, so the two siblings end up
   stating the SAME [leq_int(0, X)] term and the CRC discharges the pair by
   hypothesis (measured: 6 pairs -> 0). A leading [not(..)] guard is flattened
   the same way, lining a negated [match_K]/subtype/boolean-function test up
   with its positive twin.

   Satisfiability-equivalent in the structural prelude: [lt(a,b) ->* true] iff
   [leq(b,a) ->* false] (both read the same total [bcompare]; the [leq_int] sign
   rules are likewise total), and [not(X) ->* true] iff [X ->* false]. The swap
   preserves the variable set. Analysis-only, like {!owise} -- the executable
   surface keeps the original guards. *)
let align_guards ~(scalars : T.scalar_theory) (sys : R.t) : R.t =
  let tru = T.bool_t ~scalars true in
  let fls = T.bool_t ~scalars false in
  let flip b = if b then fls else tru in
  (* One rewrite step toward canonical form; [None] when nothing applies. *)
  let step ((l, r) : R.cond) : R.cond option =
    let polarity =
      if r = tru then Some true else if r = fls then Some false else None
    in
    match (l, polarity) with
    | R.App ("not", [ x ]), Some p -> Some (x, flip p)
    | R.App ("lt", [ a; b ]), Some p -> Some (T.leq_t b a, flip p)
    | R.App ("lt_int", [ a; b ]), Some p -> Some (T.leq_int_t b a, flip p)
    | _ -> None
  in
  let rec normalize (c : R.cond) : R.cond =
    match step c with Some c' -> normalize c' | None -> c
  in
  let rules =
    List.map
      (fun (r : R.rule) -> { r with R.conds = List.map normalize r.R.conds })
      sys.R.rules
  in
  { sys with R.rules }

(* -------------------------------------------------------------------------- *)
(* Skip conditions. *)

exception Gate of string

(* The iteration helpers a guard may NOT mention raw UNLESS a totalized
   [holds_] counterpart has already been generated for it -- [succ] is the
   currently-assumed-successful set (an in-progress attempt's candidates
   while judgment reflection is still generating [holds_] rules, the final
   successful set afterwards). The pure stream transformers
   [$itermap]/[$iterproj] are allowed unconditionally: they call no
   relation, and they enter a guard only through a binding condition the
   sibling itself carries with the same spelling, so the hypothesis
   alignment is preserved. *)
let iter_helper_prefixes = [ "$iterall"; "$itercollect" ]

let has_prefix p s =
  String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* Reject a guard component that mentions a relation call whose success is
   not reflected (not in [succ]), an iteration helper not yet in [succ], or a
   gensym state-threaded symbol. *)
let rec check_reflectable (tbl : tables) (effectful : string list)
    (succ : string list) (t : R.term) : unit =
  match t with
  | R.Var _ -> ()
  | R.App (f, args) ->
      if Hashtbl.mem tbl.relsigs f && not (List.mem f succ) then
        raise (Gate (Printf.sprintf "relation call %s" f));
      if
        (not (List.mem f succ))
        && List.exists (fun p -> has_prefix p f) iter_helper_prefixes
      then raise (Gate (Printf.sprintf "iteration helper %s" f));
      if List.mem f effectful then
        raise (Gate (Printf.sprintf "gensym-threaded %s" f));
      List.iter (check_reflectable tbl effectful succ) args

let rec term_vars (t : R.term) : string list =
  match t with
  | R.Var v -> [ v ]
  | R.App (_, args) -> List.concat_map term_vars args

let rec subst (s : (string * R.term) list) (t : R.term) : R.term =
  match t with
  | R.Var v -> ( match List.assoc_opt v s with Some u -> u | None -> t)
  | R.App (f, args) -> R.App (f, List.map (subst s) args)

let rec ground (t : R.term) : bool =
  match t with R.Var _ -> false | R.App (_, args) -> List.for_all ground args

(* -------------------------------------------------------------------------- *)
(* (B') subty-guard head specialization: expand a clause guarded by a
   membership test [subty_<S>(v) = true] on a head-bound variable [v] into one
   clone per member constructor of [S], substituting [v := K_i(fresh..)]
   through the whole rule and dropping the guard (keeping the member rule's
   payload conjunction as a residual condition when it is not literally
   [true]). Subtype totality makes this exact: the [subty_<S>] rule family
   enumerates the guard's true-set syntactically -- member cases rewrite to
   their payload conjunction, non-members to [false] ({!To_ctrs}'s use-based
   complement) -- so the clones' application set equals the guarded clause's.
   Unlike {!hoist_matchers} (a 1-rule -> 1-rule respelling for a matcher that
   names ONE constructor), this is a 1 -> N fan-out, and the substitution runs
   through companion conditions too, so each clone is partially evaluated:
   a matcher or subty test applied to the now-concrete constructor decides to
   [true] (condition dropped), [false]/stuck (clone dead, dropped), or its
   payload residual; a companion destructuring equation [K(sf..) = K(pats..)]
   decomposes pointwise and the fresh payload variables are renamed away into
   the sibling's own binder names (deepening the head pattern exactly like the
   fold this pass makes unnecessary). Anything the evaluator does not
   recognize is conservatively kept as a condition for the later passes.
   Analysis-only, like {!hoist_matchers}/{!owise}; fresh variable names are
   gensym-numbered ([expand_N]) so they cannot collide with a {!Var_hints} key
   and inherit their sorts positionally from the constructor signature. *)

(* One [subty_<S>] family, scanned off the translated system itself (the
   guard's presence keeps the family from being pruned, and scanning avoids
   re-deriving {!To_ctrs.sub_pred}'s spelling): the member constructors with
   their payload variables and residual rhs, or a degenerate shape. *)
type subty_family =
  | Members of (string * string list * R.term) list
    (* ctor, payload vars, residual rhs (the payload conjunction) *)
  | Always_true (* [subty_<S>(x) -> true]: type parameter fallback *)
  | Opaque of string (* unrecognized shape: do not expand *)

let expand_subty_guards ~(scalars : T.scalar_theory) ~(orig : spec) (sys : R.t)
    : R.t =
  let tbl = build_tables orig in
  let mtbl = build_matcher_table orig in
  let yes = T.bool_t ~scalars true in
  let no = T.bool_t ~scalars false in
  let by_head : (string, R.rule) Hashtbl.t = Hashtbl.create 1024 in
  List.iter
    (fun (r : R.rule) ->
      match r.R.lhs with
      | R.App (f, _) -> Hashtbl.add by_head f r
      | R.Var _ -> ())
    sys.R.rules;
  let rules_of_head f = List.rev (Hashtbl.find_all by_head f) in
  let is_ctor c =
    Hashtbl.mem tbl.ctor_types c
    || List.mem c [ "tuple"; "cons"; "nil"; "none"; "some" ]
    || has_prefix "struct_" c
  in
  (* Family scan, memoized; alias delegation [subty_<S>(x) -> subty_<U>(x)]
     chases to the target family (cycle-guarded). *)
  let fam_cache : (string, subty_family) Hashtbl.t = Hashtbl.create 64 in
  let distinct_vars ps =
    let vs = List.filter_map (function R.Var v -> Some v | _ -> None) ps in
    if
      List.length vs = List.length ps
      && List.sort_uniq compare vs = List.sort compare vs
    then Some vs
    else None
  in
  let rec family (seen : string list) (s : string) : subty_family =
    match Hashtbl.find_opt fam_cache s with
    | Some f -> f
    | None ->
        let f =
          if List.mem s seen then Opaque "alias cycle"
          else scan (s :: seen) (rules_of_head s)
        in
        Hashtbl.replace fam_cache s f;
        f
  and scan seen rules =
    let step acc (r : R.rule) =
      match acc with
      | Opaque _ -> acc
      | _ when r.R.conds <> [] -> Opaque "conditional subty rule"
      | _ -> (
          match (r.R.lhs, r.R.rhs) with
          | R.App (_, [ R.App (_, _) ]), rhs when rhs = no ->
              acc (* complement *)
          | R.App (_, [ R.App (c, ps) ]), rhs -> (
              match distinct_vars ps with
              | Some vs
                when List.for_all (fun v -> List.mem v vs) (term_vars rhs) -> (
                  match acc with
                  | Members ms -> Members (ms @ [ (c, vs, rhs) ])
                  | Always_true -> Opaque "mixed member/fallback"
                  | Opaque _ -> acc)
              | _ -> Opaque "non-linear member payload")
          | R.App (_, [ R.Var _ ]), rhs when rhs = yes -> (
              match acc with
              | Members [] -> Always_true
              | Members _ -> Opaque "mixed member/fallback"
              | acc -> acc)
          | R.App (_, [ R.Var x ]), R.App (g, [ R.Var x' ])
            when x' = x && has_prefix "subty_" g -> (
              match (acc, family seen g) with
              | Members [], f -> f
              | _ -> Opaque "mixed delegation")
          | _ -> Opaque "unrecognized subty rule shape")
    in
    if rules = [] then Opaque "no defining rules"
    else List.fold_left step (Members []) rules
  in
  (* Partial evaluation of one clone's conditions after the [v := K(sf..)]
     substitution. Returns [None] when a condition is decided unsatisfiable
     (dead clone): a matcher or subty test on a concrete NON-member
     constructor either rewrites to [false] (a sibling case) or is stuck
     (outside the type) -- unsatisfiable either way, since constructors are
     free (a constructor-headed term never rewrites at the root).
     [fresh_set] are the variables THIS pass introduced: only those may be
     renamed away by a decomposed destructuring pair (a general [Var = term]
     condition is a join test, not a definition). *)
  let rec app_heads (t : R.term) : string list =
    match t with
    | R.Var _ -> []
    | R.App (f, args) -> f :: List.concat_map app_heads args
  in
  let pattern_safe t = List.for_all is_ctor (app_heads t) in
  let exception Dead in
  let simplify (fresh_set : (string, unit) Hashtbl.t) (r0 : R.rule) :
      R.rule option =
    let subst_rule s (r : R.rule) =
      {
        r with
        R.lhs = subst s r.R.lhs;
        rhs = subst s r.R.rhs;
        conds = List.map (fun (l, rr) -> (subst s l, subst s rr)) r.R.conds;
      }
    in
    let rec go fuel (r : R.rule) : R.rule =
      if fuel = 0 then r
      else
        let lhs_vars = term_vars r.R.lhs in
        (* a condition variable the head does not bind: existentially
           quantified, so a [w = t] condition on it (t a pure constructor
           term, no computation to duplicate) is eliminated exactly by
           substituting [w := t] through the rule *)
        let existential w t =
          (not (List.mem w lhs_vars))
          && (not (List.mem w (term_vars t)))
          && pattern_safe t
        in
        (* one pass: rewrite the first condition a step applies to *)
        let rec step pre = function
          | [] -> None
          | (l, rr) :: rest when l = rr ->
              Some { r with R.conds = List.rev_append pre rest }
          | (R.App (m, [ R.App (c, _) ]), rr) :: rest
            when rr = yes && is_ctor c && Hashtbl.mem mtbl m ->
              if c = (Hashtbl.find mtbl m).ctor then
                Some { r with R.conds = List.rev_append pre rest }
              else raise Dead
          | ((R.App (s, [ R.App (c, args) ]), rr) as cond) :: rest
            when rr = yes && has_prefix "subty_" s && is_ctor c -> (
              match family [] s with
              | Always_true ->
                  Some { r with R.conds = List.rev_append pre rest }
              | Members ms -> (
                  match List.find_opt (fun (mc, _, _) -> mc = c) ms with
                  | Some (_, ps, residual)
                    when List.length ps = List.length args ->
                      let resid = subst (List.combine ps args) residual in
                      Some
                        {
                          r with
                          R.conds = List.rev_append pre ((resid, yes) :: rest);
                        }
                  | Some _ -> step (cond :: pre) rest
                  | None -> raise Dead)
              | Opaque _ -> step (cond :: pre) rest)
          | (R.App (c1, xs), R.App (c2, ys)) :: rest
            when is_ctor c1 && is_ctor c2 ->
              if c1 = c2 && List.length xs = List.length ys then
                Some
                  {
                    r with
                    R.conds = List.rev_append pre (List.combine xs ys @ rest);
                  }
              else raise Dead
          | (R.Var v, t) :: rest
            when Hashtbl.mem fresh_set v
                 && (not (List.mem v (term_vars t)))
                 && pattern_safe t ->
              Some
                (subst_rule
                   [ (v, t) ]
                   { r with R.conds = List.rev_append pre rest })
          | (t, R.Var v) :: rest
            when Hashtbl.mem fresh_set v
                 && (not (List.mem v (term_vars t)))
                 && pattern_safe t ->
              Some
                (subst_rule
                   [ (v, t) ]
                   { r with R.conds = List.rev_append pre rest })
          | (R.Var w, t) :: rest when existential w t ->
              Some
                (subst_rule
                   [ (w, t) ]
                   { r with R.conds = List.rev_append pre rest })
          | (t, R.Var w) :: rest when existential w t ->
              Some
                (subst_rule
                   [ (w, t) ]
                   { r with R.conds = List.rev_append pre rest })
          | cond :: rest -> step (cond :: pre) rest
        in
        match step [] r.R.conds with Some r' -> go (fuel - 1) r' | None -> r
    in
    match go 200 r0 with r -> Some r | exception Dead -> None
  in
  (* Expansion driver: fan the first qualifying guard out, then recurse into
     each clone (a residual can itself be a bare membership test on a now
     head-bound payload variable, and a rule can carry several guards). *)
  let clauses_expanded = ref 0
  and clones_kept = ref 0
  and clones_dead = ref 0
  and vacuous_dropped = ref 0 in
  let expand_rule (r0 : R.rule) : R.rule list =
    let lhs_ok = match r0.R.lhs with R.App _ -> true | R.Var _ -> false in
    if not lhs_ok then [ r0 ]
    else
      let counter = ref 0 in
      let fresh_set = Hashtbl.create 8 in
      let fresh () =
        let v = Printf.sprintf "expand_%d" !counter in
        incr counter;
        Hashtbl.replace fresh_set v ();
        v
      in
      let budget = ref 64 in
      let head_sym = match r0.R.lhs with R.App (f, _) -> f | R.Var v -> v in
      let skip reason =
        Printf.eprintf "reflect: no subty expansion for %s (%s)\n" head_sym
          reason
      in
      let pick_guard (r : R.rule) =
        let lhs_vars = term_vars r.R.lhs in
        let rec find pre = function
          | [] -> None
          | (R.App (s, [ R.Var v ]), rr) :: rest
            when rr = yes && has_prefix "subty_" s && List.mem v lhs_vars ->
              Some (List.rev pre, s, v, rest)
          | c :: rest -> find (c :: pre) rest
        in
        find [] r.R.conds
      in
      let rec go (r : R.rule) : R.rule list =
        match pick_guard r with
        | None -> [ r ]
        | Some (pre, s, v, rest) -> (
            match family [] s with
            | Always_true ->
                incr vacuous_dropped;
                go { r with R.conds = pre @ rest }
            | Opaque reason ->
                skip (Printf.sprintf "%s: %s" s reason);
                [ r ]
            | Members ms ->
                let n = List.length ms in
                if n > 16 then (
                  skip (Printf.sprintf "%s: %d members" s n);
                  [ r ])
                else if !budget < n then (
                  skip (Printf.sprintf "%s: clone budget exhausted" s);
                  [ r ])
                else (
                  budget := !budget - n;
                  incr clauses_expanded;
                  ms
                  |> List.concat_map (fun (c, ps, residual) ->
                         let sf = List.map (fun _ -> fresh ()) ps in
                         let theta = [ (v, T.app_t c (List.map T.var_t sf)) ] in
                         let resid =
                           subst
                             (List.combine ps (List.map T.var_t sf))
                             residual
                         in
                         let conds =
                           List.map
                             (fun (l, rr) -> (subst theta l, subst theta rr))
                             (pre @ rest)
                         in
                         let conds =
                           if resid = yes then conds
                           else conds @ [ (resid, yes) ]
                         in
                         let r' =
                           {
                             R.lhs = subst theta r.R.lhs;
                             rhs = subst theta r.R.rhs;
                             conds;
                             owise = r.R.owise;
                           }
                         in
                         match simplify fresh_set r' with
                         | None ->
                             incr clones_dead;
                             []
                         | Some r'' ->
                             incr clones_kept;
                             go r'')))
      in
      go r0
  in
  let rules = List.concat_map expand_rule sys.R.rules in
  if !clauses_expanded > 0 || !vacuous_dropped > 0 then
    Printf.eprintf
      "reflect: subty expansion: %d clause(s) -> %d clone(s) (%d dead, %d \
       vacuous guard(s) dropped)\n"
      !clauses_expanded !clones_kept !clones_dead !vacuous_dropped;
  R.of_rules rules

(* -------------------------------------------------------------------------- *)
(* Support rules a guard may need: matcher families, struct accessors, and the
   payload projections. Generated once each, only when the system does not
   already define the symbol. *)

type support = {
  defined : (string, unit) Hashtbl.t; (* rule heads of the input system *)
  emitted : (string, unit) Hashtbl.t; (* support heads generated here *)
  mutable keys : string list; (* emitted heads, most recent first *)
  mutable rules : R.rule list; (* in generation order (deterministic) *)
}

let have (sup : support) (sym : string) : bool =
  Hashtbl.mem sup.defined sym || Hashtbl.mem sup.emitted sym

let emit (sup : support) (sym : string) (rules : R.rule list) : unit =
  if not (Hashtbl.mem sup.emitted sym) then (
    Hashtbl.add sup.emitted sym ();
    sup.keys <- sym :: sup.keys;
    sup.rules <- sup.rules @ rules)

(* Support emission is transactional per owise rule: a gated (abandoned)
   reflection must not leave the matchers/projections its attempt pulled in
   as orphan rules. *)
type mark = int * string list

let snapshot (sup : support) : mark = (List.length sup.rules, sup.keys)

let rollback (sup : support) ((n, keys) : mark) : unit =
  List.iter
    (fun k -> if not (List.mem k keys) then Hashtbl.remove sup.emitted k)
    sup.keys;
  sup.keys <- keys;
  sup.rules <- List.filteri (fun i _ -> i < n) sup.rules

let fresh_vars (n : int) : R.term list =
  List.init n (fun i -> T.var_t (Printf.sprintf "x%d" i))

(* [proj_<ctor>_<i>(ctor(x0..xn-1)) = xi] -- the payload projection that lets
   an owise guard reach a sibling pattern's subterm from the owise rule's own
   bare variable. Safe though partial: it only ever appears to the right of
   the matcher that guards it (short-circuit [and]). [key] names the symbol
   when the constructor spelling alone is ambiguous (the variadic [tuple]
   needs its arity folded in; a variant symbol already carries its arity). *)
let ensure_proj ?key (sup : support) (ctor : string) (arity : int) (i : int) :
    string =
  let sym = Printf.sprintf "proj_%s_%d" (Option.value key ~default:ctor) i in
  (if not (have sup sym) then
     let xs = fresh_vars arity in
     emit sup sym [ T.rule (T.app_t sym [ T.app_t ctor xs ]) (List.nth xs i) ]);
  sym

(* The total matcher family of variant type [ty] (mirrors [defs_of_typ]):
   regenerated when [prune_unused] dropped it. *)
let ensure_matchers ~scalars (tbl : tables) (sup : support) (ty : string) : unit
    =
  match Hashtbl.find_opt tbl.typdefs ty with
  | Some (VariantT typcases) ->
      let cases =
        List.map
          (fun (tc : typcase) ->
            let { synid = oid; _ } = tc.origin.it in
            let mixop = Mixfix.to_mixop tc.notation.it in
            (mixop, T.variant_sym oid.it mixop, Mixfix.arity mixop))
          typcases
      in
      List.iteri
        (fun i (mixop_i, _, _) ->
          let msym = T.match_sym ty mixop_i in
          if not (have sup msym) then
            emit sup msym
              (List.mapi
                 (fun j (_, ctor_j, arity_j) ->
                   T.rule
                     (T.app_t msym [ T.app_t ctor_j (fresh_vars arity_j) ])
                     (T.bool_t ~scalars (i = j)))
                 cases))
        cases
  | _ -> ()

(* The prelude's list/option matchers, regenerated if pruned. *)
let ensure_prelude_matcher ~scalars (sup : support) (sym : string) : unit =
  if not (have sup sym) then
    let x = T.var_t "x" and xs = T.var_t "xs" in
    let yes = T.bool_t ~scalars true and no = T.bool_t ~scalars false in
    let rules =
      match sym with
      | "match_cons" ->
          [
            T.rule (T.app_t sym [ T.cons_t x xs ]) yes;
            T.rule (T.app_t sym [ T.nil_t ]) no;
          ]
      | "match_nil" ->
          [
            T.rule (T.app_t sym [ T.nil_t ]) yes;
            T.rule (T.app_t sym [ T.cons_t x xs ]) no;
          ]
      | "match_some" ->
          [
            T.rule (T.app_t sym [ T.some_t x ]) yes;
            T.rule (T.app_t sym [ T.none_t ]) no;
          ]
      | "match_none" ->
          [
            T.rule (T.app_t sym [ T.none_t ]) yes;
            T.rule (T.app_t sym [ T.some_t x ]) no;
          ]
      | _ -> []
    in
    emit sup sym rules

(* Struct accessors (mirrors [defs_of_typ]'s accessor rules), regenerated if
   pruned. *)
let ensure_accessors (tbl : tables) (sup : support) (ty : string) : unit =
  match Hashtbl.find_opt tbl.typdefs ty with
  | Some (StructT fields) ->
      let n = List.length fields in
      List.iteri
        (fun i (a, _) ->
          let sym = T.field_sym ty a in
          if not (have sup sym) then
            emit sup sym
              [
                T.rule
                  (T.app_t sym [ T.app_t (T.struct_sym ty) (fresh_vars n) ])
                  (T.var_t (Printf.sprintf "x%d" i));
              ])
        fields
  | _ -> ()

(* -------------------------------------------------------------------------- *)
(* Guard construction. *)

type acc = {
  mutable tests : R.term list; (* reversed conjuncts *)
  mutable sub : (string * (R.term * typ' option)) list;
      (* sibling var -> subject term + its type when the spec gives one *)
}

let push (acc : acc) (t : R.term) : unit = acc.tests <- t :: acc.tests
let sub_terms (acc : acc) = List.map (fun (v, (t, _)) -> (v, t)) acc.sub

(* The guard equality. NOT the structural [eq]: [eq]'s per-constructor-pair
   family shares one head, so a single [eq] in a guard would drag the WHOLE
   family (47k rules on p4) into every slice ({!Rewrite_system.slice} is
   head-based) and blow the CRC up -- the [div_int] lesson. One reflexive
   rule is all the checker needs: under a critical-pair unifier the sibling's
   own condition (hypothesis) rewrites the guard's operands to literally the
   same term -- a ground literal to itself, a non-linear re-occurrence to its
   binding, [t = t'] to [eqg(t', t')] -- so [eqg(x, x) = true] collapses the
   guard. Off the diagonal [eqg] is stuck, which only leaves a pair
   undischarged (a conservative MAYBE, never a false YES). *)
let eqg_sym = "eqg"
let eqg_t (a : R.term) (b : R.term) : R.term = T.app_t eqg_sym [ a; b ]

let ensure_eqg ~scalars (sup : support) : unit =
  if not (have sup eqg_sym) then
    emit sup eqg_sym
      [ T.rule (eqg_t (T.var_t "x") (T.var_t "x")) (T.bool_t ~scalars true) ]

let push_eq ~scalars (sup : support) (acc : acc) (a : R.term) (b : R.term) :
    unit =
  ensure_eqg ~scalars sup;
  push acc (eqg_t a b)

(* The variant type to type a pattern's matcher against: the expected type
   when the spec gives one, else the single type containing the constructor. *)
let matcher_type (tbl : tables) (et : typ' option) (ctor : string) : string =
  let of_et =
    Option.bind et (fun t ->
        match resolve tbl t with
        | VarT { synid; _ } when Option.is_some (variant_case tbl synid.it ctor)
          ->
            Some synid.it
        | _ -> None)
  in
  match of_et with
  | Some ty -> ty
  | None -> (
      match Hashtbl.find_opt tbl.ctor_types ctor with
      | Some [ ty ] -> ty
      | Some tys ->
          raise
            (Gate
               (Printf.sprintf "ambiguous matcher for %s (in %s)" ctor
                  (String.concat "," tys)))
      | None -> raise (Gate (Printf.sprintf "unknown constructor %s" ctor)))

(* Reflect sibling pattern [pat] against [subject] (a term over the owise
   rule's variables): push the boolean tests and extend the substitution. *)
let rec ptest ~scalars (tbl : tables) (sup : support) (acc : acc)
    (subject : R.term) (et : typ' option) (pat : R.term) : unit =
  let elem_et iter_et =
    match Option.map (resolve tbl) iter_et with
    | Some (IterT { typ; _ }) -> Some typ.it
    | _ -> None
  in
  match pat with
  | R.Var v -> (
      match List.assoc_opt v acc.sub with
      | Some (t0, _) -> push_eq ~scalars sup acc t0 subject (* non-linear *)
      | None -> acc.sub <- (v, (subject, et)) :: acc.sub)
  (* A fully-ground non-variant pattern (a text literal, a scalar, a ground
     list/option) is one structural [eq] test -- the prelude's [eq] family
     covers nil/cons/none/some and the scalars. Variant grounds keep the
     matcher path (their [eq] family may have been pruned). *)
  | R.App (c, _)
    when ground pat
         && (not (Hashtbl.mem tbl.ctor_types c))
         && (not (has_prefix "struct_" c))
         && c <> "tuple" ->
      push_eq ~scalars sup acc subject pat
  | R.App ("tuple", ps) ->
      let n = List.length ps in
      let comp_ets =
        match Option.map (resolve tbl) et with
        | Some (TupleT ts) when List.length ts = n ->
            List.map (fun t -> Some t.it) ts
        | _ -> List.init n (fun _ -> None)
      in
      List.iteri
        (fun i p ->
          let sym =
            ensure_proj ~key:(Printf.sprintf "tuple%d" n) sup "tuple" n i
          in
          ptest ~scalars tbl sup acc (T.app_t sym [ subject ])
            (List.nth comp_ets i) p)
        ps
  | R.App ("cons", [ ph; pt ]) ->
      ensure_prelude_matcher ~scalars sup "match_cons";
      push acc (T.app_t "match_cons" [ subject ]);
      let h = ensure_proj sup "cons" 2 0 and t = ensure_proj sup "cons" 2 1 in
      ptest ~scalars tbl sup acc (T.app_t h [ subject ]) (elem_et et) ph;
      ptest ~scalars tbl sup acc (T.app_t t [ subject ]) et pt
  | R.App ("nil", []) ->
      ensure_prelude_matcher ~scalars sup "match_nil";
      push acc (T.app_t "match_nil" [ subject ])
  | R.App ("none", []) ->
      ensure_prelude_matcher ~scalars sup "match_none";
      push acc (T.app_t "match_none" [ subject ])
  | R.App ("some", [ p1 ]) ->
      ensure_prelude_matcher ~scalars sup "match_some";
      push acc (T.app_t "match_some" [ subject ]);
      let s0 = ensure_proj sup "some" 1 0 in
      ptest ~scalars tbl sup acc (T.app_t s0 [ subject ]) (elem_et et) p1
  | R.App (c, ps) when Hashtbl.mem tbl.ctor_types c ->
      let ty = matcher_type tbl et c in
      let mixop, field_typs, n_cases = Option.get (variant_case tbl ty c) in
      (* A single-case variant needs no discrimination, only projections. *)
      if n_cases > 1 then (
        ensure_matchers ~scalars tbl sup ty;
        push acc (T.app_t (T.match_sym ty mixop) [ subject ]));
      List.iteri
        (fun i p ->
          let sym = ensure_proj sup c (List.length ps) i in
          let fet =
            match List.nth_opt field_typs i with
            | Some t -> Some t.it
            | None -> None
          in
          ptest ~scalars tbl sup acc (T.app_t sym [ subject ]) fet p)
        ps
  | R.App (c, ps) -> (
      (* struct literal? *)
      let struct_ty =
        match Option.map (resolve tbl) et with
        | Some (VarT { synid; _ }) when T.struct_sym synid.it = c ->
            Some synid.it
        | _ -> None
      in
      match struct_ty with
      | Some ty -> (
          match Hashtbl.find_opt tbl.typdefs ty with
          | Some (StructT fields) when List.length fields = List.length ps ->
              ensure_accessors tbl sup ty;
              List.iteri
                (fun i p ->
                  let a, ft = List.nth fields i in
                  ptest ~scalars tbl sup acc
                    (T.app_t (T.field_sym ty a) [ subject ])
                    (Some ft.it) p)
                ps
          | _ -> raise (Gate (Printf.sprintf "struct shape %s" c)))
      | None ->
          if ground pat then push_eq ~scalars sup acc subject pat
          else raise (Gate (Printf.sprintf "unrecognized pattern head %s" c)))

(* Reflect one sibling condition [(l, r)] under the running substitution.
   [succ] is threaded to {!check_reflectable} so an iteration helper already
   success-reflected in this attempt (or, after the attempt, in the final
   set) is not gated. *)
let ctest ~scalars (tbl : tables) (sup : support) (effectful : string list)
    (succ : string list) (acc : acc) ((l, r) : R.cond) : unit =
  let unbound t =
    List.filter (fun v -> not (List.mem_assoc v acc.sub)) (term_vars t)
  in
  (match unbound l with
  | [] -> ()
  | v :: _ -> raise (Gate (Printf.sprintf "unbound sibling variable %s" v)));
  (* [l]'s result type, when recoverable: a variable's recorded type, a
     function call's declared result, a struct field accessor's field type,
     or a single-output relation's output. *)
  let type_of_l =
    match l with
    | R.Var v -> Option.join (Option.map snd (List.assoc_opt v acc.sub))
    | R.App (f, _) -> (
        match Hashtbl.find_opt tbl.funcsigs f with
        | Some (_, ret) -> Some ret.it
        | None -> (
            match Hashtbl.find_opt tbl.fieldsigs f with
            | Some t -> Some t
            | None -> (
                match Hashtbl.find_opt tbl.rel_outs f with
                | Some [ t ] -> Some t.it
                | _ -> None)))
  in
  let sl = subst (sub_terms acc) l in
  check_reflectable tbl effectful succ sl;
  if r = T.bool_t ~scalars true then push acc sl
  else if r = T.bool_t ~scalars false then push acc (T.not_t sl)
  else
    match r with
    | R.Var v -> (
        match List.assoc_opt v acc.sub with
        | Some (t0, _) -> push_eq ~scalars sup acc sl t0
        | None ->
            acc.sub <- (v, (sl, type_of_l)) :: acc.sub (* binding condition *))
    | _ when ground r ->
        check_reflectable tbl effectful succ r;
        push_eq ~scalars sup acc sl r
    | R.App (c, _)
      when not
             (Hashtbl.mem tbl.ctor_types c
             || List.mem c [ "tuple"; "cons"; "nil"; "none"; "some" ]
             || has_prefix "struct_" c) -> (
        (* computed right-hand side (e.g. ["name" = $name(n)]): a symmetric
           join, reflectable as [eq] once its variables are bound *)
        match unbound r with
        | [] ->
            let sr = subst (sub_terms acc) r in
            check_reflectable tbl effectful succ sr;
            push_eq ~scalars sup acc sl sr
        | v :: _ ->
            raise
              (Gate (Printf.sprintf "computed condition binds variable %s" v)))
    | R.App _ ->
        (* destructuring bind: type the pattern by [l]'s result type *)
        ptest ~scalars tbl sup acc sl type_of_l r

let and_chain ~scalars (ts : R.term list) : R.term =
  match ts with
  | [] -> T.bool_t ~scalars true
  | t :: ts -> List.fold_left (fun a b -> T.and_t a b) t ts

(* Reflect the sibling's own conditions under a pre-seeded substitution
   [acc], returning the conjunction of [acc]'s accumulated tests and the
   conditions'. Shared by {!sibling_guard} (which seeds [acc] by ptest-ing
   every head position) and the complement-enumeration path (whose clause
   heads already select the constructors, so it seeds enum positions by
   direct substitution instead -- keeping ground matcher constants out of
   the guard). *)
let sibling_conds_guard ?(prep = Fun.id) ~scalars (tbl : tables) (sup : support)
    (effectful : string list) (succ : string list) (acc : acc) (s : R.rule) :
    R.term =
  (* variables the rule's OWN conditions bind to an exact constructor
     application, by either side of the condition *)
  let exactly_bound =
    List.filter_map
      (fun ((l, r) : R.cond) ->
        match (l, r) with
        | R.Var v, R.App (c, _) when Hashtbl.mem tbl.ctor_types c -> Some v
        | R.App (c, _), R.Var v when Hashtbl.mem tbl.ctor_types c -> Some v
        | _ -> None)
      s.R.conds
  in
  let redundant_membership_test ((l, r) : R.cond) : bool =
    r = T.bool_t ~scalars true
    &&
    match l with
    | R.App (f, [ R.Var v ]) ->
        (has_prefix "subty_" f || has_prefix "match_" f)
        && List.mem v exactly_bound
    | _ -> false
  in
  (* [s.R.conds] is the CTRS's raw premise order, not guaranteed to bind a
     variable before a later condition uses it ({!To_maude.print_conds} hits
     the identical issue and re-sorts by readiness for exactly this reason --
     "a cast-stripped binding can sit after a test that uses it"). A rule
     whose iterated sub-pattern is destructured into a fresh list (e.g. a
     [{name = value}*] field list projected into two streams) emits its
     [isStuckHead($iterproj_.. (fresh)) = false] guard ahead of the very
     destructure that binds [fresh], so a naive left-to-right [ctest] pass
     gates the whole judgment as unreflectable (seen on [Type_alpha]'s
     [serializable] case, permanently stranding every negated [Type_alpha]/
     [Cast_impl] premise -- i.e. every implicit numeric-to-bit cast). Repeat
     picking whichever remaining condition is already satisfiable (its [l]'s
     free variables are bound) until none are; whatever is left is fed through
     in original order so a genuinely unreflectable rule still raises its
     ordinary [Gate]. ({!Crc_surface.order_conds} now normalizes the
     pipeline's conds to binding order upstream, but this scheduler also
     credits [redundant_membership_test] conds as ready, so it stays as
     defense in depth.) *)
  let is_ready ((l, r) : R.cond) =
    redundant_membership_test (l, r)
    || List.for_all (fun v -> List.mem_assoc v acc.sub) (term_vars l)
  in
  let rec schedule (remaining : R.cond list) : unit =
    let rec pick seen = function
      | [] -> None
      | c :: rest when is_ready c -> Some (c, List.rev_append seen rest)
      | c :: rest -> pick (c :: seen) rest
    in
    match pick [] remaining with
    | Some (c, rest) ->
        if not (redundant_membership_test c) then
          ctest ~scalars tbl sup effectful succ acc (prep c);
        schedule rest
    | None ->
        List.iter
          (fun c ->
            if not (redundant_membership_test c) then
              ctest ~scalars tbl sup effectful succ acc (prep c))
          remaining
  in
  schedule s.R.conds;
  (* an empty guard means the sibling always applies: the owise is dead;
     drop duplicate conjuncts (a destructure re-tests the matcher the
     sibling's own guard condition already spelled) *)
  let dedup ts =
    List.fold_left
      (fun acc t -> if List.mem t acc then acc else acc @ [ t ])
      [] ts
  in
  and_chain ~scalars (dedup (List.rev acc.tests))

(* "sibling [s] applies", as one total boolean term over the owise rule's own
   variables. [ow_args] are the owise rule's head arguments (the subjects);
   [argtyps] the declared argument types when the spec gives them. [prep]
   pre-rewrites each condition (the judgment reflection substitutes
   [holds_<R>] heads before the reflectability check sees the raw relation). *)
let sibling_guard ?(prep = Fun.id) ~scalars (tbl : tables) (sup : support)
    (effectful : string list) (succ : string list) (ow_args : R.term list)
    (argtyps : typ' option list) (s : R.rule) : R.term =
  let s_args =
    match s.R.lhs with
    | R.App (_, args) -> args
    | R.Var _ -> raise (Gate "variable lhs")
  in
  if List.length s_args <> List.length ow_args then
    raise (Gate "sibling/owise arity mismatch");
  let acc = { tests = []; sub = [] } in
  List.iteri
    (fun j p ->
      ptest ~scalars tbl sup acc (List.nth ow_args j) (List.nth argtyps j) p)
    s_args;
  sibling_conds_guard ~prep ~scalars tbl sup effectful succ acc s

(* -------------------------------------------------------------------------- *)
(* Complement enumeration (enum-dispatch owise).

   Classify the owise rule's argument positions: an ENUM position (some
   sibling puts a nullary constructor there; requires the declared type to
   enumerate to nullary constructors only) or a PASS-THROUGH position (every
   sibling has a bare variable). Enumerate the product over enum positions;
   per tuple:
   - covered by an unconditional sibling: the owise is dead there, emit
     nothing;
   - covered only by conditional siblings: emit a ground-head clause carrying
     each covering sibling's reflected guard negated ([g = false] per
     sibling -- a conjunction of negations, no or-gate);
   - covered by nothing: emit the pure ground fall-through.
   Every clause head is disjoint from every other tuple's, so critical pairs
   only arise within a tuple, where the guards are guard-form (discharged by
   the sibling's own hypotheses under the critical-pair unifier) -- never
   the ground pattern-form disjuncts the CRC's nested-or feasibility check
   cannot see, and does not normalize away, when a true disjunct sits buried
   in a left-nested [or] (the [$join_ctk]/[$assignop_as_binop] MAYBEs; see
   todo.md M1 2026-07-15). Analysis-only, like the rest of this pass.
   [max_complement] caps the emitted clause count (the product of the enum
   positions' constructor counts minus the fully-covered tuples). *)

let max_complement = 16

(* The nullary-constructor enumeration of a declared argument type, when it
   resolves to a variant with ONLY nullary constructors (the same typdefs
   walk as {!ensure_matchers}). *)
let nullary_enum (tbl : tables) (et : typ' option) : string list option =
  match Option.map (resolve tbl) et with
  | Some (VarT { synid; _ }) -> (
      match Hashtbl.find_opt tbl.typdefs synid.it with
      | Some (VariantT typcases) ->
          let cs =
            List.map
              (fun (tc : typcase) ->
                let { synid = oid; _ } = tc.origin.it in
                let mixop = Mixfix.to_mixop tc.notation.it in
                (T.variant_sym oid.it mixop, Mixfix.arity mixop))
              typcases
          in
          if List.for_all (fun (_, a) -> a = 0) cs then Some (List.map fst cs)
          else None
      | _ -> None)
  | _ -> None

let rec tuple_product : 'a list list -> 'a list list = function
  | [] -> [ [] ]
  | cs :: rest ->
      let tails = tuple_product rest in
      List.concat_map (fun c -> List.map (fun t -> c :: t) tails) cs

type cpos = Enum of string list | Pass

(* [Some clauses] when the owise rule [r] of [f] qualifies for complement
   enumeration, [None] to fall back to the or-gate path. May raise [Gate]
   (an unreflectable sibling condition) -- the caller rolls [sup] back and
   falls back the same way. *)
let complement_clauses ~scalars (tbl : tables) (sup : support)
    (effectful : string list) (succ : string list) (f : string) (r : R.rule)
    (ow_args : R.term list) (argtyps : typ' option list)
    (siblings : R.rule list) : R.rule list option =
  let n = List.length ow_args in
  let ow_vars =
    List.filter_map (function R.Var v -> Some v | _ -> None) ow_args
  in
  let sib_args =
    List.filter_map
      (fun (s : R.rule) ->
        match s.R.lhs with
        | R.App (_, a) when List.length a = n -> Some (s, a)
        | _ -> None)
      siblings
  in
  if
    siblings = [] (* a no-sibling owise: complement would flip its meaning *)
    || r.R.conds <> []
    || List.length ow_vars <> n
    || List.sort_uniq compare ow_vars <> List.sort compare ow_vars
    || List.length sib_args <> List.length siblings
  then None
  else
    let classify j =
      let args_j = List.map (fun (_, a) -> List.nth a j) sib_args in
      if List.for_all (function R.Var _ -> true | _ -> false) args_j then
        Some Pass
      else
        match nullary_enum tbl (List.nth argtyps j) with
        | Some cs
          when List.for_all
                 (function
                   | R.Var _ -> true
                   | R.App (c, []) -> List.mem c cs
                   | _ -> false)
                 args_j ->
            Some (Enum cs)
        | _ -> None
    in
    match
      List.fold_right
        (fun j acc ->
          match (classify j, acc) with
          | Some p, Some ps -> Some (p :: ps)
          | _ -> None)
        (List.init n Fun.id) (Some [])
    with
    | None -> None
    | Some poss
      when not (List.exists (function Enum _ -> true | Pass -> false) poss) ->
        None (* purely conditional owise: guard-form or-gates discharge *)
    | Some poss ->
        (* candidate heads: per-position slots are the enum constructors, or
           a single [None] slot at a pass-through position *)
        let slots =
          List.map
            (function
              | Enum cs -> List.map (fun c -> Some c) cs | Pass -> [ None ])
            poss
        in
        let matches tuple (_, a) =
          List.for_all2
            (fun slot p ->
              match (slot, p) with
              | None, _ | Some _, R.Var _ -> true
              | Some c, R.App (c', []) -> c = c'
              | Some _, _ -> false)
            tuple a
        in
        let cases =
          List.filter_map
            (fun tuple ->
              let ms = List.filter (matches tuple) sib_args in
              if List.exists (fun ((s : R.rule), _) -> s.R.conds = []) ms then
                None (* an unconditional sibling covers it: owise dead *)
              else Some (tuple, ms))
            (tuple_product slots)
        in
        if List.length cases > max_complement then None
        else
          Some
            (List.map
               (fun (tuple, ms) ->
                 let head_args =
                   List.map2
                     (fun slot v ->
                       match slot with
                       | Some c -> T.app_t c []
                       | None -> T.var_t v)
                     tuple ow_vars
                 in
                 let sub =
                   List.concat
                     (List.map2
                        (fun slot v ->
                          match slot with
                          | Some c -> [ (v, T.app_t c []) ]
                          | None -> [])
                        tuple ow_vars)
                 in
                 (* one negated guard PER covering conditional sibling: a
                    conjunction of negations, no or-gate. The clause head
                    already selects the constructors, so enum positions seed
                    the substitution directly instead of ptest-ing -- keeping
                    ground matcher constants (the very shape the CRC cannot
                    normalize) out of the guard. *)
                 let conds =
                   List.map
                     (fun ((s : R.rule), a) ->
                       let acc = { tests = []; sub = [] } in
                       List.iteri
                         (fun j p ->
                           match (List.nth tuple j, p) with
                           | Some _, R.App (_, []) ->
                               () (* the head already selects this ctor *)
                           | Some c, R.Var v ->
                               acc.sub <-
                                 (v, (T.app_t c [], List.nth argtyps j))
                                 :: acc.sub
                           | None, p ->
                               ptest ~scalars tbl sup acc (List.nth ow_args j)
                                 (List.nth argtyps j) p
                           | Some _, _ -> assert false)
                         a;
                       ( sibling_conds_guard ~scalars tbl sup effectful succ acc
                           s,
                         T.bool_t ~scalars false ))
                     ms
                 in
                 {
                   R.lhs = T.app_t f head_args;
                   rhs = subst sub r.R.rhs;
                   conds;
                   owise = false;
                 })
               cases)

(* -------------------------------------------------------------------------- *)
(* Judgment reflection: [holds_<R>] for a judgment [R] -- the existential
   "some rule of R applies", built from each rule's lhs patterns and
   conditions with the rhs ignored, so it covers output-carrying judgments
   the same way as no-output ones (whose rules all conclude [= true];
   failure is stuckness either way) -- and [holds_<$iterall..>] for a
   no-binding iterated premise's helper. [holds_<R>] is ONE unconditional
   or-rule -- "R holds iff some rule applies" is an unordered disjunction --
   so it adds no critical pairs; [holds_<$iterall..>] is the totalized
   and-fold (unconditional step + explicit length-mismatch [false] rules).
   Negated judgment premises ([R(in) = false], today unsatisfiable because a
   judgment never rewrites to [false]) become satisfiable
   [holds_R(in) = false] when the [false] cannot be an output value
   ({!rel_output_kind}); a no-output judgment's positive uses are switched to
   [holds_R(in) = true] as well, because the CRC treats [R] and [holds_R] as
   unrelated symbols -- only the uniform spelling lets a critical pair's
   hypotheses collapse an owise guard mentioning [holds_R]. Nothing is lost
   by the switch: a no-output judgment's own rules all share the rhs [true],
   so their mutual overlaps were trivially joinable anyway. An
   output-carrying judgment's BINDING call sites keep their [:=]-style
   condition and instead gain an inserted [holds_R(in) = true] test (the
   same mechanism as the collecting iteration helpers below). *)

(* Which spelling of a [R(in..) = true/false] condition is a satisfiability
   test (negation-as-failure, respellable to [holds_R]) rather than an output
   VALUE test: a no-output judgment only ever concludes [true]; a judgment
   whose output cannot be a bool literal (non-bool single output, or several
   outputs -- the rhs is then a tuple) can only meet [= false] as a negation;
   a single bool output is syntactically ambiguous, so it is never
   respelled. *)
let rel_output_kind (tbl : tables) (f : string) :
    [ `No_output | `Non_bool | `Maybe_bool ] =
  match Hashtbl.find_opt tbl.rel_outs f with
  | None | Some [] -> `No_output
  | Some [ t ] -> (
      match resolve tbl t.it with
      | BoolT -> `Maybe_bool
      | VarT { synid; _ } ->
          if Hashtbl.mem tbl.typdefs synid.it then `Non_bool else `Maybe_bool
      | _ -> `Non_bool)
  | Some _ -> `Non_bool

(* Rewrite a [R(in) = true/false] or [$iterall..(args) = true] condition to
   its [holds_] spelling, for the assumed-reflectable set [succ]. Conditions
   binding an output variable are left alone, and a bool-literal rhs is only
   respelled when {!rel_output_kind} says it cannot be an output value. *)
let replace_cond ~scalars (tbl : tables) (succ : string list) ((l, r) : R.cond)
    : R.cond =
  match l with
  | R.App (f, args) when List.mem f succ ->
      let bool_rhs b = r = T.bool_t ~scalars b in
      let respell =
        if Hashtbl.mem tbl.relsigs f then
          match rel_output_kind tbl f with
          | `No_output -> bool_rhs true || bool_rhs false
          | `Non_bool -> bool_rhs false
          | `Maybe_bool -> false
        else bool_rhs true || bool_rhs false
      in
      if respell then (R.App (T.holds_sym f, args), r) else (l, r)
  | _ -> (l, r)

(* [holds_<R>(x0..xn-1) = or(g_1 .. g_k)]: one g per rule of [R], built by the
   same machinery as the owise sibling guards (the rules ARE the siblings,
   the fresh argument variables the subjects). *)
let gen_rel_holds ~scalars ~prep (tbl : tables) (sup : support)
    (effectful : string list) (succ : string list) (name : string)
    (argtyps : typ' option list) (rules : R.rule list) : R.rule list =
  let arity =
    match (List.hd rules).R.lhs with
    | R.App (_, args) -> List.length args
    | R.Var _ -> raise (Gate "variable lhs")
  in
  let xs = List.init arity (fun i -> T.var_t (Printf.sprintf "x%d" i)) in
  let argtyps =
    if List.length argtyps = arity then argtyps
    else List.init arity (fun _ -> None)
  in
  let gs =
    List.map
      (fun (ru : R.rule) ->
        (* the guard reflects the rule's lhs patterns and conditions only --
           an output-carrying rule's rhs is simply ignored, which is exactly
           the existential "some rule applies" reading *)
        sibling_guard ~prep ~scalars tbl sup effectful succ xs argtyps ru)
      rules
  in
  let disj =
    match gs with
    | [] -> raise (Gate "judgment without rules")
    | g :: gs -> List.fold_left (fun a b -> T.or_t a b) g gs
  in
  [ T.rule (T.app_t (T.holds_sym name) xs) disj ]

(* The explicit [false] rules a totalized [holds_$iterall../$itercollect..]
   needs for a multi-spine List iteration whose bound streams
   can desync (one hits [nil] before another): every non-trivial subset of
   spine positions (the positions the base rule pins to [nil]) pinned to
   [nil] against the rest still open, so the reflection is total over every
   reachable spine-length combination, not just the lockstep one. Shared by
   both iteration-helper reflections below (an Opt iteration never
   recurses, so it never calls this). *)
let iter_spine_mismatches ~scalars (hname : string) (base_args : R.term list) :
    R.rule list =
  let spine =
    List.mapi (fun i a -> (i, a = T.nil_t)) base_args
    |> List.filter snd |> List.map fst
  in
  let bit_of = List.mapi (fun bit i -> (i, bit)) spine in
  let k = List.length spine in
  if k = 0 then raise (Gate "iteration helper without a nil spine");
  if k > 3 then raise (Gate "iteration helper spine too wide");
  List.filter_map
    (fun mask ->
      if mask = 0 || mask = (1 lsl k) - 1 then None
      else
        Some
          (T.rule
             (T.app_t hname
                (List.mapi
                   (fun i a ->
                     match List.assoc_opt i bit_of with
                     | None -> a (* captured fv: the base rule's var *)
                     | Some bit ->
                         if mask land (1 lsl bit) <> 0 then
                           T.cons_t
                             (T.var_t (Printf.sprintf "mh%d" bit))
                             (T.var_t (Printf.sprintf "mt%d" bit))
                         else T.nil_t)
                   base_args))
             (T.bool_t ~scalars false)))
    (List.init ((1 lsl k) - 1) (fun m -> m + 1))

(* The totalized [$iterall] helper: the conditional cons-step (stuck when a
   step fails) becomes an unconditional and-fold, and the spine-length
   mismatches (multi-spine lockstep iteration) become explicit [false]. *)
let gen_iterall_holds ~scalars ~prep (tbl : tables) (sup : support)
    (effectful : string list) (succ : string list) (name : string)
    (rules : R.rule list) : R.rule list =
  let yes = T.bool_t ~scalars true in
  let base, step =
    match rules with
    | [ a; b ] ->
        if a.R.conds = [] && a.R.rhs = yes then (a, b)
        else if b.R.conds = [] && b.R.rhs = yes then (b, a)
        else raise (Gate "unexpected iterall rule shape")
    | _ -> raise (Gate "unexpected iterall rule count")
  in
  let args_of r =
    match r.R.lhs with
    | R.App (_, args) -> args
    | R.Var _ -> raise (Gate "variable lhs")
  in
  let base_args = args_of base and step_args = args_of step in
  let hname = T.holds_sym name in
  let acc =
    {
      tests = [];
      sub = List.map (fun v -> (v, (R.Var v, None))) (term_vars step.R.lhs);
    }
  in
  List.iter
    (fun c -> ctest ~scalars tbl sup effectful succ acc (prep c))
    step.R.conds;
  let tests = List.rev acc.tests in
  let rec_args =
    match step.R.rhs with
    | R.App (s, args) when s = name -> Some args
    | rhs when rhs = yes -> None (* Opt iteration: no recursion *)
    | _ -> raise (Gate "unexpected iterall step rhs")
  in
  let step_rhs =
    and_chain ~scalars
      (tests
      @ match rec_args with Some ra -> [ T.app_t hname ra ] | None -> [])
  in
  let mismatches =
    match rec_args with
    | None -> []
    | Some _ -> iter_spine_mismatches ~scalars hname base_args
  in
  [
    T.rule (T.app_t hname base_args) yes;
    T.rule (T.app_t hname step_args) step_rhs;
  ]
  @ mismatches

(* The totalized [$itercollect] helper: same recipe as [$iterall] (base ->
   [true], step -> and-fold with the recursive call), except the step rhs is
   [cons(collected, sym(rec))] ([some(collected)] for Opt) rather than a bare
   recursive call -- the collected element itself carries no information for a
   SUCCESS reflection (whether the iteration completes, not what it collects).
   Two step shapes:

   - the premise is a single relation call, inlined as the unconditional
     step's element (the ex-[$iterapply] shape): there is no separate guard
     to reflect; the call either reduces or the collect result gets stuck,
     so the success reflection is just [holds_R] applied to that same call's
     arguments, ANDed with the recursive success;
   - anything else: the element is discarded and the reflected step
     conditions are and-folded, as for [$iterall]. A general collect can
     ALSO be unconditional here -- {!Crc_surface.fold_premise_binders}
     folds a let-destructure condition into the step's lhs pattern -- and
     then the fold is simply empty (success is just the recursive call). *)
let gen_itercollect_holds ~scalars ~prep (tbl : tables) (sup : support)
    (effectful : string list) (succ : string list) (name : string)
    (rules : R.rule list) : R.rule list =
  let is_base (r : R.rule) =
    r.R.conds = [] && (r.R.rhs = T.nil_t || r.R.rhs = T.none_t)
  in
  let base, step =
    match rules with
    | [ a; b ] ->
        if is_base a then (a, b)
        else if is_base b then (b, a)
        else raise (Gate "unexpected itercollect rule shape")
    | _ -> raise (Gate "unexpected itercollect rule count")
  in
  let args_of r =
    match r.R.lhs with
    | R.App (_, args) -> args
    | R.Var _ -> raise (Gate "variable lhs")
  in
  let base_args = args_of base and step_args = args_of step in
  let hname = T.holds_sym name in
  let elem, rec_args =
    match step.R.rhs with
    | R.App ("cons", [ e; R.App (s, args) ]) when s = name -> (e, Some args)
    | R.App ("some", [ e ]) -> (e, None) (* Opt iteration: no recursion *)
    | _ -> raise (Gate "unexpected itercollect step rhs")
  in
  let rec_holds =
    match rec_args with Some ra -> [ T.app_t hname ra ] | None -> []
  in
  let step_rhs =
    match elem with
    | R.App (f, args) when step.R.conds = [] && Hashtbl.mem tbl.relsigs f ->
        List.iter (check_reflectable tbl effectful succ) args;
        and_chain ~scalars (R.App (T.holds_sym f, args) :: rec_holds)
    | _ ->
        let acc =
          {
            tests = [];
            sub =
              List.map (fun v -> (v, (R.Var v, None))) (term_vars step.R.lhs);
          }
        in
        List.iter
          (fun c -> ctest ~scalars tbl sup effectful succ acc (prep c))
          step.R.conds;
        and_chain ~scalars (List.rev acc.tests @ rec_holds)
  in
  let mismatches =
    match rec_args with
    | None -> []
    | Some _ -> iter_spine_mismatches ~scalars hname base_args
  in
  [
    T.rule (T.app_t hname base_args) (T.bool_t ~scalars true);
    T.rule (T.app_t hname step_args) step_rhs;
  ]
  @ mismatches

(* -------------------------------------------------------------------------- *)
(* The pass. *)

let owise ~(scalars : T.scalar_theory) ~(orig : spec) ~(effectful : string list)
    (sys : R.t) : R.t =
  let tbl = build_tables orig in
  let sup =
    {
      defined =
        (let h = Hashtbl.create 512 in
         List.iter
           (fun r ->
             match R.defined_head r with
             | Some f -> Hashtbl.replace h f ()
             | None -> ())
           sys.R.rules;
         h);
      emitted = Hashtbl.create 64;
      keys = [];
      rules = [];
    }
  in
  (* Declared argument types of a defined symbol, when recoverable. *)
  let argtyps_of (f : string) (arity : int) : typ' option list =
    let typs =
      match Hashtbl.find_opt tbl.funcsigs f with
      | Some (params, _) -> Some params
      | None -> Hashtbl.find_opt tbl.relsigs f
    in
    match typs with
    | Some ts when List.length ts = arity -> List.map (fun t -> Some t.it) ts
    | _ -> List.init arity (fun _ -> None)
  in
  (* ---- judgment phase: generate [holds_*] and respell the conditions. ---- *)
  let by_head : (string, R.rule list) Hashtbl.t = Hashtbl.create 512 in
  List.iter
    (fun (r : R.rule) ->
      match R.defined_head r with
      | Some f ->
          Hashtbl.replace by_head f
            (Option.value (Hashtbl.find_opt by_head f) ~default:[] @ [ r ])
      | None -> ())
    sys.R.rules;
  let owise_heads =
    List.filter_map
      (fun (r : R.rule) -> if r.R.owise then R.defined_head r else None)
      sys.R.rules
  in
  (* Candidates: judgments negated anywhere, plus judgments/iteration helpers
     conditioning the clauses of an owise-carrying symbol, plus everything
     those pull in (their own rules' judgment/iteration-helper conditions).
     [$iterall]/[$itercollect] are success-carrying (a step can fail), so
     they need a success reflection to enter a guard; [$itermap]/[$iterproj]
     are value-binding pure stream transformers, so they never
     need one and are excluded here -- {!iter_helper_prefixes} allows them
     unconditionally already. *)
  let is_rel f = Hashtbl.mem tbl.relsigs f in
  let is_iterall f = has_prefix "$iterall" f in
  let is_itercollect f = has_prefix "$itercollect" f in
  let is_iter_helper f = is_iterall f || is_itercollect f in
  (* A dependency can sit anywhere in a condition term, not just as its LHS's
     own head: {!Crc_surface.fold_premise_binders} (the pass just before
     this one) inlines a premise's output binder at its use sites, so a
     collecting helper's call can end up nested inside another condition's
     arguments (e.g. a later [$iterall]'s stream argument) instead of
     standing as its own binding condition. [term_heads] walks the whole term
     so [close] still finds it -- matching what {!check_reflectable} itself
     inspects, which recurses into every argument regardless of nesting. *)
  let rec term_heads (t : R.term) : string list =
    match t with
    | R.Var _ -> []
    | R.App (f, args) ->
        let deps = List.concat_map term_heads args in
        if is_rel f || is_iter_helper f then f :: deps else deps
  in
  let cond_heads (rs : R.rule list) : string list =
    List.concat_map
      (fun (r : R.rule) ->
        List.concat_map (fun (l, cr) -> term_heads l @ term_heads cr) r.R.conds)
      rs
  in
  let seed =
    List.concat_map
      (fun (r : R.rule) ->
        List.filter_map
          (fun (l, rr) ->
            match l with
            | R.App (f, _)
              when is_rel f
                   && rr = T.bool_t ~scalars false
                   && rel_output_kind tbl f <> `Maybe_bool ->
                Some f
            | _ -> None)
          r.R.conds)
      sys.R.rules
  in
  let seed =
    seed
    @ List.concat_map
        (fun h ->
          cond_heads (Option.value (Hashtbl.find_opt by_head h) ~default:[]))
        owise_heads
  in
  let qualified f =
    (* any judgment with rules gets an existential [holds_]; output-carrying
       ones included (their rhs is ignored by {!gen_rel_holds}) *)
    match Hashtbl.find_opt by_head f with
    | Some rs -> rs <> []
    | None -> false
  in
  let rec close (acc : string list) (work : string list) : string list =
    match work with
    | [] -> acc
    | f :: rest ->
        if List.mem f acc || not (qualified f) then close acc rest
        else
          let deps =
            cond_heads (Hashtbl.find by_head f)
            |> List.filter (fun d -> not (List.mem d acc))
          in
          close (acc @ [ f ]) (rest @ deps)
  in
  let candidates = close [] seed in
  (* Generate under the optimistic assumption that every candidate reflects;
     a gated candidate restarts the attempt without it (the set is tiny). *)
  let rec attempt (cands : string list) : string list * R.rule list =
    let mark = snapshot sup in
    let prep = replace_cond ~scalars tbl cands in
    let failed = ref None in
    let gen =
      try
        List.concat_map
          (fun f ->
            try
              let rs = Hashtbl.find by_head f in
              if is_iterall f then
                gen_iterall_holds ~scalars ~prep tbl sup effectful cands f rs
              else if is_itercollect f then
                gen_itercollect_holds ~scalars ~prep tbl sup effectful cands f
                  rs
              else
                let arity =
                  match (List.hd rs).R.lhs with
                  | R.App (_, args) -> List.length args
                  | R.Var _ -> 0
                in
                gen_rel_holds ~scalars ~prep tbl sup effectful cands f
                  (argtyps_of f arity) rs
            with Gate reason ->
              failed := Some (f, reason);
              raise (Gate reason))
          cands
      with Gate _ -> []
    in
    match !failed with
    | None -> (cands, gen)
    | Some (f, reason) ->
        rollback sup mark;
        Printf.eprintf "reflect: no holds_ reflection for %s (%s)\n" f reason;
        attempt (List.filter (fun c -> c <> f) cands)
  in
  let succ, holds_rules = attempt candidates in
  if succ <> [] then
    Printf.eprintf "reflect: judgment reflection for %s\n"
      (String.concat ", " succ);
  (* [$itercollect] -- and output-carrying judgments -- never
     carry a bool rhs of their own (they bind the collected/output
     value), so {!replace_cond} never touches their call sites. Instead,
     every rule whose conditions bind a success-reflected symbol's result --
     owise sibling or not, the same insertion applies system-wide -- gains an
     explicit [holds_<sym>(args) = true] test immediately before that
     binding.
     This is what lets a later owise guard reflecting THIS rule as a sibling
     find the success test already sitting in its conditions (ordinary
     {!ctest} then reflects it like any other condition); {!check_reflectable}
     separately stops gating the binding's own [App(helper, args)] once
     [helper] is in [succ]. Sound as an added condition (a successful
     collection implies its own success judgment) and safe: [succ] only ever
     contains a helper whose OWN rules were just reflected without a Gate. *)
  let insert_success_test (succ : string list) (conds : R.cond list) :
      R.cond list =
    List.concat_map
      (fun ((l, r) as c) ->
        match l with
        | R.App (f, args)
          when List.mem f succ
               && r <> T.bool_t ~scalars true
               && r <> T.bool_t ~scalars false ->
            [ (R.App (T.holds_sym f, args), T.bool_t ~scalars true); c ]
        | _ -> [ c ])
      conds
  in
  (* Respell every judgment/iteration-helper condition over the reflected
     set, then insert the collecting helpers' success tests. *)
  let base_rules =
    List.map
      (fun (r : R.rule) ->
        {
          r with
          R.conds =
            List.map (replace_cond ~scalars tbl succ) r.R.conds
            |> insert_success_test succ;
        })
      sys.R.rules
  in
  (* ---- owise phase. ---- *)
  let rules = Array.of_list base_rules in
  let reflected = ref 0 and enumerated = ref 0 and kept = ref 0 in
  let out =
    List.concat
      (List.mapi
         (fun i (r : R.rule) ->
           if not r.R.owise then [ r ]
           else
             match R.defined_head r with
             | None -> [ r ]
             | Some f -> (
                 let ow_args =
                   match r.R.lhs with R.App (_, args) -> args | _ -> []
                 in
                 let siblings =
                   List.filteri (fun j _ -> j < i) (Array.to_list rules)
                   |> List.filter (fun (s : R.rule) ->
                          (not s.R.owise) && R.defined_head s = Some f)
                 in
                 let argtyps = argtyps_of f (List.length ow_args) in
                 (* complement enumeration first; anything it does not cover
                    (or a Gate along the way) falls back to the or-gate. *)
                 let mark = snapshot sup in
                 match
                   try
                     complement_clauses ~scalars tbl sup effectful succ f r
                       ow_args argtyps siblings
                   with Gate _ -> None
                 with
                 | Some cls ->
                     incr enumerated;
                     let guarded =
                       List.length
                         (List.filter (fun (c : R.rule) -> c.R.conds <> []) cls)
                     in
                     Printf.eprintf
                       "reflect: owise complement-enumerated on %s (%d \
                        clause(s), %d guarded)\n"
                       f (List.length cls) guarded;
                     cls
                 | None -> (
                     rollback sup mark;
                     let mark = snapshot sup in
                     try
                       let guards =
                         List.map
                           (sibling_guard ~scalars tbl sup effectful succ
                              ow_args argtyps)
                           siblings
                       in
                       let guard =
                         match guards with
                         | [] ->
                             T.bool_t ~scalars false (* no sibling: keep rule *)
                         | g :: gs ->
                             List.fold_left (fun a b -> T.or_t a b) g gs
                       in
                       incr reflected;
                       [
                         {
                           r with
                           R.conds =
                             r.R.conds @ [ (guard, T.bool_t ~scalars false) ];
                           owise = false;
                         };
                       ]
                     with Gate reason ->
                       rollback sup mark;
                       incr kept;
                       Printf.eprintf "reflect: keeping owise on %s (%s)\n" f
                         reason;
                       [ r ])))
         (Array.to_list rules))
  in
  if !reflected > 0 || !enumerated > 0 || !kept > 0 then
    Printf.eprintf
      "reflect: %d owise rule(s) reflected, %d complement-enumerated, %d kept\n"
      !reflected !enumerated !kept;
  (* Keep only the support rules some rule actually references (a gated or
     simplified attempt may have pulled in a projection it no longer uses). *)
  let used = Hashtbl.create 256 in
  let rec mark_used (t : R.term) : unit =
    match t with
    | R.Var _ -> ()
    | R.App (f, args) ->
        Hashtbl.replace used f ();
        List.iter mark_used args
  in
  List.iter
    (fun (r : R.rule) ->
      mark_used r.R.rhs;
      List.iter
        (fun (l, rr) ->
          mark_used l;
          mark_used rr)
        r.R.conds)
    (out @ holds_rules);
  let support =
    List.filter
      (fun (r : R.rule) ->
        match R.defined_head r with
        | Some f -> Hashtbl.mem used f
        | None -> true)
      sup.rules
  in
  { sys with R.rules = out @ holds_rules @ support }
