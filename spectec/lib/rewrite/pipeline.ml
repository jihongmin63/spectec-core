(* The IL -> rewriting-system translation, in two pipelines that share the SAME
   structural translation and diverge only at the scalar theory
   ({!To_ctrs.scalar_theory}). Every consumer goes through one of these builders
   instead of re-assembling the stages.

   Both pipelines wrap the core translation with the feature passes:
     - {!Defunctionalize.defunctionalize} FIRST, so every call is first-order
       before simplification/translation see it.
     - {!Builtin.rules_of_builtins} as [~extra_defs] (P4 collection builtins,
       pruned where unreached).
     - {!Gensym.thread} LAST, threading the [$fresh_typeId] state through every
       gensym-reaching symbol.
   Each is the identity on a spec without the feature it handles (the impty
   specs have no def parameter, collection builtin, or gensym), so the pinned
   goldens are unaffected.

   Debug fallback: to tell whether an odd rule comes from {!Simplify} or from
   {!To_ctrs}, pass [spec] as the last argument instead of
   [Simplify.simplify_spec spec]. *)

let build_with (scalars : To_ctrs.scalar_theory) (spec : Lang.Il.spec) :
    Lang.Il.spec * Rewrite_system.t =
  (* First: specialize away [def]-valued arguments. *)
  let spec = Defunctionalize.defunctionalize spec in
  let sys =
    To_ctrs.of_spec ~scalars
      ~extra_defs:(Builtin.rules_of_builtins ~scalars spec)
      ~orig:spec
      (Simplify.simplify_spec spec)
    (* Last: thread the gensym state, so both surfaces see the same pure
       gensym. *)
    |> Gensym.thread ~scalars
  in
  (spec, sys)

let build (scalars : To_ctrs.scalar_theory) (spec : Lang.Il.spec) :
    Rewrite_system.t =
  snd (build_with scalars spec)

(* Analysis pipeline: self-contained structural scalars, for the MFE
   (CRC/ChC) confluence/coherence surface, with three analysis-only final
   passes. {!Reflect.hoist_matchers} first respells every opaque
   [match_K(subj) = true] guard as the structural equation
   [subj = K(fresh..)] -- a pure respelling, deciding nothing itself --
   so {!Rewrite_system.fold_premise_binders} can then recognize and fold a
   head-bound discriminator variable into the constructor pattern its guard
   tests (turning a guarded multi-clause dispatch, several rules sharing one
   head with disjointness carried only by an opaque [match_*] condition the
   CRC cannot see through, into genuinely disjoint head patterns).
   [fold_premise_binders] also folds any OTHER premise-bound variable (a
   relation/function output, or a field a destructuring extracts) back into
   the rule -- into its rhs or its head pattern -- and drops the guards the
   fold makes redundant, so the MFE's Church-Rosser checker is not tripped by
   the spurious critical pairs the single-sort [prod = v] / [v = K(..)]
   condition rendering of a (deterministic) binding would otherwise raise.
   {!Reflect.owise} then replaces each reflectable [owise] rule's marker with
   the explicit "no earlier sibling applies" guard condition, so the checker
   discharges the owise/sibling pairs instead of flagging them (the
   unreflectable remainder keeps its flag for {!Mfe}'s [drop_owise]
   fallback). It also, system-wide (not only for owise rules), inserts a
   [holds_<helper>(args) = true] test before any condition binding a
   success-reflected [$itercollect]/[$iterapply] helper's result -- the
   helper's own call site never carries a bool rhs for
   {!Reflect.replace_cond} to respell, so the guard alignment a later owise
   reflection needs is added as an extra condition instead. All three are
   analysis-only and add/respell conditions only, never removing information:
   soundness is unaffected. The execution pipeline keeps [:=] bindings, the
   [owise] attribute, and the opaque [match_*] guards; it sees none of these
   passes. *)
let ctrs_of_spec (spec : Lang.Il.spec) : Rewrite_system.t =
  let dspec, sys = build_with To_ctrs.Structural spec in
  let sys =
    Reflect.hoist_matchers ~scalars:To_ctrs.Structural ~orig:dspec sys
  in
  let sys =
    Rewrite_system.fold_premise_binders
      ~rule_heads:(To_ctrs.rule_head_syms spec)
      sys
  in
  Reflect.owise ~scalars:To_ctrs.Structural ~orig:dspec
    ~effectful:(Gensym.effectful_syms sys)
    sys

(* Execution pipeline: the DIRECT IL -> Maude path. The native built-in theory
   is the translation target from the start ([~scalars:Native]), so this is NOT
   a re-fold of [ctrs_of_spec] -- the old intermediate native-theory pass
   (deleted [Maude_theory.native_system]) is gone. Consumed only by
   {!To_maude}. See CORE_LOGIC.md §1, §6.1. *)
let maude_system_of_spec (spec : Lang.Il.spec) : Rewrite_system.t =
  build To_ctrs.Native spec
