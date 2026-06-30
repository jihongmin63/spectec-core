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

let build (scalars : To_ctrs.scalar_theory) (spec : Lang.Il.spec) :
    Rewrite_system.t =
  (* First: specialize away [def]-valued arguments. *)
  let spec = Defunctionalize.defunctionalize spec in
  To_ctrs.of_spec ~scalars
    ~extra_defs:(Builtin.rules_of_builtins ~scalars spec)
    ~orig:spec
    (Simplify.simplify_spec spec)
  (* Last: thread the gensym state, so both surfaces see the same pure gensym. *)
  |> Gensym.thread ~scalars

(* Analysis pipeline: self-contained structural scalars, for the MFE
   (CRC/ChC) confluence/coherence surface. A final
   {!Rewrite_system.fold_premise_binders} pass folds each premise-bound variable
   (a relation/function output, or a field a destructuring extracts) back into
   the rule -- into its rhs or its head pattern -- and drops the guards the fold
   makes redundant, so the MFE's Church-Rosser checker is not tripped by the
   spurious critical pairs the single-sort [prod = v] / [v = K(..)] condition
   rendering of a (deterministic) binding would otherwise raise. Analysis-only:
   the execution pipeline keeps the binding as a [:=] matching condition. *)
let ctrs_of_spec (spec : Lang.Il.spec) : Rewrite_system.t =
  Rewrite_system.fold_premise_binders
    ~rule_heads:(To_ctrs.rule_head_syms spec)
    (build To_ctrs.Structural spec)

(* Execution pipeline: the DIRECT IL -> Maude path. The native built-in theory
   is the translation target from the start ([~scalars:Native]), so this is NOT
   a re-fold of [ctrs_of_spec] -- the old intermediate native-theory pass
   (deleted [Maude_theory.native_system]) is gone. Consumed only by
   {!To_maude}. See CORE_LOGIC.md §1, §6.1. *)
let maude_system_of_spec (spec : Lang.Il.spec) : Rewrite_system.t =
  build To_ctrs.Native spec
