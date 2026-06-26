(* The IL -> rewriting-system translation, in two pipelines that share the SAME
   structural translation ({!Simplify} then {!To_ctrs.of_spec}) and diverge only
   at the scalar theory ({!To_ctrs.scalar_theory}). Every consumer goes through
   one of these builders instead of re-assembling the stages.

   The rewrite-branch pipeline wrapped the core with feature passes that the
   new-rewrite skeleton deleted -- reintroduce them around the core as you
   reimplement (see CORE_LOGIC.md §5):
     - [Defunctionalize.defunctionalize spec] FIRST, so every call is
       first-order before simplification/translation.
     - [Builtin.rules_of_builtins spec] as [~extra_defs] (P4 collection
       builtins).
     - [Gensym.thread] LAST, threading the [$fresh_typeId] state.

   Debug fallback: to tell whether an odd rule comes from [Simplify] or from
   [To_ctrs], pass [spec] as the last argument instead of
   [Simplify.simplify_spec spec]. *)

(* Analysis pipeline: self-contained structural scalars, for the COPS/TPDB
   surfaces. *)
let ctrs_of_spec (spec : Lang.Il.spec) : Rewrite_system.t =
  To_ctrs.of_spec ~scalars:To_ctrs.Structural ~orig:spec
    (Simplify.simplify_spec spec)

(* Execution pipeline: the DIRECT IL -> Maude path. The native built-in theory
   is the translation target from the start ([~scalars:Native]), so this is NOT
   a re-fold of [ctrs_of_spec] -- the old intermediate native-theory pass
   (deleted [Maude_theory.native_system]) is gone. Consumed only by
   {!To_maude}. See CORE_LOGIC.md §1, §6.1. *)
let maude_system_of_spec (spec : Lang.Il.spec) : Rewrite_system.t =
  To_ctrs.of_spec ~scalars:To_ctrs.Native ~orig:spec
    (Simplify.simplify_spec spec)
