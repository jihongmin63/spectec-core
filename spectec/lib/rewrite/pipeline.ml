(* The one composition of the IL -> CTRS translation. Every consumer (the COPS
   surface {!Rewrite.rewrite_spec} and the Maude backend {!To_maude}) goes
   through this builder instead of re-assembling the stages, so they all reduce
   the same system.

   The rewrite-branch pipeline wrapped this core with feature passes that the
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
let ctrs_of_spec (spec : Lang.Il.spec) : Rewrite_system.t =
  To_ctrs.of_spec ~orig:spec (Simplify.simplify_spec spec)

(* The execution pipeline: the same translation restated over Maude's built-in
   Bool/Nat/Int/String, consumed only by {!To_maude}. The analysis surface
   (COPS/TPDB) keeps the structural system above -- the two intentionally
   diverge here (CORE_LOGIC.md §1, §6.1).

   STUBBED: the native-theory fold lived in the deleted [Maude_theory]
   ([native_system]); reintroduce it here. *)
let maude_system_of_spec (spec : Lang.Il.spec) : Rewrite_system.t =
  ignore (ctrs_of_spec spec);
  failwith
    "TODO(new-rewrite): reimplement the native-theory fold (Maude_theory.native_system)"
