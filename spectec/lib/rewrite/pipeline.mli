(** The analysis pipeline: the IL -> CTRS translation ({!Defunctionalize}, the
    {!Simplify} pre-pass, {!To_ctrs.of_spec} with {!Builtin}'s collection rules,
    then {!Gensym.thread}) with self-contained structural scalars
    ([~scalars:Structural]), for the MFE (CRC/ChC) analysis surface. *)
val ctrs_of_spec : Lang.Il.spec -> Rewrite_system.t

(** The execution pipeline: the {b direct} IL -> Maude path. The same structural
    translation as {!ctrs_of_spec}, but targeting Maude's built-in
    Bool/Nat/Int/String from the start ([~scalars:To_ctrs.Native]) -- NOT a
    re-fold of the structural system. Consumed only by {!To_maude}. *)
val maude_system_of_spec : Lang.Il.spec -> Rewrite_system.t
