(** The analysis pipeline: the full IL -> CTRS translation ({!Simplify}
    pre-pass, {!Builtin} rules, {!To_ctrs}) with self-contained structural
    scalars, for the COPS/TPDB analysis surfaces. *)
val ctrs_of_spec : Lang.Il.spec -> Rewrite_system.t

(** The execution pipeline: [ctrs_of_spec] restated over Maude's built-in
    Bool/Nat/Int/String ({!Maude_theory.native_system}), for {!To_maude}. *)
val maude_system_of_spec : Lang.Il.spec -> Rewrite_system.t
