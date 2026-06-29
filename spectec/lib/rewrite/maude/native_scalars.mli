(** Fold an assembled structural CTRS into the native (built-in) scalar theory:
    ground scalars become Maude's built-in wrappers ([nat]/[int]/[bool]/[txt],
    see {!Maude_theory}) and the prelude rules the built-in delegations replace
    ({!Prelude.native_replaced_heads}) are dropped. Applied by
    {!To_ctrs.of_spec} on the [Native] path.

    NOTE (port): the intended new design emits Native directly at the scalar
    leaves, so this post-fold is slated for removal (todo.md refactor B). *)
val fold : Rewrite_system.t -> Rewrite_system.t
