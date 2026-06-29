(** The fixed CTRS prelude: the hand-written rules giving the {!Ctrs_term}
    scalar/list/option symbols their rewriting semantics (booleans, Peano nats,
    sign-magnitude ints, list/option operations and matchers, structural
    equality). Appended to the type-derived rules and pruned to reachability by
    {!To_ctrs.of_spec}. *)

val rules : Rewrite_system.rule list

(** The prelude symbols whose defining rules the [Native] scalar theory omits
    and {!To_maude} replaces with delegations to Maude's built-in theories; the
    [Structural] theory (analysis) keeps their hand-written rules. Must stay in
    sync with {!rules}. *)
val native_replaced_heads : string list
