(** The fixed CTRS prelude in the given scalar theory: the hand-written rules
    giving the {!Ctrs_term} scalar/list/option symbols their rewriting semantics
    (booleans, Peano nats, sign-magnitude ints, list/option operations and
    matchers, structural equality), with boolean leaves emitted in [~scalars].
    The [Native] theory drops the {!native_replaced_heads} rules (the Maude
    backend delegates them to its built-in theories); [Structural] keeps them.
    Appended to the type-derived rules and pruned to reachability by
    {!To_ctrs.of_spec}. *)
val rules : scalars:Ctrs_term.scalar_theory -> Rewrite_system.rule list

(** The prelude symbols whose defining rules the [Native] scalar theory omits
    and {!To_maude} replaces with delegations to Maude's built-in theories; the
    [Structural] theory (analysis) keeps their hand-written rules. Must stay in
    sync with {!To_maude}'s delegation equations. *)
val native_replaced_heads : string list
