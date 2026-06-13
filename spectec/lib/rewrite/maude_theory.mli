(** The native-theory pass for the execution pipeline: restate a structural CTRS
    ({!To_ctrs.of_spec}) over Maude's built-in Bool/Nat/Int/String. Ground
    scalar values fold into wrapper constructors ([nat(3)], [int(-5)],
    [bool(true)], [txt("E.")]); the hand-written scalar prelude rules are
    dropped, to be re-emitted by {!To_maude} as one-line delegations to the
    built-in operations. Structural rules (user types, lists/options, iteration
    helpers) pass through unchanged. The analysis (COPS) pipeline never runs
    this pass. *)
val native_system : Rewrite_system.t -> Rewrite_system.t

(** The wrapper constructor symbols ([nat]/[int]/[bool]/[txt]) -- the one
    spelling shared by the pass, {!To_maude}'s delegation equations, and the
    start-term encoder. *)
val nat_wrap_sym : string

val int_wrap_sym : string
val bool_wrap_sym : string
val text_wrap_sym : string

(** Wrapped-literal builders for the start-term encoder. *)
val nat_t : Bigint.t -> Rewrite_system.term

val int_t : Bigint.t -> Rewrite_system.term
val bool_t : bool -> Rewrite_system.term
val text_t : string -> Rewrite_system.term

(** A built-in literal symbol (decimal numeral or quoted string), printed
    verbatim by {!To_maude}: no identifier mangling, no op declaration. *)
val is_literal_sym : string -> bool
