(** The native (built-in) scalar theory for the execution pipeline: the one
    spelling of the scalar wrapper constructors and the literal builders for
    them.

    The analysis pipeline keeps self-contained structural scalars (binary nats,
    sign-magnitude ints, char-list texts, own booleans) because a CTRS has no
    external theories. The execution pipeline instead targets Maude's built-in
    Bool/Nat/Int/String: ground scalar values live in wrapper constructors over
    those sorts ([nat(3)], [int(-5)], [bool(true)], [txt("E.")]), and the scalar
    prelude rules ({!Prelude.native_replaced_heads}) are re-emitted by
    {!To_maude} as one-line delegations to the built-in operations.

    In the new-rewrite design those wrappers are produced DIRECTLY by
    {!To_ctrs.of_spec} with [~scalars:Native] -- there is no separate fold pass
    over a structural system. This module is the shared low-level home for the
    wrapper spelling and literal builders that {!To_ctrs} (Native emission),
    {!To_maude} (delegation equations + start-term encoder) and {!Of_maude}
    (decoding) must all agree on. *)

(** The wrapper constructor symbols ([nat]/[int]/[bool]/[txt]) -- the one
    spelling shared by {!To_ctrs}'s Native emission, {!To_maude}'s delegation
    equations and start-term encoder, and {!Of_maude}'s decoder. *)
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

(** A Maude string literal for the given OCaml string (C-style escapes;
    non-printable/UTF-8 bytes as 3-digit octal). The forward encoding
    {!Of_maude} inverts. *)
val string_literal : string -> string
