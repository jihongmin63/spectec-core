(** Emit an execution-target system as an order-sorted Maude module. Scalar
    operations delegate to native theories. Functions and input-moded relations
    are equations. Pass the specialized [spec] and [system] returned by
    {!Rewrite.compile} with [~target:Execution] together, so signature recovery
    sees the emitted definition names. *)

(** The reducible symbols (functions/relations/ops) of an already-translated
    system ({!Rewrite.compile}) in Maude spelling. Pass to {!Maude_run.run} as
    [~defined_heads] so a normal form still mentioning one of them is reported
    as stuck (a failed run). *)
val maude_defined_heads : Rewrite_system.t -> string list

(** Emit the native execution module for the specialized [spec] and [system]
    returned by {!Pipeline.compile}. *)
val module_of_system :
  ?module_name:string -> Lang.Il.spec -> Rewrite_system.t -> string

(** A spec symbol as it appears in the emitted module (sanitized + mangled), so
    a caller can name a relation/function to apply (e.g. ["Run_prog"] ->
    ["Run-prog"]). *)
val maude_sym : string -> string

(** Render one translated rule as an equation with operational conditions.
    Shared with the direct structural execution module. *)
val print_rule :
  scalars:Ctrs_term.scalar_theory ->
  (string, string) Hashtbl.t ->
  string list ->
  Rewrite_system.rule ->
  string

(** The name of the [Val -> Bool] predicate {!print_rule}'s matching conditions
    guard bare-variable binds with (see [print_rule]'s doc). *)
val stuck_head_sym : string

(** [stuck_head_eqs heads] emits the [{!stuck_head_sym}] equations: [true] on a
    term headed by one of [heads] (a stuck, not-yet-reduced application),
    [false] otherwise ([owise]). The guard patterns take [Val] arguments, since
    a stuck term's own arguments may sit at the [Val] kind -- see the
    implementation. A module using {!print_rule}'s matching conditions must
    declare these (plus the [{!stuck_head_sym} : Val -> Bool] op itself) for the
    guard to resolve. *)
val stuck_head_eqs : (string * int) list -> string list

(** The start application of relation [rel] (an IL relation name, e.g.
    ["Program_ok"]) on already-encoded {e META-TERM} argument terms, as a Maude
    META-TERM (['rel[args]]) for {!Maude_run}'s reflective [metaReduce] path.
    Appends the gensym seed when the translated [system] threads [rel]
    ({!Gensym}); such a run normalizes to [tuple(result, final-state)] instead
    of the bare result. [system] is the one the module was emitted from
    ({!Rewrite.compile}). *)
val meta_start_app :
  Lang.Il.spec -> Rewrite_system.t -> string -> string list -> string

(** Encode a value to a ground {!Rewrite_system.term} in [scalars]' theory --
    the inverse [Of_maude.decode]/[Of_mfe]-style back-translation decodes.
    [Native] wraps built-in literals ({!Maude_theory}, unbounded -- a numeral
    never builds a Peano tower); [Structural] uses {!Ctrs_term}'s own
    binary/sign-magnitude/char-list encoding, matching {!To_mfe}'s analysis
    module vocabulary (for a direct, non-reflective [reduce] on that module).
    Exposed (unlike the META-TERM encoder below, which is [Native]-only) so a
    structural start-term builder can reuse the variant/struct constructor
    resolution this does. *)
val encode_value :
  scalars:Ctrs_term.scalar_theory ->
  Lang.Il.spec ->
  Lang.Il.value ->
  Rewrite_system.term

(** Encode an IL [value] (e.g. a program parsed by a language front-end) to a
    ground Maude {e META-TERM} text in the module's vocabulary, for the
    reflective [metaReduce] start-term path ({!Maude_run}). Parses through the
    fixed META-TERM grammar instead of the module's giant mixfix signature (the
    dominant per-program cost). Scalars encode as the built-in meta literals
    Maude reflects them to (the [Native] scalar theory,
    {!To_ctrs.scalar_theory}); self-contained, nothing extra to declare. *)
val meta_term_of_value : Lang.Il.spec -> Lang.Il.value -> string
