(** Emit a translated spec as an executable, order-sorted Maude system module.

    Unlike the analysis surface ({!Rewrite_system.string_of_system_maude} over
    structural scalars, feeding the MFE confluence/coherence check), this
    targets Maude's built-in theory so the module can be {e run}:
    functions/prelude become equations, input-moded relations become equations
    too (so a [reduce] is deterministic), other relations become rules, and a
    start term is reduced/searched. [relations_as_rules] forces every relation
    back to a rule (the old behaviour, for exploring non-determinism via
    search).

    Sorts and operator signatures are recovered from the original
    (un-simplified) spec, so the elaborated spec is the input. See the module
    implementation for the order-sorted [Val] supersort scheme and the
    Maude-surface conventions. *)

(** Translate [orig] (an elaborated IL spec) all the way to Maude module text,
    over the direct Maude path ({!Pipeline.maude_system_of_spec}, i.e.
    {!To_ctrs.of_spec} with [~scalars:Native]): scalar values are wrapped
    built-in literals and the omitted scalar prelude
    ({!Prelude.native_replaced_heads}) is supplied as one-line delegation
    equations, so arithmetic and text operations run in constant time instead of
    structural recursion. *)
val module_of_spec :
  ?module_name:string -> ?relations_as_rules:bool -> Lang.Il.spec -> string

(** The module's reducible symbols (functions/relations/ops) in Maude spelling.
    Pass to {!Maude_run.run} as [~defined_heads] so a normal form still
    mentioning one of them is reported as stuck (a failed run). *)
val maude_defined_heads : Lang.Il.spec -> string list

(** Emit the module for an already-translated system (a
    {!Pipeline.maude_system_of_spec} result), given the [orig] spec it came from
    (needed for the sorts/signatures). *)
val module_of_system :
  ?module_name:string ->
  ?relations_as_rules:bool ->
  Lang.Il.spec ->
  Rewrite_system.t ->
  string

(** A spec symbol as it appears in the emitted module (sanitized + mangled), so
    a caller can name a relation/function to apply (e.g. ["Run_prog"] ->
    ["Run-prog"]). *)
val maude_sym : string -> string

(** [print_rule ~scalars vs rels defined is_rel r] renders one CTRS rule as
    [eq]/[ceq] (functional) or [rl]/[crl] (kept as a rule, [is_rel]), with its
    conditions in OPERATIONAL form: a rewrite condition [l => r] when [l] is a
    relation in [rels], a plain check [l = r] when [r] is a bool literal (in
    [scalars]' theory), otherwise a matching condition [r := l] (or [l := r],
    whichever side is fresh) that actually BINDS a fresh right-hand variable --
    unlike {!Rewrite_system.string_of_system_maude}'s/{!To_mfe}'s ANALYSIS-only
    plain [l = r] (sound for the MFE's critical-pair reasoning, which never
    executes anything, but not directly [reduce]-able: Maude's [reduce] does
    not bind a fresh variable across a bare [=]). Conditions are scheduled by
    binding-readiness (left to right is not always binding-respecting), and a
    bare-variable match gets an [{!stuck_head_sym}] guard so a stuck subterm
    does not silently masquerade as a bound value. [defined] is the set of
    symbols that reduce away (for that guard); [rels] the relations kept as
    rules. Exposed (unlike the rest of the Native module assembly below) so
    {!To_mfe}'s [Structural] execution path (a direct, non-reflective [reduce])
    can reuse this scheduling/condition machinery with
    [~scalars:Ctrs_term.Structural] instead of duplicating it. *)
val print_rule :
  scalars:Ctrs_term.scalar_theory ->
  (string, string) Hashtbl.t ->
  string list ->
  string list ->
  bool ->
  Rewrite_system.rule ->
  string

(** The name of the [Val -> Bool] predicate {!print_rule}'s matching
    conditions guard bare-variable binds with (see [print_rule]'s doc). *)
val stuck_head_sym : string

(** [stuck_head_eqs heads sg] emits the [{!stuck_head_sym}] equations: [true]
    on a term headed by one of [heads] (a stuck, not-yet-reduced application --
    [sg h n] gives its declared argument sorts, so the guard pattern types
    correctly), [false] otherwise ([owise]). A module using {!print_rule}'s
    matching conditions must declare these (plus the [{!stuck_head_sym} : Val
    -> Bool] op itself) for the guard to resolve. *)
val stuck_head_eqs :
  (string * int) list -> (string -> int -> string list * string) -> string list

(** The start application of relation [rel] (an IL relation name, e.g.
    ["Program_ok"]) on already-encoded {e META-TERM} argument terms, as a Maude
    META-TERM (['rel[args]]) for {!Maude_run}'s reflective [metaReduce] path.
    Appends the gensym seed when the translated system threads [rel]
    ({!Gensym}); such a run normalizes to [tuple(result, final-state)] instead
    of the bare result. *)
val meta_start_app : Lang.Il.spec -> string -> string list -> string

(** Encode a value to a ground {!Rewrite_system.term} in [scalars]' theory --
    the inverse [Of_maude.decode]/[Of_mfe]-style back-translation decodes.
    [Native] wraps built-in literals ({!Maude_theory}, unbounded -- a numeral
    never builds a Peano tower); [Structural] uses {!Ctrs_term}'s own
    Peano/sign-magnitude/char-list encoding, matching {!To_mfe}'s analysis
    module vocabulary (for a direct, non-reflective [reduce] on that module).
    Exposed (unlike the META-TERM encoder below, which is [Native]-only) so a
    structural start-term builder can reuse the variant/struct constructor
    resolution this does. *)
val encode_value : scalars:Ctrs_term.scalar_theory -> Lang.Il.spec -> Lang.Il.value -> Rewrite_system.term

(** Encode an IL [value] (e.g. a program parsed by a language front-end) to a
    ground Maude {e META-TERM} text in the module's vocabulary, for the
    reflective [metaReduce] start-term path ({!Maude_run}). Parses through the
    fixed META-TERM grammar instead of the module's giant mixfix signature (the
    dominant per-program cost). Scalars encode as the built-in meta literals
    Maude reflects them to (the [Native] scalar theory,
    {!To_ctrs.scalar_theory}); self-contained, nothing extra to declare. *)
val meta_term_of_value : Lang.Il.spec -> Lang.Il.value -> string
