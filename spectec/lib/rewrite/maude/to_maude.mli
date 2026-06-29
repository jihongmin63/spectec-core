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

(** The start application of relation [rel] (an IL relation name, e.g.
    ["Program_ok"]) on already-encoded {e META-TERM} argument terms, as a Maude
    META-TERM (['rel[args]]) for {!Maude_run}'s reflective [metaReduce] path.
    Appends the gensym seed when the translated system threads [rel]
    ({!Gensym}); such a run normalizes to [tuple(result, final-state)] instead
    of the bare result. *)
val meta_start_app : Lang.Il.spec -> string -> string list -> string

(** Encode an IL [value] (e.g. a program parsed by a language front-end) to a
    ground Maude {e META-TERM} text in the module's vocabulary, for the
    reflective [metaReduce] start-term path ({!Maude_run}). Parses through the
    fixed META-TERM grammar instead of the module's giant mixfix signature (the
    dominant per-program cost). Scalars encode as the built-in meta literals
    Maude reflects them to (the [Native] scalar theory,
    {!To_ctrs.scalar_theory}); self-contained, nothing extra to declare. *)
val meta_term_of_value : Lang.Il.spec -> Lang.Il.value -> string
