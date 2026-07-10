(** Which scalar theory the emitted rules target (defined in {!Ctrs_term}, where
    the mode-aware scalar leaf builders live).

    - [Structural]: self-contained scalars (binary nats, sign-magnitude ints,
      char-list texts, own booleans) with their hand-written prelude rules, for
      the analysis pipeline.
    - [Native]: scalar leaves are emitted directly as Maude's built-in wrappers
      ([nat]/[int]/[bool]/[txt]) and the structural scalar prelude rules
      ({!Prelude.scalar_rules}) are omitted, for the Maude execution backend. *)
type scalar_theory = Ctrs_term.scalar_theory = Structural | Native

(** Translate an elaborated IL spec into a CTRS ({!Rewrite_system.t}).

    [orig] is the spec before {!Simplify.simplify_spec}; it supplies the type
    definitions and relation signatures used to derive constructor/matcher/
    subtype rules and to split relation arguments. [simplified] is the spec
    whose function clauses and relation rules become the body rewrite rules.
    [extra_defs] are additional definition rules added to the prunable pool.
    [scalars] selects the scalar theory (default [Structural]). *)
val of_spec :
  ?scalars:scalar_theory ->
  ?extra_defs:Rewrite_system.rule list ->
  orig:Lang.Il.spec ->
  Lang.Il.spec ->
  Rewrite_system.t

(** The slice roots of [spec]: the CTRS symbol each top-level function/relation
    defines, in declaration order. *)
val def_symbols : Lang.Il.spec -> string list

(** The CTRS symbols of relations that declare a non-empty input mode
    ([hint(input ...)]); such relations are functional and may be emitted as
    equations. *)
val input_moded_rel_syms : Lang.Il.spec -> string list

(** The CTRS symbols of relations with an empty input mode: the complement of
    {!input_moded_rel_syms}, emitted as rules rather than equations. *)
val rule_head_syms : Lang.Il.spec -> string list

(** The constructor of a one-case variant type [name] in the spec: a builder
    wrapping its arguments in that case's [variant_<origin>_<mixop>] symbol.
    [None] when the name is undefined or not a single-case variant. *)
val single_case_ctor :
  Lang.Il.spec ->
  string ->
  (Rewrite_system.term list -> Rewrite_system.term) option

(** [case_ctor spec name case_sym]: the constructor of the case of (multi-case)
    variant type [name] whose generated symbol equals [case_sym]. [None] when
    [name] is undefined or has no such case. *)
val case_ctor :
  Lang.Il.spec ->
  string ->
  string ->
  (Rewrite_system.term list -> Rewrite_system.term) option
