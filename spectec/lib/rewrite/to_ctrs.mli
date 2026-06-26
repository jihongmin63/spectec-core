(** Which scalar theory the emitted rules target -- the one seam at which the
    analysis and the Maude pipelines diverge, so they share the {e same}
    structural translation and no separate re-fold pass is needed.

    - [Structural]: self-contained scalars (Peano nats, sign-magnitude ints,
      char-list texts, own booleans) with their hand-written prelude rules. The
      MFE analysis surface ({!Rewrite_system.string_of_system_maude} feeding the
      CRC/ChC) needs a closed system with no external theories -- the
      Church-Rosser check computes critical pairs and cannot reason over Maude's
      built-in Nat/Int/Bool/String.
    - [Native]: ground scalars fold into built-in wrappers ([nat]/[int]/[bool]/
      [txt]) and the {!native_replaced_heads} prelude rules are OMITTED, so
      {!To_maude} can re-emit them as one-line delegations to Maude's
      Bool/Nat/Int/String. This is the {b direct} IL -> Maude path -- the
      execution system is produced here, not refolded from the structural one.
*)
type scalar_theory = Structural | Native

(** Translate an elaborated IL spec into a CTRS ({!Rewrite_system.t}).

    [orig] is the spec before {!Simplify.simplify_spec}; it supplies the type
    definitions and relation signatures used to derive constructor/matcher/
    subtype rules and to split relation arguments. [simplified] is the spec
    whose function clauses and relation rules become the body rewrite rules.
    [extra_defs] are additional definition rules (e.g. {!Builtin}'s
    collection-builtin rules) added to the prunable pool, kept only where the
    body actually reaches them. [scalars] selects the scalar theory (default
    [Structural]); pass [Native] for the Maude backend. *)
val of_spec :
  ?scalars:scalar_theory ->
  ?extra_defs:Rewrite_system.rule list ->
  orig:Lang.Il.spec ->
  Lang.Il.spec ->
  Rewrite_system.t

(** The slice roots of [spec]: the CTRS symbol each top-level function/relation
    defines, in declaration order. Pair with {!Rewrite_system.slice} to check
    confluence one symbol's dependency closure at a time. *)
val def_symbols : Lang.Il.spec -> string list

(** The CTRS symbols of relations that declare a non-empty input mode
    ([hint(input ...)]). Such relations are functional (inputs determine
    outputs), so {!To_maude} may emit them as equations instead of rules. *)
val input_moded_rel_syms : Lang.Il.spec -> string list

(** {1 Symbol naming}

    The CTRS symbol-naming convention. Exposed so other backends (e.g.
    {!To_maude}) can recover a symbol's declaration from [orig] using the
    {e same} functions that produced it in the rules — the names must agree
    between a symbol's definition site and every use site. *)

val sanitize : string -> string
val variant_sym : string -> Lang.Il.mixop -> string
val struct_sym : string -> string
val field_sym : string -> Lang.Il.Mixfix.atom -> string
val upd_field_sym : string -> Lang.Il.Mixfix.atom -> string
val match_sym : string -> Lang.Il.mixop -> string
val subty_sym : string -> string
val func_sym : Lang.Il.id -> string
val rel_sym : Lang.Il.id -> string

(** Split a relation's notation arguments into its input and output positions,
    given the declared input indices. Shared with the relation signature
    recovery in {!To_maude}. *)
val split_inputs : int list -> 'a list -> 'a list * 'a list

(** Per defined symbol ([func_sym]/[rel_sym] of each [DecD]/[RelD]), the IL type
    of each variable in its clauses/rules, recovered from the simplified spec's
    [VarE] notes. Lets a typed backend ({!To_maude}) restore a variable's
    declared (narrow) type instead of the widened argument type a relation
    position would impose. A variable whose occurrences disagree is omitted. *)
val var_type_hints :
  Lang.Il.spec -> (string, (string * Lang.Il.typ') list) Hashtbl.t

(** {1 Term and rule builders}

    The construction layer other backends ({!Builtin}) emit CTRS rules through,
    so all raw {!Rewrite_system.App}/{!Rewrite_system.Var} construction stays
    confined here and the prelude symbols a built rule references ([cons], [eq],
    [mem], …) match their prelude definitions by name. *)

val var_t : string -> Rewrite_system.term
val app_t : string -> Rewrite_system.term list -> Rewrite_system.term

val variant_t :
  string -> Lang.Il.mixop -> Rewrite_system.term list -> Rewrite_system.term

val tuple_t : Rewrite_system.term list -> Rewrite_system.term
val true_t : Rewrite_system.term
val false_t : Rewrite_system.term
val nil_t : Rewrite_system.term
val none_t : Rewrite_system.term
val some_t : Rewrite_system.term -> Rewrite_system.term
val cons_t : Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term
val not_t : Rewrite_system.term -> Rewrite_system.term
val and_t : Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term
val eq_t : Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term
val mem_t : Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term
val cat_t : Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term
val take_t : Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term
val drop_t : Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term
val len_t : Rewrite_system.term -> Rewrite_system.term
val sub_t : Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term

(** Numeric/char builders for builtins that compute over numbers (e.g.
    [int_to_text]'s decimal rendering): nats are Peano [zero]/[succ]
    ([peano_of_int] for a literal), ints are sign-magnitude [int_pos]/[int_neg],
    and a text byte is a nullary [chr_<code>]. *)

val zero_t : Rewrite_system.term
val succ_t : Rewrite_system.term -> Rewrite_system.term
val peano_of_int : int -> Rewrite_system.term
val int_pos_t : Rewrite_system.term -> Rewrite_system.term
val int_neg_t : Rewrite_system.term -> Rewrite_system.term
val lt_t : Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term
val div_t : Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term
val mod_t : Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term

val add_int_t :
  Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term

val sub_int_t :
  Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term

val lt_int_t : Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.term
val negate_int_t : Rewrite_system.term -> Rewrite_system.term
val chr_t : int -> Rewrite_system.term

(** The nullary symbol of one text byte ([chr_<code>], what {!chr_t} applies).
*)
val chr_sym : int -> string

(** A text as the [cons]/[nil] list of its [chr_<code>] bytes -- the one shape
    every text takes, whether a spec literal ([TextE]) or a program value a
    backend encodes, so structural list/text operations apply to both. *)
val text_t : string -> Rewrite_system.term

(** The byte code of a [chr_<code>] constant symbol, [None] for any other. *)
val chr_code_of_sym : string -> int option

(** The byte alphabet in play in [rules], read back from the [chr_<code>]
    constants they reference. {!of_spec} closes [eq] over the spec's own
    alphabet; a backend whose start term introduces further bytes closes the
    remainder against this. *)
val char_codes_of_rules : Rewrite_system.rule list -> int list

(** The prelude symbols whose defining rules the [Native] scalar theory omits
    and {!To_maude} replaces with delegations to Maude's built-in theories; the
    [Structural] theory (analysis) keeps their hand-written structural rules. *)
val native_replaced_heads : string list

(** [rule lhs rhs] / [rule_cond lhs rhs conds]: an unconditional / conditional
    rewrite rule (with [owise = false]). *)
val rule : Rewrite_system.term -> Rewrite_system.term -> Rewrite_system.rule

val rule_cond :
  Rewrite_system.term ->
  Rewrite_system.term ->
  Rewrite_system.cond list ->
  Rewrite_system.rule

(** The constructor of a one-case variant type (e.g. [set]'s [`{ }] or [pair]'s
    [`:]) in the spec: a builder wrapping its arguments in that case's
    [variant_<origin>_<mixop>] symbol, the exact symbol the spec's own literals
    produce. [None] when the name is undefined or not a single-case variant. *)
val single_case_ctor :
  Lang.Il.spec ->
  string ->
  (Rewrite_system.term list -> Rewrite_system.term) option

(** [case_ctor spec name case_sym]: the constructor of the case of (multi-case)
    variant type [name] whose generated symbol equals [case_sym] -- the
    {!single_case_ctor} lookup, selecting one case among several. [None] when
    [name] is undefined or has no such case. *)
val case_ctor :
  Lang.Il.spec ->
  string ->
  string ->
  (Rewrite_system.term list -> Rewrite_system.term) option
