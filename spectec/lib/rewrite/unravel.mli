(** Unraveling of a (sliced) analysis CTRS into a plain TPDB TRS, for a direct
    AProVE termination proof.

    Why unravel at all: MTT, the tool this replaces, does NOT -- it rewrites a
    matching condition [s = t] into an equality TEST [equal(s,t) -> tt], which
    frees the variables [t] binds and makes the descent they carry invisible.
    Unraveling puts the condition's pattern back where it belongs, in a rule's
    left-hand side, where matching binds its variables again:

    {v
    f(p1..pk)        -> u_1(s1, x1..xn)
    u_1(t1, x1..xn)  -> r        (for f(p1..pk) -> r if s1 = t1, x1..xn the p's variables)
    v}

    The helper carries the left-hand side's VARIABLES, flat. Wrapping the
    argument list in an inert keep-constructor instead is equally sound and
    equally provable on one slice, but it hides the structural descent from
    size-change analysis and forces a reduction-pair search in its place -- on
    the 577 dependency-graph children of [Program_ok] that is 317 closed against
    502. See the implementation comment for the measurement.

    A condition may bind variables a later condition or the final rhs uses, so
    each chain step carries the original arguments plus every variable first
    bound by an earlier condition. Conditions are re-ordered greedily to binding
    order first (evaluated side fully bound; its pattern side then binds), the
    same convention as {!Crc_surface.order_conds}.

    Sorts and subsorts are dropped, so the TRS is an OVER-approximation (more
    terms match than in the typed system): termination of the TRS implies
    termination of the sorted CTRS, never the reverse. An [owise] rule is
    included with the attribute dropped -- also an over-approximation (the rule
    fires more often), so the direction is preserved; skipping it (the
    scratchpad predecessor's behaviour) would UNDER-approximate and was only
    sound because no slice had one. *)

type stats = {
  eqs : int;  (** rules of the input system *)
  rules : int;  (** TRS rules emitted *)
  chain_steps : int;  (** [u_i] helpers introduced (one per condition) *)
  vars : int;  (** distinct TPDB variables *)
  owise : int;  (** rules whose [owise] attribute was dropped *)
}

val string_of_stats : stats -> string

(** One unraveled system before any surface's spelling. Shared with {!Itrs},
    which spells the same rules with integer constraints -- the chain
    construction and its coverage checks must not diverge between the two
    surfaces, or they would prove different systems under the same name. *)
type unraveled = {
  unconditional : (Rewrite_system.term * Rewrite_system.term) list;
  defines : (string * string) list;
      (** every symbol the unraveling defines, paired with the CTRS symbol whose
          rule produced it: a defined head with itself, a chain helper [u_<n>]
          with the head of the rule whose condition introduced it. *)
  steps : int;  (** [u_i] helpers introduced (one per condition) *)
  dropped_owise : int;  (** rules whose [owise] attribute was dropped *)
}

(** Every conditional rule turned into its [u_i] chain. [Error] as for
    {!trs_of_system}, minus the identifier-collision case (no identifier is
    spelled here). *)
val unconditional_rules : Rewrite_system.t -> (unraveled, string) result

(** Spell unconditional rules as TPDB text, returning the printed [(lhs, rhs)]
    pairs and the sorted variable names for the [(VAR ...)] header. Shared with
    {!Itrs} so one identifier scrub and one collision table serve both surfaces.
    A nullary symbol spelled [#<digits>] prints bare, which is how {!Itrs} gets
    a folded integer literal into the file rather than a scrubbed identifier.
    [Error] on an identifier collision between two distinct SYMBOLS (two
    distinct variables are renumbered instead). *)
val print_rules :
  (Rewrite_system.term * Rewrite_system.term) list ->
  ((string * string) list * string list, string) result

(** The TPDB spelling of one symbol name, before collision resolution -- what
    {!Itrs} needs to name a hand-written rule after a symbol the slice calls. *)
val tpdb_sym : string -> string

(** One unraveled system as its exporters need it: the printed rules one by one,
    the identifiers the [(VAR ...)] header declares, and where each defined
    symbol came from. A caller that emits a SUBSET of the rules ({!Qdp},
    {!Termination}'s slice re-ask) must select from [printed] rather than
    re-print its subset. Two reasons, either one sufficient: the collision table
    is resolved over the whole rule list, so a subset printed on its own may
    spell a symbol differently; and the [u_<n>] helpers are numbered by a
    counter over the whole rule list, so unraveling a subset separately numbers
    them differently again. Selecting keeps one spelling for one system, which
    is what lets a caller compare its subset against what AProVE printed back.

    [origins] is {!unraveled.defines} in the TPDB spelling of its key: the
    symbol a printed rule's left-hand side is rooted by, paired with the CTRS
    symbol whose rule produced it. That is what turns "these rules" into "the
    rules of these CTRS heads" -- the question a slice asks. *)
type export = {
  printed : (string * string) list;
  vars : string list;
  origins : (string * string) list;
}

val export_of_system : Rewrite_system.t -> (export * stats, string) result

(** The TPDB document over a (possibly partial) printed rule list: a [(VAR ...)]
    header and a [(RULES ...)] block. *)
val tpdb_document : vars:string list -> (string * string) list -> string

(** The TPDB [(VAR ...)(RULES ...)] text of the unraveled system, with its
    statistics. [Error] on: conditions no greedy schedule can order (a binding
    cycle), a rule whose unraveled rhs has a variable its lhs does not bind (a
    malformed TRS), or two distinct identifiers colliding after the TPDB
    identifier scrub. *)
val trs_of_system : Rewrite_system.t -> (string * stats, string) result
