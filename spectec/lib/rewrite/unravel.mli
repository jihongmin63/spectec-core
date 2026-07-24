(** Structure-preserving unraveling of a (sliced) analysis CTRS into a plain
    TPDB TRS, for a direct AProVE termination proof.

    Why not MTT's unraveling: MTT passes the CONDITION'S variables to the helper
    symbol, taking an argument term apart and rebuilding it on the right --
    which inverts the subterm relation that carried the descent, so no argument
    projection can orient the resulting dependency pairs. Here the helper
    carries the left-hand side's argument list UNCHANGED, wrapped in a fresh
    inert keep-constructor, so the original argument structure survives every
    chain step:

    {v
    f(p1..pk)            -> u_1(s1, k_1(p1..pk))
    u_1(t1, k_1(p1..pk)) -> r                        (for f(p1..pk) -> r if s1 = t1)
    v}

    A condition may bind variables a later condition or the final rhs uses, so
    each keep-constructor carries the original arguments plus every variable
    first bound by an earlier condition. Conditions are re-ordered greedily to
    binding order first (evaluated side fully bound; its pattern side then
    binds), the same convention as {!Crc_surface.order_conds}.

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
  chain_steps : int;  (** [u_i]/[k_i] pairs introduced (one per condition) *)
  vars : int;  (** distinct TPDB variables *)
  owise : int;  (** rules whose [owise] attribute was dropped *)
}

val string_of_stats : stats -> string

(** The greedy binding-order schedule of a rule's conditions -- the convention
    {!Crc_surface.order_conds} normalizes to, and the one the unraveling above
    lifts patterns in. Given the variables the lhs binds, repeatedly take the
    EARLIEST condition whose evaluated side is fully bound, then treat its
    pattern side as a binder. [None] on a residue nothing can bind (a binding
    cycle).

    Exported so a checker can replay the same order -- and the bound set at each
    step -- from the single definition here instead of carrying its own copy of
    the greedy. *)
val schedule_conds :
  string list -> Rewrite_system.cond list -> Rewrite_system.cond list option

(** The TPDB [(VAR ...)(RULES ...)] text of the unraveled system, with its
    statistics. [Error] on: conditions no greedy schedule can order (a binding
    cycle), a rule whose unraveled rhs has a variable its lhs does not bind (a
    malformed TRS), or two distinct identifiers colliding after the TPDB
    identifier scrub. *)
val trs_of_system : Rewrite_system.t -> (string * stats, string) result
