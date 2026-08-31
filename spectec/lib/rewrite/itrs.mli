(** The ITRS (integer-constraint TRS) export of a sliced analysis CTRS: the
    unraveling {!Unravel} produces, with the structural arithmetic layer
    replaced by AProVE's native integers and [:|:] constraints.

    The CTRS computes on Coq-[positive]-style binary digit trees, so a proof
    over the plain TRS must re-derive "subtracting one decreases" from the digit
    recursion, and a caller's descent measure sits inside that black box. Here
    the whole layer -- interface operations, the binary engine under them, the
    list operations that peel a binary INDEX apart, and the two's-complement
    codec -- is dropped and re-given as integer rules, so the same descent is a
    linear fact AProVE's solver already has.

    A condition that merely TESTS an arithmetic comparison is lifted back into a
    [:|:] constraint rather than left for the unraveling to turn into a chain
    step. Matched as the opaque result of a defined symbol, the fact it stands
    for is invisible to the constraint solver, which is fatal in a counting-up
    recursion: the measure is [n_len - n], so with [n < n_len] hidden the cycle
    has no decreasing argument at all. Exact rather than approximating -- the
    rule fires on precisely the instances it fired on before.

    A YES here does NOT mean what a plain-TRS YES means. Dropping the sorts is
    an over-approximation, the safe direction the plain TRS also has, but it is
    the smaller half of what this export does: it DELETES the rules of the
    replaced layer and substitutes integer definitions for them, and deleting
    rules makes a termination proof easier rather than harder. So this is a
    MODEL of the slice, not an over-approximation of it, and a YES is a
    statement about the model -- the slice terminates provided the arithmetic
    behaves as these integer rules say. Weaker again for the codec, where the
    substitute is a different algorithm (a closed formula in place of the spec's
    +/-2^w correction loop), so a YES says nothing about that loop. A NO
    transfers no better, and additionally may be an artifact of untyped
    variables ranging over values the sorts exclude.

    [Error] on everything {!Unravel.trs_of_system} errors on, plus an OPEN
    scalar magnitude -- a rule matching [int_pos(x)] or building [bd0(p)] on a
    variable, which has no integer value to fold to. That is deliberately an
    error and not a silent pass-through: the constructor would survive as an
    uninterpreted symbol disconnected from the arithmetic that defined it, and
    AProVE would prove termination of a different system.

    [roots] are the heads the slice was taken from. They matter because the
    replaced layer is DELETED here: a root that is itself one of the replaced
    operations keeps none of its own rules, and without being named as a root it
    would be pruned along with them -- leaving an export that does not contain
    the symbol the proof is supposed to be about. Named, it gets its integer
    definition back (or, for a b-layer operation, the retargeting {!blayer}
    applies at call sites written out as a rule), and a root that has no integer
    definition at all is an [Error] rather than a vacuous [YES]. *)
val of_system :
  roots:string list ->
  Rewrite_system.t ->
  (string * Unravel.stats, string) result

(** Whether the slice names any operation of the layer this surface replaces --
    the arithmetic interface and its binary engine, the binary-indexed list
    operations, the two's-complement codec. It is the question "would the
    integer surface be a different system at all": a slice that computes no
    arithmetic exports here as (almost) the same rules the plain TRS does, so
    running both is two runs of one problem. *)
val has_arithmetic : Rewrite_system.t -> bool
