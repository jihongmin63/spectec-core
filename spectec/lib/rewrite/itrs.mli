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

    Same soundness direction as the plain TRS: sorts are dropped, so this
    over-approximates the sorted CTRS and a YES transfers to it while a NO does
    not (it may be an artifact of untyped variables ranging over values the
    sorts exclude).

    [Error] on everything {!Unravel.trs_of_system} errors on, plus an OPEN
    scalar magnitude -- a rule matching [int_pos(x)] or building [bd0(p)] on a
    variable, which has no integer value to fold to. That is deliberately an
    error and not a silent pass-through: the constructor would survive as an
    uninterpreted symbol disconnected from the arithmetic that defined it, and
    AProVE would prove termination of a different system. *)
val of_system : Rewrite_system.t -> (string * Unravel.stats, string) result

(** Whether the slice names any operation of the layer this surface replaces --
    the arithmetic interface and its binary engine, the binary-indexed list
    operations, the two's-complement codec. It is the question "would the
    integer surface be a different system at all": a slice that computes no
    arithmetic exports here as (almost) the same rules the plain TRS does, so
    running both is two runs of one problem. *)
val has_arithmetic : Rewrite_system.t -> bool
