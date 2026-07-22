(** Termination of one analysis-CTRS slice, proved by structure-preserving
    unraveling ({!Unravel}) plus a direct AProVE run ({!Aprove}) -- deliberately
    NOT routed through MTT, whose condition-variable unraveling and hard-coded
    inner budget block exactly the proofs this path completes (the
    [Do not route termination through MTT] decision, CLAUDE.md).

    The slice is unraveled as-is: signature pruning is pointless here (the
    unraveling drops sorts and never reads the signature), and the emitted rules
    are the slice's own, so the chain is [slice -> TRS -> AProVE] with no module
    emission at all. Because the TRS over-approximates the sorted CTRS, [Yes]
    transfers to the original slice; every other verdict is only about the TRS.
*)

type verdict = Yes | No | Maybe | Timeout | Degenerate | Error of string
type report = { verdict : verdict; stats : Unravel.stats option }

val string_of_verdict : verdict -> string

(** [check ?aprove_bin ?budget system]: [system] is the already-sliced CTRS
    ({!Rewrite_system.slice}). A slice with no rules is [Degenerate] (nothing to
    prove); an unraveling failure is [Error] with no AProVE run. [budget] and
    [aprove_bin] are {!Aprove.check}'s. *)
val check : ?aprove_bin:string -> ?budget:int -> Rewrite_system.t -> report
