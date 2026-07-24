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

type report = {
  verdict : verdict;
  stats : Unravel.stats option;
  budget : int option;
      (** The budget the reported verdict came from -- the smallest rung of
          {!budget_ladder} that answered, so for a [Yes] an upper bound on the
          proof's real cost. [None] when no AProVE run happened. *)
  secs : float option;
      (** Wall clock of the ANSWERING run alone, not of the search: the rungs
          below it are excluded, so this is what the verdict cost rather than
          what finding it cost. [None] when no AProVE run happened. *)
}

val string_of_verdict : verdict -> string

(** The budgets [check] tries, ascending, ending at [cap]. AProVE announces at
    its deadline, so a run costs its budget whatever the proof was worth and
    what measures a proof is the smallest budget that still answers. *)
val budget_ladder : cap:int -> int list

(** Whether a rung's verdict ends the climb. Only [Yes] and [No] do: they are
    answers about the TRS. A too-small budget can leave AProVE with no verdict
    line at all, which {!Aprove.check} reports as an [Error] indistinguishable
    from a crash, so [Error] climbs like [Maybe] rather than settling. *)
val decisive : Aprove.verdict -> bool

(** [check ?aprove_bin ?budget system]: [system] is the already-sliced CTRS
    ({!Rewrite_system.slice}). A slice with no rules is [Degenerate] (nothing to
    prove); an unraveling failure is [Error] with no AProVE run. [aprove_bin] is
    {!Aprove.check}'s; [budget] is the CAP of the budget search, whose last rung
    it is -- so the verdict is the one a single [Aprove.check ~budget] would
    have given, reached for as little time as the proof actually needs. *)
val check : ?aprove_bin:string -> ?budget:int -> Rewrite_system.t -> report
