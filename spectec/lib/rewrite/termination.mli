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
  secs : float option;
      (** Wall clock of the ANSWERING run alone, not of the search: the rungs
          below it are excluded, so this is what the verdict cost rather than
          what finding it cost. [None] when no AProVE run happened. *)
}

val string_of_verdict : verdict -> string

(** The budgets [check] tries, ascending, ending at [cap]. The ladder lets a
    symbol AProVE would search to a large deadline stop at a small budget that
    already answers, instead of running to the cap; the budget it stops at is a
    mechanism detail, not reported (AProVE answers before its deadline for most
    symbols, so the answering run's wall clock is the honest measurement).

    [from] (default 5) raises the first rung, skipping the climb through budgets
    a region is already known to exceed -- over slices that all answer near 330s
    the default ladder burns 425s per symbol before reaching the rung that
    answers. The saving costs the ladder's guarantee: a rung set above a
    symbol's real cost is reported as the cost whenever AProVE runs to its
    deadline, so seconds measured with a raised [from] are not comparable to
    default-ladder ones. *)
val budget_ladder : ?from:int -> cap:int -> unit -> int list

(** Whether a rung's verdict ends the climb. Only [Yes] and [No] do: they are
    answers about the TRS. A too-small budget can leave AProVE with no verdict
    line at all, which {!Aprove.check} reports as an [Error] indistinguishable
    from a crash, so [Error] climbs like [Maybe] rather than settling. *)
val decisive : Aprove.verdict -> bool

(** The TRS {!check} proves terminating: the slice with
    {!Crc_surface.rebind_stuck_guards} applied, then {!Unravel.trs_of_system}.

    The rebind is not optional polish. {!Crc_surface.fold_premise_binders}
    trades a dead binder [$g(A) = v] for [isStuckHead($g(A)) = false], a
    faithful restatement only where [isStuckHead] is defined -- the execution
    module defines it, neither analysis surface does. Unraveled, the guard
    leaves a consumer [u(false, ..) -> r] that can never fire, so the rule's
    right-hand side and any recursion in it drop out of the TRS and the proof is
    about a system missing them. Restoring the binder puts them back; the
    condition count, and so the helper count, is unchanged.

    Exposed so [--emit-trs] shows the TRS AProVE is given rather than one
    derived a second way. *)
val trs_of_slice :
  Rewrite_system.t -> (string * Unravel.stats, string) result

(** [check ?aprove_bin ?budget system]: [system] is the already-sliced CTRS
    ({!Rewrite_system.slice}). A slice with no rules is [Degenerate] (nothing to
    prove); an unraveling failure is [Error] with no AProVE run. [aprove_bin] is
    {!Aprove.check}'s; [budget] is the CAP of the budget search, whose last rung
    it is -- so the verdict is the one a single [Aprove.check ~budget] would
    have given, reached for as little time as the proof actually needs.
    [budget_from] is {!budget_ladder}'s [from]: it skips the rungs below it, at
    the cost of that "as little time as needed" guarantee. *)
val check :
  ?aprove_bin:string ->
  ?budget:int ->
  ?budget_from:int ->
  Rewrite_system.t ->
  report
