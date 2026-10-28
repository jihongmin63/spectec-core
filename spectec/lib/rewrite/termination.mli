(** Termination of one analysis-CTRS slice through a plain unraveled TRS and
    AProVE. A whole-run proof or a sound split that closes every open
    dependency-graph child proves the original slice. Itrs remains an
    independent model exporter and is not a termination fallback. *)

type verdict = Yes | No | Maybe | Timeout | Degenerate | Error of string

(** Whole plain TRS, followed when unproven by its sound child split. *)
type path = Whole_trs | Children

(** One budget-ladder attempt. *)
type rung = {
  budget : int;
  verdict : verdict;
  secs : float;
      (** Time until the proof tree was available, preserving the historical
          path timing. *)
  total_secs : float;
      (** Full rung wall clock, including obligation capture when requested. *)
}

(** What one path came back with. Kept per path rather than collapsed, because a
    failed split's own numbers ("48/75 closed, node 911 TIMEOUT") are the
    diagnostic the verdict alone does not carry. *)
type step = {
  path : path;
  verdict : verdict;
  secs : float;
      (** The final or answering rung, preserving the historical TSV meaning. *)
  total_secs : float;
      (** Every rung and direct subprocess spent on this path. *)
  rungs : rung list;
      (** Ordered ladder attempts; empty for non-ladder paths such as a child
          split. *)
  note : string;
}

type report = {
  verdict : verdict;
      (** [Yes] if any path answered [Yes]; otherwise the whole-system run's own
          verdict, since no other path refutes anything. Which path it was is in
          {!steps}. *)
  stats : Unravel.stats option;
  secs : float option;
      (** Wall clock of the ANSWERING path: for a budget-ladder run the
          answering rung alone (the rungs below it are excluded, so this is what
          the verdict cost rather than what finding it cost), for a split every
          child run it made. [None] when no AProVE run happened. *)
  total_secs : float option;
      (** Sum of [step.total_secs] over every attempted path. This is the AProVE
          search wall omitted by the historical answering-path [secs]. *)
  proof : string option;
      (** The whole-run AProVE proof tree, also used to identify open children.
      *)
  steps : step list;  (** Every path taken, in order. *)
}

val string_of_verdict : verdict -> string
val string_of_path : path -> string

(** One step as a row cell: [children:YES(all children closed)@412.0s]. A ladder
    path appends its total and attempts when it climbed multiple rungs or when
    obligation capture made a single rung's total visibly exceed its proof-tree
    time, without changing the five-column TSV shape. *)
val string_of_step : step -> string

(** Build one ladder step from its ordered, non-empty attempts. Exposed so the
    accounting and stable row spelling can be tested without running AProVE. *)
val step_of_rungs : path -> rung list -> string -> step

(** The budgets {!check} tries, ascending, ending at [cap]. The ladder lets a
    symbol AProVE would search to a large deadline stop at a small budget that
    already answers, instead of running to the cap; the budget it stops at is a
    mechanism detail, not reported (AProVE answers before its deadline for most
    symbols, so the answering run's wall clock is the honest measurement).

    The first rung is fixed at 5. *)
val budget_ladder : cap:int -> unit -> int list

(** Whether a rung's verdict ends the climb. Only [Yes] and [No] do: they are
    answers about the TRS. A too-small budget can leave AProVE with no verdict
    line at all, which {!Aprove.check} reports as an [Error] indistinguishable
    from a crash, so [Error] climbs like [Maybe] rather than settling. *)
val decisive : Aprove.verdict -> bool

(** The system every export is taken from: the slice with
    {!Crc_surface.rebind_stuck_guards} applied.

    The rebind is not optional polish. {!Crc_surface.fold_premise_binders}
    trades a dead binder [$g(A) = v] for [isStuckHead($g(A)) = false], a
    faithful restatement only where [isStuckHead] is defined -- the execution
    module defines it, neither analysis surface does. Unraveled, the guard
    leaves a consumer [u(false, ..) -> r] that can never fire, so the rule's
    right-hand side and any recursion in it drop out of the exported system and
    the proof is about a system missing them. Restoring the binder puts them
    back; the condition count, and so the helper count, is unchanged. *)
val proof_surface : Rewrite_system.t -> Rewrite_system.t

(** The TRS {!check} proves terminating: {!proof_surface}, then
    {!Unravel.trs_of_system}. Exposed so [--emit] shows the TRS AProVE is given
    rather than one derived a second way. *)
val trs_of_slice : Rewrite_system.t -> (string * Unravel.stats, string) result

(** Check an already-sliced CTRS. The budget ladder starts at 5 and ends at
    [budget]. Unproven whole runs retry every open dependency-graph child; [Yes]
    is returned only if all children close. *)
val check : ?aprove_bin:string -> ?budget:int -> Rewrite_system.t -> report
