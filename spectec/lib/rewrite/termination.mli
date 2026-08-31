(** Termination of one analysis-CTRS slice, proved by unraveling ({!Unravel})
    plus a direct AProVE run ({!Aprove}) -- deliberately NOT routed through MTT,
    whose condition-variable encoding and hard-coded inner budget block exactly
    the proofs this path completes (the [Do not route termination through MTT]
    decision, CLAUDE.md).

    The slice is unraveled as-is: signature pruning is pointless here (the
    unraveling drops sorts and never reads the signature), and the emitted rules
    are the slice's own, so the chain is [slice -> TRS -> AProVE] with no module
    emission at all. Because the TRS over-approximates the sorted CTRS, [Yes]
    transfers to the original slice; every other verdict is only about the TRS.

    A check takes up to three paths, in order, and stops at the first [Yes]
    ({!path}). They are not settings of one run, and none of their unproven
    verdicts says anything about the others. Nor do their [Yes]es weigh the
    same: the two plain-TRS paths over-approximate the slice, so their [Yes] is
    the slice's, while [Integer_trs] proves a MODEL of it (see {!surface}) and
    its [Yes] is a statement about that model. A row tells the two apart by the
    path named in {!report.steps}, not by the verdict, which is spelled [YES]
    either way. *)

type verdict = Yes | No | Maybe | Timeout | Degenerate | Error of string

(** The proof paths, in the order {!check} tries them.

    - [Whole_trs] -- the whole slice as one plain TPDB TRS. What every
      termination column before 2026-08-20 measured, and still the path that
      answers for most symbols.
    - [Children] -- the open children of that run's own dependency-graph
      decomposition, each given the budget alone. A whole-system run shares one
      deadline across every child, so what its tree leaves open is a mix of
      "hard" and "never reached": on [Program_ok], 48 of the 75 children open
      after 3600s close standalone in a median 37s. Each child is asked as a
      standalone dependency-pair problem ({!Qdp.standalone}) and, where
      [reslice_children] is on and that left it open, a second time as a plain
      TRS re-sliced from the source CTRS ({!reask}). The second ask is OFF by
      default: it costs one more run on every child that does not close, and
      what it buys is unproven -- on the one child measured one variable at a
      time, the [.qdp] and the slice carry the SAME 65 rules and both TIMEOUT,
      and the 4s YES belongs to the slice WITHOUT the projection pair, which
      closes nothing (2026-08-21). Reports [Yes] only when every open child
      closed, which by the soundness of AProVE's [AND] node is the proof the
      whole run was after.
    - [Integer_trs] -- the whole slice on the integer surface ({!Itrs}), tried
      only where the slice actually computes arithmetic. It is what reaches the
      residue the other two cannot: a descent that lives inside the structural
      binary-digit layer is a linear fact here and a black box there. *)
type path = Whole_trs | Children | Integer_trs

(** What one path came back with. Kept per path rather than collapsed, because a
    failed split's own numbers ("48/75 closed, node 911 TIMEOUT") are the
    diagnostic the verdict alone does not carry. *)
type step = { path : path; verdict : verdict; secs : float; note : string }

type report = {
  verdict : verdict;
      (** [Yes] if any path answered [Yes]; otherwise the whole-system run's own
          verdict, since no other path refutes anything. Which path it was is in
          {!steps}, and it matters -- an [Integer_trs] [Yes] is about a model of
          the slice rather than the slice ({!surface}). *)
  stats : Unravel.stats option;
  secs : float option;
      (** Wall clock of the ANSWERING path: for a budget-ladder run the
          answering rung alone (the rungs below it are excluded, so this is what
          the verdict cost rather than what finding it cost), for a split every
          child run it made. [None] when no AProVE run happened. *)
  proof : string option;
      (** {!Aprove.proof_tree} of the WHOLE-SYSTEM run -- the outline of the
          steps it took, which is what says how far an unproven symbol got, and
          which decomposition a split then worked on. Without it a [Timeout] row
          is a single word, and a symbol AProVE abandoned while still converting
          the system is indistinguishable from one it ranked all but the last
          dependency-pair problem of. (The integer path replaces it with its own
          tree when that is what answered; a split does not, since its answer is
          one tree per child and the whole run's is what names them.) *)
  steps : step list;  (** Every path taken, in order. *)
}

val string_of_verdict : verdict -> string
val string_of_path : path -> string

(** One step as a row cell: [children:YES(all children closed)@412.0s]. *)
val string_of_step : step -> string

(** The budgets {!check} tries, ascending, ending at [cap]. The ladder lets a
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
    {!Unravel.trs_of_system}. Exposed so [--emit-trs] shows the TRS AProVE is
    given rather than one derived a second way. *)
val trs_of_slice : Rewrite_system.t -> (string * Unravel.stats, string) result

(** Which export a proof runs on. [Plain] is {!trs_of_slice}'s TPDB TRS;
    [Integer] is {!Itrs.of_system}, the same unraveling with the structural
    arithmetic layer handed to AProVE's native integers and constraints.

    These are two SURFACES, not two settings of one, and they do not carry the
    same weight. [Plain] only ever ADDS to the slice -- it drops the sorts,
    drops [owise], flattens what a chain step carries -- so it over-approximates
    and its [Yes] is the slice's. [Integer] is a MODEL: {!Itrs.of_system} begins
    by DELETING the rules of the arithmetic layer and substituting hand-written
    integer definitions, which is the direction that makes a proof easier rather
    than harder, so its [Yes] is a statement about the model and does not
    transfer to the slice on its own. The codec ([$bitstr_to_int] /
    [$int_to_bitstr]) is a step weaker again: there the substitute is not the
    same algorithm but a closed formula in place of a correction loop.

    Neither surface's MAYBE says anything about the other either, so a column
    measured on one must not be compared row-wise against a column measured on
    the other. In a row, an [Integer] answer is told apart by the [itrs:] entry
    in the paths column, not by the verdict, which is spelled the same. *)
type surface = Plain | Integer

(** The export a surface uses, and the temp-file extension it must be handed to
    AProVE under (which is how AProVE selects the problem type -- ITRS text in a
    [.trs] file is a parse error, not an ITRS run). *)
val export :
  roots:string list ->
  surface ->
  Rewrite_system.t ->
  (string * Unravel.stats, string) result

val extension : surface -> string

(** One open child asked the OTHER way: instead of re-posing it as a
    dependency-pair problem ({!Qdp.standalone}), the source CTRS re-sliced at
    the symbols its pairs name and unraveled into a plain TRS, which AProVE then
    proves terminating from scratch -- the question [termination --symbol] asks,
    aimed at the child.

    It is worth asking because the [.qdp] FORM can be what is hard:
    [$cast_enum]'s node 140 ([pow_int, pow_int_aux], 2 pairs / 65 rules) is
    TIMEOUT at 900s as a [.qdp] and YES in 3.9s as a slice, while its node 142
    is TIMEOUT both ways -- an alternative, not a law, which is why it runs
    behind the [.qdp] rather than in front of it.

    [Error] when {!Qdp.closed_by} refuses the two inclusions that license
    answering the child this way, or when the slice turns out to be the whole
    system (nothing to gain, and it is the run that already failed). A refusal
    is a fallback, not a defect.

    The rules are SELECTED from [export] rather than unraveled again from the
    slice: {!Unravel}'s chain helpers are numbered over the whole rule list, so
    a separate unraveling would spell them differently and the child's pairs
    would match nothing. [export] must therefore be {!Unravel.export_of_system}
    of [system]'s {!proof_surface}. *)
type reask = {
  licensed : string;
      (** the slice plus {!Qdp.reposed}'s projection pair -- the submission the
          licence check passed, and the only one whose [Yes] closes the child *)
  bare : string;
      (** the same slice WITHOUT the projection pair: not licensed, never used
          to close anything, and present only so [--child-report] can measure
          what the pair costs. Adding it is the whole difference between this
          path and the plain [termination --symbol] the idea was measured on,
          and C-epsilon is Toyama's counterexample -- it can turn a terminating
          system non-terminating, so "it is only two rules" is not an argument
          available here. *)
  rules : int;
  bare_rules : int;
}

val reask :
  export:Unravel.export ->
  system:Rewrite_system.t ->
  Qdp.problem ->
  (reask, string) result

(** What one AProVE run on one child came back with. *)
type attempt = { verdict : verdict; secs : float; rules : int; proof : string }

(** One open child of the whole-system run, measured on its own: the recursion
    it is around ([heads], the symbols its dependency pairs run through -- the
    export's spelling, which {!To_ctrs.symbol_origins} turns into the
    declaration and its source line), how many dependency pairs it has, and what
    each way of asking it made of it.

    - [qdp] -- the child re-posed as a standalone dependency-pair problem
      ({!Qdp.standalone}), which is what {!check}'s split tries first.
    - [slice] -- the source CTRS re-sliced at the symbols the pairs name and
      proved as a plain TRS from scratch, plus the projection pair the
      usable-rules prune needs. [Error] when the two inclusions that license
      this ({!Qdp.closed_by}) do not hold, which is a fallback rather than a
      defect.
    - [slice_bare] -- the same slice WITHOUT the projection pair. Measured,
      never believed: it is not licensed to close anything, and it is here only
      because adding the pair is what separates this path from the plain
      [termination --symbol] the idea was measured on, and that difference
      should be a number. *)
type child = {
  node : int;
  heads : string list;
  pairs : int;
  qdp : attempt;
  slice : (attempt, string) result;
  slice_bare : (attempt, string) result;
}

(** Every open child of an unproven whole-system run, each given the budget to
    itself -- the measurement behind {!check}'s [Children] path, which answers
    with a verdict and therefore stops at the first child that does not close.
    Here none is skipped, so the result is the distribution: which children
    close, which recursions the rest are around, and what each cost.

    [budget] runs the whole-system capture, [child_budget] (default [budget])
    each of the up-to-three runs per child. [on_child] is called as each row
    lands, since a run over a large slice takes an hour and a sweep wants its
    rows before the end. [Error] when the run gave no children to measure -- the
    same conditions under which {!check}'s split reports [Unavailable]. *)
val child_report :
  ?aprove_bin:string ->
  ?budget:int ->
  ?child_budget:int ->
  ?on_child:(child -> unit) ->
  Rewrite_system.t ->
  (child list, string) result

(** [check ?aprove_bin ?budget system]: [system] is the already-sliced CTRS
    ({!Rewrite_system.slice}). A slice with no rules is [Degenerate] (nothing to
    prove); an export failure is [Error] with no AProVE run. [aprove_bin] is
    {!Aprove.check}'s; [budget] is the CAP of the budget search, whose last rung
    it is -- so a path's verdict is the one a single [Aprove.check ~budget]
    would have given, reached for as little time as the proof actually needs.
    [budget_from] is {!budget_ladder}'s [from]: it skips the rungs below it, at
    the cost of that "as little time as needed" guarantee.

    [split_children] (default true) and [integer_retry] (default true) enable
    the two fallback paths, and [reslice_children] (default FALSE) the second
    way of asking a child inside the split ({!reask}). All three run only where
    the plain whole-system run left the slice unproven -- and [integer_retry]
    only where the slice computes arithmetic at all ({!Itrs.has_arithmetic}),
    since without it the integer surface re-proves the same system under another
    name. Both cost real time on a symbol that stays unproven: the split pays
    one more whole-system run (to capture the decomposition), one run per open
    child, and a second run on the child that does not close (the slice re-ask),
    and the integer retry another ladder.

    [surface] (default [Plain]) pins the check to ONE surface and takes the
    fallbacks away with it -- [~surface:Integer] is the integer-only column, not
    an integer-first pipeline.

    [roots] are the heads [system] was sliced from. Only the integer surface
    reads them, and only because it DELETES the arithmetic layer: see
    {!Itrs.of_system} for why a root inside that layer has to be named to
    survive its own replacement. *)
val check :
  ?aprove_bin:string ->
  ?budget:int ->
  ?budget_from:int ->
  ?split_children:bool ->
  ?reslice_children:bool ->
  ?integer_retry:bool ->
  ?surface:surface ->
  roots:string list ->
  Rewrite_system.t ->
  report
