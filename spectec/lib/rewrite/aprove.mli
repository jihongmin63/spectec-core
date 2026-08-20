(** Submit a plain TPDB TRS to AProVE (WST mode, Z3 backend) through the bundled
    [tools/aprove/runme] wrapper and parse its termination verdict.

    The local-process counterpart of {!Mfe}: the tool is large and not checked
    in, so a missing runner is a clean [Error], not a crash. *)

type verdict = Yes | No | Maybe | Timeout | Error of string

type run = {
  verdict : verdict;
  proof : string;
      (** {!proof_tree} of this run's output -- empty when AProVE printed no
          proof echo (a missing binary, a parse error, a killed JVM). *)
  captured : string;
      (** The lines a [capture] run's filter kept, in output order; empty for an
          ordinary run, which never reads past the proof tree. *)
}

val string_of_verdict : verdict -> string

(** Where the runner is resolved from: the argument, then [SPECTEC_APROVE_BIN],
    then [spectec/tools/aprove/runme]. *)
val resolve_bin : string option -> string

(** The proof TREE AProVE echoes under its verdict: the outline of the steps the
    run took ([(0) ITRS], [(3) UsableRulesProof [EQUIVALENT, 0 ms]], [(9) YES]),
    down to the first obligation separator, which is where the echo stops
    outlining and starts reprinting each obligation in full.

    This is what a verdict alone does not say -- HOW FAR the proof got. A run
    that ends at its deadline reports the same [Timeout] whether it was still
    converting the system or was ranking the last of its dependency-pair
    problems, and the tree tells those apart: on the 4.8MB [Program_ok] ITRS it
    reads "(0) ITRS", meaning no ranking function was ever searched for.

    The obligations behind the tree are dropped deliberately, not for size
    alone: they are largely the exported system echoed back (6.6MB for that same
    run), and they keep streaming for minutes after the verdict, so waiting for
    them would inflate every wall clock measured around {!check}. Re-run [runme]
    on the export by hand when the full text is wanted. *)
val proof_tree : string -> string

(** Whether AProVE has printed its answer AND closed the proof tree under it,
    over the output accumulated so far -- the run's stop condition, so a run
    ends at the tree rather than at JVM exit. Only newline-terminated lines
    count, since the buffer this is polled over ends mid-read.

    This trims the shutdown tail, not the wait: AProVE announces at its own
    deadline, so a symbol it cannot answer instantly costs [budget] whatever
    this predicate does. Read a sweep's per-symbol seconds accordingly. *)
val proof_printed : string -> bool

(** What a run that reads past the proof tree keeps, and how much longer it is
    allowed to take. The obligation echo behind the tree reprints the whole rule
    set once per dependency-graph child -- gigabytes on a large slice, still
    arriving minutes after the verdict -- so a caller after the obligations must
    say both which lines are worth holding ([keep], called once per output line
    in order, so it may be a stateful section filter) and how much time past
    AProVE's own deadline the printing may take ([echo_slack] seconds). A run
    whose echo is cut short by that deadline keeps what it read: the caller sees
    a partial capture, not an error, and is the one that can tell whether what
    it needed arrived. *)
type capture = { keep : string -> bool; echo_slack : int }

(** [check ?aprove_bin ?budget ~trs ()] writes [trs] to a temp file and runs
    [runme <file> <budget>]. [budget] (default 300) is AProVE's own proof budget
    in seconds; the process is additionally killed [120]s past it (the JVM needs
    real slack over the in-tool budget), and is killed as soon as
    {!proof_printed} holds. The verdict is the first output line before the
    proof echo that is exactly [YES]/[NO]/[MAYBE] -- scanning further would read
    a closed sub-problem's own [YES] as the run's answer; a deadline kill
    without a verdict line is [Timeout].

    [ext] is the temp file's extension and therefore the PROBLEM TYPE AProVE
    reads the text as -- [".trs"] (the default) for plain TPDB, [".itrs"] for an
    integer-constraint system. Content alone does not select it: ITRS text in a
    [.trs] file is a parse error.

    [capture] turns the run into one that reads to the END of the output rather
    than stopping at the tree, filling {!run.captured} with the lines its filter
    kept. That is how the obligations behind the tree are reached, and it costs
    the echo's own minutes -- so pass it only when those lines are the point of
    the run ({!Qdp}). *)
val check :
  ?aprove_bin:string ->
  ?budget:int ->
  ?ext:string ->
  ?capture:capture ->
  trs:string ->
  unit ->
  run
