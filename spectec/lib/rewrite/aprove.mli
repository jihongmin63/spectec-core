(** Submit a plain TPDB TRS to AProVE (WST mode, Z3 backend) through the bundled
    [tools/aprove/runme] wrapper and parse its termination verdict.

    The local-process counterpart of {!Mfe}: the tool is large and not checked
    in, so a missing runner is a clean [Error], not a crash. *)

type verdict = Yes | No | Maybe | Timeout | Error of string

val string_of_verdict : verdict -> string

(** Where the runner is resolved from: the argument, then [SPECTEC_APROVE_BIN],
    then [spectec/tools/aprove/runme]. *)
val resolve_bin : string option -> string

(** Whether AProVE has printed its answer, over the output accumulated so far --
    the run's stop condition, so a run ends at the verdict rather than at JVM
    exit. Only a newline-terminated line counts, since the buffer this is polled
    over ends mid-read.

    This trims the shutdown tail, not the wait: AProVE announces at its own
    deadline, so a symbol it cannot answer instantly costs [budget] whatever
    this predicate does. Read a sweep's per-symbol seconds accordingly. *)
val verdict_printed : string -> bool

(** [check ?aprove_bin ?budget ~trs ()] writes [trs] to a temp file and runs
    [runme <file> <budget>]. [budget] (default 300) is AProVE's own proof budget
    in seconds; the process is additionally killed [120]s past it (the JVM needs
    real slack over the in-tool budget), and is killed as soon as
    {!verdict_printed} holds. The verdict is the first output line that is
    exactly [YES]/[NO]/[MAYBE] -- the final parse also accepts it on the
    trailing partial line, so a deadline kill mid-line still yields the answer
    it had already given; a deadline kill without one is [Timeout]. *)
val check : ?aprove_bin:string -> ?budget:int -> trs:string -> unit -> verdict
