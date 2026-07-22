(** Submit a plain TPDB TRS to AProVE (WST mode, Z3 backend) through the bundled
    [tools/aprove/runme] wrapper and parse its termination verdict.

    The local-process counterpart of {!Mfe}: the tool is large and not checked
    in, so a missing runner is a clean [Error], not a crash. *)

type verdict = Yes | No | Maybe | Timeout | Error of string

val string_of_verdict : verdict -> string

(** Where the runner is resolved from: the argument, then [SPECTEC_APROVE_BIN],
    then [spectec/tools/aprove/runme]. *)
val resolve_bin : string option -> string

(** [check ?aprove_bin ?budget ~trs ()] writes [trs] to a temp file and runs
    [runme <file> <budget>]. [budget] (default 300) is AProVE's own proof budget
    in seconds; the process is additionally killed [120]s past it (the JVM needs
    real slack over the in-tool budget). The verdict is the first output line
    that is exactly [YES]/[NO]/[MAYBE]; a deadline kill without one is
    [Timeout]. *)
val check : ?aprove_bin:string -> ?budget:int -> trs:string -> unit -> verdict
