(** Run a local analysis tool under a wall-clock deadline and return its merged
    stdout+stderr.

    The runner encodes the process discipline every Maude/AProVE bridge in this
    library needs: stdin is fed from a temp file (a pipe would deadlock a feed
    larger than the pipe buffer against the unread stdout), the child is
    launched through [/bin/sh] with the stack limit lifted (legitimately-deep
    critical-pair/tree-automaton/reduction work otherwise dies as a native
    "Fatal error: stack overflow" and reads as a missing verdict), and output
    already produced is returned even when the deadline kills the process. *)

(** [run ?env ?done_when ~cmd ~feed ~timeout ()] executes [cmd] (an argv whose
    head is the program path) with [feed] on stdin and returns
    [(output, timed_out)]. [timeout] is the whole-run deadline in seconds (0
    disables it); [timed_out] reports that the deadline (not a finished run)
    stopped the read. [done_when] is polled over the accumulated output: a tool
    with no clean exit (the MFE floods an incomplete-input prompt forever) is
    killed as soon as its output satisfies the predicate; without it the run
    reads to EOF. [env] replaces the child's environment (default: the
    parent's). The child is always SIGKILLed on the way out, so a verdict
    already printed is parsed even though the process never exits cleanly. *)
val run :
  ?env:string array ->
  ?done_when:(string -> bool) ->
  cmd:string list ->
  feed:string ->
  timeout:int ->
  unit ->
  string * bool

(** Whether [sub] occurs in [s] -- the primitive the verdict scanners build on.
*)
val contains : string -> string -> bool

(** Collapse every whitespace run to a single space. The Maude tools wrap result
    lines at the terminal width, splitting a verdict phrase across lines;
    matching happens on the collapsed form. *)
val normalize_ws : string -> string

(** Make a (possibly relative) path absolute against the current directory, so a
    path handed to a child tool (a [load] line, an exported library dir)
    resolves regardless of the child's working directory. *)
val absolute : string -> string
