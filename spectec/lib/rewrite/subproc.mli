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

(** [timed f] runs [f] and returns its result paired with the wall-clock seconds
    it took ([Unix.gettimeofday] delta) -- a sweep records this per symbol. *)
val timed : (unit -> 'a) -> 'a * float

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

(** A persistent child for batched sweeps: [session_start] pays the child's
    startup once, then many [session_send]/[session_read] cycles reuse it (a
    Full Maude load is ~100s, so per-symbol respawn dominates a sweep).
    [session_send] drains stdout while writing, so a feed larger than the pipe
    buffer cannot deadlock against the unread output.

    Both halves take the SAME absolute [deadline] ([Unix.gettimeofday] plus the
    budget), because a unit of work is send-then-read and only the pair of them
    is what a budget is meant to bound. Deadlining the read alone leaves the
    write unbounded: a child that stops draining its stdin while it parses a
    large feed blocks the writer for as long as it likes, and the caller's
    "timeout" then bounds nothing. That is not hypothetical -- a [--timeout 60]
    CRC symbol was measured at 35,895s, and the pre-existing sweep it came from
    already carried 436s rows at the same budget ({!run} is immune because its
    stdin is a temp file, which is exactly why it uses one). *)
type session

val session_start : ?env:string array -> cmd:string list -> unit -> session

(** [session_send s data ~deadline] writes [data] to the live child's stdin,
    draining its output as it goes. Returns [true] if it wrote everything,
    [false] if [deadline] passed first -- a partially-written feed leaves the
    child's input mid-term, so the caller must kill the session rather than read
    from it. [infinity] disables the bound. *)
val session_send : session -> string -> deadline:float -> bool

(** [session_read s ~done_when ~deadline] reads the child's output accumulated
    since the previous read until [done_when] holds over it or [deadline]
    passes, then clears the buffer so the next symbol starts fresh. Returns
    [(output, timed_out)]. *)
val session_read :
  session -> done_when:(string -> bool) -> deadline:float -> string * bool

(** [deadline_in budget] is the absolute time [budget] seconds from now, or
    [infinity] when [budget <= 0] (the "no limit" spelling the CLIs use). *)
val deadline_in : int -> float

(** SIGKILL the child and reap it. *)
val session_kill : session -> unit
