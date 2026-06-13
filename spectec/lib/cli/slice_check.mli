(** Shared driver for the commands that translate a spec to its rewriting system
    and check it: confluence (CoCoWeb) and the combined confluence + termination
    check. The per-system check and how its result renders vary by command and
    stay at the call site; the pipeline shared between them lives here. *)

(** What {!run} produces before rendering. ['r] is the caller's per-system check
    result (a confluence verdict, or a confluence/termination pair). *)
type 'r outcome =
  | Listing of string list  (** [--list-symbols]: the slice roots. *)
  | Unknown of string * string list  (** [--symbol] naming an unknown root. *)
  | Single of 'r  (** one [--symbol] slice, or the [--whole] system. *)
  | Batch of (string * 'r) list  (** every root's slice (the default). *)

(** [run ~check_system ~slice filenames] parses and elaborates [filenames],
    lists the slice roots, and dispatches according to [slice]: a single check
    (one [--symbol] or [--whole]) or a batch over every root. [check_system]
    runs the actual tool(s) on one (possibly sliced) system. *)
val run :
  check_system:(Spectec.Rewrite_system.t -> 'r) ->
  slice:Cli_args.Slice.t ->
  string list ->
  ('r outcome, Spectec.Error.t) result

(** [handle ~single ~batch outcome] renders [outcome]. [Listing] and [Unknown]
    render uniformly here ([Unknown] exits 2); [single] and [batch] are the
    caller's renderers for the verdict-bearing cases. *)
val handle :
  single:('r -> unit) ->
  batch:((string * 'r) list -> unit) ->
  'r outcome ->
  unit
