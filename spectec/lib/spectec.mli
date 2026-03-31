(** Spectec - Entrypoint API facade.

    Provides the core pipeline (parse, elaborate, structure), a unified
    interpreter entry point, and the core type modules (Error, Task, Target). *)

module Error = Error
module Task = Task
module Target = Target

type 'a result = ('a, Error.t) Stdlib.result

(** {1 Pipeline transformations} *)

(** Collect [.spectec] files from a directory, sorted. *)
val collect_spec_files : string -> string list

val parse_spec_files : string list -> Lang.El.spec result
val elaborate : Lang.El.spec -> Lang.Il.spec result
val structure : Lang.Il.spec -> Lang.Sl.spec

(** Validate instrumentation config against the current mode. *)
val validate_config : Instrumentation.Config.t -> sl_mode:bool -> unit result

(** {1 Unified interpreter entry point}

    De-duplicates IL/SL dispatch: parses input via task, sets up the target
    handler, wraps with instrumentation session, runs the appropriate
    interpreter. *)

val eval_task_with_session :
  (module Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  'i ->
  Lang.Il.Value.t list result
