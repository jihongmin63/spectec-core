(** Spectec - Entrypoint API facade.

    Provides the core pipeline (parse, elaborate, structure), a unified
    interpreter entry point, and the core type modules (Error, Task, Target). *)

module Error = Error
module Task = Task
module Target = Target

type 'a result = ('a, Error.t) Stdlib.result

(** {1 Pipeline transformations} *)

val parse_spec_files : string list -> Lang.El.spec result
val elaborate : Lang.El.spec -> Lang.Il.spec result
val structure : Lang.Il.spec -> Lang.Sl.spec

(** {1 Unified interpreter entry point}

    De-duplicates IL/SL dispatch: parses input via task, wraps with
    instrumentation session, runs the appropriate interpreter. Does NOT include
    the target handler — caller wraps with [T.Target.handler] if needed. *)

val eval_task_with_session :
  (module Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  'i ->
  Lang.Il.Value.t list result
