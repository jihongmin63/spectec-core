(** Runner - Pipeline orchestration and test infrastructure. *)

module Error = Error
module Task = Task
module Target = Target
module Checkpoint = Checkpoint

type 'a result = ('a, Error.t) Stdlib.result

(** {1 Pipeline transformations} *)

val parse_spec_files : string list -> Lang.El.spec result
val elaborate : Lang.El.spec -> Lang.Il.spec result
val structure : Lang.Il.spec -> Lang.Sl.spec

(** {1 Single-run interpreters with lifecycle}

    These manage the full instrumentation init/finish lifecycle. Use for
    individual runs outside of batch/coverage contexts. *)

val eval_il :
  (module Target.S) ->
  ?config:Instrumentation.Config.t ->
  Lang.Il.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (Interp.Eval_Il.Ctx.t * Lang.Il.Value.t list) result

val eval_sl :
  (module Target.S) ->
  ?config:Instrumentation.Config.t ->
  Lang.Sl.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (Interp.Eval_Sl.Ctx.t * Lang.Il.Value.t list) result

(** {1 Task-level interpreters with lifecycle}

    Parse input via task, then run the appropriate interpreter. Manage full
    instrumentation lifecycle. *)

val eval_il_with_task :
  (module Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  Lang.Il.spec ->
  'i ->
  (Interp.Eval_Il.Ctx.t * Lang.Il.Value.t list) result

val eval_sl_with_task :
  (module Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  Lang.Il.spec ->
  Lang.Sl.spec ->
  'i ->
  (Interp.Eval_Sl.Ctx.t * Lang.Il.Value.t list) result

(** {1 Outcome-based runners} *)

(** Result for a single test in a suite. *)
type 'i test_result = {
  input : 'i;
  source : string;
  outcome : Task.test_outcome;
}

(** Run a single input and compute outcome based on expectation. Includes full
    instrumentation lifecycle. *)
val run_with_outcome :
  (module Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  'i ->
  Task.test_outcome

(** Run a suite of inputs and return individual outcomes. Instrumentation
    lifecycle wraps the entire suite. *)
val run_suite_with_outcomes :
  (module Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  ?verbose:bool ->
  'i list ->
  'i test_result list

(** {1 Suite summary} *)

(** Summary statistics from suite results. *)
type suite_summary = {
  pass : int;
  expected_fail : int;
  fail : int;
  unexpected_pass : int;
  total : int;
}

val summarize_outcomes : 'i test_result list -> suite_summary
val summary_passed : suite_summary -> int
val summary_failed : suite_summary -> int

(** {1 Coverage} *)

(** Result for one task in a coverage run. *)
type task_result = { task_name : string; summary : suite_summary }

(** Run coverage across all tasks in a target with checkpoint support.
    Instrumentation lifecycle wraps the entire coverage run. *)
val run_target_batch :
  ?config:Instrumentation.Config.t ->
  ?test_dir:string ->
  checkpoint_config:Checkpoint.config ->
  verbose:bool ->
  sl_mode:bool ->
  spec_files:string list ->
  Lang.Il.spec ->
  Task.packed_task list ->
  task_result list
