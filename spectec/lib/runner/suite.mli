(** Suite - Test suite infrastructure and batch running. *)

(** {1 Outcome-based runners} *)

type 'i test_result = {
  input : 'i;
  source : string;
  outcome : Spectec.Task.test_outcome;
}

(** Run a single input and compute outcome. Includes full instrumentation
    lifecycle. *)
val run_with_outcome_with_session :
  (module Spectec.Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  'i ->
  Spectec.Task.test_outcome

(** Run a suite of inputs and return individual outcomes. Instrumentation
    lifecycle wraps the entire suite. *)
val run_suite_with_outcomes :
  (module Spectec.Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  ?verbose:bool ->
  'i list ->
  'i test_result list

(** {1 Suite summary} *)

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

(** {1 Presentation} *)

(** Print a test outcome with source and formatted output. *)
val print_outcome :
  (module Spectec.Task.S with type input = 'i) ->
  string ->
  Spectec.Task.test_outcome ->
  unit

(** Print pass/fail summary line. *)
val print_summary : suite_summary -> unit

(** {1 Composed run + print} *)

(** Run a single input with lifecycle and print outcome. *)
val run_and_print_single :
  (module Spectec.Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  'i ->
  unit

(** Run a suite of inputs with lifecycle, print results and summary. *)
val run_and_print_suite :
  (module Spectec.Task.S with type input = 'i) ->
  ?config:Instrumentation.Config.t ->
  sl_mode:bool ->
  spec_il:Lang.Il.spec ->
  verbose:bool ->
  'i list ->
  unit

(** {1 Target batch} *)

type task_result = { task_name : string; summary : suite_summary }

(** Run across multiple tasks with instrumentation and checkpoint support. *)
val run_target_batch :
  ?config:Instrumentation.Config.t ->
  ?test_dir:string ->
  checkpoint_config:Checkpoint.config ->
  verbose:bool ->
  sl_mode:bool ->
  spec_files:string list ->
  Lang.Il.spec ->
  Spectec.Task.packed_task list ->
  task_result list
