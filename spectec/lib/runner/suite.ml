(** Suite - Test suite infrastructure and batch running. *)

open Spectec
open Error

(* --- Outcome-based runners --- *)

type 'i test_result = {
  input : 'i;
  source : string;
  outcome : Task.test_outcome;
}

let run_with_outcome_with_session (type i)
    (module T : Task.S with type input = i)
    ?(config = Instrumentation.Config.default) ~sl_mode ~spec_il (input : i) =
  let result =
    Spectec.eval_task_with_session (module T) ~config ~sl_mode ~spec_il input
  in
  Task.compute_outcome (T.expectation input) result

let run_with_outcome (type i) (module T : Task.S with type input = i) ~sl_mode
    ~spec_il (input : i) =
  let test_case_id = T.source input in
  Instrumentation.Dispatcher.notify_test_start ~test_case_id;
  let result =
    try Spectec.eval_task (module T) ~sl_mode ~spec_il input
    with e ->
      Instrumentation.Dispatcher.notify_test_end ~test_case_id;
      raise e
  in
  Instrumentation.Dispatcher.notify_test_end ~test_case_id;
  Task.compute_outcome (T.expectation input) result

let print_outcome_tag = function
  | Task.Pass _ -> Format.printf "PASS\n%!"
  | Task.ExpectedFail _ -> Format.printf "EXPECTED FAIL\n%!"
  | Task.Fail _ -> Format.printf "FAIL\n%!"
  | Task.UnexpectedPass _ -> Format.printf "UNEXPECTED PASS\n%!"

let run_one_input (type i) (module T : Task.S with type input = i) ~sl_mode
    ~spec_il ~verbose (input : i) =
  let source = T.source input in
  let outcome =
    try run_with_outcome (module T) ~sl_mode ~spec_il input
    with exn ->
      let error = UnhandledException (Printexc.to_string exn) in
      Task.compute_outcome (T.expectation input) (Error error)
  in
  if verbose then print_outcome_tag outcome;
  { input; source; outcome }

(* --- Suite summary --- *)

type suite_summary = {
  pass : int;
  expected_fail : int;
  fail : int;
  unexpected_pass : int;
  total : int;
}

let summary_passed s = s.pass + s.expected_fail
let summary_failed s = s.fail + s.unexpected_pass

let summarize_outcomes results =
  let pass, expected_fail, fail, unexpected_pass =
    List.fold_left
      (fun (p, ef, f, up) { outcome; _ } ->
        match outcome with
        | Task.Pass _ -> (p + 1, ef, f, up)
        | Task.ExpectedFail _ -> (p, ef + 1, f, up)
        | Task.Fail _ -> (p, ef, f + 1, up)
        | Task.UnexpectedPass _ -> (p, ef, f, up + 1))
      (0, 0, 0, 0) results
  in
  { pass; expected_fail; fail; unexpected_pass; total = List.length results }

(* --- Presentation --- *)

let print_outcome (type i) (module T : Task.S with type input = i) source
    outcome =
  match outcome with
  | Task.Pass values ->
      Format.printf "Passed: %s\n  %s\n\n" source (T.format_output values)
  | Task.ExpectedFail err ->
      Format.printf "Expected fail (passed): %s\n  %s\n\n" source
        (Error.string_of_error err)
  | Task.Fail err ->
      Format.printf "Failed: %s\n  %s\n\n" source (Error.string_of_error err)
  | Task.UnexpectedPass values ->
      Format.printf "Unexpected pass (failed): %s\n  %s\n\n" source
        (T.format_output values)

let print_summary summary =
  let passed = summary_passed summary in
  let failed = summary_failed summary in
  Format.printf "\nTest Results: %d/%d passed, %d failed\n" passed summary.total
    failed

(* --- Suite runner --- *)

let run_suite_with_outcomes (type i) (module T : Task.S with type input = i)
    ?(config = Instrumentation.Config.default) ~sl_mode ~spec_il
    ?(verbose = false) (inputs : i list) =
  let total = List.length inputs in
  Instrumentation.with_session config (Instrumentation.Static.IlSpec spec_il)
  @@ fun () ->
  List.mapi
    (fun idx input ->
      if verbose then
        Format.printf "[%d/%d] %s... %!" (idx + 1) total (T.source input);
      run_one_input (module T) ~sl_mode ~spec_il ~verbose input)
    inputs

(* --- Composed run + print --- *)

let run_and_print_single (type i) (module T : Task.S with type input = i)
    ?config ~sl_mode ~spec_il (input : i) =
  let outcome =
    run_with_outcome_with_session (module T) ?config ~sl_mode ~spec_il input
  in
  print_outcome (module T) (T.source input) outcome

let run_and_print_suite (type i) (module T : Task.S with type input = i) ?config
    ~sl_mode ~spec_il ~verbose (inputs : i list) =
  let results =
    run_suite_with_outcomes (module T) ?config ~sl_mode ~spec_il ~verbose inputs
  in
  if not verbose then
    List.iter
      (fun { source; outcome; _ } ->
        Format.printf ">>> Running %s on %s\n" T.name source;
        print_outcome (module T) source outcome)
      results;
  print_summary (summarize_outcomes results)

(* --- Target batch runner --- *)

type task_result = { task_name : string; summary : suite_summary }

let run_target_batch ?(config = Instrumentation.Config.default) ?test_dir
    ~(checkpoint_config : Checkpoint.config) ~verbose ~sl_mode ~spec_files
    spec_il tasks =
  Instrumentation.with_session config (Instrumentation.Static.IlSpec spec_il)
  @@ fun () ->
  let loaded_checkpoint =
    match checkpoint_config.resume_from with
    | Some file -> (
        match Checkpoint.verify_and_load ~file ~spec_files ~verbose with
        | Ok checkpoint -> Some checkpoint
        | Error e ->
            Format.printf "%s\n" (Error.string_of_error e);
            None)
    | None -> None
  in
  let all_completed_inputs = ref [] in
  (match loaded_checkpoint with
  | Some checkpoint ->
      all_completed_inputs := checkpoint.Checkpoint.completed_inputs;
      Checkpoint.restore_coverage checkpoint
  | None -> ());
  let save_current_checkpoint () =
    Checkpoint.save ~spec_files ~completed_inputs:!all_completed_inputs
      ~output_file:checkpoint_config.output_file
  in
  let results =
    List.map
      (fun (Task.Pack (module T)) ->
        let all_inputs =
          match test_dir with
          | Some dir -> T.collect ~dir ()
          | None -> T.collect ()
        in
        let total_all = List.length all_inputs in
        let inputs =
          match loaded_checkpoint with
          | Some checkpoint ->
              Checkpoint.filter_remaining checkpoint all_inputs ~get_id:T.source
          | None -> all_inputs
        in
        let completed_count = total_all - List.length inputs in
        if verbose then
          Format.printf "Running %s (%d tests, %d already completed)...\n%!"
            T.name (List.length inputs) completed_count;
        let task_results =
          List.mapi
            (fun index input ->
              if verbose then
                Format.printf "  [%d/%d] %s... %!"
                  (completed_count + index + 1)
                  total_all (T.source input);
              let result =
                run_one_input (module T) ~sl_mode ~spec_il ~verbose input
              in
              all_completed_inputs := result.source :: !all_completed_inputs;
              if (index + 1) mod checkpoint_config.save_interval = 0 then
                save_current_checkpoint ();
              result)
            inputs
        in
        { task_name = T.name; summary = summarize_outcomes task_results })
      tasks
  in
  save_current_checkpoint ();
  results
