(** Suite - Test suite infrastructure and batch running. *)

open Spectec
open Error

let ( let* ) = Result.bind

(* --- Private eval helpers --- *)

let eval_il (module T : Interp.Target.S) spec_il rid values_input
    filename_target =
  Interp.eval_il (module T) spec_il rid values_input filename_target
  |> Result.map_error (fun e -> InterpError e)

let eval_sl (module T : Interp.Target.S) spec_sl rid values_input
    filename_target =
  Interp.eval_sl (module T) spec_sl rid values_input filename_target
  |> Result.map_error (fun e -> InterpError e)

(* De-duplicated IL/SL dispatch — no session, no handler *)
let eval_task (type i) (module T : Task.S with type input = i) ~sl_mode ~spec_il
    (input : i) =
  let* relation, values = T.parse_input ~spec:spec_il input in
  if sl_mode then
    let spec_sl = Pass.structure spec_il in
    let* _, vals =
      eval_sl (module T.Target) spec_sl relation values (T.source input)
    in
    Ok vals
  else
    let* _, vals =
      eval_il (module T.Target) spec_il relation values (T.source input)
    in
    Ok vals

(* De-duplicated IL/SL dispatch — with session, no handler *)
let eval_task_with_session (type i) (module T : Task.S with type input = i)
    ?(config = Instrumentation.Config.default) ~sl_mode ~spec_il (input : i) =
  let* relation, values = T.parse_input ~spec:spec_il input in
  if sl_mode then
    let spec_sl = Pass.structure spec_il in
    Instrumentation.with_session config (Instrumentation.Static.SlSpec spec_sl)
    @@ fun () ->
    let* _, vals =
      eval_sl (module T.Target) spec_sl relation values (T.source input)
    in
    Ok vals
  else
    Instrumentation.with_session config (Instrumentation.Static.IlSpec spec_il)
    @@ fun () ->
    let* _, vals =
      eval_il (module T.Target) spec_il relation values (T.source input)
    in
    Ok vals

(* --- Outcome-based runners --- *)

type 'i test_result = {
  input : 'i;
  source : string;
  outcome : Task.test_outcome;
}

(* Run single input and compute outcome based on expectation.
   Includes full init/finish lifecycle - use for single runs. *)
let run_with_outcome_with_session (type i)
    (module T : Task.S with type input = i)
    ?(config = Instrumentation.Config.default) ~sl_mode ~spec_il (input : i) =
  let result =
    T.Target.handler (fun () ->
        eval_task_with_session (module T) ~config ~sl_mode ~spec_il input)
  in
  Task.compute_outcome (T.expectation input) result

(* Run single input without init/finish lifecycle.
   For use in batch/coverage runs where init/finish is managed externally. *)
let run_with_outcome (type i) (module T : Task.S with type input = i) ~sl_mode
    ~spec_il (input : i) =
  let test_case_id = T.source input in
  Instrumentation.Dispatcher.notify_test_start ~test_case_id;
  let result =
    try
      T.Target.handler (fun () -> eval_task (module T) ~sl_mode ~spec_il input)
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

(* Run one input without lifecycle, catching exceptions. *)
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

(* --- Suite runner --- *)

let run_suite_with_outcomes (type i) (module T : Task.S with type input = i)
    ?(config = Instrumentation.Config.default) ~sl_mode ~spec_il
    ?(verbose = false) (inputs : i list) =
  let total = List.length inputs in
  let run () =
    List.mapi
      (fun idx input ->
        if verbose then
          Format.printf "[%d/%d] %s... %!" (idx + 1) total (T.source input);
        run_one_input (module T) ~sl_mode ~spec_il ~verbose input)
      inputs
  in
  Instrumentation.with_session config (Instrumentation.Static.IlSpec spec_il)
    run

(* --- Target batch runner --- *)

type task_result = { task_name : string; summary : suite_summary }

let run_target_batch ?(config = Instrumentation.Config.default) ?test_dir
    ~(checkpoint_config : Checkpoint.config) ~verbose ~sl_mode ~spec_files
    spec_il tasks =
  let run_coverage () =
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
                Checkpoint.filter_remaining checkpoint all_inputs
                  ~get_id:T.source
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
  in
  Instrumentation.with_session config (Instrumentation.Static.IlSpec spec_il)
    run_coverage
