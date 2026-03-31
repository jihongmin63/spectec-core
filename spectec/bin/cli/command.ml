(** CLI command generator for input specs and target specs *)

(** Extended input spec with CLI argument parsing support *)
module type CLI_TASK = sig
  include Spectec.Task.S

  (** Command-line argument parser that produces an input value *)
  val cli_flags : input Core.Command.Param.t
end

let ( let* ) = Result.bind

(* Collect spec files from a directory - I/O utility *)
let collect_spec_files spec_dir =
  Sys.readdir spec_dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".spectec")
  |> List.sort String.compare
  |> List.map (Filename.concat spec_dir)

(* Print outcome for a single test *)
let print_outcome (type i) (module T : Spectec.Task.S with type input = i)
    source outcome =
  let open Spectec in
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

(* Run interpreter on a single input and print result *)
let run_single (type i) (module T : Spectec.Task.S with type input = i) ~config
    ~sl_mode ~spec_il (input : i) =
  let outcome =
    Runner.Suite.run_with_outcome_with_session
      (module T)
      ~config ~sl_mode ~spec_il input
  in
  print_outcome (module T) (T.source input) outcome

(* Run interpreter on a suite of inputs and print results *)
let run_suite (type i) (module T : Spectec.Task.S with type input = i) ~config
    ~sl_mode ~spec_il ~verbose (inputs : i list) =
  let results =
    Runner.Suite.run_suite_with_outcomes
      (module T)
      ~config ~sl_mode ~spec_il ~verbose inputs
  in
  match verbose with
  | true ->
      let summary = Runner.Suite.summarize_outcomes results in
      let passed = Runner.Suite.summary_passed summary in
      let failed = Runner.Suite.summary_failed summary in
      Format.printf "\nTest Results: %d/%d passed, %d failed\n" passed
        summary.total failed
  | false ->
      List.iter
        (fun Runner.Suite.{ source; outcome; _ } ->
          Format.printf ">>> Running %s on %s\n" T.name source;
          print_outcome (module T) source outcome)
        results;
      let summary = Runner.Suite.summarize_outcomes results in
      let passed = Runner.Suite.summary_passed summary in
      let failed = Runner.Suite.summary_failed summary in
      Format.printf "\nTest Results: %d/%d passed, %d failed\n" passed
        summary.total failed

(* Generate a CLI command for any CLI_TASK *)
let make (type i) ~summary (module T : CLI_TASK with type input = i) =
  Core.Command.basic ~summary
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       flag "--spec" (listed string)
         ~doc:"FILES spec files (default: use target spec dir)"
     and sl_mode = flag "--sl" no_arg ~doc:" use SL interpreter (default: IL)"
     and verbose = flag "-v" no_arg ~doc:" verbose output"
     and suite_mode =
       flag "--suite" no_arg ~doc:" run on test suite (default dir)"
     and suite_dir_arg =
       flag "--suite-dir" (optional string) ~doc:"DIR run on test suite in DIR"
     and input = T.cli_flags
     and config = Cli_args.config_flags in
     fun () ->
       let open Spectec in
       let run () =
         let* () =
           Instrumentation.Config.validate_mode config ~sl_mode
           |> Result.map_error (fun msg ->
                  Error.ConfigError (Common.Source.no_region, msg))
         in
         let filenames_spec =
           match filenames_spec with
           | [] -> collect_spec_files T.Target.spec_dir
           | files -> files
         in
         let* spec = parse_spec_files filenames_spec in
         let* spec_il = elaborate spec in
         match (suite_mode, suite_dir_arg) with
         | false, None ->
             run_single (module T) ~config ~sl_mode ~spec_il input;
             Ok ()
         | true, None ->
             run_suite
               (module T)
               ~config ~sl_mode ~spec_il ~verbose (T.collect ());
             Ok ()
         | _, Some dir ->
             run_suite
               (module T)
               ~config ~sl_mode ~spec_il ~verbose (T.collect ~dir ());
             Ok ()
       in
       match run () with
       | Ok () -> ()
       | Error e ->
           Format.printf "Error:\n  %s\n" (Spectec.Error.string_of_error e))

let make_parse (type i) ~summary (module T : CLI_TASK with type input = i) =
  Core.Command.basic ~summary
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       flag "--spec" (listed string)
         ~doc:"FILES spec files (default: use target spec dir)"
     and input = T.cli_flags
     and roundtrip = flag "-r" no_arg ~doc:" roundtrip parse/unparse" in
     fun () ->
       let run () =
         let spec_files =
           match filenames_spec with
           | [] -> collect_spec_files T.Target.spec_dir
           | files -> files
         in
         let open Spectec in
         let* spec_el = parse_spec_files spec_files in
         let* spec_il = elaborate spec_el in
         let* _, values = T.parse_input ~spec:spec_il input in
         let unparsed = T.unparse ~spec:spec_il values in
         if roundtrip then
           let* values_rt =
             unparsed |> T.parse_string ~spec:spec_il ~filename:(T.source input)
           in
           let eq = Lang.Il.Eq.eq_values ~dbg:true values values_rt in
           if eq then Ok unparsed
           else
             Error
               (Error.RoundtripError
                  (Common.Source.no_region, "Roundtrip failed"))
         else Ok unparsed
       in
       match run () with
       | Ok s -> Format.printf "%s\n" s
       | Error e ->
           Format.printf "Error:\n  %s\n" (Spectec.Error.string_of_error e))

(* Functor to generate commands for a specific target.
   Enforces that only tasks belonging to this target can be used. *)
module Make (Tgt : Spectec.Target.S) = struct
  module type TARGET_TASK = Spectec.Task.S with module Target = Tgt

  type packed_task =
    | Pack : (module TARGET_TASK with type input = 'a) -> packed_task

  let to_generic (Pack (module T)) = Spectec.Task.Pack (module T)

  let make_target_batch (tasks : packed_task list) =
    Core.Command.basic
      ~summary:("Run coverage for all " ^ Tgt.name ^ " input specs")
      (let open Core.Command.Let_syntax in
       let open Core.Command.Param in
       let%map sl_mode =
         flag "--sl" no_arg ~doc:" use SL interpreter (default: IL)"
       and verbose =
         flag "-v" no_arg ~doc:" verbose: print progress for each test"
       and test_dir =
         flag "--test-dir" (optional string)
           ~doc:
             "DIR directory containing test inputs (default: target's test \
              directory)"
       and checkpoint_output_file =
         flag "--checkpoint" (optional string)
           ~doc:"FILE save checkpoint to file (enables resume)"
       and checkpoint_resume_file =
         flag "--resume" (optional string)
           ~doc:"FILE resume from checkpoint file"
       and checkpoint_save_interval =
         flag "--save-interval"
           (optional_with_default 100 int)
           ~doc:"N save checkpoint every N tests (default: 100)"
       and instrumentation_config = Cli_args.config_flags in
       fun () ->
         let open Spectec in
         let run () =
           let* () =
             Instrumentation.Config.validate_mode instrumentation_config
               ~sl_mode
             |> Result.map_error (fun msg ->
                    Error.ConfigError (Common.Source.no_region, msg))
           in
           let spec_files = collect_spec_files Tgt.spec_dir in
           let checkpoint_config : Runner.Checkpoint.config =
             {
               output_file = checkpoint_output_file;
               resume_from = checkpoint_resume_file;
               save_interval = checkpoint_save_interval;
             }
           in
           let* spec = parse_spec_files spec_files in
           let* spec_il = elaborate spec in
           let generic_tasks = List.map to_generic tasks in
           let results =
             Runner.Suite.run_target_batch ~config:instrumentation_config
               ?test_dir ~checkpoint_config ~verbose ~sl_mode ~spec_files
               spec_il generic_tasks
           in
           List.iter
             (fun Runner.Suite.{ task_name; summary } ->
               let passed = Runner.Suite.summary_passed summary in
               let failed = Runner.Suite.summary_failed summary in
               Format.printf "%s: %d/%d passed, %d failed\n" task_name passed
                 summary.total failed)
             results;
           Ok ()
         in
         match run () with
         | Ok () -> ()
         | Error error ->
             Format.printf "Error:\n  %s\n" (Error.string_of_error error))

  let make_checkpoint () =
    let report_command =
      Core.Command.basic ~summary:"Decode and display checkpoint contents"
        (let open Core.Command.Let_syntax in
         let open Core.Command.Param in
         let%map checkpoint_file = anon ("checkpoint-file" %: string)
         and instrumentation_config = Cli_args.config_flags in
         fun () ->
           let open Spectec in
           let run () =
             let spec_files = collect_spec_files Tgt.spec_dir in
             let* spec = parse_spec_files spec_files in
             let* spec_il = elaborate spec in
             let* checkpoint =
               Runner.Checkpoint.verify_and_load ~file:checkpoint_file
                 ~spec_files ~verbose:true
             in
             Runner.Checkpoint.display_report ~spec:spec_il
               ~config:instrumentation_config checkpoint;
             Ok ()
           in
           match run () with
           | Ok () -> ()
           | Error error ->
               Format.printf "Error:\n  %s\n" (Error.string_of_error error))
    in
    let merge_command =
      Core.Command.basic ~summary:"Merge two checkpoint files"
        (let open Core.Command.Let_syntax in
         let open Core.Command.Param in
         let%map checkpoint_file1 = anon ("checkpoint-file-1" %: string)
         and checkpoint_file2 = anon ("checkpoint-file-2" %: string)
         and output_file =
           flag "--output" (required string)
             ~doc:"FILE output file for merged checkpoint"
         in
         fun () ->
           let open Spectec in
           let run () =
             let spec_files = collect_spec_files Tgt.spec_dir in
             let* checkpoint1 =
               Runner.Checkpoint.verify_and_load ~file:checkpoint_file1
                 ~spec_files ~verbose:false
             in
             let* checkpoint2 =
               Runner.Checkpoint.verify_and_load ~file:checkpoint_file2
                 ~spec_files ~verbose:false
             in
             let* merged = Runner.Checkpoint.merge checkpoint1 checkpoint2 in
             Runner.Checkpoint.save_to_file ~file:output_file merged;
             Format.printf "Merged checkpoint saved to: %s\n" output_file;
             Format.printf "  Checkpoint 1: %d tests\n"
               (List.length checkpoint1.completed_inputs);
             Format.printf "  Checkpoint 2: %d tests\n"
               (List.length checkpoint2.completed_inputs);
             Format.printf "  Merged: %d tests\n"
               (List.length merged.completed_inputs);
             Ok ()
           in
           match run () with
           | Ok () -> ()
           | Error error ->
               Format.printf "Error:\n  %s\n" (Error.string_of_error error))
    in
    Core.Command.group ~summary:"Checkpoint utilities"
      [ ("report", report_command); ("merge", merge_command) ]
end
