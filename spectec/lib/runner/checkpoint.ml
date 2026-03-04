(* ============================================================================
   TYPES AND CONFIGURATION
   ============================================================================ *)

(* Configuration for checkpointing behavior *)
type config = {
  output_file : string option; (* File to save checkpoints to *)
  resume_from : string option; (* File to resume from, if any *)
  save_interval : int; (* Save checkpoint every N tests *)
}

let default_config =
  { output_file = None; resume_from = None; save_interval = 100 }

(* Coverage state: dynamic association list of (handler_name, marshaled_state) *)
type coverage = (string * bytes) list

(* Main checkpoint type - saved/loaded state *)
type t = {
  version : int; (* = 2; detect stale checkpoints from format changes *)
  spec_hash : string; (* MD5 of concatenated spec file contents *)
  completed_inputs : string list; (* IDs of processed test cases *)
  coverage : coverage;
  timestamp : float; (* Unix timestamp *)
}

let current_version = 2

(* ============================================================================
   INTERNAL HELPERS
   ============================================================================ *)

(* Compute MD5 hash of spec files for change detection *)
let compute_spec_hash spec_files =
  let contents =
    List.sort String.compare spec_files
    |> List.map (fun filename ->
           let input_channel = open_in filename in
           let length = in_channel_length input_channel in
           let content = really_input_string input_channel length in
           close_in input_channel;
           content)
    |> String.concat "\n"
  in
  Digest.string contents |> Digest.to_hex

(* Create a new checkpoint with current timestamp *)
let create ~spec_files ~completed_inputs ~coverage =
  {
    version = current_version;
    spec_hash = compute_spec_hash spec_files;
    completed_inputs;
    coverage;
    timestamp = Unix.gettimeofday ();
  }

(* Format checkpoint as human-readable summary string *)
let format_summary checkpoint =
  Printf.sprintf "Checkpoint: %d tests completed, saved at %s"
    (List.length checkpoint.completed_inputs)
    ( Unix.gmtime checkpoint.timestamp |> fun time_record ->
      Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d UTC"
        (time_record.tm_year + 1900)
        (time_record.tm_mon + 1) time_record.tm_mday time_record.tm_hour
        time_record.tm_min time_record.tm_sec )

(* ============================================================================
   FILE I/O (SERIALIZATION)
   ============================================================================ *)

(* Load checkpoint from file using Marshal.
   Returns Ok checkpoint if successful, Error if file cannot be loaded or stale format. *)
let load_from_file ~file =
  try
    let input_channel = open_in_bin file in
    let checkpoint : t = Marshal.from_channel input_channel in
    close_in input_channel;
    if checkpoint.version <> current_version then
      Error
        (Error.DirectoryError
           "Checkpoint format has changed; delete checkpoint file and re-run")
    else Ok checkpoint
  with
  | Sys_error msg ->
      Error
        (Error.DirectoryError
           (Printf.sprintf "Failed to load checkpoint file '%s': %s" file msg))
  | Failure _ | Invalid_argument _ ->
      Error
        (Error.DirectoryError
           "Checkpoint format has changed; delete checkpoint file and re-run")
  | e ->
      Error
        (Error.DirectoryError
           (Printf.sprintf "Failed to load checkpoint file '%s': %s" file
              (Printexc.to_string e)))

(* Save checkpoint to file using Marshal *)
let save_to_file ~file checkpoint =
  let output_channel = open_out_bin file in
  Marshal.to_channel output_channel checkpoint [];
  close_out output_channel

(* ============================================================================
   VALIDATION
   ============================================================================ *)

(* Verify that spec files haven't changed since checkpoint was created *)
let verify_spec checkpoint ~spec_files =
  let current_hash = compute_spec_hash spec_files in
  if checkpoint.spec_hash = current_hash then Ok ()
  else Error (Error.SpecMismatchError (checkpoint.spec_hash, current_hash))

(* ============================================================================
   COVERAGE OPERATIONS — descriptor-driven
   ============================================================================ *)

(* Capture current coverage state from all handlers that have checkpoint support *)
let snapshot_coverage () =
  List.filter_map
    (fun (module D : Instrumentation.Descriptor.S) ->
      D.checkpoint
      |> Option.map (fun (ops : Instrumentation.Descriptor.checkpoint_ops) ->
             (D.name, ops.snapshot ())))
    Instrumentation.all_descriptors

(* Restore coverage state from checkpoint into instrumentation handlers *)
let restore_coverage checkpoint =
  List.iter
    (fun (name, data) ->
      let found =
        List.find_opt
          (fun (module D : Instrumentation.Descriptor.S) -> D.name = name)
          Instrumentation.all_descriptors
      in
      match found with
      | Some (module D) -> (
          match D.checkpoint with
          | Some (ops : Instrumentation.Descriptor.checkpoint_ops) ->
              ops.restore data
          | None -> ())
      | None -> ())
    checkpoint.coverage

(* Merge two coverage structures using each descriptor's merge operation *)
let merge_coverage c1 c2 =
  let all_names =
    List.sort_uniq String.compare (List.map fst c1 @ List.map fst c2)
  in
  List.filter_map
    (fun name ->
      let ops_opt =
        match
          List.find_opt
            (fun (module D : Instrumentation.Descriptor.S) -> D.name = name)
            Instrumentation.all_descriptors
        with
        | Some (module D : Instrumentation.Descriptor.S) -> D.checkpoint
        | None -> None
      in
      match (ops_opt, List.assoc_opt name c1, List.assoc_opt name c2) with
      | Some (ops : Instrumentation.Descriptor.checkpoint_ops), Some b1, Some b2
        ->
          Some (name, ops.merge b1 b2)
      | _, Some b, None -> Some (name, b)
      | _, None, Some b -> Some (name, b)
      | _ -> None)
    all_names

(* ============================================================================
   MERGE OPERATIONS
   ============================================================================ *)

(* Merge two checkpoints into a new checkpoint.
   - Merges completed_inputs (union)
   - Merges coverage data via descriptor checkpoint ops
   - Uses spec_hash from first checkpoint (they should match)
   - Creates new timestamp *)
let merge checkpoint1 checkpoint2 =
  if checkpoint1.spec_hash <> checkpoint2.spec_hash then
    Error
      (Error.SpecMismatchError (checkpoint1.spec_hash, checkpoint2.spec_hash))
  else
    let completed_inputs =
      let seen = Hashtbl.create 256 in
      List.iter
        (fun id -> Hashtbl.replace seen id ())
        checkpoint1.completed_inputs;
      List.iter
        (fun id -> Hashtbl.replace seen id ())
        checkpoint2.completed_inputs;
      Hashtbl.fold (fun id () seen_list -> id :: seen_list) seen []
    in
    let coverage = merge_coverage checkpoint1.coverage checkpoint2.coverage in
    Ok
      {
        version = current_version;
        spec_hash = checkpoint1.spec_hash;
        completed_inputs;
        coverage;
        timestamp = Unix.gettimeofday ();
      }

(* ============================================================================
   PUBLIC API
   ============================================================================ *)

(* Load and verify checkpoint from file.
   Returns Ok checkpoint if valid, Error if file cannot be loaded or spec mismatch. *)
let verify_and_load ~file ~spec_files ~verbose =
  let ( let* ) = Result.bind in
  let* checkpoint = load_from_file ~file in
  match verify_spec checkpoint ~spec_files with
  | Ok () ->
      if verbose then
        Format.printf "Resuming from checkpoint: %s\n"
          (format_summary checkpoint);
      Ok checkpoint
  | Error e -> Error e

(* Filter out already-completed inputs from a list *)
let filter_remaining checkpoint inputs ~get_id =
  List.filter
    (fun input -> not (List.mem (get_id input) checkpoint.completed_inputs))
    inputs

(* Capture current state and save checkpoint to file.
   Collects current coverage state and completed inputs, then saves to file if configured. *)
let save ~spec_files ~completed_inputs ~output_file =
  match output_file with
  | Some file ->
      let checkpoint =
        create ~spec_files ~completed_inputs ~coverage:(snapshot_coverage ())
      in
      save_to_file ~file checkpoint
  | None -> ()

(* ============================================================================
   DISPLAY
   ============================================================================ *)

(* Display full checkpoint report with coverage data.
   Uses provided config for output destinations, or defaults to Full/stdout. *)
let display_report ~spec ~(config : Instrumentation.Config.t) checkpoint =
  Format.printf "=== Checkpoint Contents ===\n\n";
  Format.printf "%s\n\n" (format_summary checkpoint);
  Format.printf "Completed tests: %d\n\n"
    (List.length checkpoint.completed_inputs);
  (* Build active_handler list: use user config if present, else default to full/stdout *)
  let active =
    List.filter_map
      (fun (module D : Instrumentation.Descriptor.S) ->
        match D.checkpoint with
        | None -> None
        | Some _ -> (
            match
              List.find_opt
                (fun a -> a.Instrumentation.Descriptor.name = D.name)
                config
            with
            | Some a -> Some a
            | None -> D.parse [ ("level", Some "full"); ("output", None) ]))
      Instrumentation.all_descriptors
  in
  let handlers = Instrumentation.Config.to_handlers active in
  Instrumentation_static.Static.reset_all ();
  Instrumentation_static.Static.init_all
    (Instrumentation_static.Static.IlSpec spec);
  Instrumentation.Dispatcher.set_handlers handlers;
  Instrumentation.Dispatcher.init ~spec:(Instrumentation.Handler.IlSpec spec);
  restore_coverage checkpoint;
  Instrumentation.Dispatcher.finish ();
  Instrumentation.Config.close_outputs active
