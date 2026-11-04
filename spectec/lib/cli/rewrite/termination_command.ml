open Common
module S = Rewrite.Solver
module T = Rewrite.Termination
module Results = Termination_results

let write_artifact path contents =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

let artifact_name symbol =
  let prefix =
    String.map
      (function
        | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '-' | '_') as c -> c
        | _ -> '_')
      symbol
  in
  let prefix = String.sub prefix 0 (min 80 (String.length prefix)) in
  prefix ^ "-" ^ Digest.to_hex (Digest.string symbol)

let write_artifacts ~dir ~symbol ~tool (run : S.run) =
  let dir = Filename.concat dir tool in
  Core_unix.mkdir_p dir;
  let path = Filename.concat dir (artifact_name symbol) in
  write_artifact (path ^ ".stdout") run.stdout;
  write_artifact (path ^ ".stderr") run.stderr;
  write_artifact (path ^ ".proof") run.proof

let configured names =
  List.fold_left
    (fun result name ->
      Result.bind result (fun tools ->
          Result.bind (Rewrite.Termination_tools.find name) (fun adapter ->
              Result.map (fun tool -> tools @ [ tool ]) (S.configure adapter))))
    (Ok []) names

let run_checks ~system ~slicer ~roots ~tools ~timeout ~proof_dir ~out =
  let identity =
    String.concat "\n"
      ("single-execution-v1" :: string_of_int timeout :: slice_digest system
      :: Digest.to_hex (Digest.file Sys.executable_name)
      :: List.map
           (fun (tool : S.tool) -> tool.adapter.name ^ ":" ^ tool.identity)
           tools)
    |> Digest.string |> Digest.to_hex
  in
  let jobs =
    List.concat_map
      (fun symbol ->
        List.map (fun (tool : S.tool) -> (symbol, tool.adapter.name)) tools)
      roots
  in
  let row_of symbol name : Results.row =
    let tool =
      List.find (fun (tool : S.tool) -> tool.adapter.name = name) tools
    in
    let slice = Rewrite.Rewrite_system.slice_with slicer ~roots:[ symbol ] in
    if slice.rules = [] then
      {
        symbol;
        tool = name;
        verdict = Results.Degenerate;
        stats = "-";
        secs = 0.;
      }
    else
      match T.prepare slice with
      | Error message ->
          {
            symbol;
            tool = name;
            verdict = Results.Failed message;
            stats = "-";
            secs = 0.;
          }
      | Ok problem ->
          let run = T.check ~tool ~timeout problem in
          Option.iter
            (fun dir -> write_artifacts ~dir ~symbol ~tool:name run)
            proof_dir;
          {
            symbol;
            tool = name;
            verdict =
              (match run.verdict with
              | Ok verdict -> Results.Answer verdict
              | Error message -> Results.Failed message);
            stats = Rewrite.Unravel.string_of_stats problem.stats;
            secs = run.secs;
          }
  in
  Results.run ~out ~identity ~jobs ~row_of

let check ~filenames ~all ~symbols ~tools ~timeout ~emit ~proof_dir ~out =
  let* { Rewrite.spec = spec_tr; system } =
    load ~target:Rewrite.Analysis filenames
  in
  let slicer = Rewrite.Rewrite_system.make_slicer system in
  let result =
    if emit then
      let slice = Rewrite.Rewrite_system.slice_with slicer ~roots:symbols in
      match T.trs_of_slice slice with
      | Ok (trs, stats) ->
          print_string trs;
          Printf.eprintf "%s\n" (Rewrite.Unravel.string_of_stats stats);
          Ok false
      | Error message -> Error message
    else
      Result.bind (configured tools) (fun tools ->
          let roots =
            Cli.Analysis_sweep.roots ~all ~symbols
              ~all_roots:(Rewrite.To_ctrs.def_symbols spec_tr)
              ~slice_size:(slice_size slicer)
            |> Rewrite.Rewrite_system.dedup_stable
          in
          run_checks ~system ~slicer ~roots ~tools ~timeout ~proof_dir ~out)
  in
  match result with
  | Ok failed -> Ok failed
  | Error message ->
      Printf.printf "ERROR: %s\n%!" message;
      Ok true

let command =
  Core.Command.basic
    ~summary:"check per-symbol plain TRS termination with selected tools"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and symbols = symbols_flag
  and all = all_flag
  and tools =
    flag "--tool" (listed string)
      ~doc:
        "NAME termination prover: aprove or natt (repeatable; default aprove)"
  and timeout =
    flag "--timeout"
      (optional_with_default 300 int)
      ~doc:"S wall-clock limit per symbol and tool (default 300)"
  and emit =
    flag "--emit" no_arg
      ~doc:" print the plain TRS (exactly one --symbol; no tool invocation)"
  and proof_dir =
    flag "--proof-dir" (optional string)
      ~doc:"DIR save tool-specific stdout, stderr, and available proof text"
  and out =
    flag "--out" (optional string)
      ~doc:
        "TSV resume symbol/tool rows only when specification, tools and \
         settings match"
  in
  fun () ->
    Cli.Analysis_sweep.require_roots ~cmd:"termination" ~all ~symbols;
    require (timeout > 0) "--timeout must be positive";
    require
      (List.length tools = List.length (List.sort_uniq String.compare tools))
      "--tool names must be unique";
    if emit then (
      Cli.Analysis_sweep.require_single_symbol ~flag:"--emit" ~all ~symbols;
      require
        (tools = [] && proof_dir = None && out = None)
        "--emit excludes --tool, --proof-dir and --out");
    let tools = if tools = [] then [ "aprove" ] else tools in
    Cli.Error_handling.guard ~color ~on_ok:(fun failed -> if failed then exit 1)
    @@ fun () ->
    check ~filenames ~all ~symbols ~tools ~timeout ~emit ~proof_dir ~out
