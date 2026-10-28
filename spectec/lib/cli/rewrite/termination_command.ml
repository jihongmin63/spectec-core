open Common

(* One file per symbol under [--proof-dir], written as each row lands so an
   interrupted sweep keeps the trees it already has. Symbol names carry [$] and
   other punctuation the shell reads, so anything outside a bare filename
   alphabet becomes [_] -- a mangling, not a sanitization: it never collapses
   [$f] and [f] onto one path, since only the former grows a leading [_]. *)
let write_proof_tree ~(dir : string) ~(sym : string) (tree : string option) :
    unit =
  match tree with
  | None -> ()
  | Some tree ->
      if not (Sys.file_exists dir) then Core_unix.mkdir_p dir;
      let basename =
        String.map
          (fun c ->
            match c with
            | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '-' | '_' -> c
            | _ -> '_')
          sym
      in
      let oc = open_out (Filename.concat dir (basename ^ ".proof")) in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> output_string oc (tree ^ "\n"))

let report_row sym (report : Rewrite.Termination.report) =
  let stats =
    match report.stats with
    | Some s -> Rewrite.Unravel.string_of_stats s
    | None -> "-"
  in
  (* Answering-path time excludes unsuccessful lower budget rungs. *)
  let secs =
    match report.secs with Some s -> Printf.sprintf "%.1f" s | None -> "-"
  in
  (* Preserve failed path details even when a later path supplies the verdict. *)
  let paths =
    match report.steps with
    | [] -> "-"
    | steps ->
        String.concat " " (List.map Rewrite.Termination.string_of_step steps)
  in
  ( Printf.sprintf "%s\t%s\t%s\t%s\t%s" sym
      (Rewrite.Termination.string_of_verdict report.verdict)
      stats secs paths,
    match report.verdict with
    | Rewrite.Termination.Yes | Rewrite.Termination.Degenerate -> false
    | _ -> true )

let check ~filenames ~all ~symbols ~budget ~emit ~proof_dir ~out =
  let* { Rewrite.spec = spec_tr; system } =
    load ~target:Rewrite.Analysis filenames
  in
  let slicer = Rewrite.Rewrite_system.make_slicer system in
  let roots =
    Cli.Analysis_sweep.roots ~all ~symbols
      ~all_roots:(Rewrite.To_ctrs.def_symbols spec_tr)
      ~slice_size:(slice_size slicer)
  in
  if emit then (
    let slice = Rewrite.Rewrite_system.slice_with slicer ~roots:symbols in
    match Rewrite.Termination.trs_of_slice slice with
    | Ok (trs, stats) ->
        print_string trs;
        Printf.eprintf "%s\n" (Rewrite.Unravel.string_of_stats stats);
        Ok false
    | Error msg ->
        Printf.eprintf "unravel: %s\n" msg;
        Ok true)
  else
    Ok
      (Cli.Analysis_sweep.rows ~out ~roots ~key:(slice_key slicer)
         ~row_of:(fun sym ->
           let slice =
             Rewrite.Rewrite_system.slice_with slicer ~roots:[ sym ]
           in
           let report = Rewrite.Termination.check ~budget slice in
           Option.iter
             (fun dir -> write_proof_tree ~dir ~sym report.proof)
             proof_dir;
           report_row sym report))

let command =
  Core.Command.basic
    ~summary:
      "prove per-symbol termination of the analysis CTRS (unravel + AProVE)"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and symbols = symbols_flag
  and all = all_flag
  and budget =
    flag "--budget"
      (optional_with_default 300 int)
      ~doc:"S AProVE proof budget per symbol (default 300)"
  and emit =
    flag "--emit" no_arg
      ~doc:" print the plain unraveled TRS (exactly one --symbol)"
  and proof_dir =
    flag "--proof-dir" (optional string)
      ~doc:"DIR write each symbol's AProVE proof tree to DIR/<symbol>.proof"
  and out = out_flag in
  fun () ->
    Cli.Analysis_sweep.require_roots ~cmd:"termination" ~all ~symbols;
    if emit then
      Cli.Analysis_sweep.require_single_symbol ~flag:"--emit" ~all ~symbols;
    Cli.Error_handling.guard ~color ~on_ok:(fun failed -> if failed then exit 1)
    @@ fun () -> check ~filenames ~all ~symbols ~budget ~emit ~proof_dir ~out
