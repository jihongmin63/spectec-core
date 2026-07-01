open Spectec

let version = "0.1"
let ( let* ) = Result.bind

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a spec"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun spec_il ->
        Format.printf "%s\n" (Lang.Il.Print.string_of_spec spec_il))
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    Ok spec_il

let unparse_roundtrip filenames =
  let* spec_el = parse_spec_files filenames in
  let printed = Lang.El.Unparse.string_of_spec spec_el in
  let* spec_el' = parse_spec_string ~origin:"<roundtrip>" printed in
  if Lang.El.Eq.eq_spec spec_el spec_el' then Ok ()
  else
    Error
      (Error.RoundtripError
         ( Common.Source.no_region,
           "pretty-printed output did not reparse to the same AST" ))

let unparse_command =
  Core.Command.basic
    ~summary:"parse a spec and print it in canonical EL form (drops comments)"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and roundtrip =
    flag "-r" no_arg
      ~doc:
        " verify the pretty-printed output reparses to the same AST (prints \
         nothing on success)"
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    if roundtrip then
      Cli.Error_handling.guard_unit ~color (fun () ->
          unparse_roundtrip filenames)
    else
      Cli.Error_handling.guard ~color ~on_ok:(fun spec_el ->
          Format.printf "%s" (Lang.El.Unparse.string_of_spec spec_el))
      @@ fun () -> parse_spec_files filenames

let structure_command =
  Core.Command.basic ~summary:"structure a spec"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun spec_sl ->
        Format.printf "%s\n" (Lang.Sl.Print.string_of_spec spec_sl))
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_sl = structure spec_il in
    Ok spec_sl

let annotate_command =
  Core.Command.basic ~summary:"annotate a structured spec into PL form"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun spec_pl ->
        Format.printf "%s\n" (Pl.Print.string_of_spec spec_pl))
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_sl = structure spec_il in
    let henv = henv_of_el_spec spec in
    let henv = henv_with_il_spec henv spec_il in
    let spec_pl = annotate ~henv spec_sl |> shorten in
    Ok spec_pl

(* Walks [root] recursively and returns every file path under it whose
   basename ends in one of [exts]. Paths are returned relative to [root]. *)
let collect_files ~exts root =
  let rec walk acc dir =
    let entries = Sys.readdir dir in
    Array.sort String.compare entries;
    Array.fold_left
      (fun acc entry ->
        let path = Filename.concat dir entry in
        if Sys.is_directory path then walk acc path
        else if List.exists (Filename.check_suffix entry) exts then path :: acc
        else acc)
      acc entries
  in
  walk [] root |> List.rev

let splice_command =
  Core.Command.basic
    ~summary:"splice rendered spec text into AsciiDoc skeletons"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and input_dir =
    flag "-i" (required string)
      ~doc:"DIR directory of .adoc skeleton files (walked recursively)"
  and output_dir =
    flag "-o" (required string)
      ~doc:"DIR directory to write spliced output (mirrors input layout)"
  and missing_path =
    flag "--missing" (optional string)
      ~doc:"FILE write the unused-keys report to this path"
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun (spec_el, spec_pl) ->
        let inputs = collect_files ~exts:[ ".adoc" ] input_dir in
        let pairs =
          List.map
            (fun in_path ->
              let rel =
                let prefix_len = String.length input_dir + 1 in
                if
                  String.length in_path > prefix_len
                  && String.sub in_path 0 (String.length input_dir) = input_dir
                then
                  String.sub in_path prefix_len
                    (String.length in_path - prefix_len)
                else in_path
              in
              (in_path, Filename.concat output_dir rel))
            inputs
        in
        let report =
          Splice.Driver.run ~spec_el ~spec_pl
            ~source_entries:Splice.Registry.source
            ~prose_entries:Splice.Registry.prose ~filenames:pairs
        in
        match missing_path with
        | Some path ->
            let oc = open_out path in
            Fun.protect
              (fun () ->
                Out_channel.output_string oc (Splice.Report.to_string report))
              ~finally:(fun () -> Out_channel.close oc)
        | None -> ())
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_sl = structure spec_il in
    let henv = henv_of_el_spec spec in
    let henv = henv_with_il_spec henv spec_il in
    let spec_pl = annotate ~henv spec_sl |> shorten in
    Ok (spec, spec_pl)

(* Translate an elaborated IL spec to Maude. The default output is the
   executable, order-sorted Maude module ([Rewrite.To_maude.module_of_spec]) --
   the surface [run] executes. [--ctrs] instead dumps the analysis CTRS
   (single-sort Full Maude system, the same text [verify] sends the MFE), and
   [--simplified] the IL after the [Simplify] pre-pass; both are debug views of
   the translation stages. *)
let rewrite_command =
  Core.Command.basic ~summary:"translate a spec to an executable Maude module"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and ctrs =
    flag "--ctrs" no_arg
      ~doc:
        " dump the analysis CTRS (single-sort Full Maude system, what verify \
         checks) instead of the executable module"
  and simplified =
    flag "--simplified" no_arg
      ~doc:" dump the simplified IL spec (debug: inspect the Simplify pre-pass)"
  and symbol =
    flag "--symbol" (optional string)
      ~doc:
        "NAME with --ctrs, dump only this function/relation's dependency slice \
         (the unit verify checks per-symbol)"
  and relations_as_rules =
    flag "--relations-as-rules" no_arg
      ~doc:
        " keep relations as Maude rules (rl/crl) instead of equations for \
         input-moded ones"
  in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun out -> Format.printf "%s\n" out)
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    if simplified then
      Ok (Lang.Il.Print.string_of_spec (Rewrite.Simplify.simplify_spec spec_il))
    else if ctrs then
      let system = Rewrite.rewrite_spec spec_il in
      let system =
        match symbol with
        | Some name -> Rewrite.Rewrite_system.slice system ~roots:[ name ]
        | None -> system
      in
      Ok
        (Rewrite.Rewrite_system.string_of_system_maude
           ~rule_heads:(Rewrite.To_ctrs.rule_head_syms spec_il)
           system)
    else Ok (Rewrite.To_maude.module_of_spec ~relations_as_rules spec_il)

(* Confluence (Church-Rosser) and coherence of the spec's rewriting system via
   the Maude Formal Environment. [Rewrite.rewrite_spec] builds the structural
   CTRS, [--symbol] optionally slices it to one definition's dependency closure,
   and [Rewrite.Mfe.check] runs the CRC and ChC in one Maude invocation;
   non-input-moded relations ([Rewrite.To_ctrs.rule_head_syms]) are the rules,
   everything else equations. *)
let verify_command =
  Core.Command.basic
    ~summary:
      "verify confluence (Church-Rosser) and coherence of a spec via the MFE"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and symbol =
    flag "--symbol" (optional string)
      ~doc:"NAME check only this function/relation's dependency slice"
  and list_symbols =
    flag "--list-symbols" no_arg
      ~doc:" list the sliceable function/relation symbols and exit"
  and sizes =
    flag "--sizes" no_arg
      ~doc:
        " with --list-symbols, also print each symbol's slice rule count \
         (ascending) -- the cheap CRC tractability proxy"
  and timeout =
    flag "--timeout"
      (optional_with_default 60 int)
      ~doc:"S kill Maude after S seconds (default 60, 0 disables)"
  and maude_bin =
    flag "--maude-bin" (optional string) ~doc:"PATH path to the maude binary"
  and mfe_dir =
    flag "--mfe-dir" (optional string)
      ~doc:"DIR directory holding the MFE (Full Maude + CRC/ChC loader)"
  in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun (out, failed) ->
        Format.printf "%s\n" out;
        if failed then exit 1)
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    if list_symbols then
      let syms = Rewrite.def_symbols spec_il in
      if not sizes then Ok (String.concat "\n" syms, false)
      else
        (* Slice rule counts in one elaboration -- the cheap proxy for which
           symbols' CRC is tractable (a small closure) vs which pull in the whole
           system (the typing relations -> critical-pair blowup -> TIMEOUT). *)
        let system = Rewrite.rewrite_spec spec_il in
        let rows =
          List.map
            (fun s ->
              let n =
                List.length
                  (Rewrite.Rewrite_system.slice system ~roots:[ s ]).rules
              in
              (n, s))
            syms
        in
        let rows = List.sort compare rows in
        Ok
          ( String.concat "\n"
              (List.map (fun (n, s) -> Printf.sprintf "%d\t%s" n s) rows),
            false )
    else
      let system = Rewrite.rewrite_spec spec_il in
      let system =
        match symbol with
        | Some name -> Rewrite.Rewrite_system.slice system ~roots:[ name ]
        | None -> system
      in
      let result : Rewrite.Mfe.result =
        Rewrite.Mfe.check ~timeout ?maude_bin ?mfe_dir
          ~rule_heads:(Rewrite.To_ctrs.rule_head_syms spec_il)
          system
      in
      let verdict = Rewrite.Mfe.string_of_verdict in
      let line =
        Printf.sprintf "church-rosser: %s  coherence: %s"
          (verdict result.church_rosser)
          (verdict result.coherence)
      in
      let ok =
        result.church_rosser = Rewrite.Mfe.Yes
        && result.coherence = Rewrite.Mfe.Yes
      in
      Ok (line, not ok)

(* Emit the spec as an executable Maude module and run a start term through a
   local Maude binary (see {!Rewrite.Maude_run}). [--emit] dumps the module
   without invoking Maude; otherwise [--start] supplies the term to run, by
   default with [reduce] (input-moded relations are equations; use [--search]
   with [--relations-as-rules] to explore non-deterministic rule rewriting). *)
let run_command =
  Core.Command.basic
    ~summary:"run a translated spec in Maude (emit a module and execute a term)"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and start =
    flag "--start" (optional string)
      ~doc:
        "TERM Maude start term to run (required unless --imp, --p4, or --emit)"
  and imp =
    flag "--imp" (listed string)
      ~doc:
        "FILE parse an impty program and run it (builds the start term; repeat \
         to batch several through one Maude invocation)"
  and p4 =
    flag "--p4" (listed string)
      ~doc:
        "FILE parse a P4 program and run it through Program_ok against the \
         loaded spec (builds the start term; repeat to batch several through \
         one Maude invocation)"
  and includes =
    flag "-i" (listed string) ~doc:"DIR P4 include path (with --p4)"
  and task =
    flag "--task"
      (optional_with_default "run" string)
      ~doc:"WHICH run | eval | check (impty start relation; default run)"
  and emit =
    flag "--emit" no_arg ~doc:" print the Maude module and exit (do not run)"
  and search =
    flag "--search" no_arg
      ~doc:" explore rules + equations (search) instead of reduce"
  and _reduce =
    flag "--reduce" no_arg
      ~doc:" evaluate with equations only (reduce); the default mode"
  and rewrite =
    flag "--rewrite" no_arg
      ~doc:
        " apply rules along one path (rewrite); deterministic semantics \
         without the search blow-up"
  and relations_as_rules =
    flag "--relations-as-rules" no_arg
      ~doc:
        " keep relations as Maude rules (rl/crl) instead of equations for \
         input-moded ones (pair with --search to explore non-determinism)"
  and bound =
    flag "--bound" (optional int)
      ~doc:"N cap the number of search solutions explored"
  and maude_bin =
    flag "--maude-bin" (optional string) ~doc:"PATH path to the maude binary"
  and timeout =
    flag "--timeout"
      (optional_with_default 30 int)
      ~doc:"S kill Maude after S seconds (default 30, 0 disables)"
  and check_p4 =
    flag "--check-p4" no_arg
      ~doc:
        " for each --p4 program also typecheck it with the interpreter and \
         compare the typing RESULT value with Maude's (result MATCH/MISMATCH)"
  in
  fun () ->
    (match (start, imp, p4, emit) with
    | None, [], [], false ->
        Format.eprintf
          "run needs --start TERM, --imp FILE, or --p4 FILE (or --emit to just \
           print the module)@.";
        exit 2
    | _ -> ());
    Cli.Error_handling.guard ~color ~on_ok:(fun (out, failed) ->
        Format.printf "%s\n" out;
        if failed then exit 1)
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    (* Resolve the start terms to run, each labeled by its source: every impty
       program (--imp), every P4 program (--p4, run through Program_ok against
       the loaded spec), then a raw --start term. The labels survive into the
       batched output so each program's result stays identifiable. *)
    let sources =
      List.map
        (fun f ->
          ( f,
            `Other,
            fun () -> Targets_impty.Impty.maude_start_term ~task ~spec_il f ))
        imp
      @ List.map
          (fun f ->
            ( f,
              `P4 f,
              fun () -> Targets_p4.P4.maude_start_term ~includes ~spec_il f ))
          p4
      @
      match start with
      | Some t -> [ ("--start", `Other, fun () -> Ok t) ]
      | None -> []
    in
    (* Resolve each source INDEPENDENTLY: a P4 program that fails to produce a
       start term must NOT abort the whole invocation -- it becomes its own
       [Error] verdict so every other program in the batch still runs on the
       amortized path (the ~50k-line module is reflected/internalized ONCE per
       invocation, ~20s). Both failure shapes are handled: a clean [Error]
       result (e.g. a surface syntax error, which the [p4_16_errors] negatives
       are full of) AND a RAISED exception (e.g. a lexer error on a malformed
       integer literal), which the frontend does not always reflect into the
       result monad. *)
    let resolve_error_msg e =
      Diagnostic.Render.render_bag ~ansi:Diagnostic.Ansi.plain
        (Error.to_diagnostics e)
    in
    let resolved =
      List.map
        (fun (label, kind, build) ->
          let outcome =
            try build ()
            with exn ->
              Error (Error.UnhandledException (Printexc.to_string exn))
          in
          match outcome with
          | Ok term -> (label, kind, Ok term)
          | Error e -> (label, kind, Error (resolve_error_msg e)))
        sources
    in
    let module_text =
      Rewrite.To_maude.module_of_spec ~relations_as_rules spec_il
    in
    if emit then Ok (module_text, false)
    else
      let mode =
        if search then Rewrite.Maude_run.Search bound
        else if rewrite then Rewrite.Maude_run.Rewrite
        else Rewrite.Maude_run.Reduce
      in
      let defined_heads = Rewrite.To_maude.maude_defined_heads spec_il in
      let run_results =
        Rewrite.Maude_run.run_batch ?maude_bin ~timeout ~defined_heads ~mode
          ~module_text
          ~starts:
            (List.filter_map
               (fun (_, _, r) ->
                 match r with Ok t -> Some t | Error _ -> None)
               resolved)
          ()
      in
      (* Stitch the Maude results back onto the sources in order, dropping the
         resolution-failure verdict in where a program never produced a start
         term ([run_batch] returns exactly one result per runnable term,
         positionally). *)
      let rec stitch resolved run_results =
        match resolved with
        | [] -> []
        | (label, kind, Ok _) :: rest -> (
            match run_results with
            | r :: more -> (label, kind, r) :: stitch rest more
            | [] -> [])
        | (label, kind, Error msg) :: rest ->
            (label, kind, Rewrite.Maude_run.Error msg)
            :: stitch rest run_results
      in
      let outcomes = stitch resolved run_results in
      (* When [--check-p4], compare Maude's typing RESULT against the
         interpreter's for the same program: decode Maude's normal form back to
         an IL value ({!Rewrite.Of_maude}) and [Eq.eq_values] it against the
         interpreter's relation output. Returns the report line and whether it
         is a genuine mismatch (so a single-file run can exit non-zero). *)
      let string_of_values vs =
        String.concat ", " (List.map Lang.Il.Print.string_of_value vs)
      in
      let compare_p4 filename (result : Rewrite.Maude_run.result) :
          string * bool =
        match result with
        | Rewrite.Maude_run.Reduced term -> (
            let input =
              {
                Targets_p4.P4.Typecheck.includes;
                filename;
                expect = Spectec.Task.Positive;
              }
            in
            match
              Spectec.eval_task
                (module Targets_p4.P4.Typecheck)
                ~sl_mode:false ~spec_il input
            with
            | Error _ -> ("result: interp FAILED (not compared)", false)
            | Ok interp_vals -> (
                try
                  let interp_vals = Rewrite.Of_maude.canonicalize interp_vals in
                  let maude_vals =
                    Rewrite.Of_maude.canonicalize
                      (Rewrite.Of_maude.values_of_result spec_il
                         ~rel:"Program_ok" term)
                  in
                  if Lang.Il.Eq.eq_values interp_vals maude_vals then
                    ("result: MATCH", false)
                  else
                    ( Printf.sprintf
                        "result: MISMATCH\n  interp: %s\n  maude:  %s"
                        (string_of_values interp_vals)
                        (string_of_values maude_vals),
                      true )
                with Rewrite.Of_maude.Parse_error msg ->
                  (Printf.sprintf "result: decode error: %s" msg, false)))
        | _ -> ("result: not reduced (not compared)", false)
      in
      let render (_, kind, result) =
        let base = Rewrite.Maude_run.string_of_result result in
        match kind with
        | `P4 filename when check_p4 ->
            let line, mismatch = compare_p4 filename result in
            (base ^ "\n" ^ line, mismatch)
        | _ -> (base, false)
      in
      let rendered = List.map render outcomes in
      let failed =
        List.exists (fun (_, _, r) -> Rewrite.Maude_run.is_failure r) outcomes
        || List.exists snd rendered
      in
      (* A single start keeps the bare result line (golden/grep-stable); a batch
         labels each program's block so the results stay attributable. *)
      let out =
        match (outcomes, rendered) with
        | [ _ ], [ (body, _) ] -> body
        | _ ->
            List.map2
              (fun (label, _, _) (body, _) ->
                Printf.sprintf "=== %s ===\n%s" label body)
              outcomes rendered
            |> String.concat "\n"
      in
      Ok (out, failed)

let command =
  let module P4 = Targets_p4.P4.Cli in
  let module Impty = Targets_impty.Impty.Cli in
  Core.Command.group ~summary:"SpecTec command line tools"
    [
      ("unparse", unparse_command);
      ("elab", elab_command);
      ("struct", structure_command);
      ("annotate", annotate_command);
      ("splice", splice_command);
      ("rewrite", rewrite_command);
      ("verify", verify_command);
      ("run", run_command);
      (P4.name, P4.command);
      (Impty.name, Impty.command);
    ]

let () = Command_unix.run ~version command
