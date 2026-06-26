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
      Ok
        (Rewrite.Rewrite_system.string_of_system_maude
           ~rule_heads:(Rewrite.To_ctrs.rule_head_syms spec_il)
           (Rewrite.rewrite_spec spec_il))
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
      Ok (String.concat "\n" (Rewrite.def_symbols spec_il), false)
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

(* Run a translated spec in Maude. The execution backend (start-term sourcing
   for [--imp]/[--p4], [Maude_run] batch execution, the [--check-p4] result
   oracle) is reintroduced in M2; until then [run] emits the executable module,
   the same artifact [rewrite] produces by default. *)
let run_command =
  Core.Command.basic
    ~summary:"run a translated spec in Maude (currently: emit the module)"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
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
    Ok (Rewrite.To_maude.module_of_spec ~relations_as_rules spec_il)

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
