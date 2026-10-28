open Common
module R = Rewrite.Rewrite_system

let list_symbols spec =
  let symbols = Rewrite.def_symbols spec in
  let { Rewrite.system; _ } = Rewrite.compile ~target:Rewrite.Analysis spec in
  let slicer = R.make_slicer system in
  List.map (fun symbol -> (slice_size slicer symbol, symbol)) symbols
  |> List.sort compare
  |> List.map (fun (size, symbol) -> Printf.sprintf "%s\t%d" symbol size)
  |> String.concat "\n"

let translate ~filenames ~ctrs ~symbol ~list =
  let* spec = load_spec filenames in
  if list then Ok (list_symbols spec)
  else
    let target = if ctrs then Rewrite.Analysis else Rewrite.Execution in
    let { Rewrite.spec; system } = Rewrite.compile ~target spec in
    if ctrs then
      let sig_rules = system.R.rules in
      let slice =
        match symbol with
        | Some symbol -> R.slice system ~roots:[ symbol ]
        | None -> system
      in
      Ok
        (Rewrite.To_mfe.module_of_system ~prune_signature:true ~sig_rules spec
           slice)
    else Ok (Rewrite.To_maude.module_of_system spec system)

let command =
  Core.Command.basic ~summary:"translate a spec to an executable Maude module"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and ctrs =
    flag "--ctrs" no_arg
      ~doc:" dump the analysis CTRS instead of the executable Maude module"
  and symbol =
    flag "--symbol" (optional string)
      ~doc:"NAME with --ctrs, dump this symbol's dependency slice"
  and list_symbols =
    flag "--list-symbols" no_arg
      ~doc:" list symbol and slice rule count, smallest slice first"
  in
  fun () ->
    require (symbol = None || ctrs) "--symbol requires --ctrs";
    Cli.Error_handling.guard ~color ~on_ok:(fun out -> Format.printf "%s\n" out)
    @@ fun () -> translate ~filenames ~ctrs ~symbol ~list:list_symbols
