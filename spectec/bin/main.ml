open Spectec

let version = "0.1"
let ( let* ) = Result.bind

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a spec"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string)) in
  fun () ->
    let elaborate_result =
      let* spec = parse_spec_files filenames in
      let* spec_il = elaborate spec in
      Ok spec_il
    in
    match elaborate_result with
    | Ok spec_il -> Format.printf "%s\n" (Lang.Il.Print.string_of_spec spec_il)
    | Error e -> Format.printf "%s\n" (Error.string_of_error e)

let structure_command =
  Core.Command.basic ~summary:"structure a spec"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string)) in
  fun () ->
    let structure_result =
      let* spec = parse_spec_files filenames in
      let* spec_il = elaborate spec in
      let spec_sl = structure spec_il in
      Ok spec_sl
    in
    match structure_result with
    | Ok spec_sl -> Format.printf "%s\n" (Lang.Sl.Print.string_of_spec spec_sl)
    | Error e -> Format.printf "%s\n" (Error.string_of_error e)

let generate_comamnd =
  Core.Command.basic ~summary:"generate the inputs according to the spec"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map rule = anon("applying rule" %: string) 
    and value_filename = anon("expected result" %: string) 
    and spec_filenames = anon (sequence ("spec files" %: string)) in
  fun () ->
    let elaborate_result =
      let* spec = parse_spec_files spec_filenames in
      let* spec_il = elaborate spec in
      Ok spec_il
    in
    let parse_result =
      try
        let ic = open_in value_filename in
        let length = in_channel_length ic in
        let content = really_input_string ic length in
        close_in ic;
        parse_il_value content
      with
      | Sys_error msg -> Error msg
    in
    match elaborate_result with
    | Ok spec_il -> (
      match parse_result with
      | Ok value -> Format.printf "%s\n" (Lang.Il.Print.string_of_value value)
      | Error msg -> Format.printf "Parse error: %s\n" msg
    )
    | Error e -> Format.printf "%s\n" (Error.string_of_error e)

(* Instantiate CLI commands for P4 *)
module P4_Cmd = Cli.Command.Make (Targets_p4.P4.Target)

let p4_command =
  let tasks = [ P4_Cmd.Pack (module Targets_p4.P4.Typecheck) ] in
  Core.Command.group ~summary:"P4 commands"
    [
      ("typecheck", Targets.P4.command);
      ( "parse",
        Cli.Command.make_parse ~summary:"parse a P4 program"
          (module Targets.P4.Cli_task) );
      ("batch", P4_Cmd.make_target_batch tasks);
      ("checkpoint", P4_Cmd.make_checkpoint ());
    ]

let command =
  Core.Command.group ~summary:"SpecTec command line tools"
    [
      ("elab", elab_command); ("struct", structure_command); ("generate", generate_comamnd) ; ("p4", p4_command);
    ]

let () = Command_unix.run ~version command
