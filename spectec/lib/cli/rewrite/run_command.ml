open Spectec
open Common

type source_kind = Program | P4 of string
type input = Application of string * Lang.Il.value list | Raw of string

let sources ~p4 ~includes ~start =
  List.map
    (fun filename ->
      ( filename,
        P4 filename,
        fun () ->
          Result.map
            (fun value -> Application ("Program_ok", [ value ]))
            (Targets_p4.P4.parse_for_run ~includes filename) ))
    p4
  @
  match start with
  | Some term -> [ ("--start", Program, fun () -> Ok (Raw term)) ]
  | None -> []

let encode ~structural { Rewrite.spec; system } = function
  | Raw term -> term
  | Application (relation, values) ->
      if structural then Rewrite.To_mfe.start_app spec system relation values
      else
        Rewrite.To_maude.meta_start_app spec system relation
          (List.map (Rewrite.To_maude.meta_term_of_value spec) values)

(* A malformed source must leave the other programs in the batch runnable.
   Frontends report both Error values and exceptions, including lexer errors. *)
let resolve encode (label, kind, build) =
  let outcome =
    try Result.map encode (build ())
    with exn -> Error (Error.UnhandledException (Printexc.to_string exn))
  in
  let outcome =
    Result.map_error
      (fun error ->
        Diagnostic.Render.render_bag ~ansi:Diagnostic.Ansi.plain
          (Error.to_diagnostics error))
      outcome
  in
  (label, kind, outcome)

let rec stitch resolved results =
  match resolved with
  | [] -> []
  | (label, kind, Error message) :: rest ->
      (label, kind, Rewrite.Maude_run.Error message) :: stitch rest results
  | (label, kind, Ok _) :: rest -> (
      match results with
      | result :: more -> (label, kind, result) :: stitch rest more
      | [] -> invalid_arg "Maude batch returned fewer results than start terms")

let compare_p4 ~includes ~spec_il { Rewrite.spec; system } filename result =
  match result with
  | Rewrite.Maude_run.Reduced term -> (
      let input =
        { Targets_p4.P4.Typecheck.includes; filename; expect = Task.Positive }
      in
      match
        eval_task (module Targets_p4.P4.Typecheck) ~sl_mode:false ~spec_il input
      with
      | Error _ -> ("result: interp FAILED (not compared)", false)
      | Ok interp_values -> (
          try
            let interp_values = Rewrite.Of_maude.canonicalize interp_values in
            let maude_values =
              Rewrite.Of_maude.values_of_result spec ~rel:"Program_ok" ~system
                term
              |> Rewrite.Of_maude.canonicalize
            in
            if Lang.Il.Eq.eq_values interp_values maude_values then
              ("result: MATCH", false)
            else
              let show values =
                String.concat ", "
                  (List.map Lang.Il.Print.string_of_value values)
              in
              ( Printf.sprintf "result: MISMATCH\n  interp: %s\n  maude:  %s"
                  (show interp_values) (show maude_values),
                true )
          with Rewrite.Of_maude.Parse_error message ->
            (Printf.sprintf "result: decode error: %s" message, false)))
  | _ -> ("result: not reduced (not compared)", false)

let render ~compare ~check_p4 outcomes =
  let rendered =
    List.map
      (fun (label, kind, result) ->
        let base = Rewrite.Maude_run.string_of_result result in
        let body, mismatch =
          match kind with
          | P4 filename when check_p4 ->
              let comparison, mismatch = compare filename result in
              (base ^ "\n" ^ comparison, mismatch)
          | _ -> (base, false)
        in
        (label, body, mismatch || Rewrite.Maude_run.is_failure result))
      outcomes
  in
  let text =
    match rendered with
    | [ (_, body, _) ] -> body
    | _ ->
        List.map
          (fun (label, body, _) -> Printf.sprintf "=== %s ===\n%s" label body)
          rendered
        |> String.concat "\n"
  in
  (text, List.exists (fun (_, _, failed) -> failed) rendered)

let run ~filenames ~structural ~start ~p4 ~includes ~timeout ~check_p4 =
  let* spec_il = load_spec filenames in
  let target = if structural then Rewrite.Analysis else Rewrite.Execution in
  let compiled = Rewrite.compile ~target spec_il in
  let { Rewrite.spec; system } = compiled in
  let module_text =
    if structural then
      Rewrite.To_mfe.module_of_system ~full_maude:false spec system
    else Rewrite.To_maude.module_of_system spec system
  in
  let resolved =
    sources ~p4 ~includes ~start
    |> List.map (resolve (encode ~structural compiled))
  in
  let starts =
    List.filter_map
      (fun (_, _, outcome) ->
        match outcome with Ok term -> Some term | Error _ -> None)
      resolved
  in
  let defined_heads =
    if structural then
      List.map Rewrite.Maude_ident.id
        (Rewrite.Rewrite_system.defined_heads system)
    else Rewrite.To_maude.maude_defined_heads system
  in
  let run_batch =
    if structural then Rewrite.Maude_run.run_batch_direct
    else Rewrite.Maude_run.run_batch
  in
  let results = run_batch ~timeout ~defined_heads ~module_text ~starts () in
  Ok
    (render
       ~compare:(compare_p4 ~includes ~spec_il compiled)
       ~check_p4 (stitch resolved results))

let command =
  Core.Command.basic ~summary:"run a translated spec in Maude"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and structural =
    flag "--structural" no_arg
      ~doc:" directly reduce P4 input with the structural analysis module"
  and start =
    flag "--start" (optional string)
      ~doc:"TERM native Maude META-TERM to reduce; may accompany P4 inputs"
  and p4 =
    flag "--p4" (listed string)
      ~doc:"FILE parse and typecheck a P4 program (repeatable; batched)"
  and includes =
    flag "-i" (listed string) ~doc:"DIR P4 include path (with --p4)"
  and timeout =
    flag "--timeout"
      (optional_with_default 0 int)
      ~doc:"S Maude time limit in seconds (default 0: no limit)"
  and check_p4 =
    flag "--check-p4" no_arg
      ~doc:" compare each P4 result with the interpreter (MATCH/MISMATCH)"
  in
  fun () ->
    require (start <> None || p4 <> []) "run needs --start TERM or --p4 FILE";
    require
      ((not structural) || (start = None && p4 <> []))
      "--structural requires --p4 input and excludes --start";
    require ((not check_p4) || p4 <> []) "--check-p4 requires --p4";
    Cli.Error_handling.guard ~color ~on_ok:(fun (out, failed) ->
        Format.printf "%s\n" out;
        if failed then exit 1)
    @@ fun () ->
    run ~filenames ~structural ~start ~p4 ~includes ~timeout ~check_p4
