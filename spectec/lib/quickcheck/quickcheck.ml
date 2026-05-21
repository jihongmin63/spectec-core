open Il_gen
open Lang.Il

type error =
  | ParseError of string
  | ElabError of string
  | NoManualGenerator of string

type 'a result = ('a, error) Stdlib.result

let error_to_string = function
  | ParseError msg -> Printf.sprintf "parse error: %s" msg
  | ElabError msg -> Printf.sprintf "elaboration error: %s" msg
  | NoManualGenerator name ->
    Printf.sprintf
      "quickcheck: no manual generator named '%s'. \
       Add a case in manual_gen.ml gen_inputs." name

let error_to_diagnostic e =
  Diagnostic.error ~source:"quickcheck" Common.Source.no_region
    (error_to_string e)

module Nop_target : Interp.Target.S = struct
  let builtins = []
  let handler f =
    let vid_counter = ref 0 in
    Value.GlobalVidProvider.set (fun () ->
      let v = !vid_counter in incr vid_counter; v);
    f ()
  let is_impure_func _ = false
  let is_impure_rel _ = false
  let state_version = ref 0
end

let shrink_env spec (env : (id' * value) list) : (id' * value) list list =
  let shrink_value = shrink spec in
  List.concat_map (fun (i, (_, vi)) ->
    List.map (fun vi' ->
      List.mapi (fun j (idj, vj) -> if j = i then (idj, vi') else (idj, vj)) env)
    (shrink_value vi))
  (List.mapi (fun i p -> (i, p)) env)

let gen_free_vars (spec_il : spec) (free_vars : Qc_ir.ir_var list) :
    (id' * value) list Gen.t =
  Gen.sequence
    (List.map
       (fun v ->
         Gen.map
           (fun value -> (v.Qc_ir.iv_id, value))
           (gen_of_typ spec_il v.Qc_ir.iv_typ))
       free_vars)

let gen_free_vars_manual (spec_il : spec) (name : string) :
    ((id' * value) list Gen.t, error) Stdlib.result =
  match Manual_gen.gen_inputs spec_il name with
  | Some gen -> Ok gen
  | None -> Error (NoManualGenerator name)

let _smart_gen_free_vars (_spec_il : spec) (_free_vars : Qc_ir.ir_var list) (_prems : prem list) :
    (id' * value) list Gen.t =
  failwith "not implemented"

let call_prems spec env (p : Qc_ir.qc_prems) =
  try `R (Qc_eval_il.run_prems ~max_steps:100
            (module Nop_target) spec env p.Qc_ir.qp_prems p.Qc_ir.qp_outputs)
  with Qc_eval_il.StepLimitExceeded -> `Timeout

let call_goal spec env (goal : prem) =
  try `R (Qc_eval_il.run_prems ~max_steps:100
            (module Nop_target) spec env [goal] [])
  with Qc_eval_il.StepLimitExceeded -> `Timeout

let dispatch spec (command : Qc_ir.qc_command) :
    (Test.outcome * Test.opt, error) Stdlib.result =
  match command with
  | Qc_ir.QcProp { name = _; free_vars; generator; generalize; prems; goal } ->
    (match (match generator with
            | Some gen_name -> gen_free_vars_manual spec gen_name
            | None -> Ok (gen_free_vars spec free_vars)) with
    | Error _ as e -> e
    | Ok gen ->
      let generalize_fn = if generalize then Generalize.generalize_env spec else fun _ -> [] in
      let prop =
        Property.for_all ~shrink:(shrink_env spec) ~generalize:generalize_fn ~show:Generalize.show_env gen (fun initial_env ->
          match call_prems spec initial_env prems with
          | `Timeout | `R (Error _) ->
            Property.of_result Property.Result.nothing
          | `R (Ok output_vals) ->
            let output_env =
              List.mapi (fun i (id, _) -> (id, List.nth output_vals i))
                prems.Qc_ir.qp_outputs
            in
            let full_env = initial_env @ output_env in
            (match call_goal spec full_env goal with
             | `Timeout -> Property.of_result Property.Result.nothing
             | `R (Error _) -> Property.Bool_testable.property false
             | `R (Ok _) -> Property.Bool_testable.property true))
      in
      Ok (Test.quickcheck prop Test.Prop, Test.Prop))
  | Qc_ir.QcGen { name = _; free_vars; generator; prems } ->
    (match (match generator with
            | Some gen_name -> gen_free_vars_manual spec gen_name
            | None -> Ok (gen_free_vars spec free_vars)) with
    | Error _ as e -> e
    | Ok gen ->
      let prop =
        Property.for_all ~show:Generalize.show_env gen (fun initial_env ->
          match call_prems spec initial_env prems with
          | `Timeout | `R (Error _) ->
            Property.of_result Property.Result.nothing
          | `R (Ok output_vals) ->
            let output_env =
              List.mapi (fun i (id, _) -> (id, List.nth output_vals i))
                prems.Qc_ir.qp_outputs
            in
            let full_env = initial_env @ output_env in
            Property.label (Generalize.show_env full_env)
              (Property.of_result (Property.Result.with_ok true)))
      in
      let config = { Test.default_config with Test.max_size = 5 } in
      Ok (Test.quickcheck ~config:config prop Test.Gen, Test.Gen))

let quickcheck_file spec_il path : unit result =
  match Qc_parse.parse_file path with
  | Error msg -> Error (ParseError msg)
  | Ok ast ->
    match Qc_elab.elaborate spec_il ast with
    | Error msg -> Error (ElabError msg)
    | Ok cmds ->
      List.fold_left (fun acc cmd ->
        match acc with
        | Error _ -> acc
        | Ok () ->
          let name, mode_label = match cmd with
            | Qc_ir.QcProp { name; _ } -> name, "Test"
            | Qc_ir.QcGen  { name; _ } -> name, "Generation"
          in
          Printf.printf "[Quickcheck %s: %s]\n" name mode_label;
          (match dispatch spec_il cmd with
           | Error _ as e -> e
           | Ok (outcome, opt) -> Test.print_outcome opt outcome; Ok ()))
        (Ok ()) cmds
