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

let rec _generalize_paths (spec : spec) (v : Value.t) : (int * string * Value.t Gen.t) list =
  let open Common.Source in
  let t = v.note.typ in
  let root =
    try [(0, "_", gen_of_typ spec (t $ no_region))]
    with _ -> []
  in
  (* For each inner sub-path, build the outer display by embedding the inner
     display as a TextV sentinel, then lift depth and wrap the generator. *)
  let lift_sub_paths inner_paths make_display_v' rebuild =
    List.map (fun (depth, sub_display, sub_gen) ->
      let display =
        Print.string_of_value (Value.make_val t (make_display_v' (Value.text sub_display)))
      in
      (depth + 1, display, Gen.map rebuild sub_gen))
    inner_paths
  in
  let sub_paths =
    match v.it with
    | StructV fields ->
      List.concat_map (fun (i, (_, v_i)) ->
        lift_sub_paths (_generalize_paths spec v_i)
          (fun hole ->
            StructV (List.mapi (fun j (aj, vj) ->
              if j = i then (aj, hole) else (aj, vj)) fields))
          (fun new_vi ->
            Value.make_val t (StructV (List.mapi (fun j (aj, vj) ->
              if j = i then (aj, new_vi) else (aj, vj)) fields))))
      (List.mapi (fun i f -> (i, f)) fields)
    | TupleV vs ->
      List.concat_map (fun (i, vi) ->
        lift_sub_paths (_generalize_paths spec vi)
          (fun hole -> TupleV (List.mapi (fun j vj -> if j = i then hole else vj) vs))
          (fun new_vi ->
            Value.make_val t (TupleV (List.mapi (fun j vj ->
              if j = i then new_vi else vj) vs))))
      (List.mapi (fun i vi -> (i, vi)) vs)
    | CaseV vc ->
      let args = Mixfix.args vc in
      let patch_casev vc args i new_v =
        let _, patched =
          List.fold_left
            (fun (arg_idx, acc) part ->
              match part with
              | Mixfix.Atom a ->
                  (arg_idx, Mixfix.Atom a :: acc)

              | Mixfix.Arg _ ->
                  let v =
                    if arg_idx = i
                    then new_v
                    else List.nth args arg_idx
                  in
                  (arg_idx + 1, Mixfix.Arg v :: acc))
            (0, [])
            vc
        in
        List.rev patched
      in
      List.concat_map (fun (i, _) ->
        let vi = List.nth args i in
        lift_sub_paths (_generalize_paths spec vi)
          (fun hole -> CaseV (patch_casev vc args i hole))
          (fun new_vi -> Value.make_val t (CaseV (patch_casev vc args i new_vi))))
      (List.mapi (fun i vi -> (i, vi)) args)
    | _ -> []
  in
  root @
  List.stable_sort (fun (d1, _, _) (d2, _, _) -> compare d1 d2) sub_paths

let generalize_env spec (counter_env : (id' * value) list) : (string * ((id' * value) list Gen.t)) list =
  let open Common.Source in
  let n = List.length counter_env in
  if n = 0 then []
  else
    let subsets = 
      (* power set of enviornment which denotes what to generalize *)
      List.fold_left
        (fun acc i -> acc @ List.map (fun s -> i :: s) acc)
        [[]] (List.init n Fun.id)
      |> List.filter ((<>) [])
      |> List.sort (fun a b -> compare (List.length b) (List.length a))
    in
    let variable_level_candidates =
      (* generalize the whole input based on subsets *)
      List.filter_map (fun free_set ->
        let var_gen_opts =
          List.mapi (fun j (id_j, v_j) ->
            let g =
              if List.mem j free_set then gen_of_typ spec (v_j.note.typ $ no_region)
              else Gen.return v_j
            in
            Gen.map (fun v -> (id_j, v)) g)
          counter_env
        in
          let gen' = Gen.sequence (List.map Fun.id var_gen_opts) in
          let label =
            String.concat ", " (List.mapi (fun j (id_j, v_j) ->
              if List.mem j free_set then id_j ^ "=" ^ (Print.string_of_typ (v_j.note.typ $ no_region))
              else id_j ^ "=" ^ Print.string_of_value v_j)
            counter_env)
          in
          Some (label, gen'))
      subsets
    in
    let subcomponent_candidates = 
      List.concat_map (fun (i, (_, v_i)) ->
        let sub_paths = List.filter (fun (d, _, _) -> d > 0) (_generalize_paths spec v_i) in
        List.map (fun (_, display, path_gen) ->
          let label =
            String.concat ", " (List.mapi (fun j (id_j, v_j) ->
              if j = i then id_j ^ "=" ^ display
              else id_j ^ "=" ^ Print.string_of_value v_j)
            counter_env)
          in
          let gen' = Gen.map (fun new_vi ->
            List.mapi (fun j (id_j, v_j) ->
              if j = i then (id_j, new_vi) else (id_j, v_j))
            counter_env)
            path_gen
          in
          (label, gen'))
        sub_paths)
      (List.mapi (fun i p -> (i, p)) counter_env)
    in
    variable_level_candidates @ subcomponent_candidates

let show_env (bindings : (id' * value) list) : string =
  String.concat ", "
    (List.map (fun (id, v) -> id ^ "=" ^ Print.string_of_value v) bindings)

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


let call_rel spec rel_id input_vals =
  try `R (Qc_eval_il.run ~max_steps:100
            (module Nop_target) spec rel_id input_vals "")
  with Qc_eval_il.StepLimitExceeded -> `Timeout

let dispatch spec (command : Qc_ir.qc_command) :
    (Test.outcome * Test.opt, error) Stdlib.result =
  match command with
  | Qc_ir.QcProp { name = _; free_vars; generator; prems_rel; goal_rel } ->
    (match (match generator with
            | Some gen_name -> gen_free_vars_manual spec gen_name
            | None -> Ok (gen_free_vars spec free_vars)) with
    | Error _ as e -> e
    | Ok gen ->
      let prop =
        Property.for_all ~shrink:(shrink_env spec) ~generalize:(generalize_env spec) ~show:show_env gen (fun initial_env ->
          let prems_inputs =
            List.map (fun id -> List.assoc id initial_env) prems_rel.Qc_ir.sr_inputs
          in
          match call_rel spec prems_rel.Qc_ir.sr_id prems_inputs with
          | `Timeout | `R (Error _) ->
            Property.of_result Property.Result.nothing
          | `R (Ok (_, output_vals)) ->
            let output_env =
              List.mapi (fun i (id, _) -> (id, List.nth output_vals i))
                prems_rel.Qc_ir.sr_outputs
            in
            let full_env = initial_env @ output_env in
            let goal_inputs =
              List.map (fun id -> List.assoc id full_env) goal_rel.Qc_ir.sr_inputs
            in
            (match call_rel spec goal_rel.Qc_ir.sr_id goal_inputs with
             | `Timeout -> Property.of_result Property.Result.nothing
             | `R (Error _) -> Property.Bool_testable.property false
             | `R (Ok _) -> Property.Bool_testable.property true))
      in
      Ok (Test.quickcheck prop Test.Prop, Test.Prop))
  | Qc_ir.QcGen { name = _; free_vars; generator; prems_rel } ->
    (match (match generator with
            | Some gen_name -> gen_free_vars_manual spec gen_name
            | None -> Ok (gen_free_vars spec free_vars)) with
    | Error _ as e -> e
    | Ok gen ->
      let prop =
        Property.for_all ~show:show_env gen (fun initial_env ->
          let prems_inputs =
            List.map (fun id -> List.assoc id initial_env) prems_rel.Qc_ir.sr_inputs
          in
          match call_rel spec prems_rel.Qc_ir.sr_id prems_inputs with
          | `Timeout | `R (Error _) ->
            Property.of_result Property.Result.nothing
          | `R (Ok (_, output_vals)) ->
            let output_env =
              List.mapi (fun i (id, _) -> (id, List.nth output_vals i))
                prems_rel.Qc_ir.sr_outputs
            in
            let full_env = initial_env @ output_env in
            Property.label (show_env full_env)
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
    | Ok (cmds, synthetic_defs) ->
      let spec_with_synth = spec_il @ synthetic_defs in
      List.fold_left (fun acc cmd ->
        match acc with
        | Error _ -> acc
        | Ok () ->
          let name, mode_label = match cmd with
            | Qc_ir.QcProp { name; _ } -> name, "Test"
            | Qc_ir.QcGen  { name; _ } -> name, "Generation"
          in
          Printf.printf "[Quickcheck %s: %s]\n" name mode_label;
          (match dispatch spec_with_synth cmd with
           | Error _ as e -> e
           | Ok (outcome, opt) -> Test.print_outcome opt outcome; Ok ()))
        (Ok ()) cmds
