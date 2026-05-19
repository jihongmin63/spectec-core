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
  let root = [(0, (Print.string_of_value v) ^ "[" ^ Print.string_of_typ (t $ no_region) ^ "]", gen_of_typ spec (t $ no_region))] in
  let rec sequence_gen = function
    | [] -> Gen.return []
    | g :: gs -> Gen.bind g (fun x -> Gen.map (fun xs -> x :: xs) (sequence_gen gs))
  in
  let rec powerset = function
    | [] -> [[]]
    | x :: xs ->
        let tail = powerset xs in
        tail @ List.map (fun acc -> x :: acc) tail
  in
  let get_combinations l = List.filter (fun x -> x <> []) (powerset l) in

  let sub_paths =
    match v.it with
    | StructV fields ->
        let idx_list = List.mapi (fun i _ -> i) fields in
        let combos = get_combinations idx_list in
        let sub_paths_map = List.mapi (fun i (_, vj) -> (i, _generalize_paths spec vj)) fields in
        
        List.concat_map (fun indices ->
          let selected_sub_paths = List.map (fun i -> (i, List.assoc i sub_paths_map)) indices in
          
          let rec cross_product = function
            | [] -> [([], [])]
            | (i, paths) :: rest ->
                List.concat_map (fun (d, s, g) ->
                  List.map (fun (s_acc, g_acc) -> 
                    ((i, (d, s)) :: s_acc, (i, g) :: g_acc)
                  ) (cross_product rest)
                ) paths
          in
          
          List.map (fun (s_env, g_env) ->
            let max_d = List.fold_left (fun acc (_, (d, _)) -> max acc d) 0 s_env in
            
            let display = 
              let fields' = List.mapi (fun j (aj, vj) ->
                match List.assoc_opt j s_env with
                | Some (_, sub_str) -> (aj, Value.text sub_str)
                | None -> (aj, vj)
              ) fields in
              Print.string_of_value (Value.make_val t (StructV fields'))
            in

            let gen =
              let gens = List.map (fun i -> List.assoc i g_env) indices in
              Gen.map (fun values ->
                let val_env = List.combine indices values in
                let fields' = List.mapi (fun j (aj, vj) ->
                  match List.assoc_opt j val_env with
                  | Some new_v -> (aj, new_v)
                  | None -> (aj, vj)
                ) fields in
                Value.make_val t (StructV fields')
              ) (sequence_gen gens)
            in
            (max_d + 1, display, gen)
          ) (cross_product selected_sub_paths)
        ) combos

    | TupleV vs ->
        let idx_list = List.mapi (fun i _ -> i) vs in
        let combos = get_combinations idx_list in
        let sub_paths_map = List.mapi (fun i vi -> (i, _generalize_paths spec vi)) vs in
        
        List.concat_map (fun indices ->
          let selected_sub_paths = List.map (fun i -> (i, List.assoc i sub_paths_map)) indices in
          let rec cross_product = function
            | [] -> [([], [])]
            | (i, paths) :: rest ->
                List.concat_map (fun (d, s, g) ->
                  List.map (fun (s_acc, g_acc) -> ((i, (d, s)) :: s_acc, (i, g) :: g_acc)) (cross_product rest)
                ) paths
          in
          List.map (fun (s_env, g_env) ->
            let max_d = List.fold_left (fun acc (_, (d, _)) -> max acc d) 0 s_env in
            let display = 
              let vs' = List.mapi (fun j vj ->
                match List.assoc_opt j s_env with
                | Some (_, sub_str) -> Value.text sub_str
                | None -> vj
              ) vs in
              Print.string_of_value (Value.make_val t (TupleV vs'))
            in
            let gen =
              let gens = List.map (fun i -> List.assoc i g_env) indices in
              Gen.map (fun values ->
                let val_env = List.combine indices values in
                let vs' = List.mapi (fun j vj ->
                  match List.assoc_opt j val_env with
                  | Some new_v -> new_v
                  | None -> vj
                ) vs in
                Value.make_val t (TupleV vs')
              ) (sequence_gen gens)
            in
            (max_d + 1, display, gen)
          ) (cross_product selected_sub_paths)
        ) combos

    | CaseV vc ->
        let args = Mixfix.args vc in
        let idx_list = List.mapi (fun i _ -> i) args in
        let combos = get_combinations idx_list in
        let sub_paths_map = List.mapi (fun i _ -> (i, _generalize_paths spec (List.nth args i))) args in
        
        List.concat_map (fun indices ->
          let selected_sub_paths = List.map (fun i -> (i, List.assoc i sub_paths_map)) indices in
          let rec cross_product = function
            | [] -> [([], [])]
            | (i, paths) :: rest ->
                List.concat_map (fun (d, s, g) ->
                  List.map (fun (s_acc, g_acc) -> ((i, (d, s)) :: s_acc, (i, g) :: g_acc)) (cross_product rest)
                ) paths
          in
          List.map (fun (s_env, g_env) ->
            let max_d = List.fold_left (fun acc (_, (d, _)) -> max acc d) 0 s_env in
            
            let display_args = 
              let _, patched = List.fold_left (fun (arg_idx, acc) part ->
                match part with
                | Mixfix.Atom a -> (arg_idx, Mixfix.Atom a :: acc)
                | Mixfix.Arg _ ->
                    let v = match List.assoc_opt arg_idx s_env with
                      | Some (_, sub_str) -> Value.text sub_str
                      | None -> List.nth args arg_idx
                    in
                    (arg_idx + 1, Mixfix.Arg v :: acc)
              ) (0, []) vc in List.rev patched
            in
            let display = Print.string_of_value (Value.make_val t (CaseV display_args)) in

            let gen =
              let gens = List.map (fun i -> List.assoc i g_env) indices in
              Gen.map (fun values ->
                let val_env = List.combine indices values in
                let _, patched = List.fold_left (fun (arg_idx, acc) part ->
                  match part with
                  | Mixfix.Atom a -> (arg_idx, Mixfix.Atom a :: acc)
                  | Mixfix.Arg _ ->
                      let v = match List.assoc_opt arg_idx val_env with
                        | Some new_v -> new_v
                        | None -> List.nth args arg_idx
                      in
                      (arg_idx + 1, Mixfix.Arg v :: acc)
                ) (0, []) vc in List.rev patched
              ) (sequence_gen gens)
              |> Gen.map (fun patched_args -> Value.make_val t (CaseV patched_args))
            in
            (max_d + 1, display, gen)
          ) (cross_product selected_sub_paths)
        ) combos
    | _ -> []
  in
  let generalization_score s = 
    String.fold_left (fun acc c -> if c = '[' then acc + 1 else acc) 0 s 
  in
  root @
  List.stable_sort (fun (_, s1, _) (_, s2, _) -> 
    compare (generalization_score s2) (generalization_score s1)
  ) sub_paths

let generalize_env spec (counter_env : (id' * value) list) : (string * ((id' * value) list Gen.t)) list =
  let n = List.length counter_env in
  if n = 0 then []
  else
    List.concat_map (fun (i, (_, v_i)) ->
      let sub_paths = (_generalize_paths spec v_i) in
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
