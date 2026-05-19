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

let rec sequence_gen = function
  | [] -> Gen.return []
  | g :: gs -> Gen.bind g (fun x -> Gen.map (fun xs -> x :: xs) (sequence_gen gs))

let rec cross_product = function
  | [] -> [([], [])]
  | (i, paths) :: rest ->
      List.concat_map (fun (d, s, g) ->
        List.map (fun (s_acc, g_acc) ->
          ((i, (d, s)) :: s_acc, (i, g) :: g_acc)
        ) (cross_product rest)
      ) paths

let rec powerset = function
  | [] -> [[]]
  | x :: xs ->
      let rest = powerset xs in
      rest @ List.map (fun s -> x :: s) rest

let nonempty_subsets l = List.filter (fun s -> s <> []) (powerset l)

let build_generalizations sub_paths_map combos make_display make_gen =
  List.concat_map (fun indices ->
    let selected = List.map (fun i -> (i, List.assoc i sub_paths_map)) indices in
    List.map (fun (s_env, g_env) ->
      (* Min depth across positions: "deep in ALL positions" > "deep in one position only".
         This ensures [nat]+[bool] (min=1) ranks above [nat]+[expr] (min=0). *)
      let min_d = List.fold_left (fun acc (_, (d, _)) -> min acc d) max_int s_env in
      let gens = List.map (fun i -> List.assoc i g_env) indices in
      let gen = Gen.map (fun values -> make_gen (List.combine indices values)) (sequence_gen gens) in
      (min_d + 1, make_display s_env, gen)
    ) (cross_product selected)
  ) combos

let patch_mixfix lookup fallback vc =
  let _, patched = List.fold_left (fun (arg_idx, acc) part ->
    match part with
    | Mixfix.Atom a -> (arg_idx, Mixfix.Atom a :: acc)
    | Mixfix.Arg _ ->
        let v = match lookup arg_idx with Some v -> v | None -> fallback arg_idx in
        (arg_idx + 1, Mixfix.Arg v :: acc)
  ) (0, []) vc in
  List.rev patched

let rec _generalize_paths (spec : spec) (v : Value.t) : (int * string * Value.t Gen.t) list =
  let open Common.Source in
  let t = v.note.typ in
  let root = [(0, "[" ^ Print.string_of_typ (t $ no_region) ^ "]", gen_of_typ spec (t $ no_region))] in

  let sub_paths =
    match v.it with
    | StructV fields ->
        let sub_paths_map = List.mapi (fun i (_, vj) -> (i, _generalize_paths spec vj)) fields in
        let combos = nonempty_subsets (List.mapi (fun i _ -> i) fields) in
        let make_display s_env =
          let fields' = List.mapi (fun j (aj, vj) ->
            match List.assoc_opt j s_env with
            | Some (_, sub_str) -> (aj, Value.text sub_str)
            | None -> (aj, vj)
          ) fields in
          Print.string_of_value (Value.make_val t (StructV fields'))
        in
        let make_gen val_env =
          let fields' = List.mapi (fun j (aj, vj) ->
            match List.assoc_opt j val_env with
            | Some new_v -> (aj, new_v)
            | None -> (aj, vj)
          ) fields in
          Value.make_val t (StructV fields')
        in
        build_generalizations sub_paths_map combos make_display make_gen

    | TupleV vs ->
        let sub_paths_map = List.mapi (fun i vi -> (i, _generalize_paths spec vi)) vs in
        let combos = nonempty_subsets (List.mapi (fun i _ -> i) vs) in
        let make_display s_env =
          let vs' = List.mapi (fun j vj ->
            match List.assoc_opt j s_env with
            | Some (_, sub_str) -> Value.text sub_str
            | None -> vj
          ) vs in
          Print.string_of_value (Value.make_val t (TupleV vs'))
        in
        let make_gen val_env =
          let vs' = List.mapi (fun j vj ->
            match List.assoc_opt j val_env with
            | Some new_v -> new_v
            | None -> vj
          ) vs in
          Value.make_val t (TupleV vs')
        in
        build_generalizations sub_paths_map combos make_display make_gen

    | CaseV vc ->
        let args = Mixfix.args vc in
        let sub_paths_map = List.mapi (fun i vi -> (i, _generalize_paths spec vi)) args in
        let combos = nonempty_subsets (List.mapi (fun i _ -> i) args) in
        let fallback i = List.nth args i in
        let make_display s_env =
          let display_args = patch_mixfix
            (fun i -> match List.assoc_opt i s_env with Some (_, s) -> Some (Value.text s) | None -> None)
            fallback vc
          in
          Print.string_of_value (Value.make_val t (CaseV display_args))
        in
        let make_gen val_env =
          let patched_args = patch_mixfix (fun i -> List.assoc_opt i val_env) fallback vc in
          Value.make_val t (CaseV patched_args)
        in
        build_generalizations sub_paths_map combos make_display make_gen

    | _ -> []
  in
  let generalization_score s =
    String.fold_left (fun acc c -> if c = '[' then acc + 1 else acc) 0 s
  in
  root @
  (* Primary: score descending (more positions generalized = tried first by generalize_loop).
     Secondary: depth descending so that "deep in all positions" (e.g. [nat]+[bool])
     beats "deep in one, shallow in another" (e.g. [nat]+[expr]) within the same score. *)
  List.stable_sort (fun (d1, s1, _) (d2, s2, _) ->
    let c = compare (generalization_score s2) (generalization_score s1) in
    if c <> 0 then c else compare d2 d1
  ) sub_paths

let show_env (bindings : (id' * value) list) : string =
  String.concat ", "
    (List.map (fun (id, v) -> id ^ "=" ^ Print.string_of_value v) bindings)

let generalize_env spec (counter_env : (id' * value) list) : (string * ((id' * value) list Gen.t)) list =
  let n = List.length counter_env in
  if n = 0 then []
  else
    let candidates =
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
    in
    List.map (fun (s, gens) -> ((show_env counter_env) ^ "\n  (Generalized)\n  " ^ s, gens)) candidates

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
  | Qc_ir.QcProp { name = _; free_vars; generator; generalize; prems_rel; goal_rel } ->
    (match (match generator with
            | Some gen_name -> gen_free_vars_manual spec gen_name
            | None -> Ok (gen_free_vars spec free_vars)) with
    | Error _ as e -> e
    | Ok gen ->
      let generalize_fn = if generalize then generalize_env spec else fun _ -> [] in
      let prop =
        Property.for_all ~shrink:(shrink_env spec) ~generalize:generalize_fn ~show:show_env gen (fun initial_env ->
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
