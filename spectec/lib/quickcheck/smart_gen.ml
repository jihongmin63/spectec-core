open Lang.Il
open Il_gen
open Prem_env

let gen_free_vars (spec_il : spec) (free_vars : Qc_ir.ir_var list) :
    (id' * value) list Gen.t =
  Gen.sequence
    (List.map
       (fun v ->
         Gen.map
           (fun value -> (v.Qc_ir.iv_id, value))
           (gen_of_typ spec_il v.Qc_ir.iv_typ))
       free_vars)

let rec gen_of_resolved_exp_inner (seen : string list) (spec : spec) (env : prem_env) (e : exp) : Value.t Gen.t =
  let open Common.Source in
  let open Gen in
  (* If this is a VarE we're already resolving, cycle detected — generate freely *)
  let cycle = match e.it with VarE id -> List.mem id.it seen | _ -> false in
  if cycle then gen_of_typ spec (e.note $ e.at)
  else
  let seen' = match e.it with VarE id -> id.it :: seen | _ -> seen in
  let e = resolve_prem_env env e in
  let recurse sub = gen_of_resolved_exp_inner seen' spec env sub in
  match e.it with
  | BoolE b   -> return (Value.make_val e.note (BoolV b))
  | NumE n    -> return (Value.make_val e.note (NumV n))
  | TextE s   -> return (Value.make_val e.note (TextV s))
  | VarE _    -> gen_of_typ spec (e.note $ e.at)
  | CaseE ne  ->
    let mixop = Mixfix.to_mixop ne in
    let* vs = Gen.sequence (List.map recurse (Mixfix.args ne)) in
    return (Value.make_val e.note (CaseV (Mixfix.fill mixop vs)))
  | TupleE es ->
    let* vs = Gen.sequence (List.map recurse es) in
    return (Value.make_val e.note (TupleV vs))
  | StrE fields ->
    let* vfields = Gen.sequence (List.map (fun (atom, ei) ->
      let* v = recurse ei in return (atom, v)) fields) in
    return (Value.make_val e.note (StructV vfields))
  | OptE None      -> return (Value.make_val e.note (OptV None))
  | OptE (Some ei) ->
    let* v = recurse ei in
    return (Value.make_val e.note (OptV (Some v)))
  | ListE es ->
    let* vs = Gen.sequence (List.map recurse es) in
    return (Value.make_val e.note (ListV vs))
  | _ -> gen_of_typ spec (e.note $ e.at)

let gen_of_resolved_exp (spec : spec) (env : prem_env) (e : exp) : Value.t Gen.t =
  gen_of_resolved_exp_inner [] spec env e

let smart_gen_free_vars (spec_il : spec) (free_vars : Qc_ir.ir_var list) (prems : prem list) :
    (id' * value) list Gen.t =
  let open Common.Source in
  (* Pair each RulePr with the list of applicable rules from the spec *)
  let rule_prem_candidates =
    List.filter_map (fun prem ->
      match prem.it with
      | RulePr (rel_id, _) ->
        List.find_map (fun def ->
          match def.it with
          | RelD (id, _, _, rules) when id.it = rel_id.it -> Some (prem, rules)
          | _ -> None)
        spec_il
      | _ -> None)
    prems
  in
  (* Cartesian product: one (call_prem, chosen_rule) per RulePr *)
  let candidates =
    List.fold_right
      (fun (call_prem, rules) acc ->
        List.concat_map (fun rule ->
          List.map (fun rest -> (call_prem, rule) :: rest) acc)
        rules)
      rule_prem_candidates [[]]
  in
  (* Non-RulePr premises are always included *)
  let base_prems =
    List.filter (fun p -> match p.it with RulePr _ -> false | _ -> true) prems
  in
  (* Build LetPr connections from RulePr call args to the chosen rule's
     conclusion args, then append the rule's own premises.  The rule's
     premises carry the actual pattern constraints (IfPr/MatchE, LetPr)
     that narrow the command/expression variable to a specific constructor. *)
  let connection_prems (call_prem : prem) (rule : rule) : prem list =
    let (_, rule_notexp, rule_prems) = rule.it in
    match call_prem.it with
    | RulePr (_, call_notexp) ->
      let call_args = Mixfix.args call_notexp in
      let rule_args = Mixfix.args rule_notexp in
      let n = min (List.length call_args) (List.length rule_args) in
      let connects = List.init n (fun i ->
        LetPr (List.nth call_args i, List.nth rule_args i) $ no_region) in
      connects @ rule_prems
    | _ -> []
  in
  (* For each candidate, build prem_env and reject contradictory combinations *)
  let filtered_envs =
    List.filter_map (fun candidate ->
      let extra = List.concat_map (fun (call_prem, rule) ->
        connection_prems call_prem rule) candidate
      in
      let env = env_of_prems spec_il (base_prems @ extra) in
      if has_contradiction env then None else Some env)
    candidates
  in
  let gen_from_env env =
    Gen.sequence
      (List.map (fun v ->
         let var_exp = VarE (v.Qc_ir.iv_id $ no_region) $$ (no_region % v.Qc_ir.iv_typ.it) in
         Gen.map (fun value -> (v.Qc_ir.iv_id, value))
           (gen_of_resolved_exp spec_il env var_exp))
      free_vars)
  in
  match filtered_envs with
  | [] -> gen_free_vars spec_il free_vars
  | envs -> Gen.oneof (List.map gen_from_env envs)
