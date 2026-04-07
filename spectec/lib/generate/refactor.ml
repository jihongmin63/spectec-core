open Common.Source
module Il = Lang.Il

let ( let* ) = Result.bind

type selected_rule = { relation : Il.def; rule : Il.rule }

let parse_rule_selector selector =
  match String.split_on_char '.' (String.trim selector) with
  | [ relation_name; rule_name ] when relation_name <> "" && rule_name <> "" ->
      Ok (relation_name, rule_name)
  | _ -> Error "rule selector must have the form [relation].[rule]"

let find_relation_rule spec selector =
  let open Result in
  let* relation_name, rule_name = parse_rule_selector selector in
  match
    List.find_opt
      (fun def ->
        match def.it with
        | Il.RelD (relation_id, _, _, _) -> relation_id.it = relation_name
        | _ -> false)
      spec
  with
  | None ->
      Error (Printf.sprintf "relation '%s' was not found in the IL spec" relation_name)
  | Some relation -> (
      match relation.it with
      | Il.RelD (_, _, _, rules) -> (
          match
            List.find_opt
              (fun rule ->
                let rule_id, _, _ = rule.it in
                rule_id.it = rule_name)
              rules
          with
          | Some rule -> Ok { relation; rule }
          | None ->
              Error
                (Printf.sprintf
                   "rule '%s' was not found in relation '%s'"
                   rule_name relation_name))
      | _ -> assert false)






let refactor_rule spec selected_rule = 
  let relation_has_output : string -> bool = fun id -> 
    match
      List.find_opt
      (fun def ->
        match def.it with
        | Il.RelD (relation_id, _, _, _) -> relation_id.it = id
        | _ -> false)
      spec
    with
    | None -> false
    | Some relation -> (
        match relation.it with
        | RelD (_, nottyp, inputs, _) -> 
          let _, typs = nottyp.it in (List.length typs <> List.length inputs)
        | _ -> false
      )
  in
  let rec binding_prem prem =
    let rec binding_exp exp =
      match exp.it with
      | Il.SubE _ | MatchE _ -> true
      | IterE (exp, _) -> binding_exp exp
      | _ -> false
    in
    match prem.it with
    | Il.RulePr (id, _) -> relation_has_output id.it
    | IfPr exp -> binding_exp exp
    | ElsePr -> false
    | LetPr _ -> true
    | IterPr (prem_iter, _) -> binding_prem prem_iter
    | DebugPr _ -> false
  in
  let _ = spec in
  let rule' = selected_rule.rule.it in
  let _, _, prems = rule' in
  let rec refactor_prems prems binding_prems condition_prems =
    match prems with
    | [] -> binding_prems, condition_prems
    | prem :: prems ->
      let binding_prems, condition_prems = (
        if binding_prem prem then (binding_prems @ [prem]), condition_prems
        else binding_prems, (condition_prems @ [prem])
      ) in
      refactor_prems prems binding_prems condition_prems
  in
  let binding_prems, condition_prems = refactor_prems prems [] [] in
  selected_rule.relation, binding_prems, condition_prems