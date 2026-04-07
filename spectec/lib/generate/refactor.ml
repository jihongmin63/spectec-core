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

let change_premise rule = rule

let refactor_rule selected_rule = 
  {relation = selected_rule.relation; rule = change_premise selected_rule.rule}