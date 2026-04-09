open Common.Source
module Il = Lang.Il

let ( let* ) = Result.bind

let rec binding_prem prem =
  let rec binding_exp exp =
    match exp.it with
    | Il.SubE _ | MatchE _ -> true
    | IterE (exp, _) -> binding_exp exp
    | _ -> false
  in
  match prem.it with
  | Il.RulePr _ -> true
  | IfPr exp -> binding_exp exp
  | ElsePr -> false
  | LetPr _ -> true
  | IterPr (prem_iter, _) -> binding_prem prem_iter
  | DebugPr _ -> false

let refactor_prems prems =
  let rec _refactor_prems prems binding_prems condition_prems =
    match prems with
    | [] -> binding_prems, condition_prems
    | prem :: prems ->
      let binding_prems, condition_prems = (
        if binding_prem prem then (binding_prems @ [prem]), condition_prems
        else binding_prems, (condition_prems @ [prem])
      ) in
    _refactor_prems prems binding_prems condition_prems
  in _refactor_prems prems [] []

type refactored_clause' = Il.arg list * Il.exp * Il.prem list * Il.prem list
(*type refactored_clause = refactored_clause' phrase*)

let refactor_function funcdef =
  match funcdef.it with
  | Il.DecD (_, _, _, _, clauses) ->
    List.map (fun clause ->
      let args, exp, prems = clause.it in
      let binding_prems, condition_prems = refactor_prems prems in
      args, exp, binding_prems, condition_prems
    ) clauses
  | _ -> assert false
  
let find_function function_id spec = 
  List.find (fun def ->
    match def.it with
    | Il.DecD (id, _, _, _, _) -> id.it = function_id
    | _ -> false  
  ) spec