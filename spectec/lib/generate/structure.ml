module Il = Lang.Il
open Refactor
open Common.Source

type argument_type = PLAIN of Il.exp | DEFTYP of Il.id
type parent_type = REL of Il.id * Il.notexp | LET of Il.id

type original_variable = {expression : argument_type; structure : Il.typ}
type binded_varaible = {expression : argument_type; structure : Il.typ; parent : parent_type}

let search_rel spec id =
  let def = List.find (fun def ->
    match def.it with
    | Il.RelD (relid, _, _, _) -> relid.it = id.it
    | _ -> false
  ) spec in
  match def.it with
  | Il.RelD (_, nottyp, inputs, _) -> nottyp, inputs
  | _ -> assert false

let exp_2_argument_typ exp =
  match exp.it with
  | Il.VarE id -> DEFTYP id
  | _ -> PLAIN exp

let check_total spec funcdef =  
  match funcdef.it with
  | Il.DecD (id, tparams, params, _, _) ->
    if List.length tparams <> 0 then (
      let _ = Format.printf "Currently do not support generic function" in
      assert false
      )
    else (
      let init_types = List.map (fun param ->
        match param.it with
        | Il.ExpP typ -> typ
        | _ -> 
          let _ = Format.printf "Do not support function as parameter" in
          assert false
      ) params in
      let find_structure refactored_clause =
        let args, _, binding_prems, condition_prems = refactored_clause in
        if List.length condition_prems <> 0 then 
          (let _ = Format.printf "Currently do not support functions with side prems" in
          assert false)
        else (
          let arguments = 
            List.map2 (fun arg typ -> 
              match arg.it with
              | Il.ExpA exp -> (
                  {expression = exp_2_argument_typ exp; structure = typ}
                )
              | _ -> assert false
            ) args init_types
          in
          let rec find_structure_aux prems arguments binded_varaibles =
            match prems with
            | [] -> arguments, binded_varaibles
            | prem :: prems -> (
              let updated_arguments, updated_binded_varaibles = (
                match prem.it with
                | Il.RulePr (id, notexp) ->
                  let nottyp, inputs = search_rel spec id in
                  let exps = snd notexp in let typs = snd nottyp.it in
                  let updated_binded_varaibles = List.fold_left (
                    fun binded_varaibles input ->
                    {expression = exp_2_argument_typ (List.nth exps input); structure = List.nth typs input; parent = REL (id, notexp)} :: binded_varaibles
                  ) binded_varaibles inputs in
                  arguments, updated_binded_varaibles
                | Il.LetPr (exp_id, exp_val) -> assert false 
                | Il.IfPr exp -> assert false
                | Il.IterPr (prem, iterexp) -> assert false
                | _ -> assert false
              )
              in
              find_structure_aux prems updated_arguments updated_binded_varaibles
            )
          in 
          find_structure_aux binding_prems arguments []
        )
      in 
      let is_total arguments_list = 
        (* based on init type, construct a set that has to be satisfied *)
        false
      in
      let refactored_clauses : refactored_clause' list = refactor_function funcdef in
      let arguments_list, _ = List.split (
        List.map (
          fun refactored_clause -> find_structure refactored_clause 
        ) refactored_clauses
      ) in
      Format.printf (if is_total arguments_list then "%s is total" else "%s is partial") id.it 
    )
  | _ -> assert false