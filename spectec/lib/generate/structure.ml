module Il = Lang.Il
open Refactor
open Common.Source


type input = { name : Il.id; body : Il.exp(*To Do*)}
type binded = {(*To Do*)} 

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
      let init_typs = List.map (fun param ->
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
          let inputs : input list = List.map2 (fun arg init_typ ->
            match arg.it with
            | Il.ExpA (Il.VarE id) -> {name = id; body = Il.VarE id}
            | _ -> assert false
          ) args init_typs
          in
          let rec find_structure_aux prems inputs bindeds =
            match prems with
            | [] -> arguments, binded_varaibles
            | prem :: prems -> (
              let updated_inputs, updated_bindeds = (
                match prem.it with
                | Il.RulePr (id, notexp) -> (*To Do*) assert false 
                | Il.LetPr (exp_id, exp_val) -> (*To Do*) assert false 
                | Il.IfPr exp -> (*To Do*) assert false
                | Il.IterPr (prem, iterexp) -> (*To Do*) assert false
                | _ -> assert false
              )
              in
              find_structure_aux prems updated_inputs updated_bindeds
            )
          in 
          find_structure_aux binding_prems arguments []
        )
      in 
      let is_total inputs_list = 
        (* To Do *)
        false
      in
      let refactored_clauses : refactored_clause' list = refactor_function funcdef in
      let inputs_list, _ = List.split (
        List.map (
          fun refactored_clause -> find_structure refactored_clause 
        ) refactored_clauses
      ) in
      Format.printf (if is_total inputs_list then "%s is total" else "%s is partial") id.it 
    )
  | _ -> assert false