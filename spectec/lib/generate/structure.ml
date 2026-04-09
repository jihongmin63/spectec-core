module Il = Lang.Il
open Refactor
open Common.Source

type original_variable = {name : Il.id; structure : Il.deftyp}
type binded_varaible = {name : Il.id; structure : Il.deftyp; parent : Il.id}


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
          [], []
        )
      in 
      let is_total arguments_list = 
        false
      in
      let refactored_clauses : refactored_clause' list = refactor_function funcdef in
      let arguments_list, _ = List.split (
        List.map (
          fun refactored_clause -> find_structure refactored_clause 
        ) refactored_clauses
      ) in
      Format.printf (if is_total arguments_list then "%s is total"  else "%s is partial") id.it 
    )
  | _ -> assert false