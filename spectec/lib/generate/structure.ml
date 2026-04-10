module Il = Lang.Il
open Refactor
open Shared_exp
open Print
open Common.Source

module SharedExp = Map.Make(Int)

type variable = {name : Il.id; iters : iter list; body : int}
(*
type binded_body = STRUCT of Il.id * Il.atom | VARIANT of Il.id * int | LET of Il.exp | REL of string * notexp
type binded = {name : Il.id; body : binded_body; iters : Il.iterexp list}
*)

let search_rel spec id =
  let def = List.find (fun def ->
    match def.it with
    | Il.RelD (relid, _, _, _) -> relid.it = id.it
    | _ -> false
  ) spec in
  match def.it with
  | Il.RelD (_, nottyp, inputs, _) -> nottyp, inputs
  | _ -> assert false

let exp_to_input exp body =
  match filter_id_iter exp with
  | Some (id, iters) -> {name = id; iters = iters; body = body}
  | None -> assert false

let check_total spec funcdef =  
  let _ = spec in
  match funcdef.it with
  | Il.DecD (_, tparams, _, _, _) ->
    if List.length tparams <> 0 then (
      let _ = Format.printf "Currently do not support generic function" in
      assert false
      )
    else (
      let find_structure refactored_clause =
        let args, _, binding_prems, condition_prems = refactored_clause in
        if List.length condition_prems <> 0 then 
          (let _ = Format.printf "Currently do not support functions with side prems" in
          assert false)
        else (
          let hash = ref 0 in
          let emptymap : exp' SharedExp.t = SharedExp.empty in 
          let rec generate_input args map inputs =
            match args with
            | [] -> map, inputs
            | arg :: args ->
              match arg.it with
              | Il.ExpA exp ->
                let updated_map, body = exp_to_hashed map hash exp in
                let input = exp_to_input exp body in
                generate_input args updated_map (input :: inputs)
              | Il.DefA _ ->
                let _ = Format.printf "Currently do not support functional parameter" in
                assert false
          in
          let map, inputs = generate_input args emptymap [] in
          let rec find_structure_aux prems inputs map =
            match prems with
            | [] -> inputs, map
            | prem :: prems -> (
              let updated_inputs, updated_map = (
                match prem.it with
                | Il.RulePr _ ->
                  let _ = Format.printf "Currently do not support Relational call" in
                  assert false
                | Il.LetPr (exp_id, exp_val) -> (
                    match filter_id_iter exp_id with
                    | Some (id, iters) -> (
                        match filter_id_iter exp_val with
                        | Some _ -> (*To Do*) assert false
                        | None -> 
                          let updated_map, body = exp_to_hashed map hash exp_val in
                          ({name = id; iters = iters; body = body} :: inputs), updated_map
                      )
                    | None -> assert false )
                | Il.IfPr _ -> (*To Do*) assert false
                | Il.IterPr _ -> (*To Do*) assert false
                | _ -> assert false
              )
              in
              find_structure_aux prems updated_inputs updated_map
            )
          in 
          let inputs, map = find_structure_aux binding_prems inputs map in
          let _ = List.map (fun input ->
            Format.printf "%s | %d\n" (input.name.it) (input.body)
          ) inputs in
          let _ = Format.printf "--\n" in
          let _ = List.map (fun pair ->
            let key, value = pair in  
            Format.printf "%d | %s\n" key (string_of_exp' value)
          ) (SharedExp.bindings map) in
          (*To-do : filter real inputs only*)
          inputs
        )
      in 
      let refactored_clauses : refactored_clause' list = refactor_function funcdef in
      let _ = List.map (
          fun refactored_clause -> find_structure refactored_clause 
        ) refactored_clauses in
      "false"
    )
  | _ -> assert false