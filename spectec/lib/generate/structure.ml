module Il = Lang.Il
open Refactor
open Common.Source

module SharedExp = Map.Make(Int)

type input = {name : Il.id; iters : iter list; body : int}
(*
type binded_body = STRUCT of Il.id * Il.atom | VARIANT of Il.id * int | LET of Il.exp | REL of string * notexp
type binded = {name : Il.id; body : binded_body; iters : Il.iterexp list}
*)

let rec exp_to_hased map hash exp = 
  hash := !hash + 1 in
  let hashval = !hash in
  match exp with
  | Il.BoolE bool -> SharedExp.add hashval (BoolS bool) map
  | NumE num -> SharedExp.add hashval (NumS num) map
  | TextE text -> SharedExp.add hashval (TextS text) map
  | VarE id -> SharedExp.add hashval (VarS id.it) map
  | UnE (unop, optyp, exp) -> 
    let updated_map = exp_to_hased map hash exp in
    SharedExp.add hashval (UnS (unop, optyp, hashval + 1)) updated_map
  | BinE (binop, optyp, exp1, exp) ->
    let updated_map1 = exp_to_hased map hash exp1 in
    let hashval' = !hash in
    let updated_map2 = exp_to_hased updated_map1 hash exp2 in
    SharedExp.add hashval (BinS (binop, optyp, hashval + 1, hashval' + 1)) updated_map2
  | CmpE (cmpop, optyp, exp1, exp2) ->
    let updated_map1 = exp_to_hased map hash exp1 in
    let hashval' = !hash in
    let updated_map2 = exp_to_hased updated_map1 hash exp2 in
    SharedExp.add hashval (CmpS (cmpop, optyp, hashval + 1, hashval' + 1)) updated_map2
  | UpCastE typ * exp                  (* exp as typ *)
  | DownCastE typ * exp                (* exp as typ *)
  | SubE exp * typ                     (* exp `<:` typ *)
  | MatchE exp * pattern               (* exp `matches` pattern *)
  | TupleE exp list                    (* `(` exp* `)` *)
  | CaseE notexp                       (* notexp *)
  | StrE (atom * exp) list             (* { expfield* } *)
  | OptE exp option                    (* exp? *)
  | ListE exp list                     (* `[` exp* `]` *)
  | ConsE exp * exp                    (* exp `::` exp *)
  | CatE exp * exp                     (* exp `++` exp *)
  | MemE exp * exp                     (* exp `<-` exp *)
  | LenE exp                           (* `|` exp `|` *)
  | DotE exp * atom                    (* exp.atom *)
  | IdxE exp * exp                     (* exp `[` exp `]` *)
  | SliceE exp * exp * exp             (* exp `[` exp `:` exp `]` *)
  | UpdE exp * path * exp              (* exp `[` path `=` exp `]` *)
  | CallE id * targ list * arg list    (* $id`<` targ* `>``(` arg* `)` *)
  | HoldE id * notexp                  (* id `:` notexp `holds` *)
  | IterE exp * iterexp
  ()
and arg_to_hased map arg arg = ()


let search_rel spec id =
  let def = List.find (fun def ->
    match def.it with
    | Il.RelD (relid, _, _, _) -> relid.it = id.it
    | _ -> false
  ) spec in
  match def.it with
  | Il.RelD (_, nottyp, inputs, _) -> nottyp, inputs
  | _ -> assert false

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
            | Il.ExpA exp -> (
                let rec find_id exp = 
                  match exp.it with
                  | Il.VarE id -> id, []
                  | Il.IterE (exp, iterexp) -> 
                    let id, iters = find_id exp in
                    id, iters @ [iterexp]
                  | _ -> assert false
                in
                let id, iters = find_id exp in
                {name = id; body = exp; iters = iters})
            | _ -> assert false
          ) args init_typs
          in
          let rec find_structure_aux prems inputs bindeds =
            match prems with
            | [] -> inputs, bindeds
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
          find_structure_aux binding_prems inputs []
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