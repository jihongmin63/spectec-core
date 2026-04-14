open Common.Source
open Il

module SharedExp = Map.Make(struct
  type t = exp
  let compare = compare
end)

type output =
  | MEM of exp SharedExp.t * exp list
  | OTHERWISE 
  | FAIL of string 

let merge_env env =
  (* 
    TO DO
    [] output only inputs 
  *)
  MEM (env, [])

let debug env =
  let pairs = SharedExp.bindings env in
  let _ = Format.printf "[ENV]\n" in
  List.map (fun pair ->
    Format.printf "%s -> %s\n" (Print.string_of_exp (fst pair)) (Print.string_of_exp (snd pair))
  ) pairs

let rec filter_id exp = 
  match exp.it with
  | VarE id -> id
  | IterE (exp, _) -> filter_id exp
  | _ -> assert false

let rec variables_in_exp exp =
  match exp.it with
  | VarE _ -> [exp]
  | UnE (_, _, exp) | UpCastE (_, exp) | DownCastE (_, exp) | SubE (exp, _) | MatchE (exp, _) | LenE exp | DotE (exp, _) -> 
    variables_in_exp exp
  | BinE (_, _, exp_l, exp_r) | CmpE (_, _, exp_l, exp_r) | ConsE (exp_l, exp_r) | CatE (exp_l, exp_r) | MemE (exp_l, exp_r) | IdxE (exp_l, exp_r) -> 
    (variables_in_exp exp_l) @ (variables_in_exp exp_r)
  | ListE exps | TupleE exps ->
    List.concat_map variables_in_exp exps
  | OptE expopt -> (
      match expopt with
      | None -> []
      | Some exp -> variables_in_exp exp
    )
  | CaseE notexp ->
    let _, exps = notexp in
    List.concat_map variables_in_exp exps
  | StrE expfields ->
    let exps = List.map snd expfields in
    List.concat_map variables_in_exp exps
  | SliceE (exp, exp_l, exp_n) ->
    (variables_in_exp exp) @ (variables_in_exp exp_l) @ (variables_in_exp exp_n)
  | UpdE (exp, path, exp_new) ->
    (variables_in_exp exp) @ (variables_in_path path) @ (variables_in_exp exp_new)
  | IterE (exp, iterexp) ->
    let vars = variables_in_exp exp in
    let _, itervars = iterexp in
    vars @ 
    (List.filter_map (fun var ->
      if (List.exists (fun (itervar : var) ->
          let id = filter_id var in
          let iterid, _, _ = itervar in
          id.it = iterid.it) itervars)
      then Some (IterE (var, iterexp) $$ (no_region % (IterT (var.note $ no_region, fst iterexp))))
      else 
      None) vars)
  | BoolE _ | NumE _ | TextE _ | CallE _ -> []
and variables_in_path path =
  match path.it with
  | RootP -> []
  | IdxP (path, exp) ->
    (variables_in_path path) @ (variables_in_exp exp)
  | SliceP (path, exp_l, exp_n) ->
    (variables_in_path path) @ (variables_in_exp exp_l) @ (variables_in_exp exp_n)
  | DotP (path, _) ->
    (variables_in_path path)

let total_check spec envs params =
  (* TO DO *)
  let _ = spec, envs, params in
  false

let is_funcef_total spec (funcdef : (tparam list * param list * clause list)) =
  let tparams, params, clauses = funcdef in
  let _ = ref "" in
  let rec iterate_prems env prems =
    let _ = debug env in
    let prem_binds_variable prem env =
      let rec prem_binds_variable_aux prem env = (
        match prem.it with
        | RulePr _ | IfHoldPr _ | IfNotHoldPr _ -> FAIL ("Function includes relation call " ^ (Print.string_of_prem prem))
        | DebugPr _ -> FAIL "Function includes debug premise"
        | ElsePr -> OTHERWISE
        | IfPr exp -> (* TO DO : match / subtyp / equality *) (
            match exp.it with
            | MatchE (exp, pattern) -> (
              match pattern with
              | CaseP mixop -> (* TO DO *)
              | _ -> let _ = Format.printf "Currently do not support other matching pattern\n" in assert false
              )
            | _ -> FAIL ("Function includes condition premise " ^ (Print.string_of_exp exp))
          )
        | LetPr (exp, exp_val) -> 
          (* TO DO *)
          let updated_env, new_bindings = (
            match SharedExp.find_opt exp_val env with
            | Some value -> 
              let bind_exp exp value env = (
                match exp.it with
                | VarE _ -> (SharedExp.add exp value env), [exp]
                | StrE expfields -> (
                    match value.it with
                    | StrE valfields -> 
                      let exps = List.map snd expfields in 
                      let values = List.map snd valfields in
                      (List.fold_left2 (fun env exp value -> SharedExp.add exp value env) env exps values), exps 
                    | _ -> let _ = Format.printf "Wrong binding pattern\n" in assert false
                  )
                | CaseE notexp -> (
                    match value.it with
                    | CaseE notval ->
                      let _, exps = notexp in
                      let _, values = notval in
                      (List.fold_left2 (fun env exp value -> SharedExp.add exp value env) env exps values), exps 
                    | _ -> let _ = Format.printf "Wrong binding pattern\n" in assert false
                  )
                | IterE (_, _) ->
                  (* TO DO *) assert false
                | _ -> let _ = Format.printf "Wrong binding pattern\n" in assert false
              )
              in
              bind_exp exp value env
            | None -> SharedExp.add exp exp_val env, [exp] (* TO DO *)
          ) in MEM (updated_env, new_bindings)
        | IterPr (prem, _) -> (
            match prem_binds_variable_aux prem env with
            | MEM (updated_env, binded_variables) ->
              (* TO DO : do i have to ascend? *)
              MEM (updated_env, binded_variables)
            | output -> output
          )
      ) in
      prem_binds_variable_aux prem env
    in
    match prems with
    | [] -> merge_env env
    | prem :: prems ->
      match prem_binds_variable prem env with
      | MEM (updated_env, _) -> iterate_prems updated_env prems
      | output -> output
  in
  let iterate_clause (clause : clause) = 
    let args, _, prems = clause.it in
    let init_env : exp SharedExp.t = List.fold_left (fun env arg ->
      match arg.it with
      | ExpA exp -> 
        let vars = variables_in_exp exp in
        List.fold_left (fun env var -> 
          SharedExp.add var var env
        ) env vars
      | DefA _ -> let _ = Format.printf "Do not support function parameter\n" in assert false
    ) SharedExp.empty args in
    iterate_prems init_env prems
  in
  if List.length tparams <> 0 then
    let _ = Format.printf "Currently do not support generic function\n" in assert false
  else
    let envs = List.map iterate_clause clauses in
    (* TO DO : check total from envs *)
    total_check spec envs params 

let are_funcdefs_total spec =
  let funcdefs = List.filter_map (fun def ->
      match def.it with 
      | DecD (_, tparams, params, _, clauses) -> Some (tparams, params, clauses)
      | _ -> None
    ) spec in
  List.map (is_funcef_total spec) funcdefs 