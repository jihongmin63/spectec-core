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

(* Look up the field types of a constructor mixop from the spec.
   Returns the typ list of the matching typcase, or [] if not found. *)
let field_typs_of_mixop spec (exp : exp) (target_mixop : mixop) =
  match exp.note with
  | VarT (id, _) ->
    let deftyp_opt = List.find_map (fun def ->
      match def.it with
      | TypD (tid, _, deftyp) when tid.it = id.it -> Some deftyp
      | _ -> None
    ) spec in
    (match deftyp_opt with
    | Some dt -> (match dt.it with
      | VariantT cases ->
        (match List.find_opt (fun (nottyp, _) ->
          let (m, _) = nottyp.it in m = target_mixop
        ) cases with
        | Some (nottyp, _) -> let (_, typs) = nottyp.it in typs
        | None -> [])
      | _ -> [])
    | None -> [])
  | _ -> []

let is_funcdef_total spec (funcdef : (tparam list * param list * clause list)) =
  let tparams, params, clauses = funcdef in
  let dummy = ref 0 in
  let fresh_dummy (typ : typ') : exp =
    let id = ("_dummy#" ^ string_of_int !dummy) $ no_region in
    incr dummy;
    VarE id $$ (no_region % typ)
  in
  let rec iterate_prems env prems =
    let _ = debug env in
    let prem_binds_variable prem env =
      let rec prem_binds_variable_aux prem env =
        match prem.it with
        | RulePr _ | IfHoldPr _ | IfNotHoldPr _ -> FAIL ("Function includes relation call " ^ (Print.string_of_prem prem))
        | DebugPr _ -> FAIL "Function includes debug premise"
        | ElsePr -> OTHERWISE
        | IfPr exp -> (
          match exp.it with
          | MatchE (inner_exp, CaseP mixop) ->
            (* Step 1: resolve inner_exp one level to find the binding target.
               We update the VALUE that inner_exp currently points to, not
               inner_exp itself.  This keeps the SharedExp ancestry chain intact:
               e.g., if type' → b _dummy#0 and type → _dummy#0, then
               "if type matches A" should update _dummy#0 → a _dummy#1 so that
               type' is automatically b (a _dummy#1) when the chain is followed. *)
            let resolved = match SharedExp.find_opt inner_exp env with
              | Some v -> v
              | None -> inner_exp
            in
            (* Step 2: look up field types from spec using inner_exp's annotation *)
            let field_typs = field_typs_of_mixop spec inner_exp mixop in
            (* Step 3: create fresh dummies for each field *)
            let dummies = List.map (fun (typ : typ) -> fresh_dummy typ.it) field_typs in
            (* Step 4: refine env — update the resolved target, not inner_exp itself *)
            let case_exp = CaseE (mixop, dummies) $$ (inner_exp.at % inner_exp.note) in
            let env' = SharedExp.add resolved case_exp env in
            (* Step 5: register each dummy as di → di *)
            let env'' = List.fold_left (fun e d -> SharedExp.add d d e) env' dummies in
            MEM (env'', dummies)
          | IterE (inner_cond, iterexp) ->
            (* Enter the iterated context before evaluating the inner condition *)
            let _iter, itervars = iterexp in
            let inner_env = List.fold_left (fun ie (itervar : var) ->
              let iterid, typ, _iters = itervar in
              let inner_var = VarE iterid $$ (iterid.at % typ.it) in
              SharedExp.add inner_var inner_var ie
            ) env itervars in
            let inner_prem = IfPr inner_cond $ prem.at in
            (match prem_binds_variable_aux inner_prem inner_env with
            | MEM (_, new_inner_vars) ->
              let newly_bound = List.filter (fun v ->
                not (SharedExp.mem v inner_env)
              ) new_inner_vars in
              let outer_vars = List.map (fun v ->
                IterE (v, iterexp) $$ (v.at % IterT (v.note $ no_region, fst iterexp))
              ) newly_bound in
              let env' = List.fold_left (fun e ov ->
                SharedExp.add ov ov e
              ) env outer_vars in
              MEM (env', outer_vars)
            | output -> output)
          | _ -> MEM (env, []) (* SubE, CmpE, etc. — constrain path but introduce no new variables *)
        )
        | LetPr (lhs, rhs) ->
          (* Step 1: resolve rhs by following the SharedExp chain until stable.
             A single lookup is insufficient: if rhs = VarE "type" and
             env[type] = _dummy#0 which has been further refined to
             env[_dummy#0] = a _dummy#1, we need to surface a _dummy#1 so
             that CaseE pattern matching in bind_pattern succeeds. *)
          let rec resolve_deep exp =
            match SharedExp.find_opt exp env with
            | Some v when compare v exp <> 0 -> resolve_deep v
            | Some v -> v
            | None -> exp
          in
          let rhs_val = resolve_deep rhs in
          (* Step 2: match lhs pattern against the resolved value, threading the env *)
          let rec bind_pattern lhs value env =
            match lhs.it with
            | VarE _ ->
              (SharedExp.add lhs value env, [lhs])
            | StrE fields ->
              (match value.it with
              | StrE value_fields ->
                (* Known value: bind each field variable to the matching sub-value *)
                List.fold_left2 (fun (e, acc) (_, lhs_field) (_, val_field) ->
                  let (e', vars') = bind_pattern lhs_field val_field e in
                  (e', acc @ vars')
                ) (env, []) fields value_fields
              | _ ->
                (* Unknown value: vi → DotE(rhs, ai) *)
                List.fold_left (fun (e, acc) (atom, lhs_field) ->
                  let dot_exp = DotE (rhs, atom) $$ (lhs_field.at % lhs_field.note) in
                  let (e', vars') = bind_pattern lhs_field dot_exp e in
                  (e', acc @ vars')
                ) (env, []) fields)
            | CaseE (_, sub_exps) ->
              (match value.it with
              | CaseE (_, sub_vals) when List.length sub_exps = List.length sub_vals ->
                (* Known value: bind each sub-expression to the matching sub-value *)
                List.fold_left2 (fun (e, acc) lhs_sub val_sub ->
                  let (e', vars') = bind_pattern lhs_sub val_sub e in
                  (e', acc @ vars')
                ) (env, []) sub_exps sub_vals
              | _ ->
                (* Unknown value: bind each sub-expression to itself *)
                List.fold_left (fun (e, acc) lhs_sub ->
                  let (e', vars') = bind_pattern lhs_sub lhs_sub e in
                  (e', acc @ vars')
                ) (env, []) sub_exps)
            | IterE (inner_lhs, iterexp) ->
              (* Descent through iteration: bind the element variable *)
              let elem_val = match value.it with
                | IterE (inner_val, _) -> inner_val
                | _ -> value
              in
              let (env', vars') = bind_pattern inner_lhs elem_val env in
              (* Ascent: wrap newly bound element variables in IterE for the outer env *)
              let outer_vars = List.map (fun v ->
                IterE (v, iterexp) $$ (v.at % IterT (v.note $ no_region, fst iterexp))
              ) vars' in
              let env'' = List.fold_left (fun e ov ->
                SharedExp.add ov ov e
              ) env' outer_vars in
              (env'', outer_vars)
            | _ -> (env, [])
          in
          let (env', new_vars) = bind_pattern lhs rhs_val env in
          MEM (env', new_vars)
        | IterPr (inner_prem, iterexp) ->
          let _iter, itervars = iterexp in
          (* Step 1: Descent — add element-level variables to inner env *)
          let inner_env = List.fold_left (fun ie (itervar : var) ->
            let iterid, typ, _iters = itervar in
            let inner_var = VarE iterid $$ (iterid.at % typ.it) in
            SharedExp.add inner_var inner_var ie
          ) env itervars in
          (* Step 2: Process the inner premise in the inner env *)
          (match prem_binds_variable_aux inner_prem inner_env with
          | MEM (_, new_inner_vars) ->
            (* Step 3: Ascent — wrap newly bound element variables in IterE *)
            let newly_bound = List.filter (fun v ->
              not (SharedExp.mem v inner_env)
            ) new_inner_vars in
            let outer_vars = List.map (fun v ->
              IterE (v, iterexp) $$ (v.at % IterT (v.note $ no_region, fst iterexp))
            ) newly_bound in
            let env' = List.fold_left (fun e ov ->
              SharedExp.add ov ov e
            ) env outer_vars in
            MEM (env', outer_vars)
          | output -> output)
      in
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
  List.map (is_funcdef_total spec) funcdefs
