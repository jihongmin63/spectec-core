open Common.Source
open Il
open Canon_compare

module SharedExp = Map.Make(struct
  type t = exp
  let compare = compare_exp_canon
end)

type output =
  | TEMP of exp list (* temporarily output where only inputs are extracted *)
  | MEM of exp SharedExp.t * exp list
  | OTHERWISE
  | FAIL of string

let extract_env env keys =
  let rec exact_exp (exp : exp) : exp =
    match SharedExp.find_opt exp env with
    | None -> exp
    | Some value -> (
      match value.it with
      | UnE (unop, optyp, exp) -> UnE (unop, optyp, exact_exp exp) $$ (value.at % value.note)
      | BinE (binop, optyp, exp_l, exp_r) -> BinE (binop, optyp, exact_exp exp_l, exact_exp exp_r) $$ (value.at % value.note)
      | CmpE (cmpop, optyp, exp_l, exp_r) -> CmpE (cmpop, optyp, exact_exp exp_l, exact_exp exp_r) $$ (value.at % value.note)
      | UpCastE (typ, exp) -> UpCastE (typ, exact_exp exp) $$ (value.at % value.note)
      | DownCastE (typ, exp) -> DownCastE (typ, exact_exp exp) $$ (value.at % value.note)
      | SubE (exp, typ) -> SubE (exact_exp exp, typ) $$ (value.at % value.note)
      | MatchE (exp, pattern) -> MatchE (exact_exp exp, pattern) $$ (value.at % value.note)
      | TupleE exps -> TupleE (List.map exact_exp exps) $$ (value.at % value.note)
      | CaseE notexp ->
        let mixop, exps = notexp in
        CaseE ((mixop, List.map exact_exp exps)) $$ (value.at % value.note)
      | StrE expfields ->
        StrE (List.map (fun expfield ->
            let atom, exp = expfield in
            atom, exact_exp exp
          ) expfields) $$ (value.at % value.note)
      | OptE expopt -> (
          match expopt with
          | None -> OptE None
          | Some exp -> OptE (Some (exact_exp exp))
        ) $$ (value.at % value.note)
      | ListE exps -> ListE (List.map exact_exp exps)  $$ (value.at % value.note)
      | ConsE (exp_l, exp_r) -> ConsE (exact_exp exp_l, exact_exp exp_r) $$ (value.at % value.note)
      | CatE (exp_l, exp_r) -> CatE (exact_exp exp_l, exact_exp exp_r) $$ (value.at % value.note)
      | MemE (exp_l, exp_r) -> MemE (exact_exp exp_l, exact_exp exp_r) $$ (value.at % value.note)
      | LenE exp -> LenE (exact_exp exp) $$ (value.at % value.note)
      | DotE (exp, atom) -> DotE (exact_exp exp, atom) $$ (value.at % value.note)
      | IdxE (exp_l, exp_r) -> IdxE (exact_exp exp_l, exact_exp exp_r) $$ (value.at % value.note)
      | SliceE (exp, exp_l, exp_n) -> SliceE (exact_exp exp, exact_exp exp_l, exact_exp exp_n) $$ (value.at % value.note)
      | UpdE (exp, path, exp_v) -> UpdE (exact_exp exp, exact_path path, exact_exp exp_v) $$ (value.at % value.note)
      | IterE (exp, iterexp) -> IterE (exact_exp exp, iterexp) $$ (value.at % value.note)
      | _ -> value
    )
  and exact_path path = (
    match path.it with
    | RootP -> path
    | IdxP (path, exp) -> IdxP (exact_path path, exact_exp exp) $$ (path.at % path.note)
    | SliceP (path, exp_l, exp_n) -> SliceP (exact_path path, exact_exp exp_l, exact_exp exp_n) $$ (path.at % path.note)
    | DotP (path, atom) -> DotP (exact_path path, atom) $$ (path.at % path.note)
  )
  in
  TEMP (List.map exact_exp keys)

let debug env =
  let pairs = SharedExp.bindings env in
  let _ = Format.printf "\n[ENV]\n" in
  List.map (fun pair ->
    Format.printf "%s -> %s\n" (Print.string_of_exp (fst pair)) (Print.string_of_exp (snd pair))
  ) pairs

let find_by_id (target_id : id) (env : exp SharedExp.t) : exp option =
  SharedExp.bindings env
  |> List.find_map (fun (k, v) ->
    match k.it with
    | VarE id when id.it = target_id.it -> Some v
    | _ -> None
  )

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
  | IterE (exp_iter, iterexp) ->
    let vars = variables_in_exp exp_iter in
    let iter, itervars = iterexp in
    (
    match iter with
    | Opt -> [exp]
    | List ->
      vars @
      (List.filter_map (fun var ->
        if (List.exists (fun (itervar : var) ->
            let id = filter_id var in
            let iterid, _, _ = itervar in
            id.it = iterid.it) itervars)
        then Some (IterE (var, iterexp) $$ (no_region % (IterT (var.note $ no_region, fst iterexp))))
        else
        None) vars)
    )
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

let total_check spec envs params : bool * string =
  (* TO DO *)
  let _ = spec, envs, params in
  false, "msg"

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

  (* Follow the env chain until stable — stops when the value maps to itself or
     is absent.  Used for both rhs resolution in LetPr and source-root lookup
     during iter ascent (was previously duplicated as deep_outer / resolve_deep). *)
  let rec resolve_deep env exp =
    match SharedExp.find_opt exp env with
    | Some v when compare_exp_canon v exp <> 0 -> resolve_deep env v
    | Some v -> v
    | None -> exp
  in

  (* Build the inner environment for entering an iteration scope.
     Each itervar's outer (iterated) binding is unwrapped to its element form
     so that premises inside the scope see the element-level variable. *)
  let descend_iter_env env itervars =
    List.fold_left (fun ie (itervar : var) ->
      let iterid, typ, _iters = itervar in
      let inner_var = VarE iterid $$ (iterid.at % typ.it) in
      let inner_val =
        match SharedExp.find_opt inner_var env with
        | Some v -> v
        | None ->
          (match find_by_id iterid env with
          | Some v -> v
          | None -> inner_var)
      in
      SharedExp.add inner_var inner_val ie
    ) env itervars
  in

  (* After processing a premise inside an iteration scope, merge the results
     back into the outer environment:
       1. Propagate changes to pre-existing outer vars (refinements made inside).
       2. For newly bound element-level vars, add them with their deep-resolved
          outer source so the ancestry chain reaches back to the original input.
       3. Wrap newly bound vars in IterE and add outer-level bindings. *)
  let ascend_iter_env outer_env inner_env new_inner_env new_inner_vars iterexp =
    let env_propagated = SharedExp.fold (fun k v e ->
      match SharedExp.find_opt k e with
      | Some _ ->
        let old_inner = match SharedExp.find_opt k inner_env with
          | Some ov -> ov | None -> k
        in
        if compare_exp_canon old_inner v <> 0 then SharedExp.add k v e else e
      | None -> e
    ) new_inner_env outer_env in
    let newly_bound = List.filter (fun v ->
      not (SharedExp.mem v outer_env)
    ) new_inner_vars in
    let env_with_elem = List.fold_left (fun e v ->
      let inner_src = match SharedExp.find_opt v new_inner_env with
        | Some s -> s | None -> v
      in
      SharedExp.add v (resolve_deep outer_env inner_src) e
    ) env_propagated newly_bound in
    let outer_vars = List.map (fun v ->
      IterE (v, iterexp) $$ (v.at % IterT (v.note $ no_region, fst iterexp))
    ) newly_bound in
    let env' = List.fold_left (fun e ov ->
      let elem_v = match ov.it with IterE (v, _) -> v | _ -> ov in
      let inner_src = match SharedExp.find_opt elem_v new_inner_env with
        | Some s -> s | None -> elem_v
      in
      let root_src = resolve_deep outer_env inner_src in
      let outer_src = IterE (root_src, iterexp) $$ (root_src.at % IterT (root_src.note $ no_region, fst iterexp)) in
      SharedExp.add ov outer_src e
    ) env_with_elem outer_vars in
    MEM (env', outer_vars)
  in

  (* Match lhs pattern against a resolved value, extending the environment.
     rhs is the original (pre-resolution) rhs expression, used to synthesise
     DotE projections when the value is structurally unknown (StrE case). *)
  let rec bind_pattern rhs lhs value env =
    match lhs.it with
    | VarE _ ->
      (SharedExp.add lhs value env, [lhs])
    | StrE fields ->
      (match value.it with
      | StrE value_fields ->
        (* Known value: bind each field variable to the matching sub-value *)
        List.fold_left2 (fun (e, acc) (_, lhs_field) (_, val_field) ->
          let (e', vars') = bind_pattern rhs lhs_field val_field e in
          (e', acc @ vars')
        ) (env, []) fields value_fields
      | _ ->
        (* Unknown value: vi → DotE(rhs, ai) *)
        List.fold_left (fun (e, acc) (atom, lhs_field) ->
          let dot_exp = DotE (rhs, atom) $$ (lhs_field.at % lhs_field.note) in
          let (e', vars') = bind_pattern rhs lhs_field dot_exp e in
          (e', acc @ vars')
        ) (env, []) fields)
    | CaseE (_, sub_exps) ->
      (match value.it with
      | CaseE (_, sub_vals) when List.length sub_exps = List.length sub_vals ->
        (* Known value: bind each sub-expression to the matching sub-value *)
        List.fold_left2 (fun (e, acc) lhs_sub val_sub ->
          let (e', vars') = bind_pattern rhs lhs_sub val_sub e in
          (e', acc @ vars')
        ) (env, []) sub_exps sub_vals
      | _ ->
        (* Unknown value: bind each sub-expression to itself *)
        List.fold_left (fun (e, acc) lhs_sub ->
          let (e', vars') = bind_pattern rhs lhs_sub lhs_sub e in
          (e', acc @ vars')
        ) (env, []) sub_exps)
    | IterE (inner_lhs, iterexp) ->
      let iter, _itervars = iterexp in
      (match iter with
      | Opt ->
        (* Optional iter may be absent, so do not bind inner variables into env. *)
        (SharedExp.add lhs value env, [lhs])
      | List ->
        (* Descent through iteration: bind the element variable *)
        let elem_val = match value.it with
          | IterE (inner_val, _) -> inner_val
          | _ -> value
        in
        let (env', vars') = bind_pattern rhs inner_lhs elem_val env in
        (* Ascent: wrap newly bound element variables in IterE for the outer env.
           Each outer var maps to IterE(inner_src, iterexp), not to itself, to
           preserve the SharedExp ancestry chain back to the original input. *)
        let outer_vars = List.map (fun v ->
          IterE (v, iterexp) $$ (v.at % IterT (v.note $ no_region, fst iterexp))
        ) vars' in
        let env'' = List.fold_left2 (fun e ov v ->
          let inner_src = match SharedExp.find_opt v env' with
            | Some s -> s
            | None -> v
          in
          let outer_src = IterE (inner_src, iterexp) $$ (inner_src.at % IterT (inner_src.note $ no_region, fst iterexp)) in
          SharedExp.add ov outer_src e
        ) env' outer_vars vars' in
        (env'', outer_vars))
    | OptE expopt ->
      (match expopt with
      | None -> (env, [])
      | Some inner_lhs ->
        (* ?(inner_lhs) pattern: match confirmed Some, so descend into the value.
           Strip one layer of Opt/IterE from value to reach the inner expression. *)
        let inner_val = match value.it with
          | IterE (iv, _) -> iv
          | OptE (Some iv) -> iv
          | _ -> value
        in
        bind_pattern rhs inner_lhs inner_val env)
    | _ -> (env, [])
  in

  (* Process a single premise against the current environment, returning an
     updated output.  Structural errors (FAIL) and else-branches (OTHERWISE)
     propagate immediately without further processing. *)
  let rec prem_binds_variable prem env =
    match prem.it with
    | RulePr _ | IfHoldPr _ | IfNotHoldPr _ ->
      FAIL ("Function includes relation call " ^ (Print.string_of_prem prem))
    | DebugPr _ -> FAIL "Function includes debug premise"
    | ElsePr -> OTHERWISE
    | IfPr exp -> (
      match exp.it with
      | MatchE (inner_exp, CaseP mixop) ->
        (* Step 1: resolve inner_exp one level to find the binding target.
           We update the VALUE that inner_exp currently points to, not
           inner_exp itself.  This keeps the SharedExp ancestry chain intact. *)
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
      | MatchE (inner_exp, OptP `Some) ->
        (* Opt match: inner_exp is known to be Some after this premise.
           Extract the inner value and self-register all its variables. *)
        let resolved = match SharedExp.find_opt inner_exp env with
          | Some v -> v
          | None -> inner_exp
        in
        let inner_val = match resolved.it with
          | IterE (iv, _) -> iv
          | OptE (Some iv) -> iv
          | _ -> resolved
        in
        let all_vars = variables_in_exp inner_val in
        let env' = List.fold_left (fun e v ->
          if SharedExp.mem v e then e else SharedExp.add v v e
        ) env all_vars in
        MEM (env', all_vars)
      | IterE (inner_cond, iterexp) ->
        (* Enter the iterated context before evaluating the inner condition *)
        let _iter, itervars = iterexp in
        let inner_env = descend_iter_env env itervars in
        let inner_prem = IfPr inner_cond $ prem.at in
        (match prem_binds_variable inner_prem inner_env with
        | MEM (new_inner_env, new_inner_vars) ->
          ascend_iter_env env inner_env new_inner_env new_inner_vars iterexp
        | output -> output)
      | _ -> MEM (env, []) (* SubE, CmpE, etc. — constrain path but introduce no new variables *)
    )
    | LetPr (lhs, rhs) ->
      (* Resolve rhs by following the SharedExp chain until stable.
         A single lookup is insufficient when the chain has been further
         refined (e.g. rhs → _dummy#0 → CaseE(a, [_dummy#1])). *)
      let rhs_val = resolve_deep env rhs in
      let (env', new_vars) = bind_pattern rhs lhs rhs_val env in
      MEM (env', new_vars)
    | IterPr (inner_prem, iterexp) ->
      (* Descent into the iteration scope, process inner premise, then ascend. *)
      let _iter, itervars = iterexp in
      let inner_env = descend_iter_env env itervars in
      (match prem_binds_variable inner_prem inner_env with
      | MEM (new_inner_env, new_inner_vars) ->
        ascend_iter_env env inner_env new_inner_env new_inner_vars iterexp
      | output -> output)
  in

  let iterate_prems env prems =
    let rec loop env = function
      | [] -> MEM (env, [])
      | prem :: rest ->
        let _ = debug env in
        (match prem_binds_variable prem env with
        | MEM (updated_env, _) -> loop updated_env rest
        | output -> output)
    in
    let _ = debug env in
    loop env prems
  in

  let iterate_clause (clause : clause) =
    let args, _, prems = clause.it in
    let expargs = List.map (fun arg ->
      match arg.it with
      | ExpA exp -> exp
      | DefA _ -> let _ = Format.printf "Do not support function parameter\n" in assert false
    ) args in
    let init_env : exp SharedExp.t = List.fold_left (fun env exp ->
        let vars = variables_in_exp exp in
        List.fold_left (fun env var ->
          SharedExp.add var var env
        ) env vars
    ) SharedExp.empty expargs in
    match iterate_prems init_env prems with
    | MEM (env, _) -> extract_env env expargs
    | output -> output
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
