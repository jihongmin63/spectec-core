open Common.Source
open Il
open Xl

let compare_id_canon (id_a : id) (id_b : id) : int =
  compare id_a.it id_b.it

let compare_iter_canon (iter_a : iter) (iter_b : iter) : int =
  match (iter_a, iter_b) with
  | Opt, Opt | List, List -> 0
  | Opt, List -> -1
  | List, Opt -> 1

let rec compare_typ_canon (typ_a : typ) (typ_b : typ) : int =
  compare_typ'_canon typ_a.it typ_b.it

and compare_typ'_canon (typ_a : typ') (typ_b : typ') : int =
  match (typ_a, typ_b) with
  | BoolT, BoolT -> 0
  | BoolT, _ -> -1
  | _, BoolT -> 1
  | NumT numtyp_a, NumT numtyp_b -> compare numtyp_a numtyp_b
  | NumT _, _ -> -1
  | _, NumT _ -> 1
  | TextT, TextT -> 0
  | TextT, _ -> -1
  | _, TextT -> 1
  | VarT (id_a, targs_a), VarT (id_b, targs_b) ->
    let c_id = compare_id_canon id_a id_b in
    if c_id <> 0 then c_id else compare_targs_canon targs_a targs_b
  | VarT _, _ -> -1
  | _, VarT _ -> 1
  | TupleT typs_a, TupleT typs_b -> compare_typs_canon typs_a typs_b
  | TupleT _, _ -> -1
  | _, TupleT _ -> 1
  | IterT (typ_a, iter_a), IterT (typ_b, iter_b) ->
    let c_typ = compare_typ_canon typ_a typ_b in
    if c_typ <> 0 then c_typ else compare_iter_canon iter_a iter_b
  | IterT _, _ -> -1
  | _, IterT _ -> 1
  | FuncT, FuncT -> 0

and compare_targ_canon (targ_a : targ) (targ_b : targ) : int =
  compare_typ'_canon targ_a.it targ_b.it

and compare_targs_canon (targs_a : targ list) (targs_b : targ list) : int =
  List.compare compare_targ_canon targs_a targs_b

and compare_typs_canon (typs_a : typ list) (typs_b : typ list) : int =
  List.compare compare_typ_canon typs_a typs_b

and compare_atom_canon (atom_a : atom) (atom_b : atom) : int =
  Xl.Atom.compare atom_a.it atom_b.it

and compare_var_canon ((id_a, _typ_a, iters_a) : var) ((id_b, _typ_b, iters_b) : var) :
    int =
  let c_id = compare_id_canon id_a id_b in
  if c_id <> 0 then c_id
  else List.compare compare_iter_canon iters_a iters_b

and compare_vars_canon (vars_a : var list) (vars_b : var list) : int =
  List.compare compare_var_canon vars_a vars_b

and compare_iterexp_canon ((iter_a, vars_a) : iterexp) ((iter_b, vars_b) : iterexp) :
    int =
  let c_iter = compare_iter_canon iter_a iter_b in
  if c_iter <> 0 then c_iter else compare_vars_canon vars_a vars_b

and compare_pattern_canon (pattern_a : pattern) (pattern_b : pattern) : int =
  match (pattern_a, pattern_b) with
  | CaseP mixop_a, CaseP mixop_b -> Mixop.compare mixop_a mixop_b
  | CaseP _, _ -> -1
  | _, CaseP _ -> 1
  | ListP `Cons, ListP `Cons -> 0
  | ListP `Cons, _ -> -1
  | _, ListP `Cons -> 1
  | ListP (`Fixed len_a), ListP (`Fixed len_b) -> compare len_a len_b
  | ListP (`Fixed _), _ -> -1
  | _, ListP (`Fixed _) -> 1
  | ListP `Nil, ListP `Nil -> 0
  | ListP `Nil, _ -> -1
  | _, ListP `Nil -> 1
  | OptP `Some, OptP `Some -> 0
  | OptP `Some, _ -> -1
  | _, OptP `Some -> 1
  | OptP `None, OptP `None -> 0

and compare_path_canon (path_a : path) (path_b : path) : int =
  match (path_a.it, path_b.it) with
  | RootP, RootP -> 0
  | RootP, _ -> -1
  | _, RootP -> 1
  | IdxP (path_a, exp_a), IdxP (path_b, exp_b) ->
    let c_path = compare_path_canon path_a path_b in
    if c_path <> 0 then c_path else compare_exp_canon exp_a exp_b
  | IdxP _, _ -> -1
  | _, IdxP _ -> 1
  | SliceP (path_a, exp_l_a, exp_h_a), SliceP (path_b, exp_l_b, exp_h_b) ->
    let c_path = compare_path_canon path_a path_b in
    if c_path <> 0 then c_path
    else
      let c_l = compare_exp_canon exp_l_a exp_l_b in
      if c_l <> 0 then c_l else compare_exp_canon exp_h_a exp_h_b
  | SliceP _, _ -> -1
  | _, SliceP _ -> 1
  | DotP (path_a, atom_a), DotP (path_b, atom_b) ->
    let c_path = compare_path_canon path_a path_b in
    if c_path <> 0 then c_path else compare_atom_canon atom_a atom_b

and compare_arg_canon (arg_a : arg) (arg_b : arg) : int =
  match (arg_a.it, arg_b.it) with
  | ExpA exp_a, ExpA exp_b -> compare_exp_canon exp_a exp_b
  | ExpA _, _ -> -1
  | _, ExpA _ -> 1
  | DefA id_a, DefA id_b -> compare_id_canon id_a id_b

and compare_args_canon (args_a : arg list) (args_b : arg list) : int =
  List.compare compare_arg_canon args_a args_b

and compare_exps_canon (exps_a : exp list) (exps_b : exp list) : int =
  List.compare compare_exp_canon exps_a exps_b

and compare_exp_canon (exp_a : exp) (exp_b : exp) : int =
  match (exp_a.it, exp_b.it) with
    | BoolE b_a, BoolE b_b -> compare b_a b_b
    | BoolE _, _ -> -1
    | _, BoolE _ -> 1
    | NumE n_a, NumE n_b -> compare n_a n_b
    | NumE _, _ -> -1
    | _, NumE _ -> 1
    | TextE t_a, TextE t_b -> compare t_a t_b
    | TextE _, _ -> -1
    | _, TextE _ -> 1
    | VarE id_a, VarE id_b -> compare_id_canon id_a id_b
    | VarE _, _ -> -1
    | _, VarE _ -> 1
    | UnE (unop_a, optyp_a, exp_a), UnE (unop_b, optyp_b, exp_b) ->
      let c_unop = compare unop_a unop_b in
      if c_unop <> 0 then c_unop
      else
        let c_optyp = compare optyp_a optyp_b in
        if c_optyp <> 0 then c_optyp else compare_exp_canon exp_a exp_b
    | UnE _, _ -> -1
    | _, UnE _ -> 1
    | BinE (binop_a, optyp_a, exp_l_a, exp_r_a), BinE (binop_b, optyp_b, exp_l_b, exp_r_b)
      ->
      let c_binop = compare binop_a binop_b in
      if c_binop <> 0 then c_binop
      else
        let c_optyp = compare optyp_a optyp_b in
        if c_optyp <> 0 then c_optyp
        else
          let c_l = compare_exp_canon exp_l_a exp_l_b in
          if c_l <> 0 then c_l else compare_exp_canon exp_r_a exp_r_b
    | BinE _, _ -> -1
    | _, BinE _ -> 1
    | CmpE (cmpop_a, optyp_a, exp_l_a, exp_r_a), CmpE (cmpop_b, optyp_b, exp_l_b, exp_r_b)
      ->
      let c_cmpop = compare cmpop_a cmpop_b in
      if c_cmpop <> 0 then c_cmpop
      else
        let c_optyp = compare optyp_a optyp_b in
        if c_optyp <> 0 then c_optyp
        else
          let c_l = compare_exp_canon exp_l_a exp_l_b in
          if c_l <> 0 then c_l else compare_exp_canon exp_r_a exp_r_b
    | CmpE _, _ -> -1
    | _, CmpE _ -> 1
    | UpCastE (typ_a, exp_a), UpCastE (typ_b, exp_b)
    | DownCastE (typ_a, exp_a), DownCastE (typ_b, exp_b) ->
      let c_typ = compare_typ_canon typ_a typ_b in
      if c_typ <> 0 then c_typ else compare_exp_canon exp_a exp_b
    | UpCastE _, _ -> -1
    | _, UpCastE _ -> 1
    | DownCastE _, _ -> -1
    | _, DownCastE _ -> 1
    | SubE (exp_a, typ_a), SubE (exp_b, typ_b) ->
      let c_exp = compare_exp_canon exp_a exp_b in
      if c_exp <> 0 then c_exp else compare_typ_canon typ_a typ_b
    | SubE _, _ -> -1
    | _, SubE _ -> 1
    | MatchE (exp_a, pattern_a), MatchE (exp_b, pattern_b) ->
      let c_exp = compare_exp_canon exp_a exp_b in
      if c_exp <> 0 then c_exp else compare_pattern_canon pattern_a pattern_b
    | MatchE _, _ -> -1
    | _, MatchE _ -> 1
    | TupleE exps_a, TupleE exps_b -> compare_exps_canon exps_a exps_b
    | TupleE _, _ -> -1
    | _, TupleE _ -> 1
    | CaseE (mixop_a, exps_a), CaseE (mixop_b, exps_b) ->
      let c_mixop = Mixop.compare mixop_a mixop_b in
      if c_mixop <> 0 then c_mixop else compare_exps_canon exps_a exps_b
    | CaseE _, _ -> -1
    | _, CaseE _ -> 1
    | StrE fields_a, StrE fields_b ->
      let compare_field (atom_a, exp_a) (atom_b, exp_b) =
        let c_atom = compare_atom_canon atom_a atom_b in
        if c_atom <> 0 then c_atom else compare_exp_canon exp_a exp_b
      in
      List.compare compare_field fields_a fields_b
    | StrE _, _ -> -1
    | _, StrE _ -> 1
    | OptE None, OptE None -> 0
    | OptE None, _ -> -1
    | _, OptE None -> 1
    | OptE (Some exp_a), OptE (Some exp_b) -> compare_exp_canon exp_a exp_b
    | OptE _, _ -> -1
    | _, OptE _ -> 1
    | ListE exps_a, ListE exps_b -> compare_exps_canon exps_a exps_b
    | ListE _, _ -> -1
    | _, ListE _ -> 1
    | ConsE (exp_h_a, exp_t_a), ConsE (exp_h_b, exp_t_b)
    | CatE (exp_h_a, exp_t_a), CatE (exp_h_b, exp_t_b)
    | MemE (exp_h_a, exp_t_a), MemE (exp_h_b, exp_t_b) ->
      let c_h = compare_exp_canon exp_h_a exp_h_b in
      if c_h <> 0 then c_h else compare_exp_canon exp_t_a exp_t_b
    | ConsE _, _ -> -1
    | _, ConsE _ -> 1
    | CatE _, _ -> -1
    | _, CatE _ -> 1
    | MemE _, _ -> -1
    | _, MemE _ -> 1
    | LenE exp_a, LenE exp_b -> compare_exp_canon exp_a exp_b
    | LenE _, _ -> -1
    | _, LenE _ -> 1
    | DotE (exp_a, atom_a), DotE (exp_b, atom_b) ->
      let c_exp = compare_exp_canon exp_a exp_b in
      if c_exp <> 0 then c_exp else compare_atom_canon atom_a atom_b
    | DotE _, _ -> -1
    | _, DotE _ -> 1
    | IdxE (exp_b_a, exp_i_a), IdxE (exp_b_b, exp_i_b) ->
      let c_b = compare_exp_canon exp_b_a exp_b_b in
      if c_b <> 0 then c_b else compare_exp_canon exp_i_a exp_i_b
    | IdxE _, _ -> -1
    | _, IdxE _ -> 1
    | SliceE (exp_b_a, exp_l_a, exp_h_a), SliceE (exp_b_b, exp_l_b, exp_h_b) ->
      let c_b = compare_exp_canon exp_b_a exp_b_b in
      if c_b <> 0 then c_b
      else
        let c_l = compare_exp_canon exp_l_a exp_l_b in
        if c_l <> 0 then c_l else compare_exp_canon exp_h_a exp_h_b
    | SliceE _, _ -> -1
    | _, SliceE _ -> 1
    | UpdE (exp_b_a, path_a, exp_f_a), UpdE (exp_b_b, path_b, exp_f_b) ->
      let c_b = compare_exp_canon exp_b_a exp_b_b in
      if c_b <> 0 then c_b
      else
        let c_path = compare_path_canon path_a path_b in
        if c_path <> 0 then c_path else compare_exp_canon exp_f_a exp_f_b
    | UpdE _, _ -> -1
    | _, UpdE _ -> 1
    | CallE (id_a, targs_a, args_a), CallE (id_b, targs_b, args_b) ->
      let c_id = compare_id_canon id_a id_b in
      if c_id <> 0 then c_id
      else
        let c_targs = compare_targs_canon targs_a targs_b in
        if c_targs <> 0 then c_targs else compare_args_canon args_a args_b
    | CallE _, _ -> -1
    | _, CallE _ -> 1
    | IterE (exp_a, iterexp_a), IterE (exp_b, iterexp_b) ->
      let c_exp = compare_exp_canon exp_a exp_b in
      if c_exp <> 0 then c_exp else compare_iterexp_canon iterexp_a iterexp_b

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
  let rec iterate_prems env prems : output =
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
            (* Descent: use outer env value so the ancestry chain flows into inner scope *)
            let inner_env = List.fold_left (fun ie (itervar : var) ->
              let iterid, typ, _iters = itervar in
              let inner_var = VarE iterid $$ (iterid.at % typ.it) in
              let inner_val = match SharedExp.find_opt inner_var env with
                | Some v -> v
                | None -> (
                  match find_by_id iterid env with
                  | Some v -> v
                  | None -> inner_var
                )
              in
              SharedExp.add inner_var inner_val ie
            ) env itervars in
            let inner_prem = IfPr inner_cond $ prem.at in
            (match prem_binds_variable_aux inner_prem inner_env with
            | MEM (new_inner_env, new_inner_vars) ->
              (* deep_outer: follow the chain in the *original* outer env to the root *)
              let rec deep_outer exp =
                match SharedExp.find_opt exp env with
                | Some v when compare_exp_canon v exp <> 0 -> deep_outer v
                | Some v -> v
                | None -> exp
              in
              (* Propagate changes to pre-existing outer-env vars back *)
              let env_propagated = SharedExp.fold (fun k v e ->
                match SharedExp.find_opt k e with
                | Some _ ->
                  let old_inner = match SharedExp.find_opt k inner_env with
                    | Some ov -> ov | None -> k
                  in
                  if compare_exp_canon old_inner v <> 0 then SharedExp.add k v e else e
                | None -> e
              ) new_inner_env env in
              let newly_bound = List.filter (fun v ->
                not (SharedExp.mem v env)
              ) new_inner_vars in
              (* Add element-level bindings with deep-resolved root source *)
              let env_with_elem = List.fold_left (fun e v ->
                let inner_src = match SharedExp.find_opt v new_inner_env with
                  | Some s -> s | None -> v
                in
                SharedExp.add v (deep_outer inner_src) e
              ) env_propagated newly_bound in
              let outer_vars = List.map (fun v ->
                IterE (v, iterexp) $$ (v.at % IterT (v.note $ no_region, fst iterexp))
              ) newly_bound in
              let env' = List.fold_left (fun e ov ->
                let elem_v = match ov.it with IterE (v, _) -> v | _ -> ov in
                let inner_src = match SharedExp.find_opt elem_v new_inner_env with
                  | Some s -> s | None -> elem_v
                in
                let root_src = deep_outer inner_src in
                let outer_src = IterE (root_src, iterexp) $$ (root_src.at % IterT (root_src.note $ no_region, fst iterexp)) in
                SharedExp.add ov outer_src e
              ) env_with_elem outer_vars in
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
            | Some v when compare_exp_canon v exp <> 0 -> resolve_deep v
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
              (* Ascent: wrap newly bound element variables in IterE for the outer env.
                 Each outer var maps to IterE(inner_src, iterexp), not to itself, to preserve
                 the SharedExp ancestry chain back to the original input. *)
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
              (env'', outer_vars)
            | _ -> (env, [])
          in
          let (env', new_vars) = bind_pattern lhs rhs_val env in
          MEM (env', new_vars)
        | IterPr (inner_prem, iterexp) ->
          let _iter, itervars = iterexp in
          (* Step 1: Descent — use outer env value so the ancestry chain flows into inner scope *)
          let inner_env = List.fold_left (fun ie (itervar : var) ->
            let iterid, typ, _iters = itervar in
            let inner_var = VarE iterid $$ (iterid.at % typ.it) in
            let inner_val = match SharedExp.find_opt inner_var env with
              | Some v -> v
              | None -> (
                match find_by_id iterid env with
                | Some v -> v
                | None -> inner_var
              )
            in
            SharedExp.add inner_var inner_val ie
          ) env itervars in
          (* Step 2: Process the inner premise in the inner env *)
          (match prem_binds_variable_aux inner_prem inner_env with
          | MEM (new_inner_env, new_inner_vars) ->
            (* deep_outer: follow the chain in the *original* outer env to the root *)
            let rec deep_outer exp =
              match SharedExp.find_opt exp env with
              | Some v when compare_exp_canon v exp <> 0 -> deep_outer v
              | Some v -> v
              | None -> exp
            in
            (* Step 3a: Propagate changes to pre-existing outer-env vars back *)
            let env_propagated = SharedExp.fold (fun k v e ->
              match SharedExp.find_opt k e with
              | Some _ ->
                let old_inner = match SharedExp.find_opt k inner_env with
                  | Some ov -> ov | None -> k
                in
                if compare_exp_canon old_inner v <> 0 then SharedExp.add k v e else e
              | None -> e
            ) new_inner_env env in
            (* Step 3b: Add element-level bindings for newly bound vars *)
            let newly_bound = List.filter (fun v ->
              not (SharedExp.mem v env)
            ) new_inner_vars in
            let env_with_elem = List.fold_left (fun e v ->
              let inner_src = match SharedExp.find_opt v new_inner_env with
                | Some s -> s | None -> v
              in
              SharedExp.add v (deep_outer inner_src) e
            ) env_propagated newly_bound in
            (* Step 3c: Add list-level outer vars *)
            let outer_vars = List.map (fun v ->
              IterE (v, iterexp) $$ (v.at % IterT (v.note $ no_region, fst iterexp))
            ) newly_bound in
            let env' = List.fold_left (fun e ov ->
              let elem_v = match ov.it with IterE (v, _) -> v | _ -> ov in
              let inner_src = match SharedExp.find_opt elem_v new_inner_env with
                | Some s -> s | None -> elem_v
              in
              let root_src = deep_outer inner_src in
              let outer_src = IterE (root_src, iterexp) $$ (root_src.at % IterT (root_src.note $ no_region, fst iterexp)) in
              SharedExp.add ov outer_src e
            ) env_with_elem outer_vars in
            MEM (env', outer_vars)
          | output -> output)
      in
      prem_binds_variable_aux prem env
    in
    match prems with
    | [] -> MEM (env, [])
    | prem :: prems ->
      match prem_binds_variable prem env with
      | MEM (updated_env, _) -> iterate_prems updated_env prems
      | output -> output
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
