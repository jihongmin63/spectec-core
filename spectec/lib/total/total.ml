open Common.Source
open Il
open Canon_compare

(* ─── Checker context: shared state for a single funcdef analysis ──────────── *)

type checker = {
  spec       : def list;
  fresh_dummy : typ' -> exp;
}

let make_checker spec =
  let dummy = ref 0 in
  {
    spec;
    fresh_dummy = (fun typ ->
      let id = ("dummy_" ^ string_of_int !dummy) $ no_region in
      incr dummy;
      VarE id $$ (no_region % typ)
    );
  }

(* ─── Environment: two-map indirection / value store ──────────────────────── *)

module SharedExp = Map.Make(struct
  type t = exp
  let compare = compare_exp_canon
end)

module Environment = struct
  type t = {
    exp_to_dummy : exp SharedExp.t;        (* var   → dummy (indirection)   *)
    dummy_to_val : exp option SharedExp.t; (* dummy → current value         *)
  }

  let empty = {
    exp_to_dummy = SharedExp.empty;
    dummy_to_val = SharedExp.empty;
  }

  let add_binding env var dummy =
    { env with exp_to_dummy = SharedExp.add var dummy env.exp_to_dummy }

  let set_dummy env dummy value =
    { env with dummy_to_val = SharedExp.add dummy value env.dummy_to_val }

  let find_dummy_opt env exp =
    SharedExp.find_opt exp env.exp_to_dummy

  let find_dummy env exp =
    SharedExp.find exp env.exp_to_dummy

  let find_val env dummy =
    SharedExp.find_opt dummy env.dummy_to_val

  let mem env exp =
    SharedExp.mem exp env.exp_to_dummy
end

type output =
  | TEMP of (exp * exp) list  (** Temporary output: only extracted inputs *)
  | MEM of Environment.t * exp list
  | OTHERWISE         (** Hit an ElsePr branch *)
  | FAIL of string    (** Unsupported construct *)

(* ─── Env: operations on the Environment ──────────────────────────────────── *)

module Env = struct

  (** Resolve every variable in [keys] to its fully-expanded form by following
      the env chain transitively.  The result is wrapped in TEMP so callers can
      pattern-match on it alongside other [output] variants. *)
  let extract keys env =
    (* Long Term TO DO *)
    let _ = env in
    TEMP (List.combine keys keys)

  let debug env =
    Format.printf "\n[EXP_TO_DUMMY]\n";
    SharedExp.iter (fun k v ->
      Format.printf "  %s -> %s\n" (Print.string_of_exp k) (Print.string_of_exp v)
    ) env.Environment.exp_to_dummy;
    Format.printf "[DUMMY_TO_VAL]\n";
    SharedExp.iter (fun k v ->
      let v_str = match v with Some e -> "[ " ^ Print.string_of_exp e ^ " ]" | None -> "X" in
      Format.printf "  %s -> %s\n" (Print.string_of_exp k) v_str
    ) env.Environment.dummy_to_val

  (*let find_by_id (target_id : id) (env : Environment.t) : (exp option) option =
    SharedExp.bindings env.exp_to_dummy
    |> List.find_map (fun (k, dummy) ->
      match k.it with
      | VarE id when id.it = target_id.it ->
        Some (match Environment.find_val env dummy with
          | Some v -> v
          | None -> Some dummy)
      | _ -> None
    )*)
  let init_exp env exp ctx =
    let dummy = ctx.fresh_dummy exp.note in
    let env' = Environment.add_binding env exp dummy in
    Environment.set_dummy env' dummy (Some dummy)

  let find env exp = Environment.find_val env (Environment.find_dummy env exp)

end


(* ─── Spec: look up typing information from the spec ──────────────────────── *)

module Spec = struct

  (** Look up [target_mixop] in the variant type of [exp], returning [Some typs]
      if the case is found (with [typs] possibly empty for 0-field cases) or
      [None] if [exp]'s type is not a variant type or doesn't contain the case. *)
  let find_variant_case spec (exp_note : typ') (target_mixop : mixop) =
    match exp_note with
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
            let (m, _) = nottyp.it in Xl.Mixop.eq m target_mixop
          ) cases with
          | Some (nottyp, _) -> let (_, typs) = nottyp.it in Some typs
          | None -> None)
        | _ -> None)
      | None -> None)
    | _ -> None

  (** Return the field types for [target_mixop] in the variant type that
      [exp]'s type annotation resolves to, or [] if not found. *)
  let field_typs_of_mixop spec (exp : exp) (target_mixop : mixop) =
    match find_variant_case spec exp.note target_mixop with
    | Some typs -> typs
    | None -> []

  let check_and_find_one_variant spec (exp_note : typ') =
    match exp_note with
    | VarT (id, _) ->
      let deftyp_opt = List.find_map (fun def ->
        match def.it with
        | TypD (tid, _, deftyp) when tid.it = id.it -> Some deftyp
        | _ -> None
      ) spec in
      (match deftyp_opt with
      | Some dt -> (match dt.it with
        | VariantT cases ->
          (match cases with
          | [case] -> Some case
          | _ -> None) (* TO DO : control struct *)
        | _ -> None)
      | None -> None)
    | _ -> None

end

(* ─── Vars: extract variable sub-expressions from terms ───────────────────── *)

module Vars = struct

  let rec filter_id exp =
    match exp.it with
    | VarE id -> id
    | IterE (e, _) -> filter_id e
    | _ -> assert false

  let exp_mapsto_env env ctx exp =
    let rec exp_mapsto_env_aux env exp =
      match exp.it with
      | VarE _ -> 
        (
        match Spec.check_and_find_one_variant ctx.spec exp.note with
        | None -> Env.init_exp env exp ctx
        | Some case ->
            let nottype, _ = case in
            let mixop, _ = nottype.it in
            let field_typs = Spec.field_typs_of_mixop ctx.spec exp mixop in
            let dummies = List.map (fun (typ : typ) -> ctx.fresh_dummy typ.it) field_typs in
            let case_exp = CaseE (mixop, dummies) $$ (exp.at % exp.note) in
            let location = ctx.fresh_dummy exp.note in
            let env' = Environment.set_dummy env location (Some case_exp) in
            let env'' = Environment.add_binding env' exp location in
            List.fold_left (fun e d -> Environment.set_dummy e d (Some d)) env'' dummies
        )
      | UnE (_, _, exp) | UpCastE (_, exp) | DownCastE (_, exp)
      | SubE (exp, _) | MatchE (exp, _) | LenE exp | DotE (exp, _) -> exp_mapsto_env_aux env exp
      | BinE (_, _, l, r) | CmpE (_, _, l, r)
      | ConsE (l, r) | CatE (l, r) | MemE (l, r) | IdxE (l, r) ->
        let env' = exp_mapsto_env_aux env l in
        exp_mapsto_env_aux env' r
      | ListE es | TupleE es | CaseE (_, es) ->
        List.fold_left (fun env exp -> exp_mapsto_env_aux env exp) env es
      | OptE None | BoolE _ | NumE _ | TextE _ | CallE _ -> env
      | OptE (Some exp) -> exp_mapsto_env_aux env exp
      | StrE fields -> List.fold_left (fun env (_, exp) -> exp_mapsto_env_aux env exp) env fields
      | SliceE (e, l, n) ->
        let env' = exp_mapsto_env_aux env l in
        let env'' = exp_mapsto_env_aux env' n in
        exp_mapsto_env_aux env'' e
      | UpdE (e, path, new_e) ->
        let env' = exp_mapsto_env_aux env new_e in
        let env'' = path_mapsto_env_aux env' path in
        exp_mapsto_env_aux env'' e
      | IterE (e_iter, iterexp) ->
        let env' = exp_mapsto_env_aux env e_iter in
        let value = Environment.find_dummy env' e_iter in
        let iter, _ = iterexp in
        (match iter with
        | Opt -> 
            let dummy = ctx.fresh_dummy exp.note in
            let env' = Environment.add_binding env exp dummy in
            Environment.set_dummy env' dummy (Some dummy)
        | List ->
          (match value.it with
          | VarE id ->
            let dummy = ctx.fresh_dummy exp.note in
            let env'' = Environment.add_binding env' exp dummy in
            let value = IterE (value, (List, [(id, value.note $ no_region, [])])) $$ (no_region % e_iter.note) in
            Environment.set_dummy env'' dummy (Some value)
          | _ -> assert false)
        )
    and path_mapsto_env_aux env path =
      match path.it with
      | RootP -> env
      | IdxP (p, e) -> 
        let env' = path_mapsto_env_aux env p in
        exp_mapsto_env_aux env' e
      | SliceP (p, l, n) ->
        let env' = path_mapsto_env_aux env p in
        let env'' = exp_mapsto_env_aux env' l in
        exp_mapsto_env_aux env'' n
      | DotP (p, _) -> path_mapsto_env_aux env p
    in
    exp_mapsto_env_aux env exp
end

(* ─── Scope: enter and leave an iteration scope ───────────────────────────── *)

module Scope = struct
  let rec descend env exp loc =
    match exp.it with
    | IterE (inner_exp, _) -> (
      match Environment.find_val env loc with
      | Some (Some value) -> (
        let _ = Format.printf "++ %s %s\n" (Print.string_of_exp inner_exp) (Print.string_of_exp value) in
        match value.it with
        | IterE (inner_loc, _) -> (
          let env' = descend env inner_exp inner_loc in
          Environment.add_binding env' inner_exp inner_loc)
        | _ -> env
      ) 
      | _ -> assert false
    )
    | _ -> env
end

(*module Scope = struct

  (** Build the inner environment for an iter scope.
      Each itervar's outer (iterated) binding is unwrapped to its element form
      so that premises inside the scope see the element-level variable. *)
  let descend env itervars =
    List.fold_left (fun ie (itervar : var) ->
      let iterid, typ, _iters = itervar in
      let inner_var = VarE iterid $$ (iterid.at % typ.it) in
      let inner_val =
        match SharedExp.find_opt inner_var env with
        | Some v -> v                    (* v : exp option — propagate directly *)
        | None ->
          (match Env.find_by_id iterid env with
          | Some v -> v                  (* v : exp option — propagate directly *)
          | None -> Some inner_var)      (* not found: wrap as present *)
      in
      SharedExp.add inner_var inner_val ie
    ) env itervars

  (** Merge inner-scope results back into the outer environment:
      1. Propagate refinements made to pre-existing outer vars.
      2. Add newly bound element-level vars with deep-resolved outer sources
         so the ancestry chain reaches back to the original input.
      3. Wrap newly bound vars in IterE and add outer-level bindings. *)
  let ascend outer_env inner_env new_inner_env new_inner_vars iterexp =
    let env_propagated = SharedExp.fold (fun k v e ->
      match SharedExp.find_opt k e with
      | Some _ ->
        let old_inner = match SharedExp.find_opt k inner_env with
          | Some ov -> ov | None -> Some k
        in
        let changed = match old_inner, v with
          | Some oa, Some vb -> compare_exp_canon oa vb <> 0
          | None, None -> false
          | _ -> true
        in
        if changed then SharedExp.add k v e else e
      | None -> e
    ) new_inner_env outer_env in
    let newly_bound = List.filter (fun v ->
      not (SharedExp.mem v outer_env)
    ) new_inner_vars in
    let env_with_elem = List.fold_left (fun e v ->
      let inner_src = match SharedExp.find_opt v new_inner_env with
        | Some (Some s) -> s | _ -> v
      in
      SharedExp.add v (Env.resolve_deep outer_env inner_src) e
    ) env_propagated newly_bound in
    let outer_vars = List.map (fun v ->
      IterE (v, iterexp) $$ (v.at % IterT (v.note $ no_region, fst iterexp))
    ) newly_bound in
    let env' = List.fold_left (fun e ov ->
      let elem_v = match ov.it with IterE (v, _) -> v | _ -> ov in
      let inner_src = match SharedExp.find_opt elem_v new_inner_env with
        | Some (Some s) -> s | _ -> elem_v
      in
      let outer_val = match Env.resolve_deep outer_env inner_src with
        | Some root_src ->
          Some (IterE (root_src, iterexp) $$ (root_src.at % IterT (root_src.note $ no_region, fst iterexp)))
        | None -> None
      in
      SharedExp.add ov outer_val e
    ) env_with_elem outer_vars in
    MEM (env', outer_vars)

end*)

(* ─── Pattern: bind lhs patterns against resolved values ──────────────────── *)

module Pattern = struct

  let get_id exp = 
    match exp.it with
    | VarE id -> id
    | _ -> assert false

  (** Match [lhs] against [value], extending [env] with new variable bindings.
      [rhs] is the original (pre-resolution) rhs expression; it is used to
      synthesise DotE projections when the struct value is structurally unknown. *)
  let rec bind (ctx : checker) lhs location env =
    (* Long Term TO DO *)
    match lhs.it with
    | VarE _ -> Environment.add_binding env lhs location, []
    | CaseE (_, sub_exps) -> (
      match Environment.find_val env location with
      | Some (Some value) ->(
        match value.it with
        | CaseE (_, sub_dummies) -> 
          (List.fold_left2 (fun env exp dummy -> Environment.add_binding env exp dummy) env sub_exps sub_dummies), []
        | _ -> assert false
      )
      | _ -> assert false
    )
    | OptE (Some inner_exp) ->
      (match Environment.find_val env location with
       | Some (Some value) ->
        (match value.it with
        | IterE (location, _) -> 
          (
            match inner_exp.note with
            | IterT _ -> (
              let env' = Scope.descend env inner_exp location in
              Environment.add_binding env' inner_exp location, [lhs]
            )
            | _ -> bind ctx inner_exp location env
          )
        | _ -> assert false)
       | _ -> assert false
      )
    | IterE (inner_exp, (Opt, _)) -> 
      (match Environment.find_val env location with
       | Some (Some value) ->
        (match value.it with
        | VarE _ -> Environment.add_binding env lhs location, [lhs]
        | IterE (location, _) -> bind ctx inner_exp location env
        | _ -> assert false)
       | _ -> assert false
      )
    | IterE _ -> (* To Do *) assert false
    | StrE _ -> (* Long Term To Do *) assert false
    | _ -> env, []

end

(* ─── Totality stub (long-term TODO) ──────────────────────────────────────── *)

let total_check spec envs params : bool * string =
  (* Long Term TO DO *)
  let _ = spec, envs, params in
  false, "msg"

(* ─── Premise analysis ────────────────────────────────────────────────────── *)

(** Process a single premise against [env], returning an updated [output].
    FAIL and OTHERWISE propagate immediately without further processing. *)
let rec prem_binds_variable (ctx : checker) prem env =
  match prem.it with
  | RulePr _ | IfHoldPr _ | IfNotHoldPr _ ->
    FAIL ("Function includes relation call " ^ Print.string_of_prem prem)
  | DebugPr _ -> FAIL "Function includes debug premise"
  | ElsePr -> OTHERWISE
  | IfPr exp -> (
    match exp.it with
    | MatchE (inner_exp, pattern) ->
      let location = Environment.find_dummy env inner_exp in
      (
        match pattern with
        | CaseP mixop -> (
            let field_typs = Spec.field_typs_of_mixop ctx.spec inner_exp mixop in
            let dummies = List.map (fun (typ : typ) -> ctx.fresh_dummy typ.it) field_typs in
            let case_exp = CaseE (mixop, dummies) $$ (inner_exp.at % inner_exp.note) in
            let env' = Environment.set_dummy env location (Some case_exp) in
            let env'' = List.fold_left (fun e d -> Environment.set_dummy e d (Some d)) env' dummies in
            MEM (env'', dummies)
          )
        | OptP `Some -> (
          match inner_exp.it with
          | IterE (iter_exp, _) -> (
              let env' = Vars.exp_mapsto_env env ctx iter_exp in
              let dummy = Environment.find_dummy env' iter_exp in
              let location = Environment.find_dummy env' inner_exp in
              let env'' = Environment.set_dummy env' location (Some (IterE (dummy, (Opt, [(Pattern.get_id dummy, dummy.note $ no_region, [])])) $$ (no_region % inner_exp.note))) in
              MEM (env'', [inner_exp])
            )
          | _ -> assert false
        )
        | OptP `None -> (
          let env' = Environment.set_dummy env location None in
          MEM (env', [exp])
        )
        | _ -> assert false
    )
    | SubE _ -> FAIL "Mid Term TO DO"
    | IterE (inner_cond, iterexp) ->
      (* Enter the iterated context before evaluating the inner condition *)
      let _iter, _ = iterexp in
      let inner_prem = IfPr inner_cond $ prem.at in
      prem_binds_variable ctx inner_prem env
    | CmpE (`EqOp, _, lhs, rhs) ->
      let cmp_aux lhs rhs =
        match lhs.it, rhs.it with
        | CaseE (mixop, sub_exps), VarE _ ->
          (match Spec.find_variant_case ctx.spec lhs.note mixop with
          | Some field_typs when List.length field_typs = List.length sub_exps ->
            let location = Environment.find_dummy env rhs in
            Some (MEM (Environment.set_dummy env location (Some lhs), []))
          | _ -> None
          )
        | _ -> None
      in
      (match cmp_aux lhs rhs with
      | Some ans -> ans
      | None -> (
        match cmp_aux rhs lhs with
        | Some ans -> ans
        | None -> assert false
      ))
    | _ -> FAIL "Function includes conditional premise\n"
    )
  | LetPr (lhs, rhs) ->
    (match Environment.find_dummy_opt env rhs with
    | None -> (* when it is compound expression *)
      let dummy = (ctx.fresh_dummy lhs.note) in
      let env' = Environment.add_binding env lhs dummy in
      let env'' = Environment.set_dummy env' dummy (Some rhs) in
      MEM (env'', [lhs])
    | Some location -> (* when it is simple expression *)
      let (env', new_vars) = Pattern.bind ctx lhs location env in
      MEM (env', new_vars))
  | IterPr (inner_prem, iterexp) ->
    (* Descent into the scope, process inner premise, then ascend *)
    let _iter, _ = iterexp in
    prem_binds_variable ctx inner_prem env

(** Thread [env] through each premise in order.
    Prints the environment before each step for debugging.
    Short-circuits on FAIL or OTHERWISE. *)
let iterate_prems (ctx : checker) env prems =
  let rec loop env = function
    | [] ->
      let _ = Env.debug env in
      MEM (env, [])
    | prem :: rest ->
      let _ = Env.debug env in
      let _ = Format.printf "\n-- %s\n" (Print.string_of_prem prem) in
      (match prem_binds_variable ctx prem env with
      | MEM (updated_env, _) -> loop updated_env rest
      | output -> output)
  in
  loop env prems

(** Analyse a single clause: initialise the environment from the argument
    patterns, then thread it through all premises. *)
let iterate_clause (ctx : checker) (clause : clause) =
  let args, _, prems = clause.it in
  let expargs = List.map (fun arg ->
    match arg.it with
    | ExpA exp -> exp
    | DefA _ ->
      let _ = Format.printf "Do not support function parameter\n" in
      assert false
  ) args in
  let init_env : Environment.t =
    List.fold_left (fun env exp -> Vars.exp_mapsto_env env ctx exp) Environment.empty expargs
  in
  match iterate_prems ctx init_env prems with
  | MEM (env, _) -> Env.extract expargs env
  | output -> output

(* ─── Entry points ────────────────────────────────────────────────────────── *)

let is_funcdef_total spec (funcdef : tparam list * param list * clause list) =
  let tparams, params, clauses = funcdef in
  if List.length tparams <> 0 then
    let _ = Format.printf "Currently do not support generic function\n" in
    assert false
  else
    let ctx = make_checker spec in
    let envs = List.map (iterate_clause ctx) clauses in
    total_check spec envs params

let are_funcdefs_total spec =
  let funcdefs = List.filter_map (fun def ->
    match def.it with
    | DecD (_, tparams, params, _, clauses) -> Some (tparams, params, clauses)
    | _ -> None
  ) spec in
  List.map (is_funcdef_total spec) funcdefs