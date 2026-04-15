open Common.Source
open Il
open Canon_compare

module SharedExp = Map.Make(struct
  type t = exp
  let compare = compare_exp_canon
end)

type output =
  | TEMP of exp list  (** Temporary output: only extracted inputs *)
  | MEM of exp SharedExp.t * exp list
  | OTHERWISE         (** Hit an ElsePr branch *)
  | FAIL of string    (** Unsupported construct *)

(* ─── Env: operations on the SharedExp environment ────────────────────────── *)

module Env = struct

  (** Resolve every variable in [keys] to its fully-expanded form by following
      the env chain transitively.  The result is wrapped in TEMP so callers can
      pattern-match on it alongside other [output] variants. *)
  let extract keys env =
    let rec exact_exp (exp : exp) : exp =
      match SharedExp.find_opt exp env with
      | None -> exp
      | Some value -> (
        match value.it with
        | UnE (unop, optyp, e) -> UnE (unop, optyp, exact_exp e) $$ (value.at % value.note)
        | BinE (bop, otyp, l, r) -> BinE (bop, otyp, exact_exp l, exact_exp r) $$ (value.at % value.note)
        | CmpE (cop, otyp, l, r) -> CmpE (cop, otyp, exact_exp l, exact_exp r) $$ (value.at % value.note)
        | UpCastE (typ, e) -> UpCastE (typ, exact_exp e) $$ (value.at % value.note)
        | DownCastE (typ, e) -> DownCastE (typ, exact_exp e) $$ (value.at % value.note)
        | SubE (e, typ) -> SubE (exact_exp e, typ) $$ (value.at % value.note)
        | MatchE (e, pat) -> MatchE (exact_exp e, pat) $$ (value.at % value.note)
        | TupleE es -> TupleE (List.map exact_exp es) $$ (value.at % value.note)
        | CaseE (mixop, es) -> CaseE (mixop, List.map exact_exp es) $$ (value.at % value.note)
        | StrE fields ->
          StrE (List.map (fun (atom, e) -> atom, exact_exp e) fields) $$ (value.at % value.note)
        | OptE None -> OptE None $$ (value.at % value.note)
        | OptE (Some e) -> OptE (Some (exact_exp e)) $$ (value.at % value.note)
        | ListE es -> ListE (List.map exact_exp es) $$ (value.at % value.note)
        | ConsE (l, r) -> ConsE (exact_exp l, exact_exp r) $$ (value.at % value.note)
        | CatE (l, r) -> CatE (exact_exp l, exact_exp r) $$ (value.at % value.note)
        | MemE (l, r) -> MemE (exact_exp l, exact_exp r) $$ (value.at % value.note)
        | LenE e -> LenE (exact_exp e) $$ (value.at % value.note)
        | DotE (e, atom) -> DotE (exact_exp e, atom) $$ (value.at % value.note)
        | IdxE (l, r) -> IdxE (exact_exp l, exact_exp r) $$ (value.at % value.note)
        | SliceE (e, l, n) -> SliceE (exact_exp e, exact_exp l, exact_exp n) $$ (value.at % value.note)
        | UpdE (e, path, v) -> UpdE (exact_exp e, exact_path path, exact_exp v) $$ (value.at % value.note)
        | IterE (e, iterexp) -> IterE (exact_exp e, iterexp) $$ (value.at % value.note)
        | _ -> value
      )
    and exact_path path =
      match path.it with
      | RootP -> path
      | IdxP (p, e) -> IdxP (exact_path p, exact_exp e) $$ (path.at % path.note)
      | SliceP (p, l, n) -> SliceP (exact_path p, exact_exp l, exact_exp n) $$ (path.at % path.note)
      | DotP (p, atom) -> DotP (exact_path p, atom) $$ (path.at % path.note)
    in
    TEMP (List.map exact_exp keys)

  let debug env =
    let pairs = SharedExp.bindings env in
    let _ = Format.printf "\n[ENV]\n" in
    List.map (fun (k, v) ->
      Format.printf "%s -> %s\n" (Print.string_of_exp k) (Print.string_of_exp v)
    ) pairs

  let find_by_id (target_id : id) (env : exp SharedExp.t) : exp option =
    SharedExp.bindings env
    |> List.find_map (fun (k, v) ->
      match k.it with
      | VarE id when id.it = target_id.it -> Some v
      | _ -> None
    )

  (** Follow the env chain until stable — stops when a value maps to itself
      or is absent.  Used for rhs resolution and iter source-root lookup. *)
  let rec resolve_deep env exp =
    match SharedExp.find_opt exp env with
    | Some v when compare_exp_canon v exp <> 0 -> resolve_deep env v
    | Some v -> v
    | None -> exp

end

(* ─── Vars: extract variable sub-expressions from terms ───────────────────── *)

module Vars = struct

  let rec filter_id exp =
    match exp.it with
    | VarE id -> id
    | IterE (e, _) -> filter_id e
    | _ -> assert false

  let rec of_exp exp =
    match exp.it with
    | VarE _ -> [exp]
    | UnE (_, _, e) | UpCastE (_, e) | DownCastE (_, e)
    | SubE (e, _) | MatchE (e, _) | LenE e | DotE (e, _) ->
      of_exp e
    | BinE (_, _, l, r) | CmpE (_, _, l, r)
    | ConsE (l, r) | CatE (l, r) | MemE (l, r) | IdxE (l, r) ->
      of_exp l @ of_exp r
    | ListE es | TupleE es ->
      List.concat_map of_exp es
    | OptE None -> []
    | OptE (Some e) -> of_exp e
    | CaseE (_, es) -> List.concat_map of_exp es
    | StrE fields -> List.concat_map (fun (_, e) -> of_exp e) fields
    | SliceE (e, l, n) -> of_exp e @ of_exp l @ of_exp n
    | UpdE (e, path, new_e) -> of_exp e @ of_path path @ of_exp new_e
    | IterE (e_iter, iterexp) ->
      let vars = of_exp e_iter in
      let iter, itervars = iterexp in
      (match iter with
      | Opt -> [exp]
      | List ->
        vars @
        List.filter_map (fun var ->
          if List.exists (fun (itervar : var) ->
              let id = filter_id var in
              let iterid, _, _ = itervar in
              id.it = iterid.it
            ) itervars
          then Some (IterE (var, iterexp) $$ (no_region % IterT (var.note $ no_region, fst iterexp)))
          else None
        ) vars)
    | BoolE _ | NumE _ | TextE _ | CallE _ -> []
  and of_path path =
    match path.it with
    | RootP -> []
    | IdxP (p, e) -> of_path p @ of_exp e
    | SliceP (p, l, n) -> of_path p @ of_exp l @ of_exp n
    | DotP (p, _) -> of_path p

end

(* ─── Spec: look up typing information from the spec ──────────────────────── *)

module Spec = struct

  (** Return the field types for [target_mixop] in the variant type that
      [exp]'s type annotation resolves to, or [] if not found. *)
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

end

(* ─── Scope: enter and leave an iteration scope ───────────────────────────── *)

module Scope = struct

  (** Build the inner environment for an iter scope.
      Each itervar's outer (iterated) binding is unwrapped to its element form
      so that premises inside the scope see the element-level variable. *)
  let descend env itervars =
    List.fold_left (fun ie (itervar : var) ->
      let iterid, typ, _iters = itervar in
      let inner_var = VarE iterid $$ (iterid.at % typ.it) in
      let inner_val =
        match SharedExp.find_opt inner_var env with
        | Some v -> v
        | None ->
          (match Env.find_by_id iterid env with
          | Some v -> v
          | None -> inner_var)
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
      SharedExp.add v (Env.resolve_deep outer_env inner_src) e
    ) env_propagated newly_bound in
    let outer_vars = List.map (fun v ->
      IterE (v, iterexp) $$ (v.at % IterT (v.note $ no_region, fst iterexp))
    ) newly_bound in
    let env' = List.fold_left (fun e ov ->
      let elem_v = match ov.it with IterE (v, _) -> v | _ -> ov in
      let inner_src = match SharedExp.find_opt elem_v new_inner_env with
        | Some s -> s | None -> elem_v
      in
      let root_src = Env.resolve_deep outer_env inner_src in
      let outer_src = IterE (root_src, iterexp) $$ (root_src.at % IterT (root_src.note $ no_region, fst iterexp)) in
      SharedExp.add ov outer_src e
    ) env_with_elem outer_vars in
    MEM (env', outer_vars)

end

(* ─── Pattern: bind lhs patterns against resolved values ──────────────────── *)

module Pattern = struct

  (** Match [lhs] against [value], extending [env] with new variable bindings.
      [rhs] is the original (pre-resolution) rhs expression; it is used to
      synthesise DotE projections when the struct value is structurally unknown. *)
  let rec bind rhs lhs value env =
    match lhs.it with
    | VarE _ ->
      (SharedExp.add lhs value env, [lhs])
    | StrE fields ->
      (match value.it with
      | StrE value_fields ->
        (* Known struct: bind each field variable to the matching sub-value *)
        List.fold_left2 (fun (e, acc) (_, lhs_field) (_, val_field) ->
          let (e', vars') = bind rhs lhs_field val_field e in
          (e', acc @ vars')
        ) (env, []) fields value_fields
      | _ ->
        (* Unknown struct: vi → DotE(rhs, ai) *)
        List.fold_left (fun (e, acc) (atom, lhs_field) ->
          let dot_exp = DotE (rhs, atom) $$ (lhs_field.at % lhs_field.note) in
          let (e', vars') = bind rhs lhs_field dot_exp e in
          (e', acc @ vars')
        ) (env, []) fields)
    | CaseE (_, sub_exps) ->
      (match value.it with
      | CaseE (_, sub_vals) when List.length sub_exps = List.length sub_vals ->
        (* Known constructor: bind each sub-expression to the matching sub-value *)
        List.fold_left2 (fun (e, acc) lhs_sub val_sub ->
          let (e', vars') = bind rhs lhs_sub val_sub e in
          (e', acc @ vars')
        ) (env, []) sub_exps sub_vals
      | _ ->
        (* Unknown constructor: bind each sub-expression to itself *)
        List.fold_left (fun (e, acc) lhs_sub ->
          let (e', vars') = bind rhs lhs_sub lhs_sub e in
          (e', acc @ vars')
        ) (env, []) sub_exps)
    | IterE (inner_lhs, iterexp) ->
      (match fst iterexp with
      | Opt ->
        (* Optional iter may be absent — do not bind inner variables into env *)
        (SharedExp.add lhs value env, [lhs])
      | List ->
        let elem_val = match value.it with
          | IterE (inner_val, _) -> inner_val
          | _ -> value
        in
        let (env', vars') = bind rhs inner_lhs elem_val env in
        (* Ascent: wrap newly bound element vars in IterE, preserving the
           SharedExp ancestry chain back to the original input. *)
        let outer_vars = List.map (fun v ->
          IterE (v, iterexp) $$ (v.at % IterT (v.note $ no_region, fst iterexp))
        ) vars' in
        let env'' = List.fold_left2 (fun e ov v ->
          let inner_src = match SharedExp.find_opt v env' with
            | Some s -> s | None -> v
          in
          let outer_src = IterE (inner_src, iterexp) $$ (inner_src.at % IterT (inner_src.note $ no_region, fst iterexp)) in
          SharedExp.add ov outer_src e
        ) env' outer_vars vars' in
        (env'', outer_vars))
    | OptE None -> (env, [])
    | OptE (Some inner_lhs) ->
      (* ?(inner_lhs) pattern: match confirmed Some, descend into the value *)
      let inner_val = match value.it with
        | IterE (iv, _) -> iv
        | OptE (Some iv) -> iv
        | _ -> value
      in
      bind rhs inner_lhs inner_val env
    | _ -> (env, [])

end

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
      let id = ("_dummy#" ^ string_of_int !dummy) $ no_region in
      incr dummy;
      VarE id $$ (no_region % typ)
    );
  }

(* ─── Totality stub (long-term TODO) ──────────────────────────────────────── *)

let total_check spec envs params : bool * string =
  (* TO DO *)
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
    | MatchE (inner_exp, CaseP mixop) ->
      (* Step 1: resolve inner_exp one level — we update the VALUE it points to
         (not inner_exp itself) so the SharedExp ancestry chain stays intact. *)
      let resolved = match SharedExp.find_opt inner_exp env with
        | Some v -> v | None -> inner_exp
      in
      (* Step 2–3: look up field types from spec, create fresh dummies *)
      let field_typs = Spec.field_typs_of_mixop ctx.spec inner_exp mixop in
      let dummies = List.map (fun (typ : typ) -> ctx.fresh_dummy typ.it) field_typs in
      (* Step 4–5: refine the resolved target and register each dummy *)
      let case_exp = CaseE (mixop, dummies) $$ (inner_exp.at % inner_exp.note) in
      let env' = SharedExp.add resolved case_exp env in
      let env'' = List.fold_left (fun e d -> SharedExp.add d d e) env' dummies in
      MEM (env'', dummies)
    | MatchE (inner_exp, OptP `Some) ->
      (* Opt match: inner_exp is now known to be Some; extract and register vars *)
      let resolved = match SharedExp.find_opt inner_exp env with
        | Some v -> v | None -> inner_exp
      in
      let inner_val = match resolved.it with
        | IterE (iv, _) -> iv
        | OptE (Some iv) -> iv
        | _ -> resolved
      in
      let all_vars = Vars.of_exp inner_val in
      let env' = List.fold_left (fun e v ->
        if SharedExp.mem v e then e else SharedExp.add v v e
      ) env all_vars in
      MEM (env', all_vars)
    | IterE (inner_cond, iterexp) ->
      (* Enter the iterated context before evaluating the inner condition *)
      let _iter, itervars = iterexp in
      let inner_env = Scope.descend env itervars in
      let inner_prem = IfPr inner_cond $ prem.at in
      (match prem_binds_variable ctx inner_prem inner_env with
      | MEM (new_inner_env, new_inner_vars) ->
        Scope.ascend env inner_env new_inner_env new_inner_vars iterexp
      | output -> output)
    | _ ->
      (* SubE, CmpE, etc. — constrain the path but introduce no new variables *)
      MEM (env, [])
  )
  | LetPr (lhs, rhs) ->
    (* Resolve rhs transitively — a single lookup misses chains like
       rhs → _dummy#0 → CaseE(a, [_dummy#1]). *)
    let rhs_val = Env.resolve_deep env rhs in
    let (env', new_vars) = Pattern.bind rhs lhs rhs_val env in
    MEM (env', new_vars)
  | IterPr (inner_prem, iterexp) ->
    (* Descent into the scope, process inner premise, then ascend *)
    let _iter, itervars = iterexp in
    let inner_env = Scope.descend env itervars in
    (match prem_binds_variable ctx inner_prem inner_env with
    | MEM (new_inner_env, new_inner_vars) ->
      Scope.ascend env inner_env new_inner_env new_inner_vars iterexp
    | output -> output)

(** Thread [env] through each premise in order.
    Prints the environment before each step for debugging.
    Short-circuits on FAIL or OTHERWISE. *)
let iterate_prems (ctx : checker) env prems =
  let rec loop env = function
    | [] -> MEM (env, [])
    | prem :: rest ->
      let _ = Env.debug env in
      (match prem_binds_variable ctx prem env with
      | MEM (updated_env, _) -> loop updated_env rest
      | output -> output)
  in
  let _ = Env.debug env in
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
  let init_env : exp SharedExp.t =
    List.fold_left (fun env exp ->
      List.fold_left (fun env var -> SharedExp.add var var env) env (Vars.of_exp exp)
    ) SharedExp.empty expargs
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
