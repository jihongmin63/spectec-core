open Lang.Il

(* -------------------------------------------------------------------------- *)
(* Premise Environment                                                         *)
(* Statically analyzes IL premises to map expressions to their canonical      *)
(* forms. Uses a union-find structure with dummy intermediary nodes so that    *)
(* type narrowing (IfPr) propagates to all aliases automatically.              *)
(* -------------------------------------------------------------------------- *)

type dummy = int

type prem_env = {
  pe_forward  : (exp * dummy) list;
  pe_backward : (dummy * exp) list;
  pe_next_id  : int;
}

let empty_prem_env = { pe_forward = []; pe_backward = []; pe_next_id = 0 }

let exp_equal e1 e2 = Eq.eq_exp e1 e2

(* Recursively substitute from_e with to_e in all sub-expressions of in_e *)
let rec subst_exp (from_e : exp) (to_e : exp) (in_e : exp) : exp =
  if exp_equal in_e from_e then to_e
  else { in_e with it = subst_exp_inner from_e to_e in_e.it }

and subst_exp_inner (from_e : exp) (to_e : exp) : exp' -> exp' = function
  | CaseE notexp           -> CaseE (Mixfix.map (subst_exp from_e to_e) notexp)
  | IterE (e, ie)          -> IterE (subst_exp from_e to_e e, ie)
  | TupleE es              -> TupleE (List.map (subst_exp from_e to_e) es)
  | ListE es               -> ListE (List.map (subst_exp from_e to_e) es)
  | ConsE (e1, e2)         -> ConsE (subst_exp from_e to_e e1, subst_exp from_e to_e e2)
  | CatE (e1, e2)          -> CatE (subst_exp from_e to_e e1, subst_exp from_e to_e e2)
  | OptE (Some e)          -> OptE (Some (subst_exp from_e to_e e))
  | UnE (op, ot, e)        -> UnE (op, ot, subst_exp from_e to_e e)
  | BinE (op, ot, e1, e2)  -> BinE (op, ot, subst_exp from_e to_e e1, subst_exp from_e to_e e2)
  | CmpE (op, ot, e1, e2)  -> CmpE (op, ot, subst_exp from_e to_e e1, subst_exp from_e to_e e2)
  | MatchE (e, p)          -> MatchE (subst_exp from_e to_e e, p)
  | LenE e                 -> LenE (subst_exp from_e to_e e)
  | DotE (e, a)            -> DotE (subst_exp from_e to_e e, a)
  | IdxE (e1, e2)          -> IdxE (subst_exp from_e to_e e1, subst_exp from_e to_e e2)
  | SliceE (e1, e2, e3)    -> SliceE (subst_exp from_e to_e e1, subst_exp from_e to_e e2, subst_exp from_e to_e e3)
  | UpdE (e1, p, e2)       -> UpdE (subst_exp from_e to_e e1, subst_path from_e to_e p, subst_exp from_e to_e e2)
  | UpCastE (t, e)         -> UpCastE (t, subst_exp from_e to_e e)
  | DownCastE (t, e)       -> DownCastE (t, subst_exp from_e to_e e)
  | SubE (e, t)            -> SubE (subst_exp from_e to_e e, t)
  | StrE fields            -> StrE (List.map (fun (a, e) -> (a, subst_exp from_e to_e e)) fields)
  | MemE (e1, e2)          -> MemE (subst_exp from_e to_e e1, subst_exp from_e to_e e2)
  | CallE (id, targs, args) ->
    let subst_arg (a : arg) = match a.it with
      | ExpA e -> { a with it = ExpA (subst_exp from_e to_e e) }
      | DefA _ -> a
    in
    CallE (id, targs, List.map subst_arg args)
  | e' -> e'

and subst_path (from_e : exp) (to_e : exp) (p : path) : path =
  { p with it = match p.it with
    | RootP               -> RootP
    | DotP (p', a)        -> DotP (subst_path from_e to_e p', a)
    | IdxP (p', e)        -> IdxP (subst_path from_e to_e p', subst_exp from_e to_e e)
    | SliceP (p', e1, e2) -> SliceP (subst_path from_e to_e p',
                                     subst_exp from_e to_e e1,
                                     subst_exp from_e to_e e2) }

let lookup_dummy (env : prem_env) (e : exp) : dummy option =
  List.find_map (fun (e', d) -> if exp_equal e e' then Some d else None)
    env.pe_forward

let resolve_prem_env (env : prem_env) (e : exp) : exp =
  match lookup_dummy env e with
  | None   -> e
  | Some d -> Option.value ~default:e (List.assoc_opt d env.pe_backward)

(* Higher generality = more abstract/general type. The more specific value
   becomes the canonical representative of the equivalence class. *)
let generality (e : exp) : int =
  match e.it with
  | VarE _            -> 2
  | CaseE _ | CallE _ -> 1
  | _                 -> 0

let more_specific (a : exp) (b : exp) : exp =
  (* Strict less-than: ties (equal generality) resolve to b (second arg).
     This ensures the RHS of LetPr is chosen as canonical when both sides
     are equally general (e.g., both VarE). *)
  if generality a < generality b then a else b

(* Substitute non_canon → canon in all pe_backward values when non_canon is a
   plain VarE.  This propagates narrowing info into already-stored canonicals:
   e.g., if backward[d0]=CaseE(b x) and we just learn x≡CaseE(a), backward[d0]
   is updated to CaseE(b CaseE(a)) automatically. *)
let propagate_backward (env : prem_env) (non_canon : exp) (canon : exp) : prem_env =
  match non_canon.it with
  | VarE _ ->
    { env with pe_backward =
      List.map (fun (d, e) -> (d, subst_exp non_canon canon e)) env.pe_backward }
  | _ -> env

(* Union-find merge: adds (a, b) as an equivalence.
   Reuses existing dummy nodes rather than allocating fresh ones.
   After each canonical update, propagates VarE substitutions into pe_backward
   so that canonical values referencing the old variable get updated. *)
let add_pair (env : prem_env) (a : exp) (b : exp) : prem_env =
  if exp_equal a b then env
  else
    let d_a = lookup_dummy env a in
    let d_b = lookup_dummy env b in
    match d_a, d_b with
    | Some d, Some d' when d = d' -> env
    | Some d, Some d' ->
        (* Absorb d' class into d; pick more specific canonical *)
        let forward' =
          List.map (fun (e, x) -> (e, if x = d' then d else x)) env.pe_forward
        in
        let canon_d  = List.assoc d  env.pe_backward in
        let canon_d' = List.assoc d' env.pe_backward in
        let new_canon = more_specific canon_d canon_d' in
        let non_canon = if exp_equal new_canon canon_d then canon_d' else canon_d in
        let backward' =
          (d, new_canon)
          :: List.filter (fun (x, _) -> x <> d && x <> d') env.pe_backward
        in
        let env' = { env with pe_forward = forward'; pe_backward = backward' } in
        propagate_backward env' non_canon new_canon
    | Some d, None ->
        (* b is new; a already has dummy d *)
        let canon     = List.assoc d env.pe_backward in
        let new_canon = more_specific canon b in
        let non_canon = if exp_equal new_canon b then canon else b in
        let env' = { env with
          pe_forward  = (b, d) :: env.pe_forward;
          pe_backward = (d, new_canon) :: List.remove_assoc d env.pe_backward } in
        propagate_backward env' non_canon new_canon
    | None, Some d ->
        (* a is new; b already has dummy d *)
        let canon     = List.assoc d env.pe_backward in
        let new_canon = more_specific a canon in
        let non_canon = if exp_equal new_canon a then canon else a in
        let env' = { env with
          pe_forward  = (a, d) :: env.pe_forward;
          pe_backward = (d, new_canon) :: List.remove_assoc d env.pe_backward } in
        propagate_backward env' non_canon new_canon
    | None, None ->
        let d     = env.pe_next_id in
        let canon = more_specific a b in
        let non_canon = if exp_equal canon a then b else a in
        let env' = { pe_forward  = (a, d) :: (b, d) :: env.pe_forward;
                     pe_backward = (d, canon) :: env.pe_backward;
                     pe_next_id  = d + 1 } in
        propagate_backward env' non_canon canon

let add_pairs (env : prem_env) (pairs : (exp * exp) list) : prem_env =
  List.fold_left (fun env (a, b) -> add_pair env a b) env pairs

(* Extract (non-canonical, canonical) pairs from env *)
let pairs_of_env (env : prem_env) : (exp * exp) list =
  List.filter_map (fun (e, d) ->
    match List.assoc_opt d env.pe_backward with
    | Some c when not (exp_equal e c) -> Some (e, c)
    | _ -> None)
  env.pe_forward

(* Check if prem_env contains a structural contradiction: two expressions
   in the same equivalence class with incompatible concrete forms.
   Conservative: only flags definite incompatibilities (literal mismatch,
   different CaseE constructors, None vs Some). *)
let has_contradiction (env : prem_env) : bool =
  List.exists (fun (e, d) ->
    match List.assoc_opt d env.pe_backward with
    | None   -> false
    | Some c ->
      if exp_equal e c then false
      else match e.it, c.it with
      | VarE _, _ | _, VarE _ -> false
      | BoolE x,  BoolE y     -> x <> y
      | NumE _,   NumE _      -> not (exp_equal e c)
      | TextE x,  TextE y     -> x <> y
      | CaseE ne, CaseE nc    ->
          not (Mixfix.eq_mixop (Mixfix.to_mixop ne) (Mixfix.to_mixop nc))
      | OptE None, OptE (Some _)
      | OptE (Some _), OptE None -> true
      | _ -> false)
  env.pe_forward

(* Build IterE(e, iterexp) with the correct IterT type annotation *)
let mk_iter_exp (e : exp) ((iter, vars) : iterexp) : exp =
  let open Common.Source in
  let typ = IterT (e.note $ e.at, iter) in
  IterE (e, (iter, vars)) $$ (e.at % typ)

(* Placeholder for relation output positions whose value is unknown statically *)
let dummy_exp (e : exp) (name : string) : exp =
  let open Common.Source in
  VarE (name $ e.at) $$ (e.at % e.note)

(* Look up a 0-arg variant constructor in the spec and build its CaseE expression.
   Returns None if the constructor has arguments or is not found. *)
let find_zero_arg_case (spec : spec) (e_typ : typ') (mixop : mixop) : exp option =
  match e_typ with
  | VarT (id, _) ->
    let open Common.Source in
    List.find_map (fun def ->
      match def.it with
      | TypD (tid, _, { it = VariantT cases; _ }) when tid.it = id.it ->
        List.find_map (fun (nottyp, _, _) ->
          if Mixfix.arity nottyp.it = 0 && Mixfix.eq_mixop (Mixfix.to_mixop nottyp.it) mixop
          then Some (CaseE (Mixfix.fill mixop []) $$ (no_region % e_typ))
          else None
        ) cases
      | _ -> None
    ) spec
  | _ -> None

let env_of_if_exp (spec : spec) (env : prem_env) (exp : exp) : prem_env =
  match exp.it with
  | CmpE (`EqOp, _, a, b) -> add_pair env a b
  | MatchE (e, CaseP mixop) ->
      (* For 0-arg constructors (e.g., `if x matches `A``) we know the exact
         canonical form.  Multi-arg constructors are handled by the subsequent
         LetPr that destructures the matched value. *)
      (match find_zero_arg_case spec e.note mixop with
       | Some case_exp -> add_pair env e case_exp
       | None          -> env)
  | _ -> env

let env_of_rule_prem (env : prem_env) (spec : spec) (id : id) (notexp : notexp) : prem_env =
  match Qc_elab.find_rel_in_spec spec id.it with
  | None -> env
  | Some (nottyp, inputs) ->
    let arg_types = Mixfix.args nottyp.it in
    let args      = Mixfix.args notexp in
    let n         = min (List.length arg_types) (List.length args) in
    List.init n Fun.id
    |> List.filter (fun i -> not (List.mem i inputs))
    |> List.fold_left (fun env i ->
         let out_exp = List.nth args i in
         let dummy_name = Printf.sprintf "_out_%s_%d" id.it i in
         add_pair env out_exp (dummy_exp out_exp dummy_name))
       env

let rec env_of_prem (spec : spec) (env : prem_env) (prem : prem) : prem_env =
  match prem.it with
  | LetPr (a, b) ->
      add_pair env a b
  | IfPr exp ->
      env_of_if_exp spec env exp
  | IterPr (inner_prem, (List, vars)) ->
      (* Element-level bindings from inner premise *)
      let inner_env  = env_of_prem spec empty_prem_env inner_prem in
      let elem_pairs = pairs_of_env inner_env in
      (* List-level lifted bindings: wrap both sides in IterE *)
      let list_pairs = List.map (fun (a, b) ->
        (mk_iter_exp a (List, vars), mk_iter_exp b (List, vars))) elem_pairs in
      add_pairs (add_pairs env elem_pairs) list_pairs
  | IterPr (inner_prem, (Opt, vars)) ->
      (* Optional: content may not exist, so skip element-level bindings.
         Only add opt-level lifted bindings. *)
      let inner_env  = env_of_prem spec empty_prem_env inner_prem in
      let elem_pairs = pairs_of_env inner_env in
      let opt_pairs  = List.map (fun (a, b) ->
        (mk_iter_exp a (Opt, vars), mk_iter_exp b (Opt, vars))) elem_pairs in
      add_pairs env opt_pairs
  | RulePr (id, notexp) ->
      env_of_rule_prem env spec id notexp
  | IfHoldPr _ | IfNotHoldPr _ | ElsePr | DebugPr _ -> env

let env_of_prems (spec : spec) (prems : prem list) : prem_env =
  List.fold_left (fun env prem -> env_of_prem spec env prem) empty_prem_env prems
