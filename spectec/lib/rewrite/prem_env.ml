open Lang.Il

(* -------------------------------------------------------------------------- *)
(* Premise Environment                                                         *)
(* Statically analyzes IL premises to map expressions to their canonical      *)
(* forms. Uses a union-find structure with dummy intermediary nodes so that    *)
(* type narrowing (IfPr) propagates to all aliases automatically.              *)
(* -------------------------------------------------------------------------- *)

type dummy = int

type prem_env = {
  pe_forward : (exp * dummy) list;
  pe_backward : (dummy * exp) list;
  pe_next_id : int;
}

let empty_prem_env = { pe_forward = []; pe_backward = []; pe_next_id = 0 }

(* Look up a relation's (nottyp, input-indices) in the IL spec by name.
   Inlined from quickcheck's Qc_elab.find_rel_in_spec. *)
let find_rel_in_spec (spec : spec) (rel_name : string) :
    (nottyp * int list) option =
  List.find_map
    (fun (def : def) ->
      match def.it with
      | RelD { relid; reltyp; _ } when relid.it = rel_name ->
          let nottyp = { reltyp with it = Mode.notation reltyp.it } in
          let n = List.length (Mixfix.args (Mode.notation reltyp.it)) in
          let inputs, _ = Mode.partition reltyp.it (List.init n Fun.id) in
          Some (nottyp, inputs)
      | _ -> None)
    spec

let exp_equal e1 e2 = Eq.eq_exp e1 e2

module IdSet = Common.Domain.IdSet

(* ---- Gensym-reaching ("effectful") calls --------------------------------- *)

(* The stateful gensym builtins ([builtin dec]s the interpreter implements as a
   global counter). A call reaching one of these mints a NEW name on every
   evaluation, so two occurrences of the same call expression denote two
   different values: the env must never fold such a call into an equivalence
   class, where canonicalisation/substitution would duplicate one instance into
   many (or conflate two). The rewrite backend instead threads a gensym state
   through these calls ({!Gensym}), relying on each call occurring exactly
   once. *)
let gensym_ids = [ "fresh_typeId" (* p4 *); "fresh_tid" (* p4-old *) ]

module StrSet = Set.Make (String)

let rec call_ids_of_exp (acc : StrSet.t) (e : exp) : StrSet.t =
  let acc =
    match e.it with CallE (id, _, _) -> StrSet.add id.it acc | _ -> acc
  in
  List.fold_left call_ids_of_exp acc (Exp_map.subexps e.it)

let callee_ids_of_prem (acc : StrSet.t) (p : prem) : StrSet.t =
  let rec rel_ids acc (p : prem) =
    match p.it with
    | RelPr { relid; _ } | RelAssertPr { call = { relid; _ }; _ } ->
        StrSet.add relid.it acc
    | IterPr (inner, _) -> rel_ids acc inner
    | _ -> acc
  in
  List.fold_left call_ids_of_exp (rel_ids acc p) (Exp_map.exps_of_prem p)

(* Each def/relation id with the ids its bodies invoke (function calls and
   relation premises), the call-graph edges of the fixpoint below. *)
let callee_ids_of_def (d : def) : (string * StrSet.t) option =
  match d.it with
  | DecD { defid; clauses; _ } ->
      let of_clause acc (c : clause) =
        let { args; body; prems } = c.it in
        let acc =
          List.fold_left
            (fun acc (a : arg) ->
              match a.it with ExpA e -> call_ids_of_exp acc e | DefA _ -> acc)
            acc args
        in
        List.fold_left callee_ids_of_prem (call_ids_of_exp acc body) prems
      in
      Some (defid.it, List.fold_left of_clause StrSet.empty clauses)
  | RelD { relid; rules; _ } ->
      let of_rule acc (r : rule) =
        let { concl; prems; _ } = r.it in
        let acc = List.fold_left call_ids_of_exp acc (Mixfix.args concl) in
        List.fold_left callee_ids_of_prem acc prems
      in
      Some (relid.it, List.fold_left of_rule StrSet.empty rules)
  | TypD _ | BuiltinDecD _ -> None

let compute_effectful (spec : spec) : StrSet.t =
  let edges = List.filter_map callee_ids_of_def spec in
  let rec grow s =
    let s' =
      List.fold_left
        (fun s (id, callees) ->
          if StrSet.mem id s || StrSet.disjoint callees s then s
          else StrSet.add id s)
        s edges
    in
    if StrSet.cardinal s' = StrSet.cardinal s then s else grow s'
  in
  grow (StrSet.of_list gensym_ids)

(* One-slot memo keyed by physical equality: {!Simplify} threads the same
   original spec value through every env build, and redoing the call-graph
   fixpoint per premise block would dominate the pass. *)
let effectful_memo : (spec * StrSet.t) option ref = ref None

let effectful_ids (spec : spec) : StrSet.t =
  match !effectful_memo with
  | Some (s, ids) when s == spec -> ids
  | _ ->
      let ids = compute_effectful spec in
      effectful_memo := Some (spec, ids);
      ids

(* Whether [e] contains a call that (transitively) reaches a gensym builtin --
   the calls the env keeps opaque (see [gensym_ids]). *)
let mentions_effectful_call (spec : spec) (e : exp) : bool =
  let eff = effectful_ids spec in
  let rec go (e : exp) : bool =
    (match e.it with CallE (id, _, _) -> StrSet.mem id.it eff | _ -> false)
    || List.exists go (Exp_map.subexps e.it)
  in
  go e

(* Every [VarE] occurrence in [e] paired with its type note. *)
let rec var_typs (e : exp) : (id * typ) list =
  let open Common.Source in
  let here = match e.it with VarE id -> [ (id, e.note $ e.at) ] | _ -> [] in
  here @ List.concat_map var_typs (Exp_map.subexps e.it)

(* The iteration binders for an iterated body that was [a] under binders [vars] and
   became [b] (e.g. reconstruction `id_arg?` -> `?(id_arg')`). Reusing [vars]
   verbatim leaves a stale binder -- `?(id_arg')*` still bound by `id_arg`, which
   no longer occurs in the body -- and [To_ctrs] then reads the body's real variable
   [id_arg'] as a captured constant, compiling a non-left-linear `$unzip` that the
   co-iterated `$itercollect` references cyclically. The genuine captured constants
   (free in [a] but not co-iterated) are unchanged by reconstruction, so [b]'s
   co-iterated variables are exactly [free b] minus them. Keep each original binder
   whose variable survives into [b]; synthesise an entry (type from [b], iterator
   depth from a surviving original) for each newly-varying variable. *)
(* The element variables an iteration binder list binds. *)
let binder_ids (vars : var list) : IdSet.t =
  List.fold_left
    (fun s ({ varid; _ } : var) -> IdSet.add varid s)
    IdSet.empty vars

let relift_vars (a : exp) (b : exp) (vars : var list) : var list =
  let var_ids = binder_ids vars in
  let captured = IdSet.diff (Free.free_exp a) var_ids in
  let want = IdSet.diff (Free.free_exp b) captured in
  if IdSet.equal want var_ids then vars
  else
    let kept =
      List.filter (fun ({ varid; _ } : var) -> IdSet.mem varid want) vars
    in
    let kept_ids = binder_ids kept in
    let iters = match vars with { iters; _ } :: _ -> iters | [] -> [] in
    let b_typs = var_typs b in
    let new_vars =
      IdSet.elements (IdSet.diff want kept_ids)
      |> List.filter_map (fun (id : id) ->
             match List.find_opt (fun ((i : id), _) -> i.it = id.it) b_typs with
             | Some (_, typ) -> Some { varid = id; typ; iters }
             | None -> None)
    in
    kept @ new_vars

(* The base numeric type a type denotes, seen through named [PlainT] aliases
   (`syntax byte = nat` resolves to `nat`), or [None] if it is not, even
   transitively, a number. *)
let rec resolve_num_typ (spec : spec) (t : typ') : Xl.Num.typ option =
  match t with
  | NumT n -> Some n
  | VarT { synid = tid; _ } ->
      List.find_map
        (fun (d : def) ->
          match d.it with
          | TypD { synid = id2; deftyp = { it = PlainT bt; _ }; _ }
            when id2.it = tid.it ->
              resolve_num_typ spec bt.it
          | _ -> None)
        spec
  | _ -> None

(* A cast across the nat/int boundary changes representation (bare Peano vs.
   sign-magnitude), so unlike a representation-preserving subtype cast it must
   survive into the translation, where [To_ctrs] turns it into an explicit
   [int_pos]/[nat_of_int] coercion. Resolved through aliases, so a cast via a
   named alias of `nat`/`int` is recognised too. Shared with {!Simplify}'s deep
   normalization, so the cast a [let]-binder keeps here and the cast that
   survives there are decided by the same predicate. *)
let is_num_cast (spec : spec) (target : typ') (source : typ') : bool =
  match (resolve_num_typ spec target, resolve_num_typ spec source) with
  | Some `IntT, Some `NatT | Some `NatT, Some `IntT -> true
  | _ -> false

(* See through cast wrappers: [e as t] (Up/DownCastE) is just a view of the same
   value, so for structural reasoning it is equivalent to [e]. Stripping it when
   forming equivalences turns `let lit = exp as lit` into the alias `lit ≡ exp`
   instead of `lit ≡ (exp as lit)` (which otherwise leaks the cast back into
   already-clean bindings). A nat/int cast is kept (see [is_num_cast]):
   collapsing `let n = int as nat` to the alias `n ≡ int` would drop the
   coercion and retype the bound field from int to nat. *)
let rec strip_casts (spec : spec) (e : exp) : exp =
  match e.it with
  | (UpCastE (t, e') | DownCastE (t, e'))
    when not (is_num_cast spec t.it e'.note) ->
      strip_casts spec e'
  | _ -> e

(* Synthetic variables standing in for relation output positions all share this
   prefix, so the rewrite pass can recognize them and avoid leaking them into
   the rewritten spec. *)
let hidden_out_prefix = "_out_"

(* A synthetic output placeholder produced by [env_of_rule_prem]. These only
   drive narrowing inside the environment; they are never substituted into the
   rewritten spec (a real output variable keeps its own name instead). *)
let is_hidden_out_id (id : id) : bool =
  String.starts_with ~prefix:hidden_out_prefix id.it

let is_hidden_out_var (e : exp) : bool =
  match e.it with VarE id -> is_hidden_out_id id | _ -> false

(* Recursively substitute from_e with to_e in all sub-expressions of in_e.
   [Exp_map.map_subexps] handles the one-level structural recursion; the
   whole-term equality check here decides where the substitution fires.

   Capture avoidance: an [IterE]'s [var list] binds element variables inside the
   iterated body, so an occurrence of one of them is the bound element, not the
   free variable being replaced. Substituting a bound element variable [v] there
   would push [v]'s reconstructed structure into the body while the iteration
   still binds [v] -- leaving the structure's own variables unbound (e.g. a head
   pattern [(param(direction, typeIR, ..))*{param <- param*}] whose [direction]
   is then projected back out of [param], a cycle). So when [from_e] is a bound
   element variable of an iteration, leave that iteration untouched. *)
let rec subst_exp (from_e : exp) (to_e : exp) (in_e : exp) : exp =
  if exp_equal in_e from_e then to_e
  else
    match in_e.it with
    | IterE (body, (iter, vars)) -> (
        let binds_from =
          match from_e.it with
          | VarE v ->
              List.exists (fun ({ varid; _ } : var) -> varid.it = v.it) vars
          | _ -> false
        in
        if binds_from then in_e
        else
          (* Substitute inside the body, then re-derive the binder: a substitution
             that reshapes the body (`id_arg?` -> `?(id_arg')`) changes which
             variables are co-iterated, and leaving [vars] stale makes [To_ctrs]
             emit a non-left-linear `$unzip` (see [relift_vars]). Re-derive ONLY
             when the replaced expression involved this iteration's own element
             variables -- that is what renames a co-iterated variable. When the
             substitution expanded a CAPTURED variable instead (`typeIR_enum` ->
             `ENUM nameIR { nameIR_field* }`), the new structure's variables are
             captured too; promoting them ([relift_vars]'s free-variable
             arithmetic would) makes the compiled helper recurse over a constant
             (the enum's name) as one of its spines, so no application matches. *)
          let body' = subst_exp from_e to_e body in
          if exp_equal body body' then in_e
          else if
            IdSet.is_empty
              (IdSet.inter (Free.free_exp from_e) (binder_ids vars))
          then { in_e with it = IterE (body', (iter, vars)) }
          else
            (* Binder fidelity: a substitution may reshape the body only if the
               iteration still binds at least one variable afterwards. Folding a
               constant into the sole binder (`id_arg? -> ?()`) would leave the
               degenerate `?()*{}` -- an iteration severed from its length and
               from every premise co-iterating the original collection, which a
               later collapse then erases entirely (the all-none guard of
               `$find_overloaded`'s unnamed clauses). Refuse instead. *)
            match relift_vars body body' vars with
            | [] when vars <> [] -> in_e
            | vars' -> { in_e with it = IterE (body', (iter, vars')) })
    | _ ->
        { in_e with it = Exp_map.map_subexps (subst_exp from_e to_e) in_e.it }

let lookup_dummy (env : prem_env) (e : exp) : dummy option =
  List.find_map
    (fun (e', d) -> if exp_equal e e' then Some d else None)
    env.pe_forward

let resolve_prem_env (env : prem_env) (e : exp) : exp =
  match lookup_dummy env e with
  | None -> e
  | Some d -> Option.value ~default:e (List.assoc_opt d env.pe_backward)

(* Whether [e]'s equivalence class contains a synthetic relation-output
   placeholder, i.e. [e] is (equated to) a relation output. The placeholder need
   not be the canonical representative (a real variable outranks it), so this
   checks class membership rather than the canonical form. *)
let in_hidden_class (env : prem_env) (e : exp) : bool =
  match lookup_dummy env e with
  | None -> false
  | Some d ->
      List.exists
        (fun (e', d') -> d' = d && is_hidden_out_var e')
        env.pe_forward

(* Higher generality = more abstract/general type. The more specific value
   becomes the canonical representative of the equivalence class. Synthetic
   output placeholders are the *least* specific (4): a real variable or a
   reconstructed structure must always win the canonical slot, otherwise the
   class collapses onto a placeholder that the substitution step then discards.
   An iteration `e?{..}`/`e*{..}` is the option/list value in variable-ish form,
   so a concrete `OptE`/`ListE`/`ConsE` for the same value (generality 0) is more
   specific and wins -- e.g. `?(type)` beats `type'?{..}`. *)
let generality (e : exp) : int =
  match e.it with
  | VarE _ when is_hidden_out_var e -> 4
  | VarE _ -> 3
  | IterE _ -> 2
  | CaseE _ | CallE _ -> 1
  | _ -> 0

let more_specific (a : exp) (b : exp) : exp =
  (* Strict less-than: ties (equal generality) resolve to b (second arg).
     This ensures the RHS of LetPr is chosen as canonical when both sides
     are equally general (e.g., both VarE). *)
  if generality a < generality b then a else b

(* Substitute non_canon → canon in all pe_backward values when non_canon is a
   plain VarE.  This propagates narrowing info into already-stored canonicals:
   e.g., if backward[d0]=CaseE(b x) and we just learn x≡CaseE(a), backward[d0]
   is updated to CaseE(b CaseE(a)) automatically. *)
let propagate_backward (env : prem_env) (non_canon : exp) (canon : exp) :
    prem_env =
  match non_canon.it with
  | VarE _ ->
      {
        env with
        pe_backward =
          List.map
            (fun (d, e) -> (d, subst_exp non_canon canon e))
            env.pe_backward;
      }
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
    match (d_a, d_b) with
    | Some d, Some d' when d = d' -> env
    | Some d, Some d' ->
        (* Absorb d' class into d; pick more specific canonical *)
        let forward' =
          List.map (fun (e, x) -> (e, if x = d' then d else x)) env.pe_forward
        in
        let canon_d = List.assoc d env.pe_backward in
        let canon_d' = List.assoc d' env.pe_backward in
        let new_canon = more_specific canon_d canon_d' in
        let non_canon =
          if exp_equal new_canon canon_d then canon_d' else canon_d
        in
        let backward' =
          (d, new_canon)
          :: List.filter (fun (x, _) -> x <> d && x <> d') env.pe_backward
        in
        let env' =
          { env with pe_forward = forward'; pe_backward = backward' }
        in
        propagate_backward env' non_canon new_canon
    | Some d, None ->
        (* b is new; a already has dummy d *)
        let canon = List.assoc d env.pe_backward in
        let new_canon = more_specific canon b in
        let non_canon = if exp_equal new_canon b then canon else b in
        let env' =
          {
            env with
            pe_forward = (b, d) :: env.pe_forward;
            pe_backward = (d, new_canon) :: List.remove_assoc d env.pe_backward;
          }
        in
        propagate_backward env' non_canon new_canon
    | None, Some d ->
        (* a is new; b already has dummy d *)
        let canon = List.assoc d env.pe_backward in
        let new_canon = more_specific a canon in
        let non_canon = if exp_equal new_canon a then canon else a in
        let env' =
          {
            env with
            pe_forward = (a, d) :: env.pe_forward;
            pe_backward = (d, new_canon) :: List.remove_assoc d env.pe_backward;
          }
        in
        propagate_backward env' non_canon new_canon
    | None, None ->
        let d = env.pe_next_id in
        let canon = more_specific a b in
        let non_canon = if exp_equal canon a then b else a in
        let env' =
          {
            pe_forward = (a, d) :: (b, d) :: env.pe_forward;
            pe_backward = (d, canon) :: env.pe_backward;
            pe_next_id = d + 1;
          }
        in
        propagate_backward env' non_canon canon

let add_pairs (env : prem_env) (pairs : (exp * exp) list) : prem_env =
  List.fold_left (fun env (a, b) -> add_pair env a b) env pairs

(* Resolve [e] to a concrete structure recorded in [env], but only when [e] is a
   bare variable (or iterated variable) whose class canonical is a constructor --
   never an opaque [CallE] (those stay behind their binding), and never a
   self-referential class (occurs check, so resolution terminates). Lets
   [add_pair_decompose] see through a variable: `let h :: t = lv` with `lv ≡ a::b`
   recorded by an earlier premise then decomposes to `h≡a, t≡b` instead of the
   opaque `lv ≡ h::t`. *)
let resolve_struct (env : prem_env) (e : exp) : exp =
  match e.it with
  | VarE _ | IterE ({ it = VarE _; _ }, _) -> (
      let r = resolve_prem_env env e in
      match r.it with
      | (CaseE _ | TupleE _ | OptE (Some _) | ConsE _ | ListE _ | StrE _)
        when (not (exp_equal r e))
             && Common.Domain.IdSet.is_empty
                  (Common.Domain.IdSet.inter (Free.free_exp e) (Free.free_exp r))
        ->
          r
      | _ -> e)
  | _ -> e

(* Like [add_pair], but when both sides are the same constructor, equate their
   components rather than the whole terms: `let (CASE x) = (CASE y)` records the
   useful `x ≡ y` (so a later substitution can replace [x] by [y] and drop the
   now-trivial binding) instead of the opaque `CASE x ≡ CASE y`. Each side is
   first resolved through the environment ([resolve_struct]) so a variable
   carrying a known structure decomposes too. Falls back to [add_pair] when the
   heads differ. *)
let rec add_pair_decompose (env : prem_env) (a : exp) (b : exp) : prem_env =
  let a = resolve_struct env a and b = resolve_struct env b in
  match (a.it, b.it) with
  | CaseE ne_a, CaseE ne_b
    when Mixfix.eq_mixop (Mixfix.to_mixop ne_a) (Mixfix.to_mixop ne_b) ->
      List.fold_left2 add_pair_decompose env (Mixfix.args ne_a)
        (Mixfix.args ne_b)
  | TupleE es_a, TupleE es_b when List.length es_a = List.length es_b ->
      List.fold_left2 add_pair_decompose env es_a es_b
  | OptE (Some x), OptE (Some y) -> add_pair_decompose env x y
  | ConsE (h_a, t_a), ConsE (h_b, t_b) ->
      add_pair_decompose (add_pair_decompose env h_a h_b) t_a t_b
  | _ -> add_pair env a b

(* Extract (non-canonical, canonical) pairs from env *)
let pairs_of_env (env : prem_env) : (exp * exp) list =
  List.filter_map
    (fun (e, d) ->
      match List.assoc_opt d env.pe_backward with
      | Some c when not (exp_equal e c) -> Some (e, c)
      | _ -> None)
    env.pe_forward

(* Build IterE(e, iterexp) with the correct IterT type annotation *)
let mk_iter_exp (e : exp) ((iter, vars) : iterexp) : exp =
  let open Common.Source in
  let typ = IterT { typ = e.note $ e.at; iter } in
  IterE (e, (iter, vars)) $$ e.at % typ

(* Each relation-premise output is an independent unknown, so its placeholder
   needs a fresh name: two premises of the same relation in one rule (e.g. two
   [Eval_expr] subgoals) must not collide on [_out_<rel>_<idx>] and thereby
   unify their distinct outputs. The actual number is irrelevant -- these
   placeholders never reach the rewritten spec -- only its uniqueness. *)
let fresh_out_id : unit -> int =
  let counter = ref 0 in
  fun () ->
    let n = !counter in
    incr counter;
    n

(* Placeholder for relation output positions whose value is unknown statically *)
let dummy_exp (e : exp) (name : string) : exp =
  let open Common.Source in
  VarE (name $ e.at) $$ e.at % e.note

(* Reconstruct the structural form a `matches` constraint pins down: given
   `e matches CaseP mixop`, build `CaseE(mixop, [fresh fields])` so the case
   structure can flow into binding positions and the `matches`/`<:`/cast
   scaffolding around it can then be dropped. Field variables are named
   deterministically from the subject (e.g. `lit` -> `lit_0`) so repeated
   environment builds -- across fixed-point iterations and redundancy probes --
   agree, which is what lets the loop converge. Returns None when the case is
   not a known variant constructor. *)
let reconstruct_case (spec : spec) (subj : exp) (e_typ : typ') (mixop : mixop) :
    exp option =
  let open Common.Source in
  let base = match subj.it with VarE id -> id.it | _ -> "x" in
  match e_typ with
  | VarT { synid = tid; _ } ->
      List.find_map
        (fun def ->
          match def.it with
          | TypD { synid = id2; deftyp = { it = VariantT cases; _ }; _ }
            when id2.it = tid.it ->
              List.find_map
                (fun (tc : typcase) ->
                  let nottyp = tc.notation in
                  if Mixfix.eq_mixop (Mixfix.to_mixop nottyp.it) mixop then
                    let fields =
                      List.mapi
                        (fun i (ft : typ) ->
                          let name = Printf.sprintf "%s_%d" base i in
                          VarE (name $ no_region) $$ no_region % ft.it)
                        (Mixfix.args nottyp.it)
                    in
                    Some (CaseE (Mixfix.fill mixop fields) $$ no_region % e_typ)
                  else None)
                cases
          | _ -> None)
        spec
  | _ -> None

(* Reconstruct the structure pinned down by a `matches` pattern, for subjects the
   [Struct] phase left without structure. Only the *payload-free* shapes are
   reconstructed: [CaseP] (whose fresh field variables flow into binding positions
   and become bound there) and the empty [OptP `None]/[ListP `Nil].

   The payload-carrying shapes ([OptP `Some], [ListP `Cons]/[`Fixed]) are
   deliberately *not* reconstructed: their fresh element variable is bound nowhere,
   so expanding the subject into `?(x)` / `x :: xs` would both leave [x] dangling
   and overwrite the very destructuring `let` (`let ?(v) = e`) that already pins the
   shape down. Returning None keeps that `let` intact -- it then supplies the
   structure and [pattern_forced] drops the `matches` (an unaccompanied `matches`,
   a genuine guard, simply stays). *)
let reconstruct_pattern (spec : spec) (subj : exp) (pattern : pattern) :
    exp option =
  let open Common.Source in
  let at_note e = e $$ no_region % subj.note in
  match pattern with
  | CaseP mixop -> reconstruct_case spec subj subj.note mixop
  | OptP `None -> Some (at_note (OptE None))
  | ListP `Nil -> Some (at_note (ListE []))
  | OptP `Some | ListP `Cons | ListP (`Fixed _) -> None

(* Field list of a struct type [VarT id] as declared in the spec. *)
let struct_fields (spec : spec) (typ : typ') : (atom * typ) list option =
  match typ with
  | VarT { synid = tid; _ } ->
      List.find_map
        (fun (def : def) ->
          match def.it with
          | TypD { synid = id2; deftyp = { it = StructT fields; _ }; _ }
            when id2.it = tid.it ->
              Some fields
          | _ -> None)
        spec
  | _ -> None

(* Peel a field-access spine `r.a1.a2...aN` into its root and the atoms from the
   root outward. None when the base is not a bare variable. *)
let rec dot_spine (e : exp) : (exp * atom list) option =
  match e.it with
  | VarE _ -> Some (e, [])
  | DotE (base, a) ->
      Option.map (fun (root, atoms) -> (root, atoms @ [ a ])) (dot_spine base)
  | _ -> None

(* A concrete shape carrying a sub-value (destructuring it binds something), as
   opposed to a bare variable or a payload-free constructor. *)
let is_payload (e : exp) : bool =
  match e.it with
  | ConsE _ | CaseE _ | OptE (Some _) | ListE (_ :: _) -> true
  | _ -> false

(* Whether [e]'s class already carries a concrete payload shape (e.g. from a
   destructuring `let`). When it does, [Recon] need not reconstruct a fresh-field
   case for [e]: the existing member supplies the structure, and reusing it keeps
   the real variable names rather than minting fresh `_N` ones that then linger as
   an alias binding. *)
let class_has_payload (env : prem_env) (e : exp) : bool =
  match lookup_dummy env e with
  | None -> false
  | Some d ->
      List.exists (fun (e', d') -> d' = d && is_payload e') env.pe_forward

(* Every expression in [e]'s equivalence class (excluding [e] itself when it has
   no recorded dummy). Lets a caller see, e.g., that a value bound elsewhere as a
   `::` destructure shares a class with the iterated variable being `matches`-ed,
   so the constraint is already pinned down by a companion premise. *)
let class_members (env : prem_env) (e : exp) : exp list =
  match lookup_dummy env e with
  | None -> []
  | Some d ->
      List.filter_map
        (fun (e', d') -> if d' = d then Some e' else None)
        env.pe_forward

(* Reconstruct the root of a field-access chain as a struct literal: the field on
   the chain takes [leaf] (the concrete shape its equivalence class already
   carries), every other field becomes a fresh variable named from the root and
   field atom. Mirrors [reconstruct_case] but for [StructT], recursing for a
   nested chain (`r.local.frames` -> `{ ..local = { ..frames = leaf } }`). The
   fresh field variables land in a binder position (the head pattern), so nothing
   dangles, and the root drops out of the result entirely. None when any level is
   not a known struct type or lacks the chain field. *)
let reconstruct_struct (spec : spec) (root : exp) (atoms : atom list)
    (leaf : exp) : exp option =
  let open Common.Source in
  let base = match root.it with VarE id -> id.it | _ -> "x" in
  let rec build (typ : typ') : atom list -> exp option = function
    | [] -> Some leaf
    | a :: rest -> (
        match struct_fields spec typ with
        | Some fields when List.exists (fun (fa, _) -> fa.it = a.it) fields ->
            let field ((fa : atom), (ft : typ)) =
              if fa.it = a.it then
                Option.map (fun v -> (fa, v)) (build ft.it rest)
              else
                let name =
                  Printf.sprintf "%s_%s" base
                    (String.lowercase_ascii (Xl.Atom.to_string fa.it))
                in
                Some (fa, VarE (name $ no_region) $$ no_region % ft.it)
            in
            Option.map
              (fun efields -> StrE efields $$ no_region % typ)
              (List.fold_right
                 (fun f acc ->
                   match (field f, acc) with
                   | Some fe, Some fes -> Some (fe :: fes)
                   | _ -> None)
                 fields (Some []))
        | _ -> None)
  in
  build root.note atoms

(* For each head-bound clause input whose field-access chain carries a concrete
   shape, emit [root -> reconstructed struct] so the substitution step pushes the
   struct into the head (a binder position) and the conclusion, after which the
   field-access `let`s and the `matches` become redundant. One struct per root,
   only when the constrained chain is unambiguous and the root does not reappear
   inside the struct (occurs check). *)
let hoist_pairs (spec : spec) (env : prem_env) (roots : Free.t) :
    (exp * exp) list =
  let same_atoms a b =
    List.length a = List.length b
    && List.for_all2 (fun (x : atom) (y : atom) -> x.it = y.it) a b
  in
  let cands =
    List.filter_map
      (fun (m, c) ->
        match dot_spine c with
        | Some (({ it = VarE rid; _ } as root), (_ :: _ as atoms))
          when Common.Domain.IdSet.mem rid roots && is_payload m ->
            Some (rid.it, root, atoms, m)
        | _ -> None)
      (pairs_of_env env)
  in
  List.sort_uniq compare (List.map (fun (n, _, _, _) -> n) cands)
  |> List.filter_map (fun name ->
         match List.filter (fun (n, _, _, _) -> n = name) cands with
         | (_, root, atoms, leaf) :: _ as rcs
           when List.for_all (fun (_, _, a, _) -> same_atoms a atoms) rcs -> (
             match reconstruct_struct spec root atoms leaf with
             | Some s
               when match root.it with
                    | VarE id ->
                        not (Common.Domain.IdSet.mem id (Free.free_exp s))
                    | _ -> false ->
                 Some (root, s)
             | _ -> None)
         | _ -> None)

(* The environment is built in two phases. [Struct] gathers definite structure
   from `let`s (cast-stripped), relation outputs, and equalities; [Recon] then
   reconstructs case structure from `matches` constraints -- but only for
   subjects [Struct] left without structure, so a value already destructured by
   a `let` (e.g. `let e_l + e_r = exp`) is not overwritten with a conflicting
   fresh-field case. *)
type phase = Struct | Recon

let is_structural (e : exp) : bool =
  match e.it with VarE _ -> false | _ -> true

let env_of_if_exp (spec : spec) (phase : phase) (env : prem_env) (exp : exp) :
    prem_env =
  match (phase, exp.it) with
  | Struct, CmpE (`EqOp, _, a, b) ->
      let a = strip_casts spec a and b = strip_casts spec b in
      (* Fold an equality into the environment only when it pins a value down:
         one side is concrete structure (`n = $(..)`, `literal = true`), or one
         side is a relation output placeholder (`type' = type`). A plain var=var
         guard between two binders (`if K_h = K`) is left untouched -- folding it
         would rewrite the variable a pattern binds (and corrupt stored
         structures via [propagate_backward]). *)
      let foldable =
        (is_structural a || is_structural b || in_hidden_class env a
       || in_hidden_class env b)
        (* A gensym-reaching call stays opaque: folding `if x = $f(..)` would
           make the call canonical and substitution would re-mint the same
           instance at every use of [x] (see [gensym_ids]). *)
        && (not (mentions_effectful_call spec a))
        && not (mentions_effectful_call spec b)
      in
      (* Decompose like a [LetPr]: an equality between two identical
         constructors (`(NUM x) = (NUM y)`) records the useful `x ≡ y` instead
         of the opaque `(NUM x) ≡ (NUM y)`. Sound -- constructors are injective,
         and the var=var leaf pairs this yields are still filtered out of binder
         positions by [structural_pairs], so no binder gets conflated. *)
      if foldable then add_pair_decompose env a b else env
  | Recon, MatchE (e, pattern) -> (
      (* Skip subjects already given structure in the [Struct] phase -- either as
         the canonical form, or anywhere in the class (a destructuring `let`
         member), so a fresh-field case is not minted alongside a real one. *)
      let resolved_structural =
        match (resolve_prem_env env e).it with
        | CaseE _ | OptE _ | ListE _ | ConsE _ -> true
        | _ -> false
      in
      if resolved_structural || class_has_payload env e then env
      else
        match reconstruct_pattern spec e pattern with
        | Some s -> add_pair env e s
        | None -> env)
  | _ -> env

let env_of_rule_prem (env : prem_env) (spec : spec) (id : id) (notexp : notexp)
    : prem_env =
  match find_rel_in_spec spec id.it with
  | None -> env
  | Some (nottyp, inputs) ->
      let arg_types = Mixfix.args nottyp.it in
      let args = Mixfix.args notexp in
      let n = min (List.length arg_types) (List.length args) in
      List.init n Fun.id
      |> List.filter (fun i -> not (List.mem i inputs))
      |> List.fold_left
           (fun env i ->
             let out_exp = List.nth args i in
             let dummy_name =
               Printf.sprintf "%s%s_%d_%d" hidden_out_prefix id.it i
                 (fresh_out_id ())
             in
             add_pair env out_exp (dummy_exp out_exp dummy_name))
           env

let rec env_of_prem (phase : phase) (spec : spec) (env : prem_env) (prem : prem)
    : prem_env =
  match prem.it with
  | LetPr (a, b) -> (
      if
        phase <> Struct
        (* A binding over a gensym-reaching call stays opaque, like the
           equality case in [env_of_if_exp]: even though the substitution
           filters keep `x -> call` pairs out, a call canonical would still
           leak through [propagate_backward] into other classes' stored
           structures and get duplicated there. *)
        || mentions_effectful_call spec a
        || mentions_effectful_call spec b
      then env
      else
        match b.it with
        | DownCastE (t, inner) when not (is_num_cast spec t.it inner.note) ->
            (* `let a = e as t` narrows: the downcast target [a] (narrower type)
               is the better canonical, so orient the pair so [a] wins. This lets
               an untagged subtype injection `let x = expr as id` rewrite the
               wider `expr` to `x`, dropping the `<:` test. A nat/int narrowing
               (`let n = int as nat`) is excluded -- it is representation-changing,
               so it falls through to the default branch, which keeps the cast
               (`n ≡ (int as nat)`) and leaves the int source as its own
               canonical (the head pattern stays int-typed). *)
            add_pair env (strip_casts spec inner) a
        | _ -> add_pair_decompose env (strip_casts spec a) (strip_casts spec b))
  | IfPr { cond; _ } -> env_of_if_exp spec phase env cond
  | IterPr (inner_prem, (List, vars)) ->
      (* Element-level bindings from inner premise *)
      let inner_env = env_of_prem phase spec empty_prem_env inner_prem in
      let elem_pairs = pairs_of_env inner_env in
      (* List-level lifted bindings: wrap both sides in IterE *)
      let list_pairs =
        List.map
          (fun (a, b) ->
            ( mk_iter_exp a (List, vars),
              mk_iter_exp b (List, relift_vars a b vars) ))
          elem_pairs
      in
      add_pairs (add_pairs env elem_pairs) list_pairs
  | IterPr (inner_prem, (Opt, vars)) ->
      (* Optional: content may not exist, so skip element-level bindings.
         Only add opt-level lifted bindings. *)
      let inner_env = env_of_prem phase spec empty_prem_env inner_prem in
      let elem_pairs = pairs_of_env inner_env in
      let opt_pairs =
        List.map
          (fun (a, b) ->
            ( mk_iter_exp a (Opt, vars),
              mk_iter_exp b (Opt, relift_vars a b vars) ))
          elem_pairs
      in
      add_pairs env opt_pairs
  | RelPr { relid; notexp } ->
      if phase = Struct then env_of_rule_prem env spec relid notexp else env
  | RelAssertPr _ | ElsePr | DebugPr _ -> env

let env_of_prems (spec : spec) (prems : prem list) : prem_env =
  let env_struct =
    List.fold_left
      (fun env prem -> env_of_prem Struct spec env prem)
      empty_prem_env prems
  in
  List.fold_left
    (fun env prem -> env_of_prem Recon spec env prem)
    env_struct prems
