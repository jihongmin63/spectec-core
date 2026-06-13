open Lang.Il

(* -------------------------------------------------------------------------- *)
(* Simplification                                                              *)
(* Drive two transformations off the premise environment ({!Prem_env}), in     *)
(* every relation rule and function clause:                                    *)
(*   1. expand variables into their concrete canonical structure, and          *)
(*   2. drop premises the environment renders redundant.                       *)
(* -------------------------------------------------------------------------- *)

module IdSet = Common.Domain.IdSet

let exp_equal e1 e2 = Eq.eq_exp e1 e2

(* Whether [needle] occurs (by IL equality) anywhere inside [hay]. *)
let rec occurs_in (needle : exp) (hay : exp) : bool =
  exp_equal needle hay
  || List.exists (occurs_in needle) (Exp_map.subexps hay.it)

(* A concrete data-structure literal: a constructor / struct / list / cons / opt
   / tuple, as opposed to a bare variable or an opaque call. These are the shapes
   that can serve as a destructuring pattern AND be inlined component-wise. (See
   also [Prem_env.is_payload], a *narrower* notion -- only shapes that actually
   carry a sub-value -- and [Prem_env.is_structural], anything but a variable.) *)
let is_struct_lit (e : exp) : bool =
  match e.it with
  | CaseE _ | StrE _ | ListE _ | ConsE _ | OptE _ | TupleE _ -> true
  | _ -> false

(* A field path as a list of atoms, or None if it contains an index/slice step. *)
let rec dot_atoms (p : path) : atom list option =
  match p.it with
  | RootP -> Some []
  | DotP (p', a) -> Option.map (fun atoms -> atoms @ [ a ]) (dot_atoms p')
  | IdxP _ | SliceP _ -> None

(* Apply a field-path update to a struct literal: [set_path {..} [a1; a2] v] sets
   the [.a1.a2] field to [v]. None when a level is not a struct carrying [a]. *)
let rec set_path (s : exp) (atoms : atom list) (v : exp) : exp option =
  match atoms with
  | [] -> Some v
  | a :: rest -> (
      match s.it with
      | StrE fields
        when List.exists (fun ((fa : atom), _) -> fa.it = a.it) fields ->
          let rec go = function
            | [] -> Some []
            | ((fa : atom), fv) :: more -> (
                let fv' =
                  if fa.it = a.it then set_path fv rest v else Some fv
                in
                match (fv', go more) with
                | Some fv', Some others -> Some ((fa, fv') :: others)
                | _ -> None)
          in
          Option.map (fun fields' -> { s with it = StrE fields' }) (go fields)
      | _ -> None)

(* Deep IL normalization, applied once simplification has reached a fixed point:
   - drop every Up/DownCast wrapper, so no `as` survives in the output (e.g. an
     output `=> n as value` becomes `=> n`), except a nat/int cast, which is
     representation-changing and kept for [To_ctrs] (see [Prem_env.is_num_cast]); [SubE]
     (`<:`) is left in place, it only appears as an `if` premise and is dropped
     by redundancy removal;
   - collapse field access / update over a struct literal that head-pattern
     reconstruction ([Prem_env.hoist_pairs]) produced: `{ ..f = x.. }.f` becomes
     `x`, and `{ ..f = _.. }[f = v]` becomes `{ ..f = v.. }`. This turns the
     field-access `let`s left behind by reconstructing a struct input in the head
     into redundant restatements, which the redundancy pass then drops. *)
let mk_upcast (t : typ) (e : exp) : exp' = UpCastE (t, e)
let mk_downcast (t : typ) (e : exp) : exp' = DownCastE (t, e)

let rec normalize_deep (spec : spec) (e : exp) : exp =
  (* A kept numeric cast is concretized -- its target type and the inner
     expression's type note are rewritten to the resolved base [NumT] -- so
     [To_ctrs]'s literal-`NumT` cast guards fire even when the cast went through a
     named alias. *)
  let keep_num_cast mk (t : typ) (e' : exp) =
    let tgt = Option.get (Prem_env.resolve_num_typ spec t.it) in
    let src = Option.get (Prem_env.resolve_num_typ spec e'.note) in
    let e' = { (normalize_deep spec e') with note = NumT src } in
    { e with it = mk { t with it = NumT tgt } e' }
  in
  match e.it with
  (* A tuple numeric cast over a tuple literal distributes to its components
     (mirroring [interp.upcast]'s [TupleT] recursion); each then reduces via the
     scalar cases below. *)
  | UpCastE ({ it = TupleT ts; _ }, ({ it = TupleE es; _ } as inner))
  | DownCastE ({ it = TupleT ts; _ }, ({ it = TupleE es; _ } as inner))
    when List.length ts = List.length es ->
      let mk = match e.it with UpCastE _ -> mk_upcast | _ -> mk_downcast in
      let es' =
        List.map2
          (fun (t : typ) (ei : exp) -> { ei with it = mk t ei; note = t.it })
          ts es
      in
      normalize_deep spec { inner with it = TupleE es' }
  | UpCastE (t, e') when Prem_env.is_num_cast spec t.it e'.note ->
      keep_num_cast mk_upcast t e'
  | DownCastE (t, e') when Prem_env.is_num_cast spec t.it e'.note ->
      keep_num_cast mk_downcast t e'
  | UpCastE (_, e') | DownCastE (_, e') -> normalize_deep spec e'
  | _ -> (
      (* Recurse into every sub-expression; nested casts are stripped when the
         recursion reaches them via the match above, so dropping them here too
         would be redundant. *)
      let e = { e with it = Exp_map.map_subexps (normalize_deep spec) e.it } in
      match e.it with
      | DotE ({ it = StrE fields; _ }, a) -> (
          match List.find_opt (fun ((fa : atom), _) -> fa.it = a.it) fields with
          | Some (_, v) -> v
          | None -> e)
      | UpdE (({ it = StrE _; _ } as base), p, v) -> (
          match dot_atoms p with
          | Some atoms -> Option.value ~default:e (set_path base atoms v)
          | None -> e)
      | _ -> e)

let rec normalize_deep_prem (spec : spec) (prem : prem) : prem =
  let nd = normalize_deep spec in
  let it =
    match prem.it with
    | RelPr { relid; notexp } -> RelPr { relid; notexp = Mixfix.map nd notexp }
    | IfPr { cond; role } -> IfPr { cond = nd cond; role }
    | RelAssertPr { call = { relid; notexp }; expect } ->
        RelAssertPr { call = { relid; notexp = Mixfix.map nd notexp }; expect }
    | LetPr (l, r) -> LetPr (nd l, nd r)
    | IterPr (inner, ie) -> IterPr (normalize_deep_prem spec inner, ie)
    | DebugPr e -> DebugPr (nd e)
    | ElsePr -> ElsePr
  in
  { prem with it }

(* Variable -> canonical substitution drawn from the environment. A pair is kept
   only when:
   - the source is a plain variable, or an iterated variable `e?{..}`/`e*{..}`
     (an option/list value in variable-ish form, e.g. a clause argument): we
     expand value uses, not arbitrary patterns;
   - neither side is a synthetic output placeholder (those only steer narrowing,
     so a relation output with no concrete form keeps its own name); and
   - the source does not occur in the target. The last is an occurs check: a
     self-referential class such as [x = SUCC x] (representable, though
     contradictory) would otherwise make [subst_with] grow [x] without bound and
     the fixed-point loop diverge. *)
(* A reconstruction that fills a source wildcard `_` introduces a fresh discard
   variable the elaborator names with a leading `_` (e.g. `_expressionIR`,
   `_typeIR` from `_ `# `( _ ctk ) = e`). Substituting `x -> _disc # ( .. )` would
   propagate that originally-discarded variable into value positions (a
   conclusion, a relation input), where it becomes spuriously live while its only
   binder -- the very destructuring the substitution trivialises -- is pruned,
   stranding it ("variable used before it is bound"). So a canonical structure
   that carries such a discard variable must not replace the bound variable. *)
let introduces_discard (to_e : exp) : bool =
  IdSet.exists
    (fun (i : id) ->
      String.starts_with ~prefix:"_" i.it && not (Prem_env.is_hidden_out_id i))
    (Free.free_exp to_e)

(* Variables a `let` binds through a position substitution cannot push structure
   into, so canonicalising them to a destructured shape strands the pieces:
   - the leaves of a `let` whose right-hand side is an opaque call
     (`let x = $f(..)`, `let ?(x) = $f(..)`, `let xs* = $rev(..)`): the value has
     no structural form of its own; and
   - the leaves of any destructuring `let PAT = e` (PAT a constructor / cons /
     option / tuple): a `let`'s LHS is never rewritten by substitution, so a leaf
     can only be bound by the match -- unlike a relation OUTPUT position, which
     [subst_rule_prem] DOES rewrite, folding structure into it to bind components.
   Canonicalising such a variable [x] to a shape `C(y)` (drawn from a companion
   `let C(y) = x`) and substituting it turns that companion into the circular
   self-match `let C(y) = C(y)` (or a partial one on a cons tail), stranding [y]
   ("variable used before it is bound"). Keep [x]; let the destructure extract [y].
   Guarded in both branches of [subst_of_env]; the `is_structural to_e` gate there
   keeps harmless var renames flowing.

   A related hazard lands in [subst_prem]'s LetPr case (see [folds_descendant_of]),
   not here: folding a reconstruction into a `let` RHS whose own pattern binds the
   reconstruction's component. *)
let binds_by_match (l : exp) (r : exp) : bool =
  (match r.it with CallE _ -> true | _ -> false) || is_struct_lit l

(* The match-binding `let`s among [prems], seen through iterated premises: the
   hazard above concerns the element-level binding wherever it sits, so an
   iterated `(let y :: ys' = xs)*` counts the same as a top-level one. The one
   collector both guards below derive from, so they always cover the same
   premises. *)
let match_lets (prems : prem list) : (exp * exp) list =
  let rec of_prem (prem : prem) =
    match prem.it with
    | LetPr (l, r) when binds_by_match l r -> [ (l, r) ]
    | IterPr (inner, _) -> of_prem inner
    | _ -> []
  in
  List.concat_map of_prem prems

let match_bound_vars (prems : prem list) : IdSet.t =
  List.fold_left
    (fun s (l, _) -> IdSet.union s (Free.free_exp l))
    IdSet.empty (match_lets prems)

(* Each destructure/opaque-call leaf paired with the variables of the right-hand
   side it is bound from. Lets the substitution guard tell a self-reconstruction
   (leaf bound from [x] itself) apart from a cross-binding one (leaf bound from
   another var, the cycle above). *)
let destructure_source_map (prems : prem list) : (id * IdSet.t) list =
  List.concat_map
    (fun (l, r) ->
      let src = Free.free_exp r in
      IdSet.elements (Free.free_exp l) |> List.map (fun id -> (id, src)))
    (match_lets prems)

(* Whether folding [to_e] into the RHS of an alias `let v = ..` (so [l] is the
   plain variable [v]) reintroduces a destructure leaf bound by destructuring [v]
   itself. That makes `let v = C(y)` with [y] extracted from [v] afterwards -- a
   self-referential match that strands [y] (`(let infer' = infer)*` companioned by
   `(let KNOWN AS typeIR = infer')*` becoming `(let infer' = KNOWN AS typeIR)*`,
   with [typeIR] coming from [infer']). Restricted to a variable [l]: a destructure
   `let C(y) = e` folds structure into its SUBJECT [e] to bind the leaves, which is
   the intended mechanism, not a cycle; likewise a relation OUTPUT is computed, so
   it binds rather than matches. *)
let folds_descendant_of (srcs : (id * IdSet.t) list) (l : exp) (to_e : exp) :
    bool =
  (not (Prem_env.is_structural l))
  &&
  let lvars = Free.free_exp l in
  IdSet.exists
    (fun y ->
      List.exists
        (fun ((leaf : id), src) ->
          leaf.it = y.it && not (IdSet.is_empty (IdSet.inter src lvars)))
        srcs)
    (Free.free_exp to_e)

let subst_of_env (match_bound : IdSet.t) (env : Prem_env.prem_env) :
    (exp * exp) list =
  Prem_env.pairs_of_env env
  |> List.filter (fun ((from_e : exp), (to_e : exp)) ->
         match from_e.it with
         | VarE id ->
             (not (Prem_env.is_hidden_out_var from_e))
             && (not (Prem_env.is_hidden_out_var to_e))
             && (not (introduces_discard to_e))
             && (not (IdSet.mem id match_bound && Prem_env.is_structural to_e))
             && not (IdSet.mem id (Free.free_exp to_e))
         | IterE ({ it = VarE _; _ }, _) ->
             (* Same match-bound guard as the VarE case: an iterated link
                `let xs* = $rev(..)` must not be canonicalised to a destructured
                shape `y :: ys*`, or its companion `let y :: ys* = xs*` self-matches
                on the tail (`y :: ys* = z :: ys*`) and strands ys*. *)
             (not (Prem_env.is_hidden_out_var to_e))
             && (not
                   (Prem_env.is_structural to_e
                   && not
                        (IdSet.is_empty
                           (IdSet.inter (Free.free_exp from_e) match_bound))))
             && IdSet.is_empty
                  (IdSet.inter (Free.free_exp from_e) (Free.free_exp to_e))
         | _ -> false)

let subst_with (pairs : (exp * exp) list) (e : exp) : exp =
  List.fold_left
    (fun e (from_e, to_e) -> Prem_env.subst_exp from_e to_e e)
    e pairs

(* Expand structure into value positions only. Constraint premises (IfPr,
   RulePr, holds) keep this structure too. [IfPr] is the one exception: it is left
   verbatim so the variables a `matches`/equality constraint pins down stay live,
   which keeps the redundant-binding removal below from dropping a binding whose
   constraint would be lost.

   Relation premises ARE rewritten. Substituting structure into an output position
   turns it into a binding pattern -- `R: ins => literal` becomes
   `R: ins => (NUM n_l)` -- which the removal step then recognizes as binding
   `n_l`, letting the now-redundant `matches`/destructuring `let` go. *)
let subst_notexp (pairs : (exp * exp) list) (ne : notexp) : notexp =
  Mixfix.map (subst_with pairs) ne

(* Rewrite a relation premise's arguments. Inputs are plain uses, so any
   substitution applies. Outputs are binding positions: substituting one replaces
   the variable it would have bound, so we only do it for a STRUCTURAL target
   (e.g. `literal -> (NUM n_l)`), which the removal step can recognize and whose
   `matches`/`let` users it can drop, OR for a variable already bound by the rule
   head (in [safe]) -- renaming `type' -> type` there cannot strand `type', and
   makes the `if type' = type` it came from redundant. Renaming an output to a
   variable bound nowhere else could strand it, so those pairs are withheld. *)
let structural_pairs (pairs : (exp * exp) list) : (exp * exp) list =
  List.filter (fun (_, (to_e : exp)) -> Prem_env.is_structural to_e) pairs

let subst_rule_prem (spec : spec) (safe : IdSet.t) (ipairs : (exp * exp) list)
    (pairs : (exp * exp) list) (id : id) (ne : notexp) : notexp =
  match Prem_env.find_rel_in_spec spec id.it with
  | None -> subst_notexp (structural_pairs pairs @ ipairs) ne
  | Some (_, inputs) ->
      (* Inputs are binder positions, like the head: fold in STRUCTURE and subtype
         injections, never a var=var equality rename (which would conflate two
         binders). Outputs additionally accept a rename onto a head-bound variable
         (`type' -> type`), which cannot strand and makes the `if type' = type` it
         came from redundant. *)
      let binder_pairs = structural_pairs pairs @ ipairs in
      let out_pairs =
        binder_pairs
        @ List.filter
            (fun (_, (to_e : exp)) ->
              match to_e.it with VarE id -> IdSet.mem id safe | _ -> false)
            pairs
      in
      Mixfix.args ne
      |> List.mapi (fun i e ->
             subst_with
               (if List.mem i inputs then binder_pairs else out_pairs)
               e)
      |> Mixfix.fill (Mixfix.to_mixop ne)

let rec subst_prem (spec : spec) (safe : IdSet.t) (known : IdSet.t)
    (srcs : (id * IdSet.t) list) (ipairs : (exp * exp) list)
    (pairs : (exp * exp) list) (prem : prem) : prem =
  (* Everything but relation-output positions only ever takes structural folds and
     subtype injections, so a var=var equality like `if K_h = K` stays a guard
     instead of rewriting the `K_h` a pattern binds. *)
  let bpairs = structural_pairs pairs @ ipairs in
  let it =
    match prem.it with
    | LetPr (l, r) ->
        (* Never rewrite the right-hand side toward the pattern [l] it is matched
           against to bind: a pair whose target already occurs inside [l] would
           collapse the binding into a self-match that strands its variables. This
           covers the whole pattern (`let (a, b) = p` becoming `let (a, b) =
           (a, b)`) and a component of it (a pair `c -> b` folding the right of
           `let C(b) = C(c)` into the self-match `let C(b) = C(b)`).
           Also never fold in a reconstruction carrying a variable bound NOWHERE in
           the block ([known] = the rule's own variables): the env mints such a
           fresh variable when it reconstructs a `matches` (`x ≡ C(x_0)`), and a
           `let`'s LHS -- unlike a relation OUTPUT, the one position [subst_rule_prem]
           folds these into -- cannot bind it, so it would strand (an iterated alias
           `(let x' = x)*` becoming `(let x' = C(x_0))*`). *)
        let p =
          List.filter
            (fun (_, to_e) ->
              (not (occurs_in to_e l))
              && IdSet.subset (Free.free_exp to_e) known
              && not (folds_descendant_of srcs l to_e))
            bpairs
        in
        LetPr (l, subst_with p r)
    | RelPr { relid; notexp } ->
        RelPr
          {
            relid;
            notexp = subst_rule_prem spec safe ipairs pairs relid notexp;
          }
    | RelAssertPr { call = { relid; notexp }; expect } ->
        RelAssertPr
          { call = { relid; notexp = subst_notexp bpairs notexp }; expect }
    | DebugPr e -> DebugPr (subst_with bpairs e)
    | IterPr (inner, iterexp) ->
        IterPr (subst_prem spec safe known srcs ipairs pairs inner, iterexp)
    | IfPr ({ cond = { it = CmpE (`EqOp, _, _, _); _ }; _ } as r) -> IfPr r
    | IfPr { cond; role } -> IfPr { cond = subst_with bpairs cond; role }
    | ElsePr -> ElsePr
  in
  { prem with it }

(* Apply [pairs] inside `if` guards too. [subst_prem] leaves [IfPr] verbatim to
   keep var=var equality guards from rewriting a binder, but a whole-value
   reconstruction pair (a struct-input hoist, `TC -> { .. }`) must reach guards
   like `if TC.mode = pri`, otherwise the root is left unbound once the head no
   longer binds it. Restricted to the hoist pairs at the call site for that
   reason; a no-op on [] (when not hoisting). *)
let rec subst_guards_with (pairs : (exp * exp) list) (prem : prem) : prem =
  let it =
    match prem.it with
    | IfPr { cond; role } -> IfPr { cond = subst_with pairs cond; role }
    | IterPr (inner, ie) -> IterPr (subst_guards_with pairs inner, ie)
    | it -> it
  in
  { prem with it }

(* Variables that must remain bound: the conclusion plus the premises retained
   alongside the one under test. *)
let free_of (outs : exp list) (prems : prem list) : IdSet.t =
  let outs_free =
    List.fold_left (fun s e -> IdSet.union s (Free.free_exp e)) IdSet.empty outs
  in
  List.fold_left (fun s p -> IdSet.union s (Free.free_prem p)) outs_free prems

(* Does the canonical form of [e] in [env] already satisfy [pattern]? Used to
   drop a `matches` premise whose shape the surrounding premises already pin down
   (e.g. `if e matches `% + %`` once `let e_l + e_r = e` is present). *)
let single_pattern_match (pattern : pattern) (e : exp) : bool =
  match (pattern, e.it) with
  | CaseP mixop, CaseE notexp -> Mixfix.eq_mixop (Mixfix.to_mixop notexp) mixop
  | OptP `Some, OptE (Some _) | OptP `None, OptE None -> true
  | ListP `Nil, ListE [] -> true
  | ListP `Cons, ListE (_ :: _) | ListP `Cons, ConsE _ -> true
  | ListP (`Fixed n), ListE es -> List.length es = n
  | _ -> false

(* [e]'s shape satisfies [pattern] when the canonical form OR *any member* of its
   equivalence class does: a companion destructure (`let h :: t = E` alongside
   `let e* = E`) puts a matching [ConsE] in [e]'s class even when the canonical
   stays the opaque field/call, so the `matches` it would justify is dropped. *)
let pattern_forced (env : Prem_env.prem_env) (e : exp) (pattern : pattern) :
    bool =
  single_pattern_match pattern (Prem_env.resolve_prem_env env e)
  || List.exists (single_pattern_match pattern) (Prem_env.class_members env e)

(* Variables a premise binds through a pattern position: relation-premise output
   positions and let left-hand sides. A destructuring let is subsumed once these
   already bind its variables (e.g. a rewritten output `=> (NUM n_l)` binds
   `n_l`, so `let (NUM n_l) = literal` is redundant). *)
let rec binders_of_prem (spec : spec) (prem : prem) : IdSet.t =
  match prem.it with
  | RelPr { relid; notexp } -> (
      match Prem_env.find_rel_in_spec spec relid.it with
      | Some (_, inputs) ->
          Mixfix.args notexp
          |> List.mapi (fun i e -> (i, e))
          |> List.fold_left
               (fun s (i, e) ->
                 if List.mem i inputs then s
                 else IdSet.union s (Free.free_exp e))
               IdSet.empty
      | None -> IdSet.empty)
  | LetPr (l, _) -> Free.free_exp l
  | IterPr (inner, _) -> binders_of_prem spec inner
  | _ -> IdSet.empty

let binders_of_prems (spec : spec) (prems : prem list) : IdSet.t =
  List.fold_left
    (fun s p -> IdSet.union s (binders_of_prem spec p))
    IdSet.empty prems

(* Variable renames coming from a subtype injection `let x = e as t` (e, x both
   variables). Unlike a var=var equality guard (`if K_h = K`, which must stay a
   guard), an injection means "use the narrower view [x] for [e]", so it is safe
   to apply to binder positions -- it is what rewrites an `id` rule's head
   `expr` to `x`. (When [e] also has a concrete reconstruction, structural pairs
   are applied first and win.)

   But the source [e] must be a HEAD-bound variable, not one another premise
   binds. A downcast of a relation/function output (`R: .. => declarationIR` then
   `let constantDeclarationIR = declarationIR as constantDeclarationIR`) would
   otherwise rename the output binder to the narrower view, relabelling what [R]
   produces and stranding the wider [declarationIR]. When [e] is premise-bound we
   withhold the rename, so the `let` stays as a plain `constantDeclarationIR :=
   declarationIR` binding instead. *)
let injection_pairs (spec : spec) (prems : prem list) : (exp * exp) list =
  let prem_bound = binders_of_prems spec prems in
  List.filter_map
    (fun (p : prem) ->
      match p.it with
      | LetPr (({ it = VarE _; _ } as v), { it = DownCastE (_, e); _ })
        when (match e.it with VarE _ -> true | _ -> false)
             && not (IdSet.subset (Free.free_exp e) prem_bound) ->
          Some (e, v)
      | _ -> None)
    prems

(* A premise is redundant when the others (together with the head pattern) already
   entail it. [bound_head] is the set of variables the rule/clause head binds.
   - a single-variable let whose bound variable is now dead;
   - a destructuring let whose pattern variables are all bound elsewhere (the head
     or another premise's pattern) and whose right-hand side is otherwise unused:
     once `R: ins => (NUM n_l)` binds `n_l`, `let (NUM n_l) = literal` only
     re-states it;
   - an equality or `matches` test the surrounding premises already force. *)
let rec prem_redundant (spec : spec) (outs : exp list) (bound_head : IdSet.t)
    (others : prem list) (prem : prem) : bool =
  (* A subject (a variable, or an iterated variable `pair*{..}`) whose variables,
     after substitution, no longer appear anywhere among the outputs or the other
     premises: its value was inlined away, so any constraint that only mentions it
     is pure residue and can go. *)
  let dead_var (e : exp) : bool =
    let fv = Free.free_exp e in
    (not (IdSet.is_empty fv))
    && IdSet.is_empty (IdSet.inter fv (free_of outs others))
  in
  (* The redundancy env over [others] is shared by every branch that needs it
     (forced at most once); the cheap variable/identity branches never touch it. *)
  let env = lazy (Prem_env.env_of_prems spec others) in
  match prem.it with
  | LetPr (l, r)
    when exp_equal l r
         &&
         (* `let X = X` asserts nothing, but its pattern still *binds* [X]'s
               variables. Dropping it is safe only when those are bound elsewhere
               (head or another premise) or unused -- otherwise a use of them is
               stranded (e.g. an inlined `let [t] = [t]` whose [t] the conclusion
               still mentions). *)
         let lv = Free.free_exp l in
         IdSet.subset lv (IdSet.union bound_head (binders_of_prems spec others))
         || IdSet.is_empty (IdSet.inter lv (free_of outs others)) ->
      true
  | LetPr ({ it = VarE v; _ }, { it = DownCastE _ | UpCastE _; _ })
    when IdSet.mem v bound_head ->
      (* Injection scaffolding `let x = e as t` whose target [x] the head already
         binds: the env carries `e ≡ x`, so other uses of [e] are already [x] and
         this binding only re-states the head. *)
      true
  | LetPr ({ it = VarE id; _ }, _) -> not (IdSet.mem id (free_of outs others))
  | LetPr ({ it = IterE ({ it = VarE id; _ }, _); _ }, _) ->
      (* A dead iterated-variable binding `let x*{..} = ..`: its element variable
         is used nowhere (like the plain-variable case above), so it can go. *)
      not (IdSet.mem id (free_of outs others))
  | LetPr (l, r) ->
      let bound = IdSet.union bound_head (binders_of_prems spec others) in
      IdSet.subset (Free.free_exp l) bound
      && (IdSet.is_empty (IdSet.inter (Free.free_exp r) (free_of outs others))
         (* ...or the equation already holds (e.g. a cast-stripped destructure
            `let (NUM n) = lit` once `lit ≡ (NUM n)`), so the binding only
            restates what the surrounding premises entail. *)
         || exp_equal
              (Prem_env.resolve_prem_env (Lazy.force env) l)
              (Prem_env.resolve_prem_env (Lazy.force env) r))
  | IfPr { cond = { it = CmpE (`EqOp, _, a, b); _ }; _ } ->
      exp_equal
        (Prem_env.resolve_prem_env (Lazy.force env) a)
        (Prem_env.resolve_prem_env (Lazy.force env) b)
      || dead_var a || dead_var b
  | IfPr { cond = { it = MatchE (e, pattern); _ }; _ } ->
      pattern_forced (Lazy.force env) e pattern || dead_var e
  | IfPr { cond = { it = SubE (e, _); _ }; _ } -> (
      (* A subtype test is implied once its subject has a reconstructed case
         form, or once the subject has been inlined away. The latter keeps an
         untagged injection like `expr <: id` (subject still a live binder, no
         case form) while dropping `exp <: lit` in a reconstructed `num` rule. *)
      dead_var e
      ||
      match (Prem_env.resolve_prem_env (Lazy.force env) e).it with
      | CaseE _ -> true
      | _ -> false)
  | IterPr (({ it = IfPr _; _ } as inner), _) ->
      (* An iterated pure test `(if ...)*{..}` is redundant when its element-level
         body is. The body's subject is an iteration variable, and [env_of_prems]
         lifts the co-iterated bindings to that same element level, so the entailment
         check applies unchanged. Restricted to [IfPr] (a test binds nothing), so
         dropping it never strands a binder the way an iterated [let] could. *)
      prem_redundant spec outs bound_head others inner
  | _ -> false

(* Drop redundant premises one at a time, each tested against the others still
   retained, so two mutually-entailing premises never both disappear. *)
let remove_redundant_prems (spec : spec) (outs : exp list)
    (bound_head : IdSet.t) (prems : prem list) : prem list =
  let rec go kept = function
    | [] -> List.rev kept
    | prem :: rest ->
        let others = List.rev_append kept rest in
        if prem_redundant spec outs bound_head others prem then go kept rest
        else go (prem :: kept) rest
  in
  go [] prems

let rec eq_prem (a : prem) (b : prem) : bool =
  match (a.it, b.it) with
  | RelPr { relid = id_a; notexp = ne_a }, RelPr { relid = id_b; notexp = ne_b }
    ->
      Eq.eq_id id_a id_b && Eq.eq_exps (Mixfix.args ne_a) (Mixfix.args ne_b)
  | ( RelAssertPr { call = { relid = id_a; notexp = ne_a }; expect = ex_a },
      RelAssertPr { call = { relid = id_b; notexp = ne_b }; expect = ex_b } ) ->
      ex_a = ex_b && Eq.eq_id id_a id_b
      && Eq.eq_exps (Mixfix.args ne_a) (Mixfix.args ne_b)
  | IfPr { cond = e_a; _ }, IfPr { cond = e_b; _ } -> Eq.eq_exp e_a e_b
  | DebugPr e_a, DebugPr e_b -> Eq.eq_exp e_a e_b
  | LetPr (l_a, r_a), LetPr (l_b, r_b) -> Eq.eq_exp l_a l_b && Eq.eq_exp r_a r_b
  | IterPr (p_a, ie_a), IterPr (p_b, ie_b) ->
      eq_prem p_a p_b && Eq.eq_iterexp ie_a ie_b
  | ElsePr, ElsePr -> true
  | _ -> false

and eq_prems (a : prem list) (b : prem list) : bool =
  List.length a = List.length b && List.for_all2 eq_prem a b

(* An opaque value-producing call: the binding holding its result is the only
   handle on it, so such bindings cannot be inlined, only collapsed. *)
let is_opaque (e : exp) = match e.it with CallE _ -> true | _ -> false

(* Whether [link_vars] (the variables of an intermediate link) occur anywhere
   but the premises at [excluded] indices -- i.e. in the conclusion [outs] or any
   other premise. Both call-idiom collapsers below fire only when this is false,
   so dropping the link strands nothing. *)
let link_used_elsewhere (outs : exp list) (arr : prem array)
    (excluded : int list) (link_vars : IdSet.t) : bool =
  let touches s = not (IdSet.is_empty (IdSet.inter link_vars s)) in
  List.exists (fun e -> touches (Free.free_exp e)) outs
  || Array.to_list arr
     |> List.mapi (fun k p -> (k, p))
     |> List.exists (fun (k, p) ->
            (not (List.mem k excluded)) && touches (Free.free_prem p))

(* Collapse an option/structure binding fed by an opaque call. Elaboration turns
   `if $f(..) = OUT` (when OUT is a relation/clause output) into
   `let v = $f(..)` + `if v matches Some` + `let PAT = v`; the first two are
   handled by the env, leaving `let v = $f(..)` and `let PAT = v`. Since a call
   cannot absorb the pattern, re-express the pair as the equality `if $f(..) = PAT`
   (the form the `assign` rule already uses), dropping both lets. Fires only when
   [v] appears nowhere else, so nothing is stranded. *)
let destructure_call_to_eq (outs : exp list) (prems : prem list) : prem list =
  let open Common.Source in
  let arr = Array.of_list prems in
  let n = Array.length arr in
  let found = ref None in
  for i = 0 to n - 1 do
    (* def: `let LINK = call`, where LINK is the intermediate (a var, or an
       iterated var `type'?{..}`) holding the opaque result. *)
    match (Option.is_none !found, arr.(i).it) with
    | true, LetPr (link, call) when is_opaque call ->
        for j = 0 to n - 1 do
          (* use: `let PAT = LINK`, PAT a structural pattern binding the output. *)
          match (Option.is_none !found, j <> i, arr.(j).it) with
          | true, true, LetPr (pat, rhs)
            when exp_equal rhs link && is_struct_lit pat ->
              (* LINK's variables must occur nowhere but these two premises. *)
              let link_vars = Free.free_exp link in
              if not (link_used_elsewhere outs arr [ i; j ] link_vars) then
                found := Some (i, j, call, pat)
          | _ -> ()
        done
    | _ -> ()
  done;
  match !found with
  | Some (i, j, call, pat) ->
      let eq =
        IfPr
          {
            cond = CmpE (`EqOp, `BoolT, call, pat) $$ no_region % BoolT;
            role = Condition;
          }
        $ no_region
      in
      prems
      |> List.mapi (fun k p ->
             if k = i then Some eq else if k = j then None else Some p)
      |> List.filter_map Fun.id
  | None -> prems

(* Collapse the partial-call option idiom, BEFORE substitution. Elaborating
   `if PAT = $f(..)` for a partial `$f` yields the triple
     let v = $f(..)        (* v : the option link *)
     if v matches (_)      (* require Some *)
     let ?(PAT) = v        (* bind the content *)
   The plain [destructure_call_to_eq] cannot reach this: the `matches` counts as a
   use of [v], and once the env substitutes [v]'s canonical into the third premise
   the `let ?(PAT) = v` no longer mentions [v], stranding the `matches` as an
   irreducible guard. So fuse the triple up front into the single binding
   `let ?(PAT) = $f(..)`, dropping the guard and the link let. A binding let (not
   the equality `if $f(..) = ?(PAT)`) is used deliberately: it is the binder for
   [PAT], so the redundancy pass never drops it -- whereas the equality becomes
   removable once [PAT] is reconstructed elsewhere, which would strand [PAT]'s
   variables. Fires only when [v] occurs nowhere else, so nothing is stranded. *)
let fuse_option_call_matches (outs : exp list) (prems : prem list) : prem list =
  let open Common.Source in
  let is_some_match (v : exp) (p : prem) =
    match p.it with
    | IfPr { cond = { it = MatchE (e, OptP `Some); _ }; _ } -> exp_equal e v
    | _ -> false
  in
  (* The link is an option value in variable-ish form: a bare [VarE], or the
     iterated-variable form `v?{..}` elaboration gives an option binder. *)
  let is_link (e : exp) =
    match e.it with
    | VarE _ -> true
    | IterE ({ it = VarE _; _ }, _) -> true
    | _ -> false
  in
  let arr = Array.of_list prems in
  let n = Array.length arr in
  let found = ref None in
  for i = 0 to n - 1 do
    match (Option.is_none !found, arr.(i).it) with
    | true, LetPr (v, call) when is_link v && is_opaque call -> (
        let km = ref None and ku = ref None in
        for k = 0 to n - 1 do
          if k <> i then (
            if is_some_match v arr.(k) then km := Some k;
            match arr.(k).it with
            | LetPr (({ it = OptE (Some _); _ } as pat), rhs)
              when exp_equal rhs v ->
                ku := Some (k, pat)
            | _ -> ())
        done;
        match (!km, !ku) with
        | Some km, Some (ku, pat) ->
            (* [v] (the link) must occur nowhere but these three premises. *)
            let v_vars = Free.free_exp v in
            if not (link_used_elsewhere outs arr [ i; km; ku ] v_vars) then
              found := Some (i, km, ku, call, pat)
        | _ -> ())
    | _ -> ()
  done;
  match !found with
  | Some (i, km, ku, call, pat) ->
      let bind = LetPr (pat, call) $ no_region in
      prems
      |> List.mapi (fun k p ->
             if k = i then Some bind
             else if k = km || k = ku then None
             else Some p)
      |> List.filter_map Fun.id
  | None -> prems

(* Substitute [from_e] -> [to_e] across every expression of a premise, including
   the [IfPr]/`holds`/`matches` positions that [subst_prem] leaves verbatim. Used
   by [inline_var_lets], where the binding being inlined is deleted, so its
   variable must be rewritten everywhere it is still used. *)
let rec subst_exp_in_prem (from_e : exp) (to_e : exp) (prem : prem) : prem =
  let s = Prem_env.subst_exp from_e to_e in
  let it =
    match prem.it with
    | RelPr { relid; notexp } -> RelPr { relid; notexp = Mixfix.map s notexp }
    | IfPr { cond; role } -> IfPr { cond = s cond; role }
    | RelAssertPr { call = { relid; notexp }; expect } ->
        RelAssertPr { call = { relid; notexp = Mixfix.map s notexp }; expect }
    | LetPr (l, r) -> LetPr (s l, s r)
    | IterPr (inner, ie) -> IterPr (subst_exp_in_prem from_e to_e inner, ie)
    | DebugPr e -> DebugPr (s e)
    | ElsePr -> ElsePr
  in
  { prem with it }

(* Fold a `matches` constraint on a let-bound variable into that `let`'s binder,
   dropping the constraint premise -- the inverse of how elaboration flattens a
   refined binding into `let v = E` + a check:
   - `let v = E` + `if v matches M`  -->  `let recon(M) = E` (fresh fields bound
     by the new pattern), so the variant shape lives in the binder.
   The subject's other uses are rewritten to the new binder form. Sound: [v] is
   only a name for [E], so `v ≡ recon(M)`. Fires only when [v] is bound by this
   `let` alone, so nothing is stranded. The call (or any [E]) stays as a
   `let`-bound step -- structure is pushed to the binder, not inlined.

   The `<:` counterpart is handled separately by [subtype_to_cast], which keeps
   the narrowing as an explicit `as` cast on the binder rather than as a bare
   type note. *)
let fold_constraint_into_let (spec : spec) (head_bound : IdSet.t)
    (prems : prem list) (outs : exp list) : prem list * exp list =
  let rec go prems outs =
    let arr = Array.of_list prems in
    let n = Array.length arr in
    let bound_elsewhere i =
      let others = List.filteri (fun k _ -> k <> i) prems in
      IdSet.union head_bound (binders_of_prems spec others)
    in
    let result = ref None in
    for i = 0 to n - 1 do
      if Option.is_none !result then
        match arr.(i).it with
        | LetPr (({ it = VarE v; _ } as lhs), e)
          when (not (IdSet.mem v (bound_elsewhere i)))
               && not (IdSet.mem v (Free.free_exp e)) ->
            for j = 0 to n - 1 do
              if Option.is_none !result && j <> i then
                match arr.(j).it with
                | IfPr
                    { cond = { it = MatchE ({ it = VarE v'; _ }, pat); _ }; _ }
                  when Eq.eq_id v v' -> (
                    match Prem_env.reconstruct_pattern spec lhs pat with
                    | Some p' -> result := Some (i, j, lhs, e, p')
                    | None -> ())
                | _ -> ()
            done
        | _ -> ()
    done;
    match !result with
    | Some (i, j, lhs, e, newpat) ->
        let prems' =
          prems
          |> List.mapi (fun k (p : prem) ->
                 if k = j then None
                 else if k = i then Some { p with it = LetPr (newpat, e) }
                 else Some (subst_exp_in_prem lhs newpat p))
          |> List.filter_map Fun.id
        in
        let outs' = List.map (Prem_env.subst_exp lhs newpat) outs in
        go prems' outs'
    | None -> (prems, outs)
  in
  go prems outs

(* The (element type, iterator) of an iterated type, seen through named type
   aliases (`syntax foo = bar*` gives a [VarT foo] note rather than a bare
   [IterT]). [None] when the type is not, even transitively, an iteration. *)
let rec iter_elem (spec : spec) (t : typ') : (typ * iter) option =
  match t with
  | IterT { typ = elem; iter } -> Some (elem, iter)
  | VarT { synid = tid; _ } ->
      List.find_map
        (fun (d : def) ->
          match d.it with
          | TypD { synid = id2; deftyp = { it = PlainT bt; _ }; _ }
            when id2.it = tid.it ->
              iter_elem spec bt.it
          | _ -> None)
        spec
  | _ -> None

(* Number of `let` premises whose left-hand side contains [subj] (a binder
   position). For the fold below to bind its fresh fields exactly once, [subj]
   must be destructured by exactly one binding. *)
let binder_occurrences (subj : exp) (prems : prem list) : int =
  let rec lhs_of (p : prem) =
    match p.it with
    | LetPr (l, _) -> Some l
    | IterPr (inner, _) -> lhs_of inner
    | _ -> None
  in
  List.fold_left
    (fun c p ->
      match lhs_of p with Some l when occurs_in subj l -> c + 1 | _ -> c)
    0 prems

(* Fold a `matches` on an iterated or payload-carrying subject into its binder,
   the iteration/payload counterpart of [fold_constraint_into_let]. Where that one
   needs a plain `let v = E` and a payload-free pattern, this one handles an
   iterated subject bound by a `let` (whether on its own, inside a tuple, or inside
   an option pattern) plus the payload shapes (cons, fixed-length, some),
   reconstructing the subject's shape with FRESH element variables typed by the
   iteration's element type and substituting the whole subject -- binder included --
   so a cons `matches` turns `let xs = E` into `let x_h :: x_t = E`. The call/field
   [E] stays as the binding.

   Fires only when (a) no companion premise already pins the shape down (those are
   handled by [pattern_forced]'s class check, no fresh names needed); (b) [subj]
   is destructured by exactly one binding (so the fresh fields bind once); and
   (c) after substitution none of [subj]'s variables remain free (an element-level
   use the whole-iteration substitution could not reach would otherwise strand). *)
let fold_iter_match (spec : spec) (prems : prem list) (outs : exp list) :
    prem list * exp list =
  let open Common.Source in
  let rec go prems outs =
    let arr = Array.of_list prems in
    let n = Array.length arr in
    let taken =
      List.fold_left
        (fun s p -> IdSet.union s (Free.free_prem p))
        (List.fold_left
           (fun s e -> IdSet.union s (Free.free_exp e))
           IdSet.empty outs)
        prems
    in
    let fresh (base : string) (t : typ') : exp =
      let rec pick i =
        let name = if i = 0 then base else Printf.sprintf "%s_%d" base i in
        if IdSet.mem (name $ no_region) taken then pick (i + 1) else name
      in
      VarE (pick 0 $ no_region) $$ no_region % t
    in
    (* reconstruct [subj]'s shape per [pat], minting fresh dummies as needed. *)
    let reconstruct (subj : exp) (pat : pattern) : exp option =
      let mk e' = e' $$ no_region % subj.note in
      let base =
        match subj.it with
        | IterE ({ it = VarE id; _ }, _) | VarE id -> id.it
        | _ -> "x"
      in
      match (iter_elem spec subj.note, pat) with
      | Some (_, List), ListP `Nil -> Some (mk (ListE []))
      | Some (elem, List), ListP `Cons ->
          let h = fresh (base ^ "_h") elem.it in
          let t = fresh (base ^ "_t") subj.note in
          Some (mk (ConsE (h, t)))
      | Some (elem, List), ListP (`Fixed m) ->
          let es =
            List.init m (fun i -> fresh (Printf.sprintf "%s_%d" base i) elem.it)
          in
          Some (mk (ListE es))
      | Some (_, Opt), OptP `None -> Some (mk (OptE None))
      | Some (elem, Opt), OptP `Some ->
          Some (mk (OptE (Some (fresh (base ^ "_0") elem.it))))
      | _ -> Prem_env.reconstruct_pattern spec subj pat
    in
    let subj_ok (e : exp) =
      match e.it with
      | VarE _ | IterE ({ it = VarE _; _ }, _) -> true
      | _ -> false
    in
    let result = ref None in
    for j = 0 to n - 1 do
      if Option.is_none !result then
        match arr.(j).it with
        | IfPr { cond = { it = MatchE (subj, pat); _ }; _ } when subj_ok subj
          -> (
            (* Skip only when an actual destructuring `let` already binds the same
               value to this shape (a persistent companion, e.g. `let h :: t = E`
               alongside `let xs = E`): there [pattern_forced]'s class check drops
               the `matches` with the real names. A reconstruction the env adds for
               OTHER premises does not count -- it need not survive to the redundancy
               pass -- so we look for the companion among the let bindings only. *)
            let env = Prem_env.env_of_prems spec prems in
            let canon_subj = Prem_env.resolve_prem_env env subj in
            let companion =
              List.exists
                (fun (p : prem) ->
                  match p.it with
                  (* a separate destructure of the same value -- its pattern must
                     not contain [subj] itself (that would be [subj]'s own enclosing
                     binder, e.g. `let ?(.. x? ..) = E` for `x? matches (_)`). *)
                  | LetPr (l, r) ->
                      (not (occurs_in subj l))
                      && single_pattern_match pat l
                      && exp_equal canon_subj (Prem_env.resolve_prem_env env r)
                  | _ -> false)
                prems
            in
            let bo = binder_occurrences subj prems in
            let recon0 =
              if companion || bo <> 1 then None else reconstruct subj pat
            in
            match recon0 with
            | Some recon ->
                let prems' =
                  prems
                  |> List.mapi (fun k p ->
                         if k = j then None
                         else Some (subst_exp_in_prem subj recon p))
                  |> List.filter_map Fun.id
                in
                let outs' = List.map (Prem_env.subst_exp subj recon) outs in
                (* gate: [subj]'s variables must be gone after substitution. *)
                let subj_vars = Free.free_exp subj in
                let free_after =
                  List.fold_left
                    (fun s e -> IdSet.union s (Free.free_exp e))
                    (List.fold_left
                       (fun s p -> IdSet.union s (Free.free_prem p))
                       IdSet.empty prems')
                    outs'
                in
                if IdSet.is_empty (IdSet.inter subj_vars free_after) then
                  result := Some (prems', outs')
            | None -> ())
        | _ -> ()
    done;
    match !result with
    | Some (prems', outs') -> go prems' outs'
    | None -> (prems, outs)
  in
  go prems outs

(* Decompose a (binding) pattern [l] against a structural value [r] of the same
   shape into the component substitutions `leaf -> sub` it introduces, or None
   when the shapes do not line up or a leaf is neither a plain nor an iterated
   variable. A plain [VarE] leaf substitutes by structural equality (every use is
   an exact match). An iterated `x*{..}` leaf substitutes the *whole iteration*:
   only safe when every use of [x] is that same iteration, which the caller
   ([inline_lets]) verifies with a post-substitution free-variable gate. *)
let rec decompose_pat (l : exp) (r : exp) : (exp * exp) list option =
  if exp_equal l r then Some [] (* identical positions: nothing to substitute *)
  else
    match l.it with
    | VarE _ | IterE ({ it = VarE _; _ }, _) -> Some [ (l, r) ]
    | _ -> (
        let decompose_list ls rs =
          if List.length ls <> List.length rs then None
          else
            List.fold_left2
              (fun acc l r ->
                match (acc, decompose_pat l r) with
                | Some a, Some b -> Some (a @ b)
                | _ -> None)
              (Some []) ls rs
        in
        match (l.it, r.it) with
        | CaseE ne_l, CaseE ne_r
          when Mixfix.eq_mixop (Mixfix.to_mixop ne_l) (Mixfix.to_mixop ne_r) ->
            decompose_list (Mixfix.args ne_l) (Mixfix.args ne_r)
        | TupleE es_l, TupleE es_r -> decompose_list es_l es_r
        | ListE es_l, ListE es_r -> decompose_list es_l es_r
        | StrE fs_l, StrE fs_r
          when List.length fs_l = List.length fs_r
               && List.for_all2
                    (fun ((a : atom), _) ((b : atom), _) -> a.it = b.it)
                    fs_l fs_r ->
            decompose_list (List.map snd fs_l) (List.map snd fs_r)
        | ConsE (h_l, t_l), ConsE (h_r, t_r) ->
            decompose_list [ h_l; t_l ] [ h_r; t_r ]
        | OptE (Some x), OptE (Some y) -> decompose_pat x y
        | OptE None, OptE None -> Some []
        | _ -> None)

(* Inline a destructuring `let PAT = E` whose right-hand side is a concrete
   non-call structure, by substituting PAT's variables with E's matching
   sub-expressions everywhere (the conclusion and every other premise) and
   dropping the binding. This is the dual of [destructure_call_to_eq]: a call-fed
   destructure cannot inline (the call is opaque), but a structure-fed one can --
   `let n_h :: n_t = a :: b` becomes the substitution `n_h -> a, n_t -> b`.

   The env pass already inlines a *plain-variable* `let x = struct` (folding it in
   value positions, then dropping it dead); what is left for here are the
   destructuring patterns it could not project through. PAT's variables must be
   bound by this let alone (not by the head, not by another premise's pattern),
   the occurs check (PAT's variables not free in E) excludes a self-referential
   binding, and the decomposition must reach plain-variable leaves so no use is
   stranded. *)
let inline_lets (spec : spec) (head_bound : IdSet.t) (prems : prem list)
    (outs : exp list) : prem list * exp list =
  (* The variable a leaf binds: a plain [VarE], or the element variable of an
     iterated `x*{..}` leaf. *)
  let leaf_id (v : exp) : id option =
    match v.it with
    | VarE id -> Some id
    | IterE ({ it = VarE id; _ }, _) -> Some id
    | _ -> None
  in
  let apply_prem subs p =
    List.fold_left (fun p (l, r) -> subst_exp_in_prem l r p) p subs
  in
  let apply_exp subs e =
    List.fold_left (fun e (l, r) -> Prem_env.subst_exp l r e) e subs
  in
  let candidate (others : prem list) (outs : exp list) (prem : prem) =
    match prem.it with
    | LetPr (l, r) when is_struct_lit r -> (
        (* Decompose first, dropping identical sub-positions; then vet each
           resulting substitution. A pattern var bound elsewhere (e.g. the shared
           tail of `h :: t* = K :: t*`) yields no substitution, so it neither
           blocks the inline nor gets rewritten -- only the genuinely-new leaf is
           substituted. Each substituted leaf must be a plain or iterated variable
           bound by this let alone, with the occurs check (leaf id not free in its
           value). *)
        let bound_elsewhere =
          IdSet.union head_bound (binders_of_prems spec others)
        in
        match decompose_pat l r with
        | Some subs
          when subs <> []
               && List.for_all
                    (fun ((v : exp), (e : exp)) ->
                      match leaf_id v with
                      | Some id ->
                          (not (IdSet.mem id bound_elsewhere))
                          && not (IdSet.mem id (Free.free_exp e))
                      | None -> false)
                    subs ->
            (* Soundness gate (matters for iterated leaves): after applying the
               substitutions, no leaf variable may remain free anywhere -- an
               element-level use of [x] that the whole-iteration substitution
               could not reach would otherwise be stranded. *)
            let leaf_ids = List.filter_map (fun (v, _) -> leaf_id v) subs in
            let free_after =
              let from_prems =
                List.fold_left
                  (fun s p ->
                    IdSet.union s (Free.free_prem (apply_prem subs p)))
                  IdSet.empty others
              in
              List.fold_left
                (fun s e -> IdSet.union s (Free.free_exp (apply_exp subs e)))
                from_prems outs
            in
            if List.exists (fun id -> IdSet.mem id free_after) leaf_ids then
              None
            else Some subs
        | _ -> None)
    | _ -> None
  in
  let rec take_first outs before = function
    | [] -> None
    | prem :: after -> (
        let others = List.rev_append before after in
        match candidate others outs prem with
        | Some subs -> Some (others, subs)
        | None -> take_first outs (prem :: before) after)
  in
  let rec loop prems outs =
    match take_first outs [] prems with
    | None -> (prems, outs)
    | Some (rest, subs) ->
        loop (List.map (apply_prem subs) rest) (List.map (apply_exp subs) outs)
  in
  loop prems outs

(* Number of times the variable [target] occurs in an expression. [Free] only
   reports presence (a set), so a dedicated counter is needed to tell a
   single-use binding (safe to inline anywhere) from a shared one. *)
let count_var_exp (target : id) (e0 : exp) : int =
  let rec go (e : exp) : int =
    (match e.it with VarE v when Eq.eq_id v target -> 1 | _ -> 0)
    + List.fold_left (fun n s -> n + go s) 0 (Exp_map.subexps e.it)
  in
  go e0

let count_var_prem (target : id) (prem : prem) : int =
  List.fold_left
    (fun a e -> a + count_var_exp target e)
    0
    (Exp_map.exps_of_prem prem)

(* Inline a `let v = rhs` whose left-hand side is a single plain variable, then
   drop it. Two cases are safe and fire:
   - [rhs] is itself a variable (a pure alias `let v = w`): inline regardless of
     how many times [v] is used -- substituting a variable never duplicates work;
   - otherwise [v] is used at most once across the other premises and outputs
     (single use): inline that one occurrence, so no expression is duplicated.
   This catches the call/field/concat/arithmetic `let`s whose value flows into a
   single use (a guard included -- [subst_exp_in_prem] reaches `if`/`matches`), and
   the variable renames the env leaves in place. [v] must be bound by this `let`
   alone (not the head, not another premise) and must not occur in [rhs] (occurs
   check), so nothing is stranded. *)
let inline_value_lets (spec : spec) (head_bound : IdSet.t) (prems : prem list)
    (outs : exp list) : prem list * exp list =
  let rec loop prems outs =
    let arr = Array.of_list prems in
    let n = Array.length arr in
    let bound_elsewhere i =
      let others = List.filteri (fun k _ -> k <> i) prems in
      IdSet.union head_bound (binders_of_prems spec others)
    in
    let result = ref None in
    for i = 0 to n - 1 do
      if Option.is_none !result then
        match arr.(i).it with
        | LetPr (({ it = VarE v; _ } as lhs), rhs)
          when (not (IdSet.mem v (bound_elsewhere i)))
               && not (IdSet.mem v (Free.free_exp rhs)) ->
            let is_rename = match rhs.it with VarE _ -> true | _ -> false in
            let single_use () =
              let others = List.filteri (fun k _ -> k <> i) prems in
              let occ =
                List.fold_left (fun a p -> a + count_var_prem v p) 0 others
                + List.fold_left (fun a e -> a + count_var_exp v e) 0 outs
              in
              occ <= 1
            in
            if is_rename || single_use () then result := Some (i, lhs, rhs)
        | _ -> ()
    done;
    match !result with
    | Some (i, lhs, rhs) ->
        let prems' =
          prems
          |> List.mapi (fun k p ->
                 if k = i then None else Some (subst_exp_in_prem lhs rhs p))
          |> List.filter_map Fun.id
        in
        let outs' = List.map (Prem_env.subst_exp lhs rhs) outs in
        loop prems' outs'
    | None -> (prems, outs)
  in
  loop prems outs

(* Resolve a subtype premise `if S <: T` (S a plain variable) by moving the
   narrowing into S's first definition as an explicit `as` cast, then dropping
   the `<:`. This mirrors how a `matches` guard folds into its binder
   ([fold_constraint_into_let]): the refinement lives at the point of definition,
   not as a free-standing guard. Runs LATE -- after [normalize_deep] has stripped
   every other cast -- so the cast it introduces is the only `as` left, sitting
   exactly on the binding that needs it.

   Only a *bare* top-level [SubE] whose subject is a plain variable is touched; a
   `<:` nested under ~ / \/ / => or inside an iteration is a genuine predicate
   (e.g. `if ~typeIR <: setTypeIR`) and is left as a guard. For each:
   - identity `S <: S` (S's type already T): drop, no cast needed;
   - otherwise, only when narrowing keeps the new binder LIVE (else the binding
     would be dead, pruned, and the check silently lost -- so the `<:` stays):
     - a companion alias `let L = S` (L another variable, already typed T):
       retarget it to `let L = (S as T)` -- L now carries the narrowed value;
     - S's own binder `let S = E`: replace it with `let S_T = (E as T)` (fresh
       [S_T : T]) and rewrite S -> S_T throughout;
     - no binder, S head-bound (a clause/rule input): prepend `let S_T = (S as T)`
       and rewrite S -> S_T in the premises (the head keeps binding S).
   The cast is a [DownCastE] -- a `<:` injection narrows a wider value to [T], the
   same checked downcast elaboration emits and the rest of this pass assumes (see
   [injection_pairs]). *)
let subtype_to_cast (head_bound : IdSet.t) (prems : prem list) (outs : exp list)
    : prem list * exp list =
  let open Common.Source in
  let down (t : typ) (s : exp) : exp = DownCastE (t, s) $$ no_region % t.it in
  let typ_eq (a : typ') (b : typ') =
    Eq.eq_typ (a $ no_region) (b $ no_region)
  in
  (* a fresh variable typed [t], named after [t] (else [base]), unique here. *)
  let fresh_typed (taken : IdSet.t) (base : string) (t : typ) : exp =
    let root = match t.it with VarT { synid; _ } -> synid.it | _ -> base in
    let rec pick i =
      let name = if i = 0 then root else Printf.sprintf "%s_%d" root i in
      if IdSet.mem (name $ no_region) taken then pick (i + 1) else name
    in
    VarE (pick 0 $ no_region) $$ no_region % t.it
  in
  (* [nv] is live in the candidate result when it occurs beyond the single LHS
     position of the `let` that binds it (the cast RHS never mentions it). *)
  let live (nv : id) (prems' : prem list) (outs' : exp list) : bool =
    let c =
      List.fold_left (fun a p -> a + count_var_prem nv p) 0 prems'
      + List.fold_left (fun a e -> a + count_var_exp nv e) 0 outs'
    in
    c > 1
  in
  let rec go prems outs =
    let arr = Array.of_list prems in
    let n = Array.length arr in
    let taken =
      List.fold_left
        (fun s p -> IdSet.union s (Free.free_prem p))
        (List.fold_left
           (fun s e -> IdSet.union s (Free.free_exp e))
           IdSet.empty outs)
        prems
    in
    let drop_at i =
      prems
      |> List.mapi (fun k p -> if k = i then None else Some p)
      |> List.filter_map Fun.id
    in
    let result = ref None in
    for i = 0 to n - 1 do
      if Option.is_none !result then
        match arr.(i).it with
        | IfPr { cond = { it = SubE (({ it = VarE s; _ } as se), t); _ }; _ }
          -> (
            if typ_eq se.note t.it then result := Some (drop_at i, outs)
            else
              (* companion alias `let L = S`, L a distinct var already typed T *)
              let companion =
                List.mapi (fun k p -> (k, p)) prems
                |> List.find_opt (fun (k, (p : prem)) ->
                       k <> i
                       &&
                       match p.it with
                       | LetPr (({ it = VarE l; _ } as lhs), r) ->
                           (not (Eq.eq_id l s))
                           && exp_equal r se && typ_eq lhs.note t.it
                       | _ -> false)
              in
              match companion with
              | Some (k, { it = LetPr (lhs, _); _ }) -> (
                  let prems' =
                    prems
                    |> List.mapi (fun m p ->
                           if m = i then None
                           else if m = k then
                             Some { p with it = LetPr (lhs, down t se) }
                           else Some p)
                    |> List.filter_map Fun.id
                  in
                  match lhs.it with
                  | VarE l when live l prems' outs ->
                      result := Some (prems', outs)
                  | _ -> ())
              | _ -> (
                  (* S's own binder `let S = E` *)
                  let own =
                    List.mapi (fun k p -> (k, p)) prems
                    |> List.find_opt (fun (_, (p : prem)) ->
                           match p.it with
                           | LetPr ({ it = VarE s'; _ }, _) -> Eq.eq_id s' s
                           | _ -> false)
                  in
                  let s_t = fresh_typed taken s.it t in
                  let nv =
                    match s_t.it with VarE id -> id | _ -> assert false
                  in
                  match own with
                  | Some (k, { it = LetPr (_, e); _ }) ->
                      let prems' =
                        prems
                        |> List.mapi (fun m p ->
                               if m = i then None
                               else if m = k then
                                 Some { p with it = LetPr (s_t, down t e) }
                               else Some (subst_exp_in_prem se s_t p))
                        |> List.filter_map Fun.id
                      in
                      let outs' = List.map (Prem_env.subst_exp se s_t) outs in
                      if live nv prems' outs' then result := Some (prems', outs')
                  | _ when IdSet.mem s head_bound ->
                      (* head-bound input: prepend the narrowed binder, rewrite
                         only the premise uses (the head keeps binding [S]). *)
                      let bind = LetPr (s_t, down t se) $ no_region in
                      let prems' =
                        prems
                        |> List.mapi (fun m p ->
                               if m = i then None
                               else Some (subst_exp_in_prem se s_t p))
                        |> List.filter_map Fun.id
                      in
                      if live nv (bind :: prems') outs then
                        result := Some (bind :: prems', outs)
                  | _ -> ()))
        | _ -> ()
    done;
    match !result with
    | Some (prems', outs') -> go prems' outs'
    | None -> (prems, outs)
  in
  go prems outs

(* Collapse a "re-zip" iteration into a single iterated variable. The elaborator
   destructures a list tail into co-iterated element streams -- e.g. `$lookup`'s
   tail `(K_t -> V_t)*{K_t <- K_t*, V_t <- V_t*}` unzips a `pair*` into key/value
   streams in the head pattern and re-zips them in the recursive call. When the
   element variables are used ONLY inside structurally-identical copies of that
   iteration (every unzip is matched by a re-zip), the whole iteration carries
   exactly the underlying list's information, so replacing every copy with one
   fresh iterated variable `t*{t <- t*}` is meaning-preserving.

   This matters for the CTRS translation: a bare iterated variable maps straight
   to a list term (`term_of_exp (IterE (VarE t, _)) = t`), giving a linear tail
   pattern in binder position -- whereas a structured body would otherwise
   collapse to a single (wrongly-shaped) element term. The soundness gate is the
   same shape as [fold_iter_match]'s: after substituting every copy, none of the
   iteration's body variables (= [Free.free_exp] of the iteration, since [Free]
   ignores the binders) may remain free. *)
let collapse_rezip_iters (spec : spec) (prems : prem list) (outs : exp list) :
    prem list * exp list =
  let open Common.Source in
  (* Every IterE node whose body is not already a bare variable (those are
     collapsed already), gathered from [outs] and [prems], innermost included. *)
  let candidates prems outs =
    let rec visit_exp acc (e : exp) =
      let acc =
        match e.it with
        | IterE ({ it = VarE _; _ }, _) -> acc
        | IterE _ -> e :: acc
        | _ -> acc
      in
      List.fold_left visit_exp acc (Exp_map.subexps e.it)
    in
    let visit_prem acc (p : prem) =
      List.fold_left visit_exp acc (Exp_map.exps_of_prem p)
    in
    List.fold_left visit_prem (List.fold_left visit_exp [] outs) prems
  in
  let try_collapse prems outs (i : exp) : (prem list * exp list) option =
    match iter_elem spec i.note with
    | None -> None
    (* A variable-free body (`?()*{}`) makes the vanish gate below vacuous, and
       collapsing would erase the body's shape constraint (every element must
       BE that constant) rather than fold a round-trip. *)
    | Some _ when IdSet.is_empty (Free.free_exp i) -> None
    | Some (elem, iter) ->
        let taken = free_of outs prems in
        let base =
          match elem.it with VarT { synid; _ } -> synid.it | _ -> "x"
        in
        let rec pick k =
          let name = if k = 0 then base else Printf.sprintf "%s_%d" base k in
          if IdSet.mem (name $ no_region) taken then pick (k + 1) else name
        in
        let tname = pick 0 $ no_region in
        let var_e = VarE tname $$ no_region % elem.it in
        let i' =
          IterE (var_e, (iter, [ { varid = tname; typ = elem; iters = [] } ]))
          $$ i.at % i.note
        in
        let outs' = List.map (Prem_env.subst_exp i i') outs in
        let prems' = List.map (subst_exp_in_prem i i') prems in
        let vanish = Free.free_exp i in
        let remaining = free_of outs' prems' in
        if IdSet.is_empty (IdSet.inter vanish remaining) then
          Some (prems', outs')
        else None
  in
  let rec go prems outs =
    match List.find_map (try_collapse prems outs) (candidates prems outs) with
    | Some (prems', outs') -> go prems' outs'
    | None -> (prems, outs)
  in
  go prems outs

(* Substitute, then prune, iterating to a fixed point. [outs] are the output
   expressions of the conclusion (a relation's arguments, or a clause's return
   expression); binder positions (clause args) are handled by the caller.

   The occurs check in [subst_of_env] and the monotonically shrinking premise
   list together guarantee termination; [fuel] is a defensive backstop so a
   pathological spec degrades to "less simplified" rather than a hang.

   [head_bound] computes, from the current (substituted) output expressions, the
   variables the head pattern binds; it gates removal of destructuring lets. *)
let simplify_block (spec : spec) ~(head_bound : exp list -> IdSet.t)
    ?(hoist = false) (prems : prem list) (outs : exp list) :
    prem list * exp list =
  (* Fuse the partial-call option idiom into equalities first, so the `matches`
     guards fold away instead of being stranded by the substitution below. *)
  let rec fuse prems =
    let prems' = fuse_option_call_matches outs prems in
    if List.length prems' < List.length prems then fuse prems' else prems'
  in
  let prems = fuse prems in
  (* Fold `matches`/`<:` constraints on a let-bound variable into that let's
     binder (so the structure/type lives in the binding, not a separate guard). *)
  let prems, outs =
    fold_constraint_into_let spec (head_bound outs) prems outs
  in
  (* Then the iteration/payload counterpart: fold a `matches` on an iterated or
     payload-carrying subject into its binder with fresh element variables. *)
  let prems, outs = fold_iter_match spec prems outs in
  let rec loop fuel prems outs =
    if fuel <= 0 then (prems, outs)
    else
      let env = Prem_env.env_of_prems spec prems in
      (* For a clause, additionally reconstruct a struct input from a field-access
         constraint (`let f = TC.local.frames`, `if f matches _::_`) into a struct
         head pattern, so those premises fold into the head. *)
      let hpairs =
        if hoist then Prem_env.hoist_pairs spec env (head_bound outs) else []
      in
      let pairs = subst_of_env (match_bound_vars prems) env @ hpairs in
      let ipairs = injection_pairs spec prems in
      (* outs are head/binder positions: structural folds + subtype injections,
         but no var=var equality renames. *)
      let outs' =
        List.map (subst_with (structural_pairs pairs @ ipairs)) outs
      in
      let hb = head_bound outs' in
      (* The block's own variables: anything in the premises or the (substituted)
         outputs. A pair target outside this set carries a fresh env-minted var
         that a `let` cannot bind (see [subst_prem]'s LetPr case). *)
      let known =
        List.fold_left
          (fun s p -> IdSet.union s (Free.free_prem p))
          (List.fold_left
             (fun s e -> IdSet.union s (Free.free_exp e))
             IdSet.empty outs')
          prems
      in
      let srcs = destructure_source_map prems in
      let prems' =
        prems
        |> List.map (subst_prem spec hb known srcs ipairs pairs)
        (* hoist (struct-input) pairs must also reach `if` guards, else the root is
           stranded once the head binds its fields instead of the whole input. *)
        |> List.map (subst_guards_with hpairs)
        |> remove_redundant_prems spec outs' hb
      in
      if eq_prems prems prems' && Eq.eq_exps outs outs' then (prems', outs')
      else loop (fuel - 1) prems' outs'
  in
  let prems, outs = loop 100 prems outs in
  (* Final normalization: drop every residual `as` (e.g. an output `=> n as
     value`), collapse opaque-call destructures into equalities, then prune
     anything that became trivial. *)
  let outs = List.map (normalize_deep spec) outs in
  let prems = List.map (normalize_deep_prem spec) prems in
  let prems = destructure_call_to_eq outs prems in
  (* Iterate: removing a guard (an `if`/`<:`/`matches` over a now-concrete struct
     field) can leave the `let` that fed it dead, which a further pass then drops. *)
  let rec prune outs prems =
    let prems' = remove_redundant_prems spec outs (head_bound outs) prems in
    if List.length prems' < List.length prems then prune outs prems' else prems'
  in
  let prems = prune outs prems in
  (* Inline lets, then prune, to a fixed point: [inline_lets] projects a
     destructuring `let` fed by a concrete structure through its components,
     [inline_value_lets] inlines single-use / alias bindings (including into
     guards); either can make a further binding dead or a guard trivially hold. *)
  let rec settle prems outs =
    let before = List.length prems in
    (* The iterated/payload `matches` fold also belongs here: the clean
       `let xs = E` binders it needs only appear once the main loop has run. *)
    let prems, outs = fold_iter_match spec prems outs in
    let prems, outs = inline_lets spec (head_bound outs) prems outs in
    let prems, outs = inline_value_lets spec (head_bound outs) prems outs in
    let prems = prune outs prems in
    if List.length prems < before then settle prems outs else (prems, outs)
  in
  let prems, outs = settle prems outs in
  (* Collapse re-zip iterations to a single iterated variable once the head
     pattern is reconstructed (so the destructured tail and its re-zipped reuse
     are in their final, structurally-equal form). *)
  let prems, outs = collapse_rezip_iters spec prems outs in
  (* Last: move each remaining `if S <: T` injection into an `as` cast on S's
     definition and drop the `<:`. After [settle], so the binders it retargets are
     in their final form; after [normalize_deep], so the cast it mints survives. *)
  subtype_to_cast (head_bound outs) prems outs

(* A relation rule's head binds variables in its input positions; substitution
   may have turned those into structured patterns (e.g. `e_l + e_r`). *)
let simplify_rule (spec : spec) (inputs : int list) (rule : rule) : rule =
  let { ruleid; concl = notexp; prems } = rule.it in
  let head_bound outs =
    List.fold_left
      (fun s i ->
        match List.nth_opt outs i with
        | Some e -> IdSet.union s (Free.free_exp e)
        | None -> s)
      IdSet.empty inputs
  in
  let prems, outs =
    simplify_block spec ~head_bound prems (Mixfix.args notexp)
  in
  let notexp = Mixfix.fill (Mixfix.to_mixop notexp) outs in
  { rule with it = { ruleid; concl = notexp; prems } }

(* A function clause's argument expressions are binder patterns. We make them
   substitution targets (like a rule's input positions) so a list/case pattern
   destructured against an argument folds back into the head -- e.g. the `$lookup`
   clauses' `let (K_h -> V_h) :: .. = pair*` collapses `pair*` into a `::` pattern.
   Their (post-substitution) free variables are what the head binds. *)
let simplify_clause (spec : spec) (clause : clause) : clause =
  let { args; body = exp; prems } = clause.it in
  let arg_exps =
    List.filter_map
      (fun (a : arg) -> match a.it with ExpA e -> Some e | DefA _ -> None)
      args
  in
  let n_args = List.length arg_exps in
  let take n xs =
    let rec go n = function
      | e :: r when n > 0 -> e :: go (n - 1) r
      | _ -> []
    in
    go n xs
  in
  let head_bound outs =
    take n_args outs
    |> List.fold_left (fun s e -> IdSet.union s (Free.free_exp e)) IdSet.empty
  in
  let prems, outs =
    simplify_block spec ~head_bound ~hoist:true prems (arg_exps @ [ exp ])
  in
  let arg_exps', exp' =
    match List.rev outs with
    | last :: rev_init -> (List.rev rev_init, last)
    | [] -> (arg_exps, exp)
  in
  let remaining = ref arg_exps' in
  let args' =
    List.map
      (fun (a : arg) ->
        match a.it with
        | ExpA _ -> (
            match !remaining with
            | e :: rest ->
                remaining := rest;
                { a with it = ExpA e }
            | [] -> a)
        | DefA _ -> a)
      args
  in
  { clause with it = { args = args'; body = exp'; prems } }

let simplify_def (spec : spec) (def : def) : def =
  let it =
    match def.it with
    | RelD { relid; reltyp; rules } ->
        let n = List.length (Mixfix.args (Mode.notation reltyp.it)) in
        let inputs, _ = Mode.partition reltyp.it (List.init n Fun.id) in
        RelD
          { relid; reltyp; rules = List.map (simplify_rule spec inputs) rules }
    | DecD { defid; tparams; params; typ; clauses } ->
        DecD
          {
            defid;
            tparams;
            params;
            typ;
            clauses = List.map (simplify_clause spec) clauses;
          }
    | (TypD _ | BuiltinDecD _) as it -> it
  in
  { def with it }

(* Relation lookups need the whole spec, so the original is threaded through
   every rule and clause rather than the partially-rewritten one. *)
let simplify_spec (spec : spec) : spec = List.map (simplify_def spec) spec
