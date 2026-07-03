open Common.Source
open Lang.Il

(** Translation from elaborated + simplified IL into the COPS CTRS
    representation ({!Rewrite_system}).

    Two families of rules are produced:

    - {b prelude + type-derived definitions} ([defs_of_typ], [prelude]): Peano
      naturals, booleans, list/option constructors with their operations, and --
      from each [TypD] -- variant constructors, their matchers, struct field
      accessors, and subtype predicates.

    - {b spec body rules} (from [DecD]/[RelD] of the simplified spec): function
      clauses and relation rules become (conditional) rewrite rules.

    The prelude/type-derived family is then pruned ([prune_unused]) down to the
    symbols reachable from the body rules. *)

module R = Rewrite_system

(* The structural CTRS vocabulary (symbol naming + smart term/rule builders)
   this translation builds every rule through. [scalar_theory]/[Structural]/
   [Native] and the mode-aware scalar leaf builders ([bool_t]/[term_of_num]/
   [text_t]/[nat_lit]/[conj_t]) come from here; the mode is threaded as
   [~scalars] so each scalar leaf is emitted in the right theory at translation
   time (no separate fold). *)
open Ctrs_term

type scalar_theory = Ctrs_term.scalar_theory = Structural | Native

(* -------------------------------------------------------------------------- *)
(* Iteration helpers ([IterE]/[IterPr]). An iteration recurses over one or more
   co-iterated lists/options in lock-step, so it compiles to an auxiliary
   recursive symbol that destructures the [cons]/[nil] (or [some]/[none])
   spines. At a use site [Var id] denotes the list, while inside a defining rule
   the element is renamed to a fresh per-step variable. The symbol is a
   structural key of the iterated body/premise, so identical iterations share
   one definition. *)

module IdSet = Common.Domain.IdSet

let iter_tag (iter : iter) : string =
  match iter with List -> "list" | Opt -> "opt"

(* A deterministic symbol for an iteration helper: [base] (`$itermap`,
   `$unzip`, …), the iterated body/premise's pretty-printed descriptor, the
   iterator tag, then any discriminating [parts] (arity, binder name). The one
   place the descriptor is sanitized and length-bounded ([abbrev]). *)
let iter_helper_sym (base : string) (descr : string) (iter : iter)
    (parts : string list) : string =
  String.concat "_" (base :: abbrev (R.sanitize descr) :: iter_tag iter :: parts)

(* The co-iterated variables' ids, in iterexp order -- these name the list-level
   values at a use site and the destructured spines in a definition. *)
let iter_var_ids (vars : var list) : string list =
  List.map (fun ({ varid; _ } : var) -> varid.it) vars

(* Variables an iterated body/premise captures from the enclosing scope: free
   variables that are not themselves co-iterated. Sorted (IdSet order) so the
   call site and the definition agree on the leading argument order. *)
let captured_fvs (free : IdSet.t) (vars : var list) : string list =
  let iters =
    List.fold_left
      (fun s ({ varid; _ } : var) -> IdSet.add varid s)
      IdSet.empty vars
  in
  IdSet.elements free
  |> List.filter (fun id -> not (IdSet.mem id iters))
  |> List.map (fun id -> id.it)

(* Fresh per-step element / rest variable names for an iteration var. Suffixes
   are unlikely to clash with spec identifiers or the captured names. *)
let step_hd (id : string) : string = id ^ "__hd"
let step_tl (id : string) : string = id ^ "__tl"

(* The per-step substitution renaming each co-iterated variable to its fresh head
   variable, applied (via [subst_term]) to a helper's translated body. *)
let elem_renaming (ids : string list) : (string * R.term) list =
  List.map (fun id -> (id, var_t (step_hd id))) ids

(* The same per-step renaming, but at the IL level and capture-aware: a
   structured nested [IterE]/[IterPr] that re-binds one of [ids] carries its own
   iteration over the full stream, so the per-step element must not be pushed
   inside it. A bare iterated variable `x*`/`x?` is the element-list itself, so
   it is renamed like a plain occurrence. Used in place of [elem_renaming] +
   [subst_term] where the body may contain such a re-binding. *)
let rec rename_step_exp (ids : string list) (e : exp) : exp =
  match e.it with
  | VarE id when List.mem id.it ids ->
      { e with it = VarE { id with it = step_hd id.it } }
  | IterE (({ it = VarE _; _ } as v), ie) ->
      { e with it = IterE (rename_step_exp ids v, ie) }
  | IterE (body, ((_, vars) as ie)) ->
      let inner_ids =
        List.filter
          (fun x ->
            not (List.exists (fun ({ varid; _ } : var) -> varid.it = x) vars))
          ids
      in
      { e with it = IterE (rename_step_exp inner_ids body, ie) }
  | _ -> { e with it = Exp_map.map_subexps (rename_step_exp ids) e.it }

let rec rename_step_prem (ids : string list) (prem : prem) : prem =
  let s = rename_step_exp ids in
  let it =
    match prem.it with
    | RelPr { relid; notexp } -> RelPr { relid; notexp = Mixfix.map s notexp }
    | IfPr { cond; role } -> IfPr { cond = s cond; role }
    | RelAssertPr { call = { relid; notexp }; expect } ->
        RelAssertPr { call = { relid; notexp = Mixfix.map s notexp }; expect }
    | LetPr (l, r) -> LetPr (s l, s r)
    | IterPr (nested, ((_, vars) as ie)) ->
        let inner_ids =
          List.filter
            (fun x ->
              not (List.exists (fun ({ varid; _ } : var) -> varid.it = x) vars))
            ids
        in
        IterPr (rename_step_prem inner_ids nested, ie)
    | DebugPr e -> DebugPr (s e)
    | ElsePr -> ElsePr
  in
  { prem with it }

(* The variables a helper for [IterPr (inner, (_, vars))] receives as captured
   constants: [inner]'s free non-co-iterated variables ([captured_fvs]) plus any
   iteration-guiding [bound_ids] that [inner] also uses at full-stream depth (one
   re-bound by a structured nested iteration). Only [bound_ids] (the inputs)
   qualify -- a binding (output) is produced, never read as a full stream. *)
let iter_captured (inner : prem) (vars : var list) (bound_ids : string list) :
    string list =
  let base = captured_fvs (Free.free_prem inner) vars in
  let stepped_free = Free.free_prem (rename_step_prem bound_ids inner) in
  let rebound =
    List.filter (fun id -> IdSet.mem (id $ no_region) stepped_free) bound_ids
  in
  base @ rebound

(* The expression analog of [iter_captured]: an [IterE]'s "map" helper
   ([iter_map_def]) receives as leading captured constants its body's free
   non-co-iterated variables plus any co-iterated [ids] the body still uses at
   full-stream depth (one re-bound by a structured nested [IterE]). *)
let iter_captured_exp (body : exp) (vars : var list) (ids : string list) :
    string list =
  let base = captured_fvs (Free.free_exp body) vars in
  let stepped_free = Free.free_exp (rename_step_exp ids body) in
  let rebound =
    List.filter (fun id -> IdSet.mem (id $ no_region) stepped_free) ids
  in
  base @ rebound

(* Argument lists for a helper recursing over co-iterated [cons]/[nil] (or
   [some]/[none]) spines, with the captured variables [fv_terms] leading
   unchanged: [base empty] fills each spine with its base constructor; [step wrap]
   destructures each into a fresh head/tail via [wrap]; [rec_args] passes the
   tails to the recursive call. *)
let spine_args (fv_terms : R.term list) (ids : string list) :
    (R.term -> R.term list)
    * ((string -> string -> R.term) -> R.term list)
    * R.term list =
  let base empty = fv_terms @ List.map (fun _ -> empty) ids in
  let step wrap =
    fv_terms @ List.map (fun id -> wrap (step_hd id) (step_tl id)) ids
  in
  let rec_args = fv_terms @ List.map (fun id -> var_t (step_tl id)) ids in
  (base, step, rec_args)

(* The auxiliary symbol for an [IterE] (a "map" over the co-iterated lists). The
   element type joins the descriptor so two iterations whose bodies print the
   same but build different element constructors do not collapse to one helper. *)
let iter_map_sym (body : exp) (iter : iter) (n_lists : int) : string =
  let elem_typ = Option.value (typ_name_of body.note) ~default:"" in
  iter_helper_sym "$itermap" (Print.string_of_exp body) iter
    [ string_of_int n_lists; elem_typ ]

(* -------------------------------------------------------------------------- *)
(* Subtype predicate. [SubE] (`e <: T`) is a boolean term that dispatches on the
   target type and recurses. Scalars decide directly; a named type defers to its
   [subty_<T>] helper ([defs_of_typ]); tuples and iterations defer to structural
   [subty_tup]/[subty_list]/[subty_opt] helpers ([sub_helper_defs]). The
   predicate is TOTAL over each use site's subject type: [defs_of_typ] decides
   the members, and [sub_helper_defs]' usage-based false-completion decides the
   non-members, mirroring the interpreter's total-boolean [subtyp]
   (interp/eval_il/interp.ml) -- so negated uses reduce instead of getting
   stuck. *)

(* A structural-subtype helper symbol: the shape tag plus the type's
   pretty-printed descriptor, bounded the same way as [iter_helper_sym]'s. *)
let subty_helper_sym (shape : string) (t : typ') : string =
  Printf.sprintf "subty_%s_%s" shape
    (abbrev (R.sanitize (Print.string_of_typ (t $ no_region))))

let subty_tup_sym (ts : typ list) : string = subty_helper_sym "tup" (TupleT ts)
let subty_list_sym (elem : typ') : string = subty_helper_sym "list" elem
let subty_opt_sym (elem : typ') : string = subty_helper_sym "opt" elem

let sub_pred ~scalars (t : typ') (x : R.term) : R.term =
  match t with
  | NumT `NatT -> app_t "sub_nat" [ x ]
  (* int, bool, text, func: the static type already guarantees membership.
     This matches the interpreter's [subtyp] exactly: int accepts every
     number, bool/text accept their own sort, and only nat needs a runtime
     test (a negative int is not a nat -- [sub_nat]). Undefined type
     parameters (the [VarT]-without-[TypD] branch of [sub_helper_defs]) are
     trivially true there too. *)
  | NumT _ | BoolT | TextT | FuncT -> bool_t ~scalars true
  | VarT { synid; _ } -> app_t (subty_sym synid.it) [ x ]
  | TupleT ts -> app_t (subty_tup_sym ts) [ x ]
  | IterT { typ = elem; iter = List } -> app_t (subty_list_sym elem.it) [ x ]
  | IterT { typ = elem; iter = Opt } -> app_t (subty_opt_sym elem.it) [ x ]

(* -------------------------------------------------------------------------- *)
(* Expressions -> terms. Placed ahead of the type pass, which reuses it; it has
   no dependency on the type pass itself. *)

let rec term_of_exp ~scalars (e : exp) : R.term =
  let recur = term_of_exp ~scalars in
  match e.it with
  | VarE id -> var_t id.it
  | BoolE b -> bool_t ~scalars b
  | NumE n -> term_of_num ~scalars n
  | TextE s -> text_t ~scalars s
  | UnE (op, ty, e1) ->
      term_of_unop op ty ~operand_is_int:(yields_int e1) (recur e1)
  | BinE (op, ty, e1, e2) -> term_of_binop op ty (recur e1) (recur e2)
  | CmpE (op, ty, e1, e2) -> term_of_cmpop op ty (recur e1) (recur e2)
  (* Casts are transparent except across the nat/int boundary: a nat widened to
     int is injected with [int_pos], an int narrowed to a known-nonneg nat is
     projected with [nat_of_int]. *)
  | UpCastE (t, e1)
    when is_int_typ t.it && is_nat_typ e1.note && not (yields_int e1) ->
      int_pos_t (recur e1)
  | DownCastE (t, e1) when is_nat_typ t.it && is_int_typ e1.note ->
      nat_of_int_t (recur e1)
  | UpCastE (_, e1) | DownCastE (_, e1) -> recur e1
  (* A `<:` in value position is the boolean test itself, unlike the top-level
     premise form that [conds_of_prem] turns into a [== true] guard. *)
  | SubE (e1, t) -> sub_pred ~scalars t.it (recur e1)
  | MatchE (e1, _) -> recur e1
  | TupleE es -> tuple_t (List.map recur es)
  | CaseE ne ->
      let args = List.map recur (Mixfix.args ne) in
      let mixop = Mixfix.to_mixop ne in
      let origin = Option.value (typ_name_of e.note) ~default:"anon" in
      variant_t origin mixop args
  | StrE fields ->
      let terms = List.map (fun (_, ef) -> recur ef) fields in
      let typ_name = Option.value (typ_name_of e.note) ~default:"anon" in
      struct_t typ_name terms
  | OptE None -> none_t
  | OptE (Some e1) -> some_t (recur e1)
  | ListE es -> List.fold_right (fun e acc -> cons_t (recur e) acc) es nil_t
  | ConsE (h, t) -> cons_t (recur h) (recur t)
  | CatE (a, b) -> cat_t (recur a) (recur b)
  | LenE e1 -> len_t (recur e1)
  | DotE (e1, a) ->
      let typ_name = Option.value (typ_name_of e1.note) ~default:"anon" in
      app_t (field_sym typ_name a) [ recur e1 ]
  | CallE (id, _, args) ->
      app_t (func_sym id) (List.filter_map (term_of_arg ~scalars) args)
  (* List/text operations over the [cons]/[nil] encoding, backed by the prelude
     rules and, for [Upd], the statically compiled path ([upd_of_path]).
     Out-of-bounds access is left irreducible. *)
  | MemE (a, b) -> mem_t (recur a) (recur b)
  | IdxE (a, b) -> idx_t (recur a) (recur b)
  | SliceE (a, b, c) -> slice_t (recur a) (recur b) (recur c)
  | UpdE (a, path, b) -> upd_of_path ~scalars (recur a) path (recur b)
  (* A bare iterated variable [x*]/[x?] is the list/option [x] itself. *)
  | IterE ({ it = VarE id; _ }, _) -> var_t id.it
  (* A structured iterated body compiles to a call to its "map" helper, applied
     to the captured variables then the co-iterated lists (see [iter_map_def]). *)
  | IterE (body, (iter, vars)) ->
      let ids = iter_var_ids vars in
      let fvs = iter_captured_exp body vars ids in
      app_t
        (iter_map_sym body iter (List.length ids))
        (List.map var_t (fvs @ ids))

and term_of_arg ~scalars (a : arg) : R.term option =
  match a.it with ExpA e -> Some (term_of_exp ~scalars e) | DefA _ -> None

(* [Upd]'s path is compiled statically: [access_of_path] reads the sub-term
   reached by a path, [upd_of_path] rebuilds the term with the leaf replaced by
   [v], from the inside out. *)
and access_of_path ~scalars (base : R.term) (path : path) : R.term =
  let recur = access_of_path ~scalars base in
  match path.it with
  | RootP -> base
  | IdxP (p, i) -> idx_t (recur p) (term_of_exp ~scalars i)
  | SliceP (p, i, n) ->
      slice_t (recur p) (term_of_exp ~scalars i) (term_of_exp ~scalars n)
  | DotP (p, a) ->
      let typ_name = Option.value (typ_name_of p.note) ~default:"anon" in
      app_t (field_sym typ_name a) [ recur p ]

and upd_of_path ~scalars (base : R.term) (path : path) (v : R.term) : R.term =
  let access = access_of_path ~scalars base in
  match path.it with
  | RootP -> v
  | IdxP (p, i) ->
      upd_of_path ~scalars base p
        (upd_idx_t (access p) (term_of_exp ~scalars i) v)
  | SliceP (p, i, n) ->
      upd_of_path ~scalars base p
        (upd_slice_t (access p) (term_of_exp ~scalars i)
           (term_of_exp ~scalars n) v)
  | DotP (p, a) ->
      let typ_name = Option.value (typ_name_of p.note) ~default:"anon" in
      upd_of_path ~scalars base p
        (app_t (upd_field_sym typ_name a) [ access p; v ])

(* -------------------------------------------------------------------------- *)
(* Prelude + type-derived definition rules. *)

(* Defining rules for an [IterE]'s "map" helper: recurse over the co-iterated
   spines in lock-step, rebuilding the collection from the body evaluated at each
   element. [List] folds with [cons]/[nil], [Opt] with [some]/[none]. Returns the
   helper symbol with its rules, or [None] for a bare iterated variable. *)
let iter_map_def ~scalars (e : exp) : (string * R.rule list) option =
  match e.it with
  | IterE ({ it = VarE _; _ }, _) -> None
  | IterE (body, (iter, vars)) ->
      let ids = iter_var_ids vars in
      let fvs = iter_captured_exp body vars ids in
      let sym = iter_map_sym body iter (List.length ids) in
      let fv_terms = List.map var_t fvs in
      let body_elem = term_of_exp ~scalars (rename_step_exp ids body) in
      let base_args, step_args, rec_args = spine_args fv_terms ids in
      let rules =
        match iter with
        | List ->
            [
              rule (app_t sym (base_args nil_t)) nil_t;
              rule
                (app_t sym (step_args (fun h t -> cons_t (var_t h) (var_t t))))
                (cons_t body_elem (app_t sym rec_args));
            ]
        | Opt ->
            [
              rule (app_t sym (base_args none_t)) none_t;
              rule
                (app_t sym (step_args (fun h _ -> some_t (var_t h))))
                (some_t body_elem);
            ]
      in
      Some (sym, rules)
  | _ -> None

(* The auxiliary symbol that projects one co-iterated variable out of an
   iterated body in binder position (the inverse of one [iter_map_def] column).
*)
let unzip_sym (body : exp) (iter : iter) (v : string) : string =
  iter_helper_sym "$unzip" (Print.string_of_exp body) iter [ R.sanitize v ]

(* Defining rules for a single-spine helper [sym]: recurse over a [List]/[Opt]
   spine, matching each element against [elem_pat] and returning the element's
   [v] component ([var_t (step_hd v)], one of [elem_pat]'s pattern variables).
   [fv_terms] are captured constants threaded unchanged as leading arguments
   ([] when there are none). The spine's tail is always the fixed variable
   [__rest] -- there is exactly one spine here, unlike [spine_args]'s N-way
   [step_tl id] naming.

   Shared by [iter_unzip_defs] below ([$unzip]: [elem_pat] is the arbitrary
   translated iterated body -- possibly non-left-linear, when it re-mentions a
   captured [fv_terms] variable) and [iterpr_defs]'s [proj_defs] far below
   ([$iterproj]: [elem_pat] is always a bare fresh-variable tuple over an
   already-materialized stream, [fv_terms] always [] -- always irrefutable). *)
let spine_projection_rules (sym : string) (fv_terms : R.term list) (iter : iter)
    (elem_pat : R.term) (v : string) : R.rule list =
  let collected = var_t (step_hd v) in
  let rest = var_t "__rest" in
  match iter with
  | List ->
      [
        rule (app_t sym (fv_terms @ [ nil_t ])) nil_t;
        rule
          (app_t sym (fv_terms @ [ cons_t elem_pat rest ]))
          (cons_t collected (app_t sym (fv_terms @ [ rest ])));
      ]
  | Opt ->
      [
        rule (app_t sym (fv_terms @ [ none_t ])) none_t;
        rule (app_t sym (fv_terms @ [ some_t elem_pat ])) (some_t collected);
      ]

(* Defining rules for the [unzip] helpers of an [IterE] used in binder position
   (a clause/rule head pattern): one helper per co-iterated variable, recursing
   over the list and matching each element against the iterated body to project
   that variable. The iterated collection binds to a fresh variable (see
   [pattern_of_exp]) and these helpers recover the element streams the body would
   have bound. The captured variables are carried as leading parameters and
   matched in each element (a non-left-linear pattern). *)
let iter_unzip_defs ~scalars (e : exp) : (string * R.rule list) list =
  match e.it with
  | IterE ({ it = VarE _; _ }, _) -> []
  | IterE (body, (iter, vars)) ->
      let fv_terms = List.map var_t (captured_fvs (Free.free_exp body) vars) in
      let ids = iter_var_ids vars in
      let elem_pat =
        subst_term (elem_renaming ids) (term_of_exp ~scalars body)
      in
      List.map
        (fun v ->
          let sym = unzip_sym body iter v in
          (sym, spine_projection_rules sym fv_terms iter elem_pat v))
        ids
  | _ -> []

(* A variant case as seen from a containing type: its origin (the type that
   actually defines it), its mixop, and its arity. *)
type case_info = { origin : string; mixop : mixop; arity : int }

let case_info_of_typcase (tc : typcase) : case_info =
  let { synid = origin_id; _ } = tc.origin.it in
  {
    origin = origin_id.it;
    mixop = Mixfix.to_mixop tc.notation.it;
    arity = Mixfix.arity tc.notation.it;
  }

(* The constructor of a one-case variant type [name] in [spec]: a builder that
   wraps its arguments in that case's [variant_<origin>_<mixop>] symbol. [None]
   when [name] is undefined or is not a single-case variant. *)
let single_case_ctor (spec : spec) (name : string) :
    (R.term list -> R.term) option =
  List.find_map
    (fun (def : def) ->
      match def.it with
      | TypD { synid = tid; deftyp = { it = VariantT [ typcase ]; _ }; _ }
        when tid.it = name ->
          let ci = case_info_of_typcase typcase in
          Some (fun args -> variant_t ci.origin ci.mixop args)
      | _ -> None)
    spec

(* The constructor of the case of (multi-case) variant type [name] whose
   generated symbol ([variant_sym]) is [case_sym]. [None] when [name] is
   undefined or has no such case. *)
let case_ctor (spec : spec) (name : string) (case_sym : string) :
    (R.term list -> R.term) option =
  List.find_map
    (fun (def : def) ->
      match def.it with
      | TypD { synid = tid; deftyp = { it = VariantT typcases; _ }; _ }
        when tid.it = name ->
          List.find_map
            (fun typcase ->
              let ci = case_info_of_typcase typcase in
              if variant_sym ci.origin ci.mixop = case_sym then
                Some (fun args -> variant_t ci.origin ci.mixop args)
              else None)
            typcases
      | _ -> None)
    spec

(* The typcases of variant type [name] in [spec]; [None] when [name] is
   undefined or not a variant. *)
let typcases_of (spec : spec) (name : string) : typcase list option =
  List.find_map
    (fun (d : def) ->
      match d.it with
      | TypD { synid; deftyp = { it = VariantT tcs; _ }; _ }
        when synid.it = name ->
          Some tcs
      | _ -> None)
    spec

(* Unwrap plain-alias chains ([syntax T = U]) down to the underlying type. *)
let rec unalias (spec : spec) (t : typ') : typ' =
  match t with
  | VarT { synid = tid; _ } -> (
      match
        List.find_map
          (fun (d : def) ->
            match d.it with
            | TypD { synid; deftyp = { it = PlainT u; _ }; _ }
              when synid.it = tid.it ->
                Some u.it
            | _ -> None)
          spec
      with
      | Some u -> unalias spec u
      | None -> t)
  | _ -> t

(* Definition rules contributed by one [TypD]. For a variant type [T]:
   - matcher: [match_<T>_<Ci>] is [true] on [Ci]'s constructor, [false] on every
     sibling;
   - subtype: [subty_<T>] recurses into each case's payload (positive use only,
     non-members irreducible);
   - equality: [eq] on two same-case constructors recurses into their fields, on
     two different cases is [false].
   For a struct type [T]: a field accessor per field, structural [eq], and a
   trivially-true [subty_<T>] (structs are invariant in SpecTec). For a plain
   alias [T = U]: [subty_<T>] delegates to [U]'s check. *)
let defs_of_typ ~scalars (def : def) : R.rule list =
  match def.it with
  | TypD { synid = tid; deftyp = { it = VariantT typcases; _ }; _ } ->
      let t = tid.it in
      let cases = List.map case_info_of_typcase typcases in
      let con ?(prefix = "x") ci =
        variant_t ci.origin ci.mixop (fresh_vars ~prefix ci.arity)
      in
      (* One rule per ordered case pair (the diagonal carries the "same case"
         meaning); shared by the matcher and equality rules below. *)
      let per_pair f =
        List.concat
          (List.mapi
             (fun i ci -> List.mapi (fun j cj -> f i ci j cj) cases)
             cases)
      in
      (* For each case [ci]: its matcher answers [true] on [ci]'s own constructor
         and [false] on every sibling -- total and overlap-free over [t]'s cases. *)
      let matcher_rules =
        per_pair (fun i ci j cj ->
            rule
              (app_t (match_sym t ci.mixop) [ con cj ])
              (bool_t ~scalars (i = j)))
      in
      (* subtype: recurse into each case's payload (its declared field types). *)
      let subty_rules =
        List.map
          (fun (typcase : typcase) ->
            let nottyp = typcase.notation in
            let ci = case_info_of_typcase typcase in
            let xs = fresh_vars ci.arity in
            let field_typs = Mixfix.args nottyp.it in
            rule
              (app_t (subty_sym t) [ variant_t ci.origin ci.mixop xs ])
              (conj_t ~scalars
                 (List.map2
                    (fun ft x -> sub_pred ~scalars ft.it x)
                    field_typs xs)))
          typcases
      in
      let eq_rules =
        per_pair (fun i ci j cj ->
            if i = j then
              let xs = fresh_vars ~prefix:"x" ci.arity in
              let ys = fresh_vars ~prefix:"y" ci.arity in
              rule
                (eq_t
                   (variant_t ci.origin ci.mixop xs)
                   (variant_t ci.origin ci.mixop ys))
                (conj_t ~scalars (List.map2 eq_t xs ys))
            else
              rule (eq_t (con ci) (con ~prefix:"y" cj)) (bool_t ~scalars false))
      in
      matcher_rules @ subty_rules @ eq_rules
  | TypD { synid = tid; deftyp = { it = StructT fields; _ }; _ } ->
      let t = tid.it in
      let n = List.length fields in
      (* accessor reads field [a] out of the struct literal *)
      let accessor_rules =
        List.mapi
          (fun i (a, _) ->
            rule
              (app_t (field_sym t a) [ app_t (struct_sym t) (fresh_vars n) ])
              (var_t (Printf.sprintf "x%d" i)))
          fields
      in
      (* updater rebuilds the struct literal with field [a] replaced by [v] *)
      let updater_rules =
        List.mapi
          (fun i (a, _) ->
            let fields_t = fresh_vars n in
            let updated =
              List.mapi (fun j xj -> if j = i then var_t "v" else xj) fields_t
            in
            rule
              (app_t (upd_field_sym t a)
                 [ app_t (struct_sym t) fields_t; var_t "v" ])
              (app_t (struct_sym t) updated))
          fields
      in
      let xs = fresh_vars ~prefix:"x" n and ys = fresh_vars ~prefix:"y" n in
      let eq_rule =
        rule
          (eq_t (app_t (struct_sym t) xs) (app_t (struct_sym t) ys))
          (conj_t ~scalars (List.map2 eq_t xs ys))
      in
      (* Structs are invariant in SpecTec, so the membership check is trivially
         true -- the interpreter's [subtyp] has no struct case at all (its
         catch-all answers [true]; see the invariance note in
         interp/eval_il/interp.ml) because the elaborator guarantees a
         struct-typed subject already has exactly this type. No width/depth
         checking is intended. We keep the LHS keyed on [struct_<t>] rather
         than a bare variable: a well-typed subject always matches, and an
         ill-typed one (a translation bug) stays visibly stuck instead of
         being absorbed to [true]. *)
      let subty_rule =
        rule
          (app_t (subty_sym t) [ app_t (struct_sym t) (fresh_vars n) ])
          (bool_t ~scalars true)
      in
      accessor_rules @ updater_rules @ [ eq_rule; subty_rule ]
  (* A plain alias [syntax T = U]: its subtype check is [U]'s. *)
  | TypD { synid = tid; deftyp = { it = PlainT u; _ }; _ } ->
      [
        rule
          (app_t (subty_sym tid.it) [ var_t "x" ])
          (sub_pred ~scalars u.it (var_t "x"));
      ]
  | _ -> []

(* -------------------------------------------------------------------------- *)
(* Relation invocation: split a relation's notexp arguments into inputs/output
   using the relation's declared input positions. *)

let split_inputs (inputs : int list) (args : 'a list) : 'a list * 'a list =
  let ins, outs =
    List.mapi (fun i a -> (i, a)) args
    |> List.partition (fun (i, _) -> List.mem i inputs)
  in
  (List.map snd ins, List.map snd outs)

let output_term ~scalars (outs : R.term list) : R.term =
  match outs with [] -> bool_t ~scalars true | [ t ] -> t | ts -> tuple_t ts

(* A relation's notation type and input-position indices, by name. Used to split
   a relation's arguments into inputs and outputs. *)
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

(* [Rel: id : notexp] as an invocation condition [Rel(inputs) == output]. *)
let rel_invocation ~scalars (orig : spec) (id : id) (ne : notexp) : R.cond =
  let args = Mixfix.args ne in
  let in_args, out_args =
    match find_rel_in_spec orig id.it with
    | Some (_, inputs) -> split_inputs inputs args
    | None -> (args, [])
  in
  let in_terms = List.map (term_of_exp ~scalars) in_args in
  let out_terms = List.map (term_of_exp ~scalars) out_args in
  (app_t (rel_sym id) in_terms, output_term ~scalars out_terms)

(* The iteration variables a premise binds (its output positions): a relation's
   non-input arguments, or a [let]'s left-hand side. A bound iteration variable
   guides the recursion; a binding one is collected from each step's output. *)
let rec prem_binder_names (orig : spec) (prem : prem) : string list =
  let names_of es =
    List.concat_map
      (fun e -> List.map (fun id -> id.it) (IdSet.elements (Free.free_exp e)))
      es
  in
  match prem.it with
  | LetPr (l, _) -> names_of [ l ]
  | RelPr { relid = id; notexp = ne }
  | RelAssertPr { call = { relid = id; notexp = ne }; expect = true } ->
      let args = Mixfix.args ne in
      let _, out_args =
        match find_rel_in_spec orig id.it with
        | Some (_, inputs) -> split_inputs inputs args
        | None -> (args, [])
      in
      names_of out_args
  | IterPr (inner, _) -> prem_binder_names orig inner
  | IfPr _ | RelAssertPr { expect = false; _ } | ElsePr | DebugPr _ -> []

(* Split an iterated premise's variables into the (bound, binding) ids: bound
   guide the iteration, binding are produced and collected per step. A co-iterated
   binder that no longer occurs anywhere in [inner] is stale ([Simplify]
   substituted it away) and is dropped, so it is not classified as a bound spine
   the call site never supplies. *)
let iter_split (orig : spec) (inner : prem) (vars : var list) :
    string list * string list =
  let binders = prem_binder_names orig inner in
  let free = Free.free_prem inner in
  let live id = IdSet.mem (id $ no_region) free in
  iter_var_ids vars |> List.filter live
  |> List.partition (fun id -> not (List.mem id binders))

(* Auxiliary symbols for an [IterPr]: a [$iterall] predicate (every step holds)
   when nothing is collected, or one [$itercollect] per binding variable. *)
let iter_all_sym (inner : prem) (iter : iter) (n_bound : int) : string =
  iter_helper_sym "$iterall"
    (Print.string_of_prem inner)
    iter
    [ string_of_int n_bound ]

let iter_collect_sym (inner : prem) (iter : iter) (n_bound : int) (b : string) :
    string =
  iter_helper_sym "$itercollect"
    (Print.string_of_prem inner)
    iter
    [ string_of_int n_bound; R.sanitize b ]

(* When the iterated premise is a single relation call whose outputs are exactly
   the collected variables, the iteration is a plain "map" carrying the call
   result as the element. [$iterapply] returns that output stream; [$iterproj_b]
   projects one component when the call has several outputs. *)
let iter_apply_sym (inner : prem) (iter : iter) (n_bound : int) : string =
  iter_helper_sym "$iterapply"
    (Print.string_of_prem inner)
    iter
    [ string_of_int n_bound ]

let iter_proj_sym (inner : prem) (iter : iter) (n_bound : int) (b : string) :
    string =
  iter_helper_sym "$iterproj"
    (Print.string_of_prem inner)
    iter
    [ string_of_int n_bound; R.sanitize b ]

(* [Some (call, out_vars)] when [inner] is a single relation call whose output
   positions are exactly the collected [binding_ids] as bare variables.
   [out_vars] is the output variable order (one for a single output, the tuple
   component order for several). [None] for any other premise. *)
let iter_call_map ~scalars (orig : spec) (inner : prem)
    (binding_ids : string list) : (R.term * string list) option =
  match inner.it with
  | RelPr { relid = id; notexp = ne }
  | RelAssertPr { call = { relid = id; notexp = ne }; expect = true } ->
      let call, out_pat = rel_invocation ~scalars orig id ne in
      let components =
        match out_pat with R.App ("tuple", ts) -> ts | t -> [ t ]
      in
      let var_of = function R.Var v -> Some v | _ -> None in
      let out_vars = List.filter_map var_of components in
      if
        List.length out_vars = List.length components
        && List.sort compare out_vars = List.sort compare binding_ids
      then Some (call, out_vars)
      else None
  | _ -> None

(* -------------------------------------------------------------------------- *)
(* Premises -> conditions. *)

(* A source of rule-unique binder names (for the collection variables that an
   iterated pattern unzips through). One generator is threaded across a rule's
   head pattern and all its premises so the names never clash. *)
let fresh_binder () : unit -> string =
  let n = ref 0 in
  fun () ->
    let i = !n in
    incr n;
    Printf.sprintf "iterbind_%d" i

(* Translate a head (binder-position) expression into a left-hand-side pattern.
   Identical to [term_of_exp] for structural constructors, but a structured
   iterated body cannot stand as a pattern (a CTRS LHS is constructor-only), so
   it binds a fresh list variable and yields [unzip] conditions recovering the
   element streams (see [iter_unzip_defs]). [fresh] supplies rule-unique names. *)
let rec pattern_of_exp ~scalars (fresh : unit -> string) (e : exp) :
    R.term * R.cond list =
  let many es =
    let pairs = List.map (pattern_of_exp ~scalars fresh) es in
    (List.map fst pairs, List.concat_map snd pairs)
  in
  match e.it with
  | IterE ({ it = VarE id; _ }, _) -> (var_t id.it, [])
  | IterE (body, (iter, vars)) ->
      let t = fresh () in
      let fv_terms = List.map var_t (captured_fvs (Free.free_exp body) vars) in
      let conds =
        List.map
          (fun v ->
            (app_t (unzip_sym body iter v) (fv_terms @ [ var_t t ]), var_t v))
          (iter_var_ids vars)
      in
      (var_t t, conds)
  | TupleE es ->
      let ts, cs = many es in
      (tuple_t ts, cs)
  | ListE es ->
      let ts, cs = many es in
      (List.fold_right cons_t ts nil_t, cs)
  | ConsE (h, t) ->
      let th, ch = pattern_of_exp ~scalars fresh h in
      let tt, ct = pattern_of_exp ~scalars fresh t in
      (cons_t th tt, ch @ ct)
  | CaseE ne ->
      let ts, cs = many (Mixfix.args ne) in
      let origin = Option.value (typ_name_of e.note) ~default:"anon" in
      (variant_t origin (Mixfix.to_mixop ne) ts, cs)
  | StrE fields ->
      let ts, cs = many (List.map snd fields) in
      let typ_name = Option.value (typ_name_of e.note) ~default:"anon" in
      (struct_t typ_name ts, cs)
  | OptE (Some e1) ->
      let t, c = pattern_of_exp ~scalars fresh e1 in
      (some_t t, c)
  (* Non-structural (or capture-carrying iterated) heads stay as plain terms. *)
  | _ -> (term_of_exp ~scalars e, [])

let rec conds_of_prem ~scalars (orig : spec) (fresh : unit -> string)
    (prem : prem) : R.cond list =
  let term = term_of_exp ~scalars in
  match prem.it with
  (* The let's pattern is a binder position: compile it as a pattern so an
     iterated destructuring unzips into the element streams, rather than
     re-zipping them with [$itermap] (a function Maude cannot run backwards). *)
  | LetPr (lhs, rhs) ->
      let pat, conds = pattern_of_exp ~scalars fresh lhs in
      (term rhs, pat) :: conds
  | IfPr { cond = { it = CmpE (`EqOp, _, a, b); _ }; _ } -> [ (term a, term b) ]
  | IfPr { cond = { it = CmpE (`NeOp, _, a, b); _ }; _ } ->
      [ (eq_t (term a) (term b), bool_t ~scalars false) ]
  | IfPr { cond = { it = MatchE (e, pattern); _ }; _ } ->
      [ cond_of_match ~scalars e pattern ]
  | IfPr { cond = { it = SubE (e, t); _ }; _ } ->
      [ (sub_pred ~scalars t.it (term e), bool_t ~scalars true) ]
  | IfPr { cond; _ } -> [ (term cond, bool_t ~scalars true) ]
  | RelPr { relid = id; notexp = ne }
  | RelAssertPr { call = { relid = id; notexp = ne }; expect = true } ->
      [ rel_invocation ~scalars orig id ne ]
  | RelAssertPr { call = { relid = id; notexp = ne }; expect = false } ->
      let lhs, _ = rel_invocation ~scalars orig id ne in
      [ (lhs, bool_t ~scalars false) ]
  (* An iterated premise becomes a call to its auxiliary helper (see
     [iterpr_defs]): a [$iterall] check when it collects nothing; an [$iterapply]
     map (or [$iterproj_b] per output) when it iterates a single relation call;
     else an [$itercollect_b] condition per collected list [b]. *)
  | IterPr (inner, (iter, vars)) -> (
      let bound_ids, binding_ids = iter_split orig inner vars in
      let fvs = iter_captured inner vars bound_ids in
      let args = List.map var_t (fvs @ bound_ids) in
      let n = List.length bound_ids in
      if binding_ids = [] then
        [ (app_t (iter_all_sym inner iter n) args, bool_t ~scalars true) ]
      else
        match iter_call_map ~scalars orig inner binding_ids with
        (* A single relation call: [$iterapply] returns the output stream
           directly (a single output binds it; several are projected out). *)
        | Some (_, [ b ]) ->
            [ (app_t (iter_apply_sym inner iter n) args, var_t b) ]
        | Some (_, _) ->
            let apply = app_t (iter_apply_sym inner iter n) args in
            List.map
              (fun b ->
                (app_t (iter_proj_sym inner iter n b) [ apply ], var_t b))
              binding_ids
        (* Otherwise collect each output with a per-step conditional helper. *)
        | None ->
            List.map
              (fun b -> (app_t (iter_collect_sym inner iter n b) args, var_t b))
              binding_ids)
  | ElsePr | DebugPr _ -> []

and cond_of_match ~scalars (e : exp) (pattern : pattern) : R.cond =
  let subj = term_of_exp ~scalars e in
  let yes = bool_t ~scalars true in
  match pattern with
  | CaseP mixop ->
      let name = Option.value (typ_name_of e.note) ~default:"anon" in
      (app_t (match_sym name mixop) [ subj ], yes)
  | OptP `Some -> (app_t "match_some" [ subj ], yes)
  | OptP `None -> (app_t "match_none" [ subj ], yes)
  | ListP `Cons -> (app_t "match_cons" [ subj ], yes)
  | ListP `Nil -> (app_t "match_nil" [ subj ], yes)
  | ListP (`Fixed n) -> (len_t subj, nat_lit ~scalars n)

let conds_of_prems ~scalars (orig : spec) (fresh : unit -> string)
    (prems : prem list) : R.cond list =
  List.concat_map (conds_of_prem ~scalars orig fresh) prems

(* Defining rules for an [IterPr]'s helper(s), the premise counterpart of
   [iter_map_def]: recurse over the bound (iteration-guiding) spines, requiring
   the inner premise's conditions at each step. A pure check ([$iterall]) reduces
   to [true] only when every step holds; a collecting helper ([$itercollect_b])
   rebuilds the list of each step's bound value [b]. *)
let iterpr_defs ~scalars (orig : spec) (prem : prem) :
    (string * R.rule list) list =
  match prem.it with
  | IterPr (inner, (iter, vars)) -> (
      let bound_ids, binding_ids = iter_split orig inner vars in
      let fv_terms = List.map var_t (iter_captured inner vars bound_ids) in
      let inner_stepped = rename_step_prem (bound_ids @ binding_ids) inner in
      let conds = conds_of_prem ~scalars orig (fresh_binder ()) inner_stepped in
      let base_args, step_args, rec_args = spine_args fv_terms bound_ids in
      let n = List.length bound_ids in
      let cons_step h t = cons_t (var_t h) (var_t t) in
      let some_step h _ = some_t (var_t h) in
      if binding_ids = [] then
        let sym = iter_all_sym inner iter n in
        let yes = bool_t ~scalars true in
        let rules =
          match iter with
          | List ->
              [
                rule (app_t sym (base_args nil_t)) yes;
                rule_cond
                  (app_t sym (step_args cons_step))
                  (app_t sym rec_args) conds;
              ]
          | Opt ->
              [
                rule (app_t sym (base_args none_t)) yes;
                rule_cond (app_t sym (step_args some_step)) yes conds;
              ]
        in
        [ (sym, rules) ]
      else
        match iter_call_map ~scalars orig inner binding_ids with
        (* A single relation call: an unconditional "map" carrying the call
           result as the element, plus a projection helper per output when the
           call has several (the stream is then a stream of tuples). *)
        | Some (call, out_vars) ->
            let apply = iter_apply_sym inner iter n in
            let elem = subst_term (elem_renaming bound_ids) call in
            let apply_rules =
              match iter with
              | List ->
                  [
                    rule (app_t apply (base_args nil_t)) nil_t;
                    rule
                      (app_t apply (step_args cons_step))
                      (cons_t elem (app_t apply rec_args));
                  ]
              | Opt ->
                  [
                    rule (app_t apply (base_args none_t)) none_t;
                    rule (app_t apply (step_args some_step)) (some_t elem);
                  ]
            in
            let proj_defs =
              if List.length out_vars <= 1 then []
              else
                let tuple_pat =
                  tuple_t (List.map (fun v -> var_t (step_hd v)) out_vars)
                in
                List.map
                  (fun v ->
                    let sym = iter_proj_sym inner iter n v in
                    (sym, spine_projection_rules sym [] iter tuple_pat v))
                  out_vars
            in
            (apply, apply_rules) :: proj_defs
        (* Otherwise collect each output with a per-step conditional helper. *)
        | None ->
            List.map
              (fun b ->
                let sym = iter_collect_sym inner iter n b in
                let collected = var_t (step_hd b) in
                let rules =
                  match iter with
                  | List ->
                      [
                        rule (app_t sym (base_args nil_t)) nil_t;
                        rule_cond
                          (app_t sym (step_args cons_step))
                          (cons_t collected (app_t sym rec_args))
                          conds;
                      ]
                  | Opt ->
                      [
                        rule (app_t sym (base_args none_t)) none_t;
                        rule_cond
                          (app_t sym (step_args some_step))
                          (some_t collected) conds;
                      ]
                in
                (sym, rules))
              binding_ids)
  | _ -> []

(* A set of named helper definitions accumulated during a spec walk: [add] is
   idempotent on the symbol (structurally identical iterations/types share one
   helper), [rules] flattens them. *)
module Helper_defs = struct
  type t = (string, R.rule list) Hashtbl.t

  let create (size : int) : t = Hashtbl.create size
  let mem (t : t) (sym : string) : bool = Hashtbl.mem t sym

  let add (t : t) (sym : string) (rules : R.rule list) : unit =
    if not (mem t sym) then Hashtbl.add t sym rules

  let rules (t : t) : R.rule list =
    Hashtbl.fold (fun _ rules acc -> rules @ acc) t []
end

(* Each function clause / relation rule of [def] as its (head and result
   expressions, premises); [] for non-body defs. The head expressions are a
   clause's [ExpA] argument expressions plus its result, or a relation rule's
   notexp arguments -- the positions a translation walk visits. *)
let blocks_of_def (def : def) : (exp list * prem list) list =
  match def.it with
  | DecD { clauses; _ } ->
      let arg_exp (a : arg) =
        match a.it with ExpA e -> Some e | DefA _ -> None
      in
      List.map
        (fun (c : clause) ->
          let { args; body; prems } = c.it in
          (List.filter_map arg_exp args @ [ body ], prems))
        clauses
  | RelD { rules; _ } ->
      List.map
        (fun (r : rule) ->
          let { concl; prems; _ } = r.it in
          (Mixfix.args concl, prems))
        rules
  | TypD _ | BuiltinDecD _ -> []

(* Collect every iteration helper definition reachable in the spec's bodies,
   deduplicated by symbol (structurally identical iterations share one helper).
   Descends into clause/rule heads, results, and premises, including nested
   iterations. *)
let iter_helper_defs ~scalars (orig : spec) (spec : spec) : R.rule list =
  let defs = Helper_defs.create 32 in
  let add (sym, rules) = Helper_defs.add defs sym rules in
  let rec visit_exp (e : exp) =
    (match iter_map_def ~scalars e with Some d -> add d | None -> ());
    List.iter add (iter_unzip_defs ~scalars e);
    List.iter visit_exp (Exp_map.subexps e.it)
  in
  let rec visit_prem (p : prem) =
    match p.it with
    | IterPr (inner, _) ->
        List.iter add (iterpr_defs ~scalars orig p);
        visit_prem inner
    | _ -> List.iter visit_exp (Exp_map.exps_of_prem p)
  in
  List.iter
    (fun (exps, prems) ->
      List.iter visit_exp exps;
      List.iter visit_prem prems)
    (List.concat_map blocks_of_def spec);
  Helper_defs.rules defs

(* The structural subtype helpers ([subty_tup]/[subty_list]/[subty_opt]) the
   spec needs, deduplicated by symbol and pruned later. Seeded from every [SubE]
   target and every type a [subty_<T>] definition recurses into (a variant case's
   field types and a plain alias's underlying type). The named [subty_<T>]
   helpers themselves come from [defs_of_typ]; scalars from the prelude. *)
let sub_helper_defs ~scalars (orig : spec) (simplified : spec) : R.rule list =
  let defs = Helper_defs.create 64 in
  let has_typdef name =
    List.exists
      (fun (d : def) ->
        match d.it with TypD { synid = tid; _ } -> tid.it = name | _ -> false)
      orig
  in
  (* Ensure the structural helper for target type [t] (and its inner types) is
     present. *)
  let rec require (t : typ') =
    match t with
    (* A [VarT] naming a type *parameter* (no [TypD]) is abstract: approximate
       the positive check as trivially true. Named types with a [TypD] are
       defined by [defs_of_typ] instead. *)
    | VarT { synid = tid; _ } ->
        let sym = subty_sym tid.it in
        if (not (Helper_defs.mem defs sym)) && not (has_typdef tid.it) then
          Helper_defs.add defs sym
            [ rule (app_t sym [ var_t "x" ]) (bool_t ~scalars true) ]
    | TupleT ts ->
        let sym = subty_tup_sym ts in
        if not (Helper_defs.mem defs sym) then (
          let xs = fresh_vars (List.length ts) in
          Helper_defs.add defs sym
            [
              rule
                (app_t sym [ tuple_t xs ])
                (conj_t ~scalars
                   (List.map2 (fun ft x -> sub_pred ~scalars ft.it x) ts xs));
            ];
          List.iter (fun ft -> require ft.it) ts)
    | IterT { typ = elem; iter = List } ->
        let sym = subty_list_sym elem.it in
        if not (Helper_defs.mem defs sym) then (
          let h = var_t "h" and t = var_t "t" in
          Helper_defs.add defs sym
            [
              rule (app_t sym [ nil_t ]) (bool_t ~scalars true);
              rule
                (app_t sym [ cons_t h t ])
                (and_t (sub_pred ~scalars elem.it h) (app_t sym [ t ]));
            ];
          require elem.it)
    | IterT { typ = elem; iter = Opt } ->
        let sym = subty_opt_sym elem.it in
        if not (Helper_defs.mem defs sym) then (
          let v = var_t "v" in
          Helper_defs.add defs sym
            [
              rule (app_t sym [ none_t ]) (bool_t ~scalars true);
              rule (app_t sym [ some_t v ]) (sub_pred ~scalars elem.it v);
            ];
          require elem.it)
    | _ -> ()
  in
  (* every type a [subty_<T>] definition recurses into (from [orig]'s types) *)
  List.iter
    (fun (def : def) ->
      match def.it with
      | TypD { deftyp = { it = VariantT typcases; _ }; _ } ->
          List.iter
            (fun (tc : typcase) ->
              List.iter
                (fun (ft : typ) -> require ft.it)
                (Mixfix.args tc.notation.it))
            typcases
      | TypD { deftyp = { it = PlainT u; _ }; _ } -> require u.it
      | _ -> ())
    orig;
  (* every [SubE] target: the `<:` guards (including under iteration) and any
     `<:` nested in value position *)
  let rec targets_of_exp (e : exp) =
    (match e.it with SubE (_, t) -> require t.it | _ -> ());
    List.iter targets_of_exp (Exp_map.subexps e.it)
  in
  let targets_of_prem (p : prem) =
    (* [exps_of_prem] sees through [IterPr], so iterated `<:` guards are
       reached too *)
    List.iter targets_of_exp (Exp_map.exps_of_prem p)
  in
  List.iter
    (fun (exps, prems) ->
      List.iter targets_of_exp exps;
      List.iter targets_of_prem prems)
    (List.concat_map blocks_of_def simplified);
  Helper_defs.rules defs

(* The usage-based false-completion that makes [subty_<T>] total. At every
   [SubE (e, T)] site the subject's static type [S = e.note] bounds the
   constructors that can reach [subty_<T>], so a [-> false] rule per case of
   [S] not in [T] decides every non-member; [defs_of_typ]'s member rules decide
   the rest, so the predicate reduces on the whole reachable domain -- the same
   total-boolean semantics as the interpreter's [subtyp]
   (interp/eval_il/interp.ml), which is what negated uses
   ([~(e <: T)] -> [not(subty_<T>(e))]) need to reduce.

   Case identity is origin + mixop + arity (the [variant_sym] keying), so the
   super-variant's constructors pattern-match directly. Aliases are unwrapped
   on both sides; tuple/list/option pairs co-descend into their element types
   (the structural helpers recurse elementwise, so the element predicate needs
   its complement too). Deduplicated per (predicate, constructor). *)
let sub_complement_defs ~scalars (orig : spec) (simplified : spec) : R.rule list
    =
  let seen = Hashtbl.create 64 in
  let rec complement (target : typ') (subject : typ') : R.rule list =
    match (unalias orig target, unalias orig subject) with
    | VarT { synid = tid; _ }, VarT { synid = sid; _ } when tid.it <> sid.it
      -> (
        match (typcases_of orig tid.it, typcases_of orig sid.it) with
        | Some tcs, Some scs ->
            let t_infos = List.map case_info_of_typcase tcs in
            List.concat_map
              (fun sc ->
                let ci = case_info_of_typcase sc in
                if List.mem ci t_infos then []
                else (
                  (* The interpreter keys membership on the notation alone; we
                     key on origin too. Flag a non-member whose notation
                     collides with a member's under another origin, where the
                     two keyings could part ways. *)
                  if
                    List.exists
                      (fun ti ->
                        ti.mixop = ci.mixop && ti.arity = ci.arity
                        && ti.origin <> ci.origin)
                      t_infos
                  then
                    Printf.eprintf
                      "warning: subty complement %s: non-member case %s of %s \
                       shares its notation with a member case under a \
                       different origin\n"
                      tid.it
                      (variant_sym ci.origin ci.mixop)
                      sid.it;
                  let key =
                    (subty_sym tid.it, variant_sym ci.origin ci.mixop)
                  in
                  if Hashtbl.mem seen key then []
                  else (
                    Hashtbl.add seen key ();
                    [
                      rule
                        (app_t (subty_sym tid.it)
                           [
                             variant_t ci.origin ci.mixop (fresh_vars ci.arity);
                           ])
                        (bool_t ~scalars false);
                    ])))
              scs
        | _ -> [])
    | TupleT ts, TupleT ss when List.length ts = List.length ss ->
        List.concat
          (List.map2 (fun (t : typ) (s : typ) -> complement t.it s.it) ts ss)
    | IterT { typ = te; iter = List }, IterT { typ = se; iter = List }
    | IterT { typ = te; iter = Opt }, IterT { typ = se; iter = Opt } ->
        complement te.it se.it
    | _ -> []
  in
  let rec of_exp (e : exp) : R.rule list =
    (match e.it with SubE (e1, t) -> complement t.it e1.note | _ -> [])
    @ List.concat_map of_exp (Exp_map.subexps e.it)
  in
  let of_prem (p : prem) : R.rule list =
    List.concat_map of_exp (Exp_map.exps_of_prem p)
  in
  List.concat_map
    (fun (exps, prems) ->
      List.concat_map of_exp exps @ List.concat_map of_prem prems)
    (List.concat_map blocks_of_def simplified)

(* -------------------------------------------------------------------------- *)
(* Spec body rules. *)

let pattern_of_arg ~scalars (fresh : unit -> string) (a : arg) :
    R.term option * R.cond list =
  match a.it with
  | ExpA e ->
      let t, c = pattern_of_exp ~scalars fresh e in
      (Some t, c)
  | DefA _ -> (None, [])

(* Rule-unique fresh names for the collections bound by iterated head patterns. *)
(* A clause/rule carries [-- otherwise] ([ElsePr]) when it should fire only if
   no earlier sibling did. [conds_of_prem] drops [ElsePr] (it contributes no
   condition); the flag is preserved here for {!To_maude} to emit as [owise]. *)
let has_otherwise (prems : prem list) : bool =
  List.exists (fun p -> match p.it with ElsePr -> true | _ -> false) prems

let rule_of_clause ~scalars (orig : spec) (id : id) (clause : clause) : R.rule =
  let { args; body = exp; prems } = clause.it in
  let fresh = fresh_binder () in
  let arg_pairs = List.map (pattern_of_arg ~scalars fresh) args in
  {
    R.lhs = app_t (func_sym id) (List.filter_map fst arg_pairs);
    rhs = term_of_exp ~scalars exp;
    conds =
      List.concat_map snd arg_pairs @ conds_of_prems ~scalars orig fresh prems;
    owise = has_otherwise prems;
  }

let rule_of_rel_rule ~scalars (orig : spec) (id : id) (inputs : int list)
    (rl : rule) : R.rule =
  let { concl = ne; prems; _ } = rl.it in
  let args = Mixfix.args ne in
  let in_args, out_args = split_inputs inputs args in
  let fresh = fresh_binder () in
  let in_pairs = List.map (pattern_of_exp ~scalars fresh) in_args in
  {
    R.lhs = app_t (rel_sym id) (List.map fst in_pairs);
    rhs = output_term ~scalars (List.map (term_of_exp ~scalars) out_args);
    conds =
      List.concat_map snd in_pairs @ conds_of_prems ~scalars orig fresh prems;
    owise = has_otherwise prems;
  }

let rules_of_def ~scalars (orig : spec) (def : def) : R.rule list =
  match def.it with
  | DecD { defid = id; clauses; _ } ->
      List.map (rule_of_clause ~scalars orig id) clauses
  | RelD { relid = id; reltyp; rules } ->
      let n = List.length (Mixfix.args (Mode.notation reltyp.it)) in
      let inputs, _ = Mode.partition reltyp.it (List.init n Fun.id) in
      List.map (rule_of_rel_rule ~scalars orig id inputs) rules
  | TypD _ | BuiltinDecD _ -> []

(* -------------------------------------------------------------------------- *)
(* Top level. *)

(* Drop the prelude/type-derived rules in [defs] whose defined symbol is never
   reached from the actual [body] rules. Reachability is transitive: keeping a
   symbol pulls in everything the rules defining it reference. Constructors have
   no defining rules, so only operations/matchers/accessors/subtype predicates
   are ever pruned. *)
let prune_unused (defs : R.rule list) (body : R.rule list) : R.rule list =
  let roots = List.concat_map R.refs_of_rule body in
  let reachable = R.reachable_heads ~roots defs in
  List.filter
    (fun r ->
      match R.defined_head r with
      | Some head -> Hashtbl.mem reachable head
      | None -> false)
    defs

(* Structural equality over text bytes: [eq] decides every pair drawn from the
   spec's alphabet (true on the diagonal, false off it). *)
let char_eq_rules ~scalars (codes : int list) : R.rule list =
  List.concat_map
    (fun c ->
      List.map
        (fun d -> rule (eq_t (chr_t c) (chr_t d)) (bool_t ~scalars (c = d)))
        codes)
    codes

let of_spec ?(scalars = Structural) ?(extra_defs = []) ~(orig : spec)
    (simplified : spec) : R.t =
  let type_rules =
    Prelude.rules ~scalars @ List.concat_map (defs_of_typ ~scalars) orig
  in
  let body_rules = List.concat_map (rules_of_def ~scalars orig) simplified in
  let iter_rules = iter_helper_defs ~scalars orig simplified in
  let sub_rules =
    sub_helper_defs ~scalars orig simplified
    @ sub_complement_defs ~scalars orig simplified
  in
  (* [extra_defs] (builtin rules) can introduce text bytes of their own, so scan
     them too, or a produced text could meet a [chr] with no equality rule. *)
  let char_rules =
    char_eq_rules ~scalars
      (char_codes_of_rules (type_rules @ body_rules @ extra_defs))
  in
  let type_rules =
    prune_unused
      (type_rules @ char_rules @ iter_rules @ sub_rules @ extra_defs)
      body_rules
  in
  let rules = type_rules @ body_rules in
  let vars = R.dedup_stable (List.concat_map R.vars_of_rule rules) in
  { R.vars; rules }

(* The slice roots: the symbol each top-level function/relation defines, in spec
   order. *)
let def_symbols (spec : spec) : string list =
  List.filter_map
    (fun def ->
      match def.it with
      | DecD { defid = id; _ } -> Some (func_sym id)
      | RelD { relid = id; _ } -> Some (rel_sym id)
      | TypD _ | BuiltinDecD _ -> None)
    spec

(* Relations that declare a non-empty input mode (`hint(input ...)`): functional,
   so they may be emitted as Maude equations rather than rules. *)
let input_moded_rel_syms (spec : spec) : string list =
  List.filter_map
    (fun def ->
      match def.it with
      | RelD { relid = id; reltyp; _ } when Mode.inputs reltyp.it <> [] ->
          Some (rel_sym id)
      | _ -> None)
    spec

(* Relations with an empty input mode: the complement of [input_moded_rel_syms].
   Being non-deterministic, they are emitted as Maude rules (rl/crl) rather than
   equations. *)
let rule_head_syms (spec : spec) : string list =
  List.filter_map
    (fun def ->
      match def.it with
      | RelD { relid = id; reltyp; _ } when Mode.inputs reltyp.it = [] ->
          Some (rel_sym id)
      | _ -> None)
    spec
