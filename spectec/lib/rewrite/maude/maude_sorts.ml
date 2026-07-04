open Common.Source
open Lang.Il
module R = Rewrite_system
module T = Ctrs_term

(** Order-sorted signature recovery shared by both Maude surfaces: the
    executable one ({!To_maude}, native scalar theory) and the analysis one
    ({!To_mfe}, structural scalar theory). A CTRS term carries no sorts
    ({!Rewrite_system}), so each operator's Maude signature is recovered here
    from the original (un-simplified) IL spec's [TypD]/[RelD]/[DecD], using the
    {e same} naming functions {!To_ctrs} used in the rules.

    Everything sits under a single universal supersort [Val] (every sort is
    [< Val]); this is the pragmatic resolution of the parametric polymorphism
    the erased CTRS left behind (type parameters, tuples, container element
    slots).

    The recovery is {b theory-agnostic}: {!sort_of_typ} returns the same sort
    {e names} ([NatV]/[IntV]/[BoolV]/[Text]) for both theories -- only the
    {e constructors} inhabiting those sorts differ (structural [zero]/[succ]/
    [int_pos]/... vs native [nat(_)]/[int(_)]/... wrappers, see
    {!scalar_ctor_sigs}). *)

type scalar_theory = Ctrs_term.scalar_theory = Structural | Native

(* The universal supersort every other sort sits under. *)
let val_sort = "Val"
let dedup xs = List.sort_uniq compare xs

(* The named type a value/expression carries, if it is a [VarT]. *)
let typ_name_of (typ : typ') : string option =
  match typ with VarT { synid; _ } -> Some synid.it | _ -> None

(* The Maude sort name for a named IL type. Capitalised to keep sorts visually
   distinct from the lower-case operator ids and to follow Maude convention. *)
let sort_of_name (n : string) : string =
  String.capitalize_ascii (R.maude_id (R.sanitize n))

(* -------------------------------------------------------------------------- *)
(* IL types -> sorts. *)

(* Index the spec's type definitions so [VarT] references and aliases resolve. *)
let type_env (orig : spec) : (string, deftyp') Hashtbl.t =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun def ->
      match def.it with
      | TypD { synid = tid; deftyp = dt; _ } -> Hashtbl.replace tbl tid.it dt.it
      | _ -> ())
    orig;
  tbl

(* The sort of an IL type. Aliases ([syntax T = U]) and the [map = (pair)*]
   shape resolve through to the underlying sort; scalars map to [NatV]/[IntV]/
   [BoolV]/[Text] (the constructors of those sorts differ per theory, but the
   names do not); lists/options to the generic container sorts; everything else
   (tuples, [func], unresolved type parameters) falls back to [Val]. *)
let rec sort_of_typ (tenv : (string, deftyp') Hashtbl.t) (ty : typ') : string =
  match ty with
  | BoolT -> "BoolV"
  | NumT `NatT -> "NatV"
  | NumT `IntT -> "IntV"
  | TextT -> "Text"
  | VarT { synid = tid; _ } -> (
      match Hashtbl.find_opt tenv tid.it with
      | Some (PlainT u) -> sort_of_typ tenv u.it
      | Some (VariantT _ | StructT _) -> sort_of_name tid.it
      (* not a declared type: a type parameter (e.g. the [K]/[V] of [pair<K,V>])
         or an opaque external -- generic, so [Val]. *)
      | None -> val_sort)
  | IterT { iter = List; _ } -> "List"
  | IterT { iter = Opt; _ } -> "Opt"
  | TupleT _ -> val_sort
  | FuncT -> val_sort

(* -------------------------------------------------------------------------- *)
(* Signature recovery: CTRS symbol -> (argument sorts, result sort). *)

type sigs = (string, string list * string) Hashtbl.t

(* The theory-agnostic prelude/built-in operator signatures, keyed by the
   {e original} CTRS symbol (with [_]); names are mangled to Maude only at
   emission. Both scalar theories share these -- the sort names ([NatV]/[IntV]/
   [BoolV]/[List]/...) are the same; only the scalar CONSTRUCTORS differ (see
   {!scalar_ctor_sigs}). Must track the prelude in {!To_ctrs}. *)
let shared_op_sigs : (string * (string list * string)) list =
  let b1 = ([ "BoolV" ], "BoolV") in
  let b2 = ([ "BoolV"; "BoolV" ], "BoolV") in
  let n2 = ([ "NatV"; "NatV" ], "NatV") in
  let n2b = ([ "NatV"; "NatV" ], "BoolV") in
  let i1 = ([ "IntV" ], "IntV") in
  let i2 = ([ "IntV"; "IntV" ], "IntV") in
  let i2b = ([ "IntV"; "IntV" ], "BoolV") in
  [
    ("not", b1);
    ("and", b2);
    ("or", b2);
    ("impl", b2);
    ("equiv", b2);
    ("add", n2);
    ("sub", n2);
    ("mul", n2);
    ("div", n2);
    ("mod", n2);
    ("pow", n2);
    ("leq", n2b);
    ("lt", n2b);
    ("int_pos", ([ "NatV" ], "IntV"));
    ("int_neg", ([ "NatV" ], "IntV"));
    ("negate_int", i1);
    ("abs_nat", ([ "IntV" ], "NatV"));
    ("nonneg_int", ([ "IntV" ], "BoolV"));
    ("nat_of_int", ([ "IntV" ], "NatV"));
    ("sub_int_nat", ([ "NatV"; "NatV" ], "IntV"));
    ("add_int", i2);
    ("sub_int", i2);
    ("mul_int", i2);
    ("div_int", i2);
    ("mod_int", i2);
    ("pow_int", i2);
    ("leq_int", i2b);
    ("lt_int", i2b);
    ("nil", ([], "List"));
    ("cons", ([ val_sort; "List" ], "List"));
    (* [len]/[cat] also work over texts, and [List < Text] makes [Text] the
       sort covering both: inferring an argument variable at [Text] lets it
       bind a list or a text, where [List] would reject every [txt]. The
       [List]-precise overloads are declared alongside (see [overload_sigs]). *)
    ("len", ([ "Text" ], "NatV"));
    ("cat", ([ "Text"; "Text" ], "Text"));
    ("mem", ([ val_sort; "List" ], "BoolV"));
    ("idx", ([ "List"; "NatV" ], val_sort));
    ("take", ([ "List"; "NatV" ], "List"));
    ("drop", ([ "List"; "NatV" ], "List"));
    ("slice", ([ "List"; "NatV"; "NatV" ], "List"));
    ("upd_idx", ([ "List"; "NatV"; val_sort ], "List"));
    ("upd_slice", ([ "List"; "NatV"; "NatV"; "List" ], "List"));
    ("none", ([], "Opt"));
    ("some", ([ val_sort ], "Opt"));
    ("sub_nat", ([ val_sort ], "BoolV"));
    (* the list/option matchers decide a [BoolV] over a spine of their own sort *)
    ("match_some", ([ "Opt" ], "BoolV"));
    ("match_none", ([ "Opt" ], "BoolV"));
    ("match_cons", ([ "List" ], "BoolV"));
    ("match_nil", ([ "List" ], "BoolV"));
    ("eq", ([ val_sort; val_sort ], "BoolV"));
  ]

(* The scalar CONSTRUCTOR signatures, per theory. [Native] wraps Maude's
   built-ins ([nat(3) : NatV], {!Maude_theory}); [Structural] declares the
   self-contained Peano/sign-magnitude/own-boolean constructors ([zero]/[succ]/
   [true]/[false]; [int_pos]/[int_neg] already appear in {!shared_op_sigs}, and
   [chr_<n>] is handled by the {!signature} heuristic). *)
let scalar_ctor_sigs : scalar_theory -> (string * (string list * string)) list =
  function
  | Native ->
      [
        (Maude_theory.bool_wrap_sym, ([ "Bool" ], "BoolV"));
        (Maude_theory.nat_wrap_sym, ([ "Nat" ], "NatV"));
        (Maude_theory.int_wrap_sym, ([ "Int" ], "IntV"));
        (Maude_theory.text_wrap_sym, ([ "String" ], "Text"));
      ]
  | Structural ->
      [
        ("zero", ([], "NatV"));
        ("succ", ([ "NatV" ], "NatV"));
        ("true", ([], "BoolV"));
        ("false", ([], "BoolV"));
      ]

let prelude_sigs (scalars : scalar_theory) :
    (string * (string list * string)) list =
  scalar_ctor_sigs scalars @ shared_op_sigs

(* Variant case as seen from its containing type (mirrors
   [To_ctrs.case_info_of_typcase]). *)
let case_origin_mixop (tc : typcase) : string * Lang.Il.mixop =
  let { synid = origin_id; _ } = tc.origin.it in
  (origin_id.it, Mixfix.to_mixop tc.notation.it)

(* Subsort edges (sub, super) and per-symbol signatures recovered from the
   original spec's type/relation/function declarations. *)
(* The iteration/reflection layer synthesizes helpers ([$unzip_..],
   [$itercollect_..], [holds_$iterall_..], ..) that never appear in [orig]'s
   own [TypD]/[RelD]/[DecD], so [tbl] has no entry for them and [signature]'s
   fallback types them all-[Val] -- workable in isolation (a stuck [Val] can
   sit anywhere), but not when the RESULT feeds a precisely-typed consumer
   (e.g. a real variant constructor declared [List -> Set]): a stuck,
   [Val]-sorted argument there is a genuine sort mismatch (not [List]),
   which is [ERROR]-kind, and no equation -- not even an [and]/[or]
   short-circuit -- can ever match an [ERROR]-kind subterm. Recovering a
   precise RANGE for these synthesized symbols fixes that at the source: if
   EVERY rule defining a symbol has an RHS headed by the same container's own
   constructor ([nil]/[cons], [none]/[some], [true]/[false]), the symbol's
   range is that container's sort regardless of what varies inside it (the
   ELEMENT type, which [$itercollect] in particular can't pin down) -- the
   container shape itself is exactly what these rules always produce, so the
   inference is sound by construction, not a name-based guess. Symbols with no
   rules, or whose rules disagree, are left to the [Val] fallback (unchanged
   from before). Never touches a symbol already recovered from [orig] --
   FILLS GAPS, never overrides a real declared type. *)
let infer_ranges (rules : R.rule list) : sigs =
  (* per symbol: its arity (from any one rule's lhs -- a CTRS function has a
     fixed arity) and every rule's RHS head, collected together so a single
     pass can both size the [Val] argument list and check RHS agreement. *)
  let seen : (string, int * string list) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun (r : R.rule) ->
      match (R.defined_head r, r.R.lhs, r.R.rhs) with
      | Some sym, R.App (_, args), R.App (h, _) ->
          let arity, hs =
            Option.value (Hashtbl.find_opt seen sym)
              ~default:(List.length args, [])
          in
          Hashtbl.replace seen sym (arity, h :: hs)
      | _ -> ())
    rules;
  (* A rule's RHS head can itself be a call to another BoolV-producing
     combinator ($builtin_list_submem's own recursive case reduces via
     [and(mem(..), ..)], not a bare true/false) -- recognize those too, not
     just the ground constants, or one non-literal disjunct/conjunct in an
     otherwise-uniform RHS set silently drops the whole inference. *)
  let has_prefix p sym =
    String.length sym >= String.length p
    && String.sub sym 0 (String.length p) = p
  in
  let range_of_head sym =
    match sym with
    | "nil" | "cons" -> Some "List"
    | "none" | "some" -> Some "Opt"
    | "true" | "false" | "and" | "or" | "not" | "eqg" -> Some "BoolV"
    | _
      when has_prefix "match_" sym || has_prefix "subty_" sym
           || has_prefix "holds_" sym ->
        Some "BoolV"
    | _ -> None
  in
  let ranges : sigs = Hashtbl.create 64 in
  Hashtbl.iter
    (fun sym (arity, heads) ->
      (* a recursive rule's RHS head is often the symbol itself (e.g. a
         [$builtin_list_diff]-style fold calling itself on the tail): that
         occurrence says nothing new about the range (whatever it turns out
         to be, it has to agree with itself), so only the OTHER, base-case
         rules need to agree for the inference to hold. *)
      match List.filter (fun h -> h <> sym) heads with
      | [] -> ()
      | h0 :: rest -> (
          match range_of_head h0 with
          | Some rng when List.for_all (fun h -> range_of_head h = Some rng) rest
            ->
              Hashtbl.replace ranges sym (List.init arity (fun _ -> val_sort), rng)
          | _ -> ()))
    seen;
  ranges

(* [reflect.ml]'s [ensure_proj] emits, for a variant/tuple case, ONE payload
   projector per field: [proj_<ctor>_<i>(ctor(x0..xn-1)) = xi], always a bare
   pattern variable on the right, so [infer_ranges]'s RHS-shape scan (which
   needs an [App] head to read anything off) never sees these at all. But the
   field's real type doesn't need to be guessed here: [ctor] is a REAL
   constructor already recovered into [tbl] by the main scan above (its own
   argument sorts came straight off the [TypD] that declared it), so the i-th
   argument sort IS the projector's range, exactly. (A "tuple" pseudo-ctor,
   {!Ctrs_term}'s generic n-ary wrapper with no [TypD] of its own, has no
   entry to look up -- its projectors stay at the existing [Val] fallback,
   same as today.) *)
let infer_proj_ranges (tbl : sigs) (rules : R.rule list) : sigs =
  let index_of (x : R.term) (xs : R.term list) : int option =
    let rec go i = function
      | [] -> None
      | y :: ys -> if y = x then Some i else go (i + 1) ys
    in
    go 0 xs
  in
  let ranges : sigs = Hashtbl.create 64 in
  List.iter
    (fun (r : R.rule) ->
      match (r.R.lhs, r.R.rhs) with
      | R.App (sym, [ R.App (ctor, args) ]), (R.Var _ as rhs) -> (
          match index_of rhs args with
          | Some i -> (
              match Hashtbl.find_opt tbl ctor with
              | Some (ctor_args, _) when List.length ctor_args > i ->
                  Hashtbl.replace ranges sym
                    ([ val_sort ], List.nth ctor_args i)
              | _ -> ())
          | None -> ())
      | _ -> ())
    rules;
  ranges

let recover ?(rules : R.rule list = []) (scalars : scalar_theory) (orig : spec)
    (tenv : (string, deftyp') Hashtbl.t) : sigs * (string * string) list =
  let tbl : sigs = Hashtbl.create 256 in
  let subsorts = ref [] in
  let add sym sg = Hashtbl.replace tbl sym sg in
  List.iter (fun (s, sg) -> add s sg) (prelude_sigs scalars);
  let sort_of t = sort_of_typ tenv t in
  List.iter
    (fun def ->
      match def.it with
      | TypD { synid = tid; deftyp = { it = VariantT typcases; _ }; _ } ->
          List.iter
            (fun (typcase : typcase) ->
              let nottyp = typcase.notation in
              let origin, mixop = case_origin_mixop typcase in
              let field_typs = Mixfix.args nottyp.it in
              let args = List.map (fun ft -> sort_of ft.it) field_typs in
              add (T.variant_sym origin mixop) (args, sort_of_name origin);
              (* the matcher for this case tests a value of the containing type
                 [tid]: at every use site the subject has been narrowed to [tid]
                 (a cast/match precedes it), so a [tid]-sorted argument is safe
                 and drops the needless [Val]. *)
              add (T.match_sym tid.it mixop) ([ sort_of_name tid.it ], "BoolV");
              (* an injected case ([origin <> tid]) makes [origin] a subsort *)
              if origin <> tid.it then
                subsorts :=
                  (sort_of_name origin, sort_of_name tid.it) :: !subsorts)
            typcases
      | TypD { synid = tid; deftyp = { it = StructT fields; _ }; _ } ->
          let t = tid.it in
          let field_sorts = List.map (fun (_, ft) -> sort_of ft.it) fields in
          add (T.struct_sym t) (field_sorts, sort_of_name t);
          List.iter
            (fun (a, ft) ->
              add (T.field_sym t a) ([ sort_of_name t ], sort_of ft.it);
              add (T.upd_field_sym t a)
                ([ sort_of_name t; sort_of ft.it ], sort_of_name t))
            fields
      | TypD { deftyp = { it = PlainT _; _ }; _ } -> ()
      | RelD { relid = id; reltyp; _ } ->
          let arg_typs = Mixfix.args (Mode.notation reltyp.it) in
          let ins, outs = Mode.partition reltyp.it arg_typs in
          let argsorts = List.map (fun t -> sort_of t.it) ins in
          let result =
            (* a judgment (no outputs) reduces to the wrapped bool [bool(true)] *)
            match outs with
            | [] -> "BoolV"
            | [ t ] -> sort_of t.it
            | _ -> val_sort
          in
          add (T.rel_sym id) (argsorts, result)
      | DecD { defid = id; params; typ = ret; _ } ->
          let argsorts =
            List.map
              (fun p ->
                match p.it with ExpP t -> sort_of t.it | DefP _ -> val_sort)
              params
          in
          add (T.func_sym id) (argsorts, sort_of ret.it)
      | BuiltinDecD _ -> ())
    orig;
  let merge_gaps sigs =
    Hashtbl.iter
      (fun sym sg ->
        if not (Hashtbl.mem tbl sym) then Hashtbl.replace tbl sym sg)
      sigs
  in
  merge_gaps (infer_ranges rules);
  merge_gaps (infer_proj_ranges tbl rules);
  (tbl, !subsorts)

(* The signature of [sym] at [arity]: the recovered/prelude one when its arity
   agrees, otherwise a heuristic for the per-type predicates the CTRS derives
   ([match_<T>_*]/[subty_<T>]/[holds_*] (the {!Reflect} judgment reflections)
   decide [BoolV] over a bare-[Val] subject -- [subty_<T>] is totalized over
   [Val], and a generic matcher may see any value), and finally an all-[Val]
   fallback. (The iteration helpers
   [$itermap]/[$unzip]/[$iterall]/[$itercollect] are intentionally left at [Val]:
   their arg/result sorts vary -- [$iterall] returns a [BoolV], [$itercollect]'s
   spine sort depends on the body -- so a blanket [List] typing was ill-formed
   and stuck tuple/sequence programs.) *)
let signature (tbl : sigs) (sym : string) (arity : int) : string list * string =
  match Hashtbl.find_opt tbl sym with
  | Some (args, res) when List.length args = arity -> (args, res)
  | _ ->
      let has p =
        String.length sym >= String.length p
        && String.sub sym 0 (String.length p) = p
      in
      if has "match_" || has "subty_" || has "holds_" || sym = "eqg" then
        (List.init arity (fun _ -> val_sort), "BoolV")
      else (List.init arity (fun _ -> val_sort), val_sort)

(* -------------------------------------------------------------------------- *)
(* Subsort order (for picking the most specific sort of a variable). *)

(* [is_sub edges a b]: is sort [a] a subsort of (or equal to) [b]? Everything is
   [< Val]; injection edges add the rest. Small graph, so a plain DFS. *)
let is_sub (edges : (string * string) list) (a : string) (b : string) : bool =
  if a = b then true
  else if b = val_sort then true
  else
    let rec reach seen x =
      x = b
      || List.exists
           (fun (s, super) ->
             s = x && (not (List.mem super seen)) && reach (super :: seen) super)
           edges
    in
    reach [ a ] a

(* The more specific of two sorts under [edges] (defaults to keeping [cur] on an
   unrelated clash). *)
let meet edges cur s =
  if is_sub edges s cur then s else if is_sub edges cur s then cur else cur

(* -------------------------------------------------------------------------- *)
(* Per-rule variable sorts (on-the-fly typing). *)

let result_sort (sg : string -> int -> string list * string) (t : R.term) :
    string =
  match t with
  | Var _ -> val_sort
  | App (f, args) -> snd (sg f (List.length args))

(* [hint v] is a variable's declared (narrow) sort recovered from the IL, when
   known. It is authoritative: every position the variable occupies is a
   supersort of its declared type, so the narrow sort stays well-sorted and, for
   a relation input, stops the rule overlapping its siblings. Variables without
   a hint (prelude rules, synthetic binders) fall back to position inference. *)
let infer_var_sorts edges (sg : string -> int -> string list * string)
    (hint : string -> string option) (r : R.rule) : (string, string) Hashtbl.t =
  let vs = Hashtbl.create 16 in
  let update v s =
    match hint v with
    | Some hs -> Hashtbl.replace vs v hs
    | None -> (
        match Hashtbl.find_opt vs v with
        | None -> Hashtbl.replace vs v s
        | Some cur -> Hashtbl.replace vs v (meet edges cur s))
  in
  let rec go expected t =
    match t with
    | R.Var v -> update v expected
    | R.App (f, args) ->
        let argsorts, _ = sg f (List.length args) in
        let argsorts =
          if List.length argsorts = List.length args then argsorts
          else List.map (fun _ -> val_sort) args
        in
        List.iter2 go argsorts args
  in
  (match r.R.lhs with
  | R.App (f, args) ->
      let argsorts, _ = sg f (List.length args) in
      let argsorts =
        if List.length argsorts = List.length args then argsorts
        else List.map (fun _ -> val_sort) args
      in
      List.iter2 go argsorts args
  | R.Var _ -> ());
  go (result_sort sg r.R.lhs) r.R.rhs;
  List.iter
    (fun (l, rr) ->
      go (result_sort sg l) l;
      go (result_sort sg l) rr)
    r.R.conds;
  vs

(* -------------------------------------------------------------------------- *)
(* Term printing with on-the-fly variable sorts. *)

(* A nullary symbol that Maude parses as a built-in literal to print verbatim (a
   numeral, quoted string, or [true]/[false]) rather than declare + mangle. Only
   in the [Native] theory: [Structural] has no built-ins, so its [true]/[false]
   are ordinary declared constructors and every numeral is a Peano tower. *)
let is_literal (scalars : scalar_theory) (s : string) : bool =
  match scalars with
  | Native -> Maude_theory.is_literal_sym s
  | Structural -> false

let sort_of_var (vs : (string, string) Hashtbl.t) (v : string) : string =
  Option.value (Hashtbl.find_opt vs v) ~default:val_sort

let rec print_term scalars vs (t : R.term) : string =
  match t with
  | R.Var v -> R.maude_var v ^ ":" ^ sort_of_var vs v
  (* a built-in literal (numeral / quoted string / bool): verbatim, never mangled *)
  | R.App (f, []) when is_literal scalars f -> f
  | R.App (f, []) -> R.maude_id f
  | R.App (f, args) ->
      R.maude_id f ^ "("
      ^ String.concat ", " (List.map (print_term scalars vs) args)
      ^ ")"

(* -------------------------------------------------------------------------- *)
(* Symbol collection for op declarations. *)

(* Sorts and constructors that must be declared even if the spec's rules never
   mention them, so concrete start terms (built by a language encoder) can be
   formed: every variant case and struct constructor/accessor from [orig]. *)
let il_constructor_syms (orig : spec) : string list =
  List.concat_map
    (fun def ->
      match def.it with
      | TypD { deftyp = { it = VariantT typcases; _ }; _ } ->
          List.map
            (fun tc ->
              let origin, mixop = case_origin_mixop tc in
              T.variant_sym origin mixop)
            typcases
      | TypD { synid = t; deftyp = { it = StructT fields; _ }; _ } ->
          T.struct_sym t.it
          :: List.concat_map
               (fun (a, _) -> [ T.field_sym t.it a; T.upd_field_sym t.it a ])
               fields
      | _ -> [])
    orig

(* The distinct (symbol, arity) pairs that occur as application heads anywhere in
   the rules. A symbol used at several arities (the generic [tuple] constructor
   spans 2-tuples, 3-tuples, …) yields one pair per arity, so each gets its own
   Maude [op] declaration; collapsing them to one arity would leave the others
   unparseable ("didn't expect token ,"). *)
let symbol_arities scalars (rules : R.rule list) : (string * int) list =
  let acc = Hashtbl.create 256 in
  let rec walk = function
    | R.Var _ -> ()
    | R.App (f, args) ->
        if not (is_literal scalars f) then
          Hashtbl.replace acc (f, List.length args) ();
        List.iter walk args
  in
  List.iter
    (fun r ->
      walk r.R.lhs;
      walk r.R.rhs;
      List.iter
        (fun (a, b) ->
          walk a;
          walk b)
        r.R.conds)
    rules;
  Hashtbl.fold (fun pair () acc -> pair :: acc) acc []
