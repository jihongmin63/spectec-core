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

(* The common supersort of the two scalar number sorts, [NatV] and [IntV].
   It exists for one operator: [sub_nat], the [e <: nat] membership test, which
   {!Prelude} defines over BOTH representations (the binary nats [bzero]/[bone]/
   [bd0]/[bd1] and the sign-magnitude ints [int_pos]/[int_neg]; the [Native]
   theory delegates it over Maude's own [nat(_)]/[int(_)] just as widely). With
   no supersort of its own it could only be declared over [Val], which reads as
   "any value at all" and makes the operator look partial to a sufficient-
   completeness checker -- every constructor of every sort becomes a legal
   argument, and naturally no rule covers a struct. Over [NumV] its six rules
   are exactly total. *)
let num_sort = "NumV"
let dedup xs = List.sort_uniq compare xs

let has_prefix p sym =
  String.length sym >= String.length p && String.sub sym 0 (String.length p) = p

(* The per-type predicates the CTRS derives -- variant-case matchers
   ({!Reflect.ensure_matchers}), subtype tests ({!To_ctrs}'s [subty_<T>]), the
   judgment reflections ({!Reflect}'s [holds_<R>]) and the generic equality.
   They share two properties no other symbol has: they always return [BoolV],
   and their argument domain is declared NOWHERE -- the generators emit them
   as needed, so the domain has to be recovered from how they are used
   ({!predicate_domains}). *)
let is_predicate (sym : string) : bool =
  has_prefix "match_" sym || has_prefix "subty_" sym || has_prefix "holds_" sym
  || sym = "eqg"

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
  | VarT { synid = tid; _ } -> sort_of_typname tenv tid.it
  | IterT { iter = List; _ } -> "List"
  | IterT { iter = Opt; _ } -> "Opt"
  | TupleT _ -> val_sort
  | FuncT -> val_sort

(* The sort of a named IL type, resolving [syntax T = U] aliases through to the
   underlying sort. *)
and sort_of_typname (tenv : (string, deftyp') Hashtbl.t) (n : string) : string =
  match Hashtbl.find_opt tenv n with
  | Some (PlainT u) -> sort_of_typ tenv u.it
  | Some (VariantT _ | StructT _) -> sort_of_name n
  (* not a declared type: a type parameter (e.g. the [K]/[V] of [pair<K,V>])
     or an opaque external -- generic, so [Val]. *)
  | None -> val_sort

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
    (* BUG FIX: [abs_nat]/[sub_int_nat] used to be declared over Peano [NatV]
       (correct back when [int_pos]/[int_neg]'s magnitude WAS Peano); the
       Phase 4 retype to [BNatV] updated their RULES (in {!Prelude}) but not
       these two declarations, leaving both silently ill-sorted against
       their own actual output/argument sort ever since. Unlike [nat_of_int]
       (a genuine [DownCastE] bridge whose callers throughout the wider spec
       need real Peano [NatV] back, hence [bnat_to_nat] there), [abs_nat]/
       [sub_int_nat] are purely internal to this int-family (only ever
       consumed by [div_int]/[mod_int]/[add_int]'s own rules here, never by
       {!Builtin} or the wider [.spectec] source, confirmed by grep), and
       every one of those consumers already correctly expects [BNatV] --
       so the fix is these two declarations, not a bridging rule. Found via
       the differential corpus run: any program computing [div_int]/
       [mod_int] or the [int_pos]/[int_neg] cross-sign [add_int] cases (e.g.
       [int<n>]-typed negative constant folding, which routes through
       [add_int]'s [sub_int_nat] case) got permanently stuck one level up,
       the same failure shape as the [nat_of_int] bug. *)
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
    (* Binary (Coq [positive]/[N]-style) nat family: the representation of BOTH
       a bare nat AND [int_pos]/[int_neg]'s magnitude (the nat->binary retype
       merged the former separate [BNatV] magnitude sort into [NatV]). All four
       constructors sit in the ONE sort [NatV] (no separate zero-free [positive]
       sort: this recovery table holds only one signature per symbol name, so
       [bd0]/[bd1] cannot be typed to statically reject a zero argument the way
       Coq's [positive] does -- canonicity is instead a by-construction property
       of every rule in {!Prelude} that builds a nat term, see {!Ctrs_term}'s
       doc comment). [Bmask] is [bsub_mask]'s 3-valued truncated-subtraction
       result, [Bcmp] is [bcompare]'s 3-valued relation-so-far; neither is ever
       itself used as a magnitude, so neither has a [NatV] subsort edge. (The old
       Peano<->binary bridges [bnat_of_nat]/[bnat_to_nat]/[double_nat] are gone
       now that a nat IS binary.) *)
    ("bzero", ([], "NatV"));
    ("bone", ([], "NatV"));
    ("bd0", ([ "NatV" ], "NatV"));
    ("bd1", ([ "NatV" ], "NatV"));
    ("bsucc", ([ "NatV" ], "NatV"));
    ("bpred", ([ "NatV" ], "NatV"));
    ("bpred_double", ([ "NatV" ], "NatV"));
    ("bis_zero", ([ "NatV" ], "BoolV"));
    ("badd", ([ "NatV"; "NatV" ], "NatV"));
    ("badd_carry", ([ "NatV"; "NatV" ], "NatV"));
    ("bmask_nul", ([], "Bmask"));
    ("bmask_neg", ([], "Bmask"));
    ("bmask_pos", ([ "NatV" ], "Bmask"));
    ("bsub_mask", ([ "NatV"; "NatV" ], "Bmask"));
    ("bsub_mask_carry", ([ "NatV"; "NatV" ], "Bmask"));
    ("bdouble_mask", ([ "Bmask" ], "Bmask"));
    ("bsucc_double_mask", ([ "Bmask" ], "Bmask"));
    ("bsub_of_mask", ([ "Bmask" ], "NatV"));
    ("bsub", ([ "NatV"; "NatV" ], "NatV"));
    ("bmul", ([ "NatV"; "NatV" ], "NatV"));
    ("bring0", ([ "NatV" ], "NatV"));
    ("bring1", ([ "NatV" ], "NatV"));
    ("bdivmod", ([ "NatV"; "NatV" ], "Bdivmod"));
    ("bquot", ([ "Bdivmod" ], "NatV"));
    ("brem", ([ "Bdivmod" ], "NatV"));
    ("bdivmod_pos", ([ "NatV"; "NatV" ], "Bdivmod"));
    ("bdivmod_step0", ([ "Bdivmod"; "NatV" ], "Bdivmod"));
    ("bdivmod_step1", ([ "Bdivmod"; "NatV" ], "Bdivmod"));
    ("bdivmod_combine", ([ "NatV"; "NatV"; "NatV" ], "Bdivmod"));
    ("bdivmod_dispatch", ([ "BoolV"; "NatV"; "NatV"; "NatV" ], "Bdivmod"));
    ("bdivmod_base", ([ "BoolV"; "NatV" ], "Bdivmod"));
    ("bdiv", ([ "NatV"; "NatV" ], "NatV"));
    ("bmod", ([ "NatV"; "NatV" ], "NatV"));
    ("blt_kind", ([], "Bcmp"));
    ("beq_kind", ([], "Bcmp"));
    ("bgt_kind", ([], "Bcmp"));
    ("bcompare_cont", ([ "Bcmp"; "NatV"; "NatV" ], "Bcmp"));
    ("bcompare", ([ "NatV"; "NatV" ], "Bcmp"));
    ("ble_of_cmp", ([ "Bcmp" ], "BoolV"));
    ("blt_of_cmp", ([ "Bcmp" ], "BoolV"));
    ("bleq", ([ "NatV"; "NatV" ], "BoolV"));
    ("blt", ([ "NatV"; "NatV" ], "BoolV"));
    ("bpow_nat", ([ "NatV"; "NatV" ], "NatV"));
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
    (* the one nat-OR-int operator: over [Val] it would look partial to the SCC
       (see {!num_sort}) *)
    ("sub_nat", ([ num_sort ], "BoolV"));
    (* the list/option matchers decide a [BoolV] over a spine of their own sort *)
    ("match_some", ([ "Opt" ], "BoolV"));
    ("match_none", ([ "Opt" ], "BoolV"));
    ("match_cons", ([ "List" ], "BoolV"));
    ("match_nil", ([ "List" ], "BoolV"));
    ("eq", ([ val_sort; val_sort ], "BoolV"));
  ]

(* The scalar CONSTRUCTOR signatures, per theory. [Native] wraps Maude's
   built-ins ([nat(3) : NatV], {!Maude_theory}); [Structural] declares the
   self-contained own-boolean constructors ([true]/[false]). Naturals are now
   binary-encoded: their constructors ([bzero]/[bone]/[bd0]/[bd1] : NatV) and
   the sign-magnitude int constructors ([int_pos]/[int_neg]) already appear in
   {!shared_op_sigs}, and [chr_<n>] is handled by the {!signature} heuristic. *)
let scalar_ctor_sigs : scalar_theory -> (string * (string list * string)) list =
  function
  | Native ->
      [
        (Maude_theory.bool_wrap_sym, ([ "Bool" ], "BoolV"));
        (Maude_theory.nat_wrap_sym, ([ "Nat" ], "NatV"));
        (Maude_theory.int_wrap_sym, ([ "Int" ], "IntV"));
        (Maude_theory.text_wrap_sym, ([ "String" ], "Text"));
      ]
  | Structural -> [ ("true", ([], "BoolV")); ("false", ([], "BoolV")) ]

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
(* The iteration/reflection layer synthesizes helpers ([$iterproj_..],
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
            Option.value
              (Hashtbl.find_opt seen sym)
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
          | Some rng
            when List.for_all (fun h -> range_of_head h = Some rng) rest ->
              Hashtbl.replace ranges sym
                (List.init arity (fun _ -> val_sort), rng)
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
                  Hashtbl.replace ranges sym ([ val_sort ], List.nth ctor_args i)
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
          (* SEED only ({!predicate_domains} widens it to every subject the
             rules actually pass): the subtype test spans its whole
             encompassing type, and [To_ctrs.sub_complement_defs] additionally
             gives it [-> false] clauses over the SOURCE type of each [SubE]
             site, which is typically a supertype of [tid]. *)
          add (T.subty_sym tid.it) ([ sort_of_typname tenv tid.it ], "BoolV");
          List.iter
            (fun (typcase : typcase) ->
              let nottyp = typcase.notation in
              let origin, mixop = case_origin_mixop typcase in
              let field_typs = Mixfix.args nottyp.it in
              let args = List.map (fun ft -> sort_of ft.it) field_typs in
              add (T.variant_sym origin mixop) (args, sort_of_name origin);
              (* SEED only: the matcher discriminates the cases of the
                 containing type [tid], so [tid] covers its own clauses --
                 but {!Reflect.sibling_guard} also applies it to subjects known
                 only at an OUTER type, which {!predicate_domains} folds in. *)
              add (T.match_sym tid.it mixop) ([ sort_of_name tid.it ], "BoolV");
              (* an injected case ([origin <> tid]) makes [origin] a subsort *)
              if origin <> tid.it then
                subsorts :=
                  (sort_of_name origin, sort_of_name tid.it) :: !subsorts)
            typcases
      | TypD { synid = tid; deftyp = { it = StructT fields; _ }; _ } ->
          let t = tid.it in
          let field_sorts = List.map (fun (_, ft) -> sort_of ft.it) fields in
          add (T.subty_sym t) ([ sort_of_typname tenv t ], "BoolV");
          add (T.struct_sym t) (field_sorts, sort_of_name t);
          List.iter
            (fun (a, ft) ->
              add (T.field_sym t a) ([ sort_of_name t ], sort_of ft.it);
              add (T.upd_field_sym t a)
                ([ sort_of_name t; sort_of ft.it ], sort_of_name t))
            fields
      | TypD { synid = tid; deftyp = { it = PlainT _; _ }; _ } ->
          (* an alias delegates ([subty_T(x) -> subty_U(x)], a bare-variable
             lhs that pins no domain of its own), so the seed is the resolved
             underlying sort. *)
          add (T.subty_sym tid.it) ([ sort_of_typname tenv tid.it ], "BoolV")
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
          add (T.rel_sym id) (argsorts, result);
          (* {!Reflect}'s judgment reflection takes exactly the relation's INPUT
             arguments, in order (see [Reflect.gen_rel_holds]). *)
          add (T.holds_sym (T.rel_sym id)) (argsorts, "BoolV")
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
  (* [NumV] ({!num_sort}) is inhabited only through [sub_nat], so declare its
     edges only when the spec reaches that operator -- an unused edge would
     declare a sort no term of this module can have. *)
  let mentions_sub_nat =
    let rec go t =
      match t with
      | R.Var _ -> false
      | R.App (f, args) -> f = "sub_nat" || List.exists go args
    in
    List.exists
      (fun (r : R.rule) ->
        go r.R.lhs || go r.R.rhs
        || List.exists (fun (a, b) -> go a || go b) r.R.conds)
      rules
  in
  if mentions_sub_nat then
    subsorts := ("NatV", num_sort) :: ("IntV", num_sort) :: !subsorts;
  (* FILLS GAPS, never overrides a real declared type -- and never types a
     predicate, whose all-[Val] argument list here would be read as a declared
     seed and pin {!predicate_domains}' fixpoint at the top sort forever. Their
     range is [BoolV] by construction ({!signature}), their domain is recovered
     from use. *)
  let merge_gaps sigs =
    Hashtbl.iter
      (fun sym sg ->
        if (not (Hashtbl.mem tbl sym)) && not (is_predicate sym) then
          Hashtbl.replace tbl sym sg)
      sigs
  in
  merge_gaps (infer_ranges rules);
  merge_gaps (infer_proj_ranges tbl rules);
  (tbl, !subsorts)

(* The signature of [sym] at [arity]. A predicate ({!is_predicate}) always
   returns [BoolV]; its domain is whatever {!predicate_domains} left in [tbl]
   (the seed alone, if that pass never ran).

   The domain of a predicate cannot be its DECLARED type. [match_<T>_*]
   ({!Reflect.ensure_matchers}) discriminates only [T]'s own cases, but
   {!Reflect.sibling_guard} applies it to a subject known only at an OUTER
   type (e.g. [match_booleanLiteral_TRUE_0] on an [Expression]-sorted subject
   while reflecting [$name_expression]'s catch-all), and [subty_<T>] takes the
   source type of each [SubE] site, which is typically a supertype of [T]. At
   the narrow declared domain those applications sit at the ERROR kind, where
   no equation -- not even an [and]/[or] short-circuit -- can ever match, so
   the guard is permanently and silently stuck (commit 1874d212, found as a
   run-structural failure). Widening every predicate to [Val] was the blunt
   fix; {!predicate_domains} instead takes the JOIN of the subjects the rules
   actually pass, which is by construction wide enough to be well-sorted and
   as narrow as the spec allows. *)
let signature (tbl : sigs) (sym : string) (arity : int) : string list * string =
  match Hashtbl.find_opt tbl sym with
  | Some (args, res) when List.length args = arity ->
      (args, if is_predicate sym then "BoolV" else res)
  | _ ->
      ( List.init arity (fun _ -> val_sort),
        if is_predicate sym then "BoolV" else val_sort )

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
(* The declared argument sorts of an application (all-[Val] when the recovered
   signature disagrees with the arity actually used). *)
let arg_sorts (sg : string -> int -> string list * string) (f : string)
    (args : R.term list) : string list =
  let argsorts, _ = sg f (List.length args) in
  if List.length argsorts = List.length args then argsorts
  else List.map (fun _ -> val_sort) args

let infer_var_sorts edges (sg : string -> int -> string list * string)
    (hint : string -> string option) (r : R.rule) : (string, string) Hashtbl.t =
  let vs = Hashtbl.create 16 in
  (* A predicate application OBSERVES its subject, it does not declare it (its
     domain is the join of exactly these subjects, {!predicate_domains}). If it
     constrained, a variable the rule holds at an outer type would be silently
     narrowed to the nested type the matcher happens to be spelled for -- and
     [meet] keeps the narrower sort without a word -- changing which terms the
     rule's lhs pattern matches.

     But a variable that NOTHING else types (every other occurrence is a
     generic [Val] slot: a list/option element, a tuple component, an iteration
     helper's output) has nothing else to go on, and the predicate's domain is
     precisely the type it must have there. So remember the observation and,
     below, fall back to it -- without it, one such element position (e.g. the
     [cons] head of [subty_list_T]) drags the whole predicate's domain back up
     to [Val]. *)
  let observed : (string, string) Hashtbl.t = Hashtbl.create 8 in
  let observe v s =
    match Hashtbl.find_opt observed v with
    | None -> Hashtbl.replace observed v s
    | Some cur ->
        (* two predicates, two domains: only a common lower bound can be the
           variable's sort; unrelated domains say nothing (keep [Val]). *)
        Hashtbl.replace observed v
          (if is_sub edges s cur then s
           else if is_sub edges cur s then cur
           else val_sort)
  in
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
    | R.App (f, args) when is_predicate f ->
        List.iter2
          (fun s a ->
            (match a with R.Var v -> observe v s | _ -> ());
            go val_sort a)
          (arg_sorts sg f args) args
    | R.App (f, args) -> List.iter2 go (arg_sorts sg f args) args
  in
  (* The lhs head DEFINES its arguments (a definition site, not a use), so its
     top-level argument positions stay authoritative even for a predicate's own
     rules -- that is what types the [x] of [subty_T(x) -> subty_U(x)] and of
     [holds_R(x0, ..) -> or(..)], whose lhs pins no constructor. *)
  (match r.R.lhs with
  | R.App (f, args) -> List.iter2 go (arg_sorts sg f args) args
  | R.Var _ -> ());
  go (result_sort sg r.R.lhs) r.R.rhs;
  List.iter
    (fun (l, rr) ->
      go (result_sort sg l) l;
      go (result_sort sg l) rr)
    r.R.conds;
  Hashtbl.iter
    (fun v s ->
      let inferred = Option.value (Hashtbl.find_opt vs v) ~default:val_sort in
      if s <> val_sort && hint v = None && inferred = val_sort then
        Hashtbl.replace vs v s)
    observed;
  vs

(* -------------------------------------------------------------------------- *)
(* Predicate domains: the join of every subject the rules actually pass. *)

module SSet = Set.Make (String)

(* Whether the predicates keep the blunt [Val] domain (commit 1874d212) or the
   recovered join. [Wide] exists to bisect a regression back to this pass. *)
type predicate_mode = Narrow | Wide

(* The subsort edges as INFERENCE reads them: the injections recovered from the
   spec, plus the char-list/text bridge (the emitters declare [List < Text]
   whenever a [Text] position exists, and nothing is ever expected at [Text]
   when it does not). The [s < Val] edges the emitters also declare are
   redundant here -- {!is_sub} already treats [Val] as the top. *)
let inference_edges (inj : (string * string) list) : (string * string) list =
  inj @ [ ("List", "Text") ]

(* The declared IL types of a rule's variables ({!Var_hints}, keyed by the
   rule's defined symbol), as sorts. Authoritative over position inference. *)
let var_hint_fn (tenv : (string, deftyp') Hashtbl.t)
    (var_hints : (string, (string * typ') list) Hashtbl.t) (r : R.rule)
    (v : string) : string option =
  let hint_types =
    match R.defined_head r with
    | Some h -> Option.value (Hashtbl.find_opt var_hints h) ~default:[]
    | None -> []
  in
  Option.map (sort_of_typ tenv) (List.assoc_opt v hint_types)

(* Each sort's supersorts (itself and [Val] included), once. *)
let supersort_closure edges (sorts : string list) : (string, SSet.t) Hashtbl.t =
  let tbl = Hashtbl.create 512 in
  let rec up acc s =
    if SSet.mem s acc then acc
    else
      List.fold_left
        (fun acc (a, b) -> if a = s then up acc b else acc)
        (SSet.add s acc) edges
  in
  List.iter
    (fun s -> Hashtbl.replace tbl s (SSet.add val_sort (up SSet.empty s)))
    sorts;
  tbl

(* How many sorts each sort dominates -- the narrowness measure [lub] breaks
   ambiguous joins with. *)
let subsort_counts (sup : (string, SSet.t) Hashtbl.t) : (string, int) Hashtbl.t
    =
  let tbl = Hashtbl.create 512 in
  Hashtbl.iter
    (fun _ supers ->
      SSet.iter
        (fun c ->
          Hashtbl.replace tbl c
            (1 + Option.value (Hashtbl.find_opt tbl c) ~default:0))
        supers)
    sup;
  tbl

(* The narrowest sort above [ss].

   P4's unions overlap, so a join often does not exist: [BoolTypeIR] sits under
   [BaseTypeIR], [TypeIR] AND [TypedefIR] at once, and the sorts a [subty_] rule
   set observes have several incomparable minimal upper bounds. Answering [Val]
   there would throw the signature away over a tie -- yet EVERY common upper
   bound is a well-sorted domain by construction (it dominates every observed
   subject), so take a minimal one. Ties break on the narrowest, then the name,
   so the pick is deterministic. *)
let lub (sup : (string, SSet.t) Hashtbl.t) (below : (string, int) Hashtbl.t)
    (ss : string list) : string =
  let supers s =
    Option.value (Hashtbl.find_opt sup s)
      ~default:(SSet.of_list [ s; val_sort ])
  in
  let width c = Option.value (Hashtbl.find_opt below c) ~default:max_int in
  match dedup ss with
  | [] -> val_sort
  | [ s ] -> s
  | s0 :: rest ->
      let uppers =
        List.fold_left (fun acc s -> SSet.inter acc (supers s)) (supers s0) rest
      in
      let is_minimal c =
        not (SSet.exists (fun d -> d <> c && SSet.mem c (supers d)) uppers)
      in
      (* SSet.fold runs in name order and only a strictly narrower candidate
         displaces the incumbent, so equal-width minimals resolve by name. *)
      let best =
        SSet.fold
          (fun c acc ->
            if not (is_minimal c) then acc
            else
              match acc with
              | Some (w, _) when w <= width c -> acc
              | _ -> Some (width c, c))
          uppers None
      in
      Option.fold ~none:val_sort ~some:snd best

(* Recover each predicate's argument domain into [tbl] (its range is [BoolV] by
   construction, {!signature}).

   The domain is the JOIN of every subject the rules pass it -- both the
   constructors its own defining rules destructure (which for [subty_<T>]
   include the SOURCE type of each [SubE] site, not just [T]'s own cases) and
   the arguments at every call site (which for a [Reflect.sibling_guard]-emitted
   [match_<T>_<K>] can be an OUTER type). Since it is an upper bound of exactly
   the subjects that occur, every application stays well-sorted by construction
   -- the ERROR-kind stuck guard of 1874d212 cannot come back -- while the
   domain stays as narrow as the spec allows, which is what makes a sufficient-
   completeness verdict about the predicate mean anything.

   [rules] must be the WHOLE system: computing this on a slice would drop the
   call sites and shrink the domain to its seed, so a sliced module would
   declare a different (unsoundly narrow) signature than the one that runs. *)
let predicate_domains ~(mode : predicate_mode) ~(edges : (string * string) list)
    ~(hint : R.rule -> string -> string option) (tbl : sigs)
    (rules : R.rule list) : unit =
  let arity : (string, int) Hashtbl.t = Hashtbl.create 64 in
  let rec scan t =
    match t with
    | R.Var _ -> ()
    | R.App (f, args) ->
        if is_predicate f then Hashtbl.replace arity f (List.length args);
        List.iter scan args
  in
  let terms_of (r : R.rule) =
    r.R.lhs :: r.R.rhs :: List.concat_map (fun (a, b) -> [ a; b ]) r.R.conds
  in
  List.iter (fun r -> List.iter scan (terms_of r)) rules;
  match mode with
  | Wide ->
      Hashtbl.iter
        (fun f n ->
          Hashtbl.replace tbl f (List.init n (fun _ -> val_sort), "BoolV"))
        arity
  | Narrow ->
      let sorts =
        dedup
          (val_sort
          :: (Hashtbl.fold (fun _ (args, res) acc -> (res :: args) @ acc) tbl []
             @ List.concat_map (fun (a, b) -> [ a; b ]) edges))
      in
      let sup = supersort_closure edges sorts in
      let below = subsort_counts sup in
      let sg s n = signature tbl s n in
      (* per symbol, per argument index: every sort seen in that position *)
      let seen : (string, SSet.t array) Hashtbl.t = Hashtbl.create 64 in
      Hashtbl.iter
        (fun f n ->
          let seed =
            match Hashtbl.find_opt tbl f with
            | Some (args, _) when List.length args = n -> args
            | _ -> []
          in
          let a = Array.make n SSet.empty in
          List.iteri (fun i s -> a.(i) <- SSet.singleton s) seed;
          Hashtbl.replace seen f a)
        arity;
      let publish () =
        Hashtbl.iter
          (fun f a ->
            let dom =
              Array.to_list
                (Array.map (fun s -> lub sup below (SSet.elements s)) a)
            in
            Hashtbl.replace tbl f (dom, "BoolV"))
          seen
      in
      publish ();
      (* only the rules that mention a predicate can observe anything *)
      let observing =
        List.filter
          (fun r ->
            let hit = ref false in
            let rec go t =
              match t with
              | R.Var _ -> ()
              | R.App (f, args) ->
                  if is_predicate f then hit := true;
                  List.iter go args
            in
            List.iter go (terms_of r);
            !hit)
          rules
      in
      let changed = ref true in
      while !changed do
        changed := false;
        List.iter
          (fun (r : R.rule) ->
            let vs = infer_var_sorts edges sg (hint r) r in
            let sort_of t =
              match t with
              | R.Var v ->
                  Option.value (Hashtbl.find_opt vs v) ~default:val_sort
              | R.App _ -> result_sort sg t
            in
            let rec observe t =
              match t with
              | R.Var _ -> ()
              | R.App (f, args) ->
                  (match Hashtbl.find_opt seen f with
                  | Some a when Array.length a = List.length args ->
                      List.iteri
                        (fun i x ->
                          let s = sort_of x in
                          if not (SSet.mem s a.(i)) then (
                            a.(i) <- SSet.add s a.(i);
                            changed := true))
                        args
                  | _ -> ());
                  List.iter observe args
            in
            List.iter observe (terms_of r))
          observing;
        if !changed then publish ()
      done

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

(* The spec's own data constructors: every variant case and every struct
   constructor. Struct field accessors/updaters are NOT here -- they are defined
   symbols (equations rewrite them away), see {!il_declared_syms}. *)
let il_ctor_syms (orig : spec) : string list =
  List.concat_map
    (fun def ->
      match def.it with
      | TypD { deftyp = { it = VariantT typcases; _ }; _ } ->
          List.map
            (fun tc ->
              let origin, mixop = case_origin_mixop tc in
              T.variant_sym origin mixop)
            typcases
      | TypD { synid = t; deftyp = { it = StructT _; _ }; _ } ->
          [ T.struct_sym t.it ]
      | _ -> [])
    orig

(* Symbols that must be declared even if the spec's rules never mention them, so
   concrete start terms (built by a language encoder) can be formed: the spec's
   constructors, plus each struct's field accessors and updaters. *)
let il_declared_syms (orig : spec) : string list =
  il_ctor_syms orig
  @ List.concat_map
      (fun def ->
        match def.it with
        | TypD { synid = t; deftyp = { it = StructT fields; _ }; _ } ->
            List.concat_map
              (fun (a, _) -> [ T.field_sym t.it a; T.upd_field_sym t.it a ])
              fields
        | _ -> [])
      orig

(* The theory's own data constructors -- the symbols a normal form is BUILT from,
   as opposed to the defined symbols the rules compute away. Maude spells this
   split with the [ctor] operator attribute, and a tool that must know what a
   normal form looks like needs it: the Sufficient Completeness Checker asks
   whether every ground term reduces to a constructor term, which says nothing at
   all if the signature never says which symbols those are.

   Must track {!shared_op_sigs} and {!Prelude}: a constructor added there but
   missing here merely goes unmarked (a tool then reads it as defined --
   conservative, never unsound), whereas a DEFINED symbol listed here would be a
   false claim. {!To_mfe} guards against exactly that by re-checking this set
   against the rules' actual defined heads. *)
let shared_ctor_syms : string list =
  (* containers: lists (also the char-list spelling of a structural text),
     options, tuples -- the same constructors in either theory *)
  [ "nil"; "cons"; "none"; "some"; "tuple" ]

(* The scalar constructors, per theory -- the value half of {!scalar_ctor_sigs}.
   The split is not cosmetic: the sign-magnitude [int_pos]/[int_neg] BUILD an int
   in the structural theory, but in the native one they are bridges INTO Maude's
   built-in [int(_)] and carry equations, i.e. defined symbols there. Same for
   the binary-nat family, which the native theory replaces outright. *)
let scalar_ctor_syms : scalar_theory -> string list = function
  | Native ->
      [
        Maude_theory.bool_wrap_sym;
        Maude_theory.nat_wrap_sym;
        Maude_theory.int_wrap_sym;
        Maude_theory.text_wrap_sym;
      ]
  | Structural ->
      [
        "true";
        "false";
        (* binary nat, and the sign-magnitude wrappers that make it an int *)
        "bzero";
        "bone";
        "bd0";
        "bd1";
        "int_pos";
        "int_neg";
        (* the result sorts of the binary arithmetic: [bsub_mask]'s 3-valued
           truncated-subtraction mask, [bcompare]'s 3-valued comparison, and
           [bdivmod]'s quotient/remainder pair (its [bquot]/[brem] projections
           are defined symbols; this pairing constructor is not) *)
        "bmask_nul";
        "bmask_neg";
        "bmask_pos";
        "blt_kind";
        "beq_kind";
        "bgt_kind";
        "bdivmod";
      ]

(* Is [sym] a constructor of the module emitted for [scalars] and [orig]? The
   [chr_<n>] characters (the structural spelling of a text) are a constructor
   family recognized by shape: one member per byte value, generated on demand
   rather than declared anywhere. *)
let is_ctor (scalars : scalar_theory) (orig : spec) : string -> bool =
  let tbl = Hashtbl.create 512 in
  List.iter
    (fun s -> Hashtbl.replace tbl s ())
    (scalar_ctor_syms scalars @ shared_ctor_syms @ il_ctor_syms orig);
  fun sym -> Hashtbl.mem tbl sym || Option.is_some (T.chr_code_of_sym sym)

(* The [ctor] attribute for [sym]'s [op] declaration ([""] when it is a defined
   symbol), for an emitted module whose rules define [defined].

   Maude itself does not reduce differently for [ctor], but a tool that must know
   what a normal form looks like cannot work without it -- the Sufficient
   Completeness Checker asks precisely whether every ground term reduces to a
   constructor term, which says nothing at all if the signature never says which
   symbols those are.

   A nominal constructor that turns out to carry equations is not one: declare it
   plain rather than let the module state a falsehood, and say so -- a data
   constructor being rewritten away is a translation smell worth seeing, not
   worth silently papering over. *)
let ctor_attr (scalars : scalar_theory) (orig : spec) ~(defined : string list) :
    string -> string =
  let is_ctor = is_ctor scalars orig in
  fun sym ->
    if not (is_ctor sym) then ""
    else if List.mem sym defined then (
      prerr_endline
        ("maude_sorts: WARNING - constructor " ^ sym
       ^ " has defining equations; declaring it without [ctor]");
      "")
    else " [ctor]"

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
