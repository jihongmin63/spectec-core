open Common.Source
open Lang.Il

(** Translation from elaborated + simplified IL into the COPS CTRS
    representation ({!Rewrite_system}).

    Two families of rules are produced:

    - {b prelude + type-derived definitions} ([defs_of_typ], [prelude]): Peano
      naturals ([zero]/[succ]) with [add]/[sub]/[mul]/[leq]/[lt], booleans with
      [not]/[and]/[or], list/option constructors with [len]/[cat], and -- from
      each [TypD] -- variant constructors, their matchers, struct field
      accessors, and subtype predicates. These give the symbols used below an
      actual rewriting semantics rather than leaving them opaque.

    - {b spec body rules} (from [DecD]/[RelD] of the simplified spec): function
      clauses and relation rules become (conditional) rewrite rules.

    The prelude/type-derived family is then pruned ([prune_unused]) down to the
    symbols reachable from the body rules, so a spec that never touches, say,
    multiplication or struct accessors does not carry their rules.

    Numeric operators dispatch on their [optyp]: the natural family ([add]/...)
    keeps the simple Peano rules over [zero]/[succ], while the integer family
    ([add_int]/...) works over a sign-magnitude form ([int_pos n] = [+n],
    [int_neg n] = [-(n+1)]) whose constructors are disjoint from nat's, so the
    two never collide (nat [eq]/[succ] never match an integer term). The nat/int
    casts the elaborator inserts are bridged with [int_pos]/[nat_of_int] (only
    for the built-in [nat]/[int], not named aliases; see todo). Equality ([eq])
    is structural, derived per type.

    Design notes / known approximations:
    - Value casts ([UpCastE]/[DownCastE]) are transparent for non-numeric types
      (a [literal] injected into [expr] keeps its origin-keyed
      [variant_literal_NUM], matching [interp.upcast]'s identity on variants);
      [Simplify] keeps only the nat<->int casts, which become [int_pos]/
      [nat_of_int] here (alias- and tuple-aware, see [Simplify.is_num_cast]).
    - [SubE] (`e <: T`) is the structural predicate [sub_pred]: scalars decide
      directly ([sub_nat] etc.), named types defer to a [subty_<T>] helper that
      recurses into the payload, tuples/iterations to [subty_tup]/[subty_list]/
      [subty_opt] helpers. Positive (`== true`) use only; non-members are left
      irreducible (a [-> false] totality awaits the negation story below).
    - [NeOp] and [IfNotHoldPr] are encoded as boolean equations
      ([... == false]), which Join semantics only approximates.
    - Iterations compile to auxiliary recursive helpers over the [cons]/[nil]
      (or [some]/[none]) spine (see the "Iteration helpers" section): [IterE] to
      a "map" helper in value position and to a bound variable plus [unzip]
      conditions in binder position; [IterPr] to an [$iterall] predicate when it
      binds nothing. When it iterates a single relation call (the "iterpr of a
      call" shape) it is an [$iterapply] map carrying the call result as the
      element -- one output binds the stream directly, several are split out
      with an [$iterproj] per output; any other binding premise falls back to a
      per-output conditional [$itercollect]. [Simplify] first collapses re-zip
      iterations to a single iterated variable, so many reduce to a plain list.
    - Division by zero diverges (the [div]/[mod] rules are partial), and the
      generated systems are not checked for termination or confluence.
    - [Mem]/[Idx]/[Slice]/[Upd] have list/text semantics over the [cons]/[nil]
      encoding (texts are byte lists); out-of-bounds access is left irreducible,
      like division by zero. [Upd]'s path is compiled statically into nested
      [idx]/[upd_idx]/[upd_field]/[upd_slice] applications, mirroring the
      evaluator. *)

module R = Rewrite_system

(* -------------------------------------------------------------------------- *)
(* Symbol + builder layer. Raw [R.App]/[R.Var] construction is confined to this
   section; everything below builds terms through these helpers. *)

(* A readable token for a non-alphanumeric character so symbolic notations keep
   distinct, legible names (e.g. [`+`] -> "plus", not the empty string). A prime
   ['] is kept as "prime" because it distinguishes sibling definitions ([f] vs
   [f'] vs [f'']) that would otherwise collide on the same symbol; backticks,
   double quotes and whitespace are dropped; truly unknown symbols become "sym". *)
let mnemonic_of_char (c : char) : string =
  match c with
  | '+' -> "plus"
  | '-' -> "minus"
  | '*' -> "star"
  | '/' -> "slash"
  | '\\' -> "backslash"
  | '<' -> "lt"
  | '>' -> "gt"
  | '=' -> "eq"
  | '!' -> "bang"
  | '?' -> "quest"
  | '&' -> "amp"
  | '|' -> "bar"
  | '^' -> "caret"
  | '~' -> "tilde"
  | '%' -> "percent"
  | '.' -> "dot"
  | ',' -> "comma"
  | ';' -> "semi"
  | ':' -> "colon"
  | '#' -> "hash"
  | '$' -> "dollar"
  | '@' -> "at"
  | '(' -> "lparen"
  | ')' -> "rparen"
  | '[' -> "lbrack"
  | ']' -> "rbrack"
  | '{' -> "lbrace"
  | '}' -> "rbrace"
  | '\'' -> "prime"
  | '`' | '"' | ' ' | '_' -> ""
  | _ -> "sym"

(* Scrub a string into a CTRS-safe identifier: maximal [A-Za-z0-9] runs are kept,
   every other character is replaced by a mnemonic token, tokens are joined with
   [_], an alphabetic lead is guaranteed, and the result is never empty. Distinct
   inputs may still collide (a known first-cut limitation). *)
let sanitize (s : string) : string =
  let is_alnum c =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
  in
  (* Accumulate completed tokens (reversed) plus the current alphanumeric run;
     [run] is committed to [tokens] whenever a non-alphanumeric breaks it. *)
  let commit run tokens = if run = "" then tokens else run :: tokens in
  let tokens, run =
    String.fold_left
      (fun (tokens, run) c ->
        if is_alnum c then (tokens, run ^ String.make 1 c)
        else
          match mnemonic_of_char c with
          | "" -> (commit run tokens, "")
          | m -> (m :: commit run tokens, ""))
      ([], "") s
  in
  let r = String.concat "_" (List.rev (commit run tokens)) in
  if r = "" then "anon"
  else if r.[0] >= '0' && r.[0] <= '9' then "c_" ^ r
  else r

(* Bound the length of a sanitized descriptor. The iteration and structural-
   subtype helpers fold a pretty-printed body/premise/type into their symbol to
   keep distinct helpers distinct; on real specs (p4) that descriptor grows to
   hundreds of characters -- long enough to break Maude's parser. Keep a readable
   leading prefix and append a short hash of the full descriptor, so two
   descriptors sharing that prefix still get distinct symbols (pure truncation
   would merge their helper rules -- a soundness hazard). *)
let abbrev (s : string) : string =
  let keep = 40 in
  if String.length s <= keep then s
  else
    let h = String.sub (Digest.to_hex (Digest.string s)) 0 8 in
    String.sub s 0 keep ^ "_" ^ h

let sanitize_atom (a : Mixfix.atom) : string = sanitize (Xl.Atom.to_string a.it)

(* A mixop reduced to its atom spelling, e.g. the notation [`NUM %] -> "NUM". *)
let sanitize_mixop (mixop : mixop) : string =
  let atoms = Mixfix.atoms mixop in
  let s =
    String.concat "_" (List.map (fun a -> Xl.Atom.to_string a.it) atoms)
  in
  sanitize s

(* Symbol conventions -- must agree between the rule that defines a symbol and
   the rule that uses it. *)
(* Arity is folded into the symbol so two cases sharing the same atoms but a
   different number of arguments (e.g. the assignment [id `= expr] and the
   declaration [type id `= expr], both carrying only [`=]) stay distinct -- a
   CTRS function symbol must have a fixed arity. The arity is recoverable from
   the mixop at every site (notexp args, typcase nottyp, and [CaseP] pattern). *)
let variant_sym (origin : string) (mixop : mixop) : string =
  Printf.sprintf "variant_%s_%s_%d" (sanitize origin) (sanitize_mixop mixop)
    (Mixfix.arity mixop)

let match_sym (typ_name : string) (mixop : mixop) : string =
  Printf.sprintf "match_%s_%s_%d" (sanitize typ_name) (sanitize_mixop mixop)
    (Mixfix.arity mixop)

let struct_sym (typ_name : string) : string = "struct_" ^ sanitize typ_name

let field_sym (typ_name : string) (a : Mixfix.atom) : string =
  "field_" ^ sanitize typ_name ^ "_" ^ sanitize_atom a

let upd_field_sym (typ_name : string) (a : Mixfix.atom) : string =
  "upd_field_" ^ sanitize typ_name ^ "_" ^ sanitize_atom a

let subty_sym (typ_name : string) : string = "subty_" ^ sanitize typ_name
let func_sym (id : id) : string = "$" ^ sanitize id.it
let rel_sym (id : id) : string = sanitize id.it

(* Smart constructors. *)
let var_t (name : string) : R.term = R.Var name
let app_t (sym : string) (args : R.term list) : R.term = R.App (sym, args)
let true_t = app_t "true" []
let false_t = app_t "false" []
let bool_t b = if b then true_t else false_t
let not_t a = app_t "not" [ a ]
let and_t a b = app_t "and" [ a; b ]
let or_t a b = app_t "or" [ a; b ]
let impl_t a b = app_t "impl" [ a; b ]
let equiv_t a b = app_t "equiv" [ a; b ]
let zero_t = app_t "zero" []
let succ_t a = app_t "succ" [ a ]

(* Natural-number operations (assume non-negative operands). *)
let add_t a b = app_t "add" [ a; b ]
let sub_t a b = app_t "sub" [ a; b ]
let mul_t a b = app_t "mul" [ a; b ]
let div_t a b = app_t "div" [ a; b ]
let mod_t a b = app_t "mod" [ a; b ]
let pow_t a b = app_t "pow" [ a; b ]
let leq_t a b = app_t "leq" [ a; b ]
let lt_t a b = app_t "lt" [ a; b ]

(* Integers in sign-magnitude form over nat magnitudes, with constructors
   disjoint from nat ([zero]/[succ]): [int_pos n] is [+n], [int_neg n] is
   [-(n+1)]. The representation is canonical by construction (no shared [succ]
   that could sit above a sign), so nat and int never collide -- in particular
   nat [eq]/[succ] never match an integer term. [sub_int_nat] is the signed
   difference of two nats; [abs_nat]/[nonneg_int] expose an int's magnitude and
   sign for [div_int]/[mod_int]; [nat_of_int] projects a known-nonneg int. *)
let int_pos_t n = app_t "int_pos" [ n ]
let int_neg_t n = app_t "int_neg" [ n ]
let negate_int_t a = app_t "negate_int" [ a ]
let abs_nat_t a = app_t "abs_nat" [ a ]
let nonneg_int_t a = app_t "nonneg_int" [ a ]
let sub_int_nat_t a b = app_t "sub_int_nat" [ a; b ]
let nat_of_int_t a = app_t "nat_of_int" [ a ]
let add_int_t a b = app_t "add_int" [ a; b ]
let sub_int_t a b = app_t "sub_int" [ a; b ]
let mul_int_t a b = app_t "mul_int" [ a; b ]
let div_int_t a b = app_t "div_int" [ a; b ]
let mod_int_t a b = app_t "mod_int" [ a; b ]
let pow_int_t a b = app_t "pow_int" [ a; b ]
let leq_int_t a b = app_t "leq_int" [ a; b ]
let lt_int_t a b = app_t "lt_int" [ a; b ]
let eq_t a b = app_t "eq" [ a; b ]
let nil_t = app_t "nil" []
let cons_t h t = app_t "cons" [ h; t ]
let cat_t a b = app_t "cat" [ a; b ]
let len_t a = app_t "len" [ a ]

(* List/text operations defined over the [cons]/[nil] encoding (texts are
   char lists, so the same symbols serve strings). [take]/[drop] back [slice];
   [upd_idx]/[upd_slice] back the [Upd] path compilation. *)
let mem_t a b = app_t "mem" [ a; b ]
let idx_t a b = app_t "idx" [ a; b ]
let slice_t a b c = app_t "slice" [ a; b; c ]
let take_t a b = app_t "take" [ a; b ]
let drop_t a b = app_t "drop" [ a; b ]
let upd_idx_t a i v = app_t "upd_idx" [ a; i; v ]
let upd_slice_t a i n v = app_t "upd_slice" [ a; i; n; v ]

(* A single character of a text literal, as a nullary constructor keyed by its
   Unicode codepoint. *)
let chr_sym (code : int) : string = Printf.sprintf "chr_%d" code
let chr_t (code : int) : R.term = app_t (chr_sym code) []

let chr_code_of_sym (sym : string) : int option =
  if String.starts_with ~prefix:"chr_" sym then
    int_of_string_opt (String.sub sym 4 (String.length sym - 4))
  else None

(* A text value is a [cons]/[nil] list of its bytes, so the list rules ([len],
   [cat], [idx], [slice], [mem], [upd]) apply to strings unchanged; byte
   indexing matches the evaluator's [String.get]. The one builder every text --
   a spec literal or a backend's program value -- encodes through, so the two
   can never diverge in shape. *)
let text_t (s : string) : R.term =
  List.fold_right
    (fun c acc -> cons_t (chr_t (Char.code c)) acc)
    (List.init (String.length s) (String.get s))
    nil_t

let none_t = app_t "none" []
let some_t a = app_t "some" [ a ]
let tuple_t ts = app_t "tuple" ts
let variant_t origin mixop args = app_t (variant_sym origin mixop) args
let struct_t typ_name fields = app_t (struct_sym typ_name) fields

(* Peano encoding of a non-negative OCaml int. *)
let rec peano_of_int (n : int) : R.term =
  if n <= 0 then zero_t else succ_t (peano_of_int (n - 1))

(* A numeric literal: a non-negative value is a nat magnitude ([peano]); a
   negative value is intrinsically an integer ([int_neg k] is [-(k+1)], so
   [-i] is [int_neg (i-1)]). A non-negative literal in an integer position is
   injected into [int_pos] at its surrounding cast (see [term_of_exp]). *)
let term_of_num (n : Xl.Num.t) : R.term =
  let i = Bigint.to_int_exn (Xl.Num.to_int n) in
  if i >= 0 then peano_of_int i else int_neg_t (peano_of_int (-i - 1))

(* Operator dispatch onto the prelude-defined symbols. The [optyp] selects the
   natural-number family ([add]/...) or the integer family ([add_int]/..., over
   the [int_pos]/[int_neg] sign-magnitude form); boolean and equality operators
   are type-agnostic. *)
let is_int (ty : optyp) : bool = ty = `IntT

let term_of_unop (op : unop) (ty : optyp) ~(operand_is_int : bool) (a : R.term)
    : R.term =
  match op with
  | `NotOp -> not_t a
  | `PlusOp -> a
  (* Negation lives in the integer family; a nat-magnitude operand is injected
     first. An operand that already denotes a signed integer must not be
     re-injected: the elaborator still types an inner unary minus as nat (so
     [ty] reads nat), yet [a] is already an [int] term -- e.g. the inner [-n] of
     [-(-n)] -- in which case [operand_is_int] guards against a double [int_pos]. *)
  | `MinusOp ->
      if is_int ty || operand_is_int then negate_int_t a
      else negate_int_t (int_pos_t a)

let term_of_binop (op : binop) (ty : optyp) (a : R.term) (b : R.term) : R.term =
  let arith on_nat on_int = (if is_int ty then on_int else on_nat) a b in
  match op with
  | `AddOp -> arith add_t add_int_t
  | `SubOp -> arith sub_t sub_int_t
  | `MulOp -> arith mul_t mul_int_t
  | `DivOp -> arith div_t div_int_t
  | `ModOp -> arith mod_t mod_int_t
  | `PowOp -> arith pow_t pow_int_t
  | `AndOp -> and_t a b
  | `OrOp -> or_t a b
  | `ImplOp -> impl_t a b
  | `EquivOp -> equiv_t a b

let term_of_cmpop (op : cmpop) (ty : optyp) (a : R.term) (b : R.term) : R.term =
  let leq = if is_int ty then leq_int_t else leq_t in
  let lt = if is_int ty then lt_int_t else lt_t in
  match op with
  | `EqOp -> eq_t a b
  | `NeOp -> not_t (eq_t a b)
  | `LtOp -> lt a b
  | `LeOp -> leq a b
  | `GtOp -> lt b a
  | `GeOp -> leq b a

(* -------------------------------------------------------------------------- *)
(* Helpers over IL types. *)

(* The named type a value carries in its note, if it is a [VarT]. *)
let typ_name_of (typ : typ') : string option =
  match typ with VarT { synid; _ } -> Some synid.it | _ -> None

(* Concrete numeric kind of a type. Only the built-in [NumT] is recognised
   (named aliases are not resolved here -- see the cast handling in
   [term_of_exp]), which is enough to bridge the nat/int representations at the
   casts the elaborator inserts between them. *)
let is_int_typ (typ : typ') : bool = typ = NumT `IntT
let is_nat_typ (typ : typ') : bool = typ = NumT `NatT

(* A nat-typed expression that already denotes an integer term, for which a
   surrounding nat->int cast is the identity rather than an [int_pos] injection.
   Unary minus is the only such case: the elaborator types [-n] over a nat
   operand as a nat, but it evaluates to [Int (-n)] (see [Num.un]), and
   [term_of_unop] already produces the signed integer term. *)
let yields_int (e : exp) : bool =
  match e.it with UnE (`MinusOp, _, _) -> true | _ -> false

(* Deterministic field variables [<prefix>0..<prefix>(n-1)] used in
   definition-rule LHSs. [eq] rules need two disjoint sets, hence the prefix. *)
let fresh_vars ?(prefix = "x") (n : int) : R.term list =
  List.init n (fun i -> var_t (Printf.sprintf "%s%d" prefix i))

(* Substitute variables by name throughout a term. Used to rename a helper
   body's element variables to the fresh per-step names its defining rule
   introduces. *)
let rec subst_term (pairs : (string * R.term) list) (t : R.term) : R.term =
  match t with
  | R.Var v -> ( match List.assoc_opt v pairs with Some t' -> t' | None -> t)
  | R.App (sym, ts) -> R.App (sym, List.map (subst_term pairs) ts)

(* -------------------------------------------------------------------------- *)
(* Iteration helpers ([IterE]/[IterPr]). An iteration recurses over one or more
   co-iterated lists/options in lock-step, so it compiles to an auxiliary
   recursive symbol that destructures the [cons]/[nil] (or [some]/[none])
   spines. The element and list level of an iteration variable share the same IL
   id (they differ only by iteration depth, which the term layer drops), so at a
   use site [Var id] denotes the list, while inside a defining rule the element
   is renamed to a fresh per-step variable. The symbol is a structural key of the
   iterated body/premise, so identical iterations share one definition; the
   call site and the definition derive symbol and argument order from the same
   functions below, keeping them in agreement. *)

module IdSet = Common.Domain.IdSet

let iter_tag (iter : iter) : string =
  match iter with List -> "list" | Opt -> "opt"

(* A deterministic symbol for an iteration helper: [base] (`$itermap`,
   `$unzip`, …), the iterated body/premise's pretty-printed descriptor, the
   iterator tag, then any discriminating [parts] (arity, binder name). The one
   place the descriptor is sanitized and length-bounded ([abbrev]), so every
   helper family stays Maude-parseable without each builder re-applying the
   bound. *)
let iter_helper_sym (base : string) (descr : string) (iter : iter)
    (parts : string list) : string =
  String.concat "_" (base :: abbrev (sanitize descr) :: iter_tag iter :: parts)

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

(* The same per-step renaming, but at the IL level and capture-aware: a STRUCTURED
   nested [IterE]/[IterPr] that RE-BINDS one of [ids] carries its own iteration
   over the FULL stream, so the per-step element must not be pushed inside it.
   (E.g. [$align_parameters]'s `(.. = $align_parameters'({ (id : parameterTypeIR)*
   }, parameterTypeIR, ..))*` re-iterates the whole `parameterTypeIR*` inside the
   map while the outer step binds one `parameterTypeIR` -- a binder-unaware
   [subst_term] over the compiled body would rename the map's stream argument to
   the single element, so the zip helper never matches.) A BARE iterated variable
   `x*`/`x?` is the element-list itself (it compiles to [x]), so it is renamed
   like a plain occurrence -- matching the old [subst_term] behaviour. Mirrors
   [Prem_env.subst_exp]'s capture avoidance for the structured case only; used in
   place of [elem_renaming] + [subst_term] where the body may contain such a
   re-binding. *)
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
   constants: [inner]'s free non-co-iterated variables ([captured_fvs]) PLUS any
   iteration-guiding [bound_ids] that [inner] also uses at full-stream depth --
   one re-bound by a structured nested iteration, e.g. [$align_parameters]'s
   `{ (id : parameterTypeIR)* }` map, which needs the WHOLE `parameterTypeIR*`
   even though the step also consumes its spine. Those survive [rename_step_prem]
   free (it leaves nested-rebound occurrences alone), so appending them passes the
   loop-invariant full stream alongside the consumed spine. Only [bound_ids] (the
   inputs) qualify -- a binding (output) is produced, never read as a full stream.
   The call site and the definition share this, so their argument lists agree. *)
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
   non-co-iterated variables PLUS any co-iterated [ids] the body still uses at
   full-stream depth -- one re-bound by a structured nested [IterE], e.g. the
   serializable-enum member rule, whose per-member varType embeds the WHOLE
   `(nameIR_field = value_field ;)*` member list (re-iterating `value_field*`)
   while the outer step consumes `value_field`'s spine. Those survive
   [rename_step_exp] free, so appending them passes the loop-invariant full
   stream alongside the consumed spine. The call site ([term_of_exp]) and the
   definition ([iter_map_def]) share this, so their argument lists agree. *)
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

(* The auxiliary symbol for an [IterE] (a "map" over the co-iterated lists). *)
let iter_map_sym (body : exp) (iter : iter) (n_lists : int) : string =
  iter_helper_sym "$itermap" (Print.string_of_exp body) iter
    [ string_of_int n_lists ]

(* -------------------------------------------------------------------------- *)
(* Subtype predicate. [SubE] (`e <: T`) is the runtime structural check
   [interp.subtyp]: a boolean term that dispatches on the target type and
   recurses. Scalars decide directly ([sub_nat] over the int/nat representation;
   any number is an [int]; a value is already its bool/text sort); a named type
   defers to its [subty_<T>] helper ([defs_of_typ]); tuples and iterations defer
   to structural [subty_tup]/[subty_list]/[subty_opt] helpers ([sub_helper_defs]).
   Positive use only: a non-member reduces to no rule (irreducible), which the
   [== true] guard needs; negative use awaits a negation story (see todo). *)

(* A structural-subtype helper symbol: the shape tag plus the type's
   pretty-printed descriptor, bounded the same way as [iter_helper_sym]'s. *)
let subty_helper_sym (shape : string) (t : typ') : string =
  Printf.sprintf "subty_%s_%s" shape
    (abbrev (sanitize (Print.string_of_typ (t $ no_region))))

let subty_tup_sym (ts : typ list) : string = subty_helper_sym "tup" (TupleT ts)
let subty_list_sym (elem : typ') : string = subty_helper_sym "list" elem
let subty_opt_sym (elem : typ') : string = subty_helper_sym "opt" elem

let sub_pred (t : typ') (x : R.term) : R.term =
  match t with
  | NumT `NatT -> app_t "sub_nat" [ x ]
  (* int (the widest number), bool, text, func: the static type already
     guarantees membership, so the check is trivially true. *)
  | NumT _ | BoolT | TextT | FuncT -> true_t
  | VarT { synid; _ } -> app_t (subty_sym synid.it) [ x ]
  | TupleT ts -> app_t (subty_tup_sym ts) [ x ]
  | IterT { typ = elem; iter = List } -> app_t (subty_list_sym elem.it) [ x ]
  | IterT { typ = elem; iter = Opt } -> app_t (subty_opt_sym elem.it) [ x ]

(* -------------------------------------------------------------------------- *)
(* Expressions -> terms. Placed ahead of the type pass, which reuses it; it has
   no dependency on the type pass itself. *)

let rec term_of_exp (e : exp) : R.term =
  match e.it with
  | VarE id -> var_t id.it
  | BoolE b -> bool_t b
  | NumE n -> term_of_num n
  | TextE s -> text_t s
  | UnE (op, ty, e1) ->
      term_of_unop op ty ~operand_is_int:(yields_int e1) (term_of_exp e1)
  | BinE (op, ty, e1, e2) ->
      term_of_binop op ty (term_of_exp e1) (term_of_exp e2)
  | CmpE (op, ty, e1, e2) ->
      term_of_cmpop op ty (term_of_exp e1) (term_of_exp e2)
  (* Casts are transparent (see header note) except across the nat/int
     boundary, where the two representations differ: a nat widened to int is
     injected with [int_pos], and an int narrowed to a known-nonneg nat is
     projected with [nat_of_int]. *)
  | UpCastE (t, e1)
    when is_int_typ t.it && is_nat_typ e1.note && not (yields_int e1) ->
      int_pos_t (term_of_exp e1)
  | DownCastE (t, e1) when is_nat_typ t.it && is_int_typ e1.note ->
      nat_of_int_t (term_of_exp e1)
  | UpCastE (_, e1) | DownCastE (_, e1) -> term_of_exp e1
  (* a `<:` in VALUE position (e.g. nested under an implication) is the
     boolean test itself, unlike the top-level premise form that
     [conds_of_prem] turns into a [== true] guard -- dropping it like a cast
     would silently replace the test with its subject *)
  | SubE (e1, t) -> sub_pred t.it (term_of_exp e1)
  | MatchE (e1, _) -> term_of_exp e1
  | TupleE es -> tuple_t (List.map term_of_exp es)
  | CaseE ne ->
      let args = List.map term_of_exp (Mixfix.args ne) in
      let mixop = Mixfix.to_mixop ne in
      let origin = Option.value (typ_name_of e.note) ~default:"anon" in
      variant_t origin mixop args
  | StrE fields ->
      let terms = List.map (fun (_, ef) -> term_of_exp ef) fields in
      let typ_name = Option.value (typ_name_of e.note) ~default:"anon" in
      struct_t typ_name terms
  | OptE None -> none_t
  | OptE (Some e1) -> some_t (term_of_exp e1)
  | ListE es ->
      List.fold_right (fun e acc -> cons_t (term_of_exp e) acc) es nil_t
  | ConsE (h, t) -> cons_t (term_of_exp h) (term_of_exp t)
  | CatE (a, b) -> cat_t (term_of_exp a) (term_of_exp b)
  | LenE e1 -> len_t (term_of_exp e1)
  | DotE (e1, a) ->
      let typ_name = Option.value (typ_name_of e1.note) ~default:"anon" in
      app_t (field_sym typ_name a) [ term_of_exp e1 ]
  | CallE (id, _, args) ->
      app_t (func_sym id) (List.filter_map term_of_arg args)
  (* List/text operations over the [cons]/[nil] encoding (texts are byte lists),
     backed by the prelude rules ([mem]; [idx]; [slice] via [take]/[drop]) and,
     for [Upd], the statically compiled path ([upd_of_path]). Out-of-bounds
     access is left irreducible -- partial, as with division by zero. *)
  | MemE (a, b) -> mem_t (term_of_exp a) (term_of_exp b)
  | IdxE (a, b) -> idx_t (term_of_exp a) (term_of_exp b)
  | SliceE (a, b, c) -> slice_t (term_of_exp a) (term_of_exp b) (term_of_exp c)
  | UpdE (a, path, b) -> upd_of_path (term_of_exp a) path (term_of_exp b)
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

and term_of_arg (a : arg) : R.term option =
  match a.it with ExpA e -> Some (term_of_exp e) | DefA _ -> None

(* [Upd]'s path is compiled statically, mirroring the evaluator's
   [eval_access_path]/[eval_update_path] (interp.ml): [access_of_path] reads the
   sub-term reached by a path, [upd_of_path] rebuilds the term with the leaf
   replaced by [v], from the inside out. A [DotP] node's struct type is the
   inner path's note (the type of the value being indexed into). *)
and access_of_path (base : R.term) (path : path) : R.term =
  match path.it with
  | RootP -> base
  | IdxP (p, i) -> idx_t (access_of_path base p) (term_of_exp i)
  | SliceP (p, i, n) ->
      slice_t (access_of_path base p) (term_of_exp i) (term_of_exp n)
  | DotP (p, a) ->
      let typ_name = Option.value (typ_name_of p.note) ~default:"anon" in
      app_t (field_sym typ_name a) [ access_of_path base p ]

and upd_of_path (base : R.term) (path : path) (v : R.term) : R.term =
  match path.it with
  | RootP -> v
  | IdxP (p, i) ->
      upd_of_path base p (upd_idx_t (access_of_path base p) (term_of_exp i) v)
  | SliceP (p, i, n) ->
      upd_of_path base p
        (upd_slice_t (access_of_path base p) (term_of_exp i) (term_of_exp n) v)
  | DotP (p, a) ->
      let typ_name = Option.value (typ_name_of p.note) ~default:"anon" in
      upd_of_path base p
        (app_t (upd_field_sym typ_name a) [ access_of_path base p; v ])

(* -------------------------------------------------------------------------- *)
(* Prelude + type-derived definition rules. *)

let rule lhs rhs : R.rule = { R.lhs; rhs; conds = []; owise = false }
let rule_cond lhs rhs conds : R.rule = { R.lhs; rhs; conds; owise = false }

(* Conjoin boolean terms with [and]; the empty conjunction is [true]. *)
let conj_t (terms : R.term list) : R.term =
  match terms with
  | [] -> true_t
  | first :: rest -> List.fold_left and_t first rest

(* Defining rules for an [IterE]'s "map" helper: recurse over the co-iterated
   spines in lock-step, rebuilding the collection from the body evaluated at each
   element. [List] folds with [cons]/[nil], [Opt] with [some]/[none]. The
   captured variables are carried through unchanged; each co-iterated variable is
   destructured and its element renamed to a fresh per-step name. Returns the
   helper symbol with its rules, or [None] for a bare iterated variable (no
   helper -- it is the list itself). *)
let iter_map_def (e : exp) : (string * R.rule list) option =
  match e.it with
  | IterE ({ it = VarE _; _ }, _) -> None
  | IterE (body, (iter, vars)) ->
      let ids = iter_var_ids vars in
      let fvs = iter_captured_exp body vars ids in
      let sym = iter_map_sym body iter (List.length ids) in
      let fv_terms = List.map var_t fvs in
      let body_elem = term_of_exp (rename_step_exp ids body) in
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
  iter_helper_sym "$unzip" (Print.string_of_exp body) iter [ sanitize v ]

(* Defining rules for the [unzip] helpers of an [IterE] used in binder position
   (a clause/rule head pattern): one helper per co-iterated variable, recursing
   over the list and matching each element against the iterated body to project
   that variable. A CTRS left-hand side must be a constructor pattern, so the
   iterated collection itself binds to a fresh variable (see [pattern_of_exp])
   and these helpers recover the element streams the body would have bound. The
   captured variables are constant across the iteration, so they are carried as
   leading parameters and matched in each element (a non-left-linear pattern,
   but faithful: every element does carry that same value). *)
let iter_unzip_defs (e : exp) : (string * R.rule list) list =
  match e.it with
  | IterE ({ it = VarE _; _ }, _) -> []
  | IterE (body, (iter, vars)) ->
      let fv_terms = List.map var_t (captured_fvs (Free.free_exp body) vars) in
      let ids = iter_var_ids vars in
      let elem_pat = subst_term (elem_renaming ids) (term_of_exp body) in
      let rest = var_t "__rest" in
      List.map
        (fun v ->
          let sym = unzip_sym body iter v in
          let collected = var_t (step_hd v) in
          let rules =
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
                  rule
                    (app_t sym (fv_terms @ [ some_t elem_pat ]))
                    (some_t collected);
                ]
          in
          (sym, rules))
        ids
  | _ -> []

(* Fixed prelude: booleans, Peano naturals and integers, lists/options, their
   matchers, and structural equality over the built-in sorts. Integers use a
   sign-magnitude form ([int_pos n] = [+n], [int_neg n] = [-(n+1)]) whose
   constructors are disjoint from the nat [zero]/[succ], so the two never
   collide; the natural family keeps the simpler bare-Peano rules. *)
let prelude : R.rule list =
  let x = var_t "x" and y = var_t "y" and xs = var_t "xs" and ys = var_t "ys" in
  let i = var_t "i" and n = var_t "n" and v = var_t "v" in
  [
    (* booleans *)
    rule (not_t true_t) false_t;
    rule (not_t false_t) true_t;
    rule (and_t true_t y) y;
    rule (and_t false_t y) false_t;
    rule (or_t true_t y) true_t;
    rule (or_t false_t y) y;
    rule (impl_t true_t y) y;
    rule (impl_t false_t y) true_t;
    rule (equiv_t true_t y) y;
    rule (equiv_t false_t y) (not_t y);
    (* naturals: add / sub (truncated) / mul / leq / lt *)
    rule (add_t zero_t y) y;
    rule (add_t (succ_t x) y) (succ_t (add_t x y));
    rule (sub_t x zero_t) x;
    rule (sub_t zero_t y) zero_t;
    rule (sub_t (succ_t x) (succ_t y)) (sub_t x y);
    rule (mul_t zero_t y) zero_t;
    rule (mul_t (succ_t x) y) (add_t y (mul_t x y));
    rule (leq_t zero_t y) true_t;
    rule (leq_t (succ_t x) zero_t) false_t;
    rule (leq_t (succ_t x) (succ_t y)) (leq_t x y);
    rule (lt_t x zero_t) false_t;
    rule (lt_t zero_t (succ_t y)) true_t;
    rule (lt_t (succ_t x) (succ_t y)) (lt_t x y);
    (* naturals: pow / div / mod (div by zero diverges -- partial) *)
    rule (pow_t x zero_t) (succ_t zero_t);
    rule (pow_t x (succ_t y)) (mul_t x (pow_t x y));
    rule_cond (div_t x y) zero_t [ (lt_t x y, true_t) ];
    rule_cond (div_t x y) (succ_t (div_t (sub_t x y) y)) [ (leq_t y x, true_t) ];
    rule_cond (mod_t x y) x [ (lt_t x y, true_t) ];
    rule_cond (mod_t x y) (mod_t (sub_t x y) y) [ (leq_t y x, true_t) ];
    (* integer helpers: negate / magnitude / sign / projection, and the signed
       difference of two nats ([sub_int_nat m n] = m - n as an int). All
       structural over [int_pos]/[int_neg], so they stay canonical. *)
    rule (negate_int_t (int_pos_t zero_t)) (int_pos_t zero_t);
    rule (negate_int_t (int_pos_t (succ_t x))) (int_neg_t x);
    rule (negate_int_t (int_neg_t x)) (int_pos_t (succ_t x));
    (* negation is involutive, so a double negation cancels even when the inner
       operand is still symbolic (e.g. the [negate_int(negate_int(int_pos(n)))]
       of [-(-n)]); the overlap with the structural rules above stays
       confluent. *)
    rule (negate_int_t (negate_int_t x)) x;
    rule (abs_nat_t (int_pos_t x)) x;
    rule (abs_nat_t (int_neg_t x)) (succ_t x);
    rule (nonneg_int_t (int_pos_t x)) true_t;
    rule (nonneg_int_t (int_neg_t x)) false_t;
    rule (nat_of_int_t (int_pos_t x)) x;
    rule (sub_int_nat_t zero_t zero_t) (int_pos_t zero_t);
    rule (sub_int_nat_t (succ_t x) zero_t) (int_pos_t (succ_t x));
    rule (sub_int_nat_t zero_t (succ_t y)) (int_neg_t y);
    rule (sub_int_nat_t (succ_t x) (succ_t y)) (sub_int_nat_t x y);
    (* integers: add (signed by both operands) / sub / mul *)
    rule (add_int_t (int_pos_t x) (int_pos_t y)) (int_pos_t (add_t x y));
    rule
      (add_int_t (int_neg_t x) (int_neg_t y))
      (int_neg_t (succ_t (add_t x y)));
    rule (add_int_t (int_pos_t x) (int_neg_t y)) (sub_int_nat_t x (succ_t y));
    rule (add_int_t (int_neg_t x) (int_pos_t y)) (sub_int_nat_t y (succ_t x));
    rule (sub_int_t x y) (add_int_t x (negate_int_t y));
    rule (mul_int_t (int_pos_t x) (int_pos_t y)) (int_pos_t (mul_t x y));
    rule
      (mul_int_t (int_neg_t x) (int_neg_t y))
      (int_pos_t (mul_t (succ_t x) (succ_t y)));
    rule
      (mul_int_t (int_pos_t x) (int_neg_t y))
      (negate_int_t (int_pos_t (mul_t x (succ_t y))));
    rule
      (mul_int_t (int_neg_t x) (int_pos_t y))
      (negate_int_t (int_pos_t (mul_t (succ_t x) y)));
    (* integers: leq / lt *)
    rule (leq_int_t (int_pos_t x) (int_pos_t y)) (leq_t x y);
    rule (leq_int_t (int_neg_t x) (int_neg_t y)) (leq_t y x);
    rule (leq_int_t (int_pos_t x) (int_neg_t y)) false_t;
    rule (leq_int_t (int_neg_t x) (int_pos_t y)) true_t;
    rule (lt_int_t x y) (not_t (leq_int_t y x));
    (* integers: pow (non-negative exponent), and div / mod by magnitudes + sign
       (truncate toward zero). The quotient is negative iff the operands' signs
       differ; the remainder takes the dividend's sign. *)
    rule (pow_int_t x (int_pos_t zero_t)) (int_pos_t (succ_t zero_t));
    rule
      (pow_int_t x (int_pos_t (succ_t y)))
      (mul_int_t x (pow_int_t x (int_pos_t y)));
    rule_cond (div_int_t x y)
      (int_pos_t (div_t (abs_nat_t x) (abs_nat_t y)))
      [ (eq_t (nonneg_int_t x) (nonneg_int_t y), true_t) ];
    rule_cond (div_int_t x y)
      (negate_int_t (int_pos_t (div_t (abs_nat_t x) (abs_nat_t y))))
      [ (eq_t (nonneg_int_t x) (nonneg_int_t y), false_t) ];
    rule_cond (mod_int_t x y)
      (int_pos_t (mod_t (abs_nat_t x) (abs_nat_t y)))
      [ (nonneg_int_t x, true_t) ];
    rule_cond (mod_int_t x y)
      (negate_int_t (int_pos_t (mod_t (abs_nat_t x) (abs_nat_t y))))
      [ (nonneg_int_t x, false_t) ];
    (* lists *)
    rule (len_t nil_t) zero_t;
    rule (len_t (cons_t x xs)) (succ_t (len_t xs));
    rule (cat_t nil_t ys) ys;
    rule (cat_t (cons_t x xs) ys) (cons_t x (cat_t xs ys));
    (* list/text operations: membership, indexing, slicing (via take/drop), and
       the positional updates backing the [Upd] path compilation. Out-of-bounds
       cases (e.g. [idx(nil, _)]) are left irreducible -- partial, as with div. *)
    rule (mem_t x nil_t) false_t;
    rule (mem_t x (cons_t y ys)) (or_t (eq_t x y) (mem_t x ys));
    rule (idx_t (cons_t x xs) zero_t) x;
    rule (idx_t (cons_t x xs) (succ_t i)) (idx_t xs i);
    rule (take_t xs zero_t) nil_t;
    rule (take_t (cons_t x xs) (succ_t n)) (cons_t x (take_t xs n));
    rule (drop_t xs zero_t) xs;
    rule (drop_t (cons_t x xs) (succ_t n)) (drop_t xs n);
    rule (slice_t xs i n) (take_t (drop_t xs i) n);
    rule (upd_idx_t (cons_t x xs) zero_t v) (cons_t v xs);
    rule (upd_idx_t (cons_t x xs) (succ_t i) v) (cons_t x (upd_idx_t xs i v));
    rule (upd_slice_t xs i n v)
      (cat_t (take_t xs i) (cat_t v (drop_t xs (add_t i n))));
    (* the [nat] membership predicate behind [e <: nat] ([sub_pred]): a
       non-negative integer ([int_pos]) and a bare nat ([zero]/[succ]) qualify, a
       negative integer ([int_neg]) does not -- mirroring [interp.subtyp]'s
       [NatT] case over both representations. *)
    rule (app_t "sub_nat" [ int_pos_t x ]) true_t;
    rule (app_t "sub_nat" [ int_neg_t x ]) false_t;
    rule (app_t "sub_nat" [ zero_t ]) true_t;
    rule (app_t "sub_nat" [ succ_t x ]) true_t;
    (* option / list matchers used by [conds_of_prems] *)
    rule (app_t "match_some" [ some_t x ]) true_t;
    rule (app_t "match_some" [ none_t ]) false_t;
    rule (app_t "match_none" [ none_t ]) true_t;
    rule (app_t "match_none" [ some_t x ]) false_t;
    rule (app_t "match_cons" [ cons_t x xs ]) true_t;
    rule (app_t "match_cons" [ nil_t ]) false_t;
    rule (app_t "match_nil" [ nil_t ]) true_t;
    rule (app_t "match_nil" [ cons_t x xs ]) false_t;
    (* structural equality over the built-in sorts. Nats ([zero]/[succ]) and
       integers ([int_pos]/[int_neg]) have disjoint constructors, so their rules
       never overlap and a nat rule can never match an integer term. *)
    rule (eq_t zero_t zero_t) true_t;
    rule (eq_t zero_t (succ_t y)) false_t;
    rule (eq_t (succ_t x) zero_t) false_t;
    rule (eq_t (succ_t x) (succ_t y)) (eq_t x y);
    rule (eq_t (int_pos_t x) (int_pos_t y)) (eq_t x y);
    rule (eq_t (int_neg_t x) (int_neg_t y)) (eq_t x y);
    rule (eq_t (int_pos_t x) (int_neg_t y)) false_t;
    rule (eq_t (int_neg_t x) (int_pos_t y)) false_t;
    rule (eq_t true_t true_t) true_t;
    rule (eq_t true_t false_t) false_t;
    rule (eq_t false_t true_t) false_t;
    rule (eq_t false_t false_t) true_t;
    rule (eq_t none_t none_t) true_t;
    rule (eq_t none_t (some_t y)) false_t;
    rule (eq_t (some_t x) none_t) false_t;
    rule (eq_t (some_t x) (some_t y)) (eq_t x y);
    rule (eq_t nil_t nil_t) true_t;
    rule (eq_t nil_t (cons_t y ys)) false_t;
    rule (eq_t (cons_t x xs) nil_t) false_t;
    rule (eq_t (cons_t x xs) (cons_t y ys)) (and_t (eq_t x y) (eq_t xs ys));
  ]

(* The prelude symbols whose defining rules encode the hand-written scalar
   theories (booleans, Peano nats, sign-magnitude ints) or recurse over Peano
   indices (the positional list operations). The Maude backend replaces their
   rules with one-line delegations to Maude's built-in Bool/Nat/Int/String
   ({!Maude_theory}); the analysis (COPS) pipeline keeps them. Must stay in
   sync with [prelude] above. *)
let native_replaced_heads : string list =
  [
    (* booleans *)
    "not";
    "and";
    "or";
    "impl";
    "equiv";
    (* Peano nat arithmetic *)
    "add";
    "sub";
    "mul";
    "div";
    "mod";
    "pow";
    "leq";
    "lt";
    (* sign-magnitude int helpers and arithmetic *)
    "negate_int";
    "abs_nat";
    "nonneg_int";
    "nat_of_int";
    "sub_int_nat";
    "add_int";
    "sub_int";
    "mul_int";
    "div_int";
    "mod_int";
    "pow_int";
    "leq_int";
    "lt_int";
    (* nat-membership predicate over both representations *)
    "sub_nat";
    (* list operations recursing over Peano indices *)
    "len";
    "idx";
    "take";
    "drop";
    "slice";
    "upd_idx";
    "upd_slice";
  ]

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
   when [name] is undefined or is not a single-case variant. A backend emitting
   rules over [set]/[pair]/[map] values reaches for this so it builds them with
   the exact symbol the spec's own [`{ }]/[`:] literals produce -- the names must
   agree, as [variant_set_lbrace_rbrace_1] does for both. *)
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
   generated symbol ([variant_sym]) is [case_sym] -- the same lookup as
   {!single_case_ctor} but selecting one case among several by the exact name
   the spec's own literals produce, so the two cannot drift apart. [None] when
   [name] is undefined or has no such case. *)
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

(* Definition rules contributed by one [TypD]. For a variant type [T]:
   - matcher: [match_<T>_<Ci>(variant_<Oi>_<Ci>(_...)) -> true] and, for every
     sibling case [Cj], [... -> false] (total and overlap-free over [T]'s cases);
   - subtype: [subty_<T>(variant_<Oi>_<Ci>(x...)) -> and(sub_pred field_k x_k)]
     for each case, recursing into the case's payload like [interp.subtyp]'s
     [subtyps] (nullary cases reduce to [true]); the elaborator has flattened
     injected subtype cases into [T] keeping their origin, so this covers the
     whole hierarchy; non-members are left irreducible (positive use only);
   - equality: [eq] on two same-case constructors recurses into their fields,
     and on two different cases is [false] (bounded to [T]'s cases -- [eq] across
     types is left irreducible, which well-typed specs never trigger).
   For a struct type [T]: a field accessor per field, structural [eq], and a
   trivially-true [subty_<T>] (structs are invariant in SpecTec). For a plain
   alias [T = U]: [subty_<T>] delegates to [U]'s check. *)
let defs_of_typ (def : def) : R.rule list =
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
            rule (app_t (match_sym t ci.mixop) [ con cj ]) (bool_t (i = j)))
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
              (conj_t (List.map2 (fun ft x -> sub_pred ft.it x) field_typs xs)))
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
                (conj_t (List.map2 eq_t xs ys))
            else rule (eq_t (con ci) (con ~prefix:"y" cj)) false_t)
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
          (conj_t (List.map2 eq_t xs ys))
      in
      (* structs are invariant: any value of the type is trivially a subtype. *)
      let subty_rule =
        rule
          (app_t (subty_sym t) [ app_t (struct_sym t) (fresh_vars n) ])
          true_t
      in
      accessor_rules @ updater_rules @ [ eq_rule; subty_rule ]
  (* A plain alias [syntax T = U]: its subtype check is [U]'s. *)
  | TypD { synid = tid; deftyp = { it = PlainT u; _ }; _ } ->
      [
        rule
          (app_t (subty_sym tid.it) [ var_t "x" ])
          (sub_pred u.it (var_t "x"));
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

let output_term (outs : R.term list) : R.term =
  match outs with [] -> true_t | [ t ] -> t | ts -> tuple_t ts

(* [Rel: id : notexp] as an invocation condition [Rel(inputs) == output]. *)
let rel_invocation (orig : spec) (id : id) (ne : notexp) : R.cond =
  let args = Mixfix.args ne in
  let in_args, out_args =
    match Prem_env.find_rel_in_spec orig id.it with
    | Some (_, inputs) -> split_inputs inputs args
    | None -> (args, [])
  in
  let in_terms = List.map term_of_exp in_args in
  let out_terms = List.map term_of_exp out_args in
  (app_t (rel_sym id) in_terms, output_term out_terms)

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
        match Prem_env.find_rel_in_spec orig id.it with
        | Some (_, inputs) -> split_inputs inputs args
        | None -> (args, [])
      in
      names_of out_args
  | IterPr (inner, _) -> prem_binder_names orig inner
  | IfPr _ | RelAssertPr { expect = false; _ } | ElsePr | DebugPr _ -> []

(* Split an iterated premise's variables into the (bound, binding) ids: bound
   guide the iteration, binding are produced and collected per step. *)
let iter_split (orig : spec) (inner : prem) (vars : var list) :
    string list * string list =
  let binders = prem_binder_names orig inner in
  List.partition (fun id -> not (List.mem id binders)) (iter_var_ids vars)

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
    [ string_of_int n_bound; sanitize b ]

(* When the iterated premise is a single relation call whose outputs are exactly
   the collected variables (bare vars), the iteration is a plain "map": the call
   result is the element, so the helper can carry it as a term in its rhs rather
   than re-deriving each output via a per-step condition (see [iter_apply_sym]).
   [$iterapply] returns that output stream; [$iterproj_b] projects one component
   when the call has several outputs (the stream is then a stream of tuples). *)
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
    [ string_of_int n_bound; sanitize b ]

(* [Some (call, out_vars)] when [inner] is a single relation call (the
   "iterpr(callpr)" shape) whose output positions are exactly the collected
   [binding_ids] as bare variables, so its iteration is a map of [call] yielding
   the [out_vars] stream(s). [out_vars] is the output variable order (one for a
   single output, the tuple component order for several). [None] for any other
   premise -- those keep the conditional [$itercollect]/[$iterall] form. *)
let iter_call_map (orig : spec) (inner : prem) (binding_ids : string list) :
    (R.term * string list) option =
  match inner.it with
  | RelPr { relid = id; notexp = ne }
  | RelAssertPr { call = { relid = id; notexp = ne }; expect = true } ->
      let call, out_pat = rel_invocation orig id ne in
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
   element streams (see [iter_unzip_defs]); a bare iterated variable is already a
   pattern. [fresh] supplies rule-unique names for the bound collections. *)
let rec pattern_of_exp (fresh : unit -> string) (e : exp) : R.term * R.cond list
    =
  let many es =
    let pairs = List.map (pattern_of_exp fresh) es in
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
      let th, ch = pattern_of_exp fresh h in
      let tt, ct = pattern_of_exp fresh t in
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
      let t, c = pattern_of_exp fresh e1 in
      (some_t t, c)
  (* Non-structural (or capture-carrying iterated) heads stay as plain terms. *)
  | _ -> (term_of_exp e, [])

let rec conds_of_prem (orig : spec) (fresh : unit -> string) (prem : prem) :
    R.cond list =
  match prem.it with
  (* The let's pattern is a binder position: compile it as a pattern so an
     iterated destructuring ([let struct _ { (typ id `;)* } = e]) unzips into the
     element streams, rather than re-zipping them with [$itermap] -- the latter
     is a function on the match's left, which Maude cannot run backwards, so the
     rule would never fire (every struct/header/serenum well-formedness rule). *)
  | LetPr (lhs, rhs) ->
      let pat, conds = pattern_of_exp fresh lhs in
      (term_of_exp rhs, pat) :: conds
  | IfPr { cond = { it = CmpE (`EqOp, _, a, b); _ }; _ } ->
      [ (term_of_exp a, term_of_exp b) ]
  | IfPr { cond = { it = CmpE (`NeOp, _, a, b); _ }; _ } ->
      [ (eq_t (term_of_exp a) (term_of_exp b), false_t) ]
  | IfPr { cond = { it = MatchE (e, pattern); _ }; _ } ->
      [ cond_of_match e pattern ]
  | IfPr { cond = { it = SubE (e, t); _ }; _ } ->
      [ (sub_pred t.it (term_of_exp e), true_t) ]
  | IfPr { cond; _ } -> [ (term_of_exp cond, true_t) ]
  | RelPr { relid = id; notexp = ne }
  | RelAssertPr { call = { relid = id; notexp = ne }; expect = true } ->
      [ rel_invocation orig id ne ]
  | RelAssertPr { call = { relid = id; notexp = ne }; expect = false } ->
      let lhs, _ = rel_invocation orig id ne in
      [ (lhs, false_t) ]
  (* An iterated premise becomes a call to its auxiliary helper (see
     [iterpr_defs]): a [$iterall(...) == true] check when it collects nothing; an
     [$iterapply(...) == b] map (single output) or an [$iterproj_b(...) == b] per
     output (several) when it iterates a single relation call; else an
     [$itercollect_b(...) == b] condition per collected list [b]. The captured
     variables then the bound (iteration-guiding) lists are the call arguments. *)
  | IterPr (inner, (iter, vars)) -> (
      let bound_ids, binding_ids = iter_split orig inner vars in
      let fvs = iter_captured inner vars bound_ids in
      let args = List.map var_t (fvs @ bound_ids) in
      let n = List.length bound_ids in
      if binding_ids = [] then
        [ (app_t (iter_all_sym inner iter n) args, true_t) ]
      else
        match iter_call_map orig inner binding_ids with
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

and cond_of_match (e : exp) (pattern : pattern) : R.cond =
  let subj = term_of_exp e in
  match pattern with
  | CaseP mixop ->
      let name = Option.value (typ_name_of e.note) ~default:"anon" in
      (app_t (match_sym name mixop) [ subj ], true_t)
  | OptP `Some -> (app_t "match_some" [ subj ], true_t)
  | OptP `None -> (app_t "match_none" [ subj ], true_t)
  | ListP `Cons -> (app_t "match_cons" [ subj ], true_t)
  | ListP `Nil -> (app_t "match_nil" [ subj ], true_t)
  | ListP (`Fixed n) -> (len_t subj, peano_of_int n)

let conds_of_prems (orig : spec) (fresh : unit -> string) (prems : prem list) :
    R.cond list =
  List.concat_map (conds_of_prem orig fresh) prems

(* Defining rules for an [IterPr]'s helper(s), the premise counterpart of
   [iter_map_def]: recurse over the bound (iteration-guiding) spines, requiring
   the inner premise's conditions at each step with the iteration variables
   renamed to their fresh per-step names. A pure check ([$iterall]) reduces to
   [true] only when every step's conditions hold; a collecting helper
   ([$itercollect_b]) rebuilds the list of each step's bound value [b]. *)
let iterpr_defs (orig : spec) (prem : prem) : (string * R.rule list) list =
  match prem.it with
  | IterPr (inner, (iter, vars)) -> (
      let bound_ids, binding_ids = iter_split orig inner vars in
      let fv_terms = List.map var_t (iter_captured inner vars bound_ids) in
      let inner_stepped = rename_step_prem (bound_ids @ binding_ids) inner in
      let conds = conds_of_prem orig (fresh_binder ()) inner_stepped in
      let base_args, step_args, rec_args = spine_args fv_terms bound_ids in
      let n = List.length bound_ids in
      let cons_step h t = cons_t (var_t h) (var_t t) in
      let some_step h _ = some_t (var_t h) in
      if binding_ids = [] then
        let sym = iter_all_sym inner iter n in
        let rules =
          match iter with
          | List ->
              [
                rule (app_t sym (base_args nil_t)) true_t;
                rule_cond
                  (app_t sym (step_args cons_step))
                  (app_t sym rec_args) conds;
              ]
          | Opt ->
              [
                rule (app_t sym (base_args none_t)) true_t;
                rule_cond (app_t sym (step_args some_step)) true_t conds;
              ]
        in
        [ (sym, rules) ]
      else
        match iter_call_map orig inner binding_ids with
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
                let rest = var_t "__rest" in
                List.map
                  (fun v ->
                    let sym = iter_proj_sym inner iter n v in
                    let collected = var_t (step_hd v) in
                    let rules =
                      match iter with
                      | List ->
                          [
                            rule (app_t sym [ nil_t ]) nil_t;
                            rule
                              (app_t sym [ cons_t tuple_pat rest ])
                              (cons_t collected (app_t sym [ rest ]));
                          ]
                      | Opt ->
                          [
                            rule (app_t sym [ none_t ]) none_t;
                            rule
                              (app_t sym [ some_t tuple_pat ])
                              (some_t collected);
                          ]
                    in
                    (sym, rules))
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
let iter_helper_defs (orig : spec) (spec : spec) : R.rule list =
  let defs = Helper_defs.create 32 in
  let add (sym, rules) = Helper_defs.add defs sym rules in
  let rec visit_exp (e : exp) =
    (match iter_map_def e with Some d -> add d | None -> ());
    List.iter add (iter_unzip_defs e);
    List.iter visit_exp (Exp_map.subexps e.it)
  in
  let rec visit_prem (p : prem) =
    match p.it with
    | IterPr (inner, _) ->
        List.iter add (iterpr_defs orig p);
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
   spec needs, deduplicated by symbol and pruned later. A target type's helper is
   generated once and recurses into its inner types (which [sub_pred] in the
   helper body will call). Seeded from every [SubE] target (the actual `<:`
   guards) and every type a [subty_<T>] definition recurses into -- a variant
   case's field types and a plain alias's underlying type -- so the field/element
   helpers those defs reference are present. The named [subty_<T>] helpers
   themselves come from [defs_of_typ]; scalars from the prelude. *)
let sub_helper_defs (orig : spec) (simplified : spec) : R.rule list =
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
    (* A [VarT] naming a type *parameter* (no [TypD]) is abstract: its [subty_]
       helper has no structural definition, so approximate the (positive) check
       as trivially true -- a well-typed value already has that type. Named
       types with a [TypD] are defined by [defs_of_typ] instead. *)
    | VarT { synid = tid; _ } ->
        let sym = subty_sym tid.it in
        if (not (Helper_defs.mem defs sym)) && not (has_typdef tid.it) then
          Helper_defs.add defs sym [ rule (app_t sym [ var_t "x" ]) true_t ]
    | TupleT ts ->
        let sym = subty_tup_sym ts in
        if not (Helper_defs.mem defs sym) then (
          let xs = fresh_vars (List.length ts) in
          Helper_defs.add defs sym
            [
              rule
                (app_t sym [ tuple_t xs ])
                (conj_t (List.map2 (fun ft x -> sub_pred ft.it x) ts xs));
            ];
          List.iter (fun ft -> require ft.it) ts)
    | IterT { typ = elem; iter = List } ->
        let sym = subty_list_sym elem.it in
        if not (Helper_defs.mem defs sym) then (
          let h = var_t "h" and t = var_t "t" in
          Helper_defs.add defs sym
            [
              rule (app_t sym [ nil_t ]) true_t;
              rule
                (app_t sym [ cons_t h t ])
                (and_t (sub_pred elem.it h) (app_t sym [ t ]));
            ];
          require elem.it)
    | IterT { typ = elem; iter = Opt } ->
        let sym = subty_opt_sym elem.it in
        if not (Helper_defs.mem defs sym) then (
          let v = var_t "v" in
          Helper_defs.add defs sym
            [
              rule (app_t sym [ none_t ]) true_t;
              rule (app_t sym [ some_t v ]) (sub_pred elem.it v);
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
  (* every [SubE] target: the `<:` guards (including under iteration) AND any
     `<:` nested in value position (e.g. under an implication), which
     [term_of_exp] compiles to the same [sub_pred] *)
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

(* -------------------------------------------------------------------------- *)
(* Spec body rules. *)

let pattern_of_arg (fresh : unit -> string) (a : arg) :
    R.term option * R.cond list =
  match a.it with
  | ExpA e ->
      let t, c = pattern_of_exp fresh e in
      (Some t, c)
  | DefA _ -> (None, [])

(* Rule-unique fresh names for the collections bound by iterated head patterns. *)
(* A clause/rule carries [-- otherwise] ([ElsePr]) when it should fire only if
   no earlier sibling did. [conds_of_prem] drops [ElsePr] (it contributes no
   condition); the flag is preserved here for {!To_maude} to emit as [owise]. *)
let has_otherwise (prems : prem list) : bool =
  List.exists (fun p -> match p.it with ElsePr -> true | _ -> false) prems

let rule_of_clause (orig : spec) (id : id) (clause : clause) : R.rule =
  let { args; body = exp; prems } = clause.it in
  let fresh = fresh_binder () in
  let arg_pairs = List.map (pattern_of_arg fresh) args in
  {
    R.lhs = app_t (func_sym id) (List.filter_map fst arg_pairs);
    rhs = term_of_exp exp;
    conds = List.concat_map snd arg_pairs @ conds_of_prems orig fresh prems;
    owise = has_otherwise prems;
  }

let rule_of_rel_rule (orig : spec) (id : id) (inputs : int list) (rl : rule) :
    R.rule =
  let { concl = ne; prems; _ } = rl.it in
  let args = Mixfix.args ne in
  let in_args, out_args = split_inputs inputs args in
  let fresh = fresh_binder () in
  let in_pairs = List.map (pattern_of_exp fresh) in_args in
  {
    R.lhs = app_t (rel_sym id) (List.map fst in_pairs);
    rhs = output_term (List.map term_of_exp out_args);
    conds = List.concat_map snd in_pairs @ conds_of_prems orig fresh prems;
    owise = has_otherwise prems;
  }

let rules_of_def (orig : spec) (def : def) : R.rule list =
  match def.it with
  | DecD { defid = id; clauses; _ } -> List.map (rule_of_clause orig id) clauses
  | RelD { relid = id; reltyp; rules } ->
      let n = List.length (Mixfix.args (Mode.notation reltyp.it)) in
      let inputs, _ = Mode.partition reltyp.it (List.init n Fun.id) in
      List.map (rule_of_rel_rule orig id inputs) rules
  | TypD _ | BuiltinDecD _ -> []

(* -------------------------------------------------------------------------- *)
(* Variable type hints. The CTRS term layer keeps a variable's name but drops
   its IL type; the COPS/TPDB surfaces never needed it. A typed backend
   ({!To_maude}) does: a relation argument's declared type widens its variables
   (e.g. [Eval_expr]'s subject is [expr], but the "id" rule's [x] is really an
   [id]), and recovering the narrower declared type is what keeps such a rule
   from overlapping its siblings. The type is still on each [VarE] note in the
   simplified spec, so collect it per defined symbol. *)

(* Every [(variable name, IL type)] occurring in [e], descending through casts
   and all sub-expressions. A bare iterated variable [x*]/[x?] ([IterE] over a
   lone [VarE]) compiles to the list/option [x] itself ([term_of_exp]), so its
   CTRS type is the ITERATED type carried on the [IterE] note, not the element
   type on the inner [VarE]; record the former and do not descend (descending
   would record the element type, clashing and dropping the hint -- which then
   strands a list-recursive rule's tail at the element sort, e.g. a [cons] tail
   typed [TypeArgument] instead of [List], so [nil] cannot bind it). *)
let rec collect_var_types (acc : (string * typ') list) (e : exp) :
    (string * typ') list =
  match e.it with
  | IterE ({ it = VarE id; _ }, _) -> (id.it, e.note) :: acc
  (* A structured iterated body (e.g. a head destructure [(typ id `;)*]) compiles
     to helpers ([$unzip]/[$itermap]) that bind each co-iterated variable to its
     list-level stream, so the variable's CTRS type is the ITERATED type built
     from its own [iter list], not the element type its [VarE] carries inside the
     body. Record those streams and collect only the body's OTHER (captured)
     variables -- descending unfiltered would mistype a stream as its element
     (the same hazard the bare-[VarE] case above avoids), stranding e.g. a [cons]
     tail at the element sort so [nil] cannot bind it. *)
  | IterE (body, (iter, vars)) ->
      let bound = List.map (fun ({ varid; _ } : var) -> varid.it) vars in
      (* the variable iterates under this [IterE]'s [iter] (its own [iters] are
         further, inner nestings), so wrap the element type in both *)
      let stream_typ (typ : typ) (iters : iter list) : typ' =
        let inner =
          List.fold_left
            (fun acc it -> IterT { typ = acc $ typ.at; iter = it })
            typ.it iters
        in
        IterT { typ = inner $ typ.at; iter }
      in
      let streams =
        List.map
          (fun ({ varid; typ; iters } : var) ->
            (varid.it, stream_typ typ iters))
          vars
      in
      let body_others =
        List.filter
          (fun (v, _) -> not (List.mem v bound))
          (collect_var_types [] body)
      in
      streams @ body_others @ acc
  | _ ->
      let acc =
        match e.it with VarE id -> (id.it, e.note) :: acc | _ -> acc
      in
      List.fold_left collect_var_types acc (Exp_map.subexps e.it)

(* The premise-level counterpart of [collect_var_types]'s [IterE] branch: an
   [IterPr]'s co-iterated variables are bound at their STREAM type by the
   compiled [$itercollect]/[$iterall] helpers (the rule's condition binds e.g.
   [_expressionIR] to the whole projected list), so descending transparently
   into the inner premise would record the element type and missort the bound
   stream (a [List]-valued condition output typed [ExpressionIR] can never
   match). Record the streams; collect inside only for the captured rest. *)
let rec collect_prem_var_types (acc : (string * typ') list) (p : prem) :
    (string * typ') list =
  match p.it with
  | IterPr (inner, (iter, vars)) ->
      let bound = List.map (fun ({ varid; _ } : var) -> varid.it) vars in
      let stream_typ (typ : typ) (iters : iter list) : typ' =
        let wrapped =
          List.fold_left
            (fun acc it -> IterT { typ = acc $ typ.at; iter = it })
            typ.it iters
        in
        IterT { typ = wrapped $ typ.at; iter }
      in
      let streams =
        List.map
          (fun ({ varid; typ; iters } : var) ->
            (varid.it, stream_typ typ iters))
          vars
      in
      let inner_others =
        List.filter
          (fun (v, _) -> not (List.mem v bound))
          (collect_prem_var_types [] inner)
      in
      streams @ inner_others @ acc
  | _ -> List.fold_left collect_var_types acc (Exp_map.exps_of_prem p)

(* Keep a variable's type only when all its occurrences agree; an unresolved
   clash drops it (the backend then falls back to its own inference). *)
let resolve_var_types (vts : (string * typ') list) : (string * typ') list =
  let seen = Hashtbl.create 16 in
  List.iter
    (fun (v, t) ->
      match Hashtbl.find_opt seen v with
      | None -> Hashtbl.replace seen v (Some t)
      | Some (Some t') when t' = t -> ()
      | Some _ -> Hashtbl.replace seen v None)
    vts;
  Hashtbl.fold
    (fun v t acc -> match t with Some t -> (v, t) :: acc | None -> acc)
    seen []

(* Per defined symbol ([func_sym]/[rel_sym]), the IL type of each variable in
   that function's clauses or relation's rules. Keyed by symbol (not by rule) so
   it needs no rule-ordering assumption; the prelude's reuse of names like [x]
   never collides because those rules define different symbols. *)
let var_type_hints (spec : spec) : (string, (string * typ') list) Hashtbl.t =
  let tbl = Hashtbl.create 64 in
  let of_prems acc prems = List.fold_left collect_prem_var_types acc prems in
  List.iter
    (fun def ->
      match def.it with
      | DecD { defid = id; clauses; _ } ->
          let vts =
            List.fold_left
              (fun acc clause ->
                let { args; body = exp; prems } = clause.it in
                let acc =
                  List.fold_left
                    (fun acc a ->
                      match a.it with
                      | ExpA e -> collect_var_types acc e
                      | DefA _ -> acc)
                    acc args
                in
                of_prems (collect_var_types acc exp) prems)
              [] clauses
          in
          Hashtbl.replace tbl (func_sym id) (resolve_var_types vts)
      | RelD { relid = id; rules; _ } ->
          let vts =
            List.fold_left
              (fun acc rl ->
                let { concl = ne; prems; _ } = rl.it in
                of_prems
                  (List.fold_left collect_var_types acc (Mixfix.args ne))
                  prems)
              [] rules
          in
          Hashtbl.replace tbl (rel_sym id) (resolve_var_types vts)
      | TypD _ | BuiltinDecD _ -> ())
    spec;
  tbl

(* -------------------------------------------------------------------------- *)
(* Top level. *)

(* Drop the prelude/type-derived rules in [defs] whose defined symbol is never
   reached from the actual [body] rules. Reachability is transitive: keeping a
   symbol pulls in everything the rules defining it reference (e.g. reaching
   [mul] keeps the [add] its rhs calls). Constructors have no defining rules, so
   only operations/matchers/accessors/subtype predicates are ever pruned; the
   removed rules are unreachable, so term rewriting is unaffected. *)
let prune_unused (defs : R.rule list) (body : R.rule list) : R.rule list =
  let roots = List.concat_map R.refs_of_rule body in
  let reachable = R.reachable_heads ~roots defs in
  List.filter
    (fun r ->
      match R.defined_head r with
      | Some head -> Hashtbl.mem reachable head
      | None -> false)
    defs

(* The byte alphabet of the spec's text literals, read back from the [chr_<n>]
   constants its rules contain (texts translate to char lists, so every byte in
   play already appears as such a constant). *)
let char_codes_of_rules (rules : R.rule list) : int list =
  List.concat_map R.refs_of_rule rules
  |> List.filter_map chr_code_of_sym
  |> List.sort_uniq compare

(* Structural equality over text bytes: [eq] decides every pair drawn from the
   spec's alphabet (true on the diagonal, false off it). *)
let char_eq_rules (codes : int list) : R.rule list =
  List.concat_map
    (fun c ->
      List.map (fun d -> rule (eq_t (chr_t c) (chr_t d)) (bool_t (c = d))) codes)
    codes

let of_spec ?(extra_defs = []) ~(orig : spec) (simplified : spec) : R.t =
  let type_rules = prelude @ List.concat_map defs_of_typ orig in
  let body_rules = List.concat_map (rules_of_def orig) simplified in
  let iter_rules = iter_helper_defs orig simplified in
  let sub_rules = sub_helper_defs orig simplified in
  (* [extra_defs] (builtin rules) can introduce text bytes of their own -- e.g.
     [int_to_text] emits the digit/sign chars [chr_45]/[chr_48..57] -- so scan
     them too, or a produced text could meet a [chr] with no equality rule. *)
  let char_rules =
    char_eq_rules (char_codes_of_rules (type_rules @ body_rules @ extra_defs))
  in
  let type_rules =
    prune_unused
      (type_rules @ char_rules @ iter_rules @ sub_rules @ extra_defs)
      body_rules
  in
  let rules = type_rules @ body_rules in
  let vars = R.dedup_stable (List.concat_map R.vars_of_rule rules) in
  { R.ctype = R.Join; vars; rules; comment = None }

(* The slice roots: the symbol each top-level function/relation defines, in spec
   order. Confluence can then be checked one symbol's dependency closure at a
   time via [Rewrite_system.slice]. *)
let def_symbols (spec : spec) : string list =
  List.filter_map
    (fun def ->
      match def.it with
      | DecD { defid = id; _ } -> Some (func_sym id)
      | RelD { relid = id; _ } -> Some (rel_sym id)
      | TypD _ | BuiltinDecD _ -> None)
    spec

(* Relations that declare a non-empty input mode (`hint(input ...)`): their
   inputs determine their outputs, so they are functional and may be emitted as
   Maude equations rather than rules. A relation with no declared input mode has
   every position as an output and is left as a rule. *)
let input_moded_rel_syms (spec : spec) : string list =
  List.filter_map
    (fun def ->
      match def.it with
      | RelD { relid = id; reltyp; _ } when Mode.inputs reltyp.it <> [] ->
          Some (rel_sym id)
      | _ -> None)
    spec
