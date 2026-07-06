open Common.Source
open Lang.Il
module R = Rewrite_system

(* Which scalar theory a scalar leaf is emitted in -- the one seam at which the
   analysis and Maude pipelines diverge. [Structural] keeps the self-contained
   Peano/sign-magnitude/char-list/own-bool scalars; [Native] emits Maude's
   built-in wrappers ([nat]/[int]/[bool]/[txt], see {!Maude_theory}) directly at
   the leaf, so the analysis and execution systems are produced by the {e same}
   translation with no separate fold pass. *)
type scalar_theory = Structural | Native

(** The structural CTRS vocabulary: the symbol-naming conventions and the smart
    term/rule constructors every rule is built through.

    This is the structural-scalar counterpart to {!Maude_theory} (the native
    built-in vocabulary): the one place raw {!Rewrite_system.App}/
    {!Rewrite_system.Var} construction is confined, so the prelude symbols a
    built rule references ([cons], [eq], [mem], …) match their definitions by
    name and a symbol's definition site and every use site agree. The
    translation ({!To_ctrs}) and the backends ({!Builtin}, {!To_maude},
    {!Of_maude}) all build over it. *)

(* -------------------------------------------------------------------------- *)
(* Symbol + builder layer. Raw [R.App]/[R.Var] construction is confined to this
   module; everything above builds terms through these helpers. *)

(* The CTRS-safe identifier scrub ({!Rewrite_system.sanitize}) lives at the data
   model so the Maude surfaces can share it; the symbol-naming conventions below
   build on it. *)

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

let sanitize_atom (a : Mixfix.atom) : string =
  R.sanitize (Xl.Atom.to_string a.it)

(* A mixop reduced to its atom spelling, e.g. the notation [`NUM %] -> "NUM". *)
let sanitize_mixop (mixop : mixop) : string =
  let atoms = Mixfix.atoms mixop in
  let s =
    String.concat "_" (List.map (fun a -> Xl.Atom.to_string a.it) atoms)
  in
  R.sanitize s

(* Symbol conventions -- must agree between the rule that defines a symbol and
   the rule that uses it. *)
(* Arity is folded into the symbol so two cases sharing the same atoms but a
   different number of arguments (e.g. the assignment [id `= expr] and the
   declaration [type id `= expr], both carrying only [`=]) stay distinct -- a
   CTRS function symbol must have a fixed arity. The arity is recoverable from
   the mixop at every site (notexp args, typcase nottyp, and [CaseP] pattern). *)
let variant_sym (origin : string) (mixop : mixop) : string =
  Printf.sprintf "variant_%s_%s_%d" (R.sanitize origin) (sanitize_mixop mixop)
    (Mixfix.arity mixop)

let match_sym (typ_name : string) (mixop : mixop) : string =
  Printf.sprintf "match_%s_%s_%d" (R.sanitize typ_name) (sanitize_mixop mixop)
    (Mixfix.arity mixop)

let struct_sym (typ_name : string) : string = "struct_" ^ R.sanitize typ_name

let field_sym (typ_name : string) (a : Mixfix.atom) : string =
  "field_" ^ R.sanitize typ_name ^ "_" ^ sanitize_atom a

let upd_field_sym (typ_name : string) (a : Mixfix.atom) : string =
  "upd_field_" ^ R.sanitize typ_name ^ "_" ^ sanitize_atom a

let subty_sym (typ_name : string) : string = "subty_" ^ R.sanitize typ_name
let func_sym (id : id) : string = "$" ^ R.sanitize id.it
let rel_sym (id : id) : string = R.sanitize id.it

(* Smart constructors. *)
let var_t (name : string) : R.term = R.Var name
let app_t (sym : string) (args : R.term list) : R.term = R.App (sym, args)
let true_t = app_t "true" []
let false_t = app_t "false" []

(* A boolean leaf in the current scalar theory: the structural [true]/[false]
   constructors, or the native [bool(..)] wrapper. *)
let bool_t ~scalars b =
  match scalars with
  | Structural -> if b then true_t else false_t
  | Native -> Maude_theory.bool_t b

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
(* Binary (Coq [positive]/[N]-style) magnitude encoding, added alongside the
   Peano nat family above as a separate, disjoint symbol set -- not yet wired
   into anything ([int_pos]/[int_neg] still wrap a Peano [NatV] magnitude; see
   the binary-encoding plan for the phase that retargets them). Unlike Coq,
   there is no separate zero-free [positive] SORT here (this codebase's
   signature-recovery layer, {!Maude_sorts}, supports only one declared
   signature per symbol name, so [bd0]/[bd1] cannot be typed to statically
   reject a zero argument the way Coq's [positive] does) -- [bzero]/[bone]/
   [bd0]/[bd1] all inhabit the single sort [BNatV], and canonicity (every
   [bd0]/[bd1] chain bottoms out at [bone], NEVER at [bzero] -- [bd0(bzero)]
   and [bd1(bzero)] would both be non-canonical duplicate spellings of an
   already-representable value, 0 and 1 respectively) is maintained BY
   CONSTRUCTION of every rule that builds a [BNatV] term: each rule in
   {!Prelude} is checked by hand that, given canonical operands, it only ever
   wraps [bd0]/[bd1] around an already-known-non-[bzero] subterm. Digit order
   matches Coq's [positive]: construction is MSB-first, recursion is
   LSB-first ([bd0 p] is [2 * val(p)], [bd1 p] is [2 * val(p) + 1]), which is
   what makes [bsucc]/[badd] structurally recursive and O(log n). *)
let bzero_t = app_t "bzero" []
let bone_t = app_t "bone" []
let bd0_t p = app_t "bd0" [ p ]
let bd1_t p = app_t "bd1" [ p ]

(* [bsucc]/[bpred] (Coq [Pos.succ]/[Pos.pred]/[Pos.pred_double]) and
   [bis_zero]. [bpred bzero] is deliberately left without a defining rule
   (stuck), the same partiality convention as nat [div]/[mod] by zero. *)
let bsucc_t a = app_t "bsucc" [ a ]
let bpred_t a = app_t "bpred" [ a ]
let bpred_double_t a = app_t "bpred_double" [ a ]
let bis_zero_t a = app_t "bis_zero" [ a ]

(* [badd]/[badd_carry] (Coq [Pos.add]/[Pos.add_carry]): a carry-threading
   mutually recursive pair, every clause disjoint on both arguments -- so the
   18-rule system is orthogonal (no critical pairs by construction). *)
let badd_t a b = app_t "badd" [ a; b ]
let badd_carry_t a b = app_t "badd_carry" [ a; b ]

(* [bsub]/[bsub_mask]/[bsub_mask_carry] (Coq [Pos.sub_mask]/[sub_mask_carry]):
   truncated (clamped to 0) subtraction via a 3-valued mask, mirroring how
   [add]/[add_carry] thread a carry. [bdouble_mask]/[bsucc_double_mask] (Coq
   [double_mask]/[succ_double_mask]) double a mask's magnitude (2x / 2x+1)
   while leaving [bmask_nul]/[bmask_neg] as-is (0 and "negative" both stay
   representationally fixed under doubling); [bsub_of_mask] reads a mask back
   to the truncated [BNatV] result ([bmask_nul]/[bmask_neg] both clamp to
   [bzero]). *)
let bmask_nul_t = app_t "bmask_nul" []
let bmask_neg_t = app_t "bmask_neg" []
let bmask_pos_t a = app_t "bmask_pos" [ a ]
let bsub_mask_t a b = app_t "bsub_mask" [ a; b ]
let bsub_mask_carry_t a b = app_t "bsub_mask_carry" [ a; b ]
let bdouble_mask_t a = app_t "bdouble_mask" [ a ]
let bsucc_double_mask_t a = app_t "bsucc_double_mask" [ a ]
let bsub_of_mask_t a = app_t "bsub_of_mask" [ a ]
let bsub_t a b = app_t "bsub" [ a; b ]

(* [bmul] (Coq [Pos.mul], double-and-add via [badd]). *)
let bmul_t a b = app_t "bmul" [ a; b ]

(* [bcompare]/[bleq]/[blt] (Coq [Pos.compare_cont]): threads the
   relation-so-far so differing bit-lengths compare without a separate
   length computation. *)
let blt_kind_t = app_t "blt_kind" []
let beq_kind_t = app_t "beq_kind" []
let bgt_kind_t = app_t "bgt_kind" []
let bcompare_cont_t r a b = app_t "bcompare_cont" [ r; a; b ]
let bcompare_t a b = app_t "bcompare" [ a; b ]

(* [bleq]/[blt] read off a [bcompare] result via a boolean-dispatch auxiliary
   over the three ground [Bcmp] constants -- the same disjoint-ground-pattern
   idiom as [div_aux]/[mod_aux] above, rather than compositing through
   [eq_t] (which is not, and need not be, defined over [Bcmp]). *)
let ble_of_cmp_t a = app_t "ble_of_cmp" [ a ]
let blt_of_cmp_t a = app_t "blt_of_cmp" [ a ]
let bleq_t a b = app_t "bleq" [ a; b ]
let blt_t a b = app_t "blt" [ a; b ]

(* [bdiv]/[bmod]: truncating binary long division -- O(log n) rewrite steps,
   NOT a transliteration of the nat family's repeated-subtraction [div_aux]/
   [mod_aux] (which stays O(x/y) steps regardless of term representation). *)
let bdiv_t a b = app_t "bdiv" [ a; b ]
let bmod_t a b = app_t "bmod" [ a; b ]

(* [bpow_nat]: binary-magnitude base, Peano [NatV] exponent -- the exponent is
   always a small bit-width variable, so reusing the untouched Peano family
   for it (rather than inventing a binary-exponent variant) needs no new
   machinery. Structurally identical to [pow_t] above, just producing/
   threading [BNatV]. *)
let bpow_nat_t a b = app_t "bpow_nat" [ a; b ]

(* Binary encoding of a non-negative OCaml int, O(log n) in both term size and
   construction time (mirrors [peano_of_int]'s role for the Peano family). *)
let rec binary_of_pos (n : int) : R.term =
  if n <= 1 then bone_t
  else if n land 1 = 0 then bd0_t (binary_of_pos (n / 2))
  else bd1_t (binary_of_pos (n / 2))

let binary_of_int (n : int) : R.term = if n <= 0 then bzero_t else binary_of_pos n

(* Same, from a [Bigint.t] magnitude (the P4 bit-width literals this encoding
   exists for can exceed native [int] range) -- O(log n) OCaml-side recursion,
   no term-size blowup at encode time. *)
let rec binary_of_bigint_pos (n : Bigint.t) : R.term =
  if Bigint.compare n Bigint.one <= 0 then bone_t
  else
    let half = Bigint.( / ) n (Bigint.of_int 2) in
    if Bigint.equal (Bigint.bit_and n Bigint.one) Bigint.zero then
      bd0_t (binary_of_bigint_pos half)
    else bd1_t (binary_of_bigint_pos half)

let binary_of_bigint (n : Bigint.t) : R.term =
  if Bigint.compare n Bigint.zero <= 0 then bzero_t else binary_of_bigint_pos n

(* A binary-magnitude nat literal in the current scalar theory: [Structural]
   builds the [bzero]/[bone]/[bd0]/[bd1] encoding above; [Native] is
   unaffected by this encoding choice (it always wraps a [Bigint] directly),
   so it is identical to [nat_lit]'s [Native] branch. *)
let bnat_lit ~scalars (i : int) : R.term =
  match scalars with
  | Structural -> binary_of_int i
  | Native -> Maude_theory.nat_t (Bigint.of_int i)

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

(* The structural char-list encoding of a text: a [cons]/[nil] list of its
   bytes, so the list rules ([len], [cat], [idx], [slice], [mem], [upd]) apply to
   strings unchanged and byte indexing matches the evaluator's [String.get]. *)
let chars_t (s : string) : R.term =
  List.fold_right
    (fun c acc -> cons_t (chr_t (Char.code c)) acc)
    (List.init (String.length s) (String.get s))
    nil_t

(* A text leaf in the current scalar theory. [Structural] is the char list; for
   [Native] a non-empty text is the [txt(..)] wrapper, but the EMPTY text stays
   the bare [nil] (the empty-text-as-[nil] convention {!To_maude}'s [eq]/[cat]
   nil-bridges depend on -- both modes agree the empty text is [nil]). *)
let text_t ~scalars (s : string) : R.term =
  match scalars with
  | Structural -> chars_t s
  | Native -> if s = "" then nil_t else Maude_theory.text_t s

let none_t = app_t "none" []
let some_t a = app_t "some" [ a ]
let tuple_t ts = app_t "tuple" ts
let variant_t origin mixop args = app_t (variant_sym origin mixop) args
let struct_t typ_name fields = app_t (struct_sym typ_name) fields

(* Peano encoding of a non-negative OCaml int. *)
let rec peano_of_int (n : int) : R.term =
  if n <= 0 then zero_t else succ_t (peano_of_int (n - 1))

(* A nat literal in the current scalar theory: structural Peano, or the native
   [nat(..)] wrapper. (The [peano_of_int]/[succ] literal uses in the generators
   go through this so they pick the right representation per mode.) *)
let nat_lit ~scalars (i : int) : R.term =
  match scalars with
  | Structural -> peano_of_int i
  | Native -> Maude_theory.nat_t (Bigint.of_int i)

(* An int literal in the current scalar theory: structural sign-magnitude over a
   Peano magnitude, or the native [int(..)] wrapper. *)
let int_lit ~scalars (i : int) : R.term =
  match scalars with
  | Structural ->
      if i >= 0 then int_pos_t (peano_of_int i)
      else int_neg_t (peano_of_int (-i - 1))
  | Native -> Maude_theory.int_t (Bigint.of_int i)

(* A numeric literal. [Structural]: a non-negative value is a nat magnitude
   ([peano]); a negative value is intrinsically an integer ([int_neg k] is
   [-(k+1)], so [-i] is [int_neg (i-1)]); a non-negative literal in an integer
   position is injected into [int_pos] at its surrounding cast (see
   [term_of_exp]). [Native]: the ground value goes straight into the [nat]/[int]
   wrapper (no Peano tower, so no [Bigint] overflow). *)
let term_of_num ~scalars (n : Xl.Num.t) : R.term =
  let i = Xl.Num.to_int n in
  match scalars with
  | Structural ->
      let i = Bigint.to_int_exn i in
      if i >= 0 then peano_of_int i else int_neg_t (peano_of_int (-i - 1))
  | Native ->
      if Bigint.compare i Bigint.zero >= 0 then Maude_theory.nat_t i
      else Maude_theory.int_t i

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

(* The byte alphabet of a rule set's text literals, read back from the
   [chr_<n>] constants the rules contain (texts translate to char lists, so
   every byte in play appears as such a constant). *)
let char_codes_of_rules (rules : R.rule list) : int list =
  List.concat_map R.refs_of_rule rules
  |> List.filter_map chr_code_of_sym
  |> List.sort_uniq compare

(* -------------------------------------------------------------------------- *)
(* Rule builders. *)

let rule lhs rhs : R.rule = { R.lhs; rhs; conds = []; owise = false }
let rule_cond lhs rhs conds : R.rule = { R.lhs; rhs; conds; owise = false }

(* Conjoin boolean terms with [and]; the empty conjunction is [true] (in the
   current scalar theory, since it can stand as a rule's whole boolean rhs). *)
let conj_t ~scalars (terms : R.term list) : R.term =
  match terms with
  | [] -> bool_t ~scalars true
  | first :: rest -> List.fold_left and_t first rest
