module R = Rewrite_system
module T = Ctrs_term
open Lang.Il

(* Shared list helpers used by more than one set builtin, named for what they
   decide rather than the builtin that reaches them. Emitted once and pruned
   when unused (the spec has no collection builtin). All three recurse over the
   prelude [cons]/[nil] lists and reuse the prelude [mem]. *)
let list_diff_sym = "$builtin_list_diff"
let list_inter_sym = "$builtin_list_inter"
let list_submem_sym = "$builtin_list_submem"

(* [list_diff xs ys] keeps the elements of [xs] absent from [ys]; [list_inter]
   keeps those present; [list_submem] decides whether every element of [xs] is in
   [ys]. The membership branches mirror the prelude's conditional [div]/[mod]
   shape (complementary [mem == true]/[mem == false] guards). *)
let shared_list_defs ~scalars : R.rule list =
  let x = T.var_t "x" and xs = T.var_t "xs" and ys = T.var_t "ys" in
  [
    T.rule (T.app_t list_diff_sym [ T.nil_t; ys ]) T.nil_t;
    T.rule_cond
      (T.app_t list_diff_sym [ T.cons_t x xs; ys ])
      (T.app_t list_diff_sym [ xs; ys ])
      [ (T.mem_t x ys, T.bool_t ~scalars true) ];
    T.rule_cond
      (T.app_t list_diff_sym [ T.cons_t x xs; ys ])
      (T.cons_t x (T.app_t list_diff_sym [ xs; ys ]))
      [ (T.mem_t x ys, T.bool_t ~scalars false) ];
    T.rule (T.app_t list_inter_sym [ T.nil_t; ys ]) T.nil_t;
    T.rule_cond
      (T.app_t list_inter_sym [ T.cons_t x xs; ys ])
      (T.cons_t x (T.app_t list_inter_sym [ xs; ys ]))
      [ (T.mem_t x ys, T.bool_t ~scalars true) ];
    T.rule_cond
      (T.app_t list_inter_sym [ T.cons_t x xs; ys ])
      (T.app_t list_inter_sym [ xs; ys ])
      [ (T.mem_t x ys, T.bool_t ~scalars false) ];
    T.rule (T.app_t list_submem_sym [ T.nil_t; ys ]) (T.bool_t ~scalars true);
    T.rule
      (T.app_t list_submem_sym [ T.cons_t x xs; ys ])
      (T.and_t (T.mem_t x ys) (T.app_t list_submem_sym [ xs; ys ]));
  ]

(* The CTRS rules implementing one collection builtin [id] (map/set/list/text),
   the structural recursion the interpreter's OCaml does ([targets/p4/builtins/]).
   A [`$<id>_<suffix>] symbol names each builtin-private recursive helper. The
   map/set builtins need the [set]/[pair] constructors the spec defines; when
   either is absent ([orig] has no such type), the builtin emits no rule -- the
   spec cannot have built a value to reduce anyway. An [id] outside this table
   (numeric/naming builtins, still unsupported) emits no rule. *)
let rules_of_builtin ~scalars (orig : spec) (id : id) : R.rule list =
  let sym = T.func_sym id in
  let helper suffix = sym ^ "_" ^ suffix in
  (* the symbol of a sibling builtin this one delegates to (e.g. [adds_map] folds
     through [add_map]); built with the same [func_sym] so the names agree. *)
  let builtin name = T.func_sym { id with it = name } in
  let set_ctor = To_ctrs.single_case_ctor orig "set" in
  let pair_ctor = To_ctrs.single_case_ctor orig "pair" in
  let k = T.var_t "k" and v = T.var_t "v" in
  let k2 = T.var_t "k2" and v2 = T.var_t "v2" in
  let m = T.var_t "m" and ps = T.var_t "ps" and rest = T.var_t "rest" in
  let a = T.var_t "a" and b = T.var_t "b" and s = T.var_t "s" in
  let ks = T.var_t "ks" and vs = T.var_t "vs" in
  let x = T.var_t "x" and xs = T.var_t "xs" in
  let n = T.var_t "n" and t = T.var_t "t" and p = T.var_t "p" in
  match (id.it, set_ctor, pair_ctor) with
  (* ----- maps (a [set] of [pair]s) ----- *)
  | "find_map", Some set, Some pair ->
      let walk = helper "walk" in
      [
        T.rule (T.app_t sym [ set [ ps ]; k ]) (T.app_t walk [ k; ps ]);
        T.rule (T.app_t walk [ k; T.nil_t ]) T.none_t;
        T.rule_cond
          (T.app_t walk [ k; T.cons_t (pair [ k2; v2 ]) rest ])
          (T.some_t v2)
          [ (T.eq_t k k2, T.bool_t ~scalars true) ];
        T.rule_cond
          (T.app_t walk [ k; T.cons_t (pair [ k2; v2 ]) rest ])
          (T.app_t walk [ k; rest ])
          [ (T.eq_t k k2, T.bool_t ~scalars false) ];
      ]
  | "find_maps", Some _, _ ->
      (* the first map (in list order) that has the key *)
      let pick = helper "pick" in
      [
        T.rule (T.app_t sym [ T.nil_t; k ]) T.none_t;
        T.rule
          (T.app_t sym [ T.cons_t m rest; k ])
          (T.app_t pick [ T.app_t (builtin "find_map") [ m; k ]; rest; k ]);
        T.rule (T.app_t pick [ T.some_t v; rest; k ]) (T.some_t v);
        T.rule (T.app_t pick [ T.none_t; rest; k ]) (T.app_t sym [ rest; k ]);
      ]
  | "add_map", Some set, Some pair ->
      (* latest-wins: drop any existing binding for [k], then prepend [k : v], so
         [`$dom_map]/[`$distinct] never see a duplicate key (matches [VMap.add]). *)
      let drop = helper "drop" in
      [
        T.rule
          (T.app_t sym [ set [ ps ]; k; v ])
          (set [ T.cons_t (pair [ k; v ]) (T.app_t drop [ k; ps ]) ]);
        T.rule (T.app_t drop [ k; T.nil_t ]) T.nil_t;
        T.rule_cond
          (T.app_t drop [ k; T.cons_t (pair [ k2; v2 ]) rest ])
          (T.app_t drop [ k; rest ])
          [ (T.eq_t k k2, T.bool_t ~scalars true) ];
        T.rule_cond
          (T.app_t drop [ k; T.cons_t (pair [ k2; v2 ]) rest ])
          (T.cons_t (pair [ k2; v2 ]) (T.app_t drop [ k; rest ]))
          [ (T.eq_t k k2, T.bool_t ~scalars false) ];
      ]
  | "update_map", Some _, _ ->
      (* [update_map] is [add_map] (interp's [let update_map = add_map]). *)
      [
        T.rule
          (T.app_t sym [ m; k; v ])
          (T.app_t (builtin "add_map") [ m; k; v ]);
      ]
  | "adds_map", Some _, _ ->
      (* zip the key/value lists, folding each pair through [add_map]; a length
         mismatch is left stuck, as the interpreter raises. *)
      [
        T.rule (T.app_t sym [ m; T.nil_t; T.nil_t ]) m;
        T.rule
          (T.app_t sym [ m; T.cons_t k ks; T.cons_t v vs ])
          (T.app_t sym [ T.app_t (builtin "add_map") [ m; k; v ]; ks; vs ]);
      ]
  (* ----- sets ----- *)
  | "union_set", Some set, _ ->
      [
        T.rule
          (T.app_t sym [ set [ a ]; set [ b ] ])
          (set [ T.cat_t a (T.app_t list_diff_sym [ b; a ]) ]);
      ]
  | "diff_set", Some set, _ ->
      [
        T.rule
          (T.app_t sym [ set [ a ]; set [ b ] ])
          (set [ T.app_t list_diff_sym [ a; b ] ]);
      ]
  | "intersect_set", Some set, _ ->
      [
        T.rule
          (T.app_t sym [ set [ a ]; set [ b ] ])
          (set [ T.app_t list_inter_sym [ a; b ] ]);
      ]
  | "unions_set", Some set, _ ->
      [
        T.rule (T.app_t sym [ T.nil_t ]) (set [ T.nil_t ]);
        T.rule
          (T.app_t sym [ T.cons_t s rest ])
          (T.app_t (builtin "union_set") [ s; T.app_t sym [ rest ] ]);
      ]
  | "sub_set", Some set, _ ->
      [
        T.rule
          (T.app_t sym [ set [ a ]; set [ b ] ])
          (T.app_t list_submem_sym [ a; b ]);
      ]
  | "eq_set", Some set, _ ->
      [
        T.rule
          (T.app_t sym [ set [ a ]; set [ b ] ])
          (T.and_t
             (T.app_t list_submem_sym [ a; b ])
             (T.app_t list_submem_sym [ b; a ]));
      ]
  (* ----- lists ----- *)
  | "rev_", _, _ ->
      let go = helper "go" in
      [
        T.rule (T.app_t sym [ xs ]) (T.app_t go [ xs; T.nil_t ]);
        T.rule (T.app_t go [ T.nil_t; a ]) a;
        T.rule
          (T.app_t go [ T.cons_t x xs; a ])
          (T.app_t go [ xs; T.cons_t x a ]);
      ]
  | "concat_", _, _ ->
      [
        T.rule (T.app_t sym [ T.nil_t ]) T.nil_t;
        T.rule
          (T.app_t sym [ T.cons_t xs rest ])
          (T.cat_t xs (T.app_t sym [ rest ]));
      ]
  | "distinct_", _, _ ->
      [
        T.rule (T.app_t sym [ T.nil_t ]) (T.bool_t ~scalars true);
        T.rule
          (T.app_t sym [ T.cons_t x xs ])
          (T.and_t (T.not_t (T.mem_t x xs)) (T.app_t sym [ xs ]));
      ]
  | "partition_", _, _ ->
      (* split at index [n]: the prelude's [take]/[drop] do the cut. *)
      [
        T.rule
          (T.app_t sym [ xs; n ])
          (T.tuple_t [ T.take_t xs n; T.drop_t xs n ]);
      ]
  | "assoc_", _, _ ->
      (* the pairs are 2-tuples (not [pair]s): the first whose key equals [k]. *)
      [
        T.rule (T.app_t sym [ k; T.nil_t ]) T.none_t;
        T.rule_cond
          (T.app_t sym [ k; T.cons_t (T.tuple_t [ k2; v2 ]) rest ])
          (T.some_t v2)
          [ (T.eq_t k k2, T.bool_t ~scalars true) ];
        T.rule_cond
          (T.app_t sym [ k; T.cons_t (T.tuple_t [ k2; v2 ]) rest ])
          (T.app_t sym [ k; rest ])
          [ (T.eq_t k k2, T.bool_t ~scalars false) ];
      ]
  (* ----- text (a byte [cons]/[nil] list) ----- *)
  | "int_to_text", _, _ ->
      (* the decimal spelling of the int as a char list ([string_of_num]): an
         explicit sign then digits high-to-low by recursive div/mod by ten, each
         remainder mapped to its ASCII byte ([chr_48]..[chr_57]). A signed [int]
         always prints its sign (the interpreter's [Xl.Num.string_of_num], and
         {!To_maude}'s Native [$int_to_text] delegation, both do): '+' ([chr_43])
         for non-negative [int_pos n], '-' ([chr_45]) for [int_neg x] (which is
         -(x+1), so its magnitude is [bsucc x]). [n]/[x] here are [int_pos]/
         [int_neg]'s [BNatV] magnitude (can be P4-bit-width-scale), so [to_nat]'s
         own recursion is the [BNatV] (bdiv/bmod/blt) family, not the Peano one --
         {!Ctrs_term}'s doc comment. *)
      let to_nat = helper "nat" and digit = helper "digit" in
      let ten = T.bnat_lit ~scalars 10 in
      let digit_rules =
        List.init 10 (fun d ->
            T.rule (T.app_t digit [ T.bnat_lit ~scalars d ]) (T.chr_t (48 + d)))
      in
      [
        T.rule
          (T.app_t sym [ T.int_pos_t n ])
          (T.cons_t (T.chr_t 43) (T.app_t to_nat [ n ]));
        T.rule
          (T.app_t sym [ T.int_neg_t x ])
          (T.cons_t (T.chr_t 45) (T.app_t to_nat [ T.bsucc_t x ]));
        T.rule_cond (T.app_t to_nat [ n ])
          (T.cons_t (T.app_t digit [ n ]) T.nil_t)
          [ (T.blt_t n ten, T.bool_t ~scalars true) ];
        T.rule_cond (T.app_t to_nat [ n ])
          (T.cat_t
             (T.app_t to_nat [ T.bdiv_t n ten ])
             (T.cons_t (T.app_t digit [ T.bmod_t n ten ]) T.nil_t))
          [ (T.blt_t n ten, T.bool_t ~scalars false) ];
      ]
      @ digit_rules
  | "strip_prefix", _, _ ->
      [ T.rule (T.app_t sym [ t; p ]) (T.drop_t t (T.len_t p)) ]
  | "strip_suffix", _, _ ->
      [
        T.rule
          (T.app_t sym [ t; s ])
          (T.take_t t (T.sub_t (T.len_t t) (T.len_t s)));
      ]
  | "strip_all_whitespace", _, _ ->
      (* Mirrors {!To_maude}'s native delegation (Maude STRING's [find]/
         [substr], stripping every ASCII space, [targets/p4/builtins/texts.ml]'s
         [String.split_on_char ' ' |> concat]) over this theory's [cons]/[nil]
         char list instead: drop each [chr_32] (space), keep everything else.
         Reached by every table key ([TableKey_ok]'s
         [$strip_all_whitespace($name_expression(..))]) -- previously had no
         rule at all here, so any table with a key block got stuck. *)
      [
        T.rule (T.app_t sym [ T.nil_t ]) T.nil_t;
        T.rule_cond
          (T.app_t sym [ T.cons_t x xs ])
          (T.app_t sym [ xs ])
          [ (T.eq_t x (T.chr_t 32), T.bool_t ~scalars true) ];
        T.rule_cond
          (T.app_t sym [ T.cons_t x xs ])
          (T.cons_t x (T.app_t sym [ xs ]))
          [ (T.eq_t x (T.chr_t 32), T.bool_t ~scalars false) ];
      ]
  (* ----- saturating fixed-width arithmetic ----- *)
  | ("bin_satplus" | "bin_satminus"), _, _ -> (
      (* Saturating add/sub over fixed-width numbers ([w W i] unsigned, [w S i]
         signed): clamp into the width's range instead of wrapping. p4-old
         declares these [builtin dec] with no clauses (and the interpreter has
         no OCaml implementation either), so these rules mirror the clause
         definitions the new p4 spec gives them
         ([specs/p4/3-operations/3-operations.spectec]), spelled with p4-old's
         conversions ([$to_int]/[$to_bitstr]) and each [$ite] split into a
         complementary conditional rule pair. An arbitrary-precision ([D])
         operand has no width to saturate at and a width mismatch is
         meaningless: both stay stuck, where the interpreter would raise. *)
      match
        ( To_ctrs.case_ctor orig "number" "variant_number_W_2",
          To_ctrs.case_ctor orig "number" "variant_number_S_2" )
      with
      | Some unsigned, Some signed ->
          let comb =
            if id.it = "bin_satplus" then T.add_int_t else T.sub_int_t
          in
          let w = T.var_t "w" and w2 = T.var_t "w2" in
          let i_l = T.var_t "i_l" and i_r = T.var_t "i_r" in
          let i = T.var_t "i" in
          let i_ls = T.var_t "i_ls" and i_rs = T.var_t "i_rs" in
          let i_max = T.var_t "i_max" and i_min = T.var_t "i_min" in
          let zero_i = T.int_lit ~scalars 0 in
          let one_i = T.int_lit ~scalars 1 in
          (* [w] is the bit-width parameter, now a binary nat (the nat->binary
             retype), so wrapping it as an int is [int_pos] directly -- no bridge
             (same simplification as {!To_ctrs}'s [UpCastE] cast site). *)
          let w_int = T.int_pos_t w in
          let to_int x = T.app_t (builtin "to_int") [ w_int; x ] in
          let to_bitstr x = T.app_t (builtin "to_bitstr") [ w_int; x ] in
          let pow2 n = T.app_t (builtin "pow2") [ n ] in
          (* unsigned [w W i]: the raw sum/difference, clamped to
             [0 .. 2^w - 1] (only the overflowing end can be hit). *)
          let u_lhs =
            T.app_t sym [ unsigned [ w; i_l ]; unsigned [ w2; i_r ] ]
          in
          let u_conds = [ (w, w2); (comb i_l i_r, i) ] in
          let u_rules =
            if id.it = "bin_satplus" then
              [
                T.rule_cond u_lhs
                  (unsigned [ w; i ])
                  (u_conds @ [ (T.lt_int_t i (pow2 w), T.bool_t ~scalars true) ]);
                T.rule_cond u_lhs
                  (unsigned [ w; T.sub_int_t (pow2 w) one_i ])
                  (u_conds
                  @ [ (T.lt_int_t i (pow2 w), T.bool_t ~scalars false) ]);
              ]
            else
              [
                T.rule_cond u_lhs
                  (unsigned [ w; i ])
                  (u_conds @ [ (T.lt_int_t i zero_i, T.bool_t ~scalars false) ]);
                T.rule_cond u_lhs
                  (unsigned [ w; zero_i ])
                  (u_conds @ [ (T.lt_int_t i zero_i, T.bool_t ~scalars true) ]);
              ]
          in
          (* signed [w S i]: reinterpret the bits as two's-complement, combine,
             clamp into [-2^(w-1) .. 2^(w-1) - 1], convert back to bits. *)
          let s_lhs = T.app_t sym [ signed [ w; i_l ]; signed [ w2; i_r ] ] in
          let s_conds =
            [
              (w, w2);
              (to_int i_l, i_ls);
              (to_int i_r, i_rs);
              (comb i_ls i_rs, i);
            ]
          in
          let pos_conds =
            s_conds
            @ [
                (T.lt_int_t zero_i i, T.bool_t ~scalars true);
                (pow2 (T.sub_t w (T.nat_lit ~scalars 1)), i_max);
              ]
          in
          let neg_conds =
            s_conds
            @ [
                (T.lt_int_t zero_i i, T.bool_t ~scalars false);
                (T.negate_int_t (pow2 (T.sub_t w (T.nat_lit ~scalars 1))), i_min);
              ]
          in
          let s_rules =
            [
              T.rule_cond s_lhs
                (signed [ w; to_bitstr i ])
                (pos_conds @ [ (T.lt_int_t i i_max, T.bool_t ~scalars true) ]);
              T.rule_cond s_lhs
                (signed [ w; to_bitstr (T.sub_int_t i_max one_i) ])
                (pos_conds @ [ (T.lt_int_t i i_max, T.bool_t ~scalars false) ]);
              T.rule_cond s_lhs
                (signed [ w; to_bitstr i ])
                (neg_conds @ [ (T.lt_int_t i i_min, T.bool_t ~scalars false) ]);
              T.rule_cond s_lhs
                (signed [ w; to_bitstr i_min ])
                (neg_conds @ [ (T.lt_int_t i i_min, T.bool_t ~scalars true) ]);
            ]
          in
          u_rules @ s_rules
      | _ -> [])
  (* ----- fixed-width numeric conversions and bitwise ops -----
     Unlike [bin_satplus]/[bin_satminus] above (real spec [def] clauses),
     [pow2]/[shl]/[shr]/[shr_arith]/[bitstr_to_int]/[int_to_bitstr]/[bneg]/
     [band]/[bxor]/[bor]/[bitacc] are [builtin dec]s with NO spec clauses at
     all -- the only definition anywhere is the interpreter's own OCaml,
     [targets/p4/builtins/numerics.ml]. These mirror that file's recursive
     STRUCTURE directly (step-by-step halving, not a closed-form division by
     [2^o]) rather than an equivalent-looking shortcut, specifically so a
     rounding subtlety (how truncating division interacts with repeated
     halving of a negative number) can never make this oracle disagree with
     the interpreter it exists to be checked against. [$bitacc_replace] (also
     a [builtin dec] in the spec) has no OCaml implementation in
     [numerics.ml] either -- not covered here, left exactly as unimplemented
     as the interpreter itself leaves it. *)
  | "pow2", _, _ ->
      (* [pow2 w = 2^w] -- a direct power, not hand-rolled doubling:
         {!Rewrite.Prelude}'s [bpow_nat] already has real equations. This is
         the site the whole binary encoding exists for: [w] (a P4 bit-width,
         still Peano -- it stays small) is the EXPONENT, but the accumulated
         RESULT (up to 2^64) is [BNatV], never a Peano tower. *)
      let w = T.var_t "w" in
      [
        T.rule (T.app_t sym [ w ])
          (T.int_pos_t (T.bpow_nat_t (T.bnat_lit ~scalars 2) w));
      ]
  | "shl", _, _ ->
      (* [shl v o = v * 2^o] for [o >= 0] -- a single multiplication is exact
         regardless of how [numerics.ml]'s [shl'] breaks it into steps, no
         rounding to preserve. [o <= 0] is the identity ([shl'] only recurses
         while [o > 0]; it does not shift the other way for a negative
         offset). *)
      let v = T.var_t "v" and o = T.var_t "o" in
      [
        T.rule
          (T.app_t sym [ v; T.int_pos_t o ])
          (T.mul_int_t v (T.pow_int_t (T.int_lit ~scalars 2) (T.int_pos_t o)));
        T.rule (T.app_t sym [ v; T.int_neg_t o ]) v;
      ]
  | "shr", _, _ ->
      (* [numerics.ml]'s [shr'] halves [v] (a truncating division) ONE STEP
         AT A TIME, [o] times -- mirrored via structural recursion on [o]'s
         magnitude rather than a single division by [2^o], since repeated
         truncation is not always the same value as one truncation by the
         product. [o <= 0] is the identity, same as [shl]. [o]'s magnitude is
         [BNatV] now, so "is it zero" is a computed [bis_zero] tag rather
         than a [zero]/[succ] shape match -- dispatched through an auxiliary
         (the same idiom {!Rewrite.Prelude}'s [div_aux]/[mod_aux] use) so the
         zero/nonzero cases are disjoint on the TAG, not on a raw side
         condition sharing the unconditional case's LHS shape (which would be
         a real, if vacuous, critical-pair overlap for the CRC). *)
      let v = T.var_t "v" and o = T.var_t "o" in
      let aux = helper "aux" in
      [
        T.rule
          (T.app_t sym [ v; T.int_pos_t o ])
          (T.app_t aux [ T.bis_zero_t o; v; o ]);
        T.rule (T.app_t aux [ T.bool_t ~scalars true; v; o ]) v;
        T.rule
          (T.app_t aux [ T.bool_t ~scalars false; v; o ])
          (T.app_t sym
             [ T.div_int_t v (T.int_lit ~scalars 2); T.int_pos_t (T.bpred_t o) ]);
        T.rule (T.app_t sym [ v; T.int_neg_t o ]) v;
      ]
  | "shr_arith", _, _ ->
      (* Same recursive shape as [shr], but [m] is added back in at EVERY
         halving step (not once at the end) for sign extension -- genuinely
         not a closed form, so [numerics.ml]'s [shr_arith'] recursion is
         mirrored exactly (same [bis_zero]-tag-dispatch retarget as [shr]). *)
      let v = T.var_t "v" and o = T.var_t "o" and m = T.var_t "m" in
      let aux = helper "aux" in
      [
        T.rule
          (T.app_t sym [ v; T.int_pos_t o; m ])
          (T.app_t aux [ T.bis_zero_t o; v; o; m ]);
        T.rule (T.app_t aux [ T.bool_t ~scalars true; v; o; m ]) v;
        T.rule
          (T.app_t aux [ T.bool_t ~scalars false; v; o; m ])
          (T.app_t sym
             [
               T.add_int_t (T.div_int_t v (T.int_lit ~scalars 2)) m;
               T.int_pos_t (T.bpred_t o);
               m;
             ]);
        T.rule (T.app_t sym [ v; T.int_neg_t o; m ]) v;
      ]
  | "bitstr_to_int", _, _ ->
      (* Two's-complement DECODE: normalize a raw (unsigned) [n] into
         [-2^(w-1) .. 2^(w-1) - 1] by adding/subtracting [2^w] once.
         [numerics.ml]'s [bitstr_to_int'] recurses instead of assuming one
         correction step always suffices, so this does too. *)
      let w = T.var_t "w" and n = T.var_t "n" in
      let pow2w = T.var_t "pow2w" and half = T.var_t "half" in
      let neg_half = T.var_t "neg_half" in
      let common =
        [
          (T.pow_int_t (T.int_lit ~scalars 2) w, pow2w);
          (T.div_int_t pow2w (T.int_lit ~scalars 2), half);
          (T.negate_int_t half, neg_half);
        ]
      in
      [
        T.rule_cond
          (T.app_t sym [ w; n ])
          (T.app_t sym [ w; T.sub_int_t n pow2w ])
          (common @ [ (T.leq_int_t half n, T.bool_t ~scalars true) ]);
        T.rule_cond
          (T.app_t sym [ w; n ])
          (T.app_t sym [ w; T.add_int_t n pow2w ])
          (common
          @ [
              (T.leq_int_t half n, T.bool_t ~scalars false);
              (T.lt_int_t n neg_half, T.bool_t ~scalars true);
            ]);
        T.rule_cond
          (T.app_t sym [ w; n ])
          n
          (common
          @ [
              (T.leq_int_t half n, T.bool_t ~scalars false);
              (T.lt_int_t n neg_half, T.bool_t ~scalars false);
            ]);
      ]
  | "int_to_bitstr", _, _ ->
      (* Two's-complement ENCODE: wrap [n] modulo [2^w] into [0 .. 2^w - 1]. *)
      let w = T.var_t "w" and n = T.var_t "n" in
      let pow2w = T.var_t "pow2w" in
      let common = [ (T.pow_int_t (T.int_lit ~scalars 2) w, pow2w) ] in
      [
        T.rule_cond
          (T.app_t sym [ w; n ])
          (T.mod_int_t n pow2w)
          (common @ [ (T.leq_int_t pow2w n, T.bool_t ~scalars true) ]);
        T.rule_cond
          (T.app_t sym [ w; n ])
          (T.app_t sym [ w; T.add_int_t n pow2w ])
          (common
          @ [
              (T.leq_int_t pow2w n, T.bool_t ~scalars false);
              (T.lt_int_t n (T.int_lit ~scalars 0), T.bool_t ~scalars true);
            ]);
        T.rule_cond
          (T.app_t sym [ w; n ])
          n
          (common
          @ [
              (T.leq_int_t pow2w n, T.bool_t ~scalars false);
              (T.lt_int_t n (T.int_lit ~scalars 0), T.bool_t ~scalars false);
            ]);
      ]
  | "bneg", _, _ ->
      (* Arbitrary-precision two's-complement NOT is the arithmetic identity
         [~n = -(n+1)] ([Bigint.bit_not]) -- no bit decomposition needed, and
         it holds for every [n], not just nonnegative ones. *)
      let n = T.var_t "n" in
      [
        T.rule (T.app_t sym [ n ])
          (T.negate_int_t (T.add_int_t n (T.int_lit ~scalars 1)));
      ]
  | ("band" | "bxor" | "bor"), _, _ ->
      (* Unlike [bneg], AND/XOR/OR have no single arithmetic identity that
         covers every sign combination directly, so this decomposes both
         [BNatV] magnitudes bit by bit via [bdiv_t]/[bmod_t] by 2 (a
         mechanical retarget of the same recursive shape used against the old
         Peano magnitude, not a redesign around direct [bd0]/[bd1] matching:
         [bdiv]/[bmod] are already O(log n), so this stays O(w) recursive
         levels each costing O(log(current value)), not the exponential
         blowup a Peano magnitude would have meant here) for NONNEGATIVE
         operands, then reaches negative operands through the standard
         infinite-two's-complement identities below -- matching
         [Bigint.bit_and]/[bit_or]/[bit_xor] ([targets/p4/builtins/numerics.ml]),
         the reference interpreter's own arbitrary-precision semantics. *)
      let combine_bit op bl br =
        match op with
        | `And -> T.bmul_t bl br
        | `Or -> T.bsub_t (T.badd_t bl br) (T.bmul_t bl br)
        | `Xor -> T.bmod_t (T.badd_t bl br) (T.bnat_lit ~scalars 2)
      in
      (* AND's identity is 0 (0 against anything is 0); OR/XOR's is the
         OTHER operand unchanged (0 contributes nothing to either) -- a
         genuinely different base case per operator, not a shared "either
         side zero -> 0" shortcut (that shortcut is correct for AND alone --
         caught by testing band/bor/bxor independently: band(6,3)=2 checked
         out, but a shared zero-base gave bor(6,3)=3 and bxor(6,3)=1 instead
         of the correct 7 and 5). *)
      let base_rhs op other = match op with `And -> T.bzero_t | _ -> other in
      (* The recursive bit-decomposition rules for one BNat-level bitwise
         [op] over two NONNEGATIVE magnitudes, defining [nat_sym]. Every
         top-level operator needs its own shape (AND for [$band], OR for
         [$bor], XOR for [$bxor]) as its PRIMARY nat helper; AND and OR each
         also need the OTHER's shape as a CROSS helper, for the De Morgan
         identities their negative-operand cases reduce through below. *)
      let nat_op_rules op nat_sym =
        let l = T.var_t "l" and r = T.var_t "r" in
        let two_n = T.bnat_lit ~scalars 2 in
        let bl = T.var_t "bl" and br = T.var_t "br" in
        let ql = T.var_t "ql" and qr = T.var_t "qr" in
        [
          T.rule (T.app_t nat_sym [ T.bzero_t; r ]) (base_rhs op r);
          T.rule (T.app_t nat_sym [ l; T.bzero_t ]) (base_rhs op l);
          T.rule_cond
            (T.app_t nat_sym [ l; r ])
            (T.badd_t (combine_bit op bl br)
               (T.bmul_t two_n (T.app_t nat_sym [ ql; qr ])))
            [
              (T.bis_zero_t l, T.bool_t ~scalars false);
              (T.bis_zero_t r, T.bool_t ~scalars false);
              (T.bmod_t l two_n, bl);
              (T.bmod_t r two_n, br);
              (T.bdiv_t l two_n, ql);
              (T.bdiv_t r two_n, qr);
            ];
        ]
      in
      let op = match id.it with "band" -> `And | "bor" -> `Or | _ -> `Xor in
      let nat_sym = helper "nat" in
      let primary_rules = nat_op_rules op nat_sym in
      let cross_sym = helper "nat_cross" in
      let cross_rules =
        match op with
        | `And -> nat_op_rules `Or cross_sym
        | `Or -> nat_op_rules `And cross_sym
        | `Xor -> []
      in
      (* Sign cases beyond (nonneg, nonneg) all follow from writing a negative
         operand [int_neg m] as the two's-complement identity [~m] (recall
         [int_neg m] itself already represents [-(m+1)], i.e. [bneg]'s [~m] --
         {!Ctrs_term}'s [int_neg_t]), then pushing the NOT through the
         operator via De Morgan for AND/OR ([~a & b = b - (a & b)],
         [~a & ~b = ~(a | b)], and symmetrically for OR) or through XOR's own
         NOT-cancellation ([~a ^ b = ~(a ^ b)], [~a ^ ~b = a ^ b]). Every
         subtraction here is a proper bit subset of its minuend by
         construction, so [bsub_t]'s monus never clamps. *)
      let l = T.var_t "l" and r = T.var_t "r" in
      let sign_rules =
        match op with
        | `And ->
            [
              T.rule
                (T.app_t sym [ T.int_pos_t l; T.int_pos_t r ])
                (T.int_pos_t (T.app_t nat_sym [ l; r ]));
              T.rule
                (T.app_t sym [ T.int_neg_t l; T.int_pos_t r ])
                (T.int_pos_t (T.bsub_t r (T.app_t nat_sym [ l; r ])));
              T.rule
                (T.app_t sym [ T.int_pos_t l; T.int_neg_t r ])
                (T.int_pos_t (T.bsub_t l (T.app_t nat_sym [ l; r ])));
              T.rule
                (T.app_t sym [ T.int_neg_t l; T.int_neg_t r ])
                (T.int_neg_t (T.app_t cross_sym [ l; r ]));
            ]
        | `Or ->
            [
              T.rule
                (T.app_t sym [ T.int_pos_t l; T.int_pos_t r ])
                (T.int_pos_t (T.app_t nat_sym [ l; r ]));
              T.rule
                (T.app_t sym [ T.int_neg_t l; T.int_pos_t r ])
                (T.int_neg_t (T.bsub_t l (T.app_t cross_sym [ l; r ])));
              T.rule
                (T.app_t sym [ T.int_pos_t l; T.int_neg_t r ])
                (T.int_neg_t (T.bsub_t r (T.app_t cross_sym [ l; r ])));
              T.rule
                (T.app_t sym [ T.int_neg_t l; T.int_neg_t r ])
                (T.int_neg_t (T.app_t cross_sym [ l; r ]));
            ]
        | `Xor ->
            [
              T.rule
                (T.app_t sym [ T.int_pos_t l; T.int_pos_t r ])
                (T.int_pos_t (T.app_t nat_sym [ l; r ]));
              T.rule
                (T.app_t sym [ T.int_neg_t l; T.int_pos_t r ])
                (T.int_neg_t (T.app_t nat_sym [ l; r ]));
              T.rule
                (T.app_t sym [ T.int_pos_t l; T.int_neg_t r ])
                (T.int_neg_t (T.app_t nat_sym [ l; r ]));
              T.rule
                (T.app_t sym [ T.int_neg_t l; T.int_neg_t r ])
                (T.int_pos_t (T.app_t nat_sym [ l; r ]));
            ]
      in
      primary_rules @ cross_rules @ sign_rules
  | "bitacc", _, _ ->
      (* [n[m:l]] masks the low [(m - l + 1)] bits of [n >> l]; validity
         ([l >= 0], [m >= l]) is a guard, not a computed default -- an
         invalid slice is left stuck, mirroring how [numerics.ml] raises
         instead of returning a value. *)
      let n = T.var_t "n" and m = T.var_t "m" and l = T.var_t "l" in
      let shifted = T.var_t "shifted" and mask = T.var_t "mask" in
      [
        T.rule_cond
          (T.app_t sym [ n; m; l ])
          (T.app_t (builtin "band") [ shifted; mask ])
          [
            (T.leq_int_t (T.int_lit ~scalars 0) l, T.bool_t ~scalars true);
            (T.leq_int_t l m, T.bool_t ~scalars true);
            (T.app_t (builtin "shr") [ n; l ], shifted);
            ( T.sub_int_t
                (T.pow_int_t (T.int_lit ~scalars 2)
                   (T.add_int_t (T.sub_int_t m l) (T.int_lit ~scalars 1)))
                (T.int_lit ~scalars 1),
              mask );
          ];
      ]
  | ("sum_nat" | "max_nat" | "min_nat"), _, _ -> (
      (* [$sizeof_minSizeInBits']-style header/struct/tuple size computations
         fold these over a [TypeIR] list, matching [targets/p4/builtins/nats.ml]'s
         [List.fold_left (+/max/min) Bigint.zero] exactly -- including its
         [min]'s degenerate consequence: since every [nat] is already >= 0,
         seeding the fold at 0 makes [min_nat] return 0 for EVERY list, empty
         or not (0 is <= every element, so it wins every comparison), not the
         list's actual minimum. That's the reference interpreter's own
         behavior, reproduced verbatim rather than "fixed" here. [sum]/[max]
         don't have this degeneracy: 0 is additive-identity for [sum], and a
         nonempty list's true max is never below the 0 seed. *)
      let x = T.var_t "x"
      and xs = T.var_t "xs" in
      match id.it with
      | "min_nat" -> [ T.rule (T.app_t sym [ xs ]) T.zero_t ]
      | "sum_nat" ->
          [
            T.rule (T.app_t sym [ T.nil_t ]) T.zero_t;
            T.rule
              (T.app_t sym [ T.cons_t x xs ])
              (T.add_t x (T.app_t sym [ xs ]));
          ]
      | _ (* "max_nat" *) ->
          [
            T.rule (T.app_t sym [ T.nil_t ]) T.zero_t;
            T.rule_cond
              (T.app_t sym [ T.cons_t x xs ])
              x
              [ (T.leq_t (T.app_t sym [ xs ]) x, T.bool_t ~scalars true) ];
            T.rule_cond
              (T.app_t sym [ T.cons_t x xs ])
              (T.app_t sym [ xs ])
              [ (T.leq_t (T.app_t sym [ xs ]) x, T.bool_t ~scalars false) ];
          ])
  | _ -> []

(* The builtins the Maude backend re-emits as one-line delegations over Maude's
   built-in Bool/Nat/Int/String ({!To_maude.delegation_eqs}); emitting their
   structural recursion too would clash, so the [Native] theory omits them (the
   [Structural] analysis keeps them).

   "Clash" is not merely redundancy: the structural equation is declared FIRST,
   so it wins the match, and it then calls a binary-nat helper ([bpow_nat],
   [badd], [bsub], [bis_zero], ...) whose {!Prelude} equations the [Native]
   theory deliberately omits -- leaving the call permanently stuck. That is how
   [~32w0] (via [$un_bnot] -> [$pow2] -> [bpow_nat]) used to strand a whole
   program's typing. Keep this list in sync with {!To_maude.delegation_eqs}:
   every builtin with a delegation there and a rule case above belongs here. *)
let delegated_in_native =
  [
    (* text *)
    "int_to_text";
    "strip_prefix";
    "strip_suffix";
    "strip_all_whitespace";
    (* fixed-width numeric + bitwise *)
    "pow2";
    "shl";
    "shr";
    "shr_arith";
    "bneg";
    "band";
    "bxor";
    "bor";
    "bitacc";
    (* nat-list folds: the structural rules seed the fold at [bzero], a binary-nat
       constant the [Native] theory has no equation for, so a single fold step
       ([add(nat(32), bzero)]) strands the whole reduction. *)
    "sum_nat";
    "max_nat";
    "min_nat";
  ]

(* Every collection-builtin rule the spec's [BuiltinDecD]s call for, plus the
   shared list helpers, as definition rules for {!To_ctrs.of_spec}'s prunable
   pool. [] when the spec declares no collection builtin, so a spec without them
   (e.g. impty) is untouched. *)
let rules_of_builtins ~scalars (orig : spec) : R.rule list =
  let omitted id = scalars = T.Native && List.mem id delegated_in_native in
  let per_builtin =
    List.concat_map
      (fun (def : def) ->
        match def.it with
        | BuiltinDecD { defid; _ } when not (omitted defid.it) ->
            rules_of_builtin ~scalars orig defid
        | _ -> [])
      orig
  in
  if per_builtin = [] then [] else shared_list_defs ~scalars @ per_builtin
