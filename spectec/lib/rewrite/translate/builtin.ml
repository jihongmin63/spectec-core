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
      (* the decimal spelling of the int as a char list ([string_of_num]): split
         off the sign, then emit digits high-to-low by recursive div/mod by ten,
         each remainder mapped to its ASCII byte ([chr_48]..[chr_57]). [int_neg x]
         is -(x+1), so its magnitude is [succ x]; '-' is [chr_45]. *)
      let to_nat = helper "nat" and digit = helper "digit" in
      let ten = T.nat_lit ~scalars 10 in
      let digit_rules =
        List.init 10 (fun d ->
            T.rule (T.app_t digit [ T.nat_lit ~scalars d ]) (T.chr_t (48 + d)))
      in
      [
        T.rule (T.app_t sym [ T.int_pos_t n ]) (T.app_t to_nat [ n ]);
        T.rule
          (T.app_t sym [ T.int_neg_t x ])
          (T.cons_t (T.chr_t 45) (T.app_t to_nat [ T.succ_t x ]));
        T.rule_cond (T.app_t to_nat [ n ])
          (T.cons_t (T.app_t digit [ n ]) T.nil_t)
          [ (T.lt_t n ten, T.bool_t ~scalars true) ];
        T.rule_cond (T.app_t to_nat [ n ])
          (T.cat_t
             (T.app_t to_nat [ T.div_t n ten ])
             (T.cons_t (T.app_t digit [ T.mod_t n ten ]) T.nil_t))
          [ (T.lt_t n ten, T.bool_t ~scalars false) ];
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
          let to_int x = T.app_t (builtin "to_int") [ T.int_pos_t w; x ] in
          let to_bitstr x =
            T.app_t (builtin "to_bitstr") [ T.int_pos_t w; x ]
          in
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
  | _ -> []

(* The text builtins the Maude backend re-emits as built-in-String delegations
   ({!To_maude}); emitting their structural recursion too would clash, so the
   [Native] theory omits them (the [Structural] analysis keeps them). *)
let delegated_in_native = [ "int_to_text"; "strip_prefix"; "strip_suffix" ]

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
