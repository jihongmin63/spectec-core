module R = Rewrite_system
open Ctrs_term

(* Fixed prelude: booleans, Peano naturals and integers, lists/options, their
   matchers, and structural equality over the built-in sorts. Integers use a
   sign-magnitude form ([int_pos n] = [+n], [int_neg n] = [-(n+1)]) whose
   constructors are disjoint from the nat [zero]/[succ], so the two never
   collide; the natural family keeps the simpler bare-Peano rules. *)
let rules : R.rule list =
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
   sync with [rules] above. *)
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
