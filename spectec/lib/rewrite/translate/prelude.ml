module R = Rewrite_system
open Ctrs_term

(* The prelude symbols whose defining rules encode the hand-written scalar
   theories (booleans, binary nats, sign-magnitude ints) or recurse over binary
   indices (the positional list operations). The Maude backend replaces their
   rules with one-line delegations to Maude's built-in Bool/Nat/Int/String
   ({!Maude_theory}), so the [Native] prelude drops them; the analysis pipeline
   keeps them. Must stay in sync with {!To_maude}'s delegation equations. *)
let native_replaced_heads : string list =
  [
    (* booleans *)
    "not";
    "and";
    "or";
    "impl";
    "equiv";
    (* binary nat arithmetic (delegated in Structural to the [BNatV] engine
       below; delegated in Native to Maude's built-in Nat) *)
    "add";
    "sub";
    "mul";
    "div";
    "mod";
    "pow";
    (* boolean-dispatch auxiliary for the binary-exponent [pow]: analysis-surface
       only, no Native delegation ([pow] delegates to built-in [^]). *)
    "pow_aux";
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
    "div_int_aux";
    "mod_int_aux";
    "pow_int";
    "leq_int";
    "lt_int";
    (* binary (Coq [positive]/[N]-style) magnitude arithmetic backing
       [int_pos]/[int_neg]'s magnitude (see {!Ctrs_term}'s doc comment):
       Structural-only, like the rest of this list, since [Native] never
       constructs a [BNatV] term (it wraps Maude's built-in [Int] directly). *)
    "bsucc";
    "bpred";
    "bpred_double";
    "bis_zero";
    "badd";
    "badd_carry";
    "bmul";
    "bcompare";
    "bcompare_cont";
    "ble_of_cmp";
    "blt_of_cmp";
    "bleq";
    "blt";
    "bsub_mask";
    "bsub_mask_carry";
    "bdouble_mask";
    "bsucc_double_mask";
    "bsub_of_mask";
    "bsub";
    "bring0";
    "bring1";
    "bquot";
    "brem";
    "bdivmod_pos";
    "bdivmod_step0";
    "bdivmod_step1";
    "bdivmod_combine";
    "bdivmod_dispatch";
    "bdivmod_base";
    "bdiv";
    "bmod";
    "bpow_nat";
    "bpow_nat_aux";
    (* nat-membership predicate over the binary nat / int representations *)
    "sub_nat";
    (* list operations recursing over binary indices, with their bis_zero-tag
       auxiliaries (analysis-surface only; Native delegates the list ops) *)
    "len";
    "idx";
    "idx_aux";
    "take";
    "take_aux";
    "drop";
    "drop_aux";
    "slice";
    "upd_idx";
    "upd_idx_aux";
    "upd_slice";
  ]

(* A rule the [Native] prelude keeps: not a delegated-head rule, and not an [eq]
   over scalar constructors (the structural [eq] over options/lists/user types is
   kept; the scalar [eq] over nats/ints/bools is delegated). *)
let kept_in_native (r : R.rule) : bool =
  let scalar_ctor = function
    | R.App
        ( ( "bzero" | "bone" | "bd0" | "bd1" | "int_pos" | "int_neg" | "true"
          | "false" ),
          _ ) ->
        true
    | _ -> false
  in
  match R.defined_head r with
  | Some h when List.mem h native_replaced_heads -> false
  | _ -> (
      match r.R.lhs with
      | R.App ("eq", [ a; b ]) -> not (scalar_ctor a || scalar_ctor b)
      | _ -> true)

(* Fixed prelude: booleans, binary naturals and integers, lists/options, their
   matchers, and structural equality over the built-in sorts. Naturals and an
   int's magnitude share one binary ([bzero]/[bone]/[bd0]/[bd1]) family;
   integers add a sign-magnitude wrapper ([int_pos n] = [+n], [int_neg n] =
   [-(n+1)]) whose constructors are disjoint from the bare-nat constructors, so
   a nat term and an int term never collide.

   The boolean leaves are emitted in the current scalar theory ([bool_t]); on the
   [Native] path the {!native_replaced_heads} rules are dropped (the Maude backend
   re-emits them as one-line delegations to its built-in Bool/Nat/Int/String),
   so what survives there is the structural list/option machinery the built-ins
   do not cover. The rule ORDER is mode-independent, so the [Structural] output
   is unchanged (its [bool_t] is the structural [true]/[false]). *)
let rules ~scalars : R.rule list =
  let x = var_t "x" and y = var_t "y" and xs = var_t "xs" and ys = var_t "ys" in
  let i = var_t "i" and n = var_t "n" and v = var_t "v" in
  let p = var_t "p" and q = var_t "q" and r = var_t "r" in
  let r2 = var_t "r2" in
  let yes = bool_t ~scalars true and no = bool_t ~scalars false in
  let all =
    [
      (* booleans *)
      rule (not_t true_t) no;
      rule (not_t false_t) yes;
      rule (and_t true_t y) y;
      rule (and_t false_t y) no;
      rule (or_t true_t y) yes;
      rule (or_t false_t y) y;
      rule (impl_t true_t y) y;
      rule (impl_t false_t y) yes;
      rule (equiv_t true_t y) y;
      rule (equiv_t false_t y) (not_t y);
      (* Naturals are binary-encoded ([BNatV]); every nat operation delegates
         to the binary magnitude engine below (a nat is exactly a non-negative
         int's magnitude, so the same equations serve). The heads stay in
         [native_replaced_heads], so Native instead delegates them to Maude's
         built-in Nat. [sub] is truncated (monus) -- [bsub]'s 3-valued mask
         clamps a would-be-negative difference to [bzero]; [div]/[mod] by zero
         are left stuck (partial), matching [bdiv]/[bmod]. *)
      rule (add_t x y) (badd_t x y);
      rule (sub_t x y) (bsub_t x y);
      rule (mul_t x y) (bmul_t x y);
      rule (leq_t x y) (bleq_t x y);
      rule (lt_t x y) (blt_t x y);
      rule (div_t x y) (bdiv_t x y);
      rule (mod_t x y) (bmod_t x y);
      (* [pow]: binary exponent, dispatched on [bis_zero]/[bpred] rather than a
         [succ] pattern (a binary exponent like 2 is [bd0 bone], never
         [bsucc _], so it cannot be matched structurally -- the same idiom
         [pow_int] uses). [pow_aux] is analysis-surface only (Native delegates
         [pow] to built-in [^]). *)
      rule (pow_t x y) (app_t "pow_aux" [ bis_zero_t y; x; y ]);
      rule (app_t "pow_aux" [ yes; x; y ]) bone_t;
      rule (app_t "pow_aux" [ no; x; y ]) (bmul_t x (pow_t x (bpred_t y)));
      (* Naturals now share the binary [BNatV] representation with an int's
         magnitude, so the old Peano<->binary bridges ([bnat_of_nat],
         [bnat_to_nat], [double_nat]) are gone: a nat upcast to int just wraps in
         [int_pos] directly, and [nat_of_int] on a known-nonneg int projects its
         magnitude out unchanged (see the [nat_of_int] rule below). *)
      (* integer helpers: negate / magnitude / sign / projection, and the signed
         difference of two [BNatV] magnitudes ([sub_int_nat m n] = m - n as an
         int). All structural over [int_pos]/[int_neg], so they stay canonical.
         [negate_int]'s [int_pos] case is the one "peel one off the [BNatV]
         magnitude" site here (mirroring [pow_int] below and [shr]/[shr_arith]
         in {!Builtin}): since [bd0]/[bd1] give no static zero/nonzero
         distinction the way Peano [zero]/[succ] did, it dispatches through a
         boolean tag ([bis_zero]) rather than pattern-matching the shape
         directly, the same idiom [pow_aux]/[div_int_aux] use above (a raw
         [rule_cond] sharing the unconditional [bzero] case's LHS shape would
         be a genuine, if semantically vacuous, critical-pair overlap for the
         CRC to prove joinable; the disjoint boolean tag has none at all). *)
      rule
        (negate_int_t (int_pos_t x))
        (app_t "negate_int_pos_aux" [ bis_zero_t x; x ]);
      rule (app_t "negate_int_pos_aux" [ yes; x ]) (int_pos_t bzero_t);
      rule (app_t "negate_int_pos_aux" [ no; x ]) (int_neg_t (bpred_t x));
      rule (negate_int_t (int_neg_t x)) (int_pos_t (bsucc_t x));
      (* No blanket "negation is involutive" shortcut here (the Peano version
         above keeps one) -- ground execution reaches the identical result
         through the two rules above alone, just one rewrite step slower for
         a double negation (negate_int(int_pos(bpred(bsucc X))) = X, since
         bpred/bsucc are inverse for any X, Phase 1). Confirmed by CRC: WITH
         the shortcut, p4's negate_int slice comes back MAYBE -- the
         shortcut and the [int_neg] rule above overlap at
         negate_int(negate_int(int_neg(x))), and the two paths only agree
         because bsucc never returns a [bzero]-shaped term (proved by hand in
         Phase 1), a fact the [bis_zero] TAG can't see for a symbolic
         [bsucc(x)] the way the Peano version's literal [succ]/[zero] shape
         match could. Removing the shortcut removes the overlap entirely
         (verified: negate_int's p4 slice is YES/YES without it) rather than
         asking the CRC to discharge an invariant it has no way to see. *)
      rule (abs_nat_t (int_pos_t x)) x;
      rule (abs_nat_t (int_neg_t x)) (bsucc_t x);
      rule (nonneg_int_t (int_pos_t x)) yes;
      rule (nonneg_int_t (int_neg_t x)) no;
      (* [nat_of_int] on a known-nonneg int projects its magnitude out. Naturals
         are now the same binary [BNatV] as an [int_pos] magnitude (the
         nat->binary retype), so [nat_of_int]'s declared [IntV -> NatV] signature
         is satisfied by handing the magnitude back unchanged -- no bridge. (This
         is the pre-Phase-4 form, correct again now that [NatV] IS the binary
         family.) A negative int ([int_neg]) never reaches a nat [DownCastE] (P4
         only narrows known-nonneg values), so it is left stuck. *)
      rule (nat_of_int_t (int_pos_t x)) x;
      (* [sub_int_nat] is re-expressed compositionally atop [bleq]/[bsub]/
         [bpred] (Phases 1-2) rather than ported as a lockstep structural
         recursion on both magnitudes -- genuinely simpler, not just a
         retarget: [x >= y] gives the exact difference directly; [x < y]
         gives [-(y - x)] via [int_neg]'s own [-(n+1)] convention. *)
      rule (sub_int_nat_t x y) (app_t "sub_int_nat_aux" [ bleq_t y x; x; y ]);
      rule (app_t "sub_int_nat_aux" [ yes; x; y ]) (int_pos_t (bsub_t x y));
      rule
        (app_t "sub_int_nat_aux" [ no; x; y ])
        (int_neg_t (bpred_t (bsub_t y x)));
      (* integers: add (signed by both operands) / sub / mul *)
      rule (add_int_t (int_pos_t x) (int_pos_t y)) (int_pos_t (badd_t x y));
      rule
        (add_int_t (int_neg_t x) (int_neg_t y))
        (int_neg_t (bsucc_t (badd_t x y)));
      rule (add_int_t (int_pos_t x) (int_neg_t y)) (sub_int_nat_t x (bsucc_t y));
      rule (add_int_t (int_neg_t x) (int_pos_t y)) (sub_int_nat_t y (bsucc_t x));
      rule (sub_int_t x y) (add_int_t x (negate_int_t y));
      rule (mul_int_t (int_pos_t x) (int_pos_t y)) (int_pos_t (bmul_t x y));
      rule
        (mul_int_t (int_neg_t x) (int_neg_t y))
        (int_pos_t (bmul_t (bsucc_t x) (bsucc_t y)));
      rule
        (mul_int_t (int_pos_t x) (int_neg_t y))
        (negate_int_t (int_pos_t (bmul_t x (bsucc_t y))));
      rule
        (mul_int_t (int_neg_t x) (int_pos_t y))
        (negate_int_t (int_pos_t (bmul_t (bsucc_t x) y)));
      (* integers: leq / lt *)
      rule (leq_int_t (int_pos_t x) (int_pos_t y)) (bleq_t x y);
      rule (leq_int_t (int_neg_t x) (int_neg_t y)) (bleq_t y x);
      rule (leq_int_t (int_pos_t x) (int_neg_t y)) no;
      rule (leq_int_t (int_neg_t x) (int_pos_t y)) yes;
      rule (lt_int_t x y) (not_t (leq_int_t y x));
      (* integers: pow (non-negative exponent), and div / mod by magnitudes +
         sign (truncate toward zero). The quotient is negative iff the operands'
         signs differ; the remainder takes the dividend's sign. [pow_int]'s own
         exponent recursion is the other "peel one off a [BNatV] magnitude"
         site, same [bis_zero]-tag-dispatch idiom as [negate_int] above. *)
      rule
        (pow_int_t x (int_pos_t y))
        (app_t "pow_int_aux" [ bis_zero_t y; x; y ]);
      rule (app_t "pow_int_aux" [ yes; x; y ]) (int_pos_t bone_t);
      rule
        (app_t "pow_int_aux" [ no; x; y ])
        (mul_int_t x (pow_int_t x (int_pos_t (bpred_t y))));
      (* The signed div/mod also dispatch through a boolean auxiliary (the
         quotient's sign is [equiv(nonneg x, nonneg y)] -- "same sign", computed
         with the boolean [equiv] rather than structural [eq] so the slice does
         not drag in equality over every spec constructor; the remainder takes
         the dividend's sign [nonneg x]) so the two sign cases differ in a
         disjoint [yes]/[no] head pattern and raise no spurious critical pair. *)
      rule (div_int_t x y)
        (app_t "div_int_aux"
           [ equiv_t (nonneg_int_t x) (nonneg_int_t y); x; y ]);
      rule
        (app_t "div_int_aux" [ yes; x; y ])
        (int_pos_t (bdiv_t (abs_nat_t x) (abs_nat_t y)));
      rule
        (app_t "div_int_aux" [ no; x; y ])
        (negate_int_t (int_pos_t (bdiv_t (abs_nat_t x) (abs_nat_t y))));
      rule (mod_int_t x y) (app_t "mod_int_aux" [ nonneg_int_t x; x; y ]);
      rule
        (app_t "mod_int_aux" [ yes; x; y ])
        (int_pos_t (bmod_t (abs_nat_t x) (abs_nat_t y)));
      rule
        (app_t "mod_int_aux" [ no; x; y ])
        (negate_int_t (int_pos_t (bmod_t (abs_nat_t x) (abs_nat_t y))));
      (* Binary (Coq [positive]/[N]-style) magnitude arithmetic, alongside the
         Peano nat family above -- not yet referenced by [int_pos]/[int_neg]
         (see the binary-encoding plan; {!Ctrs_term}'s doc comment explains
         why there is one sort [BNatV] rather than a zero-free subsort).
         Canonicity ([bd0]/[bd1] must never wrap [bzero], which would be a
         non-canonical duplicate spelling of 0/1) is verified by hand for
         every rule below, not enforced by the sort system. *)
      (* bsucc (Coq [Pos.succ]): never returns [bzero]-shaped, so it is
         always safe to wrap its result in [bd0]/[bd1]. *)
      rule (bsucc_t bzero_t) bone_t;
      rule (bsucc_t bone_t) (bd0_t bone_t);
      rule (bsucc_t (bd0_t p)) (bd1_t p);
      rule (bsucc_t (bd1_t p)) (bd0_t (bsucc_t p));
      rule (bis_zero_t bzero_t) yes;
      rule (bis_zero_t bone_t) no;
      rule (bis_zero_t (bd0_t p)) no;
      rule (bis_zero_t (bd1_t p)) no;
      (* [bpred_double p] = [2 * val(p) - 1]; only ever called (by [bpred]
         below) with [p] already known non-[bzero], and never itself
         returns [bzero]-shaped. *)
      rule (bpred_double_t bone_t) bone_t;
      rule (bpred_double_t (bd0_t p)) (bd1_t (bpred_double_t p));
      rule (bpred_double_t (bd1_t p)) (bd1_t (bd0_t p));
      (* bpred (Coq [Pos.pred]/[pred_double]): partial at [bzero] -- no
         defining rule, left stuck, the same convention as nat [div]/[mod] by
         zero. *)
      rule (bpred_t bone_t) bzero_t;
      rule (bpred_t (bd0_t p)) (bpred_double_t p);
      rule (bpred_t (bd1_t p)) (bd0_t p);
      (* badd/badd_carry (Coq [Pos.add]/[Pos.add_carry]): a carry-threading
         mutually recursive pair. Every clause is disjoint on its (x, y)
         shape pair (16 for [badd], 9 for [badd_carry]), so the whole system
         is orthogonal -- no critical pairs by construction. Both are
         invariant-checked to never produce a [bzero]-shaped result when
         both operands are non-[bzero] (needed since the [bd0 p, bd0 q]/
         [bd1 p, bd1 q] cases wrap the recursive call in [bd0]/[bd1]). *)
      rule (badd_t bzero_t bzero_t) bzero_t;
      rule (badd_t bzero_t bone_t) bone_t;
      rule (badd_t bzero_t (bd0_t q)) (bd0_t q);
      rule (badd_t bzero_t (bd1_t q)) (bd1_t q);
      rule (badd_t bone_t bzero_t) bone_t;
      rule (badd_t (bd0_t p) bzero_t) (bd0_t p);
      rule (badd_t (bd1_t p) bzero_t) (bd1_t p);
      rule (badd_t bone_t bone_t) (bd0_t bone_t);
      rule (badd_t bone_t (bd0_t q)) (bd1_t q);
      rule (badd_t bone_t (bd1_t q)) (bd0_t (bsucc_t q));
      rule (badd_t (bd0_t p) bone_t) (bd1_t p);
      rule (badd_t (bd0_t p) (bd0_t q)) (bd0_t (badd_t p q));
      rule (badd_t (bd0_t p) (bd1_t q)) (bd1_t (badd_t p q));
      rule (badd_t (bd1_t p) bone_t) (bd0_t (bsucc_t p));
      rule (badd_t (bd1_t p) (bd0_t q)) (bd1_t (badd_t p q));
      rule (badd_t (bd1_t p) (bd1_t q)) (bd0_t (badd_carry_t p q));
      rule (badd_carry_t bone_t bone_t) (bd1_t bone_t);
      rule (badd_carry_t bone_t (bd0_t q)) (bd0_t (bsucc_t q));
      rule (badd_carry_t bone_t (bd1_t q)) (bd1_t (bsucc_t q));
      rule (badd_carry_t (bd0_t p) bone_t) (bd0_t (bsucc_t p));
      rule (badd_carry_t (bd0_t p) (bd0_t q)) (bd1_t (badd_t p q));
      rule (badd_carry_t (bd0_t p) (bd1_t q)) (bd0_t (badd_carry_t p q));
      rule (badd_carry_t (bd1_t p) bone_t) (bd1_t (bsucc_t p));
      rule (badd_carry_t (bd1_t p) (bd0_t q)) (bd0_t (badd_carry_t p q));
      rule (badd_carry_t (bd1_t p) (bd1_t q)) (bd1_t (badd_carry_t p q));
      (* bmul (Coq [Pos.mul], double-and-add via [badd]). Every [y] shape is
         spelled out explicitly rather than left as one generic variable
         (e.g. [bmul(bd0 p, y) = bd0(bmul p y)]): if [y] were left bare,
         [bmul(bd0 p, bzero)] would match BOTH the intended [bmul(x, bzero) =
         bzero] clause AND that generic recursive clause -- producing the
         non-canonical [bd0(bzero)] from the second -- a genuine
         non-confluence, not just a style preference. *)
      rule (bmul_t bzero_t y) bzero_t;
      rule (bmul_t bone_t bzero_t) bzero_t;
      rule (bmul_t bone_t bone_t) bone_t;
      rule (bmul_t bone_t (bd0_t q)) (bd0_t q);
      rule (bmul_t bone_t (bd1_t q)) (bd1_t q);
      rule (bmul_t (bd0_t p) bzero_t) bzero_t;
      rule (bmul_t (bd0_t p) bone_t) (bd0_t p);
      rule (bmul_t (bd0_t p) (bd0_t q)) (bd0_t (bmul_t p (bd0_t q)));
      rule (bmul_t (bd0_t p) (bd1_t q)) (bd0_t (bmul_t p (bd1_t q)));
      rule (bmul_t (bd1_t p) bzero_t) bzero_t;
      rule (bmul_t (bd1_t p) bone_t) (bd1_t p);
      rule
        (bmul_t (bd1_t p) (bd0_t q))
        (badd_t (bd0_t q) (bd0_t (bmul_t p (bd0_t q))));
      rule
        (bmul_t (bd1_t p) (bd1_t q))
        (badd_t (bd1_t q) (bd0_t (bmul_t p (bd1_t q))));
      (* bcompare/bcompare_cont (Coq [Pos.compare_cont]): [r] threads the
         tentative verdict from the lowest bit compared so far; a differing
         bit overrides it with a fresh verdict, an equal bit passes it
         through unchanged, so by the time both sides bottom out at [bone]
         the most-significant differing bit's override is what survives --
         MSB-first comparison via LSB-first recursion, without a separate
         bit-length computation. All 16 (x, y) shape pairs are disjoint. *)
      rule (bcompare_t x y) (bcompare_cont_t beq_kind_t x y);
      rule (bcompare_cont_t r bzero_t bzero_t) r;
      rule (bcompare_cont_t r bzero_t bone_t) blt_kind_t;
      rule (bcompare_cont_t r bzero_t (bd0_t q)) blt_kind_t;
      rule (bcompare_cont_t r bzero_t (bd1_t q)) blt_kind_t;
      rule (bcompare_cont_t r bone_t bzero_t) bgt_kind_t;
      rule (bcompare_cont_t r (bd0_t p) bzero_t) bgt_kind_t;
      rule (bcompare_cont_t r (bd1_t p) bzero_t) bgt_kind_t;
      rule (bcompare_cont_t r bone_t bone_t) r;
      rule (bcompare_cont_t r bone_t (bd0_t q)) blt_kind_t;
      rule (bcompare_cont_t r bone_t (bd1_t q)) blt_kind_t;
      rule (bcompare_cont_t r (bd0_t p) bone_t) bgt_kind_t;
      rule (bcompare_cont_t r (bd1_t p) bone_t) bgt_kind_t;
      rule (bcompare_cont_t r (bd0_t p) (bd0_t q)) (bcompare_cont_t r p q);
      rule
        (bcompare_cont_t r (bd0_t p) (bd1_t q))
        (bcompare_cont_t blt_kind_t p q);
      rule
        (bcompare_cont_t r (bd1_t p) (bd0_t q))
        (bcompare_cont_t bgt_kind_t p q);
      rule (bcompare_cont_t r (bd1_t p) (bd1_t q)) (bcompare_cont_t r p q);
      (* [bleq]/[blt] read off [bcompare]'s result via a boolean-dispatch
         auxiliary over the 3 ground [Bcmp] constants -- the same disjoint-
         ground-pattern idiom [pow_aux]/[div_int_aux] use above, rather than
         requiring [eq_t] to also cover [Bcmp]. *)
      rule (ble_of_cmp_t blt_kind_t) yes;
      rule (ble_of_cmp_t beq_kind_t) yes;
      rule (ble_of_cmp_t bgt_kind_t) no;
      rule (blt_of_cmp_t blt_kind_t) yes;
      rule (blt_of_cmp_t beq_kind_t) no;
      rule (blt_of_cmp_t bgt_kind_t) no;
      rule (bleq_t x y) (ble_of_cmp_t (bcompare_t x y));
      rule (blt_t x y) (blt_of_cmp_t (bcompare_t x y));
      (* bsub/bsub_mask/bsub_mask_carry (Coq [Pos.sub_mask]/[sub_mask_carry]):
         truncated subtraction via a 3-valued mask ([bmask_nul] = exact 0,
         [bmask_neg] = would-be-negative, [bmask_pos r] = the positive
         difference [r]), mirroring how [badd]/[badd_carry] thread a carry.
         [bdouble_mask]/[bsucc_double_mask] double a mask's magnitude (2x /
         2x+1) while [bmask_nul]/[bmask_neg] pass through unchanged (0 and
         "negative" both stay fixed under doubling). Every clause below was
         re-derived and hand-verified against concrete values (NOT
         transcribed from memory of Coq's source, which is fiddlier here
         than [add]'s carry table) -- see the binary-encoding plan's note
         that this primitive needs extra scrutiny. *)
      rule (bdouble_mask_t bmask_nul_t) bmask_nul_t;
      rule (bdouble_mask_t bmask_neg_t) bmask_neg_t;
      rule (bdouble_mask_t (bmask_pos_t p)) (bmask_pos_t (bd0_t p));
      rule (bsucc_double_mask_t bmask_nul_t) (bmask_pos_t bone_t);
      rule (bsucc_double_mask_t bmask_neg_t) bmask_neg_t;
      rule (bsucc_double_mask_t (bmask_pos_t p)) (bmask_pos_t (bd1_t p));
      rule (bsub_mask_t bzero_t bzero_t) bmask_nul_t;
      rule (bsub_mask_t bzero_t bone_t) bmask_neg_t;
      rule (bsub_mask_t bzero_t (bd0_t q)) bmask_neg_t;
      rule (bsub_mask_t bzero_t (bd1_t q)) bmask_neg_t;
      rule (bsub_mask_t bone_t bzero_t) (bmask_pos_t bone_t);
      rule (bsub_mask_t (bd0_t p) bzero_t) (bmask_pos_t (bd0_t p));
      rule (bsub_mask_t (bd1_t p) bzero_t) (bmask_pos_t (bd1_t p));
      rule (bsub_mask_t bone_t bone_t) bmask_nul_t;
      rule (bsub_mask_t bone_t (bd0_t q)) bmask_neg_t;
      rule (bsub_mask_t bone_t (bd1_t q)) bmask_neg_t;
      rule (bsub_mask_t (bd0_t p) bone_t) (bmask_pos_t (bpred_double_t p));
      rule (bsub_mask_t (bd1_t p) bone_t) (bmask_pos_t (bd0_t p));
      rule (bsub_mask_t (bd0_t p) (bd0_t q)) (bdouble_mask_t (bsub_mask_t p q));
      rule
        (bsub_mask_t (bd0_t p) (bd1_t q))
        (bsucc_double_mask_t (bsub_mask_carry_t p q));
      rule
        (bsub_mask_t (bd1_t p) (bd0_t q))
        (bsucc_double_mask_t (bsub_mask_t p q));
      rule (bsub_mask_t (bd1_t p) (bd1_t q)) (bdouble_mask_t (bsub_mask_t p q));
      rule (bsub_mask_carry_t bone_t bone_t) bmask_neg_t;
      rule (bsub_mask_carry_t bone_t (bd0_t q)) bmask_neg_t;
      rule (bsub_mask_carry_t bone_t (bd1_t q)) bmask_neg_t;
      rule
        (bsub_mask_carry_t (bd0_t p) bone_t)
        (bdouble_mask_t (bsub_mask_t p bone_t));
      rule (bsub_mask_carry_t (bd1_t p) bone_t) (bmask_pos_t (bpred_double_t p));
      rule
        (bsub_mask_carry_t (bd0_t p) (bd0_t q))
        (bsucc_double_mask_t (bsub_mask_carry_t p q));
      rule
        (bsub_mask_carry_t (bd0_t p) (bd1_t q))
        (bdouble_mask_t (bsub_mask_carry_t p q));
      rule
        (bsub_mask_carry_t (bd1_t p) (bd0_t q))
        (bdouble_mask_t (bsub_mask_t p q));
      rule
        (bsub_mask_carry_t (bd1_t p) (bd1_t q))
        (bsucc_double_mask_t (bsub_mask_carry_t p q));
      rule (bsub_of_mask_t bmask_nul_t) bzero_t;
      rule (bsub_of_mask_t bmask_neg_t) bzero_t;
      rule (bsub_of_mask_t (bmask_pos_t p)) p;
      rule (bsub_t x y) (bsub_of_mask_t (bsub_mask_t x y));
      (* bdiv/bmod: genuine O(log n)-step binary long division, NOT a
         transliteration of the old Peano [div]/[mod]'s repeated-subtraction
         shape (which stayed O(x/y) rewrite steps regardless of term size, and
         which nat [div]/[mod] now delegate here to avoid -- see {!Ctrs_term}'s
         doc comment for the full derivation). [bring0]/
         [bring1] double-and-append-a-bit in O(1); [bdivmod_pos] recurses on
         the DIVIDEND's shape, bottoming out at its most-significant bit
         first, so each level (as the recursion unwinds) brings down one
         more (less significant) bit, compares against the fixed divisor
         [y], and conditionally subtracts -- restoring binary long division.
         [y] is never pattern-matched inside the recursion (only compared/
         subtracted against), so [bdiv]/[bmod] guard [y = bzero] once at the
         top (no rule -- stuck, matching [div_t]/[mod_t]'s existing
         div-by-zero convention) rather than threading the guard through
         every recursive call. *)
      rule (bring0_t bzero_t) bzero_t;
      rule (bring0_t bone_t) (bd0_t bone_t);
      rule (bring0_t (bd0_t p)) (bd0_t (bd0_t p));
      rule (bring0_t (bd1_t p)) (bd0_t (bd1_t p));
      rule (bring1_t bzero_t) bone_t;
      rule (bring1_t bone_t) (bd1_t bone_t);
      rule (bring1_t (bd0_t p)) (bd1_t (bd0_t p));
      rule (bring1_t (bd1_t p)) (bd1_t (bd1_t p));
      rule (bquot_t (bdivmod_t q r)) q;
      rule (brem_t (bdivmod_t q r)) r;
      rule (bdivmod_dispatch_t yes q r2 y) (bdivmod_t (bring0_t q) r2);
      rule (bdivmod_dispatch_t no q r2 y) (bdivmod_t (bring1_t q) (bsub_t r2 y));
      rule (bdivmod_combine_t q r2 y) (bdivmod_dispatch_t (blt_t r2 y) q r2 y);
      rule (bdivmod_base_t yes y) (bdivmod_t bone_t bzero_t);
      rule (bdivmod_base_t no y) (bdivmod_t bzero_t bone_t);
      rule (bdivmod_pos_t bzero_t y) (bdivmod_t bzero_t bzero_t);
      rule (bdivmod_pos_t bone_t y) (bdivmod_base_t (bleq_t y bone_t) y);
      rule (bdivmod_pos_t (bd0_t p) y) (bdivmod_step0_t (bdivmod_pos_t p y) y);
      rule (bdivmod_pos_t (bd1_t p) y) (bdivmod_step1_t (bdivmod_pos_t p y) y);
      rule
        (bdivmod_step0_t (bdivmod_t q r) y)
        (bdivmod_combine_t q (bring0_t r) y);
      rule
        (bdivmod_step1_t (bdivmod_t q r) y)
        (bdivmod_combine_t q (bring1_t r) y);
      rule (bdiv_t x bone_t) (bquot_t (bdivmod_pos_t x bone_t));
      rule (bdiv_t x (bd0_t q)) (bquot_t (bdivmod_pos_t x (bd0_t q)));
      rule (bdiv_t x (bd1_t q)) (bquot_t (bdivmod_pos_t x (bd1_t q)));
      rule (bmod_t x bone_t) (brem_t (bdivmod_pos_t x bone_t));
      rule (bmod_t x (bd0_t q)) (brem_t (bdivmod_pos_t x (bd0_t q)));
      rule (bmod_t x (bd1_t q)) (brem_t (bdivmod_pos_t x (bd1_t q)));
      (* bpow_nat: binary base AND binary exponent. The exponent (a bit-width)
         is now binary too, so it dispatches on [bis_zero]/[bpred] through
         [bpow_nat_aux] rather than a [succ] pattern -- the same idiom as
         [pow]/[pow_int] (a binary exponent like 2 is [bd0 bone], never
         [bsucc _]). *)
      rule (bpow_nat_t x y) (app_t "bpow_nat_aux" [ bis_zero_t y; x; y ]);
      rule (app_t "bpow_nat_aux" [ yes; x; y ]) bone_t;
      rule
        (app_t "bpow_nat_aux" [ no; x; y ])
        (bmul_t x (bpow_nat_t x (bpred_t y)));
      (* lists. [len] builds a nat via [succ] (= [bsucc], canonical -- a builder,
         not a matcher). The index-recursive operations ([idx]/[take]/[drop]/
         [upd_idx]) cannot match a [succ] pattern (a binary index like 2 is
         [bd0 bone], never [bsucc _]), so each dispatches on [bis_zero]/[bpred]
         through a boolean auxiliary, mirroring the arithmetic above.
         Out-of-bounds cases (e.g. [idx(nil, _)]) stay irreducible -- partial, as
         with div. *)
      rule (len_t nil_t) zero_t;
      rule (len_t (cons_t x xs)) (succ_t (len_t xs));
      rule (cat_t nil_t ys) ys;
      rule (cat_t (cons_t x xs) ys) (cons_t x (cat_t xs ys));
      rule (mem_t x nil_t) no;
      rule (mem_t x (cons_t y ys)) (or_t (eq_t x y) (mem_t x ys));
      rule (idx_t (cons_t x xs) i) (app_t "idx_aux" [ bis_zero_t i; x; xs; i ]);
      rule (app_t "idx_aux" [ yes; x; xs; i ]) x;
      rule (app_t "idx_aux" [ no; x; xs; i ]) (idx_t xs (bpred_t i));
      rule (take_t xs i) (app_t "take_aux" [ bis_zero_t i; xs; i ]);
      rule (app_t "take_aux" [ yes; xs; i ]) nil_t;
      rule
        (app_t "take_aux" [ no; cons_t x xs; i ])
        (cons_t x (take_t xs (bpred_t i)));
      rule (drop_t xs i) (app_t "drop_aux" [ bis_zero_t i; xs; i ]);
      rule (app_t "drop_aux" [ yes; xs; i ]) xs;
      rule (app_t "drop_aux" [ no; cons_t x xs; i ]) (drop_t xs (bpred_t i));
      rule (slice_t xs i n) (take_t (drop_t xs i) n);
      rule
        (upd_idx_t (cons_t x xs) i v)
        (app_t "upd_idx_aux" [ bis_zero_t i; x; xs; i; v ]);
      rule (app_t "upd_idx_aux" [ yes; x; xs; i; v ]) (cons_t v xs);
      rule
        (app_t "upd_idx_aux" [ no; x; xs; i; v ])
        (cons_t x (upd_idx_t xs (bpred_t i) v));
      rule (upd_slice_t xs i n v)
        (cat_t (take_t xs i) (cat_t v (drop_t xs (add_t i n))));
      (* the [nat] membership predicate behind [e <: nat] ([sub_pred]): a
         non-negative integer ([int_pos]) and a bare binary nat
         ([bzero]/[bone]/[bd0]/[bd1]) qualify, a negative integer ([int_neg])
         does not -- mirroring [interp.subtyp]'s [NatT] case over both
         representations. *)
      rule (app_t "sub_nat" [ int_pos_t x ]) yes;
      rule (app_t "sub_nat" [ int_neg_t x ]) no;
      rule (app_t "sub_nat" [ bzero_t ]) yes;
      rule (app_t "sub_nat" [ bone_t ]) yes;
      rule (app_t "sub_nat" [ bd0_t x ]) yes;
      rule (app_t "sub_nat" [ bd1_t x ]) yes;
      (* option / list matchers used by [conds_of_prems] *)
      rule (app_t "match_some" [ some_t x ]) yes;
      rule (app_t "match_some" [ none_t ]) no;
      rule (app_t "match_none" [ none_t ]) yes;
      rule (app_t "match_none" [ some_t x ]) no;
      rule (app_t "match_cons" [ cons_t x xs ]) yes;
      rule (app_t "match_cons" [ nil_t ]) no;
      rule (app_t "match_nil" [ nil_t ]) yes;
      rule (app_t "match_nil" [ cons_t x xs ]) no;
      (* The generic equality is what the sites whose operand type is not
         statically known reach -- the collection builtins' map keys
         ({!Builtin}), which are polymorphic. Everything with a static type goes
         through its own [eq_<T>] ({!To_ctrs.eq_pred}) instead, so this symbol
         no longer carries a rule per constructor PAIR: the reflexive equation
         decides the diagonal for any key whatever, and the off-diagonal is
         decided by the execution surface's [owise] fallback
         ({!To_maude.generic_eq_fallback}) -- exactly the treatment [eqg]
         already gets ({!To_mfe}). *)
      rule (eq_t x x) yes;
      (* Structural equality over the built-in sorts. [BNatV] equality is stated
         over the four BINARY constructors and nothing else: [zero_t]/[succ_t]
         are aliases onto [bzero]/[bsucc], and [bsucc] is a DEFINED symbol, so
         the Peano-shaped clauses this table used to also carry
         ([eq(succ(x), succ(y)) = eq(x, y)] and its two base cases) put a defined
         symbol in an lhs and overlapped every binary clause it can unfold into
         -- 8 critical pairs the CRC reports on any slice reaching generic [eq]
         (measured on [$instantiable_package]). They were redundant as well as
         harmful: [bsucc] is total on the four constructors, so a ground
         successor term reaches one of them before the comparison, and
         [eq(zero, zero)] was a duplicate of [eq(bzero, bzero)] outright. All 16
         binary shape pairs are disjoint; [int_pos]/[int_neg] wrap a [BNatV]
         magnitude and are disjoint from it in turn. *)
      rule (eq_t bzero_t bzero_t) yes;
      rule (eq_t bzero_t bone_t) no;
      rule (eq_t bzero_t (bd0_t q)) no;
      rule (eq_t bzero_t (bd1_t q)) no;
      rule (eq_t bone_t bzero_t) no;
      rule (eq_t bone_t bone_t) yes;
      rule (eq_t bone_t (bd0_t q)) no;
      rule (eq_t bone_t (bd1_t q)) no;
      rule (eq_t (bd0_t p) bzero_t) no;
      rule (eq_t (bd0_t p) bone_t) no;
      rule (eq_t (bd0_t p) (bd0_t q)) (eq_t p q);
      rule (eq_t (bd0_t p) (bd1_t q)) no;
      rule (eq_t (bd1_t p) bzero_t) no;
      rule (eq_t (bd1_t p) bone_t) no;
      rule (eq_t (bd1_t p) (bd0_t q)) no;
      rule (eq_t (bd1_t p) (bd1_t q)) (eq_t p q);
      rule (eq_t (int_pos_t x) (int_pos_t y)) (eq_t x y);
      rule (eq_t (int_neg_t x) (int_neg_t y)) (eq_t x y);
      rule (eq_t (int_pos_t x) (int_neg_t y)) no;
      rule (eq_t (int_neg_t x) (int_pos_t y)) no;
      rule (eq_t true_t true_t) yes;
      rule (eq_t true_t false_t) no;
      rule (eq_t false_t true_t) no;
      rule (eq_t false_t false_t) yes;
      rule (eq_t none_t none_t) yes;
      rule (eq_t none_t (some_t y)) no;
      rule (eq_t (some_t x) none_t) no;
      rule (eq_t (some_t x) (some_t y)) (eq_t x y);
      (* one equation for the whole byte alphabet: a character carries its
         codepoint ({!Ctrs_term.chr_ctor}), so two characters are equal exactly
         when their codepoints are, decided by the binary-nat rules above *)
      rule (eq_t (chr_of_t p) (chr_of_t q)) (eq_t p q);
      rule (eq_t nil_t nil_t) yes;
      rule (eq_t nil_t (cons_t y ys)) no;
      rule (eq_t (cons_t x xs) nil_t) no;
      rule (eq_t (cons_t x xs) (cons_t y ys)) (and_t (eq_t x y) (eq_t xs ys));
    ]
  in
  match scalars with
  | Structural -> all
  | Native -> List.filter kept_in_native all
