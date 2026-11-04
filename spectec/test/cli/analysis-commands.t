Analysis subcommands over the impty base spec: the termination TRS emitter,
the removed SCC command, default signature pruning, and symbol counts.
External Maude and AProVE are not required; the proof case uses a local stub.

  $ SPEC=../../specs/impty/base/spec.spectec

termination --emit prints the structure-preserving unraveling of one
slice as a TPDB TRS (stats go to stderr):

  $ spectec termination --emit --symbol '$lookup_id_type' $SPEC
  (VAR K_h K_query K_t__hd V_h V_t__hd __rest __rest0 crcv1 crcv2 iterbind_0 pair_lt_K_comma_V_gt x x0 x1 xs y)
  (RULES
    not(true) -> false
    not(false) -> true
    and(true, y) -> y
    and(false, y) -> false
    match_cons(cons(x, xs)) -> true
    match_cons(nil) -> false
    d_itermap_K_t_minus_gt_V_t_list_1_fK_t_minus_gt_V_t_pair(nil) -> nil
    d_itermap_K_t_minus_gt_V_t_list_1_fK_t_minus_gt_V_t_pair(cons(variant_pair_minus_gt_2(K_t__hd, V_t__hd), __rest0)) -> cons(variant_pair_minus_gt_2(K_t__hd, V_t__hd), d_itermap_K_t_minus_gt_V_t_list_1_fK_t_minus_gt_V_t_pair(__rest0))
    d_iterproj_K_t_minus_gt_V_t_list_K_t(nil) -> nil
    d_iterproj_K_t_minus_gt_V_t_list_K_t(cons(variant_pair_minus_gt_2(K_t__hd, V_t__hd), __rest)) -> cons(K_t__hd, d_iterproj_K_t_minus_gt_V_t_list_K_t(__rest))
    d_iterproj_K_t_minus_gt_V_t_list_V_t(nil) -> nil
    d_iterproj_K_t_minus_gt_V_t_list_V_t(cons(variant_pair_minus_gt_2(K_t__hd, V_t__hd), __rest)) -> cons(V_t__hd, d_iterproj_K_t_minus_gt_V_t_list_V_t(__rest))
    d_lookup_id_type(nil, K_query) -> none
    d_lookup_id_type(pair_lt_K_comma_V_gt, K_query) -> u_1(match_cons(pair_lt_K_comma_V_gt), pair_lt_K_comma_V_gt, K_query)
    u_1(true, pair_lt_K_comma_V_gt, K_query) -> u_2(pair_lt_K_comma_V_gt, pair_lt_K_comma_V_gt, K_query)
    u_2(cons(variant_pair_minus_gt_2(K_query, V_h), iterbind_0), pair_lt_K_comma_V_gt, K_query) -> u_3(d_iterproj_K_t_minus_gt_V_t_list_V_t(iterbind_0), pair_lt_K_comma_V_gt, K_query, V_h, iterbind_0)
    u_3(crcv1, pair_lt_K_comma_V_gt, K_query, V_h, iterbind_0) -> u_4(d_iterproj_K_t_minus_gt_V_t_list_K_t(iterbind_0), pair_lt_K_comma_V_gt, K_query, V_h, iterbind_0, crcv1)
    u_4(crcv2, pair_lt_K_comma_V_gt, K_query, V_h, iterbind_0, crcv1) -> some(V_h)
    d_lookup_id_type(pair_lt_K_comma_V_gt, K_query) -> u_5(match_cons(pair_lt_K_comma_V_gt), pair_lt_K_comma_V_gt, K_query)
    u_5(true, pair_lt_K_comma_V_gt, K_query) -> u_6(pair_lt_K_comma_V_gt, pair_lt_K_comma_V_gt, K_query)
    u_6(cons(variant_pair_minus_gt_2(K_h, V_h), iterbind_0), pair_lt_K_comma_V_gt, K_query) -> u_7(or(eqg(pair_lt_K_comma_V_gt, nil), and(and(and(match_cons(pair_lt_K_comma_V_gt), eqg(K_query, proj_variant_pair_minus_gt_2_0(proj_cons_0(pair_lt_K_comma_V_gt)))), not(isStuckHead(d_iterproj_K_t_minus_gt_V_t_list_V_t(proj_cons_1(pair_lt_K_comma_V_gt))))), not(isStuckHead(d_iterproj_K_t_minus_gt_V_t_list_K_t(proj_cons_1(pair_lt_K_comma_V_gt)))))), pair_lt_K_comma_V_gt, K_query, K_h, V_h, iterbind_0)
    u_7(false, pair_lt_K_comma_V_gt, K_query, K_h, V_h, iterbind_0) -> d_lookup_id_type(d_itermap_K_t_minus_gt_V_t_list_1_fK_t_minus_gt_V_t_pair(iterbind_0), K_query)
    eqg(x, x) -> true
    proj_cons_0(cons(x0, x1)) -> x0
    proj_cons_1(cons(x0, x1)) -> x1
    proj_variant_pair_minus_gt_2_0(variant_pair_minus_gt_2(x0, x1)) -> x0
  )
  reflect: subty expansion: 6 clause(s) -> 6 clone(s) (4 dead, 0 vacuous guard(s) dropped)
  reflect: no complement split for $lookup_id_type (conditional owise)
  reflect: no complement split for $lookup_id_value (conditional owise)
  reflect: 2 owise rule(s) reflected, 0 complement-enumerated, 0 kept
  eqs=19 rules=26 u=7 vars=16 owise=0

A symbol with no rules is DEGENERATE, not an error (no stats to report):

  $ SPECTEC_APROVE_BIN=/bin/false spectec termination --symbol '$no-such-symbol' $SPEC 2>/dev/null | cut -f1-3
  $no-such-symbol	aprove	DEGENERATE

Usage errors still require explicit roots; SCC is no longer a command:

  $ spectec termination $SPEC
  termination needs --symbol NAME (repeatable) or --all
  [2]

  $ spectec confluence $SPEC
  confluence needs --symbol NAME (repeatable) or --all
  [2]

  $ spectec --help | grep '^    scc '
  [1]

Analysis emission now prunes the signature by default, while retaining the
whole-system predicate domain evidence for a slice:

  $ spectec rewrite --ctrs --symbol '$lookup_id_type' $SPEC 2>/dev/null | grep -c '^  op '
  20

The size-ranked listing has one symbol and rule count per line:

  $ spectec rewrite --list-symbols $SPEC 2>/dev/null | head -3
  $lookup_id_type	19
  $lookup_id_value	19
  Check_expr	30

An AProVE stand-in confirms that the plain proof path writes a tree through
--proof-dir without running the external prover:

  $ cat > aprove-stub <<'SH'
  > #!/bin/sh
  > printf 'YES\nproof of fake.trs\n(0) QTRS\n(1) YES\n----------------------------------------\n'
  > SH
  $ chmod +x aprove-stub
  $ export SPECTEC_APROVE_BIN="$PWD/aprove-stub"
  $ spectec termination --symbol '$lookup_id_type' --timeout 1 --proof-dir proofs $SPEC >/dev/null 2>/dev/null
  $ grep -F '(0) QTRS' proofs/aprove/*.proof
  (0) QTRS
