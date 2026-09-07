Analysis subcommands over the impty base spec: the termination TRS emitter,
the SCC module emitter, and the signature prune. Everything here is
translation-only (no Maude/AProVE), so the snapshots are stable.

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

  $ spectec termination --symbol '$no-such-symbol' $SPEC
  $no-such-symbol	DEGENERATE	-	-	-
  reflect: subty expansion: 6 clause(s) -> 6 clone(s) (4 dead, 0 vacuous guard(s) dropped)
  reflect: no complement split for $lookup_id_type (conditional owise)
  reflect: no complement split for $lookup_id_value (conditional owise)
  reflect: 2 owise rule(s) reflected, 0 complement-enumerated, 0 kept

Usage errors exit 2:

  $ spectec termination $SPEC
  termination needs --symbol NAME (repeatable) or --all
  [2]

  $ spectec scc $SPEC
  scc needs --symbol NAME (repeatable) or --all
  [2]

confluence has the same per-symbol structure; it needs --symbol or --all too
(neither, or both, is a usage error):

  $ spectec confluence $SPEC
  confluence needs --symbol NAME (repeatable) or --all
  [2]

  $ spectec confluence --all --symbol '$lookup_id_type' $SPEC
  confluence needs --symbol NAME (repeatable) or --all
  [2]

scc --emit prints the exact checker input: an old-Full-Maude FUNCTIONAL module
with the BOOL includes off and the signature pruned:

  $ spectec scc --emit --symbol '$lookup_id_type' $SPEC | head -3
  reflect: subty expansion: 6 clause(s) -> 6 clone(s) (4 dead, 0 vacuous guard(s) dropped)
  reflect: no complement split for $lookup_id_type (conditional owise)
  reflect: no complement split for $lookup_id_value (conditional owise)
  reflect: 2 owise rule(s) reflected, 0 complement-enumerated, 0 kept
  unconditional: dropped conditions from 2 rule(s) (2 rhs replaced by lhs)
  unconditional: linearized 1 non-left-linear lhs
  (set include BOOL off .)
  (set include BOOL-OPS off .)
  (fmod SPEC is

An unknown symbol's slice is DEGENERATE for scc too (fidelity still reported):

  $ spectec scc --symbol '$no-such-symbol' $SPEC
  $no-such-symbol	DEGENERATE	exact
  reflect: subty expansion: 6 clause(s) -> 6 clone(s) (4 dead, 0 vacuous guard(s) dropped)
  reflect: no complement split for $lookup_id_type (conditional owise)
  reflect: no complement split for $lookup_id_value (conditional owise)
  reflect: 2 owise rule(s) reflected, 0 complement-enumerated, 0 kept

--prune-signature drops the op declarations the slice's rules never use; the
rule lines themselves are untouched:

  $ spectec rewrite --ctrs --symbol '$lookup_id_type' $SPEC | grep -c '^  op '
  reflect: subty expansion: 6 clause(s) -> 6 clone(s) (4 dead, 0 vacuous guard(s) dropped)
  reflect: no complement split for $lookup_id_type (conditional owise)
  reflect: no complement split for $lookup_id_value (conditional owise)
  reflect: 2 owise rule(s) reflected, 0 complement-enumerated, 0 kept
  35
  $ spectec rewrite --ctrs --symbol '$lookup_id_type' --prune-signature $SPEC | grep -c '^  op '
  reflect: subty expansion: 6 clause(s) -> 6 clone(s) (4 dead, 0 vacuous guard(s) dropped)
  reflect: no complement split for $lookup_id_type (conditional owise)
  reflect: no complement split for $lookup_id_value (conditional owise)
  reflect: 2 owise rule(s) reflected, 0 complement-enumerated, 0 kept
  20
  $ spectec rewrite --ctrs --symbol '$lookup_id_type' $SPEC | grep -E '^  c?eq ' > full.eqs
  reflect: subty expansion: 6 clause(s) -> 6 clone(s) (4 dead, 0 vacuous guard(s) dropped)
  reflect: no complement split for $lookup_id_type (conditional owise)
  reflect: no complement split for $lookup_id_value (conditional owise)
  reflect: 2 owise rule(s) reflected, 0 complement-enumerated, 0 kept
  $ spectec rewrite --ctrs --symbol '$lookup_id_type' --prune-signature $SPEC | grep -E '^  c?eq ' > pruned.eqs
  reflect: subty expansion: 6 clause(s) -> 6 clone(s) (4 dead, 0 vacuous guard(s) dropped)
  reflect: no complement split for $lookup_id_type (conditional owise)
  reflect: no complement split for $lookup_id_value (conditional owise)
  reflect: 2 owise rule(s) reflected, 0 complement-enumerated, 0 kept
  $ diff full.eqs pruned.eqs

rewrite --list-symbols lists the sliceable symbols (the names --symbol/--all
take); --sizes adds each slice's rule count, smallest first -- the cheap
tractability proxy that used to live on verify (stderr elided for a stable
snapshot):

  $ spectec rewrite --list-symbols $SPEC
  $lookup_id_type
  $lookup_id_value
  Check_expr
  Check_command
  Check_prog
  Eval_expr
  Eval_command
  Eval_prog
  Run_prog

  $ spectec rewrite --list-symbols --sizes $SPEC 2>/dev/null
  19	$lookup_id_type
  19	$lookup_id_value
  30	Check_expr
  36	Check_command
  37	Check_prog
  132	Eval_expr
  140	Eval_command
  141	Eval_prog
  163	Run_prog
