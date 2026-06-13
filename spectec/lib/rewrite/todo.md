# `to_ctrs` — remaining work

IL → CTRS translation ([to_ctrs.ml](to_ctrs.ml)). The `impty` specs translate
end-to-end and `specs/p4` now translates in full. This list is ordered by what
unblocks real-scale specs first.

## Done — iterations (`IterE` / `IterPr`)

Both compile to auxiliary recursive helpers over the `cons`/`nil` (or
`some`/`none`) spine, in lock-step over the co-iterated variables:

- **`IterE` value position** → a `$itermap` "map" helper (`cons`/`some`); a bare
  iterated variable `x*`/`x?` stays the list/option `x` itself.
- **`IterE` binder position** (clause/rule head pattern) → the iterated
  collection binds a fresh variable and `$unzip_*` conditions recover the element
  streams (a CTRS LHS must be a constructor pattern). Captured variables ride as
  leading parameters (non-left-linear, but faithful).
- **`IterPr`** → a `$iterall(...) == true` predicate when it binds nothing.
  When the iterated premise is a single relation call whose outputs are exactly
  the collected variables, it becomes an **`$iterapply`** map that carries the
  call result as the element (`cons($Rel(..,hd), $iterapply(..,tl))`,
  *unconditional*): a single output binds the stream directly
  (`$iterapply(...) == b`), several are split out with an `$iterproj_b` per
  output (the stream is then a stream of `tuple`s). Any other binding premise
  (compound inner, non-bare output) falls back to a per-output conditional
  `$itercollect_b(...) == b`. The `$iterapply` form is what input-moded
  relations-as-equations (the Maude backend) want: no per-output re-invocation,
  unconditional recursive rules.
- **`Simplify.collapse_rezip_iters`** first folds an unzip→re-zip round-trip
  (e.g. `$lookup`'s `(K_t -> V_t)*`) into one iterated variable, so it reduces to
  a plain list term rather than a helper.

Possible follow-ups: helper symbol descriptors are now length-bounded (`abbrev`
keeps a readable prefix plus a hash, applied in one place via
`iter_helper_sym`/`subty_helper_sym`); the binder-position `$unzip` for captured
bodies is non-left-linear, which can weaken confluence analysis.

## P1 — **confirmed bug**: `$itermap` (a defined function) emitted *inside a `:=` match pattern* → struct/header/header-union field access always stuck

The single biggest p4-old execution blocker. Surfaced by the p4-old ×
`p4_16_samples` survey (see repo-root `p4old_samples_stuck_analysis.md`;
539/748 STUCK) and bisected end-to-end on `constStruct.p4`
(`const bit<16> z = (bit<16>)s.x;`).

`Expr_ok` for `expr.field` on a struct/header/header-union value destructures the
canonicalized type with an equality premise whose LHS carries an iterated field
pattern:

```
-- if STRUCT _ { (typeIR_f nameIR_f ';')* } = $canon(typeIR_base)   -- and the
--    later $assoc(nameIR, (nameIR_f, typeIR_f)*) re-zips the two streams
```

This is an **unzip→re-zip round-trip** (unzip the field list into `nameIR_f*` /
`typeIR_f*`, re-zip them in `$assoc`). The emitted Maude condition is **wrong**:

```
tuple(STRUCT(-tid, $itermap-typeIR-f-nameIR-f-semi-list-2(nameIR-f, typeIR-f)), St2)
  := $canon(typeIR-base, St1)        -- $itermap-... is a DEFINED FUNCTION, op : Val Val -> Val
```

A `pattern := term` LHS in Maude must be a **constructor pattern**; a defined
function (`$itermap-…`, the field-list *rebuild* helper) on the pattern side can
never match the concrete `cons(fieldTypeIR(..), ..)` `$canon` returns → the
condition is unsatisfiable → the rule never fires → typing **any** `.field` access
on struct/header/header-union is stuck. Since field access is ubiquitous in P4,
this plausibly accounts for a large share of the 539 STUCK (verify by re-running
the survey after the fix).

The sibling **`tablestruct` rule is correct** for the identical shape: it binds the
field list to a fresh `iterbind-0:List` and recovers the streams with `$unzip-…`
conditions (the proper binder-position compilation, "**`IterE` binder position**"
above). So the correct code path already exists — only struct/header/header-union
take the wrong branch, emitting the value-position `$itermap` re-zip into pattern
position. Strongly suspect `Simplify.collapse_rezip_iters` mis-folding the
unzip→re-zip round-trip here (it is meant to fold exactly this pattern), leaving an
`$itermap` where a binder-`$unzip` was required. Fix in
[to_ctrs.ml](to_ctrs.ml)/[simplify.ml](simplify.ml); guard against ever placing a
`$`-function term on the LHS of an emitted `:=` condition. Both specs likely
affected (same member-access shape in `specs/p4`).

## P2 — approximations to revisit

- [ ] **`otherwise` (`ElsePr`) in the CTRS surface still → non-confluence**
  (the **Maude** backend is now faithful — see below). `conds_of_prem` maps
  `ElsePr -> []`, so in the COPS/TPDB output a fallthrough clause loses its
  "no earlier clause applied" guard and becomes an unconditional rule that
  overlaps the earlier ones. Concretely, `impty/base`'s `$lookup` emits two
  rules with the *same* LHS `$lookup(cons(variant_pair_..(K_h,V_h), pair), K)`:
  one conditional on `K_h == K` (→ `some(V_h)`), one unconditional
  (→ `$lookup(pair, K)`); at `K_h == K` both fire, a critical pair that does not
  join. The source is deterministic only because clauses are tried *in order* +
  `otherwise`.
  - **Done (Maude, commit 8de7b031).** A new `owise` flag on
    `Rewrite_system.rule` is set from `ElsePr` in `rule_of_clause` /
    `rule_of_rel_rule`; `To_maude` renders it as the `owise` equation attribute,
    expressing "no earlier clause applied" **without** negating earlier guards —
    so it sidesteps the relation-premise negation wall entirely. The COPS/TPDB
    printers deliberately ignore the flag (`spec.rewrite` golden unchanged).
    Caveat: a *relation* `otherwise` cannot use `owise` and is warned + dropped.
  - **Still open (CTRS / CoCoWeb).** Confluence checking reads the COPS output,
    which keeps dropping `otherwise`, so the critical pair above remains. The
    CTRS fix is to translate `otherwise` as the negation of the preceding sibling
    clauses' distinguishing guards (here add `eq(K_h, K) == false` to the second
    rule). Clean when those guards are equalities / `matches` / decidable boolean
    tests; **but when a prior guard is a relation premise it hits the same wall
    as `IfNotHoldPr`/`NeOp` below** — relations are value-returning
    (`Rel(in) -> out`, `-> true` for judgments, never `-> false`; failure =
    stuck), so `Rel(in) == false` is unsatisfiable and the negated branch goes
    dead. No negation-as-failure in plain CTRS; a real fix needs complement
    relations (a `not_Rel`) or decidable `-> false` rules. Tie this to the
    negation story below.
- [ ] **`NeOp` / `IfNotHoldPr`** are encoded as `... == false`, which Join
  semantics only approximates. Revisit once a negation story exists.
  - **Done (Maude, judgments).** `To_maude` now totalizes every *no-output*
    relation used under negation with a guarded `owise` complement
    (`ceq R(args) = bool(false) if isStuckHead(arg_i) = false [owise]`), so an
    `IfNotHoldPr` premise on a judgment is decidable in execution — in p4-old
    exactly `Type_alpha` and `Cast_impl`, which is what brought
    `$coerce_unary`'s cast clause (and every implicit-coercion path: `const`
    initializers, assignments, `Parameter_ok`, ...) to life. Closed-world:
    sound when the positive clauses cover every holding instance; a stuck
    application deeper than the argument heads is still absorbed to `false`.
    Output-carrying or rule-mode relations are skipped with a warning (a
    complement there would turn failure into a value and break stuck
    propagation) — their negated premises stay unsatisfiable. The CTRS/COPS
    surface is untouched.
## Done — casts & subtype (`UpCastE`/`DownCastE`/`SubE`)

- Value casts: non-numeric casts are transparent (faithful — `interp.upcast` is
  identity on variants/structs); `Simplify` keeps nat<->int casts and now
  resolves them through named aliases (`syntax byte = nat`) and distributes
  tuple casts into components (`resolve_num_typ`, `is_num_cast`, `normalize_deep`).
- `SubE` is the structural `sub_pred` (`to_ctrs`): `sub_nat`/`true` for scalars,
  `subty_<T>` (recursing into the payload) for named types, `subty_tup`/
  `subty_list`/`subty_opt` helpers for tuples/iterations; a type-parameter target
  is approximated `-> true`. This removes the stuck rule-less `sub_anon`. Renamed
  `sub_` → `subty_` to avoid the arithmetic `sub_int` clash.
- **Deferred (negation story):** `-> false` totality for `subty_<T>` on
  non-member variant cases — needed only for *negative* subtype use, the same
  complement / negation-as-failure wall as `otherwise`/`IfNotHoldPr`/`NeOp` below.
- Unary minus injects `int_pos` once at the magnitude leaf. `term_of_unop` takes
  `~operand_is_int` (`yields_int` at the call site) and skips the injection when
  the operand already denotes a signed int, so the elaborator typing each level
  of `-(-n)` as nat no longer double-injects (single unary minus is unchanged).
  The prelude's involutive `negate_int(negate_int(x)) -> x` then cancels the
  residual double negation even on a symbolic operand; its overlap with the
  structural `negate_int` rules stays confluent (every critical pair joins).

## P3 — robustness / quality

- [ ] **Sanitizer collisions.** Distinct notations can still sanitize to the same
  symbol. Arity is folded in (`variant_<O>_<C>_<n>`), which removes same-atom
  different-arity clashes, but same-atom same-arity collisions remain possible.
  Consider a global symbol table with disambiguation.
- [ ] **Relation names share the prelude namespace.** `rel_sym id = sanitize
  id.it` carries *no* prefix (unlike `func_sym`, which prepends `$`), so a
  relation or `RelD` whose sanitized name equals a fixed prelude/list symbol
  (`eq`, `add`, `len`, `cat`, `mem`, `idx`, `slice`, `and`, `or`, `not`, `cons`,
  `nil`, `some`, `none`, `tuple`, `zero`, `succ`, `div`, `mod`, `sub_nat`,
  `match_some`, …) silently *merges* its rules into the prelude's — a soundness
  hazard, not just an aesthetic clash. Distinct from the same-atom/same-arity
  sanitizer note above (that is constructor-vs-constructor; this is
  relation-vs-prelude). `impty`/`p4` happen not to trip it (their relations are
  notation/`Capitalized`), but nothing enforces it. Fix = give `rel_sym` its own
  namespace prefix (e.g. `rel_`), but that re-spells every relation symbol, so it
  changes all output and the golden — needs a deliberate decision + golden bump,
  hence parked here rather than slipped into a behavior-preserving change.
- [ ] **`term_of_num` is partial / unary.** `Bigint.to_int_exn (Num.to_int n)`
  raises on a literal that overflows an OCaml `int` (and on a non-integer `Num.t`
  if one ever reaches here); even short of that, `peano_of_int` is a *unary*
  encoding, so a literal like `1000000` expands to a million `succ`s and is
  unusable. Real specs with large numeric literals need either a binary nat
  encoding or to keep big literals opaque. Decide before any spec leans on large
  constants.
- [ ] **`BuiltinDecD`** — mostly modeled. The collection builtins
  (map/set/list/text) emit CTRS rules via [builtin.ml](builtin.ml) (both
  pipelines); the numeric builtins (`$pow2`/`$shl`/`$shr`/`$shr_arith`/
  `$bneg`/`$band`/`$bor`/`$bxor`/`$bitacc`/`$to_bitstr`/`$to_int`/`$sum`/
  `$max`/`$min`) and the text builtins (`$int_to_text`, `$strip_prefix`/
  `$strip_suffix`) are delegation equations over Maude's built-ins
  ([to_maude.ml](to_maude.ml) `delegation_eqs`) — **Maude pipeline only**; the
  analysis CTRS still has no rules for the numeric ones (unary Peano cannot
  express them). `$bin_satplus`/`$bin_satminus` (clause-less in p4-old, no
  OCaml implementation either) now emit rules in [builtin.ml](builtin.ml),
  mirroring the new p4 spec's clause definitions — clamping verified by
  isolated reduces and through `Eval_static`; the *typing* of a source-level
  `|+|` still stops at the `DefA` erasure below. `$fresh_typeId`/`$fresh_tid`
  (the stateful gensym, deferred then **implemented 2026-06-12**) is modeled
  by automatic state threading ([gensym.ml](gensym.ml)): the state is the
  last issued name and issuing appends a prime
  (`$fresh_typeId(st) -> tuple(cat(st, "'"), cat(st, "'"))`, seed `"FRESH"`),
  so issued names collide neither with P4 identifiers (no prime in the
  lexer) nor with each other (strictly growing) — full gensym fidelity on
  both pipelines. `Gensym.thread` gives every fresh-reaching symbol a
  trailing state argument and a `tuple(result, state')` result;
  `Prem_env` keeps fresh-reaching calls out of the equivalence classes so
  `Simplify` never duplicates one issuance into many. Caveats: issued-name
  length grows linearly with the issue count (the issuing rule is isolated
  in `Gensym.issue_rule`; swap it for a Maude-only `$int_to_text` counter
  spelling if that ever bites), and the backends spell names differently
  (interpreter `FRESH__0`, rewrite `FRESH'`) — same semantics, different
  alphabets.
- [x] **Iterated option-equality premises** (`(id_arg? = eps)*`, the
  `$find_overloaded` "all arguments unnamed" guard on BOTH specs) — **fixed
  2026-06-13** by an empty-binder guard in `Prem_env.subst_exp`'s `IterE`
  branch. The recorded "suspected mechanism" (collapse_rezip_iters' vanish
  gate / copy equality) was wrong — collapse was only the last link. Actual
  chain: `env_of_prem`'s `IterPr` branch lifts the guard's element-level pair
  `id_arg? ≡ ?()` into the block env (load-bearing for the *some*-match
  clauses, so the leak itself must stay); substituting the variable-free `?()`
  into the head's depth-2 `IterE` body made `relift_vars` compute an EMPTY
  binder list, leaving the degenerate `?()*{}` severed from `id_arg` and the
  length; the *some* clause's premises folded the same way (guard went dead →
  dropped), the *none* clause's `IfPr` premises survived verbatim (orphaned);
  `collapse_rezip_iters` then erased the constant head (`vanish = ∅` passes
  the gate vacuously) to the observed fresh `x*`. Fix: refuse a substitution
  whose `relift_vars` result would EMPTY a previously non-empty binder list
  (binder fidelity — a reshaped iteration must keep co-varying with
  something), plus a backstop in `collapse_rezip_iters.try_collapse` skipping
  variable-free bodies (collapsing `?()*{}` erases the all-none shape
  constraint, it doesn't fold a round-trip). Both clauses now keep the
  elaborated head `id_arg?*` + the `(if id_arg? = ?())*` guard + `|id_arg?*|`,
  compiled as `$unzip(iterbind) == id_arg` / `$iterall(id_arg) == true` /
  `len($itermap(id_arg))` — the faithful target shape under the original
  variable. Verified: impty COPS+Maude goldens byte-identical; simplified-IL
  diff touches ONLY these clauses on both specs; `$find_overloaded(empty, …)`
  reduces to `none` on both specs (the BLOCK→GLOBAL fallthrough lives);
  p4-old `RoutineType_ok` on `f(8w0)` vs `extern void f<T>(in T dt);` now
  reduces to the `T → FRESH'`-specialized extern function type; tuple3 /
  `|+|` / p4-old `const bit<8> x = 0` / impty loop all still pass;
  used-before-bound stays 0 (p4-old; the new spec's 3 are pre-existing).
- [x] **`$align_parameters` itercollect helper captured a re-bound element —
  fixed 2026-06-13.** Its clause iterates `(let parameterTypeIR'? =
  $align_parameters'({ (id : parameterTypeIR)* }, parameterTypeIR,
  argumentIR))*`: the inner map re-iterates the WHOLE `parameterTypeIR*` while
  the outer step co-iterates the same name. Two coupled bugs in
  `to_ctrs`'s per-step element renaming: (a) `subst_term (elem_renaming …)`
  over the COMPILED body is binder-unaware, so it renamed the map's stream
  argument (`$itermap(id, parameterTypeIR)`) to the single element
  `parameterTypeIR__hd`; (b) even un-renamed, the helper never RECEIVED the
  full `parameterTypeIR*` because `captured_fvs` excludes co-iterated vars, so
  the spine-consumed stream was gone at each step. Fix
  ([to_ctrs.ml](to_ctrs.ml)): `rename_step_exp`/`rename_step_prem` do the
  per-step rename at the IL level, capture-aware — a STRUCTURED nested
  `IterE`/`IterPr` re-binding a co-iterated id is left alone (its full stream
  preserved), a BARE iterated var is renamed like a plain occurrence (matching
  old behaviour). `iter_captured` then ALSO passes any `bound_id` that survives
  the rename free (i.e. used at full-stream depth) as a captured constant
  alongside the consumed spine — so the helper gets `…($itermap(id,
  parameterTypeIR)), parameterTypeIR__hd, …` with `parameterTypeIR` the
  loop-invariant full stream. Both call site (`conds_of_prem`) and definition
  (`iterpr_defs`) share `iter_captured` so arities agree. Verified: impty
  COPS+Maude goldens byte-identical; new-spec CTRS changes are exactly the
  same capture pattern in 2 other helpers (serializable-enum `value_field`,
  `typeIR_prime_prime`) + their referencing rules; **p4-old `f(8w0)` now
  type-checks END-TO-END** (was stuck at `Call_ok`); tuple3 / `|+|` /
  serializable enum / p4-old `const` / impty loop all still pass;
  used-before-bound stays 0.
- [ ] **New-spec (`specs/p4`) `f(8w0)` still `FAIL (stuck)` — a SEPARATE,
  deeper frontier (NOT `$align_parameters`).** With both the option-equality
  and `$align_parameters` fixes, p4-old `f(8w0)` runs to a typing context but
  the NEW spec's same program sticks at the control `Decl_ok` (the apply
  body's `f(8w0)` call). The new spec's call path is structurally different
  (`$find_callableDef_overloaded_t` → `CallableType_ok` → `Call_ok`, not
  p4-old's `RoutineType_ok`), so this is its own bisection. Declaration-level
  typing, tuple3, `|+|`, and serializable enums all run on the new spec; only
  a control body containing a generic call is unverified. Bisect recipe (as
  used for `$align_parameters`): emit the module, extract the Program-ok arg,
  `red Decls-ok`/`Decl-ok`/`Block-ok`/`Stmt-ok`/`CallableType-ok` stepwise
  with python paren-balanced tuple splitting.
- [x] **`DefA` arguments** — **implemented 2026-06-13** by call-site
  specialization ([defunctionalize.ml](defunctionalize.ml), run first in
  `Pipeline.ctrs_of_spec`): each call `$f(args, def $g)` targets a generated
  first-order copy `$f_$g` with the def parameter removed and `$check := $g`
  substituted through the clauses (worklist closure over recursion and
  chained templates; templates removed; leftover `DefA` is a hard error).
  Verified end-to-end in Maude against the NEW p4 spec: tuple3.p4
  typechecks (`t = TUPLE(W(32,0), W(32,1))`, `f = t[0] = W(32,0)`) and
  `const bit<8> a = 8w250 |+| 8w10;` evaluates to `W(8, 255)`; p4-old's
  `const bit<8> x = 0;` (the coerce path) also unblocked. The same round
  made specs/p4 executable at all (see [CLAUDE.md](CLAUDE.md) and the
  to_maude commit 3d89fe82): defunctionalized signature recovery, the
  threaded-negation owise complement, subty_* owise totalization,
  first-argument-dispatch connective delegations, new-spec builtin
  delegation spellings, declaring-origin start-term encoding.
- [ ] **`injection_pairs` only scans top-level premises.** A subtype injection
  `let x = e as t` inside an `IterPr` contributes no rename pair. Unlike the
  match-bound guard (which now descends — see `Simplify.match_lets`), a naive
  descent here would be wrong: the element-level pair `(e, x)` refers to
  iteration-bound variables, so applying it at block level risks capture. A
  faithful fix lifts element pairs to list level the way `Prem_env.env_of_prem`
  does for iterated premises (`mk_iter_exp` + `relift_vars`).
- [ ] **`subst_prem`'s `IfPr` guard protection is top-level-only.** The
  var=var-equality protection matches only an `IfPr` whose expression is
  directly `CmpE (`EqOp, ..)`; an equality nested inside a conjunction guard
  would be substituted into. No current case — elaboration splits conjoined
  premises — but the condition should mirror the original structure (protect
  any guard whose equality subterm pins a pattern-bound variable) if compound
  `if` guards ever reach this pass.
- [ ] **`To_maude.module_of_system` re-runs `Simplify.simplify_spec`** to
  recompute `var_type_hints` from the same simplified spec its `sys` argument
  was built from (the comment there notes the idempotence). Harmless but double
  work on large specs; fixing it means passing the simplified spec (or the
  hints) alongside the system, an API change to the `module_of_system` surface.
- [ ] **`run --p4` parses a missing file as the empty program.** The
  front-end/preprocessor turns a nonexistent path into `variant-p4program-
  EMPTY-0`, which then "typechecks" vacuously — a silently-green run. Should
  be a clean error.
- [ ] **BuiltinDecD stragglers (new spec).** `$split_text`,
  `$strip_all_whitespace`, `$sort_` have no Maude delegations (no one-line
  built-in equivalent); `$sum_int`/`$max_int`/`$min_int` are declared but
  have no interpreter implementation either, so there is no semantics to
  mirror yet.
- [ ] **Tests.** Add a `test/` case pinning the `impty/base` CTRS output
  (`.expected`) so regressions in the translation surface in `make test`.
