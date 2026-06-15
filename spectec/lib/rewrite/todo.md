# `to_ctrs` — remaining work

IL → CTRS translation ([to_ctrs.ml](to_ctrs.ml)). The `impty` specs translate
end-to-end and `specs/p4` now translates in full. This list is ordered by what
unblocks real-scale specs first.

## P0 (TOP PRIORITY) — make `specs/p4` executable in Maude

**This is the priority.** Today the interpreter runs the new spec (`specs/p4`) but
Maude runs `specs/p4-old`, so every differential is interp(p4) vs Maude(p4-old): a
divergence is CONFOUNDED between a real translation bug and a p4-old-vs-new-spec
difference, and each one has to be bisected by hand to tell them apart (that confound
is exactly what forced the whole completeness/soundness triage below — 138 + 11 cases,
all spec differences). Making `specs/p4` reducible in Maude gives the **true same-spec
interp(p4) vs Maude(p4) oracle**: then ANY divergence is a pure translation bug, no
triage, directly actionable for the rewrite library. It also retires the p4-old
baseline and the dual-spec tooling.

The module-encoding blocker is FIXED (the start-term encoder, commit 14c6949b — see
the Done entry below): a fresh build encodes every start term with **0 "no parse for
term" / 0 "bad token"** errors. So `specs/p4` now *partially executes* in Maude, but
most programs stick. Measured 2026-06-15 over the first 40 interp-PASS suite programs:
**5 OK / 35 STUCK** (all `unreduced: Program-ok`), and bisected to TWO typing/execution
frontiers (NOT just generic calls — that earlier guess was too narrow):
- **top-level instantiation** `top(c()) main` (package/constructor-call typing). An
  empty control with no instantiation reduces (`control c(){apply{b=true;}}` → OK; add
  the package *type* decl, still OK; add `top(c()) main` → STUCK). This is why ~all
  real programs (they end in `package(..) main`) stick.
- **call expressions** — both `id(true)` (generic) AND `g(true)` (plain) STUCK with no
  instantiation present, so the call/argument-typing path itself sticks, not just
  generic inference.

Both plausibly share the new spec's call/argument-typing machinery (`Call_ok`/
`$infer`/argument coercion) that constructor/package instantiation also uses; p4-old's
`RoutineType_ok` path runs end-to-end. Next: descend from `Program-ok` to the actual
stuck symbol on each (capture the shared module via `tools/maude/rewrite-time.sh`,
reduce the `Call_ok`/instantiation sub-goals directly) and find the common root. Until
it lands, Maude-side tooling stays on p4-old (see [CLAUDE.md](CLAUDE.md) "Maude 실행
기준 스펙").

**Parallel prerequisite — new-spec `BuiltinDecD` stragglers.** Beyond the two typing
frontiers, full `specs/p4` execution also needs the missing new-spec builtins (the P3
"BuiltinDecD stragglers" item): `$split_text` / `$strip_all_whitespace` / `$sort_` have
no Maude delegation (no one-line built-in equivalent), and `$sum_int` / `$max_int` /
`$min_int` are declared but have no interpreter implementation either, so there is no
semantics to mirror yet. Any program that reaches one of these sticks at the builtin
regardless of the frontier fix, so model them ([builtin.ml](builtin.ml)) as part of the
same push (for `$sum_int`/`$max_int`/`$min_int`, define the interpreter semantics first).

**Then (once the frontier is fixed): re-point the differential tooling at `specs/p4`
and re-run.** Flip the Maude spec dir from `specs/p4-old` to `specs/p4` in
`find_maude_diverging.sh` (default `SPEC_DIR`), `diff_test.sh`, and `diff_review.sh`,
update the [CLAUDE.md](CLAUDE.md) "Maude 실행 기준 스펙" note, then run
`./diff_review.sh` for the full same-spec interp(p4) vs Maude(p4) completeness +
soundness sweep. Under the same spec every remaining divergence is a PURE translation
bug (no p4-old-vs-new-spec confound, no per-file triage) — that sweep becomes the
direct work queue for the rewrite library, and the p4-old baseline + dual-spec tooling
can be retired.

## Done — `specs/p4` start term used stale front-end constructor names (2026-06-15)

`specs/p4` *translated* but its Maude module would not reduce: `run --p4 <prog>` emitted
`Warning: "spectec_maude…​.maude": no parse for term. variant-methodPrototypeList-anon-2`
(and similar) and `rewrites: 0`. **Root cause was the START-TERM ENCODER, not module
emission.** The spec-independent front-end parser tags nodes with its own (petr4-grammar)
type names — e.g. `methodPrototypeList` — but the new spec RENAMED those productions
(`externConstructorOrMethodPrototypeList`, etc.). `maude_term_of_value`'s `CaseV` origin
resolution ([to_maude.ml](to_maude.ml)) tried the value's noted name, then a unique
spec-wide case-shape match; for list-cons cases the `anon-2` shape is shared by every
`X*` type, so the match was ambiguous and it fell back to the stale note name — an
operator the module never declares → "no parse". The interpreter is unaffected because it
matches by case structure, not by Maude operator name. (p4-old worked only because its
surface names still match the front-end's.) **Fix:** prefer the EXPECTED type (the parent
field's spec-declared type, already threaded down via `case_field_typs`) when resolving
the constructor origin — it is the spec's own truth and outranks the front-end note. With
it, extern programs encode against `externConstructorOrMethodPrototypeList` and the module
parses; impty/base golden byte-identical and p4-old start terms unchanged.

NOTE this only fixed the *module/encoding* frontier. `specs/p4` still `FAIL (stuck)`s on
real programs at the SEPARATE typing/execution frontier (control-body generic call
`f(8w0)` via `CallableType_ok`→`Call_ok`/`$infer`), so Maude-side tooling stays on
`p4-old` for now (see [CLAUDE.md](CLAUDE.md) "Maude 실행 기준 스펙"). Making `specs/p4`
fully executable — and thus a true same-spec interp(p4) vs Maude(p4) oracle — is the
remaining work; that frontier, not module generation, is now the blocker.

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
above). The elaborator emits that shallow-`if` + destructuring-`let` shape for a
variant result type (`tableTypeIR`); a single-constructor type (`structTypeIR`, …)
gets the deep `if`-equality instead.

- **FIXED 2026-06-13 (commit 2b5a85f2), in `Simplify`.** `lift_match_eq_to_let`
  rewrites `if ($f(..) = C { (typ id ;)* })` — an opaque-call destructure against a
  pattern carrying a *structured* iteration — into `let C { (typ id ;)* } = $f(..)`,
  routing it through [to_ctrs]'s `LetPr` → `pattern_of_exp` → `$unzip` (binder
  position). The opaque-call anchor (mirroring `binds_by_match`) keeps it off genuine
  value-equality guards. impty/base golden byte-identical. Impact: of 60 previously
  STUCK `p4_16_samples` (p4-old) **44 now typecheck end-to-end** — including the
  bmv2/ebpf/ubpf programs that were stuck on header/metadata field access, not on
  architecture coverage.

## P1 — **FIXED 2026-06-14** (P1-(b)): a defined function in a `:=` pattern from `$find_overloaded`'s NAMED-argument clauses

- **FIXED 2026-06-14, in `Simplify`** (two commits, step1 prereq + step2 fix).
  All-named overloaded calls (`a(x = .., y = ..)`) no longer stuck/loop.
  - **Step 1 (commit 523b0152): propagate the head reconstruction into
    call-result equality guards.** Head reconstruction folds the clause head
    `(id_arg?)*` to `?(id_arg')*`, but `subst_prem` left every `if .. = ..`
    guard verbatim (to protect a var=var binder pin), so `$find_overloaded`'s
    all-named clause 0 — a bare `= eps` guard — kept feeding `$find_matchings`
    the now-unbound `id_arg?*`, inconsistent with clause 1 (which got
    `?(id_arg')*` via a `let v = $f(..)` step). The `IfPr` equality case now
    folds structure INTO an opaque-**call** operand while leaving the equality's
    own top-level operands intact, so a var=var / all-none guard is still never
    rewritten. Behaviour-preserving (impty golden byte-identical); a prerequisite.
  - **Step 2 (commit 7b16e4d3): drop the orphaned some-extraction chain.** After
    the head binds `id_arg'` directly, the extraction
    `(let id? = id_arg?)*` (A) + `(let ?(id_arg') = id?)*` (C) is dead residue
    that `To_ctrs` compiled to the circular `$itercollect … := …` helpers.
    `Simplify.drop_confined_rebinding` removes C (its pattern binds only
    head-bound vars; its RHS is a single *confined* link absent from the outputs
    and from every non-producer premise), and a new `IterPr(LetPr …)` arm in
    `prem_redundant` cascades the now-dead producer A away. The confinement gate
    keeps this off a genuine assertion `let (x, y) = $f(z)` (RHS is a call, not a
    bare link).
  - **NB the plan's predicted route (ii) — "inject the HEAD pattern's
    equivalences into the redundancy env" — was unworkable**: with chain C
    excluded (as `remove_redundant_prems` tests one premise against the others),
    nothing entails C, so the "equation already holds" path can never fire. The
    confined-rebinding pass + the `IterPr(LetPr)` dead-binding cascade replace it.
  - **Verified.** impty COPS+Maude golden byte-identical (each step); p4-old
    simplified-IL diff is exactly `$find_overloaded` clauses 0/1 (chains A/C gone,
    `$find_matchings` consistent with the `?(id_arg')*` head); clauses 2/3
    (unnamed) untouched. Minimal repro
    `control c() { action a(bit<8> x, bit<8> y) {} apply { a(x=8w1, y=8w2); } }`
    typechecks end-to-end; invalid `a(x=8w1, z=8w2)` (non-param `z`) still
    `FAIL (stuck)`; neither loops. **Sound vs the interpreter:** `named_ok` is
    `p4 typecheck` PASS (so Maude now AGREES — it was the lone STUCK outlier),
    `named_bad` is interpreter FAIL too (both reject). Regressions: positional overload
    (cases/apply-cf/def-use), impty loop, p4-old `const`, new-spec tuple3/`|+|`
    all pass; used-before-bound stays 0. Full p4-old × `p4_16_samples` survey
    (748 files) confirms **zero regressions** (no OK→non-OK); OK 204→615 / STUCK
    128 (the +411 is cumulative vs a baseline predating the member-access + IterPr
    fixes, not P1-(b) alone — P1-(b)'s own contribution is the named-argument
    overload programs, which now resolve). Remaining 128 STUCK are bucketed in the
    "remaining p4-old STUCK frontiers" section below.

---

*Historical root-cause analysis (pre-fix), kept for context:*

A `$`-function on the LHS of a Maude `:=` still survives in `$find_overloaded`'s
"all arguments named" clauses (clauses 0/1, overload resolution,
[5.01-env.spectec:184](../../specs/p4-old/5.01-env.spectec)). Two circular helpers
remain after Fix A (`$find-matchings(..) := nil` is now gone):

```
$itercollect-…-id(id-arg) := id
$itercollect-…-id-arg-prime(id) := id-arg-prime
```

**The earlier diagnosis ("an iterated *identity rename* `(id_arg? = id_arg')*`,
collapse it in Simplify") was WRONG.** Reading the *elaborated* IL (`elab` subcommand)
shows the spec premise `(id_arg? = id_arg')*` is elaborated into a **some-extraction
chain**, not an identity rename:

```
-- (let id?{id <- id?} = id_arg?)*          ;; alias: id? ≡ id_arg?
-- (if id?{id <- id?} matches (_))*          ;; each is some
-- (let ?(id_arg') = id?{id <- id?})*        ;; id_arg' = the some-content
```

i.e. it means "every argument is named (some), and `id_arg'` is the unwrapped name".

**Real root cause:** `Prem_env.hoist_pairs` (head-only reconstruction) transitively
derives `id_arg? ≡ ?(id_arg')` and folds the head pattern to `?(id_arg')*` (binding
`id_arg'` via `$unzip`), but does NOT propagate `id_arg? → ?(id_arg')` to the rest of
the clause: `$find_matchings(id_r, id_arg?*, …)` keeps the now-unbound `id_arg?*`, and
the chain premises A/B/C stay as circular `$itercollect` helpers (`id_arg` / `id`
become phantom/unbound vars). The two surviving clauses are even INCONSISTENT — the
`none` clause feeds `$find_matchings` `$itermap…(id_arg)` (phantom `id_arg`) while the
`some` clause feeds it `$itermap…(id_arg')` (the head-bound one). So this is a
**hoist/redundancy reconstruction-consistency bug for an iterated some-extraction**,
not a missing rename-collapse. A correct fix must, once the head is reconstructed to
`?(id_arg')*`, either (a) propagate `id_arg? → ?(id_arg')` consistently through the
premise bodies AND drop the now-redundant A/B/C chain, or (b) suppress the head fold
for this shape and keep the clean `id_arg?*` head with a binder-position
`$unzip`-destructure (the shape clauses 2/3, the *unnamed* case, already get cleanly).
This touches core hoist/redundancy logic the impty golden exercises, so it needs care
+ golden re-verification. Related: the iterated-option machinery the **option-equality**
item below touches (`Prem_env.subst_exp`'s `IterE` empty-binder guard).

**Scope + a minimal trigger.** Positional-argument overload programs
(`cases.p4`/`apply-cf.p4`/`def-use.p4`) hit the CLEAN unnamed clauses (2/3) and already
pass after Fix A; `bool_to_bit_cast.p4` uses no explicit named args, so its stuck is a
SEPARATE frontier. The minimal trigger for THIS bug is an all-named call:

```p4
control c() { action a(bit<8> x, bit<8> y) {} apply { a(x = 8w1, y = 8w2); } }
```

It `FAIL (stuck)`s today (and `a(x=.., z=..)` must keep failing — z is not a param).

**What was tried (2026-06-13) and why it is insufficient.** Folding the structural
reconstruction pair `id_arg? → ?(id_arg')` into the `if $find_matchings(id_arg?*, ..)`
guard's call ARGUMENTS (a refined `subst_guards_with` that protects the equality's own
top-level operands so it does NOT trivialize a guard like `(if id_arg? = ?())*`) makes
`$find_matchings` consistent with the reconstructed head and keeps the impty golden
byte-identical. But it is a PREREQUISITE, not a fix: the circular `$itercollect := …`
helpers from the redundant chain A/B/C remain, and once the input correctly routes to
clauses 0/1 (instead of wrongly matching the unnamed clauses 2/3), Maude cannot satisfy
the defined-function `:=` and the named-arg repro then **times out / loops** instead of
returning. (A *broad* guard-fold that also folds into `(if id_arg? = ?())*` "fixes" the
repro fast but is UNSOUND — it trivializes+drops the all-unnamed guard, so clauses 2/3
overlap clauses 0/1.) The real fix must ELIMINATE the bad helpers by dropping the
redundant some-extraction chain A/B/C (its result `id_arg'` is head-bound; A/B/C are
dead once `$find_matchings` is consistent). That needs `remove_redundant_prems` to (i)
handle `IterPr(LetPr …)` — today its `IterPr` case only handles `IterPr(IfPr …)` by
design (line ~575, stranding concern) — and (ii) decide the entailment with the HEAD
pattern's equivalences in scope (the redundancy env is built from `others`/premises only
and does not see that the head `?(id_arg')*` already enforces the some-ness). Both are
core changes the impty golden exercises. Verify any fix with the impty golden, the
minimal repro above (valid resolves, invalid fails, neither loops), the four P1-(c)
programs, AND re-run the survey for the net flip count.

## Done — re-extracted the interp-PASS suite (exclude-independent, 300s); it had hidden 138 divergences (2026-06-14/15)

The old `p4_typecheck_suite.txt` (1061 programs) was built from `p4 batch`'s PASS
list, and `p4 batch` pre-filters with p4c's `.exclude` files
([targets/p4/p4.ml](../../targets/p4/p4.ml) `load_excludes`). **The exclude set is
exactly where the Maude divergences live**, and the suite was also effectively cut at
~60s, dropping slow-but-valid programs. Two ways it hid divergences:
- **excludes:** of 1258 `p4_16_samples`, 210 were excluded; the **new-spec** interp
  (`p4 typecheck -p`) PASSes **138** of them, and all **138/138 diverge** in p4-old
  Maude (134 STUCK + 4 ERROR, OK 0). The 1061 in the suite were all OK, so the suite
  alone reported "0 divergences" — an artifact of the oracle, not a real result.
- **~60s timeout:** ~150 valid-but-slow programs (e.g. `psa-example-dpdk-*`) interp in
  60–300s and were spuriously dropped as FAIL.

**Fixed:** `p4_typecheck_suite.txt` re-extracted exclude-independent with a 300s
per-file interp timeout over all 1258 samples → **1186 PASS / 72 reject** (the honest
divergence oracle). `diff_review.sh` (repo root) does the full resumable
completeness + soundness cross-table over this; triage with the new-spec oracle (NOT
`--spec-dir p4-old`). The 138-divergence triage below still stands.

Bucket character of the 138 (by feature, dominated by KNOWN spec gaps below):
psa-/pna- 42, issueNNNN 34, ebpf/ubpf 18, extern/factory/ctor/virtual 6, struct/const 5,
action profile/selector 4, enum 3, forloop 7 (4 ERROR), other 19. Spot-checked
`struct.p4`: struct decls + struct const literals reduce to `result:`; adding
`const int<32> x = t.t1;` (compile-time member access) → STUCK — i.e. the **`Eval_static`
general-member-access spec gap** (pinned below), not a translation bug. `forloop*` is the
for-statement frontier (no spec coverage). The architecture-heavy psa/pna/ebpf cluster is
mostly the constructor untyped-literal `Call_convention` spec gap (pinned below). Still
worth bisecting for *masked translation bugs* (as serializable-enum was): `enumCast`,
`tuple3`, `op_bin`, `list7`, `named-arg1` are interp-PASS frontiers.

**Bisected 2026-06-14 — all candidates are SPEC GAPS, zero new translation bugs.**
The p4-old interp CLI can't be used as a discriminating oracle (it hard-errors
`builtin $to_int … not implemented` on essentially every program — the broken-oracle
issue), so each was bisected by minimizing to the failing construct + reading the
p4-old rule. Results (each: faithful translation of a p4-old rule that is genuinely
weaker than the new spec, NOT a mistranslation):
- **`named-arg1`, `list7`, `op_bin`** → the pinned **directionless-constructor untyped
  literal** gap. Discriminator: `comp(add = 32w6) c0;` Maude-OK vs `comp(add = 6) c0;`
  Maude-STUCK (apply-method named args alone, `c0.apply(b = b, x = xv)`, are fine).
- **`struct.p4`/`constStruct`** → the pinned **`Eval_static` general-member-access**
  gap (p4-old `Eval_static` has member rules only for enum/serenum/stack-`.size`, none
  for struct `t.t1`). Discriminator: struct decls+literals OK; `const int<32> x = t.t1;`
  STUCK.
- **`enumCast`** → NEW pinned gap: **serializable-enum operand + untyped-int literal in
  arithmetic** (`E1.e1 + 1`). `$reduce_serenum_binary` DOES unwrap serenum operands, and
  `E1.e1 + 8w1`/`8w1 + E1.e1`/`E1.e1 + E1.e2` all reduce; but `E1.e1 + 1` STUCKs because
  in `Expr_ok/binaryExpression-plusminusmult` ([5.06.2:149]) `$coerce_binary` runs
  BEFORE `$reduce_serenum_binary`, and `$coerce_binary(E1, INT)` returns `eps` (clauses
  at [5.05.2:426]: neither `Cast_impl: E1 -> INT` nor `INT -> E1` holds — no implicit
  serenum↔arbint cast), so the rule's `if (l_cast, r_cast) = $coerce_binary(..)` match
  fails. Confirmed: `(bit<8>)E1.e1 + 1` (explicit unwrap before `+`) reduces. **Spec
  fix:** unwrap serenum to its underlying type before/inside `$coerce_binary` (or have
  `$coerce_binary` look through serenums), so the untyped int coerces to the underlying
  width. NOT a rewrite-library change.

Net: the 138 missed divergences are dominated by these three p4-old spec gaps + the
for-statement frontier; no translation bug found in the rewrite library. The only
historically-real translation bugs in this area (serializable-enum value-position
`$itermap` capture; `$itermap`-in-`:=`-pattern; table action-enum stream) are already
fixed.

## Soundness round — interp-REJECT × Maude (2026-06-15)

The completeness direction (interp-PASS × Maude-STUCK) only finds *missing* coverage.
The dual SOUNDNESS direction asks the more dangerous question: does Maude **accept** a
program the interpreter **rejects**? Ran it: of `p4_16_errors` (310), the new-spec
interp REJECTS 269 (true negatives; 41 it accepts — a new-spec permissiveness issue,
not ours). Running those 269 through p4-old Maude: **228 STUCK (Maude also rejects, by
sticking — correct), 30 ERROR (front-end/parse), 11 OK** = 11 candidate soundness bugs
(translation accepted an invalid program). The 11:
`init-entries-error3-bmv2`, `issue1944`, `issue2206`, `issue2260-1`, `issue2835-bmv2`,
`issue3057-1`, `issue3273`, `issue3299`, `issue818`, `shift-int-non-const`,
`table-entries-lpm-2`.

**Triaged ALL 11 individually (2026-06-15) — every one is p4-old being MORE PERMISSIVE
than the new spec (a spec divergence, faithful translation), ZERO translation soundness
bugs.** (Each first re-confirmed individually Maude-OK, not a batch artifact.) They are
*semantic validity checks the new spec/p4c added but p4-old's `*_ok` relations never had*:
- **arbitrary-int shift by a non-const amount** — `shift-int-non-const` `(bit<8>)(a>>b)`,
  `issue2206` `1 << h.h.c`. p4-old `Expr_ok/binaryExpression-shift-fixbit` ([5.06.2:295])
  accepts any fixed-bit right operand with NO compile-time-known guard on an arbint left,
  so `int >> bit<n>` type-checks (rule read).
- **max bit width** — `issue1944` `bit<2147483648>`. p4-old never bounds the width (no
  rule found).
- **tuple/record literal `==`** — `issue3057-1` `{1,2}=={1,2}`. p4-old `binaryExpression-eq`
  ([5.06.2:347]) gates only on `$is_equalable_typeIR`, which accepts the sequence type.
- **generic inference to arbint then narrowing** — `issue2260-1` `bit<8> y = f(255)` with
  `T f<T>(T x)`. p4-old `Call_ok` accepts `T = int` and the int→bit<8> narrowing.
- **abstract method returning infinite-precision int** — `issue3273` `abstract int f();`.
  p4-old `ExternMethod_ok/abstract` ([5.12:43]) checks the return type only with `Type_ok`
  (structural well-formedness); `int` passes, so it never rejects (full rule read).
- **non-exhaustive value `switch`** — `issue3299` `switch (m.m0) { 8w0x0: {} }`. p4-old's
  switch-general rule has no exhaustiveness check (else it would STUCK, not OK).
- **lpm const-entry mask must be a prefix** — `table-entries-lpm-2` `0x11 &&& 0x1F0`.
  p4-old has `TableEntry_keyset_ok` but no prefix/contiguity validity rule.
- **entry-priority semantics** — `init-entries-error3-bmv2` (`largest_priority_wins`,
  `priority_delta`, explicit `priority=`). p4-old has no priority-validity check.
- **extern constructor with a control-typed parameter** — `issue818` `WrapControl(C c)`.
  p4-old `ExternConstructor_ok` ([5.12:100]) `ConstructorParameters_ok` is looser.
- **partial table-action application** — `issue2835-bmv2` (`Call_action_partial_ok` in the
  new spec). p4-old's action-call typing is looser.

**Cross-cutting check that rules out the most plausible translation bug:** the duplicate-
name / distinctness builtin `$distinct_<id>(id*)` (used as a positive premise across
[5.03-wellformed.spectec], e.g. lines 73/220/257/…) is **correctly translated** in
[builtin.ml](builtin.ml): `distinct_(nil) = true`, `distinct_(cons(x,xs)) =
and(not(mem(x,xs)), distinct_(xs))` — returns `false` on a duplicate, so a duplicate-name
program is faithfully rejected (it would STUCK, not OK). So the negated-check soundness
hazard is NOT realized for distinctness. None of the 11 reject-paths depend on a totalized
`does not hold` / `owise` complement firing wrongly — they all reduce because p4-old's
loose positive rule genuinely matches.

Net across BOTH directions (completeness 138 + soundness 11): every bisected divergence is
a p4-old-spec-vs-new-spec difference, translated faithfully. **No translation bug found.**

## p4-old STUCK tail is SPEC INCOMPLETENESS, not translation bugs (2026-06-14)

> **PARTLY RETRACTED (2026-06-14, later same day).** The "zero translation bugs
> left" conclusion below is UNSAFE: it relied on
> `p4 typecheck --spec-dir specs/p4-old` as the interp oracle, and **that oracle
> is misconfigured** — `p4 batch`/`typecheck`'s CLI keeps the *new* target's
> `Builtins`/handler and only swaps the spec *files*, so the old spec runs with
> the wrong builtins and reports spurious interp-FAILs. Demonstrated: `issue3623-1`
> and `bool_to_bit_cast` were in the "128 interp-FAIL spec holes" list, yet the
> correctly-configured **new-spec** interpreter ACCEPTS them, and a
> translation-only fix (the value-position `$itermap` capture bug — see the
> serializable-enum item below) flipped both **Maude-STUCK → OK with the impty
> golden byte-identical.** So they were GENUINE translation bugs the bad oracle
> masked. An unknown number of the 128 (those touching serializable enums, and
> possibly others) may be the same. **Re-triage the 128 with the new-spec
> `p4 typecheck` (default) oracle, not `--spec-dir specs/p4-old`** — DONE: see the
> "138 missed" completeness triage and the soundness round above; with the correct
> oracle every bisected divergence is a p4-old-vs-new-spec spec difference, no
> translation bug (the historically-real ones are already fixed).

Full re-survey of p4-old × `p4_16_samples` (748): **OK 615 / STUCK 128 / OTHER 4 /
ERROR 1**, zero regressions. The original claim (now qualified by the retraction
above): every one of the 128 STUCK programs ALSO fails in the OCaml interpreter
running the same spec (`p4 typecheck --spec-dir specs/p4-old`) — **but that oracle
is unreliable**, so "0 interpreter-PASS, 128 interpreter-FAIL" cannot be trusted.
Some of the STUCK tail is genuine `specs/p4-old` incompleteness (the pinned
constructor-arg gap below is real and interp-FAILs on the new spec too); some is
masked translation bugs. Triage each with the new-spec oracle before assigning.

**Methodology (use this to triage any future STUCK).** The clean discriminator —
same-spec interp(p4-old) vs Maude(p4-old) — is UNAVAILABLE: `p4 typecheck --spec-dir
specs/p4-old` is the broken oracle (wrong builtins → `$to_int not implemented`), and
there is no other p4-old interpreter. So a divergence (new-spec interp PASS + Maude
p4-old STUCK/OK-on-reject) is confounded between a translation bug and a p4-old-vs-
new-spec difference, and you must **bisect to the stuck symbol and read the p4-old
rule**: minimize the program to the failing construct, then check whether the p4-old
spec even HAS the relevant rule. If p4-old's rule is missing/weaker (a loose positive
rule, or no rejecting premise), the faithful translation correctly diverges = a SPEC
difference, not a rewrite-library bug. A *translation bug* is only when p4-old's rule
WOULD accept/reject but Maude does the opposite (e.g. a mistranslated premise, or the
`owise`/negation totalization firing wrongly — `$distinct_` was checked and is
correct). This is how the 138 completeness gaps and 11 soundness candidates were all
attributed to spec differences. (P1-(b) `named_ok` `a(x=8w1,y=8w2)` was validated as a
real fix the same way: it is interp-PASS and was the lone Maude-STUCK outlier → now OK.)

The deep bisection below (kept for the spec authors) pins one representative spec
gap precisely; the others are analogous spec-coverage holes, **not** rewrite-library
work. Stuck terms surface as the bare `Program-ok(..)` (stuck propagation); descend
by reducing sub-goals on the *shared* module (program-independent): capture the
Maude input via a `--maude-bin` wrapper, then `reduce` the relation calls directly,
feeding each level's outputs into the next.

### Pinned spec gap: directionless (constructor) argument needs an explicit type

**Minimal repro / discrimination.** `extern E { E(bit<32> size); } control c() {
E(12) e; }` STUCKs (interp + Maude); `E()` (no-arg) and `E(32w12)` (explicitly
typed arg) both PASS. So it is not "any instantiation" — it is **an untyped int
literal as a directionless constructor argument**. Bisected term-by-term down the
chain `Decl_ok/instantiation` → `ConstructorType_ok` (RETURNS the constructor type
fine) → **`Inst_ok` (sticks)** → its premises: `$filter_default_parameters`,
`$align_parameters` both reduce fine → **`Call_convention_ok` (sticks)** →
`Call_convention_expr_ok/empty-not-action`
([5.15.1:116](../../specs/p4-old/5.15.1-typing-call-convention.spectec)). That rule,
for a directionless (`_EMPTY`) param under `NOACTION`, **does NOT coerce** ("may not
insert implicit casts") and requires `Type_alpha: typeIR_param ~~ typeIR_arg`
exactly. Direct reductions: `Call_convention_expr_ok(param bit<32>, arg 12:INT)`
sticks; with `arg :bit<32>` it returns; `Type_alpha(bit<32>, INT) = false`,
`Type_alpha(bit<32>, bit<32>) = true`. So the literal `12` (typed `INT`, never
narrowed to the param's `bit<32>`) fails the exact match and no rule fires.

**Spec fix (upstream, `specs/p4-old`):** make the directionless-arg convention
coerce a compile-time-known argument to the parameter type (mirror the `in`-rule's
`$coerce_unary`, line 56, and the `empty-action` rule's coercion, line 110), OR
type the constructor argument against the expected parameter type so `12` is
`bit<32>` before the convention check. This is the largest bucket: extern/control/
package/factory/generic instantiation with positional untyped literals
(`constructor_cast`, `extern2`, `factory1/2`, `default-*-argument`, `functors*`,
`generic*`, `action_profile*`/`action_selector*`, …). NOT a rewrite-library change.

### Serializable-enum / accessor static-eval — MIXED (re-triaged 2026-06-14)

The earlier lump ("all interp-FAIL spec holes") was wrong (bad oracle). Split:

- **Serializable enums — TRANSLATION BUG, FIXED 2026-06-14** (`issue3623-1`,
  `bool_to_bit_cast`, any serializable-enum decl/const). Root cause was NOT the
  `Eval_static` serenum member rule (277-283) and NOT a spec hole — it was a
  **value-position `$itermap` capture bug** in the serializable-enum *declaration*
  typing (`Enum_serializable_fields_ok` → the `$itermap-enum-…` helper → `$add_vars`).
  The per-member varType embeds the WHOLE `(nameIR_field = value_field ;)*` list
  (re-iterating `value_field*`), but `iter_map_def`'s binder-unaware
  `subst_term (elem_renaming …)` rewrote `value_field` → `value_field__hd` *inside*
  that nested re-zip, so the inner `$itermap` got a bare element instead of the
  stream and never matched → `$add_vars` stuck → the whole enum decl stuck (before
  any const ever reached `Eval_static`). Fix = the expression analog of the
  `$align_parameters` premise-side fix: new `iter_captured_exp` + capture-aware
  `rename_step_exp` in `iter_map_def` and its `term_of_exp` call site
  ([to_ctrs.ml](to_ctrs.ml)). Verified: impty COPS golden byte-identical; minimal
  serenum `const`, `issue3623-1`, `bool_to_bit_cast` all flip Maude-STUCK → OK; a
  15-file serial sample of the interp-validated suite is 15/15 OK.
- **Genuine spec gaps (interp-FAIL on the NEW spec too):** `constant_folding`,
  tuple-index `issue3283`. [5.06.1:286] says *"general member accesses are not
  evaluated statically"*, [5.06.1:297] *"tuple … accesses are not evaluated
  statically"* — `Eval_static` has rules only for enum/serenum/stack-`.size`. Add
  the missing rules upstream in `specs/p4-old`. NOT a rewrite-library change.
- **interp-PASS but still Maude-STUCK (separate frontiers, NOT spec holes):**
  `enumCast` (large program: enum `==`, parser, `packet.extract`, `select`),
  `constStruct`. The new-spec interpreter accepts these; their Maude stick is a
  distinct translation frontier to bisect, not covered by the serenum fix.
- **`for` statement** (`forloop*` → ERROR/OTHER/STUCK): a newer P4 feature with no
  front-end/spec coverage. Spec/front-end work, not CTRS.

### Caveat on the survey baseline

The `+411 STUCK→OK` vs `p4old_samples_results.tsv` is *cumulative* (that baseline
predates the member-access + IterPr fixes), not P1-(b) alone. P1-(b)'s own
contribution is the named-argument overload programs. The "zero regressions" and
"128/128 STUCK are interp-FAIL" are the load-bearing facts.

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
  mirror yet. **Prerequisite for P0** (making `specs/p4` executable) — tracked
  there as a parallel prerequisite.
- [ ] **Tests.** Add a `test/` case pinning the `impty/base` CTRS output
  (`.expected`) so regressions in the translation surface in `make test`.

## P1 — **FIXED 2026-06-13 (Fix A)**: table action-enum *value* built from the id STREAM, not the per-element id (action-name switch labels stick)

Found by isolating the post-`lift_match_eq_to_let` still-STUCK in-scope set (interpreter
`Typechecker success`, Maude stuck). **Shared by ≥4 programs**: `cases.p4`,
`apply-cf.p4`, `default-switch.p4`, `exit5.p4` — every control whose apply body has a
`switch (t.apply().action_run) { <action-name>: … }` with an **action-name label**
(an empty switch / `default:`-only switch is fine).

- **FIXED 2026-06-13 in `Simplify` (iteration-binder-scope discipline).**
  [simplify.ml](simplify.ml)'s `subst_prem` `IterPr` branch recursed straight through
  the `iterexp` wrapper, dropping the binder scope — so an env pair
  `value_enum_field → table_enum…id_enum_field` (an element binding from a SIBLING
  iteration) got applied inside the `varTypeIR_enum_field` iteration, lifting
  `id_enum_field` from element depth to its stream `id_enum_field*` (the ill-sorted
  List-in-Text). Fix: thread `elem_bound` (every element variable bound by some
  iteration in the block, via the new `iter_binders_prem`/`iter_binders_exp`) into
  `subst_prem` and, at each `IterPr`, withhold any pair whose `to_e` drags in an
  element variable bound elsewhere but not by THIS iteration. This is the
  premise-position analog of `Prem_env.subst_exp`'s `binds_from`/relift guard (which
  only fires on `IterE` *expression* nodes; the `IterPr` binders sit on the wrapper).
  Verified: impty COPS+Maude goldens byte-identical; p4-old simplified-IL diff is
  exactly `$find_overloaded` + `TableType_ok` (8 lines); **all four targets flip
  STUCK→OK end-to-end** (cases/apply-cf/default-switch/exit5); `def-use`, p4 `tuple3`
  still pass. Side effect: it also removed one of `$find_overloaded`'s defined-function
  `:=` helpers (`$find-matchings(..) := nil`) — see the revised P1-(b) below.

Minimal repro (bare control, p4-old accepts it):
```p4
control ctrl() {
  action a() {} table t { actions = { a; } default_action = a; }
  apply { switch (t.apply().action_run) { a: { } } }   // a: sticks; remove it -> OK
}
```

Bisected (M0–M5 isolation + stepwise reduce): the label rule
`SwitchLabel_table_ok/expressionNonBrace-prefixedNonTypeName` looks the action-enum
member up with `$find_var(LOCAL, TC, "action_list(t).a")` and checks
`value_label = TABLE_ENUM "action_list(t)" '.' nameIR_label`. Every component works in
isolation ($canon, $strip_prefix/suffix→"t", registration, $find_var, the some/option
wrapping). The break is the **registered enum VALUE's shape**: it is
`tableValue-TABLE-ENUM-dot-2("action_list(t)", cons(txt("a"), nil))` — a **List**
`["a"]` in the constructor's **second `Text` slot** (`op … : Text Text -> TableValue`),
i.e. an ill-sorted term that can never match the well-sorted lookup value
`tableValue(…, txt("a"))`. So the equality fails and the label rule never fires.

Root (simplified IL of `TableType_ok`, 5.14.1):
```
-- (let value_enum_field    = table_enum "action_list("++nameIR++")" . id_enum_field)
                                                    *{id_enum_field<-id_enum_field*, value_enum_field<-value_enum_field*}
-- (let varTypeIR_enum_field = table_enum … { id_enum_field*{…} } lctk
                               ?(table_enum "action_list("++nameIR++")" . id_enum_field))   ← value_enum_field INLINED
                                                    *{value_enum_field<-value_enum_field*, varTypeIR_enum_field<-varTypeIR_enum_field*}
```
The spec is `varTypeIR_enum_field = _EMPTY typeIR LCTK value_enum_field`, i.e. the value
field should be the **co-iterated** `?(value_enum_field)`. A `Simplify` inline replaced
`value_enum_field` with its definition `table_enum … . id_enum_field` **inside the
`varTypeIR_enum_field` iteration**, where `id_enum_field` is NOT a binder — so it denotes
the whole `id_enum_field*` stream, giving `tableValue(tid, [ids…])` instead of
`tableValue(tid, id)`. (`$flatten_prefixedNameIR` itself is correct: `-> Text`,
`(_NAME a) = "a"`.) `value_enum_field` even stays listed as an unused binder, confirming
it was substituted away rather than properly kept.

Fix lives in the inline/substitution machinery ([simplify.ml](simplify.ml)
`inline_lets`/`inline_value_lets`, or [prem_env.ml](prem_env.ml) `subst_exp`'s IterE
binder-relift — the `binds_from` guard only fires for a bare `VarE from_e`, not the
iterated `value_enum_field*{…}` form): never inline/substitute a per-element binding
into another iteration that does not co-iterate the element variables its RHS drags in
(here `id_enum_field`). Same delicate family as the option-equality item below; the
impty golden does NOT cover it, so a fix must be re-surveyed over `p4_16_samples`
(cases/apply-cf/default-switch/exit5 must flip to OK, and previously-OK must stay OK).
