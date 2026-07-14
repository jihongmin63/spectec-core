# `rewrite` — IL spec → conditional term rewriting system (CTRS)

> **의사소통은 한글을 선호합니다.** 설명·요약·질문은 한글로 (코드, 식별자, 인용한
> 에러 메시지는 원문 그대로).

> **범위: SpecTec 전체가 아니라 `rewrite` 라이브러리 하나.** 이 저장소(`spectec`)는
> SpecTec 언어 명세 컴파일러 전체(파서·엘라보레이터·인터프리터·여러 타겟 등)를
> 담은 큰 프로젝트이고, 지금 여기서 구현 중인 건 그중 `rewrite`
> 라이브러리(IL→CTRS 번역 + Maude 백엔드) 하나뿐입니다. 저장소 전체를 훑는 건 큰
> 작업이니, 이 라이브러리 밖의 코드는 필요할 때(예: IL 타입 정의, 인터프리터와의
> 대조)만 찾아보고 기본은 이 파일이 가리키는 범위 안에서 작업하세요.

This is the `spectec.rewrite` library (`spectec/lib/rewrite/dune`, branch
`new-rewrite`). It translates an elaborated SpecTec IL spec into a **conditional
term rewriting system (CTRS)**, then feeds that system to two backends: the
**Maude Formal Environment (MFE)** for confluence/coherence analysis, and a
**Maude execution module** that actually runs P4/impty programs.

**Current status:** the full pipeline is implemented and running end-to-end —
no `failwith` stubs remain anywhere in the library (verified by grep). Both
`specs/p4` and `specs/p4-old` translate and execute in Maude; the same-spec
interp-vs-Maude oracle (`check_diff_p4.sh`) shows 0 completeness gaps and 1
known soundness gap (issue1944), with 1227/1227 Phase D result-value matches.
Remaining work is analysis-surface confluence (MFE `MAYBE` verdicts on some
slices) and a struct-subtype depth corner — see [todo.md](spectec/lib/rewrite/todo.md)
for the live task list and [CORE_LOGIC.md](spectec/lib/rewrite/CORE_LOGIC.md)
for the full design rationale. This file is the **short orientation**; when it
and those two disagree, `todo.md`/`CORE_LOGIC.md` are more current (they get
updated per-change; this file is trimmed periodically).

## Pipeline

```
Lang.Il.spec ──Defunctionalize──▶ Simplify (identity) ──▶ To_ctrs.of_spec ~scalars ──▶ Gensym.thread
                                                              │
                        Structural scalars ──▶ Rewrite_system.t ──▶ Reflect (hoist_matchers/owise)
                        (self-contained Peano/                       + fold_premise_binders
                         sign-magnitude/char-list/                        │
                         own-bool)                                        ▼
                                                                    To_mfe ──▶ MFE (CRC + ChC)
                        Native scalars ──▶ Rewrite_system.t ──▶ To_maude ──▶ executable Maude module
                        (Maude's built-in
                         Bool/Nat/Int/String,
                         wrapped: nat(3), int(-5), bool(true), txt("E."))
```

Two pipeline entries in [pipeline.ml](spectec/lib/rewrite/pipeline.ml):
`ctrs_of_spec` (analysis: `~scalars:Structural`, plus three analysis-only final
passes — `Reflect.hoist_matchers`, `Rewrite_system.fold_premise_binders`,
`Reflect.owise` — that turn opaque guards into disjoint head patterns and
owise/negation guards the MFE's CRC can discharge) and
`maude_system_of_spec` (execution: `~scalars:Native`, a **direct** translation
target, not a re-fold of the structural system). Both wrap the same core
translation with `Defunctionalize` first and `Gensym.thread` last; each pass is
the identity on a spec that doesn't use its feature (so `impty` goldens are
untouched).

**`Simplify.simplify_spec` is deliberately the identity** in this project — the
original `rewrite` branch ran semantics-preserving IL→IL rewriting here (a
`Prem_env` union-find driving variable substitution, pattern folding, redundant-
premise removal); this project dropped that so `To_ctrs` is the sole
translation surface, and `Prem_env` was not reimplemented. Don't be misled by
stale doc comments elsewhere (`rewrite.mli`/`rewrite.ml`) that still describe
the old Prem_env-based behavior — [translate/simplify.ml](spectec/lib/rewrite/translate/simplify.ml)
is ground truth.

## Module map

| File | Role |
|------|------|
| [rewrite.ml](spectec/lib/rewrite/rewrite.ml) / `.mli` | Facade: re-exports submodules, `rewrite_spec = Pipeline.ctrs_of_spec`, `def_symbols`. |
| [pipeline.ml](spectec/lib/rewrite/pipeline.ml) / `.mli` | `ctrs_of_spec` (analysis) and `maude_system_of_spec` (execution); the shared `build`/`build_with` core. |
| [rewrite_system.ml](spectec/lib/rewrite/rewrite_system.ml) | Data model (`term`, `cond`, `rule` with `owise:bool`, `t = {vars; rules}`), diagnostic printer, shared lexical layer (`sanitize`, `maude_id`, `maude_var`), `slice`/`reachable_heads`/`fold_premise_binders`/`drop_owise`. No Maude module emission (that's `maude/`). |
| [translate/ctrs_term.ml](spectec/lib/rewrite/translate/ctrs_term.ml) | Symbol-naming vocabulary (`variant_sym`/`func_sym`/`rel_sym`/…), smart term/rule builders, `scalar_theory = Structural \| Native` and the mode-aware scalar leaf builders. The one place raw `R.App`/`R.Var` gets built. |
| [translate/prelude.ml](spectec/lib/rewrite/translate/prelude.ml) / `.mli` | Fixed rule set giving `Ctrs_term`'s symbols their semantics (bool/nat/int/list/option ops). `Native` drops `native_replaced_heads` (delegated by `To_maude` instead). |
| [translate/to_ctrs.ml](spectec/lib/rewrite/translate/to_ctrs.ml) / `.mli` | **Translation heart**: `of_spec`, `term_of_exp`/`pattern_of_exp`, iteration compiler (`$itermap`/`$unzip`/`$iterall`/`$itercollect`), subtype predicate, `conds_of_prem`, `rules_of_def`, `def_symbols`. Every SpecTecx relation is input-moded (`hint(input …)`), hence functional — relations translate like functions and emit as equations (`--relations-as-rules` is the only rl/crl path, a `--search` debugging override). |
| [translate/var_hints.ml](spectec/lib/rewrite/translate/var_hints.ml) / `.mli` | Per-symbol variable→IL-type map (from `VarE` notes), used only by `To_maude` to restore narrow declared types. |
| [translate/simplify.ml](spectec/lib/rewrite/translate/simplify.ml) / `.mli` | **Identity** — see above. |
| [translate/exp_map.ml](spectec/lib/rewrite/translate/exp_map.ml) / `.mli` | Shallow one-level IL traversal helpers (`map_subexps`/`subexps`/`exps_of_prem`), used by `Defunctionalize`. |
| [translate/builtin.ml](spectec/lib/rewrite/translate/builtin.ml) / `.mli` | CTRS rules for P4's collection builtins (`BuiltinDecD`s the interpreter implements natively); fed to `of_spec` as `extra_defs`. |
| [translate/gensym.ml](spectec/lib/rewrite/translate/gensym.ml) / `.mli` | Makes `$fresh_typeId`/`$fresh_tid` pure via state threading (`thread`, `effectful_syms`, `root_syms`, `seed_text`). Runs last in the pipeline; identity on gensym-free specs. |
| [translate/defunctionalize.ml](spectec/lib/rewrite/translate/defunctionalize.ml) / `.mli` | Specializes away `def`-valued arguments by call-site specialization. Runs first; identity without `DefP` (e.g. impty). |
| [translate/reflect.ml](spectec/lib/rewrite/translate/reflect.ml) / `.mli` | Analysis-only: `owise` (explicit sibling-disjointness guards + judgment reflection) and `hoist_matchers` (respell opaque `match_K` guards so `fold_premise_binders` can fold discriminators into head patterns). |
| [maude/maude_theory.ml](spectec/lib/rewrite/maude/maude_theory.ml) / `.mli` | Native scalar vocabulary: wrapper symbol spelling (`nat`/`int`/`bool`/`txt`) + literal builders, shared by `Ctrs_term`, `To_maude`, `Of_maude`. No fold pass (leaf builders emit these directly at translation time). |
| [maude/maude_sorts.ml](spectec/lib/rewrite/maude/maude_sorts.ml) | Shared order-sorted signature recovery (sorts from the original IL spec, subsort order, per-rule variable sorts, term printing) used by both `To_mfe` and `To_maude`. The `match_`/`subty_`/`holds_` predicates are declared nowhere, so `predicate_domains` recovers each domain as the **join of every subject the rules pass it** (`--wide-predicate-domains` restores the old blanket `Val`); pass `~sig_rules` with the whole system when emitting a slice, or the domains collapse to their seed. |
| [maude/to_mfe.ml](spectec/lib/rewrite/maude/to_mfe.ml) | Analysis Maude surface: emits the structural CTRS as an order-sorted Full-Maude `(mod … endm)`. Consumed by `rewrite --ctrs` and `Mfe.check`. |
| [maude/to_maude.ml](spectec/lib/rewrite/maude/to_maude.ml) / `.mli` | Execution backend: executable order-sorted Maude module (op decls, eq/rl, built-in delegations, `owise` totalization) plus the META-TERM start-term encoder (`print_meta_term`/`meta_term_of_value`) that `metaReduce` runs. |
| [maude/of_maude.ml](spectec/lib/rewrite/maude/of_maude.ml) / `.mli` | Reverse of the start-term encoder: parses a Maude normal form back into an IL `value`; `canonicalize` normalizes gensym names and sorts map entries so both sides of the result-VALUE oracle compare equal. |
| [maude/maude_run.ml](spectec/lib/rewrite/maude/maude_run.ml) / `.mli` | Runs an emitted module on a META-TERM start term via a local `maude` binary (`metaReduce`/`metaRewrite`/`metaSearch`); `run_batch` runs many starts in one Maude invocation (module internalized once). |
| [mfe.ml](spectec/lib/rewrite/mfe.ml) / `.mli` | Confluence + coherence gate: `Mfe.check` loads the MFE, runs CRC + ChC in one invocation, returns `{church_rosser; coherence}` verdicts. |

Deleted from the old `rewrite` branch and **not reimplemented** (by design, not
oversight): `prem_env.ml` (only fed the old `Simplify`), `cocoweb.ml`/
`muterm.ml`/`aprove.ml`/`termination.ml` (COPS/TPDB confluence/termination web
bridges — analysis confluence now goes through the MFE only; termination is
driven externally, see below).

## The CTRS data model

```ocaml
type term = Var of string | App of string * term list   (* nullary prints bare *)
type cond = term * term                                  (* term == term *)
type rule = { lhs : term; rhs : term; conds : cond list; owise : bool }
type t = { vars : string list; rules : rule list }
```

`slice t ~roots` restricts to rules reachable from `roots` (per-symbol
confluence checking); `def_symbols` gives slice roots in declaration order.

## Symbol-naming conventions ([translate/ctrs_term.ml](spectec/lib/rewrite/translate/ctrs_term.ml))

These **must agree** between the rule that defines a symbol and every rule that
uses it.

- `sanitize` (in `rewrite_system.ml`) scrubs a string to a CTRS-safe id
  (`->` → `minus_gt`, `&&` → `amp_amp`); Maude surfaces further mangle `_`→`-`
  (`maude_id`) and scrub variable names (`maude_var`).
- Arity is folded into variant/case symbols (`variant_<origin>_<atoms>_<n>`).
- `func_sym id` = `$` + sanitize (functions), `rel_sym id` = sanitize
  (relations). Constructors: `variant_sym`, `struct_sym`, `field_sym`,
  `match_sym`, `subty_sym`.
- Numbers: Peano `zero`/`succ` (structural nats); sign-magnitude `int_pos`/
  `int_neg` (structural ints). Lists: `nil`/`cons`. Options: `none`/`some`.
  Native scalars instead wrap Maude's built-ins: `nat(N)`/`int(N)`/`bool(B)`/
  `txt("...")`.

## IL AST cheat-sheet ([lib/lang/il/types.ml](spectec/lib/lang/il/types.ml))

- **`exp'`**: `CaseE` (variant), `StrE` (struct), `OptE`, `ListE`, `ConsE`,
  `CatE`, `DotE` (field), `IdxE`/`SliceE`/`UpdE` (path), `CallE` (function
  call), `IterE` (iteration — compiles to `$itermap` in value position or a
  `$unzip` condition in head-pattern position), `MatchE`, casts
  `UpCastE`/`DownCastE`/`SubE`.
- **`prem'`**: `RelPr` (relation call), `IfPr`, `LetPr`, `RelAssertPr`
  (`expect=true/false` for holds/does-not-hold), `ElsePr` (`otherwise`),
  `IterPr` (compiles to `$iterall`/`$itercollect`). A `relcall` is
  `{relid; notexp}`.
- **`def'`**: `TypD` (→ constructor/matcher/subtype rules), `RelD` with
  `reltyp : (typ, typ) Mode.t` (input/output slot tagging via
  `Mode.partition`), `DecD` (function clauses), `BuiltinDecD` (no rules
  emitted here — see `Builtin`).
- **`pattern`**: `CaseP`, `ListP`, `OptP`. `typcase = {notation; origin; hints}`,
  `var = {varid; typ; iters}`, `IterT = {typ; iter}`.

A clause is `{args; body; prems}`; a rule is `{ruleid; concl; prems}`. Premises
become CTRS conditions; the result/output becomes the rhs.

## CLI entry points ([bin/main.ml](spectec/bin/main.ml))

- **`rewrite [--ctrs] [--simplified] [--symbol NAME] [--relations-as-rules] FILES…`**
  — default emits the executable Maude module (`To_maude.module_of_spec`).
  `--ctrs` dumps the analysis CTRS instead (`To_mfe.module_of_system`, what
  `verify` sends the MFE); `--symbol` slices to one dependency closure.
  `--simplified` dumps the IL after `Simplify` (currently a no-op, so this is
  identical to the input).
- **`verify [--symbol NAME] [--list-symbols] [--sizes] [--timeout S] [--maude-bin P] [--mfe-dir D] FILES…`**
  — runs the MFE CRC+ChC (`Mfe.check`); exit 0 iff both verdicts are `YES`.
  Whole-system CRC explodes on critical pairs — **`--symbol` per-slice checks
  are the practical path**. `--list-symbols --sizes` ranks slices by rule count
  (the cheap tractability proxy).
- **`run [--start TERM | --imp FILE… | --p4 FILE… -i DIR] [--emit] [--search|--rewrite] [--relations-as-rules] [--bound N] [--check-p4] [--maude-bin P] [--timeout S] FILES…`**
  — emits the execution module and runs a start term through a local Maude via
  reflection (`Maude_run`). `--imp`/`--p4` build the start term from a source
  program (repeatable — batches through one Maude invocation, amortizing
  module internalization); `--emit` just prints the module. `--check-p4` also
  typechecks each `--p4` program with the interpreter and diffs the RESULT
  value against Maude's (`Of_maude`) — the Phase D oracle. `--timeout` defaults
  to 0 (no limit); see "Performance notes" for why a fixed default cannot work.

## MFE (Maude Formal Environment) — confluence + coherence gate

`Mfe.check` renders the structural system as an order-sorted Full Maude module
(`To_mfe.module_of_system`), loads it into a local Maude, and runs the
Church-Rosser Checker (CRC) + Coherence Checker (ChC). Installed under
`spectec/tools/` (gitignored — see [tools/mfe/README.md](spectec/tools/mfe/README.md)):
Maude 3.5.1 at `spectec/tools/maude/maude`, MFE at `spectec/tools/mfe/`
(entry `src/mfe.maude`).

**Protocol** (encoded in `mfe.ml`): pipe `load mfe.maude`, the module, and
`(select tool CRC .) (check Church-Rosser SPEC .)` / `(select tool ChC .)
(check coherence SPEC .)` to the MFE's stdin loop; there's no clean quit (the
loop floods `>` at EOF), so the bridge reads under a timeout and SIGKILLs once
both verdicts print.

**Whole-system CRC explodes; per-symbol slices (`verify --symbol`) are the
practical unit.** Current calibration (counts, per-symbol tables, and the two
recurring MAYBE causes — free RHS variables bound only by a premise, and owise
overlap — with their fixes `fold_premise_binders`/`Reflect.owise`) is tracked in
[todo.md](spectec/lib/rewrite/todo.md) ("Mfe calibration") rather than
duplicated here, since it shifts as reflection coverage grows.

**Termination** is not wired through this OCaml library (no `termination.ml`);
it runs externally via a Maude 2.7.1 + MTT + AProVE(Z3) stack
(`spectec/tools/mfe/run-termination.sh <symbol>`) — see
[tools/mfe/README.md](spectec/tools/mfe/README.md) for setup.

### Do not route termination through MTT — unravel and call AProVE directly

**MTT's C;A path is the wrong transformation for these slices, and it was the
cause of the long-standing termination MAYBEs — not AProVE, and not the
translation.** (2026-07-19.)

MTT unravels `l -> r if s == t` by passing the *condition's variables* to the
helper symbol:

    l            -> U(s, x1..xn)          (x1..xn = Var(l))
    U(t, x1..xn) -> r

An argument like `HU(e0,e1)` is thereby taken apart into `e0,e1` and rebuilt on
the right, which **inverts the subterm relation that carried the descent** —
`e0`,`e1` are strictly smaller than `HU(e0,e1)`, so no argument projection can
orient the resulting dependency pair, and the proof is unreachable no matter how
much budget AProVE gets.

The fix is to unravel *structure-preservingly*: carry the left-hand side's
argument list **unchanged**, wrapped in a fresh constructor `k_N` that has no
defining rule (inert, so it cannot loop — passing the bare lhs *term* instead
makes the rule reproduce its own redex, and AProVE correctly answers NO):

    f(p1..pk)            -> u_1(s, k_1(p1..pk))
    u_1(t, k_1(p1..pk))  -> r

Then hand the plain TRS straight to `tools/aprove/runme <file.trs> <budget>`
(WST mode). No MTT in the loop.

Measured effect (153 ≤500-rule symbols, both axes): the three slices MTT could
not close — `$join_text`, `$invalidate_headerUnion`, `$invalidate_value` —
each burn the full 1200 s budget to reach MAYBE through MTT, and prove **YES in
about one second** when unraveled this way. **Zero regressions** among the
symbols MTT already proved. The CTRS encoding needs no change; this is purely a
defect of the checking pipeline.

Implementing it correctly needs three details, each of which silently produces a
wrong or malformed system if missed:

1. **Multi-condition chains must accumulate bound variables.** A condition can
   bind variables a later condition or the final rhs uses
   (`... if p(x) = cons(h,t) /\ q(h) = true`). Each keep-constructor must carry
   the original arguments *plus* every variable bound by the conditions before
   it, or the last rule has an extra variable in its rhs.
2. **Emitted conditions are not always in dependency order.** Reflect's guard
   passes can place an `isStuckHead` guard ahead of the condition that binds the
   variable it inspects (`$bitstr-to-int`). Re-order greedily: repeatedly take
   the first condition whose left side is fully bound, then treat its right side
   as a binder.
3. **Verify coverage, don't assume it.** Fail loudly on any equation not
   accounted for and on any rule with an unbound rhs variable. Both bugs above
   were found that way; both had produced plausible-looking output.

Soundness: every CTRS step is simulated by the TRS
(`lσ -> u(sσ, k(argsσ)) ->* u(tσ, k(argsσ)) -> rσ`), condition evaluation
included, so **TRS termination ⟹ CTRS operational termination**. Sorts and
subsorts are dropped, making the TRS an over-approximation (well-sorted terms
are a subset) — the same safe direction MTT has. The corollary matters:
**a NO verdict does *not* prove the source non-terminating** — it may be an
artifact of the over-approximation, so treat NO as something to investigate, not
a witness.

Analysis-surface slices are safe inputs for this: they carry no `owise`, no
`rl`/`crl`, no `assoc`/`comm`/`id:` attributes, no `:=`/`=>` conditions, no
imports, and no mixfix — every declaration is prefix and single-line.
Also note `prune_slice_signature.py full` prunes only the *signature*, never a
rule, so it is a no-op for this path.

## Reading CRC / termination verdicts (MAYBE/TIMEOUT triage)

MAYBE/TIMEOUT means *unproven*, not *defective*. A real defect needs a witness:
a feasible non-joinable critical pair (CRC), or an actual infinite rewrite
(termination). A 2026-07-10 sweep over all 153 ≤500-rule symbols produced 41
MAYBE/TIMEOUT verdicts; triaging every one left **exactly one real defect**.

**Spurious CRC MAYBE/TIMEOUT.**
- Fall-through/default clause guarded by `or(all match-Xs) = false` — infeasible
  once any specific matcher fires (`$join_ctk`, `$assignop_as_binop`).
- Mutually-exclusive sign/range splits the checker can't discharge (`$bin_shr`:
  `i<0` arithmetic-shift vs `i≥0` logical; `$bin_satplus`: `sum>0` vs `sum≤0`).
- CRC TIMEOUT is usually the shared arithmetic library (`badd`/`bmul`, 13–16
  rules) exploding in critical pairs, not an own-layer overlap — that library is
  confluent (`$bin_div`/`$bin_mod` are YES).

**Spurious termination MAYBE/TIMEOUT.**
- Structural recursion whose decreasing argument is destructured in a *premise*
  (`xs = cons(h,t)`, recurse on `t`). Not a loop. (list / flatten / invalidate /
  write_value) **Cause corrected 2026-07-19:** this was blamed on AProVE's
  dependency-pair analysis, which was wrong. AProVE certifies the descent in
  about a second; MTT's unraveling was destroying it before AProVE ever saw it.
  Unravel structure-preservingly and these all come back YES — see
  "Do not route termination through MTT" above.
- Modular-(B) arith-blindness: the measure lives in the black-boxed arithmetic
  (e.g. `$shr`'s `bpred`); closed only by the (A)-lift. Real termination holds.
- Acyclic call graph + large slice → pure tool-budget TIMEOUT.

**The one real family — binenc zero-width / zero-value boundary.**
`$write_value_from_bits'` at `integerValue.V, n_var = 0`: two order-sensitive
`def` clauses share the `V` constructor, but the general clause carries no
`n_var ≠ 0` guard and no owise, so both fire at `n_var = 0` with different
results (keep the original field vs overwrite with `$int_to_bitstr(0, …)`) — a
latent non-confluence masked only by rule order. This is the root cause of all
five `write_value*` CRC MAYBEs, and the same family as the
`$bitstr_to_int` / `$int_to_bitstr` w=0 non-termination. **Lesson: when
translating order-sensitive `def` clauses that share a constructor, preserve the
disambiguating guard (or owise); always check the 0-width / 0-value boundary.**

**Surfaces differ.** CRC/termination run on the `rewrite --ctrs` *analysis*
surface (owise dropped, `isStuckHead` ruleless); confirm a real
non-confluence/non-termination on the *executable* surface (`main.exe rewrite`
without `--ctrs`, i.e. `to_maude`).

**Confirming a suspect pair fast.** Build a minimal module (full signature
preamble + only the two suspect rules + `endm)`) and run CRC — it reports just
that pair in seconds, instead of re-running the whole slice. Always
`ulimit -s unlimited` (large-slice CRC dies on stack overflow with no verdict).

## Same-spec interp-vs-Maude oracle

Since Maude runs the same `specs/p4` the interpreter does, any divergence is a
pure translation bug. **[check_diff_p4.sh](check_diff_p4.sh)** is the
self-contained, resumable driver over the full corpus (`p4_16_samples` +
`p4_16_errors`), producing:

- `check_diff_p4_completeness.tsv` — interp PASS but Maude not-OK (Maude
  under-accepts — a translation bug).
- `check_diff_p4_soundness.tsv` — interp FAIL but Maude OK (over-accepts).
- `check_diff_p4_resultmatch.tsv` — **Phase D**: for programs both engines
  accept, compares the typing RESULT value itself (`run --check-p4`, decoded
  via `Of_maude`, `canonicalize`d on both sides for gensym-name/map-ordering
  noise). `MISMATCH` here is a translation bug the verdict oracle can't catch.

**Run serially, on a clean machine** — `run --p4` parses a ~50k-line module per
invocation; concurrent Maude/dune jobs exhaust RAM and silently corrupt output.
Triage any hit immediately by bisecting the failing sub-goal with `reduce` —
see [todo.md](spectec/lib/rewrite/todo.md) for the procedure. Use the current
spec's own path (`main.exe p4 typecheck -p FILE -i INC`), never
`--spec-dir specs/p4-old`, when re-checking a file with the interpreter.

## Verified baselines — which commit measured what

The three checks (differential, CRC/ChC, termination) each cost hours, so each
is measured once at a point in history and then *carried forward* over commits
that are argued not to affect it. **No single commit has all three measured on
its own tree** — so before calling any tree "green", read what its anchor
actually proves.

| anchor | date | measured **on that exact tree** | carried over |
|---|---|---|---|
| `92618dc2` | 2026-07-10 | **differential**, full corpus, both surfaces: native completeness 0 / soundness 1 (issue1944) / Phase D 1227/1227 MATCH, and structural Phase D 1227/1227 MATCH / 0 MISMATCH ([spectec-structural-completeness-soundness.md](spectec-structural-completeness-soundness.md)) | CRC/term |
| `5647b883` | 2026-07-13 | **CRC/ChC + termination**: both columns of the 153-symbol sweep re-measured on this analysis surface (`21eac0b6`'s matcher-guard fold) — term at `3fbbe1d6`, CRC/ChC here ([recalibration.md](recalibration.md)) | differential |

**`5647b883` is the bisect anchor**: its analysis columns are current, and the
only *executable*-surface commit separating it from the differential-verified
tree is `95ddd9b3` (`[ctor]` attributes) — everything else in that window is
analysis-only (`reflect.ml`) or CLI. That one commit is therefore also the
entire bisect window for the two programs (`const.p4`, `issue1717.p4`) that a
2026-07-14 spot check found STUCK at `08dfe4ed`: they are the reason
"completeness 0" is **not** established past `92618dc2`.

Stale beyond the anchors, in commit order:

- `a290977b` (`align_guards`, 07-13) changed the analysis surface *after* the
  termination column was measured. Only the CRC column was re-measured
  (`3302d75d`/`08dfe4ed` — no column changes); **termination is carried, not
  measured**, from `08dfe4ed` on.
- `3327881f`/`381c6bd0`/`3cde77b4` (07-14, predicate-domain narrowing, `NumV`,
  ambiguous-join tie-break) change **both** surfaces, so all three checks are
  stale at HEAD. They are one family of change — re-measure once, after the
  last of them. The pending full-corpus differential re-run is tracked in
  [todo.md](spectec/lib/rewrite/todo.md).
- The `check_diff_p4_*.tsv` sitting in the repo root are **not** HEAD's numbers.
  The driver is resumable and silently skips every file already recorded, so a
  re-run over a stale TSV validates nothing — move them aside first.

## Build & run

**Build only `bin/main.exe`** — a full `dune build` drags in the P4 parser etc.
and is slow. Check for a stuck lock first:

```bash
cd spectec
lsof _build/.lock 2>/dev/null            # a held lock means a stuck dune — kill it
opam exec --switch=spectecx -- dune build bin/main.exe
```

Switch/binary name is **`spectecx`** (renamed from `spectec-core`). Build
output: `spectec/_build/default/bin/main.exe`; `make exe` (repo root)
hardlinks it to `./spectecx`. The checked-in binary can lag source — rebuild
before testing.

**Golden test** (the `--ctrs` analysis surface for `impty/base`; the default
`rewrite` emits the execution module, not this):

```bash
# from repo root
spectec/_build/default/bin/main.exe rewrite --ctrs spectec/specs/impty/base/spec.spectec \
  | diff - spectec/specs/impty/base/spec.ctrs   # must match
```

Specs live in `spectec/specs/{impty/{base,closure},p4-old,p4}`. Both `p4` and
`p4-old` fully translate and execute.

## Performance notes

A naive wall-clock comparison against the interpreter is dominated by Maude
process startup (~20-30ms) and, for large modules, start-term parsing through
the module's mixfix grammar — **not** by rewriting itself (typically 0ms for
small inputs, ~3-4M rewrites/sec at scale). Two things already fix the two
real costs:

1. **META-TERM start terms** (`To_maude.print_meta_term`/`meta_term_of_value`,
   run via `metaReduce`) replace parsing the start term through the giant
   object-level grammar with a small fixed meta-syntax — eliminates the
   per-program parse cost (was the dominant cost, ~7s/program on P4 modules).
2. **Batched invocations** (`Maude_run.run_batch`, CLI: repeat `--imp`/`--p4`)
   amortize module load + first-metaReduce internalization across every program
   in one Maude invocation.

**Current measured cost on `specs/p4`** (2026-07-11; the module is ~78k lines /
~74k equations, and it has been this size since well before the binary-nat
merge — earlier "~10s internalization, ~4ms/program" figures here were measured
before the 5,425 subty-complement equations landed and are long stale):

| phase | cost |
|---|---|
| IL → Maude translation (`run --emit`) | ~10s |
| Maude module internalization (fixed, once per invocation) | **~80s** |
| per program after that | ~6.5s |

The fixed ~80s is why **`run`/`run-structural` default to `--timeout 0` (no
limit)**: any fixed default below it turns a perfectly good run into a `TIMEOUT`
before the first program even starts (this silently broke `check_diff_p4.sh`'s
per-file fallback path). Bound the run from the caller instead — as the harness
already does with a shell `timeout`.

To break down a slow `run` invocation into startup/module-parse/rewrite phases,
use `tools/maude/rewrite-time.sh` — **not present on `new-rewrite`**, restore it
with `git checkout rewrite -- spectec/tools/maude/rewrite-time.sh` (same
restore-from-`rewrite` pattern as the deleted modules above). Details and
historical measurements: [todo.md](spectec/lib/rewrite/todo.md).

## Known gaps

Read [todo.md](spectec/lib/rewrite/todo.md) before extending — it's the live,
priority-ordered task list (currently: residual MFE `MAYBE` slices, and a
struct-subtype depth corner). [CORE_LOGIC.md](spectec/lib/rewrite/CORE_LOGIC.md)
has the full design rationale per component. Both are maintained continuously;
this file is a periodically-trimmed summary and may lag on specifics.

## Contributing conventions (see [CONTRIBUTING.md](CONTRIBUTING.md) for the full guide)

The repo-wide [CONTRIBUTING.md](CONTRIBUTING.md) is the source of truth for
commit-message format (Conventional Commits + `spec`/`reorg` types,
`Original-commit:` trailers), PR structure, rebasing, and merge-commit
conventions — read it directly for those. The rules below are the ones most
relevant to day-to-day work in this library, kept here so they're always in
view:

- **Names are part of the spec.** Prefer a name that communicates
  *responsibility*, not mechanism; check it against existing usage for the
  same concept; sweep all usage sites on rename.
- **No backward-compatibility aliases during refactors.** Finish the rename;
  don't leave transitional names.
- **Prefer self-documenting code over comments.** A comment earns its place
  only if it captures something the code can't: a non-obvious choice, an
  invariant, the spec rule being implemented.
- **`lib/` vs `bin/`.** Reusable logic goes in `lib/` (CLI infra in
  `lib/cli/`); `bin/` is only the top-level entrypoint.
- **No one-off meta-patterns** — small local duplication beats a bespoke
  helper used nowhere else. **Prefer direct code over clever abstractions**
  around exception handling; **prefer small local recursion/folds over mutable
  refs**.
- Keep refactor commits separate from fix/feature commits (bisectability).

### After finishing a code change

1. **`make fmt`** (= `dune fmt`) from the repo root, or
   `cd spectec && opam exec --switch=spectecx -- dune fmt`.
2. Apply the conventions above (and the full [CONTRIBUTING.md](CONTRIBUTING.md)
   for anything commit/PR-shaped).
3. If translation output changed, update the golden
   (`spectec/specs/impty/base/spec.ctrs`) via the diff command above, or
   `make promote` to regenerate `.expected` files.
