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

`Rewrite.compile ~target:(Analysis | Execution) spec_il` is the single entry.
It returns `{ spec; system }`: the specialized spec and its rewriting system.
Always give emitters, start-term encoders, and decoders that returned `spec`,
because specialization renames definitions.

```text
IL → Defunctionalize → Monomorphize → To_ctrs.of_spec (+Builtin)
   → Gensym.thread → orient_conds
   → Analysis only: hoist_matchers → expand_subty_guards → fold_premise_binders
                    → order_conds → align_guards → owise → order_conds
   → { spec; system }
```

`Analysis` uses structural scalars for the checkers; `Execution` directly
uses native Maude scalar wrappers. Their theories and analysis-only stages
are intentional. `Simplify` was an identity and has been removed, along with
`To_ctrs.of_spec`'s redundant `orig` argument. `Prem_env` remains absent.

CLI implementations live in `spectec/lib/cli/rewrite/` (`spectec.rewrite_cli`).
`run --structural` selects direct execution of the analysis module; both run
modes share one compilation, input resolution, batching, and result comparison.
See [refactoring.md](spectec/lib/rewrite/refactoring.md) for API/CLI migration
and verification details.

## Module map

| File | Role |
|------|------|
| [rewrite.ml](spectec/lib/rewrite/rewrite.ml) / `.mli` | Facade: shared `compile ~target` returning `{ spec; system }`, submodules, and `def_symbols`. |
| [pipeline.ml](spectec/lib/rewrite/pipeline.ml) / `.mli` | Shared `compile ~target`, specialization, and analysis-only finishing passes. |
| [rewrite_system.ml](spectec/lib/rewrite/rewrite_system.ml) / `.mli` | Data model (`term`, `cond`, `rule` with `owise:bool`, `t = {vars; rules}`), diagnostic printer, the CTRS-safe id scrub (`sanitize`), shared term vocabulary (`vars_of_term`/`subst`/`of_rules`), the condition orientation invariant (`orient_conds`), `slice`/`reachable_heads`. No Maude spelling (that's `maude/maude_ident.ml`), no checker passes (those are `crc_surface.ml`/`scc_surface.ml`), no Maude module emission (`maude/`). |
| [crc_surface.ml](spectec/lib/rewrite/crc_surface.ml) / `.mli` | Analysis (CRC) normalization passes lifted out of the data model: `fold_premise_binders`, `order_conds`, `crc_unravel`, `crc_normalize`. |
| [scc_surface.ml](spectec/lib/rewrite/scc_surface.ml) / `.mli` | SCC over-approximation passes: `drop_conds`, `linearize_lhs`. |
| [translate/ctrs_term.ml](spectec/lib/rewrite/translate/ctrs_term.ml) | Symbol-naming vocabulary (`variant_sym`/`func_sym`/`rel_sym`/…) including the derived-predicate spellings (`match_`/`subty_`/`holds_` prefixes, `eqg`), smart term/rule builders, `scalar_theory = Structural \| Native` and the mode-aware scalar leaf builders. The one place raw `R.App`/`R.Var` gets built. |
| [translate/prelude.ml](spectec/lib/rewrite/translate/prelude.ml) / `.mli` | Fixed rule set giving `Ctrs_term`'s symbols their semantics (bool/nat/int/list/option ops). `Native` drops `native_replaced_heads` (delegated by `To_maude` instead). |
| [translate/to_ctrs.ml](spectec/lib/rewrite/translate/to_ctrs.ml) / `.mli` | **Translation heart**: `of_spec`, `term_of_exp`/`pattern_of_exp`, iteration compiler (`$itermap`/`$unzip`/`$iterall`/`$itercollect`), subtype predicate, `conds_of_prem`, `rules_of_def`, `def_symbols`. Every SpecTecx relation is input-moded (`hint(input …)`), hence functional — relations translate like functions and emit as equations (`--relations-as-rules` is the only rl/crl path, a `--search` debugging override). The invariant is enforced at elaboration (`fetch_rel_input_dirs`: a hint-less relation defaults to all-input with a warning), so the translator needs no guard of its own. |
| [translate/var_hints.ml](spectec/lib/rewrite/translate/var_hints.ml) / `.mli` | Per-symbol variable→IL-type map (from `VarE` notes), used only by `To_maude` to restore narrow declared types. |
| [translate/spec_index.ml](spectec/lib/rewrite/translate/spec_index.ml) / `.mli` | One constructor/field/relation-signature index built per spec (physical-equality memo), so `Reflect`, `Of_maude`, `Maude_sorts` and `To_maude` derive their views from it instead of each re-walking the whole spec. |
| [translate/exp_map.ml](spectec/lib/rewrite/translate/exp_map.ml) / `.mli` | Shallow one-level IL traversal helpers (`map_subexps`/`subexps`/`exps_of_prem`), used by `Defunctionalize`. |
| [translate/builtin.ml](spectec/lib/rewrite/translate/builtin.ml) / `.mli` | CTRS rules for P4's collection builtins (`BuiltinDecD`s the interpreter implements natively); fed to `of_spec` as `extra_defs`. |
| [translate/gensym.ml](spectec/lib/rewrite/translate/gensym.ml) / `.mli` | Makes `$fresh_typeId`/`$fresh_tid` pure via state threading (`thread`, `effectful_syms`, `root_syms`, `seed_text`). Runs last in the pipeline; identity on gensym-free specs. |
| [translate/defunctionalize.ml](spectec/lib/rewrite/translate/defunctionalize.ml) / `.mli` | Specializes away `def`-valued arguments by call-site specialization. Runs first; identity without `DefP` (e.g. impty). |
| [translate/monomorphize.ml](spectec/lib/rewrite/translate/monomorphize.ml) / `.mli` | Specializes away polymorphic `dec`s at the type arguments their call sites pass, so no type PARAMETER survives into the translation (a parameter has no `TypD`, so its equality could only fall back on the generic `eq`). Also owns type substitution (`subst_typ`/`subst_deftyp`), which `To_ctrs` uses to instantiate a polymorphic TYPE's `eq_<T>`. Runs after `Defunctionalize`; identity without a polymorphic `dec`. |
| [translate/reflect.ml](spectec/lib/rewrite/translate/reflect.ml) / `.mli` | Analysis-only: `owise` (explicit sibling-disjointness guards + judgment reflection) and `hoist_matchers` (respell opaque `match_K` guards so `fold_premise_binders` can fold discriminators into head patterns). |
| [maude/maude_ident.ml](spectec/lib/rewrite/maude/maude_ident.ml) / `.mli` | The Maude lexical layer shared by both Maude surfaces: `id` (operator id — `_`→`-`) and `var` (variable name), so operator/variable spellings agree. Was `maude_id`/`maude_var` in `rewrite_system.ml`. |
| [maude/maude_theory.ml](spectec/lib/rewrite/maude/maude_theory.ml) / `.mli` | Native scalar vocabulary: wrapper symbol spelling (`nat`/`int`/`bool`/`txt`) + literal builders, shared by `Ctrs_term`, `To_maude`, `Of_maude`. No fold pass (leaf builders emit these directly at translation time). |
| [maude/maude_sorts.ml](spectec/lib/rewrite/maude/maude_sorts.ml) / `.mli` | Shared order-sorted signature recovery (sorts from the original IL spec, subsort order, per-rule variable sorts, term printing) used by both `To_mfe` and `To_maude`. The `match_`/`subty_`/`holds_` predicates are declared nowhere, so `predicate_domains` recovers each domain as the **join of every subject the rules pass it** (`--wide-predicate-domains` restores the old blanket `Val`); pass `~sig_rules` with the whole system when emitting a slice, or the domains collapse to their seed. |
| [maude/to_mfe.ml](spectec/lib/rewrite/maude/to_mfe.ml) | Analysis Maude surface: emits the structural CTRS as an order-sorted Full-Maude `(mod … endm)`. Consumed by `rewrite --ctrs` and `Mfe.check`. |
| [maude/to_maude.ml](spectec/lib/rewrite/maude/to_maude.ml) / `.mli` | Execution backend: executable order-sorted Maude module (op decls, eq/rl, built-in delegations, `owise` totalization) plus the META-TERM start-term encoder (`print_meta_term`/`meta_term_of_value`) that `metaReduce` runs. |
| [maude/of_maude.ml](spectec/lib/rewrite/maude/of_maude.ml) / `.mli` | Reverse of the start-term encoder: parses a Maude normal form back into an IL `value`; `canonicalize` normalizes gensym names and sorts map entries so both sides of the result-VALUE oracle compare equal. |
| [maude/maude_run.ml](spectec/lib/rewrite/maude/maude_run.ml) / `.mli` | Runs an emitted module on a META-TERM start term via a local `maude` binary (`metaReduce`/`metaRewrite`/`metaSearch`); `run_batch` runs many starts in one Maude invocation (module internalized once). |
| [mfe.ml](spectec/lib/rewrite/mfe.ml) / `.mli` | Confluence + coherence gate: `Mfe.check` loads the MFE, runs CRC + ChC in one invocation, returns `{church_rosser; coherence}` verdicts; `check_normalize_upgrade` adds the upgrade-only `--crc-normalize` retry. |
| [subproc.ml](spectec/lib/rewrite/subproc.ml) / `.mli` | Deadline subprocess runner the tool bridges share: temp-file stdin, `ulimit -s unlimited`, kill on done-predicate or deadline, partial output kept. |
| [unravel.ml](spectec/lib/rewrite/unravel.ml) / `.mli` | Unraveling of a slice into a plain TPDB TRS — the MTT replacement (see the termination section). |
| [itrs.ml](spectec/lib/rewrite/itrs.ml) / `.mli` | The integer-constraint export of the same unraveling: the structural arithmetic layer replaced by AProVE's native integers and `:\|:` constraints, and (with `~norm`) the list layer lifted to lengths on top. A MODEL of the slice, not an over-approximation — a YES is about the model. |
| [qdp.ml](spectec/lib/rewrite/qdp.ml) / `.mli` | One child of AProVE's dependency-graph decomposition, read out of a run's obligation echo and re-posed as a standalone `.qdp` problem (usable-rules pruned + Cε). |
| [aprove.ml](spectec/lib/rewrite/aprove.ml) / `.mli` | Local AProVE bridge (`tools/aprove/runme`, WST mode): YES/NO/MAYBE/TIMEOUT verdicts. |
| [termination.ml](spectec/lib/rewrite/termination.ml) / `.mli` | Per-slice termination: slice → `Unravel` → `Aprove` (the `termination` subcommand). |
| [scc.ml](spectec/lib/rewrite/scc.ml) / `.mli` | Sufficient completeness via CETA Maude 2.7 + old MFE 2.7.1: unconditional over-approximation, pruned functional module, verdict/fidelity/witness-domain classification (the `scc` subcommand). |

Deleted from the old `rewrite` branch and **not reimplemented** (by design, not
oversight): `prem_env.ml` (only fed the old `Simplify`) and `cocoweb.ml`/
`muterm.ml` (COPS/TPDB confluence/termination *web* bridges — analysis
confluence now goes through the MFE only). Today's `aprove.ml`/`termination.ml`
share those old names but are new *local*-process bridges (no web service, no
Python), built on `subproc.ml`.

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
  (`->` → `minus_gt`, `&&` → `amp_amp`); the Maude surfaces further mangle `_`→`-`
  (`Maude_ident.id`) and scrub variable names (`Maude_ident.var`) in
  `maude/maude_ident.ml`.
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

- **`rewrite [--ctrs] [--symbol NAME] [--slice-dir DIR] [--unconditional] [--crc-normalize] [--prune-signature] [--list-symbols] [--sizes] [--relations-as-rules] FILES…`**
  — default emits the executable Maude module (`To_maude.module_of_system`).
  `--ctrs` dumps the analysis CTRS instead (`To_mfe.module_of_system`, what
  `confluence` sends the MFE); `--symbol` slices to one dependency closure and
  `--slice-dir` dumps every slice in one translation. `--unconditional` (SCC)
  and `--crc-normalize` (CRC) are the verdict-shaping transforms documented
  below; `--prune-signature` keeps only the signature the rules use (rules
  untouched, so verdicts are preserved). `--list-symbols` lists the sliceable
  symbols (the names the `--symbol`/`--all` checkers take) and `--sizes` ranks
  them by slice rule count — the cheap tractability proxy. (To inspect the
  elaborated IL itself, use the `elab` command — there is no IL simplification pass, so
  there is no separate "simplified" dump.)
- **`confluence [--symbol NAME]… | --all [--crc-normalize] [--timeout S] [--maude-bin P] [--mfe-dir D] [--out TSV] FILES…`**
  — per-slice MFE CRC+ChC (`Mfe.check`), one TSV row per symbol
  `<sym>\t<church-rosser>\t<coherence>`; exit non-zero if any row is not
  `YES`/`YES`. Whole-system CRC explodes on critical pairs, so (like
  `termination`/`scc`) there is no whole-system mode — `--symbol` or `--all`
  (which sweeps `def_symbols` smallest slice first) is required, and `--out`
  makes the sweep resumable. `--crc-normalize` retries an inconclusive verdict
  on the normalized (and pruned) system, upgrade-only (`YES (normalized)`; see
  the CRC normalization section).
- **`termination [--symbol NAME]… | --all [--budget S] [--aprove-bin P] [--emit] [--itrs] [--itrs-lengths] [--no-child-split] [--no-integer-retry] [--out TSV] FILES…`**
  — proves per-slice termination by unravel + a direct AProVE run
  (`Termination.check`; **no MTT** — see below). Up to **four paths**, in
  order, stopping at the first YES: the whole slice as one TRS, then that run's
  open dependency-graph children one at a time (`--no-child-split` disables),
  then the integer surface where the slice computes arithmetic at all, then
  that surface with the LIST layer lifted to lengths as well — for a descent
  that shortens a list rather than counting an integer down, and an export
  `ERROR` where the lift does not cover the slice (`--no-integer-retry`
  disables both integer paths; `--itrs` / `--itrs-lengths` run one of those
  surfaces *alone*, which is what the ITRS columns were measured with). Row per
  symbol:
  `<sym>\t<YES|NO|MAYBE|TIMEOUT|DEGENERATE|ERROR>\t<stats>\t<secs>\t<paths>`,
  where `<paths>` is every path taken with its own verdict and seconds
  (`whole:TIMEOUT@300.0s children:YES(all children closed)@41.2s`) — that is
  where a failed split's own numbers survive a row the plain run's verdict
  won. `--all` sweeps `def_symbols` smallest slice first; `--out` makes the
  sweep resumable (recorded symbols are skipped). `--emit` prints the TPDB
  TRS instead of running AProVE.
- **`scc [--symbol NAME]… | --all [--timeout S] [--ceta-maude-bin P] [--mfe271-dir D] [--emit] [--out TSV] FILES…`**
  — per-slice sufficient completeness via CETA Maude 2.7 + the old MFE 2.7.1
  (`Scc.check`); the row format is the retired `run-scc.sh`'s
  (`<sym>\t<verdict>\t<fidelity[/analysis:…][/dom:…]>\t[witness]`), including
  the exact/approx fidelity and the witness-domain triage. `--all` sweeps every
  defined head (the derived `subty_`/`match_`/`holds_` predicates included);
  `--emit` prints the pruned functional module the checker sees.
- **`run [--structural] [--start TERM | --imp FILE… | --p4 FILE… -i DIR] [--emit] [--search|--rewrite] [--relations-as-rules] [--bound N] [--check-p4] [--maude-bin P] [--timeout S] FILES…`**
  — emits the execution module and runs a start term through a local Maude via
  reflection (`Maude_run`). `--imp`/`--p4` build the start term from a source
  program (repeatable — batches through one Maude invocation, amortizing
  module internalization); `--emit` just prints the module. `--check-p4` also
  typechecks each `--p4` program with the interpreter and diffs the RESULT
  value against Maude's (`Of_maude`) — the Phase D oracle. `--timeout` defaults
  to 0 (no limit); see "Performance notes" for why a fixed default cannot work.

The `--all`/`--out` sweep machinery (root selection, resume, row formatting)
shared by `confluence`/`termination`/`scc` lives in
[lib/cli/analysis_sweep.ml](spectec/lib/cli/analysis_sweep.ml), not `bin/main.ml`
— `bin/` keeps only the command wiring.

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

> **ChC (Coherence) measurement halted (from 2026-07-24).** `Mfe.check` still
> runs both checkers and returns `{church_rosser; coherence}`, but across the
> entire ≤500 sweep the ChC verdict tracked CRC exactly (CRC YES → ChC YES,
> CRC unreached → ChC `-`) with no independent defect signal, so the tables and
> summaries no longer carry a ChC column — axis dropped in
> [verification.md](verification.md); rationale in [verification-notes.md](verification-notes.md).

**Whole-system CRC explodes; per-symbol slices (`confluence --symbol`) are the
practical unit.** Current calibration (counts, per-symbol tables, and the two
recurring MAYBE causes — free RHS variables bound only by a premise, and owise
overlap — with their fixes `fold_premise_binders`/`Reflect.owise`) is tracked in
[todo.md](spectec/lib/rewrite/todo.md) ("Mfe calibration") rather than
duplicated here, since it shifts as reflection coverage grows.

**Termination** is wired through this library too: `main.exe termination`
(`Termination.check` = `Unravel` + `Aprove`, needing only
`tools/aprove/runme`). The old external Maude 2.7.1 + MTT stack
(`run-termination.sh`) is retired — the section below is why.

### Do not route termination through MTT — unravel and call AProVE directly

**MTT's transformation, not AProVE and not the translation, was the cause of the
long-standing termination MAYBEs.** Unravel the slice yourself and hand the
result to AProVE. (2026-07-19; causal analysis corrected 2026-07-20 — see the
note at the end of this section for what the first version got wrong.)

**MTT does not unravel at all.** Capturing the file it actually hands AProVE
(point `mfe.config` at a wrapper that copies `$1` aside) shows it emits a
*conditional* TRS in TPDB format. What it does instead is:

1. lower order-sorted to unsorted by turning every sort into a **predicate**
   (`isText`, `isList`, `is[Val]`) and attaching per-variable sort conditions to
   every rule;
2. rewrite each condition `s = t` into **`equal(s,t) -> tt`**, with one global
   non-left-linear rule `equal(X,X) -> tt`.

Step 2 is what kills it. Our conditions are *matching* conditions — in
`... if text = cons(t_h2, t_t)`, the variables `t_h2`/`t_t` are **bound by the
match**. Turned into a symmetric equality *test* they become free, and the rule
MTT emits is:

    $join-text(cons(t-h1, text), t-sep)
      -> cat(cat(t-h1, t-sep), $join-text(cons(t-h2, t-t), t-sep))
       | equal(match-cons(text), true) -> tt,
         equal(text, cons(t-h2, t-t)) -> tt, ...

`t-h2` and `t-t` occur nowhere in the lhs. That is an **extra-variable CTRS
(Bergstra–Klop type 3)**: the recursive argument `cons(t-h2,t-t)` bears no
syntactic relation to the lhs argument, and equals `text` only *semantically*,
via the `equal` condition. The dependency-pair framework sees no descent to
orient, and type-3 termination is far more expensive than type 1/2.

Expensive, not impossible: feed the captured file to AProVE with enough budget
and it says YES. But **MTT always calls AProVE with a hardcoded 120 s budget**
(`mtt.maude:90`, `termCheck(TOOL, In:String, 120)`), so the expensive problem
gets cut off. The `TERMA_TMO=1200` we set is a timeout on the *Maude process* and
never reaches this inner limit — which is why raising it never changed a verdict.
So a MAYBE here is the product of two factors: the `equal` encoding makes the
problem ~100× more expensive (a 13-rule `$join_text` needs >120 s there versus
**1 s** as an unraveled TRS), and the 120 s cap then truncates it. Either one
alone would have been survivable.

Unraveling restores the matching semantics, which is the whole fix: the
condition's right-hand side goes back to being a **pattern in a rule's lhs**, so
its variables are bound by matching and the descent is syntactically visible.

    f(p1..pk)      -> u_1(s, <carried>)
    u_1(t, <carried>) -> r          -- t matched here; its variables are bound

This is what `main.exe termination` does (`unravel.ml` builds the plain TRS
from the CTRS values directly; `aprove.ml` hands it to
`tools/aprove/runme <file.trs> <budget>`, WST mode; `--emit` shows the
TRS).

**What `<carried>` is decides which processor closes the cycle** — and at scale
that decides everything. `unravel.ml` passes the lhs **variables, flat**
(Marchiori's classic form). Wrapping the lhs *argument list* verbatim in an
inert `k_N` constructor, which it shipped until 2026-08-19, is equally sound and
equally provable on a small slice (both YES in 0–1 s, which is how the choice
was first — and wrongly — judged irrelevant), but it hides the descent from
size-change analysis: the recursive argument meets a `k_N(...)` that *contains*
it, so no edge is recorded and only a reduction-pair search can close the cycle.
One-variable re-measurement on the 577 dependency-graph children of
`Program_ok`: **keep 317 closed, flat 502**. (One trap either way: passing the
bare lhs *term* makes the rule reproduce its own redex — AProVE correctly
answers NO.)

**A whole-system TIMEOUT is then split.** AProVE's `(4) AND` node is one QDP
child per SCC of the dependency graph, and all 577 of them share the run's ONE
deadline — so a child its tree leaves open may be hard or may simply never have
been reached. `termination` re-poses each open child as a standalone problem
([qdp.ml](spectec/lib/rewrite/qdp.ml)) and gives it the budget alone; all closed
⟹ YES, by the soundness of the `AND`. Two details make it cheap and sound: the
child carries **our** exported rules (a superset of whatever the run had reduced
them to — a superset only adds chains), pruned to the pairs' **usable rules**
with the Cε pair `c(x,y) → x | y` alongside (sound on a minimal problem; the
prune is skipped when the echo does not say *minimal*). Measured on four of
`Program_ok`'s open children: same YES verdicts, 33–36 s → **1–4 s**, since
~30 s of each was parsing the 4 MB rule set the unpruned child carried.

Measured effect (153 ≤500-rule symbols): `term` goes from YES 117 / MAYBE 12 /
TIMEOUT 24 through MTT to **YES 153/153**. The slices MTT could not close —
`$join_text`, `$invalidate_headerUnion`, `$invalidate_value` — burn the full
1200 s budget to reach MAYBE through MTT and prove YES in about one second here.
The CTRS encoding needs no change; this is purely a defect of the checking
pipeline.

The sort-predicate encoding (step 1) contributes bulk but is not the mechanism —
the `equal` rewriting is.

Implementing it correctly needs three details (`unravel.ml` encodes all three,
and its tests pin them), each of which silently produces a wrong or malformed
system if missed:

1. **Multi-condition chains must accumulate bound variables.** A condition can
   bind variables a later condition or the final rhs uses
   (`... if p(x) = cons(h,t) /\ q(h) = true`). Each helper must carry whatever it
   carries *plus* every variable bound by the conditions before it, or the last
   rule has an extra variable in its rhs — reintroducing exactly the type-3
   defect this whole approach exists to avoid.
2. **Emitted conditions are not always in dependency order.** Reflect's guard
   passes can place an `isStuckHead` guard ahead of the condition that binds the
   variable it inspects (`$bitstr-to-int`). Re-order greedily: repeatedly take
   the first condition whose left side is fully bound, then treat its right side
   as a binder.
3. **Verify coverage, don't assume it.** Fail loudly on any equation not
   accounted for and on any rule with an unbound rhs variable. Both bugs above
   were found that way; both had produced plausible-looking output.

Soundness: every CTRS step is simulated by the TRS
(`lσ -> u(sσ, carried) ->* u(tσ, carried) -> rσ`), condition evaluation
included, so **TRS termination ⟹ CTRS operational termination**. Sorts and
subsorts are dropped, making the TRS an over-approximation (well-sorted terms
are a subset) — the same safe direction MTT has. The corollary matters:
**a NO verdict does *not* prove the source non-terminating** — it may be an
artifact of the over-approximation, so treat NO as something to investigate, not
a witness.

Analysis-surface slices are safe inputs for this: they carry no `owise` (and
should one appear, `unravel.ml` drops the attribute and keeps the rule — an
over-approximation, the same safe direction), no `rl`/`crl`, no
`assoc`/`comm`/`id:` attributes, no `:=`/`=>` conditions, no imports, and no
mixfix. Signature pruning (`--prune-signature`) touches only the *signature*,
never a rule — and the unraveling drops sorts anyway, so the termination path
does not prune at all.

> **Correction (2026-07-20).** The first version of this section claimed MTT
> unravels by passing the condition's variables, that decomposing a structured
> argument this way inverts the subterm relation, and that the proof was
> therefore unreachable at any budget. All three were wrong — MTT does not
> unravel, so nothing is decomposed or inverted, and the proof is reachable given
> budget. The error came from comparing whole *pipelines* and then blaming one
> component inside them, when the two paths differed in sort encoding, condition
> encoding, and AProVE budget all at once. Three one-variable-at-a-time
> experiments settled it: swapping only the unraveling style (variable-passing vs
> variable-passing vs keep-constructor) leaves both at YES; running MTT's captured output
> directly gives YES; and `mtt.maude` turns out to hardcode a 120 s budget. The
> measurements and the operational conclusion (don't use MTT; unravel yourself
> and call AProVE) were unaffected. **Lessons: to claim X causes Y, change X and
> nothing else — and when a tool fails, get hold of what it actually emits before
> theorising. Repointing `mfe.config` at a wrapper took five minutes and
> collapsed the whole wrong story.**

### CRC normalization (`--crc-normalize`) — remove determinacy critical pairs

The CRC re-encodes a matching condition `$f(A) = t` as a joinability obligation
and, not knowing `$f` is a function, raises a **determinacy critical pair** it
cannot close (`#v# = v`). That single cause accounts for the five `$write_value*`
MAYBEs and several large-slice TIMEOUTs. `--crc-normalize` rewrites those binders
away before the checker runs. Analysis-surface only — execution, termination, and
ChC never see it, and a run without the flag (or one that produces no chain
operator) is byte-identical; opt-in because it is a verdict-shaping transform.

Three levers, of different strength — this is the crux:

- **inline** (`fold_premise_binders ~aggressive`) — inline every single-variable
  binder `v := $f(A)`, dropping the use-once cap. Duplicating a deterministic
  producer is an **equivalence** (preserves *and* reflects confluence), so it is
  safe as a blanket and its verdict transfers to the original both ways.
- **unravel** (`crc_unravel`) — turn a tuple/constructor binder
  `tuple(v,b) := $f(A)` into a fresh `crcu`/`crck` chain, moving the binding into
  an lhs pattern. Unraveling **reflects but does not preserve** confluence
  (Marchiori 1996; Nishida–Sakai–Sakabe LMCS 2012): a YES on the unraveled module
  proves the original confluent (soundness holds for these left-linear structural
  rules), a MAYBE is inconclusive. So use it **upgrade-only** — run the plain CRC
  first, normalize only its MAYBE/TIMEOUT symbols, promote a normalized YES, and
  *never* downgrade an original YES. `confluence --crc-normalize`
  (`Mfe.check_normalize_upgrade`) automates exactly this per slice, printing an
  upgraded verdict as `YES (normalized)`. (Termination transfers cleanly under
  unraveling — that is the MTT-replacement path above — but confluence only
  reflects, hence the asymmetry.)
- **real-sort** (`To_mfe`, `crc_sigs`) — `crcu`/`crck` are not IL symbols, so
  `Maude_sorts.signature` defaults them to all-`Val`. That is *sound* (widening a
  sort only adds overlaps, never a false YES) but widens the chain-step pattern
  variables, and the spurious overlaps can cost the verdict (`$set_priorities`
  TIMEOUTs under all-`Val`). Recover a real signature from the emitted rules:
  `crck<id>` argument sorts = its carried variables' inferred sorts (typed in the
  level-0 producer, whose lhs head is still the original `$f`), result a fresh
  `CrcKeep`; `crcu<id>` = subject sort, `CrcKeep`, mapping to the function's result
  sort. Deeper chain steps fill in ascending id (= level) order, each typed once
  its dependencies have real sorts. The narrowing is sound like the rest of the
  order-sorted encoding — a carried variable keeps the sort of the value it holds,
  so only spurious pairs go.

Measured (real-sort binary, `prune` companion, serial Maude): the five
`$write_value*` MAYBEs close to YES; `$bin_concat` TIMEOUT→YES (inline+prune, no
chain); `$set_priorities` YES 78 s (all-`Val` TIMEOUTs). `$bin_shl`/`$bin_shr`
stay TIMEOUT (critical-pair blowup the pruning can't tame).

`$join_text` first regressed YES→MAYBE under normalization — a lesson in *what*
to unravel. `crc_unravel` was unraveling every constructor-pattern binder, but a
determinacy critical pair only arises when the binder's SUBJECT is a
defined-function call; a value destructure `text = cons(t-h2, t-t)` (subject a
bound variable) has none, and `Reflect.hoist_matchers` respells `match_K(v)=true`
into exactly that destructure so the CRC can see through it. Unraveling it split
the destructure into a `crcu` consumer, leaving only the opaque
`match-cons(text)=true` at the sibling overlap, so the CRC could no longer pair it
against the base clause's `len=bone` (⟹ `text=nil`) to see the contradiction.
Gating the unravel on a defined-function subject (`1dd1e43a`) restores
`$join_text` to YES (0 pairs, 6 s) and leaves the `$write_value*`/`$set_priorities`
chains (function-call subjects) untouched. Unraveling still only *reflects*
confluence in theory, so the sweep stays upgrade-only as a safety net. Fuller
writeup and the sweep protocol live in [todo.md](spectec/lib/rewrite/todo.md)
(2026-07-21).

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
  write_value) **Cause corrected 2026-07-19/20:** this was blamed on AProVE's
  dependency-pair analysis, which was wrong. AProVE certifies the descent in
  about a second — but only if the premise's destructuring survives as a *match*.
  MTT rewrites it into an `equal(s,t) -> tt` test, which frees the pattern's
  variables and makes the descent invisible. `main.exe termination` unravels
  and these all come back YES — see "Do not route
  termination through MTT" above.
- Modular-(B) arith-blindness: the measure lives in the black-boxed arithmetic
  (e.g. `$shr`'s `bpred`); closed only by the (A)-lift. Real termination holds.
- Acyclic call graph + large slice → pure tool-budget TIMEOUT.

**The one real family — binenc zero-width / zero-value boundary (spec, fixed
2026-07-29).** `$write_value_from_bits'` at `integerValue.V, n_var = 0`: two
order-sensitive `def` clauses share the `V` constructor, but the general clause
carried no `n_var ≠ 0` guard and no owise, so both fired at `n_var = 0` with
different results (keep the original field vs overwrite with
`$int_to_bitstr(0, …)`) — a latent non-confluence masked only by rule order. The
guard the clause order was carrying is now written out in
[2.1.2-value-aux.spectec](spectec/specs/p4/2-static-runtime/2.1.2-value-aux.spectec)
as `-- if $(n_var > 0)`. The same family as the `$bitstr_to_int` /
`$int_to_bitstr` w=0 non-termination, which is still open.

This section used to call that overlap *the* root cause of the five
`write_value*` CRC MAYBEs; measuring the fixed spec refutes that. The base CRC
on `$write_value_from_bits_prime` is still `MAYBE` (118.8 s) and only
`--crc-normalize` lifts it to YES, exactly as before the fix — so the operative
cause of those MAYBEs is the determinacy critical pairs of the tuple binders
(see the CRC-normalization section), and the `n_var = 0` overlap was a real
order-dependence defect sitting on top of them, not their source.

**Lessons: when
translating order-sensitive `def` clauses that share a constructor, preserve the
disambiguating guard (or owise), and always check the 0-width / 0-value
boundary. Spell a "≠ constant" guard on a nat/int as an arithmetic comparison —
`=/=` translates to the *generic structural* `eq`, whose defining table (50,629
rules) then joins the dependency closure and takes the slice from 271 rules to
50,900; `$(n_var > 0)` uses the arithmetic prelude's `leq` and leaves the slice
at 271. Re-measure `rewrite --list-symbols --sizes` after any such spec edit.**

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
| `5647b883` | 2026-07-13 | **CRC/ChC + termination**: both columns of the 153-symbol sweep re-measured on this analysis surface (`21eac0b6`'s matcher-guard fold) — term at `3fbbe1d6`, CRC/ChC here ([verification-notes.md](verification-notes.md)) | differential |
| `bff805ec` | 2026-07-23 | **CRC/ChC + termination, with per-symbol timing**: full 153-symbol ≤500 sweep re-measured on this tree with the `Subproc.timed` instrumentation — CRC/ChC YES 146 (5 normalized) / TIMEOUT 7, term YES 153/153. **The term column of this row is RETRACTED** (2026-08-31): it predates `bae6e096`, which fixed a verdict parser that read a sub-proof's `YES` out of the echo of a run AProVE had given up on, so a `KILLED` run was recorded as a proof. The defect is not merely *earlier* than this anchor, it is **older than the subcommand's first measurement**: `verdict_line` scanned the whole output from `e518f5d4` (the commit that introduced `termination`, one hour before this one), so **the AProVE bridge never read a verdict correctly until `bae6e096`**. Do not re-litigate this from `025c7979`'s title ("end an AProVE run at the verdict") — that commit added `done_when` and made the misreading worse, it did not create it; check `git show <commit>:spectec/lib/rewrite/aprove.ml` instead. 34 of its 145 still-existing rows answer AT the deadline, which is that bug's signature, and 32 of those 34 do not prove on the plain surface today even at budget 1800; the other 2 (`$strip_prefix_rec`, `$strip_suffix_rec`, YES in half a second there) bisect to `b1ca0af6`, the fix that restored the 41.6% of conditional rules the `isStuckHead` guard had been deleting from the TRS — so all 34 are measuring-side artifacts and NONE of them is a regression — see [term_145_2026-08-22.tsv](sweeps/term_145_2026-08-22.tsv) and the 2026-08-31 entries in [todo.md](spectec/lib/rewrite/todo.md). Read `term YES 153/153` as **YES 111 measured + 34 unproven**. **ChC column halted 2026-07-24** (verdict always tracked CRC — dropped from the tables). Times taken **serially** (one fresh process per symbol): parallel sharding was verified verdict-identical (0 false timeouts) but inflates heavy-symbol times up to ~40× under memory contention, so it is unfit for timing. `$bin_minus` term needs budget 1800; `$write_bits_from_value` term needs AProVE `-Xss512m` (proof-export stack overflow, not a search failure) ([verification.md](verification.md) / [verification-notes.md](verification-notes.md)) | differential |

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
- The **termination column is stale as of 2026-08-20 in four ways at once**, so
  every `sweeps/term*.tsv` and `sweeps/itrs*.tsv` needs re-measuring whole (not
  patched row-wise): the unraveling now carries the lhs variables flat rather
  than wrapped (2026-08-19), `termination` takes three paths instead of one
  (whole → children → integer), the integer export now goes through
  `rebind_stuck_guards` like the plain one — before that it proved a system with
  every stuck-guarded rule silently deleted, so its YES verdicts do not carry —
  and the integer surface now lifts an `eq` guard against an integer literal
  into a `:|:` constraint instead of leaving it a chain step, which is what a
  `-- if n =/= 0` compiles to (`$fresh_typeIds` TIMEOUT → YES 16.5s at the same
  budget). The row also grew a fifth column (the paths taken).
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

**Those rows are stale as of 2026-08-05** and are kept only as the last figures
actually measured. The equality rework (`5dd8f11d` type-indexed `eq`, `1585e814`
case-index discrimination, and the monomorphization that followed) took the
execution module from ~78k lines to **~29k**, and `run --emit` from ~10s to ~1s;
internalization and per-program cost scale with module size and have not been
re-measured (this environment has no `spectec/tools/maude`).

The fixed ~80s is why **`run`/`run --structural` default to `--timeout 0` (no
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
