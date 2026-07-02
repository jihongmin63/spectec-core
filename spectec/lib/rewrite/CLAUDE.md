# `rewrite` library — IL spec → conditional term rewriting system (CTRS)

> **⚠️ SKELETON (new-rewrite 브랜치).** 이 브랜치는 `rewrite` 브랜치를 재작성하기
> 위한 *골격*입니다. `.mli` 인터페이스·데이터 모델(`rewrite_system.ml`)·심볼/빌더
> 레이어(`to_ctrs.ml` 상단)·thin 구조 질의는 온전하지만, **번역 로직은 비어
> 있습니다**: `To_ctrs.of_spec`/`var_type_hints`, `Simplify.simplify_spec`,
> `To_maude.*`, `Of_maude.*`가 `failwith "TODO(new-rewrite): ..."` 스텁입니다.
> 베이스는 `origin/main`이고 이 라이브러리가 의존하는 IL(record `typcase`/`var`,
> `Mode.t` reltyp 등)은 **이미 베이스에 들어 있어 골격이 클린 빌드됩니다**
> (`opam exec --switch=spectecx -- dune build lib/rewrite/rewrite.cma`). 즉
> *컴파일은 되지만 기능은 비어 있는* 상태이며, 위 스텁을 채우면 동작합니다. (스텁이
> 미사용으로 남긴 `to_ctrs.ml` 빌더 레이어는 그 파일 상단의 `[@@@warning "-32-69"]`로
> 막아둠 — 번역 재구현 후 제거.) 아래 문서는 재구현의
> 설계 기준(원래 의도한 파이프라인)으로 그대로 유지합니다 — **골격은 핵심
> 척추(데이터 모델·`to_ctrs`/`simplify`/`to_maude`/`pipeline`/`rewrite`)에 더해
> 지원 패스(`exp_map`/`defunctionalize`/`gensym`/`builtin`)를 `rewrite` 브랜치에서
> 복구해 `pipeline.ml`에 다시 배선했고**(자세히 [CORE_LOGIC.md](CORE_LOGIC.md) §5;
> `of_spec`/`Simplify` 스텁에 막혀 런타임은 아직 `failwith`), **나머지
> (`prem_env`/`maude_theory`/`maude_run`/`of_maude`/`cocoweb`/`muterm`/`aprove`/
> `termination`)는 삭제 상태입니다.** 삭제된 모듈들의 핵심 로직은
> [CORE_LOGIC.md](CORE_LOGIC.md)에 보존돼 있으니 재구현의 1차 기준으로 삼고, 필요하면
> `git checkout rewrite -- <file>` 로 원본을 복구하세요. (아래 "모듈 map"은 삭제분을
> 포함한 *전체* 설계를 기술합니다.)

> **의사소통은 한글을 선호합니다.** 설명·요약·질문은 한글로 작성하세요 (코드,
> 식별자, 인용한 에러 메시지는 원문 그대로).

This library translates an **elaborated SpecTec IL spec** into a **COPS
conditional term rewriting system** so that off-the-shelf tools can decide its
**confluence** (CoCoWeb) and **termination** (AProVE for unconditional systems,
MuTerm for conditional ones). It is the `spectec.rewrite` public library
(`lib/rewrite/dune`).

## Pipeline at a glance — TWO pipelines since the built-in-theory split

```
                                                    ┌─(analysis)──▶ Rewrite_system.t ──{Cocoweb, Termination}──▶ verdict
Lang.Il.spec ──Simplify──▶ Lang.Il.spec ──To_ctrs──┤                (COPS / TPDB text; structural scalars)
 (elaborated)                (simplified)           └─(execution)──▶ Maude_theory.native_system ──▶ To_maude ──▶ Maude module
                                                                     (scalars on Maude's built-in Bool/Nat/Int/String)
```

- **Analysis** (`Pipeline.ctrs_of_spec`: `Defunctionalize.defunctionalize` the
  spec, then `To_ctrs.of_spec ~extra_defs:(Builtin.rules_of_builtins spec)
  ~orig:spec (Simplify.simplify_spec spec)` piped through `Gensym.thread`;
  entry `Rewrite.rewrite_spec`): self-contained structural scalars — Peano nats,
  sign-magnitude ints, char-list texts, own booleans — because a CTRS has no
  external theories. Feeds COPS/TPDB; goldens pin this surface.

> **⚠️ new-rewrite 설계 변경 (② 제거):** 아래 Execution 설명은 *옛* 설계다. 새
> 골격은 `Maude_theory.native_system`(구조적 시스템을 다시 fold하는 별도 패스)를
> 삭제하고, `Pipeline.maude_system_of_spec`을 `To_ctrs.of_spec ~scalars:Native`
> **직접 경로**로 바꿨다 — 스칼라 인코딩/ prelude 생략이 번역 단계에서 일어난다.
> 권위 있는 현재 설계는 [CORE_LOGIC.md](CORE_LOGIC.md) §1·§6.1. 아래 단락은 fold가
> *무엇을* 했는지의 참고로만 읽어라.

- **Execution** (`Pipeline.maude_system_of_spec` = `Maude_theory.native_system`
  over the analysis system; consumed only by `To_maude`): ground scalar values
  fold into wrapper constructors over the built-ins (`nat(3) : NatV`,
  `int(-5)`, `bool(true)`, `txt("E.")`), the hand-written scalar prelude rules
  are dropped, and `To_maude` re-emits each surviving operator as a one-line
  delegation (`eq add(nat(X), nat(Y)) = nat(X + Y)`) — constant-time via GMP.
  The built-in sorts stay OUTSIDE `Val` (kinds must not merge, or imported
  operator attributes clash); `TextE ""` stays the bare `nil` and is bridged by
  `List < Text` plus `eq`/`cat` nil-equations.

The **original** (pre-simplify) spec is threaded through `of_spec` because type
definitions and relation signatures (needed for constructor/matcher/subtype
rules and for splitting relation args into input/output) must be read from the
un-simplified form.

## Module map

| File | Role |
|------|------|
| [rewrite.ml](rewrite.ml) / [.mli](rewrite.mli) | Top-level facade: `rewrite_spec`, `def_symbols`; re-exports the public submodules. |
| [pipeline.ml](pipeline.ml) / [.mli](pipeline.mli) | The two pipeline entries: `ctrs_of_spec` (analysis, structural) and `maude_system_of_spec` (execution, built-in theory). |
| [maude_theory.ml](maude/maude_theory.ml) / [.mli](maude/maude_theory.mli) | The native (built-in) scalar **vocabulary**: the wrapper symbol spelling (`nat`/`int`/`bool`/`txt`) and the literal builders (`nat_t`/…/`string_literal`) that `Ctrs_term` (Native leaf emission), `To_maude` and `Of_maude` must agree on. No fold pass — `Ctrs_term`'s mode-aware leaf builders emit these wrappers directly at translation time. |
| [rewrite_system.ml](rewrite_system.ml) | **Data model + diagnostic printer** for a CTRS (`type t`, `term`, `rule`, `cond`; `string_of_term`/`string_of_rule` for messages). Also the **shared lexical layer**: the CTRS-safe id scrub `sanitize` (`mnemonic_of_char`) and the Maude id/variable mangling `maude_id`/`maude_var` — defined here, the lowest layer, so the symbol-naming layer (`Ctrs_term`) and both Maude surfaces agree on one spelling. `slice`/`reachable_heads`/`drop_owise`. No translation logic and **no Maude module emission** here — both order-sorted surfaces live in `maude/` (`To_maude` execution, `To_mfe` analysis), which can read the IL spec to recover sorts. **(new-rewrite: the old single-sort `string_of_system_maude` was replaced by the order-sorted `To_mfe`; the older COPS/TPDB printers, `ctype`/`comment`, and `is_unconditional` were removed with CoCoWeb/AProVE/MuTerm.)** |
| [maude/maude_sorts.ml](maude/maude_sorts.ml) | **Shared order-sorted signature recovery** for BOTH Maude surfaces. Recovers each CTRS symbol's Maude sort from the original IL spec (`type_env`/`sort_of_typ`/`recover`/`signature`), the subsort order (`is_sub`/`meet`), per-rule on-the-fly variable sorts (`infer_var_sorts`), term printing (`print_term`), and op collection (`symbol_arities`/`il_constructor_syms`). Theory-agnostic: `sort_of_typ` gives the same sort names both ways; only `scalar_ctor_sigs`/`is_literal` branch on `scalar_theory` (native `nat`/`int`/`bool`/`txt` wrappers vs structural `zero`/`succ`/`int_pos`/`true`/…). `recover` also registers matchers (`match_<T> : <T> -> BoolV`); `subty_*` and the iteration helpers stay `Val` (irreducible / theory-specific arg sorts). |
| [maude/to_mfe.ml](maude/to_mfe.ml) | **Analysis Maude surface** for the MFE: emit the structural CTRS as an **order-sorted** Full-Maude `(mod … endm)` reusing `Maude_sorts` — structural scalar constructors, `set include BOOL off .`, `eq`/`ceq` + `rl`/`crl` for `rule_heads`, conditions in join form `s = t`, per-occurrence `name:Sort` variables. No `isStuckHead`/delegation/owise-totalization (those are execution-only). Consumed by `--ctrs` and `Mfe.check`. |
| [ctrs_term.ml](translate/ctrs_term.ml) | **Structural CTRS vocabulary** (no `.mli`, like `rewrite_system.ml`): the `scalar_theory` (`Structural`/`Native`) type, the symbol-naming conventions (`variant_sym`/`func_sym`/`rel_sym`/…, built on `Rewrite_system.sanitize`), the smart term/rule builders (`app_t`, `cons_t`, `int_pos_t`, `rule`/`rule_cond`, operator dispatch, `char_codes_of_rules`), and the **mode-aware scalar leaf builders** (`bool_t`/`term_of_num`/`text_t`/`nat_lit`/`int_lit`/`conj_t` taking `~scalars`): `Structural` emits the self-contained Peano/sign-magnitude/char-list/own-bool scalars, `Native` emits `Maude_theory`'s `nat`/`int`/`bool`/`txt` wrappers directly (empty text stays bare `nil` in both). The one place raw `R.App`/`R.Var` is built; the structural-scalar counterpart to `maude_theory.ml`. Shared by `To_ctrs`, `Builtin`, `To_maude`, `Of_maude`, `Gensym` (each aliases it `module T = Ctrs_term`). |
| [prelude.ml](translate/prelude.ml) / [.mli](translate/prelude.mli) | **The fixed prelude** (`Prelude.rules ~scalars`): booleans, Peano-nat / sign-magnitude-int arithmetic, list/option operations and matchers, structural equality — giving `Ctrs_term`'s symbols their rewriting semantics, with mode-aware boolean leaves. One ordered list (so `Structural` is byte-stable); on `Native` the `native_replaced_heads` rules + scalar `eq` are dropped (`kept_in_native` filter) because `To_maude` delegates them to Maude's built-ins. Appended + pruned by `of_spec`. |
| [to_ctrs.ml](translate/to_ctrs.ml) / [.mli](translate/to_ctrs.mli) | **The translation heart**, built on `Ctrs_term`'s vocabulary, threading `~scalars` so every scalar leaf is emitted in the right theory at translation time (no fold pass): `term_of_exp`/`pattern_of_exp`, the iteration compiler (`$itermap`/`$unzip`/`$iterall`/`$itercollect`/…), the subtype predicate (`sub_pred` — **total**: `defs_of_typ`가 멤버를, `sub_helper_defs`의 사용-기반 false-보완이 비멤버를 `subty_<T>(비멤버 케이스) -> false`로 결정 — interp.subtyp과 같은 total boolean이라 `~(e <: T)`가 양 표면에서 환원됨), `defs_of_typ`, premise→condition lowering (`conds_of_prem`), `rules_of_def`, and the top-level `of_spec` (+ `def_symbols`/`input_moded_rel_syms`/`rule_head_syms`, `single_case_ctor`/`case_ctor`). `scalar_theory` is re-exported from `Ctrs_term`. |
| [var_hints.ml](translate/var_hints.ml) / [.mli](translate/var_hints.mli) | `Var_hints.of_spec`: per defined symbol, the IL type of each variable in its clauses/rules (read off the simplified spec's `VarE` notes), so the typed `To_maude` backend can restore a variable's narrow declared type instead of the widened argument type. Consumed by `To_maude` only. |
| [simplify.ml](translate/simplify.ml) / [.mli](translate/simplify.mli) | Pre-pass over IL: expand variables into concrete structure (via `Prem_env`) and drop redundant premises. Runs **before** `to_ctrs`. |
| [prem_env.ml](prem_env.ml) / [.mli](prem_env.mli) | Union-find over IL expressions built from a rule/clause's premises; gives each expression its canonical (most specific) member. Consumed by `Simplify`. |
| [exp_map.ml](translate/exp_map.ml) / [.mli](translate/exp_map.mli) | Shallow one-level traversal helpers over IL: `map_subexps` / `subexps` / `map_path_exps` over expressions, `exps_of_prem` for the expressions a premise embeds (caller controls descent). |
| [builtin.ml](translate/builtin.ml) / [.mli](translate/builtin.mli) | Backend-local CTRS rules for P4's collection builtins (map/set/list/text) that `BuiltinDecD` declares but `To_ctrs` emits no rules for; fed to `of_spec` as `extra_defs`. |
| [gensym.ml](translate/gensym.ml) / [.mli](translate/gensym.mli) | Make the stateful gensym (`$fresh_typeId`/p4-old `$fresh_tid`) pure by state threading: every fresh-reaching symbol gains a trailing state argument and a `tuple(result, state')` result; issuing appends a prime to the last issued name (seed `"FRESH"` → `FRESH'`, `FRESH''`, …). Runs last in `ctrs_of_spec`; identity on gensym-free specs (impty golden untouched). |
| [defunctionalize.ml](translate/defunctionalize.ml) / [.mli](translate/defunctionalize.mli) | Specialize away `def`-valued arguments (`DefP`/`DefA`): each call `$f(args, def $g)` → a generated first-order copy `$f_$g` with `$check := $g` substituted through the template's clauses (worklist closure over recursion/chained templates; templates removed; no `DefA` may survive). Runs FIRST in `ctrs_of_spec`; identity without `DefP` (impty). |
| [to_maude.ml](maude/to_maude.ml) / [.mli](maude/to_maude.mli) | **Execution Maude backend**: emit the native-theory system as an executable order-sorted Maude module (sort recovery via `Maude_sorts`, op declarations, eq/rl printing, the built-in delegation equations, `isStuckHead`/owise totalization), plus the **META-TERM start-term encoding** (`print_meta_term`/`meta_term_of_value`/`meta_start_app`) the reflective `metaReduce` path runs. |
| [of_maude.ml](maude/of_maude.ml) / [.mli](maude/of_maude.mli) | **Reverse of the start-term encoder**: parse a Maude object normal form (`To_maude` vocabulary) back into a {!Lang.Il.value} via a forward table read off the spec (the sanitizing `variant_sym`/`struct_sym` spelling is lossy). `values_of_result` strips the gensym `tuple(result, state)` wrapper; `canonicalize` (applied to BOTH sides) renames `FRESH…` leaves so the two gensym models compare equal AND sorts each `map<K,V>`'s entries by `Value.compare` on the key (a map is unordered: interp renders it `VMap.bindings`-sorted, the translation insertion-ordered). Powers the result-VALUE oracle (`run --check-p4`, Phase D below). |
| [maude_run.ml](maude/maude_run.ml) / [.mli](maude/maude_run.mli) | Execution bridge: run an emitted module on a **META-TERM** start term with a local `maude` binary, reflectively (`metaReduce`/`metaRewrite`/`metaSearch` via a `META-LEVEL`-importing wrapper module), `downTerm` the result back to object syntax, parse the normal form, flag stuck heads. `run` does one start; `run_batch` runs a list of starts in **one** Maude invocation (sentinel-delimited per-start output) so the reflected module is internalized once for the whole batch — eliminating the per-program start-term parse (the old dominant cost; see the performance section). |
| [cocoweb.ml](cocoweb.ml) / [.mli](cocoweb.mli) | Confluence bridge: serialize → POST via `tools/cocoweb/cocoweb_client.py` → verdict. |
| [muterm.ml](muterm.ml) / [.mli](muterm.mli) | Termination bridge (conditional systems): same shape, `tools/muterm/muterm_client.py`. |
| [aprove.ml](aprove.ml) / [.mli](aprove.mli) | Termination bridge (unconditional systems): runs a **local** `aprove.jar` (`java -ea -jar … -m wst -t N file.trs`) directly — no Python client. See [tools/aprove/README.md](../../tools/aprove/README.md). |
| [termination.ml](termination.ml) / [.mli](termination.mli) | Termination **dispatcher**: `is_unconditional system` → AProVE, else MuTerm; normalizes the verdict and reports which `tool` decided. |

## The CTRS data model ([rewrite_system.ml](rewrite_system.ml))

```ocaml
type term = Var of string | App of string * term list   (* App(id,[]) prints bare *)
type cond = term * term                                  (* term == term *)
type rule = { lhs : term; rhs : term; conds : cond list }
type ctype = SemiEquational | Join | Oriented            (* we emit Join *)
type t = { ctype; vars : string list; rules : rule list; comment : string option }
```

Two textual surfaces, and **they differ for a reason** — keep them in sync only
where intended:
- `string_of_system` — COPS: leading `(CONDITIONTYPE JOIN)`, conditions `s == t`.
  For CoCoWeb / the `rewrite` CLI dump.
- `string_of_system_tpdb` — TPDB: **no** CONDITIONTYPE header, conditions written
  oriented `s -> t` separated by ` , `. MuTerm's parser **crashes** on the COPS
  surface (Haskell stack trace → falls back to MAYBE), so termination must use
  this form.

`slice t ~roots` restricts the system to the rules reachable (downward
dependency closure) from `roots` — used for per-symbol confluence/termination
checking. `def_symbols` gives the slice roots in declaration order.

## Symbol-naming conventions ([ctrs_term.ml](translate/ctrs_term.ml))

These **must agree** between the rule that *defines* a symbol and every rule
that *uses* it. All raw `R.App`/`R.Var` construction is confined to this layer.

- `sanitize` (now in [rewrite_system.ml](rewrite_system.ml), the shared lexical
  layer) — scrub a string to a CTRS-safe id (`[A-Za-z0-9]` runs kept, other
  chars become mnemonics, e.g. `->` → `minus_gt`, `&&` → `amp_amp`). The Maude
  surfaces then mangle a CTRS id `_`→`-` (`maude_id`) and scrub variable names
  (`maude_var`), also from `rewrite_system.ml`.
- **Arity is folded into variant/case symbols** (`variant_<origin>_<atoms>_<n>`)
  to remove same-atom different-arity clashes. Same-atom **same-arity** clashes
  are still possible (see [todo.md](todo.md) P3).
- `func_sym id` = `$` + sanitize (functions/`DecD`), `rel_sym id` = sanitize
  (relations/`RelD`). Constructors: `variant_sym`, `struct_sym`, `field_sym`,
  `match_sym`, `subty_sym`.
- Numbers: Peano `zero`/`succ` for nats; sign-magnitude `int_pos`/`int_neg` over
  nat magnitudes for ints. Lists: `nil`/`cons`. Options: `none`/`some`. Text
  chars: nullary `chr_<code>`.

## The prelude ([prelude.ml](translate/prelude.ml) `Prelude.rules`)

A fixed rule set defining booleans, Peano nat arithmetic, sign-magnitude int
arithmetic, lists/options and their operations (`add`, `leq`, `sub_int_nat`,
`cat`, `mem`, `idx`, `slice`, `upd_idx`, …). Type-derived rules
(`defs_of_typ`) and char-equality rules are appended; `prune_unused` then drops
any definition rule unreachable from the body rules.

## IL AST cheat-sheet (from [lib/lang/il/types.ml](../lang/il/types.ml))

The translation pattern-matches these. Key constructors `to_ctrs` cares about:

- **`exp'`**: `CaseE of notexp` (variant), `StrE` (struct), `OptE`, `ListE`,
  `ConsE`, `CatE`, `DotE` (field), `IdxE`/`SliceE`/`UpdE` (path),
  `CallE of id*targ*arg` (function call), `IterE` (iteration — compiles to a
  `$itermap` helper in value position, or a fresh binder + `$unzip` conditions
  in head-pattern position), `MatchE of exp*pattern`, casts
  `UpCastE`/`DownCastE`/`SubE`.
- **`prem'`**: `RelPr of relcall` (relation invocation), `IfPr of {cond; role}`,
  `LetPr`, `RelAssertPr of {call; expect}` (`expect=true` is the old `IfHoldPr`
  "holds"; `expect=false` the old `IfNotHoldPr` "does not hold"), `ElsePr`,
  `IterPr` (iterated premise — compiles to a `$iterall` predicate or
  `$itercollect` helpers). A `relcall` is `{relid : id; notexp : notexp}`.
- **`def'`** (all record-shaped): `TypD of {synid; tparams; deftyp}` (→
  constructor/matcher/subtype rules), `RelD of {relid; reltyp; rules}` where
  `reltyp = (typ, typ) Mode.t` carries the per-slot input/output tagging
  (`Mode.notation` recovers the `nottyp`; `Mode.partition reltyp args` splits
  args into inputs/outputs — replaces the old `int list` input indices),
  `DecD of {defid; tparams; params; typ; clauses}` (function clauses),
  `BuiltinDecD of {defid; tparams; params; typ; hints}` (emits no rules).
- **`pattern`**: `CaseP of mixop`, `ListP`, `OptP`.
- **`typcase`** is `{notation : nottyp; origin : typorigin; hints}` and a
  **`var`** (iteration binder) is `{varid : id; typ; iters : iter list}`;
  `IterT` is `{typ; iter}`. (All three moved from tuples to records.)

A clause is `{args : arg list; body : exp; prems : prem list}` (head args,
result, premises); a rule is `{ruleid : id; concl : notexp; prems : prem list}`.
Premises become CTRS **conditions** on the rewrite rule; the result/output
becomes the rhs.

## How the `Simplify` pre-pass changes the IL

`Prem_env.env_of_prems` builds a union-find from a block's premises; `Simplify`
then (a) substitutes each variable with its canonical concrete structure, (b)
folds `matches`/field-access constraints into head patterns (`reconstruct_pattern`,
`hoist_pairs`), (c) inlines value/let bindings, (d) turns subtype premises into
casts, and (e) drops premises the env renders redundant. This is **semantics-
preserving rewriting of the IL**, producing a spec whose clauses/rules map more
directly to CTRS rules. To debug whether an odd output comes from here or from
`to_ctrs`, pass `spec` (not `Simplify.simplify_spec spec`) as the 2nd arg to
`of_spec` (noted in [pipeline.ml](pipeline.ml)).

## CLI entry points (defined in [bin/main.ml](../../bin/main.ml))

- `rewrite [--ctrs] [--simplified] [--symbol NAME] [--relations-as-rules] FILES…`
  — default emits the executable Maude module (`To_maude.module_of_spec`).
  `--ctrs` instead dumps the analysis CTRS (`To_mfe.module_of_system`, the
  order-sorted text `verify` sends the MFE); with `--symbol NAME` only that symbol's dependency
  slice (`Rewrite_system.slice`) — handy to inspect/pin one slice's module.
  `--simplified` dumps the IL after the `Simplify` pre-pass.
- `verify [--symbol NAME] [--list-symbols] [--timeout S] [--maude-bin P]
  [--mfe-dir D] FILES…` — run the MFE CRC+ChC (`Mfe.check`) on the structural
  system; whole-system by default, `--symbol NAME` for one slice,
  `--list-symbols` to list the sliceable roots (`def_symbols`). Exit 0 iff both
  verdicts are `YES`. **Whole-system CRC explodes on critical pairs**, so the
  **per-symbol slice is the practical path** (no batch/`--jobs` driver in this
  skeleton — loop over `--list-symbols` yourself). (The old `--only`/`--whole`/
  `--jobs`/`--client`/`slice_check.ml` and the CoCoWeb/termination axes are the
  *deleted* legacy design.)

## MFE (Maude Formal Environment) — confluence + coherence gate [installed]

The live confluence/coherence gate is `Mfe.check` ([mfe.ml](mfe.ml)): it renders
the structural system as an **order-sorted** Full Maude `(mod SPEC … endm)`
(`To_mfe.module_of_system`, sorts recovered via `Maude_sorts` — so ill-sorted
overlaps no longer surface as spurious critical pairs the way the old single-sort
`Term` surface caused), loads the MFE into a local Maude, and runs the
**Church-Rosser Checker (CRC)** and **Coherence Checker (ChC)**. (The COCOWEB/
MuTerm/AProVE bridges below are the *deleted* legacy surface, kept here only for
the full design.)

**Installed in this environment** (gitignored; see [.gitignore](../../../.gitignore)
and [tools/mfe/README.md](../../tools/mfe/README.md)):
- Maude 3.5.1 (stock) at `spectec/tools/maude/maude`.
- MFE (`maude-team/MFE`) at `spectec/tools/mfe/`, entry **`src/mfe.maude`**. Loads
  under stock Maude 3.5.1 (CRC 3t / ChC 3t / SCC 2b) — Maude++ is not needed just
  to load + run CRC/ChC.

**MTT (Maude Termination Tool)** does not run in *this* (3.5.1) MFE — its
`termCheck`/`writeToFile` hooks need Maude++, and its transformations are Maude-2.x
syntax 3.5.1 rejects. But the termination path **does run on the matching
Maude-2.7.1 stack**, which is wired here (all gitignored under `spectec/tools/`,
full setup in [tools/mfe/README.md](../../tools/mfe/README.md)):
`tools/maude271-hooks/maude` (v2.7.1-ext-hooks, binds the hooks) + `tools/mfe271/`
(old MFE, bundles **MTT 1.5j** + old Full Maude) + `tools/aprove/aprove.jar` +
`tools/z3/z3`. Run one slice with `tools/mfe/run-termination.sh <symbol>`: it
dumps `rewrite --ctrs --symbol`, rewrites the header to an old Full-Maude `fmod`,
and drives `(select tool MTT .) (select external tool aprove .)` `(ct SPEC .)`;
MTT transforms the (order-sorted conditional) module to a TPDB CTRS and shells out
to AProVE per `mfe.config`. **Backend = Z3** (AProVE's default `SmtSolver=z3`); the
WST strategy's legacy yices-1.x calls abort gracefully, so the license-gated yices
is **not needed** (only slower for arithmetic-heavy slices). Verified: `FOO` and
the `$empty_map`/`$is_lpm_key_prime` slices → termination `YES`.

So the gate can pair **CRC local confluence** with an **MTT/AProVE(Z3) termination
proof** for full Church-Rosser. Standalone, the CRC yields local confluence +
sort-decreasingness (the CTRS assumed terminating).

**Protocol (calibrated; `mfe.ml` now encodes this — the old constants were
wrong guesses):**
- entry `src/mfe.maude` (not `full-maude.maude`); export `MAUDE_LIB` = the maude
  binary's dir so `mfe.maude`'s `sload file/process/time` resolve;
- the MFE is a Full Maude **loop reading STDIN** — pipe `load mfe.maude`, the
  module (first line `set include BOOL off .`), and the tool commands to stdin
  (a `maude FILE` invocation never feeds the loop the file's trailing lines);
- select the tool then check: CRC `(select tool CRC .)` + `(check Church-Rosser
  SPEC .)`; ChC `(select tool ChC .)` + `(check coherence SPEC .)` (a bare
  `(check … .)` with no tool selected is a parse error);
- **no clean quit**: the loop floods `> ` at EOF, so the bridge reads under the
  timeout and SIGKILLs once the ChC output is followed by that flood, parsing the
  printed verdicts (a killed process with a verdict is still authoritative; only
  no-verdict-at-deadline is `Timeout`);
- verdict tokens (whitespace-normalized substring): CRC confluent `The
  specification is locally-confluent.`; CRC pending `The following critical pairs
  must be proved joinable:`; ChC coherent `… no rewrite with rules can happen at
  non-overlapping positions of equations left-hand sides.`.

Whole-system CRC explodes on critical pairs; **`verify --symbol NAME` per-symbol
slices are the practical path** (`$lookup` → YES/YES ~1.4s). See
[tools/mfe/README.md](../../tools/mfe/README.md).

**Per-symbol verdicts recorded** (full table + analysis in [todo.md](todo.md)
"Mfe calibration"; use `verify --list-symbols --sizes` to find tractable
slices). impty/base: `$lookup`/`Check_*`/`Eval_*` = YES/YES, `Eval_prog` = CRC
MAYBE, `Run_prog` = TIMEOUT. p4 (159 slices ≤200 rules): 104 YES, **33 MAYBE**,
13 TIMEOUT, 9 degenerate (no rules); the 415 >200-rule symbols are
whole-system-ish → TIMEOUT.

**Full 574-symbol p4 sweep + triage tool** (see [todo.md](todo.md) "전체 p4
sweep"): the whole `verify --list-symbols` set is swept ascending by slice size
(`--timeout 120`, background; live `spectec/mfe_sweep.log`/`.tsv` symlinks).
[`triage_mfe.py`](../../triage_mfe.py) pulls the CRC=MAYBE / ChC=NO rows and links
each back to its IL declaration (`file:line`), and **re-concretizes
defunctionalization templates** (the 7 higher-order/generic `$find_overloaded<V>`-
class symbols show as size-0 NOVERDICT because Defunctionalize *copies* their rules
into per-instantiation `-of-<type>` names; the tool expands each to its concrete
CTRS ops and resolves each def-arg getter to source). The 9 degenerate (no-rule)
symbols are all rule-less by design, in three classes: 7 higher-order/generic
templates (rules copied to specialized names, checked in caller slices), 1 target
hook (`$init_objectState`, `dec` with no `def`), 1 extern relation stub
(`ExternFunctionCall_eval_lctk`, no `rule`).

ChC is YES throughout (no `rl`/`crl`: every relation
is input-moded → equations, so coherence is vacuous). The MAYBE non-confluence
has two analysis-surface causes (both approximations, not translation bugs — the
executable surface handles them with `:=`/`=>` and owise complements):
(1) a **free RHS variable bound only by a premise** (`$dom_map`, `$empty_store`,
`Eval_prog`: `f(x) = v if g(x) = v`, CRC can't join the two witnesses of opaque
`g(x)`); (2) **owise overlap** (`$is_lpm_key_prime`: `= true if x = "lpm"` vs
`= false [owise]` → CRC ignores owise → `true = false`).

Cause (1) is fixed by `Rewrite_system.fold_premise_binders`, a final
`ctrs_of_spec` pass that folds each premise-bound variable back into the rule —
an output binder `(prod, v)` inlined into the rhs, a pure-accessor destructuring
`(v, K(..))` folded into the head pattern (iteration-helper binders too).
Analysis-only (execution module byte-identical). It is **surgical** (pure
accessors only): folding a guarded clause would strip the `match_*` disjointness
guard the CRC uses and expose the clause's owise overlap (an aggressive variant
regressed 6 impty symbols YES→MAYBE). Result: no regression; **19 of the 33 p4
MAYBEs flip to YES** (the output + accessor classes, incl. iteration). The
remaining 14 are cause (2) owise + a few multi-clause symbols.

## External tool bridges

`Cocoweb.check` and `Muterm.check` write the serialized system to a temp file
and shell out to a Python client that POSTs to the tool's **web interface**,
mapping the printed token to `Yes | No | Maybe | Timeout | Error of string`.
- Clients: `spectec/tools/cocoweb/cocoweb_client.py`,
  `spectec/tools/muterm/muterm_client.py` (need `python3`, network access).
- Override path via `--client`/`--muterm-client` or env
  `SPECTEC_COCOWEB_CLIENT` / `SPECTEC_MUTERM_CLIENT`.
- `Muterm.check ~solver`: 0 auto (default), 1 polynomials, 2 RPO, 3 dependency
  pairs. Default timeout 30 s. **MuTerm reports termination only**; CoCoWeb
  reports confluence only — `Timeout` is kept distinct from `Maybe`.

`Aprove.check` is different: it runs a **local** `aprove.jar`
(`java -ea -jar … -m wst -t N file.trs`) directly — no web POST, no Python —
because the jar prints the verdict as its first token. Needs `java` on `PATH`
and the jar via `--aprove-jar` / `SPECTEC_APROVE_JAR` / `spectec/tools/aprove/aprove.jar`
(a missing jar is a clean `Error`, not a crash). See
[../../tools/aprove/README.md](../../tools/aprove/README.md).

**Termination dispatch.** `verify`'s termination side goes through
`Termination.check`, which picks the tool per slice by
`Rewrite_system.is_unconditional`: a plain TRS → AProVE (WST mode, the stronger
competition tool for unconditional rewriting), a CTRS → MuTerm. The verify
output labels each row with the deciding tool: `termination(aprove): …` /
`termination(muterm): …`.

## Build & run

**`bin/main.exe`만 빌드하세요 — 전체 빌드는 느립니다.** 저장소에는 p4 파서 등
무거운 타깃이 포함돼 전체(`dune build`)는 오래 걸리는 경우가 많습니다. CTRS
변환을 확인하려면 항상 타깃을 `bin/main.exe`로 한정하세요.

**빌드 전 lock을 확인하세요.** 멈춘 dune 프로세스가 `_build/.lock`을 쥐고 있으면
새 빌드가 무한정 멈춥니다. 빌드가 진행되지 않으면 lock을 잡은 dune이 있는지 먼저
확인하세요:

```bash
cd /home/min/spectec-core/spectec
lsof _build/.lock 2>/dev/null            # lock 보유 프로세스 확인 (있으면 그 dune 종료)
opam exec --switch=spectecx -- dune build bin/main.exe   # 항상 main만 빌드
```

**스위치/바이너리 이름은 이제 `spectecx`입니다.** 프로젝트는 `spectec-core`에서
`spectecx`로 이름이 바뀌었습니다 — 활성 opam 스위치는 `spectecx`이고
(`make`/`SWITCH ?= spectecx`), 구 `spectec-core` 스위치/바이너리는 더 이상
쓰지 않습니다. 빌드 산출물은 `spectec/_build/default/bin/main.exe`이고,
`make exe`(저장소 루트)는 이를 `./spectecx`로 하드링크합니다. 체크인된
바이너리는 소스보다 뒤처지므로(stale) 테스트 전 반드시 재빌드하세요.

Golden test — the `impty/base` analysis CTRS is pinned (the `--ctrs` surface;
the default `rewrite` emits the execution Maude module, not this):

```bash
# from repo root /home/min/spectec-core
spectec/_build/default/bin/main.exe rewrite --ctrs spectec/specs/impty/base/spec.spectec \
  | diff - spectec/specs/impty/base/spec.ctrs   # must match
```

Specs live in `spectec/specs/{impty/{base,closure},p4-old,p4}`; `impty/base` is
the end-to-end reference that fully translates today. Both `p4` and `p4-old`
translate in full (iterations are supported); to dump one pass every file in
sorted order, e.g. `rewrite $(find spectec/specs/p4-old -name '*.spectec' | sort)`.

> **`specs/p4`(새 스펙)는 이제 interp-PASS 스위트(1061개 `p4_16_samples`)를
> 사실상 전부 Maude에서 실행합니다 (2026-06-16).** 전체 스위트 sweep + 직접 재현으로
> 이전의 STUCK이 0으로 떨어졌습니다 (1060/1061 직접 확인, 나머지도 배치 노이즈일 뿐
> 번역 STUCK 아님). 따라서 **이제 진짜 same-spec interp(p4) vs Maude(p4) 오라클이
> 가능합니다.** differential 툴링은 단일 자립형 스크립트
> [`check_diff_p4.sh`](../../../check_diff_p4.sh)로 통합되어 `specs/p4`를 돌립니다
> (구 `find_maude_diverging.sh`/`diff_test.sh`/`diff_review.sh` 대체).
>
> 거기까지 온 여섯 개의 번역 버그 수정 (전부 impty/base 골든 byte-identical):
> 1. **모듈 인코딩**(start-term의 stale 생성자 이름) — FIXED 2026-06-15.
> 2. **positional overload**(`$match_overloaded_unnamed`이 arity 전제
>    `|id_param*| = n_arg`를 defined function `len(..)`로 rule LHS에 fold) —
>    FIXED 2026-06-15. control/function call + top-level instantiation 공통 근원.
> 3. **stale iteration binder**(relation의 두 번째 출력 `# eps`가 빈 스트림으로
>    치환되며 외부 IterPr binder가 stale로 남아 `iter_split`이 입력 스트림으로
>    오분류) — struct/header/enum **선언 자체**를 막던 지배적 블로커. FIXED 2026-06-16
>    ([to_ctrs.ml](translate/to_ctrs.ml) `iter_split`).
> 4. **table 관련**: `$strip_all_whitespace` delegation 누락(키 있는 모든 테이블) +
>    빈 텍스트 `nil` 케이스(식 키 `a&b`/`h.isValid()`) — [to_maude.ml](maude/to_maude.ml);
>    그리고 table `entries`의 loop-invariant `TBLC_2 = ..|tableEntry*|..`가 iterated
>    `TableEntry_ok` 안으로 inline되며 stream을 capture — [simplify.ml](translate/simplify.ml)
>    `subst_prem`/`inline_value_lets` capture 가드. FIXED 2026-06-16.
> 5. **itermap 헬퍼 충돌**(같은 notation을 가진 두 타입 `typeFieldIR`/`fieldTypeIR`의
>    IterE body가 한 `$itermap`으로 collapse) — `typedef struct {..} N`의 subty
>    체크 실패. FIXED 2026-06-16 ([to_ctrs.ml](translate/to_ctrs.ml) `iter_map_sym`에 element
>    타입 추가; impty 골든에 itermap 없어 무영향).
> 6. **refutable shape guard 소실 → ctk 오추론**(Phase D issue447-2/3/4/5-bmv2):
>    `Expr_ok/headerStack-size`의 가드 `typeIR `[ n_size `] = $unroll_typeIR(..)`가
>    equality 전제 `if ($unroll(..) = typeIR[n_size])`로 들어오는데, 출력이
>    `BIT<32> LCTK` 고정이라 `typeIR`/`n_size`가 미사용 → [simplify.ml](translate/simplify.ml)
>    `prem_redundant`의 `dead_var`가 생성자 패턴 `typeIR[n_size]`를 "dead subject"로
>    오판해 그 *refutable shape check* 전제를 통째로 드롭. 그 결과 멤버 이름이 "size"인
>    임의의 헤더 필드 읽기(`hdr.s.size`, DYN)가 header-stack `.size` 절(LCTK)로 새어,
>    번역만 ctk를 LCTK로 과잉정적 추론. FIX: `dead_var`를 실제 subject(bare 변수 /
>    iterated bare 변수)로 한정 — 합성식(생성자 패턴·필드접근·함수호출)은 dead로 안 봄.
>    FIXED 2026-06-18 (impty 골든 byte-identical; 69-file Phase D 표본 회귀 0, 4건
>    MISMATCH→MATCH).

## 회귀/divergence 측정 — same-spec interp(p4) vs Maude(p4) (repo 루트)

이제 인터프리터와 Maude가 **같은 `specs/p4`**를 돌리므로, 모든 divergence는 **순수
번역 버그**입니다(p4-old-vs-new-spec 혼동 없음, per-file triage 불필요). 단일
통합 스크립트로 전체 corpus를 검사합니다:

- **[`check_diff_p4.sh`](../../../check_diff_p4.sh)** — 전체 corpus
  (`p4_16_samples` positive + `p4_16_errors` negative)에 대해 **completeness +
  soundness** 교차표를 만드는 자립형(self-contained) 리뷰. 두 phase 모두
  resumable(progress TSV에 기록, 재실행 시 done 건너뜀), Maude phase는 CHUNK 단위
  배치(거대 모듈을 chunk당 1회만 파싱). 산출물:
  - `check_diff_p4_interp.tsv` / `check_diff_p4_maude.tsv` — 각 엔진의 verdict.
  - `check_diff_p4_completeness.tsv` — **completeness gap** (interp PASS인데 Maude
    not-OK = Maude under-accept = 번역 버그).
  - `check_diff_p4_soundness.tsv` — **soundness gap** (interp FAIL인데 Maude OK =
    Maude over-accept).
  - `check_diff_p4_resultmatch.tsv` — **Phase D: 결과-VALUE 매치** (PASS & OK
    교집합). verdict ∈ {MATCH, MISMATCH, DECODE_ERR, NOCOMP, INTERP_FAIL,
    TIMEOUT}. **MISMATCH는 verdict 오라클이 못 잡는 번역 버그** — Maude가 통과는
    시키지만 *잘못된* 타입 항으로 reduce한 경우다(예: extern method 순서가 뒤바뀜).

> **결과-VALUE 오라클 (Phase D).** PASS/STUCK 판정 일치를 넘어, 두 엔진이 모두
> 받아들이는 프로그램에 대해 **타이핑 결과값 자체**를 비교한다. `run --p4 ...
> --check-p4`가 각 프로그램을 인터프리터로 타입체크해 관계 출력값을 얻고,
> Maude의 reduce된 normal form을 [of_maude.ml](maude/of_maude.ml)로 **IL value로
> 역번역**한 뒤 `Eq.eq_values`로 맞춰 본다. 비교 전 `Of_maude.canonicalize`를
> **양쪽 모두**에 적용해 의미 없는 표현 차이를 제거한다: (1) gensym fresh 이름
> (인터프리터 `FRESH__0` vs 번역 `FRESH'` — 같은 fresh 식별자의 다른 철자)을
> 정규화하고, (2) `map<K,V>`의 엔트리를 키의 `Value.compare`로 정렬한다(맵은
> unordered — 인터프리터는 `VMap.bindings` 정렬, 번역은 삽입 순서라 그냥 비교하면
> 헛-MISMATCH; 정렬은 실제 키/값 차이는 그대로 드러냄). 단일 invocation에 여러 `--p4`를
> 넘기면 거대 모듈 reflection을 한 번만 치르므로 배치로 상각된다. **단,
> `--check-p4`는 인터프리터를 in-process로 돌리므로**(maude `--timeout`은
> 인터프리터에 안 걸림) chunk를 작게 잡고 batch 전체를 바깥 `timeout`으로 묶어
> 멈춘 인터프리터 한 건이 chunk를 물지 않게 한다. `--check-p4` 없는 평소 출력은
> 그대로라 Phase B와 골든에 영향 없다.
- **[`p4_typecheck_suite.txt`](../../../p4_typecheck_suite.txt)** — 참고용
  interp-PASS 스위트(1061 positive `p4_16_samples`). `check_diff_p4.sh`는 이
  스위트가 아니라 **corpus 전체**(`ls SAMPLES` + `ls ERRORS`)를 돌리므로 과거
  스위트가 가졌던 `.exclude` 누락 문제(아래 옛 경고)에서 자유롭습니다.

> **반드시 serial로, clean 환경에서 돌리세요.** `run --p4`는 매 실행마다 ~50k줄
> 모듈을 maude로 파싱하므로 동시 실행(다른 Maude job이나 concurrent `dune build`
> 포함)이 RAM을 고갈시켜 프로세스가 죽고 출력이 깨집니다(빈 출력→오분류). 실측:
> 8코어/16GB에서 `-P 3`도 41건 중 39건이 깨졌고 serial은 54/54 정상. 전체 corpus는
> 오래 걸리니 resume로 나눠 돌리세요(Ctrl-C 후 재실행).

**Divergence triage.** same-spec이므로 `check_diff_p4_completeness.tsv` /
`_soundness.tsv`의 모든 항목은 곧바로 번역 버그입니다 — 바로 bisect하세요
(`Program-ok`에서 sub-goal을 단계별로 `reduce`, 아래 "divergence triage 오라클"
절차). 어떤 파일을 인터프리터로 다시 확인할 때도 **새 스펙 기본 경로**
`main.exe p4 typecheck -p FILE -i INC`를 쓰고, `--spec-dir specs/p4-old`는 쓰지
마세요(새 타깃의 `Builtins`/handler를 유지한 채 스펙 파일만 바꿔 *가짜 interp-FAIL*을
냄 — 과거 p4-old 시절의 오분류 원인).

## 성능 측정 시 주의 — 단순 wall-clock은 Maude 기동 비용에 지배된다

`spectecx run`의 end-to-end 시간을 인터프리터(`impty eval`)와 그대로
비교하면 **결과가 거꾸로 해석될 수 있습니다.** `Maude_run.run`은 실행마다
maude 프로세스를 새로 띄우는데, 빈 입력 기준 기동(prelude 파싱 포함)만
**~24ms**라 작은 테스트에서는 이 상수 비용이 전부입니다. 실측
(impty/base positive 4건, 10회 평균, 2026-06):

- end-to-end: 인터프리터 ~18ms vs Maude ~46ms → "Maude가 2.5배 느림"처럼 보임
- 그러나 Maude 내부 순수 rewriting은 4건 모두 **0ms** (skip 8 / bool_ops 46 /
  ite 73 / loop 473 rewrites). 공통 OCaml 프런트엔드(spec 파싱+elab) ~16ms,
  모듈 emission ~4ms, 나머지가 maude 기동.
- 부하를 키우면(`loop.imp`의 `i <= 10`을 1000/5000으로) 역전이 드러남:
  인터프리터 21.5초/18분47초(초선형 폭증) vs Maude 순수 rewriting 8ms/48ms
  (~3–4M rewrites/sec, 거의 선형).

기동 비용을 빼거나 줄이는 방법:

1. **Maude 시간을 phase별로 분해 — 스크립트로 자동화돼 있음:
   [`tools/maude/rewrite-time.sh`](../../tools/maude/rewrite-time.sh).** maude 호출
   비용 = **기동(prelude) + 모듈 파싱 + start-term 파싱 + rewriting**. 이 스크립트는
   `--maude-bin` 래퍼로 `Maude_run.run`이 넘기는 self-contained 임시 파일(모듈+명령+
   `quit`)을 복사해 두고, 세 변형 — `quit`만(기동), 명령 잘라낸 모듈만(기동+모듈
   파싱), 캡처 파일 전체 — 의 maude 프로세스 wall을 재서(`-n REPS` 최소) 각 phase로
   분해합니다. rewriting은 maude 자신의 `rewrites: N in Xms cpu (Yms real)` 줄에서
   읽습니다(`parse_output`이 버리는 그 줄). 사용법:

   ```bash
   # repo 루트에서. `--` 뒤는 평소 쓰던 run 명령을 그대로 (스크립트가
   # --maude-bin 래퍼와 넉넉한 --timeout만 덧붙임). -n REPS(기본 3)는 변형별로
   # N회 돌려 최소 wall을 보고(순수 비용에 가장 근접).
   SPEC=$(find spectec/specs/p4 -name '*.spectec' | sort | tr '\n' ' ')
   spectec/tools/maude/rewrite-time.sh -n 3 -- \
     spectec/_build/default/bin/main.exe run \
     --p4 spectec/testdata/interp/p4/p4c/p4_16_samples/tuple3.p4 \
     -i spectec/testdata/interp/p4/p4c/includes $SPEC
   ```

   **실측(2026-06-16, tuple3.p4)으로 드러난 핵심:** 작은 P4 프로그램의 end-to-end
   ~10s 중 maude 안에서 지배적인 건 모듈 파싱(~0.4s)도 rewriting(0ms, 316 rewrites)도
   아니라 **start-term 파싱 ~6.9s** 다 — 거대 mixfix 연산자 문법으로 시작항(`reduce
   Program-ok(..)`의 인자)을 파싱하는 비용. 기동은 ~30ms. 즉 "거대 모듈 파싱 비용"의
   실체는 모듈이 아니라 **그 모듈 문법으로 항을 파싱하는 것**이다. 부하를 키운
   `impty` loop(`i <= 2000`)은 74k rewrites / 24ms cpu로 rewriting이 분명히 잡힌다.
   인터프리터 쪽 baseline은 `impty parse -p FILE`(스펙 로드+프로그램 파싱만, ~16ms)을
   빼면 된다.

   > **start-term 파싱 병목은 meta-level(reflection) 전환으로 제거됨 (2026-06-17).**
   > 시작항을 거대 mixfix object 문법으로 파싱하는 대신, **고정·소형 META-TERM
   > 문법**으로 적어 `metaReduce(upModule('SPEC, false), <meta-항>)`로 돌린다
   > ([to_maude.ml](maude/to_maude.ml) `print_meta_term`/`meta_term_of_value`/
   > `meta_start_app`가 object `foo(a,b)`를 meta `'foo[a, b]`로, 0-arity 상수를
   > `'foo.Sort`로 인코딩; 내장 스칼라는 Maude가 reflect하는 형태 그대로 —
   > nat는 `'nat['s_^N['0.Zero]]` (plain `'N.Nat`는 metaReduce에서 파싱 안 됨),
   > 음수 int는 `'-_[..]`, bool/txt는 `'true.Bool`/`'"..".String`).
   > [maude_run.ml](maude/maude_run.ml)은 emit된 `mod SPEC` 뒤에 `META-LEVEL`을 import한
   > 작은 wrapper 모듈(`SPECTEC-META-RUN`)을 붙이고, 결과를
   > `downTerm(getTerm(metaReduce(..)), $downerr)`로 **object 항으로 되돌려** 기존
   > `result <Sort>:`/stuck-head 파싱·출력을 그대로 재사용한다(텍스트 재파싱 없음).
   > `upModule`은 `op $specmod : -> Module [memo]`에 바인딩해 invocation당 1회만.
   >
   > **실측 before/after (tuple3.p4, 같은 p4 코퍼스 모듈 50507줄):**
   > - **object**: start-term 파싱 **~6.9s/프로그램** (모듈 파싱 ~0.4s, rewriting 0ms).
   > - **meta**: start-term 파싱 사실상 **0** (META-TERM 문법). 대신 첫 `metaReduce`가
   >   reflect된 50k줄 모듈을 **내부화하는 1회 비용 ~10.4s**(maude stats상 22844
   >   rewrites)를 치르고, **이후 같은 invocation의 모든 프로그램은 ~4ms**
   >   (Maude가 metamodule을 캐시 — 확인: 같은 큰 항 6회 반복 시 1회차 10.4s, 2~6회차
   >   각 4ms / 358 rewrites). 즉 6.9s/프로그램 → 10.4s/invocation 고정비 + 4ms/프로그램.
   > - 배치 효과: 80 OK + 80 STUCK = 160건이 단일 invocation 80s(=0.5s/건), verdict는
   >   object-level 기록(`check_diff_p4_maude.tsv`)과 **160/160 일치**. 무거운 예
   >   key-bmv2/issue561-bmv2도 OK 동일. impty/base `spec.ctrs` 골든 byte-identical
   >   (분석 파이프라인 무관). `check_diff_p4.sh`는 출력 형식이 동일해 수정 불필요.
   >
   > 주의: `rewrite-time.sh`의 phase 분해는 object 기준이라 meta에서는 "rewriting"
   > 항목에 metamodule 내부화 1회 비용이 섞여 들어간다(위 첫 `metaReduce` 10.4s).
   > 캐싱 여부는 같은 항을 여러 번 reduce해 2회차부터의 stats로 본다.
2. **배치 실행으로 상각 (구현됨).** 모듈 텍스트는 입력 프로그램과 무관하게
   동일하므로, 모듈 1개 + `reduce` 명령 N개를 한 파일에 이어붙이면 기동(+거대
   모듈 파싱)을 1회만 치릅니다. `Maude_run.run_batch`가 이 경로입니다: start term
   리스트를 받아 **maude를 1회** 실행하고, 각 명령 뒤에 센티넬
   `reduce "$$SPECTEC_BATCH_SEP$$" .`을 끼워 출력을 프로그램별 세그먼트로 잘라
   순서대로 파싱합니다(결과 리스트는 입력과 위치 대응). CLI에서는 `run`의
   `--p4`/`--imp`를 **여러 번** 지정하면 됩니다 — 스펙 로드·모듈 emission·maude
   파싱이 전부 1회로 상각됩니다 (실측: p4-old 3건 33.6s→14.7s). 단일 입력은
   기존 출력(`result:`/`FAIL (stuck):` 한 줄)을 그대로 유지하고, 다중 입력은
   `=== <파일> ===` 블록으로 프로그램별 결과를 라벨링합니다. **주의:** `--timeout`은
   배치 *전체*에 걸리므로(프로그램별 아님), 안 끝나는 한 건이 배치 전체를 물고
   타임아웃나면 그 배치의 모든 결과를 잃습니다 — 무거운 스위트는 적당히 끊어
   배치하세요.
3. **벤치마크를 키우기.** 실행 시간이 기동 비용을 압도하도록 입력 규모를
   키우면 end-to-end 비교도 유효해집니다.

장기적으로는 `Maude_run`이 통계 줄을 파싱해 rewrites 수와 cpu/real ms를
결과에 실어 주는 것(`--stats` 류)이 깔끔합니다. 인터프리터의 초선형 거동은
별도 조사 대상입니다.

## 코드 작업을 끝낸 뒤 (마무리 절차)

코드 변경이 끝나면 **반드시 다음을 순서대로** 진행하세요:

1. **`make fmt`** (= `dune fmt`) — 커밋 전 포맷. (저장소 루트에서 실행, 또는
   `cd spectec && opam exec --switch=spectecx -- dune fmt`.)
2. **[CONTRIBUTING.md](../../../CONTRIBUTING.md)를 참고한 리팩토링** — 동작이
   끝난 코드를 컨벤션에 맞춰 다듬습니다. 특히 이 라이브러리에서 자주 닿는 규칙:
   - **이름은 스펙의 일부.** 메커니즘이 아니라 *책임*을 전달하는 이름을 쓰고,
     같은 개념의 기존 사용처와 대조해 맞춥니다. 리네임 시 모든 사용처를 함께
     쓸어 고칩니다 (`sweep all usage sites`).
   - **리팩토링 중 하위호환 alias 금지.** 리네임은 끝까지 완료 — 과도기 이름을
     남기지 않습니다.
   - **`lib/` ↔ `bin/` 경계.** 재사용 로직은 `lib/`(CLI 인프라는 `lib/cli/`)에,
     `bin/`에는 최상위 엔트리포인트만. 새 로직은 `bin/`이 아니라 `lib/`로.
   - **일회용 메타패턴 도입 금지** — 한 곳만 쓰는 헬퍼보다 작은 지역 중복이 낫습니다.
   - **영리한 추상화보다 직접적인 코드**, 특히 예외 처리가 얽힐 때.
   - **mutable ref보다 작은 지역 재귀/fold**로 제어 흐름을 읽기 쉽게.
   - `with_*`는 setup/teardown 콜백 래퍼에만; 누산기성 헬퍼는 `fold_left`식
     인자 순서로. `@@`는 단일 콜백 들여쓰기를 줄일 때만.
   - 리팩토링 커밋은 fix/feature와 **분리**합니다 (bisectability).
3. 변환 출력이 바뀌었다면 golden(`impty/base/spec.ctrs`)을 갱신하고
   (위 diff 명령으로 의도된 변화인지 확인), 필요하면 `make promote`로 `.expected`
   재생성.

## Known gaps — read [todo.md](todo.md) before extending

Priority-ordered. The big ones:
- **Iterations done.** `IterE`/`IterPr` compile to recursive helpers
  (`$itermap`/`$unzip`/`$iterall`/`$itercollect`); `Simplify.collapse_rezip_iters`
  pre-folds unzip→re-zip round-trips. Remaining nits: helper symbols can be very
  long; the captured-body `$unzip` is non-left-linear (see [todo.md](todo.md)).
- **Iteration-binder-scope discipline (Fix A, 2026-06-13).** `Simplify.subst_prem`'s
  `IterPr` branch now threads `elem_bound` (every element var bound by some iteration
  in the block, via `iter_binders_prem`) and withholds any pair whose `to_e` drags in
  an element var bound by ANOTHER iteration — the premise-position analog of
  `Prem_env.subst_exp`'s `binds_from` guard. This fixed the table action-enum
  STREAM-vs-element bug end-to-end (`cases`/`apply-cf`/`default-switch`/`exit5` all
  flip STUCK→OK; impty goldens byte-identical).
- **`$find_overloaded` named-argument `:=`-helper (P1-(b), FIXED 2026-06-14).**
  All-named overloaded calls (`a(x = .., y = ..)`) no longer stuck/loop. Two
  `Simplify` steps: (1) `subst_prem`'s `IfPr` equality case folds structure into an
  opaque-**call** operand (`if $f(..) = rhs`) while protecting the equality's own
  operands, so the reconstructed head `?(id_arg')*` reaches `$find_matchings`; (2)
  `drop_confined_rebinding` removes the orphaned some-extraction `(let ?(id_arg') =
  id?)*` (rebinds a head-bound var from a confined link) and a new `IterPr(LetPr …)`
  arm in `prem_redundant` cascades the now-dead producer `(let id? = id_arg?)*` away
  — killing the circular `$itercollect … := …` helpers. impty goldens byte-identical;
  see [todo.md](todo.md).
- **P2 `otherwise` (`ElsePr`) is dropped** (`conds_of_prem` → `[]`), so a
  fallthrough clause loses its "no earlier clause applied" guard and overlaps the
  earlier rules → **non-confluent** (e.g. `impty/base` `$lookup` emits two rules
  with the same LHS; at `K_h == K` both fire). Fix = translate `otherwise` as the
  negation of the preceding clauses' guards; clean for equality/`matches`
  guards, but a relation-premise guard hits the negation wall (relations are
  value-returning, never `-> false`, failure = stuck) — same problem as
  `NeOp`/`IfNotHoldPr`, which are approximated as `== false`. **The Maude
  surface now totalizes negated no-output judgments** with guarded `owise`
  complements (`To_maude`), so `IfNotHoldPr` is decidable in execution; the
  CTRS/COPS side still approximates. See [todo.md](todo.md).
- **Casts & subtype done.** Non-numeric casts are transparent (faithful);
  nat↔int casts are alias/tuple-resolved in `Simplify`. Unary minus injects
  `int_pos` once at the magnitude leaf: `term_of_unop` skips the injection when
  its operand already denotes a signed int (`yields_int`), so nested `-(-n)` no
  longer double-injects; the prelude's `negate_int(negate_int(x)) -> x` cancels
  the residual double negation. `SubE` is the structural
  `sub_pred` (`sub_nat`/`subty_<T>`/`subty_tup`/`subty_list`/`subty_opt`,
  recursing into payloads); positive-use only — `-> false` totality for
  non-member cases is deferred to the negation story above.
- **Gensym done.** `$fresh_typeId`/`$fresh_tid` is modeled by automatic state
  threading ([gensym.ml](translate/gensym.ml)): state = last issued name, issuing
  appends a prime (`FRESH'`, `FRESH''`, … — no collision with P4 identifiers
  or each other); every fresh-reaching symbol gains a trailing state argument,
  `To_maude.start_app` appends the `txt("FRESH")` seed at the start term, and
  the run normalizes to `tuple(result, final-state)`. `Prem_env` keeps
  fresh-reaching calls opaque so `Simplify` never duplicates an issuance.
- **`DefA` args done.** Defunctionalized by call-site specialization
  ([defunctionalize.ml](translate/defunctionalize.ml), first pass in
  `Pipeline.ctrs_of_spec`): `$f(args, def $g)` → a generated first-order copy
  `$f_$g` with `$check := $g` substituted through the clauses (worklist
  closure, templates removed, leftover `DefA` is a hard error). This + the
  Maude-surface work it enabled (defunctionalized signature recovery,
  threaded-negation owise complement, `subty_*` owise totalization,
  first-argument-dispatch connective delegations, declaring-origin start-term
  encoding) make **specs/p4 (not just p4-old) executable**: tuple3.p4 and
  saturating `|+|` typecheck end-to-end in Maude.
- **Iterated option-equality premises done** (`$find_overloaded`'s
  `(id_arg? = eps)*` family): `Prem_env.subst_exp` now refuses a substitution
  whose `relift_vars` would EMPTY an iteration's binder list (folding the
  constant `?()` into the head's depth-2 body left the degenerate `?()*{}`,
  orphaning/dropping the premises), and `collapse_rezip_iters` skips
  variable-free bodies (its vanish gate is vacuous there). Overload
  resolution now reduces on both specs — p4-old `f(8w0)` resolves through
  `RoutineType_ok` to the specialized extern type.
- **`$align_parameters` capture done** (`Call_ok` no longer sticks): the
  per-step element rename now runs at the IL level capture-aware
  (`rename_step_exp`/`rename_step_prem`) so a structured nested `IterE`
  re-binding a co-iterated var keeps its full stream, and `iter_captured`
  passes that full stream to the helper as a constant alongside the consumed
  spine. **p4-old `f(8w0)` type-checks end-to-end.**
- **P3 remaining** sanitizer same-atom/same-arity collisions; relation-vs-
  prelude namespace; unary `term_of_num`. **The new spec's call/instantiation
  execution frontier is FIXED** (2026-06-15: `$match_overloaded_unnamed` folded
  the arity premise `|id_param*| = n_arg` into the rule LHS as `len(..)`, an
  unmatchable pattern — every positional overloaded call + `top(c()) main`
  stuck; see [todo.md](todo.md) P0 "Done"). `specs/p4` still sticks on OTHER
  frontiers (member access/`Eval_static`, BuiltinDecD stragglers, `for`-loops),
  so Maude tooling stays on p4-old for now. Still needs a pinned `test/` beyond
  the `impty/base` golden.
