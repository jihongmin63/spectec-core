# `rewrite` library — IL spec → conditional term rewriting system (CTRS)

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
| [maude_theory.ml](maude_theory.ml) / [.mli](maude_theory.mli) | The native-theory pass: fold ground scalars into `nat`/`int`/`bool`/`txt` wrappers, drop the replaced scalar prelude rules. Also owns the wrapper symbol spelling and the literal builders the encoder uses. |
| [rewrite_system.ml](rewrite_system.ml) | **Data model + printer** for a CTRS (`type t`, `term`, `rule`, `cond`). Two printers: `string_of_system` (COPS, for CoCoWeb) and `string_of_system_tpdb` (TPDB, for MuTerm **and** AProVE — an unconditional system prints as plain WST). `is_unconditional` (the AProVE/MuTerm dispatch predicate), `slice`/`reachable_heads`. No translation logic here. |
| [to_ctrs.ml](to_ctrs.ml) / [.mli](to_ctrs.mli) | **The translation.** IL → CTRS terms/rules: the symbol-naming conventions, the fixed `prelude`, type-derived rules, and rule/clause bodies. The heart of the library. |
| [simplify.ml](simplify.ml) / [.mli](simplify.mli) | Pre-pass over IL: expand variables into concrete structure (via `Prem_env`) and drop redundant premises. Runs **before** `to_ctrs`. |
| [prem_env.ml](prem_env.ml) / [.mli](prem_env.mli) | Union-find over IL expressions built from a rule/clause's premises; gives each expression its canonical (most specific) member. Consumed by `Simplify`. |
| [exp_map.ml](exp_map.ml) / [.mli](exp_map.mli) | Shallow one-level traversal helpers over IL: `map_subexps` / `subexps` / `map_path_exps` over expressions, `exps_of_prem` for the expressions a premise embeds (caller controls descent). |
| [builtin.ml](builtin.ml) / [.mli](builtin.mli) | Backend-local CTRS rules for P4's collection builtins (map/set/list/text) that `BuiltinDecD` declares but `To_ctrs` emits no rules for; fed to `of_spec` as `extra_defs`. |
| [gensym.ml](gensym.ml) / [.mli](gensym.mli) | Make the stateful gensym (`$fresh_typeId`/p4-old `$fresh_tid`) pure by state threading: every fresh-reaching symbol gains a trailing state argument and a `tuple(result, state')` result; issuing appends a prime to the last issued name (seed `"FRESH"` → `FRESH'`, `FRESH''`, …). Runs last in `ctrs_of_spec`; identity on gensym-free specs (impty golden untouched). |
| [defunctionalize.ml](defunctionalize.ml) / [.mli](defunctionalize.mli) | Specialize away `def`-valued arguments (`DefP`/`DefA`): each call `$f(args, def $g)` → a generated first-order copy `$f_$g` with `$check := $g` substituted through the template's clauses (worklist closure over recursion/chained templates; templates removed; no `DefA` may survive). Runs FIRST in `ctrs_of_spec`; identity without `DefP` (impty). |
| [to_maude.ml](to_maude.ml) / [.mli](to_maude.mli) | **Maude backend**: emit the native-theory system as an executable order-sorted Maude module (sort recovery, op declarations, eq/rl printing, the built-in delegation equations, start-term encoding). |
| [maude_run.ml](maude_run.ml) / [.mli](maude_run.mli) | Execution bridge: run an emitted module on a start term with a local `maude` binary (`reduce`/`rewrite`/`search`), parse the normal form, flag stuck heads. `run` does one start; `run_batch` runs a list of starts in **one** Maude invocation (sentinel-delimited per-start output) so the ~50k-line module is parsed once for the whole batch. |
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

## Symbol-naming conventions ([to_ctrs.ml](to_ctrs.ml), "Symbol + builder layer")

These **must agree** between the rule that *defines* a symbol and every rule
that *uses* it. All raw `R.App`/`R.Var` construction is confined to this layer.

- `sanitize` — scrub a string to a CTRS-safe id (`[A-Za-z0-9]` runs kept, other
  chars become mnemonics, e.g. `->` → `minus_gt`, `&&` → `amp_amp`).
- **Arity is folded into variant/case symbols** (`variant_<origin>_<atoms>_<n>`)
  to remove same-atom different-arity clashes. Same-atom **same-arity** clashes
  are still possible (see [todo.md](todo.md) P3).
- `func_sym id` = `$` + sanitize (functions/`DecD`), `rel_sym id` = sanitize
  (relations/`RelD`). Constructors: `variant_sym`, `struct_sym`, `field_sym`,
  `match_sym`, `subty_sym`.
- Numbers: Peano `zero`/`succ` for nats; sign-magnitude `int_pos`/`int_neg` over
  nat magnitudes for ints. Lists: `nil`/`cons`. Options: `none`/`some`. Text
  chars: nullary `chr_<code>`.

## The prelude ([to_ctrs.ml](to_ctrs.ml) `prelude`)

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

- `rewrite [--symbol NAME] FILES…` — dump the CTRS (`string_of_system`); with
  `--symbol`, only that symbol's dependency slice.
- `verify [--only confluence|termination|both] [--whole] [--symbol NAME]
  [--list-symbols] [--timeout S] [--solver N] [--jobs N] [--client P]
  [--muterm-client P] [--aprove-jar P] FILES…` — run CoCoWeb and/or
  termination (AProVE/MuTerm dispatched per slice). **Default is
  per-symbol slices** (batch over every root, `--jobs` concurrent, default 4);
  `--whole` checks the whole system at once. Shared driver:
  [lib/cli/slice_check.ml](../cli/slice_check.ml); flags:
  [lib/cli/cli_args.ml](../cli/cli_args.ml) `Slice`.

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

Golden test — the `impty/base` CTRS is pinned:

```bash
# from repo root /home/min/spectec-core
spectec/_build/default/bin/main.exe rewrite spectec/specs/impty/base/spec.spectec \
  | diff - spectec/specs/impty/base/spec.rewrite   # must match
```

Specs live in `spectec/specs/{impty/{base,closure},p4-old,p4}`; `impty/base` is
the end-to-end reference that fully translates today. Both `p4` and `p4-old`
translate in full (iterations are supported); to dump one pass every file in
sorted order, e.g. `rewrite $(find spectec/specs/p4-old -name '*.spectec' | sort)`.

> **실행은 `p4-old` 스펙에 집중하세요 — `p4`(새 스펙)는 실행 프론티어 이슈가
> 있습니다.** `specs/p4`는 control apply body의 generic call(`f(8w0)`)이 새
> 제약-기반 추론 경로(`CallableType_ok`→`Call_ok`/`$infer`)에서 Maude `FAIL
> (stuck)` 납니다(원인 미확정, [todo.md](todo.md) 참조). **동일 프로그램은
> `specs/p4-old`에서 end-to-end로 동작**하므로(`RoutineType_ok` 경로), P4
> 실행/회귀 확인은 `p4-old`로 하세요. `p4`는 번역(`rewrite` 덤프)까지만
> 신뢰합니다.

## 회귀/divergence 측정 — 인터프리터 오라클 + Maude 백엔드 (repo 루트)

p4-old 실행 커버리지는 **레퍼런스 인터프리터가 받아들이는 valid 프로그램**을
기준으로 잽니다. 두 산출물이 repo 루트에 있습니다:

- **[`p4_typecheck_suite.txt`](../../../p4_typecheck_suite.txt)** — 인터프리터가
  통과시키는 1061개 positive `p4_16_samples`. `main.exe p4 batch -v`(새 스펙)로
  생성: positive 1061/1061 PASS, negative(`p4_16_errors`) 253/253 reject, 197개는
  인터프리터 exclude. 잘려있던 `p4old_samples_results.tsv`(748) baseline을 대체.
  재생성: `main.exe p4 batch -v 2>&1 | grep 'p4_16_samples.*\.\.\. pass'`.
- **[`find_maude_diverging.sh`](../../../find_maude_diverging.sh)** — suite를
  p4-old Maude(`run --p4`)로 돌려 **OK가 아닌 것(STUCK/TIMEOUT/ERROR)만** 추림 =
  "Maude가 잘못한" 후보(인터프리터는 받는데 Maude는 못 돌림). resumable
  (progress TSV에 기록, 재실행 시 done 건너뜀), 출력은 `maude_diverging.tsv`.

> **반드시 serial(JOBS=1, 기본)로 돌리세요.** `run --p4`는 매 실행마다 ~50k줄
> 모듈을 maude로 파싱하므로 동시 실행이 RAM을 고갈시켜 프로세스가 죽고 출력이
> 깨집니다(빈 출력→오분류). 실측: 8코어/16GB에서 `-P 3`도 41건 중 39건이 깨졌고,
> serial은 54/54 정상. 1061개 serial은 오래 걸리니 resume로 나눠 돌리세요.

**Divergence triage — 올바른 오라클을 쓰세요.** divergence(인터프리터 PASS +
Maude not-OK)가 *번역 버그*인지 *진짜 p4-old 스펙 구멍*인지는 그 파일을 인터프리터로
다시 돌려 가립니다 — **반드시 새 스펙 기본 경로** `main.exe p4 typecheck -p FILE -i
INC` 로. **`--spec-dir specs/p4-old`는 쓰지 마세요**: `p4 batch`/`typecheck` CLI가
새 타깃의 `Builtins`/handler를 유지한 채 스펙 파일만 바꿔서 *가짜 interp-FAIL*을
냅니다(예: `issue3623-1`·`bool_to_bit_cast`는 `--spec-dir p4-old`에선 `error`지만
새 스펙 인터프리터는 PASS, Maude도 수정 후 OK — 둘 다 번역 버그였음). 과거
"128 STUCK 전부 interp-FAIL → 번역 버그 0개" 결론은 이 깨진 오라클 탓에 틀렸습니다
([todo.md](todo.md) 참조).

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

1. **Maude 자체 통계를 읽기 (가장 간단) — 스크립트로 자동화돼 있음:
   [`tools/maude/rewrite-time.sh`](../../tools/maude/rewrite-time.sh).** `reduce`는
   `rewrites: N in Xms cpu (Yms real)`를 항상 출력하지만 `parse_output`이
   `result` 줄만 보고 버립니다. 이 스크립트는 `--maude-bin`에 래퍼를 끼워
   `Maude_run.run`이 maude에 넘기는 self-contained 임시 파일(모듈+명령+`quit`)을
   복사해 두고, 그 파일로 maude를 직접 다시 돌려 통계 줄을 파싱합니다 — **순수
   rewriting의 cpu/real ms와 rewrites 수만** 보고하고, 참고로 end-to-end
   wall-clock도 함께 찍습니다. 사용법:

   ```bash
   # repo 루트에서. `--` 뒤는 평소 쓰던 run 명령을 그대로 (스크립트가
   # --maude-bin 래퍼와 넉넉한 --timeout만 덧붙임). -n REPS는 캡처한 모듈을
   # N회 재실행해 최소 cpu/real을 보고(순수 비용에 가장 근접).
   SPEC=$(find spectec/specs/p4 -name '*.spectec' | sort | tr '\n' ' ')
   spectec/tools/maude/rewrite-time.sh -n 3 -- \
     spectec/_build/default/bin/main.exe run \
     --p4 spectec/testdata/interp/p4/p4c/p4_16_samples/tuple3.p4 \
     -i spectec/testdata/interp/p4/p4c/includes $SPEC
   # -> end-to-end ~12s, 그러나 pure-cpu 0ms (303 rewrites)
   ```

   실측(2026-06): p4 선언 샘플들(tuple3/intType/octal/cast_noop)은 모두
   순수 rewriting **0ms**(303~1157 rewrites)인데 end-to-end는 11~19s —
   전부 P4 스펙 emission + maude의 거대 모듈 파싱 비용입니다. 부하를 키운
   `impty` loop(`i <= 2000`)은 74k rewrites / 24ms cpu vs end-to-end 121ms로
   순수 시간이 분명히 잡힙니다(스크립트 검증). 인터프리터 쪽 baseline은
   `impty parse -p FILE`(스펙 로드+프로그램 파싱만, ~16ms)을 빼면 됩니다.
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
3. 변환 출력이 바뀌었다면 golden(`impty/base/spec.rewrite`)을 갱신하고
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
  threading ([gensym.ml](gensym.ml)): state = last issued name, issuing
  appends a prime (`FRESH'`, `FRESH''`, … — no collision with P4 identifiers
  or each other); every fresh-reaching symbol gains a trailing state argument,
  `To_maude.start_app` appends the `txt("FRESH")` seed at the start term, and
  the run normalizes to `tuple(result, final-state)`. `Prem_env` keeps
  fresh-reaching calls opaque so `Simplify` never duplicates an issuance.
- **`DefA` args done.** Defunctionalized by call-site specialization
  ([defunctionalize.ml](defunctionalize.ml), first pass in
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
  prelude namespace; unary `term_of_num`; **the execution frontier is now the
  new spec's control-body generic call** (`specs/p4` `f(8w0)` sticks at the
  control `Decl_ok`; p4-old's runs) — a distinct call path
  (`$find_callableDef_overloaded_t`/`CallableType_ok`) needing its own
  bisection. See [todo.md](todo.md). Still needs a pinned `test/` beyond the
  `impty/base` golden.
