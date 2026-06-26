# `rewrite` 재구현 TODO (new-rewrite)

골격은 클린 빌드되지만 모든 변환이 `failwith` 스텁입니다. 알고리즘 설계 기준은
[CORE_LOGIC.md](CORE_LOGIC.md), 모듈 상태는 [CLAUDE.md](CLAUDE.md) 참고. 이 문서는
*무엇을 어떤 순서로 채우는가*만 추적합니다.

## 현재 상태

- ✅ 빌드됨·온전: `Rewrite_system`(`{vars;rules}` + `string_of_term`·질의·`slice` +
  `string_of_system_maude`/`ops_of_system`), `To_ctrs` 심볼/빌더 레이어 + thin 질의
  (`def_symbols`·`input_moded_rel_syms`·`rule_head_syms`), `pipeline`/`rewrite`,
  `Mfe`(CRC+ChC 브리지), **CLI 배선**(`bin/main.ml` `rewrite`/`verify`/`run` +
  `bin/dune`에 `spectec.rewrite`).
- ✅ 복구됨(지원 패스, `rewrite` 브랜치에서): `exp_map`(IL 얕은 traversal),
  `defunctionalize`(def-인자 specialization), `gensym`(`$fresh` 상태 스레딩),
  `builtin`(P4 컬렉션 builtin 규칙). `pipeline`이 두 경로(Structural/Native)에서
  공통 `build`로 wrap: Defunctionalize FIRST → `of_spec ~extra_defs:Builtin` →
  Gensym LAST. impty엔 전부 identity(골든 무영향). 단, `gensym`은
  `Prem_env.gensym_ids` 대신 자체 `gensym_ids`를 가짐(Prem_env 미복구) — Simplify/
  Prem_env 복구 시 이 목록을 opaque로 유지해야 함.
- 🔴 스텁: `To_ctrs.of_spec`·`var_type_hints`, `Simplify.simplify_spec`, `To_maude.*`.
  (지원 패스는 컴파일·배선됐지만 `of_spec`/`Simplify` 스텁에 막혀 런타임은 아직
  `failwith` — M1/M2에서 풀림.)
- ⛔ 삭제됨(로직은 CORE_LOGIC.md §4–§6): `prem_env`·`maude_theory`·`maude_run`·
  `of_maude`·`cocoweb`·`muterm`·`aprove`·`termination`. (COPS/TPDB 표면도 제거 —
  분석 confluence는 MFE 한 경로.)

## 파이프라인

```
Lang.Il.spec → Simplify → To_ctrs.of_spec ~scalars:(Structural|Native)
  ├ Structural → string_of_system_maude → Mfe (CRC + ChC)   ← 분석
  └ Native     → To_maude → maude 실행                        ← 실행
```

## 가로지르는 작업

- [x] **CLI 최소 배선** (done) — `bin/main.ml`에 `spectec.rewrite`를 link하고
  `Rewrite.` 파사드로 세 서브커맨드 연결:
  - `rewrite` — 기본은 **실행 모듈**(`To_maude.module_of_spec`); `--ctrs`=분석 CTRS
    덤프(`string_of_system_maude ~rule_heads`, verify가 MFE에 넘기는 텍스트),
    `--simplified`=Simplify 전처리 덤프, `--relations-as-rules`.
  - `verify` — MFE `Mfe.check`(CRC+ChC), `--symbol` 슬라이스·`--list-symbols`·
    `--timeout`/`--maude-bin`/`--mfe-dir`; 둘 다 YES 아니면 exit 1.
  - `run` — 현재 **모듈 emit만**(실행/`--imp`/`--p4`/`--check-p4`는 M2: `Maude_run`·
    `Of_maude`·targets `maude_start_term` 복귀 시).
  `Mfe.check`/maude 표면이 받는 `rule_heads`(= 비입력-moded relation)는
  `To_ctrs.rule_head_syms`로 계산(`input_moded_rel_syms`의 여집합). 세 커맨드 모두
  컴파일·dispatch 확인 — 호출 시 각 백엔드 스텁(M1 `of_spec`/`Simplify`, M2
  `To_maude`)의 `failwith`에 도달하며, 스텁을 채우면 즉시 동작.

## M1 — 분석 동작 (CTRS 생성 + confluence)

- [x] **`Exp_map` 재생성** (done) — IL 얕은 traversal(`map_subexps`/`subexps`/
  `exps_of_prem`). `Defunctionalize`가 사용.
- [ ] **`Prem_env` 재생성** — 전제 union-find, 각 식의 canonical(최구체) member. Simplify 엔진.
  복구 시 `Gensym.gensym_ids`(현재 gensym 내부)를 opaque로 유지.
- [ ] **`Simplify.simplify_spec`** — 변수 전개, `matches`/필드접근을 head 패턴으로 fold,
  value/let inline, subtype→cast, 잉여 전제 제거. **capture-awareness가 핵심 난점**
  (CORE_LOGIC §3.7·§4).
- [ ] **`To_ctrs.of_spec`** (심장) — `prelude` + `defs_of_typ` + `term_of_exp` +
  `conds_of_prem` + `rules_of_def` + 반복/subtype 헬퍼 + `prune_unused`. **`~scalars`**:
  `Structural`=Peano 등 + prelude / `Native`=ground 스칼라 wrapper(`nat`/`int`/`bool`/`txt`)
  + `native_replaced_heads` 생략.
- [ ] **`To_ctrs.var_type_hints`** — `VarE` note에서 변수 narrow 타입 복원(To_maude용).
- [ ] **검증: impty/base CTRS 골든** byte-identical (`rewrite --ctrs` 덤프 ↔
  `spec.rewrite`). 기본 `rewrite`는 실행 모듈을 내므로 분석-CTRS 골든은 `--ctrs` 경로.
- [ ] **`Mfe` calibration** — 실제 MFE로 load 파일명·`(check ...)` 문법·verdict 토큰 보정
  (`mfe.ml` 상수/파서). impty/base structural 시스템에 CRC+ChC 적용.
- [ ] `to_ctrs.ml` 상단 `[@@@warning "-32-69"]` 제거(빌더 레이어가 다시 쓰이면).

## M2 — 실행 (Maude)

- [ ] **`To_maude.*`** — Native 시스템 → order-sorted Maude 모듈(sort 복구·op 선언·eq/rl·
  내장 delegation eq·input-moded relation을 eq로) + META-TERM 인코더
  (`meta_term_of_value`/`meta_start_app`).
- [ ] **`Maude_run` 재생성** — 로컬 maude 구동·`metaReduce`·normal form 파싱·stuck 판정·
  `run_batch`(모듈 내부화 상각).
- [x] **`Defunctionalize` + `Gensym` + `Builtin` 재생성 + 파이프라인 재배선** (done) —
  p4용(def-인자 specialization, `$fresh` 상태 스레딩, 컬렉션 builtin 규칙). 두 경로
  공통 `Pipeline.build`로 wrap(Defunctionalize FIRST / `of_spec ~extra_defs:Builtin`
  / Gensym LAST). impty/base엔 identity. `of_spec`/`Simplify` 스텁이 채워지면 즉시
  효과 발생.

## M3 — 결과-VALUE 오라클

- [ ] **`Of_maude` 재생성** — Maude normal form → IL value 역번역 + `canonicalize`
  (gensym 이름·map 정렬 정규화).
- [ ] **differential** — same-spec interp(p4) vs Maude(p4) 결과값 비교.

## 권장 순서

(이미 복구: CLI 배선, `Exp_map`, `Defunctionalize`/`Gensym`/`Builtin` + 파이프라인 배선.)

```
  → Prem_env → Simplify → To_ctrs.of_spec(Structural)             [M1: impty/base 골든]
  → Mfe calibration
  → of_spec(Native) → To_maude → Maude_run                         [M2: 실행]
  → p4(def-인자/gensym/builtin 활성) → Of_maude                    [M3: 오라클]
```
