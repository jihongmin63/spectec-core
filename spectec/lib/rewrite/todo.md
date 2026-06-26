# `rewrite` 재구현 TODO (new-rewrite)

골격은 클린 빌드됩니다. 남은 `failwith` 스텁은 **`To_ctrs`의 `of_spec`·`var_type_hints`
둘뿐**입니다(= 당신이 채울 핵심). 보조(auxiliary)는 모두 정리됐습니다. 알고리즘 설계
기준은 [CORE_LOGIC.md](CORE_LOGIC.md), 모듈 상태는 [CLAUDE.md](CLAUDE.md) 참고. 이
문서는 *무엇을 어떤 순서로 채우는가*만 추적합니다.

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
  `Prem_env.gensym_ids` 대신 자체 `gensym_ids`를 가짐(Prem_env 미복구).
- ✅ **`Simplify` = identity (이 프로젝트의 설계 결정).** `simplify_spec`은 스펙을
  그대로 반환합니다. `To_ctrs`가 유일한 번역 표면이라, 옛 IL→IL 단순화 로직(과 그것만
  먹이던 `Prem_env`)은 **재도입하지 않습니다.** 따라서 `Prem_env` 재구현 불필요.
- ✅ **Maude 백엔드 복구·컴파일됨**(`rewrite` 브랜치에서): `to_maude`(스텁→복구),
  `maude_run`·`of_maude`(삭제됐던 것 복구), `maude_theory`. `maude_theory`는 새
  설계에 맞게 **죽은 `native_system` fold 패스를 제거**하고, 공유 저수준 역할만
  남김 — wrapper 철자(`nat`/`int`/`bool`/`txt`) + 리터럴 빌더(`nat_t`/…)·
  `is_literal_sym`·`string_literal`·`chars_value`. (Native 항은 옛 fold가 아니라
  `of_spec ~scalars:Native`가 직접 생성.)
- 🔴 스텁: **`To_ctrs.of_spec`·`var_type_hints` 둘뿐.** 채우면 분석/실행 모두 즉시
  동작(지원 패스·Maude 백엔드는 이미 배선·컴파일됨).
- ⛔ 삭제 유지: `prem_env`(불필요), `cocoweb`·`muterm`·`aprove`·`termination`
  (COPS/TPDB 표면 제거 — 분석 confluence는 MFE 한 경로).

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
- [x] **`Simplify` = identity** (done) — 이 프로젝트는 단순화를 하지 않음. `Prem_env`도
  재구현하지 않음(그것만 먹이던 엔진이라 불필요).
- [ ] **`To_ctrs.of_spec`** (심장) — `prelude` + `defs_of_typ` + `term_of_exp` +
  `conds_of_prem` + `rules_of_def` + 반복/subtype 헬퍼 + `prune_unused`. **`~scalars`**:
  `Structural`=Peano 등 + prelude / `Native`=ground 스칼라 wrapper(`Maude_theory`의
  `nat_t`/`int_t`/`bool_t`/`text_t` 철자) + `native_replaced_heads` 생략.
  ⚠️ `Simplify`가 identity이므로 `of_spec`는 **un-simplified IL**을 직접 받는다 —
  옛 설계가 simplify에 맡기던 정규화(변수 전개·`matches`/필드접근 fold·subtype→cast
  등)를 `of_spec`가 스스로 감당하거나, 그 단순화를 전제하지 않는 generic 번역이어야 함.
- [ ] **`To_ctrs.var_type_hints`** — `VarE` note에서 변수 narrow 타입 복원(To_maude용).
- [ ] **검증: impty/base CTRS 골든** byte-identical (`rewrite --ctrs` 덤프 ↔
  `spec.rewrite`). 기본 `rewrite`는 실행 모듈을 내므로 분석-CTRS 골든은 `--ctrs` 경로.
  ⚠️ Simplify=identity이므로 옛 골든과 다를 수 있음 — 의도된 변화면 골든 갱신.
- [ ] **`Mfe` calibration** — 실제 MFE로 load 파일명·`(check ...)` 문법·verdict 토큰 보정
  (`mfe.ml` 상수/파서). impty/base structural 시스템에 CRC+ChC 적용.
- [ ] `to_ctrs.ml` 상단 `[@@@warning "-32-69"]` 제거(빌더 레이어가 다시 쓰이면).

## M2 — 실행 (Maude)

- [x] **`To_maude.*` 복구** (done) — Native 시스템 → order-sorted Maude 모듈(sort 복구·
  op 선언·eq/rl·내장 delegation eq·input-moded relation을 eq로) + META-TERM 인코더
  (`meta_term_of_value`/`meta_start_app`). `Pipeline.maude_system_of_spec`
  (= `of_spec ~scalars:Native`) 직접 경로 사용. `of_spec` 스텁이 채워지면 즉시 동작.
- [x] **`Maude_run` 복구** (done) — 로컬 maude 구동·`metaReduce`·normal form 파싱·stuck
  판정·`run_batch`(모듈 내부화 상각).
- [x] **`Defunctionalize` + `Gensym` + `Builtin` 재생성 + 파이프라인 재배선** (done) —
  p4용(def-인자 specialization, `$fresh` 상태 스레딩, 컬렉션 builtin 규칙). 두 경로
  공통 `Pipeline.build`로 wrap(Defunctionalize FIRST / `of_spec ~extra_defs:Builtin`
  / Gensym LAST). impty/base엔 identity. `of_spec` 스텁이 채워지면 즉시 효과 발생.
- [ ] **CLI `run` 실행 배선** — 현재는 모듈 emit만. `--imp`/`--p4`/`--check-p4` 실제
  실행은 targets의 `maude_start_term`(start value/relation 이름 공급) 복귀 +
  `Maude_run`/`Of_maude` 연결 필요. `of_spec`가 동작해야 검증 가능.

## M3 — 결과-VALUE 오라클

- [x] **`Of_maude` 복구** (done) — Maude normal form → IL value 역번역 + `canonicalize`
  (gensym 이름·map 정렬 정규화).
- [ ] **differential** — same-spec interp(p4) vs Maude(p4) 결과값 비교(CLI `run`
  실행 배선 후).

## 권장 순서

(이미 복구·완료: CLI 배선, `Exp_map`, `Defunctionalize`/`Gensym`/`Builtin` + 파이프라인,
`Simplify`=identity, Maude 백엔드 `To_maude`/`Maude_run`/`Of_maude`.)

남은 핵심은 `To_ctrs` 둘뿐:

```
  → To_ctrs.of_spec(Structural) + var_type_hints                   [M1: impty/base 골든]
  → Mfe calibration
  → of_spec(Native) 검증(이미 To_maude/Maude_run 대기 중)          [M2: 실행]
  → CLI run 실행 배선 + targets maude_start_term                    [M2/M3: 오라클]
```
