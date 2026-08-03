# `rewrite` 재구현 TODO (new-rewrite)

**골격은 전부 채워졌습니다 — `failwith` 스텁은 하나도 남아 있지 않습니다**(코드
전수 grep 확인). 번역(`To_ctrs.of_spec`/`var_type_hints`), 지원 패스
(`Defunctionalize`/`Gensym`/`Builtin`/`Exp_map`), Maude 백엔드
(`To_maude`/`Maude_run`/`Of_maude`), confluence 게이트(`Mfe`)가 모두 구현·컴파일되고
동작합니다. **M2(실행)·M3(오라클)도 완료** — `run --imp/--p4/--check-p4` 실행 배선과
same-spec differential(전체 corpus; completeness 0 / soundness 1(알려진 issue1944) /
Phase D 결과-VALUE 1227/1227 MATCH)까지 끝났습니다. **분석 confluence의 subty-가드
MAYBE도 해소** — B′ 7심볼 전부 YES(2026-07-07, `Reflect.expand_subty_guards`), owise
반사 72/72 달성 + `drop_owise` 폴백 제거, holds_R output-carrying 일반화까지 완료
(아래 2026-07-07 항목). **구조적 CTRS differential**(binary nat 이론, Phase D
1227/1227 MATCH)과 **termination 스윕**(153심볼 CRC+term, [verification.md](../../../verification.md))도
완료됐습니다. 남은 일은 아래 "남은 작업" 블록(대형 슬라이스 CRC의 슬라이스 축소,
`$bitstr_to_int` w=0 비종료, LTL 모델 검사, SCC, 잔여 MAYBE)입니다.
알고리즘 설계 기준은 [CORE_LOGIC.md](CORE_LOGIC.md), 모듈 상태는
[CLAUDE.md](CLAUDE.md) 참고. **실험의 우선순위는 이제 논문 계획
([PAPER.md](../../../PAPER.md), CAV 2027 Short Application Papers, rev.3)이 함께
정합니다** — 측정 freeze 10월 말, `== false` 근사 정량화(실험 ⑨)와 증분 게이트 비용(실험 ⑩)이
그쪽의 최우선 항목입니다. 이 파일은 **무엇을 할지**, PAPER.md는 **왜 그것을 쓰는지**를 답니다.

## 현재 상태

- ✅ 빌드됨·온전: `Rewrite_system`(`{vars;rules}` + `string_of_term`·질의·`slice` +
  `string_of_system_maude`/`ops_of_system`), `To_ctrs` 심볼/빌더 레이어 + thin 질의
  (`def_symbols`·`input_moded_rel_syms`·`rule_head_syms` — 뒤 둘은 2026-07-14
  삭제: 모든 SpecTecx 관계가 입력-모드라 각각 "전 관계"/∅ 상수였다),
  `pipeline`/`rewrite`, `Mfe`(CRC+ChC 브리지), **CLI 배선**(`bin/main.ml`
  `rewrite`/`verify`/`run` + `bin/dune`에 `spectec.rewrite`).
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
- ✅ **`To_ctrs.of_spec`·`var_type_hints` 포팅 완료.** 분석/실행 모두 동작하며
  impty/base 골든이 byte-identical로 고정됨. (스텁 아님.)
- ⛔ 삭제 유지: `prem_env`(불필요), `cocoweb`·`muterm`·`aprove`·`termination`
  (COPS/TPDB 표면 제거 — 분석 confluence는 MFE 한 경로; termination은 MTT/AProVE를
  Maude 2.7.1 스택으로 외부 구동, 아래 참조).

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
    `--relations-as-rules`.
  - `verify` — MFE `Mfe.check`(CRC+ChC), `--symbol` 슬라이스·`--list-symbols`·
    `--timeout`/`--maude-bin`/`--mfe-dir`; 둘 다 YES 아니면 exit 1.
  - `run` — 현재 **모듈 emit만**(실행/`--imp`/`--p4`/`--check-p4`는 M2: `Maude_run`·
    `Of_maude`·targets `maude_start_term` 복귀 시).
  `Mfe.check`/maude 표면이 받는 `rule_heads`(= 비입력-moded relation)는
  `To_ctrs.rule_head_syms`로 계산(`input_moded_rel_syms`의 여집합 — 이 `rule_heads`
  배선은 항상 ∅이어서 2026-07-14 통째로 삭제됨, 분석 표면은 순수 등식). 세 커맨드 모두
  컴파일·dispatch 확인 — 호출 시 각 백엔드 스텁(M1 `of_spec`/`Simplify`, M2
  `To_maude`)의 `failwith`에 도달하며, 스텁을 채우면 즉시 동작.

## M1 — 분석 동작 (CTRS 생성 + confluence)

### 2026-07-30 — confluence 전수 스윕을 2,490규칙에서 세웠다 (`c9b64e92`·`052938fb`)

**중단은 예산 부족이 아니라 수확 체감이다.** `orient_conds` 이후 재측정 스윕이 565심볼 중
**278**을 채웠다(YES 150 · YES\* 6 · **TIMEOUT 122**). 규칙 오름차순으로 돌기 때문에 남은
287심볼은 전부 대형 슬라이스이고 **중앙값이 52,394규칙**이다. 결정적인 관측은
**874규칙 위가 예외 없이 전부 TIMEOUT**이라는 것 — 886~1203 밴드는 예고대로 50/50 TIMEOUT이었고
(`c9b64e92`), 그 위도 같았다. 심볼당 base 2,040초 + 정규화까지 4,080초를 그대로 태우고
verdict가 없으므로, 남은 287을 같은 프로토콜로 도는 것은 **약 2주를 써서 `-`를 `TIMEOUT`으로
바꾸는 일**이다. 그래서 세웠다.

- **다운그레이드 0**: 재측정한 278 중 옛 판정을 잃은 심볼이 없다 →
  `orient_conds`는 측정된 범위에서 confluence-neutral(그 부채의 CRC 쪽은 이로써 닫힘).
- TIMEOUT 122의 성격은 둘로 갈린다 — 400규칙 이하 비트벡터 산술 7건과, 874~2,490규칙 구간에서
  예산을 소진한 115건. **둘 다 임계쌍 폭발이지 비합류 witness가 아니다.**
- **다음 수는 예산이 아니라 슬라이스 축소다.** 후보는 (a) 맨변수 바인더 제거(아래 2026-07-24
  research note — 측정된 비용의 98.6%가 여기 몰려 있고 `$callableId*`는 250.3초→0.7초),
  (b) 추가 pruning, (c) 모듈러 분해(산술을 블랙박스로; termination 쪽에서 이미 통한 수법).
  셋 다 착수 전이다.
- term 스윕은 별도로 멈춰 있다(측정 308: YES 307 · TIMEOUT 1).

### 2026-07-29 — `$write_value_from_bits'` n_var=0 경계 수정 — ✅ 완료. 가드 **철자**가 슬라이스 크기를 지배한다

**결함(스펙 쪽).** `2.1.2-value-aux.spectec`의 varbit(`V`) 절 두 개가 `n_var = 0`에서 동시에
발화한다 — 폭 0 절(필드 유지)과 일반 절(0비트를 디코드해 덮어씀). 인터프리터는 절 순서로 첫
절을 골랐지만 그 순서는 CTRS에 없으므로, 번역하면 겹치는 등식 두 개가 된다(= `$write_value*`
CRC MAYBE 5의 뿌리). 일반 절에 가드를 복원해 순서가 지던 disambiguation을 명시했다.

**철자 선택이 결정적이다 — `=/=`는 값 우주 전체를 슬라이스로 끌어온다.** 처음 쓴
`-- if n_var =/= 0`은 nat 비교가 아니라 **일반 구조 등식 `eq`** 로 번역된다. `eq`의 정의
테이블(문자 9,025 + `variant-*` 전부, 합 50,629규칙)이 통째로 의존성 폐포에 들어와 슬라이스가
**271규칙 → 50,900규칙**으로 폭발했고, CRC는 5심볼 전부 outer 5400초에 verdict 없이 kill됐다
(종전 `YES* ~252초`에서 명백한 회귀). `-- if $(n_var > 0)`으로 바꾸면 산술 프리루드의 `leq`
하나만 쓰므로 슬라이스는 **271규칙 그대로**고, 조건은 `leq(n-var, bzero) = false`로 나와
형제 절의 head 패턴 `bzero`와 같은 subject의 true/false 극성 충돌이 된다 — `align_guards`가
이미 정규화해 두는 바로 그 형태라 CRC가 임계쌍을 discharge한다.

> **교훈: nat/int 위의 "≠ 상수" 가드는 `$(x > 0)` 같은 산술 비교로 쓴다.** `=/=`는 실행
> 표면에는 무해하지만(등식 하나) 분석 표면에서는 심볼 하나를 전체-시스템급 슬라이스로 만든다.
> 스펙을 고쳐 CRC를 돕는 작업은 **고친 뒤 슬라이스 크기(`rewrite --list-symbols --sizes`)를
> 반드시 다시 재라** — 판정을 기다리기 전에 5초면 회귀가 보인다.

**판정은 안 바뀐다 — 이 오버랩은 MAYBE의 원인이 아니었다.** 가드를 넣은 뒤에도 base CRC는
`$write_value_from_bits_prime`에서 여전히 **MAYBE(118.8초)**이고 `--crc-normalize`가 YES로
올린다(수정 전과 같은 `YES*`). 즉 다섯 MAYBE의 실제 원인은 tuple 바인더의 **결정성 임계쌍**
(아래 "CRC 정규화 패스" 절)이고, `n_var = 0` 오버랩은 그 위에 얹혀 있던 별개의 진짜
순서-의존 결함이었다. CLAUDE.md·todo가 "MAYBE 5의 뿌리"라고 적어 둔 부분은 이 측정으로
정정한다 — 고칠 가치는 여전하지만(순서가 지던 disambiguation을 CTRS가 잃는 건 실제 결함),
CRC 열이 좋아지리라 기대할 근거는 없었다.

**무회귀 검증**(`> 0` 판): varbit 프로그램 44개(p4_16_samples + p4_16_errors) 인터프리터 판정이
baseline TSV와 **44/44 일치**, `run --check-p4` 결과-VALUE **35/35 MATCH**(유일한 STUCK
`issue1879-bmv2.p4`는 baseline에서도 interp FAIL/Maude STUCK). CRC 재측정치는
[verification.md](../../../verification.md) 표.

### 2026-08-03 — `sanitize`의 밑줄 소실로 인한 심볼 합쳐짐 — ✅ 해소 (밑줄을 이름 문자로)

**수정.** `R.sanitize`의 `is_alnum`을 `is_name_char`로 바꿔 `_`를 **구분자가 아니라 이름
문자**로 다룬다(`mnemonic_of_char`의 버리는 목록에서도 제거). 선행 밑줄에는 `c_` 접두를
**붙이지 않는다** — Maude는 선두 `-` 연산자 이름을 받고(미니 모듈로 실측), TPDB 쪽은
`unravel.ml`의 `scrub`이 이미 `_` 선두를 식별자 선두로 허용한다. 숫자 선두 가드만 남는다.

**측정 (p4 두 표면 전량 대조).**
- **순수 개명이다.** 분석 표면의 eq 72,504 / ceq 2,441 / op 3,139가 **모두 불변**이고,
  이름을 옛 철자로 되돌리는 역변환을 걸면 잔차가 `$capture-avoiding` 2줄과 길이 상한에
  걸려 해시 접미가 바뀐 생성 헬퍼 이름들뿐이다. 합류성·종료성은 개명에 불변이므로
  [verification.md](../../../verification.md)의 판정은 **재측정 없이 이월**했다(같은 파일의
  "이름·규칙 수 갱신" 항목). 실물 확인: `$join_text`·`$invalidate_header`·`$is_some_`
  CRC 전부 YES, `$join_text`·`$is_some_` term YES.
- **중복 op 이름이 사라졌다**: 옛 표면은 `$capture-avoiding`이 이름 중복(서로 다른 함수
  둘), 새 표면은 arity 오버로드인 `tuple`만 남는다.
- **실행 표면에서 진짜 결함 두 개가 같이 고쳐졌다.** (a) `$capture_avoiding_`의
  `isStuckHead` 총체화 등식이 **아예 없었다**(합쳐진 5-arity 쪽에 가려짐) — eq 70,756→70,757의
  +1이 그것이다. (b) 그 함수의 gensym state 변수가 **다른 함수의 선언 sort를 받아
  `St0:Set`** 이었고 이제 `St0:Val`이다. 종전 항목이 "미규명 부작용"이라 적었던
  `Set List List Set`→all-`Val` 변화가 이것으로, **옛 좁은 sort 쪽이 틀린 것**이었다:
  gensym state가 붙은 op는 선언 arity와 어긋나 원래 전부 all-`Val`이다(`$subst_typeIR` 등
  대조 확인). **단 이 둘은 잠복 결함이다** — `$capture_avoiding_`는 스펙 전체에서 `dec`와
  `def` 두 줄이 전부이고 **호출자가 없다**(`grep`으로 확인). 충돌이 지금껏 differential에
  안 잡힌 이유가 이것이며(Maude는 arity로 op를 구분하므로 4인자 쪽 호출은 늘 옳게 풀렸다),
  따라서 이 두 수정은 **실행으로는 검증할 수 없다**. 검증의 무게는 개명 증명과 아래
  differential이 진다.
- **무작위 differential (2026-08-03, 시드 7, 150/1568 표본)**: Phase D **126/126 RESULT
  MATCH**(MISMATCH 0), completeness gap 0(Maude 거부 24건 전부 interp도 FAIL),
  soundness gap 0. 낡은 baseline TSV 대비 판정 차 14건은 전부 개선 방향(`OTHER`→`OK` 11,
  `ERROR`→`NOTRED` 3 — 후자는 `run` 대 `run --check-p4`의 출력 어휘 차)이고 **`OK`→비-`OK`는
  0건**이다. 분석 표면은 무작위 심볼 24개(CRC 12 + term 12)를 뽑아 `verification.md`
  기록치와 대조해 **24/24 판정 일치**(`$bitacc_offset_op`의 TIMEOUT 포함).
- 부수: p4 분석 표면 36,984줄·실행 표면 34,090줄이 개명으로 달라지고 impty/base 골든을
  갱신했다. 대부분은 선행 밑줄을 가진 **mixop 자리표시자**(`_BOOL`→`-BOOL`)에서 온다.

**남은 한계**: 니모닉 토큰과 리터럴이 만나는 자리(`a-b`와 `a_minus_b`)는 여전히 합쳐질 수
있다. 지금 스펙에는 그런 이름이 없고, `sanitize`의 주석이 이 한계를 명시한다.

<details><summary>2026-07-24 원래 진단 (기록 보존)</summary>

`R.sanitize`(`rewrite_system.ml`)는 `_`를 **토큰 구분자로 버리고** 토큰을 다시 `_`로
잇는다. 그래서 내부 밑줄은 우연히 복원되지만 **선행·후행·연속 밑줄은 소실**된다.
`To_ctrs.func_sym = "$" ^ sanitize`이므로 p4의 두 함수가 한 CTRS 심볼로 합쳐진다:

```
dec $capture_avoiding_(theta, typeParameterIR*, bound)         -- 3인자
dec $capture_avoiding (theta, typeId*, typeId*, bound)          -- 4인자
                    ↓ 둘 다 $capture_avoiding
op $capture-avoiding : Set List List Set -> Val .               -- 분석 표면에
op $capture-avoiding : Val Val Val Val Val -> Val .             -- 같은 이름 2회
```

**실측 범위** (`dec`/`relation` 574개 전수, 스크럽 재현):
- 실제 충돌은 **이 1쌍뿐**. `--list-symbols`가 574행이지만 고유 이름은 573.
- 후행 밑줄 이름은 30개(`$assoc_`·`$concat_`·`$filter_`·`$exists_`·`$forall_`·
  `$init_`·`$is_some_`·`$opt_as_seq_`·생성된 `$itermap-…` 등)인데 짝이 없어 충돌은 안 한다.

**수정을 시도했다가 되돌린 이유.** `_`를 이름 문자로 바꾸면(=`is_alnum`에 추가) 충돌은
해소되고 심볼 수가 573→574가 되지만, 밑줄을 보존하는 어떤 방식이든 위 30개 이름이 전부
바뀌어 **p4 분석 표면이 37,003줄 달라지고** impty 골든도 깨진다. 게다가 분리된 4인자 op의
sort가 `Set List List Set`→`Val Val Val Val`로 변하는 미규명 부작용이 관측됐다. 실익
1쌍에 비해 파급이 크고 모든 기존 측정의 재검증이 필요하므로, 이 커밋에서는 한계를
기록만 하고 코드는 손대지 않았다(`test_rewrite.ml`의 `sanitize/known-collision`이
현재 동작을 고정해 두므로, 고칠 때 뒤집을 실패 케이스가 이미 있다).

**고친다면**: 이름 보존(30개 변경 수용 + 골든·측정 재검증) 대 충돌 시에만 유일화
(`func_sym`을 중앙 인덱스 경유로 바꿔야 — `maude_sorts`/`builtin`/`spec_index`가 각각
독립 호출하므로 순수 함수로는 불가) 중 선택. 전자가 근본적이고, 후자는 이름이 인위적이 된다.

→ 전자로 갔다(위 2026-08-03 항목). 당시 셈한 "37,003줄"·"30개 이름"은 함수 이름만 센 것이라
과소평가였다 — 실제로는 선행 밑줄 mixop까지 바뀌어 분석 표면 36,984줄이 달라진다. 다만 그
전량이 **개명**임을 등식 수 불변 + 역변환 대조로 확인해서, 판정 재측정 없이 이월할 수 있었다.

</details>

### 2026-07-24 — refactor/rewrite-clean 통합 + termination 예산 사다리 + term 전수 재측정 — ✅ 완료

**통합 (`160fef97`, merge).** `refactor/rewrite-clean`(17커밋)을 `new-rewrite`에 merge로
합쳤다. rebase가 아니라 merge인 이유: (a) `verification.md`가 SHA로 앵커를 거는
`new-rewrite` 히스토리를 rewrite하면 측정 기록의 추적성이 깨진다, (b) `git merge-tree`
실측에서 충돌 파일이 `bin/main.ml`·`rewrite_system.ml` **2개뿐**인 반면 rebase는 17커밋을
차례로 replay하며 `e179ce0d`(rewrite_system 분할)·`6022a3a5`(스윕 이관)에서 정면 충돌한다.

해소 요지 — refactor의 레이아웃을 채택하고 그 위에 new-rewrite의 기능을 다시 앉혔다:
- `Rewrite_system`은 refactor의 슬림판(검사기 패스는 `crc_surface`/`scc_surface`) + `slice`
  뒤에 slicer 인덱스(`type slicer`/`make_slicer`/`slice_with`) 재삽입. slicer는 검사기 패스가
  아니라 `slice`의 인덱스형 변종이므로 데이터 모델에 남는 게 맞다.
- `main.ml`의 스윕 골격은 `Cli.Analysis_sweep`(`require_roots`/`roots`/`rows`/
  `recorded_symbols`). 다만 **confluence는 `rows`로 표현할 수 없다** — `rows`는 심볼당 순차인데
  new-rewrite의 confluence는 `Mfe.check_batch` 한 세션 + `on_result` 스트리밍 + inconclusive
  보류/재시도다. 그래서 배치 블록은 유지하고 roots·resume·정규화 호출만 refactor 모듈로 돌렸다.
- termination/scc는 `rows ~row_of` 안에서 `Subproc.timed`로 벽시계 열을 만든다.
- `slice_size` 헬퍼는 slicer를 받도록 바꿔 세 커맨드가 인덱스를 공유한다.

**무회귀 게이트 (통합 전후 실측).** 결정적 출력이 전부 byte-identical: `specs/p4`·`p4-old`·
`impty/{base,closure}`의 `rewrite`(실행 모듈)와 `rewrite --ctrs`(분석 CTRS), p4
`--slice-dir`의 **심볼별 슬라이스 2356개 전량 + `_fidelity.tsv`**, impty 골든
(`specs/impty/base/spec.ctrs`), `--all` 루트 순서, resume 프로토콜, `scc --emit`. 유일한
비동일 항목은 스윕 벽시계 열의 20.9초 대 20.8초(측정 지터)와 캡처 스크립트가 stderr에
덧붙이던 `exit=0` 한 줄이다.
- ⚠️ p4-old의 `--slice-dir` 전량 대조는 **생략**했다(p4 2356개가 같은 표면을 덮고, pre/post
  각각 20여 분이 더 든다). p4-old는 `--ctrs`/`rewrite` 전문 대조로 남는다.

**예산 사다리 (`a582c670` + 수정 `a1fd043b`).** `verification.md`의 `term초`가 난이도가
아니라 `--budget` 설정값이었다는 진단에 대한 대응. `Termination.check`가 unravel된 TRS
하나에 예산을 올려가며(5·20·80·…·cap) AProVE를 반복 호출하고 처음 답이 나온 rung을
기록한다. 마지막 rung이 항상 cap이라 **판정은 바뀔 수 없다**. TSV 5열 추가, resume은 0열만
읽으므로 옛 4열 파일과 호환.
- ⚠️ **첫 구현은 AProVE `Error`를 영구 실패로 보고 cap으로 점프시켰고, 이는 틀렸다.**
  `$bitacc_offset_op`은 예산 5에서 판정 줄 없이 6초 만에 끝나고(→ `Error "no YES/NO/MAYBE
  line"`, 크래시와 구별 불가) **같은 TRS가 예산 20에서 22초에 YES**다. 답이 나오는 rung을
  건너뛰고 1800초를 태웠다. 수정: 답(`Yes`/`No`)이 아니면 전부 등반하고, 진짜 영구 오류인
  바이너리 부재만 사다리 진입 전에 한 번 검사한다. 판단은 순수 술어 `Termination.decisive`로
  분리해 유닛 테스트가 잡는다(뮤테이션으로 이빨 확인).

**전수 재측정 (`0621234d`, `2791cf01`).** 판정은 전부 불변, 시간만 제자리를 찾았다.
- ≤500 (153심볼): **153/153 YES**. 답한 실행의 벽시계 합 279.8초(종전 열은 예산 300 고정의 합 13,000.9초를 재고 있었다). 151심볼이 첫 사다리 단계(≤6초)에 답하고 `$bitacc_{range,offset}_op` 둘만 21.6초다. 옛 표의 1위 `$bin_minus`(1800.5초)는 5.6초로 답한다.
- \>500 (124심볼): **124/124 YES, 답한 실행 벽시계 합 479.6초.** 폐기된 모듈러-B 축의 MAYBE 11건이
  전부 닫혔다. §2 표를 27행 stale 스냅샷에서 124행 직접 축으로 교체.
- 밴드 분모 127 → 124는 iter 헬퍼 통합(2026-07-18)에 따른 세대 차이이지 버그가 아니다.

**남는 일**: >500 구간의 CRC 전수 측정(심볼당 20~25분, 124심볼이면 2일 규모).

### 2026-07-24 — 【research note】 CRC 비용의 지배 요인은 맨변수 바인더 조건 — inline/unravel을 슬라이스별로 **선택**할 여지 — 논의/확인 필요

> `verification.md` ≤500 슬라이스에서 CRC 시간이 규칙 수와 어긋나는 이상치를 추적하다,
> 비용의 98.6%가 한 구문 부류에 몰려 있음을 확인했다. `40319e67`(`order_conds`)이
> 닿지 못하는 잔여 부류이고, `fold_premise_binders`와 `crc_unravel` **양쪽을 다 빠져나간다**.
> 아래 수치 중 벽시계 초는 `confluence` 서브커맨드(생산 경로)로 잰 것이고, 구조 수치는
> `rewrite --ctrs` 덤프의 정적 분석이다.

**잔여 부류의 정체.** Maude의 CRC는 조건부 등식 `l = r if s = t`를 조건부 규칙
`crl l => r if s => @X /\ t => @X`로 재인코딩한다. `t`가 **맨변수**면 그 변수가 재작성
조건의 왼쪽에 놓이고, Maude는 `l => r`에서 `r`쪽만 바인딩으로 인정하므로
`Warning: variable V is used before it is bound`를 찍는다. 매칭이어야 할 것이 탐색이 된다.
`order_conds`가 보장하는 것은 **평가측 `s`**가 이미 묶여 있다는 것뿐이라, 어떤 순서로
재배열해도 이 부류는 남는다(맨변수 패턴측을 자기 바인딩 조건보다 먼저 묶을 순서는 없다).
`$callableId_IR`에서 조건 6개 전부 평가측 OK인데 경고 5개 = 맨변수 패턴측 5개로 정확히 일치.
2026-07-24 WLL 노트의 「결정성 위반 0」은 DCTRS 결정성(평가측)을 잰 것이고 이것과
**다른 층위의 수치**다 — 서로 모순이 아니다.

**전수 실측 (≤500 슬라이스 150/153, CRC초는 `bff805ec` 측정치).**
- 조건부 등식 없는 84 슬라이스: 합 **25.1초**, 최대 8.3초(`$bin_div`, 113규칙).
  조건부 등식 있는 66 슬라이스: 합 **15,402.6초**. → ceq가 예산의 **99.8%**를 진다.
- 맨변수 패턴측 0인 109 슬라이스: 합 212.4초, 중앙값 0.1초.
  1개 이상인 41 슬라이스: 합 **15,215.3초 = 전체의 98.6%**, 중앙값 43.6초.
- 둘 중 하나만으로는 안 터진다 — 2×2 (중앙값):

  | | 규칙<50 | 규칙≥50 |
  |---|---|---|
  | 맨변수 0 | 0.1초 (n=98) | 6.5초 (n=11) |
  | 맨변수 ≥1 | 0.2초 (n=9) | **63.7초 (n=32, 합 15,171초)** |

- 순위상관은 규칙 수가 여전히 최선(ρ=+0.898, 조건부 부분집합 내 +0.951). 규칙 수는
  **순위는 맞히되 크기를 크게 틀리고 기전을 안 준다** — 프록시 교체가 아니라 보강이 맞다.

**두 패스를 다 빠져나가는 이유.** 죽은 바인더(uses=0)는 `fold`가 인라인할 곳이 없어 남기고,
`crc_unravel`은 게이트 `| App _, App (f,_) when fresh <> [] && is_defined f`가 **tp가 맨변수인
경우를 명시적으로 제외**한다(주석: "a BARE-VARIABLE binder `s = v` is fold's job"). 그 판단은
결정성 임계쌍만 보고 내린 것이고, `=`→`=>` 재인코딩이 만드는 자유변수 탐색 비용은 고려 밖이었다.

**게이트를 열어 본 실측** (env 스위치 2개로 4구성을 한 빌드에서 덤프 — 커밋하지 않은 실험):

| 구성 | `$callableId_IR` rules/conds/맨변수 | `$bin_band` | `$bin_shl` | `$write_bits_from_value` |
|---|---|---|---|---|
| C0 base | 13 / 6 / **5** | 202/42/**10** | 201/43/**8** | 103/12/**3** |
| C1 현행 `--crc-normalize` | 13 / 6 / **4** | 202/42/1 | 201/43/1 | 103/12/**3** |
| C2 게이트 완화(inline→unravel) | 17 / 2 / **0** | 203/41/0 | 202/42/0 | 106/9/0 |
| C3 unravel만(inline 생략) | 18 / 1 / **0** | 209/32/0 | 206/35/0 | 106/9/0 |

- **`$write_bits_from_value`는 현행 정규화가 완전한 no-op**(C0=C1). TIMEOUT 7개 중 유일하게
  현행 수단이 무력하고 게이트 완화만이 유일한 경로.
- **C3가 C2를 조건 수·중첩쌍 양쪽에서 이긴다**: 조건 band 41→32, bxor 35→28, shl 42→35;
  중첩쌍 band 230/10→228/8, bxor 227/8→225/6, shl 232/15→230/13, write_value 217/4→216/3.
  aggressive inline이 subject 항을 사용처마다 복제하는 반면 unravel은 한 번만 묶기 때문.
  → **`crc_normalize`의 `fold ~aggressive |> crc_unravel` 순서가 uses가 많은 바인더에는
  역방향일 수 있다.**
- **회귀 없음**: 게이트 완화가 subject 게이트(`is_defined f`)를 안 건드리므로 `$join_text`는
  C0/C1/C2/C3 **전부 동일**(13/11/2/4/0/2) — `1dd1e43a`가 막은 over-unravel 실패 모드는 그대로.

**WLL 재검사 (독립 구현, 위 노트 수치 정확 재현).** 승격 6슬라이스 clean, `$join_text` clean,
`$set_priorities_of_tableEntryListIR` **2건**, `$starts_with`는 `t-prefix:Text` 재사용 1건.
- **CRC TIMEOUT 7개는 전부 WLL clean** — 승격 전제가 정확히 필요한 곳에서 성립한다.
- 무거운 41 슬라이스 중 **27 clean / 14 위반**(`$bin_band`·`$bin_bor` 각 2, `$bin_satminus` 3,
  `$set_priorities…` 2 등). 위반 슬라이스는 게이트를 완화해도 승격이 불가.

**소스 수준 대안 (측정 완료).** `$callableId_IR`의 전제
`-- if (_ _ _ id _ = parameterIR)*`는 와일드카드 4개가 이름 붙은 바인더로 승격되어
`$iterproj` 5벌(등식 10개)과 조건 5개를 만드는데, RHS는 `id`만 쓴다(나머지 4개가 RHS·다른
조건 어디에도 없음을 프로그램 확인). 이미 같은 명세가 쓰는 접근자 관용구
(`4.2-ir-call-overload.spectec:58`)로 바꾸면 — `-- if (id = $id_of_parameterIR(parameterIR))*` —
슬라이스가 13규칙/6조건/맨변수 5 → **4규칙/2조건/맨변수 0**이 되고, `$callableId*` 3심볼이
**250.3초 → 0.7초**(verdict YES 유지). 같은 와일드카드 구조분해 전제가 명세에 **30군데**.

**논의/확인 필요:**
- [ ] **WLL 검사로 inline/unravel을 슬라이스별 자동 선택.** 이 세션의 결론은 "둘 중 하나"가
  아니라 "슬라이스마다 다름"이다. inline은 **동치**(양방향 전이, base 표면 가능),
  unravel은 **reflect-only**(upgrade-only, WLL 필요). 그러면 선택 규칙의 자연스러운 형태는
  「WLL clean이면 unravel(더 싸고 중첩쌍도 줄임), 아니면 inline(승격 못 하니 동치 유지)」.
  검사 자체는 `Rewrite_system`의 기존 원시연산만으로 8줄 — `cond = term * term`이고
  `vars_of_term`이 중복 제거를 **안 하므로**(출현 리스트 그대로) 새 헬퍼가 필요 없다.
  규칙당 선형이라 74,945규칙 전수가 1초 미만. 붙일 자리는
  `Mfe.check_normalize_upgrade`의 게이트 / `confluence` TSV 컬럼 / `rewrite --wll-check` 중 택일.
- [x] **선결 조건 — 뒤집힌 가드의 방향 규약** — **해소 2026-07-24** (`bdceb303`,
  `Rewrite_system.orient_conds`; 아래 전용 절). 실측 142건(analysis)/95건(execution)이었고
  둘 다 0이 됐다. 이제 WLL 체커는 `(l, t₁..t_k)` = lhs + 각 조건의 `snd`, `(r, s₁..s_k)` =
  rhs + 각 조건의 `fst`로 **슬롯을 그대로 믿어도 된다**. 다만 "양쪽 다 호출"인 잔여 15건은
  방향이 여전히 임의이므로 체커가 따로 다뤄야 한다.
- [ ] **base 표면에서 고칠 수 있는 부류를 따로 둘 것.** 죽은 바인더(uses=0)는 조건
  `$f(A) = v`를 `isStuckHead($f(A)) = false`로 치환하면 패턴측이 생성자가 되어 자유변수가
  사라진다(`fold_premise_binders`가 이미 쓰는 기계). **upgrade-only 강등 없이** 적용 가능.
  다만 변수 매칭은 sort 검사를 겸하므로 `isStuckHead`와 완전 동일하지 않다 — 그 간극 확인 필요.
- [ ] **비트벡터 계열에 자유변수 제거가 충분한지 미검증.** `$bin_plus`(ceq 8, 36.3초)와
  `$bin_shl`(ceq 14, TIMEOUT)은 맨변수 8개·중첩 222 대 232로 사실상 같다. 이 계열 비용은
  공유 비트연산 라이브러리(`badd`/`bmul`/`bsub-mask` 각 16규칙, 230쌍 중첩)에서도 온다.
  `$callableId*`(중첩 0, 비용 전부가 자유변수 탐색)와 달리 **필요조건이되 충분조건이 아닐**
  가능성이 크다. C0~C3 × TIMEOUT 7을 직렬로 실측해야 갈린다.
- [ ] **소스 30군데를 접근자 관용구로 통일할지.** 가장 싸고 upgrade-only 강등도 없지만
  명세 변경이므로 interp-vs-Maude 오라클·differential 재검이 전제(이번엔 CRC YES와
  "같은 명세가 이미 쓰는 관용구"까지만 확인).

> 측정 환경 주의: 이번 세션의 벽시계 초는 컨테이너에서 `reverify` 워크트리의 스윕이 동시에
> 도는 동안 잰 것이라 `bff805ec` 기준치보다 1.6~2.0배 부풀어 있다(`$callableId_IR` 42.7→68.0,
> Constructor 75.4→137.2). 위 본문의 절대 초는 `bff805ec` 기준치를, 250.3→0.7 같은 **비교**는
> 같은 조건에서 잰 쌍을 쓴다. 별도 `.mod` 하니스로 잰 수치는 계통 오차가 커서 채택하지 않았다.

### 2026-07-24 — 【research note】 `--crc-normalize` 승격의 문헌 근거와 슬라이스별 WLL 실측 — 논의/확인 필요

> unravel(reflect-only) 승격의 이론 근거를 원전으로 재검토하고(Gmeiner–Nishida–
> Gramlich IWC 2013; Nishida–Sakai–Sakabe LMCS 8(3:4) 2012; Gmeiner WPTE'16 =
> arXiv:1701.00638), 그 구문 전제를 분석 표면 전수에 기계 검사했다.

**근거의 구조.** "정규화 YES → 원본 YES" 승격이 기대는 것은 GNG IWC 2013
Lemma 2("U(R) confluent + U가 joinability-sound ⇒ R confluent")인데,
joinability-soundness는 공짜가 아니다 — reduction-soundness가 이를 함의하지
않는 반례가 같은 논문 Ex. 3에 있다. 그것이 성립하는 증명된 클래스는 (i)
oriented **DCTRS**, (ii) **WLL**(규칙마다 `l, t_1..t_k`에 2회 이상 나오는
변수는 `r, s_1..s_k`에 등장 금지), (iii) unraveling이 **U_conf꼴**(U-심볼을
(lhs, 조건항)으로 키잉해 형제 규칙이 공유 — 우리 `crcu`의 (chain-lhs, subject)
키잉과 동형)의 삼중 전제 하의 Thm 9. 대안 경로(WPTE'16 Thm 22: right-stable +
U_seq(R) confluent **and terminating**)는 **U_opt(변수 운반 최적화판)에는
성립하지 않는다**(Ex. 2) — *어떤* unraveling인지가 전제의 일부다.

**실측 (2026-07-24, 분석 표면 74,945 규칙 / 2,356 head 전수 검사).**
- 결정성(use-before-bind)·type-3(extra-var): **위반 0** — `order_conds` 실측과 정합.
- **WLL: 전체 표면은 위반 273건.** 바인딩된 변수를 조건 패턴 자리에 재사용하는
  동등성 테스트 인코딩(`$starts_with`의 `slice(t,…) = t-prefix`, `$lookup`의
  key 매칭)이 원인. right-stability fresh 위반 247건; 별도 157건은
  `none = $f(x)`꼴 방향 뒤집힌 가드(문헌 방향으로 읽으면 무해한 표기 artifact).
- **승격이 실제 일어난 6개 슬라이스**(`$write_value*` 5종, `$bin_concat`)는
  **plain·normalized 덤프 모두 위반 0**(WLL·right-stability·좌선형 전부 충족).
  `$join_text`도 clean. WLL 위반 2건(rhs 재사용)을 가진 `$set_priorities_of_…`는
  plain CRC가 이미 YES라 unravel soundness 비의존.

**논의/확인 필요:**
- [ ] **향후 승격 시 슬라이스별 WLL 재검사의 절차화.** 전체 표면이 WLL이 아닌
  이상, 잔여 TIMEOUT 7 등 다른 심볼을 `--crc-normalize`로 승격하게 되면 그
  슬라이스의 WLL을 먼저 확인해야 한다. `confluence --crc-normalize`에 검사를
  내장할지(승격 전 WLL 검사, 위반 시 승격 거부 또는 경고), 수동 체크리스트로
  둘지 결정 필요. 검사 자체는 `rewrite --ctrs --symbol NAME` 덤프의 eq/ceq를
  파싱해 「(l, t₁..t_k)에 2회 이상 나오는 변수가 r/s_i에 등장하는가」를 세는
  ~100줄 스크립트로 재현된다.
- [ ] **crcu/crck 변형의 joinability-soundness 지위.** 정리들은 무sort TRS 위
  U_seq/U_conf 원형에 대한 것이고 우리 것은 변형(함수-subject 바인더만 부분
  unravel + keep-생성자 + order-sorted)이라 현재는 같은 증명 구조 안의 유추다.
  자체 논증(tb-역번역 soundness를 우리 변형에 맞게)으로 격상할지, 유추인 채로
  두고 upgrade-only 안전망을 계속 유지할지.
- [x] **방향 뒤집힌 가드 표기 통일** — **완료 2026-07-24** (`bdceb303`). "Maude ceq
  의미론에선 무해한 표기 artifact"라는 이 항목의 판단은 **틀렸다**: unravel에서 체인이
  끊겨 termination 판정의 전제가 깨진다. 아래 전용 절 참조.

### 2026-07-24 — 조건 방향 규약 통일 (`orient_conds`) — ✅ 완료, 재측정 부채 발생

**규약.** 조건 `(s, t)`는 `s`를 **평가**하고 결과를 패턴 `t`에 **매칭**한다
(`Crc_surface.order_conds`가 명시하고 `Unravel.schedule_conds`가 그대로 읽는 규약).
그런데 소스 등식 전제는 **소스 순서 그대로** 번역돼(`to_ctrs.ml`의 `EqOp` 케이스),
명세가 호출을 뒤에 쓰면(`-- if eps = $f(x)`) 쌍이 뒤집힌 채로 나왔다.
실측: **analysis 142건 / execution 95건**, **2356 슬라이스 중 282개**가 영향.

**"무해한 표기"가 아니었다 — termination 건전성 구멍.** Maude의 `=` 조건은 양측을
정규화 후 비교하므로 *찍히기만 하는* 곳에선 안 보인다. 그러나 unravel은 패턴측을
헬퍼 규칙의 **lhs로 들어올리므로**, 패턴이 defined 심볼로 헤드되면 producer가 공급하는
값과 **영원히 매칭되지 않는다**. `$find_name_annotation_opt` 실측:

```
d_find_name_annotation_opt(…) -> u_5(none, k_5(…))              ← producer
u_5(d_find_name_annotation_opt(annotation), k_5(…)) -> u_6(…)   ← 불발
u_6(true, k_6(…)) -> d_find_name_annotation_opt(annotationListNonEmpty)  ← 재귀가 갇힘
```

`u_5(none, …)`이 정규형이 되어 그 뒤 재귀가 AProVE에 안 보인다. `TRS 종료 ⟹ CTRS 종료`는
TRS가 모든 CTRS 스텝을 시뮬레이션할 때만 성립하므로, **영향받은 슬라이스의 YES는
틀린 게 아니라 미증명이었다**. `unravel.ml`의 자체 검사(미바인딩 rhs 변수)는 자유변수가
없어서 못 잡고, 죽은 규칙은 오히려 종료를 쉽게 만든다 — 안전한 방향이 아니다.
바로 옆 형제 절(`u_7`/`u_8`, 원래 정방향)이 대조군이다.

**수정.** `Rewrite_system.orient_conds` — 패턴측이 defined 적용이고 평가측은 아닌 쌍만
뒤집는다. 양쪽 다 호출인 경우(analysis 15건)는 더 나은 방향이 없어 그대로 둔다.
`Pipeline.build_with` 말미에서 호출해 **두 표면 모두** 규약을 만족시킨다.
`conds_of_prem`이 아니라 파이프라인 후단인 이유: 번역 시점엔 `defined_heads`가 없어
IL 수준 추측이 필요한데, 후단에선 `order_conds`/`unravel`과 **정확히 같은** defined
개념을 쓸 수 있고 `EqOp` 외의 생성 경로도 함께 덮인다.

**무회귀 실측** (같은 트리에서 패스만 빼고 대조):

| 표면 | 순수 방향 뒤집기 | 그 외 |
|---|---|---|
| p4 `--ctrs` | 143 | 1 (아래) |
| p4 실행 | 90 | 1 (같은 줄) |
| p4-old `--ctrs` | 68 | 0 |
| p4-old 실행 | 28 | 0 |
| impty base/closure (양 표면) | — | **byte-identical** |

- 실행 표면 `:=`/`=>` 개수 불변(p4 6371/0, p4-old 6376/0) → **조건의 Maude 형태가 하나도
  안 바뀌었다**. 순수 등호 조건의 좌우 교환뿐.
- 유일한 비-flip 변경: `$iterall_…_ca113dfc_list_2` 한 줄에서 호출이 평가측으로 가며 sort
  추론이 `id-param--hd`를 `Val`→`Text`로 회복. op 선언은 불변. 그 헬퍼는 절이 둘뿐이라
  케이스 전수 확인 가능 — 헤드가 Text이고 일치/불일치/비-Text 세 경우 모두 발화·stuck이
  동일(조건이 이미 Text 동등성을 요구). 그래도 **최종 권위는 differential**이다.
- 유닛 테스트 5건(`orient/*`) 추가, 뮤테이션(뒤집기 무력화)으로 이빨 확인 — 2건 실패.
  impty 골든 MATCH, `dune build @fmt`/`@test/rewrite/runtest` 통과.

**⚠️ 재측정 부채**: analysis 표면이 바뀌었으므로 영향 282 슬라이스의 **CRC 열과 term 열이
stale**이다. term은 "판정이 바뀔 수 있다"가 아니라 **"기존 YES 중 일부가 애초에 미증명"**
이라는 성격이라 성격이 다르다. ≤500 밴드 확인된 대상: `$name_annotation_opt`(256규칙),
`$set_priorities_of_tableEntryListIR`(226), `…_prime`(200),
`$optional_annotation_of_parameterIR_prime_prime`(20), `$is_tableDefaultActionProperty`(16).

### 2026-07-22 — 【research note】 실행 경로를 in-binary 서브커맨드로 통합 (confluence/termination/scc 계열)

> 스크립트·스크래치패드에 흩어져 있던 검증 실행 경로를 바이너리 서브커맨드로 모았다.
> new-rewrite `b35215b3` → tip `57a99547`, 커밋 8개(각 단독 빌드 OK, **push 완료 시점은
> 아래 참조**).

**세 분석 검사기 = 하나의 균일 계열.** 분석 CTRS를 심볼별 슬라이스로 검사하는 셋이 동일한
스윕 표면을 갖는다: `--symbol NAME`(반복) 또는 `--all`(작은 슬라이스 먼저), `--out
sweep.tsv`(기록된 심볼 skip → 재개). `rewrite --ctrs`가 검사 대상을 만들고 `rewrite
--list-symbols [--sizes]`가 슬라이스할 이름을 준다.

- `confluence`(구 `verify`) — CRC+ChC(`Mfe.check`), 행 `<sym>\t<cr>\t<chc>`. 전체-시스템
  모드 없음(임계쌍 폭발; 규칙 head가 정의마다 달라 `--all` per-symbol이 사실상 동등 sound).
  `--crc-normalize`는 MAYBE/TIMEOUT만 정규화+prune 재검해 YES면 `YES (normalized)` 승격
  (upgrade-only, 하향 절대 없음; `Mfe.check_normalize_upgrade`).
- `termination` — 구조 보존 unravel(`unravel.ml`) → AProVE(`aprove.ml`, tools/aprove/runme).
  `--emit-trs`/`--budget`. **MTT 완전 대체**(2026-07-19 note 참조).
- `scc` — over-approx(drop_conds+linearize) → 프루닝된 `(fmod)`(To_mfe `?functional`) →
  CETA Maude 2.7 + 구 MFE. 행 포맷은 옛 `run-scc.sh`와 byte 호환. CETA 에셋 부재로 실
  verdict 미측정.

**커밋 8개**: `d3bf2847`(MTT 사장 스크립트 삭제) · `1f9666dd`(Subproc 러너 추출) ·
`c2d22823`(`rewrite --ctrs --prune-signature`, To_mfe `?prune_signature`) · `e518f5d4`
(termination) · `b69e4f63`(scc) · `c269d5bd`(confluence `--crc-normalize`, 재시도 prune
적용) · `4f0bc5a2`(docs) · `57a99547`(reorg: verify→confluence 재명명 + termination/scc와
동일 스윕 구조, `--list-symbols`/`--sizes`를 rewrite로 이관, `--emit` 미추가=rewrite
--ctrs --symbol 중복).

**동등성 검증(전부 통과, verification.md 반영)**: (a) 프루닝 vs python `prune_slice_signature.py
full` — 2354 슬라이스 전수에서 규칙/sort/subsort-edge 완전 동일(op 차이 189건은 python
프루너의 오버로드-collapse 버그일 뿐, OCaml이 옳음). (b) `termination --emit-trs` vs
sp_unravel.py — 10심볼 byte-identical + 153/153 HEAD 슬라이스가 측정 골든 TRS와 byte-identical.
(e) `confluence --crc-normalize` — 정규화 불필요 심볼 plain YES, 승격 심볼 `YES (normalized)`
(prune 없으면 signature-blowup으로 TIMEOUT 재현 못하던 결함을 재시도 prune으로 수정).
유닛(test/rewrite) + cram(test/cli/analysis-commands.t: termination/scc emit·guard, prune,
confluence guard, rewrite --list-symbols/--sizes) 추가.

**폐기 스크립트 삭제 완료(2026-07-22).** `run-scc.sh`/`run-scc-sweep.sh`(→ `scc`),
`prune_slice_signature.py`(→ `rewrite --prune-signature`) 삭제. 애초 게이트였던 "reverify
phaseB가 메인의 python 프루너를 shell out" 조건은, 삭제 시점의 reverify 스윕이 이미 새
`confluence --all --crc-normalize`(in-binary 프루닝)로 옮겨가 python을 안 부르면서 해소됐다.
**caveat**: `scc` 실 verdict는 CETA Maude 2.7 에셋 부재로 옛 `run-scc.sh`와 행 diff 미실시
(모듈 방출은 cram `scc --emit`로 byte 확인); 에셋 확보 시 git history에서 `run-scc.sh` 부활해
대조 가능. `check_diff_p4.sh`/`check_diff_structural_p4.sh`(differential)는 서브커맨드
대체물이 없어 스크립트로 **유지**.

### 2026-07-19 — 【research note】 termination MAYBE의 진짜 원인은 MTT의 unraveling이었다 — 구조 보존 unraveling + AProVE 직접으로 153/153

> 이 항목은 그대로 research note로 옮겨 쓸 수 있게 서술한다. 아래 2026-07-17 항목이
> "AProVE 자동 전략의 도구 측 한계"로 내린 결론을 **반증**한다.

#### 배경 — 우리가 termination을 재던 방식

`tools/mfe/run-termination.sh`는 심볼 하나의 분석 슬라이스를 이렇게 흘려보냈다:

1. `main.exe rewrite --ctrs --symbol <sym>` 로 슬라이스(.mod)를 덤프.
2. `prune_slice_signature.py … full` 로 **시그니처만** 축소. To_mfe는 슬라이스마다 P4 전체
   order-sorted 시그니처(~460 sort / ~750 op)를 찍는데 MTT의 변환이 시그니처에 초선형이라
   20-rule 슬라이스도 안 끝난다. 규칙은 한 줄도 안 건드린다(측정으로 확인: 13 rule → 13 rule).
3. 헤더를 옛 Full Maude functional module로 고침(`mod`→`fmod`, `set include BOOL[-OPS] off`).
4. Maude 2.7.1(hook 빌드) + MFE 2.7.1에 stdin으로 밀어넣고
   `(select tool MTT .) (select external tool aprove .) (select path C;A .) (ct SPEC .)`.
5. **MTT 1.5j**가 order-sorted 조건부 모듈을 TPDB **조건부** TRS로 변환한다. 조건부라서
   `C;A` 경로를 고른다.
6. `termCheck` 훅이 `mfe.config`대로 `tools/aprove/runme`(AProVE WST 모드, Z3 백엔드)를
   호출. 판정은 Maude 출력의 `is terminating`(YES) / `not been found`(MAYBE)로 읽는다.

두 축의 차이는 **2번의 pruning뿐**이었다. `term(AProVE직접)`은 위 그대로, `term(모듈러B)`는
2번을 `prune_modular.py abstract-builtins`로 바꿔 **산술 규칙을 통째로 빼고 산술 op을 자유
생성자로 남긴다**(R_arith를 블랙박스로 두고 spec 층만 증명 → 모듈러 합성). 5·6번은 동일하다.
**즉 두 축 모두 MTT의 변환을 통과했다.** 이게 핵심이다.

#### MTT가 실제로 무엇을 하는가 — 출력 캡처

`mfe.config`(`aprove <path>/runme .trs`)가 가리키는 실행 파일을 "인자 `$1`을 복사해 두고 진짜
runme를 exec하는" 래퍼로 바꾸고, `MAUDE_LIB`을 그 사본 디렉터리로 지정하면 MTT가 AProVE에
넘기는 파일을 그대로 얻을 수 있다. 그렇게 얻은 `$join_text` 슬라이스의 TPDB를 보면:

**MTT는 unravel을 하지 않는다.** 조건부 TRS를 그대로 넘긴다. 대신 두 가지를 한다.

1. order-sorted를 unsorted로 낮추며 **모든 sort를 술어로** 바꾸고(`isText`, `isList`,
   `is[Val]`) 규칙마다 변수별 sort 조건을 붙인다.
2. **조건 `s = t`를 `equal(s,t) -> tt`** 로 바꾸고, 전역 비좌선형 규칙 `equal(X,X) -> tt`를
   추가한다.

#### 결함 — 매칭 조건이 동등성 검사로 바뀌면서 3형 CTRS가 된다

2번이 치명적이다. 캡처된 실제 규칙:

```
$join-text(cons(t-h1, text), t-sep)
   ->  cat(cat(t-h1, t-sep), $join-text(cons(t-h2, t-t), t-sep))
   |   equal(match-cons(text), true) -> tt ,
       equal(text, cons(t-h2, t-t)) -> tt , ...
```

`t-h2`, `t-t`가 **좌변 어디에도 없다**. 원래 `text = cons(t_h2, t_t)`는 *매칭* 조건이라 두
변수가 매칭으로 바인딩되는데, 대칭 동등성 *검사*로 바뀌면서 자유 변수가 됐다. 우변은 그
자유 변수로 지은 `cons(t-h2, t-t)`에 재귀한다.

이것이 **extra-variable CTRS (Bergstra–Klop 3형)** 이다. dependency pair framework 입장에서
재귀 인자 `cons(t-h2,t-t)`는 좌변 인자의 부분항도 아니고 어떤 구문적 관계도 없다 — `equal`
조건을 *의미적으로* 풀어야만 `text`와 같음을 알 수 있다. 3형 CTRS 종료 증명은 1/2형보다
훨씬 비싸다.

우리 인코딩이 premise에서 destructure하는 구조 재귀(`xs = cons(h,t)` 두고 `t`로 재귀)를 즐겨
쓰기 때문에 이 패턴을 대량 생산했고, 그래서 피해가 컸다.

**단, 증명 불가능해지는 건 아니다 — 비싸질 뿐이다.** 캡처한 TPDB를 AProVE에 직접 넣으면
예산을 충분히 줬을 때 **YES**가 나온다. 13-rule짜리 `$join_text` 하나가 이 인코딩에서는
120초를 넘기고, 우리 unraveled TRS에서는 **1초**다. 100배 이상이고, 슬라이스가 커질수록
격차는 벌어진다.

#### 왜 그 비용이 곧바로 MAYBE가 되는가 — MTT의 120초 하드코딩

`mtt.maude:90`이 `termCheck(TOOL, In:String, 120)` 이다. **MTT는 AProVE를 언제나 120초
예산으로 호출한다.** 우리가 스윕에서 준 `TERMA_TMO=1200`/`TERMB_TMO=1800`은 Maude *프로세스*
타임아웃이라 이 내부 한도에 아무 영향이 없었다 — 예산을 늘려도 판정이 안 바뀌던 이유가
이것이고, 나(2026-07-19 최초 서술)는 그걸 "어떤 예산으로도 도달 불가"로 잘못 읽었다.

정리하면 MAYBE는 **두 요인의 곱**이다: (a) `equal` 인코딩이 문제를 3형 CTRS로 만들어 100배
비싸게 하고, (b) MTT가 그 비싼 문제를 120초에서 자른다. 어느 하나만 없어도 살아남는다.
우리 경로는 (a)를 없애서 문제를 1초짜리로 만들고, 덤으로 (b)도 우리가 예산을 직접 쥔다.

#### 해법 — 직접 unravel해서 매칭 의미를 되살린다

unraveling은 조건을 규칙 구조 안으로 옮겨 무조건 TRS로 만드는 표준 기법이다:

```
l            -> U(s, <carried>)     -- 조건을 평가하러 간다
U(t, <carried>) -> r                -- t가 여기서 패턴으로 매칭된다
```

핵심은 마지막 줄이다. `t`가 **규칙 좌변의 패턴**으로 돌아오므로 그 변수들이 매칭으로
바인딩되고, 재귀 인자가 원 인자의 부분항임이 **구문적으로 보인다**. subterm criterion이
바로 잡는다. 이것이 이득의 전부다.

**`<carried>`가 무엇인지는 무관하다.** Marchiori 고전형(좌변 *변수*를 평평하게)과 구조
보존형(좌변 *인자 목록*을 정의 규칙 없는 불활성 생성자 `k_N`에 그대로)을 같은 슬라이스·같은
AProVE로 맞대조하면 **둘 다 0~1초에 YES**다. 실무에서는 단순한 변수 전달형을 권한다.
(공통 함정: 좌변 *항* 자체를 넘기면 규칙이 자기 redex를 재생산해 무한 루프가 된다 — 첫
시도에서 AProVE가 **NO**를 내어 그 버그를 잡아줬다.)

MTT를 빼고 이 평범한 TRS를 `tools/aprove/runme <f>.trs <budget>`(WST)에 직접 던진다.

#### 건전성

`lσ -> u(sσ, k(argsσ)) ->* u(tσ, k(argsσ)) -> rσ` 로 **원 CTRS의 모든 스텝이 시뮬레이션**된다
(조건 평가 `sσ ->* tσ` 자체도 `u(□, k(…))` 문맥 안에서 재현되므로 operational termination까지
덮는다). 따라서 **TRS 종료 ⇒ CTRS 종료**. sort/subsort는 버리므로 TRS는 **과근사**(well-sorted
항이 부분집합) — MTT와 같은 안전한 방향이다.

따름정리로 **NO는 비종료의 증거가 아니다**(과근사 탓일 수 있다). NO가 나오면 witness를
확인할 대상으로만 취급할 것.

분석면 슬라이스가 이 변환에 안전한 근거(153개 전수 확인): `owise` 0, `rl`/`crl` 0,
`assoc`/`comm`/`id:` 0, `:=`/`=>` 조건 0, import 0, mixfix 0 — 전부 prefix·단일행이다.

#### 측정 (153심볼 × 2축, 슬라이스는 `30d413ad` 덤프 재사용)

| 축 | MTT 경로 | 구조 보존 + AProVE 직접 |
|---|---|---|
| AProVE 직접 | YES 117 / MAYBE 12 / TIMEOUT 24 | **YES 153 / 153** |
| 모듈러 B | YES 150 / MAYBE 3 | YES 150 / MAYBE 2 / TIMEOUT 1 |

- **MAYBE 12건 전부, TIMEOUT 24건 중 23건**이 닫혔다. 이진 산술 계열
  (`$bin_*`·`$un_*`·`$bitacc_*`·`$write_value*`)이 통째로 풀렸다.
- 2026-07-17 항목이 "도구 한계"로 지목했던 `$join_text`/`$invalidate_value`/
  `$invalidate_headerUnion`은 MTT로 1200초 태우고 MAYBE였는데 **각 1초에 YES**다.
- **인코딩은 한 줄도 안 고쳤다.** 원인은 번역도 AProVE도 아니고 MTT였다.
- 예산 주의: `$write_bits_from_value` 축A는 300초 TIMEOUT / 1200초 YES. 표 값은 MTT와 같은
  예산으로 맞췄다.

**회귀 3건은 모듈러B 축에만**: `$write_bits_from_value`(TIMEOUT),
`$set_priorities_of_tableEntryListIR{,_prime}`(MAYBE). keep-생성자가 인자 구조를 복제하므로
항이 커지는 비용이 있고, MTT의 분해가 마침 무해했던 슬라이스에서는 그 비용만 남는다.
**두 방법의 비-YES 집합은 서로소**라, 모듈러B도 둘 중 하나만 돌리면 153/153이다.

#### 구현 함정 (놓치면 조용히 틀린 결과가 나온다 — 셋 다 실제로 밟았다)

1. **다조건 체인의 바인딩 변수 누적**. 조건이 뒤 조건이나 최종 우변이 쓰는 변수를 바인딩할
   수 있다(`… if p(x) = cons(h,t) /\ q(h) = true`). keep-생성자가 원 인자 + **앞선 조건들이
   바인딩한 변수 전부**를 날라야 한다. 안 그러면 마지막 규칙 우변에 미바인딩 변수가 생긴다.
2. **소스 조건이 use-before-bind 순서로 나온다**. reflect의 guard 패스가 `isStuckHead` 가드를
   그 변수를 바인딩하는 조건보다 **앞에** 놓는다(`$bitstr-to-int`에서 `half`). 좌변이 전부
   바인딩된 조건을 하나씩 골라가는 위상 정렬로 재배치해야 한다.
3. **커버리지를 가정하지 말고 검증**. 처리 못 한 방정식과 미바인딩 우변 변수에서 **크게
   실패**시킬 것. 위 두 버그는 전부 이 검사로 잡혔고, 둘 다 그럴듯해 보이는 출력을 내고 있었다.

#### 방법론 반성 — 어떻게 틀렸었나

최초(2026-07-19) 서술은 "MTT의 unraveling이 인자를 분해해 부분항 관계를 역전시켜, 예산과
무관하게 증명이 도달 불가"였다. **세 군데가 틀렸다**: MTT는 unravel을 하지 않고, 따라서
분해도 역전도 없으며, 도달 불가도 아니다(예산만 주면 YES).

원인은 방법이다. **파이프라인 전체(MTT 경로 vs 우리 경로)를 맞대조해 놓고 그 안의 한 요소를
원인으로 지목했다.** 두 경로는 unraveling 말고도 sort 인코딩·조건 인코딩·AProVE 예산이 전부
달랐는데, 그중 하나를 골라 이야기를 지어낸 것이다. 반증한 실험은 셋 다 "한 번에 하나만
바꾸는" 형태였다:

1. 같은 슬라이스에서 **unraveling 방식만** 교체(변수 전달 vs 구조 보존) → 둘 다 YES.
   ⇒ unraveling 방식은 원인이 아니다.
2. MTT가 AProVE에 넘기는 파일을 **캡처**해 직접 실행 → YES.
   ⇒ MTT의 출력 자체는 증명 가능하다. 문제는 출력이 아니라 호출 조건이다.
3. `mtt.maude` 확인 → `termCheck(…, 120)`.
   ⇒ 예산이 잘렸던 것이고, 우리가 준 1200s는 엉뚱한 층의 타임아웃이었다.

**교훈: X가 Y의 원인이라 주장하려면 X만 바꿔라.** 그리고 도구가 왜 실패하는지 궁금하면
도구가 실제로 내놓는 산출물을 먼저 확보하라 — `mfe.config`를 래퍼로 바꾸는 데 5분 걸렸고,
그것 하나가 잘못된 이야기 전체를 무너뜨렸다.

#### 후속

- [x] unraveler 승격 (2026-07-22 완료): 스크립트가 아니라 in-binary `main.exe
      termination`(unravel.ml/aprove.ml)으로 완전 포팅. 구조 보존형을 채택(측정
      골든 trsA와 byte-identical하게 검증 가능해서); MTT 폴백은 두지 않고 경로 폐기.
- [ ] 폐지한 모듈러 축에서 구조 보존이 MTT보다 나빴던 3건: 항 크기 가설 검증. 맞다면
      **escape하지 않는 인자는 keep에서 제외**하는 최적화로 닫힌다. 변수 전달형을 쓰면
      애초에 발생하지 않을 수도 있다(미확인).
- [ ] MTT 인코딩의 실제 비용 곡선 측정(선택). `$join_text`는 우리 1초 : MTT 인코딩 >120초다.
      슬라이스 크기에 따라 이 배율이 어떻게 커지는지 보면 "MTT를 폴백으로 둘 가치"가 정해진다.
- [ ] §2 >500 표(term(B), MAYBE 11)도 같은 경로로 재측정 — 상당수가 닫힐 것으로 예상.
- [ ] 측정 기준 커밋 확인: 측정은 `30d413ad` 덤프 기준인데 그 뒤 술어 도메인 변경
      (`6e740f3e` 계열)이 들어왔다. HEAD에서 재덤프해 대조할 것(sort 태그만 달라졌다면
      unraveled TRS는 byte-identical이라 판정 불변).

### 2026-07-17 — co-iteration `$unzip` fusion (SoA→AoS): termination 개선 시도

> ⚠️ **이 항목의 결론(맨 아래 "AProVE 자동 전략의 도구 측 한계")은 2026-07-19에 반증됐다.**
> AProVE는 이 구조를 1초에 증명한다 — 문제는 그 앞단 MTT의 unraveling이 하강을 파괴한 것이었다.
> 위 2026-07-19 research note 참조. 단, fusion 자체의 정당성과 무회귀 검증 결과는 유효하다.

**동기**: term(AProVE) MAYBE 심볼 `$invalidate_value`/`$invalidate_headerUnion`의
원인을 실험으로 규명 — co-iteration의 축별 분리(SoA) 인코딩이 소비 헬퍼의 재귀를
`$unzip_v(iterbind)`라는 **함수 호출 결과**의 tail에 걸어, AProVE가 감소를 syntactic
subterm으로 못 봄. → 소비 헬퍼(`$itercollect`/`$itermap`/`$iterall`/`$iterapply`)가
원본 `iterbind`를 직접 elem-pattern으로 destructure하는 **fused(AoS)** 인코딩으로 전환
(상세: [CORE_LOGIC.md](CORE_LOGIC.md) §3.7.1).

**구현**(`to_ctrs.ml`): rule 단위 binder 레지스트리(`iter_ctx`) 도입 — `pattern_of_exp`가
head `IterE`를 등록하고 unzip 조건을 제자리에 방출, 소비 지점(`term_of_exp`/`conds_of_prem`)이
`spines_of_ids`로 fused/bare spine 계산 + `absorbed` 기록, `prune_absorbed_unzips`가
흡수+미사용 unzip만 사후 제거(escape/dead는 유지). `spine_disamb`로 fused 변형 네이밍
(base 이름 유지 → reflect.ml 무보수 호환). helper 정의 walk는 clause 단위로 재구성.

**검증(전부 통과 — 인코딩 정확·무회귀)**:
- impty 골든 재생성 + `run-structural` 8/8 `result: true`.
- full-corpus `--ctrs` 대조: `op $unzip` **105→45**(escape 잔존분만), `$dom_map`/`$codom_map`
  등 escape 무변화, **비-iter 심볼 rule 0라인 변화**.
- 샘플 diff test: `run-structural --check-p4` 16개(union invalidate 포함) + `run --check-p4`
  10개 — **전부 MATCH, MISMATCH 0**.
- owise 반사: 72처리(69 reflected + 3 complement), **0 kept**(fused 변형도 반사됨).

**term 결과: 대상 2심볼 MAYBE 유지(성공 판정 미달) — 원인은 unzip이 아님.**
toy(단순 self-recursive)·toy2(3함수 상호재귀 미러)는 fused로 AProVE **YES** 실측(종료
구조 개선 입증). 그러나 실제 대상은 fused 후에도 MAYBE. 원인 후보를 **전부 배제**:
unzip 소거(✓)·subty guard 제거(여전 MAYBE)·yices 부재(README상 불필요, z3로 진행)·JVM 힙
(-Xmx100g로도 MAYBE)·signature 크기(prune 후 8-rule/12-op 극소 슬라이스도 MAYBE). 곧
AProVE 자동 전략이 이 특정 구조의 종료 증명을 못 찾는 **도구 측 한계**(동형 toy2는 YES).
⇒ fusion은 종료를 논리적으로 보장하고 규칙 수를 줄이는 정당한 개선이나, 이 대상들의
AProVE 판정을 바꾸지는 못했다. 후속: AProVE 전략 튜닝 / 수동 종료증명 별도 트랙.

### 2026-07-18 — Native 실행 표면의 completeness gap 2건 수정 (structural builtin 규칙 누출)

**발견 경로**: iter 헬퍼 통합 후 전체 corpus differential(1568 프로그램)에서 completeness
gap 2건(`samples/const.p4`, `samples/issue1717.p4`). fusion 이전 바이너리로도 재현돼
**통합과 무관한 별개 회귀**임을 먼저 확정한 뒤 원인을 팠다.

**공통 원인 — delegation이 있는 builtin의 structural 규칙이 Native 모듈에 함께 방출**.
`Native` 스칼라 이론은 스칼라를 손수 구현한 {!Prelude} 규칙을 **의도적으로 생략**하고
{!To_maude.delegation_eqs}의 한 줄 위임으로 대체한다. 그런데 `builtin.ml`이 만드는
structural 규칙은 Native 모드에서도 방출되고, **먼저 선언되므로 매칭을 이긴 뒤**
생략된 binary-nat 심볼로 내려가 영구 stuck이 된다(뒤에 있는 완전한 delegation은
영원히 도달 불가).

- **gap1 `const.p4`**: `~32w0` → `$un_bnot` → `$pow2` → `bpow_nat`(Native에 eq 0개)
  → `isStuckHead(i'') = false` 가드 실패 → 규칙 발화 불가. (`$bneg`은 우연히 생존 —
  structural 경로가 delegation 있는 `add_int`/`negate_int`로 이어졌기 때문.)
- **gap2 `issue1717.p4`**: `const bit<32> sz = h1.minSizeInBits();` →
  `$sizeof_minSizeInBits'(HEADER…)` → `$sum_nat` → **폴드 시드가 `bzero`**(0-arity
  상수라 1차 스캔에서 놓쳤다) → `add(nat(32), bzero)`에서 정지. 최소 재현 15줄:
  control apply 안 `const bit<32> sz = h1.minSizeInBits();` 하나면 충분(함수/control
  무관, `const sz = 5`나 `return minSizeInBits()`는 정상 — **const + 메서드 호출**
  조합만 실패).

**수정**(`builtin.ml`): `delegated_in_native`가 주석부터 "the **text** builtins"라
적혀 있었고 실제로 `int_to_text`/`strip_prefix`/`strip_suffix` 3개뿐이었다. delegation을
가진 나머지를 전부 추가 — `pow2 shl shr shr_arith bneg band bxor bor bitacc
strip_all_whitespace` + nat-list 폴드 `sum_nat max_nat min_nat`. 추가 전 각 delegation이
Native 값 전체를 커버하는 **완전한 정의**임을 확인했다. 주석도 "왜 clash가 단순 중복이
아니라 stuck인가"와 "{!To_maude.delegation_eqs}와 동기화하라"로 고쳐 적었다.

**검증**:
- 실행 모듈의 참조-미정의 심볼: `bzero`/`bpow_nat`/`badd`/`bsub`/`bmul`/`bdiv`/`bmod`/
  `bis_zero`/`bpred` 전부 소멸. 남은 8개는 spec 자체에 정의가 없는 의도적 미구현
  (`$bitacc_replace`, `$init_objectState`, `ExternFunctionCall_eval_lctk`,
  `$int_to_bits_*`/`$bits_to_int_*`)과 패턴 위치의 `bsucc`뿐 — **분석 표면에도 동일**.
- **분석 표면(CTRS) byte-identical**, impty 골든 둘 다 무변화(수정은 Native 전용).
- **전체 corpus differential 재실행**: **completeness gap 2 → 0**, Maude OK 1226 → **1228**,
  **결과값 일치 1225 → 1227 MATCH / 0 MISMATCH**(= 2026-07-15 clean-run 기준과 일치).

**남은 soundness gap 1건은 오라클 아티팩트 — 수정 대상 아님.**
`errors/issue1944.p4`는 `const bit<2147483648> x = 0;` 한 줄, 즉 폭 2³¹ 비트 타입이다.
spec에는 **비트 폭 상한 규정이 없어**(grep 확인) 이를 수락하는 Maude가 오히려 spec에
충실하다. 인터프리터는 이 프로그램을 300초는 물론 그 뒤로도 끝내지 못했고(2³¹비트
값을 실제로 만들려는 것으로 보임), `check_diff_p4.sh`가 타임아웃을 FAIL로 기록하면서
"interp FAIL & Maude OK" = soundness gap으로 잡힌다. CTRS 번역 버그가 아니므로 손대지
않는다. 진짜로 거부해야 한다면 spec에 폭 상한을 넣는 별건 작업이다.

### 2026-07-18 — iter 헬퍼 패밀리 통합: `$iterapply`/`$iterproj`→`$itercollect`, 변수별→튜플 수집, `$unzip`→`$iterproj` 개명

**동기(사용자 결정)**: IterPr 출력 쪽 3패밀리를 하나로. (a) 변수별 collect 중복이
만연(예: descriptor `e71f64a7` 전제 하나에 헬퍼 5개 — 같은 리스트 5회 주사, 같은
let 5회 평가); (b) 구 `$iterproj($iterapply(…))`는 함수 호출 결과 위 재귀 — unzip이
termination MAYBE를 만들던 SoA 잔재와 동일 패턴; (c) 2026-07-16 head-side fusion
기계(SFused)가 premise-side 소비자 배선을 이미 풀어놓음.

**설계** ([CORE_LOGIC.md](CORE_LOGIC.md) §3.7/§3.7.1): 전제당 `$itercollect` 하나가
성분(comps = rel call이면 출력 순서, 아니면 binding 순서; `iter_collect_components`)
스트림을 반환 — k=1이면 성분 자체(이름·형태 종전과 동일), k≥2면 **성분 튜플**
(조건 1회 평가). ex-apply는 원소=호출 인라인의 **무조건 특수화**로 흡수(⛔
2026-07-02의 "원소당 1회 호출" 불변식 유지; gensym effectful 판정은 call-graph라
개명 무관). k≥2 사용처는 튜플 스트림을 `iterbind_N`으로 받아 head binder와 동일
등록 → 소비 헬퍼 fused destructure / escape는 합성 튜플 본문(`collect_tuple_body`)
위 `$iterproj` / dead는 `prune_absorbed_projs`. head-side `$unzip`은 `$iterproj`로
개명해 한 패밀리(`$iterall`/`$itercollect`/`$itermap`/`$iterproj`).

**검토·기각한 대안 — SoA-return collect** (n tuple list 대신 n list tuple 반환;
재귀-조건형/accumulator형): accumulator형은 사용부가 튜플 패턴 cond 1개로 끝나
표면상 가장 깔끔하고 producer 종료도 자명하지만, **MAYBE의 원인은 사용부 경계** —
성분 리스트가 함수 결과로 태어나는 한 소비자 재귀가 C;A 후
`$itermap(π₁(collect…), π₂(…))` 꼴로 SoA 패턴을 재현한다. 부수 비용(snoc/reverse
O(n²), k≥2 ex-apply 무조건성 상실, `gen_*_holds` 형태 전제 파괴, `infer_ranges`
컨테이너 휴리스틱 실패)까지 겹쳐 기각. 전 성분 escape 사이트에 한해선 더 깔끔하나
인코딩 2벌 유지비가 projection cond 몇 개 절약을 압도.

**구현**: `to_ctrs.ml` — `iter_collect_components`/`collect_tuple_body` 신설,
`iter_collect_sym`이 comps 접미사(k=1은 byte-identical), `iter_apply_sym`/구
`iter_proj_sym` 삭제, `conds_of_prem` IterPr가 k≥2에서 등록+제자리 projection 방출,
`iterpr_defs` Some/None arm 병합(+proj_defs는 여기서만 방출 — 합성 exp는 IL에
없어 visit_exp가 못 찾음), `unzip_sym`→`iter_proj_sym`·`iter_unzip_defs`→
`iter_proj_defs`·`prune_absorbed_unzips`→`prune_absorbed_projs` 개명.
`reflect.ml` — `iter_helper_prefixes`=`[$iterall; $itercollect]`,
`gen_iterapply_holds`를 `gen_itercollect_holds`에 **형태 기반**으로 병합.
**교훈(구현 중 회귀 1건)**: "스텝 무조건 → apply형" 판정은 틀림 —
`fold_premise_binders`가 let-destructure 조건을 LHS 패턴으로 접으면 일반 collect도
무조건이 된다(owise 66/3/3 kept 회귀 관측). 기준은 **원소가 relation call인가**;
수정 후 69/3/0 복원.

**검증(전부 통과)**:
- 구조 sanity(신규 스크립트): 참조 헬퍼 전부 정의 존재·arity 일관·구 패밀리 출현 0
  (**SANITY OK**). 최초 실행에서 `$itermap-…-list-2-pair` 3건이 참조-미정의로
  검출됐는데, 추적 결과 **2026-07-16 head-side fusion 커밋(`6e96deb9`)이 만든
  회귀**였다(아래 "동반 수정" 참조). 통합 자체가 만든 것은 아니지만 같은 ctx
  스레딩 결함이므로 이 작업에서 함께 고쳤다.
- impty 골든: 개명+방출순서 churn만(내용 동일), 재생성 커밋.
- p4 corpus 심볼 대조: helper 406→411, eq/ceq 3061→3071(+10) — churn은 전부
  (i) 개명 (ii) apply 흡수 (iii) 변수별→튜플 병합(e71f64a7 5→1 등) (iv) 구 proj
  소멸 (v) 소비자 fused 전환(`$iterall-…-f<tuple>…`; 재귀가 튜플 스트림의
  구문적 tail로 하강 — 육안 확인) 분류에 귀속, 미분류 잔여 없음.
- owise 반사: **69 reflected / 3 complement-enumerated / 0 kept** (베이스라인과
  동일). 다출력 binding cond의 head가 collect 호출 자체가 되면서
  `insert_success_test`가 이제 그 사이트에도 적용됨.
- 실행 differential: `run --check-p4` 26표본(entries 10 포함) **25 MATCH /
  0 MISMATCH**(잔여 1 `issue3671.p4`는 변경 전 바이너리로도 not reduced — 기존
  동작). structural 레그 `run-structural --check-p4` 표본 **2/2 MATCH**.
- MFE CRC (`verify --symbol`): `$invalidate_headerUnion`(head-side co-iteration)
  **CR YES / ChC YES**; `$is_default_parameterIR`(premise-side k=1 collect)
  **CR YES / ChC YES**(이전 세션 값과 일치 — 무회귀). k≥2 튜플 collect 소비자
  (`$resolve_constraint`, `$callableId_IR`)는 **TIMEOUT** — slice 자체는 작으나
  (13/9룰) typing/constraint relation(`Type_ok`/`TableEntry_ok`/`gen_constraint`)을
  끌어와 critical-pair가 폭발하는 이 심볼들 특유의 tractability 문제. **통합 직전
  커밋(1307c45d)을 stash-빌드해 대조 → `$resolve_constraint` CRC 동일하게
  TIMEOUT**(즉 회귀 아님, 통합 무관). 이 슬라이스들의 의미 보존은 CRC verdict가
  아니라 아래 실행/structural differential MATCH로 뒷받침됨.
  termination은 이 계열이 YES(`$callableId_IR`/`$dom_map`).
- AProVE termination (`rtv.sh`): 통합된 다출력 ex-apply 심볼 `$callableId_IR`
  **YES**, `$dom_map` **YES**(둘 다 튜플-collect/`$iterproj` 포함 슬라이스가
  syntactic-subterm 하강으로 종료 증명 성공 — 통합 인코딩이 종료 친화적임을 실증).
  대상 2심볼 `$invalidate_value`/`$invalidate_headerUnion`은 **MAYBE 유지 =
  통합 전과 동일**(2026-07-17 기록의 AProVE 자동 전략 도구 한계, 회귀 아님).

**동반 수정 — fusion 회귀(`6e96deb9`)로 정의가 누락된 중첩 `$itermap` 3건.**

**증상**: `$itermap-id-field-a-colon-typeIR-field-a-list-2-pair`(및 field-b,
`nameIR-field-colon-value-field`)가 **호출되지만 정의 규칙이 없음** — 분석·실행
양 표면 모두(각 104 선언 / 101 정의). op 선언은 range 복구도 실패해 `-> Val` 폴백.

**시점 확정**: fusion 이전 커밋(main `2f9f8cba`)을 덤프 대조하니 **96 선언 / 96 정의,
미정의 0**. 즉 head-side fusion이 도입한 회귀이고, 이번 통합은 이를 물려받았을 뿐.

**원인 — ctx 스레딩 비대칭**. 문제 형태는 head binder가 fusion 등록된 rule 안의
**중첩 IterE**다([5.05.2-typing-casting.spectec:157](../../specs/p4/5-typing/5.05.2-typing-casting.spectec)
`Cast_expl_neq/structTypeIR`의 `$find_map({ (id_field_a ':' typeIR_field_a)* }, …)`):
- 호출부 — `iterpr_defs`가 내부 전제의 조건을 **ctx 없이** 계산(helper 안에는
  head binder가 없고 spine이 인자로 들어오므로 이게 맞다) → bare 2-spine 이름.
- 정의부 — `iter_helper_defs`의 `visit_prem ctx inner`가 **바깥 rule의 ctx로**
  방문 → 같은 IterE가 fused 1-spine 이름으로 정의 방출.
⇒ 호출된 이름은 정의가 없고, 정의된 이름은 아무도 안 불러 `prune_unused`가 제거.

**영향**: 그 스텝의 `isStuckHead($find_map(…)) = false` 가드가 항상 실패해
**규칙이 발화 불가** — 필드 순서가 다른 struct/header 간 명시적 캐스트
(`Cast_expl_neq/structTypeIR`·`headerTypeIR`와 대응 value 규칙)가 죽어 있었다.
분석 전용이 아니라 실행 표면에도 동일했다.

**수정**: `iter_map_def`/`iterpr_defs`/`visit_exp`/`visit_prem`의 ctx를
`iter_ctx option`으로 바꾸고, **iterated 전제의 inner는 `visit_prem None`으로**
내려보낸다(= 그 위치의 호출부가 컴파일된 것과 같은 레지스트리 상태). 원칙:
**정의 방출은 언제나 그 호출 사이트가 쓴 ctx로 해야 한다** — 심볼이
`spines_of_ids`에 의존하기 때문.

**수정 후 검증**: sanity **OK**(104 선언 / 104 정의, 참조-미정의 0, 양 표면),
op range도 `-> Val` 폴백에서 정확한 `-> List`로 복구. corpus churn은 정확히
누락 정의 6줄 추가 + op 선언 3줄 재선언뿐. impty 골든 무변화, owise 69/3/0 불변,
실행 differential 32표본(struct-cast 후보 6개 추가) **31 MATCH / 0 MISMATCH**
(잔여 1 `issue3671.p4`는 fusion 이전 바이너리로도 not reduced — 별개 기존 문제),
structural differential 2/2 MATCH. CRC `$invalidate_headerUnion`·
`$is_default_parameterIR` 모두 **YES/YES 유지**, termination `$callableId_IR`
**YES**·`$dom_map` **YES**·`$invalidate_value` **MAYBE** — 전부 수정 전과 동일.

### 2026-07-15 — 잔여 CRC MAYBE 재분류 + owise or-gate 진단 + 수정 후보 (미구현)

**작업 격리**: worktree `/home/spectec-core-matchbridge`(branch `match-bridge-ceq`,
HEAD 08dfe4ed, 자체 `_build`). 돌고 있던 differential test(`/home/spectec-core`,
new-rewrite)와 완전 분리. MFE 도구는 gitignore라 main clone 것 참조
(`SPECTEC_MFE_DIR=/home/spectec-core/spectec/tools/mfe`, `MAUDE_LIB`=maude dir) — 별도 프로세스.

**잔여 CRC MAYBE 재분류 (현 브랜치 실측 `verify --symbol`).** verification.md 표의 CRC열은
stale. 옛 category-A owise 9개 재측정 → **6개 이미 YES**(`$join_flow` `$is_lpm_key_prime`
`$requires_priority_prime` `$is_default_parameterIR` `$is_tableDefaultActionProperty`
`$join_text` — hoist_matchers/expand_subty_guards/align_guards/fold-matcher-destructure가 해소).
표의 CRC=MAYBE 8개 중 5개는 `$write_value*`(binenc zero-width 진짜 비합류, 별도 트랙),
1개 `$bin_satplus`(arith sign-split, 재측정 TIMEOUT). **잔여 owise/match MAYBE = 정확히 2개:
`$join_ctk`(2-인자), `$assignop_as_binop`(1-인자, 13생성자중 12매치).** 둘 다 **enum-dispatch**:
ground 특정절 + 서로 다른 rhs + owise or-gate 반사.

**진단 (최소예제로 확정) — 상호배타성 문제가 아니라 CRC의 중첩 or-gate feasibility 불완전성.**
- CRC는 조건을 eq/ceq로 **환원한다**(반례: `ceq h=ff if big=ff`+`eq big=tt` → YES; matcher/or-gate
  단일인자 mini2 → YES). 그러니 "조건 미환원"이 원인이 아님.
- 진짜 원인: owise가 ground 형제와 겹치면 조건이 `or(and(match,match)…)=false`(ground, `true`로
  환원가능)인데, **왼쪽-중첩 `or`에서 참 disjunct가 깊이 묻히면** CRC feasibility 검사가 그걸
  못 보고 임계쌍을 보수적으로 보고. 증거: mini3(2인자 4-way)에서 4개 겹침 중 참 disjunct가
  **맨 바깥**인 `f(cb,cb)`만 생존; **or-gate 순서만 바꾸면(mini3b) 생존 ccp 집합이 바뀜** →
  진짜 비합류 아님, 게이트 인코딩 아티팩트.
- 그래서 사용자 제안 **부정 ceq `match_B=false if match_A=true`는 못 고침**: (1) 이 문제를 전혀
  안 건드림, (2) 양방향 ceq가 조건평가 상호재귀로 **CRC 폭발**(Val/Ctk/unfold 무관, timeout/OOM),
  (3) 막는 ccp 가설이 `and(…)=true`라 원자 match가 없어 애초에 발화도 안 함.

**수정 후보 실측 (전부 실제 MFE):**
- **[유력] complement 열거**: enum-dispatch owise를 or-gate 대신 **미매치 생성자 튜플마다 ground
  fall-through 절**로 반사 → 모든 절 ground·disjoint → 임계쌍 0 → YES. `$join_ctk`(DYN관련 5절)
  **CRC YES**, `$assignop`(plain-`=` 1절) **CRC YES**. 국소적·비폭발. 실행표면 무변경(분석 전용).
  미매치수 = ∏(arg타입 생성자수)−매치수 → **size guard** 필요(대형/고arity는 or-gate 폴백).
  v1 스코프: 모든 형제 arg가 **nullary 생성자**인 순수 enum-dispatch(두 대상 다 해당).
- **[기각] `or [assoc comm]`**: AC 매칭이 참 disjunct를 위치무관 발견 → `$join_ctk` **YES**(진단 확증).
  그러나 `$assignop` 12-way 게이트에서 **AC 단일화 폭증, 850s 내 verdict 없음**(baseline은 <420s에
  MAYBE 완주). + `or` 전역 변경이라 타 슬라이스 회귀 위험. 실전 부적합.

**[완료, 2026-07-16] complement 열거 정식 구현 — `Reflect.owise`(pipeline은 여전히 분석
전용).** v1 스코프를 조건부 형제까지 확장해서 구현(사용자 지시): 인자 위치를 **enum**(형제 중
누군가 nullary 생성자를 놓는 자리, 선언 타입이 nullary-only variant로 열거 가능해야 함)과
**pass-through**(전 형제가 변수)로 분류, enum 위치의 곱집합을 튜플별로 순회 —
- 무조건 형제가 커버 → owise 도달불능, 절 생략
- 조건부 형제만 커버 → ground-head 절 + **형제 가드 부정의 논리곱**(형제당 `g=false` 1개,
  or-gate 아님) — 실측 대상 2건은 형제가 전부 무조건이라 이 경로 미시험(v2 설계로 남김,
  todo.md에 명기)
- 아무도 안 커버 → 순수 ground fall-through (v1 원안)

`max_complement=16` size guard. `sibling_guard`에서 조건-반사부를 `sibling_conds_guard`로
추출해 재사용(리팩토링 단계에서 p4 corpus `--ctrs`/stderr **byte-identical** 확인 후 기능
추가). `reflect.ml`의 owise 단계를 `Array.mapi`(1규칙→1규칙)에서 `List.concat(List.mapi…)`
(1→N)로 교체.

**실측 결과(전부 실제 MFE, `verify --symbol … --timeout 900`):**
- `$join_ctk` **CRC YES**(5절, 전부 무조건 — 7/15 수동 실측 재현), `$assignop_as_binop`
  **CRC YES**(1절, 전부 무조건 — 동일 재현).
- p4 corpus 전체에서 새로 열거된 심볼은 예상외로 `$join_flow`도 포함(3절, 무조건) — **회귀
  확인: YES 유지**(기존에도 YES였던 대상). impty 대조군 `$lookup` **YES**(환경 정상).
- 반사 수 보존: `72 owise rule(s)` → **`69 reflected + 3 complement-enumerated`, kept 0**
  (7/15 표에서 인용한 `71/1`은 stale — 실측 기준 정확한 불변식은 `72/0`).
- p4 corpus `--ctrs` diff: 위 3심볼 절만 변화(전부 `ceq…or(...)=false` → `eq` N개), 그 외
  0. 실행 표면(`rewrite` 출력) sha256 편집 전후 동일 — 분석 전용 패스라 자동 보장, 실측으로도
  재확인. impty 골든(`spec.ctrs`/`spec.maude`) 둘 다 diff 0.

**v2 후보(미구현, 실측 미검증)**: 조건부 형제의 부정-누적 경로. 사용자가 "실측은 안 돼도 구현은
해달라"고 명시해 코드는 포함했으나, 현재 corpus엔 이 경로가 발화하는 MAYBE 대상이 없어 CRC
개선 효과를 검증할 표본이 없다. **다음에 owise CRC MAYBE가 새로 나타나고 그 형제가 조건부면
가장 먼저 확인할 지점** — `sibling_conds_guard`가 이미 재사용 가능한 형태로 분리돼 있음.
guarded 절 발생 시 회귀 나오면 자격 검사에 "전 형제 무조건" 요구를 되살리는 최소 revert로
대응(설계는 위 커밋 참고).

- [x] **`Exp_map` 재생성** (done) — IL 얕은 traversal(`map_subexps`/`subexps`/
  `exps_of_prem`). `Defunctionalize`가 사용.
- [x] **`Simplify` = identity** (done) — 이 프로젝트는 단순화를 하지 않음. `Prem_env`도
  재구현하지 않음(그것만 먹이던 엔진이라 불필요).
- [x] **`To_ctrs.of_spec`** (심장; `rewrite` 브랜치에서 포팅, done) —
  `prelude` + `defs_of_typ` + `term_of_exp` + `conds_of_prem` + `rules_of_def` +
  반복/subtype 헬퍼 + `prune_unused`. 결과 레코드 `{ R.vars; rules }`. `Prem_env`
  의존(`find_rel_in_spec`)은 로컬 인라인. impty/base에서 분석/실행 둘 다 실행됨.
  ✅ **`~scalars:Native`는 이제 생성 시점 직접 방출**(`Ctrs_term`의 mode-aware
  leaf 빌더 + `~scalars` threading; post-fold 제거) — 아래 "Native 직접 생성
  리팩토링 (B)" 완료.
- [x] **`To_ctrs.var_type_hints`** (포팅, done) — `VarE` note에서 변수 narrow 타입
  복원(To_maude용). 헬퍼 `collect_var_types`/`collect_prem_var_types`/
  `resolve_var_types`도 함께 포팅.
- [x] **검증: impty/base CTRS 골든** byte-identical (done) — `specs/impty/base/spec.ctrs`
  고정(`rewrite --ctrs` 덤프). 기본 `rewrite`는 실행 모듈을 내므로 분석-CTRS 골든은
  `--ctrs` 경로. 실행(Native) 모듈 골든은 아직 미고정(원하면 `spec.maude`로 별도 핀).
- [x] **`Mfe` calibration** (done) — `mfe.ml`을 실측 프로토콜로 재작성(entry `src/mfe.maude`,
  `MAUDE_LIB`, stdin 파이프, `(select tool …)`+`(check …)`, EOF flood→streaming
  SIGKILL, verdict 토큰; [tools/mfe/README.md](../../tools/mfe/README.md) Calibration).
  impty/base에 **per-symbol slice**(`verify --symbol`, `rewrite --ctrs --symbol`)로
  CRC+ChC 적용한 결과:

  | symbol | CRC | ChC | 비고 |
  |---|---|---|---|
  | `$lookup` | YES | YES | owise + 서로소 `match_*` 가드로 합류 |
  | `Check_expr`/`Check_command`/`Check_prog` | YES | YES | |
  | `Eval_expr`/`Eval_command` | YES | YES | |
  | `Eval_prog` | **MAYBE** | YES | `ccp SPEC226` 미합류(아래) |
  | `Run_prog` | TIMEOUT | TIMEOUT | 전체-reachable, critical pair 폭증(>150s) |

  - **ChC가 전부 YES**인 이유: impty/base는 `rl`/`crl`이 0개(모든 relation이
    input-moded → 등식)라 coherence가 vacuous.
  - **`Eval_prog` 비합류는 진짜이며 알려진 한계의 발현**: 규칙
    `Eval-prog(command) = env if Eval-command(nil, command) = env`의 결과 `env`가
    **전제로만 묶이는 RHS 자유변수**라, CRC가 같은 불투명 `Eval-command(nil,command)`의
    두 증인 `env`/`#env#`를 합류시키지 못함(ccp SPEC226). 상수 RHS인 `Check-prog`
    (`= true if … = tenv`)는 자유변수가 안 생겨 YES. 실행 표면(`to_maude`)은 이
    전제를 `:=`/`=>` 조건으로 내보내 문제없음 — 분석 표면의 join-condition `=` 근사가
    원인(같은 뿌리: "variable used before bound" 경고). **번역 버그 아님.**
  - **참고**: `$lookup`(owise 보유)이 YES인 건 owise 형제 절들이 서로소 `match_*`
    가드를 써 CRC가 임계쌍을 infeasible로 처리하기 때문. owise 자체가 안전한 건
    아니다(아래 p4 sweep의 2번째 원인 참조).

  **p4 spec sweep (per-symbol, slice 1–200 규칙 150개; degenerate 9개; >200규칙
  415개는 전체-시스템급 → TIMEOUT 예상이라 미실행).** `verify --list-symbols --sizes`로
  슬라이스 크기를 한 번에 구해 tractable한 것만 MFE에 돌림. 아래는 `fold_premise_binders`
  적용 후 **전체 150개 재-sweep**(authoritative; `--timeout 60`, `-P 3`):

  | CRC verdict | n | 비고 |
  |---|---|---|
  | YES | 125 | (fold 전 104 → +21; `$bin_mod` 포함 — mod overlap 임계쌍이 막지 않음) |
  | **MAYBE** (비합류 아님) | 18 | 세 분류 — 아래 |
  | TIMEOUT | 7 | `$write_*_from_bits` 6종 + `$assignop_as_binop` — 비트쓰기 임계쌍 폭증 |
  | (규칙 0개 degenerate) | 9 | `$find_overloaded*`/`$match_overloaded_*` 등 — 정의 규칙 없음, N/A |

  **남은 18 MAYBE의 가드별 분류**(전부 결정적 함수, 진짜 비합류 0 — CRC 불완전성):
  - **A. owise 중첩 (9)**: `$is_lpm_key_prime`·`$requires_priority_prime`·
    `$is_default_parameterIR`·`$join_flow`·`$join_ctk`·`$is_tableDefaultActionProperty`·
    `$optional_annotation_…_prime_prime` (+ 호출만 하는 `$is_lpm_key`·`$requires_priority`).
    가드가 전부 `match_*`/텍스트 등식 → **negation wall 없음** → 아래 (A) owise-보완으로
    깔끔히 해소 가능.
  - **B. owise-없는 순수 `match_*` 케이스-분기 (2)**: `$un_op`·`$inherit_i` → 아래 (B)
    discriminator head-패턴 폴드로 바로 안전하게 YES.
  - **B′. subty 섞인 케이스-분기 (7)**: `$flatten_constOpt`·`$tableCustomName`·`$name`·
    `$prefixedTypeName`·`$prefixedNonTypeName`·`$invalidate_value`(+`$invalidate_headerUnion`).
    `subty_*`는 RHS가 리터럴이 아니라 discriminator가 아님 → 단순 폴드로 **안 됨**.
    subty 절 disjointness 증명이라는 별도 메커니즘 필요(미설계).
  ⇒ (A)+(B)로 11/18 해소 가능, 나머지 7은 subty-disjointness 신규 작업.
  **(2026-07-07: B′ 7개 전부 해소 — `Reflect.expand_subty_guards`, 아래 권장 순서
  섹션의 2026-07-07 항목 참조.)**

  ChC는 (출력이 나온 모든 경우) YES — p4 분석 모듈도 `rl`/`crl` 0개라 vacuous.
  **비합류(MAYBE) 33건은 두 원인으로 갈린다(둘 다 분석 표면 근사, 번역 버그 아님 —
  실행 표면은 `:=`/`=>` + owise 보완으로 처리):**
  1. **전제로만 묶이는 RHS 자유변수** (impty `Eval_prog`과 동류; 지배적):
     `$dom_map`/`$codom_map`(`= set{K} if $unzip_K(it)=K`, ccp SPEC4),
     `$empty_store`(`= store if $empty_map=store`), `$ctk_of_typedExpressionIR`/
     `$type_of_*`(`= ctk if it = variant(..ctk..)` 구조분해 바인딩) 등.
  2. **owise 중첩** (todo.md P2 `otherwise` 예측대로 발현):
     `$is_lpm_key_prime` = `ccp SPEC1: false = true if text = "lpm"` — 절1
     `= true if text="lpm"` vs 절2 `= false [owise]`인데 **Maude CRC가 owise를
     임계쌍 생성에서 무시**해 두 절이 충돌. 가드가 raw 등식/구조라 CRC가 owise 보집합을
     disjoint로 못 봄(`$lookup`은 가드가 `match_*`라 회피).

  **원인 1(자유변수)을 `fold_premise_binders`로 해소 (done).** 분석 파이프라인
  (`Pipeline.ctrs_of_spec`)에 마지막 패스를 추가해 전제로만 묶이는 변수를 규칙에 도로
  접는다([rewrite_system.ml](rewrite_system.ml)): (a) 관계/함수 **출력** 바인더
  `(prod, v)`(v 비-head)를 rhs/조건에 **인라인**, (b) **순수 접근자** 구조분해
  `(v, K(..))`(v head-bound, 다른 조건엔 안 나옴)를 **head 패턴으로 폴드**. iteration
  helper(`$iterapply`/`$itercollect`/`$unzip`) 안의 재귀 바인더도 동일 처리. **분석
  표면 전용**(실행 모듈 byte-identical; `To_maude`는 `:=` 유지). **수술적(surgical)**
  으로 제한 — 가드 절(head 변수가 `match_*`/owise 가드에도 쓰이는 `$lookup` 등)을 폴드하면
  CRC가 의존하던 disjointness 가드가 사라져 owise 중첩이 노출돼 **YES→MAYBE 역행**하기
  때문(aggressive 변형은 impty 6개를 실제로 역행시켜 폐기). 효과: impty·기존-YES 무역행,
  p4의 33 MAYBE 중 **19개가 YES로**(출력-바인더 + 순수 접근자, iteration 포함;
  `$dom_map`/`$ctk_of_*`/`$type_of_*`/`$set_priorities_..._prime` 등). 남은 14개는
  **원인 2(owise)** 와 일부 multi-clause — owise-보완 별건.

  **남은 14 MAYBE의 두 분류와 추가 해소안(미구현).** 잔여는 전부 "진짜 비합류"가
  아니라 CRC가 임계쌍 전제의 disjointness/unsatisfiability를 못 푸는 불완전성이다.
  번역 단계에서 disjointness를 **구문적으로 노출**해 주면 대부분 YES로 떨어진다:

  - [x] **(B) owise-없는 케이스-분기 절의 head-패턴 폴드 — 구현 완료 (2026-07-03).**
    `$un_op`/`$tableCustomName`류는 동일 head `$un-op(unop,value)` 위에
    `if match_X(unop)=true` 가드 4절이라 CRC가
    `$un-bnot(value)=$un-lnot(value) if match-tilde(unop)=true /\ match-bang(unop)=true`
    같은 임계쌍을 만들고 두 match가 배타적임을 못 본다.

    **구현 계획(새 엔진 불필요 — 기존 `fold_premise_binders` 재사용).** 처음엔
    CRC-facing 전용 "discriminator fold" 패스를 새로 설계하려 했으나, 조사 결과
    `Rewrite_system.fold_premise_binders`([rewrite_system.ml:264](rewrite_system.ml#L264))가
    이미 `v = K(fresh..)`형 구조적 동등식을 만나면 head로 접는 로직(+ 안전 게이트:
    `is_ctor_pattern`/"다른 조건에 안 쓰임"/"LHS에 1회만 등장")을 갖고 있음을
    발견했다 — `MatchE`(matches) 가드만 `cond_of_match`
    ([to_ctrs.ml:840-851](translate/to_ctrs.ml#L840-L851))가 항상
    `match_sym(subj)=true`(불투명 술어) 형태로 내보내서 이 기존 메커니즘을 못
    탈 뿐이다. 그래서 새 엔진 대신, `match_sym(v)=true`를 동등식
    `v = K(fresh..)`로 **재철자**만 해주는 작은 패스(`Reflect.hoist_matchers`,
    신설)를 `fold_premise_binders` 바로 앞에 끼워 넣고, 나머지(head-bound 여부,
    안전성, 실제 치환)는 전부 `fold_premise_binders`에 위임한다.

    **`IterPr` 내부에 중첩된 `IfPr`도 별도 재귀 스캔 없이 자동으로 커버된다** —
    두 패스 다 `to_ctrs` 이후의 **flat한 CTRS rule 리스트**를 균일하게 훑기
    때문에, `iterpr_defs`([to_ctrs.ml:862-974](translate/to_ctrs.ml#L862-L974))가
    만드는 반복 헬퍼의 step rule도 그냥 "리스트의 규칙 하나"라 캡처 변수
    (`iter_captured`/`captured_fvs`, [to_ctrs.ml:62-138](translate/to_ctrs.ml#L62-L138))든
    원소 변수든 같은 경로로 처리된다(헬퍼 심볼당 step rule이 1개뿐이라 sibling
    경쟁이 없어 안전 — 가드가 head로 옮겨가도 발화 조건 집합이 동일).

    - **범위**: `CaseP`(임의 arity, variant-case 역조회) + `OptP` `\`Some`/`\`None` +
      `ListP` `\`Cons`/`\`Nil`. 옛 `Prem_env.reconstruct_pattern`이 companion-let
      충돌을 우려해 배제했던 Some/Cons까지 포함 — `fold_premise_binders`의
      "다른 조건에 안 쓰임" 게이트가 그 충돌을 이미 구조적으로 막아준다는 게
      이번 결론(실측으로 확인 필요, 아래 검증 참조). `ListP` `\`Fixed n``(길이
      체크, `len(subj)=n` 형태)은 코드 경로가 달라 이번엔 보류.
    - **배치**: `translate/reflect.ml`에 새 함수 `hoist_matchers`
      (`~orig:spec -> Rewrite_system.t -> Rewrite_system.t`)로 추가 — `owise`가
      이미 만들어 둔 `tables`/`build_tables`/`variant_case`/`matcher_type`/
      `fresh_vars` 재사용. 매처 심볼 → (타입, mixop, 필드타입) 역테이블은
      `build_tables`의 `ctor_types` 구성 방식을 그대로 거울처럼 뒤집어서 만든다
      (`T.match_sym ty mixop`를 타입 순회하며 계산 — 문자열 파싱 없이 안전).
      `match_some`/`match_none`/`match_cons`/`match_nil` 4개는 고정 이름으로
      직접 인식(각각 fresh 0/1/0/2개).
    - **파이프라인**: `pipeline.ml`의 `ctrs_of_spec`에서 `Gensym.thread` 직후,
      `Rewrite_system.fold_premise_binders` 바로 앞에 `Reflect.hoist_matchers`
      삽입. `maude_system_of_spec`(실행)에는 절대 넣지 않음 — 기존
      owise/fold_premise_binders와 동일하게 분석 전용.
    - **범위 밖(유지)**: subty 섞인 (B′) 7개(`$flatten_constOpt`/`$tableCustomName`/
      `$name`/`$prefixedTypeName`/`$prefixedNonTypeName`/`$invalidate_value`
      (+`$invalidate_headerUnion`))는 이 fold로 안 풀림 — subty totality가 만든
      case-membership 정보 기반 multi-way clause 전개가 별도로 필요(멤버
      생성자 수만큼 클로즈 복제, 단일 substitution이 아님 — 위 subty totality
      절 참조). `IterPr` 원소 자체의 매치로부터 **스트림 전체 모양을 재구성**하는
      것(옛 `Prem_env.env_of_prem`의 `IterPr` 승격 트릭, `IterE`로 감싼 pair
      생성)도 범위 밖 — 이건 head disjoint화가 아니라 premise 단순화 성격이라
      지금 기록된 MAYBE 원인 어디에도 필요하지 않다.
    - **검증 계획**: impty 골든(`spec.ctrs`) byte-identical(해당 케이스 없어
      no-op) → `verify --symbol '$un_op'`/`'$inherit_i'` MAYBE→YES 확인 → 기존
      YES 표본 재검(회귀 없는지, 특히 Some/Cons 확장 리스크) → 실행 표면
      (`spec.maude`, `run --p4 --check-p4`) 무영향 확인.
    - **구현 결과 (2026-07-03).** `Reflect.hoist_matchers`를 계획대로 추가하고
      `pipeline.ml`의 `ctrs_of_spec`에서 `Gensym.thread` 직후 · `fold_premise_binders`
      바로 앞에 배선. **계획에서 한 가지 실측으로 드러난 보정**: naive하게
      "`match_K(subj)=true`는 전부 재철자"로 구현했더니 impty 골든이
      **비-no-op**으로 깨졌다 — `Check-command`/`Check-expr`/`Eval-expr`/
      `Eval-command`/`$lookup`처럼 SpecTec 엘라보레이터가 `matches` 가드(바인딩
      없음)와 필드-바인딩 `let K(x,y)=subj` 구조분해를 **별개의 두 premise**로
      내보내는 경우, 재철자된 `subj=K(fresh..)`가 기존 `subj=K(x,y)`와 같은
      `subj`를 다시 언급해 서로의 "다른 조건에 안 쓰임" 게이트를 막아버려
      **아무것도 안 접히면서** opaque `match_*` 조건만 죽은 fresh 변수투성이
      동등식으로 바뀌는 순수 노이즈가 됐다(=fold는 여전히 안 되는데 텍스트만
      나빠짐). 고쳐서, `hoist_matchers`는 이제 **subj가 bare 변수이고 같은
      rule의 다른 어떤 조건에도 그 변수가 안 나올 때만** 재철자한다 — 이
      가드가 있으면 companion-let이 있는 절은 원래 `match_*` 조건을 그대로
      두고(=이전과 동일 출력), companion이 없는 절(discriminator만 있는 경우,
      즉 (B)가 실제로 겨냥한 경우)만 재철자→폴드된다.
      - **impty 골든 갱신(byte-identical 아님, 의도된 3건 개선)**: `$lookup`의
        nil 베이스케이스, `Check-command`/`Eval-command`의 SKIP 케이스가
        head-패턴으로 접혔다(전부 필드 없는 nullary 케이스라 companion
        destructure가 애초에 없음 — "해당 케이스 없어 no-op"이었던 원래
        가정이 부분적으로 틀렸던 것; impty에도 대상 케이스가 3건 있었다).
        나머지(Check-expr/Eval-expr/필드 있는 $lookup cons 케이스 등)는
        companion-let 때문에 게이트대로 스킵 — 출력 불변. `$lookup`의 owise
        가드에서 nil-sibling 항이 `match-nil(..)`→`eqg(..,nil)`로 바뀌었는데,
        이는 sibling1의 head가 이제 리터럴 `nil`이라 owise reflection의
        `ptest`가 (더 이른 순서의) "ground 패턴은 eqg 한 방" 분기를 타기
        때문 — 유니파이어 하에서 `eqg(nil,nil)⇒true`로 즉시 반사적으로
        붕괴하므로 기존 hypothesis-rewriting 경로보다 오히려 더 직접적이고
        안전(critical-pair discharge 의미는 동일). `spec.ctrs` 갱신,
        `spec.maude`(실행 표면)는 무변화 확인(hoist_matchers는 분석
        파이프라인에만 배선).
      - **MFE 실측(`$un_op`/`$inherit_i` MAYBE→YES 확인, `$is_lpm_key_prime`
        회귀 재검, impty `$lookup` 대조군) — 이번 세션에서 완주 못 함, 환경
        차단.** `verify --symbol '$lookup' spec.spectec`(과거 ~1.4s로
        기록됐던 대조군)가 `--timeout 900`(15분)에도 TIMEOUT. `git stash`로
        **코드 변경을 뺀 베이스라인**에서 동일 슬라이스를 재현해도 60s
        타임아웃까지 동일하게 TIMEOUT → **이 세션의 코드 변경과 무관한
        기존(pre-existing) 환경 문제**로 확인(회귀 아님). Maude 바이너리
        자체는 정상(단순 `reduce 1 + 1 .` ~0.17s). MFE 프로토콜을 수동으로
        재현해 관찰한 결과, 모듈 로딩 직후 "variable used before bound"
        경고 3개 세트가 30초 지점과 90초 지점에서 **글자 그대로 반복
        출력**됨 — CRC 내부가 critical pair마다 모듈을 재적재/재선언하는
        것으로 보이며, [Maude 배치 폴백 슬로우다운] 메모의 "50k줄 모듈
        재내부화" 현상과 결이 비슷하다(다만 이건 MFE `check`의 내부 동작이라
        `check_diff_p4.sh`와는 별개 경로). [MFE 환경 슬로우다운] 메모의
        "심볼당 ~4분"보다도 더 나빠진 상태로 보임 — 재측정 전까지 MFE
        확인은 이 환경에서 사실상 막혀 있다고 간주. **다음 세션에서 할 일**:
        환경(WSL 메모리/디스크) 점검 또는 다른 머신에서 `verify --symbol
        '$un_op'`/`'$inherit_i'`/`'$is_lpm_key_prime'`/impty `'$lookup'`
        재실행해 MAYBE→YES 전환과 무회귀를 확인.
        **(2026-07-07 해소: 차단은 옛 WSL 환경 고유 문제 — 현 dev 컨테이너에서
        `$lookup` 1초 YES/YES, `$inherit_i` YES/YES. `$un_op`은 슬라이스 크기
        문제로 재분류. 권장 순서 섹션의 2026-07-07 항목 참조.)**
  - [x] **(A) MFE 입력에서 owise 규칙 drop (구현·검증 완료 — 옛 "owise-보완 번역"안 대체).**
    `$is_lpm_key'`/`$requires_priority'` 류는 `= true if text="lpm"` + `= false [owise]`인데
    CRC가 owise를 임계쌍 생성에서 무시해 `false=true if text="lpm"` 충돌. **단순 confluence
    게이트엔 owise 규칙을 그냥 빼고 검사하면 건전하다:** owise는 sibling이 안 맞을 때만
    발화하므로 sibling과의 overlap은 **구조적으로 infeasible**(허상 임계쌍)이고, op당 owise
    1개라 owise끼리도·다른 head와도 안 겹친다. ⇒ owise를 빼면 그 허상만 사라지고 진짜
    비합류(sibling overlap)는 남아 **false YES(버그 은폐) 불가**; 최악이 false MAYBE(합류에
    owise 스텝이 필요할 때 — owise RHS가 상수라 실측상 안 생김)라 게이트엔 보수적·안전.
    **`.ctrs` 정본은 건드리지 않는다**(termination 등 다른 용도가 owise를 필요로 함) —
    drop은 [mfe.ml](mfe.ml) `check`에서 `Rewrite_system.drop_owise`로 **MFE에 넘기는
    시스템에만** 적용(`ctrs_of_spec`/골든/slice-size 집계는 owise 유지). **negation wall
    완전 우회**(보집합을 항으로 표현 안 함). **검증(실측, 무회귀):** owise-9 중 **6개 YES**
    (`$is_lpm_key'`·`$is_lpm_key`·`$requires_priority'`·`$requires_priority`·
    `$is_default_parameterIR`·`$join_flow`). 남은 3은 owise 탓이 아니다 — `$join_ctk`는
    자기 4 `match_*` 절(같은 head, (B) 필요), `$is_tableDefaultActionProperty`/
    `$optional_annotation_…'`는 슬라이스가 B′ 의존(`$tableCustomName`/`$name`, subty
    overlap)을 끌어옴. impty/base 골든 byte-identical, 기존-YES 무회귀. **전체: 18 → 12 MAYBE.**
  - [x] **subty totality — 사용-기반 false-보완 (구현 완료 2026-07-02; B′의 전제 조건).**
    `subty_<T>`가 positive-only(비멤버 무규칙→stuck)라 분석 표면에서 `~(e <: T)`가
    죽은 조건이 되고 B′ 임계쌍의 disjointness를 CRC가 볼 수 없었다. **구현:**
    모든 `SubE(e,T)` 사이트에서 (타겟 T, 주어 정적 타입 S=e.note) 쌍을 수집해
    (alias unwrap + tuple/list/opt 원소 co-descent), 둘 다 variant면
    `subty_T(ctor_c(x..)) -> false` 를 c ∈ cases(S)\cases(T)(키 = origin+mixop+arity,
    `variant_sym` 키잉과 동일)별로 방출 — [translate/to_ctrs.ml](translate/to_ctrs.ml)
    `sub_complement_defs`(+ 최상위 `typcases_of`/`unalias`), `of_spec`에서
    `sub_helper_defs` 뒤에 연결. interp.subtyp(interp/eval_il/interp.ml:210-258,
    **total boolean**)과 같은 의미론 — negation-as-failure가 negation-as-false-value로.
    **of_spec 공통 레벨**(분석+실행; 실행에선 To_maude subty owise complement가 주던
    값과 동일해 명시 규칙이 선점, complement는 잔여 head 흡수용으로 유지).
    - 산출: p4 보완 규칙 5425개(무조건·상수 RHS라 슬라이스에 자기 자신만 추가);
      origin-vs-mixop 충돌 경고(P3급) 0건; impty 골든은 elaborator 삽입 SubE
      (`literal`/`id` ⊂ `expr`) 보완 11줄 순수-추가로 재생성; impty 실행 정상.
    - **MFE 재검 결과 (timeout 360s, 2026-07-02):**
      | symbol | CRC | ChC | 비고 |
      |---|---|---|---|
      | `$is_lpm_key_prime` (대조군, byte-identical) | YES | YES | **무회귀** (~4분 소요) |
      | `$flatten_constOpt` (B′) | MAYBE | YES | totality만으론 미해소 — 아래 |
      | `$invalidate_value` (B′) | TIMEOUT | TIMEOUT | 360s도 부족(149규칙+느린 환경) |
      | `$tableCustomName` (B′) | TIMEOUT | TIMEOUT | 동일(255규칙) |
      (표본 중단 — 잔여도 같은 패턴 예상; 환경 복구 후 재측정할 것)
    - **⚠️ 환경 변화 — MFE 벤치 기준 무효화 (2026-07-02):** 7/1 sweep 당시 60s면
      되던 심볼이 지금은 **심볼당 ~4분**(byte-identical 대조군으로 확인 — 코드 변경과
      무관). WSL RAM 16→7GB 등 환경이 느려짐. 이후 verify/sweep은 `--timeout 360`+
      이상을 기본으로; 과거 TIMEOUT 표는 재측정 전까지 환경-보정 없이 비교 금지.
    - **B′가 totality만으로 안 풀리는 이유(실측 확인):** 임계쌍의 두 절이 같은
      head `f(x)`라 조건 `match_*(x)=true /\ subty_*(x)=true`가 **변수 x에 대해
      심볼릭**하게 남고, CRC는 x를 생성자 케이스로 분할하지 않는다 — 보완 규칙은
      x가 구체 생성자일 때만 발화. ⇒ **B′의 실제 해법은 (B) discriminator fold의
      subty-가드 확장**: subty가 total해진 지금 `subty_A(x)=true` 가드의 true-집합이
      구문적으로 정확히 열거되므로(멤버 케이스 = true 규칙), 그 절을 멤버 생성자별
      head 패턴으로 안전하게 펼칠 수 있다(`$flatten(EMPTY)=none` /
      `$flatten(CONST)=some(CONST)` — head 서로소 → 임계쌍 소멸). totality가 이
      폴드의 건전성 근거. (B) 구현 시 match-discriminator와 subty-discriminator를
      함께 다룰 것.
  - [x] **subty false-보완의 실행 무회귀 표본 (확인 완료 2026-07-02).** suite 균등
    추출 47 + casting-heavy(cast-call/issue447-2-bmv2/key-bmv2) = **49개 표본,
    13개 단위 serial 배치 4회, 49/49 `result: MATCH`** (Phase B verdict + Phase D
    결과-VALUE 일치; MISMATCH/FAIL/TIMEOUT 0). impty 실행도 8/8 정상
    (skip/arith/assign/bool_ops/hello/ite/loop/shadow). 내부화 비용은 배치당
    ~60s(13파일, 5,425 eq 추가 후; 느려진 환경 기준)로 상각 유지. 같은 검증에서
    **stale 실행 골든 발견·재생성**: `impty/base/spec.maude`가 matcher-sort 협소화
    (48e59d5f)와 subty 보완(5a406ccc)을 반영하지 않은 채 남아 있었다 — diff는
    정확히 그 두 변경(match-* 34줄 sort 변경 + subty-* 11줄 추가)뿐임을 분류 확인
    후 재생성.
  - [x] **owise 반사 + judgment 반사 (구현 완료 2026-07-02, 커밋 752c82f8 — (A) drop을
    충실 인코딩으로 대체).** 분석 전용 `ctrs_of_spec` 최종 패스
    [translate/reflect.ml](translate/reflect.ml). MFE CRC의 실측 메커니즘(조건을
    가설로 서로 재작성, 생성자 narrowing 없음 — 미니모듈 T1–T5로 검증)에 맞춰:
    - **owise**: 반사 가능한 owise 절의 조건에 `or(g_1..g_k) = false`(선행 형제
      적용가능성의 or) 추가 + owise 플래그 클리어. g는 형제의 **번역된** head
      패턴(match/proj, 단락 `and` 뒤)과 조건에서 구성 — 같은-subject 정렬이
      가설 접힘의 전제. p4 **51/72 반사**, impty `$lookup` 반사(골든 재생성).
    - **judgment 반사**: 부정되거나 owise 형제에 나오는 무출력 judgment R에
      `holds_R(xs) = or(g_1..g_n)` **무조건 규칙 1개**(임계쌍 0) 생성, 시스템
      전체에서 `R(in)=true/false` 조건을 `holds_R`로 재철자(양성 포함 — CRC에겐
      R과 holds_R이 무관 심볼이라 통일해야 정렬; 무출력 R의 자기 규칙은 전부
      RHS true라 검증력 손실 없음). no-binding IterPr은 totalized and-fold
      `holds_$iterall..`(+길이불일치 false). p4: `Type_alpha`·`ParameterType_alpha`
      + iterall 15개; **`Type_alpha:/` 죽은 절 8개가 충족 가능해짐**.
    - 잔여 21 kept(게이트, stderr 사유 로그): 수집형 iteration 헬퍼
      (`$itercollect`/`$iterapply` — 아래 후속 확장; `Cast_impl_neq`→`Cast_impl`→
      `$cast_unary`/`$cast_binary`가 이 체인에 막힘), gensym-threaded `$subst_*` 5,
      ambiguous matcher 1. `drop_owise`는 이 잔여분 fallback으로 유지.
      **(2026-07-03 후속으로 21→6까지 반사 확장됨 — 아래 "owise 반사 확장" 항목
      참조; 이 21의 스냅샷은 그대로 둠, 최신 잔여는 아래 항목의 6개.)**
    - 실행 표면 byte-identical; support(match/proj) 규칙은 prune 이후라 필요 시
      재생성(트랜잭션, 미사용 필터). `holds_*`는 BoolV 시그니처(Maude_sorts).
    - **가드 등호는 `eqg` (a7d85f94) — 구조적 `eq` 금지.** 가드에 `eq` 하나만
      넣어도 head-기반 slice가 전 타입 eq 패밀리(47,213규칙)를 끌어와
      `$is_lpm_key'` 슬라이스가 4→47,217규칙으로 폭증, CRC가 어떤 timeout으로도
      안 끝났다(div_int 교훈의 재발). CRC가 필요로 하는 건 대각뿐 — 임계쌍
      unifier 아래서 형제 가설이 양변을 문자 그대로 같은 항으로 재작성하므로
      (ground 리터럴/비선형 재출현/`t = t'` 모두) 비선형 규칙 하나
      `eqg(x, x) = true`로 전부 접힌다. 대각 밖 stuck = 보수적 MAYBE(false YES
      불가). 슬라이스 3규칙 복귀.
    - **MFE 실측 (2026-07-02, timeout 600s, 느려진 환경):**
      | symbol | CRC | ChC | 비고 |
      |---|---|---|---|
      | `$is_lpm_key'` (p4, 가드 포함 3규칙) | **YES** | **YES** | drop_owise가 규칙을 빼고 얻던 YES를 **충실 인코딩으로 재현** |
      | `$join_ctk` (p4) | MAYBE | YES | 예측대로 — 잔여 쌍은 owise가 아니라 **형제끼리** 같은-subject 다른-matcher(B의 몫: discriminator fold) |
      | `$lookup` (impty, 18규칙) | TIMEOUT@1800s | — | eq 폭증 아님(슬라이스 정상). 3규칙이 4분 걸리는 현 환경에서 조건부(unzip/itermap) 18규칙 슬라이스의 CRC 비용으로 추정 — 단, 현 환경 기준선이 없어(7/1의 YES는 빠른 환경 + drop_owise 측정) 미확정. 환경 복구 후 drop_owise 대조와 함께 재측정할 것 |
      150-슬라이스 재-sweep은 환경 복구 후(심볼당 4분+ 현재로는 ~10시간) 진행할 것.
      게이트의 안전성상 어떤 결과든 false YES는 불가(반사 가드는 discharge를
      돕기만 하고, 실패 시 보수적 MAYBE).
  - [x] **owise 반사 확장 — 수집형 IterPr 성공 반사 (구현 완료 2026-07-03).**
    reflect 게이트가 skip하던 `$itercollect`/`$iterapply`를 위한 후속
    [translate/reflect.ml](translate/reflect.ml). 재구성 없이 형제의 바인딩
    조건(`$itercollect_S(args), ys`)은 그대로 두고 세 메커니즘으로 연다:
    1. **성공 반사 생성기** `gen_itercollect_holds`/`gen_iterapply_holds`
       (`gen_iterall_holds`와 같은 골격, base→`true`/step→and-fold; 공유
       스핀-불일치 로직은 `iter_spine_mismatches`로 분리). `$itercollect`는
       step rhs가 `cons(collected, S(rec))`/`some(collected)`라 재귀 인자를
       cons의 둘째 항에서 뽑고 collected는 버림(성공 반사엔 무의미) —
       step 조건은 `gen_iterall_holds`처럼 그대로 반사. `$iterapply`는 절 자체가
       무조건(step 조건 없음, 원소가 relation call 항 그대로)이라 g_inner를
       `holds_<call 심볼>`로 직접 재구성.
    2. **전역 성공-테스트 삽입** (`insert_success_test`): `S`가 성공-반사된 모든
       규칙에서, `S`를 호출하는 바인딩 조건 `(App(S,args), r)`(`r`이 불리언
       리터럴이 아닌 경우) 바로 앞에 `(holds_S(args), true)`를 삽입 — owise
       형제뿐 아니라 시스템 전역(수집 성공은 의미상 함의라 안전; 분석
       전용이라 실행 무영향). `replace_cond`(1→1 매핑, 불리언 조건 전용)로는
       처리 못 하는 항상-바인딩 케이스를 `List.concat_map` 기반 삽입으로 보완.
    3. **게이트 완화**: `check_reflectable`이 `succ`(현재 시도의 후보 집합/
       최종 성공 집합)를 받아, 이미 성공-반사된 iteration helper 호출은
       `iter_helper_prefixes` 게이트를 통과시킴 — (2)가 심어둔 테스트 덕에
       형제의 원래 바인딩 조건이 ctest에서 그대로(“ys := S(args) 그대로
       인라인”) 처리됨, 별도 명시 처리 불필요(설계대로 확인).
    - **부수 발견 + 수정**: `cond_heads`(후보 종속성 탐색)가 조건의 최상위
      head만 보던 얕은 스캔이라, `fold_premise_binders`가 이미 인라인해 둔
      바람에 다른 호출의 **인자 위치에 중첩**된 iteration-helper 호출을
      놓쳐 `Cast_impl_neq`/`Cast_impl`/`$cast_unary`/`$cast_binary` 사슬이
      안 열렸다. `cond_heads`를 조건 term 전체(양변)를 재귀 순회하는
      `term_heads`로 교체해 해결(=check_reflectable이 실제로 보는 것과 동일
      깊이로 탐색을 맞춤).
    - **결과 (p4)**: owise 51/72 → **66/72 반사**(21 kept → **6 kept**: 잔여는
      `$subst_typeIR`/`$subst_parameterIR`/`$subst_callableTypeIR`/
      `$subst_callableTypeDefIR`/`$subst_constructorTypeIR`(gensym-threaded
      상태 인자가 형제 조건에 그대로 남아 "같은 subject 정렬" 전제가 깨져
      `effectful` 목록으로 무조건 게이트, 5개) + `$find_local_return_type_t`
      (반사는 시도되나 sibling 패턴의 생성자 `FUNCTION_colon_1`이
      `definedFunctionLocalKind`/`functionLocalKind`/`localKind` 세 타입에
      걸쳐 이름이 겹쳐 `matcher_type`이 유일 소유 타입을 못 찾아
      `Gate "ambiguous matcher"`, 1개)뿐 — 둘 다 `Mfe.check`의 `drop_owise`
      폴백이 계속 커버(분석 자체는 안 막힘), 실행 표면은 원래대로 `owise`
      속성 유지. **(2026-07-04 정정: 이 5개 "무조건 게이트" 서술은 부정확했음 —
      아래 권장 순서 섹션의 2026-07-04 항목 참조, 실제로는 안전하게 반사 가능해서
      66/72 → 71/72로 늘어남.)** 태스크가 기대한 사슬
      `$itercollect→Cast_impl_neq→Cast_impl→$cast_unary/$cast_binary`,
      `$match_overloaded_{named,unnamed}_*` 12심볼,
      `$gen_constraint`가 전부 열림. `$cast_unary` 절2가
      `holds_Cast_impl(..)=true`로 재철자되고 owise 절이
      `or(holds_Type_alpha(...), and(not(...), holds_Cast_impl(...)))=false`
      가드로 반사됨을 CTRS 육안 확인. impty golden(`spec.ctrs`)·impty/p4 실행
      모듈(`spec.maude`/native emit) 전부 byte-identical(SHA256 대조).
    - **MFE 실측**: `$itercollect_let_b_default_..._b_default`(7규칙 슬라이스:
      itercollect base/step + `$is_default_parameterIR` 2절 + 지원규칙) →
      **CRC YES / ChC YES**(무회귀, 새 반사가 실제로 discharge됨을 확인).
      `$cast_unary`/`$cast_binary`/`Cast_impl_neq` 자체 슬라이스는 subty
      전수 보완 규칙을 끌어와 5만 규칙급이라 이 환경(심볼당 분↔십분대)에서는
      스윕 불가 — 별도 세션에서 환경 여유 시 재시도.
    - **범위 밖으로 남긴 것**: 출력을 갖는 일반 relation의 `R_succeeds?`
      (임의 output-moded relation의 존재성 반사)는 이번에 구현하지 않음 —
      사용자 승인 설계가 명시적으로 이번 범위에서 제외했고, 위 게이트 결과
      (잔여 6개 중 output-relation 게이트 0개)로 현재는 불필요함이 확인됨;
      게이트 로그가 실제로 요구하면 별건으로 착수.
  - [x] **`$unzip`/`$iterproj` 규칙-생성 코드 통합 + reflect.ml `$iterproj`
    하드게이트 제거 (설계 2026-07-03, 구현 완료 2026-07-03).**

    **배경**: `iter_unzip_defs`(`$unzip`, `to_ctrs.ml:363-396`)와
    `iterpr_defs` 안의 `proj_defs`(`$iterproj`, `to_ctrs.ml:916-946`)가 규칙
    생성 로직상 동일함을 발견 — 차이는 `fv_terms`(캡처 자유변수 leading
    args, iterproj는 항상 빈 리스트)와 매칭 패턴(`elem_pat`: 임의 구조 vs
    `tuple_pat`: 항상 bare 튜플) 뿐. 조사 중 `reflect.ml`의
    `iter_helper_prefixes`(`reflect.ml:181-182`)가 `$iterproj`를 owise
    가드에서 하드게이트하는 이유도 "success reflection(`holds_`) 미구현"일
    뿐, `$unzip`/`$itermap`이 무조건 허용되는 근거("relation 미호출, 형제의
    binding 조건으로만 가드에 진입")가 `$iterproj`에도 문자 그대로 적용됨을
    코드로 확인(`proj_defs`가 만드는 규칙은 `nil_t`/`cons_t`/`tuple_t`/
    자기재귀 호출만 참조, relation 심볼 없음). **이전에 막힌 `⛔ $iterproj
    완전 제거`(위 항목, 커밋 `0c260cc6`, gensym 벽)와는 무관** — 그건
    `$iterapply`의 "원소당 1회 호출" 구조 자체를 없애려던 시도였고, 이번
    건 이미 계산된 튜플 스트림의 순수 후처리 코드만 공유/게이트 완화하는
    것이라 gensym에 전혀 영향 없음(`$iterapply`/`apply_rules`,
    `to_ctrs.ml:898-915`는 미변경).

    **Phase 1 — `to_ctrs.ml` 코드 공유** (3개 edit, `to_ctrs.ml`만):

    (a) `unzip_sym`(353-354줄) 뒤·`iter_unzip_defs`(356줄) 앞에 삽입:
    ```ocaml
    (* Defining rules for a single-spine helper [sym]: recurse over a [List]/[Opt]
       spine, matching each element against [elem_pat] and returning the element's
       [v] component ([var_t (step_hd v)], one of [elem_pat]'s pattern variables).
       [fv_terms] are captured constants threaded unchanged as leading arguments
       ([] when there are none). The spine's tail is always the fixed variable
       [__rest] -- there is exactly one spine here, unlike [spine_args]'s N-way
       [step_tl id] naming.

       Shared by [iter_unzip_defs] below ([$unzip]: [elem_pat] is the arbitrary
       translated iterated body -- possibly non-left-linear, when it re-mentions a
       captured [fv_terms] variable) and [iterpr_defs]'s [proj_defs] far below
       ([$iterproj]: [elem_pat] is always a bare fresh-variable tuple over an
       already-materialized stream, [fv_terms] always [] -- always irrefutable). *)
    let spine_projection_rules (sym : string) (fv_terms : R.term list)
        (iter : iter) (elem_pat : R.term) (v : string) : R.rule list =
      let collected = var_t (step_hd v) in
      let rest = var_t "__rest" in
      match iter with
      | List ->
          [
            rule (app_t sym (fv_terms @ [ nil_t ])) nil_t;
            rule
              (app_t sym (fv_terms @ [ cons_t elem_pat rest ]))
              (cons_t collected (app_t sym (fv_terms @ [ rest ])));
          ]
      | Opt ->
          [
            rule (app_t sym (fv_terms @ [ none_t ])) none_t;
            rule (app_t sym (fv_terms @ [ some_t elem_pat ])) (some_t collected);
          ]
    ```
    이름은 기존 `spine_args`(158-167줄, IL 노드가 아니라 이미 추상화된
    term/string을 받는 범용 콤비네이터에 붙는 접두사)와의 명명 관례를 따름
    — 이 계열의 다른 함수는 전부 `iter_` 접두사(특정 IL exp/prem 사이트에서
    직접 계산). **`spine_args`로 위임 금지** — tail 변수 명명이 `step_tl id`
    (`v__tl`)라 `$unzip`/`$iterproj`가 쓰는 고정 리터럴 `"__rest"`와 달라
    위임하면 방출 심볼이 바뀜(byte-identical 불변식 위반).

    (b) `iter_unzip_defs`(363-396줄) 본문 교체:
    ```ocaml
      match e.it with
      | IterE ({ it = VarE _; _ }, _) -> []
      | IterE (body, (iter, vars)) ->
          let fv_terms = List.map var_t (captured_fvs (Free.free_exp body) vars) in
          let ids = iter_var_ids vars in
          let elem_pat =
            subst_term (elem_renaming ids) (term_of_exp ~scalars body)
          in
          List.map
            (fun v ->
              let sym = unzip_sym body iter v in
              (sym, spine_projection_rules sym fv_terms iter elem_pat v))
            ids
      | _ -> []
    ```
    (`let rest`/`let collected` 라인은 공유 함수 안으로 이동해 제거.)

    (c) `iterpr_defs` 안 `proj_defs` 블록(916-946줄) 교체 — `apply`/
    `apply_rules`(898-915줄)와 `(apply, apply_rules) :: proj_defs`/`None`
    분기(947줄 이후)는 미변경:
    ```ocaml
                let proj_defs =
                  if List.length out_vars <= 1 then []
                  else
                    let tuple_pat =
                      tuple_t (List.map (fun v -> var_t (step_hd v)) out_vars)
                    in
                    List.map
                      (fun v ->
                        let sym = iter_proj_sym inner iter n v in
                        (sym, spine_projection_rules sym [] iter tuple_pat v))
                      out_vars
                in
    ```

    **Phase 1 검증**: 심볼 이름(`unzip_sym`/`iter_proj_sym`)·방출 규칙 모양
    불변 → byte-identical이어야 함:
    - impty/base golden diff 0: `main.exe rewrite --ctrs
      specs/impty/base/spec.spectec | diff - specs/impty/base/spec.ctrs`,
      `main.exe rewrite specs/impty/base/spec.spectec | diff -
      specs/impty/base/spec.maude`.
    - p4 전체 corpus diff 0: `main.exe rewrite --ctrs $(find specs/p4 -name
      '*.spectec' | sort)` 편집 전/후 비교(golden 없음, 직접 캡처; ~69초/회).
    - `$iterproj` golden 없는 경로는 슬라이스로 확인: `--symbol
      TableProperty_ok`(`TableEntry_ok` 반복 —
      `specs/p4/5-typing/5.14.1-typing-control-table.spectec:814`
      `TableProperty_ok/entries` 규칙 안), `--symbol Decl_ok`(`Type_ok`
      block 반복 3곳 —
      `specs/p4/5-typing/5.11-typing-declaration.spectec:875,901,927`
      `Decl_ok` 규칙들 안). 슬라이스 루트가 바깥쪽 relation인 이유:
      `iterpr_defs`는 반복 프리미스를 담은 바깥쪽 rule을 순회할 때 트리거됨.
    - `reflect:` stderr 요약도 diff 0 기대(이 단계는 `reflect.ml` 미변경).

    **Phase 2 — `reflect.ml` 하드게이트 제거** (2개 edit, `reflect.ml`만):

    (d) `iter_helper_prefixes`(181-182줄):
    ```ocaml
    let iter_helper_prefixes = [ "$iterall"; "$itercollect"; "$iterapply" ]
    ```

    (e) doc comment 3곳 정정(문장 삭제/이동만, 로직 변경 없음):
    - 43-52줄(파일 상단 개요): "an iteration helper without a success
      reflection (`$iterproj` -- multi-output projection, not handled yet --
      and `$iterall`/...; `$unzip`/`$itermap` are pure stream transformers
      and never gated)"에서 `$iterproj`를 빼고 `$unzip`/`$itermap` 목록으로
      옮김.
    - 171-180줄(`iter_helper_prefixes` 바로 위): "[$iterproj] (multi-output
      projection) has no success reflection yet, so it stays hard-gated
      regardless of [succ]." 문장 삭제, "The pure stream transformers
      [$unzip]/[$itermap]" → "[$unzip]/[$itermap]/[$iterproj]"로 확장.
    - 913-920줄(judgment-phase candidate 주석): "[$iterproj] does not [get a
      success reflection] ... so it is deliberately excluded here"를
      "`$iterall`/`$itercollect`/`$iterapply`는 boolean-valued judgment라
      success reflection이 필요하고, `$unzip`/`$itermap`/`$iterproj`는
      value-binding pure stream transformer라 애초에 불필요해 제외"로 정정.
    - **`is_iter_helper` 함수 자체(`is_iterall f || is_itercollect f ||
      is_iterapply f`)는 코드 변경 불요** — `$iterproj`는 원래도 이
      OR-체인에 없었음(주석만 stale).

    **Phase 2 검증**: p4 전체 corpus `--ctrs` 실행 시 `reflect:` stderr를
    Phase 1 이후 상태와 diff. 카운트 불변이면 "이 코퍼스에선 게이트가 실제로
    아무것도 막고 있지 않았다"는 뜻(그래도 정당한 정리 — 최신 "잔여 6 kept"
    목록(위 owise 반사 확장 항목)에 iterproj 관련 항목이 원래 없어 무회귀가
    오히려 예상됨). 새로 반사되는 symbol이 생기면 그중 하나를 `verify
    --symbol <name> --timeout 360`(MFE CRC/ChC)으로 무회귀 확인. impty/base
    golden도 재확인(iterproj 자체가 impty엔 없어 무영향 예상).

    **마무리**: `make fmt`. (계획 전문은 세션 로컬
    `/home/min/.claude/plans/tingly-enchanting-star.md`에도 있으나, 이
    항목이 authoritative — repo에 커밋되는 쪽.)

    **구현 결과 (2026-07-03).** 계획대로 Phase 1(`to_ctrs.ml`: `spine_projection_rules`
    신설 + `iter_unzip_defs`/`proj_defs` 위임) + Phase 2(`reflect.ml`:
    `iter_helper_prefixes`에서 `$iterproj` 제거, 주석 3곳 정정) 그대로 구현, `make fmt`
    적용(포맷만, 로직 무변경). **검증 전부 통과, 예상대로 무회귀:**
    - impty/base `--ctrs`/기본(`spec.maude`) 골든 둘 다 byte-identical.
    - p4 전체 corpus(`rewrite --ctrs`, 79파일) 출력 **및** `reflect:` stderr 요약
      (`66 owise rule(s) reflected, 6 kept`) Phase 1 전/후/포맷 후 3단 비교 모두
      byte-identical — 예측대로 이 코퍼스에선 `$iterproj` 하드게이트가 실제로 아무
      owise 절도 막고 있지 않았음(잔여 6 kept는 예상대로 iterproj 무관).
    - `--symbol TableProperty_ok`/`--symbol Decl_ok` 슬라이스에 `$iterproj` 규칙이
      각각 7·22개 존재 확인 — 공유 코드 경로(`spine_projection_rules`)가 실제로
      두 소비처 모두에서 발화함.
    - 새로 반사된 symbol이 없어(카운트 불변) 계획의 "새 symbol 있으면 MFE
      재검" 조건이 vacuous — 별도 MFE 확인 불필요.
    - **MFE 환경 재확인(본 세션 시작 시, 이 작업과 무관하게 선행 시도):** todo.md가
      "다음 세션에서 환경 복구 후 재검"이라 적어 둔 (B) MFE 실측을 먼저 시도했으나,
      impty `$lookup` 대조군(과거 ~1.4s)이 `--timeout 280`에서도 TIMEOUT — 지난
      세션 기록([[MFE env slowdown]] 메모)과 동일 증상으로 **환경이 아직 회복되지
      않음**을 재확인(회귀 아님, RAM/OOM 로그 이상 없음 — 원인 불명, 별도 조사 필요).
      이번 세션은 그 대신 환경 의존이 없는 본 항목(코드 통합, golden/corpus diff로만
      검증 가능)을 완료함.
  - [x] **prelude 산술 overlap 해소(완료).** `mod`/`div`/`mod_int`/`div_int`은
    `= A if lt(x,y)=true` + `= B if leq(y,x)=true`처럼 보집합 가드를 *다른 술어*로
    적어 CRC가 동시 불가를 못 보고 `x = mod(sub(x,y),y) if lt(x,y)=true /\ leq(y,x)=true`
    같은 헛-임계쌍을 냈다. **fold(구현):** 가드 boolean을 **보조 함수로 dispatch** —
    `mod(x,y)=mod-aux(lt(x,y),x,y)`, `mod-aux(true,..)=x`, `mod-aux(false,..)=mod(sub(x,y),y)`.
    base/recursive 절이 서로소 `true`/`false` head 패턴이라 단일화 불가 → 임계쌍 미생성.
    `div_int`의 부호 판정은 구조적 `eq`(전 생성자 동치 → 슬라이스 44k 폭증) 대신
    boolean `equiv(nonneg x, nonneg y)`로 바꿔 슬라이스 37로 축소. 보조 head 4개
    (`div_aux`/`mod_aux`/`div_int_aux`/`mod_int_aux`)는 `native_replaced_heads`라
    분석 surface 전용(Native는 `quo`/`rem` delegation, 실행 byte-identical). 결과:
    `$bin_div`/`$bin_mod`/`$bin_plus`/`$bin_minus`/`$bin_mul` 전부 YES.

  **전체 p4 sweep (574 심볼 전수, 진행 중) + triage 도구.** 이전 sweep들은 "≤200
  규칙 tractable slice"만 골라 돌렸으나, 이제 **`verify --list-symbols` 574개 전체**를
  slice-size **오름차순**(작은 것부터 결과가 빨리 쌓임)으로 돌린다. 심볼당 maude
  `--timeout 120` + 외부 `timeout 150`. 백그라운드로 돌리며 라이브 로그를 repo 루트에
  symlink: `spectec/mfe_sweep.log`(심볼마다 `[시각] i/574 이름 size CRC ChC 경과초`),
  `spectec/mfe_sweep.tsv`(`symbol/size/church_rosser/coherence/elapsed_s`). 스윕
  스크립트·결과는 scratchpad에 있고 gitignore됨(`mfe_sweep.*`/`triage*`/`ctrs_ops.txt`).
  대형 슬라이스(수천~6만 규칙; `Program_inst` 등 전체-시스템급)는 임계쌍 폭증으로 120s
  TIMEOUT 예상 — 오름차순이라 뒤로 갈수록 TIMEOUT 밀도가 오른다.

  - **triage 도구 [`triage_mfe.py`](../../triage_mfe.py)** — sweep TSV에서 CRC=MAYBE /
    ChC=NO를 뽑아 **각 심볼을 IL 소스 선언 위치(`file:line`+시그니처)로 연결**한다.
    sweep 심볼은 전부 직접 IL 이름이라 grep 인덱스로 해결되고, sanitize된 `_prime`은
    `'`로 역변환(`desanitize`)해 매칭한다. **defunctionalize 템플릿 재구체화("다시
    구체화"):** `$find_overloaded<V>`/`$reduce_serenum_binary(def $check)` 류는 sweep에서
    size-0 NOVERDICT로 뜨는데(defunctionalize가 원본 규칙을 특수화 이름으로 **복사**해
    옮겨 제네릭 이름엔 규칙 0), 실제 CTRS op 집합을 ground-truth로 스캔해 구체 인스턴스
    (`$find-overloaded-parameterListIR-of-constructorDef` 등)로 펼치고 각 인스턴스의
    def-arg getter를 그 IL 선언으로 역해결한다. 매핑은 de-mangling 없이 `norm()`
    (소문자화+`-`/`_` 통일+`$` 제거) 기반이라 견고; 템플릿 감지는 dec 헤더의 `<..>`/
    `def $` 파라미터(선언 인식을 **컬럼 0 앵커**로 고정해 들여쓴 `def $check` 파라미터를
    오탐 안 함).

  - **degenerate NOVERDICT 9개의 정체 (전부 설계상 규칙 0 — 번역 누락 아님):**
    1. **고차/제네릭 템플릿 7** (`$find_overloaded`/`$find_overloadeds_named`/
       `$find_overloadeds_unnamed`/`$match_overloaded_named`/`$match_overloaded_unnamed`/
       `$reduce_serenum_binary`/`$reduce_serenum_unary`) — `<V>` 타입 파라미터 및/또는
       `def f(..)` 고차 파라미터를 받아 defunctionalize가 인스턴스별로 규칙을 복사·특수화.
       제네릭 이름엔 규칙 0(실측: `$find-overloaded(` 0개 vs `-of-constructorDef` 4개 등
       원본 4절 × 6타입 = 24 특수화 규칙). 실제 규칙은 caller 슬라이스에 transitively
       끌려가 거기서 검사됨. (`--list-symbols`는 IL dec만 열거해 특수화 이름은 단독으로
       안 잡힘 — triage가 재구체화로 메꿈.)
    2. **타겟 구현 훅 1** (`$init_objectState`) — `dec`만 있고 `def` 없음("in the target").
    3. **extern 관계 스텁 1** (`ExternFunctionCall_eval_lctk`) — `rule` 0개, `testgen_ignore`.
- **Termination(MTT/AProVE) 경로 확보 — CRC 보완용 (done, sweep 진행중).** CRC는
  *국소* 합류성 + sort-decreasingness만 주고 완전 Church-Rosser엔 종료성 증명이
  필요하다. 이 3.5.1 MFE의 MTT는 안 돌지만(Maude++ 훅·2.x 문법), **매칭되는
  Maude-2.7.1 스택**을 붙여 실제로 돌린다: `tools/maude271-hooks/maude`(v2.7.1-ext-hooks,
  훅 바인딩) + `tools/mfe271/`(옛 MFE, **MTT 1.5j** + 옛 Full Maude) +
  `tools/aprove/aprove.jar` + `tools/z3/z3`. 슬라이스 하나는
  `tools/mfe/run-termination.sh <symbol>`: `--ctrs` 슬라이스를 옛 Full-Maude
  `fmod`로 헤더 변환 → `(select tool MTT .)(select external tool aprove .)(ct SPEC .)`
  → MTT가 order-sorted 조건부 모듈을 TPDB CTRS로 변환(‑isTerm/isThruth sort 가드
  추가) → `mfe.config`대로 AProVE 호출. **백엔드=Z3**(AProVE 기본 `SmtSolver=z3`);
  WST 전략의 레거시 yices-1.x 호출은 곱게 abort하므로 **라이선스 게이트 yices 불필요**
  (산술 무거운 슬라이스만 느려짐). 검증: `FOO`·`$empty_map`·`$is_lpm_key_prime` → YES.
  전부 gitignore(`tools/{maude271-hooks,mfe271,aprove,yices,z3}/`), 셋업 상세는
  [tools/mfe/README.md](../../tools/mfe/README.md). **종합 스윕(153심볼 CRC+termination) +
  모듈러(A/B) 분해 완료 (2026-07-09)** — 결과·수정 합성 정리는 저장소 루트
  `spectec-crc-termination-recalibration.md`(이후 recalibration.md로 개명, 현재는
  verification.md + verification-notes.md로 분리). full-mode의 고정폭 산술 TIMEOUT은 도구 예산이
  아니라 폭-정규화 헬퍼로 국소화됨: **모듈러 (B) 분해(`tools/mfe/prune_modular.py`, 산술을
  종료 블랙박스로 추상)로 11개 산술 + 3개 비트와이즈 슬라이스가 전부 수초 YES**,
  sum/max/min_nat·strip_all_whitespace(eq-theory abstract, 50634→5규칙)도 YES.
  **단 하나의 진짜 잔여물 = `$bitstr_to_int` w=0 실행 비종료**(아래 신규 이슈). termination
  verdict는 **analysis 표면 한정**(owise drop + isStuckHead ruleless)이라 executable 인증은 후속.
- [ ] **⚠️ `$bitstr_to_int` w=0 실행 비종료 (신규 2026-07-09, termination 재캘리브레이션에서 발견).**
  `builtin.ml:457-489`의 two's-complement decode가 `w=0`이면 목표 구간 `[-2^(w-1),2^(w-1))`가
  공집합이 되어 `n:0→-1→0` 진동 루프(규칙1 `n≥0`→`n-1`, 규칙2 `n<0`→`n+1`, base 도달 불가).
  참조 `numerics.ml:51-56 bitstr_to_int'`도 구조 동일 → w=0에서 같은 루프(**번역은 faithful**,
  differential MATCH와 부합). 실무 종료는 오직 `w≥1` 호출 덕분(구간 비어있지 않아 수렴). **규명 필요**:
  P4 타입시스템이 `bit<0>`/`int<0>` 산술을 막는가? → 막으면 도달불가 `w≥1` 불변식(문서화로 종결),
  안 막으면 numerics.ml+CTRS **공유 실행 버그**(builtin.ml w=0 처리 수정). 영향: 고정폭 W/S decode
  op 전부(lt/le/gt/ge, plus/minus/mul, un_minus-S, band/bxor/bor-W/S, nat_of_integerValue, satplus/satminus, shl/shr).
- [x] `to_ctrs.ml` 상단 `[@@@warning "-32-69"]` 제거 (완료 — 어트리뷰트는 이미
  소스에 없고 `dune build bin/main.exe`가 경고 없이 통과. 빌더 레이어가 전부
  참조된다는 뜻).
- **⛔ `$iterproj` 제거(수집 헬퍼 통일 1단계) — 보류, 정합성 벽 (2026-07-02).**
  다중 출력 iterated relation을 "출력별 무조건 map"으로 바꾸려던 안은 **gensym과
  충돌**한다: p4의 multi-output `$iterapply` 3개(`TableEntry_ok`,
  `Type_ok/block` ×2 — 출력에 fresh `typeId'` 포함)가 전부 state-thread돼 있어,
  출력별 map이 각자 relation을 재호출하면 **fresh 이름 발급이 map마다 갈라져**
  인터프리터와 divergence가 난다(성능이 아니라 정합성 문제). 단일 호출 + 순수
  스트림 projection(현 `$iterapply`+`$iterproj`)이 효과적 다중 출력의 올바른
  형태라 유지. 후속 `$itercollect`-only 통일(4e00da94 revert)도 같은 벽 —
  effectful 반복은 원소당 1회 호출 구조가 불변식임을 전제로 재설계해야 함.

  **→ 해소 (2026-07-18, M1 "iter 헬퍼 패밀리 통합" 참조).** 불변식을 지키는
  재설계로 통일 완료: 출력별 재호출 대신 **단일 튜플-수집 `$itercollect`**(호출
  원소당 정확히 1회, 무조건 eq 유지)를 두고, 성분 복원은 이미 계산된 튜플
  스트림 위의 순수 projection(합성 튜플 본문의 `$iterproj`) 또는 소비 헬퍼의
  fused destructure가 맡는다 — 2026-07-16 head-side fusion 기계가 premise-side로
  확장되면서 당시 벽이던 "소비자 배선" 문제가 풀렸다.

### CRC 조건-단순화 **예산** 실측 — `$join_ctk`/`$assignop_as_binop` MAYBE의 원인 규명 (2026-07-14)

CLAUDE.md가 *"Fall-through/default clause guarded by `or(all match-Xs) = false`"* 로
분류해 둔 허위 MAYBE(`$join_ctk`, `$assignop_as_binop`)의 **진짜 원인을 규명**했다.
기존 트리아지("infeasible once any specific matcher fires")는 *현상*은 맞지만 *왜 CRC가
그걸 못 보는지*를 짚지 못했다.

**실측 1 — 원인은 or-체인 길이(예산)지, 정보 부족이 아니다.**
`$join_ctk`의 잔여 ccp를 **20줄 미니모듈(J0)로 그대로 재현**했다(실물과 동일한 ccp):

```
ccp: variant-ctk-CTK-0 = variant-ctk-DYN-0
     if or(or(or(and(match-ctk-LCTK-0(CTK), match-ctk-LCTK-0(CTK)), …), …),
           and(match-ctk-CTK-0(CTK), match-ctk-CTK-0(CTK))) = false .
```

- 조건은 **100% ground**다 (owise가 ground head 형제 절과 겹쳐서 unifier가 이미 생성자를
  대입해 버렸다). 슬라이스엔 `match-ctk-CTK-0(CTK)=true`, `or(true,y)=true`가 **전부 있다**.
- 같은 항을 stock Maude `reduce`에 넣으면 **9 rewrites 만에 `true`** = 조건 모순 = 불가능.
  **그런데 CRC는 안 줄인다.**
- disjunct 수를 바꿔가며 경계를 찍었다:

  | or-disjunct 수 | CRC |
  |---|---|
  | 2 (H0/J2) | YES |
  | 3 (J3) | YES |
  | **4 (J0 = `$join_ctk` 실물 형태)** | **MAYBE** |

  → **CRC의 CCP 조건 단순화에는 한정된 예산이 있다.** "프로그램 등식을 안 쓴다"가 아니라
  **깊어지면 못 쓴다**. (CRC 판정 법칙 전체는 memory `mfe-crc-hypothesis-rewriting` 참조:
  ①가설 충돌로 refute ②생성자 narrowing 안 함 ③프로그램 등식은 쓰되 예산 있음.)

**따름 — guard를 "똑똑하게" 만드는 처방은 이 부류에 전부 무효.** 줄여야 할 항이 이미
ground라 가설이 개입할 자리가 없다. 이번 세션에 시험 구현한 `Reflect.tag_guards`
(타입당 단일 판별자 `tag_<T>` + `tagcase_<T>_<Ci>`, `align_guards`의 N치 일반화)가
정확히 이 이유로 **이득 0이라 되돌렸다**. 상세:

- 메커니즘 자체는 옳다 — 통제 모듈에서 `match_` spelling MAYBE → tag spelling **YES**.
  겨냥한 건 법칙②(narrowing 불가로 형제 matcher guard의 배타성에 도달 못 함)다.
- **그런데 검사 가능한 MAYBE 중 법칙②에 막힌 게 하나도 없었다.**
  - `Check_expr`(impty), `$typedLvalueIR_as_typedExpressionIR`(p4): 형제들이 **head 패턴으로
    이미 분기**(`variant-expr-plus-2` vs `variant-expr-lt-eq-2`) → 좌변이 **unify 불가** →
    **임계쌍이 아예 생성되지 않는다**. tag는 짝을 이루지 않는 규칙에 조건만 덧붙였다.
  - `$join_ctk`/`$assignop_as_binop`: 위의 예산 문제 → tag로 respell하면 disjunct마다
    `tag(..)→tagcase` + `eqg→true` 두 스텝이 **더 붙어 오히려 악화**.
  - `Check_expr`의 잔여 ccp는 `$lookup(t, ID(x))` 출력이 두 변수에 묶인 **결정성 쌍** — 무관.
- **구조적 이유**: `hoist_matchers`/`fold_premise_binders`/`expand_subty_guards`가 하는 일이
  바로 "guard에 있던 판별을 head 패턴으로 옮기기"다. 성공할 때마다 tag의 먹잇감을 없앤다.
  남는 잔여물은 (i) subject가 **계산된 값**이라 못 접는 경우(→ 그런 절들은 head가 이미
  서로소라 CCP 없음)와 (ii) `guarded_family` 예외(→ 그게 곧 owise 예산 문제)다.
- 다만 **tag의 사정거리를 과소평가했던 점은 정정**한다: guard를 *커버된 쪽*이 아니라
  **여집합 쪽**으로 철자하면 `$assignop_as_binop`은 tag로도 풀린다 —
  `or(12겹 match) = false` 대신 `tag_assignop(x) = tagcase_eq` 한 줄(생성자 13개 중 12개
  커버 → 여집합 1개). 단 `$join_ctk`은 커버 4/여집합 5로 **양쪽 다 예산 초과**라 어느 쪽
  철자로도 안 된다. 즉 tag는 이 부류의 일부만 건드릴 수 있다.
- p4에서 tag가 실제 발화한 형제-겹침 심볼(`Lvalue_ok` 23규칙, `TableAction_ok` 17 …)은
  슬라이스가 **5만 규칙 이상**이라 CRC가 애초에 안 돈다 → 이득 측정 자체가 불가능.
  **명제: tag는 법칙②를 정확히 푼다. 그런데 검사 가능한 슬라이스에 법칙② 봉쇄가 없다.**
  MFE가 5만 규칙을 감당하게 되면 재고 가치 있음.

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
- [x] **CLI `run` 실행 배선** (done) — targets의 `maude_start_term`
  (`Targets_impty.Impty`/`Targets_p4.P4`) + `Maude_run.run_batch` + `--check-p4`
  (Of_maude 역번역·`Eq.eq_values` 비교)까지 [bin/main.ml](../../bin/main.ml)에 배선 완료.

## M3 — 결과-VALUE 오라클

- [x] **`Of_maude` 복구** (done) — Maude normal form → IL value 역번역 + `canonicalize`
  (gensym 이름·map 정렬 정규화).
- [x] **differential** (done) — same-spec interp(p4) vs Maude(p4)를
  [check_diff_p4.sh](../../../check_diff_p4.sh)로 전체 corpus 교차 검사. 결과:
  completeness gap 0, soundness gap 1(알려진 issue1944), Phase D 결과-VALUE
  오라클 1227/1227 MATCH. 상세는 [CLAUDE.md](CLAUDE.md) "회귀/divergence 측정" 절.
- [x] **CTRS(구조적) differential — 완료 (2026-07-10, `92618dc2`).**
  [check_diff_structural_p4.sh](../../../check_diff_structural_p4.sh)가 인터프리터
  vs **구조적 CTRS**를 전체 corpus에서 교차 검증한다(Phase A/B/C/D, resumable,
  `CORPUS_LIST`/`MPROG`/`RESMATCH` 샤딩). 이진 nat 이론 위에서 `$int_to_text`
  부호 수정(`5e1c3ea1`) 후 **Phase D 1227/1227 MATCH / 0 MISMATCH**
  ([spectec-structural-completeness-soundness.md](../../../spectec-structural-completeness-soundness.md)).
  ⇒ `Reflect.owise`/`fold_premise_binders`의 의미 보존이 처음으로 **실행 기반**으로
  뒷받침됨. 아래는 착수 당시의 설계 메모(이력).

  위 differential은 인터프리터
  vs **Native 실행 모듈**(`To_maude`, 내장 Bool/Nat/Int/String 위임)만
  교차 검증한다. **구조적 CTRS**(`Reflect.owise`/`Rewrite_system.
  fold_premise_binders`/`prune_unused`를 거친 분석 표면 —
  `To_mfe.module_of_system`이 만드는 order-sorted `eq`/`ceq` Full Maude
  모듈)는 지금까지 MFE의 confluence(CRC)·coherence(ChC) 판정만 받았을 뿐,
  **실제로 Maude에서 rewrite해 나온 값을 오라클과 대조한 적은 없다** —
  owise/judgment 반사가 "분석 전용, 실행 무영향"이라는 주장은 지금 논거로만
  뒷받침되고 실행 기반 실측은 없다(반사 로직에 숨은 의미 버그가 있어도
  실행 표면은 이 패스들을 안 타므로 현재 체계로는 못 잡음).
  - **목표**: structural CTRS 모듈을 Maude에서 직접 `reduce`(또는 META-TERM
    경로)해 얻은 정규형을 IL value로 역번역해, 같은 프로그램의 인터프리터
    결과(그리고/또는 기존 Native 실행 오라클)와 비교하는 **제3의 오라클
    레그**로 삼는다.
  - **필요한 조각**: (a) structural scalar(Peano `succ`/`zero`, 부호-크기
    `int_pos`/`int_neg`, char-list `chr_<code>` text, 자체 `true`/`false`)
    시작항 인코딩 — 지금 `to_maude.ml`의 META-TERM 인코더(`meta_term_of_value`/
    `meta_start_app`)는 Native 전용이라 재사용 불가, structural 전용 인코더가
    필요; (b) structural 정규형 → IL value 역번역기(`Of_maude`의 structural
    버전 — 생성자 스펠링이 다름: `succ`/`zero`, `cons`/`nil`, `chr_<code>`,
    `variant_*`/`struct_*` 등, `canonicalize`의 gensym/map 정렬 정규화는
    그대로 재사용 가능할 것); (c) 성능 — Peano 인코딩·문자 단위 텍스트라
    Native보다 훨씬 느릴 것(작은 입력·표본 위주로 시작, `run_batch`류 배치
    상각 필요할 수 있음).
  - **가치**: `Reflect.owise`/`fold_premise_binders`가 실제로 의미 보존인지
    최초로 **실행 기반**으로 검증(현재는 byte-identical golden + CRC/ChC
    판정으로만 뒷받침 — 둘 다 반사 인코딩이 "말이 되는지"는 보지만 실제
    reduce 결과까지 확인하지는 않는다).
  - **2026-07-04 진행 상황(위 목표의 (a)/(b)는 이미 이전 세션에서 `run-structural`
    커맨드로 배선 완료 — impty는 end-to-end 검증됨).** P4 `Program_ok`를 이
    경로로 처음 실제 reduce해보니 세 겹의 실행 전용 버그가 순서대로 드러났다:
    (i) reflect.ml/to_ctrs.ml이 만드는 헬퍼 심볼(`$unzip_*`/`proj_<ctor>_<i>`
    등)이 원 spec의 `TypD`/`RelD`에 없어 `Maude_sorts.recover`의 폴백으로
    전부 `Val` 타입이 되고, 이게 정밀 타입의 실제 constructor에 들어가면
    ERROR-kind 항이 되어 `and`/`or` short-circuit으로도 못 고침 — `recover`에
    `infer_ranges`/`infer_proj_ranges` 갭필 패스 추가로 해결(`8d161af8`).
    (ii) `Program_ok` 끝까지 돌리면 native Maude가 stack overflow — 원인은
    `eqg`(구조적 동등성)가 반사 규칙(`x=x→true`)만 있고 "다르면 false"가 없어
    분석(CRC)에선 문제 없지만 ground reduction에선 무한정 stuck, 그리고
    `and`/`or`에 붙인 `[comm strat (1 0 2 0)]`의 `comm`이 Maude의 AC 매처
    때문에 `strat`이 막으려던 두 번째 인자 평가를 강제해버려 재귀 judgment
    guard가 안 끝남(toy 모듈로 직접 재현) — `comm` 제거하고 미러 방정식을
    직접 찍는 걸로 교체, `eqg`엔 실행 전용 owise-false 추가(둘 다 `full_maude`
    게이트라 CRC엔 무영향)로 해결(`a3e9c271`).
    (iii) 그 다음 막힌 지점은 `$int_to_bitstr`/`$bitstr_to_int`/`$shl`/`$shr`/
    `$shr_arith`/`$pow2`/`$bneg`/`$band`/`$bxor`/`$bor`/`$bitacc` — spec엔
    `builtin dec`로만 있고 Maude 방정식이 하나도 없었던 것. 인터프리터의 실제
    구현(`targets/p4/builtins/numerics.ml`)을 그대로 참고해 11개 전부 구현
    (반올림 방향이 갈릴 수 있는 것들은 클로즈드폼 대신 numerics.ml과 동일한
    재귀 구조로), 독립적으로 14개 케이스 검증 후 `2583c31f`로 커밋.
    **결과: 작은 bit-width P4 프로그램은 `Program_ok`가 이제 완전히 끝까지
    풀린다** (`const bit<4> x = 3;` → 정확한 typed constantDeclarationIR로 축약
    확인). 단, **새로운, 별도의 한계**를 발견: Structural 이론의 Peano/unary
    수 인코딩은 `bit<32>` 하나 표현하는 데만 2^32(~43억) 개 중첩 `succ()` 항이
    필요해 실제 corpus 대부분(`bit<8/16/32/48/64>` 흔함)에서 OOM(exit 137)으로
    죽는다 — 버그가 아니라 표현 자체의 스케일 한계(`pow2(8)`은 ~2초, `pow2(32)`는
    단독으로도 20초 안에 안 끝남 확인). 고치려면 Structural 이론에 binary 수
    인코딩이 필요 — 별도의, 더 큰 트랙.
    check_diff_p4.sh를 본떠 [check_diff_structural_p4.sh](../../../check_diff_structural_p4.sh)
    작성(`cde3d763`) — Phase A는 기존 `check_diff_p4_interp.tsv` 재사용, Phase B는
    OOM 격리를 위해 CHUNK를 30→10으로 줄이고 `ulimit -v`(기본 ~4GB)로 Maude
    프로세스별 메모리 상한을 걸었으며 분류기에 OOM 카테고리 추가. 4/12개 표본으로
    스모크 테스트 통과 후, 전체 1568개 corpus 대상 실행을 tmux 세션
    (`spectec-structural-diff`, 로그: `check_diff_structural_p4.log`)으로 백그라운드
    시작함 — **완주 확인(2026-07-07, `RESUMED_RUN_DONE_EXIT=0`): RESULT-VALUE
    3 MATCH / 0 MISMATCH, 나머지 179는 DECODE_ERR/NOCOMP/INTERP_FAIL/TIMEOUT/
    UNKNOWN(예측된 Peano 스케일 한계 계열; wall-clock 구조 14h vs interp 16m).
    MISMATCH 0 = 반사 패스 의미-보존의 실행 기반 확인이라는 원래 목표는 소형
    표본에서 달성; 커버리지 확대는 binary 수 인코딩 트랙(별도 체크아웃
    `spectec-core-binenc`에서 진행 중)의 몫.**

## Native 직접 생성 리팩토링 (B) — ✅ 완료

**한 일:** `~scalars:Native`를 post-fold(`native_scalars.fold`)에서 **생성 시점 직접
방출**로 바꿨다. `Ctrs_term`에 `scalar_theory`와 mode-aware leaf 빌더
(`bool_t`/`term_of_num`/`text_t`/`nat_lit`/`int_lit`/`conj_t`가 `~scalars`)를 두고,
`to_ctrs`/`prelude`/`builtin`/`gensym`에 `~scalars`를 thread했다. `prelude`는 한 개의
정렬된 리스트(`rules ~scalars`) + Native head-filter(`kept_in_native`), `builtin`은
delegated text builtin을 Native에서 생략, `gensym`은 native txt/`cat`로 fresh 이름을
짓고 prime `chr` eq를 Native에서 생략. `maude/native_scalars.ml`은 삭제. CORE_LOGIC
§6.1 "별도 fold 없음" 실현. **검증:** impty/base 분석 골든(`spec.ctrs`)·실행
모듈 모두 **byte-identical**(Structural 경로 무변, Native 경로 post-fold와 동일),
p4(79 파일) native 모듈 emit 시 구조적 스칼라 leaf 누출 0·delegation 정상.

다음 줄들은 조사 당시 기록이며 **전부 위 완료 시점에 해소됐다**(historical, `[x]`):

조사로 드러났던 blocker(왜 단순 leaf 교체로 안 됐나):

- [x] **bool leaf가 `to_ctrs` 밖에서도 생성됨.** `builtin.ml`(30+곳)·`gensym.ml`(5곳)이
  `T.true_t`/`T.false_t`/`T.text_t`/`T.peano_of_int`/`T.int_pos_t`로 규칙을 직접
  만든다. 이 규칙들도 시스템에 들어가므로 native에선 wrapper여야 한다 → **3개 모듈
  모두 mode-aware**로 만들어야 함(post-fold는 조립 시스템 전체를 균일 fold해 자동 처리).
- [x] **유지되는 prelude 규칙도 bool leaf 보유.** native에서 살아남는
  `mem(x,nil)→false`, `match_some/none/cons/nil`, 옵션/리스트 `eq(...)→true/false/and`은
  native bool이 필요. 드롭되는 scalar 규칙과 한 리스트에 섞여 있어 **prelude를
  scalar(드롭)/struct(유지) 둘로 분리** 필요.
- [x] **`int_pos` 막다른 길.** `term_of_unop` 단항 마이너스 = `negate_int(int_pos(x))`.
  post-fold는 ground `int_pos(peano)`→`int(n)`으로 접지만, direct-gen은 operand가
  `nat(5)`라 `int_pos(nat(5))`가 되고 `int_pos`는 `native_replaced_heads`에 없어
  **delegation도 fold도 없어 stuck**. → `To_maude`에 `int_pos`/`int_neg` delegation
  추가 또는 `term_of_unop` mode-aware화.
- [x] **드롭 필터 충돌.** scalar 규칙 드롭하는 `scalar_pat`은 bare `true`/`false`/`zero`를
  본다. leaf를 `bool(true)`로 바꾸면 `eq(bool(true),bool(true))`를 못 알아봐 안 드롭됨
  → To_maude의 scalar `eq` delegation과 충돌. **필터를 head-기반(scalar/struct 분리)으로
  재설계** 필요.
- [x] **OCaml 제약:** `true_t`/`false_t`는 값(value)이라 모듈 로드 시 1회 평가됨 →
  ref로 mode-aware 못 만듦. mode-aware는 함수(`bool_t`/`term_of_num`/`text_t`)로 두고
  `true_t`/`false_t` 사용처를 `bool_t true`/`bool_t false`로 교체하거나, prelude는
  thunk로 만들어 of_spec에서 mode 설정 후 평가.

작업 순서(안):
1. `to_ctrs`: `scalars_mode` 결정 방식 확정(ref vs 인자 thread), `bool_t`/`term_of_num`/
   `text_t` mode-aware화, prelude scalar/struct 분리, 살아남는 규칙의 `true_t`/`false_t`
   교체, 드롭 필터 head-기반 재설계, `native_scalars`/`native_term` 제거.
2. `To_maude`: `int_pos`/`int_neg` delegation 추가(또는 `term_of_unop` 대응).
3. `builtin.ml`·`gensym.ml`: scalar leaf를 mode-aware 빌더로 교체.
4. 검증: impty 실행(native 모듈 eyeball) + 가능하면 p4 일부로 회귀 확인.

## subtype의 struct 처리 — ✅ 해소 (interp 확인 결과: width/depth 검사 자체가 없음)

**결정 절차 1번(interp.subtyp 확인)의 답이 나와 종결.** 인터프리터
(`interp/eval_il/interp.ml:210-258`, eval_sl 동일)의 `subtyp`은 VarT 가지에서
`match (deftyp.it, value.it)`로 PlainT/(VariantT,CaseV)만 명시 처리하고 **StructT는
catch-all `| _ -> true`** — width도 depth도 head 모양 검사도 없이 무조건 true.
이유는 interp.ml:150 주석: "structs are invariant in SpecTec, so we do not need
to check for subtyping" — struct 간 subtype 관계가 언어에 없고(invariant), 좁힘은
elaborator가 정적으로 처리, 런타임 검사 시점엔 정적 타입이 정확히 그 타입임이 보장.
⇒ **옛 접근안 (c)가 코드로 확정: trivially-true가 정답 의미론, width/depth 구현 불요.**

- width 구현은 오라클에 없는 검사 발명(이름-키 재인코딩 등 큰 변경으로 의미론을
  벗어남); depth 재귀 추가는 interp보다 엄격해져 가짜 STUCK(completeness gap) 위험.
- 현행 `subty_<T>(struct_<T>(x...)) -> true`(head-키)는 도달 가능한 주어에서 interp와
  일치(invariance ⇒ struct 타겟 SubE의 주어 정적 타입 = T). 변수 LHS 대신 head-키를
  유지하는 근거(번역 버그 tripwire)는 defs_of_typ StructT 주석에 명시.
- 경험적 뒷받침: 이 규칙 그대로 Phase D 1227/1227 MATCH·completeness gap 0.
- "struct를 품은 복합타입 전파" 우려(아래 depth 섹션 (1))도 동시 해소: list/tuple/opt의
  depth 검사가 struct 원소에서 true로 멈추는 건 interp도 동일 — 공유된 정답 의미론.

## subtype의 depth 처리 — 재귀 경계와 근사 (struct 외)

`sub_pred`([translate/to_ctrs.ml:191](translate/to_ctrs.ml))가 타입 구조를 따라 depth로
내려가며 멤버십 검사를 생성한다. 어디서 재귀하고 어디서 trivially-true로 멈추는지,
그리고 멈추는 지점의 근사를 정리한다(struct 구멍은 위 섹션; 아래 (1)이 그걸 복합
타입으로 전파시킨다).

**depth 재귀가 실제로 되는 곳:**
- VariantT 케이스 payload(필드 타입별 `sub_pred`), TupleT 성분, IterT list/opt 원소
  — `defs_of_typ`/`sub_helper_defs`가 `and(sub_pred ..)` / 구조 재귀로 내려감.
- PlainT alias `T = U`: `subty_<T>(x) -> sub_pred U x`로 위임(체인 OK).

**trivially-true로 멈추는(근사였다가 interp 대조로 확정된) 곳:**
1. **struct를 품은 복합타입**: ✅ 해소 — interp도 struct 원소에서 true로 멈추므로
   (위 struct 섹션) 근사 불일치가 아니라 공유된 정답 의미론.
2. **타입 파라미터(generic VarT, TypD 없음)**: ✅ 일치 확인 — interp.subtyp도 정의
   없는 타입 파라미터는 catch-all `| _ -> true`. `sub_helper_defs`의
   `subty_<param>(x) -> true`와 동일 의미론(over-accept가 아니라 오라클 그대로).
3. **스칼라 leaf**: ✅ 일치 확인 + 주석화 완료 — interp와 정확히 동일(int는 모든 수,
   bool/text는 자기 sort, nat만 런타임 검사 `sub_nat`; `sub_pred` 주석에 명시).

**depth + 음성(negation) 전파 — ✅ false-보완으로 분석 표면도 닫힘 (2026-07-02):**
- subty가 positive-only여서 분석 표면에서 비멤버가 stuck이던 문제는 **사용-기반
  false-보완**(아래 "subty totality" 절)으로 해소: 모든 SubE 사이트의 (타겟 T,
  주어 정적 타입 S) 쌍에 대해 cases(S)\cases(T)별 `subty_T(..) -> false`를 방출,
  도달 가능한 도메인 전체에서 true/false로 환원. `~(e <: T)` 조건이 분석 표면에서도
  유의미해짐. 실행 표면의 To_maude owise complement는 잔여 head 흡수용으로 유지
  (명시 규칙이 선점, 의미 불변).

**termination(생성·재기록) — ✅ 확인 완료 (2026-07-10, p4 `--ctrs` 전수 실측):**
- **상호재귀는 실제로 존재한다**: 방출된 `subty_*` 호출 그래프에 크기>1 SCC가 **11개**
  (최대 26심볼 — `subty_expressionIR` ↔ `subty_argumentIR` ↔ `subty_callableTargetIR` …;
  그 외 `expression` 계열 17, `statementIR`/`blockStatementIR` 계열 8, `statement` 계열 6).
- **생성 종료**: `require`가 compound 타입(`TupleT`/`IterT`)에 대해 **먼저 `Helper_defs.add`
  하고 나서** 성분으로 재귀하고, 명명 타입은 `VarT` leaf라 재귀를 끊는다(규칙은
  `defs_of_typ`가 spec의 `TypD` 리스트를 1회 순회하며 방출) ⇒ SCC가 있어도 fixpoint.
  실측: `op subty-*` 중복 선언 0건, `eq subty-*` 완전중복 0건(7,310 등식).
- **재기록 종료**: 생성자 패턴 lhs의 재귀 호출은 전부 패턴 변수(진부분항)를 인자로 받는다 —
  실측으로 **lhs 인자 전체를 그대로 다시 `subty_*`에 넘기는 규칙 0건**. 유일한 비-하강
  형태는 alias 위임 `subty_T(x) -> subty_U(x)`(bare 변수 lhs, 71개)인데 이 위임 그래프는
  **비순환**(SCC 없음)이라 유한 체인 뒤 반드시 구조 하강 규칙에 도달. ⇒ 서브텀 순서 +
  비순환 alias 체인의 사전식 조합으로 종료.

## SCC (Sufficient Completeness Checker) — 배관 완료, CETA 바이너리만 남음 (2026-07-11)

**"빠진 케이스 = stuck 후보"를 정적으로 사냥하는 축.** `check_diff_p4.sh`의
completeness gap(Maude가 stuck → under-accept)을 1568개 프로그램의 경험칙이 아니라
**모든 항에 대해** 묻는 것이 sufficient completeness다. `subty_*` totality(사용-기반
false-보완으로 손수 닫은 것)도 이 성질의 국소 버전.

현재 상태: `[ctor]` 방출(선행)·`--unconditional` 변환·`run-scc.sh` 배선이 전부 끝났고
`badd` 슬라이스로 end-to-end 검증됨(프루닝 737→7 ops, `[ctor]` 보존, verdict 파싱).
**남은 건 CETA 링크 바이너리 하나** — 그게 없으면 SCC가 `ERROR-NO-CETA`로 정직하게 거절한다.

### 원인 — CETA 훅 미바인딩

MFE 3.5.1 배포본에 SCC 2b는 들어 있고(`tools/mfe/src/SCC/scc.maude`, 배너에도 뜸)
`(select tool SCC .)` 까진 되지만, 실제 검사는 안 된다:

```
MFE> (select tool SCC .)   →  The SCC has been set as current tool.
MFE> (scc SPEC .)          →  Warning: The sufficient completeness checker is not
                              fully available. Please use the trust command ...
```

**원인 — CETA 훅 미바인딩.** SCC의 핵심 연산은 트리 오토마타 공허성 판정
`op test-emptiness : Module ~> EmptinessResult [special (id-hook CetaSymbol ...)]`
(`tools/mfe/src/SCC/scc.maude:792`)인데, 이건 **CETA 라이브러리를 링크해 빌드한
Maude(Maude++)** 에만 있는 빌트인이다. 우리 바이너리 둘 다 없음
(`strings maude | grep -ci ceta` → 0):

| 바이너리 | ceta | TerminationCheckerSymbol |
|---|---|---|
| `tools/maude/maude` (stock 3.5.1) | 0 | 0 |
| `tools/maude271-hooks/maude` (v2.7.1-ext-hooks) | **0** | 2 |
| `maude-2.7-hooks-linux` (v2.7-ext-hooks 릴리스 에셋) | **111** | 2 |

주의: `v2.7.1-ext-hooks` 릴리스 **제목**은 "CETA library and MTT hooks"라고 하지만,
그 리눅스 에셋에는 CETA가 실제로 안 들어 있다(MTT 훅만). CETA가 들어 있는 건
한 세대 **아래**인 `v2.7-ext-hooks`의 `maude-2.7-hooks-linux.zip` 뿐이다. 구형
스택(`maude271-hooks` + `mfe271`, SCC 2a 번들)으로 돌려도 같은 warning이 난다 —
즉 MFE 버전 문제가 아니라 **바이너리 문제**.

**설치·구동 (완료 2026-07-11).** `maude-2.7-hooks-linux.zip`(v2.7-ext-hooks)을
`tools/maude27-ceta/`에 풀고(gitignore됨) `MAUDE_LIB`을 거기로, 구형
`tools/mfe271/MFE-mfe-2.7.1/src/mfe.maude`(SCC 2a 번들)로 `(scc SPEC .)`.
[tools/mfe/run-scc.sh](../../tools/mfe/run-scc.sh)가 `run-termination.sh` 패턴을
그대로 복제(슬라이스 덤프 → `prune_slice_signature.py` → 헤더 변환 `(mod`→`(fmod` →
구형 MFE). **버전 스큐 주의**: 구형 Full Maude가 Maude 2.7 아래서 자기 소스에 대해
무해한 `no parse` 경고를 뿜는다 — verdict를 먼저 파싱해야 하고, 안 그러면 판정이 난
실행을 ERROR로 덮는다.

### 판정 읽는 법 (극성이 전부다)

**1. `ceq`→`eq` 변환이 전제.** `CC-CONFIG`의 `drop-bad-eqs = true`
([scc.maude:941-944](../../tools/mfe/src/SCC/scc.maude))가 **조건부·비선형 방정식을
검사 전에 드롭**하므로, 그냥 먹이면 그 심볼의 규칙이 통째로 사라져 판정이 무의미해진다.
그래서 `rewrite --ctrs --unconditional`(=`Rewrite_system.drop_conds`+`linearize_lhs`,
커밋 f1b979c1)로 우리가 먼저 변환해 드롭될 게 없게 만든다. SCC는 오토마타 전이를
**`lhs(Eq)`에서만** 만들므로(`sca-eq-rules`) rhs는 봐도 그만 — 조건 제거로 rhs가
unbound가 되는 규칙(p4 1,164건)은 rhs를 lhs로 대체한다(`nonexec`가 되면 SCC의
`is-exec?`가 도로 드롭).

**2. 그래서 반례만 신뢰한다.** 조건 제거·선형화는 **매칭 과대 근사** → 빠진 케이스를
가릴 순 있어도 없는 걸 만들어내진 못한다. ⇒ **COUNTEREXAMPLE은 건전**(변환 여부 무관),
**COMPLETE는 변환 안 된 심볼(`exact`)에서만 증명**. run-scc.sh가 fidelity 열로 구분한다.

**3. 규모 실측 (2026-07-11, p4 분석 표면).** 예전 이 절에 있던 "우리 표면은 `ceq`가
압도적"이라는 서술은 **틀렸다**: 실제로는 `eq` 72,518 / `ceq` 2,511. 심볼 기준
2,403개 중 **무조건-only 1,722**(= SCC가 규칙 전부를 보는 `exact` 심볼: `subty_*` 486,
`match_*` 465, 함수 435, relation·프리루드 285, `holds_*` 51), 혼재 394, `ceq`-only 287
(전부 함수·relation — 의미론 본체). ⇒ 본체는 여전히 `approx`지만, **`subty_*`/`match_*`/
산술 프리루드는 온전히 검사된다** — 마침 우리가 totality를 손수 넣은 바로 그 가족들.

### 실측 결과 — 산술 프리루드 (2026-07-11, 전부 `exact`, SCC 자기 분석도 `complete+sound`)

| 심볼 | verdict | witness |
|---|---|---|
| `bsucc` | **COMPLETE** | — (total) |
| `badd` / `bmul` | COUNTEREXAMPLE | `badd_carry(bzero, bzero)` |
| `bsub` | COUNTEREXAMPLE | `bpred_double(bzero)` |
| `bdiv` / `bmod` | COUNTEREXAMPLE | `bdiv(bone, bzero)` / `bmod(bone, bzero)` |

**트리아지 — 둘 다 진짜 gap이 아니라 이미 알고 있던 두 성질의 기계적 재발견.**

- **canonicity 불변식** (`badd_carry(bzero,·)`, `bpred_double(bzero)`): `bd0`/`bd1`은
  절대 `bzero`를 감싸지 않는다는 이진 인코딩의 불변식 때문에 도달 불가. [prelude.ml:294-297]
  (translate/prelude.ml)이 **"verified by hand for every rule below, not enforced by the
  sort system"** 이라고 명시한 바로 그것 — SCC가 그 문장을 기계적으로 확인해 준 셈이다.
  `BPos < BNatV` 서브소트를 넣으면 COMPLETE로 떨어지는데, 그 서브소트는 `Maude_sorts`가
  심볼당 시그니처 1개만 지원해서 포기했던 것(835d1537). **⇒ SCC가 그 설계 타협의 비용을
  정확히 정량화한다: 불변식이 깨져도 타입 시스템이 안 잡고 조용히 stuck된다.**
- **의도적 부분성** (`bdiv`/`bmod`의 0 제수): 제수 패턴이 `bone`/`bd0`/`bd1`뿐 — 0으로
  나눈 값은 없다. 인터프리터는 에러, Maude는 stuck ⇒ 양쪽 다 거부라 differential 정합
  (completeness gap 0과 모순 없음). `eqg`의 대각 밖 stuck과 같은 부류: 고칠 게 아니라
  문서화할 것.

### 실측 결과 — `subty_*`/`match_*` 표본 (2026-07-11, 3개만 시범)

| 심볼 | verdict | witness |
|---|---|---|
| `subty_typeIR` | COUNTEREXAMPLE | `subty_list_fieldValue(bone)` |
| `subty_value` | COUNTEREXAMPLE | `subty_value(bone)` |
| `match_typeIR_BOOL_0` | COUNTEREXAMPLE | `match_typeIR_BOOL_0(false)` |

**셋 다 같은 원인 — op 도메인이 `Val`로 너무 넓다 (⇒ 협소화가 선행 과제).**
`op subty-typeIR : Val -> BoolV`, `op match-typeIR-BOOL-0 : Val -> BoolV`로 선언돼
있어서 `bone`(NatV < Val)·`false`(BoolV < Val) 같은 **엉뚱한 타입의 인자가 sort상
합법**이 되고, 당연히 어떤 규칙도 안 덮으니 반례가 된다. 실제 호출은 언제나 선언된
IL 타입의 값이라 **전부 도달 불가** — CRC MAYBE 트리아지와 같은 "witness를 보라"의
전형이다.

⇒ **이 가족이 SCC의 최고가치 타겟인데, 지금 그대로는 판정이 의미가 없다.**
`subty_<T>`/`match_<T>_<K>`의 도메인을 `Val`이 아니라 실제 IL 타입 sort(`TypeIR`
등)로 좁혀야 비로소 "COMPLETE = usage-based totality의 기계 증명"이 성립한다.

**⚠️ 단, 그냥 좁히면 안 된다 — `Val`-wide는 의도적이다.** `1874d212`가 정확히
그 반대 방향으로 갔다: matcher/subty/holds/eqg 도메인을 **무조건 `Val`로 넓힌** 이유가
"좁은 도메인이면 `Reflect.sibling_guard`가 바깥 타입 주어에 중첩 variant의 matcher를
불러 ill-sorted가 되고, owise 가드가 영구 stuck"이기 때문(48e59d5f의 협소화를 되돌린
것). ⇒ **선언 타입으로 좁히는 건 답이 아니다.**

### P1 해소 — 술어 도메인 = "쓰이는 주어들의 join" 고정점 (2026-07-14, 완료)

`Maude_sorts.predicate_domains`: 선언에서 **시드**(matcher→포함 타입, `holds_R`→R의 입력
타입, `subty_T`→T의 sort)를 잡고, **규칙에 실제로 나타나는 모든 인자 sort**(자기 정의
규칙이 해체하는 생성자 + 모든 호출부의 인자)로 **join**해 고정점까지 넓힌다. 결과는
정의상 "실제로 오는 주어들의 상한"이라 **1874d212의 stuck guard가 구조적으로 재발
불가**하면서, 스펙이 허락하는 만큼 좁다. `eqg`만 제외(임의 두 타입을 비교하므로 `Val`이
진짜 도메인).

- **실측(specs/p4)**: `match_` **464/465**, `subty_` **366/486**, `holds_` **39/51** —
  가족의 **86%**가 진짜 IL sort로 선언된다. 남는 `Val`은 전부 **컨테이너/스칼라
  erasure**(`cons`의 원소, 튜플 슬롯, `sub_nat`의 Int-or-Nat 주어)라 정직하게 `Val`로 둔다.
- 부수적으로 필요했던 두 가지: (1) `infer_var_sorts`에서 **술어 인자 위치는 변수 sort를
  좁히지 않는다**(consumer-only; 정의 규칙의 lhs head는 계속 authoritative) —
  안 그러면 바깥 타입 변수가 중첩 타입으로 조용히 좁아져 lhs 패턴 의미가 바뀐다.
  (2) `stuck_head_eqs`의 가드 패턴을 **`Val` 고정**(head가 뭔지만 묻는 등식인데, 잡아야 할
  대상이 바로 인자가 `Val` kind에 뜬 stuck 항이다).
- **슬라이스 주의(치명적)**: 도메인은 **풀 시스템**에서 복원해야 한다(`~sig_rules`).
  슬라이스/`--unconditional`은 **호출부를 지우므로**, 거기서 join하면 도메인이 seed로
  붕괴하고 SCC가 "아무도 실행하지 않는 시스템"에 대해 COMPLETE를 준다.
- **무회귀**: impty 골든은 op 도메인 줄만 변경(`--wide-predicate-domains`로 돌리면
  byte-identical), `dune runtest` 통과, impty 8/8(native+run-structural),
  p4 `--check-p4` 12/12 MATCH·`run-structural` 6/6(두 모드 동일), CRC `$lookup` YES/YES.

### 실측 결과 — 협소화 후 (2026-07-14)

| 심볼 | 2026-07-11 | 지금 |
|---|---|---|
| `match_typeIR_BOOL_0` | COUNTEREXAMPLE `match_typeIR_BOOL_0(false)` | **COMPLETE** (exact) |
| `subty_value` | COUNTEREXAMPLE `subty_value(bone)` | COUNTEREXAMPLE `sub_nat(invalidHeaderValue)` — **`dom:Val-wide`** |
| `subty_typeIR` | COUNTEREXAMPLE `subty_list_fieldValue(bone)` | COUNTEREXAMPLE `subty_prefixedNameIR(setValue)` — **`dom:Val-wide`** |

`run-scc.sh`가 이제 반례마다 **witness의 head 심볼 도메인**을 `dom:narrow|Val-wide`로
자동 표시한다(트리아지 1번 질문의 기계화). 남은 두 반례는 **둘 다 아직 `Val`인 심볼**을
가리킨다 — 즉 위양성이 `subty_*` 자신에서 **한 단계 아래 Val-wide 심볼로 이동**했다:

- **`sub_nat` (프리루드)**: `IntV`(`int_pos`/`int_neg`)와 `NatV`(`bzero`/`bone`/`bd0`/`bd1`)
  **양쪽에서 호출**되는데 두 sort의 공통 상위는 `Val`뿐이라 join이 `Val`로 떨어진다.
  → 후속 선택지: (a) 합성 union sort `Scalar`(`NatV`,`IntV` < `Scalar`)를 도입해
  `sub_nat : Scalar -> BoolV`로 선언하면 6개 규칙이 그 도메인을 총망라 → COMPLETE.
  (b) 애초에 **주어의 정적 타입이 이미 nat이면 `sub_nat` 호출 자체가 자명하게 true**다
  (`subty_value(integerLiteral_W(x0:NatV, ..)) = and(sub_nat(x0:NatV), true)`) — `sub_pred`가
  그 경우 `true`를 바로 방출하면 호출이 사라진다. (b)가 더 근본적.
- **`subty_prefixedNameIR` 등 120개 subty_**: 컨테이너 원소를 통해서만 불려서 `Val`에 남음.

**⇒ COMPLETE는 도메인 폭과 무관하게 항상 유효한 증명이다**(더 넓은 도메인에서의 totality는
더 강한 진술). 신뢰도 캐비엇이 필요한 건 **COUNTEREXAMPLE 쪽뿐**이고, 그건 이제 `dom:` 열이
자동으로 알려준다.

### 전수 스윕 결과 — `subty_`/`match_` 951심볼 (2026-07-14)

`--slice-dir`로 2,403개 슬라이스를 **한 번의 번역**(30분, 2.7G)으로 덤프하고
`run-scc-sweep.sh '^(subty|match)_'`로 951개 전부를 검사했다. **951/951이 `exact`
fidelity + `analysis:complete+sound`** — 즉 SCC가 우리 규칙을 그대로 보았고 자기 분석도
sound하다고 선언했으므로, 아래 판정은 전부 액면 그대로 읽어도 된다.

| verdict | 수 | 내역 |
|---|---|---|
| **COMPLETE** | **503** | `match_` 464/465 + `subty_` 39 — **totality의 기계 증명** |
| COUNTEREXAMPLE | 448 | `dom:Val-wide` 405 / `dom:elem-erased` 43 |
| **`dom:narrow`** | **0** | — |

**`dom:narrow` 반례가 하나도 없다** = 이 가족에서 SCC가 찾아낸 **진짜 빠진 케이스는 0건**.
2026-07-11에 위양성이던 `match_*` 465개는 이제 464개가 COMPLETE다(유일한 예외 `match_cons`도
`dom:Val-wide`).

**반례 448건의 정체 — 448건 중 407건은 검사 대상 심볼이 아니라 그 의존성을 가리킨다.**
witness의 head를 세면 `subty-boolTypeIR` 77 / `subty-name` 59 / `subty-expression` 58 / …
— **아직 `Val`로 남은 소수의 `subty_` 심볼 하나가 그것을 쓰는 모든 슬라이스를 오염시킨다.**
따라서 잔여 `Val`을 지우는 것이 곧 반례를 지우는 것이다.

**그 중 절반은 join의 모호성이었다(해소, `3cde77b4`).** `BoolTypeIR`의 상위는
`BaseTypeIR`/`TypeIR`/`TypedefIR` 셋으로 **비교 불가** — p4의 union이 겹치므로 관측 주어들의
최소 상계가 여럿이고 join이 아예 없다. `lub`가 그때 `Val`로 후퇴했는데, **공통 상계는 어느
것이든 정의상 well-sorted**(모든 관측 주어를 지배)이므로 그 중 **최소·최협소인 것을
결정적으로** 고르면 된다. → `subty_` 도메인 366/486 → **404/486**, `subty_boolTypeIR`은
`Val` → `TypeIR`이 되고 SCC 판정이 **COUNTEREXAMPLE → COMPLETE**로 뒤집혔다
(`match_` 464/465·`holds_` 39/51 무변화, impty 골든 무변화, p4 `--check-p4` 6/6 MATCH).

**남은 82개 `subty_`의 `Val`은 다른 원인 — 보완(complement) 절이 무관한 소스 타입까지 넘나든다.**
`subty_name`의 주어를 세면 `Name` 변수 외에 `nameList`·`typeParameterList`(리스트 타입!)
생성자가 섞여 있고, `subty_expression`의 반례 witness는 `setValue`(**Value** 생성자)다.
리스트 sort와 `Name`의 공통 상위는 `Val`뿐이라 join이 진짜로 `Val`이다. 즉 `sub_complement_defs`가
"타깃 `T`에 대해 어딘가에서 소스로 등장한 **모든** 타입"의 생성자에 `= false` 절을 다는 탓에,
서로 무관한 union을 가로질러 도메인이 벌어진다. **후속(P3)**: 보완 절을 실제로 검사되는
(소스, 타깃) 쌍으로 제한하거나, 타깃별이 아니라 (소스,타깃)별 술어로 쪼갠다. 이걸 닫으면
`dom:Val-wide` 405건이 대부분 COMPLETE로 떨어질 것으로 본다.

`dom:elem-erased` 43건(`subty_list_value`/`subty_list_fieldValue` 등)은 **원소 타입이 `List`
sort에 남아 있지 않아서** 생기는 구조적 잔재로, 규칙 2(원소 변수 협소화)의 대가이자 호출부가
만들 수 없는 항이다 — 고칠 대상이 아니라 보고할 대상.

### 할 일
- [x] (선행) op 선언에 `[ctor]` 방출 — **완료 2026-07-11**, `to_mfe`(분석)·`to_maude`
  (실행) 두 표면 모두. 생성자 집합은 `Maude_sorts.is_ctor`/`ctor_attr`(이론별 스칼라 +
  컨테이너 + `il_ctor_syms`), 방정식을 가진 심볼이 생성자로 선언되는 걸 막는 데모션
  가드 포함(p4에서 발화 0건).
- [x] `tools/maude27-ceta/` 설치 + `run-scc.sh` — **완료 2026-07-11** (커밋 7e88a68c).
- [x] 산술 프리루드 + `subty_*`/`match_*` **최소 시범** — 완료 (위 두 표).
- [x] **(P1, 선행) 술어 op 도메인 협소화** — **완료 2026-07-14** (`Maude_sorts.predicate_domains`,
  위 절). 선언 타입이 아니라 **쓰이는 주어들의 join 고정점**. 가족의 86%가 진짜 sort로.
- [x] **프루닝 선행 수정** — **완료 2026-07-14**. `prune_slice_signature.py`가 subsort 경로의
  **내부 노드를 keep**하도록(p4에 2단 체인 286개 — `BaseType < RealTypeArgument <
  TypeArgument`). 도메인이 전부 `Val`이던 동안엔 모든 sort가 `< Val` 직결이라 무해했지만,
  좁힌 도메인은 실제 격자에 의존하므로 중간 sort가 잘리면 슬라이스가 ill-sorted가 된다.
- [x] **(P2, 선행) `--slice-dir` 배치 덤프** — **완료 2026-07-14** (`b97afab3`). 심볼당
  49.9초 재번역(=24시간)이 **한 번의 번역**(2,403 슬라이스, 30분)으로. `run-scc.sh`가
  `SCC_SLICE_DIR`로 소비하고, `run-scc-sweep.sh`가 작은 슬라이스부터 resumable하게 돈다.
- [x] **(P2) `subty_`/`match_` 전수 스윕** — **완료 2026-07-14** (951/951, 위 절).
  503 COMPLETE / 448 COUNTEREXAMPLE, **`dom:narrow` 0건**.
- [x] **(P3) `sub_nat`의 Val 도메인 해소** — **완료 2026-07-14** (`381c6bd0`). `NatV`,`IntV`
  `< NumV` 상위 sort를 (스펙이 `sub_nat`에 실제 도달할 때만) 선언 → 6개 규칙이 도메인을
  총망라, 반례 소멸. impty 골든은 byte-identical.
- [x] **(P3) 모호한 join의 `Val` 후퇴 해소** — **완료 2026-07-14** (`3cde77b4`). 최소 상계가
  여럿일 때 `Val`이 아니라 그 중 최협소한 것을 결정적으로 선택.
- [ ] **(P3, 다음) 잔여 82개 `subty_`의 `Val`** — 원인은 보완 절이 무관한 소스 타입을
  넘나드는 것(위 절). 보완을 실제 검사되는 (소스,타깃) 쌍으로 제한 → `dom:Val-wide` 405건
  대부분이 COMPLETE로 떨어질 전망.
- [ ] **(P2) 나머지 심볼 스윕** — 함수 435 / relation·프리루드 285 / `holds_*` 51, 그리고
  `approx` 756개(COMPLETE는 무시하고 **COUNTEREXAMPLE만** 수확 → 진짜 stuck 후보 목록).
  슬라이스는 이미 덤프돼 있으니 `run-scc-sweep.sh <pattern>`만 돌리면 된다.
- [ ] (선택) `BPos < BNatV` 서브소트 — `Maude_sorts`의 심볼당-단일-시그니처 제약을
  풀어야 함. 풀면 canonicity 반례 3개가 COMPLETE로 떨어지고, 불변식이 sort로 강제된다.

## LTL 모델 검사 — P4 언어의 시간적 성질 검증 (신규, 미착수)

**동기.** 지금까지 Maude는 두 축으로만 쓰였다: (1) **실행 엔진**(`reduce` — 프로그램
하나의 타입검사 결과), (2) **분석 게이트**(CRC/ChC/termination — 재작성 시스템 자체의
성질). 어느 쪽도 "**P4 프로그램이 실행 도중 무엇을 지키는가**"(header validity, parser
종료, table 적용 결정성 …)를 묻지 않는다. Maude의 **LTL 모델 검사기**
(`model-checker.maude`, `modelCheck(init, φ)`)가 세 번째 축 — 시간 논리 공식 φ를
상태공간 전체에 대해 검사하고, 실패하면 **반례 경로**(`counterexample(path, cycle)`)를
돌려준다.

**LTL은 Kripke 구조를 요구한다 — 지금 우리 모듈엔 그게 없다.** 필요한 네 조각:
상태 sort(`State`), **`rl` 전이 규칙**, 원자명제 sort(`Prop`), 만족 관계
(`op _|=_ : State Prop -> Bool`). 현재 p4 모듈은 relation이 전부 input-moded라 **`rl`이
0개**(전부 `eq`) — 전이가 없으니 상태공간 자체가 없고 `modelCheck`는 무의미하다. 아래
(P1)·(P2)가 그 구조를 만드는 일이고, (P3)이 LTL 본체다. `search`는 이 축의 목표가
아니라 **LTL 배선을 만드는 김에 거의 공짜로 얻는 보조 반증 도구**로만 다룬다(아래).

**전제 조건 (배선부터 필요).**

- [ ] **(P1) 선택적 `rl` 모드 — Kripke 전이 만들기.** `--relations-as-rules`는 *모든*
  relation을 rl로 바꿔 상태공간을 무의미하게 키운다. 필요한 건 **동적 의미론의 step
  relation만** rl로 두는 선택적 플래그(`--rules-for R1,R2` 같은 것). 시작항도
  `Program_ok(..)` 판정 호출이 아니라 **전이할 상태**여야 한다. 이게 LTL의 유일한
  하드 블로커다.
  **⚠️ 2026-07-14 삭제 주의**: 모드-힌트 기반 rl 선택 기계 —
  `To_ctrs.rule_head_syms`/`input_moded_rel_syms`, `To_mfe`/`Mfe.check`/
  `fold_premise_binders`의 `~rule_heads` 배선, 그리고 `To_maude.rule_relation_syms`의
  **ceq-는-`=>`-조건-불가 fixpoint**(eq로 남은 관계의 조건이 rl 관계를 호출하면 그
  관계도 crl로 강등) — 는 모든 관계가 입력-모드라 사어여서 삭제됐다(이 항목 작성
  당시에도 `rule_head_syms`는 ∅). `--rules-for` 구현 시 (a) 선택 집합 배선과 (b) 그
  fixpoint 적법성 제약을 다시 들여와야 한다. 복원 참조: `git log -S rule_relation_syms`
  직전 트리(삭제 커밋 부모)에서 — 기존 삭제-모듈 관례와 동일.
- [ ] **(P2) `modelCheck` 배선.** `MODEL-CHECKER` 모듈 protecting + step relation의 상태
  sort를 `State`로 subsort + `Prop` 선언 + `_|=_` 등식. **설계의 핵심 질문은 "원자명제를
  스펙에서 어떻게 얻는가"** — 유력 후보는 `Reflect`가 이미 만드는 **`holds_R`**(무출력
  judgment의 성공 반사, 무조건 규칙 1개, BoolV 시그니처)를 그대로 명제로 쓰는 것
  (`eq S |= p_R = holds_R(S)`). 결과 디코딩(`true` vs `counterexample(...)`)엔 `Of_maude`에
  **전이-경로 파서**가 필요하다. CLI는 `run`에 `--ltl 'φ'` 정도로 붙이면 된다.
  **⚠️ 유한성 제약**: P4 **타입검사**의 상태공간은 항 크기가 무제한이라 무한하다 —
  modelCheck가 완주하려면 **고정 프로그램 + 고정 입력의 동적 의미론**처럼 상태공간이
  유한한 곳에 걸어야 한다. (무한 상태공간에서 반증만 원한다면 depth-bounded search가
  대안 — 아래 P3.)
- [ ] **(P3, 보조) `Maude_run.Search` 일반화.** LTL이 감당 못 하는 무한/거대 상태공간에서
  **bound된 반증**을 얻는 용도. 현재는 디버깅용 근사다
  ([maude_run.ml:148-152](maude/maude_run.ml#L148-L152)):
  `metaSearch(M, start, 'R:Val, nil, '!, unbounded, N)` — 패턴이 bare 변수, 조건 `nil`,
  화살표 `'!`(`=>!`) 고정, 깊이 unbounded, 해는 인덱스 `0..cap-1` 순차 열거
  (`search_cap = 100`). 필요한 건 임의 **search pattern** + **`such that` 조건**
  (metaSearch의 3·4번째 인자), 화살표 `=>*`/`=>+`/`=>1` 선택, **깊이 bound**, 그리고 반례
  경로용 `metaSearchPath`. "반례 없음"은 기존 `NoSolution`을 그대로 쓴다. (P2)의 경로
  파서와 디코딩 코드를 공유한다.

**검증 대상 LTL 성질.** 전부 **반증 지향** — 반례가 나오면 진짜 결함, 안 나오면 (유한
모델/bound 안에서) 성립.

- [ ] **header validity 안전성**: invalid header를 절대 읽지 않는다 — `[] ~ readInvalid`.
  LTL이 가장 자연스럽게 잡는 성질이고 첫 타깃으로 적합.
- [ ] **parser 종료/도달성**: P4 parser는 루프 가능 — `<> (accept \/ reject)`(항상 결말에
  도달), `accept` 도달성, 무한 루프 검출.
- [ ] **progress (무-stuck)**: well-typed 프로그램은 stuck 상태에 안 빠진다 —
  `[] ~ stuck`. 현재의 `Stuck` 판정을 프로그램 하나가 아니라 **상태공간 전체**로 확장.
- [ ] **preservation**: 모든 도달 가능 상태가 well-typed — `[] p_Type_ok`
  (`holds_Type_ok`를 원자명제로).
- [ ] **결정성**: 종료 상태가 유일 — LTL로는 직접 못 쓰므로 `search P =>! X`의 해가 정확히
  1개인지로 확인(P3 몫). CRC(등식 층의 합류성)와 상보적 — rl 층 + 실제 도달 가능한 상태만
  본다.
- [ ] **table entry 우선순위 결정성**: 같은 키에 매칭되는 두 엔트리의 우선순위가 동률이면
  적용 순서가 비결정 — search로 반례 탐색(P3 몫).

**권장 착수 순서**: (P1) rl 모드 → (P2) modelCheck 배선 → header validity(`[] ~ readInvalid`)
를 기존 corpus 프로그램 하나에 걸어 end-to-end 확인 → parser/progress/preservation LTL →
(P3) search 일반화로 무한 상태공간 성질(결정성·table 우선순위) 보강.

## 권장 순서

(이미 복구·완료: CLI 배선, `Exp_map`, `Defunctionalize`/`Gensym`/`Builtin` + 파이프라인,
`Simplify`=identity, Maude 백엔드 `To_maude`/`Maude_run`/`Of_maude`,
`To_ctrs.of_spec`/`var_type_hints` 포팅, **Native 직접 생성 리팩토링 (B)**,
**impty/base 분석 골든 고정**(`specs/impty/base/spec.ctrs`), **Mfe calibration**,
**M2 CLI `run` 실행 배선**, **M3 differential**(completeness 0 / soundness 1 /
Phase D 1227/1227 MATCH).)

(추가 완료 2026-07-02: **subty totality — 사용-기반 false-보완**(`sub_complement_defs`;
interp.subtyp 대조로 struct width/depth·타입파라미터·스칼라 근사 전부 종결),
**MFE 환경 재보정**(심볼당 ~4분, `--timeout 360`+ 필요).)

(추가 완료 2026-07-03: **owise 반사 확장 — 수집형 IterPr(`$itercollect`/`$iterapply`)
성공 반사**(`Reflect.gen_itercollect_holds`/`gen_iterapply_holds` + 전역 성공-테스트
삽입 + `cond_heads`의 재귀 term 스캔; p4 owise 51/72 → 66/72 반사, kept 21 → 6).)

(추가 완료 2026-07-03: **(B) match-가드 discriminator head-패턴 폴드**
(`Reflect.hoist_matchers`; 위 M1 "남은 14 MAYBE" 항목 참조) — `match_K(subj)=true`를
`subj=K(fresh..)`로 재철자해 기존 `fold_premise_binders`가 head로 접게 함, subj가 같은
rule의 다른 조건에도 안 쓰일 때만(companion destructure 충돌 회피). impty 골든 갱신
(`$lookup`/`Check-command`/`Eval-command`의 nullary 케이스 3건 head-fold, byte-identical
아님이지만 의도된 개선; 실행 표면 `spec.maude` 무변화 확인). **MFE 실측(p4 `$un_op`/
`$inherit_i` MAYBE→YES, 회귀 재검)은 이 세션에서 미완 — MFE 환경이 impty `$lookup`
대조군(과거 ~1.4s)조차 `--timeout 900`에서 TIMEOUT, `git stash` 베이스라인도 동일 증상이라
코드 회귀 아닌 기존 환경 문제로 확인(아래 M1 (B) 항목에 상세). 다음 세션에서 환경 복구 후
재검 필요.**)

(추가 완료 2026-07-03: **`$unzip`/`$iterproj` 규칙-생성 코드 통합 + reflect.ml
`$iterproj` 하드게이트 제거**(위 M1 항목 참조) — `to_ctrs.ml`의 공유 헬퍼
`spine_projection_rules` + `reflect.ml`의 `iter_helper_prefixes`에서 `$iterproj`
제거; impty golden·p4 전체 corpus `--ctrs`+`reflect:` stderr 모두 byte-identical,
`TableProperty_ok`/`Decl_ok` 슬라이스로 공유 경로 발화 확인. 이 세션 시작 시 (B) MFE
재검을 먼저 시도했으나 impty `$lookup` 대조군이 `--timeout 280`에서도 TIMEOUT —
환경이 여전히 미회복임을 재확인(회귀 아님), 그래서 환경 비의존적인 이 항목으로
전환해 완료.)

(추가 완료 2026-07-04: **owise 반사의 gensym-effectful 사전 게이트 제거**
(`reflect.ml`의 `owise` 함수 안, 형제 guard를 만들기도 전에 "이 규칙의 head 심볼이
effectful 집합에 속하냐"만 보고 무조건 막던 2줄 삭제). `gensym.ml`의 `thread_rule`이
컴파일 타임에 강제하는 불변식(effectful 규칙의 원래 LHS 패턴에는 절대 effectful
호출이 들어갈 수 없음 — 위반 시 `failwith`)과, `ctest`가 형제 조건을 훑을 때 이미
무조건 `check_reflectable`을 호출해 실제 effectful 언급을 정확히 잡아내는 기존
안전장치 덕분에, 이 사전 게이트는 불필요하게 보수적이었음이 실측으로 확인됨(예:
`$subst_typeIR`의 유일한 형제 절은 조건이 0개라 guard가
`eqg(theta, variant-set-lbrace-rbrace-1(nil))`뿐이라 gensym state를 아예 언급하지
않는데도 막혀 있었음). 2줄 삭제 후 p4: 66/72 → **71/72 반사**(5개
`$subst_typeIR`/`$subst_parameterIR`/`$subst_callableTypeIR`/
`$subst_callableTypeDefIR`/`$subst_constructorTypeIR` 전부 새 Gate 사유 없이 반사됨,
남은 1개는 무관한 `$find_local_return_type_t`). impty/base·impty/closure 골든(`spec.ctrs`/
`spec.maude`) byte-identical. 부수 효과로 레거시 `specs/p4-old`(활성 검증 대상 아님)에서도
동일 패턴 7개가 반사됨; 그중 `$compat_table_key`는 `check_reflectable`이 실제로 다른
gensym 심볼(`$compat_table_exact_optional_key`)을 잡아 정상적으로 계속 게이트되는 것도
확인 — 안전장치가 실전에서도 정확히 작동함을 보여줌.)

(추가 완료 2026-07-13: **`Reflect.align_guards` — 상보 비교/부정 가드 정렬로
sign-split MAYBE 해소**(`reflect.ml`, `pipeline.ml`의 `ctrs_of_spec`에서
`fold_premise_binders` 뒤·`owise` 앞). 조건 위치의 `lt(a,b)=true`를
`leq(b,a)=false`로(그리고 `_int`/`false`-극성 변형, 선두 `not(x)=b`는 `x=¬b`로)
재철자해, `i<0`(arith shift) vs `i>=0`(logical shift)로 갈리는 형제 절이 번역
후 갖게 되는 서로 다른 subject(`lt_int(X,0)=true` vs swapped `leq_int(0,X)=true`)를
**같은 subject `leq_int(0,X)`의 true/false 극성**으로 통일 → CRC가 가설 재작성으로
임계쌍을 discharge.
**근본 원인 규명**: prelude의 기존 bridge `lt_int(x,y)=not(leq_int(y,x))`가
이 쌍을 못 고쳤던 이유는 — 여기서 `X=$bitstr_to_int(..)`의 복원 sort가 최상위
`Val`인데 bridge는 하위 sort `IntV`에 선언돼 있어 **발화하지 못하기** 때문
(축소 슬라이스 CRC 실측: bridge 있는 채로도 6 임계쌍 생존). align_guards는
`leq_int` 심볼을 직접 써 sort와 무관하게 정렬하므로 이 문제를 우회.
**실측**(`ulimit -s unlimited` + 시그니처-축소 슬라이스 CRC, CRC 3t/Maude 3.5.1a):
`$bin_shr` 자기-레이어 12절 — 재철자 전 MAYBE(6 ccp) → 후 **YES(0 ccp)**;
`$bin_satplus` **YES**; `$bin_satminus`는 동형(같은 상보 형태)이나 축소 CRC가
산술 라이브러리 무게로 완주 미측정(sign-split 해소는 동일). 미니모듈 e1~e9로
CRC 판정 법칙 재확인 — "same-subject true/false 극성 충돌만 discharge, not()/
bridge 사슬은 sort가 맞을 때만 통과"(메모리 `mfe-crc-hypothesis-rewriting` v2).
**무회귀**: impty/base 골든 byte-identical(impty엔 `lt`/`not` 조건 없음), p4
전체 `--ctrs` 재철자 정합(조건 위치 `lt`/`lt-int`/최상위 `not(` 0건, subty·함수
부정은 `= false`로 평탄화), owise 반사 72 reflected/0 kept·subty expansion 정상
(align_guards가 뒤따르는 owise 반사를 안 깨뜨림 — 오히려 평탄화된 `not`을 owise가
정상 처리). 실행 표면 무영향(패스는 `ctrs_of_spec` 전용). CLAUDE.md의
"spurious CRC MAYBE — `$bin_shr`/`$bin_satplus` sign/range split" 트리아지 항목은
이 패스로 해소됨 — 갱신 필요.)

(추가 완료 2026-07-07: **MFE 환경 차단 해제 확인 + (B) 실측 완주 + B′ subty-가드
확장 + owise 72/72(ambiguous matcher 해소) + holds_R output-carrying 일반화 +
`drop_owise` 폴백 제거.** 상세:)

- **환경: 차단은 옛 WSL 환경 문제였고 현 dev 컨테이너(RAM 121GB/32코어, cgroup
  무제한)에서는 재현 안 됨** — impty `$lookup` 대조군 `--timeout 120` → **YES/YES
  1초**(과거 ~1.4s 수준 복원). p4 verify는 회당 ~110s의 translate 고정비 + CRC.
  과거 "심볼당 ~4분/TIMEOUT" 메모는 이 환경에는 적용되지 않음.
- **(B) MFE 실측 완주**: `$inherit_i`(10규칙) **YES/YES**, `$un_op`(287규칙)은
  환경이 아니라 슬라이스 크기가 원인으로 재분류(baseline `--timeout 600` TIMEOUT;
  subty 가드가 subty/보완 가족을 슬라이스로 끌어옴). impty `$lookup` 대조군 YES/YES.
  B′ 확장 후 `$un_op` 슬라이스는 287→**87규칙**으로 줄었는데, CRC가 이번엔 Peano
  산술 재귀 규칙군($int_to_bitstr/$bitstr_to_int류)의 임계쌍 계산에서 **maude
  native "Fatal error: stack overflow"로 사망**(verdict 없음으로 관측) —
  check_diff_structural_p4.sh가 이미 배운 것과 같은 교훈(e73fcb44, 기본 8MB 스택은
  정상적으로-깊은 계산에 부족)이라 [mfe.ml](mfe.ml) `run_mfe`가 maude를
  `ulimit -s unlimited` 셸 래핑으로 spawn하도록 수정. 무제한 스택에서는 크래시
  없이 계산이 지속됨(수동 재현 15분+ 생존, 8MB에서는 ~5분에 사망) — 잔여 장애물은
  스택이 아니라 87규칙 산술 슬라이스의 순수 CRC 임계쌍 비용 — 무제한 스택 재검에서
  **CRC 완주(~15분): verdict MAYBE + sort-decreasing**. 잔여 임계쌍(ccp SPEC100)은
  `$un_bnot`의 `$bneg(i)=i'` 출력 바인더가 rhs `$ite(..i'..i'..)`에 2회 쓰여
  `fold_premise_binders`의 중복-방지 게이트(`uses=1 || alias`)에 걸려 남은
  premise-witness 쌍 — **원인 1(전제-only 바인더)의 알려진 잔여 계열**, B′와 무관.
  baseline TIMEOUT 대비 "판정 가능"으로 개선.
- **owise 72/72 (kept 0)**: 마지막 kept `$find_local_return_type_t`의
  `Gate "ambiguous matcher"`는 `ctest`의 `type_of_l`이 struct 필드 접근자 체인의
  결과 타입을 복구하지 못해 `matcher_type`이 `ctor_types` 3-타입 폴백으로 떨어진
  것이 원인 — `tables`에 `fieldsigs`(struct 필드 타입)·`rel_outs`(relation 출력
  타입) 추가, `type_of_l`을 funcsigs → fieldsigs → 단일-출력 rel_outs 순 폴백으로
  확장([translate/reflect.ml](translate/reflect.ml)). 결과: owise 절이 union 타입
  `localKind`의 total matcher 가족(4 matcher × 9 케이스 지원규칙)으로 반사.
  p4 `--ctrs` diff는 정확히 그 owise 절 + 지원규칙뿐, impty 골든·실행 표면
  byte-identical.
- **(B′) subty-가드 확장 — `Reflect.expand_subty_guards` 구현·배선·실측 완료.**
  분석 파이프라인에서 `hoist_matchers` 뒤·`fold_premise_binders` 앞. `subty_S(v)=true`
  가드(v가 head-bound)를 CTRS 스캔으로 얻은 S의 멤버 생성자별로 **1규칙→N규칙
  팬아웃**(θ=[v:=K_i(expand_j..)] 전체 치환, 멤버 residual은 잔여 조건으로),
  클론별 **부분평가**: 콘크리트 ctor 위 matcher/subty 평가(true→조건 제거,
  false/stuck→클론 폐기 — 생성자는 free라 둘 다 unsatisfiable), 동반 destructure
  `K(sf..)=K(pats..)` 점별 분해, fresh-변수 rename(head 패턴 심화), head 밖
  존재변수의 `w=t`(t는 순수 ctor 항) 제거. alias 위임 체이싱, 타입파라미터
  `subty_T(x)->true`는 vacuous drop. 게이트: 멤버 >16 또는 규칙당 클론 budget
  64 초과 시 skip+stderr(`subty_expression` 31멤버 등 대형 variant가 이에 걸림 —
  의도된 보수성). fresh 이름 `expand_%d`는 Var_hints 키와 서로소(sort는 ctor
  시그니처에서 위치 복구).
  - p4: **1738절 → 2496클론(956 dead, vacuous 48)**, 실행 표면(native emit)
    sha256 불변. impty 골든은 의도된 개선으로 재생성(6절 → 6클론+4 dead;
    `Check-expr`/`Eval-expr`의 literal 절들이 무조건 `eq`로, id 절은 head 특수화 +
    존재변수 제거로 `$lookup(..)=some(t)` 한 줄로 접힘; `spec.maude` 불변).
  - **의미 보존 실측**: run-structural impty 8/8 `result: true`, p4
    `issue1301.p4` `--check-p4` **result: MATCH**.
  - **MFE 실측 (post-A, `--timeout 600`, B′ 7심볼 전부 + 회귀 표본):**

    | symbol | 착수 전 | after | 비고 |
    |---|---|---|---|
    | `$flatten_constOpt` | YES | **YES** (116s) | 기존 패스로 이미 해소돼 있었음 |
    | `$tableCustomName` | YES (425s) | **YES** (118s) | 슬라이스 축소로 3.6× 가속 |
    | `$name` | TIMEOUT@600 | **YES** (121s) | 343규칙 → 수십 규칙급 |
    | `$prefixedTypeName` | TIMEOUT@600 | **YES** (125s) | |
    | `$prefixedNonTypeName` | TIMEOUT@600 | **YES** (133s) | |
    | `$invalidate_value` | MAYBE | **YES** (213s) | 잔여 payload 가드 남아도 head 서로소화로 discharge |
    | `$invalidate_headerUnion` | MAYBE | **YES** (210s) | |
    | `$is_lpm_key_prime` (회귀) | YES | YES | 무회귀 |
    | `$inherit_i` (회귀) | YES | YES | 무회귀 |
    | `$join_ctk` (회귀) | MAYBE | MAYBE | 무회귀 — subty 아닌 match-가드 잔여((B)의 companion-destructure 게이트 케이스) |

    **B′ 7/7 YES — "남은 7 MAYBE(subty-disjointness)" 완전 해소.**
- **holds_R output-carrying 일반화 (negation-as-false-value 확장) 구현 완료,
  현 코퍼스에서는 휴면(byte-identical).** `gen_rel_holds`의
  `Gate "output-carrying judgment rule"` 제거(가드는 lhs 패턴+조건만 반사 —
  출력 무시가 곧 존재성 의미), `check_reflectable`의 relation Gate를 `succ` 미포함
  시로 완화, `qualified`를 "규칙 있는 모든 judgment"로 완화. 부정 respell은
  신설 `rel_output_kind`(rel_outs 기반)로 게이트: `No_output`(양/음 모두 respell),
  `Non_bool`(출력이 bool 리터럴일 수 없음 — `=false`만 respell),
  `Maybe_bool`(단일 bool 출력 — 출력값 false와 부정을 구문으로 구별 불가,
  **respell 금지**). 바인딩 call site는 기존 `insert_success_test`가 자동 커버
  (succ 포함 + 비-bool rhs 기준이라 코드 무변경). **p4/impty `--ctrs`
  byte-identical + succ 집합 47 불변으로 무회귀 확정** — p4에는 부정된
  output-carrying relation 조건이 0건(사전 스캔)이라 예측대로 소비자 없음.
  미래 spec에서 `~R(in)` (output relation)이 나타나면 즉시 동작.
- **`drop_owise` 폴백 제거 ([mfe.ml](mfe.ml)).** 72/72 반사로 no-op이 된 drop을
  제거하고, 대신 MFE 입력에 owise 규칙이 남아 있으면(미래 spec의 반사 Gate 회귀)
  `mfe: WARNING - N unreflected owise rule(s) reach the MFE input` stderr 경고 —
  침묵 false MAYBE 대신 원인이 로그로 드러남. CRC엔 보수적(허상 임계쌍 = MAYBE,
  false YES 불가). `Rewrite_system.drop_owise` 함수 자체는 유지(다른 소비자).
  p4/impty에서 경고 미출력(vacuous) 확인, impty verify YES/YES.

(binary 수 인코딩 트랙 진행 상황 갱신 2026-07-08 — 별도 체크아웃
`/home/spectec-core-binenc`, `origin/new-rewrite` 대비 10 commit 전진.
**이 체크아웃/잡은 병행 세션이 사용 중이니 건드리지 말 것**(빌드/재실행 겹치면
in-flight 결과 손상, 과거에도 겪음):

- **Phase 0–3 (BNatV 이진 인코딩 기본기) 완료.** `bzero`/`bone`/`bd0`/`bd1`
  스캐폴딩(`50d2abdc`) — 처음엔 `BPos<BNatV` 서브소트로 설계했다가
  `Maude_sorts`가 심볼당 시그니처 1개만 지원해 서브소트가 안 먹혀 **단일
  `BNatV` 소트로 축소**(`835d1537`) → `bsucc`/`bpred`/`badd`/`bmul`/
  `bcompare`/`bleq`/`blt`(`b84dfb59`) → 절단 뺄셈 `bsub`(3치 마스크, Coq
  `Pos.sub_mask` 스타일, `d8ca81a7`) → **O(log n) 이진 장제법** `bdiv`/`bmod`
  (`99266995`, naive 반복뺄셈 대비 결정적 승리 — `2^64/10`이 1133 rewrite로
  끝남, 반복뺄셈이면 ~1.8e18 스텝) → `bpow_nat`(`397be13c`). 전부
  unreferenced 상태에서 단독 MFE confluence 확인 후 착륙(impty/p4 골든
  no-op 검증 포함).
- **Phase 4 "스위치 전환" 완료(`69023118`).** `int_pos`/`int_neg`의
  magnitude를 Peano `NatV`→`BNatV`로 retype, 동시에 `builtin.ml`
  (`shr`/`shr_arith`/`pow2`/`band`/`bxor`/`bor`/`int_to_text`)까지 같이
  리타겟(분리 불가 — 안 그러면 두 커밋 사이 기간에 tree가 ill-sorted). 이
  시점부터 실제 P4 int가 이진 인코딩으로 실행됨.
- **스위치 전환 후 실 corpus 실행에서 버그 3개 순차 발견·수정**(전부
  impty/synthetic 골든은 못 잡던 것 — 실 corpus 최초 실행에서만 드러남):
  1. `negate_int`의 involution 단축 규칙이 BNatV에서 CRC MAYBE 유발 →
     제거 + CRC용 `ulimit -s unlimited` 배선(`mfe.ml`/
     `Maude_run.run_process`) — `59b5e10c`.
  2. `nat_of_int`가 옛 Peano 시절 규칙(`rule (nat_of_int_t (int_pos_t x)) x`)을
     그대로 갖고 있어 `bit<n>` 있는 프로그램이 전부 첫 스텝에서 stuck →
     `bnat_to_nat` 역브릿지 추가로 수정(`fc01f90c`). **커밋 메시지가 명시:
     이 수정 전에 수집된 binary-encoded 구조적 corpus differential 결과는
     전부 무효, 재실행 필요.**
  3. `abs_nat`/`sub_int_nat`가 `maude_sorts.ml`에 여전히 Peano `NatV`
     시그니처로 선언돼 있어(규칙 자체는 맞았음) 음수 상수 폴딩류 프로그램이
     stuck → 시그니처만 `BNatV`로 수정(`06b05760`, 2026-07-07, 현재 최신
     커밋).
- **진행 중(미커밋, working tree).** `to_ctrs.ml`의 `char_rules` 생성을
  스펙 텍스트 정적 스캔 결과에서 **printable ASCII 전체(32–126) 유니온**으로
  확장 — 인코딩된 시작항은 대상 P4 프로그램의 식별자/문자열 리터럴을
  담는데, 정적 스캔은 스펙 자체의 규칙 텍스트만 보므로 스펙에 안 나온
  바이트(예: 제네릭 파라미터 이름 `T`)는 `chr`는 선언되면서 `eq`가 안 생겨
  stuck. 이 수정 아래 **전체 1568개 corpus 재실행이 진행 중**(로그
  `check_diff_structural_p4_charfix.log`, 2026-07-08 02:56 시작, 확인 시점
  기준 90/1568 완료, 10개 배치당 ~4분 → 완주까지 대략 10시간+ 예상) — 결과
  미확정.
- **참고**: 아래 남은 작업 목록의 "Peano가 bit<32+>에서 OOM" 항목이 가리키던
  원래 기준선(2026-07-07, 메인 체크아웃 Peano 인코딩으로 RESULT 3 MATCH /
  0 MISMATCH / 179 other)은 이 binenc 트랙의 **출발점**이었을 뿐, binenc
  자체의 현재 결과가 아님 — binenc의 첫 실제 결과는 위 진행 중인 재실행이
  끝나야 나옴.)

(추가 완료 2026-07-11: **`[ctor]` 방출 (SCC 선행 작업) + `run`의 타임아웃 기본값 제거.**

- **`[ctor]`**: `Maude_sorts`가 생성자/정의된-심볼 분할을 갖게 됨 — `il_ctor_syms`
  (variant 케이스 + struct 생성자; 필드 접근자/갱신자는 **정의된 심볼**이라 제외,
  선언 커버리지용 `il_declared_syms`와 분리), `shared_ctor_syms`(컨테이너),
  이론별 `scalar_ctor_syms`, 그리고 공유 `ctor_attr`. **이론별 분리가 핵심**:
  `int_pos`/`int_neg`는 Structural에선 생성자지만 Native에선 Maude 내장 `int(_)`로
  가는 **브리지 방정식을 가진 정의된 심볼**이다(데모션 가드가 실제로 잡아냄).
  `To_mfe`(분석)·`To_maude`(실행) 두 표면 모두 방출. 가드: 생성자로 등록됐는데
  방정식이 있으면 `[ctor]`를 떼고 stderr 경고(거짓 주장 방지). p4/impty 양쪽 경고
  0건, impty 실행 8/8 정상, `$lookup` CRC/ChC YES 무회귀. 골든 `spec.ctrs`/
  `spec.maude` 재생성. **SCC 선행 조건 1(= `[ctor]` 방출) 완료** — 남은 건 CETA
  훅 바이너리.
- **`spec.maude` 골든의 matcher-sort "회귀"는 회귀가 아니었음**: `1874d212`
  (match_/subty_/holds_/eqg 도메인을 무조건 Val로 넓힘 — 좁은 도메인이면
  `Reflect.sibling_guard`가 바깥 타입 주어에 중첩 variant의 matcher를 불러 ill-sorted,
  owise 가드가 영구 stuck)가 **골든을 재생성하지 않았을 뿐**. `spec.ctrs`는 이후
  커밋들이 우연히 따라잡았고 `spec.maude`만 stale로 남아 있었다 — 이번에 갱신.
- **`run`/`run-structural`의 `--timeout` 기본값 30초 → 0(무제한)**. p4 native 실행
  실측(2026-07-11): 번역 ~10s + **Maude 모듈 내부화 고정비 ~80s** + 프로그램당 ~6.5s
  (1개 91.6s vs 3개 배치 104.7s로 분리). 고정비가 기본 타임아웃보다 커서 `run --p4`가
  **첫 프로그램을 시작하기도 전에 항상 TIMEOUT**났고, 그 탓에 `check_diff_p4.sh`의
  per-file fallback 경로(`--timeout 0`을 안 넘김)가 조용히 전부 무효였다. 실행 자체는
  정상이었음(타임아웃 늘리면 3/3 `result: MATCH`). 모듈 규모(~78k줄/~74k eq)는
  binenc 병합 이전 커밋(`199c72eb`)에서도 동일 — 최근 회귀 아님, CLAUDE.md의
  "~10s 내부화 / ~4ms per program" 수치가 stale이었던 것(subty 보완 5,425 eq 이전 값).
  CLAUDE.md 성능 절도 실측표로 갱신.)

(추가 완료 2026-07-11: **동반 destructure에 함의된 잉여 매처 가드 제거 —
termination MAYBE의 지배적 원인 해소.** `hoist_matchers`와 `fold_premise_binders`가
**서로를 막고 있었다**: 절 하나가 `match_K(v) = true`와 `v = K(..)`를 둘 다 들고 있으면
(SpecTec 엘라보레이터가 `matches K` 가드와 `let K(x,y) = v`를 별개 전제로 내보내므로 흔한
모양), hoist는 "v가 다른 조건(destructure)에도 나온다"고 skip하고, fold는 "v가 다른
조건(매처 가드)에도 나온다"고 skip한다. 결과적으로 둘 다 남고 **head가 맨 변수로 굳는다**.

- **증상**: 리스트/구조 재귀의 감소 인자가 head가 아니라 **전제에만** 존재
  (`ceq $f(v) = ..$f(v_t).. if match_cons(v)=true /\ v = cons(v_h,v_t)`). AProVE의
  dependency-pair 분석은 `v_t < v`를 세우지 못해 MAYBE — 루프가 아니라 **증명 실패**.
  [verification.md](../../../verification.md)의 term MAYBE 18건 중 14건이 이 모양
  (`$concat_text`/`$exists`/`$forall`/`$filter`/`$join_text`/`$flatten_*`/`$invalidate_*`/
  `$lvalue_as_expression`/`$set_priorities_*`).
- **수정** ([translate/reflect.ml](translate/reflect.ml)): `match_K(v)=true`의 **유일한**
  다른 등장이 같은 K로 destructure하는 조건일 때 그 매처를 **삭제**한다
  (`restated_by_destructure`). `v = K(..)`가 이미 함의하므로 절의 적용 조건은 불변이고,
  v가 더는 "다른 조건"에 안 나오니 기존 `fold_premise_binders`가 **자기 게이트를 그대로 지킨 채**
  destructure를 head로 접는다. 접힌 head `K(..)`가 매처가 지고 있던 disjointness를 그대로 진다.
- **owise 계열은 제외** (`owise_head_syms`) — 거기선 CRC가 `Reflect.owise`의 부정 가드를
  형제의 **동일한 가드 항**으로 재작성해야만 반박하므로(같은-subject 정렬), 형제의 매처를
  지우면 discharge 근거가 사라져 impty `$lookup`이 YES→MAYBE로 역행한다. 실측으로 확인함.
- **실측 (같은 기기, before = 변경 전 바이너리 / after = 이 커밋)**:

  | 측정 | before | after |
  |---|---|---|
  | `$concat_text` termination | MAYBE (237s) | **YES (98s)** |
  | `$flatten_nameList` termination | MAYBE (240s) | **YES (101s)** |
  | `$lookup` CRC/ChC (impty, owise 계열) | YES/YES | YES/YES (무회귀) |
  | `$filter` CRC | TIMEOUT (397s) | TIMEOUT — **회귀 아님**(변경 전에도 TIMEOUT; 이 기기 MFE 성능. verification.md의 YES는 더 빠른 환경 값) |

  의미 보존: 실행(native) 표면 **sha256 불변**(분석 표면 전용), impty `run-structural` 8/8
  `result: true`, p4 `run-structural --check-p4` 표본 **12/12 MATCH / 0 MISMATCH**.
  impty 골든(`spec.ctrs`)은 `Check-expr`/`Check-command`/`Eval-expr`/`Eval-command` 20절이
  head-패턴으로 접히는 **의도된 개선**이라 재생성; `$lookup` 정의 규칙은 byte-identical.
- **남은 것**: `$join_ctk`/`$assignop_as_binop`의 CRC MAYBE는 이 수정 범위 밖 — 원인이 다르다
  (owise 반사 가드 + **패턴형** 형제). MFE 원출력으로 확인: `ccp SPEC1`의 조건이 완전히 ground인
  `or(match_ctk_*(CTK)..) = false`인데, **CRC는 임계쌍 조건을 모듈 등식으로 정규화하지 않아**
  `true = false`(무모순 불가)를 못 본다. 형제가 가드형이면 그 가드 항으로 재작성돼 discharge되지만
  ($is_lpm_key' 등 YES), 패턴형 형제는 밀어줄 hypothesis가 없다. → owise 절을 멤버 생성자로
  fan-out(`expand_subty_guards`와 동일 기법, ctk 3멤버/assignop 13멤버로 ≤16 게이트 통과)하면
  head가 서로소가 되어 임계쌍 자체가 사라진다.)

### ⚠️ 전수 differential 재실행 필요 (2026-07-14, 미완)

**`check_diff_p4_*.tsv`의 "completeness 0 / soundness 1 / Phase D 1227/1227"은 현재 HEAD의
수치가 아니다.** 술어 도메인 협소화(위 SCC P1) 검증 중에 드러났다:

- `check_diff_p4.sh`는 **resumable**이라 TSV에 기록된 프로그램을 건너뛴다. 즉 **옛 바이너리로
  만든 TSV가 남아 있으면 재실행이 아무것도 검증하지 않는다**(`1568 done, 0 to run`).
  ⇒ 회귀 게이트로 쓰기 전에 반드시 TSV를 baseline으로 옮기고 지울 것.
- 그렇게 지우고 새로 돌린 부분 실행(Phase B 1140/1568)에서 **`const.p4`/`issue1717.p4`가
  기존 TSV의 `OK`와 달리 `STUCK`** 으로 나왔다. 협소화 회귀를 의심했으나 —
  **변경 직전 커밋(08dfe4ed)의 바이너리로도 똑같이 STUCK**임을 워크트리로 확인했다
  (`--wide-predicate-domains`에서도 STUCK). ⇒ **이번 변경의 회귀가 아니라, 그 이전 어느
  커밋에서 이미 생긴 completeness gap 2건**이거나, 기존 TSV 자체가 낡은 바이너리 산물이다.
- 이번 변경의 무회귀는 **50개 표본으로 확인**(baseline TSV와 50/50 일치; 1874d212가 고쳤던
  cross-type matcher 케이스 `issue122.p4`/`key-name.p4` 포함).

**할 일**: 깨끗한 환경에서 `check_diff_p4.sh` **전수 재실행**(~8시간) → 진짜 completeness/
soundness/Phase D 수치를 다시 세우고, `const.p4`/`issue1717.p4`가 실제 gap이면 어느 커밋에서
생겼는지 이분 탐색(둘 다 `Program_ok`가 stuck).

남은 작업:

```
  → 큰 슬라이스의 CRC를 예산이 아니라 **슬라이스 축소**로 뚫기                  [confluence 스윕은 2,490규칙에서 세웠다(2026-07-30, 아래 절). 874규칙 위는 예외 없이 TIMEOUT이고 남은 287심볼 중앙값이 52,394규칙이라 예산을 늘려도 결과가 같다. 유력 수단은 맨변수 바인더 제거(아래 2026-07-24 research note: 비용의 98.6%) + 추가 pruning/모듈러 분해]
  → orient_conds(bdceb303) 이후 term 재측정                                    [CRC 열은 위 스윕으로 278심볼까지 재측정 완료(다운그레이드 0). term은 "판정 변경 가능"이 아니라 "기존 YES 일부가 미증명"이라 성격이 달라 여전히 남아 있다 — 스윕 중단 상태]
  → 전수 differential 재실행 + const.p4/issue1717.p4 이분 탐색            [기존 TSV 수치가 낡음; 위 절 참조. sanitize 밑줄 보존(2026-08-03)이 실행 표면을 전량 개명했으므로 이번 재실행이 그 무회귀도 겸한다 — 표본 5개는 이미 RESULT MATCH]
  → $bitstr_to_int w=0 실행 비종료                                             [유일한 "진짜 결함" 후보; P4 타입시스템이 bit<0>/int<0> 산술을 막는지 규명하면 갈림길 결정]
  → LTL 모델 검사로 P4 시간적 성질 검증                                        [신규 축; (P1) 선택적 rl 모드(= Kripke 전이, 유일한 하드 블로커) → (P2) modelCheck 배선(holds_R을 원자명제로) → header validity `[] ~ readInvalid` → parser/progress/preservation → (P3) 보조 search 일반화]
  → SCC (sufficient completeness)                                             [P1 도메인 협소화 완료(2026-07-14, predicate_domains = 쓰이는 주어들의 join 고정점; 가족의 86%가 진짜 sort, match_typeIR_BOOL_0이 위양성 반례 → COMPLETE). 남은 건 (P2) --slice-dir 배치 덤프(심볼당 50초 재번역 → 24시간이라 스윕 전 필수) → 전체 스윕(exact는 COMPLETE 수확, approx는 COUNTEREXAMPLE만; dom: 열로 1차 분류) → (P3) sub_nat의 Val 도메인]
  → 잔여 MAYBE: rhs-2회-사용 출력 바인더($un_op의 $bneg 케이스 — fold 중복-방지 게이트의 몫) + 대형 variant(>16멤버) subty 가드 + 전체-시스템급 슬라이스 [B′ 범위 밖; 필요 시 별건 설계]
                (companion-destructure 케이스는 위 2026-07-11 항목에서 해소)

  (완료: sanitize 밑줄 소실로 인한 심볼 합쳐짐 — `_`를 이름 문자로(위 2026-08-03 항목)
         $write_value_from_bits' n_var=0 경계 — 스펙에 `$(n_var > 0)` 가드 복원(아래 2026-07-28 항목)
         CTRS(구조적) differential — binary 수 인코딩 전환 후 Phase D 1227/1227 MATCH, 92618dc2
         termination 열 채우기 — 153심볼 CRC+term 스윕, verification.md
         owise 절 생성자 fan-out(complement 열거) — $join_ctk/$assignop_as_binop CRC MAYBE 2건
         해소(둘 다 YES), $join_flow 회귀 없음, 2026-07-16 상세는 위 M1 블록)
```

## CRC 정규화 패스 (2026-07-21): inline + unravel + prune, unravel은 reflect-only/upgrade-only

CRC MAYBE의 원인 = **determinacy critical pair**: 조건 `$f(A)=v`를 CRC가 fresh-변수
joinability로 인코딩하나 `$f`의 결정성을 몰라 self-overlap `#v#=v`를 못 닫는다. 세 레버:

1. **inline** (`fold_premise_binders ~aggressive:true`, [rewrite_system.ml] Pass 1 — done):
   single-var binder `$f(A)=v`를 use-count 무관 인라인. **등식(equivalence)** — preserve와
   reflect 양방향, 안전. `--crc-normalize` 플래그. e2e 검증: bin_concat TIMEOUT→YES,
   write_value MAYBE(5쌍, tuple binder 잔존).
2. **unravel** (tuple binder `$f(A)=tuple(v,b)`를 U-체인으로): **confluence-reflecting,
   NOT preserving** (Marchiori 1996; Nishida-Sakai-Sakabe LMCS 2012; Gmeiner-Gramlich-
   Schernhammer RTA 2010). 즉 `R` confl ⇏ `U(R)` confl (초기 join_text에서 YES→MAYBE로
   관측됐으나, 이는 값-destructure over-unravel이었고 subject 게이트 `1dd1e43a`로 교정 —
   아래 "over-unravel 교정" 참조; 이론상 non-preservation은 여전히 가능), 하지만
   soundness(좌선형 등) 하에 `U(R)` confl ⇒
   `R` confl. **그래서 upgrade-only로만 사용**: `U(R)`이 YES면 `R`을 YES로 승격,
   `U(R)`이 MAYBE/TIMEOUT이면 **원본 verdict 유지**(절대 하향 안 함). 형제 충돌을 줄이려
   **생성자-disjoint entry**에 우선 적용.
3. **prune** (`prune_slice_signature.py full`): 안 쓰는 시그니처 제거, tractability 회수.
   rules 불변이라 CRC 결론 보존. 기존 Python 후처리 유지(OCaml 재구현 불필요).

**건전성 방향 요약**: inline은 등식이라 blanket 가능; unravel은 reflect-only라 upgrade-only
필수. termination은 unravel이 깔끔히 transfer되지만(우리 MTT가 그 용도), confluence는
reflection만 성립 — 그래서 CRC에서 신중.

**구현 상태 (done)**: `--crc-normalize` = inline(Pass 1) + `crc_unravel` + `order_conds`
([rewrite_system.ml], new-rewrite `d109f982`). unravel의 `decompose`는 **App-패턴 binder만** 풀고
bare-var binder는 조건으로 남긴다(uses=0 self-pair가 spurious crcu overlap 되는 것 방지).
prune은 기존 Python 후처리 유지.

**crcu/crck sort**: 처음엔 `MS.signature` 미지-심볼 fallback으로 `Val…→Val`(all-Val)로 방출 —
**건전**(sort 확장 = overlap 추가 → 거짓 YES 불가)하나 체인-스텝 패턴 변수가 Val로 넓어져
spurious overlap을 만들 수 있다(set_priorities all-Val TIMEOUT 실측). 이후 [to_mfe.ml]에서
**실제 sort 복원**(`crc_sigs`): `crcu<id> : <subject sort> CrcKeep -> <함수 결과 sort>`,
`crck<id> : <carried 변수 sort…> -> CrcKeep`. carried 변수는 producer 규칙(head=원래 `$f`라
hint/arg-sort 유효)에서 타입되고, 다단계 체인은 id 오름차순(=레벨 순) 점진 처리로 의존성 해소
(작은 id의 crcu/crck가 먼저 실제 sort를 얻어 다음 레벨 producer 타이핑에 쓰임). crcu/crck 없으면
완전 inert(byte-identical). 이 narrowing은 나머지 order-sorted 인코딩과 동일하게 건전 —
carried 변수가 binding-site의 실제 sort를 유지하므로 real 임계쌍 보존, spurious만 제거.

**결과**: 5 write_value MAYBE→YES, bin_concat TIMEOUT→YES. 회귀: bin_bor/un_op YES 유지.
set_priorities: all-Val TIMEOUT → **real-sort YES 0쌍 78s**(Python 프로토타입 동일 시간).
sweep이 upgrade-only 채택 담당.

**over-unravel 교정 (new-rewrite `1dd1e43a`)**: `crc_unravel`이 조건 `s = tp`의 **패턴 `tp`만**
보고 unravel해서, subject `s`가 **변수**인 값-destructure(`text = cons(..)`)까지 풀었다. 그런데
determinacy CCP는 subject가 **함수 호출**일 때만 생긴다 — 변수-destructure는 CRC가 통일로 잘
처리하고, `Reflect.hoist_matchers`가 `match_K(v)=true`를 바로 그 destructure로 respell하는 이유가
그것이다(CRC가 볼 수 있게). 값-destructure를 unravel하면 destructure가 crcu consumer로 떨어져
나가고 형제 overlap엔 opaque `match-cons`만 남아, CRC가 base 절의 `len=bone`(⟹ nil)과 못 엮어
모순을 못 본다 — 이게 join_text YES→MAYBE의 진짜 원인이었다(reflect-only의 예가 아니라 고칠 수
있는 버그). `decompose`를 **subject가 정의 함수일 때만** unravel하도록 게이트: join_text **YES
0쌍 6s** 회복, write_value(5)/set_priorities(함수-subject) 무영향. unravel은 이론상 여전히
reflect-only라 upgrade-only는 안전망으로 유지.
