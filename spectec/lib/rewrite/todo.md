# `rewrite` 재구현 TODO (new-rewrite)

**골격은 전부 채워졌습니다 — `failwith` 스텁은 하나도 남아 있지 않습니다**(코드
전수 grep 확인). 번역(`To_ctrs.of_spec`/`var_type_hints`), 지원 패스
(`Defunctionalize`/`Gensym`/`Builtin`/`Exp_map`), Maude 백엔드
(`To_maude`/`Maude_run`/`Of_maude`), confluence 게이트(`Mfe`)가 모두 구현·컴파일되고
동작합니다. 남은 일은 *새 번역 로직*이 아니라 **(a) 분석 confluence 잔여 MAYBE 해소,
(b) CLI `run`의 실제 실행 배선(오라클), (c) subtype struct 처리 + 문서 정리**입니다.
알고리즘 설계 기준은 [CORE_LOGIC.md](CORE_LOGIC.md), 모듈 상태는
[CLAUDE.md](CLAUDE.md) 참고.

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

  - [ ] **(B) owise-없는 케이스-분기 절의 head-패턴 폴드.** `$un_op`/`$tableCustomName`
    류는 동일 head `$un-op(unop,value)` 위에 `if match_X(unop)=true` 가드 4절이라
    CRC가 `$un-bnot(value)=$un-lnot(value) if match-tilde(unop)=true /\ match-bang(unop)=true`
    같은 임계쌍을 만들고 두 match가 배타적임을 못 본다. **fold:** match 분기자를
    head 패턴으로 끌어올려 `$un-op(variant-unop-tilde-0,value)=$un-bnot(value)` /
    `$un-op(variant-unop-bang-0,value)=$un-lnot(value)`. head 생성자가 달라져
    단일화 불가 → 임계쌍 미생성 → YES. **판별은 전역 "case-분기 함수" 라벨이 아니라
    per-condition 구조 인식**: 조건 `App(p,[Var v])=true`, `v`가 선형 head 변수,
    `p`가 *discriminator*(정의 규칙이 `p(ctor(서로 다른 변수))=true|false` 무조건절,
    유일한 true-생성자)인 경우만. 이 discriminator_index가 곧 안전 게이트 —
    `subty_*`(RHS가 `and(sub-nat..)` 계산식이라 리터럴 아님)·다중-true·비선형은
    자동 배제(=`$invalidate_value`의 subty 부분은 이걸로 **안** 됨, 별도). **반드시
    owise-없는 심볼로 게이트** — owise-짝 절을 폴드하면 disjointness 가드가 사라져
    owise 중첩이 노출(aggressive 변형이 impty 6개를 역행시킨 그 원인). `discriminator_index`
    는 이전 aggressive 시도에서 프로토타입했다가 surgical 후퇴 때 제거 — 재도입.
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
  [tools/mfe/README.md](../../tools/mfe/README.md). 남은 일: tractable 150 슬라이스
  Z3 sweep으로 CRC 표 옆 termination 열 채우기(작은/중간=verdict, 산술=MAYBE/TIMEOUT).
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

## subtype의 struct 처리 — width(+depth) subtyping 미구현

**의도된 의미론:** struct(record)는 **필드가 많을수록 더 좁은(refined) 타입**이다 —
필드가 더 많은 struct가 필드가 더 적은 struct의 **subtype**(record **width**
subtyping). 공통 필드에 대해서는 필드 타입끼리 다시 subtype 관계여야 한다(**depth**).
즉 `struct { a:T1; b:T2; c:T3 } <: struct { a:U1; b:U2 }` ⟺ `T1<:U1 ∧ T2<:U2`
(추가 필드 `c`는 허용). **이 width 관계가 현재 번역에 구현돼 있지 않다.**

**현재 구현:** `defs_of_typ`의 StructT는 `subty_<T>(struct_<T>(x...)) -> true` 한
줄만 낸다([translate/to_ctrs.ml:519](translate/to_ctrs.ml)).
- LHS가 **정확히 같은 타입의 생성자 `struct_<T>`(정확한 arity)** 에만 매칭 →
  **width 미구현**: 더 넓은 struct 값(다른 타입 `struct_<S>`, S의 필드 ⊇ T)은
  `subty_<T>`에 **매칭 자체가 안 돼 irreducible/stuck**.
- 매칭돼도 필드 타입을 재귀 검사하지 않고 **무조건 true** → **depth 미구현**
  (variant/tuple/list/opt는 재귀하는데 struct만 빠짐: `subty_<T>(variant_...(x...))
  -> and(sub_pred field_typ_k x_k)`, `subty_tup`/`subty_list`/`subty_opt`는
  `sub_helper_defs`에서 성분·원소 타입으로 재귀). `sub_helper_defs`의 `require`도
  struct 필드 타입은 seed하지 않는다.

**인코딩 장애물(왜 단순치 않나):** struct는 **타입명으로 keyed된 고정 arity
위치(positional) 생성자** `struct_<T>(f1..fn)` 로 인코딩되고, 필드 접근은
(타입, 필드명) 쌍별 접근자 `field_<T>_<a>` 다. 서로 다른 struct 타입 = 서로 다른
생성자라, width가 요구하는 **이름 기반 필드 투영**(서로 다른 struct 생성자에서 같은
이름 필드 꺼내 비교)을 위치 인코딩이 직접 표현하지 못한다. (variant는 "T의 케이스
열거 + injected origin 포함"으로 width를 표현하지만, struct엔 케이스가 없고 필드
집합 자체가 타입이라 그 트릭이 안 통한다.)

**결정 절차 / 접근안:**
1. `interp.subtyp`의 struct 케이스를 먼저 확인 — 런타임에 width/depth를 실제로
   검사하는가, 아니면 elaborator가 항상 정확한 struct 타입으로 좁혀 두어 런타임
   검사가 trivially-true여도 되는가. (이 답이 width 구현 필요 여부를 가른다.)
2. **depth만(같은 타입 내) 필요하면**: variant처럼 필드별 conjunction으로 교체
   `subty_<T>(struct_<T>(x...)) -> and(sub_pred field_typ_k x_k)`, 그리고
   `sub_helper_defs`의 `require`에 **StructT 가지 추가**(필드 타입 seed; 없으면 필드의
   `subty_*` 헬퍼가 없어 stuck).
3. **width까지 필요하면**: 위치 인코딩으로는 부족하므로 설계 논의 필요 — 후보:
   (a) struct를 **이름 키 페어 리스트/맵**으로 재인코딩해 필드 투영을 이름 기반으로
       (레코드 표현 재설계, 큰 변경; `field_*`/`upd_field_*`/`eq`/생성자 전부 영향),
   (b) 각 super-struct 타입 S마다 `subty_<T>(struct_<S>(...)) -> and(...)` 규칙을
       생성(S 조합마다 규칙 폭증 — 비현실적, 기록만),
   (c) elaborator가 width 좁힘을 이미 처리해 런타임 width가 안 들어온다는 불변식이
       성립하면 trivially-true 유지(근거 주석 명시).
4. width/depth 어느 쪽이든 **soundness 방향**(현재는 stuck=거부라 width를 *under*-
   accept; depth는 무조건 true라 *over*-accept)을 명시해 회귀 시 방향을 못 박을 것.

검증: impty/base 골든 고정 후 diff(struct subty 규칙 변경 시 골든도 바뀜). impty엔
struct가 있으니 이 규칙 변화가 골든에 바로 드러난다.

## subtype의 depth 처리 — 재귀 경계와 근사 (struct 외)

`sub_pred`([translate/to_ctrs.ml:191](translate/to_ctrs.ml))가 타입 구조를 따라 depth로
내려가며 멤버십 검사를 생성한다. 어디서 재귀하고 어디서 trivially-true로 멈추는지,
그리고 멈추는 지점의 근사를 정리한다(struct 구멍은 위 섹션; 아래 (1)이 그걸 복합
타입으로 전파시킨다).

**depth 재귀가 실제로 되는 곳:**
- VariantT 케이스 payload(필드 타입별 `sub_pred`), TupleT 성분, IterT list/opt 원소
  — `defs_of_typ`/`sub_helper_defs`가 `and(sub_pred ..)` / 구조 재귀로 내려감.
- PlainT alias `T = U`: `subty_<T>(x) -> sub_pred U x`로 위임(체인 OK).

**trivially-true로 멈추는(근사) 곳 — 점검 대상:**
1. **struct를 품은 복합타입**: struct subty가 무조건 true(위 섹션)이므로
   `(structT)*` / `(structT, ..)` / `structT?` 등 **struct를 포함한 list/tuple/opt의
   depth 검사가 struct 부분에서 통과**돼 버린다 — struct 구멍이 격리되지 않고 복합
   타입으로 전파됨(struct 수정 시 자동 해소).
2. **타입 파라미터(generic VarT, TypD 없음)**: `sub_helper_defs`의 `require`가
   `subty_<param>(x) -> true`로 근사(추상 타입엔 구조가 없어 내려갈 수 없음). 제네릭
   컨테이너(`pair<K,V>`의 K/V 등)의 원소는 depth 검사가 사실상 생략 — positive 근사라
   실제 멤버십 위반을 못 잡을 수 있음(over-accept 가능). `interp.subtyp`의 generic
   처리와 대조 필요.
3. **스칼라 leaf**: `NumT IntT | BoolT | TextT | FuncT -> true_t`(`sub_pred`). NatT만
   실제 검사(`sub_nat`: int_pos/bare nat→true, int_neg→false). int은 가장 넓은 수,
   bool/text/func는 자기 sort라 positive 검사에선 무해하지만 **"정적 타입이 멤버십을
   보장한다"는 가정**을 주석으로 명시할 것.

**depth + 음성(negation) 전파 — 모드 의존(중요):**
- subty 규칙은 **positive-only**(멤버→true; 비멤버→규칙 없음→irreducible). 중첩 비멤버의
  false 전파는 `conj_t`의 `and`에 달려 있는데,
  - **Structural(분석/MFE)**: 비멤버가 false가 아니라 **stuck**이라 `and`도 stuck. 즉
    분석 표면에서 depth 비멤버는 false로 닫히지 않는다(positive-only).
  - **Native(실행)**: `To_maude`가 사용된 `subty_*`에 guarded **owise → bool(false)**
    complement를 붙여(닫힌세계) 중첩 비멤버가 false로 전파됨. 단 인자 head보다 깊은
    stuck은 여전히 흡수(근사).
- ⇒ depth 비멤버 판정은 **실행에서만 결정적, 분석에선 근사**. `~(e <: T)`류를 쓰는 분석
  규칙의 confluence/coherence에 영향 가능 — MFE calibration 때 함께 점검.

**termination(생성·재기록):**
- 재귀/상호재귀 타입: 헬퍼 생성은 `Helper_defs.mem`/`require` 메모이즈로 타입당 1회 →
  생성 종료. 재기록은 구조적으로 더 작은 항으로 내려가 종료. (확인만; 상호재귀 타입
  표본으로 헬퍼 누락/중복 없는지 점검.)

**할 일:**
- [ ] (2) 타입 파라미터 trivially-true 근사의 over-accept 실제 케이스를 `interp.subtyp`
  generic 처리와 대조.
- [ ] (3) 스칼라 trivially-true 가정 `sub_pred`에 주석화.
- [ ] depth 음성 전파의 분석/실행 비대칭을 MFE calibration 항목과 연결.
- [ ] 상호재귀 타입 표본으로 생성·재기록 종료 확인.

## 권장 순서

(이미 복구·완료: CLI 배선, `Exp_map`, `Defunctionalize`/`Gensym`/`Builtin` + 파이프라인,
`Simplify`=identity, Maude 백엔드 `To_maude`/`Maude_run`/`Of_maude`,
`To_ctrs.of_spec`/`var_type_hints` 포팅, **Native 직접 생성 리팩토링 (B)**,
**impty/base 분석 골든 고정**(`specs/impty/base/spec.ctrs`).)

남은 작업:

```
  → subtype의 struct width(+depth) subtyping 구현                  [위 섹션; interp.subtyp 확인 후]
  → subtype depth 근사 점검(타입파라미터/스칼라/음성 전파)         [위 섹션; MFE calibration과 연계]
  → Mfe calibration                                                [분석 confluence]
  → CLI run 실행 배선 + targets maude_start_term                    [M2/M3: 오라클]
  → differential (same-spec interp vs Maude)                       [M3]
```
