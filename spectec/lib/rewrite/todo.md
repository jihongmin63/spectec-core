# `rewrite` 재구현 TODO (new-rewrite)

**골격은 전부 채워졌습니다 — `failwith` 스텁은 하나도 남아 있지 않습니다**(코드
전수 grep 확인). 번역(`To_ctrs.of_spec`/`var_type_hints`), 지원 패스
(`Defunctionalize`/`Gensym`/`Builtin`/`Exp_map`), Maude 백엔드
(`To_maude`/`Maude_run`/`Of_maude`), confluence 게이트(`Mfe`)가 모두 구현·컴파일되고
동작합니다. **M2(실행)·M3(오라클)도 완료** — `run --imp/--p4/--check-p4` 실행 배선과
same-spec differential(전체 corpus; completeness 0 / soundness 1(알려진 issue1944) /
Phase D 결과-VALUE 1227/1227 MATCH)까지 끝났습니다. 남은 일은 *새 번역 로직*이 아니라
**(a) 분석 confluence 잔여 MAYBE 해소(match/subty 가드), (b) subtype struct 처리 +
문서 정리**입니다.
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
  - [ ] **owise 반사 확장 — 출력 relation "성공 반사" + 수집형 IterPr (한 확장).**
    reflect 게이트가 skip한 심볼(stderr 집계 목록 참조)을 열기 위한 후속.
    두 케이스는 동일한 확장 하나다: `all_R?` = 원소별 `R_succeeds?`의 fold.
    - **성공 반사**: input-moded relation은 출력이 있어도 성공 여부(∃out)가
      입력만의 boolean → `R_succeeds?(in) = or(g_1…g_n)` (무출력 R?와 같은
      스킴). g 구성 추가 규칙: 절 안의 출력 바인딩 전제 `R'(in') = ⟨패턴⟩` →
      `and(R'_succeeds?(in'), match_⟨패턴⟩(R'(in')), …)`, 바인딩 변수의 후속
      사용처는 호출 항 `R'(in')`/`proj_tuple_i(R'(in'))`로 인라인(성공 가드
      뒤라 `and(false,·)=false` 단락으로 stuck 안전). 결정성 가정 = 분석 표면
      기존 가정(CRC가 검증하는 성질). **주의**: to_maude의 "출력 relation 보완
      금지"는 R 자체 totalize(실패=값 → stuck 전파 붕괴) 이야기 —
      `R_succeeds?`는 별도 심볼이라 R은 partial 유지, 해당 제약 무관.
    - **수집형 IterPr**: `(R: in ~ out)*` 수집의 적용가능성 =
      `all_R?(ins)`(성공 반사 fold) + 수집 스트림은 `$iterapply(ins)` **항**으로
      가드에 인라인. **정렬 필수 조건**: 양성 sibling에도 `all_R?(ins) = true`
      잉여 조건을 추가해야 CRC 가설이 정렬됨(sibling 조건엔 원래 all_R?가
      없어서 가설 없음 → or-가드 안 접힘; `$iterapply(ins) = outs` 가설만으론
      부족). `$itercollect`/`$unzip`도 같은 레시피(성공? fold + 항 인라인).
    - 검증: reflect 게이트 집계에서 열리는 심볼 수 확인 → 해당 슬라이스
      per-symbol CRC, 기존-YES 무회귀, 골든 재생성.
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
- **⛔ `$iterproj` 제거(수집 헬퍼 통일 1단계) — 보류, 정합성 벽 (2026-07-02).**
  다중 출력 iterated relation을 "출력별 무조건 map"으로 바꾸려던 안은 **gensym과
  충돌**한다: p4의 multi-output `$iterapply` 3개(`TableEntry_ok`,
  `Type_ok/block` ×2 — 출력에 fresh `typeId'` 포함)가 전부 state-thread돼 있어,
  출력별 map이 각자 relation을 재호출하면 **fresh 이름 발급이 map마다 갈라져**
  인터프리터와 divergence가 난다(성능이 아니라 정합성 문제). 단일 호출 + 순수
  스트림 projection(현 `$iterapply`+`$iterproj`)이 효과적 다중 출력의 올바른
  형태라 유지. 후속 `$itercollect`-only 통일(4e00da94 revert)도 같은 벽 —
  effectful 반복은 원소당 1회 호출 구조가 불변식임을 전제로 재설계해야 함.

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

**termination(생성·재기록):**
- 재귀/상호재귀 타입: 헬퍼 생성은 `Helper_defs.mem`/`require` 메모이즈로 타입당 1회 →
  생성 종료. 재기록은 구조적으로 더 작은 항으로 내려가 종료. (확인만; 상호재귀 타입
  표본으로 헬퍼 누락/중복 없는지 점검.)

**할 일:**
- [ ] 상호재귀 타입 표본으로 생성·재기록 종료 확인.

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

남은 작업:

```
  → subty false-보완 실행 무회귀 표본 (run --p4 --check-p4 ~50개)   [위 M1 체크박스; 보류 중]
  → (B) discriminator head-패턴 폴드 — match + subty 가드 확장     [분석 confluence; B′ 해법]
  → owise 제거 + relation R? 반사 (negation-as-false-value 확장)    [subty totality가 기반]
  → termination 열 채우기 (tractable 150 슬라이스 Z3 sweep)         [CRC 보완; timeout 재보정]
```
