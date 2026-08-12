# P4 structural CTRS — 검증 결과 (표)

> **표만 둔다.** 각 셀은 `판정 (초s)` — 판정과 그 판정에 걸린 심볼당 **직렬 fresh**
> 벽시계다. 행은 슬라이스 규칙 수 오름차순. 열 이름은 그 열을 만드는 서브커맨드
> 이름과 같다(`main.exe confluence` / `main.exe termination`). **ChC(Coherence)는
> 2026-07-24부로 측정 중단·열 삭제**(사유·이력은 notes).
>
> 재현 커맨드·방법론·측정 이력·비-YES 해석은
> **[verification-notes.md](verification-notes.md)**, 이번 측정의 원자료와 경위는
> **[sweeps/RESULTS-2026-08-07.md](sweeps/RESULTS-2026-08-07.md)**.

**측정 기준**: 각 열은 **직접 측정한 값만** 싣는다. 심볼당 프로세스 하나, 직렬,
`ulimit -s unlimited`.

- **termination** = 구조 보존 unravel → AProVE 직접. **650/650 전수**, 평평한
  `--budget 20`, **2026-08-12 재측정**(`bccc26d9` 트리 — 분석면은 `ab950751`과
  동일). 환경 지문: nproc=32, 가용 108GB, loadavg 3.5–5.7, java 25.0.3,
  minisat2 없음. `TIMEOUT (>20s)`는 **20초 안에 증명도 반증도 못 했다**는
  뜻인데, 아래 "환경 연성" 절대로 **이 판정은 측정 환경의 함수**라 지문 없는
  TIMEOUT 열은 비교 대상이 못 된다.
- **confluence** = Church-Rosser. **262/650**, `43ed519d` 트리에서 `--timeout 60`
  으로 측정 — 당시는 단일 deadline이 MFE 로드 여유 240초를 체커에 넘겨 **사실상
  300초 예산**이었다(`bccc26d9`가 로드/체크 deadline을 분리해 고침 — 이후 스윕은
  같은 판정을 보존하려면 `--timeout 300` 명시가 규약). `-`는 미측정.
  `TIMEOUT (>300s)`는 그 예산을 다 쓴 것이다.

## 이 표의 예전 ⚠️ 무효화는 해소됐다 (2026-08-11)

termination 열을 무효로 만들던 세 결함이 모두 고쳐졌고, **열 전체를 그 뒤에 다시
쟀다**. 무엇이 왜 무효였는지는 남겨 둔다 — 같은 함정을 다시 밟지 않기 위해서다.

1. **판정 파서** (`bae6e096`). AProVE는 1행에 판정을 찍고 그 뒤 증명을 되풀이하는데,
   그 서사에도 판정 토큰만 있는 줄이 있다. 파서가 버퍼 전체를 훑으면서 마감에 걸린
   실행의 `KILLED`를 지나치고 **서사 속 `YES`를 답으로 집어갔다** — 포기한 실행이
   종료증명으로 기록됐다.
2. **`isStuckHead` masking** (`b1ca0af6`). 분석면 조건부 규칙 2,510개 중
   **1,044개(41.6%)** 가 분석면에 정의가 없는 술어를 가드로 달고 있어, unravel 후
   그 규칙들의 우변이 도달 불가였다. AProVE는 **그 규칙들이 지워진 시스템**의
   종료성을 증명하고 있었다. 복원 후 재측정하니 YES 210건 중 **30건(14%)이 살아남지
   못했다**.
3. **`holds_` or-gate의 루프** (`91e8af50`). ⑵를 고치고 처음 돌린 전수 스윕이
   **NO 23건**을 냈다. 판단 반사가 내던 맨변수 lhs 규칙 하나가, 가드에서 형제의
   부분항을 `proj_K_i(subject)`로 되찾으면서 인자가 줄지 않는 자기/상호 루프를
   만들고 있었다(증인: [sweeps/witness/WITNESSES.md](sweeps/witness/WITNESSES.md)).
   owise 경우분할과 같은 방식으로 갈라서 없앴다.

**반례 검증**: 그 23건은 고치기 전 **11.0~23.0초에 NO**를 냈다. 고친 뒤 평평한
300초로 재질의하면 **23/23 TIMEOUT (300.5~306.2초)** — 루프가 있을 때 드는 시간의
13~27배를 줘도 안 나온다. "아직 못 찾았다"가 아니라 없어졌다고 말할 근거다.

**CRC 예산이 처음으로 지켜졌다** (`43ed519d`). 그전까지 `--timeout`은 아무것도 묶지
않았다 — `--timeout 60`인 심볼 하나가 **35,895초**를 태웠고, 예전 기준선에도 같은
예산에서 436.5초·321.7초 행이 있다. **따라서 이 저장소가 지금까지 기록한 CRC 초는
전부 예산 밖 값이다**(판정은 유효, 비용 분석은 무효).

## 종합 (650심볼)

**termination**: 650/650 — YES **415** · TIMEOUT **233** · DEGENERATE 2(슬라이스가
비어 자명). **비종료 후보 0**.

- **TIMEOUT 열은 환경-연성이다 — 이 열이 하루 만에 175건 플립한 이유.** 08-10의
  세 스윕은 같은 조건에서 YES 240 / TIMEOUT 408을 냈는데, 08-12 재측정은
  **TIMEOUT→YES 175건**(YES→YES 240 그대로, 회귀 0)이다. 문제도 도구도 스펙도
  바뀐 게 없다: `$update_headerUnion`(151규칙, TRS 스탯 동일)이 08-07 예산 300에서
  MAYBE(279.8s) → 08-10 TIMEOUT×3 → 08-12 **예산 5에서 1초 YES**였고, minisat2
  설치는 한-변수 실험으로 반증(제거해도 1초 YES), aprove.jar·java·z3·번역 경로
  전부 불변. 남는 용의자는 당시 이 박스의 JVM 가용 자원(cgroup 배분으로 추정,
  소급 검증 불가)이다. 교훈: **TIMEOUT 판정은 시간 열처럼 환경 지문과 함께
  읽어야 하고**, 08-09의 "≤500 구간 300초 재질의 YES 0건" 결론은 저하된 환경에서
  측정된 것이라 **근거 효력을 잃었다**.
- 플립 175건의 규칙 수 중앙값은 1,221, 최대는 `Cast_impl` 3,860(19.8s) — 건강한
  환경의 AProVE는 수천 규칙 슬라이스도 수 초에 증명한다. YES 415건 중 297건이
  5초 이내, 최대 22.0초.
- 잔여 TIMEOUT 233건: >500규칙 192건(중앙값 5,674 — 대형 폐포 구간), **≤500규칙
  41건**. 후자가 의미론적 표적이다(`$strip_prefix_rec` 59, `$init_` 79,
  `$repeat__value` 81, `$fresh_typeIds` 84, `$strip_suffix_rec` 98…) — 작은
  슬라이스인데 안 닫히는 것은 예산이 아니라 증명 구조의 문제일 가능성이 크다.
  `$bin_minus`(225규칙)는 07-23 앵커에서 예산 1800으로 YES였으니, 큰 예산에서만
  답하는 구간이 실재한다.
- **`Program_ok`(20,399규칙, 최대 슬라이스)은 별도로 12시간을 버텼다**: 평평한
  예산 43,200초에서 **TIMEOUT 43,210.7초**(2026-08-11, `term_program_ok.tsv`).
  holds_ 분할 전 표면(11,039규칙)의 같은 12시간은 **NO**였다 — 루프 제거의 최대
  규모 확인이다. 반증도 증명도 없이 예산을 소진했으므로 판정은 미증명이다.

**confluence**: 262/650 — YES **225** · TIMEOUT **37**. 미측정 `-` 388.

- 대상은 예전 기준선이 YES로 기록한 225개 + 이번 작업이 직접 건드린 40개다.
  **기준선 YES가 non-YES로 내려간 건 0건**이다.
- TIMEOUT 37건은 **전부 그 40개 안**이고, 원래 CRC 측정이 없던 >500규칙 꼬리다.
  전부 정확히 300.3~300.4초, 즉 예산을 그대로 소진했다.
- 미측정 388개를 채우려면 예산이 아니라 슬라이스를 줄여야 한다(추가 pruning /
  모듈러 분해). 예전 스윕이 874규칙 위에서 예외 없이 TIMEOUT이었다.
- **`YES (300.4s)` 3건의 초에 대한 이전 해설(EOF 프롬프트 대기가 예산을 소진)은
  틀렸다** — `bccc26d9`의 분리된 deadline로 재실험해 반증했다(2026-08-12).
  `$bin_band`는 체크만 60초 주면 TIMEOUT, 300초 주면 301.0s에 YES: **진짜로
  ~300초짜리 검사**고, 당시 60이라 쓰고 300을 준 예산 부풀림 위에서만 YES였다
  (그래서 이후 스윕의 `--timeout 300` 규약이 판정 보존에 필수다).
  `ExternMethod_inst`는 오늘 같은 검사가 **~61초에 YES** — 기록된 300.4초의
  1/5로, termination에서 확인된 08-10/11 환경 저하가 **CRC의 초에도 묻어
  있다**는 증거다(판정과 달리 초 단위 비교는 환경 지문 없이 무효).
  `ExternMethods_inst`는 분리 deadline로 재측정하지 않았다.
- 위 두 발견의 귀결: TIMEOUT 37건도 저하 환경에서 기록된 값이므로, 정직한
  예산(`--timeout 300`, 분리 deadline)의 재질의가 걸려 있다 — 일부는 닫힐 수
  있다.

| # | symbol | rules | confluence | termination |
|---|---|---|---|---|
| 1 | `$init_objectState` | 0 | YES (0.7s) | DEGENERATE |
| 2 | `ExternFunctionCall_eval_lctk` | 0 | YES (0.7s) | DEGENERATE |
| 3 | `$annotationList_of_parameterIR` | 1 | YES (0.8s) | YES (0.5s) |
| 4 | `$ctk_of_typedExpressionIR` | 1 | YES (0.8s) | YES (0.4s) |
| 5 | `$direction_of_parameterIR` | 1 | YES (0.8s) | YES (0.4s) |
| 6 | `$empty_map_callableId_callableDef` | 1 | YES (0.7s) | YES (0.4s) |
| 7 | `$empty_map_callableId_callableTypeDefIR` | 1 | YES (0.8s) | YES (0.4s) |
| 8 | `$empty_map_callableId_constructorDef` | 1 | YES (0.7s) | YES (0.4s) |
| 9 | `$empty_map_constructorId_constructorTypeDefIR` | 1 | YES (0.7s) | YES (0.4s) |
| 10 | `$empty_map_id_parserStateIR` | 1 | YES (0.7s) | YES (0.4s) |
| 11 | `$empty_map_id_value` | 1 | YES (0.8s) | YES (0.4s) |
| 12 | `$empty_map_id_varTypeIR` | 1 | YES (0.8s) | YES (0.4s) |
| 13 | `$empty_map_objectId_object` | 1 | YES (0.7s) | YES (0.4s) |
| 14 | `$empty_map_typeId_typeDefIR` | 1 | YES (0.7s) | YES (0.4s) |
| 15 | `$empty_map_typeId_typeIR` | 1 | YES (0.8s) | YES (0.4s) |
| 16 | `$empty_set` | 1 | YES (0.7s) | YES (0.4s) |
| 17 | `$empty_tableContext` | 1 | YES (0.8s) | YES (0.5s) |
| 18 | `$id_of_parameterIR` | 1 | YES (0.7s) | YES (0.4s) |
| 19 | `$invalidate_header` | 1 | YES (0.8s) | YES (0.4s) |
| 20 | `$name_of_parserState` | 1 | YES (0.7s) | YES (0.4s) |
| 21 | `$parameterListIR_of_actionDef` | 1 | YES (0.8s) | YES (0.4s) |
| 22 | `$parameterListIR_of_actionTypeDefIR` | 1 | YES (0.7s) | YES (0.4s) |
| 23 | `$parameterListIR_of_constructorTypeDefIR` | 1 | YES (0.7s) | YES (0.4s) |
| 24 | `$parameterListIR_of_controlApplyMethodDef` | 1 | YES (0.7s) | YES (0.4s) |
| 25 | `$parameterListIR_of_controlApplyMethodTypeIR` | 1 | YES (0.7s) | YES (0.4s) |
| 26 | `$parameterListIR_of_definedFunctionDef` | 1 | YES (0.8s) | YES (0.4s) |
| 27 | `$parameterListIR_of_definedFunctionTypeDefIR` | 1 | YES (0.7s) | YES (0.4s) |
| 28 | `$parameterListIR_of_externFunctionDef` | 1 | YES (0.7s) | YES (0.4s) |
| 29 | `$parameterListIR_of_externFunctionTypeDefIR` | 1 | YES (0.7s) | YES (0.4s) |
| 30 | `$parameterListIR_of_externMethodDef` | 1 | YES (0.7s) | YES (0.4s) |
| 31 | `$parameterListIR_of_parserApplyMethodDef` | 1 | YES (0.8s) | YES (0.4s) |
| 32 | `$parameterListIR_of_parserApplyMethodTypeIR` | 1 | YES (0.7s) | YES (0.4s) |
| 33 | `$parameterListIR_of_tableApplyMethodDef` | 1 | YES (0.8s) | YES (0.4s) |
| 34 | `$parameterListIR_of_tableApplyMethodTypeDefIR` | 1 | YES (0.7s) | YES (0.4s) |
| 35 | `$set_priority_of_tableEntryIR` | 1 | YES (0.8s) | YES (0.4s) |
| 36 | `$tableActionReferenceIR_of_tableActionIR` | 1 | YES (0.7s) | YES (0.4s) |
| 37 | `$tableEntryPriorityOptIR_of_tableEntryIR` | 1 | YES (0.7s) | YES (0.4s) |
| 38 | `$typeIR_of_parameterIR` | 1 | YES (0.8s) | YES (0.4s) |
| 39 | `$type_of_typedExpressionIR` | 1 | YES (0.7s) | YES (0.4s) |
| 40 | `$type_of_typedLvalueIR` | 1 | YES (0.7s) | YES (0.4s) |
| 41 | `$empty_callableDefEnv` | 2 | YES (0.8s) | YES (0.4s) |
| 42 | `$empty_callableTypeDefEnv` | 2 | YES (0.7s) | YES (0.4s) |
| 43 | `$empty_constructorDefEnv` | 2 | YES (0.8s) | YES (0.4s) |
| 44 | `$empty_constructorTypeDefEnv` | 2 | YES (0.8s) | YES (0.4s) |
| 45 | `$empty_frame` | 2 | YES (0.7s) | YES (0.4s) |
| 46 | `$empty_stateEnv` | 2 | YES (0.7s) | YES (0.4s) |
| 47 | `$empty_theta` | 2 | YES (0.8s) | YES (0.4s) |
| 48 | `$empty_typeDefEnv` | 2 | YES (0.8s) | YES (0.4s) |
| 49 | `$empty_typeFrame` | 2 | YES (0.8s) | YES (0.4s) |
| 50 | `$flatten_constOpt` | 2 | YES (0.8s) | YES (0.4s) |
| 51 | `$flatten_objectInitializerOptIR` | 2 | YES (0.8s) | YES (0.4s) |
| 52 | `$is_some_` | 2 | YES (0.7s) | YES (0.4s) |
| 53 | `$ite_boolValue` | 2 | YES (0.7s) | YES (0.4s) |
| 54 | `$ite_callTargetMatch` | 2 | YES (0.8s) | YES (0.5s) |
| 55 | `$ite_controlPlaneNameIR` | 2 | YES (0.7s) | YES (0.4s) |
| 56 | `$ite_int` | 2 | YES (0.8s) | YES (0.4s) |
| 57 | `$ite_text` | 2 | YES (0.7s) | YES (0.4s) |
| 58 | `$opt_as_seq__nameIR` | 2 | YES (0.7s) | YES (0.4s) |
| 59 | `$parameterListIR_of_externMethodTypeDefIR` | 2 | YES (0.8s) | YES (0.4s) |
| 60 | `$type_of_externMethodPrototypeIR` | 2 | YES (0.8s) | YES (0.4s) |
| 61 | `$callable_builtinMethod` | 3 | YES (0.8s) | YES (0.4s) |
| 62 | `$empty_constraint` | 3 | YES (0.8s) | YES (0.4s) |
| 63 | `$empty_store` | 3 | YES (0.7s) | YES (0.4s) |
| 64 | `$instantiable_extern` | 3 | YES (0.7s) | YES (0.4s) |
| 65 | `$join_tableEntryState` | 3 | YES (0.8s) | YES (0.4s) |
| 66 | `$un_lnot` | 3 | YES (0.8s) | YES (0.4s) |
| 67 | `$concat_text` | 4 | YES (0.8s) | YES (0.4s) |
| 68 | `$constructorTypeDef_of_externConstructorPrototypeIR` | 4 | YES (0.8s) | YES (0.5s) |
| 69 | `$constructor_of_externConstructorPrototypeIR` | 4 | YES (0.8s) | YES (0.5s) |
| 70 | `$exists_` | 4 | YES (0.7s) | YES (0.4s) |
| 71 | `$flatten_blockElementStatementList` | 4 | YES (0.8s) | YES (0.5s) |
| 72 | `$flatten_controlLocalDeclarationList` | 4 | YES (0.7s) | YES (0.4s) |
| 73 | `$flatten_externConstructorOrMethodPrototypeList` | 4 | YES (0.8s) | YES (0.5s) |
| 74 | `$flatten_objectDeclarationList` | 4 | YES (0.8s) | YES (0.5s) |
| 75 | `$flatten_parserLocalDeclarationList` | 4 | YES (0.8s) | YES (0.5s) |
| 76 | `$flatten_parserStatementList` | 4 | YES (0.8s) | YES (0.5s) |
| 77 | `$flatten_prefixedNameIR` | 4 | YES (0.8s) | YES (0.5s) |
| 78 | `$flatten_selectCaseList` | 4 | YES (0.8s) | YES (0.4s) |
| 79 | `$flatten_switchCaseList` | 4 | YES (0.7s) | YES (0.4s) |
| 80 | `$flatten_tableActionList` | 4 | YES (0.8s) | YES (0.4s) |
| 81 | `$flatten_tableEntryList` | 4 | YES (0.8s) | YES (0.4s) |
| 82 | `$flatten_tableKeyList` | 4 | YES (0.8s) | YES (0.4s) |
| 83 | `$flatten_tablePropertyList` | 4 | YES (0.8s) | YES (0.4s) |
| 84 | `$flatten_typeFieldList` | 4 | YES (0.7s) | YES (0.4s) |
| 85 | `$forall_` | 4 | YES (0.8s) | YES (0.4s) |
| 86 | `$is_lpm_key_prime` | 4 | YES (0.8s) | YES (0.5s) |
| 87 | `$join_flow` | 4 | YES (0.8s) | YES (0.4s) |
| 88 | `$add_action_tbl` | 5 | YES (0.7s) | YES (0.5s) |
| 89 | `$add_key_tbl` | 5 | YES (0.8s) | YES (0.5s) |
| 90 | `$enter_path_i` | 5 | YES (0.7s) | YES (0.5s) |
| 91 | `$filter__externConstructorOrMethodPrototype` | 5 | YES (0.8s) | YES (0.5s) |
| 92 | `$filter__id` | 5 | YES (0.8s) | YES (0.5s) |
| 93 | `$filter__tableKeyIR` | 5 | YES (0.8s) | YES (0.5s) |
| 94 | `$filter__tableProperty` | 5 | YES (0.7s) | YES (0.5s) |
| 95 | `$filter__tablePropertyIR` | 5 | YES (0.8s) | YES (0.5s) |
| 96 | `$flatten_p4program` | 5 | YES (0.8s) | YES (0.5s) |
| 97 | `$is_default_parameterIR` | 5 | YES (0.7s) | YES (0.5s) |
| 98 | `$is_lpm_key` | 5 | YES (0.8s) | YES (0.4s) |
| 99 | `$callableId_IR` | 6 | YES (0.8s) | YES (0.4s) |
| 100 | `$tableCustomName` | 6 | YES (0.8s) | YES (0.5s) |
| 101 | `$callableId_of_externConstructorPrototypeIR` | 7 | YES (0.8s) | YES (0.5s) |
| 102 | `$codom_map` | 7 | YES (0.8s) | YES (0.5s) |
| 103 | `$dom_map_callableId_callableDef` | 7 | YES (0.8s) | YES (0.4s) |
| 104 | `$dom_map_callableId_callableTypeDefIR` | 7 | YES (0.8s) | YES (0.5s) |
| 105 | `$dom_map_callableId_constructorTypeDefIR` | 7 | YES (0.8s) | YES (0.4s) |
| 106 | `$dom_map_id_value` | 7 | YES (0.8s) | YES (0.4s) |
| 107 | `$dom_map_id_varTypeIR` | 7 | YES (0.8s) | YES (0.5s) |
| 108 | `$dom_map_nameIR_parserStateIR` | 7 | YES (0.8s) | YES (0.5s) |
| 109 | `$dom_map_typeId_infer` | 7 | YES (0.8s) | YES (0.5s) |
| 110 | `$dom_map_typeId_typeDefIR` | 7 | YES (0.8s) | YES (0.5s) |
| 111 | `$enter_i` | 7 | YES (0.8s) | YES (0.5s) |
| 112 | `$enter_t` | 7 | YES (0.8s) | YES (0.4s) |
| 113 | `$is_concrete_extern_object_prime_prime` | 7 | YES (0.8s) | YES (0.4s) |
| 114 | `$callableId_of_externMethodPrototypeIR` | 8 | YES (0.8s) | YES (0.5s) |
| 115 | `$requires_priority_prime` | 8 | YES (1.0s) | YES (0.5s) |
| 116 | `$empty_typingContext` | 9 | YES (0.8s) | YES (0.5s) |
| 117 | `$is_tableActionsProperty` | 9 | YES (0.8s) | YES (0.4s) |
| 118 | `$is_tableKeysProperty` | 9 | YES (0.8s) | YES (0.4s) |
| 119 | `$join_ctk` | 9 | YES (0.8s) | YES (0.4s) |
| 120 | `$requires_priority` | 9 | YES (1.1s) | YES (0.5s) |
| 121 | `$exit_i` | 10 | YES (0.8s) | YES (0.5s) |
| 122 | `$exit_t` | 10 | YES (0.7s) | YES (0.5s) |
| 123 | `$name` | 10 | YES (0.9s) | YES (0.5s) |
| 124 | `$prefixedTypeName` | 12 | YES (1.0s) | YES (0.6s) |
| 125 | `$resolve_constraint` | 12 | YES (0.8s) | YES (0.5s) |
| 126 | `$assignop_as_binop` | 13 | YES (0.8s) | YES (0.4s) |
| 127 | `$empty_instContext` | 13 | YES (0.8s) | YES (0.4s) |
| 128 | `$flatten_nameList` | 13 | YES (0.8s) | YES (0.5s) |
| 129 | `$flatten_typeParameterList` | 13 | YES (0.8s) | YES (0.5s) |
| 130 | `$inherit_i` | 13 | YES (0.8s) | YES (0.5s) |
| 131 | `$un_plus` | 13 | YES (0.8s) | YES (0.5s) |
| 132 | `$width_of_integerTypeIR` | 13 | YES (0.8s) | YES (0.5s) |
| 133 | `$flatten_typeParameterListOpt` | 15 | YES (0.8s) | YES (0.4s) |
| 134 | `$objectId_ends_with` | 15 | YES (0.8s) | YES (0.5s) |
| 135 | `$join_text` | 17 | YES (0.8s) | YES (0.5s) |
| 136 | `$prefixedNonTypeName` | 19 | YES (1.1s) | YES (0.5s) |
| 137 | `$is_tableDefaultActionProperty` | 20 | YES (1.2s) | YES (0.6s) |
| 138 | `$typedLvalueIR_as_typedExpressionIR` | 20 | YES (0.9s) | YES (0.7s) |
| 139 | `$optional_annotation_of_parameterIR_prime_prime` | 24 | YES (1.3s) | YES (0.6s) |
| 140 | `$lvalue_as_expression` | 27 | YES (1.0s) | YES (0.5s) |
| 141 | `$in_set_nameIR` | 42 | YES (1.4s) | YES (0.5s) |
| 142 | `$update_fieldValue` | 42 | YES (1.4s) | YES (0.6s) |
| 143 | `$in_set_id` | 43 | YES (1.4s) | YES (0.5s) |
| 144 | `$in_set_typeId` | 43 | YES (1.4s) | YES (0.5s) |
| 145 | `$joins_ctk` | 44 | YES (1.3s) | YES (0.6s) |
| 146 | `$find_action_prime` | 47 | - | YES (0.6s) |
| 147 | `$find_typeDef_i` | 48 | YES (2.2s) | YES (0.6s) |
| 148 | `$find_action` | 49 | - | YES (0.6s) |
| 149 | `$find_non_overloadeds_callableDef` | 49 | YES (1.7s) | YES (0.8s) |
| 150 | `$find_non_overloadeds_callableTypeDefIR` | 49 | YES (1.7s) | YES (0.8s) |
| 151 | `$find_non_overloadeds_externMethodTypeDefIR` | 49 | YES (1.7s) | YES (0.8s) |
| 152 | `$in_set_callableId` | 49 | YES (1.7s) | YES (0.5s) |
| 153 | `$add_store` | 50 | YES (1.8s) | YES (0.6s) |
| 154 | `$add_typeDef_i` | 50 | YES (2.4s) | YES (0.6s) |
| 155 | `$merge_frames` | 53 | YES (1.9s) | YES (0.6s) |
| 156 | `$starts_with` | 53 | YES (2.1s) | YES (0.5s) |
| 157 | `$find_store` | 54 | YES (1.9s) | YES (0.6s) |
| 158 | `$instantiable_package` | 54 | YES (2.0s) | YES (0.5s) |
| 159 | `$instantiable_table` | 54 | YES (2.0s) | YES (0.5s) |
| 160 | `$partition_parameterListIR` | 55 | - | YES (0.8s) |
| 161 | `$add_constructorDef_i` | 56 | YES (2.9s) | YES (0.7s) |
| 162 | `$add_type_i` | 56 | YES (2.9s) | YES (0.6s) |
| 163 | `$callable_externAbstractMethod` | 56 | - | YES (0.5s) |
| 164 | `$callable_externMethod` | 56 | - | YES (0.5s) |
| 165 | `$directionless_trailing_prime` | 57 | YES (1.9s) | YES (0.5s) |
| 166 | `$extend_map_typeId_typeIR` | 58 | - | YES (0.6s) |
| 167 | `$add_types_i` | 59 | YES (3.1s) | YES (0.7s) |
| 168 | `$split_dataplane_parameters` | 59 | - | YES (0.6s) |
| 169 | `$strip_prefix_rec` | 59 | YES (2.7s) | TIMEOUT (>20s) |
| 170 | `$add_constructorDefs_i` | 60 | YES (3.0s) | YES (0.9s) |
| 171 | `$directionless_trailing` | 61 | YES (2.2s) | YES (0.5s) |
| 172 | `$add_parserState_i` | 62 | YES (3.0s) | YES (0.7s) |
| 173 | `$bound` | 66 | YES (3.0s) | YES (0.7s) |
| 174 | `$add_constructorDef_t` | 71 | YES (3.9s) | YES (0.7s) |
| 175 | `$find_non_overloaded_callableDef` | 74 | YES (3.1s) | YES (0.9s) |
| 176 | `$find_non_overloaded_callableTypeDefIR` | 74 | YES (3.1s) | YES (0.9s) |
| 177 | `$find_non_overloaded_externMethodTypeDefIR` | 74 | YES (3.1s) | YES (0.9s) |
| 178 | `$find_typeDef_t` | 74 | YES (4.2s) | YES (0.8s) |
| 179 | `$add_constructorDefs_t` | 75 | YES (4.1s) | YES (0.8s) |
| 180 | `$add_callableDef_overload_i` | 76 | YES (4.7s) | YES (0.8s) |
| 181 | `$find_var_i` | 76 | YES (3.9s) | YES (0.8s) |
| 182 | `$find_var_t` | 76 | YES (4.3s) | YES (0.9s) |
| 183 | `$add_constructorDef_non_overload_t` | 79 | YES (4.3s) | YES (0.8s) |
| 184 | `$init_` | 79 | - | TIMEOUT (>20s) |
| 185 | `$add_callableDef_overload_t` | 80 | YES (5.0s) | YES (0.9s) |
| 186 | `$isValid_header` | 81 | YES (5.5s) | YES (0.5s) |
| 187 | `$repeat__value` | 81 | - | TIMEOUT (>20s) |
| 188 | `$add_var_i` | 82 | YES (5.0s) | YES (0.9s) |
| 189 | `$find_var_value_t` | 82 | YES (5.5s) | YES (0.9s) |
| 190 | `$add_typeDef_t` | 83 | YES (5.9s) | YES (1.0s) |
| 191 | `$fresh_typeIds` | 84 | YES (4.2s) | TIMEOUT (>20s) |
| 192 | `ConstDecl_inst` | 84 | YES (6.1s) | YES (0.9s) |
| 193 | `$add_vars_i` | 86 | YES (5.4s) | YES (1.2s) |
| 194 | `$add_typeDefs_t` | 87 | YES (6.2s) | YES (1.2s) |
| 195 | `$add_callableDef_non_overload_i` | 88 | YES (5.3s) | YES (1.1s) |
| 196 | `FuncDecl_inst` | 88 | YES (13.8s) | YES (0.9s) |
| 197 | `$find_callableDef_non_overloaded_t` | 89 | YES (4.8s) | YES (1.0s) |
| 198 | `$add_typeParameters_t` | 90 | - | YES (1.2s) |
| 199 | `ExternMethod_inst` | 90 | YES (300.4s) | YES (1.0s) |
| 200 | `$invalidate_headerUnion` | 91 | YES (7.2s) | YES (0.9s) |
| 201 | `$invalidate_value` | 91 | YES (7.2s) | YES (0.9s) |
| 202 | `$add_callableDef_non_overload_t` | 92 | YES (5.7s) | YES (1.3s) |
| 203 | `$ends_with` | 92 | YES (5.5s) | YES (0.6s) |
| 204 | `$find_callableDef_non_overload_i` | 94 | YES (5.1s) | YES (1.1s) |
| 205 | `ExternMethods_inst` | 94 | YES (300.4s) | YES (1.2s) |
| 206 | `ActionDecl_inst` | 97 | YES (8.3s) | YES (1.2s) |
| 207 | `$strip_suffix_rec` | 98 | YES (6.2s) | TIMEOUT (>20s) |
| 208 | `$bin_mod` | 109 | YES (8.2s) | YES (0.6s) |
| 209 | `$contains_prime` | 109 | YES (7.1s) | TIMEOUT (>20s) |
| 210 | `$replace_text_except_prime` | 111 | YES (8.2s) | TIMEOUT (>20s) |
| 211 | `$replace_text_prime` | 111 | YES (8.2s) | TIMEOUT (>20s) |
| 212 | `$contains` | 112 | YES (7.6s) | TIMEOUT (>20s) |
| 213 | `$bin_div` | 113 | YES (9.0s) | YES (0.6s) |
| 214 | `$replace_text` | 114 | YES (8.9s) | TIMEOUT (>20s) |
| 215 | `$replace_text_except` | 114 | YES (8.9s) | TIMEOUT (>20s) |
| 216 | `$write_bits_from_value` | 115 | - | YES (0.8s) |
| 217 | `TypeParameterListOpt_ok` | 122 | - | YES (1.5s) |
| 218 | `$modulo` | 126 | YES (10.5s) | YES (0.6s) |
| 219 | `$modulo_42` | 132 | YES (11.2s) | YES (0.6s) |
| 220 | `$un_bnot` | 142 | YES (15.2s) | YES (3.9s) |
| 221 | `$update_headerUnion` | 151 | YES (17.3s) | YES (0.9s) |
| 222 | `SwitchLabel_table_ok` | 180 | - | YES (4.9s) |
| 223 | `$tableEntry_lpm_prefix_prime` | 185 | YES (23.8s) | TIMEOUT (>20s) |
| 224 | `$tableEntry_lpm_prefix` | 186 | YES (24.1s) | TIMEOUT (>20s) |
| 225 | `$bin_eq` | 191 | - | YES (2.4s) |
| 226 | `$bin_ne` | 194 | - | YES (2.5s) |
| 227 | `$int_of_integerValue` | 200 | YES (30.5s) | TIMEOUT (>20s) |
| 228 | `$bin_ge` | 203 | YES (30.9s) | TIMEOUT (>20s) |
| 229 | `$bin_le` | 203 | YES (30.8s) | TIMEOUT (>20s) |
| 230 | `$bin_gt` | 204 | YES (31.3s) | TIMEOUT (>20s) |
| 231 | `$bin_lt` | 204 | YES (31.1s) | TIMEOUT (>20s) |
| 232 | `$nat_of_integerValue` | 207 | - | TIMEOUT (>20s) |
| 233 | `$name_annotationToken` | 213 | YES (85.7s) | TIMEOUT (>20s) |
| 234 | `$un_minus` | 223 | - | TIMEOUT (>20s) |
| 235 | `$bin_minus` | 225 | YES (37.6s) | TIMEOUT (>20s) |
| 236 | `$bin_mul` | 225 | YES (37.6s) | TIMEOUT (>20s) |
| 237 | `$bin_plus` | 225 | YES (37.6s) | TIMEOUT (>20s) |
| 238 | `$bin_bxor` | 235 | YES (53.8s) | TIMEOUT (>20s) |
| 239 | `$set_priorities_of_tableEntryListIR_prime` | 236 | YES (45.1s) | TIMEOUT (>20s) |
| 240 | `$un_op` | 240 | - | TIMEOUT (>20s) |
| 241 | `$bin_band` | 244 | YES (300.4s) | TIMEOUT (>20s) |
| 242 | `$bin_bor` | 244 | - | TIMEOUT (>20s) |
| 243 | `$bin_shl` | 244 | - | TIMEOUT (>20s) |
| 244 | `$bin_satminus` | 247 | YES (43.9s) | TIMEOUT (>20s) |
| 245 | `$bin_concat` | 248 | - | TIMEOUT (>20s) |
| 246 | `$bin_satplus` | 248 | - | TIMEOUT (>20s) |
| 247 | `$name_annotation_opt` | 268 | - | TIMEOUT (>20s) |
| 248 | `$bin_shr` | 287 | - | TIMEOUT (>20s) |
| 249 | `$set_priorities_of_tableEntryListIR` | 297 | YES (67.0s) | TIMEOUT (>20s) |
| 250 | `$write_value_field_from_bits_prime` | 312 | - | TIMEOUT (>20s) |
| 251 | `$write_value_fields_from_bits_prime` | 312 | - | TIMEOUT (>20s) |
| 252 | `$write_value_from_bits_prime` | 312 | - | TIMEOUT (>20s) |
| 253 | `$write_values_from_bits_prime` | 312 | - | TIMEOUT (>20s) |
| 254 | `$write_value_from_bits` | 317 | - | TIMEOUT (>20s) |
| 255 | `$bitacc_range_replace_op` | 508 | - | TIMEOUT (>20s) |
| 256 | `$bitacc_offset_replace_op` | 528 | - | TIMEOUT (>20s) |
| 257 | `$bitacc_offset_op` | 673 | - | TIMEOUT (>20s) |
| 258 | `$flatten_namedExpressionList` | 749 | - | YES (4.8s) |
| 259 | `$flatten_realTypeArgumentList` | 749 | - | YES (5.0s) |
| 260 | `$flatten_expressionList` | 750 | - | YES (4.6s) |
| 261 | `$flatten_typeArgumentList` | 750 | - | YES (5.1s) |
| 262 | `$bitacc_range_op` | 765 | - | TIMEOUT (>20s) |
| 263 | `$bin_op` | 774 | - | TIMEOUT (>20s) |
| 264 | `$expression_as_lvalue` | 777 | - | YES (12.3s) |
| 265 | `$flatten_argumentList` | 788 | - | YES (13.1s) |
| 266 | `$flatten_simpleKeysetExpressionList` | 790 | - | YES (4.6s) |
| 267 | `$flatten_forUpdateStatementList` | 794 | - | YES (5.3s) |
| 268 | `$is_singleton_list_expression` | 817 | - | YES (10.7s) |
| 269 | `$add_annotationList` | 871 | - | YES (6.5s) |
| 270 | `$flatten_annotationList` | 872 | - | YES (4.6s) |
| 271 | `$flatten_parameterList` | 876 | - | YES (5.1s) |
| 272 | `$flatten_constructorParameterListOpt` | 878 | - | YES (5.3s) |
| 273 | `$is_externConstructorPrototype` | 878 | TIMEOUT (>300s) | YES (5.7s) |
| 274 | `$is_externMethodPrototype` | 881 | - | YES (4.7s) |
| 275 | `$callableId_prime` | 886 | - | YES (6.4s) |
| 276 | `$callableId` | 887 | - | YES (6.6s) |
| 277 | `$constructorId_of_externConstructorPrototype` | 888 | - | YES (6.5s) |
| 278 | `$callableId_of_externMethodPrototype` | 889 | - | YES (7.3s) |
| 279 | `$constructorId` | 889 | - | YES (5.9s) |
| 280 | `$optional_annotation_of_parameterIR_prime` | 904 | - | YES (12.7s) |
| 281 | `$optional_annotation_of_parameterIR` | 907 | - | YES (12.2s) |
| 282 | `$is_optional_parameterIR` | 908 | - | YES (12.3s) |
| 283 | `$flatten_forInitStatementList` | 912 | - | YES (6.8s) |
| 284 | `$expressionNonBrace_as_expression` | 916 | - | TIMEOUT (>20s) |
| 285 | `$find_name_annotation_opt` | 922 | TIMEOUT (>300s) | YES (15.3s) |
| 286 | `$split_externConstructorOrMethodPrototypeList` | 945 | - | YES (10.5s) |
| 287 | `$match_overloaded_unnamed_parameterListIR_of_constructorTypeDefIR_constructorTypeDefIR` | 1020 | - | YES (9.9s) |
| 288 | `$match_overloaded_unnamed_parameterListIR_of_controlApplyMethodTypeIR_controlApplyMethodTypeDefIR` | 1020 | - | YES (9.7s) |
| 289 | `$match_overloaded_unnamed_parameterListIR_of_parserApplyMethodTypeIR_parserApplyMethodTypeIR` | 1020 | - | YES (9.1s) |
| 290 | `$match_overloaded_unnamed_parameterListIR_of_externMethodTypeDefIR_externMethodTypeDefIR` | 1021 | - | YES (9.3s) |
| 291 | `$flatten_parserStateList` | 1030 | - | YES (3.0s) |
| 292 | `$find_overloadeds_unnamed_parameterListIR_of_constructorTypeDefIR_constructorTypeDefIR` | 1031 | - | YES (17.7s) |
| 293 | `$find_overloadeds_unnamed_parameterListIR_of_controlApplyMethodTypeIR_controlApplyMethodTypeDefIR` | 1031 | - | TIMEOUT (>20s) |
| 294 | `$find_overloadeds_unnamed_parameterListIR_of_parserApplyMethodTypeIR_parserApplyMethodTypeIR` | 1031 | - | TIMEOUT (>20s) |
| 295 | `$find_overloadeds_unnamed_parameterListIR_of_externMethodTypeDefIR_externMethodTypeDefIR` | 1032 | - | YES (17.4s) |
| 296 | `$match_overloaded_named_parameterListIR_of_constructorTypeDefIR_constructorTypeDefIR` | 1043 | - | YES (9.1s) |
| 297 | `$match_overloaded_named_parameterListIR_of_controlApplyMethodTypeIR_controlApplyMethodTypeDefIR` | 1043 | - | YES (8.9s) |
| 298 | `$match_overloaded_named_parameterListIR_of_parserApplyMethodTypeIR_parserApplyMethodTypeIR` | 1043 | - | YES (8.3s) |
| 299 | `$match_overloaded_named_parameterListIR_of_externMethodTypeDefIR_externMethodTypeDefIR` | 1044 | - | YES (9.3s) |
| 300 | `$find_overloadeds_named_parameterListIR_of_constructorTypeDefIR_constructorTypeDefIR` | 1054 | - | YES (14.2s) |
| 301 | `$find_overloadeds_named_parameterListIR_of_controlApplyMethodTypeIR_controlApplyMethodTypeDefIR` | 1054 | - | YES (13.9s) |
| 302 | `$find_overloadeds_named_parameterListIR_of_parserApplyMethodTypeIR_parserApplyMethodTypeIR` | 1054 | - | YES (15.2s) |
| 303 | `$find_overloadeds_named_parameterListIR_of_externMethodTypeDefIR_externMethodTypeDefIR` | 1055 | - | YES (14.7s) |
| 304 | `$name_annotation` | 1145 | - | TIMEOUT (>20s) |
| 305 | `$name_annotation_default` | 1151 | - | TIMEOUT (>20s) |
| 306 | `$find_overloaded_parameterListIR_of_constructorTypeDefIR_constructorTypeDefIR` | 1161 | - | YES (20.7s) |
| 307 | `$find_overloaded_parameterListIR_of_controlApplyMethodTypeIR_controlApplyMethodTypeDefIR` | 1161 | - | YES (22.0s) |
| 308 | `$find_overloaded_parameterListIR_of_parserApplyMethodTypeIR_parserApplyMethodTypeIR` | 1161 | - | YES (20.6s) |
| 309 | `$find_overloaded_parameterListIR_of_externMethodTypeDefIR_externMethodTypeDefIR` | 1162 | - | YES (19.9s) |
| 310 | `$cast_header_stack` | 1194 | - | YES (2.9s) |
| 311 | `$compat_lnot` | 1195 | - | YES (7.7s) |
| 312 | `$callTargetKey_prime` | 1196 | - | YES (2.8s) |
| 313 | `$cast_header` | 1196 | - | YES (2.8s) |
| 314 | `$cast_struct` | 1196 | - | YES (2.8s) |
| 315 | `$nestable_constructor_package` | 1196 | - | YES (7.3s) |
| 316 | `$compat_divmod` | 1197 | - | YES (7.1s) |
| 317 | `$compat_logical` | 1197 | - | YES (6.8s) |
| 318 | `$name_expression` | 1197 | TIMEOUT (>300s) | TIMEOUT (>20s) |
| 319 | `$resolve_type_alias` | 1197 | - | YES (7.3s) |
| 320 | `$compat_bnot` | 1198 | - | YES (7.1s) |
| 321 | `$compat_array_index` | 1199 | - | YES (6.1s) |
| 322 | `$compat_bitslice_offset_index` | 1199 | - | YES (6.2s) |
| 323 | `$compat_bitslice_offset_width` | 1199 | - | YES (6.1s) |
| 324 | `$compat_bitslice_range_index` | 1199 | - | YES (6.2s) |
| 325 | `$compat_uplusminus` | 1199 | - | YES (6.5s) |
| 326 | `$nestable_headerStack` | 1199 | - | YES (6.3s) |
| 327 | `$nestable_headerUnion` | 1199 | - | YES (6.2s) |
| 328 | `$cast_bool` | 1200 | - | YES (10.5s) |
| 329 | `$nestable_constructor_control` | 1200 | - | YES (6.5s) |
| 330 | `$nestable_constructor_parser` | 1200 | - | YES (5.9s) |
| 331 | `$nestable_controlApplyMethod` | 1200 | - | YES (6.4s) |
| 332 | `$parameterListIR_of_functionTypeDefIR` | 1200 | - | YES (2.0s) |
| 333 | `$nestable_new_in_enum_serializable` | 1201 | - | YES (6.0s) |
| 334 | `$definable_constructor` | 1202 | - | YES (6.5s) |
| 335 | `$nestable_constructor_extern` | 1202 | - | YES (6.4s) |
| 336 | `$nestable_externFunction` | 1202 | - | YES (6.4s) |
| 337 | `$nestable_externMethod` | 1202 | - | YES (6.6s) |
| 338 | `$nestable_new` | 1202 | - | YES (5.7s) |
| 339 | `$nestable_parserApplyMethod` | 1202 | - | YES (6.0s) |
| 340 | `$callTargetKey` | 1203 | - | YES (4.9s) |
| 341 | `$compat_switch` | 1203 | - | YES (6.1s) |
| 342 | `$compat_table_lpm_ternary_range_key` | 1203 | - | YES (6.0s) |
| 343 | `$is_static_assert_callableTypeIR` | 1205 | TIMEOUT (>300s) | YES (1.6s) |
| 344 | `$compat_concat` | 1209 | - | YES (6.3s) |
| 345 | `$compat_table_exact_optional_key` | 1209 | - | YES (5.3s) |
| 346 | `$typedExpressionIR_as_typedLvalueIR` | 1209 | - | YES (2.0s) |
| 347 | `$callableTypeIR_of_callableTypeDefIR` | 1210 | - | YES (1.6s) |
| 348 | `$flatten_keysetExpressionIR` | 1210 | - | YES (1.5s) |
| 349 | `$typeParameterListIR_of_callableTypeDefIR` | 1210 | - | YES (1.7s) |
| 350 | `$nestable_enum_serializable` | 1211 | - | YES (5.9s) |
| 351 | `$parameterListIR_of_methodTypeDefIR` | 1211 | - | YES (1.5s) |
| 352 | `$nestable_tuple_in_set` | 1213 | - | YES (5.4s) |
| 353 | `$check_switchLabel_default` | 1216 | - | YES (6.4s) |
| 354 | `$nestable_sequence_in_set` | 1216 | - | YES (5.4s) |
| 355 | `$nestable_struct_in_header` | 1216 | - | YES (8.2s) |
| 356 | `$compat_shift` | 1217 | - | YES (5.6s) |
| 357 | `$is_monomorphic_typeDefIR` | 1219 | TIMEOUT (>300s) | YES (1.4s) |
| 358 | `$typeId_of_typeDefIR` | 1219 | - | YES (1.5s) |
| 359 | `$typeParameterListIR_of_typeDefIR` | 1219 | - | YES (1.7s) |
| 360 | `$nestable_tuple` | 1220 | - | YES (5.3s) |
| 361 | `$compat_bitslice_base` | 1221 | - | YES (5.0s) |
| 362 | `$nestable_struct` | 1221 | - | YES (4.9s) |
| 363 | `$is_polymorphic_typeDefIR` | 1222 | - | YES (1.4s) |
| 364 | `$init_tableKeys` | 1223 | - | YES (2.4s) |
| 365 | `$nestable_definedFunction` | 1224 | - | YES (5.2s) |
| 366 | `$nestable_action` | 1225 | - | YES (4.9s) |
| 367 | `$nestable_list` | 1225 | - | YES (5.0s) |
| 368 | `$is_equalable_typeIR` | 1226 | - | YES (5.1s) |
| 369 | `$is_assignable_typeIR` | 1227 | - | YES (5.1s) |
| 370 | `$unroll_typeIR` | 1228 | TIMEOUT (>300s) | YES (4.7s) |
| 371 | `$typeIR_of_typeDefIR` | 1229 | - | YES (1.3s) |
| 372 | `$unroll_aliasType` | 1231 | TIMEOUT (>300s) | YES (4.9s) |
| 373 | `$is_table_application` | 1232 | - | YES (4.6s) |
| 374 | `$nestable_typedef` | 1232 | - | YES (5.1s) |
| 375 | `$nestable_header` | 1235 | - | YES (9.5s) |
| 376 | `$compat_bitwise` | 1236 | - | YES (4.5s) |
| 377 | `$compat_satplusminus` | 1236 | - | YES (4.4s) |
| 378 | `$parameterListIR_of_callableTypeDefIR` | 1236 | - | YES (1.4s) |
| 379 | `$compat_compare` | 1237 | - | YES (4.4s) |
| 380 | `$compat_mask` | 1237 | - | YES (4.3s) |
| 381 | `$compat_plusminusmult` | 1237 | - | YES (4.2s) |
| 382 | `$compat_range` | 1237 | - | YES (4.5s) |
| 383 | `$is_defaultable_typeIR` | 1237 | - | YES (8.2s) |
| 384 | `$is_static_callableTypeIR` | 1248 | TIMEOUT (>300s) | YES (1.3s) |
| 385 | `$nestable_set` | 1258 | - | YES (6.3s) |
| 386 | `$callable_definedFunction` | 1259 | - | YES (1.2s) |
| 387 | `$compat_table_key` | 1266 | - | YES (5.3s) |
| 388 | `$align_parameterListIR_given` | 1272 | - | YES (6.0s) |
| 389 | `$sizeof_minSizeInBits_prime` | 1276 | - | YES (7.4s) |
| 390 | `$sizeof_minSizeInBits` | 1277 | - | YES (7.5s) |
| 391 | `$result_concat` | 1280 | - | YES (4.8s) |
| 392 | `$find_local_return_type_t` | 1287 | - | YES (1.3s) |
| 393 | `$align_parameterListIR` | 1300 | - | YES (5.8s) |
| 394 | `$sizeof_maxSizeInBits_prime` | 1302 | - | YES (5.2s) |
| 395 | `$sizeof_maxSizeInBits` | 1303 | - | YES (5.9s) |
| 396 | `$resolve_inference_prime` | 1305 | - | YES (4.3s) |
| 397 | `$parameterListIR_of_functionDef` | 1306 | - | YES (1.1s) |
| 398 | `$resolve_inference` | 1312 | - | YES (5.1s) |
| 399 | `$is_concrete_extern_object_prime` | 1314 | TIMEOUT (>300s) | YES (1.2s) |
| 400 | `$reduce_serenum` | 1315 | - | YES (4.3s) |
| 401 | `$reduce_serenum_unary_compat_lnot` | 1320 | TIMEOUT (>300s) | YES (6.1s) |
| 402 | `$subexpressions_of_argumentIR` | 1320 | TIMEOUT (>300s) | YES (5.9s) |
| 403 | `$subexpressions_of_argumentListIR` | 1320 | - | YES (5.5s) |
| 404 | `$subexpressions_of_expressionIR` | 1320 | TIMEOUT (>300s) | YES (5.2s) |
| 405 | `$subexpressions_of_typedExpressionIR` | 1320 | - | YES (5.6s) |
| 406 | `$subexpressions_of_typedExpressionListIR` | 1320 | - | YES (5.9s) |
| 407 | `$reduce_serenum_unary_compat_bnot` | 1323 | TIMEOUT (>300s) | YES (6.0s) |
| 408 | `$reduce_serenum_unary_compat_array_index` | 1324 | TIMEOUT (>300s) | YES (5.6s) |
| 409 | `$reduce_serenum_unary_compat_bitslice_offset_index` | 1324 | TIMEOUT (>300s) | YES (5.0s) |
| 410 | `$reduce_serenum_unary_compat_bitslice_offset_width` | 1324 | TIMEOUT (>300s) | YES (5.6s) |
| 411 | `$reduce_serenum_unary_compat_bitslice_range_index` | 1324 | TIMEOUT (>300s) | YES (5.2s) |
| 412 | `$reduce_serenum_unary_compat_uplusminus` | 1324 | TIMEOUT (>300s) | YES (4.9s) |
| 413 | `$is_valid_bitslice` | 1329 | TIMEOUT (>300s) | YES (2.4s) |
| 414 | `$reduce_serenum_binary_compat_divmod` | 1329 | TIMEOUT (>300s) | YES (8.6s) |
| 415 | `$reduce_serenum_binary_compat_logical` | 1329 | TIMEOUT (>300s) | YES (8.1s) |
| 416 | `$update_mode_tbl` | 1329 | - | YES (5.8s) |
| 417 | `$reduce_serenum_binary_compat_concat` | 1341 | TIMEOUT (>300s) | YES (7.9s) |
| 418 | `$reduce_serenum_unary_compat_bitslice_base` | 1346 | TIMEOUT (>300s) | YES (4.3s) |
| 419 | `$reduce_serenum_binary_compat_shift` | 1349 | TIMEOUT (>300s) | YES (5.4s) |
| 420 | `$is_concrete_extern_object` | 1352 | - | YES (4.0s) |
| 421 | `$add_var_t` | 1360 | - | YES (3.5s) |
| 422 | `$add_constructorParameter_t` | 1362 | - | YES (2.9s) |
| 423 | `$add_vars_t` | 1364 | - | YES (4.4s) |
| 424 | `$merge_externMethodDefEnvs` | 1364 | - | YES (3.5s) |
| 425 | `$sizeof_minSizeInBytes` | 1365 | - | YES (4.2s) |
| 426 | `$add_constructorParameters_t` | 1366 | - | YES (2.3s) |
| 427 | `$reduce_serenum_binary_compat_bitwise` | 1368 | TIMEOUT (>300s) | YES (3.7s) |
| 428 | `$reduce_serenum_binary_compat_satplusminus` | 1368 | TIMEOUT (>300s) | YES (4.0s) |
| 429 | `$reduce_serenum_binary_compat_compare` | 1369 | TIMEOUT (>300s) | YES (5.6s) |
| 430 | `$reduce_serenum_binary_compat_mask` | 1369 | TIMEOUT (>300s) | YES (5.3s) |
| 431 | `$reduce_serenum_binary_compat_plusminusmult` | 1369 | TIMEOUT (>300s) | YES (5.6s) |
| 432 | `$reduce_serenum_binary_compat_range` | 1369 | TIMEOUT (>300s) | YES (5.6s) |
| 433 | `$sizeof_maxSizeInBytes` | 1370 | - | YES (3.4s) |
| 434 | `$add_parameter_t` | 1375 | - | YES (2.1s) |
| 435 | `$match_overloaded_unnamed_parameterListIR_of_callableTypeDefIR_callableTypeDefIR` | 1404 | - | YES (3.7s) |
| 436 | `TableType_ok` | 1404 | - | YES (3.5s) |
| 437 | `$init_tableEntries` | 1413 | - | YES (1.4s) |
| 438 | `$sizeof` | 1413 | - | YES (4.4s) |
| 439 | `$find_overloadeds_unnamed_parameterListIR_of_callableTypeDefIR_callableTypeDefIR` | 1415 | - | YES (13.4s) |
| 440 | `$free_callableTypeDefIR` | 1418 | - | TIMEOUT (>20s) |
| 441 | `$free_callableTypeIR` | 1418 | - | TIMEOUT (>20s) |
| 442 | `$free_parameterIR` | 1418 | - | TIMEOUT (>20s) |
| 443 | `$free_typeIR` | 1418 | - | TIMEOUT (>20s) |
| 444 | `$match_overloaded_named_parameterListIR_of_callableTypeDefIR_callableTypeDefIR` | 1427 | - | YES (3.7s) |
| 445 | `$find_overloadeds_named_parameterListIR_of_callableTypeDefIR_callableTypeDefIR` | 1438 | - | YES (10.6s) |
| 446 | `$free_typeDefIR` | 1480 | - | TIMEOUT (>20s) |
| 447 | `$init_table` | 1486 | - | YES (1.5s) |
| 448 | `$capture_avoiding_` | 1508 | - | TIMEOUT (>20s) |
| 449 | `$find_constructorDef_overloaded_t` | 1519 | - | YES (7.5s) |
| 450 | `$capture_avoiding` | 1526 | - | TIMEOUT (>20s) |
| 451 | `$find_overloaded_parameterListIR_of_callableTypeDefIR_callableTypeDefIR` | 1545 | - | YES (18.3s) |
| 452 | `$callable_controlApplyMethod` | 1551 | - | YES (0.6s) |
| 453 | `$callable_parserApplyMethod` | 1551 | - | YES (0.7s) |
| 454 | `$callable_action` | 1553 | - | YES (0.7s) |
| 455 | `$callable_tableApplyMethod` | 1553 | - | YES (0.6s) |
| 456 | `$parameterListIR_of_methodDef` | 1556 | - | YES (0.7s) |
| 457 | `$instantiable_control` | 1572 | - | YES (0.6s) |
| 458 | `$instantiable_parser` | 1572 | - | YES (0.7s) |
| 459 | `$parameterListIR_of_constructorDef` | 1574 | - | YES (0.7s) |
| 460 | `$callable_externFunction` | 1575 | - | YES (0.6s) |
| 461 | `$find_callableDef_overloaded_t` | 1580 | - | YES (18.5s) |
| 462 | `$parameterListIR_of_callableDef` | 1580 | - | YES (0.7s) |
| 463 | `TableAction_inst` | 1738 | - | TIMEOUT (>20s) |
| 464 | `$match_overloaded_unnamed_parameterListIR_of_constructorDef_constructorDef` | 1742 | - | YES (3.8s) |
| 465 | `TableActions_inst` | 1742 | - | TIMEOUT (>20s) |
| 466 | `$find_overloadeds_unnamed_parameterListIR_of_constructorDef_constructorDef` | 1753 | - | YES (9.0s) |
| 467 | `$match_overloaded_named_parameterListIR_of_constructorDef_constructorDef` | 1765 | - | YES (4.0s) |
| 468 | `$find_overloadeds_named_parameterListIR_of_constructorDef_constructorDef` | 1776 | - | YES (7.6s) |
| 469 | `$subst_callableTypeDefIR` | 1801 | - | TIMEOUT (>20s) |
| 470 | `$subst_callableTypeDefIR_prime` | 1801 | - | TIMEOUT (>20s) |
| 471 | `$subst_callableTypeIR_prime` | 1801 | - | TIMEOUT (>20s) |
| 472 | `$subst_parameterIR` | 1801 | - | TIMEOUT (>20s) |
| 473 | `$subst_parameterIR_prime` | 1801 | - | TIMEOUT (>20s) |
| 474 | `$subst_typeIR` | 1801 | - | TIMEOUT (>20s) |
| 475 | `$subst_typeIR_prime` | 1801 | - | TIMEOUT (>20s) |
| 476 | `$subst_callableTypeIR` | 1805 | - | TIMEOUT (>20s) |
| 477 | `$subst_constructorTypeIR_prime` | 1808 | - | TIMEOUT (>20s) |
| 478 | `$subst_constructorTypeIR` | 1812 | - | TIMEOUT (>20s) |
| 479 | `$subst_type_i` | 1826 | - | TIMEOUT (>20s) |
| 480 | `$specialize_callableTypeDefIR` | 1853 | - | TIMEOUT (>20s) |
| 481 | `$specialize_constructorTypeDefIR` | 1856 | - | TIMEOUT (>20s) |
| 482 | `$specialize_typeDefIR` | 1867 | - | TIMEOUT (>20s) |
| 483 | `$find_overloaded_parameterListIR_of_constructorDef_constructorDef` | 1883 | - | YES (10.7s) |
| 484 | `$find_constructorDef_i` | 1901 | - | YES (12.2s) |
| 485 | `Constructor_inst` | 1983 | - | YES (11.9s) |
| 486 | `$cast_default` | 2027 | - | TIMEOUT (>20s) |
| 487 | `$cast_enum` | 2027 | - | TIMEOUT (>20s) |
| 488 | `$cast_error` | 2027 | - | TIMEOUT (>20s) |
| 489 | `$cast_int` | 2027 | - | TIMEOUT (>20s) |
| 490 | `$cast_invalid_header` | 2027 | - | TIMEOUT (>20s) |
| 491 | `$cast_list` | 2027 | - | TIMEOUT (>20s) |
| 492 | `$cast_op` | 2027 | - | TIMEOUT (>20s) |
| 493 | `$cast_record` | 2027 | - | TIMEOUT (>20s) |
| 494 | `$cast_sequence` | 2027 | - | TIMEOUT (>20s) |
| 495 | `$cast_set` | 2027 | - | TIMEOUT (>20s) |
| 496 | `$cast_to_enum` | 2027 | - | TIMEOUT (>20s) |
| 497 | `$cast_to_enum_prime` | 2027 | - | TIMEOUT (>20s) |
| 498 | `$default` | 2027 | - | TIMEOUT (>20s) |
| 499 | `ParameterType_alpha` | 2437 | TIMEOUT (>300s) | YES (2.2s) |
| 500 | `ExternMethodType_alpha` | 2442 | - | YES (2.3s) |
| 501 | `Type_alpha` | 2635 | TIMEOUT (>300s) | YES (2.3s) |
| 502 | `$insert_NoAction_tablePropertyIR` | 2764 | - | YES (3.1s) |
| 503 | `Expr_lvalue_ok` | 2905 | - | YES (4.4s) |
| 504 | `$instantiable` | 3006 | - | YES (2.7s) |
| 505 | `ExternMethodTypeDef_alpha` | 3083 | - | TIMEOUT (>20s) |
| 506 | `$subst_externMethodTypeDefEnv` | 3141 | - | TIMEOUT (>20s) |
| 507 | `CallableTypeDef_wf` | 3626 | - | TIMEOUT (>20s) |
| 508 | `CallableType_wf` | 3626 | - | TIMEOUT (>20s) |
| 509 | `ParameterType_wf` | 3626 | - | TIMEOUT (>20s) |
| 510 | `ReturnType_wf` | 3626 | - | TIMEOUT (>20s) |
| 511 | `Type_wf` | 3626 | - | TIMEOUT (>20s) |
| 512 | `ConstructorParameterType_wf` | 3630 | - | TIMEOUT (>20s) |
| 513 | `TypeDef_wf` | 3691 | - | TIMEOUT (>20s) |
| 514 | `$cast_unary` | 3860 | TIMEOUT (>300s) | TIMEOUT (>20s) |
| 515 | `Cast_impl` | 3860 | TIMEOUT (>300s) | YES (19.8s) |
| 516 | `ConstructorType_wf` | 3890 | - | TIMEOUT (>20s) |
| 517 | `ConstructorTypeDef_wf` | 3907 | - | TIMEOUT (>20s) |
| 518 | `$merge_constraint_prime` | 3917 | - | TIMEOUT (>20s) |
| 519 | `$merge_constraint` | 3933 | - | TIMEOUT (>20s) |
| 520 | `$merge_constraints` | 3937 | - | TIMEOUT (>20s) |
| 521 | `Cast_impl_neq` | 4177 | TIMEOUT (>300s) | TIMEOUT (>20s) |
| 522 | `Cast_expl` | 4223 | - | TIMEOUT (>20s) |
| 523 | `Cast_expl_neq` | 4223 | - | TIMEOUT (>20s) |
| 524 | `ConstructorType_ok` | 4777 | - | TIMEOUT (>20s) |
| 525 | `CallableType_ok` | 5104 | - | TIMEOUT (>20s) |
| 526 | `$gen_constraint` | 5325 | TIMEOUT (>300s) | TIMEOUT (>20s) |
| 527 | `$gen_constraints` | 5325 | - | TIMEOUT (>20s) |
| 528 | `$infer_prime` | 5333 | - | TIMEOUT (>20s) |
| 529 | `$infer` | 5448 | - | TIMEOUT (>20s) |
| 530 | `$cast_binary` | 5457 | TIMEOUT (>300s) | TIMEOUT (>20s) |
| 531 | `Call_convention_expr_ok` | 5512 | - | TIMEOUT (>20s) |
| 532 | `Call_convention_argument_ok` | 5525 | - | TIMEOUT (>20s) |
| 533 | `Call_convention_ok` | 5529 | - | TIMEOUT (>20s) |
| 534 | `Call_action_default_ok` | 5674 | - | TIMEOUT (>20s) |
| 535 | `Call_action_partial_ok` | 5675 | - | TIMEOUT (>20s) |
| 536 | `Argument_inst` | 6202 | - | TIMEOUT (>20s) |
| 537 | `BlockElementStmtList_inst` | 6202 | - | TIMEOUT (>20s) |
| 538 | `BlockElementStmt_inst` | 6202 | - | TIMEOUT (>20s) |
| 539 | `Block_inst` | 6202 | - | TIMEOUT (>20s) |
| 540 | `Constructor_call` | 6202 | - | TIMEOUT (>20s) |
| 541 | `ControlLocalDecl_inst` | 6202 | - | TIMEOUT (>20s) |
| 542 | `ControlLocalDecls_inst` | 6202 | - | TIMEOUT (>20s) |
| 543 | `Copy_in_argument_inst` | 6202 | - | TIMEOUT (>20s) |
| 544 | `Copy_in_argument_inst_default` | 6202 | - | TIMEOUT (>20s) |
| 545 | `Copy_in_inst` | 6202 | - | TIMEOUT (>20s) |
| 546 | `Copy_in_inst_default` | 6202 | - | TIMEOUT (>20s) |
| 547 | `DirectApplicationStmt_inst` | 6202 | - | TIMEOUT (>20s) |
| 548 | `Expr_inst` | 6202 | - | TIMEOUT (>20s) |
| 549 | `Exprs_inst` | 6202 | - | TIMEOUT (>20s) |
| 550 | `InstDecl_inst` | 6202 | - | TIMEOUT (>20s) |
| 551 | `ObjectDecl_inst` | 6202 | - | TIMEOUT (>20s) |
| 552 | `ObjectDecls_inst` | 6202 | - | TIMEOUT (>20s) |
| 553 | `ParserLocalDecl_inst` | 6202 | - | TIMEOUT (>20s) |
| 554 | `ParserLocalDecls_inst` | 6202 | - | TIMEOUT (>20s) |
| 555 | `ParserState_inst` | 6202 | - | TIMEOUT (>20s) |
| 556 | `ParserStates_inst` | 6202 | - | TIMEOUT (>20s) |
| 557 | `ParserStmt_inst` | 6202 | - | TIMEOUT (>20s) |
| 558 | `ParserStmts_inst` | 6202 | - | TIMEOUT (>20s) |
| 559 | `Stmt_inst` | 6202 | - | TIMEOUT (>20s) |
| 560 | `SwitchCase_inst` | 6202 | - | TIMEOUT (>20s) |
| 561 | `SwitchCases_inst` | 6202 | - | TIMEOUT (>20s) |
| 562 | `TableProperties_inst` | 6202 | - | TIMEOUT (>20s) |
| 563 | `TableProperty_inst` | 6202 | - | TIMEOUT (>20s) |
| 564 | `Argument_eval_lctk` | 7784 | - | TIMEOUT (>20s) |
| 565 | `Expr_eval_lctk` | 7784 | - | TIMEOUT (>20s) |
| 566 | `Decl_inst` | 7903 | - | TIMEOUT (>20s) |
| 567 | `Decls_inst` | 7907 | - | TIMEOUT (>20s) |
| 568 | `Call_ok` | 8681 | - | TIMEOUT (>20s) |
| 569 | `Inst_ok` | 8885 | - | TIMEOUT (>20s) |
| 570 | `ArgumentList_ok` | 14564 | - | TIMEOUT (>20s) |
| 571 | `Argument_ok` | 14564 | - | TIMEOUT (>20s) |
| 572 | `CallableTarget_ok` | 14564 | - | TIMEOUT (>20s) |
| 573 | `Expr_ok` | 14564 | - | TIMEOUT (>20s) |
| 574 | `TypeArgumentList_ok` | 14564 | - | TIMEOUT (>20s) |
| 575 | `TypeArgument_ok` | 14564 | - | TIMEOUT (>20s) |
| 576 | `TypeArguments_ok` | 14564 | - | TIMEOUT (>20s) |
| 577 | `Type_ok` | 14564 | - | TIMEOUT (>20s) |
| 578 | `Enum_serializable_field_ok` | 14573 | - | TIMEOUT (>20s) |
| 579 | `InstDecl_non_objectInitializer_ok` | 14573 | - | TIMEOUT (>20s) |
| 580 | `TableEntry_priority_ok` | 14573 | - | TIMEOUT (>20s) |
| 581 | `ConstDecl_ok` | 14576 | - | TIMEOUT (>20s) |
| 582 | `Enum_serializable_fields_ok` | 14577 | - | TIMEOUT (>20s) |
| 583 | `Enum_serializable_fieldList_ok` | 14580 | - | TIMEOUT (>20s) |
| 584 | `CallableTarget_lvalue_ok` | 14594 | - | TIMEOUT (>20s) |
| 585 | `Parameter_ok` | 14599 | - | TIMEOUT (>20s) |
| 586 | `ParameterList_ok_inner` | 14604 | - | TIMEOUT (>20s) |
| 587 | `VarDecl_ok` | 14617 | - | TIMEOUT (>20s) |
| 588 | `ParameterList_ok` | 14620 | - | TIMEOUT (>20s) |
| 589 | `ConstructorParameterListOpt_ok` | 14622 | - | TIMEOUT (>20s) |
| 590 | `ForCollectionExpr_ok` | 14638 | - | TIMEOUT (>20s) |
| 591 | `ExternConstructor_ok` | 14663 | - | TIMEOUT (>20s) |
| 592 | `SelectCase_keyset_simple_ok` | 14683 | - | TIMEOUT (>20s) |
| 593 | `ExternMethod_ok` | 14686 | - | TIMEOUT (>20s) |
| 594 | `SwitchLabel_general_ok` | 14749 | - | TIMEOUT (>20s) |
| 595 | `TableEntry_action_ok` | 14767 | - | TIMEOUT (>20s) |
| 596 | `TableEntry_keyset_simple_ok` | 14812 | - | TIMEOUT (>20s) |
| 597 | `Lvalue_ok` | 14817 | - | TIMEOUT (>20s) |
| 598 | `TableEntry_keysets_simple_ok` | 14820 | - | TIMEOUT (>20s) |
| 599 | `TableDefaultAction_ok` | 14901 | - | TIMEOUT (>20s) |
| 600 | `TableAction_ok` | 14933 | - | TIMEOUT (>20s) |
| 601 | `TableActions_ok` | 14937 | - | TIMEOUT (>20s) |
| 602 | `SelectCase_keyset_ok` | 14984 | - | TIMEOUT (>20s) |
| 603 | `SelectCase_ok` | 14988 | - | TIMEOUT (>20s) |
| 604 | `ParserSelect_ok` | 15015 | - | TIMEOUT (>20s) |
| 605 | `ParserTransition_ok` | 15071 | - | TIMEOUT (>20s) |
| 606 | `TableKey_ok` | 15105 | - | TIMEOUT (>20s) |
| 607 | `TableKeys_ok` | 15109 | - | TIMEOUT (>20s) |
| 608 | `TableEntry_keyset_ok` | 15115 | - | TIMEOUT (>20s) |
| 609 | `TableEntry_ok` | 15338 | - | TIMEOUT (>20s) |
| 610 | `BlockElementStmtList_ok` | 16263 | - | TIMEOUT (>20s) |
| 611 | `BlockElementStmt_ok` | 16263 | - | TIMEOUT (>20s) |
| 612 | `BlockElementStmts_ok` | 16263 | - | TIMEOUT (>20s) |
| 613 | `Block_ok` | 16263 | - | TIMEOUT (>20s) |
| 614 | `ForInitStmtList_ok` | 16263 | - | TIMEOUT (>20s) |
| 615 | `ForInitStmt_ok` | 16263 | - | TIMEOUT (>20s) |
| 616 | `ForInitStmts_ok` | 16263 | - | TIMEOUT (>20s) |
| 617 | `ForUpdateStmtList_ok` | 16263 | - | TIMEOUT (>20s) |
| 618 | `ForUpdateStmt_ok` | 16263 | - | TIMEOUT (>20s) |
| 619 | `Stmt_ok` | 16263 | - | TIMEOUT (>20s) |
| 620 | `SwitchCaseList_general_ok` | 16263 | - | TIMEOUT (>20s) |
| 621 | `SwitchCaseList_table_ok` | 16263 | - | TIMEOUT (>20s) |
| 622 | `SwitchCase_general_ok` | 16263 | - | TIMEOUT (>20s) |
| 623 | `SwitchCase_table_ok` | 16263 | - | TIMEOUT (>20s) |
| 624 | `SwitchCases_general_ok` | 16263 | - | TIMEOUT (>20s) |
| 625 | `SwitchCases_table_ok` | 16263 | - | TIMEOUT (>20s) |
| 626 | `ActionDecl_ok` | 16368 | - | TIMEOUT (>20s) |
| 627 | `ParserStmtList_ok` | 16395 | - | TIMEOUT (>20s) |
| 628 | `ParserStmt_ok` | 16395 | - | TIMEOUT (>20s) |
| 629 | `ParserStmts_ok` | 16395 | - | TIMEOUT (>20s) |
| 630 | `FuncDecl_ok` | 16410 | - | TIMEOUT (>20s) |
| 631 | `InstDecl_objectInitializer_ok` | 16619 | - | TIMEOUT (>20s) |
| 632 | `InstDecl_ok` | 16619 | - | TIMEOUT (>20s) |
| 633 | `ObjectDeclList_ok` | 16619 | - | TIMEOUT (>20s) |
| 634 | `ObjectDecl_ok` | 16619 | - | TIMEOUT (>20s) |
| 635 | `ObjectDecls_ok` | 16619 | - | TIMEOUT (>20s) |
| 636 | `TableProperty_ok` | 16644 | - | TIMEOUT (>20s) |
| 637 | `TableProperties_ok` | 16648 | - | TIMEOUT (>20s) |
| 638 | `ParserLocalDecl_ok` | 16655 | - | TIMEOUT (>20s) |
| 639 | `ParserLocalDecls_ok` | 16659 | - | TIMEOUT (>20s) |
| 640 | `ParserLocalDeclList_ok` | 16664 | - | TIMEOUT (>20s) |
| 641 | `Table_ok` | 16745 | - | TIMEOUT (>20s) |
| 642 | `ParserState_ok` | 16897 | - | TIMEOUT (>20s) |
| 643 | `ParserStateList_ok` | 16925 | - | TIMEOUT (>20s) |
| 644 | `ControlLocalDecl_ok` | 19152 | - | TIMEOUT (>20s) |
| 645 | `ControlLocalDecls_ok` | 19156 | - | TIMEOUT (>20s) |
| 646 | `ControlLocalDeclList_ok` | 19161 | - | TIMEOUT (>20s) |
| 647 | `Decl_ok` | 20388 | - | TIMEOUT (>20s) |
| 648 | `Decls_ok` | 20392 | - | TIMEOUT (>20s) |
| 649 | `Program_ok` | 20399 | - | TIMEOUT (>20s) |
| 650 | `Program_inst` | 21884 | - | TIMEOUT (>20s) |
