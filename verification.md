# P4 structural CTRS — 검증 결과 (표)

> **표만 둔다.** 각 셀은 `판정 (초s)` — 판정과 그 판정에 걸린 심볼당 **직렬 fresh** 벽시계다.
> 행은 슬라이스 규칙 수 오름차순. 종전의 ≤500 / >500 두 표는 **한 표로 합쳤다** — 밴드
> 경계는 측정 시기의 산물이지 성질의 구분이 아니었다. 열 이름은 이제 그 열을 만드는
> 서브커맨드 이름과 같다(`main.exe confluence` / `main.exe termination`).
> **confluence** = Church-Rosser(합류성): `YES` / `YES*`(=`--crc-normalize` upgrade-only로
> 닫힘) / `MAYBE` / `TIMEOUT (>Xs)`(적힌 예산 X초 내 미완) / `-`(미측정). **ChC(Coherence)는
> 2026-07-24부로 측정 중단·열 삭제**(사유·이력은 notes).
> **termination** = 구조 보존 unravel → AProVE 직접. 그 초는 답을 낸 AProVE 실행 하나의
> 벽시계다. 예산 사다리(5·20·80·…·cap)를 올라가며 답이 나오는 최소 예산에서 멈추는데,
> **예산은 안 적는다** — AProVE가 마감 전에 답하는 경우가 많아 예산은 실제 시간과 무관한
> 천장이거나 초와 겹치므로, 초 하나가 유일한 정직한 심볼당 수치다.
> 규칙 0(정의만 있고 절이 없는) 심볼 9개는 슬라이스가 비어 CRC·termination 둘 다
> 자명하므로 표에서 뺐다.
> **규칙 ~5만대 대형 슬라이스 구간은 사다리 시작 rung을 올려 쟀다**(`--budget-from`,
> `3988f2ec`). 이 구간은 전부 170~360초에 답해서 5·20·80 rung이 헛돌기만 했다. 대신
> 「답하는 최소 예산」이라는 사다리의 보장이 약해진다 — AProVE는 증명을 찾은 시점이 아니라
> **자기 마감에** 판정을 찍으므로, 마감에 걸린 행의 초는 비용이 아니라 rung 그 자체다.
> 그런 행은 `$find_typeDef_i`(1289.6초) 하나뿐이며 그 값은 **상한**이다.
> **최상위 관계** `Program_ok`(60,723규칙)·`Program_inst`(61,345규칙)는 표에서 가장 큰
> 슬라이스로, 사다리를 `[5120, 20480]`로 따로 걸어 쟀다. `orient_conds` 이전 `Program_ok`은
> 440.3초 YES였는데, 되살아난 절들이 AProVE 탐색을 폭발시켜 20,480초 rung에서야 YES가 났다
> — 그 초는 마감값이라 **상한**이고, `Program_inst`은 같은 예산 안에 못 끝내 TIMEOUT이다.
> 판정 회귀가 아니라 도구 비용의 폭발이며, 측정된 나머지 심볼은 모두 YES다.
>
> **범위**: 슬라이스 가능한 심볼 574개 중 규칙 0(정의만 있고 절이 없는) 9개를 뺀 **565개
> 전부**를 싣는다. 셀은 이번에 직접 측정한 것만 채웠다 — termination 308(YES 307·TIMEOUT 1),
> confluence 278(YES 150 · YES* 6 · TIMEOUT 122). 나머지는 `-`(미측정)이며, 스윕이 규칙
> 오름차순이라 미측정은 무-verdict 1건(`$flatten_argumentList` 784규칙)을 빼면 전부
> **2491규칙 이상**(중앙값 52,394)이다. confluence 스윕은 2,490규칙까지 채운 뒤
> **2026-07-30에 세웠다**(사유는 아래 종합). termination 스윕도 멈춰 있다.
>
> 재현 커맨드·방법론·측정 이력·비-YES 해석·병렬
> 안전성 소견은 모두 **[verification-notes.md](verification-notes.md)**.
>
> **측정 기준**: 두 열 모두 `orient_conds`(`bdceb303`) 이후 tip(`c10bde20`)에서 **이번에
> 직접 측정한 값만** 싣는다. 옛 `bff805ec` confluence 측정은 `orient_conds`가 분석 표면의
> 조건 방향을 바꿔 stale해졌으므로 옮겨오지 않고, 재측정 못 한 셀은 `-`로 비웠다. 지금까지
> confluence 재측정 278심볼은 **다운그레이드 0**(뒤집힌 조건을 포함했던 슬라이스도 전부 옛 판정
> 유지, `$expression_as_lvalue`만 옛 무-verdict에서 `YES*`로 올라섰다) — `orient_conds`는
> 측정된 범위에서 confluence-neutral이다. `$write_value*` 5행은 `n_var > 0` 가드를 넣은
> 스펙(`8bf15510`)에서 다시 쟀고, 판정은 그대로 `YES*`다.

## 종합 (565심볼)

**termination**: 측정 308 — YES 307 · TIMEOUT 1 · 비종료 후보 0. 미측정 `-` 257. TIMEOUT 1건은 최상위 관계 `Program_inst`(61,345규칙)로, 20,480초 예산 내 미완일 뿐 비종료 witness가 아니다(도구 예산 한계).
**confluence**: 측정 278 — YES 150 · YES* 6 · TIMEOUT 122. 미측정 `-` 287. TIMEOUT 122건은 둘로 나뉜다 — 400규칙 이하의 비트벡터 산술 7건(`$write_bits_from_value`·`$bin_shl`·`$bin_shr`·`$bitacc_*` 4개)과, **874~2490규칙 구간에서 예산(base 2040초, 정규화까지 가면 4080초)을 그대로 소진한 115건**이다. 둘 다 임계쌍 폭발로 예산이 먼저 끝난 것이며 비합류 witness가 아니다. **874규칙 위는 예외 없이 전부 TIMEOUT**이고 남은 287심볼은 중앙값 52,394규칙이라 같은 예산으로는 같은 결과가 나온다 — 그래서 스윕을 여기서 세웠다. 이 축을 더 밀려면 예산이 아니라 슬라이스를 줄여야 한다(추가 pruning / 모듈러 분해).

| # | symbol | rules | confluence | termination |
|---|---|---|---|---|
| 1 | `$annotationList_of_parameterIR` | 1 | YES (0.0s) | YES (0.4s) |
| 2 | `$ctk_of_typedExpressionIR` | 1 | YES (0.0s) | YES (0.4s) |
| 3 | `$empty_map` | 1 | YES (0.0s) | YES (0.4s) |
| 4 | `$empty_set` | 1 | YES (0.0s) | YES (0.4s) |
| 5 | `$empty_tableContext` | 1 | YES (0.0s) | YES (0.4s) |
| 6 | `$id_of_parameterIR` | 1 | YES (0.0s) | YES (0.4s) |
| 7 | `$invalidate_header` | 1 | YES (0.0s) | YES (0.4s) |
| 8 | `$parameterListIR_of_actionDef` | 1 | YES (0.0s) | YES (0.4s) |
| 9 | `$parameterListIR_of_actionTypeDefIR` | 1 | YES (0.0s) | YES (0.4s) |
| 10 | `$parameterListIR_of_constructorTypeDefIR` | 1 | YES (0.0s) | YES (0.4s) |
| 11 | `$parameterListIR_of_controlApplyMethodDef` | 1 | YES (0.0s) | YES (0.4s) |
| 12 | `$parameterListIR_of_controlApplyMethodTypeIR` | 1 | YES (0.0s) | YES (0.4s) |
| 13 | `$parameterListIR_of_definedFunctionDef` | 1 | YES (0.0s) | YES (0.4s) |
| 14 | `$parameterListIR_of_definedFunctionTypeDefIR` | 1 | YES (0.0s) | YES (0.4s) |
| 15 | `$parameterListIR_of_externFunctionDef` | 1 | YES (0.0s) | YES (0.4s) |
| 16 | `$parameterListIR_of_externFunctionTypeDefIR` | 1 | YES (0.0s) | YES (0.4s) |
| 17 | `$parameterListIR_of_externMethodDef` | 1 | YES (0.0s) | YES (0.4s) |
| 18 | `$parameterListIR_of_parserApplyMethodDef` | 1 | YES (0.0s) | YES (0.4s) |
| 19 | `$parameterListIR_of_parserApplyMethodTypeIR` | 1 | YES (0.0s) | YES (0.4s) |
| 20 | `$parameterListIR_of_tableApplyMethodDef` | 1 | YES (0.0s) | YES (0.4s) |
| 21 | `$parameterListIR_of_tableApplyMethodTypeDefIR` | 1 | YES (0.0s) | YES (0.4s) |
| 22 | `$set_priority_of_tableEntryIR` | 1 | YES (0.0s) | YES (0.5s) |
| 23 | `$tableEntryPriorityOptIR_of_tableEntryIR` | 1 | YES (0.0s) | YES (0.4s) |
| 24 | `$type_of_typedExpressionIR` | 1 | YES (0.0s) | YES (0.4s) |
| 25 | `$type_of_typedLvalueIR` | 1 | YES (0.0s) | YES (0.4s) |
| 26 | `$empty_callableDefEnv` | 2 | YES (0.0s) | YES (0.4s) |
| 27 | `$empty_callableTypeDefEnv` | 2 | YES (0.0s) | YES (0.4s) |
| 28 | `$empty_constructorDefEnv` | 2 | YES (0.0s) | YES (0.4s) |
| 29 | `$empty_constructorTypeDefEnv` | 2 | YES (0.0s) | YES (0.4s) |
| 30 | `$empty_frame` | 2 | YES (0.0s) | YES (0.4s) |
| 31 | `$empty_stateEnv` | 2 | YES (0.0s) | YES (0.4s) |
| 32 | `$empty_store` | 2 | YES (0.0s) | YES (0.4s) |
| 33 | `$empty_theta` | 2 | YES (0.0s) | YES (0.4s) |
| 34 | `$empty_typeDefEnv` | 2 | YES (0.0s) | YES (0.4s) |
| 35 | `$empty_typeFrame` | 2 | YES (0.0s) | YES (0.4s) |
| 36 | `$flatten_constOpt` | 2 | YES (0.0s) | YES (0.4s) |
| 37 | `$flatten_objectInitializerOptIR` | 2 | YES (0.0s) | YES (0.4s) |
| 38 | `$is_some` | 2 | YES (0.0s) | YES (0.4s) |
| 39 | `$ite` | 2 | YES (0.0s) | YES (0.4s) |
| 40 | `$opt_as_seq` | 2 | YES (0.0s) | YES (0.4s) |
| 41 | `$parameterListIR_of_externMethodTypeDefIR` | 2 | YES (0.0s) | YES (0.4s) |
| 42 | `$type_of_externMethodPrototypeIR` | 2 | YES (0.0s) | YES (0.4s) |
| 43 | `$callable_builtinMethod` | 3 | YES (0.0s) | YES (0.4s) |
| 44 | `$constructorTypeDef_of_externConstructorPrototypeIR` | 3 | YES (0.0s) | YES (0.4s) |
| 45 | `$constructor_of_externConstructorPrototypeIR` | 3 | YES (0.0s) | YES (0.4s) |
| 46 | `$empty_constraint` | 3 | YES (0.0s) | YES (0.4s) |
| 47 | `$filter` | 3 | YES (0.0s) | YES (0.5s) |
| 48 | `$instantiable_extern` | 3 | YES (0.0s) | YES (0.4s) |
| 49 | `$is_lpm_key_prime` | 3 | YES (0.0s) | YES (0.4s) |
| 50 | `$join_tableEntryState` | 3 | YES (0.0s) | YES (0.4s) |
| 51 | `$un_lnot` | 3 | YES (0.0s) | YES (0.4s) |
| 52 | `$concat_text` | 4 | YES (0.0s) | YES (0.4s) |
| 53 | `$exists` | 4 | YES (0.0s) | YES (0.4s) |
| 54 | `$flatten_blockElementStatementList` | 4 | YES (0.0s) | YES (0.4s) |
| 55 | `$flatten_controlLocalDeclarationList` | 4 | YES (0.0s) | YES (0.4s) |
| 56 | `$flatten_externConstructorOrMethodPrototypeList` | 4 | YES (0.0s) | YES (0.4s) |
| 57 | `$flatten_objectDeclarationList` | 4 | YES (0.0s) | YES (0.4s) |
| 58 | `$flatten_parserLocalDeclarationList` | 4 | YES (0.0s) | YES (0.4s) |
| 59 | `$flatten_parserStatementList` | 4 | YES (0.0s) | YES (0.4s) |
| 60 | `$flatten_prefixedNameIR` | 4 | YES (0.0s) | YES (0.4s) |
| 61 | `$flatten_selectCaseList` | 4 | YES (0.0s) | YES (0.4s) |
| 62 | `$flatten_switchCaseList` | 4 | YES (0.0s) | YES (0.4s) |
| 63 | `$flatten_tableActionList` | 4 | YES (0.0s) | YES (0.4s) |
| 64 | `$flatten_tableEntryList` | 4 | YES (0.0s) | YES (0.4s) |
| 65 | `$flatten_tableKeyList` | 4 | YES (0.0s) | YES (0.4s) |
| 66 | `$flatten_tablePropertyList` | 4 | YES (0.0s) | YES (0.4s) |
| 67 | `$flatten_typeFieldList` | 4 | YES (0.0s) | YES (0.4s) |
| 68 | `$forall` | 4 | YES (0.0s) | YES (0.4s) |
| 69 | `$is_concrete_extern_object_prime_prime` | 4 | YES (0.1s) | YES (0.4s) |
| 70 | `$is_default_parameterIR` | 4 | YES (0.1s) | YES (0.4s) |
| 71 | `$is_lpm_key` | 4 | YES (0.1s) | YES (0.4s) |
| 72 | `$join_flow` | 4 | YES (0.0s) | YES (0.4s) |
| 73 | `$add_action_tbl` | 5 | YES (0.1s) | YES (0.4s) |
| 74 | `$add_key_tbl` | 5 | YES (0.1s) | YES (0.4s) |
| 75 | `$codom_map` | 5 | YES (0.1s) | YES (0.4s) |
| 76 | `$dom_map` | 5 | YES (0.1s) | YES (0.4s) |
| 77 | `$enter_path_i` | 5 | YES (0.1s) | YES (0.5s) |
| 78 | `$flatten_p4program` | 5 | YES (0.1s) | YES (0.4s) |
| 79 | `$empty_typingContext` | 6 | YES (0.1s) | YES (0.4s) |
| 80 | `$is_tableActionsProperty` | 6 | YES (0.1s) | YES (0.4s) |
| 81 | `$is_tableKeysProperty` | 6 | YES (0.1s) | YES (0.4s) |
| 82 | `$tableCustomName` | 6 | YES (0.1s) | YES (0.4s) |
| 83 | `$enter_i` | 7 | YES (0.1s) | YES (0.5s) |
| 84 | `$enter_t` | 7 | YES (0.1s) | YES (0.4s) |
| 85 | `$exit_i` | 7 | YES (0.1s) | YES (0.5s) |
| 86 | `$exit_t` | 7 | YES (0.1s) | YES (0.5s) |
| 87 | `$requires_priority_prime` | 7 | YES (0.1s) | YES (0.5s) |
| 88 | `$empty_instContext` | 8 | YES (0.1s) | YES (0.5s) |
| 89 | `$requires_priority` | 8 | YES (0.2s) | YES (0.5s) |
| 90 | `$typedLvalueIR_as_typedExpressionIR` | 8 | YES (0.2s) | YES (0.6s) |
| 91 | `$join_ctk` | 9 | YES (0.1s) | YES (0.4s) |
| 92 | `$resolve_constraint` | 9 | YES (0.1s) | YES (0.5s) |
| 93 | `$inherit_i` | 10 | YES (0.1s) | YES (0.5s) |
| 94 | `$name` | 10 | YES (0.2s) | YES (0.5s) |
| 95 | `$width_of_integerTypeIR` | 10 | YES (0.2s) | YES (0.5s) |
| 96 | `$objectId_ends_with` | 11 | YES (0.1s) | YES (0.5s) |
| 97 | `$un_plus` | 11 | YES (0.2s) | YES (0.4s) |
| 98 | `$prefixedTypeName` | 12 | YES (0.2s) | YES (0.5s) |
| 99 | `$assignop_as_binop` | 13 | YES (0.2s) | YES (0.4s) |
| 100 | `$callableId_IR` | 13 | YES (43.9s) | YES (0.5s) |
| 101 | `$flatten_nameList` | 13 | YES (0.4s) | YES (0.4s) |
| 102 | `$flatten_typeParameterList` | 13 | YES (0.4s) | YES (0.5s) |
| 103 | `$join_text` | 13 | YES (0.3s) | YES (0.5s) |
| 104 | `$callableId_of_externConstructorPrototypeIR` | 14 | YES (89.0s) | YES (0.5s) |
| 105 | `$callableId_of_externMethodPrototypeIR` | 15 | YES (140.9s) | YES (0.5s) |
| 106 | `$flatten_typeParameterListOpt` | 15 | YES (0.3s) | YES (0.4s) |
| 107 | `$is_tableDefaultActionProperty` | 16 | YES (0.5s) | YES (0.5s) |
| 108 | `$prefixedNonTypeName` | 19 | YES (0.4s) | YES (0.5s) |
| 109 | `$optional_annotation_of_parameterIR_prime_prime` | 20 | YES (0.6s) | YES (0.5s) |
| 110 | `$lvalue_as_expression` | 22 | YES (0.7s) | YES (0.5s) |
| 111 | `$starts_with` | 50 | YES (3.7s) | YES (0.5s) |
| 112 | `$strip_prefix_rec` | 53 | YES (2.1s) | YES (0.5s) |
| 113 | `$isValid_header` | 80 | YES (7.8s) | YES (0.5s) |
| 114 | `$invalidate_headerUnion` | 87 | YES (7.8s) | YES (0.7s) |
| 115 | `$invalidate_value` | 87 | YES (8.0s) | YES (0.7s) |
| 116 | `$ends_with` | 88 | YES (6.1s) | YES (0.6s) |
| 117 | `$strip_suffix_rec` | 91 | YES (7.2s) | YES (0.5s) |
| 118 | `$write_bits_from_value` | 103 | TIMEOUT (>2040s) | YES (6.0s) |
| 119 | `$bin_mod` | 109 | YES (7.6s) | YES (0.6s) |
| 120 | `$bin_div` | 113 | YES (9.9s) | YES (0.6s) |
| 121 | `$un_bnot` | 139 | YES (19.4s) | YES (5.5s) |
| 122 | `$bin_ge` | 183 | YES (44.7s) | YES (5.6s) |
| 123 | `$bin_le` | 183 | YES (45.6s) | YES (5.6s) |
| 124 | `$bin_gt` | 184 | YES (51.4s) | YES (5.6s) |
| 125 | `$bin_lt` | 184 | YES (55.6s) | YES (5.6s) |
| 126 | `$int_of_integerValue` | 184 | YES (59.1s) | YES (5.6s) |
| 127 | `$nat_of_integerValue` | 187 | YES (67.8s) | YES (5.6s) |
| 128 | `$bin_minus` | 193 | YES (79.3s) | YES (5.6s) |
| 129 | `$bin_mul` | 193 | YES (84.0s) | YES (5.7s) |
| 130 | `$bin_plus` | 193 | YES (88.3s) | YES (5.6s) |
| 131 | `$un_minus` | 197 | YES (95.3s) | YES (5.8s) |
| 132 | `$bin_bxor` | 199 | YES (107.3s) | YES (5.6s) |
| 133 | `$bin_concat` | 200 | YES (555.6s) | YES (5.6s) |
| 134 | `$set_priorities_of_tableEntryListIR_prime` | 200 | YES (44.0s) | YES (5.7s) |
| 135 | `$bin_satminus` | 201 | YES (45.4s) | YES (5.8s) |
| 136 | `$bin_satplus` | 201 | YES (113.7s) | YES (5.8s) |
| 137 | `$bin_shl` | 201 | TIMEOUT (>4080s) | YES (5.6s) |
| 138 | `$bin_band` | 202 | YES (142.9s) | YES (5.7s) |
| 139 | `$bin_bor` | 202 | YES (1792.7s) | YES (5.8s) |
| 140 | `$name_annotationToken` | 209 | YES (54.6s) | YES (6.0s) |
| 141 | `$un_op` | 209 | YES (48.4s) | YES (5.6s) |
| 142 | `$bin_shr` | 215 | TIMEOUT (>4080s) | YES (5.8s) |
| 143 | `$set_priorities_of_tableEntryListIR` | 226 | YES (64.5s) | YES (5.7s) |
| 144 | `$name_annotation_opt` | 256 | YES (151.7s) | YES (6.0s) |
| 145 | `$write_value_field_from_bits_prime` | 271 | YES* (261.7s) | YES (5.8s) |
| 146 | `$write_value_fields_from_bits_prime` | 271 | YES* (263.8s) | YES (5.8s) |
| 147 | `$write_value_from_bits_prime` | 271 | YES* (263.5s) | YES (5.7s) |
| 148 | `$write_values_from_bits_prime` | 271 | YES* (266.0s) | YES (5.8s) |
| 149 | `$write_value_from_bits` | 274 | YES* (278.1s) | YES (5.8s) |
| 150 | `$bitacc_range_op` | 283 | TIMEOUT (>4086s) | YES (21.5s) |
| 151 | `$bitacc_offset_op` | 285 | TIMEOUT (>4090s) | YES (21.3s) |
| 152 | `$bitacc_range_replace_op` | 391 | TIMEOUT (>4080s) | YES (5.6s) |
| 153 | `$bitacc_offset_replace_op` | 394 | TIMEOUT (>4080s) | YES (5.7s) |
| 154 | `$flatten_namedExpressionList` | 748 | YES (1422.3s) | YES (1.5s) |
| 155 | `$flatten_realTypeArgumentList` | 748 | YES (1450.1s) | YES (1.5s) |
| 156 | `$flatten_expressionList` | 749 | YES (1421.8s) | YES (1.5s) |
| 157 | `$flatten_typeArgumentList` | 749 | YES (1455.6s) | YES (1.5s) |
| 158 | `$expression_as_lvalue` | 766 | YES* (3198.2s) | YES (4.2s) |
| 159 | `$flatten_argumentList` | 784 | - | YES (6.1s) |
| 160 | `$flatten_simpleKeysetExpressionList` | 789 | YES (1611.5s) | YES (1.5s) |
| 161 | `$flatten_forUpdateStatementList` | 790 | YES (1728.2s) | YES (2.3s) |
| 162 | `$is_singleton_list_expression` | 812 | YES (1687.5s) | YES (1.7s) |
| 163 | `$add_annotationList` | 867 | YES (2023.9s) | YES (1.8s) |
| 164 | `$flatten_annotationList` | 868 | YES (1976.0s) | YES (2.5s) |
| 165 | `$flatten_parameterList` | 874 | TIMEOUT (>2055s) | YES (2.4s) |
| 166 | `$flatten_constructorParameterListOpt` | 876 | TIMEOUT (>2058s) | YES (2.5s) |
| 167 | `$is_externConstructorPrototype` | 880 | TIMEOUT (>2057s) | YES (1.8s) |
| 168 | `$is_externMethodPrototype` | 883 | TIMEOUT (>2057s) | YES (1.8s) |
| 169 | `$callableId_prime` | 884 | TIMEOUT (>2058s) | YES (2.5s) |
| 170 | `$callableId` | 885 | TIMEOUT (>2058s) | YES (2.6s) |
| 171 | `$constructorId_of_externConstructorPrototype` | 886 | TIMEOUT (>2059s) | YES (2.5s) |
| 172 | `$callableId_of_externMethodPrototype` | 887 | TIMEOUT (>2059s) | YES (2.5s) |
| 173 | `$constructorId` | 887 | TIMEOUT (>2059s) | YES (2.5s) |
| 174 | `$expressionNonBrace_as_expression` | 887 | TIMEOUT (>2063s) | YES (2.1s) |
| 175 | `$optional_annotation_of_parameterIR_prime` | 893 | TIMEOUT (>2058s) | YES (2.3s) |
| 176 | `$optional_annotation_of_parameterIR` | 895 | TIMEOUT (>2058s) | YES (2.3s) |
| 177 | `$is_optional_parameterIR` | 896 | TIMEOUT (>2058s) | YES (2.5s) |
| 178 | `$flatten_forInitStatementList` | 907 | TIMEOUT (>2062s) | YES (2.6s) |
| 179 | `$split_externConstructorOrMethodPrototypeList` | 940 | TIMEOUT (>4142s) | YES (2.2s) |
| 180 | `$flatten_parserStateList` | 1029 | TIMEOUT (>2085s) | YES (0.6s) |
| 181 | `$name_annotation` | 1125 | TIMEOUT (>4273s) | YES (6.1s) |
| 182 | `$name_annotation_default` | 1127 | TIMEOUT (>4249s) | YES (6.2s) |
| 183 | `$cast_header_stack` | 1192 | TIMEOUT (>2141s) | YES (0.6s) |
| 184 | `$cast_header` | 1194 | TIMEOUT (>2140s) | YES (0.6s) |
| 185 | `$cast_struct` | 1194 | TIMEOUT (>2141s) | YES (0.6s) |
| 186 | `$compat_lnot` | 1194 | TIMEOUT (>2141s) | YES (5.8s) |
| 187 | `$nestable_constructor_package` | 1194 | TIMEOUT (>2141s) | YES (5.7s) |
| 188 | `$resolve_type_alias` | 1194 | TIMEOUT (>2141s) | YES (5.7s) |
| 189 | `$callTargetKey_prime` | 1195 | TIMEOUT (>2141s) | YES (0.6s) |
| 190 | `$compat_bnot` | 1195 | TIMEOUT (>2141s) | YES (5.7s) |
| 191 | `$compat_divmod` | 1195 | TIMEOUT (>2142s) | YES (5.7s) |
| 192 | `$compat_logical` | 1195 | TIMEOUT (>2142s) | YES (5.7s) |
| 193 | `$cast_bool` | 1196 | TIMEOUT (>2142s) | YES (5.8s) |
| 194 | `$compat_array_index` | 1196 | TIMEOUT (>2141s) | YES (5.7s) |
| 195 | `$compat_bitslice_offset_index` | 1196 | TIMEOUT (>2141s) | YES (5.7s) |
| 196 | `$compat_bitslice_offset_width` | 1196 | TIMEOUT (>2141s) | YES (5.6s) |
| 197 | `$compat_bitslice_range_index` | 1196 | TIMEOUT (>2141s) | YES (5.7s) |
| 198 | `$compat_uplusminus` | 1196 | TIMEOUT (>2141s) | YES (5.6s) |
| 199 | `$nestable_constructor_control` | 1196 | TIMEOUT (>2141s) | YES (5.7s) |
| 200 | `$nestable_constructor_parser` | 1196 | TIMEOUT (>2141s) | YES (5.7s) |
| 201 | `$nestable_controlApplyMethod` | 1196 | TIMEOUT (>2141s) | YES (5.7s) |
| 202 | `$nestable_headerStack` | 1196 | TIMEOUT (>2141s) | YES (5.7s) |
| 203 | `$nestable_headerUnion` | 1196 | TIMEOUT (>2140s) | YES (5.6s) |
| 204 | `$definable_constructor` | 1197 | TIMEOUT (>2141s) | YES (5.8s) |
| 205 | `$nestable_constructor_extern` | 1197 | TIMEOUT (>2141s) | YES (5.6s) |
| 206 | `$nestable_externFunction` | 1197 | TIMEOUT (>2141s) | YES (5.8s) |
| 207 | `$nestable_externMethod` | 1197 | TIMEOUT (>2140s) | YES (5.8s) |
| 208 | `$nestable_new_in_enum_serializable` | 1197 | TIMEOUT (>2141s) | YES (5.6s) |
| 209 | `$nestable_parserApplyMethod` | 1197 | TIMEOUT (>2141s) | YES (5.6s) |
| 210 | `$compat_switch` | 1198 | TIMEOUT (>2141s) | YES (5.8s) |
| 211 | `$compat_table_lpm_ternary_range_key` | 1198 | TIMEOUT (>2140s) | YES (5.6s) |
| 212 | `$nestable_new` | 1198 | TIMEOUT (>2141s) | YES (5.7s) |
| 213 | `$parameterListIR_of_functionTypeDefIR` | 1198 | TIMEOUT (>2157s) | YES (0.6s) |
| 214 | `$typedExpressionIR_as_typedLvalueIR` | 1198 | TIMEOUT (>4329s) | YES (0.6s) |
| 215 | `$compat_concat` | 1199 | TIMEOUT (>2141s) | YES (5.7s) |
| 216 | `$callTargetKey` | 1200 | TIMEOUT (>2141s) | YES (0.6s) |
| 217 | `$compat_table_exact_optional_key` | 1202 | TIMEOUT (>2141s) | YES (5.7s) |
| 218 | `$callableTypeIR_of_callableTypeDefIR` | 1203 | TIMEOUT (>2154s) | YES (0.6s) |
| 219 | `$compat_shift` | 1203 | TIMEOUT (>2142s) | YES (5.7s) |
| 220 | `$nestable_enum_serializable` | 1203 | TIMEOUT (>2141s) | YES (5.7s) |
| 221 | `$typeParameterListIR_of_callableTypeDefIR` | 1203 | TIMEOUT (>2155s) | YES (0.6s) |
| 222 | `$flatten_keysetExpressionIR` | 1206 | TIMEOUT (>2162s) | YES (0.6s) |
| 223 | `$is_static_assert_callableTypeIR` | 1206 | TIMEOUT (>2158s) | YES (0.6s) |
| 224 | `$nestable_tuple_in_set` | 1206 | TIMEOUT (>2145s) | YES (5.6s) |
| 225 | `$parameterListIR_of_methodTypeDefIR` | 1206 | TIMEOUT (>2161s) | YES (0.6s) |
| 226 | `$typeId_of_typeDefIR` | 1207 | TIMEOUT (>2160s) | YES (0.6s) |
| 227 | `$typeParameterListIR_of_typeDefIR` | 1207 | TIMEOUT (>2161s) | YES (0.6s) |
| 228 | `$nestable_sequence_in_set` | 1208 | TIMEOUT (>2165s) | YES (5.7s) |
| 229 | `$nestable_struct_in_header` | 1208 | TIMEOUT (>2224s) | YES (5.8s) |
| 230 | `$nestable_tuple` | 1208 | TIMEOUT (>2232s) | YES (5.7s) |
| 231 | `$nestable_struct` | 1209 | TIMEOUT (>2221s) | YES (5.8s) |
| 232 | `$nestable_definedFunction` | 1211 | TIMEOUT (>2227s) | YES (5.8s) |
| 233 | `$nestable_action` | 1212 | TIMEOUT (>2224s) | YES (5.7s) |
| 234 | `$nestable_list` | 1212 | TIMEOUT (>2200s) | YES (5.7s) |
| 235 | `$is_equalable_typeIR` | 1213 | TIMEOUT (>2202s) | YES (5.9s) |
| 236 | `$typeIR_of_typeDefIR` | 1213 | TIMEOUT (>2189s) | YES (0.6s) |
| 237 | `$is_assignable_typeIR` | 1214 | TIMEOUT (>2164s) | YES (5.7s) |
| 238 | `$nestable_typedef` | 1215 | TIMEOUT (>2164s) | YES (5.8s) |
| 239 | `$init_tableKeys` | 1216 | TIMEOUT (>4322s) | YES (1.4s) |
| 240 | `$compat_bitslice_base` | 1218 | TIMEOUT (>2147s) | YES (5.7s) |
| 241 | `$nestable_header` | 1218 | TIMEOUT (>2164s) | YES (5.7s) |
| 242 | `$is_defaultable_typeIR` | 1219 | TIMEOUT (>2162s) | YES (5.7s) |
| 243 | `$parameterListIR_of_callableTypeDefIR` | 1221 | TIMEOUT (>2187s) | YES (0.6s) |
| 244 | `$unroll_typeIR` | 1229 | TIMEOUT (>2164s) | YES (5.8s) |
| 245 | `$is_table_application` | 1231 | TIMEOUT (>2164s) | YES (5.8s) |
| 246 | `$nestable_set` | 1235 | TIMEOUT (>2176s) | YES (5.9s) |
| 247 | `$sizeof_minSizeInBits_prime` | 1259 | TIMEOUT (>2190s) | YES (5.7s) |
| 248 | `$sizeof_minSizeInBits` | 1260 | TIMEOUT (>2189s) | YES (5.7s) |
| 249 | `$unroll_aliasType` | 1268 | TIMEOUT (>2176s) | YES (5.8s) |
| 250 | `$result_concat` | 1269 | TIMEOUT (>2184s) | YES (5.6s) |
| 251 | `$find_local_return_type_t` | 1270 | TIMEOUT (>4380s) | YES (0.6s) |
| 252 | `$is_concrete_extern_object_prime` | 1278 | TIMEOUT (>2202s) | YES (0.6s) |
| 253 | `$sizeof_maxSizeInBits_prime` | 1283 | TIMEOUT (>2213s) | YES (5.7s) |
| 254 | `$sizeof_maxSizeInBits` | 1284 | TIMEOUT (>2216s) | YES (5.7s) |
| 255 | `$is_monomorphic_typeDefIR` | 1289 | TIMEOUT (>2224s) | YES (0.6s) |
| 256 | `$is_polymorphic_typeDefIR` | 1292 | TIMEOUT (>2227s) | YES (0.6s) |
| 257 | `$resolve_inference_prime` | 1301 | TIMEOUT (>4416s) | YES (5.8s) |
| 258 | `$parameterListIR_of_functionDef` | 1304 | TIMEOUT (>2263s) | YES (0.6s) |
| 259 | `$resolve_inference` | 1306 | TIMEOUT (>4418s) | YES (5.9s) |
| 260 | `$reduce_serenum` | 1310 | TIMEOUT (>4450s) | YES (5.9s) |
| 261 | `$is_concrete_extern_object` | 1317 | TIMEOUT (>2225s) | YES (5.7s) |
| 262 | `$update_mode_tbl` | 1337 | TIMEOUT (>4499s) | YES (5.7s) |
| 263 | `$sizeof_minSizeInBytes` | 1347 | TIMEOUT (>2276s) | YES (5.7s) |
| 264 | `$sizeof_maxSizeInBytes` | 1350 | TIMEOUT (>2270s) | YES (5.7s) |
| 265 | `$sizeof` | 1375 | TIMEOUT (>2299s) | YES (5.8s) |
| 266 | `$init_tableEntries` | 1406 | TIMEOUT (>4520s) | YES (1.4s) |
| 267 | `$is_valid_bitslice` | 1431 | TIMEOUT (>2284s) | YES (6.0s) |
| 268 | `$init_table` | 1462 | TIMEOUT (>4634s) | YES (1.3s) |
| 269 | `$parameterListIR_of_methodDef` | 1551 | TIMEOUT (>2442s) | YES (0.7s) |
| 270 | `$parameterListIR_of_callableDef` | 1565 | TIMEOUT (>2477s) | YES (0.7s) |
| 271 | `$parameterListIR_of_constructorDef` | 1570 | TIMEOUT (>2469s) | YES (0.6s) |
| 272 | `$subexpressions_of_argumentIR` | 1614 | TIMEOUT (>2682s) | YES (5.7s) |
| 273 | `$subexpressions_of_argumentListIR` | 1614 | TIMEOUT (>2684s) | YES (5.7s) |
| 274 | `$subexpressions_of_expressionIR` | 1614 | TIMEOUT (>2676s) | YES (5.7s) |
| 275 | `$subexpressions_of_typedExpressionIR` | 1614 | TIMEOUT (>2677s) | YES (5.7s) |
| 276 | `$subexpressions_of_typedExpressionListIR` | 1614 | TIMEOUT (>2679s) | YES (5.7s) |
| 277 | `$name_expression` | 1840 | TIMEOUT (>2494s) | YES (6.1s) |
| 278 | `ParameterType_alpha` | 2489 | TIMEOUT (>4199s) | YES (6.1s) |
| 279 | `ExternMethodType_alpha` | 2490 | TIMEOUT (>4219s) | YES (6.1s) |
| 280 | `Type_alpha` | 2572 | - | YES (6.2s) |
| 281 | `$check_switchLabel_default` | 50638 | - | YES (285.4s) |
| 282 | `$find_action_prime` | 50638 | - | YES (330.5s) |
| 283 | `$update_fieldValue` | 50638 | - | YES (330.3s) |
| 284 | `$add_store` | 50640 | - | YES (329.3s) |
| 285 | `$callable_controlApplyMethod` | 50640 | - | YES (170.4s) |
| 286 | `$callable_parserApplyMethod` | 50640 | - | YES (183.6s) |
| 287 | `$find_action` | 50640 | - | YES (363.7s) |
| 288 | `$in_set` | 50640 | - | YES (170.0s) |
| 289 | `$instantiable_package` | 50640 | - | YES (171.0s) |
| 290 | `$instantiable_table` | 50640 | - | YES (169.8s) |
| 291 | `$callable_action` | 50642 | - | YES (175.5s) |
| 292 | `$callable_externAbstractMethod` | 50642 | - | YES (166.3s) |
| 293 | `$callable_externMethod` | 50642 | - | YES (168.9s) |
| 294 | `$callable_tableApplyMethod` | 50642 | - | YES (168.3s) |
| 295 | `$find_non_overloadeds` | 50642 | - | YES (292.1s) |
| 296 | `$find_store` | 50642 | - | YES (329.8s) |
| 297 | `$split_dataplane_parameters` | 50642 | - | YES (329.7s) |
| 298 | `$directionless_trailing_prime` | 50643 | - | YES (351.9s) |
| 299 | `$find_typeDef_i` | 50643 | - | YES (1289.6s) |
| 300 | `$partition_parameterListIR` | 50643 | - | YES (159.3s) |
| 301 | `$add_constructorDef_i` | 50644 | - | YES (330.3s) |
| 302 | `$add_typeDef_i` | 50644 | - | YES (330.6s) |
| 303 | `$instantiable_control` | 50644 | - | YES (162.7s) |
| 304 | `$instantiable_parser` | 50644 | - | YES (160.7s) |
| 305 | `$add_constructorDefs_i` | 50646 | - | YES (330.4s) |
| 306 | `$merge_frames` | 50646 | - | YES (332.5s) |
| 307 | `$callable_externFunction` | 50647 | - | - |
| 308 | `$directionless_trailing` | 50647 | - | - |
| 309 | `$add_type_i` | 50649 | - | - |
| 310 | `$extend_map` | 50650 | - | - |
| 311 | `$add_types_i` | 50651 | - | - |
| 312 | `$joins_ctk` | 50652 | - | - |
| 313 | `$add_constructorDef_t` | 50654 | - | - |
| 314 | `$add_parserState_i` | 50654 | - | - |
| 315 | `$add_constructorDefs_t` | 50656 | - | - |
| 316 | `$bound` | 50657 | - | - |
| 317 | `$find_non_overloaded` | 50657 | - | - |
| 318 | `$find_typeDef_t` | 50657 | - | - |
| 319 | `$find_var_i` | 50657 | - | - |
| 320 | `$find_var_t` | 50657 | - | - |
| 321 | `$add_constructorDef_non_overload_t` | 50658 | - | - |
| 322 | `$find_var_value_t` | 50658 | - | - |
| 323 | `$add_callableDef_overload_i` | 50659 | - | - |
| 324 | `$add_callableDef_overload_t` | 50659 | - | - |
| 325 | `$add_callableDef_non_overload_i` | 50663 | - | - |
| 326 | `$add_callableDef_non_overload_t` | 50663 | - | - |
| 327 | `$add_typeDef_t` | 50664 | - | - |
| 328 | `$add_typeDefs_t` | 50666 | - | - |
| 329 | `$add_var_i` | 50666 | - | - |
| 330 | `ConstDecl_inst` | 50667 | - | - |
| 331 | `$add_vars_i` | 50668 | - | - |
| 332 | `$find_callableDef_non_overloaded_t` | 50668 | - | - |
| 333 | `$add_typeParameters_t` | 50669 | - | - |
| 334 | `$find_callableDef_non_overload_i` | 50669 | - | - |
| 335 | `FuncDecl_inst` | 50675 | - | - |
| 336 | `$init` | 50676 | - | - |
| 337 | `ExternMethod_inst` | 50676 | - | - |
| 338 | `ActionDecl_inst` | 50677 | - | - |
| 339 | `$repeat` | 50678 | - | - |
| 340 | `ExternMethods_inst` | 50678 | - | - |
| 341 | `$fresh_typeIds` | 50679 | - | - |
| 342 | `SwitchLabel_table_ok` | 50697 | - | - |
| 343 | `TypeParameterListOpt_ok` | 50697 | - | - |
| 344 | `$contains_prime` | 50699 | - | - |
| 345 | `$contains` | 50702 | - | - |
| 346 | `$replace_text_except_prime` | 50703 | - | - |
| 347 | `$replace_text_prime` | 50703 | - | - |
| 348 | `$replace_text` | 50706 | - | - |
| 349 | `$replace_text_except` | 50706 | - | - |
| 350 | `$modulo` | 50724 | - | - |
| 351 | `$modulo_42` | 50727 | - | - |
| 352 | `$update_headerUnion` | 50745 | - | - |
| 353 | `$bin_eq` | 50749 | - | - |
| 354 | `$bin_ne` | 50752 | - | - |
| 355 | `$tableEntry_lpm_prefix_prime` | 50765 | - | - |
| 356 | `$tableEntry_lpm_prefix` | 50766 | - | - |
| 357 | `$bin_op` | 51056 | - | - |
| 358 | `$find_name_annotation_opt` | 51529 | - | - |
| 359 | `$compat_bitwise` | 51829 | - | - |
| 360 | `$compat_satplusminus` | 51829 | - | - |
| 361 | `$compat_compare` | 51830 | - | - |
| 362 | `$compat_mask` | 51830 | - | - |
| 363 | `$compat_plusminusmult` | 51830 | - | - |
| 364 | `$compat_range` | 51830 | - | - |
| 365 | `$callable_definedFunction` | 51844 | - | - |
| 366 | `$compat_table_key` | 51847 | - | - |
| 367 | `$align_parameterListIR_given` | 51849 | - | - |
| 368 | `$is_static_callableTypeIR` | 51856 | - | - |
| 369 | `$insert_NoAction_tablePropertyIR` | 51862 | - | - |
| 370 | `$align_parameterListIR` | 51870 | - | - |
| 371 | `$add_var_t` | 51929 | - | - |
| 372 | `$add_constructorParameter_t` | 51930 | - | - |
| 373 | `$add_parameter_t` | 51931 | - | - |
| 374 | `$add_vars_t` | 51931 | - | - |
| 375 | `$add_constructorParameters_t` | 51932 | - | - |
| 376 | `$free_callableTypeDefIR` | 51932 | - | - |
| 377 | `$free_callableTypeIR` | 51932 | - | - |
| 378 | `$free_parameterIR` | 51932 | - | - |
| 379 | `$free_typeIR` | 51932 | - | - |
| 380 | `Expr_lvalue_ok` | 51933 | - | - |
| 381 | `$merge_externMethodDefEnvs` | 51950 | - | - |
| 382 | `TableType_ok` | 51959 | - | - |
| 383 | `$free_typeDefIR` | 51963 | - | - |
| 384 | `$find_constructorDef_overloaded_t` | 52020 | - | - |
| 385 | `$capture_avoiding` | 52021 | - | - |
| 386 | `$capture_avoiding` | 52021 | - | - |
| 387 | `$find_callableDef_overloaded_t` | 52056 | - | - |
| 388 | `$instantiable` | 52065 | - | - |
| 389 | `$subst_callableTypeDefIR` | 52157 | - | - |
| 390 | `$subst_callableTypeDefIR_prime` | 52157 | - | - |
| 391 | `$subst_callableTypeIR_prime` | 52157 | - | - |
| 392 | `$subst_parameterIR` | 52157 | - | - |
| 393 | `$subst_parameterIR_prime` | 52157 | - | - |
| 394 | `$subst_typeIR` | 52157 | - | - |
| 395 | `$subst_typeIR_prime` | 52157 | - | - |
| 396 | `$subst_callableTypeIR` | 52159 | - | - |
| 397 | `$subst_constructorTypeIR_prime` | 52160 | - | - |
| 398 | `$subst_constructorTypeIR` | 52162 | - | - |
| 399 | `$subst_type_i` | 52173 | - | - |
| 400 | `$specialize_typeDefIR` | 52190 | - | - |
| 401 | `$specialize_callableTypeDefIR` | 52193 | - | - |
| 402 | `$specialize_constructorTypeDefIR` | 52196 | - | - |
| 403 | `TableAction_inst` | 52251 | - | - |
| 404 | `TableActions_inst` | 52253 | - | - |
| 405 | `$cast_default` | 52307 | - | - |
| 406 | `$cast_enum` | 52307 | - | - |
| 407 | `$cast_error` | 52307 | - | - |
| 408 | `$cast_int` | 52307 | - | - |
| 409 | `$cast_invalid_header` | 52307 | - | - |
| 410 | `$cast_list` | 52307 | - | - |
| 411 | `$cast_op` | 52307 | - | - |
| 412 | `$cast_record` | 52307 | - | - |
| 413 | `$cast_sequence` | 52307 | - | - |
| 414 | `$cast_set` | 52307 | - | - |
| 415 | `$cast_to_enum` | 52307 | - | - |
| 416 | `$cast_to_enum_prime` | 52307 | - | - |
| 417 | `$default` | 52307 | - | - |
| 418 | `CallableTypeDef_wf` | 52394 | - | - |
| 419 | `CallableType_wf` | 52394 | - | - |
| 420 | `ParameterType_wf` | 52394 | - | - |
| 421 | `ReturnType_wf` | 52394 | - | - |
| 422 | `Type_wf` | 52394 | - | - |
| 423 | `ConstructorParameterType_wf` | 52395 | - | - |
| 424 | `$find_constructorDef_i` | 52398 | - | - |
| 425 | `TypeDef_wf` | 52425 | - | - |
| 426 | `Constructor_inst` | 52454 | - | - |
| 427 | `ConstructorType_wf` | 52606 | - | - |
| 428 | `ConstructorTypeDef_wf` | 52613 | - | - |
| 429 | `ConstructorType_ok` | 53118 | - | - |
| 430 | `CallableType_ok` | 53229 | - | - |
| 431 | `Cast_impl` | 53277 | - | - |
| 432 | `$cast_unary` | 53280 | - | - |
| 433 | `$merge_constraint_prime` | 53290 | - | - |
| 434 | `$merge_constraint` | 53296 | - | - |
| 435 | `$merge_constraints` | 53298 | - | - |
| 436 | `$cast_binary` | 53328 | - | - |
| 437 | `Cast_impl_neq` | 53329 | - | - |
| 438 | `Call_convention_expr_ok` | 53350 | - | - |
| 439 | `Cast_expl` | 53353 | - | - |
| 440 | `Cast_expl_neq` | 53353 | - | - |
| 441 | `Call_convention_argument_ok` | 53358 | - | - |
| 442 | `Call_convention_ok` | 53360 | - | - |
| 443 | `$gen_constraint` | 53389 | - | - |
| 444 | `$gen_constraints` | 53389 | - | - |
| 445 | `$infer_prime` | 53394 | - | - |
| 446 | `$infer` | 53454 | - | - |
| 447 | `ExternMethodTypeDef_alpha` | 53469 | - | - |
| 448 | `$subst_externMethodTypeDefEnv` | 53493 | - | - |
| 449 | `Call_action_default_ok` | 53784 | - | - |
| 450 | `Call_action_partial_ok` | 53784 | - | - |
| 451 | `Argument_eval_lctk` | 54289 | - | - |
| 452 | `Expr_eval_lctk` | 54289 | - | - |
| 453 | `Argument_inst` | 54339 | - | - |
| 454 | `BlockElementStmtList_inst` | 54339 | - | - |
| 455 | `BlockElementStmt_inst` | 54339 | - | - |
| 456 | `Block_inst` | 54339 | - | - |
| 457 | `Constructor_call` | 54339 | - | - |
| 458 | `ControlLocalDecl_inst` | 54339 | - | - |
| 459 | `ControlLocalDecls_inst` | 54339 | - | - |
| 460 | `Copy_in_argument_inst` | 54339 | - | - |
| 461 | `Copy_in_argument_inst_default` | 54339 | - | - |
| 462 | `Copy_in_inst` | 54339 | - | - |
| 463 | `Copy_in_inst_default` | 54339 | - | - |
| 464 | `DirectApplicationStmt_inst` | 54339 | - | - |
| 465 | `Expr_inst` | 54339 | - | - |
| 466 | `Exprs_inst` | 54339 | - | - |
| 467 | `InstDecl_inst` | 54339 | - | - |
| 468 | `ObjectDecl_inst` | 54339 | - | - |
| 469 | `ObjectDecls_inst` | 54339 | - | - |
| 470 | `ParserLocalDecl_inst` | 54339 | - | - |
| 471 | `ParserLocalDecls_inst` | 54339 | - | - |
| 472 | `ParserState_inst` | 54339 | - | - |
| 473 | `ParserStates_inst` | 54339 | - | - |
| 474 | `ParserStmt_inst` | 54339 | - | - |
| 475 | `ParserStmts_inst` | 54339 | - | - |
| 476 | `Stmt_inst` | 54339 | - | - |
| 477 | `SwitchCase_inst` | 54339 | - | - |
| 478 | `SwitchCases_inst` | 54339 | - | - |
| 479 | `TableProperties_inst` | 54339 | - | - |
| 480 | `TableProperty_inst` | 54339 | - | - |
| 481 | `Call_ok` | 54414 | - | - |
| 482 | `Decl_inst` | 54484 | - | - |
| 483 | `Decls_inst` | 54486 | - | - |
| 484 | `Inst_ok` | 54604 | - | - |
| 485 | `ArgumentList_ok` | 56634 | - | - |
| 486 | `Argument_ok` | 56634 | - | - |
| 487 | `CallableTarget_ok` | 56634 | - | - |
| 488 | `Expr_ok` | 56634 | - | - |
| 489 | `TypeArgumentList_ok` | 56634 | - | - |
| 490 | `TypeArgument_ok` | 56634 | - | - |
| 491 | `TypeArguments_ok` | 56634 | - | - |
| 492 | `Type_ok` | 56634 | - | - |
| 493 | `ConstDecl_ok` | 56635 | - | - |
| 494 | `Enum_serializable_field_ok` | 56635 | - | - |
| 495 | `InstDecl_non_objectInitializer_ok` | 56635 | - | - |
| 496 | `TableEntry_priority_ok` | 56636 | - | - |
| 497 | `Enum_serializable_fields_ok` | 56637 | - | - |
| 498 | `Enum_serializable_fieldList_ok` | 56638 | - | - |
| 499 | `Parameter_ok` | 56639 | - | - |
| 500 | `ParameterList_ok_inner` | 56641 | - | - |
| 501 | `ParameterList_ok` | 56653 | - | - |
| 502 | `ConstructorParameterListOpt_ok` | 56655 | - | - |
| 503 | `CallableTarget_lvalue_ok` | 56657 | - | - |
| 504 | `VarDecl_ok` | 56659 | - | - |
| 505 | `SelectCase_keyset_simple_ok` | 56661 | - | - |
| 506 | `ExternConstructor_ok` | 56676 | - | - |
| 507 | `ForCollectionExpr_ok` | 56677 | - | - |
| 508 | `ExternMethod_ok` | 56683 | - | - |
| 509 | `Lvalue_ok` | 56697 | - | - |
| 510 | `TableEntry_keyset_simple_ok` | 56741 | - | - |
| 511 | `TableEntry_keysets_simple_ok` | 56746 | - | - |
| 512 | `SwitchLabel_general_ok` | 56782 | - | - |
| 513 | `SelectCase_keyset_ok` | 56898 | - | - |
| 514 | `SelectCase_ok` | 56899 | - | - |
| 515 | `ParserSelect_ok` | 56912 | - | - |
| 516 | `ParserTransition_ok` | 56962 | - | - |
| 517 | `TableEntry_keyset_ok` | 56980 | - | - |
| 518 | `TableEntry_action_ok` | 57072 | - | - |
| 519 | `TableDefaultAction_ok` | 57087 | - | - |
| 520 | `TableAction_ok` | 57130 | - | - |
| 521 | `TableActions_ok` | 57132 | - | - |
| 522 | `TableEntry_ok` | 57424 | - | - |
| 523 | `BlockElementStmtList_ok` | 57471 | - | - |
| 524 | `BlockElementStmt_ok` | 57471 | - | - |
| 525 | `BlockElementStmts_ok` | 57471 | - | - |
| 526 | `Block_ok` | 57471 | - | - |
| 527 | `ForInitStmtList_ok` | 57471 | - | - |
| 528 | `ForInitStmt_ok` | 57471 | - | - |
| 529 | `ForInitStmts_ok` | 57471 | - | - |
| 530 | `ForUpdateStmtList_ok` | 57471 | - | - |
| 531 | `ForUpdateStmt_ok` | 57471 | - | - |
| 532 | `Stmt_ok` | 57471 | - | - |
| 533 | `SwitchCaseList_general_ok` | 57471 | - | - |
| 534 | `SwitchCaseList_table_ok` | 57471 | - | - |
| 535 | `SwitchCase_general_ok` | 57471 | - | - |
| 536 | `SwitchCase_table_ok` | 57471 | - | - |
| 537 | `SwitchCases_general_ok` | 57471 | - | - |
| 538 | `SwitchCases_table_ok` | 57471 | - | - |
| 539 | `ActionDecl_ok` | 57502 | - | - |
| 540 | `FuncDecl_ok` | 57525 | - | - |
| 541 | `ParserStmtList_ok` | 57561 | - | - |
| 542 | `ParserStmt_ok` | 57561 | - | - |
| 543 | `ParserStmts_ok` | 57561 | - | - |
| 544 | `InstDecl_objectInitializer_ok` | 57655 | - | - |
| 545 | `InstDecl_ok` | 57655 | - | - |
| 546 | `ObjectDeclList_ok` | 57655 | - | - |
| 547 | `ObjectDecl_ok` | 57655 | - | - |
| 548 | `ObjectDecls_ok` | 57655 | - | - |
| 549 | `ParserLocalDecl_ok` | 57675 | - | - |
| 550 | `ParserLocalDecls_ok` | 57677 | - | - |
| 551 | `ParserLocalDeclList_ok` | 57680 | - | - |
| 552 | `TableKey_ok` | 57770 | - | - |
| 553 | `TableKeys_ok` | 57772 | - | - |
| 554 | `ParserState_ok` | 57884 | - | - |
| 555 | `ParserStateList_ok` | 57903 | - | - |
| 556 | `TableProperty_ok` | 58824 | - | - |
| 557 | `TableProperties_ok` | 58826 | - | - |
| 558 | `Table_ok` | 58883 | - | - |
| 559 | `ControlLocalDecl_ok` | 60199 | - | - |
| 560 | `ControlLocalDecls_ok` | 60201 | - | - |
| 561 | `ControlLocalDeclList_ok` | 60204 | - | - |
| 562 | `Decl_ok` | 60717 | - | - |
| 563 | `Decls_ok` | 60719 | - | - |
| 564 | `Program_ok` | 60723 | - | YES (≥20480s) |
| 565 | `Program_inst` | 61345 | - | TIMEOUT (>20480s) |
