# P4 structural CTRS — 검증 결과 (표)

> **표만 둔다.** 각 셀은 `판정 (초s)` — 판정과 그 판정에 걸린 심볼당 **직렬 fresh** 벽시계다.
> 행은 슬라이스 규칙 수 오름차순. 종전의 ≤500 / >500 두 표는 **한 표로 합쳤다** — 밴드
> 경계는 측정 시기의 산물이지 성질의 구분이 아니었다. 열 이름은 이제 그 열을 만드는
> 서브커맨드 이름과 같다(`main.exe confluence` / `main.exe termination`).
> **confluence** = Church-Rosser(합류성): `YES` / `YES*`(=`--crc-normalize` upgrade-only로
> 닫힘) / `MAYBE` / `TIMEOUT (>1800s)`(예산 내 미완) / `-`(미측정). **ChC(Coherence)는
> 2026-07-24부로 측정 중단·열 삭제**(사유·이력은 notes). 규칙 수 748 이상 행의 confluence는
> 옛 bigsweep에서 옮긴 값이라 초가 없다.
> **termination** = 구조 보존 unravel → AProVE 직접. 그 초는 답을 낸 AProVE 실행 하나의
> 벽시계다. 예산 사다리(5·20·80·…·cap)를 올라가며 답이 나오는 최소 예산에서 멈추는데,
> **예산은 안 적는다** — AProVE가 마감 전에 답하는 경우가 많아 예산은 실제 시간과 무관한
> 천장이거나 초와 겹치므로, 초 하나가 유일한 정직한 심볼당 수치다.
> 규칙 0(정의만 있고 절이 없는) 심볼 9개는 슬라이스가 비어 CRC·termination 둘 다
> 자명하므로 표에서 뺐다.
> **292행부터는 사다리의 시작 rung을 올려 쟀다**(`--budget-from`, `3988f2ec`). 이 구간은
> 전부 170~360초에 답해서 5·20·80 rung이 헛돌기만 했다. 대신 「답하는 최소 예산」이라는
> 사다리의 보장이 약해진다 — AProVE는 증명을 찾은 시점이 아니라 **자기 마감에** 판정을
> 찍으므로, 마감에 걸린 행의 초는 비용이 아니라 rung 그 자체다. 이 표에서 그런 행은
> `$find_typeDef_i`(1289.6초) 하나뿐이며 그 값은 **상한**이다.
> **316·317행은 최상위 관계** `Program_ok`(67,634규칙)·`Program_inst`(69,126규칙)로,
> 표의 다른 어떤 슬라이스보다 크다. 사다리를 `[5120, 20480]`로 따로 걸어 쟀다. `orient_conds`
> 이전 `Program_ok`은 440.3초 YES였는데, 되살아난 절들이 AProVE 탐색을 폭발시켜 20,480초
> rung에서야 YES가 났다 — 그 초는 마감값이라 **상한**이고, `Program_inst`은 같은 예산 안에
> 못 끝내 TIMEOUT이다. 판정 회귀가 아니라 도구 비용의 폭발이며, 표의 나머지 315행은 재측정
> 후에도 모두 YES다.
>
> **범위**: 슬라이스 가능한 심볼 574개 중 규칙 0(정의만 있고 절이 없는) 9개를 뺀 565개가
> 대상이고 이 표는 그중 **308행**이다. 나머지는 미측정 —
> 스윕은 규칙 수 오름차순이라 남은 것은 전부 대형 슬라이스이고, 심볼당 수 분~수십 분이
> 든다(`$find_action_prime` 50,638규칙이 330.5초). 스윕은 답을 못 내는 심볼이 나오면
> 멈추도록 감시했는데, 여기까지 그런 심볼은 하나도 없었다.
>
> 재현 커맨드·방법론·측정 이력·비-YES 해석·병렬
> 안전성 소견은 모두 **[verification-notes.md](verification-notes.md)**.
>
> **측정 기준**: termination은 `bdceb303` (2026-07-24, `orient_conds` 이후 전수 재측정).
> confluence는 `orient_conds`(`bdceb303`)가 분석 표면의 조건 방향을 바꿔 `bff805ec`
> 측정이 stale해졌으므로 `c10bde20`에서 재측정 중이다(1차). **규칙 ≤748 구간 140심볼을
> 재측정했고 판정 변화 0** — 뒤집힌 조건을 포함했던 슬라이스도 전부 옛 YES를 유지했다.
> 아직 그 위(748 초과) 행은 `bff805ec` 값이고, 스윕은 tmux에서 규칙 오름차순으로 진행 중.

## 종합 (308심볼)

**termination**: YES 307 / 308 측정 · TIMEOUT 1 · 비종료 후보 0. TIMEOUT 1건은 최상위 관계 `Program_inst`(69,126규칙)로, 20,480초 예산 내 미완일 뿐 비종료 witness가 아니다(도구 예산 한계).
**confluence**: YES 162 · - 128 · TIMEOUT 12 · YES* 5 · MAYBE 1.

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
| 118 | `$write_bits_from_value` | 103 | TIMEOUT (>1800s) | YES (6.0s) |
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
| 134 | `$set_priorities_of_tableEntryListIR_prime` | 200 | YES (43.7s) | YES (5.7s) |
| 135 | `$bin_satminus` | 201 | YES (45.4s) | YES (5.8s) |
| 136 | `$bin_satplus` | 201 | YES (113.7s) | YES (5.8s) |
| 137 | `$bin_shl` | 201 | TIMEOUT (>1800s) | YES (5.6s) |
| 138 | `$bin_band` | 202 | YES (142.9s) | YES (5.7s) |
| 139 | `$bin_bor` | 202 | YES (1792.7s) | YES (5.8s) |
| 140 | `$name_annotationToken` | 209 | YES (53.3s) | YES (6.0s) |
| 141 | `$un_op` | 209 | YES (48.4s) | YES (5.6s) |
| 142 | `$bin_shr` | 215 | TIMEOUT (>1800s) | YES (5.8s) |
| 143 | `$set_priorities_of_tableEntryListIR` | 226 | YES (64.5s) | YES (5.7s) |
| 144 | `$name_annotation_opt` | 256 | YES (151.7s) | YES (6.0s) |
| 145 | `$write_value_field_from_bits_prime` | 271 | YES* (244.1s) | YES (5.8s) |
| 146 | `$write_value_fields_from_bits_prime` | 271 | YES* (243.9s) | YES (5.8s) |
| 147 | `$write_value_from_bits_prime` | 271 | YES* (244.0s) | YES (5.7s) |
| 148 | `$write_values_from_bits_prime` | 271 | YES* (242.8s) | YES (5.8s) |
| 149 | `$write_value_from_bits` | 274 | YES* (253.0s) | YES (5.8s) |
| 150 | `$bitacc_range_op` | 283 | TIMEOUT (>1800s) | YES (21.5s) |
| 151 | `$bitacc_offset_op` | 285 | TIMEOUT (>1800s) | YES (21.3s) |
| 152 | `$bitacc_range_replace_op` | 391 | TIMEOUT (>1800s) | YES (5.6s) |
| 153 | `$bitacc_offset_replace_op` | 394 | TIMEOUT (>1800s) | YES (5.7s) |
| 154 | `$flatten_namedExpressionList` | 748 | YES (1422.3s) | YES (1.5s) |
| 155 | `$flatten_realTypeArgumentList` | 748 | YES | YES (1.5s) |
| 156 | `$flatten_expressionList` | 749 | TIMEOUT | YES (1.5s) |
| 157 | `$flatten_typeArgumentList` | 749 | TIMEOUT | YES (1.5s) |
| 158 | `$expression_as_lvalue` | 766 | MAYBE | YES (4.2s) |
| 159 | `$flatten_argumentList` | 784 | TIMEOUT | YES (6.1s) |
| 160 | `$flatten_simpleKeysetExpressionList` | 789 | TIMEOUT | YES (1.5s) |
| 161 | `$flatten_forUpdateStatementList` | 790 | YES | YES (2.3s) |
| 162 | `$is_singleton_list_expression` | 812 | YES | YES (1.7s) |
| 163 | `$add_annotationList` | 867 | YES | YES (1.8s) |
| 164 | `$flatten_annotationList` | 868 | YES | YES (2.5s) |
| 165 | `$flatten_parameterList` | 874 | YES | YES (2.4s) |
| 166 | `$flatten_constructorParameterListOpt` | 876 | YES | YES (2.5s) |
| 167 | `$is_externConstructorPrototype` | 880 | YES | YES (1.8s) |
| 168 | `$is_externMethodPrototype` | 883 | YES | YES (1.8s) |
| 169 | `$callableId_prime` | 884 | YES | YES (2.5s) |
| 170 | `$callableId` | 885 | YES | YES (2.6s) |
| 171 | `$constructorId_of_externConstructorPrototype` | 886 | YES | YES (2.5s) |
| 172 | `$callableId_of_externMethodPrototype` | 887 | YES | YES (2.5s) |
| 173 | `$constructorId` | 887 | YES | YES (2.5s) |
| 174 | `$expressionNonBrace_as_expression` | 887 | TIMEOUT | YES (2.1s) |
| 175 | `$optional_annotation_of_parameterIR_prime` | 893 | YES | YES (2.3s) |
| 176 | `$optional_annotation_of_parameterIR` | 895 | YES | YES (2.3s) |
| 177 | `$is_optional_parameterIR` | 896 | YES | YES (2.5s) |
| 178 | `$flatten_forInitStatementList` | 907 | YES | YES (2.6s) |
| 179 | `$split_externConstructorOrMethodPrototypeList` | 940 | YES | YES (2.2s) |
| 180 | `$flatten_parserStateList` | 1029 | YES | YES (0.6s) |
| 181 | `$name_annotation` | 1125 | - | YES (6.1s) |
| 182 | `$name_annotation_default` | 1127 | - | YES (6.2s) |
| 183 | `$cast_header_stack` | 1192 | - | YES (0.6s) |
| 184 | `$cast_header` | 1194 | - | YES (0.6s) |
| 185 | `$cast_struct` | 1194 | - | YES (0.6s) |
| 186 | `$compat_lnot` | 1194 | - | YES (5.8s) |
| 187 | `$nestable_constructor_package` | 1194 | - | YES (5.7s) |
| 188 | `$resolve_type_alias` | 1194 | - | YES (5.7s) |
| 189 | `$callTargetKey_prime` | 1195 | - | YES (0.6s) |
| 190 | `$compat_bnot` | 1195 | - | YES (5.7s) |
| 191 | `$compat_divmod` | 1195 | - | YES (5.7s) |
| 192 | `$compat_logical` | 1195 | - | YES (5.7s) |
| 193 | `$cast_bool` | 1196 | - | YES (5.8s) |
| 194 | `$compat_array_index` | 1196 | - | YES (5.7s) |
| 195 | `$compat_bitslice_offset_index` | 1196 | - | YES (5.7s) |
| 196 | `$compat_bitslice_offset_width` | 1196 | - | YES (5.6s) |
| 197 | `$compat_bitslice_range_index` | 1196 | - | YES (5.7s) |
| 198 | `$compat_uplusminus` | 1196 | - | YES (5.6s) |
| 199 | `$nestable_constructor_control` | 1196 | - | YES (5.7s) |
| 200 | `$nestable_constructor_parser` | 1196 | - | YES (5.7s) |
| 201 | `$nestable_controlApplyMethod` | 1196 | - | YES (5.7s) |
| 202 | `$nestable_headerStack` | 1196 | - | YES (5.7s) |
| 203 | `$nestable_headerUnion` | 1196 | - | YES (5.6s) |
| 204 | `$definable_constructor` | 1197 | - | YES (5.8s) |
| 205 | `$nestable_constructor_extern` | 1197 | - | YES (5.6s) |
| 206 | `$nestable_externFunction` | 1197 | - | YES (5.8s) |
| 207 | `$nestable_externMethod` | 1197 | - | YES (5.8s) |
| 208 | `$nestable_new_in_enum_serializable` | 1197 | - | YES (5.6s) |
| 209 | `$nestable_parserApplyMethod` | 1197 | - | YES (5.6s) |
| 210 | `$compat_switch` | 1198 | - | YES (5.8s) |
| 211 | `$compat_table_lpm_ternary_range_key` | 1198 | - | YES (5.6s) |
| 212 | `$nestable_new` | 1198 | - | YES (5.7s) |
| 213 | `$parameterListIR_of_functionTypeDefIR` | 1198 | - | YES (0.6s) |
| 214 | `$typedExpressionIR_as_typedLvalueIR` | 1198 | - | YES (0.6s) |
| 215 | `$compat_concat` | 1199 | - | YES (5.7s) |
| 216 | `$callTargetKey` | 1200 | - | YES (0.6s) |
| 217 | `$compat_table_exact_optional_key` | 1202 | - | YES (5.7s) |
| 218 | `$callableTypeIR_of_callableTypeDefIR` | 1203 | - | YES (0.6s) |
| 219 | `$compat_shift` | 1203 | - | YES (5.7s) |
| 220 | `$nestable_enum_serializable` | 1203 | - | YES (5.7s) |
| 221 | `$typeParameterListIR_of_callableTypeDefIR` | 1203 | - | YES (0.6s) |
| 222 | `$flatten_keysetExpressionIR` | 1206 | - | YES (0.6s) |
| 223 | `$is_static_assert_callableTypeIR` | 1206 | - | YES (0.6s) |
| 224 | `$nestable_tuple_in_set` | 1206 | - | YES (5.6s) |
| 225 | `$parameterListIR_of_methodTypeDefIR` | 1206 | - | YES (0.6s) |
| 226 | `$typeId_of_typeDefIR` | 1207 | - | YES (0.6s) |
| 227 | `$typeParameterListIR_of_typeDefIR` | 1207 | - | YES (0.6s) |
| 228 | `$nestable_sequence_in_set` | 1208 | - | YES (5.7s) |
| 229 | `$nestable_struct_in_header` | 1208 | - | YES (5.8s) |
| 230 | `$nestable_tuple` | 1208 | - | YES (5.7s) |
| 231 | `$nestable_struct` | 1209 | - | YES (5.8s) |
| 232 | `$nestable_definedFunction` | 1211 | - | YES (5.8s) |
| 233 | `$nestable_action` | 1212 | - | YES (5.7s) |
| 234 | `$nestable_list` | 1212 | - | YES (5.7s) |
| 235 | `$is_equalable_typeIR` | 1213 | - | YES (5.9s) |
| 236 | `$typeIR_of_typeDefIR` | 1213 | - | YES (0.6s) |
| 237 | `$is_assignable_typeIR` | 1214 | - | YES (5.7s) |
| 238 | `$nestable_typedef` | 1215 | - | YES (5.8s) |
| 239 | `$init_tableKeys` | 1216 | - | YES (1.4s) |
| 240 | `$compat_bitslice_base` | 1218 | - | YES (5.7s) |
| 241 | `$nestable_header` | 1218 | - | YES (5.7s) |
| 242 | `$is_defaultable_typeIR` | 1219 | - | YES (5.7s) |
| 243 | `$parameterListIR_of_callableTypeDefIR` | 1221 | - | YES (0.6s) |
| 244 | `$unroll_typeIR` | 1229 | - | YES (5.8s) |
| 245 | `$is_table_application` | 1231 | - | YES (5.8s) |
| 246 | `$nestable_set` | 1235 | - | YES (5.9s) |
| 247 | `$sizeof_minSizeInBits_prime` | 1259 | - | YES (5.7s) |
| 248 | `$sizeof_minSizeInBits` | 1260 | - | YES (5.7s) |
| 249 | `$unroll_aliasType` | 1268 | - | YES (5.8s) |
| 250 | `$result_concat` | 1269 | - | YES (5.6s) |
| 251 | `$find_local_return_type_t` | 1270 | - | YES (0.6s) |
| 252 | `$is_concrete_extern_object_prime` | 1278 | - | YES (0.6s) |
| 253 | `$sizeof_maxSizeInBits_prime` | 1283 | - | YES (5.7s) |
| 254 | `$sizeof_maxSizeInBits` | 1284 | - | YES (5.7s) |
| 255 | `$is_monomorphic_typeDefIR` | 1289 | - | YES (0.6s) |
| 256 | `$is_polymorphic_typeDefIR` | 1292 | - | YES (0.6s) |
| 257 | `$resolve_inference_prime` | 1301 | - | YES (5.8s) |
| 258 | `$parameterListIR_of_functionDef` | 1304 | - | YES (0.6s) |
| 259 | `$resolve_inference` | 1306 | - | YES (5.9s) |
| 260 | `$reduce_serenum` | 1310 | - | YES (5.9s) |
| 261 | `$is_concrete_extern_object` | 1317 | - | YES (5.7s) |
| 262 | `$update_mode_tbl` | 1337 | - | YES (5.7s) |
| 263 | `$sizeof_minSizeInBytes` | 1347 | - | YES (5.7s) |
| 264 | `$sizeof_maxSizeInBytes` | 1350 | - | YES (5.7s) |
| 265 | `$sizeof` | 1375 | - | YES (5.8s) |
| 266 | `$init_tableEntries` | 1406 | - | YES (1.4s) |
| 267 | `$is_valid_bitslice` | 1431 | - | YES (6.0s) |
| 268 | `$init_table` | 1462 | - | YES (1.3s) |
| 269 | `$parameterListIR_of_methodDef` | 1551 | - | YES (0.7s) |
| 270 | `$parameterListIR_of_callableDef` | 1565 | - | YES (0.7s) |
| 271 | `$parameterListIR_of_constructorDef` | 1570 | - | YES (0.6s) |
| 272 | `$subexpressions_of_argumentIR` | 1614 | - | YES (5.7s) |
| 273 | `$subexpressions_of_argumentListIR` | 1614 | - | YES (5.7s) |
| 274 | `$subexpressions_of_expressionIR` | 1614 | - | YES (5.7s) |
| 275 | `$subexpressions_of_typedExpressionIR` | 1614 | - | YES (5.7s) |
| 276 | `$subexpressions_of_typedExpressionListIR` | 1614 | - | YES (5.7s) |
| 277 | `$name_expression` | 1840 | - | YES (6.1s) |
| 278 | `ParameterType_alpha` | 2489 | - | YES (6.1s) |
| 279 | `ExternMethodType_alpha` | 2490 | - | YES (6.1s) |
| 280 | `Type_alpha` | 2572 | - | YES (6.2s) |
| 281 | `$check_switchLabel_default` | 50638 | - | YES (285.4s) |
| 282 | `$find_action_prime` | 50638 | - | YES (330.5s) |
| 283 | `$update_fieldValue` | 50638 | - | YES (330.3s) |
| 284 | `$add_store` | 50640 | - | YES (329.3s) |
| 285 | `$callable_controlApplyMethod` | 50640 | - | YES (170.4s) |
| 286 | `$callable_parserApplyMethod` | 50640 | - | YES (183.6s) |
| 287 | `$in_set` | 50640 | - | YES (170.0s) |
| 288 | `$find_action` | 50640 | - | YES (363.7s) |
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
| 307 | `Program_ok` | 67634 | - | YES (≥20480s) |
| 308 | `Program_inst` | 69126 | - | TIMEOUT (>20480s) |
