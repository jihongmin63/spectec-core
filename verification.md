# P4 structural CTRS — 검증 결과 (표)

> **표만 둔다.** 각 셀은 `판정 (초s)` — 판정과 그 판정에 걸린 심볼당 **직렬 fresh** 벽시계다.
> **CRC** = Church-Rosser(합류성): `YES` / `YES*`(=`--crc-normalize` upgrade-only로 닫힘) /
> `MAYBE` / `TIMEOUT (>1800s)`(예산 내 미완) / `-`(미측정). **ChC(Coherence)는 2026-07-24부로
> 측정 중단·열 삭제**(사유·이력은 notes).
> **term** = 구조 보존 unravel → AProVE 직접. 그 초는 답을 낸 AProVE 실행 하나의 벽시계다
> (`-` = AProVE 미실행: DEGENERATE/unravel 오류). term은 예산 사다리(5·20·80·…·cap)를
> 올라가며 답이 나오는 최소 예산에서 멈추는데, **예산은 안 적는다** — AProVE가 마감 전에
> 답하는 경우가 많아(277행 중 171행) 예산은 실제 시간과 무관한 천장이거나(즉답 부류) 초와
> 겹치므로(마감 부류), 초 하나가 유일한 정직한 심볼당 수치다.
>
> 재현 커맨드·방법론·측정 이력·비-YES 해석·병렬
> 안전성 소견은 모두 **[verification-notes.md](verification-notes.md)**.
>
> **측정 기준**: CRC는 `bff805ec` (2026-07-23 전수 재측정) — 통합 커밋 `160fef97`이
> 분석 표면을 byte-identical로 보존하므로 유효하다. term은 `a6f056ed` (2026-07-24
> 예산 사다리 + 답한 단계 타이밍, cap 1800, `$write_bits_from_value`는 AProVE 증명 export
> 스택오버플로 회피에 JVM `-Xss512m`). **종전 term초는 예산 설정값이었다** — 어려운 심볼일수록
> AProVE가 마감까지 탐색해 값이 최대 320배 부풀려져 있었다(경위는 notes).

## 1. ≤500 종합 (153심볼)

**요약**: CRC YES 146 (정규화 5 포함) / TIMEOUT 7 · term YES **153 / 153** · 비합류·비종료 후보 0. 잔여 CRC TIMEOUT 7(전부 비트벡터 산술)은 도구 한계(자세히는 notes).
**term 시간**: 답한 단계의 벽시계 합 279.8초 — 151심볼이 1단계(≤6초)에 답하고, `$bitacc_range_op`·`$bitacc_offset_op` 둘만 21.6초다.


| symbol | rules | CRC | term |
|---|---|---|---|
| `$annotationList_of_parameterIR` | 1 | YES (0.1s) | YES (0.4s) |
| `$ctk_of_typedExpressionIR` | 1 | YES (0.1s) | YES (0.5s) |
| `$empty_map` | 1 | YES (0.1s) | YES (0.4s) |
| `$empty_set` | 1 | YES (0.1s) | YES (0.4s) |
| `$empty_tableContext` | 1 | YES (0.1s) | YES (0.5s) |
| `$id_of_parameterIR` | 1 | YES (0.1s) | YES (0.4s) |
| `$invalidate_header` | 1 | YES (0.1s) | YES (0.5s) |
| `$parameterListIR_of_actionDef` | 1 | YES (0.1s) | YES (0.4s) |
| `$parameterListIR_of_actionTypeDefIR` | 1 | YES (0.1s) | YES (0.4s) |
| `$parameterListIR_of_constructorTypeDefIR` | 1 | YES (0.1s) | YES (0.4s) |
| `$parameterListIR_of_controlApplyMethodDef` | 1 | YES (0.1s) | YES (0.4s) |
| `$parameterListIR_of_controlApplyMethodTypeIR` | 1 | YES (0.1s) | YES (0.4s) |
| `$parameterListIR_of_definedFunctionDef` | 1 | YES (0.1s) | YES (0.5s) |
| `$parameterListIR_of_definedFunctionTypeDefIR` | 1 | YES (0.1s) | YES (0.5s) |
| `$parameterListIR_of_externFunctionDef` | 1 | YES (0.1s) | YES (0.5s) |
| `$parameterListIR_of_externFunctionTypeDefIR` | 1 | YES (0.1s) | YES (0.5s) |
| `$parameterListIR_of_parserApplyMethodDef` | 1 | YES (0.1s) | YES (0.4s) |
| `$parameterListIR_of_parserApplyMethodTypeIR` | 1 | YES (0.1s) | YES (0.4s) |
| `$parameterListIR_of_tableApplyMethodDef` | 1 | YES (0.1s) | YES (0.4s) |
| `$parameterListIR_of_tableApplyMethodTypeDefIR` | 1 | YES (0.1s) | YES (0.4s) |
| `$tableEntryPriorityOptIR_of_tableEntryIR` | 1 | YES (0.1s) | YES (0.4s) |
| `$type_of_typedExpressionIR` | 1 | YES (0.1s) | YES (0.4s) |
| `$type_of_typedLvalueIR` | 1 | YES (0.1s) | YES (0.4s) |
| `$parameterListIR_of_externMethodDef` | 1 | YES (0.1s) | YES (0.4s) |
| `$set_priority_of_tableEntryIR` | 1 | YES (0.1s) | YES (0.4s) |
| `$empty_callableDefEnv` | 2 | YES (0.1s) | YES (0.4s) |
| `$empty_callableTypeDefEnv` | 2 | YES (0.1s) | YES (0.5s) |
| `$empty_constructorDefEnv` | 2 | YES (0.1s) | YES (0.4s) |
| `$empty_constructorTypeDefEnv` | 2 | YES (0.1s) | YES (0.4s) |
| `$empty_frame` | 2 | YES (0.1s) | YES (0.4s) |
| `$empty_stateEnv` | 2 | YES (0.1s) | YES (0.4s) |
| `$empty_store` | 2 | YES (0.1s) | YES (0.4s) |
| `$empty_theta` | 2 | YES (0.1s) | YES (0.4s) |
| `$empty_typeDefEnv` | 2 | YES (0.1s) | YES (0.4s) |
| `$empty_typeFrame` | 2 | YES (0.1s) | YES (0.4s) |
| `$flatten_constOpt` | 2 | YES (0.1s) | YES (0.4s) |
| `$ite` | 2 | YES (0.1s) | YES (0.4s) |
| `$flatten_objectInitializerOptIR` | 2 | YES (0.1s) | YES (0.4s) |
| `$is_some` | 2 | YES (0.1s) | YES (0.4s) |
| `$opt_as_seq` | 2 | YES (0.1s) | YES (0.5s) |
| `$parameterListIR_of_externMethodTypeDefIR` | 2 | YES (0.1s) | YES (0.5s) |
| `$type_of_externMethodPrototypeIR` | 2 | YES (0.1s) | YES (0.4s) |
| `$callable_builtinMethod` | 3 | YES (0.1s) | YES (0.4s) |
| `$constructorTypeDef_of_externConstructorPrototypeIR` | 3 | YES (0.1s) | YES (0.5s) |
| `$constructor_of_externConstructorPrototypeIR` | 3 | YES (0.1s) | YES (0.5s) |
| `$empty_constraint` | 3 | YES (0.1s) | YES (0.4s) |
| `$instantiable_extern` | 3 | YES (0.1s) | YES (0.5s) |
| `$is_lpm_key_prime` | 3 | YES (0.1s) | YES (0.5s) |
| `$un_lnot` | 3 | YES (0.1s) | YES (0.4s) |
| `$join_tableEntryState` | 3 | YES (0.1s) | YES (0.4s) |
| `$filter` | 3 | YES (0.1s) | YES (0.5s) |
| `$is_concrete_extern_object_prime_prime` | 4 | YES (0.1s) | YES (0.4s) |
| `$is_default_parameterIR` | 4 | YES (0.1s) | YES (0.5s) |
| `$is_lpm_key` | 4 | YES (0.1s) | YES (0.5s) |
| `$concat_text` | 4 | YES (0.1s) | YES (0.5s) |
| `$exists` | 4 | YES (0.1s) | YES (0.5s) |
| `$flatten_blockElementStatementList` | 4 | YES (0.1s) | YES (0.4s) |
| `$flatten_controlLocalDeclarationList` | 4 | YES (0.1s) | YES (0.4s) |
| `$flatten_externConstructorOrMethodPrototypeList` | 4 | YES (0.1s) | YES (0.4s) |
| `$flatten_objectDeclarationList` | 4 | YES (0.1s) | YES (0.4s) |
| `$flatten_parserLocalDeclarationList` | 4 | YES (0.1s) | YES (0.4s) |
| `$flatten_parserStatementList` | 4 | YES (0.1s) | YES (0.4s) |
| `$flatten_selectCaseList` | 4 | YES (0.1s) | YES (0.4s) |
| `$flatten_switchCaseList` | 4 | YES (0.1s) | YES (0.4s) |
| `$flatten_tableActionList` | 4 | YES (0.1s) | YES (0.4s) |
| `$flatten_tableEntryList` | 4 | YES (0.1s) | YES (0.4s) |
| `$flatten_tableKeyList` | 4 | YES (0.1s) | YES (0.5s) |
| `$flatten_tablePropertyList` | 4 | YES (0.1s) | YES (0.5s) |
| `$flatten_typeFieldList` | 4 | YES (0.1s) | YES (0.5s) |
| `$forall` | 4 | YES (0.1s) | YES (0.5s) |
| `$join_flow` | 4 | YES (0.1s) | YES (0.5s) |
| `$flatten_prefixedNameIR` | 4 | YES (0.1s) | YES (0.4s) |
| `$add_action_tbl` | 5 | YES (0.2s) | YES (0.5s) |
| `$add_key_tbl` | 5 | YES (0.1s) | YES (0.4s) |
| `$codom_map` | 5 | YES (0.1s) | YES (0.5s) |
| `$dom_map` | 5 | YES (0.1s) | YES (0.5s) |
| `$enter_path_i` | 5 | YES (0.1s) | YES (0.4s) |
| `$flatten_p4program` | 5 | YES (0.1s) | YES (0.4s) |
| `$empty_typingContext` | 6 | YES (0.1s) | YES (0.5s) |
| `$is_tableActionsProperty` | 6 | YES (0.1s) | YES (0.5s) |
| `$is_tableKeysProperty` | 6 | YES (0.1s) | YES (0.4s) |
| `$tableCustomName` | 6 | YES (0.1s) | YES (0.5s) |
| `$enter_i` | 7 | YES (0.1s) | YES (0.4s) |
| `$enter_t` | 7 | YES (0.1s) | YES (0.4s) |
| `$exit_i` | 7 | YES (0.2s) | YES (0.4s) |
| `$exit_t` | 7 | YES (0.1s) | YES (0.5s) |
| `$requires_priority_prime` | 7 | YES (0.2s) | YES (0.5s) |
| `$empty_instContext` | 8 | YES (0.1s) | YES (0.5s) |
| `$requires_priority` | 8 | YES (0.2s) | YES (0.5s) |
| `$typedLvalueIR_as_typedExpressionIR` | 8 | YES (0.2s) | YES (0.5s) |
| `$join_ctk` | 9 | YES (0.1s) | YES (0.4s) |
| `$width_of_integerTypeIR` | 10 | YES (0.2s) | YES (0.4s) |
| `$inherit_i` | 10 | YES (0.2s) | YES (0.5s) |
| `$name` | 10 | YES (0.2s) | YES (0.5s) |
| `$un_plus` | 11 | YES (0.2s) | YES (0.4s) |
| `$callableId_IR` | 13 | YES (42.7s) | YES (0.5s) |
| `$objectId_ends_with` | 11 | YES (0.2s) | YES (0.5s) |
| `$callableId_of_externConstructorPrototypeIR` | 14 | YES (75.4s) | YES (0.5s) |
| `$prefixedTypeName` | 12 | YES (0.2s) | YES (0.5s) |
| `$join_text` | 13 | YES (0.2s) | YES (0.6s) |
| `$resolve_constraint` | 9 | YES (0.2s) | YES (0.5s) |
| `$callableId_of_externMethodPrototypeIR` | 15 | YES (132.2s) | YES (0.5s) |
| `$flatten_nameList` | 13 | YES (0.2s) | YES (0.5s) |
| `$flatten_typeParameterList` | 13 | YES (0.2s) | YES (0.4s) |
| `$assignop_as_binop` | 13 | YES (0.2s) | YES (0.4s) |
| `$flatten_typeParameterListOpt` | 15 | YES (0.2s) | YES (0.4s) |
| `$is_tableDefaultActionProperty` | 16 | YES (0.3s) | YES (0.5s) |
| `$prefixedNonTypeName` | 19 | YES (0.3s) | YES (0.5s) |
| `$optional_annotation_of_parameterIR_prime_prime` | 20 | YES (0.4s) | YES (0.5s) |
| `$lvalue_as_expression` | 22 | YES (0.5s) | YES (0.5s) |
| `$starts_with` | 50 | YES (1.5s) | YES (0.5s) |
| `$strip_prefix_rec` | 53 | YES (1.7s) | YES (0.5s) |
| `$isValid_header` | 80 | YES (4.8s) | YES (0.6s) |
| `$ends_with` | 88 | YES (4.9s) | YES (0.6s) |
| `$strip_suffix_rec` | 91 | YES (5.5s) | YES (0.6s) |
| `$invalidate_headerUnion` | 87 | YES (6.6s) | YES (0.8s) |
| `$invalidate_value` | 87 | YES (6.5s) | YES (0.8s) |
| `$write_bits_from_value` | 103 | TIMEOUT (>1800s) | YES (6.0s) |
| `$bin_mod` | 109 | YES (7.6s) | YES (0.6s) |
| `$bin_div` | 113 | YES (8.3s) | YES (0.6s) |
| `$un_bnot` | 139 | YES (14.1s) | YES (5.5s) |
| `$bin_ge` | 183 | YES (30.1s) | YES (5.6s) |
| `$bin_le` | 183 | YES (29.9s) | YES (5.6s) |
| `$bin_gt` | 184 | YES (30.0s) | YES (5.6s) |
| `$bin_lt` | 184 | YES (29.9s) | YES (5.6s) |
| `$int_of_integerValue` | 184 | YES (29.6s) | YES (5.6s) |
| `$nat_of_integerValue` | 187 | YES (31.2s) | YES (5.6s) |
| `$bin_minus` | 193 | YES (36.1s) | YES (5.6s) |
| `$bin_mul` | 193 | YES (36.3s) | YES (5.7s) |
| `$bin_plus` | 193 | YES (36.3s) | YES (5.6s) |
| `$un_minus` | 197 | YES (37.5s) | YES (5.6s) |
| `$bin_bxor` | 199 | YES (43.6s) | YES (5.6s) |
| `$bin_concat` | 200 | YES (425.7s) | YES (5.6s) |
| `$set_priorities_of_tableEntryListIR_prime` | 200 | YES (43.7s) | YES (5.7s) |
| `$bin_satminus` | 201 | YES (46.0s) | YES (5.7s) |
| `$bin_satplus` | 201 | YES (45.8s) | YES (5.9s) |
| `$bin_shl` | 201 | TIMEOUT (>1800s) | YES (5.6s) |
| `$bin_band` | 202 | YES (142.9s) | YES (5.7s) |
| `$bin_bor` | 202 | YES (143.3s) | YES (5.7s) |
| `$name_annotationToken` | 209 | YES (53.3s) | YES (5.9s) |
| `$un_op` | 209 | YES (47.9s) | YES (5.6s) |
| `$bin_shr` | 215 | TIMEOUT (>1800s) | YES (5.8s) |
| `$set_priorities_of_tableEntryListIR` | 226 | YES (63.7s) | YES (5.7s) |
| `$name_annotation_opt` | 256 | YES (99.5s) | YES (6.1s) |
| `$write_value_field_from_bits_prime` | 271 | YES* (244.1s) | YES (5.7s) |
| `$write_value_fields_from_bits_prime` | 271 | YES* (243.9s) | YES (5.7s) |
| `$write_value_from_bits_prime` | 271 | YES* (244.0s) | YES (5.7s) |
| `$write_values_from_bits_prime` | 271 | YES* (242.8s) | YES (5.7s) |
| `$write_value_from_bits` | 274 | YES* (253.0s) | YES (5.7s) |
| `$bitacc_range_op` | 283 | TIMEOUT (>1800s) | YES (21.6s) |
| `$bitacc_offset_op` | 285 | TIMEOUT (>1800s) | YES (21.6s) |
| `$bitacc_range_replace_op` | 391 | TIMEOUT (>1800s) | YES (5.6s) |
| `$bitacc_offset_replace_op` | 394 | TIMEOUT (>1800s) | YES (5.7s) |

## 2. >500 슬라이스 (bigsweep · 124/124 측정)

**요약 (term, 124심볼)**: YES 124. 비종료 후보 0. 답한 단계의 벽시계 합 479.6초(전부 1단계에서 답).
**CRC 열은 옛 bigsweep 값을 그대로 옮긴 것**(이번에 재측정한 축은 term뿐, `-`는 미도달).

| # | symbol | rules | CRC | term |
|---|---|---|---|---|
| 1 | `$flatten_namedExpressionList` | 748 | YES | YES (1.4s) |
| 2 | `$flatten_realTypeArgumentList` | 748 | YES | YES (1.5s) |
| 3 | `$flatten_expressionList` | 749 | TIMEOUT | YES (1.5s) |
| 4 | `$flatten_typeArgumentList` | 749 | TIMEOUT | YES (1.4s) |
| 5 | `$expression_as_lvalue` | 766 | MAYBE | YES (4.0s) |
| 6 | `$flatten_argumentList` | 784 | TIMEOUT | YES (5.9s) |
| 7 | `$flatten_simpleKeysetExpressionList` | 789 | TIMEOUT | YES (1.5s) |
| 8 | `$flatten_forUpdateStatementList` | 790 | YES | YES (2.3s) |
| 9 | `$is_singleton_list_expression` | 812 | YES | YES (1.7s) |
| 10 | `$add_annotationList` | 867 | YES | YES (1.8s) |
| 11 | `$flatten_annotationList` | 868 | YES | YES (2.5s) |
| 12 | `$flatten_parameterList` | 874 | YES | YES (2.4s) |
| 13 | `$flatten_constructorParameterListOpt` | 876 | YES | YES (2.5s) |
| 14 | `$is_externConstructorPrototype` | 880 | YES | YES (1.8s) |
| 15 | `$is_externMethodPrototype` | 883 | YES | YES (1.9s) |
| 16 | `$callableId_prime` | 884 | YES | YES (2.7s) |
| 17 | `$callableId` | 885 | YES | YES (2.5s) |
| 18 | `$constructorId_of_externConstructorPrototype` | 886 | YES | YES (2.5s) |
| 19 | `$callableId_of_externMethodPrototype` | 887 | YES | YES (2.5s) |
| 20 | `$constructorId` | 887 | YES | YES (2.7s) |
| 21 | `$expressionNonBrace_as_expression` | 887 | TIMEOUT | YES (2.1s) |
| 22 | `$optional_annotation_of_parameterIR_prime` | 893 | YES | YES (2.0s) |
| 23 | `$optional_annotation_of_parameterIR` | 895 | YES | YES (2.2s) |
| 24 | `$is_optional_parameterIR` | 896 | YES | YES (2.2s) |
| 25 | `$flatten_forInitStatementList` | 907 | YES | YES (2.8s) |
| 26 | `$split_externConstructorOrMethodPrototypeList` | 940 | YES | YES (2.3s) |
| 27 | `$flatten_parserStateList` | 1029 | YES | YES (0.6s) |
| 28 | `$name_annotation` | 1125 | - | YES (6.0s) |
| 29 | `$name_annotation_default` | 1127 | - | YES (6.1s) |
| 30 | `$cast_header_stack` | 1192 | - | YES (0.6s) |
| 31 | `$cast_header` | 1194 | - | YES (0.7s) |
| 32 | `$cast_struct` | 1194 | - | YES (0.6s) |
| 33 | `$compat_lnot` | 1194 | - | YES (5.7s) |
| 34 | `$nestable_constructor_package` | 1194 | - | YES (5.8s) |
| 35 | `$resolve_type_alias` | 1194 | - | YES (5.6s) |
| 36 | `$callTargetKey_prime` | 1195 | - | YES (0.6s) |
| 37 | `$compat_bnot` | 1195 | - | YES (5.6s) |
| 38 | `$compat_divmod` | 1195 | - | YES (5.6s) |
| 39 | `$compat_logical` | 1195 | - | YES (5.7s) |
| 40 | `$cast_bool` | 1196 | - | YES (5.7s) |
| 41 | `$compat_array_index` | 1196 | - | YES (5.6s) |
| 42 | `$compat_bitslice_offset_index` | 1196 | - | YES (5.7s) |
| 43 | `$compat_bitslice_offset_width` | 1196 | - | YES (5.7s) |
| 44 | `$compat_bitslice_range_index` | 1196 | - | YES (5.6s) |
| 45 | `$compat_uplusminus` | 1196 | - | YES (5.6s) |
| 46 | `$nestable_constructor_control` | 1196 | - | YES (5.8s) |
| 47 | `$nestable_constructor_parser` | 1196 | - | YES (5.7s) |
| 48 | `$nestable_controlApplyMethod` | 1196 | - | YES (5.6s) |
| 49 | `$nestable_headerStack` | 1196 | - | YES (5.8s) |
| 50 | `$nestable_headerUnion` | 1196 | - | YES (5.6s) |
| 51 | `$definable_constructor` | 1197 | - | YES (5.7s) |
| 52 | `$nestable_constructor_extern` | 1197 | - | YES (5.6s) |
| 53 | `$nestable_externFunction` | 1197 | - | YES (5.6s) |
| 54 | `$nestable_externMethod` | 1197 | - | YES (5.7s) |
| 55 | `$nestable_new_in_enum_serializable` | 1197 | - | YES (5.6s) |
| 56 | `$nestable_parserApplyMethod` | 1197 | - | YES (5.6s) |
| 57 | `$compat_switch` | 1198 | - | YES (5.7s) |
| 58 | `$compat_table_lpm_ternary_range_key` | 1198 | - | YES (5.6s) |
| 59 | `$nestable_new` | 1198 | - | YES (5.6s) |
| 60 | `$parameterListIR_of_functionTypeDefIR` | 1198 | - | YES (0.6s) |
| 61 | `$typedExpressionIR_as_typedLvalueIR` | 1198 | - | YES (0.6s) |
| 62 | `$compat_concat` | 1199 | - | YES (5.6s) |
| 63 | `$callTargetKey` | 1200 | - | YES (0.6s) |
| 64 | `$compat_table_exact_optional_key` | 1202 | - | YES (5.6s) |
| 65 | `$callableTypeIR_of_callableTypeDefIR` | 1203 | - | YES (0.6s) |
| 66 | `$compat_shift` | 1203 | - | YES (5.9s) |
| 67 | `$nestable_enum_serializable` | 1203 | - | YES (5.7s) |
| 68 | `$typeParameterListIR_of_callableTypeDefIR` | 1203 | - | YES (0.6s) |
| 69 | `$flatten_keysetExpressionIR` | 1206 | - | YES (0.6s) |
| 70 | `$is_static_assert_callableTypeIR` | 1206 | - | YES (0.6s) |
| 71 | `$nestable_tuple_in_set` | 1206 | - | YES (5.6s) |
| 72 | `$parameterListIR_of_methodTypeDefIR` | 1206 | - | YES (0.6s) |
| 73 | `$typeId_of_typeDefIR` | 1207 | - | YES (0.6s) |
| 74 | `$typeParameterListIR_of_typeDefIR` | 1207 | - | YES (0.6s) |
| 75 | `$nestable_sequence_in_set` | 1208 | - | YES (5.6s) |
| 76 | `$nestable_struct_in_header` | 1208 | - | YES (5.6s) |
| 77 | `$nestable_tuple` | 1208 | - | YES (5.7s) |
| 78 | `$nestable_struct` | 1209 | - | YES (5.8s) |
| 79 | `$nestable_definedFunction` | 1211 | - | YES (5.6s) |
| 80 | `$nestable_action` | 1212 | - | YES (5.8s) |
| 81 | `$nestable_list` | 1212 | - | YES (5.7s) |
| 82 | `$is_equalable_typeIR` | 1213 | - | YES (5.7s) |
| 83 | `$typeIR_of_typeDefIR` | 1213 | - | YES (0.6s) |
| 84 | `$is_assignable_typeIR` | 1214 | - | YES (5.8s) |
| 85 | `$nestable_typedef` | 1215 | - | YES (5.7s) |
| 86 | `$init_tableKeys` | 1216 | - | YES (1.4s) |
| 87 | `$compat_bitslice_base` | 1218 | - | YES (5.9s) |
| 88 | `$nestable_header` | 1218 | - | YES (5.8s) |
| 89 | `$is_defaultable_typeIR` | 1219 | - | YES (5.7s) |
| 90 | `$parameterListIR_of_callableTypeDefIR` | 1221 | - | YES (0.6s) |
| 91 | `$unroll_typeIR` | 1229 | - | YES (5.7s) |
| 92 | `$is_table_application` | 1231 | - | YES (5.7s) |
| 93 | `$nestable_set` | 1235 | - | YES (5.9s) |
| 94 | `$sizeof_minSizeInBits_prime` | 1259 | - | YES (5.8s) |
| 95 | `$sizeof_minSizeInBits` | 1260 | - | YES (5.7s) |
| 96 | `$unroll_aliasType` | 1268 | - | YES (5.7s) |
| 97 | `$result_concat` | 1269 | - | YES (5.7s) |
| 98 | `$find_local_return_type_t` | 1270 | - | YES (0.7s) |
| 99 | `$is_concrete_extern_object_prime` | 1278 | - | YES (0.7s) |
| 100 | `$sizeof_maxSizeInBits_prime` | 1283 | - | YES (5.7s) |
| 101 | `$sizeof_maxSizeInBits` | 1284 | - | YES (5.7s) |
| 102 | `$is_monomorphic_typeDefIR` | 1289 | - | YES (0.7s) |
| 103 | `$is_polymorphic_typeDefIR` | 1292 | - | YES (0.6s) |
| 104 | `$resolve_inference_prime` | 1301 | - | YES (5.7s) |
| 105 | `$parameterListIR_of_functionDef` | 1304 | - | YES (0.6s) |
| 106 | `$resolve_inference` | 1306 | - | YES (5.6s) |
| 107 | `$reduce_serenum` | 1310 | - | YES (5.6s) |
| 108 | `$is_concrete_extern_object` | 1317 | - | YES (5.7s) |
| 109 | `$update_mode_tbl` | 1337 | - | YES (5.7s) |
| 110 | `$sizeof_minSizeInBytes` | 1347 | - | YES (5.7s) |
| 111 | `$sizeof_maxSizeInBytes` | 1350 | - | YES (5.7s) |
| 112 | `$sizeof` | 1375 | - | YES (5.8s) |
| 113 | `$init_tableEntries` | 1406 | - | YES (1.4s) |
| 114 | `$is_valid_bitslice` | 1431 | - | YES (5.7s) |
| 115 | `$init_table` | 1462 | - | YES (1.4s) |
| 116 | `$parameterListIR_of_methodDef` | 1551 | - | YES (0.7s) |
| 117 | `$parameterListIR_of_callableDef` | 1565 | - | YES (0.6s) |
| 118 | `$parameterListIR_of_constructorDef` | 1570 | - | YES (0.6s) |
| 119 | `$subexpressions_of_argumentIR` | 1614 | - | YES (5.7s) |
| 120 | `$subexpressions_of_argumentListIR` | 1614 | - | YES (5.7s) |
| 121 | `$subexpressions_of_expressionIR` | 1614 | - | YES (5.6s) |
| 122 | `$subexpressions_of_typedExpressionIR` | 1614 | - | YES (5.7s) |
| 123 | `$subexpressions_of_typedExpressionListIR` | 1614 | - | YES (5.6s) |
| 124 | `$name_expression` | 1840 | - | YES (6.2s) |
