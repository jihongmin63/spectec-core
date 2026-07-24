# P4 structural CTRS — 검증 결과 (표)

> **표만 둔다.** 범례: **CRC** = Church-Rosser(합류성) — `YES` / `YES*`(=`--crc-normalize` upgrade-only로 닫힘) / `MAYBE` / `TIMEOUT` / `-`(미도달). **ChC(Coherence)는 2026-07-24부로 측정 중단·열 삭제**(사유·이력은 notes).
> **term** = 구조 보존 unravel → AProVE 직접. **CRC초/term초** = 심볼당 **직렬 fresh**
> 측정 벽시계 초(`>1800` = 예산 내 미완). **term예산** = 그 판정을 낸 AProVE 예산
> — 예산 사다리(5·20·80·…·cap)를 올라가 처음 답이 나온 rung이므로, `YES`에 대해서는
> 증명 난이도의 상한이다. 재현 커맨드·방법론·측정 이력·비-YES 해석·병렬
> 안전성 소견은 모두 **[verification-notes.md](verification-notes.md)**.
>
> **측정 기준**: CRC는 `bff805ec` (2026-07-23 전수 재측정) — 통합 커밋 `160fef97`이
> 분석 표면을 byte-identical로 보존하므로 유효하다. term은 `a1fd043b` (2026-07-24
> 예산 사다리 도입 후 전수 재측정, cap 1800, `$write_bits_from_value`는 AProVE 증명
> export 스택오버플로 회피에 JVM `-Xss512m`). **종전 term초는 예산 설정값이었다** —
> AProVE가 자기 마감 시각에 판정을 발표해 값이 최대 320배 부풀려져 있었다(경위는 notes).

## 1. ≤500 종합 (153심볼)

**요약**: CRC YES 146 (정규화 5 포함) / TIMEOUT 7 · term YES **153 / 153** · 비합류·비종료 후보 0. 잔여 CRC TIMEOUT 7(전부 비트벡터 산술)은 도구 한계(자세히는 notes).
**term 예산 분포**: 5 → 151심볼 / 20 → 2심볼(`$bitacc_range_op`·`$bitacc_offset_op`). 전수 벽시계 합 291.3초.


| symbol | rules | CRC | term | CRC초 | term초 | term예산 |
|---|---|---|---|---|---|---|
| `$annotationList_of_parameterIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$ctk_of_typedExpressionIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_map` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_set` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_tableContext` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$id_of_parameterIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$invalidate_header` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_actionDef` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_actionTypeDefIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_constructorTypeDefIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_controlApplyMethodDef` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_controlApplyMethodTypeIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_definedFunctionDef` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_definedFunctionTypeDefIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_externFunctionDef` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_externFunctionTypeDefIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_parserApplyMethodDef` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_parserApplyMethodTypeIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_tableApplyMethodDef` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_tableApplyMethodTypeDefIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$tableEntryPriorityOptIR_of_tableEntryIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$type_of_typedExpressionIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$type_of_typedLvalueIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_externMethodDef` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$set_priority_of_tableEntryIR` | 1 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_callableDefEnv` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_callableTypeDefEnv` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_constructorDefEnv` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_constructorTypeDefEnv` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_frame` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_stateEnv` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_store` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_theta` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_typeDefEnv` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_typeFrame` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$flatten_constOpt` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$ite` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$flatten_objectInitializerOptIR` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$is_some` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$opt_as_seq` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$parameterListIR_of_externMethodTypeDefIR` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$type_of_externMethodPrototypeIR` | 2 | YES | YES | 0.1 | 0.4 | 5 |
| `$callable_builtinMethod` | 3 | YES | YES | 0.1 | 0.4 | 5 |
| `$constructorTypeDef_of_externConstructorPrototypeIR` | 3 | YES | YES | 0.1 | 0.5 | 5 |
| `$constructor_of_externConstructorPrototypeIR` | 3 | YES | YES | 0.1 | 0.5 | 5 |
| `$empty_constraint` | 3 | YES | YES | 0.1 | 0.4 | 5 |
| `$instantiable_extern` | 3 | YES | YES | 0.1 | 0.4 | 5 |
| `$is_lpm_key_prime` | 3 | YES | YES | 0.1 | 0.5 | 5 |
| `$un_lnot` | 3 | YES | YES | 0.1 | 0.4 | 5 |
| `$join_tableEntryState` | 3 | YES | YES | 0.1 | 0.4 | 5 |
| `$filter` | 3 | YES | YES | 0.1 | 0.5 | 5 |
| `$is_concrete_extern_object_prime_prime` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$is_default_parameterIR` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$is_lpm_key` | 4 | YES | YES | 0.1 | 0.5 | 5 |
| `$concat_text` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$exists` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$flatten_blockElementStatementList` | 4 | YES | YES | 0.1 | 0.5 | 5 |
| `$flatten_controlLocalDeclarationList` | 4 | YES | YES | 0.1 | 0.5 | 5 |
| `$flatten_externConstructorOrMethodPrototypeList` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$flatten_objectDeclarationList` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$flatten_parserLocalDeclarationList` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$flatten_parserStatementList` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$flatten_selectCaseList` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$flatten_switchCaseList` | 4 | YES | YES | 0.1 | 0.5 | 5 |
| `$flatten_tableActionList` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$flatten_tableEntryList` | 4 | YES | YES | 0.1 | 0.5 | 5 |
| `$flatten_tableKeyList` | 4 | YES | YES | 0.1 | 0.5 | 5 |
| `$flatten_tablePropertyList` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$flatten_typeFieldList` | 4 | YES | YES | 0.1 | 0.5 | 5 |
| `$forall` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$join_flow` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$flatten_prefixedNameIR` | 4 | YES | YES | 0.1 | 0.4 | 5 |
| `$add_action_tbl` | 5 | YES | YES | 0.2 | 0.4 | 5 |
| `$add_key_tbl` | 5 | YES | YES | 0.1 | 0.4 | 5 |
| `$codom_map` | 5 | YES | YES | 0.1 | 0.5 | 5 |
| `$dom_map` | 5 | YES | YES | 0.1 | 0.5 | 5 |
| `$enter_path_i` | 5 | YES | YES | 0.1 | 0.4 | 5 |
| `$flatten_p4program` | 5 | YES | YES | 0.1 | 0.4 | 5 |
| `$empty_typingContext` | 6 | YES | YES | 0.1 | 0.4 | 5 |
| `$is_tableActionsProperty` | 6 | YES | YES | 0.1 | 0.4 | 5 |
| `$is_tableKeysProperty` | 6 | YES | YES | 0.1 | 0.4 | 5 |
| `$tableCustomName` | 6 | YES | YES | 0.1 | 0.5 | 5 |
| `$enter_i` | 7 | YES | YES | 0.1 | 0.4 | 5 |
| `$enter_t` | 7 | YES | YES | 0.1 | 0.4 | 5 |
| `$exit_i` | 7 | YES | YES | 0.2 | 0.5 | 5 |
| `$exit_t` | 7 | YES | YES | 0.1 | 0.5 | 5 |
| `$requires_priority_prime` | 7 | YES | YES | 0.2 | 0.5 | 5 |
| `$empty_instContext` | 8 | YES | YES | 0.1 | 0.4 | 5 |
| `$requires_priority` | 8 | YES | YES | 0.2 | 0.5 | 5 |
| `$typedLvalueIR_as_typedExpressionIR` | 8 | YES | YES | 0.2 | 0.6 | 5 |
| `$join_ctk` | 9 | YES | YES | 0.1 | 0.4 | 5 |
| `$width_of_integerTypeIR` | 10 | YES | YES | 0.2 | 0.5 | 5 |
| `$inherit_i` | 10 | YES | YES | 0.2 | 0.5 | 5 |
| `$name` | 10 | YES | YES | 0.2 | 0.5 | 5 |
| `$un_plus` | 11 | YES | YES | 0.2 | 0.4 | 5 |
| `$callableId_IR` | 13 | YES | YES | 42.7 | 0.5 | 5 |
| `$objectId_ends_with` | 11 | YES | YES | 0.2 | 0.5 | 5 |
| `$callableId_of_externConstructorPrototypeIR` | 14 | YES | YES | 75.4 | 0.5 | 5 |
| `$prefixedTypeName` | 12 | YES | YES | 0.2 | 0.5 | 5 |
| `$join_text` | 13 | YES | YES | 0.2 | 0.5 | 5 |
| `$resolve_constraint` | 9 | YES | YES | 0.2 | 0.5 | 5 |
| `$callableId_of_externMethodPrototypeIR` | 15 | YES | YES | 132.2 | 0.5 | 5 |
| `$flatten_nameList` | 13 | YES | YES | 0.2 | 0.4 | 5 |
| `$flatten_typeParameterList` | 13 | YES | YES | 0.2 | 0.5 | 5 |
| `$assignop_as_binop` | 13 | YES | YES | 0.2 | 0.4 | 5 |
| `$flatten_typeParameterListOpt` | 15 | YES | YES | 0.2 | 0.5 | 5 |
| `$is_tableDefaultActionProperty` | 16 | YES | YES | 0.3 | 0.5 | 5 |
| `$prefixedNonTypeName` | 19 | YES | YES | 0.3 | 0.5 | 5 |
| `$optional_annotation_of_parameterIR_prime_prime` | 20 | YES | YES | 0.4 | 0.5 | 5 |
| `$lvalue_as_expression` | 22 | YES | YES | 0.5 | 0.5 | 5 |
| `$starts_with` | 50 | YES | YES | 1.5 | 0.5 | 5 |
| `$strip_prefix_rec` | 53 | YES | YES | 1.7 | 0.5 | 5 |
| `$isValid_header` | 80 | YES | YES | 4.8 | 0.5 | 5 |
| `$ends_with` | 88 | YES | YES | 4.9 | 0.6 | 5 |
| `$strip_suffix_rec` | 91 | YES | YES | 5.5 | 0.6 | 5 |
| `$invalidate_headerUnion` | 87 | YES | YES | 6.6 | 0.7 | 5 |
| `$invalidate_value` | 87 | YES | YES | 6.5 | 0.8 | 5 |
| `$write_bits_from_value` | 103 | TIMEOUT | YES | >1800 | 6.0 | 5 |
| `$bin_mod` | 109 | YES | YES | 7.6 | 0.6 | 5 |
| `$bin_div` | 113 | YES | YES | 8.3 | 0.6 | 5 |
| `$un_bnot` | 139 | YES | YES | 14.1 | 5.5 | 5 |
| `$bin_ge` | 183 | YES | YES | 30.1 | 5.6 | 5 |
| `$bin_le` | 183 | YES | YES | 29.9 | 5.6 | 5 |
| `$bin_gt` | 184 | YES | YES | 30.0 | 5.6 | 5 |
| `$bin_lt` | 184 | YES | YES | 29.9 | 5.6 | 5 |
| `$int_of_integerValue` | 184 | YES | YES | 29.6 | 5.6 | 5 |
| `$nat_of_integerValue` | 187 | YES | YES | 31.2 | 5.6 | 5 |
| `$bin_minus` | 193 | YES | YES | 36.1 | 5.6 | 5 |
| `$bin_mul` | 193 | YES | YES | 36.3 | 5.7 | 5 |
| `$bin_plus` | 193 | YES | YES | 36.3 | 5.7 | 5 |
| `$un_minus` | 197 | YES | YES | 37.5 | 5.7 | 5 |
| `$bin_bxor` | 199 | YES | YES | 43.6 | 5.6 | 5 |
| `$bin_concat` | 200 | YES | YES | 425.7 | 5.6 | 5 |
| `$set_priorities_of_tableEntryListIR_prime` | 200 | YES | YES | 43.7 | 5.7 | 5 |
| `$bin_satminus` | 201 | YES | YES | 46.0 | 5.7 | 5 |
| `$bin_satplus` | 201 | YES | YES | 45.8 | 5.6 | 5 |
| `$bin_shl` | 201 | TIMEOUT | YES | >1800 | 5.6 | 5 |
| `$bin_band` | 202 | YES | YES | 142.9 | 5.6 | 5 |
| `$bin_bor` | 202 | YES | YES | 143.3 | 5.6 | 5 |
| `$name_annotationToken` | 209 | YES | YES | 53.3 | 6.0 | 5 |
| `$un_op` | 209 | YES | YES | 47.9 | 5.7 | 5 |
| `$bin_shr` | 215 | TIMEOUT | YES | >1800 | 5.7 | 5 |
| `$set_priorities_of_tableEntryListIR` | 226 | YES | YES | 63.7 | 5.7 | 5 |
| `$name_annotation_opt` | 256 | YES | YES | 99.5 | 6.0 | 5 |
| `$write_value_field_from_bits_prime` | 271 | YES* | YES | 244.1 | 5.9 | 5 |
| `$write_value_fields_from_bits_prime` | 271 | YES* | YES | 243.9 | 5.8 | 5 |
| `$write_value_from_bits_prime` | 271 | YES* | YES | 244.0 | 5.8 | 5 |
| `$write_values_from_bits_prime` | 271 | YES* | YES | 242.8 | 5.7 | 5 |
| `$write_value_from_bits` | 274 | YES* | YES | 253.0 | 5.8 | 5 |
| `$bitacc_range_op` | 283 | TIMEOUT | YES | >1800 | 27.0 | 20 |
| `$bitacc_offset_op` | 285 | TIMEOUT | YES | >1800 | 29.2 | 20 |
| `$bitacc_range_replace_op` | 391 | TIMEOUT | YES | >1800 | 5.6 | 5 |
| `$bitacc_offset_replace_op` | 394 | TIMEOUT | YES | >1800 | 5.6 | 5 |

## 2. >500 슬라이스 (bigsweep · term = 모듈러 B · 27/127 확정)

| # | symbol | rules | CRC | term(B) | 종료(term) 시간 |
|---|---|---|---|---|---|
| 1 | `$flatten_namedExpressionList` | 748 | YES | MAYBE | 2061s |
| 2 | `$expression_as_lvalue` | 764 | MAYBE | YES | 4352s |
| 3 | `$flatten_realTypeArgumentList` | 767 | YES | MAYBE | 2154s |
| 4 | `$flatten_typeArgumentList` | 777 | TIMEOUT | MAYBE | 2115s |
| 5 | `$flatten_expressionList` | 780 | TIMEOUT | MAYBE | 2142s |
| 6 | `$flatten_argumentList` | 782 | TIMEOUT | MAYBE | 2172s |
| 7 | `$flatten_forUpdateStatementList` | 788 | YES | YES | 3413s |
| 8 | `$is_singleton_list_expression` | 810 | YES | YES | 2148s |
| 9 | `$flatten_simpleKeysetExpressionList` | 823 | TIMEOUT | MAYBE | 2387s |
| 10 | `$flatten_annotationList` | 866 | YES | YES | 3377s |
| 11 | `$add_annotationList` | 867 | YES | YES | 6261s |
| 12 | `$flatten_parameterList` | 872 | YES | YES | 4345s |
| 13 | `$flatten_constructorParameterListOpt` | 876 | YES | YES | 4416s |
| 14 | `$is_externConstructorPrototype` | 878 | YES | YES | 3192s |
| 15 | `$is_externMethodPrototype` | 881 | YES | YES | 3227s |
| 16 | `$callableId_prime` | 882 | YES | YES | 4500s |
| 17 | `$callableId` | 883 | YES | YES | 4540s |
| 18 | `$constructorId_of_externConstructorPrototype` | 884 | YES | YES | 4622s |
| 19 | `$expressionNonBrace_as_expression` | 885 | TIMEOUT | YES | 5001s |
| 20 | `$constructorId` | 887 | YES | YES | 4719s |
| 21 | `$callableId_of_externMethodPrototype` | 889 | YES | YES | 4769s |
| 22 | `$optional_annotation_of_parameterIR_prime` | 893 | YES | MAYBE | 4706s |
| 23 | `$optional_annotation_of_parameterIR` | 895 | YES | MAYBE | 4808s |
| 24 | `$is_optional_parameterIR` | 896 | YES | MAYBE | 4835s |
| 25 | `$flatten_forInitStatementList` | 905 | YES | YES | 5598s |
| 26 | `$split_externConstructorOrMethodPrototypeList` | 942 | YES | MAYBE | 14734s |
| 27 | `$flatten_parserStateList` | 1029 | YES | MAYBE | 10134s |

**27개 요약**: CRC YES 21 / MAYBE 1 / TIMEOUT 5 · term(B) YES 16 / MAYBE 11 · 비종료·비합류 후보 0.
