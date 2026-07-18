# P4 structural CTRS — 검증 결과 (표)

CRC/ChC = Church-Rosser(합류성)/Coherence, term = 종료. 값 = YES / MAYBE / TIMEOUT / `-`(미도달).
측정 이력·방법·MAYBE/TIMEOUT 해석은 **[verification-notes.md](verification-notes.md)** 참조.

## 1. ≤500 fresh 재검증 — 비-YES 행 (2026-07-18 · fresh 덤프 · CRC + AProVE 직접)

현재 바이너리(HEAD `2f9f8cba`)로 153슬라이스 재번역 후 CRC 재계산 + AProVE 직접 종료 (`/tmp/fresh500`, 153/153).
요약 — CRC: YES 140 / MAYBE 5 / TIMEOUT 8 · ChC: YES 145 · term(AProVE 직접): YES 117 / MAYBE 11 / TIMEOUT 25.
아래는 **비-YES 37행만** (나머지 116행은 CRC·ChC·term 모두 YES). `term(모듈러B)`는 §2 표의 종전 모듈러-B 값.

| symbol | rules | CRC | ChC | crc_s | term(AProVE직접) | term(모듈러B) |
|---|---|---|---|---|---|---|
| `$join_text` | 13 | YES | YES | 127 | MAYBE | MAYBE |
| `$invalidate_headerUnion` | 91 | YES | YES | 254 | MAYBE | MAYBE |
| `$invalidate_value` | 91 | YES | YES | 230 | MAYBE | MAYBE |
| `$write_bits_from_value` | 107 | TIMEOUT | - | 2402 | YES | TIMEOUT |
| `$un_bnot` | 139 | YES | YES | 479 | MAYBE | YES |
| `$bin_ge` | 183 | YES | YES | 658 | MAYBE | YES |
| `$bin_le` | 183 | YES | YES | 664 | MAYBE | YES |
| `$bin_gt` | 184 | YES | YES | 671 | MAYBE | YES |
| `$bin_lt` | 184 | YES | YES | 674 | MAYBE | YES |
| `$int_of_integerValue` | 184 | YES | YES | 656 | MAYBE | YES |
| `$nat_of_integerValue` | 187 | YES | YES | 706 | MAYBE | YES |
| `$bin_minus` | 193 | YES | YES | 749 | TIMEOUT | YES |
| `$bin_mul` | 193 | YES | YES | 755 | TIMEOUT | YES |
| `$bin_plus` | 193 | YES | YES | 749 | TIMEOUT | YES |
| `$un_minus` | 197 | YES | YES | 821 | TIMEOUT | YES |
| `$bin_bxor` | 199 | YES | YES | 802 | TIMEOUT | YES |
| `$bin_concat` | 200 | TIMEOUT | - | 2402 | TIMEOUT | YES |
| `$set_priorities_of_tableEntryListIR_prime` | 200 | YES | YES | 890 | TIMEOUT | MAYBE |
| `$bin_satminus` | 201 | YES | YES | 911 | TIMEOUT | YES |
| `$bin_satplus` | 201 | YES | YES | 920 | TIMEOUT | YES |
| `$bin_shl` | 201 | TIMEOUT | - | 2402 | TIMEOUT | YES |
| `$bin_band` | 202 | YES | YES | 1013 | TIMEOUT | YES |
| `$bin_bor` | 202 | YES | YES | 1007 | TIMEOUT | YES |
| `$name_annotationToken` | 209 | YES | YES | 683 | MAYBE | YES |
| `$un_op` | 209 | YES | YES | 995 | TIMEOUT | YES |
| `$bin_shr` | 215 | TIMEOUT | - | 2403 | TIMEOUT | YES |
| `$set_priorities_of_tableEntryListIR` | 226 | YES | YES | 1137 | TIMEOUT | MAYBE |
| `$name_annotation_opt` | 256 | YES | YES | 1136 | TIMEOUT | YES |
| `$write_value_field_from_bits_prime` | 271 | MAYBE | YES | 1314 | TIMEOUT | YES |
| `$write_value_fields_from_bits_prime` | 271 | MAYBE | YES | 1337 | TIMEOUT | YES |
| `$write_value_from_bits_prime` | 271 | MAYBE | YES | 1328 | TIMEOUT | YES |
| `$write_values_from_bits_prime` | 271 | MAYBE | YES | 1314 | TIMEOUT | YES |
| `$write_value_from_bits` | 274 | MAYBE | YES | 1350 | TIMEOUT | YES |
| `$bitacc_range_op` | 283 | TIMEOUT | - | 2403 | TIMEOUT | TIMEOUT |
| `$bitacc_offset_op` | 285 | TIMEOUT | - | 2403 | TIMEOUT | TIMEOUT |
| `$bitacc_range_replace_op` | 391 | TIMEOUT | - | 2402 | TIMEOUT | YES |
| `$bitacc_offset_replace_op` | 394 | TIMEOUT | - | 2402 | TIMEOUT | TIMEOUT |

## 2. ≤500 종합 (153심볼 · term = 모듈러 B)

| symbol | rules | CRC | ChC | term | new-commit helpers |
|---|---|---|---|---|---|
| `$annotationList_of_parameterIR` | 1 | YES | YES | YES |  |
| `$ctk_of_typedExpressionIR` | 1 | YES | YES | YES |  |
| `$empty_map` | 1 | YES | YES | YES |  |
| `$empty_set` | 1 | YES | YES | YES |  |
| `$empty_tableContext` | 1 | YES | YES | YES |  |
| `$id_of_parameterIR` | 1 | YES | YES | YES |  |
| `$invalidate_header` | 1 | YES | YES | YES |  |
| `$parameterListIR_of_actionDef` | 1 | YES | YES | YES |  |
| `$parameterListIR_of_actionTypeDefIR` | 1 | YES | YES | YES |  |
| `$parameterListIR_of_constructorTypeDefIR` | 1 | YES | YES | YES |  |
| `$parameterListIR_of_controlApplyMethodDef` | 1 | YES | YES | YES |  |
| `$parameterListIR_of_controlApplyMethodTypeIR` | 1 | YES | YES | YES |  |
| `$parameterListIR_of_definedFunctionDef` | 1 | YES | YES | YES |  |
| `$parameterListIR_of_definedFunctionTypeDefIR` | 1 | YES | YES | YES |  |
| `$parameterListIR_of_externFunctionDef` | 1 | YES | YES | YES |  |
| `$parameterListIR_of_externFunctionTypeDefIR` | 1 | YES | YES | YES |  |
| `$parameterListIR_of_parserApplyMethodDef` | 1 | YES | YES | YES |  |
| `$parameterListIR_of_parserApplyMethodTypeIR` | 1 | YES | YES | YES |  |
| `$parameterListIR_of_tableApplyMethodDef` | 1 | YES | YES | YES |  |
| `$parameterListIR_of_tableApplyMethodTypeDefIR` | 1 | YES | YES | YES |  |
| `$tableEntryPriorityOptIR_of_tableEntryIR` | 1 | YES | YES | YES |  |
| `$type_of_typedExpressionIR` | 1 | YES | YES | YES |  |
| `$type_of_typedLvalueIR` | 1 | YES | YES | YES |  |
| `$empty_callableDefEnv` | 2 | YES | YES | YES |  |
| `$empty_callableTypeDefEnv` | 2 | YES | YES | YES |  |
| `$empty_constructorDefEnv` | 2 | YES | YES | YES |  |
| `$empty_constructorTypeDefEnv` | 2 | YES | YES | YES |  |
| `$empty_frame` | 2 | YES | YES | YES |  |
| `$empty_stateEnv` | 2 | YES | YES | YES |  |
| `$empty_store` | 2 | YES | YES | YES |  |
| `$empty_theta` | 2 | YES | YES | YES |  |
| `$empty_typeDefEnv` | 2 | YES | YES | YES |  |
| `$empty_typeFrame` | 2 | YES | YES | YES |  |
| `$flatten_constOpt` | 2 | YES | YES | YES |  |
| `$ite` | 2 | YES | YES | YES |  |
| `$callable_builtinMethod` | 3 | YES | YES | YES |  |
| `$constructorTypeDef_of_externConstructorPrototypeIR` | 3 | YES | YES | YES |  |
| `$constructor_of_externConstructorPrototypeIR` | 3 | YES | YES | YES |  |
| `$empty_constraint` | 3 | YES | YES | YES |  |
| `$instantiable_extern` | 3 | YES | YES | YES |  |
| `$is_lpm_key_prime` | 3 | YES | YES | YES |  |
| `$parameterListIR_of_externMethodDef` | 3 | YES | YES | YES |  |
| `$un_lnot` | 3 | YES | YES | YES |  |
| `$flatten_objectInitializerOptIR` | 4 | YES | YES | YES |  |
| `$is_concrete_extern_object_prime_prime` | 4 | YES | YES | YES |  |
| `$is_default_parameterIR` | 4 | YES | YES | YES |  |
| `$is_lpm_key` | 4 | YES | YES | YES |  |
| `$is_some` | 4 | YES | YES | YES |  |
| `$opt_as_seq` | 4 | YES | YES | YES |  |
| `$add_action_tbl` | 5 | YES | YES | YES |  |
| `$add_key_tbl` | 5 | YES | YES | YES |  |
| `$codom_map` | 5 | YES | YES | YES |  |
| `$dom_map` | 5 | YES | YES | YES |  |
| `$enter_path_i` | 5 | YES | YES | YES |  |
| `$join_tableEntryState` | 5 | YES | YES | YES |  |
| `$concat_text` | 6 | YES | YES | YES |  |
| `$empty_typingContext` | 6 | YES | YES | YES |  |
| `$exists` | 6 | YES | YES | YES |  |
| `$flatten_blockElementStatementList` | 6 | YES | YES | YES |  |
| `$flatten_controlLocalDeclarationList` | 6 | YES | YES | YES |  |
| `$flatten_externConstructorOrMethodPrototypeList` | 6 | YES | YES | YES |  |
| `$flatten_objectDeclarationList` | 6 | YES | YES | YES |  |
| `$flatten_parserLocalDeclarationList` | 6 | YES | YES | YES |  |
| `$flatten_parserStatementList` | 6 | YES | YES | YES |  |
| `$flatten_selectCaseList` | 6 | YES | YES | YES |  |
| `$flatten_switchCaseList` | 6 | YES | YES | YES |  |
| `$flatten_tableActionList` | 6 | YES | YES | YES |  |
| `$flatten_tableEntryList` | 6 | YES | YES | YES |  |
| `$flatten_tableKeyList` | 6 | YES | YES | YES |  |
| `$flatten_tablePropertyList` | 6 | YES | YES | YES |  |
| `$flatten_typeFieldList` | 6 | YES | YES | YES |  |
| `$forall` | 6 | YES | YES | YES |  |
| `$is_tableActionsProperty` | 6 | YES | YES | YES |  |
| `$is_tableKeysProperty` | 6 | YES | YES | YES |  |
| `$join_flow` | 6 | YES | YES | YES |  |
| `$parameterListIR_of_externMethodTypeDefIR` | 6 | YES | YES | YES |  |
| `$tableCustomName` | 6 | YES | YES | YES |  |
| `$type_of_externMethodPrototypeIR` | 6 | YES | YES | YES |  |
| `$enter_i` | 7 | YES | YES | YES |  |
| `$enter_t` | 7 | YES | YES | YES |  |
| `$exit_i` | 7 | YES | YES | YES |  |
| `$exit_t` | 7 | YES | YES | YES |  |
| `$filter` | 7 | YES | YES | YES |  |
| `$requires_priority_prime` | 7 | YES | YES | YES |  |
| `$set_priority_of_tableEntryIR` | 7 | YES | YES | YES |  |
| `$empty_instContext` | 8 | YES | YES | YES |  |
| `$flatten_prefixedNameIR` | 8 | YES | YES | YES |  |
| `$requires_priority` | 8 | YES | YES | YES |  |
| `$width_of_integerTypeIR` | 8 | YES | YES | YES |  |
| `$join_text` | 9 | YES | YES | MAYBE |  |
| `$un_plus` | 9 | YES | YES | YES |  |
| `$inherit_i` | 10 | YES | YES | YES |  |
| `$name` | 10 | YES | YES | YES |  |
| `$callableId_IR` | 11 | YES | YES | YES |  |
| `$flatten_p4program` | 11 | YES | YES | YES |  |
| `$objectId_ends_with` | 11 | YES | YES | YES |  |
| `$callableId_of_externConstructorPrototypeIR` | 12 | YES | YES | YES |  |
| `$resolve_constraint` | 13 | YES | YES | YES |  |
| `$prefixedTypeName` | 14 | YES | YES | YES |  |
| `$join_ctk` | 15 | YES | YES | YES |  |
| `$starts_with` | 15 | YES | YES | YES |  |
| `$is_tableDefaultActionProperty` | 16 | YES | YES | YES |  |
| `$callableId_of_externMethodPrototypeIR` | 17 | YES | YES | YES |  |
| `$ends_with` | 20 | YES | YES | YES |  |
| `$optional_annotation_of_parameterIR_prime_prime` | 20 | YES | YES | YES |  |
| `$strip_prefix_rec` | 20 | YES | YES | YES |  |
| `$strip_suffix_rec` | 23 | YES | YES | YES |  |
| `$flatten_nameList` | 24 | YES | YES | YES |  |
| `$flatten_typeParameterList` | 24 | YES | YES | YES |  |
| `$flatten_typeParameterListOpt` | 28 | YES | YES | YES |  |
| `$prefixedNonTypeName` | 28 | YES | YES | YES |  |
| `$typedLvalueIR_as_typedExpressionIR` | 36 | YES | YES | YES |  |
| `$isValid_header` | 78 | YES | YES | YES |  |
| `$invalidate_headerUnion` | 89 | YES | YES | MAYBE |  |
| `$invalidate_value` | 89 | YES | YES | MAYBE |  |
| `$lvalue_as_expression` | 97 | YES | YES | YES |  |
| `$write_bits_from_value` | 105 | TIMEOUT | - | TIMEOUT |  |
| `$bin_mod` | 109 | YES | YES | YES | bsub bmod negate-int |
| `$bin_div` | 113 | YES | YES | YES | bsub bdiv negate-int |
| `$un_bnot` | 138 | YES | YES | YES | badd bmul bsub bpow-nat bneg negate-int |
| `$assignop_as_binop` | 171 | YES | YES | YES |  |
| `$bin_ge` | 184 | YES | YES | YES | badd bmul bsub bdiv negate-int |
| `$bin_gt` | 184 | YES | YES | YES | badd bmul bsub bdiv negate-int |
| `$bin_le` | 184 | YES | YES | YES | badd bmul bsub bdiv negate-int |
| `$bin_lt` | 184 | YES | YES | YES | badd bmul bsub bdiv negate-int |
| `$int_of_integerValue` | 189 | YES | YES | YES | badd bmul bsub bdiv negate-int |
| `$bin_minus` | 194 | YES | YES | YES | badd bmul bsub bdiv bmod negate-int |
| `$bin_mul` | 194 | YES | YES | YES | badd bmul bsub bdiv bmod negate-int |
| `$bin_plus` | 194 | YES | YES | YES | badd bmul bsub bdiv bmod negate-int |
| `$un_minus` | 197 | YES | YES | YES | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$nat_of_integerValue` | 198 | YES | YES | YES | badd bmul bsub bdiv negate-int nat-of-int |
| `$bin_bxor` | 200 | YES | YES | YES | badd bmul bsub bdiv bmod negate-int bxor |
| `$bin_concat` | 202 | TIMEOUT | - | YES | badd bmul bsub bdiv bmod negate-int |
| `$bin_satminus` | 202 | YES | YES | YES | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$bin_satplus` | 202 | MAYBE | YES | YES | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$bin_shl` | 202 | TIMEOUT | - | YES | badd bmul bsub bdiv bmod negate-int |
| `$bin_band` | 203 | YES | YES | YES | badd bmul bsub bdiv bmod negate-int band |
| `$bin_bor` | 203 | YES | YES | YES | badd bmul bsub bdiv bmod negate-int bor |
| `$name_annotationToken` | 209 | YES | YES | YES | bsub bdiv bmod |
| `$un_op` | 209 | YES | YES | YES | badd bmul bsub bdiv bmod bpow-nat bneg negate-int |
| `$set_priorities_of_tableEntryListIR_prime` | 214 | YES | YES | MAYBE | badd bmul bsub bdiv negate-int nat-of-int |
| `$bin_shr` | 217 | TIMEOUT | - | YES | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$set_priorities_of_tableEntryListIR` | 243 | YES | YES | MAYBE | badd bmul bsub bdiv negate-int nat-of-int |
| `$name_annotation_opt` | 249 | YES | YES | YES | bsub bdiv bmod |
| `$write_value_field_from_bits_prime` | 278 | MAYBE | YES | YES | badd bmul bsub bmod negate-int |
| `$write_value_fields_from_bits_prime` | 278 | MAYBE | YES | YES | badd bmul bsub bmod negate-int |
| `$write_value_from_bits_prime` | 278 | MAYBE | YES | YES | badd bmul bsub bmod negate-int |
| `$write_values_from_bits_prime` | 278 | MAYBE | YES | YES | badd bmul bsub bmod negate-int |
| `$write_value_from_bits` | 281 | MAYBE | YES | YES | badd bmul bsub bmod negate-int |
| `$bitacc_range_op` | 294 | TIMEOUT | - | TIMEOUT | badd bmul bsub bdiv bmod negate-int nat-of-int band |
| `$bitacc_offset_op` | 296 | TIMEOUT | - | TIMEOUT | badd bmul bsub bdiv bmod negate-int nat-of-int band |
| `$bitacc_range_replace_op` | 396 | TIMEOUT | - | YES | badd bmul bsub bdiv negate-int |
| `$bitacc_offset_replace_op` | 405 | TIMEOUT | - | TIMEOUT | badd bmul bsub bdiv negate-int nat-of-int |

## 3. >500 슬라이스 (bigsweep · term = 모듈러 B · 27/127 확정)

| # | symbol | rules | CRC | ChC | term(B) | 종료(term) 시간 |
|---|---|---|---|---|---|---|
| 1 | `$flatten_namedExpressionList` | 748 | YES | YES | MAYBE | 2061s |
| 2 | `$expression_as_lvalue` | 764 | MAYBE | YES | YES | 4352s |
| 3 | `$flatten_realTypeArgumentList` | 767 | YES | YES | MAYBE | 2154s |
| 4 | `$flatten_typeArgumentList` | 777 | TIMEOUT | - | MAYBE | 2115s |
| 5 | `$flatten_expressionList` | 780 | TIMEOUT | - | MAYBE | 2142s |
| 6 | `$flatten_argumentList` | 782 | TIMEOUT | - | MAYBE | 2172s |
| 7 | `$flatten_forUpdateStatementList` | 788 | YES | YES | YES | 3413s |
| 8 | `$is_singleton_list_expression` | 810 | YES | YES | YES | 2148s |
| 9 | `$flatten_simpleKeysetExpressionList` | 823 | TIMEOUT | - | MAYBE | 2387s |
| 10 | `$flatten_annotationList` | 866 | YES | YES | YES | 3377s |
| 11 | `$add_annotationList` | 867 | YES | YES | YES | 6261s |
| 12 | `$flatten_parameterList` | 872 | YES | YES | YES | 4345s |
| 13 | `$flatten_constructorParameterListOpt` | 876 | YES | YES | YES | 4416s |
| 14 | `$is_externConstructorPrototype` | 878 | YES | YES | YES | 3192s |
| 15 | `$is_externMethodPrototype` | 881 | YES | YES | YES | 3227s |
| 16 | `$callableId_prime` | 882 | YES | YES | YES | 4500s |
| 17 | `$callableId` | 883 | YES | YES | YES | 4540s |
| 18 | `$constructorId_of_externConstructorPrototype` | 884 | YES | YES | YES | 4622s |
| 19 | `$expressionNonBrace_as_expression` | 885 | TIMEOUT | - | YES | 5001s |
| 20 | `$constructorId` | 887 | YES | YES | YES | 4719s |
| 21 | `$callableId_of_externMethodPrototype` | 889 | YES | YES | YES | 4769s |
| 22 | `$optional_annotation_of_parameterIR_prime` | 893 | YES | YES | MAYBE | 4706s |
| 23 | `$optional_annotation_of_parameterIR` | 895 | YES | YES | MAYBE | 4808s |
| 24 | `$is_optional_parameterIR` | 896 | YES | YES | MAYBE | 4835s |
| 25 | `$flatten_forInitStatementList` | 905 | YES | YES | YES | 5598s |
| 26 | `$split_externConstructorOrMethodPrototypeList` | 942 | YES | YES | MAYBE | 14734s |
| 27 | `$flatten_parserStateList` | 1029 | YES | YES | MAYBE | 10134s |

**27개 요약**: CRC YES 21 / MAYBE 1 / TIMEOUT 5 · term(B) YES 16 / MAYBE 11 · 비종료·비합류 후보 0.
