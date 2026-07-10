# P4 structural CTRS — CRC(합류성) + termination(모듈러 B) 결과

종합 스윕: ≤500규칙 153심볼. CRC/ChC = Church-Rosser/Coherence, term = 모듈러(B) 종료.
MAYBE·TIMEOUT = 미증명(도구 한계)이지 결함 아님 — 유일한 실제 결함은 `$write_value*`(5)
integerValue.V n_var=0 non-confluence. 해석 기준은 [CLAUDE.md](CLAUDE.md) 참고.

| symbol | rules | owise | CRC | ChC | crc_s | term | term_s | pruned ops/sorts | new-commit helpers |
|---|---|---|---|---|---|---|---|---|---|
| `$annotationList_of_parameterIR` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$ctk_of_typedExpressionIR` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$empty_map` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$empty_set` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$empty_tableContext` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$id_of_parameterIR` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$invalidate_header` | 1 | 0 | YES | YES | 101 | YES | 2 | 1/1 |  |
| `$parameterListIR_of_actionDef` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$parameterListIR_of_actionTypeDefIR` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$parameterListIR_of_constructorTypeDefIR` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$parameterListIR_of_controlApplyMethodDef` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$parameterListIR_of_controlApplyMethodTypeIR` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$parameterListIR_of_definedFunctionDef` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$parameterListIR_of_definedFunctionTypeDefIR` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$parameterListIR_of_externFunctionDef` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$parameterListIR_of_externFunctionTypeDefIR` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$parameterListIR_of_parserApplyMethodDef` | 1 | 0 | YES | YES | 101 | YES | 2 | 1/1 |  |
| `$parameterListIR_of_parserApplyMethodTypeIR` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$parameterListIR_of_tableApplyMethodDef` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$parameterListIR_of_tableApplyMethodTypeDefIR` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$tableEntryPriorityOptIR_of_tableEntryIR` | 1 | 0 | YES | YES | 101 | YES | 2 | 1/1 |  |
| `$type_of_typedExpressionIR` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$type_of_typedLvalueIR` | 1 | 0 | YES | YES | 100 | YES | 2 | 1/1 |  |
| `$empty_callableDefEnv` | 2 | 0 | YES | YES | 100 | YES | 2 | 2/2 |  |
| `$empty_callableTypeDefEnv` | 2 | 0 | YES | YES | 101 | YES | 2 | 2/2 |  |
| `$empty_constructorDefEnv` | 2 | 0 | YES | YES | 100 | YES | 1 | 2/2 |  |
| `$empty_constructorTypeDefEnv` | 2 | 0 | YES | YES | 101 | YES | 2 | 2/2 |  |
| `$empty_frame` | 2 | 0 | YES | YES | 100 | YES | 2 | 2/2 |  |
| `$empty_stateEnv` | 2 | 0 | YES | YES | 100 | YES | 2 | 2/2 |  |
| `$empty_store` | 2 | 0 | YES | YES | 101 | YES | 2 | 2/2 |  |
| `$empty_theta` | 2 | 0 | YES | YES | 100 | YES | 2 | 2/2 |  |
| `$empty_typeDefEnv` | 2 | 0 | YES | YES | 100 | YES | 2 | 2/2 |  |
| `$empty_typeFrame` | 2 | 0 | YES | YES | 101 | YES | 2 | 2/2 |  |
| `$flatten_constOpt` | 2 | 0 | YES | YES | 100 | YES | 2 | 2/2 |  |
| `$ite` | 2 | 0 | YES | YES | 130 | YES | 2 | 2/2 |  |
| `$callable_builtinMethod` | 3 | 0 | YES | YES | 100 | YES | 2 | 3/3 |  |
| `$constructorTypeDef_of_externConstructorPrototypeIR` | 3 | 0 | YES | YES | 100 | YES | 3 | 3/3 |  |
| `$constructor_of_externConstructorPrototypeIR` | 3 | 0 | YES | YES | 100 | YES | 4 | 3/3 |  |
| `$empty_constraint` | 3 | 0 | YES | YES | 100 | YES | 2 | 3/3 |  |
| `$instantiable_extern` | 3 | 0 | YES | YES | 100 | YES | 2 | 3/3 |  |
| `$is_lpm_key_prime` | 3 | 0 | YES | YES | 103 | YES | 3 | 3/3 |  |
| `$parameterListIR_of_externMethodDef` | 3 | 0 | YES | YES | 101 | YES | 2 | 3/3 |  |
| `$un_lnot` | 3 | 0 | YES | YES | 100 | YES | 2 | 3/3 |  |
| `$flatten_objectInitializerOptIR` | 4 | 0 | YES | YES | 103 | YES | 2 | 4/4 |  |
| `$is_concrete_extern_object_prime_prime` | 4 | 0 | YES | YES | 100 | YES | 3 | 4/4 |  |
| `$is_default_parameterIR` | 4 | 0 | YES | YES | 107 | YES | 2 | 4/4 |  |
| `$is_lpm_key` | 4 | 0 | YES | YES | 107 | YES | 3 | 4/4 |  |
| `$is_some` | 4 | 0 | YES | YES | 104 | YES | 2 | 4/4 |  |
| `$opt_as_seq` | 4 | 0 | YES | YES | 103 | YES | 2 | 4/4 |  |
| `$add_action_tbl` | 5 | 0 | YES | YES | 104 | YES | 2 | 5/5 |  |
| `$add_key_tbl` | 5 | 0 | YES | YES | 109 | YES | 2 | 5/5 |  |
| `$codom_map` | 5 | 0 | YES | YES | 143 | YES | 2 | 5/5 |  |
| `$dom_map` | 5 | 0 | YES | YES | 139 | YES | 3 | 5/5 |  |
| `$enter_path_i` | 5 | 0 | YES | YES | 103 | YES | 2 | 5/5 |  |
| `$join_tableEntryState` | 5 | 0 | YES | YES | 103 | YES | 3 | 5/5 |  |
| `$concat_text` | 6 | 0 | YES | YES | 103 | MAYBE | 124 | 6/6 |  |
| `$empty_typingContext` | 6 | 0 | YES | YES | 103 | YES | 2 | 6/6 |  |
| `$exists` | 6 | 0 | YES | YES | 103 | MAYBE | 122 | 6/6 |  |
| `$flatten_blockElementStatementList` | 6 | 0 | YES | YES | 103 | YES | 3 | 6/6 |  |
| `$flatten_controlLocalDeclarationList` | 6 | 0 | YES | YES | 103 | YES | 3 | 6/6 |  |
| `$flatten_externConstructorOrMethodPrototypeList` | 6 | 0 | YES | YES | 103 | YES | 3 | 6/6 |  |
| `$flatten_objectDeclarationList` | 6 | 0 | YES | YES | 104 | YES | 2 | 6/6 |  |
| `$flatten_parserLocalDeclarationList` | 6 | 0 | YES | YES | 103 | YES | 3 | 6/6 |  |
| `$flatten_parserStatementList` | 6 | 0 | YES | YES | 103 | YES | 3 | 6/6 |  |
| `$flatten_selectCaseList` | 6 | 0 | YES | YES | 103 | YES | 3 | 6/6 |  |
| `$flatten_switchCaseList` | 6 | 0 | YES | YES | 107 | YES | 2 | 6/6 |  |
| `$flatten_tableActionList` | 6 | 0 | YES | YES | 104 | YES | 2 | 6/6 |  |
| `$flatten_tableEntryList` | 6 | 0 | YES | YES | 103 | YES | 3 | 6/6 |  |
| `$flatten_tableKeyList` | 6 | 0 | YES | YES | 103 | YES | 3 | 6/6 |  |
| `$flatten_tablePropertyList` | 6 | 0 | YES | YES | 103 | YES | 3 | 6/6 |  |
| `$flatten_typeFieldList` | 6 | 0 | YES | YES | 103 | YES | 3 | 6/6 |  |
| `$forall` | 6 | 0 | YES | YES | 103 | MAYBE | 126 | 6/6 |  |
| `$is_tableActionsProperty` | 6 | 0 | YES | YES | 100 | YES | 2 | 6/6 |  |
| `$is_tableKeysProperty` | 6 | 0 | YES | YES | 103 | YES | 3 | 6/6 |  |
| `$join_flow` | 6 | 0 | YES | YES | 103 | YES | 2 | 6/6 |  |
| `$parameterListIR_of_externMethodTypeDefIR` | 6 | 0 | YES | YES | 103 | YES | 3 | 6/6 |  |
| `$tableCustomName` | 6 | 0 | YES | YES | 104 | YES | 2 | 6/6 |  |
| `$type_of_externMethodPrototypeIR` | 6 | 0 | YES | YES | 103 | YES | 3 | 6/6 |  |
| `$enter_i` | 7 | 0 | YES | YES | 104 | YES | 2 | 7/7 |  |
| `$enter_t` | 7 | 0 | YES | YES | 103 | YES | 2 | 7/7 |  |
| `$exit_i` | 7 | 0 | YES | YES | 103 | YES | 3 | 7/7 |  |
| `$exit_t` | 7 | 0 | YES | YES | 103 | YES | 3 | 7/7 |  |
| `$filter` | 7 | 0 | YES | YES | 104 | MAYBE | 126 | 7/7 |  |
| `$requires_priority_prime` | 7 | 0 | YES | YES | 106 | YES | 3 | 7/7 |  |
| `$set_priority_of_tableEntryIR` | 7 | 0 | YES | YES | 103 | YES | 2 | 1/7 |  |
| `$empty_instContext` | 8 | 0 | YES | YES | 103 | YES | 2 | 8/8 |  |
| `$flatten_prefixedNameIR` | 8 | 0 | YES | YES | 103 | YES | 3 | 8/8 |  |
| `$requires_priority` | 8 | 0 | YES | YES | 106 | YES | 4 | 8/8 |  |
| `$width_of_integerTypeIR` | 8 | 0 | YES | YES | 103 | YES | 2 | 4/8 |  |
| `$join_text` | 9 | 0 | YES | YES | 107 | MAYBE | 129 | 9/9 |  |
| `$un_plus` | 9 | 0 | YES | YES | 103 | YES | 3 | 5/9 |  |
| `$inherit_i` | 10 | 0 | YES | YES | 103 | YES | 2 | 10/10 |  |
| `$name` | 10 | 0 | YES | YES | 104 | YES | 1 | 10/10 |  |
| `$callableId_IR` | 11 | 0 | YES | YES | 103 | YES | 5 | 11/11 |  |
| `$flatten_p4program` | 11 | 0 | YES | YES | 103 | MAYBE | 126 | 11/11 |  |
| `$objectId_ends_with` | 11 | 0 | YES | YES | 103 | YES | 3 | 11/11 |  |
| `$callableId_of_externConstructorPrototypeIR` | 12 | 0 | YES | YES | 103 | YES | 5 | 12/12 |  |
| `$resolve_constraint` | 13 | 0 | YES | YES | 164 | YES | 6 | 13/13 |  |
| `$prefixedTypeName` | 14 | 0 | YES | YES | 107 | YES | 3 | 14/14 |  |
| `$join_ctk` | 15 | 0 | MAYBE | YES | 103 | YES | 3 | 15/15 |  |
| `$starts_with` | 15 | 0 | YES | YES | 106 | YES | 6 | 15/15 |  |
| `$is_tableDefaultActionProperty` | 16 | 0 | YES | YES | 109 | YES | 5 | 16/16 |  |
| `$callableId_of_externMethodPrototypeIR` | 17 | 0 | YES | YES | 106 | YES | 8 | 17/17 |  |
| `$ends_with` | 20 | 0 | YES | YES | 113 | YES | 9 | 20/20 |  |
| `$optional_annotation_of_parameterIR_prime_prime` | 20 | 0 | YES | YES | 109 | YES | 5 | 20/20 |  |
| `$strip_prefix_rec` | 20 | 0 | YES | YES | 138 | YES | 8 | 20/20 |  |
| `$strip_suffix_rec` | 23 | 0 | YES | YES | 143 | YES | 12 | 23/23 |  |
| `$flatten_nameList` | 24 | 0 | YES | YES | 109 | MAYBE | 126 | 24/24 |  |
| `$flatten_typeParameterList` | 24 | 0 | YES | YES | 109 | MAYBE | 129 | 24/24 |  |
| `$flatten_typeParameterListOpt` | 28 | 0 | YES | YES | 115 | MAYBE | 131 | 28/28 |  |
| `$prefixedNonTypeName` | 28 | 0 | YES | YES | 115 | YES | 3 | 28/28 |  |
| `$typedLvalueIR_as_typedExpressionIR` | 36 | 0 | YES | YES | 121 | YES | 18 | 36/36 |  |
| `$isValid_header` | 78 | 0 | YES | YES | 163 | YES | 53 | 74/78 |  |
| `$invalidate_headerUnion` | 89 | 0 | YES | YES | 200 | MAYBE | 332 | 85/89 |  |
| `$invalidate_value` | 89 | 0 | YES | YES | 199 | MAYBE | 330 | 85/89 |  |
| `$lvalue_as_expression` | 97 | 0 | YES | YES | 208 | MAYBE | 144 | 97/97 |  |
| `$write_bits_from_value` | 105 | 0 | TIMEOUT | - | 1802 | TIMEOUT | 700 | 101/105 |  |
| `$bin_mod` | 109 | 0 | YES | YES | 303 | YES | 2 | 1/109 | bsub bmod negate-int |
| `$bin_div` | 113 | 0 | YES | YES | 313 | YES | 2 | 5/113 | bsub bdiv negate-int |
| `$un_bnot` | 138 | 0 | YES | YES | 418 | YES | 5 | 7/138 | badd bmul bsub bpow-nat bneg negate-int |
| `$assignop_as_binop` | 171 | 0 | MAYBE | YES | 283 | YES | 7 | 171/171 |  |
| `$bin_ge` | 184 | 0 | YES | YES | 726 | YES | 3 | 9/184 | badd bmul bsub bdiv negate-int |
| `$bin_gt` | 184 | 0 | YES | YES | 721 | YES | 4 | 9/184 | badd bmul bsub bdiv negate-int |
| `$bin_le` | 184 | 0 | YES | YES | 718 | YES | 3 | 9/184 | badd bmul bsub bdiv negate-int |
| `$bin_lt` | 184 | 0 | YES | YES | 715 | YES | 4 | 9/184 | badd bmul bsub bdiv negate-int |
| `$int_of_integerValue` | 189 | 0 | YES | YES | 695 | YES | 3 | 14/189 | badd bmul bsub bdiv negate-int |
| `$bin_minus` | 194 | 0 | YES | YES | 839 | YES | 6 | 9/194 | badd bmul bsub bdiv bmod negate-int |
| `$bin_mul` | 194 | 0 | YES | YES | 828 | YES | 6 | 9/194 | badd bmul bsub bdiv bmod negate-int |
| `$bin_plus` | 194 | 0 | YES | YES | 859 | YES | 6 | 9/194 | badd bmul bsub bdiv bmod negate-int |
| `$un_minus` | 197 | 0 | YES | YES | 851 | YES | 6 | 10/197 | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$nat_of_integerValue` | 198 | 0 | YES | YES | 769 | YES | 6 | 18/198 | badd bmul bsub bdiv negate-int nat-of-int |
| `$bin_bxor` | 200 | 0 | YES | YES | 797 | YES | 6 | 12/200 | badd bmul bsub bdiv bmod negate-int bxor |
| `$bin_concat` | 202 | 0 | TIMEOUT | - | 1802 | YES | 30 | 17/202 | badd bmul bsub bdiv bmod negate-int |
| `$bin_satminus` | 202 | 0 | YES | YES | 881 | YES | 21 | 15/202 | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$bin_satplus` | 202 | 0 | MAYBE | YES | 893 | YES | 21 | 15/202 | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$bin_shl` | 202 | 0 | TIMEOUT | - | 1802 | YES | 16 | 17/202 | badd bmul bsub bdiv bmod negate-int |
| `$bin_band` | 203 | 0 | YES | YES | 1049 | YES | 8 | 12/203 | badd bmul bsub bdiv bmod negate-int band |
| `$bin_bor` | 203 | 0 | YES | YES | 1040 | YES | 11 | 15/203 | badd bmul bsub bdiv bmod negate-int bor |
| `$name_annotationToken` | 209 | 0 | YES | YES | 686 | YES | 51 | 109/209 | bsub bdiv bmod |
| `$un_op` | 209 | 0 | YES | YES | 944 | YES | 12 | 22/209 | badd bmul bsub bdiv bmod bpow-nat bneg negate-int |
| `$set_priorities_of_tableEntryListIR_prime` | 214 | 0 | YES | YES | 1025 | MAYBE | 156 | 34/214 | badd bmul bsub bdiv negate-int nat-of-int |
| `$bin_shr` | 217 | 0 | TIMEOUT | - | 1802 | YES | 105 | 30/217 | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$set_priorities_of_tableEntryListIR` | 243 | 0 | YES | YES | 1308 | MAYBE | 525 | 63/243 | badd bmul bsub bdiv negate-int nat-of-int |
| `$name_annotation_opt` | 249 | 0 | YES | YES | 1031 | YES | 402 | 149/249 | bsub bdiv bmod |
| `$write_value_field_from_bits_prime` | 278 | 0 | MAYBE | YES | 1319 | TIMEOUT | 700 | 104/278 | badd bmul bsub bmod negate-int |
| `$write_value_fields_from_bits_prime` | 278 | 0 | MAYBE | YES | 1319 | MAYBE | 696 | 104/278 | badd bmul bsub bmod negate-int |
| `$write_value_from_bits_prime` | 278 | 0 | MAYBE | YES | 1316 | MAYBE | 637 | 104/278 | badd bmul bsub bmod negate-int |
| `$write_values_from_bits_prime` | 278 | 0 | MAYBE | YES | 1232 | MAYBE | 643 | 104/278 | badd bmul bsub bmod negate-int |
| `$write_value_from_bits` | 281 | 0 | MAYBE | YES | 1283 | MAYBE | 666 | 107/281 | badd bmul bsub bmod negate-int |
| `$bitacc_range_op` | 294 | 0 | TIMEOUT | - | 1802 | TIMEOUT | 700 | 104/294 | badd bmul bsub bdiv bmod negate-int nat-of-int band |
| `$bitacc_offset_op` | 296 | 0 | TIMEOUT | - | 1801 | TIMEOUT | 700 | 106/296 | badd bmul bsub bdiv bmod negate-int nat-of-int band |
| `$bitacc_range_replace_op` | 396 | 0 | TIMEOUT | - | 1802 | YES | 499 | 221/396 | badd bmul bsub bdiv negate-int |
| `$bitacc_offset_replace_op` | 405 | 0 | TIMEOUT | - | 1801 | TIMEOUT | 700 | 225/405 | badd bmul bsub bdiv negate-int nat-of-int |
