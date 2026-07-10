# P4 structural CTRS — CRC(합류성) + termination(모듈러 B) 결과

종합 스윕: ≤500규칙 153심볼. CRC/ChC = Church-Rosser/Coherence, term = 모듈러(B) 종료.

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
| `$concat_text` | 6 | YES | YES | MAYBE |  |
| `$empty_typingContext` | 6 | YES | YES | YES |  |
| `$exists` | 6 | YES | YES | MAYBE |  |
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
| `$forall` | 6 | YES | YES | MAYBE |  |
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
| `$filter` | 7 | YES | YES | MAYBE |  |
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
| `$flatten_p4program` | 11 | YES | YES | MAYBE |  |
| `$objectId_ends_with` | 11 | YES | YES | YES |  |
| `$callableId_of_externConstructorPrototypeIR` | 12 | YES | YES | YES |  |
| `$resolve_constraint` | 13 | YES | YES | YES |  |
| `$prefixedTypeName` | 14 | YES | YES | YES |  |
| `$join_ctk` | 15 | MAYBE | YES | YES |  |
| `$starts_with` | 15 | YES | YES | YES |  |
| `$is_tableDefaultActionProperty` | 16 | YES | YES | YES |  |
| `$callableId_of_externMethodPrototypeIR` | 17 | YES | YES | YES |  |
| `$ends_with` | 20 | YES | YES | YES |  |
| `$optional_annotation_of_parameterIR_prime_prime` | 20 | YES | YES | YES |  |
| `$strip_prefix_rec` | 20 | YES | YES | YES |  |
| `$strip_suffix_rec` | 23 | YES | YES | YES |  |
| `$flatten_nameList` | 24 | YES | YES | MAYBE |  |
| `$flatten_typeParameterList` | 24 | YES | YES | MAYBE |  |
| `$flatten_typeParameterListOpt` | 28 | YES | YES | MAYBE |  |
| `$prefixedNonTypeName` | 28 | YES | YES | YES |  |
| `$typedLvalueIR_as_typedExpressionIR` | 36 | YES | YES | YES |  |
| `$isValid_header` | 78 | YES | YES | YES |  |
| `$invalidate_headerUnion` | 89 | YES | YES | MAYBE |  |
| `$invalidate_value` | 89 | YES | YES | MAYBE |  |
| `$lvalue_as_expression` | 97 | YES | YES | MAYBE |  |
| `$write_bits_from_value` | 105 | TIMEOUT | - | TIMEOUT |  |
| `$bin_mod` | 109 | YES | YES | YES | bsub bmod negate-int |
| `$bin_div` | 113 | YES | YES | YES | bsub bdiv negate-int |
| `$un_bnot` | 138 | YES | YES | YES | badd bmul bsub bpow-nat bneg negate-int |
| `$assignop_as_binop` | 171 | MAYBE | YES | YES |  |
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
| `$write_value_field_from_bits_prime` | 278 | MAYBE | YES | TIMEOUT | badd bmul bsub bmod negate-int |
| `$write_value_fields_from_bits_prime` | 278 | MAYBE | YES | MAYBE | badd bmul bsub bmod negate-int |
| `$write_value_from_bits_prime` | 278 | MAYBE | YES | MAYBE | badd bmul bsub bmod negate-int |
| `$write_values_from_bits_prime` | 278 | MAYBE | YES | MAYBE | badd bmul bsub bmod negate-int |
| `$write_value_from_bits` | 281 | MAYBE | YES | MAYBE | badd bmul bsub bmod negate-int |
| `$bitacc_range_op` | 294 | TIMEOUT | - | TIMEOUT | badd bmul bsub bdiv bmod negate-int nat-of-int band |
| `$bitacc_offset_op` | 296 | TIMEOUT | - | TIMEOUT | badd bmul bsub bdiv bmod negate-int nat-of-int band |
| `$bitacc_range_replace_op` | 396 | TIMEOUT | - | YES | badd bmul bsub bdiv negate-int |
| `$bitacc_offset_replace_op` | 405 | TIMEOUT | - | TIMEOUT | badd bmul bsub bdiv negate-int nat-of-int |
