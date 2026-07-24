# P4 structural CTRS — 검증 결과 (표)

## 종합

| 축 | YES | YES* | NO | MAYBE | TIMEOUT | DEGENERATE | `-` | 계 | 합계 초 |
|---|---|---|---|---|---|---|---|---|---|
| confluence | 162 | 5 | 0 | 1 | 12 | — | 135 | 315 | — |
| termination | 306 | — | 0 | — | — | 9 | — | 315 | 8360.8 |

| # | symbol | rules | confluence | termination |
|---|---|---|---|---|
| 1 | `$find_overloaded` | 0 | - | DEGENERATE |
| 2 | `$find_overloadeds_named` | 0 | - | DEGENERATE |
| 3 | `$find_overloadeds_unnamed` | 0 | - | DEGENERATE |
| 4 | `$init_objectState` | 0 | - | DEGENERATE |
| 5 | `$match_overloaded_named` | 0 | - | DEGENERATE |
| 6 | `$match_overloaded_unnamed` | 0 | - | DEGENERATE |
| 7 | `$reduce_serenum_binary` | 0 | - | DEGENERATE |
| 8 | `$reduce_serenum_unary` | 0 | - | DEGENERATE |
| 9 | `ExternFunctionCall_eval_lctk` | 0 | - | DEGENERATE |
| 10 | `$annotationList_of_parameterIR` | 1 | YES (0.1s) | YES (0.4s) |
| 11 | `$ctk_of_typedExpressionIR` | 1 | YES (0.1s) | YES (0.4s) |
| 12 | `$empty_map` | 1 | YES (0.1s) | YES (0.4s) |
| 13 | `$empty_set` | 1 | YES (0.1s) | YES (0.4s) |
| 14 | `$empty_tableContext` | 1 | YES (0.1s) | YES (0.4s) |
| 15 | `$id_of_parameterIR` | 1 | YES (0.1s) | YES (0.4s) |
| 16 | `$invalidate_header` | 1 | YES (0.1s) | YES (0.4s) |
| 17 | `$parameterListIR_of_actionDef` | 1 | YES (0.1s) | YES (0.4s) |
| 18 | `$parameterListIR_of_actionTypeDefIR` | 1 | YES (0.1s) | YES (0.4s) |
| 19 | `$parameterListIR_of_constructorTypeDefIR` | 1 | YES (0.1s) | YES (0.4s) |
| 20 | `$parameterListIR_of_controlApplyMethodDef` | 1 | YES (0.1s) | YES (0.4s) |
| 21 | `$parameterListIR_of_controlApplyMethodTypeIR` | 1 | YES (0.1s) | YES (0.4s) |
| 22 | `$parameterListIR_of_definedFunctionDef` | 1 | YES (0.1s) | YES (0.4s) |
| 23 | `$parameterListIR_of_definedFunctionTypeDefIR` | 1 | YES (0.1s) | YES (0.4s) |
| 24 | `$parameterListIR_of_externFunctionDef` | 1 | YES (0.1s) | YES (0.4s) |
| 25 | `$parameterListIR_of_externFunctionTypeDefIR` | 1 | YES (0.1s) | YES (0.4s) |
| 26 | `$parameterListIR_of_externMethodDef` | 1 | YES (0.1s) | YES (0.4s) |
| 27 | `$parameterListIR_of_parserApplyMethodDef` | 1 | YES (0.1s) | YES (0.4s) |
| 28 | `$parameterListIR_of_parserApplyMethodTypeIR` | 1 | YES (0.1s) | YES (0.4s) |
| 29 | `$parameterListIR_of_tableApplyMethodDef` | 1 | YES (0.1s) | YES (0.4s) |
| 30 | `$parameterListIR_of_tableApplyMethodTypeDefIR` | 1 | YES (0.1s) | YES (0.4s) |
| 31 | `$set_priority_of_tableEntryIR` | 1 | YES (0.1s) | YES (0.5s) |
| 32 | `$tableEntryPriorityOptIR_of_tableEntryIR` | 1 | YES (0.1s) | YES (0.4s) |
| 33 | `$type_of_typedExpressionIR` | 1 | YES (0.1s) | YES (0.4s) |
| 34 | `$type_of_typedLvalueIR` | 1 | YES (0.1s) | YES (0.4s) |
| 35 | `$empty_callableDefEnv` | 2 | YES (0.1s) | YES (0.4s) |
| 36 | `$empty_callableTypeDefEnv` | 2 | YES (0.1s) | YES (0.4s) |
| 37 | `$empty_constructorDefEnv` | 2 | YES (0.1s) | YES (0.4s) |
| 38 | `$empty_constructorTypeDefEnv` | 2 | YES (0.1s) | YES (0.4s) |
| 39 | `$empty_frame` | 2 | YES (0.1s) | YES (0.4s) |
| 40 | `$empty_stateEnv` | 2 | YES (0.1s) | YES (0.4s) |
| 41 | `$empty_store` | 2 | YES (0.1s) | YES (0.4s) |
| 42 | `$empty_theta` | 2 | YES (0.1s) | YES (0.4s) |
| 43 | `$empty_typeDefEnv` | 2 | YES (0.1s) | YES (0.4s) |
| 44 | `$empty_typeFrame` | 2 | YES (0.1s) | YES (0.4s) |
| 45 | `$flatten_constOpt` | 2 | YES (0.1s) | YES (0.4s) |
| 46 | `$flatten_objectInitializerOptIR` | 2 | YES (0.1s) | YES (0.4s) |
| 47 | `$is_some` | 2 | YES (0.1s) | YES (0.4s) |
| 48 | `$ite` | 2 | YES (0.1s) | YES (0.4s) |
| 49 | `$opt_as_seq` | 2 | YES (0.1s) | YES (0.4s) |
| 50 | `$parameterListIR_of_externMethodTypeDefIR` | 2 | YES (0.1s) | YES (0.4s) |
| 51 | `$type_of_externMethodPrototypeIR` | 2 | YES (0.1s) | YES (0.4s) |
| 52 | `$callable_builtinMethod` | 3 | YES (0.1s) | YES (0.4s) |
| 53 | `$constructorTypeDef_of_externConstructorPrototypeIR` | 3 | YES (0.1s) | YES (0.4s) |
| 54 | `$constructor_of_externConstructorPrototypeIR` | 3 | YES (0.1s) | YES (0.4s) |
| 55 | `$empty_constraint` | 3 | YES (0.1s) | YES (0.4s) |
| 56 | `$filter` | 3 | YES (0.1s) | YES (0.5s) |
| 57 | `$instantiable_extern` | 3 | YES (0.1s) | YES (0.4s) |
| 58 | `$is_lpm_key_prime` | 3 | YES (0.1s) | YES (0.4s) |
| 59 | `$join_tableEntryState` | 3 | YES (0.1s) | YES (0.4s) |
| 60 | `$un_lnot` | 3 | YES (0.1s) | YES (0.4s) |
| 61 | `$concat_text` | 4 | YES (0.1s) | YES (0.4s) |
| 62 | `$exists` | 4 | YES (0.1s) | YES (0.4s) |
| 63 | `$flatten_blockElementStatementList` | 4 | YES (0.1s) | YES (0.4s) |
| 64 | `$flatten_controlLocalDeclarationList` | 4 | YES (0.1s) | YES (0.4s) |
| 65 | `$flatten_externConstructorOrMethodPrototypeList` | 4 | YES (0.1s) | YES (0.4s) |
| 66 | `$flatten_objectDeclarationList` | 4 | YES (0.1s) | YES (0.4s) |
| 67 | `$flatten_parserLocalDeclarationList` | 4 | YES (0.1s) | YES (0.4s) |
| 68 | `$flatten_parserStatementList` | 4 | YES (0.1s) | YES (0.4s) |
| 69 | `$flatten_prefixedNameIR` | 4 | YES (0.1s) | YES (0.4s) |
| 70 | `$flatten_selectCaseList` | 4 | YES (0.1s) | YES (0.4s) |
| 71 | `$flatten_switchCaseList` | 4 | YES (0.1s) | YES (0.4s) |
| 72 | `$flatten_tableActionList` | 4 | YES (0.1s) | YES (0.4s) |
| 73 | `$flatten_tableEntryList` | 4 | YES (0.1s) | YES (0.4s) |
| 74 | `$flatten_tableKeyList` | 4 | YES (0.1s) | YES (0.4s) |
| 75 | `$flatten_tablePropertyList` | 4 | YES (0.1s) | YES (0.4s) |
| 76 | `$flatten_typeFieldList` | 4 | YES (0.1s) | YES (0.4s) |
| 77 | `$forall` | 4 | YES (0.1s) | YES (0.4s) |
| 78 | `$is_concrete_extern_object_prime_prime` | 4 | YES (0.1s) | YES (0.4s) |
| 79 | `$is_default_parameterIR` | 4 | YES (0.1s) | YES (0.4s) |
| 80 | `$is_lpm_key` | 4 | YES (0.1s) | YES (0.4s) |
| 81 | `$join_flow` | 4 | YES (0.1s) | YES (0.4s) |
| 82 | `$add_action_tbl` | 5 | YES (0.2s) | YES (0.4s) |
| 83 | `$add_key_tbl` | 5 | YES (0.1s) | YES (0.4s) |
| 84 | `$codom_map` | 5 | YES (0.1s) | YES (0.4s) |
| 85 | `$dom_map` | 5 | YES (0.1s) | YES (0.4s) |
| 86 | `$enter_path_i` | 5 | YES (0.1s) | YES (0.5s) |
| 87 | `$flatten_p4program` | 5 | YES (0.1s) | YES (0.4s) |
| 88 | `$empty_typingContext` | 6 | YES (0.1s) | YES (0.4s) |
| 89 | `$is_tableActionsProperty` | 6 | YES (0.1s) | YES (0.4s) |
| 90 | `$is_tableKeysProperty` | 6 | YES (0.1s) | YES (0.4s) |
| 91 | `$tableCustomName` | 6 | YES (0.1s) | YES (0.4s) |
| 92 | `$enter_i` | 7 | YES (0.1s) | YES (0.5s) |
| 93 | `$enter_t` | 7 | YES (0.1s) | YES (0.4s) |
| 94 | `$exit_i` | 7 | YES (0.2s) | YES (0.5s) |
| 95 | `$exit_t` | 7 | YES (0.1s) | YES (0.5s) |
| 96 | `$requires_priority_prime` | 7 | YES (0.2s) | YES (0.5s) |
| 97 | `$empty_instContext` | 8 | YES (0.1s) | YES (0.5s) |
| 98 | `$requires_priority` | 8 | YES (0.2s) | YES (0.5s) |
| 99 | `$typedLvalueIR_as_typedExpressionIR` | 8 | YES (0.2s) | YES (0.6s) |
| 100 | `$join_ctk` | 9 | YES (0.1s) | YES (0.4s) |
| 101 | `$resolve_constraint` | 9 | YES (0.2s) | YES (0.5s) |
| 102 | `$inherit_i` | 10 | YES (0.2s) | YES (0.5s) |
| 103 | `$name` | 10 | YES (0.2s) | YES (0.5s) |
| 104 | `$width_of_integerTypeIR` | 10 | YES (0.2s) | YES (0.5s) |
| 105 | `$objectId_ends_with` | 11 | YES (0.2s) | YES (0.5s) |
| 106 | `$un_plus` | 11 | YES (0.2s) | YES (0.4s) |
| 107 | `$prefixedTypeName` | 12 | YES (0.2s) | YES (0.5s) |
| 108 | `$assignop_as_binop` | 13 | YES (0.2s) | YES (0.4s) |
| 109 | `$callableId_IR` | 13 | YES (42.7s) | YES (0.5s) |
| 110 | `$flatten_nameList` | 13 | YES (0.2s) | YES (0.4s) |
| 111 | `$flatten_typeParameterList` | 13 | YES (0.2s) | YES (0.5s) |
| 112 | `$join_text` | 13 | YES (0.2s) | YES (0.5s) |
| 113 | `$callableId_of_externConstructorPrototypeIR` | 14 | YES (75.4s) | YES (0.5s) |
| 114 | `$callableId_of_externMethodPrototypeIR` | 15 | YES (132.2s) | YES (0.5s) |
| 115 | `$flatten_typeParameterListOpt` | 15 | YES (0.2s) | YES (0.4s) |
| 116 | `$is_tableDefaultActionProperty` | 16 | YES (0.3s) | YES (0.5s) |
| 117 | `$prefixedNonTypeName` | 19 | YES (0.3s) | YES (0.5s) |
| 118 | `$optional_annotation_of_parameterIR_prime_prime` | 20 | YES (0.4s) | YES (0.5s) |
| 119 | `$lvalue_as_expression` | 22 | YES (0.5s) | YES (0.5s) |
| 120 | `$starts_with` | 50 | YES (1.5s) | YES (0.5s) |
| 121 | `$strip_prefix_rec` | 53 | YES (1.7s) | YES (0.5s) |
| 122 | `$isValid_header` | 80 | YES (4.8s) | YES (0.5s) |
| 123 | `$invalidate_headerUnion` | 87 | YES (6.6s) | YES (0.7s) |
| 124 | `$invalidate_value` | 87 | YES (6.5s) | YES (0.7s) |
| 125 | `$ends_with` | 88 | YES (4.9s) | YES (0.6s) |
| 126 | `$strip_suffix_rec` | 91 | YES (5.5s) | YES (0.5s) |
| 127 | `$write_bits_from_value` | 103 | TIMEOUT (>1800s) | YES (6.0s) |
| 128 | `$bin_mod` | 109 | YES (7.6s) | YES (0.6s) |
| 129 | `$bin_div` | 113 | YES (8.3s) | YES (0.6s) |
| 130 | `$un_bnot` | 139 | YES (14.1s) | YES (5.5s) |
| 131 | `$bin_ge` | 183 | YES (30.1s) | YES (5.6s) |
| 132 | `$bin_le` | 183 | YES (29.9s) | YES (5.6s) |
| 133 | `$bin_gt` | 184 | YES (30.0s) | YES (5.6s) |
| 134 | `$bin_lt` | 184 | YES (29.9s) | YES (5.6s) |
| 135 | `$int_of_integerValue` | 184 | YES (29.6s) | YES (5.6s) |
| 136 | `$nat_of_integerValue` | 187 | YES (31.2s) | YES (5.6s) |
| 137 | `$bin_minus` | 193 | YES (36.1s) | YES (5.6s) |
| 138 | `$bin_mul` | 193 | YES (36.3s) | YES (5.7s) |
| 139 | `$bin_plus` | 193 | YES (36.3s) | YES (5.6s) |
| 140 | `$un_minus` | 197 | YES (37.5s) | YES (5.8s) |
| 141 | `$bin_bxor` | 199 | YES (43.6s) | YES (5.6s) |
| 142 | `$bin_concat` | 200 | YES (425.7s) | YES (5.6s) |
| 143 | `$set_priorities_of_tableEntryListIR_prime` | 200 | YES (43.7s) | YES (5.7s) |
| 144 | `$bin_satminus` | 201 | YES (46.0s) | YES (5.8s) |
| 145 | `$bin_satplus` | 201 | YES (45.8s) | YES (5.8s) |
| 146 | `$bin_shl` | 201 | TIMEOUT (>1800s) | YES (5.6s) |
| 147 | `$bin_band` | 202 | YES (142.9s) | YES (5.7s) |
| 148 | `$bin_bor` | 202 | YES (143.3s) | YES (5.8s) |
| 149 | `$name_annotationToken` | 209 | YES (53.3s) | YES (6.0s) |
| 150 | `$un_op` | 209 | YES (47.9s) | YES (5.6s) |
| 151 | `$bin_shr` | 215 | TIMEOUT (>1800s) | YES (5.8s) |
| 152 | `$set_priorities_of_tableEntryListIR` | 226 | YES (63.7s) | YES (5.7s) |
| 153 | `$name_annotation_opt` | 256 | YES (99.5s) | YES (6.0s) |
| 154 | `$write_value_field_from_bits_prime` | 271 | YES* (244.1s) | YES (5.8s) |
| 155 | `$write_value_fields_from_bits_prime` | 271 | YES* (243.9s) | YES (5.8s) |
| 156 | `$write_value_from_bits_prime` | 271 | YES* (244.0s) | YES (5.7s) |
| 157 | `$write_values_from_bits_prime` | 271 | YES* (242.8s) | YES (5.8s) |
| 158 | `$write_value_from_bits` | 274 | YES* (253.0s) | YES (5.8s) |
| 159 | `$bitacc_range_op` | 283 | TIMEOUT (>1800s) | YES (21.5s) |
| 160 | `$bitacc_offset_op` | 285 | TIMEOUT (>1800s) | YES (21.3s) |
| 161 | `$bitacc_range_replace_op` | 391 | TIMEOUT (>1800s) | YES (5.6s) |
| 162 | `$bitacc_offset_replace_op` | 394 | TIMEOUT (>1800s) | YES (5.7s) |
| 163 | `$flatten_namedExpressionList` | 748 | YES | YES (1.5s) |
| 164 | `$flatten_realTypeArgumentList` | 748 | YES | YES (1.5s) |
| 165 | `$flatten_expressionList` | 749 | TIMEOUT | YES (1.5s) |
| 166 | `$flatten_typeArgumentList` | 749 | TIMEOUT | YES (1.5s) |
| 167 | `$expression_as_lvalue` | 766 | MAYBE | YES (4.2s) |
| 168 | `$flatten_argumentList` | 784 | TIMEOUT | YES (6.1s) |
| 169 | `$flatten_simpleKeysetExpressionList` | 789 | TIMEOUT | YES (1.5s) |
| 170 | `$flatten_forUpdateStatementList` | 790 | YES | YES (2.3s) |
| 171 | `$is_singleton_list_expression` | 812 | YES | YES (1.7s) |
| 172 | `$add_annotationList` | 867 | YES | YES (1.8s) |
| 173 | `$flatten_annotationList` | 868 | YES | YES (2.5s) |
| 174 | `$flatten_parameterList` | 874 | YES | YES (2.4s) |
| 175 | `$flatten_constructorParameterListOpt` | 876 | YES | YES (2.5s) |
| 176 | `$is_externConstructorPrototype` | 880 | YES | YES (1.8s) |
| 177 | `$is_externMethodPrototype` | 883 | YES | YES (1.8s) |
| 178 | `$callableId_prime` | 884 | YES | YES (2.5s) |
| 179 | `$callableId` | 885 | YES | YES (2.6s) |
| 180 | `$constructorId_of_externConstructorPrototype` | 886 | YES | YES (2.5s) |
| 181 | `$callableId_of_externMethodPrototype` | 887 | YES | YES (2.5s) |
| 182 | `$constructorId` | 887 | YES | YES (2.5s) |
| 183 | `$expressionNonBrace_as_expression` | 887 | TIMEOUT | YES (2.1s) |
| 184 | `$optional_annotation_of_parameterIR_prime` | 893 | YES | YES (2.3s) |
| 185 | `$optional_annotation_of_parameterIR` | 895 | YES | YES (2.3s) |
| 186 | `$is_optional_parameterIR` | 896 | YES | YES (2.5s) |
| 187 | `$flatten_forInitStatementList` | 907 | YES | YES (2.6s) |
| 188 | `$split_externConstructorOrMethodPrototypeList` | 940 | YES | YES (2.2s) |
| 189 | `$flatten_parserStateList` | 1029 | YES | YES (0.6s) |
| 190 | `$name_annotation` | 1125 | - | YES (6.1s) |
| 191 | `$name_annotation_default` | 1127 | - | YES (6.2s) |
| 192 | `$cast_header_stack` | 1192 | - | YES (0.6s) |
| 193 | `$cast_header` | 1194 | - | YES (0.6s) |
| 194 | `$cast_struct` | 1194 | - | YES (0.6s) |
| 195 | `$compat_lnot` | 1194 | - | YES (5.8s) |
| 196 | `$nestable_constructor_package` | 1194 | - | YES (5.7s) |
| 197 | `$resolve_type_alias` | 1194 | - | YES (5.7s) |
| 198 | `$callTargetKey_prime` | 1195 | - | YES (0.6s) |
| 199 | `$compat_bnot` | 1195 | - | YES (5.7s) |
| 200 | `$compat_divmod` | 1195 | - | YES (5.7s) |
| 201 | `$compat_logical` | 1195 | - | YES (5.7s) |
| 202 | `$cast_bool` | 1196 | - | YES (5.8s) |
| 203 | `$compat_array_index` | 1196 | - | YES (5.7s) |
| 204 | `$compat_bitslice_offset_index` | 1196 | - | YES (5.7s) |
| 205 | `$compat_bitslice_offset_width` | 1196 | - | YES (5.6s) |
| 206 | `$compat_bitslice_range_index` | 1196 | - | YES (5.7s) |
| 207 | `$compat_uplusminus` | 1196 | - | YES (5.6s) |
| 208 | `$nestable_constructor_control` | 1196 | - | YES (5.7s) |
| 209 | `$nestable_constructor_parser` | 1196 | - | YES (5.7s) |
| 210 | `$nestable_controlApplyMethod` | 1196 | - | YES (5.7s) |
| 211 | `$nestable_headerStack` | 1196 | - | YES (5.7s) |
| 212 | `$nestable_headerUnion` | 1196 | - | YES (5.6s) |
| 213 | `$definable_constructor` | 1197 | - | YES (5.8s) |
| 214 | `$nestable_constructor_extern` | 1197 | - | YES (5.6s) |
| 215 | `$nestable_externFunction` | 1197 | - | YES (5.8s) |
| 216 | `$nestable_externMethod` | 1197 | - | YES (5.8s) |
| 217 | `$nestable_new_in_enum_serializable` | 1197 | - | YES (5.6s) |
| 218 | `$nestable_parserApplyMethod` | 1197 | - | YES (5.6s) |
| 219 | `$compat_switch` | 1198 | - | YES (5.8s) |
| 220 | `$compat_table_lpm_ternary_range_key` | 1198 | - | YES (5.6s) |
| 221 | `$nestable_new` | 1198 | - | YES (5.7s) |
| 222 | `$parameterListIR_of_functionTypeDefIR` | 1198 | - | YES (0.6s) |
| 223 | `$typedExpressionIR_as_typedLvalueIR` | 1198 | - | YES (0.6s) |
| 224 | `$compat_concat` | 1199 | - | YES (5.7s) |
| 225 | `$callTargetKey` | 1200 | - | YES (0.6s) |
| 226 | `$compat_table_exact_optional_key` | 1202 | - | YES (5.7s) |
| 227 | `$callableTypeIR_of_callableTypeDefIR` | 1203 | - | YES (0.6s) |
| 228 | `$compat_shift` | 1203 | - | YES (5.7s) |
| 229 | `$nestable_enum_serializable` | 1203 | - | YES (5.7s) |
| 230 | `$typeParameterListIR_of_callableTypeDefIR` | 1203 | - | YES (0.6s) |
| 231 | `$flatten_keysetExpressionIR` | 1206 | - | YES (0.6s) |
| 232 | `$is_static_assert_callableTypeIR` | 1206 | - | YES (0.6s) |
| 233 | `$nestable_tuple_in_set` | 1206 | - | YES (5.6s) |
| 234 | `$parameterListIR_of_methodTypeDefIR` | 1206 | - | YES (0.6s) |
| 235 | `$typeId_of_typeDefIR` | 1207 | - | YES (0.6s) |
| 236 | `$typeParameterListIR_of_typeDefIR` | 1207 | - | YES (0.6s) |
| 237 | `$nestable_sequence_in_set` | 1208 | - | YES (5.7s) |
| 238 | `$nestable_struct_in_header` | 1208 | - | YES (5.8s) |
| 239 | `$nestable_tuple` | 1208 | - | YES (5.7s) |
| 240 | `$nestable_struct` | 1209 | - | YES (5.8s) |
| 241 | `$nestable_definedFunction` | 1211 | - | YES (5.8s) |
| 242 | `$nestable_action` | 1212 | - | YES (5.7s) |
| 243 | `$nestable_list` | 1212 | - | YES (5.7s) |
| 244 | `$is_equalable_typeIR` | 1213 | - | YES (5.9s) |
| 245 | `$typeIR_of_typeDefIR` | 1213 | - | YES (0.6s) |
| 246 | `$is_assignable_typeIR` | 1214 | - | YES (5.7s) |
| 247 | `$nestable_typedef` | 1215 | - | YES (5.8s) |
| 248 | `$init_tableKeys` | 1216 | - | YES (1.4s) |
| 249 | `$compat_bitslice_base` | 1218 | - | YES (5.7s) |
| 250 | `$nestable_header` | 1218 | - | YES (5.7s) |
| 251 | `$is_defaultable_typeIR` | 1219 | - | YES (5.7s) |
| 252 | `$parameterListIR_of_callableTypeDefIR` | 1221 | - | YES (0.6s) |
| 253 | `$unroll_typeIR` | 1229 | - | YES (5.8s) |
| 254 | `$is_table_application` | 1231 | - | YES (5.8s) |
| 255 | `$nestable_set` | 1235 | - | YES (5.9s) |
| 256 | `$sizeof_minSizeInBits_prime` | 1259 | - | YES (5.7s) |
| 257 | `$sizeof_minSizeInBits` | 1260 | - | YES (5.7s) |
| 258 | `$unroll_aliasType` | 1268 | - | YES (5.8s) |
| 259 | `$result_concat` | 1269 | - | YES (5.6s) |
| 260 | `$find_local_return_type_t` | 1270 | - | YES (0.6s) |
| 261 | `$is_concrete_extern_object_prime` | 1278 | - | YES (0.6s) |
| 262 | `$sizeof_maxSizeInBits_prime` | 1283 | - | YES (5.7s) |
| 263 | `$sizeof_maxSizeInBits` | 1284 | - | YES (5.7s) |
| 264 | `$is_monomorphic_typeDefIR` | 1289 | - | YES (0.6s) |
| 265 | `$is_polymorphic_typeDefIR` | 1292 | - | YES (0.6s) |
| 266 | `$resolve_inference_prime` | 1301 | - | YES (5.8s) |
| 267 | `$parameterListIR_of_functionDef` | 1304 | - | YES (0.6s) |
| 268 | `$resolve_inference` | 1306 | - | YES (5.9s) |
| 269 | `$reduce_serenum` | 1310 | - | YES (5.9s) |
| 270 | `$is_concrete_extern_object` | 1317 | - | YES (5.7s) |
| 271 | `$update_mode_tbl` | 1337 | - | YES (5.7s) |
| 272 | `$sizeof_minSizeInBytes` | 1347 | - | YES (5.7s) |
| 273 | `$sizeof_maxSizeInBytes` | 1350 | - | YES (5.7s) |
| 274 | `$sizeof` | 1375 | - | YES (5.8s) |
| 275 | `$init_tableEntries` | 1406 | - | YES (1.4s) |
| 276 | `$is_valid_bitslice` | 1431 | - | YES (6.0s) |
| 277 | `$init_table` | 1462 | - | YES (1.3s) |
| 278 | `$parameterListIR_of_methodDef` | 1551 | - | YES (0.7s) |
| 279 | `$parameterListIR_of_callableDef` | 1565 | - | YES (0.7s) |
| 280 | `$parameterListIR_of_constructorDef` | 1570 | - | YES (0.6s) |
| 281 | `$subexpressions_of_argumentIR` | 1614 | - | YES (5.7s) |
| 282 | `$subexpressions_of_argumentListIR` | 1614 | - | YES (5.7s) |
| 283 | `$subexpressions_of_expressionIR` | 1614 | - | YES (5.7s) |
| 284 | `$subexpressions_of_typedExpressionIR` | 1614 | - | YES (5.7s) |
| 285 | `$subexpressions_of_typedExpressionListIR` | 1614 | - | YES (5.7s) |
| 286 | `$name_expression` | 1840 | - | YES (6.1s) |
| 287 | `ParameterType_alpha` | 2489 | - | YES (6.1s) |
| 288 | `ExternMethodType_alpha` | 2490 | - | YES (6.1s) |
| 289 | `Type_alpha` | 2572 | - | YES (6.2s) |
| 290 | `$check_switchLabel_default` | 50638 | - | YES (285.4s) |
| 291 | `$find_action_prime` | 50638 | - | YES (330.5s) |
| 292 | `$update_fieldValue` | 50638 | - | YES (330.3s) |
| 293 | `$add_store` | 50640 | - | YES (329.3s) |
| 294 | `$callable_controlApplyMethod` | 50640 | - | YES (170.4s) |
| 295 | `$callable_parserApplyMethod` | 50640 | - | YES (183.6s) |
| 296 | `$in_set` | 50640 | - | YES (170.0s) |
| 297 | `$find_action` | 50640 | - | YES (363.7s) |
| 298 | `$instantiable_package` | 50640 | - | YES (171.0s) |
| 299 | `$instantiable_table` | 50640 | - | YES (169.8s) |
| 300 | `$callable_action` | 50642 | - | YES (175.5s) |
| 301 | `$callable_externAbstractMethod` | 50642 | - | YES (166.3s) |
| 302 | `$callable_externMethod` | 50642 | - | YES (168.9s) |
| 303 | `$callable_tableApplyMethod` | 50642 | - | YES (168.3s) |
| 304 | `$find_non_overloadeds` | 50642 | - | YES (292.1s) |
| 305 | `$find_store` | 50642 | - | YES (329.8s) |
| 306 | `$split_dataplane_parameters` | 50642 | - | YES (329.7s) |
| 307 | `$directionless_trailing_prime` | 50643 | - | YES (351.9s) |
| 308 | `$find_typeDef_i` | 50643 | - | YES (1289.6s) |
| 309 | `$partition_parameterListIR` | 50643 | - | YES (159.3s) |
| 310 | `$add_constructorDef_i` | 50644 | - | YES (330.3s) |
| 311 | `$add_typeDef_i` | 50644 | - | YES (330.6s) |
| 312 | `$instantiable_control` | 50644 | - | YES (162.7s) |
| 313 | `$instantiable_parser` | 50644 | - | YES (160.7s) |
| 314 | `$add_constructorDefs_i` | 50646 | - | YES (330.4s) |
| 315 | `$merge_frames` | 50646 | - | YES (332.5s) |
