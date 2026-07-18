# P4 structural CTRS — 검증 결과 (표)

CRC/ChC = Church-Rosser(합류성)/Coherence. 값 = YES / MAYBE / TIMEOUT / `-`(미도달).
측정 이력·방법·MAYBE/TIMEOUT 해석은 **[verification-notes.md](verification-notes.md)** 참조.

## 1. ≤500 종합 (153심볼 · term 두 방식 병기)

CRC/ChC/`term(AProVE직접)`는 **2026-07-18 fresh 재검**(현재 바이너리 `2f9f8cba`, 새 덤프, `/tmp/fresh500`, 153/153).
`term(모듈러B)`는 **종전 stale 측정**(pre-fix 덤프) — spine과 다른 런이라 재측정 필요(§ 하단 TODO).

- **CRC**: YES 140 / TIMEOUT 8 / MAYBE 5  ·  **ChC**: YES 145 / - 8
- **term(모듈러B)**: YES 144 / MAYBE 5 / TIMEOUT 4  ·  **term(AProVE직접)**: YES 117 / TIMEOUT 25 / MAYBE 11
- 두 term 열 차이 = 방법론 차이(모듈러-B는 산술 블랙박스 → YES, AProVE 직접은 full-arith 미증명 → TIMEOUT). 정확성 아님. 행별 해석은 notes.

| symbol | rules | CRC | ChC | term(모듈러B) | term(AProVE직접) | new-commit helpers |
|---|---|---|---|---|---|---|
| `$annotationList_of_parameterIR` | 1 | YES | YES | YES | YES |  |
| `$ctk_of_typedExpressionIR` | 1 | YES | YES | YES | YES |  |
| `$empty_map` | 1 | YES | YES | YES | YES |  |
| `$empty_set` | 1 | YES | YES | YES | YES |  |
| `$empty_tableContext` | 1 | YES | YES | YES | YES |  |
| `$id_of_parameterIR` | 1 | YES | YES | YES | YES |  |
| `$invalidate_header` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_actionDef` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_actionTypeDefIR` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_constructorTypeDefIR` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_controlApplyMethodDef` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_controlApplyMethodTypeIR` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_definedFunctionDef` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_definedFunctionTypeDefIR` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_externFunctionDef` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_externFunctionTypeDefIR` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_parserApplyMethodDef` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_parserApplyMethodTypeIR` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_tableApplyMethodDef` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_tableApplyMethodTypeDefIR` | 1 | YES | YES | YES | YES |  |
| `$tableEntryPriorityOptIR_of_tableEntryIR` | 1 | YES | YES | YES | YES |  |
| `$type_of_typedExpressionIR` | 1 | YES | YES | YES | YES |  |
| `$type_of_typedLvalueIR` | 1 | YES | YES | YES | YES |  |
| `$parameterListIR_of_externMethodDef` | 1 | YES | YES | YES | YES |  |
| `$set_priority_of_tableEntryIR` | 1 | YES | YES | YES | YES |  |
| `$empty_callableDefEnv` | 2 | YES | YES | YES | YES |  |
| `$empty_callableTypeDefEnv` | 2 | YES | YES | YES | YES |  |
| `$empty_constructorDefEnv` | 2 | YES | YES | YES | YES |  |
| `$empty_constructorTypeDefEnv` | 2 | YES | YES | YES | YES |  |
| `$empty_frame` | 2 | YES | YES | YES | YES |  |
| `$empty_stateEnv` | 2 | YES | YES | YES | YES |  |
| `$empty_store` | 2 | YES | YES | YES | YES |  |
| `$empty_theta` | 2 | YES | YES | YES | YES |  |
| `$empty_typeDefEnv` | 2 | YES | YES | YES | YES |  |
| `$empty_typeFrame` | 2 | YES | YES | YES | YES |  |
| `$flatten_constOpt` | 2 | YES | YES | YES | YES |  |
| `$ite` | 2 | YES | YES | YES | YES |  |
| `$flatten_objectInitializerOptIR` | 2 | YES | YES | YES | YES |  |
| `$is_some` | 2 | YES | YES | YES | YES |  |
| `$opt_as_seq` | 2 | YES | YES | YES | YES |  |
| `$parameterListIR_of_externMethodTypeDefIR` | 2 | YES | YES | YES | YES |  |
| `$type_of_externMethodPrototypeIR` | 2 | YES | YES | YES | YES |  |
| `$callable_builtinMethod` | 3 | YES | YES | YES | YES |  |
| `$constructorTypeDef_of_externConstructorPrototypeIR` | 3 | YES | YES | YES | YES |  |
| `$constructor_of_externConstructorPrototypeIR` | 3 | YES | YES | YES | YES |  |
| `$empty_constraint` | 3 | YES | YES | YES | YES |  |
| `$instantiable_extern` | 3 | YES | YES | YES | YES |  |
| `$is_lpm_key_prime` | 3 | YES | YES | YES | YES |  |
| `$un_lnot` | 3 | YES | YES | YES | YES |  |
| `$join_tableEntryState` | 3 | YES | YES | YES | YES |  |
| `$filter` | 3 | YES | YES | YES | YES |  |
| `$is_concrete_extern_object_prime_prime` | 4 | YES | YES | YES | YES |  |
| `$is_default_parameterIR` | 4 | YES | YES | YES | YES |  |
| `$is_lpm_key` | 4 | YES | YES | YES | YES |  |
| `$concat_text` | 4 | YES | YES | YES | YES |  |
| `$exists` | 4 | YES | YES | YES | YES |  |
| `$flatten_blockElementStatementList` | 4 | YES | YES | YES | YES |  |
| `$flatten_controlLocalDeclarationList` | 4 | YES | YES | YES | YES |  |
| `$flatten_externConstructorOrMethodPrototypeList` | 4 | YES | YES | YES | YES |  |
| `$flatten_objectDeclarationList` | 4 | YES | YES | YES | YES |  |
| `$flatten_parserLocalDeclarationList` | 4 | YES | YES | YES | YES |  |
| `$flatten_parserStatementList` | 4 | YES | YES | YES | YES |  |
| `$flatten_selectCaseList` | 4 | YES | YES | YES | YES |  |
| `$flatten_switchCaseList` | 4 | YES | YES | YES | YES |  |
| `$flatten_tableActionList` | 4 | YES | YES | YES | YES |  |
| `$flatten_tableEntryList` | 4 | YES | YES | YES | YES |  |
| `$flatten_tableKeyList` | 4 | YES | YES | YES | YES |  |
| `$flatten_tablePropertyList` | 4 | YES | YES | YES | YES |  |
| `$flatten_typeFieldList` | 4 | YES | YES | YES | YES |  |
| `$forall` | 4 | YES | YES | YES | YES |  |
| `$join_flow` | 4 | YES | YES | YES | YES |  |
| `$flatten_prefixedNameIR` | 4 | YES | YES | YES | YES |  |
| `$add_action_tbl` | 5 | YES | YES | YES | YES |  |
| `$add_key_tbl` | 5 | YES | YES | YES | YES |  |
| `$codom_map` | 5 | YES | YES | YES | YES |  |
| `$dom_map` | 5 | YES | YES | YES | YES |  |
| `$enter_path_i` | 5 | YES | YES | YES | YES |  |
| `$flatten_p4program` | 5 | YES | YES | YES | YES |  |
| `$empty_typingContext` | 6 | YES | YES | YES | YES |  |
| `$is_tableActionsProperty` | 6 | YES | YES | YES | YES |  |
| `$is_tableKeysProperty` | 6 | YES | YES | YES | YES |  |
| `$tableCustomName` | 6 | YES | YES | YES | YES |  |
| `$enter_i` | 7 | YES | YES | YES | YES |  |
| `$enter_t` | 7 | YES | YES | YES | YES |  |
| `$exit_i` | 7 | YES | YES | YES | YES |  |
| `$exit_t` | 7 | YES | YES | YES | YES |  |
| `$requires_priority_prime` | 7 | YES | YES | YES | YES |  |
| `$empty_instContext` | 8 | YES | YES | YES | YES |  |
| `$requires_priority` | 8 | YES | YES | YES | YES |  |
| `$typedLvalueIR_as_typedExpressionIR` | 8 | YES | YES | YES | YES |  |
| `$join_ctk` | 9 | YES | YES | YES | YES |  |
| `$width_of_integerTypeIR` | 10 | YES | YES | YES | YES |  |
| `$inherit_i` | 10 | YES | YES | YES | YES |  |
| `$name` | 10 | YES | YES | YES | YES |  |
| `$un_plus` | 11 | YES | YES | YES | YES |  |
| `$callableId_IR` | 11 | YES | YES | YES | YES |  |
| `$objectId_ends_with` | 11 | YES | YES | YES | YES |  |
| `$callableId_of_externConstructorPrototypeIR` | 12 | YES | YES | YES | YES |  |
| `$prefixedTypeName` | 12 | YES | YES | YES | YES |  |
| `$join_text` | 13 | YES | YES | MAYBE | MAYBE | bsucc |
| `$resolve_constraint` | 13 | YES | YES | YES | YES |  |
| `$callableId_of_externMethodPrototypeIR` | 13 | YES | YES | YES | YES |  |
| `$flatten_nameList` | 13 | YES | YES | YES | YES |  |
| `$flatten_typeParameterList` | 13 | YES | YES | YES | YES |  |
| `$assignop_as_binop` | 13 | YES | YES | YES | YES |  |
| `$flatten_typeParameterListOpt` | 15 | YES | YES | YES | YES |  |
| `$is_tableDefaultActionProperty` | 16 | YES | YES | YES | YES |  |
| `$prefixedNonTypeName` | 19 | YES | YES | YES | YES |  |
| `$optional_annotation_of_parameterIR_prime_prime` | 20 | YES | YES | YES | YES |  |
| `$lvalue_as_expression` | 22 | YES | YES | YES | YES |  |
| `$starts_with` | 50 | YES | YES | YES | YES | bsucc bpred bcompare |
| `$strip_prefix_rec` | 53 | YES | YES | YES | YES | bsucc bpred bcompare |
| `$isValid_header` | 80 | YES | YES | YES | YES |  |
| `$ends_with` | 88 | YES | YES | YES | YES | bsub bsucc bpred bcompare |
| `$strip_suffix_rec` | 91 | YES | YES | YES | YES | bsub bsucc bpred bcompare |
| `$invalidate_headerUnion` | 91 | YES | YES | MAYBE | MAYBE |  |
| `$invalidate_value` | 91 | YES | YES | MAYBE | MAYBE |  |
| `$write_bits_from_value` | 107 | TIMEOUT | - | TIMEOUT | YES |  |
| `$bin_mod` | 109 | YES | YES | YES | YES | bsub bmod negate-int |
| `$bin_div` | 113 | YES | YES | YES | YES | bsub bdiv negate-int |
| `$un_bnot` | 139 | YES | YES | YES | MAYBE | badd bmul bsub bpow-nat bneg negate-int |
| `$bin_ge` | 183 | YES | YES | YES | MAYBE | badd bmul bsub bdiv negate-int |
| `$bin_le` | 183 | YES | YES | YES | MAYBE | badd bmul bsub bdiv negate-int |
| `$bin_gt` | 184 | YES | YES | YES | MAYBE | badd bmul bsub bdiv negate-int |
| `$bin_lt` | 184 | YES | YES | YES | MAYBE | badd bmul bsub bdiv negate-int |
| `$int_of_integerValue` | 184 | YES | YES | YES | MAYBE | badd bmul bsub bdiv negate-int |
| `$nat_of_integerValue` | 187 | YES | YES | YES | MAYBE | badd bmul bsub bdiv negate-int nat-of-int |
| `$bin_minus` | 193 | YES | YES | YES | TIMEOUT | badd bmul bsub bdiv bmod negate-int |
| `$bin_mul` | 193 | YES | YES | YES | TIMEOUT | badd bmul bsub bdiv bmod negate-int |
| `$bin_plus` | 193 | YES | YES | YES | TIMEOUT | badd bmul bsub bdiv bmod negate-int |
| `$un_minus` | 197 | YES | YES | YES | TIMEOUT | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$bin_bxor` | 199 | YES | YES | YES | TIMEOUT | badd bmul bsub bdiv bmod negate-int bxor |
| `$bin_concat` | 200 | TIMEOUT | - | YES | TIMEOUT | badd bmul bsub bdiv bmod negate-int |
| `$set_priorities_of_tableEntryListIR_prime` | 200 | YES | YES | MAYBE | TIMEOUT | badd bmul bsub bdiv negate-int nat-of-int |
| `$bin_satminus` | 201 | YES | YES | YES | TIMEOUT | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$bin_satplus` | 201 | YES | YES | YES | TIMEOUT | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$bin_shl` | 201 | TIMEOUT | - | YES | TIMEOUT | badd bmul bsub bdiv bmod negate-int |
| `$bin_band` | 202 | YES | YES | YES | TIMEOUT | badd bmul bsub bdiv bmod negate-int band |
| `$bin_bor` | 202 | YES | YES | YES | TIMEOUT | badd bmul bsub bdiv bmod negate-int bor |
| `$name_annotationToken` | 209 | YES | YES | YES | MAYBE | bsub bdiv bmod |
| `$un_op` | 209 | YES | YES | YES | TIMEOUT | badd bmul bsub bdiv bmod bpow-nat bneg negate-int |
| `$bin_shr` | 215 | TIMEOUT | - | YES | TIMEOUT | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$set_priorities_of_tableEntryListIR` | 226 | YES | YES | MAYBE | TIMEOUT | badd bmul bsub bdiv negate-int nat-of-int |
| `$name_annotation_opt` | 256 | YES | YES | YES | TIMEOUT | bsub bdiv bmod |
| `$write_value_field_from_bits_prime` | 271 | MAYBE | YES | YES | TIMEOUT | badd bmul bsub bmod negate-int |
| `$write_value_fields_from_bits_prime` | 271 | MAYBE | YES | YES | TIMEOUT | badd bmul bsub bmod negate-int |
| `$write_value_from_bits_prime` | 271 | MAYBE | YES | YES | TIMEOUT | badd bmul bsub bmod negate-int |
| `$write_values_from_bits_prime` | 271 | MAYBE | YES | YES | TIMEOUT | badd bmul bsub bmod negate-int |
| `$write_value_from_bits` | 274 | MAYBE | YES | YES | TIMEOUT | badd bmul bsub bmod negate-int |
| `$bitacc_range_op` | 283 | TIMEOUT | - | TIMEOUT | TIMEOUT | badd bmul bsub bdiv bmod negate-int nat-of-int band |
| `$bitacc_offset_op` | 285 | TIMEOUT | - | TIMEOUT | TIMEOUT | badd bmul bsub bdiv bmod negate-int nat-of-int band |
| `$bitacc_range_replace_op` | 391 | TIMEOUT | - | YES | TIMEOUT | badd bmul bsub bdiv negate-int |
| `$bitacc_offset_replace_op` | 394 | TIMEOUT | - | TIMEOUT | TIMEOUT | badd bmul bsub bdiv negate-int nat-of-int |

## 2. >500 슬라이스 (bigsweep · term = 모듈러 B · 27/127 확정)

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


## TODO

- [ ] **≤500 `term(모듈러B)` 열 fresh 재측정.** 현재 이 열은 pre-fix stale 덤프
  (7-09/7-10) 측정치라, 같은 표의 fresh AProVE/CRC 열과 측정 기준이 다르다. 현재
  바이너리(`2f9f8cba`)로 새 덤프를 뽑아 모듈러-B 종료를 다시 돌려 열을 갱신해야
  두 term 열이 동일 런 기준으로 비교된다. (bigfresh가 >500엔 이미 fresh 모듈러-B를
  돌리는 중; ≤500은 fresh500이 AProVE만 돌렸으므로 모듈러-B fresh가 빠져 있음.)
- [ ] **§2 >500 표 fresh 값 갱신.** bigfresh(현재 진행) 완주 시 27행 stale 표를 교체.

