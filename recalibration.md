# P4 structural CTRS — CRC(합류성) + termination(모듈러 B) 결과

종합 스윕: ≤500규칙 153심볼. CRC/ChC = Church-Rosser/Coherence, term = 모듈러(B) 종료.

**term 열 재측정 (2026-07-12, `fix(rewrite): drop matcher guards a companion
destructure already implies`).** 그 커밋이 `match_K(v)=true`를 동반 destructure
`v = K(..)`에 흡수시켜 하강 인자를 head 패턴으로 올린 결과, 종료 MAYBE 18개 중
**13개가 YES**로, TIMEOUT이던 `$write_value_field_from_bits_prime`도 YES로 바뀌었다
(dependency-pair 분석이 전제 안에 숨은 구조적 하강을 이제 볼 수 있다):
`$concat_text` `$exists` `$forall` `$filter` `$flatten_p4program` `$flatten_nameList`
`$flatten_typeParameterList` `$flatten_typeParameterListOpt` `$lvalue_as_expression`
+ `$write_value*` 5형제. 잔여 MAYBE 5: `$join_text`,
`$set_priorities_of_tableEntryListIR{,_prime}` (슬라이스는 바뀌었으나 미해소),
`$invalidate_value`/`$invalidate_headerUnion` (fix가 슬라이스를 전혀 바꾸지 않음 —
접을 destructure-동반 matcher가 없다). `$bitacc_*`/`$write_bits_from_value`의 TIMEOUT은
그대로지만 이 머신에선 모듈러(B) 한 건이 15~25분 걸려(대조군 `$bitacc_range_replace_op`
= YES, 25분) 예산 문제와 구분되지 않는다.

**CRC/ChC 열 재측정 (2026-07-13, 같은 커밋) — 변화 없음.** 같은 fold가 head를
서로소화하니 CRC도 움직일 것으로 봤지만, 열의 값은 하나도 바뀌지 않았다.
① CRC가 YES가 아니던 행 중 `$join_ctk` `$assignop_as_binop` `$bin_satplus`
`$bin_concat` `$bin_shl` `$bin_shr` `$write_bits_from_value`는 fold 전/후 분석
슬라이스가 **바이트 동일**하다(접을 destructure-동반 matcher가 없다) — verdict가
움직일 수 없으므로 재측정 대상이 아니다. ② 슬라이스가 실제로 바뀐 비-YES 행은
`$write_value*` 5개(MAYBE)와 `$bitacc_*` 4개(TIMEOUT)뿐인데, `$write_value*`는 이
머신에서 CRC가 아예 판정을 못 낸다(1800s 4× TIMEOUT, 단독 5400s 예산에서도 74분
뒤 verdict 없이 ERROR) — 표의 MAYBE는 더 빠른 환경의 옛 측정값이라 그대로 둔다.
③ **회귀 없음**: fold로 슬라이스가 바뀐 기존 YES 8개(`$concat_text` `$exists`
`$forall` `$filter` `$flatten_p4program` `$flatten_nameList`
`$flatten_typeParameterListOpt` `$lvalue_as_expression`)를 다시 돌려 전부
CRC/ChC YES/YES 유지를 확인했다(7~10분/건). 즉 이 커밋의 순이익은 termination
쪽에만 나타난다.

**CRC 열 후속 — 상보 비교 가드 정렬 (2026-07-13, `feat(rewrite): align
complementary comparison/negation guards for the CRC`, a290977b).** 분석 전용
패스 `Reflect.align_guards`가 조건 위치의 `lt`/`lt_int`(및 선두 `not`)을 정준
`leq`/`leq_int` 술어의 반대 극성으로 재철자한다. `i<0`(arith shift) vs
`i>=0`(logical shift)로 갈리는 형제 절이 번역 후 갖던 서로 다른 subject
(`lt_int(X,0)=true` vs swapped `leq_int(0,X)=true`)를 **같은 subject
`leq_int(0,X)`의 true/false 극성**으로 통일해, CRC가 가설 재작성으로 임계쌍을
discharge한다.

**대상은 상보 비교쌍을 가진 3심볼뿐**(전 p4 표면 pairscan): `$bin_satplus`
(CRC **MAYBE**), `$bin_satminus`(이미 **YES**), `$bin_shr`(CRC **TIMEOUT**). 표의
다른 비-YES 행(`$join_ctk`/`$assignop_as_binop` = match fall-through,
`$bin_shl`/`$bin_concat`/`$write_*`/`$bitacc_*` = 산술 CP 폭발 TIMEOUT 또는 별
원인)은 sign-split이 아니므로 이 패스와 무관하다.

**실측 (시그니처-축소 슬라이스 CRC, `ulimit -s unlimited`, Maude 3.5.1a/CRC 3t).**
`$bin_shr` 자기-레이어 12절: 재철자 전 **MAYBE(6 임계쌍)** → 후 **YES(0)**;
`$bin_satplus` 축소 슬라이스 **YES**. `$bin_satminus`는 동형(같은 상보 형태)이나
축소 CRC가 산술 라이브러리 무게로 완주 미측정.

**근본 원인 규명 — 왜 기존 bridge가 이걸 못 고쳤나.** prelude에는 이미
`lt_int(x,y)=not(leq_int(y,x))` bridge가 있는데도 sign-split이 살아남았다. 이유는
sort다: 여기서 `X=$bitstr_to_int(..)`의 복원 sort가 최상위 `Val`인데 bridge는
하위 sort `IntV`에 선언돼 있어 **이 항에 발화하지 못한다**(bridge 있는 채로도 6
임계쌍 생존 실측). align_guards는 `leq_int` 심볼을 직접 써 sort와 무관하게
정렬하므로 우회한다. (이 `Val`-wide 도메인 문제는 아래 표의 여러 산술/판정
심볼에 공통이며, todo.md의 "subty_*/match_* op 도메인 협소화(P1)" 항목과 같은
뿌리다.)

**⚠️ 표 verdict는 이 커밋 기준으로 재측정하지 않았다.** 표는 **전체 슬라이스**
CRC 측정값이고, 위 실측은 **축소 슬라이스**다 — 서로 다른 측정이다. 전체 슬라이스
CRC는 이 심볼들에서 산술 라이브러리(`badd`/`bmul`) 임계쌍 폭발이 지배적이라
(`$bin_shr` = TIMEOUT), align_guards가 sign-split MAYBE의 **원인은 제거해도**
전체 슬라이스 verdict(특히 `$bin_shr`의 TIMEOUT)는 산술 무게 때문에 그대로 남을
공산이 크다. `$bin_satplus`(전체 MAYBE)만 재측정 시 YES로 떨어질 후보이며, 이
불안정한 WSL 환경에서 전체 슬라이스 CRC 완주가 반복 실패(장시간 실행 중 프로세스
소멸)해 미확정으로 둔다 — 다음 안정 환경에서 `verify --symbol '$bin_satplus'`
재측정 필요. 아래 표의 `$bin_satplus`/`$bin_shr` CRC 열은 그 전까지 옛 값을
유지한다.

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
| `$join_ctk` | 15 | MAYBE | YES | YES |  |
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
| `$write_value_field_from_bits_prime` | 278 | MAYBE | YES | YES | badd bmul bsub bmod negate-int |
| `$write_value_fields_from_bits_prime` | 278 | MAYBE | YES | YES | badd bmul bsub bmod negate-int |
| `$write_value_from_bits_prime` | 278 | MAYBE | YES | YES | badd bmul bsub bmod negate-int |
| `$write_values_from_bits_prime` | 278 | MAYBE | YES | YES | badd bmul bsub bmod negate-int |
| `$write_value_from_bits` | 281 | MAYBE | YES | YES | badd bmul bsub bmod negate-int |
| `$bitacc_range_op` | 294 | TIMEOUT | - | TIMEOUT | badd bmul bsub bdiv bmod negate-int nat-of-int band |
| `$bitacc_offset_op` | 296 | TIMEOUT | - | TIMEOUT | badd bmul bsub bdiv bmod negate-int nat-of-int band |
| `$bitacc_range_replace_op` | 396 | TIMEOUT | - | YES | badd bmul bsub bdiv negate-int |
| `$bitacc_offset_replace_op` | 405 | TIMEOUT | - | TIMEOUT | badd bmul bsub bdiv negate-int nat-of-int |
