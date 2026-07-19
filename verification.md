# P4 structural CTRS — 검증 결과 (표)

CRC/ChC = Church-Rosser(합류성)/Coherence. 값 = YES / MAYBE / TIMEOUT / `-`(미도달).
측정 이력·방법·MAYBE/TIMEOUT 해석은 **[verification-notes.md](verification-notes.md)** 참조.

## 1. ≤500 종합 (153심볼 · term 두 방식 병기)

**term 두 열은 2026-07-19에 MTT를 걷어낸 새 방법으로 전수 재측정했다**
(바이너리 `30d413ad` = iter-fuse의 IterPr 헬퍼 통합 반영, 슬라이스는 직전 런과
byte-identical, `/tmp/claude-iterfuse/sp`, 153/153 × 2축). `rules` 열은 그 직전
동시 재측정 값 그대로다.
`CRC`/`ChC`는 **2026-07-18 측정 유지**(바이너리 `2f9f8cba`, `/tmp/fresh500`) — 이번 재검은
term 두 축만 대상이었다(§ 하단 TODO).

> ⚠️ **측정 기준 커밋 주의.** term 두 열은 `30d413ad` 덤프 기준이고, 그 이후 new-rewrite에
> 술어 도메인을 사용처에서 복원하는 변경(`6e740f3e` 계열, `spec.ctrs`/`spec.maude` 골든 동반
> 변경)이 들어왔다. 구조 보존 경로는 sort 태그를 버리고 규칙만 읽으므로 판정이 바뀌지 않을
> 공산이 크지만 **미확인**이다 — 아래 TODO 참조.

**방법론 전환: MTT의 C;A unraveling을 쓰지 않는다.** 종전 두 축은 모두
`슬라이스 → MTT(C;A) → AProVE` 경로였다. MTT는 `l -> r if s == t`를 *조건의 변수*를
헬퍼로 넘겨 unravel하므로 `HU(e0,e1)` 같은 인자가 성분으로 분해됐다가 우변에서
재조립되고, **하강을 나르던 부분항 관계가 역전**된다(`e0`,`e1`은 `HU(e0,e1)`보다
작다) — 어떤 argument projection으로도 dependency pair를 정렬할 수 없어 예산을
아무리 줘도 증명이 불가능했다. 새 경로는 좌변 인자 목록을 **정의 규칙 없는 불활성
생성자 `k_N`에 감싸 그대로** 넘기는 구조 보존 unraveling으로 평범한 TRS를 만들어
`tools/aprove/runme`(WST)에 직접 투입한다. 상세·건전성 논증·구현 함정은
[CLAUDE.md](CLAUDE.md) "Do not route termination through MTT" 참조.

- **CRC**: YES 140 / TIMEOUT 8 / MAYBE 5  ·  **ChC**: YES 145 / - 8   *(2f9f8cba 기준)*
- **term(AProVE직접)**: YES **153 / 153**  ·  **term(모듈러B)**: YES 150 / MAYBE 2 / TIMEOUT 1
- **AProVE직접 축은 117 → 153으로 완결됐다.** MTT의 MAYBE 12건 **전부**와 TIMEOUT 24건 중
  **23건**이 닫혔고, 이진 산술 계열(`$bin_*`·`$un_*`·`$bitacc_*`·`$write_value*`)이 통째로
  풀렸다. 인코딩은 한 줄도 고치지 않았다 — 원인은 번역도 AProVE도 아닌 MTT였다.
- 종전 "두 축 모두 MAYBE"였던 3건(`$join_text`, `$invalidate_value`,
  `$invalidate_headerUnion`)은 MTT로 1200s를 소진하고 MAYBE였으나 새 경로에서 **각 1초에
  YES**다. 종전 이 표가 "AProVE 자동 전략의 도구 한계"로 적었던 진단은 **틀렸다**.
- **회귀는 모듈러B 축에만, 3건.** `$write_bits_from_value`(TIMEOUT),
  `$set_priorities_of_tableEntryListIR{,_prime}`(MAYBE) — MTT 경로에서는 YES였다.
  구조 보존은 keep-생성자가 인자 구조를 복제하므로 항이 커지는 비용이 있고, MTT의 분해가
  마침 무해했던 슬라이스에서는 그 비용만 남는다. 세 건 모두 MTT와 같은 1200s 예산으로
  재측정한 값이다(`stage2.tsv`).
- **두 방법의 비-YES 집합은 서로소다.** MTT가 못 푼 3건과 구조 보존이 못 푼 3건이 겹치지
  않으므로, 모듈러B 축도 **둘 중 하나라도 돌리면 153/153**이다. 실무 권고는 포트폴리오:
  구조 보존을 기본으로 쓰되 비-YES가 남으면 MTT 경로를 보조로 돌린다.
- 예산 주의: `$write_bits_from_value` 축A는 300s에서 TIMEOUT, 1200s에서 YES다. 표의 값은
  MTT와 같은 예산 기준으로 맞췄다.
- `rules` 열이 종전과 다른 7행은 헬퍼 통합의 직접 효과다(변수별 collect 병합으로 감소:
  `$resolve_constraint` 13→9, `$invalidate_value`/`$invalidate_headerUnion` 91→87,
  `$write_bits_from_value` 107→103; 다출력 ex-apply의 튜플화로 증가: `$callableId_IR` 11→13,
  `$callableId_of_extern{Constructor,Method}PrototypeIR` 12→14 / 13→15).

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
| `$callableId_IR` | 13 | YES | YES | YES | YES |  |
| `$objectId_ends_with` | 11 | YES | YES | YES | YES |  |
| `$callableId_of_externConstructorPrototypeIR` | 14 | YES | YES | YES | YES |  |
| `$prefixedTypeName` | 12 | YES | YES | YES | YES |  |
| `$join_text` | 13 | YES | YES | YES | YES | bsucc |
| `$resolve_constraint` | 9 | YES | YES | YES | YES |  |
| `$callableId_of_externMethodPrototypeIR` | 15 | YES | YES | YES | YES |  |
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
| `$invalidate_headerUnion` | 87 | YES | YES | YES | YES |  |
| `$invalidate_value` | 87 | YES | YES | YES | YES |  |
| `$write_bits_from_value` | 103 | TIMEOUT | - | TIMEOUT | YES |  |
| `$bin_mod` | 109 | YES | YES | YES | YES | bsub bmod negate-int |
| `$bin_div` | 113 | YES | YES | YES | YES | bsub bdiv negate-int |
| `$un_bnot` | 139 | YES | YES | YES | YES | badd bmul bsub bpow-nat bneg negate-int |
| `$bin_ge` | 183 | YES | YES | YES | YES | badd bmul bsub bdiv negate-int |
| `$bin_le` | 183 | YES | YES | YES | YES | badd bmul bsub bdiv negate-int |
| `$bin_gt` | 184 | YES | YES | YES | YES | badd bmul bsub bdiv negate-int |
| `$bin_lt` | 184 | YES | YES | YES | YES | badd bmul bsub bdiv negate-int |
| `$int_of_integerValue` | 184 | YES | YES | YES | YES | badd bmul bsub bdiv negate-int |
| `$nat_of_integerValue` | 187 | YES | YES | YES | YES | badd bmul bsub bdiv negate-int nat-of-int |
| `$bin_minus` | 193 | YES | YES | YES | YES | badd bmul bsub bdiv bmod negate-int |
| `$bin_mul` | 193 | YES | YES | YES | YES | badd bmul bsub bdiv bmod negate-int |
| `$bin_plus` | 193 | YES | YES | YES | YES | badd bmul bsub bdiv bmod negate-int |
| `$un_minus` | 197 | YES | YES | YES | YES | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$bin_bxor` | 199 | YES | YES | YES | YES | badd bmul bsub bdiv bmod negate-int bxor |
| `$bin_concat` | 200 | TIMEOUT | - | YES | YES | badd bmul bsub bdiv bmod negate-int |
| `$set_priorities_of_tableEntryListIR_prime` | 200 | YES | YES | MAYBE | YES | badd bmul bsub bdiv negate-int nat-of-int |
| `$bin_satminus` | 201 | YES | YES | YES | YES | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$bin_satplus` | 201 | YES | YES | YES | YES | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$bin_shl` | 201 | TIMEOUT | - | YES | YES | badd bmul bsub bdiv bmod negate-int |
| `$bin_band` | 202 | YES | YES | YES | YES | badd bmul bsub bdiv bmod negate-int band |
| `$bin_bor` | 202 | YES | YES | YES | YES | badd bmul bsub bdiv bmod negate-int bor |
| `$name_annotationToken` | 209 | YES | YES | YES | YES | bsub bdiv bmod |
| `$un_op` | 209 | YES | YES | YES | YES | badd bmul bsub bdiv bmod bpow-nat bneg negate-int |
| `$bin_shr` | 215 | TIMEOUT | - | YES | YES | badd bmul bsub bdiv bmod bpow-nat negate-int |
| `$set_priorities_of_tableEntryListIR` | 226 | YES | YES | MAYBE | YES | badd bmul bsub bdiv negate-int nat-of-int |
| `$name_annotation_opt` | 256 | YES | YES | YES | YES | bsub bdiv bmod |
| `$write_value_field_from_bits_prime` | 271 | MAYBE | YES | YES | YES | badd bmul bsub bmod negate-int |
| `$write_value_fields_from_bits_prime` | 271 | MAYBE | YES | YES | YES | badd bmul bsub bmod negate-int |
| `$write_value_from_bits_prime` | 271 | MAYBE | YES | YES | YES | badd bmul bsub bmod negate-int |
| `$write_values_from_bits_prime` | 271 | MAYBE | YES | YES | YES | badd bmul bsub bmod negate-int |
| `$write_value_from_bits` | 274 | MAYBE | YES | YES | YES | badd bmul bsub bmod negate-int |
| `$bitacc_range_op` | 283 | TIMEOUT | - | YES | YES | badd bmul bsub bdiv bmod negate-int nat-of-int band |
| `$bitacc_offset_op` | 285 | TIMEOUT | - | YES | YES | badd bmul bsub bdiv bmod negate-int nat-of-int band |
| `$bitacc_range_replace_op` | 391 | TIMEOUT | - | YES | YES | badd bmul bsub bdiv negate-int |
| `$bitacc_offset_replace_op` | 394 | TIMEOUT | - | YES | YES | badd bmul bsub bdiv negate-int nat-of-int |

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

- [x] **≤500 `term(모듈러B)` 열 fresh 재측정** (2026-07-19 완료). 두 term 축을 한 런에서
  동시에 재측정해(`/tmp/claude-iterfuse/iterterm.sh`, tmux `iterterm`, 153/153) 열을
  교체했다. 기준 바이너리는 `2f9f8cba`가 아니라 **`30d413ad`**(iter-fuse의 IterPr 헬퍼
  통합)이며, 그 통합 자체의 무회귀 검증을 겸했다 — 위 §1 요약의 회귀 대조 참조.
  모듈러-B TIMEOUT은 4→0으로 해소.
- [ ] **≤500 `CRC`/`ChC` 열 재측정.** 이 두 열만 아직 `2f9f8cba` 기준이라, 같은 표의
  `rules`/term 열(`30d413ad`)과 측정 기준이 다르다. 통합이 CRC에 미치는 영향은
  표본으로만 확인됨(head-side·k=1 collect YES/YES, k≥2 소비자는 통합 직전 커밋에서도
  동일 TIMEOUT = 무회귀). 전수 재검은 미실시.
- [ ] **§2 >500 표 fresh 값 갱신.** bigfresh(현재 진행) 완주 시 27행 stale 표를 교체.
      term(B) 열은 MTT 경로 값이므로, 갱신 시 §1과 같은 구조 보존 경로로 재측정할 것
      (§1에서 MAYBE 11건 중 상당수가 닫힐 가능성이 높다).
- [ ] **term 두 열의 기준 커밋 확인.** 측정은 `30d413ad` 덤프 기준인데 HEAD는 그 뒤로
      술어 도메인 변경(`6e740f3e` 계열)을 포함한다. HEAD에서 153 슬라이스를 다시 덤프해
      `30d413ad` 덤프와 대조할 것 — sort 태그만 달라졌다면 구조 보존 경로의 TRS는
      byte-identical이므로 판정이 그대로다(재측정 불필요).
- [ ] **구조 보존 unraveler를 `tools/mfe/`로 승격.** 현재는 스크래치패드 스크립트
      (`/tmp/claude-iterfuse/sp_unravel.py` + `sp_run.sh`)로만 존재한다. `run-termination.sh`의
      MTT C;A 경로를 이걸로 교체하고, 비-YES 시 MTT로 폴백하는 포트폴리오로 묶는 것이 목표.
- [ ] **모듈러B 회귀 3건 규명.** `$write_bits_from_value`,
      `$set_priorities_of_tableEntryListIR{,_prime}`가 구조 보존에서만 비-YES인 이유가
      keep-생성자의 항 크기 증가 때문인지 확인(가설). 확인되면 escape하지 않는 인자는
      keep에서 빼는 최적화로 닫힐 수 있다.

