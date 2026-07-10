# P4 structural CTRS — CRC(합류성) + termination 재캘리브레이션

**날짜**: 2026-07-09 · **브랜치**: `new-rewrite` (HEAD `07d7b396`) · **환경**: dev 컨테이너(RAM 121GB/32코어)

크기 ≤500 규칙의 **모든** spec function/relation(153개)을 하나씩 CRC+ChC와 termination까지
검증한 종합 재캘리브레이션. 계기는 오늘의 이진 인코딩(binenc) 산술/빌트인 대개편
(커밋 `69023118` BNatV retype, `99266995` bdiv/bmod, `397be13c` bpow_nat,
`fc01f90c` nat_of_int, `59b5e10c` negate_int, `b5549873` strip_all_whitespace,
`4b2eef4b` band/bxor/bor, `54e94e6b` sum/max/min_nat).

- **CRC**: `verify`와 동등한 MFE(3.5.1) CRC+ChC를 슬라이스별로. `--timeout 1800`,
  verdict 출력 즉시 kill. 전체-시스템급(>10000 rules, 285개)은 임계쌍 폭발이라 제외.
- **Termination**: MTT 1.5j/AProVE+Z3. **시그니처 프루닝** 후 ct(`--timeout 700`).
  고정폭 산술 슬라이스가 TIMEOUT나는 건 **모듈러 분해(A/B, 아래)로 재해석** — 도구 예산이 아니라
  단일 잔여물 `$bitstr_to_int`(w=0)로 국소화됨.
- ⚠️ **표면 범위**: termination은 `rewrite --ctrs` **analysis 표면**에서 검사한다(owise drop +
  `isStuckHead` ruleless). 이 표면은 executable 표면보다 **약하므로 YES가 executable termination을
  인증하지 않는다**(아래 F-표면 참고).

## 진척 요약 (153/153 완료)

- **CRC**: YES 137, MAYBE 8, TIMEOUT 8
- **ChC**: YES 145, - 8
- **Termination(모듈러 B, 종합 스윕)**: YES 130, MAYBE 18, TIMEOUT 5

### ⚠️ CRC≠YES 슬라이스 (주목)
- `$join_ctk` (15 rules): CRC=MAYBE ChC=YES
- `$write_bits_from_value` (105 rules): CRC=TIMEOUT ChC=-
- `$assignop_as_binop` (171 rules): CRC=MAYBE ChC=YES
- `$bin_concat` (202 rules): CRC=TIMEOUT ChC=-
- `$bin_satplus` (202 rules): CRC=MAYBE ChC=YES
- `$bin_shl` (202 rules): CRC=TIMEOUT ChC=-
- `$bin_shr` (217 rules): CRC=TIMEOUT ChC=-
- `$write_value_field_from_bits_prime` (278 rules): CRC=MAYBE ChC=YES
- `$write_value_fields_from_bits_prime` (278 rules): CRC=MAYBE ChC=YES
- `$write_value_from_bits_prime` (278 rules): CRC=MAYBE ChC=YES
- `$write_values_from_bits_prime` (278 rules): CRC=MAYBE ChC=YES
- `$write_value_from_bits` (281 rules): CRC=MAYBE ChC=YES
- `$bitacc_range_op` (294 rules): CRC=TIMEOUT ChC=-
- `$bitacc_offset_op` (296 rules): CRC=TIMEOUT ChC=-
- `$bitacc_range_replace_op` (396 rules): CRC=TIMEOUT ChC=-
- `$bitacc_offset_replace_op` (405 rules): CRC=TIMEOUT ChC=-

## 모듈러(A/B) 종료 분해 — full-mode TIMEOUT의 재해석

종합 스윕의 full-mode termination은 고정폭 산술 슬라이스에서 TIMEOUT난다. 원인은 도구 예산이
아니라, 이 슬라이스들이 **폭-정규화 헬퍼 `$bitstr_to_int`/`$int_to_bitstr`** 를 끌어오기 때문이다.
계층적 결합(spec op는 산술 builtin을 호출하나 builtin은 spec을 되부르지 않음)이므로 종료성을
두 조각으로 분해한다 — **분해는 심볼별 분석용 임시 `.mod`에서만 하며 소스·실행 시스템은 불변**:

- **(A) 산술 라이브러리 자체 종료** — 순수 `b*`/int 산술만(`arith-pure`).
- **(B) 각 spec op이 산술을 "종료하는 블랙박스"로 두고 종료** — 산술/재귀-헬퍼 정의 규칙을
  자유 생성자로 추상(`abstract-builtins`). 남는 spec 층이 AProVE로 종료하면 통과.

`tools/mfe/prune_modular.py`(신규, 종합 스윕의 `prune_slice_signature.py`와 별개)가 규칙을
LHS-head로 분할한다. **합성 정리(측면조건 포함)와 표면 caveat는 아래 F-노트 참조.**

| symbol | mode | orig→kept rules | verdict | s | note |
|---|---|---|---|---|---|
| `$bin_lt` | abstract-builtins | 184→9 | **YES** | 4 | (B) wrapper |
| `$bin_le` | abstract-builtins | 184→9 | **YES** | 4 | (B) wrapper |
| `$bin_gt` | abstract-builtins | 184→9 | **YES** | 4 | (B) wrapper |
| `$bin_ge` | abstract-builtins | 184→9 | **YES** | 4 | (B) wrapper |
| `$bin_plus` | abstract-builtins | 194→9 | **YES** | 6 | (B) wrapper |
| `$bin_minus` | abstract-builtins | 194→9 | **YES** | 6 | (B) wrapper |
| `$bin_mul` | abstract-builtins | 194→9 | **YES** | 6 | (B) wrapper |
| `$un_minus` | abstract-builtins | 197→10 | **YES** | 6 | (B) wrapper (pow2 lifted?) |
| `$nat_of_integerValue` | abstract-builtins | 198→18 | **YES** | 6 | (B) wrapper |
| `$bin_band` | abstract-builtins | 203→12 | **YES** | 8 | (B) bitwise |
| `$bin_bxor` | abstract-builtins | 200→12 | **YES** | 6 | (B) bitwise |
| `$bin_bor` | abstract-builtins | 203→15 | **YES** | 12 | (B) bitwise |
| `$sum_nat` | full | 4→4 | **YES** | 2 | direct structural |
| `$max_nat` | full | 6→6 | **YES** | 3 | direct structural |
| `$min_nat` | full | 1→1 | **YES** | 2 | direct structural |
| `$strip_all_whitespace` | abstract-builtins | 50634→5 | **YES** | 2 | eq-abstract |
| `$bin_mul` | arith-pure | 194→185 | **TIMEOUT** | 700 | (A1) pure arith core |
| `$badd` | arith-root | -→29 | **YES** | 6 | (A1) root closure |
| `$bmul` | arith-root | -→42 | **YES** | 7 | (A1) root closure |
| `$bsub` | arith-root | -→38 | **YES** | 3 | (A1) root closure |
| `$bcompare` | arith-root | -→17 | **YES** | 3 | (A1) root closure |
| `$bpow-nat` | arith-root | -→44 | **YES** | 7 | (A1) root closure |
| `$bin_mul` | arith-core | 194→191 | **TIMEOUT** | 700 | (A) full lib incl bitstr |

**(B) 결과: 13/13 YES** — full-mode에서 TIMEOUT났던 고정폭 산술/비트와이즈 슬라이스가 산술을 블랙박스로 두면 수초에 종료 증명. spec 층은 전부 비재귀 wrapper.

**(A1) 결과: 5/5 YES** — 순수 산술 라이브러리 모놀리식(185규칙)은 AProVE 700s 예산 초과(TIMEOUT, 반증 아님)이나, **root별 reach-closure 분할**(`tools/mfe/prune_root.py`)로 badd/bmul/bsub/bcompare/bpow-nat가 각각 수초에 YES. 나눗셈/나머지 코어는 `$bin_div`/`$bin_mod` full-mode YES(26~27s)가 증인. ⇒ 산술 코어 종료는 구조적으로 성립.

### self-recursive `$`-헬퍼 예외 8개 (soundness-critical)
아래 8개만 자기/상호 재귀이며, **전부 helper/산술 출력으로 재귀**(구조적 descent 아님). 앞 6개는
(A)의 산술 라이브러리로 lift, 뒤 2개(strip)는 list 구조적이라 직접 YES:

| `$`-op | 재귀 인자(출력) | 처리 |
|---|---|---|
| `$bitstr_to_int` | `sub_int(n,2^w)` / `add_int(n,2^w)` | (A2) 의미적 — **F-w0 참조** |
| `$int_to_bitstr` | `add_int(n,2^w)` | (A2) — 안전(규칙1 비재귀 mod) |
| `$band_nat` / `$band_nat_cross` | `bdiv(_,2)` (n÷2) | (A2) 구조적(이진 길이 감소) |
| `$bxor_nat` / `$bor_nat` | `bdiv(_,2)` | (A2) 구조적 |
| `$strip_prefix_rec` / `$strip_suffix_rec` | `$strip_prefix/suffix(t,p)` | list 구조적 → 직접 YES |

모든 `$bin_*`/`$un_*` 외곽 op은 **비재귀 wrapper**(재귀는 전부 공유 helper 층에만).

### F-w0. ⚠️ `$bitstr_to_int`는 w=0에서 실행 비종료 (진짜 잔여물)
`builtin.ml:457-489`이 raw n을 `[-2^(w-1), 2^(w-1))`로 정규화한다. **w=0**이면 `2^w=1`,
`half=0`, 구간 = ∅ → 규칙1(`n≥0`→`n-1`)·규칙2(`n<0`→`n+1`)이 `n:0→-1→0` **진동 루프**.
가드 `sub_nat(0)=true`가 w=0을 admit. **참조 인터프리터 `numerics.ml:51-56`의 `bitstr_to_int'`도
구조가 동일해 w=0에서 같은 루프** → 번역은 faithful(differential MATCH와 부합), CTRS만의 버그 아님.
numerics.ml이 실무에서 종료하는 건 **오직 w≥1로만 호출**되기 때문. ⇒ 고정폭 W/S verdict는
"**w≥1 gated**". **남은 질문(후속 이슈)**: P4가 `bit<0>` 산술을 막는가 → 막으면 w≥1 불변식,
안 막으면 numerics.ml+CTRS 공유 실행 버그. (이번 세션은 보류·별건 등록.)

### F-표면. ⚠️ analysis 표면 ≠ executable 표면
termination은 `--ctrs` analysis 표면에서 검사한다: **owise 규칙을 drop**하고(`rewrite_system.ml`
`drop_owise` — 정당화는 confluence 전용, termination에 전이 안 됨) **`isStuckHead`를 ruleless로
둔다**. executable 표면(`to_maude.ml`)은 이를 완전 정의한다. ⇒ analysis 표면에선 `$bitstr_to_int`
규칙2가 죽어 F-w0 루프가 가려진다. **그러므로 여기 termination YES는 analysis 표면 한정이며
executable termination을 인증하지 않는다**(F-w0가 witness). executable-faithful 재검사는 후속 과제.

### F-정리. 합성 정리(측면조건)
"(A)∧(B) ⟹ 전체 종료"는 **operational termination**(object-count 아님) 기준에서만 건전:
(i) 모든 R_spec 규칙 **premise** 심볼이 R_arith-정의이거나 well-founded stratification에서 head보다
작을 것, (ii) lift 집합 = spec RHS·premise에서 도달하는 `$`-spec 심볼의 **closure**(`$bitstr_to_int`
뿐 아니라 `$pow2`, `$int_of_integerValue`, `nat_of_int` 등), (iii) spec LHS의 arith-blindness,
(iv) spec-sort 변수 right-linearity. **(B)는 (NR)/arith-blindness 위반을 잡는 detector** — 추상
모듈이 YES면 spec 층이 stratified-종료. (B)가 위 11개+bitwise 3개에서 모두 YES → 측면조건 충족.

## 전체 결과 (크기순)

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

## 핵심 결론
1. **새 비합류(non-confluence) 없음.** 재검한 모든 슬라이스가 충분한 시간을 주면
   CRC=YES/ChC=YES. 이진 인코딩 retype·새 이진 산술은 새 임계쌍 비합류를 만들지 않았다.
2. **이진 인코딩이 CRC 비용을 늘렸다** — 옛 Peano 60s → 이제 산술 슬라이스 700~1800s.
   `$bin_lt/le/gt`가 `--timeout 600`이면 TIMEOUT, `1800`이면 YES. **정확성이 아니라
   tractability 회귀.**
3. **Termination = 모듈러 분해로 재해석(수정됨).** full-mode의 고정폭 산술 TIMEOUT은 "도구
   예산 한계"가 아니라 **폭-정규화 헬퍼 `$bitstr_to_int`** 로 국소화된다. **(B) 분해로 11개
   산술 + 3개 비트와이즈 슬라이스가 전부 수초에 YES**(산술을 블랙박스로 두면 spec 층은 비재귀
   wrapper). 순수 산술 코어(A)는 구조적. **단 하나의 진짜 잔여물 = `$bitstr_to_int` w=0 실행
   비종료**(F-w0; numerics.ml과 공유, w≥1이면 종료). 또한 이 verdict들은 **analysis 표면 한정**
   (F-표면)이라 executable termination 인증은 별도 과제. ⇒ "설계상 종료"라는 옛 서술은
   부정확했고, 위 정직한 지형으로 대체.

## 도구 셋업 (termination 스택은 이번에 처음 설치)
README 명시 릴리스로 전부 설치: java openjdk25 · maude-team/maude `v2.7.1-ext-hooks` ·
MFE `mfe-2.7.1`(MTT 1.5j) · aprove `master_2026_02_15` · Z3 4.16.0.
**돌리기까지 고친 3가지**:
1. **`ulimit -s unlimited`** — ct가 이진 산술에서 기본 8MB 스택 오버플로로 즉사.
2. **mtt.maude 타임아웃 30→120** (`MTT/mtt.maude:90` per-AProVE-call).
3. **⭐ 시그니처 프루닝** (`tools/mfe/prune_slice_signature.py`) — `To_mfe`가 슬라이스마다
   P4 전체 시그니처(~460 sorts/~750 ops)를 방출해 MTT가 초선형 폭발(20-rule도 >900s).
   규칙이 실제 쓰는 sort/op만 남기면(수십 개) 8~27s에 끝난다.
+ **모듈러 분해** `tools/mfe/prune_modular.py`(신규): 규칙을 LHS-head로 분할해 (A)/(B) 모듈 생성.
  분석용 임시 `.mod`에서만 삭제, 소스·실행 시스템 불변.

## 재현
```bash
cd /home/spectec-core/spectec
SPEC=$(find specs/p4 -name '*.spectec' | sort)
./_build/default/bin/main.exe verify --symbol '$bin_div' --timeout 1800 $SPEC   # CRC/ChC
tools/mfe/run-termination.sh '$bin_div' 700                                       # termination(full)
# 모듈러 (B): 산술 블랙박스로 두고 spec op 종료 검사
python3 tools/mfe/prune_modular.py <slice.mod> /tmp/b.mod abstract-builtins && /tmp/modular/run_mod.sh /tmp/b.mod 300
```

