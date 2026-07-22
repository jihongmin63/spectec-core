# P4 structural CTRS — 검증 결과 (표)

CRC/ChC = Church-Rosser(합류성)/Coherence. 값 = YES / MAYBE / TIMEOUT / `-`(미도달).
측정 이력·방법·MAYBE/TIMEOUT 해석은 **[verification-notes.md](verification-notes.md)** 참조.

> **재현 커맨드 (2026-07-22 in-binary 통합, tip `57a99547`).** 이 표의 축들은 이제
> 스크립트·스크래치패드 없이 바이너리 서브커맨드로 전수 재현한다. 세 분석 검사기는
> 동일한 스윕 표면을 갖는다 — `--symbol NAME`(반복) 또는 `--all`(작은 슬라이스 먼저),
> `--out sweep.tsv`(이미 기록된 심볼 skip → 재개 가능):
>
> - `term` = `main.exe termination --all --out term.tsv <specs>` — 구조 보존 unravel →
>   AProVE(`lib/rewrite/unravel.ml`+`aprove.ml`). 단일: `termination --symbol NAME`,
>   TRS만 보려면 `--emit-trs --symbol NAME`.
> - `CRC`/`ChC` = `main.exe confluence --all --out crc.tsv <specs>`(구 `verify`). 행
>   `<sym>\t<church-rosser>\t<coherence>`. MAYBE/TIMEOUT은 `--crc-normalize`로 upgrade-only
>   재검(정규화+prune, YES면 `YES (normalized)`, 하향 없음).
> - 충분완전성(SCC) = `main.exe scc --all --out scc.tsv <specs>` — CETA Maude 2.7 필요.
> - 슬라이스 목록/크기 = `main.exe rewrite --list-symbols [--sizes] <specs>`,
>   분석 모듈 덤프 = `rewrite --ctrs --symbol NAME [--prune-signature]`.
>
> 구 셸/파이썬 드라이버(`run-termination.sh`/`run-scc.sh`/`run-scc-sweep.sh`/
> `prune_slice_signature.py`/`prune_modular.py`/`prune_root.py`)는 전부 위 커맨드로
> 대체·**삭제 완료**(git history에서 부활 가능). differential 드라이버
> `check_diff_p4.sh`/`check_diff_structural_p4.sh`는 서브커맨드 대체물이 없어 유지.

## 1. ≤500 종합 (153심볼)

**`term` 열은 2026-07-19에 MTT를 걷어낸 새 방법으로 전수 재측정했다**
(바이너리 `30d413ad` = iter-fuse의 IterPr 헬퍼 통합 반영, 슬라이스는 직전 런과
byte-identical, `/tmp/claude-iterfuse/sp`, 153/153). `rules` 열은 그 직전 재측정 값 그대로다.
`CRC`/`ChC`는 **2026-07-18 측정 유지**(바이너리 `2f9f8cba`, `/tmp/fresh500`) — 이번 재검은
term 축만 대상이었다(§ 하단 TODO).

> ⚠️ **측정 기준 커밋 주의.** `term` 열은 `30d413ad` 덤프 기준이고, 그 이후 new-rewrite에
> 술어 도메인을 사용처에서 복원하는 변경(`6e740f3e` 계열, `spec.ctrs`/`spec.maude` 골든 동반
> 변경)이 들어왔다. 구조 보존 경로는 sort 태그를 버리고 규칙만 읽으므로 판정이 바뀌지 않을
> 공산이 크지만 **미확인**이다 — 아래 TODO 참조.

**`term` = 구조 보존 unraveling → AProVE 직접.** 슬라이스의 조건부 규칙을 좌변 인자
목록을 정의 규칙 없는 불활성 생성자 `k_N`에 감싸 넘기는 방식으로 평범한 TRS로 만든 뒤,
`tools/aprove/runme <f>.trs <budget>`(WST 모드)에 바로 던진다.
`f(p1..pk) -> u(s, k(p1..pk))` / `u(t, k(p1..pk)) -> r` 꼴이다.

**MTT는 쓰지 않는다.** MTT는 unravel을 하지 않고 조건부 TRS를 그대로 AProVE에 넘기면서,
조건 `s = t`를 **`equal(s,t) -> tt`** 로 바꾼다(전역 규칙 `equal(X,X) -> tt` 동반). 우리
조건은 *매칭* 조건이라 `text = cons(t_h2, t_t)`의 `t_h2`/`t_t`가 매칭으로 바인딩되는데,
대칭 동등성 검사로 바뀌면 **자유 변수**가 되어 좌변에 나타나지 않는다 — extra-variable
CTRS(Bergstra–Klop 3형)다. 재귀 인자 `cons(t_h2,t_t)`가 좌변 인자와 구문적 관계가 없어
dependency pair로 정렬할 하강이 보이지 않고, 3형 종료 증명은 1/2형보다 훨씬 어렵다.
unravel하면 `t`가 다시 **규칙 좌변의 패턴**이 되어 변수가 매칭으로 바인딩되고 하강이
구문적으로 드러난다 — 이것이 이득의 전부다.
**무엇을 날라 주는지는 무관하다**: Marchiori 고전형(좌변 *변수*를 평평하게)과 구조 보존형
(좌변 *인자 목록*을 `k_N`에 그대로)을 같은 슬라이스·같은 AProVE로 맞대조하면 **둘 다
0~1초에 YES**다.
MTT 인코딩이 증명 *불가능*한 건 아니고 **비쌀 뿐**이다 — 캡처본에 예산을 충분히 주면 YES가
나온다. 다만 `mtt.maude:90`이 AProVE를 **120초로 하드코딩** 호출하므로 그 비용이 곧바로
MAYBE가 된다(우리가 준 1200s는 Maude *프로세스* 타임아웃이라 무관했다 — 예산을 늘려도
판정이 안 바뀌던 이유다). 13-rule짜리 `$join_text`가 MTT 인코딩에서 >120초, 우리 TRS에서
1초로 100배 이상 차이다.
상세·건전성 논증·구현 함정은 [CLAUDE.md](CLAUDE.md) "Do not route termination through MTT",
경위와 측정 전문은 [lib/rewrite/todo.md](spectec/lib/rewrite/todo.md) 2026-07-19 research note.

- **CRC**: YES 140 / TIMEOUT 8 / MAYBE 5   *(baseline `2f9f8cba` plain CRC)*  ·  **ChC**: YES 145 / - 8
  - 표에서 **`YES*`** = `--crc-normalize`(upgrade-only)로 닫힌 것. MAYBE 5(전부 `$write_value*`)와
    `$bin_concat`(TIMEOUT) → YES ⇒ **정규화 적용 시 CRC YES 146 / TIMEOUT 7 / MAYBE 0**. 남은
    TIMEOUT 7(`$write_bits_from_value`·`$bin_shl`·`$bin_shr`·`$bitacc_*`×4)은 현 바이너리로 미재측정.
    기전은 아래 "CRC 정규화".
- **term**: YES **153 / 153**
- **MTT 경로였을 때는 YES 117 / MAYBE 12 / TIMEOUT 24였다.** MAYBE **12건 전부**와 TIMEOUT
  24건 중 **23건**이 닫혔고, 이진 산술 계열(`$bin_*`·`$un_*`·`$bitacc_*`·`$write_value*`)이
  통째로 풀렸다. **인코딩은 한 줄도 고치지 않았다** — 원인은 번역도 AProVE도 아닌 MTT였다.
- 종전 이 표가 "AProVE 자동 전략의 도구 한계"로 적었던 3건(`$join_text`,
  `$invalidate_value`, `$invalidate_headerUnion`)은 MTT로 1200s를 소진하고 MAYBE였으나
  새 경로에서 **각 1초에 YES**다. 그 진단은 **틀렸다**.
- ⚠️ 이 항목의 인과 설명은 2026-07-20에 정정됐다. 최초 서술은 "MTT의 unraveling이 인자를
  분해해 부분항 관계를 역전시키므로 어떤 예산으로도 증명 불가"였는데, 세 군데가 틀렸다
  (MTT는 unravel을 안 하고, 분해도 역전도 없으며, 예산만 주면 증명된다). 파이프라인 전체를
  맞대조한 뒤 그 안의 한 요소를 원인으로 지목한 것이 실수였다. **측정값(153/153)과 실무
  결론(MTT를 거치지 말 것)은 불변**이고 기전만 틀렸었다. 경위는
  [todo.md](spectec/lib/rewrite/todo.md) "방법론 반성" 참조.
- 종전 병기하던 `term(모듈러B)` 열(`prune_modular.py abstract-builtins`로 산술을 블랙박스
  처리해 spec 층만 증명하던 축)은 **폐지했다.** 그 축은 MTT가 full-arith를 못 뚫는 걸
  우회하려고 둔 것인데, 직접 축이 153/153이 된 이상 존재 이유가 없다. 참고로 같은 구조
  보존 경로를 모듈러 축에 적용하면 YES 150 / MAYBE 2 / TIMEOUT 1로, 오히려 직접 축보다
  나쁘다(keep-생성자가 인자 구조를 복제해 항이 커지는 비용이 산술 블랙박싱의 이득을
  넘어선다). 원자료는 `/tmp/claude-iterfuse/sp/results.tsv`.
- 이와 함께 종전의 `new-commit helpers` 열(각 심볼이 쓰는 산술 빌트인 — 폐지된 모듈러 축에서
  `abstract-builtins`가 블랙박싱하던 대상)도 표에서 제거했다. 모듈러 축을 안 쓰므로 무의미.
- 예산: AProVE 자체 예산 300s로 스윕하고, 비-YES만 MTT와 같은 1200s로 재측정했다
  (`stage2.tsv`). 1200s가 필요했던 건 `$write_bits_from_value` 1건뿐이다.
- `rules` 열이 종전과 다른 7행은 헬퍼 통합의 직접 효과다(변수별 collect 병합으로 감소:
  `$resolve_constraint` 13→9, `$invalidate_value`/`$invalidate_headerUnion` 91→87,
  `$write_bits_from_value` 107→103; 다출력 ex-apply의 튜플화로 증가: `$callableId_IR` 11→13,
  `$callableId_of_extern{Constructor,Method}PrototypeIR` 12→14 / 13→15).

### CRC 정규화 (`--crc-normalize`, upgrade-only) — 2026-07-21

baseline CRC(위 `2f9f8cba` 열)의 MAYBE 5(전부 `$write_value*`)를 분석-전용 정규화
(inline + unravel + real-sort, **upgrade-only**)로 닫았다. 실측(real-sort 바이너리, `prune`
동반, 직렬 Maude, 조기종료):

- **MAYBE 5 → YES 0쌍**: `$write_value_from_bits`(142s)·`_prime`(134s),
  `$write_value_field_from_bits_prime`(139s), `$write_value_fields_from_bits_prime`(138s),
  `$write_values_from_bits_prime`(139s) — 표의 MAYBE 5와 정확히 일치.
- **회귀**: `$un_op`·`$bin_bor` YES 유지(inline-only, crcu 없어 byte-identical).
  `$set_priorities_of_tableEntryListIR` real-sort로 YES 0쌍 78s(all-Val이면 TIMEOUT).
  `$join_text`는 초기엔 정규화 시 YES→MAYBE였으나, 이는 `crc_unravel`이 값-destructure
  (`text = cons(..)`, subject가 변수)를 불필요하게 unravel해 hoist_matchers가 만든
  CRC-friendly 형태를 깬 **over-unravel**이었다. subject가 정의 함수일 때만 unravel하도록
  게이트(`1dd1e43a`)해 **YES 0쌍 6s**로 교정.
- **TIMEOUT 회수**: `$bin_concat` inline+prune으로 TIMEOUT→YES 259s. `$bin_shl`/`$bin_shr` 잔존.

세 레버(inline=등식·blanket / unravel=reflect-only·upgrade-only / real-sort=건전한 narrowing)·
건전성 방향·`crcu`/`crck` sort 복원·upgrade-only 프로토콜·`$join_text` 기전은
[CLAUDE.md](CLAUDE.md) "CRC normalization (`--crc-normalize`)" 참조.

| symbol | rules | CRC | ChC | term |
|---|---|---|---|---|
| `$annotationList_of_parameterIR` | 1 | YES | YES | YES |
| `$ctk_of_typedExpressionIR` | 1 | YES | YES | YES |
| `$empty_map` | 1 | YES | YES | YES |
| `$empty_set` | 1 | YES | YES | YES |
| `$empty_tableContext` | 1 | YES | YES | YES |
| `$id_of_parameterIR` | 1 | YES | YES | YES |
| `$invalidate_header` | 1 | YES | YES | YES |
| `$parameterListIR_of_actionDef` | 1 | YES | YES | YES |
| `$parameterListIR_of_actionTypeDefIR` | 1 | YES | YES | YES |
| `$parameterListIR_of_constructorTypeDefIR` | 1 | YES | YES | YES |
| `$parameterListIR_of_controlApplyMethodDef` | 1 | YES | YES | YES |
| `$parameterListIR_of_controlApplyMethodTypeIR` | 1 | YES | YES | YES |
| `$parameterListIR_of_definedFunctionDef` | 1 | YES | YES | YES |
| `$parameterListIR_of_definedFunctionTypeDefIR` | 1 | YES | YES | YES |
| `$parameterListIR_of_externFunctionDef` | 1 | YES | YES | YES |
| `$parameterListIR_of_externFunctionTypeDefIR` | 1 | YES | YES | YES |
| `$parameterListIR_of_parserApplyMethodDef` | 1 | YES | YES | YES |
| `$parameterListIR_of_parserApplyMethodTypeIR` | 1 | YES | YES | YES |
| `$parameterListIR_of_tableApplyMethodDef` | 1 | YES | YES | YES |
| `$parameterListIR_of_tableApplyMethodTypeDefIR` | 1 | YES | YES | YES |
| `$tableEntryPriorityOptIR_of_tableEntryIR` | 1 | YES | YES | YES |
| `$type_of_typedExpressionIR` | 1 | YES | YES | YES |
| `$type_of_typedLvalueIR` | 1 | YES | YES | YES |
| `$parameterListIR_of_externMethodDef` | 1 | YES | YES | YES |
| `$set_priority_of_tableEntryIR` | 1 | YES | YES | YES |
| `$empty_callableDefEnv` | 2 | YES | YES | YES |
| `$empty_callableTypeDefEnv` | 2 | YES | YES | YES |
| `$empty_constructorDefEnv` | 2 | YES | YES | YES |
| `$empty_constructorTypeDefEnv` | 2 | YES | YES | YES |
| `$empty_frame` | 2 | YES | YES | YES |
| `$empty_stateEnv` | 2 | YES | YES | YES |
| `$empty_store` | 2 | YES | YES | YES |
| `$empty_theta` | 2 | YES | YES | YES |
| `$empty_typeDefEnv` | 2 | YES | YES | YES |
| `$empty_typeFrame` | 2 | YES | YES | YES |
| `$flatten_constOpt` | 2 | YES | YES | YES |
| `$ite` | 2 | YES | YES | YES |
| `$flatten_objectInitializerOptIR` | 2 | YES | YES | YES |
| `$is_some` | 2 | YES | YES | YES |
| `$opt_as_seq` | 2 | YES | YES | YES |
| `$parameterListIR_of_externMethodTypeDefIR` | 2 | YES | YES | YES |
| `$type_of_externMethodPrototypeIR` | 2 | YES | YES | YES |
| `$callable_builtinMethod` | 3 | YES | YES | YES |
| `$constructorTypeDef_of_externConstructorPrototypeIR` | 3 | YES | YES | YES |
| `$constructor_of_externConstructorPrototypeIR` | 3 | YES | YES | YES |
| `$empty_constraint` | 3 | YES | YES | YES |
| `$instantiable_extern` | 3 | YES | YES | YES |
| `$is_lpm_key_prime` | 3 | YES | YES | YES |
| `$un_lnot` | 3 | YES | YES | YES |
| `$join_tableEntryState` | 3 | YES | YES | YES |
| `$filter` | 3 | YES | YES | YES |
| `$is_concrete_extern_object_prime_prime` | 4 | YES | YES | YES |
| `$is_default_parameterIR` | 4 | YES | YES | YES |
| `$is_lpm_key` | 4 | YES | YES | YES |
| `$concat_text` | 4 | YES | YES | YES |
| `$exists` | 4 | YES | YES | YES |
| `$flatten_blockElementStatementList` | 4 | YES | YES | YES |
| `$flatten_controlLocalDeclarationList` | 4 | YES | YES | YES |
| `$flatten_externConstructorOrMethodPrototypeList` | 4 | YES | YES | YES |
| `$flatten_objectDeclarationList` | 4 | YES | YES | YES |
| `$flatten_parserLocalDeclarationList` | 4 | YES | YES | YES |
| `$flatten_parserStatementList` | 4 | YES | YES | YES |
| `$flatten_selectCaseList` | 4 | YES | YES | YES |
| `$flatten_switchCaseList` | 4 | YES | YES | YES |
| `$flatten_tableActionList` | 4 | YES | YES | YES |
| `$flatten_tableEntryList` | 4 | YES | YES | YES |
| `$flatten_tableKeyList` | 4 | YES | YES | YES |
| `$flatten_tablePropertyList` | 4 | YES | YES | YES |
| `$flatten_typeFieldList` | 4 | YES | YES | YES |
| `$forall` | 4 | YES | YES | YES |
| `$join_flow` | 4 | YES | YES | YES |
| `$flatten_prefixedNameIR` | 4 | YES | YES | YES |
| `$add_action_tbl` | 5 | YES | YES | YES |
| `$add_key_tbl` | 5 | YES | YES | YES |
| `$codom_map` | 5 | YES | YES | YES |
| `$dom_map` | 5 | YES | YES | YES |
| `$enter_path_i` | 5 | YES | YES | YES |
| `$flatten_p4program` | 5 | YES | YES | YES |
| `$empty_typingContext` | 6 | YES | YES | YES |
| `$is_tableActionsProperty` | 6 | YES | YES | YES |
| `$is_tableKeysProperty` | 6 | YES | YES | YES |
| `$tableCustomName` | 6 | YES | YES | YES |
| `$enter_i` | 7 | YES | YES | YES |
| `$enter_t` | 7 | YES | YES | YES |
| `$exit_i` | 7 | YES | YES | YES |
| `$exit_t` | 7 | YES | YES | YES |
| `$requires_priority_prime` | 7 | YES | YES | YES |
| `$empty_instContext` | 8 | YES | YES | YES |
| `$requires_priority` | 8 | YES | YES | YES |
| `$typedLvalueIR_as_typedExpressionIR` | 8 | YES | YES | YES |
| `$join_ctk` | 9 | YES | YES | YES |
| `$width_of_integerTypeIR` | 10 | YES | YES | YES |
| `$inherit_i` | 10 | YES | YES | YES |
| `$name` | 10 | YES | YES | YES |
| `$un_plus` | 11 | YES | YES | YES |
| `$callableId_IR` | 13 | YES | YES | YES |
| `$objectId_ends_with` | 11 | YES | YES | YES |
| `$callableId_of_externConstructorPrototypeIR` | 14 | YES | YES | YES |
| `$prefixedTypeName` | 12 | YES | YES | YES |
| `$join_text` | 13 | YES | YES | YES |
| `$resolve_constraint` | 9 | YES | YES | YES |
| `$callableId_of_externMethodPrototypeIR` | 15 | YES | YES | YES |
| `$flatten_nameList` | 13 | YES | YES | YES |
| `$flatten_typeParameterList` | 13 | YES | YES | YES |
| `$assignop_as_binop` | 13 | YES | YES | YES |
| `$flatten_typeParameterListOpt` | 15 | YES | YES | YES |
| `$is_tableDefaultActionProperty` | 16 | YES | YES | YES |
| `$prefixedNonTypeName` | 19 | YES | YES | YES |
| `$optional_annotation_of_parameterIR_prime_prime` | 20 | YES | YES | YES |
| `$lvalue_as_expression` | 22 | YES | YES | YES |
| `$starts_with` | 50 | YES | YES | YES |
| `$strip_prefix_rec` | 53 | YES | YES | YES |
| `$isValid_header` | 80 | YES | YES | YES |
| `$ends_with` | 88 | YES | YES | YES |
| `$strip_suffix_rec` | 91 | YES | YES | YES |
| `$invalidate_headerUnion` | 87 | YES | YES | YES |
| `$invalidate_value` | 87 | YES | YES | YES |
| `$write_bits_from_value` | 103 | TIMEOUT | - | YES |
| `$bin_mod` | 109 | YES | YES | YES |
| `$bin_div` | 113 | YES | YES | YES |
| `$un_bnot` | 139 | YES | YES | YES |
| `$bin_ge` | 183 | YES | YES | YES |
| `$bin_le` | 183 | YES | YES | YES |
| `$bin_gt` | 184 | YES | YES | YES |
| `$bin_lt` | 184 | YES | YES | YES |
| `$int_of_integerValue` | 184 | YES | YES | YES |
| `$nat_of_integerValue` | 187 | YES | YES | YES |
| `$bin_minus` | 193 | YES | YES | YES |
| `$bin_mul` | 193 | YES | YES | YES |
| `$bin_plus` | 193 | YES | YES | YES |
| `$un_minus` | 197 | YES | YES | YES |
| `$bin_bxor` | 199 | YES | YES | YES |
| `$bin_concat` | 200 | YES* | - | YES |
| `$set_priorities_of_tableEntryListIR_prime` | 200 | YES | YES | YES |
| `$bin_satminus` | 201 | YES | YES | YES |
| `$bin_satplus` | 201 | YES | YES | YES |
| `$bin_shl` | 201 | TIMEOUT | - | YES |
| `$bin_band` | 202 | YES | YES | YES |
| `$bin_bor` | 202 | YES | YES | YES |
| `$name_annotationToken` | 209 | YES | YES | YES |
| `$un_op` | 209 | YES | YES | YES |
| `$bin_shr` | 215 | TIMEOUT | - | YES |
| `$set_priorities_of_tableEntryListIR` | 226 | YES | YES | YES |
| `$name_annotation_opt` | 256 | YES | YES | YES |
| `$write_value_field_from_bits_prime` | 271 | YES* | YES | YES |
| `$write_value_fields_from_bits_prime` | 271 | YES* | YES | YES |
| `$write_value_from_bits_prime` | 271 | YES* | YES | YES |
| `$write_values_from_bits_prime` | 271 | YES* | YES | YES |
| `$write_value_from_bits` | 274 | YES* | YES | YES |
| `$bitacc_range_op` | 283 | TIMEOUT | - | YES |
| `$bitacc_offset_op` | 285 | TIMEOUT | - | YES |
| `$bitacc_range_replace_op` | 391 | TIMEOUT | - | YES |
| `$bitacc_offset_replace_op` | 394 | TIMEOUT | - | YES |

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

- [x] **≤500 `term` 열 재측정 + 모듈러B 축 폐지** (2026-07-19 완료). MTT를 걷어내고 구조
  보존 unraveling → AProVE 직접으로 153/153 YES. 두 축 병기가 불필요해져 단일 `term` 열로
  합쳤다 — 경위는 §1 요약과 [todo.md](spectec/lib/rewrite/todo.md) 2026-07-19 research note.
- [ ] **≤500 `CRC`/`ChC` 열 재측정.** 이 두 열만 아직 `2f9f8cba` 기준이라, 같은 표의
  `rules`/term 열(`30d413ad`)과 측정 기준이 다르다. 통합이 CRC에 미치는 영향은
  표본으로만 확인됨(head-side·k=1 collect YES/YES, k≥2 소비자는 통합 직전 커밋에서도
  동일 TIMEOUT = 무회귀). 전수 재검은 미실시.
- [ ] **§2 >500 표 fresh 값 갱신.** bigfresh(현재 진행) 완주 시 27행 stale 표를 교체.
      term(B) 열은 MTT 경로 값이므로, 갱신 시 §1과 같은 구조 보존 경로로 재측정할 것
      (§1에서 MAYBE 11건 중 상당수가 닫힐 가능성이 높다).
- [x] **`term` 열의 기준 커밋 확인** (2026-07-22 완료). HEAD에서 153 슬라이스를 재덤프해
      unravel한 TRS가 측정 당시(`30d413ad`) 골든 TRS와 **153/153 byte-identical** —
      술어 도메인 변경은 sort 태그에만 닿았고, sort를 지우는 구조 보존 경로에는 불변.
      판정은 그대로 carry된다(재측정 불필요 확정).
- [x] **구조 보존 unraveler 승격** (2026-07-22 완료). 스크립트 승격 대신 in-binary로
      완전 포팅: `main.exe termination`(lib/rewrite/unravel.ml + aprove.ml). 10개 다양
      슬라이스에서 sp_unravel.py 출력과 byte-identical, trsA 골든과도 일치 검증.
      MTT 경로(run-termination.sh)는 폴백 없이 폐기(커밋 d3bf2847).
- [x] **실행 경로 커맨드 통합** (2026-07-22 완료, tip `57a99547`). 검증 실행이 세 분석
      서브커맨드로 모임 — `confluence`(구 verify)/`termination`/`scc`가 동일 스윕 표면
      (`--symbol`/`--all`/`--out`), 심볼 리스팅은 `rewrite --list-symbols [--sizes]`,
      프루닝은 `rewrite --ctrs --prune-signature`, upgrade는 `confluence --crc-normalize`.
      위 "재현 커맨드" 콜아웃 참조.
- [x] **폐기 스크립트 삭제** (2026-07-22 완료). `run-scc.sh`/`run-scc-sweep.sh`
      (→ `scc` 서브커맨드), `prune_slice_signature.py`(→ `rewrite --prune-signature`)
      삭제. 삭제 시점의 reverify 스윕은 이미 새 `confluence --all --crc-normalize`(in-binary
      프루닝)로 옮겨가 python 프루너를 더는 호출하지 않아 게이트 해소됨.
      **미검증 caveat**: `scc` 실 verdict는 CETA Maude 2.7 에셋 부재로 옛 `run-scc.sh`와
      행 diff를 못 했다(모듈 방출 텍스트는 cram `scc --emit`로 byte 확인). 에셋 확보 시
      `run-scc.sh`를 git history에서 부활시켜 대조할 수 있다.
- [ ] **keep-생성자 항 크기 최적화(선택).** 폐지한 모듈러 축에서 구조 보존이 MTT보다
      나빴던 3건(`$write_bits_from_value`, `$set_priorities_of_tableEntryListIR{,_prime}`)의
      원인이 keep-생성자의 인자 복제로 인한 항 크기 증가인지 확인(가설). 맞다면 escape하지
      않는 인자를 keep에서 빼면 닫힌다. §1 직접 축은 이미 153/153이라 급하지 않다.

