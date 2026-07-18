# P4 structural CTRS 검증 — 설명·측정 이력·해석

결과 표는 **[verification.md](verification.md)**. 이 문서는 방법론, 커밋별 측정 이력,
그리고 비-YES 행의 해석(왜 도구 한계이지 실제 결함이 아닌지)을 정리한다.

## 종합 스윕 개요

≤500규칙 153심볼을 CRC/ChC + 종료로 검증. term 열(§2 표)은 모듈러(B) 종료 측정치.

## 측정 이력 (커밋별)

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

**전체 슬라이스 재측정 결과 (2026-07-13, 같은 날) — 표 CRC 열 변화 없음.** 위
실측은 **축소 슬라이스**이고 표는 **전체 슬라이스** 측정값이라 서로 다른 측정이다.
CRC=MAYBE인 재측정 가능 3행을 전체 슬라이스로 다시 돌렸다(`verify --symbol`):
- `$bin_satplus` → **CRC TIMEOUT**(과거 MAYBE에서 이 WSL 환경 악화로 완주 실패;
  1900s 예산 소진). align_guards가 sign-split MAYBE의 **원인은 제거**했음이 축소
  슬라이스로 확인됐으나, 전체 202규칙 슬라이스는 산술 라이브러리(`badd`/`bmul`)
  임계쌍 폭발이 지배적이라 verdict가 그 아래 가린다.
- `$join_ctk` → **MAYBE**, `$assignop_as_binop` → **MAYBE** (둘 다 무변화). 이
  둘은 sign-split이 아니라 match fall-through라 애초에 align_guards 대상이 아니며,
  YES→MAYBE 역행이 없음(무회귀)도 이 재측정으로 확인됐다.
- `$write_value*` 5행은 이 환경에서 CRC가 완주하지 못해(1800s 다중 TIMEOUT)
  재측정 불가.

⇒ **align_guards의 순이익은 축소 슬라이스 CRC(sign-split 원인 제거)에서만 실측되고,
전체 슬라이스 CRC verdict로는 산술 CP 폭발/환경 악화에 가려 드러나지 않는다.** 표의
`$bin_satplus`/`$bin_shr` CRC 열은 더 빠른 환경의 옛 측정값을 유지한다(위
`$write_value*`와 같은 처리) — 다음 안정 환경에서 재측정 필요.

**`$join_ctk`/`$assignop_as_binop` CRC 열 재측정 (2026-07-16) — MAYBE → YES.** 위
7/13 재측정에서 "match fall-through라 align_guards 대상이 아니다"로 남겨뒀던 이
두 행의 진짜 원인을 이번에 확정: 진짜 비합류가 아니라, owise 반사가 만드는
왼쪽-중첩 `or(and(match,match)…)=false` 게이트에서 참 disjunct가 깊이 묻히면 CRC의
feasibility 검사가 못 보는 **인코딩 아티팩트**였다(todo.md M1 2026-07-15 진단).
수정: `Reflect.owise`에 **complement 열거**를 구현(enum-dispatch owise를 or-gate
대신 미매치 생성자 튜플별 ground fall-through 절로 반사 → 절이 전부 ground·서로소 →
임계쌍 소멸). 전체 슬라이스 `verify --symbol`로 재측정: `$join_ctk` **CRC YES**(5절),
`$assignop_as_binop` **CRC YES**(1절). 회귀 없음 확인(`$join_flow`도 같은 패스로
새로 열거됐고 기존 YES 유지, impty `$lookup` 대조군 YES, 실행 표면 sha256 불변).
상세는 todo.md M1 2026-07-16 항목.

## 2026-07-18 post-fix 전면 fresh 재검증 (CRC + AProVE 직접)

위 재측정들은 7-09/7-10 번역 덤프를 재사용했다 — owise-complement 열거(`2f9f8cba`)·
align_guards(`a290977b`) **이전** 산출물이다. fix는 번역 덤프 자체를 바꾸므로, 스테일
스윕(smallcheck/bigsweep)을 전부 중단하고 **현재 바이너리(HEAD `2f9f8cba`)로 153심볼을
하나씩 재번역**해 CRC를 처음부터 다시 계산했고, termination은 **모듈러-B가 아니라 AProVE
직접**(시그니처 프루닝 → old-FullMaude fmod → `ct`, `TERM_TMO=1200`, `CRC_TMO=2400`)으로
다시 돌렸다. `/tmp/fresh500`, **153/153 완주**.

**요약** — CRC: YES 140 / TIMEOUT 8 / MAYBE 5 · ChC: YES 145 / - 8 · term(AProVE 직접): YES 117 / TIMEOUT 25 / MAYBE 11.

1. **fix 정정, 전면 스윕에서 재확인.** 스테일에서 MAYBE였던 `$join_ctk`(9절)·
   `$assignop_as_binop`(13절)이 fresh 전면 재검에서 **CRC=YES / ChC=YES / term=YES**.
   위 7-16 `verify --symbol` 단발 측정이 독립 전면 스윕에서 재현됨. **회귀 0.**
2. **새 비합류/비종료 후보 0.** CRC=YES 140개. 비-YES 37행은 전부 산술/비트 슬라이스이며,
   아래 두 종류의 알려진 도구 한계다.
3. **⭐ term 열은 여기서 AProVE 직접이다 — 모듈러-B와 다르다.** full-arith 25개 TIMEOUT
   (`$bin_minus/mul/plus/satplus/satminus/band/bor/bxor`, `$un_minus/op`, …)은 **AProVE가
   이진 산술 종료를 1200s 내 못 찾은 것**이고, **같은 심볼이 모듈러-B에선 전부 term=YES**
   (아래 표 `term(모듈러B)` 열). AProVE 직접이 산술을 블랙박스하지 않아 생기는 차이 —
   즉 **정확성이 아니라 tractability**이며, ≤500을 AProVE 직접으로 돌리기로 정할 때 예고된
   결과다. CRC MAYBE/TIMEOUT도 같은 성질(Val-wide subty CP·산술 CP 폭발; ChC는 YES).

**소결(≤500 fresh)**: post-fix 현재 바이너리·fresh 덤프에서도 **진짜 비합류/비종료 0**.
스테일 MAYBE 2건(`$join_ctk`/`$assignop_as_binop`)은 fix로 YES 정정 확인. 비-YES는 전부
tractability(AProVE 직접의 산술 미증명 = 모듈러-B에선 YES, 또는 대형 슬라이스 CP 예산).
아래 표는 종전 모듈러-B 측정치이며, 이 fresh 재검은 그 결론(도구 근사이지 번역 버그 아님)을
독립적으로 재확인한다.

### 비-YES 37행 해석 (fresh, AProVE 직접)

verification.md §1 표의 각 행이 왜 tractability(도구 근사)이고 실제 비합류/비종료가
아닌지, 같은 해석끼리 묶어 정리한다.

- term: 전제-인코딩 하강(양쪽 MAYBE, 종료함)
  - `$join_text` `$invalidate_headerUnion` `$invalidate_value`

- CRC 산술/비트 CP 폭발, 2400s 부족(비합류 아님)
  - `$write_bits_from_value`

- term: 전제-인코딩 하강 AProVE 미증명 → **모듈러-B=YES**
  - `$un_bnot` `$bin_ge` `$bin_le` `$bin_gt` `$bin_lt` `$int_of_integerValue` `$nat_of_integerValue` `$name_annotationToken`

- term: AProVE가 full-arith 종료 못 찾음 → **모듈러-B=YES**
  - `$bin_minus` `$bin_mul` `$bin_plus` `$un_minus` `$bin_bxor` `$bin_satminus` `$bin_satplus` `$bin_band` `$bin_bor` `$un_op` `$name_annotation_opt`

- CRC 산술/비트 CP 폭발, 2400s 부족(비합류 아님); term: AProVE가 full-arith 종료 못 찾음 → **모듈러-B=YES**
  - `$bin_concat` `$bin_shl` `$bin_shr` `$bitacc_range_replace_op`

- term: 양쪽 미해소(종료함)
  - `$set_priorities_of_tableEntryListIR_prime` `$set_priorities_of_tableEntryListIR`

- CRC Val-wide subty CP 잔여(ChC=YES, false MAYBE); term: AProVE가 full-arith 종료 못 찾음 → **모듈러-B=YES**
  - `$write_value_field_from_bits_prime` `$write_value_fields_from_bits_prime` `$write_value_from_bits_prime` `$write_values_from_bits_prime` `$write_value_from_bits`

- CRC 산술/비트 CP 폭발, 2400s 부족(비합류 아님); term: 최대 슬라이스 예산 초과(모듈러-B도 TIMEOUT)
  - `$bitacc_range_op` `$bitacc_offset_op` `$bitacc_offset_replace_op`

## >500 구간 (bigsweep)

`≤500` 종합 스윕 밖의 대형 슬라이스(501~2000규칙). tmux 세션 `bigsweep`에서 심볼당
1~4시간(프루닝해도 `kept≈rules` — 실제로 시그니처 대부분을 씀). **아래 27/127은
확정치, 나머지는 진행 중.** 예산 `CRC_TMO=TERM_TMO=2592000`(사실상 무제한),
`term(B)`=모듈러 종료.

**중요: 이 구간은 오늘의 binenc(이진 산술) 커밋과 무관하다.** helper 열이 전부
비어 있고(산술 helper 미사용), 심볼은 전부 **list-flatten / id-accessor /
prototype-분류 / 코어션(`as_lvalue`)** 계열이다. 대형인 이유는 이들이
`subty-<T>` 여집합(complement) 가족을 슬라이스로 크게 끌어오기 때문.

**27개 요약**: CRC YES 21 / MAYBE 1 / TIMEOUT 5. term(B) YES 16 / MAYBE 11.
**NO(비종료)·비합류 후보 0.**

## MAYBE / TIMEOUT 실제 분석 (덤프 정적 분석)

### CRC=TIMEOUT (5): `$flatten_{typeArgument,expression,argument,simpleKeysetExpression}List`, `$expressionNonBrace_as_expression`
flatten 3절 구조 —
`$flatten(EMPTY)=nil` · `$flatten(x)=cons(x,nil) if subty-elem(x)=true` (싱글턴) ·
`$flatten(x')=cat($flatten(xs),cons(e,nil)) if match-comma(x')=true /\ x'=comma(xs,e)` (재귀).
싱글턴 절과 재귀 절이 같은 head에 겹치고, CRC는 두 가드
`subty-elem(x)=true`(x가 원소)와 `match-comma(x)=true`(x가 콤마-노드)가
**상호배타**임을 증명해야 discharge. 배타는 참이나(원소는 콤마-노드가 아님), 증명하려면
`subty-<elem>` **여집합 가족(수백 규칙)** 전체에 대한 임계쌍 계산이 필요 →
780+규칙·near-full 시그니처에서 **자원 소진해 verdict 없이 종료(TIMEOUT)**.
→ **비합류 아님** (flatten은 EMPTY/싱글턴/재귀가 의미상 서로소·완전한 total 함수).
이 구간은 `Reflect.expand_subty_guards`가 ≤500에서 풀던 subty-disjointness가
**슬라이스 규모 때문에 CRC가 완주 못 하는** 케이스.

### CRC=MAYBE (1): `$expression_as_lvalue` (764)
identifier/nonTypeName 등 다수 무조건 원소 절 + memberAccess/indexAccess/slice/paren의
재귀 절(base를 `$expression-as-lvalue`로 재귀). MAYBE는 위 subty-가드 배타를 CRC가
완전 discharge 못 한 잔여(같은 가족). well-defined 부분함수 — **false MAYBE.**

### term(B)=MAYBE (11): flatten/optional/split 계열 — 전부 구조 감소, 감소가 전제에 숨음
- **flatten MAYBE** (`namedExpressionList`/`realTypeArgumentList`/`typeArgumentList`/
  `expressionList`/`argumentList`/`simpleKeysetExpressionList`/`parserStateList`):
  재귀 인자 `xs`가 `x'=comma(xs,e)` **전제에서** 나와 `xs ⊂ x'`(콤마-리스트 한 칸 감소)가
  syntactic subterm이 아님 → dependency-pair 분석이 감소를 못 봄. **종료하나 false MAYBE.**
  (7-12 fix가 `match_K ∧ v=K(..)`를 head로 접어 18개 중 13개를 풀었지만, 이 대형
  슬라이스들은 fold 미적용/미해소분. YES로 바뀐 flatten들 — `forUpdateStatementList`
  `annotationList` `parameterList` 등 —과 대비.)
- **`$optional_annotation_of_parameterIR{,_prime}`, `$is_optional_parameterIR`**:
  `$optional-...(p)=$optional-...-prime($annotationList-of-parameterIR(p))` — 재귀는
  `-prime`가 annotation 리스트를 감소시키며 수행. 같은 리스트-감소 MAYBE. 종료.
- **`$split_externConstructorOrMethodPrototypeList`** (14734s): 자기 재귀 아님 —
  `$flatten` + `$filter`×2 + `$itermap` **합성**. 밑 helper들의 전제-인코딩 감소가
  MAYBE로 전파. 종료(종료하는 helper들의 합성).

## 총평 (>500 구간)
- **진짜 비합류/비종료 후보 0.** CRC MAYBE/TIMEOUT = subty-여집합 배타의 CRC
  미완주(규모), term MAYBE = 전제-인코딩 리스트 감소의 AProVE 미증명 — 둘 다 알려진
  도구 근사이며 **번역 버그도, 오늘 binenc 변경과의 연관도 없다**(비-산술 슬라이스).
- 나머지 100개(r=1122~2000, `$name_annotation`·`$sizeof_*`·`$compat_*`·`$name_expression`
  등)는 진행 중. 여기 `$sizeof_*`(sum/max/min_nat)·`$name_expression`(strip_all_whitespace)
  등 오늘 커밋 helper의 carrier가 있어 완주 시 binenc 관련 커버리지가 채워진다.
