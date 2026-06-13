# p4-old × p4_16_samples — STUCK 원인 generic 분석

`spectec/specs/p4-old` 스펙으로 `testdata/interp/p4/p4c/p4_16_samples/*.p4`(1258개)를
Maude 실행(`run --p4`)했을 때 나온 STUCK들을 표본 분석한 문서. 실행 스크립트는
[run_p4old_samples.sh](run_p4old_samples.sh), 원시 결과는
[p4old_samples_results.tsv](p4old_samples_results.tsv).

## 측정 개요 (부분 — 748/1258 처리 후 중단)

| 상태 | 개수 |
|------|------|
| STUCK | 539 |
| OK (`result:` 도달) | 204 |
| OTHER | 4 |
| ERROR | 1 |

- 아키텍처 접미사(`-bmv2`/`-ebpf`/`-ubpf`/`-dpdk`/`-psa`/`-pna`) 파일: **STUCK 257 vs OK 18**.
- 실행당 ~8–11s (대부분 OCaml emission + Maude의 ~49k줄 모듈 파싱 비용; 순수
  rewriting은 ms 단위).

## 핵심 발견 — 모든 STUCK는 똑같이 `Program-ok` unreduced로 보이고, 진짜 원인은 숨는다

표본 8개(action-two-params / alias / bool_to_bit_cast / array_field / cases /
constStruct / actions-almost-duplicate-names1 / arith-bmv2)를 돌려보면 **전부**
`unreduced: Program-ok` 한 줄만 나오고, 정규형은 `Program-ok(<전부 constructor인
프로그램 값>, txt("FRESH"))` 통째다.

이유는 번역 구조에 있다. `Program_ok` / `Decls_ok` / `Decl_ok` 및 거의 모든 타이핑
judgment는 **조건부 등식(ceq)** 으로 emit된다. 예:

```
rule Program_ok:                                  -- spectec/specs/p4-old/5.11-...:960
  |- p4program : TC_1 (declarationIR* ';')
  -- if declaration* = $flatten_p4program(p4program)
  -- if TC_0 = $empty_typingContext
  -- Decls_ok: GLOBAL TC_0 |- declaration* : TC_1 declarationIR*
```

Maude ceq 의미상 **조건 중 하나라도 기대 패턴으로 reduce되지 않으면 등식이 적용되지
않고**, LHS 적용항(`Program-ok(...)`)이 정규형에 그대로 남는다. 이때 인자는 전부
constructor 값이라 **정규형 안에 내부 defined head가 보이지 않는다.** 따라서:

- 보고되는 stuck head 이름(`Program-ok`)은 **원인에 대해 아무 정보도 주지 않는다.**
- 진짜 실패는 N단계 아래에 있고, **각 ceq 단계가 그 아래 단계를 가린다**
  (`Program_ok` → `Decls_ok` → 선언별 `Decl_ok` → …).
- 어떤 STUCK든 원인 규명에는 **단계별 수동 bisection이 필수**다([todo.md](spectec/lib/rewrite/todo.md) 레시피).

## 재현 가능한 bisection 레시피 (constStruct.p4, 18줄, custom package)

```bash
SPEC=$(find spectec/specs/p4-old -name '*.spectec' | sort | tr '\n' ' ')
INC=spectec/testdata/interp/p4/p4c/includes
P=spectec/testdata/interp/p4/p4c/p4_16_samples/constStruct.p4
# 1) 모듈 덤프 + stuck 시작항 캡처
spectec/_build/default/bin/main.exe run --emit --p4 $P -i $INC $SPEC > mod.maude
spectec/_build/default/bin/main.exe run        --p4 $P -i $INC $SPEC | sed -n 's/^FAIL (stuck): //p' > start.txt
# 2) start.txt에서 Program-ok( <value> , txt("FRESH")) 의 <value> 추출(괄호-균형 분리)
# 3) 모듈 + 안쪽 관계 red 들을 한 파일에 이어붙여 maude 직접 실행
```

추적 결과:

1. `red $flatten-p4program(<value>)` → **36 rewrites, `cons(...)` 정상** ✓
2. `red Decls-ok(GLOBAL, $empty-typingContext, $flatten-p4program(<value>), txt("FRESH"))`
   → 503 rewrites인데 **루트에 `Decls-ok(...)`가 그대로 잔존**(stuck).
   - 잔여 항 분석: 선언 리스트 spine 길이 **7 = 원본 전체**, TC의 set들이 전부
     `nil`(= `$empty_typingContext`). 즉 **`Decls_ok`가 step 0에서 막힘** — 단 하나의
     선언도 소비하지 못함.
3. 그런데 그 첫 선언(`structTypeDeclaration`)에 대해
   `red Decl-ok(GLOBAL, $empty-typingContext, <decl0>, txt("FRESH"))`
   → **102 rewrites, `tuple(tuple(struct-typingContext(...), …), …)` 로 깨끗이 reduce** ✓

→ **결론(이 케이스):** 개별 `Decl_ok`는 성공하는데 `Decls_ok`가 그 결과를 소비하지
못해 step 0에서 막힌다. `Decl_ok`은 gensym-threaded라 `tuple(tuple(TC', declIR),
state')` 형태의 **중첩 tuple**을 돌려주는데, `Decls_ok`의 재귀 ceq 조건이 그 shape를
패턴으로 받지 못하는 것으로 의심된다(threaded 출력 shape 불일치). **단, 204개
프로그램이 `Decls_ok`를 통과하므로 보편 버그는 아니다** — 이 선언 특유의 `Decl_ok`
출력 모양에서만 어긋난다는 뜻이고, 정확한 메커니즘은 한 단계 더 bisection이 필요하다.

## STUCK 카테고리 (generic 원인)

1. **아키텍처 의존 (~257 + `#include <v1model.p4>`류 다수).** `arith-bmv2` /
   `bool_to_bit_cast` / `action-two-params` 등은 `V1Switch(...) main` 패키지
   인스턴스, `standard_metadata_t`, 타깃 extern을 쓴다. arch-suffix에서 STUCK 257 :
   OK 18로 압도적. 인스턴스/extern/아키텍처 plumbing 경로의 어느 inner judgment에서
   막힌다(미커버 또는 위 threaded-shape류).
2. **core-language (~282).** 아키텍처 없이 custom package만 쓰는 작은 프로그램
   (constStruct 18줄, array_field — header stack `H[2]`)도 막힌다. 공통적으로
   **선언/인스턴스 체인의 threaded 출력 소비**가 의심 지점(위 constStruct 추적).
3. **parser / 신규 surface 구문 (forloop\*).** `forloop2/3/4/7` = OTHER:
   Maude `Warning: no parse for term` — P4 `for` 루프가 emit된 모듈 문법으로
   **표현 불가**(인코딩 미지원). `forloop5a` = ERROR: 더 앞선 elaboration 단계 실패.

## STUCK ≠ 버그 (해석 주의)

한 프로그램의 STUCK는 다음 중 무엇이든 될 수 있고, **per-case bisection 없이는
구분 불가**다:

- (a) 스펙이 모델링하지 않는 **아키텍처/extern** 의존,
- (b) 샘플 자체가 **음성 테스트**(p4c 샘플에는 타입 에러 케이스가 섞여 있음),
- (c) **번역/threading shape gap** (constStruct에서 의심된 종류).

따라서 이 표는 "p4-old 실행 커버리지의 현재 표면"으로 읽되, 각 STUCK의 책임 소재
(스펙 vs 번역)는 개별 추적 후에만 단정한다.
