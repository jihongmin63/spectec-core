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

추적 결과 (`Decls_ok/cons`는 재귀 `Decls_ok`를 **조건 안에서** 호출하므로, 리스트의
어디서 막히든 최외곽 `Decls-ok`가 전체 리스트+빈 TC로 unreduced 상태로 보인다 —
"step 0에서 막힌 것처럼" 보이지만 실제 원인은 더 깊은 선언이다):

1. `red $flatten-p4program(<value>)` → 36 rewrites, `cons(...)` 정상 (7개 선언) ✓
2. **prefix를 1→7로 늘려 `Decls-ok`을 reduce** → `[d0]`/`[d0,d1]` OK, `[d0,d1,d2]`부터
   STUCK. 범인 = **d2**.
   - d0=`struct S { bit<8> x; }`, d1=`const S s = { x = 1024 }`,
     **d2=`const bit<16> z = (bit<16>)s.x;`**.
3. `Decl-ok(GLOBAL, TC_{d0,d1}, d2, st)` → 86 rewrites 후 stuck. `Decl_ok/constantDeclaration`
   조건(`Type_ok`→`Expr_ok`→LCTK 체크→`$coerce_unary`→`Eval_static`) 중 하나가 막힘.
4. `Expr-ok(…, (bit<16>)s.x, …)` → stuck. 더 안쪽 `Expr-ok(…, s.x, …)`(struct const의
   멤버 접근) → **stuck**. 그런데 그 base `Expr-ok(…, s, …)`(11 rw, 타입 `namedTypeIR(struct S)`,
   ctk `LCTK`) ✓, `$canon(namedTypeIR…)`(7 rw → `structTypeIR(S, cons(fieldTypeIR(bit<8>,"x"),nil))`) ✓
   — 조각은 다 되는데 멤버접근 규칙만 안 fire.

### 확정된 근본 원인 — `$itermap`(정의 함수)이 `:=` 매칭 패턴에 박혀 있음 (lib/rewrite 번역 버그)

emit된 struct 멤버접근 `Expr-ok` 등식(모듈의 `...memberAccessExpression…struct` ceq)의
조건:

```
tuple(variant-structTypeIR-STRUCT-lbrace-rbrace-2(
        -tid:Text,
        $itermap-typeIR-f-nameIR-f-semi-list-2(nameIR-f:Val, typeIR-f:Val)),  ← 함수가 패턴 안!
      St2:Val)
  := $canon(typeIR-base:TypeIR, St1:Val)
```

`$itermap-typeIR-f-nameIR-f-semi-list-2 : Val Val -> Val` 는 두 스트림(이름들·타입들)에서
필드 리스트를 **재조립하는 정의 함수**(`eq …(cons(n,ns),cons(t,ts)) = cons(fieldTypeIR(t,n), …)`).
Maude는 `pattern := term` 의 **좌변을 constructor 패턴으로만** 매칭하는데, 여기엔 정의 함수
호출이 들어 있다. `$canon`이 내놓는 실제 `cons(fieldTypeIR-semi-2(bit<8>,"x"), nil)` 은 이
`$itermap-…(nameIR-f, typeIR-f)` 함수항과 **구문적으로 매칭 불가** → 조건 영구 실패 →
**struct / header / headerunion 의 필드 접근(`.field`) 타이핑이 항상 stuck**.

**대조 — 바로 옆 `tablestruct` 규칙은 올바르다:** 필드 리스트를 fresh `iterbind-0:List` 변수로
받고 `$unzip-…` 조건으로 스트림을 복원한다 (binder-position iteration의 정상 컴파일).
즉 struct/header/headerunion 경로만 binder-position 반복(`(typeIR_f nameIR_f ';')*`)을
`$unzip` 대신 **`$itermap` 재-zip을 패턴 위치에** 내보냈다.

- **버그 위치:** [to_ctrs.ml](spectec/lib/rewrite/to_ctrs.ml) — 등식 전제
  `if STRUCT _ { (typeIR_f nameIR_f ';')* } = $canon(typeIR_base)` 처럼 **함수 결과를
  destructure하는 `:=` 조건의 좌변**에 iterated 구조 패턴이 올 때, binder-position 컴파일
  (fresh 변수 + `$unzip` 조건)이 적용돼야 하는데 value-position 컴파일(`$itermap` 재-zip)이
  적용된 것. `tablestruct`가 정상 동작하므로 정답 코드 경로는 이미 존재 — 분기 조건만
  어긋난다.
- **영향:** struct/header/header-union 멤버 필드 접근은 P4에서 극히 흔하므로(헤더 필드,
  메타데이터 접근 등) **539 STUCK의 큰 비중**을 이 하나가 설명할 개연성이 높다. 확정엔
  수정 후 재측정이 필요.
- **참고:** [todo.md](spectec/lib/rewrite/todo.md)가 적어둔 "binder-position `$unzip`은
  non-left-linear" 항목과 같은 계열(iteration의 binder vs value 위치 컴파일)의 문제다.

## STUCK 카테고리 (generic 원인)

1. **아키텍처 의존 (~257 + `#include <v1model.p4>`류 다수).** `arith-bmv2` /
   `bool_to_bit_cast` / `action-two-params` 등은 `V1Switch(...) main` 패키지
   인스턴스, `standard_metadata_t`, 타깃 extern을 쓴다. arch-suffix에서 STUCK 257 :
   OK 18로 압도적. 이들은 거의 항상 헤더/메타데이터 **필드 접근**을 하므로 위
   `$itermap`-in-pattern 버그에도 직접 노출된다(별도로 아키텍처 미커버 가능성도 있음).
2. **core-language (~282).** 아키텍처 없이 custom package만 쓰는 작은 프로그램
   (constStruct 18줄, array_field — header stack `H[2]`)도 막힌다. constStruct는 위에서
   **`$itermap`-in-`:=`-pattern 번역 버그(struct/header/headerunion 필드 접근)** 로
   확정 추적됐다. 다른 core-language STUCK도 같은 버그이거나, 같은 방법(prefix/관계별
   stepwise reduce)으로 개별 추적해야 한다.
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
