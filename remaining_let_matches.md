# p4 rewrite 출력에 남아있는 `let` / `matches` 정리

`spectec-core rewrite spectec/specs/p4/**/*.spectec` 결과 기준.

| 구문 | 개수 | 직전(4차 전) | 그 전(3차 전) | 그 이전(2차 전) |
|---|---:|---:|---:|---:|
| `let` premise | **488** | 487 | 488 | 624 |
| `matches` | **0** | 0 | 11 | 23 |
| `if .. <:` (전체 라인) | **77** | 144 | — | — |
| `... as ...` (cast) | **53** | 15 | — | — |

> 집계 기준: `grep -cE '^\s*-- let '`, `grep -cE 'if .* matches '`, `grep -cE ' <: '`, `grep -cE ' as '`.
> 블록 수 불변(rule 700 / clause 1543 / def 422 / relation 152), rewrite exit 0.

---

## 4차 — subtype `<:` 를 cast 기반으로 (`<:` 144 → 77, cast 15 → 53)

`<:`(subtype) 주입을 **강제 타입 변경 대신 정의에 붙는 `as` cast**로 바꿨다
([simplify.ml](spectec/lib/rewrite/simplify.ml)). 엘라보레이션은 모든 `<:`에 companion
cast-let(`let L = (S as T)`)을 함께 냈지만, 기존 `normalize_deep`이 cast를 전부 벗기고
(`let L = S`) `<:`만 남겨 **136개**가 잔존했다. 이를 뒤집어, cast를 정의에 남기고 `<:`를
지운다.

- **`fold_constraint_into_let`의 SubE 분기 제거**: `let v = E; if v <: T` 를 fresh
  `v_T:T`(강제 타입 note, cast 없음)로 만들던 처리를 삭제. `matches` 폴딩만 남겼다.
- **신규 late 패스 `subtype_to_cast`** (settle·`normalize_deep` 이후라 새로 만든 cast가
  보존됨). **bare top-level `if S <: T`**(S=plain 변수)만 처리하고, `~`/`\/`/`=>`/iteration
  아래의 `<:`(진짜 술어, 예 `if ~typeIR <: setTypeIR`)는 그대로 둔다. 각 주입을:
  - **항등** `S <: S`(`S.note ≡ T`): cast 없이 드롭.
  - **companion** `let L = S`(L은 이미 T 타입 별칭): `let L = (S as T)`로 retarget.
    예: `let forStatementIR = statementIR as forStatementIR`.
  - **자체 binder** `let S = E`: fresh `S_T:T`로 `let S_T = (E as T)`, S→S_T 치환.
  - **binder 없음·head 입력**: `let S_T = (S as T)`를 앞에 도입, premise의 S만 S_T로
    치환(head는 계속 S를 바인딩). 예: `let integerValue = value as integerValue`.
  - cast는 `DownCastE`(주입은 넓은 값을 T로 좁히는 checked downcast — 기존
    `injection_pairs`·`prem_redundant`가 쓰는 규약과 동일).

핵심 게이트: 새 binder가 **live**할 때만 변환(`count_var`로 LHS 1회 초과 사용 확인). 죽으면
prune이 지워 검사가 소실되기 때문. **타입 정합성**: 모든 새 binder의 note == cast 타겟
(companion은 `typ_eq lhs.note T` 요구, 나머지는 fresh `:T`)이라 강제·ill-typed 바인딩 0.

### 남은 `<:` 77개 — 전부 irreducible

| 부류 | 개수 | 사유 |
|---|---:|---|
| iteration `(if X <: Y)*{..}` / `xs* <: ys*` | 20 | SubE가 IterPr 안 — 정의-site 캐스트로 못 옮김 |
| implication `(P => X <: T)` | 3 | 함의의 부분식, 진리값 필요 |
| negation/boolean `~X <: T`, `.. /\ ..` | 17 | 부정·논리곱 안의 술어("T가 아님") — 캐스트로 표현 불가 |
| call subject `$f(..) <: T` | 29 | 호출 결과 직접 검사, 바인딩 핸들 없음(구조 분석 한계) |
| bare `if S <: T` | 6 | S가 분해/옵션 패턴으로 바인딩(plain `let`/head 아님) → 단순 정의-site 부재 |
| other(`(X<:A \/ X<:B)`, clause body) | 2 | 논리합/식 자체가 결과 |

42개(iteration/implication/negation/boolean/other)는 `~`·`=>`·`\/`·`/\`·반복 **연산자 안의
진짜 boolean 술어**라 주입이 아니며 변환 시 의미가 바뀐다. 29개는 호출 결과 직접 검사로
정의-site가 없다. 6개 bare는 주체가 옵션/분해 패턴 바인딩이라 캐스트를 붙일 단순 위치가 없다.
전부 진짜 subtype 검사로 floor다.

**무회귀 확인**: 차분 스코프 검사(`REWRITE_BASELINE`로 패스 토글, USED−BOUND 비교) — baseline
170 = new 170, **새 stranding 0건**. 블록 수 불변, rewrite exit 0. `let`은 거의 불변
(487→488: cast가 강제-타입 binder를 대체).

---

## 3차 축소 — iteration `matches` 소거 (11 → 0)

반복(iteration)/payload subject에 대한 `matches`를 두 메커니즘으로 모두 제거했다
([simplify.ml](spectec/lib/rewrite/simplify.ml), [prem_env.ml](spectec/lib/rewrite/prem_env.ml)).

- **A. companion 재사용** — `pattern_forced`를 canonical 하나가 아니라 subject **동치류의
  임의 멤버**로 확장(`Prem_env.class_members` 추가·노출). `let frame'* = E` 옆에
  `let frame :: _frame* = E`(동일 E 분해)가 있으면 `frame'*` 클래스에 `ConsE`가 있어
  `frame'* matches _::_`가 redundancy 단계에서 사라진다(새 dummy 없음).
- **B. fresh-dummy 재구성** — companion이 없는 경우 `fold_iter_match`가 subject를 재구성
  구조로 binder까지 전역 substitute한다: `let xs = E` + `if xs matches _::_` →
  `let x_h :: x_t = E`; `let (vu, bool*) = $f` + `if bool* matches []` → `let (vu, []) = $f`;
  `let ?(.. value?) = $f` + `if value? matches (_)` → `let ?(.. ?(value_0)) = $f`;
  `[_/1]` → `[x_0]`. 원소 타입은 IterT(또는 named alias를 풀어) 에서 얻어 fresh 변수를
  타이핑한다. 호출/필드 `E`는 let로 보존.

핵심 게이트(B): companion(subject를 포함하지 않는 별도 분해 let)이 없을 것, subject가
let-LHS에 정확히 1회 출현할 것, 치환 후 subject의 원소변수가 free로 남지 않을 것. 또
`fold_iter_match`는 메인 루프 **이후**(settle)에도 돌려, 깨끗한 `let xs = E` binder가
생긴 뒤의 match까지 처리한다.

**무회귀 확인**: 새 패스 토글로 baseline과 차분 스코프 검사 — 새 stranding **0건**
(eq/holds/iteration binder 포함). 블록 수 불변, exit 0. `let`은 재구성이 binder를 유지하므로
거의 불변(488→487).

## 2차 축소 (624 → 488 let, 23 → 11 matches, 161 → 136 `<:`)

제약을 binder로 폴딩하고 단일사용/별칭/반복·구조체 분해를 추가했다
([simplify.ml](spectec/lib/rewrite/simplify.ml)). 가드 종류별 철학:

- **P1 `fold_constraint_into_let`**: `let v = E; if v matches M` → `let recon(M) = E`
  (패턴을 binder에 반영, match 제거, v 사용 → 재구성 구조). `let v = E; if v <: T` →
  fresh `v_T : T` 도입해 `let v_T = E`, `<:` 제거(캐스트 없이 타입 note만), v → v_T.
  호출은 let로 보존. `reconstruct_pattern`을 [prem_env.mli](spectec/lib/rewrite/prem_env.mli)
  에 노출해 재사용.
- **P2 `inline_value_lets`**: `let v = rhs`(v는 plain VarE)를 인라인 — rhs가 변수(순수
  별칭)면 무조건, 아니면 v가 (다른 premise+outs에서) **단일 사용**일 때만. 가드(`IfPr`)에도
  치환되어 호출/필드/concat/산술의 단일사용 let과 변수 별칭이 사라진다.
- **P3 `decompose_pat` 확장**: StrE 필드별 분해 + 반복 leaf(`x*{..}` 전체 치환). 동일-생성자
  반복·구조체 분해 `let`을 인라인. `inline_lets`에 **free-변수 건전성 게이트**(치환 후 원소
  변수가 남지 않을 때만 채택) 추가.

**건전성**: `prem_env.add_pair_decompose`/`env_of_if_exp`와 별개로, `prem_redundant`의
`let X = X` 제거 규칙이 잠재적으로 **binder를 strand**시키던 버그를 수정했다(이전 committed
출력에도 28건 이상 잠복). 이제 `let X = X`는 X의 변수가 다른 곳에서 바인딩되거나 미사용일
때만 제거한다.

**무회귀 확인(스코프 검사)**: 새 패스를 끄는 토글로 baseline과 출력을 비교해, rule/clause별
`(사용 변수) − (바인딩 변수, 동치·holds·반복 binder 포함)`이 **baseline 대비 0건 증가**임을
확인(새 stranding 0). 블록 수 불변, exit 0.

> `as`는 실제 IL 업/다운캐스트가 아니라 `KNOWN AS typeIR` variant 생성자(값 그 자체)일
> 뿐이라 제거 대상이 아님을 확인했고, IL 캐스트는 `normalize_deep`이 이미 0개로 만들었으므로
> 더 이상 추적 대상이 아니다.

---

## 구조 분해 인라인 (787 → 624, −163)

`Simplify`에 **구조 분해 `let` 인라인**을 추가했다 (`inline_lets`,
[simplify.ml](spectec/lib/rewrite/simplify.ml)). RHS가 **불투명 호출이 아닌 구체 데이터
구조**(생성자/구조체/리스트/cons/opt/튜플)인 분해 `let PAT = E`는, PAT를 E에 대해 분해해
얻은 컴포넌트 치환을 하위(결론 + 다른 premise)에 적용하고 `let`을 드롭한다. 예:

```
rule integer: ... |- int < n > : int < n > # []
   -- let int < int > = int < n >      ← 제거
   -- if int <: nat                    ← if n <: nat
   -- if (n > 0)
```

핵심 설계:
- **호출 RHS는 건드리지 않는다** — `let PAT = $f(..)`는 바인딩이 결과에 접근하는 유일한
  핸들이라 그대로 둔다 (받아들인 한계).
- 분해는 **plain 변수 leaf**까지만 (치환이 구조 동치로 use를 덮으므로 stranded 없음).
- **컴포넌트별 검증**: 패턴 변수가 다른 곳에서 바인딩되면(예: `h :: t* = K :: t*`의 공유
  꼬리 `t*`) 그 위치는 항등이라 치환을 만들지 않고 인라인을 막지도 않는다. 새로 바인딩되는
  변수(`h`)만 치환되며, occurs check(`v`가 자기 값에 free하지 않음)를 통과해야 한다.
- 보조로 `Prem_env.add_pair_decompose`가 변수 RHS를 환경에서 resolve한 뒤 분해하도록 확장
  ([prem_env.ml](spectec/lib/rewrite/prem_env.ml), `resolve_struct`).

**이름 위생**: 이 변환은 새 이름을 *만들지 않고* RHS의 기존 in-scope 식만 substitute한다.
검증: 출력의 distinct 변수 토큰 집합이 baseline의 **부분집합**(새 토큰 0개, 12개 감소).
따라서 `type_*` 변수명 충돌이 원천적으로 발생하지 않는다.

**무회귀 확인**: rule/clause/def/relation 블록 수 불변 (700 / 1543 / 422 / 152). premise
라인만 2641 → 2475로 감소. p4 코퍼스 rewrite exit 0.

---

## `let` — 624개 (RHS 종류별)

| RHS | 개수 | 상태 |
|---|---:|---|
| 직접 호출 `$f(..)` | 454 | 불투명 호출 결과 — 인라인 불가 (한계) |
| 호출 포함 | 14 | 부분식에 호출 — 인라인 불가 |
| 필드 접근 `x.y` | 32 | `TBLC.mode` 등 — 구조체 reconstruction 불가(루트가 호출/출력) |
| concat `++` | 12 | 리스트 조립 — 분해 대응 없음 |
| 기타 | 112 | 아래 |

"기타" 112는 **데이터 구조 분해가 아닌** 부류라 이번 범위 밖이다:
- **서브타입 주입 별칭** — `let narrow = wide` + `if wide <: narrow` (wide는 relation
  출력). `as` 미태깅 형태. 출력 타입 정합성에 민감해 보존. 예:
  `let forStatementIR = statementIR` + `if statementIR <: forStatementIR`.
- **호출 결과 변수 분해** — `let typedef nameIR_alias typeIR_alias = typeDefIR` 처럼
  RHS 변수가 호출 결과(`$find_typeDef_t`)인 경우. 한계.
- **리스트 cons 바인딩** — `let x* = h :: t*` (전체 리스트를 cons로). 분해 대응 없음.

> 구조 분해(생성자/cons/튜플/구조체)로 *구체 구조에서 직접 분해*되던 `let`은 인라인으로
> 거의 소진됐다. 특히 필드 접근 분해는 (이전 41 → ) 현재 호출/출력 루트만 남아 reconstruct
> 불가한 32개뿐이다.

---

## `matches` — 23개 (전부 irreducible)

23개 모두 **field 접근 또는 호출 결과 값에 대한 진짜 케이스/모양 가드**임을 subject 바인더를
추적해 재확인했다. subject가 head 입력인 경우가 하나도 없어 head 패턴으로 hoist할 여지가
없다 (hoist는 무의미하거나 unsound).

| 가드 | 개수 | subject 바인더 | 분류 |
|---|---:|---|---|
| `matchMode matches \`LPM %\`` | 8 | `let matchMode = TBLC.mode` | 필드(variant 판별) |
| `frame'* matches _ :: _` | 2 | `... = $enter_i(..).local.frames` | 호출+필드 |
| `ctk matches \`DYN\`` | 2 | `?(.. ctk ..) = $find_var_t(..)` | 호출 결과 필드 |
| `callTargetMatch matches \`MATCH % %\`` | 2 | `= $match_overloaded_named(..)` | 호출 결과 |
| `value? matches (_)` / `()` | 2 | `$find_var_t(..)` 분해 필드 | 호출 결과 필드 |
| `typeParameterIR* matches []` | 1 | `= $typeParameterListIR_of_typeDefIR(..)` | 호출 결과 |
| `tableKeyIR'* matches [ _/1 ]` | 1 | `= $filter_<tableKeyIR>(..)` | 호출 결과 |
| `sequenceTypeIR matches \`SEQ <%>\`` | 1 | `= $unroll_typeIR(..)` | 호출 결과 |
| `prefixedNameIR matches \`\` %\`` | 1 | `= $prefixedNonTypeName(..)` | 호출 결과 |
| `parameterIR'* matches []` | 1 | `= $align_parameterListIR(..)` | 호출 결과 |
| `expression''* matches _ :: _` | 1 | `= $rev_<..>($flatten_..)` | 호출 결과 |
| `bool* matches []` | 1 | `= $write_value_from_bits'(..)` | 호출 결과 |

전부 런타임에 달라지는 값(필드의 variant, 호출 결과의 모양)을 판별하는 분기라 제거하면
의미가 바뀐다. **함수 호출 결과를 match하는 경우는 구조적 분석의 한계**라는 전제 그대로,
이 23개는 floor다.

---

## 요약

- **`let` 787 → 624 (−163, −21%)**: 구체 데이터 구조에서 직접 분해하던 `let`을 substitute로
  인라인. 남은 624 중 454는 불투명 호출 결과(한계), 나머지는 호출/concat/서브타입 주입 등
  데이터 분해가 아닌 부류.
- **`matches` 23 (불변)**: 전부 필드·호출 결과 값에 대한 진짜 가드. subject가 head 입력이
  아니라 hoist 불가. irreducible.
