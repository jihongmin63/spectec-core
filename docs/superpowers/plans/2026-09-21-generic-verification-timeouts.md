# 반복 바인더 수정과 범용 검증 Timeout 단축 전략

2026-09-21 KST. 오류 수정과 계획의 저위험 공통 최적화는 반영했다. 증명 입력을
바꾸는 고비용 항목은 아래 단계별 게이트와 실측 결과에 따라 계속 분리한다.

## 대상과 판정 범위

- 원본: `/root/spectec-core`, `new-rewrite`, HEAD `80a9f450`.
- 참조: `.worktrees/extern-meta-confluence`, `feat/extern-meta-confluence`, 같은 HEAD 위의 미커밋 변경.
- 실제 확보한 명세: `_test/p4-spectec-meta2-upstream`, P4-SpecTec `meta2@da36ac3c434cd291940293a63da64544307730a3`, `spec-meta/common + al/sl`.
- 사용자 요청의 `squaring`이라는 파일·브랜치는 지정 작업 트리와 로컬 upstream에서 확인되지 않았다. 사용자에게 경로 확인을 요청했다. 아래를 별도의 `squaring` 명세에 대한 검증 결과로 읽으면 안 된다.
- `extern syntax`, `extern relation`, `rulegroup` 및 이를 지원하는 AST 변경은 이식하지 않는다. 외부 관계 구현의 종료성·합류성도 이번 분석 범위가 아니다.

## 반영한 논리 오류

[`To_ctrs.iterpr_defs`](../../../spectec/lib/rewrite/translate/to_ctrs.ml)는 반복 입력과 새로 바인딩되는 출력을 모두 `__hd` 이름으로 바꾸었다. 그러나 구조화된 내부 반복은 자체 바인딩 범위를 유지한다. 결과적으로 조건은 `y`를 바인딩하고 collector는 존재하지 않는 `y__hd`를 반환했다.

최소 재현은 다음 패턴이다. 실제 메타 명세의 대입 규칙에도 `val_sub* = $finds_vari(C_sub*, vari)`를 다시 반복하는 패턴이 있다.

```text
dec $finds(val*, val) : val*?
dec $copy(val*, val*) : val*?*
def $copy(x*, z*) = y*?*
  -- if (y*? = $finds(x*, z))*
```

수정은 참조 작업 트리와 같은 원리로 `iterpr_defs`에 한정했다.

1. 반복을 구동하는 입력 `bound_ids`만 단계별 이름으로 바꾼다.
2. collector는 내부 조건이 실제 바인딩한 출력 이름을 그대로 저장한다.
3. 다중 출력 저장용 tuple과 projection이 소비할 tuple 패턴을 구분한다. projection은 자신의 `__hd` 변수를 계속 바인딩한다.

원본에서 회귀 테스트가 `unbound rhs variable`로 실패하는 것을 확인한 뒤 수정했고, 같은 테스트가 통과했다. 다중 출력 export도 통과했다. [`termination-nested-iter.t`](../../../spectec/test/cli/termination-nested-iter.t)가 이를 고정한다. 이 테스트의 판정 범위는 TRS export와 바인딩 일관성이며, 전체 명세의 실행 결과 동등성이나 종료성 증명은 아니다.

참조 트리에서는 9월 17일 AL 26개·SL 27개, 총 53개 이전 ERROR 슬라이스의 export 성공이 기록되어 있다. 이 수치를 이번 원본 트리의 전수 재검증 결과로 옮겨 쓰지 않는다. [기존 재검증 기록](../../../_test/meta2-confluence-20260914/termination-recheck-20260917/SUMMARY.md).

## 근거: 증명 난이도와 반복 실행 비용을 분리해야 한다

### 기존 측정에서 확인한 현상

아래 숫자는 **9월 14–17일 당시 입력**의 값이다. 이번 수정 이후의 성능 수치가 아니다.

| 대상 | 확인된 결과·구조 | 해석의 한계 |
| --- | --- | --- |
| AL / SL confluence | 정의 80 / 81개, 각각 고유 MFE 모듈 59개. 각각 대표 50 YES, 9 TIMEOUT | 동일 입력 대표 재사용은 이미 수행했다. 이를 새 최적화 성과로 세지 않는다. |
| 타입 변환 네 대표 | `$upcast`, `$downcast`, `$subtyp`, `$subtyps`; 본체 약 95–100규칙, 대부분 조건부. 정규화 명령당 약 1,202초, 승격 0 | 반사로 규칙이 확대된 것은 확인했다. CP 생성·조건 평가·join 중 어느 단계가 지배적인지는 미계측이다. |
| 대입 계열 | `holds_Assign_exp` case split 297절, 관련 `holds_*` 방정식 336개. 정규화 명령당 약 601초, 승격 0 | 타입 변환을 포함하지 않는 `Assign_exp`만으로도 TIMEOUT. 별도의 비용원이다. |
| 타입 변환 termination | 전체 TRS와 단독 열린 QDP 모두 TIMEOUT. 열린 코어는 `$upcast`/`$downcast` 반복·반사 또는 `$subtyp` | 다른 SCC가 예산을 선점했다는 설명만으로는 해결되지 않는다. |
| 오류 수정 후 AL `Assign_exp` | 전체 TIMEOUT, 열린 자식 `d_repeat__val` TIMEOUT, 정수 모델 TIMEOUT, 길이 모델 export ERROR | `unbound RHS` 결함과 증명 난이도는 별개다. 마지막 ERROR는 모델 적용 불가다. |

원자료: [기존 전체 요약](../../../_test/meta2-confluence-20260914/README.md), [기존 원인 조사](../../../_test/meta2-confluence-20260914/timeouts.md), [AL 정규화 TSV](../../../_test/meta2-confluence-20260914/confluence/al-normalized-300-representatives.tsv), [수정 후 대표 TSV](../../../_test/meta2-confluence-20260914/termination-recheck-20260917/representatives-300.tsv).

### 이번에 코드와 로그로 추가 확인한 실행 비용

[`Termination.children`](../../../spectec/lib/rewrite/termination.ml)는 이전 whole 실행의 캡처를 재사용하지 않고 `Aprove.check ~capture`로 전체 문제를 다시 실행한다. 따라서 `children:601.4s`에는 **전체 문제 재실행 + 자식 검사**가 포함된다. 자식 자체가 600초 검사된 것이 아니다.

`probe`는 budget 사다리를 오르지만 반환하는 `secs`는 마지막 시도의 시간뿐이다. 기본 cap 300이면 시도 예산은 `5, 20, 80, 300`이다. 모두 소진하면 한 경로의 예산 합은 405초인데 마지막 300초만 경로 시간에 남는다. `report_of.secs`도 전체 시간이 아니라 선택된 경로의 시간이다.

9월 17일 AL `Assign_exp`의 [시작·종료 로그](../../../_test/meta2-confluence-20260914/termination-recheck-20260917/representatives-300.log)는 다음 차이를 실제로 보여 준다.

- 시작 `22:38:10`, 종료 `23:01:55` KST: **1,425초**.
- 기록된 경로 합: `300.9 + 601.4 + 301.2 = 1,203.5초`.
- 차이 **221.5초**. plain/ITRS 양쪽의 하위 rung 예산 합 210초가 대부분을 설명한다. 나머지 11.5초의 단계별 귀속은 기존 로그만으로 확정하지 않는다.

CRC의 `--crc-normalize --timeout 300`도 총 300초 제한이 아니다. [`Mfe.check_normalize_upgrade`](../../../spectec/lib/rewrite/mfe.ml)는 base와 각 정규화 표면을 각각 검사한다. 따라서 1,202초·601초는 여러 검사의 합이며 단일 CP 검사 시간으로 해석할 수 없다. 현재는 원본과 같은 정규화 결과만 건너뛰므로, 서로 다른 전략이 같은 최종 모듈을 만드는 경우의 중복도 추가 확인할 대상이다.

### 이번 최소 예제 실험: 숫자 반복

메타 명세 common `0-stdlib.watsup`의 `$repeat_<X>`를 단일 `val` 타입으로 인스턴스화한 [최소 명세](../../../_test/iteration-fix-20260921/repeat.spectec)를 검사했다. 각 행에서 plain과 ITRS 사이에 표면만 바꾸었고, 모든 실행은 직렬이었다. child split과 자동 integer retry를 끄고, 20초 실험은 `--budget-from 20`으로 한 번만 실행했다.

| AProVE 예산 | Structural TRS | ITRS 모델 |
| --- | --- | --- |
| 5초 | TIMEOUT, wall 6.267초 | TIMEOUT, wall 5.455초 |
| 20초 | TIMEOUT, wall 25.563초 | **YES, wall 6.471초** |

export는 TRS 81규칙·chain 2개에서 ITRS 21규칙·chain 1개로 바뀌었다. 같은 숫자 감소를 정수 제약으로 드러내는 접근에 최소 예제의 실측 근거가 생겼다. 그러나 이는 **독립된 반복 함수의 정수 모델**에 대한 YES이며, 기존 `Assign_exp` 전체 ITRS의 TIMEOUT을 해소했다는 결과가 아니다. TIMEOUT 행의 wall이 내부 예산을 넘는 이유도 전체 subprocess 수명과 AProVE 내부 budget을 구분해야 하는 사례다.

정확한 명령·KST·wall 기록: [5초 실험](../../../_test/iteration-fix-20260921/repeat-probes.json), [20초 실험](../../../_test/iteration-fix-20260921/repeat-probes-20.json). 현재 트리에서의 최소 예제 결과이며 기존 전수 측정 컬럼에 합치지 않았다.

## 단계별 전략

각 단계는 명세 이름을 분기 조건으로 쓰지 않는다. 구문 구조·의존성·정확한 증명 입력에 따라 적용한다. **검사 종료까지의 시간 감소**와 **동일 예산에서 YES 증가**를 별도 성과로 평가한다.

### 1. 기준 입력 고정과 단계별 계측

대상 파일: `pipeline.ml`, `subproc.ml`, `mfe.ml`, `termination.ml`, `lib/cli/rewrite/`의 결과 기록 경로.

1. 변경 전후 코드·명세 hash, 도구 버전, 옵션, 정확한 emitted 입력을 manifest에 남긴다. 이번 수정 뒤에는 기존 입력 hash와 판정을 승계하지 않고 새 기준을 만든다.
2. `translate`, `emit`, `load`, `check`, `capture`, `child`, 각 budget rung, 전체 wall 시간을 별도 기록한다. 기존 `secs` 의미를 바꿀 경우 컬럼을 명시적으로 구분한다.
3. 반사 전후의 규칙 수, 조건 수, helper 수, 최대 helper 인자 수, defined-head SCC를 기록한다. CRC의 CP 수·단계별 시간은 실제 도구가 제공하는 범위부터 수집하며, 미노출이면 `unknown`으로 남긴다.
4. `$upcast`, `$downcast`, `$subtyp`, `$subtyps`, `Assign_exp`, 단순 `$repeat`, `Entry`를 우선 패널로 삼고 AL/SL 및 impty/P4의 기존 YES 대조군을 함께 둔다.

통과 조건: 전체 wall과 단계 합의 차이를 설명할 수 있고, 반복 시도와 실제 증명 시간을 구분할 수 있어야 한다. 계측만 바꾼 실행은 proof 입력 hash와 판정을 보존해야 한다.

### 2. 같은 증명 문제의 재실행 제거

대상 파일: `termination.ml`, `aprove.ml`, `qdp.ml`, `mfe.ml`.

1. whole 경로의 최종 시도에서 필요할 때 obligation을 스트리밍 수집하고, 그 실행에서 얻은 proof tree·QDP를 `children`으로 넘기는 실험을 한다. 별도 whole capture 재실행을 제거한다.
2. 성공한 쉬운 실행에서 거대한 echo를 끝까지 기다리지 않도록 한다. 캡처 비용 자체를 측정하고, 누락된 open child는 반드시 미완료로 처리한다. 일부 child만 모아 전체 YES를 내면 안 된다.
3. 정규화 전략 사이에서 **최종 MFE 모듈이 정확히 같은 경우** 중복 검사를 건너뛴다. 설정·signature·도구 버전까지 키에 포함한다.
4. termination 캐시는 MFE 모듈 hash 대신 실제 `.trs`/`.itrs`/`.qdp`와 해당 증명 문맥을 사용한다. QDP에는 usable rules, minimal 표기, Cε, dependency pair를 포함한다. 이름이 같은 SCC만으로 재사용하지 않는다.

실험 변수: capture 재사용 하나만 먼저 변경한다. 기존 `Assign_exp`에서 약 300초의 재실행 항이 사라지는지가 검증 목표이며, 아직 달성한 단축값이 아니다. 원래 YES가 유지되고 캡처 누락이 YES로 승격되지 않아야 한다.

### 3. CRC의 생성자 분기를 공유하는 분석 표면

대상 파일: `translate/reflect.ml`, `crc_surface.ml`, `maude/maude_sorts.ml`.

메타 명세는 type/value 생성자별 dispatch, 마지막 `otherwise`, output을 내는 재귀 judgment를 반복 사용한다. 현재 complement 열거와 재귀 `holds_*`의 case split이 이 구조를 넓게 복제한다.

1. `$upcast`를 첫 대상으로, 생성자 판별의 공통 접두부를 보조 함수로 공유하는 구조적 decision tree를 실험한다. 타입·값 생성자의 전체 조합을 미리 펼치기보다 실제로 필요한 인자·위치만 단계별로 판별한다.
2. 다음으로 `holds_Assign_exp`에 같은 구조를 적용한다. 값의 계산과 성공 여부 판단의 공통 하위 검사를 공유하되, 재귀 성공 판정과 실패 보완의 적용 범위를 유지한다.
3. 각 변환은 원래 규칙과의 대응, 바인딩 순서, 변수 도메인, `otherwise`의 형제 규칙 실패 조건을 확인한다. 작은 유한 입력의 결과 비교에 더해 overlap/조건 경계 사례를 검사한다.

통과 조건: 동일 signature·동일 예산에서 CP 작업량/시간이 감소하거나 YES가 늘어야 한다. 단순 규칙 수 감소만으로 성공이라 하지 않는다. 조건 삭제, MFE가 무시하는 `owise`의 무검증 사용, 도메인을 임의로 좁혀 overlap을 지우는 방법은 채택하지 않는다. 작은 슬라이스가 각각 YES라는 사실만으로 cross-overlap 확인 없이 전체 합류성을 합성하지 않는다.

### 4. Termination에 필요한 변환만 적용하고 감소량을 노출

대상 파일: `pipeline.ml`, `termination.ml`, `unravel.ml`, `translate/to_ctrs.ml`, `itrs.ml`.

현재 termination은 CRC용 `Analysis` 표면을 받는다. CRC용 complement·`holds_*` 확대가 termination에는 더 큰 재귀 SCC를 줄 수 있다. 첫 실험은 **같은 Structural scalar의 반사 전 CTRS**를 별도 종료성 표면으로 사용하는 것이다. Native 실행 모델과 동시에 바꾸지 않는다.

1. `To_ctrs + Builtin + Gensym + orient_conds` 이후 표면을 후보로 삼는다. 조건의 평가·바인딩은 보존하고, 남은 `owise`는 속성 제거로 규칙 적용을 넓히는 안전한 방향만 검토한다. 반사에서 공급하던 predicate 정의가 필요한지 빠짐없이 확인한다.
2. 원래 실행 단계의 시뮬레이션과 조건의 평가 종료성까지 포함하는 변환 정당성을 세운다. `isStuckHead`를 정의 없는 조건으로 남겨 재귀 규칙을 지우는 과거 결함을 재현하지 않아야 한다.
3. list/option map·collector·projection의 생산자/소비자 관계가 DP에 남도록 구조를 공유한다. 유효한 fusion, constructor binder folding, 이미 도입된 flat-variable unraveling을 활용한다. projection 결과만으로 재귀해서 구조 감소가 가려지는 사례를 최소 명세로 검사한다.
4. 숫자 반복은 `$repeat(v,n) -> ... $repeat(v,n-1)`과 `n>0`의 결합을 별도 검사한다. 위 최소 실험을 출발점으로, 실제 열린 QDP가 운반하는 규칙·제약을 보존한 문제에서도 닫히는지 확인한다. 전체 슬라이스에서 정수 연산이 보인다는 이유만으로 정수 모델이 열린 QDP를 해결한다고 가정하지 않는다.
5. `cons(head,tail)`을 길이 하나로 치환해서 exporter 오류를 없애지 않는다. head/tail·구조적 값은 유지하고 필요한 크기 관계를 부가하는 모델은 별도 연구 항목으로 둔다.

통과 조건: rule coverage·bound-variable invariant·실행 대응을 보존한 표면에서 같은 예산으로 열린 QDP가 줄거나 닫혀야 한다. ITRS/길이 모델의 YES는 [`itrs.mli`](../../../spectec/lib/rewrite/itrs.mli)의 계약대로 **모델의 판정**이다. scalar/list 구현과의 대응 증명 없이 원본의 무조건적 종료 증명으로 보고하지 않는다.

### 5. 종료성을 기대할 수 있는 입력 영역 확인

타입 alias를 환경에서 찾아 다시 cast/subtype하는 재귀는 단순히 `typ`의 크기가 줄어드는 재귀가 아니다. 예를 들어 환경이 `T ↦ DEF eps -> ALIAS (VAR T eps)`를 허용하면, alias 조회와 빈 치환 뒤에 `$upcast(C, VAR T eps, v)`가 같은 호출로 되돌아온다. `$downcast`와 `$subtyp`에도 같은 형태의 alias 절이 있다.

이는 원문 규칙에 대한 **수작업 유도 후보**이며, 이번에 실행기에서 검증한 루프 증인이나 관측된 TIMEOUT의 확정 원인은 아니다. 근거는 AL `5.1-eval-typ.watsup`의 VAR alias 절, `3-context.watsup`의 `$find_typ`, common `5.0-eval-typ.watsup`의 빈 치환 항등식이다.

먼저 순환 alias가 검사 시작점의 허용 입력인지, 별도 well-formedness가 배제하는지 확인한다. 허용된다면 모든 환경에 대한 strong termination을 최적화만으로 얻을 수 없다. 배제된다면 alias 순위/비순환성 불변식을 정당한 입력 전제로 명시해 증명에 연결한다. `Entry`의 사용자 정의 재귀 호출도 같은 검토가 필요하다. 임의 fuel 추가나 의미 변경은 이번 단축 작업의 해법으로 넣지 않는다.

### 6. 근거에 따른 재시도 순서와 전수 비교

1. 열린 QDP의 실제 의존성에 따라 다음 경로를 고른다. 정수 감소가 지배적이면 정수 모델을 먼저 실험하고, 구조 감소면 구조 표면을 먼저 실험한다. 같은 입력에서 경로 순서 하나만 바꿔 효과를 측정한다.
2. 길이 모델 지원 여부는 무거운 실행 전에 검사한다. unsupported이면 별도 사유를 남기고 그 모델 실행을 생략한다. 이 검사는 지금도 export에서 실패하므로, 이것만으로 앞선 1,200초가 줄지는 않는다.
3. 반복 검증에서 전체 wall cap과 경로별 cap을 구분한다. cap 감소는 빨리 미증명으로 끝내는 정책이며 증명 가속과 구분한다. 이전 TIMEOUT을 캐시에 저장해 향후 큰 예산 시도를 영구 차단하지 않는다.
4. 각 후보는 새 디렉터리에서 같은 패널·예산으로 직렬 측정한다. 최종 채택 전 AL/SL 전체를 다시 측정하고 impty/P4 대조군을 검사한다. resumable TSV는 해당 실험 전체를 새로 시작할 때만 초기화하고 다른 실험의 행을 섞지 않는다.

평가 항목은 `전체 wall`, `YES 수`, `미증명 수`, `ERROR 사유`, `최대 메모리`, `모델/원본 판정 구분`이다. 기대 효과는 2단계의 중복 실행 감소부터 확인하고, 3·4단계에서 증명 난이도 자체를 낮추는 순서로 검증한다.

## 이번 검증 기록

- 원본 회귀 테스트: 수정 전 실패, 수정 후 성공. production 수정은 `iterpr_defs`만이다.
- `bin/main.exe` 빌드, rewrite 단위 테스트, CLI 테스트: 통과.
- 변경 OCaml 파일 `ocamlformat --check`: 통과.
- impty analysis CTRS golden: 일치. `git diff --check`: 통과.
- 전체 `dune runtest`: 실패. `test/elab/p4.expected`, `test/struct/p4.expected`, `test/roundtrip/el/p4.expected`, `test/roundtrip/il/p4.expected` 네 golden 차이가 있다. 마지막은 clauses 기대값 1543, 실제 1547이다. 이 네 테스트는 `spectec.rewrite`에 의존하지 않는다(`test_lib`와 각 `dune` 의존성 확인). 관련 소스·기대값은 수정하지 않았다.
- 읽기 전용 별도 코드 리뷰: 중첩 binder, projection/fusion, captured 변수, 직접 relation-map 경로에서 차단 사항 없음.
- timeout 변경 전체의 fresh-context 최종 리뷰: Critical/Important 없음, `Ready to merge: Yes`.
- 로그: [`_test/iteration-fix-20260921`](../../../_test/iteration-fix-20260921/). 기존 sweep 결과는 변경하지 않았다. 커밋하지 않았다.

## 승인 후 구현 작업 (2026-09-21)

### Task 1: 종료성 budget 사다리의 실제 비용 계측

**파일:** `spectec/lib/rewrite/termination.ml`, `.mli`,
`spectec/test/rewrite/test_rewrite.ml`, 필요하면 CLI 출력 테스트.

- `probe`가 최종 rung 시간만 버리지 않고 시도한 budget·verdict·wall을 보존한다.
- 기존 TSV의 `secs`/`paths` 의미를 조용히 바꾸지 않는다. 전체 탐색 비용은 별도
  필드와 표기로 추가한다.
- 순수 함수 테스트로 rung 누적과 문자열 표기를 RED→GREEN으로 고정한다.

### Task 2: child split의 whole-system 재실행 제거

**파일:** `spectec/lib/rewrite/aprove.ml`, `.mli`, `termination.ml`, `.mli`,
`qdp.ml`, `.mli`, `spectec/test/rewrite/test_rewrite.ml`.

- split이 활성화된 plain 최종 rung은 proof tree와 필요한 QDP obligation을 같은
  실행에서 수집한다.
- decisive verdict면 obligation echo를 기다리지 않고 종료하고, inconclusive이면 모든
  open leaf의 obligation을 받은 뒤 종료한다.
- open leaf 하나라도 누락되면 지금처럼 split을 거부한다.
- 파서/완료 조건의 합성 출력 테스트를 먼저 실패시킨다.

### Task 3: CRC 정규화 입력 중복 제거

**파일:** `spectec/lib/rewrite/mfe.ml`, `.mli`,
`spectec/test/rewrite/test_rewrite.ml`.

- 서로 다른 ladder 전략이 같은 normalized system을 만들면 한 번만 검사한다.
- 중복 제거는 `Rewrite_system.t` 구조 동등성과 고정된 `sig_rules`/pruned signature
  문맥 안에서만 수행한다.
- checker 호출 없이 순수하게 고유 후보 순서를 검사하는 API 테스트를 먼저 작성한다.

### Task 4: termination 전용 structural 표면 실험

**파일:** `spectec/lib/rewrite/pipeline.ml`, `.mli`, `rewrite.ml`, `.mli`,
`spectec/lib/cli/rewrite/termination_command.ml`, 종료성 관련 테스트·문서.

- CRC 반사 전 Structural CTRS를 명시적인 `Termination_analysis` target으로 만든다.
- 기존 analysis 표면과 규칙 coverage, owise 포함 방향, condition binder를 작은
  명세와 대표 meta2 slice에서 비교한다.
- proof soundness 전제가 충족되고 같은 예산에서 개선된 경우에만 CLI 기본값으로
  채택한다. 아니면 실험 결과만 기록하고 기본값은 유지한다.

### Task 5: 직렬 실측·회귀 검증·문서 갱신

**파일:** `_test/`의 새 KST 산출물, 이 문서, `spectec/lib/rewrite/todo.md`.

- `$repeat`, `Assign_exp`, `$upcast`를 변경 전과 같은 명령·예산으로 직렬 비교한다.
- 새 결과 TSV는 기존 결과에 병합하지 않는다.
- 빌드, rewrite/CLI 테스트, impty golden, `git diff --check`를 실행한다.
- 전체 suite의 기존 P4 golden 4건은 별도로 보고하며 이번 변경과 섞어 고치지 않는다.

## 승인 후 구현 결과

### 계측과 child split

`Termination`은 각 budget rung의 budget·verdict·proof-tree 도달 시간과 전체 subprocess
시간을 보존한다. 기존 `secs`는 마지막/응답 rung이라는 의미를 유지하고, 실제 누적 시간은
`total_secs`와 `paths`의 `total=…; rungs=…` 표기로 추가했다. 단일 rung이라도 obligation
캡처 tail이 0.1초 단위로 다르면 total이 표시된다.

plain 최종 rung은 열린 QDP obligation을 같은 AProVE 실행에서 스트리밍 수집한다. 모든 열린
leaf가 있어야 child split을 시작하며, 누락되면 전용 capture를 한 번 재실행한다. 구현 중 실제
AProVE 출력의 마지막 obligation에는 dashed separator와 마지막 newline이 모두 없다는 사실을
확인했다. `Qdp.problems`는 chain 문장을 완결 표지로 사용하고, `Subproc.run`은 실제 EOF에서만
필터의 마지막 partial line을 flush한다. deadline/조기 종료의 잘린 줄은 계속 버린다.

5초 예산, integer retry 비활성, 직렬 실행 결과는 다음과 같다. 전후 모두 같은 열린 child를
검사했고 판정은 바뀌지 않았다.

| 대상 | 변경 전 wall | 변경 후 wall | 판정·coverage |
| --- | ---: | ---: | --- |
| P4 `$repeat__value` | 19초 | 13초 | TIMEOUT, node 25 QDP 검사 유지 |
| meta2 AL `Assign_exp` | 18초 | 13초 | TIMEOUT, 첫 열린 node 175 QDP 검사 유지 |
| meta2 AL `$upcast` | 11초 | 12초 | TIMEOUT, root QTRS node 0이라 re-pose 불가 |

앞의 두 사례는 whole capture 재실행 하나(약 6초)를 제거했다. `$upcast`는 독립 QDP로 만들 수
있는 열린 leaf가 없어 같은 최적화가 적용되지 않는다. 이 표의 TIMEOUT은 5초 예산에서의
미증명이며 비종료 판정이 아니다. 원자료와 KST는
[`_test/timeout-optimization-20260921`](../../../_test/timeout-optimization-20260921/)에 있다.

### Confluence 중복과 termination 표면

CRC upgrade ladder는 구조적으로 같은 non-identity normalized system을 최초 한 번만 검사한다.
고정된 `sig_rules`와 pruned-signature 문맥 안에서만 중복 제거하며 전략 순서는 유지한다. 순수
후보 테스트로 중복 제거를 고정했다. 이번에는 장시간 MFE 대표를 다시 돌리지 않았으므로,
실제 wall 단축값은 새 전수 측정 전까지 주장하지 않는다.

반사 전 Structural CTRS를 `Termination_analysis` target으로 명시해 비교했다. 기본 CLI 채택은
보류했다. P4 `$repeat__value`는 예산 20에서 reflected/raw 모두 TIMEOUT이고 TRS 규칙은
81/82였다. meta2 AL `Assign_exp`는 방정식이 568→462로 줄었지만 unravel chain이 96→151로
늘고 wall이 23.1→31.2초로 악화됐다. 따라서 CLI는 기존 `Analysis` 표면을 유지하며 새 target은
후속 실험용으로만 노출한다.
