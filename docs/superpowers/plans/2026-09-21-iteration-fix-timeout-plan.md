# 반복 바인더 오류 이식 및 Timeout 분석 계획

> 실행: `superpowers:executing-plans` 절차로 현재 세션에서 수행한다. 사용자가 원본 브랜치 반영을 지시했으므로 `new-rewrite`에서 진행하며 커밋하지 않는다.

**목표:** extern 작업 트리의 문법 독립적인 반복 바인더 오류 수정을 이식하고, 검증 Timeout을 줄일 범용 전략을 증거와 함께 제시한다.

**구조:** `To_ctrs.iterpr_defs`의 입력 spine 이름 변경과 출력 binder 사용을 분리한다. exporter의 unbound-variable 검사는 보존한다. 성능 변경은 이번에 구현하지 않는다.

**기술:** OCaml, SpecTec IL, Dune cram, Maude MFE, AProVE.

**요구사항:** 사용자 요청 및 `/root/spectec-core/AGENTS.md`.

## 제약과 판단

- 원본은 `new-rewrite`, 참조는 `.worktrees/extern-meta-confluence`의 미커밋 수정이다.
- extern/rulegroup 문법과 이를 지원하는 AST 변경은 이식하지 않는다.
- 현재 확보한 upstream은 `meta2@da36ac3c`; `squaring`이라는 대상은 사용자 확인 중이다. 동일하다고 단정하지 않는다.
- 기존 측정 TSV는 보존한다. 새 실험은 별도 디렉터리, KST, 직렬 실행이다.
- Timeout은 미증명이다. 입력 변환의 정당성과 측정 시간 감소를 별도로 평가한다.

## 작업 1: 오류 재현과 최소 이식

파일: `spectec/test/cli/termination-nested-iter.t`, `spectec/lib/rewrite/translate/to_ctrs.ml`.

- [x] 참조 작업 트리의 회귀 테스트를 복사한다.
- [x] `cd spectec && TZ=Asia/Seoul opam exec --switch=spectecx -- dune build @test/cli/termination-nested-iter`를 실행해 structured binder의 unbound RHS 실패를 확인한다.
- [x] `iterpr_defs`의 논리 변경만 이식한다: `rename_step_prem bound_ids inner`, collector element는 원래 binder 이름, projection pattern은 자신의 stepped 이름을 사용한다.
- [x] 같은 테스트를 재실행하고 성공을 확인한다. 다중 출력 projection 경로도 TRS export가 성공하고 unbound RHS가 없는지 확인한다. 실제 결과 값 동등성 전수 검증으로 해석하지 않는다.
- [x] `bin/main.exe`, `@test/rewrite/runtest`, `test/cli`를 직렬 검증하고 impty golden을 비교한다. 변경 파일만 포맷한다.

## 작업 2: 근거 기반 Timeout 전략

파일: `docs/superpowers/plans/2026-09-21-generic-verification-timeouts.md`, `spectec/lib/rewrite/todo.md`.

- [x] 기존 AL/SL 입력, coverage TSV, 로그, emitted modules를 확인한다.
- [x] 코드와 대조하여 reflection 확대, recursive SCC, 중복 whole-run capture 및 budget ladder 시간 집계의 영향을 구분한다.
- [x] 측정 기반 확정 사항과 아직 검증되지 않은 최적화 가설을 분리한다.
- [x] 공통 계측/정확한 입력 해시 재사용 → 증명 표면 분리 → 구조적 dispatch/귀납적 감소 → 선택적 재시도 순으로 실험 변수, 성공 조건, 정당성 조건을 작성한다.
- [x] 이번 오류 수정으로 이전 confluence/termination 숫자가 새 트리의 측정값이 되지는 않음을 기록한다.

## 검토 초점

중첩 list/option binder, tuple 다중 출력, output의 fusion/escape, 관계 직접-map 경로, helper 변수 포획을 검토한다. 기존 문법과 기존 사용자 변경을 보존한다.

## 실행 결과 및 판단 기록

- 작업 1 완료: 원본에서 중첩 binder export 실패(RED)를 확인한 뒤 함수 하나만 이식하여 같은 cram 통과(GREEN). 직접 relation-map 경로는 유지했다.
- 작업 2 완료: `2026-09-21-generic-verification-timeouts.md`에 기존 근거, 시간 집계의 차이, 여섯 단계 실험과 판정 이전 조건을 기록했다. 성능 변경은 구현하지 않았다.
- 판단: 사용자 요청에 따라 원본 `new-rewrite`에서 직접 반영하고 미커밋으로 유지했다. 참조 작업 트리는 변경하지 않았다.
- 판단: `squaring` 경로는 확인 요청 중이다. 실제 확보한 `meta2`만 근거로 삼으며 별도 명세 검증으로 주장하지 않는다.
- 검증: 빌드, rewrite 단위, CLI, 변경 OCaml 파일 포맷, impty golden 통과. 전체 `dune runtest`는 rewrite에 의존하지 않는 P4 golden 네 건에서 실패했다. 기대값을 수정하지 않았다.
- 별도 읽기 전용 리뷰: 중첩 반복, projection/fusion, capture, 직접 relation-map 경로에 차단 사항 없음. export 테스트가 실행 결과 전수 동등성을 증명하는 것은 아니라는 한계를 본문에 명시했다.
