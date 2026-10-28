# Rewrite CLI 최소화 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 승인된 네 명령·14개 기능 옵션과 그에 필요한 구현만 유지한다.
**Architecture:** 기존 cli/rewrite 경계를 유지하고 내부 정책을 단순화한다. 사라진 기능의 배선을 라이브러리까지 따라 제거하되 독립 Itrs exporter는 보존한다.
**Tech Stack:** OCaml, Jane Street Core.Command, dune, cram, shell.
**Spec:** docs/superpowers/specs/2026-10-28-rewrite-cli-minimal.md

## Global Constraints
- 새 승인 요청 없이 사용자 합의대로 실행. 사용자는 최종 commit/push를 승인했지만 이 worker는 검증 완료 후 parent review를 위해 미커밋으로 인계한다.
- 한국어 보고, TZ=Asia/Seoul, 무거운 실행 직렬. 기존 미커밋 파일은 보존하며 stage하지 않는다.
- 공개 옵션 집합은 설계 문서 그대로. 호환 별칭/새 advanced CLI 없음. 이 작업 밖의 인터프리터/명세 변경 없음.
- 구현은 gpt-6-sol/xhigh 한 작업으로 수행. 다른 모델/하위 구현 agent를 임의로 사용하지 않는다.

## Review Focus
- shared constructors/sorts: pruning이 입력에만 등장하는 structural 생성자를 지우지 않아야 함.
- raw/native/structural와 P4 batch 입력의 결과 attribution 및 오류 격리.
- confluence 자동 정규화가 원래 YES를 낮추거나 모델 판정을 섞지 않아야 함.
- termination은 모델 fallback 없이 증명 입력/증명트리/재개 기능 유지.
- 삭제된 SCC/run impty 참조, old options/alias, 잘못된 façade 및 dune deps가 남지 않아야 함.

### Task 1: 공개 CLI와 회귀 계약 축소
**Files:** spectec/lib/cli/rewrite/*.ml/.mli, spectec/bin/main.ml, spectec/test/cli/{help,analysis-commands,rewrite-run,termination-nested-iter}.t, spectec/lib/cli/rewrite/dune.
**Interfaces:** 현재 Rewrite facade를 사용, 설계의 공개 네 명령/14 옵션을 생성.
- [ ] baseline build 및 관련 tests를 실행하고 로그 저장.
- [ ] 도움말/사용 오류/list-symbols/환경변수 stub 회귀 테스트를 새 계약으로 갱신하고 실제 실패 확인.
- [ ] SCC와 run impty 및 제거 옵션 파싱을 제거. rewrite 목록/자동 prune, confluence 자동 normalize, termination plain-only 기본 배선 구현.
- [ ] raw 시작항 기반 stub 테스트로 기존 impty 실행 테스트 이행; 구조적 입력 제약은 P4 경로 또는 라이브러리 테스트로 검증. 테스트 통과 확인.

### Task 2: 죽은 코드와 불필요한 인터페이스 제거
**Files:** spectec/lib/rewrite/{rewrite,scc,scc_surface,termination}.ml/.mli, maude/{maude_run,to_maude}.ml/.mli 및 실제 소비자, spectec/test/rewrite/*.ml.
**Interfaces:** CLI에는 유지된 동작만 제공. Itrs exporter는 별도 모델 API로 남길 수 있음.
- [ ] rg로 SCC/search/relation-rule/impty 실행 전용 소비자를 확인. 현재 baseline과 분리해 삭제 대상 명시.
- [ ] 호출자 없는 SCC 모듈/exports/tests, mode variants/parameters와 branch, integer fallback orchestration을 제거. 남길 라이브러리 기능의 의미와 tests 유지.
- [ ] 유지된 종료성 plain/child/proof와 Maude reduce/batch 동작에 대한 의미 있는 tests 확인 및 필요 회귀 보강.
- [ ] build + rewrite/CLI test 통과, 제거된 참조 및 공개 옵션 수 재확인.

### Task 3: 자동화·문서·최종 검증
**Files:** check_diff_p4.sh, check_diff_structural_p4.sh, README/current usage docs, spectec/lib/rewrite/{CORE_LOGIC,todo,refactoring}.md 필요 부분.
**Interfaces:** 기존 oracle/resume 작업은 새 CLI로 작동. 과거 측정은 당시 기록으로 보존.
- [ ] 실행 중인 oracle 스크립트 및 현재 문서의 old options를 새 계약에 맞춤; unrelated history 수정 금지.
- [ ] opam exec --switch=spectecx -- dune fmt; 변경 밖의 formatting은 되돌리지 말고 범위를 구분해 보고.
- [ ] bin/main.exe, @test/rewrite/runtest, @test/cli/runtest, @fmt를 직렬 실행. syntax check, help option audit, git diff --check, intended golden 변화 확인.
- [ ] parent에게 작업트리 경로/HEAD, 변경 통계, test command와 exit, 알려진 미해결 사항을 보고하고 미커밋 상태로 인계. 이 worker에서 push하지 않음.
