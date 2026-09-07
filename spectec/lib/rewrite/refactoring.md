# rewrite 리팩토링 (2026-09-07, KST)

`new-rewrite`의 `04ebcd29`에서 `refactor/rewrite-pipeline-cli` 브랜치를 만들고
별도 worktree `/root/spectec-rewrite-refactor`에서 작업했다.

```bash
git worktree add -b refactor/rewrite-pipeline-cli /root/spectec-rewrite-refactor new-rewrite
```

원본 worktree의 실행 파일·측정 드라이버·TSV는 수정하지 않는다. 빌드와 검증은
별도 `_build`에서 `TZ=Asia/Seoul`, 낮은 우선순위, `dune -j 1`로 수행했다.
AProVE/MFE/CETA 측정 및 P4 전수 실행 오라클은 실행하지 않았다.

## 변경 내용

1. **컴파일 진입점 통합.** `Rewrite.compile ~target spec_il`이 `{ spec; system }`을
   반환한다. `Analysis | Execution`에 따라 스칼라 이론과 분석 마무리 단계만
   선택한다. 항등 모듈 `Simplify`, `To_ctrs.of_spec` 및 내부 수집기의 중복 명세 인자,
   별도 `To_maude.module_of_spec` 경로를 제거했다. 분석 패스 순서는 보존했다.
2. **함수 책임 분리.** CLI를 `lib/cli/rewrite/`의 별도 라이브러리로 옮겼다.
   `bin/main.ml`은 해당 명령의 등록만 담당한다. 옵션 파싱, 실행, TSV 행 렌더링을
   분리하고, 공통 옵션 및 TSV 기록/flush/정리 처리는 공유한다.
3. **실행 처리 통합.** 두 실행 명령의 입력 파싱·예외 처리·배치 결과 연결·P4 결과
   비교를 공유한다. 프런트엔드는 값만 파싱하고, 공통 실행 코드가 한 번 만든
   시스템으로 모듈과 모든 시작항을 인코딩한다. 프로그램별 재컴파일 경로를 없앴다.
4. **슬라이스 처리 정리.** `rewrite --sizes`와 `--slice-dir`도 하나의 인덱스를
   재사용한다. 분석 출력의 predicate signature는 계속 전체 시스템에서 회수한다.
   TSV 컬럼, 중복 결과의 `=source` 표기, 재개 시 기록된 심볼을 건너뛰는 규약은 유지한다.

## API와 명령 변경

| 이전 | 현재 |
|---|---|
| `Rewrite.rewrite_spec spec` | `Rewrite.compile ~target:Rewrite.Analysis spec`의 `system` |
| `Rewrite.maude_system spec` | `Rewrite.compile ~target:Rewrite.Execution spec`의 `system` |
| `Rewrite.translated_spec spec` | 컴파일 결과의 `spec` |
| `To_ctrs.of_spec ~orig:spec spec` | `To_ctrs.of_spec spec` |
| `To_maude.module_of_spec spec` | 컴파일 후 `To_maude.module_of_system compiled.spec compiled.system` |
| `run-structural ...` | `run --structural ...` |
| `termination --emit-trs ...` | `termination --emit ...` |
| `run --reduce ...` | `run ...` (기본 동작) |

과거 이름의 호환 별칭은 남기지 않는다. 저장소의 구조적 오라클 스크립트와 현재
사용 문서를 갱신했다. 날짜별 실험 기록의 명령은 당시 실행을 가리키므로 보존한다.

`run`의 impty 기본 task는 `run`, `run --structural`은 기존과 같은 `check`다.
`--task`로 명시할 수 있다. 구조적 모드는 프로그램 입력의 직접 equation reduction을
지원한다. `--start`/`--search`/`--rewrite`/`--relations-as-rules`와의 조합,
상충하는 출력 변환, 인코딩 플래그 동시 선택은 사용 오류(exit 2)로 알린다.

## 검증

- `bin/main.exe` 빌드, `@fmt`, `@test/rewrite/runtest`, `@test/cli/runtest` 통과.
- 수정 전 실행 파일과 **34개 출력 대조 일치**: impty base/closure의 모듈·심볼·크기·
  WLL·슬라이스·정규화·SCC 입력·TRS 출력과 P4/P4-old의 전체 native/analysis 모듈 및
  심볼 목록. stdout/stderr/종료 코드까지 비교했다.
- impty `--slice-dir`의 57개 파일(모듈과 fidelity 기록)이 수정 전과 일치했다.
- 저장소의 impty 분석 골든(`specs/impty/base/spec.ctrs`)은 그대로 일치한다.
- 실제 Maude: native 8개 프로그램 배치, 단일 실행, search/rewrite가 수정 전과 일치하고
  종료 코드 0. 구조적 `skip.imp`도 수정 전과 동일한 `result: true`, 종료 코드 0.
- 새 회귀 테스트: 특수화 명세/규칙의 대응, 분석/native 분기, 잘못된 CLI 조합,
  배치 중간의 입력 오류 격리, 단일 Maude 호출, 결과 순서와 실패 종료 코드,
  구조적 직접 reduction, TSV 중복/재개를 검증한다. 배치 CLI 테스트는 Maude 대역을
  사용하므로 외부 도구 설치 없이 실행된다.
- 기존 CLI 스냅샷은 특수화 이전 `$lookup`과 오래된 unravel 출력을 기대했다.
  현재 `$lookup_id_type`으로 갱신했으며, 해당 TRS와 진단은 수정 전 바이너리에서도
  동일함을 확인한 뒤 해당 스냅샷만 갱신했다.

**기존 제약:** 식별자 문자열을 포함한 impty의 구조적 실행에서 Maude의
`bad token chr` 오류가 수정 전·후 모두 발생한다. 오류 출력은 임시 파일명을
제외하면 동일하다. 이번 리팩토링은 이 인코딩 결함을 수정하지 않았으며 구조적
8개 프로그램의 성공이나 P4 전수 오라클 통과를 주장하지 않는다.

비교 출력과 실행 로그는 작업 환경의 `/tmp/rewrite-refactor-validation/`에 보관했다.
`comparisons.json`, `execution.json`, `extra.log`, `final-checks.log`가 검증 근거다.

재현 가능한 기본 검사:

```bash
cd /root/spectec-rewrite-refactor/spectec
TZ=Asia/Seoul nice -n 19 ionice -c 3 opam exec --switch=spectecx -- \
  dune build -j 1 bin/main.exe @fmt @test/rewrite/runtest @test/cli/runtest
```
