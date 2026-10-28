# Rewrite CLI 최소화 설계 — 2026-10-28 KST

사용자는 설계 논의 후 다음 범위의 구현, 관련 코드 축소, tmux에서 GPT-6 Sol xhigh 실행, 검증 후 commit/push를 승인했다. 재승인 없이 실행한다.

## 목표와 공개 계약

rewrite 관련 명령은 rewrite, run, confluence, termination만 남긴다. SCC 명령과 전용 backend/변환/API/테스트를 제거한다. 다른 SpecTec 명령과 P4/impty 인터프리터 CLI는 유지한다. run에서만 impty 프런트엔드와 --imp/--task를 제거한다.

공개 기능 옵션은 아래 14개 고유 이름뿐이다. 기존 help/color 공통 옵션은 별도 유지.
- rewrite: --ctrs, --symbol NAME (ctrs 필요), --list-symbols.
- run: --start TERM, --p4 FILE (반복), -i DIR (반복), --structural, --check-p4, --timeout S (기본 0).
- confluence: --symbol NAME (반복) 또는 --all, --timeout S (기본 60), --out TSV.
- termination: --symbol NAME (반복) 또는 --all, --budget S (기본 300), --out TSV, --emit (정확히 한 symbol), --proof-dir DIR.

## 기본 동작

rewrite 기본은 native 실행 모듈, --ctrs는 base analysis 모듈. 분석 출력의 signature pruning을 기본화하되 전체 sig_rules에 의한 술어 domain 복원 유지. --list-symbols는 symbol TAB rule-count 형식으로 크기 오름차순(동률 symbol) 출력; --sizes 제거.
run은 equation reduction만 한다. raw META-TERM과 P4 입력은 함께 사용할 수 있다. structural은 P4 입력의 직접 reduction만 허용하고 raw --start와 배타적이다. --check-p4는 --p4 필요. native/structural oracle, batch attribution/입력 오류 격리 유지.
confluence는 원래 시스템을 먼저 검사하고 미증명에 대해 기존 WLL 기반 정규화 ladder를 자동 재시도한다. 원래 YES는 유지하고 sound upgrade만 허용한다. 전략/시도 정보 보존. ERROR 승격에 관한 기존 불일치 등 별도 결함을 임의로 광범위 수정하지 않는다.
termination은 plain TRS + sound child split만 수행하며 기존 budget ladder 시작5를 내부 정책으로 고정한다. ITRS/length model은 더 이상 public termination fallback이나 YES 통합에 들어가지 않는다. Itrs exporter와 필요한 모델 테스트는 독립 라이브러리 기능으로 유지 가능하나 termination의 surface/path 플래그 배선은 제거한다.
--out은 현재 TSV append/resume 계약 유지. 새 검사 정책에는 새 TSV 필요함을 문서화한다. 이번 작업에서 대규모 새 artifact/fingerprint 프레임워크는 만들지 않는다.
도구 위치는 기존 SPECTEC_MAUDE_BIN/SPECTEC_MFE_DIR/SPECTEC_APROVE_BIN 환경변수로 지정하며 CLI 경로 옵션은 제거한다.

## 코드 축소와 경계

옵션만 숨기지 말고 없어진 명령/모드의 파싱, record/variant/함수 인자, 미사용 분기, façade exports, dune dependency를 함께 줄인다. backward compatibility aliases, advanced CLI, 중복 대체 드라이버를 만들지 않는다.
SCC 전용 코드 제거. run impty 전용 경로 제거 및 rewrite_cli의 targets.impty 의존성 제거. 규칙 실행/search는 관련 호출자 전수 확인 후 남는 실행 요구가 없으면 Maude_run/To_maude까지 제거한다. ITRS는 위 모델 라이브러리 예외로 보존한다. 살아 있는 공통 rewrite translation/CR C/기존 P4 및 일반 인터프리터 기능은 손대지 않는다.
실행 스크립트 두 개와 CLI/라이브러리 테스트를 새 계약에 맞춘다. 과거 날짜별 측정 기록은 당시 사실로 보존하고 현재 안내/CORE_LOGIC/todo 상단을 갱신한다. source spec의 의미를 바꾸지 않는다.

## 검증

bin/main.exe build, rewrite와 CLI 테스트, formatter, impty base 분석 golden(분석 pruning으로 선언만 바뀌면 의도한 변화 검증 및 golden 재생성)을 확인한다. 외부 Maude/AProVE/MFE를 설치하거나 대규모 sweep을 돌리지 않는다. 실행 테스트는 stub과 raw --start로 이행하고 P4 oracle 스크립트 syntax를 확인한다.
최종 help에서 네 명령의 14개 기능 옵션만 존재함, scc와 제거 옵션 거절, list-symbols 크기 포함, 환경변수 stub 사용, native/structural P4 및 raw 시작항 제약, plain-only 종료성, resume/proof/emit 동작을 검증한다. baseline과 관련 없는 실패는 구분한다.
