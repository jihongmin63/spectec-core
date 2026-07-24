# P4 structural CTRS 검증 — 설명·방법·측정 이력·해석

결과 **표는 [verification.md](verification.md)에만** 둔다(표만 깔끔하게). 이 문서는 재현
커맨드, 측정 방법론, 시간·병렬 측정 소견, 커밋별 측정 이력, 그리고 비-YES 행의 해석(왜
도구 한계이지 실제 결함이 아닌지)을 모은다.

> **ChC(Coherence) 측정 중단 (2026-07-24부).** 이후 스윕에서 ChC 열은 측정·기록하지
> 않는다 — `verification.md`에서 축을 삭제했다. 근거: ≤500 전 구간에서 ChC 판정이
> CRC와 항상 일치했고(CRC YES → ChC YES, CRC 미도달 → ChC `-`) 독립적인 결함 신호가
> 없어, 별도 계측이 비용만 늘리고 정보를 더하지 않았다. `confluence` 서브커맨드는
> 여전히 coherence 값을 내보내지만 표·요약에는 CRC만 싣는다.

- **CRC** = Church-Rosser(합류성). 값 = YES / `YES*`(=`--crc-normalize`
  upgrade-only로 닫힘) / MAYBE / TIMEOUT / `-`(미도달). **ChC 열은 2026-07-24부로 삭제**(위 참조).
- **term** = 구조 보존 unravel → AProVE 직접(모듈러-B 아님).
- 표의 `CRC초`/`term초` = 심볼당 **직렬 fresh 측정** 벽시계 초(아래 "시간·병렬 측정" 참조).

---

## 재현 커맨드 (2026-07-22 in-binary 통합, tip `57a99547` 이후)

표의 축들은 스크립트·스크래치패드 없이 바이너리 서브커맨드로 전수 재현한다. 세 분석
검사기는 동일한 스윕 표면을 갖는다 — `--symbol NAME`(반복) 또는 `--all`(작은 슬라이스
먼저), `--out sweep.tsv`(이미 기록된 심볼 skip → 재개 가능):

- `term` = `main.exe termination --all --out term.tsv <specs>` — 구조 보존 unravel →
  AProVE(`lib/rewrite/unravel.ml`+`aprove.ml`). 단일: `termination --symbol NAME`,
  TRS만 보려면 `--emit-trs --symbol NAME`. AProVE 자체 예산은 `--budget S`(기본 300,
  프로세스는 S+120s에 kill).
- `CRC`/`ChC` = `main.exe confluence --all --out crc.tsv <specs>`(구 `verify`). 행
  `<sym>\t<church-rosser>\t<coherence>\t<secs>`. MAYBE/TIMEOUT은 `--crc-normalize`로
  upgrade-only 재검(정규화+prune, YES면 `YES (normalized)`, 하향 없음). `--timeout S`.
- 충분완전성(SCC) = `main.exe scc --all --out scc.tsv <specs>` — CETA Maude 2.7 필요.
- 슬라이스 목록/크기 = `main.exe rewrite --list-symbols [--sizes] <specs>`,
  분석 모듈 덤프 = `rewrite --ctrs --symbol NAME [--prune-signature]`.

`confluence`/`termination`은 각 행 4번째 컬럼에 그 심볼의 벽시계 초(`Subproc.timed`)를
찍는다 — CRC는 정규화가 필요했던 행이면 base+normalize 합, term은 slice+unravel+AProVE.

구 셸/파이썬 드라이버(`run-termination.sh`/`run-scc.sh`/`run-scc-sweep.sh`/
`prune_slice_signature.py`/`prune_modular.py`/`prune_root.py`)는 전부 위 커맨드로
대체·**삭제 완료**(2026-07-22, git history에서 부활 가능). differential 드라이버
`check_diff_p4.sh`/`check_diff_structural_p4.sh`는 서브커맨드 대체물이 없어 유지.

---

## 시간·병렬 측정 (2026-07-23, baseline `bff805ec`)

CRC/ChC·term을 **심볼당 벽시계 시간과 함께** 전수 재측정했다. 측정에 쓴 바이너리는
`bff805ec` + 이 세션의 계측 패치(`Subproc.timed`, `check_batch`/`termination`가 secs
컬럼을 스트리밍). 원자료: CRC = `/tmp/claude-reverify/crc_full153.tsv`,
term = `/tmp/claude-reverify/term_serial.tsv`.

### 병렬 실행은 판정엔 안전하나 시간 측정엔 부적합

프로세스 레벨 샤딩(심볼을 K개로 나눠 독립 프로세스 K개 동시 실행, 각자 독립 MFE 세션)을
실측 검증했다. 각 프로세스는 **서로소·독립 슬라이스**를 결정적 검사기로 돌리므로 판정이
옆 프로세스에 의존할 수 없다 — 구조적으로 안전하고, 실측으로도 확인됐다:

- **판정 정확성: 완벽.** 153심볼 K=8 병렬의 판정은 확정본(CRC/ChC YES 146 / TIMEOUT 7)과
  **완전 일치, 거짓 timeout 0.** upgrade-only 정규화가 base 검사가 경합으로 아슬아슬하게
  timeout 나도 YES를 회복시킨다.
- **그러나 시간은 심하게 오염.** 무거운 심볼(maude RSS 6~8GB)이 여럿 동시에 겹치면
  메모리 압박·스왑으로 감속: `$bin_mul` fresh 43s → K=8 병렬 **1838s(≈40×)**,
  `$bin_minus` 94s → 1708s. K=8·timeout=1800에서 peak 메모리 **92.9GB/124GB**로 천장 근접.
  넉넉한 예산이라 판정은 안 뒤집혔지만(1708 < 1800), 예산이 빠듯하면 거짓 timeout이 될 수
  있는 여백이었다.
- **부수 소견**: 단일 세션에 모듈을 많이 쌓으면 모듈 테이블 성장으로 뒤 심볼이 느려진다
  (`$bin_mul` fresh 세션 43s vs 16-모듈 세션 pos5 88s, ≈2×). MFE `load`는 사실상 0s라
  세션 재사용의 이득이 없다 → **심볼당 fresh 세션이 오히려 빠르고 깨끗하다.**

⇒ **판정용 전수 스윕은 병렬 OK, per-symbol 시간 측정은 직렬 필수.** 표의 시간 컬럼은
아래 직렬 방식으로 측정했다.

### 직렬 정확 측정 (표의 시간 컬럼 방식)

경합·세션 degradation·거짓 timeout을 모두 피하려고, **심볼당 fresh 프로세스(fresh MFE
세션)를 엄격히 직렬로** 돌렸다. 4번째 컬럼 secs는 그 심볼의 참 고립 검사 시간이다.

- **CRC**: 146개 YES-able 심볼을 직렬 fresh로 측정(146/146 YES 재현, 거짓 timeout 0).
  알려진 hard-timeout 7개(`$bin_shl`·`$bin_shr`·`$bitacc_*`×4·`$write_bits_from_value`)는
  직렬로도 각 base 1800+normalize 1800을 소진해 시간 낭비이므로 스윕에서 제외하고 표엔
  TIMEOUT(시간 `>1800`)으로 기록. 참값 예: `$bin_concat` 425.7s(fresh 세션에선 정규화
  없이 base YES — 배치에서 1800이 필요했던 건 경합/degradation 탓), `$write_value*` 5형제
  ~243–253s(정규화, 5개), `$bin_bor`/`$bin_band` ~143s, 나머지 대부분 <1s.
- **term**: 153개를 직렬 fresh(AProVE budget 300)로 측정 → **153/153 YES**. budget 300에서
  비-YES 2건은 1800으로 재시도:
  - `$bin_minus`: budget 300에선 TIMEOUT(420s kill), **budget 1800에서 YES(1800s)** — AProVE가
    이 산술 종료 증명에 300s 넘게 필요했을 뿐.
  - `$write_bits_from_value`: budget 300·1800 모두 **ERROR**("no YES/NO/MAYBE line"). 원인은
    AProVE 증명 탐색이 아니라 **결과 export의 JVM StackOverflowError**
    (`ParallelExportManager.traverseProofTreeRecursively` — 증명 트리가 기본 스레드 스택보다
    깊음)다. `tools/aprove/runme`가 `-Xss` 없이 `java -jar`를 부르는데(기본 ~512KB–1MB),
    `-Xss512m` 래퍼로 감싸니 크래시 없이 **YES(1320s, budget 1200)**. 즉 종료는 증명되며,
    ERROR는 순전히 export 스택 크기 아티팩트다. 표의 term초 1320.4s는 이 bigstack 측정값.
  - 나머지 CRC hard-timeout(`$bin_shl`·`$bin_shr`·`$bitacc_*`×4)은 term에선 모두 YES
    (~300–420s) — **CRC 난이도 ≠ term 난이도.**

---

## 측정 방법론 — term (구조 보존 unravel → AProVE 직접, MTT 안 씀)

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

**term 요약 (구조 보존 경로 도입 효과, 2026-07-19).**
- MTT 경로였을 때는 YES 117 / MAYBE 12 / TIMEOUT 24였다. MAYBE **12건 전부**와 TIMEOUT
  24건 중 **23건**이 닫혔고, 이진 산술 계열(`$bin_*`·`$un_*`·`$bitacc_*`·`$write_value*`)이
  통째로 풀렸다. **인코딩은 한 줄도 고치지 않았다** — 원인은 번역도 AProVE도 아닌 MTT였다.
- 종전 "AProVE 자동 전략의 도구 한계"로 적었던 3건(`$join_text`, `$invalidate_value`,
  `$invalidate_headerUnion`)은 MTT로 1200s를 소진하고 MAYBE였으나 새 경로에서 **각 1초에
  YES**다. 그 진단은 틀렸다.
- ⚠️ 이 항목의 인과 설명은 2026-07-20에 정정됐다. 최초 서술("MTT의 unraveling이 인자를
  분해해 부분항 관계를 역전시키므로 어떤 예산으로도 증명 불가")은 세 군데가 틀렸다(MTT는
  unravel을 안 하고, 분해도 역전도 없으며, 예산만 주면 증명된다). **측정값(153/153)과 실무
  결론(MTT를 거치지 말 것)은 불변**이고 기전만 틀렸었다. 경위는
  [todo.md](spectec/lib/rewrite/todo.md) "방법론 반성".
- 종전 병기하던 `term(모듈러B)` 열(`prune_modular.py abstract-builtins`로 산술을 블랙박스
  처리)은 폐지했다. 직접 축이 153/153이 된 이상 존재 이유가 없다(같은 구조 보존 경로를
  모듈러 축에 적용하면 오히려 YES 150 / MAYBE 2 / TIMEOUT 1로 나쁘다 — keep-생성자가 인자
  구조를 복제해 항이 커지는 비용이 산술 블랙박싱의 이득을 넘어선다).

---

## 측정 방법론 — CRC 정규화 (`--crc-normalize`, upgrade-only) — 2026-07-21

baseline CRC의 MAYBE 5(전부 `$write_value*`)를 분석-전용 정규화(inline + unravel +
real-sort, **upgrade-only**)로 닫았다. 실측(real-sort 바이너리, `prune` 동반, 직렬 Maude,
조기종료):

- **MAYBE 5 → YES 0쌍**: `$write_value_from_bits`·`_prime`, `$write_value_field_from_bits_prime`,
  `$write_value_fields_from_bits_prime`, `$write_values_from_bits_prime` — 표의 MAYBE 5와 일치.
- **회귀**: `$un_op`·`$bin_bor` YES 유지(inline-only, crcu 없어 byte-identical).
  `$set_priorities_of_tableEntryListIR` real-sort로 YES 0쌍 78s(all-Val이면 TIMEOUT).
  `$join_text`는 초기엔 정규화 시 YES→MAYBE였으나, 이는 `crc_unravel`이 값-destructure
  (`text = cons(..)`, subject가 변수)를 불필요하게 unravel해 hoist_matchers가 만든
  CRC-friendly 형태를 깬 **over-unravel**이었다. subject가 정의 함수일 때만 unravel하도록
  게이트(`1dd1e43a`)해 YES 0쌍 6s로 교정.
- **TIMEOUT 회수**: `$bin_concat` inline+prune으로 TIMEOUT→YES. `$bin_shl`/`$bin_shr` 잔존.

세 레버(inline=등식·blanket / unravel=reflect-only·upgrade-only / real-sort=건전한 narrowing)·
건전성 방향·`crcu`/`crck` sort 복원·upgrade-only 프로토콜·`$join_text` 기전은
[CLAUDE.md](CLAUDE.md) "CRC normalization (`--crc-normalize`)" 참조.

---

## 측정 방법론 — term 예산 사다리 (2026-07-24)

**종전 `term초` 열은 난이도가 아니라 우리가 준 `--budget`이었다.** 예산 300으로 잰
153심볼은 `<1초` 119개, `1–100초` **0개**, `300.5–305.8초` 31개, 그리고 개별 상향한
3개(420.2 / 1320.4 / 1800.5)로 갈렸다 — 분포에 뚫린 구멍 자체가 그 열이 무엇을 재고
있었는지 말해 준다.

**기전: AProVE에는 두 부류가 있다.** ① 자동 전략이 즉시 결론을 내면 그 자리에서 발표한다
— 1규칙짜리 `$annotationList_of_parameterIR`의 TRS는 예산 5에서 **438ms**에 YES다(JVM
기동 포함). ② 결론이 바로 안 나면 포트폴리오가 **마감까지 계속 탐색하다** 그때 발표한다.
`term초`를 부풀린 건 ②뿐이고, 위 분포의 `300.5–305.8초` 31개가 정확히 그 부류다.
**"AProVE는 언제나 마감에 발표한다"는 서술은 과장이다** — 재측정 277행 중 144행이 1초
미만에 답했다.

**직접 확인 (2026-07-23).** `$un_bnot`의 unravel된 TRS 하나를 예산만 바꿔 돌리고 출력
줄마다 타임스탬프를 찍어 YES가 나온 시각을 쟀다:

| 예산 | 5 | 10 | 20 | 40 | 120 | 300 | 600 |
|---|---|---|---|---|---|---|---|
| YES 시각(초) | 5.5 | 10.6 | 21.2 | 41.2 | 119.8 | 299.7 | 601.6 |

전부 YES다. **예산 5에서도 YES가 나오므로 진짜 증명 시간은 5초 미만**인데 표에는 303.3초로
적혀 있었다.

**우리 harness 탓이 아니다.** 처음에는 `Aprove.check`의 `~done_when` 누락(프로세스를
EOF까지 읽음)을 원인으로 지목했으나, 옛 바이너리(`bff805ec`)와 `done_when`을 넣은
바이너리를 같은 심볼·예산·스펙으로 나란히 돌린 결과 **302.6초 대 304.2초**로 차이가 없었다.
기다릴 판정 자체가 마감까지 안 나오므로 `done_when`은 도울 수 없다. 누락은 실재하는
결함이라 `025c7979`로 고쳤지만 값어치는 심볼당 약 1.3초(JVM 종료 꼬리)다.

**해결 — 예산 사다리.** `Termination.check`가 unravel된 TRS 하나에 대해 예산을 올려가며
AProVE를 반복 호출하고, 처음 답이 나온 단계에서 멈춘다(`Termination.budget_ladder`). 단계는
5에서 4배씩 커지고 **마지막 단계는 항상 `--budget` cap**이다 — 사다리가 바닥나면 종전의 단발
cap 실행과 정확히 같은 실행이 되므로 **판정은 바뀔 수 없다**. 답이 아닌
판정(MAYBE/TIMEOUT/Error)은 전부 다음 단계로 올라간다. 사다리가 하는 일은 `$un_bnot`처럼
AProVE가 큰 마감까지 탐색할 심볼을 cap(1800초)이 아니라 답이 나오는 작은 예산에서 멈추게
하는 것이다(예산 5에서 5.5초에 YES). **기록하는 것은 그 실행의 벽시계(`term초`) 하나뿐이고,
멈춘 예산은 적지 않는다** — 아래 "예산 열을 지운 이유" 참조.

**`term초` = 답한 실행 하나의 벽시계 (2026-07-24, `a6f056ed`).** 아래에서 실패한 단계들은
빠져 있다. 처음 구현은 사다리 전체를 쟀는데 그러면 `$bitacc_offset_op`이 29.2초로 찍혔다 —
실패한 예산 5의 6초 + 증명한 예산 20의 21초다. 탐색 비용을 심볼의 비용으로 보고하는 건 이
탐색이 없애려던 바로 그 혼동이라 승자 실행만 재도록 고쳤다(21.6초). AProVE를 아예 안 돌린
행(DEGENERATE, unravel 실패)은 `0.0` 대신 `-`다.

**예산 열을 지운 이유 (2026-07-24, 사용자 지적).** 사다리가 멈춘 예산을 한동안 `term예산`
열로 병기했으나 지웠다. 근거는 "AProVE가 예산 마감을 기다리지 않는다"는 실측이다: 1규칙
TRS는 예산 5에서 **438ms**에 YES이고, 재측정 277행 중 **171행이 자기 예산보다 일찍**
답한다. 그러면 예산은 두 부류 모두에서 잉여다 — 즉답 부류에서는 실제 시간(0.4초)과 무관한
천장값이고(275/277행이 예산 5 상수), 마감 부류에서는 예산 ≈ `term초`라 중복이다. 남는 정직한
심볼당 수치는 `term초` 하나뿐이다.

**⚠️ 첫 구현의 오진 — Error를 영구 실패로 본 것 (당일 정정).** 최초 사다리는 AProVE의
`Error`를 "예산과 무관한 실행 실패"로 보고 곧장 cap으로 점프시켰다. 실측이 반박한다:
`$bitacc_offset_op`의 TRS를 예산 5로 직접 돌리면 6초 만에 종료하며 **판정 줄을 한 줄도
찍지 않고**(734줄 전부 서사와 스택트레이스) 끝난다 — 우리 코드에는
`Error "no YES/NO/MAYBE line"`으로 보이고, 이는 JVM이 죽었을 때와 구별되지 않는다. 그런데
**같은 TRS가 예산 20에서 22초에 YES**다. 즉 cap 점프는 답이 나오는 rung을 건너뛰고 1800초를
태웠다. 수정(`a1fd043b`): 답(`Yes`/`No`)이 아닌 모든 판정은 등반한다. 진짜로 영구적인
오류인 "바이너리 없음"만 사다리 진입 전에 한 번 검사한다. 판단은 순수 술어
`Termination.decisive`로 분리해 유닛 테스트가 잡는다.
→ 저장소에 이미 적혀 있던 교훈(2026-07-20 MTT 오진 정정)의 재확인이다: **도구가 실패하면
이론을 세우기 전에 그 도구가 실제로 뱉는 것을 확보하라.**

**≤500 재측정 결과 (153심볼, `a6f056ed`, 직렬 fresh, 한산한 머신).** 판정 **153/153 YES
불변**. 151심볼이 첫 사다리 단계(≤6초)에 답하고 `$bitacc_range_op`·`$bitacc_offset_op`
둘만 둘째 단계(21.6초)까지 간다. 답한 실행의 벽시계 합은 **279.8초**로, 종전 열이 재던 예산 300 고정의 합 13,000.9초(3.61시간)와는
잴 대상 자체가 다르다 — 종전 값이 그 심볼의 난이도가 아니라 우리가 준 예산을 재고 있었다는
게 요지다.

**옛 표의 "어려운 심볼" 순위는 전부 예산 아티팩트였다.** 상위 5개의 새 값:

| symbol | 옛 term초(예산 300 고정) | 새 term초(답한 실행) |
|---|---|---|
| `$bin_minus` | 1800.5 | 5.6 |
| `$write_bits_from_value` | 1320.4 | 6.0 |
| `$bin_shr` | 420.2 | 5.8 |
| `$write_value_from_bits_prime` | 321.7 | 5.7 |
| `$bitacc_offset_op` | 319.7 | 21.6 |

1위였던 `$bin_minus`는 예산 5에 답한다. 실제로 남는 난이도 차이는 `$bitacc_{range,offset}_op`
둘뿐이고, 그마저 rung 하나 차이다.

**비용 특성.** 답이 낮은 rung에서 나오면 사다리 비용은 그 rung 값에 수렴한다. 반대로 cap까지
가야 하는 심볼은 사다리 합(≈1.3–2배 cap)을 낸다 — cap 단발 실행보다 비싸다. ≤500에는 그런
심볼이 없고, 이 교환으로 스윕 전체가 44배 빨라졌다.
---

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
접을 destructure-동반 matcher가 없다).

**CRC/ChC 열 재측정 (2026-07-13, 같은 커밋) — 변화 없음.** 같은 fold가 head를
서로소화하니 CRC도 움직일 것으로 봤지만, 열의 값은 하나도 바뀌지 않았다.
① CRC가 YES가 아니던 행 중 `$join_ctk` `$assignop_as_binop` `$bin_satplus`
`$bin_concat` `$bin_shl` `$bin_shr` `$write_bits_from_value`는 fold 전/후 분석
슬라이스가 **바이트 동일**하다 — verdict가 움직일 수 없으므로 재측정 대상이 아니다.
② 슬라이스가 실제로 바뀐 비-YES 행은 `$write_value*` 5개(MAYBE)와 `$bitacc_*` 4개
(TIMEOUT)뿐이다. ③ **회귀 없음**: fold로 슬라이스가 바뀐 기존 YES 8개를 다시 돌려 전부
CRC/ChC YES/YES 유지 확인. 즉 이 커밋의 순이익은 termination 쪽에만 나타난다.

**CRC 열 후속 — 상보 비교 가드 정렬 (2026-07-13, `feat(rewrite): align complementary
comparison/negation guards for the CRC`, a290977b).** 분석 전용 패스 `Reflect.align_guards`가
조건 위치의 `lt`/`lt_int`(및 선두 `not`)을 정준 `leq`/`leq_int` 술어의 반대 극성으로
재철자한다. `i<0`(arith shift) vs `i>=0`(logical shift)로 갈리는 형제 절이 갖던 서로 다른
subject를 같은 subject의 true/false 극성으로 통일해, CRC가 가설 재작성으로 임계쌍을
discharge한다. 대상은 상보 비교쌍을 가진 3심볼뿐(`$bin_satplus` MAYBE, `$bin_satminus`
이미 YES, `$bin_shr` TIMEOUT). 축소 슬라이스 실측: `$bin_shr` MAYBE(6 임계쌍)→YES(0).
전체 슬라이스는 산술 라이브러리 CP 폭발이 지배적이라 verdict가 그 아래 가린다(순이익은
축소 슬라이스에서만 실측). 근본 원인은 sort — prelude bridge `lt_int(x,y)=not(leq_int(y,x))`가
하위 sort `IntV`에 선언돼 `Val`-wide 복원 항에 발화 못 함. align_guards는 `leq_int`를 직접
써 우회. (이 `Val`-wide 도메인 문제는 여러 산술/판정 심볼 공통이며 todo.md "subty_*/match_*
op 도메인 협소화(P1)"와 같은 뿌리.)

**`$join_ctk`/`$assignop_as_binop` CRC 열 재측정 (2026-07-16) — MAYBE → YES.** 7-13에서
"match fall-through라 align_guards 대상 아님"으로 남겨뒀던 이 두 행의 진짜 원인 확정: 진짜
비합류가 아니라, owise 반사가 만드는 왼쪽-중첩 `or(and(match,match)…)=false` 게이트에서 참
disjunct가 깊이 묻히면 CRC feasibility 검사가 못 보는 **인코딩 아티팩트**였다. 수정:
`Reflect.owise`에 **complement 열거** 구현(enum-dispatch owise를 미매치 생성자 튜플별 ground
fall-through 절로 반사 → 절이 전부 ground·서로소 → 임계쌍 소멸). `$join_ctk` YES(5절),
`$assignop_as_binop` YES(1절). 회귀 없음 확인. 상세는 todo.md M1 2026-07-16.

### 2026-07-18 post-fix 전면 fresh 재검증 (CRC + AProVE 직접)

owise-complement 열거(`2f9f8cba`)·align_guards(`a290977b`) fix는 번역 덤프 자체를 바꾸므로,
스테일 스윕을 전부 중단하고 **현재 바이너리로 153심볼을 하나씩 재번역**해 CRC를 처음부터
재계산, termination은 AProVE 직접(`TERM_TMO=1200`, `CRC_TMO=2400`)으로 재측정. `/tmp/fresh500`,
153/153 완주. 요약 — CRC: YES 140 / TIMEOUT 8 / MAYBE 5 · ChC: YES 145 / - 8 · term(AProVE
직접): YES 117 / TIMEOUT 25 / MAYBE 11.

1. **fix 정정 재확인.** 스테일에서 MAYBE였던 `$join_ctk`·`$assignop_as_binop`이 fresh
   전면 재검에서 CRC=YES/ChC=YES/term=YES. 회귀 0.
2. **새 비합류/비종료 후보 0.** 비-YES 37행은 전부 산술/비트 슬라이스이며 알려진 도구 한계.
3. **⭐ term 열은 여기서 AProVE 직접이다.** full-arith 25개 TIMEOUT은 AProVE가 이진 산술
   종료를 1200s 내 못 찾은 것이고, 같은 심볼이 모듈러-B에선 전부 YES였다 — **정확성이 아니라
   tractability.** 이후 2026-07-19에 구조 보존 unravel로 바꿔 153/153 YES 달성(위 term 요약).

---

### 2026-07-24 표 통합 + termination 전수 재측정 (`bdceb303`)

**왜 다시 쟀나.** `bdceb303`(`orient_conds`)이 조건의 평가측/패턴측 방향을 바로잡아
**분석 표면을 바꾼다**. 종전 termination 열은 `a6f056ed` 기준이라 그대로 옮길 수 없었다.
단순한 재확인이 아니라 **판정의 근거가 달라진** 경우다 — 뒤집힌 조건은 unravel에서
패턴측이 헬퍼 lhs로 올라갈 때 defined 심볼을 거기 놓아 체인을 끊었고, 끊긴 뒤의 재귀는
AProVE에 보이지 않았다. 즉 영향 슬라이스의 종전 YES는 **틀린 게 아니라 미증명**이었다
(경위·실측은 `lib/rewrite/todo.md`의 2026-07-24 절).

**방법.** 동일 커맨드(`termination --all --budget 1800 --aprove-bin <bigstack>`), 직렬,
심볼당 fresh 프로세스, 유휴 머신. 표의 277심볼 전부가 이 스윕에 포함됐다(미측정 0).

**결과 — 판정 변경 0건, 시간도 오차 범위.**

| 밴드 | 종전 합 | 재측정 합 | 비 | 0.15초 넘게 느려진 심볼 |
|---|---|---|---|---|
| ≤500 (153) | 279.8초 | 277.3초 | ×0.99 | 1 (`$un_minus` +0.2초) |
| >500 (124) | 479.6초 | 482.5초 | ×1.01 | 11 (최대 +0.3초) |

체인이 되살아난 ≤500의 5심볼(`$is_tableDefaultActionProperty` 16규칙,
`$optional_annotation_of_parameterIR_prime_prime` 20, `$set_priorities_of_tableEntryListIR_prime`
200, `$set_priorities_of_tableEntryListIR` 226, `$name_annotation_opt` 256)은 **시간이 동일**
하다(0.5/0.5/5.7/5.7/6.0초). 비용이 안 드는 이유: unravel이 내는 규칙 **개수**는 방향과
무관하게 같고(내용만 바뀐다), 되살아난 하강은 형제 절이 이미 증명하던 것과 같은 구조
재귀라 추가 탐색이 없다.

**⚠️ 대형 관계 슬라이스는 다르다.** 표 밖의 `Program_ok`(60,723식 / 67,634규칙)는 수정 전
**YES 440.3초**였는데, 수정 후 사다리 5·20·80·320·1280을 **전부 소진하고도 답하지 않았다**
(1800 rung 진입 후 측정이 외부 요인으로 중단되어 최종 판정 미확인). 그 슬라이스에는 뒤집힌
조건 소유 심볼이 대거 들어 있다(`Expr_ok` 84규칙, `Decl_ok` 41, `TableDefaultAction_ok` 32,
`Type_ok` 22, `Stmt_ok` 20, `TableEntry_action_ok` 17, `TableProperty_ok` 8). ≤500이 무비용
이었던 건 거기 영향 심볼이 5개뿐이고 전부 작았기 때문이다 — **비용은 영향 절의 밀도에
비례하며, 최대 관계에서는 판정이 흔들릴 수 있다.**

**confluence 열의 stale 범위.** 뒤집힌 조건은 분석 표면 142건이고 **2,356 슬라이스 중
282개**가 그중 하나 이상을 포함한다. confluence 열은 `bff805ec`(수정 이전) 값이므로
그 282개에 해당하는 행은 stale이다. 재측정은 심볼당 20~25분이라 별건으로 남긴다.

**표를 하나로 합친 이유.** ≤500 / >500 경계는 측정 시기(≤500 먼저, >500은 나중 bigsweep)의
산물이지 성질의 구분이 아니었고, 두 표가 컬럼 형식마저 달라(`#` 열 유무) 대조가 번거로웠다.
행은 규칙 수 오름차순 한 줄로 세우고, 열 이름은 그 열을 만드는 서브커맨드(`confluence` /
`termination`)와 일치시켰다.

## 비-YES 행 해석 (≤500)

CRC의 잔여 비-YES 7행(`$bin_shl`·`$bin_shr`·`$bitacc_offset_op`·`$bitacc_offset_replace_op`·
`$bitacc_range_op`·`$bitacc_range_replace_op`·`$write_bits_from_value`)은 전부 **무한 비트폭
재귀가 있는 비트벡터 산술 연산자**로, CRC 검사기의 산술 임계쌍 폭발로 예산 내 판정 불가다
(1800s 직렬·병렬 모두 TIMEOUT). ChC는 이 행들에서 도달 못 함(`-`). **비합류 아님** — 이들은
well-defined total 함수이며, 임계쌍 배타가 참이나 `Val`-wide subty 여집합 계산이 예산을
넘는 것뿐이다. 나머지 146행은 CRC/ChC YES(정규화 5건 포함).

---

## >500 구간 (bigsweep)

`≤500` 종합 스윕 밖의 대형 슬라이스(501~2000규칙, **124심볼**).

**term은 2026-07-24에 전수 재측정됐다(`a1fd043b`, §1과 같은 구조 보존 직접 축 + 예산 사다리,
cap 1800): 124/124 YES, 답한 실행의 벽시계 합 479.6초(전부 첫 단계에서 답).** 종전 §2가 이고 있던 `term(B)`
(모듈러-B: `prune_modular.py abstract-builtins`로 산술 블랙박스) 열은 폐기했다 — 축이 다르고,
그 축의 MAYBE 11건은 직접 축에서 전부 YES다. CRC 열은 이번에 재측정하지 않았고 옛 bigsweep
값(27심볼분)만 §2에 남아 있다.

**밴드 크기 127 → 124.** 옛 §2 제목의 분모 127은 과거 심볼 집합의 개수다. 현재 바이너리에서
`501 < rules ≤ 2000`은 124이고(`big500_sized.tsv`, rules 748~1840), 줄어든 원인은 iter 헬퍼
패밀리 통합(2026-07-18: `$iterapply`/`$iterproj` → `$itercollect`, `$unzip` → `$iterproj`)으로
$-심볼 몇 개가 합쳐졌기 때문이다. 버그가 아니라 세대 차이다. 501~747 구간에는 심볼이 없다.

**이 구간은 이진 산술 커밋과 무관하다.** helper 열이 전부 비어 있고, 심볼은 전부
list-flatten / id-accessor / prototype-분류 / 코어션(`as_lvalue`) 계열이다. 대형인 이유는
이들이 `subty-<T>` 여집합 가족을 슬라이스로 크게 끌어오기 때문.

### CRC=TIMEOUT (5): `$flatten_{typeArgument,expression,argument,simpleKeysetExpression}List`, `$expressionNonBrace_as_expression`
flatten 3절 구조(EMPTY / 싱글턴 `subty-elem=true` / 재귀 `match-comma=true`)에서 싱글턴·재귀
절이 같은 head에 겹치고, CRC는 두 가드가 상호배타임을 증명해야 discharge. 배타는 참이나
증명하려면 `subty-<elem>` 여집합 가족(수백 규칙) 전체 임계쌍 계산 필요 → 자원 소진 TIMEOUT.
**비합류 아님**(flatten은 total). `Reflect.expand_subty_guards`가 ≤500에서 풀던
subty-disjointness가 슬라이스 규모 때문에 완주 못 하는 케이스.

### CRC=MAYBE (1): `$expression_as_lvalue` (764)
다수 무조건 원소 절 + 재귀 절. 위 subty-가드 배타를 CRC가 완전 discharge 못 한 잔여.
well-defined 부분함수 — **false MAYBE.**

### 폐기된 `term(B)=MAYBE (11)` 항목 — 직접 축에서 전부 YES
모듈러-B 축에서 MAYBE로 남던 flatten/optional/split 계열 11건(재귀 인자 `xs`가
`x'=comma(xs,e)` **전제에서** 나와 `xs ⊂ x'`가 syntactic subterm이 아니라 dependency-pair
분석이 감소를 못 본다는 해석이었다)은 구조 보존 직접 축에서 **전부 YES**다(첫 단계에서).
그 MAYBE는 종료성에 대한 사실이 아니라 모듈러 인코딩의 성질이었다. §1이 2026-07-19에
모듈러 축을 폐지한 것과 같은 결론이 이 구간에서도 확인된 셈이다.

### 총평 (>500)
**term은 124/124 YES — 비종료 후보 0이고 미해결 term 행이 없다.** 남은 비-YES는 CRC 쪽뿐이고
(옛 27심볼분 측정에서 MAYBE 1 / TIMEOUT 5), 그 원인은 subty-여집합 배타의 CRC 미완주(규모)로
알려진 도구 근사이며 번역 버그가 아니다. **이 구간의 CRC 전수 측정은 미완이다** — 심볼당
20~25분(구 드라이버 실측 1450초/심볼)이라 124심볼이면 2일 규모다.

---

## TODO

- [x] **≤500 `term` 열 재측정 + 모듈러B 축 폐지** (2026-07-19). MTT 제거, 구조 보존
  unravel → AProVE 직접으로 153/153 YES.
- [x] **≤500 `CRC`/`ChC` 열 재측정** (2026-07-23). 현재 바이너리(`bff805ec`)로 전수
  재측정 + 시간 계측. CRC/ChC YES 146 / TIMEOUT 7. 병렬(K=8)로 판정 확인 후, 시간은 직렬
  fresh로 정확 측정(위 "시간·병렬 측정").
- [x] **`term` 열의 기준 커밋 확인** (2026-07-22). HEAD 재덤프 unravel TRS가 측정 당시
  골든과 153/153 byte-identical — sort 태그 변경은 구조 보존 경로에 불변.
- [x] **구조 보존 unraveler 승격** (2026-07-22). in-binary 포팅(`termination` 서브커맨드),
  MTT 경로 폐기(커밋 d3bf2847).
- [x] **실행 경로 커맨드 통합** (2026-07-22, tip `57a99547`). `confluence`/`termination`/`scc`
  동일 스윕 표면 + `rewrite --list-symbols`/`--ctrs --prune-signature`.
- [x] **§2 >500 term 열 갱신** (2026-07-24, `a1fd043b`). 27행 stale 표를 124행으로 교체하고
  `term(B)`(모듈러) 축을 §1과 같은 구조 보존 직접 축 + 예산 사다리로 재측정: **124/124 YES,
  전부 첫 단계, 합 479.6초**. 모듈러 축의 MAYBE 11건은 전부 닫혔다.
- [ ] **§2 >500 CRC 열 전수 측정.** 이번에 잰 축은 term뿐이고 CRC는 옛 27심볼분만 있다.
  심볼당 20~25분이라 124심볼은 2일 규모 — 별도 스윕이 필요하다.
- [x] **폐기 스크립트 삭제** (2026-07-22 완료). `run-scc.sh`/`run-scc-sweep.sh`(→ `scc`),
  `prune_slice_signature.py`(→ `rewrite --prune-signature`) 삭제. 삭제 시점의 reverify 스윕은
  이미 `confluence --all --crc-normalize`(in-binary 프루닝)로 옮겨가 python 프루너를 더는
  호출하지 않아 게이트 해소. **미검증 caveat**: `scc` 실 verdict는 CETA Maude 2.7 에셋 부재로
  옛 `run-scc.sh`와 행 diff를 못 했다(모듈 방출 텍스트는 cram `scc --emit`로 byte 확인). 에셋
  확보 시 `run-scc.sh`를 git history에서 부활시켜 대조할 수 있다.
- [ ] **keep-생성자 항 크기 최적화(선택).** 폐지한 모듈러 축에서 구조 보존이 MTT보다 나빴던
  3건 원인이 keep-생성자 인자 복제인지 확인. §1 직접 축은 이미 153/153이라 급하지 않다.
