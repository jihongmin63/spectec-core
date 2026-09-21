# `rewrite` 재구현 TODO (new-rewrite)

**골격은 전부 채워졌습니다 — `failwith` 스텁은 하나도 남아 있지 않습니다**(코드
전수 grep 확인). 번역(`To_ctrs.of_spec`/`var_type_hints`), 지원 패스
(`Defunctionalize`/`Gensym`/`Builtin`/`Exp_map`), Maude 백엔드
(`To_maude`/`Maude_run`/`Of_maude`), confluence 게이트(`Mfe`)가 모두 구현·컴파일되고
동작합니다. **M2(실행)·M3(오라클)도 완료** — `run --imp/--p4/--check-p4` 실행 배선과
same-spec differential(전체 corpus; completeness 0 / soundness 1(알려진 issue1944) /
Phase D 결과-VALUE 1227/1227 MATCH)까지 끝났습니다. **분석 confluence의 subty-가드
MAYBE도 해소** — B′ 7심볼 전부 YES(2026-07-07, `Reflect.expand_subty_guards`), owise
반사 72/72 달성 + `drop_owise` 폴백 제거, holds_R output-carrying 일반화까지 완료
(아래 2026-07-07 항목). **구조적 CTRS differential**(binary nat 이론, Phase D
1227/1227 MATCH)과 **termination 스윕**(153심볼 CRC+term, [verification.md](../../../verification.md))도
완료됐습니다. 남은 일은 아래 "남은 작업" 블록(대형 슬라이스 CRC의 슬라이스 축소,
`$bitstr_to_int` w=0 비종료, LTL 모델 검사, SCC, 잔여 MAYBE)입니다.
알고리즘 설계 기준은 [CORE_LOGIC.md](CORE_LOGIC.md), 모듈 상태는
[CLAUDE.md](CLAUDE.md) 참고. **실험의 우선순위는 이제 논문 계획
([PAPER.md](../../../PAPER.md), CAV 2027 Short Application Papers, rev.3)이 함께
정합니다** — 측정 freeze 10월 말, `== false` 근사 정량화(실험 ⑨)와 증분 게이트 비용(실험 ⑩)이
그쪽의 최우선 항목입니다. 이 파일은 **무엇을 할지**, PAPER.md는 **왜 그것을 쓰는지**를 답니다.

> **원자료 주의 (2026-08-20).** 아래 날짜별 항목들이 증거로 인용하는 `sweeps/` 밑의
> 측정 산출물(`.tsv`/`.itrs`/`.trs`/`.qdp`/`proof_*`)은 대부분 추적되지 않는 스크래치였고
> 2026-08-20 정리에서 삭제됐다. **항목 본문에 적힌 수치가 기록의 전부이며**, 인용된
> 파일명은 그 측정이 무엇이었는지를 가리키는 이름으로만 읽어야 한다. 살아남은 것은
> `sweeps/itrs-probe/closed-form-decode.patch`와 커밋된 `sweeps/*.md`뿐이다. 재측정은
> 파일을 되찾는 게 아니라 스윕을 다시 도는 것이다.

## 현재 구조

- 번역 진입점: `Rewrite.compile ~target:(Analysis | Execution)` → `{ spec; system }`.
  `Defunctionalize → Monomorphize → To_ctrs (+Builtin) → Gensym → orient_conds`를
  공유하고, 분석 목표에만 `Reflect`/`Crc_surface`를 적용한다.
- 항등 모듈 `Simplify`와 `To_ctrs.of_spec`의 중복 명세 인자를 제거했다.
  `Prem_env` 단순화는 도입하지 않는다.
- CLI: `lib/cli/rewrite/`에서 `rewrite`, `run`, `confluence`, `termination`, `scc`를
  구현한다. `bin/main.ml`은 이 명령들을 등록한다.
- 실행 경로는 `run`으로 통합했다. 구조적 오라클은 `run --structural`이고,
  내보내기 플래그는 `termination --emit`이다. 과거 명령 이름은 별칭으로 남기지 않는다.
- 백엔드와 판정 로직은 유지한다. 날짜별 과거 측정은 아래 기록의 시점에 한정된다.

이번 리팩토링의 변경 범위와 검증: [refactoring.md](refactoring.md).

### 2026-09-21 — 중첩 반복 출력 바인더 수정 원본 이식

`extern-meta-confluence` 작업 트리에서 발견한 문법 독립적인
`To_ctrs.iterpr_defs` 수정을 `new-rewrite`에 이식했다. 반복 입력만 `__hd`로
바꾸고, collector는 내부 조건이 실제 바인딩한 출력 이름을 사용한다. 다중 출력
저장 tuple과 projection의 stepped pattern을 분리해 기존 projection/fusion을
보존했다. extern/rulegroup 문법 변경은 포함하지 않는다.

`termination-nested-iter.t`의 중첩 `val*?*` 사례는 수정 전
`unbound rhs variable`로 실패했고 수정 후 통과했다. 다중 출력 export,
rewrite/CLI 테스트, 빌드, 변경 파일 포맷, impty CTRS golden도 통과했다.
전체 `dune runtest`에는 rewrite에 의존하지 않는 P4 golden 4건
(`elab`, `struct`, `roundtrip/el`, `roundtrip/il`)의 기존 기대값 차이가 남아 있다.
기대값을 갱신하지 않았으며 커밋도 하지 않았다.

Timeout은 별도 과제다. 기존 AL `Assign_exp`의 실제 wall은 1,425초지만
경로 시간 합은 1,203.5초다. budget 사다리의 하위 시도 시간이 집계에서 빠지고,
`children`에는 QDP 수집을 위한 whole 재실행도 포함된다.
단계별 범용 단축 전략과 판정 범위는
[분석 계획](../../../docs/superpowers/plans/2026-09-21-generic-verification-timeouts.md)에
정리했다. 기존 메타 명세는 `meta2@da36ac3c`이고, 요청의 `squaring` 대상은
확인되지 않아 동일하다고 단정하지 않는다. 이전 53개 export 성공과 CRC/termination
측정은 당시 참조 트리의 기록이며 이번 원본 트리 전수 재측정 결과가 아니다.

후속 구현에서 budget rung별 proof/전체 시간을 보존하고, 최종 plain rung의 QDP
echo를 child split에 재사용했다. AProVE 마지막 obligation에 separator와 newline이
없다는 실제 출력 형식도 파서와 스트리밍 필터에 반영했다. 모든 열린 leaf가 없으면
전용 capture를 한 번 재시도하므로 일부 child만으로 YES를 만들지 않는다. 5초 직렬
비교에서 P4 `$repeat__value`는 19→13초, meta2 AL `Assign_exp`는 18→13초였고
동일한 열린 QDP를 검사했다. `$upcast`는 root QTRS가 열린 형태라 11→12초로 개선이
없었다. 모두 TIMEOUT(미증명)이다.

CRC normalize ladder는 구조적으로 같은 non-identity 입력을 한 번만 검사한다. 반사 전
Structural `Termination_analysis` target도 추가했지만 CLI 기본값에는 채택하지 않았다.
`Assign_exp` raw 표면은 방정식이 568→462로 줄어도 chain이 96→151로 늘며
23.1→31.2초로 느려졌다. 세부 근거와 산출물은 위 [분석 계획] 및
`_test/timeout-optimization-20260921`에 있다.

### 2026-09-07 — 구조적 실행의 `bad token chr` 수정

규칙에는 없고 입력 문자열에만 있는 `chr`가 실행 모듈에서 선언되지 않았다.
`To_mfe`의 직접 실행 모드에 기본 생성자 선언을 보충했다. impty base 구조적 배치
8개 모두 `true`, native 배치 8개 결과 유지, 분석/native 내보내기 12개 대조 일치.
선언 누락 회귀 테스트와 빌드·포맷·rewrite/CLI 테스트 통과. 분석용 선언과 규칙은
그대로이며 새 측정은 수행하지 않았다. 상세 근거는 [refactoring.md](refactoring.md)의
후속 수정 항목에 기록했다.

## M1 — 분석 동작 (CTRS 생성 + confluence)

### 2026-09-01 — 【research note】 finite variant property는 이 파이프라인에 적용 대상이 아니다 — 그러나 그 전제가 이진 nat 인코딩의 **자유 생성자 가정**을 드러낸다

> FVP(finite variant property)를 이 프로젝트에 쓸 수 있는지 검토했다. 결론은
> **쓸 수 없고 쓸 이유도 없다**. 다만 검토 과정에서 FVP 후속 결과(variant
> satisfiability)가 요구하는 *자유 생성자* 전제가, 우리 이진 nat 인코딩이
> sort로 강제하지 못하고 손검증 불변식으로만 유지하는 지점과 정확히 겹친다는
> 걸 확인했다. 후자가 이 항목의 실질이다.

**FVP 자체는 무관하다 (기록만).** FVP는 등식 이론을 규칙부 E와 공리부 B로 나눴을 때
임의 항의 variant(치환 후 E,B-정규형과 그 치환의 쌍)가 유한한 대표 집합으로 커버되는
성질이고, 그 값어치는 E,B-단일화의 결정성과 narrowing 기반 기호적 도달성 분석이다
(Comon-Lundh–Delaune RTA 2005; Escobar–Sasse–Meseguer, folding variant narrowing 2012).
숫자 이론 중 FVP를 갖는 건 XOR(ACUN)·아벨 군처럼 **공리로 주어진** 것들이고,
**생성자 재귀로 정의된 산술은 갖지 않는다.** 우리 세 스칼라 표현 전부 후자다:

- **Structural 이진 nat** — `badd(bd0 p, bd0 q) = bd0(badd p q)` 꼴로 우변이 자기
  자신을 품으므로 `badd(x,y)`의 생성자 narrowing이 피연산자 자릿수를 따라 무한히
  뻗는다. 이진이라 Peano보다 O(log n)으로 싼 것이지, variant가 유한해지지는 않는다.
  sign-magnitude int도 같은 자리 위에 있다.
- **Native 스칼라** — Maude 내장 `Nat`/`Int`는 등식이 아니라 hook이라 variant/narrowing
  기계의 바깥이고, 애초에 실행 표면 전용이라 분석에 등장하지 않는다.

검증하려는 성질(합류성·종료성·충분완전성)이 variant 기법이 주는 것과 종류가 달라서,
FVP는 도입 후보가 아니다. 확인이 필요하면 `get variants badd(X, Y) .`가 끝나지 않는 걸
보면 된다.

**진짜 접점 — 생성자가 서명 수준에서 자유가 아니다.** FVP의 후속인 variant
satisfiability(Meseguer–Skeirik)는 *FVP + 자유 생성자*에서 초기 대수의 QF 충족가능성
결정 절차를 얻는다. 우리는 FVP 쪽에서 이미 탈락이라 이 정리를 쓸 일이 없지만, **자유
생성자 전제**는 우리가 이미 돌리는 SCC가 요구하는 것과 같은 종류이고, 거기서 이진
인코딩의 알려진 약점이 드러난다:

- [ctrs_term.ml](translate/ctrs_term.ml)의 주석대로, `Maude_sorts`가 심볼 이름당 서명
  하나만 지원해서 Coq의 zero-free `positive` **sort**를 따로 둘 수 없다. 실측:
  [maude_sorts.ml](maude/maude_sorts.ml)에 `bzero, bone : NatV`,
  `bd0, bd1 : NatV -> NatV` — 즉 **`bd0(bzero)`는 well-sorted**인데 0의 비정규 철자다.
- canonicity(`bd0`/`bd1`는 절대 `bzero`를 감싸지 않는다)는 타입이 아니라
  [prelude.ml](translate/prelude.ml)의 **모든 규칙을 손으로 검증한 불변식**으로만
  유지된다. 그리고 그 손검증의 전제는 명시적으로 **"canonical 피연산자가 주어지면"**이다.
- `bzero`/`bone`/`bd0`/`bd1`은 Structural 이론에서 `[ctor]`로 선언된다
  (`Maude_sorts.scalar_ctor_syms`), 즉 **SCC가 보는 생성자 항 집합에 비정규 항이 들어
  있다**. 따라서 SCC의 YES는 "모든 기저 항이 *어떤* 생성자 항으로 내려간다"까지이지
  "*canonical* 이진 nat으로 내려간다"가 아니다. 그 간극은 손검증 불변식이 메우고 있고,
  검사기가 확인해 주는 게 아니다.

**따라서 triage 규칙 하나:** 이진 nat 계열의 CRC MAYBE나 SCC witness가
`bd0(bzero)`/`bd1(bzero)` 모양 항을 물고 있으면 **spurious**다 — 불변식이 배제하는
입력인데 서명이 배제하지 못해서 검사기가 양화 범위에 넣은 것이다. 반대로 판정의
안전 방향은 유지된다(입력을 넓게 잡는 쪽이라 YES는 그대로 유효).
현재 기록된 witness에서 실제로 관찰된 적은 없다 —
`sweeps/witness/*.md`·`sweeps/*.md`에 `bd0`/`bd1` 0건.

**논의/확인 필요:**
- [ ] **canonicity를 검사기가 보게 할지.** 두 갈래다. (a) `NatV`의 zero-free 부분
  sort를 두려면 `Maude_sorts`의 "심볼당 서명 하나" 제약을 풀어야 하고, 그건
  서명 복구 레이어 전체를 건드린다. (b) 불변식을 등식이 아니라 **분석 표면의 문서화된
  가정**으로 명시하고 위 triage 규칙만 유지한다. 비용상 (b)가 기본값이고, (a)는 실제로
  비정규 witness가 관찰될 때만 검토할 값어치가 있다.
- [ ] **`[ctor]` 선언과 실제 canonical 상(像)의 불일치를 PAPER.md §6(threats)에 넣을지.**
  §2의 R3(생성자 판별 규율)이 SC의 전제로 선언돼 있는데, 이진 nat은 그 규율을
  sort가 아니라 손검증으로 만족시키는 유일한 계열이다. threats에 한 문장 값어치는 있다.

### 2026-09-05 — **`Itrs`에 리스트-길이 승격(norm lift): 145 열의 마지막 미증명 2행이 `itrs-lengths`로 닫힌다** (`itrs.ml` `norm_lift`)

바로 위 09-05 항목이 남긴 진단 — "SN은 이제 참인데 하강이 `drop(t, len(t_prefix))`
속이라 plain은 부분항으로 못 보고 itrs는 리스트 층이라 아예 못 본다" — 을 스펙이
아니라 **인코더** 쪽에서 푼다. 스펙 불변, 분석 전용.

**무엇을 하는가.** 리스트 정렬 위치를 전부 그 **길이**로 사상한다: `nil`↦0,
`cons(h,t)`↦`t+1`, `len`↦항등, `drop(x,k)`↦`x−k :|: 0≤k ∧ k≤x`,
`take`/`slice`/`cat`/`upd_idx`/`upd_slice`도 각각 `list_prelude`에서 하나씩 읽어 온
닫힌 식, `match_cons`/`match_nil`은 길이 비교(`lt`/`leq`)로 — 그래야 `lift_guards`가
그것을 `:|:` 제약으로 올린다. 나오는 것은 sort 하나가 접힌 같은 슬라이스이고,
하강은 이제 `p > 0`인 `t − p`, 즉 ITRS 솔버가 바로 읽는 산술이다.

**판정의 무게는 현행 itrs와 같은 종류이고 한 단 더 나갔다 — 모델이지 근사가
아니다.** 길이가 같은 두 리스트가 여기선 한 항이므로, 그 둘을 구분하던 규칙이
양쪽에서 발화한다(스텝을 **더하는** 방향, 안전한 쪽). 지우는 스텝은 닫힌 식으로
대치한 연산들의 것이고 그건 codec 대치와 같은 자리다. 그러므로 YES는 "리스트 층이
이 길이들이 말하는 대로 행동한다면 이 슬라이스는 종료한다"는 진술이다. 행에서는
경로 열의 `itrs-lengths:`로 구분한다(현행 `itrs:`와 같은 자리, 같은 성격).

**설계가 아니라 측정이 정한 것 넷.** 각각 한 변수씩, 이 순서로 필요했다.

1. **값 위치의 인코더-소유 호출은 닫힌 식으로 인라인해야 한다.** 09-05 프로브
   (`sweeps/itrs-probe/strip-norm/`, budget 300): 래퍼 하나면 YES(a·b·c),
   `dropL`만 인라인해도 YES(d2), `sw`만 인라인해도 YES(d1), **둘 다 래퍼면
   KILLED**(d). `$strip_prefix`는 builtin이라 본문이 `drop(t, len p)`인데,
   이름으로 잡지 않고 **모양**으로 잡는다 — 승격 뒤 정의가 무조건부·서로 다른
   변수 인자·순수 산술 우변이면 매크로로 보고 호출부에 인라인한 뒤 규칙을 버린다
   (`discover_macros`; 루트는 제외 — 방출물은 슬라이스가 말하는 심볼을 담아야
   한다). `inline_literal_divisors`와 같은 이유·같은 정확성이다.
2. **사례 분할은 occurrence마다가 아니라 부분항마다 한 번.** `sub`(monus)는 닫힌
   식이 두 가지라 이를 품은 규칙이 복제되는데, 같은 부분항이 여러 번 나올 때
   occurrence별로 따로 고르면 서로 모순인 사본이 나온다 — `$ends_with` 8사본
   (`x >= y`와 `x < y`를 동시에 요구하는 것들). 소스에서 같은 호출이니 같은
   가지를 골라야 정확하고, 그러면 2사본이다.
3. **닫힌 원자는 제약이 아니라 결정이다.** `0 >= 0`처럼 변수 없는 원자는
   `of_system`의 `tail`이 "자기 변수를 전부 나르는 규칙"에 붙이는데, 변수가
   없으면 **모든** 규칙에 붙는다(`not(true) -> false :|: 0 >= 0`). 참이면 버리고,
   거짓이면 그 규칙은 발화할 수 없으므로 규칙째 버린다.
4. **승격 뒤 스텝이 아니게 되는 조건 둘을 지운다.** (a) 순수 산술식에 신선 변수를
   묶는 바인더 — `rebind_stuck_guards`가 되살린 `crcv`가 바로 이것이고, 원래
   표면에선 호출을 강제하느라 스텝값을 하지만 승격 뒤엔 강제할 호출이 없다;
   (b) 양변이 같은 항이 된 매칭 조건(`t[0:|p|] = p` ↦ `|p| = |p|`). 둘 다 모든
   체인을 한 스텝씩 줄이고, 이게 `$strip_suffix_rec`을 뒤집은 마지막 변수다:
   체인을 남기면 **KILLED@300**, 둘 다 지우면 **YES@17.2s**. (`$starts_with`/
   `$ends_with`가 체인에서 제약 하나짜리 규칙으로 접히는 순간 호출자의 증명이
   그 가드를 읽는다.)

**고정점 sort 추론이 실제로 사는 곳은 하나 — 길이의 비음수성 복원.** 승격 자체는
심볼로 키가 잡히므로 sort 없이도 발화한다. 추론이 사 오는 것은 untyped 방출이
버리는 사실, 즉 길이는 `>= 0`이라는 것이고, 이는 `bis_zero`→`leq`가 b-layer에
해 준 것과 정확히 같은 종류다(`t`가 음수를 훑을 수 있으면 `p > 0`인 `t − p`는
영원히 내려가고 측도가 없다). 씨앗은 `nil`/`cons` 출현 + 프렐류드 리스트 연산의
리스트 슬롯이고, 슬라이스 자신의 등식을 따라 전파한다. listy·scalar 양쪽으로
표시된 클래스는 **오류가 아니라 미상**이다 — 제네릭 등식은 호출부마다 다른 sort를
나르고 승격 뒤엔 어느 쪽이든 정수이므로 — 그런 클래스엔 비음수를 붙이지 않는다
(정수가 아닌 값에 대한 제약은 어디서도 성립하지 않아 규칙을 조용히 지운다).

여기서 실측으로 하나 배웠다: **`isStuckHead`/`eq`/`eqg`는 인자 sort가 호출부마다
정해지는 제네릭 심볼**이라, 이들의 인자 슬롯을 다른 심볼처럼 통합하면 한 클래스가
모든 sort를 삼킨다. `$strip_suffix_rec`에서 `isStuckHead`가 한 규칙에선
`$strip_suffix`(리스트)에, 다른 규칙에선 `sub(...)`(nat)에 걸려 슬라이스 전체가
미상이 됐다(비음수 0개 방출). 이 셋만 occurrence별 슬롯을 준다.

**커버리지는 검사한다, 가정하지 않는다** (`unravel.ml`의 규율 그대로). 닫힌 식이
없는 리스트 연산 잔존(`idx`/`mem`/`*_aux` — 길이가 결정하지 못하는 **원소**를
읽는 것들), 패턴에서의 리스트 해체, 순수 산술로 렌더되지 않는 가드, 계산을 품은
원소의 폐기, `split_cap`(16) 초과 분할 — 각각 메시지와 함께 승격을 포기하고
호출자는 현행 itrs로 폴백한다(경로 열에 `itrs-lengths:ERROR: ...(export)`).
패턴 해체를 포기하는 건 보수가 아니라 분업이다: 꼬리가 전체 리스트를 진부분항으로
만나는 그 모양은 **plain 표면이 size-change로 이미 닫는다**.

**배선.** `Termination.surface`에 `Integer_lengths`, 경로에 `Length_trs`
(열 표기 `itrs-lengths`), CLI `--itrs-lengths`(단독 표면 지정/`--emit-trs`용).
`check`는 현행 itrs가 답하지 못한 뒤에만 이 경로를 시도한다 —
`--no-integer-retry`는 둘 다 끈다.

**측정 (AProVE, 직렬, `--out`으로 재개 가능, 08-22/09-01/09-04와 같은 ladder).**

| 심볼 | whole | children | itrs | **itrs-lengths** |
|---|---|---|---|---|
| `$strip_prefix_rec` | TIMEOUT@301.9s | TIMEOUT(0/1, node 31)@603.0s | TIMEOUT@302.8s | **YES@0.7s** |
| `$strip_suffix_rec` | TIMEOUT@301.4s | TIMEOUT(0/1, node 36)@602.2s | TIMEOUT@300.6s | **YES@16.8s** |

앞 세 경로는 09-04 열을 그대로 재현한다(같은 판정, 같은 미종결 자식). 뒤집힌 것은
새 경로 하나뿐이다. 슬라이스는 prefix 55등식/62규칙/u=7, suffix 91/99/u=8.

**검증 순서 (한 번에 한 변수).**

① **표면 불변**(주장이 아니라 대조): impty 골든 byte-identical이고 체크인된
`specs/impty/base/spec.ctrs`와도 일치, `specs/p4` 실행 모듈 sha `e94b580d…`
불변, 분석 CTRS sha `0e1129ec…` 불변, 그리고 **145 심볼 전부의 plain TRS와 현행
itrs 방출이 HEAD 바이너리의 것과 byte-identical**(양쪽 `578b0dd6…`). 마지막
항목이 중요한 이유: 이번 변경 중 승격 플래그로 감싸이지 **않은** 것이 하나 있다 —
`lift_guards`가 이미 `itrszN`인 변수를 다시 개명하지 않게 한 것(`is_constraint_var`).
개명하면 앞선 패스가 그 이름으로 건 제약이 고아가 되어 방출에서 조용히 사라지므로
이건 잠재 결함의 수정이지만, 그래도 바이트가 바뀔 수 있었다. 145행 대조가 그렇지
않음을 보인다.

② **strip 2행**: 위 표.

③ **기존 `itrs:`가 답한 32행이 흔들리지 않았는지**의 spot check(싼 것 하나, 비싼
것 하나): `$name_annotationToken` `itrs:YES@0.7s`, `$bin_shr` `itrs:YES@123.6s`
— 09-04 열의 `0.7s`/`80.1s`와 판정·경로·stats 동일. `$bin_shr`의 답한-rung
시간이 1.5배 늘었지만 같은 rung이고 그 슬라이스의 itrs 방출은 ①에서 byte-identical로
확인됐으므로 기계 변동이다(프로토콜의 "2배 & 5초 초과" 기준 아래). 32행이 구조적으로
안전한 이유는 따로 있다: 새 경로는 `itrs`가 **답하지 못한** 뒤에만 붙으므로 그
32행은 새 경로에 닿지도 않는다.

④ **145 전수 재측정 — YES 145/145, 열이 닫혔다** (`term_145_2026-09-05.tsv`).
`run_term145_0905.sh`, 08-22/09-01/09-04 프로토콜 그대로(같은 145 심볼, 같은
플래그, 두 단계), 직렬, 09-05 17:10 → 09-06 02:22 KST (9h12m). HEAD `838e4a01`
+ 미커밋 strip 가드 + 미커밋 승격, exe `abcff810…`.

| | 08-22 열 | 09-01 열 | 09-04 열 | **09-05 열** |
|---|---|---|---|---|
| YES | 135 | 143 | 143 | **145** |
| — whole | 111 | 111 | 111 | 111 |
| — itrs | 24 | 32 | 32 | 32 |
| — **itrs-lengths** | – | – | – | **2** |
| TIMEOUT | 10 | 2 | 2 | **0** |

**b1800 재질의는 이번엔 없다 — 2단계에 넘길 TIMEOUT 행이 0개다.** 09-04 열과의
전이는 정확히 예측대로 **두 행뿐**이다: `$strip_prefix_rec`·`$strip_suffix_rec`이
TIMEOUT→YES, 답한 경로 `-`→`itrs-lengths`. 나머지 143행은 **판정·경로·stats 모두
불변**이고, 2배&5초 초과로 느려진 행 **0개**. `itrs-lengths`가 경로 열에 등장한
행은 145행 중 그 **2행뿐**이고 `itrs-lengths:ERROR`는 0건 — 새 경로는 `itrs`가
답하지 못한 뒤에만 붙으므로, 다른 143행은 이 코드에 닿지도 않았다는 구조적 사실이
열에서 그대로 관측된다.

읽는 법: 09-04 열의 TIMEOUT 2는 "SN 참 — 측도가 도구 시야 밖"이었고, 이 열에서
그 시야가 리스트 층까지 넓어졌다. 다만 두 YES는 **모델에 대한 진술**이라
(경로 열의 `itrs-lengths:`) plain 표면의 111과 같은 무게가 아니다 — itrs가 답한
32행에 대해 09-02 항목이 단 단서와 같은 단서가 그대로 붙는다. 145 열은 이제
**plain 111 / 모델 34**로 읽어야 한다.

### 2026-09-05 — **strip 가드 수정 + 145 재측정: 의미론은 고쳐졌고(SN 참), 판정은 그대로다 — 2행이 "실루프" 패밀리에서 "측도 은닉" 패밀리로 이동** (`term_145_2026-09-04.tsv`)

아래 09-04 항목의 처분 (b)를 적용했다(0.0-stdlib.spectec: 두 `_rec`의 재귀
클로즈에 `-- if $(|t_prefix| > 0)` 산술 철자, 베이스 클로즈는 `~$starts_with`
가드 유지 시 `""`에서 부분함수가 되므로 `-- otherwise`로). 적용 검증: 슬라이스
59→55 / 98→91 (eq 폭발 없음, 가드는 `leq(len(t_prefix), bzero) = false` 체인
스텝), 빈-패턴 시작항이 스텝 캡 발산 → **13스텝 정규형**, 실행 모듈 방출 정상,
impty 골든 불변.

145 전수 재측정(08-22/09-01 프로토콜 그대로, tmux, 직렬, ~14.7h):

- **판정 145/145 동일** (YES 143 / TIMEOUT 2), 경로 플립 0, 2배&5초 초과
  감속 0 — 두 헬퍼는 호출부가 없으므로 다른 143행 불변은 예측 그대로.
- **strip 2행은 여전히 미증명**: prefix `whole:MAYBE@1203s`(b1800),
  suffix TIMEOUT@1801; children 0/1, itrs도 TIMEOUT@1800. 09-04 항목의
  경고 그대로 — SN은 이제 참이지만 하강이 `drop(t, len(t_prefix))` 속
  (정의 심볼이 재귀 인자에 앉는 그 패턴)이고, 리스트 층이라 itrs의 제약
  산술도 못 본다.

읽는 법: 이 수정의 가치는 판정 열이 아니라 **분류의 이동**이다. 09-01 열의
TIMEOUT 2는 "SN 거짓 — 어떤 예산에도 YES 불가"였고, 09-04 열의 TIMEOUT 2는
"SN 참 — 측도가 도구 시야 밖"(write_value·$shr의 arith-blind와 같은 결,
호출부 0인 죽은 헬퍼라 실해악도 없음)이다. YES까지 밀려면 하강을 구문화하는
재정식화(예: `t_prefix`를 cons 패턴으로 받아 구조 재귀로)나 삭제뿐 — 둘 다
사용자 결정 대기.

### 2026-09-04 — **`$strip_prefix_rec`/`$strip_suffix_rec` 분석: 빈-패턴 실루프를 현행 표면에서 기계 확인 — 루프는 실재하고, 도달은 불능(호출부 0)** (`witness/simulate_strip.py`)

145 열의 마지막 TIMEOUT 2행. 08-12 증인 ⑷의 순환이 오늘의 방출 TRS에서
그대로 성립하는지, 그리고 실행면·도달성까지 한 번에 확정했다.

- **기계 확인.** `--emit-trs` 슬라이스(prefix 62줄 / suffix 101줄)를
  innermost 시뮬레이터로 몰았다(`witness/simulate_strip.py` — 가드·스트립을
  슬라이스 자신의 규칙으로 정규화하며 최상위 재귀만 한 펼침씩 전진).
  `t = "x"`, 패턴 `nil`: `d_starts_with(t, nil) →* true`,
  `d_strip_prefix(t, nil) = drop(t, len(nil)) →* t` 항등 — **한 펼침에 상태
  복귀, 길이-1 순환** (prefix 25스텝 / suffix 57스텝). SN 거짓.
- **아티팩트가 아니라 스펙 의미론이다.** `$starts_with(t, "")`는 정의상
  true(`|""| ≤ |t|`, `t[0:0] = ""`)이고, 인터프리터의 builtin도 같다
  (`texts.ml`: `String.starts_with ~prefix:""` = true, 스트립 항등) —
  `$strip_prefix_rec(t, "")`는 **인터프리터에서도 발산**한다. write_value의
  `n_var = 0`, decode의 `w = 0`과 같은 0-경계 패밀리의 텍스트판.
- **도달성: 없음.** 두 `_rec` 헬퍼는 스펙 전체에 **호출부가 0**이다
  (0.0-stdlib의 정의뿐, `*.spectec` 전수 grep). 비-rec builtin
  `$strip_prefix/suffix`의 호출 4곳은 전부 리터럴 비빈 패턴(`"."`, `"\""`,
  `")"`, `"action_list("`). 루프는 실재하되 죽은 코드에 갇혀 있고, 어떤
  프로그램의 타이핑 경로도 닿지 못한다.
- **판정 읽기.** 그러므로 이 2행의 MAYBE/TIMEOUT은 **정직하다** — SN이
  거짓이라 어떤 예산에도 YES는 불가능하다. `bff805ec` 기준선의 YES 0.7s는
  `b1ca0af6` 이전의 stuck-guard 규칙 삭제가 만든 가짜였음이 이걸로 소급
  확정된다(`bisect_strip_prefix_rec.tsv`). AProVE가 NO까지 못 가는 건
  u-체인 + 비선형 `u_2` 매치를 뚫는 narrowing이 필요해서다(b1800에서
  prefix는 MAYBE@650s로 포기, suffix는 TIMEOUT).
- **처분 선택지 (사용자 결정 대기).**
  (a) **죽은 헬퍼 삭제** — 호출부 0이라 의미 손실이 없고, 145 열의 잔여
  TIMEOUT 2가 심볼째 사라진다. 최소·결정적.
  (b) **스펙 가드** — 재귀 클로즈에 `-- if $(|t_prefix| > 0)`(산술 철자,
  `=/=`의 generic-eq 폭발 회피) + 베이스 클로즈를 `-- otherwise`로
  (현행 `~$starts_with` 가드를 그대로 두면 `""`에서 두 클로즈 다 미발화 =
  부분함수). SN은 참이 되지만 하강이 `drop(t, len(t_prefix))` 속에 있어 —
  정의 심볼이 재귀 인자에 앉는, 잔여 8을 막던 바로 그 패턴 — plain 판정이
  YES로 돌아온다는 보장은 없다(고치면 측정 필요). 헬퍼를 살릴 계획이 있을
  때만 값어치가 있다.

### 2026-09-03 — **Program_ok 자식 분열 측정 (plain child-report): 열린 70 중 46 닫힘 / 24 미종결 — 미종결의 13개는 ITRS가 슬라이스 단위로 이미 닫는 바로 그 산술 사이클** (`program_ok_children_0902.tsv`)

`termination --symbol Program_ok --child-report --budget 3600 --child-budget
300` (plain 표면 — 이 계기는 `.qdp` 재질의 경로라 `inline_literal_divisors`와
무관), tmux 세션에서 직렬, 10:35–13:49 KST (캡처 ~77분 + 자식 70런 ~2시간).
드라이버 `sweeps/run_program_ok_children_0902.sh`; 비재개형(TSV truncate + 매번
새 캡처)이므로 tmux가 세션 teardown 유실(09-01/09-02에 두 번 겪은 그것)의 대책.

**3600초 전체 런이 열어 둔 자식 70개: 단독 300초에 46 YES / 24 TIMEOUT.**
keep-캐리어 시절의 75/48(2026-08-19 이전)과 같은 그림이다. 닫힌 46의 비용은
med 4.7s / p90 16.1s / max 204.8s이고 43개를 `QDPOrderProof`(reduction-pair
탐색 — 공유 런이 감당 못 하는 그 프로세서)가 닫았다 — "unreached-easy"는 이
규모에서 실재하고, 그걸 걷어내고 남는 잔여가 아래다.

**미종결 24의 패밀리 분해**:

| 패밀리 | 수 | 노드 (names) |
|---|---|---|
| **산술/b-layer 사이클 — ITRS가 슬라이스 단위로 닫는 바로 그것들** | 13 | `$int_to_text_nat` · `$bor_nat`/`$bor_nat_cross`/`$bxor_nat`/`$band_nat`/`$band_nat_cross` · `bpow_nat(+aux)` · `pow_int(+aux)` · `$int_to_bitstr` · `$bitstr_to_int` · `$fresh_typeIds` · `$shr(+aux)` · `$shr_arith(+aux)` |
| 타이핑 관계 대형 상호재귀 SCC | 7 | `CallableTypeDef_wf`± · `Cast_expl`± · `Argument_eval_lctk`± · `ArgumentList_ok/CallableTarget_ok`± · `BlockElementStmtList_ok`± · `InstDecl_objectInitializer_ok`± · `ParserStmtList_ok`± |
| 기타 | 4 | `$cast_binary`(17p/5459r — usable 자체가 무겁다) · `$repeat__value`(08-21의 그 자식: `.qdp`도 슬라이스도 TIMEOUT) · `$tableEntry_lpm_prefix'`(08-31 처분표의 미결 항목 — 여기 실재) · `$cast_enum` 캐스트 패밀리 |

읽는 법 두 가지. (a) plain 표면의 Program_ok 잔여는 절반 이상이 modular-(B)
arith-blindness다 — 13개 전부, 어제 145 열에서 `itrs:`가 답한 행들의 막던
사이클과 같은 이름들이다(`bor_nat` 등은 잔여 8의 그 사이클). 단, **표면이
달라 합성되지는 않는다**: itrs YES는 모델에 대한 진술이라 Program_ok의 plain
AND에 그 자식의 닫힘으로 넣을 수 없다. (b) 산술을 걷어낸 나머지(7+4)가
Program_ok 종료 증명의 진짜 미해결 — 대형 상호재귀 SCC와 무거운 캐스트
패밀리다. rc=1은 미종결 자식이 남았다는 CLI의 정상 신호.

### 2026-09-02 — **145 전수 재측정: term 열 YES 143 / TIMEOUT 2 — 인라인이 잔여 8을 정확히, 그리고 그것만 뒤집었다** (`term_145_2026-09-01.tsv`)

아래 2026-09-01 항목의 인코더 변경을 반영한 열 전체 재측정.
`sweeps/run_term145_0901.sh` — **08-22 프로토콜 그대로**(같은 145 심볼, 같은
플래그, 두 단계: 기본 ladder cap 300 → TIMEOUT 행만 b1800), 커밋된 08-22
파일들은 보존하고 새 날짜 파일로. 두 열의 델타가 인코더 변경(`bis_zero`→`leq`
`9cd816c9` + `inline_literal_divisors` 미커밋)만을 재도록 했다. 직렬; 트리
스탬프는 status 로그에(HEAD `3e4fdb1f` + 미커밋 인라인). 09-01 15:48 시작,
도중 세션 종료로 한 번 중단됐다가(126/145에서, resumable로 손실 없이 재개)
09-02 17:45 KST 완료.

**결과** (`term_145_2026-09-01.tsv`, b1800은 `_b1800.tsv`):

| | 08-22 열 | 09-01 열 |
|---|---|---|
| YES | 135 (whole 111 · itrs 24 · children 0) | **143** (whole 111 · itrs **32** · children 0) |
| TIMEOUT | 10 | **2** |

전이는 정확히 셋뿐이다: **YES→YES 135** (답한 경로 플립 0, 2배·5초 초과
느려진 행 0 — 인라인이 기존 증명을 하나도 건드리지 않았다), **TIMEOUT→YES 8**
(= 잔여 8 그대로: `$bin_band/bor/bxor/shr` `$bitacc_offset/range_op`
`$name_annotationToken/_opt`), **TIMEOUT→TIMEOUT 2** (`$strip_prefix_rec`
`$strip_suffix_rec` — b1ca0af6이 되살린 재귀의 그 둘. b1800 재질의도 08-31과
같은 그림: prefix `whole:MAYBE@649.9s`(08-31 MAYBE@675.5s의 재현) /
suffix TIMEOUT, 양쪽 다 자식 `d_strip_*_rec` 미종결, itrs도 TIMEOUT).
children 경로는 이번에도 145행에서 YES 0 — 08-22의 관찰 그대로다.

itrs가 답한 32행의 YES는 여전히 **모델에 대한 진술**이다(경로 열에 `itrs:`로
표시). 남는 후속 둘, 둘 다 열 갱신이 아니라 별도 결정: (i) itrs가 구한 32행에
plain@1800을 물은 적 없다는 08-22의 측정 구멍은 24행 시절 `term_24_plain`이
전부 TIMEOUT으로 닫았고, 새로 늘어난 8행에 같은 질문을 할지. (ii)
`verification.md`의 650심볼 열은 여전히 145만 다시 잰 상태라는 것.

### 2026-09-01 — **ITRS 자식 분할을 붙이려다 진짜 범인을 찾았다: 리터럴 제수의 `div`/`mod` 래퍼 — 인라인 한 패스로 잔여 8행 전부 YES** (`itrs.ml` `inline_literal_divisors`)

자식 분할 설계에 앞서 "AProVE가 독립 입력으로 무엇을 받는지"를 실물로 확인하는
것부터 시작했고(아래 ①), 그 확인이 진행되면서 잔여 8행의 전제 자체가
무너졌다(②). 결국 붙인 것은 자식 분할이 아니라 인코더의 한 패스다(③).

**① 확인된 사실 — 자식 분할을 되살릴 때 그대로 쓸 것.**

- ITRS 런의 `(6) AND` 자식은 IDP 문제가 맞다. AProVE에는 **`.idp` 단독 입력
  형식이 존재한다** (`aprove/Framework/Input/extension.properties`:
  `idp = ...newIDP.Translator`; 문법은 `(VAR)(PREDEFINED)(RULES)(TERMS)(EDGES)
  (INITIALNODES)(STRATEGY)(MINIMAL)` — pair graph 전체를 요구하는 무거운 형식).
- 실용적 재질의는 **pairs-as-rules `.itrs`**다: obligation echo의
  `(n): PAIR -> PAIR` 줄을 변수만 개명해(`name[pairidx]` — pair 번호가 붙는다)
  규칙으로 다시 내면, `ITRStoIDPProof`가 `:|:` 제약을 `Cond_*` 항 인코딩으로
  옮겨 놓았기 때문에 제약이 그대로 보존된다. 장난감 2-SCC로 파스·판정 검증(YES).
- **usable 축소가 여기서는 최적화가 아니라 정합성 조건이다.** `.qdp`는 `(PAIRS)`
  섹션이 P를 고정하지만 `.itrs`에는 P/R 구분이 없어서, 전체 R을 동반하면 R의
  모든 정의 심볼이 DP를 다시 만들어 **전체 문제의 재제출**이 된다 — pow 자식:
  전체 R 동반 KILLED@60, usable 축소 후 **YES 0초**. 자식은 innermost(echo의
  Q가 전체 lhs를 덮는다)라 Cε 없이 [FROCOS05] 면허로 충분하다.
- 재질의 단위는 deepest open leaf가 아니라 **AND 직계 자식**이다: `$bin_bxor`
  트리에서 한 자식(`(7) IDP`)에 `IDPtoQDPProof`가 **두 갈래**(노드 10, 338)로
  달려, leaf 단위로는 같은 자식을 두 번 묻게 된다.

**② 전제 붕괴 — 막는 자식은 unreached가 아니라 hard였다.** 2026-08-31 항목은
"두 사이클 다 개별로는 1초 미만인데 전체는 자식에 도달하지 못한다"고 읽었지만,
**충실한 방출형으로 재질의한 막는 자식은 단독 300초를 줘도 KILLED다**
(`d_bxor_nat` 자식 KILLED@300; `$name_annotationToken`의 `d_int_to_text_nat`
자식도 KILLED@300 — 후자는 전체 런이 60+노드를 진행한, 도달됐던 자식이다).
0.7초였던 "사이클 단독" 프로브는 연산을 **인라인으로** 쓴 컴팩트형이었고,
결정 변수는 사이클도 u-체인도 아니라 표기였다. 한 변수 격리(@60, 직렬):

| 프로브 (bxor_nat 사이클) | 판정 |
|---|---|
| 충실한 방출형 (u-체인 + `div`/`mod` 래퍼) | KILLED@300 |
| 체인 접음 + **래퍼 유지** | KILLED@60 |
| 체인 접음 + **인라인 `/` `%`** | **YES 1초** |
| `d_int_to_text_nat` 자식, `z / 10` 인라인 | **YES 1초** |

기제: dependency pair의 **인자**에 들어간 `div(l, 2)`는 정의 심볼이라 pair
graph의 간선 조건이 산술이 아닌 도달성(`div(l,2) ->^* l'`)이 되고, `IDPNonInf`가
불발하면 AProVE는 `IDPtoQDPProof [SOUND]` → transformation 체인으로 빠져 어떤
예산이든 태운다. `integer_prelude`의 `div(x,y) -> x / y :|: y > 0` 주석이 근거로
든 1초 프로브(`bits(l,r) -> … 2*bits(l/2, r/2)`)는 인라인 표기였다 — 얇은
래퍼 규칙이 사실을 보이게 해 준다는 그 문단의 암묵적 주장이 pair-인자 위치에서
반증된 것. pow 단일-진입에 대한 2026-08-31의 경고("산술 제약이 도달성 조건으로
바뀐다")와 같은 기제다.

**③ 수정 — `Itrs.inline_literal_divisors`.** 제수가 **양의 정수 리터럴**인
`div`/`mod` 호출을 값 위치에서 네이티브 infix로 인라인한다(`div(l, 2)` →
`l / 2`; 프린터 쪽은 `Unravel.print_rules`의 기존 `#` 리터럴 escape를 이항
infix로 확장 — `#/`가 `a / b`로 인쇄, 중첩 infix 피연산자는 괄호). **정확
변환이다**: 가드 `y > 0`이 리터럴로 방출 시점에 결정되므로 래퍼 규칙이 모든
인스턴스에서 밟았을 한 스텝을 미리 밟는 것뿐이고, 0·음수 리터럴은 호출을
남겨(0-나눗셈의 의도된 stuck 보존) 변수 제수는 래퍼 규칙이 그대로 맡는다.
도달 불가가 된 `div` prelude 규칙은 `reachable_prelude`가 자동으로 떨군다.

**검증** (전부 이 트리, 직렬, KST 12:40–12:50):

- 실행면 sha256 `175bf048…` **불변**, impty 골든 byte-identical (분석 전용).
- 회귀 6/6 — 기준선(08-31)과 일치: `$bin_ge` 0.7s, `$bin_shl` 1.3s,
  `$int_of_integerValue` 0.7s, `$un_minus` 0.7s,
  `$set_priorities_of_tableEntryListIR` 24.0s, `$write_value_from_bits` 27.5s.
- **잔여 8행 전부 `whole:YES`** (`--itrs --budget 300`): `$bin_band` 1.6s ·
  `$bin_bor` 1.4s · `$bin_bxor` 1.1s · `$bin_shr` 161.6s ·
  `$bitacc_offset_op` 63.0s · `$bitacc_range_op` 59.7s ·
  `$name_annotationToken` 0.9s · `$name_annotation_opt` 1.1s.

원자료 주의: 프로브 산출물(`bxor300.out` 전체 echo, `child7*`/`child16*` 격리
프로브, `*_inline.itrs`)은 세션 스크래치라 사라진다 — **본문 수치가 기록의
전부다.** ITRS 열이 있는 `sweeps/itrs*.tsv`·`term_145*`는 이 변경으로 재측정
대상(기존 규칙대로 파일을 옮기고 새로 돌 것).

**처분: "ITRS 표면의 자식 분할" 표적은 은퇴한다.** 8행 모두 wrapper-hard 자식을
최소 하나 갖고 있어 분할만으로는 판정이 안 바뀌고(분할이 구했을 unreached-easy
자식도 실재한다 — bxor의 pow 자식은 단독 0초인데 전체 런에선 미도달 — 그러나
그걸 닫아도 wrapper-hard 자식이 남는다), 인라인 후에는 8행이 whole로 닫혀 분할할
대상이 없다. 되살릴 조건: 대형 심볼의 ITRS 런에서 자식 수 자체가 마감을 넘기는
사례가 실측되면, 위 ①의 확인 사실에서 시작할 것.

**교훈.** (a) "사이클 단독 0.7초"처럼 프로브로 잰 수치는 **프로브의 표기**에 대한
진술이다 — 방출형과 표기가 다르면 같은 사이클이 아니며, 이번엔 그 차이가 전부였다.
(b) 도구가 실제로 받는 것을 확인하는 5분이 이번에도 설계 전체를 뒤집었다(MTT
교훈의 재현 — 이번엔 순방향으로). (c) 재질의 형식마다 prune의 의미가 다르다:
`.qdp`는 P가 고정이라 coarse seed가 싸지만, `.itrs`는 실은 규칙이 곧 P라
coarse가 SCC를 복제한다.

### 2026-08-31 — **"회귀 34개"는 회귀가 아니었다. 기준선의 34행이 애초에 증명이 아니었다** (`bae6e096`)

`sweeps/term_24_plain_2026-08-22.tsv`가 24/24 TIMEOUT@1800으로 끝나서, 기준선
`bff805ec`가 plain으로 증명하던 34개를 지금은 못 한다는 그림이 나왔습니다. 원인을
`$bin_ge` 하나로 이분탐색(`sweeps/bisect_symbol.sh`, 66커밋)한 결과:

```
FIRST BAD: bae6e096  fix(rewrite): read AProVE's verdict from the header, not the proof echo
```

**범인은 회귀를 넣은 커밋이 아니라, 거짓 YES를 내던 파서를 고친 커밋입니다.** 그전에는
`verdict_line`이 버퍼 전체에서 첫 `YES`/`NO`/`MAYBE` 줄을 찾았는데, `KILLED`(AProVE가
자기 마감을 알리는 표시)는 그 토큰이 아니어서 스캔이 그걸 지나쳐 echo로 들어가
**하위 DP 문제 하나의 `YES`를 런 전체의 답으로 기록**했습니다. 즉 AProVE가 포기한 런이
종료 증명으로 남았습니다.

기준선(2026-07-23)은 그 수정(2026-08-05)보다 2주 앞섭니다. 지문이 정확히 맞습니다:

| 기준선 | 지금 | 수 |
|---|---|---|
| 마감 전 YES | plain YES | **109** |
| **마감(≥300초)에서 YES** | 모델(itrs)로만 | **24** |
| **마감에서 YES** | TIMEOUT | **8** |
| 마감에서 YES | plain YES | 2 |
| 마감 전 YES | TIMEOUT | **2** |

기준선 145행 중 **마감에서 YES를 찍은 게 정확히 34개**이고, 우리가 "후퇴"라 부른 것도
정확히 34개이며, 그 34개 중 32개가 겹칩니다. 마감에 답한다는 것은 진짜 증명의 모습이
아니라 **`KILLED`를 YES로 오독한 흔적**입니다.

**따라서 열은 나빠진 게 아니라 정직해졌습니다.** `CLAUDE.md`의 앵커 표에서
`bff805ec`의 term 열은 **철회**했습니다(`term YES 153/153` → 실측 111 + 미증명 34).

`RESULTS-2026-08-07.md` §5는 이 34개를 보고 *"마감에서 답한 것들이라 증명 가능성의
경계에 걸쳐 있었다"* 고 적고 판정을 보류했는데, 원인을 AProVE의 마감 통보 습성으로
돌렸습니다 — 파서 결함이 그 **하루 전**에 이미 고쳐져 있었는데도요. 같은 저장소가 이미
찾아 고친 결함을, 그 결함이 만든 데이터를 해석할 때 떠올리지 못한 것입니다.
**교훈: 도구를 고쳤으면 그 도구가 만든 과거 데이터를 전부 다시 의심하라.**

### 남은 2개도 회귀가 아니었다 — 두 번째 범인도 fix 커밋이다 (`b1ca0af6`)

```
$strip_prefix_rec   기준선 YES 0.5초 → 지금 TIMEOUT (b1800에선 MAYBE@675.5s)
$strip_suffix_rec   기준선 YES 0.6초 → 지금 TIMEOUT
```

0.5초는 마감 근처가 아니라 파서 아티팩트일 수 없어서, 이 둘만 진짜 회귀 후보로
남았습니다. `$strip_prefix_rec`을 예산 60으로 이분탐색한 결과:

```
FIRST BAD: b1ca0af6  fix(rewrite): prove termination of the rules the isStuckHead guard hid
```

그 커밋은 `Crc_surface.fold_premise_binders`가 죽은 바인더를 `isStuckHead($g(A)) =
false`로 바꿔 놓은 것을 되돌립니다(`rebind_stuck_guards`). 분석 표면은 `isStuckHead`를
정의하지 않으므로, 언레이블링하면 그 가드는 **절대 발화할 수 없는 소비자**
`u(false, ..) -> r`가 되고 그 규칙의 rhs와 거기 실린 재귀가 TRS에서 통째로
사라집니다 — `specs/p4`에서 조건 규칙 2,510개 중 **1,044개(41.6%)**.

즉 기준선의 `$strip_prefix_rec` YES 0.5초는 **그 재귀가 빠진 시스템**의 종료를
증명한 것이었습니다. 규칙을 되살리자 증명이 안 되는 게 정상입니다.

### 결론: `bff805ec` term 열에 진짜 회귀는 **0개**

34개 전부가 측정 쪽 결함 둘의 산물이고, 둘 다 이미 고쳐져 있었습니다:

| 원인 | 수 | 고친 커밋 |
|---|---|---|
| `KILLED` 런의 echo에서 하위 증명의 `YES`를 읽음 | 32 | `bae6e096` (2026-08-05) |
| `isStuckHead` 가드가 조건 규칙 41.6%를 TRS에서 삭제 | 2 | `b1ca0af6` (2026-08-09) |

**열은 나빠진 적이 없습니다. 측정이 정직해졌을 뿐입니다.** 그리고 잔여 10개는
"닫히던 게 안 닫히는 것"이 아니라 **한 번도 증명된 적이 없는 것**입니다 — 그중 8개는
파서 결함으로 YES처럼 보였고, 2개는 재귀가 빠진 시스템에서 YES처럼 보였습니다.

### 부산물 — bisect를 한 번 잘못 돌렸다

첫 실행은 7개 probe가 전부 `ERROR: aprove/runme not found`였는데도 그걸 bad로 세어
창의 첫 커밋을 범인으로 지목했습니다. 직접 원인은 `spectec/tools/`가 창 안 커밋들에서
**추적되는 실제 디렉터리**라 `ln -sfn`이 그 안에 링크를 만든 것이고, 진짜 원인은
**끝점을 검증하지 않고 탐색을 돌린 것**입니다. 지금은 스크립트가 `probe(GOOD)=YES`와
`probe(BAD)≠YES`를 먼저 확인하고, 아니면 abort합니다. 무효한 실행은
`sweeps/logs/bisect_bin_ge.INVALID.*`.

### 2026-08-31 — **정렬 손실을 고쳤다: `bis_zero`는 `eq`가 아니라 `leq`로 간다** (`itrs.ml`)

위 항목이 표적으로 짚은 둘 중 첫째. 실질 변경은 한 줄이다:

```diff
- ("bis_zero", fun a -> app "eq"  (a @ [ literal 0 ]));
+ ("bis_zero", fun a -> app "leq" (a @ [ literal 0 ]));
```

`bis_zero`는 **b-layer**, 즉 이진 nat의 영 판정이라 인자가 magnitude이고 CTRS가 nat로
정렬한다. 일반 `eq`로 보내면 `equality_relation`이 `bis_zero(a) = false`를 제약
`a != 0`으로 올리는데, 그건 `a`가 음수여도 참이라 `div(a, 2)`가 기저를 향해 줄지
않는다 — 비트 분해 재귀의 측도가 사라진다. `leq`로 보내면 `comparison_relation`이
이미 아는 대로 `a > 0`이 되고, 이건 정렬이 처음부터 말하던 것이다.

값은 위 항목의 단독 측정 그대로다: 같은 사이클이 `!= 0`이면 KILLED, `> 0`이면
**YES 0.7초**. 부수적으로 내보내기에서 `!=`가 사라져 AProVE `IDPNonInf`의 알려진
예외도 피한다.

**표면 영향은 정수 표면 하나뿐이다.** `Itrs`를 쓰는 곳은 `termination.ml`의
`Integer` 하나(`grep`으로 확인)이고, impty 골든 byte-identical, 실행 표면 sha256
`175bf048…` 불변.

**회귀 검사 6/6 통과** (`--itrs --budget 300`, 직렬):

| 심볼 | 기준(08-22) | 수정 후 |
|---|---|---|
| `$bin_ge` | YES 0.7s | YES 0.7s |
| `$bin_shl` | YES 1.3s | YES 1.4s |
| `$int_of_integerValue` | YES 0.7s | YES 0.7s |
| `$un_minus` | YES 0.7s | YES 0.7s |
| `$set_priorities_of_tableEntryListIR` | YES 23.8s | YES 24.0s |
| `$write_value_from_bits` | YES 26.6s | YES 27.1s |

**이 수정만으로 뒤집히는 행은 없다** — 전체 슬라이스는 여전히 자식에 도달하지
못한다(위 항목). 정보를 되돌린 것이지 경로를 연 것이 아니다. 남은 표적은 **ITRS
표면의 자식 분할** 하나다.

### 2026-08-31 — **헬퍼 방침은 필요 없었다: 잔여를 막는 건 사이클이 아니라 ITRS 표면에 자식 분할이 없다는 것**

> **표적 정정 (2026-09-01, 위 항목).** 이 항목이 남긴 표적 둘 중 ①(정렬 복구,
> `bis_zero`→`leq`)은 유효했지만 ②(자식 분할)는 표적이 아니었다: 막는 자식은
> 도달하지 못한 게 아니라 **재질의해 단독 예산을 줘도 안 닫히는** 문제였고, 아래
> "사이클 단독 YES 0.7초" 표는 인라인 표기 프로브에 대한 진술이라 방출형에는
> 성립하지 않는다. 실제 범인은 리터럴 제수의 `div`/`mod` 래퍼이고,
> `inline_literal_divisors`로 잔여 8행 전부가 whole로 닫혔다 — 전말은 위 항목.

§8이 "builtin은 알고 있다고 처리"를 미결로 남겨 둔 헬퍼 넷이 잔여 8행을 막고 있다고
읽었는데, **표적을 잘못 짚고 있었다.** `$bin_bor`의 ITRS 표면을 한 변수씩 세 번
밀어 본 결과다 (전부 AProVE @300, 직렬).

| 변형 | 판정 | CPU | 트리 노드 |
|---|---|---|---|
| 원본 | KILLED | 115분 48초 | — |
| ① `!= 0` → `> 0` (정렬 복구) | KILLED | 31분 17초 | 2,264 |
| ①+② `bor_nat` 단일 진입 | KILLED | 24분 50초 | 358 |
| ①+②+`pow` 단일 진입 | KILLED | 24분 18초 | — |

탐색량은 CPU 4.8배·트리 6.3배 줄었지만 **판정은 한 번도 안 바뀌었다.** 그래서 트리가
마지막까지 열어 둔 사이클을 읽고 그것만 떼어 단독으로 물었다 — 이 저장소가
`Program_ok`에서 이미 배운 "어렵다 vs 차례가 안 왔다"를 가르는 질문이다:

| 사이클 **단독** | 판정 |
|---|---|
| `pow(x,y) -> x * pow(x, y-1) :|: y > 0` (현재 방출 그대로) | **YES 0.7초** |
| `bor_nat` 비트 분해, `!= 0` (현재 방출) | **KILLED** |
| `bor_nat` 비트 분해, `> 0` | **YES 0.7초** |

**두 사이클 다 개별로는 1초 미만인데 둘을 고쳐 넣은 전체 슬라이스는 300초에 자식
3개 중 0개를 닫는다.** 어려운 게 아니라 도달하지 못하는 것이다. 막고 있는 건 헬퍼의
정의가 아니라 **ITRS 표면에 자식 분할이 없다는 사실**이다(2026-08-21 정정이 "자식
분할과 ITRS가 조합되지 않는다"로 적어 둔 그것).

**그래서 §8의 헬퍼 방침 결정은 필요 없다.** 공리로 선언할 것도, 인코더에 정의를 새로
쓸 것도 없다. 남은 표적은 둘이고 둘 다 방침이 아니라 코드다:

1. **정렬 손실 (①)** — CTRS는 `bor_nat : nat × nat`인데 내보내진 제약은 `!= 0`이다
   (`bis_zero` 가드만 옮겨왔다). 위 표가 그 대가다: 같은 사이클이 `!= 0`이면 KILLED,
   `> 0`이면 0.7초. 공리가 아니라 **버리고 있는 정보의 복구**다.
2. **ITRS 표면의 자식 분할** — plain 표면에는 있고 정수 표면에는 없다.

**②(단일 진입)에 대한 단서 하나 — 정수 표면에서는 오히려 해가 될 수 있다.** `pow`를
태그 디스패치로 바꾸자 DP 그래프의 간선 조건이 `y = 0`/`y > 0`이라는 **산술 제약**에서
`poszero(y) ->* false`라는 **정의된 심볼을 통과하는 도달성 조건**으로 바뀌었다.
2026-08-13의 태그 디스패치 idiom은 *구조적* 분기에 맞는 것이고, 정수 표면의 산술
가드에 그대로 적용하면 솔버가 보던 것을 가린다. 코덱이 단일 진입을 쓰는 것은 옳지만
`integer_prelude`까지 밀 이유는 없다 — `pow`는 지금 형태로 0.7초에 닫힌다.

(참고: `itrs.ml`의 `integer_prelude`에는 같은 lhs + 재귀가 `pow`·`pow_int`·`idx`·
`upd_idx` 넷 있다. 위 결과로 보면 **손댈 이유가 없다.**)

### 2026-08-31 — **잔여 정리 항목 넷의 처분**

| 항목 | 처분 |
|---|---|
| `--itrs` 84행이 루트 삭제 버그로 무효 | **재측정 안 함, 주장 철회.** 원자료가 없고, 커밋된 표 중 그 행에 기대는 게 없다 (`verification.md`엔 ITRS 열 자체가 없다) |
| `aprove.ml`이 `KILLED`를 TIMEOUT으로 안 보낸다 | **이미 되어 있다 — 항목이 stale.** `hit_deadline`이 헤더에서 `KILLED`를 읽어 `Timeout`으로 보낸다(`aprove.ml`). 남은 증상은 커널 OOM으로 JVM이 아무것도 못 찍는 경우고, 거기선 `ERROR`가 정직한 판정이다 |
| builtin 내부 헬퍼 6개 방침 미결 | **방침 결정이 필요 없다 — 표적이 틀렸다.** 위 항목 참조: 막는 건 헬퍼가 아니라 ITRS 표면에 자식 분할이 없다는 것 |
| `$tableEntry_lpm_prefix'` | **열려 있음.** 145 집합에 없어서 현재 열에 아예 들어 있지 않다. 스윕 집합에 넣을지가 먼저 |

**헬퍼 넷(`$bor_nat` `$band_nat_cross` `$bxor_nat` `$int_to_text_nat`)의 전제가
실측으로 바뀌었다** — 그리고 이어진 측정이 방침 질문 자체를 없앴다(위 항목). 아래
사실 셋은 그 과정의 앞부분이다:

⑴ **실행 표면에 그 재귀가 없다.** 실행 모듈(29,006줄)에서 `$bor`은
`eq $bor(int(I), int(J)) = int(I | J)` 한 줄이고 `bor-nat` 등장 횟수는 **0**이다.
`$band`/`$bxor`/`$int_to_text`/`$bitacc`도 같다. 비트 분해 재귀는 **분석 표면에만
있는 코드**다. 2026-08-13의 "builtin.ml은 건드리지 않는다" 방침이 근거로 삼은 사실
(`bitstr_to_int`은 `native_replaced_heads`에 없어 실행 모듈에도 구조 규칙이 나간다)이
여기엔 **적용되지 않는다**.

⑵ **ITRS 표면이 nat 정렬을 잃는다.** CTRS는 `bor_nat : nat × nat`인데 내보내진
제약은 `!= 0`이다(`bis_zero` 가드만 옮겨온 것). `l`이 음수면 `div(l,2)`가 기저 `0`을
향해 줄지 않으니 **하강이 보이지 않는다.** 바로 아래 `d_bor` 규칙들은 `>= 0`/`< 0`을
제대로 달고 있다 — 호출자는 아는데 피호출자가 모른다.

⑶ **정렬 복구는 필요조건이지만 충분조건이 아니다.** `!= 0` → `> 0` **한 변수만**
바꿔 AProVE @300:

| | 판정 | CPU |
|---|---|---|
| 원본 (`!= 0`) | KILLED | 115분 48초 |
| 정렬 복구 (`> 0`) | KILLED | **31분 17초** |

3.7배가 줄지만 안 닫힌다. 그러니 정렬 손실은 **고쳐야 할 결함이되 이 8행의 마지막
장애물은 아니다**. 남은 후보는 2026-08-13이 decode에서 이미 분리한 것과 같은 모양 —
`d_bor_nat`의 진입 규칙이 셋이고 셋째 lhs가 앞의 둘을 포섭한다("같은 lhs 둘이면
KILLED, 머리만 갈라도 YES 4초").

### 2026-08-22 — **term 열 재측정 (145행): YES 135인데 그중 24개가 모델 수준이고, children 경로는 0개를 닫았다**

`sweeps/run_term145.sh`, `bff805ec` 기준선의 심볼 집합에서 모노모피즘으로 특수화된
8개(`$dom_map` `$empty_map` `$exists` `$filter` `$forall` `$is_some` `$ite`
`$opt_as_seq`)를 뺀 **145개**를 현재 표면에서 직렬로. `--reslice-children`은 끔.
원자료 `sweeps/term_145_2026-08-22.tsv`.

| | |
|---|---|
| 판정 | YES **135** / TIMEOUT **10** |
| 답한 경로 | whole **111** · children **0** · itrs **24** |
| 기준선(153행 전부 YES) 대비 | YES→YES 135, YES→TIMEOUT 10 |

**"YES 135"는 두 종류의 진술을 합산한 수입니다.** 기준선은 전부 **plain 표면**
YES였는데, 이번에 plain으로 답한 건 111개뿐이고 24개는 `itrs:`가 구했습니다 —
그리고 ITRS의 YES는 슬라이스가 아니라 **그 모델**에 대한 진술입니다(2026-08-21에
문서를 정정한 그 이유: `Itrs.of_system`은 산술 층 규칙을 지우고 손으로 쓴 정수 정의를
대입하므로 과대근사가 아님). plain만 놓고 세면 145 중 111입니다.

그런데 예산도 다릅니다 — 기준선은 일부 심볼에 1800초를 썼고 이번 plain cap은
300초입니다. **재질의 pass는 verdict가 TIMEOUT인 10개만 다시 묻기 때문에, itrs가 구한
24개는 plain 1800초를 한 번도 안 물어봤습니다.** 이게 이번 측정 설계의 구멍이라
`sweeps/run_term24_plain.sh`가 그 24개를 `--no-integer-retry --no-child-split
--budget 1800`으로 다시 묻습니다(`term_24_plain_2026-08-22.tsv`). YES면 그 행은 예산
아티팩트였고 plain 열은 온전한 것이고, TIMEOUT이면 정말 모델로만 되는 것이라 열이
그렇게 말해야 합니다.

**children 경로가 145개 전체에서 YES를 0개 냈습니다** — 심볼당 ~600초를 쓰면서요.
자식 분할의 이득(`Program_ok` 577개 중 550개)은 이 집합에 없는 훨씬 큰 심볼에서
측정된 것이고, 이 규모대에서는 비용만입니다. 스윕에서 켜 둘 이유가 없습니다.

**진짜 plain 단축 하나**: `$write_bits_from_value` 기준선 1320.4초 → **whole:YES
0.8초** (양쪽 다 plain — flat 캐리어 전환의 효과로 보임). 반면 `$bin_minus` 1800.5초
→ 0.7초는 **같은 표면이 아닙니다**: whole은 여전히 TIMEOUT@301.3s이고 0.7초는 itrs.
표면을 안 보고 초만 비교하면 정확히 이런 착시가 납니다.

**잔여 10개는 예산 연성이 아니고, §8의 미결 결정 위에 그대로 앉아 있습니다.**
`--budget 1800` 재질의에서 (측정된 5/5 모두) 다시 TIMEOUT. 막는 자식이 4개 패밀리뿐:

| 막는 자식 | 심볼 |
|---|---|
| `d_band_nat_cross` · `d_bor_nat_cross` · `d_bxor_nat` | `$bin_band` `$bin_bor` `$bin_bxor` `$bitacc_offset_op` `$bitacc_range_op` |
| `bpow_nat, bpow_nat_aux` | `$bin_shr` |
| `d_int_to_text_nat` | `$name_annotationToken` `$name_annotation_opt` |
| `d_strip_prefix_rec` / `d_strip_suffix_rec` | 각 1개 |

앞의 넷(`$band_nat_cross` `$bor_nat_cross` `$bxor_nat` `$int_to_text_nat`)은 §8이
"builtin은 알고 있다고 처리" **방침 미결**로 적어둔 헬퍼 6개 중 4개입니다. 즉 이
열의 잔여는 새 결함이 아니라 **아직 안 내린 결정**입니다.

**두 재질의 모두 끝났습니다** (2026-08-31): b1800 10/10 — 9 TIMEOUT +
`$strip_prefix_rec` MAYBE@675.5s; plain@1800 24/24 TIMEOUT. `verification.md`의
termination 열은 아직 갱신하지 않았습니다 — 그 열은 **650심볼**이고 여기서 다시 잰
건 145개뿐이라, 행을 부분으로 덮어쓰면 서로 다른 표면의 숫자가 한 표에 섞입니다.

### 2026-08-21 (후속) — **열린 자식을 "슬라이스"로 재질문하는 평면 경로**, 그리고 그 건전성이 Cε를 요구했다는 것

> **이 경로는 2026-08-31에 코드에서 제거됐다.** 아래는 만든 이유·건전성 계약·실측을
> 남겨 둔 기록이고, `Termination.reask`/`Qdp.closed_by`/`--reslice-children`은 더
> 이상 없다. 제거 근거는 아래 실측 그대로다: 형태는 중립이고 결정 변수는 Cε인데,
> 라이선스가 성립하려면 슬라이스 루트를 usable 규칙의 헤드에서 뽑아야 하고 그러면
> `T_sub ⊇ U`가 **항상** 성립해서 자식보다 **더 많은 규칙 위에서 더 많은 것**을
> 묻는다. 즉 우연한 Cε 부재 말고는 `.qdp`를 이길 구조가 없다. 되살릴 이유가
> 생긴다면(예: Cε 없이 닫히는 자식을 건전하게 인정하는 다른 정리) 이 항목이 계약과
> 함정을 그대로 담고 있으니 여기서부터 다시 시작할 것 — 특히 (b)를 문자 그대로
> 검사할 수 없다는 점과, 슬라이스를 따로 언레이블하면 `u_<n>` 번호가 어긋난다는 점.
>
> 라이선스 검사가 실제로 잡아낸 결함 하나는 제거해도 남는다: usable 폐포는
> *언레이블된* 규칙 위에서, 슬라이스 폐포는 *CTRS* 규칙 위에서 돌아 두 폐포가
> 어긋났다(node 47이 `and(true, y) -> y` 누락으로 거부). 검사가 없었으면 조용히
> 불건전한 YES가 나올 자리였다. **교훈: 두 번 계산하는 포함관계는 검사하지 않으면
> 언젠가 어긋난다.**

열린 dependency-graph 자식을 `.qdp`로 재제출하는 것 말고, 그 자식의 pair가 이름
붙이는 심볼들로 **원본 CTRS를 다시 슬라이스해서 평범한 평면 TRS로 처음부터 증명**하는
경로를 붙였습니다 (`Termination.reask`). `.qdp` **형식 자체가** 문제를 어렵게
만드는 경우가 실재하기 때문입니다 — `$cast_enum`의 node 140(`pow_int,
pow_int_aux`, 2 pairs / 65 rules)은 `.qdp` @900s TIMEOUT인데 슬라이스로는 YES
3.9s, 반면 node 142(`$repeat__value`)는 양쪽 다 TIMEOUT. 일반 법칙이 아니라
대안이라서, `.qdp` **뒤에** 붙는 폴백이고 (`check`의 Children 경로) 자식별 대조는
`--child-report`가 행으로 냅니다. **다만 기본은 OFF입니다**(`--reslice-children`
opt-in) — 아래 실측이 이득을 뒷받침하지 않는데 안 닫히는 자식마다 AProVE 런을 한 번
더 먹기 때문입니다.

**착수 전에 계약을 세우다 보니 지시서에 없던 결정이 하나 나왔습니다.** 닫으려는 건
자식 QDP 문제 `(P_i, Q, R_i)`의 유한성이고 증명하는 건 `SN(T_sub)`이라, 다리는 두
포함뿐입니다:

- (a) `P_i ⊆ DP(T_sub)`
- (b) `R_i ⊆ T_sub`

(b)를 **문자 그대로는 검사할 수 없습니다.** `Qdp.capture_filter`가 자식의 `R`
섹션을 통째로 버리기 때문에(자식 하나당 전체 규칙 집합 재출력 — `Program_ok`에서
2.9GB) 우리가 아는 건 `R_i ⊆ export.printed`(전체)뿐이고, `T_sub`는 그보다 작으니
포함이 안 섭니다. 세울 수 있는 유일한 길은 `.qdp` 경로가 이미 쓰는 **usable-rules
정리를 한 번 더** 쓰는 것입니다:

```
(P_i,Q,R_i) 유한 ⟸ (P_i,Q, U(P_i,R_i) ∪ Cε) 유한 ⟸ (P_i,Q, U ∪ Cε) 유한
                   U = usable(P_i, export.printed) ⊇ U(P_i,R_i)
```

그래서 검사는 (a) + (b′) `U ⊆ T_sub`가 되고, **증명 대상이 `SN(T_sub)`가 아니라
`SN(T_sub ∪ Cε)`가 됩니다.** Cε는 장식이 아니라 정리의 전제고, Toyama의 반례가
정확히 `Cε`라 "두 규칙쯤"으로 넘길 수 없습니다 — 종료하는 시스템을 비종료로 만들 수
있습니다. 따라서 **위의 `pow_int` YES 3.9s는 `termination --symbol`(Cε 없음)로 잰
숫자이고, Cε를 얹은 제출물에서 그대로 재현된다는 보장이 없습니다.** 사용자 결정:
Cε를 넣되(건전 경로), `--child-report`가 **Cε 없는 판정도 같은 슬라이스에 대해 같이
재서** 그 대가를 숫자로 남긴다 (`slice_bare` 열 — 아무것도 닫지 않는 참고 수치).

`Qdp.closed_by`가 두 포함을 실제로 검사합니다. (a)는 `T_sub`의 dependency pair를
직접 계산해서 자식의 pair를 **인스턴스 매칭**합니다 — AProVE가 pair의 변수를
`x0`/`x1`로 개명하고 instantiation 프로세서가 특수화하므로 등호가 아니라 인스턴스
관계입니다. pair를 **rewrite**하는 프로세서를 거친 자식은 우리 DP 집합 밖이라 검사가
거부하고, 그때는 `.qdp`가 유일한 건전한 질문 방식입니다 — 결함이 아니라 폴백.

두 가지 함정이 있었고 둘 다 코드에 박아뒀습니다:

- **슬라이스를 따로 unravel하면 안 됩니다.** `u_<n>` 헬퍼는 규칙 리스트 전체에 걸친
  카운터로 번호가 붙어서, 부분집합을 따로 언레이블링하면 같은 헬퍼가 다른 이름을
  얻고 자식의 pair가 아무것과도 매칭되지 않습니다. 그래서 `Unravel.export`에
  `origins`(정의된 심볼 → 그 규칙의 CTRS head)를 실어, **전체 export의 printed에서
  부분집합을 골라내는** 방식으로 갑니다. 철자가 한 벌이라 (a)·(b′)가 텍스트 그대로
  성립합니다.
- **슬라이스 루트는 `pair_heads`가 아니라 `Qdp.reposed.seeds`입니다.**
  `pair_heads`는 사람에게 자식을 이름 붙이려고 헬퍼를 버리므로, 그걸 루트로 쓰면
  (b′)가 슬라이스가 지킬 이유가 없던 규칙에서 깨집니다. seeds(=pair가 어디서든
  이름 붙이는 모든 심볼)를 루트로 잡으면 CTRS 슬라이스의 하향 폐포가 TRS 수준
  usable 폐포를 덮습니다.

**ITRS는 이 경로에 넣지 않습니다** (사용자 결정, 재론 금지). `Itrs.of_system`은 첫
동작이 replaced 규칙 **삭제** + 손으로 쓴 정수 정의 대입이라 과대근사가 아니고,
코덱은 아예 다른 알고리즘(±2^w 보정 루프 → 닫힌 식)입니다. 위 (b′) 검사가 ITRS를
자동으로 거부합니다 — 자식이 든 usable 규칙이 `I_sub`에 없으니까. 이 김에 사실과
달랐던 문서 두 곳을 정정했습니다: `itrs.ml`/`itrs.mli`의 "평면 TRS와 같은 방향의
건전성", `termination.mli`의 `type surface` "both over-approximating the same
CTRS, so a Yes on either transfers to the slice". 둘 다 **"ITRS는 모델이고 그
YES는 모델 수준 진술(코덱은 한 단계 더 약함)"**로 바꿨습니다. 행 표기는 현행
유지 — verdict 열은 그대로 `YES`, 구분은 paths 열의 `itrs:`가 합니다.

**첫 실측이 이 경로의 전제를 흔들었습니다** (`$bin_shl`, budget 30 / child 20):

| node | names | pairs | `.qdp` | slice **+Cε** | slice **−Cε** |
|---|---|---|---|---|---|
| 45 | `pow_int, pow_int_aux` | 2 | TIMEOUT (65규칙) | TIMEOUT (65규칙) | **YES 4.0s** |
| 46 | `$int_to_bitstr` | 4 | TIMEOUT (177) | TIMEOUT (177) | TIMEOUT |

node 45는 `$cast_enum`의 node 140과 같은 `pow_int` 패밀리이고, `−Cε`의 4.0s는
지시서가 인용한 3.9s와 일치합니다. 그런데 `.qdp`와 slice의 **규칙 집합이 똑같이
65개**였습니다 — 즉 두 제출물의 차이가 *형식*뿐이었고, **형식만으로는 판정이 안
움직였습니다.** 움직인 건 Cε 두 줄입니다. 이전 세션의 "`.qdp` 형식 자체가 문제를
어렵게 만든다"는 결론은 형식과 Cε를 한꺼번에 바꾼 비교였습니다 — MTT에서 얻은 그
교훈("X가 Y를 일으킨다고 하려면 X만 바꿔라")에 다시 걸린 셈입니다.

구조 논증도 같은 쪽을 가리킵니다. 루트를 usable 규칙들의 head에서 뽑도록 고친 뒤로
`T_sub ⊇ U`가 항상 참이므로, slice 재질문은 `.qdp`보다 **더 많은 규칙 위에서 더 센
것**(SCC 하나의 사슬 유한성이 아니라 시스템 전체의 종료)을 묻습니다. 살 수 있는 건
"AProVE가 자기 분해를 다시 해서 다른 궤적을 탄다"뿐인데, node 45에서 그게 일어나지
않았습니다. 그래서 기본 OFF.

((b′) 검사가 우리 쪽 결함도 하나 잡았습니다: node 47이 `the system is missing the
rule and(true, y) -> y`로 거부됐는데, usable 폐포는 **언레이블링된** 규칙 위에서,
슬라이스 폐포는 **CTRS** 규칙 위에서 도는 탓이었습니다. 루트를 usable 규칙들의
head에서 뽑도록 고쳐 구조적으로 성립하게 했습니다 — 루트는 자기 head의 모든 규칙을
지키므로.)

budget 300 재확인(Cε가 치명적인지 그냥 비싼지)은 **아직**입니다.

**다음 방향은 term 열 전수 재측정으로 정했습니다** (2026-08-22, 사용자 결정). 지금
모든 설계 판단이 stale 숫자 위에 서 있습니다 — 캐리어 flat 전환, 경로 3개화, 정수
표면 `rebind_stuck_guards`, `eq` 가드 제약화, 루트 삭제 버그가 전부 열을 무효화했고,
행 자체도 5열로 늘었습니다. `sweeps/run_term145.sh`가 `bff805ec` 기준선의 심볼 집합
(153개 중 모노모피즘으로 특수화된 8개를 뺀 **145개**, 행 대조 가능)을 현재 표면에서
직렬로 다시 잽니다. `--reslice-children`은 끄고 — 이 열은 기본 check가 밟는 세 경로를
재는 것입니다. 300초 cap에서 원리상 YES가 안 나오는 심볼들(기준선의 `$bin_minus`
1800.5초, `$write_bits_from_value` 1320.4초, `$bin_shr` 420.2초)을 위해 TIMEOUT 행에
`--budget 1800` 재질의 pass가 뒤따릅니다. `$cast_enum`은 측정 대상이
아닙니다 — 건전한 경로가 전부 소진됐고(plain 통짜 KILLED @2,032규칙 JVM ~30GB OOM,
plain 자식 5/5 TIMEOUT @900s, itrs 통짜 KILLED에 분해 자체 없음), 이 경로를 만들어도
자식 5개 중 140 하나만 닫히며 node 158(pairs 143 / rules 2,032 = 사실상 원래 문제
전체)이 남습니다. 잔여로 기록.

### 2026-08-21 (정정 2) — **`Itrs.of_system`이 슬라이스 루트를 지웠다. 그 루트의 `itrs:YES`는 공허했다**

아래 "정정" 항목이 근거로 쓴 `itrs:YES@0.6s` 두 줄은 **그 심볼이 들어 있지 않은
시스템**의 종료를 증명한 것이었습니다. 결론(자식을 정수 표면으로 보내면 닫힌다)은
고친 표면에서 다시 재어 살아남았지만, 숫자는 갈아끼웠습니다.

**기전.** `dropped`가 교체 대상 머리(`replaced_head`)의 규칙을 전부 걷어냅니다.
그 머리가 **슬라이스의 루트 자신**이면 살아남은 규칙 어디에도 그 이름이 안 남고,
`reachable_prelude used candidates`가 도달 불가로 보아 **그 자리를 메우라고 써 둔
정수 정의까지 같이 잘라 냅니다.** `undefined` 검사도 못 잡습니다 — 그 검사는
"규칙에 살아남았는데 정의가 없는 머리"만 보는데, 루트는 흔적 없이 사라지니까요.

    $ termination --symbol '$int_to_bitstr' --itrs --emit-trs   # 고치기 전
    (RULES
      negate_int_pos_aux(...) ... add ... sub ... negate_int ...   # 8줄, 전부 잔여물
    )                                                             # $int_to_bitstr 없음

한 변수 대조 — 코덱을 **호출하는** 쪽은 멀쩡했습니다:

    $bin_plus       (호출자)   48규칙, 코덱 있음
    $bitstr_to_int  (루트 자신) 12규칙, 코덱 0
    $int_to_bitstr  (루트 자신)  8규칙, 코덱 0

**고침 (`itrs.ml`, `~roots` 추가).** `Itrs.of_system`이 슬라이스 루트를 받아
`used`에 심고, 그러면 ⑴ 그 루트의 정수 정의가 가지치기에서 살아남고 ⑵ 정의가
아예 없는 루트는 기존 `undefined` 검사에 그대로 걸려 **공허한 YES 대신 ERROR**가
됩니다. b-layer 루트(`bpow_nat` 등)는 `blayer` 재지정이 *호출 지점*에서만 일어나
루트일 때는 아무 일도 안 하므로, 같은 표를 규칙으로 써 내는 `blayer_bridge`를
붙였습니다. `Termination.check`/`export`도 `~roots`를 받습니다.

    bpow_nat  →  bpow_nat(x, y) -> pow(x, y)  + pow 2줄
    bcompare  →  unravel: replaced head(s) left with no integer definition: bcompare

**고친 표면에서 다시 잰 값 (예산 300s, 직렬):**

    $int_to_bitstr  itrs:YES@0.7s   rules=19 (전 8)
    $bitstr_to_int  itrs:YES@0.7s   rules=26 (전 12)
    bpow_nat        itrs:YES@0.7s   rules=3
    pow_int         itrs:YES@0.7s   rules=8

교체 머리가 **아닌** 루트에는 무영향입니다(`$bin_plus` 48규칙 그대로): 심는 이름이
프렐류드 머리와 겹치지 않으면 `reachable_prelude`는 아무것도 더 keep 하지 않습니다.

**재측정 대상.** `--itrs`로 잰 행 중 **루트가 `arith_heads`/`list_heads`/
`codec_prefixes`에 든 것**은 전부 무효입니다(84개 심볼이 그 집합). 그 외 행은
그대로 유효합니다. 아래 "정정" 항목의 `itrs:YES@0.6s` 두 줄은 이 항목의 0.7s로
읽으세요.

> **처리 (2026-08-31): 재측정하지 않고 주장을 철회한다.** 그 84행은 `--itrs` 단독
> 스윕의 스크래치 `.tsv`에 있었고 2026-08-20 정리에서 삭제됐다. 그리고 **커밋된 표
> 중 그 행들에 기대는 것이 하나도 없다** — `verification.md`에는 ITRS 열 자체가
> 없고, 2026-08-22의 145행 열은 이 버그가 고쳐진 뒤의 코드로 쟀다. 없는 데이터를
> 다시 만드는 건 스윕 시간만 쓰고 아무 주장도 되살리지 못한다. 남길 것은 이
> 항목의 교훈뿐이다.

**교훈:** 어떤 층을 *삭제하고 다른 표현으로 되돌려 주는* 변환은, 질문의 주어가 그
층 안에 있을 때를 따로 봐야 합니다. 도달성 가지치기는 "누가 부르는가"만 보는데,
루트는 아무도 부르지 않습니다.

### 2026-08-21 (정정, 같은 날 후속) — **산술 자식이 안 닫히는 진짜 이유는 표면이다. 자식 분할과 ITRS가 조합되지 않는다**

아래 Cε 항목은 *완전 재작성 세계 안에서는* 맞지만 원인을 잘못 짚었다. 한 변수씩
더 밀어 본 결과 결정 변수는 Cε도 최내도 아니라 **어느 표면에 올리느냐**였다.

**측정 (현재 트리, 각 단독 실행, 예산 300s):**

    $bitstr_to_int  whole:TIMEOUT@300.8s  children:TIMEOUT(0/1)@601.9s  itrs:YES@0.6s
    $int_to_bitstr  whole:TIMEOUT@300.7s  children:TIMEOUT(0/1)@601.5s  itrs:YES@0.6s

이 둘이 정확히 `$cast_enum`의 열린 자식들이다. plain 표면에서는 통짜로도 자식으로도
안 닫히고 ITRS에서 **0.6초**에 닫힌다. 이유는 하강 측도가 `-n`이고 그게 줄어들려면
`2^w >= 1`을 알아야 하는데, 구조적 이진 자릿수(`bd0`/`bd1`) 위에서 `pow_int`의 재귀
정의를 뚫어야 하기 때문 — CLAUDE.md가 "modular-(B) 산술 맹목"이라 부르는 경우다.

**구조적 빈 구멍: 세 번째 경로(ITRS)는 통짜 슬라이스에만 적용되고 `Qdp.standalone`은
언제나 plain `.qdp`를 낸다.** `$cast_enum`은 통짜로는 plain·ITRS 둘 다 너무 크고
(2,032규칙, 각 TIMEOUT), 자식은 작지만 plain에서 산술 맹목이다. 그래서 세 경로를
다 태워도 닫히지 않는다 — 각 경로가 혼자서는 맞는 도구를 맞는 크기에 못 댄다.

건전하게 메우는 후보 둘 (미결):

1. **자식을 정수 표면으로 재제출.** 자식의 pairs가 plain 심볼(`d_int_to_bitstr`,
   `bd0`, ...)로 쓰여 있고 `Itrs.of_system`의 심볼계는 다르므로, 단순히 규칙만
   갈아끼울 수 없다. pairs 자체를 정수 표면으로 옮기는 작업이 필요.
2. **열린 자식을 그 pair-head 심볼의 슬라이스로 되돌려 3경로 전체를 태운다.**
   `Qdp.pair_heads`가 이미 자식을 심볼 이름으로 부르려고 존재한다. 건전성은
   usable-rules 정리(minimal + Cε)로 논증해야 하고, `slice(S)`가 Cε 하에서도
   종료함을 요구하므로 그 부분을 따로 적어야 한다.

**최내(innermost)는 곁길이었다 — 되돌렸다.** `(STRATEGY INNERMOST)` 한 줄을
`unravel.ml`에 넣어 재 봤고, 패치는 `sweeps/`가 아니라 세션 스크래치패드의
`innermost-experiment.patch`에만 있다. 재 본 내용:

| | 완전 재작성 | 최내 |
|---|---|---|
| 자식 `.qdp` (65규칙, Cε 포함) | KILLED@900s, 프로세서 0 | YES 4s |
| `$fresh_typeIds` 통짜 84규칙 | TIMEOUT@900s | YES 5s |
| `$cast_enum` 통짜 2,032규칙 | TIMEOUT | KILLED@300s (OOM 5회) |
| 미해결 7개(`$bor_nat` 계열 6 + `$tableEntry_lpm_prefix'`) | TIMEOUT | **7/7 KILLED** |

즉 최내는 Cε 병목을 없애 주지만 **얻는 게 ITRS 경로가 이미 주는 것과 겹친다**
(`$fresh_typeIds`는 ITRS로 16.9초에 닫힌다). 반면 증명하는 명제가 완전 종료 →
최내 종료로 **약해진다**. 정당화 근거(Maude `eq` 축약이 최내)는 있지만
SN(TRS)⟹SN(CTRS) 논증을 최내 판으로 다시 쓰고 unraveling 정리의 최내 대응을
확인해야 한다. 얻는 것이 겹치므로 지금 치를 값은 아니다.

**최내 실험이 딸려서 잡은 진짜 버그 (`qdp.ml`, 최내와 무관):** `is_renamed_var`가
`x<n>`만 인식해서 AProVE가 스스로 만든 변수를 상수로 볼 뻔했고, 실제로는 split을
`ERROR`로 거부했다. AProVE의 신선 변수 이름은 관측된 대로 `x`/`y`/`z` + 인덱스이며,
평범한 철자가 이미 우리 `(VAR ...)`에 있으면 인덱스 앞에 밑줄이 붙는다 — `y0..y3`을
선언하고 `z`를 안 쓰는 실행이 `z0..z4`는 맨몸으로, `y_0`은 밑줄로 돌려받는다.
**완전 재작성에서도 터진다**: `Type_wf`의 완전 재작성 에코에 `x_<n>`이 1,750번 나온다.
잘못 읽어도 안전한 방향(문제가 일반화되어 더 어려워짐, YES는 전이)이라는 논거를
주석에 적었다.

### 2026-08-21 — **자식 분할이 산술 자식에서 못 닫는 이유는 Cε다** (그리고 `TRUE`/`FALSE` 트리 어휘)

두 가지를 잡았습니다. 하나는 고쳤고, 하나는 **고치면 안 되는 것**입니다.

**(1) 트리 어휘 누락 — 고침.** `Qdp.is_verdict`가 `YES`/`NO`/`MAYBE`만 알았는데,
`DependencyGraphProof`가 그래프를 비워 분기를 닫으면 AProVE는 `TRUE`를 찍고 에코
본문도 `TRUE` 한 줄뿐입니다(의무 없음). 이걸 열린 자식으로 세어 없는 의무를 찾다
**분할 전체를 거부**했습니다. `is_verdict`에 `TRUE`를 넣고, `FALSE`는 `is_refuted`로
분리해 `OR`처럼 트리 전체를 거부하게 했습니다 — `FALSE`는 분기를 닫지도 재제출되지도
않으므로 조용히 넘기면 "모든 자식이 닫혔다"는 **거짓 YES**가 됩니다. 유닛 테스트
2개(`qdp/true-terminal-is-closed`, `qdp/false-terminal-refuses-the-split`).

효과: `$subst_externMethodTypeDefEnv`·`$cast_enum`이 거부 → 실제 분할. `Type_wf`는
열린 잎 2→1. `Type_wf`가 `bsplit2`에서 여전히 거부된 건 별개 원인(그 실행의 에코
절단)이고, 단독 실행하면 정상 분할됩니다(`0/1 closed, node 233`). 잘리지 않은 원본
29MB로 `open_leaves`를 재현하면 열린 잎은 정확히 `[233]` 하나이고 의존쌍 103개와
함께 에코에 있습니다.

**(2) Cε가 증명을 못 하게 만든다 — 고치면 안 됨.** `$cast_enum`의 자식 `pow_int`은
단독 슬라이스 `.trs`(63규칙)로는 **YES 4초**인데, 재제출된 `.qdp`(65규칙)로는
**900초 KILLED, 프로세서 0개**입니다. `--aprove-bin`을 복사 래퍼로 돌려 실제 `.qdp`를
잡고 한 변수씩 바꿔 격리했습니다:

| 변형 | 규칙 | Cε | 결과 |
|---|---|---|---|
| v1 (실제 자식) | 65+2 | 있음 | KILLED 120s (900s도 동일) |
| v2 (`(MINIMAL)` 제거) | 65+2 | 있음 | KILLED |
| v3 (`(VAR)` 226→6) | 65+2 | 있음 | KILLED |
| v4 (둘 다) | 65+2 | 있음 | KILLED |
| **v5** | 65 | **없음** | **YES 4s** |
| v6 (가지치기 없음) | 2034 | 없음 | KILLED 180s |

기각된 원인: `(MINIMAL)` 지시자, `(VAR …)` 226개 선언, `.qdp` 전용 전략
(`automatic_strategies.properties`는 `QDP`·`TRS` 둘 다 `aprove.Auto.current`로 같음).

**기전**: Cε가 있으면 AProVE가 값싼 처리기들을 건너뛰고
`QDPReductionPair[Order=KBO[Engine=MINISAT]]`로 직행해
`aprove.DPFramework.Orders.Utility.KBO.IntMatrix.multiply`에서 예산을 통째로 태웁니다
(`bmul`/`badd` 이진 산술 규칙 위에서 부등식계가 폭발). 없으면
`MNOCProof → UsableRulesProof → QReductionProof → TransformationProof →
DependencyGraphProof → … → MRRProof → TRUE`가 **전부 0ms**에 끝납니다.

**그런데 Cε는 뺄 수 없습니다.** 그게 usable-rules 가지치기를 건전한 증명 단계로
만들어 주는 조건입니다(Urbain 2004; Giesl et al., JAR 2006) — R에서 규칙을 덜어낸
문제의 유한성은 원래 문제의 유한성을 함의하지 않고, 그 간극을 메우는 게 Cε입니다.
치환 인스턴스가 임의 심볼을 들여올 수 있으므로 "도달 불가능한 규칙 버리기"로
우회할 수도 없습니다. v5는 **진단이지 해법이 아니며**, 그대로 채택하면 거짓 YES를
양산합니다.

**남은 건전한 후보는 "가지치기를 아예 안 하기"** 하나입니다(가지치기가 없으면 Cε도
불필요). v6이 180초에 죽었으므로 예산이 원인인지 확인 중. 성립하면 `Qdp.standalone`의
가지치기는 파싱 시간을 사는 최적화였을 뿐(qdp.mli가 그렇게 적어 둠)이므로,
산술이 섞인 자식에서만 끄는 선택이 가능합니다.

**측정에 미치는 뜻**: 자식 분할의 TIMEOUT은 "이 자식이 어렵다"가 아니라 **Cε가
붙었다**는 뜻일 수 있습니다. `$cast_enum`의 열린 자식 4개
(`pow_int`/`$int_to_bitstr`/`$repeat__value`/`$bitstr_to_int`)가 전부 이 부류이고,
넷 다 프로세서 0개로 900초를 태웠습니다. `Program_ok`의 미증명 25개 중 Group A
15개도 같은 의심을 받아야 합니다.

**주의**: `$fresh_typeIds`는 이 부류가 **아닙니다** — 단독 슬라이스를 plain 표면에
900초 줘도 TIMEOUT이고(84규칙), ITRS에서만 16.9초에 닫힙니다. 그건 표면 문제입니다.


### 2026-08-20 — **`eq` 가드도 제약이어야 했다** (`Itrs.equality_relation`), 그리고 `Program_ok` 자식 73개의 분포는 193규칙에서 갈린다

**08-18 항목이 `leq`/`lt`/`nonneg_int`에 대해 고친 결함의 나머지 절반이 `eq`에 남아
있었다.** 스펙은 nat 가드를 `-- if n =/= 0`으로 쓰고 그건 조건 `eq(n, 0) = false`가
되는데, `lift_guards`가 `eq`를 몰라 그대로 사슬 단계가 되고 하강 근거가 제약 논리에서
사라진다. `Itrs.equality_relation`이 이걸 `:|:` 제약으로 되돌린다.

`eq`는 제네릭 구조적 등호라 무조건 올릴 수 없다. **한쪽이 접힌 정수 리터럴일 때만**
올린다: 원본 CTRS가 order-sorted라 리터럴이 반대편 인자의 sort를 확정하고, 그게 스펙이
이 가드를 쓰는 유일한 형태다. 정수가 아니면 두 형태 **모두** 발화하지 않으므로(구조적
`eq` 규칙에 생성자-정수리터럴 짝이 없고, 비정수 제약은 평가되지 않는다) 등가가 유지된다.

```
- u_1(crcv1, n, St0) -> u_2(eq(n, 0), n, St0, crcv1)          불투명한 사슬 단계
+ d_fresh_typeIds(itrsz1, St0) -> u_1(...) :|: itrsz1 != 0     제약이 사슬 전 단계에
```

| 예산 120 ITRS, 심볼 고정 | 이전 | 이후 |
|---|---|---|
| `$fresh_typeIds` | TIMEOUT 120.8s | **YES 16.5s** (u 4→3) |
| `$repeat__value` | YES 5.8s | YES 7.0s (u 2→1) |
| `$bin_shr` | TIMEOUT | TIMEOUT (u 50 불변 — 올릴 가드가 없다) |
| `$bin_bor` | TIMEOUT | TIMEOUT (u 20→16, 4개 올랐지만 부족) |

**손으로 만든 근사가 실제 수정을 과소평가했다.** 사슬 단계를 남긴 채 제약 규칙 두 개를
덧대는 형태는 같은 예산에서 128초였는데, 단계를 없애고 제약을 사슬 전체에 싣는 실제
리프트는 16.5초다. 프로브로 방향을 잡되 수치는 구현으로 다시 재야 한다.

**`Program_ok` 자식 전수 측정** (`--child-report`, 전체 예산 300 / 자식 120).
자식 이름은 `To_ctrs.symbol_origins`가 붙인다(TPDB 철자 → 슬라이스 CTRS 심볼 →
`선언명@파일:줄`).

- **73자식: YES 48 / TIMEOUT 25.** 닫힌 48건은 평균 7.1초(최대 60.8초, 합 339초)로
  싸다. AProVE 순수 시간 57.2분.
- 잔여 25건이 **규칙 수 193과 2034 사이에서 완전히 이분된다**: 아래 15건(57–193규칙)은
  전부 산술(`bpow_nat`·`pow_int`·`$repeat__value`·`$fresh_typeIds`·`$int_to_text_nat`·
  `$shr`·`$shr_arith`·`$bor_nat`·`$band_nat`·`$bxor_nat`·`$int_to_bitstr`·
  `$bitstr_to_int`·`$tableEntry_lpm_prefix'`), 위 10건(2034–16626규칙)은 대형 구조
  (`$cast_enum`·`$subst_externMethodTypeDefEnv`·`Type_wf`계열·`Cast_expl`·`$cast_binary`·
  `Argument_eval_lctk`·`ArgumentList_ok`·`BlockElementStmtList_ok`·`ParserStmtList_ok`·
  `InstDecl_objectInitializer_ok`).
- **작은 잔여의 절반 이상이 spec이 아니라 prelude에 있다.** `bpow_nat`·`pow_int`·
  `$bor_nat`·`$band_nat`·`$int_to_bitstr`·`$bitstr_to_int`·`$int_to_text_nat`은 650개
  슬라이스 루트 어디에도 없다 — `--symbol`로 겨냥조차 안 되는 산술 라이브러리 내부다.

**`$fresh_typeIds`의 원인은 산술이 아니라 gensym 상태 표현이었다 (변형 5개로 분리).**
`Gensym.thread`가 심는 상태는 `cat(St0, cons(chr(39), nil))` — 문자 하나씩 자라는
문자열이고, 그게 모든 재귀 호출을 타고 흐른다.

| 변형 (한 번에 한 변수) | 결과 |
|---|---|
| 원본, 예산 120 / 300 | TIMEOUT / **YES 302s** |
| `n≠0` 가드를 제약으로 | YES 128s |
| 성장을 `cat` 대신 `cons`로 | YES 121s |
| 성장 자체를 제거 | YES **5s** |
| 상태를 네이티브 정수 카운터로 | YES **7s** |

즉 지배 요인은 **성장 그 자체**이고 `cat`은 부수적이다. 정수 카운터는 근사가 아니라
구현 선택이다(gensym에 필요한 건 서로 다름뿐).

**다만 대형 심볼에서는 이 변수를 관측하지 못했다.** `$subst_externMethodTypeDefEnv`
(ITRS 3058규칙)와 `Argument_eval_lctk`(7469규칙)는 예산 300에서 base·카운터 **양쪽 다
YES/NO/MAYBE 줄을 아예 출력하지 못했다**(전단조차 못 끝냄). 결론은 "효과 없음"이 아니라
**"더 큰 예산 없이는 알 수 없음"**이다. 슬라이스에 gensym이 실린 대형 자식은 10건 중
6건이고, `ArgumentList_ok`에서는 14569규칙 중 **1616(11%)**이 상태 변수를 나른다 — 그것도
하필 증명해야 할 리스트·치환 재귀 규칙들이다. 표현 변경의 값어치는 여기서 갈리는데,
아직 재지 못했다.

**표현 변경이 정수 표면 한정 이득이라는 점도 기록해 둔다.** 구조적 표면에서 nat은
Peano/이진이라 카운터도 여전히 자라는 항이고, 실행 표면에서 카운터로부터 이름을 만들려면
int→text가 필요한데 `$int_to_text_nat`이 마침 잔여 자식 중 하나다.

**잔여 25건의 수동 검토 — 실루프는 없고, 둘 다 예산 문제도 아니다.**

*Group A(산술 15건, 57–193규칙).* `bpow_nat`·`pow_int`·`$repeat__value`·`$fresh_typeIds`·
`$int_to_text_nat`·`$shr(_arith)`·`$bor_nat(_cross)`·`$band_nat(_cross)`·`$bxor_nat`·
`$int_to_bitstr`·`$bitstr_to_int`·`$tableEntry_lpm_prefix'`. 전부 실제로 종료하고 하강이
`pow`/`div`/`sub` 안에 묻힌 arith-blind다. **예산이 아니라는 직접 증거**: `$bin_bor`을
정수 표면 예산 300과 900으로 돌린 트리가 노드 941로 완전히 동일하다(전략 소진).

`w=0` 계열에 대한 정정: `ISSUE-bit0-divergence.md`는 그 경계가 157 슬라이스(`Program_ok`
포함)의 종료성을 provably false로 만든다고 적지만, **이 트리에는 해당하지 않는다** —
방출된 규칙이 `d_bitstr_to_int(w, n) -> 0 :|: w <= 0`이고 재귀 가지는 `w > 0`으로 막혀
있다(작업 트리의 `builtin.ml` 수정이 측정에 들어가 있다). 잔여는 증명 가능한 거짓이
아니라 **미증명**이다.

*Group B(대형 구조 10건, 2034–16626규칙).* `$cast_*` 계열·`$subst_externMethodTypeDefEnv`·
`Type_wf` 계열·`Cast_expl(_neq)`·`$cast_binary`·`Argument_eval_lctk`·`ArgumentList_ok`
계열·`Block/Stmt/SwitchCase` 계열·`ParserStmt` 계열·`InstDecl/ObjectDecl` 계열. 전부 AST
구조 재귀다. **예산 가설은 기각됐다**: 자식 예산 120초 대신 심볼 슬라이스에 **600초**를
주고 plain 단독으로 돌린 결과가 **10/10 TIMEOUT**이다. 그리고 usable-rules 가지치기가 이
무리에선 아무것도 못 줄인다 — `$subst_externMethodTypeDefEnv`의 슬라이스가 3141규칙,
자식이 3143규칙으로 사실상 같다. 크기 자체가 벽이다.

닫힌 48건의 상한이 이 판단의 기준이다: `$gen_constraint`(5327규칙/59쌍) 58.1초,
`holds_Cast_impl`(3853/68) 60.8초, `$free_callableTypeDefIR`(1420/**119쌍**) 19.8초가
닫혔다 — 규칙 수천 개나 쌍 100개 이상이 그 자체로 장애는 아니다.

구조 하강을 스펙만으로 단정할 수 없어 따로 표시해 둔 2건: **`$cast_binary`**(자기 재귀가
아니라 `Cast_impl`/`Type_alpha` 경유 17쌍 순환)와 **`Type_wf` 계열**(타입이 자기를
참조하면 하강이 무너진다). 이 둘은 실루프 위트니스를 찾아볼 후보다.

**`Program_ok`을 통째로 정수 표면에서 예산 3600으로 돌려 봤다 — TIMEOUT인데 트리가
이유를 말한다.** `eqs=13321 rules=19970 u=6823`.

```
(1) ITRStoIDPProof [EQUIVALENT, 1593.7 s]   예산의 44%를 전단이 먹는다
(3) ItpfGraphProof              596 ms
(5) IDependencyGraphProof         0 ms
(6) AND                                      IDP 자식 약 540개
    (7)…            UsableRulesProof [0 ms] → IDP   손댄 건 80개
    (81)…(704)      IDP                             나머지는 도달조차 못 함
```

**하나도 닫히지 않았다.** plain 표면의 자식 굶주림과 같은 구조인데 전단이 먼저 절반을
먹는다. 그래서 정수 표면에도 자식 분할이 필요하고, 이득은 plain보다 크다 — 독립 자식은
1594초짜리 ITRS→IDP 변환을 통째로 건너뛴다.

**AProVE는 `.idp`를 되읽는다.** `extension.properties`에
`idp = aprove.InputModules.Programs.newIDP.Translator`가 있고, `NewIDPParser` 토큰은
`PREDEFINED · RULES · EDGES · INITIALNODES · STRATEGY · MINIMAL · CONDITION ·
CONSTRUCTOR`다. 에코도 그래프째로 찍는다(노드별 변수 첨자 `itrsz9[27]`, 간선
`(21) -> (27), if (…)`). **다만 QDP 분할보다 훨씬 무겁다**: `(VAR)(PAIRS)(RULES)(Q)`만
옮기면 됐던 것과 달리 의존 그래프의 간선·위치·변수개명을 재구성해야 한다. 가능성은
확인됐고 비용도 확인됐다 — 착수 여부는 별건.

### `IDPNonInf`이 `!=`에서 예외로 죽는다 (그리고 그게 판정을 바꾸지는 못했다)

`$bin_bor`의 ITRS 실행 출력 맨 앞:

```
Aborted … Exec. 10, IDPNonInf with some error. Reason: java.lang.IllegalArgumentException:
can not interpret predefined function !=_2.
   aprove…IDPGInterpretation.getPolynomialFromFunction(IDPGInterpretation.java:871)
```

10건 전부 같은 예외다. `IDPNonInf`은 정수 순환을 닫는 처리기이고(08-18에 `$contains_prime`을
닫은 게 `IDPNonInfProof`였다), `!=`를 다항식 해석으로 옮기지 못한다. CTRS에서 nat은
sort상 음이 아니므로 `n != 0` ⟺ `n > 0`인데, ITRS가 무타입이라 그 등가를 잃고 `!=`를 쓴다.

| 예산 300(`$contains_prime`은 120) | `!=` (현행) | `> 0` | `< 0 \|\| > 0` |
|---|---|---|---|
| `$bin_bor` `IDPNonInf` 크래시 | **10** | **0** | **0** |
| `$bin_bor` QDP 노드(탐색량) | 1906 | 297 | 944 |
| `$bin_bor` 판정 | 미증명 | 미증명 | 미증명 |
| `$contains_prime` 판정(예산 120) | TIMEOUT | (치환 무효 — 위 참조) | (치환 무효) |
| `$fresh_typeIds` 판정(예산 120) | **YES 16s** | **TIMEOUT** | YES 39s |

**크래시 원천은 우리 가드만이 아니다.** `$contains_prime`의 ITRS에는 `!=`가 딱 한 줄,
프렐류드 자신의 정수 등호 규칙 `eq(x, y) -> false :|: x != y`뿐인데 그것만으로 크래시가
10건 난다.

**08-18 사례로 회귀를 재려던 시도는 두 번 틀렸다(자기 점검용으로 남긴다).** 첫째, 치환
패턴이 `x != 0`이라 위의 `x != y` 한 줄에 닿지 않아 **같은 파일을 세 번 돌렸다**(세 md5가
같다). 둘째, 그 출력을 `grep -E '^(YES|NO|MAYBE|TIMEOUT)'`로 읽었는데 AProVE 출력에는
**하위 의무의 판정 줄이 섞여 있어** 중첩 `YES`를 최상위로 오독했다 — 실제로는 셋 다
예산 120에서 TIMEOUT이다. 판정은 반드시 `Aprove.header_lines`의 규칙(에코 시작
`proof of ` 이전 헤더에서만 찾기)으로 읽어야 한다. **2026-08-06에 판정 파서 결함이
termination 열 전체를 무효화했던 것과 같은 실패 모드다.**

**크래시는 없앴지만 판정은 하나도 안 움직였다.** 그리고 예산 900을 줘도 `> 0` 변형의
트리는 노드 941로 300초와 **완전히 동일하다** — AProVE가 전략을 소진하고 멈춘다. 즉
`$bin_bor`은 예산 문제가 아니고, 트리를 보면 `(10) IDPtoQDPProof [SOUND, 9 ms]`로 즉시
평범한 QDP로 내려가 거기서 `TransformationProof` 수백 개를 돈다 — 08-18이 이미 "남은 두
번째 장애물"로 적어 둔 그것이다.

**그래서 `!=` 방출을 바꾸는 수정은 아직 하지 않았다.** 크래시 제거와 탐색량 감소는
실측됐지만 판정 이득이 없고, 08-18은 `!=`가 분해형보다 **낫다**고 기록하고 있다(그때
측정은 `!=`로 YES 15s가 나온, `IDPNonInf`이 살아 있던 사례다). 방출을 바꾸려면 그 사례를
세 철자로 되재서 회귀가 없음을 먼저 보여야 한다.

### 2026-08-20 — **판정 경로가 셋이 되었다**: `Termination.check`의 자식 분할 + ITRS 재시도, 캐리어는 flat 하나로

앞의 두 항목(캐리어, 자식 단독 실행)이 측정으로 끝낸 이야기를 코드로 옮겼다. `termination`
한 심볼은 이제 **세 경로를 순서대로** 밟고 첫 YES에서 멈춘다.

| 경로 | 무엇을 | 언제 |
|---|---|---|
| `whole` | 슬라이스 전체를 TRS 하나로 | 항상 |
| `children` | 그 실행의 **미해결 자식**을 하나씩 단독 실행 | whole이 미증명일 때 (`--no-child-split`로 끔) |
| `itrs` | 슬라이스 전체를 정수 표면으로 | 앞 둘이 미증명 + 슬라이스가 산술을 쓸 때 (`--no-integer-retry`로 끔) |

**행 형식이 5열이 되었다** — `<sym>\t<verdict>\t<stats>\t<secs>\t<paths>`, 마지막 열이
`whole:TIMEOUT@300.0s children:TIMEOUT(48/75 closed, node 911 TIMEOUT)@412.0s itrs:...`
꼴이다. 실패한 분할의 숫자가 판정(=whole의 것)에 먹히지 않고 남는 자리다.

**구현 (`qdp.ml`/`qdp.mli` 신규).**
1. **캡처 실행.** `Aprove.check ~capture`가 트리에서 멈추지 않고 끝까지 읽되, 스트리밍
   중에 줄 단위로 거른다(`Subproc.run ~keep`, 상태를 갖는 섹션 필터). `Program_ok`의
   전량 에코 **2.9GB / 11,867,032줄 → 8.4MB**만 남는다. 버린 것은 각 자식마다 다시 찍히는
   `R` 본문뿐이고, 그건 우리가 이미 가진 export다.
2. **트리 읽기(`open_leaves`).** 문제·프로세서 체인이 *같은 들여쓰기*로 찍히고 AND의
   자식만 한 칸 들어가므로, "다음 줄이 더 깊거나, 같은 깊이의 프로세서면 이어진다"로
   판정한다. 프로세서 판별은 대괄호가 아니라 **이름의 `Proof` 접미사**로 한다 —
   프로세서를 문제로 오독하면 이미 닫힌 노드를 한 번 더 푸는 낭비지만, 문제를
   프로세서로 오독하면 **열린 자식이 통째로 안 보이고** 분할이 "전부 닫힘"을 보고한다.
   실측 검증: `Program_ok.flat.tree` **75/75**, `Program_ok.keep.tree` **260/260**이
   `*_full.out.children.tsv`와 완전 일치.
3. **자식 재구성(`standalone`).** `(VAR)(PAIRS)(RULES)(Q)(MINIMAL)`. 규칙은 에코의 것이
   아니라 **우리 export**를 쓴다(실행이 그때까지 줄여둔 것의 상위집합 — 규칙을 줄이면
   체인만 없어지므로 상위집합은 항상 안전한 방향). 식별자는 전부 계정 처리한다:
   export의 변수, 또는 (대문자 무시) export의 심볼 — 남는 게 있으면 **분할을 거부**한다.
   `(VAR)`에 없는 식별자는 **상수로 파싱**되어 문제를 좁히고, 좁아진 문제의 YES는
   원래 자식의 YES가 아니기 때문이다. (에코는 변수 이름을 바꾸지 않는다 — `x0/x1`은
   `subty_*` 규칙이 원래 그 이름을 쓴 것이었다. `extract_child.py`의 `\bx\d+\b`
   가정은 그래서 반만 맞았다.)
4. **usable rules + Cε.** 쌍이 부르는 심볼의 규칙을, 그 규칙들이 부르는 심볼로 닫아
   모은다. 규칙을 지우는 것은 필터가 아니라 **증명 단계**이므로 두 조건을 코드가 강제한다:
   에코가 *minimal*이라 하지 않으면 아무것도 지우지 않고, 하나라도 지웠으면
   `c(x,y) -> x | y`를 함께 내보낸다(Urbain 2004 / Giesl et al. JAR 2006).

**실측 (실제 `Program_ok` 자식 4개, 같은 AProVE, 직렬).**

| 자식 | 쌍 | 규칙 20,404 → | 단독 실행(전체 규칙, 08-19) | **usable+Cε** |
|---|---|---|---|---|
| n629 | 260 | 746 | YES 33s | **YES 2s** |
| n639 | 34 | 918 | YES 34s | **YES 1s** |
| n703 | 88 | 1,193 | YES 36s | **YES 1s** |
| n812 | 8 | 1,218 | YES 35s | **YES 4s** |

판정은 전부 같고, 파일은 4MB → ~130KB, 시간은 **10–30배**. 사용자가 예상한 대로 비용의
대부분은 파싱이었다.

**엔드투엔드 (CLI, `specs/p4`).**
```
$subst_typeIR  TIMEOUT  eqs=1559 rules=1801 u=242 vars=170 owise=0  35.1
   whole:TIMEOUT@35.1s children:TIMEOUT(1/2 closed, node 126 TIMEOUT)@90.4s itrs:TIMEOUT@31.1s
$tableEntry_lpm_prefix  TIMEOUT  eqs=171 rules=186 u=15 vars=18 owise=0  20.6
   whole:TIMEOUT@20.6s children:TIMEOUT(0/1 closed, node 40 TIMEOUT)@44.5s itrs:TIMEOUT@20.6s
```
(둘 다 예산 20–30의 짧은 확인 실행이다. 판정을 뒤집는 예산이 아니라 **경로가 도는지**를
본 것.)

**캐리어는 지웠다 — 전부 flat.** `Unravel.carrier`와 `--flat-carrier`가 사라지고 `k_N`
래퍼는 코드에 없다. 08-19 측정(자식 317 대 502)이 이미 답을 냈고, 두 형태를 남겨두면
"어느 열이 어느 형태였나"가 영원히 따라다닌다. `stats`의 `k=` 항목도 함께 사라졌다
(더 이상 `k_N`이 없으니 거짓말이었다).

> **주의 — 이 커밋은 ITRS 열의 의미를 바꾼다.** `Termination.export`가 이제 두 표면 모두에
> `Crc_surface.rebind_stuck_guards`를 먹인다. 이전에는 plain만 먹였고, ITRS는
> `isStuckHead(...) = false` 가드를 정의 없는 심볼로 들고 갔다 — 언레이블 후
> `u(false, ..) -> r`은 절대 발화하지 않으므로 **그 규칙과 그 안의 재귀가 삭제된 시스템**을
> 증명하고 있었다(plain에서 `b1ca0af6`이 고친 바로 그 결함, 08-09 항목). 방향이 나쁜 쪽의
> 불건전이므로 고쳤고, 따라서 `sweeps/itrs*.tsv`의 YES는 **재측정 대상**이다.

**남은 것.**
- 자식 분할은 **plain 표면에서만** 돈다(ITRS 실행은 QDP가 아니라 IDP 트리라 `standalone`이
  쌍을 못 찾고, 그러면 분할을 거부한다). 의도한 대로지만 문서화 항목.
- 첫 실패 자식에서 멈춘다(나머지 자식은 어차피 시스템을 못 닫으므로 예산 낭비). 전수
  분포가 필요하면 `sweeps/run_children_standalone.sh`가 그 용도다.
- 캡처 실행은 예산을 한 번 더 쓴다(마감 뒤 에코 인쇄에 `echo_slack = 900s`). 미증명
  심볼 하나의 비용은 대략 `2 × budget + Σ자식`.
- **전수 재스윕이 필요하다** — 판정이 오르는 방향의 변경이지만 열이 셋으로 늘었고 ITRS
  표면이 바뀌었으므로, 기워 붙이지 말고 전부 다시.

### 2026-08-18 — **언레이블링이 산술 가드를 파괴한다**: `Itrs.lift_guards`, 그리고 `Program_ok`의 잔여는 예산이 아니다

**조건의 주체가 프렐류드의 비교(`leq`/`lt`/`leq_int`/`lt_int`/`nonneg_int`/
`sub_nat`)이고 패턴이 불리언이면 그 조건은 아무것도 바인딩하지 않는 산술 *검사*인데,
`Unravel`이 이걸 사슬 단계로 바꿔 놓으면 규칙이 실제로 기대는 사실이 제약 논리에서
사라진다.** 이 표면이 존재하는 이유가 산술 하강을 보이게 만드는 것인데, 정작 그 하강을
숨기고 있었다. `Itrs.lift_guards`가 그런 조건을 `:|:` 제약으로 되돌린다.

**위트니스 — `$contains_prime` (27규칙, 통째로 읽힌다).** 스펙은
[0.0-stdlib.spectec:137-143](../../specs/p4/0-aux/0.0-stdlib.spectec)의 카운트업
루프(`n → n+1`, 가드 `n < n_len`)다. 이전 인코딩:

```
d_contains_prime(n, n_len, t, t_c) -> u_4(leq(n_len, n), k_4(n, n_len, t, t_c))
u_4(false, k_4(n, n_len, t, t_c))  -> u_5(eq(idx(t, n), t_c), k_5(...))
u_5(false, k_5(n, n_len, t, t_c))  -> d_contains_prime(add(n, 1), n_len, t, t_c)
```

순환 안에서 `n`은 **증가**만 한다. 감소를 정당화하는 측도는 `n_len - n`이고 그건
`n < n_len`에 달렸는데, 그 사실은 `leq`라는 불투명한 정의 심볼의 결과를 `false`에
매치하는 형태로 숨었다. 카운트업 재귀에서는 이게 치명적이다 — 구조적 재귀와 달리
**감소하는 인자가 아예 하나도 없다.**

| `$contains_prime` ITRS | 도달점 |
|---|---|
| 사슬 단계 (이전), 예산 60·300 | `(0) ITRS` — 앞단조차 못 벗어남 |
| `:|:` 제약 (이후), 예산 60 | `(1) ITRStoIDPProof` → `(7) IDependencyGraphProof` → `(8) AND`, **`(21) IDPNonInfProof`가 산술 순환을 닫음** |

**이건 CLAUDE.md의 MTT 결함과 정확히 쌍대다.** MTT는 *매칭* 조건을 `equal(s,t) -> tt`
검사로 바꿔 **구조적** 하강을 숨겼고(고친 방법이 언레이블링), 언레이블링은 *산술* 가드를
함수 결과로 바꿔 **산술** 하강을 숨긴다(고친 방법이 제약 복원). 조건을 체커가 읽을 수
있는 자리에 되돌려 놓는다는 점에서 같은 처방이다.

**리프팅은 근사가 아니라 정확하다.** 이전 규칙은 비교가 기대한 불리언으로 환원될 때만
발화했고 그건 양쪽 피연산자가 정수일 것을 요구하므로 제약의 인스턴스와 정확히 일치한다.
없어지는 단계(비교 자신의 환원 + 그걸 소비하던 사슬 단계)는 규칙 자신의 두 단계 사이에
유한 개뿐이므로, 이전 시스템의 무한 유도는 여기서도 무한 유도로 사상된다 — YES가 되돌아
전달된다.

**설계 메모 세 개.**
- 제약이 언급하는 변수는 규칙 **전체**에서 시스템 단위 fresh 이름으로 개명한다.
  `of_system`의 `tail`이 제약을 규칙에 붙일 때 **이름으로** 찾기 때문에, 무관한 규칙이
  같은 이름을 쓰면 거기에도 붙는다 — 그건 그 규칙을 *제한*하는 것이고 단계를 없애는
  불건전한 방향이다. (`lift_signs`가 이미 쓰던 규약을 일반화했다.)
- 제약의 표현을 `(변수, 텍스트)`에서 `(변수 목록, 텍스트)`로 넓히고 `tail`이 **전부**
  실린 규칙에만 붙이도록 했다. 부호 리프트는 그 한 변수짜리 특수 경우가 된다.
- 피연산자는 변수와 접힌 리터럴만 올린다. 복합 인자는 산술 *식*으로 렌더링해야 하는
  별도 작업이고, 사슬 단계로 남겨두면 이전 동작 그대로다. 그래서 가드 리프팅은
  **접기 이후**에 돈다 — 닫힌 크기를 정수 리터럴로 만드는 게 접기다.

**남은 두 번째 장애물(고치지 않음, 실측함).** 위트니스는 이 수정 뒤에도 예산 300에서
YES가 아니다. 미해결 의무를 읽으면 `IDPtoQDPProof [SOUND]`가 산술과 구조를 **섞는**
가지(정수 인덱스로 cons-리스트를 훑는 `idx(t, n)`)를 평범한 QDP로 내리면서 내장 정수를
`pos(s(01))`/`plus_nat`/`greater_int` 규칙표로 **되풀어버린다**. 산술 이점이 거기서
증발한다.

**반증된 가설 둘 (기록해 둔다).** `eq`가 구조적 표와 정수 제약을 한 심볼에 겹쳐 갖는 게
원인이라 의심해 두 변형을 손으로 만들어 측정했다 — `eq`/`eqi` 분리도, 비선형
`eq(x, x) -> true` 제거도 둘 다 KILLED 그대로. 헤더의 "정수는 bare로 두고 `eq`를
오버로드한다"는 선택은 이 결함과 무관하다.

**무회귀 확인.** `dune runtest test/rewrite` 통과, impty 골든 일치, 그리고 이전에 YES였던
8개 심볼(`$strip_prefix_rec`·`$strip_suffix_rec`·`$repeat__value`·`$fresh_typeIds`·
`$replace_text`·`$bin_le`·`$int_of_integerValue`·`$init_`)이 예산 60에서 전부 YES,
시간도 동일.

#### 2026-08-19 측정 결과 — 앞단은 뚫렸고, 벽은 한 칸 뒤로 갔다

**심볼 규모에서는 순-제로.** 227 심볼 ITRS 재스윕(`sweeps/itrs227_guardfix.tsv`,
예산 사다리 5/20/80/300, 인코더 하나만 바뀐 대조):

| | 기준선 | 가드 수정 |
|---|---|---|
| YES | 69 | 70 |
| TIMEOUT | 158 | 156 |
| ERROR | 0 | 1 |

전환은 `TIMEOUT -> YES` **1건**(`$infer`)뿐이고 그마저 경계선이다 — 302.5s TIMEOUT →
**300.6s** YES로 예산 300의 벽에 붙어 있고, 그 슬라이스에서 올라간 가드는 5,386규칙 중
**3개**뿐이다. 회귀(`YES -> not-YES`)는 0.

**그런데 수정 자체는 전면적으로 걸렸다.** 사슬 단계 수를 비교하면 **227개 중 205개**
슬라이스에서 `u`가 줄었다. 널리 적용됐는데 판정이 안 움직였다는 건 약한 신호가 아니라
강한 부정 결과 — **단, 이 표본에 한해서다.**

**`Program_ok`에서는 벽이 실제로 움직였다.** 이 스윕의 227개는 전부 `Program_ok`보다
작은 슬라이스라 앞단이 애초에 병목이 아니었고, 그래서 이 효과를 볼 수 없는 표본이었다.

| `Program_ok` ITRS | rules | u/k | 도달점 |
|---|---|---|---|
| 12시간 실행 (2026-08-16, 43,252s) | 20,204 | 7,057 | `(0) ITRS` |
| 가드 수정, 예산 1200 | 19,982 | **6,835** | `(1) ITRStoIDPProof [1133.3 s]` → `(2) IDP` |
| 가드 수정, 예산 7200 | 19,982 | 6,835 | `(5) IDependencyGraphProof` → `(6) AND`, **538 자식** |

가드 **222개**가 올라가면서 `ITRStoIDPProof`가 처음으로 완료됐다(1,070–1,133초). 12시간을
써도 못 하던 일이다.

**하지만 자식은 하나도 안 닫힌다.** 두 표면을 나란히:

| | plain (예산 1200) | ITRS (예산 7200) |
|---|---|---|
| 앞단 | DP 5.7s + 그래프 0.7s | **ITRStoIDP 1,069.8s** + 9.5s + 0.5s |
| 자식(SCC) 수 | 577 | 538 |
| **닫힌 자식** | **317** | **0** |
| 자식에 쓴 시간 | ~1,203s | ~6,100s |

트리에 기록된 시간은 1,238.7초뿐이고 나머지 **5,961초**는 아무 노드도 남기지 못했다.
`UsableRulesProof`가 249개 자식에 적용된 게 전부다.

**교환의 손익이 여기서 분명해진다.** 산술을 내장하는 대가로 앞단에 1,070초를 내고, 그
대신 plain이 `QDPSizeChangeProof`로 사실상 공짜에 닫던 **구조적 재귀 317개를 잃는다.**
`Program_ok`의 자식 대부분은 산술이 아니라 AST 구조 재귀이므로 이 교환은 손해다. 위트니스
`$contains_prime`에서 본 두 번째 장애물(산술·구조 혼합 가지에서 정수가 규칙표로 되풀림)이
규모에서 그대로 재현된 것이다. **`Program_ok`의 실질 경로는 plain이고, ITRS는 이 심볼에
대해 여기서 닫는다.**

**ERROR 1건은 일시적이었다 — 재현되지 않는다.** `TableEntry_priority_ok`는 가드가 194개
올라간 슬라이스(`u` 4627 → 4433)이고 스윕 중 125.3초에 판정 줄 없이 죽었는데, **단독으로
같은 예산에 다시 돌리니 313.9초 TIMEOUT**이다. 통계 열이 바이트 단위로 같으므로 인코딩은
결정적이고 달라진 건 AProVE 실행뿐 — 13시간 스윕 도중의 머신 상태이지 이 수정의 결정적
회귀가 아니다. 2026-08-12에 기록한 "termination 판정의 환경 연성"과 같은 가족.

**그래서 이 수정의 값어치는:** 심볼 규모 판정 이득 0, 회귀 0, 건전성 비용 0(리프팅은
정확), 그리고 **`Program_ok`의 ITRS 앞단을 뚫은 유일한 변경.** 표면이 표방하는 일(산술
하강을 보이게 만들기)을 실제로 하게 만든다.

**다음 실제 작업은 ITRS가 아니라 plain의 미해결 260개가 무엇인지 알아내는 것이다** —
`aprove.ml`의 조기 종료(`proof_printed`)를 끄면 `----` 뒤 상세 섹션이 통째로 잡히고(위트니스
기준 33,642줄, 389섹션) 각 QDP의 실제 DP 쌍이 거기 있다.

> **운영 교훈 (스윕 자동화).** 후속 단계를 잇던 `run_guardfix_chain.sh`가 `pgrep -f`로
> 선행 작업을 기다렸는데, 그 패턴이 **스크립트를 생성한 부모 셸의 커맨드라인**(스크립트
> 본문을 담은 heredoc)을 잡아 자기 자신을 "아직 실행 중"으로 읽었다. 스윕이 04:09에
> 끝났는데 **5시간 53분을 대기만 하다** 단계 2·3을 시작조차 못 했다. `pgrep -f`(전체
> 커맨드라인)가 아니라 `pgrep -x`(프로세스 이름 정확 일치)를 써야 한다 —
> `run_guardfix_chain2.sh`가 그 형태다.

#### 같은 날 확인: `Program_ok`의 잔여는 예산이 아니다 (plain 표면)

`--proof-dir`로 증명 트리를 보관하기 시작한 덕에 처음으로 *어디서* 죽는지 읽을 수 있었다.

| | plain 예산 60 | plain 예산 1200 | ITRS 예산 60 |
|---|---|---|---|
| DependencyPairsProof | 5,651 ms | 6,462 ms | — |
| DependencyGraphProof | 648 ms → **577 SCC** | 729 ms → **577 SCC** | — |
| 닫힌 자식 | 316 | **317** | — |
| 도달점 | `(4) AND` | `(4) AND` | `(0) ITRS` |

**20× 예산이 자식 하나를 더 닫았다.** 두 트리의 diff는 노드 하나뿐이고, 1,210초 동안
적용에 성공한 프로세서는 `QDPSizeChangeProof`가 전부다 — 260개 중 어느 하나도 다른
프로세서로 전진하지 못했다. **12시간 plain 실행은 의미가 없다.**

그리고 이건 지난번 논의의 결론을 뒤집는다: **우리가 손으로 만들려던 SCC 분해를 AProVE가
이미 0.65초에 한다.** `(4) AND`가 곧 모듈화이고, 정적 SCC 지도는 AProVE가 못 하는 일이
아니라 이미 하는 일을 더 거칠게 다시 하는 것이다. 상대 종료(`->=`) 사다리를 손으로 짓는
계획도 같은 이유로 근거가 약해졌다.

**두 표면이 상보적으로 실패한다.** plain은 크기를 감당하지만 자식을 못 닫고, ITRS는
내용(산술)을 감당하지만 앞단이 크기를 못 버틴다. ITRS는 *적용하는 프로세서*가 아니라
*입력 형식*이고, plain 분해의 자식 안에는 내장할 정수가 없다(분석 표면은
`~scalars:Structural`).

**부수 소득.** `aprove.ml`의 조기 종료(`proof_printed`)만 끄면 `----` 뒤의 상세
섹션이 통째로 잡힌다(위트니스에서 33,642줄, 389섹션) — 각 QDP의 실제 DP 쌍이 거기 있어
미해결 자식이 어느 순환인지 바로 읽힌다. 플래그 하나 수준의 작업.

> **정정(2026-08-19).** 이 항목은 처음에 "577개는 AProVE 내부 객체라 손댈 수 없다"고
> 썼다. 틀렸다. `runme`를 직접 돌리면(조기 종료 없이) 각 자식의 P·R·Q가 그대로 찍히고,
> AProVE는 그걸 도로 받는다 — `aprove/Framework/Input/extension.properties`에
> `qdp = newTrs.Translator`가 있고 문법은 `(VAR)(PAIRS)(RULES)(Q)(MINIMAL)(EDGES)`다.
> 손으로 변환한 자식 하나(PAIRS 3 / RULES 88 / Q 76 / VAR 19)가 파싱되어 QDP
> 프로세서를 통과했다. **불가능하다고 말하기 전에 도구가
> 실제로 무엇을 읽는지 확인할 것** — 이 저장소가 MTT에서 이미 배운 교훈이다.
> 다만 그 자식에게 300초를 통째로 줘도 `QReductionProof`만 적용되고 닫히지 않았으므로,
> 조작 가능성이 곧 수익은 아니다. 수익 여부는 아래 항목이 측정으로 답한다.

> **정정(2026-08-20, 같은 날 재측정). 아래 항목이 "예산 배분"이라고 부른 것은
> 예산 문제가 아니다 — *차례* 문제다.** 전체 실행의 예산 사다리를 재보면
> 120s → 498 / 300s → 499 / 600s → 501 / 1200s → 502 / 3600s → 507 (자식 577개).
> **예산을 30배 늘려 자식 9개**를 더 얻었고, 그 9개는 전부
> `QDPSizeChangeProof`, 전부 **0 ms**다. 3480초를 더 써서 0밀리초짜리 호출
> 9번을 얻은 것이므로, 남은 75개는 시간이 모자라 못 푼 게 아니라 **한 번도
> 방문되지 않았다**.
>
> 근거 세 가지:
> 1. **문제가 동일하다.** 추출한 `n639.qdp`/`n703`/`n911`의 `P`(34/88/11),
>    `R`(20,404), `Q is empty`, `minimal (P,Q,R)-chains`가 전체 실행의 해당
>    obligation과 정확히 일치한다. 단독 실행 트리에도 시작 지점의
>    `DependencyGraphProof`가 없어(= `Maybe(depgraph)`가 무변화) `P`가 이미 한
>    SCC임이 확인된다.
> 2. **`n639`은 단독으로 `QDPSizeChangeProof [0 ms]` 하나로 닫힌다** — 전체
>    실행이 다른 498개에 이미 성공시킨 바로 그 프로세서다. 결정적이고 0 ms인
>    단계가 3600초 안에 실행되지 않았다면, 실행될 기회가 없었던 것이다.
> 3. **전체 실행 트리에 등장하는 프로세서는 `QDPSizeChangeProof` 한 종류뿐**
>    (+루트의 `DependencyPairsProof`/`DependencyGraphProof`). 단독에서는
>    `QDPOrderProof` 40회, `DependencyGraphProof` 43회, `UsableRulesProof` 4회,
>    `QReductionProof`/`MNOCProof`/`MRRProof`/`PisEmptyProof`가 모두 나온다.
>    전체 실행은 **정렬 탐색을 단 한 번도 완료하지 못했다.**
>
> 메커니즘은 AProVE의 공식 전략
> (`aprove/PredefinedStrategies/Auto/current.strategy`, jar 내장, 커밋 ID가
> 실행 출력과 일치; `automatic_strategies.properties`에서 `QTRS`와 `QDP`가 모두
> `aprove.Auto.current`로 같은 전략)에 있다. 그 안의 시간 제한은 전부
> `Timer(<밀리초>, <프로세서>)` 꼴로 **하드코딩된 절대값**이지 전역 예산의
> 지분이 아니다(`Timer(250, QDPSizeChange[Subterm = True])`,
> `Timer(5000, QDPReductionPair[...])`, `Timer(20000, QDPSemanticLabelling[...])`
> …). 게다가 `decr = Repeat(1,*, ...)`처럼 **상한 없는 반복**과
> `QDPUsableRules[BeComplete = True, ...]`처럼 **아예 타이머가 없는** 단계가
> 섞여 있다. 그래서 어려운 자식 하나가 수백 초를 태우고 실패하는 동안 그
> 뒤의 자식들은 — 0 ms면 끝날 것이라도 — 차례를 받지 못한다. 자식당 평균
> 약 390초가 그렇게 소모된다(3480초 / 9개).
>
> **따라서 분할이 싼 이유는 총 시간이 더 많아서가 아니다.** 오히려 분할 쪽이
> 총 시간을 덜 쓴다(75 × 300초 상한, 실측 중앙값 37초 → 약 2,800초 대 3,600초).
> 분할이 바꾸는 것은 **마감의 소유권**이다: 공유 실행에서 가망 없는 자식은 제
> 시간만 태우는 게 아니라 뒤의 모든 자식을 막고, 분할하면 정확히 제 몫만
> 태우고 멈춘다. 27개의 진짜 산술 루프가 바로 그 막는 쪽이다.
>
> 재현: `sweeps/run_budget_ladder.sh`, 트리는
> `sweeps/children/ladder/b{120,300,600}.tree`.

### 2026-08-20 — **자식에게 예산을 혼자 주면 닫힌다**: `Program_ok` 577개 중 550개

AProVE의 `(4) AND`는 자신의 건전한 모듈화다 — 의존 그래프의 SCC마다 QDP 자식 하나,
전부 닫으면 시스템이 증명된다. 전체 실행은 **하나의 마감을 577개가 나눠 쓰므로**, tree에
미해결로 남은 자식이 어려운 것인지 *도달하지 못한 것*인지 tree는 말해주지 않는다.

각 자식을 표준 TPDB QDP 문제로 꺼내(`sweeps/children/extract_child.py`) 300초씩 단독
실행한 결과(`sweeps/children/standalone_300.tsv`):

| | 자식 |
|---|---|
| 전체 실행(Flat, 3600초)에서 닫힘 | 502 |
| 단독 실행으로 추가로 닫힘 | **+48** |
| **합계** | **550 / 577** |
| 미해결 | **27** |

닫은 프로세서는 `QDPOrderProof` 40, `QDPSizeChangeProof` 8 — **정렬 쌍 탐색이 주력**이다.
중앙값 37초, 최대 73초이고 그중 ~30초는 자식이 이고 있는 4MB 규칙 집합의 파싱이다.
즉 실제 증명 시간은 수 초. 전체 실행이 3600초로 못 한 일을, 예산을 나눠주지 않고 하면
자식당 몇 초다.

**남은 27개는 산술 라이브러리다.** 19개가 `ARITH`:
`$band_nat`/`$bor_nat`/`$bxor_nat`, `$shr`/`$shr_arith`, `$bpow_nat`/`$pow_int`,
`$int_to_bitstr`/`$bitstr_to_int`, `$int_to_text_nat`, `$tableEntry_lpm_prefix'`.
이들은 대부분 `def_symbols`에 없는 프렐류드/빌트인이라 **심볼 단위 스윕이 한 번도 단독으로
검사한 적이 없다**. 그리고 `$bitstr_to_int`/`$int_to_bitstr`의 w=0은 이미 알려진 열린
문제다(CLAUDE.md, `sweeps/witness/ISSUE-bit0-divergence.md`) — 도구의 한계가 아니라
명세의 질문이다.

슬라이스 루트인 몇몇은 ITRS 표면에서 이미 닫힌다(`sweeps/itrs227_guardfix.tsv`):
`$gen_constraint`/`$gen_constraints`, `$cast_binary`, `$repeat_value`, `$cast_*` 10종,
`$fresh_typeIds` — 전부 plain TIMEOUT → **ITRS YES**. 즉 이번 세션의 두 수정이 서로
맞물린다: 캐리어가 구조적 자식을 값싸게 닫고, 남은 산술을 ITRS 표면이 가져간다.

**다음 작업.** `Termination.check`에 자식 분할 실행 경로를 넣는 것 — 전체 실행이 TIMEOUT일
때 tree에서 미해결 자식을 읽어 각각에 예산을 주고, 전부 닫히면 YES를 보고한다. `AND`의
건전성이 근거이므로 판정은 그대로 전이된다. 파싱 비용(자식당 ~30초)은 규칙 집합을 자식마다
다시 싣기 때문이니, 한 파일에 여러 자식을 넣거나 usable rules로 미리 줄이면 더 싸진다.

### 2026-08-19 — **언레이블링의 캐리어가 공짜 프로세서를 막고 있었다**: `Unravel.carrier`

**`Program_ok`의 미해결 자식 260개 중 산술 순환은 24개(9%)뿐이다.** 전체 obligation을
캡처해(2.9GB / 1,186만 줄, `runme` 직접 실행) 각 미해결 자식의 의존쌍을 프렐류드 어휘와
대조한 결과:

| 순환의 구성 | 개수 |
|---|---|
| STRUCT — 산술이 전혀 없음 | **226 (86.9%)** |
| ARITH | 31 (그중 7개는 `sub_nat` 부기뿐) |
| MIXED / LIST | 3 |

자식별 ITRS 재인코딩이 완벽히 작동해도 236개가 남는다. **그 길은 수익이 없다** — 위
2026-08-18 항목이 다른 이유로 도달한 결론과 같은 곳이다.

**대신, 미해결 자식을 실제로 열어 보니 원인은 우리 인코더에 있었다.** `$filter__id`의
자식은 교과서적으로 하강하는 순환인데 닫히지 않았다:

```
D_FILTER__ID(cons(X_h,X_t), cons(b_h,b_t)) -> U_233(b_h, k_233(cons(X_h,X_t), cons(b_h,b_t)))
U_233(true, k_233(cons(X_h,X_t), cons(b_h,b_t))) -> D_FILTER__ID(X_t, b_t)
```

size-change는 **인자 위치끼리** 비교한다. `cons(X_h,X_t)`가 만나는 상대는 그것을
*포함하는* `k_233(...)`이라 감소도 동등도 아니고 — **간선이 아예 기록되지 않는다.**
같은 순환을 캐리어만 바꿔 A/B로 세웠더니(`sweeps/children/carrier/`):

| 캐리어 | 닫은 프로세서 |
|---|---|
| `k_N` 래퍼 (기본) | `UsableRulesReductionPairsProof` — 정렬 탐색, SMT 호출 |
| 변수 평평하게 | **`QDPSizeChangeProof` — 사실상 공짜** |

그리고 `Program_ok`에서 `QDPSizeChangeProof`는 **성공한 유일한 프로세서**였다
(317/317). 즉 지금 형태가 모든 구조적 자식을 공짜 프로세서에서 밀어내고 있었다.

**한 변수 재측정** (budget 1200 동일, `Keep` 내보내기가 2026-08-18 기준선과 바이트
동일함을 확인 — 진짜 대조):

| | 자식 | 닫힘 | 미해결 |
|---|---|---|---|
| Keep | 577 | 317 | 260 |
| **Flat** | 577 | **502** | **75** |

`DependencyPairsProof`/`DependencyGraphProof`가 같은 577개를 만들었으니 분해는 그대로고,
502개 전부 `QDPSizeChangeProof`로 닫혔다.

**심볼 판정 수준에서는 33개 표본 중 1개 뒤집힘**(`$free_callableTypeIR`
TIMEOUT→YES, `sweeps/carrier_{keep,flat}.tsv`). 저장된 열은 환경-민감(2026-08-12,
하루에 175/408 뒤집힘)이라 비교 대상이 못 되므로 **두 열을 연달아 재측정**했다. 다만
budget 20은 `Program_ok`이 DP 변환에만 6초를 쓰는 규모에 비해 작아, 이 표본에서는
캐리어 효과와 예산 굶주림이 분리되지 않는다. 효과가 드러나는 지표는 자식 개수 쪽이다.
`lift_guards`와 같은 모양이다 — 앞단은 크게 뚫리고 심볼 판정은 거의 그대로.

**구현.** `Unravel.carrier = Keep | Flat`, 기본 `Keep`(153/153 스윕과 골든 TRS가
측정된 형태). CLI는 `termination --flat-carrier`. 골든·유닛테스트 통과.
`unravel.mli`의 기존 정당화가 CLAUDE.md가 2026-07-20에 **철회한** 주장("MTT가 부분항
관계를 뒤집는다")에 기대고 있어 함께 고쳤다 — MTT는 언레이블링을 하지 않는다.

**두 형태 모두 건전하다.** Flat의 소비자 `u(pat, x1..xn)`은 생산자가 만드는 것보다 *더
많은* 항에 맞으므로(원 인자의 패턴 구조와 비선형성이 강제되지 않는다) 과대근사이고,
정렬 삭제와 같은 안전한 방향이다: TRS 종료 ⟹ CTRS 종료.

**남은 75개는 이제 목록이다** (`sweeps/children/program_ok_flat_full.out.children.tsv`):
`$reduce_serenum_*_compat_*` 18, `$find_overloadeds_*` 10, `subty_*` 상호재귀,
`$cast_*`. 260개의 안개가 아니라 식별 가능한 소수 가족.

> **정정(2026-08-20). 아래 "잔여는 예산이 아니다"와 "의미론적 불변량이라 못 옮긴다"는
> 둘 다 틀렸다.** 근거였던 "예산 3배에 5개"는 *577개가 하나의 마감을 나눠 쓰는* 상황의
> 관측이었다. 자식을 꺼내 **예산을 혼자 쓰게** 하면 미해결 75개 중 **48개가 닫힌다**
> (중앙값 37초, 최대 73초 — 그나마 대부분이 4MB 파싱 시간이다). tree의 "미해결"은
> "어렵다"가 아니라 "도달하지 못했다"일 수 있고, tree만으로는 구분되지 않는다.
> 내가 지목했던 `$reduce_serenum_*`(의미론적 불변량이라 인코딩으로 못 옮긴다고 썼다)도
> 단독 55초에 `QDPOrderProof`로 닫혔다. **도구가 실제로 무엇을 내놓는지 먼저 볼 것** —
> MTT 때와 같은 실수를, 이번엔 tree를 obligation 대신 읽어서 반복했다.
> 측정: `sweeps/children/standalone_300.tsv`, `sweeps/run_children_standalone.sh`.

**그리고 그 잔여는 예산이 아니다.** 같은 Flat 표면에 예산을 3배(1200→3600) 준 재측정
(`sweeps/program_ok_flat3600.tsv`, `sweeps/proof_flat3600/`)은 닫힘 502→**507**,
미해결 75→**70**. 3배를 주고 5개다. 그리고 3600초 내내 성공한 프로세서는 여전히
`QDPSizeChangeProof` 하나뿐 — 정렬 탐색은 단 한 번도 성립하지 않았다. Keep에서
60초 vs 1200초가 316 vs 317이었던 것과 같은 모양이다.

**잔여의 공통 원인: 재귀 인자가 정의된 호출의 결과다.** 75개의 의존쌍을 읽으면
**57개**가 이 모양이다 — `$itermap`이 컬렉션을 다시 쌓거나(`$find_non_overloadeds`),
`$unroll_typeIR`이 typedef를 펼친 결과(`$reduce_serenum_*`)를 재귀 인자로 넘긴다:

```
D_FIND_NON_OVERLOADEDS(variant_set{cons(pair(k,v), rest)}, f)
  -> D_FIND_NON_OVERLOADEDS(variant_set{d_itermap_...(rest)}, f)
```

구문적 하강이 아예 없으므로 size-change는 **구조적으로 불가능**하고, 정렬 쌍으로 닫으려면
호출되는 함수가 크기를 늘리지 않는다는 것까지 usable rules 안에서 증명해야 한다.
`lift_guards`가 고친 것과 같은 계열이지만(사실이 정의된 심볼 뒤에 숨는다) 숨은 것이
비교 하나가 아니라 함수 전체의 크기 거동이라, 인코더 수준의 값싼 수는 닿지 않는다.
`$reduce_serenum_*`의 경우 종료성은 애초에 "typedef가 무한히 펼쳐지지 않는다"는
명세의 의미론적 불변량에 달려 있다 — 구문적 하강으로 옮길 수 있는 성질이 아니다.

나머지 18개는 `subty_*` 상호재귀로, 인자는 순수 생성자이고 하강도 보인다
(`SUBTY_NAMEDTYPE(variant_specializedType(x0,x1)) -> SUBTY_TYPEARGUMENTLIST(x1)`).
왜 닫히지 않는지는 아직 안 봤다 — SCC가 커서인지 특정 간선 때문인지.

**교훈.** 캐리어 선택은 2026-07-19에 "작은 슬라이스에서 둘 다 YES, 0–1초"로 측정하고
*무관하다*고 결론냈던 항목이다. 그 측정은 틀리지 않았다 — 작은 슬라이스에서는 정렬
탐색을 감당할 수 있으니까. 틀린 건 **규모에 무관하게 무관하다**는 일반화였다. 자식이
577개가 되면 어느 프로세서가 닫느냐가 전부다. *동등한 두 인코딩*이라는 판단은 그것을
소비하는 도구의 예산 안에서만 유효하다.

### 2026-08-18 — 스윕이 같은 질문을 여러 번 묻고 있었다 (슬라이스 dedup)

**한 호출그래프 SCC의 심볼들은 서로에게 도달하므로 정확히 같은 규칙 집합에 도달하고,
따라서 슬라이스가 동일합니다.** 추정이 아니라 확인했습니다 — `Cast_expl`과
`Cast_expl_neq`의 방출 ITRS는 4,166줄 **바이트 동일**입니다. 세 검사(confluence,
termination, SCC)는 전부 슬라이스만의 함수이므로, 두 번째 실행은 첫 번째의 답을
다시 유도할 뿐입니다.

기존 스윕에 그대로 남아 있던 낭비:

| TSV | 행 | 서로 다른 슬라이스 |
|---|---|---|
| `sweeps/term_all20_w0fix_clean.tsv` | 650 | 429 |
| `sweeps/itrs227.tsv` | 227 | **136** |

중복 그룹이 정확히 상호재귀 계열입니다 — `*_inst` 28개, `*_ok`(문장) 16개,
`$cast_*` 13개, `Expr_ok`/`Type_ok` 계열 8개. itrs227 기준 **총 15.4h 중 6.3h(41%)**가
같은 파일 재풀이였고, 미증명 잔여는 158이 아니라 실제로 **91**입니다.

**구현.** `Analysis_sweep.rows`에 `~key` 인자를 추가하고
(`bin/main.ml`의 `slice_key`/`slice_digest`가 슬라이스 다이제스트를 줍니다),
같은 키의 두 번째 심볼부터는 대표 행의 열을 그대로 돌려주고 **마지막 열에
`=<대표심볼>`을 덧붙입니다**. confluence는 정규화 사다리 때문에 자체 루프를
쓰므로 `slices` 구성 시점에서 같은 방식으로 접습니다.

- 마커는 **덧붙이는** 것이지 치환이 아닙니다. 기존 소비자의 `cut -f2`(판정)·
  `cut -f3`(통계)는 그대로 동작하고, 벽시계를 합산하는 쪽만 마킹된 행을 빼면 됩니다
  (복사 행의 초는 **대표의 것**이고 두 번째 실행은 존재하지 않았습니다).
- `--out` resume에서도 유지됩니다: 파일에 이미 있는 행에서 메모를 시드합니다.
  단 **현재 스윕의 roots에 있는 심볼만** 시드합니다 — 스펙이 더 이상 정의하지 않는
  심볼은 빈 슬라이스로 키가 잡혀 서로 전부 충돌합니다.
- `owise`는 다이제스트에 명시적으로 넣습니다(`string_of_rule`이 떨어뜨림).
- 다이제스트는 통계 서명보다 정확합니다: `$cast_header`/`$cast_struct`는
  `eqs=1194 rules=1196`으로 같지만 vars가 19/18로 달라 **합쳐지지 않습니다**.

**실측(2026-08-18).** termination 10심볼 → AProVE 3회, 8.0s(중복 제거 전 환산 ~47s).
resume 경로: 1행 기록된 TSV에 3심볼 추가 → AProVE 0회, 0.8s. confluence 5심볼 →
`$cast_enum`/`$cast_error`가 `$cast_default`에서 복사(`$cast_struct` 행은 바깥
`timeout 540`에 잘려 미확인 — dedup이 아니라 측정이 중단된 것).

**이건 모듈화가 아닙니다.** 같은 시스템을 한 번만 푸는 것이라 완전히 건전하지만,
`Program_ok`처럼 슬라이스가 곧 전체인 심볼에는 아무 도움이 안 됩니다. 진짜 분해는
아래 상대 종료 항목을 볼 것.

### 2026-08-18 — AProVE **증명 트리**를 버리지 않고 보관 (`--proof-dir`)

`Program_ok` 12시간 ITRS 실행이 `TIMEOUT 43,252.2s` 한 줄만 남기고 끝났다. 판정은
"이 예산에선 못 증명했다"만 말하고 **어디까지 갔는지**는 말하지 않는데, 그 정보는
AProVE가 이미 출력하고 있었고 `Aprove.check`가 파싱 후 버리고 있었다.

**무엇을 남기는가.** WST 출력은 `<판정>` / `proof of <파일>` / **증명 트리** /
`----------------------------------------` / 각 obligation 전문, 순이다. 남기는 건
트리까지다 — `(0) ITRS`, `(3) UsableRulesProof [EQUIVALENT, 0 ms]`, `(9) YES` 같은
단계 개요. 그 뒤 obligation 전문은 **우리가 넣은 시스템을 되돌려 찍는 것**이라
`--emit-trs` 산출물 이상을 주지 않는다.

**공짜다 — 그래서 플래그 뒤가 아니라 항상 붙는다.** 4.8MB `program_ok.itrs` 실행의
스트림에 타임스탬프를 찍으면 `KILLED`·트리·구분선이 **전부 65.00s 동일 시각**이고,
그 뒤 6.6MB의 obligation이 몇 분간 흐른다. 그래서 `done_when`을 "판정 줄"에서
"구분선"으로 옮겨도 초 단위 비용이 없고, 반대로 전문까지 기다렸다면 `Termination`의
`secs` 열이 통째로 오염됐을 것이다. `Aprove.check`는 `{verdict; proof}`를 돌려주고
`Termination.report`는 **답을 낸 rung의** 트리만 싣는다(`secs`와 같은 규칙 — 아래
rung들은 AProVE가 말할 것을 말하기 전에 잘렸다).

**첫 수확.** `Program_ok` ITRS의 트리는 예산 20·60 둘 다 **`(0) ITRS` 한 줄**이다.
의존쌍 변환(`ITRStoIDPProof`)조차 끝내지 못했다는 뜻이므로, 그 TIMEOUT은 순위함수
탐색의 실패가 **아니다**. 같은 자리에서 평문 TRS가 KILLED되면 트리는 어느 DP 하위
문제가 자기 `YES`로 닫혔고 어느 게 열려 있는지를 나열한다 — "AProVE가 모듈화를
하는가"에 대한 답이 실측으로 읽히는 자리이기도 하다.

**CLI**: `termination --proof-dir DIR` → `DIR/<symbol>.proof` (심볼의 `$` 등은 파일명
알파벳 밖이라 `_`로 바뀐다; `$f`만 앞에 `_`가 붙으므로 `f`와 겹치지 않는다).

**주의**: 트리의 `(9) YES`는 **하위 문제 하나**이지 실행의 판정이 아니다. 판정 스캔이
`proof of` 이전 헤더로 제한된 이유가 그것이고(2026-08-06 ⑶), 트리를 보관해도 그
제한은 그대로다.

### 2026-08-13 — decode가 종료성을 막는 **두 겹의 원인**을 실측으로 분리, mod 형태는 ITRS 인코더 몫으로

`$bitstr_to_int`의 `±2^w` 루프가 왜 폐포에 든 슬라이스를 전부 미증명으로 만드는지를
갈랐고, 그 해법(닫힌 식)을 **어디에 둘지**를 정했다.

**방침: `builtin.ml`은 건드리지 않는다.** builtin 규칙은 `numerics.ml`의 재귀 구조를
그대로 미러링하는 실행/참조 의미론이고, 그 자리를 바꾸면 실행면이 함께 바뀐다
(`bitstr_to_int`은 `delegated_in_native`에 없어 구조 규칙이 실행 모듈에도 방출된다 —
실측 확인). 대신 **ITRS 인코더가 decode/encode를 mod 기반 닫힌 식으로 정의**한다.
그 정의의 원본(태그 디스패치 단일 진입까지 포함한 OCaml)은
`sweeps/itrs-probe/closed-form-decode.patch`에 보관 — 값 등가는 파이썬 전수 대조로
확인했다(**49,276쌍, w=0..12, 불일치 0**; `w=8, n=600`처럼 루프가 2회 도는 입력 포함,
w=32/64 스팟 포함).

> **주의**: 닫힌 식만으론 `w=0`에서 **−1**이 나온다(`2^0=1`, 나머지 0, `half=0`,
> `0≥0` → `0−1`). 발산이 조용한 오답으로 바뀌는 것이라 `w ≤ 0 → 0` 가드가 필수다.
> `to_maude`의 native 위임(`B := ((2^W)−1) & N`)이 정확히 그 무가드 형태이며, 지금은
> 구조 규칙이 먼저 선언돼 이겨서 죽은 코드다 — **위임을 살리는 변경은 이 경계를 함께
> 고치지 않는 한 금지**.

**두 겹의 원인.** 실제 슬라이스(`$bin_ge` 208규칙)에 손으로 패치해 **현행 평문 TRS
경로**에서 한-변수로 갈랐다(ITRS 아님):

| 변형 | @60 | @300 |
|---|---|---|
| 원본 | KILLED | (기존 TIMEOUT) |
| up 분기만 삭제(재귀 유지) | KILLED | **KILLED 301s** |
| 닫힌 식만(진입 규칙 2개) | KILLED | **KILLED 301s** |
| 분기 병합만(재귀 유지) | KILLED | **KILLED 301s** |
| **닫힌 식 + 단일 진입** | **YES 5s** | — |

⑴ **두 보정 분기가 한 SCC**라 선형 순위함수가 없다(down은 `a>0`, up은 `a<0` 요구).
실제로는 교대 불가(down 후 `n' ≥ −h`, up 후 `n' < h`)지만 도구엔 안 보인다. down
분단독은 정수 인코딩에서 YES 0~1초.
⑵ **`Unravel`은 조건부 절마다 체인을 하나씩 내므로 절이 k개면 같은 lhs 진입 규칙이
k개**다. 이것만으로도 판정이 뒤집힌다 — 산술 177규칙 위 **비순환 5규칙** 실험에서
같은 lhs 둘이면 KILLED(재현 2회), 머리만 갈라도 **YES 4초**. 그래서 ITRS 인코더의
정의는 `shr`/`div_aux`의 태그 디스패치 idiom으로 **모든 심볼을 단일 진입**으로 쓴다.

**이득의 크기**(참고용 — builtin.ml을 임시로 바꿔 잰 값, 그 변경은 되돌렸다):
budget 20에서 소형 decode 26건 중 **YES 13 / TIMEOUT 13**(YES는 전부 4.3~5.5초).
남은 13은 decode 밖에 자기 루프가 있다 — `$bin_band/bor/bxor`(비트 분해),
`$bin_shr`(한 스텝씩 반감), `$write_value*`, `$set_priorities*`. 대형 decode 표본
10건은 전부 TIMEOUT 유지(761~14,551규칙: 독이 아니라 규모 문제). 회귀 표본 10/10
YES 유지, impty 골든 byte-identical, 오라클 스팟 12/12 MATCH.

**부수 확인**: 산술 라이브러리는 무죄다 — b층 145규칙 **YES 0초**, int 계층 177규칙
(`pow_int`/`div_int` 포함) **YES 4초**. "decode를 지우면 YES 4초"는 나머지가 쉽다는
뜻이 아니라 usable-rules 축소로 산술이 통째로 빠진 결과이며, int 계층 단독 측정이
그걸 갈라 준다. 이 두 값은 ITRS 이전 논증의 전제(무한 유도가 산술 안에만 머물 수
없어야 한다)를 **현재 표면에서** 처음 실측한 것이기도 하다.

**남은 검증**: `numerics.ml`의 w=0 수정(미커밋)이 들어간 뒤로 전체 코퍼스
differential과 termination 전수는 아직 재측정되지 않았다(RESULTS-2026-08-07 3부의
무효화 장부). 실행면 sha256 `175bf048…` 기준으로 둘 다 재실행 — 결과는 아래.

원자료·프로브: `sweeps/itrs-probe/` — 측정 산출물(`.itrs`/`.trs`/`.qdp`)은 이후
정리로 삭제됐고 `closed-form-decode.patch`만 남아 있다. 수치는 본문에 적힌 것이 전부.

### 2026-08-13 — w=0 수정 재검증: differential 클린, 그리고 **스윕이 자기 부하로 오답을 냈다**

RESULTS-2026-08-07 3부의 무효화 장부를 닫으려고 두 축을 다시 쟀다.

**differential (06:02–06:49, 실행면 `175bf048…`): 클린.** interp 1227 PASS / 341
FAIL, maude 1228 OK / 310 STUCK / 30 ERROR, completeness **0**, soundness **1**
(`issue1944.p4`), Phase D **1227/1227 MATCH** — 기준선 `516e8c89`와 전부 동일하다.
집계만 맞춘 게 아니라 다섯 TSV 모두 `diff <(cut -f1,2 …|sort)`가 **0줄**이라
상쇄 교환도 없다. `bit0-bmv2.p4`는 코퍼스에 있고 전후 모두 OK이며, 갈라지는 경로를
타는 건 손으로 쓴 증인 `bit0_add.p4`(코퍼스 밖)뿐이다.

**termination 1차 스윕은 폐기한다 — 측정이 자기 부하에 오염됐다.**
`YES 404 / TIMEOUT 244`로 기준선(415/233)보다 나빠 보였지만, 11건 전부 아티팩트다.

- 스윕 중 **고아 `minisat` 25개**(PPID 1, 각 ~94% CPU)가 06:56:54–07:13:13 사이에
  쌓여 32코어 중 **~24코어**를 남은 3시간 내내 점유했다. 시작 시 loadavg 2.67 →
  종료 시 27.40. 기준선은 loadavg ~3.5에서 잰 값이다.
- 11건의 새 시간이 전부 **21.9~23.3초** — 예산 20초를 갓 넘긴 자리다. 원래 시간은
  7.5~19.8초였으니 3배 감속이 판정을 뒤집은 것이고, 예산 기반 판정에서 이건 느린
  실행이 아니라 **틀린 답**이다. 양쪽 YES 404건의 평균 시간도 +1.25초 밀렸다.
- 깨끗한 기계에서 11건 재측정: **10건이 기준선 시간(±1.5초)으로 YES 복귀**
  (`sweeps/term_flips_recheck.tsv`). 남은 `Cast_impl`은 이 예산에서 동전던지기다 —
  같은 코드로 TIMEOUT 20.9 / YES 20.1 / YES 19.8, 예산 60에서 YES 16.5초.

**원인은 [subproc.ml](subproc.ml)의 kill 범위였다.** `cmd` 머리는 실제 도구로
`exec`하는 셸 래퍼라(`Aprove`의 `runme`가 java로 exec) `pid`는 java 자신이고 SIGKILL은
제대로 닿는다. 그런데 SIGKILL은 java에게 **자기 자식을 거둘 틈을 주지 않고**, AProVE의
z3/minisat 백엔드가 바로 그 자식이다. 이제 `setsid`로 자식을 새 세션에 넣고 프로세스
**그룹째** 죽인다. 한-변수 대조(같은 4개 산술 심볼, 같은 예산): 고아 **4 → 0**,
판정은 두 바이너리가 **완전 일치**.

> **교훈: 장시간 스윕은 자기가 만든 부하를 재게 된다.** 시작 시점의 loadavg만 적어
> 두면 이 오염은 로그에 흔적을 남기지 않는다. 예산 기반 판정을 쓰는 스윕은 **종료
> 시점의 부하와 남은 자식 프로세스 수**를 함께 기록해야 하고, 전이가 한 방향으로만
> (YES→TIMEOUT) 몰리면서 새 시간이 전부 예산 언저리에 모이면 표면이 아니라 기계를
> 먼저 의심해야 한다.

기운 컬럼은 한 번의 측정으로 오독되기 쉬우므로 10행을 끼워 넣지 않고 **650개를 고친
바이너리로 전부 다시** 잰다(`sweeps/run_term_w0fix_clean.sh` →
`sweeps/term_all20_w0fix_clean.tsv`). 1차 컬럼은
`sweeps/baseline/term_all20-w0fix-CONTAMINATED.tsv`로 보존. 비교는
`sweeps/term_transition.sh`.

### 2026-08-12 — 잔여 TIMEOUT 233의 전수 분류: **68%(159)는 실증된 실루프를 품어 YES가 수학적으로 불가**

재스윕(아래 항목)의 잔여 233을 구조로 갈랐다. 방법: 전 슬라이스 TRS에 decode
규칙(`d_bitstr_to_int`/`d_int_to_bitstr`) 포함 여부 스캔
(`sweeps/decode_membership.tsv`), 소형 41은 own-계층 규칙 직독, 실루프 후보는
**TRS 자신의 규칙만으로 innermost 시뮬레이션**해 증인 확보
([sweeps/witness/WITNESSES.md](../../../sweeps/witness/WITNESSES.md) ⑶⑷,
[simulate_w0.py](../../../sweeps/witness/simulate_w0.py)).

| 분류 | 건수 | 성격 |
|---|---|---|
| **decode-독** (w=0 진동을 폐포에 포함) | **157** (소형 26 + 대형 131) | SN 거짓 — **어떤 예산에도 YES 불가**. `Program_ok` 포함: 12h 런은 예산 부족이 아니라 목표 불능이었다 |
| **빈-패턴 실루프** (`$strip_prefix_rec`/`$strip_suffix_rec`) | **2** | `prefix=""`에서 가드 항진·스트립 항등 → 길이 1 순환(증인 ⑷). 0-경계 교훈의 텍스트판 |
| **arith-blind** (이진 자릿수 산술 속 측도) | **13** | `sub(n,1)`/`add(n,1)`-toward-bound/`div` 하강 — 실제 종료인데 측도가 digit 인코딩에 묻힘. 평평한 300초 재질의로 판별 (`term_arith13_300.tsv`) |
| **대형 clean** | **61** | 알려진 독 없음, >500규칙 — 유일하게 예산/분해가 의미 있는 구간 |

**스윕 정책 귀결**: decode-독 157 + strip 2는 w=0/빈-패턴이 스펙에서 해소되기
전까지 **재질의 자체가 낭비다**(159 × ~25s ≈ 66분/스윕). 표의 TIMEOUT 셀은 도구
출력 그대로 두되 해석을 이 분류로 달았다(verification.md 종합). w=0 항목(아래
2026-07-09)의 "규명 필요"가 이제 157슬라이스의 SN을 인질로 잡고 있으므로 우선순위가
올라갔다 — 해소되면(가드 추가 시 재번역, 도달불가 판명 시 그대로) 157건이 전부
재질의 가능해진다.

### 2026-08-12 — **termination TIMEOUT 열의 환경 연성: 175/408이 하루 만에 YES로**

시간 감축 논의용 프로브가 우연히 찾아냈다. `$update_headerUnion`(151규칙, TRS 스탯
동일)의 행적: 08-07 예산 300에서 MAYBE(279.8s) → 08-10 세 스윕 모두 TIMEOUT(~21s)
→ **08-12 예산 5에서 1초 YES**. 한-변수 반증: minisat2(08-12 유일한 설치)는
제거해도 1초 YES, aprove.jar·java·z3·스펙·번역 경로 전부 불변. 크기대별 10개
스팟체크에서 5개 플립(1,020~1,404규칙이 2~3초) → 650 전수 재스윕(동일 조건, 환경
지문 스탬프: nproc=32/가용 108GB/loadavg 3.5~5.7)으로 확정:

- **YES 240 → 415 (TIMEOUT→YES 175), YES→비YES 회귀 0, NO/ERROR 0.** 2h33m.
- 플립 중앙값 1,221규칙, 최대 `Cast_impl` 3,860(19.8s). 건강한 AProVE는 수천 규칙도
  수 초.
- 잔여 TIMEOUT 233 = 대형 폐포 192(>500규칙, 중앙값 5,674) + **소형 41**(≤500:
  `$strip_prefix_rec` 59, `$init_` 79, `$repeat__value` 81, `$fresh_typeIds` 84,
  `$strip_suffix_rec` 98…). 소형 잔여가 다음 의미론적 표적.
- 용의자는 당시 이 박스의 JVM 가용 자원(cgroup 배분 추정, 소급 검증 불가). **CRC도
  초 단위는 묻었다**: `ExternMethod_inst`가 기록 300.4s vs 오늘 ~61s(분리 deadline
  검증). 단 CRC의 판정 자체는 재현됨($bin_band ~300s 필요, $cast_binary 여전히
  hard).
- 무효화된 결론: 08-09의 "≤500 TIMEOUT은 300초 재질의에도 YES 0 → 예산이 아니라
  구조 문제" — 저하 환경에서 측정된 것. verification.md에서 걷어냄.
- **교훈: TIMEOUT은 시간 열처럼 환경 지문(nproc/메모리/부하/솔버 유무)과 함께
  기록해야 하는 판정이다.** 스윕 스크립트가 STATUS에 지문을 스탬프하는 규약 추가
  (`sweeps/run_term_resweep.sh`).
- 걸린 후속: ① CRC TIMEOUT 37 + `ExternMethods_inst`를 정직 예산(`--timeout 300`,
  분리 deadline)으로 재질의 — 일부 닫힐 수 있다. ② 소형 잔여 41의 증인/구조 분석.
  ③ 상대 종료성 분해(`SN(S)+SN(R/S)⟹SN(R∪S)`, AProVE가 TPDB `->=` 수용 확인,
  토이 YES)는 실슬라이스 프로브($bin_minus strict 13/상대 212)가 300초에 미착지 —
  RelADP 기계가 무거워 공짜 아님. 진짜 hard core가 확정된 지금 재프로브 가치 재평가.

### 2026-08-12 — CRC `--timeout`의 의미론 정직화: 로드/체크 deadline 분리

`43ed519d`가 예산 누수를 막으면서 심볼당 경로의 deadline을
`timeout + load_budget(240)` 하나로 접었는데, MFE 로드 실측이 **0.38s**(3회 일관)라
접힌 240초가 사실상 전부 체커에게 넘어갔다 — `--timeout 60` 행이 실제로는 ~300초를
검사했고, `crc_after_cap.tsv`의 YES 5건(`$bin_band`,`$name_annotationToken`,
`$set_priorities_of_tableEntryListIR`,`ExternMethod_inst`,`ExternMethods_inst`)이
60초를 넘겨 성립한, 이 부풀림에 의존한 판정이었다(그중 3건은 300.4s 천장에 밀착).

고침: `run_mfe`가 로드 완료 마커(`MFE> ` 프롬프트 — 배너 직후 정확히 한 번, EOF
홍수는 맨 `> `)를 관찰해 **체크 단계 시계를 마커 등장 시점부터** 돌린다. 로드는
`load_budget`이 따로 묶고(하드 실링 load_budget+timeout 유지), `check_batch`의 단일
슬라이스 위임은 `+ load_budget` 접기를 제거. 검증: `$cast_binary --timeout 30` →
TIMEOUT **30.8s**(전엔 사실상 270s), `$bin_band --timeout 60` → TIMEOUT 60.8s /
`--timeout 300` → YES 301.0s.

**스윕 규약이 바뀐다**: 기존 행들의 "60"은 사실상 300이었으므로, 판정을 보존하려면
앞으로의 CRC 스윕은 **`--timeout 300`을 명시**할 것. `$bin_band`는 판정이 와이어에
밀착(체크 ~300s 필요)이라 300 예산에서도 경계 위에 있다. TIMEOUT 행의 비용은 그대로
~300s — 시간 절감은 이 고침의 목적이 아니고(장부 정직화가 목적), 절감은 지문 carry
쪽 논의로.

### 2026-08-10 — `max_complement` 제거, 그리고 그게 드러낸 **부분 커버 드롭** (`ab950751`)

캡 두 개는 이름만 비슷하고 성격이 다르다.

- **`max_enumeration = 4096`은 남긴다.** 검사가 `tuple_product` **앞**에 있다 —
  곱집합을 통째로 실체화하고 튜플마다 형제 전체를 훑으므로, 이건 "값어치" 판단이
  아니라 **번역기 자신의 생존 한계**다. 비무항 case를 허용한 ⑽ 이후로는 두 자리만
  돼도 이미 네 자리 수(`typeIR × typeIR` = 1,156)다.
- **`max_complement = 16`은 지웠다.** 근거가 기록된 적 없는 크기 휴리스틱인데,
  실측은 반대 방향을 가리킨다: 분할은 or-gate의 matcher/투영 뒷받침과 그 전이
  폐포를 지우므로 대개 표면을 **줄인다**(⑽에서 변동 슬라이스 153개 중 152개 감소;
  `holds_` 분할은 2,313절을 내고 966줄만 늘었다).

**실측**: enumerate **30 → 36** (`$unroll_typeIR`·`$unroll_aliasType`·
`$is_valid_bitslice`·`$is_concrete_extern_object_prime`·
`$subexpressions_of_expressionIR`·`$gen_constraint`), 분석면 31,164 → **31,556줄
(+1.3%)**, 번역 1초. 실행면 sha256과 impty 골든 불변.

**폴백 사유를 찍게 했더니(이제 `complement_clauses`가 `Error reason`을 돌려준다)
캡이 막고 있던 건 정확히 그 6개뿐이었다.** 남은 38개 or-gate: `every position is
pass-through` 15 / `owise head is not distinct variables` 13 / `conditional owise`
8 / `a position does not enumerate` 2 — **캡 때문인 건 0건**.

**그리고 캡을 풀자 진짜 결함이 나왔다 — 부분 커버 드롭.** 튜플을 버리는 조건이
"무조건 형제가 하나라도 맞으면"이었는데, `tuple_matches`는 **최상위 생성자만** 본다.
`$f(K(zero))`가 조건 없이 튜플 `K`에 맞지만 `K(succ(x))`는 안 덮으므로, 버리면
`f`가 그 나머지에서 **정의를 잃는다**. 열거가 nullary 전용이던 시절엔 payload가
없어 생길 수 없었고, ⑽이 비무항을 허용한 뒤로 줄곧 살아 있었다 —
`$name_expression`이 18개 튜플 중 2개를 잃고 있었고, **바로 앞 650 스윕을 잰
표면에도 그대로 있었다.** 드롭 조건을 `total_cover`(payload가 전부 변수)로 좁히고,
부분 커버는 조건부 형제처럼 가드에서 부정한다.

두 효과는 절 수로 깨끗이 분리된다: 기존 30개 중 절 수가 바뀐 건
`$name_expression` **16 → 18** 하나뿐(= 드롭 고침), 나머지는 전부 캡 제거분이다.

**재측정** (`--budget 20`, 직렬, 심볼당 프로세스): 650/650에서
YES→YES **240** / TIMEOUT→TIMEOUT 407 / DEGENERATE 2 / ERROR→TIMEOUT 1,
**회귀 0건**.

**교훈**: 근거 없는 상수는 결함을 가린다. 이 캡은 6개를 막는 대가로 그 6개 중
2개에 있던 부분 커버 드롭을 **안 보이게** 하고 있었고(세 번째는 캡 안쪽에 이미
있었다), 풀지 않았으면 계속 안 보였을 것이다.

### 2026-08-10 — **judgment reflection도 ⑽의 경우분할이 필요했다**: `holds_` or-gate의 루프 2종 제거

> `b1ca0af6`(masking 복원) 이후 처음으로 termination 열을 전수로 잰 결과가 이걸
> 드러냈다. 650심볼 중 **NO 23건**, 전부 2,489규칙 이상의 alpha/cast/constraint/
> call-convention 계열이고 초가 11.0~11.1과 21~23에 뭉쳐 있었다 — 하나의 루프가
> 의존 폐포를 타고 번진 모양이다. NO는 unravel의 정렬 과대근사 때문일 수도 있으므로
> **증인을 봐야** 했고, 봤더니 과대근사가 아니었다.

**⑴ 증인 — `holds_<R>`의 or-gate가 ⑺과 같은 모양이었다.** `ParameterType_alpha`
(2,489규칙, 11초)의 AProVE 서사:

```
(45) NonLoopProof (COMPLETE)
By Theorem 8 [NONLOOP] we deduce infiniteness of the QDP.
  σ' = [x1 / proj_variant_typedefTypeIR_TYPEDEF_2_1(x1)] on the rule
  HOLDS_TYPE_ALPHA(x0, x1) -> HOLDS_TYPE_ALPHA(x0, proj_variant_typedefTypeIR_TYPEDEF_2_1(x1))
```

`gen_rel_holds`는 `holds_<R>(x0..xn-1) = or(g_1..g_k)` **무조건 규칙 하나**를
낸다. 의도적이었다 — 주석이 "so it adds no critical pairs"라고 적고 있다. 그런데
그 하나의 lhs가 맨변수라, 가드가 형제의 부분항을 `proj_K_i(subject)`로 되찾는 순간
⑺과 **완전히 같은 모양**이 된다: 인자가 줄지 않고, lhs는 언제나 다시 맞는다.
⑽이 owise 경로에서 이걸 고칠 때 judgment reflection 경로는 손대지 않았고,
그래서 결함이 그대로 남아 있었다.

**⑵ 고침은 ⑽ 그대로, 극성만 반대.** `complement_clauses`에서 분할 기계를 떼어내
공유 함수로 만들고(`enum_positions`/`split_slots`/`tuple_matches`/`clause_head`/
`split_guard`), `holds_`에 같은 것을 쓴다:

| | `complement_clauses` (owise) | `holds_split_clauses` |
|---|---|---|
| 절마다 | 덮는 형제의 가드를 **부정해 and** | 덮는 규칙의 가드를 **긍정해 or** |
| 아무도 안 덮는 튜플 | 순수 fall-through | `-> false` (전역성 유지) |
| 무조건 형제가 덮는 튜플 | 버림(owise 죽음) | 버리지 않음 |

사는 것도 ⑽과 같다 — **맨변수 lhs가 하나도 안 남는다.** `proj_TYPEDEF_1(x1)`이
head의 payload 변수 `x0-1`이 되어 진짜 하강이 되고, 갈린 절들의 head는 서로
단일화되지 않으므로 "임계쌍을 안 만든다"는 원래 목적도 그대로 유지된다.

**⑶ 자기재귀 검사로는 부족했다 — 측정이 그걸 보여줬다.** 처음엔 owise 경로의 기준
(`가드가 정의 중인 심볼을 부른다`)을 그대로 옮겼다. 결과는 `Type_alpha` 하나만 분할,
**NO 23 → 14건 해소, 9건 잔존**. 잔존분(`$merge_constraint_prime`, 3,333규칙)의 증인:

```
HOLDS_CAST_IMPL(x0, x1)     -> HOLDS_CAST_IMPL_NEQ($unroll_typeIR(x0), $unroll_typeIR(x1))
HOLDS_CAST_IMPL_NEQ(x0, x1) -> HOLDS_CAST_IMPL(proj_..._serializableEnumTypeIR_ENUM_3_1(x0), x1)
```

**상호재귀**다. 어느 규칙도 자기 자신을 언급하지 않으므로 자기재귀 검사에 안 걸린다.
기준을 "가드가 `holds_`를 하나라도 부르면"으로 넓히면 SCC 계산 없이 둘 다 덮인다 —
고리 안의 어느 반사도 맨변수 lhs를 남기지 않으면 고리는 닫힐 수 없기 때문이다.

**⑷ 열거가 안 되는 판단은 or-gate를 **유지**한다(버리지 않는다).** `Cast_impl`은
규칙이 `typeIR_a -> typeIR_b` 둘뿐이고 **양쪽 다 맨변수 head**라 나눌 게 없다.
owise 경로는 이럴 때 `Gate`로 반사를 포기하지만(owise 속성 하나 남기면 그만),
judgment reflection에서 포기하면 그 판단의 **부정 사용이 전부 좌초되고** 그걸 부르는
반사들로 연쇄된다. 그래서 or-gate를 남긴다 — 고리는 상대편(`Cast_impl_neq`)이
갈리면서 이미 끊긴다.

**⑸ 실측** (`--budget 20`, 심볼당 AProVE 별도 프로세스):

| | 자기재귀만 | `holds_` 호출 전체 |
|---|---|---|
| 분할된 판단 | `Type_alpha`(1156절) | + `Cast_impl_neq`(1156절), `ParameterType_alpha`(1절) |
| NO 23건 재측정 | TIMEOUT 14 / **NO 9** | **TIMEOUT 23 / NO 0** |
| 분석면 | 30,198줄 | 31,164줄 |

불변: impty 골든, **실행면 sha256 `516e8c89…5652e`**(`Reflect`는 분석 전용),
owise 지문 `44 reflected, 30 complement-enumerated, 0 kept`.
`holds-Type-alpha` 규칙 안의 `proj-…` 출현은 34개 계열 → **0개**.

TIMEOUT은 미증명이지 결함이 아니다 — ⑽에서 `$find_name_annotation_opt`가
NO(6.1초)→MAYBE(227.7초)로 간 것과 같은 방향이다.

**교훈** — 2026-08-09의 "판정이 무엇에 대한 것인지도 보라"의 짝: **한 곳에서 고친
결함은 같은 기계를 쓰는 다른 곳에도 있다고 가정하라.** ⑽의 주석은 분할이 사는 것을
"맨변수 lhs가 안 남는다"로 정확히 적어놨는데, 바로 아래 `gen_rel_holds`가 "critical
pair를 안 만들려고" 맨변수 lhs 하나를 일부러 내고 있었다. 그리고 **기준은 증인이
정한다** — 자기재귀는 그럴듯했지만 상호재귀 증인 하나가 그걸 반증했다.

### 2026-08-09 — **termination 증명이 규칙의 41.6%를 지운 시스템에 대한 것이었다** (`b1ca0af6`), 그리고 사다리가 사는 게 없다는 것

> 2026-08-06의 재측정을 이어가다 `Program_ok`에 12시간을 걸기 직전에 나왔다. 파서
> 결함(`bae6e096`)이 **판정을 잘못 읽은** 것이었다면, 이건 **판정의 대상이 우리가
> 의미하는 시스템이 아니었다**는 것이다. 둘은 독립이고, `verification.md`의
> termination 열은 두 겹으로 무효였다.

**⑴ 무엇이 가려졌나.** `Crc_surface.fold_premise_binders`는 **죽은 binder**
(`$g(A) = v`인데 `v`를 아무도 안 씀)를 `isStuckHead($g(A)) = false`로 바꾼다. 이건
`isStuckHead`가 **정의된 곳에서만** 같은 말이다 — 실행면은 정의하지만(`To_maude`),
분석면은 어느 쪽도 정의하지 않는다(`To_mfe`는 `full_maude = false`일 때만 등식을
방출하고, CTRS에는 규칙이 아예 없어 `Maude_sorts`가 generic `Val -> Val`로 복원한다).

unravel하면 그 가드는 **아무것도 발화시킬 수 없는** 소비자가 된다:

```
f(p)                 -> u1(isStuckHead($g(A)), <carried>)
u1(false, <carried>) -> r          -- isStuckHead를 false로 보내는 규칙이 없다
```

`r`이 생성되지 않으므로 **`r` 안의 재귀가 TRS에서 문법적으로 사라진다.** p4 전체에서
조건부 규칙 2,510개 중 **1,044개(41.6%)** 가 이 가드를 달고 있었다.

**⑵ 고침은 근사가 아니라 복원.** `Crc_surface.rebind_stuck_guards`가 이미 그 일을
하고 있었는데 **CRC 정규화 사다리에서만** 호출됐다. `Termination.trs_of_slice`를 두어
`Unravel` 직전에 태운다 — `$g(A) = <fresh>`, 즉 fold가 지운 그 binder다. 조건 수가
그대로라 헬퍼 수도 그대로고, 달라지는 건 소비자가 발화 가능해진다는 것뿐이다.
`--emit-trs`도 같은 함수를 쓰게 해서 디버깅 출력이 AProVE가 받는 것과 갈라지지 않게 했다.

**⑶ 실측 — YES의 14%가 masking에 기대고 있었다.** 이미 잰 254개를 rebind 후 재측정:

| | 수 |
|---|---|
| YES → YES | **180** (종료성 진짜) |
| **YES → non-YES** | **30 / 210 = 14%** |
| TIMEOUT/MAYBE → TIMEOUT | 42 |
| non-YES → YES | **0** (복원은 관계를 키우기만 하므로 당연) |

**예산 인공물이 아니다**: 30건 전부 원래 **1초 안팎의 YES**였고(가려진 규칙이 빠지면
남는 게 사소해진다), 원래 예산 300으로 되돌려 재질의해도 **TIMEOUT 26 / MAYBE 3 /
ERROR 1, YES 0건**이다. 계열도 일관된다 — `$add_typeDefs_t`·`$add_vars_i`·
`$find_non_overloaded_*`·`$invalidate_value`·`$replace_text*`·`$strip_prefix_rec`,
즉 **결과를 안 쓰는 호출**이 흔한 환경 갱신·검색·문자열 재귀다.

**⑷ 예산 사다리는 사는 게 없다.** 254개에서 **모든 YES가 2.0초 안에** 났다(1초 미만
183, 최대 `SwitchLabel_table_ok` 2.0초). **rung 5를 넘긴 YES는 0건**이고 2~10초 구간은
비어 있다 — 이봉 분포에 중간이 없다. 그런데 사다리는 non-YES마다 앞 rung(5+20+80=105초)을
매번 더 태운다.

| cap 300 스윕, 254심볼 | |
|---|---|
| 사다리(5,20,80,300) | 17,132초 = 4.8시간 |
| 평평한 300 한 번 | 12,722초 = 3.5시간 |
| 차이 | **+4,410초 (+35%), 바뀐 판정 0건** |

게다가 그 105초는 **장부에 안 잡힌다** — `Termination.check`의 `secs`는 답을 낸 실행만
잰다. TSV만 보고 비용을 분석하면 계속 과소평가한다.

비용의 본질은 사다리가 아니라 **`(못 푸는 심볼 수) × (예산)`** 이다: 4.8시간 중 3.5시간이
42개 non-YES의 예산 소진이고, YES 210건은 다 합쳐 **122초**다. 그래서 rebind 검증
스윕은 `--budget 20`(관측 상한의 10배)으로 254개를 **20분**에 끝냈다.

**교훈** — 2026-08-06의 "판정에 든 비용이 말이 되는지 보라"의 세 번째 짝:
**판정이 무엇에 대한 것인지도 보라.** 1초 만에 나오는 YES 30개가 정상으로 보였던 이유는
그것들이 사소한 시스템에 대한 정답이었기 때문이다. 비용이 너무 싸다는 것 자체가 신호였다.

### 2026-08-06 — 툴체인 복구 + 전체 differential 무회귀, 그리고 **termination 열 전체를 무효화한 판정 파서 결함** (`dd39e4fa`·`bae6e096`)

> 이 트리(`40074861`)에서 `spectec/tools/`(maude 3.5.1 · MFE master · AProVE
> `master_2026_02_15` · z3 5.0.0)를 설치하고 "미측정 부채"(2026-08-05 항목)를 갚으러
> 들어갔다가, **측정 도구 두 개가 모두 판정을 잘못 읽고 있었다**는 것을 발견했다.
> 번역은 무죄다 — 결함은 전부 판정을 읽는 쪽에 있었다.

**⑴ differential은 무회귀** (전체 corpus 1568, `40074861`에서 직접 측정).
completeness **0** / soundness **1**(알려진 `issue1944`) / Phase D
**1227/1227 MATCH · 0 MISMATCH**. `92618dc2` 기준선과 정확히 일치한다 — `eq` 경로를
통째로 갈아엎었는데(`5dd8f11d`·`1585e814`·`38ad855d`) 1,227개 프로그램의 타이핑
결과값이 하나도 달라지지 않았다. 2026-07-14 spot check에서 STUCK이던
`const.p4`·`issue1717.p4`도 둘 다 OK/MATCH로 재현되지 않는다.
비용도 같이 줄었다: 전체 45분 46초, Maude 단계가 1568개에 14분 29초(**≈0.55초/프로그램**,
종전 ~6.5초/프로그램). 슬라이스 재밴딩은 `sizes.tsv` — 650심볼 / 합 **1,277,055** /
≤500 **259** · 501–2490 **241** · >2490 **150**, 최대 슬라이스는
`Program_inst` **12,607**규칙(종전 61,345, −79%).

**⑵ MFE 브리지: 임계쌍 0개인 슬라이스를 ERROR로 읽고 있었다** (`dd39e4fa`).
CRC의 성공 텍스트는 두 갈래다(`makeCRCmessage(…, confluence)`, `mfe.maude:839`):
임계쌍이 전부 joined면 `The specification is locally-confluent.`, **하나도 없으면**
`There are no critical pairs.` + `The specification is confluent.`. `mfe.ml`이 앞엣것만
매칭해서 **더 강한** 판정을 `ERROR: could not find a Church-Rosser verdict`로 보고했다.
README에 적힌 스모크 테스트(`$lookup`)가 정확히 그 경우라 설치 직후 드러났다.
슬라이스가 −68% 줄어 임계쌍 0개 슬라이스가 훨씬 많아진 지금은 영향 범위가 이전과 다르다.

**⑶ AProVE 브리지: 포기한 실행을 종료증명으로 읽고 있었다** (`bae6e096`) — **이번
발견의 본체**. AProVE WST 출력은 `<판정>` / `proof of <파일>` / `<증명 서사>` 순이고
**1행이 그 실행의 판정**이다. 그런데 `verdict_line`이 버퍼 **전체**에서 "정확히 판정
토큰인 첫 줄"을 찾았다. 자기 마감에 걸린 실행은 1행에 `KILLED`를 찍는데 이건 토큰
목록에 없으므로 그냥 지나치고, 서사로 들어가 **하위 obligation의 `YES`**(증명 트리
`(7)` 노드 — `(6) QDPSizeChangeProof` 다음, `(8) Obligation:` 앞)를 슬라이스 전체의
판정으로 집어갔다. `done_when`이 이를 가중시켰다: 그 줄을 보는 순간 프로세스를 죽이므로
기록된 초가 "판정이 난 시각"이 아니라 "서사가 그 지점을 지난 시각"이 된다.

증명 방법은 저장소에 이미 적혀 있던 것 그대로다 — **도구가 실제로 받는 파일과 내는
출력을 가로챈다**(`--aprove-bin`에 래퍼). 사다리는 rung 20에서 멈췄고, 그 실행은
`+25s KILLED`를 찍은 뒤 `+120s`에 서사의 `YES`를 냈으며, 넘겨진 TRS는 `--emit-trs`
출력과 **바이트 단위로 동일**했다. (같은 TRS를 예산 300으로 직접 돌리면 312초 KILLED,
2회 재현.)

수정: 판정 스캔을 `proof of` 이전 **헤더로 제한**하고, `KILLED`를 마감 표지로 인식해
`Timeout`으로 보낸다(사다리가 다음 rung으로 오르는 그 의미이지 파스 `Error`가 아니다).
`done_when`도 헤더 완성에서 끝나므로 판정 후 몇 분씩 흐르던 서사 스트리밍이 사라진다.

**영향 범위는 이 세션을 넘는다.** 결함은 `025c7979`(2026-07-24, "end an AProVE run at
the verdict, not at JVM exit")에서 들어왔다. `verification.md`의 **현재 termination
열을 잰 tip `c10bde20`은 이 커밋을 포함**하므로 그 열(308측정 YES 307 / TIMEOUT 1)은
무효다.

> **정정 (2026-08-31).** 이 문단이 원래 "`bff805ec`(2026-07-23)·`5647b883`의
> '153/153 YES'는 결함 **이전**이라 영향받지 않는다"로 끝났다. **틀렸다. 둘 다
> 틀렸다.**
>
> ⑴ `025c7979`가 넣은 건 `done_when`이고, **버퍼 전체 스캔은 그 전부터 있었다.**
> 코드를 직접 보면 `git show bff805ec:spectec/lib/rewrite/aprove.ml`의
> `verdict_line`이 이미 `List.find_map`으로 출력 **전체**를 훑는다. `KILLED`는 토큰
> 목록에 없으니 지나치고 서사의 `YES`를 집는다 — `done_when` 없이도 같은 오독이다
> (그건 초를 추가로 망가뜨렸을 뿐이다). 결함은 `e518f5d4`(2026-07-22 04:10,
> `termination` 서브커맨드가 처음 들어온 커밋)에서 태어났고 `bff805ec`는 그
> **1시간 뒤**다(`git merge-base --is-ancestor e518f5d4 bff805ec` 확인). 즉 **이
> 브리지는 `bae6e096` 전까지 한 번도 판정을 옳게 읽은 적이 없다.**
>
> ⑵ `5647b883`에는 **`aprove.ml`이 아예 없다**(`git ls-tree`로 확인). 그 열은 구
> MTT 스택으로 잰 것이라 "영향 없음"은 맞지만 이유가 전혀 다르고, 이 파이프라인의
> 숫자와 **행 대조가 성립하지 않는다**.
>
> 이 틀린 문장의 대가: `RESULTS-2026-08-07.md` §5가 기준선의 34행이 마감에서
> 답하는 걸 보고도 "`bff805ec`는 결함 이전"을 믿고 판정을 보류했다. 2026-08-31
> bisect가 같은 34행을 다시 짚고 나서야 갈렸다(위 2026-08-31 항목).
>
> **교훈: "언제 들어왔나"는 커밋 메시지가 아니라 그 시점의 코드로 확인할 것.**
> `025c7979`의 제목이 그럴듯해서 결함의 출생지로 오인됐다.

**⑷ 재측정 부분 결과 — 오탐률 최소 28%.** 수정 후 `--all`을 350/650까지 돌린 시점의
분포(규칙 수 오름차순, 즉 **작은 슬라이스 쪽 350개**):

| | 수정 전 | 수정 후(350행) |
|---|---|---|
| YES | 350/350 | **251** (합 194초) |
| TIMEOUT | 0 | **89** (합 26,860초) |
| MAYBE | 0 | **6** (합 1,442초) |
| NO | 0 | **1** |
| ERROR | 0 | 1 |
| DEGENERATE | 2 | 2 |

크기로 걸러낼 수 없다는 게 중요하다: `$bin_eq`는 **159규칙**짜리인데 YES→TIMEOUT으로
뒤집혔다(`Program_inst` 12,607도 YES→TIMEOUT, 반면 `eq_annotation` 1,166은 0.6초 YES
그대로). 시간의 99%를 TIMEOUT이 먹는다 — 진짜 YES는 심볼당 0.6초, 뒤집힌 심볼은
사다리 5→20→80→300을 전부 올라 ~5분. 남은 300개가 전부 더 큰 슬라이스라 CRC를 먼저
돌리려고 350행에서 **의도적으로 중단**했다(`--out`이 재개 가능하므로 손실 없음).
eq/eqcase/tag 185개 스윕은 시작 전이다.

- **측정 confound 하나**: spy 래퍼가 파이프라인(`| while read`)이라 래퍼를 죽여도 JVM이
  남아, `-t 20` AProVE 하나가 이 350행 구간 내내(~11시간) 함께 돌았다. 32코어에
  AProVE가 ~2.2코어를 쓰므로 판정을 뒤집을 정도는 아닐 것으로 보지만, 이 구간의 초는
  그만큼 비관적으로 읽어야 한다. 래퍼를 다시 쓸 일이 있으면 `exec`로 붙이거나 프로세스
  그룹째 죽일 것.
- **`NO` 1건은 조사 대상**이다. unravel이 sort를 버리는 과대근사이므로 NO가 곧
  비종료 증거는 아니다(원 시스템이 아니라 과대근사의 성질일 수 있다) — 증인으로 확인할 것.

**⑸ `Program_inst`의 YES는 애초에 의미론적 주장이 될 수 없었다** (파서 버그와 **별개**의,
설계상 알려진 표면 차이). 슬라이스를 세어 보면: 조건부 규칙 2,255개 중 **966개(43%)**가
`isStuckHead` 가드에 달려 있는데 분석면에서 이 술어는 **정의 규칙이 0개**다(선언만
`op isStuckHead : Val -> Val`). 기약형이므로 `isStuckHead(t) = false`는 결코 성립하지
않고, unravel하면 `f(p) -> u1(isStuckHead(x), …)`는 항상 적용되지만
`u1(false, …) -> r`은 절대 발화하지 않는다 — 그 966개는 전부 막다른 길이 되고 그 아래
재귀는 TRS에서 문법적으로 사라진다. 같은 술어가 **실행면에는 2,229등식**으로 정의돼
있다. 게다가 이 슬라이스에는 **이미 알려진 미해결 비종료**인 `$bitstr_to_int`(148회)·
`$int_to_bitstr`(85회) w=0 루프가 들어 있다. 즉 분석면 YES는 *반례가 이미 알려진*
시스템에 대한 YES였다. 이 수치는 판정이 아니라 슬라이스를 센 것이라 파서 버그와 무관하게
유효하다.

- **남은 실험**: 가드를 되살린 TRS(`isStuckHead(x) -> true` / `-> false` 두 규칙 추가 =
  실행면 정의의 과대근사)로 재측정하면 판정이 masking에 **의존**하는지 직접 갈린다.
  입력은 만들어 뒀으나 아직 돌리지 않았다. `Program_inst`는 이제 TIMEOUT이라 대비가
  약해졌으니, 재측정에서 **진짜 YES가 난 큰 슬라이스 중 `isStuckHead` 의존도가 높은
  것**을 골라 거는 편이 낫다.

**⑹ CRC 스윕이 198행에서 죽었다 — 원인은 binder를 `=`로 내는 인코딩, 그리고 `Error`가
정규화 사다리에서 빠져 있는 배선.** `TypeParameterListOpt_ok`(**100규칙**)에서 MFE가
판정 없이 **SIGKILL**로 죽고 스윕 프로세스까지 함께 끝났다(cgroup OOM 이벤트 0 —
예산 초과가 아니라 검사기가 터진다). 단독 재현됨(ERROR, 143.6초, exit 1). 그 실행의
Maude 경고가 원인을 그대로 말한다 — `variable … is used before it is bound` **40건**:

```
ceq $add-typeDef-t(LOCAL, TC, typeId, typeDefIR)
  = upd-…( … $add-map-typeId(typeDefEnv, typeId, typeDefIR) … )
  if field-localTypingLayer-TYPE(field-typingContext-LOCAL(TC)) = typeDefEnv:Set
  /\ isStuckHead(…) = false /\ …
```

`typeDefEnv`는 lhs에 없고 **첫 조건이 묶는** 변수인데, Maude의 매칭 조건 `:=`가 아니라
**등식 조건 `=`**로 나간다. `=`는 양변이 이미 묶여 있어야 하므로 CRC는 자유변수가 든
joinability 의무로 바꿔 풀려 든다.

**이 인코딩 자체는 알려진 것이다** — CRC 정규화 절이 "CRC가 매칭 조건 `$f(A) = t`를
joinability 의무로 재인코딩하고 닫을 수 없는 determinacy 임계쌍을 만든다"고 적고 있고,
`:=`를 못 쓰는 것도 의도다(분석면이 `:=`/`=>`를 안 쓴다는 게 unravel의 전제다). 그래서
우회책이 `--crc-normalize`의 두 레버(inline / `crc_unravel`)인 것이다. **새로운 것은 두
가지다.**

- **대가가 문서가 적은 것보다 크다.** 문서는 비용을 MAYBE/TIMEOUT으로 적는데, 여기서는
  검사기가 죽고 **스윕 전체가 멈춘다**. 100규칙 슬라이스에서.
- **범위가 슬라이스 하나가 아니다** (정적 계수, Maude 불필요): 전체 등식 26,228 중
  조건부 2,495개, 그 가운데 **1,092개(43.8%)**가 binder를 `=`로 내며, 그런 조건이
  2,584개, 해당 head가 **332개**다. 상위는 `Expr_ok`(249)·`$bitacc_range_op`(147)·
  `Decl_ok`(137)·`Lvalue_ok`(108) — 핵심 타이핑 관계들이다.

**배선 결함**: `bin/main.ml`의 `inconclusive`가 `Maybe | Timeout`만 참으로 봐서
**`Error` 행은 정규화 사다리를 아예 못 탄다.** 정규화가 바로 이 binder를 없애는
장치인데, 그게 가장 필요한 — 검사기가 죽은 — 심볼이 정확히 재시도에서 빠진다.
`TypeParameterListOpt_ok`는 정규화를 한 번도 시도해보지 못하고 ERROR로 기록됐다.
**수정 방향**: `Error`를 `inconclusive`에 넣는다. 판정 전이가 upgrade-only라 YES를 잃을
수 없고, ERROR는 의미상 "다른 인코딩으로 다시 물어라"이지 판정이 아니므로 사다리의
취지에 맞는다. (스윕이 도는 중에는 `dune build`가 CRC의 벽시계 TIMEOUT과 경합하므로
빌드를 미루고, **스윕 완주 → 수정 → ERROR 행만 재시도** 순으로 간다.)

- 스윕 드라이버에 재시도 루프를 씌웠다(`--out`이 재개 가능하고 죽은 심볼의 행은 죽기
  **전에** 기록되므로 재시작이 그 심볼을 건너뛴다). 행을 못 남기고 죽는 슬라이스가 있으면
  무한 재시작이 되므로, **행이 늘지 않은 pass**를 감지해 멈추고 다음 용의자를 출력한다.

**교훈** — 2026-07-20의 MTT 교훈("도구가 실패하면 이론화하기 전에 그것이 실제로 내는
것을 손에 넣어라")이 이번엔 **성공**에 적용됐다. 실패는 의심을 부르지만 성공은 부르지
않아서, 이 결함은 YES를 650개 받는 동안 조용히 살아 있었다. 발견의 계기는 판정이 아니라
**비용의 불일치**였다 — "기준선에서 20,480초 rung에도 TIMEOUT이던 게 118초 YES"라는
값이 슬라이스 −80%만으로는 설명되지 않았다. **판정만 보지 말고 판정에 든 비용이 말이
되는지 보라.**

**⑺ owise 반사가 재귀 절을 만나면 분석면의 종료성을 깨뜨린다** —
`$find_name_annotation_opt`, term `NO`(6.1초 반례). **⑽에서 해소** — 아래 게이트는
증상만 막는 임시 조치였고, 진짜 고칠 자리는 owise를 경우별로 풀어 쓰는 쪽이었다.

`Reflect.owise`는 owise 속성을 `or(g_1,…,g_n) = false` 가드로 반사하고, 각 `g_i`를
**번역된 형제 규칙**에서 만든다 — 형제가 패턴에서 묶은 변수는 `proj_K_j(subject)` 투영이
된다. [4.3-annotation.spectec:127-142](../../specs/p4/4-p4-ir/4.3-annotation.spectec)의
4·5번 절은 재귀 호출을 **전제 안에** 둔다(`-- if eps = $find_name_annotation_opt(annotation)`).
형제 자기 자리에서는 lhs가 `variant-…-anon-2(xs, a)`이고 호출이 `a`라 하강이 구문적으로
보이지만, owise의 lhs는 catch-all이라 **변수 하나**뿐이므로 반사된 가드에서는 그 `a`가
투영으로 바뀐다:

```
ceq $find-name-annotation-opt(L) = none
 if or(…, and(and(match-annotationList-anon-2(L),
                  eqg($find-name-annotation-opt(proj-…-anon-2-1(L)), none)), …), …) = false .
```

`proj-…-1`은 해당 생성자로 만들어지지 않은 항에 정의 방정식이 없어 **막힌다**. 막힌 채로도
`op proj-…-anon-2-1 : Val -> Annotation` + `subsort Annotation < AnnotationList`이므로
`$find-name-annotation-opt`의 정상 인자이고, 어떤 특정 절과도 매치되지 않아 **owise가 또
발화**한다 → 가드가 자기 자신을 무한히 평가한다. **정렬을 넣어도 성립하므로 unravel이
정렬을 떨어뜨려 생기는 과대근사 인공물이 아니다** — 분석면의 실제 루프다. (종료성은 전략
무관 성질이라 "지연 평가면 안 부른다"는 반론도 통하지 않는다.)

실행면은 무사하다(owise를 속성으로 두므로 가드를 평가하지 않는다 — 이번 differential이
깨끗한 것과 일관). 그래도 문제인 이유는 **종료성이 CRC 합류성 판정의 전제**이기
때문이다(Newman). 반사된 가드가 그 전제를 부수면, 그 가드가 없애준 owise/형제 임계쌍보다
손해가 크다.

- **게이트**(`reflect.ml`, 미커밋): 가드가 정의 중인 심볼을 부르면 반사하지 않고 owise
  플래그를 유지 — 코드베이스에 이미 있는 `Gate` 관용구. 발동은 **정확히 1개**
  (`70 reflected, 3 complement-enumerated, 1 kept`; 단항 catch-all 206개 중 1개).
- **효과와 대가**: 슬라이스 930→901, `owise=1`, term `NO` → **`TIMEOUT`**(300초 상한 소진).
  루프는 사라졌지만 증명이 그 자리를 대신하진 않았다. 그리고 그 심볼은 owise를 단 채
  CRC로 가므로 반사가 애초에 막으려던 임계쌍이 돌아온다 — 합류성이 MAYBE로 떨어질 수 있다.
- ~~**complement-enumeration은 여기 못 쓴다**: 그 경로는 각 인자 자리가 전부 변수이거나
  **무항 생성자 열거**여야 하는데(`complement_clauses`), 이 형제들은 2항 생성자 + 조건
  (`$name(name) = "name"`)으로 갈리므로 패턴만으로 여집합을 못 쓴다.~~ **틀렸다 — ⑽ 참조.**
  못 쓰는 게 아니라 `nullary_enum`이 **무항으로 잠가둔 것**뿐이었고, 그 잠금을 푸는 게
  정답이었다. 조건으로 갈리는 형제는 애초에 문제가 아니다(그 경로는 조건부 형제를 덮는
  튜플에 각 형제의 가드를 **부정해서 붙이도록** 이미 설계돼 있다).
- **남는 설계 결함**: 재귀 절의 전제를 "전체 주어의 투영을 먹는 자기 호출"로 다시 쓰는
  순간 구조적 하강이 사라진다. 오늘은 1건이지만 원인은 그대로다. → ⑽에서 제거.

**⑻ 재측정 전량 재시작 (2026-08-06 06:19)**. ⑺의 게이트가 **분석면 자체를 바꾸므로**
그 전에 기록된 행과 이후 행은 서로 다른 이론에서 잰 값이 된다. `sizes.tsv` 재생성 결과
**13/650 슬라이스**가 달라졌다(문제의 심볼 930→901, 나머지 12개는 전부 큰 슬라이스가
−20〜−22: `Program_inst` 12,607→12,587, `Decl_ok`/`Decls_ok` −20, `Table*_ok` −20〜−22).
종전 CRC 227행·term 350행은 `stale_presurfacefix/`로 치우고 **한 표면에서 전량 재실행**한다.

> **두 번째 재시작 (11:41)**: ⑽이 게이트를 대체하면서 표면이 다시 바뀌었다(153/650 슬라이스).
> 이때 기록돼 있던 CRC 241행은 `stale_preowisesplit/`로 치우고 0/650부터 다시 돌린다.
> `sizes.tsv`도 ⑽ 표면의 것으로 교체했다(직전 것은 같은 디렉터리에 보관).

**⑼ CRC 스윕 하네스의 결함 둘 — 판정이 이웃에 의존하고, 비싼 행일수록 기록되지 않는다.**
재실행 4시간 뒤 "행이 226에서 안 움직인다"를 추적해 나왔다. 번역도, 판정 파서도 아니고
**스윕 배선**이다. (⑵⑶에 이어 세 번째로, 결함이 측정하는 쪽에 있었다.)

- **비싼 행이 기록되지 않는다.** `--crc-normalize`를 주면 `main.ml`이 inconclusive 행을
  `pending`에 담아두고 **배치 전체가 끝난 뒤에야** 사다리를 돌린다. YES만 스트리밍되므로
  `--out`의 재개 가능성이 **정확히 가장 비싼 행에서만** 무효다. 실측: 기록 226행의 시간
  합계 **18분** vs 경과 **249분** — 3시간 51분어치가 버퍼에만 있었고, 죽으면 전부 소멸한다.
- **판정이 앞 심볼의 운에 달려 있다.** `check_batch`는 Maude 세션 하나를 공유하고 TIMEOUT이
  나면 죽였다 **재생성**하는데, 재생성 직후 **첫 심볼만** `timeout + load_budget(240)`을
  받는다(Full Maude의 `load`가 첫 read에서 지연 실행되므로 그 자체는 옳다). 결과적으로
  같은 심볼이 자리에 따라 60초 또는 300초를 받는다. 226행 중 60초 초과는 셋뿐인데
  — `$bin_band` 145.7 · `$name_annotationToken` 86.1 ·
  `$set_priorities_of_tableEntryListIR` 65.9 — **셋 다 앞 심볼이 TIMEOUT나서 얻은 YES다.**
  다른 자리였으면 TIMEOUT이었다. 2026-07-23 기준선이 "심볼마다 새 프로세스, 직렬"로
  측정된 이유가 바로 이 분산인데, 배치 모드가 도로 들여왔다.

**수정: 코드가 아니라 예산으로 가른다 (3-pass, `run_crc.sh`).** 예산을 늘리는 것은
non-YES를 YES로만 바꿀 수 있고 그 역은 불가능하므로, 싼 예산의 1차는 **거짓 YES를 만들 수
없고 과소보고만 한다**. 뒤 pass가 균일 예산으로 그걸 걷어낸다.

| pass | 인코딩 | 예산 | 대상 | 출력 |
|---|---|---|---|---|
| 1 | plain CRC | 60s | 650 전부 | `crc_sweep.tsv` (모든 행 즉시 스트리밍) |
| 2 | plain CRC | 600s | 1의 non-YES | `crc_retry600.tsv` |
| 3 | `--crc-normalize` | 600s | 2의 non-YES | `crc_normalized.tsv` |

pass마다 **별도 TSV**여야 한다(`--out`은 이미 기록된 심볼을 건너뛰므로 같은 파일이면
재시도가 통째로 no-op이 된다). 기존 226행은 전부 plain `YES`(정규화가 돌지 않았다는 뜻)라
pass 1의 정의와 일치하므로 그대로 이어 쓴다.

**교훈** — ⑶의 "판정에 든 비용이 말이 되는지 보라"의 짝: **측정이 언제 파일에 도달하는지도
보라.** 여기서는 판정이 옳게 계산되고 있었는데도 4시간치가 프로세스 메모리 안에만 있었고,
바깥에서 보이는 증상은 "멈춘 것 같다"였다.

**⑽ ⑺의 진짜 해법 — 경우분할을 무항 생성자 밖으로 푼다 (`reflect.ml`, 2026-08-06)**

⑺의 게이트는 증상만 막았다(반사를 포기하고 owise를 남김 ⟹ 그 심볼의 CRC 판정을 버림).
`Reflect.owise`는 owise마다 **두 경로**를 순서대로 시도하는데:

1. `complement_clauses` — **경우분할**. 인자 자리를 `Enum`(생성자 열거)/`Pass`(전 형제가
   변수)로 분류하고, 곱집합의 튜플마다 ground head를 하나씩 찍는다. 무조건 형제가 덮는
   튜플은 owise가 죽었으니 안 찍고, 조건부 형제만 덮는 튜플엔 그 형제들의 가드를 **각각
   부정해 conjunction**으로 단다 — `or` 게이트가 없다.
2. 실패하면 `or(g_1,…,g_n) = false` **한 덩어리 게이트**. ⑺의 투영이 여기서 나온다.

1번이 `nullary_enum`(**모든 생성자가 0항인 variant**만 허용)에 잠겨 있었다.
`annotationList`는 평탄화하면 `_EMPTY`(0항) + `annotation`의 4개 case(2항) + `anon-2`(2항)
= 6개라 걸러져서 2번으로 떨어졌다. **잠금을 푸는 게 전부다** — `ctor_enum`이 arity를
같이 돌려주고, enum 자리의 head가 **fresh 변수를 payload로** 받고, 그 자리를 덮는 형제의
하위 패턴을 **주어의 투영이 아니라 그 fresh 변수에 대고** `ptest`한다.

핵심은 부분항 하강보다 강하다: **경우분할을 하면 그 심볼에 all-variable lhs가 아예 남지
않는다.** 그러면 가드가 정의 중인 심볼을 불러도 재진입할 절이 없다 — 루프가 구조적으로
불가능해진다. (게이트가 필요했던 이유가 사라진다.)

```
전  ceq $find-name-annotation-opt(L) = none
     if or(…, and(match-annotationList-anon-2(L),
                  eqg($f(proj-…-anon-2-1(L)), none)), …) = false .

후  ceq $find-name-annotation-opt(anon-2(-annotationList-0, -annotationList-1)) = none
     if and(eqg($f(-annotationList-1), none), …) = false
     /\ and(match-some($f(-annotationList-1)), …) = false .
```

**실측** (p4 전체):

| | 전 | 후 |
|---|---|---|
| owise 처리 | 70 or-게이트 / 3 경우분할 / **1 보류** | 44 or-게이트 / **30 경우분할** / **0 보류** |
| `$find_name_annotation_opt`의 `proj-` | 있음 | **0개** |
| term 판정 | `NO`(6.1초 반례) → 게이트 후 `TIMEOUT` | **`MAYBE`**(227.7초, `owise=0`) |
| 슬라이스 크기 | — | **153개 변동, 152개 감소**(증가는 이 심볼 901→910뿐) |
| impty 골든 | — | 무변화 |

`NO`가 사라진 게 핵심이다 — `NO`는 비종료 증인을 주장하는 판정이고 `MAYBE`는 그냥 미증명이다.
슬라이스가 152개나 줄어든 건 부수 효과가 아니라 같은 원인이다: all-variable owise 절 하나가
`or` 게이트의 matcher/투영 뒷받침과 그 전이 폐포를 통째로 끌고 들어오고 있었다.

`kept = 0`이므로 ⑺의 게이트는 p4에서 **휴면 상태**다. 코드에는 남겨둔다 — 열거 불가능한
타입(비-variant 인자 등)에 재귀 owise가 다시 생기면 똑같은 루프가 나고, 그때 `NO`가 아니라
보류로 떨어지는 편이 낫다.

**교훈**: 반사가 만들어낸 판정 문제를 "반사를 포기해서" 막으면 판정 자체를 잃는다. 여기서는
**이미 있던 더 좋은 경로가 필요 이상으로 좁게 잠겨 있었고**, 잠금 조건(무항)이 정작 그
경로가 필요로 하는 성질(head 서로소 + 가드가 guard-form)과 무관했다. **가드 조건이 실제
불변식에서 나온 것인지, 구현 편의에서 나온 것인지 구분하라.**

### 2026-08-05 — 【research note】 규칙 수 실측: 분석면 26,228 / 실행면 25,174, 그리고 "도달 불가 13,253"의 정체는 `Reflect`의 respell

> 도구(maude/MFE/AProVE) 없이 **번역만으로** 재는 값이라 이 트리(`38ad855d`)에서
> 그대로 측정된다. 위 항목의 "미측정 부채"와 달리 이 수치는 현재값이다.

**총량.** 원래 spec(elaborated IL) = `def` clause 1,547 + relation `rule` 700 =
**2,247**. 분석면(`rewrite --ctrs`) **26,228** eq(무조건 23,733 + `ceq` 2,495,
op 3,249 / sort 462), 실행면(`rewrite`) **25,174** eq.

분석면 26,228의 내역: `match_*` 12,373 / `subty_*` 7,310 / `$func` 3,094 /
relation 994 / `eqcase_*`+`tag_*`+`eq_<T>`+`eqg` 1,893 / `proj_*`+`field_*` 207 /
prelude 235 / `holds_*` 122. 사람이 쓴 규칙 자체는 clause 1,547 → `$func` 3,094
(2.0배), rule 700 → relation 994(1.4배)로 **1.8배**밖에 안 늘고, 나머지 84%는
`syntax` 선언 541개에서 전수 생성된 파생 술어다.

**도달성 — 두 표면이 정반대다.**

| | 분석면 | 실행면 |
|---|---|---|
| 총 eq | 26,228 | 25,174 |
| `Program_ok`에서 도달 | 11,981 | **25,174 (100%)** |
| `Program_inst`에서 도달 | 12,607 | 25,174 (100%) |
| 두 루트 합집합 | 12,607 (`Program_ok` ⊆ `Program_inst`) | 25,174 |
| `def_symbols` 650루트 전체 | 12,975 (49.5%) | — |
| 도달 불가 | 13,253 | **0** |

**실행면엔 죽은 규칙이 없다.** 분석면에서만 절반이 고아가 되는데, 원인은 스펙도
번역도 아니라 `Reflect`다 — judgment reflection이 조건의 `R(…)`을 `holds_R(…)`로
respell하고(`reflect.ml:1360`), `hoist_matchers`가 `match_K(v) = true`를
destructure 패턴으로 respell한다. 규칙은 남고 **호출부만 다른 형태로 대체**되므로
도달 폐포에서 떨어져 나간다. 같은 심볼의 정의/호출 수:

| 심볼 | 분석면 | 실행면 |
|---|---|---|
| `match_annotationToken_lt_lt_0` | 91 / **0** | 91 / 2 |
| `Type_alpha` | 50 / **0** | 27 / 22 |
| `Cast_impl` | 2 / **0** | 3 / 26 |
| `subty_stringLiteral` | 161 / 1 | 162 / 8 |

**확인할 것.**

- [ ] `confluence --all`이 도는 650루트가 분석면의 **49.5%**만 덮는다. 나머지
      13,253은 respell로 고아가 된 `match_`/`subty_` 테이블이고, 루트 목록에
      `subty_`는 0개·`match_`는 12개뿐이라 스윕 대상도 아니다. 실행에 쓰이는
      판별 로직 자체는 destructure 형태로 검사를 받으므로 "빠졌다"기보다
      **등가 형태로 검사된다**에 가깝지만, 커버리지 수치로 기록해 둔다. 고아
      테이블은 상수 패턴 대 상수 패턴이라 자기들끼리 겹칠 여지도 없다 —
      위험도 판단은 이 근거로 충분한지 재검토 필요.
- [ ] `Type_alpha`는 루트 목록에 있고 슬라이스 2,572이므로 스윕에서 검사된다
      (사각지대 아님). `Cast_impl`/`Cast_impl_neq` 계열도 같은지 확인.

**교훈 — 부분 문자열 grep으로 호출 수를 세지 말 것.** `grep "Type-alpha("`가
`holds-Type-alpha(`와 `ExternMethodType-alpha(`를 잡아 "호출 106회"라는 정반대
결론을 냈다. 실제로는 0회다. 심볼 호출을 셀 땐 단어 경계
(`(^|[^A-Za-z0-9'-])sym\(`)를 쓰고, 도달성은 텍스트가 아니라
`Rewrite_system.slice`(`--symbol`/`--sizes`)와 대조해 검증할 것 —
`Program_ok` 11,981 / `Program_inst` 12,607 / `$join_text` 13으로 교차 검증했다.

**재현.**

```bash
SPEC=$(find spectec/specs/p4 -name '*.spectec' | sort | tr '\n' ' ')
main.exe elab $SPEC | grep -cE '^\s*clause [0-9]+'   # 1547
main.exe elab $SPEC | grep -cE '^\s*rule '           # 700
main.exe rewrite --ctrs $SPEC | grep -cE '^\s*c?eq ' # 26228
main.exe rewrite $SPEC        | grep -cE '^\s*c?eq ' # 25174
main.exe rewrite --ctrs --list-symbols --sizes $SPEC # 650행, 최대 Program_inst 12607
```

### 2026-08-05 — 다형성을 번역 전에 없앤다: `Monomorphize` + 인스턴스별 `eq_<T>` (`1585e814`·`ffadcbc7`·이번 커밋)

> 타입별 `eq`(`5dd8f11d`)가 남긴 **분석면의 구멍**을 닫는 작업. 구멍의 정체는
> "타입 **파라미터**에서의 비교"였다: `$in_set<K>`의 몸통은 `K`에서 비교하는데 `K`엔
> `TypD`가 없으므로 `eq_pred`가 일반 `eq`로 떨어지고, 일반 `eq`의 off-diagonal은
> 분석면에 **없다**(실행면만 `owise`로 닫는다 — CRC엔 보여줄 수 없는 규칙이다).
> 따라서 그 아래로 내려간 조건은 CRC가 "성립 불가"를 못 보고, 규칙이 적은 쪽
> = YES가 쉬운 쪽으로 기운다.

**세 겹이었다.** ① 다형 **def**(`$in_set<K>` 등 18개), ② 다형 **타입**
(`set<K>`/`pair<K,V>`/`map<K,V>` — `eq_map → eq_set → eq_list_K → eq_K → eq`),
③ 스칼라/텍스트(정당 — 일반 `eq`가 그 정렬 위에서 완전하다).

- **①은 `Monomorphize`** (`translate/monomorphize.ml`, `Defunctionalize` 다음):
  호출지점의 `targ list`를 읽어 다형 `dec`을 인스턴스마다 복제한다. 타입 추론은
  하지 않는다 — 타입 인자는 elaboration이 이미 채워 뒀다. 특수화된 몸통이 스스로
  요구하는 다형 호출까지 **고정점**으로 따라간다. p4에서 123개 인스턴스화.
- **②는 `eq_pred`의 키를 타입 **이름**이 아니라 **표기 전체**로** 바꿔서
  (`typ_key` = `string_of_typ` 기반) 인스턴스마다 별도 `eq_<T>`를 만든다.
  정의 쪽은 `eq_rules_of_deftyp`로 떼어내, 다형 `TypD`는 `defs_of_typ`에서
  eq를 **내지 않고** `eq_helper_defs`가 인스턴스별로 치환해 낸다
  (`Monomorphize.subst_deftyp`). 인스턴스의 필드 타입도 다시 요구하므로
  `map<id,type>` → `set<pair<…>>` → `pair<…>` → `id`/`type`까지 닫힌다.

**결과(측정).** `eq_K`/`eq_list_K`/`mem_K`가 시스템에서 **사라졌다**. 일반 `eq`로
떨어지는 값 타입은 0(남은 `= eq(…)` 104곳은 전부 nat/int/bool/text 잎, 그리고
`syntax nameIR = text` 같은 별칭). 슬라이스 합 3,990,525 → 1,277,055(−68%),
>2490 밴드 171 → 150, 실행 모듈 ~29k줄. **레버 1(케이스 인덱스)과 합산한 수치**다.

**부수 결함 하나를 함께 고쳤다**(`ffadcbc7`): `bin/main.ml`이 Maude 표면들에
**변환 전** `spec_il`을 넘기고 있었다. `Defunctionalize`가 def을 개명하므로
`def_symbols`가 템플릿 이름(빈 슬라이스 7개)을 루트로 내놓고 특수화된 41개는
아예 스윕에서 빠져 있었다. `Pipeline.translated_spec`이 그 스펙에 이름을 주고,
`bin/`의 각 명령이 그걸 넘긴다. 시작항/인터프리터 경로는 원본 스펙 유지.

**미측정 부채.** 이 저장소 환경엔 `spectec/tools/`(maude·MFE·AProVE)가 **없다** —
CRC/termination/differential을 이 트리에서 재측정하지 못했다. `verification.md`의
모든 열과 `check_diff_p4*.tsv`는 이 세 커밋 이전 것이며, 특히 `def_symbols`가
개명·증가했으므로 per-symbol 표의 행 이름부터 다시 잡아야 한다. 도구가 있는
환경에서 ⑴ 전체 1,227 differential, ⑵ 619심볼 기준 CRC 스윕 순으로 재측정할 것.

### 2026-08-03 — 맨변수 바인더 세 갈래를 전부 착수 (`3a6794e7`·`7215c5dd`·`48a17692`)

> 2026-07-24 research note가 남긴 세 항목 — ① WLL로 inline/unravel 슬라이스별 선택,
> ② 죽은 바인더를 base 표면에서 제거, ③ 소스 와일드카드 전제를 접근자 관용구로 — 을
> 순서대로 구현하고, 각 단계마다 무작위 100개 differential로 무회귀를 확인했다.
> **세 갈래는 서로 다른 표면을 건드린다**: ①은 `--crc-normalize`(옵트인)만, ②는 base
> 분석 표면, ③은 명세 자체(따라서 실행 표면까지). 아래 무회귀 근거도 그에 맞춰 다르다.

**① WLL 검사로 unravel을 게이트 + 정규화 사다리 (`3a6794e7` → `c01dd14c`).**
`wll_violations`가 GNG IWC 2013 Thm 9의 삼중 전제 중 WLL을 슬라이스별로 검사한다.
reduction-soundness가 joinability-soundness를 함의하지 **않는다**(같은 논문 Ex. 3)는 것이
이 검사를 형식이 아니라 필수로 만든다. 검사는 `orient_conds`가 세운 (평가측, 패턴측)
분할을 그대로 읽고, 방향이 임의인 "양쪽 다 호출" 조건은 **양쪽 슬롯에 모두** 세어
과다보고 쪽으로만 틀린다. 실측: `specs/p4` 574심볼 중 **clean 400 / 위반 174**. 승격이
실제 일어난 6슬라이스와 CRC TIMEOUT 7개가 **전부 clean** — 반사만 하는 arm이 정확히
필요한 곳에서만 쓰인다. `crc_unravel`의 패턴 게이트도 맨변수까지 열었다(종전 주석
"맨변수는 fold의 몫"은 결정성 임계쌍만 보고 내린 판단이고 `=`→`=>` 재인코딩의 탐색
비용은 고려 밖이었다). subject 게이트(`1dd1e43a`)는 그대로라 `$join_text` over-unravel
회귀는 계속 막힌다.

**WLL이 사는 것은 unravel을 할 *권리*이지 inline을 대신할 더 싼 수단이 아니다** —
`3a6794e7`은 이걸 반대로 구현했고 측정이 그것을 반증했다(아래). 지금은 **사다리**다:
clean이면 `[inline+unravel, inline+rebind+unravel]`, 아니면 `[inline]` 한 단.
판정 전이가 upgrade-only이고 각 단이 **자기 힘으로** 승격을 정당화하므로 다음 단 시도는
건전하며 YES를 더할 수만 있다. `confluence`는 각 단마다 아직 미결인 심볼만 재검사하고
행에 어느 단이 정착시켰는지 붙인다(`YES (normalized:unravel|rebind|inline)`).

**측정이 두 번 예측을 뒤집었다.**

| arm (셋 다 task ② 포함) | `$write_bits_from_value` | `$write_value_*_prime` |
|---|---|---|
| unravel만 (note의 정적 지표 1위 = 최초 구현) | YES | **MAYBE ×5** |
| inline + unravel | TIMEOUT | YES |
| inline + rebind + unravel | YES | **MAYBE** |
| **사다리(위 둘)** | **YES** | **YES** |

- 1행: 2026-07-24 note가 조건 수·중첩쌍으로 C3를 1위로 꼽았는데, 승격 5개를 잃고 1개를 얻어
  **순손실 −4**였다. 정적 지표는 **순위를 매기되 결정하지 못한다**.
- 3행의 `rebind_stuck_guards`는 ②와의 상호작용을 푸는 장치다 — ②가 죽은 바인더를
  `isStuckHead($f(A)) = false`로 바꾸면 subject가 조건 안에 남아 unravel이 닿지 못하고,
  `$write_bits_from_value`는 정규화가 **완전한 no-op**(103등식 불변)이 되어 재시도 자체가
  건너뛰어진다. normalize 경로에서만(그리고 inline **뒤에** — 앞에 두면 inline이 가드를 다시
  만든다) 가드를 바인더로 되돌리면 106등식 + `crcu` 체인이 되고 YES가 된다.
- 결과: `$write_bits_from_value`가 기록된 `TIMEOUT (>2040s)`에서 **YES (normalized:rebind)**로,
  `$write_value*`는 YES\* 유지.
- **무회귀 근거**: 실행 모듈과 base 분석 표면 **둘 다 바이트 동일**(`specs/p4` 전량 대조).
  옵트인 경로만 바뀌므로 differential은 형식적 확인이다(seed 101, 100개: MATCH 77,
  Maude 거부 23 전부 interp도 FAIL → gap 0, MISMATCH 0).

**② 죽은 바인더 제거 (`7215c5dd`).** `$f(A) = v`에서 `v`가 아무 데도 안 쓰이면 그 조건이
말하는 것은 "`$f(A)`가 평가된다"뿐인데, 그걸 말하려고 **자유변수 하나**를 쓴다 —
CRC가 조건을 `=>`로 재인코딩할 때 매칭을 탐색으로 바꾸는 바로 그 부류다.
`fold_premise_binders`의 inline이 `uses >= 1`을 요구해 죽은 바인더는 전부 통과시키고
있었다. `uses = 0`도 발화하게 하면 치환은 no-op이고 조건만 사라지며, 그 자리에는
이 패스가 이미 쓰는 `isStuckHead(prod) = false`가 들어간다(= 사라진 조건이 실제로
검사하던 것). **upgrade-only 강등이 없다** — 모든 체커가 보는 표면에 그대로 적용된다.
- **sort 간극 확인 (research note의 미해결 항목).** 맨변수 매칭은 sort 검사도 겸한다.
  `specs/p4` 분석 표면 전수로 세었다: 죽은 바인더 **324개 중 320개**가 변수의 sort =
  producer의 선언된 result sort라 검사가 공허하다. 나머지 4개도 공허하되 이유가 다르다 —
  3개는 `$bitstr-to-int(..) = i:IntV`인데 그 op의 IL result type은 `int`인데도
  `Maude_sorts.signature`가 `Val`로 넓혀 선언했고(즉 잃는 것은 signature가 이미 잃은
  정밀도), 1개는 바로 앞 줄의 `subty_typedExpressionIR(..) = true` 가드와 중복이다.
- 표면 효과: 맨변수 바인더 조건 **1,326 → 1,001**(죽은 324 전부 소멸), 등식·op 수는 불변,
  574슬라이스 중 **119개 축소(−476규칙) / 17개 증가(+136규칙)**. 증가분은 owise 여집합이
  새 가드를 부정해야 해서 생긴다(`$lookup`이 `proj_cons_1`을 끌어오는 식).
- **무회귀 근거**: 실행 모듈 **바이트 동일**. differential(seed 102, 100개: MATCH 81,
  Maude 거부 19 전부 interp도 FAIL → gap 0, MISMATCH 0).

**③ 소스 29군데를 접근자 관용구로 (`48a17692`).** `-- if (_ _ typeIR _ _ = parameterIR)*`
같은 전제는 필드 하나를 얻으려고 레코드 전체를 분해하는데, 번역이 와일드카드를 전부 이름
바인더로 승격시키므로 위치마다 `$iterproj` 헬퍼(등식 2개)와 조건 1개가 생기고 이름 붙은
것 말고는 전부 죽는다. 같은 명세가 이미 쓰는 관용구(`4.2-ir-call-overload.spectec:58`)로
통일했다. `$id_of_parameterIR`는 있었고, `$direction_of_parameterIR`·
`$typeIR_of_parameterIR`·`$tableActionReferenceIR_of_tableActionIR`·`$name_of_parserState`
넷을 새로 뒀다. 관련 타입이 전부 단일 케이스라 분해가 실패할 수 없으므로 **철자 변경**이다.
(research note는 30군데라 했는데 실제로는 29군데다.)
- `$callableId_IR` **13규칙 → 4규칙**(note의 예측과 일치). 전체로는 139슬라이스가 −5,746규칙
  축소되고 **늘어난 슬라이스가 하나도 없다**; 맨변수 조건 1,001 → 970, 등식 72,504 → 72,395,
  op 3,139 → 3,093.
- **무회귀 근거 (명세 변경이라 여기가 제일 중요)**: (a) 인터프리터 전수 골든
  `p4-il-pos`/`p4-il-neg`가 **차이 0** — 1,568개 프로그램의 PASS/FAIL이 그대로다.
  (b) 엘라보레이트 렌더링에서 미묘한 자리 하나가 보존됨을 확인: `$gen_constraint`의 두 번째
  `direction`은 첫 번째에 대한 **검사**였고 지금도 검사다. (c) differential(seed 103, 100개:
  MATCH 75, Maude 거부 25 전부 interp도 FAIL → gap 0, MISMATCH 0).

**CRC 재측정 (9심볼, base 400초 상한, `c59788ad` tip).** 정보가 있는 심볼만 골랐다 —
나머지 8개(비트벡터 6 + `$callableId`/`$callableId_prime`)는 이미 TIMEOUT이 확정돼 있어
사다리 3단을 다 태워도 같은 답이 나올 뿐이다.

| 심볼 | 기록 | 재측정 |
|---|---|---|
| `$write_bits_from_value` | TIMEOUT (>2040s) | **YES\* (651.5s, rebind 단)** |
| `$write_value_field_from_bits'` | YES\* 261.7s | YES\* 268.8s (unravel) |
| `$write_value_fields_from_bits'` | YES\* 263.8s | YES\* 470.6s (unravel) |
| `$write_value_from_bits'` | YES\* 263.5s | YES\* 632.1s (unravel) |
| `$write_values_from_bits'` | YES\* 266.0s | YES\* 773.8s (unravel) |
| `$write_value_from_bits` | YES\* 278.1s | YES\* 957.3s (unravel-bare) |
| `$callableId_IR` | YES 43.9s | **YES 0.2s** |
| `$callableId_of_externConstructorPrototypeIR` | YES 89.0s | **YES 0.0s** |
| `$callableId_of_externMethodPrototypeIR` | YES 140.9s | **YES 0.1s** |

- **다운그레이드 0, 업그레이드 1.** `$write_bits_from_value`는 2026-07-24 note가
  "TIMEOUT 7개 중 유일하게 현행 수단이 무력"이라고 짚은 심볼이다.
- `$callableId*` 3심볼 합 **273.8초 → 0.3초**(note의 250.3→0.7 예측을 넘어선다).
- 초가 늘어난 것은 사다리가 여러 단을 무는 값이다 — upgrade-only라 잃는 것은 시간뿐이다.

**전수 differential (2026-08-03, `c59788ad` tip).** 코퍼스 1,568개 중 인터프리터 하네스가
스스로 제외하는 271개를 뺀 **1,297개**를 돌렸다. 분류 실패·UNKNOWN·TIMEOUT 0.

```
Maude 수용    1,047  → 골든도 전부 PASS. Phase D 1,047 MATCH / 0 MISMATCH / 0 decode error
Maude 거부      250  → 골든도 전부 FAIL (248 + 인터프리터 크래시 2건, Maude도 거부하므로 일치)
completeness gap  0
soundness gap     0
```

- **인터프리터 단계를 다시 돌리지 않았다.** `p4-il-pos`/`p4-il-neg` 전수 골든이 이미
  프로그램별 PASS/FAIL을 고정하고 있고 명세 변경 후에도 차이 0이므로, Maude 쪽만 배치로
  돌려 join했다. `check_diff_p4.sh` 런타임의 절반이 여기서 빠진다.
- **함정 1 — 제외 목록.** `testdata/interp/p4/excludes/**/*.exclude`의 286종 중 267개가
  코퍼스에 있는데, 이들은 골든에 판정이 없어 애초에 differential 결과를 못 내면서
  타입체커를 몇 분씩 붙잡는다(첫 시도에서 한 청크가 Maude를 띄우지도 못하고 11분 소진).
  `check_diff_p4.sh`도 이 목록을 반영하지 않으므로 같은 함정이 있다.
- **함정 2 — basename 키.** `action-bind.p4`는 `p4_16_samples`와 `p4_16_errors`에 **둘 다**
  있다. 골든을 basename으로 키잉하면 서로 다른 두 프로그램이 합쳐지고, 실제로 그 상태에서
  "soundness gap 9건"이라는 **허위 경보**가 나왔다. `<코퍼스>/<파일명>`으로 키잉해야 한다.
- 알려진 soundness gap `issue1944`가 제외 목록 안에 있어 이 1,297개 집계에서는 안 잡힌다.
  제외된 271개는 별도로 돌린다(골든 판정은 없지만 `--check-p4`가 인터프리터를 in-process로
  돌리므로 `interp FAILED` + Maude 환원 = soundness gap과 Phase D는 그대로 나온다).

**제외 271개도 돌렸다.** 골든 판정이 없어 completeness는 못 재지만 `--check-p4`가
인터프리터를 in-process로 돌리므로 soundness와 Phase D는 그대로 나온다.

```
Maude 수용  173  → Phase D 173 MATCH / 0 MISMATCH
Maude 거부   87
미측정       11  → soundness gap 0, Phase D 실패 0
```

**코퍼스 1,568개 합산: Phase D 1,220 MATCH / 0 MISMATCH, completeness gap 0(판정이 있는
1,297개 범위), soundness gap 0, 미측정 11.** 역사적 기준치 `92618dc2`의 Phase D 1227/1227과
견줄 만하다.

**미측정 11개는 예산 문제가 아니라 비종료다.** `issue1944`를 단독으로 900초 걸어 돌리면
stdout·stderr **둘 다 0바이트**로 그냥 돈다(배치 5,400초·solo 1,800초에서도 동일). 11개
전부 인터프리터 하네스의 제외 목록에 있고, 이 상태는 이번 변경 이전에도 같았다.
- **그래서 CLAUDE.md가 적어 온 "알려진 soundness gap 1건(issue1944)"은 재확인되지 않았다.**
  가설(미검증): 그 수치는 `check_diff_p4.sh`의 2단계 구조에서 나왔는데 — Phase A가
  `ITIMEOUT=300`으로 인터프리터를 돌려 **타임아웃을 FAIL로 기록**하고 Phase B가 Maude를 따로
  돌려 OK를 얻으면, 실제로는 "인터프리터가 답을 못 냈다"인 것이 "인터프리터가 거부했다"로
  분류된다. 제외 목록에 올라 있다는 사실과도 정합적이다. **확인 전에는 결론이 아니다** —
  gap이 진짜인지 분류 artifact인지 가르려면 인터프리터를 무제한으로 한 번 완주시켜야 한다.

### 2026-07-30 — confluence 전수 스윕을 2,490규칙에서 세웠다 (`c9b64e92`·`052938fb`)

**중단은 예산 부족이 아니라 수확 체감이다.** `orient_conds` 이후 재측정 스윕이 565심볼 중
**278**을 채웠다(YES 150 · YES\* 6 · **TIMEOUT 122**). 규칙 오름차순으로 돌기 때문에 남은
287심볼은 전부 대형 슬라이스이고 **중앙값이 52,394규칙**이다. 결정적인 관측은
**874규칙 위가 예외 없이 전부 TIMEOUT**이라는 것 — 886~1203 밴드는 예고대로 50/50 TIMEOUT이었고
(`c9b64e92`), 그 위도 같았다. 심볼당 base 2,040초 + 정규화까지 4,080초를 그대로 태우고
verdict가 없으므로, 남은 287을 같은 프로토콜로 도는 것은 **약 2주를 써서 `-`를 `TIMEOUT`으로
바꾸는 일**이다. 그래서 세웠다.

- **다운그레이드 0**: 재측정한 278 중 옛 판정을 잃은 심볼이 없다 →
  `orient_conds`는 측정된 범위에서 confluence-neutral(그 부채의 CRC 쪽은 이로써 닫힘).
- TIMEOUT 122의 성격은 둘로 갈린다 — 400규칙 이하 비트벡터 산술 7건과, 874~2,490규칙 구간에서
  예산을 소진한 115건. **둘 다 임계쌍 폭발이지 비합류 witness가 아니다.**
- **다음 수는 예산이 아니라 슬라이스 축소다.** 후보는 (a) 맨변수 바인더 제거(아래 2026-07-24
  research note — 측정된 비용의 98.6%가 여기 몰려 있고 `$callableId*`는 250.3초→0.7초),
  (b) 추가 pruning, (c) 모듈러 분해(산술을 블랙박스로; termination 쪽에서 이미 통한 수법).
  **(a)는 2026-08-03에 세 갈래 전부 완료**(위 절); (b)·(c)는 착수 전.
- term 스윕은 별도로 멈춰 있다(측정 308: YES 307 · TIMEOUT 1).

### 2026-07-29 — `$write_value_from_bits'` n_var=0 경계 수정 — ✅ 완료. 가드 **철자**가 슬라이스 크기를 지배한다

**결함(스펙 쪽).** `2.1.2-value-aux.spectec`의 varbit(`V`) 절 두 개가 `n_var = 0`에서 동시에
발화한다 — 폭 0 절(필드 유지)과 일반 절(0비트를 디코드해 덮어씀). 인터프리터는 절 순서로 첫
절을 골랐지만 그 순서는 CTRS에 없으므로, 번역하면 겹치는 등식 두 개가 된다(= `$write_value*`
CRC MAYBE 5의 뿌리). 일반 절에 가드를 복원해 순서가 지던 disambiguation을 명시했다.

**철자 선택이 결정적이다 — `=/=`는 값 우주 전체를 슬라이스로 끌어온다.** 처음 쓴
`-- if n_var =/= 0`은 nat 비교가 아니라 **일반 구조 등식 `eq`** 로 번역된다. `eq`의 정의
테이블(문자 9,025 + `variant-*` 전부, 합 50,629규칙)이 통째로 의존성 폐포에 들어와 슬라이스가
**271규칙 → 50,900규칙**으로 폭발했고, CRC는 5심볼 전부 outer 5400초에 verdict 없이 kill됐다
(종전 `YES* ~252초`에서 명백한 회귀). `-- if $(n_var > 0)`으로 바꾸면 산술 프리루드의 `leq`
하나만 쓰므로 슬라이스는 **271규칙 그대로**고, 조건은 `leq(n-var, bzero) = false`로 나와
형제 절의 head 패턴 `bzero`와 같은 subject의 true/false 극성 충돌이 된다 — `align_guards`가
이미 정규화해 두는 바로 그 형태라 CRC가 임계쌍을 discharge한다.

> **교훈: nat/int 위의 "≠ 상수" 가드는 `$(x > 0)` 같은 산술 비교로 쓴다.** `=/=`는 실행
> 표면에는 무해하지만(등식 하나) 분석 표면에서는 심볼 하나를 전체-시스템급 슬라이스로 만든다.
> 스펙을 고쳐 CRC를 돕는 작업은 **고친 뒤 슬라이스 크기(`rewrite --list-symbols --sizes`)를
> 반드시 다시 재라** — 판정을 기다리기 전에 5초면 회귀가 보인다.

**판정은 안 바뀐다 — 이 오버랩은 MAYBE의 원인이 아니었다.** 가드를 넣은 뒤에도 base CRC는
`$write_value_from_bits_prime`에서 여전히 **MAYBE(118.8초)**이고 `--crc-normalize`가 YES로
올린다(수정 전과 같은 `YES*`). 즉 다섯 MAYBE의 실제 원인은 tuple 바인더의 **결정성 임계쌍**
(아래 "CRC 정규화 패스" 절)이고, `n_var = 0` 오버랩은 그 위에 얹혀 있던 별개의 진짜
순서-의존 결함이었다. CLAUDE.md·todo가 "MAYBE 5의 뿌리"라고 적어 둔 부분은 이 측정으로
정정한다 — 고칠 가치는 여전하지만(순서가 지던 disambiguation을 CTRS가 잃는 건 실제 결함),
CRC 열이 좋아지리라 기대할 근거는 없었다.

**무회귀 검증**(`> 0` 판): varbit 프로그램 44개(p4_16_samples + p4_16_errors) 인터프리터 판정이
baseline TSV와 **44/44 일치**, `run --check-p4` 결과-VALUE **35/35 MATCH**(유일한 STUCK
`issue1879-bmv2.p4`는 baseline에서도 interp FAIL/Maude STUCK). CRC 재측정치는
[verification.md](../../../verification.md) 표.

### 2026-08-03 — `sanitize`의 밑줄 소실로 인한 심볼 합쳐짐 — ✅ 해소 (밑줄을 이름 문자로)

**수정.** `R.sanitize`의 `is_alnum`을 `is_name_char`로 바꿔 `_`를 **구분자가 아니라 이름
문자**로 다룬다(`mnemonic_of_char`의 버리는 목록에서도 제거). 선행 밑줄에는 `c_` 접두를
**붙이지 않는다** — Maude는 선두 `-` 연산자 이름을 받고(미니 모듈로 실측), TPDB 쪽은
`unravel.ml`의 `scrub`이 이미 `_` 선두를 식별자 선두로 허용한다. 숫자 선두 가드만 남는다.

**측정 (p4 두 표면 전량 대조).**
- **순수 개명이다.** 분석 표면의 eq 72,504 / ceq 2,441 / op 3,139가 **모두 불변**이고,
  이름을 옛 철자로 되돌리는 역변환을 걸면 잔차가 `$capture-avoiding` 2줄과 길이 상한에
  걸려 해시 접미가 바뀐 생성 헬퍼 이름들뿐이다. 합류성·종료성은 개명에 불변이므로
  [verification.md](../../../verification.md)의 판정은 **재측정 없이 이월**했다(같은 파일의
  "이름·규칙 수 갱신" 항목). 실물 확인: `$join_text`·`$invalidate_header`·`$is_some_`
  CRC 전부 YES, `$join_text`·`$is_some_` term YES.
- **중복 op 이름이 사라졌다**: 옛 표면은 `$capture-avoiding`이 이름 중복(서로 다른 함수
  둘), 새 표면은 arity 오버로드인 `tuple`만 남는다.
- **실행 표면에서 진짜 결함 두 개가 같이 고쳐졌다.** (a) `$capture_avoiding_`의
  `isStuckHead` 총체화 등식이 **아예 없었다**(합쳐진 5-arity 쪽에 가려짐) — eq 70,756→70,757의
  +1이 그것이다. (b) 그 함수의 gensym state 변수가 **다른 함수의 선언 sort를 받아
  `St0:Set`** 이었고 이제 `St0:Val`이다. 종전 항목이 "미규명 부작용"이라 적었던
  `Set List List Set`→all-`Val` 변화가 이것으로, **옛 좁은 sort 쪽이 틀린 것**이었다:
  gensym state가 붙은 op는 선언 arity와 어긋나 원래 전부 all-`Val`이다(`$subst_typeIR` 등
  대조 확인). **단 이 둘은 잠복 결함이다** — `$capture_avoiding_`는 스펙 전체에서 `dec`와
  `def` 두 줄이 전부이고 **호출자가 없다**(`grep`으로 확인). 충돌이 지금껏 differential에
  안 잡힌 이유가 이것이며(Maude는 arity로 op를 구분하므로 4인자 쪽 호출은 늘 옳게 풀렸다),
  따라서 이 두 수정은 **실행으로는 검증할 수 없다**. 검증의 무게는 개명 증명과 아래
  differential이 진다.
- **무작위 differential (2026-08-03, 시드 7, 150/1568 표본)**: Phase D **126/126 RESULT
  MATCH**(MISMATCH 0), completeness gap 0(Maude 거부 24건 전부 interp도 FAIL),
  soundness gap 0. 낡은 baseline TSV 대비 판정 차 14건은 전부 개선 방향(`OTHER`→`OK` 11,
  `ERROR`→`NOTRED` 3 — 후자는 `run` 대 `run --check-p4`의 출력 어휘 차)이고 **`OK`→비-`OK`는
  0건**이다. 분석 표면은 무작위 심볼 24개(CRC 12 + term 12)를 뽑아 `verification.md`
  기록치와 대조해 **24/24 판정 일치**(`$bitacc_offset_op`의 TIMEOUT 포함).
- 부수: p4 분석 표면 36,984줄·실행 표면 34,090줄이 개명으로 달라지고 impty/base 골든을
  갱신했다. 대부분은 선행 밑줄을 가진 **mixop 자리표시자**(`_BOOL`→`-BOOL`)에서 온다.

**남은 한계**: 니모닉 토큰과 리터럴이 만나는 자리(`a-b`와 `a_minus_b`)는 여전히 합쳐질 수
있다. 지금 스펙에는 그런 이름이 없고, `sanitize`의 주석이 이 한계를 명시한다.

<details><summary>2026-07-24 원래 진단 (기록 보존)</summary>

`R.sanitize`(`rewrite_system.ml`)는 `_`를 **토큰 구분자로 버리고** 토큰을 다시 `_`로
잇는다. 그래서 내부 밑줄은 우연히 복원되지만 **선행·후행·연속 밑줄은 소실**된다.
`To_ctrs.func_sym = "$" ^ sanitize`이므로 p4의 두 함수가 한 CTRS 심볼로 합쳐진다:

```
dec $capture_avoiding_(theta, typeParameterIR*, bound)         -- 3인자
dec $capture_avoiding (theta, typeId*, typeId*, bound)          -- 4인자
                    ↓ 둘 다 $capture_avoiding
op $capture-avoiding : Set List List Set -> Val .               -- 분석 표면에
op $capture-avoiding : Val Val Val Val Val -> Val .             -- 같은 이름 2회
```

**실측 범위** (`dec`/`relation` 574개 전수, 스크럽 재현):
- 실제 충돌은 **이 1쌍뿐**. `--list-symbols`가 574행이지만 고유 이름은 573.
- 후행 밑줄 이름은 30개(`$assoc_`·`$concat_`·`$filter_`·`$exists_`·`$forall_`·
  `$init_`·`$is_some_`·`$opt_as_seq_`·생성된 `$itermap-…` 등)인데 짝이 없어 충돌은 안 한다.

**수정을 시도했다가 되돌린 이유.** `_`를 이름 문자로 바꾸면(=`is_alnum`에 추가) 충돌은
해소되고 심볼 수가 573→574가 되지만, 밑줄을 보존하는 어떤 방식이든 위 30개 이름이 전부
바뀌어 **p4 분석 표면이 37,003줄 달라지고** impty 골든도 깨진다. 게다가 분리된 4인자 op의
sort가 `Set List List Set`→`Val Val Val Val`로 변하는 미규명 부작용이 관측됐다. 실익
1쌍에 비해 파급이 크고 모든 기존 측정의 재검증이 필요하므로, 이 커밋에서는 한계를
기록만 하고 코드는 손대지 않았다(`test_rewrite.ml`의 `sanitize/known-collision`이
현재 동작을 고정해 두므로, 고칠 때 뒤집을 실패 케이스가 이미 있다).

**고친다면**: 이름 보존(30개 변경 수용 + 골든·측정 재검증) 대 충돌 시에만 유일화
(`func_sym`을 중앙 인덱스 경유로 바꿔야 — `maude_sorts`/`builtin`/`spec_index`가 각각
독립 호출하므로 순수 함수로는 불가) 중 선택. 전자가 근본적이고, 후자는 이름이 인위적이 된다.

→ 전자로 갔다(위 2026-08-03 항목). 당시 셈한 "37,003줄"·"30개 이름"은 함수 이름만 센 것이라
과소평가였다 — 실제로는 선행 밑줄 mixop까지 바뀌어 분석 표면 36,984줄이 달라진다. 다만 그
전량이 **개명**임을 등식 수 불변 + 역변환 대조로 확인해서, 판정 재측정 없이 이월할 수 있었다.

</details>

### 2026-07-24 — refactor/rewrite-clean 통합 + termination 예산 사다리 + term 전수 재측정 — ✅ 완료

**통합 (`160fef97`, merge).** `refactor/rewrite-clean`(17커밋)을 `new-rewrite`에 merge로
합쳤다. rebase가 아니라 merge인 이유: (a) `verification.md`가 SHA로 앵커를 거는
`new-rewrite` 히스토리를 rewrite하면 측정 기록의 추적성이 깨진다, (b) `git merge-tree`
실측에서 충돌 파일이 `bin/main.ml`·`rewrite_system.ml` **2개뿐**인 반면 rebase는 17커밋을
차례로 replay하며 `e179ce0d`(rewrite_system 분할)·`6022a3a5`(스윕 이관)에서 정면 충돌한다.

해소 요지 — refactor의 레이아웃을 채택하고 그 위에 new-rewrite의 기능을 다시 앉혔다:
- `Rewrite_system`은 refactor의 슬림판(검사기 패스는 `crc_surface`/`scc_surface`) + `slice`
  뒤에 slicer 인덱스(`type slicer`/`make_slicer`/`slice_with`) 재삽입. slicer는 검사기 패스가
  아니라 `slice`의 인덱스형 변종이므로 데이터 모델에 남는 게 맞다.
- `main.ml`의 스윕 골격은 `Cli.Analysis_sweep`(`require_roots`/`roots`/`rows`/
  `recorded_symbols`). 다만 **confluence는 `rows`로 표현할 수 없다** — `rows`는 심볼당 순차인데
  new-rewrite의 confluence는 `Mfe.check_batch` 한 세션 + `on_result` 스트리밍 + inconclusive
  보류/재시도다. 그래서 배치 블록은 유지하고 roots·resume·정규화 호출만 refactor 모듈로 돌렸다.
- termination/scc는 `rows ~row_of` 안에서 `Subproc.timed`로 벽시계 열을 만든다.
- `slice_size` 헬퍼는 slicer를 받도록 바꿔 세 커맨드가 인덱스를 공유한다.

**무회귀 게이트 (통합 전후 실측).** 결정적 출력이 전부 byte-identical: `specs/p4`·`p4-old`·
`impty/{base,closure}`의 `rewrite`(실행 모듈)와 `rewrite --ctrs`(분석 CTRS), p4
`--slice-dir`의 **심볼별 슬라이스 2356개 전량 + `_fidelity.tsv`**, impty 골든
(`specs/impty/base/spec.ctrs`), `--all` 루트 순서, resume 프로토콜, `scc --emit`. 유일한
비동일 항목은 스윕 벽시계 열의 20.9초 대 20.8초(측정 지터)와 캡처 스크립트가 stderr에
덧붙이던 `exit=0` 한 줄이다.
- ⚠️ p4-old의 `--slice-dir` 전량 대조는 **생략**했다(p4 2356개가 같은 표면을 덮고, pre/post
  각각 20여 분이 더 든다). p4-old는 `--ctrs`/`rewrite` 전문 대조로 남는다.

**예산 사다리 (`a582c670` + 수정 `a1fd043b`).** `verification.md`의 `term초`가 난이도가
아니라 `--budget` 설정값이었다는 진단에 대한 대응. `Termination.check`가 unravel된 TRS
하나에 예산을 올려가며(5·20·80·…·cap) AProVE를 반복 호출하고 처음 답이 나온 rung을
기록한다. 마지막 rung이 항상 cap이라 **판정은 바뀔 수 없다**. TSV 5열 추가, resume은 0열만
읽으므로 옛 4열 파일과 호환.
- ⚠️ **첫 구현은 AProVE `Error`를 영구 실패로 보고 cap으로 점프시켰고, 이는 틀렸다.**
  `$bitacc_offset_op`은 예산 5에서 판정 줄 없이 6초 만에 끝나고(→ `Error "no YES/NO/MAYBE
  line"`, 크래시와 구별 불가) **같은 TRS가 예산 20에서 22초에 YES**다. 답이 나오는 rung을
  건너뛰고 1800초를 태웠다. 수정: 답(`Yes`/`No`)이 아니면 전부 등반하고, 진짜 영구 오류인
  바이너리 부재만 사다리 진입 전에 한 번 검사한다. 판단은 순수 술어 `Termination.decisive`로
  분리해 유닛 테스트가 잡는다(뮤테이션으로 이빨 확인).

**전수 재측정 (`0621234d`, `2791cf01`).** 판정은 전부 불변, 시간만 제자리를 찾았다.
- ≤500 (153심볼): **153/153 YES**. 답한 실행의 벽시계 합 279.8초(종전 열은 예산 300 고정의 합 13,000.9초를 재고 있었다). 151심볼이 첫 사다리 단계(≤6초)에 답하고 `$bitacc_{range,offset}_op` 둘만 21.6초다. 옛 표의 1위 `$bin_minus`(1800.5초)는 5.6초로 답한다.
- \>500 (124심볼): **124/124 YES, 답한 실행 벽시계 합 479.6초.** 폐기된 모듈러-B 축의 MAYBE 11건이
  전부 닫혔다. §2 표를 27행 stale 스냅샷에서 124행 직접 축으로 교체.
- 밴드 분모 127 → 124는 iter 헬퍼 통합(2026-07-18)에 따른 세대 차이이지 버그가 아니다.

**남는 일**: >500 구간의 CRC 전수 측정(심볼당 20~25분, 124심볼이면 2일 규모).

### 2026-07-24 — 【research note】 CRC 비용의 지배 요인은 맨변수 바인더 조건 — inline/unravel을 슬라이스별로 **선택**할 여지 — 논의/확인 필요

> `verification.md` ≤500 슬라이스에서 CRC 시간이 규칙 수와 어긋나는 이상치를 추적하다,
> 비용의 98.6%가 한 구문 부류에 몰려 있음을 확인했다. `40319e67`(`order_conds`)이
> 닿지 못하는 잔여 부류이고, `fold_premise_binders`와 `crc_unravel` **양쪽을 다 빠져나간다**.
> 아래 수치 중 벽시계 초는 `confluence` 서브커맨드(생산 경로)로 잰 것이고, 구조 수치는
> `rewrite --ctrs` 덤프의 정적 분석이다.

**잔여 부류의 정체.** Maude의 CRC는 조건부 등식 `l = r if s = t`를 조건부 규칙
`crl l => r if s => @X /\ t => @X`로 재인코딩한다. `t`가 **맨변수**면 그 변수가 재작성
조건의 왼쪽에 놓이고, Maude는 `l => r`에서 `r`쪽만 바인딩으로 인정하므로
`Warning: variable V is used before it is bound`를 찍는다. 매칭이어야 할 것이 탐색이 된다.
`order_conds`가 보장하는 것은 **평가측 `s`**가 이미 묶여 있다는 것뿐이라, 어떤 순서로
재배열해도 이 부류는 남는다(맨변수 패턴측을 자기 바인딩 조건보다 먼저 묶을 순서는 없다).
`$callableId_IR`에서 조건 6개 전부 평가측 OK인데 경고 5개 = 맨변수 패턴측 5개로 정확히 일치.
2026-07-24 WLL 노트의 「결정성 위반 0」은 DCTRS 결정성(평가측)을 잰 것이고 이것과
**다른 층위의 수치**다 — 서로 모순이 아니다.

**전수 실측 (≤500 슬라이스 150/153, CRC초는 `bff805ec` 측정치).**
- 조건부 등식 없는 84 슬라이스: 합 **25.1초**, 최대 8.3초(`$bin_div`, 113규칙).
  조건부 등식 있는 66 슬라이스: 합 **15,402.6초**. → ceq가 예산의 **99.8%**를 진다.
- 맨변수 패턴측 0인 109 슬라이스: 합 212.4초, 중앙값 0.1초.
  1개 이상인 41 슬라이스: 합 **15,215.3초 = 전체의 98.6%**, 중앙값 43.6초.
- 둘 중 하나만으로는 안 터진다 — 2×2 (중앙값):

  | | 규칙<50 | 규칙≥50 |
  |---|---|---|
  | 맨변수 0 | 0.1초 (n=98) | 6.5초 (n=11) |
  | 맨변수 ≥1 | 0.2초 (n=9) | **63.7초 (n=32, 합 15,171초)** |

- 순위상관은 규칙 수가 여전히 최선(ρ=+0.898, 조건부 부분집합 내 +0.951). 규칙 수는
  **순위는 맞히되 크기를 크게 틀리고 기전을 안 준다** — 프록시 교체가 아니라 보강이 맞다.

**두 패스를 다 빠져나가는 이유.** 죽은 바인더(uses=0)는 `fold`가 인라인할 곳이 없어 남기고,
`crc_unravel`은 게이트 `| App _, App (f,_) when fresh <> [] && is_defined f`가 **tp가 맨변수인
경우를 명시적으로 제외**한다(주석: "a BARE-VARIABLE binder `s = v` is fold's job"). 그 판단은
결정성 임계쌍만 보고 내린 것이고, `=`→`=>` 재인코딩이 만드는 자유변수 탐색 비용은 고려 밖이었다.

**게이트를 열어 본 실측** (env 스위치 2개로 4구성을 한 빌드에서 덤프 — 커밋하지 않은 실험):

| 구성 | `$callableId_IR` rules/conds/맨변수 | `$bin_band` | `$bin_shl` | `$write_bits_from_value` |
|---|---|---|---|---|
| C0 base | 13 / 6 / **5** | 202/42/**10** | 201/43/**8** | 103/12/**3** |
| C1 현행 `--crc-normalize` | 13 / 6 / **4** | 202/42/1 | 201/43/1 | 103/12/**3** |
| C2 게이트 완화(inline→unravel) | 17 / 2 / **0** | 203/41/0 | 202/42/0 | 106/9/0 |
| C3 unravel만(inline 생략) | 18 / 1 / **0** | 209/32/0 | 206/35/0 | 106/9/0 |

- **`$write_bits_from_value`는 현행 정규화가 완전한 no-op**(C0=C1). TIMEOUT 7개 중 유일하게
  현행 수단이 무력하고 게이트 완화만이 유일한 경로.
- **C3가 C2를 조건 수·중첩쌍 양쪽에서 이긴다**: 조건 band 41→32, bxor 35→28, shl 42→35;
  중첩쌍 band 230/10→228/8, bxor 227/8→225/6, shl 232/15→230/13, write_value 217/4→216/3.
  aggressive inline이 subject 항을 사용처마다 복제하는 반면 unravel은 한 번만 묶기 때문.
  → **`crc_normalize`의 `fold ~aggressive |> crc_unravel` 순서가 uses가 많은 바인더에는
  역방향일 수 있다.**
- **회귀 없음**: 게이트 완화가 subject 게이트(`is_defined f`)를 안 건드리므로 `$join_text`는
  C0/C1/C2/C3 **전부 동일**(13/11/2/4/0/2) — `1dd1e43a`가 막은 over-unravel 실패 모드는 그대로.

**WLL 재검사 (독립 구현, 위 노트 수치 정확 재현).** 승격 6슬라이스 clean, `$join_text` clean,
`$set_priorities_of_tableEntryListIR` **2건**, `$starts_with`는 `t-prefix:Text` 재사용 1건.
- **CRC TIMEOUT 7개는 전부 WLL clean** — 승격 전제가 정확히 필요한 곳에서 성립한다.
- 무거운 41 슬라이스 중 **27 clean / 14 위반**(`$bin_band`·`$bin_bor` 각 2, `$bin_satminus` 3,
  `$set_priorities…` 2 등). 위반 슬라이스는 게이트를 완화해도 승격이 불가.

**소스 수준 대안 (측정 완료).** `$callableId_IR`의 전제
`-- if (_ _ _ id _ = parameterIR)*`는 와일드카드 4개가 이름 붙은 바인더로 승격되어
`$iterproj` 5벌(등식 10개)과 조건 5개를 만드는데, RHS는 `id`만 쓴다(나머지 4개가 RHS·다른
조건 어디에도 없음을 프로그램 확인). 이미 같은 명세가 쓰는 접근자 관용구
(`4.2-ir-call-overload.spectec:58`)로 바꾸면 — `-- if (id = $id_of_parameterIR(parameterIR))*` —
슬라이스가 13규칙/6조건/맨변수 5 → **4규칙/2조건/맨변수 0**이 되고, `$callableId*` 3심볼이
**250.3초 → 0.7초**(verdict YES 유지). 같은 와일드카드 구조분해 전제가 명세에 **30군데**.

**논의/확인 필요:**
- [x] **WLL 검사로 inline/unravel을 슬라이스별 자동 선택** — **완료 2026-08-03**
  (`3a6794e7` → `c01dd14c`; 위 2026-08-03 절 ①). 붙인 자리는 셋 다 —
  `Crc_surface.normalize_ladder`가 정하고, `confluence` 행이
  `YES (normalized:unravel|rebind|inline)`로 어느 단이 정착시켰는지 밝히며,
  `rewrite --wll-check`가 심볼별 위반 수와 첫 단을 찍는다. 실측 clean 400 / 위반 174.
  **단, "선택"이 아니라 "사다리"가 맞았다** — 지배적 arm이 없다는 것이 측정으로 확정됐고,
  upgrade-only라 여러 단을 시도해도 잃을 것이 없다.
  아래는 착수 전 계획이다. 이 세션의 결론은 "둘 중 하나"가
  아니라 "슬라이스마다 다름"이다. inline은 **동치**(양방향 전이, base 표면 가능),
  unravel은 **reflect-only**(upgrade-only, WLL 필요). 그러면 선택 규칙의 자연스러운 형태는
  「WLL clean이면 unravel(더 싸고 중첩쌍도 줄임), 아니면 inline(승격 못 하니 동치 유지)」.
  검사 자체는 `Rewrite_system`의 기존 원시연산만으로 8줄 — `cond = term * term`이고
  `vars_of_term`이 중복 제거를 **안 하므로**(출현 리스트 그대로) 새 헬퍼가 필요 없다.
  규칙당 선형이라 74,945규칙 전수가 1초 미만. 붙일 자리는
  `Mfe.check_normalize_upgrade`의 게이트 / `confluence` TSV 컬럼 / `rewrite --wll-check` 중 택일.
- [x] **선결 조건 — 뒤집힌 가드의 방향 규약** — **해소 2026-07-24** (`bdceb303`,
  `Rewrite_system.orient_conds`; 아래 전용 절). 실측 142건(analysis)/95건(execution)이었고
  둘 다 0이 됐다. 이제 WLL 체커는 `(l, t₁..t_k)` = lhs + 각 조건의 `snd`, `(r, s₁..s_k)` =
  rhs + 각 조건의 `fst`로 **슬롯을 그대로 믿어도 된다**. 다만 "양쪽 다 호출"인 잔여 15건은
  방향이 여전히 임의이므로 체커가 따로 다뤄야 한다.
- [x] **base 표면에서 고칠 수 있는 부류를 따로 둘 것** — **완료 2026-08-03** (`7215c5dd`;
  위 2026-08-03 절 ②). sort 간극도 실측으로 닫았다: 죽은 바인더 324개 중 320개는 변수의
  sort가 producer의 result sort와 같아 검사가 공허하고, 나머지 4개도 공허하되 이유가
  다르다(3개는 signature가 `Val`로 넓힌 `$bitstr-to-int`, 1개는 앞선 `subty_` 가드와 중복).
  아래는 착수 전 계획이다. 죽은 바인더(uses=0)는 조건
  `$f(A) = v`를 `isStuckHead($f(A)) = false`로 치환하면 패턴측이 생성자가 되어 자유변수가
  사라진다(`fold_premise_binders`가 이미 쓰는 기계). **upgrade-only 강등 없이** 적용 가능.
  다만 변수 매칭은 sort 검사를 겸하므로 `isStuckHead`와 완전 동일하지 않다 — 그 간극 확인 필요.
- [ ] **비트벡터 계열에 자유변수 제거가 충분한지 미검증.** `$bin_plus`(ceq 8, 36.3초)와
  `$bin_shl`(ceq 14, TIMEOUT)은 맨변수 8개·중첩 222 대 232로 사실상 같다. 이 계열 비용은
  공유 비트연산 라이브러리(`badd`/`bmul`/`bsub-mask` 각 16규칙, 230쌍 중첩)에서도 온다.
  `$callableId*`(중첩 0, 비용 전부가 자유변수 탐색)와 달리 **필요조건이되 충분조건이 아닐**
  가능성이 크다. C0~C3 × TIMEOUT 7을 직렬로 실측해야 갈린다.
- [x] **소스 30군데를 접근자 관용구로 통일할지** — **완료 2026-08-03** (`48a17692`;
  위 2026-08-03 절 ③). 실제로는 29군데였다. 전제였던 재검은 인터프리터 전수 골든
  차이 0 + 무작위 100개 differential gap 0으로 통과했고, 전수 differential이 남았다.

> 측정 환경 주의: 이번 세션의 벽시계 초는 컨테이너에서 `reverify` 워크트리의 스윕이 동시에
> 도는 동안 잰 것이라 `bff805ec` 기준치보다 1.6~2.0배 부풀어 있다(`$callableId_IR` 42.7→68.0,
> Constructor 75.4→137.2). 위 본문의 절대 초는 `bff805ec` 기준치를, 250.3→0.7 같은 **비교**는
> 같은 조건에서 잰 쌍을 쓴다. 별도 `.mod` 하니스로 잰 수치는 계통 오차가 커서 채택하지 않았다.

### 2026-07-24 — 【research note】 `--crc-normalize` 승격의 문헌 근거와 슬라이스별 WLL 실측 — 논의/확인 필요

> unravel(reflect-only) 승격의 이론 근거를 원전으로 재검토하고(Gmeiner–Nishida–
> Gramlich IWC 2013; Nishida–Sakai–Sakabe LMCS 8(3:4) 2012; Gmeiner WPTE'16 =
> arXiv:1701.00638), 그 구문 전제를 분석 표면 전수에 기계 검사했다.

**근거의 구조.** "정규화 YES → 원본 YES" 승격이 기대는 것은 GNG IWC 2013
Lemma 2("U(R) confluent + U가 joinability-sound ⇒ R confluent")인데,
joinability-soundness는 공짜가 아니다 — reduction-soundness가 이를 함의하지
않는 반례가 같은 논문 Ex. 3에 있다. 그것이 성립하는 증명된 클래스는 (i)
oriented **DCTRS**, (ii) **WLL**(규칙마다 `l, t_1..t_k`에 2회 이상 나오는
변수는 `r, s_1..s_k`에 등장 금지), (iii) unraveling이 **U_conf꼴**(U-심볼을
(lhs, 조건항)으로 키잉해 형제 규칙이 공유 — 우리 `crcu`의 (chain-lhs, subject)
키잉과 동형)의 삼중 전제 하의 Thm 9. 대안 경로(WPTE'16 Thm 22: right-stable +
U_seq(R) confluent **and terminating**)는 **U_opt(변수 운반 최적화판)에는
성립하지 않는다**(Ex. 2) — *어떤* unraveling인지가 전제의 일부다.

**실측 (2026-07-24, 분석 표면 74,945 규칙 / 2,356 head 전수 검사).**
- 결정성(use-before-bind)·type-3(extra-var): **위반 0** — `order_conds` 실측과 정합.
- **WLL: 전체 표면은 위반 273건.** 바인딩된 변수를 조건 패턴 자리에 재사용하는
  동등성 테스트 인코딩(`$starts_with`의 `slice(t,…) = t-prefix`, `$lookup`의
  key 매칭)이 원인. right-stability fresh 위반 247건; 별도 157건은
  `none = $f(x)`꼴 방향 뒤집힌 가드(문헌 방향으로 읽으면 무해한 표기 artifact).
- **승격이 실제 일어난 6개 슬라이스**(`$write_value*` 5종, `$bin_concat`)는
  **plain·normalized 덤프 모두 위반 0**(WLL·right-stability·좌선형 전부 충족).
  `$join_text`도 clean. WLL 위반 2건(rhs 재사용)을 가진 `$set_priorities_of_…`는
  plain CRC가 이미 YES라 unravel soundness 비의존.

**논의/확인 필요:**
- [ ] **향후 승격 시 슬라이스별 WLL 재검사의 절차화.** 전체 표면이 WLL이 아닌
  이상, 잔여 TIMEOUT 7 등 다른 심볼을 `--crc-normalize`로 승격하게 되면 그
  슬라이스의 WLL을 먼저 확인해야 한다. `confluence --crc-normalize`에 검사를
  내장할지(승격 전 WLL 검사, 위반 시 승격 거부 또는 경고), 수동 체크리스트로
  둘지 결정 필요. 검사 자체는 `rewrite --ctrs --symbol NAME` 덤프의 eq/ceq를
  파싱해 「(l, t₁..t_k)에 2회 이상 나오는 변수가 r/s_i에 등장하는가」를 세는
  ~100줄 스크립트로 재현된다.
- [ ] **crcu/crck 변형의 joinability-soundness 지위.** 정리들은 무sort TRS 위
  U_seq/U_conf 원형에 대한 것이고 우리 것은 변형(함수-subject 바인더만 부분
  unravel + keep-생성자 + order-sorted)이라 현재는 같은 증명 구조 안의 유추다.
  자체 논증(tb-역번역 soundness를 우리 변형에 맞게)으로 격상할지, 유추인 채로
  두고 upgrade-only 안전망을 계속 유지할지.
- [x] **방향 뒤집힌 가드 표기 통일** — **완료 2026-07-24** (`bdceb303`). "Maude ceq
  의미론에선 무해한 표기 artifact"라는 이 항목의 판단은 **틀렸다**: unravel에서 체인이
  끊겨 termination 판정의 전제가 깨진다. 아래 전용 절 참조.

### 2026-07-24 — 조건 방향 규약 통일 (`orient_conds`) — ✅ 완료, 재측정 부채 발생

**규약.** 조건 `(s, t)`는 `s`를 **평가**하고 결과를 패턴 `t`에 **매칭**한다
(`Crc_surface.order_conds`가 명시하고 `Unravel.schedule_conds`가 그대로 읽는 규약).
그런데 소스 등식 전제는 **소스 순서 그대로** 번역돼(`to_ctrs.ml`의 `EqOp` 케이스),
명세가 호출을 뒤에 쓰면(`-- if eps = $f(x)`) 쌍이 뒤집힌 채로 나왔다.
실측: **analysis 142건 / execution 95건**, **2356 슬라이스 중 282개**가 영향.

**"무해한 표기"가 아니었다 — termination 건전성 구멍.** Maude의 `=` 조건은 양측을
정규화 후 비교하므로 *찍히기만 하는* 곳에선 안 보인다. 그러나 unravel은 패턴측을
헬퍼 규칙의 **lhs로 들어올리므로**, 패턴이 defined 심볼로 헤드되면 producer가 공급하는
값과 **영원히 매칭되지 않는다**. `$find_name_annotation_opt` 실측:

```
d_find_name_annotation_opt(…) -> u_5(none, k_5(…))              ← producer
u_5(d_find_name_annotation_opt(annotation), k_5(…)) -> u_6(…)   ← 불발
u_6(true, k_6(…)) -> d_find_name_annotation_opt(annotationListNonEmpty)  ← 재귀가 갇힘
```

`u_5(none, …)`이 정규형이 되어 그 뒤 재귀가 AProVE에 안 보인다. `TRS 종료 ⟹ CTRS 종료`는
TRS가 모든 CTRS 스텝을 시뮬레이션할 때만 성립하므로, **영향받은 슬라이스의 YES는
틀린 게 아니라 미증명이었다**. `unravel.ml`의 자체 검사(미바인딩 rhs 변수)는 자유변수가
없어서 못 잡고, 죽은 규칙은 오히려 종료를 쉽게 만든다 — 안전한 방향이 아니다.
바로 옆 형제 절(`u_7`/`u_8`, 원래 정방향)이 대조군이다.

**수정.** `Rewrite_system.orient_conds` — 패턴측이 defined 적용이고 평가측은 아닌 쌍만
뒤집는다. 양쪽 다 호출인 경우(analysis 15건)는 더 나은 방향이 없어 그대로 둔다.
`Pipeline.build_with` 말미에서 호출해 **두 표면 모두** 규약을 만족시킨다.
`conds_of_prem`이 아니라 파이프라인 후단인 이유: 번역 시점엔 `defined_heads`가 없어
IL 수준 추측이 필요한데, 후단에선 `order_conds`/`unravel`과 **정확히 같은** defined
개념을 쓸 수 있고 `EqOp` 외의 생성 경로도 함께 덮인다.

**무회귀 실측** (같은 트리에서 패스만 빼고 대조):

| 표면 | 순수 방향 뒤집기 | 그 외 |
|---|---|---|
| p4 `--ctrs` | 143 | 1 (아래) |
| p4 실행 | 90 | 1 (같은 줄) |
| p4-old `--ctrs` | 68 | 0 |
| p4-old 실행 | 28 | 0 |
| impty base/closure (양 표면) | — | **byte-identical** |

- 실행 표면 `:=`/`=>` 개수 불변(p4 6371/0, p4-old 6376/0) → **조건의 Maude 형태가 하나도
  안 바뀌었다**. 순수 등호 조건의 좌우 교환뿐.
- 유일한 비-flip 변경: `$iterall_…_ca113dfc_list_2` 한 줄에서 호출이 평가측으로 가며 sort
  추론이 `id-param--hd`를 `Val`→`Text`로 회복. op 선언은 불변. 그 헬퍼는 절이 둘뿐이라
  케이스 전수 확인 가능 — 헤드가 Text이고 일치/불일치/비-Text 세 경우 모두 발화·stuck이
  동일(조건이 이미 Text 동등성을 요구). 그래도 **최종 권위는 differential**이다.
- 유닛 테스트 5건(`orient/*`) 추가, 뮤테이션(뒤집기 무력화)으로 이빨 확인 — 2건 실패.
  impty 골든 MATCH, `dune build @fmt`/`@test/rewrite/runtest` 통과.

**⚠️ 재측정 부채**: analysis 표면이 바뀌었으므로 영향 282 슬라이스의 **CRC 열과 term 열이
stale**이다. term은 "판정이 바뀔 수 있다"가 아니라 **"기존 YES 중 일부가 애초에 미증명"**
이라는 성격이라 성격이 다르다. ≤500 밴드 확인된 대상: `$name_annotation_opt`(256규칙),
`$set_priorities_of_tableEntryListIR`(226), `…_prime`(200),
`$optional_annotation_of_parameterIR_prime_prime`(20), `$is_tableDefaultActionProperty`(16).

### 2026-07-22 — 【research note】 실행 경로를 in-binary 서브커맨드로 통합 (confluence/termination/scc 계열)

> 스크립트·스크래치패드에 흩어져 있던 검증 실행 경로를 바이너리 서브커맨드로 모았다.
> new-rewrite `b35215b3` → tip `57a99547`, 커밋 8개(각 단독 빌드 OK, **push 완료 시점은
> 아래 참조**).

**세 분석 검사기 = 하나의 균일 계열.** 분석 CTRS를 심볼별 슬라이스로 검사하는 셋이 동일한
스윕 표면을 갖는다: `--symbol NAME`(반복) 또는 `--all`(작은 슬라이스 먼저), `--out
sweep.tsv`(기록된 심볼 skip → 재개). `rewrite --ctrs`가 검사 대상을 만들고 `rewrite
--list-symbols [--sizes]`가 슬라이스할 이름을 준다.

- `confluence`(구 `verify`) — CRC+ChC(`Mfe.check`), 행 `<sym>\t<cr>\t<chc>`. 전체-시스템
  모드 없음(임계쌍 폭발; 규칙 head가 정의마다 달라 `--all` per-symbol이 사실상 동등 sound).
  `--crc-normalize`는 MAYBE/TIMEOUT만 정규화+prune 재검해 YES면 `YES (normalized)` 승격
  (upgrade-only, 하향 절대 없음; `Mfe.check_normalize_upgrade`).
- `termination` — 구조 보존 unravel(`unravel.ml`) → AProVE(`aprove.ml`, tools/aprove/runme).
  `--emit-trs`/`--budget`. **MTT 완전 대체**(2026-07-19 note 참조).
- `scc` — over-approx(drop_conds+linearize) → 프루닝된 `(fmod)`(To_mfe `?functional`) →
  CETA Maude 2.7 + 구 MFE. 행 포맷은 옛 `run-scc.sh`와 byte 호환. CETA 에셋 부재로 실
  verdict 미측정.

**커밋 8개**: `d3bf2847`(MTT 사장 스크립트 삭제) · `1f9666dd`(Subproc 러너 추출) ·
`c2d22823`(`rewrite --ctrs --prune-signature`, To_mfe `?prune_signature`) · `e518f5d4`
(termination) · `b69e4f63`(scc) · `c269d5bd`(confluence `--crc-normalize`, 재시도 prune
적용) · `4f0bc5a2`(docs) · `57a99547`(reorg: verify→confluence 재명명 + termination/scc와
동일 스윕 구조, `--list-symbols`/`--sizes`를 rewrite로 이관, `--emit` 미추가=rewrite
--ctrs --symbol 중복).

**동등성 검증(전부 통과, verification.md 반영)**: (a) 프루닝 vs python `prune_slice_signature.py
full` — 2354 슬라이스 전수에서 규칙/sort/subsort-edge 완전 동일(op 차이 189건은 python
프루너의 오버로드-collapse 버그일 뿐, OCaml이 옳음). (b) `termination --emit-trs` vs
sp_unravel.py — 10심볼 byte-identical + 153/153 HEAD 슬라이스가 측정 골든 TRS와 byte-identical.
(e) `confluence --crc-normalize` — 정규화 불필요 심볼 plain YES, 승격 심볼 `YES (normalized)`
(prune 없으면 signature-blowup으로 TIMEOUT 재현 못하던 결함을 재시도 prune으로 수정).
유닛(test/rewrite) + cram(test/cli/analysis-commands.t: termination/scc emit·guard, prune,
confluence guard, rewrite --list-symbols/--sizes) 추가.

**폐기 스크립트 삭제 완료(2026-07-22).** `run-scc.sh`/`run-scc-sweep.sh`(→ `scc`),
`prune_slice_signature.py`(→ `rewrite --prune-signature`) 삭제. 애초 게이트였던 "reverify
phaseB가 메인의 python 프루너를 shell out" 조건은, 삭제 시점의 reverify 스윕이 이미 새
`confluence --all --crc-normalize`(in-binary 프루닝)로 옮겨가 python을 안 부르면서 해소됐다.
**caveat**: `scc` 실 verdict는 CETA Maude 2.7 에셋 부재로 옛 `run-scc.sh`와 행 diff 미실시
(모듈 방출은 cram `scc --emit`로 byte 확인); 에셋 확보 시 git history에서 `run-scc.sh` 부활해
대조 가능. `check_diff_p4.sh`/`check_diff_structural_p4.sh`(differential)는 서브커맨드
대체물이 없어 스크립트로 **유지**.

### 2026-07-19 — 【research note】 termination MAYBE의 진짜 원인은 MTT의 unraveling이었다 — 구조 보존 unraveling + AProVE 직접으로 153/153

> 이 항목은 그대로 research note로 옮겨 쓸 수 있게 서술한다. 아래 2026-07-17 항목이
> "AProVE 자동 전략의 도구 측 한계"로 내린 결론을 **반증**한다.

#### 배경 — 우리가 termination을 재던 방식

`tools/mfe/run-termination.sh`는 심볼 하나의 분석 슬라이스를 이렇게 흘려보냈다:

1. `main.exe rewrite --ctrs --symbol <sym>` 로 슬라이스(.mod)를 덤프.
2. `prune_slice_signature.py … full` 로 **시그니처만** 축소. To_mfe는 슬라이스마다 P4 전체
   order-sorted 시그니처(~460 sort / ~750 op)를 찍는데 MTT의 변환이 시그니처에 초선형이라
   20-rule 슬라이스도 안 끝난다. 규칙은 한 줄도 안 건드린다(측정으로 확인: 13 rule → 13 rule).
3. 헤더를 옛 Full Maude functional module로 고침(`mod`→`fmod`, `set include BOOL[-OPS] off`).
4. Maude 2.7.1(hook 빌드) + MFE 2.7.1에 stdin으로 밀어넣고
   `(select tool MTT .) (select external tool aprove .) (select path C;A .) (ct SPEC .)`.
5. **MTT 1.5j**가 order-sorted 조건부 모듈을 TPDB **조건부** TRS로 변환한다. 조건부라서
   `C;A` 경로를 고른다.
6. `termCheck` 훅이 `mfe.config`대로 `tools/aprove/runme`(AProVE WST 모드, Z3 백엔드)를
   호출. 판정은 Maude 출력의 `is terminating`(YES) / `not been found`(MAYBE)로 읽는다.

두 축의 차이는 **2번의 pruning뿐**이었다. `term(AProVE직접)`은 위 그대로, `term(모듈러B)`는
2번을 `prune_modular.py abstract-builtins`로 바꿔 **산술 규칙을 통째로 빼고 산술 op을 자유
생성자로 남긴다**(R_arith를 블랙박스로 두고 spec 층만 증명 → 모듈러 합성). 5·6번은 동일하다.
**즉 두 축 모두 MTT의 변환을 통과했다.** 이게 핵심이다.

#### MTT가 실제로 무엇을 하는가 — 출력 캡처

`mfe.config`(`aprove <path>/runme .trs`)가 가리키는 실행 파일을 "인자 `$1`을 복사해 두고 진짜
runme를 exec하는" 래퍼로 바꾸고, `MAUDE_LIB`을 그 사본 디렉터리로 지정하면 MTT가 AProVE에
넘기는 파일을 그대로 얻을 수 있다. 그렇게 얻은 `$join_text` 슬라이스의 TPDB를 보면:

**MTT는 unravel을 하지 않는다.** 조건부 TRS를 그대로 넘긴다. 대신 두 가지를 한다.

1. order-sorted를 unsorted로 낮추며 **모든 sort를 술어로** 바꾸고(`isText`, `isList`,
   `is[Val]`) 규칙마다 변수별 sort 조건을 붙인다.
2. **조건 `s = t`를 `equal(s,t) -> tt`** 로 바꾸고, 전역 비좌선형 규칙 `equal(X,X) -> tt`를
   추가한다.

#### 결함 — 매칭 조건이 동등성 검사로 바뀌면서 3형 CTRS가 된다

2번이 치명적이다. 캡처된 실제 규칙:

```
$join-text(cons(t-h1, text), t-sep)
   ->  cat(cat(t-h1, t-sep), $join-text(cons(t-h2, t-t), t-sep))
   |   equal(match-cons(text), true) -> tt ,
       equal(text, cons(t-h2, t-t)) -> tt , ...
```

`t-h2`, `t-t`가 **좌변 어디에도 없다**. 원래 `text = cons(t_h2, t_t)`는 *매칭* 조건이라 두
변수가 매칭으로 바인딩되는데, 대칭 동등성 *검사*로 바뀌면서 자유 변수가 됐다. 우변은 그
자유 변수로 지은 `cons(t-h2, t-t)`에 재귀한다.

이것이 **extra-variable CTRS (Bergstra–Klop 3형)** 이다. dependency pair framework 입장에서
재귀 인자 `cons(t-h2,t-t)`는 좌변 인자의 부분항도 아니고 어떤 구문적 관계도 없다 — `equal`
조건을 *의미적으로* 풀어야만 `text`와 같음을 알 수 있다. 3형 CTRS 종료 증명은 1/2형보다
훨씬 비싸다.

우리 인코딩이 premise에서 destructure하는 구조 재귀(`xs = cons(h,t)` 두고 `t`로 재귀)를 즐겨
쓰기 때문에 이 패턴을 대량 생산했고, 그래서 피해가 컸다.

**단, 증명 불가능해지는 건 아니다 — 비싸질 뿐이다.** 캡처한 TPDB를 AProVE에 직접 넣으면
예산을 충분히 줬을 때 **YES**가 나온다. 13-rule짜리 `$join_text` 하나가 이 인코딩에서는
120초를 넘기고, 우리 unraveled TRS에서는 **1초**다. 100배 이상이고, 슬라이스가 커질수록
격차는 벌어진다.

#### 왜 그 비용이 곧바로 MAYBE가 되는가 — MTT의 120초 하드코딩

`mtt.maude:90`이 `termCheck(TOOL, In:String, 120)` 이다. **MTT는 AProVE를 언제나 120초
예산으로 호출한다.** 우리가 스윕에서 준 `TERMA_TMO=1200`/`TERMB_TMO=1800`은 Maude *프로세스*
타임아웃이라 이 내부 한도에 아무 영향이 없었다 — 예산을 늘려도 판정이 안 바뀌던 이유가
이것이고, 나(2026-07-19 최초 서술)는 그걸 "어떤 예산으로도 도달 불가"로 잘못 읽었다.

정리하면 MAYBE는 **두 요인의 곱**이다: (a) `equal` 인코딩이 문제를 3형 CTRS로 만들어 100배
비싸게 하고, (b) MTT가 그 비싼 문제를 120초에서 자른다. 어느 하나만 없어도 살아남는다.
우리 경로는 (a)를 없애서 문제를 1초짜리로 만들고, 덤으로 (b)도 우리가 예산을 직접 쥔다.

#### 해법 — 직접 unravel해서 매칭 의미를 되살린다

unraveling은 조건을 규칙 구조 안으로 옮겨 무조건 TRS로 만드는 표준 기법이다:

```
l            -> U(s, <carried>)     -- 조건을 평가하러 간다
U(t, <carried>) -> r                -- t가 여기서 패턴으로 매칭된다
```

핵심은 마지막 줄이다. `t`가 **규칙 좌변의 패턴**으로 돌아오므로 그 변수들이 매칭으로
바인딩되고, 재귀 인자가 원 인자의 부분항임이 **구문적으로 보인다**. subterm criterion이
바로 잡는다. 이것이 이득의 전부다.

**`<carried>`가 무엇인지는 무관하다.** Marchiori 고전형(좌변 *변수*를 평평하게)과 구조
보존형(좌변 *인자 목록*을 정의 규칙 없는 불활성 생성자 `k_N`에 그대로)을 같은 슬라이스·같은
AProVE로 맞대조하면 **둘 다 0~1초에 YES**다. 실무에서는 단순한 변수 전달형을 권한다.
(공통 함정: 좌변 *항* 자체를 넘기면 규칙이 자기 redex를 재생산해 무한 루프가 된다 — 첫
시도에서 AProVE가 **NO**를 내어 그 버그를 잡아줬다.)

MTT를 빼고 이 평범한 TRS를 `tools/aprove/runme <f>.trs <budget>`(WST)에 직접 던진다.

#### 건전성

`lσ -> u(sσ, k(argsσ)) ->* u(tσ, k(argsσ)) -> rσ` 로 **원 CTRS의 모든 스텝이 시뮬레이션**된다
(조건 평가 `sσ ->* tσ` 자체도 `u(□, k(…))` 문맥 안에서 재현되므로 operational termination까지
덮는다). 따라서 **TRS 종료 ⇒ CTRS 종료**. sort/subsort는 버리므로 TRS는 **과근사**(well-sorted
항이 부분집합) — MTT와 같은 안전한 방향이다.

따름정리로 **NO는 비종료의 증거가 아니다**(과근사 탓일 수 있다). NO가 나오면 witness를
확인할 대상으로만 취급할 것.

분석면 슬라이스가 이 변환에 안전한 근거(153개 전수 확인): `owise` 0, `rl`/`crl` 0,
`assoc`/`comm`/`id:` 0, `:=`/`=>` 조건 0, import 0, mixfix 0 — 전부 prefix·단일행이다.

#### 측정 (153심볼 × 2축, 슬라이스는 `30d413ad` 덤프 재사용)

| 축 | MTT 경로 | 구조 보존 + AProVE 직접 |
|---|---|---|
| AProVE 직접 | YES 117 / MAYBE 12 / TIMEOUT 24 | **YES 153 / 153** |
| 모듈러 B | YES 150 / MAYBE 3 | YES 150 / MAYBE 2 / TIMEOUT 1 |

- **MAYBE 12건 전부, TIMEOUT 24건 중 23건**이 닫혔다. 이진 산술 계열
  (`$bin_*`·`$un_*`·`$bitacc_*`·`$write_value*`)이 통째로 풀렸다.
- 2026-07-17 항목이 "도구 한계"로 지목했던 `$join_text`/`$invalidate_value`/
  `$invalidate_headerUnion`은 MTT로 1200초 태우고 MAYBE였는데 **각 1초에 YES**다.
- **인코딩은 한 줄도 안 고쳤다.** 원인은 번역도 AProVE도 아니고 MTT였다.
- 예산 주의: `$write_bits_from_value` 축A는 300초 TIMEOUT / 1200초 YES. 표 값은 MTT와 같은
  예산으로 맞췄다.

**회귀 3건은 모듈러B 축에만**: `$write_bits_from_value`(TIMEOUT),
`$set_priorities_of_tableEntryListIR{,_prime}`(MAYBE). keep-생성자가 인자 구조를 복제하므로
항이 커지는 비용이 있고, MTT의 분해가 마침 무해했던 슬라이스에서는 그 비용만 남는다.
**두 방법의 비-YES 집합은 서로소**라, 모듈러B도 둘 중 하나만 돌리면 153/153이다.

#### 구현 함정 (놓치면 조용히 틀린 결과가 나온다 — 셋 다 실제로 밟았다)

1. **다조건 체인의 바인딩 변수 누적**. 조건이 뒤 조건이나 최종 우변이 쓰는 변수를 바인딩할
   수 있다(`… if p(x) = cons(h,t) /\ q(h) = true`). keep-생성자가 원 인자 + **앞선 조건들이
   바인딩한 변수 전부**를 날라야 한다. 안 그러면 마지막 규칙 우변에 미바인딩 변수가 생긴다.
2. **소스 조건이 use-before-bind 순서로 나온다**. reflect의 guard 패스가 `isStuckHead` 가드를
   그 변수를 바인딩하는 조건보다 **앞에** 놓는다(`$bitstr-to-int`에서 `half`). 좌변이 전부
   바인딩된 조건을 하나씩 골라가는 위상 정렬로 재배치해야 한다.
3. **커버리지를 가정하지 말고 검증**. 처리 못 한 방정식과 미바인딩 우변 변수에서 **크게
   실패**시킬 것. 위 두 버그는 전부 이 검사로 잡혔고, 둘 다 그럴듯해 보이는 출력을 내고 있었다.

#### 방법론 반성 — 어떻게 틀렸었나

최초(2026-07-19) 서술은 "MTT의 unraveling이 인자를 분해해 부분항 관계를 역전시켜, 예산과
무관하게 증명이 도달 불가"였다. **세 군데가 틀렸다**: MTT는 unravel을 하지 않고, 따라서
분해도 역전도 없으며, 도달 불가도 아니다(예산만 주면 YES).

원인은 방법이다. **파이프라인 전체(MTT 경로 vs 우리 경로)를 맞대조해 놓고 그 안의 한 요소를
원인으로 지목했다.** 두 경로는 unraveling 말고도 sort 인코딩·조건 인코딩·AProVE 예산이 전부
달랐는데, 그중 하나를 골라 이야기를 지어낸 것이다. 반증한 실험은 셋 다 "한 번에 하나만
바꾸는" 형태였다:

1. 같은 슬라이스에서 **unraveling 방식만** 교체(변수 전달 vs 구조 보존) → 둘 다 YES.
   ⇒ unraveling 방식은 원인이 아니다.
2. MTT가 AProVE에 넘기는 파일을 **캡처**해 직접 실행 → YES.
   ⇒ MTT의 출력 자체는 증명 가능하다. 문제는 출력이 아니라 호출 조건이다.
3. `mtt.maude` 확인 → `termCheck(…, 120)`.
   ⇒ 예산이 잘렸던 것이고, 우리가 준 1200s는 엉뚱한 층의 타임아웃이었다.

**교훈: X가 Y의 원인이라 주장하려면 X만 바꿔라.** 그리고 도구가 왜 실패하는지 궁금하면
도구가 실제로 내놓는 산출물을 먼저 확보하라 — `mfe.config`를 래퍼로 바꾸는 데 5분 걸렸고,
그것 하나가 잘못된 이야기 전체를 무너뜨렸다.

#### 후속

- [x] unraveler 승격 (2026-07-22 완료): 스크립트가 아니라 in-binary `main.exe
      termination`(unravel.ml/aprove.ml)으로 완전 포팅. 구조 보존형을 채택(측정
      골든 trsA와 byte-identical하게 검증 가능해서); MTT 폴백은 두지 않고 경로 폐기.
- [ ] 폐지한 모듈러 축에서 구조 보존이 MTT보다 나빴던 3건: 항 크기 가설 검증. 맞다면
      **escape하지 않는 인자는 keep에서 제외**하는 최적화로 닫힌다. 변수 전달형을 쓰면
      애초에 발생하지 않을 수도 있다(미확인).
- [ ] MTT 인코딩의 실제 비용 곡선 측정(선택). `$join_text`는 우리 1초 : MTT 인코딩 >120초다.
      슬라이스 크기에 따라 이 배율이 어떻게 커지는지 보면 "MTT를 폴백으로 둘 가치"가 정해진다.
- [ ] §2 >500 표(term(B), MAYBE 11)도 같은 경로로 재측정 — 상당수가 닫힐 것으로 예상.
- [ ] 측정 기준 커밋 확인: 측정은 `30d413ad` 덤프 기준인데 그 뒤 술어 도메인 변경
      (`6e740f3e` 계열)이 들어왔다. HEAD에서 재덤프해 대조할 것(sort 태그만 달라졌다면
      unraveled TRS는 byte-identical이라 판정 불변).

### 2026-07-17 — co-iteration `$unzip` fusion (SoA→AoS): termination 개선 시도

> ⚠️ **이 항목의 결론(맨 아래 "AProVE 자동 전략의 도구 측 한계")은 2026-07-19에 반증됐다.**
> AProVE는 이 구조를 1초에 증명한다 — 문제는 그 앞단 MTT의 unraveling이 하강을 파괴한 것이었다.
> 위 2026-07-19 research note 참조. 단, fusion 자체의 정당성과 무회귀 검증 결과는 유효하다.

**동기**: term(AProVE) MAYBE 심볼 `$invalidate_value`/`$invalidate_headerUnion`의
원인을 실험으로 규명 — co-iteration의 축별 분리(SoA) 인코딩이 소비 헬퍼의 재귀를
`$unzip_v(iterbind)`라는 **함수 호출 결과**의 tail에 걸어, AProVE가 감소를 syntactic
subterm으로 못 봄. → 소비 헬퍼(`$itercollect`/`$itermap`/`$iterall`/`$iterapply`)가
원본 `iterbind`를 직접 elem-pattern으로 destructure하는 **fused(AoS)** 인코딩으로 전환
(상세: [CORE_LOGIC.md](CORE_LOGIC.md) §3.7.1).

**구현**(`to_ctrs.ml`): rule 단위 binder 레지스트리(`iter_ctx`) 도입 — `pattern_of_exp`가
head `IterE`를 등록하고 unzip 조건을 제자리에 방출, 소비 지점(`term_of_exp`/`conds_of_prem`)이
`spines_of_ids`로 fused/bare spine 계산 + `absorbed` 기록, `prune_absorbed_unzips`가
흡수+미사용 unzip만 사후 제거(escape/dead는 유지). `spine_disamb`로 fused 변형 네이밍
(base 이름 유지 → reflect.ml 무보수 호환). helper 정의 walk는 clause 단위로 재구성.

**검증(전부 통과 — 인코딩 정확·무회귀)**:
- impty 골든 재생성 + `run-structural` 8/8 `result: true`.
- full-corpus `--ctrs` 대조: `op $unzip` **105→45**(escape 잔존분만), `$dom_map`/`$codom_map`
  등 escape 무변화, **비-iter 심볼 rule 0라인 변화**.
- 샘플 diff test: `run-structural --check-p4` 16개(union invalidate 포함) + `run --check-p4`
  10개 — **전부 MATCH, MISMATCH 0**.
- owise 반사: 72처리(69 reflected + 3 complement), **0 kept**(fused 변형도 반사됨).

**term 결과: 대상 2심볼 MAYBE 유지(성공 판정 미달) — 원인은 unzip이 아님.**
toy(단순 self-recursive)·toy2(3함수 상호재귀 미러)는 fused로 AProVE **YES** 실측(종료
구조 개선 입증). 그러나 실제 대상은 fused 후에도 MAYBE. 원인 후보를 **전부 배제**:
unzip 소거(✓)·subty guard 제거(여전 MAYBE)·yices 부재(README상 불필요, z3로 진행)·JVM 힙
(-Xmx100g로도 MAYBE)·signature 크기(prune 후 8-rule/12-op 극소 슬라이스도 MAYBE). 곧
AProVE 자동 전략이 이 특정 구조의 종료 증명을 못 찾는 **도구 측 한계**(동형 toy2는 YES).
⇒ fusion은 종료를 논리적으로 보장하고 규칙 수를 줄이는 정당한 개선이나, 이 대상들의
AProVE 판정을 바꾸지는 못했다. 후속: AProVE 전략 튜닝 / 수동 종료증명 별도 트랙.

### 2026-07-18 — Native 실행 표면의 completeness gap 2건 수정 (structural builtin 규칙 누출)

**발견 경로**: iter 헬퍼 통합 후 전체 corpus differential(1568 프로그램)에서 completeness
gap 2건(`samples/const.p4`, `samples/issue1717.p4`). fusion 이전 바이너리로도 재현돼
**통합과 무관한 별개 회귀**임을 먼저 확정한 뒤 원인을 팠다.

**공통 원인 — delegation이 있는 builtin의 structural 규칙이 Native 모듈에 함께 방출**.
`Native` 스칼라 이론은 스칼라를 손수 구현한 {!Prelude} 규칙을 **의도적으로 생략**하고
{!To_maude.delegation_eqs}의 한 줄 위임으로 대체한다. 그런데 `builtin.ml`이 만드는
structural 규칙은 Native 모드에서도 방출되고, **먼저 선언되므로 매칭을 이긴 뒤**
생략된 binary-nat 심볼로 내려가 영구 stuck이 된다(뒤에 있는 완전한 delegation은
영원히 도달 불가).

- **gap1 `const.p4`**: `~32w0` → `$un_bnot` → `$pow2` → `bpow_nat`(Native에 eq 0개)
  → `isStuckHead(i'') = false` 가드 실패 → 규칙 발화 불가. (`$bneg`은 우연히 생존 —
  structural 경로가 delegation 있는 `add_int`/`negate_int`로 이어졌기 때문.)
- **gap2 `issue1717.p4`**: `const bit<32> sz = h1.minSizeInBits();` →
  `$sizeof_minSizeInBits'(HEADER…)` → `$sum_nat` → **폴드 시드가 `bzero`**(0-arity
  상수라 1차 스캔에서 놓쳤다) → `add(nat(32), bzero)`에서 정지. 최소 재현 15줄:
  control apply 안 `const bit<32> sz = h1.minSizeInBits();` 하나면 충분(함수/control
  무관, `const sz = 5`나 `return minSizeInBits()`는 정상 — **const + 메서드 호출**
  조합만 실패).

**수정**(`builtin.ml`): `delegated_in_native`가 주석부터 "the **text** builtins"라
적혀 있었고 실제로 `int_to_text`/`strip_prefix`/`strip_suffix` 3개뿐이었다. delegation을
가진 나머지를 전부 추가 — `pow2 shl shr shr_arith bneg band bxor bor bitacc
strip_all_whitespace` + nat-list 폴드 `sum_nat max_nat min_nat`. 추가 전 각 delegation이
Native 값 전체를 커버하는 **완전한 정의**임을 확인했다. 주석도 "왜 clash가 단순 중복이
아니라 stuck인가"와 "{!To_maude.delegation_eqs}와 동기화하라"로 고쳐 적었다.

**검증**:
- 실행 모듈의 참조-미정의 심볼: `bzero`/`bpow_nat`/`badd`/`bsub`/`bmul`/`bdiv`/`bmod`/
  `bis_zero`/`bpred` 전부 소멸. 남은 8개는 spec 자체에 정의가 없는 의도적 미구현
  (`$bitacc_replace`, `$init_objectState`, `ExternFunctionCall_eval_lctk`,
  `$int_to_bits_*`/`$bits_to_int_*`)과 패턴 위치의 `bsucc`뿐 — **분석 표면에도 동일**.
- **분석 표면(CTRS) byte-identical**, impty 골든 둘 다 무변화(수정은 Native 전용).
- **전체 corpus differential 재실행**: **completeness gap 2 → 0**, Maude OK 1226 → **1228**,
  **결과값 일치 1225 → 1227 MATCH / 0 MISMATCH**(= 2026-07-15 clean-run 기준과 일치).

**남은 soundness gap 1건은 오라클 아티팩트 — 수정 대상 아님.**
`errors/issue1944.p4`는 `const bit<2147483648> x = 0;` 한 줄, 즉 폭 2³¹ 비트 타입이다.
spec에는 **비트 폭 상한 규정이 없어**(grep 확인) 이를 수락하는 Maude가 오히려 spec에
충실하다. 인터프리터는 이 프로그램을 300초는 물론 그 뒤로도 끝내지 못했고(2³¹비트
값을 실제로 만들려는 것으로 보임), `check_diff_p4.sh`가 타임아웃을 FAIL로 기록하면서
"interp FAIL & Maude OK" = soundness gap으로 잡힌다. CTRS 번역 버그가 아니므로 손대지
않는다. 진짜로 거부해야 한다면 spec에 폭 상한을 넣는 별건 작업이다.

### 2026-07-18 — iter 헬퍼 패밀리 통합: `$iterapply`/`$iterproj`→`$itercollect`, 변수별→튜플 수집, `$unzip`→`$iterproj` 개명

**동기(사용자 결정)**: IterPr 출력 쪽 3패밀리를 하나로. (a) 변수별 collect 중복이
만연(예: descriptor `e71f64a7` 전제 하나에 헬퍼 5개 — 같은 리스트 5회 주사, 같은
let 5회 평가); (b) 구 `$iterproj($iterapply(…))`는 함수 호출 결과 위 재귀 — unzip이
termination MAYBE를 만들던 SoA 잔재와 동일 패턴; (c) 2026-07-16 head-side fusion
기계(SFused)가 premise-side 소비자 배선을 이미 풀어놓음.

**설계** ([CORE_LOGIC.md](CORE_LOGIC.md) §3.7/§3.7.1): 전제당 `$itercollect` 하나가
성분(comps = rel call이면 출력 순서, 아니면 binding 순서; `iter_collect_components`)
스트림을 반환 — k=1이면 성분 자체(이름·형태 종전과 동일), k≥2면 **성분 튜플**
(조건 1회 평가). ex-apply는 원소=호출 인라인의 **무조건 특수화**로 흡수(⛔
2026-07-02의 "원소당 1회 호출" 불변식 유지; gensym effectful 판정은 call-graph라
개명 무관). k≥2 사용처는 튜플 스트림을 `iterbind_N`으로 받아 head binder와 동일
등록 → 소비 헬퍼 fused destructure / escape는 합성 튜플 본문(`collect_tuple_body`)
위 `$iterproj` / dead는 `prune_absorbed_projs`. head-side `$unzip`은 `$iterproj`로
개명해 한 패밀리(`$iterall`/`$itercollect`/`$itermap`/`$iterproj`).

**검토·기각한 대안 — SoA-return collect** (n tuple list 대신 n list tuple 반환;
재귀-조건형/accumulator형): accumulator형은 사용부가 튜플 패턴 cond 1개로 끝나
표면상 가장 깔끔하고 producer 종료도 자명하지만, **MAYBE의 원인은 사용부 경계** —
성분 리스트가 함수 결과로 태어나는 한 소비자 재귀가 C;A 후
`$itermap(π₁(collect…), π₂(…))` 꼴로 SoA 패턴을 재현한다. 부수 비용(snoc/reverse
O(n²), k≥2 ex-apply 무조건성 상실, `gen_*_holds` 형태 전제 파괴, `infer_ranges`
컨테이너 휴리스틱 실패)까지 겹쳐 기각. 전 성분 escape 사이트에 한해선 더 깔끔하나
인코딩 2벌 유지비가 projection cond 몇 개 절약을 압도.

**구현**: `to_ctrs.ml` — `iter_collect_components`/`collect_tuple_body` 신설,
`iter_collect_sym`이 comps 접미사(k=1은 byte-identical), `iter_apply_sym`/구
`iter_proj_sym` 삭제, `conds_of_prem` IterPr가 k≥2에서 등록+제자리 projection 방출,
`iterpr_defs` Some/None arm 병합(+proj_defs는 여기서만 방출 — 합성 exp는 IL에
없어 visit_exp가 못 찾음), `unzip_sym`→`iter_proj_sym`·`iter_unzip_defs`→
`iter_proj_defs`·`prune_absorbed_unzips`→`prune_absorbed_projs` 개명.
`reflect.ml` — `iter_helper_prefixes`=`[$iterall; $itercollect]`,
`gen_iterapply_holds`를 `gen_itercollect_holds`에 **형태 기반**으로 병합.
**교훈(구현 중 회귀 1건)**: "스텝 무조건 → apply형" 판정은 틀림 —
`fold_premise_binders`가 let-destructure 조건을 LHS 패턴으로 접으면 일반 collect도
무조건이 된다(owise 66/3/3 kept 회귀 관측). 기준은 **원소가 relation call인가**;
수정 후 69/3/0 복원.

**검증(전부 통과)**:
- 구조 sanity(신규 스크립트): 참조 헬퍼 전부 정의 존재·arity 일관·구 패밀리 출현 0
  (**SANITY OK**). 최초 실행에서 `$itermap-…-list-2-pair` 3건이 참조-미정의로
  검출됐는데, 추적 결과 **2026-07-16 head-side fusion 커밋(`6e96deb9`)이 만든
  회귀**였다(아래 "동반 수정" 참조). 통합 자체가 만든 것은 아니지만 같은 ctx
  스레딩 결함이므로 이 작업에서 함께 고쳤다.
- impty 골든: 개명+방출순서 churn만(내용 동일), 재생성 커밋.
- p4 corpus 심볼 대조: helper 406→411, eq/ceq 3061→3071(+10) — churn은 전부
  (i) 개명 (ii) apply 흡수 (iii) 변수별→튜플 병합(e71f64a7 5→1 등) (iv) 구 proj
  소멸 (v) 소비자 fused 전환(`$iterall-…-f<tuple>…`; 재귀가 튜플 스트림의
  구문적 tail로 하강 — 육안 확인) 분류에 귀속, 미분류 잔여 없음.
- owise 반사: **69 reflected / 3 complement-enumerated / 0 kept** (베이스라인과
  동일). 다출력 binding cond의 head가 collect 호출 자체가 되면서
  `insert_success_test`가 이제 그 사이트에도 적용됨.
- 실행 differential: `run --check-p4` 26표본(entries 10 포함) **25 MATCH /
  0 MISMATCH**(잔여 1 `issue3671.p4`는 변경 전 바이너리로도 not reduced — 기존
  동작). structural 레그 `run-structural --check-p4` 표본 **2/2 MATCH**.
- MFE CRC (`verify --symbol`): `$invalidate_headerUnion`(head-side co-iteration)
  **CR YES / ChC YES**; `$is_default_parameterIR`(premise-side k=1 collect)
  **CR YES / ChC YES**(이전 세션 값과 일치 — 무회귀). k≥2 튜플 collect 소비자
  (`$resolve_constraint`, `$callableId_IR`)는 **TIMEOUT** — slice 자체는 작으나
  (13/9룰) typing/constraint relation(`Type_ok`/`TableEntry_ok`/`gen_constraint`)을
  끌어와 critical-pair가 폭발하는 이 심볼들 특유의 tractability 문제. **통합 직전
  커밋(1307c45d)을 stash-빌드해 대조 → `$resolve_constraint` CRC 동일하게
  TIMEOUT**(즉 회귀 아님, 통합 무관). 이 슬라이스들의 의미 보존은 CRC verdict가
  아니라 아래 실행/structural differential MATCH로 뒷받침됨.
  termination은 이 계열이 YES(`$callableId_IR`/`$dom_map`).
- AProVE termination (`rtv.sh`): 통합된 다출력 ex-apply 심볼 `$callableId_IR`
  **YES**, `$dom_map` **YES**(둘 다 튜플-collect/`$iterproj` 포함 슬라이스가
  syntactic-subterm 하강으로 종료 증명 성공 — 통합 인코딩이 종료 친화적임을 실증).
  대상 2심볼 `$invalidate_value`/`$invalidate_headerUnion`은 **MAYBE 유지 =
  통합 전과 동일**(2026-07-17 기록의 AProVE 자동 전략 도구 한계, 회귀 아님).

**동반 수정 — fusion 회귀(`6e96deb9`)로 정의가 누락된 중첩 `$itermap` 3건.**

**증상**: `$itermap-id-field-a-colon-typeIR-field-a-list-2-pair`(및 field-b,
`nameIR-field-colon-value-field`)가 **호출되지만 정의 규칙이 없음** — 분석·실행
양 표면 모두(각 104 선언 / 101 정의). op 선언은 range 복구도 실패해 `-> Val` 폴백.

**시점 확정**: fusion 이전 커밋(main `2f9f8cba`)을 덤프 대조하니 **96 선언 / 96 정의,
미정의 0**. 즉 head-side fusion이 도입한 회귀이고, 이번 통합은 이를 물려받았을 뿐.

**원인 — ctx 스레딩 비대칭**. 문제 형태는 head binder가 fusion 등록된 rule 안의
**중첩 IterE**다([5.05.2-typing-casting.spectec:157](../../specs/p4/5-typing/5.05.2-typing-casting.spectec)
`Cast_expl_neq/structTypeIR`의 `$find_map({ (id_field_a ':' typeIR_field_a)* }, …)`):
- 호출부 — `iterpr_defs`가 내부 전제의 조건을 **ctx 없이** 계산(helper 안에는
  head binder가 없고 spine이 인자로 들어오므로 이게 맞다) → bare 2-spine 이름.
- 정의부 — `iter_helper_defs`의 `visit_prem ctx inner`가 **바깥 rule의 ctx로**
  방문 → 같은 IterE가 fused 1-spine 이름으로 정의 방출.
⇒ 호출된 이름은 정의가 없고, 정의된 이름은 아무도 안 불러 `prune_unused`가 제거.

**영향**: 그 스텝의 `isStuckHead($find_map(…)) = false` 가드가 항상 실패해
**규칙이 발화 불가** — 필드 순서가 다른 struct/header 간 명시적 캐스트
(`Cast_expl_neq/structTypeIR`·`headerTypeIR`와 대응 value 규칙)가 죽어 있었다.
분석 전용이 아니라 실행 표면에도 동일했다.

**수정**: `iter_map_def`/`iterpr_defs`/`visit_exp`/`visit_prem`의 ctx를
`iter_ctx option`으로 바꾸고, **iterated 전제의 inner는 `visit_prem None`으로**
내려보낸다(= 그 위치의 호출부가 컴파일된 것과 같은 레지스트리 상태). 원칙:
**정의 방출은 언제나 그 호출 사이트가 쓴 ctx로 해야 한다** — 심볼이
`spines_of_ids`에 의존하기 때문.

**수정 후 검증**: sanity **OK**(104 선언 / 104 정의, 참조-미정의 0, 양 표면),
op range도 `-> Val` 폴백에서 정확한 `-> List`로 복구. corpus churn은 정확히
누락 정의 6줄 추가 + op 선언 3줄 재선언뿐. impty 골든 무변화, owise 69/3/0 불변,
실행 differential 32표본(struct-cast 후보 6개 추가) **31 MATCH / 0 MISMATCH**
(잔여 1 `issue3671.p4`는 fusion 이전 바이너리로도 not reduced — 별개 기존 문제),
structural differential 2/2 MATCH. CRC `$invalidate_headerUnion`·
`$is_default_parameterIR` 모두 **YES/YES 유지**, termination `$callableId_IR`
**YES**·`$dom_map` **YES**·`$invalidate_value` **MAYBE** — 전부 수정 전과 동일.

### 2026-07-15 — 잔여 CRC MAYBE 재분류 + owise or-gate 진단 + 수정 후보 (미구현)

**작업 격리**: worktree `/home/spectec-core-matchbridge`(branch `match-bridge-ceq`,
HEAD 08dfe4ed, 자체 `_build`). 돌고 있던 differential test(`/home/spectec-core`,
new-rewrite)와 완전 분리. MFE 도구는 gitignore라 main clone 것 참조
(`SPECTEC_MFE_DIR=/home/spectec-core/spectec/tools/mfe`, `MAUDE_LIB`=maude dir) — 별도 프로세스.

**잔여 CRC MAYBE 재분류 (현 브랜치 실측 `verify --symbol`).** verification.md 표의 CRC열은
stale. 옛 category-A owise 9개 재측정 → **6개 이미 YES**(`$join_flow` `$is_lpm_key_prime`
`$requires_priority_prime` `$is_default_parameterIR` `$is_tableDefaultActionProperty`
`$join_text` — hoist_matchers/expand_subty_guards/align_guards/fold-matcher-destructure가 해소).
표의 CRC=MAYBE 8개 중 5개는 `$write_value*`(binenc zero-width 진짜 비합류, 별도 트랙),
1개 `$bin_satplus`(arith sign-split, 재측정 TIMEOUT). **잔여 owise/match MAYBE = 정확히 2개:
`$join_ctk`(2-인자), `$assignop_as_binop`(1-인자, 13생성자중 12매치).** 둘 다 **enum-dispatch**:
ground 특정절 + 서로 다른 rhs + owise or-gate 반사.

**진단 (최소예제로 확정) — 상호배타성 문제가 아니라 CRC의 중첩 or-gate feasibility 불완전성.**
- CRC는 조건을 eq/ceq로 **환원한다**(반례: `ceq h=ff if big=ff`+`eq big=tt` → YES; matcher/or-gate
  단일인자 mini2 → YES). 그러니 "조건 미환원"이 원인이 아님.
- 진짜 원인: owise가 ground 형제와 겹치면 조건이 `or(and(match,match)…)=false`(ground, `true`로
  환원가능)인데, **왼쪽-중첩 `or`에서 참 disjunct가 깊이 묻히면** CRC feasibility 검사가 그걸
  못 보고 임계쌍을 보수적으로 보고. 증거: mini3(2인자 4-way)에서 4개 겹침 중 참 disjunct가
  **맨 바깥**인 `f(cb,cb)`만 생존; **or-gate 순서만 바꾸면(mini3b) 생존 ccp 집합이 바뀜** →
  진짜 비합류 아님, 게이트 인코딩 아티팩트.
- 그래서 사용자 제안 **부정 ceq `match_B=false if match_A=true`는 못 고침**: (1) 이 문제를 전혀
  안 건드림, (2) 양방향 ceq가 조건평가 상호재귀로 **CRC 폭발**(Val/Ctk/unfold 무관, timeout/OOM),
  (3) 막는 ccp 가설이 `and(…)=true`라 원자 match가 없어 애초에 발화도 안 함.

**수정 후보 실측 (전부 실제 MFE):**
- **[유력] complement 열거**: enum-dispatch owise를 or-gate 대신 **미매치 생성자 튜플마다 ground
  fall-through 절**로 반사 → 모든 절 ground·disjoint → 임계쌍 0 → YES. `$join_ctk`(DYN관련 5절)
  **CRC YES**, `$assignop`(plain-`=` 1절) **CRC YES**. 국소적·비폭발. 실행표면 무변경(분석 전용).
  미매치수 = ∏(arg타입 생성자수)−매치수 → **size guard** 필요(대형/고arity는 or-gate 폴백).
  v1 스코프: 모든 형제 arg가 **nullary 생성자**인 순수 enum-dispatch(두 대상 다 해당).
- **[기각] `or [assoc comm]`**: AC 매칭이 참 disjunct를 위치무관 발견 → `$join_ctk` **YES**(진단 확증).
  그러나 `$assignop` 12-way 게이트에서 **AC 단일화 폭증, 850s 내 verdict 없음**(baseline은 <420s에
  MAYBE 완주). + `or` 전역 변경이라 타 슬라이스 회귀 위험. 실전 부적합.

**[완료, 2026-07-16] complement 열거 정식 구현 — `Reflect.owise`(pipeline은 여전히 분석
전용).** v1 스코프를 조건부 형제까지 확장해서 구현(사용자 지시): 인자 위치를 **enum**(형제 중
누군가 nullary 생성자를 놓는 자리, 선언 타입이 nullary-only variant로 열거 가능해야 함)과
**pass-through**(전 형제가 변수)로 분류, enum 위치의 곱집합을 튜플별로 순회 —
- 무조건 형제가 커버 → owise 도달불능, 절 생략
- 조건부 형제만 커버 → ground-head 절 + **형제 가드 부정의 논리곱**(형제당 `g=false` 1개,
  or-gate 아님) — 실측 대상 2건은 형제가 전부 무조건이라 이 경로 미시험(v2 설계로 남김,
  todo.md에 명기)
- 아무도 안 커버 → 순수 ground fall-through (v1 원안)

`max_complement=16` size guard. `sibling_guard`에서 조건-반사부를 `sibling_conds_guard`로
추출해 재사용(리팩토링 단계에서 p4 corpus `--ctrs`/stderr **byte-identical** 확인 후 기능
추가). `reflect.ml`의 owise 단계를 `Array.mapi`(1규칙→1규칙)에서 `List.concat(List.mapi…)`
(1→N)로 교체.

**실측 결과(전부 실제 MFE, `verify --symbol … --timeout 900`):**
- `$join_ctk` **CRC YES**(5절, 전부 무조건 — 7/15 수동 실측 재현), `$assignop_as_binop`
  **CRC YES**(1절, 전부 무조건 — 동일 재현).
- p4 corpus 전체에서 새로 열거된 심볼은 예상외로 `$join_flow`도 포함(3절, 무조건) — **회귀
  확인: YES 유지**(기존에도 YES였던 대상). impty 대조군 `$lookup` **YES**(환경 정상).
- 반사 수 보존: `72 owise rule(s)` → **`69 reflected + 3 complement-enumerated`, kept 0**
  (7/15 표에서 인용한 `71/1`은 stale — 실측 기준 정확한 불변식은 `72/0`).
- p4 corpus `--ctrs` diff: 위 3심볼 절만 변화(전부 `ceq…or(...)=false` → `eq` N개), 그 외
  0. 실행 표면(`rewrite` 출력) sha256 편집 전후 동일 — 분석 전용 패스라 자동 보장, 실측으로도
  재확인. impty 골든(`spec.ctrs`/`spec.maude`) 둘 다 diff 0.

**v2 후보(미구현, 실측 미검증)**: 조건부 형제의 부정-누적 경로. 사용자가 "실측은 안 돼도 구현은
해달라"고 명시해 코드는 포함했으나, 현재 corpus엔 이 경로가 발화하는 MAYBE 대상이 없어 CRC
개선 효과를 검증할 표본이 없다. **다음에 owise CRC MAYBE가 새로 나타나고 그 형제가 조건부면
가장 먼저 확인할 지점** — `sibling_conds_guard`가 이미 재사용 가능한 형태로 분리돼 있음.
guarded 절 발생 시 회귀 나오면 자격 검사에 "전 형제 무조건" 요구를 되살리는 최소 revert로
대응(설계는 위 커밋 참고).

- [x] **`Exp_map` 재생성** (done) — IL 얕은 traversal(`map_subexps`/`subexps`/
  `exps_of_prem`). `Defunctionalize`가 사용.
- [x] **`Simplify` = identity** (done) — 이 프로젝트는 단순화를 하지 않음. `Prem_env`도
  재구현하지 않음(그것만 먹이던 엔진이라 불필요).
- [x] **`To_ctrs.of_spec`** (심장; `rewrite` 브랜치에서 포팅, done) —
  `prelude` + `defs_of_typ` + `term_of_exp` + `conds_of_prem` + `rules_of_def` +
  반복/subtype 헬퍼 + `prune_unused`. 결과 레코드 `{ R.vars; rules }`. `Prem_env`
  의존(`find_rel_in_spec`)은 로컬 인라인. impty/base에서 분석/실행 둘 다 실행됨.
  ✅ **`~scalars:Native`는 이제 생성 시점 직접 방출**(`Ctrs_term`의 mode-aware
  leaf 빌더 + `~scalars` threading; post-fold 제거) — 아래 "Native 직접 생성
  리팩토링 (B)" 완료.
- [x] **`To_ctrs.var_type_hints`** (포팅, done) — `VarE` note에서 변수 narrow 타입
  복원(To_maude용). 헬퍼 `collect_var_types`/`collect_prem_var_types`/
  `resolve_var_types`도 함께 포팅.
- [x] **검증: impty/base CTRS 골든** byte-identical (done) — `specs/impty/base/spec.ctrs`
  고정(`rewrite --ctrs` 덤프). 기본 `rewrite`는 실행 모듈을 내므로 분석-CTRS 골든은
  `--ctrs` 경로. 실행(Native) 모듈 골든은 아직 미고정(원하면 `spec.maude`로 별도 핀).
- [x] **`Mfe` calibration** (done) — `mfe.ml`을 실측 프로토콜로 재작성(entry `src/mfe.maude`,
  `MAUDE_LIB`, stdin 파이프, `(select tool …)`+`(check …)`, EOF flood→streaming
  SIGKILL, verdict 토큰; [tools/mfe/README.md](../../tools/mfe/README.md) Calibration).
  impty/base에 **per-symbol slice**(`verify --symbol`, `rewrite --ctrs --symbol`)로
  CRC+ChC 적용한 결과:

  | symbol | CRC | ChC | 비고 |
  |---|---|---|---|
  | `$lookup` | YES | YES | owise + 서로소 `match_*` 가드로 합류 |
  | `Check_expr`/`Check_command`/`Check_prog` | YES | YES | |
  | `Eval_expr`/`Eval_command` | YES | YES | |
  | `Eval_prog` | **MAYBE** | YES | `ccp SPEC226` 미합류(아래) |
  | `Run_prog` | TIMEOUT | TIMEOUT | 전체-reachable, critical pair 폭증(>150s) |

  - **ChC가 전부 YES**인 이유: impty/base는 `rl`/`crl`이 0개(모든 relation이
    input-moded → 등식)라 coherence가 vacuous.
  - **`Eval_prog` 비합류는 진짜이며 알려진 한계의 발현**: 규칙
    `Eval-prog(command) = env if Eval-command(nil, command) = env`의 결과 `env`가
    **전제로만 묶이는 RHS 자유변수**라, CRC가 같은 불투명 `Eval-command(nil,command)`의
    두 증인 `env`/`#env#`를 합류시키지 못함(ccp SPEC226). 상수 RHS인 `Check-prog`
    (`= true if … = tenv`)는 자유변수가 안 생겨 YES. 실행 표면(`to_maude`)은 이
    전제를 `:=`/`=>` 조건으로 내보내 문제없음 — 분석 표면의 join-condition `=` 근사가
    원인(같은 뿌리: "variable used before bound" 경고). **번역 버그 아님.**
  - **참고**: `$lookup`(owise 보유)이 YES인 건 owise 형제 절들이 서로소 `match_*`
    가드를 써 CRC가 임계쌍을 infeasible로 처리하기 때문. owise 자체가 안전한 건
    아니다(아래 p4 sweep의 2번째 원인 참조).

  **p4 spec sweep (per-symbol, slice 1–200 규칙 150개; degenerate 9개; >200규칙
  415개는 전체-시스템급 → TIMEOUT 예상이라 미실행).** `verify --list-symbols --sizes`로
  슬라이스 크기를 한 번에 구해 tractable한 것만 MFE에 돌림. 아래는 `fold_premise_binders`
  적용 후 **전체 150개 재-sweep**(authoritative; `--timeout 60`, `-P 3`):

  | CRC verdict | n | 비고 |
  |---|---|---|
  | YES | 125 | (fold 전 104 → +21; `$bin_mod` 포함 — mod overlap 임계쌍이 막지 않음) |
  | **MAYBE** (비합류 아님) | 18 | 세 분류 — 아래 |
  | TIMEOUT | 7 | `$write_*_from_bits` 6종 + `$assignop_as_binop` — 비트쓰기 임계쌍 폭증 |
  | (규칙 0개 degenerate) | 9 | `$find_overloaded*`/`$match_overloaded_*` 등 — 정의 규칙 없음, N/A |

  **남은 18 MAYBE의 가드별 분류**(전부 결정적 함수, 진짜 비합류 0 — CRC 불완전성):
  - **A. owise 중첩 (9)**: `$is_lpm_key_prime`·`$requires_priority_prime`·
    `$is_default_parameterIR`·`$join_flow`·`$join_ctk`·`$is_tableDefaultActionProperty`·
    `$optional_annotation_…_prime_prime` (+ 호출만 하는 `$is_lpm_key`·`$requires_priority`).
    가드가 전부 `match_*`/텍스트 등식 → **negation wall 없음** → 아래 (A) owise-보완으로
    깔끔히 해소 가능.
  - **B. owise-없는 순수 `match_*` 케이스-분기 (2)**: `$un_op`·`$inherit_i` → 아래 (B)
    discriminator head-패턴 폴드로 바로 안전하게 YES.
  - **B′. subty 섞인 케이스-분기 (7)**: `$flatten_constOpt`·`$tableCustomName`·`$name`·
    `$prefixedTypeName`·`$prefixedNonTypeName`·`$invalidate_value`(+`$invalidate_headerUnion`).
    `subty_*`는 RHS가 리터럴이 아니라 discriminator가 아님 → 단순 폴드로 **안 됨**.
    subty 절 disjointness 증명이라는 별도 메커니즘 필요(미설계).
  ⇒ (A)+(B)로 11/18 해소 가능, 나머지 7은 subty-disjointness 신규 작업.
  **(2026-07-07: B′ 7개 전부 해소 — `Reflect.expand_subty_guards`, 아래 권장 순서
  섹션의 2026-07-07 항목 참조.)**

  ChC는 (출력이 나온 모든 경우) YES — p4 분석 모듈도 `rl`/`crl` 0개라 vacuous.
  **비합류(MAYBE) 33건은 두 원인으로 갈린다(둘 다 분석 표면 근사, 번역 버그 아님 —
  실행 표면은 `:=`/`=>` + owise 보완으로 처리):**
  1. **전제로만 묶이는 RHS 자유변수** (impty `Eval_prog`과 동류; 지배적):
     `$dom_map`/`$codom_map`(`= set{K} if $unzip_K(it)=K`, ccp SPEC4),
     `$empty_store`(`= store if $empty_map=store`), `$ctk_of_typedExpressionIR`/
     `$type_of_*`(`= ctk if it = variant(..ctk..)` 구조분해 바인딩) 등.
  2. **owise 중첩** (todo.md P2 `otherwise` 예측대로 발현):
     `$is_lpm_key_prime` = `ccp SPEC1: false = true if text = "lpm"` — 절1
     `= true if text="lpm"` vs 절2 `= false [owise]`인데 **Maude CRC가 owise를
     임계쌍 생성에서 무시**해 두 절이 충돌. 가드가 raw 등식/구조라 CRC가 owise 보집합을
     disjoint로 못 봄(`$lookup`은 가드가 `match_*`라 회피).

  **원인 1(자유변수)을 `fold_premise_binders`로 해소 (done).** 분석 파이프라인
  (`Pipeline.ctrs_of_spec`)에 마지막 패스를 추가해 전제로만 묶이는 변수를 규칙에 도로
  접는다([rewrite_system.ml](rewrite_system.ml)): (a) 관계/함수 **출력** 바인더
  `(prod, v)`(v 비-head)를 rhs/조건에 **인라인**, (b) **순수 접근자** 구조분해
  `(v, K(..))`(v head-bound, 다른 조건엔 안 나옴)를 **head 패턴으로 폴드**. iteration
  helper(`$iterapply`/`$itercollect`/`$unzip`) 안의 재귀 바인더도 동일 처리. **분석
  표면 전용**(실행 모듈 byte-identical; `To_maude`는 `:=` 유지). **수술적(surgical)**
  으로 제한 — 가드 절(head 변수가 `match_*`/owise 가드에도 쓰이는 `$lookup` 등)을 폴드하면
  CRC가 의존하던 disjointness 가드가 사라져 owise 중첩이 노출돼 **YES→MAYBE 역행**하기
  때문(aggressive 변형은 impty 6개를 실제로 역행시켜 폐기). 효과: impty·기존-YES 무역행,
  p4의 33 MAYBE 중 **19개가 YES로**(출력-바인더 + 순수 접근자, iteration 포함;
  `$dom_map`/`$ctk_of_*`/`$type_of_*`/`$set_priorities_..._prime` 등). 남은 14개는
  **원인 2(owise)** 와 일부 multi-clause — owise-보완 별건.

  **남은 14 MAYBE의 두 분류와 추가 해소안(미구현).** 잔여는 전부 "진짜 비합류"가
  아니라 CRC가 임계쌍 전제의 disjointness/unsatisfiability를 못 푸는 불완전성이다.
  번역 단계에서 disjointness를 **구문적으로 노출**해 주면 대부분 YES로 떨어진다:

  - [x] **(B) owise-없는 케이스-분기 절의 head-패턴 폴드 — 구현 완료 (2026-07-03).**
    `$un_op`/`$tableCustomName`류는 동일 head `$un-op(unop,value)` 위에
    `if match_X(unop)=true` 가드 4절이라 CRC가
    `$un-bnot(value)=$un-lnot(value) if match-tilde(unop)=true /\ match-bang(unop)=true`
    같은 임계쌍을 만들고 두 match가 배타적임을 못 본다.

    **구현 계획(새 엔진 불필요 — 기존 `fold_premise_binders` 재사용).** 처음엔
    CRC-facing 전용 "discriminator fold" 패스를 새로 설계하려 했으나, 조사 결과
    `Rewrite_system.fold_premise_binders`([rewrite_system.ml:264](rewrite_system.ml#L264))가
    이미 `v = K(fresh..)`형 구조적 동등식을 만나면 head로 접는 로직(+ 안전 게이트:
    `is_ctor_pattern`/"다른 조건에 안 쓰임"/"LHS에 1회만 등장")을 갖고 있음을
    발견했다 — `MatchE`(matches) 가드만 `cond_of_match`
    ([to_ctrs.ml:840-851](translate/to_ctrs.ml#L840-L851))가 항상
    `match_sym(subj)=true`(불투명 술어) 형태로 내보내서 이 기존 메커니즘을 못
    탈 뿐이다. 그래서 새 엔진 대신, `match_sym(v)=true`를 동등식
    `v = K(fresh..)`로 **재철자**만 해주는 작은 패스(`Reflect.hoist_matchers`,
    신설)를 `fold_premise_binders` 바로 앞에 끼워 넣고, 나머지(head-bound 여부,
    안전성, 실제 치환)는 전부 `fold_premise_binders`에 위임한다.

    **`IterPr` 내부에 중첩된 `IfPr`도 별도 재귀 스캔 없이 자동으로 커버된다** —
    두 패스 다 `to_ctrs` 이후의 **flat한 CTRS rule 리스트**를 균일하게 훑기
    때문에, `iterpr_defs`([to_ctrs.ml:862-974](translate/to_ctrs.ml#L862-L974))가
    만드는 반복 헬퍼의 step rule도 그냥 "리스트의 규칙 하나"라 캡처 변수
    (`iter_captured`/`captured_fvs`, [to_ctrs.ml:62-138](translate/to_ctrs.ml#L62-L138))든
    원소 변수든 같은 경로로 처리된다(헬퍼 심볼당 step rule이 1개뿐이라 sibling
    경쟁이 없어 안전 — 가드가 head로 옮겨가도 발화 조건 집합이 동일).

    - **범위**: `CaseP`(임의 arity, variant-case 역조회) + `OptP` `\`Some`/`\`None` +
      `ListP` `\`Cons`/`\`Nil`. 옛 `Prem_env.reconstruct_pattern`이 companion-let
      충돌을 우려해 배제했던 Some/Cons까지 포함 — `fold_premise_binders`의
      "다른 조건에 안 쓰임" 게이트가 그 충돌을 이미 구조적으로 막아준다는 게
      이번 결론(실측으로 확인 필요, 아래 검증 참조). `ListP` `\`Fixed n``(길이
      체크, `len(subj)=n` 형태)은 코드 경로가 달라 이번엔 보류.
    - **배치**: `translate/reflect.ml`에 새 함수 `hoist_matchers`
      (`~orig:spec -> Rewrite_system.t -> Rewrite_system.t`)로 추가 — `owise`가
      이미 만들어 둔 `tables`/`build_tables`/`variant_case`/`matcher_type`/
      `fresh_vars` 재사용. 매처 심볼 → (타입, mixop, 필드타입) 역테이블은
      `build_tables`의 `ctor_types` 구성 방식을 그대로 거울처럼 뒤집어서 만든다
      (`T.match_sym ty mixop`를 타입 순회하며 계산 — 문자열 파싱 없이 안전).
      `match_some`/`match_none`/`match_cons`/`match_nil` 4개는 고정 이름으로
      직접 인식(각각 fresh 0/1/0/2개).
    - **파이프라인**: `pipeline.ml`의 `ctrs_of_spec`에서 `Gensym.thread` 직후,
      `Rewrite_system.fold_premise_binders` 바로 앞에 `Reflect.hoist_matchers`
      삽입. `maude_system_of_spec`(실행)에는 절대 넣지 않음 — 기존
      owise/fold_premise_binders와 동일하게 분석 전용.
    - **범위 밖(유지)**: subty 섞인 (B′) 7개(`$flatten_constOpt`/`$tableCustomName`/
      `$name`/`$prefixedTypeName`/`$prefixedNonTypeName`/`$invalidate_value`
      (+`$invalidate_headerUnion`))는 이 fold로 안 풀림 — subty totality가 만든
      case-membership 정보 기반 multi-way clause 전개가 별도로 필요(멤버
      생성자 수만큼 클로즈 복제, 단일 substitution이 아님 — 위 subty totality
      절 참조). `IterPr` 원소 자체의 매치로부터 **스트림 전체 모양을 재구성**하는
      것(옛 `Prem_env.env_of_prem`의 `IterPr` 승격 트릭, `IterE`로 감싼 pair
      생성)도 범위 밖 — 이건 head disjoint화가 아니라 premise 단순화 성격이라
      지금 기록된 MAYBE 원인 어디에도 필요하지 않다.
    - **검증 계획**: impty 골든(`spec.ctrs`) byte-identical(해당 케이스 없어
      no-op) → `verify --symbol '$un_op'`/`'$inherit_i'` MAYBE→YES 확인 → 기존
      YES 표본 재검(회귀 없는지, 특히 Some/Cons 확장 리스크) → 실행 표면
      (`spec.maude`, `run --p4 --check-p4`) 무영향 확인.
    - **구현 결과 (2026-07-03).** `Reflect.hoist_matchers`를 계획대로 추가하고
      `pipeline.ml`의 `ctrs_of_spec`에서 `Gensym.thread` 직후 · `fold_premise_binders`
      바로 앞에 배선. **계획에서 한 가지 실측으로 드러난 보정**: naive하게
      "`match_K(subj)=true`는 전부 재철자"로 구현했더니 impty 골든이
      **비-no-op**으로 깨졌다 — `Check-command`/`Check-expr`/`Eval-expr`/
      `Eval-command`/`$lookup`처럼 SpecTec 엘라보레이터가 `matches` 가드(바인딩
      없음)와 필드-바인딩 `let K(x,y)=subj` 구조분해를 **별개의 두 premise**로
      내보내는 경우, 재철자된 `subj=K(fresh..)`가 기존 `subj=K(x,y)`와 같은
      `subj`를 다시 언급해 서로의 "다른 조건에 안 쓰임" 게이트를 막아버려
      **아무것도 안 접히면서** opaque `match_*` 조건만 죽은 fresh 변수투성이
      동등식으로 바뀌는 순수 노이즈가 됐다(=fold는 여전히 안 되는데 텍스트만
      나빠짐). 고쳐서, `hoist_matchers`는 이제 **subj가 bare 변수이고 같은
      rule의 다른 어떤 조건에도 그 변수가 안 나올 때만** 재철자한다 — 이
      가드가 있으면 companion-let이 있는 절은 원래 `match_*` 조건을 그대로
      두고(=이전과 동일 출력), companion이 없는 절(discriminator만 있는 경우,
      즉 (B)가 실제로 겨냥한 경우)만 재철자→폴드된다.
      - **impty 골든 갱신(byte-identical 아님, 의도된 3건 개선)**: `$lookup`의
        nil 베이스케이스, `Check-command`/`Eval-command`의 SKIP 케이스가
        head-패턴으로 접혔다(전부 필드 없는 nullary 케이스라 companion
        destructure가 애초에 없음 — "해당 케이스 없어 no-op"이었던 원래
        가정이 부분적으로 틀렸던 것; impty에도 대상 케이스가 3건 있었다).
        나머지(Check-expr/Eval-expr/필드 있는 $lookup cons 케이스 등)는
        companion-let 때문에 게이트대로 스킵 — 출력 불변. `$lookup`의 owise
        가드에서 nil-sibling 항이 `match-nil(..)`→`eqg(..,nil)`로 바뀌었는데,
        이는 sibling1의 head가 이제 리터럴 `nil`이라 owise reflection의
        `ptest`가 (더 이른 순서의) "ground 패턴은 eqg 한 방" 분기를 타기
        때문 — 유니파이어 하에서 `eqg(nil,nil)⇒true`로 즉시 반사적으로
        붕괴하므로 기존 hypothesis-rewriting 경로보다 오히려 더 직접적이고
        안전(critical-pair discharge 의미는 동일). `spec.ctrs` 갱신,
        `spec.maude`(실행 표면)는 무변화 확인(hoist_matchers는 분석
        파이프라인에만 배선).
      - **MFE 실측(`$un_op`/`$inherit_i` MAYBE→YES 확인, `$is_lpm_key_prime`
        회귀 재검, impty `$lookup` 대조군) — 이번 세션에서 완주 못 함, 환경
        차단.** `verify --symbol '$lookup' spec.spectec`(과거 ~1.4s로
        기록됐던 대조군)가 `--timeout 900`(15분)에도 TIMEOUT. `git stash`로
        **코드 변경을 뺀 베이스라인**에서 동일 슬라이스를 재현해도 60s
        타임아웃까지 동일하게 TIMEOUT → **이 세션의 코드 변경과 무관한
        기존(pre-existing) 환경 문제**로 확인(회귀 아님). Maude 바이너리
        자체는 정상(단순 `reduce 1 + 1 .` ~0.17s). MFE 프로토콜을 수동으로
        재현해 관찰한 결과, 모듈 로딩 직후 "variable used before bound"
        경고 3개 세트가 30초 지점과 90초 지점에서 **글자 그대로 반복
        출력**됨 — CRC 내부가 critical pair마다 모듈을 재적재/재선언하는
        것으로 보이며, [Maude 배치 폴백 슬로우다운] 메모의 "50k줄 모듈
        재내부화" 현상과 결이 비슷하다(다만 이건 MFE `check`의 내부 동작이라
        `check_diff_p4.sh`와는 별개 경로). [MFE 환경 슬로우다운] 메모의
        "심볼당 ~4분"보다도 더 나빠진 상태로 보임 — 재측정 전까지 MFE
        확인은 이 환경에서 사실상 막혀 있다고 간주. **다음 세션에서 할 일**:
        환경(WSL 메모리/디스크) 점검 또는 다른 머신에서 `verify --symbol
        '$un_op'`/`'$inherit_i'`/`'$is_lpm_key_prime'`/impty `'$lookup'`
        재실행해 MAYBE→YES 전환과 무회귀를 확인.
        **(2026-07-07 해소: 차단은 옛 WSL 환경 고유 문제 — 현 dev 컨테이너에서
        `$lookup` 1초 YES/YES, `$inherit_i` YES/YES. `$un_op`은 슬라이스 크기
        문제로 재분류. 권장 순서 섹션의 2026-07-07 항목 참조.)**
  - [x] **(A) MFE 입력에서 owise 규칙 drop (구현·검증 완료 — 옛 "owise-보완 번역"안 대체).**
    `$is_lpm_key'`/`$requires_priority'` 류는 `= true if text="lpm"` + `= false [owise]`인데
    CRC가 owise를 임계쌍 생성에서 무시해 `false=true if text="lpm"` 충돌. **단순 confluence
    게이트엔 owise 규칙을 그냥 빼고 검사하면 건전하다:** owise는 sibling이 안 맞을 때만
    발화하므로 sibling과의 overlap은 **구조적으로 infeasible**(허상 임계쌍)이고, op당 owise
    1개라 owise끼리도·다른 head와도 안 겹친다. ⇒ owise를 빼면 그 허상만 사라지고 진짜
    비합류(sibling overlap)는 남아 **false YES(버그 은폐) 불가**; 최악이 false MAYBE(합류에
    owise 스텝이 필요할 때 — owise RHS가 상수라 실측상 안 생김)라 게이트엔 보수적·안전.
    **`.ctrs` 정본은 건드리지 않는다**(termination 등 다른 용도가 owise를 필요로 함) —
    drop은 [mfe.ml](mfe.ml) `check`에서 `Rewrite_system.drop_owise`로 **MFE에 넘기는
    시스템에만** 적용(`ctrs_of_spec`/골든/slice-size 집계는 owise 유지). **negation wall
    완전 우회**(보집합을 항으로 표현 안 함). **검증(실측, 무회귀):** owise-9 중 **6개 YES**
    (`$is_lpm_key'`·`$is_lpm_key`·`$requires_priority'`·`$requires_priority`·
    `$is_default_parameterIR`·`$join_flow`). 남은 3은 owise 탓이 아니다 — `$join_ctk`는
    자기 4 `match_*` 절(같은 head, (B) 필요), `$is_tableDefaultActionProperty`/
    `$optional_annotation_…'`는 슬라이스가 B′ 의존(`$tableCustomName`/`$name`, subty
    overlap)을 끌어옴. impty/base 골든 byte-identical, 기존-YES 무회귀. **전체: 18 → 12 MAYBE.**
  - [x] **subty totality — 사용-기반 false-보완 (구현 완료 2026-07-02; B′의 전제 조건).**
    `subty_<T>`가 positive-only(비멤버 무규칙→stuck)라 분석 표면에서 `~(e <: T)`가
    죽은 조건이 되고 B′ 임계쌍의 disjointness를 CRC가 볼 수 없었다. **구현:**
    모든 `SubE(e,T)` 사이트에서 (타겟 T, 주어 정적 타입 S=e.note) 쌍을 수집해
    (alias unwrap + tuple/list/opt 원소 co-descent), 둘 다 variant면
    `subty_T(ctor_c(x..)) -> false` 를 c ∈ cases(S)\cases(T)(키 = origin+mixop+arity,
    `variant_sym` 키잉과 동일)별로 방출 — [translate/to_ctrs.ml](translate/to_ctrs.ml)
    `sub_complement_defs`(+ 최상위 `typcases_of`/`unalias`), `of_spec`에서
    `sub_helper_defs` 뒤에 연결. interp.subtyp(interp/eval_il/interp.ml:210-258,
    **total boolean**)과 같은 의미론 — negation-as-failure가 negation-as-false-value로.
    **of_spec 공통 레벨**(분석+실행; 실행에선 To_maude subty owise complement가 주던
    값과 동일해 명시 규칙이 선점, complement는 잔여 head 흡수용으로 유지).
    - 산출: p4 보완 규칙 5425개(무조건·상수 RHS라 슬라이스에 자기 자신만 추가);
      origin-vs-mixop 충돌 경고(P3급) 0건; impty 골든은 elaborator 삽입 SubE
      (`literal`/`id` ⊂ `expr`) 보완 11줄 순수-추가로 재생성; impty 실행 정상.
    - **MFE 재검 결과 (timeout 360s, 2026-07-02):**
      | symbol | CRC | ChC | 비고 |
      |---|---|---|---|
      | `$is_lpm_key_prime` (대조군, byte-identical) | YES | YES | **무회귀** (~4분 소요) |
      | `$flatten_constOpt` (B′) | MAYBE | YES | totality만으론 미해소 — 아래 |
      | `$invalidate_value` (B′) | TIMEOUT | TIMEOUT | 360s도 부족(149규칙+느린 환경) |
      | `$tableCustomName` (B′) | TIMEOUT | TIMEOUT | 동일(255규칙) |
      (표본 중단 — 잔여도 같은 패턴 예상; 환경 복구 후 재측정할 것)
    - **⚠️ 환경 변화 — MFE 벤치 기준 무효화 (2026-07-02):** 7/1 sweep 당시 60s면
      되던 심볼이 지금은 **심볼당 ~4분**(byte-identical 대조군으로 확인 — 코드 변경과
      무관). WSL RAM 16→7GB 등 환경이 느려짐. 이후 verify/sweep은 `--timeout 360`+
      이상을 기본으로; 과거 TIMEOUT 표는 재측정 전까지 환경-보정 없이 비교 금지.
    - **B′가 totality만으로 안 풀리는 이유(실측 확인):** 임계쌍의 두 절이 같은
      head `f(x)`라 조건 `match_*(x)=true /\ subty_*(x)=true`가 **변수 x에 대해
      심볼릭**하게 남고, CRC는 x를 생성자 케이스로 분할하지 않는다 — 보완 규칙은
      x가 구체 생성자일 때만 발화. ⇒ **B′의 실제 해법은 (B) discriminator fold의
      subty-가드 확장**: subty가 total해진 지금 `subty_A(x)=true` 가드의 true-집합이
      구문적으로 정확히 열거되므로(멤버 케이스 = true 규칙), 그 절을 멤버 생성자별
      head 패턴으로 안전하게 펼칠 수 있다(`$flatten(EMPTY)=none` /
      `$flatten(CONST)=some(CONST)` — head 서로소 → 임계쌍 소멸). totality가 이
      폴드의 건전성 근거. (B) 구현 시 match-discriminator와 subty-discriminator를
      함께 다룰 것.
  - [x] **subty false-보완의 실행 무회귀 표본 (확인 완료 2026-07-02).** suite 균등
    추출 47 + casting-heavy(cast-call/issue447-2-bmv2/key-bmv2) = **49개 표본,
    13개 단위 serial 배치 4회, 49/49 `result: MATCH`** (Phase B verdict + Phase D
    결과-VALUE 일치; MISMATCH/FAIL/TIMEOUT 0). impty 실행도 8/8 정상
    (skip/arith/assign/bool_ops/hello/ite/loop/shadow). 내부화 비용은 배치당
    ~60s(13파일, 5,425 eq 추가 후; 느려진 환경 기준)로 상각 유지. 같은 검증에서
    **stale 실행 골든 발견·재생성**: `impty/base/spec.maude`가 matcher-sort 협소화
    (48e59d5f)와 subty 보완(5a406ccc)을 반영하지 않은 채 남아 있었다 — diff는
    정확히 그 두 변경(match-* 34줄 sort 변경 + subty-* 11줄 추가)뿐임을 분류 확인
    후 재생성.
  - [x] **owise 반사 + judgment 반사 (구현 완료 2026-07-02, 커밋 752c82f8 — (A) drop을
    충실 인코딩으로 대체).** 분석 전용 `ctrs_of_spec` 최종 패스
    [translate/reflect.ml](translate/reflect.ml). MFE CRC의 실측 메커니즘(조건을
    가설로 서로 재작성, 생성자 narrowing 없음 — 미니모듈 T1–T5로 검증)에 맞춰:
    - **owise**: 반사 가능한 owise 절의 조건에 `or(g_1..g_k) = false`(선행 형제
      적용가능성의 or) 추가 + owise 플래그 클리어. g는 형제의 **번역된** head
      패턴(match/proj, 단락 `and` 뒤)과 조건에서 구성 — 같은-subject 정렬이
      가설 접힘의 전제. p4 **51/72 반사**, impty `$lookup` 반사(골든 재생성).
    - **judgment 반사**: 부정되거나 owise 형제에 나오는 무출력 judgment R에
      `holds_R(xs) = or(g_1..g_n)` **무조건 규칙 1개**(임계쌍 0) 생성, 시스템
      전체에서 `R(in)=true/false` 조건을 `holds_R`로 재철자(양성 포함 — CRC에겐
      R과 holds_R이 무관 심볼이라 통일해야 정렬; 무출력 R의 자기 규칙은 전부
      RHS true라 검증력 손실 없음). no-binding IterPr은 totalized and-fold
      `holds_$iterall..`(+길이불일치 false). p4: `Type_alpha`·`ParameterType_alpha`
      + iterall 15개; **`Type_alpha:/` 죽은 절 8개가 충족 가능해짐**.
    - 잔여 21 kept(게이트, stderr 사유 로그): 수집형 iteration 헬퍼
      (`$itercollect`/`$iterapply` — 아래 후속 확장; `Cast_impl_neq`→`Cast_impl`→
      `$cast_unary`/`$cast_binary`가 이 체인에 막힘), gensym-threaded `$subst_*` 5,
      ambiguous matcher 1. `drop_owise`는 이 잔여분 fallback으로 유지.
      **(2026-07-03 후속으로 21→6까지 반사 확장됨 — 아래 "owise 반사 확장" 항목
      참조; 이 21의 스냅샷은 그대로 둠, 최신 잔여는 아래 항목의 6개.)**
    - 실행 표면 byte-identical; support(match/proj) 규칙은 prune 이후라 필요 시
      재생성(트랜잭션, 미사용 필터). `holds_*`는 BoolV 시그니처(Maude_sorts).
    - **가드 등호는 `eqg` (a7d85f94) — 구조적 `eq` 금지.** 가드에 `eq` 하나만
      넣어도 head-기반 slice가 전 타입 eq 패밀리(47,213규칙)를 끌어와
      `$is_lpm_key'` 슬라이스가 4→47,217규칙으로 폭증, CRC가 어떤 timeout으로도
      안 끝났다(div_int 교훈의 재발). CRC가 필요로 하는 건 대각뿐 — 임계쌍
      unifier 아래서 형제 가설이 양변을 문자 그대로 같은 항으로 재작성하므로
      (ground 리터럴/비선형 재출현/`t = t'` 모두) 비선형 규칙 하나
      `eqg(x, x) = true`로 전부 접힌다. 대각 밖 stuck = 보수적 MAYBE(false YES
      불가). 슬라이스 3규칙 복귀.
    - **MFE 실측 (2026-07-02, timeout 600s, 느려진 환경):**
      | symbol | CRC | ChC | 비고 |
      |---|---|---|---|
      | `$is_lpm_key'` (p4, 가드 포함 3규칙) | **YES** | **YES** | drop_owise가 규칙을 빼고 얻던 YES를 **충실 인코딩으로 재현** |
      | `$join_ctk` (p4) | MAYBE | YES | 예측대로 — 잔여 쌍은 owise가 아니라 **형제끼리** 같은-subject 다른-matcher(B의 몫: discriminator fold) |
      | `$lookup` (impty, 18규칙) | TIMEOUT@1800s | — | eq 폭증 아님(슬라이스 정상). 3규칙이 4분 걸리는 현 환경에서 조건부(unzip/itermap) 18규칙 슬라이스의 CRC 비용으로 추정 — 단, 현 환경 기준선이 없어(7/1의 YES는 빠른 환경 + drop_owise 측정) 미확정. 환경 복구 후 drop_owise 대조와 함께 재측정할 것 |
      150-슬라이스 재-sweep은 환경 복구 후(심볼당 4분+ 현재로는 ~10시간) 진행할 것.
      게이트의 안전성상 어떤 결과든 false YES는 불가(반사 가드는 discharge를
      돕기만 하고, 실패 시 보수적 MAYBE).
  - [x] **owise 반사 확장 — 수집형 IterPr 성공 반사 (구현 완료 2026-07-03).**
    reflect 게이트가 skip하던 `$itercollect`/`$iterapply`를 위한 후속
    [translate/reflect.ml](translate/reflect.ml). 재구성 없이 형제의 바인딩
    조건(`$itercollect_S(args), ys`)은 그대로 두고 세 메커니즘으로 연다:
    1. **성공 반사 생성기** `gen_itercollect_holds`/`gen_iterapply_holds`
       (`gen_iterall_holds`와 같은 골격, base→`true`/step→and-fold; 공유
       스핀-불일치 로직은 `iter_spine_mismatches`로 분리). `$itercollect`는
       step rhs가 `cons(collected, S(rec))`/`some(collected)`라 재귀 인자를
       cons의 둘째 항에서 뽑고 collected는 버림(성공 반사엔 무의미) —
       step 조건은 `gen_iterall_holds`처럼 그대로 반사. `$iterapply`는 절 자체가
       무조건(step 조건 없음, 원소가 relation call 항 그대로)이라 g_inner를
       `holds_<call 심볼>`로 직접 재구성.
    2. **전역 성공-테스트 삽입** (`insert_success_test`): `S`가 성공-반사된 모든
       규칙에서, `S`를 호출하는 바인딩 조건 `(App(S,args), r)`(`r`이 불리언
       리터럴이 아닌 경우) 바로 앞에 `(holds_S(args), true)`를 삽입 — owise
       형제뿐 아니라 시스템 전역(수집 성공은 의미상 함의라 안전; 분석
       전용이라 실행 무영향). `replace_cond`(1→1 매핑, 불리언 조건 전용)로는
       처리 못 하는 항상-바인딩 케이스를 `List.concat_map` 기반 삽입으로 보완.
    3. **게이트 완화**: `check_reflectable`이 `succ`(현재 시도의 후보 집합/
       최종 성공 집합)를 받아, 이미 성공-반사된 iteration helper 호출은
       `iter_helper_prefixes` 게이트를 통과시킴 — (2)가 심어둔 테스트 덕에
       형제의 원래 바인딩 조건이 ctest에서 그대로(“ys := S(args) 그대로
       인라인”) 처리됨, 별도 명시 처리 불필요(설계대로 확인).
    - **부수 발견 + 수정**: `cond_heads`(후보 종속성 탐색)가 조건의 최상위
      head만 보던 얕은 스캔이라, `fold_premise_binders`가 이미 인라인해 둔
      바람에 다른 호출의 **인자 위치에 중첩**된 iteration-helper 호출을
      놓쳐 `Cast_impl_neq`/`Cast_impl`/`$cast_unary`/`$cast_binary` 사슬이
      안 열렸다. `cond_heads`를 조건 term 전체(양변)를 재귀 순회하는
      `term_heads`로 교체해 해결(=check_reflectable이 실제로 보는 것과 동일
      깊이로 탐색을 맞춤).
    - **결과 (p4)**: owise 51/72 → **66/72 반사**(21 kept → **6 kept**: 잔여는
      `$subst_typeIR`/`$subst_parameterIR`/`$subst_callableTypeIR`/
      `$subst_callableTypeDefIR`/`$subst_constructorTypeIR`(gensym-threaded
      상태 인자가 형제 조건에 그대로 남아 "같은 subject 정렬" 전제가 깨져
      `effectful` 목록으로 무조건 게이트, 5개) + `$find_local_return_type_t`
      (반사는 시도되나 sibling 패턴의 생성자 `FUNCTION_colon_1`이
      `definedFunctionLocalKind`/`functionLocalKind`/`localKind` 세 타입에
      걸쳐 이름이 겹쳐 `matcher_type`이 유일 소유 타입을 못 찾아
      `Gate "ambiguous matcher"`, 1개)뿐 — 둘 다 `Mfe.check`의 `drop_owise`
      폴백이 계속 커버(분석 자체는 안 막힘), 실행 표면은 원래대로 `owise`
      속성 유지. **(2026-07-04 정정: 이 5개 "무조건 게이트" 서술은 부정확했음 —
      아래 권장 순서 섹션의 2026-07-04 항목 참조, 실제로는 안전하게 반사 가능해서
      66/72 → 71/72로 늘어남.)** 태스크가 기대한 사슬
      `$itercollect→Cast_impl_neq→Cast_impl→$cast_unary/$cast_binary`,
      `$match_overloaded_{named,unnamed}_*` 12심볼,
      `$gen_constraint`가 전부 열림. `$cast_unary` 절2가
      `holds_Cast_impl(..)=true`로 재철자되고 owise 절이
      `or(holds_Type_alpha(...), and(not(...), holds_Cast_impl(...)))=false`
      가드로 반사됨을 CTRS 육안 확인. impty golden(`spec.ctrs`)·impty/p4 실행
      모듈(`spec.maude`/native emit) 전부 byte-identical(SHA256 대조).
    - **MFE 실측**: `$itercollect_let_b_default_..._b_default`(7규칙 슬라이스:
      itercollect base/step + `$is_default_parameterIR` 2절 + 지원규칙) →
      **CRC YES / ChC YES**(무회귀, 새 반사가 실제로 discharge됨을 확인).
      `$cast_unary`/`$cast_binary`/`Cast_impl_neq` 자체 슬라이스는 subty
      전수 보완 규칙을 끌어와 5만 규칙급이라 이 환경(심볼당 분↔십분대)에서는
      스윕 불가 — 별도 세션에서 환경 여유 시 재시도.
    - **범위 밖으로 남긴 것**: 출력을 갖는 일반 relation의 `R_succeeds?`
      (임의 output-moded relation의 존재성 반사)는 이번에 구현하지 않음 —
      사용자 승인 설계가 명시적으로 이번 범위에서 제외했고, 위 게이트 결과
      (잔여 6개 중 output-relation 게이트 0개)로 현재는 불필요함이 확인됨;
      게이트 로그가 실제로 요구하면 별건으로 착수.
  - [x] **`$unzip`/`$iterproj` 규칙-생성 코드 통합 + reflect.ml `$iterproj`
    하드게이트 제거 (설계 2026-07-03, 구현 완료 2026-07-03).**

    **배경**: `iter_unzip_defs`(`$unzip`, `to_ctrs.ml:363-396`)와
    `iterpr_defs` 안의 `proj_defs`(`$iterproj`, `to_ctrs.ml:916-946`)가 규칙
    생성 로직상 동일함을 발견 — 차이는 `fv_terms`(캡처 자유변수 leading
    args, iterproj는 항상 빈 리스트)와 매칭 패턴(`elem_pat`: 임의 구조 vs
    `tuple_pat`: 항상 bare 튜플) 뿐. 조사 중 `reflect.ml`의
    `iter_helper_prefixes`(`reflect.ml:181-182`)가 `$iterproj`를 owise
    가드에서 하드게이트하는 이유도 "success reflection(`holds_`) 미구현"일
    뿐, `$unzip`/`$itermap`이 무조건 허용되는 근거("relation 미호출, 형제의
    binding 조건으로만 가드에 진입")가 `$iterproj`에도 문자 그대로 적용됨을
    코드로 확인(`proj_defs`가 만드는 규칙은 `nil_t`/`cons_t`/`tuple_t`/
    자기재귀 호출만 참조, relation 심볼 없음). **이전에 막힌 `⛔ $iterproj
    완전 제거`(위 항목, 커밋 `0c260cc6`, gensym 벽)와는 무관** — 그건
    `$iterapply`의 "원소당 1회 호출" 구조 자체를 없애려던 시도였고, 이번
    건 이미 계산된 튜플 스트림의 순수 후처리 코드만 공유/게이트 완화하는
    것이라 gensym에 전혀 영향 없음(`$iterapply`/`apply_rules`,
    `to_ctrs.ml:898-915`는 미변경).

    **Phase 1 — `to_ctrs.ml` 코드 공유** (3개 edit, `to_ctrs.ml`만):

    (a) `unzip_sym`(353-354줄) 뒤·`iter_unzip_defs`(356줄) 앞에 삽입:
    ```ocaml
    (* Defining rules for a single-spine helper [sym]: recurse over a [List]/[Opt]
       spine, matching each element against [elem_pat] and returning the element's
       [v] component ([var_t (step_hd v)], one of [elem_pat]'s pattern variables).
       [fv_terms] are captured constants threaded unchanged as leading arguments
       ([] when there are none). The spine's tail is always the fixed variable
       [__rest] -- there is exactly one spine here, unlike [spine_args]'s N-way
       [step_tl id] naming.

       Shared by [iter_unzip_defs] below ([$unzip]: [elem_pat] is the arbitrary
       translated iterated body -- possibly non-left-linear, when it re-mentions a
       captured [fv_terms] variable) and [iterpr_defs]'s [proj_defs] far below
       ([$iterproj]: [elem_pat] is always a bare fresh-variable tuple over an
       already-materialized stream, [fv_terms] always [] -- always irrefutable). *)
    let spine_projection_rules (sym : string) (fv_terms : R.term list)
        (iter : iter) (elem_pat : R.term) (v : string) : R.rule list =
      let collected = var_t (step_hd v) in
      let rest = var_t "__rest" in
      match iter with
      | List ->
          [
            rule (app_t sym (fv_terms @ [ nil_t ])) nil_t;
            rule
              (app_t sym (fv_terms @ [ cons_t elem_pat rest ]))
              (cons_t collected (app_t sym (fv_terms @ [ rest ])));
          ]
      | Opt ->
          [
            rule (app_t sym (fv_terms @ [ none_t ])) none_t;
            rule (app_t sym (fv_terms @ [ some_t elem_pat ])) (some_t collected);
          ]
    ```
    이름은 기존 `spine_args`(158-167줄, IL 노드가 아니라 이미 추상화된
    term/string을 받는 범용 콤비네이터에 붙는 접두사)와의 명명 관례를 따름
    — 이 계열의 다른 함수는 전부 `iter_` 접두사(특정 IL exp/prem 사이트에서
    직접 계산). **`spine_args`로 위임 금지** — tail 변수 명명이 `step_tl id`
    (`v__tl`)라 `$unzip`/`$iterproj`가 쓰는 고정 리터럴 `"__rest"`와 달라
    위임하면 방출 심볼이 바뀜(byte-identical 불변식 위반).

    (b) `iter_unzip_defs`(363-396줄) 본문 교체:
    ```ocaml
      match e.it with
      | IterE ({ it = VarE _; _ }, _) -> []
      | IterE (body, (iter, vars)) ->
          let fv_terms = List.map var_t (captured_fvs (Free.free_exp body) vars) in
          let ids = iter_var_ids vars in
          let elem_pat =
            subst_term (elem_renaming ids) (term_of_exp ~scalars body)
          in
          List.map
            (fun v ->
              let sym = unzip_sym body iter v in
              (sym, spine_projection_rules sym fv_terms iter elem_pat v))
            ids
      | _ -> []
    ```
    (`let rest`/`let collected` 라인은 공유 함수 안으로 이동해 제거.)

    (c) `iterpr_defs` 안 `proj_defs` 블록(916-946줄) 교체 — `apply`/
    `apply_rules`(898-915줄)와 `(apply, apply_rules) :: proj_defs`/`None`
    분기(947줄 이후)는 미변경:
    ```ocaml
                let proj_defs =
                  if List.length out_vars <= 1 then []
                  else
                    let tuple_pat =
                      tuple_t (List.map (fun v -> var_t (step_hd v)) out_vars)
                    in
                    List.map
                      (fun v ->
                        let sym = iter_proj_sym inner iter n v in
                        (sym, spine_projection_rules sym [] iter tuple_pat v))
                      out_vars
                in
    ```

    **Phase 1 검증**: 심볼 이름(`unzip_sym`/`iter_proj_sym`)·방출 규칙 모양
    불변 → byte-identical이어야 함:
    - impty/base golden diff 0: `main.exe rewrite --ctrs
      specs/impty/base/spec.spectec | diff - specs/impty/base/spec.ctrs`,
      `main.exe rewrite specs/impty/base/spec.spectec | diff -
      specs/impty/base/spec.maude`.
    - p4 전체 corpus diff 0: `main.exe rewrite --ctrs $(find specs/p4 -name
      '*.spectec' | sort)` 편집 전/후 비교(golden 없음, 직접 캡처; ~69초/회).
    - `$iterproj` golden 없는 경로는 슬라이스로 확인: `--symbol
      TableProperty_ok`(`TableEntry_ok` 반복 —
      `specs/p4/5-typing/5.14.1-typing-control-table.spectec:814`
      `TableProperty_ok/entries` 규칙 안), `--symbol Decl_ok`(`Type_ok`
      block 반복 3곳 —
      `specs/p4/5-typing/5.11-typing-declaration.spectec:875,901,927`
      `Decl_ok` 규칙들 안). 슬라이스 루트가 바깥쪽 relation인 이유:
      `iterpr_defs`는 반복 프리미스를 담은 바깥쪽 rule을 순회할 때 트리거됨.
    - `reflect:` stderr 요약도 diff 0 기대(이 단계는 `reflect.ml` 미변경).

    **Phase 2 — `reflect.ml` 하드게이트 제거** (2개 edit, `reflect.ml`만):

    (d) `iter_helper_prefixes`(181-182줄):
    ```ocaml
    let iter_helper_prefixes = [ "$iterall"; "$itercollect"; "$iterapply" ]
    ```

    (e) doc comment 3곳 정정(문장 삭제/이동만, 로직 변경 없음):
    - 43-52줄(파일 상단 개요): "an iteration helper without a success
      reflection (`$iterproj` -- multi-output projection, not handled yet --
      and `$iterall`/...; `$unzip`/`$itermap` are pure stream transformers
      and never gated)"에서 `$iterproj`를 빼고 `$unzip`/`$itermap` 목록으로
      옮김.
    - 171-180줄(`iter_helper_prefixes` 바로 위): "[$iterproj] (multi-output
      projection) has no success reflection yet, so it stays hard-gated
      regardless of [succ]." 문장 삭제, "The pure stream transformers
      [$unzip]/[$itermap]" → "[$unzip]/[$itermap]/[$iterproj]"로 확장.
    - 913-920줄(judgment-phase candidate 주석): "[$iterproj] does not [get a
      success reflection] ... so it is deliberately excluded here"를
      "`$iterall`/`$itercollect`/`$iterapply`는 boolean-valued judgment라
      success reflection이 필요하고, `$unzip`/`$itermap`/`$iterproj`는
      value-binding pure stream transformer라 애초에 불필요해 제외"로 정정.
    - **`is_iter_helper` 함수 자체(`is_iterall f || is_itercollect f ||
      is_iterapply f`)는 코드 변경 불요** — `$iterproj`는 원래도 이
      OR-체인에 없었음(주석만 stale).

    **Phase 2 검증**: p4 전체 corpus `--ctrs` 실행 시 `reflect:` stderr를
    Phase 1 이후 상태와 diff. 카운트 불변이면 "이 코퍼스에선 게이트가 실제로
    아무것도 막고 있지 않았다"는 뜻(그래도 정당한 정리 — 최신 "잔여 6 kept"
    목록(위 owise 반사 확장 항목)에 iterproj 관련 항목이 원래 없어 무회귀가
    오히려 예상됨). 새로 반사되는 symbol이 생기면 그중 하나를 `verify
    --symbol <name> --timeout 360`(MFE CRC/ChC)으로 무회귀 확인. impty/base
    golden도 재확인(iterproj 자체가 impty엔 없어 무영향 예상).

    **마무리**: `make fmt`. (계획 전문은 세션 로컬
    `/home/min/.claude/plans/tingly-enchanting-star.md`에도 있으나, 이
    항목이 authoritative — repo에 커밋되는 쪽.)

    **구현 결과 (2026-07-03).** 계획대로 Phase 1(`to_ctrs.ml`: `spine_projection_rules`
    신설 + `iter_unzip_defs`/`proj_defs` 위임) + Phase 2(`reflect.ml`:
    `iter_helper_prefixes`에서 `$iterproj` 제거, 주석 3곳 정정) 그대로 구현, `make fmt`
    적용(포맷만, 로직 무변경). **검증 전부 통과, 예상대로 무회귀:**
    - impty/base `--ctrs`/기본(`spec.maude`) 골든 둘 다 byte-identical.
    - p4 전체 corpus(`rewrite --ctrs`, 79파일) 출력 **및** `reflect:` stderr 요약
      (`66 owise rule(s) reflected, 6 kept`) Phase 1 전/후/포맷 후 3단 비교 모두
      byte-identical — 예측대로 이 코퍼스에선 `$iterproj` 하드게이트가 실제로 아무
      owise 절도 막고 있지 않았음(잔여 6 kept는 예상대로 iterproj 무관).
    - `--symbol TableProperty_ok`/`--symbol Decl_ok` 슬라이스에 `$iterproj` 규칙이
      각각 7·22개 존재 확인 — 공유 코드 경로(`spine_projection_rules`)가 실제로
      두 소비처 모두에서 발화함.
    - 새로 반사된 symbol이 없어(카운트 불변) 계획의 "새 symbol 있으면 MFE
      재검" 조건이 vacuous — 별도 MFE 확인 불필요.
    - **MFE 환경 재확인(본 세션 시작 시, 이 작업과 무관하게 선행 시도):** todo.md가
      "다음 세션에서 환경 복구 후 재검"이라 적어 둔 (B) MFE 실측을 먼저 시도했으나,
      impty `$lookup` 대조군(과거 ~1.4s)이 `--timeout 280`에서도 TIMEOUT — 지난
      세션 기록([[MFE env slowdown]] 메모)과 동일 증상으로 **환경이 아직 회복되지
      않음**을 재확인(회귀 아님, RAM/OOM 로그 이상 없음 — 원인 불명, 별도 조사 필요).
      이번 세션은 그 대신 환경 의존이 없는 본 항목(코드 통합, golden/corpus diff로만
      검증 가능)을 완료함.
  - [x] **prelude 산술 overlap 해소(완료).** `mod`/`div`/`mod_int`/`div_int`은
    `= A if lt(x,y)=true` + `= B if leq(y,x)=true`처럼 보집합 가드를 *다른 술어*로
    적어 CRC가 동시 불가를 못 보고 `x = mod(sub(x,y),y) if lt(x,y)=true /\ leq(y,x)=true`
    같은 헛-임계쌍을 냈다. **fold(구현):** 가드 boolean을 **보조 함수로 dispatch** —
    `mod(x,y)=mod-aux(lt(x,y),x,y)`, `mod-aux(true,..)=x`, `mod-aux(false,..)=mod(sub(x,y),y)`.
    base/recursive 절이 서로소 `true`/`false` head 패턴이라 단일화 불가 → 임계쌍 미생성.
    `div_int`의 부호 판정은 구조적 `eq`(전 생성자 동치 → 슬라이스 44k 폭증) 대신
    boolean `equiv(nonneg x, nonneg y)`로 바꿔 슬라이스 37로 축소. 보조 head 4개
    (`div_aux`/`mod_aux`/`div_int_aux`/`mod_int_aux`)는 `native_replaced_heads`라
    분석 surface 전용(Native는 `quo`/`rem` delegation, 실행 byte-identical). 결과:
    `$bin_div`/`$bin_mod`/`$bin_plus`/`$bin_minus`/`$bin_mul` 전부 YES.

  **전체 p4 sweep (574 심볼 전수, 진행 중) + triage 도구.** 이전 sweep들은 "≤200
  규칙 tractable slice"만 골라 돌렸으나, 이제 **`verify --list-symbols` 574개 전체**를
  slice-size **오름차순**(작은 것부터 결과가 빨리 쌓임)으로 돌린다. 심볼당 maude
  `--timeout 120` + 외부 `timeout 150`. 백그라운드로 돌리며 라이브 로그를 repo 루트에
  symlink: `spectec/mfe_sweep.log`(심볼마다 `[시각] i/574 이름 size CRC ChC 경과초`),
  `spectec/mfe_sweep.tsv`(`symbol/size/church_rosser/coherence/elapsed_s`). 스윕
  스크립트·결과는 scratchpad에 있고 gitignore됨(`mfe_sweep.*`/`triage*`/`ctrs_ops.txt`).
  대형 슬라이스(수천~6만 규칙; `Program_inst` 등 전체-시스템급)는 임계쌍 폭증으로 120s
  TIMEOUT 예상 — 오름차순이라 뒤로 갈수록 TIMEOUT 밀도가 오른다.

  - **triage 도구 [`triage_mfe.py`](../../triage_mfe.py)** — sweep TSV에서 CRC=MAYBE /
    ChC=NO를 뽑아 **각 심볼을 IL 소스 선언 위치(`file:line`+시그니처)로 연결**한다.
    sweep 심볼은 전부 직접 IL 이름이라 grep 인덱스로 해결되고, sanitize된 `_prime`은
    `'`로 역변환(`desanitize`)해 매칭한다. **defunctionalize 템플릿 재구체화("다시
    구체화"):** `$find_overloaded<V>`/`$reduce_serenum_binary(def $check)` 류는 sweep에서
    size-0 NOVERDICT로 뜨는데(defunctionalize가 원본 규칙을 특수화 이름으로 **복사**해
    옮겨 제네릭 이름엔 규칙 0), 실제 CTRS op 집합을 ground-truth로 스캔해 구체 인스턴스
    (`$find-overloaded-parameterListIR-of-constructorDef` 등)로 펼치고 각 인스턴스의
    def-arg getter를 그 IL 선언으로 역해결한다. 매핑은 de-mangling 없이 `norm()`
    (소문자화+`-`/`_` 통일+`$` 제거) 기반이라 견고; 템플릿 감지는 dec 헤더의 `<..>`/
    `def $` 파라미터(선언 인식을 **컬럼 0 앵커**로 고정해 들여쓴 `def $check` 파라미터를
    오탐 안 함).

  - **degenerate NOVERDICT 9개의 정체 (전부 설계상 규칙 0 — 번역 누락 아님):**
    1. **고차/제네릭 템플릿 7** (`$find_overloaded`/`$find_overloadeds_named`/
       `$find_overloadeds_unnamed`/`$match_overloaded_named`/`$match_overloaded_unnamed`/
       `$reduce_serenum_binary`/`$reduce_serenum_unary`) — `<V>` 타입 파라미터 및/또는
       `def f(..)` 고차 파라미터를 받아 defunctionalize가 인스턴스별로 규칙을 복사·특수화.
       제네릭 이름엔 규칙 0(실측: `$find-overloaded(` 0개 vs `-of-constructorDef` 4개 등
       원본 4절 × 6타입 = 24 특수화 규칙). 실제 규칙은 caller 슬라이스에 transitively
       끌려가 거기서 검사됨. (`--list-symbols`는 IL dec만 열거해 특수화 이름은 단독으로
       안 잡힘 — triage가 재구체화로 메꿈.)
    2. **타겟 구현 훅 1** (`$init_objectState`) — `dec`만 있고 `def` 없음("in the target").
    3. **extern 관계 스텁 1** (`ExternFunctionCall_eval_lctk`) — `rule` 0개, `testgen_ignore`.
- **Termination(MTT/AProVE) 경로 확보 — CRC 보완용 (done, sweep 진행중).** CRC는
  *국소* 합류성 + sort-decreasingness만 주고 완전 Church-Rosser엔 종료성 증명이
  필요하다. 이 3.5.1 MFE의 MTT는 안 돌지만(Maude++ 훅·2.x 문법), **매칭되는
  Maude-2.7.1 스택**을 붙여 실제로 돌린다: `tools/maude271-hooks/maude`(v2.7.1-ext-hooks,
  훅 바인딩) + `tools/mfe271/`(옛 MFE, **MTT 1.5j** + 옛 Full Maude) +
  `tools/aprove/aprove.jar` + `tools/z3/z3`. 슬라이스 하나는
  `tools/mfe/run-termination.sh <symbol>`: `--ctrs` 슬라이스를 옛 Full-Maude
  `fmod`로 헤더 변환 → `(select tool MTT .)(select external tool aprove .)(ct SPEC .)`
  → MTT가 order-sorted 조건부 모듈을 TPDB CTRS로 변환(‑isTerm/isThruth sort 가드
  추가) → `mfe.config`대로 AProVE 호출. **백엔드=Z3**(AProVE 기본 `SmtSolver=z3`);
  WST 전략의 레거시 yices-1.x 호출은 곱게 abort하므로 **라이선스 게이트 yices 불필요**
  (산술 무거운 슬라이스만 느려짐). 검증: `FOO`·`$empty_map`·`$is_lpm_key_prime` → YES.
  전부 gitignore(`tools/{maude271-hooks,mfe271,aprove,yices,z3}/`), 셋업 상세는
  [tools/mfe/README.md](../../tools/mfe/README.md). **종합 스윕(153심볼 CRC+termination) +
  모듈러(A/B) 분해 완료 (2026-07-09)** — 결과·수정 합성 정리는 저장소 루트
  `spectec-crc-termination-recalibration.md`(이후 recalibration.md로 개명, 현재는
  verification.md + verification-notes.md로 분리). full-mode의 고정폭 산술 TIMEOUT은 도구 예산이
  아니라 폭-정규화 헬퍼로 국소화됨: **모듈러 (B) 분해(`tools/mfe/prune_modular.py`, 산술을
  종료 블랙박스로 추상)로 11개 산술 + 3개 비트와이즈 슬라이스가 전부 수초 YES**,
  sum/max/min_nat·strip_all_whitespace(eq-theory abstract, 50634→5규칙)도 YES.
  **단 하나의 진짜 잔여물 = `$bitstr_to_int` w=0 실행 비종료**(아래 신규 이슈). termination
  verdict는 **analysis 표면 한정**(owise drop + isStuckHead ruleless)이라 executable 인증은 후속.
- [ ] **⚠️ `$bitstr_to_int` w=0 실행 비종료 (신규 2026-07-09, termination 재캘리브레이션에서 발견).**
  **→ 2026-08-12 기계적 증인 확보 + 폭발 반경 확정**: TRS 규칙만으로 하는 innermost
  시뮬레이션이 `0→−1→0` 진동을 56스텝에 재현(WITNESSES.md ⑶ — 잘 정렬된 항이라
  과대근사와 무관한 진짜 비종료). 잔여 termination TIMEOUT 233 중 **157슬라이스**
  (`Program_ok` 포함)가 이 규칙을 폐포에 품어 SN 증명이 원천 봉쇄됨
  (`sweeps/decode_membership.tsv`).
  **→ 같은 날 규명도 종결: 막지 않는다.** 이 스펙의 타입체커는 `bit<0>`을 합법으로
  수용하고(`const bit<0> A = 0;` succeeded), `$bin_plus`의 **W(unsigned) 절도**
  `$bitstr_to_int`를 부르므로 `const bit<0> B = A + A;` 두 줄이 타입체킹을
  실제로 발산시킨다(30초 킬; 대조군 `bit<1>` 덧셈·산술 없는 `bit<0>`·decode 없는
  `-A`는 전부 성공, `int<0>`은 거부 — WITNESSES.md ⑸, 재현
  [sweeps/witness/bit0_add.p4](../../../sweeps/witness/bit0_add.p4)). 따라서
  "도달불가 문서화" 분기는 죽었고 **numerics.ml+builtin.ml 공유 실행 버그** 분기가
  확정 — 남은 결정은 고치는 방식(bit<0> 산술을 정형성에서 거부 vs decode/encode의
  w=0 전역화)이며 스펙 소유자 사안. 해소되면 termination 잔여 157건이 재질의
  가능해진다. (부수 혐의: `$un_minus(0 W 0) = $(2^0−0) = 1` — bit<0>에 표현
  불가능한 값 생성 가능성, 별도 확인 필요.)
  `builtin.ml:457-489`의 two's-complement decode가 `w=0`이면 목표 구간 `[-2^(w-1),2^(w-1))`가
  공집합이 되어 `n:0→-1→0` 진동 루프(규칙1 `n≥0`→`n-1`, 규칙2 `n<0`→`n+1`, base 도달 불가).
  참조 `numerics.ml:51-56 bitstr_to_int'`도 구조 동일 → w=0에서 같은 루프(**번역은 faithful**,
  differential MATCH와 부합). 실무 종료는 오직 `w≥1` 호출 덕분(구간 비어있지 않아 수렴). **규명 필요**:
  P4 타입시스템이 `bit<0>`/`int<0>` 산술을 막는가? → 막으면 도달불가 `w≥1` 불변식(문서화로 종결),
  안 막으면 numerics.ml+CTRS **공유 실행 버그**(builtin.ml w=0 처리 수정). 영향: 고정폭 W/S decode
  op 전부(lt/le/gt/ge, plus/minus/mul, un_minus-S, band/bxor/bor-W/S, nat_of_integerValue, satplus/satminus, shl/shr).
- [x] `to_ctrs.ml` 상단 `[@@@warning "-32-69"]` 제거 (완료 — 어트리뷰트는 이미
  소스에 없고 `dune build bin/main.exe`가 경고 없이 통과. 빌더 레이어가 전부
  참조된다는 뜻).
- **⛔ `$iterproj` 제거(수집 헬퍼 통일 1단계) — 보류, 정합성 벽 (2026-07-02).**
  다중 출력 iterated relation을 "출력별 무조건 map"으로 바꾸려던 안은 **gensym과
  충돌**한다: p4의 multi-output `$iterapply` 3개(`TableEntry_ok`,
  `Type_ok/block` ×2 — 출력에 fresh `typeId'` 포함)가 전부 state-thread돼 있어,
  출력별 map이 각자 relation을 재호출하면 **fresh 이름 발급이 map마다 갈라져**
  인터프리터와 divergence가 난다(성능이 아니라 정합성 문제). 단일 호출 + 순수
  스트림 projection(현 `$iterapply`+`$iterproj`)이 효과적 다중 출력의 올바른
  형태라 유지. 후속 `$itercollect`-only 통일(4e00da94 revert)도 같은 벽 —
  effectful 반복은 원소당 1회 호출 구조가 불변식임을 전제로 재설계해야 함.

  **→ 해소 (2026-07-18, M1 "iter 헬퍼 패밀리 통합" 참조).** 불변식을 지키는
  재설계로 통일 완료: 출력별 재호출 대신 **단일 튜플-수집 `$itercollect`**(호출
  원소당 정확히 1회, 무조건 eq 유지)를 두고, 성분 복원은 이미 계산된 튜플
  스트림 위의 순수 projection(합성 튜플 본문의 `$iterproj`) 또는 소비 헬퍼의
  fused destructure가 맡는다 — 2026-07-16 head-side fusion 기계가 premise-side로
  확장되면서 당시 벽이던 "소비자 배선" 문제가 풀렸다.

### CRC 조건-단순화 **예산** 실측 — `$join_ctk`/`$assignop_as_binop` MAYBE의 원인 규명 (2026-07-14)

CLAUDE.md가 *"Fall-through/default clause guarded by `or(all match-Xs) = false`"* 로
분류해 둔 허위 MAYBE(`$join_ctk`, `$assignop_as_binop`)의 **진짜 원인을 규명**했다.
기존 트리아지("infeasible once any specific matcher fires")는 *현상*은 맞지만 *왜 CRC가
그걸 못 보는지*를 짚지 못했다.

**실측 1 — 원인은 or-체인 길이(예산)지, 정보 부족이 아니다.**
`$join_ctk`의 잔여 ccp를 **20줄 미니모듈(J0)로 그대로 재현**했다(실물과 동일한 ccp):

```
ccp: variant-ctk-CTK-0 = variant-ctk-DYN-0
     if or(or(or(and(match-ctk-LCTK-0(CTK), match-ctk-LCTK-0(CTK)), …), …),
           and(match-ctk-CTK-0(CTK), match-ctk-CTK-0(CTK))) = false .
```

- 조건은 **100% ground**다 (owise가 ground head 형제 절과 겹쳐서 unifier가 이미 생성자를
  대입해 버렸다). 슬라이스엔 `match-ctk-CTK-0(CTK)=true`, `or(true,y)=true`가 **전부 있다**.
- 같은 항을 stock Maude `reduce`에 넣으면 **9 rewrites 만에 `true`** = 조건 모순 = 불가능.
  **그런데 CRC는 안 줄인다.**
- disjunct 수를 바꿔가며 경계를 찍었다:

  | or-disjunct 수 | CRC |
  |---|---|
  | 2 (H0/J2) | YES |
  | 3 (J3) | YES |
  | **4 (J0 = `$join_ctk` 실물 형태)** | **MAYBE** |

  → **CRC의 CCP 조건 단순화에는 한정된 예산이 있다.** "프로그램 등식을 안 쓴다"가 아니라
  **깊어지면 못 쓴다**. (CRC 판정 법칙 전체는 memory `mfe-crc-hypothesis-rewriting` 참조:
  ①가설 충돌로 refute ②생성자 narrowing 안 함 ③프로그램 등식은 쓰되 예산 있음.)

**따름 — guard를 "똑똑하게" 만드는 처방은 이 부류에 전부 무효.** 줄여야 할 항이 이미
ground라 가설이 개입할 자리가 없다. 이번 세션에 시험 구현한 `Reflect.tag_guards`
(타입당 단일 판별자 `tag_<T>` + `tagcase_<T>_<Ci>`, `align_guards`의 N치 일반화)가
정확히 이 이유로 **이득 0이라 되돌렸다**. 상세:

- 메커니즘 자체는 옳다 — 통제 모듈에서 `match_` spelling MAYBE → tag spelling **YES**.
  겨냥한 건 법칙②(narrowing 불가로 형제 matcher guard의 배타성에 도달 못 함)다.
- **그런데 검사 가능한 MAYBE 중 법칙②에 막힌 게 하나도 없었다.**
  - `Check_expr`(impty), `$typedLvalueIR_as_typedExpressionIR`(p4): 형제들이 **head 패턴으로
    이미 분기**(`variant-expr-plus-2` vs `variant-expr-lt-eq-2`) → 좌변이 **unify 불가** →
    **임계쌍이 아예 생성되지 않는다**. tag는 짝을 이루지 않는 규칙에 조건만 덧붙였다.
  - `$join_ctk`/`$assignop_as_binop`: 위의 예산 문제 → tag로 respell하면 disjunct마다
    `tag(..)→tagcase` + `eqg→true` 두 스텝이 **더 붙어 오히려 악화**.
  - `Check_expr`의 잔여 ccp는 `$lookup(t, ID(x))` 출력이 두 변수에 묶인 **결정성 쌍** — 무관.
- **구조적 이유**: `hoist_matchers`/`fold_premise_binders`/`expand_subty_guards`가 하는 일이
  바로 "guard에 있던 판별을 head 패턴으로 옮기기"다. 성공할 때마다 tag의 먹잇감을 없앤다.
  남는 잔여물은 (i) subject가 **계산된 값**이라 못 접는 경우(→ 그런 절들은 head가 이미
  서로소라 CCP 없음)와 (ii) `guarded_family` 예외(→ 그게 곧 owise 예산 문제)다.
- 다만 **tag의 사정거리를 과소평가했던 점은 정정**한다: guard를 *커버된 쪽*이 아니라
  **여집합 쪽**으로 철자하면 `$assignop_as_binop`은 tag로도 풀린다 —
  `or(12겹 match) = false` 대신 `tag_assignop(x) = tagcase_eq` 한 줄(생성자 13개 중 12개
  커버 → 여집합 1개). 단 `$join_ctk`은 커버 4/여집합 5로 **양쪽 다 예산 초과**라 어느 쪽
  철자로도 안 된다. 즉 tag는 이 부류의 일부만 건드릴 수 있다.
- p4에서 tag가 실제 발화한 형제-겹침 심볼(`Lvalue_ok` 23규칙, `TableAction_ok` 17 …)은
  슬라이스가 **5만 규칙 이상**이라 CRC가 애초에 안 돈다 → 이득 측정 자체가 불가능.
  **명제: tag는 법칙②를 정확히 푼다. 그런데 검사 가능한 슬라이스에 법칙② 봉쇄가 없다.**
  MFE가 5만 규칙을 감당하게 되면 재고 가치 있음.

## M2 — 실행 (Maude)

- [x] **`To_maude.*` 복구** (done) — Native 시스템 → order-sorted Maude 모듈(sort 복구·
  op 선언·eq/rl·내장 delegation eq·input-moded relation을 eq로) + META-TERM 인코더
  (`meta_term_of_value`/`meta_start_app`). `Pipeline.maude_system_of_spec`
  (= `of_spec ~scalars:Native`) 직접 경로 사용. `of_spec` 스텁이 채워지면 즉시 동작.
- [x] **`Maude_run` 복구** (done) — 로컬 maude 구동·`metaReduce`·normal form 파싱·stuck
  판정·`run_batch`(모듈 내부화 상각).
- [x] **`Defunctionalize` + `Gensym` + `Builtin` 재생성 + 파이프라인 재배선** (done) —
  p4용(def-인자 specialization, `$fresh` 상태 스레딩, 컬렉션 builtin 규칙). 두 경로
  공통 `Pipeline.build`로 wrap(Defunctionalize FIRST / `of_spec ~extra_defs:Builtin`
  / Gensym LAST). impty/base엔 identity. `of_spec` 스텁이 채워지면 즉시 효과 발생.
- [x] **CLI `run` 실행 배선** (done) — targets의 `maude_start_term`
  (`Targets_impty.Impty`/`Targets_p4.P4`) + `Maude_run.run_batch` + `--check-p4`
  (Of_maude 역번역·`Eq.eq_values` 비교)까지 [bin/main.ml](../../bin/main.ml)에 배선 완료.

## M3 — 결과-VALUE 오라클

- [x] **`Of_maude` 복구** (done) — Maude normal form → IL value 역번역 + `canonicalize`
  (gensym 이름·map 정렬 정규화).
- [x] **differential** (done) — same-spec interp(p4) vs Maude(p4)를
  [check_diff_p4.sh](../../../check_diff_p4.sh)로 전체 corpus 교차 검사. 결과:
  completeness gap 0, soundness gap 1(알려진 issue1944), Phase D 결과-VALUE
  오라클 1227/1227 MATCH. 상세는 [CLAUDE.md](CLAUDE.md) "회귀/divergence 측정" 절.
- [x] **CTRS(구조적) differential — 완료 (2026-07-10, `92618dc2`).**
  [check_diff_structural_p4.sh](../../../check_diff_structural_p4.sh)가 인터프리터
  vs **구조적 CTRS**를 전체 corpus에서 교차 검증한다(Phase A/B/C/D, resumable,
  `CORPUS_LIST`/`MPROG`/`RESMATCH` 샤딩). 이진 nat 이론 위에서 `$int_to_text`
  부호 수정(`5e1c3ea1`) 후 **Phase D 1227/1227 MATCH / 0 MISMATCH**
  ([spectec-structural-completeness-soundness.md](../../../spectec-structural-completeness-soundness.md)).
  ⇒ `Reflect.owise`/`fold_premise_binders`의 의미 보존이 처음으로 **실행 기반**으로
  뒷받침됨. 아래는 착수 당시의 설계 메모(이력).

  위 differential은 인터프리터
  vs **Native 실행 모듈**(`To_maude`, 내장 Bool/Nat/Int/String 위임)만
  교차 검증한다. **구조적 CTRS**(`Reflect.owise`/`Rewrite_system.
  fold_premise_binders`/`prune_unused`를 거친 분석 표면 —
  `To_mfe.module_of_system`이 만드는 order-sorted `eq`/`ceq` Full Maude
  모듈)는 지금까지 MFE의 confluence(CRC)·coherence(ChC) 판정만 받았을 뿐,
  **실제로 Maude에서 rewrite해 나온 값을 오라클과 대조한 적은 없다** —
  owise/judgment 반사가 "분석 전용, 실행 무영향"이라는 주장은 지금 논거로만
  뒷받침되고 실행 기반 실측은 없다(반사 로직에 숨은 의미 버그가 있어도
  실행 표면은 이 패스들을 안 타므로 현재 체계로는 못 잡음).
  - **목표**: structural CTRS 모듈을 Maude에서 직접 `reduce`(또는 META-TERM
    경로)해 얻은 정규형을 IL value로 역번역해, 같은 프로그램의 인터프리터
    결과(그리고/또는 기존 Native 실행 오라클)와 비교하는 **제3의 오라클
    레그**로 삼는다.
  - **필요한 조각**: (a) structural scalar(Peano `succ`/`zero`, 부호-크기
    `int_pos`/`int_neg`, char-list `chr_<code>` text, 자체 `true`/`false`)
    시작항 인코딩 — 지금 `to_maude.ml`의 META-TERM 인코더(`meta_term_of_value`/
    `meta_start_app`)는 Native 전용이라 재사용 불가, structural 전용 인코더가
    필요; (b) structural 정규형 → IL value 역번역기(`Of_maude`의 structural
    버전 — 생성자 스펠링이 다름: `succ`/`zero`, `cons`/`nil`, `chr_<code>`,
    `variant_*`/`struct_*` 등, `canonicalize`의 gensym/map 정렬 정규화는
    그대로 재사용 가능할 것); (c) 성능 — Peano 인코딩·문자 단위 텍스트라
    Native보다 훨씬 느릴 것(작은 입력·표본 위주로 시작, `run_batch`류 배치
    상각 필요할 수 있음).
  - **가치**: `Reflect.owise`/`fold_premise_binders`가 실제로 의미 보존인지
    최초로 **실행 기반**으로 검증(현재는 byte-identical golden + CRC/ChC
    판정으로만 뒷받침 — 둘 다 반사 인코딩이 "말이 되는지"는 보지만 실제
    reduce 결과까지 확인하지는 않는다).
  - **2026-07-04 진행 상황(위 목표의 (a)/(b)는 이미 이전 세션에서 `run-structural`
    커맨드로 배선 완료 — impty는 end-to-end 검증됨).** P4 `Program_ok`를 이
    경로로 처음 실제 reduce해보니 세 겹의 실행 전용 버그가 순서대로 드러났다:
    (i) reflect.ml/to_ctrs.ml이 만드는 헬퍼 심볼(`$unzip_*`/`proj_<ctor>_<i>`
    등)이 원 spec의 `TypD`/`RelD`에 없어 `Maude_sorts.recover`의 폴백으로
    전부 `Val` 타입이 되고, 이게 정밀 타입의 실제 constructor에 들어가면
    ERROR-kind 항이 되어 `and`/`or` short-circuit으로도 못 고침 — `recover`에
    `infer_ranges`/`infer_proj_ranges` 갭필 패스 추가로 해결(`8d161af8`).
    (ii) `Program_ok` 끝까지 돌리면 native Maude가 stack overflow — 원인은
    `eqg`(구조적 동등성)가 반사 규칙(`x=x→true`)만 있고 "다르면 false"가 없어
    분석(CRC)에선 문제 없지만 ground reduction에선 무한정 stuck, 그리고
    `and`/`or`에 붙인 `[comm strat (1 0 2 0)]`의 `comm`이 Maude의 AC 매처
    때문에 `strat`이 막으려던 두 번째 인자 평가를 강제해버려 재귀 judgment
    guard가 안 끝남(toy 모듈로 직접 재현) — `comm` 제거하고 미러 방정식을
    직접 찍는 걸로 교체, `eqg`엔 실행 전용 owise-false 추가(둘 다 `full_maude`
    게이트라 CRC엔 무영향)로 해결(`a3e9c271`).
    (iii) 그 다음 막힌 지점은 `$int_to_bitstr`/`$bitstr_to_int`/`$shl`/`$shr`/
    `$shr_arith`/`$pow2`/`$bneg`/`$band`/`$bxor`/`$bor`/`$bitacc` — spec엔
    `builtin dec`로만 있고 Maude 방정식이 하나도 없었던 것. 인터프리터의 실제
    구현(`targets/p4/builtins/numerics.ml`)을 그대로 참고해 11개 전부 구현
    (반올림 방향이 갈릴 수 있는 것들은 클로즈드폼 대신 numerics.ml과 동일한
    재귀 구조로), 독립적으로 14개 케이스 검증 후 `2583c31f`로 커밋.
    **결과: 작은 bit-width P4 프로그램은 `Program_ok`가 이제 완전히 끝까지
    풀린다** (`const bit<4> x = 3;` → 정확한 typed constantDeclarationIR로 축약
    확인). 단, **새로운, 별도의 한계**를 발견: Structural 이론의 Peano/unary
    수 인코딩은 `bit<32>` 하나 표현하는 데만 2^32(~43억) 개 중첩 `succ()` 항이
    필요해 실제 corpus 대부분(`bit<8/16/32/48/64>` 흔함)에서 OOM(exit 137)으로
    죽는다 — 버그가 아니라 표현 자체의 스케일 한계(`pow2(8)`은 ~2초, `pow2(32)`는
    단독으로도 20초 안에 안 끝남 확인). 고치려면 Structural 이론에 binary 수
    인코딩이 필요 — 별도의, 더 큰 트랙.
    check_diff_p4.sh를 본떠 [check_diff_structural_p4.sh](../../../check_diff_structural_p4.sh)
    작성(`cde3d763`) — Phase A는 기존 `check_diff_p4_interp.tsv` 재사용, Phase B는
    OOM 격리를 위해 CHUNK를 30→10으로 줄이고 `ulimit -v`(기본 ~4GB)로 Maude
    프로세스별 메모리 상한을 걸었으며 분류기에 OOM 카테고리 추가. 4/12개 표본으로
    스모크 테스트 통과 후, 전체 1568개 corpus 대상 실행을 tmux 세션
    (`spectec-structural-diff`, 로그: `check_diff_structural_p4.log`)으로 백그라운드
    시작함 — **완주 확인(2026-07-07, `RESUMED_RUN_DONE_EXIT=0`): RESULT-VALUE
    3 MATCH / 0 MISMATCH, 나머지 179는 DECODE_ERR/NOCOMP/INTERP_FAIL/TIMEOUT/
    UNKNOWN(예측된 Peano 스케일 한계 계열; wall-clock 구조 14h vs interp 16m).
    MISMATCH 0 = 반사 패스 의미-보존의 실행 기반 확인이라는 원래 목표는 소형
    표본에서 달성; 커버리지 확대는 binary 수 인코딩 트랙(별도 체크아웃
    `spectec-core-binenc`에서 진행 중)의 몫.**

## Native 직접 생성 리팩토링 (B) — ✅ 완료

**한 일:** `~scalars:Native`를 post-fold(`native_scalars.fold`)에서 **생성 시점 직접
방출**로 바꿨다. `Ctrs_term`에 `scalar_theory`와 mode-aware leaf 빌더
(`bool_t`/`term_of_num`/`text_t`/`nat_lit`/`int_lit`/`conj_t`가 `~scalars`)를 두고,
`to_ctrs`/`prelude`/`builtin`/`gensym`에 `~scalars`를 thread했다. `prelude`는 한 개의
정렬된 리스트(`rules ~scalars`) + Native head-filter(`kept_in_native`), `builtin`은
delegated text builtin을 Native에서 생략, `gensym`은 native txt/`cat`로 fresh 이름을
짓고 prime `chr` eq를 Native에서 생략. `maude/native_scalars.ml`은 삭제. CORE_LOGIC
§6.1 "별도 fold 없음" 실현. **검증:** impty/base 분석 골든(`spec.ctrs`)·실행
모듈 모두 **byte-identical**(Structural 경로 무변, Native 경로 post-fold와 동일),
p4(79 파일) native 모듈 emit 시 구조적 스칼라 leaf 누출 0·delegation 정상.

다음 줄들은 조사 당시 기록이며 **전부 위 완료 시점에 해소됐다**(historical, `[x]`):

조사로 드러났던 blocker(왜 단순 leaf 교체로 안 됐나):

- [x] **bool leaf가 `to_ctrs` 밖에서도 생성됨.** `builtin.ml`(30+곳)·`gensym.ml`(5곳)이
  `T.true_t`/`T.false_t`/`T.text_t`/`T.peano_of_int`/`T.int_pos_t`로 규칙을 직접
  만든다. 이 규칙들도 시스템에 들어가므로 native에선 wrapper여야 한다 → **3개 모듈
  모두 mode-aware**로 만들어야 함(post-fold는 조립 시스템 전체를 균일 fold해 자동 처리).
- [x] **유지되는 prelude 규칙도 bool leaf 보유.** native에서 살아남는
  `mem(x,nil)→false`, `match_some/none/cons/nil`, 옵션/리스트 `eq(...)→true/false/and`은
  native bool이 필요. 드롭되는 scalar 규칙과 한 리스트에 섞여 있어 **prelude를
  scalar(드롭)/struct(유지) 둘로 분리** 필요.
- [x] **`int_pos` 막다른 길.** `term_of_unop` 단항 마이너스 = `negate_int(int_pos(x))`.
  post-fold는 ground `int_pos(peano)`→`int(n)`으로 접지만, direct-gen은 operand가
  `nat(5)`라 `int_pos(nat(5))`가 되고 `int_pos`는 `native_replaced_heads`에 없어
  **delegation도 fold도 없어 stuck**. → `To_maude`에 `int_pos`/`int_neg` delegation
  추가 또는 `term_of_unop` mode-aware화.
- [x] **드롭 필터 충돌.** scalar 규칙 드롭하는 `scalar_pat`은 bare `true`/`false`/`zero`를
  본다. leaf를 `bool(true)`로 바꾸면 `eq(bool(true),bool(true))`를 못 알아봐 안 드롭됨
  → To_maude의 scalar `eq` delegation과 충돌. **필터를 head-기반(scalar/struct 분리)으로
  재설계** 필요.
- [x] **OCaml 제약:** `true_t`/`false_t`는 값(value)이라 모듈 로드 시 1회 평가됨 →
  ref로 mode-aware 못 만듦. mode-aware는 함수(`bool_t`/`term_of_num`/`text_t`)로 두고
  `true_t`/`false_t` 사용처를 `bool_t true`/`bool_t false`로 교체하거나, prelude는
  thunk로 만들어 of_spec에서 mode 설정 후 평가.

작업 순서(안):
1. `to_ctrs`: `scalars_mode` 결정 방식 확정(ref vs 인자 thread), `bool_t`/`term_of_num`/
   `text_t` mode-aware화, prelude scalar/struct 분리, 살아남는 규칙의 `true_t`/`false_t`
   교체, 드롭 필터 head-기반 재설계, `native_scalars`/`native_term` 제거.
2. `To_maude`: `int_pos`/`int_neg` delegation 추가(또는 `term_of_unop` 대응).
3. `builtin.ml`·`gensym.ml`: scalar leaf를 mode-aware 빌더로 교체.
4. 검증: impty 실행(native 모듈 eyeball) + 가능하면 p4 일부로 회귀 확인.

## subtype의 struct 처리 — ✅ 해소 (interp 확인 결과: width/depth 검사 자체가 없음)

**결정 절차 1번(interp.subtyp 확인)의 답이 나와 종결.** 인터프리터
(`interp/eval_il/interp.ml:210-258`, eval_sl 동일)의 `subtyp`은 VarT 가지에서
`match (deftyp.it, value.it)`로 PlainT/(VariantT,CaseV)만 명시 처리하고 **StructT는
catch-all `| _ -> true`** — width도 depth도 head 모양 검사도 없이 무조건 true.
이유는 interp.ml:150 주석: "structs are invariant in SpecTec, so we do not need
to check for subtyping" — struct 간 subtype 관계가 언어에 없고(invariant), 좁힘은
elaborator가 정적으로 처리, 런타임 검사 시점엔 정적 타입이 정확히 그 타입임이 보장.
⇒ **옛 접근안 (c)가 코드로 확정: trivially-true가 정답 의미론, width/depth 구현 불요.**

- width 구현은 오라클에 없는 검사 발명(이름-키 재인코딩 등 큰 변경으로 의미론을
  벗어남); depth 재귀 추가는 interp보다 엄격해져 가짜 STUCK(completeness gap) 위험.
- 현행 `subty_<T>(struct_<T>(x...)) -> true`(head-키)는 도달 가능한 주어에서 interp와
  일치(invariance ⇒ struct 타겟 SubE의 주어 정적 타입 = T). 변수 LHS 대신 head-키를
  유지하는 근거(번역 버그 tripwire)는 defs_of_typ StructT 주석에 명시.
- 경험적 뒷받침: 이 규칙 그대로 Phase D 1227/1227 MATCH·completeness gap 0.
- "struct를 품은 복합타입 전파" 우려(아래 depth 섹션 (1))도 동시 해소: list/tuple/opt의
  depth 검사가 struct 원소에서 true로 멈추는 건 interp도 동일 — 공유된 정답 의미론.

## subtype의 depth 처리 — 재귀 경계와 근사 (struct 외)

`sub_pred`([translate/to_ctrs.ml:191](translate/to_ctrs.ml))가 타입 구조를 따라 depth로
내려가며 멤버십 검사를 생성한다. 어디서 재귀하고 어디서 trivially-true로 멈추는지,
그리고 멈추는 지점의 근사를 정리한다(struct 구멍은 위 섹션; 아래 (1)이 그걸 복합
타입으로 전파시킨다).

**depth 재귀가 실제로 되는 곳:**
- VariantT 케이스 payload(필드 타입별 `sub_pred`), TupleT 성분, IterT list/opt 원소
  — `defs_of_typ`/`sub_helper_defs`가 `and(sub_pred ..)` / 구조 재귀로 내려감.
- PlainT alias `T = U`: `subty_<T>(x) -> sub_pred U x`로 위임(체인 OK).

**trivially-true로 멈추는(근사였다가 interp 대조로 확정된) 곳:**
1. **struct를 품은 복합타입**: ✅ 해소 — interp도 struct 원소에서 true로 멈추므로
   (위 struct 섹션) 근사 불일치가 아니라 공유된 정답 의미론.
2. **타입 파라미터(generic VarT, TypD 없음)**: ✅ 일치 확인 — interp.subtyp도 정의
   없는 타입 파라미터는 catch-all `| _ -> true`. `sub_helper_defs`의
   `subty_<param>(x) -> true`와 동일 의미론(over-accept가 아니라 오라클 그대로).
3. **스칼라 leaf**: ✅ 일치 확인 + 주석화 완료 — interp와 정확히 동일(int는 모든 수,
   bool/text는 자기 sort, nat만 런타임 검사 `sub_nat`; `sub_pred` 주석에 명시).

**depth + 음성(negation) 전파 — ✅ false-보완으로 분석 표면도 닫힘 (2026-07-02):**
- subty가 positive-only여서 분석 표면에서 비멤버가 stuck이던 문제는 **사용-기반
  false-보완**(아래 "subty totality" 절)으로 해소: 모든 SubE 사이트의 (타겟 T,
  주어 정적 타입 S) 쌍에 대해 cases(S)\cases(T)별 `subty_T(..) -> false`를 방출,
  도달 가능한 도메인 전체에서 true/false로 환원. `~(e <: T)` 조건이 분석 표면에서도
  유의미해짐. 실행 표면의 To_maude owise complement는 잔여 head 흡수용으로 유지
  (명시 규칙이 선점, 의미 불변).

**termination(생성·재기록) — ✅ 확인 완료 (2026-07-10, p4 `--ctrs` 전수 실측):**
- **상호재귀는 실제로 존재한다**: 방출된 `subty_*` 호출 그래프에 크기>1 SCC가 **11개**
  (최대 26심볼 — `subty_expressionIR` ↔ `subty_argumentIR` ↔ `subty_callableTargetIR` …;
  그 외 `expression` 계열 17, `statementIR`/`blockStatementIR` 계열 8, `statement` 계열 6).
- **생성 종료**: `require`가 compound 타입(`TupleT`/`IterT`)에 대해 **먼저 `Helper_defs.add`
  하고 나서** 성분으로 재귀하고, 명명 타입은 `VarT` leaf라 재귀를 끊는다(규칙은
  `defs_of_typ`가 spec의 `TypD` 리스트를 1회 순회하며 방출) ⇒ SCC가 있어도 fixpoint.
  실측: `op subty-*` 중복 선언 0건, `eq subty-*` 완전중복 0건(7,310 등식).
- **재기록 종료**: 생성자 패턴 lhs의 재귀 호출은 전부 패턴 변수(진부분항)를 인자로 받는다 —
  실측으로 **lhs 인자 전체를 그대로 다시 `subty_*`에 넘기는 규칙 0건**. 유일한 비-하강
  형태는 alias 위임 `subty_T(x) -> subty_U(x)`(bare 변수 lhs, 71개)인데 이 위임 그래프는
  **비순환**(SCC 없음)이라 유한 체인 뒤 반드시 구조 하강 규칙에 도달. ⇒ 서브텀 순서 +
  비순환 alias 체인의 사전식 조합으로 종료.

## SCC (Sufficient Completeness Checker) — 배관 완료, CETA 바이너리만 남음 (2026-07-11)

**"빠진 케이스 = stuck 후보"를 정적으로 사냥하는 축.** `check_diff_p4.sh`의
completeness gap(Maude가 stuck → under-accept)을 1568개 프로그램의 경험칙이 아니라
**모든 항에 대해** 묻는 것이 sufficient completeness다. `subty_*` totality(사용-기반
false-보완으로 손수 닫은 것)도 이 성질의 국소 버전.

현재 상태: `[ctor]` 방출(선행)·`--unconditional` 변환·`run-scc.sh` 배선이 전부 끝났고
`badd` 슬라이스로 end-to-end 검증됨(프루닝 737→7 ops, `[ctor]` 보존, verdict 파싱).
**남은 건 CETA 링크 바이너리 하나** — 그게 없으면 SCC가 `ERROR-NO-CETA`로 정직하게 거절한다.

### 원인 — CETA 훅 미바인딩

MFE 3.5.1 배포본에 SCC 2b는 들어 있고(`tools/mfe/src/SCC/scc.maude`, 배너에도 뜸)
`(select tool SCC .)` 까진 되지만, 실제 검사는 안 된다:

```
MFE> (select tool SCC .)   →  The SCC has been set as current tool.
MFE> (scc SPEC .)          →  Warning: The sufficient completeness checker is not
                              fully available. Please use the trust command ...
```

**원인 — CETA 훅 미바인딩.** SCC의 핵심 연산은 트리 오토마타 공허성 판정
`op test-emptiness : Module ~> EmptinessResult [special (id-hook CetaSymbol ...)]`
(`tools/mfe/src/SCC/scc.maude:792`)인데, 이건 **CETA 라이브러리를 링크해 빌드한
Maude(Maude++)** 에만 있는 빌트인이다. 우리 바이너리 둘 다 없음
(`strings maude | grep -ci ceta` → 0):

| 바이너리 | ceta | TerminationCheckerSymbol |
|---|---|---|
| `tools/maude/maude` (stock 3.5.1) | 0 | 0 |
| `tools/maude271-hooks/maude` (v2.7.1-ext-hooks) | **0** | 2 |
| `maude-2.7-hooks-linux` (v2.7-ext-hooks 릴리스 에셋) | **111** | 2 |

주의: `v2.7.1-ext-hooks` 릴리스 **제목**은 "CETA library and MTT hooks"라고 하지만,
그 리눅스 에셋에는 CETA가 실제로 안 들어 있다(MTT 훅만). CETA가 들어 있는 건
한 세대 **아래**인 `v2.7-ext-hooks`의 `maude-2.7-hooks-linux.zip` 뿐이다. 구형
스택(`maude271-hooks` + `mfe271`, SCC 2a 번들)으로 돌려도 같은 warning이 난다 —
즉 MFE 버전 문제가 아니라 **바이너리 문제**.

**설치·구동 (완료 2026-07-11).** `maude-2.7-hooks-linux.zip`(v2.7-ext-hooks)을
`tools/maude27-ceta/`에 풀고(gitignore됨) `MAUDE_LIB`을 거기로, 구형
`tools/mfe271/MFE-mfe-2.7.1/src/mfe.maude`(SCC 2a 번들)로 `(scc SPEC .)`.
[tools/mfe/run-scc.sh](../../tools/mfe/run-scc.sh)가 `run-termination.sh` 패턴을
그대로 복제(슬라이스 덤프 → `prune_slice_signature.py` → 헤더 변환 `(mod`→`(fmod` →
구형 MFE). **버전 스큐 주의**: 구형 Full Maude가 Maude 2.7 아래서 자기 소스에 대해
무해한 `no parse` 경고를 뿜는다 — verdict를 먼저 파싱해야 하고, 안 그러면 판정이 난
실행을 ERROR로 덮는다.

### 판정 읽는 법 (극성이 전부다)

**1. `ceq`→`eq` 변환이 전제.** `CC-CONFIG`의 `drop-bad-eqs = true`
([scc.maude:941-944](../../tools/mfe/src/SCC/scc.maude))가 **조건부·비선형 방정식을
검사 전에 드롭**하므로, 그냥 먹이면 그 심볼의 규칙이 통째로 사라져 판정이 무의미해진다.
그래서 `rewrite --ctrs --unconditional`(=`Rewrite_system.drop_conds`+`linearize_lhs`,
커밋 f1b979c1)로 우리가 먼저 변환해 드롭될 게 없게 만든다. SCC는 오토마타 전이를
**`lhs(Eq)`에서만** 만들므로(`sca-eq-rules`) rhs는 봐도 그만 — 조건 제거로 rhs가
unbound가 되는 규칙(p4 1,164건)은 rhs를 lhs로 대체한다(`nonexec`가 되면 SCC의
`is-exec?`가 도로 드롭).

**2. 그래서 반례만 신뢰한다.** 조건 제거·선형화는 **매칭 과대 근사** → 빠진 케이스를
가릴 순 있어도 없는 걸 만들어내진 못한다. ⇒ **COUNTEREXAMPLE은 건전**(변환 여부 무관),
**COMPLETE는 변환 안 된 심볼(`exact`)에서만 증명**. run-scc.sh가 fidelity 열로 구분한다.

**3. 규모 실측 (2026-07-11, p4 분석 표면).** 예전 이 절에 있던 "우리 표면은 `ceq`가
압도적"이라는 서술은 **틀렸다**: 실제로는 `eq` 72,518 / `ceq` 2,511. 심볼 기준
2,403개 중 **무조건-only 1,722**(= SCC가 규칙 전부를 보는 `exact` 심볼: `subty_*` 486,
`match_*` 465, 함수 435, relation·프리루드 285, `holds_*` 51), 혼재 394, `ceq`-only 287
(전부 함수·relation — 의미론 본체). ⇒ 본체는 여전히 `approx`지만, **`subty_*`/`match_*`/
산술 프리루드는 온전히 검사된다** — 마침 우리가 totality를 손수 넣은 바로 그 가족들.

### 실측 결과 — 산술 프리루드 (2026-07-11, 전부 `exact`, SCC 자기 분석도 `complete+sound`)

| 심볼 | verdict | witness |
|---|---|---|
| `bsucc` | **COMPLETE** | — (total) |
| `badd` / `bmul` | COUNTEREXAMPLE | `badd_carry(bzero, bzero)` |
| `bsub` | COUNTEREXAMPLE | `bpred_double(bzero)` |
| `bdiv` / `bmod` | COUNTEREXAMPLE | `bdiv(bone, bzero)` / `bmod(bone, bzero)` |

**트리아지 — 둘 다 진짜 gap이 아니라 이미 알고 있던 두 성질의 기계적 재발견.**

- **canonicity 불변식** (`badd_carry(bzero,·)`, `bpred_double(bzero)`): `bd0`/`bd1`은
  절대 `bzero`를 감싸지 않는다는 이진 인코딩의 불변식 때문에 도달 불가. [prelude.ml:294-297]
  (translate/prelude.ml)이 **"verified by hand for every rule below, not enforced by the
  sort system"** 이라고 명시한 바로 그것 — SCC가 그 문장을 기계적으로 확인해 준 셈이다.
  `BPos < BNatV` 서브소트를 넣으면 COMPLETE로 떨어지는데, 그 서브소트는 `Maude_sorts`가
  심볼당 시그니처 1개만 지원해서 포기했던 것(835d1537). **⇒ SCC가 그 설계 타협의 비용을
  정확히 정량화한다: 불변식이 깨져도 타입 시스템이 안 잡고 조용히 stuck된다.**
- **의도적 부분성** (`bdiv`/`bmod`의 0 제수): 제수 패턴이 `bone`/`bd0`/`bd1`뿐 — 0으로
  나눈 값은 없다. 인터프리터는 에러, Maude는 stuck ⇒ 양쪽 다 거부라 differential 정합
  (completeness gap 0과 모순 없음). `eqg`의 대각 밖 stuck과 같은 부류: 고칠 게 아니라
  문서화할 것.

### 실측 결과 — `subty_*`/`match_*` 표본 (2026-07-11, 3개만 시범)

| 심볼 | verdict | witness |
|---|---|---|
| `subty_typeIR` | COUNTEREXAMPLE | `subty_list_fieldValue(bone)` |
| `subty_value` | COUNTEREXAMPLE | `subty_value(bone)` |
| `match_typeIR_BOOL_0` | COUNTEREXAMPLE | `match_typeIR_BOOL_0(false)` |

**셋 다 같은 원인 — op 도메인이 `Val`로 너무 넓다 (⇒ 협소화가 선행 과제).**
`op subty-typeIR : Val -> BoolV`, `op match-typeIR-BOOL-0 : Val -> BoolV`로 선언돼
있어서 `bone`(NatV < Val)·`false`(BoolV < Val) 같은 **엉뚱한 타입의 인자가 sort상
합법**이 되고, 당연히 어떤 규칙도 안 덮으니 반례가 된다. 실제 호출은 언제나 선언된
IL 타입의 값이라 **전부 도달 불가** — CRC MAYBE 트리아지와 같은 "witness를 보라"의
전형이다.

⇒ **이 가족이 SCC의 최고가치 타겟인데, 지금 그대로는 판정이 의미가 없다.**
`subty_<T>`/`match_<T>_<K>`의 도메인을 `Val`이 아니라 실제 IL 타입 sort(`TypeIR`
등)로 좁혀야 비로소 "COMPLETE = usage-based totality의 기계 증명"이 성립한다.

**⚠️ 단, 그냥 좁히면 안 된다 — `Val`-wide는 의도적이다.** `1874d212`가 정확히
그 반대 방향으로 갔다: matcher/subty/holds/eqg 도메인을 **무조건 `Val`로 넓힌** 이유가
"좁은 도메인이면 `Reflect.sibling_guard`가 바깥 타입 주어에 중첩 variant의 matcher를
불러 ill-sorted가 되고, owise 가드가 영구 stuck"이기 때문(48e59d5f의 협소화를 되돌린
것). ⇒ **선언 타입으로 좁히는 건 답이 아니다.**

### P1 해소 — 술어 도메인 = "쓰이는 주어들의 join" 고정점 (2026-07-14, 완료)

`Maude_sorts.predicate_domains`: 선언에서 **시드**(matcher→포함 타입, `holds_R`→R의 입력
타입, `subty_T`→T의 sort)를 잡고, **규칙에 실제로 나타나는 모든 인자 sort**(자기 정의
규칙이 해체하는 생성자 + 모든 호출부의 인자)로 **join**해 고정점까지 넓힌다. 결과는
정의상 "실제로 오는 주어들의 상한"이라 **1874d212의 stuck guard가 구조적으로 재발
불가**하면서, 스펙이 허락하는 만큼 좁다. `eqg`만 제외(임의 두 타입을 비교하므로 `Val`이
진짜 도메인).

- **실측(specs/p4)**: `match_` **464/465**, `subty_` **366/486**, `holds_` **39/51** —
  가족의 **86%**가 진짜 IL sort로 선언된다. 남는 `Val`은 전부 **컨테이너/스칼라
  erasure**(`cons`의 원소, 튜플 슬롯, `sub_nat`의 Int-or-Nat 주어)라 정직하게 `Val`로 둔다.
- 부수적으로 필요했던 두 가지: (1) `infer_var_sorts`에서 **술어 인자 위치는 변수 sort를
  좁히지 않는다**(consumer-only; 정의 규칙의 lhs head는 계속 authoritative) —
  안 그러면 바깥 타입 변수가 중첩 타입으로 조용히 좁아져 lhs 패턴 의미가 바뀐다.
  (2) `stuck_head_eqs`의 가드 패턴을 **`Val` 고정**(head가 뭔지만 묻는 등식인데, 잡아야 할
  대상이 바로 인자가 `Val` kind에 뜬 stuck 항이다).
- **슬라이스 주의(치명적)**: 도메인은 **풀 시스템**에서 복원해야 한다(`~sig_rules`).
  슬라이스/`--unconditional`은 **호출부를 지우므로**, 거기서 join하면 도메인이 seed로
  붕괴하고 SCC가 "아무도 실행하지 않는 시스템"에 대해 COMPLETE를 준다.
- **무회귀**: impty 골든은 op 도메인 줄만 변경(`--wide-predicate-domains`로 돌리면
  byte-identical), `dune runtest` 통과, impty 8/8(native+run-structural),
  p4 `--check-p4` 12/12 MATCH·`run-structural` 6/6(두 모드 동일), CRC `$lookup` YES/YES.

### 실측 결과 — 협소화 후 (2026-07-14)

| 심볼 | 2026-07-11 | 지금 |
|---|---|---|
| `match_typeIR_BOOL_0` | COUNTEREXAMPLE `match_typeIR_BOOL_0(false)` | **COMPLETE** (exact) |
| `subty_value` | COUNTEREXAMPLE `subty_value(bone)` | COUNTEREXAMPLE `sub_nat(invalidHeaderValue)` — **`dom:Val-wide`** |
| `subty_typeIR` | COUNTEREXAMPLE `subty_list_fieldValue(bone)` | COUNTEREXAMPLE `subty_prefixedNameIR(setValue)` — **`dom:Val-wide`** |

`run-scc.sh`가 이제 반례마다 **witness의 head 심볼 도메인**을 `dom:narrow|Val-wide`로
자동 표시한다(트리아지 1번 질문의 기계화). 남은 두 반례는 **둘 다 아직 `Val`인 심볼**을
가리킨다 — 즉 위양성이 `subty_*` 자신에서 **한 단계 아래 Val-wide 심볼로 이동**했다:

- **`sub_nat` (프리루드)**: `IntV`(`int_pos`/`int_neg`)와 `NatV`(`bzero`/`bone`/`bd0`/`bd1`)
  **양쪽에서 호출**되는데 두 sort의 공통 상위는 `Val`뿐이라 join이 `Val`로 떨어진다.
  → 후속 선택지: (a) 합성 union sort `Scalar`(`NatV`,`IntV` < `Scalar`)를 도입해
  `sub_nat : Scalar -> BoolV`로 선언하면 6개 규칙이 그 도메인을 총망라 → COMPLETE.
  (b) 애초에 **주어의 정적 타입이 이미 nat이면 `sub_nat` 호출 자체가 자명하게 true**다
  (`subty_value(integerLiteral_W(x0:NatV, ..)) = and(sub_nat(x0:NatV), true)`) — `sub_pred`가
  그 경우 `true`를 바로 방출하면 호출이 사라진다. (b)가 더 근본적.
- **`subty_prefixedNameIR` 등 120개 subty_**: 컨테이너 원소를 통해서만 불려서 `Val`에 남음.

**⇒ COMPLETE는 도메인 폭과 무관하게 항상 유효한 증명이다**(더 넓은 도메인에서의 totality는
더 강한 진술). 신뢰도 캐비엇이 필요한 건 **COUNTEREXAMPLE 쪽뿐**이고, 그건 이제 `dom:` 열이
자동으로 알려준다.

### 전수 스윕 결과 — `subty_`/`match_` 951심볼 (2026-07-14)

`--slice-dir`로 2,403개 슬라이스를 **한 번의 번역**(30분, 2.7G)으로 덤프하고
`run-scc-sweep.sh '^(subty|match)_'`로 951개 전부를 검사했다. **951/951이 `exact`
fidelity + `analysis:complete+sound`** — 즉 SCC가 우리 규칙을 그대로 보았고 자기 분석도
sound하다고 선언했으므로, 아래 판정은 전부 액면 그대로 읽어도 된다.

| verdict | 수 | 내역 |
|---|---|---|
| **COMPLETE** | **503** | `match_` 464/465 + `subty_` 39 — **totality의 기계 증명** |
| COUNTEREXAMPLE | 448 | `dom:Val-wide` 405 / `dom:elem-erased` 43 |
| **`dom:narrow`** | **0** | — |

**`dom:narrow` 반례가 하나도 없다** = 이 가족에서 SCC가 찾아낸 **진짜 빠진 케이스는 0건**.
2026-07-11에 위양성이던 `match_*` 465개는 이제 464개가 COMPLETE다(유일한 예외 `match_cons`도
`dom:Val-wide`).

**반례 448건의 정체 — 448건 중 407건은 검사 대상 심볼이 아니라 그 의존성을 가리킨다.**
witness의 head를 세면 `subty-boolTypeIR` 77 / `subty-name` 59 / `subty-expression` 58 / …
— **아직 `Val`로 남은 소수의 `subty_` 심볼 하나가 그것을 쓰는 모든 슬라이스를 오염시킨다.**
따라서 잔여 `Val`을 지우는 것이 곧 반례를 지우는 것이다.

**그 중 절반은 join의 모호성이었다(해소, `3cde77b4`).** `BoolTypeIR`의 상위는
`BaseTypeIR`/`TypeIR`/`TypedefIR` 셋으로 **비교 불가** — p4의 union이 겹치므로 관측 주어들의
최소 상계가 여럿이고 join이 아예 없다. `lub`가 그때 `Val`로 후퇴했는데, **공통 상계는 어느
것이든 정의상 well-sorted**(모든 관측 주어를 지배)이므로 그 중 **최소·최협소인 것을
결정적으로** 고르면 된다. → `subty_` 도메인 366/486 → **404/486**, `subty_boolTypeIR`은
`Val` → `TypeIR`이 되고 SCC 판정이 **COUNTEREXAMPLE → COMPLETE**로 뒤집혔다
(`match_` 464/465·`holds_` 39/51 무변화, impty 골든 무변화, p4 `--check-p4` 6/6 MATCH).

**남은 82개 `subty_`의 `Val`은 다른 원인 — 보완(complement) 절이 무관한 소스 타입까지 넘나든다.**
`subty_name`의 주어를 세면 `Name` 변수 외에 `nameList`·`typeParameterList`(리스트 타입!)
생성자가 섞여 있고, `subty_expression`의 반례 witness는 `setValue`(**Value** 생성자)다.
리스트 sort와 `Name`의 공통 상위는 `Val`뿐이라 join이 진짜로 `Val`이다. 즉 `sub_complement_defs`가
"타깃 `T`에 대해 어딘가에서 소스로 등장한 **모든** 타입"의 생성자에 `= false` 절을 다는 탓에,
서로 무관한 union을 가로질러 도메인이 벌어진다. **후속(P3)**: 보완 절을 실제로 검사되는
(소스, 타깃) 쌍으로 제한하거나, 타깃별이 아니라 (소스,타깃)별 술어로 쪼갠다. 이걸 닫으면
`dom:Val-wide` 405건이 대부분 COMPLETE로 떨어질 것으로 본다.

`dom:elem-erased` 43건(`subty_list_value`/`subty_list_fieldValue` 등)은 **원소 타입이 `List`
sort에 남아 있지 않아서** 생기는 구조적 잔재로, 규칙 2(원소 변수 협소화)의 대가이자 호출부가
만들 수 없는 항이다 — 고칠 대상이 아니라 보고할 대상.

### 할 일
- [x] (선행) op 선언에 `[ctor]` 방출 — **완료 2026-07-11**, `to_mfe`(분석)·`to_maude`
  (실행) 두 표면 모두. 생성자 집합은 `Maude_sorts.is_ctor`/`ctor_attr`(이론별 스칼라 +
  컨테이너 + `il_ctor_syms`), 방정식을 가진 심볼이 생성자로 선언되는 걸 막는 데모션
  가드 포함(p4에서 발화 0건).
- [x] `tools/maude27-ceta/` 설치 + `run-scc.sh` — **완료 2026-07-11** (커밋 7e88a68c).
- [x] 산술 프리루드 + `subty_*`/`match_*` **최소 시범** — 완료 (위 두 표).
- [x] **(P1, 선행) 술어 op 도메인 협소화** — **완료 2026-07-14** (`Maude_sorts.predicate_domains`,
  위 절). 선언 타입이 아니라 **쓰이는 주어들의 join 고정점**. 가족의 86%가 진짜 sort로.
- [x] **프루닝 선행 수정** — **완료 2026-07-14**. `prune_slice_signature.py`가 subsort 경로의
  **내부 노드를 keep**하도록(p4에 2단 체인 286개 — `BaseType < RealTypeArgument <
  TypeArgument`). 도메인이 전부 `Val`이던 동안엔 모든 sort가 `< Val` 직결이라 무해했지만,
  좁힌 도메인은 실제 격자에 의존하므로 중간 sort가 잘리면 슬라이스가 ill-sorted가 된다.
- [x] **(P2, 선행) `--slice-dir` 배치 덤프** — **완료 2026-07-14** (`b97afab3`). 심볼당
  49.9초 재번역(=24시간)이 **한 번의 번역**(2,403 슬라이스, 30분)으로. `run-scc.sh`가
  `SCC_SLICE_DIR`로 소비하고, `run-scc-sweep.sh`가 작은 슬라이스부터 resumable하게 돈다.
- [x] **(P2) `subty_`/`match_` 전수 스윕** — **완료 2026-07-14** (951/951, 위 절).
  503 COMPLETE / 448 COUNTEREXAMPLE, **`dom:narrow` 0건**.
- [x] **(P3) `sub_nat`의 Val 도메인 해소** — **완료 2026-07-14** (`381c6bd0`). `NatV`,`IntV`
  `< NumV` 상위 sort를 (스펙이 `sub_nat`에 실제 도달할 때만) 선언 → 6개 규칙이 도메인을
  총망라, 반례 소멸. impty 골든은 byte-identical.
- [x] **(P3) 모호한 join의 `Val` 후퇴 해소** — **완료 2026-07-14** (`3cde77b4`). 최소 상계가
  여럿일 때 `Val`이 아니라 그 중 최협소한 것을 결정적으로 선택.
- [ ] **(P3, 다음) 잔여 82개 `subty_`의 `Val`** — 원인은 보완 절이 무관한 소스 타입을
  넘나드는 것(위 절). 보완을 실제 검사되는 (소스,타깃) 쌍으로 제한 → `dom:Val-wide` 405건
  대부분이 COMPLETE로 떨어질 전망.
- [ ] **(P2) 나머지 심볼 스윕** — 함수 435 / relation·프리루드 285 / `holds_*` 51, 그리고
  `approx` 756개(COMPLETE는 무시하고 **COUNTEREXAMPLE만** 수확 → 진짜 stuck 후보 목록).
  슬라이스는 이미 덤프돼 있으니 `run-scc-sweep.sh <pattern>`만 돌리면 된다.
- [ ] (선택) `BPos < BNatV` 서브소트 — `Maude_sorts`의 심볼당-단일-시그니처 제약을
  풀어야 함. 풀면 canonicity 반례 3개가 COMPLETE로 떨어지고, 불변식이 sort로 강제된다.

## LTL 모델 검사 — P4 언어의 시간적 성질 검증 (신규, 미착수)

**동기.** 지금까지 Maude는 두 축으로만 쓰였다: (1) **실행 엔진**(`reduce` — 프로그램
하나의 타입검사 결과), (2) **분석 게이트**(CRC/ChC/termination — 재작성 시스템 자체의
성질). 어느 쪽도 "**P4 프로그램이 실행 도중 무엇을 지키는가**"(header validity, parser
종료, table 적용 결정성 …)를 묻지 않는다. Maude의 **LTL 모델 검사기**
(`model-checker.maude`, `modelCheck(init, φ)`)가 세 번째 축 — 시간 논리 공식 φ를
상태공간 전체에 대해 검사하고, 실패하면 **반례 경로**(`counterexample(path, cycle)`)를
돌려준다.

**LTL은 Kripke 구조를 요구한다 — 지금 우리 모듈엔 그게 없다.** 필요한 네 조각:
상태 sort(`State`), **`rl` 전이 규칙**, 원자명제 sort(`Prop`), 만족 관계
(`op _|=_ : State Prop -> Bool`). 현재 p4 모듈은 relation이 전부 input-moded라 **`rl`이
0개**(전부 `eq`) — 전이가 없으니 상태공간 자체가 없고 `modelCheck`는 무의미하다. 아래
(P1)·(P2)가 그 구조를 만드는 일이고, (P3)이 LTL 본체다. `search`는 이 축의 목표가
아니라 **LTL 배선을 만드는 김에 거의 공짜로 얻는 보조 반증 도구**로만 다룬다(아래).

**전제 조건 (배선부터 필요).**

- [ ] **(P1) 선택적 `rl` 모드 — Kripke 전이 만들기.** `--relations-as-rules`는 *모든*
  relation을 rl로 바꿔 상태공간을 무의미하게 키운다. 필요한 건 **동적 의미론의 step
  relation만** rl로 두는 선택적 플래그(`--rules-for R1,R2` 같은 것). 시작항도
  `Program_ok(..)` 판정 호출이 아니라 **전이할 상태**여야 한다. 이게 LTL의 유일한
  하드 블로커다.
  **⚠️ 2026-07-14 삭제 주의**: 모드-힌트 기반 rl 선택 기계 —
  `To_ctrs.rule_head_syms`/`input_moded_rel_syms`, `To_mfe`/`Mfe.check`/
  `fold_premise_binders`의 `~rule_heads` 배선, 그리고 `To_maude.rule_relation_syms`의
  **ceq-는-`=>`-조건-불가 fixpoint**(eq로 남은 관계의 조건이 rl 관계를 호출하면 그
  관계도 crl로 강등) — 는 모든 관계가 입력-모드라 사어여서 삭제됐다(이 항목 작성
  당시에도 `rule_head_syms`는 ∅). `--rules-for` 구현 시 (a) 선택 집합 배선과 (b) 그
  fixpoint 적법성 제약을 다시 들여와야 한다. 복원 참조: `git log -S rule_relation_syms`
  직전 트리(삭제 커밋 부모)에서 — 기존 삭제-모듈 관례와 동일.
- [ ] **(P2) `modelCheck` 배선.** `MODEL-CHECKER` 모듈 protecting + step relation의 상태
  sort를 `State`로 subsort + `Prop` 선언 + `_|=_` 등식. **설계의 핵심 질문은 "원자명제를
  스펙에서 어떻게 얻는가"** — 유력 후보는 `Reflect`가 이미 만드는 **`holds_R`**(무출력
  judgment의 성공 반사, 무조건 규칙 1개, BoolV 시그니처)를 그대로 명제로 쓰는 것
  (`eq S |= p_R = holds_R(S)`). 결과 디코딩(`true` vs `counterexample(...)`)엔 `Of_maude`에
  **전이-경로 파서**가 필요하다. CLI는 `run`에 `--ltl 'φ'` 정도로 붙이면 된다.
  **⚠️ 유한성 제약**: P4 **타입검사**의 상태공간은 항 크기가 무제한이라 무한하다 —
  modelCheck가 완주하려면 **고정 프로그램 + 고정 입력의 동적 의미론**처럼 상태공간이
  유한한 곳에 걸어야 한다. (무한 상태공간에서 반증만 원한다면 depth-bounded search가
  대안 — 아래 P3.)
- [ ] **(P3, 보조) `Maude_run.Search` 일반화.** LTL이 감당 못 하는 무한/거대 상태공간에서
  **bound된 반증**을 얻는 용도. 현재는 디버깅용 근사다
  ([maude_run.ml:148-152](maude/maude_run.ml#L148-L152)):
  `metaSearch(M, start, 'R:Val, nil, '!, unbounded, N)` — 패턴이 bare 변수, 조건 `nil`,
  화살표 `'!`(`=>!`) 고정, 깊이 unbounded, 해는 인덱스 `0..cap-1` 순차 열거
  (`search_cap = 100`). 필요한 건 임의 **search pattern** + **`such that` 조건**
  (metaSearch의 3·4번째 인자), 화살표 `=>*`/`=>+`/`=>1` 선택, **깊이 bound**, 그리고 반례
  경로용 `metaSearchPath`. "반례 없음"은 기존 `NoSolution`을 그대로 쓴다. (P2)의 경로
  파서와 디코딩 코드를 공유한다.

**검증 대상 LTL 성질.** 전부 **반증 지향** — 반례가 나오면 진짜 결함, 안 나오면 (유한
모델/bound 안에서) 성립.

- [ ] **header validity 안전성**: invalid header를 절대 읽지 않는다 — `[] ~ readInvalid`.
  LTL이 가장 자연스럽게 잡는 성질이고 첫 타깃으로 적합.
- [ ] **parser 종료/도달성**: P4 parser는 루프 가능 — `<> (accept \/ reject)`(항상 결말에
  도달), `accept` 도달성, 무한 루프 검출.
- [ ] **progress (무-stuck)**: well-typed 프로그램은 stuck 상태에 안 빠진다 —
  `[] ~ stuck`. 현재의 `Stuck` 판정을 프로그램 하나가 아니라 **상태공간 전체**로 확장.
- [ ] **preservation**: 모든 도달 가능 상태가 well-typed — `[] p_Type_ok`
  (`holds_Type_ok`를 원자명제로).
- [ ] **결정성**: 종료 상태가 유일 — LTL로는 직접 못 쓰므로 `search P =>! X`의 해가 정확히
  1개인지로 확인(P3 몫). CRC(등식 층의 합류성)와 상보적 — rl 층 + 실제 도달 가능한 상태만
  본다.
- [ ] **table entry 우선순위 결정성**: 같은 키에 매칭되는 두 엔트리의 우선순위가 동률이면
  적용 순서가 비결정 — search로 반례 탐색(P3 몫).

**권장 착수 순서**: (P1) rl 모드 → (P2) modelCheck 배선 → header validity(`[] ~ readInvalid`)
를 기존 corpus 프로그램 하나에 걸어 end-to-end 확인 → parser/progress/preservation LTL →
(P3) search 일반화로 무한 상태공간 성질(결정성·table 우선순위) 보강.

## 권장 순서

(이미 복구·완료: CLI 배선, `Exp_map`, `Defunctionalize`/`Gensym`/`Builtin` + 파이프라인,
`Simplify`=identity, Maude 백엔드 `To_maude`/`Maude_run`/`Of_maude`,
`To_ctrs.of_spec`/`var_type_hints` 포팅, **Native 직접 생성 리팩토링 (B)**,
**impty/base 분석 골든 고정**(`specs/impty/base/spec.ctrs`), **Mfe calibration**,
**M2 CLI `run` 실행 배선**, **M3 differential**(completeness 0 / soundness 1 /
Phase D 1227/1227 MATCH).)

(추가 완료 2026-07-02: **subty totality — 사용-기반 false-보완**(`sub_complement_defs`;
interp.subtyp 대조로 struct width/depth·타입파라미터·스칼라 근사 전부 종결),
**MFE 환경 재보정**(심볼당 ~4분, `--timeout 360`+ 필요).)

(추가 완료 2026-07-03: **owise 반사 확장 — 수집형 IterPr(`$itercollect`/`$iterapply`)
성공 반사**(`Reflect.gen_itercollect_holds`/`gen_iterapply_holds` + 전역 성공-테스트
삽입 + `cond_heads`의 재귀 term 스캔; p4 owise 51/72 → 66/72 반사, kept 21 → 6).)

(추가 완료 2026-07-03: **(B) match-가드 discriminator head-패턴 폴드**
(`Reflect.hoist_matchers`; 위 M1 "남은 14 MAYBE" 항목 참조) — `match_K(subj)=true`를
`subj=K(fresh..)`로 재철자해 기존 `fold_premise_binders`가 head로 접게 함, subj가 같은
rule의 다른 조건에도 안 쓰일 때만(companion destructure 충돌 회피). impty 골든 갱신
(`$lookup`/`Check-command`/`Eval-command`의 nullary 케이스 3건 head-fold, byte-identical
아님이지만 의도된 개선; 실행 표면 `spec.maude` 무변화 확인). **MFE 실측(p4 `$un_op`/
`$inherit_i` MAYBE→YES, 회귀 재검)은 이 세션에서 미완 — MFE 환경이 impty `$lookup`
대조군(과거 ~1.4s)조차 `--timeout 900`에서 TIMEOUT, `git stash` 베이스라인도 동일 증상이라
코드 회귀 아닌 기존 환경 문제로 확인(아래 M1 (B) 항목에 상세). 다음 세션에서 환경 복구 후
재검 필요.**)

(추가 완료 2026-07-03: **`$unzip`/`$iterproj` 규칙-생성 코드 통합 + reflect.ml
`$iterproj` 하드게이트 제거**(위 M1 항목 참조) — `to_ctrs.ml`의 공유 헬퍼
`spine_projection_rules` + `reflect.ml`의 `iter_helper_prefixes`에서 `$iterproj`
제거; impty golden·p4 전체 corpus `--ctrs`+`reflect:` stderr 모두 byte-identical,
`TableProperty_ok`/`Decl_ok` 슬라이스로 공유 경로 발화 확인. 이 세션 시작 시 (B) MFE
재검을 먼저 시도했으나 impty `$lookup` 대조군이 `--timeout 280`에서도 TIMEOUT —
환경이 여전히 미회복임을 재확인(회귀 아님), 그래서 환경 비의존적인 이 항목으로
전환해 완료.)

(추가 완료 2026-07-04: **owise 반사의 gensym-effectful 사전 게이트 제거**
(`reflect.ml`의 `owise` 함수 안, 형제 guard를 만들기도 전에 "이 규칙의 head 심볼이
effectful 집합에 속하냐"만 보고 무조건 막던 2줄 삭제). `gensym.ml`의 `thread_rule`이
컴파일 타임에 강제하는 불변식(effectful 규칙의 원래 LHS 패턴에는 절대 effectful
호출이 들어갈 수 없음 — 위반 시 `failwith`)과, `ctest`가 형제 조건을 훑을 때 이미
무조건 `check_reflectable`을 호출해 실제 effectful 언급을 정확히 잡아내는 기존
안전장치 덕분에, 이 사전 게이트는 불필요하게 보수적이었음이 실측으로 확인됨(예:
`$subst_typeIR`의 유일한 형제 절은 조건이 0개라 guard가
`eqg(theta, variant-set-lbrace-rbrace-1(nil))`뿐이라 gensym state를 아예 언급하지
않는데도 막혀 있었음). 2줄 삭제 후 p4: 66/72 → **71/72 반사**(5개
`$subst_typeIR`/`$subst_parameterIR`/`$subst_callableTypeIR`/
`$subst_callableTypeDefIR`/`$subst_constructorTypeIR` 전부 새 Gate 사유 없이 반사됨,
남은 1개는 무관한 `$find_local_return_type_t`). impty/base·impty/closure 골든(`spec.ctrs`/
`spec.maude`) byte-identical. 부수 효과로 레거시 `specs/p4-old`(활성 검증 대상 아님)에서도
동일 패턴 7개가 반사됨; 그중 `$compat_table_key`는 `check_reflectable`이 실제로 다른
gensym 심볼(`$compat_table_exact_optional_key`)을 잡아 정상적으로 계속 게이트되는 것도
확인 — 안전장치가 실전에서도 정확히 작동함을 보여줌.)

(추가 완료 2026-07-13: **`Reflect.align_guards` — 상보 비교/부정 가드 정렬로
sign-split MAYBE 해소**(`reflect.ml`, `pipeline.ml`의 `ctrs_of_spec`에서
`fold_premise_binders` 뒤·`owise` 앞). 조건 위치의 `lt(a,b)=true`를
`leq(b,a)=false`로(그리고 `_int`/`false`-극성 변형, 선두 `not(x)=b`는 `x=¬b`로)
재철자해, `i<0`(arith shift) vs `i>=0`(logical shift)로 갈리는 형제 절이 번역
후 갖게 되는 서로 다른 subject(`lt_int(X,0)=true` vs swapped `leq_int(0,X)=true`)를
**같은 subject `leq_int(0,X)`의 true/false 극성**으로 통일 → CRC가 가설 재작성으로
임계쌍을 discharge.
**근본 원인 규명**: prelude의 기존 bridge `lt_int(x,y)=not(leq_int(y,x))`가
이 쌍을 못 고쳤던 이유는 — 여기서 `X=$bitstr_to_int(..)`의 복원 sort가 최상위
`Val`인데 bridge는 하위 sort `IntV`에 선언돼 있어 **발화하지 못하기** 때문
(축소 슬라이스 CRC 실측: bridge 있는 채로도 6 임계쌍 생존). align_guards는
`leq_int` 심볼을 직접 써 sort와 무관하게 정렬하므로 이 문제를 우회.
**실측**(`ulimit -s unlimited` + 시그니처-축소 슬라이스 CRC, CRC 3t/Maude 3.5.1a):
`$bin_shr` 자기-레이어 12절 — 재철자 전 MAYBE(6 ccp) → 후 **YES(0 ccp)**;
`$bin_satplus` **YES**; `$bin_satminus`는 동형(같은 상보 형태)이나 축소 CRC가
산술 라이브러리 무게로 완주 미측정(sign-split 해소는 동일). 미니모듈 e1~e9로
CRC 판정 법칙 재확인 — "same-subject true/false 극성 충돌만 discharge, not()/
bridge 사슬은 sort가 맞을 때만 통과"(메모리 `mfe-crc-hypothesis-rewriting` v2).
**무회귀**: impty/base 골든 byte-identical(impty엔 `lt`/`not` 조건 없음), p4
전체 `--ctrs` 재철자 정합(조건 위치 `lt`/`lt-int`/최상위 `not(` 0건, subty·함수
부정은 `= false`로 평탄화), owise 반사 72 reflected/0 kept·subty expansion 정상
(align_guards가 뒤따르는 owise 반사를 안 깨뜨림 — 오히려 평탄화된 `not`을 owise가
정상 처리). 실행 표면 무영향(패스는 `ctrs_of_spec` 전용). CLAUDE.md의
"spurious CRC MAYBE — `$bin_shr`/`$bin_satplus` sign/range split" 트리아지 항목은
이 패스로 해소됨 — 갱신 필요.)

(추가 완료 2026-07-07: **MFE 환경 차단 해제 확인 + (B) 실측 완주 + B′ subty-가드
확장 + owise 72/72(ambiguous matcher 해소) + holds_R output-carrying 일반화 +
`drop_owise` 폴백 제거.** 상세:)

- **환경: 차단은 옛 WSL 환경 문제였고 현 dev 컨테이너(RAM 121GB/32코어, cgroup
  무제한)에서는 재현 안 됨** — impty `$lookup` 대조군 `--timeout 120` → **YES/YES
  1초**(과거 ~1.4s 수준 복원). p4 verify는 회당 ~110s의 translate 고정비 + CRC.
  과거 "심볼당 ~4분/TIMEOUT" 메모는 이 환경에는 적용되지 않음.
- **(B) MFE 실측 완주**: `$inherit_i`(10규칙) **YES/YES**, `$un_op`(287규칙)은
  환경이 아니라 슬라이스 크기가 원인으로 재분류(baseline `--timeout 600` TIMEOUT;
  subty 가드가 subty/보완 가족을 슬라이스로 끌어옴). impty `$lookup` 대조군 YES/YES.
  B′ 확장 후 `$un_op` 슬라이스는 287→**87규칙**으로 줄었는데, CRC가 이번엔 Peano
  산술 재귀 규칙군($int_to_bitstr/$bitstr_to_int류)의 임계쌍 계산에서 **maude
  native "Fatal error: stack overflow"로 사망**(verdict 없음으로 관측) —
  check_diff_structural_p4.sh가 이미 배운 것과 같은 교훈(e73fcb44, 기본 8MB 스택은
  정상적으로-깊은 계산에 부족)이라 [mfe.ml](mfe.ml) `run_mfe`가 maude를
  `ulimit -s unlimited` 셸 래핑으로 spawn하도록 수정. 무제한 스택에서는 크래시
  없이 계산이 지속됨(수동 재현 15분+ 생존, 8MB에서는 ~5분에 사망) — 잔여 장애물은
  스택이 아니라 87규칙 산술 슬라이스의 순수 CRC 임계쌍 비용 — 무제한 스택 재검에서
  **CRC 완주(~15분): verdict MAYBE + sort-decreasing**. 잔여 임계쌍(ccp SPEC100)은
  `$un_bnot`의 `$bneg(i)=i'` 출력 바인더가 rhs `$ite(..i'..i'..)`에 2회 쓰여
  `fold_premise_binders`의 중복-방지 게이트(`uses=1 || alias`)에 걸려 남은
  premise-witness 쌍 — **원인 1(전제-only 바인더)의 알려진 잔여 계열**, B′와 무관.
  baseline TIMEOUT 대비 "판정 가능"으로 개선.
- **owise 72/72 (kept 0)**: 마지막 kept `$find_local_return_type_t`의
  `Gate "ambiguous matcher"`는 `ctest`의 `type_of_l`이 struct 필드 접근자 체인의
  결과 타입을 복구하지 못해 `matcher_type`이 `ctor_types` 3-타입 폴백으로 떨어진
  것이 원인 — `tables`에 `fieldsigs`(struct 필드 타입)·`rel_outs`(relation 출력
  타입) 추가, `type_of_l`을 funcsigs → fieldsigs → 단일-출력 rel_outs 순 폴백으로
  확장([translate/reflect.ml](translate/reflect.ml)). 결과: owise 절이 union 타입
  `localKind`의 total matcher 가족(4 matcher × 9 케이스 지원규칙)으로 반사.
  p4 `--ctrs` diff는 정확히 그 owise 절 + 지원규칙뿐, impty 골든·실행 표면
  byte-identical.
- **(B′) subty-가드 확장 — `Reflect.expand_subty_guards` 구현·배선·실측 완료.**
  분석 파이프라인에서 `hoist_matchers` 뒤·`fold_premise_binders` 앞. `subty_S(v)=true`
  가드(v가 head-bound)를 CTRS 스캔으로 얻은 S의 멤버 생성자별로 **1규칙→N규칙
  팬아웃**(θ=[v:=K_i(expand_j..)] 전체 치환, 멤버 residual은 잔여 조건으로),
  클론별 **부분평가**: 콘크리트 ctor 위 matcher/subty 평가(true→조건 제거,
  false/stuck→클론 폐기 — 생성자는 free라 둘 다 unsatisfiable), 동반 destructure
  `K(sf..)=K(pats..)` 점별 분해, fresh-변수 rename(head 패턴 심화), head 밖
  존재변수의 `w=t`(t는 순수 ctor 항) 제거. alias 위임 체이싱, 타입파라미터
  `subty_T(x)->true`는 vacuous drop. 게이트: 멤버 >16 또는 규칙당 클론 budget
  64 초과 시 skip+stderr(`subty_expression` 31멤버 등 대형 variant가 이에 걸림 —
  의도된 보수성). fresh 이름 `expand_%d`는 Var_hints 키와 서로소(sort는 ctor
  시그니처에서 위치 복구).
  - p4: **1738절 → 2496클론(956 dead, vacuous 48)**, 실행 표면(native emit)
    sha256 불변. impty 골든은 의도된 개선으로 재생성(6절 → 6클론+4 dead;
    `Check-expr`/`Eval-expr`의 literal 절들이 무조건 `eq`로, id 절은 head 특수화 +
    존재변수 제거로 `$lookup(..)=some(t)` 한 줄로 접힘; `spec.maude` 불변).
  - **의미 보존 실측**: run-structural impty 8/8 `result: true`, p4
    `issue1301.p4` `--check-p4` **result: MATCH**.
  - **MFE 실측 (post-A, `--timeout 600`, B′ 7심볼 전부 + 회귀 표본):**

    | symbol | 착수 전 | after | 비고 |
    |---|---|---|---|
    | `$flatten_constOpt` | YES | **YES** (116s) | 기존 패스로 이미 해소돼 있었음 |
    | `$tableCustomName` | YES (425s) | **YES** (118s) | 슬라이스 축소로 3.6× 가속 |
    | `$name` | TIMEOUT@600 | **YES** (121s) | 343규칙 → 수십 규칙급 |
    | `$prefixedTypeName` | TIMEOUT@600 | **YES** (125s) | |
    | `$prefixedNonTypeName` | TIMEOUT@600 | **YES** (133s) | |
    | `$invalidate_value` | MAYBE | **YES** (213s) | 잔여 payload 가드 남아도 head 서로소화로 discharge |
    | `$invalidate_headerUnion` | MAYBE | **YES** (210s) | |
    | `$is_lpm_key_prime` (회귀) | YES | YES | 무회귀 |
    | `$inherit_i` (회귀) | YES | YES | 무회귀 |
    | `$join_ctk` (회귀) | MAYBE | MAYBE | 무회귀 — subty 아닌 match-가드 잔여((B)의 companion-destructure 게이트 케이스) |

    **B′ 7/7 YES — "남은 7 MAYBE(subty-disjointness)" 완전 해소.**
- **holds_R output-carrying 일반화 (negation-as-false-value 확장) 구현 완료,
  현 코퍼스에서는 휴면(byte-identical).** `gen_rel_holds`의
  `Gate "output-carrying judgment rule"` 제거(가드는 lhs 패턴+조건만 반사 —
  출력 무시가 곧 존재성 의미), `check_reflectable`의 relation Gate를 `succ` 미포함
  시로 완화, `qualified`를 "규칙 있는 모든 judgment"로 완화. 부정 respell은
  신설 `rel_output_kind`(rel_outs 기반)로 게이트: `No_output`(양/음 모두 respell),
  `Non_bool`(출력이 bool 리터럴일 수 없음 — `=false`만 respell),
  `Maybe_bool`(단일 bool 출력 — 출력값 false와 부정을 구문으로 구별 불가,
  **respell 금지**). 바인딩 call site는 기존 `insert_success_test`가 자동 커버
  (succ 포함 + 비-bool rhs 기준이라 코드 무변경). **p4/impty `--ctrs`
  byte-identical + succ 집합 47 불변으로 무회귀 확정** — p4에는 부정된
  output-carrying relation 조건이 0건(사전 스캔)이라 예측대로 소비자 없음.
  미래 spec에서 `~R(in)` (output relation)이 나타나면 즉시 동작.
- **`drop_owise` 폴백 제거 ([mfe.ml](mfe.ml)).** 72/72 반사로 no-op이 된 drop을
  제거하고, 대신 MFE 입력에 owise 규칙이 남아 있으면(미래 spec의 반사 Gate 회귀)
  `mfe: WARNING - N unreflected owise rule(s) reach the MFE input` stderr 경고 —
  침묵 false MAYBE 대신 원인이 로그로 드러남. CRC엔 보수적(허상 임계쌍 = MAYBE,
  false YES 불가). `Rewrite_system.drop_owise` 함수 자체는 유지(다른 소비자).
  p4/impty에서 경고 미출력(vacuous) 확인, impty verify YES/YES.

(binary 수 인코딩 트랙 진행 상황 갱신 2026-07-08 — 별도 체크아웃
`/home/spectec-core-binenc`, `origin/new-rewrite` 대비 10 commit 전진.
**이 체크아웃/잡은 병행 세션이 사용 중이니 건드리지 말 것**(빌드/재실행 겹치면
in-flight 결과 손상, 과거에도 겪음):

- **Phase 0–3 (BNatV 이진 인코딩 기본기) 완료.** `bzero`/`bone`/`bd0`/`bd1`
  스캐폴딩(`50d2abdc`) — 처음엔 `BPos<BNatV` 서브소트로 설계했다가
  `Maude_sorts`가 심볼당 시그니처 1개만 지원해 서브소트가 안 먹혀 **단일
  `BNatV` 소트로 축소**(`835d1537`) → `bsucc`/`bpred`/`badd`/`bmul`/
  `bcompare`/`bleq`/`blt`(`b84dfb59`) → 절단 뺄셈 `bsub`(3치 마스크, Coq
  `Pos.sub_mask` 스타일, `d8ca81a7`) → **O(log n) 이진 장제법** `bdiv`/`bmod`
  (`99266995`, naive 반복뺄셈 대비 결정적 승리 — `2^64/10`이 1133 rewrite로
  끝남, 반복뺄셈이면 ~1.8e18 스텝) → `bpow_nat`(`397be13c`). 전부
  unreferenced 상태에서 단독 MFE confluence 확인 후 착륙(impty/p4 골든
  no-op 검증 포함).
- **Phase 4 "스위치 전환" 완료(`69023118`).** `int_pos`/`int_neg`의
  magnitude를 Peano `NatV`→`BNatV`로 retype, 동시에 `builtin.ml`
  (`shr`/`shr_arith`/`pow2`/`band`/`bxor`/`bor`/`int_to_text`)까지 같이
  리타겟(분리 불가 — 안 그러면 두 커밋 사이 기간에 tree가 ill-sorted). 이
  시점부터 실제 P4 int가 이진 인코딩으로 실행됨.
- **스위치 전환 후 실 corpus 실행에서 버그 3개 순차 발견·수정**(전부
  impty/synthetic 골든은 못 잡던 것 — 실 corpus 최초 실행에서만 드러남):
  1. `negate_int`의 involution 단축 규칙이 BNatV에서 CRC MAYBE 유발 →
     제거 + CRC용 `ulimit -s unlimited` 배선(`mfe.ml`/
     `Maude_run.run_process`) — `59b5e10c`.
  2. `nat_of_int`가 옛 Peano 시절 규칙(`rule (nat_of_int_t (int_pos_t x)) x`)을
     그대로 갖고 있어 `bit<n>` 있는 프로그램이 전부 첫 스텝에서 stuck →
     `bnat_to_nat` 역브릿지 추가로 수정(`fc01f90c`). **커밋 메시지가 명시:
     이 수정 전에 수집된 binary-encoded 구조적 corpus differential 결과는
     전부 무효, 재실행 필요.**
  3. `abs_nat`/`sub_int_nat`가 `maude_sorts.ml`에 여전히 Peano `NatV`
     시그니처로 선언돼 있어(규칙 자체는 맞았음) 음수 상수 폴딩류 프로그램이
     stuck → 시그니처만 `BNatV`로 수정(`06b05760`, 2026-07-07, 현재 최신
     커밋).
- **진행 중(미커밋, working tree).** `to_ctrs.ml`의 `char_rules` 생성을
  스펙 텍스트 정적 스캔 결과에서 **printable ASCII 전체(32–126) 유니온**으로
  확장 — 인코딩된 시작항은 대상 P4 프로그램의 식별자/문자열 리터럴을
  담는데, 정적 스캔은 스펙 자체의 규칙 텍스트만 보므로 스펙에 안 나온
  바이트(예: 제네릭 파라미터 이름 `T`)는 `chr`는 선언되면서 `eq`가 안 생겨
  stuck. 이 수정 아래 **전체 1568개 corpus 재실행이 진행 중**(로그
  `check_diff_structural_p4_charfix.log`, 2026-07-08 02:56 시작, 확인 시점
  기준 90/1568 완료, 10개 배치당 ~4분 → 완주까지 대략 10시간+ 예상) — 결과
  미확정.
- **참고**: 아래 남은 작업 목록의 "Peano가 bit<32+>에서 OOM" 항목이 가리키던
  원래 기준선(2026-07-07, 메인 체크아웃 Peano 인코딩으로 RESULT 3 MATCH /
  0 MISMATCH / 179 other)은 이 binenc 트랙의 **출발점**이었을 뿐, binenc
  자체의 현재 결과가 아님 — binenc의 첫 실제 결과는 위 진행 중인 재실행이
  끝나야 나옴.)

(추가 완료 2026-07-11: **`[ctor]` 방출 (SCC 선행 작업) + `run`의 타임아웃 기본값 제거.**

- **`[ctor]`**: `Maude_sorts`가 생성자/정의된-심볼 분할을 갖게 됨 — `il_ctor_syms`
  (variant 케이스 + struct 생성자; 필드 접근자/갱신자는 **정의된 심볼**이라 제외,
  선언 커버리지용 `il_declared_syms`와 분리), `shared_ctor_syms`(컨테이너),
  이론별 `scalar_ctor_syms`, 그리고 공유 `ctor_attr`. **이론별 분리가 핵심**:
  `int_pos`/`int_neg`는 Structural에선 생성자지만 Native에선 Maude 내장 `int(_)`로
  가는 **브리지 방정식을 가진 정의된 심볼**이다(데모션 가드가 실제로 잡아냄).
  `To_mfe`(분석)·`To_maude`(실행) 두 표면 모두 방출. 가드: 생성자로 등록됐는데
  방정식이 있으면 `[ctor]`를 떼고 stderr 경고(거짓 주장 방지). p4/impty 양쪽 경고
  0건, impty 실행 8/8 정상, `$lookup` CRC/ChC YES 무회귀. 골든 `spec.ctrs`/
  `spec.maude` 재생성. **SCC 선행 조건 1(= `[ctor]` 방출) 완료** — 남은 건 CETA
  훅 바이너리.
- **`spec.maude` 골든의 matcher-sort "회귀"는 회귀가 아니었음**: `1874d212`
  (match_/subty_/holds_/eqg 도메인을 무조건 Val로 넓힘 — 좁은 도메인이면
  `Reflect.sibling_guard`가 바깥 타입 주어에 중첩 variant의 matcher를 불러 ill-sorted,
  owise 가드가 영구 stuck)가 **골든을 재생성하지 않았을 뿐**. `spec.ctrs`는 이후
  커밋들이 우연히 따라잡았고 `spec.maude`만 stale로 남아 있었다 — 이번에 갱신.
- **`run`/`run-structural`의 `--timeout` 기본값 30초 → 0(무제한)**. p4 native 실행
  실측(2026-07-11): 번역 ~10s + **Maude 모듈 내부화 고정비 ~80s** + 프로그램당 ~6.5s
  (1개 91.6s vs 3개 배치 104.7s로 분리). 고정비가 기본 타임아웃보다 커서 `run --p4`가
  **첫 프로그램을 시작하기도 전에 항상 TIMEOUT**났고, 그 탓에 `check_diff_p4.sh`의
  per-file fallback 경로(`--timeout 0`을 안 넘김)가 조용히 전부 무효였다. 실행 자체는
  정상이었음(타임아웃 늘리면 3/3 `result: MATCH`). 모듈 규모(~78k줄/~74k eq)는
  binenc 병합 이전 커밋(`199c72eb`)에서도 동일 — 최근 회귀 아님, CLAUDE.md의
  "~10s 내부화 / ~4ms per program" 수치가 stale이었던 것(subty 보완 5,425 eq 이전 값).
  CLAUDE.md 성능 절도 실측표로 갱신.)

(추가 완료 2026-07-11: **동반 destructure에 함의된 잉여 매처 가드 제거 —
termination MAYBE의 지배적 원인 해소.** `hoist_matchers`와 `fold_premise_binders`가
**서로를 막고 있었다**: 절 하나가 `match_K(v) = true`와 `v = K(..)`를 둘 다 들고 있으면
(SpecTec 엘라보레이터가 `matches K` 가드와 `let K(x,y) = v`를 별개 전제로 내보내므로 흔한
모양), hoist는 "v가 다른 조건(destructure)에도 나온다"고 skip하고, fold는 "v가 다른
조건(매처 가드)에도 나온다"고 skip한다. 결과적으로 둘 다 남고 **head가 맨 변수로 굳는다**.

- **증상**: 리스트/구조 재귀의 감소 인자가 head가 아니라 **전제에만** 존재
  (`ceq $f(v) = ..$f(v_t).. if match_cons(v)=true /\ v = cons(v_h,v_t)`). AProVE의
  dependency-pair 분석은 `v_t < v`를 세우지 못해 MAYBE — 루프가 아니라 **증명 실패**.
  [verification.md](../../../verification.md)의 term MAYBE 18건 중 14건이 이 모양
  (`$concat_text`/`$exists`/`$forall`/`$filter`/`$join_text`/`$flatten_*`/`$invalidate_*`/
  `$lvalue_as_expression`/`$set_priorities_*`).
- **수정** ([translate/reflect.ml](translate/reflect.ml)): `match_K(v)=true`의 **유일한**
  다른 등장이 같은 K로 destructure하는 조건일 때 그 매처를 **삭제**한다
  (`restated_by_destructure`). `v = K(..)`가 이미 함의하므로 절의 적용 조건은 불변이고,
  v가 더는 "다른 조건"에 안 나오니 기존 `fold_premise_binders`가 **자기 게이트를 그대로 지킨 채**
  destructure를 head로 접는다. 접힌 head `K(..)`가 매처가 지고 있던 disjointness를 그대로 진다.
- **owise 계열은 제외** (`owise_head_syms`) — 거기선 CRC가 `Reflect.owise`의 부정 가드를
  형제의 **동일한 가드 항**으로 재작성해야만 반박하므로(같은-subject 정렬), 형제의 매처를
  지우면 discharge 근거가 사라져 impty `$lookup`이 YES→MAYBE로 역행한다. 실측으로 확인함.
- **실측 (같은 기기, before = 변경 전 바이너리 / after = 이 커밋)**:

  | 측정 | before | after |
  |---|---|---|
  | `$concat_text` termination | MAYBE (237s) | **YES (98s)** |
  | `$flatten_nameList` termination | MAYBE (240s) | **YES (101s)** |
  | `$lookup` CRC/ChC (impty, owise 계열) | YES/YES | YES/YES (무회귀) |
  | `$filter` CRC | TIMEOUT (397s) | TIMEOUT — **회귀 아님**(변경 전에도 TIMEOUT; 이 기기 MFE 성능. verification.md의 YES는 더 빠른 환경 값) |

  의미 보존: 실행(native) 표면 **sha256 불변**(분석 표면 전용), impty `run-structural` 8/8
  `result: true`, p4 `run-structural --check-p4` 표본 **12/12 MATCH / 0 MISMATCH**.
  impty 골든(`spec.ctrs`)은 `Check-expr`/`Check-command`/`Eval-expr`/`Eval-command` 20절이
  head-패턴으로 접히는 **의도된 개선**이라 재생성; `$lookup` 정의 규칙은 byte-identical.
- **남은 것**: `$join_ctk`/`$assignop_as_binop`의 CRC MAYBE는 이 수정 범위 밖 — 원인이 다르다
  (owise 반사 가드 + **패턴형** 형제). MFE 원출력으로 확인: `ccp SPEC1`의 조건이 완전히 ground인
  `or(match_ctk_*(CTK)..) = false`인데, **CRC는 임계쌍 조건을 모듈 등식으로 정규화하지 않아**
  `true = false`(무모순 불가)를 못 본다. 형제가 가드형이면 그 가드 항으로 재작성돼 discharge되지만
  ($is_lpm_key' 등 YES), 패턴형 형제는 밀어줄 hypothesis가 없다. → owise 절을 멤버 생성자로
  fan-out(`expand_subty_guards`와 동일 기법, ctk 3멤버/assignop 13멤버로 ≤16 게이트 통과)하면
  head가 서로소가 되어 임계쌍 자체가 사라진다.)

### ⚠️ 전수 differential 재실행 필요 (2026-07-14, 미완)

**`check_diff_p4_*.tsv`의 "completeness 0 / soundness 1 / Phase D 1227/1227"은 현재 HEAD의
수치가 아니다.** 술어 도메인 협소화(위 SCC P1) 검증 중에 드러났다:

- `check_diff_p4.sh`는 **resumable**이라 TSV에 기록된 프로그램을 건너뛴다. 즉 **옛 바이너리로
  만든 TSV가 남아 있으면 재실행이 아무것도 검증하지 않는다**(`1568 done, 0 to run`).
  ⇒ 회귀 게이트로 쓰기 전에 반드시 TSV를 baseline으로 옮기고 지울 것.
- 그렇게 지우고 새로 돌린 부분 실행(Phase B 1140/1568)에서 **`const.p4`/`issue1717.p4`가
  기존 TSV의 `OK`와 달리 `STUCK`** 으로 나왔다. 협소화 회귀를 의심했으나 —
  **변경 직전 커밋(08dfe4ed)의 바이너리로도 똑같이 STUCK**임을 워크트리로 확인했다
  (`--wide-predicate-domains`에서도 STUCK). ⇒ **이번 변경의 회귀가 아니라, 그 이전 어느
  커밋에서 이미 생긴 completeness gap 2건**이거나, 기존 TSV 자체가 낡은 바이너리 산물이다.
- 이번 변경의 무회귀는 **50개 표본으로 확인**(baseline TSV와 50/50 일치; 1874d212가 고쳤던
  cross-type matcher 케이스 `issue122.p4`/`key-name.p4` 포함).

**할 일**: 깨끗한 환경에서 `check_diff_p4.sh` **전수 재실행**(~8시간) → 진짜 completeness/
soundness/Phase D 수치를 다시 세우고, `const.p4`/`issue1717.p4`가 실제 gap이면 어느 커밋에서
생겼는지 이분 탐색(둘 다 `Program_ok`가 stuck).

남은 작업:

```
  → 큰 슬라이스의 CRC를 예산이 아니라 **슬라이스 축소**로 뚫기                  [맨변수 축은 2026-08-03에 세 갈래 전부 착수(3a6794e7/7215c5dd/48a17692, 위 절): 맨변수 조건 1,326 → 970, 슬라이스 258개 축소. 남은 수단은 추가 pruning/모듈러 분해이고, 비트벡터 7건은 자유변수 제거가 필요조건이되 충분조건이 아닐 수 있다(아래 2026-07-24 note)]
  → orient_conds(bdceb303) 이후 term 재측정                                    [CRC 열은 위 스윕으로 278심볼까지 재측정 완료(다운그레이드 0). term은 "판정 변경 가능"이 아니라 "기존 YES 일부가 미증명"이라 성격이 달라 여전히 남아 있다 — 스윕 중단 상태]
  → 전수 differential 재실행 + const.p4/issue1717.p4 이분 탐색            [기존 TSV 수치가 낡음; 위 절 참조. sanitize 밑줄 보존(2026-08-03)이 실행 표면을 전량 개명했으므로 이번 재실행이 그 무회귀도 겸한다 — 표본 5개는 이미 RESULT MATCH]
  → $bitstr_to_int w=0 실행 비종료                                             [유일한 "진짜 결함" 후보; P4 타입시스템이 bit<0>/int<0> 산술을 막는지 규명하면 갈림길 결정]
  → LTL 모델 검사로 P4 시간적 성질 검증                                        [신규 축; (P1) 선택적 rl 모드(= Kripke 전이, 유일한 하드 블로커) → (P2) modelCheck 배선(holds_R을 원자명제로) → header validity `[] ~ readInvalid` → parser/progress/preservation → (P3) 보조 search 일반화]
  → SCC (sufficient completeness)                                             [P1 도메인 협소화 완료(2026-07-14, predicate_domains = 쓰이는 주어들의 join 고정점; 가족의 86%가 진짜 sort, match_typeIR_BOOL_0이 위양성 반례 → COMPLETE). 남은 건 (P2) --slice-dir 배치 덤프(심볼당 50초 재번역 → 24시간이라 스윕 전 필수) → 전체 스윕(exact는 COMPLETE 수확, approx는 COUNTEREXAMPLE만; dom: 열로 1차 분류) → (P3) sub_nat의 Val 도메인]
  → 잔여 MAYBE: rhs-2회-사용 출력 바인더($un_op의 $bneg 케이스 — fold 중복-방지 게이트의 몫) + 대형 variant(>16멤버) subty 가드 + 전체-시스템급 슬라이스 [B′ 범위 밖; 필요 시 별건 설계]
                (companion-destructure 케이스는 위 2026-07-11 항목에서 해소)

  (완료: sanitize 밑줄 소실로 인한 심볼 합쳐짐 — `_`를 이름 문자로(위 2026-08-03 항목)
         $write_value_from_bits' n_var=0 경계 — 스펙에 `$(n_var > 0)` 가드 복원(아래 2026-07-28 항목)
         CTRS(구조적) differential — binary 수 인코딩 전환 후 Phase D 1227/1227 MATCH, 92618dc2
         termination 열 채우기 — 153심볼 CRC+term 스윕, verification.md
         owise 절 생성자 fan-out(complement 열거) — $join_ctk/$assignop_as_binop CRC MAYBE 2건
         해소(둘 다 YES), $join_flow 회귀 없음, 2026-07-16 상세는 위 M1 블록)
```

## CRC 정규화 패스 (2026-07-21): inline + unravel + prune, unravel은 reflect-only/upgrade-only

CRC MAYBE의 원인 = **determinacy critical pair**: 조건 `$f(A)=v`를 CRC가 fresh-변수
joinability로 인코딩하나 `$f`의 결정성을 몰라 self-overlap `#v#=v`를 못 닫는다. 세 레버:

1. **inline** (`fold_premise_binders ~aggressive:true`, [rewrite_system.ml] Pass 1 — done):
   single-var binder `$f(A)=v`를 use-count 무관 인라인. **등식(equivalence)** — preserve와
   reflect 양방향, 안전. `--crc-normalize` 플래그. e2e 검증: bin_concat TIMEOUT→YES,
   write_value MAYBE(5쌍, tuple binder 잔존).
2. **unravel** (tuple binder `$f(A)=tuple(v,b)`를 U-체인으로): **confluence-reflecting,
   NOT preserving** (Marchiori 1996; Nishida-Sakai-Sakabe LMCS 2012; Gmeiner-Gramlich-
   Schernhammer RTA 2010). 즉 `R` confl ⇏ `U(R)` confl (초기 join_text에서 YES→MAYBE로
   관측됐으나, 이는 값-destructure over-unravel이었고 subject 게이트 `1dd1e43a`로 교정 —
   아래 "over-unravel 교정" 참조; 이론상 non-preservation은 여전히 가능), 하지만
   soundness(좌선형 등) 하에 `U(R)` confl ⇒
   `R` confl. **그래서 upgrade-only로만 사용**: `U(R)`이 YES면 `R`을 YES로 승격,
   `U(R)`이 MAYBE/TIMEOUT이면 **원본 verdict 유지**(절대 하향 안 함). 형제 충돌을 줄이려
   **생성자-disjoint entry**에 우선 적용.
3. **prune** (`prune_slice_signature.py full`): 안 쓰는 시그니처 제거, tractability 회수.
   rules 불변이라 CRC 결론 보존. 기존 Python 후처리 유지(OCaml 재구현 불필요).

**건전성 방향 요약**: inline은 등식이라 blanket 가능; unravel은 reflect-only라 upgrade-only
필수. termination은 unravel이 깔끔히 transfer되지만(우리 MTT가 그 용도), confluence는
reflection만 성립 — 그래서 CRC에서 신중.

**구현 상태 (done)**: `--crc-normalize` = inline(Pass 1) + `crc_unravel` + `order_conds`
([rewrite_system.ml], new-rewrite `d109f982`). unravel의 `decompose`는 **App-패턴 binder만** 풀고
bare-var binder는 조건으로 남긴다(uses=0 self-pair가 spurious crcu overlap 되는 것 방지).
prune은 기존 Python 후처리 유지.

**crcu/crck sort**: 처음엔 `MS.signature` 미지-심볼 fallback으로 `Val…→Val`(all-Val)로 방출 —
**건전**(sort 확장 = overlap 추가 → 거짓 YES 불가)하나 체인-스텝 패턴 변수가 Val로 넓어져
spurious overlap을 만들 수 있다(set_priorities all-Val TIMEOUT 실측). 이후 [to_mfe.ml]에서
**실제 sort 복원**(`crc_sigs`): `crcu<id> : <subject sort> CrcKeep -> <함수 결과 sort>`,
`crck<id> : <carried 변수 sort…> -> CrcKeep`. carried 변수는 producer 규칙(head=원래 `$f`라
hint/arg-sort 유효)에서 타입되고, 다단계 체인은 id 오름차순(=레벨 순) 점진 처리로 의존성 해소
(작은 id의 crcu/crck가 먼저 실제 sort를 얻어 다음 레벨 producer 타이핑에 쓰임). crcu/crck 없으면
완전 inert(byte-identical). 이 narrowing은 나머지 order-sorted 인코딩과 동일하게 건전 —
carried 변수가 binding-site의 실제 sort를 유지하므로 real 임계쌍 보존, spurious만 제거.

**결과**: 5 write_value MAYBE→YES, bin_concat TIMEOUT→YES. 회귀: bin_bor/un_op YES 유지.
set_priorities: all-Val TIMEOUT → **real-sort YES 0쌍 78s**(Python 프로토타입 동일 시간).
sweep이 upgrade-only 채택 담당.

**over-unravel 교정 (new-rewrite `1dd1e43a`)**: `crc_unravel`이 조건 `s = tp`의 **패턴 `tp`만**
보고 unravel해서, subject `s`가 **변수**인 값-destructure(`text = cons(..)`)까지 풀었다. 그런데
determinacy CCP는 subject가 **함수 호출**일 때만 생긴다 — 변수-destructure는 CRC가 통일로 잘
처리하고, `Reflect.hoist_matchers`가 `match_K(v)=true`를 바로 그 destructure로 respell하는 이유가
그것이다(CRC가 볼 수 있게). 값-destructure를 unravel하면 destructure가 crcu consumer로 떨어져
나가고 형제 overlap엔 opaque `match-cons`만 남아, CRC가 base 절의 `len=bone`(⟹ nil)과 못 엮어
모순을 못 본다 — 이게 join_text YES→MAYBE의 진짜 원인이었다(reflect-only의 예가 아니라 고칠 수
있는 버그). `decompose`를 **subject가 정의 함수일 때만** unravel하도록 게이트: join_text **YES
0쌍 6s** 회복, write_value(5)/set_priorities(함수-subject) 무영향. unravel은 이론상 여전히
reflect-only라 upgrade-only는 안전망으로 유지.
