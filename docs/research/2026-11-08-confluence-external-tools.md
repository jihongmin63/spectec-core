# 외부 합류성 도구와 unsorted 번역 — 조사 노트

- 작성: 2026-11-08 · 측정 트리: `601ad938` (`new-rewrite`)
- 범위: 합류성 검사기 범용화(범위 B)를 논의하기 위한 사실·측정 자료. 설계 결정은 담지 않는다.
- 측정 스크립트: [2026-11-08-injection-cost.py](2026-11-08-injection-cost.py)

## 요약

1. 11월 초의 검사기 범용화(`78634ca3`, `2ffa852d`)는 **종료성 전용**이다.
   `Solver.problem`은 plain TRS이고, 등록된 어댑터 6개(AProVE, NaTT, MU-TERM, NTI, Mint,
   NaTT++)는 모두 종료성 증명기다. 합류성은 여전히 MFE의 CRC 하나에 고정돼 있다.
2. MFE를 택한 이유는 두 가지였다. 그중 coherence 검사는 표면에서 `rl`이 사라지면서 의미를
   잃었고, **order-sorted 조건부 시스템을 그대로 받는다는 이유 하나만 남았다.**
3. 종료성처럼 "unravel → plain TRS → 외부 도구"로 합류성을 검사할 수는 없다. unraveling은
   합류성을 반사(reflect)할 뿐 보존하지 않고, sort를 지우면 가짜 임계쌍이 생기며 YES의
   전이도 보장되지 않는다.
4. sort 정보를 항 구조로 옮기는 충실한 번역(injection 인코딩)이 있으면 sort 쪽 문제는
   해결된다. 조건부 문제, 도구 운용 문제, 규모 문제는 그대로 남는다.
5. 그렇게 번역한 결과를 받을 도구는 unsorted CTRS 합류성 도구로 한정된다. 현재 실질적인
   후보는 **CONFident와 CO3 둘(넓게 잡아 ACP까지 셋)**이고, 로컬 설치가 확인된 것은 ACP뿐이다.
6. P4 분석 표면 전체를 기준으로 injection 인코딩의 비용은 규칙 약 3.0배, injection 위치는
   항 노드의 28%, sort-decreasing 위반 116건이다. 한편 sort를 지우면 매칭이 달라지는
   규칙은 **6.0%뿐**이다.

## 1. 합류성 검사에 MFE를 쓰게 된 경위

| 시점 | 내용 | 근거 |
|---|---|---|
| 06-06 ~ 06-23 | 첫 합류성 검사는 웹 서비스 CoCoWeb에 COPS 텍스트를 올리는 방식이었다. 첫 측정에서 impty/base 8심볼이 모두 TIMEOUT | `49058567`, [RESEARCH_NOTES.md](../../RESEARCH_NOTES.md) 단계 O1·O2 |
| 06-26 | CoCoWeb을 MFE(CRC + ChC)로 교체 | `38cac10f` |
| 07-01 | 단일 sort `[Term]` 표면을 order-sorted 표면으로 교체 | `97b8c4ee` |
| 07-14 | 모든 관계가 입력-모드라 `rl`을 가르던 `rule_heads` 배선 삭제 | [CORE_LOGIC.md](../../spectec/lib/rewrite/CORE_LOGIC.md) §6.5 |
| 07-24 | ChC 측정 중단: 판정이 CRC와 항상 같았다 | [CLAUDE.md](../../CLAUDE.md) MFE 절 |
| 07-27 | 실험 ⑤ 파일럿: 무sort 도구군과 비교 | [PAPER.md](../../PAPER.md) 실험 ⑤ |

결정에 쓰인 이유는 다음 두 가지였다.

- **Maude 자체의 전제를 검사한다** (`38cac10f`). Maude는 `eq`가 Church-Rosser이고 `rl`이
  `eq`와 coherent하다고 가정만 하고, 실행 중에는 검사하지 않는다. 당시 표면에는 `eq`와
  `rl`이 함께 있었다. coherence는 Maude 고유의 개념이라 CoCo 계열 도구로는 물을 수 없다.
  **07-14 이후 `rl`이 없으므로 이 이유는 소멸했다.**
- **order-sorted 조건부 시스템을 그대로 받는다** (`97b8c4ee`). 단일 sort 표면에서는 IL
  타입이 지워져 가짜 임계쌍이 생겼다. PAPER.md는 이를 "order-sorted와 조건부를 동시에 받는
  검사기는 사실상 하나뿐"이라고 정리한다.

이유라기보다 궁합에 가까운 점도 하나 있다. CRC가 요구하는 조건(built-in 금지, owise 금지,
조작적 종료성은 별도로 증명)이 분석 표면이 이미 갖춘 형태와 일치한다(Durán–Meseguer,
WRLA 2010).

## 2. 종료성 경로를 합류성에 그대로 쓸 수 없는 이유

### 2.1 방향이 반대다

| | 종료성 | 합류성 |
|---|---|---|
| 변환 결과에서 원본으로 | TRS 종료 ⟹ CTRS 조작적 종료 (항상 성립) | U(R) 합류 ⟹ R 합류 (전제가 있을 때만) |
| 변환의 성격 | 과대근사. 항상 건전 | 반사만 함. 보존하지 않음 |
| NO/MAYBE의 의미 | 근사의 부산물 | 아무것도 말하지 않음 |
| sort 소거 | 안전한 방향 | 가짜 임계쌍이 생기고, YES 전이도 조건부(§3) |

### 2.2 unraveling의 전제

- YES 전이 정리(Gmeiner–Nishida–Gramlich, IWC 2013, 정리 9)는 **약좌선형(WLL) DCTRS와
  공유 키잉 unraveling**을 전제로 한다.
- 당시 분석 표면 전체(74,945규칙)를 기계 검사한 결과 WLL 위반이 273건 있었다
  ([RESEARCH_NOTES.md](../../RESEARCH_NOTES.md)). 이미 바인딩된 변수를 조건의 패턴 자리에
  다시 쓰는 동치 검사 인코딩 때문이다.
- 실측: 합류하는 `$join_text`가 unravel 후 무sort 도구에서 **NO**를 받았다
  ([PAPER.md](../../PAPER.md) §4).

현재 `confluence` 명령의 정규화 사다리(`crc_unravel`)도 같은 이유로 YES 승격 전용이다.

### 2.3 sort 소거의 비용

실험 ⑤ 파일럿(2026-07-27): 13규칙 `$join_text`를 MFE는 0.7초 만에 YES로 닫았지만, 무sort
도구 4개는 모두 실패했다. 같은 파일럿에서 무sort 도구 하나가 쓰기 계열 심볼 하나를 YES로
닫기도 했다.

주의할 점이 둘 있다.
- 파일럿의 원자료(로그)는 저장소에 없다.
- 닫힌 심볼이 `$write_value_from_bits`인지 `$write_bits_from_value`인지가 PAPER.md 부록
  A.6에 미확정으로 남아 있다.

### 2.4 운용

- CoCoWeb: 2026-11-04 기준 접속 불가.
- 후속 서비스 ARIWeb: timeout 상한 60초(11-04 확인).
- 우리 슬라이스는 이보다 훨씬 오래 걸린다(`$write_value_from_bits` 261.2초,
  `$set_priorities` 78초).

## 3. sort 소거와 YES 전이

PAPER.md 실험 ⑤는 "무sort YES는 sorted로 내려가 전이되므로 유효한 증명"이라고 서술한다.
이 서술은 **order-sorted에서는 일반적으로 성립하지 않는다.** 반례는 다음과 같다.

```
sorts Nat Bool < Val     a, c : -> Val   b : -> Bool   f : Val -> Val
a = f(b)     a = c     f(X:Nat) = c
```

- **sort를 쓰면:** `b`가 `X:Nat`에 매칭되지 않으므로 `f(b)`는 정규형이다. `a`의 정규형이
  `f(b)`와 `c` 둘이 되어 **합류하지 않는다.**
- **sort를 지우면:** `f(b) → c`가 되므로 **합류한다.**

다정렬(many-sorted) TRS에서는 합류성이 sort 소거에 대해 persistent하다(Aoto–Toyama 1997).
하지만 서브소트로 변수를 좁히는 경우에는 이 결과가 적용되지 않는다.

우리 표면은 실제로 좁은 변수 sort를 쓴다. [maude_sorts.ml:564-567](../../spectec/lib/rewrite/maude/maude_sorts.ml#L564-L567)은
IL에 선언된 좁은 sort를 일부러 써서 "관계 입력의 형제 규칙끼리 겹치지 않게" 한다. §5의
측정으로 보면 그런 변수(좁은 binder)를 가진 규칙이 2,310개(6.0%)다. 따라서 실험 ⑤의 "무sort
YES는 유효" 서술은, 해당 슬라이스에 좁은 binder가 없다는 확인이 따라야 성립한다.

## 4. 충실한 unsorted 번역

### 4.1 두 가지 인코딩

**A. sort 술어를 조건으로 붙인다 (MTT 방식).** `f(X:Nat)`를 `f(X) | isNat(X) == true`로 바꾼다.
- 무조건 규칙까지 모두 조건부가 된다. TRS에 강한 도구에 불리하다.
- 가짜 임계쌍이 없어지지 않는다. 조건이 실현 불가능한 임계쌍으로 남고, 이를 지울 수
  있는지는 도구의 infeasibility 검사에 달려 있다.
- Maude는 정규형이 아닌 항에도 선언된 결과 sort를 준다. 생성자 항만 인식하는 술어로는
  이 의미가 어긋날 수 있다.

**B. injection 기호로 many-sorted 시스템을 만든다.** `Nat < Val`을 기호 `inj_Nat_Val(t)`로 바꾼다.
- `f(inj_Nat_Val(X))`와 `f(inj_Bool_Val(Y))`는 기호가 달라서 임계쌍이 처음부터 생기지 않는다.
- 무조건 규칙은 무조건으로 남는다.
- **YES 전이가 구성상 성립한다.** many-sorted에는 오버로딩도 서브소트도 없으므로 위치가
  sort를 결정한다. 그래서 무sort 관계를 well-sorted 항으로 제한하면 many-sorted 관계와
  일치하고, well-sorted 항의 집합은 재작성해도 그 안에 머문다. 따라서 무sort에서 모든 항에
  대해 CR이면 many-sorted에서도 CR이다.
- NO까지 옮기려면 반대 방향의 persistence가 필요하다. TRS에는 Aoto–Toyama가 있지만
  조건부로 확장되는지는 확인하지 못했다. YES만 받아들이는 용도라면 필요 없다.

### 4.2 B로 해결되는 것과 남는 것

| 문제 | B로 |
|---|---|
| 무sort YES의 전이 (§3) | ✅ 구성상 해결 |
| sort 소거 비용(가짜 임계쌍) | ✅ 해결 |
| order-sorted 때문에 MFE만 쓸 수 있던 제약 | ✅ 사라짐 |
| unraveling이 반사만 함, WLL 위반 (§2.2) | ❌ 조건의 문제다. CTRS를 직접 받는 도구에 넘기면 변환의 건전성은 도구가 책임지지만, MAYBE 가능성은 그대로다 |
| 도구 운용 (§2.4, §6) | ❌ 무관 |
| 규모 | ❌ 무관. 인코딩으로 규칙이 오히려 늘어난다 |

### 4.3 B 없이도 sort 소거가 충실한 경우

다음 세 조건을 갖춘 슬라이스는 sort를 그냥 지워도 충실하다.

1. 좁은 binder가 없다.
2. 모든 규칙이 sort-decreasing이다.
3. 오버로딩이 없다. P4 표면 전체에서 0으로 확인했다.

well-sorted 항에서는 각 위치가 그 자리의 sort를 보장하므로, 좁은 binder가 없으면 sorted
매칭과 unsorted 매칭이 같다. sort-decreasing이면 재작성해도 well-sorted로 남는다. 이
조건을 만족하는 슬라이스에서는 §3의 반례가 생길 수 없고, 무sort 도구의 YES가 그대로
유효하다. 즉 B는 좁은 binder가 있는 슬라이스에만 필요하다.

## 5. B의 비용 실측

**대상:** `main.exe rewrite --ctrs`가 `specs/p4` 전체에 대해 내는 분석 모듈(43,416줄, 방정식
38,691개). 생성에 12.6초, 최대 RSS 258MB. 슬라이스별 수치가 아니라 전체 시스템 기준이다.

**집계 방법:**
- 좁은 binder: 변수의 첫 패턴 출현(lhs 또는 조건의 패턴 쪽)에서, 선언 sort가 그 위치의
  sort보다 엄격히 아래인 경우.
- 복제 추정: 연산자의 결과 sort인 모든 sort를 항의 최소 sort 후보로 본다. 임의의 항
  기준이므로 도달 가능한 항만 보면 더 작아질 수 있다.

| 항목 | 값 |
|---|---|
| sort / 선언된 서브소트 간선 | 494 / 1,410 |
| 부모(Val 제외)가 둘 이상인 sort | 152 (예: `BinaryExpression`은 상위 7개) |
| 연산자 오버로딩 | 0 |
| 좁은 binder가 있는 규칙 | **2,310 (6.0%)** |
| 복제, 쌍마다 injection 하나 | 2,254,923개 (×58.3), 한 규칙 최대 825,792 |
| 복제, 격자 경로를 따르는 injection | **116,644개 (×3.0)**. 37,935규칙 그대로, 1,000배 이상 9규칙, 최대 8,000 |
| injection 삽입 위치 | 106,931 / 항 노드 377,847 (28.3%) |
| sort-decreasing 위반 | 116 (0.3%) |

해석:
- **쌍마다 injection을 두는 방식은 비현실적이다.** 좁은 binder `X:S`가 S 아래의 모든
  최소 sort마다 패턴을 하나씩 요구해서 곱이 폭증한다.
- **격자 경로를 따르면 ×3.0까지 줄어든다.** 항이 injection 경로를 그대로 지니므로 변수가
  자기 sort에서 경로를 끊고 잡을 수 있고, 다중 부모 sort에서 갈라지는 경우만 복제된다.
  대가로 항이 경로 길이만큼 더 깊어진다.
- **복제가 큰 규칙은 소수에 몰려 있다.** 격자 경로 방식에서 1,000배 이상인 9규칙의 머리는
  `$lift-ternaryExpressionIR`·`$lift-sliceAccessExpressionIR`(각 8,000),
  `InstDecl-*-ok`(4,830), `$lift-conditionalStatementIR`(2,880),
  `$eBPF-setup-*-callee`(2,645), `$lift-typedLvalueIR`(1,600)다. 이 규칙들이 속한 슬라이스의
  현재 MFE 판정은 확인하지 않았다.
- **sort-decreasing 위반 116건**은 표본으로 확인해 보니 결과 sort가 `Val`로 복구된
  합성·단형화·prelude 심볼(`$union-set-typeId`, `$unions-set-typeId`, `mul` 등)이 우변에
  오는 경우였다. 진짜 sort 증가가 아니라 sort 복구의 정밀도 문제로 보인다. 인코딩 전에
  복구를 고치거나 retract 기호를 둬야 한다.

재현:

```bash
SPEC=$(find spectec/specs/p4 -name '*.spectec' | sort)
_build/default/spectec/bin/main.exe rewrite --ctrs $SPEC > p4.ctrs.maude
python3 -I docs/research/2026-11-08-injection-cost.py p4.ctrs.maude
```

## 6. unsorted CTRS 합류성 도구 현황 (2026-11-08)

### 6.1 형식

ARI 형식(CoCo 2024부터 COPS를 대체)은 TRS, CTRS, MSTRS, LCTRS, CSTRS, CSCTRS와 파생
형식(commutation, INF)으로 이루어진다.
- **CTRS:** sort 선언 없이 `oriented | join | semi-equational` 중 하나를 고른다. 우변의
  추가 변수가 허용된다.
- **MSTRS:** 규칙이 무조건뿐이고 서브소트가 없다.
- **sort와 조건을 함께 받는 형식은 없다.** B의 결과도 sort를 지운 CTRS로 넘겨야 한다.

### 6.2 도구

CoCo 2026 CTRS 부문의 참가 도구는 CO3와 CONFident 둘이었다(100문제, 문제당 60초).

| 도구 | 최근 CTRS 부문 | 지원 범위·방법 | 로컬 실행 |
|---|---|---|---|
| **CONFident** | 2026: YES 49 / NO 21 / MAYBE 1 / TIMEOUT 29 | TRS, CS-TRS, CTRS, CS-CTRS, GTRS. Haskell 구현. MU-TERM, infChecker, AGES, Prover9/Mace4, Barcelogic에 의존. 2025년부터 ARI 형식을 직접 받음 | ❓ 홈페이지(zenon.dsic.upv.es/confident)가 연결 거부(11-08) |
| **CO3** v2.7 | 2026: YES 35 / NO 22 / MAYBE 43 / TIMEOUT 0 | WLL 3-DCTRS를 Uconf로 unravel한 뒤 TRS 합류성을 검사. join·semi-equational은 DCTRS로 변환. 외부 도구에 의존하지 않음. **우리 `crc_unravel` 사다리와 같은 정리**를 씀 | ❌ 홈페이지에 다운로드가 없고 v2.0(2019)으로 표기 |
| **ACP** v0.74 | 2025 3위, 2026 미참가 | TRS가 주력이고 oriented CTRS도 다룸. SML/NJ 구현, MiniSAT·YICES 필요 | ✅ 소스 tarball 공개 |
| ConCon | 2020이 마지막 | — | 유지 여부 불명 |

### 6.3 대상에서 빠지는 것

- **FORT-h:** 바닥항 합류성(GCR) 도구로, 무조건 TRS와 MSTRS만 받는다.
- **crest, CRaris (LCTRS):** 조건이 SMT 제약이라 재작성 조건이 아니다.
- **infChecker, nonreach, Natto (INF):** 실현 불가능성만 판정한다. 인코딩 A를 택할 때
  부품으로만 쓸모가 있다.
- **CSI, Hakusan 등 (TRS):** unraveling을 거쳐야 하므로 §2.2의 제약을 그대로 받는다.

### 6.4 공통 조건

- 대회 조건은 문제당 60초, 메모리 128GB이고, 문제는 교과서 크기다.
- ARIWeb은 웹 실행기일 뿐 바이너리를 배포하지 않는다. CoCo 2026 페이지에 "참가 도구의
  바이너리를 ARIWeb으로 공개한다"는 문장이 있지만, 실제 ARIWeb 페이지에 다운로드는 없다.

## 7. 열린 질문

- **슬라이스 분류.** ≤500 슬라이스 중 §4.3의 조건(좁은 binder 0, sort-decreasing)을 만족하는
  것이 몇 개인가. 만족하는 슬라이스는 B 없이 무sort 도구에 바로 넘길 수 있다.
- **PAPER.md 실험 ⑤의 서술.** "무sort YES는 유효"를 §3의 조건부 서술로 고칠지.
  파일럿에서 닫힌 슬라이스가 좁은 binder를 가지는지부터 확인해야 한다.
- **도구 입수 경로.** CONFident는 홈페이지가 응답할 때 다시 확인하고, CO3 바이너리는 저자
  문의 외에 경로가 보이지 않는다. ACP는 소스 빌드가 가능하다.
- **CO3의 추가 가치.** 우리 정규화 사다리와 같은 정리를 쓰므로 더해 주는 것이 적어 보인다.
  다만 이는 추론이며, 실측으로 확인하지 않았다.
- **sort 복구 정밀도.** sort-decreasing 위반 116건을 B와 상관없이 고칠지. MFE CRC의
  sort-decreasingness 의무에도 영향이 있을 수 있다.
- **조건부 persistence.** 무sort NO를 sorted NO로 옮기는 정리가 CTRS에 있는지는 문헌을
  확인하지 못했다.
- **11-04 조사 문서.** `docs/research/2026-11-04-verification-tool-survey.md`는 11-08 현재
  디렉터리에 없다.

## 출처

저장소: `49058567`, `38cac10f`, `97b8c4ee`, `78634ca3`, `2ffa852d`;
[RESEARCH_NOTES.md](../../RESEARCH_NOTES.md), [PAPER.md](../../PAPER.md),
[CORE_LOGIC.md](../../spectec/lib/rewrite/CORE_LOGIC.md),
[maude_sorts.ml](../../spectec/lib/rewrite/maude/maude_sorts.ml),
[solver.mli](../../spectec/lib/rewrite/solver.mli).

웹 (2026-11-08 열람):
- [CoCo 2026 results](https://project-coco.uibk.ac.at/2026/results.php)
- [CoCo 2025 results](https://project-coco.uibk.ac.at/2025/results.php)
- [CoCo 2026 CTRS detail](https://ari-cops.uibk.ac.at/CoCo/2026/competition/CTRS/)
- [ARI format](https://project-coco.uibk.ac.at/ARI/)
- [ARI CTRS](https://project-coco.uibk.ac.at/ARI/ctrs.php)
- [ARI MSTRS](https://project-coco.uibk.ac.at/ARI/mstrs.php)
- [CONFident CoCo 2026](https://project-coco.uibk.ac.at/2026/participants/papers/CONFident.pdf)
- [CONFident CoCo 2025](https://project-coco.uibk.ac.at/2025/participants/papers/CONFident.pdf)
- [CO3 CoCo 2026](https://project-coco.uibk.ac.at/2026/participants/papers/CO3.pdf)
- [CO3 home](http://www.lctrs.jp/tools/co3/)
- [ACP CoCo 2026](https://project-coco.uibk.ac.at/2026/participants/papers/ACP.pdf)
- [ACP downloads](http://www.nue.ie.niigata-u.ac.jp/tools/acp/)
- [ARIWeb](https://ari-web.uibk.ac.at/)

문헌: Gmeiner–Nishida–Gramlich, IWC 2013; Aoto–Toyama, "Persistency of confluence", J.UCS
1997; Durán–Meseguer, WRLA 2010.
