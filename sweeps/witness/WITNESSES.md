# `holds_` or-gate 루프의 AProVE 증인 (2026-08-10)

`main.exe termination`이 낸 NO 23건의 원인. 재현:

```bash
SPEC=$(find spectec/specs/p4 -name '*.spectec' | sort | tr '\n' ' ')
main.exe termination --symbol SYM --emit-trs $SPEC > SYM.trs
spectec/tools/aprove/runme SYM.trs 120 > SYM.out   # 서사까지 전부 받는다
```

`Aprove.check`의 `done_when`은 판정 헤더에서 끝나므로(`bae6e096`) 스윕은 서사를
남기지 않는다. 루프는 서사 안에 있으니 여기서는 드라이버를 직접 부른다.

## ⑴ 자기루프 — `ParameterType_alpha` (2,489규칙, 11.1초)

```
(45) NonLoopProof (COMPLETE)
By Theorem 8 [NONLOOP] we deduce infiniteness of the QDP.
We apply the theorem with m = 1, b = 0, 
?' = [ ], and ?' = [x1 / proj_variant_typedefTypeIR_TYPEDEF_2_1(x1)] on the rule
HOLDS_TYPE_ALPHA(x0, proj_variant_typedefTypeIR_TYPEDEF_2_1(x1))[ ]^n[ ] -> HOLDS_TYPE_ALPHA(x0, proj_variant_typedefTypeIR_TYPEDEF_2_1(x1))[ ]^n[x1 / proj_variant_typedefTypeIR_TYPEDEF_2_1(x1)]
This rule is correct for the QDP as the following derivation shows:

HOLDS_TYPE_ALPHA(x0, proj_variant_typedefTypeIR_TYPEDEF_2_1(x1))[ ]^n[ ] -> HOLDS_TYPE_ALPHA(x0, proj_variant_typedefTypeIR_TYPEDEF_2_1(x1))[ ]^n[x1 / proj_variant_typedefTypeIR_TYPEDEF_2_1(x1)]
    by Equivalency by Simplifying Mu with mu1: [x1 / proj_variant_typedefTypeIR_TYPEDEF_2_1(x1)] mu2: [ ]
    intermediate steps: Instantiate mu
    HOLDS_TYPE_ALPHA(x0, x1)[ ]^n[ ] -> HOLDS_TYPE_ALPHA(x0, proj_variant_typedefTypeIR_TYPEDEF_2_1(x1))[ ]^n[ ]
        by Rule from TRS P
```

`gen_rel_holds`가 내던 맨변수 lhs 하나(`holds_R(x0..xn-1) = or(g_1..g_k)`)에,
가드가 형제의 부분항을 `proj_K_i(subject)`로 되찾으면서 인자가 줄지 않는다.

## ⑵ 상호루프 — `$merge_constraint_prime` (3,333규칙, 21.1초)

자기재귀 판정만으로 ⑴을 고친 뒤에도 남았던 9건의 원인. 어느 규칙도 자기 자신을
언급하지 않으므로 자기재귀 검사에 걸리지 않는다.

```
(263) NonLoopProof (COMPLETE)
By Theorem 8 [NONLOOP] we deduce infiniteness of the QDP.
We apply the theorem with m = 1, b = 0, 
?' = [ ], and ?' = [x0 / proj_variant_serializableEnumTypeIR_ENUM__lt___gt___lbrace___rbrace_3_1(d_unroll_typeIR(x0)), x1 / d_unroll_typeIR(x1)] on the rule
HOLDS_CAST_IMPL(proj_variant_serializableEnumTypeIR_ENUM__lt___gt___lbrace___rbrace_3_1(d_unroll_typeIR(x0)), d_unroll_typeIR(x1))[ ]^n[ ] -> HOLDS_CAST_IMPL(proj_variant_serializableEnumTypeIR_ENUM__lt___gt___lbrace___rbrace_3_1(d_unroll_typeIR(x0)), d_unroll_typeIR(x1))[ ]^n[x0 / proj_variant_serializableEnumTypeIR_ENUM__lt___gt___lbrace___rbrace_3_1(d_unroll_typeIR(x0)), x1 / d_unroll_typeIR(x1)]
This rule is correct for the QDP as the following derivation shows:

HOLDS_CAST_IMPL(proj_variant_serializableEnumTypeIR_ENUM__lt___gt___lbrace___rbrace_3_1(d_unroll_typeIR(x0)), d_unroll_typeIR(x1))[ ]^n[ ] -> HOLDS_CAST_IMPL(proj_variant_serializableEnumTypeIR_ENUM__lt___gt___lbrace___rbrace_3_1(d_unroll_typeIR(x0)), d_unroll_typeIR(x1))[ ]^n[x0 / proj_variant_serializableEnumTypeIR_ENUM__lt___gt___lbrace___rbrace_3_1(d_unroll_typeIR(x0)), x1 / d_unroll_typeIR(x1)]
    by Equivalency by Simplifying Mu with mu1: [x0 / proj_variant_serializableEnumTypeIR_ENUM__lt___gt___lbrace___rbrace_3_1(d_unroll_typeIR(x0)), x1 / d_unroll_typeIR(x1)] mu2: [ ]
    intermediate steps: Instantiate mu
    HOLDS_CAST_IMPL(x0, x1)[ ]^n[ ] -> HOLDS_CAST_IMPL(proj_variant_serializableEnumTypeIR_ENUM__lt___gt___lbrace___rbrace_3_1(d_unroll_typeIR(x0)), d_unroll_typeIR(x1))[ ]^n[ ]
        by Narrowing at position: []
        HOLDS_CAST_IMPL(x0, x1)[ ]^n[ ] -> HOLDS_CAST_IMPL_NEQ(d_unroll_typeIR(x0), d_unroll_typeIR(x1))[ ]^n[ ]
            by Rule from TRS P

        intermediate steps: Instantiation - Instantiation - Instantiation - Instantiation
        HOLDS_CAST_IMPL_NEQ(x0, x1)[ ]^n[ ] -> HOLDS_CAST_IMPL(proj_variant_serializableEnumTypeIR_ENUM__lt___gt___lbrace___rbrace_3_1(x0), x1)[ ]^n[ ]
            by Rule from TRS P
----------------------------------------
```

# 잔여 TIMEOUT의 실루프 증인 2종 (2026-08-12)

650 재스윕(환경 회복 후)의 잔여 TIMEOUT 233건을 분류하다 얻었다. 이번 증인은
AProVE가 아니라 **TRS 자신의 규칙만으로 하는 innermost 정규화 시뮬레이션**이다
([simulate_w0.py](simulate_w0.py) — 유도의 모든 산술·가드가 슬라이스의 규칙으로
수렴함을 기계적으로 확인):

```bash
main.exe termination --symbol '$bin_ge' --emit-trs $SPEC > bin_ge.trs
python3 simulate_w0.py bin_ge.trs
```

## ⑶ w=0 진동 — `d_bitstr_to_int` (todo.md 2026-07-09 미해결 이슈의 첫 기계적 증인)

```
w=0:  2^w = int_pos(bone),  half = int_pos(bzero),  -half = int_pos(bzero)
n = int_pos(bzero)
  chain1 발화 (half<=n = true): n := n - 2^w
n = int_neg(bzero)
  chain2 발화 (half<=n = false, -half<=n = false): n := n + 2^w
n = int_pos(bzero)
** 순환 확인: n이 0번째 상태로 복귀 — 무한 유도 존재 **
(TRS 규칙 적용 횟수: 56)
```

`d_bitstr_to_int(int_pos(bzero), int_pos(bzero))`에서 chain1(`half≤n`→`n−2^w`)과
chain2(둘 다 거짓→`n+2^w`)가 `0 → −1 → 0`으로 영원히 진동한다. **잘 정렬된
항이므로 sorts 드롭 과대근사와 무관한, CTRS(그리고 numerics.ml)의 진짜 w=0
비종료다.** 남은 물음은 도달성뿐(P4 타입계가 `bit<0>` 산술을 막는가 — todo.md
규명 항목).

**폭발 반경**: 잔여 TIMEOUT 233 중 **157개 슬라이스**(소형 26 + 대형 131,
`sweeps/decode_membership.tsv`)가 이 decode 규칙을 폐포에 품는다 — `Program_ok`
포함. **이들의 SN은 거짓이므로 어떤 예산으로도 YES가 불가능하다.** 12시간
`Program_ok` 런이 TIMEOUT이었던 것은 예산 부족이 아니라 목표 불능이었다.

## ⑷ 빈-패턴 루프 — `$strip_prefix_rec` / `$strip_suffix_rec`

`prefix = ""`(nil)이면 가드와 스트립이 슬라이스 규칙으로 이렇게 정규화된다:

```
len(nil) = bzero
starts_with 가드 (leq ∧ eqg-slice) = true        -- leq(0,|t|) ∧ slice(t,0,0)=nil
strip_prefix(t, nil) = drop(t, 0) = t             -- 항등
∴ strip_prefix_rec(t, nil) → … → strip_prefix_rec(t, nil)   (길이 1 순환)
```

suffix 판도 동일(`ends_with`의 u_1 체인이 leq(0,|t|)=true, slice(t,|t|,0)=nil로
발화, `take(t,|t|) = t`). CLAUDE.md의 교훈("0-폭/0-값 경계를 항상 확인하라")의
텍스트판 — `$(n_var > 0)` 가드를 받은 write_value 가족과 같은 결이며, 스펙에
`|prefix| > 0` 가드(또는 호출부 불변식 문서화)가 필요하다. 대형 clean 61엔 이
루프가 없다(`sweeps/strip_membership.tsv`).

## ⑸ w=0 루프의 끝-대-끝 도달성 — **타입체커를 발산시키는 2줄 P4 프로그램** (2026-08-12)

⑶의 남은 물음("실제 프로그램이 w=0에 도달하는가")의 답은 **예**다.
[bit0_add.p4](bit0_add.p4):

```p4
const bit<0> A = 0;
const bit<0> B = A + A;
```

```bash
timeout 30 main.exe p4 typecheck -p bit0_add.p4 -i spectec/testdata/interp/p4/p4c/includes
# → 30초 킬 (발산). 대조군은 전부 정상:
```

| 프로그램 | 결과 | 읽기 |
|---|---|---|
| `const bit<1> B = A + A;` | succeeded | 하네스 정상, w=1 무결 |
| `const bit<0> A = 0;` (산술 없음) | succeeded | **bit<0>은 합법으로 수용됨** |
| `const bit<0> B = A + A;` | **HANG** | `$bin_plus`의 W 절이 `$bitstr_to_int(0,·)` 호출 → ⑶의 진동 |
| `const bit<0> B = -A;` | succeeded | `$un_minus` W 절은 decode를 안 부름 — hang이 decode 경로에 고정 |
| `const int<0> A = 0;` | 거부 | signed 문은 타입 정형성에서 닫힘 |

즉 **도달 가능한 실행 결함**이다: 스펙(3-operations.spectec의 W-절 decode)과
참조 구현(numerics.ml)이 공유하는 비종료를, bit<0> 상수 산술이 있는 합법(적어도
이 스펙이 수용하는) 프로그램이 밟는다. 고칠 곳의 선택지는 둘 — bit<0> 산술을
정형성에서 거부하거나, decode/encode를 w=0에서 전역화(bit<0>의 값은 0 하나뿐)
— 스펙 소유자의 결정 사안. (부수 관찰: `-A`는 성공하지만 `$un_minus(0 W 0) =
$(2^0 − 0) = 1`로 bit<0>에 표현 불가능한 값 1을 만들 혐의가 있다 — 별도 확인
필요.)
