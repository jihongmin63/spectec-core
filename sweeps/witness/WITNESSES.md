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
