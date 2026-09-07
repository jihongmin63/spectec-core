# Two defects in fixed-width arithmetic at the boundary

Both are reproduced below from nothing but a P4 source snippet and
`p4 typecheck`. *(On this tree the fix for ⑴ is already in the working
directory — revert `numerics.ml` and `builtin.ml` to see it hang.)*

```bash
EXE=spectec/_build/default/bin/main.exe
INC=spectec/testdata/interp/p4/p4c/includes
```

## ⑴ `bit<0>` arithmetic diverges — `$bitstr_to_int` loops at width 0

```bash
cat > /tmp/bit0.p4 <<'EOF'
const bit<0> A = 0;      // typechecks on its own -- P4-16 allows bit<0>
const bit<0> B = A + A;  // never returns
EOF
timeout 30 $EXE p4 typecheck -p /tmp/bit0.p4 -i $INC     # exit 124, 100% CPU
```

`$bitstr_to_int(w, n)` normalizes `n` into `[-2^(w-1), 2^(w-1))` by repeatedly
adding/subtracting `2^w`:

```ocaml
let rec bitstr_to_int' (w : Bigint.t) (n : Bigint.t) : Bigint.t =
  let two = Bigint.(one + one) in
  let w' = pow2' w in
  if Bigint.(n >= w' / two) then bitstr_to_int' w Bigint.(n - w')
  else if Bigint.(n < -(w' / two)) then bitstr_to_int' w Bigint.(n + w')
  else n
```

At `w = 0` that interval is `[0, 0)` — **empty**, base case unreachable, so
`n = 0` oscillates `0 → −1 → 0 → …`.

`bit<0>` reaches it because the unsigned (`W`) clauses decode as well, not just
the signed ones — `def $bin_plus(w W i_l, w W i_r)` opens with
`-- if i_l' = $bitstr_to_int(w, i_l)`. So do `< <= > >=`, `+ - *`, unary `-`
(S), `& ^ |`, `|+| |-|`, `<< >>`, `$nat_of_integerValue`,
`$int_of_integerValue`, and `$write_value*_from_bits` — every one of them is a
door to the same loop.

Beyond execution: this boundary makes termination of 157 of 650 per-symbol
rewrite slices (`Program_ok` included) *provably false*.

## ⑵ `-0` overflows `bit<w>` — `$un_minus` does not wrap

```bash
cat > /tmp/neg.p4 <<'EOF'
const bit<8> A = 0;
const bit<8> B = -A;
EOF
$EXE p4 typecheck -p /tmp/neg.p4 -i $INC \
  --trace.level full --trace.output /tmp/neg.trace     # succeeds
grep -m1 'w 256' /tmp/neg.trace                        # 8 w 256  <-- the value of B
```

`bit<8>` holds `0..255`, so `B` is out of range; real P4 negation is
`(2^w − i) mod 2^w`. The clause never reduces:

```
def $un_minus(w W i) = w W i'
  -- if i' = $($pow2(w) - i)
```

It is wrong for every `w`, not only `w = 0`, and no verdict catches it — only
the computed value is bad. `$un_minus`'s *signed* clause is fine (it decodes and
re-encodes). Likely fix: `i' = $int_to_bitstr(w, $($pow2(w) - i))`.

## Fix for ⑴ — totalize the decode at `w ≤ 0`

`bit<0>` holds exactly one value, `0`, so the decode has an answer at width 0;
the correction loop simply cannot reach it. Both implementations of the
definition now return it directly and never enter the loop.

`numerics.ml` — one guard ahead of the recursion, so `w ≥ 1` below it and
`w' / two ≥ 1` keeps the interval non-empty:

```ocaml
let rec bitstr_to_int' (w : Bigint.t) (n : Bigint.t) : Bigint.t =
  if Bigint.(w <= zero) then Bigint.zero
  else
    ...unchanged...
```

`builtin.ml` (the CTRS translation of the same builtin) has no `else`, so the
split takes two parts: a new base rule `w ≤ 0 → 0`, and a `0 < w` condition
added to the three existing clauses through their shared `common` prefix, which
keeps them disjoint from it:

```
ceq $bitstr-to-int(w, n) = int(0) if leq-int(w, int(0)) = bool(true) .
ceq $bitstr-to-int(w, n) = $bitstr-to-int(w, sub-int(n, pow2w))
      if lt-int(int(0), w) = bool(true) /\ ... .
```

The guard is arithmetic (`0 < w`, the prelude's `leq`) rather than `w =/= 0`:
`=/=` on an int translates to the generic structural `eq`, whose 50k-rule table
would join the dependency closure and blow the slice up.

Effect: the snippet in ⑴ typechecks, and nothing at `w > 0` moves — `bit<1>`
arithmetic still passes, `int<0>` is still rejected by well-formedness, decoding
`9` at `w = 4` still gives `-7`, and the impty golden is byte-identical.

The alternative is to reject `bit<0>` arithmetic in type well-formedness (as
`int<0>` already is) — a maintainer decision; totalizing matches P4-16 accepting
`bit<0>`. ⑵ is uncovered either way.
