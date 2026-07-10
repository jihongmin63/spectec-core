#!/usr/bin/env python3
# Modular rule-partition of a To_mfe SPEC .mod for termination analysis.
#
# Operates ONLY on the emitted analysis-slice .mod (a transient artifact produced
# by `rewrite --ctrs --symbol`) -- it NEVER touches the spec source, the OCaml
# prelude/builtin definitions, or the executable rewrite system. This is exactly
# the premise of the modular composition: the real system R keeps ALL rules; we
# only abstract R_arith to free constructors *inside the analysis module* to prove
# a local sub-obligation, then recompose.
#
# Modes (complementary partition of the rules by their LHS head):
#   abstract-builtins : DROP the defining rules of the arithmetic / terminating-
#                       builtin denylist, KEEP their op declarations (so the
#                       arith ops become free constructors). Leaves the spec layer
#                       only. Used as a (NR)/stratification DETECTOR: AProVE YES
#                       => the spec op terminates relative to arith-as-black-box;
#                       MAYBE => the lift set is incomplete (a recursive $-helper
#                       is still in the spec layer).
#   arith-core        : KEEP only the denylist (+ bool) rules. The full arithmetic
#                       library incl. the recursive $-helpers (obligation A).
#   arith-pure        : like arith-core but EXCLUDING the recursive $-helpers
#                       (bitstr/int-to-bitstr/*-nat) -> the pure structural b*/int
#                       core (obligation A1).
#
# Signature is pruned exactly like prune_slice_signature.py's `full` mode (keep
# only sorts/ops the KEPT rules use) so MTT stays bakery-scale.
#
# Usage: prune_modular.py SRC DST {abstract-builtins|arith-core} [extra_deny_csv]
#   extra_deny_csv : extra LHS-head names to treat as terminating builtins
#                    (e.g. "eq,eqg" for the strip slices whose char-equality guard
#                    drags in the 50k-rule Val-equality theory).
import sys, re

src, dst, mode = sys.argv[1], sys.argv[2], sys.argv[3]
extra = set(x for x in (sys.argv[4].split(",") if len(sys.argv) > 4 and sys.argv[4] else []) if x)

# $-prefixed helpers that recurse on arithmetic/helper OUTPUT (semantic, not
# structural). They must be lifted into R_arith, so they belong on the denylist
# even though they are $-prefixed.
DOLLAR_RECURSIVE = {"$bitstr-to-int", "$int-to-bitstr",
                    "$band-nat", "$band-nat-cross", "$bxor-nat", "$bor-nat"}
INT_SET = {"int-pos", "int-neg", "abs-nat", "nonneg-int", "sub-nat"}
BOOL_KEEP = {"not", "and", "equiv"}   # trivially terminating; kept in both modes


def is_arith(h):
    if h in DOLLAR_RECURSIVE:
        return True
    if h in extra:
        return True
    if h.startswith("$"):
        return False
    if h.startswith("b"):                       # b* binary-nat family + auxiliaries
        return True
    if h.endswith("-int") or "-int-" in h:      # add-int, mul-int, div-int-aux, sub-int-nat ...
        return True
    if h in INT_SET:
        return True
    return False


op_re = re.compile(r'^\s*op\s+(\S+)\s+:\s*(.*?)\s*->\s+(\S+)\s*(\[.*\])?\s*\.\s*$')
sorts_re = re.compile(r'^\s*sorts\s+(.*)\s+\.\s*$')
subsort1_re = re.compile(r'^\s*subsort\s+(\S+)\s+<\s+(\S+)\s+\.\s*$')
subsortN_re = re.compile(r'^\s*subsorts\s+(.*)\s+<\s+(\S+)\s+\.\s*$')
rule_re = re.compile(r'^\s*(eq|ceq|rl|crl)\s+(.*)$')


def head(body):
    b = body.strip()
    i = b.find('(')
    return b.split()[0] if i < 0 else b[:i].strip()


ops = {}
all_sorts = []
subsort1 = []
subsortN = []
other = []
arith_rules = []
spec_rules = []

for ln in open(src).read().splitlines():
    m = op_re.match(ln)
    if m:
        name, dom, cod, _ = m.groups()
        ops[name] = (ln, dom.split() if dom.strip() else [], cod)
        continue
    m = sorts_re.match(ln)
    if m:
        all_sorts = m.group(1).split()
        continue
    m = subsortN_re.match(ln)
    if m:
        subsortN.append((m.group(1).split(), m.group(2), ln))
        continue
    m = subsort1_re.match(ln)
    if m:
        subsort1.append((m.group(1), m.group(2), ln))
        continue
    m = rule_re.match(ln)
    if m:
        h = head(m.group(2))
        if is_arith(h):
            arith_rules.append((h, ln))          # (head, line)
        elif h in BOOL_KEEP and mode in ("arith-core", "arith-pure"):
            arith_rules.append((h, ln))          # bool kept alongside arith in A-modes
        else:
            spec_rules.append(ln)                # plain line (bool falls here in abstract mode)
        continue
    other.append(ln)

if mode == "abstract-builtins":
    kept = spec_rules
elif mode == "arith-core":                 # full arithmetic library (A), incl. recursive $-helpers
    kept = [ln for _, ln in arith_rules]
elif mode == "arith-pure":                 # A1: pure b*/int arithmetic, EXCLUDING the recursive
    kept = [ln for h, ln in arith_rules     # $-helpers (bitstr/int-to-bitstr/*-nat) -> structural core
            if h not in DOLLAR_RECURSIVE]
else:
    sys.stderr.write("mode must be abstract-builtins|arith-core|arith-pure\n")
    sys.exit(2)

# used ops = ops appearing in the KEPT rules (so the signature shrinks to what
# remains; abstracted arith ops still referenced by kept rules survive as
# ruleless op declarations = free constructors).
rt = "\n".join(kept)
for d in "(),:=":
    rt = rt.replace(d, " ")
rt = rt.replace("/\\", " ")
tokens = set(rt.split())
used_ops = {n for n in ops if n in tokens}

# needed sorts = codomain+domain of used ops + sort annotations in kept rules
# (same as prune_slice_signature.py `full`).
needed_sorts = set()
for n in used_ops:
    _, doms, cod = ops[n]
    needed_sorts.add(cod)
    needed_sorts.update(doms)
needed_sorts &= set(all_sorts) if all_sorts else needed_sorts
needed_sorts |= (tokens & set(all_sorts))
needed_sorts.add("Val")
keep_sorts = needed_sorts

out = []
for ln in other:
    out.append(ln)
    if ln.strip().startswith("(mod SPEC is"):
        if keep_sorts:
            out.append("  sorts " + " ".join(sorted(keep_sorts)) + " .")
        for al, b, sl in subsortN:
            kk = [a for a in al if a in keep_sorts]
            if b in keep_sorts and kk:
                out.append("  subsorts " + " ".join(kk) + " < " + b + " .")
        for a, b, sl in subsort1:
            if a in keep_sorts and b in keep_sorts:
                out.append("  subsort " + a + " < " + b + " .")
        for n in sorted(used_ops):
            out.append(ops[n][0])
        out.extend(kept)

open(dst, "w").write("\n".join(out) + "\n")
sys.stderr.write(
    f"[modular:{mode}] arith_rules {len(arith_rules)} spec_rules {len(spec_rules)} "
    f"kept {len(kept)}  ops {len(ops)}->{len(used_ops)}  sorts {len(all_sorts)}->{len(keep_sorts)}\n")
