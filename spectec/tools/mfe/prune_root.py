#!/usr/bin/env python3
# Keep only the rules reachable (RHS-call closure) from a ROOT op set, then prune
# the signature. For proving an individual arithmetic operator's termination in
# isolation (the A1 split, when the whole arith library is too big for AProVE in
# one shot). Operates only on the transient analysis .mod.
# Usage: prune_root.py SRC DST root1,root2,...
import sys, re

src, dst = sys.argv[1], sys.argv[2]
roots = set(x for x in sys.argv[3].split(",") if x)

op_re = re.compile(r'^\s*op\s+(\S+)\s+:\s*(.*?)\s*->\s+(\S+)\s*(\[.*\])?\s*\.\s*$')
rule_re = re.compile(r'^\s*(eq|ceq|rl|crl)\s+(.*)$')
sorts_re = re.compile(r'^\s*sorts\s+(.*)\s+\.\s*$')
subsort1_re = re.compile(r'^\s*subsort\s+(\S+)\s+<\s+(\S+)\s+\.\s*$')
subsortN_re = re.compile(r'^\s*subsorts\s+(.*)\s+<\s+(\S+)\s+\.\s*$')


def head(body):
    b = body.strip(); i = b.find('(')
    return b.split()[0] if i < 0 else b[:i].strip()


def toks(s):
    for d in "(),:=":
        s = s.replace(d, " ")
    return set(s.replace("/\\", " ").split())


ops = {}; all_sorts = []; ss1 = []; ssN = []; other = []; rules = []
for ln in open(src).read().splitlines():
    m = op_re.match(ln)
    if m:
        ops[m.group(1)] = (ln, m.group(2).split() if m.group(2).strip() else [], m.group(3)); continue
    m = sorts_re.match(ln)
    if m:
        all_sorts = m.group(1).split(); continue
    m = subsortN_re.match(ln)
    if m:
        ssN.append((m.group(1).split(), m.group(2), ln)); continue
    m = subsort1_re.match(ln)
    if m:
        ss1.append((m.group(1), m.group(2), ln)); continue
    m = rule_re.match(ln)
    if m:
        rules.append((head(m.group(2)), ln, toks(m.group(2)))); continue
    other.append(ln)

opnames = set(ops)
clo = set(roots)
changed = True
while changed:
    changed = False
    for h, ln, tk in rules:
        if h in clo:
            new = (tk & opnames) - clo
            if new:
                clo |= new; changed = True

kept = [ln for h, ln, tk in rules if h in clo]
kt = set()
for h, ln, tk in rules:
    if h in clo:
        kt |= tk
used_ops = {n for n in ops if n in kt}
needed = set()
for n in used_ops:
    _, doms, cod = ops[n]
    needed.add(cod); needed.update(doms)
needed &= set(all_sorts)
needed |= (kt & set(all_sorts))
needed.add("Val")

out = []
for ln in other:
    out.append(ln)
    if ln.strip().startswith("(mod SPEC is"):
        out.append("  sorts " + " ".join(sorted(needed)) + " .")
        for al, b, sl in ssN:
            kk = [a for a in al if a in needed]
            if b in needed and kk:
                out.append("  subsorts " + " ".join(kk) + " < " + b + " .")
        for a, b, sl in ss1:
            if a in needed and b in needed:
                out.append("  subsort " + a + " < " + b + " .")
        for n in sorted(used_ops):
            out.append(ops[n][0])
        out.extend(kept)
open(dst, "w").write("\n".join(out) + "\n")
sys.stderr.write(f"[root:{','.join(sorted(roots))}] closure_ops {len(clo)} kept {len(kept)} rules "
                 f"used_ops {len(used_ops)} sorts {len(needed)}\n")
