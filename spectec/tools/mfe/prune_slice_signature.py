#!/usr/bin/env python3
# Prune a To_mfe SPEC .mod to only the signature its rules actually use, so the
# MTT termination transform sees a bakery-scale module instead of the full
# ~460-sort / ~750-op P4 signature. Two modes:
#   ops   : keep all sorts/subsorts, drop unused op decls (safe).
#   full  : also drop sorts/subsorts not reachable from used ops+rules.
import sys, re

src, dst, mode = sys.argv[1], sys.argv[2], (sys.argv[3] if len(sys.argv) > 3 else "full")
lines = open(src).read().splitlines()

op_re = re.compile(r'^\s*op\s+(\S+)\s+:\s*(.*?)\s*->\s+(\S+)\s*(\[.*\])?\s*\.\s*$')
sorts_re = re.compile(r'^\s*sorts\s+(.*)\s+\.\s*$')
subsort1_re = re.compile(r'^\s*subsort\s+(\S+)\s+<\s+(\S+)\s+\.\s*$')
subsortN_re = re.compile(r'^\s*subsorts\s+(.*)\s+<\s+(\S+)\s+\.\s*$')
rule_re = re.compile(r'^\s*(eq|ceq|rl|crl)\s')

ops = {}          # name -> (line, domain-sorts list, codomain)
sort_line = None
subsort1 = []     # (a,b,line)
subsortN = []     # (list-of-a, b, line)
all_sorts = []
rule_lines = []
other = []        # header/footer lines to keep verbatim (mod header, endm, blanks)

for ln in lines:
    m = op_re.match(ln)
    if m:
        name, dom, cod, _ = m.groups()
        doms = dom.split() if dom.strip() else []
        ops[name] = (ln, doms, cod)
        continue
    m = sorts_re.match(ln)
    if m:
        sort_line = ln
        all_sorts = m.group(1).split()
        continue
    m = subsortN_re.match(ln)
    if m:
        subsortN.append((m.group(1).split(), m.group(2), ln)); continue
    m = subsort1_re.match(ln)
    if m:
        subsort1.append((m.group(1), m.group(2), ln)); continue
    if rule_re.match(ln):
        rule_lines.append(ln); continue
    other.append(ln)

# tokens used in rules
rule_text = "\n".join(rule_lines)
for d in "(),:=":
    rule_text = rule_text.replace(d, " ")
rule_text = rule_text.replace("/\\", " ")
tokens = set(rule_text.split())

used_ops = {n for n in ops if n in tokens}

# needed sorts = codomain+domain of used ops + sort annotations in rules (tokens that are sort names)
needed_sorts = set()
for n in used_ops:
    _, doms, cod = ops[n]
    needed_sorts.add(cod); needed_sorts.update(doms)
needed_sorts &= set(all_sorts) if all_sorts else needed_sorts
# sort annotations appear as tokens too (e.g. BoolV, List, Val, NatV, Text)
needed_sorts |= (tokens & set(all_sorts))
needed_sorts.add("Val")  # top sort always kept

if mode == "ops":
    keep_sorts = set(all_sorts)
else:
    # Keep the sorts named by used ops' signatures + rule sort-annotations + Val,
    # PLUS every sort that lies on a subsort path between two of them.
    #
    # Do NOT close over subsort edges wholesale: every sort is `< Val`, so any
    # closure that touches Val re-expands to the whole lattice. But dropping the
    # interior of a path does break well-formedness, now that the predicates are
    # declared over a recovered domain instead of Val (Maude_sorts.predicate_
    # domains): a rule may pass a `BaseType` term to a `TypeArgument` position
    # via `BaseType < RealTypeArgument < TypeArgument`, and pruning the middle
    # sort leaves `BaseType` unrelated to `TypeArgument` -- the slice then parses
    # ill-sorted (`no parse` / ERROR) where the full module is fine. P4 has 286
    # such two-step chains.
    edges = [(a, b) for a, b, _ in subsort1 if b != "Val"]
    edges += [(a, b) for al, b, _ in subsortN if b != "Val" for a in al]
    up, down = {}, {}
    for a, b in edges:
        up.setdefault(a, set()).add(b)     # a < b
        down.setdefault(b, set()).add(a)
    def reach(starts, adj):
        seen, stack = set(), list(starts)
        while stack:
            x = stack.pop()
            for y in adj.get(x, ()):
                if y not in seen:
                    seen.add(y); stack.append(y)
        return seen
    above = reach(needed_sorts, up)     # sorts reachable UP from a kept sort
    below = reach(needed_sorts, down)   # sorts from which a kept sort is reachable
    keep_sorts = needed_sorts | (above & below)

# emit
out = []
for ln in other:
    out.append(ln)
    if ln.strip().startswith("(mod SPEC is") or ln.strip() == "(mod SPEC is":
        # sorts + subsorts inserted right after the mod header
        if keep_sorts:
            out.append("  sorts " + " ".join(sorted(keep_sorts)) + " .")
        for al, b, sl in subsortN:
            kept = [a for a in al if a in keep_sorts]
            if b in keep_sorts and kept:
                out.append("  subsorts " + " ".join(kept) + " < " + b + " .")
        for a, b, sl in subsort1:
            if a in keep_sorts and b in keep_sorts:
                out.append("  subsort " + a + " < " + b + " .")
        for n in sorted(used_ops):
            out.append(ops[n][0])
        out.extend(rule_lines)

open(dst, "w").write("\n".join(out) + "\n")
sys.stderr.write(f"[prune:{mode}] ops {len(ops)}->{len(used_ops)}  sorts {len(all_sorts)}->{len(keep_sorts)}  rules {len(rule_lines)}\n")
