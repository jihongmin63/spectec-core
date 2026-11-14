"""Estimate what an injection encoding (order-sorted -> many-sorted) of the
analysis module would cost.

Input: the module that `main.exe rewrite --ctrs` prints for a spec.

    SPEC=$(find spectec/specs/p4 -name '*.spectec' | sort)
    _build/default/spectec/bin/main.exe rewrite --ctrs $SPEC > p4.ctrs.maude
    python3 -I docs/research/2026-11-08-injection-cost.py p4.ctrs.maude

A "narrow binder" is the first pattern occurrence (lhs, or the pattern side of
a condition) of a variable whose declared sort is strictly below the sort of
the position it occupies. Only those occurrences make order-sorted matching
differ from unsorted matching.

Two duplication estimates are reported for encoding a narrow binder X:S at a
position of sort T:
  direct  one injection per comparable pair; X must be matched once per sort
          that can be the least sort of a term below S.
  chain   injections along the subsort Hasse diagram with a fixed parent
          choice; X needs one pattern per distinct point where a canonical
          path from below S leaves S's down-set.
Every sort that is some operator's result sort is assumed to be a possible
least sort, so both are estimates for arbitrary (not only reachable) terms.
"""

import sys
from collections import Counter, defaultdict
from functools import lru_cache
from math import prod

VAL = "Val"


def tokenize(s):
    toks, cur = [], []
    for ch in s:
        if ch in "(),":
            if cur:
                toks.append("".join(cur))
                cur = []
            toks.append(ch)
        elif ch.isspace():
            if cur:
                toks.append("".join(cur))
                cur = []
        else:
            cur.append(ch)
    if cur:
        toks.append("".join(cur))
    return toks


class Parser:
    def __init__(self, toks):
        self.t, self.i = toks, 0

    def peek(self):
        return self.t[self.i] if self.i < len(self.t) else None

    def next(self):
        tok = self.t[self.i]
        self.i += 1
        return tok

    def term(self):
        head = self.next()
        if self.peek() == "(":
            self.next()
            args = [self.term()]
            while self.peek() == ",":
                self.next()
                args.append(self.term())
            assert self.next() == ")"
            return (head, args)
        if ":" in head and not head.startswith(":"):
            name, sort = head.rsplit(":", 1)
            return ("$VAR", name, sort)
        return (head, [])


def parse_eq(line):
    body = line.strip()
    kind = body.split(None, 1)[0]
    body = body[len(kind) :].strip()
    assert body.endswith(" ."), body[-40:]
    p = Parser(tokenize(body[:-2]))
    lhs = p.term()
    assert p.next() == "="
    rhs = p.term()
    conds = []
    if p.peek() == "if":
        p.next()
        while True:
            s = p.term()
            assert p.next() == "="
            conds.append((s, p.term()))
            if p.peek() != "/\\":
                break
            p.next()
    assert p.peek() is None
    return lhs, rhs, conds


def load(path):
    sorts, edges, sigs, eqs = {VAL}, set(), defaultdict(list), []
    for line in open(path):
        w = line.split()
        if not w:
            continue
        if w[0] == "sorts":
            sorts.update(x for x in w[1:] if x != ".")
        elif w[0] in ("subsort", "subsorts"):
            lt = w.index("<")
            edges.update((sub, w[lt + 1]) for sub in w[1:lt])
        elif w[0] == "op":
            colon, arrow = w.index(":"), w.index("->")
            sigs[(" ".join(w[1:colon]), arrow - colon - 1)].append(
                (w[colon + 1 : arrow], w[arrow + 1])
            )
        elif w[0] in ("eq", "ceq"):
            eqs.append(parse_eq(line))
    return sorts, edges, sigs, eqs


def main(path):
    sorts, edges, sigs, eqs = load(path)
    ups = defaultdict(set)
    for a, b in edges:
        ups[a].add(b)

    @lru_cache(None)
    def above(s):
        return frozenset().union(*({b} | above(b) for b in ups[s]))

    def leq(a, b):
        return a == b or b == VAL or b in above(a)

    @lru_cache(None)
    def hasse_parents(a):
        return tuple(
            sorted(
                b for b in ups[a] if not any(c != b and b in above(c) for c in ups[a])
            )
        )

    multi_parent = [
        a for a in list(ups) if len([b for b in hasse_parents(a) if b != VAL]) > 1
    ]
    overloaded = [k for k, v in sigs.items() if len(v) > 1]

    def sig_of(f, n):
        lst = sigs.get((f, n))
        return lst[0] if lst and len(lst) == 1 else ([VAL] * n, VAL)

    def sort_of(t):
        return t[2] if t[0] == "$VAR" else sig_of(t[0], len(t[1]))[1]

    result_sorts = {res for lst in sigs.values() for _, res in lst}
    down = defaultdict(set)
    for s in result_sorts:
        for b in above(s) | {s}:
            down[b].add(s)

    @lru_cache(None)
    def exit_point(sp, s, t):
        cur, last = sp, sp if leq(sp, s) else None
        while cur != t:
            nxt = [p for p in hasse_parents(cur) if leq(p, t)]
            if not nxt:
                break
            cur = nxt[0]
            if leq(cur, s):
                last = cur
        return last

    def chain_factor(s, t):
        return max(1, len({exit_point(sp, s, t) for sp in down[s]} - {None}))

    nodes = injections = 0
    narrow_rules = 0
    direct, chain = [], []
    not_sort_decreasing = Counter()
    chain_heavy = Counter()

    for lhs, rhs, conds in eqs:
        bound, narrow = set(), []

        def walk(t, expected, pattern):
            nonlocal nodes, injections
            nodes += 1
            s = sort_of(t)
            below = expected is not None and s != expected and leq(s, expected)
            injections += below
            if t[0] == "$VAR":
                if pattern and t[1] not in bound:
                    bound.add(t[1])
                    if below:
                        narrow.append((s, expected))
                return
            for a, e in zip(t[1], sig_of(t[0], len(t[1]))[0]):
                walk(a, e, pattern)

        largs, lres = sig_of(lhs[0], len(lhs[1]))
        for a, e in zip(lhs[1], largs):
            walk(a, e, True)
        for s, t in conds:
            walk(s, None, False)
            walk(t, sort_of(s), True)
        walk(rhs, lres, False)
        if not leq(sort_of(rhs), lres):
            not_sort_decreasing[lhs[0]] += 1
        narrow_rules += bool(narrow)
        direct.append(prod(max(1, len(down[s])) for s, _ in narrow))
        chain.append(prod(chain_factor(s, t) for s, t in narrow))
        if chain[-1] >= 1000:
            chain_heavy[lhs[0]] = max(chain_heavy[lhs[0]], chain[-1])

    n = len(eqs)

    def buckets(fs):
        c = Counter(
            "1" if f == 1 else "2-9" if f < 10 else "10-99" if f < 100
            else "100-999" if f < 1000 else ">=1000"
            for f in fs
        )
        return {k: c[k] for k in ("1", "2-9", "10-99", "100-999", ">=1000")}

    print(f"equations                        {n}")
    print(f"sorts                            {len(sorts)}")
    print(f"declared subsort edges           {len(edges)}")
    print(f"sorts with >=2 non-Val parents   {len(multi_parent)}")
    print(f"overloaded (name, arity)         {len(overloaded)}")
    print(f"term nodes                       {nodes}")
    print(f"injection sites                  {injections} ({100*injections/nodes:.1f}%)")
    print(f"rules with a narrow binder       {narrow_rules} ({100*narrow_rules/n:.1f}%)")
    for name, fs in (("direct", direct), ("chain", chain)):
        print(
            f"{name:6} expanded rules          {sum(fs)} (x{sum(fs)/n:.2f})"
            f"  max {max(fs)}  {buckets(fs)}"
        )
    print(f"chain >=1000 heads               {chain_heavy.most_common()}")
    print(
        f"not sort-decreasing              {sum(not_sort_decreasing.values())}"
        f"  e.g. {not_sort_decreasing.most_common(4)}"
    )


if __name__ == "__main__":
    main(sys.argv[1])
