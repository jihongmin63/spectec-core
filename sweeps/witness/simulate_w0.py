import re, sys
sys.setrecursionlimit(100000)

# --- TPDB parsing ---
src = open(sys.argv[1]).read().splitlines()
VARS = set()
rules = []
for l in src:
    if l.startswith("(VAR"):
        VARS = set(l[4:].rstrip(")").split())
    m = re.match(r"^  (.*?) -> (.*)$", l)
    if m: rules.append((m.group(1), m.group(2)))

def parse(s):
    s = s.strip()
    m = re.match(r"^([^\s(),]+)\s*(\((.*)\))?$", s)
    if not m or m.group(2) is None:
        tok = s
        return ("V", tok) if tok in VARS else ("A", tok, [])
    head, inner = m.group(1), m.group(3)
    args, depth, cur = [], 0, ""
    for c in inner:
        if c == "," and depth == 0: args.append(cur); cur = ""
        else:
            if c == "(": depth += 1
            if c == ")": depth -= 1
            cur += c
    if cur.strip(): args.append(cur)
    return ("A", head, [parse(a) for a in args])

RULES = [(parse(l), parse(r)) for l, r in rules]

def match(p, t, s):
    if p[0] == "V":
        if p[1] in s: return s[p[1]] == t
        s[p[1]] = t; return True
    if t[0] != "A" or p[1] != t[1] or len(p[2]) != len(t[2]): return False
    return all(match(pa, ta, s) for pa, ta in zip(p[2], t[2]))

def subst(t, s):
    if t[0] == "V": return s[t[1]]
    return ("A", t[1], [subst(a, s) for a in t[2]])

def show(t):
    if t[0] == "V": return t[1]
    return t[1] + ("(" + ", ".join(show(a) for a in t[2]) + ")" if t[2] else "")

steps = [0]
def normalize(t, depth=0):
    # leftmost-innermost, first matching rule; loop guard by step cap
    if t[0] == "V": return t
    t = ("A", t[1], [normalize(a, depth+1) for a in t[2]])
    for lhs, rhs in RULES:
        s = {}
        if match(lhs, t, s):
            steps[0] += 1
            if steps[0] > 200000: raise RuntimeError("step cap")
            return normalize(subst(rhs, s), depth+1)
    return t

def T(s): return parse(s)

# --- the w=0 orbit, every computation normalized by the TRS's own rules ---
TWO  = "int_pos(bd0(bone))"
w0   = T("int_pos(bzero)")
def calc(expr_head, *args):
    return normalize(("A", expr_head, list(args)))

pow2w = calc("pow_int", T(TWO), w0)
half  = calc("div_int", pow2w, T(TWO))
neg_half = calc("negate_int", half)
print(f"w=0:  2^w = {show(pow2w)},  half = {show(half)},  -half = {show(neg_half)}")

n = T("int_pos(bzero)")
seen = []
for i in range(6):
    key = show(n)
    print(f"n = {key}")
    if key in seen:
        print(f"** 순환 확인: n이 {seen.index(key)}번째 상태로 복귀 — 무한 유도 존재 **")
        break
    seen.append(key)
    t1 = calc("leq_int", half, n)
    if show(t1) == "true":
        print(f"  chain1 발화 (half<=n = true): n := n - 2^w")
        n = calc("sub_int", n, pow2w)
    else:
        t2 = calc("leq_int", neg_half, n)
        if show(t2) == "false":
            print(f"  chain2 발화 (half<=n = {show(t1)}, -half<=n = false): n := n + 2^w")
            n = calc("add_int", n, pow2w)
        else:
            print(f"  chain3 발화 (종료): 결과 {show(n)}")
            break
print(f"(TRS 규칙 적용 횟수: {steps[0]})")
