# Empty-pattern loop witness for $strip_prefix_rec / $strip_suffix_rec:
# drive the top-level recursion one unfolding at a time, with every
# sub-computation (guard, strip) normalized by the emitted TRS's OWN rules
# (innermost, first matching rule -- same discipline as simulate_w0.py).
import re, sys
sys.setrecursionlimit(100000)

src = open(sys.argv[1]).read().splitlines()
MODE = sys.argv[2]  # "prefix" | "suffix"
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
def normalize(t):
    if t[0] == "V": return t
    t = ("A", t[1], [normalize(a) for a in t[2]])
    for lhs, rhs in RULES:
        s = {}
        if match(lhs, t, s):
            steps[0] += 1
            if steps[0] > 200000: raise RuntimeError("step cap")
            return normalize(subst(rhs, s))
    return t

def T(s): return parse(s)
def calc(h, *a): return normalize(("A", h, [T(x) if isinstance(x, str) else x for x in a]))

REC   = "d_strip_prefix_rec" if MODE == "prefix" else "d_strip_suffix_rec"
GUARD = "d_starts_with"      if MODE == "prefix" else "d_ends_with"
STRIP = "d_strip_prefix"     if MODE == "prefix" else "d_strip_suffix"

# t = "x" (any one-char text), pattern = "" (nil) -- the empty-pattern boundary
t, pat = T("cons(chr_120, nil)"), T("nil")
print(f"start: {REC}({show(t)}, {show(pat)})")
seen = []
for i in range(4):
    key = show(t)
    if key in seen:
        print(f"** 순환 확인: t가 {seen.index(key)}번째 상태로 복귀, 패턴 불변 — "
              f"{REC}의 무한 유도 존재 (길이-1 순환) **")
        break
    seen.append(key)
    g = calc(GUARD, t, pat)
    if show(g) != "true":
        print(f"  guard = {show(g)} -> 재귀 클로즈 미발화, 종료"); break
    stripped = calc(STRIP, t, pat)
    print(f"  step {i}: guard=true, {STRIP}(t, nil) = {show(stripped)}  ->  재귀")
    t = stripped
print(f"(TRS 규칙 적용 횟수: {steps[0]})")
