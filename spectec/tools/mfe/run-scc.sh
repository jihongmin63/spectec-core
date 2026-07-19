#!/bin/bash
# run-scc.sh <symbol> [maude-timeout-secs]
#
# Sufficient-completeness check of ONE analysis-CTRS slice via the CETA-enabled
# Maude 2.7 + old MFE 2.7.1 (SCC 2a) backend. Prints one line:
#     <symbol>\t<COMPLETE|COUNTEREXAMPLE|MAYBE|DEGENERATE|TIMEOUT|ERROR>\t<exact|approx>\t[witness]
#
# The verdict is only half the answer -- read the third column with it:
#
#   exact   the slice needed no transformation, so the SCC saw every rule.
#           COMPLETE means the symbol really is sufficiently complete (modulo
#           the SCC's standing assumptions: ground weak normalization,
#           confluence, sort-decreasingness -- established separately by the
#           CRC and termination runs).
#   approx  --unconditional dropped conditions and/or linearized patterns to
#           get past the SCC's drop-bad-eqs filter, which over-approximates
#           what the rules match. COMPLETE then proves NOTHING (a guard that
#           fails at run time still leaves the term stuck).
#
# A COUNTEREXAMPLE is sound EITHER WAY: matching more can only hide a missing
# case, never invent one. So the witness term is the thing to chase -- it names
# a constructor case no rule's lhs covers. Triage it like a CRC MAYBE: confirm
# the term is reachable before calling it a bug (a sort-correct witness outside
# any real call site is not a defect).
#
# Prereqs (all under spectec/tools/, gitignored -- see spectec/tools/mfe/README.md):
#   - tools/maude27-ceta/maude     (v2.7-ext-hooks, asset maude-2.7-hooks-linux.zip:
#                                   the ONLY Linux binary with the CETA library
#                                   linked in, which binds SCC's test-emptiness.
#                                   v2.7.1-ext-hooks ships MTT hooks only.)
#   - tools/mfe271/MFE-mfe-2.7.1/  (old MFE, bundles SCC 2a + old Full Maude)
set -uo pipefail

# Same lesson as the CRC and termination paths (commit 59b5e10c): the default
# 8 MB C stack is too small for legitimately-deep tree-automaton construction.
ulimit -s unlimited 2>/dev/null || true

SPECTEC="$(cd "$(dirname "$0")/../.." && pwd)"
BIN="$SPECTEC/_build/default/bin/main.exe"
# SCC_MAUDE overrides the binary: pointing it at a CETA-less Maude (e.g.
# tools/maude271-hooks/maude) exercises everything up to the emptiness test and
# reports ERROR-NO-CETA, which is how to smoke-test the plumbing without CETA.
M2="${SCC_MAUDE:-$SPECTEC/tools/maude27-ceta/maude}"
MFE="$SPECTEC/tools/mfe271/MFE-mfe-2.7.1/src/mfe.maude"
export MAUDE_LIB="$(dirname "$M2")"

sym="${1:?usage: run-scc.sh <symbol> [timeout]}"
tmo="${2:-600}"
P4=$(find "$SPECTEC/specs/p4" -name '*.spectec' | sort)

tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT

# --unconditional makes the slice survive SCC's drop-bad-eqs filter (which
# discards every conditional or non-left-linear equation before building its
# automaton). Its stderr reports what it had to transform: no transformation
# means the SCC sees the rules verbatim, so the verdict is exact.
"$BIN" rewrite --ctrs --unconditional --symbol "$sym" $P4 \
  > "$tmp/slice.raw.mod" 2> "$tmp/xform.log"
if grep -q '^unconditional:' "$tmp/xform.log"; then fidelity=approx; else fidelity=exact; fi

if [ "$(grep -cE '^\s*c?eq |^\s*c?rl ' "$tmp/slice.raw.mod")" -eq 0 ]; then
  printf '%s\tDEGENERATE\t%s\n' "$sym" "$fidelity"; exit 0
fi

# To_mfe emits the WHOLE P4 order-sorted signature (~460 sorts / ~750 ops) for
# every slice, only the rules are sliced. Prune to the sorts/ops the rules
# actually use -- the [ctor] attributes ride along on the retained op decls,
# and the SCC needs them to split the signature into constructors and defined
# symbols at all.
python3 "$(dirname "$0")/prune_slice_signature.py" "$tmp/slice.raw.mod" "$tmp/slice.mod" full 2>/dev/null

# 3.5.1 analysis surface  ->  old Full Maude functional module.
python3 - "$tmp/slice.mod" "$tmp/slice.fm" <<'PY'
import sys
src, dst = sys.argv[1], sys.argv[2]
t = open(src).read().replace("set include BOOL off .", "")
t = t.replace("(mod SPEC is", "(fmod SPEC is").replace("endm)", "endfm)")
open(dst, "w").write("(set include BOOL off .)\n(set include BOOL-OPS off .)\n" + t.strip() + "\n")
PY

{ echo "load $MFE";
  cat "$tmp/slice.fm";
  echo "(select tool SCC .)";
  echo "(scc SPEC .)";
  echo q;
} > "$tmp/in.txt"

# The MFE wraps its result lines at the terminal width, so a verdict phrase is
# split across lines: collapse whitespace before matching (as mfe.ml does).
out=$(timeout "$tmo" "$M2" -no-banner < "$tmp/in.txt" 2>&1 | tr -s ' \t\n' ' ')

# The MFE's object loop reports the check as
#
#   Sufficient completeness check for SPEC
#   Completeness counter-examples: badd-carry(bzero,bzero) with sort NatV
#   Freeness counter-examples: none were found
#   Analysis: it is complete and it is sound
#
# and it is the "Analysis:" line that says how to read the rest: a SOUND
# analysis means a reported counterexample is real, a COMPLETE analysis means
# the absence of one is a proof. Both are reported per run, so pass them
# through rather than hard-coding an assumption.
#
# ORDER MATTERS. A Maude without the CETA library bound refuses with "Warning:
# The sufficient completeness checker is not fully available. Please use the
# trust command to assume that module SPEC IS SUFFICIENTLY COMPLETE." -- match
# a bare 'sufficiently complete' and you read that refusal as a proof. And the
# old Full Maude emits benign "no parse" warnings from its own source while
# loading under Maude 2.7, so an unscoped 'error|no parse' grep reports ERROR
# over a run that actually produced a verdict: test for the verdict FIRST.
witness=""
analysis=$(sed -n 's/.*Analysis: it is \([a-z]*\) and it is \([a-z]*\).*/\1+\2/p' <<<"$out" | head -1)
if   grep -q 'not fully available'                          <<<"$out"; then v=ERROR-NO-CETA
elif grep -q 'Completeness counter-examples: none were found' <<<"$out"; then v=COMPLETE
elif grep -q 'Completeness counter-examples:'                <<<"$out"; then
  v=COUNTEREXAMPLE
  witness=$(sed -n 's/.*Completeness counter-examples: \(.*\)with sort \([A-Za-z0-9]*\) Freeness.*/\1: \2/p' <<<"$out")
elif grep -qiE 'no parse for [^ ]*SPEC|error'                <<<"$out"; then v=ERROR
else v=TIMEOUT; fi

# The transform fidelity (does the SCC see our rules verbatim?) and the SCC's
# own analysis fidelity (is its abstraction sound/complete here?) are separate
# caveats; report both.
[ -n "$analysis" ] && fidelity="$fidelity/analysis:${analysis// /-}"

printf '%s\t%s\t%s\t%s\n' "$sym" "$v" "$fidelity" "$witness"
