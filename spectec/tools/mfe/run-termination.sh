#!/bin/bash
# run-termination.sh <symbol> [maude-timeout-secs]
#
# Termination check of ONE analysis-CTRS slice via the hook-enabled Maude 2.7.1
# + old MFE 2.7.1 (MTT 1.5j) + AProVE (WST) backend. Prints one line:
#     <symbol>\t<YES|NO|MAYBE|TIMEOUT|ERROR>
#
# Prereqs (all under spectec/tools/, gitignored -- see spectec/tools/mfe/README.md):
#   - tools/maude271-hooks/maude   (v2.7.1-ext-hooks: binds termCheck/writeToFile)
#   - tools/mfe271/MFE-mfe-2.7.1/  (old MFE, bundles MTT 1.5j + old Full Maude)
#   - tools/aprove/aprove.jar + tools/aprove/runme  (WST backend; java required)
#   - tools/maude271-hooks/mfe.config  ->  "aprove <abs>/tools/aprove/runme .trs"
#   - tools/yices1/bin/yices  (yices 1.0.40 -- AProVE's WST strategy hardcodes it;
#     yices 2.x/Z3 do NOT substitute. Manual license-gated download.)
set -uo pipefail

SPECTEC="$(cd "$(dirname "$0")/../.." && pwd)"
BIN="$SPECTEC/_build/default/bin/main.exe"
M2="$SPECTEC/tools/maude271-hooks/maude"
MFE="$SPECTEC/tools/mfe271/MFE-mfe-2.7.1/src/mfe.maude"
export MAUDE_LIB="$SPECTEC/tools/maude271-hooks"

sym="${1:?usage: run-termination.sh <symbol> [timeout]}"
tmo="${2:-600}"
P4=$(find "$SPECTEC/specs/p4" -name '*.spectec' | sort)

tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
"$BIN" rewrite --ctrs --symbol "$sym" $P4 2>/dev/null > "$tmp/slice.mod"
if [ "$(grep -cE '^\s*c?eq |^\s*c?rl ' "$tmp/slice.mod")" -eq 0 ]; then
  printf '%s\tDEGENERATE\n' "$sym"; exit 0
fi

# 3.5.1 analysis surface  ->  old Full Maude functional module.
python3 - "$tmp/slice.mod" "$tmp/slice.fm" <<'PY'
import sys
src, dst = sys.argv[1], sys.argv[2]
t = open(src).read().replace("set include BOOL off .", "")
t = t.replace("(mod SPEC is", "(fmod SPEC is").replace("endm)", "endfm)")
open(dst, "w").write("(set include BOOL off .)\n(set include BOOL-OPS off .)\n" + t.strip() + "\n")
PY

cond=$(grep -c 'ceq' "$tmp/slice.mod")
{ echo "load $MFE";
  cat "$tmp/slice.fm";
  echo "(select tool MTT .)";
  echo "(select external tool aprove .)";
  [ "$cond" -gt 0 ] && echo "(select path C;A .)";  # conditional systems
  echo "(ct SPEC .)";
  echo q;
} > "$tmp/in.txt"

out=$(timeout "$tmo" "$M2" -no-banner < "$tmp/in.txt" 2>&1)
if   grep -q 'is terminating'      <<<"$out"; then v=YES
elif grep -q 'not been found'      <<<"$out"; then v=MAYBE
elif grep -qi 'no parse\|error'    <<<"$out"; then v=ERROR
else v=TIMEOUT; fi
printf '%s\t%s\n' "$sym" "$v"
