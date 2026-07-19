#!/bin/bash
# run-scc-sweep.sh [PATTERN] [maude-timeout-secs]
#
# Sufficient-completeness sweep over every symbol of the analysis CTRS, using a
# ONE-SHOT slice dump: translating specs/p4 takes ~50s and dwarfs the check
# itself, so `rewrite --ctrs --unconditional --slice-dir` writes all ~2.4k slices
# in a single translation and this driver replays them through run-scc.sh.
#
#   PATTERN   grep -E over the symbol name, to sweep a family first
#             (e.g. '^(subty|match)_' -- the SCC's highest-value targets, since
#             their rules are unconditional so their verdicts are `exact`).
#             Default: every symbol.
#
# Appends one run-scc.sh row per symbol to scc_sweep.tsv and SKIPS symbols
# already recorded, so a re-run continues where it stopped. Smallest slices
# first: those are the ones the checker can actually finish, and a big slice
# TIMEOUT at the front would otherwise burn the budget.
#
# RUN IN A CLEAN ENVIRONMENT -- like the CRC and differential drivers, two
# concurrent Maude jobs exhaust RAM and corrupt each other's output.
set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
SPECTEC="$(cd "$HERE/../.." && pwd)"
BIN="$SPECTEC/_build/default/bin/main.exe"
SLICES="${SCC_SLICE_DIR:-$SPECTEC/tools/mfe/.scc-slices}"
OUT="${SCC_OUT:-$SPECTEC/../scc_sweep.tsv}"

pattern="${1:-.}"
tmo="${2:-600}"

if [ ! -f "$SLICES/_fidelity.tsv" ]; then
  echo "dumping every slice once (~10 min on p4) -> $SLICES" >&2
  mkdir -p "$SLICES"
  P4=$(find "$SPECTEC/specs/p4" -name '*.spectec' | sort)
  "$BIN" rewrite --ctrs --unconditional --slice-dir "$SLICES" $P4 >/dev/null 2>&1 \
    || { echo "slice dump failed" >&2; exit 1; }
fi

touch "$OUT"
# smallest first: rule count is the tractability proxy the CRC sweep already uses
syms=$(cd "$SLICES" && grep -cE '^\s*c?eq |^\s*c?rl ' *.mod 2>/dev/null \
       | sed 's/\.mod:/\t/' | awk -F'\t' '{print $2"\t"$1}' | sort -n \
       | cut -f2 | grep -E "$pattern")

total=$(wc -l <<<"$syms"); n=0
for s in $syms; do
  n=$((n + 1))
  if cut -f1 "$OUT" | grep -qxF "$s"; then continue; fi
  SCC_SLICE_DIR="$SLICES" "$HERE/run-scc.sh" "$s" "$tmo" >> "$OUT"
  printf '\r[%d/%d] %-60s' "$n" "$total" "$s" >&2
done
echo >&2

echo "--- verdicts" >&2
cut -f2 "$OUT" | sort | uniq -c | sort -rn >&2
