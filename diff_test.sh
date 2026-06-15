#!/usr/bin/env bash
# Differential test: native interpreter (p4 typecheck) vs Maude rewrite backend
# (run --p4) on the same P4 programs, both running judgment Program_ok.
#
# The Maude side executes the **p4-old** spec (the project's executable baseline).
# The new `specs/p4` spec now builds a parsable Maude module (the start-term encoder
# fix), but still `FAIL (stuck)`s on the typing/execution frontier (control-body
# generic calls), so p4-old is the spec ALL Maude-side tooling uses (matches
# find_maude_diverging.sh, diff_review.sh, lib/rewrite/CLAUDE.md). The interpreter
# oracle stays on the new spec (the only working interpreter), so this is an
# interp(p4) vs Maude(p4-old) comparison.
#
# The Maude backend is BATCHED: every sample is run in ONE `run` invocation
# (repeated --p4), so the ~50k-line module is parsed once for the whole set
# instead of once per program. The per-program `=== <path> ===` blocks are split
# back out and joined to each interpreter result. (The interpreter stays
# per-file -- it is the oracle and is already cheap.) Because the batch shares
# one timeout, the maude column shows verdicts only, with the batch's total time
# reported once below the table.
set -u
cd /home/min/spectec-core

BIN=spectec/_build/default/bin/main.exe
INC=spectec/testdata/interp/p4/p4c/includes
SAMPLES=spectec/testdata/interp/p4/p4c/p4_16_samples
SPEC=$(find spectec/specs/p4-old -name '*.spectec' | sort | tr '\n' ' ')
MAUDE_TIMEOUT=${MAUDE_TIMEOUT:-0}   # whole-batch timeout (0 disables)
OUT=/tmp/difftest_out

# Valid samples (existing files), preserving the given order.
files=(); for f in "$@"; do [ -f "$SAMPLES/$f" ] && files+=("$f"); done

# --- maude backend, batched over all samples in one invocation ---
declare -A MVERDICT
mtotal=0
if [ "${#files[@]}" -gt 0 ]; then
  args=(); for f in "${files[@]}"; do args+=(--p4 "$SAMPLES/$f"); done
  s=$(date +%s%N)
  $BIN run "${args[@]}" -i $INC --timeout $MAUDE_TIMEOUT $SPEC >$OUT.m 2>&1
  e=$(date +%s%N); mtotal=$(( (e - s) / 1000000 ))
  # A single sample prints the bare result (no === markers); synthesize one so
  # the splitter below has a uniform shape.
  if [ "${#files[@]}" -eq 1 ]; then
    { printf '=== %s ===\n' "$SAMPLES/${files[0]}"; cat $OUT.m; } > $OUT.m.tmp && mv $OUT.m.tmp $OUT.m
  fi
  while IFS=$'\t' read -r p v; do MVERDICT["$p"]=$v; done < <(awk '
    /^=== .* ===$/ { if (seen) emit(); seen=1; path=$0; sub(/^=== /,"",path); sub(/ ===$/,"",path); block=""; next }
    { block = block $0 "\n" }
    END { if (seen) emit() }
    function emit() {
      v = (block ~ /FAIL \(stuck\)/) ? "STUCK" :
          (block ~ /(^|\n)result:/) ? "PASS" :
          (block ~ /TIMEOUT/) ? "TIMEOUT" :
          (block ~ /(^|\n)ERROR:|no result|cannot/) ? "ERROR" : "FAIL";
      print path "\t" v
    }' $OUT.m)
fi

printf "%-24s | %-20s | %-14s | %s\n" "sample" "interpreter" "maude backend" "match"
printf -- "-------------------------+----------------------+----------------+------\n"

for f in "$@"; do
  [ -f "$SAMPLES/$f" ] || { printf "%-24s | (missing file)\n" "$f"; continue; }

  # --- interpreter (per file; the oracle) ---
  s=$(date +%s%N)
  $BIN p4 typecheck -p "$SAMPLES/$f" -i $INC >$OUT.i 2>&1
  irc=$?
  e=$(date +%s%N); itime=$(( (e - s) / 1000000 ))
  if [ "$irc" -eq 0 ] && grep -q "Typechecker succeeded" $OUT.i; then ires="PASS"; else ires="FAIL"; fi

  mres=${MVERDICT["$SAMPLES/$f"]:-FAIL}

  if [ "$ires" = "PASS" ] && [ "$mres" = "PASS" ]; then match="YES"
  elif [ "$ires" = "FAIL" ] && [ "$mres" != "PASS" ]; then match="(both reject)"
  else match="NO"; fi

  printf "%-24s | %-6s %10sms | %-14s | %s\n" "$f" "$ires" "$itime" "$mres" "$match"
done

printf -- "-------------------------+----------------------+----------------+------\n"
printf "maude backend: %d samples batched in one invocation, %sms total\n" "${#files[@]}" "$mtotal"
