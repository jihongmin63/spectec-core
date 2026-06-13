#!/usr/bin/env bash
# Differential test: native interpreter (p4 typecheck) vs Maude rewrite backend
# (run --p4) on the same P4 programs, both running judgment Program_ok.
set -u
cd /home/min/spectec-core

BIN=spectec/_build/default/bin/main.exe
INC=spectec/testdata/interp/p4/p4c/includes
SAMPLES=spectec/testdata/interp/p4/p4c/p4_16_samples
SPEC=$(find spectec/specs/p4 -name '*.spectec' | sort | tr '\n' ' ')
MAUDE_TIMEOUT=${MAUDE_TIMEOUT:-60}
OUT=/tmp/difftest_out

printf "%-24s | %-20s | %-22s | %s\n" "sample" "interpreter" "maude backend" "match"
printf -- "-------------------------+----------------------+------------------------+------\n"

for f in "$@"; do
  path="$SAMPLES/$f"
  [ -f "$path" ] || { printf "%-24s | (missing file)\n" "$f"; continue; }

  # --- interpreter ---
  s=$(date +%s%N)
  $BIN p4 typecheck -p "$path" -i $INC >$OUT.i 2>&1
  irc=$?
  e=$(date +%s%N); itime=$(( (e - s) / 1000000 ))
  if [ "$irc" -eq 0 ] && grep -q "Typechecker succeeded" $OUT.i; then ires="PASS"; else ires="FAIL"; fi

  # --- maude backend ---
  s=$(date +%s%N)
  $BIN run --p4 "$path" -i $INC --timeout $MAUDE_TIMEOUT $SPEC >$OUT.m 2>&1
  mrc=$?
  e=$(date +%s%N); mtime=$(( (e - s) / 1000000 ))
  if grep -q "TIMEOUT" $OUT.m; then mres="TIMEOUT"
  elif grep -q "isStuckHead\|isStuck\|stuck" $OUT.m; then mres="STUCK"
  elif grep -qiE "no result|cannot" $OUT.m; then mres="ERROR"
  elif grep -q "^result:\|result:" $OUT.m && [ "$mrc" -eq 0 ]; then mres="PASS"
  else mres="FAIL($mrc)"; fi

  if [ "$ires" = "PASS" ] && [ "$mres" = "PASS" ]; then match="YES"
  elif [ "$ires" = "FAIL" ] && [ "$mres" != "PASS" ]; then match="(both reject)"
  else match="NO"; fi

  printf "%-24s | %-6s %10sms | %-9s %10sms | %s\n" "$f" "$ires" "$itime" "$mres" "$mtime" "$match"
done
