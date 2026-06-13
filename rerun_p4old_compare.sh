#!/usr/bin/env bash
# Re-run the p4-old spec over the files recorded in p4old_samples_results.tsv
# (post member-access fix) and tabulate old->new status transitions.
set -u
cd /home/min/spectec-core
EXE=spectec/_build/default/bin/main.exe
SPEC=$(find spectec/specs/p4-old -name '*.spectec' | sort | tr '\n' ' ')
INC=spectec/testdata/interp/p4/p4c/includes
OLD=p4old_samples_results.tsv
NEW=p4old_samples_results_after.tsv
TRANS=p4old_transitions.tsv
PER_FILE_TIMEOUT=90
: > "$NEW"; : > "$TRANS"
total=$(wc -l < "$OLD"); i=0
declare -A t=()
while IFS=$'\t' read -r oldstatus name; do
  i=$((i+1))
  raw=$(timeout "$PER_FILE_TIMEOUT" "$EXE" run --p4 "spectec/testdata/interp/p4/p4c/p4_16_samples/$name" -i "$INC" $SPEC 2>&1)
  rc=$?
  if   [ "$rc" -eq 124 ]; then ns=TIMEOUT
  elif echo "$raw" | grep -q "FAIL (stuck)"; then ns=STUCK
  elif echo "$raw" | grep -qE "^result:"; then ns=OK
  elif echo "$raw" | grep -qiE "parse error|syntax error|Sys_error|Failure|exception|Fatal error"; then ns=ERROR
  else ns=OTHER; fi
  printf '%s\t%s\n' "$ns" "$name" >> "$NEW"
  key="${oldstatus}->${ns}"
  t[$key]=$(( ${t[$key]:-0} + 1 ))
  if [ "$oldstatus" = "OK" ] && [ "$ns" != "OK" ]; then
    printf 'REGRESSION\t%s\t%s\n' "$key" "$name" >> "$TRANS"
  fi
  [ $((i % 25)) -eq 0 ] && printf 'progress %d/%d\n' "$i" "$total"
done < "$OLD"
echo "=== transitions (old->new) ==="
for k in "${!t[@]}"; do printf '  %s : %d\n' "$k" "${t[$k]}"; done | sort
newOK=$(grep -cP '^OK\t' "$NEW"); oldOK=$(grep -cP '^OK\t' "$OLD")
echo "OK: $oldOK -> $newOK  (+$((newOK-oldOK)))"
echo "regressions logged in $TRANS: $(grep -c REGRESSION "$TRANS" 2>/dev/null || echo 0)"
