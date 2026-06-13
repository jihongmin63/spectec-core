#!/usr/bin/env bash
# Run the p4-old spec over every p4_16_samples/*.p4 and categorize the result.
set -u
cd /home/min/spectec-core

EXE=spectec/_build/default/bin/main.exe
SPEC=$(find spectec/specs/p4-old -name '*.spectec' | sort | tr '\n' ' ')
INC=spectec/testdata/interp/p4/p4c/includes
SAMPLES=(spectec/testdata/interp/p4/p4c/p4_16_samples/*.p4)
OUT=/home/min/spectec-core/p4old_samples_results.tsv
PER_FILE_TIMEOUT=90   # outer wall-clock guard per file (seconds)

: > "$OUT"
total=${#SAMPLES[@]}
i=0
declare -A counts=()
for f in "${SAMPLES[@]}"; do
  i=$((i+1))
  name=$(basename "$f")
  raw=$(timeout "$PER_FILE_TIMEOUT" "$EXE" run --p4 "$f" -i "$INC" $SPEC 2>&1)
  rc=$?
  if [ "$rc" -eq 124 ]; then
    status=TIMEOUT
  elif echo "$raw" | grep -q "FAIL (stuck)"; then
    status=STUCK
  elif echo "$raw" | grep -qE "^result:"; then
    status=OK
  elif echo "$raw" | grep -qiE "parse error|syntax error|Sys_error|Failure|exception|Fatal error"; then
    status=ERROR
  else
    status=OTHER
  fi
  counts[$status]=$(( ${counts[$status]:-0} + 1 ))
  printf '%s\t%s\n' "$status" "$name" >> "$OUT"
  if [ $((i % 25)) -eq 0 ] || [ "$i" -eq "$total" ]; then
    printf 'progress %d/%d  OK=%d STUCK=%d TIMEOUT=%d ERROR=%d OTHER=%d\n' \
      "$i" "$total" "${counts[OK]:-0}" "${counts[STUCK]:-0}" \
      "${counts[TIMEOUT]:-0}" "${counts[ERROR]:-0}" "${counts[OTHER]:-0}"
  fi
done

echo "=== DONE: $total files ==="
echo "OK=${counts[OK]:-0} STUCK=${counts[STUCK]:-0} TIMEOUT=${counts[TIMEOUT]:-0} ERROR=${counts[ERROR]:-0} OTHER=${counts[OTHER]:-0}"
echo "results -> $OUT"
