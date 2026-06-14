#!/usr/bin/env bash
# find_maude_diverging.sh — list P4 programs the spectec reference interpreter
# ACCEPTS but the Maude rewrite backend does NOT execute to a typing result.
# Each such program is a candidate Maude/translation divergence to triage.
#
# Oracle: p4_typecheck_suite.txt — the interpreter-PASS suite (`p4 batch -v` over
# the new spec; 1061 positive p4_16_samples). Every program in it is known-valid,
# so "Maude != OK" on it is a divergence: either a translation bug OR genuine
# p4-old spec incompleteness. To classify a divergence, re-run the interpreter on
# THAT file with the new spec (`main.exe p4 typecheck -p FILE -i INC`) — do NOT
# use `--spec-dir specs/p4-old`, which is misconfigured (keeps the new target's
# builtins/handler) and reports spurious interp-FAILs. See lib/rewrite/CLAUDE.md.
#
# BATCHED: each Maude run parses a ~50k-line module, which dominates the cost, so
# we run CHUNK programs per Maude invocation (`run` takes repeated --p4) — the
# module is parsed once per chunk instead of once per program. The per-program
# `=== <file> ===` blocks in the batched output are split back out and classified.
# Runs SERIAL (one chunk at a time): parallel chunks would each parse the giant
# module and exhaust RAM. A chunk whose batch times out or whose output is corrupt
# falls back to per-file runs (`classify_one`) so one hanging program is isolated
# (recorded TIMEOUT) without forfeiting its chunk-mates.
# Resumable: re-running skips files already recorded in the progress TSV (chunk
# granularity), so a long run can be done in chunks (Ctrl-C and resume).
#
# Env knobs: SUITE, SPEC_DIR (default p4-old), TIMEOUT (s, per-file fallback),
# BATCH_TIMEOUT (s, whole chunk), CHUNK (programs per Maude run), PROGRESS, OUT.
set -u
cd "$(dirname "$0")"

SUITE=${SUITE:-p4_typecheck_suite.txt}
SPEC_DIR=${SPEC_DIR:-spectec/specs/p4-old}
TIMEOUT=${TIMEOUT:-120}                           # per-file fallback timeout
BATCH_TIMEOUT=${BATCH_TIMEOUT:-600}               # whole-chunk timeout
CHUNK=${CHUNK:-40}                                # programs per Maude invocation
PROGRESS=${PROGRESS:-maude_suite_verdicts.tsv}   # VERDICT\tpath, one per program (resume cache)
OUT=${OUT:-maude_diverging.tsv}                  # the divergences (interp-PASS, Maude-not-OK)

EXE=spectec/_build/default/bin/main.exe
INC=spectec/testdata/interp/p4/p4c/includes
SPEC=$(find "$SPEC_DIR" -name '*.spectec' | sort | tr '\n' ' ')

[ -x "$EXE" ] || { echo "build first: (cd spectec && opam exec --switch=spectecx -- dune build bin/main.exe)" >&2; exit 1; }
[ -f "$SUITE" ] || { echo "missing suite: $SUITE" >&2; exit 1; }

# Map one program's Maude run text (passed as $1) to a verdict. OK = reduced to
# a typing result (`result:`); everything else is a divergence bucket. (Each
# check re-pipes the text -- a single shared stdin would be drained by the first
# grep.)
classify_text() {
  if   printf '%s' "$1" | grep -q  "FAIL (stuck)"; then echo STUCK
  elif printf '%s' "$1" | grep -qE "^result:";     then echo OK
  elif printf '%s' "$1" | grep -qiE "parse error|syntax error|Sys_error|Failure|exception|Fatal error|^ERROR:"; then echo ERROR
  else echo OTHER; fi
}

# Classify one program by its own Maude `run --p4` outcome (the fallback path).
classify_one() {
  local f="$1" raw rc
  raw=$(timeout "$TIMEOUT" "$EXE" run --p4 "$f" -i "$INC" $SPEC 2>&1); rc=$?
  if [ "$rc" -eq 124 ]; then printf 'TIMEOUT\t%s\n' "$f"
  else printf '%s\t%s\n' "$(classify_text "$raw")" "$f"; fi
}

# Classify a whole chunk in ONE Maude invocation, splitting the per-program
# `=== <file> ===` blocks. On a batch-level timeout or a block/file count
# mismatch (corrupt or single-file output), fall back to per-file runs.
classify_chunk() {
  local files=("$@") args=() f raw rc
  for f in "${files[@]}"; do args+=(--p4 "$f"); done
  # --timeout 0 disables the exe's own (whole-batch) timeout; the shell `timeout`
  # bounds the chunk so a hanging program trips the per-file fallback below.
  raw=$(timeout "$BATCH_TIMEOUT" "$EXE" run "${args[@]}" -i "$INC" --timeout 0 $SPEC 2>&1); rc=$?
  if [ "$rc" -eq 124 ]; then
    for f in "${files[@]}"; do classify_one "$f"; done
    return
  fi
  # Split into per-block bodies (in order), one verdict per `=== ... ===` marker.
  mapfile -t verdicts < <(printf '%s\n' "$raw" | awk '
    /^=== .* ===$/ { if (seen) emit(); seen=1; block=""; next }
    { block = block $0 "\n" }
    END { if (seen) emit() }
    function emit() {
      if (block ~ /FAIL \(stuck\)/) print "STUCK";
      else if (block ~ /(^|\n)result:/) print "OK";
      else if (block ~ /parse error|syntax error|Sys_error|Failure|exception|Fatal error|(^|\n)ERROR:/) print "ERROR";
      else print "OTHER";
    }')
  if [ "${#verdicts[@]}" -ne "${#files[@]}" ]; then
    for f in "${files[@]}"; do classify_one "$f"; done
    return
  fi
  local i
  for i in "${!files[@]}"; do printf '%s\t%s\n' "${verdicts[$i]}" "${files[$i]}"; done
}

touch "$PROGRESS"
mapfile -t all < <(grep -v '^#' "$SUITE" | grep -vE '^[[:space:]]*$')
todo=()
for f in "${all[@]}"; do
  awk -F'\t' -v p="$f" '$2==p{ok=1} END{exit !ok}' "$PROGRESS" || todo+=("$f")
done
echo "suite ${#all[@]} | done $(( ${#all[@]} - ${#todo[@]} )) | to run ${#todo[@]} | spec=$SPEC_DIR CHUNK=$CHUNK batch_timeout=${BATCH_TIMEOUT}s" >&2

# Process the todo list one CHUNK-sized batch at a time (serial).
i=0
while [ "$i" -lt "${#todo[@]}" ]; do
  chunk=("${todo[@]:i:CHUNK}")
  classify_chunk "${chunk[@]}" >> "$PROGRESS"
  i=$(( i + CHUNK ))
  echo "  classified $(( i < ${#todo[@]} ? i : ${#todo[@]} )) / ${#todo[@]}" >&2
done

# Divergences: the interpreter accepts every suite program, so any non-OK is one.
grep -vP '^OK\t' "$PROGRESS" | sort -t$'\t' -k2 -u > "$OUT"
{
  echo "=== Maude ($SPEC_DIR) verdicts over $(wc -l < "$PROGRESS") classified suite programs ==="
  cut -f1 "$PROGRESS" | sort | uniq -c | sort -rn
  echo "=== $(wc -l < "$OUT") DIVERGING (interp-PASS, Maude-not-OK) written to $OUT ==="
} >&2
