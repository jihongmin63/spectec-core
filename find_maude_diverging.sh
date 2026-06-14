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
# SERIAL by default: each Maude run parses a ~50k-line module, so several at once
# exhaust RAM and corrupt the verdicts (killed processes -> empty output). Set
# JOBS=N to override at your own risk; on an 8-core/16GB box even JOBS=3 corrupts.
# Resumable: re-running skips files already recorded in the progress TSV, so a
# long run can be done in chunks (Ctrl-C and resume).
#
# Env knobs: SUITE, SPEC_DIR (default p4-old), TIMEOUT (s), JOBS, PROGRESS, OUT.
set -u
cd "$(dirname "$0")"

SUITE=${SUITE:-p4_typecheck_suite.txt}
SPEC_DIR=${SPEC_DIR:-spectec/specs/p4-old}
TIMEOUT=${TIMEOUT:-120}
JOBS=${JOBS:-1}
PROGRESS=${PROGRESS:-maude_suite_verdicts.tsv}   # VERDICT\tpath, one per program (resume cache)
OUT=${OUT:-maude_diverging.tsv}                  # the divergences (interp-PASS, Maude-not-OK)

EXE=spectec/_build/default/bin/main.exe
INC=spectec/testdata/interp/p4/p4c/includes
SPEC=$(find "$SPEC_DIR" -name '*.spectec' | sort | tr '\n' ' ')

[ -x "$EXE" ] || { echo "build first: (cd spectec && opam exec --switch=spectecx -- dune build bin/main.exe)" >&2; exit 1; }
[ -f "$SUITE" ] || { echo "missing suite: $SUITE" >&2; exit 1; }

# Classify one program by its Maude `run --p4` outcome. OK = reduced to a typing
# result (`result:`); everything else is a divergence bucket.
classify_one() {
  local f="$1" raw rc
  raw=$(timeout "$TIMEOUT" "$EXE" run --p4 "$f" -i "$INC" $SPEC 2>&1); rc=$?
  if   [ "$rc" -eq 124 ];                                   then printf 'TIMEOUT\t%s\n' "$f"
  elif printf '%s' "$raw" | grep -q  "FAIL (stuck)";        then printf 'STUCK\t%s\n'   "$f"
  elif printf '%s' "$raw" | grep -qE "^result:";            then printf 'OK\t%s\n'      "$f"
  elif printf '%s' "$raw" | grep -qiE "parse error|syntax error|Sys_error|Failure|exception|Fatal error"; then printf 'ERROR\t%s\n' "$f"
  else                                                           printf 'OTHER\t%s\n'   "$f"; fi
}

touch "$PROGRESS"
mapfile -t all < <(grep -v '^#' "$SUITE" | grep -vE '^[[:space:]]*$')
todo=()
for f in "${all[@]}"; do
  awk -F'\t' -v p="$f" '$2==p{ok=1} END{exit !ok}' "$PROGRESS" || todo+=("$f")
done
echo "suite ${#all[@]} | done $(( ${#all[@]} - ${#todo[@]} )) | to run ${#todo[@]} | spec=$SPEC_DIR JOBS=$JOBS timeout=${TIMEOUT}s" >&2

if [ "$JOBS" -gt 1 ]; then
  echo "WARNING: JOBS>1 — parallel Maude runs can OOM and corrupt verdicts." >&2
  export -f classify_one; export EXE INC TIMEOUT SPEC
  printf '%s\n' "${todo[@]}" | xargs -P "$JOBS" -I{} bash -c 'classify_one "$@"' _ {} >> "$PROGRESS"
else
  for f in "${todo[@]}"; do classify_one "$f" >> "$PROGRESS"; done
fi

# Divergences: the interpreter accepts every suite program, so any non-OK is one.
grep -vP '^OK\t' "$PROGRESS" | sort -t$'\t' -k2 -u > "$OUT"
{
  echo "=== Maude ($SPEC_DIR) verdicts over $(wc -l < "$PROGRESS") classified suite programs ==="
  cut -f1 "$PROGRESS" | sort | uniq -c | sort -rn
  echo "=== $(wc -l < "$OUT") DIVERGING (interp-PASS, Maude-not-OK) written to $OUT ==="
} >&2
