#!/usr/bin/env bash
# check_diff_p4.sh — full COMPLETENESS + SOUNDNESS differential review of the
# Maude rewrite backend against the reference interpreter, over the WHOLE P4
# corpus (p4_16_samples positives + p4_16_errors negatives).
#
# SAME-SPEC oracle: BOTH the interpreter (`p4 typecheck`) and Maude (`run --p4`)
# now run `specs/p4`, so every divergence is a PURE translation bug (no
# p4-old-vs-new-spec confound, no per-file triage). The cross-table:
#   completeness gap = interp PASS  & Maude not OK   (Maude under-accepts)
#   soundness    gap = interp FAIL  & Maude OK       (Maude over-accepts)
#
# Self-contained: the Maude phase batches the corpus through `run --p4` (one
# ~50k-line module parse per CHUNK, not per program) and classifies each
# program's `=== <file> ===` block. RESUMABLE: both phases skip programs already
# recorded in their progress TSV, so a re-run continues where it stopped (Ctrl-C,
# a crash, or a transient death from a concurrent `dune build` are all safe).
# RUN IN A CLEAN ENVIRONMENT (no concurrent dune build, no other Maude job — two
# Maude runs exhaust RAM and corrupt output). Maude runs SERIAL.
#
# Env knobs: ITIMEOUT (per-file interp + per-file Maude-fallback timeout, default
# 300 — see note), CHUNK (programs per Maude batch), BATCH_TIMEOUT (whole-chunk
# Maude timeout), IPROG/MPROG/COMP/SOUND (output paths).
#
# NOTE on ITIMEOUT=300: some valid programs (e.g. psa-example-dpdk-*) take well
# over a minute to typecheck. Keep this generous or slow-but-valid programs are
# spuriously dropped (interp FALSE-FAIL) and mis-classified as noise.
set -u
cd "$(dirname "$0")"

EXE=spectec/_build/default/bin/main.exe
INC=spectec/testdata/interp/p4/p4c/includes
SAMPLES=spectec/testdata/interp/p4/p4c/p4_16_samples
ERRORS=spectec/testdata/interp/p4/p4c/p4_16_errors
SPEC=$(find spectec/specs/p4 -name '*.spectec' | sort | tr '\n' ' ')

IPROG=${IPROG:-check_diff_p4_interp.tsv}        # interp verdicts: PASS/FAIL \t path
MPROG=${MPROG:-check_diff_p4_maude.tsv}         # maude verdicts: OK/STUCK/... \t path
COMP=${COMP:-check_diff_p4_completeness.tsv}    # completeness gaps
SOUND=${SOUND:-check_diff_p4_soundness.tsv}     # soundness gaps
ITIMEOUT=${ITIMEOUT:-300}
CHUNK=${CHUNK:-30}
BATCH_TIMEOUT=${BATCH_TIMEOUT:-1800}

[ -x "$EXE" ] || { echo "build first: (cd spectec && opam exec --switch=spectecx -- dune build bin/main.exe)" >&2; exit 1; }

CORPUS=$(mktemp)
cat <(ls "$SAMPLES"/*.p4) <(ls "$ERRORS"/*.p4) | sort -u > "$CORPUS"
total=$(wc -l < "$CORPUS")

# --- Phase A: interpreter verdicts (resumable; skip files already recorded) ---
touch "$IPROG"
echo "[A] interpreter verdicts (timeout ${ITIMEOUT}s) over $total programs ..." >&2
while IFS= read -r f; do
  awk -F'\t' -v p="$f" '$2==p{ok=1} END{exit !ok}' "$IPROG" && continue
  if timeout "$ITIMEOUT" "$EXE" p4 typecheck -p "$f" -i "$INC" 2>&1 | grep -q 'Typechecker succeeded'; then
    printf 'PASS\t%s\n' "$f" >> "$IPROG"
  else
    printf 'FAIL\t%s\n' "$f" >> "$IPROG"
  fi
done < "$CORPUS"
echo "[A] done: $(grep -c $'^PASS\t' "$IPROG") PASS / $(grep -c $'^FAIL\t' "$IPROG") FAIL" >&2

# --- Phase B: Maude (specs/p4) verdicts, batched + resumable ---
# OK = reduced to a typing result (`result:`); STUCK = `FAIL (stuck)`; ERROR =
# front-end/parse failure; OTHER = anything else; TIMEOUT = per-file fallback hit.
classify_text() {
  if   printf '%s' "$1" | grep -q  "FAIL (stuck)"; then echo STUCK
  elif printf '%s' "$1" | grep -qE "^result:";     then echo OK
  elif printf '%s' "$1" | grep -qiE "parse error|syntax error|Sys_error|Failure|exception|Fatal error|^ERROR:"; then echo ERROR
  else echo OTHER; fi
}
classify_one() {  # one program via its own run (the per-file fallback)
  local f="$1" raw rc
  raw=$(timeout "$ITIMEOUT" "$EXE" run --p4 "$f" -i "$INC" $SPEC 2>&1); rc=$?
  if [ "$rc" -eq 124 ]; then printf 'TIMEOUT\t%s\n' "$f"
  else printf '%s\t%s\n' "$(classify_text "$raw")" "$f"; fi
}
classify_chunk() {  # a whole chunk in ONE invocation; split per `=== file ===`
  local files=("$@") args=() f raw rc
  for f in "${files[@]}"; do args+=(--p4 "$f"); done
  raw=$(timeout "$BATCH_TIMEOUT" "$EXE" run "${args[@]}" -i "$INC" --timeout 0 $SPEC 2>&1); rc=$?
  if [ "$rc" -eq 124 ]; then
    for f in "${files[@]}"; do classify_one "$f"; done; return
  fi
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
    for f in "${files[@]}"; do classify_one "$f"; done; return
  fi
  local i
  for i in "${!files[@]}"; do printf '%s\t%s\n' "${verdicts[$i]}" "${files[$i]}"; done
}

touch "$MPROG"
mapfile -t todo < <(while IFS= read -r f; do
  awk -F'\t' -v p="$f" '$2==p{ok=1} END{exit !ok}' "$MPROG" || echo "$f"
done < "$CORPUS")
echo "[B] Maude (specs/p4) verdicts, batched (CHUNK=$CHUNK): $(( total - ${#todo[@]} )) done, ${#todo[@]} to run ..." >&2
i=0
while [ "$i" -lt "${#todo[@]}" ]; do
  chunk=("${todo[@]:i:CHUNK}")
  classify_chunk "${chunk[@]}" >> "$MPROG"
  i=$(( i + CHUNK ))
  echo "  classified $(( i < ${#todo[@]} ? i : ${#todo[@]} )) / ${#todo[@]}" >&2
done
echo "[B] done: $(wc -l < "$MPROG") / $total classified" >&2

# --- Phase C: cross-tabulate ---
join -t$'\t' -1 2 -2 2 \
  <(sort -t$'\t' -k2 "$IPROG") <(sort -t$'\t' -k2 "$MPROG") \
  > /tmp/check_diff_p4_joined.tsv
awk -F'\t' '$2=="PASS" && $3!="OK" { print $3 "\t" $1 }' /tmp/check_diff_p4_joined.tsv | sort > "$COMP"
awk -F'\t' '$2=="FAIL" && $3=="OK" { print $1 }'        /tmp/check_diff_p4_joined.tsv | sort > "$SOUND"

{
  echo "=== diff review: interp(p4) vs Maude(p4) over $total programs ==="
  echo "interp:  $(grep -c $'^PASS\t' "$IPROG") PASS / $(grep -c $'^FAIL\t' "$IPROG") FAIL"
  echo "maude:"; cut -f1 "$MPROG" | sort | uniq -c | sort -rn | sed 's/^/   /'
  echo "COMPLETENESS gaps (interp PASS, Maude not OK): $(wc -l < "$COMP")  -> $COMP"
  echo "SOUNDNESS    gaps (interp FAIL, Maude OK):     $(wc -l < "$SOUND") -> $SOUND"
} >&2
rm -f "$CORPUS"
