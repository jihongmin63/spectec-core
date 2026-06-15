#!/usr/bin/env bash
# diff_review.sh — full completeness + soundness differential review of the
# Maude rewrite backend against the reference interpreter, over the WHOLE P4
# corpus (p4_16_samples positives + p4_16_errors negatives).
#
# Oracle: the interpreter runs the NEW spec (`specs/p4`, the only working
# interpreter); Maude runs `specs/p4-old` (the executable baseline -- `specs/p4`
# still sticks on the typing/execution frontier, see lib/rewrite/CLAUDE.md). So
# this is an interp(p4) vs Maude(p4-old) comparison; a mismatch is EITHER a
# translation bug OR a p4-old-vs-new-spec difference (triage per file with the
# new-spec interpreter, never `--spec-dir p4-old`). The cross-table:
#   completeness gap = interp PASS  & Maude not OK   (Maude under-accepts)
#   soundness    gap = interp FAIL  & Maude OK       (Maude over-accepts)
#
# RESUMABLE. Both phases skip programs already recorded in their progress TSV,
# so re-running continues where it stopped (Ctrl-C, a crash, or a transient
# death from a concurrent `dune build` swapping the exe are all safe). Phase B
# auto-retries find_maude_diverging.sh until the corpus is fully classified or
# no progress is made. RUN IN A CLEAN ENVIRONMENT (no concurrent dune build, no
# other Maude job -- two Maude runs exhaust RAM and corrupt output).
#
# Env knobs: ITIMEOUT (per-file interp+maude-fallback timeout, default 300 --
# see note), CHUNK (programs per Maude batch), RETRIES (Phase B resume attempts),
# IPROG/MPROG/COMP/SOUND (output paths).
#
# NOTE on ITIMEOUT=300: some valid programs (e.g. psa-example-dpdk-*) take well
# over a minute to typecheck. The 2026-06-14 suite was cut at an effective ~60s
# and spuriously dropped ~150 slow-but-valid programs (interp FALSE-FAIL), which
# would mis-classify them as soundness/coverage noise here. Keep this generous.
set -u
cd "$(dirname "$0")"

EXE=spectec/_build/default/bin/main.exe
INC=spectec/testdata/interp/p4/p4c/includes
SAMPLES=spectec/testdata/interp/p4/p4c/p4_16_samples
ERRORS=spectec/testdata/interp/p4/p4c/p4_16_errors

IPROG=${IPROG:-diff_review_interp.tsv}          # interp verdicts: PASS/FAIL \t path
MPROG=${MPROG:-diff_review_maude.tsv}           # maude verdicts: OK/STUCK/... \t path
COMP=${COMP:-diff_review_completeness.tsv}      # completeness gaps
SOUND=${SOUND:-diff_review_soundness.tsv}       # soundness gaps
ITIMEOUT=${ITIMEOUT:-300}
CHUNK=${CHUNK:-30}
RETRIES=${RETRIES:-100}

[ -x "$EXE" ] || { echo "build first: (cd spectec && opam exec --switch=spectecx -- dune build bin/main.exe)" >&2; exit 1; }
[ -x ./find_maude_diverging.sh ] || { echo "missing find_maude_diverging.sh" >&2; exit 1; }

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

# --- Phase B: Maude (p4-old) verdicts (resumable, batched, auto-retry) ---
# find_maude_diverging.sh records EVERY verdict (OK + non-OK) to its PROGRESS,
# chunk-granular and resumable, so it is reused verbatim as the Maude engine.
echo "[B] Maude (p4-old) verdicts, batched (CHUNK=$CHUNK) with auto-resume ..." >&2
n=0
while [ "$(wc -l < "$MPROG" 2>/dev/null || echo 0)" -lt "$total" ] && [ "$n" -lt "$RETRIES" ]; do
  before=$(wc -l < "$MPROG" 2>/dev/null || echo 0)
  SUITE="$CORPUS" PROGRESS="$MPROG" OUT=/tmp/diff_review_div.tsv \
    SPEC_DIR=spectec/specs/p4-old CHUNK="$CHUNK" BATCH_TIMEOUT=1800 TIMEOUT="$ITIMEOUT" \
    ./find_maude_diverging.sh
  after=$(wc -l < "$MPROG" 2>/dev/null || echo 0)
  n=$((n + 1))
  [ "$after" -le "$before" ] && { echo "[B] no progress on attempt $n -- stopping (a chunk may be deterministically failing)" >&2; break; }
done
echo "[B] done: $(wc -l < "$MPROG") / $total classified" >&2

# --- Phase C: cross-tabulate ---
# join interp (col2=path) with maude (col2=path) -> path \t interp \t maude
join -t$'\t' -1 2 -2 2 \
  <(sort -t$'\t' -k2 "$IPROG") <(sort -t$'\t' -k2 "$MPROG") \
  > /tmp/diff_review_joined.tsv
awk -F'\t' '$2=="PASS" && $3!="OK"  { print $3 "\t" $1 }' /tmp/diff_review_joined.tsv | sort > "$COMP"
awk -F'\t' '$2=="FAIL" && $3=="OK" { print $1 }'         /tmp/diff_review_joined.tsv | sort > "$SOUND"

{
  echo "=== diff review: interp(p4) vs Maude(p4-old) over $total programs ==="
  echo "interp:  $(grep -c $'^PASS\t' "$IPROG") PASS / $(grep -c $'^FAIL\t' "$IPROG") FAIL"
  echo "maude:"; cut -f1 "$MPROG" | sort | uniq -c | sort -rn | sed 's/^/   /'
  echo "COMPLETENESS gaps (interp PASS, Maude not OK): $(wc -l < "$COMP")  -> $COMP"
  echo "SOUNDNESS    gaps (interp FAIL, Maude OK):     $(wc -l < "$SOUND") -> $SOUND"
} >&2
rm -f "$CORPUS"
