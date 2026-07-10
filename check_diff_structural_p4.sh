#!/usr/bin/env bash
# check_diff_structural_p4.sh — differential review of the STRUCTURAL oracle
# (`run-structural`, the CTRS/Maude analysis surface run as real ground-term
# reduction) against the reference interpreter, over the whole P4 corpus.
#
# This is check_diff_p4.sh's sibling for the THIRD oracle leg (todo.md's
# "CTRS(구조적) differential"), not a replacement: check_diff_p4.sh compares
# interp vs the NATIVE execution module (to_maude.ml, machine arithmetic,
# META-TERM reflection); this compares interp vs the STRUCTURAL analysis
# module (to_mfe.ml, the SAME module the Church-Rosser/coherence checker
# verifies) executed directly. A divergence here is a bug in the ANALYSIS
# surface itself -- exactly the surface CRC/ChC assumes is semantically
# faithful but never actually runs to a concrete answer.
#
# Interp verdicts don't depend on which Maude oracle they're compared
# against, so this reuses check_diff_p4.sh's completed IPROG file as-is by
# default (IPROG=check_diff_p4_interp.tsv) -- Phase A should need no new work.
#
# KNOWN, MATERIAL FAILURE MODE (read before running the whole corpus): the
# Structural theory encodes nat/int as Peano unary (`succ(succ(...zero))`),
# fine for the CRC (which reasons about rules symbolically via unification
# and never builds a concrete number this large) but not for ground execution
# of real arithmetic -- `bit<32>` needs 2^32 (~4.3 BILLION) nested `succ`
# terms to represent a single value; `bit<64>` is astronomically worse. Most
# real P4 headers use bit<8/16/32/48/64>, so this is not a corner case: a
# large share of the corpus is expected to OOM (Maude killed by the OS/cgroup
# memory limit, not a hang -- confirmed empirically, exit 137) rather than
# produce a real STUCK/OK verdict. That is a REPRESENTATION limit, not a
# logic bug (see commit 2583c31f) -- fixing it for real means giving the
# Structural theory a binary numeral encoding, a separate, larger undertaking
# not attempted here. Two containment measures below exist BECAUSE of this:
#   - CHUNK defaults far smaller than check_diff_p4.sh's (an OOM kills the
#     whole batched Maude process, not just the one offending program, so a
#     small chunk bounds how much gets lost per crash before per-file retry);
#   - MEMLIMIT_KB caps each Maude invocation's virtual memory (`ulimit -v`)
#     so a runaway Peano computation dies fast and cleanly instead of
#     pressuring the whole container (shared with other work).
# A program hitting this shows up as its own OOM verdict (see classify_text),
# not as noise contaminating everything else -- resumable, same as Phase B/D.
#
# Every Maude invocation also runs with `ulimit -s unlimited` -- caught by an
# actual run over the real corpus, not anticipated up front: the default 8MB
# stack is too small for a lot of legitimately-deep (not runaway) reductions,
# so without this a large share of what should be real STUCK/OK verdicts came
# back as a native "Fatal error: stack overflow" instead, inflating ERROR.
# classify_text/classify_chunk still recognize that string as its own
# STACKOVERFLOW verdict (distinct from OOM/ERROR) for whatever still hits it
# even unlimited (a genuinely runaway computation, not a stack-size problem).
#
# RUN IN A CLEAN ENVIRONMENT (no concurrent dune build, no other Maude job).
# Maude runs SERIAL. See check_diff_p4.sh's own header for the resumability/
# TSV-format conventions this mirrors exactly.
#
# Env knobs: ITIMEOUT (per-file timeout, default 60 -- see note below on why
# this is smaller than check_diff_p4.sh's 300), CHUNK (programs per Maude
# batch, default 10), BATCH_TIMEOUT (whole-chunk timeout), MEMLIMIT_KB (per-
# Maude-invocation `ulimit -v`, default ~4GB), IPROG/MPROG/COMP/SOUND/RESMATCH
# (output paths).
#
# NOTE on ITIMEOUT=60 (vs check_diff_p4.sh's 300): a slow-but-valid program
# there is genuinely computing; here, a program that hasn't finished in 60s
# is far more likely climbing the Peano wall than doing legitimate deep
# recursion (confirmed: pow2(8)=2s, pow2(32) alone didn't finish in 20s) --
# waiting longer mostly delays reaching the OOM everyone already expects.
# Raise it if legitimate small-width programs start showing spurious TIMEOUT.
set -u
cd "$(dirname "$0")"

EXE=spectec/_build/default/bin/main.exe
INC=spectec/testdata/interp/p4/p4c/includes
SAMPLES=spectec/testdata/interp/p4/p4c/p4_16_samples
ERRORS=spectec/testdata/interp/p4/p4c/p4_16_errors
SPEC=$(find spectec/specs/p4 -name '*.spectec' | sort | tr '\n' ' ')

IPROG=${IPROG:-check_diff_p4_interp.tsv}                    # reused as-is from check_diff_p4.sh
MPROG=${MPROG:-check_diff_p4_structural_maude.tsv}
COMP=${COMP:-check_diff_p4_structural_completeness.tsv}
SOUND=${SOUND:-check_diff_p4_structural_soundness.tsv}
RESMATCH=${RESMATCH:-check_diff_p4_structural_resultmatch.tsv}
RESCHUNK=${RESCHUNK:-50}
ITIMEOUT=${ITIMEOUT:-60}
CHUNK=${CHUNK:-50}
BATCH_TIMEOUT=${BATCH_TIMEOUT:-600}
MEMLIMIT_KB=${MEMLIMIT_KB:-4000000}   # ~4GB virtual memory per Maude invocation
# Per-program Maude reduction cap (the run-structural --timeout, applied to each
# start term INSIDE a batch, not to the whole invocation). Default 0 = unbounded
# so every reduction runs to a real normal form -- required for a trustworthy
# result-value comparison (a clipped reduction would report a spurious
# STUCK/MISMATCH). Speed instead comes from a large CHUNK/RESCHUNK: the ~1min
# module-load fixed cost is paid once per batch and amortized over many programs.
# Set a finite value only to bound a batch against a genuinely non-terminating
# program (BATCH_TIMEOUT is the outer guard either way).
REDUCE_TIMEOUT=${REDUCE_TIMEOUT:-0}
# Phase C's join scratch file. Made overridable so several instances (each on a
# disjoint CORPUS_LIST shard, with its own MPROG/RESMATCH/COMP/SOUND) can run in
# parallel without clobbering each other's cross-tab.
JOINED=${JOINED:-/tmp/check_diff_p4_structural_joined.tsv}

[ -x "$EXE" ] || { echo "build first: (cd spectec && opam exec --switch=spectecx -- dune build bin/main.exe)" >&2; exit 1; }

CORPUS=$(mktemp)
# CORPUS_LIST lets a caller restrict the run to a preselected set of programs
# (e.g. only the interp-PASS files, for a fast result-value-match-first pass, or
# only the interp-FAIL files, for the soundness leg) instead of the whole
# corpus. The default is the full samples+errors set, sorted-unique.
if [ -n "${CORPUS_LIST:-}" ]; then
  sort -u "$CORPUS_LIST" > "$CORPUS"
else
  cat <(ls "$SAMPLES"/*.p4) <(ls "$ERRORS"/*.p4) | sort -u > "$CORPUS"
fi
total=$(wc -l < "$CORPUS")

fmt_dur() { local s=$1; printf '%dh %02dm %02ds' $((s/3600)) $((s%3600/60)) $((s%60)); }
t_run0=$(date +%s)

# --- Phase A: interpreter verdicts (reused from check_diff_p4.sh, resumable) ---
touch "$IPROG"
tA0=$(date +%s)
echo "[A] interpreter verdicts (timeout ${ITIMEOUT}s) over $total programs ..." >&2
while IFS= read -r f; do
  awk -F'\t' -v p="$f" '$2==p{ok=1} END{exit !ok}' "$IPROG" && continue
  ts=$(date +%s)
  if timeout "$ITIMEOUT" "$EXE" p4 typecheck -p "$f" -i "$INC" 2>&1 | grep -q 'Typechecker succeeded'; then v=PASS; else v=FAIL; fi
  printf '%s\t%s\t%d\n' "$v" "$f" $(( $(date +%s) - ts )) >> "$IPROG"
done < "$CORPUS"
tA1=$(date +%s); dur_A=$((tA1-tA0))
echo "[A] done: $(grep -c $'^PASS\t' "$IPROG") PASS / $(grep -c $'^FAIL\t' "$IPROG") FAIL [$(fmt_dur $dur_A)]" >&2

# --- Phase B: Structural (run-structural) verdicts, batched + resumable ---
# OK = reduced to a typing result (`result:`); STUCK = `FAIL (stuck)`; OOM =
# Maude killed by a signal (the OS/cgroup memory limit -- the Peano-width
# wall, see header); ERROR = front-end/parse failure or any other Maude
# process failure; OTHER = anything else; TIMEOUT = per-file fallback hit.
classify_text() {
  if   printf '%s' "$1" | grep -qE "exited with status 1[3-9][0-9]|killed by signal"; then echo OOM
  elif printf '%s' "$1" | grep -qi "stack overflow"; then echo STACKOVERFLOW
  elif printf '%s' "$1" | grep -q  "FAIL (stuck)"; then echo STUCK
  elif printf '%s' "$1" | grep -qE "^result:";     then echo OK
  elif printf '%s' "$1" | grep -qiE "parse error|syntax error|Sys_error|Failure|exception|Fatal error|^ERROR:"; then echo ERROR
  else echo OTHER; fi
}
classify_one() {  # one program via its own run (the per-file fallback)
  local f="$1" raw rc
  raw=$(ulimit -v "$MEMLIMIT_KB"; ulimit -s unlimited; timeout "$ITIMEOUT" "$EXE" run-structural --p4 "$f" -i "$INC" --timeout "$REDUCE_TIMEOUT" $SPEC 2>&1); rc=$?
  if [ "$rc" -eq 124 ]; then printf 'TIMEOUT\t%s\n' "$f"
  else printf '%s\t%s\n' "$(classify_text "$raw")" "$f"; fi
}
classify_chunk() {  # a whole chunk in ONE invocation; split per `=== file ===`
  local files=("$@") args=() f raw rc
  for f in "${files[@]}"; do args+=(--p4 "$f"); done
  raw=$(ulimit -v "$MEMLIMIT_KB"; ulimit -s unlimited; timeout "$BATCH_TIMEOUT" "$EXE" run-structural "${args[@]}" -i "$INC" --timeout "$REDUCE_TIMEOUT" $SPEC 2>&1); rc=$?
  if [ "$rc" -ne 0 ]; then
    # covers both the shell-level timeout (124) AND the whole batched Maude
    # process dying (OOM or otherwise) -- either way, none of this chunk's
    # results can be trusted, so retry it program-by-program.
    for f in "${files[@]}"; do classify_one "$f"; done; return
  fi
  mapfile -t verdicts < <(printf '%s\n' "$raw" | awk '
    /^=== .* ===$/ { if (seen) emit(); seen=1; block=""; next }
    { block = block $0 "\n" }
    END { if (seen) emit() }
    function emit() {
      if (block ~ /exited with status 1[3-9][0-9]|killed by signal/) print "OOM";
      else if (block ~ /[Ss]tack overflow/) print "STACKOVERFLOW";
      else if (block ~ /FAIL \(stuck\)/) print "STUCK";
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
echo "[B] Structural (run-structural) verdicts, batched (CHUNK=$CHUNK, MEMLIMIT_KB=$MEMLIMIT_KB): $(( total - ${#todo[@]} )) done, ${#todo[@]} to run ..." >&2
tB0=$(date +%s)
i=0
while [ "$i" -lt "${#todo[@]}" ]; do
  chunk=("${todo[@]:i:CHUNK}")
  cs=$(date +%s)
  mapfile -t lines < <(classify_chunk "${chunk[@]}")
  secs=$(( $(date +%s) - cs )); n=${#lines[@]}
  if [ "$n" -gt 0 ]; then
    base=$(( secs / n )); rem=$(( secs % n )); k=0
    for ln in "${lines[@]}"; do
      per=$base; [ "$k" -lt "$rem" ] && per=$(( base + 1 ))
      printf '%s\t%d\n' "$ln" "$per"; k=$(( k + 1 ))
    done >> "$MPROG"
  fi
  i=$(( i + CHUNK ))
  echo "  classified $(( i < ${#todo[@]} ? i : ${#todo[@]} )) / ${#todo[@]} [+$(fmt_dur $secs)]" >&2
done
tB1=$(date +%s); dur_B=$((tB1-tB0))
echo "[B] done: $(wc -l < "$MPROG") / $total classified [$(fmt_dur $dur_B)]" >&2

# --- Phase C: cross-tabulate ---
join -t$'\t' -1 2 -2 2 \
  <(sort -t$'\t' -k2 "$IPROG") <(sort -t$'\t' -k2 "$MPROG") \
  > "$JOINED"
awk -F'\t' '$2=="PASS" && $4!="OK" { print $4 "\t" $1 }' "$JOINED" | sort > "$COMP"
awk -F'\t' '$2=="FAIL" && $4=="OK" { print $1 }'        "$JOINED" | sort > "$SOUND"

# --- Phase D: result-value match (PASS & OK intersection) ---
# Same idea as check_diff_p4.sh's Phase D, but over the (likely much smaller)
# PASS & OK intersection here -- most of the corpus is expected to land in
# STUCK/OOM instead, per the header note.
resultmatch_chunk() {
  local files=("$@") args=() f raw rc
  for f in "${files[@]}"; do args+=(--p4 "$f"); done
  raw=$(ulimit -v "$MEMLIMIT_KB"; ulimit -s unlimited; timeout "$BATCH_TIMEOUT" "$EXE" run-structural "${args[@]}" -i "$INC" --check-p4 --timeout "$REDUCE_TIMEOUT" $SPEC 2>/dev/null); rc=$?
  if [ "$rc" -ne 0 ]; then
    for f in "${files[@]}"; do printf 'TIMEOUT\n'; done; return
  fi
  printf '%s\n' "$raw" | awk '
    /^=== .* ===$/ { if (seen) print v; seen=1; v=""; next }
    /^result: MATCH/        { if (v=="") v="MATCH" }
    /^result: MISMATCH/     { if (v=="") v="MISMATCH" }
    /^result: decode error/ { if (v=="") v="DECODE_ERR" }
    /^result: not reduced/  { if (v=="") v="NOCOMP" }
    /^result: interp FAILED/{ if (v=="") v="INTERP_FAIL" }
    END { if (seen) print (v=="" ? "NOCOMP" : v) }'
}

touch "$RESMATCH"
mapfile -t dtodo < <(awk -F'\t' '$2=="PASS" && $4=="OK" { print $1 }' "$JOINED" | sort | while IFS= read -r f; do
  awk -F'\t' -v p="$f" '$2==p{ok=1} END{exit !ok}' "$RESMATCH" || echo "$f"
done)
dtotal=$(awk -F'\t' '$2=="PASS" && $4=="OK"' "$JOINED" | wc -l)
echo "[D] result-value match over PASS&OK ($dtotal): $(( dtotal - ${#dtodo[@]} )) done, ${#dtodo[@]} to run (RESCHUNK=$RESCHUNK) ..." >&2
tD0=$(date +%s)
di=0
while [ "$di" -lt "${#dtodo[@]}" ]; do
  dchunk=("${dtodo[@]:di:RESCHUNK}")
  mapfile -t dverdicts < <(resultmatch_chunk "${dchunk[@]}")
  if [ "${#dverdicts[@]}" -eq "${#dchunk[@]}" ]; then
    for k in "${!dchunk[@]}"; do printf '%s\t%s\n' "${dverdicts[$k]}" "${dchunk[$k]}"; done >> "$RESMATCH"
  else
    for f in "${dchunk[@]}"; do printf 'UNKNOWN\t%s\n' "$f"; done >> "$RESMATCH"
  fi
  di=$(( di + RESCHUNK ))
  echo "  result-matched $(( di < ${#dtodo[@]} ? di : ${#dtodo[@]} )) / ${#dtodo[@]}" >&2
done
tD1=$(date +%s); dur_D=$((tD1-tD0))
echo "[D] done [$(fmt_dur $dur_D)]" >&2

sum_secs() { awk -F'\t' '{s+=$3} END{print s+0}' "$1"; }
t_iface=$(sum_secs "$IPROG"); t_mface=$(sum_secs "$MPROG")
t_run1=$(date +%s)
{
  echo "=== diff review: interp(p4) vs STRUCTURAL(p4) over $total programs ==="
  echo "interp:  $(grep -c $'^PASS\t' "$IPROG") PASS / $(grep -c $'^FAIL\t' "$IPROG") FAIL"
  echo "structural:"; cut -f1 "$MPROG" | sort | uniq -c | sort -rn | sed 's/^/   /'
  echo "COMPLETENESS gaps (interp PASS, structural not OK): $(wc -l < "$COMP")  -> $COMP"
  echo "SOUNDNESS    gaps (interp FAIL, structural OK):     $(wc -l < "$SOUND") -> $SOUND"
  echo "RESULT-VALUE match (PASS & OK): $(grep -c $'^MATCH\t' "$RESMATCH") MATCH / $(grep -c $'^MISMATCH\t' "$RESMATCH") MISMATCH  -> $RESMATCH"
  rm_other=$(grep -vcE $'^(MATCH|MISMATCH)\t' "$RESMATCH")
  [ "$rm_other" -gt 0 ] && echo "  (result-match other: $rm_other — see DECODE_ERR/NOCOMP/INTERP_FAIL/TIMEOUT/UNKNOWN rows)"
  echo "time (stored per-file sums): interp $(fmt_dur "$t_iface") | structural $(fmt_dur "$t_mface") | sum $(fmt_dur $((t_iface + t_mface)))"
  echo "time (this run wall-clock):  total $(fmt_dur $((t_run1 - t_run0)))"
} >&2
rm -f "$CORPUS"
