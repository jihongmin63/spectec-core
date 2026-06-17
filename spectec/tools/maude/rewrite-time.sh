#!/usr/bin/env bash
#
# rewrite-time.sh — break a `run` command's Maude cost into its phases, stripped
# of the OCaml front-end (spec parse + elaboration) and module emission. The
# end-to-end wall-clock of a small program is dominated NOT by rewriting but by
# Maude parsing (see lib/rewrite/CLAUDE.md "성능 측정 시 주의"); this tool
# attributes the time to each phase so that is visible:
#
#   start-up      Maude loading its own prelude
#   module parse  Maude reading the emitted ~50k-line spec module
#   term parse    Maude parsing the `reduce` command's START TERM against the
#                 module's (huge) mixfix operator grammar -- usually the dominator
#   rewriting     the actual rewriting engine work (Maude's own `rewrites:` stat)
#
# How it works: `Maude_run.run` writes one self-contained temp file (module +
# `reduce`/`rewrite`/`search` + `quit`) and runs `maude -no-banner <file>`. We
# pass a `--maude-bin` wrapper that copies that file aside, then build two cut-down
# variants and time three maude processes (min wall over -n REPS):
#   trivial      just `quit`                         -> start-up
#   parse-only   the module with the commands cut    -> start-up + module parse
#   full         the captured file as-is             -> all four phases
# giving  module parse = parse-only - trivial,  term parse = full - parse-only -
# rewriting,  with rewriting read from the full run's `rewrites: N in _ (Yms
# real)` line (which Maude_run.parse_output otherwise discards).
#
# Usage:
#   rewrite-time.sh [-n REPS] -- <main.exe> run --p4 FILE -i INC <spec files...>
#   rewrite-time.sh [-n REPS] -- <main.exe> run --imp FILE <spec files...>
#
# Everything after `--` is the run command, forwarded verbatim (we append only
# `--maude-bin <wrapper>` and a generous `--timeout`). -n REPS (default 3) takes
# the minimum wall of that many reps per variant (most representative; the
# rewrite count is identical each rep).
#
# Real maude binary: $SPECTEC_MAUDE_BIN, else repo-relative
# spectec/tools/maude/maude, else `maude` on PATH.
set -euo pipefail

reps=3
if [ "${1:-}" = "-n" ]; then reps="$2"; shift 2; fi
if [ "${1:-}" = "--" ]; then shift; fi
if [ "$#" -lt 2 ]; then
  sed -n '2,38p' "$0"; exit 2
fi

# Resolve the real maude binary the same way Maude_run does.
script_dir=$(cd "$(dirname "$0")" && pwd)
real_maude="${SPECTEC_MAUDE_BIN:-}"
if [ -z "$real_maude" ]; then
  if [ -x "$script_dir/maude" ]; then real_maude="$script_dir/maude"
  else real_maude="maude"; fi
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
captured="$work/captured.maude"
wrapper="$work/maude-wrap.sh"

cat > "$wrapper" <<EOF
#!/usr/bin/env bash
# capture the module+command file Maude_run hands to maude, then exec real maude
for a in "\$@"; do
  case "\$a" in *.maude) cp -f "\$a" "$captured" 2>/dev/null || true;; esac
done
exec "$real_maude" "\$@"
EOF
chmod +x "$wrapper"

# Forward the run command, injecting our wrapper and a long internal timeout so
# the capture run itself doesn't get killed before maude writes the file.
echo "# capturing emitted module via a normal run ..." >&2
start=$(date +%s%N)
"$@" --maude-bin "$wrapper" --timeout 600 >/dev/null 2>&1 || true
end=$(date +%s%N)
e2e=$(( (end-start)/1000000 ))
echo "# end-to-end wall-clock (front-end + emission + maude): ${e2e}ms" >&2

if [ ! -s "$captured" ]; then
  echo "error: no Maude module was captured (did the run reach maude? is --emit set?)" >&2
  exit 1
fi

# Cut-down variants: trivial (start-up only) and parse-only (module, no commands).
trivial="$work/trivial.maude"; printf 'quit\n' > "$trivial"
parse_only="$work/parse.maude"
grep -vE '^(reduce|red|rewrite|rew|frewrite|frew|search) ' "$captured" > "$parse_only"
grep -qE '^quit' "$parse_only" || printf 'quit\n' >> "$parse_only"
mod_lines=$(wc -l < "$captured")

# Minimum process wall (ms) over reps for a maude input file.
min_wall() {
  local f="$1" best="" t0 t1 ms
  for _ in $(seq 1 "$reps"); do
    t0=$(date +%s%N); "$real_maude" -no-banner "$f" >/dev/null 2>&1 || true; t1=$(date +%s%N)
    ms=$(( (t1-t0)/1000000 ))
    if [ -z "$best" ] || [ "$ms" -lt "$best" ]; then best="$ms"; fi
  done
  echo "$best"
}

w_start=$(min_wall "$trivial")
w_parse=$(min_wall "$parse_only")
w_full=$(min_wall "$captured")

# Pure rewriting from the full run's own stats (sum the per-command cpu/real ms,
# in case of a batch with several reduces).
rw_line=$("$real_maude" -no-banner "$captured" 2>/dev/null | grep -E '^rewrites:' || true)
[ -n "$rw_line" ] || { echo "error: no 'rewrites:' line from maude" >&2; exit 1; }
rw_count=$(printf '%s\n' "$rw_line" | sed -E 's/^rewrites: ([0-9]+) .*/\1/'        | awk '{s+=$1} END{print s+0}')
rw_cpu=$(printf '%s\n'   "$rw_line" | sed -E 's/.* in ([0-9]+)ms cpu .*/\1/'       | awk '{s+=$1} END{print s+0}')
rw_real=$(printf '%s\n'  "$rw_line" | sed -E 's/.*\(([0-9]+)ms real\).*/\1/'       | awk '{s+=$1} END{print s+0}')

mod_parse=$(( w_parse - w_start )); [ "$mod_parse" -lt 0 ] && mod_parse=0
term_parse=$(( w_full - w_parse - rw_real )); [ "$term_parse" -lt 0 ] && term_parse=0

printf 'module: %s lines   (reps=%s, min wall)\n' "$mod_lines" "$reps"
printf '  Maude start-up (prelude)        : %6sms\n' "$w_start"
printf '  module parse (the spec)         : %6sms\n' "$mod_parse"
printf '  term parse (the start term)     : %6sms\n' "$term_parse"
printf '  rewriting (maude stats)         : %6sms cpu / %sms real   (%s rewrites)\n' "$rw_cpu" "$rw_real" "$rw_count"
printf '  --------------------------------\n'
printf '  full maude process              : %6sms\n' "$w_full"
printf 'end-to-end (incl. OCaml front-end + emission): %sms\n' "$e2e"
