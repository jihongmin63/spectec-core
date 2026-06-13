#!/usr/bin/env bash
#
# rewrite-time.sh — measure the *pure Maude rewriting* time of a `run` command,
# stripped of the OCaml front-end (spec parse + elaboration), module emission,
# and the Maude start-up / module-parsing cost that dominate end-to-end
# wall-clock (see lib/rewrite/CLAUDE.md "성능 측정 시 주의").
#
# How it works: `Maude_run.run` writes one self-contained temp file holding the
# emitted module + the `reduce`/`rewrite`/`search` command + `quit`, then runs
# `maude -no-banner <file>`. We pass a `--maude-bin` wrapper that copies that
# file aside, let the normal run finish, then re-run the real maude on the
# captured file ourselves: `maude` always prints `rewrites: N in Xms cpu
# (Yms real)`, which `Maude_run.parse_output` discards. That stats line is the
# pure rewriting cost — independent of front-end and start-up.
#
# Usage:
#   rewrite-time.sh [-n REPS] -- <main.exe> run --p4 FILE -i INC <spec files...>
#   rewrite-time.sh [-n REPS] -- <main.exe> run --imp FILE <spec files...>
#
# Everything after `--` is the run command, forwarded verbatim (we only append
# `--maude-bin <wrapper>` and a generous `--timeout`). -n REPS (default 1)
# re-runs the captured module that many times and reports the minimum cpu/real
# ms (most representative of the pure cost; the count is identical each rep).
#
# Real maude binary: $SPECTEC_MAUDE_BIN, else repo-relative
# spectec/tools/maude/maude, else `maude` on PATH.
set -euo pipefail

reps=1
if [ "${1:-}" = "-n" ]; then reps="$2"; shift 2; fi
if [ "${1:-}" = "--" ]; then shift; fi
if [ "$#" -lt 2 ]; then
  sed -n '2,30p' "$0"; exit 2
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
echo "# end-to-end wall-clock (front-end + emission + maude): $(( (end-start)/1000000 ))ms" >&2

if [ ! -s "$captured" ]; then
  echo "error: no Maude module was captured (did the run reach maude? is --emit set?)" >&2
  exit 1
fi

# Re-run the captured self-contained module directly: pure rewriting only.
best_cpu="" best_real="" count=""
for _ in $(seq 1 "$reps"); do
  line=$("$real_maude" -no-banner "$captured" 2>/dev/null \
         | grep -m1 -E '^rewrites:' || true)
  [ -n "$line" ] || { echo "error: no 'rewrites:' line from maude" >&2; exit 1; }
  # line: rewrites: N in Xms cpu (Yms real) (...)
  count=$(echo "$line" | sed -E 's/^rewrites: ([0-9]+) .*/\1/')
  cpu=$(echo "$line"   | sed -E 's/.* in ([0-9]+)ms cpu .*/\1/')
  rl=$(echo "$line"    | sed -E 's/.*\(([0-9]+)ms real\).*/\1/')
  if [ -z "$best_cpu" ] || [ "$cpu" -lt "$best_cpu" ]; then best_cpu="$cpu"; fi
  if [ -z "$best_real" ] || [ "$rl" -lt "$best_real" ]; then best_real="$rl"; fi
done

echo "rewrites: $count   pure-cpu(min of $reps): ${best_cpu}ms   pure-real(min): ${best_real}ms"
