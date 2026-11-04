The runner accepts only raw native starts and P4 input. Removed modes and
exports are rejected by the command parser.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ spectec run --structural --start 'true' $SPEC
  --structural requires --p4 input and excludes --start
  [2]
  $ spectec run --start 'true' --check-p4 $SPEC
  --check-p4 requires --p4
  [2]
  $ spectec rewrite --symbol '$lookup_id_type' $SPEC
  --symbol requires --ctrs
  [2]
  $ spectec termination --emit --all
  --emit takes exactly one --symbol
  [2]

A Maude stand-in validates the environment-variable binary and attribution of
one bad P4 source beside a runnable raw start.

  $ cat > maude-stub <<'SH'
  > #!/bin/sh
  > cp "$2" maude-input
  > printf 'invoked\n' >> invocations
  > sed -n '/^reduce .*SPECTEC-BATCH-SEP/ { s/.*/result Bool: true/p; s/.*/result String: "$$SPECTEC-BATCH-SEP$$"/p; }' "$2"
  > SH
  $ chmod +x maude-stub
  $ export SPECTEC_MAUDE_BIN="$PWD/maude-stub"
  $ cat > good.p4 <<'P4'
  > action a() { ; }
  > P4
  $ printf '@\n' > bad.p4
  $ spectec run --p4 good.p4 --p4 bad.p4 --start "'true.Bool" $SPEC 2>/dev/null | sed -n '/^===/p; /^result:/p; /^ERROR:/p'
  === good.p4 ===
  result: true
  === bad.p4 ===
  ERROR: error: syntax error
  === --start ===
  result: true
  $ wc -l < invocations | tr -d ' '
  1
  $ grep -c 'metaReduce' maude-input
  2
  $ spectec run --structural --p4 good.p4 --p4 bad.p4 $SPEC 2>/dev/null | sed -n '/^===/p; /^result:/p; /^ERROR:/p'
  === good.p4 ===
  result: true
  === bad.p4 ===
  ERROR: error: syntax error
  $ grep -c '^reduce Program-ok' maude-input
  1
  $ spectec run --p4 bad.p4 --start "'true.Bool" $SPEC >/dev/null 2>/dev/null
  [1]
  $ spectec run --start "'true.Bool" $SPEC
  result: true

Termination resumes each symbol/tool row, including empty slices. The header
binds the file to this specification and tool configuration.

  $ export SPECTEC_APROVE_BIN=/bin/false
  $ spectec termination --symbol '$missing-a' --symbol '$missing-b' --out rows.tsv $SPEC 2>/dev/null | cut -f1-3
  $missing-a	aprove	DEGENERATE
  $missing-b	aprove	DEGENERATE
  $ spectec termination --symbol '$missing-a' --symbol '$missing-b' --out rows.tsv $SPEC 2>/dev/null
  $ spectec termination --symbol '$missing-a' --symbol '$missing-c' --out rows.tsv $SPEC 2>/dev/null | cut -f1-3
  $missing-c	aprove	DEGENERATE
  $ wc -l < rows.tsv | tr -d ' '
  4
