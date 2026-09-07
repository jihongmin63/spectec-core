The unified runner validates mode combinations before loading a spec:

  $ spectec run --structural --search --emit
  --structural supports program inputs and equation reduction only
  [2]

  $ spectec run --search --rewrite --emit
  --search and --rewrite are mutually exclusive
  [2]

  $ spectec run --bound 2 --emit
  --bound requires --search
  [2]

  $ spectec rewrite --sizes
  --sizes requires --list-symbols
  [2]

  $ spectec rewrite --ctrs --unconditional --crc-normalize
  --unconditional and --crc-normalize are mutually exclusive
  [2]

The emit spelling is shared with SCC and does not invoke AProVE:

  $ spectec termination --emit --all
  --emit takes exactly one --symbol
  [2]

The rewrite and run commands emit the same native module:

  $ SPEC=../../specs/impty/base/spec.spectec
  $ spectec rewrite $SPEC > native.mod
  $ spectec run --emit $SPEC > run.mod
  $ diff native.mod run.mod

A stand-in for Maude checks batch assembly and result attribution without
requiring an external Maude installation. It logs the actual input file and
answers once for each runnable start term.

  $ cat > maude-stub <<'SH'
  > #!/bin/sh
  > cp "$2" maude-input
  > printf 'invoked\n' >> invocations
  > sed -n '/^reduce .*SPECTEC-BATCH-SEP/ { s/.*/result Bool: true/p; s/.*/result String: "$$SPECTEC-BATCH-SEP$$"/p; }' "$2"
  > SH
  $ chmod +x maude-stub
  $ cat > good.imp <<'IMP'
  > skip
  > IMP
  $ cp good.imp also-good.imp
  $ printf '@\n' > bad.imp

  $ spectec run --imp good.imp --imp bad.imp --imp also-good.imp --maude-bin ./maude-stub $SPEC 2>/dev/null | sed -n '/^===/p; /^result:/p'
  === good.imp ===
  result: true
  === bad.imp ===
  === also-good.imp ===
  result: true
  $ wc -l < invocations | tr -d ' '
  1
  $ grep -c 'metaReduce' maude-input
  2

Structural execution shares input resolution and attribution, using direct
reduction and its existing default task (Check_prog):

  $ spectec run --structural --imp good.imp --imp bad.imp --imp also-good.imp --maude-bin ./maude-stub $SPEC 2>/dev/null | sed -n '/^===/p; /^result:/p'
  === good.imp ===
  result: true
  === bad.imp ===
  === also-good.imp ===
  result: true
  $ wc -l < invocations | tr -d ' '
  2
  $ grep -c '^reduce Check-prog' maude-input
  2

A batch with a bad source still exits non-zero; a single result stays unlabelled:

  $ spectec run --imp good.imp --imp bad.imp --maude-bin ./maude-stub $SPEC >/dev/null 2>/dev/null
  [1]
  $ spectec run --imp good.imp --maude-bin ./maude-stub $SPEC
  result: true

Equal slices reuse the first row and record its source. Resuming skips recorded
symbols and can reuse their result for another symbol in the same sweep:

  $ spectec termination --symbol '$missing-a' --symbol '$missing-b' --out rows.tsv $SPEC 2>/dev/null
  $missing-a	DEGENERATE	-	-	-
  $missing-b	DEGENERATE	-	-	-	=$missing-a
  $ spectec termination --symbol '$missing-a' --symbol '$missing-b' --out rows.tsv $SPEC 2>/dev/null
  $ spectec termination --symbol '$missing-a' --symbol '$missing-c' --out rows.tsv $SPEC 2>/dev/null
  $missing-c	DEGENERATE	-	-	-	=$missing-a
  $ wc -l < rows.tsv | tr -d ' '
  3
