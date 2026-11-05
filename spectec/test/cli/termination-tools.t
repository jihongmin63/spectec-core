Each selected tool gets exactly one execution, even after another tool proves
termination. The process fixtures exercise the external tool protocol.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ cat > aprove <<'SH'
  > #!/bin/sh
  > printf 'aprove\n' >> calls
  > test "$2" = 2 || exit 9
  > printf 'YES\nproof of input.trs\n(0) YES\n----------------------------------------\n'
  > SH
  $ cat > natt <<'SH'
  > #!/bin/sh
  > printf 'natt\n' >> calls
  > test "$2" = -V || exit 9
  > grep -q '<problem><trs><rules>' "$1" || exit 8
  > printf 'YES\n'
  > SH
  $ chmod +x aprove natt
  $ ln -s /bin/true z3
  $ export PATH="$PWD:$PATH"
  $ export SPECTEC_APROVE_BIN="$PWD/aprove" SPECTEC_NATT_BIN="$PWD/natt"
  $ spectec termination --tool aprove --tool natt --timeout 2 --symbol '$lookup_id_type' --out results.tsv --proof-dir proofs $SPEC 2>/dev/null | cut -f1-3
  $lookup_id_type	aprove	YES
  $lookup_id_type	natt	YES
  $ cat calls
  aprove
  natt
  $ spectec termination --tool aprove --tool natt --timeout 2 --symbol '$lookup_id_type' --out results.tsv --proof-dir proofs $SPEC 2>/dev/null
  $ wc -l < calls | tr -d ' '
  2
  $ spectec termination --tool aprove --tool natt --timeout 3 --symbol '$lookup_id_type' --out results.tsv $SPEC > changed.out 2>/dev/null
  [1]
  $ grep -o 'configuration mismatch' changed.out
  configuration mismatch

No global answer is inferred from stderr or a child proof; failed runs never
turn into a successful resume.

  $ cat > aprove <<'SH'
  > #!/bin/sh
  > printf 'KILLED\nproof of input.trs\n(0)\nYES\n----------------------------------------\n'
  > SH
  $ spectec termination --tool aprove --timeout 2 --symbol '$lookup_id_type' --out failed.tsv $SPEC 2>/dev/null | cut -f1-3
  $lookup_id_type	aprove	TIMEOUT
  $ spectec termination --tool aprove --timeout 2 --symbol '$lookup_id_type' --out failed.tsv $SPEC >/dev/null 2>/dev/null
  [1]
  $ cat > natt <<'SH'
  > #!/bin/sh
  > printf 'YES\n' >&2
  > exit 7
  > SH
  $ spectec termination --tool natt --timeout 2 --symbol '$lookup_id_type' $SPEC 2>/dev/null | cut -f1-3
  $lookup_id_type	natt	ERROR

Invalid options fail before invoking any tool.

  $ spectec termination --tool natt --timeout 0 --symbol '$lookup_id_type' $SPEC
  --timeout must be positive
  [2]
  $ spectec termination --tool natt --tool natt --symbol '$lookup_id_type' $SPEC
  --tool names must be unique
  [2]
  $ spectec termination --budget 2 --symbol '$lookup_id_type' $SPEC >/dev/null 2>&1
  [1]

Resume refuses changed tool contents and truncated files before executing jobs.

  $ cp results.tsv old.tsv
  $ spectec termination --tool aprove --tool natt --timeout 2 --symbol '$lookup_id_type' --out old.tsv $SPEC > version.out 2>/dev/null
  [1]
  $ grep -o 'configuration mismatch' version.out
  configuration mismatch
  $ printf 'partial' > truncated.tsv
  $ spectec termination --tool aprove --timeout 2 --symbol '$lookup_id_type' --out truncated.tsv $SPEC > truncated.out 2>/dev/null
  [1]
  $ grep -o 'incomplete final row' truncated.out
  incomplete final row
