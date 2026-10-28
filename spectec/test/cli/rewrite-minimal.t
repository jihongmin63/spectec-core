The rewrite CLI keeps a size-ranked symbol list and rejects removed entry
points and options.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ spectec rewrite --list-symbols $SPEC 2>/dev/null | head -2
  $lookup_id_type	19
  $lookup_id_value	19

  $ spectec scc --help >/dev/null 2>&1
  [1]
  $ spectec rewrite --sizes $SPEC >/dev/null 2>&1
  [1]
  $ spectec run --emit $SPEC >/dev/null 2>&1
  [1]
  $ spectec termination --itrs --symbol '$no-such-symbol' $SPEC >/dev/null 2>&1
  [1]

The structural executor requires P4 input; raw starts use the native executor.

  $ spectec run --structural --start 'true' $SPEC
  --structural requires --p4 input and excludes --start
  [2]
