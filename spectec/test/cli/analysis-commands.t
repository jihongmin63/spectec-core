Analysis dumps over the impty base spec: the signature prune. Everything here
is translation-only (no Maude), so the snapshots are stable.

  $ SPEC=../../specs/impty/base/spec.spectec

--prune-signature drops the op declarations the slice's rules never use; the
rule lines themselves are untouched:

  $ spectec rewrite --ctrs --symbol '$lookup' $SPEC | grep -c '^  op '
  reflect: subty expansion: 6 clause(s) -> 6 clone(s) (4 dead, 0 vacuous guard(s) dropped)
  reflect: 1 owise rule(s) reflected, 0 complement-enumerated, 0 kept
  32
  $ spectec rewrite --ctrs --symbol '$lookup' --prune-signature $SPEC | grep -c '^  op '
  reflect: subty expansion: 6 clause(s) -> 6 clone(s) (4 dead, 0 vacuous guard(s) dropped)
  reflect: 1 owise rule(s) reflected, 0 complement-enumerated, 0 kept
  17
  $ spectec rewrite --ctrs --symbol '$lookup' $SPEC | grep -E '^  c?eq ' > full.eqs
  reflect: subty expansion: 6 clause(s) -> 6 clone(s) (4 dead, 0 vacuous guard(s) dropped)
  reflect: 1 owise rule(s) reflected, 0 complement-enumerated, 0 kept
  $ spectec rewrite --ctrs --symbol '$lookup' --prune-signature $SPEC | grep -E '^  c?eq ' > pruned.eqs
  reflect: subty expansion: 6 clause(s) -> 6 clone(s) (4 dead, 0 vacuous guard(s) dropped)
  reflect: 1 owise rule(s) reflected, 0 complement-enumerated, 0 kept
  $ diff full.eqs pruned.eqs
