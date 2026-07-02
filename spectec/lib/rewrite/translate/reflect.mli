(** Analysis-only owise reflection: replace each [owise] rule's implicit "no
    earlier sibling applied" semantics with an explicit total-boolean guard
    condition [or(g_1 .. g_k) = false] (one [g_i] per preceding sibling clause,
    spelled over the SAME subject terms as that sibling's own conditions so the
    MFE's Church-Rosser checker can discharge the owise/sibling critical pairs
    by hypothesis rewriting), and clear the [owise] flag. Symbols whose siblings
    are not yet reflectable (relation calls, iteration helpers, gensym-threaded
    symbols, untypable shapes) are kept as-is with a stderr note; {!Mfe}'s
    [drop_owise] fallback still covers them. Support rules a guard needs
    (matcher families, struct accessors, payload projections) are appended when
    the pruned system lacks them. *)

val owise :
  scalars:Ctrs_term.scalar_theory ->
  orig:Lang.Il.spec ->
  effectful:string list ->
  Rewrite_system.t ->
  Rewrite_system.t
