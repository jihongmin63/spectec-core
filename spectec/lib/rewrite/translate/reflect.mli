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

(** Respell an opaque matcher test [match_K(v) = true] (from a
    [CaseP]/[OptP]/[ListP] `Cons`/`Nil` guard) as the structural equation
    [v = K(fresh..)], for a bare-variable subject [v] no OTHER condition of the
    same rule mentions, so {!Rewrite_system.fold_premise_binders} -- run
    immediately after this pass -- can fold a head-bound discriminator variable
    into the constructor pattern it tests. A subject [v] a companion condition
    also mentions (e.g. a separate destructuring [let K(x, y) = v] alongside the
    [matches K] guard) is left as the original opaque predicate: respelling it
    would not unblock the fold (both conditions would still mention [v]) and
    would only replace one inert condition with a noisier one. Analysis-only,
    like {!owise}. *)
val hoist_matchers :
  scalars:Ctrs_term.scalar_theory ->
  orig:Lang.Il.spec ->
  Rewrite_system.t ->
  Rewrite_system.t
