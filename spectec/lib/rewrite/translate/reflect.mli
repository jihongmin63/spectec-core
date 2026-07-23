(** The shared context of the analysis-only passes: the scalar theory and the
    (defunctionalized) spec the type/constructor tables are read from. A future
    selective-rl mode (the LTL [--rules-for] wiring) adds its rule-head
    selection here instead of threading another label through every pass. *)
type ctx = { scalars : Ctrs_term.scalar_theory; orig : Lang.Il.spec }

(** Analysis-only owise reflection: replace each [owise] rule's implicit "no
    earlier sibling applied" semantics with an explicit total-boolean guard
    condition [or(g_1 .. g_k) = false] (one [g_i] per preceding sibling clause,
    spelled over the SAME subject terms as that sibling's own conditions so the
    MFE's Church-Rosser checker can discharge the owise/sibling critical pairs
    by hypothesis rewriting), and clear the [owise] flag. Symbols whose siblings
    are not yet reflectable (relation calls, iteration helpers, gensym-threaded
    symbols, untypable shapes) are kept as-is with a stderr note; {!Mfe} warns
    when such a rule reaches the checker. Support rules a guard needs (matcher
    families, struct accessors, payload projections) are appended when the
    pruned system lacks them. *)

val owise : ctx -> effectful:string list -> Rewrite_system.t -> Rewrite_system.t

(** Respell an opaque matcher test [match_K(v) = true] (from a
    [CaseP]/[OptP]/[ListP] `Cons`/`Nil` guard) as the structural equation
    [v = K(fresh..)], for a bare-variable subject [v] no OTHER condition of the
    same rule mentions, so {!Crc_surface.fold_premise_binders} -- run
    immediately after this pass -- can fold a head-bound discriminator variable
    into the constructor pattern it tests. A subject [v] a companion condition
    also mentions (e.g. a separate destructuring [let K(x, y) = v] alongside the
    [matches K] guard) is left as the original opaque predicate: respelling it
    would not unblock the fold (both conditions would still mention [v]) and
    would only replace one inert condition with a noisier one. Analysis-only,
    like {!owise}. *)
val hoist_matchers : ctx -> Rewrite_system.t -> Rewrite_system.t

(** Respell each top-level comparison / negation guard to the canonical
    [leq]/[leq_int] predicate at inverted polarity: [lt(a,b) = true] becomes
    [leq(b,a) = false] (and the [_int] and [false]-polarity variants likewise),
    and a leading [not(x) = b] guard is flattened to [x = not b]. Sibling
    clauses split on complementary comparisons ([i < 0] vs [i >= 0], which
    translation had spelled as [lt_int(X,0)] vs the swapped [leq_int(0,X)]) then
    state the SAME subject term, so the CRC discharges their otherwise-spurious
    critical pair by hypothesis rewriting -- the prelude's [lt_int]/[not] bridge
    cannot, because [X]'s recovered sort is the top [Val] rather than the
    bridge's [IntV]. Satisfiability-equivalent over the total structural
    predicates; preserves the variable set. Analysis-only, like {!owise}. *)
val align_guards : ctx -> Rewrite_system.t -> Rewrite_system.t

(** (B') subty-guard head specialization: expand a clause guarded by a
    membership test [subty_<S>(v) = true] on a head-bound variable [v] into one
    clone per member constructor of [S] (substituting [v := K(fresh..)] through
    the whole rule, dropping the guard, and keeping the member's payload
    conjunction as a residual condition when it is not literally [true]), so
    sibling clauses dispatching on the same subject get genuinely disjoint head
    patterns instead of overlapping variable heads whose disjointness the CRC
    cannot see. Exact by subtype totality: the [subty_<S>] rule family (member
    cases -> payload conjunction, use-based complement -> [false]) enumerates
    the guard's true-set syntactically. Companion conditions are partially
    evaluated per clone (matcher/subty tests on the now-concrete constructor
    decide or kill the clone; destructuring equations decompose pointwise);
    unrecognized shapes are conservatively kept. Analysis-only, like {!owise}.
*)
val expand_subty_guards : ctx -> Rewrite_system.t -> Rewrite_system.t
