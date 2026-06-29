(* Pre-pass over the elaborated IL, run before {!To_ctrs}.

   In this project {!simplify_spec} is intentionally the IDENTITY: the spec is
   handed to {!To_ctrs} unchanged. The original [rewrite] branch ran a
   semantics-preserving IL->IL rewriting here (variable expansion from a
   {!Prem_env} union-find, [matches]/field-access folding into head patterns,
   value/let inlining, subtype-to-cast lowering, redundant-premise removal) so
   that clauses mapped more directly to CTRS rules. We deliberately drop that:
   [To_ctrs] is the sole translation surface, so the simplification logic (and
   the [Prem_env] engine that only fed it) is not reintroduced. *)

let simplify_spec (spec : Lang.Il.spec) : Lang.Il.spec = spec
