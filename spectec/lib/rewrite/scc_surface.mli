(** SCC-facing over-approximation passes, applied (via {!Scc.unconditional}) so
    the sufficient-completeness checker's [drop-bad-eqs] filter keeps every
    rule. Counterexamples on the transformed slice stay sound; "complete"
    verdicts for transformed symbols prove nothing. Never applied to the
    canonical analysis or execution surfaces. *)

(** Strip every rule's conditions; an RHS left with unbound variables is
    replaced by the LHS (always well-sorted). Reports the dropped counts on
    stderr. *)
val drop_conds : Rewrite_system.t -> Rewrite_system.t

(** Rename the second and later occurrences of any repeated LHS variable so
    every lhs is left-linear; the first occurrence keeps its name, so an RHS
    that mentions the variable stays bound. Reports the count on stderr. *)
val linearize_lhs : Rewrite_system.t -> Rewrite_system.t
