(** Termination problems share the same proof surface and unraveling across
    tools. TRS YES implies termination of the sorted analysis CTRS; TRS NO alone
    does not prove nontermination of the original specification. *)
type problem = { input : Solver.problem; stats : Unravel.stats }

(** Restore dead binders hidden by isStuckHead guards, whose semantics are
    unavailable on the analysis surface. Omitting this restoration would
    silently remove recursive paths from the proof problem. *)
val proof_surface : Rewrite_system.t -> Rewrite_system.t

val trs_of_slice : Rewrite_system.t -> (string * Unravel.stats, string) result
val prepare : Rewrite_system.t -> (problem, string) result
val check : tool:Solver.tool -> timeout:int -> problem -> Solver.run
