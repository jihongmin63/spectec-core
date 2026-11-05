(** Register new single-execution adapters here. *)
val find : string -> (Solver.adapter, string) result

val names : string list
