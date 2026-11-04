(** AProVE runner contract: [runme INPUT.trs TIMEOUT]. Override its path with
    SPECTEC_APROVE_BIN. Only the header before the proof echo carries the global
    verdict; a child proof's YES never establishes termination of the input. *)
val adapter : Solver.adapter

val proof_tree : string -> string
val proof_printed : string -> bool
