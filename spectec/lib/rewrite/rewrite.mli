module Rewrite_system = Rewrite_system
module To_ctrs = To_ctrs
module Simplify = Simplify
module To_maude = To_maude
module Maude_sorts = Maude_sorts
module To_mfe = To_mfe
module Maude_run = Maude_run
module Of_maude = Of_maude
module Mfe = Mfe
module Subproc = Subproc
module Unravel = Unravel
module Termination = Termination
module Scc = Scc

(** Translate an elaborated IL spec into a rewriting system.

    Simplifies each relation rule and function clause via the premise
    environment (structural substitution + redundant-premise removal) and
    carries the result into the placeholder representation. *)
val rewrite_spec : Lang.Il.spec -> Rewrite_system.t

(** The function/relation symbols a spec defines, in declaration order. Pair
    with {!Rewrite_system.slice} to check confluence one symbol's dependency
    closure at a time. *)
val def_symbols : Lang.Il.spec -> string list
