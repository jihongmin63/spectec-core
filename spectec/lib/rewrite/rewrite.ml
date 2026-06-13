module Rewrite_system = Rewrite_system
module To_ctrs = To_ctrs
module Simplify = Simplify
module To_maude = To_maude
module Maude_run = Maude_run
module Cocoweb = Cocoweb
module Muterm = Muterm
module Aprove = Aprove
module Termination = Termination

(* Simplify each rule and clause via the premise environment (structural
   substitution + redundant-premise removal), then translate the result into the
   CTRS representation; see [Pipeline.ctrs_of_spec] for the stages (and the
   debug fallback that bypasses simplification). *)
let rewrite_spec : Lang.Il.spec -> Rewrite_system.t = Pipeline.ctrs_of_spec

(* The function/relation symbols a spec defines, usable as slice roots for
   per-symbol confluence checking (see [Rewrite_system.slice]). *)
let def_symbols : Lang.Il.spec -> string list = To_ctrs.def_symbols
