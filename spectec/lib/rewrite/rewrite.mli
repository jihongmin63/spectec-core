module Rewrite_system = Rewrite_system
module Crc_surface = Crc_surface
module To_ctrs = To_ctrs
module Maude_ident = Maude_ident
module To_maude = To_maude
module Maude_sorts = Maude_sorts
module To_mfe = To_mfe
module Maude_run = Maude_run
module Of_maude = Of_maude
module Mfe = Mfe
module Subproc = Subproc
module Unravel = Unravel
module Itrs = Itrs
module Solver = Solver
module Natt = Natt
module Termination_tools = Termination_tools
module Aprove = Aprove
module Termination = Termination

type target = Pipeline.target = Analysis | Execution

type compilation = Pipeline.compilation = {
  spec : Lang.Il.spec;
  system : Rewrite_system.t;
}

(** Compile an elaborated spec for analysis or execution through the shared
    pipeline. Use the returned [spec] and [system] together for emission and
    start-term encoding; see {!Pipeline.compile} for the pass order. *)
val compile : target:target -> Lang.Il.spec -> compilation

(** Specialized slice roots in declaration order; does not build the system. *)
val def_symbols : Lang.Il.spec -> string list
