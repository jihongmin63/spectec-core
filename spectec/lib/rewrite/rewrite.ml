module Rewrite_system = Rewrite_system
module Crc_surface = Crc_surface
module Scc_surface = Scc_surface
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
module Aprove = Aprove
module Qdp = Qdp
module Termination = Termination
module Scc = Scc
module Defunctionalize = Defunctionalize
module Monomorphize = Monomorphize
module Gensym = Gensym
module Builtin = Builtin

type target = Pipeline.target = Analysis | Termination_analysis | Execution

type compilation = Pipeline.compilation = {
  spec : Lang.Il.spec;
  system : Rewrite_system.t;
}

let compile = Pipeline.compile
let def_symbols = Pipeline.def_symbols
