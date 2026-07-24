module Rewrite_system = Rewrite_system
module Crc_surface = Crc_surface
module Scc_surface = Scc_surface
module To_ctrs = To_ctrs
module Simplify = Simplify
module Maude_ident = Maude_ident
module To_maude = To_maude
module Maude_sorts = Maude_sorts
module To_mfe = To_mfe
module Maude_run = Maude_run
module Of_maude = Of_maude
module Mfe = Mfe
module Subproc = Subproc
module Unravel = Unravel
module Wll = Wll
module Aprove = Aprove
module Termination = Termination
module Scc = Scc
module Defunctionalize = Defunctionalize
module Gensym = Gensym
module Builtin = Builtin

(* The analysis pipeline entry; see [Pipeline.ctrs_of_spec] for the stage
   order. [To_ctrs] is the sole translation surface ([Simplify] is deliberately
   the identity). *)
let rewrite_spec : Lang.Il.spec -> Rewrite_system.t = Pipeline.ctrs_of_spec

(* The execution pipeline entry: the direct IL -> Maude path in the native
   scalar theory (see [Pipeline.maude_system_of_spec]), the counterpart of
   [rewrite_spec] (which builds the analysis, structural system). A run driver
   builds the system once and threads it into the emitter and the start-term
   encoders/decoders rather than each rebuilding it. *)
let maude_system : Lang.Il.spec -> Rewrite_system.t =
  Pipeline.maude_system_of_spec

(* The function/relation symbols a spec defines, usable as slice roots for
   per-symbol confluence checking (see [Rewrite_system.slice]). *)
let def_symbols : Lang.Il.spec -> string list = To_ctrs.def_symbols
