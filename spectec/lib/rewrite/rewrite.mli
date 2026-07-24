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
module Aprove = Aprove
module Termination = Termination
module Scc = Scc

(** Translate an elaborated IL spec into the analysis rewriting system:
    defunctionalize, translate via {!To_ctrs} in the structural scalar theory,
    thread gensym state, then apply the analysis-only reflection and folding
    passes; see {!Pipeline.ctrs_of_spec} for the stage order. *)
val rewrite_spec : Lang.Il.spec -> Rewrite_system.t

(** Translate an elaborated IL spec into the execution rewriting system: the
    direct IL -> Maude path in the native scalar theory over Maude's built-in
    nat/int/bool/string; see {!Pipeline.maude_system_of_spec}. The execution
    counterpart of {!rewrite_spec}. A run driver builds this once and threads it
    into {!To_maude.module_of_system}, {!To_maude.meta_start_app} and
    {!Of_maude.values_of_result} instead of each rebuilding it. *)
val maude_system : Lang.Il.spec -> Rewrite_system.t

(** The function/relation symbols a spec defines, in declaration order. Pair
    with {!Rewrite_system.slice} to check confluence one symbol's dependency
    closure at a time. *)
val def_symbols : Lang.Il.spec -> string list
