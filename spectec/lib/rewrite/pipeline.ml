type target = Analysis | Termination_analysis | Execution
type compilation = { spec : Lang.Il.spec; system : Rewrite_system.t }

(* Specialization renames definitions. Keep its result physically shared so
   signature recovery and start-term encoding use the same per-spec indices. *)
let memo : (Lang.Il.spec * Lang.Il.spec) option ref = ref None

let prepare spec =
  match !memo with
  | Some (source, prepared) when source == spec || prepared == spec -> prepared
  | _ ->
      let prepared =
        spec |> Defunctionalize.defunctionalize |> Monomorphize.monomorphize
      in
      memo := Some (spec, prepared);
      prepared

let analysis_surface spec system =
  let ctx = { Reflect.scalars = To_ctrs.Structural; orig = spec } in
  let system =
    system |> Reflect.hoist_matchers ctx
    |> Reflect.expand_subty_guards ctx
    |> Crc_surface.fold_premise_binders
    (* Folding can prepend an isStuckHead guard ahead of its binder.
       Reflection reads conditions in binding order, so schedule it here. *)
    |> Crc_surface.order_conds
    |> Reflect.align_guards ctx
  in
  Reflect.owise ctx ~effectful:(Gensym.effectful_syms system) system
  |> Crc_surface.order_conds

let compile ~target spec =
  let spec = prepare spec in
  let scalars =
    match target with
    | Analysis | Termination_analysis -> To_ctrs.Structural
    | Execution -> To_ctrs.Native
  in
  let system =
    To_ctrs.of_spec ~scalars
      ~extra_defs:(Builtin.rules_of_builtins ~scalars spec)
      spec
    |> Gensym.thread ~scalars |> Rewrite_system.orient_conds
  in
  let system =
    match target with
    | Analysis -> analysis_surface spec system
    | Termination_analysis | Execution -> system
  in
  { spec; system }

let def_symbols spec = To_ctrs.def_symbols (prepare spec)
