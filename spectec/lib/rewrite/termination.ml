type problem = { input : Solver.problem; stats : Unravel.stats }

let proof_surface (system : Rewrite_system.t) : Rewrite_system.t =
  Crc_surface.rebind_stuck_guards system

let trs_of_slice (system : Rewrite_system.t) :
    (string * Unravel.stats, string) result =
  Unravel.trs_of_system (proof_surface system)

let prepare (system : Rewrite_system.t) : (problem, string) result =
  let system = proof_surface system in
  Result.bind (Unravel.unconditional_rules system) (fun unraveled ->
      Result.map
        (fun (tpdb, stats) ->
          { input = { Solver.rules = unraveled.unconditional; tpdb }; stats })
        (Unravel.trs_of_system system))

let check ~tool ~timeout problem : Solver.run =
  Solver.check ~tool ~timeout ~problem:problem.input
