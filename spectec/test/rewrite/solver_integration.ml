(* Explicit opt-in integration executable: requires local AProVE and NaTT.
   Run with z3 on PATH: dune exec test/rewrite/solver_integration.exe.
   This checks the same prepared TRS through both real adapters. *)
open Rewrite

let unwrap = function Ok value -> value | Error message -> failwith message

let problem rhs =
  let open Rewrite_system in
  of_rules [ { lhs = App ("f", [ Var "x" ]); rhs; conds = []; owise = false } ]
  |> Termination.prepare |> unwrap

let () =
  let yes = problem (Rewrite_system.Var "x") in
  let collision = problem (Rewrite_system.App ("x", [])) in
  let no = problem (Rewrite_system.App ("f", [ Rewrite_system.Var "x" ])) in
  let conditional =
    let open Rewrite_system in
    of_rules
      [
        {
          lhs = App ("f", [ Var "x" ]);
          rhs = Var "x";
          conds = [ (App ("p", [ Var "x" ]), App ("true", [])) ];
          owise = false;
        };
        {
          lhs = App ("p", [ Var "x" ]);
          rhs = App ("true", []);
          conds = [];
          owise = false;
        };
      ]
    |> Termination.prepare |> unwrap
  in
  List.iter
    (fun adapter ->
      let tool = Solver.configure adapter |> unwrap in
      List.iter
        (fun (label, problem, expected) ->
          let run = Termination.check ~tool ~timeout:15 problem in
          let verdict = unwrap run.verdict in
          Printf.printf "%s %s: %s (%.3fs)\n%!" adapter.Solver.name label
            (Solver.string_of_verdict verdict)
            run.secs;
          if verdict <> expected then failwith (adapter.name ^ " " ^ label))
        [
          ("terminating", yes, Solver.Yes);
          ("looping", no, Solver.No);
          ("variable/function name collision", collision, Solver.Yes);
          ("conditional unraveling", conditional, Solver.Yes);
        ])
    [ Aprove.adapter; Natt.adapter ]
