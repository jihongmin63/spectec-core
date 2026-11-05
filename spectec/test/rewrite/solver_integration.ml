(* Explicit opt-in integration executable: pass installed tool names, with z3
   on PATH. Every selected tool checks the same prepared TRSs. *)
open Rewrite

let unwrap = function Ok value -> value | Error message -> failwith message

let problem rhs =
  let open Rewrite_system in
  of_rules [ { lhs = App ("f", [ Var "x" ]); rhs; conds = []; owise = false } ]
  |> Termination.prepare |> unwrap

let () =
  let adapters =
    Array.to_list Sys.argv |> List.tl
    |> List.map (fun name -> Termination_tools.find name |> unwrap)
  in
  if adapters = [] then failwith "pass at least one installed tool name";
  let yes = problem (Rewrite_system.Var "x") in
  let empty = Rewrite_system.of_rules [] |> Termination.prepare |> unwrap in
  let collision = problem (Rewrite_system.App ("x", [])) in
  let no = problem (Rewrite_system.App ("f", [ Rewrite_system.Var "x" ])) in
  let overloaded = problem (Rewrite_system.App ("f", [])) in
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
          if not (List.mem verdict expected) then
            failwith (adapter.name ^ " " ^ label))
        [
          ("empty TRS", empty, [ Solver.Yes ]);
          ("terminating", yes, [ Solver.Yes ]);
          ("overloaded function arities", overloaded, [ Solver.Yes ]);
          ( "looping",
            no,
            if List.mem adapter.name [ "aprove"; "natt"; "nti" ] then
              [ Solver.No ]
            else [ Solver.No; Solver.Maybe; Solver.Timeout ] );
          ("variable/function name collision", collision, [ Solver.Yes ]);
          ("conditional unraveling", conditional, [ Solver.Yes ]);
        ];
      if List.mem adapter.name [ "muterm"; "nti"; "mint"; "nattpp" ] then (
        let open Rewrite_system in
        let problem : Solver.problem =
          { rules = [ (App ("$", []), App ("_24", [])) ]; tpdb = "" }
        in
        let run = Solver.check ~tool ~timeout:15 ~problem in
        if unwrap run.verdict <> Solver.Yes then
          failwith (adapter.name ^ " conflates escaped identifiers");
        Printf.printf "%s escaped identifiers: YES\n%!" adapter.name))
    adapters
