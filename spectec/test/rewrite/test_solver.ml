open Rewrite

let expect name condition = if not condition then failwith name

let () =
  let module S = Solver in
  expect "AProVE must not read a subproof YES"
    (Aprove.adapter.parse "KILLED\nproof of x.trs\n(0)\nYES\n" = Ok S.Timeout);
  expect "NaTT reads only the global verdict"
    (Natt.adapter.parse "MAYBE\nYES\n" = Ok S.Maybe);
  expect "invalid output is an error"
    (Result.is_error (Natt.adapter.parse "diagnostic\nYES\n"));
  let exited =
    Subproc.run_detailed ~env:None ~keep:None ~done_when:None
      ~cmd:[ "/bin/sh"; "-c"; "printf failure; exit 7" ]
      ~feed:"" ~timeout:2 ()
  in
  expect "exit status preserved" (exited.status = Unix.WEXITED 7);
  expect "output preserved" (exited.output = "failure");
  let started = Unix.gettimeofday () in
  let expired =
    Subproc.run_detailed ~env:None ~keep:None ~done_when:None
      ~cmd:[ "/bin/sh"; "-c"; "sleep 30" ]
      ~feed:"" ~timeout:1 ()
  in
  expect "deadline enforced" expired.timed_out;
  expect "no grace period" (Unix.gettimeofday () -. started < 3.0)

let () =
  let adapter =
    {
      Natt.adapter with
      command =
        (fun ~executable ~input:_ ~timeout:_ ->
          [ executable; "-c"; "printf 'YES\\n'; exit 7" ]);
      finished =
        (fun _ ->
          Unix.sleepf 0.1;
          true);
    }
  in
  let tool : Solver.tool =
    { adapter; executable = "/bin/sh"; identity = "fixture" }
  in
  let problem : Solver.problem = { rules = []; tpdb = "" } in
  let run = Solver.check ~tool ~timeout:1 ~problem in
  expect "abnormal exit is not a proof" (Result.is_error run.verdict)

let () =
  let adapter =
    {
      Natt.adapter with
      command =
        (fun ~executable ~input:_ ~timeout:_ ->
          [ executable; "-c"; "printf YES; sleep 30" ]);
    }
  in
  let tool : Solver.tool =
    { adapter; executable = "/bin/sh"; identity = "fixture" }
  in
  let problem : Solver.problem = { rules = []; tpdb = "" } in
  let run = Solver.check ~tool ~timeout:1 ~problem in
  expect "partial verdict at deadline is not a proof"
    (run.verdict = Ok Solver.Timeout)

let () =
  let open Rewrite_system in
  let problem : Solver.problem =
    { rules = [ (App ("f", []), App ("f", [ Var "x" ])) ]; tpdb = "" }
  in
  expect "inconsistent arity is rejected"
    (Result.is_error (Natt.adapter.encode problem))

let () = print_endline "test_solver: all tests passed"
