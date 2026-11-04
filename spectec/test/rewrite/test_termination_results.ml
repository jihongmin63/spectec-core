module Results = Rewrite_cli.Termination_results

let () =
  let path = Filename.temp_file "termination_results" ".tsv" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let row_of symbol tool : Results.row =
        let pid = Unix.fork () in
        if pid = 0 then (
          let fd = Unix.openfile path [ Unix.O_RDWR ] 0 in
          let locked =
            try
              Unix.lockf fd Unix.F_TLOCK 0;
              false
            with Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) -> true
          in
          Unix.close fd;
          exit (if locked then 0 else 1));
        let _, status = Unix.waitpid [] pid in
        if status <> Unix.WEXITED 0 then
          failwith "TSV lock released before tool execution";
        {
          symbol;
          tool;
          verdict = Results.Answer Rewrite.Solver.Yes;
          stats = "-";
          secs = 0.;
        }
      in
      match
        Results.run ~out:(Some path) ~identity:"test"
          ~jobs:[ ("f", "natt") ]
          ~row_of
      with
      | Ok false -> print_endline "test_termination_results: all tests passed"
      | _ -> failwith "result recording failed")
