let adapter : Solver.adapter =
  {
    name = "nti";
    variable = "SPECTEC_NTI_BIN";
    candidates = [ "spectec/tools/nti/runme"; "tools/nti/runme" ];
    encode = Trs_input.tpdb;
    command = (fun ~executable ~input ~timeout:_ -> [ executable; input; "-v" ]);
    parse = Solver.first_line;
    finished = (fun _ -> false);
    proof = Solver.after_first_line;
    dependencies =
      (fun ~executable ->
        [ Filename.concat (Filename.dirname executable) "bin/nti.jar"; "java" ]);
  }
