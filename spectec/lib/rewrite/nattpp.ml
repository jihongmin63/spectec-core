let adapter : Solver.adapter =
  {
    name = "nattpp";
    variable = "SPECTEC_NATTPP_BIN";
    candidates =
      [ "spectec/tools/nattpp/bin/natt++"; "tools/nattpp/bin/natt++"; "natt++" ];
    encode = Trs_input.ari;
    command = (fun ~executable ~input ~timeout:_ -> [ executable; "-q"; input ]);
    parse = Solver.first_line;
    finished = (fun _ -> false);
    proof = (fun _ -> "");
    dependencies = (fun ~executable:_ -> [ "z3" ]);
  }
