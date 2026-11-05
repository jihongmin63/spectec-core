let adapter : Solver.adapter =
  {
    name = "natt";
    variable = "SPECTEC_NATT_BIN";
    candidates =
      [
        "spectec/tools/natt/bin/NaTT.exe"; "tools/natt/bin/NaTT.exe"; "NaTT.exe";
      ];
    encode = Trs_input.xml;
    command = (fun ~executable ~input ~timeout:_ -> [ executable; input; "-V" ]);
    parse = Solver.first_line;
    finished = (fun _ -> false);
    proof = (fun _ -> "");
    dependencies = (fun ~executable:_ -> [ "z3" ]);
  }
