let orders =
  "Linear LPO MaxPlus StdMat(2) SPO(Model(Finite)) \
   RPLex(WPO(MaxPlus),StdMat(2)) SPO(RTLex(Mark(MaxPlus),Mark(Linear))) \
   LexMat(3) SPO(Model(LexMat(3))) SPO(RTLex(Mark(MaxPlus),Mark(MaxPlus)))"

let adapter : Solver.adapter =
  {
    name = "mint";
    variable = "SPECTEC_MINT_BIN";
    candidates =
      [ "spectec/tools/mint/bin/mint"; "tools/mint/bin/mint"; "mint" ];
    encode = Trs_input.ari;
    command =
      (fun ~executable ~input ~timeout:_ -> [ executable; "-o"; orders; input ]);
    parse = Solver.first_line;
    finished = (fun _ -> false);
    proof = Solver.after_first_line;
    dependencies = (fun ~executable:_ -> [ "z3" ]);
  }
