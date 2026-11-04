let adapter : Solver.adapter =
  {
    name = "muterm";
    variable = "SPECTEC_MUTERM_BIN";
    candidates =
      [ "spectec/tools/muterm/bin/muterm"; "tools/muterm/bin/muterm"; "muterm" ];
    encode = Trs_input.tpdb;
    command =
      (fun ~executable ~input ~timeout:_ ->
        [
          "/bin/sh";
          "-c";
          "cd -- \"$1\" && exec \"$2\" -i \"$3\"";
          "muterm";
          Filename.dirname executable;
          executable;
          input;
        ]);
    parse = Solver.first_line;
    finished = (fun _ -> false);
    proof = Solver.after_first_line;
    dependencies =
      (fun ~executable ->
        List.map
          (Filename.concat (Filename.dirname executable))
          [
            "barcelogicNIA";
            "callyices.sh";
            "callyicesDen.sh";
            "interpformat";
            "invocanlsol.sh";
            "invocayicesIPOL.sh";
            "invocayicesIPOLDC.sh";
            "mace4";
            "nlsol";
            "timelimit";
            "transYicesOut";
            "transYicesOutDen";
            "yices";
          ]);
  }
