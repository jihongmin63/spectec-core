let () =
  Rewrite_cli.P4_frontend.register
    {
      parse =
        (fun ~includes filename ->
          Targets_p4.P4.Frontend.parse_file includes filename);
      typecheck =
        (fun ~spec_il ~includes filename ->
          Spectec.eval_task
            (module Targets_p4.P4.Typecheck)
            ~mode:Spectec.Interp_mode.Il ~spec_il
            {
              Targets_p4.P4.Typecheck.includes;
              filename;
              expect = Spectec.Task.Positive;
            });
    }
