(** MU-TERM's native [muterm -i INPUT.trs] accepts TPDB with ranked function
    names from {!Trs_input.tpdb}. Run from the distribution directory because
    its helper executables use relative paths. The common runner imposes the
    whole-process deadline. Set SPECTEC_MUTERM_BIN to the native binary beside
    the bundled helpers. All bundled search helpers are fingerprinted for resume
    validation. The verified distribution is raulgut/muterm-image:term-comp2025
    (the 2026 competition entry); exact source layers are pinned in
    tools/termination. The ARI-to-XML-to-TPDB competition wrapper is unnecessary
    for this input. *)
val adapter : Solver.adapter
