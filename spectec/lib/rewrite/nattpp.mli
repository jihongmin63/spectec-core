(** NaTT++ from akihisayamada/nattpp:termcomp2026.3 uses ARI.
    [natt++ -q INPUT.ari] suppresses narrative output; stdout contains the
    global verdict. Set SPECTEC_NATTPP_BIN to the binary, with Z3 on PATH. Both
    binaries are fingerprinted. The published image includes C++20 source and a
    Makefile, but no license text was found. Distribution layers are pinned in
    tools/termination; their contents are fetched locally only. *)
val adapter : Solver.adapter
