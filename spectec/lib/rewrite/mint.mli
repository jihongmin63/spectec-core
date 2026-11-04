(** Mint 0.5 development distribution from tspn/mint-termcomp:2026. Uses ARI and
    the explicit TRS_Standard order sequence from its official competition
    wrapper. Set SPECTEC_MINT_BIN to the native executable; Z3 is required on
    PATH. Both binaries are fingerprinted. Malformed input can exit zero with a
    diagnostic, so only the first-line global verdict is accepted. The proof
    follows that line. No source/license grant was found in the image metadata;
    binaries are fetched locally from the publisher and are not vendored in this
    repository. *)
val adapter : Solver.adapter
