(** NaTT 2.3 accepts XML and [-V] suppresses narrative output, leaving its
    global verdict on stdout. The adapter emits unconditional XTC rules from the
    same unraveling as TPDB. Set SPECTEC_NATT_BIN to the executable; NaTT's
    default SMT backend expects z3 on PATH. Both binaries are fingerprinted for
    resume validation. Variable and function identifiers use disjoint XML
    namespaces, and inconsistent function arities are rejected before execution.

    Install in the project switch with
    [opam install --switch=spectecx ./spectec/tools/natt/natt.opam]. The
    manifest pins the source archive and its build dependencies.

    Official source:
    https://www.trs.cm.is.nagoya-u.ac.jp/NaTT/downloads/NaTT.2.3.tar.gz The
    public 2.3 distribution is distinct from the termCOMP 2026 image. *)
val adapter : Solver.adapter
