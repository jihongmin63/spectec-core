(** NTI 2026.09, pinned to upstream commit
    88a47f9db976fc7c1ad1921057f0a68d68748137, runs FULL rewriting on TPDB.
    [runme INPUT.trs -v] invokes the adjacent bin/nti.jar using Java 21+. The
    jar and Java executable are fingerprinted. SPECTEC_NTI_BIN can select
    another launcher with the same layout. NTI's verbose stdout begins with the
    global verdict, followed by its proof argument and description. Source and
    LGPL-3.0-or-later license: https://github.com/etiennepayet/nti. Build with
    the upstream Maven wrapper and a JDK 21+, then place target/nti.jar in
    spectec/tools/nti/bin. See tools/termination for pinned provisioning. *)
val adapter : Solver.adapter
