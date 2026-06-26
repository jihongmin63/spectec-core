(** The IL pre-pass that runs before {!To_ctrs}. In this project it is the
    IDENTITY -- the spec is passed to {!To_ctrs} unchanged (see [simplify.ml]
    for why the original simplification logic is deliberately not reintroduced).
*)
val simplify_spec : Lang.Il.spec -> Lang.Il.spec
