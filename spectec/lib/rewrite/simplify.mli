(** Simplify every relation rule and function clause in [spec]: expand variables
    into their concrete canonical structure (drawn from the {!Prem_env}) and
    drop premises the premise environment renders redundant. *)
val simplify_spec : Lang.Il.spec -> Lang.Il.spec
