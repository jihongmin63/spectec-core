(** Per defined symbol ([Ctrs_term.func_sym]/[Ctrs_term.rel_sym] of each
    [DecD]/[RelD]), the IL type of each variable in its clauses/rules, recovered
    from the simplified spec's [VarE] notes. Lets a typed backend ({!To_maude})
    restore a variable's declared (narrow) type instead of the widened argument
    type a relation position would impose. A variable whose occurrences disagree
    is omitted. *)
val of_spec : Lang.Il.spec -> (string, (string * Lang.Il.typ') list) Hashtbl.t
