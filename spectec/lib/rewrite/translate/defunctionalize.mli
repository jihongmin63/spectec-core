(** Defunctionalize [def]-valued arguments by call-site specialization.

    A SpecTec [DefA] argument is always a literal `def $name`, so a definition
    with a [DefP] parameter is a template over finitely many instantiations:
    each call `$f(args, def $g)` is redirected to a generated first-order copy
    `$f_$g` (the def parameter removed, `$check := $g` substituted through the
    clauses), to a worklist closure over recursion and chained templates. The
    templates themselves are removed; the result carries no [DefP]/[DefA] (a
    leftover would silently translate to a call of an undefined symbol, the old
    dropped-argument behaviour).

    The identity on a spec with no def parameters (impty), and memoized one slot
    by physical equality so every consumer of the same elaborated spec sees the
    same physical result. *)

val defunctionalize : Lang.Il.spec -> Lang.Il.spec
