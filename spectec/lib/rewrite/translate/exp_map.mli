open Lang.Il

(** Shallow, one-level expression maps. [map_subexps f e'] rebuilds [e'],
    applying [f] to each immediate sub-expression and leaving everything else
    (atoms, types, mixops, iterexps, [DefA] args) untouched; it does not
    recurse, so [f] controls the descent. Cast wrappers are preserved. *)

val map_subexps : (exp -> exp) -> exp' -> exp'

(** The immediate sub-expressions of [e'], in [map_subexps] visit order. The
    read-only counterpart of [map_subexps]: occurrence checks, counters and
    collectors recurse over this with plain [List] folds instead of running
    [map_subexps] for its side effects and discarding the rebuilt node. *)
val subexps : exp' -> exp list

(** The expressions a premise embeds (notation arguments, guards, a [let]'s two
    sides), descending through [IterPr] wrappers to the inner premise. *)
val exps_of_prem : prem -> exp list

(** Apply [f] to every expression embedded in a path (index/slice positions). *)
val map_path_exps : (exp -> exp) -> path -> path
