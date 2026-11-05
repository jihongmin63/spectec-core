(** Plain TRS encoders over the same prepared rule pairs. All three use disjoint
    variable/function namespaces and identify functions by name and arity,
    preserving overloaded constructors such as tuples. ARI escapes every
    non-alphanumeric byte (including underscore), preserving distinct
    identifiers without relying on a solver's quoted-symbol parser. *)
val xml : Solver.problem -> (Solver.document, string) result

val ari : Solver.problem -> (Solver.document, string) result
val tpdb : Solver.problem -> (Solver.document, string) result
