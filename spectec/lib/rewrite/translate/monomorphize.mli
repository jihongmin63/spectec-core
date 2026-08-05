(** Specialize every polymorphic [dec] at the type instantiations its call sites
    ask for, so no type PARAMETER survives into the translation.

    A parameter is a type {!To_ctrs} cannot dispatch on: [$in_set<K>] compares
    at [K], which has no [TypD], so its equality can only fall back on the
    polymorphic [eq] -- the one symbol whose off-diagonal the analysis surface
    cannot decide. With the body copied per instantiation the comparison reaches
    the instantiating type's own [eq_<T>] instead.

    Type arguments are read off [CallE], never inferred, and specialization runs
    to a fixed point (a specialized body's own polymorphic calls have their type
    arguments substituted). A [dec] with no instantiation is left polymorphic.
    Identity on a spec with no polymorphic [dec]. *)

val monomorphize : Lang.Il.spec -> Lang.Il.spec

(** A type parameter -> argument substitution. *)
type theta = (string * Lang.Il.typ') list

val subst_typ : theta -> Lang.Il.typ' -> Lang.Il.typ'

(** A type definition's body under a substitution, for the derived rules a
    polymorphic TYPE needs per instantiation ({!To_ctrs}'s [eq_<T>]): unlike a
    [dec], the definition itself is never cloned -- its constructors are shared
    across instantiations -- only the rules read off it. *)
val subst_deftyp : theta -> Lang.Il.deftyp' -> Lang.Il.deftyp'
