(** Backend-local CTRS rules for P4's collection builtins (map/set/list/text).

    These are the [builtin dec]s the interpreter implements natively in OCaml
    ([targets/p4/builtins/]) but which {!To_ctrs} emits no rules for, so a call
    to one stops the surrounding term in the Maude backend. Each is expressed as
    the structural recursion the interpreter does, over the [cons]/[nil] lists,
    [set]/[pair] constructors and [tuple] association pairs the spec already
    builds, reusing the prelude term helpers. Numeric ([$sum], bitwise/arith)
    and text ([$int_to_text]) builtins are out of scope here (the Maude backend
    delegates them); the gensym ([$fresh_typeId]/[$fresh_tid]) is modeled
    separately by state threading ({!Gensym}). *)

(** The collection-builtin rules the spec's [BuiltinDecD]s call for, plus the
    shared list helpers, as definition rules for {!To_ctrs.of_spec}'s
    [extra_defs] pool (pruned where unreached). [[]] when the spec declares no
    collection builtin, leaving such a spec (e.g. impty) untouched. *)
val rules_of_builtins : Lang.Il.spec -> Rewrite_system.rule list
