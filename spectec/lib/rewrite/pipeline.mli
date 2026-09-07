type target = Analysis | Execution

(** The specialized spec and the system compiled from it. Emitters, slice-root
    lookup, and value encoders must use this [spec]: specialization renames
    definitions, so their declarations are absent from the elaborated input. *)
type compilation = { spec : Lang.Il.spec; system : Rewrite_system.t }

(** Compile once: defunctionalize, monomorphize, translate with collection
    builtins, thread gensym state, and orient conditions as (evaluated,
    pattern). [Analysis] uses structural scalars and then reflects/folds guards
    for the MFE. [Execution] targets native Maude scalars directly. Analysis
    passes and their binding order are kept out of the execution target. *)
val compile : target:target -> Lang.Il.spec -> compilation

(** Specialized function/relation names in declaration order, without building a
    rewriting system. *)
val def_symbols : Lang.Il.spec -> string list
