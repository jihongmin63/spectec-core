(** The Maude lexical layer shared by both Maude surfaces ({!To_maude} execution
    and {!To_mfe} analysis), so operator and variable identifiers get a single
    spelling. The order-sorted module emission lives in the other [maude/]
    backends; this layer owns only the lexical scrub. *)

(** A CTRS id ([A-Za-z0-9_$]+) as a Maude-safe id: [_] is a mixfix placeholder
    in Maude, so it maps to [-] (injective, since CTRS ids never contain [-]).
    Shared by both Maude surfaces so operator identifiers get one spelling. *)
val id : string -> string

(** A CTRS variable name as a valid Maude variable identifier: names already
    confined to [A-Za-z0-9_] render exactly as the {!id} mangling; the rest are
    run through {!Rewrite_system.sanitize} first. *)
val var : string -> string
