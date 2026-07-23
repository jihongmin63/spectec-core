(** Shared index of an elaborated (defunctionalized) IL spec: the type,
    constructor and signature tables the reflection passes and the Maude
    backends all need, built in one spec traversal and memoized per spec
    (physical equality). Keys are the CTRS symbol spellings ({!Ctrs_term}); a
    backend needing a differently-spelled view (e.g. {!Of_maude}'s
    Maude-identifier keys) derives it from these tables instead of re-walking
    the spec. *)

open Lang.Il

type t = {
  typdefs : (string, deftyp') Hashtbl.t;  (** type name -> definition *)
  ctor_types : (string, string list) Hashtbl.t;
      (** variant sym -> type names, in declaration order *)
  variant_cases : (string, string * mixop * typ' list) Hashtbl.t;
      (** variant sym -> (origin type, mixop, field types) *)
  struct_fields : (string, string * (atom * typ') list) Hashtbl.t;
      (** struct sym -> (type name, fields) *)
  funcsigs : (string, typ list * typ) Hashtbl.t;  (** $f -> params, result *)
  relsigs : (string, typ list) Hashtbl.t;  (** Rel -> input types *)
  rel_outs : (string, typ list) Hashtbl.t;  (** Rel -> output types *)
  fieldsigs : (string, typ') Hashtbl.t;  (** field_<ty>_<a> -> field type *)
}

(** The origin type and mixop identifying a variant case's constructor. *)
val case_origin_mixop : typcase -> string * mixop

val of_spec : spec -> t

(** Unwrap plain aliases down to a variant/struct/structural type. *)
val resolve : t -> typ' -> typ'

(** The case of variant type [ty] whose generated symbol is [ctor]: its mixop,
    field types, and [ty]'s case count. *)
val variant_case : t -> string -> string -> (mixop * typ list * int) option
