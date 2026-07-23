(** Shared index of an elaborated (defunctionalized) IL spec: the type,
    constructor and signature tables the reflection passes and the Maude
    backends all need, built in one spec traversal and memoized per spec. Keys
    are the CTRS symbol spellings ({!Ctrs_term}); a backend needing a
    differently-spelled view (e.g. {!Of_maude}'s Maude-identifier keys) derives
    it from these tables instead of re-walking the spec. *)

open Common.Source
open Lang.Il
module T = Ctrs_term

type t = {
  typdef_order : (string * deftyp') list;
      (** every [TypD] in spec declaration order (duplicates preserved), for
          consumers whose derivation is order-sensitive *)
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

(* The origin type and mixop identifying a variant case's constructor. *)
let case_origin_mixop (tc : typcase) : string * mixop =
  (tc.origin.it.synid.it, Mixfix.to_mixop tc.notation.it)

let build (orig : spec) : t =
  let typdef_order =
    List.filter_map
      (fun (def : def) ->
        match def.it with
        | TypD { synid; deftyp; _ } -> Some (synid.it, deftyp.it)
        | _ -> None)
      orig
  in
  let idx =
    {
      typdef_order;
      typdefs = Hashtbl.create 64;
      ctor_types = Hashtbl.create 256;
      variant_cases = Hashtbl.create 512;
      struct_fields = Hashtbl.create 256;
      funcsigs = Hashtbl.create 64;
      relsigs = Hashtbl.create 64;
      rel_outs = Hashtbl.create 64;
      fieldsigs = Hashtbl.create 256;
    }
  in
  List.iter
    (fun (def : def) ->
      match def.it with
      | TypD { synid; deftyp; _ } -> (
          if not (Hashtbl.mem idx.typdefs synid.it) then
            Hashtbl.add idx.typdefs synid.it deftyp.it;
          match deftyp.it with
          | VariantT typcases ->
              List.iter
                (fun (tc : typcase) ->
                  let origin, mixop = case_origin_mixop tc in
                  let ctor = T.variant_sym origin mixop in
                  let tys =
                    Option.value
                      (Hashtbl.find_opt idx.ctor_types ctor)
                      ~default:[]
                  in
                  if not (List.mem synid.it tys) then
                    Hashtbl.replace idx.ctor_types ctor (tys @ [ synid.it ]);
                  let ftyps =
                    List.map (fun t -> t.it) (Mixfix.args tc.notation.it)
                  in
                  Hashtbl.replace idx.variant_cases ctor (origin, mixop, ftyps))
                typcases
          | StructT fields ->
              List.iter
                (fun ((a, ft) : typfield) ->
                  Hashtbl.replace idx.fieldsigs (T.field_sym synid.it a) ft.it)
                fields;
              Hashtbl.replace idx.struct_fields (T.struct_sym synid.it)
                (synid.it, List.map (fun (a, t) -> (a, t.it)) fields)
          | _ -> ())
      | DecD { defid; params; typ; _ } ->
          let exps =
            List.filter_map
              (fun p -> match p.it with ExpP t -> Some t | DefP _ -> None)
              params
          in
          if List.length exps = List.length params then
            Hashtbl.replace idx.funcsigs (T.func_sym defid) (exps, typ)
      | RelD { relid; reltyp; _ } ->
          let typs = Mixfix.args (Mode.notation reltyp.it) in
          let idxs = List.init (List.length typs) Fun.id in
          let ins, outs = Mode.partition reltyp.it idxs in
          Hashtbl.replace idx.relsigs (T.rel_sym relid)
            (List.map (List.nth typs) ins);
          Hashtbl.replace idx.rel_outs (T.rel_sym relid)
            (List.map (List.nth typs) outs)
      | BuiltinDecD _ -> ())
    orig;
  idx

(* One-slot memo on the spec (physical equality), the same discipline as
   {!Defunctionalize.defunctionalize}: every consumer of one pipeline run
   passes the same spec value, so the index is built once. *)
let memo : (spec * t) option ref = ref None

let of_spec (orig : spec) : t =
  match !memo with
  | Some (o, idx) when o == orig -> idx
  | _ ->
      let idx = build orig in
      memo := Some (orig, idx);
      idx

(* Unwrap plain aliases down to a variant/struct/structural type. *)
let rec resolve (idx : t) (ty : typ') : typ' =
  match ty with
  | VarT { synid; _ } -> (
      match Hashtbl.find_opt idx.typdefs synid.it with
      | Some (PlainT u) -> resolve idx u.it
      | _ -> ty)
  | _ -> ty

(* The case of variant type [ty] whose generated symbol is [ctor]:
   its mixop, field types, and [ty]'s case count. *)
let variant_case (idx : t) (ty : string) (ctor : string) :
    (mixop * typ list * int) option =
  match Hashtbl.find_opt idx.typdefs ty with
  | Some (VariantT typcases) ->
      List.find_map
        (fun (tc : typcase) ->
          let origin, mixop = case_origin_mixop tc in
          if T.variant_sym origin mixop = ctor then
            Some (mixop, Mixfix.args tc.notation.it, List.length typcases)
          else None)
        typcases
  | _ -> None
