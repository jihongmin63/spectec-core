open Lang
(* Cache entry for relation and function invocations *)

module Entry = struct
  type t = string * Il.Value.t list

  let equal (id_a, values_a) (id_b, values_b) =
    id_a = id_b
    && List.length values_a = List.length values_b
    && List.for_all2
         (fun (v_a : Il.Value.t) (v_b : Il.Value.t) ->
           v_a.note.vhash = v_b.note.vhash && Il.Value.compare v_a v_b = 0)
         values_a values_b

  (* Infix operator for combining hash values. *)
  let ( +! ) h1 h2 = (h1 * 65599) + h2

  let hash (id, values) =
    let base_hash = Hashtbl.hash id in
    List.fold_left
      (fun hash (v : Il.Value.t) -> hash +! v.note.vhash)
      base_hash values
end

(* LFU (with LRU tiebreak) cache over Entry keys *)

module Cache = struct
  module Table = Hashtbl.Make (Entry)

  let create ~size = Table.create size
  let clear cache = Table.clear cache
  let find cache key = Table.find_opt cache key
  let add cache key value = Table.add cache key value
end

let is_cached_func = function
  | "subst_type" | "subst_typeDef" | "specialize_typeDef" | "canon"
  | "free_type" | "is_nominal_typeIR" | "bound" | "gen_constraint_type"
  | "merge_constraint" | "merge_constraint'" | "find_matchings"
  | "nestable_struct" | "nestable_struct_in_header" | "find_map" ->
      true
  | _ -> false

let is_cached_rule = function
  | "Sub_expl" | "Sub_expl_canon" | "Sub_expl_canon_neq" | "Sub_impl"
  | "Sub_impl_canon" | "Sub_impl_canon_neq" | "Type_wf" | "Type_alpha" ->
      true
  | _ -> false

let with_cache cache (id, values) compute =
  let key = (id, values) in
  match Cache.find !cache key with
  | Some v -> Ok v
  | None ->
      let result = compute () in
      (match result with Ok v -> Cache.add !cache key v | _ -> ());
      result
