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

module Table = Hashtbl.Make (Entry)

type t = {
  is_cached_func : string -> bool;
  is_cached_rel : string -> bool;
  func_cache : Il.Value.t Table.t ref;
  rel_cache : Il.Value.t list Table.t ref;
}

let make ~is_cached_func ~is_cached_rel =
  {
    is_cached_func;
    is_cached_rel;
    func_cache = ref (Table.create 10000);
    rel_cache = ref (Table.create 10000);
  }

let clear t =
  Table.clear !(t.func_cache);
  Table.clear !(t.rel_cache)

let with_cache cache (id, values) compute =
  let key = (id, values) in
  match Table.find_opt !cache key with
  | Some v -> Ok v
  | None ->
      let result = compute () in
      (match result with Ok v -> Table.add !cache key v | _ -> ());
      result
