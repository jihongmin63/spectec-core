(* Maude lexical layer, shared by both Maude surfaces ({!To_maude} execution and
   {!To_mfe} analysis) so operator and variable identifiers get a single
   spelling. The order-sorted module emission itself lives in the other [maude/]
   backends (which read the IL spec to recover sorts); this layer owns only the
   lexical scrub from a CTRS identifier ({!Rewrite_system.sanitize}) to a
   Maude-safe one. *)

module R = Rewrite_system

(* A CTRS id ([A-Za-z0-9_$]+) to a Maude-safe id: [_] is a mixfix placeholder in
   Maude, so map it to [-] (injective, since CTRS ids never contain [-]). *)
let id (s : string) : string =
  String.map (fun c -> if c = '_' then '-' else c) s

(* A CTRS variable name as a valid Maude variable identifier. A variable built
   from a pretty-printed pattern (a tuple bind ["(value, id)"], an angle-bracket
   type ["pair<K, V>"], a primed name) can carry characters Maude forbids in a
   variable -- spaces, parens, commas, dots, angle brackets. Names already
   confined to [A-Za-z0-9_] (the overwhelming majority) render exactly as the
   [id] mangling; only the rest are run through {!Rewrite_system.sanitize} first
   to become well-formed (and stay distinct). *)
let var (v : string) : string =
  let plain =
    String.for_all
      (function
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
      v
  in
  id (if plain then v else R.sanitize v)
