type t = {
  parse :
    includes:string list -> string -> (Lang.Il.value, Spectec.Error.t) result;
  typecheck :
    spec_il:Lang.Il.spec ->
    includes:string list ->
    string ->
    (Lang.Il.Value.t list, Spectec.Error.t) result;
}

let registered = ref None
let register frontend = registered := Some frontend

let get () =
  match !registered with
  | Some frontend -> Ok frontend
  | None ->
      Error
        (Spectec.Error.ConfigError
           ( Spectec.Error.Location.Unknown,
             "--p4 needs the P4 target plugin; run through the `make exe` \
              launcher" ))
