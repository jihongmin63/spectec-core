open Source

let debug_errors = false

let string_of_located_error (at : region) (msg : string) =
  if at = no_region then msg else string_of_region at ^ "Error: " ^ msg

let warn (at : region) (category : string) (msg : string) =
  Printf.eprintf "%s\n%!"
    ((if at = no_region then "" else string_of_region at)
    ^ "Warning:" ^ category ^ ":" ^ msg)
