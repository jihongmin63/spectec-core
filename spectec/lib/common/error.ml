open Source

let debug_errors = false

let string_of_located_error (at : region) (msg : string) =
  if at = no_region then msg else string_of_region at ^ "Error: " ^ msg

let warn (at : region) (category : string) (msg : string) =
  Diagnostic.Sink.emit
    (Diagnostic.Sink.global ())
    (Diagnostic.warning ~source:category at msg)
