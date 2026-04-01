type error

val error_to_string : error -> string
val parse_files : string list -> (Lang.El.spec, error) result
val elaborate : Lang.El.spec -> (Lang.Il.spec, error) result
val structure : Lang.Il.spec -> Lang.Sl.spec
