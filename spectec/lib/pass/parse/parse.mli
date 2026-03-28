type error = Common.Source.region * string
type 'a result = ('a, error) Stdlib.result

val parse_file : string -> Lang.El.spec result
val parse_files : string list -> Lang.El.spec result
