open Common.Source

type t =
  | PassError of Pass.error
  | InterpError of Interp.error
  | UnhandledException of string
  | TaskParseError of region * string
  | RoundtripError of region * string
  | SpecMismatchError of string * string
  | DirectoryError of string
  | ConfigError of region * string

val string_of_error : t -> string
