(** Reduce native META-TERM starts or direct structural starts in one local
    Maude process. Batches preserve each input's result and retry starts
    separately if the shared process fails. *)

type result =
  | Reduced of string
  | Stuck of { term : string; symbols : string list }
  | Timeout
  | Error of string

val string_of_result : result -> string
val is_failure : result -> bool

(** Resolve the executable from an explicit library argument, then
    [SPECTEC_MAUDE_BIN], the project download location, or [PATH]. *)
val resolve_bin : string option -> string

val run :
  ?maude_bin:string ->
  ?timeout:int ->
  ?defined_heads:string list ->
  module_text:string ->
  start:string ->
  unit ->
  result

val run_batch :
  ?maude_bin:string ->
  ?timeout:int ->
  ?defined_heads:string list ->
  module_text:string ->
  starts:string list ->
  unit ->
  result list

(** The marker emitted between starts, also declared by the structural module.
*)
val batch_sep : string

val run_direct :
  ?maude_bin:string ->
  ?timeout:int ->
  ?defined_heads:string list ->
  module_text:string ->
  start:string ->
  unit ->
  result

val run_batch_direct :
  ?maude_bin:string ->
  ?timeout:int ->
  ?defined_heads:string list ->
  module_text:string ->
  starts:string list ->
  unit ->
  result list
