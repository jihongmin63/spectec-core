(** The P4 frontend that [run --p4] reads programs through. This library cannot
    link the P4 target, whose package depends on [spectec]; the P4 target plugin
    registers its frontend when it loads. *)

type t = {
  parse :
    includes:string list -> string -> (Lang.Il.value, Spectec.Error.t) result;
      (** Parses a P4 program into its IL value. *)
  typecheck :
    spec_il:Lang.Il.spec ->
    includes:string list ->
    string ->
    (Lang.Il.Value.t list, Spectec.Error.t) result;
      (** Runs the interpreter's [Program_ok] on a P4 program. *)
}

val register : t -> unit

(** The registered frontend, or an error when the P4 plugin did not load. *)
val get : unit -> (t, Spectec.Error.t) result
