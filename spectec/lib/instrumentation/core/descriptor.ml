(* Descriptor — plugin declarations for instrumentation handlers.

   Separates static plugin metadata (name, params, parse, checkpoint)
   from the runtime Handler.S interface.

   Handler.S     = what the interpreter calls at runtime.
   Descriptor    = what the CLI uses to build Handler.S instances from flags.
*)

(* Checkpoint operations for handlers with persistent state *)
type checkpoint_ops = {
  snapshot : unit -> bytes; (* Marshal state for checkpointing *)
  restore : bytes -> unit; (* Unmarshal and reload state *)
  merge : bytes -> bytes -> bytes; (* Merge two checkpoint states *)
}

(* A configured, active handler — name + handler + its output destination.
   `name` is the descriptor name, surfaced here so callers (e.g. checkpoint)
   can match by name without opening the handler module.
   `mode` is carried forward so Config.validate_mode can check compatibility
   without re-opening the descriptor.
   `output` is surfaced so Config.close_outputs can call Output.close generically.
   Note: should only expose things callers need to act on generically after
   handler construction. Everything else can be encapsulated inside each handler. *)
type active_handler = {
  name : string;
  mode : [ `IL | `SL | `Both ];
  handler : (module Handler.S);
  output : Output.t;
}

(* Static declaration of a handler plugin:
   what CLI params it accepts, how to parse them into an active_handler,
   and optionally how to checkpoint state. *)
module type S = sig
  val name : string
  val mode : [ `IL | `SL | `Both ]

  (* (param_name, doc) *)
  val params : (string * string) list
  val parse : (string * string option) list -> active_handler option
  val checkpoint : checkpoint_ops option
end

(* First-class module wrapper for heterogeneous descriptor lists *)
type t = (module S)
