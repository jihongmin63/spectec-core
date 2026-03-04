(* Instrumentation configuration.

   Config.t is a list of active handlers, built by parsing CLI flags
   against handler descriptors. Use `to_handlers` to extract the handler list
   for the dispatcher, and `close_outputs` to flush and close output files. *)

open Instrumentation_core
open Descriptor

type t = active_handler list

let default = []

(* Convert config to handler list, auto-registering static dependencies *)
let to_handlers (config : t) =
  let handlers = List.map (fun a -> a.handler) config in
  List.iter
    (fun (module H : Handler.S) ->
      List.iter
        (fun (module M : Instrumentation_static.Static.S) ->
          Instrumentation_static.Static.register (module M))
        H.static_dependencies)
    handlers;
  handlers

(* Directly access output field — same logic as original, now generic *)
let close_outputs (config : t) =
  List.iter (fun a -> Output.close a.output) config
