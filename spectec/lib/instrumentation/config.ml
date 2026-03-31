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

(* Check that all active handlers are compatible with the chosen interpreter mode.
   Returns Error with a message listing incompatible handler names, or Ok (). *)
let validate_mode (config : t) ~sl_mode =
  let interp_mode = if sl_mode then `SL else `IL in
  let incompatible =
    List.filter_map
      (fun { name; mode; _ } ->
        match (interp_mode, mode) with
        | `IL, `SL -> Some (name, "SL only")
        | `SL, `IL -> Some (name, "IL only")
        | _ -> None)
      config
  in
  match incompatible with
  | [] -> Ok ()
  | errs ->
      let mode_str = if sl_mode then "SL" else "IL" in
      let details =
        String.concat ", "
          (List.map (fun (n, reason) -> Printf.sprintf "%s (%s)" n reason) errs)
      in
      Error
        (Printf.sprintf "Instrumentation handlers incompatible with %s mode: %s"
           mode_str details)

(* Directly access output field — same logic as original, now generic *)
let close_outputs (config : t) =
  List.iter (fun a -> Output.close a.output) config
