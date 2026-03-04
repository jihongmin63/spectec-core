(** Shared CLI argument parsers, driven by handler descriptors. Flag names are
    --<handler-name>.<param-name>, e.g. --trace.level. *)

open Instrumentation

(** Build a Param for one typed descriptor. Maps each declared param to a
    --name.param_name flag, then calls D.parse. *)
let handler_param (module D : Descriptor.S) :
    Descriptor.active_handler option Core.Command.Param.t =
  let open Core.Command.Param in
  (* One flag per declared parameter *)
  let flag_params =
    List.map
      (fun (param_name, doc) ->
        flag ("--" ^ D.name ^ "." ^ param_name) (optional string) ~doc
        |> map ~f:(fun v -> (param_name, v)))
      D.params
  in
  (* Combine individual flag Params into one Param yielding the full alist.
     both p rest sequences two Params; map cons's each entry onto the list. *)
  let combined =
    List.fold_right
      (fun flag_p rest ->
        both flag_p rest |> map ~f:(fun (entry, entries) -> entry :: entries))
      flag_params (return [])
  in
  map combined ~f:D.parse

(** Shared instrumentation config CLI flags — one set of flags per descriptor *)
let config_flags : Config.t Core.Command.Param.t =
  let open Core.Command.Param in
  List.fold_right
    (fun descriptor rest ->
      both (handler_param descriptor) rest
      |> map ~f:(fun (handler_opt, handler_opts) -> handler_opt :: handler_opts))
    all_descriptors (return [])
  |> map ~f:(List.filter_map Fun.id)
