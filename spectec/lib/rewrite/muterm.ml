(* MuTerm bridge: serialize a rewriting system, run the Python client that POSTs
   it to the MuTerm web interface, and read back the single-token verdict. See
   muterm.mli for the public contract and tools/muterm/muterm_client.py for the
   client. Structurally a sibling of {!Cocoweb} (termination instead of
   confluence). *)

type verdict = Yes | No | Maybe | Timeout | Error of string

let string_of_verdict = function
  | Yes -> "YES"
  | No -> "NO"
  | Maybe -> "MAYBE"
  | Timeout -> "TIMEOUT"
  | Error msg -> "ERROR: " ^ msg

(* Where to find muterm_client.py: explicit argument, then the
   SPECTEC_MUTERM_CLIENT env var, then a repo-relative default. *)
let resolve_client = function
  | Some path -> Ok path
  | None -> (
      match Sys.getenv_opt "SPECTEC_MUTERM_CLIENT" with
      | Some path -> Ok path
      | None -> (
          let candidates =
            [
              "spectec/tools/muterm/muterm_client.py";
              "tools/muterm/muterm_client.py";
            ]
          in
          match List.find_opt Sys.file_exists candidates with
          | Some path -> Ok path
          | None ->
              Error
                "muterm client not found; pass --muterm-client or set \
                 SPECTEC_MUTERM_CLIENT"))

(* Map the client's stdout token to a verdict; unknown output is an error.
   [TIMEOUT] (the tool ran out of time, inconclusive for want of resources) is
   kept distinct from [MAYBE] (the tool finished but could not decide). *)
let verdict_of_output output =
  match String.trim output with
  | "YES" -> Yes
  | "NO" -> No
  | "MAYBE" -> Maybe
  | "TIMEOUT" -> Timeout
  | "" -> Error "muterm client produced no verdict"
  | other -> Error ("unexpected muterm client output: " ^ other)

let run_client client tmpfile timeout solver =
  let cmd =
    Printf.sprintf "python3 %s --timeout %d --solver %d --file %s"
      (Filename.quote client) timeout solver (Filename.quote tmpfile)
  in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> verdict_of_output output
  | Unix.WEXITED n ->
      Error (Printf.sprintf "muterm client exited with status %d" n)
  | Unix.WSIGNALED n | Unix.WSTOPPED n ->
      Error (Printf.sprintf "muterm client killed by signal %d" n)

let check ?(timeout = 30) ?(solver = 0) ?client (system : Rewrite_system.t) =
  match resolve_client client with
  | Error msg -> Error msg
  | Ok client ->
      let tmpfile = Filename.temp_file "spectec_ctrs" ".trs" in
      Fun.protect
        ~finally:(fun () -> try Sys.remove tmpfile with Sys_error _ -> ())
        (fun () ->
          let oc = open_out tmpfile in
          output_string oc (Rewrite_system.string_of_system_tpdb system);
          close_out oc;
          run_client client tmpfile timeout solver)
