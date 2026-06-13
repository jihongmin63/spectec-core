(* AProVE bridge: serialize a rewriting system to WST/TPDB text and run a local
   AProVE jar to decide its termination. See aprove.mli for the public contract.
   Unlike {!Muterm} (a web interface reached through a Python client), AProVE is
   invoked directly as [java -ea -jar aprove.jar -m wst -t N file.trs]: the jar
   prints the verdict as its first token, so no client wrapper is needed. AProVE
   handles only unconditional (plain TRS) systems; conditional systems go to
   {!Muterm}. *)

type verdict = Yes | No | Maybe | Timeout | Error of string

let string_of_verdict = function
  | Yes -> "YES"
  | No -> "NO"
  | Maybe -> "MAYBE"
  | Timeout -> "TIMEOUT"
  | Error msg -> "ERROR: " ^ msg

(* Where to find aprove.jar: explicit argument, then the SPECTEC_APROVE_JAR env
   var, then a repo-relative default. The jar is large and not checked in, so a
   missing jar is a clean [Error] rather than a crash. *)
let resolve_jar = function
  | Some path -> Ok path
  | None -> (
      match Sys.getenv_opt "SPECTEC_APROVE_JAR" with
      | Some path -> Ok path
      | None -> (
          let candidates =
            [ "spectec/tools/aprove/aprove.jar"; "tools/aprove/aprove.jar" ]
          in
          match List.find_opt Sys.file_exists candidates with
          | Some path -> Ok path
          | None ->
              Error
                "aprove jar not found; pass --aprove-jar or set \
                 SPECTEC_APROVE_JAR"))

(* AProVE leads its WST output with the verdict word, so the first token of the
   first non-empty line is the answer; the proof text that follows is ignored. *)
let verdict_of_output output =
  let first_token =
    String.split_on_char '\n' output
    |> List.find_opt (fun line -> String.trim line <> "")
    |> Option.value ~default:"" |> String.trim |> String.split_on_char ' '
    |> List.hd
  in
  match String.uppercase_ascii first_token with
  | "YES" -> Yes
  | "NO" -> No
  | "MAYBE" -> Maybe
  | "TIMEOUT" -> Timeout
  | "" -> Error "aprove produced no verdict"
  | other -> Error ("unexpected aprove output: " ^ other)

let run_jar jar tmpfile timeout =
  let cmd =
    Printf.sprintf "java -ea -jar %s -m wst -t %d %s" (Filename.quote jar)
      timeout (Filename.quote tmpfile)
  in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> verdict_of_output output
  | Unix.WEXITED n -> Error (Printf.sprintf "aprove exited with status %d" n)
  | Unix.WSIGNALED n | Unix.WSTOPPED n ->
      Error (Printf.sprintf "aprove killed by signal %d" n)

let check ?(timeout = 30) ?jar (system : Rewrite_system.t) =
  match resolve_jar jar with
  | Error msg -> Error msg
  | Ok jar ->
      let tmpfile = Filename.temp_file "spectec_trs" ".trs" in
      Fun.protect
        ~finally:(fun () -> try Sys.remove tmpfile with Sys_error _ -> ())
        (fun () ->
          let oc = open_out tmpfile in
          output_string oc (Rewrite_system.string_of_system_tpdb system);
          close_out oc;
          run_jar jar tmpfile timeout)
