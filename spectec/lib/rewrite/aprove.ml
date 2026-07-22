type verdict = Yes | No | Maybe | Timeout | Error of string

let string_of_verdict = function
  | Yes -> "YES"
  | No -> "NO"
  | Maybe -> "MAYBE"
  | Timeout -> "TIMEOUT"
  | Error msg -> "ERROR: " ^ msg

let resolve_bin = function
  | Some path -> path
  | None -> (
      match Sys.getenv_opt "SPECTEC_APROVE_BIN" with
      | Some path -> path
      | None -> (
          let candidates =
            [ "spectec/tools/aprove/runme"; "tools/aprove/runme" ]
          in
          match List.find_opt Sys.file_exists candidates with
          | Some path -> path
          | None -> "spectec/tools/aprove/runme"))

(* The first line that is exactly a verdict token. AProVE prints its proof (or
   failure narrative) around it; only the bare line is the answer. *)
let verdict_line (output : string) : verdict option =
  String.split_on_char '\n' output
  |> List.find_map (fun line ->
         match String.trim line with
         | "YES" -> Some Yes
         | "NO" -> Some No
         | "MAYBE" -> Some Maybe
         | _ -> None)

let check ?aprove_bin ?(budget = 300) ~(trs : string) () : verdict =
  let bin = resolve_bin aprove_bin in
  if not (Sys.file_exists bin) then
    Error
      (bin
     ^ " not found; pass --aprove-bin or set SPECTEC_APROVE_BIN (see \
        spectec/tools/mfe/README.md)")
  else
    let trs_file = Filename.temp_file "spectec_aprove" ".trs" in
    Fun.protect
      ~finally:(fun () -> try Sys.remove trs_file with Sys_error _ -> ())
      (fun () ->
        let oc = open_out trs_file in
        output_string oc trs;
        close_out oc;
        let output, timed_out =
          Subproc.run
            ~cmd:[ Subproc.absolute bin; trs_file; string_of_int budget ]
            ~feed:"" ~timeout:(budget + 120) ()
        in
        match verdict_line output with
        | Some v -> v
        | None ->
            if timed_out then Timeout
            else Error "no YES/NO/MAYBE line in the AProVE output")
