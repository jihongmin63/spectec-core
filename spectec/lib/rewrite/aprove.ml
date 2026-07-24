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

let verdict_of_line (line : string) : verdict option =
  match String.trim line with
  | "YES" -> Some Yes
  | "NO" -> Some No
  | "MAYBE" -> Some Maybe
  | _ -> None

(* The first line that is exactly a verdict token. AProVE prints its proof (or
   failure narrative) around it; only the bare line is the answer. The final
   parse scans the whole buffer, trailing partial line included: a deadline kill
   can cut the output mid-line, and a verdict already printed still counts. *)
let verdict_line (output : string) : verdict option =
  String.split_on_char '\n' output |> List.find_map verdict_of_line

(* Nothing left to wait for: the answer is on the wire, so the rest of the run
   is the JVM shutting down. Worth about a second per symbol -- and no more,
   because AProVE announces at its own deadline rather than when the proof
   lands: on the same `$un_bnot` TRS the bare YES prints at 119.8s / 299.7s /
   601.6s under budgets 120 / 300 / 600, while budget 5 already answers YES. A
   sweep's per-symbol time is therefore set by [budget], and this predicate
   cannot recover it: what measures a proof is the smallest budget that still
   answers.

   Only a newline-terminated line counts. [Subproc.run] polls this over the
   accumulated buffer, whose last line is routinely a partial read, and a chunk
   that happens to break after the "MAYBE" of "MAYBEX" would end the run on a
   verdict the tool never gave. *)
let verdict_printed (output : string) : bool =
  match String.rindex_opt output '\n' with
  | None -> false
  | Some last_newline ->
      String.sub output 0 last_newline
      |> String.split_on_char '\n'
      |> List.exists (fun line -> verdict_of_line line <> None)

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
          Subproc.run ~done_when:verdict_printed
            ~cmd:[ Subproc.absolute bin; trs_file; string_of_int budget ]
            ~feed:"" ~timeout:(budget + 120) ()
        in
        match verdict_line output with
        | Some v -> v
        | None ->
            if timed_out then Timeout
            else Error "no YES/NO/MAYBE line in the AProVE output")
