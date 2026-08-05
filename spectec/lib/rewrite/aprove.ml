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

(* AProVE leads with the run's own verdict and then echoes the proof:

     KILLED
     proof of /tmp/spectec_aprove….trs
     …
     (6) QDPSizeChangeProof (EQUIVALENT)
     …
     (7)
     YES
     …
     (8) Obligation:

   The echo closes each sub-obligation with a bare verdict line of its own, so
   scanning the whole buffer reads sub-proof (7)'s YES -- one closed DP problem
   among many -- as the run's answer. That silently turns a run AProVE gave up
   on into a termination proof: [KILLED], its deadline marker, is not one of the
   tokens, so the scan walks straight past it into the echo. Only the header,
   the part before the echo begins, carries the answer. *)
let proof_echo_prefix = "proof of "
let deadline_marker = "KILLED"

let header_lines (output : string) : string list =
  let rec take acc = function
    | [] -> List.rev acc
    | line :: _
      when String.starts_with ~prefix:proof_echo_prefix (String.trim line) ->
        List.rev acc
    | line :: rest -> take (line :: acc) rest
  in
  take [] (String.split_on_char '\n' output)

let verdict_line (output : string) : verdict option =
  header_lines output |> List.find_map verdict_of_line

(* AProVE announcing its own deadline is not an answer, and not our timeout
   either -- it is the same "ask again with more time" the budget ladder climbs
   for, so it reports as [Timeout] rather than a parse [Error]. *)
let hit_deadline (output : string) : bool =
  header_lines output
  |> List.exists (fun line -> String.trim line = deadline_marker)

(* Nothing left to wait for once the header is complete: the answer (or the
   deadline marker) is on the wire and everything after it is the proof echo,
   which on a large slice streams for minutes after the verdict has been
   decided. Worth about a second per symbol -- and no more,
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
      |> List.exists (fun line ->
             verdict_of_line line <> None
             || String.trim line = deadline_marker
             || String.starts_with ~prefix:proof_echo_prefix (String.trim line))

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
            if timed_out || hit_deadline output then Timeout
            else Error "no YES/NO/MAYBE line in the AProVE output")
