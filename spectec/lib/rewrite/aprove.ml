type verdict = Yes | No | Maybe | Timeout | Error of string

type run = {
  verdict : verdict;
  proof : string;
  captured : string;
  proof_secs : float;
  total_secs : float;
}

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
let obligation_separator = "----------------------------------------"

(* Whole lines only. [Subproc.run] polls the scanners below over the accumulated
   buffer, whose last line is routinely a partial read, and a chunk that happens
   to break after the "MAYBE" of "MAYBEX" would end the run on a verdict the
   tool never gave. *)
let terminated_lines (output : string) : string list =
  match String.rindex_opt output '\n' with
  | None -> []
  | Some last_newline ->
      String.sub output 0 last_newline |> String.split_on_char '\n'

let is_echo_start (line : string) : bool =
  String.starts_with ~prefix:proof_echo_prefix (String.trim line)

let header_lines (output : string) : string list =
  let rec take acc = function
    | [] -> List.rev acc
    | line :: _ when is_echo_start line -> List.rev acc
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

(* The echo opens with the PROOF TREE -- the outline of the steps the run took,
   [(0) ITRS], [(3) UsableRulesProof [EQUIVALENT, 0 ms]], [(9) YES] -- and
   closes it with the first obligation separator, after which every obligation
   is reprinted in full. The tree is the diagnostic: it says how far a proof
   got, which for a run that ends at its deadline is the only thing the verdict
   does not say. On the 4.8MB Program_ok ITRS it reads "(0) ITRS" and stops --
   AProVE never finished converting the system to dependency pairs, so no
   ranking function was ever searched for; on a plain-TRS run that ends the same
   way it lists which DP sub-problems closed with a YES of their own and which
   were still open. The obligations behind it are mostly the input echoed back,
   and the export that produced it is kept anyway.

   Both the tree and its separator arrive in the same instant as the verdict:
   timestamping the stream of that same Program_ok run puts [KILLED], the tree
   and the separator all at 65.00s, with the 6.6MB of obligations behind them
   streaming on for minutes. Capturing the tree is therefore free, which is what
   lets [check] return it unconditionally rather than behind a flag -- waiting
   for the obligations instead would have inflated every seconds column that
   times a call to it. *)
let proof_tree (output : string) : string =
  let rec after_echo_start = function
    | [] -> []
    | line :: rest when is_echo_start line -> rest
    | _ :: rest -> after_echo_start rest
  in
  let rec upto_separator acc = function
    | [] -> List.rev acc
    | line :: _ when String.trim line = obligation_separator -> List.rev acc
    | line :: rest -> upto_separator (line :: acc) rest
  in
  let rec drop_blank = function
    | line :: rest when String.trim line = "" -> drop_blank rest
    | lines -> lines
  in
  let trim_blank lines =
    drop_blank lines |> List.rev |> drop_blank |> List.rev
  in
  String.split_on_char '\n' output
  |> after_echo_start |> upto_separator [] |> trim_blank |> String.concat "\n"

(* Nothing left to wait for once the tree is closed: the answer is on the wire
   (it precedes the echo) and so is the whole of {!proof_tree}, while what
   follows streams for minutes on a large slice. Only the separator ends the
   run, not the verdict line alone, because the tree is worth the instant it
   costs; a run that prints no echo at all -- a parse error, a crashed JVM --
   simply reads to EOF, which for a process with nothing left to say is
   immediate.

   This trims the shutdown tail, not the wait: AProVE announces at its own
   deadline rather than when the proof lands, so a symbol it cannot answer
   instantly costs [budget] whatever this predicate does. On the same [$un_bnot]
   TRS the bare YES prints at 119.8s / 299.7s / 601.6s under budgets 120 / 300 /
   600, while budget 5 already answers YES. A sweep's per-symbol time is set by
   [budget], and what measures a proof is the smallest budget that still
   answers. *)
let proof_printed (output : string) : bool =
  let lines = terminated_lines output in
  List.exists is_echo_start lines
  && List.exists (fun line -> String.trim line = obligation_separator) lines

(* Reading the run to its end instead of stopping at the proof tree is what a
   caller after the OBLIGATIONS asks for, and it is a different kind of run: the
   echo behind the tree reprints the whole rule set once per dependency-graph
   child, so it arrives in gigabytes and keeps arriving for minutes after the
   verdict. Two things follow, and both are the caller's to set. The lines worth
   keeping are selected as they stream ([keep], {!Subproc.run}), so what the
   buffer holds is the fraction that was asked for rather than the whole echo.
   And the deadline has to cover the echo as well as the proof, since AProVE's
   own budget stops the SEARCH, not the printing. *)
type capture = {
  keep : string -> bool;
  done_when : string -> bool;
  echo_slack : int;
}

(* AProVE picks the problem type from the FILE EXTENSION, not the content: an
   ITRS handed over as [.trs] is parsed as a plain TPDB TRS and fails on the
   first [:|:] ("ParseErrors [2,11]"), which reads like a malformed export
   rather than a misnamed one. So the surface that built the text chooses the
   extension. *)
let check ?aprove_bin ?(budget = 300) ?(ext = ".trs") ?capture ~(trs : string)
    () : run =
  let bin = resolve_bin aprove_bin in
  if not (Sys.file_exists bin) then
    {
      verdict =
        Error
          (bin
         ^ " not found; set SPECTEC_APROVE_BIN (see \
            spectec/tools/mfe/README.md)");
      proof = "";
      captured = "";
      proof_secs = 0.0;
      total_secs = 0.0;
    }
  else
    let trs_file = Filename.temp_file "spectec_aprove" ext in
    Fun.protect
      ~finally:(fun () -> try Sys.remove trs_file with Sys_error _ -> ())
      (fun () ->
        let oc = open_out trs_file in
        output_string oc trs;
        close_out oc;
        (* The JVM needs real slack over the in-tool budget; a capture run needs
           the echo's own time on top of that. Without [capture] the run stops
           at the tree, which is on the wire in the same instant as the verdict,
           so the echo costs it nothing. *)
        let timeout =
          budget + 120 + match capture with None -> 0 | Some c -> c.echo_slack
        in
        let started = Unix.gettimeofday () in
        let proof_secs = ref None in
        let done_when output =
          if Option.is_none !proof_secs && proof_printed output then
            proof_secs := Some (Unix.gettimeofday () -. started);
          match capture with
          | None -> proof_printed output
          | Some c -> c.done_when output
        in
        let output, timed_out =
          Subproc.run
            ?keep:(Option.map (fun c -> c.keep) capture)
            ~done_when
            ~cmd:[ Subproc.absolute bin; trs_file; string_of_int budget ]
            ~feed:"" ~timeout ()
        in
        let total_secs = Unix.gettimeofday () -. started in
        let verdict =
          match verdict_line output with
          | Some v -> v
          | None ->
              if timed_out || hit_deadline output then Timeout
              else Error "no YES/NO/MAYBE line in the AProVE output"
        in
        {
          verdict;
          proof = proof_tree output;
          captured = (if Option.is_none capture then "" else output);
          proof_secs = Option.value !proof_secs ~default:total_secs;
          total_secs;
        })
