type verdict = Yes | No | Maybe | Timeout | Degenerate | Error of string

type report = {
  verdict : verdict;
  stats : Unravel.stats option;
  budget : int option;
  secs : float option;
}

let string_of_verdict = function
  | Yes -> "YES"
  | No -> "NO"
  | Maybe -> "MAYBE"
  | Timeout -> "TIMEOUT"
  | Degenerate -> "DEGENERATE"
  | Error msg -> "ERROR: " ^ msg

let of_aprove : Aprove.verdict -> verdict = function
  | Aprove.Yes -> Yes
  | Aprove.No -> No
  | Aprove.Maybe -> Maybe
  | Aprove.Timeout -> Timeout
  | Aprove.Error msg -> Error msg

(* AProVE announces its verdict at its own deadline rather than when the proof
   lands (see {!Aprove.verdict_printed}), so one run at a fixed budget costs
   that budget whatever the proof was worth -- and the seconds it reports
   measure our setting, not the difficulty. Climbing instead from a small
   budget and stopping at the first decisive answer records the SMALLEST budget
   that still answers, an upper bound on the real cost.

   Rungs grow by 4 so the total spent staying under an answer is about a third
   of the next rung: overshooting a proof's cost is cheap, and the ladder ends
   at [cap] so a search that runs out of rungs makes exactly the run a single
   [cap] call would have made. *)
let budget_ladder ~(cap : int) : int list =
  let rec go b acc =
    if b >= cap then List.rev (cap :: acc) else go (b * 4) (b :: acc)
  in
  if cap <= 5 then [ cap ] else go 5 []

(* Which verdicts end the climb. [Yes] and [No] are answers about the TRS and a
   larger budget cannot overturn them.

   Everything else is "ask again with more time", INCLUDING [Error]. A small
   budget is not just a MAYBE machine: on the $bitacc_offset_op TRS AProVE runs
   out at budget 5 having printed nothing but its narrative, so
   {!Aprove.check} reports "no YES/NO/MAYBE line" -- and the very same TRS
   answers YES at budget 20. Treating that as a permanent failure (it looks like
   one -- a crashed JVM reads identically) would skip the rung that answers and
   fall through to the cap. The one error that really is permanent, a missing
   binary, is checked once before the climb starts. *)
let decisive : Aprove.verdict -> bool = function
  | Aprove.Yes | Aprove.No -> true
  | Aprove.Maybe | Aprove.Timeout | Aprove.Error _ -> false

let check ?aprove_bin ?(budget = 300) (system : Rewrite_system.t) : report =
  if system.Rewrite_system.rules = [] then
    { verdict = Degenerate; stats = None; budget = None; secs = None }
  else
    match Unravel.trs_of_system system with
    | Error msg ->
        { verdict = Error msg; stats = None; budget = None; secs = None }
    | Ok (trs, stats) ->
        let run b =
          Subproc.timed (fun () -> Aprove.check ?aprove_bin ~budget:b ~trs ())
        in
        (* A missing binary is an Error at every rung; skip straight to the one
           run whose message the caller reports. *)
        let ladder =
          if Sys.file_exists (Aprove.resolve_bin aprove_bin) then
            budget_ladder ~cap:budget
          else [ budget ]
        in
        (* Only the ANSWERING rung is timed into the report. The rungs below it
           found nothing, so folding their deadlines in would report the
           search's cost as the proof's -- the very confusion this search was
           built to remove. (The whole search's cost stays observable: it is the
           sum over the rungs the reported budget names.) *)
        let rec probe = function
          | [] -> assert false
          | [ last ] ->
              let v, secs = run last in
              (of_aprove v, last, secs)
          | b :: rest ->
              let v, secs = run b in
              if decisive v then (of_aprove v, b, secs) else probe rest
        in
        let verdict, answered_at, secs = probe ladder in
        {
          verdict;
          stats = Some stats;
          budget = Some answered_at;
          secs = Some secs;
        }
