type verdict = Yes | No | Maybe | Timeout | Degenerate | Error of string

type report = {
  verdict : verdict;
  stats : Unravel.stats option;
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
   [cap] call would have made.

   [from] raises the first rung for a region already measured to sit far above
   it. Climbing from 5 burns 5+20+80+320 = 425s before the 1280 rung even
   starts, which is pure waste over a stretch of slices that all answer near
   330s -- there [~from:1280] spends only the answering run. It is a
   measurement trade, not a free speedup: the ladder exists to record the
   SMALLEST budget that still answers, and AProVE announces at its own deadline
   for every symbol that does not finish early, so a rung raised above a
   symbol's real cost reports that rung instead of the cost. Raise it only
   where the region's difficulty is already known, and do not compare the
   seconds it produces against default-ladder rows. *)
let budget_ladder ?(from = 5) ~(cap : int) () : int list =
  let first = max 1 from in
  let rec go b acc =
    if b >= cap then List.rev (cap :: acc) else go (b * 4) (b :: acc)
  in
  if cap <= first then [ cap ] else go first []

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

let check ?aprove_bin ?(budget = 300) ?(budget_from = 5)
    (system : Rewrite_system.t) : report =
  if system.Rewrite_system.rules = [] then
    { verdict = Degenerate; stats = None; secs = None }
  else
    match Unravel.trs_of_system system with
    | Error msg -> { verdict = Error msg; stats = None; secs = None }
    | Ok (trs, stats) ->
        let run b =
          Subproc.timed (fun () -> Aprove.check ?aprove_bin ~budget:b ~trs ())
        in
        (* A missing binary is an Error at every rung; skip straight to the one
           run whose message the caller reports. *)
        let ladder =
          if Sys.file_exists (Aprove.resolve_bin aprove_bin) then
            budget_ladder ~from:budget_from ~cap:budget ()
          else [ budget ]
        in
        (* [secs] times the ANSWERING run alone. The ladder exists so a symbol
           AProVE would search to a large deadline stops at a small budget that
           already answers ($un_bnot is YES at budget 5 in 5.5s, not 1800s); it
           is a mechanism, not a measurement, so the budget it stopped at is not
           reported -- AProVE answers before its deadline for most symbols
           (171/277 finish under their budget), which leaves [secs] the only
           honest per-symbol number. *)
        let rec probe = function
          | [] -> assert false
          | [ last ] -> run last
          | b :: rest ->
              let v, secs = run b in
              if decisive v then (v, secs) else probe rest
        in
        let verdict, secs = probe ladder in
        { verdict = of_aprove verdict; stats = Some stats; secs = Some secs }
