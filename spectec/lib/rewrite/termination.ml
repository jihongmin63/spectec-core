type verdict = Yes | No | Maybe | Timeout | Degenerate | Error of string

type report = {
  verdict : verdict;
  stats : Unravel.stats option;
  budget : int option;
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

let check ?aprove_bin ?(budget = 300) (system : Rewrite_system.t) : report =
  if system.Rewrite_system.rules = [] then
    { verdict = Degenerate; stats = None; budget = None }
  else
    match Unravel.trs_of_system system with
    | Error msg -> { verdict = Error msg; stats = None; budget = None }
    | Ok (trs, stats) ->
        let run b = Aprove.check ?aprove_bin ~budget:b ~trs () in
        (* A missing binary is an Error at every rung; skip straight to the one
           run whose message the caller reports. *)
        let ladder =
          if Sys.file_exists (Aprove.resolve_bin aprove_bin) then
            budget_ladder ~cap:budget
          else [ budget ]
        in
        let rec probe = function
          | [] -> assert false
          | [ last ] -> (of_aprove (run last), last)
          (* [Yes]/[No] are answers; a bigger budget cannot change them. *)
          | b :: rest -> (
              match run b with
              | (Aprove.Yes | Aprove.No) as v -> (of_aprove v, b)
              | Aprove.Maybe | Aprove.Timeout -> probe rest
              (* An Error is the run failing, not the budget running out (a
                 missing binary, a crashed JVM, output with no verdict line):
                 climbing would just repeat it, so settle it at [cap]. *)
              | Aprove.Error _ -> probe [ budget ])
        in
        let verdict, answered_at = probe ladder in
        { verdict; stats = Some stats; budget = Some answered_at }
