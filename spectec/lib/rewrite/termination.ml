type verdict = Yes | No | Maybe | Timeout | Degenerate | Error of string
type report = { verdict : verdict; stats : Unravel.stats option }

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

let check ?aprove_bin ?budget (system : Rewrite_system.t) : report =
  if system.Rewrite_system.rules = [] then
    { verdict = Degenerate; stats = None }
  else
    match Unravel.trs_of_system system with
    | Error msg -> { verdict = Error msg; stats = None }
    | Ok (trs, stats) ->
        {
          verdict = of_aprove (Aprove.check ?aprove_bin ?budget ~trs ());
          stats = Some stats;
        }
