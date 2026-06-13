(* Termination check dispatcher. Routes a system to the right tool by its shape:
   an unconditional (plain TRS) system goes to {!Aprove} (the WST-mode jar, the
   stronger competition tool for plain rewriting), and a conditional one to
   {!Muterm} (which handles CTRSs). The two tools' verdicts share a shape, so
   they are normalized into one local {!verdict} and returned alongside the
   {!tool} that produced it, letting callers report which tool decided. *)

type verdict = Yes | No | Maybe | Timeout | Error of string
type tool = Aprove | Muterm

let string_of_verdict = function
  | Yes -> "YES"
  | No -> "NO"
  | Maybe -> "MAYBE"
  | Timeout -> "TIMEOUT"
  | Error msg -> "ERROR: " ^ msg

let string_of_tool = function Aprove -> "aprove" | Muterm -> "muterm"

let of_aprove : Aprove.verdict -> verdict = function
  | Aprove.Yes -> Yes
  | Aprove.No -> No
  | Aprove.Maybe -> Maybe
  | Aprove.Timeout -> Timeout
  | Aprove.Error msg -> Error msg

let of_muterm : Muterm.verdict -> verdict = function
  | Muterm.Yes -> Yes
  | Muterm.No -> No
  | Muterm.Maybe -> Maybe
  | Muterm.Timeout -> Timeout
  | Muterm.Error msg -> Error msg

let check ?timeout ?solver ?muterm_client ?aprove_jar
    (system : Rewrite_system.t) : tool * verdict =
  if Rewrite_system.is_unconditional system then
    (Aprove, of_aprove (Aprove.check ?timeout ?jar:aprove_jar system))
  else
    ( Muterm,
      of_muterm (Muterm.check ?timeout ?solver ?client:muterm_client system) )
