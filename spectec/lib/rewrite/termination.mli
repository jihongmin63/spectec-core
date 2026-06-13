(** Decide a system's termination, picking the tool by its shape.

    [check] routes an unconditional (plain TRS) system to {!Aprove} and a
    conditional one to {!Muterm}, normalizing the chosen tool's verdict into the
    local {!verdict} and returning it with the {!tool} that produced it. *)

type verdict = Yes | No | Maybe | Timeout | Error of string
type tool = Aprove | Muterm

val string_of_verdict : verdict -> string
val string_of_tool : tool -> string

(** [check ?timeout ?solver ?muterm_client ?aprove_jar system].

    [timeout] is the per-tool timeout in seconds. [solver] selects MuTerm's
    proof method (ignored on the AProVE path; see {!Muterm.check}).
    [muterm_client] and [aprove_jar] locate the respective tools (see
    {!Muterm.check}/{!Aprove.check}); only the one for the selected tool is
    consulted. *)
val check :
  ?timeout:int ->
  ?solver:int ->
  ?muterm_client:string ->
  ?aprove_jar:string ->
  Rewrite_system.t ->
  tool * verdict
