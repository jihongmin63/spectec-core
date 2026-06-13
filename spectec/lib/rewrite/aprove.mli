(** Submit a {!Rewrite_system.t} to a local AProVE jar and report its verdict.

    [check] serializes the system with {!Rewrite_system.string_of_system_tpdb}
    (which, for an unconditional system, is exactly the WST/TPDB term-rewriting
    surface AProVE expects) and runs
    [java -ea -jar aprove.jar -m wst -t N file], mapping the jar's leading token
    to a {!verdict}. Termination only, and only meaningful for unconditional
    systems (AProVE's [wst] mode is plain TRS); conditional systems are handled
    by {!Muterm}.

    [Yes] means terminating, [No] non-terminating, [Maybe] that AProVE finished
    without deciding, and [Timeout] that it exhausted its time budget. *)

type verdict = Yes | No | Maybe | Timeout | Error of string

val string_of_verdict : verdict -> string

(** [check ?timeout ?jar system] runs AProVE on [system].

    [timeout] is the tool timeout in seconds (default 30, passed as AProVE's
    [-t]). [jar] is the path to [aprove.jar]; when omitted it is taken from the
    [SPECTEC_APROVE_JAR] environment variable, then a repo-relative default
    ([Error] if none is found). Requires [java] on [PATH]. *)
val check : ?timeout:int -> ?jar:string -> Rewrite_system.t -> verdict
