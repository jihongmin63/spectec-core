(** A tool adapter only describes a single execution. It never retries or
    decomposes a problem. Add an adapter and register it in {!Termination_tools}
    to support another prover without changing the runner. *)
type verdict = Yes | No | Maybe | Timeout

type problem = {
  rules : (Rewrite_system.term * Rewrite_system.term) list;
  tpdb : string;
}

type document = { extension : string; contents : string }

type adapter = {
  name : string;
  variable : string;
  candidates : string list;
  encode : problem -> (document, string) result;
  command : executable:string -> input:string -> timeout:int -> string list;
  parse : string -> (verdict, string) result;
  finished : string -> bool;
  proof : string -> string;
  dependencies : executable:string -> string list;
}

type tool = { adapter : adapter; executable : string; identity : string }

type run = {
  verdict : (verdict, string) result;
  secs : float;
  stdout : string;
  stderr : string;
  proof : string;
}

val string_of_verdict : verdict -> string
val verdict_of_token : string -> (verdict, string) result

(** Resolve the executable and fingerprint its contents, declared dependencies,
    and environment. This is deliberately conservative: changing the environment
    requires a new TSV. Wrappers must declare otherwise invisible changes via
    SPECTEC_<TOOL>_VERSION; this value participates in the environment digest.
*)
val configure : adapter -> (tool, string) result

(** One execution with an external wall-clock deadline, including any converter
    launched by the adapter command. No retry or extra printing budget. *)
val check : tool:tool -> timeout:int -> problem:problem -> run
