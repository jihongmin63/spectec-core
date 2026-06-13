(** Run an emitted Maude module ({!To_maude}) on a start term with a local Maude
    binary, parsing the normal form ([reduce]) or reachable solutions
    ([search]).

    Unlike the analysis bridges ({!Cocoweb}/{!Aprove}/{!Muterm}, which return a
    yes/no/maybe verdict), Maude *executes* the rewriting system, so the result
    is the term(s) it rewrites to. The binary is invoked directly via
    [Unix.open_process_in] (no Python client), mirroring {!Aprove}. *)

type mode =
  | Reduce  (** apply equations only ([reduce]); for pure function queries *)
  | Rewrite
      (** apply rules + equations along one fair path ([rewrite]); for a
          deterministic semantics this gives the single result without the
          [search] state-space blow-up *)
  | Search of int option
      (** explore rules + equations ([search =>!]); the optional solution bound
          caps a possibly non-terminating/branching search *)

type result =
  | Reduced of string  (** the normal form (a Maude term, as text) *)
  | Stuck of { term : string; symbols : string list }
      (** the reduction halted at a non-value: [term] still mentions the defined
          symbols [symbols] (functions/relations/ops that should have rewritten
          away), so the rewrite system stopped mid-evaluation *)
  | Solutions of string list  (** the distinct reachable normal forms *)
  | NoSolution
  | Timeout
  | Error of string

val string_of_result : result -> string

(** Whether a result counts as a failed run: a [Stuck] normal form, an [Error],
    a [Timeout], or no solution. Callers use it to set the process exit code. *)
val is_failure : result -> bool

(** Where the binary is resolved from: [maude_bin] argument, then
    [SPECTEC_MAUDE_BIN], then [spectec/tools/maude/maude], then [maude] on
    [PATH]. A missing binary is a clean [Error], not a crash. *)
val resolve_bin : string option -> string

(** [run ?maude_bin ?timeout ?defined_heads ~mode ~module_text ~start ()] writes
    [module_text] plus the [mode] command on [start] to a temp file and runs
    Maude on it. [timeout] is in seconds (0 disables it); a timed-out run is
    [Timeout]. [defined_heads] are the module's reducible symbols (in Maude
    spelling); a [reduce]/[rewrite] normal form still containing one of them is
    reported as [Stuck] rather than [Reduced]. *)
val run :
  ?maude_bin:string ->
  ?timeout:int ->
  ?defined_heads:string list ->
  mode:mode ->
  module_text:string ->
  start:string ->
  unit ->
  result
