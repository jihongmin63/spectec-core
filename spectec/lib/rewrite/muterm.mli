(** Submit a {!Rewrite_system.t} to MuTerm and report the tool's verdict.

    [check] serializes the system with {!Rewrite_system.string_of_system_tpdb}
    (the TPDB conditional form MuTerm accepts; its parser crashes on the COPS
    surface used for confluence), hands it to the bundled Python client
    ([tools/muterm/muterm_client.py], which POSTs it to the MuTerm web interface
    at [filter.php]), and maps the printed token to a {!verdict}. Termination
    only.

    [Yes] means the system was proved terminating, [No] non-terminating, and
    [Maybe] that the tool finished without deciding. [Timeout] (the tool
    exhausted its time budget) is reported separately from [Maybe]. *)

type verdict = Yes | No | Maybe | Timeout | Error of string

val string_of_verdict : verdict -> string

(** [check ?timeout ?solver ?client system] runs the MuTerm client on [system].

    [timeout] is the tool timeout in seconds (default 30). [solver] selects the
    proof method MuTerm uses: [0] automatic/best (default), [1] polynomials, [2]
    RPO, [3] dependency pairs. [client] is the path to [muterm_client.py]; when
    omitted it is taken from the [SPECTEC_MUTERM_CLIENT] environment variable,
    then a repo-relative default ([Error] if none is found). *)
val check :
  ?timeout:int -> ?solver:int -> ?client:string -> Rewrite_system.t -> verdict
