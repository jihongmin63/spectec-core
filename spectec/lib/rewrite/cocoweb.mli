(** Submit a {!Rewrite_system.t} to CoCoWeb and report the tool's verdict.

    [check] serializes the system to COPS format, hands it to the bundled Python
    client ([tools/cocoweb/cocoweb_client.py], which POSTs it to the CoCoWeb web
    interface), and maps the printed token to a {!verdict}. Confluence (CR) with
    CONFident only; CoCoWeb has no termination category.

    [Timeout] (the tool exhausted its time budget) is reported separately from
    [Maybe] (the tool finished but could not decide). *)

type verdict = Yes | No | Maybe | Timeout | Error of string

val string_of_verdict : verdict -> string

(** [check ?timeout ?client system] runs the CoCoWeb client on [system].

    [timeout] is the per-tool timeout in seconds (default 30). [client] is the
    path to [cocoweb_client.py]; when omitted it is taken from the
    [SPECTEC_COCOWEB_CLIENT] environment variable, then a repo-relative default
    ([Error] if none is found). *)
val check : ?timeout:int -> ?client:string -> Rewrite_system.t -> verdict
