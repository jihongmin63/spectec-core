(** The per-symbol sweep plumbing the analysis subcommands ([confluence],
    [termination], [scc]) share: the usage guards, the slice-root selection, and
    the resumable [--out] TSV protocol. Domain-agnostic: the caller supplies the
    slice-size measure and each row's contents. *)

(** Exit 2 with a usage line unless exactly one of [--all] / [--symbol] was
    given; [cmd] names the subcommand in the message. *)
val require_roots : cmd:string -> all:bool -> symbols:string list -> unit

(** Exit 2 with a usage line unless the single-symbol emit flag ([flag], e.g.
    ["--emit-trs"]) came with exactly one [--symbol] and no [--all]. *)
val require_single_symbol :
  flag:string -> all:bool -> symbols:string list -> unit

(** The requested slice roots: the explicit [--symbol] list, or with [--all]
    every root in [all_roots] ordered smallest slice first (by [slice_size]), so
    a sweep front-loads the tractable results. *)
val roots :
  all:bool ->
  symbols:string list ->
  all_roots:string list ->
  slice_size:(string -> int) ->
  string list

(** Column 0 of every row an existing [--out] TSV records (empty if the file
    does not exist) -- the symbols a resumed sweep skips. *)
val recorded_symbols : string -> string list

(** Run [row_of] over every requested slice root, printing each TSV row as it
    lands and appending it to [out] when given (skipping symbols the file
    already records -- a sweep runs for hours, so [--out] makes it resumable).
    Returns whether any row failed. *)
val rows :
  out:string option ->
  roots:string list ->
  row_of:(string -> string * bool) ->
  bool
