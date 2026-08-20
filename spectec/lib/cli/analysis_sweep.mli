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

(** Every line an existing [--out] TSV records (empty if the file does not
    exist). *)
val recorded_rows : string -> string list

(** Column 0 of every row an existing [--out] TSV records -- the symbols a
    resumed sweep skips. *)
val recorded_symbols : string -> string list

(** [parse_row line] splits a TSV row into its symbol, the columns after it, and
    the symbol whose run produced them -- itself, unless the row carries the
    trailing [=<symbol>] column that marks a copy. *)
val parse_row : string -> string * string * string

(** Run [row_of] over every requested slice root, printing each TSV row as it
    lands and appending it to [out] when given (skipping symbols the file
    already records -- a sweep runs for hours, so [--out] makes it resumable).
    Returns whether any row failed.

    [key] identifies the question a symbol poses, so the sweep asks each
    distinct one once. Symbols in a call-graph SCC reach each other, hence reach
    exactly the same rules, so they slice to the same system and every check
    here is a function of that system alone: the second run can only re-derive
    the first's answer. Symbols after the first with a given key get the
    answering row's columns back verbatim, plus a trailing [=<symbol>] naming
    where they came from -- see {!parse_row}. The copies are real coverage (the
    verdict does hold of them), but their wall clock is the source's, so a
    reader totalling time must drop the marked rows.

    Measured on one ITRS termination sweep: 227 symbols pose 136 distinct
    questions, and 6.3 of its 15.4 hours went to re-answering the other 91. *)
val rows :
  out:string option ->
  roots:string list ->
  key:(string -> string) ->
  row_of:(string -> string * bool) ->
  bool
