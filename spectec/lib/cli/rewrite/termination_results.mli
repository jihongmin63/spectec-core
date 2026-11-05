(** Versioned six-column TSV: symbol, tool, verdict, stats, wall seconds,
    diagnostic. Text cells use OCaml string escapes. Resume validates the
    complete run identity and every row, and preserves failure exit status. Each
    selected symbol/tool is measured independently. Recorded rows are not rerun
    to regenerate artifacts; keep their original proof directory. *)
type verdict =
  | Answer of Rewrite.Solver.verdict
  | Degenerate
  | Failed of string

type row = {
  symbol : string;
  tool : string;
  verdict : verdict;
  stats : string;
  secs : float;
}

val run :
  out:string option ->
  identity:string ->
  jobs:(string * string) list ->
  row_of:(string -> string -> row) ->
  (bool, string) result
