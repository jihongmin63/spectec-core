(** Diagnostic rendering — single layout, optional ANSI color.

    Separate from {!Diagnostic} because rendering touches file I/O (for source
    snippets) and ANSI styling, neither of which belongs in the structured
    diagnostic representation. The same layout is produced whether color is on
    or off; only the styling differs. *)

(** Render a single diagnostic. The [cache] is used to look up source snippets
    for the location section. *)
val render : ansi:Ansi.t -> cache:Source_cache.t -> Diagnostic.t -> string

(** Render a bag of diagnostics, one per entry, in sorted order. Manages an
    internal {!Source_cache.t} so callers don't have to. *)
val render_bag : ansi:Ansi.t -> Diagnostic.Bag.t -> string
