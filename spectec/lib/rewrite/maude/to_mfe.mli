(** Emit a {!Rewrite_system.t} (the structural analysis system,
    {!Rewrite.rewrite_spec}) as an {b order-sorted} Full-Maude module for the
    analysis tools: the MFE's Church-Rosser/Coherence checkers ({!Mfe}), the old
    MFE's Sufficient Completeness Checker ({!Scc}), and the direct structural
    execution leg ({!Maude_run.run_batch_direct}). *)

(** [module_of_system ?module_name ?full_maude ?prune_signature ?predicates
     ?sig_rules orig sys]. [orig] is the elaborated IL spec (for sort recovery);
    [sys] the structural CTRS.

    [full_maude] (default [true]) wraps the module in Full Maude's
    [(mod ... endm)] parens, needed for the MFE loops to accept it; [false]
    emits a plain stock-Maude module for the direct (non-reflective) execution
    path instead. [prune_signature] (analysis modes only) restricts the declared
    signature to what the rules use: the ops actually applied, the sorts they
    and the rule variable annotations name, and every sort on a subsort path
    between two kept sorts (dropping a path's interior would leave a slice
    ill-sorted; the [< Val] edges are excluded from that closure, or it would
    re-expand to the whole lattice). Pruning never touches the rules, so a
    checker's verdict is preserved; without it every slice carries the whole
    ~460-sort P4 signature.

    [sig_rules] are the rules the signature is recovered from (default: [sys]'s
    own) -- pass the whole system when [sys] is a slice, as {!Mfe.check} does.
*)
val module_of_system :
  ?module_name:string ->
  ?full_maude:bool ->
  ?prune_signature:bool ->
  ?predicates:Maude_sorts.predicate_mode ->
  ?sig_rules:Rewrite_system.rule list ->
  Lang.Il.spec ->
  Rewrite_system.t ->
  string

(** The structural start-term encoding for a direct (non-reflective) [reduce] of
    the analysis module: [start_app orig system rel args] renders [rel(args...)]
    (plus the gensym seed when [system] threads [rel]) in the module's object
    syntax. *)
val start_app :
  Lang.Il.spec -> Rewrite_system.t -> string -> Lang.Il.value list -> string
