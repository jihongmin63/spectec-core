(** Conditional term-rewriting system (CTRS) representation produced from an
    elaborated + simplified IL spec: the data model, term/rule queries, symbol
    slicing, the diagnostic printer, and the shared Maude lexical layer.

    The IL -> CTRS translation is in {!To_ctrs}; the checker-facing pass
    families are {!Crc_surface} (analysis normalization) and {!Scc_surface}
    (over-approximation); the order-sorted Maude module surfaces are {!To_maude}
    (execution) and {!To_mfe} (analysis). All types are concrete: passes and
    backends pattern-match terms and build rules directly. *)

(** A CTRS term: either a variable, or a function symbol applied to zero or more
    argument terms. A nullary application prints as a bare [id]. *)
type term = Var of string | App of string * term list

(** A condition is an equation between two terms. *)
type cond = term * term

(** A (possibly conditional) rewrite rule [lhs -> rhs | conds]. [owise] marks a
    clause that applied "otherwise" (SpecTec [ElsePr]): it fires only when no
    earlier sibling did. The Maude surfaces ({!To_maude}/{!To_mfe}) render it as
    Maude's [owise] equation attribute. *)
type rule = { lhs : term; rhs : term; conds : cond list; owise : bool }

type t = {
  vars : string list;  (** every variable used, deduplicated *)
  rules : rule list;
}

(** Scrub a string into a CTRS-safe identifier: maximal [A-Za-z0-9] runs are
    kept, every other character is replaced by a mnemonic token, tokens are
    joined with [_], an alphabetic lead is guaranteed, and the result is never
    empty. Distinct inputs may still collide (a known first-cut limitation). *)
val sanitize : string -> string

val string_of_term : term -> string

(** A single rule for debug/error messages: [lhs -> rhs], any conditions
    appended as [ | s == t, ...]. Not a surface a tool parses -- only
    human-readable diagnostics. *)
val string_of_rule : rule -> string

(** Every variable occurring in a term, in occurrence order (duplicates kept).
*)
val vars_of_term : term -> string list

(** Every variable occurring in a rule (lhs, rhs and conditions). *)
val vars_of_rule : rule -> string list

(** Occurrences of variable [v] in a term. *)
val count_var : string -> term -> int

(** Substitute variables by name throughout a term (parallel, by association
    list): a variable bound in the list is replaced, all others are kept. *)
val subst : (string * term) list -> term -> term

(** Drop later duplicates, preserving first-occurrence order. *)
val dedup_stable : string list -> string list

(** Rebuild a system from its rules, recomputing the variable list in stable
    first-occurrence order. Every pass that filters, rewrites or extends the
    rule list closes with this. *)
val of_rules : rule list -> t

(** Every function symbol applied anywhere in a term (the [App] heads). *)
val heads_of_term : term -> string list

(** Symbols a rule references: every head in its lhs, rhs and conditions
    (including the lhs root, the symbol the rule defines). *)
val refs_of_rule : rule -> string list

(** The symbol a rule defines: the root head of its lhs, or [None] for a bare
    variable lhs (never produced by the translation). *)
val defined_head : rule -> string option

(** Every symbol the system defines (the root of some rule's lhs): functions,
    relations, and prelude operations -- the symbols that should rewrite away,
    never a value constructor. A normal form still mentioning one of these is
    stuck (reduction halted mid-evaluation). *)
val defined_heads : t -> string list

(** The function symbols reachable from [roots], following each reached symbol's
    defining rules transitively (downward dependency closure). Used both to
    prune unreachable definitions and to slice the system to one symbol's
    dependencies. *)
val reachable_heads : roots:string list -> rule list -> (string, unit) Hashtbl.t

(** Restrict the system to the rules reachable from [roots] (each root's
    defining rules plus their transitive downward dependencies); variables are
    recomputed. *)
val slice : t -> roots:string list -> t

(** A head index over one system, reusable across many [slice_with] calls. A
    sweep slices the same system once per symbol; building the index once turns
    each slice from a full rule-list rescan into a walk of the reachable rules
    alone. *)
type slicer

val make_slicer : t -> slicer

(** [slice_with (make_slicer t) ~roots] equals [slice t ~roots]. *)
val slice_with : slicer -> roots:string list -> t
