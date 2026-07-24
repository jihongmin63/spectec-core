(** Weak left-linearity (WLL) of a conditional rewrite system: the missing
    premise of the [--crc-normalize] upgrade.

    Unraveling only REFLECTS confluence in general; it PRESERVES it -- so a
    normalized YES may be upgraded to the original -- for oriented DCTRSs that
    are additionally weakly left-linear and whose unraveled system is of the
    [U_conf] shape (GNG, IWC 2013, Thm 9; Nishida-Sakai-Sakabe, LMCS 2012).
    {!Mfe.check_normalize_upgrade} performs that upgrade today without checking
    WLL; this module measures the premise.

    {b WLL}: the unraveling lifts each condition's PATTERN side into a helper
    rule's lhs, so a rule's "pattern basket" spans its [lhs] and every condition
    pattern [t_1..t_k], while its "right basket" spans [rhs] and every evaluated
    side [s_1..s_k]. A rule is weakly left-linear when no variable occurring
    twice or more in the pattern basket occurs in the right basket at all.
    (Left-linearity is the special case with no repeated pattern variable.)

    Which side of a condition is which is not a notational preference:
    {!Rewrite_system.orient_conds} fixes the convention that a condition
    [(s, t)] evaluates [s] and matches the result against the pattern [t], and
    that a defined symbol therefore never heads [t]. This checker reads the
    split as given -- valid only because that pass runs in
    {!Pipeline.ctrs_of_spec} before anything here sees the system.

    Read-only: nothing here rewrites a system. Each violation is additionally
    classified by whether merely re-orienting conditions could remove it, which
    is the datum a re-orientation pass would be built on. *)

(** Why a rule's violation can or cannot be removed by re-orienting its
    conditions.

    [Flippable] is class A: an assignment of condition directions exists that
    leaves no violation, and it moves no defined symbol onto a pattern side. Any
    [Blocked_*] means no assignment can help; they differ in what pins the
    offending variable down:

    - [Blocked_rule] -- the [lhs]/[rhs] contributions alone already violate, so
      condition directions are irrelevant.
    - [Blocked_defined] -- the pinning condition evaluates a defined symbol, and
      flipping it would put that symbol on a pattern side, breaking the
      {!Rewrite_system.orient_conds} invariant (which severs the unravel chain
      and hides the recursion behind it from the termination prover -- a worse
      problem than the one being fixed).
    - [Blocked_bothsides] -- the offending variable occurs on BOTH sides of one
      condition, so that condition feeds both baskets whichever way it faces.
    - [Blocked_binder] -- the pinning condition binds a variable that is not yet
      bound at that point, so its direction is forced by the binding order.
    - [Blocked_bothcall] -- the pinning condition is a call on both sides, where
      {!Rewrite_system.orient_conds} already had no good orientation to pick.
    - [Blocked_combination] -- no single condition pins the variable, yet no
      global assignment satisfies every variable at once.
    - [Unknown_cap] -- the rule has more freely-orientable conditions than the
      exhaustive search bound, so the answer is NOT KNOWN rather than negative.
      Reported explicitly, never silently dropped. *)
type cls =
  | Flippable
  | Blocked_rule
  | Blocked_defined
  | Blocked_bothsides
  | Blocked_binder
  | Blocked_bothcall
  | Blocked_combination
  | Unknown_cap

val string_of_cls : cls -> string

(** One rule's offending variables sharing one classification. [rule_index] is
    the rule's position in the system it was checked in; [head] is the symbol it
    defines. *)
type violation = {
  rule_index : int;
  head : string;
  vars : string list;
  cls : cls;
}

(** [violations = []] means the rule IS weakly left-linear. [orientation] is
    [Some flips] exactly for a [Flippable] rule: one boolean per condition, in
    the rule's own condition order, [true] where the condition must be read the
    other way round. The remaining counts describe the rule's conditions:
    [free_conds] are the ones a re-orientation may flip (both sides already
    bound, neither a call), [bothcall_conds] the ones with a defined symbol on
    both sides. *)
type rule_report = {
  violations : violation list;
  orientation : bool list option;
  conds : int;
  free_conds : int;
  bothcall_conds : int;
}

(** A slice's verdict: [Clean] when every rule is weakly left-linear,
    [Flippable_all] when every violating rule is class A, [Blocked] when none
    is, [Partial] in between. Only [Clean] licenses the unravel upgrade;
    [Flippable_all] says a re-orientation pass would reach [Clean]. *)
type slice_verdict = Clean | Flippable_all | Partial | Blocked

val string_of_verdict : slice_verdict -> string

(** [check_rule ~defined r] classifies one rule. [defined] decides whether a
    symbol is a defined one (a call) -- the same predicate
    {!Rewrite_system.orient_conds} uses, so pass {!Rewrite_system.defined_heads}
    of the enclosing system, not of the rule. [index] only labels the resulting
    violations. *)
val check_rule :
  ?index:int -> defined:(string -> bool) -> Rewrite_system.rule -> rule_report

(** [check_system t] is [check_rule] over [t]'s rules in order, with [defined]
    taken from [t]. *)
val check_system : Rewrite_system.t -> rule_report list

val slice_verdict : rule_report list -> slice_verdict

(** [report t ~syms] is [(tsv, summary)]: a tab-separated row per slice that has
    at least one conditional rule (a [#]-prefixed header line first), and a
    human-readable whole-system summary naming every [Unknown_cap] rule. The
    slices are [t] sliced at each of [syms]; a rule shared by several slices is
    checked once. *)
val report : Rewrite_system.t -> syms:string list -> string * string
