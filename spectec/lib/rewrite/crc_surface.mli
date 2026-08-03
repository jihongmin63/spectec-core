(** Analysis-surface normalization passes for the Church-Rosser checker. All are
    analysis-only: the execution surface ({!To_maude}) sees none of them. *)

(** Fold every premise-bound variable back into its rule (inline a deterministic
    output binder; fold a pure-accessor destructuring into the lhs pattern), so
    the CRC is not tripped by the spurious critical pairs the single-sort
    [prod = v] condition rendering raises. [~aggressive] (CRC-only) drops the
    [uses = 1] duplication cap -- still an equivalence, so verdicts transfer
    both ways. An inlined partial-function call keeps its
    [isStuckHead(..) = false] guard. *)
val fold_premise_binders :
  ?aggressive:bool -> Rewrite_system.t -> Rewrite_system.t

(** Restore every rule's conditions to binding order (greedy stable readiness
    schedule): a condition's evaluated side must only use variables bound by the
    lhs or an earlier condition's pattern. Idempotent; warns and keeps source
    order on a genuinely unorderable rule. Wired into the analysis pipeline
    only. *)
val order_conds : Rewrite_system.t -> Rewrite_system.t

(** Unravel every defined-function binding condition [$f(A) = t] -- [t] a
    constructor pattern or a bare fresh variable -- into a fresh [crcu]/[crck]
    chain, moving the binding into a lhs pattern. Unraveling REFLECTS but does
    not preserve confluence, so the result is used UPGRADE-ONLY: a YES on the
    unraveled module proves the original confluent; a MAYBE falls back to the
    original verdict (see {!Mfe.check_normalize_upgrade}). *)
val crc_unravel : Rewrite_system.t -> Rewrite_system.t

(** Every [(defining head, variable)] breaking weak left-linearity: a variable
    occurring twice or more in a rule's lhs and condition patterns that also
    occurs in its rhs or a condition's evaluated side. Empty is the premise
    {!crc_unravel}'s upgrade rests on (Gmeiner-Nishida-Gramlich IWC 2013, Thm
    9); a condition with calls on both sides, having no canonical orientation,
    is counted on both slots and so can only over-report. *)
val wll_violations : Rewrite_system.t -> (string * string) list

(** Which normalization a slice admits: [Unravel_only] where {!wll_violations}
    is empty (cheaper -- one binding site per subject instead of one copy per
    use -- but reflect-only), [Inline_only] otherwise (an equivalence, so it
    needs no premise). *)
type strategy = Unravel_only | Inline_only

val select_strategy : Rewrite_system.t -> strategy
val string_of_strategy : strategy -> string

(** The [--crc-normalize] transform: {!select_strategy}'s choice, then
    {!order_conds}. Opt-in; never part of the shared {!Pipeline.ctrs_of_spec}
    surface, and never seen by execution/termination/ChC. [?strategy] overrides
    the selection (to measure one arm). *)
val crc_normalize : ?strategy:strategy -> Rewrite_system.t -> Rewrite_system.t
