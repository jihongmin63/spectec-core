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

(** Unravel every remaining defined-function binding condition [$f(A) = K(..)]
    into a fresh [crcu]/[crck] chain, moving the binding into a lhs pattern.
    Unraveling REFLECTS but does not preserve confluence, so the result is used
    UPGRADE-ONLY: a YES on the unraveled module proves the original confluent; a
    MAYBE falls back to the original verdict (see
    {!Mfe.check_normalize_upgrade}). *)
val crc_unravel : Rewrite_system.t -> Rewrite_system.t

(** The [--crc-normalize] chain:
    [fold_premise_binders ~aggressive:true |> crc_unravel |> order_conds].
    Opt-in; never part of the shared {!Pipeline.ctrs_of_spec} surface, and never
    seen by execution/termination/ChC. *)
val crc_normalize : Rewrite_system.t -> Rewrite_system.t
