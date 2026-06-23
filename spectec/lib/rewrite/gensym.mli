(** Make the stateful gensym builtins ([$fresh_typeId] / p4-old's [$fresh_tid])
    pure by threading the issuing state through the CTRS.

    The state is the last issued name (a text); issuing appends a prime, so from
    the seed ["FRESH"] the names are [FRESH'], [FRESH''], ... -- a P4 identifier
    cannot contain a prime, so issued names collide neither with source names
    nor with each other. Every symbol that transitively reaches a gensym gains a
    trailing state argument and returns [tuple(result, state')]; its rule
    conditions thread the state in premise order. *)

(** Thread the gensym state through [sys]. The identity when no rule mentions a
    gensym root (e.g. the impty specs), so the pinned goldens are unaffected. *)
val thread : Rewrite_system.t -> Rewrite_system.t

(** The symbols carrying the extra state argument after {!thread}: the gensym
    roots plus everything transitively reaching them. The same answer on the
    pre- and post-threading system, so backends ({!To_maude}, {!Maude_run}) can
    recompute it from the system they were handed. *)
val effectful_syms : Rewrite_system.t -> string list

(** The CTRS spelling of the gensym builtins themselves ({!Prem_env.gensym_ids}
    as function symbols). *)
val root_syms : string list

(** The start-of-run state a runner supplies for the trailing state argument
    (["FRESH"]; the first issued name is then [FRESH']). *)
val seed_text : string
