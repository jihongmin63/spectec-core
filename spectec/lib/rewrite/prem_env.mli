open Lang.Il

(** Premise environment: a union-find over IL expressions mapping each
    expression to the canonical (most specific) member of its equivalence class.
    Built from a rule's or clause's premises, then consumed by {!Simplify} to
    expand variables into structure and prune redundant premises. *)

type prem_env

(** Build the environment for [prems], threading the whole [spec] so relation
    outputs and [matches] constraints can be reconstructed. *)
val env_of_prems : spec -> prem list -> prem_env

(** Canonical form of [exp] in the environment ([exp] itself when unknown). *)
val resolve_prem_env : prem_env -> exp -> exp

(** The recorded (non-canonical, canonical) equivalences. *)
val pairs_of_env : prem_env -> (exp * exp) list

(** Look up a relation's notation type and input-position indices by name. *)
val find_rel_in_spec : spec -> string -> (nottyp * int list) option

(** The stateful gensym builtins ([$fresh_typeId] / p4-old's [$fresh_tid]).
    Calls reaching one of these mint a new name per evaluation, so the env keeps
    them opaque (never canonical, never duplicated by substitution); {!Gensym}
    later threads a gensym state through them, relying on each such call
    occurring exactly once in its rule. *)
val gensym_ids : string list

(** The base numeric type [typ'] denotes, resolved through named [PlainT]
    aliases (`syntax byte = nat` resolves to `nat`); [None] when it is not, even
    transitively, a number. *)
val resolve_num_typ : spec -> typ' -> Xl.Num.typ option

(** Whether a cast from [source] to [target] crosses the nat/int boundary
    (alias-resolved). Such a cast changes the CTRS representation (bare Peano
    vs. sign-magnitude), so it must survive into the translation instead of
    being stripped as a transparent view. *)
val is_num_cast : spec -> typ' -> typ' -> bool

(** Whether [exp] (resp. the identifier [id]) is a synthetic relation-output
    placeholder. These steer narrowing inside the environment but must never
    reach the rewritten spec. *)
val is_hidden_out_var : exp -> bool

val is_hidden_out_id : id -> bool

(** Concrete structure: any expression other than a bare variable. *)
val is_structural : exp -> bool

(** Every expression equivalent to [exp] in the environment (its equivalence
    class), [[]] when [exp] has no recorded equivalence. Used to spot a
    companion premise that already pins down a [matches] subject's shape. *)
val class_members : prem_env -> exp -> exp list

(** Reconstruct the structure a [matches] pattern pins down for [subj]: for a
    [CaseP] the variant constructor with fresh, deterministically-named field
    variables; for the payload-free [OptP `None]/[ListP `Nil] the empty shape.
    [None] for payload-carrying [OptP `Some]/[ListP `Cons]/[`Fixed] and unknown
    variants. Used to fold a [matches] guard into the binder of the [let] that
    defines its subject. *)
val reconstruct_pattern : spec -> exp -> pattern -> exp option

(** Substitution pairs that reconstruct a struct-typed input from a field-access
    constraint. For each variable in [roots] (the clause's head-bound inputs)
    whose field chain `r.a1...aN` is equated to a concrete shape, returns
    [(r, struct)] where [struct] is a struct literal carrying that shape on the
    chain field and fresh variables elsewhere -- so substituting [r] folds the
    field destructure into a struct head pattern. *)
val hoist_pairs : spec -> prem_env -> Free.t -> (exp * exp) list

(** [subst_exp from_e to_e in_e] replaces every occurrence of [from_e] (compared
    by IL equality) with [to_e] throughout [in_e]. *)
val subst_exp : exp -> exp -> exp -> exp

(** The element variables an iteration's binder list binds. *)
val binder_ids : var list -> Common.Domain.IdSet.t
