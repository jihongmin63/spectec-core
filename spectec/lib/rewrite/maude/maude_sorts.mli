(** Order-sorted signature recovery shared by both Maude surfaces: the
    executable one ({!To_maude}, native scalar theory) and the analysis one
    ({!To_mfe}, structural scalar theory). A CTRS term carries no sorts
    ({!Rewrite_system}), so each operator's Maude signature is recovered here
    from the original (un-simplified) IL spec's [TypD]/[RelD]/[DecD], using the
    same naming functions {!To_ctrs} used in the rules.

    Everything sits under a single universal supersort [Val]; the internal sort
    inference, the predicate-domain fixpoint and the constructor bookkeeping
    stay private -- callers drive the recovery through {!recover}, read a
    symbol's signature through {!signature}, and print terms through
    {!print_term}. *)

open Lang.Il
module R = Rewrite_system

type scalar_theory = Ctrs_term.scalar_theory = Structural | Native

(** The universal supersort every other sort sits under. *)
val val_sort : string

(** Sort a list, dropping duplicates. *)
val dedup : 'a list -> 'a list

(** Index the spec's type definitions so [VarT] references and aliases resolve
    (last declaration wins, from {!Spec_index}'s ordered view). *)
val type_env : spec -> (string, deftyp') Hashtbl.t

(** A recovered signature table: CTRS symbol -> (argument sorts, result sort).
*)
type sigs = (string, string list * string) Hashtbl.t

(** Recover, from the original spec's type/relation/function declarations, every
    operator's signature and the subsort (sub, super) edges. [rules], when
    given, lets range inference fill gaps for the synthesized
    reflection/iteration helpers that no [TypD] declares (it never overrides a
    declared type). *)
val recover :
  ?rules:R.rule list ->
  scalar_theory ->
  spec ->
  (string, deftyp') Hashtbl.t ->
  sigs * (string * string) list

(** The signature of a symbol at a given arity. A predicate always returns
    [BoolV]; its domain is whatever {!predicate_domains} left in the table (the
    declaration seed alone, if that pass never ran). *)
val signature : sigs -> string -> int -> string list * string

(** The more specific of two sorts under the subsort edges (keeps the first on
    an unrelated clash). *)
val meet : (string * string) list -> string -> string -> string

(** The result sort of a term under a signature lookup. *)
val result_sort : (string -> int -> string list * string) -> R.term -> string

(** Per-rule variable sorts inferred on the fly from a signature lookup and a
    per-variable declared-type [hint] (authoritative when present). *)
val infer_var_sorts :
  (string * string) list ->
  (string -> int -> string list * string) ->
  (string -> string option) ->
  R.rule ->
  (string, string) Hashtbl.t

(** Whether the predicates keep the blunt [Val] domain ([Wide], kept to bisect a
    regression) or the recovered join ([Narrow]). *)
type predicate_mode = Narrow | Wide

(** The subsort edges as inference reads them: the spec injections plus the
    char-list/text bridge. *)
val inference_edges : (string * string) list -> (string * string) list

(** A rule's variables' declared IL types ({!Var_hints}, keyed by the rule's
    defined symbol) as sorts. Authoritative over position inference. *)
val var_hint_fn :
  (string, deftyp') Hashtbl.t ->
  (string, (string * typ') list) Hashtbl.t ->
  R.rule ->
  string ->
  string option

(** Recover each predicate's argument domain into the table (its range is
    [BoolV] by construction): the join of every subject the rules pass it.
    [rules] must be the WHOLE system -- a slice would drop call sites and shrink
    the domain to its seed. *)
val predicate_domains :
  mode:predicate_mode ->
  edges:(string * string) list ->
  hint:(R.rule -> string -> string option) ->
  sigs ->
  R.rule list ->
  unit

(** A variable's inferred sort (defaults to [Val]). *)
val sort_of_var : (string, string) Hashtbl.t -> string -> string

(** Print a term to the Maude surface, spelling each variable with its inferred
    sort and each built-in literal verbatim. *)
val print_term : scalar_theory -> (string, string) Hashtbl.t -> R.term -> string

(** The [ctor] attribute for a symbol's [op] declaration ([""] when it is a
    defined symbol), for an emitted module whose rules define [defined]. Warns
    when a nominal constructor turns out to carry equations. *)
val ctor_attr : scalar_theory -> spec -> defined:string list -> string -> string

(** The distinct (symbol, arity) pairs that occur as application heads anywhere
    in the rules (one pair per arity a symbol is used at). *)
val symbol_arities : scalar_theory -> R.rule list -> (string * int) list

(** Arities of every IL-declared constructor/accessor present in the recovered
    signature table, so ops stay declarable even when no rule mentions the case
    (a start term must still be formable). *)
val ctor_arities : sigs -> spec -> (string * int) list

(** The [List]-precise overload of [cat]/[len], declared alongside the
    [Text]-wide prelude signature so an application over a plain list is still
    well-sorted. *)
val cat_list_sig : string * (string list * string)

val len_list_sig : string * (string list * string)

(** [List < Text], only when [Text] actually appears as a signature sort (an
    empty text is the bare [nil], a char list). *)
val text_subsort_edge :
  (string * (string list * string)) list -> (string * string) list
