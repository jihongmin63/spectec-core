(** Back-translate a Maude object term (the normal form {!Maude_run} prints as
    [result: <term>]) into SpecTec IL {!Lang.Il.value}s -- the inverse of
    {!To_maude.encode_value}. Used by the same-spec oracle to compare the typing
    RESULT value, not just the PASS/STUCK verdict. *)

exception Parse_error of string

(** Normalize the two semantically-irrelevant representation choices that make
    otherwise-equal results compare unequal, so this can be applied to BOTH
    sides before {!Lang.Il.Eq.eq_values}: (1) rename gensym fresh identifiers
    ({!Gensym}, [FRESH…] text leaves -- [FRESH__0] vs [FRESH']) to a canonical
    [FRESH#k] in first-appearance order; (2) sort every [map<K,V>] value's
    entries by [Value.compare] on the key (a map is unordered -- the interpreter
    renders it [VMap.bindings]-sorted, the translation insertion-ordered).
    Neither can repair a genuine content difference. *)
val canonicalize : Lang.Il.value list -> Lang.Il.value list

(** Decode the [result:] term of running relation [rel] (by name) against [spec]
    back to the relation's IL OUTPUT value(s), stripping the gensym state
    component when [rel] is threaded ({!Gensym}). The list mirrors the
    interpreter's relation output, so the two are directly
    {!Lang.Il.Eq.eq_values}-comparable. Raises {!Parse_error} when [term] does
    not denote a clean value of [rel]'s output type (e.g. a stuck term).
    [system] (the translated system the run used, {!Rewrite.maude_system})
    supplies which relations thread the gensym state. *)
val values_of_result :
  Lang.Il.spec ->
  rel:string ->
  system:Rewrite_system.t ->
  string ->
  Lang.Il.value list
