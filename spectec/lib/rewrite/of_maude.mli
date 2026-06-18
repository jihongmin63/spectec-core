(** Back-translate a Maude object term (the normal form {!Maude_run} prints as
    [result: <term>]) into SpecTec IL {!Lang.Il.value}s -- the inverse of
    {!To_maude.encode_value}. Used by the same-spec oracle to compare the typing
    RESULT value, not just the PASS/STUCK verdict. *)

exception Parse_error of string

(** Rename gensym-issued fresh identifiers ({!Gensym}, text leaves spelled
    [FRESH...]) to a canonical [FRESH#k] in first-appearance order. The
    interpreter and the Maude translation spell fresh names differently
    ([FRESH__0] vs [FRESH']), so apply this to BOTH sides before
    {!Lang.Il.Eq.eq_values} to compare up to consistent fresh renaming. *)
val canonicalize_fresh : Lang.Il.value list -> Lang.Il.value list

(** Decode the [result:] term of running relation [rel] (by name) against [spec]
    back to the relation's IL OUTPUT value(s), stripping the gensym state
    component when [rel] is threaded ({!Gensym}). The list mirrors the
    interpreter's relation output, so the two are directly
    {!Lang.Il.Eq.eq_values}-comparable. Raises {!Parse_error} when [term] does
    not denote a clean value of [rel]'s output type (e.g. a stuck term). *)
val values_of_result :
  Lang.Il.spec -> rel:string -> string -> Lang.Il.value list
