(* Back-translate a Maude object normal form into SpecTec IL values -- the
   inverse of {!To_maude.encode_value}. Powers the result-VALUE oracle.

   STUBBED for the new-rewrite skeleton -- reimplement the forward table read off
   the spec, the object-term parser, the gensym/[map] canonicalization, and the
   relation-output projection. *)

exception Parse_error of string

let canonicalize (values : Lang.Il.value list) : Lang.Il.value list =
  ignore values;
  failwith "TODO(new-rewrite): reimplement Of_maude.canonicalize"

let values_of_result (spec : Lang.Il.spec) ~(rel : string) (term : string) :
    Lang.Il.value list =
  ignore spec;
  ignore rel;
  ignore term;
  failwith "TODO(new-rewrite): reimplement Of_maude.values_of_result"
