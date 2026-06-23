(* Maude backend: emit a translated spec as an executable, order-sorted Maude
   system module, plus the META-TERM start-term encoding the reflective
   [metaReduce] path runs.

   STUBBED for the new-rewrite skeleton -- reimplement, over
   {!Pipeline.maude_system_of_spec}: sort recovery and op declarations, the
   eq/rl printing, the built-in delegation equations ({!Maude_theory}), and the
   META-TERM encoders ([meta_term_of_value]/[meta_start_app]). *)

let module_of_spec ?(module_name = "SPEC") ?(relations_as_rules = false)
    (spec : Lang.Il.spec) : string =
  ignore module_name;
  ignore relations_as_rules;
  ignore spec;
  failwith "TODO(new-rewrite): reimplement To_maude.module_of_spec"

let module_of_system ?(module_name = "SPEC") ?(relations_as_rules = false)
    (orig : Lang.Il.spec) (system : Rewrite_system.t) : string =
  ignore module_name;
  ignore relations_as_rules;
  ignore orig;
  ignore system;
  failwith "TODO(new-rewrite): reimplement To_maude.module_of_system"

let maude_defined_heads (spec : Lang.Il.spec) : string list =
  ignore spec;
  failwith "TODO(new-rewrite): reimplement To_maude.maude_defined_heads"

let maude_sym (sym : string) : string =
  ignore sym;
  failwith "TODO(new-rewrite): reimplement To_maude.maude_sym"

let meta_start_app (spec : Lang.Il.spec) (rel : string) (args : string list) :
    string =
  ignore spec;
  ignore rel;
  ignore args;
  failwith "TODO(new-rewrite): reimplement To_maude.meta_start_app"

let meta_term_of_value (spec : Lang.Il.spec) (value : Lang.Il.value) : string =
  ignore spec;
  ignore value;
  failwith "TODO(new-rewrite): reimplement To_maude.meta_term_of_value"
