val find_rel_in_spec :
  Lang.Il.spec -> string -> (Lang.Il.nottyp * int list) option

val elaborate :
  Lang.Il.spec ->
  Qc_ast.ast_file ->
  (Qc_ir.t, string) result
