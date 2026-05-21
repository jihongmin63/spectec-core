val gen_of_resolved_exp :
  Lang.Il.spec -> Prem_env.prem_env -> Lang.Il.exp -> Lang.Il.Value.t Gen.t

val smart_gen_free_vars :
  Lang.Il.spec ->
  Qc_ir.ir_var list ->
  Lang.Il.prem list ->
  (Lang.Il.id' * Lang.Il.value) list Gen.t
