type dummy = int

type prem_env = {
  pe_forward  : (Lang.Il.exp * dummy) list;
  pe_backward : (dummy * Lang.Il.exp) list;
  pe_next_id  : int;
}

val empty_prem_env    : prem_env
val resolve_prem_env  : prem_env -> Lang.Il.exp -> Lang.Il.exp
val has_contradiction : prem_env -> bool
val env_of_prems      : Lang.Il.spec -> Lang.Il.prem list -> prem_env
