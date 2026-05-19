open Lang.El

type ast_param = {
  p_id  : id;
  p_typ : plaintyp;
}

type ast_hint =
  | GeneratorHint of string
  | GeneralizeHint

type ast_block = {
  name   : string;
  params : ast_param list;
  hints  : ast_hint list;
  goal   : prem option;
  prems  : prem list;
}

type ast_file = ast_block list
