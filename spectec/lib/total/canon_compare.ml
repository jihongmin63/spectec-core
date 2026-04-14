open Common.Source
open Il
open Xl

let compare_id_canon (id_a : id) (id_b : id) : int =
  compare id_a.it id_b.it

let compare_iter_canon (iter_a : iter) (iter_b : iter) : int =
  match (iter_a, iter_b) with
  | Opt, Opt | List, List -> 0
  | Opt, List -> -1
  | List, Opt -> 1

let rec compare_typ_canon (typ_a : typ) (typ_b : typ) : int =
  compare_typ'_canon typ_a.it typ_b.it

and compare_typ'_canon (typ_a : typ') (typ_b : typ') : int =
  match (typ_a, typ_b) with
  | BoolT, BoolT -> 0
  | BoolT, _ -> -1
  | _, BoolT -> 1
  | NumT numtyp_a, NumT numtyp_b -> compare numtyp_a numtyp_b
  | NumT _, _ -> -1
  | _, NumT _ -> 1
  | TextT, TextT -> 0
  | TextT, _ -> -1
  | _, TextT -> 1
  | VarT (id_a, targs_a), VarT (id_b, targs_b) ->
    let c_id = compare_id_canon id_a id_b in
    if c_id <> 0 then c_id else compare_targs_canon targs_a targs_b
  | VarT _, _ -> -1
  | _, VarT _ -> 1
  | TupleT typs_a, TupleT typs_b -> compare_typs_canon typs_a typs_b
  | TupleT _, _ -> -1
  | _, TupleT _ -> 1
  | IterT (typ_a, iter_a), IterT (typ_b, iter_b) ->
    let c_typ = compare_typ_canon typ_a typ_b in
    if c_typ <> 0 then c_typ else compare_iter_canon iter_a iter_b
  | IterT _, _ -> -1
  | _, IterT _ -> 1
  | FuncT, FuncT -> 0

and compare_targ_canon (targ_a : targ) (targ_b : targ) : int =
  compare_typ'_canon targ_a.it targ_b.it

and compare_targs_canon (targs_a : targ list) (targs_b : targ list) : int =
  List.compare compare_targ_canon targs_a targs_b

and compare_typs_canon (typs_a : typ list) (typs_b : typ list) : int =
  List.compare compare_typ_canon typs_a typs_b

and compare_atom_canon (atom_a : atom) (atom_b : atom) : int =
  Xl.Atom.compare atom_a.it atom_b.it

and compare_var_canon ((id_a, _typ_a, iters_a) : var) ((id_b, _typ_b, iters_b) : var) :
    int =
  let c_id = compare_id_canon id_a id_b in
  if c_id <> 0 then c_id
  else List.compare compare_iter_canon iters_a iters_b

and compare_vars_canon (vars_a : var list) (vars_b : var list) : int =
  List.compare compare_var_canon vars_a vars_b

and compare_iterexp_canon ((iter_a, vars_a) : iterexp) ((iter_b, vars_b) : iterexp) :
    int =
  let c_iter = compare_iter_canon iter_a iter_b in
  if c_iter <> 0 then c_iter else compare_vars_canon vars_a vars_b

and compare_pattern_canon (pattern_a : pattern) (pattern_b : pattern) : int =
  match (pattern_a, pattern_b) with
  | CaseP mixop_a, CaseP mixop_b -> Mixop.compare mixop_a mixop_b
  | CaseP _, _ -> -1
  | _, CaseP _ -> 1
  | ListP `Cons, ListP `Cons -> 0
  | ListP `Cons, _ -> -1
  | _, ListP `Cons -> 1
  | ListP (`Fixed len_a), ListP (`Fixed len_b) -> compare len_a len_b
  | ListP (`Fixed _), _ -> -1
  | _, ListP (`Fixed _) -> 1
  | ListP `Nil, ListP `Nil -> 0
  | ListP `Nil, _ -> -1
  | _, ListP `Nil -> 1
  | OptP `Some, OptP `Some -> 0
  | OptP `Some, _ -> -1
  | _, OptP `Some -> 1
  | OptP `None, OptP `None -> 0

and compare_path_canon (path_a : path) (path_b : path) : int =
  match (path_a.it, path_b.it) with
  | RootP, RootP -> 0
  | RootP, _ -> -1
  | _, RootP -> 1
  | IdxP (path_a, exp_a), IdxP (path_b, exp_b) ->
    let c_path = compare_path_canon path_a path_b in
    if c_path <> 0 then c_path else compare_exp_canon exp_a exp_b
  | IdxP _, _ -> -1
  | _, IdxP _ -> 1
  | SliceP (path_a, exp_l_a, exp_h_a), SliceP (path_b, exp_l_b, exp_h_b) ->
    let c_path = compare_path_canon path_a path_b in
    if c_path <> 0 then c_path
    else
      let c_l = compare_exp_canon exp_l_a exp_l_b in
      if c_l <> 0 then c_l else compare_exp_canon exp_h_a exp_h_b
  | SliceP _, _ -> -1
  | _, SliceP _ -> 1
  | DotP (path_a, atom_a), DotP (path_b, atom_b) ->
    let c_path = compare_path_canon path_a path_b in
    if c_path <> 0 then c_path else compare_atom_canon atom_a atom_b

and compare_arg_canon (arg_a : arg) (arg_b : arg) : int =
  match (arg_a.it, arg_b.it) with
  | ExpA exp_a, ExpA exp_b -> compare_exp_canon exp_a exp_b
  | ExpA _, _ -> -1
  | _, ExpA _ -> 1
  | DefA id_a, DefA id_b -> compare_id_canon id_a id_b

and compare_args_canon (args_a : arg list) (args_b : arg list) : int =
  List.compare compare_arg_canon args_a args_b

and compare_exps_canon (exps_a : exp list) (exps_b : exp list) : int =
  List.compare compare_exp_canon exps_a exps_b

and compare_exp_canon (exp_a : exp) (exp_b : exp) : int =
  match (exp_a.it, exp_b.it) with
    | BoolE b_a, BoolE b_b -> compare b_a b_b
    | BoolE _, _ -> -1
    | _, BoolE _ -> 1
    | NumE n_a, NumE n_b -> compare n_a n_b
    | NumE _, _ -> -1
    | _, NumE _ -> 1
    | TextE t_a, TextE t_b -> compare t_a t_b
    | TextE _, _ -> -1
    | _, TextE _ -> 1
    | VarE id_a, VarE id_b -> compare_id_canon id_a id_b
    | VarE _, _ -> -1
    | _, VarE _ -> 1
    | UnE (unop_a, optyp_a, exp_a), UnE (unop_b, optyp_b, exp_b) ->
      let c_unop = compare unop_a unop_b in
      if c_unop <> 0 then c_unop
      else
        let c_optyp = compare optyp_a optyp_b in
        if c_optyp <> 0 then c_optyp else compare_exp_canon exp_a exp_b
    | UnE _, _ -> -1
    | _, UnE _ -> 1
    | BinE (binop_a, optyp_a, exp_l_a, exp_r_a), BinE (binop_b, optyp_b, exp_l_b, exp_r_b)
      ->
      let c_binop = compare binop_a binop_b in
      if c_binop <> 0 then c_binop
      else
        let c_optyp = compare optyp_a optyp_b in
        if c_optyp <> 0 then c_optyp
        else
          let c_l = compare_exp_canon exp_l_a exp_l_b in
          if c_l <> 0 then c_l else compare_exp_canon exp_r_a exp_r_b
    | BinE _, _ -> -1
    | _, BinE _ -> 1
    | CmpE (cmpop_a, optyp_a, exp_l_a, exp_r_a), CmpE (cmpop_b, optyp_b, exp_l_b, exp_r_b)
      ->
      let c_cmpop = compare cmpop_a cmpop_b in
      if c_cmpop <> 0 then c_cmpop
      else
        let c_optyp = compare optyp_a optyp_b in
        if c_optyp <> 0 then c_optyp
        else
          let c_l = compare_exp_canon exp_l_a exp_l_b in
          if c_l <> 0 then c_l else compare_exp_canon exp_r_a exp_r_b
    | CmpE _, _ -> -1
    | _, CmpE _ -> 1
    | UpCastE (typ_a, exp_a), UpCastE (typ_b, exp_b)
    | DownCastE (typ_a, exp_a), DownCastE (typ_b, exp_b) ->
      let c_typ = compare_typ_canon typ_a typ_b in
      if c_typ <> 0 then c_typ else compare_exp_canon exp_a exp_b
    | UpCastE _, _ -> -1
    | _, UpCastE _ -> 1
    | DownCastE _, _ -> -1
    | _, DownCastE _ -> 1
    | SubE (exp_a, typ_a), SubE (exp_b, typ_b) ->
      let c_exp = compare_exp_canon exp_a exp_b in
      if c_exp <> 0 then c_exp else compare_typ_canon typ_a typ_b
    | SubE _, _ -> -1
    | _, SubE _ -> 1
    | MatchE (exp_a, pattern_a), MatchE (exp_b, pattern_b) ->
      let c_exp = compare_exp_canon exp_a exp_b in
      if c_exp <> 0 then c_exp else compare_pattern_canon pattern_a pattern_b
    | MatchE _, _ -> -1
    | _, MatchE _ -> 1
    | TupleE exps_a, TupleE exps_b -> compare_exps_canon exps_a exps_b
    | TupleE _, _ -> -1
    | _, TupleE _ -> 1
    | CaseE (mixop_a, exps_a), CaseE (mixop_b, exps_b) ->
      let c_mixop = Mixop.compare mixop_a mixop_b in
      if c_mixop <> 0 then c_mixop else compare_exps_canon exps_a exps_b
    | CaseE _, _ -> -1
    | _, CaseE _ -> 1
    | StrE fields_a, StrE fields_b ->
      let compare_field (atom_a, exp_a) (atom_b, exp_b) =
        let c_atom = compare_atom_canon atom_a atom_b in
        if c_atom <> 0 then c_atom else compare_exp_canon exp_a exp_b
      in
      List.compare compare_field fields_a fields_b
    | StrE _, _ -> -1
    | _, StrE _ -> 1
    | OptE None, OptE None -> 0
    | OptE None, _ -> -1
    | _, OptE None -> 1
    | OptE (Some exp_a), OptE (Some exp_b) -> compare_exp_canon exp_a exp_b
    | OptE _, _ -> -1
    | _, OptE _ -> 1
    | ListE exps_a, ListE exps_b -> compare_exps_canon exps_a exps_b
    | ListE _, _ -> -1
    | _, ListE _ -> 1
    | ConsE (exp_h_a, exp_t_a), ConsE (exp_h_b, exp_t_b)
    | CatE (exp_h_a, exp_t_a), CatE (exp_h_b, exp_t_b)
    | MemE (exp_h_a, exp_t_a), MemE (exp_h_b, exp_t_b) ->
      let c_h = compare_exp_canon exp_h_a exp_h_b in
      if c_h <> 0 then c_h else compare_exp_canon exp_t_a exp_t_b
    | ConsE _, _ -> -1
    | _, ConsE _ -> 1
    | CatE _, _ -> -1
    | _, CatE _ -> 1
    | MemE _, _ -> -1
    | _, MemE _ -> 1
    | LenE exp_a, LenE exp_b -> compare_exp_canon exp_a exp_b
    | LenE _, _ -> -1
    | _, LenE _ -> 1
    | DotE (exp_a, atom_a), DotE (exp_b, atom_b) ->
      let c_exp = compare_exp_canon exp_a exp_b in
      if c_exp <> 0 then c_exp else compare_atom_canon atom_a atom_b
    | DotE _, _ -> -1
    | _, DotE _ -> 1
    | IdxE (exp_b_a, exp_i_a), IdxE (exp_b_b, exp_i_b) ->
      let c_b = compare_exp_canon exp_b_a exp_b_b in
      if c_b <> 0 then c_b else compare_exp_canon exp_i_a exp_i_b
    | IdxE _, _ -> -1
    | _, IdxE _ -> 1
    | SliceE (exp_b_a, exp_l_a, exp_h_a), SliceE (exp_b_b, exp_l_b, exp_h_b) ->
      let c_b = compare_exp_canon exp_b_a exp_b_b in
      if c_b <> 0 then c_b
      else
        let c_l = compare_exp_canon exp_l_a exp_l_b in
        if c_l <> 0 then c_l else compare_exp_canon exp_h_a exp_h_b
    | SliceE _, _ -> -1
    | _, SliceE _ -> 1
    | UpdE (exp_b_a, path_a, exp_f_a), UpdE (exp_b_b, path_b, exp_f_b) ->
      let c_b = compare_exp_canon exp_b_a exp_b_b in
      if c_b <> 0 then c_b
      else
        let c_path = compare_path_canon path_a path_b in
        if c_path <> 0 then c_path else compare_exp_canon exp_f_a exp_f_b
    | UpdE _, _ -> -1
    | _, UpdE _ -> 1
    | CallE (id_a, targs_a, args_a), CallE (id_b, targs_b, args_b) ->
      let c_id = compare_id_canon id_a id_b in
      if c_id <> 0 then c_id
      else
        let c_targs = compare_targs_canon targs_a targs_b in
        if c_targs <> 0 then c_targs else compare_args_canon args_a args_b
    | CallE _, _ -> -1
    | _, CallE _ -> 1
    | IterE (exp_a, iterexp_a), IterE (exp_b, iterexp_b) ->
      let c_exp = compare_exp_canon exp_a exp_b in
      if c_exp <> 0 then c_exp else compare_iterexp_canon iterexp_a iterexp_b
