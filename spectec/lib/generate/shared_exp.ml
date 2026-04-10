open Xl
module Il = Lang.Il
open Common.Source

type num = Num.t

(* Texts *)

type text = string

(* Identifiers *)

type id = string

(* Atoms *)

type atom = Atom.t

(* Mixfix operators *)

type mixop = Mixop.t

(* Iterators *)

type iter = Il.iter

(* Hints *)

type hint = { hintid : id; hintexp : El.exp }

(* Types *)

type typ =
  | BoolT                   (* `bool` *)
  | NumT of Num.typ         (* numtyp *)
  | TextT                   (* `text` *)
  | VarT of id * targ list  (* id (`<` list(targ, `,`) `>`)? *)
  | TupleT of typ list      (* `(` list(typ, `,`) `)` *)
  | IterT of typ * iter     (* typ iter *)
  | FuncT                   (* `func` *)

(* Type arguments *)

and targ = typ

(* Variables *)

type var = id * typ * iter list

type nottyp = mixop * typ list

and deftyp =
  | PlainT of typ
  | StructT of typfield list
  | VariantT of typcase list

and typfield = atom * typ
and typcase = nottyp * hint list

(* Values *)

and value =
  | BoolV of bool
  | NumV of Num.t
  | TextV of string
  | StructV of valuefield list
  | CaseV of valuecase
  | TupleV of value list
  | OptV of value option
  | ListV of value list
  | FuncV of id

and valuefield = atom * value
and valuecase = mixop * value list

type numop = Il.numop
and unop = Il.unop
and binop = Il.binop
and cmpop = Il.cmpop
and optyp = Il.optyp

(* Expressions *)

and exp = Il.exp
and exp' =
  | BoolS of bool                         (* bool *)
  | NumS of num                           (* num *)
  | TextS of text                         (* text *)
  | VarS of id                            (* varid *)
  | UnS of unop * optyp * exp             (* unop exp *)
  | BinS of binop * optyp * exp * exp     (* exp binop exp *)
  | CmpS of cmpop * optyp * exp * exp     (* exp cmpop exp *)
  | UpCastS of typ * exp                  (* exp as typ *)
  | DownCastS of typ * exp                (* exp as typ *)
  | SubS of exp * typ                     (* exp `<:` typ *)
  | MatchS of exp * pattern               (* exp `matches` pattern *)
  | TupleS of exp list                    (* `(` exp* `)` *)
  | CaseS of notexp                       (* notexp *)
  | StrS of (atom * exp) list             (* { expfield* } *)
  | OptS of exp option                    (* exp? *)
  | ListS of exp list                     (* `[` exp* `]` *)
  | ConsS of exp * exp                    (* exp `::` exp *)
  | CatS of exp * exp                     (* exp `++` exp *)
  | MemS of exp * exp                     (* exp `<-` exp *)
  | LenS of exp                           (* `|` exp `|` *)
  | DotS of exp * atom                    (* exp.atom *)
  | IdxS of exp * exp                     (* exp `[` exp `]` *)
  | SliceS of exp * exp * exp             (* exp `[` exp `:` exp `]` *)
  | UpdS of exp * path * exp              (* exp `[` path `=` exp `]` *)
  | CallS of id * targ list * arg list    (* $id`<` targ* `>``(` arg* `)` *)
  | HoldS of id * notexp                  (* id `:` notexp `holds` *)
  | IterS of exp * iterexp                (* exp iterexp *)

and notexp = mixop * exp list
and iterexp = iter * var list

(* Patterns *)

and pattern = Il.pattern

(* Path *)

and path =
  | RootP                        (*  *)
  | IdxP of path * exp           (* path `[` exp `]` *)
  | SliceP of path * exp * exp   (* path `[` exp `:` exp `]` *)
  | DotP of path * atom          (* path `.` atom *)

(* Parameters *)

and param =
  (* typ *)
  | ExpP of typ
  (* `def` `$`id ` (`<` list(tparam, `,`) `>`)? (`(` list(param, `,`) `)`)? `:` typ *)
  | DefP of id * tparam list * param list * typ

(* Type parameters *)

and tparam = id

(* Arguments *)

and arg =
  | ExpA of exp   (* exp *)
  | DefA of id    (* `$`id *)

(* Rules *)

and rule = id * notexp * prem list

(* Clauses *)

and clause = arg list * exp * prem list

(* Premises *)

and prem =
  | RulePr of id * notexp          (* id `:` notexp *)
  | IfPr of exp                    (* `if` exp *)
  | ElsePr                         (* `otherwise` *)
  | LetPr of exp * exp             (* `let` exp `=` exp *)
  | IterPr of prem * iterexp       (* prem iterexp *)
  | DebugPr of exp                 (* `debug` exp *)

(* translation *)


module SharedExp = Map.Make(struct
  type t = Il.exp
  let compare lhs rhs =
    String.compare (Lang.Il.Print.string_of_exp lhs) (Lang.Il.Print.string_of_exp rhs)
end)

let add_shared_node map key node =
  if SharedExp.mem key map then map, key
  else SharedExp.add key node map, key

let rec typ'_to_hashed (typ' : Il.typ') : typ =
  match typ' with
  | Il.BoolT -> BoolT
  | Il.NumT numtyp -> NumT numtyp
  | Il.TextT -> TextT
  | Il.VarT (id, targs) -> VarT (id.it, List.map targ_to_hashed targs)
  | Il.TupleT typs -> TupleT (List.map typ_to_hashed typs)
  | Il.IterT (elem_typ, iter) -> IterT (typ_to_hashed elem_typ, iter)
  | Il.FuncT -> FuncT

and typ_to_hashed (typ : Il.typ) : typ =
  typ'_to_hashed typ.it

and targ_to_hashed (targ : Il.targ) : targ =
  typ'_to_hashed targ.it

let pattern_to_hashed (pattern : Il.pattern) : pattern =
  match pattern with
  | Il.CaseP mixop -> CaseP mixop
  | Il.ListP listp -> ListP listp
  | Il.OptP optp -> OptP optp

let var_to_hashed ((id, typ, iters) : Il.var) = id.it, typ_to_hashed typ, iters
let iterexp_to_hashed ((iter, vars) : Il.iterexp) = iter, List.map var_to_hashed vars

let rec filter_id_iter exp =
  match exp.it with
  | Il.VarE id -> Some (id, [])
  | IterE (exp, iterexp) ->
    let iter, _ = iterexp in
    (match filter_id_iter exp with
    | Some (id, iters) -> Some (id, iter :: iters)
    | None -> None)
  | _ -> None


let rec exp_to_hashed map (exp : Il.exp) =
  let exp_to_hashed_aux exp =
    match exp.it with
    | Il.BoolE bool -> add_shared_node map exp (BoolS bool)
    | Il.NumE num -> add_shared_node map exp (NumS num)
    | Il.TextE text -> add_shared_node map exp (TextS text)
    | Il.VarE id -> add_shared_node map exp (VarS id.it)
    | Il.UnE (unop, optyp, subexp) ->
      let updated_map, subexp_id = exp_to_hashed map subexp in
      add_shared_node updated_map exp (UnS (unop, optyp, subexp_id))
    | Il.BinE (binop, optyp, exp1, exp2) ->
      let updated_map, exp1_id = exp_to_hashed map exp1 in
      let updated_map, exp2_id = exp_to_hashed updated_map exp2 in
      add_shared_node updated_map exp (BinS (binop, optyp, exp1_id, exp2_id))
    | Il.CmpE (cmpop, optyp, exp1, exp2) ->
      let updated_map, exp1_id = exp_to_hashed map exp1 in
      let updated_map, exp2_id = exp_to_hashed updated_map exp2 in
      add_shared_node updated_map exp (CmpS (cmpop, optyp, exp1_id, exp2_id))
    | Il.UpCastE (typ, subexp) ->
      let updated_map, subexp_id = exp_to_hashed map subexp in
      add_shared_node updated_map exp (UpCastS (typ_to_hashed typ, subexp_id))
    | Il.DownCastE (typ, subexp) ->
      let updated_map, subexp_id = exp_to_hashed map subexp in
      add_shared_node updated_map exp (DownCastS (typ_to_hashed typ, subexp_id))
    | Il.SubE (subexp, typ) ->
      let updated_map, subexp_id = exp_to_hashed map subexp in
      add_shared_node updated_map exp (SubS (subexp_id, typ_to_hashed typ))
    | Il.MatchE (subexp, pattern) ->
      let updated_map, subexp_id = exp_to_hashed map subexp in
      add_shared_node updated_map exp (MatchS (subexp_id, pattern))
    | Il.TupleE exps ->
      let updated_map, exp_ids = exp_list_to_hashed map exps in
      add_shared_node updated_map exp (TupleS exp_ids)
    | Il.CaseE notexp ->
      let updated_map, notexp' = notexp_to_hashed map notexp in
      add_shared_node updated_map exp (CaseS notexp')
    | Il.StrE fields ->
      let updated_map, fields' = field_list_to_hashed map fields in
      add_shared_node updated_map exp (StrS fields')
    | Il.OptE exp_opt ->
      let updated_map, exp_opt' = opt_exp_to_hashed map exp_opt in
      add_shared_node updated_map exp (OptS exp_opt')
    | Il.ListE exps ->
      let updated_map, exp_ids = exp_list_to_hashed map exps in
      add_shared_node updated_map exp (ListS exp_ids)
    | Il.ConsE (exp1, exp2) ->
      let updated_map, exp1_id = exp_to_hashed map exp1 in
      let updated_map, exp2_id = exp_to_hashed updated_map exp2 in
      add_shared_node updated_map exp (ConsS (exp1_id, exp2_id))
    | Il.CatE (exp1, exp2) ->
      let updated_map, exp1_id = exp_to_hashed map exp1 in
      let updated_map, exp2_id = exp_to_hashed updated_map exp2 in
      add_shared_node updated_map exp (CatS (exp1_id, exp2_id))
    | Il.MemE (exp1, exp2) ->
      let updated_map, exp1_id = exp_to_hashed map exp1 in
      let updated_map, exp2_id = exp_to_hashed updated_map exp2 in
      add_shared_node updated_map exp (MemS (exp1_id, exp2_id))
    | Il.LenE subexp ->
      let updated_map, subexp_id = exp_to_hashed map subexp in
      add_shared_node updated_map exp (LenS subexp_id)
    | Il.DotE (subexp, atom) ->
      let updated_map, subexp_id = exp_to_hashed map subexp in
      add_shared_node updated_map exp (DotS (subexp_id, atom.it))
    | Il.IdxE (exp1, exp2) ->
      let updated_map, exp1_id = exp_to_hashed map exp1 in
      let updated_map, exp2_id = exp_to_hashed updated_map exp2 in
      add_shared_node updated_map exp (IdxS (exp1_id, exp2_id))
    | Il.SliceE (exp1, exp2, exp3) ->
      let updated_map, exp1_id = exp_to_hashed map exp1 in
      let updated_map, exp2_id = exp_to_hashed updated_map exp2 in
      let updated_map, exp3_id = exp_to_hashed updated_map exp3 in
      add_shared_node updated_map exp (SliceS (exp1_id, exp2_id, exp3_id))
    | Il.UpdE (target, path, value) ->
      let updated_map, target_id = exp_to_hashed map target in
      let updated_map, path' = path_to_hashed updated_map path in
      let updated_map, value_id = exp_to_hashed updated_map value in
      add_shared_node updated_map exp (UpdS (target_id, path', value_id))
    | Il.CallE (id, targs, args) ->
      let updated_map, args' = args_to_hashed map args in
      let targs' = List.map targ_to_hashed targs in
      add_shared_node updated_map exp (CallS (id.it, targs', args'))
    | Il.HoldE (id, notexp) ->
      let updated_map, notexp' = notexp_to_hashed map notexp in
      add_shared_node updated_map exp (HoldS (id.it, notexp'))
    | Il.IterE (subexp, iterexp) ->
      let updated_map, subexp_id = exp_to_hashed map subexp in
      add_shared_node updated_map exp (IterS (subexp_id, iterexp_to_hashed iterexp))
  in
  match filter_id_iter exp with
  | Some (id, iters) ->
    let id_hash =
      let rec search_hashmap bindings =
        match bindings with
        | [] -> None
        | (key, value) :: bindings ->
          match value with
          | VarS id_value -> if id.it = id_value then Some key else search_hashmap bindings
          | _ -> search_hashmap bindings
      in
      search_hashmap (SharedExp.bindings map)
    in
    (* repeat iters times *)
    (match id_hash with
    | Some id -> 
      let rec ascend id iters =
        match iters with
        | [] -> map, id
        | _ :: iters -> (
          let rec search_hashmap bindings =
            match bindings with
            | [] -> None
            | (key, value) :: bindings ->
              match value with
              | IterS (id_value, _) -> if id = id_value then Some key else search_hashmap bindings
              | _ -> search_hashmap bindings
          in
          match search_hashmap (SharedExp.bindings map) with
          | Some id -> ascend id iters
          | None -> 
            (* may fail *)
            exp_to_hashed_aux exp
        )
      in
      ascend id iters
    | None -> exp_to_hashed_aux exp)
  | None -> exp_to_hashed_aux exp

and exp_list_to_hashed map exps =
  List.fold_left (fun (current_map, acc_ids) exp ->
    let updated_map, exp_id = exp_to_hashed current_map exp in
    updated_map, exp_id :: acc_ids
  ) (map, []) exps
  |> fun (updated_map, rev_ids) -> updated_map, List.rev rev_ids

and opt_exp_to_hashed map = function
  | None -> map, None
  | Some exp ->
    let updated_map, exp_id = exp_to_hashed map exp in
    updated_map, Some exp_id

and field_list_to_hashed map fields =
  List.fold_left (fun (current_map, acc_fields) (atom, exp) ->
    let updated_map, exp_id = exp_to_hashed current_map exp in
    updated_map, (atom.it, exp_id) :: acc_fields
  ) (map, []) fields
  |> fun (updated_map, rev_fields) -> updated_map, List.rev rev_fields

and notexp_to_hashed map (mixop, exps) =
  let updated_map, exp_ids = exp_list_to_hashed map exps in
  updated_map, (mixop, exp_ids)

and path_to_hashed map (path : Il.path) =
  match path.it with
  | Il.RootP -> map, RootP
  | Il.IdxP (base_path, idx_exp) ->
    let updated_map, base_path' = path_to_hashed map base_path in
    let updated_map, idx_exp_id = exp_to_hashed updated_map idx_exp in
    updated_map, IdxP (base_path', idx_exp_id)
  | Il.SliceP (base_path, start_exp, end_exp) ->
    let updated_map, base_path' = path_to_hashed map base_path in
    let updated_map, start_exp_id = exp_to_hashed updated_map start_exp in
    let updated_map, end_exp_id = exp_to_hashed updated_map end_exp in
    updated_map, SliceP (base_path', start_exp_id, end_exp_id)
  | Il.DotP (base_path, atom) ->
    let updated_map, base_path' = path_to_hashed map base_path in
    updated_map, DotP (base_path', atom.it)

and arg_to_hashed map (arg : Il.arg) =
  match arg.it with
  | Il.ExpA exp ->
    let updated_map, exp_id = exp_to_hashed map exp in
    updated_map, ExpA exp_id
  | Il.DefA id -> map, DefA id.it

and args_to_hashed map args =
  List.fold_left (fun (current_map, acc_args) arg ->
    let updated_map, arg' = arg_to_hashed current_map arg in
    updated_map, arg' :: acc_args
  ) (map, []) args
  |> fun (updated_map, rev_args) -> updated_map, List.rev rev_args