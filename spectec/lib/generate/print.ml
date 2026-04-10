open Xl
open Shared_exp
open Common.Source

(* Numbers *)

let string_of_num = Num.string_of_num

(* Texts *)

let string_of_text text = text

(* Identifiers *)

let string_of_id id = id

(* Atoms *)

let string_of_atom atom =
  match atom with
  | Atom.SilentAtom _ -> ""
  | _ -> Atom.string_of_atom atom |> String.lowercase_ascii

let string_of_atoms atoms =
  match atoms with
  | [] -> ""
  | _ -> atoms |> List.map string_of_atom |> String.concat ""

let string_of_atom_phrase atom =
  match atom.it with
  | Atom.SilentAtom _ -> ""
  | _ -> Atom.string_of_atom atom.it |> String.lowercase_ascii

let string_of_atom_phrases atoms =
  match atoms with
  | [] -> ""
  | _ -> atoms |> List.map string_of_atom_phrase |> String.concat ""

(* Mixfix operators *)

let string_of_mixop mixop = Mixop.string_of_mixop mixop

(* Iterators *)

let string_of_iter iter = match iter with Il.Opt -> "?" | List -> "*"

(* References *)

let string_of_exp_ref exp = "#" ^ string_of_int exp

(* Types *)

let rec string_of_typ typ =
  match typ with
  | BoolT -> "bool"
  | NumT numtyp -> Num.string_of_typ numtyp
  | TextT -> "text"
  | VarT (id, targs) -> string_of_id id ^ string_of_targs targs
  | TupleT typs -> "(" ^ string_of_typs ", " typs ^ ")"
  | IterT (typ, iter) -> string_of_typ typ ^ string_of_iter iter
  | FuncT -> "func"

and string_of_typs sep typs = String.concat sep (List.map string_of_typ typs)

and string_of_targ targ = string_of_typ targ

and string_of_targs targs =
  match targs with
  | [] -> ""
  | _ -> "<" ^ String.concat ", " (List.map string_of_targ targs) ^ ">"

(* Variables *)

let string_of_var (id, _typ, iters) =
  string_of_id id ^ String.concat "" (List.map string_of_iter iters)

(* Operators *)

let string_of_unop = function
  | #Bool.unop as op -> Bool.string_of_unop op
  | #Num.unop as op -> Num.string_of_unop op

let string_of_binop = function
  | #Bool.binop as op -> Bool.string_of_binop op
  | #Num.binop as op -> Num.string_of_binop op

let string_of_cmpop = function
  | #Bool.cmpop as op -> Bool.string_of_cmpop op
  | #Num.cmpop as op -> Num.string_of_cmpop op

(* Patterns *)

let string_of_pattern pattern =
  match pattern with
  | Il.CaseP mixop -> string_of_mixop mixop
  | Il.ListP `Cons -> "_ :: _"
  | Il.ListP (`Fixed len) -> Printf.sprintf "[ _/%d ]" len
  | Il.ListP `Nil -> "[]"
  | Il.OptP `Some -> "(_)"
  | Il.OptP `None -> "()"

(* Expressions and expression payloads *)

let rec string_of_exp' = function
  | BoolS b -> string_of_bool b
  | NumS n -> string_of_num n
  | TextS text -> "\"" ^ String.escaped text ^ "\""
  | VarS id -> string_of_id id
  | UnS (unop, _, exp) -> string_of_unop unop ^ string_of_exp_ref exp
  | BinS (binop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp_ref exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ string_of_exp_ref exp_r ^ ")"
  | CmpS (cmpop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp_ref exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
      ^ string_of_exp_ref exp_r ^ ")"
  | UpCastS (typ, exp) -> string_of_exp_ref exp ^ " as " ^ string_of_typ typ
  | DownCastS (typ, exp) -> string_of_exp_ref exp ^ " as " ^ string_of_typ typ
  | SubS (exp, typ) -> string_of_exp_ref exp ^ " <: " ^ string_of_typ typ
  | MatchS (exp, pattern) ->
      string_of_exp_ref exp ^ " matches " ^ string_of_pattern pattern
  | TupleS exps -> "(" ^ string_of_exp_refs ", " exps ^ ")"
  | CaseS notexp -> string_of_notexp notexp
  | StrS fields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> string_of_atom atom ^ " " ^ string_of_exp_ref exp)
             fields)
      ^ "}"
  | OptS exp_opt -> "?(" ^ string_of_exp_refs "" (Option.to_list exp_opt) ^ ")"
  | ListS exps -> "[" ^ string_of_exp_refs ", " exps ^ "]"
  | ConsS (exp_h, exp_t) ->
      string_of_exp_ref exp_h ^ " :: " ^ string_of_exp_ref exp_t
  | CatS (exp_l, exp_r) ->
      string_of_exp_ref exp_l ^ " ++ " ^ string_of_exp_ref exp_r
  | MemS (exp_e, exp_s) ->
      string_of_exp_ref exp_e ^ " <- " ^ string_of_exp_ref exp_s
  | LenS exp -> "|" ^ string_of_exp_ref exp ^ "|"
  | DotS (exp_b, atom) -> string_of_exp_ref exp_b ^ "." ^ string_of_atom atom
  | IdxS (exp_b, exp_i) ->
      string_of_exp_ref exp_b ^ "[" ^ string_of_exp_ref exp_i ^ "]"
  | SliceS (exp_b, exp_l, exp_h) ->
      string_of_exp_ref exp_b ^ "[" ^ string_of_exp_ref exp_l ^ " : "
      ^ string_of_exp_ref exp_h ^ "]"
  | UpdS (exp_b, path, exp_f) ->
      string_of_exp_ref exp_b ^ "[" ^ string_of_path path ^ " = "
      ^ string_of_exp_ref exp_f ^ "]"
  | CallS (id, targs, args) ->
      "$" ^ string_of_id id ^ string_of_targs targs ^ string_of_args args
  | HoldS (id, notexp) ->
      string_of_id id ^ ": " ^ string_of_notexp notexp ^ " holds"
  | IterS (exp, iterexp) -> string_of_exp_ref exp ^ string_of_iterexp iterexp

and string_of_exp_refs sep exps = String.concat sep (List.map string_of_exp_ref exps)

and string_of_notexp notexp =
  let mixop, exps = notexp in
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> string_of_atom_phrases
      else idx / 2 |> List.nth exps |> string_of_exp_ref)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " "

and string_of_iterexp iterexp =
  let iter, vars = iterexp in
  string_of_iter iter ^ "{"
  ^ String.concat ", "
      (List.map
         (fun var ->
           let id, typ, iters = var in
           string_of_var var ^ " <- " ^ string_of_var (id, typ, iters @ [ iter ]))
         vars)
  ^ "}"

and string_of_path path =
  match path with
  | RootP -> ""
  | IdxP (path, exp) -> string_of_path path ^ "[" ^ string_of_exp_ref exp ^ "]"
  | SliceP (path, exp_l, exp_h) ->
      string_of_path path ^ "[" ^ string_of_exp_ref exp_l ^ " : "
      ^ string_of_exp_ref exp_h ^ "]"
  | DotP (RootP, atom) -> string_of_atom atom
  | DotP (path, atom) -> string_of_path path ^ "." ^ string_of_atom atom

and string_of_arg = function
  | ExpA exp -> string_of_exp_ref exp
  | DefA id -> "$" ^ string_of_id id

and string_of_args args =
  match args with
  | [] -> ""
  | _ -> "(" ^ String.concat ", " (List.map string_of_arg args) ^ ")"

let string_of_shared_exp_element (id, exp') =
  string_of_exp_ref id ^ " = " ^ string_of_exp' exp'

let string_of_shared_exp shared_exp =
  SharedExp.bindings shared_exp
  |> List.map string_of_shared_exp_element
  |> String.concat "\n"
