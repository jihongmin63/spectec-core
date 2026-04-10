open Xl
open Spectec

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

type iter =
  | Opt       (* `?` *)
  | List      (* `*` *)

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

and exp = int
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

and pattern =
  | CaseP of mixop
  | ListP of [ `Cons | `Fixed of int | `Nil ]
  | OptP of [ `Some | `None ]

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