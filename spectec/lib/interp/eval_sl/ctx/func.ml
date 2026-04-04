open Lang.Sl
open Lang.Sl.Print

(* Function *)

type t = Builtin | Defined of tparam list * arg list * instr list

let to_string = function
  | Builtin -> "builtin function"
  | Defined (tparams, args, instrs) ->
      "def " ^ string_of_tparams tparams ^ string_of_args args ^ " :\n\n"
      ^ string_of_instrs instrs
