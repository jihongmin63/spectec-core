module Builtins = Builtins
module Eval_Il = Eval_il
module Eval_Sl = Eval_sl
module Target = Target

type error = EvalIlError of Eval_Il.error | EvalSlError of Eval_Sl.error

val error_to_string : error -> string
