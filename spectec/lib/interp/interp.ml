module Builtins = Builtins
module Eval_Il = Eval_il
module Eval_Sl = Eval_sl
module Target = Target

type error = EvalIlError of Eval_il.error | EvalSlError of Eval_sl.error

let error_to_string = function
  | EvalIlError e -> Eval_il.error_to_string e
  | EvalSlError e -> Eval_sl.error_to_string e
