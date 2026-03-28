open Common.Source
open Common.Attempt

type single_error = region * failtrace list
type error = single_error list
type 'a result = ('a, error) Stdlib.result

module Fresh : sig
  val fresh_id :
    Common.Domain.IdSet.t -> Common.Domain.Id.t -> Common.Domain.Id.t
end

val elab_spec : Lang.El.spec -> Lang.Il.spec result
