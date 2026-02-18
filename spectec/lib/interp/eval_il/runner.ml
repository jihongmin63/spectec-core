open Lang.Il
module Cache = Semantics.Dynamic.Cache
module F = Format
open Attempt
open Common.Source

let run_relation (filename : string) (spec : spec) (rid : id')
    (values : value list) : Ctx.t * value list =
  let ctx = Interp.load_spec filename spec in
  let+ ctx, values = Interp.invoke_rel ctx (rid $ no_region) values in
  (ctx, values)

let init () : unit =
  Cache.Cache.clear !Interp.func_cache;
  Cache.Cache.clear !Interp.rule_cache

let run_relation_fresh (spec : spec) (rid : id') (values : value list)
    (filename : string) : Ctx.t * value list =
  init ();
  run_relation filename spec rid values
