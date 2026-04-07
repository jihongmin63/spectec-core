module Target : SpecTec.Target.s = struct
  let name = "stlc"
  let spec_dir = "examples/stlc"

  let builtins = []
  let handler f = f ()
  let is_impure_func _ = false
  let is_impure_rel _ = false
  let state_version = ref 0
end