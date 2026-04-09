module Il = Lang.Il

let find_structure spec relation binding_prems =
  let _ = Format.printf "%s\n" (Lang.Il.Print.string_of_def relation) in
  let _ = spec, relation, binding_prems in
  ()                                  