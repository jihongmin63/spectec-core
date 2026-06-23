(* Pre-pass over the elaborated IL, run before {!To_ctrs}: expand variables into
   their concrete canonical structure (via {!Prem_env}) and drop premises the
   premise environment renders redundant.

   STUBBED for the new-rewrite skeleton -- reimplement the rewriting steps
   (variable substitution from the union-find, [matches]/field-access folding
   into head patterns, value/let inlining, subtype-to-cast lowering, redundant-
   premise removal). *)

let simplify_spec (spec : Lang.Il.spec) : Lang.Il.spec =
  ignore spec;
  failwith "TODO(new-rewrite): reimplement Simplify.simplify_spec"
