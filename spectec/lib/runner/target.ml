(** Target - Extends Interp.Target.S with metadata for test infrastructure.

    Adds to the interpreter config:
    - name: Target identifier (e.g., "p4", "ethereum")
    - spec_dir: Directory containing the target's spec files
    - test_dir: Directory containing test inputs *)

module type S = sig
  include Interp.Target.S

  val name : string
  val spec_dir : string
  val test_dir : string
end
