(** Target - Groups multiple tasks for a target.

    A TARGET defines:
    - name: Target identifier (e.g., "p4", "ethereum")
    - spec_dir: Directory containing the target's spec files *)

module type TARGET = sig
  val name : string
  val spec_dir : string
end
