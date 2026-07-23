(** SCC-facing over-approximation passes: strip conditions and linearize
    left-hand sides so the sufficient-completeness checker's [drop-bad-eqs]
    filter keeps every rule. Counterexamples on the transformed slice stay
    sound; "complete" verdicts for transformed symbols prove nothing (see
    {!Scc}). Never applied to the canonical analysis or execution surfaces. *)

open Rewrite_system

(* SCC-facing over-approximation, part 1: strip every rule's conditions, so the
   sufficient-completeness checker's [drop-bad-eqs] filter (which silently
   discards conditional equations before building its tree automaton) has
   nothing left to discard. Pretending a guarded rule always fires
   over-approximates reducibility, so on this surface a "sufficiently complete"
   verdict for a symbol that had conditions proves nothing -- but a
   counterexample is sound: a constructor case no rule's LHS matches is missing
   regardless of any condition. The SCC reads only rule LHSs (its automaton is
   built from [lhs(Eq)] alone), so the RHS is free to change: when dropping the
   conditions would leave an RHS variable unbound (it was bound by a condition),
   the whole RHS is replaced by the LHS -- always well-sorted, never unbound --
   rather than marking the rule [nonexec], which the SCC's [is-exec?] filter
   would discard again. Never applied to the canonical analysis or execution
   surfaces; only the [rewrite --ctrs --unconditional] dump. *)
let drop_conds (t : t) : t =
  let dropped = ref 0 and identity_rhs = ref 0 in
  let drop_rule (r : rule) : rule =
    if r.conds = [] then r
    else (
      incr dropped;
      let lhs_vars = vars_of_term r.lhs in
      let rhs =
        if List.for_all (fun v -> List.mem v lhs_vars) (vars_of_term r.rhs) then
          r.rhs
        else (
          incr identity_rhs;
          r.lhs)
      in
      { r with rhs; conds = [] })
  in
  let rules = List.map drop_rule t.rules in
  if !dropped > 0 then
    Printf.eprintf
      "unconditional: dropped conditions from %d rule(s) (%d rhs replaced by \
       lhs)\n"
      !dropped !identity_rhs;
  of_rules rules

(* SCC-facing over-approximation, part 2: rename the second and later
   occurrences of any repeated LHS variable ([eqg(x, x)], [$iterproj]'s captured
   free variables re-mentioned in the element pattern), so the SCC's
   [drop-bad-eqs] filter -- which also discards non-left-linear equations --
   keeps the rule. Same polarity as {!drop_conds}: a linear pattern matches
   more, so counterexamples stay sound and "complete" verdicts for linearized
   symbols prove nothing. The first occurrence keeps its name, so an RHS that
   mentions the variable stays bound. *)
let linearize_lhs (t : t) : t =
  let linearized = ref 0 in
  let lin_rule (r : rule) : rule =
    let taken = Hashtbl.create 16 in
    List.iter (fun v -> Hashtbl.replace taken v ()) (vars_of_rule r);
    let seen = Hashtbl.create 16 in
    let fresh v =
      let rec pick k =
        let cand = Printf.sprintf "%s__lin%d" v k in
        if Hashtbl.mem taken cand then pick (k + 1)
        else (
          Hashtbl.replace taken cand ();
          cand)
      in
      pick 2
    in
    let changed = ref false in
    (* Explicit list recursion: which occurrence is "first" (and keeps the
       name binding the RHS) must be the leftmost one, deterministically. *)
    let rec go = function
      | Var v ->
          if Hashtbl.mem seen v then (
            changed := true;
            Var (fresh v))
          else (
            Hashtbl.replace seen v ();
            Var v)
      | App (f, ts) -> App (f, go_list ts)
    and go_list = function
      | [] -> []
      | x :: xs ->
          let x' = go x in
          x' :: go_list xs
    in
    let lhs = go r.lhs in
    if !changed then (
      incr linearized;
      { r with lhs })
    else r
  in
  let rules = List.map lin_rule t.rules in
  if !linearized > 0 then
    Printf.eprintf "unconditional: linearized %d non-left-linear lhs\n"
      !linearized;
  of_rules rules
