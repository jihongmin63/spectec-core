module R = Rewrite_system
open Ctrs_term

(** Native scalar theory ([To_ctrs.scalar_theory.Native]) as a post-fold over an
    assembled structural system: fold ground structural scalars into Maude's
    built-in wrappers ([nat(3)]/[int(-5)]/[bool(true)]/[txt("E.")]) and drop the
    prelude rules the built-in delegations replace. The wrapper spelling and
    literal builders live in {!Maude_theory} (shared with To_maude/Of_maude);
    the def/use invariant is that this fold and those backends agree on them.

    NOTE (port): this reproduces the old [Maude_theory.native_system] post-fold.
    The intended new design emits Native directly at the scalar leaves of
    [To_ctrs.term_of_exp] (so no fold is needed); this whole module is slated
    for removal in that refactor (todo.md "Native 직접 생성 리팩토링 (B)"). *)

let peano_value (t : R.term) : int option =
  let rec go = function
    | R.App ("zero", []) -> Some 0
    | R.App ("succ", [ t' ]) -> Option.map (( + ) 1) (go t')
    | _ -> None
  in
  go t

let native_chars_value (t : R.term) : string option =
  let rec go = function
    | R.App ("nil", []) -> Some ""
    | R.App ("cons", [ R.App (c, []); rest ]) -> (
        match chr_code_of_sym c with
        | Some code ->
            Option.map (fun s -> String.make 1 (Char.chr code) ^ s) (go rest)
        | None -> None)
    | _ -> None
  in
  match t with R.App ("cons", _) -> go t | _ -> None

(* Restate one term: fold ground scalar values into wrapped literals, keep
   everything else (symbols, variables, structure) as is. *)
let rec native_term (t : R.term) : R.term =
  match t with
  | R.Var _ -> t
  | R.App ("true", []) -> Maude_theory.bool_t true
  | R.App ("false", []) -> Maude_theory.bool_t false
  | R.App (("zero" | "succ"), _) when peano_value t <> None ->
      Maude_theory.nat_t (Bigint.of_int (Option.get (peano_value t)))
  | R.App ("int_pos", [ m ]) when peano_value m <> None ->
      Maude_theory.int_t (Bigint.of_int (Option.get (peano_value m)))
  | R.App ("int_neg", [ m ]) when peano_value m <> None ->
      Maude_theory.int_t (Bigint.of_int (-Option.get (peano_value m) - 1))
  | R.App ("cons", _) when native_chars_value t <> None ->
      Maude_theory.text_t (Option.get (native_chars_value t))
  | R.App (f, args) -> R.App (f, List.map native_term args)

(* Whether a rule defines a scalar the built-in delegations replace: a delegated
   operator head, a replaced text builtin, or an [eq] over scalar constructors
   (the structural [eq] over options/lists/user types is kept). *)
let scalar_pat (t : R.term) : bool =
  match t with
  | R.App (("zero" | "succ" | "int_pos" | "int_neg" | "true" | "false"), _) ->
      true
  | R.App (c, []) -> chr_code_of_sym c <> None
  | _ -> false

let replaced_builtin_prefixes =
  [ "$int_to_text"; "$strip_prefix"; "$strip_suffix" ]

let replaced_rule (r : R.rule) : bool =
  match R.defined_head r with
  | None -> false
  | Some h -> (
      List.mem h Prelude.native_replaced_heads
      || List.exists
           (fun p -> String.starts_with ~prefix:p h)
           replaced_builtin_prefixes
      ||
      match r.R.lhs with
      | R.App ("eq", [ a; b ]) -> scalar_pat a || scalar_pat b
      | _ -> false)

let fold (sys : R.t) : R.t =
  let rules =
    sys.R.rules
    |> List.filter (fun r -> not (replaced_rule r))
    |> List.map (fun (r : R.rule) ->
           {
             r with
             R.lhs = native_term r.R.lhs;
             rhs = native_term r.R.rhs;
             conds =
               List.map (fun (l, c) -> (native_term l, native_term c)) r.R.conds;
           })
  in
  let vars = R.dedup_stable (List.concat_map R.vars_of_rule rules) in
  { R.vars; rules }
