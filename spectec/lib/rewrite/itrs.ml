(* ITRS (integer-constraint TRS) surface: the same unraveling {!Unravel}
   produces, but with the structural arithmetic layer removed and re-expressed
   in AProVE's native integers and [:|:] constraints.

   WHY. The analysis CTRS carries its own arithmetic: naturals and an int's
   magnitude are Coq-[positive]-style binary digit trees ([bzero]/[bone]/[bd0]/
   [bd1]) and every operation on them is a rewrite system of its own (the
   b-layer alone is ~145 rules). A termination proof over that surface must
   re-derive facts as basic as "subtracting one decreases" from the digit
   recursion, and the measure that would witness a caller's descent lives inside
   that black box -- the "modular-(B) arith-blindness" family in
   [CLAUDE.md]'s triage. Handing those operations to AProVE as integers instead
   makes the descent a linear arithmetic fact its solver already knows.

   The b-layer is not the bottleneck by itself: measured alone it is YES in 0s,
   and the int layer above it (177 rules, [pow_int]/[div_int] included) YES in
   4s. What it costs is the CALLER's proof, which has to carry the whole library
   in its usable rules.

   Four things this must get right, each measured rather than assumed (the
   probe files the measurements were taken on were scratch and are gone; the
   numbers each bullet carries are what survived):

   - [monus] keeps its clamp. Naturals are non-negative by SORT in the CTRS, and
     ITRS is untyped, so that invariant is simply gone here: [sub(n,1)] on a
     variable that ITRS lets range over the negatives descends forever. The
     clamp ([sub(x,y) -> 0] when [x < y]) is what makes the recursion stop, so
     it is load-bearing for termination, not just for semantics.
   - Disequality is [!=], one constraint. Splitting it into [x < y || x > y] or
     into two rules reads as the more solver-friendly shape and is not: [!=] is
     YES in 15s where both alternatives are KILLED.
   - Integers stay bare, with [eq] overloaded across its structural rules and
     two integer-constraint rules. The open question was whether AProVE accepts
     one symbol used both ways; it does, and it is faster than the wrapped
     encoding (15s vs 24s).
   - Division by a literal is INLINED as the native operator, not called
     through the prelude's [div] wrapper: in a dependency pair's argument the
     wrapper is a defined symbol that hides the descent from the integer
     solver, and that alone was the term column's last eight >=300s rows --
     see {!inline_literal_divisors} for the one-variable measurements.

   WHAT A YES HERE MEANS -- and it is NOT what a plain-TRS YES means. Dropping
   the sorts is an over-approximation, the same safe direction the plain TRS
   has, but that covers only part of what happens below. The first thing
   {!of_system} does is DELETE every rule of the replaced layer and substitute
   hand-written integer definitions for them, and deleting rules makes a
   termination proof EASIER, not harder -- so this is not an over-approximation
   of the slice at all. It is a MODEL of it, and a YES is a statement about the
   model: the slice terminates PROVIDED the arithmetic behaves as these integer
   rules say. For the arithmetic proper that proviso is close to a definition
   (the b-layer computes exactly integer addition and the rest). For the codec
   it is weaker again, because the substitute is not the same algorithm: the
   spec's [$bitstr_to_int]/[$int_to_bitstr] correct by looping a +/-2^w
   adjustment where {!itrs_dec1}/{!itrs_enc1} evaluate a closed formula, so a
   YES says nothing about the loop's own termination.

   A NO transfers no better, and for one more reason on top: untyped variables
   range over values the sorts exclude. *)

module R = Rewrite_system

(* -------------------------------------------------------------------------- *)
(* The replaced layer.                                                        *)

(* Arithmetic, from the interface operations down to the binary-digit engine
   that implements them. All of it goes: a surviving b-layer rule would compute
   on digit trees that no longer exist, since every literal has been folded to
   an integer. This is {!Prelude.native_replaced_heads} minus the booleans
   (kept structural -- 2-3 rules each, and ITRS has no boolean sort to delegate
   them to) and minus the list operations, which get their own integer-indexed
   definitions below rather than disappearing. *)
let arith_heads =
  [
    "add";
    "sub";
    "mul";
    "div";
    "mod";
    "pow";
    "pow_aux";
    "leq";
    "lt";
    "negate_int";
    "abs_nat";
    "nonneg_int";
    "nat_of_int";
    "sub_int_nat";
    "add_int";
    "sub_int";
    "mul_int";
    "div_int";
    "mod_int";
    "div_int_aux";
    "mod_int_aux";
    "pow_int";
    "pow_int_aux";
    "leq_int";
    "lt_int";
    "sub_nat";
    "bsucc";
    "bpred";
    "bpred_double";
    "bis_zero";
    "badd";
    "badd_carry";
    "bmul";
    "bcompare";
    "bcompare_cont";
    "ble_of_cmp";
    "blt_of_cmp";
    "bleq";
    "blt";
    "bsub_mask";
    "bsub_mask_carry";
    "bdouble_mask";
    "bsucc_double_mask";
    "bsub_of_mask";
    "bsub";
    "bring0";
    "bring1";
    "bquot";
    "brem";
    "bdivmod_pos";
    "bdivmod_step0";
    "bdivmod_step1";
    "bdivmod_combine";
    "bdivmod_dispatch";
    "bdivmod_base";
    "bdiv";
    "bmod";
    "bpow_nat";
    "bpow_nat_aux";
  ]

(* List operations whose recursion peels a BINARY INDEX apart ([bis_zero]-tag
   dispatch, [bpred] to step). With the b-layer gone they are redefined over
   integer indices, which also turns their termination argument from a digit
   recursion into [i > 0 -> i - 1]. [cat] and [mem] recurse on the LIST and are
   left alone. *)
let list_heads =
  [
    "len";
    "idx";
    "idx_aux";
    "take";
    "take_aux";
    "drop";
    "drop_aux";
    "slice";
    "upd_idx";
    "upd_idx_aux";
    "upd_slice";
  ]

(* The two's-complement codec. [Builtin] mirrors [numerics.ml]'s +/-2^w
   correction LOOP, which is the reference semantics and stays that way on the
   executable surface; here it is replaced by the closed form ("the
   representative of n mod 2^w in [-2^(w-1), 2^(w-1))"), because the loop is
   what the 157-symbol decode family cannot get past: its two correction
   branches share one SCC with no linear ranking function, and the tool cannot
   see that they are mutually exclusive. Monomorphization can suffix these
   names, so they are matched by prefix. *)
let codec_prefixes = [ "$bitstr_to_int"; "$int_to_bitstr" ]

let has_prefix ~(prefix : string) (s : string) : bool =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let replaced_head (h : string) : bool =
  List.mem h arith_heads || List.mem h list_heads
  || List.exists (fun p -> has_prefix ~prefix:p h) codec_prefixes

(* [eq] is kept for structure but its SCALAR cases are delegated, exactly as
   {!Prelude.kept_in_native} splits them for the Maude backend: [eq] over
   nats/ints/bools becomes the two integer-constraint rules below, while [eq]
   over options/lists/user constructors keeps its structural definition. *)
let scalar_ctor = function
  | R.App
      ( ( "bzero" | "bone" | "bd0" | "bd1" | "int_pos" | "int_neg" | "true"
        | "false" ),
        _ ) ->
      true
  | _ -> false

let dropped (r : R.rule) : bool =
  match R.defined_head r with
  | Some h when replaced_head h -> true
  | _ -> (
      match r.R.lhs with
      | R.App ("eq", [ a; b ]) -> scalar_ctor a || scalar_ctor b
      | _ -> false)

(* -------------------------------------------------------------------------- *)
(* Literal folding.                                                           *)

(* [bd0 p] = 2p, [bd1 p] = 2p+1, [int_pos n] = +n, [int_neg n] = -(n+1)
   ({!Prelude}'s doc). Only CLOSED terms fold; an open one (a variable
   magnitude) has no integer value and is reported by {!unfolded} instead of
   being silently dropped. *)
let rec nat_of_term (t : R.term) : int option =
  match t with
  | R.App ("bzero", []) -> Some 0
  | R.App ("bone", []) -> Some 1
  | R.App ("bd0", [ p ]) -> Option.map (fun n -> 2 * n) (nat_of_term p)
  | R.App ("bd1", [ p ]) -> Option.map (fun n -> (2 * n) + 1) (nat_of_term p)
  | _ -> None

let int_of_term (t : R.term) : int option =
  match t with
  | R.App ("int_pos", [ n ]) -> nat_of_term n
  | R.App ("int_neg", [ n ]) -> Option.map (fun k -> -(k + 1)) (nat_of_term n)
  | _ -> nat_of_term t

(* {!Unravel.print_rules} prints a [#]-prefixed nullary bare, so the literal
   reaches the file as [-3] rather than as a scrubbed identifier. *)
let literal (i : int) : R.term = R.App ("#" ^ string_of_int i, [])

(* An OPEN [int_pos] is the other way a magnitude reaches here: not a literal
   but a computed one, [int_pos(div(sub(n,1),2))]. Sign-magnitude wraps a
   non-negative value in a constructor that the integer encoding has no need
   for -- the magnitude of a non-negative integer IS the integer -- so the
   wrapper is simply dropped and the payload folded on its own.

   In a value position that is exact. In a PATTERN it widens the match from
   "non-negative" to "any integer", which is the same over-approximation that
   dropping sorts already makes and so preserves the direction termination
   transfers in.

   An open [int_neg] is the mirror case and cannot be treated the same way: its
   payload is the magnitude of [-(n+1)], so the wrapper carries arithmetic, not
   just a sign claim. In a VALUE position that arithmetic is writable --
   [negate_int(add(e, 1))], both of which the integer prelude defines -- and the
   result is exact. In a PATTERN it is not: a rule matching [f(int_neg(x))]
   selects the negative arguments and binds [x] to the magnitude, and no
   left-hand side over plain integers expresses that. Widening it to [f(x)] the
   way [int_pos] is widened would not merely match more, it would feed the
   right-hand side a different number -- which can turn a descending recursion
   into a non-descending one, in either direction. So [pattern] folding leaves
   it, and the coverage check below turns it into an error. *)
(* Dropping the binary-digit engine's RULES is only half of removing it: the
   spec's own rules, and P4's builtins especially, call into that engine
   directly ([bsub], [bdiv], [bis_zero]), and a call left standing after its
   definition is gone is worse than either -- an uninterpreted constant where
   arithmetic used to be, which AProVE will happily prove things about. So each
   b-layer operation is RETARGETED onto its nat-level counterpart, which the
   integer prelude does define.

   [bpred]/[bsub] go to the clamped [sub] because that is what they are: the
   magnitude type has no negatives, and Coq's [Pos.pred] of zero is zero.

   [bis_zero] becomes [leq(a, 0)] rather than [eq(a, 0)], and the difference is
   not cosmetic. This is the b-layer, so [a] is a MAGNITUDE: the CTRS sorts it
   nat. Routed through the generic [eq], the guard [bis_zero(a) = false] lifts
   to the constraint [a != 0], which lets [a] be negative -- and then
   [div(a, 2)] does not descend toward the base case, so a bit-decomposition
   recursion has no visible measure. Through [leq] it lifts to [a > 0], which
   is what the sort said all along. Measured on the [$bor_nat] cycle alone
   (AProVE, budget 60): [!= 0] is KILLED, [> 0] is YES in 0.7s. The wider
   [!= 0] is not unsound -- it is the sorted CTRS's information being thrown
   away, and this puts it back.

   [<= 0] rather than [= 0] on the true side for the same reason: on a nat they
   are the same set, and the inequality is what the constraint solver reasons
   with. It also keeps [!=] out of the export, which AProVE's [IDPNonInf]
   crashes on. *)
let blayer : (string * (R.term list -> R.term)) list =
  let app f args = R.App (f, args) in
  [
    ("bsucc", fun a -> app "add" (a @ [ literal 1 ]));
    ("bpred", fun a -> app "sub" (a @ [ literal 1 ]));
    ( "bpred_double",
      fun a -> app "sub" [ app "mul" (literal 2 :: a); literal 1 ] );
    ("badd", fun a -> app "add" a);
    ("badd_carry", fun a -> app "add" [ app "add" a; literal 1 ]);
    ("bmul", fun a -> app "mul" a);
    ("bsub", fun a -> app "sub" a);
    ("bdiv", fun a -> app "div" a);
    ("bmod", fun a -> app "mod" a);
    ("bleq", fun a -> app "leq" a);
    ("blt", fun a -> app "lt" a);
    ("bpow_nat", fun a -> app "pow" a);
    ("bis_zero", fun a -> app "leq" (a @ [ literal 0 ]));
  ]

(* [blayer] retargets a b-layer CALL, which is all that is needed while
   something still calls it -- and is nothing at all when the head IS the slice
   root, since the rules that got dropped are its own and no call site is left
   to retarget. Written out as a RULE the same table becomes the root's
   definition, so a slice about [bpow_nat] exports a system that still contains
   [bpow_nat]. *)
let rec render (t : R.term) : string =
  match t with
  | R.Var v -> v
  | R.App (f, []) when String.length f > 1 && f.[0] = '#' ->
      String.sub f 1 (String.length f - 1)
  | R.App (f, []) -> Unravel.tpdb_sym f
  | R.App (f, args) ->
      Unravel.tpdb_sym f ^ "(" ^ String.concat ", " (List.map render args) ^ ")"

(* The arity the slice itself gives a head. Taken from the rules rather than
   from a table beside {!blayer}, so the two cannot drift apart. *)
let arity_of (t : R.t) (head : string) : int option =
  List.find_map
    (fun (r : R.rule) ->
      match r.R.lhs with
      | R.App (f, args) when f = head -> Some (List.length args)
      | _ -> None)
    t.R.rules

let blayer_bridge (t : R.t) (head : string) : string option =
  match (List.assoc_opt head blayer, arity_of t head) with
  | Some build, Some arity ->
      let args = List.filteri (fun i _ -> i < arity) [ R.Var "x"; R.Var "y" ] in
      if List.length args <> arity then None
      else
        Some
          (Printf.sprintf "  %s(%s) -> %s" (Unravel.tpdb_sym head)
             (String.concat ", " (List.map render args))
             (render (build args)))
  | _ -> None

let rec fold_ints ?(pattern = false) (t : R.term) : R.term =
  let fold = fold_ints ~pattern in
  match int_of_term t with
  | Some i -> literal i
  | None -> (
      match t with
      | R.Var _ -> t
      | R.App ("int_pos", [ n ]) -> fold n
      | R.App ("int_neg", [ n ]) when not pattern ->
          R.App ("negate_int", [ R.App ("add", [ fold n; literal 1 ]) ])
      | R.App (f, args) -> (
          let args = List.map fold args in
          match List.assoc_opt f blayer with
          | Some build when not pattern -> build args
          | _ -> R.App (f, args)))

(* Division and remainder by a positive integer LITERAL, spelled as the native
   infix operator instead of a call to the prelude's [div]/[mod] ([#/] and
   [#%], printed infix by {!Unravel.print_rules}).

   The wrapper rule [div(x, y) -> x / y :|: y > 0] is not enough, and measuring
   that is what closed the term column's last eight rows. A recursion that
   halves its argument puts [div(l, 2)] in a dependency pair's ARGUMENT, and
   there the call is a defined symbol: the pair graph's edge condition becomes
   the reachability [div(l, 2) ->^* l'] instead of arithmetic the solver can
   read, [IDPNonInf] never fires, and AProVE falls back to an [IDPtoQDP]
   conversion whose transformation chain eats any budget -- standalone with the
   budget to itself just as inside the whole run, so this is not a scheduling
   problem a child split could fix. One-variable on the [$bxor_nat] cycle
   (chain collapsed either way, budget 60): through the wrapper KILLED, inlined
   YES in 1s. On whole slices the wrapper was the entire residue: all eight
   >=300s rows ([$bin_band]/[$bin_bor]/[$bin_bxor]/[$bin_shr],
   [$bitacc_offset_op]/[$bitacc_range_op], [$name_annotationToken]/
   [$name_annotation_opt]) close in 1-164s with only this inline applied
   (2026-09-01).

   Inlining is EXACT, not an approximation: the guard [y > 0] is decided at
   emission time by the literal, so this merely performs, once, the rewrite
   step the wrapper rule would have performed on every instance. A zero or
   negative literal keeps the call -- and with it the deliberate stuckness of
   division by zero -- and a variable divisor keeps the wrapper rule, which
   stays in the prelude for exactly those. Value positions only: a pattern
   cannot match on an infix operator, and no pattern can head with the defined
   [div]/[mod] anyway ({!Rewrite_system.orient_conds}). *)
let inline_literal_divisors (t : R.term) : R.term =
  let positive (k : string) =
    match int_of_string_opt (String.sub k 1 (String.length k - 1)) with
    | Some n -> n > 0
    | None -> false
  in
  let rec go t =
    match t with
    | R.Var _ -> t
    | R.App (f, args) -> (
        let args = List.map go args in
        match (f, args) with
        | ("div" | "mod"), [ _; R.App (k, []) ]
          when String.length k > 1 && k.[0] = '#' && positive k ->
            R.App ((if f = "div" then "#/" else "#%"), args)
        | _ -> R.App (f, args))
  in
  go t

(* Every scalar constructor must be gone after folding. One that survives is an
   OPEN magnitude -- a rule matching [int_pos(x)] or building [bd0(p)] on a
   variable -- which the integer encoding cannot express, and which would
   otherwise reach AProVE as an uninterpreted constructor silently disconnected
   from the arithmetic that used to define it. That is the failure mode the
   unraveler's own coverage check exists for, so it is an error, not a warning:
   the proof would be about a different system. *)
let unfolded (t : R.term) : string list =
  let rec go acc t =
    match t with
    | R.Var _ -> acc
    | R.App (f, args) ->
        let acc =
          if
            List.mem f [ "bzero"; "bone"; "bd0"; "bd1"; "int_pos"; "int_neg" ]
            || List.mem f [ "bmask_nul"; "bmask_neg"; "bmask_pos" ]
          then R.string_of_term t :: acc
          else acc
        in
        List.fold_left go acc args
  in
  go [] t

(* -------------------------------------------------------------------------- *)
(* Sign dispatch in PATTERN position.                                         *)

(* P4's bitwise builtins ([$band]/[$bor]/[$bxor]/[$shl]/[$shr]/[$concat]) and a
   handful of others select on an operand's SIGN by matching its constructor:

     d_band(int_neg(l), int_pos(r)) -> ...        (* l, r are magnitudes *)

   Folding cannot reach these. [int_pos(l)] could be widened to [l] -- more
   terms match, which is the direction termination transfers in -- but
   [int_neg(l)] cannot: it binds [l] to the magnitude of [-(l+1)], so widening
   it to [l] would not merely match more, it would hand the right-hand side a
   different NUMBER, and a recursion that descended might not. So these were an
   error, and they are the whole of the residual's coverage gap: every one of
   the 16 ERROR rows the arithmetic sweep produced is this and nothing else.

   The fix keeps the case split but moves it from the pattern to a constraint,
   which is EXACT rather than an approximation -- the rule fires on precisely
   the arguments it fired on before:

     d_band(z, y) -> ...[l := negate_int(add_int(z, 1)), r := y]
       :|: z < 0 && y >= 0

   Read [z = -(l+1)] backwards for [l = -(z+1)], which is non-negative exactly
   when [z] is negative, so the magnitude the right-hand side receives is the
   one it received before.

   Fresh names are unique across the WHOLE system, not per rule, so a constraint
   can be attached later by looking for its variable: the unraveler splits one
   source rule into a chain and the constraint has to ride along on every step
   that still carries the variable. *)

(* Fresh names for the variables a [:|:] constraint is keyed on. Unique across
   the WHOLE system, because {!of_system}'s [tail] matches a constraint to the
   rules it rides on BY those names: a name reused in an unrelated rule would
   attach the constraint there too, which RESTRICTS that rule -- removing steps,
   the unsound direction. *)
let constraint_var (n : int) : string = Printf.sprintf "itrsz%d" n

(* A name {!constraint_var} already handed out. Every pass that renames operands
   has to recognize one and leave it alone: the names are already unique
   system-wide, and renaming one again would orphan the constraint an earlier
   pass keyed on it -- [tail] would find no rule carrying its variable, and the
   guard would silently vanish from the export. *)
let is_constraint_var (v : string) : bool =
  has_prefix ~prefix:"itrsz" v
  && String.length v > 5
  && String.for_all
       (function '0' .. '9' -> true | _ -> false)
       (String.sub v 5 (String.length v - 5))

(* [lift_signs] rewrites one lhs, returning the new lhs, the constraints it
   owes, and the substitution its rhs and conditions must be put through. A
   payload that is not a plain variable is left alone for {!unfolded} to report:
   destructuring a magnitude further ([int_neg(bd0(p))]) would need the pattern
   to survive, which is the thing this cannot do. *)
let lift_signs (next : int ref) (lhs : R.term) :
    R.term * (string list * string) list * (string * R.term) list =
  let conds = ref [] and subst = ref [] in
  let rec go (t : R.term) : R.term =
    match t with
    | R.App ((("int_pos" | "int_neg") as ctor), [ R.Var v ]) ->
        incr next;
        let z = constraint_var !next in
        if ctor = "int_pos" then (
          conds := !conds @ [ ([ z ], Printf.sprintf "%s >= 0" z) ];
          subst := !subst @ [ (v, R.Var z) ])
        else (
          conds := !conds @ [ ([ z ], Printf.sprintf "%s < 0" z) ];
          subst :=
            !subst
            @ [
                ( v,
                  R.App
                    ("negate_int", [ R.App ("add_int", [ R.Var z; literal 1 ]) ])
                );
              ]);
        R.Var z
    | R.Var _ -> t
    | R.App (f, args) -> R.App (f, List.map go args)
  in
  let lhs' = go lhs in
  (lhs', !conds, !subst)

(* -------------------------------------------------------------------------- *)
(* Integer comparisons in CONDITION position.                                 *)

(* A condition whose subject is one of the prelude's comparisons and whose
   pattern is a boolean TESTS an arithmetic fact and binds nothing. Unraveled it
   becomes a chain step matching the opaque result of a defined symbol:

     d_contains_prime(n, n_len, t, t_c) -> u_4(leq(n_len, n), k_4(...))
     u_4(false, k_4(n, n_len, t, t_c))  -> u_5(...)
     u_5(false, k_5(n, n_len, t, t_c))  -> d_contains_prime(add(n, 1), ...)

   and the fact the recursion actually rests on -- [n < n_len] -- is no longer
   anything the constraint solver can see. That is fatal exactly where it costs
   most. In a COUNTING-UP recursion the measure is [n_len - n], so with the guard
   hidden behind [leq] the cycle has no decreasing argument at all, and the
   surface built to make arithmetic descent visible hides this one. Measured on
   [$contains_prime]: as chain steps AProVE never leaves [(0) ITRS] at any
   budget to 300s; as constraints it reaches the dependency graph and closes the
   arithmetic cycle with [IDPNonInfProof].

   This is the dual of the MTT defect in [CLAUDE.md]: MTT rewrote MATCHING
   conditions into equality tests and hid structural descent; unraveling rewrites
   ARITHMETIC guards into function results and hides arithmetic descent. Both are
   fixed by putting the condition back where the checker can read it.

   Lifting is exact rather than an approximation. The old rule fired precisely
   when the comparison reduced to the expected boolean, which needs both operands
   to be integers and holds on exactly the constraint's instances; and the steps
   it removes -- the comparison's own reduction and the chain step consuming it
   -- are finitely many between consecutive steps of the rule itself, so an
   infinite derivation of the unlifted system still maps to an infinite one here
   and a YES transfers back. *)

let comparison_relation (head : string) (expect : bool) : string option =
  match (head, expect) with
  | ("leq" | "leq_int"), true -> Some "<="
  | ("leq" | "leq_int"), false -> Some ">"
  | ("lt" | "lt_int"), true -> Some "<"
  | ("lt" | "lt_int"), false -> Some ">="
  | _ -> None

let sign_test_relation (head : string) (expect : bool) : string option =
  match (head, expect) with
  | ("nonneg_int" | "sub_nat"), true -> Some ">= 0"
  | ("nonneg_int" | "sub_nat"), false -> Some "< 0"
  | _ -> None

(* [eq] carries the same arithmetic guard as the comparisons above, but it is
   the GENERIC structural equality -- the specs write a nat guard as
   [-- if n =/= 0], which translates to the condition [eq(n, 0) = false], and
   left as a chain step it hides the very fact the recursion rests on. Measured
   on [$fresh_typeIds]: behind [eq] the proof needs a budget above 120s (it runs
   out there and closes at 300); as a constraint it closes at 120.

   Lifting [eq] is only exact where the operands really are integers, and a rule
   cannot say what sort a variable has. An integer LITERAL on one side settles
   it: the source CTRS is order-sorted, so [eq]'s other argument has the literal's
   sort. That is also the only shape the guard is ever written in. Restricting to
   it costs nothing and keeps the equivalence: on integers the constraint holds on
   exactly the instances the [eq] rules reduced to that boolean, and on anything
   else NEITHER form fires (the structural [eq] rules have no case pairing a
   constructor with an integer literal, and a constraint over a non-integer does
   not evaluate). *)
let equality_relation (head : string) (expect : bool) : string option =
  match (head, expect) with
  | "eq", true -> Some "="
  | "eq", false -> Some "!="
  | _ -> None

(* What a constraint can name directly. A compound argument would have to be
   rendered as an arithmetic EXPRESSION, which is a separate job; left as a chain
   step it keeps the behaviour this had before. *)
type operand = Ovar of string | Olit of string

let operand_of_term (t : R.term) : operand option =
  match t with
  | R.Var v -> Some (Ovar v)
  | R.App (s, []) when String.length s > 1 && s.[0] = '#' ->
      Some (Olit (String.sub s 1 (String.length s - 1)))
  | _ -> None

let is_literal : operand -> bool = function Olit _ -> true | Ovar _ -> false

(* The rule with its comparison conditions removed, plus the constraints they
   became. The variables those constraints mention are renamed system-wide fresh
   in the WHOLE rule, so that [tail] can find the steps each one rides on. *)
let lift_guards (next : int ref) (r : R.rule) :
    R.rule * (string list * string) list =
  let classify ((subject, pattern) : R.cond) :
      (operand * operand option * string) option =
    match (subject, pattern) with
    | R.App (h, args), R.App ((("true" | "false") as b), []) -> (
        let expect = b = "true" in
        match
          (args, comparison_relation h expect, sign_test_relation h expect)
        with
        | [ x; y ], Some rel, _ -> (
            match (operand_of_term x, operand_of_term y) with
            | Some x, Some y -> Some (x, Some y, rel)
            | _ -> None)
        | [ x; y ], None, _ -> (
            match
              (equality_relation h expect, operand_of_term x, operand_of_term y)
            with
            | Some rel, Some x, Some y when is_literal x || is_literal y ->
                Some (x, Some y, rel)
            | _ -> None)
        | [ x ], _, Some rel ->
            Option.map (fun x -> (x, None, rel)) (operand_of_term x)
        | _ -> None)
    | _ -> None
  in
  let classified = List.map (fun c -> (c, classify c)) r.R.conds in
  if List.for_all (fun (_, lifted) -> lifted = None) classified then (r, [])
  else
    let renames : (string, string) Hashtbl.t = Hashtbl.create 4 in
    let name = function
      | Olit s -> s
      | Ovar v when is_constraint_var v -> v
      | Ovar v -> (
          match Hashtbl.find_opt renames v with
          | Some z -> z
          | None ->
              incr next;
              let z = constraint_var !next in
              Hashtbl.add renames v z;
              z)
    in
    let render (x, y, rel) =
      let text =
        match y with
        | None -> Printf.sprintf "%s %s" (name x) rel
        | Some y -> Printf.sprintf "%s %s %s" (name x) rel (name y)
      in
      let vars =
        List.filter_map
          (function Ovar _ as o -> Some (name o) | Olit _ -> None)
          (x :: Option.to_list y)
      in
      (vars, text)
    in
    let constraints =
      List.filter_map (fun (_, lifted) -> Option.map render lifted) classified
    in
    let conds =
      List.filter_map
        (fun (c, lifted) -> if lifted = None then Some c else None)
        classified
    in
    let sub = Hashtbl.fold (fun v z acc -> (v, R.Var z) :: acc) renames [] in
    let apply t = R.subst sub t in
    ( {
        r with
        R.lhs = apply r.R.lhs;
        R.rhs = apply r.R.rhs;
        R.conds = List.map (fun (s, p) -> (apply s, apply p)) conds;
      },
      constraints )

(* -------------------------------------------------------------------------- *)
(* The list-length lift.                                                      *)

(* WHY, and what it is for. The surface above replaces the ARITHMETIC layer, so
   a recursion counting an integer down becomes visible. A recursion shortening
   a LIST does not. [$strip_prefix_rec] recurses on
   [$strip_prefix(t, p) = drop(t, len(p))] under the guards [len(p) > 0] and
   [$starts_with(t, p)]: it descends by [len(p)] every step, and every part of
   that measure is spelled in list constructors the integer surface leaves
   alone. Neither export can prove it -- on the plain TRS the recursive argument
   is not a subterm of the old one, and on the integer surface it is an opaque
   list operation -- and both TIMEOUT at budget 1800.

   The lift maps a list to its LENGTH, uniformly: every list-sorted position
   becomes an integer, [nil] is 0, [cons] is one more, and each list operation
   becomes the closed form its result's length has. The slice comes out with one
   sort collapsed, and the descent -- now [t - p] under [p > 0] -- is arithmetic
   the ITRS solver reads directly.

   WHAT A YES MEANS is what it means on the rest of this surface, one step
   further: a MODEL, not an over-approximation. Two lists of equal length are
   now one term, so a rule that told them apart fires here on both -- which ADDS
   rewrite steps, the safe direction -- and the steps the lift REMOVES belong to
   the operations it replaces by their closed forms, exactly as the codec
   replacement does. A YES states that the slice terminates provided the list
   layer behaves as these lengths say.

   Element computations are the one place that trade is not free: [cons(h, t)]
   becomes [t + 1] and [h] goes with whatever it would have reduced to.
   Discarding a CONSTRUCTOR term is harmless (it takes no steps); discarding a
   CALL would delete a recursion, so that is a coverage failure below rather
   than a silent drop.

   Coverage is checked, not assumed ({!Unravel}'s discipline): a list operation
   with no closed form, a list destructured in a PATTERN, a guard that does not
   render as pure integer arithmetic, and a case split that multiplies out of
   hand each abandon the lift with a message. The caller then falls back to the
   plain integer surface, which is what it would have run anyway. *)

exception Norm_fail of string

let norm_fail fmt = Printf.ksprintf (fun s -> raise (Norm_fail s)) fmt

(* The list vocabulary this lift knows. Everything named here is REPLACED, so
   anything still standing afterwards is a gap: [idx] and [mem] and the
   [bis_zero]-tag auxiliaries read an ELEMENT out of a list, which its length
   does not determine, and left in place they would be applied to an integer --
   a system whose list layer means nothing, proved under this one's name. *)
let list_vocabulary =
  [
    "nil";
    "cons";
    "len";
    "cat";
    "mem";
    "idx";
    "idx_aux";
    "take";
    "take_aux";
    "drop";
    "drop_aux";
    "slice";
    "upd_idx";
    "upd_idx_aux";
    "upd_slice";
    "match_cons";
    "match_nil";
  ]

(* Rules the lift drops on top of {!dropped}'s. {!list_heads} already covers the
   index-recursing operations (the plain surface re-gives them as
   {!list_prelude}); these three recurse on the LIST itself, which is why that
   surface keeps them and this one cannot. *)
let norm_dropped_heads = [ "cat"; "match_cons"; "match_nil" ]

let norm_dropped (r : R.rule) : bool =
  dropped r
  ||
  match R.defined_head r with
  | Some h -> List.mem h norm_dropped_heads
  | None -> false

(* -------------------------------------------------------------------------- *)
(* Which positions hold lists.                                                *)

(* The lift itself is keyed by SYMBOL and needs no sorts to fire. What the sorts
   buy is the fact the untyped export throws away: a length is NON-NEGATIVE.
   That is the same information [bis_zero -> leq] puts back for the b-layer, and
   it is load-bearing for the same reason -- with [t] free to be negative,
   [t - p] under [p > 0] descends forever and there is no measure to find.
   Nothing else here needs the inference, so nothing else asks it.

   Slots are unified along the slice's own equations from two seeds: the
   occurrences of [nil]/[cons], and the list arguments of {!Prelude}'s own
   operations. A class marked BOTH ways is not an error but an unknown -- the
   generic equality carries lists at one call site and scalars at another, and
   under the lift both are integers anyway; it simply gets no nonnegativity,
   since a constraint over a value that is not an integer holds nowhere and
   would silently delete the rule. *)

let ret_slot (f : string) : string = "r:" ^ f
let arg_slot (f : string) (i : int) : string = Printf.sprintf "a:%s:%d" f i
let var_slot (k : int) (v : string) : string = Printf.sprintf "v:%d:%s" k v

type sorts = {
  parent : (string, string) Hashtbl.t;
  mutable marks : (string * bool) list; (* [true] = holds a list *)
}

let rec find (s : sorts) (x : string) : string =
  match Hashtbl.find_opt s.parent x with
  | None ->
      Hashtbl.replace s.parent x x;
      x
  | Some p when p = x -> x
  | Some p ->
      let r = find s p in
      Hashtbl.replace s.parent x r;
      r

let union (s : sorts) (a : string) (b : string) : unit =
  let a = find s a and b = find s b in
  if a <> b then Hashtbl.replace s.parent a b

let mark (s : sorts) (listy : bool) (x : string) : unit =
  s.marks <- (x, listy) :: s.marks

let is_literal_sym (f : string) : bool = String.length f > 1 && f.[0] = '#'

(* The seeds, {!Prelude}'s list operations argument by argument. The scalar
   slots are named too, since a class is only known to be lengths when nothing
   scalar shares it. *)
let seed_sorts (s : sorts) : unit =
  let listy = mark s true and scalar = mark s false in
  List.iter listy
    [
      ret_slot "nil";
      ret_slot "cons";
      arg_slot "cons" 1;
      arg_slot "len" 0;
      ret_slot "cat";
      arg_slot "cat" 0;
      arg_slot "cat" 1;
      ret_slot "take";
      arg_slot "take" 0;
      ret_slot "drop";
      arg_slot "drop" 0;
      ret_slot "slice";
      arg_slot "slice" 0;
      ret_slot "upd_idx";
      arg_slot "upd_idx" 0;
      ret_slot "upd_slice";
      arg_slot "upd_slice" 0;
      arg_slot "upd_slice" 3;
      arg_slot "idx" 0;
      arg_slot "mem" 1;
      arg_slot "match_cons" 0;
      arg_slot "match_nil" 0;
    ];
  List.iter scalar
    [
      ret_slot "len";
      arg_slot "take" 1;
      arg_slot "drop" 1;
      arg_slot "slice" 1;
      arg_slot "slice" 2;
      arg_slot "upd_idx" 1;
      arg_slot "upd_slice" 1;
      arg_slot "upd_slice" 2;
      arg_slot "idx" 1;
      ret_slot "match_cons";
      ret_slot "match_nil";
      ret_slot "mem";
      ret_slot "true";
      ret_slot "false";
    ];
  List.iter
    (fun h ->
      scalar (ret_slot h);
      List.iter (fun i -> scalar (arg_slot h i)) [ 0; 1; 2 ])
    arith_heads

(* The heads whose argument sort is decided by the CALL SITE rather than by the
   symbol: structural equality, and the reflection of a stuck head. Unifying
   their argument slots the way every other symbol's are unified would merge
   every sort they are ever applied to into one class -- measured on
   [$strip_suffix_rec], where [isStuckHead] is applied to a list in one rule and
   to a nat in another and the whole slice came out unknown -- so each
   occurrence gets a slot of its own. The argument still carries its own sort
   into that slot; what it no longer does is carry it to the next call site. *)
let generic_heads = [ "eq"; "eqg"; "isStuckHead" ]

(* One rule's equations. A term at a slot unifies that slot with its head's
   result slot (or, for a variable, with the variable's own) and recurses into
   the argument slots; a condition's two sides share a slot of their own. *)
let walk_sorts (s : sorts) (k : int) : string -> R.term -> unit =
  let fresh = ref 0 in
  let rec go slot t =
    match t with
    | R.Var v -> union s slot (var_slot k v)
    | R.App (f, args) ->
        union s slot (ret_slot f);
        if is_literal_sym f then mark s false slot;
        List.iteri
          (fun i a ->
            if List.mem f generic_heads then (
              incr fresh;
              go (Printf.sprintf "g:%d:%d" k !fresh) a)
            else go (arg_slot f i) a)
          args
  in
  go

(* The variables each rule holds a length in, by rule index. *)
let length_vars (rules : R.rule list) : (int, string list) Hashtbl.t =
  let s = { parent = Hashtbl.create 1024; marks = [] } in
  seed_sorts s;
  List.iteri
    (fun k (r : R.rule) ->
      let go = walk_sorts s k in
      let root =
        match r.R.lhs with
        | R.App (f, _) -> ret_slot f
        | R.Var v -> var_slot k v
      in
      go root r.R.lhs;
      go root r.R.rhs;
      List.iteri
        (fun j (subject, pattern) ->
          let c = Printf.sprintf "c:%d:%d" k j in
          go c subject;
          go c pattern)
        r.R.conds)
    rules;
  let listy = Hashtbl.create 256 and scalar = Hashtbl.create 256 in
  List.iter
    (fun (slot, is_list) ->
      Hashtbl.replace (if is_list then listy else scalar) (find s slot) ())
    s.marks;
  let lengths = Hashtbl.create 256 in
  List.iteri
    (fun k (r : R.rule) ->
      let held =
        List.filter
          (fun v ->
            let c = find s (var_slot k v) in
            Hashtbl.mem listy c && not (Hashtbl.mem scalar c))
          (R.dedup_stable (R.vars_of_term r.R.lhs))
      in
      Hashtbl.replace lengths k held)
    rules;
  lengths

(* -------------------------------------------------------------------------- *)
(* The dictionary.                                                            *)

(* An integer constraint the lift owes, over the expressions it built. *)
type atom = R.term * string * R.term

(* One reading of an operation: the length of its result, and the constraint
   under which {!Prelude} gives it a result at all (out of range it is
   irreducible there, and totalizing it here would let a proof rely on a step
   the real system cannot take). Several readings are a CASE SPLIT: the rule
   containing the call is duplicated, one copy per reading, their constraints
   disjoint and together exhaustive. *)
type closed = { value : R.term; guard : atom list }

(* A symbol the slice itself defines by such a form -- see {!discover_macros}. *)
type macro = { params : string list; readings : closed list }

(* The lift builds arithmetic where the CTRS had list operations, and folds it
   as it goes. Not cosmetic: an unfolded [0 + p] is one more thing for the
   constraint solver to carry on every rule the guard rides on, and an
   all-literal guard folds to a decision -- see {!settled_atoms}. *)
let lit_of (t : R.term) : int option =
  match t with
  | R.App (f, []) when is_literal_sym f ->
      int_of_string_opt (String.sub f 1 (String.length f - 1))
  | _ -> None

let arith (op : string) (f : int -> int -> int) (unit_ : int option) a b =
  match (lit_of a, lit_of b) with
  | Some x, Some y -> literal (f x y)
  | Some x, None when unit_ = Some x -> b
  | None, Some y when unit_ = Some y -> a
  | _ -> R.App (op, [ a; b ])

let plus = arith "#+" ( + ) (Some 0)
let minus a b = if lit_of b = Some 0 then a else arith "#-" ( - ) None a b
let times = arith "#*" ( * ) (Some 1)

(* The closed forms. The list entries are read off {!list_prelude} one for one:
   [take]'s result is as long as its index and needs the list to reach that far,
   [drop]'s is what is left, [slice] is [take] after [drop].

   The three nat operations are here for a different reason. A guard has to
   render as PURE integer arithmetic -- [take(t, sub(len t, len s)) <= len t] is
   not something a [:|:] can say while [sub] is a defined symbol -- so the
   arithmetic computing an index must be inlined along with the operation it
   indexes. [sub] is monus and needs both of its cases to say that;
   {!integer_prelude} keeps its rules for the callers this does not reach. *)
let dictionary (f : string) (args : R.term list) : closed list option =
  let plain value = Some [ { value; guard = [] } ] in
  match (f, args) with
  | "nil", [] -> plain (literal 0)
  | "len", [ x ] -> plain x
  | "cat", [ x; y ] -> plain (plus x y)
  | "take", [ x; k ] ->
      Some [ { value = k; guard = [ (k, ">=", literal 0); (k, "<=", x) ] } ]
  | "drop", [ x; k ] ->
      Some
        [
          { value = minus x k; guard = [ (k, ">=", literal 0); (k, "<=", x) ] };
        ]
  | "slice", [ x; i; n ] ->
      Some
        [
          {
            value = n;
            guard =
              [
                (i, ">=", literal 0); (n, ">=", literal 0); (plus i n, "<=", x);
              ];
          };
        ]
  | "upd_idx", [ x; i; _ ] ->
      Some [ { value = x; guard = [ (i, ">=", literal 0); (i, "<", x) ] } ]
  | "upd_slice", [ x; i; n; v ] ->
      Some
        [
          {
            value = plus (minus x n) v;
            guard =
              [
                (i, ">=", literal 0); (n, ">=", literal 0); (plus i n, "<=", x);
              ];
          };
        ]
  (* A matcher on a list is a comparison on its length, which is the shape
     {!lift_guards} can turn into a constraint of its own. *)
  | "match_cons", [ x ] -> plain (R.App ("lt", [ literal 0; x ]))
  | "match_nil", [ x ] -> plain (R.App ("leq", [ x; literal 0 ]))
  | "add", [ x; y ] -> plain (plus x y)
  | "mul", [ x; y ] -> plain (times x y)
  | "sub", [ x; y ] ->
      Some
        [
          { value = minus x y; guard = [ (x, ">=", y) ] };
          { value = literal 0; guard = [ (x, "<", y) ] };
        ]
  | _ -> None

let lookup (macros : (string * macro) list) (f : string) (args : R.term list) :
    closed list option =
  match dictionary f args with
  | Some _ as found -> found
  | None -> (
      match List.assoc_opt f macros with
      | Some m when List.length m.params = List.length args ->
          let inst = R.subst (List.combine m.params args) in
          Some
            (List.map
               (fun c ->
                 {
                   value = inst c.value;
                   guard =
                     List.map (fun (l, rel, r) -> (inst l, rel, inst r)) c.guard;
                 })
               m.readings)
      | _ -> None)

(* An expression a [:|:] constraint can name: variables, folded literals, and
   the infix operators {!Unravel.print_rules} spells. Anything else -- a defined
   symbol, a constructor -- has no integer value at constraint time. *)
let rec pure (t : R.term) : bool =
  match t with
  | R.Var _ -> true
  | R.App (f, []) -> is_literal_sym f
  | R.App (f, [ a; b ]) -> is_literal_sym f && pure a && pure b
  | R.App _ -> false

let rec render_expr (t : R.term) : string =
  match t with
  | R.Var v -> v
  | R.App (f, []) when is_literal_sym f -> String.sub f 1 (String.length f - 1)
  | R.App (f, [ a; b ]) when is_literal_sym f ->
      Printf.sprintf "(%s %s %s)" (render_expr a)
        (String.sub f 1 (String.length f - 1))
        (render_expr b)
  | _ ->
      norm_fail "constraint operand is not integer arithmetic: %s"
        (R.string_of_term t)

(* An atom over literals alone is a decision, not a constraint. Carrying one
   would be worse than useless: it names no variable, and {!of_system}'s [tail]
   attaches a constraint to every rule that carries all of ITS variables -- which
   for none is every rule in the export. A true one is dropped; a false one says
   the operation is undefined on exactly the instances that rule fires on, so
   the rule can never fire and goes with it (exact -- it contributes no step to
   the model either way). *)
let decided ((l, rel, r) : atom) : bool option =
  match (lit_of l, lit_of r) with
  | Some x, Some y ->
      Some
        (match rel with
        | ">=" -> x >= y
        | ">" -> x > y
        | "<=" -> x <= y
        | "<" -> x < y
        | _ -> x = y)
  | _ -> None

let settled_atoms (atoms : atom list) : atom list option =
  let atoms =
    List.fold_left
      (fun acc a -> if List.mem a acc then acc else acc @ [ a ])
      [] atoms
  in
  if List.exists (fun a -> decided a = Some false) atoms then None
  else Some (List.filter (fun a -> decided a <> Some true) atoms)

let render_atom ((l, rel, r) : atom) : string =
  Printf.sprintf "%s %s %s" (render_expr l) rel (render_expr r)

(* -------------------------------------------------------------------------- *)
(* The transformation.                                                        *)

(* How many copies of one rule a case split may make. A rule with several
   independent [sub]s multiplies out, and past this the export is no longer the
   question anyone asked. *)
let split_cap = 16

(* How many readings an operation has, without yet knowing its arguments -- the
   count is a property of the symbol, and asking the dictionary itself is what
   keeps this from drifting away from it. *)
let readings_count (macros : (string * macro) list) (f : string) (arity : int) :
    int =
  let dummy = List.init arity (fun i -> R.Var (Printf.sprintf "_%d" i)) in
  match lookup macros f dummy with Some rs -> List.length rs | None -> 1

(* The subterms of one rule whose operation has more than one reading, each
   listed ONCE however often it occurs. One choice per distinct subterm, not per
   occurrence, and that is not merely an economy: the occurrences are the same
   call, so in the source they reduce the same way, and letting them differ
   would emit copies whose guards contradict each other ([x >= y] here and
   [x < y] there) -- eight copies of [$ends_with] where two say everything. *)
let split_sites (macros : (string * macro) list) (terms : R.term list) :
    (R.term * int) list =
  let rec go t acc =
    match t with
    | R.Var _ -> acc
    | R.App (f, args) ->
        let acc = List.fold_left (fun acc a -> go a acc) acc args in
        let n = readings_count macros f (List.length args) in
        if n > 1 && not (List.mem_assoc t acc) then acc @ [ (t, n) ] else acc
  in
  List.fold_left (fun acc t -> go t acc) [] terms

let assignments (sites : (R.term * int) list) : (R.term * int) list list =
  let out =
    List.fold_right
      (fun (t, n) acc ->
        List.concat_map
          (fun i -> List.map (fun rest -> (t, i) :: rest) acc)
          (List.init n Fun.id))
      sites [ [] ]
  in
  if List.length out > split_cap then
    norm_fail "a rule case-splits into %d copies" (List.length out)
  else out

(* A discarded element must not be a computation: dropping [cons(h, t)]'s head
   drops whatever [h] would have reduced to, and where that is a CALL the lift
   would be deleting a recursion rather than abstracting a value. *)
let rec computes (defined : (string, unit) Hashtbl.t) (t : R.term) : bool =
  match t with
  | R.Var _ -> false
  | R.App (f, args) ->
      Hashtbl.mem defined f || List.exists (computes defined) args

(* One value position under one assignment of readings. *)
let rec norm_value (defined : (string, unit) Hashtbl.t)
    (macros : (string * macro) list) (choice : (R.term * int) list) (t : R.term)
    : R.term * atom list =
  match t with
  | R.Var _ -> (t, [])
  | R.App ("cons", [ h; tl ]) ->
      if computes defined h then
        norm_fail "a list element is computed and the lift would drop it: %s"
          (R.string_of_term h);
      let tl, owed = norm_value defined macros choice tl in
      (plus tl (literal 1), owed)
  | R.App (f, args) -> (
      let args, owed =
        List.fold_left
          (fun (args, owed) a ->
            let a, owed' = norm_value defined macros choice a in
            (args @ [ a ], owed @ owed'))
          ([], []) args
      in
      match lookup macros f args with
      | None -> (R.App (f, args), owed)
      | Some readings ->
          let c =
            List.nth readings
              (Option.value (List.assoc_opt t choice) ~default:0)
          in
          (c.value, owed @ c.guard))

(* A pattern has room for neither half of the lift: an arithmetic expression
   cannot be matched against, and a constraint cannot be attached to a binding
   the match performs. [nil] is the one shape that survives, as the literal it
   denotes. A [cons] pattern is where the PLAIN surface is already at its
   strongest -- the tail meets the whole list as a strict subterm and
   size-change closes the cycle unaided -- so there is nothing here for the lift
   to add, and it says so rather than guessing. *)
let rec norm_pattern (t : R.term) : R.term =
  match t with
  | R.Var _ -> t
  | R.App ("nil", []) -> literal 0
  | R.App ("cons", _) ->
      norm_fail "a list is destructured in a pattern (%s)" (R.string_of_term t)
  | R.App (f, args) -> R.App (f, List.map norm_pattern args)

(* Two kinds of condition stop being steps once the lift has run, and each one
   it leaves standing is a chain step on every path through the rule.

   A BINDER of a fresh variable to a closed arithmetic expression: unraveled it
   becomes [u(t - p, ..)] produced and [u(crcv, ..)] consumed, which always
   fires. On the source surface it earns its step -- it is the binder
   {!Crc_surface.rebind_stuck_guards} restores, and the subject there is a CALL
   that forcing is the whole point of. Once the lift has turned that call into
   arithmetic there is nothing left to force, since an ITRS evaluates an
   expression in place rather than by rewriting. Only where the variable is
   bound HERE: one that also occurs in the left-hand side or in another
   condition's pattern is a MATCH, and a match decides whether the rule fires.

   A condition whose two sides are the SAME TERM, which is what a matching
   condition against a computed list becomes: [$starts_with] tests
   [t[0 : |p|] = p], and under the lift both sides are [|p|]. It held on every
   instance -- {!R.orient_conds} leaves the pattern side a constructor term, so
   a subject equal to it is already a normal form and the match cannot fail --
   and dropping it is what turns [$starts_with] and [$ends_with] from a chain
   into a single constrained rule, the shape a caller's proof can read a guard
   out of.

   Both are exact and both shorten every chain, which is the whole of their
   value: the recursion these slices are about is guarded by a call, and a
   guard the checker has to reach through three steps is one it does not reach.
   Measured on [$strip_suffix_rec] (budget 300): with the chain, KILLED; with
   both drops, YES. *)
let drop_vacuous_conds (r : R.rule) : R.rule =
  let matched v =
    R.count_var v r.R.lhs
    + List.fold_left (fun n (_, p) -> n + R.count_var v p) 0 r.R.conds
  in
  let bound = ref (R.vars_of_term r.R.lhs) in
  let sub, conds =
    List.fold_left
      (fun (sub, conds) (subject, pattern) ->
        let subject = R.subst sub subject in
        match pattern with
        | R.Var v when pure subject && matched v = 1 ->
            (sub @ [ (v, subject) ], conds)
        | _
          when subject = pattern
               && List.for_all
                    (fun v -> List.mem v !bound)
                    (R.vars_of_term pattern) ->
            (sub, conds)
        | _ ->
            bound := !bound @ R.vars_of_term pattern;
            (sub, conds @ [ (subject, pattern) ]))
      ([], []) r.R.conds
  in
  { r with R.rhs = R.subst sub r.R.rhs; R.conds }

let norm_rule (defined : (string, unit) Hashtbl.t)
    (macros : (string * macro) list) (lengths : string list) (r : R.rule) :
    (R.rule * atom list) list =
  let lhs = norm_pattern r.R.lhs in
  let nonneg = List.map (fun v -> (R.Var v, ">=", literal 0)) lengths in
  let values = r.R.rhs :: List.map fst r.R.conds in
  List.map
    (fun choice ->
      let value = norm_value defined macros choice in
      let rhs, owed = value r.R.rhs in
      let conds, owed =
        List.fold_left
          (fun (conds, owed) (subject, pattern) ->
            let subject, owed' = value subject in
            (conds @ [ (subject, norm_pattern pattern) ], owed @ owed'))
          ([], owed) r.R.conds
      in
      (drop_vacuous_conds { r with R.lhs; R.rhs; R.conds }, nonneg @ owed))
    (assignments (split_sites macros values))

(* A symbol the slice defines by a closed form is a symbol whose CALLS hide what
   they compute. [$strip_prefix] is [drop(t, len(p))], which the dictionary
   turns into [t - p]: left as a rule that puts a defined symbol in the
   recursive argument, where the pair graph reads an edge as reachability rather
   than as arithmetic -- the wrapper defect {!inline_literal_divisors} was
   written for, measured again here. On probes carrying this recursion's shape
   and nothing else (budget 300) one wrapper is YES and two are KILLED, and
   [$strip_prefix_rec] has two: its own and [$starts_with]'s. Only the
   conditional one has to stay.

   Inlining is exact in the same way the divisor inline is -- the rule's step is
   performed once at emission instead of on every instance, and the guard it
   owed rides to the call site. A ROOT is left alone: the export has to contain
   the symbol the slice is about. *)
let discover_macros ~(roots : string list) (macros : (string * macro) list)
    (normed : (R.rule * atom list) list) : (string * macro) list =
  let heads =
    R.dedup_stable (List.filter_map (fun (r, _) -> R.defined_head r) normed)
  in
  List.filter_map
    (fun h ->
      if List.mem h roots || List.mem_assoc h macros then None
      else
        let own =
          List.filter (fun (r, _) -> R.defined_head r = Some h) normed
        in
        let params =
          match own with
          | ({ R.lhs = R.App (_, args); _ }, _) :: _ ->
              List.filter_map (function R.Var v -> Some v | _ -> None) args
          | _ -> []
        in
        let reading ((r : R.rule), owed) =
          let bound t =
            List.for_all (fun v -> List.mem v params) (R.vars_of_term t)
          in
          if
            params <> []
            && List.length (R.dedup_stable params) = List.length params
            && r.R.conds = [] && (not r.R.owise)
            && r.R.lhs = R.App (h, List.map (fun v -> R.Var v) params)
            && pure r.R.rhs && bound r.R.rhs
            && List.for_all
                 (fun (l, _, rt) -> pure l && pure rt && bound l && bound rt)
                 owed
          then Some { value = r.R.rhs; guard = owed }
          else None
        in
        let readings = List.map reading own in
        if readings <> [] && List.for_all Option.is_some readings then
          Some (h, { params; readings = List.filter_map Fun.id readings })
        else None)
    heads

(* The lift as a whole: the length variables, then the dictionary and the macros
   it uncovers to a fixpoint (a macro's body can only turn closed once the
   macros it calls are inlined, so one pass is not enough), then the rules of
   every symbol that turned out to be a macro dropped, then the check that no
   list vocabulary survived anywhere. *)
let norm_lift ~(roots : string list) (rules : R.rule list) :
    (R.rule * atom list) list =
  let lengths = length_vars rules in
  let defined = Hashtbl.create 256 in
  List.iter
    (fun r ->
      Option.iter (fun h -> Hashtbl.replace defined h ()) (R.defined_head r))
    rules;
  let pass macros =
    List.concat
      (List.mapi
         (fun k r ->
           norm_rule defined macros
             (Option.value (Hashtbl.find_opt lengths k) ~default:[])
             r)
         rules)
  in
  let rec settle macros rounds =
    let normed = pass macros in
    if rounds = 0 then (macros, normed)
    else
      match discover_macros ~roots macros normed with
      | [] -> (macros, normed)
      | found -> settle (macros @ found) (rounds - 1)
  in
  let macros, normed = settle [] 4 in
  let normed =
    List.filter
      (fun ((r : R.rule), _) ->
        match R.defined_head r with
        | Some h -> not (List.mem_assoc h macros)
        | None -> true)
      normed
  in
  let residue =
    List.concat_map
      (fun ((r : R.rule), owed) ->
        R.heads_of_term r.R.lhs @ R.heads_of_term r.R.rhs
        @ List.concat_map
            (fun (s, p) -> R.heads_of_term s @ R.heads_of_term p)
            r.R.conds
        @ List.concat_map
            (fun (l, _, rt) -> R.heads_of_term l @ R.heads_of_term rt)
            owed)
      normed
    |> R.dedup_stable
    |> List.filter (fun h -> List.mem h list_vocabulary)
  in
  if residue <> [] then
    norm_fail "list operation(s) with no closed form: %s"
      (String.concat ", " residue);
  normed

(* The lift, spelled into {!of_system}'s two channels: the rules it returns, and
   the constraint list every chain step of a rule rides on. Constraint variables
   are renamed system-wide fresh for the reason {!lift_guards} renames them --
   [tail] matches a constraint to its rules BY those names, so a name shared
   with an unrelated rule would attach it there too, restricting that rule
   instead of this one. *)
let norm_rules ~(roots : string list) ~(next : int ref)
    ~(constraints : (string list * string) list ref) (rules : R.rule list) :
    (R.rule list, string) result =
  try
    Ok
      (List.filter_map
         (fun ((r : R.rule), atoms) ->
           match settled_atoms atoms with
           | None -> None
           | Some [] -> Some r
           | Some atoms ->
               let vars =
                 R.dedup_stable
                   (List.concat_map
                      (fun (l, _, rt) -> R.vars_of_term l @ R.vars_of_term rt)
                      atoms)
               in
               let sub =
                 List.map
                   (fun v ->
                     incr next;
                     (v, R.Var (constraint_var !next)))
                   vars
               in
               let apply = R.subst sub in
               let atoms =
                 List.map (fun (l, rel, rt) -> (apply l, rel, apply rt)) atoms
               in
               constraints :=
                 !constraints
                 @ List.map
                     (fun ((l, _, rt) as a) ->
                       ( R.dedup_stable (R.vars_of_term l @ R.vars_of_term rt),
                         render_atom a ))
                     atoms;
               Some
                 {
                   r with
                   R.lhs = apply r.R.lhs;
                   R.rhs = apply r.R.rhs;
                   R.conds =
                     List.map (fun (s, p) -> (apply s, apply p)) r.R.conds;
                 })
         (norm_lift ~roots rules))
  with Norm_fail msg -> Error ("list-length lift: " ^ msg)

(* -------------------------------------------------------------------------- *)
(* The hand-written integer prelude.                                          *)

(* Written as text, not as {!R.rule} values, because a [:|:] constraint has no
   representation in the CTRS data model -- that is the whole point of the
   surface. Partiality is preserved deliberately: [div]/[mod] by zero and
   [idx]/[take]/[drop] past the end stay irreducible here exactly as they are in
   {!Prelude}, since totalizing them would let a proof rely on steps the real
   system cannot take. *)
(* Integer equality is emitted only where an integer can actually reach [eq]:
   where folding left a literal or an arithmetic head behind.

   The test has to be that, and not the cheaper "does the slice call [eq]":
   dropping the structural scalar [eq] rules WITHOUT putting these in their
   place would leave [eq(3,5)] irreducible, and removing rewrite steps is the
   unsound direction -- termination of the smaller system would not transfer
   back. Where neither a literal nor an arithmetic head survives, no scalar can
   reach [eq] at all, so the drop and the omission are both safe.

   This is housekeeping, not a fix for anything measured. It was written to
   explain [$expression_as_lvalue] regressing from YES in 3.5s on the plain TRS
   to TIMEOUT here, and that explanation is WRONG: the slice defines no [eq]
   rules whatsoever, and gating them changed neither its rule count nor its
   verdict. The real difference is that its ITRS carries ONE constrained pair
   ([sub_nat], whose six constructor cases collapse to two sign tests) inside an
   otherwise purely structural 773-rule system -- and that appears to be enough
   to move the whole problem onto AProVE's integer machinery, which is the
   costlier analysis when the difficulty was never arithmetic. Unconfirmed: it
   is one symbol, and no one-variable experiment has isolated it yet. *)
let integer_eq =
  [ "  eq(x, y) -> true :|: x = y"; "  eq(x, y) -> false :|: x != y" ]

let integer_prelude =
  [
    (* Naturals. [sub] is monus WITH its clamp -- see the header. *)
    "  add(x, y) -> x + y";
    "  sub(x, y) -> x - y :|: x >= y";
    "  sub(x, y) -> 0 :|: x < y";
    "  mul(x, y) -> x * y";
    (* Native [/] and [%], not a repeated-subtraction recursion.
       AProVE's ITRS front end accepts both and, more to the point, KNOWS
       [x / 2 < x] for [x > 0]: a probe holding the bit-decomposition shape of
       [$band]/[$bor]/[$bxor] and nothing else,
       [bits(l,r) -> (l%2)*(r%2) + 2*bits(l/2, r/2)], is YES in 1s, closed by
       the dependency graph alone.

       Defining [div] recursively instead hides exactly that fact. The
       recursion terminates and is easy to prove terminating on its own, but it
       makes [div] a DEFINED symbol, and a caller halving its argument then
       needs [div(l,2) < l] about a defined symbol rather than an arithmetic
       fact the solver already has -- which is the same "the measure lives
       inside a black box" failure this whole surface exists to remove, just
       one layer down.

       Measured later (2026-09-01), even THIS thin wrapper hides it where it
       costs most: the probe above wrote [l/2] inline, and a call [div(l, 2)]
       in a dependency pair's argument still makes the edge a reachability
       through a defined symbol rather than arithmetic. Literal-divisor calls
       are therefore inlined away before emission
       ({!inline_literal_divisors}); this rule's remaining job is the variable
       divisors ([mod_int(n, p)] under the codec), which no inline can
       decide.

       Guarded on [y > 0] so division by zero stays irreducible, as it is in
       {!Prelude}. Both are the nat layer, whose arguments are magnitudes, so
       the sign wrappers below never hand them a negative and the
       floor-vs-truncate question does not arise here. *)
    "  div(x, y) -> x / y :|: y > 0";
    "  mod(x, y) -> x % y :|: y > 0";
    "  pow(x, y) -> 1 :|: y = 0";
    "  pow(x, y) -> x * pow(x, y - 1) :|: y > 0";
    "  leq(x, y) -> true :|: x <= y";
    "  leq(x, y) -> false :|: x > y";
    "  lt(x, y) -> true :|: x < y";
    "  lt(x, y) -> false :|: x >= y";
    (* Integers. Sign-magnitude collapses to plain integers, so the sign-case
       machinery ([negate_int]'s tag dispatch, [add_int]'s four constructor
       pairs) disappears into the native operators. *)
    "  add_int(x, y) -> x + y";
    "  sub_int(x, y) -> x - y";
    "  sub_int_nat(x, y) -> x - y";
    "  mul_int(x, y) -> x * y";
    "  negate_int(x) -> 0 - x";
    "  abs_nat(x) -> x :|: x >= 0";
    "  abs_nat(x) -> 0 - x :|: x < 0";
    "  nat_of_int(x) -> x :|: x >= 0";
    "  nonneg_int(x) -> true :|: x >= 0";
    "  nonneg_int(x) -> false :|: x < 0";
    "  sub_nat(x) -> true :|: x >= 0";
    "  sub_nat(x) -> false :|: x < 0";
    (* Signed div/mod truncate toward zero: the quotient is negative iff the
       signs differ, the remainder takes the DIVIDEND's sign ({!Prelude}). *)
    "  div_int(x, y) -> div(x, y) :|: x >= 0 && y > 0";
    "  div_int(x, y) -> 0 - div(x, 0 - y) :|: x >= 0 && y < 0";
    "  div_int(x, y) -> 0 - div(0 - x, y) :|: x < 0 && y > 0";
    "  div_int(x, y) -> div(0 - x, 0 - y) :|: x < 0 && y < 0";
    "  mod_int(x, y) -> mod(x, y) :|: x >= 0 && y > 0";
    "  mod_int(x, y) -> mod(x, 0 - y) :|: x >= 0 && y < 0";
    "  mod_int(x, y) -> 0 - mod(0 - x, y) :|: x < 0 && y > 0";
    "  mod_int(x, y) -> 0 - mod(0 - x, 0 - y) :|: x < 0 && y < 0";
    "  pow_int(x, y) -> 1 :|: y = 0";
    "  pow_int(x, y) -> x * pow_int(x, y - 1) :|: y > 0";
    "  leq_int(x, y) -> true :|: x <= y";
    "  leq_int(x, y) -> false :|: x > y";
    "  lt_int(x, y) -> true :|: x < y";
    "  lt_int(x, y) -> false :|: x >= y";
  ]

(* Lists over integer indices, still built from [nil]/[cons]. Out-of-bounds
   stays irreducible. Kept apart from the arithmetic above because the
   list-length lift below replaces the whole list layer with closed forms over
   lengths, and then none of these symbols exists any more. *)
let list_prelude =
  [
    "  len(nil) -> 0";
    "  len(cons(x, xs)) -> len(xs) + 1";
    "  idx(cons(x, xs), i) -> x :|: i = 0";
    "  idx(cons(x, xs), i) -> idx(xs, i - 1) :|: i > 0";
    "  take(xs, i) -> nil :|: i = 0";
    "  take(cons(x, xs), i) -> cons(x, take(xs, i - 1)) :|: i > 0";
    "  drop(xs, i) -> xs :|: i = 0";
    "  drop(cons(x, xs), i) -> drop(xs, i - 1) :|: i > 0";
    "  slice(xs, i, n) -> take(drop(xs, i), n)";
    "  upd_idx(cons(x, xs), i, v) -> cons(v, xs) :|: i = 0";
    "  upd_idx(cons(x, xs), i, v) -> cons(x, upd_idx(xs, i - 1, v)) :|: i > 0";
    "  upd_slice(xs, i, n, v) -> cat(take(xs, i), cat(v, drop(xs, i + n)))";
  ]

let prelude_vars = [ "i"; "n"; "v"; "x"; "xs"; "y"; "ys" ]

(* The codec, as the closed form and as a SINGLE ENTRY per symbol.

   Both halves matter and neither is sufficient alone -- measured one-variable
   on the real [$bin_ge] slice, each alone is KILLED at budget 300 and together
   YES in 5s. The closed form removes the +/-2^w recursion whose two branches
   share an SCC; the single entry removes the other cost, which is structural
   rather than about this codec at all: {!Unravel} emits one chain per
   conditional CLAUSE, so a definition with k clauses leaves k rules sharing one
   left-hand side, and on an acyclic 5-rule probe that duplication alone is the
   difference between KILLED at 61s and YES at 4s.

   Each step below therefore has one lhs per symbol, with the case split carried
   by disjoint constraints. [2 * r >= p] rather than [r >= p / 2] keeps the
   comparison linear and avoids a division the solver would have to reason
   under.

   DECODE needs the [w <= 0] guard for the same reason [Builtin] does: the
   closed form alone returns -1 at [w = 0] ([2^0 = 1], remainder 0, half 0,
   [0 >= 0] so [0 - 1]), which would turn a divergence into a silent wrong
   answer. ENCODE needs no guard -- at [w = 0] it wraps into [0, 1) and yields
   0, which is the one value of [bit<0>]. *)
let decode_rules (decode : string) =
  [
    Printf.sprintf "  %s(w, n) -> 0 :|: w <= 0" decode;
    Printf.sprintf "  %s(w, n) -> itrs_dec1(pow(2, w), n) :|: w > 0" decode;
  ]

let encode_rules (encode : string) =
  [ Printf.sprintf "  %s(w, n) -> itrs_enc1(pow(2, w), n)" encode ]

(* The shared tail of both codecs, emitted once however many monomorphized
   instances call it. *)
let codec_helpers =
  [
    "  itrs_dec1(p, n) -> itrs_dec2(mod_int(n, p), p)";
    "  itrs_dec2(r, p) -> itrs_dec3(r, p) :|: r >= 0";
    "  itrs_dec2(r, p) -> itrs_dec3(r + p, p) :|: r < 0";
    "  itrs_dec3(r, p) -> r - p :|: 2 * r >= p";
    "  itrs_dec3(r, p) -> r :|: 2 * r < p";
    "  itrs_enc1(p, n) -> itrs_enc2(mod_int(n, p), p)";
    "  itrs_enc2(r, p) -> r :|: r >= 0";
    "  itrs_enc2(r, p) -> r + p :|: r < 0";
  ]

let codec_vars = [ "n"; "p"; "r"; "w" ]

(* -------------------------------------------------------------------------- *)

(* The codec heads the SLICE actually calls, in their monomorphized spelling,
   so a slice that never decodes carries no codec rules at all. *)
let codec_calls (t : R.t) : string list =
  let heads = Hashtbl.create 8 in
  let rec go = function
    | R.Var _ -> ()
    | R.App (f, args) ->
        if List.exists (fun p -> has_prefix ~prefix:p f) codec_prefixes then
          Hashtbl.replace heads f ();
        List.iter go args
  in
  List.iter
    (fun (r : R.rule) ->
      go r.R.lhs;
      go r.R.rhs;
      List.iter
        (fun (s, p) ->
          go s;
          go p)
        r.R.conds)
    t.R.rules;
  Hashtbl.fold (fun k () acc -> k :: acc) heads [] |> List.sort compare

(* The head a hand-written prelude rule defines: the text before its first
   [(] (or before [ ->] for a nullary one). *)
let rule_head (s : string) : string =
  let s = String.trim s in
  let stop =
    match (String.index_opt s '(', String.index_opt s ' ') with
    | Some i, Some j -> min i j
    | Some i, None -> i
    | None, Some j -> j
    | None, None -> String.length s
  in
  String.sub s 0 stop

(* Keep only the prelude a slice can reach. The prelude is written once for
   every surface, but a slice that never divides should not carry [div_int]'s
   four sign cases -- they are unreachable, so AProVE's usable-rules analysis
   would discard them anyway, but only after parsing and indexing them, and on
   the largest slices that cost is paid on every proof attempt.

   Reachability is a fixpoint because the prelude calls itself: [slice] calls
   [take]/[drop], [div_int] calls [div], the codec calls [pow]/[mod_int]. *)
let reachable_prelude (used : (string, unit) Hashtbl.t) (rules : string list) :
    string list =
  let rec grow () =
    let added = ref false in
    List.iter
      (fun r ->
        if Hashtbl.mem used (rule_head r) then
          String.split_on_char ' '
            (String.map (function '(' | ')' | ',' -> ' ' | c -> c) r)
          |> List.iter (fun tok ->
                 if tok <> "" && not (Hashtbl.mem used tok) then (
                   Hashtbl.replace used tok ();
                   added := true)))
      rules;
    if !added then grow ()
  in
  grow ();
  List.filter (fun r -> Hashtbl.mem used (rule_head r)) rules

(* Both spellings of every symbol: the rules carry CTRS names ([$bitstr_to_int])
   while the hand-written prelude is written in TPDB ones ([d_bitstr_to_int]),
   and reachability has to match across that boundary. For everything without a
   [$] the two coincide, which is why only the codec ever noticed. *)
let symbols_used (rules : (R.term * R.term) list) : (string, unit) Hashtbl.t =
  let used = Hashtbl.create 256 in
  let rec go = function
    | R.Var _ -> ()
    | R.App (f, args) ->
        Hashtbl.replace used f ();
        Hashtbl.replace used (Unravel.tpdb_sym f) ();
        List.iter go args
  in
  List.iter
    (fun (l, r) ->
      go l;
      go r)
    rules;
  used

(* Which slices this surface is FOR. Replacing nothing is not an error -- the
   export still works -- but it costs a second full AProVE run to re-prove the
   same system under another name, so a caller that only wants the integer
   surface where it differs asks this first. *)
let has_arithmetic (t : R.t) : bool =
  let names (term : R.term) = R.heads_of_term term in
  List.exists
    (fun (r : R.rule) ->
      List.exists replaced_head
        (names r.R.lhs @ names r.R.rhs
        @ List.concat_map (fun (s, p) -> names s @ names p) r.R.conds))
    t.R.rules

let of_system ~(roots : string list) ?(norm = false) (t : R.t) :
    (string * Unravel.stats, string) result =
  let kept =
    List.filter
      (fun r -> not ((if norm then norm_dropped else dropped) r))
      t.R.rules
  in
  (* Sign dispatch first: it rewrites the lhs and substitutes into the rest, so
     folding must see the result, not the constructors it removes. *)
  let next = ref 0 in
  let constraints = ref [] in
  let lifted =
    List.map
      (fun (r : R.rule) ->
        let lhs, cs, sub = lift_signs next r.R.lhs in
        constraints := !constraints @ cs;
        let apply t = R.subst sub t in
        {
          r with
          R.lhs;
          R.rhs = apply r.R.rhs;
          R.conds = List.map (fun (s, p) -> (apply s, apply p)) r.R.conds;
        })
      kept
  in
  let folded =
    List.map
      (fun (r : R.rule) ->
        {
          r with
          R.lhs = fold_ints ~pattern:true r.R.lhs;
          R.rhs = inline_literal_divisors (fold_ints r.R.rhs);
          R.conds =
            List.map
              (fun (s, p) ->
                ( inline_literal_divisors (fold_ints s),
                  fold_ints ~pattern:true p ))
              r.R.conds;
        })
      lifted
  in
  (* The list layer comes down to lengths before the comparisons are lifted, so
     that a guard the lift turns into [leq(p, 0)] is one {!lift_guards} can name
     ([len(p)] is a compound operand and would stay a chain step). *)
  match
    if norm then norm_rules ~roots ~next ~constraints folded else Ok folded
  with
  | Error msg -> Error msg
  | Ok normed -> (
      (* Comparisons come back as constraints only now: folding is what turns a
     closed magnitude into the integer literal an operand can name. *)
      let guarded =
        List.map
          (fun (r : R.rule) ->
            let r, cs = lift_guards next r in
            constraints := !constraints @ cs;
            r)
          normed
      in
      let leftover =
        List.concat_map
          (fun (r : R.rule) ->
            unfolded r.R.lhs @ unfolded r.R.rhs
            @ List.concat_map (fun (s, p) -> unfolded s @ unfolded p) r.R.conds)
          guarded
      in
      match R.dedup_stable leftover with
      | _ :: _ as bad ->
          Error
            (Printf.sprintf
               "open scalar magnitude(s) the integer encoding cannot express: \
                %s"
               (String.concat ", " (List.filteri (fun i _ -> i < 5) bad)))
      | [] -> (
          let system = { t with R.rules = guarded } in
          match Unravel.unconditional_rules system with
          | Error msg -> Error msg
          | Ok { Unravel.unconditional = rules; steps; dropped_owise; _ } -> (
              match Unravel.print_rules rules with
              | Error msg -> Error msg
              | Ok (printed, variables) ->
                  let codecs = codec_calls t in
                  let candidates =
                    integer_prelude
                    @ (if norm then [] else list_prelude)
                    @ List.concat_map
                        (fun h ->
                          if has_prefix ~prefix:"$bitstr_to_int" h then
                            decode_rules (Unravel.tpdb_sym h)
                          else encode_rules (Unravel.tpdb_sym h))
                        codecs
                    @ codec_helpers
                  in
                  let used = symbols_used rules in
                  (* A slice whose ROOT is itself a replaced head keeps none of its
                 own rules, so nothing left in [rules] names it and the
                 reachability filter below would prune the very replacement
                 written to stand in for it -- exporting a system that no longer
                 contains the symbol the slice is about, and a YES that says
                 nothing about it. Seeding the roots is what keeps the
                 replacement; it is also what lets the [undefined] check below
                 see a root whose rules vanished without one, instead of
                 answering a question about the empty remainder. *)
                  List.iter
                    (fun r ->
                      Hashtbl.replace used r ();
                      Hashtbl.replace used (Unravel.tpdb_sym r) ())
                    roots;
                  let candidates =
                    candidates @ List.filter_map (blayer_bridge t) roots
                  in
                  (* Can a scalar reach [eq] at all? Only if folding left an
                 integer literal or an arithmetic head behind. *)
                  let has_integers =
                    Hashtbl.fold
                      (fun f () acc ->
                        acc
                        || (String.length f > 1 && f.[0] = '#')
                        || List.mem f arith_heads)
                      used false
                  in
                  let candidates =
                    if has_integers then integer_eq @ candidates else candidates
                  in
                  let extra = reachable_prelude used candidates in
                  (* A replaced head that survived into the rules but got no
                 definition back is the worst outcome available here: AProVE
                 reads it as an uninterpreted constructor, which is IRREDUCIBLE,
                 which removes rewrite steps -- the unsound direction, where a
                 YES would not transfer. It happens whenever the b-layer is
                 dropped but something still CALLS into it (the P4 bitwise
                 builtins call [bsub]/[bdiv] directly), so it is checked rather
                 than trusted. *)
                  let defined = List.map rule_head extra in
                  let undefined =
                    Hashtbl.fold
                      (fun f () acc ->
                        (* [defined] holds TPDB spellings (the codec rules are named
                       after the scrubbed symbol), [used] holds CTRS ones. *)
                        if
                          replaced_head f
                          && (not (List.mem (Unravel.tpdb_sym f) defined))
                          && not (List.mem f (List.map fst blayer))
                        then f :: acc
                        else acc)
                      used []
                    |> List.sort_uniq compare
                  in
                  if undefined <> [] then
                    Error
                      (Printf.sprintf
                         "replaced head(s) left with no integer definition: %s"
                         (String.concat ", " undefined))
                  else
                    let vars =
                      R.dedup_stable
                        (List.sort compare
                           (variables @ prelude_vars
                           @ List.concat_map fst !constraints
                           @ if codecs = [] then [] else codec_vars))
                    in
                    (* A lifted sign constraint rides on every chain step that still
                 carries its variable, not just the entry rule: the [k_i]
                 keep-constructors pass the original arguments along, so the
                 variable -- and the case split it stands for -- is still there
                 several steps down. Repeating it is exact, not a narrowing:
                 it is the same condition the constructor pattern imposed. *)
                    let tail lhs_term =
                      let carried = R.vars_of_term lhs_term in
                      match
                        List.filter
                          (fun (vs, _) ->
                            List.for_all (fun v -> List.mem v carried) vs)
                          !constraints
                      with
                      | [] -> ""
                      | cs -> " :|: " ^ String.concat " && " (List.map snd cs)
                    in
                    let b = Buffer.create 8192 in
                    Buffer.add_string b "(GOAL TERMINATION)\n(STARTTERM FULL)\n";
                    Buffer.add_string b
                      (Printf.sprintf "(VAR %s)\n" (String.concat " " vars));
                    Buffer.add_string b "(RULES\n";
                    List.iter2
                      (fun (lhs, rhs) (lhs_term, _) ->
                        Buffer.add_string b
                          ("  " ^ lhs ^ " -> " ^ rhs ^ tail lhs_term ^ "\n"))
                      printed rules;
                    List.iter (fun s -> Buffer.add_string b (s ^ "\n")) extra;
                    Buffer.add_string b ")\n";
                    Ok
                      ( Buffer.contents b,
                        {
                          Unravel.eqs = List.length t.R.rules;
                          rules = List.length rules + List.length extra;
                          chain_steps = steps;
                          vars = List.length vars;
                          owise = dropped_owise;
                        } ))))
