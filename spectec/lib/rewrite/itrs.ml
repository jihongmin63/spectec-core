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

   Three things this must get right, each measured rather than assumed
   (`sweeps/itrs-probe/FINDINGS.md`):

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

   Soundness is the same direction as the plain TRS: sorts are dropped, so the
   ITRS is an over-approximation of the sorted CTRS and YES transfers to it,
   while NO does not (it may be an artifact of untyped variables ranging over
   values the sorts exclude). *)

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
   [bis_zero] becomes an equality test rather than a constructor look. *)
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
    ("bis_zero", fun a -> app "eq" (a @ [ literal 0 ]));
  ]

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
   the 16 ERROR rows in sweeps/itrs_arith.tsv is this and nothing else.

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
       [x / 2 < x] for [x > 0]: the probe [sweeps/itrs-probe/p8_nativediv.itrs]
       -- the bit-decomposition shape of [$band]/[$bor]/[$bxor] stripped to
       nothing else, [bits(l,r) -> (l%2)*(r%2) + 2*bits(l/2, r/2)] -- is YES in
       1s, closed by the dependency graph alone.

       Defining [div] recursively instead hides exactly that fact. The
       recursion terminates and is easy to prove terminating on its own, but it
       makes [div] a DEFINED symbol, and a caller halving its argument then
       needs [div(l,2) < l] about a defined symbol rather than an arithmetic
       fact the solver already has -- which is the same "the measure lives
       inside a black box" failure this whole surface exists to remove, just
       one layer down.

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
    (* Lists over integer indices. Out-of-bounds stays irreducible. *)
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

let of_system (t : R.t) : (string * Unravel.stats, string) result =
  let kept = List.filter (fun r -> not (dropped r)) t.R.rules in
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
          R.rhs = fold_ints r.R.rhs;
          R.conds =
            List.map
              (fun (s, p) -> (fold_ints s, fold_ints ~pattern:true p))
              r.R.conds;
        })
      lifted
  in
  (* Comparisons come back as constraints only now: folding is what turns a
     closed magnitude into the integer literal an operand can name. *)
  let guarded =
    List.map
      (fun (r : R.rule) ->
        let r, cs = lift_guards next r in
        constraints := !constraints @ cs;
        r)
      folded
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
           "open scalar magnitude(s) the integer encoding cannot express: %s"
           (String.concat ", " (List.filteri (fun i _ -> i < 5) bad)))
  | [] -> (
      let system = { t with R.rules = guarded } in
      match Unravel.unconditional_rules system with
      | Error msg -> Error msg
      | Ok (rules, chain_steps, owise) -> (
          match Unravel.print_rules rules with
          | Error msg -> Error msg
          | Ok (printed, variables) ->
              let codecs = codec_calls t in
              let candidates =
                integer_prelude
                @ List.concat_map
                    (fun h ->
                      if has_prefix ~prefix:"$bitstr_to_int" h then
                        decode_rules (Unravel.tpdb_sym h)
                      else encode_rules (Unravel.tpdb_sym h))
                    codecs
                @ codec_helpers
              in
              let used = symbols_used rules in
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
                      chain_steps;
                      vars = List.length vars;
                      owise;
                    } )))
