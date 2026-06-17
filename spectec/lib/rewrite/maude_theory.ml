module R = Rewrite_system

(* -------------------------------------------------------------------------- *)
(* The native-theory pass: restate a structural CTRS over Maude's built-ins.   *)
(*                                                                             *)
(* The analysis (COPS) pipeline needs self-contained structural scalars        *)
(* (Peano nats, sign-magnitude ints, char-list texts, own booleans) because a  *)
(* CTRS has no external theories. Maude has them built in, so the execution    *)
(* pipeline rewrites the system once, after [To_ctrs.of_spec]:                 *)
(*                                                                             *)
(*   - scalar VALUES move into wrapper constructors over the built-in sorts:   *)
(*     [nat(3)], [int(-5)], [bool(true)], [txt("E.")] -- ground Peano towers,  *)
(*     [int_pos]/[int_neg] injections of them, [true]/[false], and ground      *)
(*     char-list chains fold into wrapped literals;                            *)
(*   - the prelude rules that DEFINE those scalars (bool/nat/int arithmetic,   *)
(*     scalar equality, char equality, Peano-indexed list operations) are      *)
(*     dropped -- {!To_maude} re-emits each surviving operator as a one-line   *)
(*     delegation to the built-in theory ([eq add(nat(X), nat(Y)) = nat(X+Y)]),*)
(*     constant-time via GMP instead of linear/quadratic structural recursion. *)
(*                                                                             *)
(* The wrappers keep every value in a spec-owned sort (NatV/IntV/BoolV/Text    *)
(* < Val), so the built-in sorts never sit under [Val]: their kinds stay       *)
(* separate and the imported NAT/STRING/BOOL operators cannot clash. A stuck   *)
(* subterm never looks like a wrapped literal, so the delegations' constructor *)
(* patterns reject it and stuckness propagates without extra guards.           *)
(*                                                                             *)
(* Everything structural -- user types, lists/options, iteration helpers,     *)
(* matchers, structural equality -- passes through unchanged.                  *)
(* -------------------------------------------------------------------------- *)

(* The wrapper constructors. The one place their symbols are spelled, shared
   by this pass, {!To_maude}'s delegation equations, and the start-term
   encoder. *)
let nat_wrap_sym = "nat"
let int_wrap_sym = "int"
let bool_wrap_sym = "bool"
let text_wrap_sym = "txt"

(* A built-in literal as a CTRS symbol, printed verbatim by {!To_maude} (no
   identifier mangling, no op declaration): a decimal numeral, a quoted
   string, or the [Bool] constants (which only occur inside the [bool]
   wrapper after this pass). *)
let is_literal_sym (s : string) : bool =
  let digits s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s in
  s = "true" || s = "false"
  || String.length s > 0
     && (s.[0] = '"'
        || digits s
        || (s.[0] = '-' && digits (String.sub s 1 (String.length s - 1))))

(* A Maude string literal for [s] (C-style escapes). Printable ASCII passes
   through; quote/backslash and tab/newline take their named escapes; every
   other byte (control bytes, and the individual bytes of UTF-8 sequences such
   as the [\342] of an arrow [⟶] in a P4 [@name] annotation) takes a 3-digit
   OCTAL escape, which is what Maude's STRING reader expects ([\065] is ['5'],
   not ['A']). The fixed width keeps a following digit from being absorbed. *)
let string_literal (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\t' -> Buffer.add_string b "\\t"
      | '\n' -> Buffer.add_string b "\\n"
      | c when c >= ' ' && c <= '~' -> Buffer.add_char b c
      | c -> Buffer.add_string b (Printf.sprintf "\\%03o" (Char.code c)))
    s;
  Buffer.add_char b '"';
  Buffer.contents b

(* Wrapped-literal builders (also used by the start-term encoder). *)
let nat_t (i : Bigint.t) : R.term =
  R.App (nat_wrap_sym, [ R.App (Bigint.to_string i, []) ])

let int_t (i : Bigint.t) : R.term =
  R.App (int_wrap_sym, [ R.App (Bigint.to_string i, []) ])

let bool_t (b : bool) : R.term =
  R.App (bool_wrap_sym, [ R.App ((if b then "true" else "false"), []) ])

let text_t (s : string) : R.term =
  R.App (text_wrap_sym, [ R.App (string_literal s, []) ])

(* The value of a ground Peano tower, [None] when symbolic anywhere. *)
let rec peano_value (t : R.term) : int option =
  match t with
  | R.App ("zero", []) -> Some 0
  | R.App ("succ", [ t' ]) -> Option.map (( + ) 1) (peano_value t')
  | _ -> None

(* The string of a ground char-list chain [cons(chr_a, ... nil)], [None] when
   any element is not a [chr] or the spine is symbolic. A bare [nil] does NOT
   qualify: without the leading [chr] it is indistinguishable from an empty
   LIST, so an empty text literal stays [nil] and {!To_maude} bridges it into
   [Text] positions (the pre-existing [List < Text] subsort). *)
let chars_value (t : R.term) : string option =
  let rec go t =
    match t with
    | R.App ("nil", []) -> Some ""
    | R.App ("cons", [ R.App (c, []); rest ]) -> (
        match To_ctrs.chr_code_of_sym c with
        | Some code ->
            Option.map (fun s -> String.make 1 (Char.chr code) ^ s) (go rest)
        | None -> None)
    | _ -> None
  in
  match t with R.App ("cons", _) -> go t | _ -> None

(* Restate one term: fold ground scalar values into wrapped literals, keep
   everything else (symbols, variables, structure) as is. Symbolic [succ]/
   [int_pos]/[int_neg]/[chr] occurrences are left for the delegations. *)
let rec native_term (t : R.term) : R.term =
  match t with
  | R.Var _ -> t
  | R.App ("true", []) -> bool_t true
  | R.App ("false", []) -> bool_t false
  | R.App (("zero" | "succ"), _) when peano_value t <> None ->
      nat_t (Bigint.of_int (Option.get (peano_value t)))
  | R.App ("int_pos", [ m ]) when peano_value m <> None ->
      int_t (Bigint.of_int (Option.get (peano_value m)))
  | R.App ("int_neg", [ m ]) when peano_value m <> None ->
      int_t (Bigint.of_int (-Option.get (peano_value m) - 1))
  | R.App ("cons", _) when chars_value t <> None ->
      text_t (Option.get (chars_value t))
  | R.App (f, args) -> R.App (f, List.map native_term args)

(* Whether a rule defines a replaced scalar: its head is a delegated operator,
   or it is an [eq] rule over scalar constructors (Peano/sign-magnitude/bool/
   char pairs -- the structural [eq] rules over options, lists and user types
   are kept). *)
let scalar_pat (t : R.term) : bool =
  match t with
  | R.App (("zero" | "succ" | "int_pos" | "int_neg" | "true" | "false"), _) ->
      true
  | R.App (c, []) -> To_ctrs.chr_code_of_sym c <> None
  | _ -> false

(* The text builtins {!To_maude} restates over [String] ([substr]/[length]/
   decimal conversion); their structural char-list rules are dropped along
   with their private helpers (`$<id>_<suffix>`). *)
let replaced_builtin_prefixes =
  [ "$int_to_text"; "$strip_prefix"; "$strip_suffix" ]

let replaced_rule (r : R.rule) : bool =
  match R.defined_head r with
  | None -> false
  | Some h -> (
      List.mem h To_ctrs.native_replaced_heads
      || List.exists
           (fun p -> String.starts_with ~prefix:p h)
           replaced_builtin_prefixes
      ||
      match r.R.lhs with
      | R.App ("eq", [ a; b ]) -> scalar_pat a || scalar_pat b
      | _ -> false)

(* The execution-pipeline system: the structural CTRS with its scalar theory
   replaced by Maude's built-ins (see the header comment). *)
let native_system (sys : R.t) : R.t =
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
  { sys with R.rules; vars }
