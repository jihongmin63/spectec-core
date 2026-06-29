module R = Rewrite_system

(* -------------------------------------------------------------------------- *)
(* The native (built-in) scalar theory: the spelling and literal builders for  *)
(* the execution pipeline's scalar wrappers.                                   *)
(*                                                                             *)
(* The analysis pipeline keeps self-contained structural scalars (Peano nats,  *)
(* sign-magnitude ints, char-list texts, own booleans) because a CTRS has no   *)
(* external theories. The execution pipeline instead targets Maude's built-in  *)
(* Bool/Nat/Int/String: ground scalar values live in wrapper constructors over *)
(* those sorts ([nat(3)], [int(-5)], [bool(true)], [txt("E.")]), and the       *)
(* scalar prelude rules ({!Prelude.native_replaced_heads}) are re-emitted by   *)
(* {!To_maude} as one-line delegations ([eq add(nat(X), nat(Y)) = nat(X+Y)]),  *)
(* constant-time via GMP. The wrappers keep every value in a spec-owned sort   *)
(* (NatV/IntV/BoolV/Text < Val), so the built-in sorts never sit under [Val]   *)
(* and the imported NAT/STRING/BOOL operators cannot clash.                    *)
(*                                                                             *)
(* In the new-rewrite design the wrappers are produced DIRECTLY by             *)
(* {!To_ctrs.of_spec} with [~scalars:Native] -- there is no separate fold pass *)
(* over a structural system (the old [native_system] is gone). This module is  *)
(* now just the shared low-level home for the wrapper spelling and the literal *)
(* builders that {!To_ctrs} (Native scalar emission), {!To_maude} (delegation  *)
(* equations, start-term encoder) and {!Of_maude} (decoding) must all agree on. *)
(* -------------------------------------------------------------------------- *)

(* The wrapper constructors. The one place their symbols are spelled, shared
   by {!To_ctrs}'s Native emission, {!To_maude}'s delegation equations and
   start-term encoder, and {!Of_maude}'s decoder. *)
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
