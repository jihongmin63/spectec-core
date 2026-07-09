open Common.Source
open Lang.Il
module R = Rewrite_system
module T = Ctrs_term

(** Back-translate a Maude object term (the normal form {!Maude_run} prints as
    [result: <term>]) into a SpecTec IL {!Lang.Il.value}, the inverse of
    {!To_maude.encode_value} (+ [print_term]). This lets the same-spec oracle
    compare not just the PASS/STUCK verdict but the typing RESULT value the
    interpreter computes against the one Maude reduced to.

    Two stages: (a) a small recursive-descent parser reads the term text into a
    {!mt} tree; (b) a decoder maps each symbol back to its IL constructor using
    a FORWARD table built from the spec (the sanitizing
    [variant_sym]/[struct_sym] spelling is lossy, so the reverse is read off the
    spec, not the string).

    Scalars are unambiguous from their wrapper ([nat]/[int]/[bool]/[txt]), so
    the nat-vs-int and (non-empty) text-vs-list distinctions need no type
    context. The one ambiguity is the bare [nil]: an empty text [TextE ""] and
    an empty list both encode to it (the empty-text-as-[nil] convention), so the
    decoder threads the EXPECTED type (the same [expected]
    {!To_maude.encode_value} threads) to put a [nil] in a text position back as
    [TextV ""]. *)

(* Lexical layer: the forward table keys must match what Maude prints, so a
   constructor symbol is mangled with the same [_]->[-] map the emitter used
   ({!Rewrite_system.maude_id}, referenced as [R.maude_id] below and shared with
   both Maude surfaces). Variant/struct arms key the forward table by that
   maude-spelled symbol directly; only the scalar arms matching {!Ctrs_term}'s
   own underscored [chr_<code>]/[int_pos]/[int_neg] spelling undo the mangling
   first, via {!ctrs_sym}. *)

(* -------------------------------------------------------------------------- *)
(* The parsed Maude term. *)

type mt =
  | MApp of string * mt list
      (** operator application [f(a, ..)] / constant [f] *)
  | MStr of string  (** a string literal, decoded to its raw bytes *)
  | MNum of string  (** a decimal numeral, possibly signed *)

exception Parse_error of string

(* Decode a Maude string literal body (between the quotes) to its raw bytes:
   the inverse of {!Maude_theory.string_literal} -- named escapes plus 3-digit
   octal bytes (an unknown escape keeps the following char verbatim). *)
let unescape (s : string) : string =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let is_oct c = c >= '0' && c <= '7' in
  let rec go i =
    if i >= n then ()
    else if s.[i] = '\\' && i + 1 < n then (
      match s.[i + 1] with
      | 'n' ->
          Buffer.add_char b '\n';
          go (i + 2)
      | 't' ->
          Buffer.add_char b '\t';
          go (i + 2)
      | 'r' ->
          Buffer.add_char b '\r';
          go (i + 2)
      | '"' ->
          Buffer.add_char b '"';
          go (i + 2)
      | '\\' ->
          Buffer.add_char b '\\';
          go (i + 2)
      | c when is_oct c && i + 3 < n && is_oct s.[i + 2] && is_oct s.[i + 3] ->
          Buffer.add_char b
            (Char.chr (int_of_string ("0o" ^ String.sub s (i + 1) 3)));
          go (i + 4)
      | c ->
          Buffer.add_char b c;
          go (i + 2))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

(* Recursive-descent parse of one Maude object term. The term is the single-line
   string {!Maude_run.gather_term} already produced (continuation lines joined),
   so there is no continuation handling here. *)
let parse (input : string) : mt =
  let n = String.length input in
  let pos = ref 0 in
  let peek () = if !pos < n then Some input.[!pos] else None in
  let skip_ws () =
    while
      !pos < n
      && (input.[!pos] = ' ' || input.[!pos] = '\n' || input.[!pos] = '\t')
    do
      incr pos
    done
  in
  let is_id_char c =
    (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || c = '-' || c = '_' || c = '$'
  in
  let is_digit c = c >= '0' && c <= '9' in
  let expect c =
    if !pos < n && input.[!pos] = c then incr pos
    else
      raise
        (Parse_error (Printf.sprintf "expected %c at %d in: %s" c !pos input))
  in
  let read_while p =
    let start = !pos in
    while !pos < n && p input.[!pos] do
      incr pos
    done;
    String.sub input start (!pos - start)
  in
  (* a maude string literal, honoring backslash escapes for quote/backslash *)
  let read_string () =
    expect '"';
    let b = Buffer.create 16 in
    let rec go () =
      if !pos >= n then raise (Parse_error "unterminated string")
      else
        let c = input.[!pos] in
        if c = '"' then incr pos
        else if c = '\\' && !pos + 1 < n then (
          Buffer.add_char b c;
          Buffer.add_char b input.[!pos + 1];
          pos := !pos + 2;
          go ())
        else (
          Buffer.add_char b c;
          incr pos;
          go ())
    in
    go ();
    MStr (unescape (Buffer.contents b))
  in
  let rec term () =
    skip_ws ();
    match peek () with
    | Some '"' -> read_string ()
    | Some '(' ->
        (* grouping, possibly a sort-qualified constant [(c).Sort] *)
        incr pos;
        let t = term () in
        skip_ws ();
        expect ')';
        skip_ws ();
        if peek () = Some '.' then (
          incr pos;
          ignore (read_while is_id_char));
        t
    | Some '-' ->
        incr pos;
        skip_ws ();
        MNum ("-" ^ read_while is_digit)
    | Some c when is_digit c -> MNum (read_while is_digit)
    | Some c when is_id_char c ->
        let id = read_while is_id_char in
        skip_ws ();
        if peek () = Some '(' then (
          incr pos;
          let args = args () in
          skip_ws ();
          expect ')';
          MApp (id, args))
        else MApp (id, [])
    | _ ->
        raise
          (Parse_error (Printf.sprintf "unexpected input at %d: %s" !pos input))
  and args () =
    let first = term () in
    skip_ws ();
    if peek () = Some ',' then (
      incr pos;
      first :: args ())
    else [ first ]
  in
  let t = term () in
  t

(* -------------------------------------------------------------------------- *)
(* Forward tables: maude symbol -> the IL constructor it spells. *)

type tables = {
  tenv : (string, deftyp') Hashtbl.t;  (** alias resolution, like {!To_maude} *)
  variants : (string, string * mixop * typ' list) Hashtbl.t;
      (** [maude_id (variant_sym origin mixop)] -> (origin, mixop, field types)
      *)
  structs : (string, string * (atom * typ') list) Hashtbl.t;
      (** [maude_id (struct_sym t)] -> (t, fields) *)
}

let case_origin_mixop (tc : typcase) : string * mixop =
  (tc.origin.it.synid.it, Mixfix.to_mixop tc.notation.it)

let build_tables (orig : spec) : tables =
  let tenv = Hashtbl.create 256 in
  let variants = Hashtbl.create 512 in
  let structs = Hashtbl.create 256 in
  List.iter
    (fun def ->
      match def.it with
      | TypD { synid = tid; deftyp = dt; _ } -> (
          Hashtbl.replace tenv tid.it dt.it;
          match dt.it with
          | VariantT typcases ->
              List.iter
                (fun (tc : typcase) ->
                  let origin, mixop = case_origin_mixop tc in
                  let ftyps =
                    List.map (fun t -> t.it) (Mixfix.args tc.notation.it)
                  in
                  Hashtbl.replace variants
                    (R.maude_id (T.variant_sym origin mixop))
                    (origin, mixop, ftyps))
                typcases
          | StructT fields ->
              let fields = List.map (fun (a, t) -> (a, t.it)) fields in
              Hashtbl.replace structs
                (R.maude_id (T.struct_sym tid.it))
                (tid.it, fields)
          | PlainT _ -> ())
      | _ -> ())
    orig;
  { tenv; variants; structs }

(* One-slot memo on the spec (physical equality), like {!To_maude.meta_signature}:
   a whole batch decodes against the same spec, so the tables are built once. *)
let memo : (spec * tables) option ref = ref None

let tables_of (orig : spec) : tables =
  match !memo with
  | Some (o, t) when o == orig -> t
  | _ ->
      let t = build_tables orig in
      memo := Some (orig, t);
      t

(* Follow [syntax T = U] aliases to the underlying type (mirrors
   {!To_maude.sort_of_typ}'s alias handling). *)
let rec resolve (tbl : tables) (ty : typ') : typ' =
  match ty with
  | VarT { synid; _ } -> (
      match Hashtbl.find_opt tbl.tenv synid.it with
      | Some (PlainT u) -> resolve tbl u.it
      | _ -> ty)
  | _ -> ty

(* -------------------------------------------------------------------------- *)
(* Decode a parsed term to an IL value, threading the expected type. *)

let var_typ (name : string) : typ' =
  VarT { synid = name $ no_region; targs = [] }

(* The element type of an expected iteration/tuple type, when known. *)
let iter_elem (tbl : tables) (expected : typ' option) (iter : iter) :
    typ' option =
  match Option.map (resolve tbl) expected with
  | Some (IterT { typ; iter = it }) when it = iter -> Some typ.it
  | _ -> None

(* A structural Peano nat ([zero]/[succ]) as a [Bigint]. Only ever reached
   decoding {!To_mfe}'s module output (the [Native] module never emits bare
   [zero]/[succ] -- its nats are the [nat(..)] wrapper around a Maude numeral,
   {!Ctrs_term.nat_lit}), so no [scalars] flag is needed: the two theories'
   scalar vocabularies never overlap (see the module doc comment). *)
let rec bigint_of_peano (m : mt) : Bigint.t =
  match m with
  | MApp ("zero", []) -> Bigint.zero
  | MApp ("succ", [ n ]) -> Bigint.succ (bigint_of_peano n)
  | _ -> raise (Parse_error "expected a Peano nat (zero/succ)")

(* A binary (Coq [positive]/[N]-style) [BNatV] magnitude -- [int_pos]/
   [int_neg]'s payload -- as a [Bigint], the decode-side counterpart of
   {!Ctrs_term.binary_of_bigint}. *)
let rec bigint_of_binary (m : mt) : Bigint.t =
  match m with
  | MApp ("bzero", []) -> Bigint.zero
  | MApp ("bone", []) -> Bigint.one
  | MApp ("bd0", [ p ]) -> Bigint.( * ) (Bigint.of_int 2) (bigint_of_binary p)
  | MApp ("bd1", [ p ]) ->
      Bigint.succ (Bigint.( * ) (Bigint.of_int 2) (bigint_of_binary p))
  | _ -> raise (Parse_error "expected a binary BNatV (bzero/bone/bd0/bd1)")

(* Undo {!R.maude_id}'s [_]->[-] mangling on a parsed symbol, recovering
   {!Ctrs_term}'s own spelling. Injective because a CTRS id never contains [-]
   (see {!R.maude_id}). The scalar arms that match [Ctrs_term]'s underscored
   [chr_<code>]/[int_pos]/[int_neg] symbols need this because the parser reads
   the already-mangled ([chr-<code>]/[int-pos]/[int-neg]) spelling Maude prints;
   the variant/struct arms don't (they key the forward table by that mangled
   spelling directly). *)
let ctrs_sym (s : string) : string =
  String.map (fun c -> if c = '-' then '_' else c) s

(* A structural char-list text (a [cons]/[nil] spine of [chr_<code>] leaves) as
   its byte codes, innermost first is outermost in the source order (the
   spine is already left-to-right). Reused by the [cons] decode arm below when
   [expected] says the position wants [TextT] -- the same disambiguation the
   [nil] arm already makes for the empty string. *)
let rec char_spine (m : mt) : int list =
  match m with
  | MApp ("nil", []) -> []
  | MApp ("cons", [ MApp (sym, []); t ])
    when T.chr_code_of_sym (ctrs_sym sym) <> None ->
      Option.get (T.chr_code_of_sym (ctrs_sym sym)) :: char_spine t
  | MApp ("cons", [ _; _ ]) ->
      raise (Parse_error "expected a char-list element (chr_<code>)")
  | _ -> raise (Parse_error "malformed char-list spine")

let string_of_char_codes (codes : int list) : string =
  let b = Buffer.create (List.length codes) in
  List.iter (fun c -> Buffer.add_char b (Char.chr c)) codes;
  Buffer.contents b

let rec decode (tbl : tables) (expected : typ' option) (m : mt) : value =
  match m with
  | MStr s -> Value.Make.text TextT s
  (* Structural scalar leaves ({!To_mfe}'s module: Peano nats, sign-magnitude
     ints, bare booleans -- {!Ctrs_term}'s own constructors, never emitted by
     the [Native] module, so these arms are unreachable there). *)
  | MApp ("zero", []) -> Value.Make.nat (NumT `NatT) Bigint.zero
  | MApp ("succ", [ _ ]) -> Value.Make.nat (NumT `NatT) (bigint_of_peano m)
  | MApp (sym, [ n ]) when ctrs_sym sym = "int_pos" ->
      Value.Make.int (NumT `IntT) (bigint_of_binary n)
  | MApp (sym, [ n ]) when ctrs_sym sym = "int_neg" ->
      Value.Make.int (NumT `IntT)
        (Bigint.neg (Bigint.succ (bigint_of_binary n)))
  | MApp ("true", []) -> Value.Make.bool BoolT true
  | MApp ("false", []) -> Value.Make.bool BoolT false
  | MNum s -> (
      (* a bare numeral with no wrapper: take the sign/expectation as the type *)
      match Option.map (resolve tbl) expected with
      | Some (NumT `IntT) -> Value.Make.int (NumT `IntT) (Bigint.of_string s)
      | _ -> Value.Make.nat (NumT `NatT) (Bigint.of_string s))
  | MApp ("nat", [ MNum s ]) -> Value.Make.nat (NumT `NatT) (Bigint.of_string s)
  | MApp ("int", [ arg ]) -> (
      match arg with
      | MNum s -> Value.Make.int (NumT `IntT) (Bigint.of_string s)
      | _ -> raise (Parse_error "int wrapper without a numeral"))
  | MApp ("bool", [ MApp ("true", []) ]) -> Value.Make.bool BoolT true
  | MApp ("bool", [ MApp ("false", []) ]) -> Value.Make.bool BoolT false
  | MApp ("txt", [ MStr s ]) -> Value.Make.text TextT s
  | MApp ("none", []) ->
      Value.Make.opt (Option.value expected ~default:(var_typ "anon")) None
  | MApp ("some", [ v ]) ->
      let elem = iter_elem tbl expected Opt in
      Value.Make.opt
        (Option.value expected ~default:(var_typ "anon"))
        (Some (decode tbl elem v))
  | MApp ("nil", []) -> (
      (* the bare-nil ambiguity: an empty text vs an empty list *)
      match Option.map (resolve tbl) expected with
      | Some TextT -> Value.Make.text TextT ""
      | _ ->
          Value.Make.list (Option.value expected ~default:(var_typ "anon")) [])
  | MApp ("cons", [ _; _ ]) when Option.map (resolve tbl) expected = Some TextT
    ->
      (* the structural char-list spelling of a non-empty text (the empty
         string is the bare [nil] the arm above already handles, same
         convention as [Native]'s wrapper-vs-bare-nil choice) *)
      Value.Make.text TextT (string_of_char_codes (char_spine m))
  | MApp ("cons", [ _; _ ]) ->
      let elem = iter_elem tbl expected List in
      let items = List.map (decode tbl elem) (list_spine m) in
      Value.Make.list (Option.value expected ~default:(var_typ "anon")) items
  | MApp ("tuple", parts) ->
      let elems =
        match Option.map (resolve tbl) expected with
        | Some (TupleT typs) when List.length typs = List.length parts ->
            List.map (fun t -> Some t.it) typs
        | _ -> List.map (fun _ -> None) parts
      in
      Value.Make.tuple
        (Option.value expected ~default:(var_typ "anon"))
        (List.map2 (decode tbl) elems parts)
  | MApp (sym, parts) when Hashtbl.mem tbl.variants sym ->
      let origin, mixop, ftyps = Hashtbl.find tbl.variants sym in
      let expts =
        if List.length ftyps = List.length parts then List.map Option.some ftyps
        else List.map (fun _ -> None) parts
      in
      let vals = List.map2 (decode tbl) expts parts in
      Value.Make.case (var_typ origin) (Mixfix.fill mixop vals)
  | MApp (sym, parts) when Hashtbl.mem tbl.structs sym ->
      let tid, fields = Hashtbl.find tbl.structs sym in
      let vals =
        if List.length fields = List.length parts then
          List.map2 (fun (_, ft) p -> decode tbl (Some ft) p) fields parts
        else List.map (decode tbl None) parts
      in
      let valuefields = List.map2 (fun (a, _) v -> (a, v)) fields vals in
      Value.Make.record (var_typ tid) valuefields
  | MApp (sym, _) -> raise (Parse_error ("unknown maude symbol: " ^ sym))

(* The element terms of a [cons]/[nil] spine. *)
and list_spine (m : mt) : mt list =
  match m with
  | MApp ("nil", []) -> []
  | MApp ("cons", [ h; t ]) -> h :: list_spine t
  | _ -> raise (Parse_error "malformed list spine")

(* -------------------------------------------------------------------------- *)
(* Comparison canonicalization (applied to BOTH sides before [Eq.eq_values]).

   Two semantically-irrelevant representation choices make otherwise-equal
   results compare unequal; normalizing both sides factors them out without
   masking a genuine difference.

   1. {b Fresh names.} The interpreter and the Maude translation spell the
      gensym-issued identifiers ({!Gensym}) differently -- the interpreter's
      [$fresh_*] counter yields [FRESH__0], [FRESH__1], ..., while the threaded
      translation appends primes ([FRESH'], [FRESH''], ...). So the two are equal
      only up to a consistent renaming. Rename each fresh-prefixed text leaf to
      [FRESH#k] in first-appearance order (a fixed left-to-right traversal); no
      renaming can repair a genuine structural difference.

   2. {b Map order.} A [map<K,V>] is semantically UNORDERED. The interpreter
      stores it in a [Map.Make(Value)] and renders it [VMap.bindings]-sorted (by
      [Value.compare] on the key); the Maude translation keeps an insertion-
      ordered association list ([Builtin.add_map]). Comparing the two as ordered
      lists is the WRONG comparison: sort each map's entries by the SAME
      [Value.compare] key order the interpreter's [VMap] uses, so the maps are
      compared as the unordered structures they are. (A real key/value content
      difference survives the sort and still shows.) A map value is the brace
      constructor [{ pair* }] ({!Targets_p4...maps.ml}'s [value_of_map] /
      {!Builtin}'s [set]/[pair]); a [pair] is the [key : value] colon case. *)

(* The key of a [pair] case [key : value] (its first argument). *)
let pair_key (v : value) : value option =
  match v.it with
  | CaseV vc -> (
      match (Mixfix.atoms vc, Mixfix.args vc) with
      | [ a ], (k :: _ as args)
        when Xl.Atom.to_string a.it = ":" && List.length args = 2 ->
          Some k
      | _ -> None)
  | _ -> None

(* If [vc] is a brace map constructor [{ pair* }], return its entries sorted by
   [Value.compare] on the pair key; otherwise [None]. *)
let sorted_map_entries (vc : valuecase) : value list option =
  match (Mixfix.atoms vc, Mixfix.args vc) with
  | atoms, [ { it = ListV entries; _ } ]
    when List.exists (fun a -> a.it = Xl.Atom.LBrace) atoms
         && entries <> []
         && List.for_all (fun e -> pair_key e <> None) entries ->
      Some
        (List.stable_sort
           (fun a b ->
             Lang.Il.Value.compare
               (Option.get (pair_key a))
               (Option.get (pair_key b)))
           entries)
  | _ -> None

let canonicalize (vs : value list) : value list =
  let tbl = Hashtbl.create 16 and next = ref 0 in
  let canon s =
    if not (String.starts_with ~prefix:Gensym.seed_text s) then s
    else
      let k =
        match Hashtbl.find_opt tbl s with
        | Some k -> k
        | None ->
            let k = !next in
            incr next;
            Hashtbl.replace tbl s k;
            k
      in
      Printf.sprintf "%s#%d" Gensym.seed_text k
  in
  let rec go (v : value) : value =
    let it =
      match v.it with
      | TextV s -> TextV (canon s)
      | (BoolV _ | NumV _ | FuncV _) as it -> it
      | StructV fs -> StructV (List.map (fun (a, x) -> (a, go x)) fs)
      | CaseV vc -> (
          let vc = Mixfix.map go vc in
          match sorted_map_entries vc with
          | Some sorted ->
              CaseV
                (Mixfix.map
                   (function
                     | { it = ListV _; note; _ } -> ListV sorted $$$ note
                     | x -> x)
                   vc)
          | None -> CaseV vc)
      | TupleV xs -> TupleV (List.map go xs)
      | OptV o -> OptV (Option.map go o)
      | ListV xs -> ListV (List.map go xs)
    in
    Value.Make.value v.note.typ it
  in
  List.map go vs

(* -------------------------------------------------------------------------- *)
(* Public entry: a Maude result term -> the decoded output value(s). *)

(* The output types of relation [rel] (by name), read from the spec's [RelD]. *)
let relation_output_typs (orig : spec) (rel : string) : typ' list =
  Option.value ~default:[]
  @@ List.find_map
       (fun def ->
         match def.it with
         | RelD { relid; reltyp; _ } when relid.it = rel ->
             let arg_typs = Mixfix.args (Mode.notation reltyp.it) in
             let _, outs = Mode.partition reltyp.it arg_typs in
             Some (List.map (fun t -> t.it) outs)
         | _ -> None)
       orig

(* Decode the [result:] term of running relation [rel] back to the IL output
   value(s), stripping the gensym state component when [rel] is threaded
   ({!Gensym}). The result mirrors the interpreter's relation OUTPUT list, so the
   two are directly [Eq.eq_values]-comparable.

   [term] is the object-syntax normal form (as in {!Maude_run.Reduced}). A run
   that does not denote a clean value (e.g. a stuck term) raises
   {!Parse_error}. *)
let values_of_result (orig : spec) ~(rel : string) (term : string) : value list
    =
  let tbl = tables_of orig in
  let parsed = parse term in
  let out_typs = relation_output_typs orig rel in
  let nout = List.length out_typs in
  let effectful =
    List.mem (R.sanitize rel)
      (Gensym.effectful_syms (Pipeline.maude_system_of_spec orig))
  in
  let total = nout + if effectful then 1 else 0 in
  (* The components of the result: a single output is the bare term; several (or
     a single output plus the threaded state) are a [tuple(..)]. *)
  let comps =
    if total <= 1 then [ parsed ]
    else
      match parsed with
      | MApp ("tuple", parts) when List.length parts = total -> parts
      | _ ->
          raise
            (Parse_error
               (Printf.sprintf "expected tuple of %d for %s, got: %s" total rel
                  term))
  in
  (* drop the trailing gensym state, then decode each output at its declared type *)
  let outputs =
    if effectful then List.filteri (fun i _ -> i < nout) comps else comps
  in
  List.map2 (fun ty m -> decode tbl (Some ty) m) out_typs outputs
