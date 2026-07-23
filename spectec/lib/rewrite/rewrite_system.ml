(** Conditional term-rewriting system (CTRS) representation produced from an
    elaborated + simplified IL spec.

    Only the representation, the diagnostic printer, and the shared Maude
    lexical layer live here; the IL -> CTRS translation is in {!To_ctrs}, and
    the order-sorted Maude module surfaces are {!To_maude} (execution) and
    {!To_mfe} (analysis, for the Maude Formal Environment). *)

(* A CTRS term: either a variable, or a function symbol applied to zero or more
   argument terms. A nullary application prints as a bare [id]. *)
type term = Var of string | App of string * term list

(* A condition is an equation between two terms. *)
type cond = term * term

(* A (possibly conditional) rewrite rule [lhs -> rhs | conds]. [owise] marks a
   clause that applied "otherwise" (SpecTec [ElsePr]): it fires only when no
   earlier sibling did. The Maude surfaces ({!To_maude}/{!To_mfe}) render it as
   Maude's [owise] equation attribute. *)
type rule = { lhs : term; rhs : term; conds : cond list; owise : bool }

type t = {
  vars : string list; (* every variable used, deduplicated *)
  rules : rule list;
}

(* The IL -> CTRS translation lives in {!To_ctrs}; this module is only the data
   model and printer. *)

(* -------------------------------------------------------------------------- *)
(* CTRS identifier lexical conventions.

   Scrubbing an arbitrary string into a CTRS-safe identifier lives here at the
   data model, not in the symbol-naming layer, because BOTH that layer
   ({!Ctrs_term}, which builds every rule's symbols) and the Maude surfaces
   ({!To_mfe} analysis and {!To_maude} execution) must agree on the exact
   spelling -- so the one definition sits at the layer all of them can reach. *)

(* A readable token for a non-alphanumeric character so symbolic notations keep
   distinct, legible names (e.g. [`+`] -> "plus", not the empty string). A prime
   ['] is kept as "prime" because it distinguishes sibling definitions ([f] vs
   [f'] vs [f'']) that would otherwise collide on the same symbol; backticks,
   double quotes and whitespace are dropped; truly unknown symbols become "sym". *)
let mnemonic_of_char (c : char) : string =
  match c with
  | '+' -> "plus"
  | '-' -> "minus"
  | '*' -> "star"
  | '/' -> "slash"
  | '\\' -> "backslash"
  | '<' -> "lt"
  | '>' -> "gt"
  | '=' -> "eq"
  | '!' -> "bang"
  | '?' -> "quest"
  | '&' -> "amp"
  | '|' -> "bar"
  | '^' -> "caret"
  | '~' -> "tilde"
  | '%' -> "percent"
  | '.' -> "dot"
  | ',' -> "comma"
  | ';' -> "semi"
  | ':' -> "colon"
  | '#' -> "hash"
  | '$' -> "dollar"
  | '@' -> "at"
  | '(' -> "lparen"
  | ')' -> "rparen"
  | '[' -> "lbrack"
  | ']' -> "rbrack"
  | '{' -> "lbrace"
  | '}' -> "rbrace"
  | '\'' -> "prime"
  | '`' | '"' | ' ' | '_' -> ""
  | _ -> "sym"

(* Scrub a string into a CTRS-safe identifier: maximal [A-Za-z0-9] runs are kept,
   every other character is replaced by a mnemonic token, tokens are joined with
   [_], an alphabetic lead is guaranteed, and the result is never empty. Distinct
   inputs may still collide (a known first-cut limitation). *)
let sanitize (s : string) : string =
  let is_alnum c =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
  in
  (* Accumulate completed tokens (reversed) plus the current alphanumeric run;
     [run] is committed to [tokens] whenever a non-alphanumeric breaks it. *)
  let commit run tokens = if run = "" then tokens else run :: tokens in
  let tokens, run =
    String.fold_left
      (fun (tokens, run) c ->
        if is_alnum c then (tokens, run ^ String.make 1 c)
        else
          match mnemonic_of_char c with
          | "" -> (commit run tokens, "")
          | m -> (m :: commit run tokens, ""))
      ([], "") s
  in
  let r = String.concat "_" (List.rev (commit run tokens)) in
  if r = "" then "anon"
  else if r.[0] >= '0' && r.[0] <= '9' then "c_" ^ r
  else r

let rec string_of_term = function
  | Var id -> id
  | App (id, []) -> id
  | App (id, terms) ->
      id ^ "(" ^ String.concat ", " (List.map string_of_term terms) ^ ")"

(* A single rule for debug/error messages: [lhs -> rhs], any conditions appended
   as [ | s == t, ...]. Not a surface a tool parses -- the Maude surfaces are
   {!To_maude}/{!To_mfe}; this is only for human-readable diagnostics. *)
let string_of_rule { lhs; rhs; conds; _ } =
  let head = string_of_term lhs ^ " -> " ^ string_of_term rhs in
  match conds with
  | [] -> head
  | _ ->
      head ^ " | "
      ^ String.concat ", "
          (List.map
             (fun (l, r) -> string_of_term l ^ " == " ^ string_of_term r)
             conds)

(* -------------------------------------------------------------------------- *)
(* Term/rule queries shared by the translation ({!To_ctrs}) and slicing below. *)

(* Every variable occurring in a term. *)
let rec vars_of_term = function
  | Var v -> [ v ]
  | App (_, ts) -> List.concat_map vars_of_term ts

(* Every variable occurring in a rule (lhs, rhs and conditions). *)
let vars_of_rule (r : rule) : string list =
  vars_of_term r.lhs @ vars_of_term r.rhs
  @ List.concat_map (fun (a, b) -> vars_of_term a @ vars_of_term b) r.conds

(* Drop later duplicates, preserving first-occurrence order. *)
let dedup_stable (xs : string list) : string list =
  let seen = Hashtbl.create 64 in
  List.filter
    (fun x ->
      if Hashtbl.mem seen x then false
      else (
        Hashtbl.add seen x ();
        true))
    xs

(* Rebuild a system from its rules, recomputing the variable list in stable
   first-occurrence order. Every pass that filters, rewrites or extends the
   rule list closes with this. *)
let of_rules (rules : rule list) : t =
  { rules; vars = dedup_stable (List.concat_map vars_of_rule rules) }

(* Every function symbol applied anywhere in a term (the [App] heads). *)
let rec heads_of_term = function
  | Var _ -> []
  | App (head, ts) -> head :: List.concat_map heads_of_term ts

(* Symbols a rule references: every head in its lhs, rhs and conditions. The lhs
   root (the symbol the rule defines) is included too, harmless for the
   reachability closure below. *)
let refs_of_rule (r : rule) : string list =
  heads_of_term r.lhs @ heads_of_term r.rhs
  @ List.concat_map (fun (a, b) -> heads_of_term a @ heads_of_term b) r.conds

(* The symbol a rule defines: the root head of its lhs (always an [App] here). *)
let defined_head (r : rule) : string option =
  match r.lhs with App (head, _) -> Some head | Var _ -> None

(* Every symbol the system defines (the root of some rule's lhs): functions,
   relations, and prelude operations -- the symbols that should rewrite away,
   never a value constructor. A normal form still mentioning one of these is
   stuck (reduction halted mid-evaluation). *)
let defined_heads (t : t) : string list =
  dedup_stable (List.filter_map defined_head t.rules)

(* The function symbols reachable from [roots], following each reached symbol's
   defining rules in [rules] transitively (downward dependency closure). Used
   both to prune unreachable definitions and to slice the system to one
   symbol's dependencies. *)
let reachable_heads ~(roots : string list) (rules : rule list) :
    (string, unit) Hashtbl.t =
  let by_head = Hashtbl.create 256 in
  List.iter
    (fun r ->
      match defined_head r with
      | Some head ->
          let prev = try Hashtbl.find by_head head with Not_found -> [] in
          Hashtbl.replace by_head head (r :: prev)
      | None -> ())
    rules;
  let reachable = Hashtbl.create 256 in
  let worklist = ref roots in
  while !worklist <> [] do
    match !worklist with
    | [] -> ()
    | head :: rest ->
        worklist := rest;
        if not (Hashtbl.mem reachable head) then (
          Hashtbl.add reachable head ();
          match Hashtbl.find_opt by_head head with
          | Some rules ->
              worklist := List.concat_map refs_of_rule rules @ !worklist
          | None -> ())
  done;
  reachable

(* Restrict the system to the rules reachable from [roots] (each root's defining
   rules plus their transitive downward dependencies); variables are recomputed. *)
let slice (t : t) ~(roots : string list) : t =
  let reachable = reachable_heads ~roots t.rules in
  let rules =
    List.filter
      (fun r ->
        match defined_head r with
        | Some head -> Hashtbl.mem reachable head
        | None -> false)
      t.rules
  in
  of_rules rules

(* -------------------------------------------------------------------------- *)
(* Premise-binder normalization (analysis-surface confluence). *)

(* Substitute the variable [v] by [repl] throughout a term. *)
let rec subst_var (v : string) (repl : term) = function
  | Var u -> if u = v then repl else Var u
  | App (f, ts) -> App (f, List.map (subst_var v repl) ts)

(* Occurrences of variable [v] in a term. *)
let rec count_var (v : string) = function
  | Var u -> if u = v then 1 else 0
  | App (_, ts) -> List.fold_left (fun n t -> n + count_var v t) 0 ts

(* A value-constructor pattern: every applied head is a constructor (one no rule
   defines, [is_defined] false) and the leaves are variables. Folding such a
   pattern into a rule's lhs keeps the lhs a matchable value pattern. *)
let rec is_ctor_pattern (is_defined : string -> bool) = function
  | Var _ -> true
  | App (f, ts) ->
      (not (is_defined f)) && List.for_all (is_ctor_pattern is_defined) ts

(* Normalize every premise-bound variable out of the analysis system's rules so
   the MFE's Church-Rosser checker is not tripped by the spurious critical pairs
   the single-sort [prod = v] / [v = K(..)] condition rendering raises.

   The surface renders a CTRS join-condition as an equality, so the fresh
   variable a condition binds (a relation/function output, or a field a
   destructuring pattern extracts) is left FREE on the rule's right -- and the
   CRC, which never solves the (deterministic) binding, reports e.g.
   [#v# = v if prod = v /\ prod = #v#]. Per rule, a fixpoint folds each such
   binder back into the rule until none remains, applying the first applicable of:

   - {b inline} an output binder [(prod, v)] with [v] NOT head-bound: substitute
     [v := prod] into the rhs/conditions ([prod] a deterministic value, so this is
     semantics-preserving). The binder must be live and used once (or be a plain
     [Var] alias) so a producer is never duplicated;
   - {b fold} a PURE-accessor destructuring [(v, K(..))] -- [v] head-bound and
     used in no other condition, [K(..)] a constructor pattern: substitute
     [v := K(..)] EVERYWHERE, so [K]'s fresh field variables become
     lhs-pattern-bound (the binder moves from the right to the head, where Maude
     binds it by matching).

   Uniform over every rule, so a binder inside a recursive iteration helper
   ([$itercollect]/[$iterproj]) is normalized the same way. Gensym
   threading (run before this) binds a [tuple(out, state)], not a bare [Var], so a
   threaded binder is skipped. Analysis-surface only: {!To_maude} keeps the [:=]
   matching condition its stuck-head guard relies on. *)
let fold_premise_binders ?(aggressive = false) (t : t) : t =
  let defined = Hashtbl.create 512 in
  List.iter (fun h -> Hashtbl.replace defined h ()) (defined_heads t);
  let is_defined h = Hashtbl.mem defined h in
  let fold_rule (r : rule) : rule =
    (* The variable a condition binds and the term to fold it to, if any. *)
    let binding lhs_vars ~rhs ~others ((a, b) : cond) : (string * term) option =
      (* inline: [v] a non-head output bound to [prod] -- deterministic, since
         every SpecTecx relation is input-moded. *)
      let inline v prod =
        if List.mem v lhs_vars || List.mem v (vars_of_term prod) then None
        else
          let is_alias = match prod with Var _ -> true | _ -> false in
          let uses =
            count_var v rhs
            + List.fold_left
                (fun n (l, r) -> n + count_var v l + count_var v r)
                0 others
          in
          (* [aggressive] (CRC-only) drops the [uses = 1] cap: a
             deterministic producer may be duplicated because the CRC
             neither executes nor terminates the rules, only computes
             critical pairs -- and inlining a single-var binder removes
             the determinacy critical pair the [prod = v] condition
             would otherwise raise. Still meaning-preserving (an
             equivalence), unlike an unraveling. *)
          if uses >= 1 && (aggressive || is_alias || uses = 1) then
            Some (v, prod)
          else None
      in
      (* fold: [v] a head variable destructured against a constructor pattern.
         Only a PURE accessor -- [v] used in no other condition -- is folded:
         folding a guarded clause (where [v] also feeds a [match_*]/owise guard,
         e.g. [$lookup]'s [pair]) would strip the disjointness guard the CRC
         relies on and expose the clause's owise overlap (turning a YES into a
         MAYBE). *)
      let fold v pat =
        let pat_vars = vars_of_term pat in
        let used_elsewhere =
          List.exists (fun (l, r) -> count_var v l + count_var v r > 0) others
        in
        match pat with
        | App _
          when List.mem v lhs_vars && (not used_elsewhere)
               && count_var v r.lhs = 1
               && is_ctor_pattern is_defined pat
               && (not (List.mem v pat_vars))
               && not (List.exists (fun w -> List.mem w lhs_vars) pat_vars) ->
            Some (v, pat)
        | _ -> None
      in
      let binder v other =
        match inline v other with Some x -> Some x | None -> fold v other
      in
      match (a, b) with
      | Var v, _ -> (
          match binder v b with
          | Some x -> Some x
          | None -> ( match b with Var w -> binder w a | _ -> None))
      | _, Var w -> binder w a
      | _ -> None
    in
    let rec loop (r : rule) =
      let lhs_vars = vars_of_term r.lhs in
      let rec find before = function
        | [] -> None
        | c :: after -> (
            let others = List.rev_append before after in
            match binding lhs_vars ~rhs:r.rhs ~others c with
            | Some sub -> Some (sub, others)
            | None -> find (c :: before) after)
      in
      match find [] r.conds with
      | None -> r
      | Some ((v, repl), others) ->
          let sub = subst_var v repl in
          (* [inline] (above) substitutes a dec-function call [repl] straight
             into the rule because it has no relation head ("deterministic"),
             but a dec function can still be PARTIAL: stuck when none of its
             own conditional equations hold (e.g. [$add_var_t]'s "main must be
             a package" or "no duplicate identifier" premises). Dropping the
             binder without keeping that check turns a precondition on the
             ENCLOSING equation firing into an opaque value no caller ever
             re-inspects, so a program that should be rejected can silently
             "succeed" instead (confirmed on issue4140.p4/dup-param.p4-shaped
             cases). Re-adding [isStuckHead(repl) = false] preserves the guard
             using [repl] itself -- no fresh variable -- so it does not
             reintroduce the [prod = v] critical-pair problem this pass exists
             to avoid; a pure-constructor [repl] (not in [defined]) can never
             get stuck, so this only fires where a real check would otherwise
             be lost.

             The prepend can place the guard AHEAD of a condition that binds
             one of [repl]'s variables (use-before-bind); {!order_conds},
             run right after this pass in the pipeline, restores binding
             order. *)
          let guard =
            match repl with
            | App (f, _) when is_defined f ->
                [ (App ("isStuckHead", [ repl ]), App ("false", [])) ]
            | _ -> []
          in
          loop
            {
              r with
              lhs = sub r.lhs;
              rhs = sub r.rhs;
              conds = guard @ List.map (fun (l, rr) -> (sub l, sub rr)) others;
            }
    in
    let r = loop r in
    (* Drop conditions a substitution made trivially true ([t = t]). *)
    { r with conds = List.filter (fun (l, rr) -> l <> rr) r.conds }
  in
  let rules = List.map fold_rule t.rules in
  of_rules rules

(* Restore every rule's conditions to binding order. A condition [s = t]
   evaluates [s] and matches the result against the pattern [t], so [s]'s
   variables must already be bound (by the lhs or an earlier condition's
   pattern) when it runs, and [t]'s fresh variables become bound after it.
   The source spec satisfies this by construction, but pipeline passes build
   conditions out of that order -- {!fold_premise_binders} above prepends an
   [isStuckHead(repl) = false] guard ahead of the condition that binds
   [repl]'s variables. {!To_maude.print_conds} and
   {!Reflect.sibling_conds_guard} each re-fix the order locally, but
   {!To_mfe} prints source order (the CRC's rewrite encoding then reads a
   use-before-bind condition as a non-executable rule: "variable used before
   it is bound") and {!Reflect}'s [gen_*_holds] generators thread their
   substitution in source order. Normalize once, per rule: greedy stable
   readiness scheduling -- among the not-yet-emitted conditions, always take
   the EARLIEST whose evaluated side is fully bound. A well-ordered rule is
   reproduced verbatim, and re-running the pass is the identity. Which
   condition binds a variable is decided per rule by the schedule itself (the
   first scheduled pattern containing it); the same variable's binding
   position legitimately differs from rule to rule, and any later pattern
   occurrence is a plain equality check. Wired into the analysis pipeline
   ({!Pipeline.ctrs_of_spec}) only: the Native execution path's sole consumer
   is {!To_maude.print_conds}, which re-schedules regardless, so pre-ordering
   there would only churn the emitted module text. *)
let order_conds (t : t) : t =
  let order_rule (r : rule) : rule =
    if r.conds = [] then r
    else
      let ready bound (s, _) =
        List.for_all (fun v -> List.mem v bound) (vars_of_term s)
      in
      (* The earliest pending condition that is ready, and the rest in order. *)
      let take_ready bound pending =
        let rec go before = function
          | [] -> None
          | c :: after ->
              if ready bound c then Some (c, List.rev_append before after)
              else go (c :: before) after
        in
        go [] pending
      in
      let rec schedule bound pending acc =
        match pending with
        | [] -> List.rev acc
        | _ -> (
            match take_ready bound pending with
            | Some (((_, pat) as c), rest) ->
                schedule (bound @ vars_of_term pat) rest (c :: acc)
            | None ->
                (* No condition is ready: a genuinely unbound variable (a
                   cycle, or a free variable no pattern binds). The current
                   translation never produces one -- checked corpus-wide --
                   so keep the source order (behaviour unchanged) and warn,
                   mirroring To_maude.print_conds's fallback. *)
                Printf.eprintf
                  "rewrite: WARNING - unorderable conditions in a rule for %s\n"
                  (match defined_head r with Some h -> h | None -> "?");
                List.rev_append acc pending)
      in
      { r with conds = schedule (vars_of_term r.lhs) r.conds [] }
  in
  let rules = List.map order_rule t.rules in
  of_rules rules

(* CRC-only normalization: an aggressive single-variable inline (the
   [uses = 1] cap dropped) followed by a re-order. A condition
   [$f(A) = v] binding a single variable makes the Church-Rosser checker
   raise a determinacy critical pair ([$f] is a function, but the CRC
   does not know that); inlining [v := $f(A)] removes the condition and
   the pair. Inlining is an equivalence (meaning-preserving), so a
   verdict on the normalized system transfers to the original -- unlike
   an unraveling, which only REFLECTS confluence. Opt-in via
   [--crc-normalize]; NOT part of the shared [ctrs_of_spec] surface, and
   never seen by execution/termination/ChC. Tuple-pattern binders
   ([$f(A) = tuple(v, b)]) are handled by the [crc_unravel] pass below
   (reflect-only, so used UPGRADE-ONLY -- see there). *)
(* Unravel every remaining binding condition [s = t] (t a non-variable pattern
   introducing fresh variables -- [fold ~aggressive] above has already inlined
   the single-variable ones) into a fresh [crcu]/[crck] chain, moving the
   binding into a left-hand-side pattern. This removes the determinacy critical
   pair a [tuple(v, b) := $f(A)] condition raises, at the cost of possibly
   INTRODUCING sibling-overlap pairs: unraveling REFLECTS but does NOT PRESERVE
   confluence (Marchiori 1996; Nishida-Sakai-Sakabe LMCS 2012). So the result is
   used UPGRADE-ONLY -- a YES on the unraveled module proves the original
   confluent (soundness holds for these left-linear structural rules); a MAYBE
   is inconclusive and falls back to the original verdict.

   Siblings sharing a head are kept from colliding by SHARING a [crcu]: the chain
   operator is keyed by (current chain lhs, evaluated subject s), so rules that
   reach the same point with the same subject reuse one entry step and let their
   guards discriminate afterwards; kept variables ride in a [crck] container.
   [crcu]/[crck] are declared nowhere -- {!Maude_sorts.signature} gives an
   unknown symbol the default [Val ... -> Val], exactly what these encode (so
   To_mfe needs no change and no fresh sort is introduced). [owise] rules are
   left intact. *)
let crc_unravel (t : t) : t =
  (* Only a binding condition whose SUBJECT is a defined-function application
     raises a determinacy critical pair -- that is the whole reason to unravel.
     A destructure of an already-bound value ([v = K(..)], or [K(..) = v]) has
     no such pair: the CRC handles it by unification, and
     {!Reflect.hoist_matchers} deliberately respells [match_K(v) = true] into
     exactly this destructure form so the checker CAN see through it. Unraveling
     such a destructure is both needless and harmful -- it splits the destructure
     off from any companion guard into a separate chain rule, undoing hoist's
     work ([$join_text] regressed YES -> MAYBE that way: its recursive clause's
     [text = cons(t-h2, t-t)] moved into a [crcu] consumer, leaving only the
     opaque [match-cons(text) = true] at the sibling overlap, so the CRC could no
     longer see the [len = bone] contradiction). So gate on the subject. *)
  let defined = Hashtbl.create 512 in
  List.iter (fun h -> Hashtbl.replace defined h ()) (defined_heads t);
  let is_defined h = Hashtbl.mem defined h in
  let rec key_of = function
    | Var v -> "#" ^ v
    | App (f, args) -> f ^ "(" ^ String.concat "," (List.map key_of args) ^ ")"
  in
  let seg_vars (guards, s, tp) =
    List.concat_map (fun (a, b) -> vars_of_term a @ vars_of_term b) guards
    @ vars_of_term s @ vars_of_term tp
  in
  let decompose (r : rule) =
    let bound = ref (vars_of_term r.lhs) in
    let segs = ref [] and cur = ref [] in
    List.iter
      (fun ((s, tp) as c) ->
        let fresh =
          List.filter (fun v -> not (List.mem v !bound)) (vars_of_term tp)
        in
        (* Unravel a binder only when (a) its pattern [tp] is a constructor with
           fresh variables -- a BARE-VARIABLE binder [s = v] is [fold]'s job (an
           unused [uses = 0] one it leaves behind forms only a trivial rhs=rhs
           self-pair, so keep it as a condition) -- AND (b) its subject [s] is a
           defined-function call, the only shape with a determinacy critical pair
           (see the header). A value destructure [v = K(..)] stays a condition. *)
        match (tp, s) with
        | App _, App (f, _) when fresh <> [] && is_defined f ->
            segs := (List.rev !cur, s, tp) :: !segs;
            cur := [];
            bound := !bound @ fresh
        | _ ->
            cur := c :: !cur;
            bound := !bound @ fresh)
      r.conds;
    (List.rev !segs, List.rev !cur)
  in
  let decomp = List.map (fun r -> (r, decompose r)) t.rules in
  let plain =
    List.filter_map
      (fun (r, (segs, _)) -> if segs = [] || r.owise then Some r else None)
      decomp
  in
  let work =
    List.filter_map
      (fun (r, (segs, tail)) ->
        if segs = [] || r.owise then None
        else Some (ref r.lhs, ref (vars_of_term r.lhs), segs, tail, r.rhs))
      decomp
  in
  if work = [] then t
  else
    let ids = Hashtbl.create 64 and next = ref 0 in
    let id_of key =
      match Hashtbl.find_opt ids key with
      | Some i -> i
      | None ->
          let i = !next in
          incr next;
          Hashtbl.replace ids key i;
          i
    in
    let emitted = ref [] in
    let emit r = if not (List.mem r !emitted) then emitted := r :: !emitted in
    let maxlvl =
      List.fold_left
        (fun m (_, _, segs, _, _) -> max m (List.length segs))
        0 work
    in
    for lvl = 0 to maxlvl - 1 do
      let groups = Hashtbl.create 16 and order = ref [] in
      List.iter
        (fun (chain, bnd, segs, tail, rhs) ->
          if lvl < List.length segs then (
            let guards, s, tp = List.nth segs lvl in
            let later =
              List.rev
                (snd
                   (List.fold_left
                      (fun (i, acc) x ->
                        (i + 1, if i > lvl then x :: acc else acc))
                      (0, []) segs))
            in
            let rest =
              List.concat_map seg_vars later
              @ vars_of_term rhs
              @ List.concat_map
                  (fun (a, b) -> vars_of_term a @ vars_of_term b)
                  tail
            in
            let carried = List.filter (fun v -> List.mem v rest) !bnd in
            let key = key_of !chain ^ "|" ^ key_of s in
            let cref, mref =
              match Hashtbl.find_opt groups key with
              | Some x -> x
              | None ->
                  let x = (ref [], ref []) in
                  Hashtbl.replace groups key x;
                  order := key :: !order;
                  x
            in
            cref :=
              !cref @ List.filter (fun v -> not (List.mem v !cref)) carried;
            mref := !mref @ [ (chain, bnd, guards, s, tp) ]))
        work;
      List.iter
        (fun key ->
          let cref, mref = Hashtbl.find groups key in
          let id = id_of key in
          let u = Printf.sprintf "crcu%d" id
          and kp = Printf.sprintf "crck%d" id in
          let keep = App (kp, List.map (fun v -> Var v) !cref) in
          List.iter
            (fun (chain, bnd, guards, s, tp) ->
              emit
                {
                  lhs = !chain;
                  rhs = App (u, [ s; keep ]);
                  conds = guards;
                  owise = false;
                };
              chain := App (u, [ tp; keep ]);
              bnd :=
                !bnd
                @ List.filter (fun v -> not (List.mem v !bnd)) (vars_of_term tp))
            !mref)
        (List.rev !order)
    done;
    List.iter
      (fun (chain, _, _, tail, rhs) ->
        emit { lhs = !chain; rhs; conds = tail; owise = false })
      work;
    of_rules (plain @ List.rev !emitted)

let crc_normalize (t : t) : t =
  t |> fold_premise_binders ~aggressive:true |> crc_unravel |> order_conds

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

(* -------------------------------------------------------------------------- *)
(* Maude lexical layer, shared by both Maude surfaces ({!To_maude} execution and
   {!To_mfe} analysis) so operator and variable identifiers get a single
   spelling. The order-sorted module emission itself lives in the [maude/]
   backends (which read the IL spec to recover sorts); this low layer owns only
   the lexical scrub. *)

(* A CTRS id ([A-Za-z0-9_$]+) to a Maude-safe id: [_] is a mixfix placeholder in
   Maude, so map it to [-] (injective, since CTRS ids never contain [-]). *)
let maude_id (s : string) : string =
  String.map (fun c -> if c = '_' then '-' else c) s

(* A CTRS variable name as a valid Maude variable identifier. A variable built
   from a pretty-printed pattern (a tuple bind ["(value, id)"], an angle-bracket
   type ["pair<K, V>"], a primed name) can carry characters Maude forbids in a
   variable -- spaces, parens, commas, dots, angle brackets. Names already
   confined to [A-Za-z0-9_] (the overwhelming majority) render exactly as the
   [maude_id] mangling; only the rest are run through {!sanitize} first to become
   well-formed (and stay distinct). *)
let maude_var (v : string) : string =
  let plain =
    String.for_all
      (function
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
      v
  in
  maude_id (if plain then v else sanitize v)
