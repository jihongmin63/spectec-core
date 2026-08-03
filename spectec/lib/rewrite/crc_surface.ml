(** Analysis-surface normalization passes for the Church-Rosser checker: fold
    premise-bound variables back into rules, restore binding order, and the
    opt-in [--crc-normalize] chain (aggressive inline + [crcu]/[crck]
    unraveling). All passes are CRC-facing and analysis-only; the execution
    surface ({!To_maude}) sees none of them. *)

open Rewrite_system

(* Substitute the variable [v] by [repl] throughout a term. *)
let rec subst_var (v : string) (repl : term) = function
  | Var u -> if u = v then repl else Var u
  | App (f, ts) -> App (f, List.map (subst_var v repl) ts)

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
     semantics-preserving). The binder must be used once (or be a plain [Var]
     alias) so a producer is never duplicated -- or DEAD, where there is nothing
     to duplicate and only the condition's free variable goes;
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
          (* A DEAD binder ([uses = 0]) is inlined nowhere -- the substitution
             below is a no-op and only the condition goes. What the condition
             was still asserting is that [prod] evaluates at all, and the
             [isStuckHead] guard the caller re-adds says exactly that; a bare
             variable pattern imposes nothing else, since it is spelled at the
             producer's own result sort ({!Maude_sorts.infer_var_sorts} types a
             condition's two sides alike, and a dead variable has no other
             occurrence to narrow it). Worth removing rather than leaving alone:
             a free variable on the pattern side is what turns the CRC's
             [=] -> [=>] re-encoding of the condition from a match into a
             search, and that class carries 98.6% of the measured CRC budget.

             [aggressive] (CRC-only) additionally drops the [uses = 1] cap: a
             deterministic producer may be duplicated because the CRC
             neither executes nor terminates the rules, only computes
             critical pairs -- and inlining a single-var binder removes
             the determinacy critical pair the [prod = v] condition
             would otherwise raise. Still meaning-preserving (an
             equivalence), unlike an unraveling. *)
          if uses = 0 || aggressive || is_alias || uses = 1 then Some (v, prod)
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

(* Unravel every binding condition [s = t] (t a pattern introducing fresh
   variables) into a fresh [crcu]/[crck] chain, moving the
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
(* Turn an existence guard [isStuckHead(s) = false] back into the binding
   condition [s = <fresh>] it replaced, so {!crc_unravel} has something to move
   into a chain operator's lhs. The two say the same thing -- [s] reduces to a
   value -- but only the binding form puts [s] in a position the unravel can
   reach, and the guard form leaves the CRC an opaque predicate no rule defines.

   This exists because the two ways of spending a dead binder are NOT
   interchangeable. {!fold_premise_binders} replaces it with this guard on the
   base surface, which is right there (it costs no free variable and every
   checker sees it); the unravel moves the subject into a pattern instead, which
   is what closed [$write_bits_from_value]. Doing the first blocks the second,
   so the normalize chain undoes it -- AFTER the aggressive inline, which would
   otherwise just put the guard straight back. *)
let rebind_stuck_guards (t : t) : t =
  let defined = Hashtbl.create 512 in
  List.iter (fun h -> Hashtbl.replace defined h ()) (defined_heads t);
  let next = ref 0 in
  let rebind (r : rule) : rule =
    let conds =
      List.map
        (fun ((s, tp) as c) ->
          match (s, tp) with
          | App ("isStuckHead", [ (App (f, _) as subject) ]), App ("false", [])
            when Hashtbl.mem defined f ->
              incr next;
              (subject, Var (Printf.sprintf "crcv%d" !next))
          | _ -> c)
        r.conds
    in
    { r with conds }
  in
  of_rules (List.map rebind t.rules)

let crc_unravel ?(bare_binders = false) (t : t) : t =
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
        (* Unravel a binder when its subject [s] is a defined-function call --
           the only shape with a determinacy critical pair (see the header) --
           and its pattern [tp] introduces a fresh variable. A value destructure
           [v = K(..)] stays a condition.

           A BARE-VARIABLE pattern is the class whose free variable turns the
           CRC's [=] -> [=>] re-encoding from a match into a search, and it
           carries 98.6% of the measured CRC budget -- but taking it costs a
           chain step that overlaps every sibling reaching the same point, and
           measured that trade goes both ways ([$write_bits_from_value] needs
           it, [$write_value_from_bits] loses its verdict to it). So it is a
           separate rung of {!normalize_ladder}, not a blanket. *)
        match (tp, s) with
        | Var _, App (f, _) when fresh <> [] && is_defined f && bare_binders ->
            segs := (List.rev !cur, s, tp) :: !segs;
            cur := [];
            bound := !bound @ fresh
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

(* Weak left-linearity, the syntactic premise under which an unraveling is
   joinability-sound and a normalized YES therefore transfers back to the
   original system (Gmeiner-Nishida-Gramlich IWC 2013, Thm 9, for an oriented
   DCTRS unraveled in [U_conf] shape -- which is how {!crc_unravel} keys its
   chain operators). A rule is weakly left-linear when every variable occurring
   twice or more in its MATCHING slots -- the lhs and every condition's pattern
   -- occurs nowhere in its PRODUCING slots, the rhs and every condition's
   evaluated side. Reduction-soundness does NOT imply joinability-soundness
   (same paper, Ex. 3), so this is not a formality: without it, the upgrade has
   no theorem behind it.

   {!Rewrite_system.orient_conds} establishes the (evaluated, pattern) split, so
   each slot reads straight off the pair; the residue where BOTH sides are calls
   has no canonical orientation, and counting such a condition on both slots can
   only over-report a violation, which is the safe direction here. *)
let wll_violations (t : t) : (string * string) list =
  let defined = Hashtbl.create 512 in
  List.iter (fun h -> Hashtbl.replace defined h ()) (defined_heads t);
  let is_call = function
    | App (f, _) -> Hashtbl.mem defined f
    | Var _ -> false
  in
  let of_rule (r : rule) =
    let matching, producing =
      List.fold_left
        (fun (m, p) (s, tp) ->
          if is_call s && is_call tp then (s :: tp :: m, s :: tp :: p)
          else (tp :: m, s :: p))
        ([ r.lhs ], [ r.rhs ]) r.conds
    in
    let produced = List.concat_map vars_of_term producing in
    let matched = List.concat_map vars_of_term matching in
    let head = match defined_head r with Some h -> h | None -> "?" in
    (* [vars_of_term] keeps every occurrence, so a second sighting of a variable
       already seen is exactly the "occurs twice or more" test. *)
    let seen = Hashtbl.create 16 in
    List.filter
      (fun v ->
        let repeat = Hashtbl.mem seen v in
        Hashtbl.replace seen v ();
        repeat && List.mem v produced)
      matched
    |> dedup_stable
    |> List.map (fun v -> (head, v))
  in
  List.concat_map of_rule t.rules

(** One normalization a slice may be re-checked under. *)
type strategy =
  | Inline_only
  | Inline_and_unravel
  | Unravel_bare_binders
  | Rebind_and_unravel

let string_of_strategy = function
  | Inline_only -> "inline"
  | Inline_and_unravel -> "unravel"
  | Unravel_bare_binders -> "unravel-bare"
  | Rebind_and_unravel -> "rebind"

(* What the weak-left-linearity check buys is the RIGHT to unravel, not a
   cheaper substitute for inlining. {!fold_premise_binders} [~aggressive] is an
   EQUIVALENCE, so it needs no premise and is all a slice gets when
   {!wll_violations} is non-empty; {!crc_unravel} then reaches the binders
   inlining cannot (a fresh variable the rule uses more than once still has to
   be BOUND somewhere) but only REFLECTS confluence, hence the gate.

   Beyond that gate, WHICH normalization wins is not predictable and not
   uniform, so a WLL-clean slice gets a LADDER rather than a choice: the
   verdicts are upgrade-only, and each rung independently justifies its own
   upgrade, so trying the next rung on a still-inconclusive slice is sound and
   can only add YESes. Measured, on the slices that discriminate:

   | rung                    | $write_bits | $write_value_*' | $write_value_from_bits |
   | inline + unravel        | TIMEOUT     | YES             | YES                    |
   | + bare binders          | TIMEOUT     | YES             | TIMEOUT                |
   | + rebind                | YES         | MAYBE           | -                      |

   No rung dominates, and three attempts to predict a winner from cheaper
   evidence all failed: the 2026-07-24 note's static proxies (condition counts,
   nested pairs) rank unravel-alone first and it loses all five [$write_value*]
   upgrades; taking bare-variable binders is what reaches
   [$write_bits_from_value] and it costs [$write_value_from_bits]; rebinding is
   what finally closes [$write_bits_from_value] and it costs the primed ones.
   Rank with proxies, decide with the checker.

   Rung 0 is therefore the chain as it stood before any of this, so no slice can
   lose a verdict it already had, and every later rung is a pure addition. *)
let normalize_ladder (t : t) : strategy list =
  if wll_violations t = [] then
    [ Inline_and_unravel; Unravel_bare_binders; Rebind_and_unravel ]
  else [ Inline_only ]

let select_strategy (t : t) : strategy = List.hd (normalize_ladder t)

let crc_normalize ?strategy (t : t) : t =
  let strategy =
    match strategy with Some s -> s | None -> select_strategy t
  in
  let inlined = fold_premise_binders ~aggressive:true t in
  match strategy with
  | Inline_only -> inlined |> order_conds
  | Inline_and_unravel -> inlined |> crc_unravel |> order_conds
  | Unravel_bare_binders ->
      inlined |> crc_unravel ~bare_binders:true |> order_conds
  | Rebind_and_unravel ->
      inlined |> rebind_stuck_guards
      |> crc_unravel ~bare_binders:true
      |> order_conds
