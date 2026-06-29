(* Make the stateful gensym builtins ([$fresh_typeId] / p4-old's [$fresh_tid])
   pure by threading the issuing state through the CTRS.

   The interpreter implements these builtins as a global counter -- a nullary
   call that returns a different name each time, which no equation can express
   (and Maude's COUNTER is rule-only, unreachable from the substitution
   functions' equations). Here the state is the LAST ISSUED NAME (a text) and
   issuing appends a prime:

     $fresh_typeId(st) -> tuple(cat(st, "'"), cat(st, "'"))

   Seeded with "FRESH", the issued names are FRESH', FRESH'', ...: a P4
   identifier cannot contain a prime, so they collide neither with source names
   nor (strictly growing) with each other -- full gensym fidelity from a single
   unconditional rule, on both the analysis and the execution pipeline.

   Every symbol that transitively reaches a gensym (the "effectful" symbols)
   gets an extra state argument and returns a [tuple(result, state')]; inside
   its rules the state is threaded through the conditions in premise order.
   Each effectful call occurrence counts as one issuance -- the premise
   environment {!Simplify} reuses must keep such calls out of the equivalence
   classes (see {!gensym_ids}) so one instance is never duplicated into many. *)

module R = Rewrite_system
module T = Ctrs_term
module SS = Set.Make (String)

(* The gensym builtins the interpreter implements as a global counter
   ([$fresh_typeId] for p4, [$fresh_tid] for p4-old). The premise environment
   {!Simplify} reuses must keep calls to these opaque (out of the equivalence
   classes) so an issuance is never duplicated. *)
let gensym_ids = [ "fresh_typeId" (* p4 *); "fresh_tid" (* p4-old *) ]

let root_syms : string list =
  List.map (fun name -> T.func_sym Common.Source.(name $ no_region)) gensym_ids

let rec mentions (syms : SS.t) : R.term -> bool = function
  | R.Var _ -> false
  | R.App (f, args) -> SS.mem f syms || List.exists (mentions syms) args

let rule_mentions (syms : SS.t) (r : R.rule) : bool =
  mentions syms r.R.rhs
  || List.exists (fun (l, p) -> mentions syms l || mentions syms p) r.R.conds

let head_sym (r : R.rule) : string option =
  match r.R.lhs with R.App (f, _) -> Some f | R.Var _ -> None

(* The symbols whose evaluation (transitively) issues a gensym name: the
   fixpoint of "a defining rule mentions an effectful symbol", from the gensym
   roots. The same set on the pre- and post-threading system (threading keeps
   every mention), so later consumers can recompute it. *)
let effectful_set (sys : R.t) : SS.t =
  let rec grow s =
    let s' =
      List.fold_left
        (fun s r ->
          match head_sym r with
          | Some f when (not (SS.mem f s)) && rule_mentions s r -> SS.add f s
          | _ -> s)
        s sys.R.rules
    in
    if SS.cardinal s' = SS.cardinal s then s else grow s'
  in
  grow (SS.of_list root_syms)

let effectful_syms (sys : R.t) : string list = SS.elements (effectful_set sys)

(* The start-of-run state. Issued names extend it by primes, so the first
   issued name is FRESH' -- already impossible as a source identifier. *)
let seed_text = "FRESH"
let issued ~scalars (st : R.term) : R.term = T.cat_t st (T.text_t ~scalars "'")

let issue_rule ~scalars (sym : string) : R.rule =
  let st = R.Var "St0" in
  T.rule
    (R.App (sym, [ st ]))
    (T.tuple_t [ issued ~scalars st; issued ~scalars st ])

(* Thread one rule of an effectful symbol: [St<i>] are the state variables
   ([St0] the incoming state), [Sh<i>] the hoisted call results; a clash with
   the rule's own variables grows a [_] suffix. Conditions keep premise order,
   each hoisted call inserted before the condition (or the right-hand side)
   it was nested in. *)
let thread_rule (eff : SS.t) (r : R.rule) : R.rule =
  let taken = SS.of_list (R.vars_of_rule r) in
  let mk base i =
    let rec free s = if SS.mem s taken then free (s ^ "_") else s in
    free (Printf.sprintf "%s%d" base i)
  in
  let st i = R.Var (mk "St" i) in
  let fail what =
    failwith
      (Printf.sprintf "Gensym.thread: effectful call in %s of: %s" what
         (R.string_of_rule r))
  in
  (* Hoist every effectful call in value position out of [t] into a fresh
     condition, threading the state left-to-right, innermost first.
     [(cur, n, extra)]: the current state term, the next fresh index, the
     hoisted conditions (reversed). *)
  let rec hoist (cur, n, extra) t =
    match t with
    | R.Var _ -> ((cur, n, extra), t)
    | R.App (f, args) ->
        let (cur, n, extra), args =
          List.fold_left_map hoist (cur, n, extra) args
        in
        if SS.mem f eff then
          let v = R.Var (mk "Sh" n) and cur' = st n in
          let cond = (R.App (f, args @ [ cur ]), T.tuple_t [ v; cur' ]) in
          ((cur', n + 1, cond :: extra), v)
        else ((cur, n, extra), R.App (f, args))
  in
  let thread_cond (cur, n, conds) (l, p) =
    if mentions eff p then fail "a condition pattern";
    match l with
    | R.App (f, args) when SS.mem f eff ->
        (* The call is the whole condition subject: rewrite it in place, the
           pattern side gaining the state component. *)
        let (cur, n, extra), args =
          List.fold_left_map hoist (cur, n, []) args
        in
        let cur' = st n in
        let cond = (R.App (f, args @ [ cur ]), T.tuple_t [ p; cur' ]) in
        (cur', n + 1, (cond :: extra) @ conds)
    | _ ->
        let (cur, n, extra), l = hoist (cur, n, []) l in
        (cur, n, ((l, p) :: extra) @ conds)
  in
  match r.R.lhs with
  | R.App (f, args) when SS.mem f eff ->
      if List.exists (mentions eff) args then fail "the left-hand side";
      let st0 = st 0 in
      let lhs = R.App (f, args @ [ st0 ]) in
      let cur, n, rev_conds = List.fold_left thread_cond (st0, 1, []) r.conds in
      let (cur, _, extra), rhs = hoist (cur, n, []) r.rhs in
      {
        r with
        lhs;
        rhs = T.tuple_t [ rhs; cur ];
        conds = List.rev (extra @ rev_conds);
      }
  | _ ->
      (* The fixpoint puts every mentioning rule's head in [eff], so a pure
         rule cannot mention an effectful symbol (a [Var] head would). *)
      if rule_mentions eff r then fail "a rule of a pure symbol";
      r

let prime_code = Char.code '\''

(* The issuing rule introduces the prime byte; close the structural char
   equality over it (the analysis pipeline decides text equality bytewise, and
   {!To_ctrs.of_spec} only closed [eq] over the spec's own alphabet). On the
   [Native] path texts are [txt(..)]-wrapped Strings compared by the built-in
   [eq], so the per-byte [chr] equality never applies -- emit nothing. *)
let prime_eq_rules ~scalars (codes : int list) : R.rule list =
  match scalars with
  | T.Native -> []
  | T.Structural ->
      if List.mem prime_code codes then []
      else
        T.rule (T.eq_t (T.chr_t prime_code) (T.chr_t prime_code)) T.true_t
        :: List.concat_map
             (fun c ->
               [
                 T.rule (T.eq_t (T.chr_t prime_code) (T.chr_t c)) T.false_t;
                 T.rule (T.eq_t (T.chr_t c) (T.chr_t prime_code)) T.false_t;
               ])
             codes

let thread ~scalars (sys : R.t) : R.t =
  let used_roots =
    List.filter
      (fun root -> List.exists (rule_mentions (SS.singleton root)) sys.R.rules)
      root_syms
  in
  if used_roots = [] then sys
  else
    let eff = effectful_set sys in
    let rules =
      List.map (thread_rule eff) sys.R.rules
      @ List.map (issue_rule ~scalars) used_roots
      @ prime_eq_rules ~scalars (T.char_codes_of_rules sys.R.rules)
    in
    let vars = R.dedup_stable (List.concat_map R.vars_of_rule rules) in
    { R.vars; rules }
