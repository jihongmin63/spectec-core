(* Unit tests for the pure analysis-tool plumbing: the unraveling
   ({!Rewrite.Unravel}), the upgrade-only verdict transfer
   ({!Rewrite.Mfe.upgrade}), the AProVE stop condition ({!Rewrite.Aprove.proof_printed}). Running the
   tools themselves is left to the CLI cram tests and the calibration runs. *)

module R = Rewrite.Rewrite_system
module Crc = Rewrite.Crc_surface
module U = Rewrite.Unravel
module Mfe = Rewrite.Mfe
module Aprove = Rewrite.Aprove
module Subproc = Rewrite.Subproc
module Itrs = Rewrite.Itrs

let failures = ref 0

let check name cond =
  if not cond then (
    incr failures;
    Printf.eprintf "FAIL %s\n" name)

let check_str name (actual : string) (expected : string) =
  if actual <> expected then (
    incr failures;
    Printf.eprintf "FAIL %s\n  expected: %S\n  actual:   %S\n" name expected
      actual)

let rule ?(owise = false) ?(conds = []) lhs rhs : R.rule =
  { R.lhs; rhs; conds; owise }

let system rules : R.t =
  { R.rules; vars = R.dedup_stable (List.concat_map R.vars_of_rule rules) }

let v x = R.Var x
let app f args = R.App (f, args)

(* ------------------------------------------------------------------------- *)
(* Unravel. *)

let trs_exn sys =
  match U.trs_of_system sys with
  | Ok (trs, stats) -> (trs, stats)
  | Error msg -> failwith ("unexpected unravel error: " ^ msg)

let () =
  (* An unconditional rule passes through; [$] spells as the [d_] prefix. *)
  let trs, stats = trs_exn (system [ rule (app "$f" [ v "x" ]) (v "x") ]) in
  check_str "unravel/unconditional" trs "(VAR x)\n(RULES\n  d_f(x) -> x\n)\n";
  check "unravel/unconditional stats"
    (stats = { U.eqs = 1; rules = 1; chain_steps = 0; vars = 1; owise = 0 })

let () =
  (* One condition becomes a chain step; the helper carries the lhs variables
     flat, as direct arguments. *)
  let trs, stats =
    trs_exn
      (system
         [
           rule
             ~conds:[ (app "$g" [ v "x" ], v "y") ]
             (app "$f" [ v "x" ])
             (v "y");
         ])
  in
  check_str "unravel/one-cond" trs
    "(VAR x y)\n(RULES\n  d_f(x) -> u_1(d_g(x), x)\n  u_1(y, x) -> y\n)\n";
  check "unravel/one-cond stats"
    (stats = { U.eqs = 1; rules = 2; chain_steps = 1; vars = 2; owise = 0 })

let () =
  (* Conditions are re-ordered to binding order (the guard inspects [b] before
     the condition that binds it), and a later chain step carries the variables
     the earlier condition bound. *)
  let trs, _ =
    trs_exn
      (system
         [
           rule
             ~conds:
               [
                 (app "isStuckHead" [ v "b" ], app "false" []);
                 (app "$g" [ v "x" ], app "cons" [ v "b"; v "t" ]);
               ]
             (app "$f" [ v "x" ])
             (app "$h" [ v "b" ]);
         ])
  in
  check_str "unravel/reorder-and-carry" trs
    "(VAR b t x)\n\
     (RULES\n\
    \  d_f(x) -> u_1(d_g(x), x)\n\
    \  u_1(cons(b, t), x) -> u_2(isStuckHead(b), x, b, t)\n\
    \  u_2(false, x, b, t) -> d_h(b)\n\
     )\n"

let () =
  (* An owise rule is included with the attribute dropped (over-approximation,
     the sound direction) and counted. *)
  let _, stats =
    trs_exn
      (system
         [
           rule (app "$f" [ app "nil" [] ]) (app "true" []);
           rule ~owise:true (app "$f" [ v "x" ]) (app "false" []);
         ])
  in
  check "unravel/owise-included"
    (stats = { U.eqs = 2; rules = 2; chain_steps = 0; vars = 1; owise = 1 })

let () =
  (* A rhs variable the lhs does not bind is a malformed TRS. *)
  (match U.trs_of_system (system [ rule (app "$f" [ v "x" ]) (v "y") ]) with
  | Error msg -> check "unravel/extravar" (String.length msg > 0)
  | Ok _ -> check "unravel/extravar" false);
  (* A condition cycle nothing can bind is an error, not a silent skip. *)
  (match
     U.trs_of_system
       (system
          [
            rule
              ~conds:
                [ (app "$g" [ v "b" ], v "c"); (app "$h" [ v "c" ], v "b") ]
              (app "$f" [ v "x" ])
              (v "x");
          ])
   with
  | Error _ -> check "unravel/cycle" true
  | Ok _ -> check "unravel/cycle" false);
  (* Two distinct source identifiers may not collide after the TPDB scrub. *)
  match
    U.trs_of_system
      (system
         [
           rule (app "$foo" [ v "x" ]) (v "x");
           rule (app "d_foo" [ v "x" ]) (v "x");
         ])
  with
  | Error _ -> check "unravel/collision" true
  | Ok _ -> check "unravel/collision" false

let () =
  (* A variable is rule-local, so two distinct source variables that scrub to
     one TPDB spelling are renamed apart, not merged -- contrast the symbol
     collision above, where merging two functions would alias their rules and
     must stay an error. Regression: the full typing/instantiation slices carry
     both [x'] and [_x'], which the scrub ('_' -> "", '\'' -> "prime") collapses
     to [x_prime]; the giants used to fail TPDB export outright. Both variables
     must survive as distinct names (stats.vars = 2). *)
  match
    U.trs_of_system (system [ rule (app "$f" [ v "x'"; v "_x'" ]) (v "x'") ])
  with
  | Error _ -> check "unravel/var-collision" false
  | Ok (_, stats) -> check "unravel/var-collision" (stats.U.vars = 2)

let () =
  (* A non-plain variable name goes through the CTRS sanitize, matching the
     Maude surface's spelling of the same variable. *)
  let trs, _ = trs_exn (system [ rule (app "$f" [ v "a-b" ]) (v "a-b") ]) in
  check "unravel/sanitized-var" (String.length trs > 0);
  check_str "unravel/sanitized-var text" trs
    "(VAR a_minus_b)\n(RULES\n  d_f(a_minus_b) -> a_minus_b\n)\n"

(* ------------------------------------------------------------------------- *)
(* Itrs: which guards reach the constraint solver. *)

let itrs_exn ?(roots = []) sys =
  match Itrs.of_system ~roots sys with
  | Ok (text, _) -> text
  | Error msg -> failwith ("unexpected itrs error: " ^ msg)

let () =
  (* A nat guard is written [-- if n =/= 0] and arrives as the condition
     [eq(n, 0) = false]. Left as a chain step it hides the fact the recursion
     rests on, exactly as an unlifted [leq] would; the integer LITERAL is what
     makes it safe to read the generic [eq] as an integer comparison. *)
  let text =
    itrs_exn
      (system
         [
           rule
             ~conds:[ (app "eq" [ v "n"; app "bzero" [] ], app "false" []) ]
             (app "$f" [ v "n" ])
             (app "$f" [ app "sub" [ v "n"; app "bone" [] ] ]);
         ])
  in
  check "itrs/eq-literal-guard-lifted"
    (Subproc.contains text "d_f(itrsz1) -> d_f(sub(itrsz1, 1)) :|: itrsz1 != 0");
  check "itrs/eq-literal-guard-leaves-no-chain-step"
    (not (Subproc.contains text "u_1(eq("))

let () =
  (* Both operands variables: nothing says they are integers, so the guard stays
     a chain step over the structural [eq]. *)
  let text =
    itrs_exn
      (system
         [
           rule
             ~conds:[ (app "eq" [ v "x"; v "y" ], app "false" []) ]
             (app "$g" [ v "x"; v "y" ])
             (app "true" []);
         ])
  in
  check "itrs/eq-open-guard-kept"
    (Subproc.contains text "d_g(x, y) -> u_1(eq(x, y), x, y)")

let () =
  (* A slice ROOT inside the replaced layer keeps none of its own rules, so
     nothing left names it -- and the reachability prune would take its integer
     replacement with it, exporting a system that does not contain the symbol
     the proof is about. Naming the root is what keeps the replacement. *)
  let codec = system [ rule (app "$bitstr_to_int" [ v "w"; v "n" ]) (v "n") ] in
  let text = itrs_exn ~roots:[ "$bitstr_to_int" ] codec in
  check "itrs/replaced-root-keeps-its-definition"
    (Subproc.contains text "d_bitstr_to_int(w, n) -> itrs_dec1(pow(2, w), n)");
  check "itrs/unnamed-replaced-root-is-empty"
    (not (Subproc.contains (itrs_exn codec) "d_bitstr_to_int"))

let () =
  (* [blayer] retargets a CALL, so a b-layer head that is itself the root has
     no call site left to retarget; the same table written out as a rule is its
     definition. *)
  let text =
    itrs_exn ~roots:[ "bpow_nat" ]
      (system [ rule (app "bpow_nat" [ v "x"; v "y" ]) (v "x") ])
  in
  check "itrs/blayer-root-gets-a-bridge"
    (Subproc.contains text "bpow_nat(x, y) -> pow(x, y)");
  check "itrs/blayer-root-bridge-target-is-defined"
    (Subproc.contains text "pow(x, y) -> x * pow(x, y - 1)")

let () =
  (* A root the integer surface has no counterpart for is an error, not a
     proof about the empty remainder. *)
  match
    Itrs.of_system ~roots:[ "bcompare" ]
      (system [ rule (app "bcompare" [ v "x"; v "y" ]) (v "x") ])
  with
  | Error msg ->
      check "itrs/undefined-root-errors"
        (Subproc.contains msg "no integer definition: bcompare")
  | Ok _ -> check "itrs/undefined-root-errors" false

(* ------------------------------------------------------------------------- *)
(* sanitize: the spellings the CTRS symbol names are built from. *)

let () =
  (* Interior underscores round-trip (the join restores them) and every non-name
     character maps to its mnemonic. *)
  check_str "sanitize/interior" (R.sanitize "add_action_tbl") "add_action_tbl";
  check_str "sanitize/prime" (R.sanitize "callableId'") "callableId_prime";
  check_str "sanitize/mnemonic" (R.sanitize "a-b") "a_minus_b";
  (* An underscore is a name character, not a separator, so a leading, trailing
     or doubled one survives -- [$capture_avoiding_] and [$capture_avoiding] are
     two p4 functions of different arity and must not share a symbol. *)
  check_str "sanitize/trailing-underscore" (R.sanitize "exists_") "exists_";
  check_str "sanitize/leading-underscore" (R.sanitize "_BOOL") "_BOOL";
  check_str "sanitize/doubled-underscore" (R.sanitize "a__b") "a__b";
  check "sanitize/no-collision"
    (R.sanitize "capture_avoiding_" <> R.sanitize "capture_avoiding");
  (* A numeric lead still needs the prefix; an underscore lead does not, since
     both Maude and TPDB take it as an ordinary identifier character. *)
  check_str "sanitize/numeric-lead" (R.sanitize "0BOOL") "c_0BOOL"

(* Randomized properties, on a fixed seed so a failure is reproducible. A spec
   name that is already identifier-shaped must survive untouched -- that identity
   is what makes distinct declarations distinct symbols, and losing it on the
   underscore is what merged two p4 functions into one slice. *)
let () =
  let rnd = Random.State.make [| 20260803 |] in
  let pick s = s.[Random.State.int rnd (String.length s)] in
  let lead = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_" in
  let body = lead ^ "0123456789" in
  let name () =
    String.init
      (1 + Random.State.int rnd 12)
      (fun i -> if i = 0 then pick lead else pick body)
  in
  let names = List.init 2000 (fun _ -> name ()) in
  check "sanitize/identifier-shaped-is-identity"
    (List.for_all (fun n -> R.sanitize n = n) names);
  let module S = Set.Make (String) in
  let distinct = S.of_list names in
  check "sanitize/injective-on-identifiers"
    (S.cardinal (S.of_list (List.map R.sanitize names)) = S.cardinal distinct);
  (* Over the full alphabet the scrub is lossy by design (mnemonics), but it must
     be stable: scrubbing a scrubbed name changes nothing. *)
  let noisy () =
    String.init
      (1 + Random.State.int rnd 12)
      (fun _ -> pick (body ^ "-+*/<>=!?&|^%.,;:#$@()[]{}'` \""))
  in
  check "sanitize/idempotent"
    (List.for_all
       (fun n ->
         let s = R.sanitize n in
         R.sanitize s = s)
       (List.init 2000 (fun _ -> noisy ())))

(* ------------------------------------------------------------------------- *)
(* orient_conds: a defined symbol belongs on a condition's evaluated side. *)

(* [$f] is defined by the first rule, so it counts as a call; the conditions
   under test sit on the last rule. *)
let def_f = rule (app "$f" [ v "x" ]) (v "x")

let orient_last (rules : R.rule list) : R.cond list =
  let out = (R.orient_conds (system rules)).R.rules in
  (List.nth out (List.length out - 1)).R.conds

let subject conds = rule ~conds (app "$h" [ v "a" ]) (v "a")

let () =
  (* Inverted (the shape a source premise [-- if eps = $f(x)] emits): the call
     sits in the pattern slot and is flipped back. *)
  check "orient/flips-inverted"
    (orient_last [ def_f; subject [ (app "none" [], app "$f" [ v "a" ]) ] ]
    = [ (app "$f" [ v "a" ], app "none" []) ]);
  (* Already canonical: the pass is the identity, so a well-formed system is
     untouched and re-running it changes nothing. *)
  check "orient/keeps-canonical"
    (orient_last [ def_f; subject [ (app "$f" [ v "a" ], app "none" []) ] ]
    = [ (app "$f" [ v "a" ], app "none" []) ]);
  (* A call on both sides has no better orientation to pick. *)
  check "orient/leaves-both-calls"
    (orient_last [ def_f; subject [ (app "$f" [ v "a" ], app "$f" [ v "b" ]) ] ]
    = [ (app "$f" [ v "a" ], app "$f" [ v "b" ]) ]);
  (* A value destructure names no defined symbol either side. *)
  check "orient/leaves-destructure"
    (orient_last [ def_f; subject [ (v "a", app "cons" [ v "h"; v "t" ]) ] ]
    = [ (v "a", app "cons" [ v "h"; v "t" ]) ])

let () =
  (* Why the invariant earns its pass: {!Unravel} lifts the pattern side into a
     helper's lhs. Left inverted, that lhs is [u_1(d_f(a), ..)] while the
     producer supplies [u_1(none, ..)] -- the two never match, so the chain and
     every recursion behind it go dead, and the termination prover sees a
     system it was never asked about. *)
  let sys = system [ def_f; subject [ (app "none" [], app "$f" [ v "a" ]) ] ] in
  let broken, _ = trs_exn sys in
  check_str "orient/unravel-chain-severed-when-inverted" broken
    "(VAR a x)\n\
     (RULES\n\
    \  d_f(x) -> x\n\
    \  d_h(a) -> u_1(none, a)\n\
    \  u_1(d_f(a), a) -> a\n\
     )\n";
  let fixed, _ = trs_exn (R.orient_conds sys) in
  check_str "orient/unravel-chain-connected-when-oriented" fixed
    "(VAR a x)\n\
     (RULES\n\
    \  d_f(x) -> x\n\
    \  d_h(a) -> u_1(d_f(a), a)\n\
    \  u_1(none, a) -> a\n\
     )\n"

(* ------------------------------------------------------------------------- *)
(* Crc_surface: weak left-linearity, and the normalization it selects. *)

let () =
  let heads (t : R.t) = List.map fst (Crc.wll_violations t) in
  let vars (t : R.t) = List.map snd (Crc.wll_violations t) in
  (* Which heads are DEFINED decides both what counts as a call and what
     {!Crc.crc_unravel} will chain, so the auxiliaries need defining rules. *)
  let defs =
    [
      rule (app "d_f" [ v "z" ]) (app "k" []);
      rule (app "d_h" [ v "z" ]) (app "k" []);
    ]
  in
  let system rules = system (rules @ defs) in
  (* [a] is matched twice (lhs and the condition's pattern) but produced
     nowhere: linear enough for the unraveling's soundness premise. *)
  let clean =
    system
      [
        rule
          ~conds:[ (app "d_f" [ v "b" ], v "a") ]
          (app "d_g" [ v "a"; v "b" ])
          (app "k" []);
      ]
  in
  check "wll/matched-twice-not-produced" (Crc.wll_violations clean = []);
  check "wll/clean-allows-unravel"
    (Crc.select_strategy clean = Crc.Inline_and_unravel);
  (* The same [a] now also produced (in the rhs): a violation, named by the
     rule's defining head. *)
  let dirty =
    system
      [
        rule
          ~conds:[ (app "d_f" [ v "b" ], v "a") ]
          (app "d_g" [ v "a"; v "b" ])
          (app "c" [ v "a" ]);
      ]
  in
  check "wll/matched-twice-and-produced" (vars dirty = [ "a" ]);
  check "wll/violation-names-head" (heads dirty = [ "d_g" ]);
  check "wll/dirty-selects-inline" (Crc.select_strategy dirty = Crc.Inline_only);
  (* Matched ONCE and produced is the ordinary shape of every rule -- WLL
     constrains only repeated matches. *)
  let linear = system [ rule (app "d_g" [ v "a" ]) (app "c" [ v "a" ]) ] in
  check "wll/matched-once-and-produced" (Crc.wll_violations linear = []);
  (* Calls on both sides have no canonical orientation. Read as printed this
     rule is clean ([a] matched once); read the other way [a] is matched twice
     and produced. Counting the condition on both slots reports it. *)
  let ambiguous =
    system
      [
        rule
          ~conds:[ (app "d_f" [ v "a" ], app "d_h" [ v "b" ]) ]
          (app "d_g" [ v "a" ])
          (app "k" []);
      ]
  in
  check "wll/both-sides-calls-over-report" (vars ambiguous = [ "a" ])

let () =
  (* The two normalizations differ in what they leave behind: unraveling binds
     inlining reaches a binder whose pattern is a single variable, and the
     unravel reaches the rest by splitting the rule at the binding point. *)
  let aux = rule (app "d_f" [ v "z" ]) (app "k" []) in
  let rules_of t = List.filter (fun (r : R.rule) -> r <> aux) t.R.rules in
  (* A single-variable binder: the inline substitutes it away, so the unravel
     arm has nothing left to do and the two agree. *)
  let bare =
    system
      [
        rule
          ~conds:[ (app "d_f" [ v "a" ], v "x") ]
          (app "d_g" [ v "a" ])
          (app "c" [ v "x"; v "x" ]);
        aux;
      ]
  in
  let inlined = rules_of (Crc.crc_normalize ~strategy:Crc.Inline_only bare) in
  check "crc_normalize/inline-removes-a-bare-binder" (List.length inlined = 1);
  (* The inline leaves the existence guard behind; the unravel arm rebinds it and
     moves the subject into a chain lhs, which is a position the guard form does
     not offer. *)
  check "crc_normalize/rebind-reopens-the-guard"
    (match
       rules_of (Crc.crc_normalize ~strategy:Crc.Rebind_and_unravel bare)
     with
    | [ a; b ] -> a.conds = [] && b.conds = []
    | _ -> false);
  (* Without the rebind there is nothing left for the unravel to move, which is
     the interaction the ladder exists to cover. *)
  check "crc_normalize/unravel-alone-cannot-reach-the-guard"
    (rules_of (Crc.crc_normalize ~strategy:Crc.Inline_and_unravel bare)
    = inlined);
  (* The first two ladder strategies produce this same system. The checker must
     see it once, in first-occurrence order, while the distinct rebind arm stays
     available as the next retry. *)
  check "mfe/normalization-candidates-deduplicate-equal-systems"
    (List.map fst (Mfe.normalized_candidates bare)
    = [ Crc.Inline_and_unravel; Crc.Rebind_and_unravel ]);
  (* The inline copies [d_f(a)] to both uses, and keeps the partiality guard the
     dropped binder was carrying. *)
  check "crc_normalize/inline-copies-per-use"
    (match inlined with
    | [
     {
       rhs = R.App ("c", [ p; q ]);
       conds = [ (R.App ("isStuckHead", _), _) ];
       _;
     };
    ] ->
        p = q && p = app "d_f" [ v "a" ]
    | _ -> false);
  (* The bare-binder gate, directly: a fresh single-variable pattern is left as
     a condition unless [~bare_binders] takes it. *)
  let one_bare =
    system
      [
        rule
          ~conds:[ (app "d_f" [ v "a" ], v "x") ]
          (app "d_g" [ v "a" ])
          (app "c" [ v "x" ]);
        aux;
      ]
  in
  check "crc_unravel/bare-binder-needs-the-flag"
    (Crc.crc_unravel one_bare = one_bare
    && List.length (Crc.crc_unravel ~bare_binders:true one_bare).R.rules = 3);
  (* A constructor-pattern binder is what inlining CANNOT reach -- the fields it
     introduces have to be bound somewhere -- so this is the shape the unravel
     arm exists for, and the one the WLL premise is spent on. *)
  let structured =
    system
      [
        rule
          ~conds:[ (app "d_f" [ v "a" ], app "tuple" [ v "x"; v "y" ]) ]
          (app "d_g" [ v "a" ])
          (app "c" [ v "x"; v "y" ]);
        aux;
      ]
  in
  let unravelled =
    rules_of (Crc.crc_normalize ~strategy:Crc.Inline_and_unravel structured)
  in
  check "crc_normalize/unravel-splits-at-the-binder"
    (List.length unravelled = 2
    && List.for_all (fun (r : R.rule) -> r.conds = []) unravelled);
  check "crc_normalize/inline-keeps-a-structured-binder"
    (match
       rules_of (Crc.crc_normalize ~strategy:Crc.Inline_only structured)
     with
    | [ { conds = [ (_, R.App ("tuple", _)) ]; _ } ] -> true
    | _ -> false)

(* ------------------------------------------------------------------------- *)
(* Mfe.upgrade: YES transfers up, nothing else ever changes. *)

let () =
  let up o n = Mfe.upgrade ~original:o ~normalized:n in
  check "upgrade/maybe+yes" (up Mfe.Maybe Mfe.Yes = Mfe.Yes);
  check "upgrade/timeout+yes" (up Mfe.Timeout Mfe.Yes = Mfe.Yes);
  check "upgrade/maybe+maybe" (up Mfe.Maybe Mfe.Maybe = Mfe.Maybe);
  check "upgrade/maybe+timeout" (up Mfe.Maybe Mfe.Timeout = Mfe.Maybe);
  check "upgrade/never-downgrade" (up Mfe.Maybe Mfe.No = Mfe.Maybe);
  check "upgrade/yes-stays" (up Mfe.Yes Mfe.Maybe = Mfe.Yes);
  check "upgrade/no-stays" (up Mfe.No Mfe.Yes = Mfe.No);
  check "upgrade/error-stays" (up (Mfe.Error "e") Mfe.Yes = Mfe.Error "e")

(* ------------------------------------------------------------------------- *)
(* Mfe.batch_checks_done: in a batched session, a symbols read buffer holds
   only that symbols output; its block is complete once the coherence check
   output is followed by the next MFE prompt. *)
let () =
  let d = Mfe.batch_checks_done in
  check "batch-done/complete"
    (d "MFE> Coherence checking of S1\nnon-overlapping ...\nMFE> ");
  check "batch-done/chc-running"
    (not (d "MFE> Coherence checking of S1\nAll critical pairs"));
  check "batch-done/crc-only"
    (not (d "MFE> Church-Rosser check for S1\nlocally-confluent.\nMFE> "))

(* Subproc session: send to a live childs stdin and read its echoed stdout
   without respawning (the batched-sweep primitive). Exercised with [cat]. *)
let () =
  let sess = Subproc.session_start ~cmd:[ "/bin/cat" ] () in
  (* Send and read share ONE deadline: a unit of work is the pair of them, and
     a bound on the read alone leaves the write unbounded. *)
  let deadline = Subproc.deadline_in 5 in
  check "session/sent" (Subproc.session_send sess "hello\n" ~deadline);
  let out, timed =
    Subproc.session_read sess
      ~done_when:(fun b -> Subproc.contains b "hello")
      ~deadline
  in
  check "session/echo" (Subproc.contains out "hello");
  check "session/echo-not-timed" (not timed);
  let deadline = Subproc.deadline_in 5 in
  check "session/sent-again" (Subproc.session_send sess "world\n" ~deadline);
  let out2, _ =
    Subproc.session_read sess
      ~done_when:(fun b -> Subproc.contains b "world")
      ~deadline
  in
  check "session/second-send" (Subproc.contains out2 "world");
  check "session/buffer-reset" (not (Subproc.contains out2 "hello"));
  Subproc.session_kill sess

(* Rewrite_system.slicer: repeated slicing shares one head index; slice_with
   must match the one-shot slice on every root set. *)
let () =
  let sys =
    system
      [
        rule (app "$f" [ v "x" ]) (app "$g" [ v "x" ]);
        rule (app "$g" [ v "x" ]) (v "x");
        rule (app "$h" [ v "x" ]) (v "x");
      ]
  in
  let sl = R.make_slicer sys in
  check "slicer/matches-f"
    (R.slice_with sl ~roots:[ "$f" ] = R.slice sys ~roots:[ "$f" ]);
  check "slicer/matches-h"
    (R.slice_with sl ~roots:[ "$h" ] = R.slice sys ~roots:[ "$h" ]);
  check "slicer/reachable-count"
    (List.length (R.slice_with sl ~roots:[ "$f" ]).R.rules = 2)

(* Subproc.timed: wraps a thunk, returning its result paired with the wall-clock
   seconds it took. The value passes through unchanged and the elapsed is a
   non-negative float that reflects real time spent (a sweep records it per
   symbol). *)
let () =
  let v, dt0 = Subproc.timed (fun () -> 42) in
  check "timed/value" (v = 42);
  check "timed/nonneg" (dt0 >= 0.0);
  let (), dt1 = Subproc.timed (fun () -> Unix.sleepf 0.05) in
  check "timed/measures-sleep" (dt1 >= 0.04)

(* An AProVE run as it comes off the wire: the verdict, then the echo, whose
   proof TREE runs from the "proof of" line to the first obligation separator.
   The tree's own "(9) YES" is the trap both scanners below exist to survive --
   it is one closed sub-problem, not the run's answer, and the run here is one
   AProVE gave up on. *)
let aprove_killed_run =
  "KILLED\n\
   proof of /tmp/spectec_aprove123.trs\n\
   # AProVE Commit ID: 192b3c9f\n\n\
   Termination w.r.t. Q of the given QTRS could not be shown:\n\n\
   (0) QTRS\n\
   (1) DependencyPairsProof [EQUIVALENT, 0 ms]\n\
   (2) AND\n\
  \    (3) QDP\n\
  \        (4) QDPSizeChangeProof [EQUIVALENT, 0 ms]\n\
  \        (5) YES\n\n\n\
   ----------------------------------------\n\n\
   (0)\n\
   Obligation:\n"

(* Aprove.proof_printed: the stop condition Subproc polls over a buffer that
   ends mid-read, so it must fire only once the tree is CLOSED -- the verdict
   line alone is not enough (the tree costs an instant and is the whole
   diagnostic), and a separator seen on a partial line is not a line at all. *)
let () =
  check "aprove/printed-closed-tree" (Aprove.proof_printed aprove_killed_run);
  check "aprove/pending-empty" (not (Aprove.proof_printed ""));
  (* A verdict with no echo behind it yet: the tree is still coming. *)
  check "aprove/pending-verdict-only"
    (not (Aprove.proof_printed "YES\nproof of /tmp/x.trs\n"));
  check "aprove/pending-open-tree"
    (not (Aprove.proof_printed "YES\nproof of /tmp/x.trs\n(0) QTRS\n(1) YES\n"));
  check "aprove/pending-narrative"
    (not (Aprove.proof_printed "Termination proof:\nDP problem\n"));
  (* The whole point of the terminated-line rule: a read that breaks right
     inside the separator is still a partial line. *)
  check "aprove/pending-partial-line"
    (not
       (Aprove.proof_printed
          "YES\nproof of /tmp/x.trs\n(0) QTRS\n------------------------"));
  (* A separator with no echo before it is some other tool's output, not a
     tree we can hand back. *)
  check "aprove/pending-separator-without-echo"
    (not
       (Aprove.proof_printed "YES\n----------------------------------------\n"))

(* Aprove.proof_tree: exactly the outline, with the obligations behind it and
   the temp-file path in front of it left out -- the path is a fresh name on
   every run and would make two identical proofs compare unequal. *)
let () =
  let tree = Aprove.proof_tree aprove_killed_run in
  check "tree/starts-at-commit-id"
    (String.starts_with ~prefix:"# AProVE Commit ID:" tree);
  check "tree/keeps-steps" (Subproc.contains tree "(1) DependencyPairsProof");
  check "tree/keeps-sub-verdict" (Subproc.contains tree "(5) YES");
  check "tree/drops-temp-path" (not (Subproc.contains tree "spectec_aprove123"));
  check "tree/drops-separator"
    (not (Subproc.contains tree "----------------------------------------"));
  check "tree/drops-obligations" (not (Subproc.contains tree "Obligation:"));
  check "tree/no-trailing-blank" (String.trim tree = tree);
  (* No echo at all, such as a parse error or a crashed JVM, is an empty tree. *)
  check "tree/no-echo-empty" (Aprove.proof_tree "MAYBE\n" = "")

let () =
  (* Where each emitted symbol came from. A chain helper is numbered by a
     counter over the whole rule list, so it is the one part of an unraveling
     that is not a function of its own rule -- and knowing the rule it belongs to
     is what lets a slice select it rather than renumber it. *)
  match
    U.export_of_system
      (system
         [
           rule
             ~conds:[ (app "$p" [ v "x" ], app "true" []) ]
             (app "$f" [ v "x" ])
             (v "x");
           rule (app "$p" [ v "x" ]) (app "true" []);
         ])
  with
  | Error msg -> check ("unravel/origins: " ^ msg) false
  | Ok (e, _) ->
      check "unravel/origins-name-the-defining-head"
        (List.assoc_opt "d_f" e.U.origins = Some "$f");
      check "unravel/origins-name-a-helper's-rule"
        (List.assoc_opt "u_1" e.U.origins = Some "$f")

let () =
  if !failures > 0 then (
    Printf.eprintf "%d test failure(s)\n" !failures;
    exit 1)
  else print_endline "test_rewrite: all tests passed"
