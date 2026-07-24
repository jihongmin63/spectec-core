(* Unit tests for the pure analysis-tool plumbing: the structure-preserving
   unraveling ({!Rewrite.Unravel}), the upgrade-only verdict transfer
   ({!Rewrite.Mfe.upgrade}), the SCC output classification ({!Rewrite.Scc}), and
   the AProVE stop condition ({!Rewrite.Aprove.verdict_printed}). Running the
   tools themselves is left to the CLI cram tests and the calibration runs. *)

module R = Rewrite.Rewrite_system
module U = Rewrite.Unravel
module Mfe = Rewrite.Mfe
module Aprove = Rewrite.Aprove
module Subproc = Rewrite.Subproc
module Scc = Rewrite.Scc
module Termination = Rewrite.Termination

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
  (* One condition becomes a u/k chain step; the keep-constructor carries the
     lhs arguments verbatim. *)
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
    "(VAR x y)\n\
     (RULES\n\
    \  d_f(x) -> u_1(d_g(x), k_1(x))\n\
    \  u_1(y, k_1(x)) -> y\n\
     )\n";
  check "unravel/one-cond stats"
    (stats = { U.eqs = 1; rules = 2; chain_steps = 1; vars = 2; owise = 0 })

let () =
  (* Conditions are re-ordered to binding order (the guard inspects [b] before
     the condition that binds it), and a later keep-constructor carries the
     variables the earlier condition bound. *)
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
    \  d_f(x) -> u_1(d_g(x), k_1(x))\n\
    \  u_1(cons(b, t), k_1(x)) -> u_2(isStuckHead(b), k_2(x, b, t))\n\
    \  u_2(false, k_2(x, b, t)) -> d_h(b)\n\
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
(* sanitize: the spellings the CTRS symbol names are built from. *)

let () =
  (* Interior underscores round-trip (the join restores them) and every non-name
     character maps to its mnemonic. *)
  check_str "sanitize/interior" (R.sanitize "add_action_tbl") "add_action_tbl";
  check_str "sanitize/prime" (R.sanitize "callableId'") "callableId_prime";
  check_str "sanitize/mnemonic" (R.sanitize "a-b") "a_minus_b";
  (* Known limitation, recorded so a fix has a failing case to flip: a leading,
     trailing or doubled underscore is dropped, so [$capture_avoiding_] and
     [$capture_avoiding] -- two p4 functions of different arity -- share one
     CTRS symbol and one slice. *)
  check_str "sanitize/trailing-underscore-dropped" (R.sanitize "exists_")
    "exists";
  check "sanitize/known-collision"
    (R.sanitize "capture_avoiding_" = R.sanitize "capture_avoiding")

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
    \  d_h(a) -> u_1(none, k_1(a))\n\
    \  u_1(d_f(a), k_1(a)) -> a\n\
     )\n";
  let fixed, _ = trs_exn (R.orient_conds sys) in
  check_str "orient/unravel-chain-connected-when-oriented" fixed
    "(VAR a x)\n\
     (RULES\n\
    \  d_f(x) -> x\n\
    \  d_h(a) -> u_1(d_f(a), k_1(a))\n\
    \  u_1(none, k_1(a)) -> a\n\
     )\n"

(* ------------------------------------------------------------------------- *)
(* Wll: the premise the --crc-normalize unravel upgrade needs. A variable
   repeated in the PATTERN basket (lhs + every condition pattern) may not appear
   in the RIGHT basket (rhs + every evaluated side). *)

module Wll = Rewrite.Wll

(* [$g] is the only defined symbol in these fixtures, so [$g(..)] is a call and
   every other application is a constructor. *)
let defined h = h = "$g"
let wll r = Wll.check_rule ~defined r

let classes (rep : Wll.rule_report) =
  List.map (fun (v : Wll.violation) -> v.Wll.cls) rep.Wll.violations

let offending (rep : Wll.rule_report) =
  R.dedup_stable
    (List.concat_map (fun (v : Wll.violation) -> v.Wll.vars) rep.Wll.violations)

let () =
  (* Twice in the pattern basket AND on the right: the violation. With no
     conditions at all, no re-orientation can reach it. *)
  let rep = wll (rule (app "$h" [ v "a"; v "a" ]) (v "a")) in
  check "wll/repeated-pattern-on-right" (classes rep = [ Wll.Blocked_rule ]);
  check "wll/repeated-pattern-on-right vars" (offending rep = [ "a" ]);
  (* Twice in the pattern basket but NOWHERE on the right: precisely what WLL
     permits -- a non-left-linear rule that is still weakly left-linear. *)
  check "wll/repeated-pattern-not-on-right"
    ((wll (rule (app "$h" [ v "a"; v "a" ]) (app "nil" []))).Wll.violations = []);
  (* Once in the pattern basket, on the right: the ordinary shape. *)
  check "wll/single-pattern-on-right"
    ((wll (rule (app "$h" [ v "a" ]) (v "a"))).Wll.violations = []);
  (* The condition pattern joins the PATTERN basket, the evaluated side the
     right one -- that is the whole reason the orientation matters. *)
  check "wll/condition-pattern-counts"
    (classes
       (wll
          (rule
             ~conds:[ (app "$g" [ v "b" ], v "a") ]
             (app "$h" [ v "a"; v "b" ])
             (v "a")))
    = [ Wll.Blocked_defined ])

let () =
  (* Class A: [a] is in the pattern basket twice (lhs, and the condition's
     pattern side) and on the right (rhs). Flipping the condition -- legal, as
     both sides are bound and neither is a call -- moves [a] out of the pattern
     basket and [b] into it, and [b] is not on the right. *)
  let r = rule ~conds:[ (v "b", v "a") ] (app "$h" [ v "a"; v "b" ]) (v "a") in
  let rep = wll r in
  check "wll/flippable" (classes rep = [ Wll.Flippable ]);
  check "wll/flippable orientation" (rep.Wll.orientation = Some [ true ]);
  (* The assignment must actually be a fix, not merely a label: re-check the
     rule with the conditions it names flipped. *)
  let flip (fl : bool list) (x : R.rule) : R.rule =
    {
      x with
      R.conds =
        List.map2 (fun f (s, p) -> if f then (p, s) else (s, p)) fl x.R.conds;
    }
  in
  let fixed = flip (Option.get rep.Wll.orientation) r in
  check "wll/flippable assignment really fixes it"
    ((wll fixed).Wll.violations = [])

let () =
  (* Evaluated side is a call: flipping would head the pattern with a defined
     symbol, which is what orient_conds exists to prevent. *)
  check "wll/blocked-defined"
    (classes
       (wll
          (rule
             ~conds:[ (app "$g" [ v "a" ], v "w") ]
             (app "$h" [ v "a"; v "w" ])
             (v "w")))
    = [ Wll.Blocked_defined ]);
  (* The offending variable sits on BOTH sides of one condition, so that
     condition feeds both baskets whichever way it faces. *)
  check "wll/blocked-bothsides"
    (classes
       (wll
          (rule
             ~conds:[ (v "a", app "cons" [ v "a"; v "b" ]) ]
             (app "$h" [ v "a"; v "b" ])
             (v "a")))
    = [ Wll.Blocked_bothsides ]);
  (* The condition binds [c], so its direction is forced by the binding order
     (flipped, the evaluated side would mention an unbound variable). *)
  check "wll/blocked-binder"
    (classes
       (wll
          (rule
             ~conds:[ (v "a", app "cons" [ v "b"; v "c" ]) ]
             (app "$h" [ v "a"; v "b" ])
             (v "b")))
    = [ Wll.Blocked_binder ]);
  (* A call on both sides: orient_conds already had no good orientation. *)
  check "wll/blocked-bothcall"
    (classes
       (wll
          (rule
             ~conds:[ (app "$g" [ v "a" ], app "$g" [ v "b" ]) ]
             (app "$h" [ v "a"; v "b" ])
             (v "b")))
    = [ Wll.Blocked_bothcall ])

let () =
  (* Past the exhaustive search bound the answer is NOT KNOWN, and says so --
     it is never quietly folded into "blocked". Thirteen free conditions, one
     of which causes the violation. *)
  let filler = List.init 12 (fun _ -> (v "c", v "d")) in
  let rep =
    wll
      (rule
         ~conds:((v "b", v "a") :: filler)
         (app "$h" [ v "a"; v "b"; v "c"; v "d" ])
         (v "a"))
  in
  check "wll/unknown-cap" (classes rep = [ Wll.Unknown_cap ]);
  check "wll/unknown-cap no orientation" (rep.Wll.orientation = None);
  (* One condition fewer and the same rule is searched to a verdict. *)
  let rep' =
    wll
      (rule
         ~conds:((v "b", v "a") :: List.tl filler)
         (app "$h" [ v "a"; v "b"; v "c"; v "d" ])
         (v "a"))
  in
  check "wll/just-under-cap" (classes rep' = [ Wll.Flippable ])

let () =
  (* The invariant a re-orientation pass rests on: flipping the free conditions
     of a rule whose conditions are ALREADY in binding order (what
     Crc_surface.order_conds leaves, and what every checked system has been
     through) leaves the greedy schedule picking the same conditions in the
     same order -- so the unravel chain keeps its shape.

     "Already in binding order" is load-bearing: the greedy takes the EARLIEST
     ready condition, so a flip can only make a LATER condition ready sooner,
     which changes nothing while every earlier one is ready too. *)
  let schedule_perm (lhs : R.term) (conds : R.cond list) : int list option =
    match U.schedule_conds (R.dedup_stable (R.vars_of_term lhs)) conds with
    | None -> None
    | Some sched ->
        let arr = Array.of_list conds in
        let claimed = Array.make (Array.length arr) false in
        Some
          (List.map
             (fun c ->
               let rec go i =
                 if (not claimed.(i)) && arr.(i) = c then (
                   claimed.(i) <- true;
                   i)
                 else go (i + 1)
               in
               go 0)
             sched)
  in
  let lhs = app "$h" [ v "a"; v "b"; v "c"; v "d" ] in
  let conds = [ (app "$g" [ v "c" ], v "e"); (v "b", v "a") ] in
  let r = rule ~conds lhs (v "a") in
  let rep = wll r in
  check "wll/invariant flippable" (rep.Wll.orientation = Some [ false; true ]);
  let flipped =
    List.map2
      (fun f (s, p) -> if f then (p, s) else (s, p))
      (Option.get rep.Wll.orientation)
      conds
  in
  check "wll/invariant schedule unchanged"
    (schedule_perm lhs conds = schedule_perm lhs flipped
    && schedule_perm lhs conds = Some [ 0; 1 ])

let () =
  (* check_system takes the call predicate from the system: [$g] is defined by
     a rule here, so the same condition that was Blocked_defined above stays
     so, and slice_verdict aggregates. *)
  let sys =
    system
      [
        rule (app "$g" [ v "x" ]) (v "x");
        rule
          ~conds:[ (app "$g" [ v "a" ], v "w") ]
          (app "$h" [ v "a"; v "w" ])
          (v "w");
      ]
  in
  let reps = Wll.check_system sys in
  check "wll/check_system indexes" (List.length reps = 2);
  check "wll/check_system clean rule" ((List.nth reps 0).Wll.violations = []);
  check "wll/check_system violating rule"
    (match (List.nth reps 1).Wll.violations with
    | [ v ] ->
        v.Wll.cls = Wll.Blocked_defined
        && v.Wll.rule_index = 1 && v.Wll.head = "$h"
    | _ -> false);
  check "wll/slice_verdict blocked" (Wll.slice_verdict reps = Wll.Blocked);
  check "wll/slice_verdict clean"
    (Wll.slice_verdict
       (Wll.check_system (system [ rule (app "$g" [ v "x" ]) (v "x") ]))
    = Wll.Clean)

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
(* Scc: output classification, calibrated phrases. *)

let mod_text =
  "(set include BOOL off .)\n\
   (set include BOOL-OPS off .)\n\
   (fmod SPEC is\n\
  \  sorts NatV Val .\n\
  \  op badd-carry : NatV NatV -> NatV [ctor] .\n\
  \  op wide : Val -> Val .\n\
  \  op over-list : List -> BoolV .\n\
   endfm)\n"

let () =
  let classify = Scc.classify ~module_text:mod_text in
  check "scc/no-ceta"
    (classify
       "Warning: The sufficient completeness checker is not fully available. \
        Please use the trust command to assume that module SPEC IS \
        SUFFICIENTLY COMPLETE."
    = Scc.No_ceta);
  check "scc/complete"
    (classify
       "Sufficient completeness check for SPEC\n\
        Completeness counter-examples: none were found\n\
        Freeness counter-examples: none were found\n\
        Analysis: it is complete and it is sound"
    = Scc.Complete);
  (match
     classify
       "Sufficient completeness check for SPEC\n\
        Completeness counter-examples: badd-carry(bzero,bzero) with sort NatV \
        Freeness counter-examples: none were found\n\
        Analysis: it is complete and it is sound"
   with
  | Scc.Counterexample { witness; sort; domain } ->
      check_str "scc/counterexample witness" witness "badd-carry(bzero,bzero) ";
      check_str "scc/counterexample sort" sort "NatV";
      check "scc/counterexample domain" (domain = Scc.Narrow)
  | _ -> check "scc/counterexample" false);
  (match
     classify
       "Completeness counter-examples: wide(bone) with sort Val Freeness \
        counter-examples: none were found"
   with
  | Scc.Counterexample { domain; _ } ->
      check "scc/domain val-wide" (domain = Scc.Val_wide)
  | _ -> check "scc/domain val-wide" false);
  (match
     classify
       "Completeness counter-examples: over-list(nil) with sort BoolV Freeness \
        counter-examples: none were found"
   with
  | Scc.Counterexample { domain; _ } ->
      check "scc/domain elem-erased" (domain = Scc.Elem_erased)
  | _ -> check "scc/domain elem-erased" false);
  (match classify "Warning: no parse for SPEC-term ." with
  | Scc.Error _ -> check "scc/no-parse" true
  | _ -> check "scc/no-parse" false);
  check "scc/timeout" (classify "half an output and then silence" = Scc.Timeout)

let () =
  check "scc/analysis"
    (Scc.analysis_of_output
       "blah\nAnalysis: it is complete and\nit is sound\nrest"
    = Some "complete+sound");
  check "scc/analysis-none" (Scc.analysis_of_output "no such line" = None)

let () =
  (* unconditional: a conditional rule is over-approximated (Approx); a plain
     system passes through untouched (Exact). *)
  let cond_sys =
    system
      [
        rule
          ~conds:[ (app "$g" [ v "x" ], app "true" []) ]
          (app "$f" [ v "x" ])
          (v "x");
      ]
  in
  let uncond, fid = Scc.unconditional cond_sys in
  check "scc/unconditional approx" (fid = Scc.Approx);
  check "scc/unconditional conds dropped"
    (List.for_all (fun (r : R.rule) -> r.conds = []) uncond.R.rules);
  let plain_sys = system [ rule (app "$f" [ v "x" ]) (v "x") ] in
  let same, fid = Scc.unconditional plain_sys in
  check "scc/unconditional exact" (fid = Scc.Exact && same = plain_sys)

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
  Subproc.session_send sess "hello\n";
  let out, timed =
    Subproc.session_read sess
      ~done_when:(fun b -> Subproc.contains b "hello")
      ~timeout:5
  in
  check "session/echo" (Subproc.contains out "hello");
  check "session/echo-not-timed" (not timed);
  Subproc.session_send sess "world\n";
  let out2, _ =
    Subproc.session_read sess
      ~done_when:(fun b -> Subproc.contains b "world")
      ~timeout:5
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

(* Aprove.verdict_printed: the stop condition Subproc polls over a buffer that
   ends mid-read, so it must fire on a terminated verdict line and on nothing
   else -- firing early ends the run on a verdict AProVE never gave. *)
let () =
  check "aprove/printed-yes" (Aprove.verdict_printed "some proof\nYES\n");
  check "aprove/printed-maybe" (Aprove.verdict_printed "MAYBE\nProof:\n");
  check "aprove/printed-crlf" (Aprove.verdict_printed "YES\r\n");
  check "aprove/pending-empty" (not (Aprove.verdict_printed ""));
  check "aprove/pending-narrative"
    (not (Aprove.verdict_printed "Termination proof:\nDP problem\n"));
  (* The whole point of the terminated-line rule: a read that breaks right
     after a verdict token is still a partial line. *)
  check "aprove/pending-partial-line" (not (Aprove.verdict_printed "YES"));
  check "aprove/pending-partial-prefix"
    (not (Aprove.verdict_printed "proof\nMAYBE"));
  (* ...and the same buffer one chunk later, where it was never a verdict. *)
  check "aprove/not-a-verdict-token"
    (not (Aprove.verdict_printed "proof\nMAYBE_SUCH_RULE\n"))

(* Termination.budget_ladder: the budgets a search tries, ascending. AProVE
   announces at its deadline, so a rung costs its own budget -- the ladder must
   start small, grow fast enough that overshooting stays cheap, and END AT THE
   CAP so a search that runs out of rungs is exactly the single capped run it
   replaces. *)
let () =
  check "ladder/default-cap"
    (Termination.budget_ladder ~cap:300 = [ 5; 20; 80; 300 ]);
  check "ladder/big-cap"
    (Termination.budget_ladder ~cap:1800 = [ 5; 20; 80; 320; 1280; 1800 ]);
  (* a cap that is already a rung is not repeated *)
  check "ladder/cap-on-a-rung"
    (Termination.budget_ladder ~cap:80 = [ 5; 20; 80 ]);
  (* ...and one that is not lands as the final rung, never overshooting it *)
  check "ladder/cap-off-a-rung"
    (Termination.budget_ladder ~cap:100 = [ 5; 20; 80; 100 ]);
  check "ladder/cap-below-first-rung" (Termination.budget_ladder ~cap:3 = [ 3 ]);
  check "ladder/last-rung-is-cap"
    (List.for_all
       (fun cap ->
         let l = Termination.budget_ladder ~cap in
         l <> [] && List.nth l (List.length l - 1) = cap)
       [ 1; 5; 6; 20; 21; 99; 300; 601; 1800 ]);
  check "ladder/ascending"
    (List.for_all
       (fun cap ->
         let l = Termination.budget_ladder ~cap in
         fst
           (List.fold_left
              (fun (ok, prev) b -> (ok && b > prev, b))
              (true, 0) l))
       [ 5; 20; 100; 300; 1800 ])

(* Termination.decisive: which rung verdict ends the climb. Only an answer
   about the TRS does. An Error must NOT: AProVE at a budget too small to
   finish prints no verdict line at all, which Aprove.check reports as an
   Error that reads exactly like a crashed run -- and the same TRS answers at
   the next rung. Stopping (or jumping to the cap) on Error skips the rung
   that would have answered. *)
let () =
  check "decisive/yes" (Termination.decisive Aprove.Yes);
  check "decisive/no" (Termination.decisive Aprove.No);
  check "decisive/maybe" (not (Termination.decisive Aprove.Maybe));
  check "decisive/timeout" (not (Termination.decisive Aprove.Timeout));
  check "decisive/no-verdict-line-keeps-climbing"
    (not
       (Termination.decisive
          (Aprove.Error "no YES/NO/MAYBE line in the AProVE output")));
  check "decisive/any-error-keeps-climbing"
    (not (Termination.decisive (Aprove.Error "anything at all")))

let () =
  if !failures > 0 then (
    Printf.eprintf "%d test failure(s)\n" !failures;
    exit 1)
  else print_endline "test_rewrite: all tests passed"
