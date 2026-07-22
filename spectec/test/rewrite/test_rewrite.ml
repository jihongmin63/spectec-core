(* Unit tests for the pure analysis-tool plumbing: the structure-preserving
   unraveling ({!Rewrite.Unravel}) and the SCC output classification
   ({!Rewrite.Scc}). Everything process-shaped (Maude, AProVE) is exercised by
   the CLI cram tests and the calibration runs instead. *)

module R = Rewrite.Rewrite_system
module U = Rewrite.Unravel
module Scc = Rewrite.Scc

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
  (* A non-plain variable name goes through the CTRS sanitize, matching the
     Maude surface's spelling of the same variable. *)
  let trs, _ = trs_exn (system [ rule (app "$f" [ v "a-b" ]) (v "a-b") ]) in
  check "unravel/sanitized-var" (String.length trs > 0);
  check_str "unravel/sanitized-var text" trs
    "(VAR a_minus_b)\n(RULES\n  d_f(a_minus_b) -> a_minus_b\n)\n"

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

let () =
  if !failures > 0 then (
    Printf.eprintf "%d test failure(s)\n" !failures;
    exit 1)
  else print_endline "test_rewrite: all tests passed"
