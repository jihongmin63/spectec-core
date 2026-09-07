type verdict = Yes | No | Maybe | Timeout | Degenerate | Error of string
type path = Whole_trs | Children | Integer_trs | Length_trs
type step = { path : path; verdict : verdict; secs : float; note : string }

type report = {
  verdict : verdict;
  stats : Unravel.stats option;
  secs : float option;
  proof : string option;
  steps : step list;
}

let string_of_verdict = function
  | Yes -> "YES"
  | No -> "NO"
  | Maybe -> "MAYBE"
  | Timeout -> "TIMEOUT"
  | Degenerate -> "DEGENERATE"
  | Error msg -> "ERROR: " ^ msg

let string_of_path = function
  | Whole_trs -> "whole"
  | Children -> "children"
  | Integer_trs -> "itrs"
  | Length_trs -> "itrs-lengths"

let string_of_step (s : step) : string =
  Printf.sprintf "%s:%s%s@%.1fs" (string_of_path s.path)
    (string_of_verdict s.verdict)
    (if s.note = "" then "" else "(" ^ s.note ^ ")")
    s.secs

let of_aprove : Aprove.verdict -> verdict = function
  | Aprove.Yes -> Yes
  | Aprove.No -> No
  | Aprove.Maybe -> Maybe
  | Aprove.Timeout -> Timeout
  | Aprove.Error msg -> Error msg

(* AProVE announces its verdict at its own deadline rather than when the proof
   lands (see {!Aprove.proof_printed}), so one run at a fixed budget costs
   that budget whatever the proof was worth -- and the seconds it reports
   measure our setting, not the difficulty. Climbing instead from a small
   budget and stopping at the first decisive answer records the SMALLEST budget
   that still answers, an upper bound on the real cost.

   Rungs grow by 4 so the total spent staying under an answer is about a third
   of the next rung: overshooting a proof's cost is cheap, and the ladder ends
   at [cap] so a search that runs out of rungs makes exactly the run a single
   [cap] call would have made.

   [from] raises the first rung for a region already measured to sit far above
   it. Climbing from 5 burns 5+20+80+320 = 425s before the 1280 rung even
   starts, which is pure waste over a stretch of slices that all answer near
   330s -- there [~from:1280] spends only the answering run. It is a
   measurement trade, not a free speedup: the ladder exists to record the
   SMALLEST budget that still answers, and AProVE announces at its own deadline
   for every symbol that does not finish early, so a rung raised above a
   symbol's real cost reports that rung instead of the cost. Raise it only
   where the region's difficulty is already known, and do not compare the
   seconds it produces against default-ladder rows. *)
let budget_ladder ?(from = 5) ~(cap : int) () : int list =
  let first = max 1 from in
  let rec go b acc =
    if b >= cap then List.rev (cap :: acc) else go (b * 4) (b :: acc)
  in
  if cap <= first then [ cap ] else go first []

(* Which verdicts end the climb. [Yes] and [No] are answers about the TRS and a
   larger budget cannot overturn them.

   Everything else is "ask again with more time", INCLUDING [Error]. A small
   budget is not just a MAYBE machine: on the $bitacc_offset_op TRS AProVE runs
   out at budget 5 having printed nothing but its narrative, so
   {!Aprove.check} reports "no YES/NO/MAYBE line" -- and the very same TRS
   answers YES at budget 20. Treating that as a permanent failure (it looks like
   one -- a crashed JVM reads identically) would skip the rung that answers and
   fall through to the cap. The one error that really is permanent, a missing
   binary, is checked once before the climb starts. *)
let decisive : Aprove.verdict -> bool = function
  | Aprove.Yes | Aprove.No -> true
  | Aprove.Maybe | Aprove.Timeout | Aprove.Error _ -> false

(* The system the proof is actually about.

   The analysis surface hides a binder behind a guard that no rule defines, and
   the TRS is where that costs a proof. {!Crc_surface.fold_premise_binders}
   replaces a DEAD binder [$g(A) = v] with [isStuckHead($g(A)) = false], which
   restates it faithfully only where [isStuckHead] is defined -- the execution
   module defines it (2,229 equations), neither analysis surface defines it at
   all. Unraveled, that guard becomes a consumer [u(false, ..) -> r] nothing can
   ever fire: the rule's right-hand side is unreachable and whatever recursion
   it carried is gone, so AProVE proves termination of a system with those rules
   silently deleted -- and a YES about that system is not a YES about this one.

   {!Crc_surface.rebind_stuck_guards} undoes that one step, restoring
   [$g(A) = <fresh>] -- the binder the fold removed. A restoration, not an
   approximation: the condition count is unchanged, so the helper count is too;
   what changes is that the consumer can fire. Both surfaces need it for the
   same reason (the integer surface used to skip it, which made its YES verdicts
   claims about the same silently-shortened system). *)
let proof_surface (system : Rewrite_system.t) : Rewrite_system.t =
  Crc_surface.rebind_stuck_guards system

let trs_of_slice (system : Rewrite_system.t) :
    (string * Unravel.stats, string) result =
  Unravel.trs_of_system (proof_surface system)

(* Which export the proof runs on. [Plain] is the TPDB TRS above; [Integer] is
   {!Itrs}, the same unraveling with the arithmetic layer handed to AProVE's
   native integers. They are separate SURFACES, not settings of one: their
   verdicts are about differently-encoded systems (both over-approximating the
   same CTRS, so a YES on either transfers), and the file extension has to
   follow, since AProVE reads the problem type off it. *)
type surface = Plain | Integer | Integer_lengths

let export ~(roots : string list) = function
  | Plain -> trs_of_slice
  | Integer -> fun system -> Itrs.of_system ~roots (proof_surface system)
  | Integer_lengths ->
      fun system -> Itrs.of_system ~roots ~norm:true (proof_surface system)

let extension = function
  | Plain -> ".trs"
  | Integer | Integer_lengths -> ".itrs"

(* How much longer than its own proof budget a capture run may spend PRINTING.
   AProVE's obligation echo reprints the submitted rule set once per
   dependency-graph child -- 2.9GB over 577 children on [Program_ok] -- and it
   keeps arriving for minutes after the verdict. A capture cut short here is not
   an error but it is short some children, which {!split} reports rather than
   splitting on the ones that made it. *)
let echo_slack = 900

(* The budget search: the ladder, stopping at the first rung that answers, and
   timing THAT rung alone -- the ones below it were cut off before AProVE had
   said anything worth keeping. A missing binary is an Error at every rung, so
   it skips straight to the one run whose message the caller reports. *)
let probe ?aprove_bin ~budget ~budget_from ~ext ~trs () : Aprove.run * float =
  let run b =
    Subproc.timed (fun () -> Aprove.check ?aprove_bin ~budget:b ~ext ~trs ())
  in
  let ladder =
    if Sys.file_exists (Aprove.resolve_bin aprove_bin) then
      budget_ladder ~from:budget_from ~cap:budget ()
    else [ budget ]
  in
  let rec climb = function
    | [] -> assert false
    | [ last ] -> run last
    | b :: rest ->
        let r, secs = run b in
        if decisive r.Aprove.verdict then (r, secs) else climb rest
  in
  climb ladder

(* ------------------------------------------------------------------------- *)
(* The child split.

   AProVE's [(4) AND] is its own sound modularization -- one QDP child per SCC
   of the dependency graph, all of them closed means the system is proved -- and
   a whole-system run shares ONE deadline across every child. What the tree
   calls open therefore mixes "hard" with "never reached", and on [Program_ok]
   the second is the larger half: of 75 children open after 3600s, 48 close
   standalone in a median 37s, by processors (the reduction-pair search) the
   shared run never completed once. Splitting does not buy more time -- it buys
   each child its OWN deadline, so a hopeless child burns only its own share
   instead of blocking the queue behind it.

   The verdict transfers by the soundness of the AND, not by an argument of
   ours: every open child closed, plus the closed ones the run already proved,
   is the proof the whole run was assembling. *)

type split_outcome = Closed | Unclosed of string | Unavailable of string

(* The children of one whole-system run, in tree order, with the export they are
   to be re-posed against. Separated from the running because two callers want
   the same children for different reasons: {!split} stops at the first that
   does not close (a checker has its answer there), while {!child_report} runs
   every one of them (a measurement wants the distribution). *)
let children ?aprove_bin ~budget ~(system : Rewrite_system.t) ~trs () :
    (Unravel.export * Qdp.problem list, string) result =
  (* The child carries OUR rules, not the ones the echo printed back: the same
     export the whole run was given, which is a superset of whatever the run had
     reduced them to by the time it reached this child, and a superset only adds
     chains. Re-derived here rather than threaded in, because it costs
     milliseconds against a run that costs the budget. *)
  match Unravel.export_of_system (proof_surface system) with
  | Error msg -> Error msg
  | Ok (export, _) -> (
      let run =
        Aprove.check ?aprove_bin ~budget ~ext:".trs"
          ~capture:{ Aprove.keep = Qdp.capture_filter (); echo_slack }
          ~trs ()
      in
      match Qdp.open_leaves run.Aprove.proof with
      | [] -> Error "no open child in the proof tree"
      | leaves ->
          let echoed = Qdp.problems run.Aprove.captured in
          let problem node =
            List.find_opt
              (fun (p : Qdp.problem) -> p.Qdp.node = node && p.Qdp.pairs <> [])
              echoed
          in
          let missing = List.filter (fun node -> problem node = None) leaves in
          if missing <> [] then
            (* Either the echo was cut short by [echo_slack] or the open node is
               not a dependency-pair problem at all. Splitting on the children
               that did arrive would prove a strict subset of what the tree
               needs. The node ids are what makes the next occurrence
               diagnosable: re-running [runme] by hand on the same export shows
               whether that node's obligation was printed at all. *)
            Error
              (Printf.sprintf
                 "node(s) %s not in the echo (%d open, %d obligations)"
                 (String.concat "," (List.map string_of_int missing))
                 (List.length leaves) (List.length echoed))
          else Ok (export, List.filter_map problem leaves))

let split ?aprove_bin ~budget ~(system : Rewrite_system.t) ~trs () :
    split_outcome =
  match children ?aprove_bin ~budget ~system ~trs () with
  | Error msg -> Unavailable msg
  | Ok (export, problems) ->
      let total = List.length problems in
      let rec close closed = function
        | [] -> Closed
        | (p : Qdp.problem) :: rest -> (
            match Qdp.standalone ~export p with
            | Error msg ->
                Unavailable (Printf.sprintf "node %d: %s" p.Qdp.node msg)
            | Ok (text, _) -> (
                let child =
                  Aprove.check ?aprove_bin ~budget ~ext:".qdp" ~trs:text ()
                in
                match child.Aprove.verdict with
                | Aprove.Yes -> close (closed + 1) rest
                | v ->
                    (* One child that does not close leaves the whole system
                       unproven, so the remaining children are not worth their
                       budget; the count says how far the split got. *)
                    Unclosed
                      (Printf.sprintf "%d/%d closed, node %d [%s] %s" closed
                         total p.Qdp.node
                         (String.concat "," (Qdp.pair_heads ~export p))
                         (Aprove.string_of_verdict v))))
      in
      close 0 problems

(* One row per open child, every one of them run: what {!split} answers with a
   verdict, measured instead. The distribution is the thing a residue is read
   from -- which children close, which recursion the ones that do not are around
   ([heads]), and what each cost -- and the early exit that makes {!split} a
   cheap checker is exactly what hides it.

   The budget is the child's own ([child_budget]), so a row says what the child
   costs when nothing else is competing for the deadline -- which is the
   comparison {!split} is built on. *)
type attempt = { verdict : verdict; secs : float; rules : int; proof : string }
type child = { node : int; heads : string list; pairs : int; qdp : attempt }

let child_report ?aprove_bin ?(budget = 300) ?child_budget
    ?(on_child = fun (_ : child) -> ()) (system : Rewrite_system.t) :
    (child list, string) result =
  let child_budget = Option.value child_budget ~default:budget in
  match export ~roots:[] Plain system with
  | Error msg -> Error msg
  | Ok (trs, _) -> (
      match children ?aprove_bin ~budget ~system ~trs () with
      | Error msg -> Error msg
      | Ok (export, problems) ->
          let attempt ~ext ~rules text =
            let run, secs =
              Subproc.timed (fun () ->
                  Aprove.check ?aprove_bin ~budget:child_budget ~ext ~trs:text
                    ())
            in
            {
              verdict = of_aprove run.Aprove.verdict;
              secs;
              rules;
              proof = run.Aprove.proof;
            }
          in
          Ok
            (List.map
               (fun (p : Qdp.problem) ->
                 let qdp =
                   match Qdp.standalone ~export p with
                   | Error msg ->
                       {
                         verdict = Error msg;
                         secs = 0.0;
                         rules = 0;
                         proof = "";
                       }
                   | Ok (text, rules) -> attempt ~ext:".qdp" ~rules text
                 in
                 let row =
                   {
                     node = p.Qdp.node;
                     heads = Qdp.pair_heads ~export p;
                     pairs = List.length p.Qdp.pairs;
                     qdp;
                   }
                 in
                 on_child row;
                 row)
               problems))

(* ------------------------------------------------------------------------- *)

let step_of path (run : Aprove.run) secs note =
  { path; verdict = of_aprove run.Aprove.verdict; secs; note }

(* The verdict of a check that took several paths: a [Yes] from ANY of them is
   the slice's, since each path proves the same slice through a different
   over-approximation. Failing that, the whole-system run's own verdict stands
   -- it is the one the column has always carried, and neither of the other
   paths refutes anything (an unclosed child says the split did not finish, not
   that the slice loops). [secs] and the proof tree follow the same choice. *)
let report_of ~stats ~(tree : string option) (steps : step list) : report =
  let chosen =
    match List.find_opt (fun (s : step) -> s.verdict = Yes) steps with
    | Some s -> Some s
    | None -> List.nth_opt steps 0
  in
  {
    verdict = (match chosen with Some s -> s.verdict | None -> Degenerate);
    stats = Some stats;
    secs = Option.map (fun (s : step) -> s.secs) chosen;
    proof = tree;
    steps;
  }

let check ?aprove_bin ?(budget = 300) ?(budget_from = 5)
    ?(split_children = true) ?(integer_retry = true) ?(surface = Plain)
    ~(roots : string list) (system : Rewrite_system.t) : report =
  let failed verdict =
    { verdict; stats = None; secs = None; proof = None; steps = [] }
  in
  if system.Rewrite_system.rules = [] then failed Degenerate
  else
    match export ~roots surface system with
    | Error msg -> failed (Error msg)
    | Ok (trs, stats) ->
        let tree_of (run : Aprove.run) =
          if run.Aprove.proof = "" then None else Some run.Aprove.proof
        in
        let whole, secs =
          probe ?aprove_bin ~budget ~budget_from ~ext:(extension surface) ~trs
            ()
        in
        let steps = [ step_of Whole_trs whole secs "" ] in
        (* The two fallback paths are about the plain surface's failure: the
           split reads the run that just happened, and the integer surface is
           the answer to arithmetic the plain one could not see. A check asked
           for one surface alone gets exactly that surface. *)
        if decisive whole.Aprove.verdict || surface <> Plain then
          report_of ~stats ~tree:(tree_of whole) steps
        else
          let steps =
            if not split_children then steps
            else
              let outcome, secs =
                Subproc.timed (fun () ->
                    split ?aprove_bin ~budget ~system ~trs ())
              in
              let verdict, note =
                match outcome with
                | Closed -> (Yes, "all children closed")
                | Unclosed note | Unavailable note ->
                    (of_aprove whole.Aprove.verdict, note)
              in
              steps @ [ { path = Children; verdict; secs; note } ]
          in
          if List.exists (fun (s : step) -> s.verdict = Yes) steps then
            report_of ~stats ~tree:(tree_of whole) steps
          else if not (integer_retry && Itrs.has_arithmetic system) then
            report_of ~stats ~tree:(tree_of whole) steps
          else
            (* Two integer surfaces, in order of how much of the slice they
               re-give: the arithmetic layer first, then the same export with
               the LIST layer lifted to lengths as well. The second is for the
               descent the first still cannot see -- a recursion that shortens a
               list -- and it applies to far fewer slices, so it is asked only
               where the first has already failed and only where the lift covers
               the slice at all (an export [Error] there means it does not). *)
            let rec climb steps tree = function
              | [] -> report_of ~stats ~tree steps
              | (path, surface) :: rest -> (
                  match export ~roots surface system with
                  | Error msg ->
                      climb
                        (steps
                        @ [
                            {
                              path;
                              verdict = Error msg;
                              secs = 0.0;
                              note = "export";
                            };
                          ])
                        tree rest
                  | Ok (text, _) ->
                      let run, secs =
                        probe ?aprove_bin ~budget ~budget_from
                          ~ext:(extension surface) ~trs:text ()
                      in
                      let steps = steps @ [ step_of path run secs "" ] in
                      if run.Aprove.verdict = Aprove.Yes then
                        report_of ~stats ~tree:(tree_of run) steps
                      else climb steps tree rest)
            in
            climb steps (tree_of whole)
              [ (Integer_trs, Integer); (Length_trs, Integer_lengths) ]
