type verdict = Yes | No | Maybe | Timeout | Degenerate | Error of string
type path = Whole_trs | Children

type rung = {
  budget : int;
  verdict : verdict;
  secs : float;
  total_secs : float;
}

type step = {
  path : path;
  verdict : verdict;
  secs : float;
  total_secs : float;
  rungs : rung list;
  note : string;
}

type report = {
  verdict : verdict;
  stats : Unravel.stats option;
  secs : float option;
  total_secs : float option;
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

let string_of_path = function Whole_trs -> "whole" | Children -> "children"

let string_of_step (s : step) : string =
  let base =
    Printf.sprintf "%s:%s%s@%.1fs" (string_of_path s.path)
      (string_of_verdict s.verdict)
      (if s.note = "" then "" else "(" ^ s.note ^ ")")
      s.secs
  in
  let rounded_tenth x = Float.round (x *. 10.0) in
  match s.rungs with
  | [] -> base
  | [ _ ] when rounded_tenth s.total_secs = rounded_tenth s.secs -> base
  | rungs ->
      let rung (r : rung) =
        Printf.sprintf "%d:%s@%.1fs" r.budget
          (string_of_verdict r.verdict)
          r.secs
      in
      Printf.sprintf "%s(total=%.1fs; rungs=%s)" base s.total_secs
        (String.concat "," (List.map rung rungs))

let step_of_rungs path (rungs : rung list) note : step =
  match List.rev rungs with
  | [] -> invalid_arg "Termination.step_of_rungs: empty ladder"
  | final :: _ ->
      {
        path;
        verdict = final.verdict;
        secs = final.secs;
        total_secs =
          List.fold_left (fun n (r : rung) -> n +. r.total_secs) 0.0 rungs;
        rungs;
        note;
      }

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

   The public termination command always starts at 5. *)
let budget_ladder ~(cap : int) () : int list =
  let first = 5 in
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
   what changes is that the consumer can fire. *)
let proof_surface (system : Rewrite_system.t) : Rewrite_system.t =
  Crc_surface.rebind_stuck_guards system

let trs_of_slice (system : Rewrite_system.t) :
    (string * Unravel.stats, string) result =
  Unravel.trs_of_system (proof_surface system)

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
let probe ?aprove_bin ?final_capture ~budget ~trs () :
    Aprove.run * float * rung list =
  let run ?capture b =
    let result =
      Aprove.check ?aprove_bin ?capture ~budget:b ~ext:".trs" ~trs ()
    in
    let rung =
      {
        budget = b;
        verdict = of_aprove result.verdict;
        secs = result.proof_secs;
        total_secs = result.total_secs;
      }
    in
    (result, result.proof_secs, rung)
  in
  let ladder =
    if Sys.file_exists (Aprove.resolve_bin aprove_bin) then
      budget_ladder ~cap:budget ()
    else [ budget ]
  in
  let rec climb attempted = function
    | [] -> assert false
    | [ last ] ->
        let result, secs, rung = run ?capture:final_capture last in
        (result, secs, List.rev (rung :: attempted))
    | b :: rest ->
        let result, secs, rung = run b in
        let attempted = rung :: attempted in
        if decisive result.Aprove.verdict then (result, secs, List.rev attempted)
        else climb attempted rest
  in
  climb [] ladder

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
type children_error = Missing_echo of string | Child_unavailable of string

let string_of_children_error = function
  | Missing_echo msg | Child_unavailable msg -> msg

(* The children of one whole-system run, in tree order, with the export they are
   to be re-posed against. Separated from the running because two callers want
   the same children for different reasons: {!split} stops at the first that
   does not close (a checker has its answer there), while {!child_report} runs
   every one of them (a measurement wants the distribution). *)
let children ~(system : Rewrite_system.t) (run : Aprove.run) :
    (Unravel.export * Qdp.problem list, children_error) result =
  (* The child carries OUR rules, not the ones the echo printed back: the same
     export the whole run was given, which is a superset of whatever the run had
     reduced them to by the time it reached this child, and a superset only adds
     chains. Re-derived here rather than threaded in, because it costs
     milliseconds against a run that costs the budget. *)
  match Unravel.export_of_system (proof_surface system) with
  | Error msg -> Error (Child_unavailable msg)
  | Ok (export, _) -> (
      match Qdp.open_leaves run.Aprove.proof with
      | [] -> Error (Child_unavailable "no open child in the proof tree")
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
              (Missing_echo
                 (Printf.sprintf
                    "node(s) %s not in the echo (%d open, %d obligations)"
                    (String.concat "," (List.map string_of_int missing))
                    (List.length leaves) (List.length echoed)))
          else Ok (export, List.filter_map problem leaves))

let capture_whole ?aprove_bin ~budget ~(trs : string) () : Aprove.run =
  Aprove.check ?aprove_bin ~budget ~ext:".trs"
    ~capture:
      {
        Aprove.keep = Qdp.capture_filter ();
        done_when = Qdp.capture_complete;
        echo_slack;
      }
    ~trs ()

let split ?aprove_bin ~budget ~(system : Rewrite_system.t) ~(trs : string)
    ~(whole : Aprove.run) () : split_outcome =
  let children =
    match children ~system whole with
    | Ok children -> Ok children
    | Error (Missing_echo _) ->
        (* Some AProVE runs reach EOF without echoing every obligation named by
           their own proof tree. The final-rung capture is then unusable: a
           split over only the obligations that arrived would prove a strict
           subset. Preserve the historical behavior with one dedicated retry;
           complete same-run captures still avoid it. *)
        capture_whole ?aprove_bin ~budget ~trs () |> children ~system
    | Error error -> Error error
  in
  match children with
  | Error error -> Unavailable (string_of_children_error error)
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

(* ------------------------------------------------------------------------- *)

let direct_step path verdict secs note =
  { path; verdict; secs; total_secs = secs; rungs = []; note }

(* A YES from the plain whole TRS or a complete sound child split proves
   the slice. Otherwise the whole-run verdict stands; an unclosed child only
   says the split did not finish. The time and tree follow the chosen path. *)
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
    total_secs =
      (match steps with
      | [] -> None
      | _ ->
          Some
            (List.fold_left (fun n (s : step) -> n +. s.total_secs) 0.0 steps));
    proof = tree;
    steps;
  }

let check ?aprove_bin ?(budget = 300) (system : Rewrite_system.t) : report =
  let failed verdict =
    {
      verdict;
      stats = None;
      secs = None;
      total_secs = None;
      proof = None;
      steps = [];
    }
  in
  if system.Rewrite_system.rules = [] then failed Degenerate
  else
    match trs_of_slice system with
    | Error msg -> failed (Error msg)
    | Ok (trs, stats) ->
        let tree_of (run : Aprove.run) =
          if run.Aprove.proof = "" then None else Some run.Aprove.proof
        in
        let final_capture =
          {
            Aprove.keep = Qdp.capture_filter ();
            done_when = Qdp.capture_complete;
            echo_slack;
          }
        in
        let whole, _, whole_rungs =
          probe ?aprove_bin ~final_capture ~budget ~trs ()
        in
        let steps = [ step_of_rungs Whole_trs whole_rungs "" ] in
        if decisive whole.Aprove.verdict then
          report_of ~stats ~tree:(tree_of whole) steps
        else
          let outcome, secs =
            Subproc.timed (fun () ->
                split ?aprove_bin ~budget ~system ~trs ~whole ())
          in
          let verdict, note =
            match outcome with
            | Closed -> (Yes, "all children closed")
            | Unclosed note | Unavailable note ->
                (of_aprove whole.Aprove.verdict, note)
          in
          report_of ~stats ~tree:(tree_of whole)
            (steps @ [ direct_step Children verdict secs note ])
