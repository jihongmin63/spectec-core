module R = Rewrite_system

type cls =
  | Flippable
  | Blocked_rule
  | Blocked_defined
  | Blocked_bothsides
  | Blocked_binder
  | Blocked_bothcall
  | Blocked_combination
  | Unknown_cap

let string_of_cls = function
  | Flippable -> "flippable"
  | Blocked_rule -> "blk-rule"
  | Blocked_defined -> "blk-defined"
  | Blocked_bothsides -> "blk-bothsides"
  | Blocked_binder -> "blk-binder"
  | Blocked_bothcall -> "blk-bothcall"
  | Blocked_combination -> "blk-combination"
  | Unknown_cap -> "cap"

(* Report order, and the TSV column order: class A first, then the blocking
   reasons from "the rule itself" outwards, then the not-known bucket. *)
let all_cls =
  [
    Flippable;
    Blocked_rule;
    Blocked_defined;
    Blocked_bothsides;
    Blocked_binder;
    Blocked_bothcall;
    Blocked_combination;
    Unknown_cap;
  ]

type violation = {
  rule_index : int;
  head : string;
  vars : string list;
  cls : cls;
}

type rule_report = {
  violations : violation list;
  orientation : bool list option;
  conds : int;
  free_conds : int;
  bothcall_conds : int;
}

type slice_verdict = Clean | Flippable_all | Partial | Blocked

let string_of_verdict = function
  | Clean -> "clean"
  | Flippable_all -> "flippable-all"
  | Partial -> "partial"
  | Blocked -> "blocked"

(* ------------------------------------------------------------------------- *)
(* Condition kinds.

   A condition may be re-oriented only when doing so keeps BOTH invariants the
   rest of the pipeline relies on: the greedy binding order stays valid (so
   {!Unravel.schedule_conds} picks the same conditions in the same order), and
   no defined symbol lands on a pattern side ({!Rewrite_system.orient_conds}).
   [Free] is exactly the conjunction: both sides fully bound already, neither a
   call. Every other kind pins the direction, and which one it is becomes the
   blocking reason a violation is reported with. *)

type kind =
  | Free
  | K_defined (* evaluated side is a call: flipping breaks orient_conds *)
  | K_inverted (* pattern side is a call: orient_conds should have flipped it *)
  | K_bothcall (* calls both sides: orient_conds had no good choice *)
  | K_binder (* binds a not-yet-bound variable: the order forces it *)

(* The exhaustive re-orientation search is [2^k] over a rule's free conditions.
   Beyond this the rule is reported as [Unknown_cap] -- not silently treated as
   blocked. *)
let search_cap = 12

let is_call ~(defined : string -> bool) = function
  | R.App (h, _) -> defined h
  | R.Var _ -> false

(* Each condition's kind, in the rule's own condition order. The bound set is
   developed along the BINDING order ({!Unravel.schedule_conds}, the single
   definition of the greedy), then mapped back onto the source positions: a
   condition is only "free" relative to what is bound when it runs. A rule
   whose conditions no schedule can order keeps its source order (the checker
   then over-reports [K_binder], the conservative direction). *)
let kinds_of_rule ~(defined : string -> bool) (r : R.rule) : kind array =
  let conds = Array.of_list r.conds in
  let n = Array.length conds in
  let kinds = Array.make n K_binder in
  let lhs_vars = R.dedup_stable (R.vars_of_term r.lhs) in
  let scheduled =
    match Unravel.schedule_conds lhs_vars r.conds with
    | Some cs -> cs
    | None -> r.conds
  in
  (* Recover each scheduled condition's source position. The schedule is a
     stable permutation of the same values, so the first not-yet-claimed equal
     condition is its own. *)
  let claimed = Array.make n false in
  let position (c : R.cond) : int option =
    let rec go i =
      if i >= n then None
      else if (not claimed.(i)) && conds.(i) = c then (
        claimed.(i) <- true;
        Some i)
      else go (i + 1)
    in
    go 0
  in
  let bound = ref lhs_vars in
  List.iter
    (fun ((s, pat) as c) ->
      let all_bound t =
        List.for_all (fun v -> List.mem v !bound) (R.vars_of_term t)
      in
      let pure = all_bound s && all_bound pat in
      let call_s = is_call ~defined s and call_p = is_call ~defined pat in
      let k =
        if call_s && call_p then K_bothcall
        else if not pure then K_binder
        else if call_s then K_defined
        else if call_p then K_inverted
        else Free
      in
      (match position c with Some i -> kinds.(i) <- k | None -> ());
      bound := !bound @ R.vars_of_term pat)
    scheduled;
  kinds

(* ------------------------------------------------------------------------- *)

let check_rule ?(index = 0) ~(defined : string -> bool) (r : R.rule) :
    rule_report =
  let head = Option.value (R.defined_head r) ~default:"?" in
  let conds = Array.of_list r.conds in
  let n = Array.length conds in
  let kinds = kinds_of_rule ~defined r in
  let indices = List.init n Fun.id in
  let free_idx = List.filter (fun i -> kinds.(i) = Free) indices in
  let k = List.length free_idx in
  let bothcall_conds =
    List.length (List.filter (fun i -> kinds.(i) = K_bothcall) indices)
  in
  let base =
    {
      violations = [];
      orientation = None;
      conds = n;
      free_conds = k;
      bothcall_conds;
    }
  in
  let ev i = fst conds.(i) and pat i = snd conds.(i) in
  (* The two baskets under an orientation. A flipped condition contributes its
     evaluated side to the pattern basket and its pattern side to the right
     one -- that IS what flipping means. *)
  let pat_count flipped v =
    List.fold_left
      (fun acc i -> acc + R.count_var v (if flipped i then ev i else pat i))
      (R.count_var v r.lhs) indices
  in
  let right_count flipped v =
    List.fold_left
      (fun acc i -> acc + R.count_var v (if flipped i then pat i else ev i))
      (R.count_var v r.rhs) indices
  in
  let violates flipped v =
    pat_count flipped v >= 2 && right_count flipped v >= 1
  in
  let as_is _ = false in
  let vars = R.dedup_stable (R.vars_of_rule r) in
  let offenders = List.filter (violates as_is) vars in
  if offenders = [] then base
  else
    (* A free condition that mentions the variable on BOTH sides contributes to
       both baskets whichever way it faces, so its lower bound is fixed too --
       counting it here (not only the pinned conditions) is what lets the cut
       below name [Blocked_bothsides] instead of falling through to a fruitless
       search ending in [Blocked_combination]. *)
    let pinned i = kinds.(i) <> Free in
    let straddles v i = R.count_var v (ev i) > 0 && R.count_var v (pat i) > 0 in
    let fixed_contrib v side_of =
      List.fold_left
        (fun acc i ->
          if pinned i then acc + R.count_var v (side_of i)
          else if straddles v i then
            acc + min (R.count_var v (ev i)) (R.count_var v (pat i))
          else acc)
        0 indices
    in
    let lb_pat v = R.count_var v r.lhs + fixed_contrib v pat in
    let lb_right v = R.count_var v r.rhs + fixed_contrib v ev in
    let stuck =
      List.filter (fun v -> lb_pat v >= 2 && lb_right v >= 1) offenders
    in
    let violation cls vars = { rule_index = index; head; vars; cls } in
    if stuck <> [] then
      (* Why this variable cannot be freed: the strongest reason among the
         conditions that mention it, "strongest" meaning the one a
         re-orientation pass would have to defeat first. *)
      let reason v =
        let touches i = R.count_var v (ev i) > 0 || R.count_var v (pat i) > 0 in
        let some p = List.exists p indices in
        if some (fun i -> straddles v i) then Blocked_bothsides
        else if
          some (fun i ->
              touches i && (kinds.(i) = K_defined || kinds.(i) = K_inverted))
        then Blocked_defined
        else if some (fun i -> touches i && kinds.(i) = K_binder) then
          Blocked_binder
        else if some (fun i -> touches i && kinds.(i) = K_bothcall) then
          Blocked_bothcall
        else Blocked_rule
      in
      let grouped =
        List.filter_map
          (fun c ->
            match List.filter (fun v -> reason v = c) stuck with
            | [] -> None
            | vs -> Some (violation c vs))
          all_cls
      in
      { base with violations = grouped }
    else if k > search_cap then
      { base with violations = [ violation Unknown_cap offenders ] }
    else
      (* Only variables a free condition mentions can change verdict, plus the
         current offenders; everything else counts the same under every
         assignment. *)
      let free_vars =
        R.dedup_stable
          (List.concat_map
             (fun i -> R.vars_of_term (ev i) @ R.vars_of_term (pat i))
             free_idx)
      in
      let candidates = R.dedup_stable (offenders @ free_vars) in
      let slot = Array.make n (-1) in
      List.iteri (fun j i -> slot.(i) <- j) free_idx;
      let flipped_of m i = slot.(i) >= 0 && (m lsr slot.(i)) land 1 = 1 in
      let rec search m =
        if m >= 1 lsl k then None
        else if
          List.for_all (fun v -> not (violates (flipped_of m) v)) candidates
        then Some m
        else search (m + 1)
      in
      match search 0 with
      | Some m ->
          {
            base with
            violations = [ violation Flippable offenders ];
            orientation = Some (List.init n (flipped_of m));
          }
      | None ->
          { base with violations = [ violation Blocked_combination offenders ] }

let defined_of (t : R.t) : string -> bool =
  let tbl = Hashtbl.create 512 in
  List.iter (fun s -> Hashtbl.replace tbl s ()) (R.defined_heads t);
  fun h -> Hashtbl.mem tbl h

let check_system (t : R.t) : rule_report list =
  let defined = defined_of t in
  List.mapi (fun i r -> check_rule ~index:i ~defined r) t.R.rules

let slice_verdict (reps : rule_report list) : slice_verdict =
  let violating = List.filter (fun r -> r.violations <> []) reps in
  if violating = [] then Clean
  else
    let flippable = List.filter (fun r -> r.orientation <> None) violating in
    if List.length flippable = List.length violating then Flippable_all
    else if flippable = [] then Blocked
    else Partial

(* ------------------------------------------------------------------------- *)
(* Reporting. *)

let cls_count (reps : rule_report list) (c : cls) : int =
  List.fold_left
    (fun n r -> n + List.length (List.filter (fun v -> v.cls = c) r.violations))
    0 reps

let sum f reps = List.fold_left (fun n r -> n + f r) 0 reps

let tsv_row (sym : string) (reps : rule_report list) : string =
  let conditional = List.filter (fun r -> r.conds > 0) reps in
  let violating = List.filter (fun r -> r.violations <> []) reps in
  String.concat "\t"
    ([
       sym;
       string_of_verdict (slice_verdict reps);
       string_of_int (List.length reps);
       string_of_int (List.length conditional);
       string_of_int (List.length violating);
     ]
    @ List.map (fun c -> string_of_int (cls_count reps c)) all_cls
    @ [
        string_of_int (sum (fun r -> r.conds) reps);
        string_of_int (sum (fun r -> r.free_conds) reps);
        string_of_int (sum (fun r -> r.bothcall_conds) reps);
      ])

let tsv_header =
  String.concat "\t"
    ([ "#symbol"; "verdict"; "rules"; "cond-rules"; "viol-rules" ]
    @ List.map string_of_cls all_cls
    @ [ "conds"; "free-conds"; "bothcall-conds" ])

let report (t : R.t) ~(syms : string list) : string * string =
  let defined = defined_of t in
  (* One report per RULE, reused by every slice that contains it: a sweep
     slices the same system once per symbol, and the giants share almost all
     their rules. The classification depends only on the rule and on which
     symbols are defined, and a slice is a downward closure -- so every symbol
     a sliced rule calls is defined in the slice too, and the whole-system
     predicate is the same one the slice would compute. *)
  let cache : (R.rule, rule_report) Hashtbl.t = Hashtbl.create 4096 in
  let reports =
    List.mapi
      (fun i r ->
        let rep = check_rule ~index:i ~defined r in
        if not (Hashtbl.mem cache r) then Hashtbl.replace cache r rep;
        rep)
      t.R.rules
  in
  let report_of r =
    match Hashtbl.find_opt cache r with
    | Some rep -> rep
    | None -> check_rule ~index:(-1) ~defined r
  in
  let slicer = R.make_slicer t in
  let rows =
    List.filter_map
      (fun sym ->
        let slice = R.slice_with slicer ~roots:[ sym ] in
        let reps = List.map report_of slice.R.rules in
        if List.for_all (fun r -> r.conds = 0) reps then None
        else Some (tsv_row sym reps))
      syms
  in
  let tsv = String.concat "\n" (tsv_header :: rows) ^ "\n" in
  (* Whole-system summary: a rule is one rule however many slices reach it. *)
  let violating = List.filter (fun r -> r.violations <> []) reports in
  let flips =
    sum
      (fun r ->
        match r.orientation with
        | None -> 0
        | Some fl -> List.length (List.filter Fun.id fl))
      reports
  in
  let capped =
    List.concat_map
      (fun r -> List.filter (fun v -> v.cls = Unknown_cap) r.violations)
      reports
  in
  let b = Buffer.create 1024 in
  let line fmt =
    Printf.ksprintf (fun s -> Buffer.add_string b ("wll: " ^ s ^ "\n")) fmt
  in
  line "%d rules, %d conditional, %d violating WLL" (List.length reports)
    (List.length (List.filter (fun r -> r.conds > 0) reports))
    (List.length violating);
  line "classes: %s"
    (String.concat ", "
       (List.map
          (fun c ->
            Printf.sprintf "%s=%d" (string_of_cls c) (cls_count reports c))
          all_cls));
  line "conditions: %d total, %d free (re-orientable), %d call-on-both-sides"
    (sum (fun r -> r.conds) reports)
    (sum (fun r -> r.free_conds) reports)
    (sum (fun r -> r.bothcall_conds) reports);
  line "class A sites: %d rules, %d conditions to flip"
    (cls_count reports Flippable)
    flips;
  List.iter
    (fun r ->
      List.iter
        (fun v ->
          if v.cls = Flippable then
            line "  class A: rule %d (%s): %s" v.rule_index v.head
              (String.concat " " v.vars))
        r.violations)
    reports;
  line "slices with conditions: %d of %d symbols" (List.length rows)
    (List.length syms);
  if capped = [] then
    line "no rule exceeded the 2^%d re-orientation search" search_cap
  else (
    line
      "%d rule(s) EXCEEDED the 2^%d search -- classified as not-known, not \
       blocked:"
      (List.length capped) search_cap;
    List.iter
      (fun v ->
        line "  rule %d (%s): %s" v.rule_index v.head (String.concat " " v.vars))
      capped);
  (tsv, Buffer.contents b)
