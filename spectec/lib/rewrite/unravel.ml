module R = Rewrite_system

type stats = {
  eqs : int;
  rules : int;
  chain_steps : int;
  vars : int;
  owise : int;
}

let string_of_stats s =
  Printf.sprintf "eqs=%d rules=%d u=%d vars=%d owise=%d" s.eqs s.rules
    s.chain_steps s.vars s.owise

(* ------------------------------------------------------------------------- *)
(* TPDB identifier scrub. CTRS identifiers are [A-Za-z0-9_$] ({!R.sanitize},
   plus the [$] function prefix); [$] is not a TPDB identifier character, so it
   maps to the [d_] prefix. Variable names can carry arbitrary characters (a
   pretty-printed pattern); the non-plain ones go through {!R.sanitize} first,
   exactly as the Maude surface's {!Maude_ident.var} does -- so this scrub composed
   with the CTRS names equals the scratchpad unraveler's scrub composed with
   the printed Maude module. *)

let plain (v : string) : bool =
  String.for_all
    (function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
    v

let scrub (s : string) : string =
  let s =
    if String.length s >= 1 && s.[0] = '$' then
      "d_" ^ String.sub s 1 (String.length s - 1)
    else s
  in
  let s = if plain s then s else R.sanitize s in
  match s.[0] with 'A' .. 'Z' | 'a' .. 'z' | '_' -> s | _ -> "s_" ^ s

let tpdb_sym (f : string) : string = scrub f
let tpdb_var (v : string) : string = scrub (if plain v then v else R.sanitize v)

(* ------------------------------------------------------------------------- *)

(* Variables of a term in first-appearance order, deduplicated. *)
let vars_of (t : R.term) : string list = R.dedup_stable (R.vars_of_term t)

let subset (xs : string list) (ys : string list) : bool =
  List.for_all (fun x -> List.mem x ys) xs

(* Greedy binding-order schedule (the {!Crc_surface.order_conds} convention): repeatedly
   take the first condition whose evaluated side is fully bound, then treat its
   pattern side as a binder. [None] on a residue nothing can bind (a cycle). *)
let schedule_conds (lhs_vars : string list) (conds : R.cond list) :
    R.cond list option =
  let rec go bound pending acc =
    match pending with
    | [] -> Some (List.rev acc)
    | _ -> (
        let rec take before = function
          | [] -> None
          | ((s, _) as c) :: after ->
              if subset (vars_of s) bound then
                Some (c, List.rev_append before after)
              else take (c :: before) after
        in
        match take [] pending with
        | None -> None
        | Some (((_, pat) as c), rest) ->
            go (bound @ vars_of pat) rest (c :: acc))
  in
  go lhs_vars conds []

(* What the chain step CARRIES across a condition: the lhs VARIABLES, as direct
   arguments of [u_i]. The alternative -- wrapping the lhs argument list
   verbatim in an inert keep-constructor [k_i], which is what this unraveler
   emitted until 2026-08-19 -- is equally sound and equally provable on a small
   slice, but it hands AProVE a problem a far more expensive processor has to
   close, and at scale that decides everything.

   On a structural recursion -- destructure a list in a premise, recurse on the
   tail -- the two shapes are:

     kept: D(cons(h,t), ..) -> U(b, k(cons(h,t), ..))    U(true, k(cons(h,t), ..)) -> D(t, ..)
     flat: D(cons(h,t), ..) -> U(b, h, t, ..)            U(true, h, t, ..)          -> D(t, ..)

   Size-change termination compares argument POSITIONS pairwise. Flat, the
   [cons(h,t)] at D's position 1 meets [t] at U's -- a strict subterm, a strict
   decrease, and the cycle closes by [QDPSizeChangeProof] at no cost. Kept, that
   argument meets [k(cons(h,t), ..)], which CONTAINS it: neither a decrease nor
   an equality, so no edge is recorded at all and size-change has nothing to work
   with. The cycle is still closable, but only by searching for a reduction pair
   -- affordable for one slice, not for the 577 dependency-graph children of
   [Program_ok], where size-change was the only processor that ever succeeded.
   Measured one-variable on those children: kept closes 317, flat closes 502.

   Flat is an over-approximation of the kept shape, which is the safe direction:
   the consumer [u(pat, x1..xn)] matches MORE terms than its producer builds
   (the original arguments' pattern structure and non-linearity are no longer
   enforced), exactly as dropping the sorts does. *)

(* The unraveling proper: every conditional rule becomes a [u_i] chain, so
   what comes back is an UNCONDITIONAL rewrite system -- the common core of
   every plain-TRS export. Kept separate from the printers because the two
   surfaces spell the same rules differently ({!trs_of_system} in TPDB,
   {!Itrs.of_system} in ITRS with integer constraints) while the chain
   construction, its binder accumulation and its coverage checks must stay
   identical: a divergence there would make the two surfaces prove different
   systems while claiming to be the same one. *)

type unraveled = {
  unconditional : (R.term * R.term) list;
  defines : (string * string) list;
  steps : int;
  dropped_owise : int;
}

let unconditional_rules (t : R.t) : (unraveled, string) result =
  let exception Fail of string in
  try
    let counter = ref 0 in
    let owise = ref 0 in
    let emitted = ref [] in
    (* Which CTRS symbol each emitted symbol came from: a defined head from
       itself, a chain helper from the head of the rule whose condition
       introduced it. A helper's NUMBER is assigned by a counter over the whole
       rule list, so it is the one thing about an unraveling that is not a
       function of the rule alone -- which is why a caller wanting the
       unraveling of a SUBSET must select from these rules rather than unravel
       the subset (its helpers would be numbered differently), and why it needs
       this table to know which rules to select. *)
    let defines : (string, string) Hashtbl.t = Hashtbl.create 512 in
    let define sym origin =
      if origin <> "" then Hashtbl.replace defines sym origin
    in
    let emit lhs rhs = emitted := (lhs, rhs) :: !emitted in
    List.iter
      (fun (r : R.rule) ->
        if r.owise then incr owise;
        let origin = Option.value (R.defined_head r) ~default:"" in
        define origin origin;
        match r.conds with
        | [] -> emit r.lhs r.rhs
        | conds ->
            let args = match r.lhs with R.App (_, a) -> a | R.Var _ -> [] in
            let arg_vars =
              R.dedup_stable (List.concat_map R.vars_of_term args)
            in
            let ordered =
              match schedule_conds arg_vars conds with
              | Some cs -> cs
              | None ->
                  raise
                    (Fail
                       ("unorderable conditions in a rule for "
                       ^ Option.value (R.defined_head r) ~default:"?"))
            in
            let bound = ref [] in
            let cur = ref r.lhs in
            List.iter
              (fun (s, pat) ->
                incr counter;
                let i = !counter in
                let u = Printf.sprintf "u_%d" i in
                define u origin;
                let carried = List.map (fun v -> R.Var v) (arg_vars @ !bound) in
                emit !cur (R.App (u, s :: carried));
                List.iter
                  (fun v ->
                    if (not (List.mem v !bound)) && not (List.mem v arg_vars)
                    then bound := !bound @ [ v ])
                  (vars_of pat);
                cur := R.App (u, pat :: carried))
              ordered;
            emit !cur r.rhs)
      t.R.rules;
    let rules = List.rev !emitted in
    (* Every rhs variable must be lhs-bound, or the TRS is malformed. *)
    List.iter
      (fun (lhs, rhs) ->
        let extra =
          List.filter
            (fun v -> not (List.mem v (R.vars_of_term lhs)))
            (R.vars_of_term rhs)
        in
        if extra <> [] then
          raise
            (Fail
               (Printf.sprintf "unbound rhs variable(s) %s in %s -> %s"
                  (String.concat ", " (R.dedup_stable extra))
                  (R.string_of_term lhs) (R.string_of_term rhs))))
      rules;
    Ok
      {
        unconditional = rules;
        defines =
          List.sort compare
            (Hashtbl.fold (fun k v acc -> (k, v) :: acc) defines []);
        steps = !counter;
        dropped_owise = !owise;
      }
  with Fail msg -> Error msg

(* Spell a set of unconditional rules as TPDB text: the identifier scrub, the
   collision table, and the [(VAR ...)] roll-up. Shared with {!Itrs} for the
   same reason {!unconditional_rules} is -- one scrub, so a symbol has one
   spelling on both surfaces -- and returned as printed PAIRS rather than a
   finished document because the two surfaces wrap them differently (ITRS adds
   a goal header, a hand-written integer prelude, and a [:|:] tail per rule). *)
let print_rules (rules : (R.term * R.term) list) :
    ((string * string) list * string list, string) result =
  let exception Fail of string in
  try
    (* One TPDB spelling per source name, and never two sources per spelling.
       The scrub ({!R.sanitize}) is intentionally non-injective (its own doc
       notes distinct inputs may collide), so a collision is resolved here.
       Merging two symbols -- or a symbol with a variable -- is unsound: their
       rules would alias (and the [(VAR ...)] header turns any shared spelling
       into a variable everywhere), so that stays an error. Two distinct
       VARIABLES are rule-local, so the later one takes a numbered spelling
       instead of aborting the whole export. [spelling] maps a chosen spelling
       to its owning source; [assigned] gives each source its final spelling so
       repeat occurrences stay consistent. A source with no collision keeps its
       bare scrub, so non-colliding systems are byte-identical to before. *)
    let spelling : (string, string) Hashtbl.t = Hashtbl.create 256 in
    let assigned : (string, string) Hashtbl.t = Hashtbl.create 256 in
    let name kind source spelled =
      let key = kind ^ source in
      match Hashtbl.find_opt assigned key with
      | Some final -> final
      | None ->
          let rec uniq cand i =
            match Hashtbl.find_opt spelling cand with
            | Some owner when owner <> key ->
                if kind = "var " then
                  uniq (Printf.sprintf "%s_%d" spelled i) (i + 1)
                else
                  raise
                    (Fail
                       (Printf.sprintf
                          "TPDB identifier collision: %s and %s both spell %s"
                          owner key spelled))
            | _ -> cand
          in
          let final = uniq spelled 1 in
          Hashtbl.replace spelling final key;
          Hashtbl.replace assigned key final;
          final
    in
    let sym f = name "sym " f (tpdb_sym f) in
    let var v = name "var " v (tpdb_var v) in
    let variables = ref [] in
    (* An integer literal folded in by {!Itrs} is already its own spelling: it
       must reach the file as [-3], not as a scrubbed identifier. *)
    let rec print (t : R.term) : string =
      match t with
      | R.Var v ->
          let v' = var v in
          if not (List.mem v' !variables) then variables := v' :: !variables;
          v'
      | R.App (f, []) when String.length f > 1 && f.[0] = '#' ->
          String.sub f 1 (String.length f - 1)
      | R.App (f, []) -> sym f
      | R.App (f, args) ->
          sym f ^ "(" ^ String.concat ", " (List.map print args) ^ ")"
    in
    let printed = List.map (fun (lhs, rhs) -> (print lhs, print rhs)) rules in
    Ok (printed, List.sort compare !variables)
  with Fail msg -> Error msg

(* The pieces of one export a consumer of the TRS TEXT cannot recover from it:
   the printed rules one by one, and the identifiers the [(VAR ...)] header
   declares. {!Qdp} needs both to re-pose one dependency-graph child against a
   SUBSET of these rules -- and must take that subset from here rather than
   re-printing it, since the collision table is resolved over the whole list. *)
type export = {
  printed : (string * string) list;
  vars : string list;
  origins : (string * string) list;
}

let export_of_system (t : R.t) : (export * stats, string) result =
  match unconditional_rules t with
  | Error msg -> Error msg
  | Ok u -> (
      match print_rules u.unconditional with
      | Error msg -> Error msg
      | Ok (printed, vars) ->
          let origins =
            List.map (fun (sym, origin) -> (tpdb_sym sym, origin)) u.defines
          in
          Ok
            ( { printed; vars; origins },
              {
                eqs = List.length t.R.rules;
                rules = List.length u.unconditional;
                chain_steps = u.steps;
                vars = List.length vars;
                owise = u.dropped_owise;
              } ))

(* The TPDB document: a [(VAR ...)] header over [vars] and a [(RULES ...)] block
   over [printed]. Split from {!export_of_system} so a caller that emits a
   SUBSET of the same rules ({!Qdp}) spells the header the same way. *)
let tpdb_document ~(vars : string list) (printed : (string * string) list) :
    string =
  let b = Buffer.create 4096 in
  Buffer.add_string b (Printf.sprintf "(VAR %s)\n" (String.concat " " vars));
  Buffer.add_string b "(RULES\n";
  List.iter
    (fun (lhs, rhs) -> Buffer.add_string b ("  " ^ lhs ^ " -> " ^ rhs ^ "\n"))
    printed;
  Buffer.add_string b ")\n";
  Buffer.contents b

let trs_of_system (t : R.t) : (string * stats, string) result =
  match export_of_system t with
  | Error msg -> Error msg
  | Ok (e, stats) -> Ok (tpdb_document ~vars:e.vars e.printed, stats)
