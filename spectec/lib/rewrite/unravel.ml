module R = Rewrite_system

type stats = {
  eqs : int;
  rules : int;
  chain_steps : int;
  vars : int;
  owise : int;
}

let string_of_stats s =
  Printf.sprintf "eqs=%d rules=%d u=%d k=%d vars=%d owise=%d" s.eqs s.rules
    s.chain_steps s.chain_steps s.vars s.owise

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

let trs_of_system (t : R.t) : (string * stats, string) result =
  let exception Fail of string in
  try
    (* One TPDB spelling per source name, and never two sources per spelling:
       a collision would silently merge two distinct symbols (or a variable
       with a constant, which the [(VAR ...)] header turns into a variable
       everywhere). *)
    let spelling : (string, string) Hashtbl.t = Hashtbl.create 256 in
    let name kind source spelled =
      (match Hashtbl.find_opt spelling spelled with
      | Some prior when prior <> kind ^ source ->
          raise
            (Fail
               (Printf.sprintf
                  "TPDB identifier collision: %s and %s both spell %s" prior
                  (kind ^ source) spelled))
      | _ -> ());
      Hashtbl.replace spelling spelled (kind ^ source);
      spelled
    in
    let sym f = name "sym " f (tpdb_sym f) in
    let var v = name "var " v (tpdb_var v) in
    let variables = ref [] in
    let rec print (t : R.term) : string =
      match t with
      | R.Var v ->
          let v' = var v in
          if not (List.mem v' !variables) then variables := v' :: !variables;
          v'
      | R.App (f, []) -> sym f
      | R.App (f, args) ->
          sym f ^ "(" ^ String.concat ", " (List.map print args) ^ ")"
    in
    let counter = ref 0 in
    let owise = ref 0 in
    let emitted = ref [] in
    let emit lhs rhs = emitted := (lhs, rhs) :: !emitted in
    List.iter
      (fun (r : R.rule) ->
        if r.owise then incr owise;
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
                let u = Printf.sprintf "u_%d" i
                and k = Printf.sprintf "k_%d" i in
                let keep =
                  R.App (k, args @ List.map (fun v -> R.Var v) !bound)
                in
                emit !cur (R.App (u, [ s; keep ]));
                List.iter
                  (fun v ->
                    if (not (List.mem v !bound)) && not (List.mem v arg_vars)
                    then bound := !bound @ [ v ])
                  (vars_of pat);
                cur := R.App (u, [ pat; keep ]))
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
    let printed = List.map (fun (lhs, rhs) -> (print lhs, print rhs)) rules in
    let b = Buffer.create 4096 in
    Buffer.add_string b
      (Printf.sprintf "(VAR %s)\n"
         (String.concat " " (List.sort compare !variables)));
    Buffer.add_string b "(RULES\n";
    List.iter
      (fun (lhs, rhs) -> Buffer.add_string b ("  " ^ lhs ^ " -> " ^ rhs ^ "\n"))
      printed;
    Buffer.add_string b ")\n";
    Ok
      ( Buffer.contents b,
        {
          eqs = List.length t.R.rules;
          rules = List.length rules;
          chain_steps = !counter;
          vars = List.length !variables;
          owise = !owise;
        } )
  with Fail msg -> Error msg
