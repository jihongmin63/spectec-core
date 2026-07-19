open Common.Source
open Lang.Il
open Ctrs_term

(** Variable type hints. The CTRS term layer keeps a variable's name but drops
    its IL type; the COPS/TPDB surfaces never needed it. A typed backend
    ({!To_maude}) does: a relation argument's declared type widens its variables
    (e.g. [Eval_expr]'s subject is [expr], but the "id" rule's [x] is really an
    [id]), and recovering the narrower declared type is what keeps such a rule
    from overlapping its siblings. The type is still on each [VarE] note in the
    simplified spec, so collect it per defined symbol. *)

(* Every [(variable name, IL type)] occurring in [e], descending through casts
   and all sub-expressions. A bare iterated variable [x*]/[x?] ([IterE] over a
   lone [VarE]) compiles to the list/option [x] itself ([term_of_exp]), so its
   CTRS type is the ITERATED type carried on the [IterE] note, not the element
   type on the inner [VarE]; record the former and do not descend (descending
   would record the element type, clashing and dropping the hint -- which then
   strands a list-recursive rule's tail at the element sort, e.g. a [cons] tail
   typed [TypeArgument] instead of [List], so [nil] cannot bind it). *)
let rec collect_var_types (acc : (string * typ') list) (e : exp) :
    (string * typ') list =
  match e.it with
  | IterE ({ it = VarE id; _ }, _) -> (id.it, e.note) :: acc
  (* A structured iterated body (e.g. a head destructure [(typ id `;)*]) compiles
     to helpers ([$unzip]/[$itermap]) that bind each co-iterated variable to its
     list-level stream, so the variable's CTRS type is the ITERATED type built
     from its own [iter list], not the element type its [VarE] carries inside the
     body. Record those streams and collect only the body's OTHER (captured)
     variables -- descending unfiltered would mistype a stream as its element
     (the same hazard the bare-[VarE] case above avoids), stranding e.g. a [cons]
     tail at the element sort so [nil] cannot bind it. *)
  | IterE (body, (iter, vars)) ->
      let bound = List.map (fun ({ varid; _ } : var) -> varid.it) vars in
      (* the variable iterates under this [IterE]'s [iter] (its own [iters] are
         further, inner nestings), so wrap the element type in both *)
      let stream_typ (typ : typ) (iters : iter list) : typ' =
        let inner =
          List.fold_left
            (fun acc it -> IterT { typ = acc $ typ.at; iter = it })
            typ.it iters
        in
        IterT { typ = inner $ typ.at; iter }
      in
      let streams =
        List.map
          (fun ({ varid; typ; iters } : var) ->
            (varid.it, stream_typ typ iters))
          vars
      in
      let body_others =
        List.filter
          (fun (v, _) -> not (List.mem v bound))
          (collect_var_types [] body)
      in
      streams @ body_others @ acc
  | _ ->
      let acc =
        match e.it with VarE id -> (id.it, e.note) :: acc | _ -> acc
      in
      List.fold_left collect_var_types acc (Exp_map.subexps e.it)

(* The premise-level counterpart of [collect_var_types]'s [IterE] branch: an
   [IterPr]'s co-iterated variables are bound at their STREAM type by the
   compiled [$itercollect]/[$iterall] helpers (the rule's condition binds e.g.
   [_expressionIR] to the whole projected list), so descending transparently
   into the inner premise would record the element type and missort the bound
   stream (a [List]-valued condition output typed [ExpressionIR] can never
   match). Record the streams; collect inside only for the captured rest. *)
let rec collect_prem_var_types (acc : (string * typ') list) (p : prem) :
    (string * typ') list =
  match p.it with
  | IterPr (inner, (iter, vars)) ->
      let bound = List.map (fun ({ varid; _ } : var) -> varid.it) vars in
      let stream_typ (typ : typ) (iters : iter list) : typ' =
        let wrapped =
          List.fold_left
            (fun acc it -> IterT { typ = acc $ typ.at; iter = it })
            typ.it iters
        in
        IterT { typ = wrapped $ typ.at; iter }
      in
      let streams =
        List.map
          (fun ({ varid; typ; iters } : var) ->
            (varid.it, stream_typ typ iters))
          vars
      in
      let inner_others =
        List.filter
          (fun (v, _) -> not (List.mem v bound))
          (collect_prem_var_types [] inner)
      in
      streams @ inner_others @ acc
  | _ -> List.fold_left collect_var_types acc (Exp_map.exps_of_prem p)

(* Keep a variable's type only when all its occurrences agree; an unresolved
   clash drops it (the backend then falls back to its own inference). *)
let resolve_var_types (vts : (string * typ') list) : (string * typ') list =
  let seen = Hashtbl.create 16 in
  List.iter
    (fun (v, t) ->
      match Hashtbl.find_opt seen v with
      | None -> Hashtbl.replace seen v (Some t)
      | Some (Some t') when t' = t -> ()
      | Some _ -> Hashtbl.replace seen v None)
    vts;
  Hashtbl.fold
    (fun v t acc -> match t with Some t -> (v, t) :: acc | None -> acc)
    seen []

(* Per defined symbol ([func_sym]/[rel_sym]), the IL type of each variable in
   that function's clauses or relation's rules. Keyed by symbol (not by rule) so
   it needs no rule-ordering assumption; the prelude's reuse of names like [x]
   never collides because those rules define different symbols. *)
let of_spec (spec : spec) : (string, (string * typ') list) Hashtbl.t =
  let tbl = Hashtbl.create 64 in
  let of_prems acc prems = List.fold_left collect_prem_var_types acc prems in
  List.iter
    (fun def ->
      match def.it with
      | DecD { defid = id; clauses; _ } ->
          let vts =
            List.fold_left
              (fun acc clause ->
                let { args; body = exp; prems } = clause.it in
                let acc =
                  List.fold_left
                    (fun acc a ->
                      match a.it with
                      | ExpA e -> collect_var_types acc e
                      | DefA _ -> acc)
                    acc args
                in
                of_prems (collect_var_types acc exp) prems)
              [] clauses
          in
          Hashtbl.replace tbl (func_sym id) (resolve_var_types vts)
      | RelD { relid = id; rules; _ } ->
          let vts =
            List.fold_left
              (fun acc rl ->
                let { concl = ne; prems; _ } = rl.it in
                of_prems
                  (List.fold_left collect_var_types acc (Mixfix.args ne))
                  prems)
              [] rules
          in
          Hashtbl.replace tbl (rel_sym id) (resolve_var_types vts)
      | TypD _ | BuiltinDecD _ -> ())
    spec;
  tbl
