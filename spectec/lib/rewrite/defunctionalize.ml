(* Defunctionalize [def]-valued arguments by call-site specialization.

   A template (a definition with a [DefP] parameter) is specialized per call
   into a first-order copy with the def-parameter substituted away, and the call
   rewritten to that copy; chained and recursive def-parameter passing runs to
   closure. Runs before {!Simplify}, memoized one slot by physical equality, and
   the identity on a spec with no [def]-parameters. *)

open Lang.Il
open Common.Source
module StrSet = Set.Make (String)
module StrMap = Map.Make (String)

(* A specialization: a template id with the concrete def ids filling its [DefP]
   parameters. Keys the copy's name, so equal specializations share one copy. *)
type inst = string * string list

module InstMap = Map.Make (struct
  type t = inst

  let compare = compare
end)

(* A definition with a [DefP] parameter, cloned per specialization. *)
type template = {
  defid : id;
  tparams : tparam list;
  params : param list;
  typ : typ;
  clauses : clause list;
}

let id_strs : id list -> string list = List.map (fun (g : id) -> g.it)

let def_arg_ids (args : arg list) : id list =
  List.filter_map
    (fun (a : arg) -> match a.it with DefA g -> Some g | ExpA _ -> None)
    args

let is_def_param (p : param) =
  match p.it with DefP _ -> true | ExpP _ -> false

let drop_def_args (args : arg list) : arg list =
  List.filter
    (fun (a : arg) -> match a.it with DefA _ -> false | _ -> true)
    args

let templates_of_spec (spec : spec) : (string * template) list =
  List.filter_map
    (fun (d : def) ->
      match d.it with
      | DecD { defid; tparams; params; typ; clauses }
        when List.exists is_def_param params ->
          Some (defid.it, { defid; tparams; params; typ; clauses })
      | _ -> None)
    spec

(* Names already taken at top level, which a copy's name must avoid. *)
let names_of_spec (spec : spec) : StrSet.t =
  List.fold_left
    (fun acc (d : def) ->
      match d.it with
      | DecD { defid = id; _ }
      | RelD { relid = id; _ }
      | TypD { synid = id; _ }
      | BuiltinDecD { defid = id; _ } ->
          StrSet.add id.it acc)
    StrSet.empty spec

(* Apply a def-parameter substitution [def_subst] (def-parameter name -> concrete
   def) at the two positions a def-parameter can occur: [resolve_callee] at a
   called def-parameter (`$check(x)`), [resolve_def_args] at a [DefA] argument
   (`def $check`). *)
let resolve_callee (def_subst : string StrMap.t) (callee : id) : id =
  match StrMap.find_opt callee.it def_subst with
  | Some concrete -> { callee with it = concrete }
  | None -> callee

let resolve_def_args (def_subst : string StrMap.t) (args : arg list) : arg list
    =
  List.map
    (fun (a : arg) ->
      match a.it with
      | DefA def_id -> (
          match StrMap.find_opt def_id.it def_subst with
          | Some concrete -> { a with it = DefA { def_id with it = concrete } }
          | None -> a)
      | ExpA _ -> a)
    args

(* The def-parameter substitution for one clause: its head def-parameter names
   mapped to the concrete [def_ids], in order. *)
let def_subst_of_clause (template_id : string) (def_ids : string list)
    (c : clause) : string StrMap.t =
  let param_ids = id_strs (def_arg_ids c.it.args) in
  if List.length param_ids <> List.length def_ids then
    failwith
      (Printf.sprintf
         "Defunctionalize: clause of %s binds %d def parameters, call supplies \
          %d"
         template_id (List.length param_ids) (List.length def_ids));
  List.fold_left2
    (fun acc param concrete -> StrMap.add param concrete acc)
    StrMap.empty param_ids def_ids

(* Phase 1: discover and name every specialization the spec needs, to closure. *)
module Discover = struct
  (* Search the IL for the specializations it needs, over one exp/prem/clause
     walk: [specializations_induced_by_inst] expands an instantiation, and
     [specializations_in_def] reads a non-template definition (the seed). *)
  let search (templates : (string * template) list) :
      (inst -> inst list) * (def -> inst list) =
    let is_template f = List.mem_assoc f templates in
    let rec specializations_in_exp (def_subst : string StrMap.t) (e : exp) :
        inst list =
      let in_subexps =
        List.concat_map
          (specializations_in_exp def_subst)
          (Exp_map.subexps e.it)
      in
      match e.it with
      | CallE (callee, _, args) ->
          let callee = resolve_callee def_subst callee in
          let def_args = def_arg_ids (resolve_def_args def_subst args) in
          if def_args <> [] && is_template callee.it then
            (callee.it, id_strs def_args) :: in_subexps
          else in_subexps
      | _ -> in_subexps
    in
    let specializations_in_prem def_subst p =
      List.concat_map
        (specializations_in_exp def_subst)
        (Exp_map.exps_of_prem p)
    in
    let specializations_in_clause def_subst (c : clause) : inst list =
      let { args; body; prems } = c.it in
      List.concat_map
        (fun (a : arg) ->
          match a.it with
          | ExpA e -> specializations_in_exp def_subst e
          | DefA _ -> [])
        args
      @ specializations_in_exp def_subst body
      @ List.concat_map (specializations_in_prem def_subst) prems
    in
    let specializations_induced_by_inst ((template_id, def_ids) : inst) :
        inst list =
      let { clauses; _ } = List.assoc template_id templates in
      List.concat_map
        (fun c ->
          specializations_in_clause
            (def_subst_of_clause template_id def_ids c)
            c)
        clauses
    in
    let specializations_in_def (d : def) : inst list =
      match d.it with
      | DecD { defid; _ } when is_template defid.it -> []
      | DecD { clauses; _ } ->
          List.concat_map (specializations_in_clause StrMap.empty) clauses
      | RelD { rules; _ } ->
          List.concat_map
            (fun (r : rule) ->
              let { concl; prems; _ } = r.it in
              List.concat_map
                (specializations_in_exp StrMap.empty)
                (Mixfix.args concl)
              @ List.concat_map (specializations_in_prem StrMap.empty) prems)
            rules
      | TypD _ | BuiltinDecD _ -> []
    in
    (specializations_induced_by_inst, specializations_in_def)

  (* Returns the [inst -> copy name] map. *)
  let run (templates : (string * template) list) (spec : spec) :
      string InstMap.t =
    let specializations_induced_by_inst, specializations_in_def =
      search templates
    in
    let rec fresh_name used_names name =
      if StrSet.mem name used_names then fresh_name used_names (name ^ "'")
      else name
    in
    (* Closure over the worklist: name each unseen instantiation and append the
       specializations it in turn requires. *)
    let rec saturate copy_names used_names = function
      | [] -> copy_names
      | inst :: rest when InstMap.mem inst copy_names ->
          saturate copy_names used_names rest
      | inst :: rest ->
          let template_id, def_ids = inst in
          let copy_name =
            fresh_name used_names (String.concat "_" (template_id :: def_ids))
          in
          saturate
            (InstMap.add inst copy_name copy_names)
            (StrSet.add copy_name used_names)
            (rest @ specializations_induced_by_inst inst)
    in
    saturate InstMap.empty (names_of_spec spec)
      (List.concat_map specializations_in_def spec)
end

(* Phase 2: rewrite calls to their named copies and splice the copies in. *)
module Apply = struct
  (* Clause and rule rewriters: redirect each call with concrete def-arguments to
     its named copy; a non-template call with a def-argument is rejected. *)
  let rewriters (names : string InstMap.t) :
      (string StrMap.t -> clause -> clause) * (rule -> rule) =
    let rec rewrite_exp (def_subst : string StrMap.t) (e : exp) : exp =
      let e =
        { e with it = Exp_map.map_subexps (rewrite_exp def_subst) e.it }
      in
      match e.it with
      | CallE (callee, targs, args) -> (
          let callee = resolve_callee def_subst callee in
          let args = resolve_def_args def_subst args in
          match def_arg_ids args with
          | [] -> { e with it = CallE (callee, targs, args) }
          | def_args -> (
              match InstMap.find_opt (callee.it, id_strs def_args) names with
              | Some copy_name ->
                  {
                    e with
                    it =
                      CallE
                        ( { callee with it = copy_name },
                          targs,
                          drop_def_args args );
                  }
              | None ->
                  failwith
                    (Printf.sprintf
                       "Defunctionalize: %s takes a def argument but declares \
                        no def parameter"
                       callee.it)))
      | _ -> e
    in
    let rec rewrite_prem (def_subst : string StrMap.t) (p : prem) : prem =
      let rw = rewrite_exp def_subst in
      let it =
        match p.it with
        | RelPr { relid; notexp } ->
            RelPr { relid; notexp = Mixfix.map rw notexp }
        | RelAssertPr { call = { relid; notexp }; expect } ->
            RelAssertPr
              { call = { relid; notexp = Mixfix.map rw notexp }; expect }
        | IfPr { cond; role } -> IfPr { cond = rw cond; role }
        | LetPr (l, r) -> LetPr (rw l, rw r)
        | IterPr (inner, ie) -> IterPr (rewrite_prem def_subst inner, ie)
        | DebugPr e -> DebugPr (rw e)
        | ElsePr -> ElsePr
      in
      { p with it }
    in
    let rewrite_clause (def_subst : string StrMap.t) (c : clause) : clause =
      let { args; body; prems } = c.it in
      let args =
        List.map
          (fun (a : arg) ->
            match a.it with
            | ExpA e -> { a with it = ExpA (rewrite_exp def_subst e) }
            | DefA _ -> a)
          args
      in
      {
        c with
        it =
          {
            args;
            body = rewrite_exp def_subst body;
            prems = List.map (rewrite_prem def_subst) prems;
          };
      }
    in
    let rewrite_rule (r : rule) : rule =
      let { ruleid; concl; prems } = r.it in
      {
        r with
        it =
          {
            ruleid;
            concl = Mixfix.map (rewrite_exp StrMap.empty) concl;
            prems = List.map (rewrite_prem StrMap.empty) prems;
          };
      }
    in
    (rewrite_clause, rewrite_rule)

  let run (templates : (string * template) list) (names : string InstMap.t)
      (spec : spec) : spec =
    let is_template f = List.mem_assoc f templates in
    let rewrite_clause, rewrite_rule = rewriters names in
    (* The specialized copy for an instantiation: clone its template, substitute
       the def-parameters, drop the def positions. *)
    let specialized_copy ((template_id, def_ids) as inst : inst) : def =
      let { defid; tparams; params; typ; clauses } =
        List.assoc template_id templates
      in
      let params = List.filter (fun p -> not (is_def_param p)) params in
      let clauses =
        List.map
          (fun c ->
            let c =
              rewrite_clause (def_subst_of_clause template_id def_ids c) c
            in
            let { args; body; prems } = c.it in
            { c with it = { args = drop_def_args args; body; prems } })
          clauses
      in
      DecD
        {
          defid = { defid with it = InstMap.find inst names };
          tparams;
          params;
          typ;
          clauses;
        }
      $ defid.at
    in
    (* The copies, grouped by template. [InstMap] keys sort template-first, so
       each template's instantiations are visited together. *)
    let copies_by_template =
      InstMap.fold
        (fun ((template_id, _) as inst) _copy_name acc ->
          StrMap.update template_id
            (fun prev ->
              Some (Option.value prev ~default:[] @ [ specialized_copy inst ]))
            acc)
        names StrMap.empty
    in
    (* Splice each template's copies in at its position; rewrite everything else. *)
    List.concat_map
      (fun (d : def) ->
        match d.it with
        | DecD { defid; _ } when is_template defid.it ->
            Option.value
              (StrMap.find_opt defid.it copies_by_template)
              ~default:[]
        | DecD { defid; tparams; params; typ; clauses } ->
            [
              {
                d with
                it =
                  DecD
                    {
                      defid;
                      tparams;
                      params;
                      typ;
                      clauses = List.map (rewrite_clause StrMap.empty) clauses;
                    };
              };
            ]
        | RelD { relid; reltyp; rules } ->
            [
              {
                d with
                it = RelD { relid; reltyp; rules = List.map rewrite_rule rules };
              };
            ]
        | TypD _ | BuiltinDecD _ -> [ d ])
      spec
end

(* One-slot memo by physical equality, so every consumer shares one result. *)
let memo : (spec * spec) option ref = ref None

let defunctionalize (spec : spec) : spec =
  match !memo with
  | Some (s, defunctionalized_spec) when s == spec -> defunctionalized_spec
  | _ ->
      let templates = templates_of_spec spec in
      let names = Discover.run templates spec in
      let defunctionalized_spec = Apply.run templates names spec in
      memo := Some (spec, defunctionalized_spec);
      defunctionalized_spec
