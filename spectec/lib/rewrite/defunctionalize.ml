(* Defunctionalize [def]-valued arguments by call-site specialization.

   The translation has no higher-order terms: a [DefP] parameter
   (`def $check(typeIR) : bool`) and the [DefA] argument that fills it
   (`def $compat_lnot`) used to be dropped, so a translated clause called a
   bare `$check` symbol that nothing defines -- the guard never held and every
   caller was stuck (todo.md: the tuple3.p4 / `|+|` typing blocker).

   In SpecTec a [DefA] argument is always a literal `def $name`, so the
   higher-order definition is a TEMPLATE over finitely many instantiations:
   for each call `$f(args, def $g)` we emit a first-order copy `$f_$g` with
   the [DefP] parameter removed and `$check := $g` substituted through its
   clauses, and rewrite the call to `$f_$g(args)`. A specialized clause that
   passes its own `def`-parameter along (`$f(.., def $check)` recursion, or a
   chained template call) becomes concrete after substitution and is
   specialized in turn (worklist closure). The templates themselves are
   removed -- their semantics lives on in the copies -- and the result must
   contain no [DefA]/[DefP] at all (checked).

   Runs on the elaborated IL before {!Simplify}, shared by both pipelines via
   {!Pipeline.ctrs_of_spec}; one-slot memo so every consumer of the same spec
   value (the pipelines, {!To_maude}'s hint recomputation) sees the same
   physical result. The identity on a spec with no [def]-parameters (impty),
   so the pinned goldens are unaffected. *)

open Lang.Il
open Common.Source
module StrSet = Set.Make (String)
module StrMap = Map.Make (String)

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

(* The DefP-carrying definitions: the templates specialization copies from. *)
let templates_of_spec (spec : spec) :
    (string * (id * tparam list * param list * typ * clause list)) list =
  List.filter_map
    (fun (d : def) ->
      match d.it with
      | DecD { defid; tparams; params; typ; clauses }
        when List.exists is_def_param params ->
          Some (defid.it, (defid, tparams, params, typ, clauses))
      | _ -> None)
    spec

(* Top-level names already in use (so a specialized name cannot collide). *)
let names_of_spec (spec : spec) : StrSet.t =
  List.fold_left
    (fun s (d : def) ->
      match d.it with
      | DecD { defid = id; _ }
      | RelD { relid = id; _ }
      | TypD { synid = id; _ }
      | BuiltinDecD { defid = id; _ } ->
          StrSet.add id.it s)
    StrSet.empty spec

let run (spec : spec) : spec =
  let templates = templates_of_spec spec in
  let taken = ref (names_of_spec spec) in
  (* (template id, concrete def ids) -> specialized id *)
  let made : (string * string list, string) Hashtbl.t = Hashtbl.create 16 in
  (* specializations still to generate / already generated, per template *)
  let queue : (string * string list * string) Queue.t = Queue.create () in
  let clones : (string, def list) Hashtbl.t = Hashtbl.create 16 in
  let request (f : string) (gs : string list) : string =
    match Hashtbl.find_opt made (f, gs) with
    | Some nid -> nid
    | None ->
        let rec free s = if StrSet.mem s !taken then free (s ^ "'") else s in
        let nid = free (String.concat "_" (f :: gs)) in
        taken := StrSet.add nid !taken;
        Hashtbl.add made (f, gs) nid;
        Queue.add (f, gs, nid) queue;
        nid
  in
  (* Rewrite calls in an expression tree. [formals] maps the enclosing
     clause's def-parameter names to the concrete ids of the specialization
     being generated (empty outside template clones): a call OF a formal
     (`$check(x)`) retargets to the concrete def, a pass-through ARGUMENT
     (`def $check`) becomes the concrete `def $g`. A call whose def-arguments
     are then all concrete is redirected to its specialization. *)
  let rec rewrite_exp (formals : string StrMap.t) (e : exp) : exp =
    let e = { e with it = Exp_map.map_subexps (rewrite_exp formals) e.it } in
    match e.it with
    | CallE (f, targs, args) ->
        let f =
          match StrMap.find_opt f.it formals with
          | Some g -> { f with it = g }
          | None -> f
        in
        let args =
          List.map
            (fun (a : arg) ->
              match a.it with
              | DefA g -> (
                  match StrMap.find_opt g.it formals with
                  | Some g' -> { a with it = DefA { g with it = g' } }
                  | None -> a)
              | ExpA _ -> a)
            args
        in
        let gs = def_arg_ids args in
        if gs = [] then { e with it = CallE (f, targs, args) }
        else if not (List.mem_assoc f.it templates) then
          failwith
            (Printf.sprintf
               "Defunctionalize: %s takes a def argument but declares no def \
                parameter"
               f.it)
        else
          let nid = request f.it (List.map (fun (g : id) -> g.it) gs) in
          { e with it = CallE ({ f with it = nid }, targs, drop_def_args args) }
    | _ -> e
  in
  let rec rewrite_prem (formals : string StrMap.t) (p : prem) : prem =
    let rw = rewrite_exp formals in
    let it =
      match p.it with
      | RelPr { relid; notexp } ->
          RelPr { relid; notexp = Mixfix.map rw notexp }
      | RelAssertPr { call = { relid; notexp }; expect } ->
          RelAssertPr
            { call = { relid; notexp = Mixfix.map rw notexp }; expect }
      | IfPr { cond; role } -> IfPr { cond = rw cond; role }
      | LetPr (l, r) -> LetPr (rw l, rw r)
      | IterPr (inner, ie) -> IterPr (rewrite_prem formals inner, ie)
      | DebugPr e -> DebugPr (rw e)
      | ElsePr -> ElsePr
    in
    { p with it }
  in
  let rewrite_clause (formals : string StrMap.t) (c : clause) : clause =
    let { args; body; prems } = c.it in
    let args =
      List.map
        (fun (a : arg) ->
          match a.it with
          | ExpA e -> { a with it = ExpA (rewrite_exp formals e) }
          | DefA _ -> a)
        args
    in
    {
      c with
      it =
        {
          args;
          body = rewrite_exp formals body;
          prems = List.map (rewrite_prem formals) prems;
        };
    }
  in
  (* Generate a specialization: clone the template's clauses with the clause's
     own def-parameter names (its head [DefA]s) mapped to the concrete ids,
     and the def positions removed from the head and the parameter list. *)
  let generate (f : string) (gs : string list) (nid : string) : unit =
    let fid, tps, params, t, cls = List.assoc f templates in
    let params' = List.filter (fun p -> not (is_def_param p)) params in
    let cls' =
      List.map
        (fun (c : clause) ->
          let { args; _ } = c.it in
          let formal_names =
            List.map (fun (g : id) -> g.it) (def_arg_ids args)
          in
          if List.length formal_names <> List.length gs then
            failwith
              (Printf.sprintf
                 "Defunctionalize: clause of %s binds %d def parameters, call \
                  supplies %d"
                 f (List.length formal_names) (List.length gs));
          let formals =
            List.fold_left2
              (fun m fo g -> StrMap.add fo g m)
              StrMap.empty formal_names gs
          in
          let c = rewrite_clause formals c in
          let { args; body; prems } = c.it in
          { c with it = { args = drop_def_args args; body; prems } })
        cls
    in
    let d =
      DecD
        {
          defid = { fid with it = nid };
          tparams = tps;
          params = params';
          typ = t;
          clauses = cls';
        }
      $ fid.at
    in
    let prev = Option.value (Hashtbl.find_opt clones f) ~default:[] in
    Hashtbl.replace clones f (prev @ [ d ])
  in
  (* Rewrite every non-template definition (seeding the worklist), then drain
     it -- a clone can request further specializations (recursion, chained
     templates), so generation runs to closure. *)
  let none = StrMap.empty in
  let rewritten =
    List.map
      (fun (d : def) ->
        match d.it with
        | DecD { defid; _ } when List.mem_assoc defid.it templates -> None
        | DecD { defid; tparams; params; typ; clauses } ->
            Some
              {
                d with
                it =
                  DecD
                    {
                      defid;
                      tparams;
                      params;
                      typ;
                      clauses = List.map (rewrite_clause none) clauses;
                    };
              }
        | RelD { relid; reltyp; rules } ->
            let rules =
              List.map
                (fun (r : rule) ->
                  let { ruleid; concl; prems } = r.it in
                  {
                    r with
                    it =
                      {
                        ruleid;
                        concl = Mixfix.map (rewrite_exp none) concl;
                        prems = List.map (rewrite_prem none) prems;
                      };
                  })
                rules
            in
            Some { d with it = RelD { relid; reltyp; rules } }
        | TypD _ | BuiltinDecD _ -> Some d)
      spec
  in
  let rec drain () =
    match Queue.take_opt queue with
    | None -> ()
    | Some (f, gs, nid) ->
        generate f gs nid;
        drain ()
  in
  drain ();
  (* Splice each template's specializations at the template's position. *)
  List.concat
    (List.map2
       (fun (d : def) rewritten ->
         match rewritten with
         | Some d' -> [ d' ]
         | None -> (
             match d.it with
             | DecD { defid; _ } ->
                 Option.value (Hashtbl.find_opt clones defid.it) ~default:[]
             | _ -> assert false))
       spec rewritten)

(* No [DefA] may survive: a leftover means a call this pass missed, which the
   translation would silently break (the old dropped-argument behaviour). *)
let leftover_def_arg (spec : spec) : string option =
  let in_exp (e : exp) : bool =
    let rec go (e : exp) =
      (match e.it with
      | CallE (_, _, args) -> def_arg_ids args <> []
      | _ -> false)
      || List.exists go (Exp_map.subexps e.it)
    in
    go e
  in
  List.find_map
    (fun (d : def) ->
      match d.it with
      | DecD { defid; params; clauses; _ } ->
          if
            List.exists is_def_param params
            || List.exists
                 (fun (c : clause) ->
                   let { args; body; prems } = c.it in
                   List.exists
                     (fun (a : arg) ->
                       match a.it with DefA _ -> true | ExpA e -> in_exp e)
                     args
                   || in_exp body
                   || List.exists
                        (fun p -> List.exists in_exp (Exp_map.exps_of_prem p))
                        prems)
                 clauses
          then Some defid.it
          else None
      | RelD { relid; rules; _ } ->
          if
            List.exists
              (fun (r : rule) ->
                let { concl; prems; _ } = r.it in
                List.exists in_exp (Mixfix.args concl)
                || List.exists
                     (fun p -> List.exists in_exp (Exp_map.exps_of_prem p))
                     prems)
              rules
          then Some relid.it
          else None
      | TypD _ | BuiltinDecD _ -> None)
    spec

(* One-slot memo keyed by physical equality, like {!Prem_env}'s: the pipelines
   and {!To_maude}'s hint recomputation all start from the same elaborated
   spec value and must agree on the same physical output. *)
let memo : (spec * spec) option ref = ref None

let defunctionalize (spec : spec) : spec =
  match !memo with
  | Some (s, out) when s == spec -> out
  | _ ->
      let out = if templates_of_spec spec = [] then spec else run spec in
      (match leftover_def_arg out with
      | Some id -> failwith ("Defunctionalize: def argument survives in " ^ id)
      | None -> ());
      memo := Some (spec, out);
      out
