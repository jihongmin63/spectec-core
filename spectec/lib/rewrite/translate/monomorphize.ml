open Common.Source
open Lang.Il
module R = Rewrite_system
module T = Ctrs_term

(* Specialize every polymorphic [dec] at the type instantiations its call sites
   ask for, so no type PARAMETER survives into the translation.

   A parameter is a type the translation cannot dispatch on. [$in_set<K>]'s body
   compares at [K], and [K] has no [TypD], so {!To_ctrs.eq_pred} can only fall
   back on the polymorphic [eq] -- the one symbol whose off-diagonal the
   analysis surface cannot decide (the executable one closes it with an [owise]
   the Church-Rosser checker must not be shown). Every caller instantiates [K]
   at a concrete type; giving each its own copy of the body lets the comparison
   reach that type's own [eq_<T>] instead. The same substitution reaches the
   collection builtins: a [$adds_map<K, V>] inside a polymorphic body picks up
   concrete type arguments here, so {!Builtin}'s per-type copies apply to it
   too.

   Type arguments are READ OFF the call site ([CallE]'s [targ list], which
   elaboration fills in), never inferred. Specialization iterates to a fixed
   point: a specialized body's own polymorphic calls have their type arguments
   substituted, and so ask for further instantiations.

   A [dec] with no instantiation, or one a call site reaches without type
   arguments, is left polymorphic rather than dropped -- the translation still
   has something to translate and the symbol keeps its place in
   {!To_ctrs.def_symbols}. [BuiltinDecD]s are left alone throughout: their type
   parameters are resolved where their rules are emitted ({!Builtin}), off the
   same call-site type arguments. Identity on a spec with no polymorphic [dec]
   (impty), like every other pass in the pipeline. *)

(* -------------------------------------------------------------------------- *)
(* Type substitution. *)

type theta = (string * typ') list

let rec subst_typ (th : theta) (t : typ') : typ' =
  match t with
  | BoolT | NumT _ | TextT | FuncT -> t
  (* A type parameter is never applied to arguments (the IL has no higher-order
     type variables), so a hit replaces the whole node. *)
  | VarT { synid; targs } -> (
      match List.assoc_opt synid.it th with
      | Some u -> u
      | None -> VarT { synid; targs = List.map (subst_phrase th) targs })
  | TupleT ts -> TupleT (List.map (subst_phrase th) ts)
  | IterT { typ; iter } -> IterT { typ = subst_phrase th typ; iter }

and subst_phrase (th : theta) (t : typ) : typ =
  { t with it = subst_typ th t.it }

(* A type DEFINITION under a substitution: what [syntax set<K>]'s body says once
   [K] is known. {!To_ctrs} instantiates a polymorphic type's derived equality
   with it -- the definitions themselves are never cloned (a variant's
   constructors are shared across instantiations), only the rules read off
   them. *)
let subst_deftyp (th : theta) (dt : deftyp') : deftyp' =
  let phrase f p = { p with it = f p.it } in
  match dt with
  | PlainT u -> PlainT (subst_phrase th u)
  | StructT fields ->
      StructT (List.map (fun (a, t) -> (a, subst_phrase th t)) fields)
  | VariantT typcases ->
      VariantT
        (List.map
           (fun (tc : typcase) ->
             {
               tc with
               notation = phrase (Mixfix.map (subst_phrase th)) tc.notation;
               origin =
                 phrase
                   (fun (o : typorigin') ->
                     { o with targs = List.map (subst_phrase th) o.targs })
                   tc.origin;
             })
           typcases)

let subst_var (th : theta) (v : var) : var =
  { v with typ = subst_phrase th v.typ }

let subst_iterexp (th : theta) ((i, vs) : iterexp) : iterexp =
  (i, List.map (subst_var th) vs)

(* -------------------------------------------------------------------------- *)
(* The specialized name. Bounded and scrubbed the same way the helper symbols
   are ({!Ctrs_term.abbrev}), since it goes on to become a CTRS symbol. *)

let mono_id (id : id) (targs : targ list) : id =
  let part (t : targ) = "_" ^ T.abbrev (R.sanitize (Print.string_of_typ t)) in
  { id with it = id.it ^ String.concat "" (List.map part targs) }

(* -------------------------------------------------------------------------- *)
(* One traversal serves both jobs: it applies [th] to every type a node carries
   AND retargets a call whose instantiation has been specialized. With an empty
   [th] and the finished [specialized] table it is pure retargeting, which is
   how the rest of the spec is rewritten. *)

type ctx = {
  th : theta;
  poly : (string, def') Hashtbl.t;
  (* [None] while instantiations are still being discovered: no call is
     retargeted yet, only its type arguments substituted. *)
  specialized : (string, unit) Hashtbl.t option;
}

let retarget (c : ctx) (id : id) (targs : targ list) : id * targ list =
  match (targs, c.specialized) with
  | _ :: _, Some done_ when Hashtbl.mem c.poly id.it ->
      let mono = mono_id id targs in
      if Hashtbl.mem done_ mono.it then (mono, []) else (id, targs)
  | _ -> (id, targs)

let rec rw_exp (c : ctx) (e : exp) : exp =
  let it =
    match e.it with
    | UpCastE (t, e1) -> UpCastE (subst_phrase c.th t, rw_exp c e1)
    | DownCastE (t, e1) -> DownCastE (subst_phrase c.th t, rw_exp c e1)
    | SubE (e1, t) -> SubE (rw_exp c e1, subst_phrase c.th t)
    | CallE (id, targs, args) ->
        let targs = List.map (subst_phrase c.th) targs in
        let id, targs = retarget c id targs in
        CallE (id, targs, List.map (rw_arg c) args)
    | IterE (e1, ie) -> IterE (rw_exp c e1, subst_iterexp c.th ie)
    | UpdE (e1, p, e2) -> UpdE (rw_exp c e1, rw_path c p, rw_exp c e2)
    | it -> Exp_map.map_subexps (rw_exp c) it
  in
  { e with it; note = subst_typ c.th e.note }

and rw_arg (c : ctx) (a : arg) : arg =
  match a.it with ExpA e -> { a with it = ExpA (rw_exp c e) } | DefA _ -> a

and rw_path (c : ctx) (p : path) : path =
  let it =
    match p.it with
    | RootP -> RootP
    | IdxP (p1, e) -> IdxP (rw_path c p1, rw_exp c e)
    | SliceP (p1, e1, e2) -> SliceP (rw_path c p1, rw_exp c e1, rw_exp c e2)
    | DotP (p1, a) -> DotP (rw_path c p1, a)
  in
  { p with it; note = subst_typ c.th p.note }

let rec rw_prem (c : ctx) (p : prem) : prem =
  let notexp ne = Mixfix.map (rw_exp c) ne in
  let it =
    match p.it with
    | RelPr { relid; notexp = ne } -> RelPr { relid; notexp = notexp ne }
    | RelAssertPr { call = { relid; notexp = ne }; expect } ->
        RelAssertPr { call = { relid; notexp = notexp ne }; expect }
    | IfPr { cond; role } -> IfPr { cond = rw_exp c cond; role }
    | ElsePr -> ElsePr
    | LetPr (a, b) -> LetPr (rw_exp c a, rw_exp c b)
    | IterPr (p1, ie) -> IterPr (rw_prem c p1, subst_iterexp c.th ie)
    | DebugPr e -> DebugPr (rw_exp c e)
  in
  { p with it }

let rw_clause (c : ctx) (cl : clause) : clause =
  let { args; body; prems } = cl.it in
  {
    cl with
    it =
      {
        args = List.map (rw_arg c) args;
        body = rw_exp c body;
        prems = List.map (rw_prem c) prems;
      };
  }

let rec rw_param (c : ctx) (p : param) : param =
  let it =
    match p.it with
    | ExpP t -> ExpP (subst_phrase c.th t)
    | DefP { defid; tparams; params; typ } ->
        DefP
          {
            defid;
            tparams;
            params = List.map (rw_param c) params;
            typ = subst_phrase c.th typ;
          }
  in
  { p with it }

let rw_rule (c : ctx) (r : rule) : rule =
  let { ruleid; concl; prems } = r.it in
  {
    r with
    it =
      {
        ruleid;
        concl = Mixfix.map (rw_exp c) concl;
        prems = List.map (rw_prem c) prems;
      };
  }

let rw_def (c : ctx) (d : def) : def =
  let it =
    match d.it with
    | DecD ({ clauses; _ } as r) ->
        DecD { r with clauses = List.map (rw_clause c) clauses }
    | RelD ({ rules; _ } as r) ->
        RelD { r with rules = List.map (rw_rule c) rules }
    | (TypD _ | BuiltinDecD _) as it -> it
  in
  { d with it }

(* -------------------------------------------------------------------------- *)
(* Instantiations. *)

type instance = { id : id; targs : targ list }

let poly_decs (spec : spec) : (string, def') Hashtbl.t =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun (d : def) ->
      match d.it with
      | DecD { defid; tparams; _ } when tparams <> [] ->
          Hashtbl.replace tbl defid.it d.it
      | _ -> ())
    spec;
  tbl

let rec calls_of_exp (poly : (string, def') Hashtbl.t) (e : exp) : instance list
    =
  (match e.it with
  | CallE (id, (_ :: _ as targs), _) when Hashtbl.mem poly id.it ->
      [ { id; targs } ]
  | _ -> [])
  @ List.concat_map (calls_of_exp poly) (Exp_map.subexps e.it)

let exps_of_clause (cl : clause) : exp list =
  List.filter_map
    (fun (a : arg) -> match a.it with ExpA e -> Some e | DefA _ -> None)
    cl.it.args
  @ (cl.it.body :: List.concat_map Exp_map.exps_of_prem cl.it.prems)

let exps_of_def (d : def) : exp list =
  match d.it with
  | DecD { clauses; _ } -> List.concat_map exps_of_clause clauses
  | RelD { rules; _ } ->
      List.concat_map
        (fun (r : rule) ->
          Mixfix.args r.it.concl
          @ List.concat_map Exp_map.exps_of_prem r.it.prems)
        rules
  | TypD _ | BuiltinDecD _ -> []

(* The substitution a call site's type arguments make. [None] when the arity
   disagrees, which leaves the [dec] polymorphic instead of specializing it
   wrongly. *)
let theta_of (tparams : tparam list) (targs : targ list) : theta option =
  if List.length tparams <> List.length targs then None
  else
    Some
      (List.map2
         (fun (tp : tparam) (ta : targ) -> (tp.it, ta.it))
         tparams targs)

(* -------------------------------------------------------------------------- *)

let monomorphize (spec : spec) : spec =
  let poly = poly_decs spec in
  if Hashtbl.length poly = 0 then spec
  else
    (* Discover every instantiation, following the ones a specialized body
       itself asks for. *)
    let insts : (string, instance) Hashtbl.t = Hashtbl.create 64 in
    let discovering targs = { th = targs; poly; specialized = None } in
    let rec discover (i : instance) =
      let k = (mono_id i.id i.targs).it in
      if not (Hashtbl.mem insts k) then
        match Hashtbl.find_opt poly i.id.it with
        | Some (DecD { tparams; clauses; _ }) -> (
            match theta_of tparams i.targs with
            | None -> ()
            | Some th ->
                Hashtbl.replace insts k i;
                let c = discovering th in
                List.iter
                  (fun cl ->
                    List.iter
                      (fun e -> List.iter discover (calls_of_exp poly e))
                      (exps_of_clause (rw_clause c cl)))
                  clauses)
        | _ -> ()
    in
    List.iter
      (fun (d : def) ->
        match d.it with
        (* a polymorphic body's calls are reached through its own
           specializations, with their type arguments substituted *)
        | DecD { tparams; _ } when tparams <> [] -> ()
        | _ ->
            List.iter discover
              (List.concat_map (calls_of_exp poly) (exps_of_def d)))
      spec;
    let specialized = Hashtbl.create 64 in
    Hashtbl.iter (fun k _ -> Hashtbl.replace specialized k ()) insts;
    let rewriting th = { th; poly; specialized = Some specialized } in
    (* Emit each polymorphic [dec] as its specializations, in place, so
       declaration order (and with it [def_symbols]) stays stable. *)
    List.concat_map
      (fun (d : def) ->
        match d.it with
        | DecD { defid; tparams; params; typ; clauses } when tparams <> [] ->
            let copies =
              Hashtbl.fold
                (fun _ (i : instance) acc ->
                  if i.id.it <> defid.it then acc
                  else
                    match theta_of tparams i.targs with
                    | None -> acc
                    | Some th ->
                        let c = rewriting th in
                        let d' =
                          {
                            d with
                            it =
                              DecD
                                {
                                  defid = mono_id defid i.targs;
                                  tparams = [];
                                  params = List.map (rw_param c) params;
                                  typ = subst_phrase th typ;
                                  clauses = List.map (rw_clause c) clauses;
                                };
                          }
                        in
                        d' :: acc)
                insts []
            in
            if copies = [] then [ rw_def (rewriting []) d ]
            else
              List.sort
                (fun (a : def) (b : def) ->
                  match (a.it, b.it) with
                  | DecD x, DecD y -> compare x.defid.it y.defid.it
                  | _ -> 0)
                copies
        | _ -> [ rw_def (rewriting []) d ])
      spec
