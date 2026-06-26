open Lang.Il

(* -------------------------------------------------------------------------- *)
(* Shallow expression map                                                      *)
(* A generic "rebuild this node, applying [f] to each immediate sub-expression" *)
(* combinator. Leaves ([BoolE]/[NumE]/[TextE]/[VarE]) and the non-expression    *)
(* parts of a node (atoms, types, mixops, iterexps, [DefA] args) are returned   *)
(* unchanged. It maps exactly one level deep: [f] decides whether and how to     *)
(* recurse, so the same combinator drives both a substitution (recurse with a    *)
(* whole-term equality check at each node) and a normalization (recurse, then    *)
(* simplify the rebuilt node). Cast wrappers are kept -- this is the identity     *)
(* structural map; callers that want to drop casts do so themselves.             *)
(* -------------------------------------------------------------------------- *)

let map_path_exps (f : exp -> exp) (p : path) : path =
  let rec go (p : path) =
    {
      p with
      it =
        (match p.it with
        | RootP -> RootP
        | DotP (p', a) -> DotP (go p', a)
        | IdxP (p', e) -> IdxP (go p', f e)
        | SliceP (p', e1, e2) -> SliceP (go p', f e1, f e2));
    }
  in
  go p

let map_subexps (f : exp -> exp) : exp' -> exp' = function
  | CaseE notexp -> CaseE (Mixfix.map f notexp)
  | IterE (e, ie) -> IterE (f e, ie)
  | TupleE es -> TupleE (List.map f es)
  | ListE es -> ListE (List.map f es)
  | ConsE (e1, e2) -> ConsE (f e1, f e2)
  | CatE (e1, e2) -> CatE (f e1, f e2)
  | OptE (Some e) -> OptE (Some (f e))
  | UnE (op, ot, e) -> UnE (op, ot, f e)
  | BinE (op, ot, e1, e2) -> BinE (op, ot, f e1, f e2)
  | CmpE (op, ot, e1, e2) -> CmpE (op, ot, f e1, f e2)
  | MatchE (e, p) -> MatchE (f e, p)
  | LenE e -> LenE (f e)
  | DotE (e, a) -> DotE (f e, a)
  | IdxE (e1, e2) -> IdxE (f e1, f e2)
  | SliceE (e1, e2, e3) -> SliceE (f e1, f e2, f e3)
  | UpdE (e1, p, e2) -> UpdE (f e1, map_path_exps f p, f e2)
  | UpCastE (t, e) -> UpCastE (t, f e)
  | DownCastE (t, e) -> DownCastE (t, f e)
  | SubE (e, t) -> SubE (f e, t)
  | StrE fields -> StrE (List.map (fun (a, e) -> (a, f e)) fields)
  | MemE (e1, e2) -> MemE (f e1, f e2)
  | CallE (id, targs, args) ->
      let map_arg (a : arg) =
        match a.it with ExpA e -> { a with it = ExpA (f e) } | DefA _ -> a
      in
      CallE (id, targs, List.map map_arg args)
  | (BoolE _ | NumE _ | TextE _ | VarE _ | OptE None) as e' -> e'

(* The expressions a premise embeds, seen through [IterPr] wrappers: the
   notation arguments of a relation/holds premise, an [if]/[debug] guard, a
   [let]'s two sides. The premise-level counterpart of [subexps]: collectors
   and occurrence checks recurse over this instead of each re-enumerating the
   premise constructors. *)
let rec exps_of_prem (p : prem) : exp list =
  match p.it with
  | RelPr { notexp; _ } | RelAssertPr { call = { notexp; _ }; _ } ->
      Mixfix.args notexp
  | IfPr { cond; _ } -> [ cond ]
  | DebugPr e -> [ e ]
  | LetPr (lhs, rhs) -> [ lhs; rhs ]
  | IterPr (inner, _) -> exps_of_prem inner
  | ElsePr -> []

(* Derived from [map_subexps] (rather than re-enumerating the constructors) so
   there is exactly one place that knows which children a node has. *)
let subexps (e' : exp') : exp list =
  let acc = ref [] in
  ignore
    (map_subexps
       (fun e ->
         acc := e :: !acc;
         e)
       e');
  List.rev !acc
