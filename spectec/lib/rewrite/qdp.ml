(* Re-posing one child of AProVE's dependency-graph decomposition as a problem
   of its own -- see qdp.mli for what that buys and why the verdict transfers.

   Everything here is text on both ends. The children exist only inside AProVE's
   proof, so the only way to learn what one is, is to read the obligation echo
   it printed; and the only way to hand one back is a TPDB file. The parsing is
   therefore pinned to the shape of that echo:

     ----------------------------------------
     (629)
     Obligation:
     Q DP problem:
     The TRS P consists of the following rules:
        SUBTY_NAMEDTYPE(variant_specializedType(x0, x1)) -> SUBTY_TYPEARGUMENTLIST(x1)
        ...
     The TRS R consists of the following rules:
        ... the whole submitted system, once per child ...
     Q is empty.
     We have to consider all minimal (P,Q,R)-chains.

   Two of those sections are worth holding and one is not: [P] is what the child
   IS, [Q] and the chain sentence are what it must be posed under, and [R] is
   the export we already have -- reprinted per child, which is what makes a full
   capture run gigabytes. *)

module U = Unravel

let separator = "----------------------------------------"
let pairs_marker = "The TRS P consists of the following rules:"
let rules_marker = "The TRS R consists of the following rules:"
let q_marker = "The set Q consists of the following terms:"
let q_empty = "Q is empty."
let chains_marker = "We have to consider"

type problem = {
  node : int;
  pairs : string list;
  q : string list;
  minimal : bool;
}

(* -------------------------------------------------------------------------- *)
(* Lexical helpers: node headers, and the identifiers of a printed rule. *)

(* A node header is [(<digits>)] and then, in the proof tree, a label; in the
   obligation echo it stands alone on its line. *)
let node_header (line : string) : (int * string) option =
  let t = String.trim line in
  if String.length t < 3 || t.[0] <> '(' then None
  else
    match String.index_opt t ')' with
    | None -> None
    | Some close -> (
        let digits = String.sub t 1 (close - 1) in
        match int_of_string_opt digits with
        | None -> None
        | Some id ->
            Some
              ( id,
                String.trim
                  (String.sub t (close + 1) (String.length t - close - 1)) ))

let indent_of (line : string) : int =
  let n = String.length line in
  let rec go i = if i < n && line.[i] = ' ' then go (i + 1) else i in
  go 0

let ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
  | _ -> false

(* The identifiers of a line, in order. A run of digits alone is not one: it can
   only be a numeric literal, which names nothing. *)
let identifiers (line : string) : string list =
  let n = String.length line in
  let rec go i acc =
    if i >= n then List.rev acc
    else if ident_char line.[i] then (
      let j = ref i in
      while !j < n && ident_char line.[!j] do
        incr j
      done;
      let tok = String.sub line i (!j - i) in
      let numeric =
        String.for_all (function '0' .. '9' -> true | _ -> false) tok
      in
      go !j (if numeric then acc else tok :: acc))
    else go (i + 1) acc
  in
  go 0 []

let upper = String.uppercase_ascii

(* -------------------------------------------------------------------------- *)
(* The capture filter. *)

let capture_filter () : string -> bool =
  let in_rules = ref false in
  fun line ->
    let t = String.trim line in
    if t = rules_marker then (
      in_rules := true;
      true)
    else if
      t = separator || t = pairs_marker || t = q_marker || t = q_empty
      || String.starts_with ~prefix:chains_marker t
    then (
      (* Every section that can follow [R] closes it, and the separator does so
         even for an echo that prints none of them. *)
      in_rules := false;
      true)
    else not !in_rules

(* -------------------------------------------------------------------------- *)
(* The proof tree: which nodes were left open. *)

(* A tree node is a PROBLEM ([QDP], [ITRS], [QTRS]), a PROCESSOR applied to one
   ([QDPSizeChangeProof [EQUIVALENT, 0 ms]]), a connective, or a verdict --
   and the difference has to be drawn from the label alone, because a chain of
   problem and processor is printed at ONE indentation; only an AND's children
   step in. So a node continues iff what follows is deeper (its branches) or a
   processor beside it (its next step).

   Read by name suffix rather than by the status bracket, because the two
   misreadings are not equally bad. Mistaking a processor for a problem makes
   the node above it look open: the split then re-poses a node the run had
   continued, which costs a run and proves at least as much. Mistaking a problem
   for a processor makes an OPEN node invisible, and a split that never noticed
   it would report every child closed while one was never posed. Every processor
   AProVE has printed here ends in [Proof]; no problem type does. *)
let is_processor (label : string) : bool =
  let name =
    match String.index_opt label '[' with
    | Some i -> String.trim (String.sub label 0 i)
    | None -> label
  in
  String.ends_with ~suffix:"Proof" name

(* A node that carries a RESULT rather than an obligation, so it is not a child
   anything can be re-posed from. [TRUE] is the one that is not a WST verdict: a
   [DependencyGraphProof] that empties the graph discharges its branch with it,
   and the echo prints the node with [TRUE] as the whole body -- no obligation at
   all. Reading it as an open child made {!Termination.children} look for an
   obligation that does not exist and refuse the entire split, which is what
   [$subst_externMethodTypeDefEnv], [$cast_enum] and [Type_wf] were doing
   (2026-08-21). *)
let is_verdict (label : string) : bool =
  match label with "YES" | "NO" | "MAYBE" | "TRUE" -> true | _ -> false

(* A label that makes the whole tree unusable, for the same reason [OR] does: it
   is not an obligation, so it cannot be re-posed, and unlike [TRUE] it does not
   mean the branch closed. Counting it as neither would let a split report "all
   children closed" over a branch that did not. *)
let is_refuted (label : string) : bool = label = "FALSE"

let is_connective (label : string) : bool =
  match label with "AND" | "OR" -> true | _ -> false

let open_leaves (tree : string) : int list =
  let nodes =
    String.split_on_char '\n' tree
    |> List.filter_map (fun line ->
           Option.map
             (fun (id, label) -> (indent_of line, id, label))
             (node_header line))
  in
  (* An OR node's branches are alternatives: closing all the leaves under it
     proves more than the tree needs, and reading it as a conjunction would be a
     guess about a shape we have never seen printed. Refuse the whole tree. *)
  if List.exists (fun (_, _, label) -> label = "OR" || is_refuted label) nodes
  then []
  else
    let rec go = function
      | [] -> []
      | (indent, id, label) :: rest ->
          let continues =
            match rest with
            | (next_indent, _, next_label) :: _ ->
                next_indent > indent
                || (next_indent = indent && is_processor next_label)
            | [] -> false
          in
          let leaf =
            (not continues)
            && (not (is_processor label))
            && (not (is_verdict label))
            && not (is_connective label)
          in
          if leaf then id :: go rest else go rest
    in
    go nodes

(* -------------------------------------------------------------------------- *)
(* The obligation echo: what each child IS. *)

type section = Skip | Pairs | Q

let problems (captured : string) : problem list =
  let finish cur acc =
    match cur with
    | None -> acc
    | Some (node, pairs, q, minimal) ->
        { node; pairs = List.rev pairs; q = List.rev q; minimal } :: acc
  in
  let rec go lines cur sec acc =
    match lines with
    | [] -> List.rev (finish cur acc)
    | line :: rest -> (
        let t = String.trim line in
        if t = separator then go rest None Skip (finish cur acc)
        else
          match cur with
          | None -> (
              match node_header line with
              | Some (id, "") -> go rest (Some (id, [], [], false)) Skip acc
              | _ -> go rest None Skip acc)
          | Some (node, pairs, q, minimal) -> (
              if t = pairs_marker then go rest cur Pairs acc
              else if t = rules_marker || t = q_empty then go rest cur Skip acc
              else if t = q_marker then go rest cur Q acc
              else if String.starts_with ~prefix:chains_marker t then
                go rest
                  (Some (node, pairs, q, Subproc.contains t "minimal"))
                  Skip acc
              else
                match sec with
                | Pairs when Subproc.contains t "->" ->
                    go rest (Some (node, t :: pairs, q, minimal)) sec acc
                | Q when t <> "" ->
                    go rest (Some (node, pairs, t :: q, minimal)) sec acc
                | _ -> go rest cur sec acc))
  in
  go (String.split_on_char '\n' captured) None Skip []

(* -------------------------------------------------------------------------- *)
(* Naming a child. *)

(* A node id is unique and says nothing. What a child IS, is the recursion its
   dependency pairs run around, and the root symbols of those pairs name it --
   which is what turns a residue list from "nodes 1410, 1411, ..." into the
   families it consists of. Returned in the EXPORT's own spelling, so a caller
   holding the spec can look the symbol up (the declaration's own name and its
   source line are the spec's to give, not ours: {!To_ctrs.symbol_origins}).

   Chain helpers [u_<n>] drop out whenever anything else is present -- there is
   one per condition of the source rule, so they name the unraveling rather than
   the recursion. *)
let is_chain_helper (sym : string) : bool =
  String.length sym > 2
  && (sym.[0] = 'u' || sym.[0] = 'U')
  && sym.[1] = '_'
  && String.for_all
       (function '0' .. '9' -> true | _ -> false)
       (String.sub sym 2 (String.length sym - 2))

let pair_heads ~(export : U.export) (problem : problem) : string list =
  let exported : (string, string) Hashtbl.t = Hashtbl.create 1024 in
  let vars = Hashtbl.create 1024 in
  List.iter (fun v -> Hashtbl.replace vars v ()) export.U.vars;
  List.iter
    (fun (lhs, rhs) ->
      List.iter
        (fun tok ->
          if not (Hashtbl.mem vars tok) then
            Hashtbl.replace exported (upper tok) tok)
        (identifiers lhs @ identifiers rhs))
    export.U.printed;
  let roots =
    List.filter_map
      (fun line -> match identifiers line with h :: _ -> Some h | [] -> None)
      problem.pairs
    |> List.sort_uniq compare
  in
  let named =
    match List.filter (fun r -> not (is_chain_helper r)) roots with
    | [] -> roots
    | kept -> kept
  in
  List.sort_uniq compare
    (List.map
       (fun r -> Option.value (Hashtbl.find_opt exported (upper r)) ~default:r)
       named)

(* -------------------------------------------------------------------------- *)
(* The standalone problem. *)

(* Usable rules: the rules defining a symbol the pairs name, closed under the
   symbols those rules use. Coarser than the definition that looks only below a
   pair's root -- a symbol named anywhere in a pair seeds the closure -- which
   is the safe direction, since keeping a rule that is not usable only makes the
   problem harder.

   Pruning R at all is a PROOF STEP, not a filter: the usable-rules processor is
   sound on a minimal problem provided the C-epsilon pair travels with it (the
   projection rules [c(x,y) -> x] and [c(x,y) -> y]; Urbain 2004, Giesl et al.,
   Mechanizing and Improving Dependency Pairs, JAR 2006). Both conditions are
   enforced below: nothing is dropped from a problem that is not minimal, and
   the pair is emitted whenever anything was.

   What it buys is parsing: about 30s of each standalone child run measured on
   [Program_ok] was AProVE reading the 4MB rule set back in, against a median
   37s for the whole run. *)
let usable (rules : (string * string) list)
    ~(symbols : (string, unit) Hashtbl.t) ~(seeds : string list) :
    (string * string) list =
  let head (lhs : string) =
    match identifiers lhs with h :: _ -> Some h | [] -> None
  in
  let by_head : (string, (string * string) list) Hashtbl.t =
    Hashtbl.create 512
  in
  List.iter
    (fun ((lhs, _) as rule) ->
      match head lhs with
      | None -> ()
      | Some h ->
          Hashtbl.replace by_head h
            (rule :: Option.value (Hashtbl.find_opt by_head h) ~default:[]))
    rules;
  let wanted : (string, unit) Hashtbl.t = Hashtbl.create 512 in
  let rec pull (sym : string) =
    if not (Hashtbl.mem wanted sym) then (
      Hashtbl.replace wanted sym ();
      List.iter
        (fun (_, rhs) ->
          List.iter
            (fun tok -> if Hashtbl.mem symbols tok then pull tok)
            (identifiers rhs))
        (Option.value (Hashtbl.find_opt by_head sym) ~default:[]))
  in
  List.iter pull seeds;
  List.filter
    (fun (lhs, _) ->
      match head lhs with Some h -> Hashtbl.mem wanted h | None -> false)
    rules

(* A name no symbol and no variable of the export already spells. *)
let fresh (taken : (string, unit) Hashtbl.t) (base : string) : string =
  let rec go i =
    let cand = if i = 0 then base else Printf.sprintf "%s_%d" base i in
    if Hashtbl.mem taken cand then go (i + 1) else cand
  in
  go 0

type reposed = {
  seeds : string list;
  rules : (string * string) list;
  cepsilon : (string * string) list;
  vars : string list;
}

let reposed ~(export : U.export) (problem : problem) : (reposed, string) result
    =
  (* An identifier of the export is one or the other: a variable if the
     [(VAR ...)] header declares it, a symbol otherwise. [taken] is both, and
     only serves to keep a fresh name fresh. *)
  let vars : (string, unit) Hashtbl.t = Hashtbl.create 1024 in
  let symbols : (string, unit) Hashtbl.t = Hashtbl.create 1024 in
  let taken : (string, unit) Hashtbl.t = Hashtbl.create 1024 in
  List.iter
    (fun v ->
      Hashtbl.replace vars v ();
      Hashtbl.replace taken v ())
    export.U.vars;
  List.iter
    (fun (lhs, rhs) ->
      List.iter
        (fun tok ->
          if not (Hashtbl.mem taken tok) then (
            Hashtbl.replace symbols tok ();
            Hashtbl.replace taken tok ()))
        (identifiers lhs @ identifiers rhs))
    export.U.printed;
  (* Every identifier in the pairs has to be accounted for. The echo prints our
     own variable names (it does not rename) and spells a dependency-pair symbol
     as the UPPERCASE of the symbol it came from, so the two lookups below cover
     what a child can name -- and anything left over stops the split, because an
     identifier the [(VAR ...)] header omits parses as a CONSTANT, narrowing the
     problem to prove less than the child asked. The last arm is the scheme
     AProVE names the variables IT introduces by -- one of [x]/[y]/[z] and an
     index, with an underscore before the index when the plain spelling is
     already taken. Both halves are observed, not guessed: a run whose [(VAR
     ...)] declares [y0..y3] and no [z] at all gets back [z0..z4] bare and [y_0]
     underscored. The introducer is [TransformationProof]'s instantiation, which
     the innermost surface reaches where the full one does not. Reading one of
     AProVE's SYMBOLS as a variable this way would only generalize the child
     (more chains, a harder problem, a [YES] that still transfers); it is the
     converse -- an unrecognised name parsing as a CONSTANT -- that the error
     below exists to prevent. *)
  let by_upper : (string, string) Hashtbl.t = Hashtbl.create 1024 in
  Hashtbl.iter (fun sym () -> Hashtbl.replace by_upper (upper sym) sym) symbols;
  let is_renamed_var tok =
    let index_from = if String.length tok > 2 && tok.[1] = '_' then 2 else 1 in
    String.length tok > index_from
    && (match tok.[0] with 'x' | 'y' | 'z' -> true | _ -> false)
    && String.for_all
         (function '0' .. '9' -> true | _ -> false)
         (String.sub tok index_from (String.length tok - index_from))
  in
  let unknown = ref None and seeds = ref [] and renamed = ref [] in
  List.iter
    (fun line ->
      List.iter
        (fun tok ->
          if not (Hashtbl.mem vars tok) then
            match Hashtbl.find_opt by_upper (upper tok) with
            | Some sym -> seeds := sym :: !seeds
            | None ->
                if is_renamed_var tok then renamed := tok :: !renamed
                else if !unknown = None then unknown := Some tok)
        (identifiers line))
    (problem.pairs @ problem.q);
  match !unknown with
  | Some tok ->
      Error
        (Printf.sprintf
           "node %d: %S in the dependency pairs is neither a variable nor a \
            symbol of the export"
           problem.node tok)
  | None -> (
      (* A symbol the export spells as a variable would be declared as one here,
         silently rewriting every rule that uses it. *)
      match
        List.find_opt
          (fun v -> Hashtbl.mem symbols v)
          (Rewrite_system.dedup_stable !renamed)
      with
      | Some clash ->
          Error
            (Printf.sprintf
               "node %d: %S is both a symbol and a renamed variable"
               problem.node clash)
      | None ->
          let kept =
            if problem.minimal then
              usable export.U.printed ~symbols
                ~seeds:(Rewrite_system.dedup_stable !seeds)
            else export.U.printed
          in
          let dropped = List.length export.U.printed - List.length kept in
          let ceps =
            if dropped = 0 then None
            else
              let c = fresh taken "c_eps" in
              Hashtbl.replace taken c ();
              let x = fresh taken "ceps_x" in
              Hashtbl.replace taken x ();
              Some (c, x, fresh taken "ceps_y")
          in
          let ceps_rules =
            match ceps with
            | None -> []
            | Some (c, x, y) ->
                let app = Printf.sprintf "%s(%s, %s)" c x y in
                [ (app, x); (app, y) ]
          in
          Ok
            {
              seeds = Rewrite_system.dedup_stable !seeds;
              rules = kept;
              cepsilon = ceps_rules;
              vars =
                (export.U.vars
                @ List.sort compare (Rewrite_system.dedup_stable !renamed)
                @ match ceps with None -> [] | Some (_, x, y) -> [ x; y ]);
            })

let document (problem : problem) (r : reposed) : string =
  let b = Buffer.create 65536 in
  Buffer.add_string b (Printf.sprintf "(VAR %s)\n" (String.concat " " r.vars));
  Buffer.add_string b "(PAIRS\n";
  List.iter (fun p -> Buffer.add_string b ("  " ^ p ^ "\n")) problem.pairs;
  Buffer.add_string b ")\n(RULES\n";
  List.iter
    (fun (lhs, rhs) -> Buffer.add_string b ("  " ^ lhs ^ " -> " ^ rhs ^ "\n"))
    (r.rules @ r.cepsilon);
  Buffer.add_string b ")\n";
  if problem.q <> [] then (
    Buffer.add_string b "(Q\n";
    List.iter (fun t -> Buffer.add_string b ("  " ^ t ^ "\n")) problem.q;
    Buffer.add_string b ")\n");
  if problem.minimal then Buffer.add_string b "(MINIMAL)\n";
  Buffer.contents b

let standalone ~(export : U.export) (problem : problem) :
    (string * int, string) result =
  match reposed ~export problem with
  | Error msg -> Error msg
  | Ok r -> Ok (document problem r, List.length r.rules + List.length r.cepsilon)

(* -------------------------------------------------------------------------- *)
(* Whether a SYSTEM proves a child.

   The other way to answer a child is not to re-pose it as a dependency-pair
   problem at all, but to prove a system terminating and let the child fall out
   of that. Two inclusions carry the whole argument, and both are checked here
   rather than assumed:

     (a) every pair of [P] is (an instance of) a dependency pair of the system;
     (b) every rule the re-posed child would carry is a rule of the system.

   Given both, a [(P,Q,R)]-chain is a [(DP(S),S)]-chain -- the pairs are DP(S)
   pairs, [Q] only removes chains, and the R-steps are S-steps -- so termination
   of [S] leaves the child no infinite chain. The pairs may be instances rather
   than equals because AProVE renames a pair's variables ([x0], [x1], ...) and
   its instantiation processors specialize them; an instance of a DP is still a
   DP up to substitution, and chains built from instances are chains.

   The (b) side is the reason the [C-epsilon] pair travels with the system as
   well as with the [.qdp]: [R] here is {!reposed}'s usable-rules PRUNE, which is
   a proof step and not a subset ({!standalone}'s comment), so what the system
   has to leave no chain for is [usable + C-epsilon], not [R] itself. A system
   proved terminating without the pair would be proving the wrong thing.

   A failed check is a fallback, not a defect: an AProVE processor that REWRITES
   a pair (rather than instantiating it) puts the child outside any DP set of
   ours, and there the [.qdp] re-pose is the only sound way to ask. *)

type parsed = PVar of string | PApp of string * parsed list

exception Unparsed

(* TPDB term syntax as both sides of this print it: an identifier, optionally
   applied to a comma-separated argument list. A nullary symbol prints bare, so
   a bare identifier is a variable when the header declares it and a constant
   otherwise -- which is why the classifier is a parameter and not a heuristic. *)
let parse_term ~(is_var : string -> bool) (text : string) : parsed option =
  let n = String.length text in
  let space i = i < n && (text.[i] = ' ' || text.[i] = '\t') in
  let rec skip i = if space i then skip (i + 1) else i in
  let rec term i =
    let i = skip i in
    let rec ident j =
      if j < n && ident_char text.[j] then ident (j + 1) else j
    in
    let stop = ident i in
    if stop = i then raise Unparsed;
    let id = String.sub text i (stop - i) in
    let j = skip stop in
    if j < n && text.[j] = '(' then
      let args, k = arguments (j + 1) in
      (PApp (id, args), k)
    else ((if is_var id then PVar id else PApp (id, [])), j)
  and arguments i =
    let arg, j = term i in
    let j = skip j in
    if j < n && text.[j] = ',' then
      let rest, k = arguments (j + 1) in
      (arg :: rest, k)
    else if j < n && text.[j] = ')' then ([ arg ], j + 1)
    else raise Unparsed
  in
  match term 0 with
  | t, i when skip i = n -> Some t
  | _ -> None
  | exception Unparsed -> None

(* A printed rule split at its arrow. No identifier can contain [-] (the CTRS
   scrub spells it out), so the first arrow is the rule's. *)
let arrow_split (line : string) : (string * string) option =
  let n = String.length line in
  let rec go i =
    if i + 1 >= n then None
    else if line.[i] = '-' && line.[i + 1] = '>' then
      Some
        ( String.trim (String.sub line 0 i),
          String.trim (String.sub line (i + 2) (n - i - 2)) )
    else go (i + 1)
  in
  go 0

let root = function PApp (f, _) -> Some f | PVar _ -> None

(* [pattern] matched against [subject], extending [bindings]. A pattern variable
   binds once and must meet the same subject everywhere after. *)
let rec matched (bindings : (string * parsed) list option) (pattern : parsed)
    (subject : parsed) : (string * parsed) list option =
  match bindings with
  | None -> None
  | Some b -> (
      match pattern with
      | PVar v -> (
          match List.assoc_opt v b with
          | Some bound -> if bound = subject then bindings else None
          | None -> Some ((v, subject) :: b))
      | PApp (f, args) -> (
          match subject with
          | PApp (g, args') when f = g && List.length args = List.length args'
            ->
              List.fold_left2 matched bindings args args'
          | _ -> None))

(* One dependency pair against one printed pair of [P]. Both sides of a pair are
   rooted by a MARKED symbol, which AProVE spells as the uppercase of the symbol
   it came from -- the same convention {!pair_heads} reads -- and one
   substitution has to serve both sides, or the pair would be an instance of the
   dependency pair only rule-by-rule. *)
let instantiates ((pl, pt) : parsed * parsed) ((cl, ct) : parsed * parsed) :
    bool =
  match (pl, cl, pt, ct) with
  | PApp (f, fargs), PApp (f', fargs'), PApp (g, gargs), PApp (g', gargs')
    when upper f = f'
         && upper g = g'
         && List.length fargs = List.length fargs'
         && List.length gargs = List.length gargs' ->
      let b = List.fold_left2 matched (Some []) fargs fargs' in
      List.fold_left2 matched b gargs gargs' <> None
  | _ -> false

(* The dependency pairs of a system, indexed by the MARKED root of their left
   side (the spelling a child's pair is printed under, so the index is what a
   lookup keys on). Every defined-rooted subterm of a right-hand side counts,
   the whole right side included -- the plain Arts-Giesl definition, without the
   "not a proper subterm of the left side" refinement, since dropping a
   refinement can only make the set bigger and (a) is an inclusion INTO it. *)
let dependency_pairs ~(is_var : string -> bool) (rules : (string * string) list)
    : ((string, (parsed * parsed) list) Hashtbl.t, string) result =
  let defined : (string, unit) Hashtbl.t = Hashtbl.create 512 in
  List.iter
    (fun (lhs, _) ->
      match identifiers lhs with
      | h :: _ -> Hashtbl.replace defined h ()
      | [] -> ())
    rules;
  let index : (string, (parsed * parsed) list) Hashtbl.t = Hashtbl.create 512 in
  let rec collect key lhs = function
    | PApp (f, args) as t ->
        if Hashtbl.mem defined f then
          Hashtbl.replace index key
            ((lhs, t) :: Option.value (Hashtbl.find_opt index key) ~default:[]);
        List.iter (collect key lhs) args
    | PVar _ -> ()
  in
  let rec go = function
    | [] -> Ok index
    | (lhs, rhs) :: rest -> (
        match
          (parse_term ~is_var lhs, parse_term ~is_var rhs, identifiers lhs)
        with
        | Some l, Some r, h :: _ ->
            collect (upper h) l r;
            go rest
        | _ ->
            Error
              (Printf.sprintf "cannot parse the rule %S" (lhs ^ " -> " ^ rhs)))
  in
  go rules

let closed_by (r : reposed) (problem : problem)
    ~(rules : (string * string) list) : (unit, string) result =
  let declared : (string, unit) Hashtbl.t = Hashtbl.create 1024 in
  List.iter (fun v -> Hashtbl.replace declared v ()) r.vars;
  let is_var v = Hashtbl.mem declared v in
  let present : (string * string, unit) Hashtbl.t = Hashtbl.create 8192 in
  List.iter (fun rule -> Hashtbl.replace present rule ()) rules;
  (* [cepsilon] is checked alongside the usable rules rather than left to the
     caller's care: it is the side condition of the prune that produced them, so
     a system without it does not close the child however well it is proved. *)
  match
    List.find_opt
      (fun rule -> not (Hashtbl.mem present rule))
      (r.rules @ r.cepsilon)
  with
  | Some (lhs, rhs) ->
      Error
        (Printf.sprintf "the system is missing the rule %s"
           (lhs ^ " -> " ^ rhs))
  | None -> (
      match dependency_pairs ~is_var rules with
      | Error msg -> Error msg
      | Ok index -> (
          let covered (line : string) =
            match arrow_split line with
            | None -> false
            | Some (lhs, rhs) -> (
                match (parse_term ~is_var lhs, parse_term ~is_var rhs) with
                | Some l, Some rt -> (
                    match root l with
                    | None -> false
                    | Some key ->
                        List.exists
                          (fun dp -> instantiates dp (l, rt))
                          (Option.value
                             (Hashtbl.find_opt index key)
                             ~default:[]))
                | _ -> false)
          in
          match
            List.find_opt (fun line -> not (covered line)) problem.pairs
          with
          | Some line ->
              Error
                (Printf.sprintf
                   "the pair %S is not a dependency pair of the system" line)
          | None -> Ok ()))
