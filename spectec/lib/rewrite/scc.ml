(* CETA/SCC bridge; the verdict tokens and their ORDER are calibrated against
   real runs of the old MFE 2.7.1 under Maude 2.7 (see scc.mli and
   tools/mfe/README.md):

   - A Maude without the CETA library bound refuses with "... not fully
     available. Please use the trust command to assume that module SPEC IS
     SUFFICIENTLY COMPLETE." -- match a bare success phrase and that refusal
     reads as a proof, so the refusal is tested FIRST.
   - The old Full Maude emits benign "no parse" warnings from its own source
     while loading under Maude 2.7, so an unscoped error test would report
     ERROR over a run that produced a verdict: the verdict tests come first,
     the error test after. *)

module R = Rewrite_system

type domain = Val_wide | Elem_erased | Narrow | Unknown

type verdict =
  | Complete
  | Counterexample of { witness : string; sort : string; domain : domain }
  | Degenerate
  | Timeout
  | No_ceta
  | Error of string

type fidelity = Exact | Approx

type report = {
  verdict : verdict;
  fidelity : fidelity;
  analysis : string option;
}

let string_of_domain = function
  | Val_wide -> "dom:Val-wide"
  | Elem_erased -> "dom:elem-erased"
  | Narrow -> "dom:narrow"
  | Unknown -> "dom:?"

let module_name = "SPEC"
let mfe271_entry = "src/mfe.maude"

let resolve_ceta_bin = function
  | Some path -> path
  | None -> (
      match Sys.getenv_opt "SPECTEC_CETA_MAUDE_BIN" with
      | Some path -> path
      | None -> (
          let candidates =
            [ "spectec/tools/maude27-ceta/maude"; "tools/maude27-ceta/maude" ]
          in
          match List.find_opt Sys.file_exists candidates with
          | Some path -> path
          | None -> "spectec/tools/maude27-ceta/maude"))

let resolve_mfe271_dir = function
  | Some dir -> Ok dir
  | None -> (
      match Sys.getenv_opt "SPECTEC_MFE271_DIR" with
      | Some dir -> Ok dir
      | None -> (
          let candidates =
            [
              "spectec/tools/mfe271/MFE-mfe-2.7.1"; "tools/mfe271/MFE-mfe-2.7.1";
            ]
          in
          match
            List.find_opt
              (fun d -> Sys.file_exists (Filename.concat d mfe271_entry))
              candidates
          with
          | Some dir -> Ok dir
          | None ->
              Error
                "old MFE 2.7.1 not found; pass --mfe271-dir or set \
                 SPECTEC_MFE271_DIR (see spectec/tools/mfe/README.md)"))

let unconditional (slice : R.t) : R.t * fidelity =
  let uncond = Scc_surface.linearize_lhs (Scc_surface.drop_conds slice) in
  (uncond, if uncond = slice then Exact else Approx)

let module_text ?sig_rules (orig : Lang.Il.spec) (uncond : R.t) : string =
  To_mfe.module_of_system ~module_name ~functional:true ~prune_signature:true
    ?sig_rules orig uncond

(* -------------------------------------------------------------------------- *)
(* Output scanning (over the whitespace-normalized output). *)

let index_of ?(from = 0) (s : string) (sub : string) : int option =
  let ns = String.length s and nb = String.length sub in
  let rec go i =
    if i + nb > ns then None
    else if String.sub s i nb = sub then Some i
    else go (i + 1)
  in
  go from

let last_index_of (s : string) (sub : string) : int option =
  let rec go acc from =
    match index_of ~from s sub with None -> acc | Some i -> go (Some i) (i + 1)
  in
  go None 0

let contains s sub = Subproc.contains s sub

let contains_ci (s : string) (sub : string) : bool =
  contains (String.lowercase_ascii s) (String.lowercase_ascii sub)

(* "no parse for <token>SPEC" with no space inside the token, case-insensitive
   -- the retired driver's grep -iE 'no parse for [^ ]*SPEC'. *)
let no_parse_for_spec (norm : string) : bool =
  let l = String.lowercase_ascii norm in
  let marker = "no parse for " in
  let rec scan from =
    match index_of ~from l marker with
    | None -> false
    | Some i ->
        let start = i + String.length marker in
        let stop =
          match index_of ~from:start l " " with
          | Some j -> j
          | None -> String.length l
        in
        if contains (String.sub l start (stop - start)) "spec" then true
        else scan (i + 1)
  in
  scan 0

(* The counterexample phrase is "<term> with sort <Sort> Freeness ..."; the
   witness keeps its raw spelling (trailing space included) so the rendered
   "<witness>: <Sort>" column is byte-compatible with the retired driver. *)
let witness_of (norm : string) : (string * string) option =
  let marker = "Completeness counter-examples: " in
  match last_index_of norm marker with
  | None -> None
  | Some i -> (
      let start = i + String.length marker in
      match index_of ~from:start norm "with sort " with
      | None -> None
      | Some ws ->
          let witness = String.sub norm start (ws - start) in
          let sstart = ws + String.length "with sort " in
          let send = ref sstart in
          let n = String.length norm in
          while
            !send < n
            &&
            match norm.[!send] with
            | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
            | _ -> false
          do
            incr send
          done;
          Some (witness, String.sub norm sstart (!send - sstart)))

(* The argument sorts of [head]'s op declaration in the emitted module, read
   back from the module text (the witness head is already in Maude spelling,
   which is how the declaration spells it too). *)
let op_domain (module_text : string) (head : string) : string list option =
  let prefix = "  op " ^ head ^ " : " in
  String.split_on_char '\n' module_text
  |> List.find_map (fun line ->
         if not (String.starts_with ~prefix line) then None
         else
           let rest =
             String.sub line (String.length prefix)
               (String.length line - String.length prefix)
           in
           match index_of rest "-> " with
           | None -> None
           | Some i ->
               Some
                 (String.sub rest 0 i |> String.split_on_char ' '
                 |> List.filter (fun t -> t <> "")))

let domain_of (module_text : string) (witness : string) : domain =
  let head =
    let upto =
      match String.index_opt witness '(' with
      | Some i -> String.sub witness 0 i
      | None -> witness
    in
    String.concat "" (String.split_on_char ' ' upto)
  in
  match op_domain module_text head with
  | None | Some [] -> Unknown
  | Some doms ->
      if List.mem "Val" doms then Val_wide
      else if List.mem "List" doms || List.mem "Opt" doms then Elem_erased
      else Narrow

let analysis_of_output (output : string) : string option =
  let norm = Subproc.normalize_ws output in
  let marker = "Analysis: it is " in
  match index_of norm marker with
  | None -> None
  | Some i -> (
      let start = i + String.length marker in
      let word from =
        let n = String.length norm in
        let stop = ref from in
        while
          !stop < n && match norm.[!stop] with 'a' .. 'z' -> true | _ -> false
        do
          incr stop
        done;
        (String.sub norm from (!stop - from), !stop)
      in
      let w1, after = word start in
      let mid = " and it is " in
      match index_of ~from:after norm mid with
      | Some j when j = after ->
          let w2, _ = word (after + String.length mid) in
          if w1 <> "" && w2 <> "" then Some (w1 ^ "+" ^ w2) else None
      | _ -> None)

let classify ~(module_text : string) (output : string) : verdict =
  let norm = Subproc.normalize_ws output in
  if contains norm "not fully available" then No_ceta
  else if contains norm "Completeness counter-examples: none were found" then
    Complete
  else if contains norm "Completeness counter-examples:" then
    match witness_of norm with
    | Some (witness, sort) ->
        Counterexample { witness; sort; domain = domain_of module_text witness }
    | None -> Counterexample { witness = ""; sort = ""; domain = Unknown }
  else if no_parse_for_spec norm || contains_ci norm "error" then
    Error "checker reported a parse/execution error"
  else Timeout

(* -------------------------------------------------------------------------- *)

let check ?(timeout = 600) ?ceta_bin ?mfe271_dir ?sig_rules
    (orig : Lang.Il.spec) (slice : R.t) : report =
  let uncond, fidelity = unconditional slice in
  if uncond.R.rules = [] then
    { verdict = Degenerate; fidelity; analysis = None }
  else
    match resolve_mfe271_dir mfe271_dir with
    | Error msg -> { verdict = Error msg; fidelity; analysis = None }
    | Ok mfe_dir ->
        let bin = resolve_ceta_bin ceta_bin in
        if not (Sys.file_exists bin) then
          {
            verdict =
              Error
                (bin
               ^ " not found; pass --ceta-maude-bin or set \
                  SPECTEC_CETA_MAUDE_BIN (see spectec/tools/mfe/README.md)");
            fidelity;
            analysis = None;
          }
        else
          let text = module_text ?sig_rules orig uncond in
          let mfe_path =
            Subproc.absolute (Filename.concat mfe_dir mfe271_entry)
          in
          let feed =
            Printf.sprintf "load %s\n%s%s\n" mfe_path text
              (String.concat "\n"
                 [ "(select tool SCC .)"; "(scc SPEC .)"; "q" ])
          in
          (* The old MFE's loop quits cleanly on [q], so the run reads to EOF;
             MAUDE_LIB points at the binary's own directory (its prelude), as
             the retired driver exported it. *)
          let env =
            let base = Unix.environment () in
            let lib = Filename.dirname (Subproc.absolute bin) in
            Array.of_list
              (("MAUDE_LIB=" ^ lib)
              :: (Array.to_list base
                 |> List.filter (fun kv ->
                        not (String.starts_with ~prefix:"MAUDE_LIB=" kv))))
          in
          let output, _timed_out =
            Subproc.run ~env ~cmd:[ bin; "-no-banner" ] ~feed ~timeout ()
          in
          {
            verdict = classify ~module_text:text output;
            fidelity;
            analysis = analysis_of_output output;
          }
