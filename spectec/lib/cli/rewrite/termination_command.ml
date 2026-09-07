open Common

(* One file per symbol under [--proof-dir], written as each row lands so an
   interrupted sweep keeps the trees it already has. Symbol names carry [$] and
   other punctuation the shell reads, so anything outside a bare filename
   alphabet becomes [_] -- a mangling, not a sanitization: it never collapses
   [$f] and [f] onto one path, since only the former grows a leading [_]. *)
let write_proof_tree ~(dir : string) ~(sym : string) (tree : string option) :
    unit =
  match tree with
  | None -> ()
  | Some tree ->
      if not (Sys.file_exists dir) then Core_unix.mkdir_p dir;
      let basename =
        String.map
          (fun c ->
            match c with
            | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '-' | '_' -> c
            | _ -> '_')
          sym
      in
      let oc = open_out (Filename.concat dir (basename ^ ".proof")) in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> output_string oc (tree ^ "\n"))

(* How a child of the dependency-graph decomposition gets a NAME.

   {!Rewrite.Qdp.pair_heads} says which symbols the child's dependency pairs
   recurse through, in the TPDB spelling the export uses -- which is two scrubs
   away from anything a person reads: [$f] prints as [d_f], and the CTRS names
   are already sanitized ([callableId'] -> [callableId_prime]). Only the spec
   can undo that, so the naming happens here, where it is loaded: TPDB spelling
   -> the slice's own CTRS symbol -> the declaration and the line that declares
   it ({!Rewrite.To_ctrs.symbol_origins}). A symbol a later pass invented has no
   declaration to find, and keeps its CTRS spelling. *)
let child_naming (spec_tr : Lang.Il.spec) (slice : Rewrite.Rewrite_system.t) :
    string -> string =
  let origins = Rewrite.To_ctrs.symbol_origins spec_tr in
  let of_tpdb = Hashtbl.create 512 in
  List.iter
    (fun sym -> Hashtbl.replace of_tpdb (Rewrite.Unravel.tpdb_sym sym) sym)
    (Rewrite.Rewrite_system.defined_heads slice);
  fun head ->
    match Hashtbl.find_opt of_tpdb head with
    | None -> head
    | Some sym -> (
        match Hashtbl.find_opt origins sym with
        | None -> sym
        | Some { Rewrite.To_ctrs.name; at } ->
            Printf.sprintf "%s@%s:%d" name
              (Filename.basename at.left.file)
              at.left.line)

(* [--child-report]: every open child measured, rows streamed as they land (the
   run takes an hour on a large slice, and a row that only exists at the end is
   a row nobody watches). *)
let write_child_report ~(path : string) ~(budget : int)
    ~(child_budget : int option) ?aprove_bin ~(naming : string -> string)
    (slice : Rewrite.Rewrite_system.t) =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      output_string oc "node\tnames\tpairs\tverdict\trules\tsecs\tprocessors\n";
      flush oc;
      let processors (proof : string) =
        String.split_on_char '\n' proof
        |> List.filter_map (fun line ->
               let line = String.trim line in
               match String.index_opt line ' ' with
               | None -> None
               | Some i -> (
                   let word =
                     String.sub line (i + 1) (String.length line - i - 1)
                   in
                   match String.index_opt word ' ' with
                   | None ->
                       if String.ends_with ~suffix:"Proof" word then Some word
                       else None
                   | Some j ->
                       let word = String.sub word 0 j in
                       if String.ends_with ~suffix:"Proof" word then Some word
                       else None))
        |> List.sort_uniq compare |> String.concat ","
      in
      let row (c : Rewrite.Termination.child) =
        (* Declared symbols first: a cycle also runs through helpers the
           translation synthesized ([$itermap_...]), which have no declaration
           to name and would otherwise bury the handful of names that do. *)
        let named = List.map naming c.heads in
        let declared, synthesized =
          List.partition (fun n -> String.contains n '@') named
        in
        let line =
          Printf.sprintf "%d\t%s\t%d\t%s\t%d\t%.1f\t%s" c.node
            (String.concat "," (declared @ synthesized))
            c.pairs
            (Rewrite.Termination.string_of_verdict c.qdp.verdict)
            c.qdp.rules c.qdp.secs (processors c.qdp.proof)
        in
        print_endline line;
        output_string oc (line ^ "\n");
        flush oc
      in
      match
        Rewrite.Termination.child_report ?aprove_bin ~budget ?child_budget
          ~on_child:row slice
      with
      | Error msg ->
          Printf.eprintf "child report: %s\n" msg;
          Ok true
      | Ok children ->
          let closed =
            List.length
              (List.filter
                 (fun (c : Rewrite.Termination.child) ->
                   c.qdp.verdict = Rewrite.Termination.Yes)
                 children)
          in
          Printf.eprintf "%d/%d children closed\n" closed (List.length children);
          Ok (closed < List.length children))

let report_row sym (report : Rewrite.Termination.report) =
  let stats =
    match report.stats with
    | Some s -> Rewrite.Unravel.string_of_stats s
    | None -> "-"
  in
  (* Answering-path time excludes unsuccessful lower budget rungs. *)
  let secs =
    match report.secs with Some s -> Printf.sprintf "%.1f" s | None -> "-"
  in
  (* Preserve failed path details even when a later path supplies the verdict. *)
  let paths =
    match report.steps with
    | [] -> "-"
    | steps ->
        String.concat " " (List.map Rewrite.Termination.string_of_step steps)
  in
  ( Printf.sprintf "%s\t%s\t%s\t%s\t%s" sym
      (Rewrite.Termination.string_of_verdict report.verdict)
      stats secs paths,
    match report.verdict with
    | Rewrite.Termination.Yes | Rewrite.Termination.Degenerate -> false
    | _ -> true )

let check ~filenames ~all ~symbols ~budget ~budget_from ~aprove_bin ~emit ~itrs
    ~itrs_lengths ~no_child_split ~no_integer_retry ~proof_dir ~child_report
    ~child_budget ~out =
  let* { Rewrite.spec = spec_tr; system } =
    load ~target:Rewrite.Analysis filenames
  in
  (* one head index shared across the whole sweep -- [slice_with] then costs
     O(slice size) per symbol instead of rebuilding the index once per symbol *)
  let slicer = Rewrite.Rewrite_system.make_slicer system in
  let surface =
    if itrs_lengths then Rewrite.Termination.Integer_lengths
    else if itrs then Rewrite.Termination.Integer
    else Rewrite.Termination.Plain
  in
  let roots =
    Cli.Analysis_sweep.roots ~all ~symbols
      ~all_roots:(Rewrite.To_ctrs.def_symbols spec_tr)
      ~slice_size:(slice_size slicer)
  in
  match child_report with
  | Some path ->
      let slice =
        Rewrite.Rewrite_system.slice_with slicer ~roots:[ List.hd roots ]
      in
      write_child_report ~path ~budget ~child_budget ?aprove_bin
        ~naming:(child_naming spec_tr slice)
        slice
  | None ->
      if emit then (
        let slice =
          Rewrite.Rewrite_system.slice_with slicer ~roots:[ List.hd roots ]
        in
        match
          Rewrite.Termination.export ~roots:[ List.hd roots ] surface slice
        with
        | Ok (trs, stats) ->
            print_string trs;
            Printf.eprintf "%s\n" (Rewrite.Unravel.string_of_stats stats);
            Ok false
        | Error msg ->
            Printf.eprintf "unravel: %s\n" msg;
            Ok true)
      else
        Ok
          (Cli.Analysis_sweep.rows ~out ~roots ~key:(slice_key slicer)
             ~row_of:(fun sym ->
               let slice =
                 Rewrite.Rewrite_system.slice_with slicer ~roots:[ sym ]
               in
               let report : Rewrite.Termination.report =
                 Rewrite.Termination.check ?aprove_bin ~budget ~budget_from
                   ~split_children:(not no_child_split)
                   ~integer_retry:(not no_integer_retry) ~surface ~roots:[ sym ]
                   slice
               in
               Option.iter
                 (fun dir -> write_proof_tree ~dir ~sym report.proof)
                 proof_dir;
               report_row sym report))

let command =
  Core.Command.basic
    ~summary:
      "prove per-symbol termination of the analysis CTRS (unravel + AProVE, no \
       MTT)"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and symbols = symbols_flag
  and all = all_flag
  and budget =
    flag "--budget"
      (optional_with_default 300 int)
      ~doc:
        "S AProVE's own proof budget per symbol (default 300); the process is \
         killed S+120s in"
  and budget_from =
    flag "--budget-from"
      (optional_with_default 5 int)
      ~doc:
        "S start the budget ladder at S instead of 5, skipping the rungs below \
         it -- over a stretch of slices already known to answer near the top \
         rung the climb is pure waste (425s per symbol before the 1280 rung). \
         The saving costs the ladder's guarantee: AProVE announces at its own \
         deadline, so a rung above a symbol's real cost is reported AS that \
         cost, and seconds measured this way are not comparable to \
         default-ladder rows"
  and aprove_bin =
    flag "--aprove-bin" (optional string)
      ~doc:"PATH path to the AProVE runme wrapper"
  and emit =
    flag "--emit" no_arg
      ~doc:
        " print the unraveled TRS instead of running AProVE (exactly one \
         --symbol)"
  and itrs =
    flag "--itrs" no_arg
      ~doc:
        " prove on the INTEGER-constraint surface ALONE: the same unraveling \
         with the structural arithmetic layer (binary-digit nats, \
         sign-magnitude ints, binary-indexed list ops, the two's-complement \
         codec) handed to AProVE as native integers and constraints. A \
         separate surface, not a tuning knob -- its verdicts are about a \
         differently-encoded system, so do not compare a column measured here \
         row-wise against a plain-TRS one. Without this flag the surface is \
         still tried, as the last path of a slice the plain one left unproven \
         and only where the slice computes arithmetic at all"
  and itrs_lengths =
    flag "--itrs-lengths" no_arg
      ~doc:
        " prove on the integer surface with the LIST layer lifted to lengths \
         as well: every list-sorted position becomes its length, so a \
         recursion that shortens a list ($strip_prefix_rec on drop(t, len p)) \
         has a measure the constraint solver reads. A model one step beyond \
         --itrs -- two lists of equal length are one term there -- and it \
         applies only where the lift covers the slice (a list destructured in \
         a pattern, or an operation with no closed form, is an export error). \
         Without this flag it is still tried, as the last path of a slice \
         --itrs left unproven"
  and no_child_split =
    flag "--no-child-split" no_arg
      ~doc:
        " on an unproven plain run, do NOT re-pose its open dependency-graph \
         children as standalone problems. The split is what separates a hard \
         child from one the shared deadline never reached (48 of Program_ok's \
         75 open children close standalone in a median 37s), but it costs one \
         more whole-system run, one run per open child, and a second run on \
         the child that does not close (the same child re-sliced from the CTRS \
         and proved as a plain TRS) -- turn it off to measure the plain \
         surface alone"
  and no_integer_retry =
    flag "--no-integer-retry" no_arg
      ~doc:
        " do not fall back to the integer surface on a slice the plain paths \
         left unproven"
  and proof_dir =
    flag "--proof-dir" (optional string)
      ~doc:
        "DIR also write each symbol's AProVE proof tree to DIR/<symbol>.proof \
         -- the step outline ((0) ITRS, (3) UsableRulesProof, (9) YES) that \
         says how far the proof got, which a one-word TIMEOUT row does not. \
         Free: the tree lands in the same instant as the verdict"
  and child_report =
    flag "--child-report" (optional string)
      ~doc:
        "TSV measure EVERY open dependency-graph child of the whole-system run \
         instead of checking the symbol -- one row per child (node, the \
         symbols its dependency pairs recurse through and where the spec \
         declares them, how many pairs, its verdict and seconds given the \
         budget alone, and the processors that closed it). The check itself \
         stops at the first child that does not close, which is right for a \
         verdict and is exactly what hides the distribution a residue is read \
         from (exactly one --symbol)"
  and child_budget =
    flag "--child-budget" (optional int)
      ~doc:
        "S AProVE's budget per child in --child-report (default: --budget). A \
         child that closes takes seconds; one that does not costs the whole of \
         this, so it is what sets the run's length"
  and out = out_flag in
  fun () ->
    Cli.Analysis_sweep.require_roots ~cmd:"termination" ~all ~symbols;
    require
      (not (itrs && itrs_lengths))
      "--itrs and --itrs-lengths are mutually exclusive";
    require
      ((not emit) || child_report = None)
      "--emit and --child-report are mutually exclusive";
    if emit then
      Cli.Analysis_sweep.require_single_symbol ~flag:"--emit" ~all ~symbols;
    if child_report <> None then
      Cli.Analysis_sweep.require_single_symbol ~flag:"--child-report" ~all
        ~symbols;
    Cli.Error_handling.guard ~color ~on_ok:(fun failed -> if failed then exit 1)
    @@ fun () ->
    check ~filenames ~all ~symbols ~budget ~budget_from ~aprove_bin ~emit ~itrs
      ~itrs_lengths ~no_child_split ~no_integer_retry ~proof_dir ~child_report
      ~child_budget ~out
