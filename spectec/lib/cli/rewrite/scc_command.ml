open Common

(* Per-symbol sufficient completeness via the CETA-enabled Maude 2.7 + old MFE
   2.7.1 backend ({!Rewrite.Scc}); the row format is the retired run-scc.sh's,
   byte-compatible so the two can be diffed. *)
let report_row sym (report : Rewrite.Scc.report) =
  let fid =
    (match report.fidelity with
    | Rewrite.Scc.Exact -> "exact"
    | Rewrite.Scc.Approx -> "approx")
    ^ (match report.analysis with Some a -> "/analysis:" ^ a | None -> "")
    ^
    match report.verdict with
    | Rewrite.Scc.Counterexample { domain; _ } ->
        "/" ^ Rewrite.Scc.string_of_domain domain
    | _ -> ""
  in
  match report.verdict with
  | Rewrite.Scc.Degenerate ->
      (Printf.sprintf "%s\tDEGENERATE\t%s" sym fid, false)
  | Rewrite.Scc.Complete -> (Printf.sprintf "%s\tCOMPLETE\t%s\t" sym fid, false)
  | Rewrite.Scc.Counterexample { witness; sort; _ } ->
      ( Printf.sprintf "%s\tCOUNTEREXAMPLE\t%s\t%s: %s" sym fid witness sort,
        true )
  | Rewrite.Scc.Timeout -> (Printf.sprintf "%s\tTIMEOUT\t%s\t" sym fid, true)
  | Rewrite.Scc.No_ceta ->
      (Printf.sprintf "%s\tERROR-NO-CETA\t%s\t" sym fid, true)
  | Rewrite.Scc.Error msg ->
      Printf.eprintf "scc: %s: %s\n" sym msg;
      (Printf.sprintf "%s\tERROR\t%s\t" sym fid, true)

let check ~filenames ~all ~symbols ~timeout ~ceta_bin ~mfe271_dir ~emit ~out =
  let* { Rewrite.spec = spec_tr; system } =
    load ~target:Rewrite.Analysis filenames
  in
  let sig_rules = system.Rewrite.Rewrite_system.rules in
  (* one head index shared across the whole sweep (see termination_command) *)
  let slicer = Rewrite.Rewrite_system.make_slicer system in
  let roots =
    (* every defined head, not just [def_symbols]: the derived predicates --
       subty_<T>, match_<T>_<K>, holds_<R> -- are the SCC's most valuable
       targets, and no DecD/RelD declares them *)
    Cli.Analysis_sweep.roots ~all ~symbols
      ~all_roots:(Rewrite.Rewrite_system.defined_heads system)
      ~slice_size:(slice_size slicer)
  in
  if emit then (
    let slice =
      Rewrite.Rewrite_system.slice_with slicer ~roots:[ List.hd roots ]
    in
    let uncond, _ = Rewrite.Scc.unconditional slice in
    print_string (Rewrite.Scc.module_text ~sig_rules spec_tr uncond);
    Ok false)
  else
    Ok
      (Cli.Analysis_sweep.rows ~out ~roots ~key:(slice_key slicer)
         ~row_of:(fun sym ->
           let slice =
             Rewrite.Rewrite_system.slice_with slicer ~roots:[ sym ]
           in
           let report : Rewrite.Scc.report =
             Rewrite.Scc.check ~timeout ?ceta_bin ?mfe271_dir ~sig_rules spec_tr
               slice
           in
           report_row sym report))

let command =
  Core.Command.basic
    ~summary:
      "check per-symbol sufficient completeness of the analysis CTRS (CETA \
       Maude 2.7 + old MFE SCC)"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and symbols = symbols_flag
  and all =
    flag "--all" no_arg
      ~doc:
        " check every defined head's slice (including the derived \
         subty_/match_/holds_ predicates), smallest slice first"
  and timeout =
    flag "--timeout"
      (optional_with_default 600 int)
      ~doc:"S kill the checker after S seconds per symbol (default 600)"
  and ceta_bin =
    flag "--ceta-maude-bin" (optional string)
      ~doc:"PATH path to the CETA-enabled maude 2.7 binary"
  and mfe271_dir =
    flag "--mfe271-dir" (optional string)
      ~doc:"DIR directory holding the old MFE 2.7.1 (bundles SCC 2a)"
  and emit =
    flag "--emit" no_arg
      ~doc:
        " print the pruned functional module the checker would see (exactly \
         one --symbol)"
  and out = out_flag in
  fun () ->
    Cli.Analysis_sweep.require_roots ~cmd:"scc" ~all ~symbols;
    if emit then
      Cli.Analysis_sweep.require_single_symbol ~flag:"--emit" ~all ~symbols;
    Cli.Error_handling.guard ~color ~on_ok:(fun failed -> if failed then exit 1)
    @@ fun () ->
    check ~filenames ~all ~symbols ~timeout ~ceta_bin ~mfe271_dir ~emit ~out
