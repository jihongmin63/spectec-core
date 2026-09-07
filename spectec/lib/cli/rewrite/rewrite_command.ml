open Common
module R = Rewrite.Rewrite_system

let list_symbols ~sizes spec =
  let symbols = Rewrite.def_symbols spec in
  if not sizes then String.concat "\n" symbols
  else
    let { Rewrite.system; _ } = Rewrite.compile ~target:Rewrite.Analysis spec in
    let slicer = R.make_slicer system in
    List.map (fun symbol -> (slice_size slicer symbol, symbol)) symbols
    |> List.sort compare
    |> List.map (fun (size, symbol) -> Printf.sprintf "%d\t%s" size symbol)
    |> String.concat "\n"

let wll_report ~symbol spec system =
  let symbols =
    match symbol with
    | Some s -> [ s ]
    | None -> Rewrite.To_ctrs.def_symbols spec
  in
  let slicer = R.make_slicer system in
  List.map
    (fun symbol ->
      let slice = R.slice_with slicer ~roots:[ symbol ] in
      Printf.sprintf "%s\t%d\t%s" symbol
        (List.length (Rewrite.Crc_surface.wll_violations slice))
        (Rewrite.Crc_surface.string_of_strategy
           (Rewrite.Crc_surface.select_strategy slice)))
    symbols
  |> String.concat "\n"

let emit_analysis ~symbol ~slice_dir ~unconditional ~crc_normalize
    ~prune_signature ~predicates spec system =
  (* Predicate domains must see every call site, even when a slice or an
     approximation removes some of them from the emitted rules. *)
  let sig_rules = system.R.rules in
  let slicer = R.make_slicer system in
  let emit slice =
    let transformed =
      if unconditional then
        Rewrite.Scc_surface.(linearize_lhs (drop_conds slice))
      else if crc_normalize then Rewrite.Crc_surface.crc_normalize slice
      else slice
    in
    let fidelity = if transformed = slice then "exact" else "approx" in
    ( Rewrite.To_mfe.module_of_system ~prune_signature ~predicates ~sig_rules
        spec transformed,
      fidelity )
  in
  match slice_dir with
  | None ->
      let slice =
        match symbol with
        | Some symbol -> R.slice_with slicer ~roots:[ symbol ]
        | None -> system
      in
      fst (emit slice)
  | Some dir ->
      let symbols = R.defined_heads system in
      let fid = open_out (Filename.concat dir "_fidelity.tsv") in
      Fun.protect
        ~finally:(fun () -> close_out fid)
        (fun () ->
          List.iter
            (fun symbol ->
              let text, fidelity =
                emit (R.slice_with slicer ~roots:[ symbol ])
              in
              let oc = open_out (Filename.concat dir (symbol ^ ".mod")) in
              Fun.protect
                ~finally:(fun () -> close_out oc)
                (fun () -> output_string oc text);
              Printf.fprintf fid "%s\t%s\n" symbol fidelity)
            symbols);
      Printf.sprintf "%d slices written to %s" (List.length symbols) dir

let translate ~filenames ~ctrs ~symbol ~relations_as_rules ~unconditional
    ~crc_normalize ~wll_check ~wide_predicates ~slice_dir ~prune_signature ~list
    ~sizes =
  let* spec = load_spec filenames in
  if list then Ok (list_symbols ~sizes spec)
  else
    let target =
      if ctrs || wll_check then Rewrite.Analysis else Rewrite.Execution
    in
    let { Rewrite.spec; system } = Rewrite.compile ~target spec in
    let predicates = predicates_of_flag wide_predicates in
    if wll_check then Ok (wll_report ~symbol spec system)
    else if ctrs then
      Ok
        (emit_analysis ~symbol ~slice_dir ~unconditional ~crc_normalize
           ~prune_signature ~predicates spec system)
    else
      Ok
        (Rewrite.To_maude.module_of_system ~relations_as_rules ~predicates spec
           system)

let command =
  Core.Command.basic ~summary:"translate a spec to an executable Maude module"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and ctrs =
    flag "--ctrs" no_arg
      ~doc:
        " dump the analysis CTRS (order-sorted Full Maude system, what \
         confluence checks) instead of the executable module"
  and symbol =
    flag "--symbol" (optional string)
      ~doc:
        "NAME with --ctrs, dump only this function/relation's dependency slice \
         (the unit confluence checks per-symbol)"
  and relations_as_rules = relations_as_rules_flag
  and unconditional =
    flag "--unconditional" no_arg
      ~doc:
        " with --ctrs, over-approximate for the SCC: drop rule conditions and \
         linearize non-left-linear lhs (counterexamples stay sound; a \
         'complete' verdict for a transformed symbol proves nothing)"
  and crc_normalize =
    flag "--crc-normalize" no_arg
      ~doc:
        " with --ctrs, normalize the slice for the Church-Rosser checker, \
         leaving no binder condition whose pattern is a free variable: unravel \
         the binders into crcu/crck chain operators where the slice is weakly \
         left-linear, inline them otherwise. The inline is meaning-preserving; \
         the unravel only REFLECTS confluence, so a normalized YES is \
         upgrade-only. Analysis-only."
  and wll_check =
    flag "--wll-check" no_arg
      ~doc:
        " report each sliceable symbol's weak-left-linearity violations and \
         the normalization --crc-normalize would pick for it, as \
         <symbol>\\t<violations>\\t<unravel|inline>"
  and wide_predicates = wide_predicates_flag
  and slice_dir =
    flag "--slice-dir" (optional string)
      ~doc:
        "DIR with --ctrs, write EVERY symbol's slice to DIR/<symbol>.mod in \
         one translation instead of dumping one to stdout"
  and prune_signature =
    flag "--prune-signature" no_arg
      ~doc:
        " with --ctrs, declare only the signature the rules use (applied ops, \
         their and the variable annotations' sorts, subsort-path interiors) \
         instead of the whole spec's; the rules are untouched, so checker \
         verdicts are preserved"
  and list_symbols =
    flag "--list-symbols" no_arg
      ~doc:
        " list the sliceable function/relation symbols (the names --symbol and \
         the confluence/termination/scc checkers take) and exit"
  and sizes =
    flag "--sizes" no_arg
      ~doc:
        " with --list-symbols, also print each symbol's slice rule count \
         (ascending) -- the cheap CRC tractability proxy"
  in
  fun () ->
    require ((not sizes) || list_symbols) "--sizes requires --list-symbols";
    require
      (not (list_symbols && wll_check))
      "--list-symbols and --wll-check are mutually exclusive";
    require
      (symbol = None || ctrs || wll_check)
      "--symbol requires --ctrs or --wll-check";
    require
      (slice_dir = None && (not unconditional) && (not crc_normalize)
       && not prune_signature
      || ctrs)
      "--slice-dir, --unconditional, --crc-normalize, and --prune-signature \
       require --ctrs";
    require
      (not (unconditional && crc_normalize))
      "--unconditional and --crc-normalize are mutually exclusive";
    require
      (slice_dir = None || symbol = None)
      "--slice-dir and --symbol are mutually exclusive";
    require
      ((not relations_as_rules) || not (ctrs || wll_check || list_symbols))
      "--relations-as-rules requires executable module output";
    Cli.Error_handling.guard ~color ~on_ok:(fun out -> Format.printf "%s\n" out)
    @@ fun () ->
    translate ~filenames ~ctrs ~symbol ~relations_as_rules ~unconditional
      ~crc_normalize ~wll_check ~wide_predicates ~slice_dir ~prune_signature
      ~list:list_symbols ~sizes
