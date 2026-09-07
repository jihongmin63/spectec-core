open Common

(* Per-symbol confluence (Church-Rosser) and coherence of the analysis CTRS via
   the Maude Formal Environment ({!Rewrite.Mfe.check}, one Maude invocation for
   both, the module being purely equational since every SpecTecx relation is
   input-moded). Structured like {!termination_command}/{!scc_command}: a
   [--symbol] list or an [--all] sweep of the sliceable symbols, one TSV row per
   slice, resumable through [--out]. Whole-system CRC explodes on critical
   pairs, so there is no whole-system mode -- and with each definition's rules
   under their own head, [--all] over the slices is as sound as it. The listing
   of sliceable symbols moved to [rewrite --list-symbols]. *)
let check ~filenames ~all ~symbols ~timeout ~maude_bin ~mfe_dir ~crc_normalize
    ~out =
  let* { Rewrite.spec = spec_tr; system } =
    load ~target:Rewrite.Analysis filenames
  in
  let sig_rules = system.Rewrite.Rewrite_system.rules in
  let slicer = Rewrite.Rewrite_system.make_slicer system in
  let roots =
    Cli.Analysis_sweep.roots ~all ~symbols
      ~all_roots:(Rewrite.To_ctrs.def_symbols spec_tr)
      ~slice_size:(slice_size slicer)
  in
  let recorded =
    match out with
    | Some p -> Cli.Analysis_sweep.recorded_symbols p
    | None -> []
  in
  let todo = List.filter (fun s -> not (List.mem s recorded)) roots in
  (* One entry per DISTINCT slice, with the rest of each group parked in
     [followers]. The MFE is asked about a system, not a name, so symbols
     that slice equal -- every member of a call-graph SCC does -- would put
     the same question to it once each: 28 times over for the *_inst family,
     16 for the statement judgments. Their rows still land, copied from the
     representative's verdict and marked as copies. This mirrors
     {!Cli.Analysis_sweep.rows}'s [key]; confluence keeps its own loop for
     the normalization ladder. *)
  let followers : (string, string list) Hashtbl.t = Hashtbl.create 16 in
  let slices =
    let representative = Hashtbl.create 64 in
    List.filter_map
      (fun sym ->
        let slice = Rewrite.Rewrite_system.slice_with slicer ~roots:[ sym ] in
        let key = slice_digest slice in
        match Hashtbl.find_opt representative key with
        | Some rep ->
            Hashtbl.replace followers rep
              (Option.value ~default:[] (Hashtbl.find_opt followers rep)
              @ [ sym ]);
            None
        | None ->
            Hashtbl.replace representative key sym;
            Some (sym, slice))
      todo
  in
  let verdict = Rewrite.Mfe.string_of_verdict in
  (* What the normalization ladder is allowed to retry. [Error] belongs here:
     the MFE dying with no verdict at all (it can be SIGKILLed on a slice
     whose binder conditions carry free variables) is not a judgment about the
     system, it is the checker asking for a different encoding -- exactly what
     normalization supplies. Leaving it out skipped the retry on precisely the
     symbols that failed hardest. The transition stays upgrade-only, so
     admitting more rows here cannot cost a YES. *)
  let inconclusive = function
    | Rewrite.Mfe.Maybe | Rewrite.Mfe.Timeout | Rewrite.Mfe.Error _ -> true
    | _ -> false
  in
  let failed = ref false in
  Cli.Analysis_sweep.with_output ~out (fun put ->
      let emit label crc chc secs row_failed =
        put (Printf.sprintf "%s\t%s\t%s\t%.1f" label crc chc secs);
        List.iter
          (fun sym ->
            put (Printf.sprintf "%s\t%s\t%s\t%.1f\t=%s" sym crc chc secs label))
          (Option.value ~default:[] (Hashtbl.find_opt followers label));
        if row_failed then failed := true
      in
      let emit_base label (r : Rewrite.Mfe.result) secs =
        emit label (verdict r.church_rosser) (verdict r.coherence) secs
          (not
             (r.church_rosser = Rewrite.Mfe.Yes && r.coherence = Rewrite.Mfe.Yes))
      in
      (* YES rows stream out immediately; inconclusive rows wait for the
         normalize retry (they are the minority). [held] keeps each held
         symbol's base result and base time. *)
      let held = Hashtbl.create 16 in
      let _ =
        Rewrite.Mfe.check_batch ~timeout ?maude_bin ?mfe_dir
          ~prune_signature:true ~sig_rules
          ~on_result:(fun label (r : Rewrite.Mfe.result) secs ->
            if
              crc_normalize
              && (inconclusive r.church_rosser || inconclusive r.coherence)
            then Hashtbl.replace held label (r, secs)
            else emit_base label r secs)
          spec_tr slices
      in
      (* Climb the normalization ladder: each rung re-checks only the symbols
         still inconclusive after the previous one, and a symbol leaves
         [held] the moment a rung settles it. The upgrade is per rung, so
         trying another one never costs a verdict -- only time. *)
      let rung depth =
        let strategies = Hashtbl.create 16 in
        let norm_slices =
          List.filter_map
            (fun (sym, slice) ->
              match Hashtbl.find_opt held sym with
              | None -> None
              | Some _ -> (
                  match
                    List.nth_opt
                      (Rewrite.Crc_surface.normalize_ladder slice)
                      depth
                  with
                  | None -> None
                  | Some strategy ->
                      let ns =
                        Rewrite.Crc_surface.crc_normalize ~strategy slice
                      in
                      Hashtbl.replace strategies sym
                        (Rewrite.Crc_surface.string_of_strategy strategy);
                      if ns = slice then None else Some (sym, ns)))
            slices
        in
        if norm_slices <> [] then
          ignore
            (Rewrite.Mfe.check_batch ~timeout ?maude_bin ?mfe_dir
               ~prune_signature:true ~sig_rules
               ~on_result:(fun label (n : Rewrite.Mfe.result) n_secs ->
                 let (b : Rewrite.Mfe.result), b_secs =
                   Hashtbl.find held label
                 in
                 let up o nn = Rewrite.Mfe.upgrade ~original:o ~normalized:nn in
                 let c = up b.church_rosser n.church_rosser in
                 let h = up b.coherence n.coherence in
                 (* Settled: emit and drop. Still inconclusive: carry the
                    accumulated time to the next rung. *)
                 if c = Rewrite.Mfe.Yes && h = Rewrite.Mfe.Yes then (
                   let show v orig =
                     verdict v
                     ^
                     if v <> orig then
                       Printf.sprintf " (normalized:%s)"
                         (Hashtbl.find strategies label)
                     else ""
                   in
                   emit label (show c b.church_rosser) (show h b.coherence)
                     (b_secs +. n_secs) false;
                   Hashtbl.remove held label)
                 else Hashtbl.replace held label (b, b_secs +. n_secs))
               spec_tr norm_slices)
      in
      if crc_normalize then (
        (* Run every rung any held symbol has: a rung that is a no-op for one
           slice can still be the one that settles another. *)
        let rungs =
          List.fold_left
            (fun n (sym, slice) ->
              if Hashtbl.mem held sym then
                max n (List.length (Rewrite.Crc_surface.normalize_ladder slice))
              else n)
            0 slices
        in
        for depth = 0 to rungs - 1 do
          if Hashtbl.length held > 0 then rung depth
        done;
        (* nothing left to try: report what the base check found *)
        Hashtbl.iter
          (fun label ((b : Rewrite.Mfe.result), secs) -> emit_base label b secs)
          held));
  Ok !failed

let command =
  Core.Command.basic
    ~summary:
      "check per-symbol confluence (Church-Rosser) and coherence of the \
       analysis CTRS via the MFE"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and symbols = symbols_flag
  and all = all_flag
  and timeout =
    flag "--timeout"
      (optional_with_default 60 int)
      ~doc:"S kill Maude after S seconds per symbol (default 60, 0 disables)"
  and maude_bin = maude_bin_flag
  and mfe_dir =
    flag "--mfe-dir" (optional string)
      ~doc:"DIR directory holding the MFE (Full Maude + CRC/ChC loader)"
  and crc_normalize =
    flag "--crc-normalize" no_arg
      ~doc:
        " retry an inconclusive verdict (MAYBE/TIMEOUT) on the crc-normalized \
         system and upgrade it to YES only when the retry proves it \
         (upgrade-only, never a downgrade); an upgraded verdict prints as 'YES \
         (normalized:unravel)' or 'YES (normalized:inline)', naming which \
         normalization the slice's weak left-linearity allowed"
  and out = out_flag in
  fun () ->
    Cli.Analysis_sweep.require_roots ~cmd:"confluence" ~all ~symbols;
    Cli.Error_handling.guard ~color ~on_ok:(fun failed -> if failed then exit 1)
    @@ fun () ->
    check ~filenames ~all ~symbols ~timeout ~maude_bin ~mfe_dir ~crc_normalize
      ~out
