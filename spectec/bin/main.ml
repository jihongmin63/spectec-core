open Spectec

let version = "0.1"
let ( let* ) = Result.bind

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a spec"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun spec_il ->
        Format.printf "%s\n" (Lang.Il.Print.string_of_spec spec_il))
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    Ok spec_il

let unparse_roundtrip filenames =
  let* spec_el = parse_spec_files filenames in
  let printed = Lang.El.Unparse.string_of_spec spec_el in
  let* spec_el' =
    parse_spec_source { filename = "<roundtrip>"; contents = printed }
  in
  if Lang.El.Eq.eq_spec spec_el spec_el' then Ok ()
  else
    Error
      (Error.RoundtripError
         ( Common.Source.no_region,
           "pretty-printed output did not reparse to the same AST" ))

let unparse_command =
  Core.Command.basic
    ~summary:"parse a spec and print it in canonical EL form (drops comments)"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and roundtrip =
    flag "-r" no_arg
      ~doc:
        " verify the pretty-printed output reparses to the same AST (prints \
         nothing on success)"
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    if roundtrip then
      Cli.Error_handling.guard_unit ~color (fun () ->
          unparse_roundtrip filenames)
    else
      Cli.Error_handling.guard ~color ~on_ok:(fun spec_el ->
          Format.printf "%s" (Lang.El.Unparse.string_of_spec spec_el))
      @@ fun () -> parse_spec_files filenames

let structure_command =
  Core.Command.basic ~summary:"structure a spec"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun spec_sl ->
        Format.printf "%s\n" (Lang.Sl.Print.string_of_spec spec_sl))
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_sl = structure spec_il in
    Ok spec_sl

let annotate_command =
  Core.Command.basic ~summary:"annotate a structured spec into PL form"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun spec_pl ->
        Format.printf "%s\n" (Pl.Print.string_of_spec spec_pl))
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_sl = structure spec_il in
    let henv = henv_of_el_spec spec in
    let henv = henv_with_il_spec henv spec_il in
    let spec_pl = annotate ~henv spec_sl |> shorten in
    Ok spec_pl

(* Walks [root] recursively and returns every file path under it whose
   basename ends in one of [exts]. Paths are returned relative to [root]. *)
let collect_files ~exts root =
  let rec walk acc dir =
    let entries = Sys.readdir dir in
    Array.sort String.compare entries;
    Array.fold_left
      (fun acc entry ->
        let path = Filename.concat dir entry in
        if Sys.is_directory path then walk acc path
        else if List.exists (Filename.check_suffix entry) exts then path :: acc
        else acc)
      acc entries
  in
  walk [] root |> List.rev

let splice_command =
  Core.Command.basic
    ~summary:"splice rendered spec text into AsciiDoc skeletons"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and input_dir =
    flag "-i" (required string)
      ~doc:"DIR directory of .adoc skeleton files (walked recursively)"
  and output_dir =
    flag "-o" (required string)
      ~doc:"DIR directory to write spliced output (mirrors input layout)"
  and missing_path =
    flag "--missing" (optional string)
      ~doc:"FILE write the unused-keys report to this path"
  and color = Cli.Cli_args.Output.color_flag in
  fun () ->
    Cli.Error_handling.guard ~color ~on_ok:(fun (spec_el, spec_pl) ->
        let inputs = collect_files ~exts:[ ".adoc" ] input_dir in
        let pairs =
          List.map
            (fun in_path ->
              let rel =
                let prefix_len = String.length input_dir + 1 in
                if
                  String.length in_path > prefix_len
                  && String.sub in_path 0 (String.length input_dir) = input_dir
                then
                  String.sub in_path prefix_len
                    (String.length in_path - prefix_len)
                else in_path
              in
              (in_path, Filename.concat output_dir rel))
            inputs
        in
        let report =
          Splice.Driver.run ~spec_el ~spec_pl
            ~source_entries:Splice.Registry.source
            ~prose_entries:Splice.Registry.prose ~filenames:pairs
        in
        match missing_path with
        | Some path ->
            let oc = open_out path in
            Fun.protect
              (fun () ->
                Out_channel.output_string oc (Splice.Report.to_string report))
              ~finally:(fun () -> Out_channel.close oc)
        | None -> ())
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_sl = structure spec_il in
    let henv = henv_of_el_spec spec in
    let henv = henv_with_il_spec henv spec_il in
    let spec_pl = annotate ~henv spec_sl |> shorten in
    Ok (spec, spec_pl)

(* Slice-size measure for {!Cli.Analysis_sweep.roots}'s smallest-first
   ordering, over the sweep's shared head index. *)
let slice_size slicer sym =
  List.length
    (Rewrite.Rewrite_system.slice_with slicer ~roots:[ sym ])
      .Rewrite.Rewrite_system.rules

(* The identity of the question a slice poses, for the sweeps' dedup
   ({!Cli.Analysis_sweep.rows}'s [key]). Every check the sweeps run --
   confluence, termination, sufficient completeness -- is a function of the
   slice alone, so equal slices have equal verdicts, and two symbols in one
   call-graph SCC always slice equal: reaching each other, they reach the same
   rules. [owise] is spelled into the digest because
   {!Rewrite.Rewrite_system.string_of_rule} drops it. *)
let slice_digest (sliced : Rewrite.Rewrite_system.t) : string =
  let open Rewrite.Rewrite_system in
  String.concat "\n"
    (String.concat "," sliced.vars
    :: List.map
         (fun r -> (if r.owise then "owise " else "") ^ string_of_rule r)
         sliced.rules)
  |> Digest.string |> Digest.to_hex

let slice_key slicer sym =
  slice_digest (Rewrite.Rewrite_system.slice_with slicer ~roots:[ sym ])

(* The signature is recovered from the WHOLE system even when the emitted
   module is a slice or otherwise transformed: a predicate's domain is the
   join of its call sites, and slicing/condition-dropping delete call sites,
   so recovering from the transformed rules would declare a narrower domain
   than the module that actually runs -- and an SCC verdict about that domain
   would be about a system nobody executes. *)
let whole_system_sig_rules (system : Rewrite.Rewrite_system.t) :
    Rewrite.Rewrite_system.rule list =
  system.Rewrite.Rewrite_system.rules

(* Translate an elaborated IL spec to Maude. The default output is the
   executable, order-sorted Maude module ([Rewrite.To_maude.module_of_spec]) --
   the surface [run] executes. [--ctrs] instead dumps the analysis CTRS
   (single-sort Full Maude system, the same text [verify] sends the MFE), a
   debug view of the translation stage. *)
let rewrite_command =
  Core.Command.basic ~summary:"translate a spec to an executable Maude module"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and ctrs =
    flag "--ctrs" no_arg
      ~doc:
        " dump the analysis CTRS (single-sort Full Maude system, what \
         confluence checks) instead of the executable module"
  and symbol =
    flag "--symbol" (optional string)
      ~doc:
        "NAME with --ctrs, dump only this function/relation's dependency slice \
         (the unit confluence checks per-symbol)"
  and relations_as_rules =
    flag "--relations-as-rules" no_arg
      ~doc:
        " keep relations as Maude rules (rl/crl) instead of equations for \
         input-moded ones"
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
  and wide_predicates =
    flag "--wide-predicate-domains" no_arg
      ~doc:
        " declare the match_/subty_/holds_/eqg predicates over the top sort \
         Val instead of the domain recovered from their use (the pre-fixpoint \
         behaviour; to bisect a regression back to that pass)"
  and slice_dir =
    flag "--slice-dir" (optional string)
      ~doc:
        "DIR with --ctrs, write EVERY symbol's slice to DIR/<symbol>.mod in \
         one translation instead of dumping one to stdout (the whole-spec \
         translation is the per-symbol cost of a sweep: ~50s each on p4)"
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
    Cli.Error_handling.guard ~color ~on_ok:(fun out -> Format.printf "%s\n" out)
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_tr = Rewrite.translated_spec spec_il in
    let predicates =
      if wide_predicates then Rewrite.Maude_sorts.Wide
      else Rewrite.Maude_sorts.Narrow
    in
    if wll_check then
      let system = Rewrite.rewrite_spec spec_tr in
      let syms =
        match symbol with
        | Some s -> [ s ]
        | None -> Rewrite.def_symbols spec_tr
      in
      let slicer = Rewrite.Rewrite_system.make_slicer system in
      Ok
        (String.concat "\n"
           (List.map
              (fun s ->
                let slice =
                  Rewrite.Rewrite_system.slice_with slicer ~roots:[ s ]
                in
                Printf.sprintf "%s\t%d\t%s" s
                  (List.length (Rewrite.Crc_surface.wll_violations slice))
                  (Rewrite.Crc_surface.string_of_strategy
                     (Rewrite.Crc_surface.select_strategy slice)))
              syms))
    else if list_symbols then
      let syms = Rewrite.def_symbols spec_tr in
      if not sizes then Ok (String.concat "\n" syms)
      else
        (* Slice rule counts in one elaboration -- the cheap proxy for which
           symbols' analysis is tractable (a small closure) vs which pull in the
           whole system (the typing relations -> critical-pair blowup). *)
        let system = Rewrite.rewrite_spec spec_tr in
        let rows =
          List.map
            (fun s ->
              ( List.length
                  (Rewrite.Rewrite_system.slice system ~roots:[ s ]).rules,
                s ))
            syms
        in
        let rows = List.sort compare rows in
        Ok
          (String.concat "\n"
             (List.map (fun (n, s) -> Printf.sprintf "%d\t%s" n s) rows))
    else if ctrs then
      let system = Rewrite.rewrite_spec spec_tr in
      let sig_rules = whole_system_sig_rules system in
      (* the module text, and whether [--unconditional] had to over-approximate
         this slice to get it past the SCC's drop-bad-eqs filter (a COMPLETE
         verdict proves nothing when it did) *)
      let emit sys =
        let sys' =
          if unconditional then
            Rewrite.Scc_surface.(linearize_lhs (drop_conds sys))
          else if crc_normalize then Rewrite.Crc_surface.crc_normalize sys
          else sys
        in
        let fidelity = if sys' = sys then "exact" else "approx" in
        ( Rewrite.To_mfe.module_of_system ~prune_signature ~predicates
            ~sig_rules spec_tr sys',
          fidelity )
      in
      match slice_dir with
      | Some dir ->
          (* every symbol the rules define, not just [def_symbols] (the spec's
             own functions/relations): the SCC's most valuable targets are the
             DERIVED predicates -- subty_<T>, match_<T>_<K>, holds_<R> -- which
             no [DecD]/[RelD] declares. *)
          let syms = Rewrite.Rewrite_system.defined_heads system in
          let fid = open_out (Filename.concat dir "_fidelity.tsv") in
          Fun.protect
            ~finally:(fun () -> Out_channel.close fid)
            (fun () ->
              List.iter
                (fun s ->
                  let sys = Rewrite.Rewrite_system.slice system ~roots:[ s ] in
                  let text, fidelity = emit sys in
                  let oc = open_out (Filename.concat dir (s ^ ".mod")) in
                  Fun.protect
                    (fun () -> Out_channel.output_string oc text)
                    ~finally:(fun () -> Out_channel.close oc);
                  Printf.fprintf fid "%s\t%s\n" s fidelity)
                syms);
          Ok (Printf.sprintf "%d slices written to %s" (List.length syms) dir)
      | None ->
          let system =
            match symbol with
            | Some name -> Rewrite.Rewrite_system.slice system ~roots:[ name ]
            | None -> system
          in
          Ok (fst (emit system))
    else
      Ok
        (Rewrite.To_maude.module_of_spec ~relations_as_rules ~predicates spec_tr)

(* Per-symbol confluence (Church-Rosser) and coherence of the analysis CTRS via
   the Maude Formal Environment ({!Rewrite.Mfe.check}, one Maude invocation for
   both, the module being purely equational since every SpecTecx relation is
   input-moded). Structured like {!termination_command}/{!scc_command}: a
   [--symbol] list or an [--all] sweep of the sliceable symbols, one TSV row per
   slice, resumable through [--out]. Whole-system CRC explodes on critical
   pairs, so there is no whole-system mode -- and with each definition's rules
   under their own head, [--all] over the slices is as sound as it. The listing
   of sliceable symbols moved to [rewrite --list-symbols]. *)
let confluence_command =
  Core.Command.basic
    ~summary:
      "check per-symbol confluence (Church-Rosser) and coherence of the \
       analysis CTRS via the MFE"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and symbols =
    flag "--symbol" (listed string)
      ~doc:"NAME check this function/relation's dependency slice (repeatable)"
  and all =
    flag "--all" no_arg
      ~doc:" check every spec-defined symbol's slice, smallest slice first"
  and timeout =
    flag "--timeout"
      (optional_with_default 60 int)
      ~doc:"S kill Maude after S seconds per symbol (default 60, 0 disables)"
  and maude_bin =
    flag "--maude-bin" (optional string) ~doc:"PATH path to the maude binary"
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
  and out =
    flag "--out" (optional string)
      ~doc:
        "TSV append each symbol's row here and skip symbols the file already \
         records (a resumable sweep)"
  in
  fun () ->
    Cli.Analysis_sweep.require_roots ~cmd:"confluence" ~all ~symbols;
    Cli.Error_handling.guard ~color ~on_ok:(fun failed -> if failed then exit 1)
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_tr = Rewrite.translated_spec spec_il in
    let system = Rewrite.rewrite_spec spec_tr in
    let sig_rules = whole_system_sig_rules system in
    let slicer = Rewrite.Rewrite_system.make_slicer system in
    let roots =
      Cli.Analysis_sweep.roots ~all ~symbols
        ~all_roots:(Rewrite.def_symbols spec_tr)
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
          match Hashtbl.find_opt representative (slice_digest slice) with
          | Some rep ->
              Hashtbl.replace followers rep
                (Option.value ~default:[] (Hashtbl.find_opt followers rep)
                @ [ sym ]);
              None
          | None ->
              Hashtbl.replace representative (slice_digest slice) sym;
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
    let oc =
      Option.map (fun p -> open_out_gen [ Open_append; Open_creat ] 0o644 p) out
    in
    let failed = ref false in
    (* Stream each symbol's row the moment it lands (so a sweep is observable and
       [--out] is resumable across a kill), instead of buffering the whole
       batch. *)
    (* The 4th column is the symbol's wall-clock seconds; for a normalized
       verdict it is base + normalize time (the total spent reaching it). *)
    let put line =
      print_endline line;
      Option.iter
        (fun oc ->
          output_string oc (line ^ "\n");
          flush oc)
        oc
    in
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
    Fun.protect
      ~finally:(fun () -> Option.iter close_out oc)
      (fun () ->
        (* YES rows stream out immediately; inconclusive rows wait for the
           normalize retry (they are the minority). [pending] keeps each held
           symbol's base result and base time. *)
        let pending = Hashtbl.create 16 in
        let _ =
          Rewrite.Mfe.check_batch ~timeout ?maude_bin ?mfe_dir
            ~prune_signature:true ~sig_rules
            ~on_result:(fun label (r : Rewrite.Mfe.result) secs ->
              if
                crc_normalize
                && (inconclusive r.church_rosser || inconclusive r.coherence)
              then Hashtbl.replace pending label (r, secs)
              else emit_base label r secs)
            spec_tr slices
        in
        (* Climb the normalization ladder: each rung re-checks only the symbols
           still inconclusive after the previous one, and a symbol leaves
           [pending] the moment a rung settles it. The upgrade is per rung, so
           trying another one never costs a verdict -- only time. *)
        let held = Hashtbl.create 16 in
        Hashtbl.iter (fun k v -> Hashtbl.replace held k v) pending;
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
                   let up o nn =
                     Rewrite.Mfe.upgrade ~original:o ~normalized:nn
                   in
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
                  max n
                    (List.length (Rewrite.Crc_surface.normalize_ladder slice))
                else n)
              0 slices
          in
          for depth = 0 to rungs - 1 do
            if Hashtbl.length held > 0 then rung depth
          done;
          (* nothing left to try: report what the base check found *)
          Hashtbl.iter
            (fun label ((b : Rewrite.Mfe.result), secs) ->
              emit_base label b secs)
            held));
    Ok !failed

(* Emit the spec as an executable Maude module and run a start term through a
   local Maude binary (see {!Rewrite.Maude_run}). [--emit] dumps the module
   without invoking Maude; otherwise [--start] supplies the term to run, by
   default with [reduce] (input-moded relations are equations; use [--search]
   with [--relations-as-rules] to explore non-deterministic rule rewriting). *)
let run_command =
  Core.Command.basic
    ~summary:"run a translated spec in Maude (emit a module and execute a term)"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and start =
    flag "--start" (optional string)
      ~doc:
        "TERM Maude start term to run (required unless --imp, --p4, or --emit)"
  and imp =
    flag "--imp" (listed string)
      ~doc:
        "FILE parse an impty program and run it (builds the start term; repeat \
         to batch several through one Maude invocation)"
  and p4 =
    flag "--p4" (listed string)
      ~doc:
        "FILE parse a P4 program and run it through Program_ok against the \
         loaded spec (builds the start term; repeat to batch several through \
         one Maude invocation)"
  and includes =
    flag "-i" (listed string) ~doc:"DIR P4 include path (with --p4)"
  and task =
    flag "--task"
      (optional_with_default "run" string)
      ~doc:"WHICH run | eval | check (impty start relation; default run)"
  and emit =
    flag "--emit" no_arg ~doc:" print the Maude module and exit (do not run)"
  and search =
    flag "--search" no_arg
      ~doc:" explore rules + equations (search) instead of reduce"
  and _reduce =
    flag "--reduce" no_arg
      ~doc:" evaluate with equations only (reduce); the default mode"
  and rewrite =
    flag "--rewrite" no_arg
      ~doc:
        " apply rules along one path (rewrite); deterministic semantics \
         without the search blow-up"
  and relations_as_rules =
    flag "--relations-as-rules" no_arg
      ~doc:
        " keep relations as Maude rules (rl/crl) instead of equations for \
         input-moded ones (pair with --search to explore non-determinism)"
  and wide_predicates =
    flag "--wide-predicate-domains" no_arg
      ~doc:
        " declare the match_/subty_/holds_/eqg predicates over the top sort \
         Val instead of the domain recovered from their use (to bisect a \
         regression back to that pass)"
  and bound =
    flag "--bound" (optional int)
      ~doc:"N cap the number of search solutions explored"
  and maude_bin =
    flag "--maude-bin" (optional string) ~doc:"PATH path to the maude binary"
  and timeout =
    flag "--timeout"
      (optional_with_default 0 int)
      ~doc:
        "S kill Maude after S seconds (0 = no limit, the default: a real \
         spec's module costs ~80s to internalize before the first program even \
         starts, so any fixed default silently turns a working run into a \
         TIMEOUT -- bound the run from the caller instead)"
  and check_p4 =
    flag "--check-p4" no_arg
      ~doc:
        " for each --p4 program also typecheck it with the interpreter and \
         compare the typing RESULT value with Maude's (result MATCH/MISMATCH)"
  in
  fun () ->
    (match (start, imp, p4, emit) with
    | None, [], [], false ->
        Format.eprintf
          "run needs --start TERM, --imp FILE, or --p4 FILE (or --emit to just \
           print the module)@.";
        exit 2
    | _ -> ());
    Cli.Error_handling.guard ~color ~on_ok:(fun (out, failed) ->
        Format.printf "%s\n" out;
        if failed then exit 1)
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_tr = Rewrite.translated_spec spec_il in
    (* Resolve the start terms to run, each labeled by its source: every impty
       program (--imp), every P4 program (--p4, run through Program_ok against
       the loaded spec), then a raw --start term. The labels survive into the
       batched output so each program's result stays identifiable. *)
    let sources =
      List.map
        (fun f ->
          ( f,
            `Other,
            fun () -> Targets_impty.Impty.maude_start_term ~task ~spec_il f ))
        imp
      @ List.map
          (fun f ->
            ( f,
              `P4 f,
              fun () -> Targets_p4.P4.maude_start_term ~includes ~spec_il f ))
          p4
      @
      match start with
      | Some t -> [ ("--start", `Other, fun () -> Ok t) ]
      | None -> []
    in
    (* Resolve each source INDEPENDENTLY: a P4 program that fails to produce a
       start term must NOT abort the whole invocation -- it becomes its own
       [Error] verdict so every other program in the batch still runs on the
       amortized path (the ~50k-line module is reflected/internalized ONCE per
       invocation, ~20s). Both failure shapes are handled: a clean [Error]
       result (e.g. a surface syntax error, which the [p4_16_errors] negatives
       are full of) AND a RAISED exception (e.g. a lexer error on a malformed
       integer literal), which the frontend does not always reflect into the
       result monad. *)
    let resolve_error_msg e =
      Diagnostic.Render.render_bag ~ansi:Diagnostic.Ansi.plain
        (Error.to_diagnostics e)
    in
    let resolved =
      List.map
        (fun (label, kind, build) ->
          let outcome =
            try build ()
            with exn ->
              Error (Error.UnhandledException (Printexc.to_string exn))
          in
          match outcome with
          | Ok term -> (label, kind, Ok term)
          | Error e -> (label, kind, Error (resolve_error_msg e)))
        sources
    in
    let predicates =
      if wide_predicates then Rewrite.Maude_sorts.Wide
      else Rewrite.Maude_sorts.Narrow
    in
    let system = Rewrite.maude_system spec_tr in
    let module_text =
      Rewrite.To_maude.module_of_system ~relations_as_rules ~predicates spec_tr
        system
    in
    if emit then Ok (module_text, false)
    else
      let mode =
        if search then Rewrite.Maude_run.Search bound
        else if rewrite then Rewrite.Maude_run.Rewrite
        else Rewrite.Maude_run.Reduce
      in
      let defined_heads = Rewrite.To_maude.maude_defined_heads system in
      let run_results =
        Rewrite.Maude_run.run_batch ?maude_bin ~timeout ~defined_heads ~mode
          ~module_text
          ~starts:
            (List.filter_map
               (fun (_, _, r) ->
                 match r with Ok t -> Some t | Error _ -> None)
               resolved)
          ()
      in
      (* Stitch the Maude results back onto the sources in order, dropping the
         resolution-failure verdict in where a program never produced a start
         term ([run_batch] returns exactly one result per runnable term,
         positionally). *)
      let rec stitch resolved run_results =
        match resolved with
        | [] -> []
        | (label, kind, Ok _) :: rest -> (
            match run_results with
            | r :: more -> (label, kind, r) :: stitch rest more
            | [] -> [])
        | (label, kind, Error msg) :: rest ->
            (label, kind, Rewrite.Maude_run.Error msg)
            :: stitch rest run_results
      in
      let outcomes = stitch resolved run_results in
      (* When [--check-p4], compare Maude's typing RESULT against the
         interpreter's for the same program: decode Maude's normal form back to
         an IL value ({!Rewrite.Of_maude}) and [Eq.eq_values] it against the
         interpreter's relation output. Returns the report line and whether it
         is a genuine mismatch (so a single-file run can exit non-zero). *)
      let string_of_values vs =
        String.concat ", " (List.map Lang.Il.Print.string_of_value vs)
      in
      let compare_p4 filename (result : Rewrite.Maude_run.result) :
          string * bool =
        match result with
        | Rewrite.Maude_run.Reduced term -> (
            let input =
              {
                Targets_p4.P4.Typecheck.includes;
                filename;
                expect = Spectec.Task.Positive;
              }
            in
            match
              Spectec.eval_task
                (module Targets_p4.P4.Typecheck)
                ~sl_mode:false ~spec_il input
            with
            | Error _ -> ("result: interp FAILED (not compared)", false)
            | Ok interp_vals -> (
                try
                  let interp_vals = Rewrite.Of_maude.canonicalize interp_vals in
                  let maude_vals =
                    Rewrite.Of_maude.canonicalize
                      (Rewrite.Of_maude.values_of_result spec_tr
                         ~rel:"Program_ok" ~system term)
                  in
                  if Lang.Il.Eq.eq_values interp_vals maude_vals then
                    ("result: MATCH", false)
                  else
                    ( Printf.sprintf
                        "result: MISMATCH\n  interp: %s\n  maude:  %s"
                        (string_of_values interp_vals)
                        (string_of_values maude_vals),
                      true )
                with Rewrite.Of_maude.Parse_error msg ->
                  (Printf.sprintf "result: decode error: %s" msg, false)))
        | _ -> ("result: not reduced (not compared)", false)
      in
      let render (_, kind, result) =
        let base = Rewrite.Maude_run.string_of_result result in
        match kind with
        | `P4 filename when check_p4 ->
            let line, mismatch = compare_p4 filename result in
            (base ^ "\n" ^ line, mismatch)
        | _ -> (base, false)
      in
      let rendered = List.map render outcomes in
      let failed =
        List.exists (fun (_, _, r) -> Rewrite.Maude_run.is_failure r) outcomes
        || List.exists snd rendered
      in
      (* A single start keeps the bare result line (golden/grep-stable); a batch
         labels each program's block so the results stay attributable. *)
      let out =
        match (outcomes, rendered) with
        | [ _ ], [ (body, _) ] -> body
        | _ ->
            List.map2
              (fun (label, _, _) (body, _) ->
                Printf.sprintf "=== %s ===\n%s" label body)
              outcomes rendered
            |> String.concat "\n"
      in
      Ok (out, failed)

(* Direct (non-reflective) reduce of the STRUCTURAL analysis module
   ({!Rewrite.rewrite_spec}/{!Rewrite.To_mfe}) -- the third oracle leg
   (todo.md "CTRS(구조적) differential"): unlike [run] (the [Native] execution
   module, {!Rewrite.To_maude}), this actually EXECUTES the surface
   {!Rewrite.Reflect.owise}/{!Rewrite.Crc_surface.fold_premise_binders} feed
   the MFE confluence/coherence checker, so a semantic bug those analysis-only
   passes introduced (previously invisible to byte-identical goldens + CRC/ChC
   verdicts alone) would show up here as a wrong decoded result. Mirrors
   [run]'s [--p4]/[--check-p4]/batching shape closely, but: no [--imp] task
   beyond parsing (impty's relation still comes from [--task]), no [--start]/
   [--search]/[--rewrite] (Reduce only -- {!Maude_run.run_batch_direct} does
   not support Search, and Reduce is this oracle's whole point), and the start
   term is built via {!Rewrite.To_mfe.start_app} (object-syntax text) instead
   of a per-target META-TERM. *)
let run_structural_command =
  Core.Command.basic
    ~summary:
      "directly reduce the STRUCTURAL analysis module (no META-TERM \
       reflection) -- a third oracle leg comparing the CRC/ChC analysis \
       surface's actual execution against the interpreter"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and imp =
    flag "--imp" (listed string)
      ~doc:
        "FILE parse an impty program and reduce it structurally (repeat to \
         batch several through one Maude invocation)"
  and wide_predicates =
    flag "--wide-predicate-domains" no_arg
      ~doc:
        " declare the match_/subty_/holds_/eqg predicates over the top sort \
         Val instead of the domain recovered from their use (to bisect a \
         regression back to that pass)"
  and p4 =
    flag "--p4" (listed string)
      ~doc:
        "FILE parse a P4 program and reduce it structurally through Program_ok \
         (repeat to batch several through one Maude invocation)"
  and includes =
    flag "-i" (listed string) ~doc:"DIR P4 include path (with --p4)"
  and task =
    flag "--task"
      (optional_with_default "check" string)
      ~doc:"WHICH run | eval | check (impty start relation; default check)"
  and check_p4 =
    flag "--check-p4" no_arg
      ~doc:
        " for each --p4 program also typecheck it with the interpreter and \
         compare the typing RESULT value with Maude's (result MATCH/MISMATCH)"
  and emit =
    flag "--emit" no_arg
      ~doc:" print the Maude structural module and exit (do not run)"
  and maude_bin =
    flag "--maude-bin" (optional string) ~doc:"PATH path to the maude binary"
  and timeout =
    flag "--timeout"
      (optional_with_default 0 int)
      ~doc:
        "S kill Maude after S seconds (0 = no limit, the default -- see \
         [run]'s --timeout)"
  in
  fun () ->
    (match (imp, p4, emit) with
    | [], [], false ->
        Format.eprintf
          "run-structural needs --imp FILE or --p4 FILE (or --emit to just \
           print the module)@.";
        exit 2
    | _ -> ());
    Cli.Error_handling.guard ~color ~on_ok:(fun (out, failed) ->
        Format.printf "%s\n" out;
        if failed then exit 1)
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_tr = Rewrite.translated_spec spec_il in
    let system = Rewrite.rewrite_spec spec_tr in
    let predicates =
      if wide_predicates then Rewrite.Maude_sorts.Wide
      else Rewrite.Maude_sorts.Narrow
    in
    let module_text =
      Rewrite.To_mfe.module_of_system ~full_maude:false ~predicates spec_tr
        system
    in
    if emit then Ok (module_text, false)
    else
      (* Every source resolves to its relation + argument values (not yet
         encoded): [To_mfe.start_app] below encodes against the ONE already-
         built [system], so a batch amortizes that (not the per-program cost
         anyway -- encoding is cheap; the module's op-signature lookups are the
         shared, memoized cost, same as the Native path). *)
      let sources =
        List.map
          (fun f ->
            ( f,
              `Other,
              fun () ->
                Result.map
                  (fun (rel, v) -> (rel, [ v ]))
                  (Targets_impty.Impty.parse_for_run ~task f) ))
          imp
        @ List.map
            (fun f ->
              ( f,
                `P4 f,
                fun () ->
                  Result.map
                    (fun v -> ("Program_ok", [ v ]))
                    (Targets_p4.P4.parse_for_run ~includes f) ))
            p4
      in
      let resolve_error_msg e =
        Diagnostic.Render.render_bag ~ansi:Diagnostic.Ansi.plain
          (Error.to_diagnostics e)
      in
      let resolved =
        List.map
          (fun (label, kind, build) ->
            let outcome =
              try build ()
              with exn ->
                Error (Error.UnhandledException (Printexc.to_string exn))
            in
            match outcome with
            | Ok (rel, vs) -> (label, kind, Ok (rel, vs))
            | Error e -> (label, kind, Error (resolve_error_msg e)))
          sources
      in
      let starts =
        List.filter_map
          (fun (_, _, r) ->
            match r with
            | Ok (rel, vs) ->
                Some (Rewrite.To_mfe.start_app spec_tr system rel vs)
            | Error _ -> None)
          resolved
      in
      let defined_heads =
        List.map Rewrite.Maude_ident.id
          (Rewrite.Rewrite_system.defined_heads system)
      in
      let run_results =
        Rewrite.Maude_run.run_batch_direct ?maude_bin ~timeout ~defined_heads
          ~mode:Rewrite.Maude_run.Reduce ~module_text ~starts ()
      in
      let rec stitch resolved run_results =
        match resolved with
        | [] -> []
        | (label, kind, Ok _) :: rest -> (
            match run_results with
            | r :: more -> (label, kind, r) :: stitch rest more
            | [] -> [])
        | (label, kind, Error msg) :: rest ->
            (label, kind, Rewrite.Maude_run.Error msg)
            :: stitch rest run_results
      in
      let outcomes = stitch resolved run_results in
      let string_of_values vs =
        String.concat ", " (List.map Lang.Il.Print.string_of_value vs)
      in
      let compare_p4 filename (result : Rewrite.Maude_run.result) :
          string * bool =
        match result with
        | Rewrite.Maude_run.Reduced term -> (
            let input =
              {
                Targets_p4.P4.Typecheck.includes;
                filename;
                expect = Spectec.Task.Positive;
              }
            in
            match
              Spectec.eval_task
                (module Targets_p4.P4.Typecheck)
                ~sl_mode:false ~spec_il input
            with
            | Error _ -> ("result: interp FAILED (not compared)", false)
            | Ok interp_vals -> (
                try
                  let interp_vals = Rewrite.Of_maude.canonicalize interp_vals in
                  let maude_vals =
                    Rewrite.Of_maude.canonicalize
                      (Rewrite.Of_maude.values_of_result spec_tr
                         ~rel:"Program_ok" ~system term)
                  in
                  if Lang.Il.Eq.eq_values interp_vals maude_vals then
                    ("result: MATCH", false)
                  else
                    ( Printf.sprintf
                        "result: MISMATCH\n  interp: %s\n  maude:  %s"
                        (string_of_values interp_vals)
                        (string_of_values maude_vals),
                      true )
                with Rewrite.Of_maude.Parse_error msg ->
                  (Printf.sprintf "result: decode error: %s" msg, false)))
        | _ -> ("result: not reduced (not compared)", false)
      in
      let render (_, kind, result) =
        let base = Rewrite.Maude_run.string_of_result result in
        match kind with
        | `P4 filename when check_p4 ->
            let line, mismatch = compare_p4 filename result in
            (base ^ "\n" ^ line, mismatch)
        | _ -> (base, false)
      in
      let rendered = List.map render outcomes in
      let failed =
        List.exists (fun (_, _, r) -> Rewrite.Maude_run.is_failure r) outcomes
        || List.exists snd rendered
      in
      let out =
        match (outcomes, rendered) with
        | [ _ ], [ (body, _) ] -> body
        | _ ->
            List.map2
              (fun (label, _, _) (body, _) ->
                Printf.sprintf "=== %s ===\n%s" label body)
              outcomes rendered
            |> String.concat "\n"
      in
      Ok (out, failed)

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
      output_string oc
        "node\tnames\tpairs\tqdp\tqdp_rules\tqdp_secs\tslice\tslice_rules\tslice_secs\tslice_bare\tslice_bare_secs\tprocessors\n";
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
        (* A re-ask the licence check refused has no verdict and no seconds:
           the reason stands in the verdict column, so a reader is never left
           reading a blank as "it did not close". *)
        let asked = function
          | Ok (a : Rewrite.Termination.attempt) ->
              Printf.sprintf "%s\t%d\t%.1f"
                (Rewrite.Termination.string_of_verdict a.verdict)
                a.rules a.secs
          | Error msg -> Printf.sprintf "UNLICENSED: %s\t0\t0.0" msg
        in
        let bare = function
          | Ok (a : Rewrite.Termination.attempt) ->
              Printf.sprintf "%s\t%.1f"
                (Rewrite.Termination.string_of_verdict a.verdict)
                a.secs
          | Error _ -> "-\t0.0"
        in
        let line =
          Printf.sprintf "%d\t%s\t%d\t%s\t%d\t%.1f\t%s\t%s\t%s" c.node
            (String.concat "," (declared @ synthesized))
            c.pairs
            (Rewrite.Termination.string_of_verdict c.qdp.verdict)
            c.qdp.rules c.qdp.secs (asked c.slice) (bare c.slice_bare)
            (processors c.qdp.proof)
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
                   c.qdp.verdict = Rewrite.Termination.Yes
                   ||
                   match c.slice with
                   | Ok a -> a.verdict = Rewrite.Termination.Yes
                   | Error _ -> false)
                 children)
          in
          Printf.eprintf "%d/%d children closed\n" closed (List.length children);
          Ok (closed < List.length children))

(* Per-symbol termination via the unravel + a direct
   AProVE run ({!Rewrite.Termination}) -- deliberately NOT through MTT (whose
   condition-variable unraveling and hard-coded inner budget block exactly
   these proofs; see CLAUDE.md). *)
let termination_command =
  Core.Command.basic
    ~summary:
      "prove per-symbol termination of the analysis CTRS (unravel + AProVE, no \
       MTT)"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and symbols =
    flag "--symbol" (listed string)
      ~doc:"NAME check this function/relation's dependency slice (repeatable)"
  and all =
    flag "--all" no_arg
      ~doc:" check every spec-defined symbol's slice, smallest slice first"
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
  and emit_trs =
    flag "--emit-trs" no_arg
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
  and reslice_children =
    flag "--reslice-children" no_arg
      ~doc:
        " on a child the standalone .qdp re-pose left open, ask it a SECOND \
         way: re-slice the source CTRS at the symbols its dependency pairs \
         name and prove that slice terminating as a plain TRS. Off by default \
         because what it buys is unproven -- measured one variable at a time \
         on $bin_shl's pow_int child, the .qdp and the slice carry the same 65 \
         rules and both TIMEOUT, and the 4s YES belongs to the slice without \
         the C-epsilon pair, which closes nothing. It costs one more run on \
         every child that does not close; measure it with --child-report \
         rather than turning it on for a sweep"
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
         declares them, pairs, then the same child asked THREE ways, and the \
         processors that closed the first). The three are: as a standalone \
         .qdp problem; as the source CTRS re-sliced at the symbols its pairs \
         name and proved as a plain TRS, which the check falls back to and \
         which reads UNLICENSED where the two inclusions that justify it do \
         not hold; and that same slice WITHOUT the C-epsilon pair, which is a \
         reference figure only (it closes nothing -- it says what the pair \
         costs). The check itself stops at the first child that does not \
         close, which is right for a verdict and is exactly what hides the \
         distribution a residue is read from (exactly one --symbol)"
  and child_budget =
    flag "--child-budget" (optional int)
      ~doc:
        "S AProVE's budget per child in --child-report (default: --budget). A \
         child that closes takes seconds; one that does not costs the whole of \
         this, so it is what sets the run's length"
  and out =
    flag "--out" (optional string)
      ~doc:
        "TSV append each symbol's row here and skip symbols the file already \
         records (a resumable sweep)"
  in
  fun () ->
    Cli.Analysis_sweep.require_roots ~cmd:"termination" ~all ~symbols;
    if emit_trs then
      Cli.Analysis_sweep.require_single_symbol ~flag:"--emit-trs" ~all ~symbols;
    if child_report <> None then
      Cli.Analysis_sweep.require_single_symbol ~flag:"--child-report" ~all
        ~symbols;
    Cli.Error_handling.guard ~color ~on_ok:(fun failed -> if failed then exit 1)
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_tr = Rewrite.translated_spec spec_il in
    let system = Rewrite.rewrite_spec spec_tr in
    (* one head index shared across the whole sweep -- [slice_with] then costs
       O(slice size) per symbol instead of rebuilding the index once per symbol *)
    let slicer = Rewrite.Rewrite_system.make_slicer system in
    let surface =
      if itrs then Rewrite.Termination.Integer else Rewrite.Termination.Plain
    in
    let roots =
      Cli.Analysis_sweep.roots ~all ~symbols
        ~all_roots:(Rewrite.def_symbols spec_tr)
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
        if emit_trs then (
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
                     ~split_children:(not no_child_split) ~reslice_children
                     ~integer_retry:(not no_integer_retry) ~surface
                     ~roots:[ sym ] slice
                 in
                 Option.iter
                   (fun dir -> write_proof_tree ~dir ~sym report.proof)
                   proof_dir;
                 let stats =
                   match report.stats with
                   | Some s -> Rewrite.Unravel.string_of_stats s
                   | None -> "-"
                 in
                 (* The 4th column is the wall clock of the ANSWERING path: for a
                budget-ladder run the answering rung alone (the rungs below it
                found nothing and are excluded, so this is what the verdict
                cost, not what finding it cost), for a child split every child
                run it made. ("-" when no AProVE ran -- DEGENERATE or an export
                error.) *)
                 let secs =
                   match report.secs with
                   | Some s -> Printf.sprintf "%.1f" s
                   | None -> "-"
                 in
                 (* The 5th column is every path the check took, in order, with its
                own verdict and seconds -- where a split's "48/75 closed, node
                911 TIMEOUT" survives a row whose verdict is the plain run's. *)
                 let paths =
                   match report.steps with
                   | [] -> "-"
                   | steps ->
                       String.concat " "
                         (List.map Rewrite.Termination.string_of_step steps)
                 in
                 ( Printf.sprintf "%s\t%s\t%s\t%s\t%s" sym
                     (Rewrite.Termination.string_of_verdict report.verdict)
                     stats secs paths,
                   match report.verdict with
                   | Rewrite.Termination.Yes | Rewrite.Termination.Degenerate ->
                       false
                   | _ -> true )))

(* Per-symbol sufficient completeness via the CETA-enabled Maude 2.7 + old MFE
   2.7.1 backend ({!Rewrite.Scc}); the row format is the retired run-scc.sh's,
   byte-compatible so the two can be diffed. *)
let scc_command =
  Core.Command.basic
    ~summary:
      "check per-symbol sufficient completeness of the analysis CTRS (CETA \
       Maude 2.7 + old MFE SCC)"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map filenames = anon (sequence ("spec files" %: string))
  and color = Cli.Cli_args.Output.color_flag
  and symbols =
    flag "--symbol" (listed string)
      ~doc:"NAME check this symbol's dependency slice (repeatable)"
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
  and out =
    flag "--out" (optional string)
      ~doc:
        "TSV append each symbol's row here and skip symbols the file already \
         records (a resumable sweep)"
  in
  fun () ->
    Cli.Analysis_sweep.require_roots ~cmd:"scc" ~all ~symbols;
    if emit then
      Cli.Analysis_sweep.require_single_symbol ~flag:"--emit" ~all ~symbols;
    Cli.Error_handling.guard ~color ~on_ok:(fun failed -> if failed then exit 1)
    @@ fun () ->
    let* spec = parse_spec_files filenames in
    let* spec_il = elaborate spec in
    let spec_tr = Rewrite.translated_spec spec_il in
    let system = Rewrite.rewrite_spec spec_tr in
    let sig_rules = whole_system_sig_rules system in
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
               Rewrite.Scc.check ~timeout ?ceta_bin ?mfe271_dir ~sig_rules
                 spec_tr slice
             in
             let fid =
               (match report.fidelity with
               | Rewrite.Scc.Exact -> "exact"
               | Rewrite.Scc.Approx -> "approx")
               ^ (match report.analysis with
                 | Some a -> "/analysis:" ^ a
                 | None -> "")
               ^
               match report.verdict with
               | Rewrite.Scc.Counterexample { domain; _ } ->
                   "/" ^ Rewrite.Scc.string_of_domain domain
               | _ -> ""
             in
             match report.verdict with
             | Rewrite.Scc.Degenerate ->
                 (Printf.sprintf "%s\tDEGENERATE\t%s" sym fid, false)
             | Rewrite.Scc.Complete ->
                 (Printf.sprintf "%s\tCOMPLETE\t%s\t" sym fid, false)
             | Rewrite.Scc.Counterexample { witness; sort; _ } ->
                 ( Printf.sprintf "%s\tCOUNTEREXAMPLE\t%s\t%s: %s" sym fid
                     witness sort,
                   true )
             | Rewrite.Scc.Timeout ->
                 (Printf.sprintf "%s\tTIMEOUT\t%s\t" sym fid, true)
             | Rewrite.Scc.No_ceta ->
                 (Printf.sprintf "%s\tERROR-NO-CETA\t%s\t" sym fid, true)
             | Rewrite.Scc.Error msg ->
                 Printf.eprintf "scc: %s: %s\n" sym msg;
                 (Printf.sprintf "%s\tERROR\t%s\t" sym fid, true)))

let command =
  let module P4 = Targets_p4.P4.Cli in
  let module Impty = Targets_impty.Impty.Cli in
  Core.Command.group ~summary:"SpecTec command line tools"
    [
      ("unparse", unparse_command);
      ("elab", elab_command);
      ("struct", structure_command);
      ("annotate", annotate_command);
      ("splice", splice_command);
      ("rewrite", rewrite_command);
      ("confluence", confluence_command);
      ("termination", termination_command);
      ("scc", scc_command);
      ("run", run_command);
      ("run-structural", run_structural_command);
      (P4.name, P4.command);
      (Impty.name, Impty.command);
    ]

let () = Command_unix.run ~version command
