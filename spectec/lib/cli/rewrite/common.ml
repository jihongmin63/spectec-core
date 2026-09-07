open Spectec

let ( let* ) = Result.bind

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

let load_spec filenames =
  let* spec = parse_spec_files filenames in
  elaborate spec

let load ~target filenames =
  let* spec = load_spec filenames in
  Ok (Rewrite.compile ~target spec)

let usage_error message =
  Format.eprintf "%s@." message;
  exit 2

let require condition message = if not condition then usage_error message

let predicates_of_flag wide =
  if wide then Rewrite.Maude_sorts.Wide else Rewrite.Maude_sorts.Narrow

let maude_bin_flag =
  Core.Command.Param.(
    flag "--maude-bin" (optional string) ~doc:"PATH path to the maude binary")

let symbols_flag =
  Core.Command.Param.(
    flag "--symbol" (listed string)
      ~doc:"NAME check this symbol's dependency slice (repeatable)")

let all_flag =
  Core.Command.Param.(
    flag "--all" no_arg
      ~doc:" check every spec-defined symbol's slice, smallest slice first")

let out_flag =
  Core.Command.Param.(
    flag "--out" (optional string)
      ~doc:"TSV append rows here and skip recorded symbols when resuming")

let wide_predicates_flag =
  Core.Command.Param.(
    flag "--wide-predicate-domains" no_arg
      ~doc:
        " declare predicates over Val instead of recovering their domains from \
         use")

let relations_as_rules_flag =
  Core.Command.Param.(
    flag "--relations-as-rules" no_arg
      ~doc:
        " emit relations as Maude rules (rl/crl); use with --search or \
         --rewrite")
