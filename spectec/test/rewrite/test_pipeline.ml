module R = Rewrite.Rewrite_system

let unwrap = function
  | Ok value -> value
  | Error _ -> failwith "spec load failed"

let check message condition = if not condition then failwith message

let () =
  let spec =
    Spectec.parse_spec_files [ "../../specs/impty/base/spec.spectec" ]
    |> unwrap |> Spectec.elaborate |> unwrap
  in
  let native = Rewrite.compile ~target:Rewrite.Execution spec in
  let analysis = Rewrite.compile ~target:Rewrite.Analysis spec in
  check "compilations must share their specialized spec"
    (native.spec == analysis.spec);
  let roots = Rewrite.def_symbols spec in
  check "lookup must be specialized before choosing slice roots"
    (List.mem "$lookup_id_type" roots && not (List.mem "$lookup" roots));
  List.iter
    (fun (compiled : Rewrite.compilation) ->
      let heads = R.defined_heads compiled.system in
      check "every declared slice root must name emitted rules"
        (List.for_all (fun root -> List.mem root heads) roots);
      check "the returned spec must describe the emitted names"
        (Rewrite.To_ctrs.def_symbols compiled.spec = roots);
      check "condition orientation must survive compilation"
        (R.orient_conds compiled.system = compiled.system))
    [ native; analysis ];
  check "analysis must retain the structural arithmetic rules"
    (List.mem "badd" (R.defined_heads analysis.system));
  check "execution must delegate native arithmetic"
    (not (List.mem "badd" (R.defined_heads native.system)));
  let repeated = Rewrite.compile ~target:Rewrite.Execution native.spec in
  check "reusing a prepared spec must preserve its identity and rules"
    (repeated.spec == native.spec && repeated.system = native.system);
  print_endline "test_pipeline: all tests passed"
