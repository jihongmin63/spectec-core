let ( let* ) = Result.bind

type 'r outcome =
  | Listing of string list
  | Unknown of string * string list
  | Single of 'r
  | Batch of (string * 'r) list

let run ~(check_system : Spectec.Rewrite_system.t -> 'r)
    ~(slice : Cli_args.Slice.t) (filenames : string list) :
    ('r outcome, Spectec.Error.t) result =
  let open Spectec in
  let* spec = parse_spec_files filenames in
  let* spec_il = elaborate spec in
  let syms = rewrite_symbols spec_il in
  if slice.list_symbols then Ok (Listing syms)
  else
    let spec_rw = rewrite spec_il in
    let check_slice roots =
      check_system (Rewrite_system.slice spec_rw ~roots)
    in
    match slice.symbol with
    | Some name when not (List.mem name syms) -> Ok (Unknown (name, syms))
    | Some name -> Ok (Single (check_slice [ name ]))
    | None when slice.whole -> Ok (Single (check_system spec_rw))
    | None ->
        (* Slice purely and sequentially, then run the (network-bound) checks
           concurrently, [slice.jobs] at a time. *)
        let sliced =
          List.map
            (fun s -> (s, Rewrite_system.slice spec_rw ~roots:[ s ]))
            syms
        in
        Ok
          (Batch
             (Parallel.map ~jobs:slice.jobs
                (fun (s, system) -> (s, check_system system))
                sliced))

let handle ~single ~batch = function
  | Listing syms -> List.iter print_endline syms
  | Unknown (name, syms) ->
      Format.eprintf "unknown symbol %s; known: %s@." name
        (String.concat " " syms);
      exit 2
  | Single r -> single r
  | Batch results -> batch results
