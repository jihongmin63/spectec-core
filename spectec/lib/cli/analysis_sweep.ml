(* The per-symbol sweep plumbing the analysis subcommands ([confluence],
   [termination], [scc]) share: the usage guards, the slice-root selection,
   and the resumable [--out] TSV protocol. Domain-agnostic -- the caller
   supplies the slice-size measure and each row's contents -- so the module
   stays plain CLI machinery with no dependency on the rewrite library. *)

let require_roots ~(cmd : string) ~(all : bool) ~(symbols : string list) : unit
    =
  match (all, symbols) with
  | true, _ :: _ | false, [] ->
      Format.eprintf "%s needs --symbol NAME (repeatable) or --all@." cmd;
      exit 2
  | _ -> ()

let require_single_symbol ~(flag : string) ~(all : bool)
    ~(symbols : string list) : unit =
  if all || List.length symbols <> 1 then (
    Format.eprintf "%s takes exactly one --symbol@." flag;
    exit 2)

let roots ~(all : bool) ~(symbols : string list) ~(all_roots : string list)
    ~(slice_size : string -> int) : string list =
  if all then
    List.map (fun s -> (slice_size s, s)) all_roots
    |> List.sort compare |> List.map snd
  else symbols

let recorded_symbols (path : string) : string list =
  if not (Sys.file_exists path) then []
  else
    let ic = open_in path in
    let rec go acc =
      match input_line ic with
      | line -> (
          match String.index_opt line '\t' with
          | Some i -> go (String.sub line 0 i :: acc)
          | None -> go acc)
      | exception End_of_file -> acc
    in
    let acc = go [] in
    close_in ic;
    acc

let rows ~out ~roots ~row_of : bool =
  let recorded = match out with Some p -> recorded_symbols p | None -> [] in
  let oc =
    Option.map (fun p -> open_out_gen [ Open_append; Open_creat ] 0o644 p) out
  in
  Fun.protect
    ~finally:(fun () -> Option.iter close_out oc)
    (fun () ->
      List.fold_left
        (fun failed sym ->
          if List.mem sym recorded then failed
          else
            let line, row_failed = row_of sym in
            print_endline line;
            Option.iter
              (fun oc ->
                output_string oc (line ^ "\n");
                flush oc)
              oc;
            failed || row_failed)
        false roots)
