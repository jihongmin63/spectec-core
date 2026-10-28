(* The per-symbol sweep plumbing the analysis subcommands ([confluence],
   [termination]) share: the usage guards, the slice-root selection,
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

let recorded_rows (path : string) : string list =
  if not (Sys.file_exists path) then []
  else
    let ic = open_in path in
    let rec go acc =
      match input_line ic with
      | line -> go (line :: acc)
      | exception End_of_file -> List.rev acc
    in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () -> go [])

let recorded_symbols (path : string) : string list =
  List.filter_map
    (fun line ->
      match String.index_opt line '\t' with
      | Some i -> Some (String.sub line 0 i)
      | None -> None)
    (recorded_rows path)

(* A row the sweep copied rather than measured names its source in a trailing
   [=<symbol>] column. It is appended, never substituted, so a reader that
   takes a fixed column (the verdict, the stats) sees the same thing it always
   did, and one that sums the wall clock can tell which rows to leave out --
   the seconds in a copied row are the source's, and no second run spent
   them. *)
let copy_marker = '='

(* A row split into its symbol, the columns after it, and the symbol whose run
   produced them (itself, unless the row is a copy). *)
let parse_row (line : string) : string * string * string =
  match String.split_on_char '\t' line with
  | [] | [ _ ] -> (line, "", line)
  | sym :: fields -> (
      match List.rev fields with
      | last :: rest when String.length last > 1 && last.[0] = copy_marker ->
          ( sym,
            String.concat "\t" (List.rev rest),
            String.sub last 1 (String.length last - 1) )
      | _ -> (sym, String.concat "\t" fields, sym))

let with_output ~out f =
  let oc =
    Option.map
      (fun path -> open_out_gen [ Open_append; Open_creat ] 0o644 path)
      out
  in
  Fun.protect
    ~finally:(fun () -> Option.iter close_out oc)
    (fun () ->
      let put line =
        print_endline line;
        Option.iter
          (fun oc ->
            output_string oc (line ^ "\n");
            flush oc)
          oc
      in
      f put)

let rows ~out ~roots ~key ~row_of : bool =
  let recorded = match out with Some p -> recorded_rows p | None -> [] in
  let recorded_syms =
    List.map
      (fun line ->
        let sym, _, _ = parse_row line in
        sym)
      recorded
  in
  (* [key] -> the columns of the row that answered it, the symbol that ran, and
     whether that row failed. The key is the whole input to the check, so two
     symbols sharing one is one question asked twice. *)
  let measured : (string, string * string * bool) Hashtbl.t =
    Hashtbl.create 64
  in
  (* Seeded from the rows already on disk so the saving survives a resume.
     Only symbols this sweep would visit are seeded: a row naming a symbol the
     spec no longer defines keys off an empty slice, and every such row would
     then answer for every other. A seeded row counts as not-failed, matching
     how a resumed sweep already treats everything it skips. *)
  List.iter
    (fun line ->
      let sym, tail, source = parse_row line in
      if List.mem sym roots then
        let k = key sym in
        if not (Hashtbl.mem measured k) then
          Hashtbl.replace measured k (tail, source, false))
    recorded;
  with_output ~out (fun put ->
      List.fold_left
        (fun failed sym ->
          if List.mem sym recorded_syms then failed
          else
            let k = key sym in
            match Hashtbl.find_opt measured k with
            | Some (tail, source, source_failed) ->
                put (Printf.sprintf "%s\t%s\t%c%s" sym tail copy_marker source);
                failed || source_failed
            | None ->
                let line, row_failed = row_of sym in
                let _, tail, _ = parse_row line in
                Hashtbl.replace measured k (tail, sym, row_failed);
                put line;
                failed || row_failed)
        false roots)
