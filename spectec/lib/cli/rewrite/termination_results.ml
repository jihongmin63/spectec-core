type verdict =
  | Answer of Rewrite.Solver.verdict
  | Degenerate
  | Failed of string

type row = {
  symbol : string;
  tool : string;
  verdict : verdict;
  stats : string;
  secs : float;
}

let failed row =
  match row.verdict with
  | Answer Rewrite.Solver.Yes | Degenerate -> false
  | _ -> true

let cell text = String.escaped text

let format row =
  let verdict, detail =
    match row.verdict with
    | Answer answer -> (Rewrite.Solver.string_of_verdict answer, "")
    | Degenerate -> ("DEGENERATE", "")
    | Failed message -> ("ERROR", message)
  in
  Printf.sprintf "%s\t%s\t%s\t%s\t%.3f\t%s" (cell row.symbol) (cell row.tool)
    verdict (cell row.stats) row.secs (cell detail)

let parse line =
  match String.split_on_char '\t' line with
  | [ symbol; tool; verdict; stats; secs; detail ] -> (
      try
        let secs = float_of_string secs in
        if (not (Float.is_finite secs)) || secs < 0. then
          Error "invalid elapsed time in termination TSV"
        else
          let answer =
            match verdict with
            | "DEGENERATE" when detail = "" -> Ok Degenerate
            | "ERROR" when detail <> "" -> Ok (Failed (Scanf.unescaped detail))
            | token when detail = "" ->
                Result.map
                  (fun answer -> Answer answer)
                  (Rewrite.Solver.verdict_of_token token)
            | _ -> Error "invalid verdict/detail in termination TSV"
          in
          Result.map
            (fun verdict ->
              {
                symbol = Scanf.unescaped symbol;
                tool = Scanf.unescaped tool;
                verdict;
                stats = Scanf.unescaped stats;
                secs;
              })
            answer
      with Failure message | Scanf.Scan_failure message ->
        Error ("invalid termination TSV: " ^ message))
  | _ -> Error "invalid termination TSV row: expected six tab-separated fields"

let read_rows ic identity =
  let length = in_channel_length ic in
  if length = 0 then Ok []
  else (
    seek_in ic (length - 1);
    if input_char ic <> '\n' then
      Error
        "incomplete final row in termination TSV; repair or use a new output \
         file"
    else (
      seek_in ic 0;
      let header = input_line ic in
      if header <> identity then
        Error "termination TSV configuration mismatch; use a new --out file"
      else
        let rec read rows =
          match input_line ic with
          | line ->
              Result.bind (parse line) (fun row ->
                  if
                    List.exists
                      (fun old ->
                        old.symbol = row.symbol && old.tool = row.tool)
                      rows
                  then Error "duplicate symbol/tool row in termination TSV"
                  else read (row :: rows))
          | exception End_of_file -> Ok (List.rev rows)
        in
        read []))

let execute ~recorded ~put ~jobs ~row_of =
  List.fold_left
    (fun failed_before (symbol, tool) ->
      match
        List.find_opt
          (fun row -> row.symbol = symbol && row.tool = tool)
          recorded
      with
      | Some row -> failed_before || failed row
      | None ->
          let row = row_of symbol tool in
          put (format row);
          failed_before || failed row)
    false jobs

let run ~out ~identity ~jobs ~row_of =
  let identity = "# spectec-termination-v1\t" ^ identity in
  match out with
  | None -> Ok (execute ~recorded:[] ~put:print_endline ~jobs ~row_of)
  | Some path ->
      let fd =
        Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_APPEND ] 0o644
      in
      let ic = Unix.in_channel_of_descr fd in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () ->
          (* Hold the file lock through the sweep, including validation and header
           creation, so two writers cannot interleave rows or settings. *)
          let lock =
            try
              Unix.lockf fd Unix.F_TLOCK 0;
              Ok ()
            with Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) ->
              Error ("termination TSV is already in use: " ^ path)
          in
          Result.bind lock (fun () ->
              Result.bind (read_rows ic identity) (fun recorded ->
                  let oc = Unix.out_channel_of_descr (Unix.dup fd) in
                  Fun.protect
                    ~finally:(fun () -> close_out oc)
                    (fun () ->
                      if (Unix.fstat fd).Unix.st_size = 0 then (
                        output_string oc (identity ^ "\n");
                        flush oc);
                      let put line =
                        output_string oc (line ^ "\n");
                        flush oc;
                        print_endline line
                      in
                      Ok (execute ~recorded ~put ~jobs ~row_of)))))
