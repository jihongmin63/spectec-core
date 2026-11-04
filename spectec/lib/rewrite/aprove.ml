let proof_echo_prefix = "proof of "
let obligation_separator = "----------------------------------------"

let is_echo_start line =
  String.starts_with ~prefix:proof_echo_prefix (String.trim line)

let terminated_lines output =
  match String.rindex_opt output '\n' with
  | None -> []
  | Some last -> String.sub output 0 last |> String.split_on_char '\n'

let proof_tree (output : string) : string =
  let rec after_echo_start = function
    | [] -> []
    | line :: rest when is_echo_start line -> rest
    | _ :: rest -> after_echo_start rest
  in
  let rec upto_separator acc = function
    | [] -> List.rev acc
    | line :: _ when String.trim line = obligation_separator -> List.rev acc
    | line :: rest -> upto_separator (line :: acc) rest
  in
  let rec drop_blank = function
    | line :: rest when String.trim line = "" -> drop_blank rest
    | lines -> lines
  in
  let trim_blank lines =
    drop_blank lines |> List.rev |> drop_blank |> List.rev
  in
  String.split_on_char '\n' output
  |> after_echo_start |> upto_separator [] |> trim_blank |> String.concat "\n"

let proof_printed (output : string) : bool =
  let lines = terminated_lines output in
  List.exists is_echo_start lines
  && List.exists (fun line -> String.trim line = obligation_separator) lines

let parse output =
  let rec header = function
    | [] -> Error "no global YES/NO/MAYBE verdict before proof echo"
    | line :: _ when is_echo_start line ->
        Error "proof echo without a global verdict"
    | line :: rest -> (
        match String.trim line with
        | "KILLED" -> Ok Solver.Timeout
        | ("YES" | "NO" | "MAYBE") as token -> Solver.verdict_of_token token
        | _ -> header rest)
  in
  header (String.split_on_char '\n' output)

let adapter : Solver.adapter =
  {
    name = "aprove";
    variable = "SPECTEC_APROVE_BIN";
    candidates = [ "spectec/tools/aprove/runme"; "tools/aprove/runme" ];
    encode =
      (fun problem ->
        Ok { Solver.extension = ".trs"; contents = problem.Solver.tpdb });
    command =
      (fun ~executable ~input ~timeout ->
        [ executable; input; string_of_int timeout ]);
    parse;
    finished = proof_printed;
    proof = proof_tree;
    dependencies =
      (fun ~executable ->
        let jar = Filename.concat (Filename.dirname executable) "aprove.jar" in
        if Sys.file_exists jar then
          let z3 =
            Filename.concat
              (Filename.dirname (Filename.dirname executable))
              "z3/z3"
          in
          [ jar; "java"; (if Sys.file_exists z3 then z3 else "z3") ]
        else []);
  }
