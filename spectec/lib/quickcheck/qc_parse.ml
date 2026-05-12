open Common.Source
module P = Parse

(* --- utilities -------------------------------------------------------- *)

let read_lines (ic : in_channel) : string list =
  let rec aux acc =
    match input_line ic with
    | s -> aux (s :: acc)
    | exception End_of_file -> List.rev acc
  in
  aux []

(* Extract top-level "(…)" groups, handling nested parens. *)
let extract_paren_groups (s : string) : string list =
  let n = String.length s in
  let groups = ref [] in
  let i = ref 0 in
  while !i < n do
    if s.[!i] = '(' then begin
      let start = !i in
      let depth = ref 1 in
      incr i;
      while !i < n && !depth > 0 do
        (if s.[!i] = '(' then incr depth
         else if s.[!i] = ')' then decr depth);
        incr i
      done;
      groups := String.sub s start (!i - start) :: !groups
    end else incr i
  done;
  List.rev !groups

(* Parse "(id : typ_str)" into an ast_param. *)
let parse_param_group (grp : string) : (Qc_ast.ast_param, string) result =
  let inner =
    let len = String.length grp in
    if len >= 2 && grp.[0] = '(' && grp.[len - 1] = ')'
    then String.sub grp 1 (len - 2) |> String.trim
    else grp
  in
  match String.index_opt inner ':' with
  | None ->
    Error (Printf.sprintf "param declaration missing ':' in '%s'" grp)
  | Some colon ->
    let id_str  = String.sub inner 0 colon |> String.trim in
    let typ_str =
      String.sub inner (colon + 1) (String.length inner - colon - 1)
      |> String.trim
    in
    if id_str = "" then Error (Printf.sprintf "empty identifier in '%s'" grp)
    else
      match P.parse_plaintyp typ_str with
      | Error e -> Error (P.error_to_string e)
      | Ok plaintyp ->
        Ok { Qc_ast.p_id = id_str $ no_region; p_typ = plaintyp }

(* Parse all "(id : typ)" groups on a line. *)
let parse_params_line (line : string) :
    (Qc_ast.ast_param list, string) result =
  let groups = extract_paren_groups line in
  List.fold_right
    (fun r acc ->
      match r, acc with
      | Ok p, Ok ps -> Ok (p :: ps)
      | Error e, _ -> Error e
      | _, Error e -> Error e)
    (List.map parse_param_group groups)
    (Ok [])

(* Parse a "-- <prem_text>" line into an EL premise. *)
let parse_prem_line (line : string) : (Lang.El.prem, string) result =
  let text =
    let line = String.trim line in
    if String.length line >= 2 && String.sub line 0 2 = "--"
    then String.sub line 2 (String.length line - 2) |> String.trim
    else line
  in
  match P.parse_prem text with
  | Error e -> Error (P.error_to_string e)
  | Ok prem -> Ok prem

(* --- line classification ---------------------------------------------- *)

type line_kind =
  | L_Header of string   (* name after "quickcheck/" and before ":" *)
  | L_Param
  | L_Prem               (* starts with "--" *)
  | L_Goal               (* non-blank, non-param, non-prem, non-header *)
  | L_Blank

let classify (line : string) : line_kind =
  let s = String.trim line in
  match s with
  | "" -> L_Blank
  | _ ->
    let prefix = "quickcheck/" in
    let plen = String.length prefix in
    let slen = String.length s in
    if slen > plen && String.sub s 0 plen = prefix && s.[slen - 1] = ':' then
      let name = String.sub s plen (slen - plen - 1) |> String.trim in
      L_Header name
    else if s.[0] = '(' then L_Param
    else if slen >= 2 && String.sub s 0 2 = "--" then L_Prem
    else L_Goal

(* --- block parser ----------------------------------------------------- *)

(* Unified block parser: mode is determined by whether a goal (non-"--") line
   appears after the params section.  The first such line is the goal; a second
   one is an error. *)
let parse_block (name : string) (block_lines : string list) :
    (Qc_ast.ast_block, string) result =
  let rec collect_params params lines =
    match lines with
    | [] ->
      Ok { Qc_ast.name; params; goal = None; prems = [] }
    | line :: rest -> (
        match classify line with
        | L_Blank -> collect_params params rest
        | L_Param -> (
            match parse_params_line line with
            | Error e -> Error e
            | Ok ps -> collect_params (params @ ps) rest)
        | L_Header _ ->
          Error (Printf.sprintf "quickcheck/%s: unexpected nested header" name)
        | L_Prem | L_Goal ->
          collect_body params None [] (line :: rest))
  and collect_body params goal prems lines =
    match lines with
    | [] ->
      Ok { Qc_ast.name; params; goal; prems = List.rev prems }
    | line :: rest -> (
        match classify line with
        | L_Blank -> collect_body params goal prems rest
        | L_Header _ ->
          Error (Printf.sprintf "quickcheck/%s: unexpected nested header" name)
        | L_Param ->
          Error (Printf.sprintf "quickcheck/%s: param declaration after body not allowed" name)
        | L_Prem -> (
            match parse_prem_line line with
            | Error e -> Error e
            | Ok p -> collect_body params goal (p :: prems) rest)
        | L_Goal -> (
            match goal with
            | Some _ ->
              Error (Printf.sprintf "quickcheck/%s: more than one goal line" name)
            | None -> (
                match P.parse_prem (String.trim line) with
                | Error e -> Error (P.error_to_string e)
                | Ok g -> collect_body params (Some g) prems rest)))
  in
  collect_params [] block_lines

(* --- top-level -------------------------------------------------------- *)

(* Group lines into (name, body_lines) pairs. *)
let split_into_blocks (lines : string list) :
    (string * string list) list =
  let rec aux current_name acc result lines =
    match lines with
    | [] ->
      let result =
        match current_name with
        | None -> result
        | Some name -> (name, List.rev acc) :: result
      in
      List.rev result
    | line :: rest -> (
        match classify line with
        | L_Header new_name ->
          let result =
            match current_name with
            | None -> result
            | Some name -> (name, List.rev acc) :: result
          in
          aux (Some new_name) [] result rest
        | _ -> aux current_name (line :: acc) result rest)
  in
  aux None [] [] lines

let parse_file (path : string) : (Qc_ast.ast_file, string) result =
  match open_in path with
  | exception Sys_error msg ->
    Error (Printf.sprintf "quickcheck: cannot open '%s': %s" path msg)
  | ic ->
    Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
    let lines = read_lines ic in
    let blocks = split_into_blocks lines in
    if blocks = [] then
      Error (Printf.sprintf "quickcheck: no blocks found in '%s'" path)
    else
      List.fold_right
        (fun (name, body) acc ->
          match acc with
          | Error _ -> acc
          | Ok bs ->
            (match parse_block name body with
             | Error e -> Error e
             | Ok b -> Ok (b :: bs)))
        blocks (Ok [])
