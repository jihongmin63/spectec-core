open Common.Source

module Il = Lang.Il
module Value = Il.Value
module Typ = Il.Typ
module Atom = Lang.Xl.Atom

type parser = { input : string; mutable index : int }

exception Parse_error of int * string

let unknown_typ = Typ.var "value" [] $ no_region

let fail parser message = raise (Parse_error (parser.index, message))

let is_space = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_continue = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let length parser = String.length parser.input
let is_eof parser = parser.index >= length parser

let current_char parser =
  if is_eof parser then None else Some parser.input.[parser.index]

let rec skip_spaces parser =
  match current_char parser with
  | Some ch when is_space ch ->
      parser.index <- parser.index + 1;
      skip_spaces parser
  | _ -> ()

let consume_char parser =
  match current_char parser with
  | Some ch ->
      parser.index <- parser.index + 1;
      ch
  | None -> fail parser "unexpected end of input"

let expect_char parser expected =
  skip_spaces parser;
  match current_char parser with
  | Some ch when Char.equal ch expected -> parser.index <- parser.index + 1
  | Some ch ->
      fail parser
        (Printf.sprintf "expected '%c' but found '%c'" expected ch)
  | None -> fail parser (Printf.sprintf "expected '%c'" expected)

let maybe_char parser expected =
  skip_spaces parser;
  match current_char parser with
  | Some ch when Char.equal ch expected ->
      parser.index <- parser.index + 1;
      true
  | _ -> false

let has_prefix parser prefix =
  let prefix_len = String.length prefix in
  parser.index + prefix_len <= length parser
  && String.sub parser.input parser.index prefix_len = prefix

let consume_keyword parser keyword =
  skip_spaces parser;
  if not (has_prefix parser keyword) then false
  else
    let next_index = parser.index + String.length keyword in
    let has_boundary =
      next_index >= length parser
      || not (is_ident_continue parser.input.[next_index])
    in
    if has_boundary then (
      parser.index <- next_index;
      true)
    else false

let parse_identifier parser =
  skip_spaces parser;
  match current_char parser with
  | Some ch when is_ident_start ch ->
      let start = parser.index in
      parser.index <- parser.index + 1;
      while
        parser.index < length parser
        && is_ident_continue parser.input.[parser.index]
      do
        parser.index <- parser.index + 1
      done;
      String.sub parser.input start (parser.index - start)
  | Some ch ->
      fail parser
        (Printf.sprintf "expected identifier but found '%c'" ch)
  | None -> fail parser "expected identifier"

let parse_atom parser =
  let name = parse_identifier parser in
  Atom.Atom name $ no_region

let parse_id parser = parse_identifier parser $ no_region

let parse_string_literal parser =
  skip_spaces parser;
  if not (maybe_char parser '"') then fail parser "expected string literal";
  let buffer = Buffer.create 16 in
  let rec loop () =
    match consume_char parser with
    | '"' -> Buffer.contents buffer
    | '\\' -> (
        match consume_char parser with
        | '\\' ->
            Buffer.add_char buffer '\\';
            loop ()
        | '"' ->
            Buffer.add_char buffer '"';
            loop ()
        | 'n' ->
            Buffer.add_char buffer '\n';
            loop ()
        | 'r' ->
            Buffer.add_char buffer '\r';
            loop ()
        | 't' ->
            Buffer.add_char buffer '\t';
            loop ()
        | ch ->
            fail parser
              (Printf.sprintf "unsupported escape sequence \\%c" ch))
    | ch ->
        Buffer.add_char buffer ch;
        loop ()
  in
  loop ()

let parse_number_literal parser =
  skip_spaces parser;
  let sign =
    match current_char parser with
    | Some ('+' | '-') as ch ->
        parser.index <- parser.index + 1;
        ch
    | _ -> None
  in
  let start_digits = parser.index in
  while parser.index < length parser && is_digit parser.input.[parser.index] do
    parser.index <- parser.index + 1
  done;
  if parser.index = start_digits then fail parser "expected number literal";
  let digits =
    String.sub parser.input start_digits (parser.index - start_digits)
  in
  match sign with
  | Some '-' -> `Int (Bigint.of_string ("-" ^ digits))
  | Some '+' -> `Int (Bigint.of_string digits)
  | _ -> `Nat (Bigint.of_string digits)

let typ_of_value (value : Il.value) = value.note.typ $ no_region

let typ_of_values (values : Il.value list) =
  match values with
  | [] -> unknown_typ
  | value :: rest ->
      let typ = value.note.typ in
      if List.for_all (fun (v : Il.value) -> v.note.typ = typ) rest then
        typ $ no_region
      else unknown_typ

let rec parse_values parser close_char =
  let rec loop acc =
    skip_spaces parser;
    if maybe_char parser close_char then List.rev acc
    else
      let value = parse_value parser in
      skip_spaces parser;
      if maybe_char parser ',' || maybe_char parser ';' then loop (value :: acc)
      else (
        expect_char parser close_char;
        List.rev (value :: acc))
  in
  loop []

and parse_tuple parser =
  expect_char parser '(';
  let values = parse_values parser ')' in
  Value.tuple values

and parse_list parser =
  expect_char parser '[';
  let values = parse_values parser ']' in
  Value.list (typ_of_values values) values

and parse_option parser =
  if consume_keyword parser "None" then Value.opt unknown_typ None
  else if consume_keyword parser "Some" then (
    expect_char parser '(';
    let value = parse_value parser in
    expect_char parser ')';
    Value.opt (typ_of_value value) (Some value))
  else if consume_keyword parser "OptV" then (
    let parse_payload () =
      if consume_keyword parser "None" then Value.opt unknown_typ None
      else if consume_keyword parser "Some" then (
        expect_char parser '(';
        let value = parse_value parser in
        expect_char parser ')';
        Value.opt (typ_of_value value) (Some value))
      else fail parser "expected None or Some(...) after OptV"
    in
    if maybe_char parser '(' then (
      let value = parse_payload () in
      expect_char parser ')';
      value)
    else parse_payload ())
  else fail parser "expected option value"

and parse_struct_field parser =
  let atom = parse_atom parser in
  skip_spaces parser;
  let value =
    if maybe_char parser '=' || maybe_char parser ':' then parse_value parser
    else parse_value parser
  in
  (atom, value)

and parse_struct parser =
  expect_char parser '{';
  let rec loop acc =
    skip_spaces parser;
    if maybe_char parser '}' then Value.record "struct" (List.rev acc)
    else
      let field = parse_struct_field parser in
      skip_spaces parser;
      if maybe_char parser ';' || maybe_char parser ',' then loop (field :: acc)
      else (
        expect_char parser '}';
        Value.record "struct" (List.rev (field :: acc)))
  in
  loop []

and parse_func parser =
  let id =
    if maybe_char parser '$' then parse_id parser
    else if consume_keyword parser "FuncV" || consume_keyword parser "Func" then (
      if maybe_char parser '(' then (
        let value = parse_id parser in
        expect_char parser ')';
        value)
      else parse_id parser)
    else fail parser "expected function value"
  in
  Value.func id

and parse_case parser =
  let parse_case_payload () =
    let tag = parse_identifier parser in
    skip_spaces parser;
    let values =
      if maybe_char parser ',' then (
        skip_spaces parser;
        expect_char parser '[';
        parse_values parser ']')
      else []
    in
    Value.Make.case (Typ.var "case" []) ([ [ Atom.Atom tag $ no_region ] ], values)
  in
  if consume_keyword parser "CaseV" || consume_keyword parser "Case" then (
    if maybe_char parser '(' then (
      let value = parse_case_payload () in
      expect_char parser ')';
      value)
    else parse_case_payload ())
  else fail parser "expected case value"

and parse_bool parser =
  if consume_keyword parser "true" then Value.bool true
  else if consume_keyword parser "false" then Value.bool false
  else if consume_keyword parser "BoolV" then (
    let parse_payload () =
      if consume_keyword parser "true" then true
      else if consume_keyword parser "false" then false
      else fail parser "expected true or false after BoolV"
    in
    let value =
      if maybe_char parser '(' then (
        let b = parse_payload () in
        expect_char parser ')';
        b)
      else parse_payload ()
    in
    Value.bool value)
  else fail parser "expected boolean value"

and parse_number parser =
  let number =
    if consume_keyword parser "NumV" then (
      let parse_payload () = parse_number_literal parser in
      if maybe_char parser '(' then (
        let n = parse_payload () in
        expect_char parser ')';
        n)
      else parse_payload ())
    else parse_number_literal parser
  in
  match number with `Nat n -> Value.nat n | `Int i -> Value.int i

and parse_text parser =
  let text =
    if consume_keyword parser "TextV" then (
      let parse_payload () = parse_string_literal parser in
      if maybe_char parser '(' then (
        let s = parse_payload () in
        expect_char parser ')';
        s)
      else parse_payload ())
    else parse_string_literal parser
  in
  Value.text text

and parse_wrapped_collection parser name parse_inner =
  if consume_keyword parser name then (
    let value =
      if maybe_char parser '(' then (
        let parsed = parse_inner parser in
        expect_char parser ')';
        parsed)
      else parse_inner parser
    in
    Some value)
  else None

and parse_value parser =
  skip_spaces parser;
  match current_char parser with
  | None -> fail parser "expected value"
  | Some '"' -> parse_text parser
  | Some '[' -> parse_list parser
  | Some '{' -> parse_struct parser
  | Some '(' -> parse_tuple parser
  | Some '$' -> parse_func parser
  | Some ('+' | '-' | '0' .. '9') -> parse_number parser
  | Some ('A' .. 'Z' | 'a' .. 'z' | '_') -> (
      match parse_wrapped_collection parser "ListV" parse_list with
      | Some value -> value
      | None -> (
          match parse_wrapped_collection parser "TupleV" parse_tuple with
          | Some value -> value
          | None -> (
              match parse_wrapped_collection parser "StructV" parse_struct with
              | Some value -> value
              | None ->
                  let checkpoint = parser.index in
                  try parse_bool parser
                  with Parse_error _ -> (
                    parser.index <- checkpoint;
                    try parse_option parser
                    with Parse_error _ -> (
                      parser.index <- checkpoint;
                      try parse_text parser
                      with Parse_error _ -> (
                        parser.index <- checkpoint;
                        try parse_number parser
                        with Parse_error _ -> (
                          parser.index <- checkpoint;
                          try parse_func parser
                          with Parse_error _ -> (
                            parser.index <- checkpoint;
                            parse_case parser))))))))
  | Some ch ->
      fail parser
        (Printf.sprintf "unexpected character '%c' while parsing value" ch)

let parse_il_value input =
  let parser = { input; index = 0 } in
  try
    let value = parse_value parser in
    skip_spaces parser;
    if is_eof parser then Ok value
    else
      Error
        (Printf.sprintf "unexpected trailing input at position %d" parser.index)
  with
  | Parse_error (index, message) ->
      Error (Printf.sprintf "parse error at position %d: %s" index message)

let parse = parse_il_value
