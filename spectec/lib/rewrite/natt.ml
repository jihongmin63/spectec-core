module R = Rewrite_system
module Symbols = Map.Make (String)

let xml_escape text =
  String.to_seq text
  |> Seq.map (function
       | '&' -> "&amp;"
       | '<' -> "&lt;"
       | '>' -> "&gt;"
       | '"' -> "&quot;"
       | '\'' -> "&apos;"
       | c -> String.make 1 c)
  |> List.of_seq |> String.concat ""

let rec term_xml = function
  | R.Var name -> "<var>" ^ xml_escape ("v_" ^ name) ^ "</var>"
  | R.App (name, args) ->
      "<funapp><name>"
      ^ xml_escape ("f_" ^ name)
      ^ "</name>"
      ^ String.concat ""
          (List.map (fun arg -> "<arg>" ^ term_xml arg ^ "</arg>") args)
      ^ "</funapp>"

let rec signature symbols = function
  | R.Var _ -> Ok symbols
  | R.App (name, args) -> (
      let arity = List.length args in
      match Symbols.find_opt name symbols with
      | Some existing when existing <> arity ->
          Error
            (Printf.sprintf "symbol %S has conflicting arities %d and %d" name
               existing arity)
      | _ ->
          List.fold_left
            (fun acc term ->
              Result.bind acc (fun symbols -> signature symbols term))
            (Ok (Symbols.add name arity symbols))
            args)

let encode (problem : Solver.problem) : (Solver.document, string) result =
  let symbols =
    List.fold_left
      (fun acc (lhs, rhs) ->
        Result.bind acc (fun symbols ->
            Result.bind (signature symbols lhs) (fun symbols ->
                signature symbols rhs)))
      (Ok Symbols.empty) problem.rules
  in
  Result.map
    (fun symbols ->
      let rules =
        List.map
          (fun (lhs, rhs) ->
            "<rule><lhs>" ^ term_xml lhs ^ "</lhs><rhs>" ^ term_xml rhs
            ^ "</rhs></rule>")
          problem.rules
      in
      let declarations =
        Symbols.bindings symbols
        |> List.map (fun (name, arity) ->
               Printf.sprintf
                 "<funcsym><name>%s</name><arity>%d</arity></funcsym>"
                 (xml_escape ("f_" ^ name))
                 arity)
      in
      {
        Solver.extension = ".xml";
        contents =
          "<problem><trs><rules>" ^ String.concat "\n" rules
          ^ "</rules><signature>"
          ^ String.concat "\n" declarations
          ^ "</signature></trs></problem>\n";
      })
    symbols

let parse output =
  match String.split_on_char '\n' output with
  | first :: _ -> Solver.verdict_of_token first
  | [] -> Error "empty NaTT output"

let adapter : Solver.adapter =
  {
    name = "natt";
    variable = "SPECTEC_NATT_BIN";
    candidates =
      [
        "spectec/tools/natt/bin/NaTT.exe"; "tools/natt/bin/NaTT.exe"; "NaTT.exe";
      ];
    encode;
    command = (fun ~executable ~input ~timeout:_ -> [ executable; input; "-V" ]);
    parse;
    finished = (fun _ -> false);
    proof = (fun _ -> "");
    dependencies = (fun ~executable:_ -> [ "z3" ]);
  }
