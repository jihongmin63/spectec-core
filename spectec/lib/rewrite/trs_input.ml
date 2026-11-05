module R = Rewrite_system

module Symbols = Set.Make (struct
  type t = string * int

  let compare = compare
end)

let function_name name arity = Printf.sprintf "f_%d_%s" arity name

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
      ^ xml_escape (function_name name (List.length args))
      ^ "</name>"
      ^ String.concat ""
          (List.map (fun arg -> "<arg>" ^ term_xml arg ^ "</arg>") args)
      ^ "</funapp>"

let rec signature symbols = function
  | R.Var _ -> symbols
  | R.App (name, args) ->
      List.fold_left signature
        (Symbols.add (name, List.length args) symbols)
        args

let symbols_of_rules rules =
  List.fold_left
    (fun symbols (lhs, rhs) -> signature (signature symbols lhs) rhs)
    Symbols.empty rules

let xml (problem : Solver.problem) : (Solver.document, string) result =
  let symbols = symbols_of_rules problem.rules in
  Ok
    (let rules =
       List.map
         (fun (lhs, rhs) ->
           "<rule><lhs>" ^ term_xml lhs ^ "</lhs><rhs>" ^ term_xml rhs
           ^ "</rhs></rule>")
         problem.rules
     in
     let declarations =
       Symbols.elements symbols
       |> List.map (fun (name, arity) ->
              Printf.sprintf
                "<funcsym><name>%s</name><arity>%d</arity></funcsym>"
                (xml_escape (function_name name arity))
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

let ari_name prefix name =
  prefix
  ^ (String.to_seq name
    |> Seq.map (function
         | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> String.make 1 c
         | c -> Printf.sprintf "_%02x" (Char.code c))
    |> List.of_seq |> String.concat "")

let rec term_ari = function
  | R.Var name -> ari_name "v_" name
  | R.App (name, []) -> ari_name "" (function_name name 0)
  | R.App (name, args) ->
      "("
      ^ ari_name "" (function_name name (List.length args))
      ^ " "
      ^ String.concat " " (List.map term_ari args)
      ^ ")"

let ari (problem : Solver.problem) : (Solver.document, string) result =
  let symbols = symbols_of_rules problem.rules in
  Ok
    (let declarations =
       Symbols.elements symbols
       |> List.map (fun (name, arity) ->
              Printf.sprintf "(fun %s %d)"
                (ari_name "" (function_name name arity))
                arity)
     in
     let rules =
       List.map
         (fun (lhs, rhs) -> "(rule " ^ term_ari lhs ^ " " ^ term_ari rhs ^ ")")
         problem.rules
     in
     {
       Solver.extension = ".ari";
       contents =
         String.concat "\n" (("(format TRS)" :: declarations) @ rules) ^ "\n";
     })

let tpdb (problem : Solver.problem) : (Solver.document, string) result =
  let rec rename = function
    | R.Var name -> R.Var (ari_name "v_" name)
    | R.App (name, args) ->
        R.App
          ( ari_name "" (function_name name (List.length args)),
            List.map rename args )
  in
  List.map (fun (lhs, rhs) -> (rename lhs, rename rhs)) problem.rules
  |> Unravel.print_rules
  |> Result.map (fun (printed, vars) ->
         {
           Solver.extension = ".trs";
           contents = Unravel.tpdb_document ~vars printed;
         })
