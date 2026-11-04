let adapters : Solver.adapter list =
  [
    Aprove.adapter;
    Natt.adapter;
    Muterm.adapter;
    Nti.adapter;
    Mint.adapter;
    Nattpp.adapter;
  ]

let names = List.map (fun (adapter : Solver.adapter) -> adapter.name) adapters

let find name : (Solver.adapter, string) result =
  match
    List.find_opt
      (fun (adapter : Solver.adapter) -> adapter.name = name)
      adapters
  with
  | Some adapter -> Ok adapter
  | None ->
      Error
        (Printf.sprintf "unknown termination tool %S; available: %s" name
           (String.concat ", " names))
