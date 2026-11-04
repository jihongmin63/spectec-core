type verdict = Yes | No | Maybe | Timeout

type problem = {
  rules : (Rewrite_system.term * Rewrite_system.term) list;
  tpdb : string;
}

type document = { extension : string; contents : string }

type adapter = {
  name : string;
  variable : string;
  candidates : string list;
  encode : problem -> (document, string) result;
  command : executable:string -> input:string -> timeout:int -> string list;
  parse : string -> (verdict, string) result;
  finished : string -> bool;
  proof : string -> string;
  dependencies : executable:string -> string list;
}

type tool = { adapter : adapter; executable : string; identity : string }

type run = {
  verdict : (verdict, string) result;
  secs : float;
  stdout : string;
  stderr : string;
  proof : string;
}

let string_of_verdict = function
  | Yes -> "YES"
  | No -> "NO"
  | Maybe -> "MAYBE"
  | Timeout -> "TIMEOUT"

let verdict_of_token token =
  match String.trim token with
  | "YES" -> Ok Yes
  | "NO" -> Ok No
  | "MAYBE" -> Ok Maybe
  | "TIMEOUT" -> Ok Timeout
  | other -> Error (Printf.sprintf "invalid global verdict %S" other)

let resolve (name : string) : string option =
  let paths =
    if String.contains name '/' then [ name ]
    else
      String.split_on_char ':'
        (Option.value (Sys.getenv_opt "PATH") ~default:"")
      |> List.map (fun dir -> Filename.concat dir name)
  in
  List.find_opt
    (fun path -> Sys.file_exists path && not (Sys.is_directory path))
    paths
  |> Option.map Subproc.absolute

let configure (adapter : adapter) : (tool, string) result =
  let candidates =
    match Sys.getenv_opt adapter.variable with
    | Some path -> [ path ]
    | None -> adapter.candidates
  in
  match List.find_map resolve candidates with
  | None ->
      Error
        (Printf.sprintf "%s executable not found; set %s (searched: %s)"
           adapter.name adapter.variable
           (String.concat ", " candidates))
  | Some executable -> (
      try
        Unix.access executable [ Unix.X_OK ];
        let files = executable :: adapter.dependencies ~executable in
        let fingerprints =
          List.map
            (fun name ->
              match resolve name with
              | None ->
                  raise (Sys_error ("required dependency not found: " ^ name))
              | Some path -> path ^ ":" ^ Digest.to_hex (Digest.file path))
            files
        in
        let environment =
          Unix.environment () |> Array.to_list |> List.sort String.compare
        in
        let identity =
          String.concat "\n" ((adapter.name :: fingerprints) @ environment)
          |> Digest.string |> Digest.to_hex
        in
        Ok { adapter; executable; identity }
      with
      | Sys_error message -> Error (adapter.name ^ " configuration: " ^ message)
      | Unix.Unix_error (error, operation, path) ->
          Error
            (Printf.sprintf "%s configuration: %s %S: %s" adapter.name operation
               path (Unix.error_message error)))

let status_text = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal

let check ~tool ~timeout ~problem : run =
  if timeout <= 0 then invalid_arg "Solver.check: timeout must be positive";
  let failed message =
    { verdict = Error message; secs = 0.; stdout = ""; stderr = ""; proof = "" }
  in
  match tool.adapter.encode problem with
  | Error message -> failed (tool.adapter.name ^ " input: " ^ message)
  | Ok document ->
      let path = Filename.temp_file "spectec_solver" document.extension in
      Fun.protect
        ~finally:(fun () -> Sys.remove path)
        (fun () ->
          let oc = open_out_bin path in
          Fun.protect
            ~finally:(fun () -> close_out oc)
            (fun () -> output_string oc document.contents);
          let cmd =
            tool.adapter.command ~executable:tool.executable ~input:path
              ~timeout
          in
          let result, secs =
            Subproc.timed (fun () ->
                Subproc.run_detailed ~env:None ~keep:None
                  ~done_when:(Some tool.adapter.finished) ~cmd ~feed:"" ~timeout
                  ())
          in
          let parseable =
            if result.timed_out || result.stopped then
              match String.rindex_opt result.output '\n' with
              | None -> ""
              | Some last -> String.sub result.output 0 (last + 1)
            else result.output
          in
          let parsed = tool.adapter.parse parseable in
          let verdict =
            if
              match result.status with
              | Unix.WEXITED code -> code <> 0
              | _ -> false
            then Error ("process " ^ status_text result.status)
            else if result.timed_out then
              (* A complete global answer can precede a slow proof export. *)
              match parsed with
              | Ok (Yes | No) -> parsed
              | _ -> Ok Timeout
            else if
              result.status <> Unix.WEXITED 0
              && not
                   (result.stopped && result.status = Unix.WSIGNALED Sys.sigkill)
            then Error ("process " ^ status_text result.status)
            else parsed
          in
          let verdict =
            Result.map_error
              (fun message ->
                Printf.sprintf
                  "%s: %s; command=%s; timeout=%d; status=%s; stdout=%S; \
                   stderr=%S"
                  tool.adapter.name message
                  (String.concat " " (List.map Filename.quote cmd))
                  timeout
                  (status_text result.status)
                  result.output result.stderr)
              verdict
          in
          {
            verdict;
            secs;
            stdout = result.output;
            stderr = result.stderr;
            proof = tool.adapter.proof result.output;
          })
