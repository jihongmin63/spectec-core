(* Maude execution bridge: write an emitted module ({!To_maude}) plus a command
   to a temp file and run a local Maude binary on it, parsing the result. Mirrors
   the {!Aprove} bridge (a local process via [Unix.open_process_in], no Python),
   but Maude does not decide a yes/no verdict -- it *runs* the rewriting system,
   so the result is a normal form ([reduce]) or the set of reachable solutions
   ([search]). See maude_run.mli for the public contract. *)

type mode = Reduce | Rewrite | Search of int option

type result =
  | Reduced of string
  | Stuck of { term : string; symbols : string list }
  | Solutions of string list
  | NoSolution
  | Timeout
  | Error of string

let string_of_result = function
  | Reduced t -> "result: " ^ t
  | Stuck { term; symbols } ->
      Printf.sprintf "FAIL (stuck): %s\n  unreduced: %s" term
        (String.concat ", " symbols)
  | Solutions [] -> "no solutions"
  | Solutions ts ->
      String.concat "\n"
        (List.mapi (fun i t -> Printf.sprintf "solution %d: %s" (i + 1) t) ts)
  | NoSolution -> "no solution"
  | Timeout -> "TIMEOUT"
  | Error msg -> "ERROR: " ^ msg

let is_failure = function
  | Reduced _ -> false
  | Solutions (_ :: _) -> false
  | Stuck _ | Solutions [] | NoSolution | Timeout | Error _ -> true

(* The operator/constant names in a ground Maude term: split on the term
   punctuation [( ) ,] and whitespace, keeping the maximal id runs in between.
   A string literal is skipped whole (its body may contain any punctuation and
   never names a symbol). *)
let symbols_of_term (s : string) : string list =
  let is_sep c =
    c = '(' || c = ')' || c = ',' || c = ' ' || c = '\n' || c = '\t'
  in
  let acc = ref [] and buf = Buffer.create 32 in
  let flush () =
    if Buffer.length buf > 0 then (
      acc := Buffer.contents buf :: !acc;
      Buffer.clear buf)
  in
  let in_string = ref false and escaped = ref false in
  String.iter
    (fun c ->
      if !in_string then (
        if !escaped then escaped := false
        else if c = '\\' then escaped := true
        else if c = '"' then in_string := false)
      else if c = '"' then (
        flush ();
        in_string := true)
      else if is_sep c then flush ()
      else Buffer.add_char buf c)
    s;
  flush ();
  List.rev !acc

(* A normal form still mentioning a defined symbol stopped mid-evaluation. *)
let classify_normal_form (defined : string list) (term : string) : result =
  match
    List.sort_uniq compare
      (List.filter (fun sym -> List.mem sym defined) (symbols_of_term term))
  with
  | [] -> Reduced term
  | symbols -> Stuck { term; symbols }

(* Where to find the [maude] binary: explicit argument, then [SPECTEC_MAUDE_BIN],
   then the repo-relative download location (see tools/maude/README.md), then
   [maude] on [PATH]. A binary that turns out to be missing surfaces as a clean
   [Error] when the process fails to start (exit 127), not a crash. *)
let resolve_bin = function
  | Some path -> path
  | None -> (
      match Sys.getenv_opt "SPECTEC_MAUDE_BIN" with
      | Some path -> path
      | None -> (
          let candidates =
            [ "spectec/tools/maude/maude"; "tools/maude/maude" ]
          in
          match List.find_opt Sys.file_exists candidates with
          | Some path -> path
          | None -> "maude"))

(* The Maude command line for [mode], searching/reducing [start]. [search] binds
   a fresh result variable of the universal sort [Val] (every emitted sort is a
   subsort of it), so any normal form matches. *)
let command_of_mode mode start =
  match mode with
  | Reduce -> Printf.sprintf "reduce %s ." start
  | Rewrite -> Printf.sprintf "rewrite %s ." start
  | Search None -> Printf.sprintf "search %s =>! R:Val ." start
  | Search (Some bound) ->
      Printf.sprintf "search [%d] %s =>! R:Val ." bound start

(* Position of [sub] in [s], if present. *)
let index_sub (s : string) (sub : string) : int option =
  let ns = String.length s and nb = String.length sub in
  let rec go i =
    if i + nb > ns then None
    else if String.sub s i nb = sub then Some i
    else go (i + 1)
  in
  go 0

(* Maude wraps long terms onto indented continuation lines; gather the lines
   following a marker line that continue it (indented, non-empty) and join them
   with [first] into one term string. *)
let gather_term (first : string) (rest : string list) : string =
  let rec take acc = function
    | l :: more when String.length l > 0 && l.[0] = ' ' -> take (l :: acc) more
    | _ -> List.rev acc
  in
  first :: take [] rest |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> String.concat " "

(* Parse Maude's stdout. [reduce] prints [result <Sort>: <term>]; [search]
   prints [Solution N (state M)] blocks each with a [R:Val --> <term>] binding,
   or [No solution.] when none. *)
let parse_output (mode : mode) (defined_heads : string list) (output : string) :
    result =
  let lines = String.split_on_char '\n' output in
  match mode with
  | Reduce | Rewrite ->
      let rec find = function
        | [] -> Error ("could not parse maude output:\n" ^ output)
        | l :: rest when String.starts_with ~prefix:"result " l -> (
            match String.index_opt l ':' with
            | Some i ->
                classify_normal_form defined_heads
                  (gather_term
                     (String.sub l (i + 1) (String.length l - i - 1))
                     rest)
            | None -> find rest)
        | _ :: rest -> find rest
      in
      find lines
  | Search _ ->
      if List.exists (fun l -> String.trim l = "No solution.") lines then
        NoSolution
      else
        let rec scan acc = function
          | [] -> List.rev acc
          | l :: rest -> (
              match index_sub l "--> " with
              | Some i ->
                  let first = String.sub l (i + 4) (String.length l - i - 4) in
                  scan (gather_term first rest :: acc) rest
              | None -> scan acc rest)
        in
        let sols = scan [] lines in
        if sols = [] then NoSolution else Solutions sols

let run_process (bin : string) (timeout : int) (file : string) =
  (* stderr folded in: Maude reports a start term it cannot parse only as a
     warning there, and the result markers parsed below never collide with
     warning lines -- without it such a run is an opaque "could not parse". *)
  let cmd =
    if timeout > 0 then
      Printf.sprintf "timeout %d %s -no-banner %s 2>&1" timeout
        (Filename.quote bin) (Filename.quote file)
    else
      Printf.sprintf "%s -no-banner %s 2>&1" (Filename.quote bin)
        (Filename.quote file)
  in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  (ic, output)

let run ?maude_bin ?(timeout = 30) ?(defined_heads = []) ~(mode : mode)
    ~(module_text : string) ~(start : string) () : result =
  let bin = resolve_bin maude_bin in
  let file = Filename.temp_file "spectec_maude" ".maude" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove file with Sys_error _ -> ())
    (fun () ->
      let oc = open_out file in
      output_string oc module_text;
      output_char oc '\n';
      output_string oc (command_of_mode mode start);
      output_string oc "\nquit\n";
      close_out oc;
      let ic, output = run_process bin timeout file in
      match Unix.close_process_in ic with
      | Unix.WEXITED 0 -> parse_output mode defined_heads output
      | Unix.WEXITED 124 -> Timeout
      | Unix.WEXITED 127 ->
          Error
            (Printf.sprintf
               "maude not found (tried %S); pass --maude-bin or set \
                SPECTEC_MAUDE_BIN (see tools/maude/README.md)"
               bin)
      | Unix.WEXITED n ->
          Error (Printf.sprintf "maude exited with status %d:\n%s" n output)
      | Unix.WSIGNALED n | Unix.WSTOPPED n ->
          Error (Printf.sprintf "maude killed by signal %d" n))
