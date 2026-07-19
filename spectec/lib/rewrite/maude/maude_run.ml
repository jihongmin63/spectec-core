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

(* Reflective (meta-level) execution. The start term is a Maude META-TERM
   ({!To_maude.meta_term_of_value}), reduced via [metaReduce(upModule('SPEC, ..),
   <meta>)] rather than parsed through the emitted module's giant mixfix
   signature -- the dominant per-program cost (~7s for a small P4 program, vs
   ~0.4s to parse the module once). The result is [downTerm]ed back to the
   spec's object syntax, so the rest of this file (output format, stuck check)
   is identical to an object-level [reduce]. *)

(* The Val-kinded default [downTerm] returns when a meta-term denotes no term of
   that kind; also marks a missing Nth search solution. *)
let down_err = "$downerr"

(* The emitted spec module's name (matches {!To_maude.module_of_spec}'s default)
   and the memoized constant the wrapper binds its reflection to, so [upModule]
   (heavy: it reflects the whole ~50k-line module) is evaluated once per Maude
   invocation and reused by every program in a batch. *)
let spec_module = "SPEC"
let spec_meta = "$specmod"

(* Cap on solutions enumerated for an unbounded [search]: [metaSearch] yields one
   solution per index, so an unbounded search is approximated by indices
   [0 .. cap-1]. Search is a debugging affordance (no automation depends on it). *)
let search_cap = 100

(* The reflection wrapper module, written after the emitted SPEC module: it
   imports SPEC (so [downTerm] rebuilds object terms in the spec's vocabulary and
   the stuck check sees the defined heads) and META-LEVEL (the descent
   functions), declares the [downTerm] default, and binds the reflected SPEC
   module to a memoized constant. The advisories Maude prints for the SPEC/prelude
   sort/op overlaps (Type, nil, none) are harmless: only [metaReduce]/[downTerm]
   are used, never the ambiguous symbols directly. *)
let meta_wrapper_module =
  String.concat "\n"
    [
      "mod SPECTEC-META-RUN is";
      "  protecting " ^ spec_module ^ " .";
      "  protecting META-LEVEL .";
      "  op " ^ down_err ^ " : -> Val .";
      "  op " ^ spec_meta ^ " : -> Module [memo] .";
      "  eq " ^ spec_meta ^ " = upModule('" ^ spec_module ^ ", false) .";
      "endm";
      "";
    ]

(* The [reduce] command(s) for [mode] on the META-TERM [start]: reflect [start]
   in the spec module, then [downTerm] the result back to object syntax.
   [Reduce]/[Rewrite] are deterministic (one command); [Search] enumerates
   solution indices ([metaSearch] returns the Nth normal form). *)
let meta_commands (mode : mode) (start : string) : string list =
  let down body =
    Printf.sprintf "reduce downTerm(getTerm(%s), %s) ." body down_err
  in
  match mode with
  | Reduce -> [ down (Printf.sprintf "metaReduce(%s, %s)" spec_meta start) ]
  | Rewrite ->
      [ down (Printf.sprintf "metaRewrite(%s, %s, unbounded)" spec_meta start) ]
  | Search bound ->
      let cap = Option.value bound ~default:search_cap in
      List.init cap (fun k ->
          down
            (Printf.sprintf "metaSearch(%s, %s, 'R:Val, nil, '!, unbounded, %d)"
               spec_meta start k))

(* The plain object-syntax command(s) for [mode] on the already-encoded object
   term [start] (e.g. ["Program-ok(<structural term>)"]), for a direct (non-
   reflective) [reduce]/[rewrite] -- used by the {!Structural} start-term path
   ({!To_maude.encode_value} with [~scalars:Structural]), which has no built-in
   wrapper for [metaReduce] to reflect specially, so it is parsed through the
   module's real (order-sorted) signature instead of the META-TERM grammar.
   [parse_output] below reads its output identically to the meta path's: either
   way the top-level command is a Maude [reduce]/[rewrite], which always prints
   [result <Sort>: <term>]. [Search] is not supported here (object-syntax search
   patterns are unimplemented; unlike the meta path, which enumerates by
   index) -- this direct path only ever backs the reduce-only structural
   differential oracle. *)
let direct_commands (mode : mode) (start : string) : string list =
  match mode with
  | Reduce -> [ Printf.sprintf "reduce %s ." start ]
  | Rewrite -> [ Printf.sprintf "rewrite %s ." start ]
  | Search _ ->
      failwith
        "Maude_run: direct (structural) Search mode is not supported -- only \
         Reduce/Rewrite"

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

(* Parse Maude's stdout. Every command is a [downTerm(...)] reduce, so each
   prints [result <Sort>: <term>] in the spec's object syntax (the meta wrapping
   is invisible here). [Reduce]/[Rewrite] emit one such line; [Search] emits one
   per enumerated solution index, the spent ones downing to the [down_err]
   default. *)
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
      (* One [downTerm] result line per enumerated solution index; an index past
         the last solution downs to the [down_err] default, which we drop. *)
      let rec gather acc = function
        | [] -> List.rev acc
        | l :: rest when String.starts_with ~prefix:"result " l -> (
            match String.index_opt l ':' with
            | Some i ->
                let term =
                  gather_term
                    (String.sub l (i + 1) (String.length l - i - 1))
                    rest
                in
                gather (term :: acc) rest
            | None -> gather acc rest)
        | _ :: rest -> gather acc rest
      in
      let sols = List.filter (fun t -> t <> down_err) (gather [] lines) in
      if sols = [] then NoSolution else Solutions sols

let run_process (bin : string) (timeout : int) (file : string) =
  (* stderr folded in: Maude reports a start term it cannot parse only as a
     warning there, and the result markers parsed below never collide with
     warning lines -- without it such a run is an opaque "could not parse". *)
  (* [ulimit -s unlimited] first: a legitimately-deep (not runaway) reduction
     can overflow Maude's native stack under whatever small default the
     invoking shell inherited, printing a native "Fatal error: stack
     overflow" that looks nothing like a real ERROR/timeout -- the same root
     cause {!Mfe.run_mfe}'s CRC path just needed the identical fix for, and
     the one [check_diff_structural_p4.sh] already works around at the
     wrapping-script level (this fixes it at the source instead, so every
     caller of [run_process] -- not just that one script -- is covered). *)
  let cmd =
    if timeout > 0 then
      Printf.sprintf "ulimit -s unlimited; timeout %d %s -no-banner %s 2>&1"
        timeout (Filename.quote bin) (Filename.quote file)
    else
      Printf.sprintf "ulimit -s unlimited; %s -no-banner %s 2>&1"
        (Filename.quote bin) (Filename.quote file)
  in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  (ic, output)

(* Map a non-zero Maude process status to the [Error]/[Timeout] that stands in
   for a result. Shared by the single and batch runners. *)
let result_of_failed_status bin output = function
  | Unix.WEXITED 0 -> assert false
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
      Error (Printf.sprintf "maude killed by signal %d" n)

(* Write [module_text] then [commands] (each already terminated) plus [quit] to a
   fresh temp file, run Maude on it once, and hand the raw stdout+status to [k].
   Centralizes the temp-file lifecycle for both runners. *)
let with_maude_run ?(wrapper = meta_wrapper_module) bin timeout module_text
    commands k =
  let file = Filename.temp_file "spectec_maude" ".maude" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove file with Sys_error _ -> ())
    (fun () ->
      let oc = open_out file in
      output_string oc module_text;
      output_char oc '\n';
      output_string oc wrapper;
      List.iter (output_string oc) commands;
      output_string oc "quit\n";
      close_out oc;
      let ic, output = run_process bin timeout file in
      k (Unix.close_process_in ic) output)

let run ?maude_bin ?(timeout = 30) ?(defined_heads = []) ~(mode : mode)
    ~(module_text : string) ~(start : string) () : result =
  let bin = resolve_bin maude_bin in
  with_maude_run bin timeout module_text
    (List.map (fun c -> c ^ "\n") (meta_commands mode start) @ [ "\n" ])
    (fun status output ->
      match status with
      | Unix.WEXITED 0 -> parse_output mode defined_heads output
      | status -> result_of_failed_status bin output status)

(* A ground term Maude reduces to itself, emitted between batched commands to
   delimit each one's output: the resulting [result ...: ...] line is the
   boundary. Chosen unlikely to collide with any real normal form. No
   underscores (unlike a normal CTRS-derived identifier, this one is never fed
   through {!Rewrite_system.maude_id}'s mangling): [run_batch]'s [Native] path
   quotes it as a Maude [String] literal (mangling wouldn't apply inside quotes
   anyway), but [run_batch_direct]'s [Structural] path
   ({!To_mfe.module_of_system}'s execution mode) declares it as a bare nullary
   identifier, and Maude's parser reads underscores in a bare identifier as
   mixfix argument slots -- hyphens avoid that without needing to mangle (and
   then re-derive the mangled spelling here) just for this one marker. *)
let batch_sep = "$$SPECTEC-BATCH-SEP$$"

(* The boundary is the marker's [result] line, not Maude's [reduce in M : ...]
   command echo -- both carry [batch_sep], so matching on the substring alone
   would double-count and shift every result by one. *)
let is_batch_boundary line =
  String.starts_with ~prefix:"result " line && index_sub line batch_sep <> None

(* Split [output]'s lines into the segments preceding each boundary line, one per
   batched command in order; trailing lines after the last boundary (the [quit]
   echo) are dropped. *)
let split_batch_segments (output : string) : string list =
  String.split_on_char '\n' output
  |> List.fold_left
       (fun (segments, buf) line ->
         if is_batch_boundary line then
           (String.concat "\n" (List.rev buf) :: segments, [])
         else (segments, line :: buf))
       ([], [])
  |> fst |> List.rev

(* Run a whole batch of [starts] against one [module_text] in a single Maude
   invocation, amortizing the (dominant) cost of parsing the emitted module. Each
   start's command is followed by a [batch_sep] marker; the output is split on
   those markers and each segment parsed as in {!run}. Returns one result per
   start, in order. A process-level failure (timeout/crash) maps every start to
   that same failure -- it cannot be attributed to one program. *)
let run_batch ?maude_bin ?(timeout = 30) ?(defined_heads = []) ~(mode : mode)
    ~(module_text : string) ~(starts : string list) () : result list =
  match starts with
  | [] -> []
  | _ ->
      let bin = resolve_bin maude_bin in
      let commands =
        List.concat_map
          (fun start ->
            List.map (fun c -> c ^ "\n") (meta_commands mode start)
            @ [ "\n"; Printf.sprintf "reduce \"%s\" .\n" batch_sep ])
          starts
      in
      with_maude_run bin timeout module_text commands (fun status output ->
          match status with
          | Unix.WEXITED 0 ->
              let segments = split_batch_segments output in
              (* Pad a short batch (e.g. Maude aborted early) so every start gets
                 a result; a missing segment is an unparseable run. *)
              List.mapi
                (fun i start ->
                  match List.nth_opt segments i with
                  | Some seg -> parse_output mode defined_heads seg
                  | None ->
                      Error
                        (Printf.sprintf
                           "maude produced no output for start term:\n%s" start))
                starts
          | _status ->
              (* A process-level failure (a crash -- e.g. SIGABRT/OOM -- or a
                 whole-batch timeout) cannot be attributed to one program: a
                 single start may have aborted Maude and poisoned the entire
                 batch's output. Re-run each start in its OWN Maude process so the
                 failure is isolated to the offending program and every other
                 start still gets its real verdict. This only fires when a batch
                 actually crashed; the amortized fast path above is untouched. *)
              List.map
                (fun start ->
                  run ?maude_bin ~timeout ~defined_heads ~mode ~module_text
                    ~start ())
                starts)


(* -------------------------------------------------------------------------- *)
(* Direct (non-reflective) object-syntax execution: the {!Structural} start-term
   path. No meta wrapper is written (nothing reflects the module), so [start] is
   already object-syntax text in the module's own vocabulary
   ({!Maude_sorts.print_term}), not a META-TERM. Otherwise mirrors [run]/
   [run_batch] exactly (same process/output plumbing, same [result]). *)

let run_direct ?maude_bin ?(timeout = 30) ?(defined_heads = []) ~(mode : mode)
    ~(module_text : string) ~(start : string) () : result =
  let bin = resolve_bin maude_bin in
  with_maude_run ~wrapper:"" bin timeout module_text
    (List.map (fun c -> c ^ "\n") (direct_commands mode start) @ [ "\n" ])
    (fun status output ->
      match status with
      | Unix.WEXITED 0 -> parse_output mode defined_heads output
      | status -> result_of_failed_status bin output status)

let run_batch_direct ?maude_bin ?(timeout = 30) ?(defined_heads = [])
    ~(mode : mode) ~(module_text : string) ~(starts : string list) () :
    result list =
  match starts with
  | [] -> []
  | _ ->
      let bin = resolve_bin maude_bin in
      let commands =
        List.concat_map
          (fun start ->
            List.map (fun c -> c ^ "\n") (direct_commands mode start)
            (* Bare identifier, not a quoted Maude [String] literal like
               [run_batch]'s separator: [module_text] here is {!To_mfe}'s
               structural module, which declares [batch_sep] itself as a
               nullary marker constant (see its doc comment) rather than
               importing [STRING] (which would clash with [BOOL]). *)
            @ [ "\n"; Printf.sprintf "reduce %s .\n" batch_sep ])
          starts
      in
      with_maude_run ~wrapper:"" bin timeout module_text commands
        (fun status output ->
          match status with
          | Unix.WEXITED 0 ->
              let segments = split_batch_segments output in
              List.mapi
                (fun i start ->
                  match List.nth_opt segments i with
                  | Some seg -> parse_output mode defined_heads seg
                  | None ->
                      Error
                        (Printf.sprintf
                           "maude produced no output for start term:\n%s" start))
                starts
          | _status ->
              List.map
                (fun start ->
                  run_direct ?maude_bin ~timeout ~defined_heads ~mode
                    ~module_text ~start ())
                starts)
