(* Maude Formal Environment bridge: render a rewriting system as a single-sort
   Full Maude system module ({!Rewrite_system.string_of_system_maude}), load the
   MFE into a local Maude binary, and run the Church-Rosser Checker (CRC) and
   Coherence Checker (ChC) on it in one invocation. Mirrors the {!Aprove} /
   {!Maude_run} local-process pattern (no Python).

   CRC decides whether the equational fragment is confluent (so [reduce] is
   well-defined); ChC whether the [rl] relations are coherent with the equations
   (so search is complete modulo them). See mfe.mli and tools/mfe/README.md.

   The constants and verdict tokens below are CALIBRATED against a real MFE run
   (MFE-master, Maude 3.5.1); the protocol they encode -- entry file, stdin
   feeding, tool selection, verdict phrasing, and EOF handling -- is documented
   in tools/mfe/README.md. The MFE behaves differently from a plain Maude file:

   - The entry is [src/mfe.maude] (not [full-maude.maude]); it [load]s [FM/] and
     [CRChC/] relative to itself and [sload]s [file]/[process]/[time] from the
     Maude library, so the Maude binary's directory is exported as [MAUDE_LIB].
   - The MFE runs an interactive object loop reading commands from STDIN. The
     module and checks must be PIPED to the loop ([load mfe.maude] + module +
     commands on stdin), not passed as a [maude FILE] (whose trailing lines the
     loop never sees).
   - A tool must be selected before a check: [(select tool CRC .)] then
     [(check Church-Rosser SPEC .)]; likewise [ChC] before [(check coherence
     SPEC .)]. A bare [(check ...)] with no tool selected is a parse error.
   - The loop has no clean [quit]: at end of input it floods an incomplete-input
     prompt ([> ]) forever. So the run is read under a deadline and the process
     is killed once the coherence check's output is followed by that flood (or
     the deadline passes); a verdict already printed is still parsed even though
     the process is killed rather than exiting cleanly. *)

type verdict = Yes | No | Maybe | Timeout | Error of string
type result = { church_rosser : verdict; coherence : verdict }

let string_of_verdict = function
  | Yes -> "YES"
  | No -> "NO"
  | Maybe -> "MAYBE"
  | Timeout -> "TIMEOUT"
  | Error msg -> "ERROR: " ^ msg

(* The emitted module's name (matches [string_of_system_maude]'s default) and the
   MFE entry file, relative to the MFE directory. *)
let module_name = "SPEC"
let mfe_entry = "src/mfe.maude"

(* The commands fed after the module: select each tool, then run its check. The
   selection is mandatory -- a bare [(check ...)] does not parse. *)
let check_commands =
  [
    "(select tool CRC .)";
    Printf.sprintf "(check Church-Rosser %s .)" module_name;
    "(select tool ChC .)";
    Printf.sprintf "(check coherence %s .)" module_name;
  ]

(* -------------------------------------------------------------------------- *)
(* Binary / MFE / library resolution. *)

(* Where to find the [maude] binary: explicit argument, then [SPECTEC_MAUDE_BIN],
   then the repo-relative download location, then [maude] on [PATH]. *)
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

(* Where to find the MFE: explicit argument or [SPECTEC_MFE_DIR] are trusted as
   given; otherwise a repo-relative default is probed for [mfe_entry]. The MFE is
   large and not checked in, so a missing one is a clean [Error]. *)
let resolve_mfe_dir = function
  | Some dir -> Ok dir
  | None -> (
      match Sys.getenv_opt "SPECTEC_MFE_DIR" with
      | Some dir -> Ok dir
      | None -> (
          let candidates = [ "spectec/tools/mfe"; "tools/mfe" ] in
          match
            List.find_opt
              (fun d -> Sys.file_exists (Filename.concat d mfe_entry))
              candidates
          with
          | Some dir -> Ok dir
          | None ->
              Error
                "MFE not found; pass --mfe-dir or set SPECTEC_MFE_DIR (see \
                 spectec/tools/mfe/README.md)"))

(* Make a (possibly relative) path absolute so [load] and [MAUDE_LIB] resolve
   regardless of Maude's working directory. *)
let absolute (path : string) : string =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

(* The Maude library directory to export as [MAUDE_LIB]: the binary's own
   directory when it holds the standard prelude (the bundled
   [spectec/tools/maude]), so the MFE's [sload file]/[process]/[time] resolve. A
   plain [maude] on [PATH] is left to Maude's built-in default. *)
let maude_lib_of_bin (bin : string) : string option =
  let dir = Filename.dirname (absolute bin) in
  if Sys.file_exists (Filename.concat dir "prelude.maude") then Some dir
  else None

(* The child's environment: the parent's, with [MAUDE_LIB] set when known. *)
let child_env (bin : string) : string array =
  let base = Unix.environment () in
  match maude_lib_of_bin bin with
  | None -> base
  | Some lib ->
      let kept =
        Array.to_list base
        |> List.filter (fun kv ->
               not (String.starts_with ~prefix:"MAUDE_LIB=" kv))
      in
      Array.of_list (("MAUDE_LIB=" ^ lib) :: kept)

(* -------------------------------------------------------------------------- *)
(* Output scanning. *)

(* True when [sub] occurs in [s]. *)
let contains (s : string) (sub : string) : bool =
  let ns = String.length s and nb = String.length sub in
  let rec go i =
    if i + nb > ns then false
    else if String.sub s i nb = sub then true
    else go (i + 1)
  in
  go 0

(* Collapse every run of whitespace (the MFE wraps result lines at the terminal
   width, splitting a verdict phrase across lines) to a single space, so the
   verdict tokens below can be matched as the contiguous strings the MFE means. *)
let normalize_ws (s : string) : string =
  let b = Buffer.create (String.length s) in
  let in_ws = ref false in
  String.iter
    (fun c ->
      match c with
      | ' ' | '\t' | '\n' | '\r' ->
          if not !in_ws then Buffer.add_char b ' ';
          in_ws := true
      | _ ->
          Buffer.add_char b c;
          in_ws := false)
    s;
  Buffer.contents b

(* The length of the trailing run of incomplete-input-prompt characters ([>],
   spaces, newlines): the MFE floods these at end of input, so a long trailing
   run after the coherence check marks the run finished. *)
let trailing_prompt_run (s : string) : int =
  let n = String.length s in
  let rec go i =
    if i < 0 then n
    else
      match s.[i] with
      | '>' | ' ' | '\n' | '\r' | '\t' -> go (i - 1)
      | _ -> n - 1 - i
  in
  go (n - 1)

(* The coherence check (the last command) has produced its output and the loop
   has fallen into the EOF prompt flood -- enough to stop reading and kill the
   process. The flood threshold is well above the short prompt bursts printed
   between commands. *)
let checks_done (raw : string) : bool =
  contains raw "Coherence checking of " && trailing_prompt_run raw >= 200

(* -------------------------------------------------------------------------- *)
(* Run the MFE on a feed, returning its merged stdout+stderr and whether the
   deadline (not a finished run) stopped it. *)

let run_mfe ~(bin : string) ~(timeout : int) (feed : string) : string * bool =
  (* Feed stdin from a temp file (not a pipe), so a module larger than the pipe
     buffer cannot deadlock against the unread stdout. *)
  let infile = Filename.temp_file "spectec_mfe" ".in" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove infile with Sys_error _ -> ())
    (fun () ->
      let oc = open_out infile in
      output_string oc feed;
      close_out oc;
      let fd_in = Unix.openfile infile [ Unix.O_RDONLY ] 0 in
      let r_out, w_out = Unix.pipe () in
      let pid =
        Unix.create_process_env bin [| bin; "-no-banner" |] (child_env bin)
          fd_in w_out w_out
      in
      Unix.close fd_in;
      Unix.close w_out;
      let buf = Buffer.create 65536 in
      let chunk = Bytes.create 8192 in
      let timed_out = ref false in
      let deadline =
        if timeout > 0 then Unix.gettimeofday () +. float_of_int timeout
        else infinity
      in
      let rec loop () =
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining <= 0.0 then timed_out := true
        else
          match Unix.select [ r_out ] [] [] (Float.min remaining 0.25) with
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
          | [], _, _ -> if not (checks_done (Buffer.contents buf)) then loop ()
          | _ ->
              let n = Unix.read r_out chunk 0 (Bytes.length chunk) in
              if n = 0 then () (* child closed stdout: run finished *)
              else (
                Buffer.add_subbytes buf chunk 0 n;
                if not (checks_done (Buffer.contents buf)) then loop ())
      in
      loop ();
      Unix.close r_out;
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      ignore (Unix.waitpid [] pid);
      (Buffer.contents buf, !timed_out))

(* -------------------------------------------------------------------------- *)
(* Verdict classification (over the whitespace-normalized output). *)

let crc_verdict ~(timed_out : bool) (norm : string) : verdict =
  if contains norm "The specification is locally-confluent." then Yes
  else if contains norm "The following critical pairs must be proved joinable"
  then Maybe
  else if timed_out then Timeout
  else Error "could not find a Church-Rosser verdict in the MFE output"

let chc_verdict ~(timed_out : bool) (norm : string) : verdict =
  if
    contains norm
      "no rewrite with rules can happen at non-overlapping positions of \
       equations left-hand sides"
  then Yes
    (* the ChC ran (its header printed) but did not report coherence: proof
       obligations remain *)
  else if contains norm "Coherence checking of " then Maybe
  else if timed_out then Timeout
  else Error "could not find a coherence verdict in the MFE output"

let check ?(timeout = 60) ?maude_bin ?mfe_dir ~(rule_heads : string list)
    (system : Rewrite_system.t) : result =
  match resolve_mfe_dir mfe_dir with
  | Error msg -> { church_rosser = Error msg; coherence = Error msg }
  | Ok mfe_dir ->
      let bin = resolve_bin maude_bin in
      let mfe_path = absolute (Filename.concat mfe_dir mfe_entry) in
      (* Drop the [owise] equations before the CRC/ChC. An owise rule fires only
         where no sibling matched, so its overlaps are infeasible by construction;
         the checker ignores the [owise] attribute and would flag them as spurious
         critical pairs. Stripping them is sound for the confluence gate and is
         confined to the MFE input -- the [--ctrs] dump and any termination view
         keep the full system. *)
      let system = Rewrite_system.drop_owise system in
      let module_text =
        Rewrite_system.string_of_system_maude ~module_name ~rule_heads system
      in
      (* [load mfe.maude] starts the object loop; the module (already prefixed
         with [set include BOOL off .]) and the tool-selecting checks follow. *)
      let feed =
        Printf.sprintf "load %s\n%s%s\n" mfe_path module_text
          (String.concat "\n" check_commands)
      in
      let output, timed_out = run_mfe ~bin ~timeout feed in
      let norm = normalize_ws output in
      {
        church_rosser = crc_verdict ~timed_out norm;
        coherence = chc_verdict ~timed_out norm;
      }
