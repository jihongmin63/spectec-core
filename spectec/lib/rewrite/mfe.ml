(* Maude Formal Environment bridge: render a rewriting system as a single-sort
   Full Maude system module ({!Rewrite_system.string_of_system_maude}), load the
   MFE into a local Maude binary, and run the Church-Rosser Checker (CRC) and
   Coherence Checker (ChC) on it in one invocation. Mirrors the {!Aprove} /
   {!Maude_run} local-process pattern ([Unix.open_process_in], no Python).

   CRC decides whether the equational fragment is confluent (so [reduce] is
   well-defined); ChC whether the [rl] relations are coherent with the equations
   (so search is complete modulo them). See mfe.mli and tools/mfe/README.md.

   NOTE: the MFE is not bundled, so the load file name, the [(check ...)] command
   syntax, and the verdict phrasings below are best-effort and must be CALIBRATED
   against a real MFE run (see tools/mfe/README.md). They are isolated as the
   named constants/parsers here so calibration touches one place. *)

type verdict = Yes | No | Maybe | Timeout | Error of string
type result = { church_rosser : verdict; coherence : verdict }

let string_of_verdict = function
  | Yes -> "YES"
  | No -> "NO"
  | Maybe -> "MAYBE"
  | Timeout -> "TIMEOUT"
  | Error msg -> "ERROR: " ^ msg

(* The emitted module's name (matches [string_of_system_maude]'s default) and the
   Full Maude entry file expected under the MFE directory. *)
let module_name = "SPEC"
let mfe_entry = "full-maude.maude"

(* Full Maude check commands. *)
let crc_command = Printf.sprintf "(check Church-Rosser %s .)" module_name
let chc_command = Printf.sprintf "(check coherence %s .)" module_name

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

(* True when [sub] occurs in [s]. *)
let contains (s : string) (sub : string) : bool =
  let ns = String.length s and nb = String.length sub in
  let rec go i =
    if i + nb > ns then false
    else if String.sub s i nb = sub then true
    else go (i + 1)
  in
  go 0

(* Classify one check's outcome from the combined output. CRC and ChC key on
   disjoint success phrases, so each is read over the whole output without
   splitting. CALIBRATE the phrases against a real MFE run. *)
let crc_verdict output =
  if contains output "is Church-Rosser" then Yes
  else if contains output "Church-Rosser" then Maybe
    (* critical pairs / POs reported *)
  else
    Error ("could not find a Church-Rosser result; calibrate parser:\n" ^ output)

let chc_verdict output =
  if contains output "is coherent" then Yes
  else if contains output "coheren" then Maybe (* coherence proof obligations *)
  else Error ("could not find a coherence result; calibrate parser:\n" ^ output)

(* Write the MFE load, the module, and both check commands to a temp file, run
   Maude on it once, and return its status and stdout (stderr folded in, as in
   {!Maude_run}: load/parse warnings surface there). *)
let run_maude bin mfe_dir timeout module_text =
  let file = Filename.temp_file "spectec_mfe" ".maude" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove file with Sys_error _ -> ())
    (fun () ->
      let oc = open_out file in
      output_string oc
        (Printf.sprintf "load %s\n" (Filename.concat mfe_dir mfe_entry));
      output_string oc module_text;
      output_string oc (crc_command ^ "\n");
      output_string oc (chc_command ^ "\n");
      output_string oc "quit\n";
      close_out oc;
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
      (Unix.close_process_in ic, output))

let check ?(timeout = 60) ?maude_bin ?mfe_dir ~(rule_heads : string list)
    (system : Rewrite_system.t) : result =
  match resolve_mfe_dir mfe_dir with
  | Error msg -> { church_rosser = Error msg; coherence = Error msg }
  | Ok mfe_dir -> (
      let bin = resolve_bin maude_bin in
      let module_text =
        Rewrite_system.string_of_system_maude ~module_name ~rule_heads system
      in
      let both v = { church_rosser = v; coherence = v } in
      let status, output = run_maude bin mfe_dir timeout module_text in
      match status with
      | Unix.WEXITED 0 ->
          { church_rosser = crc_verdict output; coherence = chc_verdict output }
      | Unix.WEXITED 124 -> both Timeout
      | Unix.WEXITED 127 ->
          both
            (Error
               (Printf.sprintf
                  "maude not found (tried %S); pass --maude-bin or set \
                   SPECTEC_MAUDE_BIN"
                  bin))
      | Unix.WEXITED n ->
          both
            (Error (Printf.sprintf "maude exited with status %d:\n%s" n output))
      | Unix.WSIGNALED n | Unix.WSTOPPED n ->
          both (Error (Printf.sprintf "maude killed by signal %d" n)))
