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
     the process is killed rather than exiting cleanly ({!Subproc.run}). *)

type verdict = Yes | No | Maybe | Timeout | Error of string
type result = { church_rosser : verdict; coherence : verdict }
type checked = { verdict : verdict; via_normalize : bool }
type upgrade_result = { crc : checked; chc : checked }

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

(* The Maude library directory to export as [MAUDE_LIB]: the binary's own
   directory when it holds the standard prelude (the bundled
   [spectec/tools/maude]), so the MFE's [sload file]/[process]/[time] resolve. A
   plain [maude] on [PATH] is left to Maude's built-in default. *)
let maude_lib_of_bin (bin : string) : string option =
  let dir = Filename.dirname (Subproc.absolute bin) in
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
  Subproc.contains raw "Coherence checking of "
  && trailing_prompt_run raw >= 200

(* In a batched MFE session (one load, many modules) a symbols read buffer
   holds only that symbols output. Its CRC+ChC block is complete once the
   coherence check output is followed by the next [MFE>] prompt -- unlike the
   single-run [checks_done], no long prompt flood is needed (the next modules
   commands follow immediately). *)
let substr_index (s : string) (sub : string) : int option =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else go (i + 1)
  in
  go 0

let batch_checks_done (raw : string) : bool =
  match substr_index raw "Coherence checking of" with
  | None -> false
  | Some i -> Subproc.contains (String.sub raw i (String.length raw - i)) "MFE>"

let run_mfe ~(bin : string) ~(timeout : int) (feed : string) : string * bool =
  Subproc.run ~env:(child_env bin) ~done_when:checks_done
    ~cmd:[ bin; "-no-banner" ] ~feed ~timeout ()

(* -------------------------------------------------------------------------- *)
(* Verdict classification (over the whitespace-normalized output). *)

let crc_verdict ~(timed_out : bool) (norm : string) : verdict =
  if Subproc.contains norm "The specification is locally-confluent." then Yes
  else if
    Subproc.contains norm "The following critical pairs must be proved joinable"
  then Maybe
  else if timed_out then Timeout
  else Error "could not find a Church-Rosser verdict in the MFE output"

let chc_verdict ~(timed_out : bool) (norm : string) : verdict =
  if
    Subproc.contains norm
      "no rewrite with rules can happen at non-overlapping positions of \
       equations left-hand sides"
  then Yes
    (* the ChC ran (its header printed) but did not report coherence: proof
       obligations remain *)
  else if Subproc.contains norm "Coherence checking of " then Maybe
  else if timed_out then Timeout
  else Error "could not find a coherence verdict in the MFE output"

let check ?(timeout = 60) ?maude_bin ?mfe_dir ?(prune_signature = false)
    ?sig_rules (orig : Lang.Il.spec) (system : Rewrite_system.t) : result =
  match resolve_mfe_dir mfe_dir with
  | Error msg -> { church_rosser = Error msg; coherence = Error msg }
  | Ok mfe_dir ->
      let bin = resolve_bin maude_bin in
      let mfe_path = Subproc.absolute (Filename.concat mfe_dir mfe_entry) in
      (* {!Reflect.owise} replaces every reflectable [owise] marker with an
         explicit sibling-disjointness guard before the system gets here, so
         normally no [owise] rule remains. Should one survive (a future spec
         hitting a reflection Gate), it reaches the checker as-is: the CRC
         ignores the [owise] attribute and will flag its structurally
         infeasible sibling overlaps as spurious critical pairs -- a
         conservative MAYBE, never a false YES. Warn so that regression is
         attributable instead of silently dropping the rules. *)
      let unreflected =
        List.length
          (List.filter
             (fun (r : Rewrite_system.rule) -> r.owise)
             system.Rewrite_system.rules)
      in
      if unreflected > 0 then
        Printf.eprintf
          "mfe: WARNING - %d unreflected owise rule(s) reach the MFE input \
           (spurious critical pairs possible)\n"
          unreflected;
      let module_text =
        To_mfe.module_of_system ~module_name ~prune_signature ?sig_rules orig
          system
      in
      (* [load mfe.maude] starts the object loop; the module (already prefixed
         with [set include BOOL off .]) and the tool-selecting checks follow. *)
      let feed =
        Printf.sprintf "load %s\n%s%s\n" mfe_path module_text
          (String.concat "\n" check_commands)
      in
      let output, timed_out = run_mfe ~bin ~timeout feed in
      let norm = Subproc.normalize_ws output in
      {
        church_rosser = crc_verdict ~timed_out norm;
        coherence = chc_verdict ~timed_out norm;
      }

(* -------------------------------------------------------------------------- *)
(* Upgrade-only normalization retry. *)

let upgrade ~(original : verdict) ~(normalized : verdict) : verdict =
  match (original, normalized) with
  | (Maybe | Timeout), Yes -> Yes
  | _ -> original

let check_normalize_upgrade ?timeout ?maude_bin ?mfe_dir ?sig_rules
    (orig : Lang.Il.spec) (system : Rewrite_system.t) : upgrade_result =
  (* Pin the signature to the ORIGINAL system's rules, so the normalized re-run
     declares the same domains (matching [rewrite --ctrs --crc-normalize],
     whose signature also comes from the un-normalized system). *)
  let sig_rules =
    match sig_rules with Some r -> r | None -> system.Rewrite_system.rules
  in
  let base = check ?timeout ?maude_bin ?mfe_dir ~sig_rules orig system in
  let exact =
    {
      crc = { verdict = base.church_rosser; via_normalize = false };
      chc = { verdict = base.coherence; via_normalize = false };
    }
  in
  let inconclusive = function Maybe | Timeout -> true | _ -> false in
  if not (inconclusive base.church_rosser || inconclusive base.coherence) then
    exact
  else
    (* Climb {!Crc_surface.normalize_ladder} until a rung settles both
       components. Each rung's upgrade stands on its own, so a later attempt can
       only add verdicts. A rung the normalization leaves unchanged is skipped:
       re-running the checker on the identical module could only re-roll a
       Timeout.

       Every retry's signature is pruned (verdict-preserving -- the rules are
       untouched). The manual protocol this automates always pruned the
       normalized module, and without it a normalized system whose original
       TIMEOUT was signature-blowup keeps timing out over the whole ~460-sort P4
       signature instead of closing. The base check stays unpruned, matching the
       plain [verify] baseline. *)
    let component original normalized =
      let v = upgrade ~original ~normalized in
      { verdict = v; via_normalize = v <> original }
    in
    let rec climb acc = function
      | [] -> acc
      | strategy :: rest ->
          if acc.crc.verdict = Yes && acc.chc.verdict = Yes then acc
          else
            let normalized_sys = Crc_surface.crc_normalize ~strategy system in
            if normalized_sys = system then climb acc rest
            else
              let n =
                check ?timeout ?maude_bin ?mfe_dir ~prune_signature:true
                  ~sig_rules orig normalized_sys
              in
              let step =
                {
                  crc = component acc.crc.verdict n.church_rosser;
                  chc = component acc.chc.verdict n.coherence;
                }
              in
              (* [via_normalize] must survive a rung that changes nothing. *)
              climb
                {
                  crc =
                    {
                      step.crc with
                      via_normalize =
                        step.crc.via_normalize || acc.crc.via_normalize;
                    };
                  chc =
                    {
                      step.chc with
                      via_normalize =
                        step.chc.via_normalize || acc.chc.via_normalize;
                    };
                }
                rest
    in
    climb exact (Crc_surface.normalize_ladder system)

(* -------------------------------------------------------------------------- *)
(* Batched checking: one MFE session (single ~100s load) for many slices. *)

(* Per-module check commands, named so a batched session's output stream carries
   each symbol's verdict under its own module name (parsed by position). *)
let check_commands_for (name : string) : string list =
  [
    "(select tool CRC .)";
    Printf.sprintf "(check Church-Rosser %s .)" name;
    "(select tool ChC .)";
    Printf.sprintf "(check coherence %s .)" name;
  ]

(* [check_batch orig slices] checks each [(label, slice)] in ONE session, paying
   the Full Maude load once. A symbol whose check exceeds [timeout] is recorded
   [Timeout]/[Timeout] and the now-blocked session is killed and respawned for
   the rest (the load is folded into the first symbol's -- and any respawn's --
   deadline). Verdicts match {!check} run per symbol. *)
let check_batch ?(timeout = 60) ?maude_bin ?mfe_dir ?(prune_signature = false)
    ?sig_rules ?(on_result = fun _ _ _ -> ()) (orig : Lang.Il.spec)
    (slices : (string * Rewrite_system.t) list) : (string * result) list =
  match resolve_mfe_dir mfe_dir with
  | Error msg ->
      List.map
        (fun (label, _) ->
          let r = { church_rosser = Error msg; coherence = Error msg } in
          on_result label r 0.0;
          (label, r))
        slices
  | Ok mfe_dir ->
      let bin = resolve_bin maude_bin in
      let mfe_path = Subproc.absolute (Filename.concat mfe_dir mfe_entry) in
      let env = child_env bin in
      let load_budget = 240 in
      let start_session () =
        let s = Subproc.session_start ~env ~cmd:[ bin; "-no-banner" ] () in
        Subproc.session_send s (Printf.sprintf "load %s\n" mfe_path);
        s
      in
      let session = ref (Some (start_session ())) in
      let first = ref true in
      let check_one idx (label, slice) =
        let s =
          match !session with
          | Some s -> s
          | None ->
              let s = start_session () in
              session := Some s;
              first := true;
              s
        in
        let name = Printf.sprintf "S_%d" idx in
        let module_text =
          To_mfe.module_of_system ~module_name:name ~prune_signature ?sig_rules
            orig slice
        in
        (* Wall-clock the send+read -- the actual checker work. The first symbol
           in a (re)spawned session additionally pays the ~100s Full Maude load,
           since [load] executes lazily on the session's first read. *)
        let (block, timed_out), elapsed =
          Subproc.timed (fun () ->
              Subproc.session_send s
                (Printf.sprintf "%s%s\n" module_text
                   (String.concat "\n" (check_commands_for name)));
              let budget = if !first then timeout + load_budget else timeout in
              first := false;
              Subproc.session_read s ~done_when:batch_checks_done
                ~timeout:budget)
        in
        let r =
          if timed_out then (
            Subproc.session_kill s;
            session := None;
            { church_rosser = Timeout; coherence = Timeout })
          else
            let norm = Subproc.normalize_ws block in
            {
              church_rosser = crc_verdict ~timed_out:false norm;
              coherence = chc_verdict ~timed_out:false norm;
            }
        in
        on_result label r elapsed;
        (label, r)
      in
      let results = List.mapi check_one slices in
      (match !session with Some s -> Subproc.session_kill s | None -> ());
      results
