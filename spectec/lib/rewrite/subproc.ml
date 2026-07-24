(* Deadline subprocess runner shared by the analysis-tool bridges ({!Mfe},
   {!Scc}, {!Aprove}). Extracted verbatim from {!Mfe}'s calibrated MFE runner;
   see subproc.mli for the discipline it encodes. *)

let timed (f : unit -> 'a) : 'a * float =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  (r, Unix.gettimeofday () -. t0)

let contains (s : string) (sub : string) : bool =
  let ns = String.length s and nb = String.length sub in
  let rec go i =
    if i + nb > ns then false
    else if String.sub s i nb = sub then true
    else go (i + 1)
  in
  go 0

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

let absolute (path : string) : string =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let run ?env ?(done_when = fun _ -> false) ~(cmd : string list) ~(feed : string)
    ~(timeout : int) () : string * bool =
  let env = match env with Some e -> e | None -> Unix.environment () in
  (* Feed stdin from a temp file (not a pipe), so a feed larger than the pipe
     buffer cannot deadlock against the unread stdout. *)
  let infile = Filename.temp_file "spectec_subproc" ".in" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove infile with Sys_error _ -> ())
    (fun () ->
      let oc = open_out infile in
      output_string oc feed;
      close_out oc;
      let fd_in = Unix.openfile infile [ Unix.O_RDONLY ] 0 in
      let r_out, w_out = Unix.pipe () in
      (* Launch through a shell that first lifts the stack limit: the default
         8MB stack is too small for legitimately-deep (not runaway) critical-
         pair/tree-automaton/reduction computations, which otherwise die as a
         native "Fatal error: stack overflow" and read as a missing verdict --
         the same lesson check_diff_structural_p4.sh already encodes for plain
         reductions (e73fcb44). *)
      let pid =
        Unix.create_process_env "/bin/sh"
          (Array.of_list
             ([
                "/bin/sh";
                "-c";
                "ulimit -s unlimited 2>/dev/null; exec \"$@\"";
                "sh";
              ]
             @ cmd))
          env fd_in w_out w_out
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
          | [], _, _ -> if not (done_when (Buffer.contents buf)) then loop ()
          | _ ->
              let n = Unix.read r_out chunk 0 (Bytes.length chunk) in
              if n = 0 then () (* child closed stdout: run finished *)
              else (
                Buffer.add_subbytes buf chunk 0 n;
                if not (done_when (Buffer.contents buf)) then loop ())
      in
      loop ();
      Unix.close r_out;
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      ignore (Unix.waitpid [] pid);
      (Buffer.contents buf, !timed_out))

(* -------------------------------------------------------------------------- *)
(* Persistent session: keep one child alive across many send/read cycles, for
   batched sweeps that would otherwise pay the child's startup once per symbol
   (a Full Maude load is ~100s). Unlike {!run}, stdin is a live pipe written
   incrementally; [session_send] drains stdout while writing so a feed larger
   than the pipe buffer cannot deadlock against the unread output. *)

type session = {
  pid : int;
  w_in : Unix.file_descr;
  r_out : Unix.file_descr;
  buf : Buffer.t;
  chunk : bytes;
}

let session_start ?env ~(cmd : string list) () : session =
  let env = match env with Some e -> e | None -> Unix.environment () in
  let r_in, w_in = Unix.pipe () in
  let r_out, w_out = Unix.pipe () in
  let pid =
    Unix.create_process_env "/bin/sh"
      (Array.of_list
         ([
            "/bin/sh";
            "-c";
            "ulimit -s unlimited 2>/dev/null; exec \"$@\"";
            "sh";
          ]
         @ cmd))
      env r_in w_out w_out
  in
  Unix.close r_in;
  Unix.close w_out;
  { pid; w_in; r_out; buf = Buffer.create 65536; chunk = Bytes.create 8192 }

(* Non-blocking drain of whatever the child has already written. *)
let session_drain (s : session) : unit =
  let rec go () =
    match Unix.select [ s.r_out ] [] [] 0.0 with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> go ()
    | [], _, _ -> ()
    | _ ->
        let n = Unix.read s.r_out s.chunk 0 (Bytes.length s.chunk) in
        if n > 0 then (
          Buffer.add_subbytes s.buf s.chunk 0 n;
          go ())
  in
  go ()

let session_send (s : session) (data : string) : unit =
  let b = Bytes.of_string data in
  let total = Bytes.length b in
  let rec write_from off =
    if off >= total then ()
    else (
      session_drain s;
      match Unix.select [] [ s.w_in ] [] 0.1 with
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_from off
      | _, [], _ -> write_from off
      | _ ->
          let n = Unix.write s.w_in b off (min 65536 (total - off)) in
          write_from (off + n))
  in
  write_from 0

let session_read (s : session) ~(done_when : string -> bool) ~(timeout : int) :
    string * bool =
  let timed_out = ref false in
  let deadline =
    if timeout > 0 then Unix.gettimeofday () +. float_of_int timeout
    else infinity
  in
  let rec loop () =
    if done_when (Buffer.contents s.buf) then ()
    else
      let remaining = deadline -. Unix.gettimeofday () in
      if remaining <= 0.0 then timed_out := true
      else
        match Unix.select [ s.r_out ] [] [] (Float.min remaining 0.25) with
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
        | [], _, _ -> loop ()
        | _ ->
            let n = Unix.read s.r_out s.chunk 0 (Bytes.length s.chunk) in
            if n = 0 then () (* child closed stdout: dead *)
            else (
              Buffer.add_subbytes s.buf s.chunk 0 n;
              loop ())
  in
  loop ();
  let out = Buffer.contents s.buf in
  Buffer.clear s.buf;
  (out, !timed_out)

let session_kill (s : session) : unit =
  (try Unix.close s.w_in with Unix.Unix_error _ -> ());
  (try Unix.close s.r_out with Unix.Unix_error _ -> ());
  (try Unix.kill s.pid Sys.sigkill with Unix.Unix_error _ -> ());
  ignore (Unix.waitpid [] s.pid)
