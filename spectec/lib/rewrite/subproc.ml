(* Deadline subprocess runner shared by the analysis-tool bridges ({!Mfe},
   {!Aprove}). Extracted verbatim from {!Mfe}'s calibrated MFE runner;
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

(* The child is launched through a shell that does two things before exec'ing:

   - lifts the stack limit, because the default 8MB is too small for
     legitimately-deep (not runaway) critical-pair/tree-automaton/reduction
     computations, which otherwise die as a native "Fatal error: stack overflow"
     and read as a missing verdict -- the same lesson check_diff_structural_p4.sh
     already encodes for plain reductions (e73fcb44);

   - puts the child in a NEW SESSION, so [kill_tree] can signal the whole
     process group rather than one pid. Our [cmd] heads are shell wrappers that
     [exec] into the real tool ({!Aprove}'s [runme] execs java), so [pid] is the
     tool itself and a plain SIGKILL does reach it -- but SIGKILL leaves it no
     chance to reap ITS children, and AProVE's z3/minisat backend is a child.
     Those survive as orphans pinned to a core: a 650-symbol [termination --all]
     sweep on 2026-08-13 leaked 25 of them (~24 of 32 cores) within its first
     20 minutes and then measured its remaining 3 hours against its own load,
     turning 11 YES verdicts into TIMEOUT with no change in the surface. Since
     these verdicts are budget-based, that is not a slow run but a wrong answer.

   [setsid] does not fork when the caller is not already a process group leader
   (ours never is -- it inherits ours), so [pid] stays the tool's pid and
   becomes its own group leader. Where [setsid] is missing the shell execs
   directly and [kill_tree] degrades to the old single-pid kill. *)
let launcher =
  "ulimit -s unlimited 2>/dev/null\n\
   if command -v setsid >/dev/null 2>&1; then exec setsid \"$@\"; else exec \
   \"$@\"; fi"

(* Group first, then the pid alone: the group kill covers the whole tree when
   the child leads its own group, and the pid kill is what still works when it
   does not (no [setsid], or a [setsid] that forked). Both are best-effort --
   ESRCH just means it is already gone. *)
let kill_tree (pid : int) : unit =
  (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> ());
  try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()

(* Retain only the lines [keep] accepts, out of a stream whose bulk is not worth
   holding. AProVE's obligation echo reprints the whole rule set once per
   dependency-graph child -- gigabytes, of which the dependency pairs are a few
   thousand lines -- and accumulating that verbatim would cost more memory than
   the proof it describes. Lines are handed over complete and in order, so the
   filter may be stateful (which section of the output it is inside is exactly
   what decides a line's fate); a partial trailing line is held back until its
   newline arrives, and dropped if the deadline kills the child first. *)
let line_splitter (keep : string -> bool) (buf : Buffer.t) :
    (bytes -> int -> unit) * (unit -> unit) =
  let pending = Buffer.create 4096 in
  let emit line =
    if keep line then (
      Buffer.add_string buf line;
      Buffer.add_char buf '\n')
  in
  let absorb chunk n =
    for i = 0 to n - 1 do
      let c = Bytes.get chunk i in
      if c = '\n' then (
        let line = Buffer.contents pending in
        Buffer.clear pending;
        emit line)
      else Buffer.add_char pending c
    done
  in
  let flush () =
    if Buffer.length pending > 0 then (
      let line = Buffer.contents pending in
      Buffer.clear pending;
      emit line)
  in
  (absorb, flush)

type result = {
  output : string;
  stderr : string;
  timed_out : bool;
  stopped : bool;
  status : Unix.process_status;
}

let run_process ~env ~keep ~done_when ~stderr_fd ~(cmd : string list)
    ~(feed : string) ~(timeout : int) () : result =
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
      let pid =
        Unix.create_process_env "/bin/sh"
          (Array.of_list ([ "/bin/sh"; "-c"; launcher; "sh" ] @ cmd))
          env fd_in w_out
          (Option.value stderr_fd ~default:w_out)
      in
      Unix.close fd_in;
      Unix.close w_out;
      let buf = Buffer.create 65536 in
      let chunk = Bytes.create 8192 in
      let absorb, flush =
        match keep with
        | None ->
            ((fun chunk n -> Buffer.add_subbytes buf chunk 0 n), fun () -> ())
        | Some keep -> line_splitter keep buf
      in
      (* Materialize the buffer only for a caller that actually reads it: the
         poll runs once per chunk, and a capture run streams gigabytes past in
         8KB chunks, so copying the accumulated output each time would cost more
         than the run. *)
      let poll =
        match done_when with
        | None -> fun () -> false
        | Some done_when -> fun () -> done_when (Buffer.contents buf)
      in
      let timed_out = ref false in
      let stopped = ref false in
      let stop () =
        let answer = poll () in
        if answer then stopped := true;
        answer
      in
      let reached_eof = ref false in
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
          | [], _, _ -> if not (stop ()) then loop ()
          | _ ->
              let n = Unix.read r_out chunk 0 (Bytes.length chunk) in
              if n = 0 then
                reached_eof := true (* child closed stdout: run finished *)
              else (
                absorb chunk n;
                if not (stop ()) then loop ())
      in
      loop ();
      (* A tool may end its final meaningful line directly at EOF. Retain that
         line only when the child really closed stdout; on a deadline or an
         early [done_when] kill, [pending] may be a truncated line. *)
      if !reached_eof then flush ();
      Unix.close r_out;
      let rec finish () =
        match Unix.waitpid [ Unix.WNOHANG ] pid with
        | 0, _ ->
            if !stopped || !timed_out then (
              kill_tree pid;
              snd (Unix.waitpid [] pid))
            else if Unix.gettimeofday () >= deadline then (
              timed_out := true;
              finish ())
            else (
              Unix.sleepf 0.01;
              finish ())
        | _, status -> status
      in
      let status = finish () in
      kill_tree pid;
      {
        output = Buffer.contents buf;
        stderr = "";
        timed_out = !timed_out;
        stopped = !stopped;
        status;
      })

let run ?env ?keep ?done_when ~cmd ~feed ~timeout () : string * bool =
  let result =
    run_process ~env ~keep ~done_when ~stderr_fd:None ~cmd ~feed ~timeout ()
  in
  (result.output, result.timed_out)

let run_detailed ~env ~keep ~done_when ~cmd ~feed ~timeout () : result =
  let path = Filename.temp_file "spectec_stderr" ".log" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
      let result =
        Fun.protect
          ~finally:(fun () -> Unix.close fd)
          (fun () ->
            run_process ~env ~keep ~done_when ~stderr_fd:(Some fd) ~cmd ~feed
              ~timeout ())
      in
      let ic = open_in_bin path in
      let stderr =
        Fun.protect
          ~finally:(fun () -> close_in ic)
          (fun () -> really_input_string ic (in_channel_length ic))
      in
      { result with stderr })

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
      (Array.of_list ([ "/bin/sh"; "-c"; launcher; "sh" ] @ cmd))
      env r_in w_out w_out
  in
  Unix.close r_in;
  Unix.close w_out;
  (* Non-blocking writes, so a deadline can actually be enforced: [select]
     reporting the pipe writable only promises SOME space, and a blocking
     [Unix.write] of a full chunk then sleeps inside the kernel until the child
     has drained the rest -- past any deadline the caller checks between
     writes. With [O_NONBLOCK] the write returns short (or [EAGAIN]) and the
     loop gets to look at the clock. *)
  Unix.set_nonblock w_in;
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

let deadline_in (budget : int) : float =
  if budget > 0 then Unix.gettimeofday () +. float_of_int budget else infinity

(* The write half is deadlined for the same reason the read half is: the child
   drains its stdin only as fast as it parses, so a large feed blocks the
   writer, and a budget that covers only the read bounds nothing. *)
let session_send (s : session) (data : string) ~(deadline : float) : bool =
  let b = Bytes.of_string data in
  let total = Bytes.length b in
  let rec write_from off =
    if off >= total then true
    else if deadline -. Unix.gettimeofday () <= 0.0 then false
    else (
      session_drain s;
      match Unix.select [] [ s.w_in ] [] 0.1 with
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_from off
      | _, [], _ -> write_from off
      | _ -> (
          match Unix.write s.w_in b off (min 65536 (total - off)) with
          | exception
              Unix.Unix_error
                ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) ->
              write_from off
          | n -> write_from (off + n)))
  in
  write_from 0

let session_read (s : session) ~(done_when : string -> bool) ~(deadline : float)
    : string * bool =
  let timed_out = ref false in
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
  kill_tree s.pid;
  ignore (Unix.waitpid [] s.pid)
