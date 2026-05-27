module Exn = Instrumentation_common.Exn

type session = Idle | Active of (module Instrumentation_api.Handler.S) list

let session = ref Idle

(* Per-test-case event buffer.  [None] = passthrough (normal mode);
   [Some q] = buffering mode: events are enqueued and forwarded to handlers
   only on [commit_buffer ()], or silently dropped on [drop_buffer ()].
   Call [begin_buffering ()] before evaluating a test case and
   [commit_buffer ()] / [drop_buffer ()] depending on whether the verdict is
   [Pass] or [Fail]/[Discard]. *)
let buffer : Instrumentation_api.Event.t Queue.t option ref = ref None

let finish_all_handlers handlers =
  handlers
  |> List.fold_left
       (fun first_error -> function
         | (module H : Instrumentation_api.Handler.S) ->
             Exn.try_record_first_error first_error H.finish)
       None
  |> Exn.raise_recorded_error

let rec init_handlers ~spec initialized = function
  | [] -> ()
  | ((module H : Instrumentation_api.Handler.S) as handler) :: rest -> (
      try
        H.init ~spec;
        init_handlers ~spec (handler :: initialized) rest
      with exn ->
        let captured = Exn.capture exn in
        (try finish_all_handlers initialized with _ -> ());
        Exn.raise_captured captured)

let init ~spec ~handlers =
  (match !session with
  | Idle -> ()
  | Active _ ->
      failwith "Instrumentation.Dispatcher.init: instrumentation already active");
  init_handlers ~spec [] handlers;
  session := Active handlers

let forward_to_session ev =
  match !session with
  | Active hs ->
      List.iter (fun (module H : Instrumentation_api.Handler.S) -> H.handle ev) hs
  | Idle -> ()

let emit (ev : Instrumentation_api.Event.t) : unit =
  match !buffer with
  | Some q -> (
      match ev with
      | Instrumentation_api.Event.Rel_enter _ ->
          (* Rel_enter must reach step-budget handlers immediately — they are
             added via [with_handler] and decrement a per-call budget on each
             relation entry.  Coverage handlers ignore Rel_enter, so bypassing
             the buffer here does not affect coverage correctness. *)
          forward_to_session ev
      | _ -> Queue.add ev q)
  | None -> forward_to_session ev

let begin_buffering () = buffer := Some (Queue.create ())

let commit_buffer () =
  match !buffer with
  | None -> ()
  | Some q ->
      buffer := None;
      Queue.iter forward_to_session q

let drop_buffer () = buffer := None

let finish () =
  match !session with
  | Idle -> ()
  | Active hs ->
      session := Idle;
      finish_all_handlers hs

(** Run [f ()] with [handler] temporarily added to the active session.
    If no session is active, initialises a fresh one for the duration. *)
let with_handler ~spec handler f =
  match !session with
  | Idle ->
      init ~spec ~handlers:[ handler ];
      let cleanup () = finish () in
      Exn.with_cleanup ~cleanup f
  | Active hs ->
      let (module H : Instrumentation_api.Handler.S) = handler in
      H.init ~spec;
      session := Active (hs @ [ handler ]);
      let restore () =
        (try H.finish () with _ -> ());
        session := Active hs
      in
      Exn.with_cleanup ~cleanup:restore f
