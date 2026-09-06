open! Core

type t =
  { event_capacity : int
  ; mutex : Eio.Mutex.t
  ; mutable state : Session_state.t
  ; mutable events : Agent_protocol.Event.Durable.t Fqueue.t
  ; archives : (int64, Session_state.t) Hashtbl.t
  }

let create ~event_capacity ~initial_state =
  if event_capacity <= 0 then invalid_arg "event capacity must be positive";
  { event_capacity
  ; mutex = Eio.Mutex.create ()
  ; state = initial_state
  ; events = Fqueue.empty
  ; archives = Hashtbl.create (module Int64)
  }
;;

let trim t =
  while Fqueue.length t.events > t.event_capacity do
    t.events <- Fqueue.drop_exn t.events
  done
;;

let commit t ~(previous : Session_state.t) transition =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if Int64.(previous.counters.revision <> t.state.counters.revision)
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"memory backend revision changed before commit"
           ~retryable:true
           ())
    else (
      List.iter
        transition.Session_transition.state.conversation.compaction_archives
        ~f:(fun archive ->
          if not (Hashtbl.mem t.archives archive.revision)
          then Hashtbl.set t.archives ~key:archive.revision ~data:previous);
      t.state <- transition.Session_transition.state;
      List.iter transition.events ~f:(fun event ->
        t.events <- Fqueue.enqueue t.events event);
      trim t;
      Ok ()))
;;

let persistence t =
  Session_actor.
    { commit = (fun ~command_audit:_ ~previous value -> commit t ~previous value) }
;;

let state t = Eio.Mutex.use_ro t.mutex (fun () -> t.state)

let archived_state t ~revision =
  Eio.Mutex.use_ro t.mutex (fun () -> Hashtbl.find t.archives revision)
;;

let events_after t sequence =
  Eio.Mutex.use_ro t.mutex (fun () ->
    let values = Fqueue.to_list t.events in
    match values with
    | first :: _ when Int64.(sequence < first.sequence - 1L) ->
      Error
        (Agent_protocol.Error.create
           Snapshot_required
           ~message:"requested event cursor is outside memory retention"
           ~retryable:true
           ())
    | _ -> Ok (List.filter values ~f:(fun event -> Int64.(event.sequence > sequence))))
;;
