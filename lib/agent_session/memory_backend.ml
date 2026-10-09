open! Core

type t =
  { event_capacity : int
  ; mutex : Eio.Mutex.t
  ; mutable state : Session_state.t
  ; mutable events : Agent_protocol.Event.Durable.t Fqueue.t
  ; archives : (int64, Session_state.t) Hashtbl.t
  ; pending_archives : (Agent_protocol.Id.Operation.t, Pending_archive.t) Hashtbl.t
  }

let create ~event_capacity ~initial_state =
  if event_capacity <= 0 then invalid_arg "event capacity must be positive";
  { event_capacity
  ; mutex = Eio.Mutex.create ()
  ; state = initial_state
  ; events = Fqueue.empty
  ; archives = Hashtbl.create (module Int64)
  ; pending_archives = Hashtbl.create (module Agent_protocol.Id.Operation)
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
    else
      let open Result.Let_syntax in
      let%map archives =
        Pending_archive_transition.collect
          previous
          ~delta:transition.Session_transition.delta
          ~limits:Session_delta.native_limits
      in
      List.iter archives ~f:(fun archive ->
        Hashtbl.set
          t.pending_archives
          ~key:
            (Pending_archive.Reference.operation_id (Pending_archive.reference archive))
          ~data:archive);
      List.iter
        transition.Session_transition.state.conversation.compaction_archives
        ~f:(fun archive ->
          if not (Hashtbl.mem t.archives archive.revision)
          then Hashtbl.set t.archives ~key:archive.revision ~data:previous);
      t.state <- transition.Session_transition.state;
      List.iter transition.events ~f:(fun event ->
        t.events <- Fqueue.enqueue t.events event);
      trim t)
;;

let persistence t =
  Session_actor.
    { commit = (fun ~command_audit:_ ~previous value -> commit t ~previous value)
    ; archive_reference =
        (fun ~previous ~kind id ->
          Compaction_archive.reference_for
            (Session_state_document.authored previous)
            ~limits:Document_schema.Limits.default
            ~kind
            id)
    }
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

let pending_archive t ~reference =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match
      Hashtbl.find t.pending_archives (Pending_archive.Reference.operation_id reference)
    with
    | Some archive
      when Pending_archive.Reference.equal reference (Pending_archive.reference archive)
      -> Some archive
    | Some _ | None -> None)
;;
