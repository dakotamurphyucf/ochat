open! Core

type t =
  { capacity : int
  ; session_id : Agent_protocol.Id.Session.t
  ; operation_id : Agent_protocol.Id.Operation.t
  ; mutex : Eio.Mutex.t
  ; mutable next_sequence : int64
  ; mutable events : Agent_protocol.Event.Recoverable.t Fqueue.t
  }

let create ~capacity ~session_id ~operation_id =
  if capacity <= 0 then invalid_arg "live event capacity must be positive";
  { capacity
  ; session_id
  ; operation_id
  ; mutex = Eio.Mutex.create ()
  ; next_sequence = 1L
  ; events = Fqueue.empty
  }
;;

let trim t =
  if Fqueue.length t.events <= t.capacity
  then ()
  else t.events <- Fqueue.drop_exn t.events
;;

let publish t ~anchor_sequence ~timestamp ~kind ~payload =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let event =
      Agent_protocol.Event.Recoverable.
        { session_id = t.session_id
        ; operation_id = t.operation_id
        ; operation_sequence = t.next_sequence
        ; anchor_sequence
        ; timestamp
        ; kind
        ; payload
        }
    in
    t.next_sequence <- Int64.(t.next_sequence + 1L);
    t.events <- Fqueue.enqueue t.events event;
    trim t;
    event)
;;

let after t sequence =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Fqueue.to_list t.events
    |> List.filter ~f:(fun event -> Int64.(event.operation_sequence > sequence)))
;;

let latest_sequence t = Eio.Mutex.use_ro t.mutex (fun () -> Int64.(t.next_sequence - 1L))
