open Core

type item =
  | Durable of Agent_protocol.Event.Durable.t
  | Recoverable of Agent_protocol.Event.Recoverable.t

type t =
  { mailbox : item Mailbox.t
  ; mutex : Eio.Mutex.t
  ; mutable snapshot_required : bool
  ; mutable closed : bool
  }

let create ~capacity =
  { mailbox = Mailbox.create ~capacity
  ; mutex = Eio.Mutex.create ()
  ; snapshot_required = false
  ; closed = false
  }
;;

let publish_durable t event =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if not t.closed
    then (
      let accepted = Mailbox.try_push t.mailbox ~priority:Normal (Durable event) in
      if not accepted then t.snapshot_required <- true))
;;

let publish_recoverable t event =
  Eio.Mutex.use_ro t.mutex (fun () ->
    if not t.closed
    then ignore (Mailbox.try_push t.mailbox ~priority:Normal (Recoverable event) : bool))
;;

let snapshot_error () =
  Agent_protocol.Error.create
    Snapshot_required
    ~message:"subscriber fell behind durable event retention"
    ~retryable:true
    ()
;;

let take t =
  let snapshot_required =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let value = t.snapshot_required in
      t.snapshot_required <- false;
      value)
  in
  if snapshot_required
  then Some (Error (snapshot_error ()))
  else Option.map (Mailbox.pop t.mailbox) ~f:Result.return
;;

let close t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.closed <- true);
  Mailbox.close t.mailbox
;;

let needs_snapshot t = Eio.Mutex.use_ro t.mutex (fun () -> t.snapshot_required)
