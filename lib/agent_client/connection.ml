type t =
  { transport : Transport.t
  ; mutex : Eio.Mutex.t
  ; mutable closed : bool
  }

let create transport = { transport; mutex = Eio.Mutex.create (); closed = false }

let closed_error () =
  Agent_protocol.Error.create
    Interrupted
    ~message:"client connection is closed"
    ~retryable:true
    ()
;;

let request t command =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if t.closed then Error (closed_error ()) else Transport.request t.transport command)
;;

let next_notification t =
  if Eio.Mutex.use_ro t.mutex (fun () -> t.closed)
  then None
  else Transport.next_notification t.transport
;;

let close t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if not t.closed
    then (
      t.closed <- true;
      Transport.close t.transport))
;;
