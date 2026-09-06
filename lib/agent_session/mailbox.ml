open Core

type priority =
  | Priority
  | Normal

type 'a t =
  { capacity : int
  ; mutex : Eio.Mutex.t
  ; not_empty : Eio.Condition.t
  ; not_full : Eio.Condition.t
  ; priority : 'a Queue.t
  ; normal : 'a Queue.t
  ; mutable closed : bool
  }

let create ~capacity =
  if capacity <= 0 then invalid_arg "mailbox capacity must be positive";
  { capacity
  ; mutex = Eio.Mutex.create ()
  ; not_empty = Eio.Condition.create ()
  ; not_full = Eio.Condition.create ()
  ; priority = Queue.create ()
  ; normal = Queue.create ()
  ; closed = false
  }
;;

let length_locked t = Queue.length t.priority + Queue.length t.normal

let enqueue t priority value =
  match priority with
  | Priority -> Queue.enqueue t.priority value
  | Normal -> Queue.enqueue t.normal value
;;

let full_error () =
  Agent_protocol.Error.create
    Command_queue_full
    ~message:"session command mailbox is full"
    ~retryable:true
    ()
;;

let closed_error () =
  Agent_protocol.Error.create
    Server_shutting_down
    ~message:"session command mailbox is closed"
    ~retryable:true
    ()
;;

let push t ~priority value =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if t.closed
    then Error (closed_error ())
    else if length_locked t >= t.capacity
    then Error (full_error ())
    else (
      enqueue t priority value;
      Eio.Condition.broadcast t.not_empty;
      Ok ()))
;;

let try_push t ~priority value = Result.is_ok (push t ~priority value)

let dequeue t =
  match Queue.dequeue t.priority with
  | Some value -> Some value
  | None -> Queue.dequeue t.normal
;;

let rec await_value t =
  match dequeue t with
  | Some value -> Some value
  | None when t.closed -> None
  | None ->
    Eio.Condition.await t.not_empty t.mutex;
    await_value t
;;

let pop t =
  let outcome =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
      match await_value t with
      | value ->
        Option.iter value ~f:(fun _ -> Eio.Condition.broadcast t.not_full);
        Ok value
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | Ok value -> value
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let close t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.closed <- true;
    Eio.Condition.broadcast t.not_empty;
    Eio.Condition.broadcast t.not_full)
;;

let length t = Eio.Mutex.use_ro t.mutex (fun () -> length_locked t)
