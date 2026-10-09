open! Core

type t =
  { store : Agent_store.Session_store.t
  ; handle : Agent_store.Session_store.Handle.t
  ; job_capacity : Job_capacity.t
  ; mutable capacity : Session_capacity.t option
  ; mutable writer : Agent_store.Commit_writer.t option
  ; mutable actor : Agent_session.Session_actor.t option
  ; mutable runtime : Runtime_owner.t option
  ; entry_close_mutex : Eio.Mutex.t
  ; close_mutex : Eio.Mutex.t
  ; mutable closing : bool
  ; mutable runtime_joined : bool
  ; mutable jobs_closed : bool
  ; mutable actor_closed : bool
  ; mutable capacity_released : bool
  ; mutable writer_closed : bool
  ; mutable handle_closed : bool
  ; mutable checkpoint_failure : Agent_protocol.Error.t option
  }

let create ~store ~handle ~job_capacity ~capacity =
  if not (Agent_store.Session_store.owns_handle store handle)
  then
    Error
      (Agent_protocol.Error.create
         Persistence_error
         ~message:"recovery resources belong to a different store"
         ~retryable:false
         ())
  else
    Ok
      { store
      ; handle
      ; job_capacity
      ; capacity
      ; writer = None
      ; actor = None
      ; runtime = None
      ; entry_close_mutex = Eio.Mutex.create ()
      ; close_mutex = Eio.Mutex.create ()
      ; closing = false
      ; runtime_joined = false
      ; jobs_closed = false
      ; actor_closed = false
      ; capacity_released = false
      ; writer_closed = false
      ; handle_closed = false
      ; checkpoint_failure = None
      }
;;

let session_id t = Agent_store.Session_store.Handle.session_id t.handle
let handle t = t.handle
let owns_actor t actor = Option.exists t.actor ~f:(phys_equal actor)
let is_closing t = t.closing
let checkpoint_failure t = t.checkpoint_failure
let record_checkpoint_failure t failure = t.checkpoint_failure <- Some failure

let adopt_capacity_exn t capacity =
  if t.closing || Option.is_some t.capacity || Option.is_some t.writer
  then failwith "recovery capacity ownership is out of order";
  t.capacity <- Some capacity
;;

let adopt_writer_exn t writer =
  if t.closing || Option.is_some t.writer
  then failwith "recovery writer ownership has already transferred";
  t.writer <- Some writer
;;

let adopt_actor_exn t actor =
  if t.closing || Option.is_none t.writer || Option.is_some t.actor
  then failwith "recovery actor ownership is out of order";
  t.actor <- Some actor
;;

let adopt_runtime_exn t runtime =
  if t.closing || Option.is_none t.actor || Option.is_some t.runtime
  then failwith "recovery runtime ownership is out of order";
  t.runtime <- Some runtime
;;

let prepare_close_unlocked t =
  Eio.Cancel.protect (fun () ->
    t.closing <- true;
    if not t.runtime_joined
    then (
      Option.iter t.runtime ~f:Runtime_owner.close_and_wait;
      t.runtime_joined <- true))
;;

let prepare_close t =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_ro t.close_mutex (fun () -> prepare_close_unlocked t))
;;

let close t =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_ro t.close_mutex (fun () ->
      prepare_close_unlocked t;
      if not t.jobs_closed
      then (
        Job_capacity.close_session t.job_capacity ~session_id:(session_id t);
        t.jobs_closed <- true);
      if not t.actor_closed
      then (
        Option.iter t.actor ~f:Agent_session.Session_actor.shutdown;
        t.actor_closed <- true);
      if not t.capacity_released
      then (
        Option.iter t.capacity ~f:Session_capacity.release;
        t.capacity_released <- true);
      if not t.writer_closed
      then (
        Option.iter t.writer ~f:Agent_store.Commit_writer.close;
        t.writer_closed <- true);
      if t.handle_closed
      then Ok ()
      else
        Agent_store.Session_store.close_session t.store t.handle
        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
        |> Result.map ~f:(fun () -> t.handle_closed <- true)))
;;

let with_entry_close t f =
  Eio.Cancel.protect (fun () -> Eio.Mutex.use_ro t.entry_close_mutex f)
;;
