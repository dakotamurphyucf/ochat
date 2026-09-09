open! Core

type stats =
  { expired_idempotency_records : int
  ; expired_temporary_blobs : int
  ; expired_response_artifacts : int
  }
[@@deriving sexp]

type wake =
  | Tick
  | Stop

type t =
  { closed : bool Atomic.t
  ; stop : unit Eio.Stream.t
  ; mutex : Eio.Mutex.t
  ; mutable last_stats : stats option
  ; mutable last_success_at : Agent_protocol.Timestamp.t option
  ; mutable last_error : Agent_store.Store_error.t option
  }

type status =
  { running : bool
  ; last_stats : stats option
  ; last_success_at : Agent_protocol.Timestamp.t option
  ; last_error : Agent_store.Store_error.t option
  }
[@@deriving sexp]

let timestamp clock =
  Eio.Time.now clock
  |> Time_ns.Span.of_sec
  |> Time_ns.of_span_since_epoch
  |> Agent_protocol.Timestamp.of_time_ns
;;

let retention_cutoff now retention =
  Agent_protocol.Timestamp.to_time_ns now
  |> Fn.flip Time_ns.sub retention
  |> Agent_protocol.Timestamp.of_time_ns
;;

let run_once
      ~idempotency_store
      ~blob_store
      ~session_store
      ~protected_response_sessions
      ~response_retention
      ~now
  =
  let open Result.Let_syntax in
  let%bind expired_idempotency_records =
    Agent_store.Idempotency_store.prune_expired idempotency_store ~now
  in
  let%bind expired_temporary_blobs =
    Agent_store.Blob_store.cleanup_expired blob_store ~now
  in
  let%map expired_response_artifacts =
    Agent_store.Session_store.prune_response_artifacts
      session_store
      ~protected:protected_response_sessions
      ~older_than:(retention_cutoff now response_retention)
  in
  { expired_idempotency_records; expired_temporary_blobs; expired_response_artifacts }
;;

let wait t clock every =
  Eio.Fiber.first
    (fun () ->
       Eio.Time.sleep clock every;
       Tick)
    (fun () ->
       Eio.Stream.take t.stop;
       Stop)
;;

let record_result t now result =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match result with
    | Ok stats ->
      t.last_stats <- Some stats;
      t.last_success_at <- Some now;
      t.last_error <- None
    | Error error -> t.last_error <- Some error)
;;

let job_uses_response_artifacts (job : Agent_protocol.Job.t) =
  match job.status with
  | Running | Waiting_permission _ | Waiting_completion _ -> true
  | Queued | Succeeded | Failed _ | Cancelled | Interrupted _ -> false
;;

let session_uses_response_artifacts state =
  Option.is_some state.Agent_session.Session_state.active_operation
  || List.exists state.jobs ~f:job_uses_response_artifacts
;;

let protected_response_sessions registry =
  Session_registry.entries registry
  |> List.filter_map ~f:(fun entry ->
    match Agent_session.Session_actor.state entry.actor with
    | Error _ ->
      Option.map entry.store_handle ~f:Agent_store.Session_store.Handle.session_id
    | Ok state when session_uses_response_artifacts state ->
      Some state.identity.session_id
    | Ok _ -> None)
;;

let rec loop
          t
          clock
          every
          idempotency_store
          blob_store
          response_retention
          registry
          session_store
          on_error
  =
  match wait t clock every with
  | Stop -> ()
  | Tick ->
    let now = timestamp clock in
    let result =
      run_once
        ~idempotency_store
        ~blob_store
        ~session_store
        ~protected_response_sessions:(protected_response_sessions registry)
        ~response_retention
        ~now
    in
    record_result t now result;
    Result.iter_error result ~f:on_error;
    ignore
      (Session_registry.unload_inactive
         registry
         ~index_entries:(Agent_store.Session_store.list_sessions session_store)
       : int);
    loop
      t
      clock
      every
      idempotency_store
      blob_store
      response_retention
      registry
      session_store
      on_error
;;

let start
      ~sw
      ~clock
      ~every
      ~idempotency_store
      ~blob_store
      ~response_retention
      ~registry
      ~session_store
      ~on_error
  =
  let t =
    { closed = Atomic.make false
    ; stop = Eio.Stream.create 1
    ; mutex = Eio.Mutex.create ()
    ; last_stats = None
    ; last_success_at = None
    ; last_error = None
    }
  in
  Eio.Fiber.fork ~sw (fun () ->
    loop
      t
      clock
      every
      idempotency_store
      blob_store
      response_retention
      registry
      session_store
      on_error);
  t
;;

let close t = if Atomic.compare_and_set t.closed false true then Eio.Stream.add t.stop ()

let status t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    { running = not (Atomic.get t.closed)
    ; last_stats = t.last_stats
    ; last_success_at = t.last_success_at
    ; last_error = t.last_error
    })
;;
