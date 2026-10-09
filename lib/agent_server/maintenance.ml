open! Core

module Collection_policy = struct
  type t =
    | Load_retained
    | Selected_only
end

type stats =
  { expired_idempotency_records : int
  ; expired_temporary_blobs : int
  ; expired_response_artifacts : int
  ; discarded_job_results : int
  ; retired_job_preparations : int
  ; deferred_result_collections : int
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

let has_preparations ~env session_store session_id =
  let open Result.Let_syntax in
  let root =
    Agent_store.Data_root.session_path
      (Agent_store.Session_store.data_root session_store)
      session_id
  in
  let probe =
    let%bind reader =
      Agent_store.Retention_reader.create ~env ~root ~max_entries:128 ~max_bytes:0
    in
    let%bind names = Agent_store.Retention_reader.list reader ~directory:"." in
    match List.mem names "result-preparations" ~equal:String.equal with
    | false -> Ok false
    | true ->
      Result.map
        (Agent_store.Retention_reader.list reader ~directory:"result-preparations")
        ~f:(fun names -> not (List.is_empty names))
  in
  (* The cheap probe supplies no deletion proof. Excess or unreadable entries
     must reach the full validated attempt rather than disappear from maintenance. *)
  match probe with
  | Ok found -> found
  | Error _ -> true
;;

let collect_results ~env ~session_store ~collection_policy registry stats =
  match registry with
  | None -> Ok stats
  | Some registry ->
    let open Result.Let_syntax in
    let%bind entries = Agent_store.Session_store.list_sessions_checked session_store in
    let stats, failure =
      List.fold entries ~init:(stats, None) ~f:(fun (stats, failure) entry ->
        match
          entry.Agent_store.Session_index.Entry.archived
          || Agent_store.Session_archive_record.Admission.equal
               entry.admission
               Explicit_resume_required
          || not (has_preparations ~env session_store entry.session.id)
        with
        | true -> stats, failure
        | false ->
          let result =
            match collection_policy with
            | Collection_policy.Load_retained ->
              Result.bind
                (Session_registry.load registry entry.session.id)
                ~f:(fun entry -> entry.collect_results ())
            | Selected_only ->
              (match Session_registry.find registry entry.session.id with
               | None -> Ok None
               | Some actual -> actual.collect_results ())
          in
          (match result with
           | Error error ->
             ( stats
             , Some
                 (Option.value
                    failure
                    ~default:(Agent_store.Store_error.Corrupt error.message)) )
           | Ok None ->
             ( { stats with
                 deferred_result_collections = stats.deferred_result_collections + 1
               }
             , failure )
           | Ok (Some collected) ->
             ( { stats with
                 discarded_job_results = stats.discarded_job_results + collected.discarded
               ; retired_job_preparations =
                   stats.retired_job_preparations + collected.retired
               }
             , failure )))
    in
    (match failure with
     | None -> Ok stats
     | Some error -> Error error)
;;

let run_once
      ~collection_policy
      ~env
      ~idempotency_store
      ~blob_store
      ~session_store
      ~registry
      ~protected_response_sessions
      ~response_retention
      ~now
  =
  let open Result.Let_syntax in
  let%bind expired_idempotency_records =
    Agent_store.Idempotency_store.prune_expired idempotency_store ~now
  in
  let%bind expired_temporary_blobs =
    Agent_store.Blob_store.cleanup_expired
      blob_store
      ~now
      ~protect:
        (Agent_store.Job_result_intent.protects_temporary
           ~env
           ~data_root:(Agent_store.Session_store.data_root session_store))
  in
  let%bind expired_response_artifacts =
    Agent_store.Session_store.prune_response_artifacts
      session_store
      ~protected:protected_response_sessions
      ~older_than:(retention_cutoff now response_retention)
  in
  collect_results
    ~collection_policy
    ~env
    ~session_store
    registry
    { expired_idempotency_records
    ; expired_temporary_blobs
    ; expired_response_artifacts
    ; discarded_job_results = 0
    ; retired_job_preparations = 0
    ; deferred_result_collections = 0
    }
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
          collection_policy
          env
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
        ~collection_policy
        ~env
        ~idempotency_store
        ~blob_store
        ~session_store
        ~registry:(Some registry)
        ~protected_response_sessions:(protected_response_sessions registry)
        ~response_retention
        ~now
    in
    record_result t now result;
    Result.iter_error result ~f:on_error;
    (match Agent_store.Session_store.list_sessions_checked session_store with
     | Error error -> on_error error
     | Ok entries ->
       ignore (Session_registry.unload_inactive registry ~index_entries:entries : int));
    loop
      t
      collection_policy
      env
      clock
      every
      idempotency_store
      blob_store
      response_retention
      registry
      session_store
      on_error
;;

let start_controlled
      ~collection_policy
      ~enabled
      ~sw
      ~env
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
    { closed = Atomic.make (not enabled)
    ; stop = Eio.Stream.create 1
    ; mutex = Eio.Mutex.create ()
    ; last_stats = None
    ; last_success_at = None
    ; last_error = None
    }
  in
  if enabled
  then
    Eio.Fiber.fork ~sw (fun () ->
      loop
        t
        collection_policy
        env
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

let start
      ~collection_policy
      ~sw
      ~env
      ~clock
      ~every
      ~idempotency_store
      ~blob_store
      ~response_retention
      ~registry
      ~session_store
      ~on_error
  =
  start_controlled
    ~collection_policy
    ~enabled:true
    ~sw
    ~env
    ~clock
    ~every
    ~idempotency_store
    ~blob_store
    ~response_retention
    ~registry
    ~session_store
    ~on_error
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
