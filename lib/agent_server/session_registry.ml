open! Core

module Session_key = struct
  module T = struct
    type t = Agent_protocol.Id.Session.t [@@deriving sexp]

    let compare = Agent_protocol.Id.Session.compare
  end

  include T
  include Comparator.Make (T)
end

type entry =
  { actor : Agent_session.Session_actor.t
  ; history_ids : Agent_session.History_id_source.t
  ; runtime : Runtime_owner.t
  ; durable_events : Agent_session.Durable_event_log.t
  ; capacity : Session_capacity.t option
  ; store_handle : Agent_store.Session_store.Handle.t option
  ; expire_permissions : now:Agent_protocol.Timestamp.t -> unit
  ; collect_results :
      unit
      -> ( Agent_store.Job_result_store.Publisher.collection_stats option
           , Agent_protocol.Error.t )
           result
  ; close : unit -> unit
  }

module Lifecycle_reservation = struct
  type target =
    | Loaded of entry
    | Indexed of Agent_store.Session_index.Entry.t
    | Absent

  type t =
    { owner : unit ref
    ; session_id : Agent_protocol.Id.Session.t
    ; target : target
    ; finished : unit Eio.Promise.t
    ; finish : unit Eio.Promise.u
    }

  let target t = t.target
end

module Read_lease = struct
  type t =
    { finished : unit Eio.Promise.t
    ; finish : unit Eio.Promise.u
    }

  let create () =
    let finished, finish = Eio.Promise.create () in
    { finished; finish }
  ;;
end

module Cleanup_failure = struct
  type t =
    | Rejected of Agent_protocol.Error.t
    | Raised of exn * Stdlib.Printexc.raw_backtrace

  let rejected error = Rejected error
  let raised exn backtrace = Raised (exn, backtrace)

  let error = function
    | Rejected error -> error
    | Raised (_, _) ->
      Agent_protocol.Error.create
        Persistence_error
        ~message:"owned session cleanup failed; retained diagnostic requires recovery"
        ~retryable:true
        ()
  ;;

  let exception_and_backtrace = function
    | Rejected _ -> None
    | Raised (exn, backtrace) -> Some (exn, backtrace)
  ;;
end

exception Cleanup_failed of Cleanup_failure.t

module Cleanup_owner = struct
  type t =
    | Entry of entry
    | Handle of Agent_store.Session_store.t * Agent_store.Session_store.Handle.t
    | Fence of entry * Agent_session.Session_actor.Lifecycle_fence.t

  let entry entry = Entry entry
  let handle ~store handle = Handle (store, handle)
  let fence entry fence = Fence (entry, fence)
end

let cleanup_owner = function
  | Cleanup_owner.Entry entry ->
    Runtime_owner.close_and_wait entry.runtime;
    entry.close ();
    Ok ()
  | Handle (store, handle) ->
    Agent_store.Session_store.close_session store handle
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  | Fence (entry, fence) -> Agent_session.Session_actor.abort_lifecycle entry.actor fence
;;

module Failed_cleanup = struct
  type t =
    { owner : Cleanup_owner.t
    ; session_id : Agent_protocol.Id.Session.t
    ; primary : Cleanup_failure.t
    ; failure : Cleanup_failure.t
    }
end

type t =
  { mutex : Eio.Mutex.t
  ; shutdown_mutex : Eio.Mutex.t
  ; lifecycle_owner : unit ref
  ; mutable reservations :
      ( Agent_protocol.Id.Session.t
        , Lifecycle_reservation.t
        , Session_key.comparator_witness )
        Map.t
  ; mutable read_leases : Read_lease.t list
  ; mutable failed_cleanups : Failed_cleanup.t list
  ; closing : bool Atomic.t
  ; sessions :
      (Agent_protocol.Id.Session.t, entry, Session_key.comparator_witness) Map.t Atomic.t
  ; mutable indexed :
      ( Agent_protocol.Id.Session.t
        , Agent_store.Session_index.Entry.t
        , Session_key.comparator_witness )
        Map.t
  ; mutable reader :
      (Agent_store.Session_index.Entry.t
       -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result)
        option
  ; mutable loader :
      (Agent_store.Session_index.Entry.t -> (entry, Agent_protocol.Error.t) result) option
  }

type stats =
  { loaded : int
  ; indexed : int
  }
[@@deriving sexp]

let create () =
  { mutex = Eio.Mutex.create ()
  ; shutdown_mutex = Eio.Mutex.create ()
  ; lifecycle_owner = ref ()
  ; reservations = Map.empty (module Session_key)
  ; read_leases = []
  ; failed_cleanups = []
  ; closing = Atomic.make false
  ; sessions = Atomic.make (Map.empty (module Session_key))
  ; indexed = Map.empty (module Session_key)
  ; reader = None
  ; loader = None
  }
;;

let cleanup_pending t session_id =
  List.exists t.failed_cleanups ~f:(fun cleanup ->
    Agent_protocol.Id.Session.equal cleanup.Failed_cleanup.session_id session_id)
;;

let install_loader t loader =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match Atomic.get t.closing with
    | true -> ()
    | false -> t.loader <- Some loader)
;;

let install_reader t reader =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if not (Atomic.get t.closing) then t.reader <- Some reader)
;;

let index t entry =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let session_id = entry.Agent_store.Session_index.Entry.session.id in
    if
      Atomic.get t.closing
      || (Map.mem t.reservations session_id || cleanup_pending t session_id)
      || Map.mem (Atomic.get t.sessions) session_id
    then ()
    else t.indexed <- Map.set t.indexed ~key:session_id ~data:entry)
;;

let index_all t entries = List.iter entries ~f:(index t)

let shutting_down () =
  Agent_protocol.Error.create
    Server_shutting_down
    ~message:"session registry is shutting down"
    ~retryable:true
    ()
;;

let add t ~session_id entry =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if Atomic.get t.closing
    then Error (shutting_down ())
    else if Map.mem t.reservations session_id || cleanup_pending t session_id
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"session lifecycle is reserved"
           ~retryable:true
           ())
    else if Map.mem (Atomic.get t.sessions) session_id
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"session is already registered"
           ~retryable:false
           ())
    else (
      Atomic.set t.sessions (Map.set (Atomic.get t.sessions) ~key:session_id ~data:entry);
      t.indexed <- Map.remove t.indexed session_id;
      Ok ()))
;;

let find t session_id = Map.find (Atomic.get t.sessions) session_id
let is_closing t = Atomic.get t.closing

let lifecycle_conflict message =
  Agent_protocol.Error.create Conflict ~message ~retryable:true ()
;;

let check_reservation t reservation =
  if
    phys_equal t.lifecycle_owner reservation.Lifecycle_reservation.owner
    && Option.exists (Map.find t.reservations reservation.session_id) ~f:(fun current ->
      phys_equal current reservation)
  then Ok ()
  else Error (lifecycle_conflict "session lifecycle reservation is no longer current")
;;

let with_lifecycle t session_id f =
  let open Result.Let_syntax in
  let%bind reservation =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if Atomic.get t.closing
      then Error (shutting_down ())
      else if Map.mem t.reservations session_id || cleanup_pending t session_id
      then Error (lifecycle_conflict "session lifecycle is already reserved")
      else (
        let target =
          match find t session_id, Map.find t.indexed session_id with
          | Some entry, _ -> Lifecycle_reservation.Loaded entry
          | None, Some indexed -> Indexed indexed
          | None, None -> Absent
        in
        let finished, finish = Eio.Promise.create () in
        let reservation =
          ({ owner = t.lifecycle_owner; session_id; target; finished; finish }
           : Lifecycle_reservation.t)
        in
        t.reservations <- Map.set t.reservations ~key:session_id ~data:reservation;
        Ok reservation))
  in
  Exn.protect
    ~f:(fun () -> f reservation)
    ~finally:(fun () ->
      Eio.Cancel.protect (fun () ->
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          match check_reservation t reservation with
          | Error _ -> ()
          | Ok () ->
            t.reservations <- Map.remove t.reservations session_id;
            Eio.Promise.resolve reservation.finish ())))
;;

let check_retained_owner entry =
  match entry.store_handle with
  | None -> Ok ()
  | Some handle ->
    Agent_store.Session_store.Handle.metadata_checked handle
    |> Result.map ~f:(fun _ -> ())
    |> Result.map_error ~f:(fun failure ->
      Agent_protocol.Error.create
        Persistence_error
        ~message:
          ("session observation owner is unavailable: "
           ^ Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t failure))
        ~retryable:false
        ())
;;

(* Actor identity is the retained ownership capability, not value equality. *)
let same_owner first second = phys_equal first.actor second.actor

let with_read_snapshot t ~capture ~check_current f =
  let open Result.Let_syntax in
  let%bind lease, snapshot =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if Atomic.get t.closing
      then Error (shutting_down ())
      else (
        let%map snapshot = capture () in
        let lease = Read_lease.create () in
        t.read_leases <- lease :: t.read_leases;
        lease, snapshot))
  in
  Exn.protect
    ~f:(fun () ->
      let%bind value = f snapshot in
      let%map () =
        Eio.Mutex.use_ro t.mutex (fun () ->
          if Atomic.get t.closing
          then Error (shutting_down ())
          else check_current snapshot)
      in
      value)
    ~finally:(fun () ->
      Eio.Cancel.protect (fun () ->
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          (* Physical identity compares this admitted read lifetime. *)
          t.read_leases
          <- List.filter t.read_leases ~f:(fun current -> not (phys_equal current lease));
          Eio.Promise.resolve lease.finish ())))
;;

let close_provisional t ~session_id entry result =
  let close primary =
    Eio.Cancel.protect (fun () ->
      match entry.close () with
      | () -> ()
      | exception cleanup_exception ->
        let cleanup_backtrace = Stdlib.Printexc.get_raw_backtrace () in
        let cleanup : Failed_cleanup.t =
          { owner = Cleanup_owner.entry entry
          ; session_id
          ; primary
          ; failure = Cleanup_failure.raised cleanup_exception cleanup_backtrace
          }
        in
        (* Retain both diagnostics and the actual owner before releasing its
           reservation. Shutdown must finish this cleanup before store release. *)
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          t.failed_cleanups <- cleanup :: t.failed_cleanups))
  in
  match result () with
  | Ok _ as success -> success
  | Error error as failure ->
    close (Cleanup_failure.Rejected error);
    failure
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    close (Cleanup_failure.Raised (exn, backtrace));
    Exn.raise_with_original_backtrace exn backtrace
;;

let load t session_id =
  let open Result.Let_syntax in
  with_lifecycle t session_id (fun reservation ->
    match reservation.Lifecycle_reservation.target with
    | Loaded entry ->
      let%map () = check_retained_owner entry in
      entry
    | Absent ->
      Error
        (Agent_protocol.Error.create
           Session_not_found
           ~message:"session does not exist"
           ~retryable:false
           ())
    | Indexed indexed ->
      let%bind () =
        if
          indexed.archived
          || not
               (Agent_store.Session_archive_record.Admission.equal
                  indexed.admission
                  Automatic)
        then
          Error
            (Agent_protocol.Error.create
               Invalid_state
               ~message:"session requires restoration and explicit execution admission"
               ~retryable:false
               ())
        else Ok ()
      in
      let%bind loader =
        Eio.Mutex.use_ro t.mutex (fun () ->
          Result.of_option t.loader ~error:(shutting_down ()))
      in
      let%bind entry = loader indexed in
      close_provisional t ~session_id entry (fun () ->
        let%bind () = check_retained_owner entry in
        let%bind state = Agent_session.Session_actor.state entry.actor in
        let%bind () = check_retained_owner entry in
        let%bind () =
          if Agent_protocol.Id.Session.equal state.identity.session_id session_id
          then Ok ()
          else
            Error
              (Agent_protocol.Error.create
                 Persistence_error
                 ~message:"loaded actor identity differs from reserved session"
                 ~retryable:false
                 ())
        in
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          let%bind () = check_reservation t reservation in
          if Atomic.get t.closing
          then Error (shutting_down ())
          else if
            Option.is_some (find t session_id)
            || not
                 (Option.equal
                    Agent_store.Session_index.Entry.equal
                    (Map.find t.indexed session_id)
                    (Some indexed))
          then Error (lifecycle_conflict "loaded session binding changed")
          else (
            let%bind () = check_retained_owner entry in
            Atomic.set
              t.sessions
              (Map.set (Atomic.get t.sessions) ~key:session_id ~data:entry);
            t.indexed <- Map.remove t.indexed session_id;
            Ok entry))))
;;

module Read_binding = struct
  type t =
    | Loaded of entry
    | Indexed of
        { entry : Agent_store.Session_index.Entry.t
        ; read :
            Agent_store.Session_index.Entry.t
            -> (Agent_session.Session_state.t, Agent_protocol.Error.t) Result.t
        }
end

let capture_read_binding t session_id =
  match find t session_id with
  | Some entry -> Ok (Read_binding.Loaded entry)
  | None ->
    (match Map.find t.indexed session_id, t.reader with
     | None, _ ->
       Error
         (Agent_protocol.Error.create
            Session_not_found
            ~message:"session does not exist"
            ~retryable:false
            ())
     | Some _, None -> Error (shutting_down ())
     | Some entry, Some read -> Ok (Read_binding.Indexed { entry; read }))
;;

let read_binding_is_current t session_id = function
  | Read_binding.Loaded original ->
    Option.exists (find t session_id) ~f:(same_owner original)
  | Indexed { entry; read = _ } ->
    Option.is_none (find t session_id)
    && Option.equal
         Agent_store.Session_index.Entry.equal
         (Map.find t.indexed session_id)
         (Some entry)
;;

let read_retained t ~authorize ~read_loaded ~read_stored session_id =
  let open Result.Let_syntax in
  let check (state, observation) =
    let summary = Agent_session.Session_state.summary state in
    if not (Agent_protocol.Id.Session.equal summary.id session_id)
    then
      Error
        (Agent_protocol.Error.create
           Persistence_error
           ~message:"read session identity does not match the index"
           ~retryable:false
           ())
    else (
      let%map () = authorize summary in
      state, observation)
  in
  with_read_snapshot
    t
    ~capture:(fun () -> capture_read_binding t session_id)
    ~check_current:(fun binding ->
      if not (read_binding_is_current t session_id binding)
      then Error (lifecycle_conflict "session observation binding changed")
      else (
        match binding with
        | Read_binding.Loaded entry -> check_retained_owner entry
        | Indexed _ -> Ok ()))
    (function
      | Read_binding.Loaded entry ->
        let%bind () = check_retained_owner entry in
        let%bind observation = read_loaded entry.actor in
        let%bind () = check_retained_owner entry in
        check observation
      | Indexed { entry; read } ->
        let%bind () = authorize entry.session in
        let%bind state = read entry in
        check (state, read_stored state))
;;

let read_state t ~authorize session_id =
  read_retained
    t
    ~authorize
    session_id
    ~read_loaded:(fun actor ->
      Agent_session.Session_actor.state actor |> Result.map ~f:(fun state -> state, ()))
    ~read_stored:(fun _ -> ())
  |> Result.map ~f:fst
;;

let read_observation t ~authorize ~now session_id =
  let open Result.Let_syntax in
  let%map state, snapshot =
    read_retained
      t
      ~authorize
      session_id
      ~read_loaded:(fun actor ->
        Agent_session.Session_actor.observe actor
        |> Result.map ~f:(fun (state, snapshot) -> state, Some snapshot))
      ~read_stored:(fun _ -> None)
  in
  let snapshot, transient =
    match snapshot with
    | Some snapshot ->
      ( snapshot
      , Agent_protocol.Session_activity.Transient.Live
          { tool_calls = List.length snapshot.active_tool_calls
          ; agent_calls = List.length snapshot.active_agent_calls
          } )
    | None -> Agent_session.Session_state.snapshot ~now state, Unavailable
  in
  Activity_service.Observation.
    { snapshot
    ; transient
    ; usage = Agent_session.Inference_ledger.summary state.inference_ledger
    }
;;

let entries t = Eio.Mutex.use_ro t.mutex (fun () -> Map.data (Atomic.get t.sessions))

let stats t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    { loaded = Map.length (Atomic.get t.sessions); indexed = Map.length t.indexed })
;;

let load_all t =
  let session_ids =
    Eio.Mutex.use_ro t.mutex (fun () ->
      (Map.to_alist t.indexed
       |> List.filter_map ~f:(fun (id, entry) ->
         Option.some_if
           ((not entry.archived)
            && Agent_store.Session_archive_record.Admission.equal
                 entry.admission
                 Automatic)
           id))
      @ Map.keys (Atomic.get t.sessions)
      |> List.dedup_and_sort ~compare:Agent_protocol.Id.Session.compare)
  in
  Result.all (List.map session_ids ~f:(load t))
;;

let remove t session_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if Map.mem t.reservations session_id || cleanup_pending t session_id
    then None
    else (
      let entry = Map.find (Atomic.get t.sessions) session_id in
      Atomic.set t.sessions (Map.remove (Atomic.get t.sessions) session_id);
      t.indexed <- Map.remove t.indexed session_id;
      entry))
;;

let catalog t ~now ~indexed_entries =
  with_read_snapshot
    t
    ~capture:(fun () -> Ok (Atomic.get t.sessions, t.indexed))
    ~check_current:(fun (loaded, indexed) ->
      if
        not
          (Map.equal same_owner loaded (Atomic.get t.sessions)
           && Map.equal Agent_store.Session_index.Entry.equal indexed t.indexed)
      then Error (lifecycle_conflict "session catalog binding changed")
      else
        Map.fold loaded ~init:(Ok ()) ~f:(fun ~key:_ ~data checked ->
          Result.bind checked ~f:(fun () -> check_retained_owner data)))
    (fun (loaded_entries, _) ->
       let open Result.Let_syntax in
       let%map loaded =
         Map.fold loaded_entries ~init:(Ok []) ~f:(fun ~key ~data entries ->
           let%bind entries = entries in
           let%bind () = check_retained_owner data in
           let%bind state = Agent_session.Session_actor.state data.actor in
           let%bind () = check_retained_owner data in
           let%map indexed =
             Result.of_option
               (List.find indexed_entries ~f:(fun indexed ->
                  Agent_protocol.Id.Session.equal
                    indexed.Agent_store.Session_index.Entry.session.id
                    key))
               ~error:
                 (Agent_protocol.Error.create
                    Persistence_error
                    ~message:"loaded session lacks checked catalog projection"
                    ~retryable:true
                    ())
           in
           ( key
           , Agent_protocol.Session_catalog.
               { effective_organization = Agent_protocol.Session_organization.Values.empty
               ; session = Agent_session.Session_state.summary state
               ; active_owner_principal_id =
                   Session_catalog_policy.active_owner ~now state.attachments
               ; archived = indexed.archived
               ; lifecycle_revision = indexed.lifecycle_revision
               ; admission = indexed.admission
               } )
           :: entries)
       in
       let indexed =
         indexed_entries
         |> List.filter_map ~f:(fun (data : Agent_store.Session_index.Entry.t) ->
           if Map.mem loaded_entries data.session.id
           then None
           else
             Some
               Agent_protocol.Session_catalog.
                 { effective_organization =
                     Agent_protocol.Session_organization.Values.empty
                 ; session = data.session
                 ; active_owner_principal_id = None
                 ; archived = data.archived
                 ; lifecycle_revision = data.lifecycle_revision
                 ; admission = data.admission
                 })
       in
       List.map loaded ~f:snd @ indexed)
;;

let summaries t =
  let snapshot =
    Eio.Mutex.use_ro t.mutex (fun () ->
      Map.keys (Atomic.get t.sessions), Map.data t.indexed)
  in
  let loaded, indexed = snapshot in
  let summaries =
    List.filter_map loaded ~f:(fun session_id ->
      read_state t session_id ~authorize:(fun _ -> Ok ())
      |> Result.ok
      |> Option.map ~f:Agent_session.Session_state.summary)
    |> List.map ~f:(fun session -> session.Agent_protocol.Session.id, session)
    |> Map.of_alist_exn (module Session_key)
  in
  List.fold indexed ~init:summaries ~f:(fun summaries data ->
    if data.Agent_store.Session_index.Entry.archived || Map.mem summaries data.session.id
    then summaries
    else Map.set summaries ~key:data.session.id ~data:data.session)
  |> Map.data
;;

let job_requires_actor job =
  match job.Agent_protocol.Job.delivery, job.status with
  | Pending, _ | _, (Queued | Running | Waiting_permission _ | Waiting_completion _) ->
    true
  | ( (Not_required | Delivered _ | Discarded _)
    , (Succeeded | Failed _ | Cancelled | Interrupted _) ) -> false
;;

let schedule_requires_actor schedule =
  match schedule.Agent_protocol.Schedule.status with
  | Scheduled | Delivering -> true
  | Delivered | Cancelled | Failed _ -> false
;;

let inactive state =
  Agent_protocol.Session.equal_desired_state
    state.Agent_session.Session_state.lifecycle.desired
    Stopped
  && (match state.lifecycle.observed with
      | Stopped -> true
      | Queued_for_slot
      | Starting
      | Recovering
      | Idle
      | Running_turn _
      | Compacting _
      | Waiting_for_permission _
      | Stopping
      | Failed _ -> false)
  && Option.is_none state.active_operation
  && List.is_empty state.attachments
  && (not (List.exists state.jobs ~f:job_requires_actor))
  && not (List.exists state.schedules ~f:schedule_requires_actor)
;;

let unload_inactive t ~index_entries =
  let indexes =
    List.fold
      index_entries
      ~init:(Map.empty (module Session_key))
      ~f:(fun indexes entry ->
        Map.set indexes ~key:entry.Agent_store.Session_index.Entry.session.id ~data:entry)
  in
  let candidates =
    Eio.Mutex.use_ro t.mutex (fun () -> Map.keys (Atomic.get t.sessions))
  in
  let unload session_id =
    let open Result.Let_syntax in
    with_lifecycle t session_id (fun reservation ->
      match reservation.Lifecycle_reservation.target, Map.find indexes session_id with
      | (Indexed _ | Absent), _ | Loaded _, None -> Ok false
      | Loaded entry, Some indexed ->
        let%bind () = check_retained_owner entry in
        let%bind state = Agent_session.Session_actor.state entry.actor in
        let matches state =
          inactive state
          && Jsonaf.exactly_equal
               (Agent_protocol.Session.to_json
                  (Agent_session.Session_state.summary state))
               (Agent_protocol.Session.to_json indexed.session)
        in
        if not (matches state)
        then Ok false
        else (
          let%bind fence =
            Agent_session.Session_actor.begin_lifecycle
              entry.actor
              ~attachment_id:None
              ~expected_generation:state.identity.generation
              ~expected_revision:state.counters.revision
          in
          let retired = ref false in
          Exn.protect
            ~f:(fun () ->
              let%bind current =
                Agent_session.Session_actor.lifecycle_state entry.actor fence
              in
              let%bind () = check_retained_owner entry in
              if not (matches current)
              then Ok false
              else
                Eio.Cancel.protect (fun () ->
                  if not (Runtime_owner.reserve_inactive_close entry.runtime)
                  then Ok false
                  else (
                    retired := true;
                    let%bind () =
                      Agent_session.Session_actor.retire_lifecycle entry.actor fence
                    in
                    (* Keep failed cleanup discoverable; detach only after its owner closed. *)
                    entry.close ();
                    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
                      let%bind () = check_reservation t reservation in
                      match find t session_id with
                      | Some current when same_owner current entry ->
                        Atomic.set
                          t.sessions
                          (Map.remove (Atomic.get t.sessions) session_id);
                        t.indexed <- Map.set t.indexed ~key:session_id ~data:indexed;
                        Ok true
                      | Some _ | None ->
                        Error (lifecycle_conflict "eviction owner changed")))))
            ~finally:(fun () ->
              if not !retired
              then
                Eio.Cancel.protect (fun () ->
                  ignore
                    (Agent_session.Session_actor.abort_lifecycle entry.actor fence
                     : (unit, Agent_protocol.Error.t) Result.t)))))
  in
  List.fold candidates ~init:0 ~f:(fun count session_id ->
    match unload session_id with
    | Ok true -> count + 1
    | Ok false | Error _ -> count)
;;

let shutdown t =
  Eio.Cancel.protect (fun () ->
    (* Only shutdown uses this coordinator. Registry admissions and callbacks
       never acquire it; concurrent retries cannot close the same owner twice. *)
    Eio.Mutex.use_ro t.shutdown_mutex (fun () ->
      let reservations, readers =
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          Atomic.set t.closing true;
          ( Map.data t.reservations
            |> List.map ~f:(fun reservation -> reservation.Lifecycle_reservation.finished)
          , List.map t.read_leases ~f:(fun lease -> lease.Read_lease.finished) ))
      in
      List.iter reservations ~f:Eio.Promise.await;
      List.iter readers ~f:Eio.Promise.await;
      let failed_cleanups = Eio.Mutex.use_ro t.mutex (fun () -> t.failed_cleanups) in
      List.iter failed_cleanups ~f:(fun cleanup ->
        match cleanup_owner cleanup.Failed_cleanup.owner with
        | Ok () ->
          Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
            (match cleanup.owner with
             | Cleanup_owner.Entry entry ->
               Atomic.set
                 t.sessions
                 (Map.filter (Atomic.get t.sessions) ~f:(fun current ->
                    not (same_owner current entry)))
             | Handle _ | Fence _ -> ());
            t.failed_cleanups
            <- List.filter t.failed_cleanups ~f:(fun current ->
                 not (phys_equal current cleanup)))
        | Error _ | (exception _) ->
          (* Preserve the original diagnostic and exact owner for another retry. *)
          (match Cleanup_failure.exception_and_backtrace cleanup.failure with
           | Some (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
           | None -> raise (Cleanup_failed cleanup.failure)));
      let entries =
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          let entries = Map.data (Atomic.get t.sessions) in
          t.indexed <- Map.empty (module Session_key);
          t.loader <- None;
          t.reader <- None;
          entries)
      in
      (* Dependency cleanup can still need another actor's durable stop acknowledgement.
     Keep the complete loaded graph and all actors alive until every runtime has
     joined cleanup. No registry mutex is held while entering a runtime owner. *)
      List.map entries ~f:(fun entry () -> Runtime_owner.close_and_wait entry.runtime)
      |> Eio.Fiber.all;
      List.map entries ~f:(fun entry () ->
        entry.close ();
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          Atomic.set
            t.sessions
            (Map.filter (Atomic.get t.sessions) ~f:(fun current ->
               not (same_owner current entry)))))
      |> Eio.Fiber.all))
;;

let install_lifecycle_projection t reservation indexed =
  let open Result.Let_syntax in
  let%map () = check_reservation t reservation in
  let session_id = reservation.Lifecycle_reservation.session_id in
  let detached = find t session_id in
  Atomic.set t.sessions (Map.remove (Atomic.get t.sessions) session_id);
  t.indexed
  <- (match indexed with
      | None -> Map.remove t.indexed session_id
      | Some entry -> Map.set t.indexed ~key:session_id ~data:entry);
  detached
;;

let commit_lifecycle t reservation ~handle installed =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let module Installed = Agent_store.Session_store.Lifecycle.Installed in
    let indexed = Installed.entry installed in
    let same_handle =
      match reservation.Lifecycle_reservation.target with
      | Loaded entry ->
        Option.exists entry.store_handle ~f:(fun current -> phys_equal current handle)
      | Indexed _ -> true
      | Absent -> false
    in
    if
      (not same_handle)
      || (not (Agent_protocol.Id.Session.equal indexed.session.id reservation.session_id))
      || not (Installed.is_current installed handle)
    then Error (lifecycle_conflict "session lifecycle installation is not current")
    else install_lifecycle_projection t reservation (Some indexed))
;;

let commit_removal t reservation removal =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let module Removal = Agent_store.Session_store.Lifecycle.Removal in
    let same_handle =
      match reservation.Lifecycle_reservation.target with
      | Loaded entry ->
        (match entry.store_handle, Removal.handle removal with
         | Some current, Some removed -> phys_equal current removed
         | None, _ | _, None -> false)
      | Indexed _ | Absent -> true
    in
    let same_id =
      match Removal.outcome removal with
      | None ->
        (match reservation.target with
         | Absent -> true
         | Loaded _ | Indexed _ -> false)
      | Some outcome ->
        Agent_protocol.Id.Session.equal outcome.session_id reservation.session_id
    in
    if same_handle && same_id
    then install_lifecycle_projection t reservation None
    else Error (lifecycle_conflict "session removal does not belong to reserved owner"))
;;

let commit_retired t reservation ~store =
  let open Result.Let_syntax in
  let%bind indexed =
    Agent_store.Session_index.find_checked
      (Agent_store.Session_store.session_index store)
      reservation.Lifecycle_reservation.session_id
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    install_lifecycle_projection t reservation indexed)
;;

let lifecycle_reserved t session_id =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Map.mem t.reservations session_id || cleanup_pending t session_id)
;;

let same_cleanup_owner (first : Cleanup_owner.t) (second : Cleanup_owner.t) =
  match first, second with
  | Cleanup_owner.Entry first, Entry second -> same_owner first second
  | Handle (_, first), Handle (_, second) -> phys_equal first second
  | Fence (first, first_fence), Fence (second, second_fence) ->
    same_owner first second && phys_equal first_fence second_fence
  | Entry _, (Handle _ | Fence _)
  | Handle _, (Entry _ | Fence _)
  | Fence _, (Entry _ | Handle _) -> false
;;

let cleanup_belongs_to_reservation reservation (owner : Cleanup_owner.t) =
  let same_handle handle =
    Agent_protocol.Id.Session.equal
      (Agent_store.Session_store.Handle.session_id handle)
      reservation.Lifecycle_reservation.session_id
  in
  match owner, reservation.Lifecycle_reservation.target with
  | Cleanup_owner.Entry actual, Loaded expected | Fence (actual, _), Loaded expected ->
    same_owner actual expected
  | Handle (store, handle), Loaded expected ->
    Agent_store.Session_store.owns_handle store handle
    && same_handle handle
    && Option.exists expected.store_handle ~f:(phys_equal handle)
  | Handle (store, handle), (Indexed _ | Absent) ->
    Agent_store.Session_store.owns_handle store handle && same_handle handle
  | (Entry _ | Fence _), (Indexed _ | Absent) -> false
;;

let retain_cleanup t reservation ~owner ~primary ~failure =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    (* The issuing reservation is an ownership capability, not a value key. *)
    match Map.find t.reservations reservation.Lifecycle_reservation.session_id with
    | Some current
      when phys_equal current reservation
           && phys_equal reservation.owner t.lifecycle_owner
           && cleanup_belongs_to_reservation reservation owner ->
      if
        not
          (List.exists t.failed_cleanups ~f:(fun cleanup ->
             same_cleanup_owner cleanup.Failed_cleanup.owner owner))
      then (
        let cleanup : Failed_cleanup.t =
          { owner; session_id = reservation.session_id; primary; failure }
        in
        t.failed_cleanups <- cleanup :: t.failed_cleanups)
    | Some _ | None -> failwith "cleanup capability differs from its issuing reservation")
;;
