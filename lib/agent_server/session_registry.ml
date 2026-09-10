open! Core

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

type t =
  { mutex : Eio.Mutex.t
  ; mutable sessions : (Agent_protocol.Id.Session.t, entry) Map.Poly.t
  ; mutable indexed :
      (Agent_protocol.Id.Session.t, Agent_store.Session_index.Entry.t) Map.Poly.t
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
  ; sessions = Map.Poly.empty
  ; indexed = Map.Poly.empty
  ; loader = None
  }
;;

let install_loader t loader =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.loader <- Some loader)
;;

let index t entry =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let session_id = entry.Agent_store.Session_index.Entry.session.id in
    if Map.mem t.sessions session_id
    then ()
    else t.indexed <- Map.set t.indexed ~key:session_id ~data:entry)
;;

let index_all t entries = List.iter entries ~f:(index t)

let add t ~session_id entry =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if Map.mem t.sessions session_id
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"session is already registered"
           ~retryable:false
           ())
    else (
      t.sessions <- Map.set t.sessions ~key:session_id ~data:entry;
      t.indexed <- Map.remove t.indexed session_id;
      Ok ()))
;;

let find t session_id =
  Eio.Mutex.use_ro t.mutex (fun () -> Map.find t.sessions session_id)
;;

let load t session_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match Map.find t.sessions session_id with
    | Some entry -> Ok entry
    | None ->
      (match Map.find t.indexed session_id, t.loader with
       | None, _ ->
         Error
           (Agent_protocol.Error.create
              Session_not_found
              ~message:"session does not exist"
              ~retryable:false
              ())
       | Some _, None ->
         Error
           (Agent_protocol.Error.create
              Server_shutting_down
              ~message:"session loader is unavailable"
              ~retryable:true
              ())
       | Some indexed, Some loader ->
         Result.map (loader indexed) ~f:(fun entry ->
           t.sessions <- Map.set t.sessions ~key:session_id ~data:entry;
           t.indexed <- Map.remove t.indexed session_id;
           entry)))
;;

let entries t = Eio.Mutex.use_ro t.mutex (fun () -> Map.data t.sessions)

let stats t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    { loaded = Map.length t.sessions; indexed = Map.length t.indexed })
;;

let load_all t =
  let session_ids =
    Eio.Mutex.use_ro t.mutex (fun () ->
      Map.keys t.indexed @ Map.keys t.sessions
      |> List.dedup_and_sort ~compare:Poly.compare)
  in
  Result.all (List.map session_ids ~f:(load t))
;;

let remove t session_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let entry = Map.find t.sessions session_id in
    t.sessions <- Map.remove t.sessions session_id;
    t.indexed <- Map.remove t.indexed session_id;
    entry)
;;

let summaries t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    let loaded =
      Map.filter_map t.sessions ~f:(fun entry ->
        Agent_session.Session_actor.state entry.actor
        |> Result.ok
        |> Option.map ~f:Agent_session.Session_state.summary)
    in
    Map.fold t.indexed ~init:loaded ~f:(fun ~key ~data summaries ->
      Map.set summaries ~key ~data:data.Agent_store.Session_index.Entry.session)
    |> Map.data)
;;

let job_requires_actor job =
  match job.Agent_protocol.Job.delivery, job.status with
  | Pending, _ | _, (Queued | Running | Waiting_permission _ | Waiting_completion _) ->
    true
  | (Not_required | Delivered _), (Succeeded | Failed _ | Cancelled | Interrupted _) ->
    false
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
    List.fold index_entries ~init:Map.Poly.empty ~f:(fun indexes entry ->
      Map.set indexes ~key:entry.Agent_store.Session_index.Entry.session.id ~data:entry)
  in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Map.fold t.sessions ~init:0 ~f:(fun ~key:session_id ~data:entry count ->
      match
        Agent_session.Session_actor.state entry.actor, Map.find indexes session_id
      with
      | Ok state, Some indexed when inactive state ->
        entry.close ();
        t.sessions <- Map.remove t.sessions session_id;
        t.indexed <- Map.set t.indexed ~key:session_id ~data:indexed;
        count + 1
      | _ -> count))
;;

let shutdown t =
  let entries =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let entries = Map.data t.sessions in
      t.sessions <- Map.Poly.empty;
      t.indexed <- Map.Poly.empty;
      t.loader <- None;
      entries)
  in
  List.map entries ~f:(fun entry () -> entry.close ()) |> Eio.Fiber.all
;;
