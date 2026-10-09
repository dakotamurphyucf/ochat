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

type t =
  { mutex : Eio.Mutex.t
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
  ; closing = Atomic.make false
  ; sessions = Atomic.make (Map.empty (module Session_key))
  ; indexed = Map.empty (module Session_key)
  ; reader = None
  ; loader = None
  }
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
    if Atomic.get t.closing || Map.mem (Atomic.get t.sessions) session_id
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

let load t session_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match Atomic.get t.closing, Map.find (Atomic.get t.sessions) session_id with
    | true, _ -> Error (shutting_down ())
    | false, Some entry -> Ok entry
    | false, None ->
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
       | Some indexed, Some loader when not indexed.archived ->
         Result.map (loader indexed) ~f:(fun entry ->
           Atomic.set
             t.sessions
             (Map.set (Atomic.get t.sessions) ~key:session_id ~data:entry);
           t.indexed <- Map.remove t.indexed session_id;
           entry)
       | Some _, Some _ ->
         Error
           (Agent_protocol.Error.create
              Invalid_state
              ~message:"archived session requires restoration before execution"
              ~retryable:false
              ())))
;;

let read_retained t ~authorize ~read_loaded ~read_stored session_id =
  Eio.Mutex.use_ro t.mutex (fun () ->
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
    match Atomic.get t.closing, find t session_id with
    | true, _ -> Error (shutting_down ())
    | false, Some entry ->
      let%bind observation = read_loaded entry.actor in
      check observation
    | false, None ->
      (match Map.find t.indexed session_id, t.reader with
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
              ~message:"session reader is unavailable"
              ~retryable:true
              ())
       | Some indexed, Some reader ->
         let%bind () = authorize indexed.Agent_store.Session_index.Entry.session in
         let%bind state = reader indexed in
         check (state, read_stored state)))
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
       |> List.filter_map ~f:(fun (id, entry) -> Option.some_if (not entry.archived) id))
      @ Map.keys (Atomic.get t.sessions)
      |> List.dedup_and_sort ~compare:Agent_protocol.Id.Session.compare)
  in
  Result.all (List.map session_ids ~f:(load t))
;;

let remove t session_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let entry = Map.find (Atomic.get t.sessions) session_id in
    Atomic.set t.sessions (Map.remove (Atomic.get t.sessions) session_id);
    t.indexed <- Map.remove t.indexed session_id;
    entry)
;;

let catalog t ~now ~indexed_entries =
  Eio.Mutex.use_ro t.mutex (fun () ->
    let open Result.Let_syntax in
    let loaded_entries = Atomic.get t.sessions in
    let%map loaded =
      Map.fold loaded_entries ~init:(Ok []) ~f:(fun ~key ~data entries ->
        let%bind entries = entries in
        let%map state = Agent_session.Session_actor.state data.actor in
        ( key
        , Agent_protocol.Session_catalog.
            { effective_organization = Agent_protocol.Session_organization.Values.empty
            ; session = Agent_session.Session_state.summary state
            ; active_owner_principal_id =
                Session_catalog_policy.active_owner ~now state.attachments
            ; archived = false
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
              { effective_organization = Agent_protocol.Session_organization.Values.empty
              ; session = data.session
              ; active_owner_principal_id = None
              ; archived = data.archived
              })
    in
    List.map loaded ~f:snd @ indexed)
;;

let summaries t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    let loaded =
      Map.filter_map (Atomic.get t.sessions) ~f:(fun entry ->
        Agent_session.Session_actor.state entry.actor
        |> Result.ok
        |> Option.map ~f:Agent_session.Session_state.summary)
    in
    Map.fold t.indexed ~init:loaded ~f:(fun ~key ~data summaries ->
      if data.archived || Map.mem summaries key
      then summaries
      else Map.set summaries ~key ~data:data.Agent_store.Session_index.Entry.session)
    |> Map.data)
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
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Map.fold (Atomic.get t.sessions) ~init:0 ~f:(fun ~key:session_id ~data:entry count ->
      match
        Agent_session.Session_actor.state entry.actor, Map.find indexes session_id
      with
      | Ok state, Some indexed
        when inactive state && Runtime_owner.reserve_inactive_close entry.runtime ->
        entry.close ();
        Atomic.set t.sessions (Map.remove (Atomic.get t.sessions) session_id);
        t.indexed <- Map.set t.indexed ~key:session_id ~data:indexed;
        count + 1
      | _ -> count))
;;

let shutdown t =
  Eio.Cancel.protect (fun () ->
    let entries =
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        let entries = Map.data (Atomic.get t.sessions) in
        Atomic.set t.closing true;
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
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      Atomic.set t.sessions (Map.empty (module Session_key)));
    List.map entries ~f:(fun entry () -> entry.close ()) |> Eio.Fiber.all)
;;
