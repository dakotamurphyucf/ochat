open! Core

type acquire_result =
  | Acquired
  | Already_acquired
  | Queue_required of Agent_session.Quota_manager.blocking_scope
  | Rejected of Agent_protocol.Error.t

type t =
  { manager : Agent_session.Quota_manager.t
  ; mutable request : Agent_session.Quota_manager.request
  ; configured_overflow : Agent_session.Quota_manager.overflow
  ; mutex : Eio.Mutex.t
  ; mutable acquisition : Agent_session.Quota_manager.acquisition option
  }

let create
      ~manager
      ~session_id
      ~principal_id
      ~quota_key
      ~prompt_limit
      ~configured_overflow
      ~workspace_lease_mode
  =
  let request =
    Agent_session.Quota_manager.
      { session_id
      ; principal_id
      ; quota_key
      ; prompt_limit
      ; overflow = configured_overflow
      ; workspace_lease_mode
      }
  in
  { manager
  ; request
  ; configured_overflow
  ; mutex = Eio.Mutex.create ()
  ; acquisition = None
  }
;;

let overflow t queue_if_limited =
  match t.configured_overflow, queue_if_limited with
  | Queue, true -> Agent_session.Quota_manager.Queue
  | (Queue | Reject), false | Reject, true -> Reject
;;

let acquire t queue_if_limited =
  let request = { t.request with overflow = overflow t queue_if_limited } in
  match Agent_session.Quota_manager.try_acquire t.manager request with
  | Acquired acquisition ->
    t.acquisition <- Some acquisition;
    Acquired
  | Queue_required scope -> Queue_required scope
  | Rejected error -> Rejected error
;;

let try_acquire t ~queue_if_limited =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match t.acquisition with
    | Some _ -> Already_acquired
    | None -> acquire t queue_if_limited)
;;

let runtime_ready t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Option.iter t.acquisition ~f:(Agent_session.Quota_manager.runtime_ready t.manager))
;;

let release t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Option.iter t.acquisition ~f:(Agent_session.Quota_manager.release t.manager);
    t.acquisition <- None)
;;

let is_acquired t = Eio.Mutex.use_ro t.mutex (fun () -> Option.is_some t.acquisition)

let blocked_by t scope =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match scope with
    | Agent_session.Quota_manager.Global | Runtime_construction -> true
    | Workspace conflict_domain ->
      String.equal t.request.quota_key.conflict_domain conflict_domain
    | Prompt quota_key ->
      Agent_session.Quota_key.compare t.request.quota_key quota_key = 0
    | Principal principal_id ->
      Agent_protocol.Id.Principal.compare t.request.principal_id principal_id = 0)
;;

let replace_workspace t ~conflict_domain ~workspace_lease_mode =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if Option.is_some t.acquisition
    then Error "cannot replace an acquired workspace capacity"
    else (
      let quota_key = { t.request.quota_key with conflict_domain } in
      t.request <- { t.request with quota_key; workspace_lease_mode };
      Ok ()))
;;
