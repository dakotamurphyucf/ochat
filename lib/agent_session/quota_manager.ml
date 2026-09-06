open Core

type limits =
  { global_running_sessions : int
  ; per_principal_running_sessions : int
  ; runtime_construction : int
  }
[@@deriving compare, equal, sexp]

type overflow =
  | Reject
  | Queue
[@@deriving compare, equal, sexp]

type request =
  { session_id : Agent_protocol.Id.Session.t
  ; principal_id : Agent_protocol.Id.Principal.t
  ; quota_key : Quota_key.t
  ; prompt_limit : int
  ; overflow : overflow
  ; workspace_lease_mode : Workspace_lease.mode option
  }

type acquisition =
  { request : request
  ; workspace_lease : Workspace_lease.lease option
  ; mutable construction_held : bool
  ; mutable released : bool
  }

type blocking_scope =
  | Global
  | Workspace of string
  | Prompt of Quota_key.t
  | Principal of Agent_protocol.Id.Principal.t
  | Runtime_construction
[@@deriving sexp]

type acquire_result =
  | Acquired of acquisition
  | Queue_required of blocking_scope
  | Rejected of Agent_protocol.Error.t

type t =
  { limits : limits
  ; workspace_leases : Workspace_lease.t
  ; mutex : Eio.Mutex.t
  ; mutable global_running : int
  ; mutable construction : int
  ; mutable principals : (Agent_protocol.Id.Principal.t, int) Map.Poly.t
  ; mutable prompts : (Quota_key.t, int) Map.Poly.t
  }

let limit_error message =
  Agent_protocol.Error.create Resource_limit ~message ~retryable:true ()
;;

let create ~limits ~workspace_leases =
  if
    limits.global_running_sessions <= 0
    || limits.per_principal_running_sessions <= 0
    || limits.runtime_construction <= 0
  then Error (limit_error "quota limits must be positive")
  else
    Ok
      { limits
      ; workspace_leases
      ; mutex = Eio.Mutex.create ()
      ; global_running = 0
      ; construction = 0
      ; principals = Map.Poly.empty
      ; prompts = Map.Poly.empty
      }
;;

let count map key = Map.find map key |> Option.value ~default:0
let increment map key = Map.set map ~key ~data:(count map key + 1)

let decrement map key =
  match count map key with
  | 0 | 1 -> Map.remove map key
  | value -> Map.set map ~key ~data:(value - 1)
;;

let queue_or_reject request scope message =
  match request.overflow with
  | Queue -> Queue_required scope
  | Reject -> Rejected (limit_error message)
;;

let acquire_workspace t request =
  match request.workspace_lease_mode with
  | None -> Ok None
  | Some mode ->
    Workspace_lease.acquire
      t.workspace_leases
      ~conflict_domain:request.quota_key.conflict_domain
      ~session_id:request.session_id
      ~mode
    |> Result.map ~f:Option.some
;;

let release_workspace t = function
  | None -> ()
  | Some lease -> Workspace_lease.release t.workspace_leases lease
;;

let capacities_available t request =
  if t.global_running >= t.limits.global_running_sessions
  then Error (Global, "global running-session capacity is exhausted")
  else if count t.prompts request.quota_key >= request.prompt_limit
  then
    Error (Prompt request.quota_key, "prompt/workspace root-agent capacity is exhausted")
  else if
    count t.principals request.principal_id >= t.limits.per_principal_running_sessions
  then
    Error
      (Principal request.principal_id, "principal running-session capacity is exhausted")
  else if t.construction >= t.limits.runtime_construction
  then Error (Runtime_construction, "runtime construction capacity is exhausted")
  else Ok ()
;;

let install t request workspace_lease =
  t.global_running <- t.global_running + 1;
  t.prompts <- increment t.prompts request.quota_key;
  t.principals <- increment t.principals request.principal_id;
  t.construction <- t.construction + 1;
  { request; workspace_lease; construction_held = true; released = false }
;;

let try_acquire t request =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if request.prompt_limit <= 0
    then Rejected (limit_error "prompt limit must be positive")
    else (
      match capacities_available t request with
      | Error (scope, message) -> queue_or_reject request scope message
      | Ok () ->
        (match acquire_workspace t request with
         | Error error ->
           (match request.overflow with
            | Queue -> Queue_required (Workspace request.quota_key.conflict_domain)
            | Reject -> Rejected error)
         | Ok workspace_lease -> Acquired (install t request workspace_lease))))
;;

let runtime_ready t acquisition =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if acquisition.construction_held && not acquisition.released
    then (
      acquisition.construction_held <- false;
      t.construction <- t.construction - 1))
;;

let release t acquisition =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if not acquisition.released
    then (
      if acquisition.construction_held then t.construction <- t.construction - 1;
      t.principals <- decrement t.principals acquisition.request.principal_id;
      t.prompts <- decrement t.prompts acquisition.request.quota_key;
      release_workspace t acquisition.workspace_lease;
      t.global_running <- t.global_running - 1;
      acquisition.construction_held <- false;
      acquisition.released <- true))
;;

let running_sessions t = Eio.Mutex.use_ro t.mutex (fun () -> t.global_running)
