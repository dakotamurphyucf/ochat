open! Core
module A = Shell_access.Approval

type ui_request =
  { id : string
  ; request : A.request
  ; runtime_id : string
  ; manifest_sha256 : string
  ; scopes : Chatmd_shell_spec.Shell_spec.approval_scope list
  }

type ui_response =
  | Approve_once
  | Approve_exact_session
  | Approve_prefix_session of string list
  | Approve_durable_exact
  | Deny of string

type error =
  | Closed
  | Capacity_exhausted
  | Admission_denied of string
  | Deadline_unavailable
  | Unknown_request of string
  | Duplicate_request of string
[@@deriving sexp, compare, equal]

type pending =
  { request : ui_request
  ; resolver : A.response Eio.Promise.u
  ; deadline : Mtime.t option
  }

module Limits = struct
  type t =
    { max_pending : int
    ; wait_seconds : float
    }

  let create ~max_pending ~wait_seconds =
    if max_pending <= 0
    then Error "approval pending capacity must be positive"
    else if not (Float.is_finite wait_seconds && Float.(wait_seconds > 0.))
    then Error "approval wait must be finite and positive"
    else if Float.(wait_seconds > 3600.)
    then Error "approval wait exceeds the one-hour bound"
    else Ok { max_pending; wait_seconds }
  ;;
end

type bounded =
  { clock : Eio.Time.Mono.ty Eio.Time.Mono.t
  ; limits : Limits.t
  ; admit : ui_request -> (unit, string) result
  ; on_finished : ui_request -> unit
  }

type t =
  { mutex : Eio.Mutex.t
  ; pending : pending String.Table.t
  ; mutable order : string list
  ; mutable closed : bool
  ; mutable next_id : int64
  ; on_pending : ui_request -> unit
  ; bounded : bounded option
  }

type provider =
  | None_available
  | Auto_deny
  | Callback of t
  | Assume_approved

let with_lock t f = Eio.Mutex.use_rw ~protect:true t.mutex f

let create ?(on_pending = ignore) () =
  { mutex = Eio.Mutex.create ()
  ; pending = String.Table.create ()
  ; order = []
  ; closed = false
  ; next_id = 0L
  ; on_pending
  ; bounded = None
  }
;;

let create_bounded ~clock ~limits ~admit ~on_pending ~on_finished () =
  { mutex = Eio.Mutex.create ()
  ; pending = String.Table.create ()
  ; order = []
  ; closed = false
  ; next_id = 0L
  ; on_pending
  ; bounded =
      Some { clock :> Eio.Time.Mono.ty Eio.Time.Mono.t; limits; admit; on_finished }
  }
;;

let remove t id =
  Hashtbl.remove t.pending id;
  t.order <- List.filter t.order ~f:(fun candidate -> not (String.equal candidate id))
;;

let add_pending t request resolver =
  let deadline =
    match t.bounded with
    | None -> Ok None
    | Some bounded ->
      let now = Eio.Time.Mono.now bounded.clock in
      let span =
        Mtime.Span.of_uint64_ns (Int64.of_float (bounded.limits.wait_seconds *. 1e9))
      in
      (match Mtime.add_span now span with
       | None -> Error Deadline_unavailable
       | Some deadline -> Ok (Some deadline))
  in
  let admitted =
    match t.bounded with
    | None -> Ok ()
    | Some bounded ->
      Result.map_error (bounded.admit request) ~f:(fun reason -> Admission_denied reason)
  in
  Result.bind deadline ~f:(fun deadline ->
    Result.bind admitted ~f:(fun () ->
      with_lock t (fun () ->
        if t.closed
        then Error Closed
        else if Hashtbl.mem t.pending request.id
        then Error (Duplicate_request request.id)
        else if
          Option.exists t.bounded ~f:(fun bounded ->
            Hashtbl.length t.pending >= bounded.limits.max_pending)
        then Error Capacity_exhausted
        else (
          let pending = { request; resolver; deadline } in
          Hashtbl.set t.pending ~key:request.id ~data:pending;
          t.order <- t.order @ [ request.id ];
          Ok pending))))
;;

let finished t request =
  Option.iter t.bounded ~f:(fun bounded -> bounded.on_finished request)
;;

let remove_pending_if_same t pending =
  with_lock t (fun () ->
    match Hashtbl.find t.pending pending.request.id with
    | Some current when phys_equal current pending ->
      remove t pending.request.id;
      true
    | None | Some _ -> false)
;;

let await_pending t pending promise =
  Fun.protect
    ~finally:(fun () ->
      if remove_pending_if_same t pending then finished t pending.request)
    (fun () ->
       t.on_pending pending.request;
       match t.bounded with
       | None -> Eio.Promise.await promise
       | Some bounded ->
         (try
            match pending.deadline with
            | Some deadline ->
              Eio.Fiber.first
                (fun () ->
                   Eio.Time.Mono.sleep_until bounded.clock deadline;
                   raise Eio.Time.Timeout)
                (fun () -> Eio.Promise.await promise)
            | None -> assert false
          with
          | Eio.Time.Timeout -> A.Deny "approval request timed out"))
;;

let request t request =
  let promise, resolver = Eio.Promise.create () in
  match add_pending t request resolver with
  | Error Closed -> A.Deny "approval broker is closed"
  | Error (Duplicate_request id) -> A.Deny ("duplicate approval request: " ^ id)
  | Error Capacity_exhausted -> A.Deny "approval pending capacity was reached"
  | Error (Admission_denied reason) -> A.Deny reason
  | Error Deadline_unavailable -> A.Deny "approval deadline is unavailable"
  | Error (Unknown_request _) -> assert false
  | Ok pending -> await_pending t pending promise
;;

let pending t =
  with_lock t (fun () ->
    List.find_map t.order ~f:(fun id ->
      Option.map (Hashtbl.find t.pending id) ~f:(fun pending -> pending.request)))
;;

let pending_all t =
  with_lock t (fun () ->
    List.filter_map t.order ~f:(fun id ->
      Option.map (Hashtbl.find t.pending id) ~f:(fun pending -> pending.request)))
;;

let pending_count t = with_lock t (fun () -> Hashtbl.length t.pending)

let has_scope scopes scope =
  List.mem scopes scope ~equal:Chatmd_shell_spec.Shell_spec.equal_approval_scope
;;

let approval_response scopes = function
  | Approve_once when has_scope scopes Once -> A.Approve
  | Approve_exact_session when has_scope scopes Exact_session ->
    A.Approve_for (Exact_session { expires_at = None })
  | Approve_prefix_session prefix when has_scope scopes Prefix_session ->
    A.Approve_for (Prefix_session { prefix; expires_at = None })
  | Approve_durable_exact when has_scope scopes Durable_exact ->
    A.Approve_for (Durable_exact { expires_at = None })
  | Approve_once
  | Approve_exact_session
  | Approve_prefix_session _
  | Approve_durable_exact ->
    A.Deny "the requested approval scope is not enabled for this runtime"
  | Deny message -> A.Deny message
;;

let respond t ~id response =
  let pending =
    with_lock t (fun () ->
      match Hashtbl.find t.pending id with
      | None -> Error (if t.closed then Closed else Unknown_request id)
      | Some pending ->
        remove t id;
        Ok pending)
  in
  Result.map pending ~f:(fun pending ->
    Eio.Promise.resolve
      pending.resolver
      (approval_response pending.request.scopes response);
    finished t pending.request)
;;

let cancel t ~id =
  ignore (respond t ~id (Deny "approval request was cancelled") : (unit, error) result)
;;

let close t =
  let pending =
    with_lock t (fun () ->
      if t.closed
      then []
      else (
        t.closed <- true;
        let pending = Hashtbl.data t.pending in
        Hashtbl.clear t.pending;
        t.order <- [];
        pending))
  in
  List.iter pending ~f:(fun pending ->
    Eio.Promise.resolve pending.resolver (A.Deny "approval broker was closed");
    finished t pending.request)
;;

let next_id t =
  with_lock t (fun () ->
    let id = t.next_id in
    t.next_id <- Int64.succ id;
    sprintf "shell-approval-%Ld" id)
;;

let callback_reviewer t ~runtime_id ~manifest_sha256 ~scopes approval_request =
  let id = next_id t in
  let promise, resolver = Eio.Promise.create () in
  let ui_request =
    { id; request = approval_request; runtime_id; manifest_sha256; scopes }
  in
  match add_pending t ui_request resolver with
  | Error Closed -> A.Deny "approval broker is closed"
  | Error (Duplicate_request duplicate) ->
    A.Deny ("duplicate approval request: " ^ duplicate)
  | Error Capacity_exhausted -> A.Deny "approval pending capacity was reached"
  | Error (Admission_denied reason) -> A.Deny reason
  | Error Deadline_unavailable -> A.Deny "approval deadline is unavailable"
  | Error (Unknown_request _) -> assert false
  | Ok pending ->
    (match await_pending t pending promise with
     | A.Approve -> approval_response scopes Approve_once
     | Approve_for (Exact_session _) -> approval_response scopes Approve_exact_session
     | Approve_for (Prefix_session { prefix; _ }) ->
       approval_response scopes (Approve_prefix_session prefix)
     | Approve_for (Durable_exact _) -> approval_response scopes Approve_durable_exact
     | Approve_for Once -> approval_response scopes Approve_once
     | response -> response)
;;

let reviewer provider ~runtime_id ~manifest_sha256 ~scopes =
  match provider with
  | None_available -> None
  | Auto_deny -> Some (fun _ -> A.Deny "shell approval was denied automatically")
  | Assume_approved -> Some (fun _ -> A.Approve)
  | Callback broker ->
    Some (callback_reviewer broker ~runtime_id ~manifest_sha256 ~scopes)
;;
