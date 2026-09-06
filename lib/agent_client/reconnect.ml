open! Core

type status =
  | Connected
  | Reconnecting of { attempt : int }
  | Disconnected
  | Failed of Agent_protocol.Error.t
[@@deriving sexp]

type policy =
  { initial_delay : Time_ns.Span.t
  ; maximum_delay : Time_ns.Span.t
  ; multiplier : float
  ; jitter_ratio : float
  ; maximum_attempts : int option
  }

type packed_clock = Clock : _ Eio.Time.clock -> packed_clock

type t =
  { sw : Eio.Switch.t
  ; clock : packed_clock
  ; sleep : float -> unit
  ; reconnect : (unit -> (Connection.t, Agent_protocol.Error.t) result) option
  ; session_id : Agent_protocol.Id.Session.t
  ; mode : Agent_protocol.Session.attachment_mode
  ; policy : policy
  ; mutex : Eio.Mutex.t
  ; closed_signal : unit Eio.Promise.t
  ; closed_resolver : unit Eio.Promise.u
  ; on_update : (Projection.t -> unit) option
  ; on_status : (status -> unit) option
  ; on_error : (Agent_protocol.Error.t -> unit) option
  ; mutable connection : Connection.t
  ; mutable handle : Session_handle.t option
  ; mutable reclaim_token : string option
  ; mutable projection : Projection.t
  ; mutable status : status
  ; mutable closed : bool
  }

let default_policy =
  { initial_delay = Time_ns.Span.of_ms 100.
  ; maximum_delay = Time_ns.Span.of_sec 5.
  ; multiplier = 2.
  ; jitter_ratio = 0.2
  ; maximum_attempts = None
  }
;;

let interrupted message =
  Agent_protocol.Error.create Interrupted ~message ~retryable:true ()
;;

let callback callback value =
  Option.iter callback ~f:(fun f ->
    try f value with
    | _ -> ())
;;

let set_status t status =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.status <- status);
  callback t.on_status status
;;

let install_projection t projection =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.projection <- projection);
  callback t.on_update projection
;;

let report_error t failure = callback t.on_error failure

let initialize connection =
  Session_handle.initialize
    connection
    ~implementation_name:"ochat-agent-client"
    ~implementation_version:"dev"
  |> Result.map ~f:(fun _ -> ())
;;

let current_handle t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    if t.closed
    then Error (interrupted "session client is closed")
    else
      Result.of_option t.handle ~error:(interrupted "session connection is unavailable"))
;;

let deterministic_jitter attempt ratio =
  let sample = Float.of_int (((attempt * 1103515245) + 12345) land 1023) /. 1023. in
  1. +. (((sample *. 2.) -. 1.) *. ratio)
;;

let retry_delay policy attempt =
  let exponent = Float.of_int (Int.max 0 (attempt - 1)) in
  let base =
    Time_ns.Span.to_sec policy.initial_delay *. (policy.multiplier ** exponent)
  in
  let maximum = Time_ns.Span.to_sec policy.maximum_delay in
  Float.min maximum base *. deterministic_jitter attempt policy.jitter_ratio
;;

let attempts_exhausted policy attempt =
  Option.exists policy.maximum_attempts ~f:(fun maximum -> attempt > maximum)
;;

let wait_or_closed t seconds =
  Eio.Fiber.first
    (fun () ->
       t.sleep seconds;
       false)
    (fun () ->
       Eio.Promise.await t.closed_signal;
       true)
;;

let attach_callbacks t =
  ( (fun projection -> install_projection t projection)
  , fun failure -> report_error t failure )
;;

let reconnect_once t connection =
  let open Result.Let_syntax in
  let%bind () = initialize connection in
  let projection = Eio.Mutex.use_ro t.mutex (fun () -> t.projection) in
  let after_sequence = (Projection.snapshot projection).latest_event_sequence in
  let on_update, on_error = attach_callbacks t in
  let reclaim_token = Eio.Mutex.use_ro t.mutex (fun () -> t.reclaim_token) in
  match t.clock with
  | Clock clock ->
    Session_handle.attach
      ~sw:t.sw
      ~clock
      ~connection
      ~session_id:t.session_id
      ~mode:t.mode
      ~after_sequence
      ?reclaim_token
      ~previous_projection:projection
      ~on_update
      ~on_error
      ()
;;

let clear_current_handle t handle =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    Option.iter t.handle ~f:(fun current ->
      if phys_equal current handle then t.handle <- None))
;;

let install_handle t connection handle =
  let projection = Session_handle.projection handle in
  let reclaim_token = Session_handle.reclaim_token handle in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.connection <- connection;
    t.handle <- Some handle;
    Option.iter reclaim_token ~f:(fun token -> t.reclaim_token <- Some token);
    t.projection <- projection);
  set_status t Connected;
  callback t.on_update projection
;;

let connect t =
  match t.reconnect with
  | None -> Error (interrupted "this client has no reconnect transport")
  | Some reconnect ->
    Result.try_with reconnect
    |> Result.map_error ~f:(fun exn ->
      interrupted ("reconnect failed: " ^ Exn.to_string exn))
    |> Result.join
;;

let rec reconnect_until t attempt =
  if attempts_exhausted t.policy attempt
  then Error (interrupted "maximum reconnect attempts exhausted")
  else (
    set_status t (Reconnecting { attempt });
    if wait_or_closed t (retry_delay t.policy attempt)
    then Error (interrupted "session client closed during reconnect")
    else (
      match connect t with
      | Error failure when failure.retryable ->
        report_error t failure;
        reconnect_until t (attempt + 1)
      | Error _ as failure -> failure
      | Ok connection ->
        (match reconnect_once t connection with
         | Ok handle -> Ok (connection, handle)
         | Error failure ->
           Connection.close connection;
           if failure.retryable
           then (
             report_error t failure;
             reconnect_until t (attempt + 1))
           else Error failure)))
;;

let rec monitor t handle =
  Eio.Fiber.first
    (fun () ->
       Session_handle.await_closed handle;
       false)
    (fun () ->
       Eio.Promise.await t.closed_signal;
       true)
  |> function
  | true -> ()
  | false ->
    clear_current_handle t handle;
    Connection.close t.connection;
    set_status t Disconnected;
    (match reconnect_until t 1 with
     | Ok (connection, handle) ->
       if Eio.Mutex.use_ro t.mutex (fun () -> t.closed)
       then (
         Session_handle.close handle;
         Connection.close connection)
       else (
         install_handle t connection handle;
         monitor t handle)
     | Error failure ->
       if not (Eio.Mutex.use_ro t.mutex (fun () -> t.closed))
       then (
         set_status t (Failed failure);
         report_error t failure))
;;

let validate_policy policy =
  if Time_ns.Span.(policy.initial_delay < zero)
  then Error (interrupted "reconnect initial delay cannot be negative")
  else if Time_ns.Span.(policy.maximum_delay < policy.initial_delay)
  then Error (interrupted "reconnect maximum delay is below its initial delay")
  else if Float.(policy.multiplier < 1.)
  then Error (interrupted "reconnect multiplier must be at least one")
  else if Float.(policy.jitter_ratio < 0. || policy.jitter_ratio > 1.)
  then Error (interrupted "reconnect jitter ratio must be between zero and one")
  else if Option.exists policy.maximum_attempts ~f:(fun attempts -> attempts < 1)
  then Error (interrupted "maximum reconnect attempts must be positive")
  else Ok ()
;;

let make
      ~sw
      ~clock
      ~connection
      ~reconnect
      ~session_id
      ~mode
      ~policy
      ~on_update
      ~on_status
      ~on_error
      build_handle
  =
  let open Result.Let_syntax in
  let%bind () = validate_policy policy in
  let%bind () = initialize connection in
  let owner = ref None in
  let handle_update projection =
    Option.iter !owner ~f:(fun t -> install_projection t projection)
  in
  let handle_error failure = Option.iter !owner ~f:(fun t -> report_error t failure) in
  let%bind handle = build_handle ~on_update:handle_update ~on_error:handle_error in
  let closed_signal, closed_resolver = Eio.Promise.create () in
  let projection = Session_handle.projection handle in
  let t =
    { sw
    ; clock = Clock clock
    ; sleep = Eio.Time.sleep clock
    ; reconnect
    ; session_id
    ; mode
    ; policy
    ; mutex = Eio.Mutex.create ()
    ; closed_signal
    ; closed_resolver
    ; on_update
    ; on_status
    ; on_error
    ; connection
    ; handle = Some handle
    ; reclaim_token = Session_handle.reclaim_token handle
    ; projection
    ; status = Connected
    ; closed = false
    }
  in
  owner := Some t;
  callback on_status Connected;
  callback on_update projection;
  Eio.Fiber.fork ~sw (fun () -> monitor t handle);
  Ok t
;;

let attach
      ~sw
      ~clock
      ~connection
      ~reconnect
      ~session_id
      ~mode
      ?(policy = default_policy)
      ?on_update
      ?on_status
      ?on_error
      ()
  =
  make
    ~sw
    ~clock
    ~connection
    ~reconnect
    ~session_id
    ~mode
    ~policy
    ~on_update
    ~on_status
    ~on_error
    (fun ~on_update ~on_error ->
       Session_handle.attach
         ~sw
         ~clock
         ~connection
         ~session_id
         ~mode
         ~on_update
         ~on_error
         ())
;;

let create
      ~sw
      ~clock
      ~connection
      ~reconnect
      ~spec
      ~mode
      ?(policy = default_policy)
      ?on_update
      ?on_status
      ?on_error
      ()
  =
  let session_id = ref None in
  let open Result.Let_syntax in
  let%bind () = validate_policy policy in
  let%bind () = initialize connection in
  let owner = ref None in
  let handle_update projection =
    Option.iter !owner ~f:(fun t -> install_projection t projection)
  in
  let handle_error failure = Option.iter !owner ~f:(fun t -> report_error t failure) in
  let%bind handle =
    Session_handle.create
      ~sw
      ~clock
      ~connection
      ~spec
      ~mode
      ~on_update:handle_update
      ~on_error:handle_error
      ()
  in
  session_id := Some (Session_handle.session_id handle);
  let session_id = Option.value_exn !session_id in
  let closed_signal, closed_resolver = Eio.Promise.create () in
  let projection = Session_handle.projection handle in
  let t =
    { sw
    ; clock = Clock clock
    ; sleep = Eio.Time.sleep clock
    ; reconnect
    ; session_id
    ; mode
    ; policy
    ; mutex = Eio.Mutex.create ()
    ; closed_signal
    ; closed_resolver
    ; on_update
    ; on_status
    ; on_error
    ; connection
    ; handle = Some handle
    ; reclaim_token = Session_handle.reclaim_token handle
    ; projection
    ; status = Connected
    ; closed = false
    }
  in
  owner := Some t;
  callback on_status Connected;
  callback on_update projection;
  Eio.Fiber.fork ~sw (fun () -> monitor t handle);
  Ok t
;;

let session_id t = t.session_id
let attachment t = Result.ok (current_handle t) |> Option.map ~f:Session_handle.attachment
let reclaim_token t = Eio.Mutex.use_ro t.mutex (fun () -> t.reclaim_token)
let projection t = Eio.Mutex.use_ro t.mutex (fun () -> t.projection)
let status t = Eio.Mutex.use_ro t.mutex (fun () -> t.status)
let with_handle t f = Result.bind (current_handle t) ~f

let start t ~queue_if_limited =
  with_handle t (fun handle -> Session_handle.start handle ~queue_if_limited)
;;

let stop t ~mode = with_handle t (fun handle -> Session_handle.stop handle ~mode)

let send_message t content =
  with_handle t (fun handle -> Session_handle.send_message handle content)
;;

let compact t ~expected_revision =
  with_handle t (fun handle -> Session_handle.compact handle ~expected_revision)
;;

let delete_history t ~expected_revision history_id =
  with_handle t (fun handle ->
    Session_handle.delete_history handle ~expected_revision history_id)
;;

let cancel_operation t operation_id =
  with_handle t (fun handle -> Session_handle.cancel_operation handle operation_id)
;;

let respond_permission t ~permission_id ~permission_generation ~choice ~reason =
  with_handle t (fun handle ->
    Session_handle.respond_permission
      handle
      ~permission_id
      ~permission_generation
      ~choice
      ~reason)
;;

let revoke_grant t ~grant_id ~reason =
  with_handle t (fun handle -> Session_handle.revoke_grant handle ~grant_id ~reason)
;;

let read_audit t ~limit =
  with_handle t (fun handle -> Session_handle.read_audit handle ~limit)
;;

let mark_closed t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if not t.closed
    then (
      t.closed <- true;
      Eio.Promise.resolve t.closed_resolver ()))
;;

let detach t =
  let result = with_handle t Session_handle.detach in
  mark_closed t;
  Connection.close t.connection;
  set_status t Disconnected;
  result
;;

let close t = ignore (detach t : (unit, Agent_protocol.Error.t) result)
