open! Core
module P = Agent_protocol

type pending_command =
  { command : P.Command.t
  ; byte_count : int
  ; identity : P.Initialize.Response.t option
  }

let has_command_receipt = function
  | P.Command.Session_create _
  | Session_attach _
  | Session_detach _
  | Session_renew_owner _
  | Session_start _
  | Session_update_metadata _
  | Session_stop _
  | Session_cancel_operation _
  | Session_configuration_update _
  | Session_send_message _
  | Session_compact _
  | Session_delete_history _
  | Session_reset _
  | Session_rebuild _
  | Session_upgrade_prompt _
  | Session_delete _
  | Permission_respond _
  | Grant_revoke _
  | Job_cancel _
  | Schedule_create _
  | Provider_setup _
  | Provider_login_begin _
  | Provider_login_cancel _
  | Provider_logout _
  | Provider_select _
  | Provider_configure_environment _
  | Schedule_cancel _ -> true
  | Protocol_initialize _
  | Command_receipt _
  | Protocol_ping _
  | Server_info
  | Server_health _
  | Prompt_list _
  | Prompt_get _
  | Workspace_list _
  | Workspace_get _
  | Blob_read _
  | Session_list _
  | Session_get _
  | Session_configuration_get _
  | Session_inference_summary _
  | Session_inference_observations _
  | Session_export _
  | Permission_list _
  | Grant_list _
  | Audit_read _
  | Job_list _
  | Job_get _
  | Schedule_list _
  | Schedule_get _
  | Provider_status _
  | Provider_login_challenge _
  | Ingress_submit _ -> false
;;

let admission_binding command =
  let params =
    match P.Command.params command with
    | `Object fields ->
      `Object
        (List.filter fields ~f:(fun (name, _) ->
           not
             (List.mem
                [ "idempotency_key"; "attachment_id"; "reclaim_token"; "after_sequence" ]
                name
                ~equal:String.equal)))
    | params -> params
  in
  P.Json_codec.canonical_string params
;;

let same_admission first second =
  String.equal (P.Command.method_name first) (P.Command.method_name second)
  &&
  match admission_binding first, admission_binding second with
  | Ok first, Ok second -> String.equal first second
  | Error _, _ | _, Error _ -> true
;;

type t =
  { transport : Transport.t
  ; mutex : Eio.Mutex.t
  ; notification_mutex : Eio.Mutex.t
  ; closed : bool Atomic.t
  ; mutable initialization : P.Initialize.Response.t option
  ; mutable notification_owner : int option
  ; mutable next_notification_owner : int
  ; mutable pending : pending_command list
  }

let create transport =
  { transport
  ; mutex = Eio.Mutex.create ()
  ; notification_mutex = Eio.Mutex.create ()
  ; closed = Atomic.make false
  ; initialization = None
  ; notification_owner = None
  ; next_notification_owner = 0
  ; pending = []
  }
;;

let closed_error () =
  Agent_protocol.Error.create
    Interrupted
    ~message:"client connection is closed"
    ~retryable:true
    ()
;;

let admit_initialization (request : P.Initialize.Request.t) response =
  let open Result.Let_syntax in
  let%bind response =
    P.Initialize.Response.of_json (P.Initialize.Response.to_json response)
  in
  if
    P.Version.compare response.selected_version request.protocol_min < 0
    || P.Version.compare response.selected_version request.protocol_max > 0
    || not
         (List.for_all response.enabled_features ~f:(fun feature ->
            List.mem request.features feature ~equal:String.equal))
  then
    Error (P.Error.invalid_request "initialize response exceeds requested capabilities")
  else Ok response
;;

let maximum_pending_commands = 64
let maximum_pending_bytes = 16 * 1024 * 1024

let unknown_outcome = function
  | P.Error.Interrupted | Persistence_error | Internal_error | Journal_corrupt -> true
  | Invalid_request
  | Method_not_found
  | Unauthenticated
  | Permission_denied
  | Session_not_found
  | Prompt_not_found
  | Workspace_not_found
  | Invalid_state
  | Already_resolved
  | Resource_limit
  | Workspace_unavailable
  | Prompt_unavailable
  | Manifest_unauthorized
  | Approval_required
  | Idempotency_conflict
  | Snapshot_required
  | Operation_not_found
  | Conflict
  | Incompatible_protocol
  | Cursor_expired
  | Blob_unavailable
  | Lease_stale
  | Configuration_invalid
  | Store_locked
  | Store_schema_too_new
  | Migration_required
  | Server_shutting_down
  | Command_queue_full -> false
;;

let pending_capacity pending byte_count =
  List.length pending < maximum_pending_commands
  && List.sum (module Int) pending ~f:(fun item -> item.byte_count)
     <= maximum_pending_bytes - byte_count
;;

let with_request_lock t f =
  (* Cancellation is an expected request outcome. Keep it outside use_rw's
     exception boundary so Eio does not poison the mutex and make retained
     uncertain intents inaccessible to receipt reconciliation. *)
  match
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
      match f () with
      | result -> Ok result
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let request t command =
  with_request_lock t (fun () ->
    let open Result.Let_syntax in
    if Atomic.get t.closed
    then Error (closed_error ())
    else if
      has_command_receipt command
      && List.exists t.pending ~f:(fun pending -> same_admission pending.command command)
    then
      Error
        (P.Error.create
           Interrupted
           ~message:
             "an equivalent earlier command remains unresolved; reconcile its receipt \
              before another admission"
           ~retryable:false
           ())
    else (
      let%bind intent =
        if not (has_command_receipt command)
        then Ok None
        else (
          let params = P.Command.params command in
          let%bind () = P.Command_receipt.Request.validate_original_params params in
          let%bind encoded = P.Json_codec.canonical_string params in
          let byte_count = String.length encoded in
          if not (pending_capacity t.pending byte_count)
          then
            Error
              (P.Error.create
                 Resource_limit
                 ~message:
                   "unresolved command retention budget exhausted; reconcile before \
                    submitting"
                 ~retryable:false
                 ())
          else Ok (Some { command; byte_count; identity = t.initialization }))
      in
      Option.iter intent ~f:(fun item -> t.pending <- item :: t.pending);
      let retire () =
        Option.iter intent ~f:(fun item ->
          t.pending
          <- List.filter t.pending ~f:(fun pending -> not (phys_equal pending item)))
      in
      let result =
        match Transport.request t.transport command with
        | Error _ as result -> result
        | Ok value ->
          if
            not
              (String.equal
                 (P.Command.method_name command)
                 (P.Public.Result.method_name value))
          then
            Error
              (P.Error.create
                 Interrupted
                 ~message:"successful response method does not match the admitted command"
                 ~retryable:false
                 ())
          else
            P.Public.Result.validate value
            |> Result.map_error ~f:(fun failure ->
              P.Error.create
                Interrupted
                ~message:
                  ("successful response failed validation: " ^ failure.P.Error.message)
                ~retryable:false
                ())
            |> Result.map ~f:(fun () -> value)
      in
      (match result with
       | Ok _ -> retire ()
       | Error failure -> if not (unknown_outcome failure.P.Error.code) then retire ());
      match command, result with
      | P.Command.Protocol_initialize request, Ok (P.Public.Result.Non_history value) ->
        (match P.Public.Result.Non_history.value value with
         | Protocol_initialize response ->
           let%bind () = P.Public.Result.validate (P.Public.Result.Non_history value) in
           let%bind response = admit_initialization request response in
           let%bind () =
             match t.initialization with
             | None -> Ok ()
             | Some previous
               when P.Id.Server.equal previous.server_id response.server_id
                    && P.Id.Principal.equal previous.principal.id response.principal.id ->
               Ok ()
             | Some _ ->
               Error
                 (P.Error.create
                    Permission_denied
                    ~message:"transport initialization changed host or principal identity"
                    ~retryable:false
                    ())
           in
           t.initialization <- Some response;
           result
         | _ -> result)
      | _, _ -> result))
;;

let initialization t =
  if Atomic.get t.closed
  then None
  else
    Eio.Mutex.use_ro t.mutex (fun () ->
      if Atomic.get t.closed then None else t.initialization)
;;

type notification_lease =
  { connection : t
  ; owner : int
  ; released : unit Eio.Promise.t
  ; release_resolver : unit Eio.Promise.u
  ; mutable reading : bool
  ; mutable release_requested : bool
  }

let claim_notifications t =
  Eio.Mutex.use_rw ~protect:true t.notification_mutex (fun () ->
    if Atomic.get t.closed
    then Error (closed_error ())
    else (
      match t.notification_owner with
      | Some _ ->
        Error
          (P.Error.create
             Invalid_state
             ~message:"connection already has a notification consumer"
             ~retryable:false
             ())
      | None ->
        let owner = t.next_notification_owner in
        t.next_notification_owner <- owner + 1;
        t.notification_owner <- Some owner;
        let released, release_resolver = Eio.Promise.create () in
        Ok
          { connection = t
          ; owner
          ; released
          ; release_resolver
          ; reading = false
          ; release_requested = false
          }))
;;

let release_notifications lease =
  let t = lease.connection in
  Eio.Mutex.use_rw ~protect:true t.notification_mutex (fun () ->
    lease.release_requested <- true;
    ignore (Eio.Promise.try_resolve lease.release_resolver ());
    match t.notification_owner with
    | Some owner when Int.equal owner lease.owner && not lease.reading ->
      t.notification_owner <- None
    | Some _ | None -> ())
;;

let next_owned_notification lease =
  let t = lease.connection in
  let admission =
    Eio.Mutex.use_rw ~protect:true t.notification_mutex (fun () ->
      if lease.reading
      then
        Error
          (P.Error.create
             Invalid_state
             ~message:"notification lease already has an active reader"
             ~retryable:false
             ())
      else if
        Atomic.get t.closed
        || lease.release_requested
        || not (Option.equal Int.equal t.notification_owner (Some lease.owner))
      then Ok false
      else (
        lease.reading <- true;
        Ok true))
  in
  Result.bind admission ~f:(fun admitted ->
    if not admitted
    then Ok None
    else
      Exn.protect
        ~finally:(fun () ->
          Eio.Mutex.use_rw ~protect:true t.notification_mutex (fun () ->
            lease.reading <- false;
            if
              lease.release_requested
              && Option.equal Int.equal t.notification_owner (Some lease.owner)
            then t.notification_owner <- None))
        ~f:(fun () ->
          Eio.Fiber.first
            (fun () ->
               Eio.Promise.await lease.released;
               Ok None)
            (fun () -> Ok (Transport.next_notification t.transport))))
;;

let next_notification t =
  match claim_notifications t with
  | Error _ -> None
  | Ok lease ->
    Exn.protect
      ~f:(fun () -> Result.ok (next_owned_notification lease) |> Option.join)
      ~finally:(fun () -> release_notifications lease)
;;

let close t =
  if Atomic.compare_and_set t.closed false true then Transport.close t.transport
;;

let request_without_history t command =
  match request t command with
  | Ok (Agent_protocol.Public.Result.Non_history value) ->
    Ok (Agent_protocol.Public.Result.Non_history.value value)
  | Ok (Private_provider_challenge _) ->
    Error
      (Agent_protocol.Error.invalid_request
         "private provider challenge requires an explicit private consumer")
  | Ok (Session_get _ | Session_attach _ | Session_create _) ->
    Error (Agent_protocol.Error.invalid_request "unexpected history-bearing result")
  | Error _ as failure -> failure
;;

let pending_commands t = Eio.Mutex.use_ro t.mutex (fun () -> t.pending)

let adopt_pending t pending =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let same_identity item =
      match item.identity, t.initialization with
      | Some original, Some current ->
        P.Id.Server.equal original.server_id current.server_id
        && P.Id.Principal.equal original.principal.id current.principal.id
      | None, _ | _, None -> false
    in
    if Atomic.get t.closed
    then Error (closed_error ())
    else if not (List.for_all pending ~f:same_identity)
    then
      Error
        (P.Error.create
           Permission_denied
           ~message:"pending commands belong to another host or principal"
           ~retryable:false
           ())
    else (
      let combined =
        List.fold pending ~init:t.pending ~f:(fun existing item ->
          if List.exists existing ~f:(fun retained -> phys_equal retained item)
          then existing
          else item :: existing)
      in
      if
        List.length combined > maximum_pending_commands
        || List.sum (module Int) combined ~f:(fun item -> item.byte_count)
           > maximum_pending_bytes
      then
        Error
          (P.Error.create
             Resource_limit
             ~message:"unresolved command adoption exceeds retention budget"
             ~retryable:false
             ())
      else (
        t.pending <- combined;
        Ok ())))
;;

let reconcile t pending =
  let open Result.Let_syntax in
  let%bind () =
    match pending.identity, initialization t with
    | Some original, Some current
      when P.Id.Server.equal original.server_id current.server_id
           && P.Id.Principal.equal original.principal.id current.principal.id -> Ok ()
    | Some _, Some _ | None, _ | _, None ->
      Error
        (P.Error.create
           Permission_denied
           ~message:"receipt reconciliation requires the original host and principal"
           ~retryable:false
           ())
  in
  let query =
    P.Command_receipt.Request.
      { method_name = P.Command.method_name pending.command
      ; original_params = P.Command.params pending.command
      }
  in
  let%bind result = request_without_history t (Command_receipt query) in
  match result with
  | Command_receipt receipt ->
    (match receipt with
     | Committed _ | Failed _ ->
       Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
         t.pending <- List.filter t.pending ~f:(fun item -> not (phys_equal item pending)))
     | Missing | Unavailable | Pending _ -> ());
    Ok receipt
  | _ -> Error (P.Error.invalid_request "unexpected command receipt response")
;;

let abandon_pending t pending =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.pending <- List.filter t.pending ~f:(fun item -> not (phys_equal item pending)))
;;
