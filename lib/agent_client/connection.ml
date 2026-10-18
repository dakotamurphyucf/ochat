open! Core
module P = Agent_protocol

module Pending_identity = struct
  type t =
    { method_name : string
    ; idempotency_key : P.Idempotency_key.t option
    }

  let of_original command params =
    let idempotency_key =
      match command with
      | P.Command.Session_run_start request -> Some request.key
      | _ ->
        (match params with
         | `Object fields ->
           (match List.Assoc.find fields "idempotency_key" ~equal:String.equal with
            | Some (`String key) -> P.Idempotency_key.of_string key |> Result.ok
            | Some _ | None -> None)
         | _ -> None)
    in
    { method_name = P.Command.method_name command; idempotency_key }
  ;;
end

type pending_command =
  { command : P.Command.t
  ; byte_count : int
  ; identity : P.Initialize.Response.t option
  ; recovery_identity : Pending_identity.t
  }

let has_command_receipt = function
  | P.Command.Prompt_dependencies_update _ -> true
  | P.Command.Session_seed _
  | Session_create _
  | Session_attach _
  | Session_detach _
  | Session_renew_owner _
  | Session_start _
  | Project_create _
  | Project_update _
  | Project_delete _
  | Collection_create _
  | Collection_update _
  | Collection_delete _
  | Session_update_organization _
  | Session_update_metadata _
  | Session_stop _
  | Session_cancel_operation _
  | Session_configuration_update _
  | Session_run_start _
  | Session_send_message _
  | Session_cancel_pending_input _
  | Session_replace_pending_input _
  | Session_compact _
  | Session_edit_history _
  | Session_continue_history _
  | Session_delete_history _
  | Session_reset _
  | Session_rebuild _
  | Session_upgrade_prompt _
  | Session_delete _
  | Session_restore _
  | Session_resume _
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
  | Prompt_dependencies_status
  | Workspace_list _
  | Workspace_get _
  | Blob_read _
  | Project_get _
  | Project_list _
  | Collection_get _
  | Collection_list _
  | Activity_list _
  | Session_work _
  | Session_search _
  | Session_search_navigate _
  | Session_runs _
  | Session_run _
  | Session_pending_inputs _
  | Session_pending_input _
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
  | Ingress_submit _
  | Authoring _ -> false
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
  ; mutable developer_capabilities :
      (P.Initialize.Response.t * P.Developer_common.Capabilities.t) option
  ; mutable repl_capabilities :
      (P.Initialize.Response.t * P.Developer_repl_wire.Capabilities.t) option
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
  ; developer_capabilities = None
  ; repl_capabilities = None
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
  | Organization_not_found
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
  | Pending_input_conflict
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
          else
            Ok
              (Some
                 { command
                 ; byte_count
                 ; identity = t.initialization
                 ; recovery_identity = Pending_identity.of_original command params
                 }))
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
          let dependency_affinity =
            match command, value with
            | ( P.Command.Prompt_dependencies_update request
              , P.Public.Result.Non_history public ) ->
              (match P.Public.Result.Non_history.value public with
               | P.Method_result.Prompt_dependencies_update result ->
                 P.Prompt_dependency_selection.Update_result.matches_request
                   result
                   request
               | _ -> false)
            | _, _ -> true
          in
          if not dependency_affinity
          then
            Error
              (P.Error.create
                 Interrupted
                 ~message:"dependency update response does not match its original request"
                 ~retryable:false
                 ())
          else if
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
           t.developer_capabilities <- None;
           t.repl_capabilities <- None;
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

let developer_error message =
  P.Error.create Incompatible_protocol ~message ~retryable:false ()
;;

let request_developer_transport t ~bound request =
  match Transport.request_developer t.transport ~bound request with
  | Error failure when not (Transport.Developer_response_bound.error_within_bound failure)
    -> Error (developer_error "Developer error response exceeds its bound")
  | result -> result
;;

let developer_initialization t =
  match t.initialization with
  | Some initialization when not (Atomic.get t.closed) -> Ok initialization
  | Some _ | None -> Error (developer_error "Developer requires initialization")
;;

let require_developer_feature initialization feature =
  if
    List.mem
      initialization.P.Initialize.Response.enabled_features
      feature
      ~equal:String.equal
  then Ok ()
  else Error (developer_error "Developer feature was not selected")
;;

let developer_capabilities t =
  with_request_lock t (fun () ->
    let open Result.Let_syntax in
    let%bind initialization = developer_initialization t in
    let%bind () =
      require_developer_feature initialization P.Developer_common.Features.run
    in
    match t.developer_capabilities with
    | Some (source, capabilities) when phys_equal source initialization -> Ok capabilities
    | None | Some _ ->
      let%bind json =
        request_developer_transport
          t
          ~bound:Transport.Developer_response_bound.Bootstrap
          P.Developer_wire.Request.Capabilities
      in
      let%bind capabilities = P.Developer_common.Capabilities.of_json json in
      if
        Atomic.get t.closed
        || not
             (Option.value_map
                t.initialization
                ~default:false
                ~f:(phys_equal initialization))
      then Error (developer_error "Developer initialization changed")
      else (
        t.developer_capabilities <- Some (initialization, capabilities);
        Ok capabilities))
;;

let developer_feature = function
  | P.Developer_wire.Request.Capabilities
  | Submit _
  | Observe _
  | Await_terminal _
  | Cancel_and_wait _ -> P.Developer_common.Features.run
  | Approval_list _ | Approval_respond _ | Approval_cancel _ ->
    P.Developer_common.Features.approvals
  | Watch _ | Unwatch _ -> P.Developer_common.Features.events
;;

let developer_view_matches initialization request response =
  let module D = P.Developer_wire in
  let module C = P.Developer_common in
  let same view ~run_id ~key =
    let owner = C.View.owner view in
    P.Id.Server.equal owner.host_id initialization.P.Initialize.Response.server_id
    && P.Id.Principal.equal
         owner.principal_id
         initialization.P.Initialize.Response.principal.id
    && P.Id.Developer_run.equal owner.run_id run_id
    && P.Idempotency_key.equal (C.View.key view) key
  in
  match request, response with
  | D.Request.Submit submit, D.Response.Submit view ->
    same view ~run_id:(C.Submit.run_id submit) ~key:(C.Submit.key submit)
    && P.Developer_receipt.Kind.equal (C.View.kind view) (C.Submit.kind submit)
  | D.Request.Observe lookup, D.Response.Observe (Some view)
  | D.Request.Await_terminal lookup, D.Response.Await_terminal view
  | D.Request.Cancel_and_wait lookup, D.Response.Cancel_and_wait view ->
    same view ~run_id:lookup.run_id ~key:lookup.key
  | D.Request.Observe _, D.Response.Observe None -> true
  | _ -> true
;;

let request_developer t ~limits request =
  with_request_lock t (fun () ->
    let module D = P.Developer_wire in
    let open Result.Let_syntax in
    let%bind initialization = developer_initialization t in
    let%bind () = require_developer_feature initialization (developer_feature request) in
    let%bind capabilities =
      match t.developer_capabilities with
      | Some (source, capabilities) when phys_equal source initialization ->
        Ok capabilities
      | None | Some _ -> Error (developer_error "Developer capabilities are absent")
    in
    if not (phys_equal limits (P.Developer_common.Capabilities.limits capabilities))
    then Error (developer_error "Developer response limits changed")
    else (
      match request with
      | D.Request.Capabilities -> Ok (D.Response.Capabilities capabilities)
      | _ ->
        let method_ = D.Request.method_name request in
        let%bind selected_method = P.Developer_common.Method.of_string method_ in
        let%bind () =
          if P.Developer_common.Capabilities.supports capabilities selected_method
          then Ok ()
          else Error (developer_error "Developer method is not advertised")
        in
        let%bind request =
          D.Request.of_method_and_params
            ~limits
            ~method_
            ~params:(D.Request.params request)
        in
        let%bind json =
          request_developer_transport
            t
            ~bound:(Transport.Developer_response_bound.Installed limits)
            request
        in
        let%bind response = D.Response.of_method_and_json ~limits ~method_ json in
        if
          Atomic.get t.closed
          || (not
                (Option.value_map
                   t.initialization
                   ~default:false
                   ~f:(phys_equal initialization)))
          || not (developer_view_matches initialization request response)
        then Error (developer_error "Developer response changed request identity")
        else Ok response))
;;

let request_repl_transport t ~initialization ~bound request =
  match
    Transport.request_repl
      t.transport
      ~max_request_bytes:initialization.P.Initialize.Response.limits.max_request_bytes
      ~bound
      request
  with
  | Ok json
    when String.length (Jsonaf.to_string json)
         > Transport.Repl_response_bound.max_result_bytes bound ->
    Error (developer_error "REPL success response exceeds its bound")
  | Error failure when not (Transport.Repl_response_bound.error_within_bound failure) ->
    Error (developer_error "REPL error response exceeds its bound")
  | result -> result
;;

let repl_capabilities t =
  with_request_lock t (fun () ->
    let module R = P.Developer_repl_wire in
    let open Result.Let_syntax in
    let%bind initialization = developer_initialization t in
    let%bind () = require_developer_feature initialization R.Features.repl in
    match t.repl_capabilities with
    | Some (source, capabilities) when phys_equal source initialization -> Ok capabilities
    | None | Some _ ->
      let%bind json =
        request_repl_transport
          t
          ~initialization
          ~bound:Transport.Repl_response_bound.Bootstrap
          R.Request.Capabilities
      in
      let%bind capabilities = R.Capabilities.of_json json in
      if
        Atomic.get t.closed
        || not
             (Option.value_map
                t.initialization
                ~default:false
                ~f:(phys_equal initialization))
      then Error (developer_error "REPL initialization changed")
      else if not (R.Capabilities.supports capabilities "developer.repl.capabilities")
      then Error (developer_error "REPL Capabilities method is not advertised")
      else (
        t.repl_capabilities <- Some (initialization, capabilities);
        Ok capabilities))
;;

let repl_feature = function
  | P.Developer_repl_wire.Request.Watch _ | Unwatch _ ->
    P.Developer_repl_wire.Features.events
  | Capabilities
  | Create _
  | Submit _
  | Observe _
  | Cancel_submission _
  | Inspect_type _
  | Task_references_page _
  | Task_submit _
  | Task_observe _
  | Task_await_terminal _
  | Task_cancel_and_wait _
  | Reset_and_wait _
  | Close_and_wait _ -> P.Developer_repl_wire.Features.repl
;;

let repl_response_matches initialization request response =
  let module R = P.Developer_repl_wire in
  let owner_matches view ~key ~run_id =
    let owner = R.Task_view.owner view in
    P.Id.Server.equal owner.host_id initialization.P.Initialize.Response.server_id
    && P.Id.Principal.equal
         owner.principal_id
         initialization.P.Initialize.Response.principal.id
    && P.Id.Developer_run.equal owner.run_id run_id
    && P.Idempotency_key.equal (R.Task_view.key view) key
  in
  let task_key_matches view ~key =
    let owner = R.Task_view.owner view in
    P.Id.Server.equal owner.host_id initialization.P.Initialize.Response.server_id
    && P.Id.Principal.equal
         owner.principal_id
         initialization.P.Initialize.Response.principal.id
    && P.Idempotency_key.equal (R.Task_view.key view) key
  in
  match request, response with
  | R.Request.Capabilities, R.Response.Capabilities _ -> true
  | Create create, Create view ->
    P.Id.Developer_run.equal (R.Create.run_id create) (R.View.run_id view)
  | Submit submit, Submit submission ->
    R.Wire_id.Epoch.equal (R.Submit.epoch submit) (R.Submission.epoch submission)
  | Observe _, Observe _
  | Cancel_submission _, Cancel_submission _
  | Inspect_type _, Inspect_type _
  | Task_references_page _, Task_references_page _ -> true
  | Task_submit submit, Task_submit view ->
    task_key_matches view ~key:(R.Task_submit.key submit)
  | Task_observe lookup, Task_observe (Some view)
  | Task_await_terminal lookup, Task_await_terminal view
  | Task_cancel_and_wait lookup, Task_cancel_and_wait view ->
    owner_matches view ~key:lookup.key ~run_id:lookup.run_id
  | Task_observe _, Task_observe None -> true
  | Reset_and_wait current, Reset_and_wait view ->
    R.Wire_id.Instance.equal (R.Current_epoch.instance current) (R.View.instance view)
  | Close_and_wait _, Close_and_wait | Watch _, Watch | Unwatch _, Unwatch -> true
  | Capabilities, _
  | Create _, _
  | Submit _, _
  | Observe _, _
  | Cancel_submission _, _
  | Inspect_type _, _
  | Task_references_page _, _
  | Task_submit _, _
  | Task_observe _, _
  | Task_await_terminal _, _
  | Task_cancel_and_wait _, _
  | Reset_and_wait _, _
  | Close_and_wait _, _
  | Watch _, _
  | Unwatch _, _ -> false
;;

let request_repl t ~limits request =
  with_request_lock t (fun () ->
    let module R = P.Developer_repl_wire in
    let open Result.Let_syntax in
    let%bind initialization = developer_initialization t in
    let%bind () = require_developer_feature initialization (repl_feature request) in
    let%bind capabilities =
      match t.repl_capabilities with
      | Some (source, capabilities) when phys_equal source initialization ->
        Ok capabilities
      | None | Some _ -> Error (developer_error "REPL capabilities are absent")
    in
    if not (phys_equal limits (R.Capabilities.limits capabilities))
    then Error (developer_error "REPL response limits changed")
    else (
      let method_ = R.Request.method_name request in
      let%bind () =
        if R.Capabilities.supports capabilities method_
        then Ok ()
        else Error (developer_error "REPL method is not advertised")
      in
      match request with
      | R.Request.Capabilities -> Ok (R.Response.Capabilities capabilities)
      | _ ->
        let%bind request =
          R.Request.of_method_and_params
            ~limits
            ~method_
            ~params:(R.Request.params request)
        in
        let%bind json =
          request_repl_transport
            t
            ~initialization
            ~bound:(Transport.Repl_response_bound.Installed limits)
            request
        in
        let%bind response = R.Response.of_method_and_json ~limits ~method_ json in
        if
          Atomic.get t.closed
          || (not
                (Option.value_map
                   t.initialization
                   ~default:false
                   ~f:(phys_equal initialization)))
          || not (repl_response_matches initialization request response)
        then Error (developer_error "REPL response changed request identity")
        else Ok response))
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

let pending_identity pending = pending.recovery_identity
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
    let%bind () =
      match pending.command, receipt with
      | ( P.Command.Prompt_dependencies_update request
        , P.Command_receipt.Committed (Prompt_dependencies_updated result) ) ->
        if P.Prompt_dependency_selection.Update_result.matches_request result request
        then Ok ()
        else
          Error
            (P.Error.create
               Interrupted
               ~message:"dependency receipt does not match its original request"
               ~retryable:false
               ())
      | P.Command.Prompt_dependencies_update _, P.Command_receipt.Committed _ ->
        Error
          (P.Error.create
             Interrupted
             ~message:"unexpected committed dependency receipt"
             ~retryable:false
             ())
      | _, _ -> Ok ()
    in
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
