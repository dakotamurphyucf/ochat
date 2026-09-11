open! Core

type status =
  | Starting
  | Ready
  | Draining
  | Stopped
  | Failed of Agent_protocol.Error.t
[@@deriving sexp]

type options =
  { implementation_name : string
  ; implementation_version : string
  ; features : string list
  ; extension_host : Agent_protocol.Extension_capabilities.host
  ; protocol_limits : Agent_protocol.Initialize.Limits.t
  ; timing : Agent_protocol.Initialize.Timing.t
  ; factory_limits : Session_factory.limits
  ; quota_limits : Agent_session.Quota_manager.limits
  ; reviewer_resolver : Catalog_builder.reviewer_resolver option
  ; policy_evaluator_resolver : Catalog_builder.policy_evaluator_resolver option
  ; model_post_stream : Agent_session.Runtime_builder.model_post_stream option
  ; qualify_chatml_extensions : bool
  ; chatml_runtime_policy : Chat_response.Runtime_semantics.policy
  ; authoring_validation_host : Chat_response.Authoring_validation.host option
  ; oauth_resolver : (string -> Authenticator.bearer_validator option) option
  }

type t =
  { env : Eio_unix.Stdenv.base
  ; store : Agent_store.Session_store.t
  ; blob_store : Agent_store.Blob_store.t
  ; prompts : Agent_session.Prompt_catalog.t
  ; workspaces : Agent_session.Workspace_catalog.t
  ; registry : Session_registry.t
  ; handler : Command_handler.t
  ; dispatcher : Dispatcher.t
  ; start_scheduler : Start_scheduler.t
  ; job_scheduler : Job_scheduler.t
  ; permission_scheduler : Permission_scheduler.t
  ; schedule_scheduler : Schedule_scheduler.t
  ; maintenance : Maintenance.t
  ; config_watcher : Config_watcher.t
  ; factory : Session_factory.t
  ; http_authenticator : Authenticator.t option
  ; oauth_bearer_validator : Authenticator.bearer_validator option
  ; reverse_proxy : Config.Server.reverse_proxy option
  ; anonymous_http_principal : Agent_protocol.Principal.t option
  ; shutdown_grace_seconds : float
  ; status_ref : status ref
  }

type health_services =
  { health_env : Eio_unix.Stdenv.base
  ; health_store : Agent_store.Session_store.t
  ; health_registry : Session_registry.t
  ; health_start_scheduler : Start_scheduler.t
  ; health_job_scheduler : Job_scheduler.t
  ; health_permission_scheduler : Permission_scheduler.t
  ; health_schedule_scheduler : Schedule_scheduler.t
  ; health_maintenance : Maintenance.t
  ; health_config_watcher : Config_watcher.t
  ; health_status_ref : status ref
  }

let default_options =
  { implementation_name = "ochat-agent-server"
  ; implementation_version = "dev"
  ; extension_host = Daemon
  ; features =
      [ "attachments.multi_client"
      ; "events.durable"
      ; "events.recoverable"
      ; "permissions.configurable"
      ; "sessions.durable"
      ; "workspaces.configured"
      ]
  ; protocol_limits =
      { max_request_bytes = 16 * 1024 * 1024
      ; max_event_bytes = 16 * 1024 * 1024
      ; max_page_size = 1_000
      ; max_attachments_per_connection = 64
      }
  ; timing =
      { heartbeat_interval_ms = 15_000
      ; owner_lease_duration_ms = 60_000
      ; owner_renew_after_ms = 30_000
      ; disconnect_grace_default_ms = 10_000
      }
  ; factory_limits =
      { max_journal_payload = 16 * 1024 * 1024
      ; max_segment_bytes = Int64.of_int (64 * 1024 * 1024)
      ; max_segment_frames = 10_000
      ; commit_queue_capacity = 1_024
      ; mailbox_capacity = 1_024
      ; snapshot_payload_limit = 64 * 1024 * 1024
      ; snapshot_every_events = 100
      ; snapshot_every_ms = 5_000
      ; moderator_reservation_size = 4_096
      ; history_block_size = 256
      ; owner_lease_duration_ms = 60_000
      ; event_replay_capacity = 100_000
      ; max_attachments_per_session = 1_024
      ; subscriber_queue_capacity = 512
      ; job_result_inline_bytes = 64 * 1024
      ; job_result_max_bytes = 9 * 1024 * 1024
      ; job_result_recovery_max_count = 4096
      ; delegation_recovery_max_count = 4096
      ; delegation_max_depth = 32
      ; delegation_recovery_max_bytes = 67108864
      ; job_result_recovery_max_bytes = 64 * 1024 * 1024
      ; subscriptions = Agent_session.Staged_subscriptions.default_limits
      ; schedules = Agent_session.Staged_schedules.default_limits
      ; notifications = Agent_session.Staged_notifications.default_limits
      ; ingress = Agent_session.Staged_ingress.default_limits
      ; job_result_collection =
          { max_intents = 4096
          ; max_entries = 65_536
          ; max_bytes = 256 * 1024 * 1024
          ; max_file_bytes = 64 * 1024 * 1024
          }
      }
  ; quota_limits =
      { global_running_sessions = 256
      ; per_principal_running_sessions = 64
      ; runtime_construction = 8
      }
  ; reviewer_resolver = None
  ; policy_evaluator_resolver = None
  ; model_post_stream = None
  ; qualify_chatml_extensions = false
  ; chatml_runtime_policy = Chat_response.Runtime_semantics.default_policy
  ; authoring_validation_host = None
  ; oauth_resolver = None
  }
;;

let status t = !(t.status_ref)
let dispatcher t = t.dispatcher
let registry t = t.registry
let store t = t.store
let blob_store t = t.blob_store
let prompts t = t.prompts
let workspaces t = t.workspaces

let timestamp env =
  Eio.Time.now (Eio.Stdenv.clock env)
  |> Time_ns.Span.of_sec
  |> Time_ns.of_span_since_epoch
  |> Agent_protocol.Timestamp.of_time_ns
;;

let unauthenticated message =
  Agent_protocol.Error.create Unauthenticated ~message ~retryable:false ()
;;

let validate_oauth validate ~now ~token =
  try validate ~now ~token with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | _ -> Error (unauthenticated "bearer token is invalid")
;;

let authenticate_with_validators t token =
  let now = timestamp t.env in
  let static () =
    Option.map t.http_authenticator ~f:(fun authenticator ->
      Authenticator.authenticate_bearer authenticator ~now ~token)
  in
  let oauth () =
    Option.map t.oauth_bearer_validator ~f:(fun validate ->
      validate_oauth validate ~now ~token)
  in
  match static (), oauth () with
  | Some (Ok principal), _ | _, Some (Ok principal) -> Ok principal
  | Some (Error _), Some (Error _) | Some (Error _), None | None, Some (Error _) ->
    Error (unauthenticated "bearer token is invalid")
  | None, None -> Error (unauthenticated "bearer authentication is unavailable")
;;

let authenticate_proxy t identity =
  match t.reverse_proxy with
  | None -> Ok None
  | Some proxy ->
    Authenticator.authenticate_reverse_proxy
      ~trusted_addresses:proxy.trusted_addresses
      ~principal_header:proxy.principal_header
      ~scopes_header:proxy.scopes_header
      identity
;;

let authenticate_http t identity token =
  let open Result.Let_syntax in
  let%bind proxy_principal = authenticate_proxy t identity in
  match proxy_principal, token, t.anonymous_http_principal with
  | Some principal, _, _ -> Ok principal
  | None, Some token, _ -> authenticate_with_validators t token
  | None, None, Some principal -> Ok principal
  | None, None, None -> Error (unauthenticated "HTTP authentication is required")
;;

let authenticate_http_bearer t token =
  let identity =
    Authenticator.Request_identity.
      { client_address = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0); headers = [] }
  in
  authenticate_http t identity token
;;

let protocol_of_store = Agent_store.Store_error.to_protocol_error

let all_scopes =
  Agent_protocol.Scope.Set.of_list
    [ List_prompts
    ; List_workspaces
    ; Create_sessions
    ; View_session_transcript
    ; Send_messages
    ; Own_sessions
    ; Answer_approvals
    ; View_security_state
    ; Manage_grants
    ; Read_audit
    ; Stop_sessions
    ; Delete_sessions
    ; Administer_configuration
    ; Diagnostics
    ; Submit_ingress
    ]
;;

let anonymous_http_principal (server : Config.Server.t) =
  if server.http.enabled && not server.http.require_auth
  then
    Agent_protocol.Principal.create
      ~id:(Agent_protocol.Id.Principal.create ())
      ~authentication_kind:"http.development_anonymous"
      ~scopes:all_scopes
      ~attributes:[]
    |> Result.map ~f:Option.some
  else Ok None
;;

let http_authenticator ~env (server : Config.Server.t) =
  match server.http.enabled, server.http.require_auth, server.http.static_tokens_file with
  | true, true, Some path ->
    Authenticator.load_static_file ~env ~path |> Result.map ~f:Option.some
  | false, _, _ | true, false, _ | true, true, None -> Ok None
;;

let oauth_bearer_validator options (server : Config.Server.t) =
  match server.http.enabled, server.http.require_auth, server.http.oauth_validator with
  | true, true, Some id ->
    Option.bind options.oauth_resolver ~f:(fun resolve -> resolve id)
    |> Result.of_option
         ~error:(unauthenticated ("HTTP OAuth validator is unavailable: " ^ id))
    |> Result.map ~f:Option.some
  | false, _, _ | true, false, _ | true, true, None -> Ok None
;;

let durability config =
  match config.Config.Server.durability.journal_flush with
  | Each | Interval -> Agent_store.Journal_segment.Flush
  | Unsafe_buffered -> Buffered
;;

let open_store ~sw ~env config ~process_start_identity =
  let root = config.Config.Server.data_dir in
  let schema = Eio.Path.(Eio.Stdenv.fs env / root / "schema.sexp") in
  let lock_nonce =
    Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string
  in
  if Eio.Path.is_file schema
  then
    Agent_store.Session_store.open_existing
      ~env
      ~sw
      ~root
      ~process_start_identity
      ~lock_nonce
  else
    Agent_store.Session_store.create
      ~env
      ~sw
      ~root
      ~server_id:(Agent_protocol.Id.Server.create ())
      ~process_start_identity
      ~lock_nonce
;;

let build_catalog ~env store config reviewer_resolver policy_evaluator_resolver =
  let open Result.Let_syntax in
  let%bind built =
    Catalog_builder.build ?reviewer_resolver ?policy_evaluator_resolver config
  in
  let%bind artifact_store =
    Agent_store.Prompt_artifact_store.create
      ~env
      ~root:
        (Agent_store.Session_store.data_root store
         |> Agent_store.Data_root.prompt_artifacts_path)
  in
  let prompts = Agent_session.Prompt_catalog.create ~env ~artifact_store in
  let prepared =
    Agent_session.Prompt_catalog.prepare
      prompts
      ~transaction_id:(fun _ -> Agent_protocol.Id.Transaction.create ())
      ~created_at:(timestamp env)
      built.prompts
  in
  Agent_session.Prompt_catalog.install prompts prepared;
  Ok (built, prompts)
;;

let implementation options =
  Agent_protocol.Initialize.Implementation.create
    ~name:options.implementation_name
    ~version:options.implementation_version
;;

(* Qualification is deliberately empty until the execution services pass their
   host-specific acceptance suites. Configuring a feature string cannot enable it. *)
let extension_capabilities options config =
  Agent_protocol.Extension_capabilities.create
    ~host:options.extension_host
    ~journal_flush:
      (match durability config.Config.server with
       | Flush -> Synced
       | Buffered -> Buffered)
    ~available_features:[]
  |> function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let enabled_features options capabilities requested =
  List.filter options.features ~f:(fun feature ->
    List.mem requested feature ~equal:String.equal)
  |> Agent_protocol.Extension_capabilities.filter_available capabilities
;;

let initialize
      env
      options
      ~event_replay_capacity
      ~capabilities
      implementation
      store
      status_ref
      ~principal
      request
  =
  let open Result.Let_syntax in
  let%bind selected_version =
    Agent_protocol.Version.negotiate
      ~client_min:request.Agent_protocol.Initialize.Request.protocol_min
      ~client_max:request.protocol_max
      ~supported:[ Agent_protocol.Version.initial; Agent_protocol.Version.current ]
  in
  if Poly.equal !status_ref Draining || Poly.equal !status_ref Stopped
  then
    Error
      (Agent_protocol.Error.create
         Server_shutting_down
         ~message:"server is not accepting new connections"
         ~retryable:true
         ())
  else (
    let principal =
      match
        Agent_protocol.Version.compare
          selected_version
          Agent_protocol.Version.ingress_minimum
        < 0
      with
      | false -> principal
      | true ->
        { principal with
          Agent_protocol.Principal.scopes =
            Set.remove principal.Agent_protocol.Principal.scopes Submit_ingress
        }
    in
    Agent_protocol.Initialize.Response.create
      ~protocol_name:"ochat.agent"
      ~selected_version
      ~implementation
      ~server_id:(Agent_store.Session_store.server_id store)
      ~enabled_features:(enabled_features options capabilities request.features)
      ~extensions:(Some capabilities)
      ~principal
      ~limits:options.protocol_limits
      ~event_retention:
        { minimum_age_ms = 0
        ; maximum_events = event_replay_capacity
        ; oldest_replayable_sequence = None
        }
      ~timing:options.timing
      ~server_time:(timestamp env))
;;

let ready = function
  | Ready -> true
  | Starting | Draining | Stopped | Failed _ -> false
;;

let draining = function
  | Draining | Stopped -> true
  | Starting | Ready | Failed _ -> false
;;

let ping env status_ref request =
  Agent_protocol.Ping.Response.
    { payload = request.Agent_protocol.Ping.Request.payload
    ; server_time = timestamp env
    ; ready = ready !status_ref
    ; draining = draining !status_ref
    }
;;

let transports (config : Config.t) =
  let values = [ "unix_socket" ] in
  if config.server.http.enabled then values @ [ "http" ] else values
;;

let server_info (options : options) implementation (config : Config.t) store =
  Agent_protocol.Method_result.Server_info.
    { server_id = Agent_store.Session_store.server_id store
    ; implementation
    ; protocol_version = Agent_protocol.Version.current
    ; features =
        Agent_protocol.Extension_capabilities.filter_available
          (extension_capabilities options config)
          options.features
    ; transports = transports config
    ; limits = options.protocol_limits
    ; unsafe_development_auth =
        config.server.http.enabled && not config.server.http.require_auth
    }
;;

let health_status = function
  | Ready -> Agent_protocol.Health.Healthy
  | Starting | Draining -> Degraded
  | Stopped | Failed _ -> Unhealthy
;;

let component ?message name status =
  Agent_protocol.Health.Component.{ name; status; message }
;;

let daemon_component status_ref =
  let message =
    match !status_ref with
    | Failed error -> Some error.Agent_protocol.Error.message
    | Starting -> Some "startup is still in progress"
    | Draining -> Some "shutdown is in progress"
    | Ready | Stopped -> None
  in
  component ?message "daemon" (health_status !status_ref)
;;

let store_component store =
  match Agent_store.Session_store.check_writable store with
  | Ok () -> component ~message:"data root is writable" "storage" Healthy
  | Error _ -> component ~message:"data root write probe failed" "storage" Unhealthy
;;

let registry_component registry =
  let stats = Session_registry.stats registry in
  component
    ~message:(sprintf "%d loaded, %d indexed sessions" stats.loaded stats.indexed)
    "session_registry"
    Healthy
;;

let scheduler_component name running message =
  if running
  then component ~message name Healthy
  else component ~message:"scheduler is stopped" name Unhealthy
;;

let maintenance_component maintenance =
  let status = Maintenance.status maintenance in
  match status.running, status.last_error, status.last_stats with
  | false, _, _ ->
    component ~message:"maintenance service is stopped" "maintenance" Unhealthy
  | true, Some _, _ ->
    component ~message:"last maintenance cycle failed" "maintenance" Degraded
  | true, None, Some stats ->
    component
      ~message:
        (sprintf
           "last cycle pruned %d idempotency records and %d temporary blobs"
           stats.expired_idempotency_records
           stats.expired_temporary_blobs)
      "maintenance"
      Healthy
  | true, None, None ->
    component ~message:"awaiting first maintenance cycle" "maintenance" Healthy
;;

let config_component config_watcher =
  match Config_watcher.status config_watcher with
  | false, _ ->
    component ~message:"configuration watcher is stopped" "configuration" Unhealthy
  | true, Some diagnostics ->
    component
      ~message:
        (sprintf "last reload failed with %d diagnostics" (List.length diagnostics))
      "configuration"
      Degraded
  | true, None ->
    component ~message:"configuration watcher is running" "configuration" Healthy
;;

let health_components t =
  [ daemon_component t.health_status_ref
  ; store_component t.health_store
  ; registry_component t.health_registry
  ; scheduler_component
      "start_scheduler"
      (Start_scheduler.is_running t.health_start_scheduler)
      "start scheduler is running"
  ; scheduler_component
      "job_scheduler"
      (Job_scheduler.is_running t.health_job_scheduler)
      (sprintf "%d jobs running" (Job_scheduler.running_count t.health_job_scheduler))
  ; scheduler_component
      "schedule_scheduler"
      (Schedule_scheduler.is_running t.health_schedule_scheduler)
      "schedule scheduler is running"
  ; scheduler_component
      "permission_scheduler"
      (Permission_scheduler.is_running t.health_permission_scheduler)
      "permission scheduler is running"
  ; maintenance_component t.health_maintenance
  ; config_component t.health_config_watcher
  ]
;;

let aggregate_health current components =
  if
    List.exists components ~f:(fun value ->
      Agent_protocol.Health.equal_status
        value.Agent_protocol.Health.Component.status
        Unhealthy)
  then Agent_protocol.Health.Unhealthy
  else if
    Agent_protocol.Health.equal_status (health_status current) Degraded
    || List.exists components ~f:(fun value ->
      Agent_protocol.Health.equal_status
        value.Agent_protocol.Health.Component.status
        Degraded)
  then Degraded
  else Healthy
;;

let server_health t request =
  let current = !(t.health_status_ref) in
  let components = health_components t in
  Agent_protocol.Health.Response.
    { status = aggregate_health current components
    ; ready = ready current
    ; draining = draining current
    ; checked_at = timestamp t.health_env
    ; components =
        (if request.Agent_protocol.Health.Request.include_details then components else [])
    }
;;

let health t ~include_details =
  let services =
    { health_env = t.env
    ; health_store = t.store
    ; health_registry = t.registry
    ; health_start_scheduler = t.start_scheduler
    ; health_job_scheduler = t.job_scheduler
    ; health_permission_scheduler = t.permission_scheduler
    ; health_schedule_scheduler = t.schedule_scheduler
    ; health_maintenance = t.maintenance
    ; health_config_watcher = t.config_watcher
    ; health_status_ref = t.status_ref
    }
  in
  server_health services Agent_protocol.Health.Request.{ include_details }
;;

let reload_diagnostic config code message remediation =
  Config.Diagnostic.
    { code
    ; config_path = "$"
    ; source_file = config.Config.source_file
    ; message
    ; remediation
    }
;;

let config_build_diagnostic config error =
  reload_diagnostic
    config
    "config.catalog_build"
    (Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
    "Correct the catalog definitions and reload the configuration."
;;

let config_watcher
      ~sw
      ~env
      ~config
      ~prompts
      ~factory
      ~audit_store
      ~reviewer_resolver
      ~policy_evaluator_resolver
  =
  let prepared = ref None in
  let prepare diff candidate =
    if diff.Config_diff.server_changed
    then
      Error
        [ reload_diagnostic
            candidate
            "config.restart_required"
            "server listener, storage, durability, or retention changes require restart"
            "Restart the daemon with the new server configuration."
        ]
    else (
      match
        Catalog_builder.build ?reviewer_resolver ?policy_evaluator_resolver candidate
      with
      | Error error -> Error [ config_build_diagnostic candidate error ]
      | Ok built ->
        let prompt_revisions =
          Agent_session.Prompt_catalog.prepare
            prompts
            ~transaction_id:(fun _ -> Agent_protocol.Id.Transaction.create ())
            ~created_at:(timestamp env)
            built.prompts
        in
        prepared := Some (built, prompt_revisions);
        Ok ())
  in
  let commit _ candidate =
    let built, prompt_revisions = Option.value_exn !prepared in
    prepared := None;
    Session_factory.install_catalogs
      factory
      ~workspaces:built.Catalog_builder.workspaces
      ~permission_profiles:built.permission_profiles
      ~manifest_grants:built.manifest_grants;
    Agent_session.Prompt_catalog.install prompts prompt_revisions;
    ignore candidate
  in
  let audit diff =
    ignore
      (Agent_store.Audit_store.append
         audit_store
         ~timestamp:(timestamp env)
         ~level:Agent_protocol.Audit.Info
         ~name:"server.configuration.reloaded"
         ~session_id:None
         ~principal_id:None
         ~payload:
           (`Object
               [ "diff", `String (Sexp.to_string_mach ([%sexp_of: Config_diff.t] diff)) ])
         ~redacted:true
       : (Agent_protocol.Audit.t, Agent_store.Store_error.t) result)
  in
  let watcher =
    Config_watcher.create
      ~env
      ~path:config.Config.source_file
      ~initial:config
      ~hooks:{ prepare; commit; audit }
  in
  Config_watcher.run ~sw ~clock:(Eio.Stdenv.clock env) ~every:1. watcher;
  watcher
;;

let close_store_on_error store result =
  match result with
  | Ok _ -> result
  | Error _ as failure ->
    ignore
      (Agent_store.Session_store.close store : (unit, Agent_store.Store_error.t) result);
    failure
;;

let compose ~sw ~env ~(config : Config.t) ~tool_dir ~home ~options store built prompts =
  let open Result.Let_syntax in
  let%bind implementation = implementation options in
  let%bind http_authenticator = http_authenticator ~env config.server in
  let%bind oauth_bearer_validator = oauth_bearer_validator options config.server in
  let%bind anonymous_http_principal = anonymous_http_principal config.server in
  let data_root = Agent_store.Session_store.data_root store in
  let%bind blob_store =
    Agent_store.Blob_store.create
      ~env
      ~temporary_directory:(Agent_store.Data_root.temporary_blobs_path data_root)
      ~durable_directory:(Agent_store.Data_root.durable_blobs_path data_root)
      ~max_upload_bytes:(Int64.of_int options.protocol_limits.max_request_bytes)
    |> Result.map_error ~f:protocol_of_store
  in
  let%bind idempotency_store =
    Agent_store.Idempotency_store.open_or_create
      ~env
      ~path:
        (Filename.concat
           (Agent_store.Data_root.indexes_path data_root)
           "idempotency.sexp")
    |> Result.map_error ~f:protocol_of_store
  in
  let%bind audit_store =
    Agent_store.Audit_store.open_or_create
      ~env
      ~directory:(Agent_store.Data_root.audit_path data_root)
      ~max_payload_length:options.protocol_limits.max_event_bytes
    |> Result.map_error ~f:protocol_of_store
  in
  let registry = Session_registry.create () in
  let start_queue = Agent_session.Start_queue.create () in
  let%bind quota_manager =
    Agent_session.Quota_manager.create
      ~limits:options.quota_limits
      ~workspace_leases:(Agent_session.Workspace_lease.create ())
  in
  let job_capacity = Job_capacity.create ~limits:config.server.job_limits in
  let factory_limits =
    { options.factory_limits with
      snapshot_every_events = config.server.durability.snapshot_every_events
    ; snapshot_every_ms = config.server.durability.snapshot_every_ms
    ; event_replay_capacity = config.server.event_retention.max_events_per_session
    ; max_attachments_per_session = config.server.max_attachments_per_session
    ; subscriber_queue_capacity = config.server.subscriber_queue_capacity
    }
  in
  let factory =
    Session_factory.create
      ~sw
      ~env
      ~store
      ~registry
      ~idempotency_store
      ~blob_store
      ~prompts
      ~workspaces:built.Catalog_builder.workspaces
      ~permission_profiles:built.permission_profiles
      ~manifest_grants:built.manifest_grants
      ~quota_manager
      ~job_capacity
      ~tool_dir
      ~home
      ~model_post_stream:options.model_post_stream
      ~qualify_chatml_extensions:options.qualify_chatml_extensions
      ~chatml_runtime_policy:options.chatml_runtime_policy
      ~authoring_validation_host:options.authoring_validation_host
      ~durability:(durability config.server)
      ~limits:factory_limits
  in
  let indexed_sessions =
    Agent_store.Session_store.list_sessions store
    |> List.filter ~f:(fun entry -> not entry.Agent_store.Session_index.Entry.archived)
  in
  Session_registry.index_all registry indexed_sessions;
  Session_registry.install_loader registry (Session_factory.recover_session factory);
  let%bind recovered = Session_factory.recover_sessions factory in
  let pinned_revisions =
    List.filter_map indexed_sessions ~f:(fun entry ->
      entry.Agent_store.Session_index.Entry.session.prompt_revision)
  in
  ignore
    (Agent_store.Delegation_store.with_records
       (Agent_store.Session_store.delegations store)
       ~max_records:factory_limits.delegation_recovery_max_count
       ~max_bytes:factory_limits.delegation_recovery_max_bytes
       ~f:(fun records ->
         let generated_revisions =
           List.map records ~f:(fun record ->
             record.Agent_store.Delegation_store.admission.revision_id)
         in
         Agent_session.Prompt_catalog.prune_unreferenced_artifacts
           prompts
           ~additional:(generated_revisions @ pinned_revisions))
     : (int, Agent_store.Store_error.t) result);
  let%bind () = Start_scheduler.seed_recovered ~registry ~queue:start_queue in
  let startup_time = timestamp env in
  let%bind () =
    Job_scheduler.reconcile_recovered
      ~registry
      ~max_count:factory_limits.job_result_recovery_max_count
      ~max_total_bytes:factory_limits.job_result_recovery_max_bytes
  in
  let%bind () = Schedule_scheduler.reconcile_recovered ~registry ~startup_time in
  let%bind () = Session_factory.complete_index_recovery factory recovered in
  let start_scheduler =
    Start_scheduler.start ~sw ~clock:(Eio.Stdenv.clock env) ~registry ~queue:start_queue
  in
  let job_scheduler =
    Job_scheduler.start ~sw ~clock:(Eio.Stdenv.clock env) ~registry ~capacity:job_capacity
  in
  let permission_scheduler =
    Permission_scheduler.start ~sw ~clock:(Eio.Stdenv.clock env) ~registry
  in
  let schedule_scheduler =
    Schedule_scheduler.start ~sw ~clock:(Eio.Stdenv.mono_clock env) ~registry
  in
  let maintenance =
    Maintenance.start
      ~env
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~every:60.
      ~idempotency_store
      ~blob_store
      ~response_retention:
        (Time_ns.Span.of_ms
           (Float.of_int config.server.event_retention.response_artifact_ms))
      ~registry
      ~session_store:store
      ~on_error:(fun _ -> ())
  in
  let status_ref = ref Ready in
  let config_watcher =
    config_watcher
      ~sw
      ~env
      ~config
      ~prompts
      ~factory
      ~audit_store
      ~reviewer_resolver:options.reviewer_resolver
      ~policy_evaluator_resolver:options.policy_evaluator_resolver
  in
  let health_services =
    { health_env = env
    ; health_store = store
    ; health_registry = registry
    ; health_start_scheduler = start_scheduler
    ; health_job_scheduler = job_scheduler
    ; health_permission_scheduler = permission_scheduler
    ; health_schedule_scheduler = schedule_scheduler
    ; health_maintenance = maintenance
    ; health_config_watcher = config_watcher
    ; health_status_ref = status_ref
    }
  in
  let handler =
    Command_handler.create
      ~sw
      ~env
      ~registry
      ~prompts
      ~workspaces:built.workspaces
      ~start_queue
      ~idempotency_store
      ~audit_store
      ~blob_store
      ~session_store:store
      ~initialize:
        (initialize
           env
           options
           ~event_replay_capacity:factory_limits.event_replay_capacity
           ~capabilities:(extension_capabilities options config)
           implementation
           store
           status_ref)
      ~ping:(ping env status_ref)
      ~server_info:(fun () -> server_info options implementation config store)
      ~server_health:(server_health health_services)
      ~cancel_job:(Job_scheduler.cancel job_scheduler)
      ~create_session:(Session_factory.create_session factory)
      ~prepare_administration:(Session_factory.prepare_administration factory)
  in
  let dispatcher = Dispatcher.create handler in
  Ok
    { env
    ; store
    ; blob_store
    ; prompts
    ; workspaces = built.workspaces
    ; registry
    ; handler
    ; dispatcher
    ; start_scheduler
    ; job_scheduler
    ; permission_scheduler
    ; schedule_scheduler
    ; maintenance
    ; config_watcher
    ; factory
    ; http_authenticator
    ; oauth_bearer_validator
    ; reverse_proxy = config.server.http.reverse_proxy
    ; anonymous_http_principal
    ; shutdown_grace_seconds = Float.of_int config.server.shutdown_grace_ms /. 1_000.
    ; status_ref
    }
;;

let start
      ~sw
      ~env
      ~(config : Config.t)
      ~tool_dir
      ~home
      ~process_start_identity
      ?(options = default_options)
      ()
  =
  Mirage_crypto_rng_unix.use_default ();
  let open Result.Let_syntax in
  let%bind () =
    match options.qualify_chatml_extensions with
    | false -> Ok ()
    | true ->
      Agent_session.Automatic_turn_budget.create options.chatml_runtime_policy
      |> Agent_session.Automatic_turn_budget.validate
  in
  let%bind store =
    open_store ~sw ~env config.server ~process_start_identity
    |> Result.map_error ~f:protocol_of_store
  in
  close_store_on_error store
  @@
  let%bind built, prompts =
    build_catalog
      ~env
      store
      config
      options.reviewer_resolver
      options.policy_evaluator_resolver
    |> Result.map_error ~f:protocol_of_store
  in
  compose ~sw ~env ~config ~tool_dir ~home ~options store built prompts
;;

let close_connection t context = Command_handler.close_connection t.handler context
let reload_config t = Config_watcher.reload t.config_watcher

let import_legacy t ~principal ~source_id ~source_path ~legacy request =
  let open Result.Let_syntax in
  let%bind entry =
    Session_factory.import_legacy
      t.factory
      ~principal
      ~source_id
      ~source_path
      ~legacy
      request
  in
  let%bind state = Agent_session.Session_actor.state entry.Session_registry.actor in
  let%map () =
    Session_registry.add t.registry ~session_id:state.identity.session_id entry
  in
  Agent_session.Session_state.summary state
;;

let shutdown_sessions t =
  match
    Eio.Time.with_timeout (Eio.Stdenv.clock t.env) t.shutdown_grace_seconds (fun () ->
      Ok (Session_registry.shutdown t.registry))
  with
  | Ok () | Error `Timeout -> ()
;;

let shutdown t =
  match !(t.status_ref) with
  | Stopped -> Ok ()
  | Starting | Ready | Failed _ | Draining ->
    t.status_ref := Draining;
    Start_scheduler.close t.start_scheduler;
    Job_scheduler.close t.job_scheduler;
    Permission_scheduler.close t.permission_scheduler;
    Schedule_scheduler.close t.schedule_scheduler;
    Maintenance.close t.maintenance;
    Config_watcher.close t.config_watcher;
    shutdown_sessions t;
    Agent_store.Session_store.close t.store
    |> Result.map_error ~f:protocol_of_store
    |> Result.map ~f:(fun () -> t.status_ref := Stopped)
;;
