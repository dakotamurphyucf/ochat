open! Core

type options =
  { prompt_file : string
  ; workspace : string
  ; tool_dir : string
  ; home : string option
  ; storage : Local_storage.t
  ; start_immediately : bool
  ; permission_profile : Config.Permission_profile.t
  ; attachment_mode : Agent_protocol.Session.attachment_mode
  ; event_capacity : int
  }

type host =
  { daemon : Daemon.t
  ; connection : Agent_client.Connection.t
  ; principal : Agent_protocol.Principal.t
  ; event_capacity : int
  ; max_attachments : int
  ; temporary_root : string option
  ; env : Eio_unix.Stdenv.base
  ; script_preparation : Agent_session.Runtime_script_preparation.t option
  ; cleanup : Embedded_host_cleanup.t
  }

type t =
  { host : host
  ; session_id : Agent_protocol.Id.Session.t
  ; attachment : Agent_protocol.Session.Attachment.t
  }

let default_permission_profile =
  Config.Permission_profile.
    { id = "embedded.interactive"
    ; tool_default = Ask
    ; approval_timeout_ms = None
    ; approval_fallback = Deny
    ; manifest_authorization = Require_grant
    }
;;

let interactive_permission_profile ~authorize_shell_manifest =
  match authorize_shell_manifest with
  | false -> default_permission_profile
  | true -> { default_permission_profile with manifest_authorization = Assume_authorized }
;;

let protocol_of_store error =
  Agent_protocol.Error.create
    Persistence_error
    ~message:(Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
    ~retryable:false
    ()
;;

let create_temporary_root env =
  let base = Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp" in
  let rec create attempts =
    if attempts = 0
    then
      Error
        (Agent_protocol.Error.create
           Persistence_error
           ~message:"unable to allocate an embedded data directory"
           ~retryable:true
           ())
    else (
      let nonce =
        Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string
      in
      let path = Filename.concat base ("ochat-embedded-" ^ nonce) in
      try
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / path);
        Ok path
      with
      | Eio.Io _ -> create (attempts - 1))
  in
  create 32
;;

let data_root env options =
  let open Result.Let_syntax in
  let%bind durable = Local_storage.durable_root options.storage ~home:options.home in
  match durable with
  | Some root -> Ok (Local_storage.Root.path root, None)
  | None -> Result.map (create_temporary_root env) ~f:(fun path -> path, Some path)
;;

let server_config ~project_sources data_root =
  Config.Server.
    { data_dir = data_root
    ; session_helpers = []
    ; authoring_packages = []
    ; authoring_budget = None
    ; project_sources
    ; unix_socket = Filename.concat data_root "agent.sock"
    ; http =
        { enabled = false
        ; address = "127.0.0.1"
        ; port = 8787
        ; require_auth = true
        ; static_tokens_file = None
        ; oauth_validator = None
        ; reverse_proxy = None
        ; max_connections = 1_024
        ; idle_connection_timeout_ms = 300_000
        }
    ; shutdown_grace_ms = 5_000
    ; max_attachments_per_session = 1_024
    ; subscriber_queue_capacity = 512
    ; event_retention =
        { completed_stream_ms = 3_600_000
        ; response_artifact_ms = 3_600_000
        ; max_events_per_session = 100_000
        }
    ; durability =
        { journal_flush = Each
        ; journal_flush_ms = 1
        ; snapshot_every_events = 100
        ; snapshot_every_ms = 5_000
        }
    ; job_limits =
        { daemon_total = 16
        ; per_principal = 8
        ; per_prompt = 8
        ; per_workspace = 8
        ; per_session = 4
        ; per_kind = 16
        ; max_nested_depth = 8
        }
    ; unsafe_allow_unauthenticated_remote_http = false
    }
;;

let workspace_config options =
  Config.Workspace.
    { id = "embedded.workspace"
    ; source = Physical options.workspace
    ; access = Shared_write
    ; conflict_domain = None
    ; prompt_limits =
        [ { prompt = "embedded.prompt"; max_root_agents = 1; overflow = Reject } ]
    }
;;

let prompt_config options =
  Config.Prompt.
    { id = "embedded.prompt"
    ; path = options.prompt_file
    ; description = Some "Embedded local ChatMD prompt"
    ; allowed_workspaces = [ "embedded.workspace" ]
    ; permission_profile = options.permission_profile.id
    ; runtime_policy = None
    ; enabled = true
    }
;;

let config ~project_sources options data_root =
  Config.
    { version = current_version
    ; source_file = Filename.concat data_root "embedded-server.sexp"
    ; server = server_config ~project_sources data_root
    ; workspaces = [ workspace_config options ]
    ; prompts = [ prompt_config options ]
    ; permission_profiles = [ options.permission_profile ]
    ; manifest_grants = []
    }
;;

let all_scopes =
  Agent_protocol.Scope.Set.of_list
    [ List_prompts
    ; List_workspaces
    ; Create_sessions
    ; View_session_transcript
    ; View_organization
    ; Manage_organization
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
    ; Provider_view
    ; Provider_manage
    ; Provider_select
    ]
;;

let principal id =
  Agent_protocol.Principal.create
    ~id
    ~authentication_kind:"embedded.local"
    ~scopes:all_scopes
    ~attributes:[]
;;

let make_connection_owner daemon principal event_capacity ~max_attachments =
  let notifications = Eio.Stream.create event_capacity in
  let closed, close_resolver = Eio.Promise.create () in
  let publish_notification envelope =
    (* A disconnected consumer cannot drain its bounded queue. Release publishers
       already waiting for space before detaching their session subscriptions. *)
    Eio.Fiber.first
      (fun () ->
         Eio.Promise.await closed;
         ())
      (fun () -> Eio.Stream.add notifications envelope)
  in
  let context =
    Connection_context.create_authenticated
      ~connection_id:
        (Agent_protocol.Id.Attachment.create () |> Agent_protocol.Id.Attachment.to_string)
      ~actor:(Operator_authorization.trusted_local principal)
      ~transport:In_memory
      ~publish_notification
      ~max_attachments
  in
  let detached = ref false in
  let detach_mutex = Eio.Mutex.create () in
  let close_actual () =
    Eio.Cancel.protect (fun () ->
      Eio.Mutex.use_rw ~protect:true detach_mutex (fun () ->
        if not !detached
        then (
          Daemon.close_connection daemon context;
          detached := true)))
  in
  let connection =
    Agent_client.In_memory.create
      ~request:(fun command ->
        Dispatcher.dispatch_command (Daemon.dispatcher daemon) ~context command)
      ~notifications
      ~close:(fun () ->
        ignore (Eio.Promise.try_resolve close_resolver ());
        close_actual ())
  in
  Embedded_host_cleanup.Connection_owner.create ~connection ~close_actual
;;

let make_connection daemon principal event_capacity ~max_attachments =
  make_connection_owner daemon principal event_capacity ~max_attachments
  |> Embedded_host_cleanup.Connection_owner.connection
;;

let initialize connection =
  let open Result.Let_syntax in
  let%bind implementation =
    Agent_protocol.Initialize.Implementation.create
      ~name:"ochat-embedded-client"
      ~version:"dev"
  in
  let%bind request =
    Agent_protocol.Initialize.Request.create
      ~implementation
      ~protocol_min:Agent_protocol.Version.current
      ~protocol_max:Agent_protocol.Version.current
      ~features:[]
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(16 * 1024 * 1024)
      ()
  in
  match
    Agent_client.Connection.request_without_history
      connection
      (Protocol_initialize request)
  with
  | Ok (Protocol_initialize _) -> Ok ()
  | Ok _ -> Error (Agent_protocol.Error.invalid_request "unexpected initialize response")
  | Error _ as failure -> failure
;;

let session_spec options =
  Agent_protocol.Session.Spec.create
    ~execution_host:Embedded
    ~prompt:(Catalog (Catalog_identity.prompt_definition "embedded.prompt"))
    ~workspace:(Configured (Catalog_identity.workspace_definition "embedded.workspace"))
    ~liveness:Process_bound
    ~persistence:
      (match options.storage with
       | Transient -> Transient
       | Default | Durable _ -> Durable)
    ~permission_profile:options.permission_profile.id
    ~start_immediately:options.start_immediately
    ~labels:[]
    ()
;;

let create_session connection options =
  let open Result.Let_syntax in
  let%bind spec = session_spec options in
  let%bind idempotency_key =
    Agent_protocol.Idempotency_key.of_string
      (Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string)
  in
  let request =
    Agent_protocol.Session.Create_request.
      { spec; requested_mode = None; subscribe = false; idempotency_key }
  in
  match Agent_client.Connection.request connection (Session_create request) with
  | Ok (Session_create result) -> Ok result.session.id
  | Ok _ -> Error (Agent_protocol.Error.invalid_request "unexpected create response")
  | Error _ as failure -> failure
;;

let attach connection ~mode session_id =
  let open Result.Let_syntax in
  let%bind idempotency_key =
    Agent_protocol.Idempotency_key.of_string
      (Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string)
  in
  let request =
    Agent_protocol.Session.Attach_request.
      { session_id
      ; requested_mode = mode
      ; subscribe = true
      ; after_sequence = None
      ; reclaim_token = None
      ; idempotency_key
      }
  in
  match Agent_client.Connection.request connection (Session_attach request) with
  | Ok (Session_attach result) -> Ok result.attachment
  | Ok _ -> Error (Agent_protocol.Error.invalid_request "unexpected attach response")
  | Error _ as failure -> failure
;;

let cleanup_root env = function
  | None -> ()
  | Some path -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / path)
;;

let open_owned_host
      ~sw
      ~env
      ~daemon_options
      ~config
      ~tool_dir
      ~home
      ~event_capacity
      ~temporary_root
  =
  let open Result.Let_syntax in
  let%bind () =
    Agent_store.Local_operator.validate_root ~env ~path:config.Config.server.data_dir
    |> Result.map_error ~f:protocol_of_store
  in
  let operator_id = ref None in
  let captured_store = ref None in
  let namespace_transferred = ref false in
  Eio.Switch.on_release sw (fun () ->
    if not !namespace_transferred
    then
      Option.iter !captured_store ~f:(fun store ->
        if Agent_store.Session_store.is_closed store then cleanup_root env temporary_root));
  let before_activation store =
    captured_store := Some store;
    let%map id =
      match temporary_root with
      | Some _ -> Ok (Agent_protocol.Id.Principal.create ())
      | None ->
        Agent_store.Local_operator.load_or_create ~env store
        |> Result.map_error ~f:protocol_of_store
    in
    operator_id := Some id
  in
  let%bind daemon =
    Daemon.start
      ~sw
      ~env
      ~options:daemon_options
      ~config
      ~tool_dir
      ?home
      ~before_activation
      ~process_start_identity:None
      ()
  in
  let cleanup = Embedded_host_cleanup.create ~sw ~env ~daemon ~temporary_root in
  namespace_transferred := true;
  let finish () =
    let%bind id =
      Result.of_option
        !operator_id
        ~error:
          (Agent_protocol.Error.invalid_request
             "local operator identity was not admitted")
    in
    let%bind principal = principal id in
    let max_attachments =
      daemon_options.Daemon.protocol_limits.max_attachments_per_connection
    in
    let owner = make_connection_owner daemon principal event_capacity ~max_attachments in
    Embedded_host_cleanup.adopt_connection_exn cleanup owner;
    let connection = Embedded_host_cleanup.Connection_owner.connection owner in
    match initialize connection with
    | Error _ as failure -> failure
    | Ok () ->
      Ok
        { daemon
        ; connection
        ; principal
        ; event_capacity
        ; max_attachments
        ; temporary_root
        ; env
        ; script_preparation = daemon_options.script_preparation
        ; cleanup
        }
  in
  Embedded_host_cleanup.protect cleanup finish
;;

let open_host
      ~sw
      ~env
      ?(daemon_options = Daemon.default_options)
      ~startup_mode
      ~config
      ~tool_dir
      ~home
      ~event_capacity
      ()
  =
  Mirage_crypto_rng_unix.use_default ();
  open_owned_host
    ~sw
    ~env
    ~daemon_options:
      { daemon_options with startup_mode; extension_host = Embedded_durable }
    ~config
    ~tool_dir
    ~home:(Some home)
    ~event_capacity
    ~temporary_root:None
;;

let close_host host = Embedded_host_cleanup.close host.cleanup
let host_connection host = host.connection
let host_principal host = host.principal
let host_dispatcher host = Daemon.dispatcher host.daemon

module Script_selection = struct
  type t =
    { session : Agent_protocol.Session.t
    ; moderator : Chat_response.Script_contract_plan.Program.Role.t option
    }
end

let script_selection_failure code message =
  Error (Agent_protocol.Error.create code ~message ~retryable:false ())
;;

let authorize_script_selection host session =
  let module P = Agent_protocol in
  let fail = script_selection_failure in
  if
    P.Principal.has_scope host.principal Own_sessions
    && P.Principal.has_scope host.principal View_session_transcript
    && Authorization.session_visible_to host.principal session
  then Ok ()
  else fail Permission_denied "script selection requires current session visibility"
;;

let script_selection_anchor host ~expected ~poll =
  let module P = Agent_protocol in
  let open Result.Let_syntax in
  let fail = script_selection_failure in
  let session_id =
    P.Session_ref.session_id (P.Session_lifecycle.Expected.reference expected)
  in
  poll ();
  if Embedded_host_cleanup.is_closing host.cleanup
  then fail Server_shutting_down "embedded host is closing"
  else (
    let%bind snapshot = Agent_client.Admin.get_session host.connection session_id in
    let fields = P.Public.Snapshot.fields snapshot in
    let%bind () = authorize_script_selection host fields.session in
    match fields.lifecycle with
    | Some lifecycle
      when P.Session_lifecycle.Expected.equal
             (P.Session_lifecycle.Observation.expected lifecycle)
             expected ->
      (match
         ( P.Session_lifecycle.Observation.status lifecycle
         , P.Session_lifecycle.Observation.admission lifecycle )
       with
       | Active, Automatic -> Ok fields.session
       | _ -> fail Invalid_state "session requires explicit restore or resume")
    | Some _ | None -> fail Conflict "script selection anchor changed")
;;

let script_selection_in_context host ~expected ~startup =
  let module P = Agent_protocol in
  let module A = Agent_session in
  let module Plan = Chat_response.Script_contract_plan in
  let module Expected = P.Session_lifecycle.Expected in
  let open Result.Let_syntax in
  let fail code message = Error (P.Error.create code ~message ~retryable:false ()) in
  let poll () =
    Eio.Fiber.check ();
    Chat_response.Startup_compilation.checkpoint startup;
    Eio.Fiber.check ()
  in
  let reference = Expected.reference expected in
  let session_id = P.Session_ref.session_id reference in
  poll ();
  let%bind () =
    if
      P.Id.Server.equal
        (P.Session_ref.server_id reference)
        (Agent_store.Session_store.server_id (Daemon.store host.daemon))
    then Ok ()
    else fail Invalid_request "script selection belongs to another host"
  in
  let%bind (_ : P.Session.t) = script_selection_anchor host ~expected ~poll in
  let%bind state =
    Session_registry.read_state
      (Daemon.registry host.daemon)
      ~authorize:(authorize_script_selection host)
      session_id
  in
  let summary = A.Session_state.summary state in
  let%bind () =
    if
      Int.equal summary.generation (Expected.generation expected)
      && Int64.equal summary.revision (Expected.session_revision expected)
      && Option.is_none state.spec.delegation
    then Ok ()
    else fail Conflict "selected root session changed or is delegated"
  in
  let%bind definition_id =
    Result.of_option
      state.spec.prompt_definition_id
      ~error:(P.Error.invalid_request "selected session has no authored definition")
  in
  let%bind revision =
    A.Prompt_catalog.restore_revision
      (Daemon.prompts host.daemon)
      ~definition_id
      ~revision_id:state.spec.prompt_revision_id
    |> Result.map_error ~f:(fun diagnostics ->
      P.Error.create
        Invalid_state
        ~message:
          (List.map diagnostics ~f:(fun diagnostic ->
             diagnostic.A.Prompt_revision_builder.Diagnostic.message)
           |> String.concat ~sep:"\n")
        ~retryable:false
        ())
  in
  A.Runtime_script_preparation.with_revision_plan
    host.script_preparation
    ~startup
    revision
    ~env:host.env
    ~invocation:
      (A.Runtime_script_preparation.Invocation.of_root state.spec.root_invocation)
    ~checkpoint:poll
    ~f:(fun startup ~compilation_limits:_ ~plan ~programs:_ ->
      match plan with
      | None -> fail Invalid_state "selected revision lacks authoritative parser custody"
      | Some plan ->
        let%bind versioned, legacy =
          List.fold_result
            (Plan.programs plan)
            ~init:(None, None)
            ~f:(fun (versioned, legacy) program ->
              Chat_response.Startup_compilation.checkpoint startup;
              let role = Plan.Program.role program in
              match role with
              | Standard (Moderator_v1 | Delegated_moderator_v1) ->
                if Option.is_some versioned
                then fail Invalid_state "selected revision has multiple moderators"
                else Ok (Some role, legacy)
              | Legacy_moderator _ -> Ok (versioned, Some role)
              | Standard (One_off_v1 | Tool_v1 | Developer_tool_v1) | Native_hook _ ->
                Ok (versioned, legacy))
        in
        Ok
          Script_selection.
            { session = summary; moderator = Option.first_some versioned legacy })
;;

let session_script_selection host ~expected ~checkpoint =
  let limits =
    Option.value_map
      host.script_preparation
      ~default:Chatml_compilation.default_limits
      ~f:Agent_session.Runtime_script_preparation.compilation
  in
  let map_error (error : Chatml_compilation.error) =
    Agent_protocol.Error.create
      Invalid_state
      ~message:(error.code ^ ": " ^ error.message)
      ~retryable:false
      ()
  in
  let checkpoint () =
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ()
  in
  Chat_response.Startup_compilation.with_context
    ~limits
    ~env:host.env
    ~checkpoint
    ~map_error
    ~f:(fun startup ->
      let open Result.Let_syntax in
      let%bind selected =
        Chat_response.Startup_compilation.within startup ~map_error ~f:(fun () ->
          script_selection_in_context host ~expected ~startup)
      in
      let%map session =
        script_selection_anchor host ~expected ~poll:(fun () ->
          Chat_response.Startup_compilation.checkpoint startup)
      in
      { selected with session })
    ()
;;

let complete_script_activation host ~started ~attachment_id ~checkpoint =
  let module P = Agent_protocol in
  let module A = Agent_session in
  let open Result.Let_syntax in
  let fail = script_selection_failure in
  let poll () =
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ()
  in
  let registry = Daemon.registry host.daemon in
  let same_source (state : A.Session_state.t) =
    let current = A.Session_state.summary state in
    Option.is_some state.spec.prompt_definition_id
    && P.Id.Session.equal current.id started.P.Session.id
    && Int.equal current.generation started.generation
    && Option.equal
         P.Id.Prompt_revision.equal
         current.prompt_revision
         started.prompt_revision
    && Option.equal
         P.Id.Workspace_instance.equal
         current.workspace_instance
         started.workspace_instance
    && String.equal
         (P.Session.Spec.to_json current.spec |> Jsonaf.to_string)
         (P.Session.Spec.to_json started.spec |> Jsonaf.to_string)
    && Option.is_none state.spec.delegation
  in
  let check_open () =
    if
      Embedded_host_cleanup.is_closing host.cleanup
      || Session_registry.is_closing registry
    then fail Server_shutting_down "embedded activation owner is closing"
    else Ok ()
  in
  poll ();
  let%bind () = check_open () in
  let%bind entry =
    Session_registry.find registry started.id
    |> Result.of_option
         ~error:
           (P.Error.create
              Conflict
              ~message:"selected activation owner is not loaded"
              ~retryable:false
              ())
  in
  let%bind () = A.Session_actor.authorize_writer entry.actor ~attachment_id in
  let%bind before = A.Session_actor.state entry.actor in
  let%bind () = authorize_script_selection host (A.Session_state.summary before) in
  let same_configuration (state : A.Session_state.t) =
    same_source state
    && Int64.equal before.spec.configuration_revision state.spec.configuration_revision
    && Inference.Selection.equal before.spec.inference_target state.spec.inference_target
    && Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.equal
         before.spec.root_invocation
         state.spec.root_invocation
    && Option.equal
         P.Id.Prompt_definition.equal
         before.spec.prompt_definition_id
         state.spec.prompt_definition_id
    && String.equal before.spec.permission_profile state.spec.permission_profile
    && Option.equal String.equal before.spec.runtime_policy state.spec.runtime_policy
    && Option.equal
         (fun left right -> A.Quota_key.compare left right = 0)
         before.spec.quota_key
         state.spec.quota_key
    && A.Workspace_instance.equal_canonical_identity
         before.spec.workspace_instance.canonical_root
         state.spec.workspace_instance.canonical_root
    && A.Workspace_definition.equal_access
         before.spec.workspace_instance.access
         state.spec.workspace_instance.access
    && String.equal
         before.spec.permission_profile_digest
         state.spec.permission_profile_digest
    && Int64.equal
         before.counters.owner_lease_generation
         state.counters.owner_lease_generation
  in
  let%bind () =
    if
      same_source before
      && Runtime_owner.is_loaded entry.runtime
      && P.Session.equal_desired_state started.desired_state Running
      && P.Session.equal_desired_state before.lifecycle.desired Running
      && Option.is_none before.active_operation
      && Option.is_none before.failure
      && not before.halted
    then Ok ()
    else fail Conflict "selected activation source or lifecycle changed"
  in
  let%bind () =
    Runtime_owner.with_background_runtime entry.runtime (fun runtime ->
      poll ();
      let%bind () = A.Session_actor.authorize_writer entry.actor ~attachment_id in
      let%bind current = A.Session_actor.state entry.actor in
      let%bind () = authorize_script_selection host (A.Session_state.summary current) in
      let%bind () =
        if
          same_configuration current
          && P.Session.equal_desired_state current.lifecycle.desired Running
          && Option.is_none current.active_operation
          && Option.is_none current.failure
          && not current.halted
        then Ok ()
        else fail Conflict "activation admission changed"
      in
      match runtime.A.Runtime_builder.moderator_activation with
      | None -> fail Invalid_state "selected runtime has no authored lifecycle activation"
      | Some activation -> Result.map (activation.run ()) ~f:ignore)
  in
  poll ();
  let%bind () = check_open () in
  let%bind () =
    match Session_registry.find registry started.id with
    | Some current when phys_equal current entry -> Ok ()
    | Some _ | None -> fail Conflict "activation runtime owner changed"
  in
  let%bind () = A.Session_actor.authorize_writer entry.actor ~attachment_id in
  let%bind after = A.Session_actor.state entry.actor in
  let summary = A.Session_state.summary after in
  let%bind () = authorize_script_selection host summary in
  let%bind () =
    if same_configuration after
    then Ok ()
    else fail Conflict "activation source, configuration or writer changed"
  in
  match
    ( summary.desired_state
    , summary.observed_state
    , after.active_operation
    , after.failure
    , after.halted )
  with
  | Running, Idle, None, None, false -> Ok summary
  | _ -> fail Conflict "activated session is not available for user input"
;;

let connect_host host =
  if Embedded_host_cleanup.is_closing host.cleanup
  then
    Error
      (Agent_protocol.Error.create
         Interrupted
         ~message:"embedded host is closed"
         ~retryable:false
         ())
  else
    Ok
      (make_connection
         host.daemon
         host.principal
         host.event_capacity
         ~max_attachments:host.max_attachments)
;;

let open_local_host
      ~sw
      ~env
      ?(daemon_options = Daemon.default_options)
      ?root_invocation
      ?project_sources
      ?(authoring_package_files = [])
      ?authoring_budget
      options
  =
  Mirage_crypto_rng_unix.use_default ();
  let open Result.Let_syntax in
  let%bind authoring_packages =
    Chat_response.Authoring_package_file.load_many ~env ~paths:authoring_package_files
    |> Result.map_error ~f:Agent_protocol.Error.invalid_request
  in
  let%bind data_root, temporary_root = data_root env options in
  let config = config ~project_sources options data_root in
  let config =
    { config with server = { config.server with authoring_packages; authoring_budget } }
  in
  open_owned_host
    ~sw
    ~env
    ~daemon_options:
      { daemon_options with
        startup_mode = On_demand
      ; root_invocation =
          Option.value root_invocation ~default:daemon_options.root_invocation
      ; extension_host =
          (match options.storage with
           | Transient -> Embedded_transient
           | Default | Durable _ -> Embedded_durable)
      }
    ~config
    ~tool_dir:options.tool_dir
    ~home:options.home
    ~event_capacity:options.event_capacity
    ~temporary_root
;;

let with_local_host
      ~sw
      ~env
      ?daemon_options
      ?root_invocation
      ?project_sources
      ?authoring_package_files
      ?authoring_budget
      options
      ~f
  =
  let open Result.Let_syntax in
  let%bind host =
    open_local_host
      ~sw
      ~env
      ?daemon_options
      ?root_invocation
      ?project_sources
      ?authoring_package_files
      ?authoring_budget
      options
  in
  let%bind result = Embedded_host_cleanup.protect host.cleanup (fun () -> f host) in
  close_host host;
  Ok result
;;

let start
      ~sw
      ~env
      ?daemon_options
      ?root_invocation
      ?project_sources
      ?authoring_package_files
      ?authoring_budget
      options
  =
  let open Result.Let_syntax in
  let%bind host =
    open_local_host
      ~sw
      ~env
      ?daemon_options
      ?root_invocation
      ?project_sources
      ?authoring_package_files
      ?authoring_budget
      options
  in
  Embedded_host_cleanup.protect host.cleanup (fun () ->
    let%bind session_id = create_session host.connection options in
    let%map attachment =
      attach host.connection ~mode:options.attachment_mode session_id
    in
    { host; session_id; attachment })
;;

let connection t = host_connection t.host
let session_id t = t.session_id
let attachment t = t.attachment
let dispatcher t = host_dispatcher t.host
let principal t = host_principal t.host

let connect t =
  match connect_host t.host with
  | Ok connection -> connection
  | Error failure -> failwith failure.Agent_protocol.Error.message
;;

let close_connection t = Daemon.close_connection t.host.daemon
let close t = close_host t.host

let connect_owned t =
  Embedded_host_cleanup.adopt_additional_connection t.host.cleanup ~create:(fun () ->
    make_connection_owner
      t.host.daemon
      t.host.principal
      t.host.event_capacity
      ~max_attachments:t.host.max_attachments)
;;

let with_session t ~f =
  Embedded_host_cleanup.protect t.host.cleanup (fun () ->
    let%bind.Result value = f t in
    close t;
    Ok value)
;;

let select_session host expected =
  if Embedded_host_cleanup.is_closing host.cleanup
  then
    Error
      (Agent_protocol.Error.create
         Server_shutting_down
         ~message:"embedded host is closing"
         ~retryable:false
         ())
  else
    Daemon.select_session host.daemon ~principal:host.principal ~expected
    |> Result.map ~f:(fun (_ : Session_registry.entry) -> ())
;;

let attach_retained host ~mode ~expected =
  let open Result.Let_syntax in
  let%bind () = select_session host expected in
  let session_id =
    Agent_protocol.Session_lifecycle.Expected.reference expected
    |> Agent_protocol.Session_ref.session_id
  in
  let%map attachment = attach host.connection ~mode session_id in
  { host; session_id; attachment }
;;
