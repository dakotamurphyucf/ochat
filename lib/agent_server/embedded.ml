open! Core

type options =
  { prompt_file : string
  ; workspace : string
  ; tool_dir : string
  ; home : string
  ; data_root : string option
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
  ; mutable closed : bool
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
  match options.data_root with
  | Some path when Filename.is_absolute path -> Ok (path, None)
  | Some _ ->
    Error (Agent_protocol.Error.invalid_request "embedded data root must be absolute")
  | None -> Result.map (create_temporary_root env) ~f:(fun path -> path, Some path)
;;

let server_config data_root =
  Config.Server.
    { data_dir = data_root
    ; session_helpers = []
    ; authoring_packages = []
    ; authoring_budget = None
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

let config options data_root =
  Config.
    { version = current_version
    ; source_file = Filename.concat data_root "embedded-server.sexp"
    ; server = server_config data_root
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

let make_connection daemon principal event_capacity ~max_attachments =
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
  Agent_client.In_memory.create
    ~request:(fun command ->
      Dispatcher.dispatch_command (Daemon.dispatcher daemon) ~context command)
    ~notifications
    ~close:(fun () ->
      ignore (Eio.Promise.try_resolve close_resolver ());
      Daemon.close_connection daemon context)
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

let create_spec options =
  Agent_protocol.Session.Spec.create
    ~execution_host:Embedded
    ~prompt:(Catalog (Catalog_identity.prompt_definition "embedded.prompt"))
    ~workspace:(Configured (Catalog_identity.workspace_definition "embedded.workspace"))
    ~liveness:Process_bound
    ~persistence:(if Option.is_some options.data_root then Durable else Transient)
    ~permission_profile:options.permission_profile.id
    ~start_immediately:options.start_immediately
    ~labels:[]
    ()
;;

let create_session connection options =
  let open Result.Let_syntax in
  let%bind spec = create_spec options in
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

let attach connection options session_id =
  let open Result.Let_syntax in
  let%bind idempotency_key =
    Agent_protocol.Idempotency_key.of_string
      (Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string)
  in
  let request =
    Agent_protocol.Session.Attach_request.
      { session_id
      ; requested_mode = options.attachment_mode
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
  let before_activation store =
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
      ~home
      ~before_activation
      ~process_start_identity:None
      ()
  in
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
    let connection = make_connection daemon principal event_capacity ~max_attachments in
    match initialize connection with
    | Error failure ->
      Agent_client.Connection.close connection;
      Error failure
    | Ok () ->
      Ok
        { daemon
        ; connection
        ; principal
        ; event_capacity
        ; max_attachments
        ; temporary_root
        ; env
        ; closed = false
        }
  in
  match finish () with
  | Ok _ as result -> result
  | Error failure ->
    ignore (Daemon.shutdown daemon : (unit, Agent_protocol.Error.t) result);
    cleanup_root env temporary_root;
    Error failure
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    Eio.Cancel.protect (fun () ->
      ignore (Daemon.shutdown daemon : (unit, Agent_protocol.Error.t) result);
      cleanup_root env temporary_root);
    Exn.raise_with_original_backtrace exn backtrace
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
    ~home
    ~event_capacity
    ~temporary_root:None
;;

let close_host host =
  Eio.Cancel.protect (fun () ->
    if not host.closed
    then (
      host.closed <- true;
      Agent_client.Connection.close host.connection;
      ignore (Daemon.shutdown host.daemon : (unit, Agent_protocol.Error.t) result);
      cleanup_root host.env host.temporary_root))
;;

let host_connection host = host.connection
let host_principal host = host.principal
let host_dispatcher host = Daemon.dispatcher host.daemon

let connect_host host =
  if host.closed
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

let start
      ~sw
      ~env
      ?(daemon_options = Daemon.default_options)
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
  let config = config options data_root in
  let config =
    { config with server = { config.server with authoring_packages; authoring_budget } }
  in
  let host_result =
    open_owned_host
      ~sw
      ~env
      ~daemon_options:
        { daemon_options with
          startup_mode = Execute
        ; extension_host =
            (if Option.is_some options.data_root
             then Embedded_durable
             else Embedded_transient)
        }
      ~config
      ~tool_dir:options.tool_dir
      ~home:options.home
      ~event_capacity:options.event_capacity
      ~temporary_root
  in
  match host_result with
  | Error failure ->
    cleanup_root env temporary_root;
    Error failure
  | Ok host ->
    let retained = ref false in
    Exn.protect
      ~finally:(fun () -> if not !retained then close_host host)
      ~f:(fun () ->
        let%bind session_id = create_session host.connection options in
        let%map attachment = attach host.connection options session_id in
        retained := true;
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
