open Core
module Config_fixture = Support.Config_fixture
module Daemon_host = Support.Daemon_host
module Daemon_process = Support.Daemon_process
module Port_reservation = Support.Port_reservation
module Process_manager = Support.Process_manager
module Stdio_client = Support.Stdio_client
module Stdio_process = Support.Stdio_process
module Temporary_environment = Support.Temporary_environment
module Unix_driver = Support.Unix_driver

type client =
  { request :
      Agent_protocol.Command.t
      -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result
  ; next_notification : unit -> Agent_protocol.Envelope.t option
  ; close : unit -> unit
  }

type read_observation =
  { protocol_name : string
  ; protocol_version : Agent_protocol.Version.t
  ; ping_ready : bool
  ; ping_draining : bool
  ; server_info : string
  ; health_ready : bool
  ; health_draining : bool
  ; prompt_list : string
  ; prompt_get : string
  ; workspace_list : string
  ; workspace_get : string
  }
[@@deriving equal, sexp]

type prompt_observation =
  { name : string
  ; description : string option
  ; enabled : bool
  ; availability : Agent_protocol.Prompt.availability
  ; has_current_revision : bool
  ; allowed_workspace_count : int
  ; permission_profile : string
  ; runtime_policy : string option
  }
[@@deriving equal, sexp]

type workspace_observation =
  { name : string
  ; kind : Agent_protocol.Workspace.kind
  ; temporary_location : Agent_protocol.Workspace.temporary_location option
  ; cleanup : Agent_protocol.Workspace.cleanup option
  ; access : Agent_protocol.Workspace.access
  ; conflict_domain : string option
  ; prompt_limit_count : int
  ; availability : Agent_protocol.Workspace.availability
  }
[@@deriving equal, sexp]

type portable_read_observation =
  { protocol_name : string
  ; protocol_version : Agent_protocol.Version.t
  ; implementation : string
  ; limits : string
  ; ping_ready : bool
  ; ping_draining : bool
  ; server_features : string list
  ; server_transports : string list
  ; unsafe_development_auth : bool
  ; health_status : Agent_protocol.Health.status
  ; health_ready : bool
  ; health_draining : bool
  ; prompts : prompt_observation list
  ; workspaces : workspace_observation list
  }
[@@deriving equal, sexp]

type lifecycle_observation =
  { duplicate_create_replayed : bool
  ; session_list_contains_created : bool
  ; session_get_matches_created : bool
  ; create_revision : int64
  ; create_sequence : int64
  ; start_revision_delta : int64
  ; start_sequence_delta : int64
  ; start_desired_state : Agent_protocol.Session.desired_state
  ; stop_revision_delta : int64
  ; stop_sequence_delta : int64
  ; stop_desired_state : Agent_protocol.Session.desired_state
  ; replay_sequences_are_contiguous : bool
  ; replay_kinds : Agent_protocol.Event.Durable.kind list
  ; replay_visibilities : Agent_protocol.Event.Durable.visibility list
  ; detach_revision_delta : int64
  ; detach_sequence_delta : int64
  }
[@@deriving equal, sexp]

type security_observation =
  { pending_permission_count : int
  ; active_grant_count : int
  ; audit_nonempty : bool
  ; missing_permission_error : Agent_protocol.Error.code
  ; missing_grant_error : Agent_protocol.Error.code
  }
[@@deriving equal, sexp]

type jobs_schedules_observation =
  { initial_job_count : int
  ; missing_job_get_error : Agent_protocol.Error.code
  ; missing_job_cancel_error : Agent_protocol.Error.code
  ; duplicate_schedule_replayed : bool
  ; created_schedule_status : string
  ; fetched_schedule_status : string
  ; scheduled_count : int
  ; cancelled_schedule_status : string
  ; cancel_revision_delta : int64
  ; cancel_sequence_delta : int64
  ; cancelled_count : int
  }
[@@deriving equal, sexp]

type blob_observation =
  { kind : Agent_protocol.Blob.kind
  ; media_type : string
  ; display_name : string option
  ; nonempty : bool
  ; byte_length_matches : bool
  ; digest_matches : bool
  ; valid_json : bool
  ; top_level_fields : string list
  ; chunk_offsets_are_contiguous : bool
  ; missing_blob_error : Agent_protocol.Error.code
  }
[@@deriving equal, sexp]

type error_observation =
  { name : string
  ; code : Agent_protocol.Error.code
  ; retryable : bool
  }
[@@deriving equal, sexp]

type event_observation =
  { relative_sequence : int64
  ; relative_revision : int64
  ; kind : Agent_protocol.Event.Durable.kind
  ; visibility : Agent_protocol.Event.Durable.visibility
  }
[@@deriving equal, sexp]

type event_order_observation =
  { live : event_observation list
  ; replay : event_observation list
  }
[@@deriving equal, sexp]

type visibility_observation =
  { writer : event_observation list
  ; reader : event_observation list
  ; reader_mutation_error : Agent_protocol.Error.code
  }
[@@deriving equal, sexp]

type history_deletion_observation =
  { reader_error : Agent_protocol.Error.code
  ; stale_error : Agent_protocol.Error.code
  ; remaining_count : int
  ; revision_delta : int64
  ; sequence_delta : int64
  ; events : event_observation list
  }
[@@deriving equal, sexp]

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
;;

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Port_reservation.create ~sw ~env in
    let port = Port_reservation.port reservation in
    Port_reservation.release reservation;
    port)
;;

let fixture env environment name =
  Config_fixture.create environment ~name ~http_port:(reserve_port env)
;;

let readiness_failure daemon error =
  raise_s
    [%sexp
      "daemon did not become ready"
    , { error : Daemon_process.readiness_error
      ; stdout = ((Daemon_process.stdout daemon).contents : string)
      ; stderr = ((Daemon_process.stderr daemon).contents : string)
      }]
;;

let stop_daemon env daemon =
  match Daemon_process.result daemon with
  | Some _ -> ()
  | None -> ignore (Daemon_process.stop daemon ~env ~grace_seconds:1.)
;;

let with_daemon ~sw env fixture f =
  let daemon =
    Daemon_process.start
      ~sw
      ~env
      ~fixture
      ~config_path:(Config_fixture.config_path fixture)
  in
  let health =
    match Daemon_process.wait_ready daemon ~env ~timeout_seconds:5. with
    | Ok health -> health
    | Error error -> readiness_failure daemon error
  in
  Exn.protect ~f:(fun () -> f daemon health) ~finally:(fun () -> stop_daemon env daemon)
;;

let request_public client command = client.request command |> protocol_ok

let request client command =
  request_public client command |> Support.Public_view.non_history
;;

let request_error client command =
  match client.request command with
  | Error error -> error
  | Ok result ->
    raise_s
      [%sexp
        "protocol operation unexpectedly succeeded"
      , (result : Agent_protocol.Public.Result.t)]
;;

let initialize ?(features = []) client =
  let implementation =
    Agent_protocol.Initialize.Implementation.create
      ~name:"agent-server-e2e-conformance"
      ~version:"dev"
    |> protocol_ok
  in
  let initialize_request =
    Agent_protocol.Initialize.Request.create
      ~implementation
      ~protocol_min:Agent_protocol.Version.current
      ~protocol_max:Agent_protocol.Version.current
      ~features
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(16 * 1024 * 1024)
      ()
    |> protocol_ok
  in
  match request client (Protocol_initialize initialize_request) with
  | Protocol_initialize initialized -> initialized
  | _ -> fail "protocol.initialize returned the wrong result variant"
;;

let client_of_connection connection =
  { request = Agent_client.Connection.request connection
  ; next_notification = (fun () -> Agent_client.Connection.next_notification connection)
  ; close = (fun () -> Agent_client.Connection.close connection)
  }
;;

let idempotency_key value = Agent_protocol.Idempotency_key.of_string value |> protocol_ok
let encode_result result = Agent_protocol.Method_result.to_json result |> Jsonaf.to_string
let page_request () = Agent_protocol.Page.Request.create ~limit:100 () |> protocol_ok

let prompt_request () =
  Agent_protocol.Prompt.List_request.
    { page = page_request (); enabled = Some true; available = Some true }
;;

let workspace_request () =
  Agent_protocol.Workspace.List_request.
    { page = page_request (); kind = None; access = None; available = Some true }
;;

let catalog_ids connection =
  let prompt =
    match request connection (Prompt_list (prompt_request ())) with
    | Prompt_list page -> (List.hd_exn page.items).id
    | _ -> fail "prompt.list returned the wrong result variant"
  in
  let workspace =
    match request connection (Workspace_list (workspace_request ())) with
    | Workspace_list page -> (List.hd_exn page.items).id
    | _ -> fail "workspace.list returned the wrong result variant"
  in
  prompt, workspace
;;

let session_spec connection =
  let prompt, workspace = catalog_ids connection in
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog prompt)
    ~workspace:(Configured workspace)
    ~liveness:Detached
    ~persistence:Durable
    ~permission_profile:"unattended"
    ~start_immediately:false
    ~labels:[ "suite", "conformance" ]
    ()
  |> protocol_ok
;;

let observe_ping connection =
  match request connection (Protocol_ping { payload = None }) with
  | Protocol_ping ping -> ping.ready, ping.draining
  | _ -> fail "protocol.ping returned the wrong result variant"
;;

let observe_health connection =
  match request connection (Server_health { include_details = true }) with
  | Server_health health -> health.ready, health.draining
  | _ -> fail "server.health returned the wrong result variant"
;;

let observe_prompts connection =
  match request connection (Prompt_list (prompt_request ())) with
  | Prompt_list page as result ->
    let prompt = List.hd_exn page.items in
    let get = request connection (Prompt_get { prompt_id = prompt.id }) in
    encode_result result, encode_result get
  | _ -> fail "prompt.list returned the wrong result variant"
;;

let observe_workspaces connection =
  match request connection (Workspace_list (workspace_request ())) with
  | Workspace_list page as result ->
    let workspace = List.hd_exn page.items in
    let get = request connection (Workspace_get { workspace_id = workspace.id }) in
    encode_result result, encode_result get
  | _ -> fail "workspace.list returned the wrong result variant"
;;

let observe connection =
  let initialized = initialize connection in
  let ping_ready, ping_draining = observe_ping connection in
  let server_info = request connection Server_info |> encode_result in
  let health_ready, health_draining = observe_health connection in
  let prompt_list, prompt_get = observe_prompts connection in
  let workspace_list, workspace_get = observe_workspaces connection in
  { protocol_name = initialized.protocol_name
  ; protocol_version = initialized.selected_version
  ; ping_ready
  ; ping_draining
  ; server_info
  ; health_ready
  ; health_draining
  ; prompt_list
  ; prompt_get
  ; workspace_list
  ; workspace_get
  }
;;

let prompt_observation (prompt : Agent_protocol.Prompt.t) =
  { name = prompt.name
  ; description = prompt.description
  ; enabled = prompt.enabled
  ; availability = prompt.availability
  ; has_current_revision = Option.is_some prompt.current_revision
  ; allowed_workspace_count = List.length prompt.allowed_workspaces
  ; permission_profile = prompt.permission_profile
  ; runtime_policy = prompt.runtime_policy
  }
;;

let workspace_observation (workspace : Agent_protocol.Workspace.t) =
  { name = workspace.name
  ; kind = workspace.kind
  ; temporary_location = workspace.temporary_location
  ; cleanup = workspace.cleanup
  ; access = workspace.access
  ; conflict_domain = workspace.conflict_domain
  ; prompt_limit_count = List.length workspace.prompt_limits
  ; availability = workspace.availability
  }
;;

let portable_catalog connection =
  let prompts =
    match request connection (Prompt_list (prompt_request ())) with
    | Prompt_list page -> List.map page.items ~f:prompt_observation
    | _ -> fail "prompt.list returned the wrong result variant"
  in
  let workspaces =
    match request connection (Workspace_list (workspace_request ())) with
    | Workspace_list page -> List.map page.items ~f:workspace_observation
    | _ -> fail "workspace.list returned the wrong result variant"
  in
  prompts, workspaces
;;

let portable_observe connection =
  let initialized = initialize connection in
  let ping_ready, ping_draining = observe_ping connection in
  let server_info =
    match request connection Server_info with
    | Server_info info -> info
    | _ -> fail "server.info returned the wrong result variant"
  in
  let health =
    match request connection (Server_health { include_details = false }) with
    | Server_health health -> health
    | _ -> fail "server.health returned the wrong result variant"
  in
  let prompts, workspaces = portable_catalog connection in
  { protocol_name = initialized.protocol_name
  ; protocol_version = initialized.selected_version
  ; implementation =
      Sexp.to_string_mach
        ([%sexp_of: Agent_protocol.Initialize.Implementation.t]
           initialized.implementation)
  ; limits =
      Sexp.to_string_mach
        ([%sexp_of: Agent_protocol.Initialize.Limits.t] initialized.limits)
  ; ping_ready
  ; ping_draining
  ; server_features = server_info.features
  ; server_transports = server_info.transports
  ; unsafe_development_auth = server_info.unsafe_development_auth
  ; health_status = health.status
  ; health_ready = health.ready
  ; health_draining = health.draining
  ; prompts
  ; workspaces
  }
;;

let require_equal_portable_read expected actual =
  if not (equal_portable_read_observation expected actual)
  then
    raise_s
      [%sexp
        "portable read observations differ"
      , { expected : portable_read_observation; actual : portable_read_observation }]
;;

let http_client ~sw env fixture =
  Agent_transport_http.Client.connect
    ~sw
    ~env
    ~uri:
      (Uri.of_string (sprintf "http://127.0.0.1:%d" (Config_fixture.http_port fixture)))
    ~bearer_token:(Some (Config_fixture.admin_token fixture))
    ~notification_capacity:1_024
  |> protocol_ok
  |> client_of_connection
;;

let unix_client ~sw env fixture =
  Unix_driver.connect ~sw ~env ~socket_path:(Config_fixture.unix_socket fixture)
  |> client_of_connection
;;

let absolute env path =
  if Filename.is_absolute path
  then path
  else Filename.concat (Eio.Path.native_exn (Eio.Stdenv.cwd env)) path
;;

let stdio_executable env =
  let fallback =
    Filename.concat
      (Eio.Path.native_exn (Eio.Stdenv.cwd env))
      "_build/default/bin/ochat_agent_stdio.exe"
  in
  let candidate = Sys.getenv "OCHAT_E2E_STDIO_EXE" |> Option.value ~default:fallback in
  let candidate = absolute env candidate in
  if Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / candidate)
  then candidate
  else raise_s [%sexp "ochat-agent-stdio executable is unavailable", (candidate : string)]
;;

let child_environment fixture =
  Temporary_environment.child_environment
    (Config_fixture.environment fixture)
    ~base:(Core_unix.environment ())
;;

let spawn_stdio ~sw env fixture arguments =
  Stdio_process.spawn
    ~sw
    ~env
    ~environment:(child_environment fixture)
    ~max_output_bytes:(2 * 1024 * 1024)
    (stdio_executable env :: arguments)
;;

let stop_stdio env process =
  Stdio_process.close_stdin process;
  match
    Eio.Time.with_timeout (Eio.Stdenv.clock env) 3. (fun () ->
      Ok (Stdio_process.await process))
  with
  | Ok _ -> ()
  | Error `Timeout ->
    ignore
      (Stdio_process.terminate process ~clock:(Eio.Stdenv.clock env) ~grace_seconds:1.
       : Process_manager.termination)
;;

let stdio_protocol_client process env =
  let stdio = Stdio_client.create ~process ~clock:(Eio.Stdenv.clock env) in
  let pending = Queue.create () in
  let request command =
    Stdio_client.request stdio command
    |> Result.map ~f:(fun response ->
      Queue.enqueue_all pending response.notifications;
      response.result)
  in
  let next_notification () =
    match Queue.dequeue pending with
    | Some notification -> Some notification
    | None ->
      Stdio_client.next_envelope stdio ~timeout_seconds:5. |> protocol_ok |> Option.some
  in
  { request; next_notification; close = (fun () -> Stdio_process.close_stdin process) }
;;

let with_stdio_client ~sw env fixture arguments f =
  let process = spawn_stdio ~sw env fixture arguments in
  let client = stdio_protocol_client process env in
  Exn.protect ~f:(fun () -> f client) ~finally:(fun () -> stop_stdio env process)
;;

let write_http_token environment fixture name =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let path = Filename.concat roots.config name in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment path)
    (Config_fixture.admin_token fixture ^ "\n");
  path
;;

let stdio_gateway_arguments environment fixture = function
  | `Unix -> [ "--connect"; "unix://" ^ Config_fixture.unix_socket fixture ]
  | `Http name ->
    let token_path = write_http_token environment fixture name in
    [ "--connect"
    ; sprintf "http://127.0.0.1:%d" (Config_fixture.http_port fixture)
    ; "--bearer-token-file"
    ; token_path
    ]
;;

let local_stdio_arguments fixture data_root =
  [ "--local"
  ; "--prompt"
  ; Config_fixture.prompt_path fixture
  ; "--workspace"
  ; Config_fixture.physical_workspace fixture
  ; "--data-root"
  ; data_root
  ]
;;

let embedded_options environment fixture data_root =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  Agent_server.Embedded.
    { prompt_file = Config_fixture.prompt_path fixture
    ; workspace = Config_fixture.physical_workspace fixture
    ; tool_dir = Config_fixture.physical_workspace fixture
    ; home = roots.home
    ; data_root = Some data_root
    ; start_immediately = false
    ; permission_profile = default_permission_profile
    ; attachment_mode = Read_write
    ; event_capacity = 1_024
    }
;;

let direct_embedded_observation env environment fixture =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let data_root = Filename.concat roots.data "conformance-embedded-direct" in
  Support.Provider_fixture.provision ~env fixture;
  Eio.Switch.run (fun sw ->
    let module Platform = Inference_composition.Provider_platform in
    let platform =
      Platform.create
        ~sw
        ~env
        ~home:roots.home
        ~api_url:None
        ~lookup:(fun _ -> None)
        ~default_model:"gpt-4.1"
        ~namespace:(Platform.new_namespace env)
        ~callback_port:1455
        ()
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum (Agent_protocol.Provider_operator.Error.sexp_of_t error))
      |> Result.ok_or_failwith
    in
    let daemon_options =
      { (Inference_composition.daemon_options (Platform.host platform)) with
        provider_operator_factory = Some (Platform.factory platform)
      }
    in
    let embedded =
      Agent_server.Embedded.start
        ~daemon_options
        ~sw
        ~env
        (embedded_options environment fixture data_root)
      |> protocol_ok
    in
    let client = Agent_server.Embedded.connect embedded |> client_of_connection in
    Exn.protect
      ~f:(fun () -> portable_observe client)
      ~finally:(fun () ->
        client.close ();
        Agent_server.Embedded.close embedded))
;;

let local_stdio_observation env environment fixture =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let data_root = Filename.concat roots.data "conformance-embedded-stdio" in
  Eio.Switch.run (fun sw ->
    with_stdio_client
      ~sw
      env
      fixture
      (local_stdio_arguments fixture data_root)
      portable_observe)
;;

let require_equal unix http =
  if not (equal_read_observation unix http)
  then
    raise_s
      [%sexp
        "cross-transport read observations differ"
      , { unix : read_observation; http : read_observation }]
;;

let command_receipt connection command =
  match
    request
      connection
      (Command_receipt
         { method_name = Agent_protocol.Command.method_name command
         ; original_params = Agent_protocol.Command.params command
         })
  with
  | Command_receipt receipt -> receipt
  | _ -> fail "command.receipt returned the wrong result variant"
;;

let create_session connection ~key =
  let create_request =
    Agent_protocol.Session.Create_request.
      { spec = session_spec connection
      ; requested_mode = Some Read_write
      ; subscribe = false
      ; idempotency_key = idempotency_key key
      }
  in
  let create () =
    match request_public connection (Session_create create_request) with
    | Session_create created -> created
    | _ -> fail "session.create returned the wrong result variant"
  in
  (match command_receipt connection (Session_create create_request) with
   | Missing -> ()
   | _ -> fail "unsubmitted create receipt must remain unresolved/missing");
  let created = create () in
  (match command_receipt connection (Session_create create_request) with
   | Committed (Created_session session_id)
     when Agent_protocol.Id.Session.equal session_id created.session.id -> ()
   | _ -> fail "create receipt must disclose only the committed session identity");
  created, create ()
;;

let start_session connection session attachment ~key =
  let start_request =
    Agent_protocol.Session.Start_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; queue_if_limited = false
      ; idempotency_key = idempotency_key key
      }
  in
  match request connection (Session_start start_request) with
  | Session_start mutation -> mutation
  | _ -> fail "session.start returned the wrong result variant"
;;

let stop_session connection session attachment ~key =
  let stop_request =
    Agent_protocol.Session.Stop_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; mode = Graceful
      ; idempotency_key = idempotency_key key
      }
  in
  match request connection (Session_stop stop_request) with
  | Session_stop mutation -> mutation
  | _ -> fail "session.stop returned the wrong result variant"
;;

let attach_replay connection session_id ~key =
  let attach_request =
    Agent_protocol.Session.Attach_request.
      { session_id
      ; requested_mode = Read_only
      ; subscribe = false
      ; after_sequence = Some 0L
      ; reclaim_token = None
      ; idempotency_key = idempotency_key key
      }
  in
  match request_public connection (Session_attach attach_request) with
  | Session_attach ({ replay = Events _; _ } as attached) ->
    (match command_receipt connection (Session_attach attach_request) with
     | Committed (Attached_session recovered)
       when Agent_protocol.Id.Session.equal recovered session_id -> ()
     | _ -> fail "attach receipt must require fresh authorized reattachment");
    attached
  | Session_attach { replay = Current; _ } -> fail "session replay returned current"
  | Session_attach { replay = Snapshot _; _ } -> fail "session replay returned snapshot"
  | _ -> fail "session.attach returned the wrong result variant"
;;

let replay_events connection session_id ~key =
  match (attach_replay connection session_id ~key).replay with
  | Events events -> events
  | Current | Snapshot _ -> assert false
;;

let list_contains_session connection session_id =
  let list_request =
    Agent_protocol.Session.List_request.
      { organization = Agent_protocol.Session_organization.Query.default
      ; page = page_request ()
      ; desired_state = None
      ; prompt_id = None
      ; workspace_id = None
      ; owner_principal_id = None
      ; creator_principal_id = None
      ; active_owner_principal_id = None
      ; sort = Agent_protocol.Session_catalog_query.Sort.default
      ; archive = Active
      ; labels = []
      }
  in
  match request connection (Session_list list_request) with
  | Session_list page ->
    List.exists page.items ~f:(fun session ->
      Agent_protocol.Id.Session.compare
        session.Agent_protocol.Session_catalog.session.id
        session_id
      = 0)
  | _ -> fail "session.list returned the wrong result variant"
;;

let get_matches_session connection session_id =
  match request_public connection (Session_get { session_id; history = None }) with
  | Session_get snapshot ->
    Agent_protocol.Id.Session.compare
      (Agent_protocol.Public.Snapshot.fields snapshot).session.id
      session_id
    = 0
  | _ -> fail "session.get returned the wrong result variant"
;;

let detach connection session_id attachment_id ~key =
  let detach_request =
    Agent_protocol.Session.Detach_request.
      { session_id; attachment_id; idempotency_key = idempotency_key key }
  in
  match request connection (Session_detach detach_request) with
  | Session_detach mutation -> mutation
  | _ -> fail "session.detach returned the wrong result variant"
;;

let sequences_are_contiguous events =
  List.for_alli events ~f:(fun index event ->
    Int64.equal event.Agent_protocol.Public.Durable.sequence (Int64.of_int (index + 1)))
;;

let lifecycle_observation connection ~key_prefix =
  ignore (initialize connection : Agent_protocol.Initialize.Response.t);
  let created, duplicate = create_session connection ~key:(key_prefix ^ ":create") in
  let attachment =
    Option.value_exn created.attachment
    |> fun (attached : Agent_protocol.Public.Result.Attach.t) -> attached.attachment
  in
  let started =
    start_session connection created.session attachment ~key:(key_prefix ^ ":start")
  in
  let stopped =
    stop_session connection started.session attachment ~key:(key_prefix ^ ":stop")
  in
  let replay =
    attach_replay connection created.session.id ~key:(key_prefix ^ ":replay")
  in
  let events =
    match replay.replay with
    | Events events -> events
    | Current | Snapshot _ -> assert false
  in
  let detached =
    detach connection created.session.id replay.attachment.id ~key:(key_prefix ^ ":detach")
  in
  { duplicate_create_replayed =
      Agent_protocol.Id.Session.compare created.session.id duplicate.session.id = 0
  ; session_list_contains_created = list_contains_session connection created.session.id
  ; session_get_matches_created = get_matches_session connection created.session.id
  ; create_revision = created.mutation.revision
  ; create_sequence = created.mutation.latest_event_sequence
  ; start_revision_delta = Int64.(started.mutation.revision - created.mutation.revision)
  ; start_sequence_delta =
      Int64.(
        started.mutation.latest_event_sequence - created.mutation.latest_event_sequence)
  ; start_desired_state = started.session.desired_state
  ; stop_revision_delta = Int64.(stopped.mutation.revision - started.mutation.revision)
  ; stop_sequence_delta =
      Int64.(
        stopped.mutation.latest_event_sequence - started.mutation.latest_event_sequence)
  ; stop_desired_state = stopped.session.desired_state
  ; replay_sequences_are_contiguous = sequences_are_contiguous events
  ; replay_kinds = List.map events ~f:(fun event -> event.kind)
  ; replay_visibilities = List.map events ~f:Support.Public_view.visibility
  ; detach_revision_delta = Int64.(detached.revision - stopped.mutation.revision)
  ; detach_sequence_delta =
      Int64.(detached.latest_event_sequence - stopped.mutation.latest_event_sequence)
  }
;;

let require_lifecycle_invariants observation =
  if not observation.duplicate_create_replayed
  then fail "duplicate create was not replayed";
  if not observation.session_list_contains_created
  then fail "session.list omitted the created session";
  if not observation.session_get_matches_created
  then fail "session.get returned a different session";
  if not observation.replay_sequences_are_contiguous
  then fail "lifecycle replay event sequences were not contiguous";
  if
    not
      (Agent_protocol.Session.equal_desired_state observation.start_desired_state Running)
  then fail "session.start did not set desired state to running";
  if
    not
      (Agent_protocol.Session.equal_desired_state observation.stop_desired_state Stopped)
  then fail "session.stop did not set desired state to stopped"
;;

let require_equal_lifecycle unix http =
  require_lifecycle_invariants unix;
  require_lifecycle_invariants http;
  if not (equal_lifecycle_observation unix http)
  then
    raise_s
      [%sexp
        "cross-transport lifecycle observations differ"
      , { unix : lifecycle_observation; http : lifecycle_observation }]
;;

let permission_count connection session_id =
  let list_request =
    Agent_protocol.Permission.List_request.
      { session_id; page = page_request (); state = Some Pending }
  in
  match request connection (Permission_list list_request) with
  | Permission_list page -> List.length page.items
  | _ -> fail "permission.list returned the wrong result variant"
;;

let grant_count connection session_id =
  let list_request =
    Agent_protocol.Grant.List_request.
      { page = page_request ()
      ; session_id = Some session_id
      ; principal_id = None
      ; state = Some Active
      }
  in
  match request connection (Grant_list list_request) with
  | Grant_list page -> List.length page.items
  | _ -> fail "grant.list returned the wrong result variant"
;;

let audit_nonempty connection session_id =
  let read_request =
    Agent_protocol.Audit.Read_request.
      { page = page_request ()
      ; session_id = Some session_id
      ; principal_id = None
      ; minimum_level = None
      ; name_prefix = None
      }
  in
  match request connection (Audit_read read_request) with
  | Audit_read page -> not (List.is_empty page.items)
  | _ -> fail "audit.read returned the wrong result variant"
;;

let missing_permission_error connection session attachment ~key =
  let respond_request =
    Agent_protocol.Permission.Respond_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; permission_id = Agent_protocol.Id.Permission.create ()
      ; permission_generation = session.generation
      ; choice = Deny
      ; reason = Some "conformance missing permission"
      ; idempotency_key = idempotency_key key
      }
  in
  (request_error connection (Permission_respond respond_request)).code
;;

let missing_grant_error connection session attachment ~key =
  let revoke_request =
    Agent_protocol.Grant.Revoke_request.
      { grant_id = Agent_protocol.Id.Grant.create ()
      ; session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; reason = "conformance missing grant"
      ; idempotency_key = idempotency_key key
      }
  in
  (request_error connection (Grant_revoke revoke_request)).code
;;

let security_observation connection ~key_prefix =
  ignore (initialize connection : Agent_protocol.Initialize.Response.t);
  let created, _duplicate = create_session connection ~key:(key_prefix ^ ":create") in
  let attachment =
    Option.value_exn created.attachment
    |> fun (attached : Agent_protocol.Public.Result.Attach.t) -> attached.attachment
  in
  { pending_permission_count = permission_count connection created.session.id
  ; active_grant_count = grant_count connection created.session.id
  ; audit_nonempty = audit_nonempty connection created.session.id
  ; missing_permission_error =
      missing_permission_error
        connection
        created.session
        attachment
        ~key:(key_prefix ^ ":permission")
  ; missing_grant_error =
      missing_grant_error
        connection
        created.session
        attachment
        ~key:(key_prefix ^ ":grant")
  }
;;

let require_equal_security unix http =
  if not (equal_security_observation unix http)
  then
    raise_s
      [%sexp
        "cross-transport permission/grant observations differ"
      , { unix : security_observation; http : security_observation }]
;;

let job_count connection session_id =
  let list_request =
    Agent_protocol.Job.List_request.
      { session_id; page = page_request (); status = None; kind = None }
  in
  match request connection (Job_list list_request) with
  | Job_list page -> List.length page.items
  | _ -> fail "job.list returned the wrong result variant"
;;

let missing_job_get_error connection session_id =
  let get_request =
    Agent_protocol.Job.Get_request.
      { session_id; job_id = Agent_protocol.Id.Job.create () }
  in
  (request_error connection (Job_get get_request)).code
;;

let missing_job_cancel_error connection session attachment ~key =
  let cancel_request =
    Agent_protocol.Job.Cancel_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; job_id = Agent_protocol.Id.Job.create ()
      ; idempotency_key = idempotency_key key
      }
  in
  (request_error connection (Job_cancel cancel_request)).code
;;

let schedule_status status =
  Sexp.to_string_mach ([%sexp_of: Agent_protocol.Schedule.status] status)
;;

let create_schedule connection session attachment ~key =
  let create_request =
    Agent_protocol.Schedule.Create_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; payload = `Object [ "event", `String "conformance" ]
      ; due = After_ms 60_000
      ; misfire = Deliver_once_immediately
      ; idempotency_key = idempotency_key key
      }
  in
  let create () =
    match request connection (Schedule_create create_request) with
    | Schedule_create result -> result
    | _ -> fail "schedule.create returned the wrong result variant"
  in
  create (), create ()
;;

let get_schedule connection session_id schedule_id =
  let get_request = Agent_protocol.Schedule.Get_request.{ session_id; schedule_id } in
  match request connection (Schedule_get get_request) with
  | Schedule_get schedule -> schedule
  | _ -> fail "schedule.get returned the wrong result variant"
;;

let schedule_count connection session_id status =
  let list_request =
    Agent_protocol.Schedule.List_request.
      { session_id; page = page_request (); status = Some status }
  in
  match request connection (Schedule_list list_request) with
  | Schedule_list page -> List.length page.items
  | _ -> fail "schedule.list returned the wrong result variant"
;;

let cancel_schedule connection session attachment schedule ~key =
  let cancel_request =
    Agent_protocol.Schedule.Cancel_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; schedule_id = schedule.Agent_protocol.Schedule.id
      ; idempotency_key = idempotency_key key
      }
  in
  match request connection (Schedule_cancel cancel_request) with
  | Schedule_cancel result -> result
  | _ -> fail "schedule.cancel returned the wrong result variant"
;;

let jobs_schedules_observation connection ~key_prefix =
  ignore (initialize connection : Agent_protocol.Initialize.Response.t);
  let created, _duplicate = create_session connection ~key:(key_prefix ^ ":session") in
  let attachment =
    Option.value_exn created.attachment
    |> fun (attached : Agent_protocol.Public.Result.Attach.t) -> attached.attachment
  in
  let first, second =
    create_schedule connection created.session attachment ~key:(key_prefix ^ ":schedule")
  in
  let fetched = get_schedule connection created.session.id first.schedule.id in
  let cancelled =
    cancel_schedule
      connection
      created.session
      attachment
      first.schedule
      ~key:(key_prefix ^ ":cancel")
  in
  { initial_job_count = job_count connection created.session.id
  ; missing_job_get_error = missing_job_get_error connection created.session.id
  ; missing_job_cancel_error =
      missing_job_cancel_error
        connection
        created.session
        attachment
        ~key:(key_prefix ^ ":job")
  ; duplicate_schedule_replayed =
      Agent_protocol.Id.Schedule.compare first.schedule.id second.schedule.id = 0
  ; created_schedule_status = schedule_status first.schedule.status
  ; fetched_schedule_status = schedule_status fetched.status
  ; scheduled_count = schedule_count connection created.session.id "scheduled"
  ; cancelled_schedule_status = schedule_status cancelled.schedule.status
  ; cancel_revision_delta = Int64.(cancelled.mutation.revision - first.mutation.revision)
  ; cancel_sequence_delta =
      Int64.(
        cancelled.mutation.latest_event_sequence - first.mutation.latest_event_sequence)
  ; cancelled_count = schedule_count connection created.session.id "cancelled"
  }
;;

let require_equal_jobs_schedules unix http =
  if not (equal_jobs_schedules_observation unix http)
  then
    raise_s
      [%sexp
        "cross-transport jobs/schedules observations differ"
      , { unix : jobs_schedules_observation; http : jobs_schedules_observation }]
;;

let export_session connection session attachment =
  let export_request =
    Agent_protocol.Session.Export_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; format = Json
      ; revision = None
      ; history = None
      }
  in
  match request connection (Session_export export_request) with
  | Session_export export -> export
  | _ -> fail "session.export returned the wrong result variant"
;;

let read_blob_chunk connection session attachment blob offset =
  let read_request =
    Agent_protocol.Blob.Read_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; blob_id = blob.Agent_protocol.Blob.Metadata.id
      ; offset
      ; max_bytes = 64
      }
  in
  match request connection (Blob_read read_request) with
  | Blob_read chunk -> chunk
  | _ -> fail "blob.read returned the wrong result variant"
;;

let rec download_blob connection session attachment blob offset chunks =
  let chunk = read_blob_chunk connection session attachment blob offset in
  let data = Base64.decode_exn chunk.data_base64 in
  let chunks = (chunk.offset, data) :: chunks in
  if chunk.eof
  then List.rev chunks
  else download_blob connection session attachment blob chunk.next_offset chunks
;;

let top_level_fields contents =
  match Result.try_with (fun () -> Jsonaf.of_string contents) with
  | Ok (`Object fields) ->
    Some (List.map fields ~f:fst |> List.sort ~compare:String.compare)
  | Ok _ | Error _ -> None
;;

let missing_blob_error connection session attachment =
  let read_request =
    Agent_protocol.Blob.Read_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; blob_id = Agent_protocol.Id.Blob.create ()
      ; offset = 0L
      ; max_bytes = 64
      }
  in
  (request_error connection (Blob_read read_request)).code
;;

let blob_observation connection ~key_prefix =
  ignore (initialize connection : Agent_protocol.Initialize.Response.t);
  let created, _duplicate = create_session connection ~key:(key_prefix ^ ":session") in
  let attachment =
    Option.value_exn created.attachment
    |> fun (attached : Agent_protocol.Public.Result.Attach.t) -> attached.attachment
  in
  let export = export_session connection created.session attachment in
  let chunks = download_blob connection created.session attachment export.blob 0L [] in
  let contents = List.map chunks ~f:snd |> String.concat in
  let fields = top_level_fields contents in
  { kind = export.blob.kind
  ; media_type = export.blob.media_type
  ; display_name = export.blob.display_name
  ; nonempty = not (String.is_empty contents)
  ; byte_length_matches =
      Int64.equal export.blob.byte_length (Int64.of_int (String.length contents))
  ; digest_matches =
      String.equal
        export.blob.digest
        (Digestif.SHA256.digest_string contents |> Digestif.SHA256.to_hex)
  ; valid_json = Option.is_some fields
  ; top_level_fields = Option.value fields ~default:[]
  ; chunk_offsets_are_contiguous =
      List.fold chunks ~init:(true, 0L) ~f:(fun (valid, expected) (offset, data) ->
        ( valid && Int64.equal offset expected
        , Int64.(expected + of_int (String.length data)) ))
      |> fst
  ; missing_blob_error = missing_blob_error connection created.session attachment
  }
;;

let require_equal_blob unix http =
  if not (equal_blob_observation unix http)
  then
    raise_s
      [%sexp
        "cross-transport blob observations differ"
      , { unix : blob_observation; http : blob_observation }]
;;

let observe_error connection name command =
  let error = request_error connection command in
  { name; code = error.code; retryable = error.retryable }
;;

let attach_read_only ?(subscribe = false) connection session_id ~key =
  let attach_request =
    Agent_protocol.Session.Attach_request.
      { session_id
      ; requested_mode = Read_only
      ; subscribe
      ; after_sequence = None
      ; reclaim_token = None
      ; idempotency_key = idempotency_key key
      }
  in
  match request_public connection (Session_attach attach_request) with
  | Session_attach attached -> attached.attachment
  | _ -> fail "session.attach returned the wrong result variant"
;;

let error_observations connection ~key_prefix =
  ignore (initialize connection : Agent_protocol.Initialize.Response.t);
  let created, _duplicate = create_session connection ~key:(key_prefix ^ ":create") in
  let writer =
    Option.value_exn created.attachment
    |> fun (attached : Agent_protocol.Public.Result.Attach.t) -> attached.attachment
  in
  let export = export_session connection created.session writer in
  let reader =
    attach_read_only connection created.session.id ~key:(key_prefix ^ ":reader")
  in
  let error_commands : (string * Agent_protocol.Command.t) list =
    [ ( "prompt-not-found"
      , Prompt_get { prompt_id = Agent_protocol.Id.Prompt_definition.create () } )
    ; ( "workspace-not-found"
      , Workspace_get { workspace_id = Agent_protocol.Id.Workspace_definition.create () }
      )
    ; ( "session-not-found"
      , Session_get { session_id = Agent_protocol.Id.Session.create (); history = None } )
    ; ( "job-not-found"
      , Job_get
          { session_id = created.session.id; job_id = Agent_protocol.Id.Job.create () } )
    ; ( "ingress-unavailable"
      , Ingress_submit
          { session_id = created.session.id
          ; registration_id = Agent_protocol.Id.Capability.create ()
          ; namespace = "external.report"
          ; idempotency_key = idempotency_key (key_prefix ^ ":ingress")
          ; payload = `Null
          } )
    ; ( "blob-offset-invalid"
      , Blob_read
          { session_id = created.session.id
          ; attachment_id = writer.id
          ; blob_id = export.blob.id
          ; offset = Int64.(export.blob.byte_length + 1L)
          ; max_bytes = 64
          } )
    ; ( "read-only-start"
      , Session_start
          { session_id = created.session.id
          ; attachment_id = reader.id
          ; queue_if_limited = false
          ; idempotency_key = idempotency_key (key_prefix ^ ":readonly-start")
          } )
    ; ( "renew-non-owner"
      , Session_renew_owner
          { session_id = created.session.id
          ; attachment_id = reader.id
          ; lease_generation = 0L
          ; idempotency_key = idempotency_key (key_prefix ^ ":renew")
          } )
    ; ( "cancel-missing-operation"
      , Session_cancel_operation
          { session_id = created.session.id
          ; attachment_id = reader.id
          ; operation_id = Agent_protocol.Id.Operation.create ()
          ; idempotency_key = idempotency_key (key_prefix ^ ":cancel-operation")
          } )
    ; ( "read-only-send"
      , Session_send_message
          { session_id = created.session.id
          ; attachment_id = reader.id
          ; content = { kind = Plain_text; text = "forbidden"; attachments = [] }
          ; idempotency_key = idempotency_key (key_prefix ^ ":send")
          } )
    ; ( "read-only-compact"
      , Session_compact
          { session_id = created.session.id
          ; attachment_id = reader.id
          ; expected_revision = None
          ; idempotency_key = idempotency_key (key_prefix ^ ":compact")
          } )
    ; ( "read-only-reset"
      , Session_reset
          { session_id = created.session.id
          ; attachment_id = reader.id
          ; expected_revision = created.session.revision
          ; keep_history = true
          ; keep_tasks = true
          ; keep_cache = true
          ; keep_workspace = true
          ; keep_grants = true
          ; keep_labels = true
          ; idempotency_key = idempotency_key (key_prefix ^ ":reset")
          } )
    ; ( "read-only-rebuild"
      , Session_rebuild
          { session_id = created.session.id
          ; attachment_id = reader.id
          ; expected_revision = created.session.revision
          ; prompt_choice = Pinned
          ; idempotency_key = idempotency_key (key_prefix ^ ":rebuild")
          } )
    ; ( "read-only-upgrade"
      , Session_upgrade_prompt
          { session_id = created.session.id
          ; attachment_id = reader.id
          ; expected_revision = created.session.revision
          ; target_revision = Agent_protocol.Id.Prompt_revision.create ()
          ; allow_migration = false
          ; idempotency_key = idempotency_key (key_prefix ^ ":upgrade")
          } )
    ; ( "read-only-delete"
      , Session_delete
          { session_id = created.session.id
          ; attachment_id = reader.id
          ; expected_revision = created.session.revision
          ; policy = Remove
          ; confirmation = Agent_protocol.Id.Session.to_string created.session.id
          ; idempotency_key = idempotency_key (key_prefix ^ ":delete")
          } )
    ]
  in
  let errors =
    List.map error_commands ~f:(fun (name, command) ->
      observe_error connection name command)
  in
  (* This stock daemon fixture has no qualified ingress moderator. Test the
     explicit unavailable-host contract, not a successful helper composition. *)
  let ingress =
    List.find_exn errors ~f:(fun error -> String.equal error.name "ingress-unavailable")
  in
  if not (Agent_protocol.Error.equal_code ingress.code Invalid_request)
  then
    raise_s [%sexp "unexpected unavailable ingress error", (ingress : error_observation)];
  let conflicting_spec =
    { (session_spec connection) with display_name = Some "idempotency conflict" }
  in
  let conflict =
    Session_create
      { spec = conflicting_spec
      ; requested_mode = Some Read_write
      ; subscribe = false
      ; idempotency_key = idempotency_key (key_prefix ^ ":create")
      }
    |> observe_error connection "idempotency-conflict"
  in
  errors @ [ conflict ]
;;

let require_equal_errors unix http =
  if not (List.equal equal_error_observation unix http)
  then
    raise_s
      [%sexp
        "cross-transport error observations differ"
      , { unix : error_observation list; http : error_observation list }]
;;

let create_subscribed_session connection ~key =
  let create_request =
    Agent_protocol.Session.Create_request.
      { spec = session_spec connection
      ; requested_mode = Some Read_write
      ; subscribe = true
      ; idempotency_key = idempotency_key key
      }
  in
  match request_public connection (Session_create create_request) with
  | Session_create created -> created
  | _ -> fail "session.create returned the wrong result variant"
;;

let durable_notification = function
  | Agent_protocol.Envelope.Notification { method_ = "session.event"; params } ->
    Agent_protocol.Public.Durable.of_json params |> protocol_ok |> Option.some
  | Notification _ -> None
  | Request _ | Response _ -> fail "notification stream returned a non-notification"
;;

let rec next_session_event env connection session_id =
  let notification =
    Eio.Time.with_timeout (Eio.Stdenv.clock env) 5. (fun () ->
      Ok (connection.next_notification ()))
  in
  match notification with
  | Error `Timeout -> fail "timed out waiting for a durable event"
  | Ok None -> fail "notification connection closed"
  | Ok (Some envelope) ->
    (match durable_notification envelope with
     | Some event when Agent_protocol.Id.Session.compare event.session_id session_id = 0
       -> event
     | Some _ | None -> next_session_event env connection session_id)
;;

let rec collect_events env connection session_id previous through events =
  if Int64.(previous >= through)
  then List.rev events
  else (
    let event = next_session_event env connection session_id in
    if not (Int64.equal event.sequence Int64.(previous + 1L))
    then fail "live durable event sequences were not contiguous";
    collect_events env connection session_id event.sequence through (event :: events))
;;

let normalize_events events ~base_sequence ~base_revision =
  List.map events ~f:(fun event ->
    { relative_sequence =
        Int64.(event.Agent_protocol.Public.Durable.sequence - base_sequence)
    ; relative_revision = Int64.(event.revision - base_revision)
    ; kind = event.kind
    ; visibility = Support.Public_view.visibility event
    })
;;

let attached_writer created =
  Option.value_exn created.Agent_protocol.Public.Result.Create.attachment
  |> fun (attached : Agent_protocol.Public.Result.Attach.t) -> attached.attachment
;;

let schedule_event_trace env connection created ~key_prefix =
  let writer = attached_writer created in
  let scheduled, _duplicate =
    create_schedule connection created.session writer ~key:(key_prefix ^ ":schedule")
  in
  let cancelled =
    cancel_schedule
      connection
      created.session
      writer
      scheduled.schedule
      ~key:(key_prefix ^ ":cancel")
  in
  collect_events
    env
    connection
    created.session.id
    created.mutation.latest_event_sequence
    cancelled.mutation.latest_event_sequence
    []
;;

let event_order_observation env connection ~key_prefix =
  ignore (initialize connection : Agent_protocol.Initialize.Response.t);
  let created = create_subscribed_session connection ~key:(key_prefix ^ ":create") in
  let live = schedule_event_trace env connection created ~key_prefix in
  let replay =
    replay_events connection created.session.id ~key:(key_prefix ^ ":replay")
  in
  let normalize =
    normalize_events
      ~base_sequence:created.mutation.latest_event_sequence
      ~base_revision:created.mutation.revision
  in
  { live = normalize live
  ; replay =
      List.filter replay ~f:(fun event ->
        Int64.(
          event.Agent_protocol.Public.Durable.sequence
          > created.mutation.latest_event_sequence))
      |> normalize
  }
;;

let require_equal_event_order unix http =
  if not (equal_event_order_observation unix http)
  then
    raise_s
      [%sexp
        "cross-transport event-order observations differ"
      , { unix : event_order_observation; http : event_order_observation }];
  if not (List.equal equal_event_observation unix.live unix.replay)
  then fail "live event order differed from durable replay"
;;

let readonly_schedule_error connection session reader ~key =
  let create_request =
    Agent_protocol.Schedule.Create_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = reader.Agent_protocol.Session.Attachment.id
      ; payload = `Object [ "event", `String "forbidden" ]
      ; due = After_ms 60_000
      ; misfire = Deliver_once_immediately
      ; idempotency_key = idempotency_key key
      }
  in
  (request_error connection (Schedule_create create_request)).code
;;

let visibility_observation env writer_connection reader_connection ~key_prefix =
  ignore (initialize writer_connection : Agent_protocol.Initialize.Response.t);
  ignore (initialize reader_connection : Agent_protocol.Initialize.Response.t);
  let created =
    create_subscribed_session writer_connection ~key:(key_prefix ^ ":create")
  in
  let reader =
    attach_read_only
      ~subscribe:true
      reader_connection
      created.session.id
      ~key:(key_prefix ^ ":reader")
  in
  let writer_events = schedule_event_trace env writer_connection created ~key_prefix in
  let through = (List.last_exn writer_events).Agent_protocol.Public.Durable.sequence in
  let reader_events =
    collect_events
      env
      reader_connection
      created.session.id
      created.mutation.latest_event_sequence
      through
      []
  in
  let normalize =
    normalize_events
      ~base_sequence:created.mutation.latest_event_sequence
      ~base_revision:created.mutation.revision
  in
  { writer = normalize writer_events
  ; reader = normalize reader_events
  ; reader_mutation_error =
      readonly_schedule_error
        reader_connection
        created.session
        reader
        ~key:(key_prefix ^ ":forbidden")
  }
;;

let require_equal_visibility unix http =
  if not (equal_visibility_observation unix http)
  then
    raise_s
      [%sexp
        "cross-transport visibility observations differ"
      , { unix : visibility_observation; http : visibility_observation }];
  if not (List.equal equal_event_observation unix.writer unix.reader)
  then fail "read-only observer received a different visible event sequence"
;;

let history_snapshot connection session_id =
  match request_public connection (Session_get { session_id; history = None }) with
  | Session_get snapshot -> Agent_protocol.Public.Snapshot.fields snapshot
  | _ -> fail "session.get returned the wrong result variant"
;;

let history_delete_command session_id attachment_id history_id revision key =
  Agent_protocol.Command.Session_delete_history
    { session_id
    ; attachment_id
    ; history_id
    ; expected_revision = revision
    ; idempotency_key = idempotency_key key
    }
;;

let require_same_snapshot before after =
  if
    not
      (Poly.equal
         (Support.Public_view.snapshot_to_json before)
         (Support.Public_view.snapshot_to_json after))
  then fail "rejected or replayed history deletion changed session state"
;;

let reject_history_deletion
      writer
      reader
      attachment
      reader_attachment
      before
      id
      key_prefix
  =
  let session_id = before.Agent_protocol.Public.Snapshot.Fields.session.id in
  let reader_error =
    request_error
      reader
      (history_delete_command
         session_id
         reader_attachment
         id
         before.revision
         (key_prefix ^ ":reader-delete"))
  in
  let stale_error =
    request_error
      writer
      (history_delete_command
         session_id
         attachment
         id
         Int64.(before.revision - 1L)
         (key_prefix ^ ":stale-delete"))
  in
  if not (Agent_protocol.Error.equal_code reader_error.code Permission_denied)
  then fail "read-only history deletion did not deny permission";
  if not (Agent_protocol.Error.equal_code stale_error.code Conflict)
  then fail "stale history deletion did not reject revision";
  require_same_snapshot before (history_snapshot writer session_id);
  reader_error.code, stale_error.code
;;

let delete_history_replayed connection command =
  let first = request connection command in
  let replay = request connection command in
  if
    not
      (Poly.equal
         (Agent_protocol.Method_result.to_json first)
         (Agent_protocol.Method_result.to_json replay))
  then fail "history deletion idempotent replay changed result";
  match first with
  | Session_delete_history mutation -> mutation
  | _ -> fail "session.delete_history returned the wrong result variant"
;;

let require_deleted_history before after id =
  let expected =
    List.filter
      before.Agent_protocol.Public.Snapshot.Fields.canonical_history.entries
      ~f:(fun entry -> Agent_protocol.History.Id.compare entry.id id <> 0)
  in
  let actual = after.Agent_protocol.Public.Snapshot.Fields.canonical_history.entries in
  if
    not
      (Poly.equal
         (List.map expected ~f:Agent_protocol.Public.History.to_json)
         (List.map actual ~f:Agent_protocol.Public.History.to_json))
  then fail "history deletion did not remove exactly the selected canonical occurrence";
  expected
;;

let require_history_replacement events expected =
  let windows =
    List.filter_map events ~f:(fun event ->
      match Support.Public_view.payload event with
      | Some (History_replaced window) -> Some window
      | _ -> None)
  in
  match windows with
  | [ window ]
    when Poly.equal
           (List.map window.entries ~f:Agent_protocol.Public.History.to_json)
           (List.map expected ~f:Agent_protocol.Public.History.to_json) -> ()
  | _ -> fail "subscriber did not receive the committed history replacement"
;;

let history_deletion_observation env writer reader ~key_prefix =
  ignore (initialize writer : Agent_protocol.Initialize.Response.t);
  ignore (initialize reader : Agent_protocol.Initialize.Response.t);
  let created = create_subscribed_session writer ~key:(key_prefix ^ ":create") in
  let reader_attachment =
    attach_read_only
      ~subscribe:true
      reader
      created.session.id
      ~key:(key_prefix ^ ":reader")
  in
  let before = history_snapshot writer created.session.id in
  let id = (List.hd_exn before.canonical_history.entries).id in
  let attachment = (attached_writer created).id in
  let reader_error, stale_error =
    reject_history_deletion
      writer
      reader
      attachment
      reader_attachment.id
      before
      id
      key_prefix
  in
  let command =
    history_delete_command
      created.session.id
      attachment
      id
      before.revision
      (key_prefix ^ ":delete-history")
  in
  let deleted = delete_history_replayed writer command in
  let after = history_snapshot writer created.session.id in
  let expected = require_deleted_history before after id in
  require_same_snapshot after (history_snapshot reader created.session.id);
  if
    (not (Int64.equal after.revision Int64.(before.revision + 1L)))
    || (not (Int64.equal after.revision deleted.mutation.revision))
    || not
         (Int64.equal after.latest_event_sequence deleted.mutation.latest_event_sequence)
  then fail "history deletion replay committed more than one mutation";
  let collect connection =
    let events =
      collect_events
        env
        connection
        created.session.id
        created.mutation.latest_event_sequence
        after.latest_event_sequence
        []
    in
    require_history_replacement events expected;
    normalize_events
      events
      ~base_sequence:created.mutation.latest_event_sequence
      ~base_revision:created.mutation.revision
  in
  let events = collect writer in
  if not (List.equal equal_event_observation events (collect reader))
  then fail "writer and reader history deletion events differ";
  { reader_error
  ; stale_error
  ; remaining_count = List.length expected
  ; revision_delta = Int64.(after.revision - before.revision)
  ; sequence_delta = Int64.(after.latest_event_sequence - before.latest_event_sequence)
  ; events
  }
;;

let require_equal_history_deletion expected actual =
  if not (equal_history_deletion_observation expected actual)
  then
    raise_s
      [%sexp
        "cross-transport history deletion observations differ"
      , { expected : history_deletion_observation; actual : history_deletion_observation }]
;;

let close_clients clients = List.iter clients ~f:(fun client -> client.close ())

let rec with_stdio_clients ~sw env fixture arguments clients f =
  match arguments with
  | [] -> f (List.rev clients)
  | arguments :: rest ->
    with_stdio_client ~sw env fixture arguments (fun client ->
      with_stdio_clients ~sw env fixture rest (client :: clients) f)
;;

let with_transport_matrix ~sw env environment fixture f =
  let unix = unix_client ~sw env fixture in
  let http = http_client ~sw env fixture in
  let gateways =
    [ stdio_gateway_arguments environment fixture `Unix
    ; stdio_gateway_arguments environment fixture (`Http "conformance-http-gateway.token")
    ]
  in
  Exn.protect
    ~f:(fun () ->
      with_stdio_clients ~sw env fixture gateways [] (function
        | [ stdio_unix; stdio_http ] -> f unix http stdio_unix stdio_http
        | _ -> fail "stdio gateway matrix size differs"))
    ~finally:(fun () -> close_clients [ http; unix ])
;;

let with_transport_pair_matrix ~sw env environment fixture f =
  let unix_writer = unix_client ~sw env fixture in
  let unix_reader = unix_client ~sw env fixture in
  let http_writer = http_client ~sw env fixture in
  let http_reader = http_client ~sw env fixture in
  let unix_arguments = stdio_gateway_arguments environment fixture `Unix in
  let http_arguments name = stdio_gateway_arguments environment fixture (`Http name) in
  let gateways =
    [ unix_arguments
    ; unix_arguments
    ; http_arguments "conformance-http-writer.token"
    ; http_arguments "conformance-http-reader.token"
    ]
  in
  Exn.protect
    ~f:(fun () ->
      with_stdio_clients ~sw env fixture gateways [] (function
        | [ stdio_unix_writer; stdio_unix_reader; stdio_http_writer; stdio_http_reader ]
          ->
          f
            (unix_writer, unix_reader)
            (http_writer, http_reader)
            (stdio_unix_writer, stdio_unix_reader)
            (stdio_http_writer, stdio_http_reader)
        | _ -> fail "stdio visibility matrix size differs"))
    ~finally:(fun () ->
      close_clients [ http_reader; http_writer; unix_reader; unix_writer ])
;;

let test_read_methods env environment =
  let fixture = fixture env environment "conformance-read" in
  let daemon_observation =
    Eio.Switch.run (fun sw ->
      with_daemon ~sw env fixture (fun _daemon health ->
        if not health.Agent_protocol.Health.Response.ready
        then fail "daemon health was not ready";
        with_transport_matrix
          ~sw
          env
          environment
          fixture
          (fun unix http stdio_unix stdio_http ->
             let baseline = observe unix in
             require_equal baseline (observe http);
             require_equal baseline (observe stdio_unix);
             require_equal baseline (observe stdio_http));
        let portable = unix_client ~sw env fixture in
        Exn.protect
          ~f:(fun () -> portable_observe portable)
          ~finally:(fun () -> portable.close ())))
  in
  ignore (daemon_observation : portable_read_observation);
  let embedded_observation = direct_embedded_observation env environment fixture in
  require_equal_portable_read
    embedded_observation
    (local_stdio_observation env environment fixture)
;;

let inference_read_observation connection ~key_prefix =
  let module Q = Agent_protocol.Inference_query in
  ignore (initialize connection : Agent_protocol.Initialize.Response.t);
  let created, _replayed = create_session connection ~key:(key_prefix ^ "-create") in
  let session_id = created.session.id in
  let before = history_snapshot connection session_id in
  let summary =
    match request connection (Session_inference_summary { session_id }) with
    | Session_inference_summary summary -> summary
    | _ -> fail "session.inference_summary returned the wrong result variant"
  in
  let query =
    Q.Request.create
      ~session_id
      ~page:(Agent_protocol.Page.Request.create ~limit:1 () |> protocol_ok)
      ~include_configuration:false
      ~include_diagnostics:false
    |> protocol_ok
  in
  let response =
    match request connection (Session_inference_observations query) with
    | Session_inference_observations response -> response
    | _ -> fail "session.inference_observations returned the wrong result variant"
  in
  let require_summary actual =
    if not (Jsonaf.exactly_equal (Q.Summary.to_json summary) (Q.Summary.to_json actual))
    then fail "inference read summaries disagree"
  in
  require_summary (Q.Response.summary response);
  (match before.session.inference_summary with
   | History_entry.Payload.Presence.Value actual -> require_summary actual
   | Absent | Null -> fail "fresh tracked session omitted its inference summary");
  let attempts = Q.Response.attempts response in
  if not (List.is_empty attempts.items && Option.is_none attempts.next_cursor)
  then fail "fresh stopped session returned inference rows or a continuation";
  let require_zero values =
    if not (List.for_all values ~f:(Int64.equal 0L))
    then fail "fresh stopped session reported prior inference accounting"
  in
  require_zero [ Q.Summary.retained_attempts summary ];
  let turns = Q.Summary.turns summary in
  require_zero
    [ turns.pending; turns.completed; turns.failed; turns.cancelled; turns.interrupted ];
  let coverage = Q.Summary.coverage summary in
  if
    coverage.before_tracking_unknown
    || not (Q.Coverage.equal_tracking_status coverage.tracking_status Available)
  then fail "fresh session reported unknown or limited accounting coverage";
  require_zero
    [ coverage.retired_attempts
    ; coverage.untracked_attempts
    ; coverage.retired_turns
    ; coverage.untracked_turns
    ];
  (* Empty sums have zero contributions, not an Actual-zero usage observation.
     Component-specific missing/null reasons remain independent accounting. *)
  let require_empty_metric (metric : Q.Metric.t) =
    if
      (not (Q.Metric.equal_sum metric.actual (Tokens 0L)))
      || (not (Q.Metric.equal_sum metric.estimated (Tokens 0L)))
      || metric.mixed_estimators
    then fail "fresh session reported inference token contributions";
    require_zero
      [ metric.actual_attempts
      ; metric.estimated_attempts
      ; metric.unknown.not_reported
      ; metric.unknown.explicit_null
      ; metric.unknown.interrupted
      ; metric.unknown.not_submitted
      ; metric.unknown.before_tracking
      ]
  in
  let components = Q.Summary.components summary in
  List.iter
    [ components.input
    ; components.output
    ; components.reported_total
    ; components.cached_input
    ; components.cache_write_input
    ; components.reasoning_output
    ]
    ~f:require_empty_metric;
  let after = history_snapshot connection session_id in
  if
    not
      (Jsonaf.exactly_equal
         (Support.Public_view.snapshot_to_json before)
         (Support.Public_view.snapshot_to_json after))
  then fail "inference reads activated or changed the stopped session";
  Q.Summary.to_json summary |> Jsonaf.to_string
;;

let test_inference_reads env environment =
  let fixture = fixture env environment "conformance-inference" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let baseline = inference_read_observation unix ~key_prefix:"unix" in
           List.iter
             [ http, "http"; stdio_unix, "stdio-unix"; stdio_http, "stdio-http" ]
             ~f:(fun (connection, key_prefix) ->
               let actual = inference_read_observation connection ~key_prefix in
               if not (String.equal baseline actual)
               then fail "cross-transport inference read summaries differ"))))
;;

let test_session_lifecycle env environment =
  let fixture = fixture env environment "conformance-lifecycle" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let baseline = lifecycle_observation unix ~key_prefix:"unix" in
           require_equal_lifecycle
             baseline
             (lifecycle_observation http ~key_prefix:"http");
           require_equal_lifecycle
             baseline
             (lifecycle_observation stdio_unix ~key_prefix:"stdio-unix");
           require_equal_lifecycle
             baseline
             (lifecycle_observation stdio_http ~key_prefix:"stdio-http"))))
;;

type metadata_observation =
  { metadata_revision_delta : int64
  ; display_name : string option
  ; labels : (string * string) list
  ; retry_replays_original : bool
  ; receipt_matches : bool
  ; visible_in_snapshot : bool
  ; stale_revision_error : Agent_protocol.Error.code
  ; read_only_error : Agent_protocol.Error.code
  }
[@@deriving equal, sexp]

let metadata_observation connection ~key_prefix =
  ignore (initialize connection : Agent_protocol.Initialize.Response.t);
  let created, _duplicate = create_session connection ~key:(key_prefix ^ ":create") in
  let attachment = (Option.value_exn created.attachment).attachment in
  let patch =
    Agent_protocol.Session_metadata.Patch.create
      ~name:(Set "Conformance metadata")
      ~set_labels:[ "conformance", "metadata" ]
      ~remove_labels:[]
    |> protocol_ok
  in
  let update_request =
    Agent_protocol.Session_metadata.Request.
      { session_id = created.session.id
      ; attachment_id = attachment.id
      ; expected_metadata_revision = created.session.metadata_revision
      ; patch
      ; idempotency_key = idempotency_key (key_prefix ^ ":metadata")
      }
  in
  let update request_ =
    match request connection (Session_update_metadata request_) with
    | Session_update_metadata result -> result
    | _ -> fail "session.update_metadata returned the wrong result variant"
  in
  let updated = update update_request in
  let retry = update update_request in
  let stale_revision_error =
    request_error
      connection
      (Session_update_metadata
         { update_request with
           idempotency_key = idempotency_key (key_prefix ^ ":metadata-stale")
         })
  in
  let reader =
    attach_replay connection created.session.id ~key:(key_prefix ^ ":reader")
  in
  let read_only_error =
    request_error
      connection
      (Session_update_metadata
         { update_request with
           attachment_id = reader.attachment.id
         ; expected_metadata_revision = updated.session.metadata_revision
         ; idempotency_key = idempotency_key (key_prefix ^ ":metadata-read-only")
         })
  in
  let visible_in_snapshot =
    match
      request_public
        connection
        (Session_get { session_id = created.session.id; history = None })
    with
    | Session_get snapshot ->
      let session = (Agent_protocol.Public.Snapshot.fields snapshot).session in
      Int64.equal session.metadata_revision updated.session.metadata_revision
      && Option.equal
           String.equal
           session.spec.display_name
           updated.session.spec.display_name
      && List.equal
           [%equal: string * string]
           session.spec.labels
           updated.session.spec.labels
    | _ -> fail "metadata snapshot returned the wrong result variant"
  in
  let receipt_matches =
    match command_receipt connection (Session_update_metadata update_request) with
    | Committed (Session_mutation { session_id; mutation }) ->
      Agent_protocol.Id.Session.equal session_id created.session.id
      && Int64.equal mutation.revision updated.mutation.revision
      && Int64.equal mutation.latest_event_sequence updated.mutation.latest_event_sequence
    | _ -> false
  in
  { metadata_revision_delta =
      Int64.(updated.session.metadata_revision - created.session.metadata_revision)
  ; display_name = updated.session.spec.display_name
  ; labels = updated.session.spec.labels
  ; retry_replays_original =
      Int64.equal retry.session.metadata_revision updated.session.metadata_revision
      && Int64.equal retry.mutation.revision updated.mutation.revision
      && Int64.equal
           retry.mutation.latest_event_sequence
           updated.mutation.latest_event_sequence
  ; receipt_matches
  ; visible_in_snapshot
  ; stale_revision_error = stale_revision_error.code
  ; read_only_error = read_only_error.code
  }
;;

let test_session_metadata env environment =
  let fixture = fixture env environment "conformance-metadata" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let baseline = metadata_observation unix ~key_prefix:"unix" in
           if
             not
               (Int64.equal baseline.metadata_revision_delta 1L
                && Option.equal
                     String.equal
                     baseline.display_name
                     (Some "Conformance metadata")
                && List.equal
                     [%equal: string * string]
                     baseline.labels
                     [ "conformance", "metadata"; "suite", "conformance" ]
                && baseline.retry_replays_original
                && baseline.receipt_matches
                && baseline.visible_in_snapshot
                && Agent_protocol.Error.equal_code baseline.stale_revision_error Conflict
                && Agent_protocol.Error.equal_code
                     baseline.read_only_error
                     Permission_denied)
           then
             raise_s
               [%sexp
                 "metadata conformance invariants failed"
               , (baseline : metadata_observation)];
           List.iter
             [ http, "http"; stdio_unix, "stdio-unix"; stdio_http, "stdio-http" ]
             ~f:(fun (client, key_prefix) ->
               let actual = metadata_observation client ~key_prefix in
               if not (equal_metadata_observation baseline actual)
               then
                 raise_s
                   [%sexp
                     "cross-transport metadata semantics differ"
                   , (baseline : metadata_observation)
                   , (actual : metadata_observation)]))))
;;

type configuration_observation =
  { revision_delta : int64
  ; repeated_intent_revision_delta : int64
  ; model : string
  ; retry_replays_original : bool
  ; receipt_matches : bool
  ; read_matches : bool
  ; stopped_without_activation : bool
  ; stale_generation_error : Agent_protocol.Error.code
  ; stale_revision_error : Agent_protocol.Error.code
  ; read_only_error : Agent_protocol.Error.code
  }
[@@deriving equal, sexp]

let configuration_observation connection ~key_prefix =
  let module C = Agent_protocol.Session_configuration in
  ignore (initialize connection : Agent_protocol.Initialize.Response.t);
  let created, _duplicate = create_session connection ~key:(key_prefix ^ ":create") in
  let session_id = created.session.id in
  let attachment = (Option.value_exn created.attachment).attachment in
  let before = history_snapshot connection session_id in
  let get () =
    match request connection (Session_configuration_get { session_id }) with
    | Session_configuration_get view -> view
    | _ -> fail "configuration_get returned the wrong result variant"
  in
  let initial = get () in
  let patch =
    C.Patch.create ~model:"conformance-config-model" ~settings:[] () |> protocol_ok
  in
  let update_request =
    C.Update_request.
      { session_id
      ; attachment_id = attachment.id
      ; expected_generation = created.session.generation
      ; expected_revision = initial.revision
      ; patch
      ; idempotency_key = idempotency_key (key_prefix ^ ":configuration")
      }
  in
  let update request_ =
    match request connection (Session_configuration_update request_) with
    | Session_configuration_update view -> view
    | _ -> fail "configuration_update returned the wrong result variant"
  in
  let updated = update update_request in
  let retry = update update_request in
  let receipt_matches =
    match command_receipt connection (Session_configuration_update update_request) with
    | Committed (Configuration_updated { session_id = actual_id; revision }) ->
      Agent_protocol.Id.Session.equal actual_id session_id
      && Int64.equal revision updated.revision
    | _ -> false
  in
  let stale_revision_error =
    request_error
      connection
      (Session_configuration_update
         { update_request with
           idempotency_key = idempotency_key (key_prefix ^ ":configuration-stale")
         })
  in
  (* A fresh accepted command records repeated intent once, even if its selected
     target is unchanged; an original retry is reconciliation, not new intent. *)
  let repeated =
    update
      { update_request with
        expected_revision = updated.revision
      ; idempotency_key = idempotency_key (key_prefix ^ ":configuration-repeated")
      }
  in
  let stale_generation_error =
    request_error
      connection
      (Session_configuration_update
         { update_request with
           expected_generation = created.session.generation + 1
         ; expected_revision = repeated.revision
         ; idempotency_key = idempotency_key (key_prefix ^ ":configuration-generation")
         })
  in
  let reader = attach_replay connection session_id ~key:(key_prefix ^ ":reader") in
  let read_only_error =
    request_error
      connection
      (Session_configuration_update
         { update_request with
           attachment_id = reader.attachment.id
         ; expected_revision = repeated.revision
         ; idempotency_key = idempotency_key (key_prefix ^ ":configuration-read-only")
         })
  in
  let current = get () in
  let after = history_snapshot connection session_id in
  { revision_delta = Int64.(updated.revision - initial.revision)
  ; repeated_intent_revision_delta = Int64.(repeated.revision - updated.revision)
  ; model = Inference.Observation.Configuration.model (Option.value_exn current.selected)
  ; retry_replays_original = Int64.equal retry.revision updated.revision
  ; receipt_matches
  ; read_matches = Jsonaf.exactly_equal (C.to_json repeated) (C.to_json current)
  ; stopped_without_activation =
      Agent_protocol.Session.equal_desired_state after.session.desired_state Stopped
      && (match after.session.observed_state with
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
      && Option.is_none after.session.active_operation
      && Option.is_none current.capture
      && (not current.pending)
      && Jsonaf.exactly_equal
           (Agent_protocol.Public.History.Window.to_json before.canonical_history)
           (Agent_protocol.Public.History.Window.to_json after.canonical_history)
  ; stale_generation_error = stale_generation_error.code
  ; stale_revision_error = stale_revision_error.code
  ; read_only_error = read_only_error.code
  }
;;

let test_session_configuration env environment =
  let fixture = fixture env environment "conformance-configuration" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let baseline = configuration_observation unix ~key_prefix:"unix" in
           if
             not
               (Int64.equal baseline.revision_delta 1L
                && Int64.equal baseline.repeated_intent_revision_delta 1L
                && String.equal baseline.model "conformance-config-model"
                && baseline.retry_replays_original
                && baseline.receipt_matches
                && baseline.read_matches
                && baseline.stopped_without_activation
                && Agent_protocol.Error.equal_code
                     baseline.stale_generation_error
                     Conflict
                && Agent_protocol.Error.equal_code baseline.stale_revision_error Conflict
                && Agent_protocol.Error.equal_code
                     baseline.read_only_error
                     Permission_denied)
           then
             raise_s
               [%sexp
                 "configuration conformance invariants failed"
               , (baseline : configuration_observation)];
           List.iter
             [ http, "http"; stdio_unix, "stdio-unix"; stdio_http, "stdio-http" ]
             ~f:(fun (client, key_prefix) ->
               let actual = configuration_observation client ~key_prefix in
               if not (equal_configuration_observation baseline actual)
               then
                 raise_s
                   [%sexp
                     "cross-transport configuration semantics differ"
                   , (baseline : configuration_observation)
                   , (actual : configuration_observation)]))))
;;

type organization_observation =
  { create_retry_exact : bool
  ; changed_key_error : Agent_protocol.Error.code
  ; get_exact : bool
  ; pages_complete_in_order : bool
  ; update_retry_exact : bool
  ; stale_revision_error : Agent_protocol.Error.code
  ; invalidated_cursor_error : Agent_protocol.Error.code
  ; receipt_matches : bool
  ; delete_retry_exact : bool
  ; deleted_get_error : Agent_protocol.Error.code
  ; deleted_list_empty : bool
  ; historical_retry_exact : bool
  }
[@@deriving equal, sexp]

let organization_name value =
  Agent_protocol.Organization_group.Name.create value |> protocol_ok
;;

let project_observation connection ~host_id ~key_prefix =
  let create_request =
    Agent_protocol.Organization_request.Create.
      { host_id
      ; name = organization_name "First"
      ; idempotency_key = idempotency_key (key_prefix ^ ":project-create")
      }
  in
  let create request_ =
    match request connection (Project_create request_) with
    | Project_create group -> group
    | _ -> fail "project.create returned the wrong result variant"
  in
  let first = create create_request in
  let duplicate = create create_request in
  let changed_key_error =
    request_error
      connection
      (Project_create { create_request with name = organization_name "Changed" })
  in
  let second =
    create
      { create_request with
        name = organization_name "Second"
      ; idempotency_key = idempotency_key (key_prefix ^ ":project-second")
      }
  in
  let get_request =
    Agent_protocol.Organization_request.Project.Get.{ host_id; id = first.id }
  in
  let fetched =
    match request connection (Project_get get_request) with
    | Project_get group -> group
    | _ -> fail "project.get returned the wrong result variant"
  in
  let list_request =
    Agent_protocol.Organization_request.List.
      { host_id
      ; creator_principal_id = Some first.creator_principal_id
      ; page = Agent_protocol.Page.Request.create ~limit:1 () |> protocol_ok
      }
  in
  let list request_ =
    match request connection (Project_list request_) with
    | Project_list page -> page
    | _ -> fail "project.list returned the wrong result variant"
  in
  let page_one = list list_request in
  let cursor =
    match page_one.next_cursor with
    | Some cursor -> cursor
    | None -> fail "organization first page must have a continuation"
  in
  let continuation =
    { list_request with
      page = Agent_protocol.Page.Request.create ~limit:1 ~cursor () |> protocol_ok
    }
  in
  let page_two = list continuation in
  let expected =
    List.sort
      [ first; second ]
      ~compare:(fun (left : Agent_protocol.Organization_group.Project.t) right ->
        let timestamp =
          Agent_protocol.Timestamp.compare left.created_at right.created_at
        in
        if Int.equal timestamp 0
        then Agent_protocol.Id.Project.compare left.id right.id
        else timestamp)
  in
  let update_request =
    Agent_protocol.Organization_request.Project.Update.
      { host_id
      ; id = first.id
      ; expected_revision = first.revision
      ; name = organization_name "Renamed"
      ; idempotency_key = idempotency_key (key_prefix ^ ":project-update")
      }
  in
  let update request_ =
    match request connection (Project_update request_) with
    | Project_update group -> group
    | _ -> fail "project.update returned the wrong result variant"
  in
  let updated = update update_request in
  let update_retry = update update_request in
  let stale_revision_error =
    request_error
      connection
      (Project_update
         { update_request with
           idempotency_key = idempotency_key (key_prefix ^ ":project-stale")
         })
  in
  let invalidated_cursor_error = request_error connection (Project_list continuation) in
  let receipt_matches =
    match command_receipt connection (Project_update update_request) with
    | Committed (Project_mutation { project_id; revision }) ->
      Agent_protocol.Id.Project.equal project_id first.id
      && Int64.equal revision updated.revision
    | _ -> false
  in
  let delete_request =
    Agent_protocol.Organization_request.Project.Delete.
      { host_id
      ; id = first.id
      ; expected_revision = updated.revision
      ; idempotency_key = idempotency_key (key_prefix ^ ":project-delete")
      }
  in
  let delete request_ =
    match request connection (Project_delete request_) with
    | Project_delete result -> result
    | _ -> fail "project.delete returned the wrong result variant"
  in
  let deleted = delete delete_request in
  let delete_retry = delete delete_request in
  let deleted_get_error = request_error connection (Project_get get_request) in
  ignore
    (delete
       { delete_request with
         id = second.id
       ; expected_revision = second.revision
       ; idempotency_key = idempotency_key (key_prefix ^ ":project-delete-second")
       });
  let final_page = list { list_request with page = page_request () } in
  { create_retry_exact = Agent_protocol.Organization_group.Project.equal first duplicate
  ; changed_key_error = changed_key_error.code
  ; get_exact = Agent_protocol.Organization_group.Project.equal first fetched
  ; pages_complete_in_order =
      List.equal
        Agent_protocol.Organization_group.Project.equal
        expected
        (page_one.items @ page_two.items)
      && Option.is_none page_two.next_cursor
  ; update_retry_exact =
      Agent_protocol.Organization_group.Project.equal updated update_retry
      && Int64.equal updated.revision 1L
  ; stale_revision_error = stale_revision_error.code
  ; invalidated_cursor_error = invalidated_cursor_error.code
  ; receipt_matches
  ; delete_retry_exact =
      Agent_protocol.Organization_result.Project_deleted.equal deleted delete_retry
      && Int64.equal deleted.revision 2L
  ; deleted_get_error = deleted_get_error.code
  ; deleted_list_empty = List.is_empty final_page.items
  ; historical_retry_exact =
      Agent_protocol.Organization_group.Project.equal first (create create_request)
  }
;;

let collection_observation connection ~host_id ~key_prefix =
  let create_request =
    Agent_protocol.Organization_request.Create.
      { host_id
      ; name = organization_name "First"
      ; idempotency_key = idempotency_key (key_prefix ^ ":collection-create")
      }
  in
  let create request_ =
    match request connection (Collection_create request_) with
    | Collection_create group -> group
    | _ -> fail "collection.create returned the wrong result variant"
  in
  let first = create create_request in
  let duplicate = create create_request in
  let changed_key_error =
    request_error
      connection
      (Collection_create { create_request with name = organization_name "Changed" })
  in
  let second =
    create
      { create_request with
        name = organization_name "Second"
      ; idempotency_key = idempotency_key (key_prefix ^ ":collection-second")
      }
  in
  let get_request =
    Agent_protocol.Organization_request.Collection.Get.{ host_id; id = first.id }
  in
  let fetched =
    match request connection (Collection_get get_request) with
    | Collection_get group -> group
    | _ -> fail "collection.get returned the wrong result variant"
  in
  let list_request =
    Agent_protocol.Organization_request.List.
      { host_id
      ; creator_principal_id = Some first.creator_principal_id
      ; page = Agent_protocol.Page.Request.create ~limit:1 () |> protocol_ok
      }
  in
  let list request_ =
    match request connection (Collection_list request_) with
    | Collection_list page -> page
    | _ -> fail "collection.list returned the wrong result variant"
  in
  let page_one = list list_request in
  let cursor =
    match page_one.next_cursor with
    | Some cursor -> cursor
    | None -> fail "organization first page must have a continuation"
  in
  let continuation =
    { list_request with
      page = Agent_protocol.Page.Request.create ~limit:1 ~cursor () |> protocol_ok
    }
  in
  let page_two = list continuation in
  let expected =
    List.sort
      [ first; second ]
      ~compare:(fun (left : Agent_protocol.Organization_group.Collection.t) right ->
        let timestamp =
          Agent_protocol.Timestamp.compare left.created_at right.created_at
        in
        if Int.equal timestamp 0
        then Agent_protocol.Id.Collection.compare left.id right.id
        else timestamp)
  in
  let update_request =
    Agent_protocol.Organization_request.Collection.Update.
      { host_id
      ; id = first.id
      ; expected_revision = first.revision
      ; name = organization_name "Renamed"
      ; idempotency_key = idempotency_key (key_prefix ^ ":collection-update")
      }
  in
  let update request_ =
    match request connection (Collection_update request_) with
    | Collection_update group -> group
    | _ -> fail "collection.update returned the wrong result variant"
  in
  let updated = update update_request in
  let update_retry = update update_request in
  let stale_revision_error =
    request_error
      connection
      (Collection_update
         { update_request with
           idempotency_key = idempotency_key (key_prefix ^ ":collection-stale")
         })
  in
  let invalidated_cursor_error =
    request_error connection (Collection_list continuation)
  in
  let receipt_matches =
    match command_receipt connection (Collection_update update_request) with
    | Committed (Collection_mutation { collection_id; revision }) ->
      Agent_protocol.Id.Collection.equal collection_id first.id
      && Int64.equal revision updated.revision
    | _ -> false
  in
  let delete_request =
    Agent_protocol.Organization_request.Collection.Delete.
      { host_id
      ; id = first.id
      ; expected_revision = updated.revision
      ; idempotency_key = idempotency_key (key_prefix ^ ":collection-delete")
      }
  in
  let delete request_ =
    match request connection (Collection_delete request_) with
    | Collection_delete result -> result
    | _ -> fail "collection.delete returned the wrong result variant"
  in
  let deleted = delete delete_request in
  let delete_retry = delete delete_request in
  let deleted_get_error = request_error connection (Collection_get get_request) in
  ignore
    (delete
       { delete_request with
         id = second.id
       ; expected_revision = second.revision
       ; idempotency_key = idempotency_key (key_prefix ^ ":collection-delete-second")
       });
  let final_page = list { list_request with page = page_request () } in
  { create_retry_exact =
      Agent_protocol.Organization_group.Collection.equal first duplicate
  ; changed_key_error = changed_key_error.code
  ; get_exact = Agent_protocol.Organization_group.Collection.equal first fetched
  ; pages_complete_in_order =
      List.equal
        Agent_protocol.Organization_group.Collection.equal
        expected
        (page_one.items @ page_two.items)
      && Option.is_none page_two.next_cursor
  ; update_retry_exact =
      Agent_protocol.Organization_group.Collection.equal updated update_retry
      && Int64.equal updated.revision 1L
  ; stale_revision_error = stale_revision_error.code
  ; invalidated_cursor_error = invalidated_cursor_error.code
  ; receipt_matches
  ; delete_retry_exact =
      Agent_protocol.Organization_result.Collection_deleted.equal deleted delete_retry
      && Int64.equal deleted.revision 2L
  ; deleted_get_error = deleted_get_error.code
  ; deleted_list_empty = List.is_empty final_page.items
  ; historical_retry_exact =
      Agent_protocol.Organization_group.Collection.equal first (create create_request)
  }
;;

let require_organization_observation observation =
  if
    not
      (observation.create_retry_exact
       && observation.get_exact
       && observation.pages_complete_in_order
       && observation.update_retry_exact
       && observation.receipt_matches
       && observation.delete_retry_exact
       && observation.deleted_list_empty
       && observation.historical_retry_exact
       && Agent_protocol.Error.equal_code
            observation.changed_key_error
            Idempotency_conflict
       && Agent_protocol.Error.equal_code observation.stale_revision_error Conflict
       && Agent_protocol.Error.equal_code observation.invalidated_cursor_error Conflict
       && Agent_protocol.Error.equal_code
            observation.deleted_get_error
            Organization_not_found)
  then
    raise_s
      [%sexp
        "organization conformance invariants failed"
      , (observation : organization_observation)]
;;

let test_organization_crud env environment =
  let fixture = fixture env environment "conformance-organization" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let observe connection key_prefix =
             let initialized = initialize connection in
             let project =
               project_observation connection ~host_id:initialized.server_id ~key_prefix
             in
             let collection =
               collection_observation
                 connection
                 ~host_id:initialized.server_id
                 ~key_prefix
             in
             require_organization_observation project;
             require_organization_observation collection;
             project, collection
           in
           let baseline = observe unix "unix" in
           List.iter
             [ http, "http"; stdio_unix, "stdio-unix"; stdio_http, "stdio-http" ]
             ~f:(fun (connection, prefix) ->
               let actual = observe connection prefix in
               if
                 not
                   ([%equal: organization_observation * organization_observation]
                      baseline
                      actual)
               then
                 raise_s
                   [%sexp
                     "cross-transport organization semantics differ"
                   , (baseline : organization_observation * organization_observation)
                   , (actual : organization_observation * organization_observation)]))))
;;

let organization_sessions_empty connection =
  let list_request =
    Agent_protocol.Session.List_request.
      { organization = Agent_protocol.Session_organization.Query.default
      ; page = page_request ()
      ; desired_state = None
      ; prompt_id = None
      ; workspace_id = None
      ; owner_principal_id = None
      ; creator_principal_id = None
      ; active_owner_principal_id = None
      ; sort = Agent_protocol.Session_catalog_query.Sort.default
      ; archive = Active
      ; labels = []
      }
  in
  match request connection (Session_list list_request) with
  | Session_list page -> List.is_empty page.items
  | _ -> fail "session.list returned the wrong result variant"
;;

let organization_public_client ~sw env fixture =
  Agent_transport_http.Client.connect
    ~sw
    ~env
    ~uri:
      (Uri.of_string (sprintf "http://127.0.0.1:%d" (Config_fixture.http_port fixture)))
    ~bearer_token:(Some (Config_fixture.public_token fixture))
    ~notification_capacity:1024
  |> protocol_ok
  |> client_of_connection
;;

let test_organization_authority_reopen env environment =
  let fixture = fixture env environment "conformance-organization-reopen" in
  Config_fixture.grant_public_scopes fixture [ View_organization; Manage_organization ];
  Eio.Switch.run (fun sw ->
    let host_id, project, project_request, collection, collection_request =
      with_daemon ~sw env fixture (fun _daemon _health ->
        let admin = unix_client ~sw env fixture in
        let owner = organization_public_client ~sw env fixture in
        Exn.protect
          ~finally:(fun () -> close_clients [ owner; admin ])
          ~f:(fun () ->
            let host_id = (initialize admin).server_id in
            ignore (initialize owner);
            if not (organization_sessions_empty admin)
            then fail "organization fixture starts with sessions";
            let foreign_request =
              Agent_protocol.Organization_request.Create.
                { host_id
                ; name = organization_name "Private admin group"
                ; idempotency_key = idempotency_key "org-foreign"
                }
            in
            let foreign =
              match request admin (Project_create foreign_request) with
              | Project_create group -> group
              | _ -> assert false
            in
            let denied = request_error owner (Project_get { host_id; id = foreign.id }) in
            if not (Agent_protocol.Error.equal_code denied.code Organization_not_found)
            then fail "organization owner can inspect another creator's group";
            (match command_receipt owner (Project_create foreign_request) with
             | Missing -> ()
             | _ -> fail "organization receipt discloses another principal's mutation");
            let project_request =
              Agent_protocol.Organization_request.Create.
                { host_id
                ; name = organization_name "Persistent project"
                ; idempotency_key = idempotency_key "org-reopen-project"
                }
            in
            let project =
              match request owner (Project_create project_request) with
              | Project_create group -> group
              | _ -> assert false
            in
            let collection_request =
              { project_request with
                name = organization_name "Persistent collection"
              ; idempotency_key = idempotency_key "org-reopen-collection"
              }
            in
            let collection =
              match request owner (Collection_create collection_request) with
              | Collection_create group -> group
              | _ -> assert false
            in
            let page =
              match
                request
                  owner
                  (Project_list
                     { host_id; creator_principal_id = None; page = page_request () })
              with
              | Project_list page -> page
              | _ -> assert false
            in
            if
              not
                (List.equal
                   Agent_protocol.Organization_group.Project.equal
                   [ project ]
                   page.items)
            then fail "organization list includes another creator's private group";
            let session_request =
              Agent_protocol.Session.List_request.
                { organization = Agent_protocol.Session_organization.Query.default
                ; page = page_request ()
                ; desired_state = None
                ; prompt_id = None
                ; workspace_id = None
                ; owner_principal_id = None
                ; creator_principal_id = None
                ; active_owner_principal_id = None
                ; sort = Agent_protocol.Session_catalog_query.Sort.default
                ; archive = Active
                ; labels = []
                }
            in
            let denied_session = request_error owner (Session_list session_request) in
            if not (Agent_protocol.Error.equal_code denied_session.code Permission_denied)
            then fail "organization grants imply session visibility";
            if not (organization_sessions_empty admin)
            then fail "organization CRUD activates sessions";
            host_id, project, project_request, collection, collection_request))
    in
    with_daemon ~sw env fixture (fun _daemon _health ->
      let admin = unix_client ~sw env fixture in
      let owner = organization_public_client ~sw env fixture in
      Exn.protect
        ~finally:(fun () -> close_clients [ owner; admin ])
        ~f:(fun () ->
          if not (Agent_protocol.Id.Server.equal host_id (initialize owner).server_id)
          then fail "organization host identity changed on reopen";
          ignore (initialize admin);
          let project_get =
            match request owner (Project_get { host_id; id = project.id }) with
            | Project_get group -> group
            | _ -> assert false
          in
          let project_retry =
            match request owner (Project_create project_request) with
            | Project_create group -> group
            | _ -> assert false
          in
          let collection_get =
            match request owner (Collection_get { host_id; id = collection.id }) with
            | Collection_get group -> group
            | _ -> assert false
          in
          let collection_retry =
            match request owner (Collection_create collection_request) with
            | Collection_create group -> group
            | _ -> assert false
          in
          if
            not
              (Agent_protocol.Organization_group.Project.equal project project_get
               && Agent_protocol.Organization_group.Project.equal project project_retry
               && Agent_protocol.Organization_group.Collection.equal
                    collection
                    collection_get
               && Agent_protocol.Organization_group.Collection.equal
                    collection
                    collection_retry
               && organization_sessions_empty admin)
          then fail "organization authority or receipt changed on durable reopen")))
;;

let test_organization_missing_scopes env environment =
  let fixture = fixture env environment "conformance-organization-scopes" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      let public = organization_public_client ~sw env fixture in
      Exn.protect ~finally:public.close ~f:(fun () ->
        let host_id = (initialize public).server_id in
        let create_request =
          Agent_protocol.Organization_request.Create.
            { host_id
            ; name = organization_name "Denied"
            ; idempotency_key = idempotency_key "organization-denied"
            }
        in
        let list_request =
          Agent_protocol.Organization_request.List.
            { host_id; creator_principal_id = None; page = page_request () }
        in
        List.iter
          [ Agent_protocol.Command.Project_create create_request
          ; Collection_create create_request
          ; Project_list list_request
          ; Collection_list list_request
          ]
          ~f:(fun command ->
            let denied = request_error public command in
            if not (Agent_protocol.Error.equal_code denied.code Permission_denied)
            then
              raise_s
                [%sexp
                  "missing organization scope was not denied"
                , (command : Agent_protocol.Command.t)
                , (denied : Agent_protocol.Error.t)]))))
;;

type membership_observation =
  { revision_once : bool
  ; retry_exact : bool
  ; stale_error : Agent_protocol.Error.code
  ; raw_visible : bool
  ; effective_visible : bool
  ; tombstone_effective_removed : bool
  ; retained_no_op : bool
  ; receipt_matches : bool
  }
[@@deriving equal, sexp]

let membership_observation connection ~key_prefix =
  let host_id = (initialize connection).server_id in
  let create =
    Agent_protocol.Organization_request.Create.
      { host_id
      ; name = organization_name "Membership"
      ; idempotency_key = idempotency_key (key_prefix ^ ":project")
      }
  in
  let project =
    match request connection (Project_create create) with
    | Project_create group -> group
    | _ -> assert false
  in
  let collection =
    match
      request
        connection
        (Collection_create
           { create with idempotency_key = idempotency_key (key_prefix ^ ":collection") })
    with
    | Collection_create group -> group
    | _ -> assert false
  in
  let created, _ = create_session connection ~key:(key_prefix ^ ":session") in
  let attachment = (Option.value_exn created.attachment).attachment in
  let patch =
    Agent_protocol.Session_organization.Patch.create
      ~project:(Set project.id)
      ~add_collections:[ collection.id ]
      ~remove_collections:[]
    |> protocol_ok
  in
  let update_request =
    Agent_protocol.Session_organization.Request.
      { host_id
      ; session_id = created.session.id
      ; attachment_id = attachment.id
      ; expected_metadata_revision = 0L
      ; patch
      ; idempotency_key = idempotency_key (key_prefix ^ ":membership")
      }
  in
  let update request_ =
    match request connection (Session_update_organization request_) with
    | Session_update_organization result -> result
    | _ -> assert false
  in
  let original = update update_request in
  let retry = update update_request in
  let stale_error =
    request_error
      connection
      (Session_update_organization
         { update_request with idempotency_key = idempotency_key (key_prefix ^ ":stale") })
  in
  let catalog () =
    let organization =
      Agent_protocol.Session_organization.Query.create
        ~project:Any
        ~collection_all_of:[ collection.id ]
      |> protocol_ok
    in
    let query =
      Agent_protocol.Session.List_request.
        { organization
        ; page = page_request ()
        ; desired_state = None
        ; prompt_id = None
        ; workspace_id = None
        ; owner_principal_id = None
        ; creator_principal_id = None
        ; active_owner_principal_id = None
        ; labels = []
        ; sort = Agent_protocol.Session_catalog_query.Sort.default
        ; archive = All
        }
    in
    match request connection (Session_list query) with
    | Session_list { items = [ entry ]; _ } -> entry
    | _ -> fail "membership catalog does not select exactly one session"
  in
  let before = catalog () in
  ignore
    (request
       connection
       (Project_delete
          { host_id
          ; id = project.id
          ; expected_revision = 0L
          ; idempotency_key = idempotency_key (key_prefix ^ ":delete")
          }));
  let after = catalog () in
  let no_op =
    update
      { update_request with
        expected_metadata_revision = 1L
      ; idempotency_key = idempotency_key (key_prefix ^ ":no-op")
      }
  in
  let receipt_matches =
    match command_receipt connection (Session_update_organization update_request) with
    | Committed (Session_mutation { session_id; _ }) ->
      Agent_protocol.Id.Session.equal session_id created.session.id
    | _ -> false
  in
  { revision_once = Int64.equal original.session.metadata_revision 1L
  ; retry_exact =
      Document_schema.Json.equal
        (Agent_protocol.Method_result.to_json (Session_update_organization original))
        (Agent_protocol.Method_result.to_json (Session_update_organization retry))
  ; stale_error = stale_error.code
  ; raw_visible =
      Agent_protocol.Session_organization.Values.equal
        before.session.organization
        original.session.organization
  ; effective_visible =
      Option.equal
        Agent_protocol.Id.Project.equal
        before.effective_organization.project_id
        (Some project.id)
  ; tombstone_effective_removed =
      Option.is_none after.effective_organization.project_id
      && List.equal
           Agent_protocol.Id.Collection.equal
           after.effective_organization.collection_ids
           [ collection.id ]
  ; retained_no_op = Int64.equal no_op.session.metadata_revision 1L
  ; receipt_matches
  }
;;

let test_session_organization env environment =
  let fixture = fixture env environment "conformance-session-organization" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let baseline = membership_observation unix ~key_prefix:"unix" in
           if
             not
               (baseline.revision_once
                && baseline.retry_exact
                && baseline.raw_visible
                && baseline.effective_visible
                && baseline.tombstone_effective_removed
                && baseline.retained_no_op
                && baseline.receipt_matches
                && Agent_protocol.Error.equal_code baseline.stale_error Conflict)
           then
             raise_s
               [%sexp
                 "membership conformance invariants failed"
               , (baseline : membership_observation)];
           List.iter
             [ http, "http"; stdio_unix, "stdio-unix"; stdio_http, "stdio-http" ]
             ~f:(fun (client, key_prefix) ->
               let actual = membership_observation client ~key_prefix in
               if not (equal_membership_observation baseline actual)
               then
                 raise_s
                   [%sexp
                     "cross-transport membership semantics differ"
                   , (baseline : membership_observation)
                   , (actual : membership_observation)]))))
;;

let test_permissions_grants env environment =
  let fixture = fixture env environment "conformance-security" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let baseline = security_observation unix ~key_prefix:"unix" in
           require_equal_security baseline (security_observation http ~key_prefix:"http");
           require_equal_security
             baseline
             (security_observation stdio_unix ~key_prefix:"stdio-unix");
           require_equal_security
             baseline
             (security_observation stdio_http ~key_prefix:"stdio-http"))))
;;

let test_jobs_schedules env environment =
  let fixture = fixture env environment "conformance-jobs" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let baseline = jobs_schedules_observation unix ~key_prefix:"unix" in
           require_equal_jobs_schedules
             baseline
             (jobs_schedules_observation http ~key_prefix:"http");
           require_equal_jobs_schedules
             baseline
             (jobs_schedules_observation stdio_unix ~key_prefix:"stdio-unix");
           require_equal_jobs_schedules
             baseline
             (jobs_schedules_observation stdio_http ~key_prefix:"stdio-http"))))
;;

let test_blob_read env environment =
  let fixture = fixture env environment "conformance-blob" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let baseline = blob_observation unix ~key_prefix:"unix" in
           require_equal_blob baseline (blob_observation http ~key_prefix:"http");
           require_equal_blob
             baseline
             (blob_observation stdio_unix ~key_prefix:"stdio-unix");
           require_equal_blob
             baseline
             (blob_observation stdio_http ~key_prefix:"stdio-http"))))
;;

let test_error_codes env environment =
  let fixture = fixture env environment "conformance-errors" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let baseline = error_observations unix ~key_prefix:"unix" in
           require_equal_errors baseline (error_observations http ~key_prefix:"http");
           require_equal_errors
             baseline
             (error_observations stdio_unix ~key_prefix:"stdio-unix");
           require_equal_errors
             baseline
             (error_observations stdio_http ~key_prefix:"stdio-http"))))
;;

let test_event_order env environment =
  let fixture = fixture env environment "conformance-event-order" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let baseline = event_order_observation env unix ~key_prefix:"unix" in
           require_equal_event_order
             baseline
             (event_order_observation env http ~key_prefix:"http");
           require_equal_event_order
             baseline
             (event_order_observation env stdio_unix ~key_prefix:"stdio-unix");
           require_equal_event_order
             baseline
             (event_order_observation env stdio_http ~key_prefix:"stdio-http"))))
;;

let test_visibility env environment =
  let fixture = fixture env environment "conformance-visibility" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_pair_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let observe_pair (writer, reader) key_prefix =
             visibility_observation env writer reader ~key_prefix
           in
           let baseline = observe_pair unix "unix" in
           require_equal_visibility baseline (observe_pair http "http");
           require_equal_visibility baseline (observe_pair stdio_unix "stdio-unix");
           require_equal_visibility baseline (observe_pair stdio_http "stdio-http"))))
;;

let test_history_deletion env environment =
  let fixture = fixture env environment "conformance-history-deletion" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_pair_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let observe (writer, reader) key_prefix =
             history_deletion_observation env writer reader ~key_prefix
           in
           let baseline = observe unix "unix" in
           require_equal_history_deletion baseline (observe http "http");
           require_equal_history_deletion baseline (observe stdio_unix "stdio-unix");
           require_equal_history_deletion baseline (observe stdio_http "stdio-http"))))
;;

module History_editing = struct
  module P = Agent_protocol
  module A = Agent_session

  type seed =
    { session_id : P.Id.Session.t
    ; target_id : P.History.Id.t
    ; prefix_ids : P.History.Id.t list
    }

  type t =
    { content_revision : int64
    ; revision_delta : int64
    ; stable_id : bool
    ; retired_suffix : bool
    ; initial_unchanged : bool
    ; retry_unchanged : bool
    ; receipt_matches : bool
    ; stale_revision : P.Error.code
    ; stale_content : P.Error.code
    ; stale_generation : P.Error.code
    ; read_only : P.Error.code
    ; stopped_continuation : bool
    ; continue_receipt : bool
    ; stopped_without_operation : bool
    }
  [@@deriving equal, sexp]

  let preseed env fixture =
    let seeds = ref [] in
    let options =
      Daemon_host.with_offline_inference Agent_server.Daemon.default_options
    in
    let options =
      { options with
        inference_policy =
          { options.inference_policy with
            select_inference_profile =
              (fun ~current ~profile ->
                if String.equal profile (Inference.Request.Target.profile current)
                then Ok current
                else Error Inference_runtime.Preparation_error.Target_unavailable)
          }
      }
    in
    Daemon_host.with_ env fixture ~options (fun sw daemon ->
      let connection = http_client ~sw env fixture in
      Exn.protect ~finally:connection.close ~f:(fun () ->
        ignore (initialize connection : P.Initialize.Response.t);
        List.iter [ "unix"; "http"; "stdio-unix"; "stdio-http" ] ~f:(fun label ->
          let created, _ = create_session connection ~key:("history-seed:" ^ label) in
          let writer = attached_writer created in
          let entry =
            Agent_server.Session_registry.load
              (Agent_server.Daemon.registry daemon)
              created.session.id
            |> protocol_ok
          in
          let before = A.Session_actor.state entry.actor |> protocol_ok in
          let prefix = before.conversation.canonical_history in
          if List.length prefix < before.conversation.initial_prompt_entry_count
          then fail "seed initial prompt prefix is incomplete";
          let reserved =
            A.Session_actor.reserve_history_block entry.actor ~count:2 |> protocol_ok
          in
          let make offset text =
            let id =
              History_entry.Id.create
                ~namespace:(P.Id.Session.to_string created.session.id)
                ~sequence:(Int64.to_int_exn reserved.first_sequence + offset)
              |> Result.ok_or_failwith
            in
            A.History_codec.user_text ~id text |> A.History_codec.to_protocol
          in
          let target = make 0 "ordinary retained user"
          and suffix = make 1 "obsolete suffix" in
          A.Session_actor.append_history
            entry.actor
            ~attachment_id:writer.id
            [ target; suffix ]
          |> protocol_ok
          |> ignore;
          seeds
          := !seeds
             @ [ { session_id = created.session.id
                 ; target_id = target.id
                 ; prefix_ids = List.map prefix ~f:(fun entry -> entry.P.History.id)
                 }
               ])));
    !seeds
  ;;

  let observe connection seed ~key_prefix =
    ignore (initialize connection : P.Initialize.Response.t);
    let attach mode suffix =
      match
        request_public
          connection
          (Session_attach
             { session_id = seed.session_id
             ; requested_mode = mode
             ; subscribe = false
             ; after_sequence = None
             ; reclaim_token = None
             ; idempotency_key = idempotency_key (key_prefix ^ suffix)
             })
      with
      | Session_attach result -> result.attachment
      | _ -> fail "history editing attachment variant"
    in
    let writer = attach Read_write ":writer" in
    let reader = attach Read_only ":reader" in
    let before = history_snapshot connection seed.session_id in
    let target =
      List.find_exn before.canonical_history.entries ~f:(fun entry ->
        P.History.Id.equal entry.id seed.target_id)
    in
    let edit =
      P.History_edit.create
        ~history_id:target.id
        ~expected_content_revision:target.content_revision
        ~text:"revised saved user"
        ~mode:Save_only
      |> protocol_ok
    in
    let intent : P.History_edit.Edit_request.t =
      { session_id = seed.session_id
      ; attachment_id = writer.id
      ; expected_generation = before.session.generation
      ; expected_revision = before.revision
      ; edit
      ; idempotency_key = idempotency_key (key_prefix ^ ":edit")
      }
    in
    let first = request connection (Session_edit_history intent) in
    let retry = request connection (Session_edit_history intent) in
    let result =
      match first with
      | Session_edit_history result -> result
      | _ -> fail "history edit result variant"
    in
    let after = history_snapshot connection seed.session_id in
    let current =
      List.find_exn after.canonical_history.entries ~f:(fun entry ->
        P.History.Id.equal entry.id seed.target_id)
    in
    let receipt_matches =
      match command_receipt connection (Session_edit_history intent) with
      | Committed (Edited_history receipt) ->
        P.Id.Session.equal receipt.session_id seed.session_id
        && P.History.Id.equal receipt.history_id seed.target_id
        && P.History.Content_revision.equal
             receipt.content_revision
             current.content_revision
        && Int64.equal receipt.mutation.revision result.mutation.revision
        && Int64.equal receipt.archived_revision result.archived_revision
        && P.History_edit.Continuation.equal receipt.continuation Not_requested
      | _ -> false
    in
    let denied suffix intent =
      (request_error
         connection
         (Session_edit_history
            { intent with idempotency_key = idempotency_key (key_prefix ^ suffix) }))
        .code
    in
    let stale_revision = denied ":stale-session" intent in
    let stale_content =
      denied ":stale-content" { intent with expected_revision = after.revision }
    in
    let stale_generation =
      denied
        ":stale-generation"
        { intent with
          expected_revision = after.revision
        ; expected_generation = after.session.generation + 1
        }
    in
    let fresh_edit =
      P.History_edit.create
        ~history_id:target.id
        ~expected_content_revision:current.content_revision
        ~text:"forbidden"
        ~mode:Save_only
      |> protocol_ok
    in
    let read_only =
      denied
        ":read-only"
        { intent with
          attachment_id = reader.id
        ; expected_revision = after.revision
        ; edit = fresh_edit
        }
    in
    let continuation_request : P.History_edit.Continue_request.t =
      { session_id = seed.session_id
      ; attachment_id = writer.id
      ; expected_generation = after.session.generation
      ; expected_revision = after.revision
      ; idempotency_key = idempotency_key (key_prefix ^ ":continue")
      }
    in
    let continuation =
      match request connection (Session_continue_history continuation_request) with
      | Session_continue_history result -> result
      | _ -> fail "history continuation result variant"
    in
    let continue_retry =
      request connection (Session_continue_history continuation_request)
    in
    let continue_receipt =
      match
        command_receipt connection (Session_continue_history continuation_request)
      with
      | Committed (Continued_history receipt) ->
        P.Id.Session.equal receipt.session_id seed.session_id
        && P.History_edit.Continuation.equal receipt.continuation (Not_started Stopped)
      | _ -> false
    in
    let final = history_snapshot connection seed.session_id in
    let prefix_count = List.length seed.prefix_ids in
    let initial_unchanged =
      List.equal
        P.Public.History.equal
        (List.take before.canonical_history.entries prefix_count)
        (List.take final.canonical_history.entries prefix_count)
    in
    let stopped_without_operation =
      match final.session.observed_state, final.session.active_operation with
      | Stopped, None -> Int64.equal final.revision after.revision
      | _ -> false
    in
    { content_revision = P.History.Content_revision.to_int64 current.content_revision
    ; revision_delta = Int64.(after.revision - before.revision)
    ; stable_id = P.History.Id.equal current.id target.id
    ; retired_suffix =
        Int.equal (List.length after.canonical_history.entries) (prefix_count + 1)
    ; initial_unchanged
    ; retry_unchanged =
        Document_schema.Json.equal
          (P.Method_result.to_json first)
          (P.Method_result.to_json retry)
        && Document_schema.Json.equal
             (P.Method_result.to_json (Session_continue_history continuation))
             (P.Method_result.to_json continue_retry)
    ; receipt_matches
    ; stale_revision
    ; stale_content
    ; stale_generation
    ; read_only
    ; stopped_continuation =
        P.History_edit.Continuation.equal continuation.continuation (Not_started Stopped)
    ; continue_receipt
    ; stopped_without_operation
    }
  ;;

  let run env environment =
    let fixture = fixture env environment "conformance-history-editing" in
    let seeds = preseed env fixture in
    Eio.Switch.run (fun sw ->
      with_daemon ~sw env fixture (fun _ _ ->
        with_transport_matrix
          ~sw
          env
          environment
          fixture
          (fun unix http stdio_unix stdio_http ->
             let observations =
               List.map
                 (List.zip_exn
                    [ unix, "unix"
                    ; http, "http"
                    ; stdio_unix, "stdio-unix"
                    ; stdio_http, "stdio-http"
                    ]
                    seeds)
                 ~f:(fun ((connection, key_prefix), seed) ->
                   observe connection seed ~key_prefix)
             in
             let expected =
               { content_revision = 1L
               ; revision_delta = 1L
               ; stable_id = true
               ; retired_suffix = true
               ; initial_unchanged = true
               ; retry_unchanged = true
               ; receipt_matches = true
               ; stale_revision = Conflict
               ; stale_content = Conflict
               ; stale_generation = Conflict
               ; read_only = Permission_denied
               ; stopped_continuation = true
               ; continue_receipt = true
               ; stopped_without_operation = true
               }
             in
             List.iter observations ~f:(fun actual ->
               if not (equal expected actual)
               then raise_s [%sexp "history editing conformance mismatch", (actual : t)]))))
  ;;
end

let provider_installed_observation client ~key_prefix =
  let module P = Agent_protocol in
  let module DTO = P.Provider_operator in
  let initialized = initialize ~features:[ "provider.operator" ] client in
  let info =
    match request client Server_info with
    | Server_info info -> info
    | _ -> fail "provider server info variant"
  in
  if
    (not (List.mem initialized.enabled_features "provider.operator" ~equal:String.equal))
    || not (List.mem info.features "provider.operator" ~equal:String.equal)
  then fail "installed application provider service omitted capability";
  let status () =
    match request client (Provider_status { profile = None }) with
    | Provider_status status -> status
    | _ -> fail "provider status variant"
  in
  let before = status () in
  if
    before.setup_required
    || not (P.Id.Server.equal before.server_id initialized.server_id)
  then fail "provisioned provider status lost application authority";
  let api_profile =
    DTO.Profile_id.of_string "first-party-openai-responses" |> protocol_ok
  in
  let selected =
    List.find before.profiles ~f:(fun profile ->
      DTO.Profile_id.equal profile.profile api_profile)
    |> Option.value_exn
  in
  if not (DTO.Status_result.equal_availability selected.availability Configured)
  then fail "protected synthetic provider binding is not configured";
  let profile = DTO.Profile_id.of_string "unknown-provider-profile" |> protocol_ok in
  let revision = DTO.Revision.of_string "selection-1" |> protocol_ok in
  let flow : DTO.Flow_ref.t =
    { server_id = initialized.server_id
    ; profile
    ; flow_id = DTO.Flow_id.of_string "nonexistent-provider-flow" |> protocol_ok
    ; expires_at = P.Timestamp.of_string "2099-01-01T00:00:00Z" |> protocol_ok
    }
  in
  let key name = idempotency_key (key_prefix ^ "-provider-" ^ name) in
  (* Fresh setup cannot adopt an existing incarnation. Other rejected methods
     exercise the installed service without starting OAuth or disabling a key. *)
  let commands : (P.Command.t * DTO.Error.t) list =
    [ Provider_setup { idempotency_key = key "setup" }, Submission_uncertain
    ; ( Provider_login_begin { profile; mode = Browser; idempotency_key = key "browser" }
      , Missing_profile )
    ; ( Provider_login_begin { profile; mode = Device; idempotency_key = key "device" }
      , Missing_profile )
    ; Provider_login_challenge { flow }, Flow_interrupted
    ; Provider_login_cancel { flow; idempotency_key = key "cancel" }, Flow_interrupted
    ; Provider_logout { profile; idempotency_key = key "logout" }, Missing_profile
    ; ( Provider_select
          { profile; expected_revision = revision; idempotency_key = key "select" }
      , Missing_profile )
    ; ( Provider_configure_environment
          { profile
          ; source = DTO.Source_id.of_string "openai-api-key" |> protocol_ok
          ; idempotency_key = key "environment"
          }
      , Denied )
    ]
  in
  let errors =
    List.map commands ~f:(fun (command, expected) ->
      let error = request_error client command in
      let fields = P.Json_codec.fields error.data |> protocol_ok in
      let provider_error =
        P.Json_codec.required_as fields "provider_error" DTO.Error.of_json |> protocol_ok
      in
      if (not (DTO.Error.equal provider_error expected)) || error.retryable
      then fail "installed provider refusal did not preserve its typed cause";
      P.Command.method_name command, DTO.Error.to_json provider_error |> Jsonaf.to_string)
  in
  let public_status = DTO.Status_result.to_json in
  if not (Jsonaf.exactly_equal (public_status before) (public_status (status ())))
  then fail "rejected provider command changed existing authority";
  ("provider.operator", "installed")
  :: ("provider.status", Jsonaf.to_string (public_status before))
  :: errors
;;

let test_provider_installed env environment =
  let fixture = fixture env environment "conformance-provider-installed" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (fun _daemon _health ->
      with_transport_matrix
        ~sw
        env
        environment
        fixture
        (fun unix http stdio_unix stdio_http ->
           let baseline = provider_installed_observation unix ~key_prefix:"unix" in
           List.iter
             [ http, "http"; stdio_unix, "stdio-unix"; stdio_http, "stdio-http" ]
             ~f:(fun (client, key_prefix) ->
               let actual = provider_installed_observation client ~key_prefix in
               if
                 not
                   (List.equal
                      (fun (a, b) (c, d) -> String.equal a c && String.equal b d)
                      baseline
                      actual)
               then fail "cross-transport installed provider semantics differ"))))
;;

let cases =
  [ "conformance.provider-installed", test_provider_installed
  ; "conformance.read-methods", test_read_methods
  ; "conformance.session-lifecycle", test_session_lifecycle
  ; "conformance.session-metadata", test_session_metadata
  ; "conformance.session-organization", test_session_organization
  ; "conformance.session-configuration", test_session_configuration
  ; "conformance.organization-crud", test_organization_crud
  ; "conformance.organization-authority-reopen", test_organization_authority_reopen
  ; "conformance.organization-missing-scopes", test_organization_missing_scopes
  ; "conformance.inference-reads", test_inference_reads
  ; "conformance.permissions-grants", test_permissions_grants
  ; "conformance.jobs-schedules", test_jobs_schedules
  ; "conformance.blob-read", test_blob_read
  ; "conformance.error-codes", test_error_codes
  ; "conformance.event-order", test_event_order
  ; "conformance.visibility", test_visibility
  ; "conformance.history-editing", History_editing.run
  ; "conformance.history-deletion", test_history_deletion
  ]
;;

let method_coverage =
  [ "provider.setup", "conformance.provider-installed"
  ; "provider.status", "conformance.provider-installed"
  ; "provider.login.begin", "conformance.provider-installed"
  ; "provider.login.challenge", "conformance.provider-installed"
  ; "provider.login.cancel", "conformance.provider-installed"
  ; "provider.logout", "conformance.provider-installed"
  ; "provider.select", "conformance.provider-installed"
  ; "provider.configure_environment", "conformance.provider-installed"
  ; "command.receipt", "conformance.session-lifecycle"
  ; "protocol.initialize", "conformance.read-methods"
  ; "protocol.ping", "conformance.read-methods"
  ; "server.info", "conformance.read-methods"
  ; "server.health", "conformance.read-methods"
  ; "prompt.list", "conformance.read-methods"
  ; "prompt.get", "conformance.read-methods"
  ; "workspace.list", "conformance.read-methods"
  ; "workspace.get", "conformance.read-methods"
  ; "project.create", "conformance.organization-crud"
  ; "project.get", "conformance.organization-crud"
  ; "project.list", "conformance.organization-crud"
  ; "project.update", "conformance.organization-crud"
  ; "project.delete", "conformance.organization-crud"
  ; "collection.create", "conformance.organization-crud"
  ; "collection.get", "conformance.organization-crud"
  ; "collection.list", "conformance.organization-crud"
  ; "collection.update", "conformance.organization-crud"
  ; "collection.delete", "conformance.organization-crud"
  ; "blob.read", "conformance.blob-read"
  ; "session.create", "conformance.session-lifecycle"
  ; "session.list", "conformance.session-lifecycle"
  ; "session.get", "conformance.session-lifecycle"
  ; "session.update_metadata", "conformance.session-metadata"
  ; "session.update_organization", "conformance.session-organization"
  ; "session.configuration_get", "conformance.session-configuration"
  ; "session.configuration_update", "conformance.session-configuration"
  ; "session.inference_summary", "conformance.inference-reads"
  ; "session.inference_observations", "conformance.inference-reads"
  ; "session.attach", "conformance.session-lifecycle"
  ; "session.detach", "conformance.session-lifecycle"
  ; "session.renew_owner", "conformance.error-codes"
  ; "session.start", "conformance.session-lifecycle"
  ; "session.stop", "conformance.session-lifecycle"
  ; "session.cancel_operation", "conformance.error-codes"
  ; "session.send_message", "conformance.error-codes"
  ; "session.compact", "conformance.error-codes"
  ; "session.edit_history", "conformance.history-editing"
  ; "session.continue_history", "conformance.history-editing"
  ; "session.delete_history", "conformance.history-deletion"
  ; "session.export", "conformance.blob-read"
  ; "session.reset", "conformance.error-codes"
  ; "session.rebuild", "conformance.error-codes"
  ; "session.upgrade_prompt", "conformance.error-codes"
  ; "session.delete", "conformance.error-codes"
  ; "permission.list", "conformance.permissions-grants"
  ; "permission.respond", "conformance.permissions-grants"
  ; "grant.list", "conformance.permissions-grants"
  ; "grant.revoke", "conformance.permissions-grants"
  ; "audit.read", "conformance.permissions-grants"
  ; "job.list", "conformance.jobs-schedules"
  ; "job.get", "conformance.jobs-schedules"
  ; "job.cancel", "conformance.jobs-schedules"
  ; "schedule.list", "conformance.jobs-schedules"
  ; "schedule.get", "conformance.jobs-schedules"
  ; "schedule.create", "conformance.jobs-schedules"
  ; "schedule.cancel", "conformance.jobs-schedules"
  ; "ingress.submit", "conformance.error-codes"
  ]
;;

let require_complete_method_coverage () =
  let expected = String.Set.of_list Agent_protocol.Command.supported_methods in
  let covered = String.Set.of_list (List.map method_coverage ~f:fst) in
  let missing = Set.diff expected covered |> Set.to_list in
  let unknown = Set.diff covered expected |> Set.to_list in
  if not (List.is_empty missing && List.is_empty unknown)
  then
    raise_s
      [%sexp
        "conformance method coverage differs from the closed protocol"
      , { missing : string list; unknown : string list }]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown conformance case", (name : string)])
;;

let run env ~case =
  require_complete_method_coverage ();
  Temporary_environment.with_
    ~scenario:"cross-transport-conformance"
    ~env
    (fun environment ->
       let selected = select case in
       List.iter selected ~f:(fun (_name, test) -> test env environment);
       print_s
         [%sexp
           { scenario = ("cross-transport-conformance" : string)
           ; selected_case = (case : string option)
           ; passed_cases = (List.map selected ~f:fst : string list)
           }])
;;
