open Core
module Config_fixture = Support.Config_fixture
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
      -> (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result
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

let request client command = client.request command |> protocol_ok

let request_error client command =
  match client.request command with
  | Error error -> error
  | Ok result ->
    raise_s
      [%sexp
        "protocol operation unexpectedly succeeded"
      , (result : Agent_protocol.Method_result.t)]
;;

let initialize client =
  let implementation =
    Agent_protocol.Initialize.Implementation.create
      ~name:"agent-server-e2e-conformance"
      ~version:"dev"
    |> protocol_ok
  in
  let initialize_request =
    Agent_protocol.Initialize.Request.create
      ~implementation
      ~protocol_min:Agent_protocol.Version.initial
      ~protocol_max:Agent_protocol.Version.initial
      ~features:[]
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
  Eio.Switch.run (fun sw ->
    let embedded =
      Agent_server.Embedded.start
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
    match request connection (Session_create create_request) with
    | Session_create created -> created
    | _ -> fail "session.create returned the wrong result variant"
  in
  create (), create ()
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
  match request connection (Session_attach attach_request) with
  | Session_attach ({ replay = Events _; _ } as attached) -> attached
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
      { page = page_request ()
      ; desired_state = None
      ; prompt_id = None
      ; workspace_id = None
      ; owner_principal_id = None
      ; labels = []
      }
  in
  match request connection (Session_list list_request) with
  | Session_list page ->
    List.exists page.items ~f:(fun session ->
      Agent_protocol.Id.Session.compare session.Agent_protocol.Session.id session_id = 0)
  | _ -> fail "session.list returned the wrong result variant"
;;

let get_matches_session connection session_id =
  match request connection (Session_get { session_id; history = None }) with
  | Session_get snapshot ->
    Agent_protocol.Id.Session.compare snapshot.session.id session_id = 0
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
    Int64.equal event.Agent_protocol.Event.Durable.sequence (Int64.of_int (index + 1)))
;;

let lifecycle_observation connection ~key_prefix =
  ignore (initialize connection : Agent_protocol.Initialize.Response.t);
  let created, duplicate = create_session connection ~key:(key_prefix ^ ":create") in
  let attachment =
    Option.value_exn created.attachment
    |> fun (attached : Agent_protocol.Method_result.Attach.t) -> attached.attachment
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
  ; replay_visibilities = List.map events ~f:(fun event -> event.visibility)
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
    |> fun (attached : Agent_protocol.Method_result.Attach.t) -> attached.attachment
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
    |> fun (attached : Agent_protocol.Method_result.Attach.t) -> attached.attachment
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
    |> fun (attached : Agent_protocol.Method_result.Attach.t) -> attached.attachment
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
  match request connection (Session_attach attach_request) with
  | Session_attach attached -> attached.attachment
  | _ -> fail "session.attach returned the wrong result variant"
;;

let error_observations connection ~key_prefix =
  ignore (initialize connection : Agent_protocol.Initialize.Response.t);
  let created, _duplicate = create_session connection ~key:(key_prefix ^ ":create") in
  let writer =
    Option.value_exn created.attachment
    |> fun (attached : Agent_protocol.Method_result.Attach.t) -> attached.attachment
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
  match request connection (Session_create create_request) with
  | Session_create created -> created
  | _ -> fail "session.create returned the wrong result variant"
;;

let durable_notification = function
  | Agent_protocol.Envelope.Notification { method_ = "session.event"; params } ->
    Agent_protocol.Event.Durable.of_json params |> protocol_ok |> Option.some
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
        Int64.(event.Agent_protocol.Event.Durable.sequence - base_sequence)
    ; relative_revision = Int64.(event.revision - base_revision)
    ; kind = event.kind
    ; visibility = event.visibility
    })
;;

let attached_writer created =
  Option.value_exn created.Agent_protocol.Method_result.Create.attachment
  |> fun (attached : Agent_protocol.Method_result.Attach.t) -> attached.attachment
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
          event.Agent_protocol.Event.Durable.sequence
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
  let through = (List.last_exn writer_events).Agent_protocol.Event.Durable.sequence in
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
  match request connection (Session_get { session_id; history = None }) with
  | Session_get snapshot -> snapshot
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
         (Agent_protocol.Snapshot.to_json before)
         (Agent_protocol.Snapshot.to_json after))
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
  let session_id = before.Agent_protocol.Snapshot.session.id in
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
    List.filter before.Agent_protocol.Snapshot.canonical_history.entries ~f:(fun entry ->
      Agent_protocol.History.Id.compare entry.id id <> 0)
  in
  let actual = after.Agent_protocol.Snapshot.canonical_history.entries in
  if
    not
      (Poly.equal
         (List.map expected ~f:Agent_protocol.History.entry_to_json)
         (List.map actual ~f:Agent_protocol.History.entry_to_json))
  then fail "history deletion did not remove exactly the selected canonical occurrence";
  expected
;;

let require_history_replacement events expected =
  let windows =
    List.filter_map events ~f:(fun event ->
      match
        Agent_protocol.Event.Durable.Payload.of_json
          ~kind:event.Agent_protocol.Event.Durable.kind
          event.payload
        |> protocol_ok
      with
      | History_replaced window -> Some window
      | _ -> None)
  in
  match windows with
  | [ window ]
    when Poly.equal
           (List.map window.entries ~f:Agent_protocol.History.entry_to_json)
           (List.map expected ~f:Agent_protocol.History.entry_to_json) -> ()
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

let cases =
  [ "conformance.read-methods", test_read_methods
  ; "conformance.session-lifecycle", test_session_lifecycle
  ; "conformance.permissions-grants", test_permissions_grants
  ; "conformance.jobs-schedules", test_jobs_schedules
  ; "conformance.blob-read", test_blob_read
  ; "conformance.error-codes", test_error_codes
  ; "conformance.event-order", test_event_order
  ; "conformance.visibility", test_visibility
  ; "conformance.history-deletion", test_history_deletion
  ]
;;

let method_coverage =
  [ "protocol.initialize", "conformance.read-methods"
  ; "protocol.ping", "conformance.read-methods"
  ; "server.info", "conformance.read-methods"
  ; "server.health", "conformance.read-methods"
  ; "prompt.list", "conformance.read-methods"
  ; "prompt.get", "conformance.read-methods"
  ; "workspace.list", "conformance.read-methods"
  ; "workspace.get", "conformance.read-methods"
  ; "blob.read", "conformance.blob-read"
  ; "session.create", "conformance.session-lifecycle"
  ; "session.list", "conformance.session-lifecycle"
  ; "session.get", "conformance.session-lifecycle"
  ; "session.attach", "conformance.session-lifecycle"
  ; "session.detach", "conformance.session-lifecycle"
  ; "session.renew_owner", "conformance.error-codes"
  ; "session.start", "conformance.session-lifecycle"
  ; "session.stop", "conformance.session-lifecycle"
  ; "session.cancel_operation", "conformance.error-codes"
  ; "session.send_message", "conformance.error-codes"
  ; "session.compact", "conformance.error-codes"
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
