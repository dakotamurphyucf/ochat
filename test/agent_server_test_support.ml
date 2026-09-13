open! Core

(* Keep actual polling/I/O waits while controlling the time observed by durable
   deadline bookkeeping. Resuming excludes time spent paused; it never jumps
   past deadlines merely because fixture work was slow. *)
let controlled_monotonic_clock real_clock =
  let logical_now = ref (Eio.Time.Mono.now real_clock) in
  let last_real = ref !logical_now in
  let paused = ref false in
  let now () =
    let actual = Eio.Time.Mono.now real_clock in
    (match !paused with
     | true -> ()
     | false ->
       logical_now
       := Mtime.add_span !logical_now (Mtime.span !last_real actual) |> Option.value_exn);
    last_real := actual;
    !logical_now
  in
  let module Clock = struct
    type t = unit
    type time = Mtime.t

    let now = now

    let sleep_until () deadline =
      let current = now () in
      match Mtime.compare deadline current <= 0 with
      | true -> Eio.Fiber.yield ()
      | false -> Eio.Time.Mono.sleep_span real_clock (Mtime.span current deadline)
    ;;
  end
  in
  let pause () =
    ignore (now ());
    paused := true
  in
  let resume () =
    ignore (now ());
    paused := false
  in
  let advance seconds =
    let span = Mtime.Span.of_float_ns (seconds *. 1_000_000_000.) |> Option.value_exn in
    logical_now := Mtime.add_span (now ()) span |> Option.value_exn
  in
  Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock)), pause, resume, advance
;;

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let observed_idle = function
  | Agent_protocol.Session.Idle -> true
  | Stopped
  | Queued_for_slot
  | Starting
  | Recovering
  | Running_turn _
  | Compacting _
  | Waiting_for_permission _
  | Stopping
  | Failed _ -> false
;;

let temporary_root env =
  let name =
    Agent_protocol.Id.Transaction.create ()
    |> Agent_protocol.Id.Transaction.to_string
    |> fun value -> "ochat-restart-test-" ^ value
  in
  let path = Filename.concat "/tmp" name in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / path);
  path
;;

let permission_profile =
  Agent_server.Config.Permission_profile.
    { id = "restart.permission"
    ; tool_default = Allow
    ; approval_timeout_ms = None
    ; approval_fallback = Deny
    ; manifest_authorization = Assume_authorized
    }
;;

let config
      ?(profile = permission_profile)
      ?(manifest_grants = [])
      root
      workspace
      prompt_file
  =
  Agent_server.Config.
    { version = current_version
    ; source_file = Filename.concat root "server.sexp"
    ; server =
        { data_dir = Filename.concat root "data"
        ; session_helpers = []
        ; authoring_packages = []
        ; authoring_budget = None
        ; unix_socket = Filename.concat root "agent.sock"
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
    ; workspaces =
        [ { id = "restart.workspace"
          ; source = Physical workspace
          ; access = Shared_write
          ; conflict_domain = None
          ; prompt_limits =
              [ { prompt = "restart.prompt"; max_root_agents = 1; overflow = Reject } ]
          }
        ]
    ; prompts =
        [ { id = "restart.prompt"
          ; path = prompt_file
          ; description = None
          ; allowed_workspaces = [ "restart.workspace" ]
          ; permission_profile = profile.id
          ; runtime_policy = None
          ; enabled = true
          }
        ]
    ; permission_profiles = [ profile ]
    ; manifest_grants
    }
;;

let scopes =
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

let principal_with_scopes id scopes =
  Agent_protocol.Principal.create
    ~id:(Agent_protocol.Id.Principal.of_string id |> protocol_ok)
    ~authentication_kind:"test"
    ~scopes
    ~attributes:[]
  |> protocol_ok
;;

let principal_with_id id = principal_with_scopes id scopes
let principal () = principal_with_id "pri_restart_test"

let connection daemon principal =
  let notifications = Eio.Stream.create 256 in
  let context =
    Agent_server.Connection_context.create
      ~connection_id:
        (Agent_protocol.Id.Attachment.create () |> Agent_protocol.Id.Attachment.to_string)
      ~principal
      ~transport:In_memory
      ~publish_notification:(Eio.Stream.add notifications)
      ~max_attachments:64
  in
  Agent_client.In_memory.create
    ~request:(fun command ->
      Agent_server.Dispatcher.dispatch_command
        (Agent_server.Daemon.dispatcher daemon)
        ~context
        command)
    ~notifications
    ~close:(fun () -> Agent_server.Daemon.close_connection daemon context)
;;

let initialize connection =
  Agent_client.Session_handle.initialize
    connection
    ~implementation_name:"restart-test"
    ~implementation_version:"dev"
  |> protocol_ok
  |> ignore
;;

let session_spec
      ?(start_immediately = false)
      ?(liveness = Agent_protocol.Session.Detached)
      ()
  =
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog (Agent_server.Catalog_identity.prompt_definition "restart.prompt"))
    ~workspace:
      (Configured (Agent_server.Catalog_identity.workspace_definition "restart.workspace"))
    ~liveness
    ~persistence:Durable
    ~permission_profile:permission_profile.id
    ~start_immediately
    ~labels:[ "suite", "restart" ]
    ()
  |> protocol_ok
;;

let create_request ?(start_immediately = false) ?(key = "restart-create") () =
  let idempotency_key = Agent_protocol.Idempotency_key.of_string key |> protocol_ok in
  Agent_protocol.Session.Create_request.
    { spec = session_spec ~start_immediately ()
    ; requested_mode = Some Read_write
    ; subscribe = false
    ; idempotency_key
    }
;;

let create_session ?(start_immediately = false) ?(key = "restart-create") connection =
  Agent_client.Connection.request
    connection
    (Session_create (create_request ~start_immediately ~key ()))
  |> protocol_ok
  |> function
  | Agent_protocol.Method_result.Session_create result ->
    result.session, (Option.value_exn result.attachment).attachment
  | _ -> failwith "unexpected create response"
;;
