open! Core

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

let job_delivered = function
  | Agent_protocol.Job.Delivered _ -> true
  | Not_required | Pending -> false
;;

let reset_session connection session attachment =
  Agent_client.Connection.request
    connection
    (Session_reset
       { session_id = session.Agent_protocol.Session.id
       ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
       ; expected_revision = session.revision
       ; keep_history = false
       ; keep_tasks = false
       ; keep_cache = false
       ; keep_workspace = true
       ; keep_grants = true
       ; keep_labels = true
       ; idempotency_key =
           Agent_protocol.Idempotency_key.of_string "restart-reset" |> protocol_ok
       })
  |> protocol_ok
  |> function
  | Agent_protocol.Method_result.Session_reset result -> result.session
  | _ -> failwith "unexpected reset response"
;;

let manifest_grant_count daemon session_id =
  Agent_server.Session_registry.find (Agent_server.Daemon.registry daemon) session_id
  |> Option.value_exn
  |> fun entry ->
  Agent_session.Session_actor.state entry.Agent_server.Session_registry.actor
  |> protocol_ok
  |> fun state -> List.length state.Agent_session.Session_state.shell.manifest_grants
;;

let grants connection session_id state =
  let page = Agent_protocol.Page.Request.create ~limit:100 () |> protocol_ok in
  Agent_client.Connection.request
    connection
    (Grant_list { page; session_id = Some session_id; principal_id = None; state })
  |> protocol_ok
  |> function
  | Agent_protocol.Method_result.Grant_list page -> page.items
  | _ -> failwith "unexpected grant list response"
;;

let shell_prompt =
  {|<developer>You are a restart test agent.</developer>
<shell_access id="direct" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false"
      child_processes="false" arbitrary_code="false" privilege_change="false">
    <read path="${workspace}"/>
  </capabilities>
  <backends merge="replace">
    <direct when="macos"/>
    <direct when="linux"/>
  </backends>
  <policy default="deny"/>
  <approvals provider="none" unavailable="deny" scopes="once"/>
  <audit format="none"/>
</shell_access>
<tool name="fixed_echo" type="shell" mode="fixed" runtime="direct"
    command="/bin/echo" result="stdout"/>|}
;;

let required_manifest_profile =
  Agent_server.Config.Permission_profile.
    { permission_profile with
      id = "restart.permission.required"
    ; manifest_authorization = Require_grant
    }
;;

let prompt_hashes daemon =
  let entry =
    Agent_session.Prompt_catalog.find_by_name
      (Agent_server.Daemon.prompts daemon)
      "restart.prompt"
    |> Option.value_exn
  in
  match entry.availability with
  | Ready revision ->
    let artifact = Agent_session.Prompt_revision.artifact revision in
    artifact.root_sha256, Option.value_exn artifact.shell_manifest_sha256
  | Disabled | Unavailable _ -> failwith "restart prompt did not compile"
;;

let operator_grant principal ~source_sha256 ~manifest_sha256 =
  Agent_server.Config.Manifest_grant.
    { id = "restart.operator.manifest"
    ; prompt = "restart.prompt"
    ; workspaces = [ "restart.workspace" ]
    ; manifest_sha256
    ; source_sha256
    ; principals = [ Agent_protocol.Id.Principal.to_string principal ]
    }
;;

let start_daemon sw env config root =
  Agent_server.Daemon.start
    ~sw
    ~env
    ~config
    ~tool_dir:root
    ~home:root
    ~process_start_identity:
      (Some
         (Agent_protocol.Id.Transaction.create ()
          |> Agent_protocol.Id.Transaction.to_string))
    ()
  |> protocol_ok
;;

let%expect_test
    "qualified moderator lifecycle uses daemon permissions and survives restart"
  =
  let module A = Agent_session.Session_actor in
  let module E = Agent_protocol.Moderator_execution in
  let module H = Agent_client.Session_handle in
  List.iter [ `Allow; `Ask; `Deny; `Cancel ] ~f:(fun mode ->
    Eio_main.run (fun env ->
      Mirage_crypto_rng_unix.use_default ();
      let root = temporary_root env in
      Exn.protect
        ~finally:(fun () ->
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
        ~f:(fun () ->
          let workspace = Filename.concat root "workspace" in
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / workspace / "value.txt")
            "approved value";
          let prompt_file = Filename.concat root "root.chatmd" in
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / prompt_file)
            {|<developer>Offline persisted lifecycle fixture.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let read = fun () -> Tool.call("read_file", `Object([{key = "root"; value = `String("data")}; {key = "file"; value = `String("value.txt")}]))
let advance = fun state amount -> Task.bind(read(), fun result -> match result with
  | `Error(code) -> Task.fail(code)
  | `Ok(value) -> Task.pure(state + amount))
let on_event = fun ctx state event -> match event with
  | `Session_start -> advance(state, 10)
  | `Session_resume -> advance(state, 100)
  | _ -> Task.pure(state)
</script>|};
          let profile =
            { permission_profile with
              tool_default =
                (match mode with
                 | `Allow -> Allow
                 | `Ask | `Cancel -> Ask
                 | `Deny -> Deny)
            }
          in
          let configuration = config ~profile root workspace prompt_file in
          let model_calls = ref 0 in
          let start sw =
            Agent_server.Daemon.start
              ~sw
              ~env
              ~config:configuration
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options:
                { Agent_server.Daemon.default_options with
                  qualify_chatml_extensions = true
                ; model_post_stream =
                    Some
                      (fun ~sw:_ ~inputs:_ ->
                        incr model_calls;
                        failwith "idle lifecycle must not call a provider")
                }
              ()
            |> protocol_ok
          in
          let entry daemon id =
            Agent_server.Session_registry.find (Agent_server.Daemon.registry daemon) id
            |> Option.value_exn
          in
          let snapshot_value state =
            match state.Agent_session.Session_state.moderator with
            | Some (`Object [ ("identity_snapshot_sexp", `String encoded) ]) ->
              let snapshot =
                Session.Moderator_state.Identity_snapshot.t_of_sexp
                  (Sexp.of_string encoded)
              in
              (match snapshot.current_state with
               | Session.Snapshot.Int value -> value
               | _ -> failwith "unexpected lifecycle state")
            | _ -> failwith "missing lifecycle checkpoint"
          in
          let await actor predicate =
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
              let rec loop () =
                let state = A.state actor |> protocol_ok in
                match predicate state with
                | true -> state
                | false ->
                  Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                  loop ()
              in
              loop ())
          in
          let finished phase state =
            List.exists
              state.Agent_session.Session_state.moderator_executions
              ~f:(fun receipt ->
                E.equal_phase receipt.context.phase phase
                &&
                match receipt.status with
                | Completed _ | Failed _ | Interrupted _ -> true
                | Running -> false)
          in
          let id, first_value, invocation_owned, permission_states =
            Eio.Switch.run (fun sw ->
              let daemon = start sw in
              let client = connection daemon (principal ()) in
              initialize client;
              let created, _ = create_session client in
              let actor = (entry daemon created.id).actor in
              let stopped = A.state actor |> protocol_ok in
              assert (List.is_empty stopped.invocations);
              assert (List.is_empty stopped.moderator_executions);
              let handle =
                H.attach
                  ~sw
                  ~clock:(Eio.Stdenv.clock env)
                  ~connection:client
                  ~session_id:created.id
                  ~mode:Read_write
                  ()
                |> protocol_ok
              in
              H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
              let invocation_owned =
                match mode with
                | `Allow | `Deny -> true
                | `Ask | `Cancel ->
                  let waiting =
                    await actor (fun state ->
                      List.exists state.permissions ~f:(fun permission ->
                        Agent_protocol.Permission.equal_state permission.state Pending))
                  in
                  let permission =
                    List.find_exn waiting.permissions ~f:(fun permission ->
                      Agent_protocol.Permission.equal_state permission.state Pending)
                  in
                  assert (Option.is_none waiting.active_operation);
                  let owned =
                    match permission.owner with
                    | Operation _ -> false
                    | Invocation id ->
                      List.exists waiting.invocations ~f:(fun invocation ->
                        Agent_protocol.Id.Invocation.equal invocation.context.id id
                        && Option.is_some invocation.parent_event)
                  in
                  (match mode with
                   | `Cancel -> H.stop handle ~mode:Cancel |> protocol_ok |> ignore
                   | `Ask ->
                     H.respond_permission
                       handle
                       ~permission_id:permission.id
                       ~permission_generation:permission.generation
                       ~choice:Approve_session
                       ~reason:None
                     |> protocol_ok
                     |> ignore
                   | `Allow | `Deny -> assert false);
                  owned
              in
              let settled = await actor (finished Session_start) in
              let value = snapshot_value settled in
              let permission_states =
                List.map settled.permissions ~f:(fun p -> p.state)
              in
              H.close handle;
              Agent_client.Connection.close client;
              Agent_server.Daemon.shutdown daemon |> protocol_ok;
              created.id, value, invocation_owned, permission_states)
          in
          let resumed_value, native_calls =
            Eio.Switch.run (fun sw ->
              let daemon = start sw in
              let client = connection daemon (principal ()) in
              initialize client;
              Agent_client.Connection.request
                client
                (Session_get { session_id = id; history = None })
              |> protocol_ok
              |> ignore;
              let actor = (entry daemon id).actor in
              let restarted_handle =
                match mode with
                | `Allow | `Ask | `Deny -> None
                | `Cancel ->
                  let handle =
                    H.attach
                      ~sw
                      ~clock:(Eio.Stdenv.clock env)
                      ~connection:client
                      ~session_id:id
                      ~mode:Read_write
                      ()
                    |> protocol_ok
                  in
                  H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
                  Some handle
              in
              let settled =
                match mode with
                | `Allow | `Ask -> await actor (finished Session_resume)
                | `Deny | `Cancel ->
                  (* Failed lifecycle receipts must not replay their effects. *)
                  ignore
                    (Agent_server.Runtime_owner.drain_idle_moderator
                       (entry daemon id).runtime
                     : (bool, Agent_protocol.Error.t) result);
                  A.state actor |> protocol_ok
              in
              let value = snapshot_value settled in
              let calls = List.length settled.invocations in
              Option.iter restarted_handle ~f:H.close;
              Agent_client.Connection.close client;
              Agent_server.Daemon.shutdown daemon |> protocol_ok;
              value, calls)
          in
          print_s
            [%sexp
              { mode : [ `Allow | `Ask | `Deny | `Cancel ]
              ; first_value : int
              ; resumed_value : int
              ; invocation_owned : bool
              ; permission_states : Agent_protocol.Permission.state list
              ; native_calls : int
              ; model_calls = (!model_calls : int)
              }])));
  [%expect
    {|
    ((mode Allow) (first_value 10) (resumed_value 110) (invocation_owned true)
     (permission_states ()) (native_calls 2) (model_calls 0))
    ((mode Ask) (first_value 10) (resumed_value 110) (invocation_owned true)
     (permission_states (Approved)) (native_calls 2) (model_calls 0))
    ((mode Deny) (first_value 0) (resumed_value 0) (invocation_owned true)
     (permission_states ()) (native_calls 1) (model_calls 0))
    ((mode Cancel) (first_value 0) (resumed_value 0) (invocation_owned true)
     (permission_states (Cancelled)) (native_calls 1) (model_calls 0))
    |}]
;;

let%expect_test "qualified runtime initialization failure releases its preparation scope" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt_file = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          {|<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = fail("initializer rejected")
let on_event = fun ctx state event -> Task.pure(state + 1)
</script>|};
        let failures, registered =
          Eio.Switch.run (fun sw ->
            let daemon =
              Agent_server.Daemon.start
                ~sw
                ~env
                ~config:(config root workspace prompt_file)
                ~tool_dir:root
                ~home:root
                ~process_start_identity:None
                ~options:
                  { Agent_server.Daemon.default_options with
                    qualify_chatml_extensions = true
                  ; model_post_stream =
                      Some
                        (fun ~sw:_ ~inputs:_ ->
                          failwith "failed initialization must not call a model")
                  }
                ()
              |> protocol_ok
            in
            let client = connection daemon (principal ()) in
            initialize client;
            let failures =
              List.map [ "first"; "second" ] ~f:(fun key ->
                match
                  Agent_client.Connection.request
                    client
                    (Session_create (create_request ~key ()))
                with
                | Ok _ -> failwith "poisoned initializer was admitted"
                | Error error ->
                  String.is_substring error.message ~substring:"initializer rejected")
            in
            let registered =
              List.length
                (Agent_server.Session_registry.entries
                   (Agent_server.Daemon.registry daemon))
            in
            Agent_client.Connection.close client;
            Agent_server.Daemon.shutdown daemon |> protocol_ok;
            failures, registered)
        in
        (* Reaching this point also joins both failed preparations' resource scopes. *)
        print_s [%sexp { failures : bool list; registered : int }]));
  [%expect {| ((failures (true true)) (registered 0)) |}]
;;

let server_health connection ~include_details =
  Agent_client.Connection.request
    connection
    (Server_health Agent_protocol.Health.Request.{ include_details })
  |> protocol_ok
  |> function
  | Agent_protocol.Method_result.Server_health health -> health
  | _ -> failwith "unexpected server health response"
;;

let%expect_test "public health is redacted and administrative health reports services" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        let prompt_file = Filename.concat root "agent.chatmd" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Health test agent.</developer>";
        Eio.Switch.run (fun sw ->
          let daemon = start_daemon sw env (config root workspace prompt_file) root in
          let public_principal =
            principal_with_scopes
              "pri_restart_public_health"
              Agent_protocol.Scope.Set.empty
          in
          let public_connection = connection daemon public_principal in
          let admin_connection = connection daemon (principal ()) in
          initialize public_connection;
          initialize admin_connection;
          let info_public =
            match Agent_client.Connection.request public_connection Server_info with
            | Ok (Agent_protocol.Method_result.Server_info _) -> true
            | Ok _ | Error _ -> false
          in
          let public_health = server_health public_connection ~include_details:true in
          let admin_health = server_health admin_connection ~include_details:true in
          let component_names =
            List.map admin_health.components ~f:(fun component -> component.name)
          in
          Agent_client.Connection.close public_connection;
          Agent_client.Connection.close admin_connection;
          Agent_server.Daemon.shutdown daemon |> protocol_ok;
          print_s
            [%sexp
              { info_public : bool
              ; public_component_count = (List.length public_health.components : int)
              ; admin_status = (admin_health.status : Agent_protocol.Health.status)
              ; admin_ready = (admin_health.ready : bool)
              ; component_names : string list
              }]))
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)));
  [%expect
    {|
    ((info_public true) (public_component_count 0) (admin_status Healthy)
     (admin_ready true)
     (component_names
      (daemon storage session_registry start_scheduler job_scheduler
       schedule_scheduler permission_scheduler maintenance configuration)))
    |}]
;;

let%expect_test "graceful shutdown checkpoints the latest durable state" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        let prompt_file = Filename.concat root "agent.chatmd" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Graceful checkpoint session.</developer>";
        Eio.Switch.run (fun sw ->
          let daemon = start_daemon sw env (config root workspace prompt_file) root in
          let client = connection daemon (principal ()) in
          initialize client;
          let created, _ = create_session ~key:"shutdown-checkpoint" client in
          let entry =
            Agent_server.Session_registry.find
              (Agent_server.Daemon.registry daemon)
              created.id
            |> Option.value_exn
          in
          let timestamp = Agent_protocol.Timestamp.now () in
          let job =
            Agent_protocol.Job.
              { id =
                  Agent_protocol.Id.Job.of_string "job_shutdown_checkpoint" |> protocol_ok
              ; session_id = created.id
              ; generation = created.generation
              ; kind = Model_call
              ; payload = `Null
              ; status = Succeeded
              ; retry_policy = Never
              ; attempt = 1
              ; created_at = timestamp
              ; started_at = Some timestamp
              ; next_run_at = None
              ; completed_at = Some timestamp
              ; result = Some `Null
              ; delivery = Not_required
              }
          in
          Agent_session.Session_actor.add_job entry.actor job |> protocol_ok |> ignore;
          let snapshot_directory =
            entry.store_handle
            |> Option.value_exn
            |> Agent_store.Session_store.Handle.snapshot_directory
          in
          Agent_client.Connection.close client;
          let state = Agent_session.Session_actor.state entry.actor |> protocol_ok in
          let expected_transaction = state.counters.transaction_sequence in
          Agent_server.Daemon.shutdown daemon |> protocol_ok;
          let installed =
            (match
               Agent_store.Snapshot.load_current
                 ~env
                 ~directory:snapshot_directory
                 ~max_payload_length:(64 * 1024 * 1024)
             with
             | Ok value -> value
             | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)])
            |> Option.value_exn
          in
          print_s
            [%sexp
              { expected_transaction : int64
              ; checkpoint_transaction = (installed.snapshot.transaction_sequence : int64)
              ; checkpoint_is_latest =
                  (Int64.equal
                     installed.snapshot.transaction_sequence
                     expected_transaction
                   : bool)
              }]))
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)));
  [%expect
    {|
    ((expected_transaction 4) (checkpoint_transaction 4)
     (checkpoint_is_latest true))
    |}]
;;

let%expect_test "operator grant authorizes only the exact compiled manifest" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        let prompt_file = Filename.concat root "agent.chatmd" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          shell_prompt;
        let principal = principal () in
        Eio.Switch.run (fun sw ->
          let preparatory =
            start_daemon sw env (config root workspace prompt_file) root
          in
          let source_sha256, manifest_sha256 = prompt_hashes preparatory in
          Agent_server.Daemon.shutdown preparatory |> protocol_ok;
          let bad_grant =
            operator_grant
              principal.id
              ~source_sha256
              ~manifest_sha256:(String.make 64 '0')
          in
          let bad_config =
            config
              ~profile:required_manifest_profile
              ~manifest_grants:[ bad_grant ]
              root
              workspace
              prompt_file
          in
          let denied_daemon = start_daemon sw env bad_config root in
          let denied_connection = connection denied_daemon principal in
          initialize denied_connection;
          let mismatched_denied =
            Agent_client.Connection.request
              denied_connection
              (Session_create (create_request ~key:"operator-bad" ()))
            |> Result.is_error
          in
          Agent_client.Connection.close denied_connection;
          Agent_server.Daemon.shutdown denied_daemon |> protocol_ok;
          let grant = operator_grant principal.id ~source_sha256 ~manifest_sha256 in
          let authorized_config =
            config
              ~profile:required_manifest_profile
              ~manifest_grants:[ grant ]
              root
              workspace
              prompt_file
          in
          let daemon = start_daemon sw env authorized_config root in
          let client = connection daemon principal in
          initialize client;
          let session, _ = create_session ~key:"operator-good" client in
          let persisted_grants = manifest_grant_count daemon session.id in
          Agent_client.Connection.close client;
          Agent_server.Daemon.shutdown daemon |> protocol_ok;
          print_s [%sexp { mismatched_denied : bool; persisted_grants : int }]))
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)));
  [%expect {| ((mismatched_denied true) (persisted_grants 1)) |}]
;;

let%test_unit
    "restart reconciles durable invocation outcomes once before lazy runtime restoration"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        let prompt_file = Filename.concat root "agent.chatmd" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Offline invocation recovery fixture.</developer>";
        let configuration = config root workspace prompt_file in
        Eio.Switch.run (fun sw ->
          let first = start_daemon sw env configuration root in
          let client = connection first (principal ()) in
          initialize client;
          let created, attachment = create_session ~key:"invocation-recovery" client in
          let entry =
            Agent_server.Session_registry.load
              (Agent_server.Daemon.registry first)
              created.id
            |> protocol_ok
          in
          List.iter [ false; true ] ~f:(fun custom ->
            let module I = Agent_protocol.Invocation in
            let id =
              Agent_session.History_id_source.allocate entry.history_ids |> protocol_ok
            in
            let call_id = if custom then "custom-interrupted" else "function-saved" in
            let item =
              if custom
              then
                Openai.Responses.Item.Custom_tool_call
                  { name = "fixture"
                  ; input = "null"
                  ; call_id
                  ; _type = "custom_tool_call"
                  ; id = None
                  }
              else
                Function_call
                  { name = "fixture"
                  ; arguments = "null"
                  ; call_id
                  ; _type = "function_call"
                  ; id = None
                  ; status = None
                  }
            in
            Agent_session.Session_actor.append_history
              entry.actor
              ~attachment_id:attachment.id
              [ Agent_session.History_codec.to_protocol
                  (History_entry.create_with_id ~id item)
              ]
            |> protocol_ok
            |> ignore;
            let state = Agent_session.Session_actor.state entry.actor |> protocol_ok in
            let admitted =
              I.create
                { id = Agent_protocol.Id.Invocation.create ()
                ; session_id = created.id
                ; generation = created.generation
                ; origin = Model
                ; provider_call_id = Some call_id
                ; call_entry_id = Some id
                ; parent_invocation = None
                ; parent_job = None
                ; tool_name = "fixture"
                ; implementation_revision = "retained-fixture"
                ; capability_fingerprint = "retained-capability"
                ; input = `Null
                ; created_at = Agent_protocol.Timestamp.now ()
                ; deadline = None
                }
              |> protocol_ok
            in
            let dispatched = I.dispatch admitted |> protocol_ok in
            let changes =
              Agent_session.Session_actor.Extension_change.
                [ Invocation admitted; Invocation dispatched ]
            in
            let changes =
              if custom
              then changes
              else
                changes
                @ [ Invocation
                      (I.resolve
                         dispatched
                         ~session_id:created.id
                         ~generation:created.generation
                         (Complete (`String "already computed"))
                       |> protocol_ok)
                  ]
            in
            Agent_session.Session_actor.commit_extensions
              entry.actor
              ~generation:created.generation
              ~expected_revision:state.counters.revision
              changes
            |> protocol_ok
            |> ignore);
          Agent_client.Connection.close client;
          Agent_server.Daemon.shutdown first |> protocol_ok;
          let second = start_daemon sw env configuration root in
          let entry =
            Agent_server.Session_registry.load
              (Agent_server.Daemon.registry second)
              created.id
            |> protocol_ok
          in
          let recovered = Agent_session.Session_actor.state entry.actor |> protocol_ok in
          assert (List.length recovered.invocations = 2);
          List.iter recovered.invocations ~f:(fun inv ->
            assert (Option.is_some inv.output_entry_id);
            Agent_session.Invocation_history.validate_publication
              ~history:recovered.conversation.canonical_history
              inv
            |> protocol_ok;
            match inv.context.provider_call_id, inv.status with
            | Some "function-saved", Published (Complete (`String "already computed")) ->
              ()
            | Some "custom-interrupted", Published (Cancelled _) -> ()
            | _ -> assert false);
          let new_id =
            Agent_session.History_id_source.allocate entry.history_ids |> protocol_ok
          in
          assert (
            not
              (List.exists recovered.conversation.canonical_history ~f:(fun e ->
                 History_entry.Id.equal new_id e.id)));
          Agent_server.Daemon.shutdown second |> protocol_ok;
          let third = start_daemon sw env configuration root in
          let entry =
            Agent_server.Session_registry.load
              (Agent_server.Daemon.registry third)
              created.id
            |> protocol_ok
          in
          let again = Agent_session.Session_actor.state entry.actor |> protocol_ok in
          assert (
            Sexp.equal
              ([%sexp_of: Agent_protocol.Invocation.t list] recovered.invocations)
              ([%sexp_of: Agent_protocol.Invocation.t list] again.invocations));
          assert (
            List.length again.conversation.canonical_history
            = List.length recovered.conversation.canonical_history);
          Agent_server.Daemon.shutdown third |> protocol_ok)))
;;

let%expect_test "durable stopped session resets, recovers, and starts after restart" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        let prompt_file = Filename.concat root "agent.chatmd" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          shell_prompt;
        let config = config root workspace prompt_file in
        let principal = principal () in
        Eio.Switch.run (fun sw ->
          let first_daemon = start_daemon sw env config root in
          let first_connection = connection first_daemon principal in
          initialize first_connection;
          let created, attachment = create_session first_connection in
          let grants_before_restart = manifest_grant_count first_daemon created.id in
          let reset = reset_session first_connection created attachment in
          Agent_client.Connection.close first_connection;
          Agent_server.Daemon.shutdown first_daemon |> protocol_ok;
          let second_daemon = start_daemon sw env config root in
          let second_connection = connection second_daemon principal in
          initialize second_connection;
          let recovered =
            Agent_client.Connection.request
              second_connection
              (Session_get { session_id = reset.id; history = None })
            |> protocol_ok
            |> function
            | Agent_protocol.Method_result.Session_get snapshot -> snapshot.session
            | _ -> failwith "unexpected recovered session response"
          in
          let grants_after_restart = manifest_grant_count second_daemon recovered.id in
          let handle =
            Agent_client.Session_handle.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:second_connection
              ~session_id:reset.id
              ~mode:Read_write
              ()
            |> protocol_ok
          in
          let started =
            Agent_client.Session_handle.start handle ~queue_if_limited:false
            |> protocol_ok
          in
          Agent_client.Session_handle.stop handle ~mode:Cancel |> protocol_ok |> ignore;
          let active_grants = grants second_connection recovered.id (Some Active) in
          let manifest_grant =
            List.find_exn active_grants ~f:(fun grant ->
              String.equal grant.Agent_protocol.Grant.tool_name "shell.manifest")
          in
          let revoked_grant =
            Agent_client.Session_handle.revoke_grant
              handle
              ~grant_id:manifest_grant.id
              ~reason:"restart test revocation"
            |> protocol_ok
          in
          let revoked_grants = grants second_connection recovered.id (Some Revoked) in
          Agent_client.Session_handle.close handle;
          Agent_client.Connection.close second_connection;
          Agent_server.Daemon.shutdown second_daemon |> protocol_ok;
          print_s
            [%sexp
              { same_session =
                  (Agent_protocol.Id.Session.compare recovered.id reset.id = 0 : bool)
              ; generation_survived = (recovered.generation = 1 : bool)
              ; revision_survived = (Int64.(recovered.revision >= reset.revision) : bool)
              ; starts_after_restart = (observed_idle started.observed_state : bool)
              ; grants_before_restart : int
              ; grants_after_restart : int
              ; projected_manifest_grants = (List.length active_grants : int)
              ; revoked =
                  (Agent_protocol.Grant.equal_state revoked_grant.state Revoked : bool)
              ; revoked_visible =
                  (List.exists revoked_grants ~f:(fun grant ->
                     Agent_protocol.Id.Grant.compare grant.id revoked_grant.id = 0)
                   : bool)
              }]))
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)));
  [%expect
    {|
    ((same_session true) (generation_survived true) (revision_survived true)
     (starts_after_restart true) (grants_before_restart 1)
     (grants_after_restart 1) (projected_manifest_grants 1) (revoked true)
     (revoked_visible true))
    |}]
;;

let%expect_test "inactive stopped actors unload and reconstruct on demand" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        let prompt_file = Filename.concat root "agent.chatmd" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Lazy stopped session.</developer>";
        Eio.Switch.run (fun sw ->
          let daemon = start_daemon sw env (config root workspace prompt_file) root in
          let client = connection daemon (principal ()) in
          initialize client;
          let created, _ = create_session ~key:"lazy-unload-create" client in
          Agent_client.Connection.close client;
          let registry = Agent_server.Daemon.registry daemon in
          let unloaded =
            Agent_server.Session_registry.unload_inactive
              registry
              ~index_entries:
                (Agent_store.Session_store.list_sessions
                   (Agent_server.Daemon.store daemon))
          in
          let absent_after_unload =
            Option.is_none (Agent_server.Session_registry.find registry created.id)
          in
          let indexed_summary_visible =
            Agent_server.Session_registry.summaries registry
            |> List.exists ~f:(fun session ->
              Agent_protocol.Id.Session.compare session.id created.id = 0)
          in
          let second_client = connection daemon (principal ()) in
          initialize second_client;
          let loaded =
            Agent_client.Connection.request
              second_client
              (Session_get { session_id = created.id; history = None })
            |> protocol_ok
            |> function
            | Agent_protocol.Method_result.Session_get snapshot -> snapshot.session
            | _ -> failwith "unexpected lazy session response"
          in
          let present_after_get =
            Option.is_some (Agent_server.Session_registry.find registry created.id)
          in
          let reconstructed_entry =
            Agent_server.Session_registry.find registry created.id |> Option.value_exn
          in
          let runtime_deferred =
            not (Agent_server.Runtime_owner.is_loaded reconstructed_entry.runtime)
          in
          let handle =
            Agent_client.Session_handle.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:second_client
              ~session_id:created.id
              ~mode:Read_write
              ~subscribe:false
              ()
            |> protocol_ok
          in
          Agent_client.Session_handle.start handle ~queue_if_limited:false
          |> protocol_ok
          |> ignore;
          let runtime_loaded_on_start =
            Agent_server.Runtime_owner.is_loaded reconstructed_entry.runtime
          in
          Agent_client.Session_handle.close handle;
          Agent_client.Connection.close second_client;
          Agent_server.Daemon.shutdown daemon |> protocol_ok;
          print_s
            [%sexp
              { unloaded : int
              ; absent_after_unload : bool
              ; indexed_summary_visible : bool
              ; same_session =
                  (Agent_protocol.Id.Session.compare loaded.id created.id = 0 : bool)
              ; present_after_get : bool
              ; runtime_deferred : bool
              ; runtime_loaded_on_start : bool
              }]))
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)));
  [%expect
    {|
    ((unloaded 1) (absent_after_unload true) (indexed_summary_visible true)
     (same_session true) (present_after_get true) (runtime_deferred true)
     (runtime_loaded_on_start true))
    |}]
;;

let%expect_test "legacy import preserves durable state without mutating its source" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        let prompt_file = Filename.concat root "agent.chatmd" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Legacy import session.</developer>";
        let allocator =
          History_entry.Allocator.create ~namespace:"legacy-import" ~next_sequence:0
          |> Result.ok_or_failwith
        in
        let id = History_entry.Allocator.allocate allocator |> Result.ok_or_failwith in
        let history = [ Agent_session.History_codec.user_text ~id "legacy message" ] in
        let task = Session.Task.create ~id:"legacy-task" ~title:"Preserve task" () in
        let legacy =
          { (Session.create ~id:"legacy-source" ~prompt_file ()) with
            history
          ; next_history_sequence = 1
          ; tasks = [ task ]
          ; kv_store = [ "legacy-key", "legacy-value" ]
          }
        in
        let source_path = Filename.concat root "legacy-source" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / source_path);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / Filename.concat source_path "sentinel")
          "unchanged";
        let imported_id =
          Eio.Switch.run (fun sw ->
            let daemon = start_daemon sw env (config root workspace prompt_file) root in
            let request =
              { (create_request ~key:"legacy-import" ()) with
                requested_mode = None
              ; subscribe = false
              }
            in
            let imported =
              Agent_server.Daemon.import_legacy
                daemon
                ~principal:(principal ())
                ~source_id:legacy.id
                ~source_path
                ~legacy
                request
              |> protocol_ok
            in
            Agent_server.Daemon.shutdown daemon |> protocol_ok;
            imported.id)
        in
        Eio.Switch.run (fun sw ->
          let daemon = start_daemon sw env (config root workspace prompt_file) root in
          let entry =
            Agent_server.Session_registry.load
              (Agent_server.Daemon.registry daemon)
              imported_id
            |> protocol_ok
          in
          let state = Agent_session.Session_actor.state entry.actor |> protocol_ok in
          let data_root =
            Agent_server.Daemon.store daemon |> Agent_store.Session_store.data_root
          in
          let provenance =
            Filename.concat
              (Agent_store.Data_root.session_path data_root imported_id)
              "archive/legacy-import.sexp"
          in
          let source_unchanged =
            Eio.Path.load
              Eio.Path.(Eio.Stdenv.fs env / Filename.concat source_path "sentinel")
            |> String.equal "unchanged"
          in
          print_s
            [%sexp
              { history_count = (List.length state.conversation.canonical_history : int)
              ; next_history_sequence = (state.conversation.next_history_sequence : int64)
              ; task_count = (List.length state.conversation.tasks : int)
              ; kv_store = (state.conversation.kv_store : (string * string) list)
              ; runtime_deferred =
                  (not (Agent_server.Runtime_owner.is_loaded entry.runtime) : bool)
              ; provenance_exists =
                  (Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / provenance) : bool)
              ; source_unchanged : bool
              }];
          Agent_server.Daemon.shutdown daemon |> protocol_ok))
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)));
  [%expect
    {|
    ((history_count 1) (next_history_sequence 4097) (task_count 1)
     (kv_store ((legacy-key legacy-value))) (runtime_deferred true)
     (provenance_exists true) (source_unchanged true))
    |}]
;;

let%expect_test "owner reclaim token survives restart and rotates on reclaim" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        let prompt_file = Filename.concat root "agent.chatmd" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Owner restart session.</developer>";
        let config = config root workspace prompt_file in
        let creator = principal () in
        let first_reclaimer = principal_with_id "pri_restart_reclaimer_one" in
        let second_reclaimer = principal_with_id "pri_restart_reclaimer_two" in
        Eio.Switch.run (fun sw ->
          let first_daemon = start_daemon sw env config root in
          let first_connection = connection first_daemon creator in
          initialize first_connection;
          let idempotency_key =
            Agent_protocol.Idempotency_key.of_string "owner-restart-create" |> protocol_ok
          in
          let spec =
            session_spec
              ~start_immediately:true
              ~liveness:
                (Owner_bound { disconnect_grace_ms = 2_000; stop_mode = Graceful })
              ()
          in
          let created, owner, original_token =
            Agent_client.Connection.request
              first_connection
              (Session_create
                 { spec
                 ; requested_mode = Some Owner_read_write
                 ; subscribe = false
                 ; idempotency_key
                 })
            |> protocol_ok
            |> function
            | Agent_protocol.Method_result.Session_create
                { session; attachment = Some attachment; _ } ->
              session, attachment.attachment, Option.value_exn attachment.reclaim_token
            | _ -> failwith "unexpected owner create response"
          in
          ignore owner;
          Agent_client.Connection.close first_connection;
          let owner_hint_persisted =
            Agent_store.Session_store.list_sessions
              (Agent_server.Daemon.store first_daemon)
            |> List.find_exn ~f:(fun entry ->
              Agent_protocol.Id.Session.compare entry.session.id created.id = 0)
            |> fun (entry : Agent_store.Session_index.Entry.t) ->
            Option.is_some entry.owner_grace_deadline
          in
          Agent_server.Daemon.shutdown first_daemon |> protocol_ok;
          let second_daemon = start_daemon sw env config root in
          let reclaim_connection = connection second_daemon first_reclaimer in
          initialize reclaim_connection;
          let handle =
            Agent_client.Session_handle.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:reclaim_connection
              ~session_id:created.id
              ~mode:Owner_read_write
              ~reclaim_token:original_token
              ()
            |> protocol_ok
          in
          let rotated_token =
            Agent_client.Session_handle.reclaim_token handle |> Option.value_exn
          in
          let generation_rotated =
            let lease =
              (Agent_client.Session_handle.attachment handle).owner_lease
              |> Option.value_exn
            in
            Int64.(lease.generation > 1L)
          in
          Agent_client.Session_handle.close handle;
          Agent_client.Connection.close reclaim_connection;
          let stale_connection = connection second_daemon second_reclaimer in
          initialize stale_connection;
          let stale_rejected =
            Agent_client.Session_handle.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:stale_connection
              ~session_id:created.id
              ~mode:Owner_read_write
              ~reclaim_token:original_token
              ()
            |> Result.is_error
          in
          let current_handle =
            Agent_client.Session_handle.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:stale_connection
              ~session_id:created.id
              ~mode:Owner_read_write
              ~reclaim_token:rotated_token
              ()
            |> protocol_ok
          in
          Agent_client.Session_handle.close current_handle;
          Agent_client.Connection.close stale_connection;
          Agent_server.Daemon.shutdown second_daemon |> protocol_ok;
          print_s
            [%sexp
              { owner_hint_persisted : bool
              ; token_rotated = (not (String.equal original_token rotated_token) : bool)
              ; generation_rotated : bool
              ; stale_rejected : bool
              }]))
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)));
  [%expect
    {|
    ((owner_hint_persisted true) (token_rotated true) (generation_rotated true)
     (stale_rejected true))
    |}]
;;

let%expect_test "daemon idle polling applies retained termination without a queued event" =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  List.iter [ `Observation; `Event ] ~f:(fun source ->
    Eio_main.run (fun env ->
      Mirage_crypto_rng_unix.use_default ();
      let root = temporary_root env in
      Exn.protect
        ~finally:(fun () ->
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
        ~f:(fun () ->
          let workspace = Filename.concat root "workspace" in
          let prompt_file = Filename.concat root "agent.chatmd" in
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / prompt_file)
            {|<developer>Idle follow-up fixture.</developer>
<script language="chatml" kind="moderator">
type event = [ `Session_start | `Session_resume ]
let initial_state = 0
let on_event : context -> int -> event -> int task = fun ctx state event -> Task.pure(state)
</script>|};
          Eio.Switch.run (fun sw ->
            let daemon =
              Agent_server.Daemon.start
                ~sw
                ~env
                ~config:(config root workspace prompt_file)
                ~tool_dir:root
                ~home:root
                ~process_start_identity:None
                ~options:
                  { Agent_server.Daemon.default_options with
                    model_post_stream =
                      Some (fun ~sw:_ ~inputs:_ -> failwith "unexpected model execution")
                  }
                ()
              |> protocol_ok
            in
            let client = connection daemon (principal ()) in
            initialize client;
            let created, _ =
              create_session ~start_immediately:true ~key:"follow-up-poll" client
            in
            let entry =
              Agent_server.Session_registry.find
                (Agent_server.Daemon.registry daemon)
                created.id
              |> Option.value_exn
            in
            let state = A.state entry.actor |> protocol_ok in
            (* Install a v1-shaped durable receipt fixture; the public runtime
             still uses the legacy moderator compiler's source fingerprint. *)
            let snapshot =
              match state.moderator with
              | Some (`Object fields) ->
                (match
                   List.Assoc.find fields "identity_snapshot_sexp" ~equal:String.equal
                 with
                 | Some (`String encoded) ->
                   Session.Moderator_state.Identity_snapshot.t_of_sexp
                     (Sexp.of_string encoded)
                 | _ -> assert false)
              | _ -> assert false
            in
            let snapshot = { snapshot with script_source_hash = String.make 64 'a' } in
            A.change_moderator
              entry.actor
              (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
            |> protocol_ok
            |> ignore;
            let state = A.state entry.actor |> protocol_ok in
            assert (
              not
                (Agent_session.Runtime_builder.moderator_snapshot_has_queued_events
                   state.moderator
                 |> protocol_ok));
            let observer =
              Agent_session.Runtime_builder.moderator_snapshot_observer state.moderator
              |> protocol_ok
              |> Option.value_exn
            in
            let parent =
              I.create
                { id = Agent_protocol.Id.Invocation.create ()
                ; session_id = created.id
                ; generation = created.generation
                ; origin = Script
                ; provider_call_id = None
                ; call_entry_id = None
                ; parent_invocation = None
                ; parent_job = None
                ; tool_name = "fixture"
                ; implementation_revision = "fixture"
                ; capability_fingerprint = "fixture"
                ; input = `Null
                ; created_at = Agent_protocol.Timestamp.now ()
                ; deadline = None
                }
              |> protocol_ok
            in
            let parent_dispatched = I.dispatch parent |> protocol_ok in
            let resolve invocation =
              I.resolve
                invocation
                ~session_id:created.id
                ~generation:created.generation
                (Complete `Null)
              |> protocol_ok
            in
            let child =
              I.create
                ~observer
                { parent.context with
                  id = Agent_protocol.Id.Invocation.create ()
                ; origin = Moderator
                ; parent_invocation = Some parent.context.id
                }
              |> protocol_ok
            in
            let dispatched = I.dispatch child |> protocol_ok in
            let resolved = resolve dispatched in
            let observing = I.claim_observation resolved |> protocol_ok in
            let observed =
              I.complete_observation
                observing
                ~follow_up:
                  { request_turn = true
                  ; request_compaction = true
                  ; end_session = Some "finished"
                  }
              |> protocol_ok
            in
            (match source with
             | `Observation ->
               A.commit_extensions
                 entry.actor
                 ~generation:created.generation
                 ~expected_revision:state.counters.revision
                 (List.map
                    [ parent
                    ; parent_dispatched
                    ; resolve parent_dispatched
                    ; child
                    ; dispatched
                    ; resolved
                    ; observing
                    ; observed
                    ]
                    ~f:(fun invocation -> A.Extension_change.Invocation invocation))
               |> protocol_ok
               |> ignore
             | `Event ->
               (* Hold the owner lock while creating and consuming the fixture head,
                so polling sees only the final empty queue with retained intent. *)
               Agent_server.Runtime_owner.For_testing.with_loaded_runtime
                 entry.runtime
                 (fun () ->
                    let open Result.Let_syntax in
                    let queued =
                      { snapshot with
                        queued_internal_events =
                          [ Session.Snapshot.Variant
                              ("Internal_event", [ Variant ("Null", []) ])
                          ]
                      }
                    in
                    let%bind _ =
                      A.change_moderator
                        entry.actor
                        (Some
                           (Agent_session.Runtime_builder.encode_moderator_snapshot
                              queued))
                    in
                    let%map _ =
                      A.with_idle_queued_moderator_event
                        entry.actor
                        ~snapshot:queued
                        (fun ~event:_ ~commit ->
                           commit
                             ~snapshot
                             ~requests:
                               { request_turn = true
                               ; request_compaction = true
                               ; end_session = Some "finished"
                               })
                    in
                    ())
               |> protocol_ok);
            let stopped =
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                let rec wait () =
                  let state = A.state entry.actor |> protocol_ok in
                  match state.lifecycle.observed with
                  | Stopped -> state
                  | _ ->
                    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                    wait ()
                in
                wait ())
            in
            let follow_up, event_intent =
              match source with
              | `Observation ->
                let retained =
                  List.find_exn stopped.invocations ~f:(fun invocation ->
                    Agent_protocol.Id.Invocation.equal
                      invocation.context.id
                      child.context.id)
                in
                (Option.value_exn retained.observation).follow_up, None
              | `Event ->
                assert (List.is_empty stopped.invocations);
                None, (List.hd_exn stopped.moderator_executions).intent
            in
            print_s
              [%sexp
                { source : [ `Observation | `Event ]
                ; halted = (stopped.halted : bool)
                ; active_operation = (Option.is_some stopped.active_operation : bool)
                ; history_entries =
                    (List.length stopped.conversation.canonical_history : int)
                ; follow_up : I.follow_up_status option
                ; event_intent : Agent_protocol.Moderator_execution.intent option
                }];
            Agent_client.Connection.close client;
            Agent_server.Daemon.shutdown daemon |> protocol_ok))));
  [%expect
    {|
    ((source Observation) (halted true) (active_operation false)
     (history_entries 1)
     (follow_up
      ((Applied_follow_up
        ((request_turn true) (request_compaction true) (end_session (finished))))))
     (event_intent ()))
    ((source Event) (halted true) (active_operation false) (history_entries 1)
     (follow_up ()) (event_intent (Applied)))
    |}]
;;

let%expect_test "running model jobs recover interrupted and redeliver without rerun" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        let prompt_file = Filename.concat root "agent.chatmd" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          {|
<developer>You are a restart job test agent.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event =
    [ `Session_start
    | `Session_resume
    | `Model_job_succeeded(string, string, json)
    | `Model_job_failed(string, string, string)
    ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx st ev ->
      match ev with
      | `Session_start -> Task.pure(st)
      | `Session_resume -> Task.pure(st)
      | `Model_job_succeeded(job_id, recipe, result) ->
        Task.bind(Runtime.end_session("unexpected recovered success"), fun ignored ->
        Task.pure(st + 1))
      | `Model_job_failed(job_id, recipe, message) ->
        Task.bind(Runtime.end_session("recovered job delivered"), fun ignored ->
        Task.pure(st + 1))
</script>
|};
        let config = config root workspace prompt_file in
        let principal = principal () in
        Eio.Switch.run (fun sw ->
          let first_daemon = start_daemon sw env config root in
          let first_connection = connection first_daemon principal in
          initialize first_connection;
          let created, _ =
            create_session
              ~start_immediately:true
              ~key:"restart-running-job-create"
              first_connection
          in
          let entry =
            Agent_server.Session_registry.find
              (Agent_server.Daemon.registry first_daemon)
              created.id
            |> Option.value_exn
          in
          let job =
            Agent_protocol.Job.
              { id = Agent_protocol.Id.Job.of_string "job_restart_running" |> protocol_ok
              ; session_id = created.id
              ; generation = created.generation
              ; kind = Model_call
              ; payload =
                  `Object [ "recipe", `String "agent_prompt_v1"; "payload", `Null ]
              ; status = Running
              ; retry_policy = Never
              ; attempt = 1
              ; created_at = Agent_protocol.Timestamp.now ()
              ; started_at = Some (Agent_protocol.Timestamp.now ())
              ; next_run_at = None
              ; completed_at = None
              ; result = None
              ; delivery = Pending
              }
          in
          Agent_session.Session_actor.add_job entry.actor job |> protocol_ok |> ignore;
          Agent_client.Connection.close first_connection;
          Agent_server.Daemon.shutdown first_daemon |> protocol_ok;
          let second_daemon = start_daemon sw env config root in
          let second_connection = connection second_daemon principal in
          initialize second_connection;
          let rec await_delivery attempts =
            let snapshot =
              Agent_client.Connection.request
                second_connection
                (Session_get { session_id = created.id; history = None })
              |> protocol_ok
              |> function
              | Agent_protocol.Method_result.Session_get snapshot -> snapshot
              | _ -> failwith "unexpected recovered job response"
            in
            match snapshot.halted, snapshot.jobs with
            | true, [ recovered ] when job_delivered recovered.delivery ->
              snapshot, recovered
            | _ when attempts = 0 -> failwith "recovered model job was not delivered"
            | _ ->
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
              await_delivery (attempts - 1)
          in
          let snapshot, recovered = await_delivery 200 in
          let interrupted =
            match recovered.Agent_protocol.Job.status with
            | Interrupted _ -> true
            | Queued | Running | Waiting_permission _ | Succeeded | Failed _ | Cancelled
              -> false
          in
          Agent_client.Connection.close second_connection;
          Agent_server.Daemon.shutdown second_daemon |> protocol_ok;
          print_s
            [%sexp
              { halted = (snapshot.halted : bool)
              ; halt_reason = (snapshot.halt_reason : string option)
              ; interrupted : bool
              ; attempt_unchanged = (recovered.attempt = 1 : bool)
              ; delivered = (job_delivered recovered.delivery : bool)
              }]))
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)));
  [%expect
    {|
    ((halted true) (halt_reason ("recovered job delivered")) (interrupted true)
     (attempt_unchanged true) (delivered true))
    |}]
;;
