open Core
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Port_reservation = Support.Port_reservation
module Process_manager = Support.Process_manager
module Stdio_client = Support.Stdio_client
module Stdio_process = Support.Stdio_process
module Temporary_environment = Support.Temporary_environment
module Unix_driver = Support.Unix_driver

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
;;

let absolute env path =
  if Filename.is_absolute path
  then path
  else Filename.concat (Eio.Path.native_exn (Eio.Stdenv.cwd env)) path
;;

let fallback_executable env =
  Filename.concat
    (Eio.Path.native_exn (Eio.Stdenv.cwd env))
    "_build/default/bin/ochat_agent_stdio.exe"
;;

let executable env =
  let candidate =
    Sys.getenv "OCHAT_E2E_STDIO_EXE"
    |> Option.value ~default:(fallback_executable env)
    |> absolute env
  in
  if Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / candidate)
  then candidate
  else raise_s [%sexp "ochat-agent-stdio executable is unavailable", (candidate : string)]
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
    (executable env :: arguments)
;;

let run_stdio_cli ~sw env fixture arguments =
  Process_manager.spawn
    ~sw
    ~env
    ~environment:(child_environment fixture)
    ~max_output_bytes:(1024 * 1024)
    (executable env :: arguments)
  |> Process_manager.await
;;

let local_arguments fixture =
  [ "--local"
  ; "--prompt"
  ; Config_fixture.prompt_path fixture
  ; "--workspace"
  ; Config_fixture.physical_workspace fixture
  ; "--data-root"
  ; Config_fixture.data_dir fixture
  ]
;;

let require_exit result expected message =
  if not (Process_manager.equal_exit result.Process_manager.exit expected)
  then
    raise_s
      [%sexp
        "E2E exit assertion failed"
      , { message : string
        ; expected : Process_manager.exit
        ; actual = (result.exit : Process_manager.exit)
        ; stdout = (result.stdout.contents : string)
        ; stderr = (result.stderr.contents : string)
        }]
;;

let await_process env process =
  match
    Eio.Time.with_timeout (Eio.Stdenv.clock env) 5. (fun () ->
      Ok (Stdio_process.await process))
  with
  | Ok result -> result
  | Error `Timeout ->
    Stdio_process.terminate process ~clock:(Eio.Stdenv.clock env) ~grace_seconds:1.
    |> fun (termination : Process_manager.termination) -> termination.result
;;

let finish_process env process =
  Stdio_process.close_stdin process;
  await_process env process
;;

let stop_process env process =
  match Stdio_process.poll_result process with
  | Some _ -> ()
  | None ->
    ignore
      (Stdio_process.terminate process ~clock:(Eio.Stdenv.clock env) ~grace_seconds:1.
       : Process_manager.termination)
;;

let assert_stdout_pure output =
  require (not output.Process_manager.truncated) "stdio stdout capture was truncated";
  output.contents
  |> String.split_lines
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.iter ~f:(fun line ->
    match Result.try_with (fun () -> Jsonaf.of_string line) with
    | Error _ -> fail ("stdio stdout contained non-JSON text: " ^ line)
    | Ok json ->
      (match Agent_protocol.Envelope.of_json json with
       | Ok _ -> ()
       | Error _ -> fail ("stdio stdout contained a non-protocol JSON value: " ^ line)))
;;

let with_stdio ~sw env fixture arguments f =
  let process = spawn_stdio ~sw env fixture arguments in
  Exn.protect ~f:(fun () -> f process) ~finally:(fun () -> stop_process env process)
;;

let typed_request client command = Stdio_client.request client command |> protocol_ok
let initialize client = Stdio_client.initialize client |> protocol_ok |> fst

let ping client =
  let response = typed_request client (Protocol_ping { payload = None }) in
  match response.result with
  | Protocol_ping ping -> ping
  | _ -> fail "protocol.ping returned the wrong result variant"
;;

let page_request () = Agent_protocol.Page.Request.create ~limit:100 () |> protocol_ok

let list_sessions client =
  let request =
    Agent_protocol.Session.List_request.
      { page = page_request ()
      ; desired_state = None
      ; prompt_id = None
      ; workspace_id = None
      ; owner_principal_id = None
      ; labels = []
      }
  in
  match (typed_request client (Session_list request)).result with
  | Session_list page -> page.items
  | _ -> fail "session.list returned the wrong result variant"
;;

let catalog client =
  let prompt_request =
    Agent_protocol.Prompt.List_request.
      { page = page_request (); enabled = Some true; available = Some true }
  in
  let workspace_request =
    Agent_protocol.Workspace.List_request.
      { page = page_request (); kind = None; access = None; available = Some true }
  in
  let prompts =
    match (typed_request client (Prompt_list prompt_request)).result with
    | Prompt_list page -> page.items
    | _ -> fail "prompt.list returned the wrong result variant"
  in
  let workspaces =
    match (typed_request client (Workspace_list workspace_request)).result with
    | Workspace_list page -> page.items
    | _ -> fail "workspace.list returned the wrong result variant"
  in
  List.hd_exn prompts, List.hd_exn workspaces
;;

let idempotency_key name = Agent_protocol.Idempotency_key.of_string name |> protocol_ok

let session_spec client =
  let prompt, workspace = catalog client in
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog prompt.id)
    ~workspace:(Configured workspace.id)
    ~liveness:Detached
    ~persistence:Durable
    ~permission_profile:"unattended"
    ~start_immediately:false
    ~labels:[ "suite", "stdio-e2e" ]
    ()
  |> protocol_ok
;;

let create_session client key =
  let request =
    Agent_protocol.Session.Create_request.
      { spec = session_spec client
      ; requested_mode = Some Read_write
      ; subscribe = true
      ; idempotency_key = idempotency_key key
      }
  in
  let response = typed_request client (Session_create request) in
  match response.result with
  | Session_create created -> created, response.notifications
  | _ -> fail "session.create returned the wrong result variant"
;;

let start_session client session attachment key =
  let request =
    Agent_protocol.Session.Start_request.
      { session_id = session.Agent_protocol.Session.id
      ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
      ; queue_if_limited = false
      ; idempotency_key = idempotency_key key
      }
  in
  let response = typed_request client (Session_start request) in
  match response.result with
  | Session_start mutation -> mutation.session, response.notifications
  | _ -> fail "session.start returned the wrong result variant"
;;

let durable_event session_id = function
  | Agent_protocol.Envelope.Notification { method_ = "session.event"; params } ->
    (match Agent_protocol.Event.Durable.of_json params with
     | Ok event when Agent_protocol.Id.Session.compare event.session_id session_id = 0 ->
       Some event
     | Ok _ -> None
     | Error error ->
       raise_s [%sexp "invalid durable stdio event", (error : Agent_protocol.Error.t)])
  | Notification _ -> None
  | Request _ | Response _ -> fail "expected a stdio notification"
;;

let rec next_durable_event client session_id queued =
  match queued with
  | envelope :: rest ->
    (match durable_event session_id envelope with
     | Some event -> event, rest
     | None -> next_durable_event client session_id rest)
  | [] ->
    let envelope = Stdio_client.next_envelope client ~timeout_seconds:5. |> protocol_ok in
    next_durable_event client session_id [ envelope ]
;;

let rec collect_through client session_id previous through queued events =
  if Int64.(previous >= through)
  then List.rev events
  else (
    let event, queued = next_durable_event client session_id queued in
    require
      (Int64.equal event.sequence Int64.(previous + 1L))
      "stdio durable event sequence was not contiguous";
    collect_through client session_id event.sequence through queued (event :: events))
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

let with_daemon ~sw env fixture config_path f =
  let daemon = Daemon_process.start ~sw ~env ~fixture ~config_path in
  let health =
    match Daemon_process.wait_ready daemon ~env ~timeout_seconds:5. with
    | Ok health -> health
    | Error error -> readiness_failure daemon error
  in
  Exn.protect ~f:(fun () -> f daemon health) ~finally:(fun () -> stop_daemon env daemon)
;;

let write_bearer_file environment name contents =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let path = Filename.concat roots.config name in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    (Temporary_environment.path environment path)
    contents;
  Temporary_environment.register_secret environment (String.strip contents);
  path
;;

let gateway_arguments environment fixture = function
  | `Unix -> [ "--connect"; "unix://" ^ Config_fixture.unix_socket fixture ]
  | `Http ->
    let token_file =
      write_bearer_file
        environment
        "stdio-http.token"
        (Config_fixture.admin_token fixture ^ "\n")
    in
    [ "--connect"
    ; sprintf "http://127.0.0.1:%d" (Config_fixture.http_port fixture)
    ; "--bearer-token-file"
    ; token_file
    ]
;;

let test_local_initialize env environment =
  let fixture = fixture env environment "stdio-local-init" in
  Eio.Switch.run (fun sw ->
    with_stdio ~sw env fixture (local_arguments fixture) (fun process ->
      let client = Stdio_client.create ~process ~clock:(Eio.Stdenv.clock env) in
      let initialized = initialize client in
      require
        (String.equal initialized.protocol_name "ochat.agent")
        "local stdio protocol name differs";
      require (ping client).ready "local stdio ping did not report ready";
      let result = finish_process env process in
      require_exit result (Exited 0) "local stdio did not exit on EOF";
      assert_stdout_pure result.stdout;
      require (String.is_empty result.stderr.contents) "valid local stdio wrote stderr"))
;;

let test_local_process_bound_eof env environment =
  let fixture = fixture env environment "stdio-local-eof" in
  Eio.Switch.run (fun sw ->
    with_stdio ~sw env fixture (local_arguments fixture) (fun process ->
      let client = Stdio_client.create ~process ~clock:(Eio.Stdenv.clock env) in
      ignore (initialize client : Agent_protocol.Initialize.Response.t);
      let sessions = list_sessions client in
      require (List.length sessions = 1) "local stdio did not expose one embedded session";
      let session = List.hd_exn sessions in
      require
        (Agent_protocol.Session.equal_liveness session.spec.liveness Process_bound)
        "local stdio session was not process-bound";
      require
        (Agent_protocol.Session.equal_execution_host session.spec.execution_host Embedded)
        "local stdio session was not embedded";
      let result = finish_process env process in
      require_exit result (Exited 0) "process-bound local stdio survived EOF";
      assert_stdout_pure result.stdout))
;;

let test_local_malformed_input env environment =
  let fixture = fixture env environment "stdio-local-malformed" in
  Eio.Switch.run (fun sw ->
    with_stdio ~sw env fixture (local_arguments fixture) (fun process ->
      let client = Stdio_client.create ~process ~clock:(Eio.Stdenv.clock env) in
      ignore (initialize client : Agent_protocol.Initialize.Response.t);
      Stdio_client.send_raw_line client {|{"jsonrpc":"2.0","id":|} |> protocol_ok;
      require (ping client).ready "local stdio did not recover after malformed input";
      let result = finish_process env process in
      require_exit result (Exited 0) "malformed input prevented clean EOF shutdown";
      assert_stdout_pure result.stdout;
      require
        (String.is_substring result.stderr.contents ~substring:"Invalid_request")
        "malformed input diagnostic was not written to stderr"))
;;

let test_local_oversized_input env environment =
  let fixture = fixture env environment "stdio-local-oversized" in
  Eio.Switch.run (fun sw ->
    with_stdio ~sw env fixture (local_arguments fixture) (fun process ->
      let oversized = String.make ((16 * 1024 * 1024) + 1) 'x' in
      ignore
        (Result.try_with (fun () -> Stdio_process.send_line process oversized)
         : (unit, exn) result);
      Stdio_process.close_stdin process;
      let result = await_process env process in
      require_exit result (Exited 0) "oversized local input did not close cleanly";
      assert_stdout_pure result.stdout;
      require
        (String.is_substring result.stderr.contents ~substring:"Invalid_request")
        "oversized local input diagnostic was not written to stderr"))
;;

let test_gateway_events transport env environment =
  let suffix =
    match transport with
    | `Unix -> "unix"
    | `Http -> "http"
  in
  let fixture = fixture env environment ("stdio-gateway-" ^ suffix) in
  Eio.Switch.run (fun sw ->
    with_daemon
      ~sw
      env
      fixture
      (Config_fixture.config_path fixture)
      (fun _daemon _health ->
         let arguments = gateway_arguments environment fixture transport in
         with_stdio ~sw env fixture arguments (fun process ->
           let client = Stdio_client.create ~process ~clock:(Eio.Stdenv.clock env) in
           ignore (initialize client : Agent_protocol.Initialize.Response.t);
           let created, create_notifications =
             create_session client ("create-" ^ suffix)
           in
           let attachment =
             Option.value_exn created.attachment
             |> fun (attached : Agent_protocol.Method_result.Attach.t) ->
             attached.attachment
           in
           let started, start_notifications =
             start_session client created.session attachment ("start-" ^ suffix)
           in
           let events =
             collect_through
               client
               created.session.id
               created.session.latest_event_sequence
               started.latest_event_sequence
               (create_notifications @ start_notifications)
               []
           in
           require (not (List.is_empty events)) "stdio gateway delivered no start events";
           let result = finish_process env process in
           require_exit result (Exited 0) "stdio gateway did not exit on EOF";
           assert_stdout_pure result.stdout;
           require
             (String.is_empty result.stderr.contents)
             "valid stdio gateway wrote stderr")))
;;

let test_gateway_unix_events = test_gateway_events `Unix
let test_gateway_http_events = test_gateway_events `Http

let single_attachment_config fixture =
  Config_fixture.configuration fixture ()
  |> String.substr_replace_first
       ~pattern:"(max_attachments_per_session 16)"
       ~with_:"(max_attachments_per_session 1)"
  |> Config_fixture.write_configuration fixture ~name:"single-attachment.sexp"
;;

let attach_direct connection session_id =
  let request =
    Agent_protocol.Session.Attach_request.
      { session_id
      ; requested_mode = Read_only
      ; subscribe = false
      ; after_sequence = None
      ; reclaim_token = None
      ; idempotency_key =
          Agent_protocol.Id.Transaction.create ()
          |> Agent_protocol.Id.Transaction.to_string
          |> idempotency_key
      }
  in
  match Agent_client.Connection.request connection (Session_attach request) with
  | Ok (Session_attach attached) -> Ok attached
  | Ok _ -> Error (Agent_protocol.Error.invalid_request "unexpected direct attach result")
  | Error error -> Error error
;;

let rec await_detached_cleanup env connection session_id deadline =
  match attach_direct connection session_id with
  | Ok attached -> attached
  | Error error when Agent_protocol.Error.equal_code error.code Resource_limit ->
    if Float.(Eio.Time.now (Eio.Stdenv.clock env) >= deadline)
    then
      raise_s
        [%sexp "gateway attachment was not cleaned", (error : Agent_protocol.Error.t)]
    else (
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
      await_detached_cleanup env connection session_id deadline)
  | Error error ->
    raise_s [%sexp "direct cleanup attach failed", (error : Agent_protocol.Error.t)]
;;

let test_gateway_eof_detaches_only env environment =
  let fixture = fixture env environment "stdio-gateway-eof" in
  Eio.Switch.run (fun sw ->
    with_daemon ~sw env fixture (single_attachment_config fixture) (fun _daemon _health ->
      let session_id =
        with_stdio
          ~sw
          env
          fixture
          (gateway_arguments environment fixture `Unix)
          (fun process ->
             let client = Stdio_client.create ~process ~clock:(Eio.Stdenv.clock env) in
             ignore (initialize client : Agent_protocol.Initialize.Response.t);
             let created, _ = create_session client "eof-create" in
             let attachment =
               Option.value_exn created.attachment
               |> fun (attached : Agent_protocol.Method_result.Attach.t) ->
               attached.attachment
             in
             ignore (start_session client created.session attachment "eof-start");
             let result = finish_process env process in
             require_exit result (Exited 0) "gateway did not exit after EOF";
             created.session.id)
      in
      Eio.Switch.run (fun client_sw ->
        let connection =
          Unix_driver.connect
            ~sw:client_sw
            ~env
            ~socket_path:(Config_fixture.unix_socket fixture)
        in
        ignore
          (Unix_driver.initialize connection |> protocol_ok
           : Agent_protocol.Initialize.Response.t);
        let deadline = Eio.Time.now (Eio.Stdenv.clock env) +. 2. in
        ignore (await_detached_cleanup env connection session_id deadline);
        let snapshot =
          Agent_client.Admin.get_session connection session_id |> protocol_ok
        in
        require
          (Agent_protocol.Session.equal_desired_state
             snapshot.session.desired_state
             Running)
          "gateway EOF stopped the detached daemon session";
        Agent_client.Connection.close connection)))
;;

let test_gateway_bad_bearer_file env environment =
  let fixture = fixture env environment "stdio-bad-bearer" in
  let token_file = write_bearer_file environment "invalid.token" "invalid token\n" in
  Eio.Switch.run (fun sw ->
    let result =
      run_stdio_cli
        ~sw
        env
        fixture
        [ "--connect"
        ; sprintf "http://127.0.0.1:%d" (reserve_port env)
        ; "--bearer-token-file"
        ; token_file
        ]
    in
    require_exit result (Exited 1) "invalid bearer-token file was accepted";
    require (String.is_empty result.stdout.contents) "bearer-token error polluted stdout";
    require
      (String.is_substring result.stderr.contents ~substring:"bearer token")
      "bearer-token diagnostic differs")
;;

let test_stdout_purity env environment =
  let fixture = fixture env environment "stdio-stdout-purity" in
  Eio.Switch.run (fun sw ->
    let result = run_stdio_cli ~sw env fixture [ "--local" ] in
    require_exit result (Exited 1) "invalid stdio CLI invocation succeeded";
    require (String.is_empty result.stdout.contents) "CLI diagnostic polluted stdout";
    require (not (String.is_empty result.stderr.contents)) "CLI diagnostic omitted stderr")
;;

let cases =
  [ "stdio.local-initialize", test_local_initialize
  ; "stdio.local-process-bound-eof", test_local_process_bound_eof
  ; "stdio.local-malformed-input", test_local_malformed_input
  ; "stdio.local-oversized-input", test_local_oversized_input
  ; "stdio.gateway-unix-events", test_gateway_unix_events
  ; "stdio.gateway-http-events", test_gateway_http_events
  ; "stdio.gateway-eof-detaches-only", test_gateway_eof_detaches_only
  ; "stdio.gateway-bad-bearer-file", test_gateway_bad_bearer_file
  ; "stdio.stdout-purity", test_stdout_purity
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown stdio case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"stdio-modes" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (_name, test) -> test env environment);
    print_s
      [%sexp
        { scenario = ("stdio-modes" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }])
;;
