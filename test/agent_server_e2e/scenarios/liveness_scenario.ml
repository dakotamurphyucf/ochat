open Core
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Http_driver = Support.Http_driver
module Port_reservation = Support.Port_reservation
module Process_manager = Support.Process_manager
module Stdio_client = Support.Stdio_client
module Stdio_process = Support.Stdio_process
module Temporary_environment = Support.Temporary_environment
module Unix_driver = Support.Unix_driver

type owner_session =
  { id : Agent_protocol.Id.Session.t
  ; attachment_id : Agent_protocol.Id.Attachment.t
  ; lease : Agent_protocol.Session.Owner_lease.t
  ; reclaim_token : string
  }

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
;;

let result_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "E2E operation failed", (error : string)]
;;

let idempotency_key value = Agent_protocol.Idempotency_key.of_string value |> protocol_ok

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

let path environment native_path = Temporary_environment.path environment native_path

let liveness_prompt =
  {|
<developer>Run deterministic liveness events without a provider.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Session_resume | `Wake ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx state event ->
      match event with
      | `Session_start ->
        Task.bind(Schedule.after_ms(250, `Wake), fun schedule_id ->
        Task.pure(state + 1))
      | `Session_resume -> Task.pure(state + 1)
      | `Wake -> Task.pure(state + 1)
</script>
|}
;;

let setup_fixture env environment name =
  let fixture = fixture env environment name in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (path environment (Config_fixture.prompt_path fixture))
    liveness_prompt;
  fixture
;;

let wait_ready daemon env =
  match Daemon_process.wait_ready daemon ~env ~timeout_seconds:5. with
  | Ok _ -> ()
  | Error error ->
    raise_s
      [%sexp
        "daemon did not become ready"
      , (error : Daemon_process.readiness_error)
      , ((Daemon_process.stdout daemon).contents : string)
      , ((Daemon_process.stderr daemon).contents : string)]
;;

let stop_daemon env daemon =
  match Daemon_process.result daemon with
  | Some _ -> ()
  | None -> ignore (Daemon_process.stop daemon ~env ~grace_seconds:1.)
;;

let start_daemon ~sw env fixture =
  let daemon =
    Daemon_process.start
      ~sw
      ~env
      ~fixture
      ~config_path:(Config_fixture.config_path fixture)
  in
  wait_ready daemon env;
  daemon
;;

let connect_unix ~sw env fixture =
  let connection =
    Unix_driver.connect ~sw ~env ~socket_path:(Config_fixture.unix_socket fixture)
  in
  ignore
    (Unix_driver.initialize connection |> protocol_ok
     : Agent_protocol.Initialize.Response.t);
  connection
;;

let unix_request connection command =
  Agent_client.Connection.request connection command |> protocol_ok
;;

let page_request () = Agent_protocol.Page.Request.create ~limit:100 () |> protocol_ok

let unix_catalog connection =
  let prompt = Agent_client.Catalog.prompts connection |> protocol_ok |> List.hd_exn in
  let workspace =
    Agent_client.Catalog.workspaces connection |> protocol_ok |> List.hd_exn
  in
  prompt, workspace
;;

let session_spec ~prompt ~workspace ~liveness =
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog prompt.Agent_protocol.Prompt.id)
    ~workspace:(Configured workspace.Agent_protocol.Workspace.id)
    ~liveness
    ~persistence:Durable
    ~permission_profile:"unattended"
    ~start_immediately:true
    ~labels:[ "suite", "session-liveness" ]
    ()
  |> protocol_ok
;;

let create_detached connection key =
  let prompt, workspace = unix_catalog connection in
  let request =
    Agent_protocol.Session.Create_request.
      { spec = session_spec ~prompt ~workspace ~liveness:Detached
      ; requested_mode = Some Read_write
      ; subscribe = false
      ; idempotency_key = idempotency_key key
      }
  in
  match unix_request connection (Session_create request) with
  | Session_create created -> created
  | _ -> fail "session.create returned the wrong result variant"
;;

let unix_snapshot connection session_id =
  match unix_request connection (Session_get { session_id; history = None }) with
  | Session_get snapshot -> snapshot
  | _ -> fail "session.get returned the wrong result variant"
;;

let schedule_delivered snapshot =
  List.exists snapshot.Agent_protocol.Snapshot.schedules ~f:(fun schedule ->
    match schedule.Agent_protocol.Schedule.status with
    | Delivered -> true
    | Scheduled | Delivering | Cancelled | Failed _ -> false)
;;

let rec await_unix_schedule env connection session_id attempts =
  let snapshot = unix_snapshot connection session_id in
  if schedule_delivered snapshot
  then snapshot
  else if attempts = 0
  then fail "detached schedule was not delivered"
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_unix_schedule env connection session_id (attempts - 1))
;;

let replay_contains_delivery events =
  List.exists events ~f:(fun event ->
    match event.Agent_protocol.Event.Durable.kind with
    | Schedule_state_changed ->
      (match
         Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
       with
       | Ok (Schedule_state_changed schedule) ->
         (match schedule.Agent_protocol.Schedule.status with
          | Delivered -> true
          | Scheduled | Delivering | Cancelled | Failed _ -> false)
       | Ok _ | Error _ -> false)
    | _ -> false)
;;

let attach_after connection session_id sequence key =
  let request =
    Agent_protocol.Session.Attach_request.
      { session_id
      ; requested_mode = Read_only
      ; subscribe = false
      ; after_sequence = Some sequence
      ; reclaim_token = None
      ; idempotency_key = idempotency_key key
      }
  in
  match unix_request connection (Session_attach request) with
  | Session_attach attached -> attached
  | _ -> fail "session.attach returned the wrong result variant"
;;

let test_detached_zero_clients env environment =
  let fixture = setup_fixture env environment "liveness-detached-zero" in
  Eio.Switch.run (fun sw ->
    let daemon = start_daemon ~sw env fixture in
    Exn.protect
      ~f:(fun () ->
        let session_id =
          Eio.Switch.run (fun client_sw ->
            let connection = connect_unix ~sw:client_sw env fixture in
            let created = create_detached connection "detached-zero:create" in
            Agent_client.Connection.close connection;
            created.session.id)
        in
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.35;
        Eio.Switch.run (fun client_sw ->
          let connection = connect_unix ~sw:client_sw env fixture in
          let snapshot = await_unix_schedule env connection session_id 100 in
          require
            (Agent_protocol.Session.equal_desired_state
               snapshot.session.desired_state
               Running)
            "detached session stopped with zero clients";
          Agent_client.Connection.close connection))
      ~finally:(fun () -> stop_daemon env daemon))
;;

let test_detached_reconnect env environment =
  let fixture = setup_fixture env environment "liveness-detached-reconnect" in
  Eio.Switch.run (fun sw ->
    let daemon = start_daemon ~sw env fixture in
    Exn.protect
      ~f:(fun () ->
        let session_id, cursor =
          Eio.Switch.run (fun client_sw ->
            let connection = connect_unix ~sw:client_sw env fixture in
            let created = create_detached connection "detached-reconnect:create" in
            Agent_client.Connection.close connection;
            created.session.id, created.session.latest_event_sequence)
        in
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.35;
        Eio.Switch.run (fun client_sw ->
          let connection = connect_unix ~sw:client_sw env fixture in
          let attached =
            attach_after connection session_id cursor "detached-reconnect:attach"
          in
          (match attached.replay with
           | Events events ->
             require
               (replay_contains_delivery events)
               "reconnect replay omitted background schedule delivery"
           | Current -> fail "reconnect replay unexpectedly reported current"
           | Snapshot _ -> fail "recent reconnect unexpectedly required a snapshot");
          Agent_client.Connection.close connection))
      ~finally:(fun () -> stop_daemon env daemon))
;;

let create_http ~sw env fixture token =
  let client =
    Http_driver.create
      ~sw
      ~env
      ~port:(Config_fixture.http_port fixture)
      ~token:(Some token)
    |> result_ok
  in
  ignore
    (Http_driver.initialize client |> protocol_ok
     : Agent_protocol.Initialize.Response.t * Http_driver.response);
  client
;;

let http_request client command =
  (Http_driver.request client command |> protocol_ok).result
;;

let http_catalog client =
  let prompt_request =
    Agent_protocol.Prompt.List_request.
      { page = page_request (); enabled = Some true; available = Some true }
  in
  let prompts =
    match http_request client (Agent_protocol.Command.Prompt_list prompt_request) with
    | Agent_protocol.Method_result.Prompt_list page -> page.items
    | _ -> fail "prompt.list returned the wrong result variant"
  in
  let workspace_request =
    Agent_protocol.Workspace.List_request.
      { page = page_request (); kind = None; access = None; available = Some true }
  in
  let workspaces =
    match
      http_request client (Agent_protocol.Command.Workspace_list workspace_request)
    with
    | Agent_protocol.Method_result.Workspace_list page -> page.items
    | _ -> fail "workspace.list returned the wrong result variant"
  in
  List.hd_exn prompts, List.hd_exn workspaces
;;

let create_owner client ~key ~grace_ms =
  let prompt, workspace = http_catalog client in
  let liveness =
    Agent_protocol.Session.Owner_bound
      { disconnect_grace_ms = grace_ms; stop_mode = Graceful }
  in
  let request =
    Agent_protocol.Session.Create_request.
      { spec = session_spec ~prompt ~workspace ~liveness
      ; requested_mode = Some Owner_read_write
      ; subscribe = false
      ; idempotency_key = idempotency_key (key ^ ":create")
      }
  in
  match http_request client (Session_create request) with
  | Session_create { session; attachment = Some attached; _ } ->
    { id = session.id
    ; attachment_id = attached.attachment.id
    ; lease = Option.value_exn attached.attachment.owner_lease
    ; reclaim_token = Option.value_exn attached.reclaim_token
    }
  | _ -> fail "owner session.create returned the wrong result variant"
;;

let close_http client =
  ignore (Http_driver.close_connection client |> result_ok : Http_driver.response);
  Http_driver.shutdown client
;;

let attach_owner_result client session_id reclaim_token key =
  let request =
    Agent_protocol.Session.Attach_request.
      { session_id
      ; requested_mode = Owner_read_write
      ; subscribe = false
      ; after_sequence = None
      ; reclaim_token = Some reclaim_token
      ; idempotency_key = idempotency_key key
      }
  in
  Http_driver.request client (Session_attach request)
;;

let attach_owner client session_id reclaim_token key =
  match attach_owner_result client session_id reclaim_token key |> protocol_ok with
  | { result = Session_attach attached; _ } -> attached
  | _ -> fail "owner session.attach returned the wrong result variant"
;;

let renew_owner client owner key =
  let request =
    Agent_protocol.Session.Renew_owner_request.
      { session_id = owner.id
      ; attachment_id = owner.attachment_id
      ; lease_generation = owner.lease.generation
      ; idempotency_key = idempotency_key key
      }
  in
  match http_request client (Session_renew_owner request) with
  | Session_renew_owner (lease, _) -> lease
  | _ -> fail "session.renew_owner returned the wrong result variant"
;;

let http_snapshot client session_id =
  match http_request client (Session_get { session_id; history = None }) with
  | Session_get snapshot -> snapshot
  | _ -> fail "HTTP session.get returned the wrong result variant"
;;

let rec await_stopped env client session_id attempts =
  let snapshot = http_snapshot client session_id in
  let observed_stopped =
    match snapshot.session.observed_state with
    | Stopped -> true
    | Queued_for_slot
    | Starting
    | Recovering
    | Idle
    | Running_turn _
    | Compacting _
    | Waiting_for_permission _
    | Stopping
    | Failed _ -> false
  in
  let stopped =
    Agent_protocol.Session.equal_desired_state snapshot.session.desired_state Stopped
    && observed_stopped
  in
  if stopped
  then snapshot
  else if attempts = 0
  then fail "owner-bound session did not stop after grace"
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_stopped env client session_id (attempts - 1))
;;

let with_owner_daemon env environment name f =
  let fixture = setup_fixture env environment name in
  Config_fixture.grant_public_all_scopes fixture;
  Eio.Switch.run (fun sw ->
    let daemon = start_daemon ~sw env fixture in
    Exn.protect
      ~f:(fun () -> f sw fixture daemon)
      ~finally:(fun () -> stop_daemon env daemon))
;;

let test_owner_renew_generation env environment =
  with_owner_daemon env environment "liveness-owner-renew" (fun sw fixture _daemon ->
    let client = create_http ~sw env fixture (Config_fixture.admin_token fixture) in
    let owner = create_owner client ~key:"owner-renew" ~grace_ms:1_000 in
    let renewed = renew_owner client owner "owner-renew:renew" in
    require
      Int64.(renewed.generation > owner.lease.generation)
      "owner renewal did not advance lease generation";
    close_http client)
;;

let reclaim_once ~sw env fixture =
  let first = create_http ~sw env fixture (Config_fixture.admin_token fixture) in
  let owner = create_owner first ~key:"owner-reclaim" ~grace_ms:2_000 in
  close_http first;
  let second = create_http ~sw env fixture (Config_fixture.public_token fixture) in
  let attached =
    attach_owner second owner.id owner.reclaim_token "owner-reclaim:second"
  in
  owner, second, attached
;;

let test_owner_token_rotation env environment =
  with_owner_daemon env environment "liveness-owner-rotation" (fun sw fixture _daemon ->
    let owner, second, attached = reclaim_once ~sw env fixture in
    let rotated = Option.value_exn attached.reclaim_token in
    let lease = Option.value_exn attached.attachment.owner_lease in
    require
      (not (String.equal owner.reclaim_token rotated))
      "owner reclaim token did not rotate";
    require
      Int64.(lease.generation > owner.lease.generation)
      "owner reclaim did not advance generation";
    close_http second)
;;

let test_owner_old_token_rejected env environment =
  with_owner_daemon
    env
    environment
    "liveness-owner-stale-token"
    (fun sw fixture _daemon ->
       let owner, second, attached = reclaim_once ~sw env fixture in
       let rotated = Option.value_exn attached.reclaim_token in
       close_http second;
       let third = create_http ~sw env fixture (Config_fixture.admin_token fixture) in
       (match
          attach_owner_result third owner.id owner.reclaim_token "owner-reclaim:stale"
        with
        | Error error ->
          require
            (Agent_protocol.Error.equal_code error.code Permission_denied)
            "stale owner token returned the wrong error"
        | Ok _ -> fail "stale owner reclaim token was accepted");
       ignore
         (attach_owner third owner.id rotated "owner-reclaim:current"
          : Agent_protocol.Method_result.Attach.t);
       close_http third)
;;

let test_owner_grace_expiry env environment =
  with_owner_daemon env environment "liveness-owner-expiry" (fun sw fixture _daemon ->
    let owner_client = create_http ~sw env fixture (Config_fixture.admin_token fixture) in
    let owner = create_owner owner_client ~key:"owner-expiry" ~grace_ms:150 in
    close_http owner_client;
    let observer = create_http ~sw env fixture (Config_fixture.admin_token fixture) in
    ignore (await_stopped env observer owner.id 150 : Agent_protocol.Snapshot.t);
    close_http observer)
;;

let test_owner_restart_during_grace env environment =
  let fixture = setup_fixture env environment "liveness-owner-restart" in
  Config_fixture.grant_public_all_scopes fixture;
  Eio.Switch.run (fun sw ->
    let first_daemon = start_daemon ~sw env fixture in
    let first = create_http ~sw env fixture (Config_fixture.admin_token fixture) in
    let owner = create_owner first ~key:"owner-restart" ~grace_ms:2_000 in
    close_http first;
    stop_daemon env first_daemon;
    let second_daemon = start_daemon ~sw env fixture in
    Exn.protect
      ~f:(fun () ->
        let observer = create_http ~sw env fixture (Config_fixture.admin_token fixture) in
        let before = http_snapshot observer owner.id in
        require
          (not
             (Agent_protocol.Session.equal_desired_state
                before.session.desired_state
                Stopped))
          "restart discarded owner disconnect grace";
        ignore (await_stopped env observer owner.id 200 : Agent_protocol.Snapshot.t);
        close_http observer)
      ~finally:(fun () -> stop_daemon env second_daemon))
;;

let absolute _env native_path =
  if Filename.is_absolute native_path
  then native_path
  else Eio_posix.Low_level.realpath native_path
;;

let stdio_executable env =
  Sys.getenv "OCHAT_E2E_STDIO_EXE"
  |> Option.value
       ~default:
         (Filename.concat
            (Eio.Path.native_exn (Eio.Stdenv.cwd env))
            "_build/default/bin/ochat_agent_stdio.exe")
  |> absolute env
;;

let child_environment fixture =
  Temporary_environment.child_environment
    (Config_fixture.environment fixture)
    ~base:(Core_unix.environment ())
;;

let test_process_bound_stdio env environment =
  let fixture = setup_fixture env environment "liveness-process-stdio" in
  Eio.Switch.run (fun sw ->
    let process =
      Stdio_process.spawn
        ~sw
        ~env
        ~environment:(child_environment fixture)
        ~max_output_bytes:(1024 * 1024)
        [ stdio_executable env
        ; "--local"
        ; "--prompt"
        ; Config_fixture.prompt_path fixture
        ; "--workspace"
        ; Config_fixture.physical_workspace fixture
        ; "--data-root"
        ; Config_fixture.data_dir fixture
        ]
    in
    let client = Stdio_client.create ~process ~clock:(Eio.Stdenv.clock env) in
    ignore
      (Stdio_client.initialize client |> protocol_ok
       : Agent_protocol.Initialize.Response.t * Agent_protocol.Envelope.t list);
    Stdio_process.close_stdin process;
    let result = Stdio_process.await process in
    require
      (Process_manager.equal_exit result.exit (Exited 0))
      "local process-bound stdio did not exit on EOF")
;;

let tui_executable env =
  Sys.getenv "OCHAT_E2E_TUI_EXE"
  |> Option.value
       ~default:
         (Filename.concat
            (Eio.Path.native_exn (Eio.Stdenv.cwd env))
            "_build/default/bin/chat_tui.exe")
  |> absolute env
;;

let pty_executable env =
  let candidate = "/usr/bin/script" in
  if Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / candidate)
  then candidate
  else fail "the script PTY utility is unavailable"
;;

let shell_quote value =
  "'" ^ String.substr_replace_all value ~pattern:"'" ~with_:"'\\''" ^ "'"
;;

let util_linux_script ~sw env script =
  let result =
    Process_manager.spawn ~sw ~env ~max_output_bytes:16_384 [ script; "-V" ]
    |> Process_manager.await
  in
  Process_manager.equal_exit result.exit (Exited 0)
;;

let pty_argv ~sw env target =
  let script = pty_executable env in
  if util_linux_script ~sw env script
  then
    [ script
    ; "-q"
    ; "-e"
    ; "-c"
    ; List.map target ~f:shell_quote |> String.concat ~sep:" "
    ; "/dev/null"
    ]
  else script :: "-q" :: "/dev/null" :: target
;;

let terminal_environment fixture =
  child_environment fixture
  |> Array.filter ~f:(Fn.non (String.is_prefix ~prefix:"TERM="))
  |> Fn.flip Array.append [| "TERM=xterm-256color" |]
;;

let rec await_tui_ready env process attempts =
  let stdout = (Stdio_process.stdout process).contents in
  if String.is_substring stdout ~substring:"\027[?1049h"
  then ()
  else if attempts = 0
  then
    raise_s
      [%sexp
        "local TUI did not enter its terminal screen"
      , (Stdio_process.poll_result process : Process_manager.result option)
      , (stdout : string)
      , ((Stdio_process.stderr process).contents : string)]
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_tui_ready env process (attempts - 1))
;;

let test_process_bound_tui env environment =
  let fixture = setup_fixture env environment "liveness-process-tui" in
  Eio.Switch.run (fun sw ->
    let target =
      [ tui_executable env
      ; "--no-config"
      ; "--local"
      ; "-file"
      ; Config_fixture.prompt_path fixture
      ]
    in
    let process =
      Stdio_process.spawn
        ~sw
        ~env
        ~cwd:(path environment (Config_fixture.physical_workspace fixture))
        ~environment:(terminal_environment fixture)
        ~max_output_bytes:(2 * 1024 * 1024)
        (pty_argv ~sw env target)
    in
    Stdio_process.send_string process "q";
    await_tui_ready env process 250;
    let termination =
      Stdio_process.terminate process ~clock:(Eio.Stdenv.clock env) ~grace_seconds:1.
    in
    require (not termination.forced) "local process-bound TUI required a forced kill")
;;

let cases =
  [ "detached.zero-clients-continues", test_detached_zero_clients
  ; "detached.reconnect-observes-work", test_detached_reconnect
  ; "owner.renew-generation", test_owner_renew_generation
  ; "owner.reclaim-token-rotation", test_owner_token_rotation
  ; "owner.old-token-rejected", test_owner_old_token_rejected
  ; "owner.grace-expiry-stop", test_owner_grace_expiry
  ; "owner.restart-during-grace", test_owner_restart_during_grace
  ; "process-bound.local-stdio-exit", test_process_bound_stdio
  ; "process-bound.local-tui-exit", test_process_bound_tui
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown liveness case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"session-liveness" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (name, test) ->
      try test env environment with
      | exn -> raise_s [%sexp "liveness E2E case failed", (name : string), (exn : Exn.t)]);
    print_s
      [%sexp
        { scenario = ("session-liveness" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }])
;;
