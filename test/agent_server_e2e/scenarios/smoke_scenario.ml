open Core
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Port_reservation = Support.Port_reservation
module Process_manager = Support.Process_manager
module Temporary_environment = Support.Temporary_environment

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require is_satisfied message = if not is_satisfied then fail message
let atom value = Sexp.to_string_mach (Sexp.Atom value)

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

let run_cli env fixture arguments =
  Eio.Switch.run (fun sw -> Daemon_process.run_cli ~sw ~env ~fixture ~arguments)
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

let validate env fixture path = run_cli env fixture [ "-config"; path; "-validate-only" ]

let test_valid_validate_only env environment =
  let fixture = fixture env environment "valid" in
  let result = validate env fixture (Config_fixture.config_path fixture) in
  require_exit result (Exited 0) "valid configuration was rejected";
  require
    (String.is_substring result.stdout.contents ~substring:"configuration is valid")
    "validation success message is missing";
  require (String.is_empty result.stderr.contents) "valid configuration wrote stderr"
;;

let test_print_normalized env environment =
  let fixture = fixture env environment "normalized" in
  let result =
    run_cli env fixture [ "-config"; Config_fixture.config_path fixture; "-print-config" ]
  in
  require_exit result (Exited 0) "normalized configuration failed";
  List.iter
    [ Config_fixture.data_dir fixture
    ; Config_fixture.unix_socket fixture
    ; Config_fixture.prompt_path fixture
    ; Config_fixture.physical_workspace fixture
    ]
    ~f:(fun path ->
      require
        (String.is_substring result.stdout.contents ~substring:path)
        ("normalized configuration omitted " ^ path));
  require
    (not
       (String.is_substring
          result.stdout.contents
          ~substring:(Config_fixture.admin_token fixture)))
    "normalized configuration disclosed a bearer token"
;;

let validate_replacement env fixture ~name ~pattern ~with_ =
  let contents = Config_fixture.configuration fixture () in
  let replaced = String.substr_replace_first contents ~pattern ~with_ in
  let path = Config_fixture.write_configuration fixture ~name replaced in
  validate env fixture path
;;

let test_missing_prompt env environment =
  let fixture = fixture env environment "missing-prompt" in
  let result =
    validate_replacement
      env
      fixture
      ~name:"missing-prompt.sexp"
      ~pattern:(atom (Config_fixture.prompt_path fixture))
      ~with_:(atom (Config_fixture.prompt_path fixture ^ ".missing"))
  in
  require_exit result (Exited 2) "missing prompt did not fail validation";
  require
    (String.is_substring result.stderr.contents ~substring:"config.prompt_unavailable")
    "missing prompt diagnostic code differs"
;;

let test_missing_workspace env environment =
  let fixture = fixture env environment "missing-workspace" in
  let result =
    validate_replacement
      env
      fixture
      ~name:"missing-workspace.sexp"
      ~pattern:(atom (Config_fixture.physical_workspace fixture))
      ~with_:(atom (Config_fixture.physical_workspace fixture ^ ".missing"))
  in
  require_exit result (Exited 2) "missing workspace did not fail validation";
  require
    (String.is_substring result.stderr.contents ~substring:"config.workspace_unavailable")
    "missing workspace diagnostic code differs"
;;

let test_unknown_field env environment =
  let fixture = fixture env environment "unknown-field" in
  let result =
    validate_replacement
      env
      fixture
      ~name:"unknown-field.sexp"
      ~pattern:"(shutdown_grace_ms 1000)"
      ~with_:"(shutdown_grace_ms 1000) (invented_field true)"
  in
  require_exit result (Exited 2) "unknown field did not fail validation";
  require
    (String.is_substring result.stderr.contents ~substring:"config.unknown_field")
    "unknown-field diagnostic code differs"
;;

let test_invalid_retention env environment =
  let fixture = fixture env environment "invalid-retention" in
  let result =
    validate_replacement
      env
      fixture
      ~name:"invalid-retention.sexp"
      ~pattern:"(response_artifact_ms 60000)"
      ~with_:"(response_artifact_ms 0)"
  in
  require_exit result (Exited 2) "invalid retention did not fail validation";
  require
    (String.is_substring result.stderr.contents ~substring:"config.range")
    "invalid-retention diagnostic code differs"
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

let start_ready_exn ~sw env fixture config_path =
  let daemon = Daemon_process.start ~sw ~env ~fixture ~config_path in
  match Daemon_process.wait_ready daemon ~env ~timeout_seconds:5. with
  | Ok health -> daemon, health
  | Error error -> readiness_failure daemon error
;;

let stop_exn env daemon =
  let termination = Daemon_process.stop daemon ~env ~grace_seconds:3. in
  require (not termination.forced) "daemon required forced shutdown";
  require_exit termination.result (Exited 0) "daemon did not exit cleanly";
  termination.result
;;

let stop_if_running env daemon =
  match Daemon_process.result daemon with
  | Some _ -> ()
  | None -> ignore (Daemon_process.stop daemon ~env ~grace_seconds:1.)
;;

let with_running_daemon env fixture config_path f =
  Eio.Switch.run (fun sw ->
    let daemon, health = start_ready_exn ~sw env fixture config_path in
    Exn.protect
      ~f:(fun () -> f daemon health)
      ~finally:(fun () -> stop_if_running env daemon))
;;

let expected_components =
  String.Set.of_list
    [ "daemon"
    ; "storage"
    ; "session_registry"
    ; "start_scheduler"
    ; "job_scheduler"
    ; "schedule_scheduler"
    ; "permission_scheduler"
    ; "maintenance"
    ; "configuration"
    ]
;;

let assert_detailed_health health =
  require health.Agent_protocol.Health.Response.ready "health did not report ready";
  require (not health.draining) "ready health reported draining";
  require
    (Agent_protocol.Health.equal_status health.status Healthy)
    "aggregate health was not healthy";
  let names =
    List.map health.components ~f:(fun component -> component.name) |> String.Set.of_list
  in
  require (Set.equal names expected_components) "health component set differs";
  require
    (List.for_all health.components ~f:(fun component ->
       Agent_protocol.Health.equal_status component.status Healthy))
    "one or more health components were not healthy"
;;

let assert_socket_kind environment path expected message =
  require
    (Poly.equal
       (Eio.Path.kind ~follow:false (Temporary_environment.path environment path))
       expected)
    message
;;

let test_ready_health env environment =
  let fixture = fixture env environment "ready-health" in
  with_running_daemon
    env
    fixture
    (Config_fixture.config_path fixture)
    (fun daemon health ->
       assert_detailed_health health;
       assert_socket_kind
         environment
         (Config_fixture.unix_socket fixture)
         `Socket
         "ready daemon Unix socket is missing";
       let public =
         Daemon_process.health daemon ~env ~token:(Config_fixture.public_token fixture)
         |> Result.ok_or_failwith
       in
       require (List.is_empty public.components) "public health disclosed components";
       ignore (stop_exn env daemon : Process_manager.result);
       assert_socket_kind
         environment
         (Config_fixture.unix_socket fixture)
         `Not_found
         "graceful shutdown left the Unix socket")
;;

let test_insecure_socket_parent env environment =
  let fixture = fixture env environment "insecure-socket" in
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let parent = Filename.concat roots.root "public-sockets" in
  Eio.Path.mkdir ~perm:0o755 (Temporary_environment.path environment parent);
  let socket = Filename.concat parent "agent.sock" in
  let config = Config_fixture.configuration fixture ~unix_socket:socket () in
  let path = Config_fixture.write_configuration fixture ~name:"insecure.sexp" config in
  let result = run_cli env fixture [ "-config"; path ] in
  require_exit result (Exited 1) "insecure socket parent did not fail startup";
  require
    (String.is_substring
       result.stderr.contents
       ~substring:"must not be accessible to group or other users")
    "insecure socket-parent diagnostic differs"
;;

let test_data_root_lock_contention env environment =
  let first = fixture env environment "lock-first" in
  let second = fixture env environment "lock-second" in
  let second_config =
    Config_fixture.configuration second ~data_dir:(Config_fixture.data_dir first) ()
  in
  let second_path =
    Config_fixture.write_configuration second ~name:"shared-data.sexp" second_config
  in
  with_running_daemon env first (Config_fixture.config_path first) (fun _daemon _health ->
    let result = run_cli env second [ "-config"; second_path ] in
    require_exit result (Exited 1) "second daemon acquired the live data root";
    require
      (String.is_substring result.stderr.contents ~substring:"lock")
      "data-root contention diagnostic omitted lock ownership")
;;

let test_live_socket_refusal env environment =
  let first = fixture env environment "socket-first" in
  let second = fixture env environment "socket-second" in
  let second_config =
    Config_fixture.configuration second ~unix_socket:(Config_fixture.unix_socket first) ()
  in
  let second_path =
    Config_fixture.write_configuration second ~name:"shared-socket.sexp" second_config
  in
  with_running_daemon env first (Config_fixture.config_path first) (fun _daemon _health ->
    let result = run_cli env second [ "-config"; second_path ] in
    require_exit result (Exited 1) "second daemon acquired the live Unix socket";
    require
      (String.is_substring
         result.stderr.contents
         ~substring:"Unix socket already has a live listener")
      "live-socket diagnostic differs")
;;

let create_stale_socket env environment target =
  let source = target ^ ".source" in
  ignore
    (Result.try_with (fun () ->
       Eio.Switch.run (fun sw ->
         ignore
           (Eio.Net.listen
              ~sw
              ~reuse_addr:true
              ~backlog:1
              (Eio.Stdenv.net env)
              (`Unix source)
            : _ Eio.Net.listening_socket);
         Eio.Path.rename
           (Temporary_environment.path environment source)
           (Temporary_environment.path environment target)))
     : (unit, exn) result)
;;

let test_stale_socket_recovery env environment =
  let fixture = fixture env environment "stale-socket" in
  let socket = Config_fixture.unix_socket fixture in
  create_stale_socket env environment socket;
  assert_socket_kind environment socket `Socket "stale socket fixture was not created";
  with_running_daemon
    env
    fixture
    (Config_fixture.config_path fixture)
    (fun daemon health ->
       assert_detailed_health health;
       assert_socket_kind environment socket `Socket "daemon did not replace stale socket";
       ignore (stop_exn env daemon : Process_manager.result));
  assert_socket_kind environment socket `Not_found "recovered socket survived shutdown"
;;

let run_one_daemon env fixture =
  Eio.Switch.run (fun sw ->
    let daemon, health =
      start_ready_exn ~sw env fixture (Config_fixture.config_path fixture)
    in
    assert_detailed_health health;
    stop_exn env daemon)
;;

let test_graceful_sigterm env environment =
  let fixture = fixture env environment "graceful" in
  ignore (run_one_daemon env fixture : Process_manager.result);
  assert_socket_kind
    environment
    (Config_fixture.unix_socket fixture)
    `Not_found
    "first shutdown left the Unix socket";
  ignore (run_one_daemon env fixture : Process_manager.result);
  assert_socket_kind
    environment
    (Config_fixture.unix_socket fixture)
    `Not_found
    "restart shutdown left the Unix socket"
;;

let cases =
  [ "config.valid-validate-only", test_valid_validate_only
  ; "config.print-normalized", test_print_normalized
  ; "config.missing-prompt", test_missing_prompt
  ; "config.missing-workspace", test_missing_workspace
  ; "config.unknown-field", test_unknown_field
  ; "config.insecure-socket-parent", test_insecure_socket_parent
  ; "config.invalid-retention", test_invalid_retention
  ; "daemon.ready-health", test_ready_health
  ; "daemon.data-root-lock-contention", test_data_root_lock_contention
  ; "daemon.live-socket-refusal", test_live_socket_refusal
  ; "daemon.stale-socket-recovery", test_stale_socket_recovery
  ; "daemon.graceful-sigterm", test_graceful_sigterm
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown daemon-smoke case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"daemon-smoke" ~env (fun environment ->
    List.iter (select case) ~f:(fun (_name, test) -> test env environment);
    print_s
      [%sexp
        { scenario = ("daemon-smoke" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map (select case) ~f:fst : string list)
        }])
;;
