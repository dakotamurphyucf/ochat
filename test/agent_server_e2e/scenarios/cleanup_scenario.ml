open Core
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Http_probe = Support.Http_probe
module Port_reservation = Support.Port_reservation
module Temporary_environment = Support.Temporary_environment
module Unix_driver = Support.Unix_driver

type location =
  | Physical
  | System_tmp
  | Session_dir

type cleanup =
  | On_stop
  | On_delete
  | Retain

type session =
  { id : Agent_protocol.Id.Session.t
  ; mutable attachment_id : Agent_protocol.Id.Attachment.t
  ; mutable revision : int64
  }

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
;;

let request connection command =
  Agent_client.Connection.request connection command |> protocol_ok
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

let atom value = Sexp.to_string_mach (Sexp.Atom value)

let source_sexp fixture managed_root location cleanup =
  let cleanup =
    match cleanup with
    | On_stop -> "on_session_stop"
    | On_delete -> "on_session_delete"
    | Retain -> "retain"
  in
  match location with
  | Physical -> sprintf "(physical %s)" (atom (Config_fixture.physical_workspace fixture))
  | Session_dir -> sprintf "(temporary ((location session_dir) (cleanup %s)))" cleanup
  | System_tmp ->
    sprintf
      "(temporary ((location system_tmp) (cleanup %s) (managed_root %s)))"
      cleanup
      (atom managed_root)
;;

let workspace_section fixture managed_root location cleanup =
  sprintf
    {|
(workspaces
 (((id cleanup)
   (source %s)
   (access exclusive)
   (prompt_limits (((prompt smoke) (max_root_agents 1) (overflow reject)))))))|}
    (source_sexp fixture managed_root location cleanup)
;;

let replace_section contents ~section ~next replacement =
  let start_marker = "(" ^ section in
  let next_marker = "\n(" ^ next in
  let start = String.substr_index_exn contents ~pattern:start_marker in
  let suffix = String.drop_prefix contents start in
  let finish = String.substr_index_exn suffix ~pattern:next_marker + start in
  String.prefix contents start ^ replacement ^ String.drop_prefix contents finish
;;

let configure fixture managed_root location cleanup =
  let environment = Config_fixture.environment fixture in
  let contents = Config_fixture.configuration fixture () in
  let contents =
    replace_section
      contents
      ~section:"workspaces"
      ~next:"prompts"
      (workspace_section fixture managed_root location cleanup)
    |> String.substr_replace_all
         ~pattern:"(allowed_workspaces (physical temporary))"
         ~with_:"(allowed_workspaces (cleanup))"
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    (Temporary_environment.path environment (Config_fixture.config_path fixture))
    contents
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

let require_maintenance_running env fixture =
  let health =
    match
      Http_probe.get_health
        env
        ~port:(Config_fixture.http_port fixture)
        ~token:(Config_fixture.admin_token fixture)
    with
    | Ok health -> health
    | Error message -> raise_s [%sexp "health request failed", (message : string)]
  in
  let maintenance =
    List.find health.components ~f:(fun component ->
      String.equal component.Agent_protocol.Health.Component.name "maintenance")
  in
  require
    (Option.value_map maintenance ~default:false ~f:(fun component ->
       Agent_protocol.Health.equal_status component.status Healthy))
    "daemon maintenance service is not healthy"
;;

let with_daemon env fixture f =
  Eio.Switch.run (fun sw ->
    let daemon =
      Daemon_process.start
        ~sw
        ~env
        ~fixture
        ~config_path:(Config_fixture.config_path fixture)
    in
    Exn.protect
      ~f:(fun () ->
        wait_ready daemon env;
        let connection =
          Unix_driver.connect ~sw ~env ~socket_path:(Config_fixture.unix_socket fixture)
        in
        Exn.protect
          ~f:(fun () ->
            ignore
              (Unix_driver.initialize connection |> protocol_ok
               : Agent_protocol.Initialize.Response.t);
            f connection)
          ~finally:(fun () -> Agent_client.Connection.close connection))
      ~finally:(fun () -> stop_daemon env daemon))
;;

let catalog connection =
  let prompts = Agent_client.Catalog.prompts connection |> protocol_ok in
  let workspaces = Agent_client.Catalog.workspaces connection |> protocol_ok in
  let prompt = List.find_exn prompts ~f:(fun value -> String.equal value.name "smoke") in
  let workspace =
    List.find_exn workspaces ~f:(fun value -> String.equal value.name "cleanup")
  in
  prompt, workspace
;;

let create_session connection ~key ~start_immediately =
  let prompt, workspace = catalog connection in
  let spec =
    Agent_protocol.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog prompt.id)
      ~workspace:(Configured workspace.id)
      ~liveness:Detached
      ~persistence:Durable
      ~permission_profile:"unattended"
      ~start_immediately
      ~labels:[ "suite", "workspace-cleanup" ]
      ()
    |> protocol_ok
  in
  let command_request =
    Agent_protocol.Session.Create_request.
      { spec
      ; requested_mode = Some Read_write
      ; subscribe = false
      ; idempotency_key = idempotency_key (key ^ ":create")
      }
  in
  match request connection (Session_create command_request) with
  | Session_create created ->
    let attachment = Option.value_exn created.attachment in
    { id = created.session.id
    ; attachment_id = attachment.attachment.id
    ; revision = created.session.revision
    }
  | _ -> fail "session.create returned the wrong result variant"
;;

let attach connection session ~key =
  let command_request =
    Agent_protocol.Session.Attach_request.
      { session_id = session.id
      ; requested_mode = Read_write
      ; subscribe = false
      ; after_sequence = None
      ; reclaim_token = None
      ; idempotency_key = idempotency_key (key ^ ":attach")
      }
  in
  match request connection (Session_attach command_request) with
  | Session_attach attached -> session.attachment_id <- attached.attachment.id
  | _ -> fail "session.attach returned the wrong result variant"
;;

let get_session connection session =
  match request connection (Session_get { session_id = session.id; history = None }) with
  | Session_get snapshot ->
    session.revision <- snapshot.session.revision;
    snapshot.session
  | _ -> fail "session.get returned the wrong result variant"
;;

let stop_session_result connection session ~key =
  let command_request =
    Agent_protocol.Session.Stop_request.
      { session_id = session.id
      ; attachment_id = session.attachment_id
      ; mode = Graceful
      ; idempotency_key = idempotency_key (key ^ ":stop")
      }
  in
  Agent_client.Connection.request connection (Session_stop command_request)
;;

let stop_session connection session ~key =
  match stop_session_result connection session ~key |> protocol_ok with
  | Session_stop stopped -> session.revision <- stopped.session.revision
  | _ -> fail "session.stop returned the wrong result variant"
;;

let start_session connection session ~key =
  let command_request =
    Agent_protocol.Session.Start_request.
      { session_id = session.id
      ; attachment_id = session.attachment_id
      ; queue_if_limited = false
      ; idempotency_key = idempotency_key (key ^ ":start")
      }
  in
  match request connection (Session_start command_request) with
  | Session_start started -> session.revision <- started.session.revision
  | _ -> fail "session.start returned the wrong result variant"
;;

let reset_session connection session ~key ~keep_workspace =
  let command_request =
    Agent_protocol.Session.Reset_request.
      { session_id = session.id
      ; attachment_id = session.attachment_id
      ; expected_revision = session.revision
      ; keep_history = true
      ; keep_tasks = true
      ; keep_cache = true
      ; keep_workspace
      ; keep_grants = true
      ; keep_labels = true
      ; idempotency_key = idempotency_key (key ^ ":reset")
      }
  in
  match request connection (Session_reset command_request) with
  | Session_reset reset -> session.revision <- reset.session.revision
  | _ -> fail "session.reset returned the wrong result variant"
;;

let rebuild_session connection session ~key =
  let command_request =
    Agent_protocol.Session.Rebuild_request.
      { session_id = session.id
      ; attachment_id = session.attachment_id
      ; expected_revision = session.revision
      ; prompt_choice = Pinned
      ; idempotency_key = idempotency_key (key ^ ":rebuild")
      }
  in
  match request connection (Session_rebuild command_request) with
  | Session_rebuild rebuilt -> session.revision <- rebuilt.session.revision
  | _ -> fail "session.rebuild returned the wrong result variant"
;;

let delete_session connection session ~key ~policy =
  let command_request =
    Agent_protocol.Session.Delete_request.
      { session_id = session.id
      ; attachment_id = session.attachment_id
      ; expected_revision = session.revision
      ; policy
      ; confirmation = Agent_protocol.Id.Session.to_string session.id
      ; idempotency_key = idempotency_key (key ^ ":delete")
      }
  in
  match request connection (Session_delete command_request) with
  | Session_delete _ -> ()
  | _ -> fail "session.delete returned the wrong result variant"
;;

let path environment value = Temporary_environment.path environment value

let exists environment value =
  match Eio.Path.kind ~follow:false (path environment value) with
  | `Not_found -> false
  | `Directory
  | `Regular_file
  | `Symbolic_link
  | `Unknown
  | `Socket
  | `Fifo
  | `Character_special
  | `Block_device -> true
;;

let save environment filename contents =
  Eio.Path.save ~create:(`Exclusive 0o600) (path environment filename) contents
;;

let session_directory fixture session =
  Filename.concat
    (Filename.concat (Config_fixture.data_dir fixture) "sessions")
    (Agent_protocol.Id.Session.to_string session.id)
;;

let session_workspace fixture session =
  Filename.concat (session_directory fixture session) "workspace"
;;

let child_directories environment root =
  Eio.Path.read_dir (path environment root)
  |> List.filter_map ~f:(fun name ->
    let child = Filename.concat root name in
    match Eio.Path.kind ~follow:false (path environment child) with
    | `Directory -> Some child
    | `Regular_file
    | `Symbolic_link
    | `Not_found
    | `Unknown
    | `Socket
    | `Fifo
    | `Character_special
    | `Block_device -> None)
;;

let only_system_workspace environment managed_root =
  match child_directories environment managed_root with
  | [ workspace ] -> workspace
  | workspaces ->
    raise_s [%sexp "expected one managed workspace", (workspaces : string list)]
;;

let setup env environment name location cleanup =
  let fixture = fixture env environment name in
  let managed_root =
    Filename.concat (Temporary_environment.roots environment).workspaces name
  in
  Eio.Path.mkdir ~perm:0o700 (path environment managed_root);
  configure fixture managed_root location cleanup;
  fixture, managed_root
;;

let marker workspace =
  Filename.concat workspace Agent_session.Workspace_resolver.ownership_marker
;;

let test_system_stop env environment =
  let fixture, managed_root =
    setup env environment "cleanup-system-stop" System_tmp On_stop
  in
  let session, replacement =
    with_daemon env fixture (fun connection ->
      let session =
        create_session connection ~key:"system-stop" ~start_immediately:true
      in
      let workspace = only_system_workspace environment managed_root in
      save environment (Filename.concat workspace "sentinel") "old";
      stop_session connection session ~key:"system-stop";
      require (not (exists environment workspace)) "on-stop system workspace survived";
      let replacement = only_system_workspace environment managed_root in
      require
        (exists environment (marker replacement))
        "replacement workspace lacks ownership marker";
      session, replacement)
  in
  with_daemon env fixture (fun connection ->
    attach connection session ~key:"system-stop-restart";
    let state = get_session connection session in
    require
      (Agent_protocol.Session.equal_desired_state state.desired_state Stopped)
      "stopped session did not recover";
    require
      (exists environment replacement)
      "replacement workspace did not survive restart";
    start_session connection session ~key:"system-stop-restart";
    stop_session connection session ~key:"system-stop-restart";
    require (not (exists environment replacement)) "second stop did not rotate workspace")
;;

let test_system_delete env environment =
  let fixture, managed_root =
    setup env environment "cleanup-system-delete" System_tmp On_delete
  in
  with_daemon env fixture (fun connection ->
    let session =
      create_session connection ~key:"system-delete" ~start_immediately:true
    in
    let workspace = only_system_workspace environment managed_root in
    save environment (Filename.concat workspace "sentinel") "delete";
    stop_session connection session ~key:"system-delete";
    require (exists environment workspace) "on-delete workspace disappeared on stop";
    delete_session connection session ~key:"system-delete" ~policy:Remove;
    require
      (not (exists environment workspace))
      "on-delete system workspace survived deletion")
;;

let test_session_dir_stop env environment =
  let fixture, _ = setup env environment "cleanup-session-stop" Session_dir On_stop in
  let session =
    with_daemon env fixture (fun connection ->
      let session =
        create_session connection ~key:"session-stop" ~start_immediately:true
      in
      let workspace = session_workspace fixture session in
      let sentinel = Filename.concat workspace "sentinel" in
      save environment sentinel "stop";
      stop_session connection session ~key:"session-stop";
      require (exists environment workspace) "session-dir replacement was not created";
      require (not (exists environment sentinel)) "session-dir contents survived stop";
      save environment sentinel "rebuild";
      rebuild_session connection session ~key:"session-stop";
      require (exists environment sentinel) "rebuild removed workspace contents";
      session)
  in
  with_daemon env fixture (fun connection ->
    attach connection session ~key:"session-stop-restart";
    ignore (get_session connection session : Agent_protocol.Session.t);
    require
      (exists environment (session_workspace fixture session))
      "session-dir workspace did not recover")
;;

let test_session_dir_delete env environment =
  let fixture, _ = setup env environment "cleanup-session-delete" Session_dir On_delete in
  with_daemon env fixture (fun connection ->
    let session =
      create_session connection ~key:"session-delete" ~start_immediately:true
    in
    let workspace = session_workspace fixture session in
    save environment (Filename.concat workspace "sentinel") "archive";
    stop_session connection session ~key:"session-delete";
    require
      (exists environment workspace)
      "on-delete session workspace disappeared on stop";
    delete_session connection session ~key:"session-delete" ~policy:Archive;
    require
      (exists environment (session_directory fixture session))
      "archive removed session directory";
    require (not (exists environment workspace)) "archive retained on-delete workspace")
;;

let test_retain env environment =
  let fixture, managed_root = setup env environment "cleanup-retain" System_tmp Retain in
  with_daemon env fixture (fun connection ->
    let session = create_session connection ~key:"retain" ~start_immediately:true in
    let workspace = only_system_workspace environment managed_root in
    let sentinel = Filename.concat workspace "sentinel" in
    save environment sentinel "retain";
    stop_session connection session ~key:"retain";
    require (exists environment sentinel) "retain policy removed data on stop";
    reset_session connection session ~key:"retain" ~keep_workspace:false;
    require (not (exists environment workspace)) "explicit reset retained old workspace";
    let replacement = only_system_workspace environment managed_root in
    let retained = Filename.concat replacement "retained-after-reset" in
    save environment retained "retain-delete";
    delete_session connection session ~key:"retain" ~policy:Archive;
    require (exists environment retained) "retain policy removed data on archive")
;;

let test_physical env environment =
  let fixture, managed_root = setup env environment "cleanup-physical" Physical Retain in
  with_daemon env fixture (fun connection ->
    let session = create_session connection ~key:"physical" ~start_immediately:true in
    let sentinel =
      Filename.concat (Config_fixture.physical_workspace fixture) "sentinel"
    in
    save environment sentinel "physical";
    stop_session connection session ~key:"physical";
    reset_session connection session ~key:"physical" ~keep_workspace:false;
    rebuild_session connection session ~key:"physical";
    delete_session connection session ~key:"physical" ~policy:Remove;
    require (exists environment sentinel) "session lifecycle deleted physical workspace";
    require
      (List.is_empty (child_directories environment managed_root))
      "physical test created a managed workspace")
;;

let test_symlink_refusal env environment =
  let fixture, managed_root =
    setup env environment "cleanup-symlink" System_tmp On_stop
  in
  with_daemon env fixture (fun connection ->
    let session = create_session connection ~key:"symlink" ~start_immediately:true in
    let workspace = only_system_workspace environment managed_root in
    let external_root =
      Filename.concat (Temporary_environment.roots environment).root "external-target"
    in
    Eio.Path.mkdir ~perm:0o700 (path environment external_root);
    let external_sentinel = Filename.concat external_root "sentinel" in
    save environment external_sentinel "external";
    Eio.Path.symlink
      ~link_to:external_root
      (path environment (Filename.concat workspace "escape"));
    let failure = stop_session_result connection session ~key:"symlink" in
    require (Result.is_error failure) "workspace cleanup followed or accepted a symlink";
    require (exists environment workspace) "symlink refusal removed workspace";
    require
      (exists environment external_sentinel)
      "cleanup followed symlink outside workspace")
;;

let test_identity_refusal env environment =
  let fixture, managed_root =
    setup env environment "cleanup-identity" System_tmp On_stop
  in
  with_daemon env fixture (fun connection ->
    let session = create_session connection ~key:"identity" ~start_immediately:true in
    let workspace = only_system_workspace environment managed_root in
    let original = workspace ^ ".original" in
    Eio.Path.rename (path environment workspace) (path environment original);
    Eio.Path.mkdir ~perm:0o700 (path environment workspace);
    save environment (marker workspace) (Filename.basename workspace ^ "\n");
    let replacement_sentinel = Filename.concat workspace "replacement" in
    save environment replacement_sentinel "replacement";
    let failure = stop_session_result connection session ~key:"identity" in
    require (Result.is_error failure) "workspace cleanup accepted a changed inode";
    require
      (exists environment replacement_sentinel)
      "identity refusal deleted replacement path";
    require (exists environment original) "identity refusal deleted original workspace")
;;

let test_reference_protection env environment =
  let fixture, managed_root =
    setup env environment "cleanup-reference" System_tmp On_delete
  in
  let session, workspace =
    with_daemon env fixture (fun connection ->
      let session = create_session connection ~key:"reference" ~start_immediately:false in
      let workspace = only_system_workspace environment managed_root in
      save environment (Filename.concat workspace "sentinel") "referenced";
      session, workspace)
  in
  with_daemon env fixture (fun connection ->
    require_maintenance_running env fixture;
    attach connection session ~key:"reference-restart";
    ignore (get_session connection session : Agent_protocol.Session.t);
    require
      (exists environment (Filename.concat workspace "sentinel"))
      "restart removed referenced workspace";
    delete_session connection session ~key:"reference-restart" ~policy:Archive;
    require
      (not (exists environment workspace))
      "delete did not clean recovered referenced workspace")
;;

let cases =
  [ "temporary.system-stop", test_system_stop
  ; "temporary.system-delete", test_system_delete
  ; "temporary.session-dir-stop", test_session_dir_stop
  ; "temporary.session-dir-delete", test_session_dir_delete
  ; "temporary.retain", test_retain
  ; "physical.never-delete", test_physical
  ; "cleanup.symlink-refusal", test_symlink_refusal
  ; "cleanup.identity-change-refusal", test_identity_refusal
  ; "cleanup.recoverable-reference-protection", test_reference_protection
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown cleanup case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"workspace-cleanup" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (_name, test) -> test env environment);
    print_s
      [%sexp
        { scenario = ("workspace-cleanup" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }])
;;
