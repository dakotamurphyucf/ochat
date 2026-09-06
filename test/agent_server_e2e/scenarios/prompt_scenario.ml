open Core
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Http_probe = Support.Http_probe
module Port_reservation = Support.Port_reservation
module Temporary_environment = Support.Temporary_environment
module Unix_driver = Support.Unix_driver

type version =
  | A
  | B

type session =
  { id : Agent_protocol.Id.Session.t
  ; mutable attachment_id : Agent_protocol.Id.Attachment.t
  ; mutable revision : int64
  ; prompt_revision : Agent_protocol.Id.Prompt_revision.t
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

let path environment native_path = Temporary_environment.path environment native_path

let save environment native_path contents =
  Eio.Path.save ~create:(`Or_truncate 0o600) (path environment native_path) contents
;;

let version_name = function
  | A -> "A"
  | B -> "B"
;;

let source_name version = String.lowercase (version_name version) ^ ".chatmd"

let imported_source version =
  let marker = version_name version in
  sprintf
    {|
<developer>You are deterministic prompt revision %s.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Session_resume | `Tick(string) ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx state event ->
      match event with
      | `Session_start ->
        Task.bind(Schedule.after_ms(60000, `Tick("%s")), fun schedule_id ->
        Task.pure(state + 1))
      | `Session_resume -> Task.pure(state + 1)
      | `Tick(value) -> Task.pure(state + 1)
</script>
|}
    marker
    marker
;;

let prompt_directory fixture = Filename.dirname (Config_fixture.prompt_path fixture)
let parts_directory fixture = Filename.concat (prompt_directory fixture) "parts"

let source_path fixture version =
  Filename.concat (parts_directory fixture) (source_name version)
;;

let write_version environment fixture version =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (path environment (parts_directory fixture));
  save environment (source_path fixture version) (imported_source version);
  save
    environment
    (Config_fixture.prompt_path fixture)
    (sprintf "<import src=\"parts/%s\"/>" (source_name version))
;;

let delete_source environment fixture version =
  Eio.Path.unlink (path environment (source_path fixture version))
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

let connect ~sw env fixture =
  let connection =
    Unix_driver.connect ~sw ~env ~socket_path:(Config_fixture.unix_socket fixture)
  in
  ignore
    (Unix_driver.initialize connection |> protocol_ok
     : Agent_protocol.Initialize.Response.t);
  connection
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
        let connection = connect ~sw env fixture in
        Exn.protect
          ~f:(fun () ->
            try f daemon connection with
            | exn ->
              raise_s
                [%sexp
                  "prompt daemon scenario failed"
                , (exn : Exn.t)
                , ((Daemon_process.stderr daemon).contents : string)])
          ~finally:(fun () -> Agent_client.Connection.close connection))
      ~finally:(fun () -> stop_daemon env daemon))
;;

let prompt connection : Agent_protocol.Prompt.t =
  Agent_client.Catalog.prompts connection
  |> protocol_ok
  |> List.find_exn ~f:(fun value -> String.equal value.name "smoke")
;;

let workspace connection : Agent_protocol.Workspace.t =
  Agent_client.Catalog.workspaces connection
  |> protocol_ok
  |> List.find_exn ~f:(fun value -> String.equal value.name "physical")
;;

let current_revision connection =
  prompt connection
  |> fun value -> value.Agent_protocol.Prompt.current_revision |> Option.value_exn
;;

let create_session connection ~key ~start_immediately =
  let prompt = prompt connection in
  let workspace = workspace connection in
  let spec =
    Agent_protocol.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog prompt.id)
      ~workspace:(Configured workspace.id)
      ~liveness:Detached
      ~persistence:Durable
      ~permission_profile:"unattended"
      ~start_immediately
      ~labels:[ "suite", "prompt-lifecycle" ]
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
    ; prompt_revision = Option.value_exn created.session.prompt_revision
    }
  | _ -> fail "session.create returned the wrong result variant"
;;

let attach_result connection session ~key =
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
  Agent_client.Connection.request connection (Session_attach command_request)
;;

let attach connection session ~key =
  match attach_result connection session ~key |> protocol_ok with
  | Session_attach attached -> session.attachment_id <- attached.attachment.id
  | _ -> fail "session.attach returned the wrong result variant"
;;

let snapshot connection session =
  match request connection (Session_get { session_id = session.id; history = None }) with
  | Session_get snapshot ->
    session.revision <- snapshot.session.revision;
    snapshot
  | _ -> fail "session.get returned the wrong result variant"
;;

let session_summary connection session = (snapshot connection session).session

let start_session connection session ~key =
  let command_request =
    Agent_protocol.Session.Start_request.
      { session_id = session.id
      ; attachment_id = session.attachment_id
      ; queue_if_limited = true
      ; idempotency_key = idempotency_key (key ^ ":start")
      }
  in
  match request connection (Session_start command_request) with
  | Session_start started ->
    session.revision <- started.session.revision;
    started.session
  | _ -> fail "session.start returned the wrong result variant"
;;

let rebuild_result connection session ~key ~expected_revision ~prompt_choice =
  let command_request =
    Agent_protocol.Session.Rebuild_request.
      { session_id = session.id
      ; attachment_id = session.attachment_id
      ; expected_revision
      ; prompt_choice
      ; idempotency_key = idempotency_key (key ^ ":rebuild")
      }
  in
  Agent_client.Connection.request connection (Session_rebuild command_request)
;;

let rebuild connection session ~key ~prompt_choice =
  match
    rebuild_result
      connection
      session
      ~key
      ~expected_revision:session.revision
      ~prompt_choice
    |> protocol_ok
  with
  | Session_rebuild rebuilt ->
    session.revision <- rebuilt.session.revision;
    rebuilt.session
  | _ -> fail "session.rebuild returned the wrong result variant"
;;

let upgrade_result connection session ~key ~expected_revision ~target_revision =
  let command_request =
    Agent_protocol.Session.Upgrade_prompt_request.
      { session_id = session.id
      ; attachment_id = session.attachment_id
      ; expected_revision
      ; target_revision
      ; allow_migration = true
      ; idempotency_key = idempotency_key (key ^ ":upgrade")
      }
  in
  Agent_client.Connection.request connection (Session_upgrade_prompt command_request)
;;

let upgrade connection session ~key ~target_revision =
  match
    upgrade_result
      connection
      session
      ~key
      ~expected_revision:session.revision
      ~target_revision
    |> protocol_ok
  with
  | Session_upgrade_prompt upgraded ->
    session.revision <- upgraded.session.revision;
    upgraded.session
  | _ -> fail "session.upgrade_prompt returned the wrong result variant"
;;

let same_revision left right = Agent_protocol.Id.Prompt_revision.compare left right = 0

let session_revision summary =
  summary.Agent_protocol.Session.prompt_revision |> Option.value_exn
;;

let json_contains marker json =
  let rec loop = function
    | `String value -> String.equal value marker
    | `Array values -> List.exists values ~f:loop
    | `Object fields -> List.exists fields ~f:(fun (_name, value) -> loop value)
    | `Null | `True | `False | `Number _ -> false
  in
  loop json
;;

let snapshot_has_marker (snapshot : Agent_protocol.Snapshot.t) version =
  let marker = version_name version in
  List.exists snapshot.schedules ~f:(fun schedule ->
    json_contains marker schedule.Agent_protocol.Schedule.payload)
;;

let require_marker connection session version =
  require
    (snapshot_has_marker (snapshot connection session) version)
    (sprintf "session does not expose revision %s behavior" (version_name version))
;;

let cancel_schedule connection session index schedule =
  let command_request =
    Agent_protocol.Schedule.Cancel_request.
      { session_id = session.id
      ; attachment_id = session.attachment_id
      ; schedule_id = schedule.Agent_protocol.Schedule.id
      ; idempotency_key = idempotency_key (sprintf "cancel-schedule:%d" index)
      }
  in
  match request connection (Schedule_cancel command_request) with
  | Schedule_cancel result -> session.revision <- result.mutation.revision
  | _ -> fail "schedule.cancel returned the wrong result variant"
;;

let cancel_pending_schedules connection session =
  snapshot connection session
  |> fun snapshot ->
  snapshot.Agent_protocol.Snapshot.schedules
  |> List.filter ~f:(fun schedule ->
    match schedule.status with
    | Scheduled -> true
    | Delivering | Delivered | Cancelled | Failed _ -> false)
  |> List.iteri ~f:(cancel_schedule connection session)
;;

let artifact_source fixture revision version =
  Filename.concat
    (Filename.concat
       (Filename.concat
          (Filename.concat (Config_fixture.data_dir fixture) "prompt-artifacts")
          (Agent_protocol.Id.Prompt_revision.to_string revision))
       "tree")
    (Filename.concat "parts" (source_name version))
;;

let require_artifact_source environment fixture revision version =
  let contents =
    Eio.Path.load (path environment (artifact_source fixture revision version))
  in
  require
    (String.is_substring contents ~substring:("revision " ^ version_name version))
    "materialized prompt source has the wrong revision marker"
;;

let signal_reload daemon = Daemon_process.signal daemon Stdlib.Sys.sighup

let rec await_revision env connection previous attempts =
  let revision = current_revision connection in
  if not (same_revision revision previous)
  then revision
  else if attempts = 0
  then fail "catalog revision did not change after reload"
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_revision env connection previous (attempts - 1))
;;

let configuration_component env fixture =
  let health =
    Http_probe.get_health
      env
      ~port:(Config_fixture.http_port fixture)
      ~token:(Config_fixture.admin_token fixture)
    |> Result.ok_or_failwith
  in
  List.find_exn health.components ~f:(fun component ->
    String.equal component.Agent_protocol.Health.Component.name "configuration")
;;

let rec await_configuration_status env fixture expected attempts =
  let component = configuration_component env fixture in
  if Agent_protocol.Health.equal_status component.status expected
  then component
  else if attempts = 0
  then
    raise_s
      [%sexp
        "configuration status did not converge"
      , (expected : Agent_protocol.Health.status)
      , (component : Agent_protocol.Health.Component.t)]
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_configuration_status env fixture expected (attempts - 1))
;;

let setup env environment name =
  let fixture = fixture env environment name in
  write_version environment fixture A;
  fixture
;;

let reload_to_b env environment fixture daemon connection revision_a =
  write_version environment fixture B;
  signal_reload daemon;
  let revision_b = await_revision env connection revision_a 250 in
  require_artifact_source environment fixture revision_b B;
  revision_b
;;

let test_pin_revision_a env environment =
  let fixture = setup env environment "prompt-pin-a" in
  with_daemon env fixture (fun _daemon connection ->
    let revision_a = current_revision connection in
    let session = create_session connection ~key:"pin-a" ~start_immediately:false in
    require
      (same_revision session.prompt_revision revision_a)
      "session did not pin catalog revision A";
    require_artifact_source environment fixture revision_a A;
    require_marker connection session A)
;;

let test_reload_revision_b env environment =
  let fixture = setup env environment "prompt-reload-b" in
  with_daemon env fixture (fun daemon connection ->
    let revision_a = current_revision connection in
    let revision_b = reload_to_b env environment fixture daemon connection revision_a in
    let session = create_session connection ~key:"reload-b" ~start_immediately:false in
    require
      (same_revision session.prompt_revision revision_b)
      "new session did not adopt revision B";
    require_marker connection session B)
;;

let test_active_remains_a env environment =
  let fixture = setup env environment "prompt-active-a" in
  with_daemon env fixture (fun daemon connection ->
    let revision_a = current_revision connection in
    let first = create_session connection ~key:"active-a" ~start_immediately:true in
    require_marker connection first A;
    let revision_b = reload_to_b env environment fixture daemon connection revision_a in
    let first_state = session_summary connection first in
    require
      (same_revision (session_revision first_state) revision_a)
      "active session changed revision during catalog reload";
    require_marker connection first A;
    let second = create_session connection ~key:"active-b" ~start_immediately:true in
    require
      (same_revision second.prompt_revision revision_b)
      "post-reload session did not adopt revision B";
    require_marker connection second B)
;;

let test_live_source_deleted env environment =
  let fixture = setup env environment "prompt-live-deleted" in
  let session_a, revision_a =
    with_daemon env fixture (fun daemon connection ->
      let revision_a = current_revision connection in
      let session_a =
        create_session connection ~key:"live-deleted-a" ~start_immediately:false
      in
      ignore (reload_to_b env environment fixture daemon connection revision_a);
      session_a, revision_a)
  in
  delete_source environment fixture A;
  with_daemon env fixture (fun _daemon connection ->
    attach connection session_a ~key:"live-deleted-a:restart";
    let state = start_session connection session_a ~key:"live-deleted-a:restart" in
    require
      (same_revision (session_revision state) revision_a)
      "restart did not restore pinned revision A";
    require_marker connection session_a A;
    let session_b =
      create_session connection ~key:"live-deleted-b" ~start_immediately:false
    in
    require_marker connection session_b B)
;;

let test_explicit_upgrade env environment =
  let fixture = setup env environment "prompt-explicit-upgrade" in
  with_daemon env fixture (fun daemon connection ->
    let revision_a = current_revision connection in
    let session =
      create_session connection ~key:"explicit-upgrade" ~start_immediately:false
    in
    let revision_b = reload_to_b env environment fixture daemon connection revision_a in
    let upgraded =
      upgrade connection session ~key:"explicit-upgrade" ~target_revision:revision_b
    in
    require
      (same_revision (session_revision upgraded) revision_b)
      "explicit upgrade did not install revision B";
    require_marker connection session B)
;;

let test_rebuild env environment =
  let fixture = setup env environment "prompt-rebuild" in
  with_daemon env fixture (fun daemon connection ->
    let revision_a = current_revision connection in
    let session = create_session connection ~key:"rebuild" ~start_immediately:false in
    let revision_b = reload_to_b env environment fixture daemon connection revision_a in
    let rebuilt =
      rebuild connection session ~key:"rebuild" ~prompt_choice:Current_catalog
    in
    require
      (same_revision (session_revision rebuilt) revision_b)
      "current-catalog rebuild did not install revision B";
    require_marker connection session B)
;;

let test_stale_revision env environment =
  let fixture = setup env environment "prompt-stale-revision" in
  with_daemon env fixture (fun daemon connection ->
    let revision_a = current_revision connection in
    let session = create_session connection ~key:"stale" ~start_immediately:false in
    let revision_b = reload_to_b env environment fixture daemon connection revision_a in
    let result =
      upgrade_result
        connection
        session
        ~key:"stale"
        ~expected_revision:Int64.(session.revision - 1L)
        ~target_revision:revision_b
    in
    (match result with
     | Error error ->
       require
         (Agent_protocol.Error.equal_code error.code Conflict)
         "stale upgrade returned the wrong error"
     | Ok _ -> fail "stale administrative revision was accepted");
    require
      (same_revision (session_revision (session_summary connection session)) revision_a)
      "stale upgrade changed the pinned revision")
;;

let test_invalid_reload env environment =
  let fixture = setup env environment "prompt-invalid-reload" in
  with_daemon env fixture (fun daemon connection ->
    let revision_a = current_revision connection in
    save
      environment
      (Config_fixture.prompt_path fixture)
      "<import src=\"parts/missing.chatmd\"/>";
    signal_reload daemon;
    ignore
      (await_configuration_status env fixture Agent_protocol.Health.Degraded 250
       : Agent_protocol.Health.Component.t);
    require
      (same_revision (current_revision connection) revision_a)
      "invalid reload replaced the published catalog";
    let session =
      create_session connection ~key:"invalid-reload" ~start_immediately:false
    in
    require
      (same_revision session.prompt_revision revision_a)
      "new session did not retain revision A after invalid reload";
    require_marker connection session A)
;;

let corrupt_artifact environment fixture revision =
  let root =
    Filename.concat
      (Filename.concat
         (Filename.concat (Config_fixture.data_dir fixture) "prompt-artifacts")
         (Agent_protocol.Id.Prompt_revision.to_string revision))
      "root.chatmd"
  in
  (* Installed files are read-only; model an owner replacing the fixture. *)
  Eio.Path.unlink (path environment root);
  save environment root "corrupt"
;;

let test_artifact_corruption env environment =
  let fixture = setup env environment "prompt-artifact-corruption" in
  let session_a, session_b, revision_a =
    with_daemon env fixture (fun daemon connection ->
      let revision_a = current_revision connection in
      let session_a =
        create_session connection ~key:"corrupt-a" ~start_immediately:false
      in
      ignore (reload_to_b env environment fixture daemon connection revision_a);
      let session_b =
        create_session connection ~key:"corrupt-b" ~start_immediately:false
      in
      cancel_pending_schedules connection session_a;
      session_a, session_b, revision_a)
  in
  corrupt_artifact environment fixture revision_a;
  with_daemon env fixture (fun _daemon connection ->
    attach connection session_b ~key:"corrupt-b:restart";
    require_marker connection session_b B;
    match attach_result connection session_a ~key:"corrupt-a:restart" with
    | Error error ->
      require
        (Agent_protocol.Error.equal_code error.code Prompt_unavailable)
        "corrupt pinned artifact returned the wrong error"
    | Ok _ -> fail "corrupt pinned artifact was accepted")
;;

let cases =
  [ "prompt.pin-revision-a", test_pin_revision_a
  ; "prompt.reload-revision-b", test_reload_revision_b
  ; "prompt.active-session-remains-a", test_active_remains_a
  ; "prompt.live-source-deleted", test_live_source_deleted
  ; "prompt.explicit-upgrade", test_explicit_upgrade
  ; "prompt.rebuild", test_rebuild
  ; "prompt.stale-revision-rejection", test_stale_revision
  ; "prompt.invalid-reload-rollback", test_invalid_reload
  ; "prompt.referenced-artifact-corruption", test_artifact_corruption
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown prompt case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_ ~scenario:"prompt-lifecycle" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (name, test) ->
      try test env environment with
      | exn -> raise_s [%sexp "prompt E2E case failed", (name : string), (exn : Exn.t)]);
    print_s
      [%sexp
        { scenario = ("prompt-lifecycle" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }])
;;
