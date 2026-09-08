open Core
module Permission = Permission_scenario
module Fixture = Support.Config_fixture
module Environment = Support.Temporary_environment
module Http = Support.Http_driver

let require condition message = if not condition then failwith message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "retention timer protocol failure", (error : Agent_protocol.Error.t)]
;;

let profile = "data-retention-timer"
let retention_seconds = 0.1
let artifact_contents = {|{"fixture":"data-integrity-retention-timer"}|}

let phase env name =
  let sexp =
    [%sexp
      { scenario = ("data-integrity" : string); retention_timer_phase = (name : string) }]
  in
  Eio.Flow.copy_string (Sexp.to_string_hum sexp ^ "\n") (Eio.Stdenv.stdout env)
;;

let configure env environment =
  let fixture =
    Permission.configure_fixture
      env
      environment
      "data-retention-timer"
      ~profile
      ~tool_default:"ask"
      ()
  in
  let path = Environment.path environment (Fixture.config_path fixture) in
  let contents =
    Eio.Path.load path
    |> String.substr_replace_first
         ~pattern:"(response_artifact_ms 60000)"
         ~with_:"(response_artifact_ms 100)"
    |> String.substr_replace_first
         ~pattern:"(idle_connection_timeout_ms 5000)"
         ~with_:"(idle_connection_timeout_ms 180000)"
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) path contents;
  fixture
;;

let entry daemon (session : Permission.session) =
  Agent_server.Session_registry.find
    (Agent_server.Daemon.registry daemon)
    session.summary.id
  |> Option.value_exn
;;

let state daemon session =
  Agent_session.Session_actor.state (entry daemon session).actor |> protocol_ok
;;

let create_running client name =
  let session = Permission.create_session client profile (name ^ ":create") in
  Permission.start_session client session (name ^ ":start")
;;

let assert_idle daemon session =
  let state = state daemon session in
  require
    (Option.is_none state.active_operation)
    "idle retention session has an active operation";
  match state.lifecycle.observed with
  | Idle -> ()
  | _ -> failwith "retention session is not idle"
;;

let assert_suspended daemon session (permission : Agent_protocol.Permission.t) =
  let state = state daemon session in
  let operation = Option.value_exn state.active_operation in
  require
    (Agent_protocol.Permission.equal_owner permission.owner (Operation operation.id))
    "suspended foreground operation changed before maintenance";
  require
    (List.exists state.permissions ~f:(fun candidate ->
       Agent_protocol.Id.Permission.compare candidate.id permission.id = 0
       && Agent_protocol.Permission.equal_state candidate.state Pending))
    "tool permission did not remain suspended during maintenance"
;;

let suspend env daemon client session =
  let sent = Permission.send_message client session "retention:send" in
  let permission =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      Permission.await_pending_permission env client session.Permission.summary.id 500)
  in
  require
    (Option.exists sent.operation_id ~f:(fun id ->
       Agent_protocol.Permission.equal_owner permission.owner (Operation id)))
    "permission belongs to another operation";
  assert_suspended daemon session permission;
  permission
;;

let write_artifact environment daemon session =
  let handle = (entry daemon session).store_handle |> Option.value_exn in
  let responses = Agent_store.Session_store.Handle.responses_directory handle in
  let directory = Environment.path environment responses in
  let path = Eio.Path.(directory / "data-integrity-timer.json") in
  Eio.Path.save ~create:(`Exclusive 0o600) path artifact_contents;
  path
;;

let rec poll env ~guard ~is_done =
  guard ();
  if not (is_done ())
  then (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
    poll env ~guard ~is_done)
;;

let await env ~name ~guard ~is_done =
  match
    Eio.Time.with_timeout (Eio.Stdenv.clock env) 75. (fun () ->
      poll env ~guard ~is_done;
      Ok ())
  with
  | Ok () -> ()
  | Error `Timeout -> failwith ("timed out waiting for daemon maintenance: " ^ name)
;;

let assert_maintenance client =
  match
    (Http.request client (Server_health { include_details = true }) |> protocol_ok).result
  with
  | Server_health health ->
    let maintenance =
      List.find_exn health.components ~f:(fun component ->
        String.equal component.name "maintenance")
    in
    require
      (Agent_protocol.Health.equal_status maintenance.status Healthy)
      "daemon maintenance health reports failure";
    require
      (Option.value_map
         maintenance.message
         ~default:false
         ~f:(String.is_prefix ~prefix:"last cycle pruned "))
      "daemon has not completed a real maintenance cycle"
  | _ -> failwith "expected daemon health response"
;;

let await_protected env daemon client active permission active_file idle_file =
  let active_mtime = (Eio.Path.stat ~follow:false active_file).mtime in
  let idle_mtime = (Eio.Path.stat ~follow:false idle_file).mtime in
  require
    Float.(active_mtime <= idle_mtime)
    "protected artifact is newer than idle control";
  phase env "waiting-for-first-real-cycle";
  await
    env
    ~name:"idle response deletion"
    ~guard:(fun () ->
      assert_suspended daemon active permission;
      require (Eio.Path.is_file active_file) "timer removed an active response artifact")
    ~is_done:(fun () -> not (Eio.Path.is_file idle_file));
  require
    Float.(Eio.Time.now (Eio.Stdenv.clock env) > idle_mtime +. retention_seconds)
    "idle artifact was removed before its retention cutoff";
  require
    (String.equal (Eio.Path.load active_file) artifact_contents)
    "timer changed active response contents";
  assert_maintenance client
;;

let complete env daemon client active permission marker =
  Permission.respond_approve client active permission "retention:approve";
  ignore
    (Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
       Permission.await_operation_end env client active.Permission.summary.id 500)
     : Agent_protocol.Session.t);
  require (Eio.Path.is_file marker) "approved suspended tool did not complete";
  assert_idle daemon active
;;

let await_released env daemon client active active_file idle_file =
  require
    (Eio.Path.is_file active_file)
    "operation completion removed the timer control artifact";
  phase env "waiting-for-post-completion-cycle";
  await
    env
    ~name:"completed response deletion"
    ~guard:(fun () -> assert_idle daemon active)
    ~is_done:(fun () -> not (Eio.Path.is_file active_file));
  require (not (Eio.Path.is_file idle_file)) "idle response artifact reappeared";
  assert_maintenance client
;;

let exercise env environment daemon client marker =
  let idle = create_running client "retention:idle" in
  let active = create_running client "retention:active" in
  assert_idle daemon idle;
  let permission = suspend env daemon client active in
  require (not (Eio.Path.is_file marker)) "tool executed without approval";
  let active_file = write_artifact environment daemon active in
  let idle_file = write_artifact environment daemon idle in
  await_protected env daemon client active permission active_file idle_file;
  complete env daemon client active permission marker;
  await_released env daemon client active active_file idle_file
;;

let run env environment =
  let fixture = configure env environment in
  let marker =
    Filename.concat (Fixture.physical_workspace fixture) "retention-tool-marker.txt"
    |> Environment.path environment
  in
  let options =
    { Agent_server.Daemon.default_options with
      model_post_stream = Some (Permission.model_post_stream (Eio.Path.native_exn marker))
    }
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 170. (fun () ->
    Support.Daemon_host.with_ env fixture ~options (fun sw daemon ->
      Permission.with_client ~sw env fixture (fun client ->
        exercise env environment daemon client marker)))
;;
