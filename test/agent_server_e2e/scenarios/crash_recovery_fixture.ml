open Core
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Http_driver = Support.Http_driver
module Process_manager = Support.Process_manager
module Temporary_environment = Support.Temporary_environment

let fail message = raise_s [%sexp "crash/recovery assertion failed", (message : string)]
let require condition message = if not condition then fail message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
;;

let store_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "store operation failed", (error : Agent_store.Store_error.t)]
;;

let key value = Agent_protocol.Idempotency_key.of_string value |> protocol_ok
let path env filename = Eio.Path.(Eio.Stdenv.fs env / filename)
let read env native = Eio.Path.load (path env native)

let write env native contents =
  Eio.Path.save ~create:(`Or_truncate 0o600) (path env native) contents
;;

let fixture env environment name =
  let port =
    Eio.Switch.run (fun sw ->
      let reservation = Support.Port_reservation.create ~sw ~env in
      let port = Support.Port_reservation.port reservation in
      Support.Port_reservation.release reservation;
      port)
  in
  Config_fixture.create environment ~name ~http_port:port
;;

let with_client ~sw env fixture f =
  let client =
    Http_driver.create
      ~sw
      ~env
      ~port:(Config_fixture.http_port fixture)
      ~token:(Some (Config_fixture.admin_token fixture))
    |> Result.ok_or_failwith
  in
  Exn.protect
    ~f:(fun () ->
      ignore (Http_driver.initialize client |> protocol_ok : _);
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () -> f client))
    ~finally:(fun () -> Http_driver.shutdown client)
;;

let request client command = (Http_driver.request client command |> protocol_ok).result

let session_spec () =
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog (Agent_server.Catalog_identity.prompt_definition "smoke"))
    ~workspace:
      (Configured (Agent_server.Catalog_identity.workspace_definition "physical"))
    ~liveness:Detached
    ~persistence:Durable
    ~permission_profile:"unattended"
    ~start_immediately:false
    ~display_name:"crash recovery exact state"
    ~labels:[ "suite", "crash-recovery"; "sentinel", "acknowledged" ]
    ()
  |> protocol_ok
;;

let create_session client =
  match
    request
      client
      (Session_create
         { spec = session_spec ()
         ; requested_mode = Some Read_write
         ; subscribe = false
         ; idempotency_key = key "crash-recovery:create"
         })
  with
  | Session_create created -> created
  | _ -> fail "session.create returned wrong result"
;;

let get client session_id =
  match request client (Session_get { session_id; history = None }) with
  | Session_get snapshot -> snapshot
  | _ -> fail "session.get returned wrong result"
;;

let await_notifications env child client session ~provider_prefix ~calls ~count =
  let observed_calls () =
    String.split_lines (Process_manager.stdout child).contents
    |> List.count ~f:(String.is_prefix ~prefix:provider_prefix)
  in
  let settled (snapshot : Agent_protocol.Snapshot.t) =
    let deliveries =
      List.filter snapshot.extension_status ~f:(fun status ->
        Agent_protocol.Extension_status.equal_kind status.kind Delivery)
    in
    Option.is_none snapshot.session.active_operation
    && List.length deliveries = count
    && List.for_all deliveries ~f:(fun status -> String.equal status.state "committed")
  in
  ignore
    (Support.Background_fixture.await_snapshot
       env
       client
       session
       "notification recovery settlement"
       (fun snapshot -> settled snapshot && observed_calls () >= calls)
     : Agent_protocol.Snapshot.t);
  for _ = 1 to 10 do
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.03;
    require (observed_calls () = calls) "saved wake was lost or repeated after restart"
  done;
  let snapshot = get client session.summary.id in
  require
    (settled snapshot && Option.is_none snapshot.failure)
    "notification recovery did not remain settled";
  snapshot
;;

let require_equal label sexp_of expected actual =
  if not (Sexp.equal (sexp_of expected) (sexp_of actual))
  then
    raise_s
      [%sexp
        "recovered value differs"
      , (label : string)
      , { expected = (sexp_of expected : Sexp.t); actual = (sexp_of actual : Sexp.t) }]
;;

let assert_summary
      (expected : Agent_protocol.Session.t)
      (actual : Agent_protocol.Session.t)
  =
  let normalized_session =
    { actual with
      updated_at = expected.updated_at
    ; revision = expected.revision
    ; latest_event_sequence = expected.latest_event_sequence
    }
  in
  require_equal
    "session identity/spec/lifecycle"
    [%sexp_of: Agent_protocol.Session.t]
    expected
    normalized_session
;;

let normalized_snapshot
      (expected : Agent_protocol.Snapshot.t)
      (actual : Agent_protocol.Snapshot.t)
  =
  let normalized =
    { actual with
      session = expected.session
    ; revision = expected.revision
    ; latest_event_sequence = expected.latest_event_sequence
    }
  in
  normalized
;;

let assert_snapshot
      (expected : Agent_protocol.Snapshot.t)
      (actual : Agent_protocol.Snapshot.t)
  =
  require expected.canonical_history.reached_start "expected history is truncated";
  require
    (not (List.is_empty expected.canonical_history.entries))
    "history oracle is empty";
  assert_summary expected.session actual.session;
  require_equal
    "complete projected history/state"
    [%sexp_of: Agent_protocol.Snapshot.t]
    expected
    (normalized_snapshot expected actual);
  require Int64.(actual.revision > expected.revision) "recovery revision did not advance";
  require
    Int64.(actual.latest_event_sequence > expected.latest_event_sequence)
    "recovery event sequence did not advance"
;;

let wait_ready env daemon =
  match Daemon_process.wait_ready daemon ~env ~timeout_seconds:10. with
  | Ok _ -> ()
  | Error error ->
    raise_s
      [%sexp
        "daemon not ready"
      , (error : Daemon_process.readiness_error)
      , ((Daemon_process.stderr daemon).contents : string)]
;;

let stop env daemon =
  match Daemon_process.result daemon with
  | Some _ -> ()
  | None ->
    ignore
      (Daemon_process.stop daemon ~env ~grace_seconds:2. : Process_manager.termination)
;;

let start ~sw env fixture =
  Daemon_process.start ~sw ~env ~fixture ~config_path:(Config_fixture.config_path fixture)
;;

let with_daemon env fixture f =
  Eio.Switch.run (fun sw ->
    let daemon = start ~sw env fixture in
    Exn.protect
      ~f:(fun () ->
        wait_ready env daemon;
        with_client ~sw env fixture f)
      ~finally:(fun () -> stop env daemon))
;;

let seed env fixture =
  with_daemon env fixture (fun client ->
    let created = create_session client in
    get client created.session.id)
;;

let session_directory fixture session_id =
  Filename.concat
    (Filename.concat (Config_fixture.data_dir fixture) "sessions")
    (Agent_protocol.Id.Session.to_string session_id)
;;

let current_journal env fixture session_id =
  let directory = Filename.concat (session_directory fixture session_id) "journal" in
  let names =
    Eio.Path.read_dir (path env directory)
    |> List.filter ~f:(String.is_suffix ~suffix:".log")
    |> List.sort ~compare:String.compare
  in
  Filename.concat directory (List.last_exn names)
;;

let snapshot_directory fixture session_id =
  Filename.concat (session_directory fixture session_id) "snapshot"
;;

let self_executable env =
  let executable = Stdlib.Sys.executable_name in
  if Filename.is_absolute executable
  then executable
  else Filename.concat (Eio.Path.native_exn (Eio.Stdenv.cwd env)) executable
;;

let child ~sw env environment ~case ~arguments =
  let overrides =
    "OCHAT_E2E_CRASH_ARGUMENTS=" ^ Sexp.to_string_mach ([%sexp_of: string list] arguments)
  in
  let inherited =
    Temporary_environment.child_environment environment ~base:(Core_unix.environment ())
    |> Array.filter ~f:(Fn.non (String.is_prefix ~prefix:"OCHAT_E2E_CRASH_ARGUMENTS="))
  in
  Process_manager.spawn
    ~sw
    ~env
    ~environment:(Array.append inherited [| overrides |])
    ~max_output_bytes:(1024 * 1024)
    [ self_executable env; "--scenario"; "crash-matrix"; "--case"; "child." ^ case ]
;;

let await_marker env child marker =
  match
    Process_manager.wait_for_stdout
      child
      ~clock:(Eio.Stdenv.clock env)
      ~timeout_seconds:10.
      ~ready:(String.is_substring ~substring:marker)
  with
  | Ok () -> ()
  | Error error ->
    raise_s
      [%sexp
        "child boundary not reached"
      , (error : Process_manager.readiness_error)
      , (Process_manager.stderr child : Process_manager.output)]
;;

let kill env child =
  Process_manager.signal child Stdlib.Sys.sigkill;
  let result =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      Process_manager.await child)
  in
  require
    (Process_manager.equal_exit result.exit (Signaled Stdlib.Sys.sigkill))
    "child did not die by SIGKILL"
;;

let terminate env child =
  ignore
    (Process_manager.terminate child ~clock:(Eio.Stdenv.clock env) ~grace_seconds:0.2
     : Process_manager.termination)
;;
