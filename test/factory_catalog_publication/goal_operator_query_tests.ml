open! Core
open Agent_server_test_support
open Retry_support
module C = Agent_client
module Q = P.Goal_query
module W = P.Goal_control

let available = function
  | Q.Result.Available view -> view
  | Unavailable _ -> failwith "authorized Goal session was unavailable"
;;

let assert_code code outcome =
  let (failure : P.Error.t) = rejected outcome in
  [%test_eq: P.Error.code] code failure.code
;;

let initialize client =
  C.Session_handle.initialize_with_features
    client
    ~implementation_name:"goal-operator-query-fixture"
    ~implementation_version:"1"
    ~features:[ Q.Features.inspect; P.Goal_control.Features.control ]
  |> protocol_ok
;;

let expect_features initialized ~controls =
  let features = initialized.P.Initialize.Response.enabled_features in
  assert (List.mem features Q.Features.inspect ~equal:String.equal);
  [%test_eq: bool]
    controls
    (List.mem features P.Goal_control.Features.control ~equal:String.equal)
;;

let attach sw env client session mode =
  C.Session_handle.attach
    ~sw
    ~clock:(Eio.Stdenv.clock env)
    ~connection:client
    ~session_id:(P.Session_ref.session_id session)
    ~mode
    ~subscribe:false
    ()
  |> protocol_ok
;;

let goal_row view =
  let row = List.hd_exn view.Q.View.goals in
  assert (P.Id.Goal.equal row.goal.id Goal_service_tests.goal_id);
  row
;;

let with_client connector f =
  let client = connector () in
  Exn.protect ~finally:(fun () -> C.Connection.close client) ~f:(fun () -> f client)
;;

let assert_idle (current : A.Session_state.t) =
  match current.lifecycle.observed with
  | Idle -> ()
  | observed ->
    raise_s
      [%sexp "expected retained Idle lifecycle", (observed : P.Session.observed_state)]
;;

let retain_idle_goal daemon =
  Deferred_source_support.with_loaded_entry daemon (fun ~entry ~runtime:_ ~revision:_ ->
    let session, current = Goal_service_tests.create_goal daemon entry in
    assert_idle current;
    session)
;;

let%expect_test
    "shared Goal operator distinguishes an observed Stop from an unavailable owner"
  =
  with_recoverable_fixture
    ~max_root_agents:2
    (fun env sw root daemon policy entry _runtime _revision counters ->
       let session, before = Goal_service_tests.create_goal daemon entry in
       let connect =
         Agent_server_wire_fixture.http_connector ~sw ~env ~daemon ~root ~principal
       in
       let owner = connect () in
       let other = connect () in
       let accepted_command =
         Exn.protect
           ~finally:(fun () ->
             C.Connection.close other;
             C.Connection.close owner)
           ~f:(fun () ->
             expect_features (initialize owner) ~controls:true;
             expect_features (initialize other) ~controls:true;
             let writer = attach sw env owner session Read_write in
             let reader = attach sw env other session Read_only in
             let writer_goals = C.Session_handle.goals writer |> protocol_ok in
             let reader_goals = C.Session_handle.goals reader |> protocol_ok in
             let fresh = C.Goal_controls.list writer_goals |> protocol_ok |> available in
             assert (Q.Availability.equal Active fresh.availability);
             let first = goal_row fresh in
             assert (List.mem first.actions Record_progress ~equal:Q.Action.equal);
             assert (List.mem first.actions Hold ~equal:Q.Action.equal);
             let read = C.Goal_controls.list reader_goals |> protocol_ok |> available in
             assert (not read.can_create);
             assert (List.is_empty (goal_row read).actions);
             let foreign_writer =
               C.Goal_controls.create
                 other
                 ~session
                 ~attachment:(fun () -> (C.Session_handle.attachment writer).id)
                 ()
             in
             let read = C.Goal_controls.list foreign_writer |> protocol_ok |> available in
             assert (not read.can_create);
             assert (List.is_empty (goal_row read).actions);
             let progress =
               P.Goal.Progress.of_string "Observed operator progress" |> protocol_ok
             in
             let command =
               P.Goal_command.create
                 ~session_id:(P.Session_ref.session_id session)
                 ~generation:fresh.generation
                 ~source:fresh.source
                 ~expected_catalog_revision:fresh.catalog_revision
                 ~expected_goal_revision:(Some first.goal.revision)
                 ~key:
                   (P.Idempotency_key.of_string "public-read-focused-progress"
                    |> protocol_ok)
                 ~mutation:(Record_progress (first.goal.id, progress))
               |> protocol_ok
             in
             C.Goal_controls.command reader_goals command |> assert_code Permission_denied;
             C.Goal_controls.command foreign_writer command
             |> assert_code Permission_denied;
             let receipt = C.Goal_controls.command writer_goals command |> protocol_ok in
             [%test_eq: int64] (Int64.succ first.goal.revision) receipt.goal_revision;
             let lookup =
               C.Goal_controls.get reader_goals first.goal.id |> protocol_ok |> available
             in
             assert (P.Goal.Progress.equal (goal_row lookup).goal.progress progress);
             [%test_eq: int64] receipt.catalog_revision lookup.catalog_revision;
             let foreign_host =
               P.Session_ref.create
                 ~server_id:(P.Id.Server.create ())
                 ~session_id:(P.Session_ref.session_id session)
             in
             C.Connection.request_without_history
               owner
               (Goal_query
                  Q.Request.{ session = foreign_host; attachment_id = None; target = All })
             |> assert_code Invalid_request;
             C.Session_handle.stop writer ~mode:Cancel |> protocol_ok |> ignore;
             let rec await_stopped () =
               let current =
                 C.Goal_controls.list reader_goals |> protocol_ok |> available
               in
               if not (Q.Availability.equal current.availability Stopped)
               then (
                 Eio.Fiber.yield ();
                 await_stopped ())
             in
             await_stopped ();
             C.Session_handle.close reader;
             C.Session_handle.close writer;
             command)
       in
       let after = state entry in
       assert (Int64.(after.counters.revision > before.counters.revision));
       let expected = Goal_service_tests.goal after |> P.Goal.progress in
       let unavailable_session = retain_idle_goal daemon in
       let counters_before = !(counters.inference_ports), !(counters.providers) in
       Daemon.shutdown daemon |> protocol_ok;
       let inspector =
         Daemon.start
           ~sw
           ~env
           ~config:(config root root (Filename.concat root "parent.chatmd"))
           ~tool_dir:root
           ~home:root
           ~process_start_identity:None
           ~options:
             { Daemon.default_options with
               startup_mode = Operator_only
             ; inference_policy = policy
             ; qualify_chatml_extensions = true
             }
           ()
         |> protocol_ok
       in
       Exn.protect
         ~finally:(fun () -> Daemon.shutdown inspector |> protocol_ok)
         ~f:(fun () ->
           let registry = Daemon.registry inspector in
           let summary = Registry.stats registry in
           [%test_eq: int] 0 summary.loaded;
           assert (summary.indexed > 0);
           let connect =
             Agent_server_wire_fixture.http_connector
               ~sw
               ~env
               ~daemon:inspector
               ~root
               ~principal
           in
           let external_actor =
             principal_with_scopes
               "pri_goal_inspection_outsider"
               (P.Scope.Set.of_list [ View_session_transcript ])
           in
           let external_connect =
             Agent_server_wire_fixture.http_connector
               ~sw
               ~env
               ~daemon:inspector
               ~root
               ~principal:external_actor
           in
           with_client external_connect (fun client ->
             expect_features (initialize client) ~controls:false;
             let goals = C.Goal_controls.create client ~session () in
             C.Goal_controls.list goals |> assert_code Permission_denied);
           let client = connect () in
           Exn.protect
             ~finally:(fun () -> C.Connection.close client)
             ~f:(fun () ->
               let initialized = initialize client in
               expect_features initialized ~controls:false;
               assert (
                 P.Id.Server.equal initialized.server_id (P.Session_ref.server_id session));
               let goals = C.Goal_controls.create client ~session () in
               let stopped = C.Goal_controls.list goals |> protocol_ok |> available in
               assert (Q.Availability.equal Stopped stopped.availability);
               assert (not stopped.can_create);
               let goal = goal_row stopped in
               assert (List.is_empty goal.actions);
               assert (P.Goal.Progress.equal goal.goal.progress expected);
               assert (Option.is_some goal.accounting);
               let last_idle =
                 Registry.read_state
                   registry
                   ~authorize:(fun summary ->
                     assert (
                       Agent_server.Authorization.session_visible_to principal summary);
                     Ok ())
                   (P.Session_ref.session_id unavailable_session)
                 |> protocol_ok
               in
               assert_idle last_idle;
               let unavailable =
                 C.Goal_controls.create client ~session:unavailable_session ()
               in
               let retained =
                 C.Goal_controls.list unavailable |> protocol_ok |> available
               in
               assert (Q.Availability.equal Unavailable retained.availability);
               assert (not retained.can_create);
               assert (List.is_empty (goal_row retained).actions);
               let missing_goal =
                 P.Id.Goal.of_string "goal_missing_operator" |> protocol_ok
               in
               let missing =
                 C.Goal_controls.get goals missing_goal |> protocol_ok |> available
               in
               assert (List.is_empty missing.goals);
               let missing_session =
                 P.Session_ref.create
                   ~server_id:initialized.server_id
                   ~session_id:(P.Id.Session.create ())
               in
               let missing = C.Goal_controls.create client ~session:missing_session () in
               (match C.Goal_controls.list missing |> protocol_ok with
                | Available _ ->
                  failwith "missing Goal session was fabricated by inspection"
                | Unavailable actual ->
                  assert (P.Session_ref.equal actual missing_session));
               let denied_command =
                 P.Goal_control.Request.command
                   ~session
                   ~attachment_id:(P.Id.Attachment.create ())
                   accepted_command
                 |> protocol_ok
               in
               C.Connection.request_without_history client (Goal_control denied_command)
               |> assert_code Invalid_state;
               [%test_eq: int] summary.loaded (Registry.stats registry).loaded;
               [%test_eq: int] summary.indexed (Registry.stats registry).indexed;
               [%test_eq: int * int]
                 counters_before
                 (!(counters.inference_ports), !(counters.providers)))));
  print_endline
    "writer, read-only and foreign leases; observed Stop and unavailable owner";
  [%expect
    {| writer, read-only and foreign leases; observed Stop and unavailable owner |}]
;;

let%expect_test "Goal Start acknowledgements bind the exact Run request and Goal fences" =
  with_recoverable_fixture
    (fun env sw root daemon _policy entry _runtime _revision _counters ->
       let session, current = Goal_service_tests.create_goal daemon entry in
       let goal = Goal_service_tests.goal current in
       let goal_id = P.Goal.id goal in
       let session_id = P.Session_ref.session_id session in
       let source = P.Goal.source goal in
       let key =
         P.Idempotency_key.of_string "acknowledged-start-fixture" |> protocol_ok
       in
       let expected_goal_revision = P.Goal.revision goal in
       let expected_catalog_revision =
         Option.value_exn current.goal_catalog |> A.Goal_catalog.revision
       in
       let intent ~goal_id ~expected_goal_revision ~input =
         W.Start.create
           ~generation:current.identity.generation
           ~expected_revision:current.counters.revision
           ~source
           ~goal_id
           ~expected_goal_revision
           ~expected_catalog_revision
           ~input
           ~key
         |> protocol_ok
       in
       let original = intent ~goal_id ~expected_goal_revision ~input:Authored_start in
       let connect =
         Agent_server_wire_fixture.http_connector ~sw ~env ~daemon ~root ~principal
       in
       with_client connect (fun host ->
         let authority = initialize host in
         let receipt =
           P.Run_receipt.create
             ~run_id:(P.Id.Run.create ())
             ~principal_id:authority.principal.id
             ~source
             ~key
             ~request_sha256:
               (W.Start.run_request_sha256 original ~session_id |> protocol_ok)
             ~kind:Admission
             ~run_revision:0L
             ~session_revision:(Int64.succ current.counters.revision)
             ~committed_at:authority.server_time
           |> protocol_ok
         in
         let association_sha256 =
           W.Start.association_sha256
             ~goal_id
             ~expected_goal_revision
             ~expected_catalog_revision
             ~expected_source:(Some source)
         in
         let admission =
           W.Start_result.Admission.create receipt ~association_sha256 |> protocol_ok
         in
         let result = W.Result.Start (Retained admission) in
         let decoded =
           P.Public.Result.of_json
             ~method_:(W.Result.method_name result)
             (W.Result.payload result)
         in
         let client =
           C.In_memory.create
             ~request:(function
               | P.Command.Protocol_initialize _ as command ->
                 P.Public.Result.of_json
                   ~method_:(P.Command.method_name command)
                   (P.Initialize.Response.to_json authority)
               | Goal_control (Start _) -> decoded
               | _ -> failwith "unexpected acknowledgement fixture request")
             ~notifications:(Eio.Stream.create 1)
             ~close:ignore
         in
         Exn.protect
           ~finally:(fun () -> C.Connection.close client)
           ~f:(fun () ->
             ignore (initialize client : P.Initialize.Response.t);
             let writer = P.Id.Attachment.create () in
             let controls =
               C.Goal_controls.create client ~session ~attachment:(fun () -> writer) ()
             in
             (match C.Goal_controls.start controls original |> protocol_ok with
              | Retained actual -> assert (P.Run_receipt.equal receipt actual.receipt)
              | _ -> failwith "exact Goal Start acknowledgement was rejected");
             let changed_input =
               intent
                 ~goal_id
                 ~expected_goal_revision
                 ~input:
                   (User_submission
                      { kind = Plain_text; text = "another input"; attachments = [] })
             in
             let another_goal =
               P.Id.Goal.of_string "goal_other_acknowledgement" |> protocol_ok
             in
             let changed_goal =
               intent ~goal_id:another_goal ~expected_goal_revision ~input:Authored_start
             in
             let changed_revision =
               intent
                 ~goal_id
                 ~expected_goal_revision:(Int64.succ expected_goal_revision)
                 ~input:Authored_start
             in
             List.iter
               [ changed_input; changed_goal; changed_revision ]
               ~f:(fun expected ->
                 C.Goal_controls.start controls expected |> assert_code Invalid_request))));
  print_endline "exact Start accepted; changed input, Goal and revision rejected";
  [%expect {| exact Start accepted; changed input, Goal and revision rejected |}]
;;

let%expect_test
    "Goal RPC persistence errors retain typed retry semantics and omit diagnostics"
  =
  List.iter [ `Metadata; `Audit ] ~f:(fun failure_port ->
    let append = ref `Ready in
    let directory, message =
      match failure_port with
      | `Metadata -> "/journal/", "Goal metadata acknowledgement is unavailable"
      | `Audit -> "/audit/", "Goal response audit could not be published"
    in
    let wrap_env env =
      Crash_fault_io.wrap
        env
        ~matches:(fun path ->
          String.is_substring path ~substring:directory
          && String.is_suffix path ~suffix:".log")
        ~boundary:After_create
        ~reached:(fun path ->
          match !append with
          | `Armed ->
            append := `Failed;
            raise (Core_unix.Unix_error (EIO, "Goal fixture journal append", path))
          | `Ready | `Failed -> ())
    in
    with_recoverable_fixture
      ~wrap_env
      (fun env sw root daemon _policy entry _runtime _revision _counters ->
         let session, _ = Goal_service_tests.create_goal daemon entry in
         let connect =
           Agent_server_wire_fixture.http_connector ~sw ~env ~daemon ~root ~principal
         in
         with_client connect (fun owner ->
           ignore (initialize owner : P.Initialize.Response.t);
           let failure =
             match failure_port with
             | `Audit ->
               let goals = C.Goal_controls.create owner ~session () in
               append := `Armed;
               C.Goal_controls.list goals |> rejected
             | `Metadata ->
               let writer = attach sw env owner session Read_write in
               let goals = C.Session_handle.goals writer |> protocol_ok in
               let current = C.Goal_controls.list goals |> protocol_ok |> available in
               let row = goal_row current in
               let progress =
                 P.Goal.Progress.of_string "Progress under test" |> protocol_ok
               in
               let command =
                 P.Goal_command.create
                   ~session_id:(P.Session_ref.session_id session)
                   ~generation:current.generation
                   ~source:current.source
                   ~expected_catalog_revision:current.catalog_revision
                   ~expected_goal_revision:(Some row.goal.revision)
                   ~key:
                     (P.Idempotency_key.of_string "goal-metadata-failure" |> protocol_ok)
                   ~mutation:(Record_progress (row.goal.id, progress))
                 |> protocol_ok
               in
               append := `Armed;
               let failure = C.Goal_controls.command goals command |> rejected in
               C.Session_handle.close writer;
               failure
           in
           (match !append with
            | `Failed -> ()
            | `Ready | `Armed -> failwith "expected journal append failure");
           [%test_eq: P.Error.code] Persistence_error failure.code;
           assert failure.retryable;
           [%test_eq: string] message failure.message;
           assert (Jsonaf.exactly_equal failure.data (`Object [])));
         match failure_port with
         | `Audit -> ()
         | `Metadata ->
           Uncertain_writer_test_support.close_uncertain_entry
             entry
             ~env
             ~store:(Daemon.store daemon)));
  print_endline "metadata and audit I/O failures preserve the public error contract";
  [%expect {| metadata and audit I/O failures preserve the public error contract |}]
;;
