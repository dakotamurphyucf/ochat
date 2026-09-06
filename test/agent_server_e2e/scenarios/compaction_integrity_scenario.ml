open Core
module F = Crash_recovery_fixture
module Provider = Support.Compaction_json_provider
module Http = Support.Http_driver
module Config = Support.Config_fixture
module Durable = Agent_protocol.Event.Durable

let fail message =
  raise_s [%sexp "compaction integrity assertion failed", (message : string)]
;;

let require condition message = if not condition then fail message
let equal label sexp_of expected actual = F.require_equal label sexp_of expected actual

let prompt =
  "<developer>compaction-retained-alpha</developer>\n\
   <developer>compaction-retained-beta</developer>\n\
   <user><system-reminder>compaction-prior-summary</system-reminder></user>\n\
   <user>compaction-seeded-user</user>\n\
   <assistant>compaction-seeded-answer</assistant>"
;;

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    port)
;;

let with_daemon env fixture port ~cwd f =
  Eio.Switch.run (fun sw ->
    let environment_overrides =
      [ "OPENAI_API_KEY", "compaction-local-test-key"
      ; "API_URL", sprintf "http://127.0.0.1:%d" port
      ]
    in
    let daemon =
      Support.Daemon_process.start_in_directory_with_environment_overrides
        ~sw
        ~env
        ~fixture
        ~cwd
        ~environment_overrides
        ~config_path:(Config.config_path fixture)
    in
    Exn.protect
      ~f:(fun () ->
        F.wait_ready env daemon;
        F.with_client ~sw env fixture (f sw))
      ~finally:(fun () -> F.stop env daemon))
;;

let assert_history
      (expected : Agent_protocol.Snapshot.t)
      (actual : Agent_protocol.Snapshot.t)
  =
  equal
    "canonical IDs/payload/order"
    [%sexp_of: Agent_protocol.History.Window.t]
    expected.canonical_history
    actual.canonical_history;
  equal
    "effective IDs/payload/order"
    [%sexp_of: Agent_protocol.History.Window.t option]
    expected.effective_history
    actual.effective_history;
  equal
    "deferred history"
    [%sexp_of: Agent_protocol.History.entry list]
    expected.deferred_entries
    actual.deferred_entries
;;

let assert_seed (snapshot : Agent_protocol.Snapshot.t) =
  let window = snapshot.canonical_history in
  require
    (window.reached_start && window.reached_end && window.structurally_complete)
    "seed history is truncated";
  require (List.length window.entries = 5) "seed must contain all five prompt items"
;;

let attachment (created : Agent_protocol.Method_result.Create.t) =
  (Option.value_exn created.attachment).attachment.id
;;

let compact created revision key =
  Agent_protocol.Command.Session_compact
    { session_id = created.Agent_protocol.Method_result.Create.session.id
    ; attachment_id = attachment created
    ; expected_revision = Some revision
    ; idempotency_key = F.key key
    }
;;

let begin_compaction client created revision key =
  match F.request client (compact created revision key) with
  | Session_compact result ->
    (match result.session.observed_state with
     | Compacting operation_id -> operation_id
     | _ -> fail "compact did not acknowledge a live operation")
  | _ -> fail "compact returned wrong result"
;;

let transcript_markers =
  [ "compaction-retained-alpha"
  ; "compaction-retained-beta"
  ; "compaction-prior-summary"
  ; "compaction-seeded-user"
  ; "compaction-seeded-answer"
  ; "<conversation>"
  ]
;;

let assert_request request =
  let fields =
    match Provider.body request with
    | `Object fields -> fields
    | _ -> fail "request is not JSON object"
  in
  require
    (match List.Assoc.find fields "stream" ~equal:String.equal with
     | Some `False -> true
     | _ -> false)
    "compactor did not request nonstreaming JSON";
  let inputs =
    List.Assoc.find_exn fields "input" ~equal:String.equal |> Jsonaf.to_string
  in
  List.iter transcript_markers ~f:(fun marker ->
    require
      (String.is_substring inputs ~substring:marker)
      ("summary request omitted " ^ marker))
;;

let assert_conflict client command =
  match Http.request client command with
  | Error error ->
    require
      (Agent_protocol.Error.equal_code error.code Conflict)
      ("mutation returned wrong error: "
       ^ Sexp.to_string_hum ([%sexp_of: Agent_protocol.Error.t] error))
  | Ok _ -> fail "concurrent mutation was accepted"
;;

let rebuild created revision key =
  Agent_protocol.Command.Session_rebuild
    { session_id = created.Agent_protocol.Method_result.Create.session.id
    ; attachment_id = attachment created
    ; expected_revision = revision
    ; prompt_choice = Pinned
    ; idempotency_key = F.key key
    }
;;

let reset created revision key =
  Agent_protocol.Command.Session_reset
    { session_id = created.Agent_protocol.Method_result.Create.session.id
    ; attachment_id = attachment created
    ; expected_revision = revision
    ; keep_history = false
    ; keep_tasks = true
    ; keep_cache = true
    ; keep_workspace = true
    ; keep_grants = true
    ; keep_labels = true
    ; idempotency_key = F.key key
    }
;;

let assert_precommit client created (before : Agent_protocol.Snapshot.t) =
  let blocked = F.get client created.Agent_protocol.Method_result.Create.session.id in
  assert_history before blocked;
  require
    Int64.(blocked.revision > before.revision)
    "compaction start did not advance revision";
  List.iter
    [ "compact", compact; "reset", reset; "rebuild", rebuild ]
    ~f:(fun (name, command) ->
      assert_conflict client (command created blocked.revision (name ^ ":busy"));
      assert_conflict client (command created before.revision (name ^ ":stale")));
  let current = F.get client created.session.id in
  equal
    "rejected mutations changed snapshot"
    [%sexp_of: Agent_protocol.Snapshot.t]
    blocked
    current
;;

let await_terminal env client session_id =
  let rec poll () =
    let snapshot = F.get client session_id in
    match snapshot.session.observed_state with
    | Stopped ->
      require
        (Option.is_none snapshot.session.active_operation)
        "terminal snapshot retained active operation";
      snapshot
    | Compacting _ ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      poll ()
    | _ -> fail "compaction reached unexpected lifecycle"
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. poll
;;

let rec collect env stream previous through acc =
  if Int64.(previous >= through)
  then List.rev acc
  else (
    let frame =
      Http.Sse.next stream ~clock:(Eio.Stdenv.clock env) ~timeout_seconds:5.
      |> Result.ok_or_failwith
    in
    let event = Durable.of_json (Jsonaf.of_string frame.data) |> F.protocol_ok in
    require Int64.(event.sequence = previous + 1L) "durable event gap or duplicate";
    collect env stream event.sequence through (event :: acc))
;;

let with_events
      ~sw
      env
      client
      (before : Agent_protocol.Snapshot.t)
      (after : Agent_protocol.Snapshot.t)
      f
  =
  let stream, _response =
    Http.open_session_events
      client
      ~sw
      ~session_id:before.session.id
      ~after_sequence:before.latest_event_sequence
      ()
    |> Result.ok_or_failwith
  in
  Exn.protect
    ~f:(fun () ->
      f (collect env stream before.latest_event_sequence after.latest_event_sequence []))
    ~finally:(fun () -> Http.Sse.close stream)
;;

let events ~sw env client before after = with_events ~sw env client before after Fn.id

let payload event =
  Durable.Payload.of_json ~kind:event.Durable.kind event.payload |> F.protocol_ok
;;

let terminal_events events =
  List.filter_map events ~f:(fun event ->
    match payload event with
    | Operation_completed op
    | Operation_cancelled op
    | Operation_failed op
    | Operation_interrupted op -> Some (event, op)
    | _ -> None)
;;

let assert_terminal operation_id expected events =
  match terminal_events events with
  | [ (event, operation) ] ->
    equal
      "terminal operation identity"
      [%sexp_of: Agent_protocol.Id.Operation.t]
      operation_id
      operation.id;
    equal "terminal event kind" [%sexp_of: Durable.kind] expected event.kind;
    (match expected, operation.state with
     | Operation_completed, Completed | Operation_cancelled, Cancelled -> ()
     | Operation_failed, Failed error ->
       require
         (String.is_substring error.message ~substring:"Missing_summary")
         "summary failed for an unexpected reason"
     | _ -> fail "terminal event and operation state disagree");
    require
      (Agent_protocol.Operation.equal_kind operation.kind Compaction)
      "wrong operation kind";
    event
  | _ -> fail "expected exactly one terminal operation event"
;;

let history_events events =
  List.filter events ~f:(fun event ->
    match event.Durable.kind with
    | History_replaced | History_appended | History_message_deferred -> true
    | _ -> false)
;;

let assert_no_replacement events =
  require
    (List.is_empty (history_events events))
    "unsuccessful compaction mutated history"
;;

let assert_atomic_event (after : Agent_protocol.Snapshot.t) terminal events =
  match history_events events with
  | [ event ] ->
    require
      Int64.(event.revision = terminal.Durable.revision)
      "replacement was not atomic with completion";
    require
      Int64.(event.sequence + 1L = terminal.sequence)
      "replacement and completion are not adjacent";
    (match payload event with
     | History_replaced window ->
       equal
         "replacement event/full snapshot"
         [%sexp_of: Agent_protocol.History.Window.t]
         after.canonical_history
         window
     | _ -> fail "history changed incrementally")
  | _ -> fail "expected exactly one atomic history replacement"
;;

let reminder_text summary =
  sprintf
    "<system-reminder>This is a message from the system that we compacted the \
     conversation history from a previous session.\n\
     Here is a summary of the session that you saved:\n\
     %s\n\
     Remember this is not a message from the user, but a system reminder that you should \
     not respond to.\n\
     </system-reminder>"
    summary
;;

let expected_reminder id marker =
  let item =
    Openai.Responses.Item.Input_message
      { role = User
      ; content = [ Text { text = reminder_text marker; _type = "input_text" } ]
      ; _type = "message"
      }
  in
  Agent_protocol.History.
    { id
    ; role = User
    ; kind = Message
    ; payload = Openai.Responses.Item.jsonaf_of_t item
    ; provenance = Canonical
    ; redacted = false
    }
;;

let assert_reminder before (reminder : Agent_protocol.History.entry) marker =
  require
    (not
       (List.exists
          before.Agent_protocol.Snapshot.canonical_history.entries
          ~f:(fun entry -> Agent_protocol.History.Id.compare entry.id reminder.id = 0)))
    "reminder reused an old ID";
  equal
    "new reminder full payload"
    [%sexp_of: Agent_protocol.History.entry]
    (expected_reminder reminder.id marker)
    reminder
;;

let assert_replacement before (after : Agent_protocol.Snapshot.t) marker terminal events =
  assert_atomic_event after terminal events;
  let retained = List.take before.Agent_protocol.Snapshot.canonical_history.entries 3 in
  let actual, new_entries = List.split_n after.canonical_history.entries 3 in
  equal
    "retained prompt/reminder IDs and payloads"
    [%sexp_of: Agent_protocol.History.entry list]
    retained
    actual;
  match new_entries with
  | [ reminder ] -> assert_reminder before reminder marker
  | _ -> fail "replacement must allocate exactly one new reminder"
;;

let seed_and_block env client provider =
  let created = F.create_session client in
  let before = F.get client created.session.id in
  assert_seed before;
  let operation_id = begin_compaction client created before.revision "compaction:begin" in
  let request = Provider.await_request provider ~env ~index:0 in
  assert_request request;
  assert_precommit client created before;
  created, before, operation_id, request
;;

let success ~sw env client provider =
  let created, before, operation_id, request = seed_and_block env client provider in
  let marker = "compaction-success-exact-summary" in
  Provider.release request (Summary marker);
  Provider.await_returned request ~env;
  let after = await_terminal env client created.session.id in
  let events = events ~sw env client before after in
  let terminal = assert_terminal operation_id Operation_completed events in
  assert_replacement before after marker terminal events;
  require
    (Provider.request_count provider = 1)
    "success made unexpected provider requests";
  after
;;

let failure ~sw env client provider =
  let created, before, operation_id, request = seed_and_block env client provider in
  Provider.release request Missing_summary;
  Provider.await_returned request ~env;
  let after = await_terminal env client created.session.id in
  assert_history before after;
  let events = events ~sw env client before after in
  ignore (assert_terminal operation_id Operation_failed events : Durable.t);
  assert_no_replacement events;
  require (Provider.request_count provider = 1) "missing summary unexpectedly retried";
  after
;;

let cancel client created operation_id =
  match
    F.request
      client
      (Session_cancel_operation
         { session_id = created.Agent_protocol.Method_result.Create.session.id
         ; attachment_id = attachment created
         ; operation_id
         ; idempotency_key = F.key "compaction:cancel"
         })
  with
  | Session_cancel_operation _ -> ()
  | _ -> fail "cancel returned wrong result"
;;

let assert_stable client expected =
  let actual = F.get client expected.Agent_protocol.Snapshot.session.id in
  equal
    "provider release changed terminal snapshot"
    [%sexp_of: Agent_protocol.Snapshot.t]
    expected
    actual
;;

let cancellation ~sw env client provider =
  let created, before, operation_id, request = seed_and_block env client provider in
  cancel client created operation_id;
  let cancelled = await_terminal env client created.session.id in
  assert_history before cancelled;
  with_events ~sw env client before cancelled (fun cancelled_events ->
    assert_no_replacement cancelled_events;
    let stable = F.get client created.session.id in
    Provider.release request (Summary "cancelled-late-summary-must-never-install");
    Provider.await_returned request ~env;
    assert_stable client stable;
    ignore (assert_terminal operation_id Operation_cancelled cancelled_events : Durable.t));
  cancelled
;;

let private_cwd environment name =
  let roots = Support.Temporary_environment.roots environment in
  let cwd =
    Support.Temporary_environment.path environment (Filename.concat roots.temporary name)
  in
  Eio.Path.mkdir ~perm:0o700 cwd;
  cwd
;;

let run_case env environment name test =
  let fixture = F.fixture env environment name in
  let cwd = private_cwd environment name in
  F.write env (Config.prompt_path fixture) prompt;
  Eio.Switch.run (fun sw ->
    let port = reserve_port env in
    let provider = Provider.start ~sw ~env ~port in
    let expected =
      with_daemon env fixture port ~cwd (fun sw client -> test ~sw env client provider)
    in
    with_daemon env fixture port ~cwd (fun _sw client ->
      let actual = F.get client expected.Agent_protocol.Snapshot.session.id in
      assert_history expected actual;
      require
        (Agent_protocol.Id.Session.compare expected.session.id actual.session.id = 0)
        "restart changed session ID"))
;;

let cases =
  [ "http.atomic-replacement-restart", "compact-atomic", success
  ; "http.cancel-blocked-summary", "compact-cancel", cancellation
  ; "http.failed-summary-no-replacement", "compact-failure", failure
  ]
;;

let report env selected =
  let sexp =
    [%sexp
      { scenario = ("compaction-integrity" : string)
      ; passed_cases = (List.map selected ~f:(fun (name, _, _) -> name) : string list)
      }]
  in
  Eio.Flow.copy_string (Sexp.to_string_hum sexp ^ "\n") (Eio.Stdenv.stdout env)
;;

let run env ~case =
  let selected =
    match case with
    | None -> cases
    | Some name ->
      [ List.find_exn cases ~f:(fun (candidate, _, _) -> String.equal name candidate) ]
  in
  Support.Temporary_environment.with_
    ~scenario:"compaction-integrity"
    ~env
    (fun environment ->
       List.iter selected ~f:(fun (name, fixture, test) ->
         try run_case env environment fixture test with
         | exn ->
           raise_s
             [%sexp "compaction integrity case failed", (name : string), (exn : Exn.t)]);
       report env selected)
;;
