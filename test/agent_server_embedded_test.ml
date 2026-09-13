open! Core

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let observed_stopped = function
  | Agent_protocol.Session.Stopped -> true
  | Queued_for_slot
  | Starting
  | Recovering
  | Idle
  | Running_turn _
  | Compacting _
  | Waiting_for_permission _
  | Stopping
  | Failed _ -> false
;;

let schedule_scheduled = function
  | Agent_protocol.Schedule.Scheduled -> true
  | Delivering | Delivered | Cancelled | Failed _ -> false
;;

let schedule_delivered = function
  | Agent_protocol.Schedule.Delivered -> true
  | Scheduled | Delivering | Cancelled | Failed _ -> false
;;

let job_delivered = function
  | Agent_protocol.Job.Delivered _ -> true
  | Not_required | Pending | Discarded _ -> false
;;

let temporary_root env =
  let name =
    Agent_protocol.Id.Transaction.create ()
    |> Agent_protocol.Id.Transaction.to_string
    |> fun value -> "ochat-embedded-test-" ^ value
  in
  let path = Filename.concat "/tmp" name in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / path);
  path
;;

let with_fixture f =
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
          "<developer>You are an embedded test agent.</developer>";
        f env root workspace prompt_file)
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)))
;;

let%expect_test "invalid authoring files fail before creating an embedded durable store" =
  with_fixture (fun env root workspace prompt_file ->
    let file = Filename.concat root "invalid-package.json" in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / file)
      {|{"version":1,"packages":[],"grant":"all"}|};
    let data_root = Filename.concat root "uncreated-store" in
    Eio.Switch.run (fun sw ->
      let options : Agent_server.Embedded.options =
        { prompt_file
        ; workspace
        ; tool_dir = workspace
        ; home = root
        ; data_root = Some data_root
        ; start_immediately = true
        ; permission_profile = Agent_server.Embedded.default_permission_profile
        ; attachment_mode = Read_write
        ; event_capacity = 128
        }
      in
      (match
         Agent_server.Embedded.start ~sw ~env ~authoring_package_files:[ file ] options
       with
       | Error error -> [%test_eq: Agent_protocol.Error.code] Invalid_request error.code
       | Ok host ->
         Agent_server.Embedded.close host;
         failwith "invalid package started a host");
      assert (not (Eio.Path.is_directory Eio.Path.(Eio.Stdenv.fs env / data_root)))));
  print_endline "package rejected; durable store not created";
  [%expect {| package rejected; durable store not created |}]
;;

let%expect_test "embedded host uses the shared protocol and process-bound session" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = None
          ; start_immediately = true
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let probe = Agent_server.Embedded.connect embedded in
      let initialized =
        Agent_client.Session_handle.initialize
          probe
          ~implementation_name:"host-metadata"
          ~implementation_version:"test"
        |> protocol_ok
      in
      let metadata = Option.value_exn initialized.extensions in
      assert (
        Agent_protocol.Extension_capabilities.equal_host metadata.host Embedded_transient);
      assert (
        Agent_protocol.Extension_capabilities.equal_journal_flush
          metadata.journal_flush
          Synced);
      [%test_eq: string list]
        [ "chatml.authoring.v1"
        ; "chatml.background.v1"
        ; "chatml.invocations.v1"
        ; "chatml.notifications.v1"
        ]
        metadata.available_features;
      Agent_client.Connection.close probe;
      let session_id = Agent_server.Embedded.session_id embedded in
      let response =
        Agent_client.Connection.request
          (Agent_server.Embedded.connection embedded)
          (Session_get { session_id; history = None })
        |> protocol_ok
      in
      let snapshot =
        match response with
        | Agent_protocol.Method_result.Session_get snapshot -> snapshot
        | _ -> failwith "unexpected response"
      in
      let attachment = Agent_server.Embedded.attachment embedded in
      Agent_server.Embedded.close embedded;
      print_s
        [%sexp
          { execution_host =
              (snapshot.session.spec.execution_host
               : Agent_protocol.Session.execution_host)
          ; liveness = (snapshot.session.spec.liveness : Agent_protocol.Session.liveness)
          ; observed =
              (snapshot.session.observed_state : Agent_protocol.Session.observed_state)
          ; attachment_mode = (attachment.mode : Agent_protocol.Session.attachment_mode)
          }]));
  [%expect
    {|
    ((execution_host Embedded) (liveness Process_bound) (observed Idle)
     (attachment_mode Read_write))
    |}]
;;

let%expect_test "session creation returns the requested attachment after session.created" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = None
          ; start_immediately = false
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let connection = Agent_server.Embedded.connection embedded in
      let original =
        Agent_client.Connection.request
          connection
          (Session_get
             { session_id = Agent_server.Embedded.session_id embedded; history = None })
        |> protocol_ok
        |> function
        | Agent_protocol.Method_result.Session_get snapshot -> snapshot
        | _ -> failwith "unexpected session response"
      in
      let idempotency_key =
        Agent_protocol.Idempotency_key.of_string "create-with-attachment" |> protocol_ok
      in
      let created =
        Agent_client.Connection.request
          connection
          (Session_create
             { spec = original.session.spec
             ; requested_mode = Some Read_only
             ; subscribe = true
             ; idempotency_key
             })
        |> protocol_ok
        |> function
        | Agent_protocol.Method_result.Session_create result -> result
        | _ -> failwith "unexpected creation response"
      in
      let attachment = Option.value_exn created.attachment in
      let first_event_present =
        match attachment.replay with
        | Agent_protocol.Method_result.Attach.Snapshot snapshot ->
          Int64.(snapshot.latest_event_sequence >= 2L)
        | Current | Events _ -> false
      in
      Agent_server.Embedded.close embedded;
      print_s
        [%sexp
          { attachment_mode =
              (attachment.attachment.mode : Agent_protocol.Session.attachment_mode)
          ; first_event_present : bool
          ; revision = (created.mutation.revision : int64)
          ; sequence = (created.mutation.latest_event_sequence : int64)
          }]));
  [%expect
    {|
    ((attachment_mode Read_only) (first_event_present false) (revision 3)
     (sequence 1))
    |}]
;;

let no_op_turn_prompt =
  {|
<developer>Handle turns without a model provider.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event =
    [ `Session_start | `Session_resume | `Item_appended(item) | `Turn_start | `Turn_end ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx st ev ->
      match ev with
      | `Session_start -> Task.pure(st + 1)
      | `Session_resume -> Task.pure(st)
      | `Item_appended(_) -> Task.pure(st)
      | `Turn_start -> Task.pure(st + 1)
      | `Turn_end -> Task.pure(st)
</script>
|}
;;

let submission_halt_prompt =
  {|
<developer>Never contact a provider.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Session_resume | `Item_appended(item) | `Turn_start ]
  let initial_state = 0
  let on_event : context -> state -> event -> state task =
    fun ctx st ev -> match ev with
    | `Item_appended(item) ->
      Task.bind(Runtime.end_session("submitted:" ++ to_string(st)), fun ignored -> Task.pure(st + 1))
    | `Turn_start -> Task.bind(Runtime.end_session("wrong-boundary"), fun ignored -> Task.pure(st))
    | _ -> Task.pure(st)
</script>
|}
;;

let submission_snapshot embedded =
  let session_id = Agent_server.Embedded.session_id embedded in
  match
    Agent_client.Connection.request
      (Agent_server.Embedded.connection embedded)
      (Session_get { session_id; history = None })
    |> protocol_ok
  with
  | Session_get snapshot -> snapshot
  | _ -> failwith "unexpected submission snapshot"
;;

let rec await_submission env embedded attempts =
  let snapshot = submission_snapshot embedded in
  if Option.is_none snapshot.session.active_operation
  then snapshot
  else if attempts = 0
  then failwith "submitted-message moderation did not finish"
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    await_submission env embedded (attempts - 1))
;;

let submit_halt embedded =
  let session_id = Agent_server.Embedded.session_id embedded in
  let attachment_id = (Agent_server.Embedded.attachment embedded).id in
  let idempotency_key =
    Agent_protocol.Idempotency_key.of_string "submitted:halt" |> protocol_ok
  in
  let content =
    Agent_protocol.Session.Message_content.
      { kind = Plain_text; text = "stop at item-appended"; attachments = [] }
  in
  ignore
    (Agent_client.Connection.request
       (Agent_server.Embedded.connection embedded)
       (Session_send_message { session_id; attachment_id; idempotency_key; content })
     |> protocol_ok
     : Agent_protocol.Method_result.t)
;;

let%expect_test "submitted user moderation runs once before turn-start or provider work" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(Eio.Stdenv.fs env / prompt_file)
      submission_halt_prompt;
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = None
          ; start_immediately = true
          ; permission_profile = default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      Exn.protect
        ~f:(fun () ->
          let before = submission_snapshot embedded in
          submit_halt embedded;
          let after = await_submission env embedded 200 in
          print_s
            [%sexp
              { halted = (after.halted : bool)
              ; reason = (after.halt_reason : string option)
              ; added_entries =
                  (List.length after.canonical_history.entries
                   - List.length before.canonical_history.entries
                   : int)
              }])
        ~finally:(fun () -> Agent_server.Embedded.close embedded)));
  [%expect {| ((halted true) (reason (submitted:0)) (added_entries 1)) |}]
;;

let%expect_test "read-only sends do not consume history IDs" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(Eio.Stdenv.fs env / prompt_file)
      no_op_turn_prompt;
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = None
          ; start_immediately = true
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let writer = Agent_server.Embedded.connection embedded in
      let reader = Agent_server.Embedded.connect embedded in
      Agent_client.Session_handle.initialize
        reader
        ~implementation_name:"read-only-history-test"
        ~implementation_version:"dev"
      |> protocol_ok
      |> ignore;
      let session_id = Agent_server.Embedded.session_id embedded in
      let reader_attachment =
        Agent_client.Connection.request
          reader
          (Session_attach
             { session_id
             ; requested_mode = Read_only
             ; subscribe = false
             ; after_sequence = None
             ; reclaim_token = None
             ; idempotency_key =
                 Agent_protocol.Idempotency_key.of_string "readonly-history:attach"
                 |> protocol_ok
             })
        |> protocol_ok
        |> function
        | Agent_protocol.Method_result.Session_attach attached -> attached.attachment
        | _ -> failwith "unexpected attach response"
      in
      let send connection attachment text key =
        Agent_client.Connection.request
          connection
          (Session_send_message
             { session_id
             ; attachment_id = attachment.Agent_protocol.Session.Attachment.id
             ; content = { kind = Plain_text; text; attachments = [] }
             ; idempotency_key =
                 Agent_protocol.Idempotency_key.of_string key |> protocol_ok
             })
      in
      let accepted result =
        match result |> protocol_ok with
        | Agent_protocol.Method_result.Session_send_message sent -> sent
        | _ -> failwith "unexpected send response"
      in
      let writer_attachment = Agent_server.Embedded.attachment embedded in
      let first =
        send writer writer_attachment "first" "readonly-history:first" |> accepted
      in
      let rejected =
        match send reader reader_attachment "rejected" "readonly-history:rejected" with
        | Error error -> Agent_protocol.Error.equal_code error.code Permission_denied
        | Ok _ -> false
      in
      let second =
        send writer writer_attachment "second" "readonly-history:second" |> accepted
      in
      let first_sequence = History_entry.Id.sequence first.history_id in
      let second_sequence = History_entry.Id.sequence second.history_id in
      Agent_client.Connection.close reader;
      Agent_server.Embedded.close embedded;
      print_s
        [%sexp
          { rejected : bool
          ; consecutive = (Int.equal second_sequence (first_sequence + 1) : bool)
          }]));
  [%expect {| ((rejected true) (consecutive true)) |}]
;;

let%expect_test "mutating command idempotency replays and rejects conflicts" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = None
          ; start_immediately = true
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let connection = Agent_server.Embedded.connection embedded in
      let session_id = Agent_server.Embedded.session_id embedded in
      let attachment_id = (Agent_server.Embedded.attachment embedded).id in
      let idempotency_key =
        Agent_protocol.Idempotency_key.of_string "schedule-test-key" |> protocol_ok
      in
      let request payload =
        Agent_protocol.Schedule.Create_request.
          { session_id
          ; attachment_id
          ; payload = `Object [ "value", `String payload ]
          ; due = After_ms 60_000
          ; misfire = Deliver_once_immediately
          ; idempotency_key
          }
      in
      let create payload =
        Agent_client.Connection.request connection (Schedule_create (request payload))
      in
      let overflow_rejected =
        Agent_client.Connection.request
          connection
          (Schedule_create
             { (request "overflow") with
               due = After_ms 9_223_372_037_854
             ; idempotency_key =
                 Agent_protocol.Idempotency_key.of_string "schedule-overflow"
                 |> protocol_ok
             })
        |> function
        | Error error ->
          Agent_protocol.Error.equal_code error.Agent_protocol.Error.code Invalid_request
        | Ok _ -> false
      in
      let first = create "same" |> protocol_ok in
      let second = create "same" |> protocol_ok in
      let first_id, second_id =
        match first, second with
        | Schedule_create first, Schedule_create second ->
          first.schedule.id, second.schedule.id
        | _ -> failwith "unexpected schedule response"
      in
      let conflict =
        match create "different" with
        | Error error -> Agent_protocol.Error.equal_code error.code Idempotency_conflict
        | Ok _ -> false
      in
      let snapshot =
        Agent_client.Connection.request
          connection
          (Session_get { session_id; history = None })
        |> protocol_ok
        |> function
        | Agent_protocol.Method_result.Session_get snapshot -> snapshot
        | _ -> failwith "unexpected session response"
      in
      Agent_server.Embedded.close embedded;
      print_s
        [%sexp
          { replayed_same_id =
              (Agent_protocol.Id.Schedule.compare first_id second_id = 0 : bool)
          ; conflict : bool
          ; overflow_rejected : bool
          ; schedule_count = (List.length snapshot.schedules : int)
          }]));
  [%expect
    {|
    ((replayed_same_id true) (conflict true) (overflow_rejected true)
     (schedule_count 1))
    |}]
;;

let%expect_test "due schedules fail visibly when the prompt has no moderator" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = None
          ; start_immediately = true
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let connection = Agent_server.Embedded.connection embedded in
      let session_id = Agent_server.Embedded.session_id embedded in
      let attachment_id = (Agent_server.Embedded.attachment embedded).id in
      let created =
        Agent_client.Connection.request
          connection
          (Schedule_create
             { session_id
             ; attachment_id
             ; payload = `Object [ "event", `String "wake" ]
             ; due = After_ms 0
             ; misfire = Deliver_once_immediately
             ; idempotency_key =
                 Agent_protocol.Idempotency_key.of_string "schedule-no-moderator"
                 |> protocol_ok
             })
        |> protocol_ok
        |> function
        | Agent_protocol.Method_result.Schedule_create result -> result.schedule
        | _ -> failwith "unexpected schedule response"
      in
      let rec await_terminal attempts =
        let schedule =
          Agent_client.Connection.request
            connection
            (Schedule_get { session_id; schedule_id = created.id })
          |> protocol_ok
          |> function
          | Agent_protocol.Method_result.Schedule_get schedule -> schedule
          | _ -> failwith "unexpected schedule get response"
        in
        match schedule.status with
        | Failed _ | Delivered | Cancelled -> schedule
        | Scheduled | Delivering ->
          if attempts = 0
          then failwith "schedule did not become terminal"
          else (
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
            await_terminal (attempts - 1))
      in
      let terminal = await_terminal 100 in
      let failed =
        match terminal.status with
        | Failed _ -> true
        | Scheduled | Delivering | Delivered | Cancelled -> false
      in
      Agent_server.Embedded.close embedded;
      print_s [%sexp { failed : bool; delivery_count = (terminal.delivery_count : int) }]));
  [%expect {| ((failed true) (delivery_count 0)) |}]
;;

let%expect_test "ChatML session startup persists delayed schedules" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(Eio.Stdenv.fs env / prompt_file)
      {|
<developer>You are an embedded scheduling test agent.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Tick ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx st ev ->
      match ev with
      | `Session_start ->
        Task.bind(Schedule.after_ms(60000, `Tick), fun timer_id ->
        Task.pure(st + 1))
      | `Tick -> Task.pure(st + 1)
</script>
|};
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = None
          ; start_immediately = false
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let connection = Agent_server.Embedded.connection embedded in
      let session_id = Agent_server.Embedded.session_id embedded in
      let snapshot =
        Agent_client.Connection.request
          connection
          (Session_get { session_id; history = None })
        |> protocol_ok
        |> function
        | Agent_protocol.Method_result.Session_get snapshot -> snapshot
        | _ -> failwith "unexpected session response"
      in
      let schedule = List.hd_exn snapshot.schedules in
      Agent_server.Embedded.close embedded;
      print_s
        [%sexp
          { schedule_count = (List.length snapshot.schedules : int)
          ; scheduled = (schedule_scheduled schedule.status : bool)
          ; generation_matches =
              (schedule.generation = snapshot.session.generation : bool)
          }]));
  [%expect
    {|
    ((schedule_count 1) (scheduled true) (generation_matches true))
    |}]
;;

let%expect_test "ChatML synchronous model calls persist intent and terminal state" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(Eio.Stdenv.fs env / prompt_file)
      {|
<developer>You are an embedded synchronous model-call test agent.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx st ev ->
      match ev with
      | `Session_start ->
        Task.bind(
          Model.call(
            "agent_prompt_v1",
            Json.parse("{\"prompt\":\"missing.chatmd\",\"input\":\"test\",\"is_local\":true}")),
          fun result ->
        Task.pure(st + 1))
</script>
|};
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = None
          ; start_immediately = false
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let connection = Agent_server.Embedded.connection embedded in
      let session_id = Agent_server.Embedded.session_id embedded in
      let snapshot =
        Agent_client.Connection.request
          connection
          (Session_get { session_id; history = None })
        |> protocol_ok
        |> function
        | Agent_protocol.Method_result.Session_get snapshot -> snapshot
        | _ -> failwith "unexpected session response"
      in
      let job = List.hd_exn snapshot.jobs in
      let failed =
        match job.Agent_protocol.Job.status with
        | Failed _ -> true
        | Queued
        | Running
        | Waiting_permission _
        | Waiting_completion _
        | Succeeded
        | Cancelled
        | Interrupted _ -> false
      in
      let delivery_not_required =
        match job.delivery with
        | Agent_protocol.Job.Not_required -> true
        | Pending | Delivered _ | Discarded _ -> false
      in
      Agent_server.Embedded.close embedded;
      print_s
        [%sexp
          { job_count = (List.length snapshot.jobs : int)
          ; attempt = (job.attempt : int)
          ; failed : bool
          ; delivery_not_required : bool
          }]));
  [%expect
    {|
    ((job_count 1) (attempt 1) (failed true) (delivery_not_required true))
    |}]
;;

let%expect_test "ChatML startup model jobs persist and deliver while idle" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(Eio.Stdenv.fs env / prompt_file)
      {|
<developer>You are an embedded durable-job test agent.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event =
    [ `Session_start
    | `Model_job_succeeded(string, string, json)
    | `Model_job_failed(string, string, string)
    ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx st ev ->
      match ev with
      | `Session_start ->
        Task.bind(Model.spawn("agent_prompt_v1", `Null), fun job_id ->
        Task.pure(st + 1))
      | `Model_job_succeeded(job_id, recipe, result) ->
        Task.bind(Runtime.end_session("unexpected model success"), fun ignored_end ->
        Task.pure(st + 1))
      | `Model_job_failed(job_id, recipe, message) ->
        Task.bind(Runtime.end_session("durable model job delivered"), fun ignored_end ->
        Task.pure(st + 1))
</script>
|};
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = None
          ; start_immediately = true
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let connection = Agent_server.Embedded.connection embedded in
      let session_id = Agent_server.Embedded.session_id embedded in
      let rec await_delivery attempts =
        let snapshot =
          Agent_client.Connection.request
            connection
            (Session_get { session_id; history = None })
          |> protocol_ok
          |> function
          | Agent_protocol.Method_result.Session_get snapshot -> snapshot
          | _ -> failwith "unexpected session response"
        in
        match snapshot.halted, snapshot.jobs with
        | true, [ job ] when job_delivered job.delivery -> snapshot, job
        | _ when attempts = 0 -> failwith "durable model job was not delivered"
        | _ ->
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
          await_delivery (attempts - 1)
      in
      let snapshot, job = await_delivery 200 in
      let failed =
        match job.Agent_protocol.Job.status with
        | Failed _ -> true
        | Queued
        | Running
        | Waiting_permission _
        | Waiting_completion _
        | Succeeded
        | Cancelled
        | Interrupted _ -> false
      in
      Agent_server.Embedded.close embedded;
      print_s
        [%sexp
          { halted = (snapshot.halted : bool)
          ; halt_reason = (snapshot.halt_reason : string option)
          ; job_count = (List.length snapshot.jobs : int)
          ; attempt = (job.attempt : int)
          ; failed : bool
          ; delivered = (job_delivered job.delivery : bool)
          }]));
  [%expect
    {|
    ((halted true) (halt_reason ("durable model job delivered")) (job_count 1)
     (attempt 1) (failed true) (delivered true))
    |}]
;;

let%expect_test "due schedules drain ChatML while idle and honor end_session" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(Eio.Stdenv.fs env / prompt_file)
      {|
<developer>You are an embedded idle-drain test agent.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start | `Tick ]

  let initial_state = 0

  let on_event : context -> state -> event -> state task =
    fun ctx st ev ->
      match ev with
      | `Session_start ->
        Task.bind(Schedule.after_ms(0, `Tick), fun timer_id ->
        Task.pure(st + 1))
      | `Tick ->
        Task.bind(Runtime.end_session("scheduled stop"), fun ignored_end ->
        Task.pure(st + 1))
</script>
|};
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = None
          ; start_immediately = true
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let connection = Agent_server.Embedded.connection embedded in
      let session_id = Agent_server.Embedded.session_id embedded in
      let rec await_halt attempts =
        let snapshot =
          Agent_client.Connection.request
            connection
            (Session_get { session_id; history = None })
          |> protocol_ok
          |> function
          | Agent_protocol.Method_result.Session_get snapshot -> snapshot
          | _ -> failwith "unexpected session response"
        in
        if snapshot.halted
        then snapshot
        else if attempts = 0
        then failwith "scheduled internal event did not halt the session"
        else (
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
          await_halt (attempts - 1))
      in
      let snapshot = await_halt 200 in
      let schedule = List.hd_exn snapshot.schedules in
      Agent_server.Embedded.close embedded;
      print_s
        [%sexp
          { halted = (snapshot.halted : bool)
          ; halt_reason = (snapshot.halt_reason : string option)
          ; stopped = (observed_stopped snapshot.session.observed_state : bool)
          ; schedule_delivered = (schedule_delivered schedule.status : bool)
          ; delivery_count = (schedule.delivery_count : int)
          }]));
  [%expect
    {|
    ((halted true) (halt_reason ("scheduled stop")) (stopped true)
     (schedule_delivered true) (delivery_count 1))
    |}]
;;

let%expect_test "session handle attaches, mutates, and reduces pushed events" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = None
          ; start_immediately = true
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let connection = Agent_server.Embedded.connect embedded in
      let _ =
        Agent_client.Session_handle.initialize
          connection
          ~implementation_name:"session-handle-test"
          ~implementation_version:"dev"
        |> protocol_ok
      in
      let updates = ref 0 in
      let handle =
        Agent_client.Session_handle.attach
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~connection
          ~session_id:(Agent_server.Embedded.session_id embedded)
          ~mode:Read_write
          ~on_update:(fun _ -> Int.incr updates)
          ()
        |> protocol_ok
      in
      let stopped = Agent_client.Session_handle.stop handle ~mode:Cancel |> protocol_ok in
      let rec await_stopped attempts =
        let snapshot =
          Agent_client.Session_handle.projection handle
          |> Agent_client.Projection.snapshot
        in
        if observed_stopped snapshot.session.observed_state
        then snapshot
        else if attempts = 0
        then failwith "session handle did not observe stopped state"
        else (
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
          await_stopped (attempts - 1))
      in
      let projected = await_stopped 100 in
      let detached = Agent_client.Session_handle.detach handle |> Result.is_ok in
      Agent_client.Connection.close connection;
      Agent_server.Embedded.close embedded;
      print_s
        [%sexp
          { command_stopped = (observed_stopped stopped.observed_state : bool)
          ; projection_stopped =
              (observed_stopped projected.session.observed_state : bool)
          ; updates_received = (!updates > 0 : bool)
          ; detached : bool
          }]));
  [%expect
    {|
    ((command_stopped true) (projection_stopped true) (updates_received true)
     (detached true))
    |}]
;;

let%expect_test "reconnect reattaches from the durable cursor and applies replay" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = None
          ; start_immediately = true
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let connection = Agent_server.Embedded.connect embedded in
      let statuses = ref [] in
      let reconnect =
        Agent_client.Reconnect.attach
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~connection
          ~reconnect:(Some (fun () -> Ok (Agent_server.Embedded.connect embedded)))
          ~session_id:(Agent_server.Embedded.session_id embedded)
          ~mode:Read_write
          ~on_status:(fun status -> statuses := status :: !statuses)
          ()
        |> protocol_ok
      in
      let first_attachment =
        Agent_client.Reconnect.attachment reconnect |> Option.value_exn
      in
      Agent_client.Connection.close connection;
      let idempotency_key =
        Agent_protocol.Id.Transaction.create ()
        |> Agent_protocol.Id.Transaction.to_string
        |> Agent_protocol.Idempotency_key.of_string
        |> protocol_ok
      in
      let _ =
        Agent_client.Connection.request
          (Agent_server.Embedded.connection embedded)
          (Session_stop
             { session_id = Agent_server.Embedded.session_id embedded
             ; attachment_id = (Agent_server.Embedded.attachment embedded).id
             ; mode = Cancel
             ; idempotency_key
             })
        |> protocol_ok
      in
      let rec await_replay attempts =
        let projection = Agent_client.Reconnect.projection reconnect in
        let attachment = Agent_client.Reconnect.attachment reconnect in
        let stopped =
          Agent_client.Projection.snapshot projection
          |> fun (snapshot : Agent_protocol.Snapshot.t) ->
          observed_stopped snapshot.session.observed_state
        in
        let reattached =
          Option.exists attachment ~f:(fun attachment ->
            Agent_protocol.Id.Attachment.compare attachment.id first_attachment.id <> 0)
        in
        if stopped && reattached
        then ()
        else if attempts = 0
        then failwith "reconnect did not apply the missing durable events"
        else (
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
          await_replay (attempts - 1))
      in
      await_replay 200;
      let saw_reconnecting =
        List.exists !statuses ~f:(function
          | Agent_client.Reconnect.Reconnecting _ -> true
          | Connected | Disconnected | Failed _ -> false)
      in
      let final_connected =
        match Agent_client.Reconnect.status reconnect with
        | Connected -> true
        | Reconnecting _ | Disconnected | Failed _ -> false
      in
      Agent_client.Reconnect.close reconnect;
      Agent_server.Embedded.close embedded;
      print_s [%sexp { saw_reconnecting : bool; final_connected : bool }]));
  [%expect {| ((saw_reconnecting true) (final_connected true)) |}]
;;

let%expect_test "audit read returns redacted durable command outcomes" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = Some (Filename.concat root "audit-data")
          ; start_immediately = true
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let connection = Agent_server.Embedded.connection embedded in
      let session_id = Agent_server.Embedded.session_id embedded in
      Agent_client.Connection.request
        connection
        (Session_get { session_id; history = None })
      |> protocol_ok
      |> ignore;
      let page = Agent_protocol.Page.Request.create ~limit:100 () |> protocol_ok in
      let audit =
        Agent_client.Connection.request
          connection
          (Audit_read
             { page
             ; session_id = Some session_id
             ; principal_id = None
             ; minimum_level = None
             ; name_prefix = Some "protocol.command."
             })
        |> protocol_ok
        |> function
        | Agent_protocol.Method_result.Audit_read page -> page
        | _ -> failwith "unexpected audit response"
      in
      Agent_server.Embedded.close embedded;
      print_s
        [%sexp
          { has_session_commands = (not (List.is_empty audit.items) : bool)
          ; all_redacted =
              (List.for_all audit.items ~f:(fun item -> item.redacted) : bool)
          ; ordered =
              (List.is_sorted audit.items ~compare:(fun left right ->
                 Int64.compare left.sequence right.sequence)
               : bool)
          }]));
  [%expect
    {|
    ((has_session_commands true) (all_redacted true) (ordered true))
    |}]
;;

let%expect_test "reset and pinned rebuild require exact stopped revisions" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Switch.run (fun sw ->
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = Some (Filename.concat root "admin-data")
          ; start_immediately = false
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let connection = Agent_server.Embedded.connect embedded in
      let session_id = Agent_server.Embedded.session_id embedded in
      Agent_client.Session_handle.initialize
        connection
        ~implementation_name:"admin-test"
        ~implementation_version:"dev"
      |> protocol_ok
      |> ignore;
      let sessions = Agent_client.Admin.list_sessions connection |> protocol_ok in
      let handle =
        Agent_client.Session_handle.attach
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~connection
          ~session_id
          ~mode:Read_write
          ~subscribe:false
          ()
        |> protocol_ok
      in
      let current =
        Agent_client.Session_handle.projection handle |> Agent_client.Projection.snapshot
      in
      let reset expected =
        Agent_client.Session_handle.reset
          handle
          ~expected_revision:expected
          ~keep_history:false
          ~keep_tasks:false
          ~keep_cache:false
          ~keep_workspace:true
          ~keep_grants:false
          ~keep_labels:true
      in
      let stale_rejected = Result.is_error (reset Int64.(current.revision - 1L)) in
      let reset_session = reset current.revision |> protocol_ok in
      let rebuilt =
        Agent_client.Session_handle.rebuild
          handle
          ~expected_revision:reset_session.revision
          ~prompt_choice:Pinned
        |> protocol_ok
      in
      let started =
        Agent_client.Session_handle.start handle ~queue_if_limited:true |> protocol_ok
      in
      let stopped = Agent_client.Session_handle.stop handle ~mode:Cancel |> protocol_ok in
      Agent_client.Session_handle.detach handle |> protocol_ok;
      Agent_client.Connection.close connection;
      Agent_server.Embedded.close embedded;
      print_s
        [%sexp
          { listed = (List.length sessions : int)
          ; stale_rejected : bool
          ; reset_generation = (reset_session.generation : int)
          ; reset_stopped = (observed_stopped reset_session.observed_state : bool)
          ; rebuild_advanced_revision =
              (Int64.(rebuilt.revision > reset_session.revision) : bool)
          ; rebuild_stopped = (observed_stopped rebuilt.observed_state : bool)
          ; start_requested =
              (Agent_protocol.Session.equal_desired_state started.desired_state Running
               : bool)
          ; stop_requested =
              (Agent_protocol.Session.equal_desired_state stopped.desired_state Stopped
               : bool)
          }]));
  [%expect
    {|
    ((listed 1) (stale_rejected true) (reset_generation 1) (reset_stopped true)
     (rebuild_advanced_revision true) (rebuild_stopped true)
     (start_requested true) (stop_requested true))
    |}]
;;

let%expect_test "session export returns a durable server-owned blob" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Switch.run (fun sw ->
      let data_root = Filename.concat root "data" in
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = Some data_root
          ; start_immediately = true
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let connection = Agent_server.Embedded.connection embedded in
      let session_id = Agent_server.Embedded.session_id embedded in
      let attachment_id = (Agent_server.Embedded.attachment embedded).id in
      let response =
        Agent_client.Connection.request
          connection
          (Session_export
             { session_id; attachment_id; format = Json; revision = None; history = None })
        |> protocol_ok
      in
      let export =
        match response with
        | Agent_protocol.Method_result.Session_export export -> export
        | _ -> failwith "unexpected export response"
      in
      let blob_path =
        Filename.concat
          data_root
          (Filename.concat
             "sessions"
             (Filename.concat
                (Agent_protocol.Id.Session.to_string session_id)
                (Filename.concat
                   "blobs"
                   (Agent_protocol.Id.Blob.to_string export.blob.id ^ ".blob"))))
      in
      let contents = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / blob_path) in
      let downloaded = Buffer.create (String.length contents) in
      Agent_client.Blob_download.download
        ~connection
        ~session_id
        ~attachment_id
        ~blob:export.blob
        ~output:(Eio.Flow.buffer_sink downloaded)
      |> protocol_ok;
      let foreign_attachment_rejected =
        Agent_client.Connection.request
          connection
          (Blob_read
             { session_id
             ; attachment_id = Agent_protocol.Id.Attachment.create ()
             ; blob_id = export.blob.id
             ; offset = 0L
             ; max_bytes = 16
             })
        |> Result.is_error
      in
      let valid_json =
        Result.try_with (fun () -> Jsonaf.of_string contents) |> Result.is_ok
      in
      Agent_server.Embedded.close embedded;
      print_s
        [%sexp
          { durable_blob_exists =
              (Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / blob_path) : bool)
          ; media_type = (export.blob.media_type : string)
          ; valid_json : bool
          ; download_matches = (String.equal contents (Buffer.contents downloaded) : bool)
          ; foreign_attachment_rejected : bool
          }]));
  [%expect
    {|
    ((durable_blob_exists true) (media_type application/json) (valid_json true)
     (download_matches true) (foreign_attachment_rejected true))
    |}]
;;

let%expect_test "stopped sessions require exact confirmation and can be removed" =
  with_fixture (fun env root workspace prompt_file ->
    Eio.Switch.run (fun sw ->
      let data_root = Filename.concat root "delete-data" in
      let options =
        Agent_server.Embedded.
          { prompt_file
          ; workspace
          ; tool_dir = workspace
          ; home = root
          ; data_root = Some data_root
          ; start_immediately = true
          ; permission_profile = Agent_server.Embedded.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 128
          }
      in
      let embedded = Agent_server.Embedded.start ~sw ~env options |> protocol_ok in
      let connection = Agent_server.Embedded.connect embedded in
      let session_id = Agent_server.Embedded.session_id embedded in
      Agent_client.Session_handle.initialize
        connection
        ~implementation_name:"delete-test"
        ~implementation_version:"dev"
      |> protocol_ok
      |> ignore;
      let handle =
        Agent_client.Session_handle.attach
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~connection
          ~session_id
          ~mode:Read_write
          ~subscribe:false
          ()
        |> protocol_ok
      in
      let stopped = Agent_client.Session_handle.stop handle ~mode:Cancel |> protocol_ok in
      let delete confirmation =
        Agent_client.Session_handle.delete
          handle
          ~expected_revision:stopped.revision
          ~policy:Remove
          ~confirmation
      in
      let wrong_confirmation_rejected = Result.is_error (delete "wrong") in
      let receipt =
        delete (Agent_protocol.Id.Session.to_string session_id) |> protocol_ok
      in
      let session_path =
        Filename.concat
          data_root
          (Filename.concat "sessions" (Agent_protocol.Id.Session.to_string session_id))
      in
      let removed =
        not (Eio.Path.is_directory Eio.Path.(Eio.Stdenv.fs env / session_path))
      in
      Agent_client.Session_handle.close handle;
      Agent_client.Connection.close connection;
      Agent_server.Embedded.close embedded;
      print_s
        [%sexp
          { wrong_confirmation_rejected : bool
          ; receipt_matches =
              (Agent_protocol.Id.Session.compare receipt.session_id session_id = 0 : bool)
          ; removed : bool
          }]));
  [%expect
    {|
    ((wrong_confirmation_rejected true) (receipt_matches true) (removed true))
    |}]
;;
