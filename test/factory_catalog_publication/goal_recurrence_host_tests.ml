open! Core
open Agent_server_test_support
open Retry_support
module Support = Goal_recurrence_host_support
module Native = Goal_native_start_tests
module G = P.Goal
module Origin = A.Recurrence_run_origin
module Terminal = A.Recurrence_run_reference
module Ledger = A.Inference_ledger
module Wake = P.Run_wake
module Native_driver = Agent_server.Goal_native_resume_dispatcher
module Snapshot = Session.Snapshot
module Client = Agent_client
module Handle = Client.Session_handle
module Lifecycle = P.Session_lifecycle
module Query = P.Recurrence_query
module Recurrences = Client.Recurrence_controls

let checkpoint (current : A.Session_state.t) =
  let snapshot =
    A.Moderator_checkpoint.decode current.moderator |> protocol_ok |> Option.value_exn
  in
  snapshot.current_state
;;

let snapshot_field value key =
  match value with
  | Snapshot.Record fields -> List.Assoc.find_exn fields key ~equal:String.equal
  | _ -> failwith ("Expected a checkpoint record for " ^ key)
;;

let assert_snapshot expected actual =
  assert (Int.equal (Snapshot.compare expected actual) 0)
;;

let assert_checkpoint_order snapshot =
  assert_snapshot (Snapshot.Int 2) (snapshot_field snapshot "schema");
  assert_snapshot
    (Snapshot.Array [ String "instructions"; String "goal"; String "recurrence" ])
    (snapshot_field snapshot "order")
;;

let assert_instruction_checkpoint snapshot =
  let instruction =
    match snapshot_field snapshot "instructions" with
    | Snapshot.Variant ("Observed", [ fields ]) -> fields
    | _ -> failwith "Expected an independently observed instruction component"
  in
  assert_snapshot (Snapshot.String "2") (snapshot_field instruction "revision");
  assert_snapshot (Snapshot.Bool true) (snapshot_field instruction "project_selected");
  assert_snapshot (Snapshot.Float 1.) (snapshot_field instruction "project_files");
  assert_snapshot (Snapshot.Float 1.) (snapshot_field instruction "skills")
;;

let assert_wait_checkpoint current wake schedule ~remaining_checks =
  let snapshot = checkpoint current in
  assert_checkpoint_order snapshot;
  assert_instruction_checkpoint snapshot;
  let waiting =
    Snapshot.Record
      [ "remaining_checks", Int remaining_checks
      ; "run_id", String (P.Id.Run.to_string wake.Wake.run_id)
      ; "schedule_id", String (P.Id.Schedule.to_string schedule.P.Schedule.id)
      ]
  in
  assert_snapshot
    (Snapshot.Variant ("Waiting", [ waiting ]))
    (snapshot_field snapshot "goal");
  assert_snapshot
    (Snapshot.Variant ("Enabled", [ String "0" ]))
    (snapshot_field snapshot "recurrence");
  snapshot
;;

let assert_finished_checkpoint current (admission : P.Run_receipt.t) first second =
  let snapshot = checkpoint current in
  assert_checkpoint_order snapshot;
  assert_instruction_checkpoint snapshot;
  assert_snapshot
    (Snapshot.Variant ("Finish_staged", [ String (P.Id.Run.to_string admission.run_id) ]))
    (snapshot_field snapshot "goal");
  let review slot origin =
    Snapshot.Record
      [ "run_id", String (P.Id.Run.to_string (Origin.run_id origin))
      ; ( "session_id"
        , String
            (P.Session_ref.session_id (Origin.session origin) |> P.Id.Session.to_string) )
      ; "slot", String slot
      ]
  in
  let reviews =
    Snapshot.Record [ "first", review "0" first; "second", review "1" second ]
  in
  assert_snapshot
    (Snapshot.Variant ("Reviewed", [ reviews ]))
    (snapshot_field snapshot "recurrence")
;;

let waiting ?since ~clock parent admission =
  Support.await
    ~clock
    ~phase:"joining the controller turn and committing its Wait"
    ~on_timeout:(fun () ->
      let current = Support.healthy parent in
      [ [%sexp (Support.run current admission.P.Run_receipt.run_id : P.Run.t)]
      ; [%sexp (current.schedules : P.Schedule.t list)]
      ])
    (fun () ->
       let current = Support.healthy parent in
       match (Support.run current admission.P.Run_receipt.run_id).lifecycle with
       | Waiting wake
         when Option.is_none current.active_operation
              && not (Option.exists since ~f:(Wake.equal wake)) -> Some (current, wake)
       | Admitted | Active | Waiting _ -> None
       | Terminal _ -> failwith "Controller Run ended before its review wait")
;;

let timer (current : A.Session_state.t) (wake : Wake.t) =
  match wake.occurrence with
  | Delivered_timer { schedule_id; delivery_count; creator; subscription } ->
    let schedule =
      List.find_exn current.schedules ~f:(fun schedule ->
        P.Id.Schedule.equal schedule_id schedule.id)
    in
    let ownership = Option.value_exn schedule.ownership in
    [%test_eq: int] 1 delivery_count;
    assert (P.Job.equal_launch_owner creator ownership.creator);
    assert (Option.is_none subscription);
    assert (P.Moderator_observer.equal wake.source.observer ownership.source);
    schedule
  | Job_completion _ | Subscription_delivery _ ->
    failwith "Wrong producer for review wait"
;;

let root_receipts current (admission : P.Run_receipt.t) kind =
  A.Run_state.receipts (Option.value_exn current.A.Session_state.run_state)
  |> List.filter ~f:(fun receipt ->
    P.Run_receipt.Kind.equal receipt.kind kind
    && P.Id.Run.equal receipt.run_id admission.run_id)
;;

let review_reads current =
  List.filter current.A.Session_state.invocations ~f:(fun invocation ->
    String.equal invocation.P.Invocation.context.tool_name "recurrence_history")
;;

let history_attempts result =
  Jsonaf.member_exn "occurrences" result
  |> Jsonaf.list_exn
  |> List.map ~f:(fun occurrence ->
    Jsonaf.member_exn "attempts" occurrence
    |> Jsonaf.list_exn
    |> Support.one "history attempt")
;;

let assert_visible_outcomes result origins =
  let member = Jsonaf.member_exn in
  let actual =
    List.map (history_attempts result) ~f:(fun attempt ->
      let target = member "target" attempt in
      [%test_eq: string] "visible" (member "visibility" target |> Jsonaf.string_exn);
      [%test_eq: string]
        "completed"
        (member "kind" (member "retained_outcome" target) |> Jsonaf.string_exn);
      member "run_id" target |> P.Id.Run.of_json |> protocol_ok)
  in
  [%test_eq: P.Id.Run.t list]
    (List.map origins ~f:Origin.run_id |> List.sort ~compare:P.Id.Run.compare)
    (List.sort ~compare:P.Id.Run.compare actual)
;;

let assert_review_read (invocation : P.Invocation.t) origins =
  let value =
    match invocation.status with
    | Resolved (Complete value) | Published (Complete value) -> value
    | _ -> failwith "Authored recurrence history read did not succeed"
  in
  assert_visible_outcomes (Jsonaf.member_exn "result" value) origins
;;

let recurrence_view = function
  | Query.Result.Available view -> view
  | Unavailable _ -> failwith "Operator did not see the composed recurrence controller"
;;

let typed_history_attempts occurrences =
  List.map occurrences ~f:(fun (occurrence : Query.Occurrence.t) ->
    Support.one "history attempt" occurrence.attempts)
;;

let assert_operator_outcomes occurrences origins =
  let actual =
    List.map (typed_history_attempts occurrences) ~f:(fun (attempt : Query.Attempt.t) ->
      assert attempt.admission_recorded;
      assert attempt.terminal_recorded;
      match attempt.target with
      | Visible
          { run =
              Some
                { id; status = Terminal Completed; retained_outcome = Some Completed; _ }
          ; _
          } -> id
      | _ -> failwith "Operator did not see the exact retained Worker outcome")
  in
  [%test_eq: P.Id.Run.t list]
    (List.map origins ~f:Origin.run_id |> List.sort ~compare:P.Id.Run.compare)
    (List.sort ~compare:P.Id.Run.compare actual)
;;

let assert_operator_schedule daemon ~session ~first_due ~next_due origin =
  Support.with_operator daemon (fun connection ->
    let controls = Recurrences.create connection ~session () in
    let listed = Recurrences.list controls |> protocol_ok |> recurrence_view in
    assert (P.Session_ref.equal listed.basis.session session);
    let entry = Support.one "authored recurrence" listed.value in
    assert (P.Id.Recurrence.equal Support.recurrence_id entry.id);
    (match entry.schedule with
     | Calendar _ -> failwith "Operator did not see the authored interval"
     | Interval { rule; first_due = observed_due } ->
       let module Rule = Recurrence_rules.Rule in
       assert (Rule.Interval.equal_mode Fixed_delay (Rule.Interval.mode rule));
       [%test_eq: int64] 60_000L (Rule.Period.milliseconds (Rule.Interval.period rule));
       assert (Time_ns.equal (P.Timestamp.to_time_ns first_due) observed_due));
    let get = Recurrences.get controls entry.id |> protocol_ok |> recurrence_view in
    assert (P.Id.Recurrence.equal get.value.id entry.id);
    [%test_eq: int64] entry.definition_revision get.value.definition_revision;
    let status = Recurrences.status controls entry.id |> protocol_ok |> recurrence_view in
    assert (P.Id.Recurrence.equal status.value.entry.id entry.id);
    [%test_eq: int64] entry.control_revision status.value.entry.control_revision;
    assert (
      match status.value.entry.control, status.value.planning with
      | Enabled, Available -> true
      | _ -> false);
    let due = Option.value_exn status.value.alarm_due_at in
    assert (Time_ns.equal next_due (P.Timestamp.to_time_ns due));
    assert_operator_outcomes status.value.occurrences [ origin ])
;;

let assert_operator_history daemon ~session origins =
  let restrict scopes =
    let scopes = Set.inter principal.scopes (P.Scope.Set.of_list scopes) in
    { principal with scopes }
  in
  let history connection =
    let controls = Recurrences.create connection ~session () in
    (Recurrences.history controls Support.recurrence_id |> protocol_ok |> recurrence_view)
      .value
      .occurrences
  in
  Support.with_operator daemon (fun connection ->
    assert_operator_outcomes (history connection) origins);
  Support.with_operator
    daemon
    ~principal:(restrict [ View_session_transcript; Send_messages ])
    (fun connection ->
       let attempts = typed_history_attempts (history connection) in
       [%test_eq: int] (List.length origins) (List.length attempts);
       List.iter attempts ~f:(fun (attempt : Query.Attempt.t) ->
         assert attempt.admission_recorded;
         assert attempt.terminal_recorded;
         assert (
           match attempt.target with
           | Denied -> true
           | _ -> false)));
  Support.with_operator daemon ~principal:(restrict [ Send_messages ]) (fun connection ->
    let controls = Recurrences.create connection ~session () in
    let denied = Recurrences.list controls |> rejected in
    [%test_eq: P.Error.code] Permission_denied denied.code)
;;

let assert_delivered current wake =
  let delivery = timer current wake in
  [%test_eq: int] 1 delivery.delivery_count;
  match delivery.status with
  | Delivered -> ()
  | Scheduled | Delivering | Cancelled | Failed _ ->
    failwith "Review timer did not deliver"
;;

let assert_review_callback (current : A.Session_state.t) (invocation : P.Invocation.t) =
  let event = Option.value_exn invocation.parent_event in
  let callback =
    List.find_exn current.moderator_executions ~f:(fun execution ->
      P.Id.Moderator_execution.equal event execution.context.id)
  in
  assert (P.Moderator_execution.equal_phase Internal_event callback.context.phase);
  match callback.status with
  | Completed _ -> ()
  | Running | Failed _ | Interrupted _ -> failwith "Review timer callback did not join"
;;

let assert_achieved current admission episode first second =
  let achievement =
    match G.state (Support.goal current) with
    | Closed (Achieved value) -> value
    | Open _ | Closed (Abandoned _) -> failwith "Root terminal did not achieve its Goal"
  in
  let terminal =
    Goal_finish_tests.terminal_receipt
      (Option.value_exn current.A.Session_state.run_state)
      admission
  in
  assert (P.Run_receipt.equal terminal (P.Goal_achievement.receipt achievement));
  assert (
    P.Goal_episode.equal
      episode
      (P.Goal_episode.Settlement.episode (P.Goal_achievement.settlement achievement)));
  let request = P.Goal_achievement.request achievement in
  assert (
    P.Run_action.equal
      Goal_finish_tests.finish_action
      (P.Goal_achievement.Request.action request));
  let evidence = P.Goal_achievement.Request.evidence request |> G.Reason.to_string in
  List.iter [ first; second ] ~f:(fun origin ->
    assert (
      String.is_substring evidence ~substring:(P.Id.Run.to_string (Origin.run_id origin))))
;;

let assert_replay ~clock daemon parent wake =
  Support.await ~clock ~phase:"rejecting the previously consumed review wake" (fun () ->
    match Factory.deliver_goal_wake (Daemon.factory daemon) parent ~wake with
    | Ok No_work -> Some ()
    | Error { P.Error.code = Conflict; retryable = true; _ } -> None
    | Error error -> raise_s [%sexp (error : P.Error.t)]
    | Ok (Handed_off _ | Controlled _ | Suspended_goal _) ->
      failwith "Review wake replayed")
;;

type review_wait =
  { admission : P.Run_receipt.t
  ; wake : Wake.t
  ; initial_schedule : P.Schedule.t
  ; episode : P.Goal_episode.t
  ; snapshot : Snapshot.t
  }

let compaction_output = "The current release-note handoff names a reviewer."

let with_waiting_controller ?resume_input f =
  let providers = ref 0 in
  let post_stream ~sw:_ ~inputs =
    Int.incr providers;
    let transcript =
      `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
    in
    let marker, response =
      match !providers, resume_input with
      | 1, _ | 3, Some _ ->
        Support.Guidance.assert_provider_inputs inputs;
        List.iter
          [ "# Persistent goals"
          ; "# Recurring work"
          ; "# Composed lifecycle decisions"
          ; "# Admitted skills"
          ; "# Project guidance"
          ]
          ~f:(fun heading -> assert (String.is_substring transcript ~substring:heading));
        ( (if Int.equal !providers 1
           then Support.input_text
           else Option.value_exn resume_input)
        , "The handoff includes a named review owner." )
      | 2, Some _ ->
        Support.Guidance.assert_provider_inputs inputs;
        Support.input_text, compaction_output
      | (2 | 3), None ->
        ( "Review the release-note handoff for a missing owner"
        , "The handoff includes a named review owner." )
      | _ -> failwith "Review workflow dispatched an extra provider turn"
    in
    assert (String.is_substring transcript ~substring:marker);
    completed_response response
  in
  let policy = guidance_inference_policy ~default_model:"fixture-model" ~post_stream () in
  Native.with_accepted_start
    ~startup:Support.startup
    ~configure:Support.configure_guidance
    ~timeout_seconds:120.
    ~v1:true
    ~policy
    ~providers
    (fun env sw root daemon parent started ->
       Support.Guidance.select env sw daemon parent;
       let clock = Eio.Stdenv.clock env in
       let native_driver = Native_driver.create ~factory:(Daemon.factory daemon) in
       protect_cleanup
         ~finally:(fun () -> Native_driver.close native_driver)
         (fun () ->
            Support.with_driver env sw daemon parent (fun driver ->
              Native_driver.process_entry native_driver parent |> protocol_ok;
              let admitted =
                Support.await ~clock ~phase:"admitting the native Goal Start" (fun () ->
                  let current = Support.healthy parent in
                  let _, intent = Native.start_invocation current in
                  match P.Goal_native_start_intent.phase intent with
                  | Admitted value -> Some value.admission
                  | Staged | Accepted _ | Dispatch_claimed _ -> None
                  | Discarded _ | Rejected _ -> failwith "Native Goal Start was rejected")
              in
              let initial, wake = waiting ~clock parent admitted in
              Native.assert_admission_ack
                ~goal_id:Support.goal_id
                env
                parent
                started.invocation
                admitted;
              let episode = G.episode (Support.goal initial) |> Option.value_exn in
              assert (P.Run_receipt.equal admitted (P.Goal_episode.admission episode));
              assert (P.Id.Run.equal wake.run_id admitted.run_id);
              assert (P.Run_source.equal wake.source admitted.source);
              assert (G.State.equal (G.state (Support.goal initial)) (Open Enabled));
              initial.schedules |> Support.one "original review timer" |> ignore;
              let scheduled = timer initial wake in
              [%test_eq: int] 0 scheduled.delivery_count;
              let at_wait =
                assert_wait_checkpoint initial wake scheduled ~remaining_checks:3
              in
              root_receipts initial admitted Action
              |> Support.one "review Wait action"
              |> ignore;
              [%test_eq: int] 0 (List.length (root_receipts initial admitted Terminal));
              Support.assert_capture initial ~session:started.session ~episode;
              Support.assert_totals initial ~session:started.session ~children:0;
              [%test_eq: int] 1 !providers;
              let waiting =
                { admission = admitted
                ; wake
                ; initial_schedule = scheduled
                ; episode
                ; snapshot = at_wait
                }
              in
              f
                ~env
                ~sw
                ~clock
                ~root
                ~daemon
                ~parent
                ~started
                ~driver
                ~providers
                ~policy
                waiting)))
;;

let assert_operator_authored_capture env sw daemon (parent : Registry.entry) ~session =
  let module Control = P.Recurrence_control in
  let id = P.Id.Recurrence.of_string "rec_operator_authored_review" |> protocol_ok in
  let session_id = P.Session_ref.session_id session in
  let command_key =
    P.Idempotency_key.of_string "operator-authored-review" |> protocol_ok
  in
  Support.with_operator daemon (fun connection ->
    Support.with_writer env sw connection ~session_id (fun handle ->
      let controls = Handle.recurrences handle |> protocol_ok in
      let current =
        Recurrences.get controls Support.recurrence_id |> protocol_ok |> recurrence_view
      in
      let capture =
        Control.Capture.create
          ~schedule:current.value.schedule
          ~policy:current.value.policy
          ~selector:(Authored_child { name = "review_worker"; mode = Workflow })
          ()
        |> protocol_ok
      in
      let basis : Control.Source_basis.t =
        { source = Option.value_exn current.basis.source
        ; expected_lifecycle_revision = current.basis.lifecycle_revision
        }
      in
      let intent =
        Control.Intent.create
          ~generation:current.basis.generation
          ~expected_catalog_revision:current.basis.catalog_revision
          ~key:command_key
          (Create { id; basis; capture })
        |> protocol_ok
      in
      let accepted = Recurrences.command controls intent |> protocol_ok in
      let current = state parent in
      let catalog = Option.value_exn current.recurrence_catalog in
      let entry = Catalog.find catalog id |> Option.value_exn in
      assert (Entry.equal_control (Entry.control entry) (Paused Operator_pause));
      assert (List.is_empty (Frontier.ready (Entry.frontier entry |> protocol_ok)));
      (match
         A.Recurrence_target.view (A.Recurrence_definition.target (Entry.current entry))
       with
       | Fresh_child { source_kind = Authored_tool authored; mode = Some Workflow; _ } ->
         [%test_eq: string] "review_worker" authored.name
       | _ -> failwith "shared Create did not freeze the actual authored Worker");
      let frames = Existing.frames env parent in
      let replayed = Recurrences.command controls intent |> protocol_ok in
      assert (
        Jsonaf.exactly_equal
          (Control.Result.payload accepted)
          (Control.Result.payload replayed));
      [%test_eq: string list] frames (Existing.frames env parent)))
;;

let check_resident
      ~env
      ~sw
      ~clock
      ~root:_
      ~daemon
      ~(parent : Registry.entry)
      ~(started : Native.accepted_start)
      ~driver
      ~providers
      ~policy:_
      { admission = admitted
      ; wake = initial_wake
      ; initial_schedule
      ; episode
      ; snapshot = initial_checkpoint
      }
  =
  let tick () = started.advance_scheduler 0.1 in
  (* Give the fixed-delay successor a nominal after the first review while
     keeping the first occurrence within its captured grace. *)
  Support.advance_schedule_and_join ~clock daemon started 35.1;
  let first, first_child =
    Support.join_occurrence ~clock ~tick daemon driver parent ~slot:0L
  in
  let first_committed =
    (Terminal.receipt first.terminal).committed_at |> P.Timestamp.to_time_ns
  in
  let expected_second_due = Time_ns.add first_committed (Time_ns.Span.of_sec 60.) in
  let after_first = Support.healthy parent in
  assert_snapshot initial_checkpoint (checkpoint after_first);
  [%test_eq: int] 0 (timer after_first initial_wake).delivery_count;
  Support.assert_totals after_first ~session:started.session ~children:1;
  [%test_eq: int] 2 !providers;
  assert (Option.is_none (Support.execution after_first ~slot:1L));
  assert_operator_schedule
    daemon
    ~session:started.session
    ~first_due:admitted.committed_at
    ~next_due:expected_second_due
    first.origin;
  started.advance_scheduler 55.1;
  let _, wake = waiting ~since:initial_wake ~clock parent admitted in
  let review =
    Support.await ~clock ~phase:"joining the authored review observations" (fun () ->
      Some (Goal_resume_runtime_tests.settled_snapshot parent))
  in
  let goal = Support.goal review in
  assert (G.State.equal (Open Enabled) (G.state goal));
  assert (Option.equal P.Goal_episode.equal (Some episode) (G.episode goal));
  assert (P.Run_source.equal wake.source initial_wake.source);
  assert_delivered review initial_wake;
  let first_review = timer review initial_wake in
  assert (P.Id.Schedule.equal initial_schedule.id first_review.id);
  let delivered_at = first_review.last_delivery_at |> Option.value_exn in
  assert (Time_ns.( < ) (P.Timestamp.to_time_ns delivered_at) expected_second_due);
  assert (Option.is_none (Support.execution review ~slot:1L));
  let first_read = review_reads review |> Support.one "pending review history read" in
  assert_review_read first_read [ first.origin ];
  assert_review_callback review first_read;
  let scheduled = timer review wake in
  let at_wait = assert_wait_checkpoint review wake scheduled ~remaining_checks:2 in
  [%test_eq: int] 2 !providers;
  [%test_eq: int] 2 (List.length review.schedules);
  [%test_eq: int] 0 scheduled.delivery_count;
  [%test_eq: int] 2 (List.length (root_receipts review admitted Action));
  [%test_eq: int] 0 (List.length (root_receipts review admitted Terminal));
  Support.advance_schedule_and_join ~clock daemon started 6.1;
  let second, second_child =
    Support.join_occurrence ~clock ~tick daemon driver parent ~slot:1L
  in
  let after_second = Support.healthy parent in
  let second_execution = Support.execution after_second ~slot:1L |> Option.value_exn in
  let second_due =
    A.Recurrence_nominal.at (Intent.nominal (Execution.intent second_execution))
  in
  assert (Time_ns.equal second_due expected_second_due);
  assert (
    not (P.Session_ref.equal (Origin.session first.origin) (Origin.session second.origin)));
  assert (not (P.Id.Run.equal (Origin.run_id first.origin) (Origin.run_id second.origin)));
  let first_handle =
    Support.assert_descendant
      daemon
      after_second
      ~session:started.session
      ~episode
      first
      first_child
  in
  let second_handle =
    Support.assert_descendant
      daemon
      after_second
      ~session:started.session
      ~episode
      second
      second_child
  in
  assert (not (Ledger.Handle.equal first_handle second_handle));
  Support.assert_totals after_second ~session:started.session ~children:2;
  assert (G.State.equal (G.state (Support.goal after_second)) (Open Enabled));
  assert (
    Wake.equal
      wake
      (match (Support.run after_second admitted.run_id).lifecycle with
       | Waiting retained -> retained
       | Admitted | Active | Terminal _ ->
         failwith "Review wait did not survive both children"));
  let before_wake = timer after_second wake in
  [%test_eq: int] 0 before_wake.delivery_count;
  assert (P.Id.Schedule.equal scheduled.id before_wake.id);
  assert_snapshot at_wait (checkpoint after_second);
  [%test_eq: int] 3 !providers;
  started.advance_scheduler 30.1;
  let settle_ticks = ref 10 in
  let describe current =
    [ [%sexp (Support.run current admitted.run_id : P.Run.t)]
    ; [%sexp (G.state (Support.goal current) : G.State.t)]
    ; [%sexp (current.A.Session_state.schedules : P.Schedule.t list)]
    ; [%sexp
        (List.map current.moderator_executions ~f:(fun execution ->
           execution.P.Moderator_execution.context.phase, execution.status)
         : (P.Moderator_execution.phase * P.Moderator_execution.status) list)]
    ; [%sexp
        (List.map current.invocations ~f:(fun invocation ->
           invocation.P.Invocation.context.tool_name)
         : string list)]
    ]
  in
  let final =
    Support.await
      ~clock
      ~phase:"settling the timer wake and root Goal"
      ~on_timeout:(fun () -> describe (Support.healthy parent))
      (fun () ->
         let current = Support.healthy parent in
         let run = Support.run current admitted.run_id in
         match run.lifecycle with
         | Terminal (Completed _) when Option.is_none current.active_operation ->
           Some (Goal_resume_runtime_tests.settled_snapshot parent)
         | Terminal (Completed _) | Admitted | Active | Waiting _ ->
           if !settle_ticks > 0
           then (
             Int.decr settle_ticks;
             tick ());
           None
         | Terminal _ ->
           raise_s
             [%sexp "Review root did not complete", (describe current : Sexp.t list)])
  in
  assert_delivered final wake;
  assert_delivered final initial_wake;
  [%test_eq: int] 2 (List.length final.schedules);
  let reads = review_reads final in
  [%test_eq: int] 2 (List.length reads);
  let read =
    List.filter reads ~f:(fun read ->
      not (P.Id.Invocation.equal first_read.context.id read.context.id))
    |> Support.one "terminal review history read"
  in
  assert_review_read read [ first.origin; second.origin ];
  assert_review_callback final read;
  assert (
    not
      (Option.equal
         P.Id.Moderator_execution.equal
         first_read.parent_event
         read.parent_event));
  [%test_eq: int] 3 (List.length (root_receipts final admitted Action));
  root_receipts final admitted Terminal |> Support.one "root Terminal receipt" |> ignore;
  assert_achieved final admitted episode first.origin second.origin;
  assert_finished_checkpoint final admitted first.origin second.origin;
  Support.assert_totals final ~session:started.session ~children:2;
  assert_operator_history daemon ~session:started.session [ first.origin; second.origin ];
  assert_replay ~clock daemon parent initial_wake;
  assert_replay ~clock daemon parent wake;
  [%test_eq: int] 3 !providers;
  let after_replay = Support.healthy parent in
  let catalog = Option.value_exn after_replay.recurrence_catalog in
  [%test_eq: int] 2 (List.length (Catalog.receipts catalog));
  [%test_eq: int] 2 (List.length (Frontier.executions (Support.frontier after_replay)));
  assert_operator_authored_capture env sw daemon parent ~session:started.session;
  Support.assert_totals (Support.healthy parent) ~session:started.session ~children:2;
  [%test_eq: int] 3 !providers
;;

let check_retired
      ~env
      ~sw
      ~clock
      ~root:_
      ~daemon
      ~(parent : Registry.entry)
      ~(started : Native.accepted_start)
      ~driver
      ~providers
      ~policy:_
      { admission; wake; initial_schedule; episode; snapshot }
  =
  let session_id = P.Session_ref.session_id started.session in
  let definition current =
    A.Recurrence_definition.digest (Entry.current (Support.recurrence current))
  in
  let read_control current = Entry.control (Support.recurrence current) in
  let ready_slot current =
    let ready =
      Frontier.ready (Support.frontier current) |> Support.one "eligible captured slot"
    in
    [%test_eq: int64]
      0L
      (A.Recurrence_identity.Occurrence.slot (A.Recurrence_nominal.key ready));
    A.Recurrence_nominal.key ready
  in
  let ready =
    Support.with_operator daemon (fun connection ->
      Support.with_writer env sw connection ~session_id (fun handle ->
        Actor.sweep_recurrences parent.actor |> protocol_ok;
        let attached = Support.healthy parent in
        let ready = ready_slot attached in
        Handle.detach handle |> protocol_ok;
        let detached = Support.healthy parent in
        assert (A.Recurrence_identity.Occurrence.equal ready (ready_slot detached));
        assert (Entry.equal_control (read_control detached) Enabled);
        assert (List.is_empty detached.attachments);
        assert (Owner.is_loaded parent.runtime);
        [%test_eq: int] 0 (timer detached wake).delivery_count;
        Support.assert_no_child daemon detached;
        ready))
  in
  Owner.unload_and_wait parent.runtime |> protocol_ok;
  assert (not (Owner.is_loaded parent.runtime));
  let cold = Support.healthy parent in
  assert_snapshot snapshot (checkpoint cold);
  assert (G.State.equal (Open Enabled) (G.state (Support.goal cold)));
  (match (Support.run cold admission.run_id).lifecycle with
   | Waiting retained -> assert (Wake.equal wake retained)
   | Admitted | Active | Terminal _ ->
     failwith "Runtime retirement erased the retained Wait");
  started.advance_scheduler 90.1;
  let current =
    Support.await ~clock ~phase:"fencing the retired native Goal wake" (fun () ->
      let current = Support.healthy parent in
      match (Support.run current admission.run_id).lifecycle with
      | Terminal Interrupted -> Some current
      | Admitted | Active | Waiting _ -> None
      | Terminal _ -> failwith "Retired native Goal produced an unexpected terminal")
  in
  let goal = Support.goal current in
  assert (G.State.equal (Open (Held Source_review)) (G.state goal));
  assert (Option.equal P.Goal_episode.equal (G.episode goal) (Some episode));
  assert_snapshot snapshot (checkpoint current);
  let scheduled = timer current wake in
  assert (P.Id.Schedule.equal scheduled.id initial_schedule.id);
  [%test_eq: int] 0 scheduled.delivery_count;
  (match scheduled.status with
   | Scheduled -> ()
   | Delivering | Delivered | Cancelled | Failed _ ->
     failwith "Retired Goal timer was claimed");
  assert (
    not
      (List.exists current.moderator_executions ~f:(fun execution ->
         P.Moderator_execution.equal_phase Internal_event execution.context.phase)));
  root_receipts current admission Action |> Support.one "original Wait action" |> ignore;
  let retained =
    root_receipts current admission Terminal |> Support.one "interrupted Run terminal"
  in
  Support.assert_totals current ~session:started.session ~children:0;
  assert (A.Recurrence_identity.Occurrence.equal ready (ready_slot current));
  assert (Entry.equal_control (read_control current) Enabled);
  let original_definition = definition current in
  let assert_retained owner_state =
    [%test_eq: string] original_definition (definition owner_state);
    let terminal =
      root_receipts owner_state admission Terminal
      |> Support.one "retained interrupted Run"
    in
    assert (P.Run_receipt.equal retained terminal);
    let timer = timer owner_state wake in
    [%test_eq: int] 0 timer.delivery_count;
    (match timer.status with
     | Cancelled -> ()
     | Scheduled | Delivering | Delivered | Failed _ ->
       failwith "Stop did not cancel the owned one-shot timer");
    Support.assert_no_child daemon owner_state;
    [%test_eq: int] 1 !providers
  in
  Support.with_operator daemon (fun connection ->
    Support.with_writer env sw connection ~session_id (fun handle ->
      let before_stop = Support.healthy parent in
      let delete expected_revision =
        Handle.delete
          handle
          ~expected_revision
          ~policy:Archive
          ~confirmation:(P.Id.Session.to_string session_id)
      in
      assert (A.Recurrence_identity.Occurrence.equal ready (ready_slot before_stop));
      let refusal = delete before_stop.counters.revision |> rejected in
      [%test_eq: P.Error.code] Invalid_state refusal.code;
      [%test_eq: string] "session must be stopped before deletion" refusal.message;
      let stopped = Handle.stop handle ~mode:Cancel |> protocol_ok in
      assert (P.Session.equal_desired_state stopped.desired_state Stopped);
      Actor.sweep_recurrences parent.actor |> protocol_ok;
      Support.Driver.process_entry driver parent |> protocol_ok;
      let stopped = Support.healthy parent in
      assert (Entry.equal_control (read_control stopped) (Paused Controller_stop));
      assert (List.is_empty (Frontier.ready (Support.frontier stopped)));
      assert_retained stopped;
      let deleted = delete stopped.counters.revision |> protocol_ok in
      assert (P.Id.Session.equal deleted.session_id session_id));
    assert (Option.is_none (Registry.find (Daemon.registry daemon) session_id));
    let archived = Support.lifecycle connection session_id in
    assert (Lifecycle.Result.Status.equal (Lifecycle.Observation.status archived) Archived);
    let stored =
      Factory.read_recurrence_state
        (Daemon.factory daemon)
        ~session:started.session
        ~actor:original
        ~authorize:(fun summary ->
          assert (P.Id.Session.equal summary.id session_id);
          Ok ())
      |> protocol_ok
    in
    assert_retained stored;
    let retired =
      Entry.effective_control
        (Support.recurrence stored)
        ~current_revision:
          (Lifecycle.Observation.expected archived
           |> Lifecycle.Expected.lifecycle_revision)
        ~archived:true
      |> protocol_ok
    in
    assert (Entry.equal_control retired (Retired Lifecycle_changed));
    let registry = Daemon.registry daemon in
    let owners = Registry.stats registry in
    let provider_calls = !providers in
    let controls = Recurrences.create connection ~session:started.session () in
    let status = Recurrences.status controls Support.recurrence_id |> protocol_ok in
    let view = recurrence_view status in
    assert (P.Session_ref.equal view.basis.session started.session);
    assert (P.Id.Recurrence.equal view.value.entry.id Support.recurrence_id);
    assert (
      Lifecycle.Revision.equal
        view.basis.lifecycle_revision
        (Lifecycle.Expected.lifecycle_revision (Lifecycle.Observation.expected archived)));
    assert (
      match view.basis.availability, view.value.planning, view.value.entry.control with
      | Stopped, Unavailable Controller_not_loaded, Retired Lifecycle_changed -> true
      | _ -> false);
    assert (not view.basis.automatic);
    assert (not view.can_create);
    assert (List.is_empty view.value.entry.actions);
    [%test_eq: int] 0 view.value.pending_count;
    assert (Option.is_none (Registry.find registry session_id));
    let after_status = Registry.stats registry in
    [%test_eq: int] owners.loaded after_status.loaded;
    [%test_eq: int] owners.indexed after_status.indexed;
    [%test_eq: int] provider_calls !providers;
    Client.Admin.restore_session
      connection
      (Support.lifecycle_request archived "recurring-review-restore")
    |> protocol_ok
    |> ignore;
    let restored = Support.lifecycle connection session_id in
    assert (
      Lifecycle.Result.Admission.equal
        (Lifecycle.Observation.admission restored)
        Explicit_resume_required);
    let resumed =
      Client.Admin.resume_session
        connection
        (Support.lifecycle_request restored "recurring-review-resume")
      |> protocol_ok
    in
    assert (
      Lifecycle.Result.Admission.equal (Lifecycle.Result.admission resumed) Automatic);
    let reopened = Registry.load (Daemon.registry daemon) session_id |> protocol_ok in
    assert (not (phys_equal parent.actor reopened.actor));
    Support.with_writer env sw connection ~session_id (fun handle ->
      Handle.start handle ~queue_if_limited:false |> protocol_ok |> ignore);
    Support.with_driver env sw daemon reopened (fun driver ->
      started.advance_scheduler 60.1;
      Actor.sweep_recurrences reopened.actor |> protocol_ok;
      Support.Driver.process_entry driver reopened |> protocol_ok);
    let recovered = Support.healthy reopened in
    assert (P.Session.equal_desired_state recovered.lifecycle.desired Running);
    assert (Entry.equal_control (read_control recovered) (Retired Lifecycle_changed));
    assert (List.is_empty (Frontier.ready (Support.frontier recovered)));
    assert_retained recovered);
  [%test_eq: int] 1 !providers
;;

let%expect_test
    "composed Goal retries once and joins two real captured recurrence outcomes"
  =
  with_waiting_controller check_resident;
  print_endline
    "each review stages one decision; two independently charged workers settle the same \
     Goal";
  print_endline
    "operator inspection sees the fixed-delay schedule, next due time and both retained \
     outcomes under current visibility";
  [%expect
    {|
    each review stages one decision; two independently charged workers settle the same Goal
    operator inspection sees the fixed-delay schedule, next due time and both retained outcomes under current visibility
    |}]
;;

let%expect_test "retired native Goal fences its timer through detach, Stop and archive" =
  with_waiting_controller check_retired;
  print_endline
    "detach preserves Ready; Stop cancels the timer; archive retires recurrence on reopen";
  print_endline
    "operator Status observes the archived recurrence without loading a session";
  [%expect
    {|
    detach preserves Ready; Stop cancels the timer; archive retires recurrence on reopen
    operator Status observes the archived recurrence without loading a session
    |}]
;;
