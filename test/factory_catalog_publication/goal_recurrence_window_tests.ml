open! Core
open Agent_server_test_support
open Retry_support
module Support = Goal_recurrence_host_support
module Host = Goal_recurrence_host_tests
module Native = Goal_native_start_tests
module Source = Goal_recurrence_source_tests
module Windows = Agent_client.Recovery_windows
module Wire = P.Recovery_rollover_control
module Scope = P.Recovery_window.Scope
module Handle = Agent_client.Session_handle
module Goals = Agent_client.Goal_controls
module G = P.Goal
module Accounting = A.Goal_accounting
module Execution = A.Recurrence_execution

let progress = "The first independent release-note review was accepted."
let resume_input = "Continue from the accepted release-note review."
let rollover_key = P.Idempotency_key.of_string "review-window-cut" |> protocol_ok

let configure root config =
  let config = Support.configure_guidance root config in
  let recovery_capacity =
    Agent_server.Recovery_capacity_configuration.create
      ~mode:Manual
      ~host_total:7_000L
      ~targets:[]
    |> protocol_ok
  in
  { config with
    server = { config.server with recovery_capacity = Some recovery_capacity }
  }
;;

let with_writer env sw daemon session f =
  let connection = Support.operator_connection daemon principal in
  protect_cleanup
    ~finally:(fun () -> Agent_client.Connection.close connection)
    (fun () ->
       let features = [ P.Recovery_query.Features.windows; Wire.feature ] in
       let initialized =
         Handle.initialize_with_features
           connection
           ~implementation_name:"composed-recovery-restart"
           ~implementation_version:"dev"
           ~features
         |> protocol_ok
       in
       List.iter features ~f:(fun feature ->
         assert (List.mem initialized.enabled_features feature ~equal:String.equal));
       Support.with_attachment
         env
         sw
         connection
         ~session_id:(P.Session_ref.session_id session)
         ~mode:Owner_read_write
         (fun handle -> f connection handle))
;;

let active_window (state : A.Session_state.t) =
  Option.value_exn state.recovery_windows
  |> A.Recovery_window_ledger.active
  |> A.Recovery_window_ledger.Window.scope
;;

let operator_cut connection handle session =
  let attachment_id = (Handle.attachment handle).id in
  let status =
    Windows.inspect_rollover
      connection
      { session; attachment_id = Some attachment_id; key = None }
    |> protocol_ok
  in
  let expected =
    match status.view with
    | Available { current; actions; _ } when actions.force -> current
    | Available _ | Unavailable ->
      failwith "The composed idle controller cannot roll over"
  in
  Windows.request_rollover
    connection
    { expected; attachment_id; key = rollover_key; force = true }
  |> protocol_ok
;;

let account = Native.Startup_fixture.accounting Support.startup

let goal_calls (state : A.Session_state.t) =
  state.invocations
  |> List.filter_map ~f:(fun invocation ->
    if
      List.mem
        [ "goal_create"; "goal_start"; "recurrence_create"; "recurrence_resume" ]
        invocation.context.tool_name
        ~equal:String.equal
    then Some invocation
    else None)
;;

let only_execution current =
  match Support.execution current ~slot:0L with
  | Some execution ->
    [%test_eq: int]
      1
      (List.length (A.Recurrence_frontier.executions (Support.frontier current)));
    [%test_eq: int] 1 (List.length (Execution.attempts execution));
    execution
  | None -> failwith "The first recurring worker was not admitted"
;;

let run_index (state : A.Session_state.t) = Option.value_exn state.run_state

let assert_recovered_run current episode previous (recovered : P.Run.t) =
  (match previous.P.Run.lifecycle with
   | Waiting _ -> ()
   | Admitted | Active | Terminal _ -> failwith "The archived Run did not retain its Wait");
  assert (P.Run.Lifecycle.equal (Terminal Interrupted) recovered.lifecycle);
  [%test_eq: int64] (Int64.succ previous.revision) recovered.revision;
  P.Run.validate_transition ~previous recovered |> protocol_ok;
  let receipt =
    A.Run_state.receipts (run_index current)
    |> List.filter ~f:(fun receipt ->
      P.Id.Run.equal recovered.id receipt.P.Run_receipt.run_id
      && P.Run_receipt.Kind.equal receipt.kind Terminal)
    |> Support.one "one restart interruption receipt for the original Run"
  in
  P.Goal_episode.Settlement.create episode ~run:recovered ~receipt
  |> protocol_ok
  |> ignore
;;

let check_retained
      ?(reopened = false)
      (before : A.Session_state.t)
      (current : A.Session_state.t)
  =
  let older_goal = Support.goal before in
  let current_goal = Support.goal current in
  let episode = Option.value_exn (G.episode older_goal) in
  let admission = P.Goal_episode.admission episode in
  assert (Option.equal P.Goal_episode.equal (Some episode) (G.episode current_goal));
  assert (G.Progress.equal (G.progress older_goal) (G.progress current_goal));
  let retain_run (previous : P.Run.t) (recovered : P.Run.t) =
    if reopened && P.Id.Run.equal admission.run_id previous.id
    then assert_recovered_run current episode previous recovered
    else assert (P.Run.equal previous recovered)
  in
  List.iter2_exn
    (A.Run_state.runs (run_index before))
    (A.Run_state.runs (run_index current))
    ~f:retain_run;
  if not reopened
  then (
    let receipts state = A.Run_state.receipts (run_index state) in
    assert (List.equal P.Run_receipt.equal (receipts before) (receipts current)));
  assert (Accounting.equal (account before) (account current));
  assert (
    A.Goal_usage.equal
      (Source.usage before (account before))
      (Source.usage current (account current)));
  assert (Execution.equal (only_execution before) (only_execution current));
  assert (
    String.equal
      (A.Recurrence_definition.digest
         (A.Recurrence_entry.current (Support.recurrence before)))
      (A.Recurrence_definition.digest
         (A.Recurrence_entry.current (Support.recurrence current))));
  assert (
    Jsonaf.exactly_equal
      (Source.selection_document before)
      (Source.selection_document current));
  assert (
    P.Id.Prompt_revision.equal
      before.spec.prompt_revision_id
      current.spec.prompt_revision_id);
  assert (List.equal P.Invocation.equal (goal_calls before) (goal_calls current))
;;

let committed (receipt : Wire.Receipt.t) =
  match receipt.outcome with
  | Committed { successor; cut_revision; quality = Archive_only_missing_checkpoint } ->
    successor, cut_revision
  | Pending | Failed _ | Committed _ -> failwith "Expected an archive-only rollover"
;;

let run_child root =
  Host.with_waiting_controller
    ~test_root:root
    ~configure
    (fun
        ~env
         ~sw
         ~clock
         ~root:_
         ~daemon
         ~parent
         ~started
         ~driver
         ~providers
         ~policy:_
         waiting
       ->
       Support.advance_schedule_and_join ~clock daemon started 35.1;
       let occurrence, child =
         Support.join_occurrence
           ~clock
           ~tick:(fun () -> started.advance_scheduler 0.1)
           daemon
           driver
           parent
           ~slot:0L
       in
       let current = Support.healthy parent in
       let session = started.session in
       ignore
         (Support.assert_descendant
            daemon
            current
            ~session
            ~episode:waiting.episode
            occurrence
            child
          : A.Inference_ledger.Handle.t);
       Support.assert_totals current ~session ~children:1;
       [%test_eq: int] 2 !providers;
       let receipt =
         with_writer env sw daemon session (fun connection handle ->
           let previous =
             Windows.enable
               connection
               { session
               ; attachment_id = (Handle.attachment handle).id
               ; expected_generation = current.identity.generation
               }
             |> protocol_ok
           in
           let controls = Handle.goals handle |> protocol_ok in
           let goal = Support.goal current in
           let record =
             Source.command
               (Support.healthy parent)
               (Record_progress
                  (Support.goal_id, G.Progress.of_string progress |> protocol_ok))
               ~source:(Some (G.source goal))
               ~key:"first-retained-review-progress"
           in
           Goals.command controls record |> protocol_ok |> ignore;
           let hold =
             Source.command
               (Support.healthy parent)
               (Hold (Support.goal_id, Operator_pause))
               ~source:None
               ~key:"hold-review-for-window-restart"
           in
           Goals.command controls hold |> protocol_ok |> ignore;
           let before = Support.healthy parent in
           assert (Scope.equal previous (active_window before));
           (match (Support.run before waiting.admission.run_id).lifecycle with
            | Waiting wake -> assert (P.Run_wake.equal waiting.wake wake)
            | Admitted | Active | Terminal _ -> failwith "The original Goal lost its Wait");
           let receipt = operator_cut connection handle session in
           let after = Support.healthy parent in
           assert (Scope.equal previous receipt.expected.scope);
           assert (Scope.equal (fst (committed receipt)) (active_window after));
           assert (not (Scope.equal previous (active_window after)));
           assert (List.is_empty after.conversation.canonical_history);
           check_retained before after;
           receipt)
       in
       [%test_eq: int] 2 !providers;
       (* Preserve the actual durable state before runtime shutdown can retire the old
          process's private wake authority. The reader is a separate physical host. *)
       let observed_at = (Support.healthy parent).identity.updated_at in
       Eio.Flow.copy_string
         (Jsonaf.to_string
            (`Object
                [ "receipt", Wire.Receipt.to_json receipt
                ; "observed_at", P.Timestamp.to_json observed_at
                ])
          ^ "\n")
         (Eio.Stdenv.stdout env);
       Core_unix.exit_immediately 0)
;;

let read_predecessor
      env
      (entry : Registry.entry)
      (current : A.Session_state.t)
      (receipt : Wire.Receipt.t)
  =
  let reference =
    List.filter current.conversation.compaction_archives ~f:(fun archive ->
      A.Session_state.Compaction_archive.equal_kind Rollover archive.kind)
    |> Support.one "physical composed rollover archive"
  in
  [%test_eq: int64] (snd (committed receipt)) reference.revision;
  let predecessor =
    A.Compaction_archive.read
      ~env
      ~handle:(Option.value_exn entry.store_handle)
      ~max_payload_length:
        (Document_schema.Limits.max_bytes Document_schema.Limits.default)
      reference
    |> protocol_ok
  in
  assert (Scope.equal receipt.expected.scope (active_window predecessor));
  [%test_eq: int] 1 (Native.user_input_count ~expected:Support.input_text predecessor);
  predecessor
;;

let resume_request (current : A.Session_state.t) =
  let goal = Support.goal current in
  let admission = P.Goal_episode.admission (Option.value_exn (G.episode goal)) in
  let run = Support.run current admission.run_id in
  let catalog = Option.value_exn current.A.Session_state.goal_catalog in
  P.Goal_resume_request.create
    ~session_id:current.identity.session_id
    ~generation:current.identity.generation
    ~goal_id:Support.goal_id
    ~run_id:run.id
    ~source:run.source
    ~expected_goal_revision:(G.revision goal)
    ~expected_catalog_revision:(A.Goal_catalog.revision catalog)
    ~expected_run_revision:run.revision
    ~key:(P.Idempotency_key.of_string "resume-composed-goal" |> protocol_ok)
  |> protocol_ok
;;

let background_instruction_denial current =
  List.find_map current.A.Session_state.moderator_executions ~f:(fun callback ->
    match callback.P.Moderator_execution.context.phase, callback.status with
    | Session_resume, Completed _ ->
      let id = callback.context.id in
      let denied =
        List.exists current.invocations ~f:(fun invocation ->
          let belongs =
            Option.equal P.Id.Moderator_execution.equal invocation.parent_event (Some id)
          in
          match invocation.status with
          | Resolved (Fail error) | Published (Fail error) ->
            belongs
            && String.equal invocation.context.tool_name "instructions_status"
            && String.equal error.code "permission_denied"
          | _ -> false)
      in
      Option.some_if denied id
    | _ -> None)
;;

let verify_resumed env sw daemon (entry : Registry.entry) receipt providers =
  let session = Scope.session receipt.Wire.Receipt.expected.scope in
  let clock = Eio.Stdenv.clock env in
  let cold =
    Support.await ~clock ~phase:"observing unattended instruction denial" (fun () ->
      let current = Support.healthy entry in
      Option.map (background_instruction_denial current) ~f:(fun _ -> current))
  in
  assert (Scope.equal (fst (committed receipt)) (active_window cold));
  let predecessor = read_predecessor env entry cold receipt in
  check_retained ~reopened:true predecessor cold;
  Host.assert_snapshot (Host.checkpoint predecessor) (Host.checkpoint cold);
  [%test_eq: string] progress (G.Progress.to_string (G.progress (Support.goal cold)));
  [%test_eq: int] 0 !providers;
  let retained = only_execution cold in
  let origin =
    Execution.Attempt.origin (Execution.current_attempt retained) |> Option.value_exn
  in
  with_writer env sw daemon session (fun connection handle ->
    Handle.stop handle ~mode:Cancel |> protocol_ok |> ignore;
    let stopped =
      Support.await ~clock ~phase:"stopping the unattended owner" (fun () ->
        let current = Support.healthy entry in
        match current.lifecycle.observed with
        | Stopped -> Some current
        | Failed error -> raise_s [%sexp (error : P.Error.t)]
        | _ -> None)
    in
    check_retained cold stopped;
    Host.assert_operator_history daemon ~session [ origin ];
    let attachment_id = (Handle.attachment handle).id in
    let request : Wire.Request.t =
      { expected = receipt.expected; attachment_id; key = rollover_key; force = true }
    in
    assert (
      Wire.Receipt.equal
        receipt
        (Windows.request_rollover connection request |> protocol_ok));
    let stale =
      { request with
        key = P.Idempotency_key.of_string "retired-review-window" |> protocol_ok
      }
    in
    [%test_eq: P.Error.code]
      Conflict
      (Windows.request_rollover connection stale |> rejected).code;
    let controls = Handle.goals handle |> protocol_ok in
    let fresh = Support.healthy entry in
    let resume = resume_request fresh in
    let previous_writer =
      List.find_exn predecessor.attachments ~f:(fun attachment ->
        P.Session.equal_attachment_mode attachment.mode Owner_read_write)
    in
    let retired_writer =
      Goals.create connection ~session ~attachment:(fun () -> previous_writer.id) ()
    in
    [%test_eq: P.Error.code]
      Permission_denied
      (Goals.resume retired_writer resume |> rejected).code;
    check_retained fresh (Support.healthy entry);
    let check_denied () =
      match Goals.resume controls resume |> protocol_ok with
      | Rejected error -> [%test_eq: P.Error.code] Conflict error.code
      | Resumed _ | Retained _ | Uncertain _ | Resume_failed _ ->
        failwith "A retired Run acquired an acknowledgement or continuation"
    in
    check_denied ();
    check_denied ();
    let after_resume = Support.healthy entry in
    check_retained cold after_resume;
    assert (
      G.State.equal (Open (Held Operator_pause)) (G.state (Support.goal after_resume)));
    [%test_eq: int] 0 !providers;
    let started = Handle.start handle ~queue_if_limited:false |> protocol_ok in
    assert (P.Session.equal_desired_state started.desired_state Running);
    let instructions =
      Agent_client.Instruction_controls.inspect (Handle.instructions handle)
      |> protocol_ok
    in
    let selected = A.Instruction_selection.summary (Source.selection cold) in
    assert (P.Instruction_status.equal selected instructions.summary);
    let submitted =
      Handle.send_message
        handle
        { kind = Plain_text; text = resume_input; attachments = [] }
      |> protocol_ok
    in
    assert (P.Method_result.Send_message.equal_disposition submitted.disposition Started);
    let operation_id = Option.value_exn submitted.operation_id in
    let after =
      Support.await ~clock ~phase:"joining the resumed composed provider" (fun () ->
        let current = Support.healthy entry in
        match
          A.Durable_event_log.replay
            entry.durable_events
            ~after_sequence:after_resume.counters.event_sequence
            ~through_sequence:current.counters.event_sequence
        with
        | Snapshot_required -> failwith "The recovered turn's terminal event expired"
        | Available events ->
          List.find_map events ~f:(fun event ->
            match
              P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
              |> protocol_ok
            with
            | Operation_completed operation
              when P.Id.Operation.equal operation.id operation_id -> Some current
            | Operation_failed operation
            | Operation_interrupted operation
            | Operation_cancelled operation
              when P.Id.Operation.equal operation.id operation_id ->
              raise_s
                [%sexp "The recovered turn did not complete", (operation : P.Operation.t)]
            | _ -> None))
    in
    Host.assert_instruction_checkpoint (Host.checkpoint after);
    check_retained cold after;
    [%test_eq: int] 1 (Native.user_input_count ~expected:resume_input after);
    assert (Scope.equal (fst (committed receipt)) (active_window after)));
  Host.assert_operator_history daemon ~session [ origin ];
  [%test_eq: int] 1 !providers
;;

let%expect_test
    "composed Goal, admitted recurrence and instructions survive window restart"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    protect_cleanup
      ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root))
      (fun () ->
         let handoff =
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 120. (fun () ->
             Eio.Process.parse_out
               (Eio.Stdenv.process_mgr env)
               ~env:[| "PATH=/usr/bin:/bin" |]
               Eio.Buf_read.take_all
               [ "./goal_native_fault_child.exe"; root; "goal-recurrence-window-restart" ])
           |> Jsonaf.of_string
         in
         let receipt =
           Jsonaf.member_exn "receipt" handoff |> Wire.Receipt.of_json |> protocol_ok
         in
         let observed_at =
           Jsonaf.member_exn "observed_at" handoff
           |> P.Timestamp.of_json
           |> protocol_ok
           |> P.Timestamp.to_time_ns
           |> Time_ns.to_span_since_epoch
           |> Time_ns.Span.to_sec
         in
         (* Begin at the last durable wall time, before the fixture's future Goal
            timer and next occurrence. Runtime and test deadlines still advance. *)
         let wall_clock, _, _, _ =
           controlled_wall_clock (Eio.Stdenv.clock env) ~initial:observed_at
         in
         let restarted_env =
           Goal_control_job_tests.with_clocks
             (env :> Eio_unix.Stdenv.base)
             ~clock:wall_clock
             ~mono_clock:(Eio.Stdenv.mono_clock env)
         in
         let providers = ref 0 in
         let policy =
           guidance_inference_policy
             ~default_model:"fixture-model"
             ~post_stream:(fun ~sw:_ ~inputs ->
               Int.incr providers;
               Support.Guidance.assert_provider_inputs inputs;
               let wire =
                 Jsonaf.to_string (`Array (List.map inputs ~f:Res.Item.jsonaf_of_t))
               in
               assert (String.is_substring wire ~substring:resume_input);
               assert (not (String.is_substring wire ~substring:Support.input_text));
               completed_response "The accepted review is ready to use.")
             ()
         in
         let startup = Support.startup in
         let prompt = Filename.concat root (Option.value_exn startup.workspace_prompt) in
         let config = config root root prompt in
         let config =
           configure
             root
             { config with
               server =
                 { config.server with
                   project_sources =
                     Some
                       { Agent_server.Project_source_configuration.local_only with
                         external_roots =
                           List.map
                             startup.workspace_project_roots
                             ~f:(Filename.concat root)
                       }
                 }
             }
         in
         Eio.Switch.run (fun sw ->
           let daemon =
             Daemon.start
               ~sw
               ~env:restarted_env
               ~config
               ~tool_dir:root
               ~home:root
               ~process_start_identity:None
               ~options:
                 { Daemon.default_options with
                   qualify_chatml_extensions = true
                 ; inference_policy = policy
                 }
               ()
             |> protocol_ok
           in
           protect_cleanup
             ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
             (fun () ->
                let id =
                  Scope.session receipt.expected.scope |> P.Session_ref.session_id
                in
                let entry = Registry.load (Daemon.registry daemon) id |> protocol_ok in
                verify_resumed env sw daemon entry receipt providers))));
  print_endline
    "retained episode interrupted once, one recurring admission and captured guidance \
     across physical window restart";
  [%expect
    {| retained episode interrupted once, one recurring admission and captured guidance across physical window restart |}]
;;
