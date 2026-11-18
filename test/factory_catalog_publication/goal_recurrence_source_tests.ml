open! Core
open Agent_server_test_support
open Retry_support
module Support = Goal_recurrence_host_support
module Host = Goal_recurrence_host_tests
module Native = Goal_native_start_tests
module Recovery = Native.Example_recovery
module G = P.Goal
module H = Agent_client.Session_handle
module Controls = Agent_client.Goal_controls
module Selection = A.Instruction_selection
module Accounting = A.Goal_accounting
module Program = Agent_store.Prompt_program_bundle

let resume_input = "Inspect the same release-note handoff after the host restarts."

let revise_sources env root =
  let write relative f =
    let path = Eio.Path.(Eio.Stdenv.fs env / root / relative) in
    let before = Eio.Path.load path in
    let after = f before in
    assert (not (String.equal before after));
    Eio.Path.save ~create:(`Or_truncate 0o600) path after
  in
  List.iter Support.guidance_members ~f:(fun member ->
    write (Filename.concat Support.guidance_root member) (fun source ->
      source ^ "\nRevised guidance must stay inactive.\n"));
  write (Support.example ^ "moderator.chatml") (fun source ->
    String.substr_replace_first
      source
      ~pattern:"Stored instruction summary is malformed"
      ~with_:"Revised controller could not read the stored instruction summary")
;;

let programs daemon revision_id =
  A.Prompt_catalog.find_revision (Daemon.prompts daemon) revision_id
  |> Option.value_exn
  |> A.Prompt_revision.programs
  |> Option.value_exn
  |> Chatml_prompt_programs.to_storage
  |> Program.manifest
;;

let selection (current : A.Session_state.t) =
  Option.value_exn current.instruction_selection
;;

let selection_document current = Selection.to_jsonaf (selection current) |> protocol_ok

let usage (current : A.Session_state.t) account =
  Accounting.usage account ~ledger:current.inference_ledger
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Accounting.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let assert_changed_discovery handle original =
  let module Wire = P.Instruction_control in
  let module Controls = Agent_client.Instruction_controls in
  let controls = H.instructions handle in
  let current = Controls.inspect controls |> protocol_ok in
  let selected =
    [ Wire.Source_request.members (Option.value_exn current.project); current.skills ]
  in
  let observed = List.map (Support.Guidance.manifests controls) ~f:snd in
  List.iter2_exn selected observed ~f:(fun old fresh ->
    let member = Support.one "retained instruction member" old in
    let refreshed = Support.one "refreshed instruction member" fresh in
    assert (Wire.Root_id.equal member.root refreshed.root);
    assert (
      String.equal
        (Support.Portable_path.to_string member.member)
        (Support.Portable_path.to_string refreshed.member));
    assert (not (Chatml_source.Digest.equal member.digest refreshed.digest)));
  let after_refresh = Controls.inspect controls |> protocol_ok in
  assert (P.Instruction_status.equal original after_refresh.summary);
  assert (P.Instruction_status.equal current.summary after_refresh.summary)
;;

let assert_captured_text ~env ~root selection =
  let paths =
    List.map Support.guidance_members ~f:(Filename.concat Support.guidance_root)
  in
  let bodies =
    List.map paths ~f:(fun path -> Eio.Path.(load (Eio.Stdenv.fs env / root / path)))
  in
  let selected = Selection.project selection @ Selection.skills selection in
  [%test_eq: string list]
    bodies
    (List.map selected ~f:Instruction_content.Loaded.contents)
;;

let command current mutation ~source ~key =
  let catalog = Option.value_exn current.A.Session_state.goal_catalog in
  P.Goal_command.create
    ~session_id:current.identity.session_id
    ~generation:current.identity.generation
    ~source
    ~expected_catalog_revision:(A.Goal_catalog.revision catalog)
    ~expected_goal_revision:(Some (G.revision (Support.goal current)))
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    ~mutation
  |> protocol_ok
;;

let stop handle entry ~clock =
  H.stop handle ~mode:Cancel |> protocol_ok |> ignore;
  Support.await ~clock ~phase:"stopping the composed controller" (fun () ->
    let current = Support.healthy entry in
    match current.lifecycle.observed with
    | Stopped -> Some current
    | Queued_for_slot
    | Starting
    | Recovering
    | Idle
    | Running_turn _
    | Compacting _
    | Waiting_for_permission _
    | Stopping -> None
    | Failed _ -> failwith "Stop failed to retain the composed controller")
;;

let assert_compaction_terminal
      (entry : Registry.entry)
      ~after_sequence
      ~through_sequence
      id
  =
  let events =
    match
      A.Durable_event_log.replay entry.durable_events ~after_sequence ~through_sequence
    with
    | Available events -> events
    | Snapshot_required -> failwith "Composed compaction terminal evidence expired"
  in
  let terminal =
    List.find_map events ~f:(fun (event : P.Event.Durable.t) ->
      match
        P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok
      with
      | Operation_completed operation
      | Operation_failed operation
      | Operation_interrupted operation
      | Operation_cancelled operation
        when P.Id.Operation.equal id operation.id -> Some operation
      | _ -> None)
    |> Option.value_exn
  in
  match terminal.state with
  | Completed -> ()
  | _ ->
    raise_s [%sexp "composed compaction did not complete", (terminal : P.Operation.t)]
;;

let check_source_and_reset
      ~env
      ~sw
      ~clock
      ~root
      ~daemon
      ~(parent : Registry.entry)
      ~(started : Native.accepted_start)
      ~driver:_
      ~providers
      ~policy
      ({ admission; episode; snapshot; _ } : Host.review_wait)
  =
  let session_id = P.Session_ref.session_id started.session in
  let waiting = Support.healthy parent in
  let original_programs = programs daemon waiting.spec.prompt_revision_id in
  let original_source = G.source (Support.goal waiting) in
  let original_selection = selection_document waiting in
  let original_summary = Selection.summary (selection waiting) in
  let account = Native.Startup_fixture.accounting Support.startup waiting in
  let original_usage = usage waiting account in
  let check_account state =
    let actual = Native.Startup_fixture.accounting Support.startup state in
    assert (Accounting.equal account actual);
    assert (A.Goal_usage.equal original_usage (usage state actual))
  in
  [%test_eq: int64 * int64]
    (1L, 1L)
    (Accounting.admitted_turns account, Accounting.admitted_attempts account);
  assert_captured_text ~env ~root (selection waiting);
  Support.with_operator daemon (fun connection ->
    Support.with_writer env sw connection ~session_id (fun handle ->
      let controls = H.goals handle |> protocol_ok in
      let hold =
        command
          (Support.healthy parent)
          (Hold (Support.goal_id, Operator_pause))
          ~source:None
          ~key:"hold-review-for-compaction"
      in
      Controls.command controls hold |> protocol_ok |> ignore));
  let held = Support.healthy parent in
  assert (G.State.equal (Open (Held Operator_pause)) (G.state (Support.goal held)));
  let operation, compacted =
    Recovery.compact ~connect:Support.operator_connection env sw daemon parent
  in
  assert_compaction_terminal
    parent
    ~after_sequence:held.counters.event_sequence
    ~through_sequence:compacted.counters.event_sequence
    operation;
  let assert_compaction (current : A.Session_state.t) =
    [%test_eq: int]
      (held.conversation.compaction_generation + 1)
      current.conversation.compaction_generation;
    let archive =
      List.find_exn current.conversation.compaction_archives ~f:(fun archive ->
        P.Id.Operation.equal operation archive.operation_id)
    in
    assert (A.Session_state.Compaction_archive.equal_kind Compaction archive.kind);
    [%test_eq: int] 0 (Native.user_input_count ~expected:Support.input_text current);
    [%test_eq: int]
      1
      (List.count current.conversation.canonical_history ~f:(fun entry ->
         String.is_substring
           (Jsonaf.to_string entry.P.History.payload)
           ~substring:Host.compaction_output))
  in
  assert_compaction compacted;
  check_account compacted;
  assert (Jsonaf.exactly_equal original_selection (selection_document compacted));
  Host.assert_snapshot snapshot (Host.checkpoint compacted);
  [%test_eq: int] 2 !providers;
  let stopped =
    Support.with_operator daemon (fun connection ->
      Support.with_writer env sw connection ~session_id (fun handle ->
        stop handle parent ~clock))
  in
  let old_run = Support.run stopped admission.run_id in
  (match old_run.lifecycle with
   | Terminal Interrupted -> ()
   | _ -> failwith "Controller did not interrupt the prior waiting Run");
  let old_terminal =
    Host.root_receipts stopped admission Terminal |> Support.one "old terminal receipt"
  in
  let assert_old_run current =
    assert (P.Run.equal old_run (Support.run current admission.run_id));
    assert (
      P.Run_receipt.equal
        old_terminal
        (Host.root_receipts current admission Terminal
         |> Support.one "retained old terminal receipt"))
  in
  let assert_retained current =
    check_account current;
    assert_old_run current;
    assert (Jsonaf.exactly_equal original_selection (selection_document current))
  in
  revise_sources env root;
  started.resume_scheduler ();
  Recovery.with_reopened
    ~configure:Support.configure_guidance
    env
    sw
    root
    daemon
    Support.startup
    policy
    Fn.id
    parent
    (fun reopened entry ->
       let cold = Support.healthy entry in
       assert (not (Owner.is_loaded entry.runtime));
       assert (
         P.Id.Prompt_revision.equal
           waiting.spec.prompt_revision_id
           cold.spec.prompt_revision_id);
       assert (
         Program.Manifest.equal
           original_programs
           (programs reopened cold.spec.prompt_revision_id));
       assert_retained cold;
       assert_compaction cold;
       Host.assert_snapshot snapshot (Host.checkpoint cold);
       [%test_eq: int] 2 !providers;
       Support.with_operator reopened (fun connection ->
         Support.with_writer env sw connection ~session_id (fun handle ->
           assert_changed_discovery handle original_summary;
           H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
           Owner.with_loaded_runtime entry.runtime (fun runtime ->
             Goal_native_test_support.await_activation entry runtime;
             Ok ())
           |> protocol_ok;
           let resumed = Support.healthy entry in
           assert_retained resumed;
           Host.assert_instruction_checkpoint (Host.checkpoint resumed);
           assert (
             List.exists resumed.moderator_executions ~f:(fun callback ->
               P.Moderator_execution.equal_phase Session_resume callback.context.phase
               && P.Moderator_observer.equal
                    original_source.observer
                    callback.context.source));
           H.send_message
             handle
             { kind = Plain_text; text = resume_input; attachments = [] }
           |> protocol_ok
           |> ignore;
           let after_turn =
             Support.await ~clock ~phase:"joining the resumed real provider" (fun () ->
               let current = Support.healthy entry in
               if
                 Int.equal !providers 3
                 && Option.is_none current.active_operation
                 && List.length current.moderator_executions
                    > List.length resumed.moderator_executions
               then Some current
               else None)
           in
           assert_retained after_turn));
       Recovery.install_revised_source env sw reopened entry;
       Support.with_operator reopened (fun connection ->
         Support.with_writer env sw connection ~session_id (fun handle ->
           let changed = Support.healthy entry in
           let goal = Support.goal changed in
           let _, new_source = A.Goal_catalog_source.capture changed |> protocol_ok in
           assert (P.Run_source.equal original_source (G.source goal));
           assert (not (P.Run_source.equal original_source new_source));
           assert (
             Int64.(new_source.installation_epoch > original_source.installation_epoch));
           assert (
             not
               (P.Id.Prompt_revision.equal
                  cold.spec.prompt_revision_id
                  changed.spec.prompt_revision_id));
           assert (
             not
               (Program.Manifest.equal
                  original_programs
                  (programs reopened changed.spec.prompt_revision_id)));
           assert_retained changed;
           let controls = H.goals handle |> protocol_ok in
           let progress : P.Goal_command.Mutation.t =
             Record_progress
               ( Support.goal_id
               , G.Progress.of_string "Review checked revised source." |> protocol_ok )
           in
           let premigration =
             command
               changed
               progress
               ~source:(Some new_source)
               ~key:"before-review-adoption"
           in
           [%test_eq: P.Error.code]
             Conflict
             (Controls.command controls premigration |> rejected).code;
           let adopt =
             command
               changed
               (Adopt_source Support.goal_id)
               ~source:(Some new_source)
               ~key:"adopt-review-source"
           in
           Controls.command controls adopt |> protocol_ok |> ignore;
           let adopted = Support.healthy entry in
           let goal = Support.goal adopted in
           assert (P.Run_source.equal new_source (G.source goal));
           assert (G.State.equal (Open (Held Source_review)) (G.state goal));
           assert (Option.equal P.Goal_episode.equal (Some episode) (G.episode goal));
           assert_retained adopted;
           let stopped = stop handle entry ~clock in
           let previous_command =
             command
               stopped
               (Hold (Support.goal_id, Operator_pause))
               ~source:None
               ~key:"stale-after-review-reset"
           in
           let reset =
             H.reset
               handle
               ~expected_revision:stopped.counters.revision
               ~keep_history:true
               ~keep_tasks:false
               ~keep_grants:true
               ~keep_cache:true
               ~keep_workspace:true
               ~keep_labels:true
             |> protocol_ok
           in
           [%test_eq: int] (stopped.identity.generation + 1) reset.generation;
           let after_reset = Support.healthy entry in
           let catalog = Option.value_exn after_reset.goal_catalog in
           [%test_eq: int64]
             previous_command.expected_catalog_revision
             (A.Goal_catalog.revision catalog);
           [%test_eq: int64 option]
             previous_command.expected_goal_revision
             (Some (G.revision (Support.goal after_reset)));
           [%test_eq: P.Error.code]
             Conflict
             (Controls.command controls previous_command |> rejected).code;
           Actor.sweep_recurrences entry.actor |> protocol_ok;
           let reset = Support.healthy entry in
           let goal = Support.goal reset in
           assert (G.State.equal (Open (Held Source_review)) (G.state goal));
           assert (P.Run_source.equal new_source (G.source goal));
           assert (Option.equal P.Goal_episode.equal (Some episode) (G.episode goal));
           assert_retained reset;
           assert_compaction reset;
           assert (Option.is_none reset.active_operation);
           assert (List.is_empty (Frontier.ready (Support.frontier reset)));
           Support.assert_no_child reopened reset;
           [%test_eq: int] 3 !providers)))
;;

let%expect_test "composed Goal retains actual usage through source adoption and reset" =
  Host.with_waiting_controller ~resume_input check_source_and_reset;
  print_endline
    "compaction and physical resume retain the captured guidance and code; adoption and \
     reset retain the charged Goal";
  [%expect
    {| compaction and physical resume retain the captured guidance and code; adoption and reset retain the charged Goal |}]
;;
