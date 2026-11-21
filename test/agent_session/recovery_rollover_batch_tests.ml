open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module Actor = A.Session_actor
module R = A.Recovery_rollover
module W = Recovery_window_actor_tests
module C = Recovery_rollover_tests
module Disk = Recovery_rollover_commit_tests
module Graph = Agent_server.Graph_tracking
module Scope = P.Recovery_window.Scope

type scenario =
  | Native
  | Force
  | Interrupted
  | Revoked
[@@deriving equal]

let signal resolver = ignore (Eio.Promise.try_resolve resolver () : bool)
let pending state = Option.bind state.A.Session_state.recovery_rollover ~f:R.pending

let ordinal_outputs state =
  List.filter_map state.A.Session_state.conversation.canonical_history ~f:(fun entry ->
    if P.History.equal_kind entry.kind Tool_output then Some entry.id else None)
;;

let proof_for_owners = W.actor_proof ()
let permission _ = Ok ()

let install_tools env ~remaining ~new_window =
  let names = [ "context_remaining"; "context_new_window" ] in
  let registrations : Chat_response.Agent_runtime.native_registration list =
    C.selected_tools env names
  in
  let contracts, implementations =
    List.map registrations ~f:(fun registration ->
      ( (registration.implementation.info.function_.name, registration.result_contract)
      , (registration.implementation_revision, registration.implementation) ))
    |> List.unzip
  in
  let declared =
    Chat_response.Tool_capability.create
      ~result_contracts:contracts
      ~owner:"settled-rollover-tests"
      ~resource_fingerprint:
        (Chatmd_shell_spec.Source_ref.digest "settled-rollover-fixture")
      implementations
    |> Result.map_error ~f:(fun error -> error.message)
    |> Result.ok_or_failwith
  in
  let tools, tool_tbl = Ochat_function.functions (List.map implementations ~f:snd) in
  let service =
    A.Script_tool_calls.create
      ~registry:(fun () -> declared)
      ~moderator_names:String.Set.empty
      ~now:P.Timestamp.now
      ~is_halted:(fun () -> false)
      ~requires_active_moderator:(fun _ -> false)
      ~authorize:(fun _ _ -> Ok ())
      ~prepare_output:(function
        | Openai.Responses.Tool_output.Output.Text text -> Ok (`String text)
        | output -> Ok (Openai.Responses.Tool_output.Output.jsonaf_of_t output))
      ~defer_observation:(fun _ -> Ok ())
    |> fun tools ->
    A.Script_tool_calls.with_recovery_context_service
      tools
      { remaining
      ; new_window
      ; checkpoint = (fun _ _ -> failwith "unexpected authored checkpoint")
      }
  in
  let dispatch_tool ~input ~capabilities =
    A.Script_tool_calls.native_dispatch service ~declared ~input ~capabilities
  in
  Chat_response.Tool.convert_tools tools, tool_tbl, dispatch_tool
;;

let check_rollover_replay ~initial transactions (receipt : R.Receipt.t) =
  let is_rollover transaction =
    List.mem (W.delta_kinds transaction) "recovery_rollover_committed" ~equal:String.equal
  in
  let prefix, tail =
    List.split_while transactions ~f:(fun transaction -> not (is_rollover transaction))
  in
  let transaction = List.hd_exn tail in
  let old_state =
    List.fold_result
      prefix
      ~init:initial
      ~f:(A.Session_persistence.apply_transaction ~limits:document_limits)
    |> store_ok
    |> A.Session_persistence.Restored.state
  in
  assert (
    List.mem
      (Document_schema.Document.required_semantics transaction.delta)
      P.Session_storage_semantics.recovery_rollover_batch
      ~equal:String.equal);
  let change =
    match
      A.Session_delta_document.decode ~limits:document_limits transaction.delta
      |> document_ok
      |> A.Session_delta_document.value
    with
    | Batch [ Recovery_rollover_committed change ] -> change
    | _ -> failwith "rollover must use one dedicated journal leaf"
  in
  let active = Option.value_exn old_state.active_operation in
  let predecessor = Option.value_exn (R.Commit.predecessor change) in
  assert (P.Id.Operation.equal active.id predecessor);
  Disk.check_turn_starting old_state predecessor;
  let applied =
    A.Session_delta.apply old_state (Recovery_rollover_committed change) |> protocol_ok
  in
  Disk.check_turn_retired applied predecessor;
  Disk.check_attempts_preserved old_state applied;
  let fields =
    match R.Commit.to_json change with
    | `Object fields -> List.Assoc.remove fields "retired_at" ~equal:String.equal
    | _ -> failwith "expected a commit object"
  in
  let historical = R.Commit.of_json (`Object fields) |> protocol_ok in
  assert (Option.is_none historical.retired_at);
  let historically_applied =
    A.Session_delta.apply old_state (Recovery_rollover_committed historical)
    |> protocol_ok
  in
  assert (Scope.equal (W.scope applied) (W.scope historically_applied));
  Disk.check_turn_starting historically_applied predecessor;
  let stale =
    let retired_at = P.Timestamp.of_string "1970-01-01T00:00:00Z" |> protocol_ok in
    assert (P.Timestamp.compare retired_at active.updated_at < 0);
    R.Commit.of_json (`Object (("retired_at", P.Timestamp.to_json retired_at) :: fields))
    |> protocol_ok
  in
  let expect_conflict (type value) ~message (result : (value, P.Error.t) Result.t) =
    match result with
    | Error error when P.Error.equal_code error.code Conflict -> ()
    | Error error -> raise_s [%sexp (message : string), (error : P.Error.t)]
    | Ok _ -> failwith message
  in
  expect_conflict
    ~message:"replay accepted retirement before the predecessor update"
    (A.Session_delta.apply old_state (Recovery_rollover_committed stale));
  expect_conflict
    ~message:"retirement accepted another generation"
    (R.Commit.retirement
       change
       ~active:(Some { active with generation = active.generation + 1 }));
  let batch = Option.value_exn change.batch in
  let batch =
    A.Recovery_rollover_batch.create
      ~operation:batch.operation
      ~ordinal:batch.ordinal
      ~source_revision:batch.source_revision
      (List.rev batch.pairs)
    |> protocol_ok
  in
  let reordered =
    R.Commit.create
      ~batch
      ?retired_at:change.retired_at
      ~schedule_foreground:change.schedule_foreground
      receipt.intent
      ~expected:change.expected
      ~successor:change.successor
      ~archive:change.archive
      ~checkpoint:change.checkpoint
      ~retain:change.retain
    |> protocol_ok
  in
  expect_conflict
    ~message:"replay accepted reordered output acknowledgements"
    (A.Session_delta.apply old_state (Recovery_rollover_committed reordered))
;;

let queue_wake actor ~source ~snapshot =
  let registry = native_registry (ref 0) ~raises:false in
  let job = Job_fixtures.add_claimed_job actor in
  Schedule_transaction_tests.with_event actor ~snapshot job (fun owner commit ->
    A.Script_notification_service.with_scope
      (Notification_access_tests.notices actor)
      ~owner
      ~source
      ~selected:registry
      ~jobs:None
      ~error:P.Error.invalid_request
      (fun scope ->
         let open Result.Let_syntax in
         let transaction = A.Script_notification_service.moderator_transaction scope in
         let error result = Result.map_error result ~f:P.Error.invalid_request in
         let%bind receipt, _ =
           transaction.handlers.publish
             ~correlation:{ key = "successor-wake"; invocation_id = None; work = None }
             ~completion:(Succeeded (`String "saved successor wake"))
             ~wake:Request_turn
           |> error
         in
         let%bind ack = transaction.prepare [ receipt ] |> error in
         let%map () =
           commit
             ~snapshot
             ~requests:
               { request_turn = false; request_compaction = false; end_session = None }
         in
         ack ()))
  |> protocol_ok
  |> ignore;
  Job_fixtures.complete actor job |> protocol_ok |> ignore;
  registry
;;

let check_authored_wait (receipt : R.Receipt.t) =
  let module H = A.Recovery_rollover_hooks in
  let hooks = Option.value_exn receipt.hooks in
  match H.before hooks, H.after hooks with
  | Completed (before, Proceed _), Completed (after, Wait) ->
    assert (P.Id.Operation.equal before.predecessor after.predecessor);
    assert (not (Scope.equal before.window after.window))
  | _ -> failwith "the installed moderator did not author both operator dispositions"
;;

let check_authored_claim_prefixes ~initial ~transactions ~id =
  let module H = A.Recovery_rollover_hooks in
  let module Restored = A.Session_persistence.Restored in
  let before, after, _ =
    List.fold
      transactions
      ~init:(false, false, initial)
      ~f:(fun (before, after, previous) transaction ->
        let restored =
          A.Session_persistence.apply_transaction
            ~limits:document_limits
            previous
            transaction
          |> store_ok
        in
        let state = Restored.state restored in
        let receipt = Option.bind state.recovery_rollover ~f:(R.find ~id) in
        match receipt with
        | Some { status = Pending; hooks = Some hooks; _ } ->
          (match H.before hooks with
           | Claimed _ ->
             assert (List.is_empty state.conversation.compaction_archives);
             true, after, restored
           | _ -> before, after, restored)
        | Some { status = Terminal (Committed committed); hooks = Some hooks; _ } ->
          (match H.after hooks with
           | Claimed claim ->
             assert (Scope.equal claim.window committed.successor);
             let schedule =
               Option.bind state.recovery_rollover ~f:(R.foreground ~scope:claim.window)
               |> Option.value_exn
             in
             assert (A.Recovery_rollover_continuation.equal_phase schedule.phase Awaiting);
             let expected =
               R.Expected.create claim.window ~revision:state.counters.revision
               |> protocol_ok
             in
             let change =
               A.Session_delta.Recovery_rollover_changed
                 (Hook
                    { id; expected; change = Stop (After, claim.execution, Interrupted) })
             in
             let interrupted = A.Session_delta.apply state change |> protocol_ok in
             let receipt = Disk.lookup interrupted ~id in
             let hooks = Option.value_exn receipt.hooks in
             (match H.after hooks with
              | Stopped (_, Interrupted) -> ()
              | _ -> failwith "an actual orphan claim did not resolve as interrupted");
             assert (List.length interrupted.conversation.compaction_archives = 1);
             assert (Option.is_none interrupted.active_operation);
             before, true, restored
           | _ -> before, after, restored)
        | _ -> before, after, restored)
  in
  assert (before && after)
;;

let check_run_transfer state (schedule : A.Recovery_rollover_continuation.t) run_id =
  let operation =
    match schedule.phase with
    | Admitted { operation; _ } -> operation
    | _ -> failwith "the successor transfer was not admitted"
  in
  let index = Option.value_exn state.A.Session_state.run_state in
  let run = A.Run_state.find index run_id |> Option.value_exn in
  let work id =
    P.Run_work.create ~key:(Operation id) ~generation:state.identity.generation
    |> protocol_ok
  in
  assert (
    List.mem run.relinquished_work (work schedule.predecessor) ~equal:P.Run_work.equal);
  assert (List.mem run.owned_work (work operation) ~equal:P.Run_work.equal)
;;

let admit_managed_turn actor ~owner ~writer ~original ~observer ~entry text =
  let scope =
    A.Run_admission.Scope.create_authenticated
      ~actor:original
      ~observer
      ~startup_pending:(fun () -> false)
      ~authorize:permission
    |> protocol_ok
  in
  let current = W.state actor in
  let request =
    P.Run_start.create
      ~session_id
      ~attachment_id:writer
      ~generation:current.identity.generation
      ~expected_revision:current.counters.revision
      ~mode:Single_turn
      ~input:(User_submission { kind = Plain_text; text; attachments = [] })
      ~key:(Recovery_note_actor_tests.key "expire-original-run")
    |> protocol_ok
  in
  let request_sha256 =
    A.Run_admission_request.of_writer request
    |> A.Run_admission_request.request_sha256
    |> protocol_ok
  in
  let receipt =
    Actor.admit_run
      actor
      ~scope
      ~request
      ~session:owner
      ~request_sha256
      ~entry:(Some entry)
    |> protocol_ok
  in
  receipt.run_id
;;

type authored_followup =
  | Wait
  | Expire_original_run

let run ?(resume = false) ?(finish = false) ?(cancel = false) ?authored_followup scenario =
  let authored = Option.is_some authored_followup in
  let expire_original_run =
    match authored_followup with
    | Some Expire_original_run -> true
    | Some Wait | None -> false
  in
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun storage_scope ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
        let first_entered, enter_first = Eio.Promise.create () in
        let second_returning, return_second = Eio.Promise.create () in
        let wait_first, release_first = Eio.Promise.create () in
        let output_ack_blocked, block_ack = Eio.Promise.create () in
        let wait_ack, release_ack = Eio.Promise.create () in
        let successor_preparing, signal_preparation = Eio.Promise.create () in
        let worker_returned, worker_return = Eio.Promise.create () in
        let window = ref None in
        let dispatches = ref 0 in
        let hook_calls = ref 0 in
        let hint_leases = ref 0 in
        let first = ref true in
        let events ~sw:_ ~inputs =
          incr dispatches;
          match !dispatches with
          | 1 ->
            let name, arguments =
              if equal_scenario scenario Native
              then (
                let expected = Scope.window (Option.value_exn !window) in
                ( "context_new_window"
                , Jsonaf.to_string
                    (`Object [ "expected_window", P.Id.Context_window.to_json expected ])
                ))
              else "context_remaining", "{}"
            in
            Stdlib.Seq.append (C.call_event ~name ~arguments 1) (fun () ->
              Eio.Promise.await first_entered;
              match C.call_event ~name:"context_remaining" ~arguments:"{}" 2 () with
              | Seq.Cons (Openai.Responses.Response_stream.Output_item_done done_, tail)
                -> Seq.Cons (Output_item_done { done_ with output_index = 1 }, tail)
              | _ -> failwith "missing second fixture call")
          | 2 when equal_scenario scenario Native -> Seq.empty
          | 2 when resume ->
            let inputs =
              List.map inputs ~f:(fun item ->
                Openai.Responses.Item.jsonaf_of_t item |> Jsonaf.to_string)
            in
            let count text = List.count inputs ~f:(String.is_substring ~substring:text) in
            assert (Int.equal (count "admit the foreground attempt") 1);
            assert (Int.equal (count "a new queued input retains its admission") 1);
            assert (Int.equal (count "context_remaining") 0);
            assert (Int.equal (count "saved successor wake") 1);
            assert (Int.equal (count "local successor fixture hint") 1);
            Seq.empty
          | _ -> failwith "retiring worker dispatched another request"
        in
        let runtime =
          Inference_fixture.create_with_capacity_and_guidance
            ~request_guidance:resume
            ~capacity_source:(fun _ ->
              Inference_runtime.Context_capacity.Source.create
                ~encoder:Openai_responses_v1
                ~read:(fun () ->
                  Covered
                    (Inference.Observation.Count.create (Actual 100L)
                     |> Result.map_error ~f:(fun _ -> "invalid test count")
                     |> Result.ok_or_failwith)))
            ~binding_is_current:(fun () -> true)
            ~namespace:"settled-rollover-fixture"
            ~default_model:"context-batch-fixture"
            ~post_stream:events
        in
        let target, context, _ =
          Recovery_effective_attempt_tests.prepare_fixture runtime
        in
        let smaller_model = "context-batch-smaller" in
        let smaller =
          Inference.Request.Target.with_model
            target
            ~model:smaller_model
            ~limits:document_limits
          |> Configuration_tests.inference_ok
        in
        let smaller_policy =
          Inference_runtime.Context_capacity.Policy.create
            ~target:smaller
            ~encoder:Openai_responses_v1
            ~model_limit:(Bounded { total = 120L; input = Some 120L; output = None })
            ~allowance:
              { host_total = None
              ; session_total = None
              ; omitted_output_default = Some 40L
              ; checkpoint_reserve = 10L
              ; warning_lead = 5L
              }
            ~estimate:None
          |> Result.map_error ~f:(fun _ -> "smaller target policy")
          |> Result.ok_or_failwith
        in
        let base =
          actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
        in
        let inference_target =
          Inference.Selection.captured target ~limits:document_limits
          |> Configuration_tests.inference_ok
        in
        let base = { base with spec = { base.spec with inference_target } } in
        let storage = Job_artifact_fixtures.create env storage_scope base in
        let owner =
          P.Session_ref.create
            ~server_id:(Agent_store.Session_store.server_id storage.sessions)
            ~session_id
        in
        let disk : W.fixture =
          { env; sessions = storage.sessions; handle = storage.session; owner }
        in
        let initial = A.Session_persistence.Restored.authored base in
        let journal = W.journal disk Agent_store.Journal.create in
        let archive_ref = ref None in
        let operator_current = ref true in
        let original_run_current = ref true in
        let original_run_id = ref None in
        let state_committed state _ =
          match !original_run_id, A.Session_delta.foreground_continuation state with
          | Some run, Some ({ phase = Admitted _; _ } as schedule)
            when !original_run_current ->
            (* The journal has acknowledged the work transfer; the provider guard
               still owns the next application write. *)
            check_run_transfer state schedule run;
            original_run_current := false
          | _ -> ()
        in
        let operator =
          Operator_authorization.guarded
            ~principal:(Operator_authorization.principal proof_for_owners)
            ~is_current:(fun () -> !operator_current)
        in
        let archive reference document =
          W.Archive.write_document
            ~env
            ~handle:storage.session
            ~max_payload_length:W.payload_limit
            reference
            document
          |> protocol_ok;
          assert (Option.is_none !archive_ref);
          archive_ref := Some reference;
          if equal_scenario scenario Revoked then operator_current := false;
          if equal_scenario scenario Interrupted
          then
            Error
              (P.Error.create
                 Persistence_error
                 ~message:"after archive sync"
                 ~retryable:true
                 ())
          else Ok ()
        in
        let followup_write = ref ignore in
        let before_commit prepublish =
          !followup_write prepublish;
          if
            List.length (ordinal_outputs prepublish) = 1
            && not (Eio.Promise.is_resolved output_ack_blocked)
          then (
            signal block_ack;
            Eio.Promise.await wait_ack);
          Ok ()
        in
        let id, old_scope, pending_id =
          W.with_actor
            disk
            storage_scope
            journal
            ~restored:initial
            ~previous_transaction_hash:None
            ~archive
            ~before_commit
            ~state_committed
            (fun actor persistence ->
               let writer = W.attach actor in
               let old_scope =
                 Actor.enable_recovery_windows
                   actor
                   ~actor:proof_for_owners
                   ~attachment_id:writer
                   ~authorize:permission
                 |> protocol_ok
               in
               window := Some old_scope;
               let host =
                 A.Session_context_capacity.Host.create
                   ~mode_for_session:(fun selected ->
                     Option.some_if
                       (P.Session_ref.equal owner selected)
                       A.Session_context_capacity.Mode.Manual)
                   ~targets:
                     [ C.exact_target_policy target, `Automatic; smaller_policy, `Manual ]
                 |> Result.map_error ~f:(fun _ -> "capacity fixture")
                 |> Result.ok_or_failwith
               in
               Actor.set_context_capacity_host actor host |> protocol_ok;
               Actor.set_configuration_policy
                 actor
                 { select_profile =
                     (fun ~current:_ ~profile:_ ->
                       Error (P.Error.invalid_request "fixture target"))
                 ; approve = (fun ~current:_ ~proposed:_ -> Ok ())
                 ; resolve =
                     (fun selected ->
                       Inference_fixture.resolve runtime selected
                       |> Result.map_error ~f:(fun _ ->
                         P.Error.invalid_request "fixture target"))
                 }
               |> protocol_ok;
               let remaining access =
                 let observed = Actor.context_remaining_native actor ~access in
                 if !first && not (equal_scenario scenario Native)
                 then (
                   first := false;
                   signal enter_first;
                   Eio.Promise.await wait_first)
                 else signal return_second;
                 observed
               in
               let new_window access expected =
                 let receipt =
                   Actor.context_new_window_native actor ~access ~window:expected
                 in
                 signal enter_first;
                 Eio.Promise.await wait_first;
                 receipt
               in
               let tools, tool_tbl, dispatch_tool =
                 install_tools env ~remaining ~new_window
               in
               let graph =
                 Graph.create
                   actor
                   ~source:(Inference_tracking_tests.source "settled-rollover")
                   ~upstream:
                     (Inference_tracking_tests.upstream (fun ~scope:_ ~accounting_id:_ ->
                        ()))
                 |> protocol_ok
               in
               let config =
                 Authoring_input_tests.worker_config env (fun ~sw:_ ~inputs:_ ->
                   Seq.empty)
               in
               let wake_registry = ref None in
               let moderator =
                 Option.some_if (resume || authored) ()
                 |> Option.map ~f:(fun () ->
                   let manager =
                     if authored
                     then (
                       let manager, _, _ =
                         handoff_definition
                           ~events:
                             {|
    | _ -> Task.pure(state)
    let on_context_rollover ctx state event =
      match event with
      | `Before_context_rollover(_) ->
        Task.pure((|state, `Before(`Proceed({hints = `Some([]); continuation = `None}))|))
      | `After_context_rollover(_) -> Task.pure((|state, `After(`Wait)|))
|}
                           env
                       in
                       manager)
                     else fst (handoff_manager env)
                   in
                   let snapshot =
                     Chat_response.Moderator_manager.identity_snapshot manager
                     |> Result.ok_or_failwith
                   in
                   let source =
                     P.Invocation.
                       { script_id = snapshot.script_id
                       ; source_sha256 = snapshot.script_source_hash
                       }
                   in
                   Actor.change_moderator
                     actor
                     (Some (A.Runtime_builder.encode_moderator_snapshot snapshot))
                   |> protocol_ok
                   |> ignore;
                   let worker =
                     Chat_response.In_memory_stream.
                       { manager
                       ; session_id = P.Id.Session.to_string session_id
                       ; session_meta = `Null
                       ; runtime_policy = Chat_response.Runtime_semantics.default_policy
                       ; event_handlers = None
                       }
                   in
                   worker, source)
               in
               let turn =
                 let initial_notification_input ~capabilities () =
                   if cancel && !dispatches > 0
                   then (
                     signal signal_preparation;
                     Eio.Fiber.await_cancel ());
                   match !wake_registry, moderator with
                   | None, _ | _, None ->
                     Ok Chat_response.In_memory_stream.Safe_point_input.empty
                   | Some current_capabilities, Some (_, source) ->
                     let open Result.Let_syntax in
                     let%bind plan =
                       A.Notification_delivery.prepare_idle
                         ~state:(W.state actor)
                         ~source
                         ~current_capabilities
                         ~policy:Chat_response.One_off_request.default_policy
                         ~max_count:64
                     in
                     capabilities
                       .A.Operation_worker.Capabilities.consume_initial_notifications
                       plan
                 in
                 let successor_guidance =
                   let open Inference_runtime in
                   let module Lease = Protected_delivery.Lease in
                   let source =
                     Lease.Source.create
                       ~host:(P.Id.Server.to_string (P.Session_ref.server_id owner))
                       ~session:(P.Id.Session.to_string (P.Session_ref.session_id owner))
                   in
                   let lease =
                     Lease.create
                       (Lease.Authority.create ())
                       ~source
                       ~scope:
                         { run =
                             (fun f ->
                               incr hint_leases;
                               Actor.Recovery_note_shares.with_source_custody
                                 actor
                                 ~check_owner:(fun () -> Ok ())
                                 ~f
                               |> protocol_ok)
                         }
                   in
                   let hint =
                     Inference.Request.Guidance.create
                       ~text:"local successor fixture hint"
                       ~provenance:"authorized local fixture"
                       ~limits:document_limits
                     |> Result.map_error ~f:(fun _ -> "invalid fixture guidance")
                     |> Result.ok_or_failwith
                   in
                   let prepared =
                     Chat_response.Agent_guidance.Prepared.create
                       ~guidance:(Some hint)
                       ~guard:
                         (Submission_guard.create_scoped
                            ~leases:[ lease ]
                            ~refresh:(fun () ->
                              (match Actor.change_moderator actor None with
                               | Error { code = Conflict; message; _ }
                                 when String.equal
                                        message
                                        "protected source is currently being disclosed" ->
                                 ()
                               | _ ->
                                 failwith
                                   "source lease admitted unrelated moderator change");
                              ignore))
                       ~reserved_input_tokens:8
                     |> protocol_ok
                   in
                   Option.some_if resume (fun ~input:_ ~capabilities:_ ->
                     Chat_response.Agent_guidance.
                       { with_guidance = (fun f -> Ok (f prepared)) })
                 in
                 let after_rollover =
                   Option.some_if
                     finish
                     (fun (view : A.Recovery_rollover_continuation.Hook.view) ->
                        incr hook_calls;
                        assert (not (Scope.equal view.window old_scope));
                        (match view.checkpoint with
                         | `Archive_only -> ()
                         | `Checkpointed ->
                           failwith "forced fixture unexpectedly had a checkpoint");
                        A.Recovery_rollover_continuation.Hook.Finish)
                 in
                 A.Turn_worker.create
                   ?after_rollover
                   ?successor_guidance
                   ~initial_notification_input
                   ~root_binding:
                     (Chat_response.Root_binding.create
                        (Inference_runtime.Session.create ~sw:storage_scope))
                   ~dispatch_tool
                   { config with
                     inference_context = context
                   ; moderator = Option.map moderator ~f:fst
                   ; inference_identity =
                       (fun allocate -> Graph.bind_identity graph ~allocate)
                   ; on_inference_attempt = Graph.on_attempt graph
                   ; on_inference_completion = Graph.on_completion graph
                   ; on_inference_observation = Graph.on_observation graph
                   ; tools
                   ; tool_tbl
                   ; parallel_tool_calls = true
                   ; permission_profile =
                       permission_policy
                         ~tool_default:Allow
                         ~fallback:Fallback_deny
                         ~evaluator:None
                         ~reviewer:None
                   }
               in
               let duplicate = ref None in
               let worker_failure = ref None in
               let worker_exception = ref None in
               let worker =
                 A.Operation_worker.create ~run:(fun ~sw ~input capabilities ->
                   let settle_tool_batch =
                     Option.map capabilities.settle_tool_batch ~f:(fun settle batch ->
                       let result = settle batch in
                       duplicate := Some (fun () -> settle batch);
                       result)
                   in
                   let capabilities = { capabilities with settle_tool_batch } in
                   Exn.protect
                     ~finally:(fun () -> signal worker_return)
                     ~f:(fun () ->
                       let outcome =
                         try A.Operation_worker.run turn ~sw ~input capabilities with
                         | exn ->
                           worker_exception := Some (Exn.to_string exn);
                           raise exn
                       in
                       (match outcome with
                        | Failed error -> worker_failure := Some error
                        | _ -> ());
                       outcome))
               in
               if authored
               then (
                 let selected, _ = Option.value_exn moderator in
                 Actor.set_runtime_worker
                   actor
                   ~moderator_manager:selected.manager
                   ~worker:(Some worker)
                   ~inference:
                     (Some
                        (Inference_client.Execution.create
                           ~context
                           ~identity:(Inference_fixture.identity runtime)
                           ~relation:Root
                           ~before_dispatch:ignore
                           ~on_attempt:ignore
                           ~on_completion:ignore
                           ~on_observation:ignore))
                 |> protocol_ok)
               else Actor.set_operation_worker actor (Some worker) |> protocol_ok;
               (* A client can replay an older completed request while a different
                  operator intent owns a live foreground cut. *)
               let prior_cancel =
                 if equal_scenario scenario Force
                 then (
                   let receipt =
                     Actor.request_context_rollover
                       actor
                       ~actor:operator
                       ~attachment_id:writer
                       ~authorize:permission
                       ~key:(Recovery_note_actor_tests.key "older-cancel")
                       (Disk.expected actor)
                     |> protocol_ok
                   in
                   let cancel () =
                     Actor.settle_context_rollover
                       actor
                       ~actor:operator
                       ~attachment_id:writer
                       ~authorize:permission
                       ~id:receipt.intent.id
                       (Disk.expected actor)
                       Cancel
                     |> protocol_ok
                   in
                   let terminal = cancel () in
                   assert (
                     R.Receipt.equal_status
                       terminal.status
                       (Terminal (Superseded Cancelled)));
                   Some (fun () -> assert (R.Receipt.equal (cancel ()) terminal)))
                 else None
               in
               Actor.start actor ~attachment_id:writer |> protocol_ok |> ignore;
               let entry text =
                 let reserved =
                   Actor.reserve_history_block actor ~count:1 |> protocol_ok
                 in
                 Pending_input_tests.occurrence
                   (Int64.to_int_exn reserved.first_sequence)
                   text
               in
               let send ?timing text =
                 Actor.submit_message_authenticated
                   actor
                   ~actor:proof_for_owners
                   ?timing
                   ~attachment_id:writer
                   (entry text)
                 |> protocol_ok
               in
               let text = "admit the foreground attempt" in
               if expire_original_run
               then (
                 let _, observer = Option.value_exn moderator in
                 let original =
                   Operator_authorization.guarded
                     ~principal:(Operator_authorization.principal proof_for_owners)
                     ~is_current:(fun () -> !original_run_current)
                 in
                 original_run_id
                 := Some
                      (admit_managed_turn
                         actor
                         ~owner
                         ~writer
                         ~original
                         ~observer
                         ~entry:(entry text)
                         text))
               else send text |> ignore;
               Eio.Promise.await first_entered;
               Eio.Promise.await second_returning;
               let get_intent () = pending (W.state actor) |> Option.value_exn in
               let admitted =
                 match scenario with
                 | Native -> get_intent ()
                 | Force | Interrupted | Revoked ->
                   let exact = Disk.expected actor in
                   let receipt =
                     Actor.request_context_rollover
                       actor
                       ~actor:operator
                       ~attachment_id:writer
                       ~authorize:permission
                       ~authorize_force:permission
                       ~key:(Recovery_note_actor_tests.key "live-operator-intent")
                       exact
                     |> protocol_ok
                   in
                   assert (R.Receipt.equal_status receipt.status Pending);
                   if equal_scenario scenario Revoked
                   then (
                     let retry =
                       Actor.request_context_rollover
                         actor
                         ~actor:proof_for_owners
                         ~attachment_id:writer
                         ~authorize:permission
                         ~authorize_force:permission
                         ~key:(Recovery_note_actor_tests.key "live-operator-intent")
                         exact
                       |> protocol_ok
                     in
                     assert (R.Receipt.equal receipt retry));
                   receipt.intent
               in
               Option.iter prior_cancel ~f:(fun retry -> retry ());
               let deferred =
                 send
                   ?timing:
                     (Option.some_if
                        resume
                        P.Pending_input.Timing.After_current_operation)
                   "a new queued input retains its admission"
               in
               assert (
                 match deferred.disposition with
                 | Deferred -> true
                 | _ -> false);
               assert (Option.is_none !archive_ref);
               Eio.Promise.resolve release_first ();
               Eio.Promise.await output_ack_blocked;
               let before_ack =
                 A.Session_persistence.restored persistence
                 |> A.Session_persistence.Restored.state
               in
               assert (List.is_empty (ordinal_outputs before_ack));
               assert (Option.is_none !archive_ref);
               assert (Scope.equal old_scope (W.scope before_ack));
               Eio.Promise.resolve release_ack ();
               Eio.Promise.await worker_returned;
               let after = await_idle actor in
               (match scenario with
                | Native ->
                  assert (
                    R.Receipt.equal_status
                      (Disk.lookup after ~id:admitted.id).status
                      (Terminal (Superseded Checkpoint_required)));
                  assert (Option.is_none !archive_ref);
                  assert (Scope.equal old_scope (W.scope after))
                | Interrupted | Revoked ->
                  assert (Scope.equal old_scope (W.scope after));
                  assert (
                    R.Receipt.equal_status
                      (Disk.lookup after ~id:admitted.id).status
                      Pending)
                | Force ->
                  if Scope.equal old_scope (W.scope after)
                  then
                    raise_s
                      [%sexp
                        "forced rollover did not settle"
                      , (!worker_failure : P.Error.t option)
                      , (!dispatches : int)];
                  assert (List.length after.conversation.canonical_history = 1);
                  assert (List.length after.conversation.deferred_user_entries = 1);
                  assert (Int.equal !dispatches 1));
               if authored then check_authored_wait (Disk.lookup after ~id:admitted.id);
               if equal_scenario scenario Force
               then (
                 let sequence = after.counters.transaction_sequence in
                 let replay = (Option.value_exn !duplicate) () |> protocol_ok in
                 assert (
                   match replay with
                   | Continue -> false
                   | Retire -> true);
                 assert (
                   Int64.equal sequence (W.state actor).counters.transaction_sequence));
               assert (Int.equal !hook_calls (if finish then 1 else 0));
               Option.iter
                 (Option.filter moderator ~f:(fun _ -> resume))
                 ~f:(fun ((worker : Chat_response.In_memory_stream.moderator), source) ->
                   let snapshot =
                     Chat_response.Moderator_manager.identity_snapshot worker.manager
                     |> Result.ok_or_failwith
                   in
                   wake_registry := Some (queue_wake actor ~source ~snapshot));
               if resume || cancel || expire_original_run
               then (
                 let request actor_proof action =
                   Actor.control_context_rollover
                     actor
                     ~actor:actor_proof
                     ~attachment_id:writer
                     ~authorize:permission
                     ~id:admitted.id
                     (Disk.expected actor)
                     action
                 in
                 let control action = request operator action |> protocol_ok in
                 let check_expired_at_writer action reached =
                   let before = W.state actor in
                   let current = ref true in
                   let expiring =
                     Operator_authorization.guarded
                       ~principal:(Operator_authorization.principal operator)
                       ~is_current:(fun () -> !current)
                   in
                   (followup_write
                    := fun candidate -> if reached candidate then current := false);
                   let rejected = request expiring action in
                   followup_write := ignore;
                   if !current then failwith "fixture did not reach canonical preparation";
                   (match rejected with
                    | Error { code = Permission_denied; _ } -> ()
                    | _ -> failwith "expired follow-up authority reached the writer");
                   let after = W.state actor in
                   if
                     not
                       (Int64.equal
                          before.counters.transaction_sequence
                          after.counters.transaction_sequence)
                   then
                     failwith
                       "expired follow-up changed the canonical transaction sequence"
                 in
                 if expire_original_run
                 then (
                   ignore (control Continue : A.Recovery_rollover_continuation.t);
                   let settled = await_idle actor in
                   assert (not !original_run_current);
                   assert !operator_current;
                   [%test_eq: int] 1 !dispatches;
                   (match !worker_failure with
                    | Some { code = Permission_denied; message; _ } ->
                      [%test_eq: string] "Run original authority expired" message
                    | _ ->
                      failwith "the provider guard did not reject original Run expiry");
                   let schedule =
                     Option.value_exn (A.Session_delta.foreground_continuation settled)
                   in
                   check_run_transfer settled schedule (Option.value_exn !original_run_id))
                 else if cancel
                 then (
                   ignore (control Continue : A.Recovery_rollover_continuation.t);
                   Eio.Promise.await successor_preparing;
                   check_expired_at_writer Cancel_followup (fun state ->
                     match state.active_operation with
                     | Some { state = Cancelling; _ } -> true
                     | _ -> false);
                   ignore (control Cancel_followup : A.Recovery_rollover_continuation.t);
                   let stopped = await_idle actor in
                   let phase =
                     (Option.value_exn (A.Session_delta.foreground_continuation stopped))
                       .phase
                   in
                   assert (
                     A.Recovery_rollover_continuation.equal_phase
                       phase
                       (Finished Cancelled));
                   assert (List.length stopped.conversation.deferred_user_entries = 1);
                   assert (Int.equal !dispatches 1))
                 else (
                   let has_phase state phase =
                     Option.exists
                       (A.Session_delta.foreground_continuation state)
                       ~f:(fun current ->
                         A.Recovery_rollover_continuation.equal_phase current.phase phase)
                   in
                   check_expired_at_writer Finish_followup (fun state ->
                     has_phase state (Finished Script_finished));
                   check_expired_at_writer Continue (fun state ->
                     match A.Session_delta.foreground_continuation state with
                     | Some { phase = Preparing _; _ } -> true
                     | _ -> false);
                   assert (Int.equal !dispatches 1);
                   let select model =
                     let before = Actor.configuration actor |> protocol_ok in
                     Actor.update_configuration
                       actor
                       P.Session_configuration.Update_request.
                         { session_id
                         ; attachment_id = writer
                         ; expected_generation = 0
                         ; expected_revision = before.revision
                         ; patch = Configuration_tests.patch model
                         ; idempotency_key = Recovery_note_actor_tests.key model
                         }
                     |> protocol_ok
                     |> ignore
                   in
                   select smaller_model;
                   ignore (control Continue : A.Recovery_rollover_continuation.t);
                   let held = await_idle actor in
                   let held_phase =
                     (Option.value_exn (A.Session_delta.foreground_continuation held))
                       .phase
                   in
                   assert (
                     A.Recovery_rollover_continuation.equal_phase
                       held_phase
                       (Held Capacity));
                   assert (List.length held.conversation.deferred_user_entries = 1);
                   assert (Int.equal !dispatches 1);
                   assert (
                     match (List.hd_exn held.deliveries).wake_disposition with
                     | Some Pending_wake -> true
                     | _ -> false);
                   check_expired_at_writer Wait_for_wake (fun state ->
                     has_phase state Waiting);
                   let before = control Wait_for_wake in
                   assert (
                     A.Recovery_rollover_continuation.equal_phase before.phase Waiting);
                   select (Inference.Request.Target.model target);
                   ignore (control Continue : A.Recovery_rollover_continuation.t);
                   let admitted_state = await_idle actor in
                   if
                     not (List.is_empty admitted_state.conversation.deferred_user_entries)
                   then
                     raise_s
                       [%sexp
                         "successor did not adopt exact FIFO"
                       , (!worker_failure : P.Error.t option)
                       , (!worker_exception : string option)
                       , (!dispatches : int)
                       , (Option.map
                            (A.Session_delta.foreground_continuation admitted_state)
                            ~f:(fun current -> current.phase)
                          : A.Recovery_rollover_continuation.phase option)];
                   assert (Int.equal !dispatches 2);
                   let queue_receipt = control Continue in
                   assert (
                     match queue_receipt.phase with
                     | Admitted { operation; _ } ->
                       (match
                          (List.hd_exn admitted_state.deliveries).wake_disposition
                        with
                        | Some (Accepted_wake accepted) ->
                          P.Id.Operation.equal accepted operation
                        | _ -> false)
                     | _ -> false);
                   assert (Int.equal !dispatches 2);
                   assert (Int.equal !hint_leases 1)));
               Graph.seal graph |> protocol_ok;
               Graph.finish graph |> protocol_ok;
               admitted.id, old_scope, deferred.history_id)
        in
        let reopened = W.journal disk Agent_store.Journal.open_existing in
        let recovered = Disk.recovery disk ~journal:reopened ~initial in
        let state = A.Session_persistence.Restored.state recovered.state in
        if authored
        then (
          check_authored_wait (Disk.lookup state ~id);
          check_authored_claim_prefixes ~initial ~transactions:recovered.transactions ~id);
        match scenario with
        | Native ->
          assert (
            R.Receipt.equal_status
              (Disk.lookup state ~id).status
              (Terminal (Superseded Checkpoint_required)))
        | Force | Interrupted | Revoked ->
          let archive =
            W.read_archive disk (Option.value_exn !archive_ref) |> protocol_ok
          in
          let call_ids =
            List.filter_map archive.conversation.canonical_history ~f:(fun entry ->
              if P.History.equal_kind entry.kind Tool_call then Some entry.id else None)
          in
          assert (List.length call_ids = 2);
          let observed_ids =
            List.map (ordinal_outputs archive) ~f:(fun id ->
              let row =
                List.find_exn archive.conversation.canonical_history ~f:(fun entry ->
                  P.History.Id.equal entry.id id)
                |> A.History_codec.of_protocol
                |> protocol_ok
              in
              match
                History_entry.Payload.(
                  Semantic.view (semantic (History_entry.payload row)))
              with
              | Result { relation = Bound id; _ } -> id
              | _ -> failwith "archive lost canonical bound result")
          in
          assert (List.equal P.History.Id.equal call_ids observed_ids);
          assert (Scope.equal old_scope (W.scope archive));
          assert (
            Bool.equal
              (not (resume || expire_original_run))
              (List.exists state.conversation.deferred_user_entries ~f:(fun pending ->
                 P.History.Id.equal (A.Pending_input_document.entry pending).id pending_id)));
          if resume || expire_original_run
          then
            assert (
              List.count state.conversation.canonical_history ~f:(fun entry ->
                P.History.Id.equal entry.id pending_id)
              = 1);
          (match scenario, (Disk.lookup state ~id).status with
           | ( Force
             , Terminal
                 (Committed
                    { checkpoint = Archive_only Checkpoint_missing
                    ; continuation = Scheduled_foreground schedule
                    ; _
                    }) ) ->
             assert (P.Id.Operation.equal schedule.owner id);
             assert (not (P.Id.Operation.equal schedule.owner schedule.predecessor));
             assert (
               match schedule.phase with
               | Admitted { operation; _ } when resume ->
                 (match (List.hd_exn state.deliveries).wake_disposition with
                  | Some (Accepted_wake accepted) ->
                    P.Id.Operation.equal accepted operation
                  | _ -> false)
               | Admitted _ when expire_original_run ->
                 check_run_transfer state schedule (Option.value_exn !original_run_id);
                 true
               | Waiting when not (resume || finish || cancel || expire_original_run) ->
                 true
               | Finished Script_finished when finish -> true
               | Finished Cancelled when cancel -> true
               | _ -> false);
             assert (List.length state.conversation.compaction_archives = 1);
             assert (Option.is_none state.active_operation);
             let active = Option.value_exn archive.active_operation in
             assert (P.Id.Operation.equal active.id schedule.predecessor);
             assert (
               match active.state with
               | Running -> true
               | _ -> false);
             Disk.check_turn_starting archive schedule.predecessor;
             Disk.check_turn_retired state schedule.predecessor;
             Disk.check_rollover_retirement
               recovered.transactions
               state
               schedule.predecessor;
             check_rollover_replay ~initial recovered.transactions (Disk.lookup state ~id)
           | (Interrupted | Revoked), Pending ->
             assert (Scope.equal old_scope (W.scope state));
             assert (List.is_empty state.conversation.compaction_archives)
           | _ -> failwith "reopen did not preserve the actual batch disposition"))))
;;

let%expect_test
    "foreground rollover waits for both actual tool output ACKs and preserves restart"
  =
  List.iter [ Native; Force; Interrupted; Revoked ] ~f:run;
  print_endline
    "native checkpoint required; forced cut archived two ordered ACKs; synced orphan and \
     revoked writer retained pending";
  [%expect
    {| native checkpoint required; forced cut archived two ordered ACKs; synced orphan and revoked writer retained pending |}]
;;

let%expect_test "fresh successor admission holds capacity without consuming staged input" =
  run ~resume:true Force;
  run ~finish:true Force;
  run ~cancel:true Force;
  print_endline
    "fresh successor accepted queued input and wake once; held, finished and cancelled \
     paths kept custody";
  [%expect
    {| fresh successor accepted queued input and wake once; held, finished and cancelled paths kept custody |}]
;;

let%expect_test "installed moderator authorizes and holds a real operator rollover" =
  run ~authored_followup:Wait Force;
  print_endline "both durable authored phases survived operator replay";
  [%expect {| both durable authored phases survived operator replay |}]
;;

let%expect_test "a transferred Run can expire before its successor provider submission" =
  run ~authored_followup:Expire_original_run Force;
  print_endline "the durable Run transfer survived without a successor provider request";
  [%expect {| the durable Run transfer survived without a successor provider request |}]
;;
