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
    A.Script_tool_calls.with_recovery_context_service tools { remaining; new_window }
  in
  let dispatch_tool ~input ~capabilities =
    A.Script_tool_calls.native_dispatch service ~declared ~input ~capabilities
  in
  Chat_response.Tool.convert_tools tools, tool_tbl, dispatch_tool
;;

let reject_reordered_replay ~initial transactions (receipt : R.Receipt.t) =
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
      receipt.intent
      ~expected:change.expected
      ~successor:change.successor
      ~archive:change.archive
      ~checkpoint:change.checkpoint
      ~retain:change.retain
    |> protocol_ok
  in
  ignore
    (A.Session_delta.apply old_state (Recovery_rollover_committed change) |> protocol_ok
     : A.Session_state.t);
  match A.Session_delta.apply old_state (Recovery_rollover_committed reordered) with
  | Error actual when P.Error.equal_code actual.code Conflict -> ()
  | Error actual ->
    raise_s [%sexp "unexpected replay validation failure", (actual : P.Error.t)]
  | Ok _ -> failwith "replay accepted reordered output acknowledgements"
;;

let run scenario =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun storage_scope ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
        let first_entered, enter_first = Eio.Promise.create () in
        let second_returning, return_second = Eio.Promise.create () in
        let wait_first, release_first = Eio.Promise.create () in
        let output_ack_blocked, block_ack = Eio.Promise.create () in
        let wait_ack, release_ack = Eio.Promise.create () in
        let worker_returned, worker_return = Eio.Promise.create () in
        let window = ref None in
        let dispatches = ref 0 in
        let first = ref true in
        let events ~sw:_ ~inputs:_ =
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
          | _ -> failwith "retiring worker dispatched another request"
        in
        let runtime =
          Inference_fixture.create_with_capacity
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
        let before_commit prepublish =
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
                   ~targets:[ C.exact_target_policy target, `Automatic ]
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
               let turn =
                 A.Turn_worker.create
                   ~root_binding:
                     (Chat_response.Root_binding.create
                        (Inference_runtime.Session.create ~sw:storage_scope))
                   ~dispatch_tool
                   { config with
                     inference_context = context
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
                         A.Operation_worker.run turn ~sw ~input capabilities
                       in
                       (match outcome with
                        | Failed error -> worker_failure := Some error
                        | _ -> ());
                       outcome))
               in
               Actor.set_operation_worker actor (Some worker) |> protocol_ok;
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
               let send text =
                 let reserved =
                   Actor.reserve_history_block actor ~count:1 |> protocol_ok
                 in
                 Actor.submit_message_authenticated
                   actor
                   ~actor:proof_for_owners
                   ~attachment_id:writer
                   (Pending_input_tests.occurrence
                      (Int64.to_int_exn reserved.first_sequence)
                      text)
                 |> protocol_ok
               in
               send "admit the foreground attempt" |> ignore;
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
               let deferred = send "a new queued input retains its admission" in
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
                  assert (List.is_empty after.conversation.canonical_history);
                  assert (List.length after.conversation.deferred_user_entries = 1);
                  assert (Int.equal !dispatches 1));
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
               Graph.seal graph |> protocol_ok;
               Graph.finish graph |> protocol_ok;
               admitted.id, old_scope, deferred.history_id)
        in
        let reopened = W.journal disk Agent_store.Journal.open_existing in
        let recovered = Disk.recovery disk ~journal:reopened ~initial in
        let state = A.Session_persistence.Restored.state recovered.state in
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
            List.exists state.conversation.deferred_user_entries ~f:(fun pending ->
              P.History.Id.equal (A.Pending_input_document.entry pending).id pending_id));
          (match scenario, (Disk.lookup state ~id).status with
           | ( Force
             , Terminal
                 (Committed
                    { checkpoint = Archive_only Checkpoint_missing
                    ; continuation = Awaiting_settled_foreground { owner; predecessor }
                    ; _
                    }) ) ->
             assert (P.Id.Operation.equal owner id);
             assert (not (P.Id.Operation.equal owner predecessor));
             assert (List.length state.conversation.compaction_archives = 1);
             assert (Option.is_none state.active_operation);
             reject_reordered_replay
               ~initial
               recovered.transactions
               (Disk.lookup state ~id)
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
