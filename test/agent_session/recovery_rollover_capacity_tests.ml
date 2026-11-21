open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module R = A.Recovery_rollover
module Actor = A.Session_actor
module Graph = Agent_server.Graph_tracking
module Runtime = Inference_runtime
module Observation = Inference.Observation
module Capacity = Chat_response.Capacity_rollover
module W = Recovery_window_actor_tests
module Disk = Recovery_rollover_commit_tests
module T = Inference.Event.Terminal

let ok = function
  | Ok value -> value
  | Error _ -> failwith "invalid capacity-cut fixture evidence"
;;

type scenario =
  | Hard
  | Rejected
  | Manual
[@@deriving equal]

let measured tokens = Observation.Count.create (Actual tokens) |> ok

let response ~scope ~accounting_id ~outcome =
  let terminal = T.create ~scope ~delivery:Response_started ~outcome |> ok in
  let unknown = Observation.Count.create (Unknown Not_reported) |> ok in
  let usage =
    Observation.Usage.create
      ~counts:
        { input = measured 100L
        ; output = unknown
        ; reported_total = unknown
        ; cached_input = unknown
        ; cache_write_input = unknown
        ; reasoning_output = unknown
        }
      ~inclusions:[]
    |> ok
  in
  let usage =
    Observation.create
      ~scope
      ~id:accounting_id
      ~revision:0L
      ~payload:(Usage usage)
      ~limits:Observation.Admission.observation
    |> ok
  in
  Runtime.Receipt.create
    ~terminal
    ~usage
    ~output:[]
    ~output_coverage:Response_output
    ~limits:Runtime.Limits.default
  |> ok
;;

let target () =
  Inference.Request.Target.create
    ~adapter:"capacity-cut-fixture"
    ~profile:"fixture"
    ~profile_revision:(Some "1")
    ~account:None
    ~endpoint:"local"
    ~model:"capacity-cut"
    ~settings:[]
    ~limits:document_limits
  |> ok
;;

let context target (old : P.History.entry) scenario writes =
  let prepare ~preparation_id request =
    let configuration =
      Observation.Configuration.of_target
        (Inference.Request.target request)
        ~preparation_id
        ~transport:In_process
        ~capabilities:[]
        ~limits:Observation.Admission.observation
      |> ok
    in
    let original =
      List.exists (Inference.Request.history request) ~f:(fun entry ->
        String.equal
          (History_entry.Id.to_string (History_entry.id entry))
          (P.History.Id.to_string old.id))
    in
    let run
          ~sw:_
          ~scope
          ~accounting_id
          ~note_delivery
          ~on_event:_
          ~on_observation:_
          ~submission_guard
      =
      Runtime.Submission_guard.with_submission submission_guard ~f:(fun () ->
        writes := original :: !writes);
      note_delivery T.Response_started;
      let outcome =
        if original && equal_scenario scenario Rejected
        then T.Failed (Provider Context_window_exceeded)
        else Completed
      in
      response ~scope ~accounting_id ~outcome
    in
    Runtime.Plan.create ~request ~configuration ~fingerprint:"private-fixture" ~run
    |> Result.map ~f:(fun plan ->
      let count =
        if original && not (equal_scenario scenario Rejected) then 450L else 100L
      in
      Runtime.Plan.with_capacity_source
        plan
        (Runtime.Context_capacity.Source.create
           ~encoder:Openai_responses_v1
           ~read:(fun () -> Covered (measured count))))
  in
  Runtime.Adapter.create
    ~id:(Inference.Request.Target.adapter target)
    ~limits:Runtime.Limits.default
    ~supports_request_guidance:false
    ~preflight_history:(fun ~target:_ _ -> Ok ())
    ~bind:(fun _ -> Ok ())
    ~prepare
    ()
  |> ok
  |> Runtime.Context.create ~target
  |> ok
;;

exception Retired

let worker scenario ~previous ~actor ~original_operation ~on_cut ~record_failure =
  let ordinal = ref 0 in
  let run
        ~sw
        ~(input : A.Operation_worker.Input.t)
        (caps : A.Operation_worker.Capabilities.t)
    : A.Operation_worker.outcome
    =
    let successor = caps.successor_inference in
    if Option.is_none successor then original_operation := Some input.operation.id;
    let upstream =
      { (Inference_tracking_tests.upstream (fun ~scope:_ ~accounting_id:_ -> ())) with
        new_preparation_id =
          (fun () ->
            incr ordinal;
            sprintf "capacity-cut-%d" !ordinal)
      }
    in
    let graph =
      Graph.create actor ~source:(Inference_tracking_tests.source "foreground") ~upstream
      |> protocol_ok
    in
    let cut cap ~trigger ~stage =
      let retired = function
        | Chat_response.Settled_tool_batch.Retire -> ()
        | Continue -> failwith "qualified cut returned Continue"
      in
      cap.Capacity.request ~trigger ~stage |> protocol_ok |> retired;
      let sequence = (W.state actor).counters.transaction_sequence in
      cap.request ~trigger ~stage |> protocol_ok |> retired;
      Recovery_note_actor_tests.expect_error
        Conflict
        (cap.request ~trigger:Soft_input_limit ~stage);
      assert (Int64.equal sequence (W.state actor).counters.transaction_sequence);
      on_cut (W.state actor);
      raise Retired
    in
    let dispatch () =
      let identity = Graph.bind_identity graph ~allocate:caps.allocate_inference in
      let root = Option.value_exn caps.root_context in
      let actual = ref None in
      let submission_guard =
        Option.map successor ~f:(fun successor ->
          Runtime.Submission_guard.create_admission ~refresh:(fun () ->
            successor.guard (Option.value_exn !actual)))
      in
      root.with_context
        ~previous
        ~history:input.history
        (fun context ~select_checkpoint:_ ~select_prepared ~on_prepared ~on_dispatch ->
           let request =
             Inference.Request.create
               ~target:(Runtime.Context.target context)
               ~history:input.history
               ~tools:[]
               ~assets:[]
               ~limits:Transcript.Admission.default
             |> ok
           in
           let cap = caps.capacity_rollover in
           let before_dispatch prepared =
             Option.iter successor ~f:(fun successor ->
               successor.check_prepared ~optional_hint:false prepared);
             on_prepared prepared;
             match scenario, successor, cap with
             | Hard, None, Some cap ->
               let stage = Capacity.Unsent prepared in
               (match cap.inspect stage |> protocol_ok with
                | Hard_limit -> cut cap ~trigger:Hard_input_limit ~stage
                | _ -> failwith "prepared pressure did not qualify")
             | Manual, None, None | Rejected, None, Some _ | _, Some _, None -> ()
             | _ -> failwith "wrong capacity port for foreground"
           in
           let receipt =
             Inference_client.run
               context
               ~sw
               ~identity
               ~relation:Root
               ~request
               ?select_prepared
               ?submission_guard
               ~before_dispatch
               ~on_attempt:(fun attempt ->
                 on_dispatch (Runtime.Attempt.configuration attempt);
                 Graph.on_attempt graph attempt;
                 actual := Some attempt)
               ~on_completion:(Graph.on_completion graph)
               ~on_event:ignore
               ~on_observation:(Graph.on_observation graph)
             |> ok
           in
           match scenario, successor, cap with
           | Rejected, None, Some cap ->
             let attempt = Option.value_exn !actual in
             let rejection = Capacity.rejection ~attempt ~receipt |> Option.value_exn in
             let stage = Capacity.Rejected rejection in
             (match cap.inspect stage |> protocol_ok with
              | Qualified_rejection -> cut cap ~trigger:Provider_context_limit ~stage
              | _ -> failwith "actual receipt did not qualify")
           | _, Some _, None -> ()
           | _ -> failwith "original root submitted unexpectedly")
    in
    match
      Exn.protect ~f:dispatch ~finally:(fun () ->
        Graph.seal graph |> protocol_ok;
        Graph.finish graph |> protocol_ok)
    with
    | () ->
      Completed
        { final_history = input.history
        ; runtime_requests = []
        ; moderator_snapshot = None
        }
    | exception Retired ->
      caps.select_rollover_followup None |> protocol_ok;
      Cancelled { reason = "retired by capacity cut" }
    | exception A.Native_tool_dispatch.Dispatch_error error
      when equal_scenario scenario Manual ->
      Recovery_note_actor_tests.expect_error Resource_limit (Error error);
      Failed error
  in
  let run ~sw ~input caps =
    try run ~sw ~input caps with
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      record_failure (exn, backtrace);
      Stdlib.Printexc.raise_with_backtrace exn backtrace
  in
  if equal_scenario scenario Manual
  then A.Operation_worker.create ~run
  else A.Operation_worker.create_capacity_aware ~run
;;

let check_receipt ~initial_scope ~scenario ~original_operation ~archive state =
  let ledger = Option.value_exn state.A.Session_state.recovery_rollover in
  let receipt =
    R.automatic_for_operation (Some ledger) ~operation:original_operation
    |> Option.value_exn
  in
  assert (P.Recovery_window.Scope.equal initial_scope receipt.intent.expected.scope);
  (match receipt.intent.requester with
   | Automatic { principal; cut } ->
     assert (P.Id.Principal.equal principal principal_id);
     assert (P.Id.Operation.equal cut.operation original_operation);
     (match scenario, cut.trigger, cut.boundary with
      | Hard, Hard_input_limit, Unsent -> assert (Int64.equal cut.attempt_high_water 0L)
      | Rejected, Provider_context_limit, Rejected ->
        assert (Int64.equal cut.attempt_high_water 1L)
      | _ -> assert false)
   | Operator _ | Native _ -> assert false);
  match receipt.status with
  | Terminal
      (Committed
         { successor; archive = committed_archive; checkpoint = Archive_only _; _ }) ->
    assert (P.Recovery_window.Scope.equal successor (W.scope state));
    assert (
      Chatml_source.Digest.equal
        committed_archive
        (Chatml_source.Digest.of_hex archive.A.Session_state.Compaction_archive.sha256
         |> ok))
  | Terminal _ | Pending -> assert false
;;

let rec successor_dispatch restored = function
  | [] -> failwith "missing physical successor admission"
  | (transaction : Agent_store.Transaction.t) :: later ->
    let delta =
      A.Session_delta_document.decode ~limits:document_limits transaction.delta
      |> document_ok
      |> A.Session_delta_document.value
    in
    let changes =
      match delta with
      | Batch changes -> changes
      | leaf -> [ leaf ]
    in
    (match
       List.find_map changes ~f:(function
         | A.Session_delta.Recovery_rollover_changed
             (Schedule { change = Admit { operation; _ }; _ }) -> Some operation
         | _ -> None)
     with
     | Some operation ->
       A.Session_persistence.Restored.state restored, changes, operation, transaction
     | None ->
       let restored =
         A.Session_persistence.apply_transaction
           ~limits:document_limits
           restored
           transaction
         |> store_ok
       in
       successor_dispatch restored later)
;;

let retained_run state ~predecessor =
  let generation = state.A.Session_state.identity.generation in
  let source =
    A.Run_source_installation.captured Run_state_tests.installed ~generation
    |> protocol_ok
  in
  let owned = P.Run_work.create ~key:(Operation predecessor) ~generation |> protocol_ok in
  let run =
    P.Run.create
      ~id:(P.Id.Run.create ())
      ~session:(P.Recovery_window.Scope.session (W.scope state))
      ~principal_id
      ~source
      ~mode:Single_turn
      ~lifecycle:Admitted
      ~revision:0L
      ~owned_work:[ owned ]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:state.identity.created_at
      ~updated_at:state.identity.updated_at
    |> protocol_ok
  in
  let receipt =
    P.Run_receipt.create
      ~run_id:run.id
      ~principal_id
      ~source
      ~key:(Recovery_note_actor_tests.key "rollover-run")
      ~request_sha256:(String.make 64 '1')
      ~kind:Admission
      ~run_revision:run.revision
      ~session_revision:state.counters.revision
      ~committed_at:state.identity.updated_at
    |> protocol_ok
  in
  let index =
    A.Run_state.commit Run_state_tests.index ~run ~receipt ~intent:None |> protocol_ok
  in
  { state with run_state = Some index }, run
;;

let check_run_transfer initial transactions =
  let state, changes, successor, physical = successor_dispatch initial transactions in
  let predecessor =
    (Option.value_exn (A.Session_delta.foreground_continuation state)).predecessor
  in
  let before, run = retained_run state ~predecessor in
  let now = before.identity.updated_at in
  let plan ~successor =
    let id, index =
      A.Run_rollover_work.prepare before ~predecessor ~successor ~now
      |> protocol_ok
      |> Option.value_exn
    in
    assert (P.Id.Run.equal id run.id);
    A.Session_delta.Run_state_changed index
  in
  let restored = A.Session_persistence.Restored.authored before in
  let document = A.Session_persistence.Restored.state_document restored in
  let prepare delta = A.Session_transition.apply ~now before ~delta ~payloads:[] in
  let transaction delta (candidate : A.Session_transition.t) =
    let payloads = List.map candidate.events ~f:event_document in
    Agent_store.Transaction.create
      ~limits:document_limits
      ~session_id:before.identity.session_id
      ~generation:before.identity.generation
      ~transaction_sequence:candidate.state.counters.transaction_sequence
      ~previous_transaction_hash:physical.previous_transaction_hash
      ~session_revision:candidate.state.counters.revision
      ~first_event_sequence:
        (Option.map (List.hd candidate.events) ~f:(fun event -> event.sequence))
      ~last_event_sequence:
        (Option.map (List.last candidate.events) ~f:(fun event -> event.sequence))
      ~accepted_at_ns:physical.accepted_at_ns
      ~command_audit:None
      ~delta:(delta_document delta)
      ~durable_events:payloads
    |> store_ok
  in
  let replay candidate delta =
    A.Session_persistence.apply_transaction
      ~limits:document_limits
      restored
      (transaction delta candidate)
  in
  let legacy = prepare (Batch changes) |> protocol_ok in
  let historical =
    replay legacy legacy.delta |> store_ok |> A.Session_persistence.Restored.state
  in
  assert (Option.equal A.Run_state.equal before.run_state historical.run_state);
  let move = plan ~successor in
  let qualified = prepare (Batch (changes @ [ move ])) |> protocol_ok in
  ignore
    (A.Session_document_transition.admit_encoded
       document
       ~delta:qualified.delta
       ~next:qualified.state
       ~limits:document_limits
     |> document_ok
     : A.Session_state_document.Admitted.t);
  let adopted =
    replay qualified qualified.delta |> store_ok |> A.Session_persistence.Restored.state
  in
  assert (Option.equal A.Run_state.equal qualified.state.run_state adopted.run_state);
  let index = Option.value_exn adopted.run_state in
  let settled = A.Run_state.find index run.id |> Option.value_exn in
  let work =
    P.Run_work.create ~key:(Operation successor) ~generation:run.source.generation
    |> protocol_ok
  in
  assert (List.equal P.Run_work.equal settled.owned_work [ work ]);
  assert (List.equal P.Run_work.equal settled.relinquished_work run.owned_work);
  assert (List.is_empty settled.terminal_work);
  assert (
    List.equal
      P.Run_receipt.equal
      (A.Run_state.receipts index)
      (A.Run_state.receipts (Option.value_exn before.run_state)));
  assert (Result.is_error (A.Run_rollover_work.dispatch_owner adopted ~predecessor));
  List.iter
    [ move
    ; Batch (move :: changes)
    ; Batch (changes @ [ Run_state_changed (Option.value_exn before.run_state) ])
    ; Batch (changes @ [ plan ~successor:(P.Id.Operation.create ()) ])
    ]
    ~f:(fun delta ->
      assert (Result.is_error (prepare delta));
      assert (Result.is_error (replay qualified delta)))
;;

let exercise scenario =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun storage_scope ->
      let old = W.history 0 "archived original corpus" in
      let writes = ref [] in
      let target = target () in
      let previous = context target old scenario writes in
      let policy = Recovery_rollover_tests.exact_target_policy target in
      let base =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let inference_target =
        Inference.Selection.captured target ~limits:document_limits
        |> Configuration_tests.inference_ok
      in
      let initial =
        A.Session_state.create
          ~identity:base.identity
          ~spec:{ base.spec with inference_target }
          ~initial_history:[ old ]
      in
      let initial =
        { initial with
          conversation = { initial.conversation with initial_prompt_entry_count = 0 }
        }
      in
      let storage = Job_artifact_fixtures.create env storage_scope initial in
      let owner =
        P.Session_ref.create
          ~server_id:(Agent_store.Session_store.server_id storage.sessions)
          ~session_id
      in
      let disk : W.fixture =
        { env; sessions = storage.sessions; handle = storage.session; owner }
      in
      let initial = A.Session_persistence.Restored.authored initial in
      let archive =
        W.Archive.write_document
          ~env
          ~handle:disk.handle
          ~max_payload_length:W.payload_limit
      in
      let original_operation = ref None in
      let after_cut = ref None in
      let failure = ref None in
      let scope, final =
        Eio.Switch.run (fun sw ->
          let journal = W.journal disk Agent_store.Journal.create in
          W.with_actor
            disk
            sw
            journal
            ~restored:initial
            ~previous_transaction_hash:None
            ~archive
            (fun actor _ ->
               let attachment_id = W.attach actor in
               let scope =
                 Actor.enable_recovery_windows
                   actor
                   ~actor:(W.actor_proof ())
                   ~attachment_id
                   ~authorize:Recovery_rollover_tests.allow
                 |> protocol_ok
               in
               let host =
                 A.Session_context_capacity.Host.create
                   ~mode_for_session:(fun id ->
                     Option.some_if
                       (P.Session_ref.equal id owner)
                       A.Session_context_capacity.Mode.Automatic)
                   ~targets:[ policy, `Automatic ]
                 |> ok
               in
               Actor.set_context_capacity_host actor host |> protocol_ok;
               let configuration_policy =
                 A.Configuration_policy.
                   { select_profile =
                       (fun ~current:_ ~profile:_ ->
                         Error (P.Error.invalid_request "fixed target"))
                   ; approve = (fun ~current:_ ~proposed:_ -> Ok ())
                   ; resolve =
                       (fun selected ->
                         if Inference.Request.Target.equal target selected
                         then Ok previous
                         else Error (P.Error.invalid_request "wrong target"))
                   }
               in
               Actor.set_configuration_policy actor configuration_policy |> protocol_ok;
               Actor.set_operation_worker
                 actor
                 (Some
                    (worker
                       scenario
                       ~previous
                       ~actor
                       ~original_operation
                       ~on_cut:(fun state -> after_cut := Some state)
                       ~record_failure:(fun exn -> failure := Some exn)))
               |> protocol_ok;
               Actor.start actor ~attachment_id |> protocol_ok |> ignore;
               let reserved = Actor.reserve_history_block actor ~count:1 |> protocol_ok in
               let input =
                 Pending_input_tests.occurrence
                   (Int64.to_int_exn reserved.first_sequence)
                   "retained original user input"
               in
               Actor.submit_message_authenticated
                 actor
                 ~actor:(W.actor_proof ())
                 ~attachment_id
                 input
               |> protocol_ok
               |> ignore;
               let rec finished () =
                 Option.iter !failure ~f:(fun (exn, trace) ->
                   Stdlib.Printexc.raise_with_backtrace exn trace);
                 let state = W.state actor in
                 let settled =
                   match scenario, A.Session_delta.foreground_continuation state with
                   | Manual, _ -> true
                   | ( (Hard | Rejected)
                     , Some { phase = Waiting | Held _ | Admitted _ | Finished _; _ } ) ->
                     true
                   | (Hard | Rejected), _ -> false
                 in
                 if settled && Option.is_none state.active_operation
                 then state
                 else (
                   Eio.Fiber.yield ();
                   finished ())
               in
               let final =
                 Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. finished
               in
               scope, final))
      in
      let journal = W.journal disk Agent_store.Journal.open_existing in
      let recovered = Disk.recovery disk ~journal ~initial in
      let replay = A.Session_persistence.Restored.state recovered.state in
      match scenario with
      | Manual ->
        assert (List.is_empty !writes);
        assert (Option.is_none !after_cut);
        assert (P.Recovery_window.Scope.equal scope (W.scope replay));
        assert (Option.is_none replay.recovery_rollover)
      | Hard | Rejected ->
        if equal_scenario scenario Hard
        then check_run_transfer initial recovered.transactions;
        [%test_eq: bool list]
          (List.rev !writes)
          (if equal_scenario scenario Hard then [ false ] else [ true; false ]);
        let after_cut = Option.value_exn !after_cut in
        let archive = List.hd_exn after_cut.conversation.compaction_archives in
        let original_operation = Option.value_exn !original_operation in
        List.iter
          [ after_cut; final; replay ]
          ~f:(check_receipt ~scenario ~archive ~original_operation ~initial_scope:scope);
        let saved =
          W.Archive.read
            ~env
            ~handle:disk.handle
            ~max_payload_length:W.payload_limit
            archive
          |> protocol_ok
        in
        let active = Option.value_exn saved.active_operation in
        assert (P.Id.Operation.equal active.id original_operation);
        assert (
          match active.state with
          | Running -> true
          | _ -> false);
        Disk.check_turn_starting saved original_operation;
        List.iter [ after_cut; final; replay ] ~f:(fun state ->
          Disk.check_turn_retired state original_operation);
        Disk.check_attempts_preserved saved after_cut;
        Disk.check_attempts_preserved final replay;
        [%test_eq: int64]
          (A.Inference_ledger.last_attempt_ordinal saved.inference_ledger)
          (if equal_scenario scenario Hard then 0L else 1L);
        Disk.check_rollover_retirement recovered.transactions replay original_operation;
        assert (
          List.exists saved.conversation.canonical_history ~f:(fun entry ->
            P.History.Id.equal old.id entry.id));
        assert (
          not
            (List.exists replay.conversation.canonical_history ~f:(fun entry ->
               P.History.Id.equal old.id entry.id)));
        [%test_eq: int] (List.length replay.conversation.canonical_history) 1))
;;

let%test_unit
    "oversize prepared root seals before allocation and dispatches a fresh successor"
  =
  exercise Hard
;;

let%test_unit "structured safe root rejection seals only after exact completion" =
  exercise Rejected
;;

let%test_unit "unaware worker keeps hard gate with no automatic journal entry" =
  exercise Manual
;;
