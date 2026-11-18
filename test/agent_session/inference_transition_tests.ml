open! Core
open Fixtures
module S = Agent_session.Session_state
module T = Agent_session.Session_transition
module D = Agent_session.Session_delta
module P = Agent_protocol
module L = Agent_session.Inference_ledger

let initial workspace_instance =
  actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
;;

let%expect_test
    "session update payload uses final counters after original domain admission"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let original = initial workspace_instance in
    let original =
      { original with
        counters =
          { original.counters with
            revision = 10L
          ; transaction_sequence = 10L
          ; event_sequence = 20L
          }
      }
    in
    let transition =
      T.apply
        ~now:timestamp
        original
        ~delta:(D.Batch [])
        ~payloads:[ Session_updated (S.summary original) ]
      |> protocol_ok
    in
    let event = List.hd_exn transition.events in
    let summary =
      match
        P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok
      with
      | Session_updated summary -> summary
      | _ -> assert false
    in
    assert (
      Int64.equal summary.revision 11L && Int64.equal summary.latest_event_sequence 21L);
    assert (
      Int64.equal event.revision summary.revision
      && Int64.equal event.sequence summary.latest_event_sequence);
    let invalid = { (S.summary original) with revision = -1L } in
    assert (
      Result.is_error
        (T.apply
           ~now:timestamp
           original
           ~delta:(D.Batch [])
           ~payloads:[ Session_updated invalid ]));
    let foreign = { (S.summary original) with id = P.Id.Session.create () } in
    assert (
      Result.is_error
        (T.apply
           ~now:timestamp
           original
           ~delta:(D.Batch [])
           ~payloads:[ Session_updated foreign ]));
    printf "payload/envelope=11/21; malformed and foreign originals reject\n");
  [%expect {| payload/envelope=11/21; malformed and foreign originals reject |}]
;;

let operation kind state =
  P.Operation.
    { id = P.Id.Operation.create ()
    ; generation = 0
    ; kind
    ; state
    ; started_at = timestamp
    ; updated_at = timestamp
    }
;;

let start state operation =
  T.apply
    ~now:timestamp
    state
    ~delta:
      (D.Batch
         [ Active_operation_changed (Some operation)
         ; Lifecycle_changed
             { desired = Running; observed = Running_turn operation.P.Operation.id }
         ])
    ~payloads:[ Operation_started operation ]
  |> protocol_ok
;;

let complete state operation =
  let operation = { operation with P.Operation.state = Completed } in
  T.apply
    ~now:timestamp
    state
    ~delta:
      (D.Batch
         [ Active_operation_changed None
         ; Lifecycle_changed { desired = Running; observed = Idle }
         ])
    ~payloads:[ Operation_completed operation ]
  |> protocol_ok
;;

let%expect_test
    "actual Turn occurrence and terminal receipt share real actor transactions"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let original = initial workspace_instance in
    let op = operation (Turn User_submit) Running in
    let admitted = start original op in
    assert (
      Int64.equal
        (P.Inference_query.Summary.turns (L.summary admitted.state.inference_ledger))
          .pending
        1L);
    let ended = { op with P.Operation.state = Completed } in
    assert (
      Result.is_error
        (T.apply
           ~now:timestamp
           admitted.state
           ~delta:(D.Batch [])
           ~payloads:[ Operation_completed ended ]));
    assert (
      Result.is_error
        (T.apply
           ~now:timestamp
           admitted.state
           ~delta:(D.Active_operation_changed None)
           ~payloads:[ Operation_completed op ]));
    assert (
      Result.is_error
        (T.apply
           ~now:timestamp
           admitted.state
           ~delta:(D.Active_operation_changed None)
           ~payloads:[ Operation_cancelled ended ]));
    let committed = complete admitted.state op in
    let turns =
      P.Inference_query.Summary.turns (L.summary committed.state.inference_ledger)
    in
    assert (Int64.equal turns.completed 1L && Int64.equal turns.pending 0L);
    let replay = D.apply admitted.state committed.delta |> protocol_ok in
    assert (
      Int64.equal
        (P.Inference_query.Summary.turns (L.summary replay.inference_ledger)).completed
        1L);
    let duplicate = complete committed.state op in
    assert (
      Int64.equal
        (P.Inference_query.Summary.turns (L.summary duplicate.state.inference_ledger))
          .completed
        1L);
    let compact = operation Compaction Running in
    let compacted =
      start committed.state compact |> fun (next : T.t) -> complete next.state compact
    in
    assert (
      Int64.equal
        (P.Inference_query.Summary.turns (L.summary compacted.state.inference_ledger))
          .completed
        1L);
    printf
      "one actual retained turn; terminal/replay exact; stale delivery and compaction \
       add none\n");
  [%expect
    {| one actual retained turn; terminal/replay exact; stale delivery and compaction add none |}]
;;

let%expect_test
    "inherited Turn hook consumes one actual admission after earlier canonical leaves"
  =
  let module GD = Agent_session.Goal_descendants in
  let module Occurrence = Agent_session.Goal_descendant_occurrence in
  let module Pending = Agent_session.Goal_descendant_pending in
  let module Included = Agent_session.Goal_descendant_included in
  let module Input = Agent_session.Host_turn_tracking.Inherited in
  let module Depth = Descendant_depth_state_tests in
  (* Pure State/transition evidence only. Pending DATA below does not claim a
     root runtime reservation ACK or authorize any child effect. The Turn's
     canonical Handle is obtained ONLY by the real Host_turn_tracking owner. *)
  Goal_run_tests.with_model
    (fun ~basis:_ ~held:_ ~admission:_ ~intent:_ ~delta:_ parent ->
       let reference =
         Depth.reference
           ~parent_session_id:parent.identity.session_id
           ~parent_generation:parent.identity.generation
           ~child_session_id:second_session_id
           ~name:"inherited_turn_hook"
       in
       let scope =
         Depth.scope
           ~goal:(Goal_run_tests.current_goal parent)
           ~reference
           ~transaction_id:(P.Id.Transaction.of_string "txn_turn_hook" |> protocol_ok)
       in
       let before = Depth.new_child_state ~parent ~scope ~own_reference:reference in
       let accounting = Option.value_exn before.goal_descendants in
       let children = Option.value_exn before.child_contributions in
       let frame revision =
         Occurrence.Frame.create
           ~session:(GD.session accounting)
           ~generation:before.identity.generation
           ~candidate_session_revision:revision
         |> protocol_ok
       in
       let receipt occurrence prefix =
         Pending.create
           ~scope
           ~branch_reference:reference
           ~nonce:prefix
           ~next_prefix:prefix
           ~occurrence
         |> protocol_ok
         |> Included.create
       in
       (* Earlier actual Attempt + Record belong to this same public revision.
          The subsequent Turn must start from their ledger AND Included head. *)
       let _, _, prepared = Inference_tracking_tests.prepared () in
       let after_attempt, handle, tracking =
         L.admit
           before.inference_ledger
           ~source:
             (Transcript.Source_id.of_string "turn-hook-prefix" |> Result.ok_or_failwith)
           ~relation:Root
           ~operation_id:None
           ~invocation_id:None
           ~configuration:(Inference_runtime.Prepared.configuration prepared)
         |> Goal_accounting_persistence_tests.ledger
       in
       let attempt =
         Occurrence.attempt
           (frame 1L)
           ~ordinal:(L.Handle.ordinal handle)
           ~operation_id:None
           ~invocation_id:None
         |> protocol_ok
       in
       let first = receipt attempt 1L in
       let first_update =
         GD.record_included
           accounting
           ~children
           ~before:before.inference_ledger
           ~after:after_attempt
           ~canonical:(Attempt (handle, tracking))
           ~occurrence:attempt
           ~receipts:[ first ]
         |> protocol_ok
       in
       let own =
         Agent_session.Delegated_usage.reconcile
           (Option.value_exn before.delegated_usage)
           ~before:before.inference_ledger
           ~after:after_attempt
         |> protocol_ok
       in
       let op = operation (Turn User_submit) Starting in
       let turn revision operation_id =
         Occurrence.host_turn (frame revision) ~operation_id |> protocol_ok
       in
       let occurrence = turn 1L op.id in
       let second = receipt occurrence 2L in
       let input = Input.create ~occurrence ~receipts:[ second ] |> protocol_ok in
       let delta =
         D.Batch
           [ Delegated_inference_ledger_changed (after_attempt, own)
           ; Goal_descendants_changed first_update
           ; Active_operation_changed (Some op)
           ; Lifecycle_changed { desired = Running; observed = Running_turn op.id }
           ]
       in
       let apply
             ?inherited_turn
             ?(payloads = [ P.Event.Durable.Payload.Operation_started op ])
             ()
         =
         T.apply ?inherited_turn ~now:timestamp before ~delta ~payloads
       in
       assert (Result.is_error (apply ()));
       let admitted, edge =
         T.apply_inherited
           ~inherited_turn:input
           ~now:timestamp
           before
           ~delta
           ~payloads:[ Operation_started op ]
         |> protocol_ok
       in
       let module Edge = Agent_session.Host_turn_tracking.Inherited_edge in
       let canonical = Edge.canonical edge in
       let checked =
         GD.Canonical.check
           ~before:(Edge.before edge)
           ~after:(Edge.after edge)
           ~canonical
           ~occurrence
         |> protocol_ok
       in
       (match canonical with
        | Attempt _ -> failwith "inherited Turn returned an Attempt edge"
        | Host_turn (actual_operation, actual_handle, actual_tracking) ->
          assert (
            Document_schema.Json.equal
              (P.Operation.to_json op)
              (P.Operation.to_json actual_operation));
          assert (P.Id.Operation.equal (L.Turn_handle.operation_id actual_handle) op.id);
          [%test_eq: int] (L.Turn_handle.generation actual_handle) op.generation;
          assert (L.equal_tracking actual_tracking (GD.Canonical.Fact.tracking checked)));
       [%test_eq: int64] (L.revision (Edge.before edge)) (L.revision after_attempt);
       assert (
         Result.is_error
           (GD.Canonical.check
              ~before:before.inference_ledger
              ~after:(Edge.after edge)
              ~canonical
              ~occurrence));
       let summary = L.summary admitted.state.inference_ledger in
       [%test_eq: int64] (P.Inference_query.Summary.retained_attempts summary) 1L;
       [%test_eq: int64] (P.Inference_query.Summary.turns summary).pending 1L;
       [%test_eq: int64] admitted.state.counters.revision 1L;
       let latest =
         GD.local_included (Option.value_exn admitted.state.goal_descendants)
       in
       assert (List.equal Included.equal latest [ second ]);
       let encoded =
         Agent_session.Session_document_transition.admit_encoded
           (Agent_session.Session_state_document.authored before)
           ~delta:admitted.delta
           ~next:admitted.state
           ~limits:document_limits
         |> document_ok
       in
       let restored =
         Agent_session.Session_state_document.Admitted.document encoded
         |> Agent_session.Session_state_document.decode ~limits:document_limits
         |> document_ok
         |> Agent_session.Session_state_document.value
       in
       assert (
         List.equal
           Included.equal
           (GD.local_included (Option.value_exn restored.goal_descendants))
           [ second ]);
       assert (Result.is_error (apply ~inherited_turn:input ~payloads:[] ()));
       assert (
         Result.is_error
           (apply
              ~inherited_turn:input
              ~payloads:[ Operation_started op; Operation_started op ]
              ()));
       let wrong_input occurrence =
         Input.create ~occurrence ~receipts:[ receipt occurrence 2L ] |> protocol_ok
       in
       List.iter
         [ turn 1L (P.Id.Operation.create ()); turn 2L op.id ]
         ~f:(fun occurrence ->
           assert (Result.is_error (apply ~inherited_turn:(wrong_input occurrence) ())));
       let compact = operation Compaction Starting in
       assert (
         Result.is_error
           (T.apply
              ~inherited_turn:input
              ~now:timestamp
              before
              ~delta:
                (Batch
                   [ Active_operation_changed (Some compact)
                   ; Lifecycle_changed
                       { desired = Running; observed = Running_turn compact.id }
                   ])
              ~payloads:[ Operation_started compact ]));
       assert (Result.is_error (Input.create ~occurrence:attempt ~receipts:[ first ]));
       assert (Result.is_error (Input.create ~occurrence ~receipts:[]));
       assert (Result.is_error (Input.create ~occurrence ~receipts:[ first ]));
       assert (Result.is_error (Input.create ~occurrence ~receipts:[ second; second ]));
       print_endline
         "Attempt then real Turn pair at revision1; replay retains head2; missing, \
          mismatched, duplicate and unused Turn inputs reject");
  [%expect
    {| Attempt then real Turn pair at revision1; replay retains head2; missing, mismatched, duplicate and unused Turn inputs reject |}]
;;

let%expect_test "optional public inference summary preserves absent versus null" =
  with_actor_workspace (fun _ workspace_instance ->
    let summary = S.summary (initial workspace_instance) in
    let absent =
      { summary with inference_summary = History_entry.Payload.Presence.Absent }
    in
    let explicit_null =
      { summary with inference_summary = History_entry.Payload.Presence.Null }
    in
    let absent = P.Session.to_json absent |> P.Session.of_json |> protocol_ok in
    let explicit_null =
      P.Session.to_json explicit_null |> P.Session.of_json |> protocol_ok
    in
    (match absent.inference_summary, explicit_null.inference_summary with
     | Absent, Null -> ()
     | _ -> assert false);
    let sexp_roundtrip summary =
      P.Session.Inference_summary.sexp_of_t summary
      |> P.Session.Inference_summary.t_of_sexp
    in
    (match
       ( sexp_roundtrip absent.inference_summary
       , sexp_roundtrip explicit_null.inference_summary
       , sexp_roundtrip summary.inference_summary )
     with
     | Absent, Null, Value _ -> ()
     | _ -> assert false);
    assert (
      not
        (String.equal
           (Jsonaf.to_string (P.Session.to_json absent))
           (Jsonaf.to_string (P.Session.to_json explicit_null))));
    print_endline "absent omitted; null explicit; decoded presence remains distinct");
  [%expect {| absent omitted; null explicit; decoded presence remains distinct |}]
;;
