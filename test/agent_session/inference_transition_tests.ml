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
