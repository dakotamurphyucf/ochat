open Core
open Fixtures
module P = Agent_protocol
module State = Agent_session.Session_state
module Delta = Agent_session.Session_delta
module A = Agent_session.Session_actor
module R = Chat_response.Authoring_reference_index
module F = Authoring_history_tests

let receipts state = State.authoring_references state |> protocol_ok |> R.receipts

let replay state delta =
  let transaction =
    Agent_store.Transaction.create
      ~session_id:state.State.identity.session_id
      ~generation:state.identity.generation
      ~transaction_sequence:Int64.(state.counters.transaction_sequence + 1L)
      ~previous_transaction_hash:None
      ~session_revision:Int64.(state.counters.revision + 1L)
      ~first_event_sequence:None
      ~last_event_sequence:None
      ~accepted_at_ns:
        (P.Timestamp.to_time_ns timestamp |> Time_ns.to_int_ns_since_epoch |> Int64.of_int)
      ~command_audit:None
      ~delta:(Delta.sexp_of_t delta |> Sexp.to_string_mach)
      ~durable_events:[]
    |> store_ok
    |> Agent_store.Transaction.encode
    |> Agent_store.Transaction.decode
    |> store_ok
  in
  Agent_session.Session_persistence.apply_transaction state transaction |> store_ok
;;

let%expect_test
    "reference receipts replay with history and reject invalid persisted scope"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Process_bound ~start_immediately:false
    in
    let value = F.entry (F.manual ()) in
    let appended = replay initial (Canonical_entries_appended [ value ]) |> F.restore in
    let expected = receipts appended in
    assert (List.length expected = 1);
    let compacted = replay appended (Canonical_history_replaced []) |> F.restore in
    assert (List.is_empty compacted.conversation.canonical_history);
    assert (
      List.equal
        Chat_response.Authoring_presence.equal_receipt
        expected
        (receipts compacted));
    let index = Option.value_exn compacted.conversation.authoring_reference_index in
    let rejects state =
      assert (
        Result.is_error
          (Agent_session.Session_persistence.restore_snapshot
             (State.sexp_of_t state |> Sexp.to_string_mach)))
    in
    rejects { compacted with schema_version = 17 };
    rejects { compacted with identity = { compacted.identity with generation = 1 } };
    let bad_index =
      match index with
      | `Object fields ->
        `Object (List.Assoc.add fields ~equal:String.equal "version" (`Number "2"))
      | _ -> assert false
    in
    rejects
      { compacted with
        conversation =
          { compacted.conversation with authoring_reference_index = Some bad_index }
      };
    let legacy = F.restore { initial with schema_version = 17 } in
    assert (Option.is_none legacy.conversation.authoring_reference_index);
    let redacted =
      replay appended (Canonical_history_replaced [ { value with redacted = true } ])
      |> F.restore
    in
    assert (List.is_empty (receipts redacted));
    let reset = Delta.apply compacted (Reset_generation 1) |> protocol_ok |> F.restore in
    assert (List.is_empty (receipts reset));
    List.iter [ true; false ] ~f:(fun keep_history ->
      let reset =
        Agent_session.Administration.reset
          appended
          { keep_history
          ; keep_tasks = false
          ; keep_grants = false
          ; keep_labels = true
          ; workspace_instance = None
          }
        |> protocol_ok
        |> F.restore
      in
      assert (Option.is_none reset.conversation.authoring_reference_index));
    print_endline
      "journal + snapshot retain receipts after compaction; redaction and reset clear \
       them";
    print_endline
      "legacy absence migrates; old-schema, changed-generation and future-index \
       smuggling rejected");
  [%expect
    {|
    journal + snapshot retain receipts after compaction; redaction and reset clear them
    legacy absence migrates; old-schema, changed-generation and future-index smuggling rejected
    |}]
;;

let%expect_test
    "explicit history deletion forgets the receipt in the committed actor state"
  =
  let value = F.entry (F.manual ()) in
  Job_fixtures.with_actor
    ~prepare_state:(fun initial ->
      Delta.apply initial (Canonical_entries_appended [ value ]) |> protocol_ok)
    (fun _env _sw actor writer backend ->
       let before = A.state actor |> protocol_ok in
       assert (List.length (receipts before) = 1);
       assert (
         Result.is_error
           (A.delete_history
              actor
              ~attachment_id:writer.id
              ~expected_revision:Int64.(before.counters.revision - 1L)
              value.id));
       assert (List.length (receipts (A.state actor |> protocol_ok)) = 1);
       A.delete_history
         actor
         ~attachment_id:writer.id
         ~expected_revision:before.counters.revision
         value.id
       |> protocol_ok
       |> ignore;
       let committed = Agent_session.Memory_backend.state backend |> F.restore in
       assert (List.is_empty (receipts committed));
       assert (List.is_empty committed.conversation.canonical_history);
       print_endline
         "stale deletion preserves metadata; committed deletion removes history and \
          receipt together");
  [%expect
    {| stale deletion preserves metadata; committed deletion removes history and receipt together |}]
;;
