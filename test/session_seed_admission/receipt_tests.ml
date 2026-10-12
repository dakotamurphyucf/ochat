open! Core
open Admission_tests
module Receipt = Session_seed_receipt

let created_state () =
  A.Prelayout.state
    (prelayout Fn.ignore |> ok)
    ~workspace:actual_workspace
    ~checkpoint:Fn.ignore
  |> ok
;;

let with_value transaction value =
  Agent_store.Transaction.with_value transaction ~limits value |> store_ok
;;

let%test_unit "only first audited seed Created binds its original protected outcome" =
  let transaction = actual_transaction (created_state ()) in
  let evidence =
    Receipt.of_created transaction ~limits ~checkpoint:Fn.ignore |> store_ok
  in
  let audit = Receipt.audit evidence in
  let record outcome : Agent_store.Idempotency_store.record =
    { key = audit.key
    ; request_digest = audit.request_digest
    ; accepted_transaction_sequence = Some 1L
    ; outcome
    ; created_at = timestamp
    ; expires_at = None
    ; retention = Protected
    }
  in
  assert (Result.is_ok (Receipt.validate_receipt evidence (record Pending)));
  assert (
    Result.is_ok
      (Receipt.validate_receipt
         evidence
         (record (Success (P.Session_seed.Result.to_json (Receipt.result evidence))))));
  assert (
    Result.is_error
      (Receipt.validate_receipt
         evidence
         { (record (Success (P.Session_seed.Result.to_json (Receipt.result evidence)))) with
           accepted_transaction_sequence = None
         }));
  let another =
    P.Session_seed.Result.{ session_id = P.Id.Session.of_string "ses_other" |> ok }
  in
  assert (
    Result.is_error
      (Receipt.validate_receipt
         evidence
         (record (Success (P.Session_seed.Result.to_json another)))));
  assert (
    Result.is_error
      (Receipt.validate_receipt evidence { (record Pending) with retention = Standard }));
  assert (
    Result.is_ok
      (Receipt.validate_receipt
         evidence
         { (record Pending) with accepted_transaction_sequence = None }));
  assert (
    Result.is_error
      (Receipt.validate_receipt
         evidence
         { (record Pending) with accepted_transaction_sequence = Some 2L }));
  let value = Agent_store.Transaction.value transaction in
  List.iter
    [ { value with transaction_sequence = 2L }
    ; { value with command_audit = None }
    ; { value with
        command_audit =
          Some
            (Agent_store.Idempotency_store.Command_audit.encode
               { audit with
                 key =
                   { audit.key with
                     principal_id = P.Id.Principal.of_string "pri_other" |> ok
                   }
               }
             |> store_ok)
      }
    ; { value with
        command_audit =
          Some
            (Agent_store.Idempotency_store.Command_audit.encode
               { audit with key = { audit.key with method_name = "session.create" } }
             |> store_ok)
      }
    ]
    ~f:(fun value ->
      assert (
        Result.is_error
          (Receipt.of_created
             (with_value transaction value)
             ~limits
             ~checkpoint:Fn.ignore)));
  let duplicate_delta =
    S.Session_delta_document.create
      (Batch [ Created (created_state ()); Created (created_state ()) ])
      ~limits
      ~state_document:S.Session_state_document.authored
    |> doc_ok
    |> S.Session_delta_document.document
  in
  assert (
    Result.is_error
      (Receipt.of_created
         (with_value transaction { value with delta = duplicate_delta })
         ~limits
         ~checkpoint:Fn.ignore))
;;

let%test_unit "first Created unknown fields survive bootstrap and unrelated later replay" =
  let transaction = actual_transaction (created_state ()) in
  let value = Agent_store.Transaction.value transaction in
  let initial_document =
    S.Session_state_document.encode
      (S.Session_state_document.authored (created_state ()))
      ~limits
    |> doc_ok
  in
  let extended =
    match D.Document.json initial_document with
    | `Object fields ->
      `Object
        (fields
         @ [ ( "future-owner"
             , `Object [ "n", `Number "1.200e+03"; "id", `String "007\\opaque" ] )
           ])
    | _ -> assert false
  in
  let extended_document = D.Document.inspect ~limits extended |> doc_ok in
  let delta =
    S.Session_delta_document.project_created extended_document ~limits |> doc_ok
  in
  let transaction = with_value transaction { value with delta } in
  let restored =
    Receipt.of_created transaction ~limits ~checkpoint:Fn.ignore
    |> store_ok
    |> Receipt.restored
  in
  let assert_extension restored =
    let doc =
      S.Session_persistence.Restored.state_document restored
      |> fun state -> S.Session_state_document.encode state ~limits |> doc_ok
    in
    match D.Json.field (D.Document.json doc) ~name:"future-owner" with
    | Value actual ->
      assert (
        Jsonaf.exactly_equal
          actual
          (`Object [ "n", `Number "1.200e+03"; "id", `String "007\\opaque" ]))
    | Absent | Null -> assert false
  in
  assert_extension restored;
  let state = S.Session_persistence.Restored.state restored in
  let delta =
    S.Session_delta_document.create
      (Halt_changed (Some "later change"))
      ~limits
      ~state_document:S.Session_state_document.authored
    |> doc_ok
    |> S.Session_delta_document.document
  in
  let later =
    Agent_store.Transaction.create
      ~limits
      ~session_id
      ~generation:0
      ~transaction_sequence:2L
      ~previous_transaction_hash:(Some (Agent_store.Transaction.hash transaction))
      ~session_revision:2L
      ~first_event_sequence:None
      ~last_event_sequence:None
      ~accepted_at_ns:
        (Time_ns.to_int63_ns_since_epoch (P.Timestamp.to_time_ns timestamp)
         |> Int63.to_int64)
      ~command_audit:None
      ~delta
      ~durable_events:[]
    |> store_ok
  in
  assert (Int64.equal state.counters.transaction_sequence 1L);
  let updated =
    S.Session_persistence.apply_transaction ~limits restored later |> store_ok
  in
  assert_extension updated;
  assert (S.Session_persistence.Restored.state updated).halted;
  assert (not state.halted)
;;
