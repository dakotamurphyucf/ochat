open! Core
open Creation_support
module Due = Due_support
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Execution = A.Recurrence_execution
module Association = A.Recurrence_child_association
module State = A.Session_state_document
module Growth = A.Capacity_growth
module Origin = A.Recurrence_run_origin
module Reference = A.Recurrence_run_reference

(* Pure validated association DATA. This does not assert that any child was
   installed or issue an actual store/actor link observation. *)
let association row =
  let key = Reservation.delegation_key row |> protocol_ok in
  Agent_store.Delegation_store.reference_of_jsonaf
    (`Object
        [ ( "key"
          , `Object
              [ "parent_session_id", P.Id.Session.to_json key.parent_session_id
              ; "parent_generation", `Number (Int.to_string key.parent_generation)
              ; "principal_id", P.Id.Principal.to_json key.principal_id
              ; "idempotency_key", P.Idempotency_key.to_json key.idempotency_key
              ] )
        ; "child_session_id", P.Id.Session.to_json (Reservation.child_session_id row)
        ; ( "revision_id"
          , P.Id.Prompt_revision.to_json (Reservation.artifact_revision_id row) )
        ; "request_sha256", `String (Reservation.request_sha256 row)
        ; "admission_sha256", `String (String.make 64 'a')
        ])
  |> store_ok
  |> fun reference -> Association.create ~reservation:row ~reference |> protocol_ok
;;

let request intent key =
  Attempt.create
    ~intent
    ~number:0
    ~principal_id
    ~expected_revision:7L
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
  |> protocol_ok
;;

let fresh_intent entry reservation association =
  let definition = E.current entry in
  let child = Intent.Child.create ~reservation ~association |> protocol_ok in
  Intent.create
    ~definition
    ~control_revision:(E.control_revision entry)
    ~nominal_json:(Reservation.nominal_json reservation)
    ~target:
      (P.Session_ref.create
         ~server_id:(P.Session_ref.server_id (I.Definition.controller (F.key definition)))
         ~session_id:(Reservation.child_session_id reservation))
    ~source:(Due.source 1L)
    ~child:(Some child)
  |> protocol_ok
;;

(* Constructor data, not an actual target observation/authorization. The whole
   host/transaction paths below are real encoders, never execution permission. *)
let run_evidence execution ~completed_at ~outcome =
  let intent = Execution.intent execution in
  let request = Execution.current_attempt execution |> Execution.Attempt.request in
  let key = Intent.occurrence intent in
  let id =
    P.Id.Run.of_string
      ("run_settlement_"
       ^ P.Id.Recurrence.to_string
           (I.Definition.recurrence_id (I.Occurrence.definition key))
       ^ "_"
       ^ Int64.to_string (I.Definition.revision (I.Occurrence.definition key))
       ^ "_"
       ^ Int64.to_string (I.Occurrence.slot key))
    |> protocol_ok
  in
  let run lifecycle revision =
    P.Run.create
      ~id
      ~session:(Intent.target intent)
      ~principal_id
      ~source:(Intent.source intent)
      ~mode:(Intent.mode intent)
      ~lifecycle
      ~revision
      ~owned_work:[]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:timestamp
      ~updated_at:completed_at
    |> protocol_ok
  in
  let receipt kind key request_sha256 run_revision committed_at =
    P.Run_receipt.create
      ~run_id:id
      ~principal_id
      ~source:(Intent.source intent)
      ~key
      ~request_sha256
      ~kind
      ~run_revision
      ~session_revision:9L
      ~committed_at
    |> protocol_ok
  in
  let admission =
    receipt Admission (Attempt.key request) (Attempt.request_sha256 request) 0L timestamp
  in
  let origin =
    Origin.create ~intent ~attempt:request ~run:(run Admitted 0L) ~admission
    |> protocol_ok
  in
  let digest =
    P.Run.Terminal.to_json outcome
    |> Jsonaf.to_string
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
  in
  let receipt =
    receipt
      Terminal
      (P.Idempotency_key.of_string "settlement:terminal" |> protocol_ok)
      digest
      1L
      completed_at
  in
  let terminal =
    Reference.create ~origin ~run:(run (Terminal outcome) 1L) ~receipt |> protocol_ok
  in
  origin, terminal
;;

let policy pending =
  A.Recurrence_policy.create
    ~misfire:(Catch_up 2)
    ~grace_ms:60_000L
    ~max_pending:pending
    ~max_history:1
    ~max_calendar_steps:256
  |> protocol_ok
;;

let redefine previous ~key ~policy =
  F.create
    ~key
    ~creator:(F.creator previous)
    ~source:(F.source previous)
    ~birth_lifecycle_revision:(F.birth_lifecycle_revision previous)
    ~schedule:(F.schedule previous)
    ~target:(F.target previous)
    ~policy
    ~created_at:(F.created_at previous)
  |> protocol_ok
;;

let%expect_test
    "captured execution carrier counts old child claims once across a lower policy"
  =
  with_actor_workspace (fun _ workspace ->
    let definition =
      Domain_tests.captured_definition workspace (Due.definition "execution_children")
    in
    let definition = redefine definition ~key:(F.key definition) ~policy:(policy 2) in
    let selected, _, _ =
      Due.step
        (Due.enabled definition)
        ~phase:None
        (Due.snapshot ~wall:(Due.at_ms 1000) ~elapsed_ms:1000)
    in
    [%test_eq: int] (H.pending_count (Due.frontier selected)) 2;
    let selected =
      Due.edit_frontier selected ~f:(fun payload ->
        match Due.field payload "ready" with
        | `Array rows ->
          Due.set
            payload
            "ready"
            (`Array
                (List.map rows ~f:(fun row ->
                   Due.append row "future_nominal" (`Number "2.00e1"))))
        | _ -> assert false)
    in
    let record entry slot =
      let reserved, reservation = Domain_tests.reserve_entry entry slot in
      let linked_data = association reservation in
      let intent = fresh_intent reserved reservation linked_data in
      let attempt = request intent ("child-execution:" ^ Int64.to_string slot) in
      assert (Result.is_error (E.record_execution reserved ~intent ~attempt));
      let linked = E.link_child reserved linked_data |> protocol_ok in
      let changed =
        Association.to_json linked_data
        |> fun json ->
        Due.append json "future_association" (`String "different retained bytes")
        |> Association.of_json
        |> protocol_ok
      in
      let different_intent = fresh_intent linked reservation changed in
      assert (
        Result.is_error
          (E.record_execution
             linked
             ~intent:different_intent
             ~attempt:(request different_intent "different")));
      let recorded, row = E.record_execution linked ~intent ~attempt |> protocol_ok in
      let recorded =
        Due.edit_frontier recorded ~f:(fun payload ->
          match Due.field payload "executions" with
          | `Array rows ->
            Due.set
              payload
              "executions"
              (`Array
                  (List.map rows ~f:(fun raw ->
                     let execution =
                       Due.field raw "execution" |> Execution.of_json |> protocol_ok
                     in
                     if
                       I.Occurrence.equal
                         (Intent.occurrence (Execution.intent execution))
                         (Intent.occurrence intent)
                     then
                       Due.append raw "future_wrapper" (`Number "3.00e1")
                       |> fun raw ->
                       Due.set
                         raw
                         "execution"
                         (Due.append
                            (Due.field raw "execution")
                            "future_execution"
                            (`Number "4.00e1"))
                     else raw)))
          | _ -> failwith "missing complete execution table")
      in
      assert (
        D.Json.equal
          (Reservation.nominal_json reservation)
          (Intent.nominal_json (Execution.intent row)));
      let replay, _ = E.record_execution recorded ~intent ~attempt |> protocol_ok in
      assert (D.Json.equal (Due.entry_json recorded) (Due.entry_json replay));
      recorded
    in
    let recorded = record selected 0L |> fun entry -> record entry 1L in
    [%test_eq: int] (H.pending_count (Due.frontier recorded)) 2;
    [%test_eq: int] (List.length (H.protected_children (Due.frontier recorded))) 2;
    [%test_eq: int] (List.length (H.executions (Due.frontier recorded))) 2;
    [%test_eq: int] (E.pending_execution_steps recorded) 4;
    let paused = E.pause recorded ~reason:Operator_pause |> protocol_ok in
    let next_key =
      I.Definition.create
        ~controller:(E.controller paused)
        ~recurrence_id:(E.id paused)
        ~revision:1L
      |> protocol_ok
    in
    let lower = redefine definition ~key:next_key ~policy:(policy 1) in
    let edited = E.edit paused lower |> protocol_ok |> E.enable |> protocol_ok in
    let next, _, _ =
      Due.step edited ~phase:None (Due.snapshot ~wall:(Due.at_ms 2000) ~elapsed_ms:2000)
    in
    [%test_eq: int] (H.pending_count (Due.frontier next)) 2;
    assert (List.is_empty (H.ready (Due.frontier next)));
    let summary = H.summary (Due.frontier next) in
    [%test_eq: int64] summary.omitted 3L;
    List.iter [ recorded; paused; edited; next ] ~f:(fun entry ->
      assert (D.Json.equal (Due.entry_json entry) (Due.entry_json (Due.restore entry))));
    assert (
      List.equal
        Execution.equal
        (H.executions (Due.frontier recorded))
        (H.executions (Due.frontier next)));
    let proofs =
      List.map
        (H.executions (Due.frontier recorded))
        ~f:(fun execution ->
          run_evidence execution ~completed_at:timestamp ~outcome:(Completed None))
    in
    let settled =
      List.fold proofs ~init:recorded ~f:(fun entry (origin, terminal) ->
        let next, _ =
          E.import_execution
            entry
            ~origin
            ~terminal:(Some terminal)
            ~automatic:Current_policy_allows
          |> protocol_ok
        in
        assert (D.Json.equal (Due.entry_json next) (Due.entry_json (Due.restore next)));
        next)
    in
    [%test_eq: int] (H.pending_count (Due.frontier settled)) 0;
    [%test_eq: int64] (H.summary (Due.frontier settled)).settled 2L;
    [%test_eq: int] (List.length (H.executions (Due.frontier settled))) 1;
    [%test_eq: int] (List.length (H.protected_children (Due.frontier settled))) 1;
    [%test_eq: int] (List.length (H.child_associations (Due.frontier settled))) 1;
    let raw = H.to_document (Due.frontier settled) |> document_ok |> D.Document.payload in
    (match Due.field raw "executions" with
     | `Array [ row ] ->
       assert (D.Json.equal (Due.field row "future_wrapper") (`Number "3.00e1"));
       assert (
         D.Json.equal
           (Due.field (Due.field row "execution") "future_execution")
           (`Number "4.00e1"))
     | _ -> failwith "terminal bundle detail did not compact atomically");
    let origin, terminal = List.hd_exn proofs in
    let same, result =
      E.import_execution
        settled
        ~origin
        ~terminal:(Some terminal)
        ~automatic:Current_policy_allows
      |> protocol_ok
    in
    assert (D.Json.equal (Due.entry_json same) (Due.entry_json settled));
    (match result with
     | Detail_not_retained _ -> ()
     | Imported _ | Retained_exact _ -> failwith "retired child detail falsely matched");
    let delay =
      Domain_tests.captured_definition
        workspace
        (Due.definition
           "interrupted_child"
           ~schedule:
             (Interval
                { rule =
                    R.Rule.Interval.create
                      ~mode:Fixed_delay
                      ~period:(R.Rule.Period.create_ms 1000L |> Due.timing)
                ; first_due = Due.at_ms 0
                }))
    in
    let selected, _, _ =
      Due.step
        (Due.enabled delay)
        ~phase:None
        (Due.snapshot ~wall:(Due.at_ms 0) ~elapsed_ms:0)
    in
    let reserved, reservation = Domain_tests.reserve_entry selected 0L in
    let linked = association reservation in
    let entry = E.link_child reserved linked |> protocol_ok in
    let intent = fresh_intent entry reservation linked in
    let entry, execution =
      E.record_execution entry ~intent ~attempt:(request intent "interrupted-child")
      |> protocol_ok
    in
    let origin, terminal =
      run_evidence execution ~completed_at:timestamp ~outcome:Interrupted
    in
    let held, _ =
      E.import_execution
        entry
        ~origin
        ~terminal:(Some terminal)
        ~automatic:Current_policy_allows
      |> protocol_ok
    in
    [%test_eq: int] (H.pending_count (Due.frontier held)) 0;
    [%test_eq: int] (List.length (H.protected_children (Due.frontier held))) 1;
    assert (E.equal_control (E.control held) (Paused Recovery_review));
    List.iter
      [ held; E.pause held ~reason:Source_review |> protocol_ok ]
      ~f:(fun held ->
        let prior = Due.entry_json held in
        assert (Result.is_error (E.enable held));
        assert (D.Json.equal prior (Due.entry_json held));
        assert (Option.is_none (H.alarm (Due.frontier held))));
    assert (D.Json.equal (Due.entry_json held) (Due.entry_json (Due.restore held))));
  print_endline
    "two child+execution claims stay two; exact linked bytes; old rows survive \
     max_pending 2 to 1";
  [%expect
    {| two child+execution claims stay two; exact linked bytes; old rows survive max_pending 2 to 1 |}]
;;

let growth_covers reserved actual =
  List.iter [ Growth.encoded_bytes; Growth.fields; Growth.nodes ] ~f:(fun dimension ->
    assert (dimension actual <= dimension reserved))
;;

let measure document =
  Growth.measure (D.Document.json document) ~limits:document_limits |> protocol_ok
;;

let host_document carrier = State.encode carrier ~limits:document_limits |> document_ok

(* Actual canonical producer and complete ordered carrier admission. This is a
   data/encoder oracle, not a public actor execution writer or target admission. *)
let admit_catalog carrier catalog =
  let previous = State.value carrier in
  let transition =
    A.Session_transition.apply
      ~now:timestamp
      previous
      ~delta:(Recurrence_catalog_changed catalog)
      ~payloads:[]
    |> protocol_ok
  in
  assert (List.is_empty transition.events);
  let admitted =
    A.Session_document_transition.admit_encoded
      carrier
      ~delta:transition.delta
      ~next:transition.state
      ~limits:document_limits
    |> document_ok
  in
  let delta =
    A.Session_delta_document.create
      transition.delta
      ~limits:document_limits
      ~state_document:State.authored
    |> document_ok
    |> A.Session_delta_document.document
  in
  let transaction =
    Agent_store.Transaction.create
      ~limits:document_limits
      ~session_id:previous.identity.session_id
      ~generation:previous.identity.generation
      ~transaction_sequence:transition.state.counters.transaction_sequence
      ~previous_transaction_hash:None
      ~session_revision:transition.state.counters.revision
      ~first_event_sequence:None
      ~last_event_sequence:None
      ~accepted_at_ns:0L
      ~command_audit:None
      ~delta
      ~durable_events:[]
    |> store_ok
  in
  let bytes = Agent_store.Transaction.encode transaction in
  let restored = Agent_store.Transaction.decode bytes |> store_ok in
  assert (D.Json.equal (D.Document.json restored.delta) (D.Document.json delta));
  Agent_store.Transaction.Capacity.document
    ~limits:document_limits
    ~session_id:previous.identity.session_id
    ~generation:previous.identity.generation
    ~delta
    ~durable_events:[]
  |> document_ok
  |> ignore;
  (* Match the persistence owner's post-admission materialization, not merely
     its pre-ACK candidate which intentionally retains the old template. The next
     independent transition must compare against this exact new encoded basis. *)
  let carrier =
    State.decode_after
      (State.Admitted.state_document admitted)
      ~limits:document_limits
      (State.Admitted.document admitted)
    |> document_ok
  in
  carrier, String.length bytes
;;

let%expect_test
    "execution obligations compose through actual host and transaction profiles"
  =
  with_store (fun fixture ->
    let actor = create_actor fixture ~sw:fixture.sw () in
    Exn.protect
      ~finally:(fun () -> A.Session_actor.shutdown actor)
      ~f:(fun () ->
        let host = open_host fixture () in
        Exn.protect
          ~finally:(fun () -> Host.close_and_wait host)
          ~f:(fun () ->
            install actor host;
            List.iter
              [ "first_execution", R.Rule.Interval.Fixed_delay
              ; "second_execution", R.Rule.Interval.Fixed_rate
              ]
              ~f:(fun (name, mode) ->
                create
                  actor
                  name
                  (interval ~mode ~first_due:(P.Timestamp.to_time_ns timestamp) ())
                |> ignore);
            sweep actor;
            let initial = state actor in
            A.Session_actor.shutdown actor;
            let original = State.authored initial |> host_document in
            let raw = D.Document.json original in
            let preserved =
              Due.set
                raw
                "payload"
                (Due.append
                   (Due.field raw "payload")
                   "future_execution_host"
                   (`Number "2.00e1"))
              |> D.Document.inspect ~limits:document_limits
              |> document_ok
              |> State.decode ~limits:document_limits
              |> document_ok
            in
            let carrier, frame_bytes =
              List.fold
                [ "first_execution"; "second_execution" ]
                ~init:(preserved, 0)
                ~f:(fun (carrier, _) name ->
                  let state = State.value carrier in
                  let catalog = Option.value_exn state.recurrence_catalog in
                  let entry = C.find catalog (id name) |> Option.value_exn in
                  let definition = E.current entry in
                  let nominal =
                    E.frontier entry
                    |> protocol_ok
                    |> H.to_document
                    |> document_ok
                    |> D.Document.payload
                    |> fun payload ->
                    match Due.field payload "ready" with
                    | `Array [ nominal ] -> nominal
                    | _ -> failwith "expected actual host-selected Ready"
                  in
                  let target, source =
                    match A.Recurrence_target.view (F.target definition) with
                    | Existing_session { session; source; _ } -> session, source
                    | Fresh_child _ -> assert false
                  in
                  let intent =
                    Intent.create
                      ~definition
                      ~control_revision:(E.control_revision entry)
                      ~nominal_json:nominal
                      ~target
                      ~source
                      ~child:None
                    |> protocol_ok
                  in
                  let attempt = request intent ("host-original:" ^ name) in
                  let next, _ =
                    C.record_execution catalog ~intent ~attempt |> protocol_ok
                  in
                  admit_catalog carrier next)
            in
            let pending_carrier = carrier in
            let pending_document = host_document pending_carrier in
            let current = State.value carrier in
            let catalog = Option.value_exn current.recurrence_catalog in
            let steps =
              C.reservation catalog
              |> protocol_ok
              |> C.Reservation.required_transition_steps
            in
            [%test_eq: int] steps 6;
            let boundary =
              { current with
                counters =
                  { current.counters with
                    revision = Int64.(max_value - 6L)
                  ; transaction_sequence = Int64.(max_value - 6L)
                  }
              }
            in
            A.Session_state.validate boundary |> protocol_ok;
            assert (
              Result.is_error
                (A.Session_state.validate
                   { boundary with
                     counters =
                       { boundary.counters with revision = Int64.(max_value - 5L) }
                   }));
            let document = host_document carrier in
            let shallow =
              List.find_map_exn
                (List.init (D.Limits.max_depth document_limits) ~f:(fun index ->
                   index + 1))
                ~f:(fun max_depth ->
                  let limits =
                    D.Limits.create
                      ~max_bytes:(D.Limits.max_bytes document_limits)
                      ~max_depth
                      ~max_fields:(D.Limits.max_fields document_limits)
                      ~max_nodes:(D.Limits.max_nodes document_limits)
                    |> document_ok
                  in
                  if Result.is_ok (D.Json.validate ~limits (D.Document.json document))
                  then Some limits
                  else None)
            in
            (match State.encode carrier ~limits:shallow with
             | Error (Limit_exceeded dimension) when String.equal dimension "depth" -> ()
             | Error error ->
               raise_s [%sexp "unexpected configured-profile error", (error : D.Error.t)]
             | Ok _ -> failwith "future proof depth was not reserved by host encoding");
            Capacity_tests.check_future
              carrier
              (Agent_store.Commit_writer.max_payload_length fixture.writer)
            |> protocol_ok;
            (match Capacity_tests.check_future carrier frame_bytes with
             | Error error ->
               assert (P.Error.equal_code error.code Invalid_request);
               [%test_eq: string]
                 error.message
                 (Sexp.to_string_hum (D.Error.sexp_of_t (Limit_exceeded "bytes")))
             | Ok () ->
               failwith
                 "current-frame budget admitted larger mandatory future transaction");
            let dispose carrier transform expected_steps =
              let before = host_document carrier in
              let reservation =
                State.recurrence_capacity carrier ~document:before ~limits:document_limits
                |> document_ok
              in
              let catalog =
                Option.value_exn (State.value carrier).recurrence_catalog
                |> transform
                |> protocol_ok
                |> Option.value_exn
              in
              [%test_eq: int]
                (C.reservation catalog
                 |> protocol_ok
                 |> C.Reservation.required_transition_steps)
                expected_steps;
              let next, _ = admit_catalog carrier catalog in
              let after = host_document next in
              let actual =
                Growth.positive_difference (measure after) ~previous:(measure before)
              in
              growth_covers (A.Recurrence_control_capacity.growth reservation) actual;
              assert (
                D.Json.equal
                  (Due.field (D.Document.payload after) "future_execution_host")
                  (`Number "2.00e1"));
              List.iter (C.entries catalog) ~f:(fun entry ->
                let frontier = E.frontier entry |> protocol_ok in
                [%test_eq: int] (H.pending_count frontier) 1;
                [%test_eq: int] (List.length (H.executions frontier)) 1);
              next
            in
            let paused = dispose carrier C.pause_for_stop 5 in
            let retired =
              dispose
                paused
                (C.retire_stale_births
                   ~current_revision:P.Session_lifecycle.Revision.one
                   ~archived:false)
                4
            in
            State.decode ~limits:document_limits (host_document retired)
            |> document_ok
            |> ignore;
            (* Independent actual canonical terminal candidate, starting from
               the pending carrier rather than replaying an already-applied
               frame. This uses real state/delta/transaction encoders, not the
               capacity projection as its expected future. *)
            let reserved =
              State.recurrence_capacity
                pending_carrier
                ~document:pending_document
                ~limits:document_limits
              |> document_ok
            in
            let before = State.value pending_carrier in
            let catalog = Option.value_exn before.recurrence_catalog in
            let entry = C.find catalog (id "first_execution") |> Option.value_exn in
            let execution = List.hd_exn (H.executions (Due.frontier entry)) in
            let origin, terminal =
              run_evidence execution ~completed_at:timestamp ~outcome:(Completed None)
            in
            let catalog, result =
              C.import_execution
                catalog
                ~origin
                ~terminal:(Some terminal)
                ~automatic:Current_policy_allows
              |> protocol_ok
            in
            (match result with
             | Imported _ -> ()
             | Retained_exact _ | Detail_not_retained _ ->
               failwith "new host completion was not imported");
            let settled, _ = admit_catalog pending_carrier catalog in
            let after = host_document settled in
            let actual =
              Growth.positive_difference
                (measure after)
                ~previous:(measure pending_document)
            in
            growth_covers (A.Recurrence_control_capacity.growth reserved) actual;
            assert (
              D.Json.equal
                (Due.field (D.Document.payload after) "future_execution_host")
                (`Number "2.00e1"));
            let entry =
              C.find
                (Option.value_exn (State.value settled).recurrence_catalog)
                (id "first_execution")
              |> Option.value_exn
            in
            [%test_eq: int64] (H.summary (Due.frontier entry)).settled 1L;
            [%test_eq: int] (H.pending_count (Due.frontier entry)) 0;
            let basis = H.delay_basis (Due.frontier entry) |> Option.value_exn in
            assert (
              P.Timestamp.equal (A.Recurrence_delay_basis.completed_at basis) timestamp);
            assert (Origin.equal (A.Recurrence_delay_basis.origin basis) origin);
            assert (Reference.equal (A.Recurrence_delay_basis.terminal basis) terminal);
            let expected_due =
              P.Timestamp.of_string "2026-08-15T12:00:01Z"
              |> protocol_ok
              |> P.Timestamp.to_time_ns
            in
            (match A.Recurrence_cursor.position (E.cursor entry) with
             | Interval next ->
               [%test_eq: int64] (R.Cursor.Interval.slot next) 1L;
               assert (Time_ns.equal (R.Cursor.Interval.due_at next) expected_due)
             | Calendar _ | Awaiting_completion _ | Exhausted ->
               failwith "actual host candidate did not rearm fixed delay");
            let alarm = H.alarm (Due.frontier entry) |> Option.value_exn in
            [%test_eq: int64] (A.Recurrence_alarm.slot alarm) 1L;
            assert (Time_ns.equal (A.Recurrence_alarm.due_at alarm) expected_due);
            Capacity_tests.check_future
              settled
              (Agent_store.Commit_writer.max_payload_length fixture.writer)
            |> protocol_ok;
            let same_catalog, _ =
              C.import_execution
                catalog
                ~origin
                ~terminal:(Some terminal)
                ~automatic:Current_policy_allows
              |> protocol_ok
            in
            [%test_eq: int64] (C.revision same_catalog) (C.revision catalog))));
  print_endline
    "6 -> 5 -> 4 independent credits; actual zero-event deltas/transactions; configured \
     depth and bytes reject before effects";
  [%expect
    {| 6 -> 5 -> 4 independent credits; actual zero-event deltas/transactions; configured depth and bytes reject before effects |}]
;;
