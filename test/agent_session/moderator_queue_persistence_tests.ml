open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module Q = A.Moderator_queue
module SD = A.Session_state_document
module D = Document_schema
module Persistence = A.Session_persistence
module Store = Agent_store

let set = Goal_catalog_tests.set
let member = Goal_catalog_tests.member
let values = Moderator_queue_tests.values

let selected_json restored =
  Persistence.Restored.state_document restored
  |> SD.encode ~limits:document_limits
  |> document_ok
  |> D.Document.payload
  |> fun raw ->
  `Array
    [ member raw "moderator"; member raw "schedules"; member raw "moderator_executions" ]
;;

let checkpoint state =
  let snapshot =
    A.Moderator_checkpoint.decode state.A.Session_state.moderator
    |> protocol_ok
    |> Option.value_exn
  in
  let metadata =
    A.Moderator_checkpoint.queue state.moderator |> protocol_ok |> Option.value_exn
  in
  snapshot, metadata
;;

let captured ?(head_producer = Q.Capture.unavailable) state =
  let source =
    A.Run_source_installation.captured
      (A.Run_state.installation (Option.value_exn state.A.Session_state.run_state))
      ~generation:state.identity.generation
    |> protocol_ok
  in
  let snapshot =
    A.Moderator_checkpoint.decode state.moderator |> protocol_ok |> Option.value_exn
  in
  let setup =
    Q.Capture.create
      ~producer:(Source_setup { basis_revision = 0L })
      ~origin:(Some Unbound)
    |> protocol_ok
  in
  let metadata =
    Q.bootstrap ~source ~queue:[]
    |> protocol_ok
    |> fun empty ->
    Q.advance
      empty
      ~before:[]
      ~after:(values 2)
      ~consume:Keep
      ~appended:[ head_producer; setup ]
    |> protocol_ok
  in
  let snapshot = { snapshot with queued_internal_events = values 2 } in
  let moderator =
    A.Moderator_checkpoint.encode_with_queue snapshot ~queue:metadata |> protocol_ok
  in
  let schedule : P.Schedule.t =
    { id = P.Id.Schedule.of_string "sch_queue_disk" |> protocol_ok
    ; session_id
    ; generation = state.identity.generation
    ; payload = `String "timer"
    ; created_at = timestamp
    ; next_due_at = timestamp
    ; misfire = Deliver_once_immediately
    ; status = Scheduled
    ; delivery_count = 0
    ; last_delivery_at = None
    ; ownership = None
    ; inference_provenance =
        Some (P.Schedule_provenance.create ~source ~origin:(Some Unbound) |> protocol_ok)
    ; delivery_cancellation = None
    }
  in
  let state =
    { state with
      moderator = Some moderator
    ; schedules = [ schedule ]
    ; counters = { state.counters with transaction_sequence = 0L; event_sequence = 0L }
    }
  in
  let raw =
    SD.authored state
    |> SD.encode ~limits:document_limits
    |> document_ok
    |> D.Document.json
  in
  let payload = member raw "payload" in
  let moderator = member payload "moderator" in
  let queue = member moderator "queue_provenance" in
  let rows =
    match member queue "entries" with
    | `Array rows -> rows
    | _ -> assert false
  in
  let rows =
    List.mapi rows ~f:(fun index row ->
      if index = 1
      then set row "future_occurrence" (`Object [ "retained", `True ])
      else row)
  in
  let moderator = set moderator "queue_provenance" (set queue "entries" (`Array rows)) in
  let schedule = P.Schedule.Storage.to_json schedule in
  let provenance = member schedule "inference_provenance" in
  let provenance =
    set
      provenance
      "origin"
      (set (member provenance "origin") "future_origin" (`String "kept"))
  in
  let schedule = set schedule "inference_provenance" provenance in
  let payload =
    set (set payload "moderator" moderator) "schedules" (`Array [ schedule ])
  in
  D.Document.inspect ~limits:document_limits (set raw "payload" payload)
  |> document_ok
  |> SD.decode ~limits:document_limits
  |> document_ok
  |> Persistence.Restored.of_state_document
;;

let captured_event state =
  let module F = Moderator_queue_tests.Consumed_receipt in
  let head_producer =
    Q.Capture.create ~producer:(Source_setup { basis_revision = 0L }) ~origin:None
    |> protocol_ok
  in
  let restored = captured ~head_producer state in
  let before = Persistence.Restored.state restored in
  let running = F.running before None in
  let initial =
    { before with moderator_executions = running :: before.moderator_executions }
  in
  let raw =
    SD.with_value (Persistence.Restored.state_document restored) initial
    |> SD.encode ~limits:document_limits
    |> document_ok
    |> D.Document.json
  in
  let payload = member raw "payload" in
  let receipts =
    match member payload "moderator_executions" with
    | `Array receipts -> receipts
    | _ -> assert false
  in
  let receipts =
    List.map receipts ~f:(fun receipt ->
      if
        D.Json.equal
          (member receipt "id")
          (P.Id.Moderator_execution.to_json running.context.id)
      then
        set
          receipt
          "queue_occurrence"
          (let capture = member receipt "queue_occurrence" in
           let producer = member capture "producer" in
           set
             (set capture "future_occurrence" (`String "disk-retained"))
             "producer"
             (set producer "future_producer" (`String "disk-producer")))
      else receipt)
  in
  set raw "payload" (set payload "moderator_executions" (`Array receipts))
  |> D.Document.inspect ~limits:document_limits
  |> document_ok
  |> SD.decode ~limits:document_limits
  |> document_ok
  |> Persistence.Restored.of_state_document
;;

let complete_event before =
  let module F = Moderator_queue_tests.Consumed_receipt in
  let running =
    List.find_exn before.A.Session_state.moderator_executions ~f:(fun receipt ->
      Option.is_some receipt.P.Moderator_execution.queue_occurrence)
  in
  let completed, moderator = F.completion before running in
  let snapshot =
    A.Moderator_checkpoint.decode (Some moderator) |> protocol_ok |> Option.value_exn
  in
  let queue =
    A.Moderator_checkpoint.queue (Some moderator) |> protocol_ok |> Option.value_exn
  in
  let moderator =
    A.Moderator_checkpoint.update_with_queue before.moderator ~snapshot ~queue
    |> protocol_ok
  in
  A.Session_transition.apply
    ~now:timestamp
    before
    ~delta:
      (Batch [ Moderator_execution_changed completed; Moderator_changed (Some moderator) ])
    ~payloads:[]
  |> protocol_ok
;;

let delivery before =
  let snapshot, metadata = checkpoint before in
  let schedule = List.hd_exn before.A.Session_state.schedules in
  let schedule =
    { schedule with
      status = Delivered
    ; delivery_count = 1
    ; last_delivery_at = Some timestamp
    }
  in
  let capture =
    Q.Capture.create
      ~producer:
        (Schedule_delivery
           { id = schedule.id; generation = schedule.generation; delivery_count = 1 })
      ~origin:(Some Unbound)
    |> protocol_ok
  in
  let after =
    List.tl_exn snapshot.queued_internal_events @ [ Session.Snapshot.String "timer" ]
  in
  let metadata =
    Q.advance
      metadata
      ~before:snapshot.queued_internal_events
      ~after
      ~consume:Consume_head
      ~appended:[ capture ]
    |> protocol_ok
  in
  let moderator =
    A.Moderator_checkpoint.update_with_queue
      before.moderator
      ~snapshot:{ snapshot with queued_internal_events = after }
      ~queue:metadata
    |> protocol_ok
  in
  A.Session_transition.apply
    ~now:timestamp
    before
    ~delta:(Batch [ Moderator_changed (Some moderator); Schedule_changed schedule ])
    ~payloads:[]
  |> protocol_ok
;;

let append before =
  let snapshot, metadata = checkpoint before in
  let after = snapshot.queued_internal_events @ [ Session.Snapshot.String "later" ] in
  let capture =
    Q.Capture.create
      ~producer:
        (Source_setup { basis_revision = before.A.Session_state.counters.revision })
      ~origin:(Some Unbound)
    |> protocol_ok
  in
  let metadata =
    Q.advance
      metadata
      ~before:snapshot.queued_internal_events
      ~after
      ~consume:Keep
      ~appended:[ capture ]
    |> protocol_ok
  in
  let moderator =
    A.Moderator_checkpoint.update_with_queue
      before.moderator
      ~snapshot:{ snapshot with queued_internal_events = after }
      ~queue:metadata
    |> protocol_ok
  in
  A.Session_transition.apply
    ~now:timestamp
    before
    ~delta:(Moderator_changed (Some moderator))
    ~payloads:[]
  |> protocol_ok
;;

(* Imported data grants no execution permission. The same physical writer and
   recovery oracle qualifies both ordered Schedule delivery and captured Event
   completion, including their exact queue and unfamiliar retained children. *)
let%expect_test
    "queue, Schedule and consumed Event facts survive physical failure and recovery"
  =
  List.iter [ `Schedule; `Event ] ~f:(fun kind ->
    List.iter [ false; true ] ~f:(fun install_checkpoint ->
      Run_admission_tests.with_startup_actor
        (fun actor _ _ request start _ _ _ _ ->
           start (request "queue-disk") |> protocol_ok |> ignore;
           let state = A.Session_actor.state actor |> protocol_ok in
           match kind with
           | `Schedule -> captured state
           | `Event -> captured_event state)
        ~after_close:(fun env workspace initial ->
          let directory =
            Filename.concat workspace.canonical_root.native_path "queue-journal"
          in
          let snapshots =
            Filename.concat workspace.canonical_root.native_path "queue-snapshots"
          in
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / snapshots);
          let journal open_ =
            open_
              ~env
              ~directory
              ~max_payload_length:1048576
              ~max_segment_bytes:4194304L
              ~max_segment_frames:16
            |> store_ok
          in
          let expected =
            Eio.Switch.run (fun sw ->
              let writer =
                Store.Commit_writer.create
                  ~sw
                  ~journal:(journal Store.Journal.create)
                  ~session_id
                  ~next_transaction_sequence:1L
                  ~previous_transaction_hash:None
                  ~queue_capacity:8
                |> store_ok
              in
              Exn.protect
                ~finally:(fun () -> Store.Commit_writer.close writer)
                ~f:(fun () ->
                  let reject = ref true in
                  let before_commit_calls = ref 0 in
                  let persistence =
                    Persistence.create
                      ~archive:(fun _ _ -> failwith "unexpected archive")
                      ~pending_archive:None
                      ~before_commit:
                        (Some
                           (fun _ ->
                             incr before_commit_calls;
                             if !reject
                             then Error (handoff_error "before queued occurrence write")
                             else Ok ()))
                      ~command_accepted:(fun _ _ -> ())
                      ~writer
                      ~durability:Flush
                      ~limits:document_limits
                      ~archive_limits:document_limits
                      ~retention_preflight:None
                      ~restored:initial
                      ~previous_transaction_hash:None
                  in
                  let before = Persistence.Restored.state initial in
                  let candidate =
                    match kind with
                    | `Schedule -> delivery before
                    | `Event -> complete_event before
                  in
                  (match
                     Persistence.commit
                       persistence
                       ~command_audit:None
                       ~previous:before
                       candidate
                   with
                   | Error error
                     when P.Error.equal_code error.code Internal_error
                          && String.equal error.message "before queued occurrence write"
                     -> ()
                   | Error error -> protocol_ok (Error error)
                   | Ok () -> failwith "injected before_commit rejection was ignored");
                  assert (
                    D.Json.equal
                      (selected_json initial)
                      (selected_json (Persistence.restored persistence)));
                  [%test_eq: int] !before_commit_calls 1;
                  reject := false;
                  let lose_response () =
                    Persistence.commit
                      persistence
                      ~command_audit:None
                      ~previous:before
                      candidate
                    |> protocol_ok;
                    Error (handoff_error "lost response after queued occurrence commit")
                  in
                  assert (Result.is_error (lose_response ()));
                  [%test_eq: int] !before_commit_calls 2;
                  let accepted = Persistence.restored persistence in
                  let state = Persistence.Restored.state accepted in
                  let _, metadata = checkpoint state in
                  [%test_eq: int64] (Q.head_ordinal metadata) 1L;
                  [%test_eq: int64] (Q.next_ordinal metadata) 3L;
                  [%test_eq: int64] state.counters.transaction_sequence 1L;
                  let candidate = append state in
                  Persistence.commit
                    persistence
                    ~command_audit:None
                    ~previous:state
                    candidate
                  |> protocol_ok;
                  let expected = Persistence.restored persistence in
                  let state = Persistence.Restored.state expected in
                  (match kind with
                   | `Schedule ->
                     assert (
                       Result.is_error
                         (SD.with_value
                            (Persistence.Restored.state_document expected)
                            { state with schedules = [] }
                          |> SD.encode ~limits:document_limits))
                   | `Event ->
                     let events =
                       List.filter state.moderator_executions ~f:(fun receipt ->
                         Option.is_none receipt.P.Moderator_execution.queue_occurrence)
                     in
                     assert (
                       Result.is_error
                         (SD.with_value
                            (Persistence.Restored.state_document expected)
                            { state with moderator_executions = events }
                          |> SD.encode ~limits:document_limits)));
                  if install_checkpoint
                  then
                    ignore
                      (Persistence.install_snapshot_at
                         persistence
                         ~env
                         ~directory:snapshots
                         ~max_payload_length:1048576
                         ~transaction_hash:(Persistence.transaction_hash persistence)
                         state
                       |> store_ok
                       : Store.Snapshot.installed);
                  expected))
          in
          let recovered =
            Store.Recovery.load
              ~env
              ~journal:(journal Store.Journal.open_existing)
              ~snapshot_directory:snapshots
              ~max_snapshot_payload_length:1048576
              ~session_id
              ~initial
              ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
              ~apply:(Persistence.apply_transaction ~limits:document_limits)
              ~validate_transaction:
                (Persistence.validate_transaction ~limits:document_limits)
              ~validate:Persistence.validate
            |> store_ok
          in
          assert (D.Json.equal (selected_json expected) (selected_json recovered.state));
          let state = Persistence.Restored.state recovered.state in
          let _, metadata = checkpoint state in
          [%test_eq: int64] (Q.head_ordinal metadata) 1L;
          [%test_eq: int64] (Q.next_ordinal metadata) 4L;
          [%test_eq: int64] state.counters.transaction_sequence 2L;
          match kind with
          | `Schedule ->
            assert (
              List.for_all state.schedules ~f:(fun schedule ->
                match schedule.P.Schedule.status, schedule.inference_provenance with
                | Delivered, Some provenance ->
                  Option.equal
                    P.Inference_origin.equal
                    (Some Unbound)
                    (P.Schedule_provenance.origin provenance)
                | (Scheduled | Delivering | Cancelled | Failed _), _ | Delivered, None ->
                  false))
          | `Event ->
            let receipt =
              List.find_exn state.moderator_executions ~f:(fun receipt ->
                Option.is_some receipt.P.Moderator_execution.queue_occurrence)
            in
            let capture = Option.value_exn receipt.queue_occurrence in
            assert (
              P.Moderator_execution.Queue_occurrence.Producer.equal
                (P.Moderator_execution.Queue_occurrence.producer capture)
                (Captured (Some (P.Queue_producer.Source_setup { basis_revision = 0L }))));
            (match receipt.status with
             | Completed _ -> ()
             | Running | Failed _ | Interrupted _ ->
               failwith "Event completion lost on disk");
            assert (
              Option.is_none
                (A.Invocation_inference_provenance.of_event
                   ~runs:state.run_state
                   ~jobs:state.jobs
                   receipt
                 |> protocol_ok));
            let payload =
              Persistence.Restored.state_document recovered.state
              |> SD.encode ~limits:document_limits
              |> document_ok
              |> D.Document.payload
            in
            let receipts =
              match member payload "moderator_executions" with
              | `Array receipts -> receipts
              | _ -> assert false
            in
            let raw =
              List.find_exn receipts ~f:(fun raw ->
                D.Json.equal
                  (member raw "id")
                  (P.Id.Moderator_execution.to_json receipt.context.id))
            in
            assert (
              D.Json.equal
                (member
                   (member (member raw "queue_occurrence") "producer")
                   "future_producer")
                (`String "disk-producer"));
            assert (
              D.Json.equal
                (member (member raw "queue_occurrence") "future_occurrence")
                (`String "disk-retained")))));
  print_endline
    "failed save unchanged; one accepted lost response; journal/checkpoint retain exact \
     future fields for Schedule and Event";
  [%expect
    {| failed save unchanged; one accepted lost response; journal/checkpoint retain exact future fields for Schedule and Event |}]
;;

(* The Run below is admitted by the actual startup service. The registration,
   receipt and queue remain DATA; no decoded correlation grants execution. *)
let%expect_test "ingress queue provenance binds the retained Run, receipt and exact frame"
  =
  let module I = A.External_ingress in
  let module DD = A.Session_delta_document in
  Run_admission_tests.with_startup_actor (fun actor _ _ request start _ _ _ _ ->
    start (request "ingress-provenance") |> protocol_ok |> ignore;
    let state = A.Session_actor.state actor |> protocol_ok in
    let run = A.Run_state.runs (Option.value_exn state.run_state) |> List.hd_exn in
    let source = run.P.Run.source in
    let origin = Some (P.Inference_origin.Run run.id) in
    let provenance = I.Provenance.create ~source ~origin |> protocol_ok in
    let invocation = invocation_fixture () in
    let template = Subscription_transaction_tests.make_subscription invocation in
    let subscription =
      P.Subscription.create { template.context with source = Some source.observer }
      |> protocol_ok
    in
    let _, template = External_ingress_tests.fixture () in
    let initial =
      I.create
        ~inference_provenance:provenance
        { template.context with
          source = source.observer
        ; subscription_id = subscription.context.id
        }
        ~subscription
      |> protocol_ok
    in
    let registered =
      { state with
        subscriptions = subscription :: state.subscriptions
      ; invocations = invocation :: state.invocations
      ; ingress_registrations = [ initial ]
      }
    in
    A.Session_state.validate registered |> protocol_ok;
    let historical =
      I.of_jsonaf (set (I.to_jsonaf initial) "inference_provenance" `Null) |> protocol_ok
    in
    let historical_state = { registered with ingress_registrations = [ historical ] } in
    let historical_document =
      SD.authored historical_state |> SD.encode ~limits:document_limits |> document_ok
    in
    assert (
      not
        (List.mem
           (D.Document.required_semantics historical_document)
           P.Session_storage_semantics.ingress_provenance
           ~equal:String.equal));
    let historical_carrier =
      SD.decode ~limits:document_limits historical_document |> document_ok
    in
    assert (
      Result.is_error
        (SD.encode (SD.with_value historical_carrier registered) ~limits:document_limits));
    let accepted ?(value = "same") key now =
      I.prepare
        initial
        ~session_id
        ~generation:state.identity.generation
        ~source:source.observer
        ~subscription
        ~producer:initial.context.producer
        ~namespace:initial.context.namespace
        ~key:(P.Idempotency_key.of_string key |> protocol_ok)
        ~payload:(External_ingress_tests.report value)
        ~now
        ~create_event_id:P.Id.Ingress_event.create
      |> protocol_ok
      |> External_ingress_tests.accepted
    in
    let registration, receipt = accepted "first" (External_ingress_tests.at 100) in
    let alternative, _ =
      accepted ~value:"changed" "other" (External_ingress_tests.at 100)
    in
    let frame = I.delivery_frame registration receipt |> protocol_ok in
    let payload =
      Chat_response.Ingress_delivery.capture frame
      |> Session.Snapshot.of_value
      |> Result.ok_or_failwith
    in
    let before =
      A.Moderator_checkpoint.decode registered.moderator
      |> protocol_ok
      |> Option.value_exn
    in
    let snapshot =
      { before with queued_internal_events = before.queued_internal_events @ [ payload ] }
    in
    let plan =
      A.External_delivery_checkpoint.prepare
        registered
        ~producer:(Ingress { registration; receipt })
        ~before
        ~snapshot:(Known snapshot)
      |> protocol_ok
    in
    let entry = A.External_delivery_checkpoint.entry plan in
    assert (
      Option.equal
        A.Queue_producer.equal
        (Q.Entry.producer entry)
        (Some
           (Ingress_delivery
              { registration_id = registration.context.id; event_id = receipt.id })));
    assert (Option.equal P.Inference_origin.equal origin (Q.Entry.origin entry));
    let delta =
      A.Session_delta.Batch
        [ Ingress_changed registration
        ; Moderator_changed (Some (A.External_delivery_checkpoint.checkpoint plan))
        ]
    in
    let transition =
      A.Session_transition.apply
        ~now:(External_ingress_tests.at 100)
        registered
        ~delta
        ~payloads:[]
      |> protocol_ok
    in
    let encoded = delta_document transition.delta in
    let transaction =
      Store.Transaction.create
        ~limits:document_limits
        ~session_id
        ~generation:0
        ~transaction_sequence:transition.state.counters.transaction_sequence
        ~previous_transaction_hash:None
        ~session_revision:transition.state.counters.revision
        ~first_event_sequence:None
        ~last_event_sequence:None
        ~accepted_at_ns:
          (P.Timestamp.to_time_ns (External_ingress_tests.at 100)
           |> Time_ns.to_int63_ns_since_epoch
           |> Int63.to_int64)
        ~command_audit:None
        ~delta:encoded
        ~durable_events:[]
      |> store_ok
      |> Store.Transaction.encode
      |> Store.Transaction.decode
      |> store_ok
    in
    let replayed = replay_transaction registered transaction |> store_ok in
    assert_same_session_snapshot transition.state replayed;
    let document =
      SD.authored replayed |> SD.encode ~limits:document_limits |> document_ok
    in
    let marker = P.Session_storage_semantics.ingress_provenance in
    assert (List.mem (D.Document.required_semantics document) marker ~equal:String.equal);
    assert (List.mem (D.Document.required_semantics encoded) marker ~equal:String.equal);
    let created =
      DD.create (Created replayed) ~limits:document_limits ~state_document:SD.authored
      |> document_ok
      |> DD.document
    in
    assert (List.mem (D.Document.required_semantics created) marker ~equal:String.equal);
    let strip document =
      let raw = D.Document.json document in
      let required =
        D.Document.required_semantics document
        |> List.filter ~f:(fun item -> not (String.equal item marker))
      in
      set
        raw
        "required_semantics"
        (`Array (List.map required ~f:(fun value -> `String value)))
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    assert (Result.is_error (SD.decode ~limits:document_limits (strip document)));
    assert (Result.is_error (DD.decode ~limits:document_limits (strip encoded)));
    assert (Result.is_error (DD.decode ~limits:document_limits (strip created)));
    let old_supported =
      List.filter A.Session_storage_semantics.supported ~f:(fun item ->
        not (String.equal item marker))
    in
    let old_reader =
      D.Domain_codec.create
        ~limits:document_limits
        ~kind:(D.Document.kind document)
        ~version:(D.Document.version document)
        ~shape:SD.shape
        ~supported_semantics:old_supported
        ~decode:(fun _ -> Ok ())
        ~encode:(fun () -> Ok (`Object []))
      |> document_ok
    in
    (match D.Domain_codec.decode old_reader document with
     | Error (D.Error.Required_semantics_unknown name) -> [%test_eq: string] marker name
     | Error error -> raise_s [%sexp (error : D.Error.t)]
     | Ok _ -> failwith "reader without ingress provenance support accepted new DATA");
    let raw = D.Document.json document in
    let raw_payload = member raw "payload" in
    let registration_raw = I.to_jsonaf registration in
    let provenance_raw = member registration_raw "inference_provenance" in
    let decode_registration value =
      I.of_jsonaf (set registration_raw "inference_provenance" value) |> protocol_ok
    in
    let removed = decode_registration `Null in
    let unbound =
      I.Provenance.create ~source ~origin:(Some Unbound)
      |> protocol_ok
      |> I.Provenance.to_jsonaf
      |> decode_registration
    in
    List.iter [ removed; unbound ] ~f:(fun changed ->
      assert (Result.is_error (A.Session_delta.apply replayed (Ingress_changed changed)));
      let carrier = SD.decode ~limits:document_limits document |> document_ok in
      assert (
        Result.is_error
          (SD.encode
             (SD.with_value carrier { replayed with ingress_registrations = [ changed ] })
             ~limits:document_limits)));
    let corrupt_registration value =
      set raw "payload" (set raw_payload "ingress_registrations" (`Array [ value ]))
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
      |> SD.decode ~limits:document_limits
    in
    assert (Result.is_error (corrupt_registration (I.to_jsonaf alternative)));
    let alternative_raw = I.to_jsonaf alternative in
    let alternative_receipt =
      match member alternative_raw "receipts" with
      | `Array [ raw_receipt ] ->
        set raw_receipt "id" (P.Id.Ingress_event.to_json receipt.id)
      | _ -> assert false
    in
    let changed_payload =
      set alternative_raw "receipts" (`Array [ alternative_receipt ])
    in
    I.of_jsonaf changed_payload |> protocol_ok |> ignore;
    assert (Result.is_error (corrupt_registration changed_payload));
    assert (
      Result.is_error
        (corrupt_registration (set registration_raw "inference_provenance" `Null)));
    let wrong_source =
      set
        (member provenance_raw "source")
        "installation_epoch"
        (`String (Int64.to_string (Int64.succ source.installation_epoch)))
    in
    assert (
      Result.is_error
        (corrupt_registration
           (set
              registration_raw
              "inference_provenance"
              (set provenance_raw "source" wrong_source))));
    let extended = set provenance_raw "future" (`Object [ "retained", `True ]) in
    let extended =
      set
        extended
        "source"
        (set (member extended "source") "future_source" (`String "kept"))
    in
    let extended =
      set
        extended
        "origin"
        (set (member extended "origin") "future_origin" (`String "kept"))
    in
    let raw =
      set
        raw
        "payload"
        (set
           raw_payload
           "ingress_registrations"
           (`Array [ set registration_raw "inference_provenance" extended ]))
    in
    let raw_moderator = member (member raw "payload") "moderator" in
    let queue = member raw_moderator "queue_provenance" in
    let entries =
      match member queue "entries" with
      | `Array entries ->
        List.map entries ~f:(fun row ->
          match member row "producer" with
          | `Object _ as producer
            when Jsonaf.exactly_equal
                   (member producer "kind")
                   (`String "ingress_delivery") ->
            set row "producer" (set producer "future_producer" (`String "kept"))
          | _ -> row)
      | _ -> assert false
    in
    let raw_moderator =
      set raw_moderator "queue_provenance" (set queue "entries" (`Array entries))
    in
    let extended_document =
      set raw "payload" (set (member raw "payload") "moderator" raw_moderator)
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    let carrier = SD.decode ~limits:document_limits extended_document |> document_ok in
    let roundtrip =
      SD.encode carrier ~limits:document_limits |> document_ok |> D.Document.json
    in
    assert (
      Jsonaf.exactly_equal (member (member roundtrip "payload") "moderator") raw_moderator);
    let retained = member (member roundtrip "payload") "ingress_registrations" in
    assert (
      Jsonaf.exactly_equal
        retained
        (`Array [ set registration_raw "inference_provenance" extended ]));
    assert (
      Result.is_error
        (corrupt_registration
           (set
              registration_raw
              "inference_provenance"
              (set provenance_raw "future" (`String (String.make 4096 'x'))))));
    let delta_raw = D.Document.json encoded in
    let delta_payload = member delta_raw "payload" in
    let changes =
      match member delta_payload "changes" with
      | `Array changes ->
        List.map changes ~f:(fun change ->
          if Jsonaf.exactly_equal (member change "kind") (`String "ingress_changed")
          then
            set
              change
              "value"
              (set
                 registration_raw
                 "inference_provenance"
                 (set provenance_raw "future" (`String (String.make 4096 'x'))))
          else change)
      | _ -> assert false
    in
    let oversized_delta =
      set delta_raw "payload" (set delta_payload "changes" (`Array changes))
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    assert (Result.is_error (DD.decode ~limits:document_limits oversized_delta));
    print_endline
      "actual Run DATA, exact receipt/frame, immutable whole state, raw extensions and \
       reader fences");
  [%expect
    {| actual Run DATA, exact receipt/frame, immutable whole state, raw extensions and reader fences |}]
;;
