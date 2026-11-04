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

let captured state =
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
      ~appended:[ Q.Capture.unavailable; setup ]
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
  let restored = captured state in
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
          (set
             (member receipt "queue_occurrence")
             "future_occurrence"
             (`String "disk-retained"))
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
                (member (member raw "queue_occurrence") "future_occurrence")
                (`String "disk-retained")))));
  print_endline
    "failed save unchanged; one accepted lost response; journal/checkpoint retain exact \
     future fields for Schedule and Event";
  [%expect
    {| failed save unchanged; one accepted lost response; journal/checkpoint retain exact future fields for Schedule and Event |}]
;;
