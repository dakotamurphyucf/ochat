open Core
open Fixtures
module P = Agent_protocol
module M = Agent_session.Managed_submission
module State = Agent_session.Session_state
module Delta = Agent_session.Session_delta

let step before delta payloads =
  let next =
    Agent_session.Session_transition.apply ~now:timestamp before ~delta ~payloads
    |> protocol_ok
  in
  let encoded = Delta.sexp_of_t next.delta |> Sexp.to_string_mach in
  let replayed =
    Delta.apply before (Sexp.of_string encoded |> Delta.t_of_sexp) |> protocol_ok
  in
  assert (List.equal M.equal next.state.managed_submissions replayed.managed_submissions);
  next.state
;;

let only state = List.hd_exn state.State.managed_submissions

let%expect_test
    "receipt journal replay preserves adoption through compaction and interrupted \
     completion"
  =
  Delegation_lifecycle_tests.with_fixture
    (fun
        _env
         _sw
         _ledger
         record
         _foreign
         actor
         _runtime
         _backend
         _closes
         _reject
         _original
       ->
       let original = Agent_session.Session_actor.state actor |> protocol_ok in
       let entry = Managed_submission_tests.input 20 "queued before compaction" in
       let receipt =
         M.create
           ~reference:(Agent_store.Delegation_store.reference record)
           ~key:(Managed_submission_tests.key "compacted-send")
           ~request_sha256:(Managed_submission_tests.digest "message")
           ~generation:0
           ~history_id:entry.id
           ~now:timestamp
         |> protocol_ok
       in
       let queued =
         step
           original
           (Batch
              [ Managed_submission_admitted receipt; Deferred_entries_enqueued [ entry ] ])
           [ History_message_deferred entry ]
       in
       let adopted =
         step queued Deferred_entries_adopted [ History_appended [ entry ] ]
       in
       assert (M.equal_status (only adopted).status Ready);
       let summary = Managed_submission_tests.input 21 "summary of the pending request" in
       let compacted =
         step
           adopted
           (Batch
              [ Canonical_history_replaced [ summary ]; Compaction_generation_changed 1 ])
           []
       in
       assert (M.equal_status (only compacted).status Ready);
       assert (
         not
           (List.exists compacted.conversation.canonical_history ~f:(fun value ->
              P.History.Id.equal value.id receipt.history_id)));
       let restored =
         State.sexp_of_t compacted
         |> Sexp.to_string_mach
         |> Agent_session.Session_persistence.restore_snapshot
         |> store_ok
       in
       assert (M.equal (only compacted) (only restored));
       let operation =
         P.Operation.
           { id = P.Id.Operation.create ()
           ; generation = 0
           ; kind = Turn Moderator_request
           ; state = Starting
           ; started_at = timestamp
           ; updated_at = timestamp
           }
       in
       let running =
         step
           restored
           (Batch
              [ Active_operation_changed (Some operation)
              ; Lifecycle_changed
                  { desired = Running; observed = Running_turn operation.id }
              ])
           [ Operation_started operation ]
       in
       let assigned = only running in
       assert (M.equal_status assigned.status (Assigned operation.id));
       let orphaned = { running with active_operation = None } in
       assert (
         Result.is_error
           (Agent_session.Session_persistence.restore_snapshot
              (State.sexp_of_t orphaned |> Sexp.to_string_mach)));
       let interrupted_operation =
         { operation with
           state = Interrupted { reason = "daemon restart"; retryable = false }
         }
       in
       let interrupted =
         step
           running
           (Batch
              [ Active_operation_changed None
              ; Lifecycle_changed { desired = Running; observed = Idle }
              ])
           [ Operation_interrupted interrupted_operation ]
       in
       assert (
         M.equal_status
           (only interrupted).status
           (Terminal (Some operation.id, Interrupted)));
       assert (
         Result.is_error (Delta.apply interrupted (Managed_submission_changed assigned)));
       let reset =
         Agent_session.Administration.reset
           queued
           { keep_history = false
           ; keep_tasks = false
           ; keep_grants = false
           ; keep_labels = true
           ; workspace_instance = None
           }
         |> protocol_ok
       in
       let reset = step queued (Created reset) [] in
       assert (M.equal_status (only reset).status (Terminal (None, Invalidated)));
       assert (M.same_key (only reset) receipt);
       let removed = step adopted (Canonical_history_replaced []) [] in
       assert (M.equal_status (only removed).status (Terminal (None, Invalidated)));
       print_endline
         "adopted input survives compaction and snapshot without pretending to complete";
       print_endline
         "later operation interruption is durable; old assignment cannot overwrite it";
       print_endline
         "reset and explicit removal invalidate unresolved input while retaining its key");
  [%expect
    {|
    adopted input survives compaction and snapshot without pretending to complete
    later operation interruption is durable; old assignment cannot overwrite it
    reset and explicit removal invalidate unresolved input while retaining its key
    |}]
;;
