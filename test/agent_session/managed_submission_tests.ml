open Core
open Fixtures
module P = Agent_protocol
module A = Agent_session.Session_actor
module M = Agent_session.Managed_submission
module State = Agent_session.Session_state
module D = Agent_store.Delegation_store

let digest = Chatmd_shell_spec.Source_ref.digest
let key value = P.Idempotency_key.of_string value |> protocol_ok

let input number text =
  let id =
    History_entry.Id.create ~namespace:"managed-input" ~sequence:number
    |> Result.ok_or_failwith
  in
  Agent_session.History_codec.user_text ~id text
  |> Agent_session.History_codec.to_protocol
;;

let%expect_test
    "managed send commits once, correlates deferred adoption and preserves terminal \
     receipts"
  =
  let reject = ref false in
  Delegation_lifecycle_tests.with_fixture
    ~reject_transition:(fun _ -> !reject)
    (fun _env
      _sw
      _ledger
      record
      foreign
      actor
      _runtime
      backend
      _closes
      _reject
      _original ->
       let reference = D.reference record in
       let runs = ref 0 in
       let started, started_u = Eio.Promise.create () in
       let adopt, adopt_u = Eio.Promise.create () in
       let output, output_u = Eio.Promise.create () in
       let finish, finish_u = Eio.Promise.create () in
       let worker =
         Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
           Int.incr runs;
           Eio.Promise.resolve started_u ();
           Eio.Promise.await adopt;
           let deferred = caps.consume_deferred () |> protocol_ok in
           let input =
             { input with
               Agent_session.Operation_worker.Input.history = input.history @ deferred
             }
           in
           let summary = completed_worker_result input caps |> protocol_ok in
           Eio.Promise.resolve output_u ();
           Eio.Promise.await finish;
           Completed summary)
       in
       A.set_operation_worker actor (Some worker) |> protocol_ok;
       let send ?(limit = Some 8) ?(generation = 0) reference name number text =
         A.submit_managed_message
           actor
           ~reference
           ~key:(key name)
           ~request_sha256:(digest text)
           ~generation
           ~max_receipts:limit
           (input number text)
       in
       let before = A.state actor |> protocol_ok in
       reject := true;
       assert (Result.is_error (send reference "one" 0 "first"));
       assert_same_session_snapshot before (A.state actor |> protocol_ok);
       assert_same_session_snapshot before (Agent_session.Memory_backend.state backend);
       [%test_eq: int] 0 !runs;
       reject := false;
       let first = send reference "one" 1 "first" |> protocol_ok in
       Eio.Promise.await started;
       let replay = send reference "one" 2 "first" |> protocol_ok in
       assert (M.equal first replay);
       (match send reference "one" 3 "changed" with
        | Error { code = Conflict; _ } -> ()
        | _ -> failwith "changed input did not conflict");
       (match send (D.reference foreign) "foreign" 4 "foreign" with
        | Error { code = Permission_denied; _ } -> ()
        | _ -> failwith "foreign child reference was accepted");
       let second = send reference "two" 5 "second" |> protocol_ok in
       assert (M.equal_status Deferred second.status);
       let queued = A.state actor |> protocol_ok in
       [%test_eq: int] 1 (List.length queued.conversation.deferred_user_entries);
       [%test_eq: int] 2 (List.length queued.managed_submissions);
       assert (Result.is_error (send ~limit:(Some 2) reference "three" 6 "over capacity"));
       assert_same_session_snapshot queued (A.state actor |> protocol_ok);
       Eio.Promise.resolve adopt_u ();
       Eio.Promise.await output;
       let emitted = A.state actor |> protocol_ok in
       let operation = (Option.value_exn emitted.active_operation).id in
       List.iter emitted.managed_submissions ~f:(fun receipt ->
         assert (M.equal_status (Assigned operation) receipt.status);
         [%test_eq: int] 1 (List.length receipt.output_ids));
       Eio.Promise.resolve finish_u ();
       let done_state = await_idle actor in
       List.iter done_state.managed_submissions ~f:(fun receipt ->
         assert (M.equal_status (Terminal (Some operation, Completed)) receipt.status));
       [%test_eq: int] 1 !runs;
       let restored =
         Agent_session.Session_persistence.restore_snapshot
           (State.sexp_of_t done_state |> Sexp.to_string_mach)
         |> store_ok
       in
       assert (
         List.equal M.equal done_state.managed_submissions restored.managed_submissions);
       let receipt = send reference "two" 7 "second" |> protocol_ok in
       assert (M.equal_status (Terminal (Some operation, Completed)) receipt.status);
       [%test_eq: int] 1 !runs;
       let legacy = { done_state with schema_version = 15 } in
       assert (Result.is_error (State.upgrade_schema legacy));
       let legacy = { legacy with managed_submissions = [] } in
       let migrated = State.upgrade_schema legacy |> protocol_ok in
       [%test_eq: int] State.current_schema_version migrated.schema_version;
       let writer, _ = A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok in
       A.stop actor ~attachment_id:writer.id ~mode:Cancel |> protocol_ok |> ignore;
       let before_reset = A.state actor |> protocol_ok in
       let reset =
         Agent_session.Administration.reset
           before_reset
           { keep_history = false
           ; keep_tasks = false
           ; keep_grants = false
           ; keep_labels = true
           ; workspace_instance = None
           }
         |> protocol_ok
       in
       A.commit_administration
         actor
         ~command_audit:None
         ~attachment_id:writer.id
         ~expected_revision:before_reset.counters.revision
         ~kind:Reset
         reset
       |> protocol_ok
       |> ignore;
       let after_reset = A.state actor |> protocol_ok in
       assert (
         List.equal M.equal done_state.managed_submissions after_reset.managed_submissions);
       let replay =
         send ~generation:after_reset.identity.generation reference "one" 8 "first"
         |> protocol_ok
       in
       assert (M.equal_status (Terminal (Some operation, Completed)) replay.status);
       [%test_eq: int] 1 !runs;
       (match
          send ~generation:after_reset.identity.generation reference "new-stopped" 9 "new"
        with
        | Error { code = Invalid_state; _ } -> ()
        | _ -> failwith "managed send silently resumed stopped child");
       print_endline
         "failed save has no effect; duplicate/conflict/foreign/capacity checks preserve \
          admission";
       print_endline
         "deferred input joins one operation; assistant output is not terminal completion";
       print_endline
         "terminal receipts survive snapshot and reset; retry never submits again");
  [%expect
    {|
    failed save has no effect; duplicate/conflict/foreign/capacity checks preserve admission
    deferred input joins one operation; assistant output is not terminal completion
    terminal receipts survive snapshot and reset; retry never submits again
    |}]
;;
