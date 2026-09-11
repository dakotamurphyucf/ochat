open Core
open Fixtures
module P = Agent_protocol
module A = Agent_session.Session_actor
module State = Agent_session.Session_state
module M = Agent_session.Managed_stop
module D = Agent_store.Delegation_store

let%expect_test
    "managed stop atomically persists retry identity and never repeats across new \
     lifetimes"
  =
  let transitions = ref [] in
  Delegation_lifecycle_tests.with_fixture
    ~reject_transition:(fun next ->
      transitions := next :: !transitions;
      false)
    (fun env _sw _ledger record foreign actor _runtime backend _closes reject _original ->
       let reference = D.reference record in
       let writer, _ = A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok in
       let entered, enter = Eio.Promise.create () in
       let exited = ref 0 in
       let worker =
         Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ _caps ->
           Exn.protect
             ~finally:(fun () -> Int.incr exited)
             ~f:(fun () ->
               Eio.Promise.resolve enter ();
               Eio.Fiber.await_cancel ()))
       in
       A.set_operation_worker actor (Some worker) |> protocol_ok;
       A.submit_message
         actor
         ~attachment_id:writer.id
         (Managed_submission_tests.input 400 "Keep running until cancelled.")
       |> protocol_ok
       |> ignore;
       Eio.Promise.await entered;
       A.submit_message
         actor
         ~attachment_id:writer.id
         (Managed_submission_tests.input 401 "Retain for a later explicit resume.")
       |> protocol_ok
       |> ignore;
       let stop ?(generation = 0) ?(maximum = Some 2) reference key mode =
         A.stop_managed
           actor
           ~reference
           ~key:(Managed_submission_tests.key key)
           ~mode
           ~generation
           ~max_receipts:maximum
       in
       let before = A.state actor |> protocol_ok in
       (match stop (D.reference foreign) "foreign" Cancel with
        | Error { code = Permission_denied; _ } -> ()
        | _ -> failwith "foreign relationship stopped child");
       reject := true;
       assert (Result.is_error (stop reference "failed" Cancel));
       assert_same_session_snapshot before (A.state actor |> protocol_ok);
       assert_same_session_snapshot before (Agent_session.Memory_backend.state backend);
       [%test_eq: int] 0 !exited;
       reject := false;
       transitions := [];
       let first, repeated =
         Eio.Fiber.pair
           (fun () -> stop reference "finish" Graceful |> protocol_ok)
           (fun () -> stop reference "finish" Graceful |> protocol_ok)
       in
       assert (M.equal first repeated);
       let graceful = A.state actor |> protocol_ok in
       [%test_eq: int] 1 (List.length graceful.managed_stops);
       [%test_eq: int] 0 !exited;
       assert (Option.is_some graceful.active_operation);
       let operation = Option.value_exn graceful.active_operation in
       assert (
         List.is_empty (A.consume_deferred actor ~operation_id:operation.id |> protocol_ok));
       assert_same_session_snapshot graceful (A.state actor |> protocol_ok);
       let saved_transition = List.last_exn !transitions in
       let replayed =
         Agent_session.Session_delta.sexp_of_t saved_transition.delta
         |> Sexp.to_string_mach
         |> Sexp.of_string
         |> Agent_session.Session_delta.t_of_sexp
         |> Agent_session.Session_delta.apply before
         |> protocol_ok
       in
       assert (List.equal M.equal graceful.managed_stops replayed.managed_stops);
       assert (P.Session.equal_desired_state replayed.lifecycle.desired Stopped);
       [%test_eq: int64] first.stop_epoch replayed.stop_epoch;
       (match stop reference "finish" Cancel with
        | Error { code = Conflict; _ } -> ()
        | _ -> failwith "same key escalated graceful stop");
       (match stop ~maximum:(Some 1) reference "escalate" Cancel with
        | Error { code = Invalid_state; _ } -> ()
        | _ -> failwith "capacity did not prevent a new stop");
       let replay = stop ~maximum:(Some 0) reference "finish" Graceful |> protocol_ok in
       assert (M.equal first replay);
       assert_same_session_snapshot graceful (A.state actor |> protocol_ok);
       [%test_eq: int] 0 !exited;
       let cancel = stop reference "escalate" Cancel |> protocol_ok in
       let rec stopped () =
         let state = A.state actor |> protocol_ok in
         match state.lifecycle.observed with
         | Stopped -> state
         | _ ->
           Eio.Time.sleep (Eio.Stdenv.clock env) 0.001;
           stopped ()
       in
       let stopped_state = stopped () in
       [%test_eq: int] 1 !exited;
       [%test_eq: int] 2 (List.length stopped_state.managed_stops);
       assert (
         List.equal
           P.History.equal_entry
           before.conversation.canonical_history
           stopped_state.conversation.canonical_history);
       let restored =
         State.sexp_of_t stopped_state
         |> Sexp.to_string_mach
         |> Agent_session.Session_persistence.restore_snapshot
         |> store_ok
       in
       assert_same_session_snapshot stopped_state restored;
       assert (
         Result.is_error (State.upgrade_schema { restored with schema_version = 16 }));
       let legacy = { restored with schema_version = 16; managed_stops = [] } in
       [%test_eq: int]
         State.current_schema_version
         (State.upgrade_schema legacy |> protocol_ok).schema_version;
       A.set_operation_worker actor None |> protocol_ok;
       A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
       let restarted = A.state actor |> protocol_ok in
       assert (
         M.equal cancel (stop ~maximum:(Some 0) reference "escalate" Cancel |> protocol_ok));
       assert_same_session_snapshot restarted (A.state actor |> protocol_ok);
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
       (* Even a replacement candidate without the private list must retain keys. *)
       let reset = { reset with managed_stops = [] } in
       A.commit_administration
         actor
         ~command_audit:None
         ~attachment_id:writer.id
         ~expected_revision:before_reset.counters.revision
         ~kind:Reset
         reset
       |> protocol_ok
       |> ignore;
       let replaced = A.state actor |> protocol_ok in
       assert (List.equal M.equal before_reset.managed_stops replaced.managed_stops);
       A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
       let latest = A.state actor |> protocol_ok in
       assert (
         M.equal
           first
           (stop ~generation:latest.identity.generation reference "finish" Graceful
            |> protocol_ok));
       assert_same_session_snapshot latest (A.state actor |> protocol_ok);
       print_endline
         "failed save has no cancellation; concurrent retry admits once; escalation \
          requires a new key";
       print_endline
         "receipt and stop intent replay together; history survives stop; old keys \
          survive restart/reset");
  [%expect
    {|
    failed save has no cancellation; concurrent retry admits once; escalation requires a new key
    receipt and stop intent replay together; history survives stop; old keys survive restart/reset
    |}]
;;
