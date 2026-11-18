open! Core
open Agent_server_test_support
open Retry_support
module Native = Goal_native_start_tests
module Callback = Goal_native_test_support
module Goal = Goal_service_tests
module G = P.Goal
module Request = P.Goal_native_start_request
module Intent = P.Goal_native_start_intent
module Service = Agent_server.Goal_run_service
module Driver = Agent_server.Goal_native_resume_dispatcher
module Accounting = A.Goal_accounting
module Ledger = A.Inference_ledger

let%expect_test
    "a second native Start preserves its terminal predecessor and first receipt"
  =
  let providers = ref 0
  and ports = ref 0 in
  let entered = Array.init 3 ~f:(fun _ -> Eio.Promise.create ()) in
  let released = Array.init 3 ~f:(fun _ -> Eio.Promise.create ()) in
  let release ordinal =
    let promise, resolver = released.(ordinal) in
    if Option.is_none (Eio.Promise.peek promise) then Eio.Promise.resolve resolver ()
  in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr providers;
      let ordinal = !providers - 1 in
      if ordinal >= 3 then failwith "later native Start dispatched an extra provider";
      let _, resolver = entered.(ordinal) in
      Eio.Promise.resolve resolver ();
      Eio.Promise.await (fst released.(ordinal));
      completed_response "actual native episode predecessor fixture")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          incr ports;
          base.runtime_inference_ports actor)
    }
  in
  Native.with_accepted_start
    ~later_callback:Start_on_explicit_metadata
    ~v1:false
    ~policy
    ~providers
    (fun env _sw _root daemon entry fixture ->
       protect_cleanup
         ~finally:(fun () -> Array.iteri released ~f:(fun i _ -> release i))
         (fun () ->
            let writer, _ =
              Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
            in
            let driver = Driver.create ~factory:(Daemon.factory daemon) in
            protect_cleanup
              ~finally:(fun () ->
                Driver.close driver;
                Actor.detach entry.actor writer.id |> protocol_ok)
              (fun () ->
                 Driver.process_entry driver entry |> protocol_ok;
                 Eio.Promise.await (fst entered.(0));
                 let first_state = state entry in
                 let _, first_intent = Native.start_invocation first_state in
                 let first =
                   match Intent.phase first_intent with
                   | Admitted value -> value.admission
                   | Staged | Accepted _ | Dispatch_claimed _ | Discarded _ | Rejected _
                     -> failwith "first native provider lacks its actual admission"
                 in
                 Native.assert_admission_ack env entry fixture.invocation first;
                 let operation = Option.value_exn first_state.active_operation in
                 let first_run =
                   A.Run_state.find (Option.value_exn first_state.run_state) first.run_id
                   |> Option.value_exn
                 in
                 assert (
                   List.exists first_run.owned_work ~f:(fun work ->
                     P.Run_work.Key.equal work.key (Operation operation.id)));
                 Actor.cancel_operation
                   entry.actor
                   ~attachment_id:writer.id
                   ~operation_id:operation.id
                 |> protocol_ok
                 |> ignore;
                 release 0;
                 let rec await_cancelled () =
                   let current =
                     Actor.with_moderator_checkpoint entry.actor (fun () ->
                       Actor.state entry.actor)
                     |> protocol_ok
                   in
                   let run =
                     A.Run_state.find (Option.value_exn current.run_state) first.run_id
                     |> Option.value_exn
                   in
                   let ended =
                     List.for_all (Ledger.rows current.inference_ledger) ~f:(fun row ->
                       match
                         Inference.Observation.Attempt_record.state
                           (Ledger.Row.record row)
                       with
                       | Prepared | Running -> false
                       | Interrupted _ | Terminal _ -> true)
                   in
                   match run.lifecycle with
                   | Terminal Cancelled
                     when Option.is_none current.active_operation && ended -> current
                   | Terminal Cancelled | Admitted | Active | Waiting _ ->
                     Eio.Fiber.yield ();
                     await_cancelled ()
                   | Terminal _ ->
                     failwith "first native episode lacks actual cancelled terminal proof"
                 in
                 let cancelled = await_cancelled () in
                 let old_index = Option.value_exn cancelled.run_state in
                 let old_run =
                   A.Run_state.find old_index first.run_id |> Option.value_exn
                 in
                 let old_link =
                   A.Run_state.goal_link old_index first.run_id |> Option.value_exn
                 in
                 let terminal = Goal_finish_tests.terminal_receipt old_index first in
                 let predecessor =
                   P.Goal_episode.Settlement.create
                     (G.episode (Goal.goal cancelled) |> Option.value_exn)
                     ~run:old_run
                     ~receipt:terminal
                   |> protocol_ok
                 in
                 let old_account = Goal.accounting cancelled in
                 [%test_eq: int64 * int64]
                   (1L, 1L)
                   ( Accounting.admitted_attempts old_account
                   , Accounting.admitted_turns old_account );
                 let old_binding =
                   Accounting.bindings old_account |> Native.one "first native binding"
                 in
                 let old_row =
                   Ledger.find
                     cancelled.inference_ledger
                     ~ordinal:
                       (Ledger.Handle.ordinal (Accounting.Binding.handle old_binding))
                   |> Option.value_exn
                 in
                 let next_input = "second actual native episode input" in
                 let ( selection
                     , second_invocation
                     , accepted
                     , second_request
                     , callback_receipt )
                   =
                   Owner.with_loaded_runtime entry.runtime (fun runtime ->
                     let evidence =
                       Callback.with_ordinary_owner
                         daemon
                         entry
                         ~v1:false
                         (fun _ordinary ->
                            Eio.Promise.await (fst entered.(1));
                            let current = state entry in
                            let goal = Goal.goal current in
                            let request =
                              Request.create
                                ~generation:current.identity.generation
                                ~source:(G.source goal)
                                ~goal_id:Goal.goal_id
                                ~expected_goal_revision:(G.revision goal)
                                ~expected_catalog_revision:
                                  (A.Goal_catalog.revision
                                     (Option.value_exn current.goal_catalog))
                                ~input:
                                  (User_submission
                                     { kind = Plain_text
                                     ; text = next_input
                                     ; attachments = []
                                     })
                                ~key:
                                  (P.Idempotency_key.of_string "native-second-episode"
                                   |> protocol_ok)
                              |> protocol_ok
                            in
                            let operation = Option.value_exn current.active_operation in
                            let result, invocations =
                              Callback.callback
                                entry
                                runtime
                                ~operation
                                ~mode:"start"
                                ~run:(fun admission ->
                                  Actor.stage_native_goal_start
                                    entry.actor
                                    ~admission
                                    ~authorize:Callback.authorize
                                    ~request)
                            in
                            assert (result |> protocol_ok);
                            let invocation =
                              invocations |> Native.one "second actual native invocation"
                            in
                            let invocation, accepted =
                              Native.find_start_invocation
                                (state entry)
                                ~id:invocation.context.id
                            in
                            (match Intent.phase accepted with
                             | Accepted _ -> ()
                             | Staged
                             | Dispatch_claimed _
                             | Admitted _
                             | Discarded _
                             | Rejected _ ->
                               failwith "later actual callback did not ACK native Start");
                            let callback_id = Option.value_exn invocation.parent_event in
                            let callback =
                              List.find_exn
                                (state entry).moderator_executions
                                ~f:(fun callback ->
                                  P.Id.Moderator_execution.equal
                                    callback.context.id
                                    callback_id)
                            in
                            ignore
                              (Native.assert_callback_ack
                                 ~created_key:None
                                 env
                                 entry
                                 invocation
                                 accepted
                                 callback
                               : Agent_store.Transaction.t);
                            release 1;
                            ignore (Callback.await_quiet entry : A.Session_state.t);
                            let selection =
                              match
                                Actor.select_native_goal_action entry.actor |> protocol_ok
                              with
                              | Some (Start selection) -> selection
                              | Some (Resume _) | None ->
                                failwith
                                  "second native Start lost actual scheduling custody"
                            in
                            assert (
                              P.Id.Invocation.equal
                                invocation.context.id
                                (Actor.Goal_native_start.Selection.invocation_id
                                   selection));
                            selection, invocation, accepted, request, callback)
                     in
                     Ok evidence)
                   |> protocol_ok
                 in
                 Driver.process_entry driver entry |> protocol_ok;
                 Eio.Promise.await (fst entered.(2));
                 let current = state entry in
                 let _, second_intent =
                   Native.find_start_invocation current ~id:second_invocation.context.id
                 in
                 let second =
                   match Intent.phase second_intent with
                   | Admitted value -> value.admission
                   | Staged | Accepted _ | Dispatch_claimed _ | Discarded _ | Rejected _
                     -> failwith "second provider lacks actual native admission"
                 in
                 assert (not (P.Id.Run.equal first.run_id second.run_id));
                 Native.assert_admission_ack env entry second_invocation second;
                 assert (Request.equal second_request (Intent.request accepted));
                 assert (
                   P.Id.Invocation.equal
                     second_invocation.context.id
                     (Actor.Goal_native_start.Selection.invocation_id selection));
                 assert (
                   List.exists
                     current.moderator_executions
                     ~f:(P.Moderator_execution.equal callback_receipt));
                 let index = Option.value_exn current.run_state in
                 assert (
                   P.Run.equal
                     old_run
                     (A.Run_state.find index first.run_id |> Option.value_exn));
                 assert (
                   A.Run_goal_link.equal
                     old_link
                     (A.Run_state.goal_link index first.run_id |> Option.value_exn));
                 List.iter [ first; terminal; second ] ~f:(fun receipt ->
                   assert (
                     List.exists
                       (A.Run_state.receipts index)
                       ~f:(P.Run_receipt.equal receipt)));
                 let goal = Goal.goal current in
                 assert (
                   P.Goal_episode.Settlement.equal
                     predecessor
                     (G.previous_episode_settlement goal |> Option.value_exn));
                 assert (
                   P.Run_receipt.equal
                     second
                     (G.episode goal |> Option.value_exn |> P.Goal_episode.admission));
                 let account = Goal.accounting current in
                 [%test_eq: int64 * int64]
                   (2L, 2L)
                   ( Accounting.admitted_attempts account
                   , Accounting.admitted_turns account );
                 [%test_eq: int] 2 (List.length (Accounting.bindings account));
                 assert (
                   List.exists
                     (Accounting.bindings account)
                     ~f:(Accounting.Binding.equal old_binding));
                 let retained_row =
                   Ledger.find
                     current.inference_ledger
                     ~ordinal:(Ledger.Handle.ordinal (Ledger.Row.handle old_row))
                   |> Option.value_exn
                 in
                 assert (
                   Ledger.Handle.equal
                     (Ledger.Row.handle old_row)
                     (Ledger.Row.handle retained_row));
                 assert (
                   Jsonaf.exactly_equal
                     (Inference.Observation.Attempt_record.to_json
                        (Ledger.Row.record old_row))
                     (Inference.Observation.Attempt_record.to_json
                        (Ledger.Row.record retained_row)));
                 [%test_eq: int] 3 !providers;
                 [%test_eq: int]
                   1
                   (List.count current.conversation.canonical_history ~f:(fun item ->
                      P.History.equal_role User item.P.History.role
                      && String.is_substring
                           (Jsonaf.to_string item.payload)
                           ~substring:next_input));
                 let frames = Existing.frames env entry in
                 let counts = !ports, !providers in
                 (match
                    Service.start_native_episode
                      (Daemon.factory daemon)
                      ~entry
                      ~selection:fixture.selection
                    |> protocol_ok
                  with
                  | Retained actual -> assert (P.Run_receipt.equal first actual)
                  | Attempted _ ->
                    failwith "first native historical receipt acquired a new episode");
                 Existing.assert_same current entry;
                 [%test_eq: string list] frames (Existing.frames env entry);
                 [%test_eq: int * int] counts (!ports, !providers);
                 release 2)));
  print_endline
    "second genuine native admission retains cancelled predecessor evidence and account; \
     first native retry returns its original receipt without effects";
  [%expect
    {| second genuine native admission retains cancelled predecessor evidence and account; first native retry returns its original receipt without effects |}]
;;
