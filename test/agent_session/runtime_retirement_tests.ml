open! Core
open Fixtures
module A = Agent_session.Session_actor
module Worker = Agent_session.Operation_worker
module P = Agent_protocol

let user_entry actor text =
  let ids =
    Agent_session.History_id_source.create
      ~namespace:(P.Id.Session.to_string session_id)
      ~block_size:1
      ~reserve:(fun ~count -> A.reserve_history_block actor ~count)
    |> protocol_ok
  in
  let id = Agent_session.History_id_source.allocate ids |> protocol_ok in
  Agent_session.History_codec.user_text ~id text
  |> Agent_session.History_codec.to_protocol
;;

let install actor worker = A.set_operation_worker actor (Some worker) |> protocol_ok
let detach actor = A.set_runtime_worker actor ~worker:None ~inference:None |> protocol_ok

let assert_conflict result =
  match result with
  | Error { P.Error.code = Conflict; _ } -> ()
  | Error error -> raise_s [%sexp "unexpected admission error", (error : P.Error.t)]
  | Ok _ -> failwith "retiring runtime admitted new work"
;;

let%expect_test "retirement before Worker_ready prevents foreground dispatch" =
  let on_started = ref (fun () -> ()) in
  Job_fixtures.with_actor
    ~state_committed:(fun _ events ->
      if
        List.exists events ~f:(fun (event : P.Event.Durable.t) ->
          P.Event.Durable.equal_kind event.kind Operation_started)
      then !on_started ())
    (fun _env sw actor writer _backend ->
       let dispatched = ref 0 in
       let worker =
         Worker.create ~run:(fun ~sw:_ ~input:_ _ ->
           Int.incr dispatched;
           failwith "retired worker reached execution")
       in
       install actor worker;
       let retirement, retirement_u = Eio.Promise.create () in
       let entered, entered_u = Eio.Promise.create () in
       (on_started
        := fun () ->
             (* Queue priority retirement while the actor still owns the Starting
           commit, before launch_worker can register Worker_ready. The child
           signals immediately before its mailbox call; the actor remains free
           to finish this commit and process that call. *)
             Eio.Fiber.fork ~sw (fun () ->
               Eio.Promise.resolve entered_u ();
               let join = A.retire_runtime_worker actor ~closing:false |> protocol_ok in
               Eio.Promise.resolve retirement_u join);
             Eio.Promise.await entered;
             Eio.Fiber.yield ());
       let submission =
         A.submit_message actor ~attachment_id:writer.id (user_entry actor "first")
         |> protocol_ok
       in
       let join = Eio.Promise.await retirement in
       A.Runtime_retirement.await join |> protocol_ok;
       assert (A.Runtime_retirement.is_finished join);
       [%test_eq: int] 0 !dispatched;
       let state = A.state actor |> protocol_ok in
       assert (Option.is_none state.active_operation);
       [%test_eq: P.Session.desired_state] Running state.lifecycle.desired;
       assert (
         List.exists state.conversation.canonical_history ~f:(fun entry ->
           History_entry.Id.equal entry.id submission.history_id));
       detach actor;
       print_endline
         "late readiness cancelled; desired Running and admitted input retained");
  [%expect {| late readiness cancelled; desired Running and admitted input retained |}]
;;

let%expect_test "retirement before compaction Worker_ready prevents auxiliary dispatch" =
  let on_started = ref (fun () -> ()) in
  Job_fixtures.with_actor
    ~state_committed:(fun _ events ->
      if
        List.exists events ~f:(fun (event : P.Event.Durable.t) ->
          P.Event.Durable.equal_kind event.kind Operation_started)
      then !on_started ())
    (fun _env sw actor writer backend ->
       let calls = ref 0 in
       let execution = Inference_ports.compaction_execution () in
       let port : A.Compaction_inference.t =
         { with_execution =
             (fun ~operation:_ ~selection:_ f ->
               Int.incr calls;
               f execution)
         }
       in
       A.set_compaction_inference actor (Some port) |> protocol_ok;
       let original = user_entry actor "history must survive cancellation" in
       A.append_history actor ~attachment_id:writer.id [ original ]
       |> protocol_ok
       |> ignore;
       let before = A.state actor |> protocol_ok in
       let retirement, retirement_u = Eio.Promise.create () in
       let entered, entered_u = Eio.Promise.create () in
       (on_started
        := fun () ->
             (* Use the same actor-commit barrier as foreground execution: retirement
           reaches the priority mailbox before the compactor's readiness ACK. *)
             Eio.Fiber.fork ~sw (fun () ->
               Eio.Promise.resolve entered_u ();
               let join = A.retire_runtime_worker actor ~closing:false |> protocol_ok in
               Eio.Promise.resolve retirement_u join);
             Eio.Promise.await entered;
             Eio.Fiber.yield ());
       A.compact
         actor
         ~attachment_id:writer.id
         ~expected_revision:(Some before.counters.revision)
       |> protocol_ok
       |> ignore;
       let join = Eio.Promise.await retirement in
       A.Runtime_retirement.await join |> protocol_ok;
       [%test_eq: int] 0 !calls;
       let after = A.state actor |> protocol_ok in
       assert (Option.is_none after.active_operation);
       [%test_eq: P.Session.desired_state] Running after.lifecycle.desired;
       [%test_eq: int]
         before.conversation.compaction_generation
         after.conversation.compaction_generation;
       assert (
         List.equal
           P.History.equal_entry
           before.conversation.canonical_history
           after.conversation.canonical_history);
       let events =
         Agent_session.Memory_backend.events_after backend before.counters.event_sequence
         |> protocol_ok
       in
       assert (
         List.exists events ~f:(fun (event : P.Event.Durable.t) ->
           P.Event.Durable.equal_kind event.kind Operation_cancelled));
       detach actor;
       print_endline
         "late compaction readiness cancelled; auxiliary callback and history unchanged");
  [%expect
    {| late compaction readiness cancelled; auxiliary callback and history unchanged |}]
;;

let%expect_test "retirement joins worker cleanup without adopting deferred input" =
  Job_fixtures.with_actor (fun _env _sw actor writer _backend ->
    let entered, entered_u = Eio.Promise.create () in
    let cleaning, cleaning_u = Eio.Promise.create () in
    let release_cleanup, release_cleanup_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let dispatched = ref 0 in
    let worker =
      Worker.create ~run:(fun ~sw:_ ~input:_ _ ->
        Int.incr dispatched;
        Exn.protect
          ~finally:(fun () ->
            Eio.Cancel.protect (fun () ->
              Eio.Promise.resolve cleaning_u ();
              Eio.Promise.await release_cleanup))
          ~f:(fun () ->
            Eio.Promise.resolve entered_u ();
            Eio.Promise.await never))
    in
    install actor worker;
    let first =
      A.submit_message actor ~attachment_id:writer.id (user_entry actor "first")
      |> protocol_ok
    in
    Eio.Promise.await entered;
    let deferred = user_entry actor "second" in
    let second =
      A.submit_message actor ~attachment_id:writer.id deferred |> protocol_ok
    in
    [%test_eq: P.Method_result.Send_message.disposition] Deferred second.disposition;
    let released = ref false in
    let release () =
      if not !released
      then (
        released := true;
        Eio.Promise.resolve release_cleanup_u ())
    in
    Exn.protect ~finally:release ~f:(fun () ->
      let join = A.retire_runtime_worker actor ~closing:false |> protocol_ok in
      Eio.Promise.await cleaning;
      assert (not (A.Runtime_retirement.is_finished join));
      let consumed =
        A.consume_deferred actor ~operation_id:(Option.value_exn first.operation_id)
        |> protocol_ok
      in
      assert (List.is_empty consumed);
      assert_conflict (A.adopt_deferred actor);
      let pending = A.state actor |> protocol_ok in
      assert (
        List.equal
          P.History.equal_entry
          [ deferred ]
          pending.conversation.deferred_user_entries);
      release ();
      A.Runtime_retirement.await join |> protocol_ok;
      let final = A.state actor |> protocol_ok in
      [%test_eq: P.Session.desired_state] Running final.lifecycle.desired;
      assert (Option.is_none final.active_operation);
      assert (
        List.equal
          P.History.equal_entry
          [ deferred ]
          final.conversation.deferred_user_entries);
      [%test_eq: bool] false (A.apply_observation_follow_up actor |> protocol_ok);
      [%test_eq: int] 1 !dispatched;
      detach actor;
      print_endline "cleanup joined; safe-point and follow-up leave deferred input queued"));
  [%expect {| cleanup joined; safe-point and follow-up leave deferred input queued |}]
;;

let%expect_test "ordinary detachment reopens auxiliary execution but closing is permanent"
  =
  List.iter [ false; true ] ~f:(fun closing ->
    Job_fixtures.with_actor (fun _env _sw actor writer _backend ->
      let execution = Inference_ports.compaction_execution () in
      let calls = ref 0 in
      let port : A.Compaction_inference.t =
        { with_execution =
            (fun ~operation:_ ~selection:_ f ->
              Int.incr calls;
              f execution)
        }
      in
      A.set_compaction_inference actor (Some port) |> protocol_ok;
      A.append_history actor ~attachment_id:writer.id [ user_entry actor "remember" ]
      |> protocol_ok
      |> ignore;
      let join = A.retire_runtime_worker actor ~closing |> protocol_ok in
      A.Runtime_retirement.await join |> protocol_ok;
      detach actor;
      (* A later reusable caller cannot downgrade permanent closing. *)
      if closing
      then (
        let same = A.retire_runtime_worker actor ~closing:false |> protocol_ok in
        A.Runtime_retirement.await same |> protocol_ok);
      let before = A.state actor |> protocol_ok in
      let compact =
        A.compact
          actor
          ~attachment_id:writer.id
          ~expected_revision:(Some before.counters.revision)
      in
      let worker = Worker.create ~run:(fun ~sw:_ ~input:_ _ -> assert false) in
      (match closing with
       | true ->
         assert_conflict compact;
         assert_conflict
           (A.set_runtime_worker actor ~worker:(Some worker) ~inference:(Some execution));
         [%test_eq: int] 0 !calls
       | false ->
         compact |> protocol_ok |> ignore;
         let compacted = await_idle actor in
         [%test_eq: int]
           (before.conversation.compaction_generation + 1)
           compacted.conversation.compaction_generation;
         [%test_eq: int] 1 !calls;
         A.set_runtime_worker actor ~worker:(Some worker) ~inference:(Some execution)
         |> protocol_ok);
      print_s [%sexp (closing : bool), "auxiliary and reinstall admission checked"]));
  [%expect
    {|
    (false "auxiliary and reinstall admission checked")
    (true "auxiliary and reinstall admission checked") |}]
;;

let%expect_test "terminal persistence failure resolves retirement and keeps detach closed"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let initial =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let backend =
        Agent_session.Memory_backend.create ~event_capacity:128 ~initial_state:initial
      in
      let persistence = Agent_session.Memory_backend.persistence backend in
      let reject_terminal = ref false in
      let failure =
        P.Error.create
          Persistence_error
          ~message:"terminal write rejected"
          ~retryable:true
          ()
      in
      let actor =
        A.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:initial
          ~operation_worker:None
          ~persistence:
            { archive_reference
            ; commit =
                (fun ~command_audit
                  ~previous
                  (next : Agent_session.Session_transition.t) ->
                  if
                    !reject_terminal
                    && Option.is_some previous.active_operation
                    && Option.is_none next.state.active_operation
                  then Error failure
                  else persistence.commit ~command_audit ~previous next)
            }
          ~services:
            { now = (fun () -> timestamp)
            ; monotonic_now = (fun () -> Mtime.min_stamp)
            ; create_attachment_id = P.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "retirement-test")
            ; job_results = None
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; notification_limits = Agent_session.Staged_notifications.default_limits
            ; ingress_limits = Agent_session.Staged_ingress.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      Exn.protect
        ~finally:(fun () -> A.shutdown actor)
        ~f:(fun () ->
          let writer, _ =
            A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
          in
          A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
          let entered, entered_u = Eio.Promise.create () in
          let never, _ = Eio.Promise.create () in
          install
            actor
            (Worker.create ~run:(fun ~sw:_ ~input:_ _ ->
               Eio.Promise.resolve entered_u ();
               Eio.Promise.await never));
          A.submit_message actor ~attachment_id:writer.id (user_entry actor "first")
          |> protocol_ok
          |> ignore;
          Eio.Promise.await entered;
          let before = A.state actor |> protocol_ok in
          reject_terminal := true;
          let join = A.retire_runtime_worker actor ~closing:true |> protocol_ok in
          (match A.Runtime_retirement.await join with
           | Error actual ->
             assert (
               Jsonaf.exactly_equal (P.Error.to_json failure) (P.Error.to_json actual))
           | Ok () -> failwith "failed terminal write was treated as an acknowledgment");
          assert (A.Runtime_retirement.is_finished join);
          let after = A.state actor |> protocol_ok in
          assert_same_session_snapshot before after;
          assert_conflict (A.set_runtime_worker actor ~worker:None ~inference:None);
          [%test_eq: int64]
            before.counters.revision
            (Agent_session.Memory_backend.state backend).counters.revision;
          print_endline
            "terminal persistence error joins promptly; state and resources stay guarded")));
  [%expect
    {| terminal persistence error joins promptly; state and resources stay guarded |}]
;;
