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
         A.submit_message
           ~submitting_principal:principal_id
           actor
           ~attachment_id:writer.id
           (user_entry actor "first")
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
             (fun ~operation:_ ~selection:_ ~borrow_agent_actor:_ f ->
               Int.incr calls;
               f execution ~guidance:None ~reserved_input_tokens:0)
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
  let module C = A.Retirement_cleanup in
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
      A.submit_message
        ~submitting_principal:principal_id
        actor
        ~attachment_id:writer.id
        (user_entry actor "first")
      |> protocol_ok
    in
    Eio.Promise.await entered;
    let deferred = user_entry actor "second" in
    let second =
      A.submit_message
        ~submitting_principal:principal_id
        actor
        ~attachment_id:writer.id
        deferred
      |> protocol_ok
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
      let cleanup =
        A.retire_runtime_worker_with_cleanup actor ~closing:false |> protocol_ok
      in
      assert (phys_equal join (C.retirement cleanup));
      let duplicate =
        A.retire_runtime_worker_with_cleanup actor ~closing:false |> protocol_ok
      in
      assert (phys_equal cleanup duplicate);
      let captured = C.snapshot cleanup |> protocol_ok in
      let pending_cleanup = C.advance cleanup ~actor ~after:captured |> protocol_ok in
      assert (C.Snapshot.equal_progress (C.Snapshot.progress pending_cleanup) Pending);
      assert (List.is_empty (C.Snapshot.failures pending_cleanup));
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
          (List.map
             pending.conversation.deferred_user_entries
             ~f:Agent_session.Pending_input_document.entry));
      release ();
      A.Runtime_retirement.await join |> protocol_ok;
      let joined_cleanup =
        C.advance cleanup ~actor ~after:pending_cleanup |> protocol_ok
      in
      assert (C.Snapshot.equal_progress (C.Snapshot.progress joined_cleanup) Joined);
      assert (List.is_empty (C.Snapshot.failures joined_cleanup));
      assert (C.Snapshot.equal_progress (C.Snapshot.progress pending_cleanup) Pending);
      assert (List.is_empty (C.Snapshot.failures pending_cleanup));
      let final = A.state actor |> protocol_ok in
      [%test_eq: P.Session.desired_state] Running final.lifecycle.desired;
      assert (Option.is_none final.active_operation);
      assert (
        List.equal
          P.History.equal_entry
          [ deferred ]
          (List.map
             final.conversation.deferred_user_entries
             ~f:Agent_session.Pending_input_document.entry));
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
            (fun ~operation:_ ~selection:_ ~borrow_agent_actor:_ f ->
              Int.incr calls;
              f execution ~guidance:None ~reserved_input_tokens:0)
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
  let module F = Agent_session.Callback_cleanup in
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
      let terminal_entered, terminal_entered_u = Eio.Promise.create () in
      let release_terminal, release_terminal_u = Eio.Promise.create () in
      let release () = ignore (Eio.Promise.try_resolve release_terminal_u ()) in
      let receipt = ref None in
      let original_getter = ref None in
      let cancelled = ref false in
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
                (fun ?observation
                  ~command_audit
                  ~previous
                  (next : Agent_session.Session_transition.t) ->
                  if
                    !reject_terminal
                    && Option.is_some previous.active_operation
                    && Option.is_none next.state.active_operation
                  then (
                    ignore (Eio.Promise.try_resolve terminal_entered_u ());
                    Eio.Cancel.protect (fun () -> Eio.Promise.await release_terminal);
                    Error failure)
                  else persistence.commit ?observation ~command_audit ~previous next)
            }
          ~services:
            { now = (fun () -> timestamp)
            ; monotonic_now = (fun () -> Mtime.min_stamp)
            ; create_attachment_id = P.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "retirement-test")
            ; job_results = None
            ; goal_descendant_profile
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; notification_limits = Agent_session.Staged_notifications.default_limits
            ; ingress_limits = Agent_session.Staged_ingress.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      Exn.protect
        ~finally:(fun () ->
          release ();
          A.shutdown actor)
        ~f:(fun () ->
          let writer, _ =
            A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
          in
          A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
          let entered, entered_u = Eio.Promise.create () in
          let never, _ = Eio.Promise.create () in
          install
            actor
            (Worker.create ~run:(fun ~sw:_ ~input:_ capabilities ->
               original_getter := Some capabilities.callback_cleanup;
               receipt := Some (capabilities.callback_cleanup () |> protocol_ok);
               Eio.Promise.resolve entered_u ();
               try Eio.Promise.await never with
               | Eio.Cancel.Cancelled _ as exn ->
                 let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                 cancelled := true;
                 Exn.raise_with_original_backtrace exn backtrace));
          A.submit_message
            ~submitting_principal:principal_id
            actor
            ~attachment_id:writer.id
            (user_entry actor "first")
          |> protocol_ok
          |> ignore;
          Eio.Promise.await entered;
          let before = A.state actor |> protocol_ok in
          let receipt = Option.value_exn !receipt in
          let original = F.Operation.operation receipt |> protocol_ok in
          assert (
            P.Id.Operation.equal original.id (Option.value_exn before.active_operation).id);
          let initial = F.Operation.snapshot receipt |> protocol_ok in
          assert (F.Cleanup_snapshot.pending initial);
          assert (not (F.Cleanup_snapshot.registration_closed initial));
          assert (Option.is_none (F.Operation.terminal receipt |> protocol_ok));
          reject_terminal := true;
          let join = A.retire_runtime_worker actor ~closing:true |> protocol_ok in
          Eio.Promise.await terminal_entered;
          assert !cancelled;
          let pending = F.Operation.snapshot receipt |> protocol_ok in
          assert (F.Cleanup_snapshot.registration_closed pending);
          assert (F.Cleanup_snapshot.pending pending);
          assert (List.is_empty (F.Cleanup_snapshot.failures pending));
          assert (Option.is_none (F.Operation.terminal receipt |> protocol_ok));
          assert (phys_equal receipt (Option.value_exn !original_getter () |> protocol_ok));
          Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
            let refused = function
              | Error (error : P.Error.t) -> [%test_eq: P.Error.code] Conflict error.code
              | Ok _ -> failwith "foreign domain observed original operation receipt"
            in
            refused (F.Operation.snapshot receipt);
            refused (F.Operation.terminal receipt);
            refused (F.Operation.operation receipt);
            refused (Option.value_exn !original_getter ()));
          release ();
          (match A.Runtime_retirement.await join with
           | Error actual ->
             assert (
               Jsonaf.exactly_equal (P.Error.to_json failure) (P.Error.to_json actual))
           | Ok () -> failwith "failed terminal write was treated as an acknowledgment");
          assert (A.Runtime_retirement.is_finished join);
          (match F.Operation.terminal receipt |> protocol_ok with
           | Some (Ok (Error actual)) -> assert (phys_equal actual failure)
           | None | Some (Ok (Ok ())) | Some (Error _) ->
             failwith "normalized worker cancellation replaced actual terminal failure");
          let completed = F.Operation.snapshot receipt |> protocol_ok in
          assert (F.Cleanup_snapshot.registration_closed completed);
          assert (not (F.Cleanup_snapshot.pending completed));
          assert (List.is_empty (F.Cleanup_snapshot.failures completed));
          assert (F.Cleanup_snapshot.pending pending);
          assert (not (F.Cleanup_snapshot.registration_closed initial));
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

let%expect_test "original operation callback registration closes before terminal reply" =
  let module F = Agent_session.Callback_cleanup in
  let module M = Chat_response.Moderator_manager in
  let entered, entered_u = Eio.Promise.create () in
  let return_worker, return_worker_u = Eio.Promise.create () in
  let terminal_entered, terminal_entered_u = Eio.Promise.create () in
  let release_terminal, release_terminal_u = Eio.Promise.create () in
  let release_worker () = ignore (Eio.Promise.try_resolve return_worker_u ()) in
  let release () = ignore (Eio.Promise.try_resolve release_terminal_u ()) in
  let manager = ref None in
  let armed = ref false in
  let terminal_attempts = ref 0 in
  with_handoff_runtime
    ~reject:(fun (next : Agent_session.Session_transition.t) ->
      if !armed && Option.is_none next.state.active_operation
      then (
        incr terminal_attempts;
        Eio.Promise.resolve terminal_entered_u ();
        Eio.Cancel.protect (fun () -> Eio.Promise.await release_terminal);
        true)
      else false)
    ~make_runtime:(fun env _actor_ready ->
      let actual =
        prepared_legacy_callback_managers
          env
          ~id:"original-terminal-callback-cut"
          ~source:
            "let initial_state = 0\nlet on_event ctx state event = Task.pure(state + 1)"
          ()
      in
      manager := Some actual;
      let worker =
        Worker.create ~run:(fun ~sw:_ ~input capabilities ->
          let receipt = capabilities.callback_cleanup () |> protocol_ok in
          Eio.Promise.resolve entered_u (receipt, input.operation);
          Eio.Promise.await return_worker;
          Completed
            { final_history = input.history
            ; runtime_requests = []
            ; moderator_snapshot =
                Some
                  (M.identity_snapshot actual
                   |> Result.ok_or_failwith
                   |> Agent_session.Runtime_builder.encode_moderator_snapshot)
            })
      in
      Handoff_runtime.Moderator
        { worker
        ; manager = actual
        ; inference =
            Inference_ports.create ~config:Chat_response.Config.default ()
            |> Inference_ports.execution
        })
    (fun _env actor _writer backend ->
       Eio.Switch.run (fun sw ->
         let request = ref None in
         Exn.protect
           ~finally:(fun () ->
             Eio.Cancel.protect (fun () ->
               (* Both real producers are released before any request/owner join. *)
               armed := false;
               release_worker ();
               release ();
               Option.iter !request ~f:(fun task -> ignore (Eio.Promise.await_exn task))))
           ~f:(fun () ->
             let receipt, operation = Eio.Promise.await entered in
             let initial = F.Operation.snapshot receipt |> protocol_ok in
             assert (not (F.Cleanup_snapshot.registration_closed initial));
             let before = A.state actor |> protocol_ok in
             (match before.active_operation with
              | Some active ->
                assert (P.Id.Operation.equal active.id operation.id);
                (match active.state with
                 | Running -> ()
                 | _ -> failwith "original operation is not running")
              | None -> failwith "worker lost its actual operation");
             armed := true;
             release_worker ();
             Eio.Promise.await terminal_entered;
             let pending = F.Operation.snapshot receipt |> protocol_ok in
             assert (F.Cleanup_snapshot.registration_closed pending);
             assert (F.Cleanup_snapshot.pending pending);
             assert (Option.is_none (F.Operation.terminal receipt |> protocol_ok));
             assert (List.is_empty (F.Cleanup_snapshot.failures pending));
             let admission_entered, admission_entered_u = Eio.Promise.create () in
             let captures = ref 0 in
             let bodies = ref 0 in
             let refusal = ref None in
             let task =
               Eio.Fiber.fork_promise ~sw (fun () ->
                 let run () =
                   let actual = Option.value_exn !manager in
                   M.handle_legacy_callback_entries
                     actual
                     ~selection:(Boundary Turn_end)
                     ~session_id:"fixture"
                     ~now_ms:0
                     ~history:[]
                     ~available_tools:[]
                     ~session_meta:`Null
                     ~with_admission:(fun callback ~snapshot:_ _run ->
                       assert (M.callback_is_current actual callback);
                       Eio.Promise.resolve admission_entered_u ();
                       match
                         A.with_legacy_callback
                           actor
                           ~operation_id:(Some operation.id)
                           ~callback
                           ~capture_cleanup:(fun _ -> incr captures)
                           (fun ~scope:_ ~commit:_ ->
                              incr bodies;
                              Ok ())
                       with
                       | Error error ->
                         refusal := Some error;
                         Error error.P.Error.message
                       | Ok _ -> failwith "terminal operation acquired another callback")
                 in
                 match run () with
                 | result -> Ok result
                 | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
             in
             request := Some task;
             Eio.Promise.await admission_entered;
             Eio.Fiber.yield ();
             assert (Option.is_none (Eio.Promise.peek task));
             [%test_eq: int] 0 !captures;
             [%test_eq: int] 0 !bodies;
             release ();
             (match Eio.Promise.await_exn task with
              | Ok (Error _) -> ()
              | Ok (Ok _) -> failwith "queued callback unexpectedly ran"
              | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace);
             let refusal = Option.value_exn !refusal in
             [%test_eq: P.Error.code] Conflict refusal.code;
             (* Diagnostic assertion distinguishes this exact early parent cut
               from the later existing expired-admission fence. No production
               dispatch/classification depends on this string. Removing this
               parent guard does not imply a permission grant. *)
             [%test_eq: string]
               "callback parent registration ended or changed"
               refusal.message;
             [%test_eq: int] 0 !captures;
             [%test_eq: int] 0 !bodies;
             [%test_eq: int] 1 !terminal_attempts;
             (match F.Operation.terminal receipt |> protocol_ok with
              | Some (Ok (Error actual)) ->
                [%test_eq: P.Error.code] Internal_error actual.code
              | None | Some (Ok (Ok ())) | Some (Error _) ->
                failwith "actual rejected terminal result disappeared");
             let completed = F.Operation.snapshot receipt |> protocol_ok in
             assert (F.Cleanup_snapshot.registration_closed completed);
             assert (not (F.Cleanup_snapshot.pending completed));
             assert (F.Cleanup_snapshot.pending pending);
             assert (not (F.Cleanup_snapshot.registration_closed initial));
             armed := false;
             assert_same_session_snapshot before (A.state actor |> protocol_ok);
             assert_same_session_snapshot
               before
               (Agent_session.Memory_backend.state backend))));
  print_endline
    "actual terminal entry closes original registration; queued same-Manager claim has \
     no ACK or capture";
  [%expect
    {| actual terminal entry closes original registration; queued same-Manager claim has no ACK or capture |}]
;;
