open! Core
open Fixtures
module A = Agent_session.Session_actor
module P = Agent_protocol

let capture f =
  match f () with
  | result -> Ok result
  | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
;;

let unavailable = function
  | Error { P.Error.code = Server_shutting_down; _ } -> ()
  | Error error -> raise_s [%sexp "unexpected closed-owner result", (error : P.Error.t)]
  | Ok _ -> failwith "terminated owner accepted a request"
;;

(* Callers live outside the actor's switch, including protected cleanup callers.
   An actor failure must release them before its owning switch can be joined. *)
let with_owner_env ~state_committed f =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      Eio.Switch.run (fun callers ->
        let ready, ready_u = Eio.Promise.create () in
        let release, release_u = Eio.Promise.create () in
        let exited =
          Eio.Fiber.fork_promise ~sw:callers (fun () ->
            capture (fun () ->
              Eio.Cancel.sub (fun owner ->
                Eio.Switch.run (fun sw ->
                  let initial =
                    actor_state
                      ~workspace_instance
                      ~liveness:Detached
                      ~start_immediately:false
                    |> fun (state : Agent_session.Session_state.t) ->
                    { state with
                      runtime_initialization = Pending { fresh_history = true }
                    }
                  in
                  let backend =
                    Agent_session.Memory_backend.create
                      ~event_capacity:32
                      ~initial_state:initial
                  in
                  let actor =
                    A.create
                      ~sw
                      ~clock:(Eio.Stdenv.clock env)
                      ~mailbox_capacity:8
                      ~compaction_env:None
                      ~initial_state:initial
                      ~operation_worker:None
                      ~persistence:(Agent_session.Memory_backend.persistence backend)
                      ~services:
                        { now = (fun () -> timestamp)
                        ; monotonic_now = (fun () -> Mtime.min_stamp)
                        ; create_attachment_id = P.Id.Attachment.create
                        ; create_reclaim_token = (fun () -> "owner-termination")
                        ; job_results = None
                        ; schedule_limits = Agent_session.Staged_schedules.default_limits
                        ; notification_limits =
                            Agent_session.Staged_notifications.default_limits
                        ; ingress_limits = Agent_session.Staged_ingress.default_limits
                        ; goal_descendant_profile
                        ; subscription_limits =
                            Agent_session.Staged_subscriptions.default_limits
                        ; state_committed
                        }
                  in
                  Eio.Promise.resolve
                    ready_u
                    (actor, backend, fun () -> Eio.Cancel.cancel owner Exit);
                  Eio.Promise.await release))))
        in
        let actor, backend, cancel = Eio.Promise.await ready in
        Exn.protect
          ~finally:(fun () ->
            Eio.Cancel.protect (fun () ->
              A.shutdown actor;
              ignore (Eio.Promise.try_resolve release_u ());
              ignore (Eio.Promise.await_exn exited)))
          ~f:(fun () -> f env callers actor backend cancel exited))))
;;

let with_owner ~state_committed f = with_owner_env ~state_committed (fun _env -> f)
let fail_publication failure = raise failure [@@inline never]

let%expect_test "post-ACK owner exception releases protected callers and expires proof" =
  let armed = ref false in
  let entered, entered_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let failure = Failure "injected post-ACK publication" in
  let previous_backtraces = Stdlib.Printexc.backtrace_status () in
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous_backtraces)
    ~f:(fun () ->
      Stdlib.Printexc.record_backtrace true;
      with_owner
        ~state_committed:(fun _ _ ->
          if !armed
          then (
            Eio.Promise.resolve entered_u ();
            Eio.Promise.await release;
            fail_publication failure))
        (fun callers actor backend _cancel exited ->
           let original = Agent_authority_tests.authentication (Atomic.make true) in
           let expected = A.state actor |> protocol_ok in
           let scope =
             A.begin_initialization_authenticated actor ~actor:original ~expected
             |> protocol_ok
           in
           let proof =
             A.initialization_agent_actor actor ~scope
             |> Agent_authority_tests.require_actor
           in
           assert (Operator_authorization.is_current proof);
           armed := true;
           let in_flight =
             Eio.Fiber.fork_promise ~sw:callers (fun () ->
               Eio.Cancel.protect (fun () ->
                 capture (fun () -> A.reserve_history_block actor ~count:1)))
           in
           Eio.Promise.await entered;
           let queued, queued_u = Eio.Promise.create () in
           let pending =
             Eio.Fiber.fork_promise ~sw:callers (fun () ->
               Eio.Cancel.protect (fun () ->
                 Eio.Promise.resolve queued_u ();
                 A.reserve_history_block actor ~count:2))
           in
           Eio.Promise.await queued;
           Eio.Promise.resolve release_u ();
           (match Eio.Promise.await_exn in_flight with
            | Error (actual, backtrace) ->
              assert (phys_equal actual failure);
              assert (
                String.is_substring
                  (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                  ~substring:"fail_publication")
            | Ok _ -> failwith "post-ACK exception became an ordinary reply");
           unavailable (Eio.Promise.await_exn pending);
           assert (Result.is_error (Eio.Promise.await_exn exited));
           unavailable (A.state actor);
           unavailable (Eio.Cancel.protect (fun () -> A.end_initialization actor ~scope));
           assert (Operator_authorization.is_current original);
           assert (not (Operator_authorization.is_current proof));
           let stored = Agent_session.Memory_backend.state backend in
           [%test_eq: int64]
             stored.counters.revision
             (Int64.succ expected.counters.revision);
           [%test_eq: int64]
             stored.conversation.next_history_sequence
             (Int64.succ expected.conversation.next_history_sequence)));
  print_endline
    "Exceptional reply keeps original backtrace; one committed write; closed queue and \
     expired owner proof.";
  [%expect
    {| Exceptional reply keeps original backtrace; one committed write; closed queue and expired owner proof. |}]
;;

let%expect_test
    "owner cancellation closes idle and in-flight requests; clean shutdown stays ordinary"
  =
  List.iter [ false; true ] ~f:(fun in_flight ->
    let entered, entered_u = Eio.Promise.create () in
    let never, _never_u = Eio.Promise.create () in
    with_owner
      ~state_committed:(fun _ _ -> ())
      (fun callers actor _backend cancel exited ->
         let pending =
           if in_flight
           then
             Some
               (Eio.Fiber.fork_promise ~sw:callers (fun () ->
                  Eio.Cancel.protect (fun () ->
                    capture (fun () ->
                      A.checkpoint actor ~persist:(fun _ ->
                        Eio.Promise.resolve entered_u ();
                        Eio.Promise.await never)))))
           else None
         in
         Option.iter pending ~f:(fun _ -> Eio.Promise.await entered);
         cancel ();
         Option.iter pending ~f:(fun pending ->
           match Eio.Promise.await_exn pending with
           | Error (Eio.Cancel.Cancelled _, _) -> ()
           | Error (other, _) -> raise other
           | Ok _ -> failwith "cancelled request returned ordinarily");
         assert (Result.is_error (Eio.Promise.await_exn exited));
         unavailable (Eio.Cancel.protect (fun () -> A.state actor))));
  with_owner
    ~state_committed:(fun _ _ -> ())
    (fun callers actor _backend _cancel _exited ->
       let entered, entered_u = Eio.Promise.create () in
       let release, release_u = Eio.Promise.create () in
       let caller_ready, caller_ready_u = Eio.Promise.create () in
       let caller =
         Eio.Fiber.fork_promise ~sw:callers (fun () ->
           capture (fun () ->
             Eio.Cancel.sub (fun context ->
               Eio.Promise.resolve caller_ready_u context;
               A.checkpoint actor ~persist:(fun _ ->
                 Eio.Promise.resolve entered_u ();
                 Eio.Promise.await release))))
       in
       let caller_context = Eio.Promise.await caller_ready in
       Eio.Promise.await entered;
       Eio.Cancel.cancel caller_context Exit;
       (match Eio.Promise.await_exn caller with
        | Error (Eio.Cancel.Cancelled _, _) -> ()
        | Error (other, _) -> raise other
        | Ok _ -> failwith "caller cancellation was lost");
       Eio.Promise.resolve release_u (Ok ());
       A.state actor |> protocol_ok |> ignore;
       A.shutdown actor;
       unavailable (A.state actor));
  print_endline
    "Owner cancellation closes requests; caller cancellation leaves the actor live; \
     ordinary shutdown still returns.";
  [%expect
    {| Owner cancellation closes requests; caller cancellation leaves the actor live; ordinary shutdown still returns. |}]
;;

let%expect_test
    "selected Job capacity and publication cuts drain actions and preserve the first \
     failure"
  =
  List.iter [ `Capacity; `State_publication ] ~f:(fun cut ->
    let selected : P.Job.t list ref = ref [] in
    let unselected = ref None in
    let published = ref []
    and aborted = ref [] in
    let retained = ref None in
    let failure = Failure "first selected Job publication failure" in
    let later_failure = Failure "later selected Job publication failure" in
    let old_backtraces = Stdlib.Printexc.backtrace_status () in
    Exn.protect
      ~finally:(fun () -> Stdlib.Printexc.record_backtrace old_backtraces)
      ~f:(fun () ->
        Stdlib.Printexc.record_backtrace true;
        with_owner_env
          ~state_committed:(fun state _ ->
            match cut, !selected with
            | `State_publication, (_ :: _ as jobs)
              when List.for_all jobs ~f:(fun job ->
                     List.exists state.Agent_session.Session_state.jobs ~f:(fun saved ->
                       P.Id.Job.equal saved.id job.id)) -> fail_publication failure
            | (`Capacity | `State_publication), _ -> ())
          (fun _env _callers actor backend _cancel exited ->
             let original = Agent_authority_tests.authentication (Atomic.make true) in
             let writer, _ =
               A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
             in
             let expected = A.state actor |> protocol_ok in
             let initialization =
               A.begin_initialization_authenticated actor ~actor:original ~expected
               |> protocol_ok
             in
             let parent : P.Job.t =
               { id = P.Id.Job.create ()
               ; session_id
               ; generation = expected.identity.generation
               ; kind = Model_call
               ; payload = `Object []
               ; status = Queued
               ; retry_policy = Never
               ; attempt = 0
               ; created_at = timestamp
               ; started_at = None
               ; next_run_at = None
               ; completed_at = None
               ; result = None
               ; delivery = Not_required
               ; launch = None
               ; inference_origin = None
               ; progress = None
               }
             in
             let parent =
               A.add_initialization_model_job actor ~scope:initialization parent
               |> protocol_ok
             in
             A.complete_initialization actor ~scope:initialization ~candidate:expected
             |> protocol_ok
             |> ignore;
             A.end_initialization actor ~scope:initialization |> protocol_ok;
             A.start actor ~actor:original ~attachment_id:writer.id
             |> protocol_ok
             |> ignore;
             let parent =
               A.claim_job actor ~job_id:parent.id ~generation:parent.generation
               |> protocol_ok
               |> Option.value_exn
             in
             let registry = native_registry (ref 0) ~raises:false in
             let request =
               Background_execution_tests.capture_tool
                 registry
                 Chat_response.One_off_request.default_policy
             in
             let invocation = Job_fixtures.root parent in
             let stage owner action =
               let job =
                 A.prepare_background_job_launch actor ~owner request |> protocol_ok
               in
               A.stage_background_job
                 actor
                 ~job
                 ~capacity:
                   { publish =
                       (fun () ->
                         published := job.id :: !published;
                         match cut, action with
                         | `Capacity, `First -> fail_publication failure
                         | `Capacity, `Second -> fail_publication later_failure
                         | `State_publication, (`First | `Second) -> ()
                         | (`Capacity | `State_publication), `Unselected ->
                           failwith "unselected Job published")
                   ; abort = (fun () -> aborted := job.id :: !aborted)
                   }
               |> protocol_ok;
               job
             in
             let outcome =
               capture (fun () ->
                 A.with_job_execution
                   actor
                   ~job_id:parent.id
                   ~generation:parent.generation
                   ~attempt:parent.attempt
                   ~deadline:(Some Job_fixtures.deadline)
                   (fun services ->
                      services.execute ~invocation (fun ~dispatched ~capabilities ->
                        let borrow_agent_actor =
                          capabilities
                            .Agent_session.Invocation_capabilities.borrow_agent_actor
                        in
                        retained
                        := Some
                             (borrow_agent_actor ~invocation:dispatched
                              |> Agent_authority_tests.require_actor);
                        let owner = P.Job.Invocation dispatched.context.id in
                        let first = stage owner `First in
                        let second = stage owner `Second in
                        let other = stage owner `Unselected in
                        selected := [ first; second ];
                        unselected := Some other;
                        A.select_background_jobs actor ~owner ~ids:[ first.id; second.id ]
                        |> protocol_ok;
                        Ok (P.Invocation.Complete `Null))))
             in
             (* These checks run outside both the actual actor owner switch and
                every native/worker exception-conversion boundary. *)
             (match outcome with
              | Error (actual, backtrace) ->
                assert (phys_equal actual failure);
                assert (
                  String.is_substring
                    (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                    ~substring:"fail_publication")
              | Ok _ -> failwith "post-ACK capacity/publication failure was swallowed");
             let selected_ids = List.map !selected ~f:(fun job -> job.P.Job.id) in
             [%test_eq: int] 2 (List.length selected_ids);
             assert (List.equal P.Id.Job.equal selected_ids (List.rev !published));
             let other = Option.value_exn !unselected in
             assert (List.equal P.Id.Job.equal [ other.id ] (List.rev !aborted));
             let stored = Agent_session.Memory_backend.state backend in
             assert (
               List.for_all selected_ids ~f:(fun id ->
                 List.exists stored.jobs ~f:(fun job -> P.Id.Job.equal id job.id)));
             assert (
               not
                 (List.exists stored.jobs ~f:(fun job -> P.Id.Job.equal other.id job.id)));
             (match
                List.find stored.invocations ~f:(fun saved ->
                  P.Id.Invocation.equal saved.context.id invocation.context.id)
              with
              | Some { status = Resolved (Complete `Null); _ } -> ()
              | Some _ | None -> failwith "ACKed parent outcome did not persist");
             assert (Result.is_error (Eio.Promise.await_exn exited));
             unavailable (A.state actor);
             unavailable
               (Eio.Cancel.protect (fun () -> A.reserve_history_block actor ~count:1));
             assert (Operator_authorization.is_current original);
             assert (not (Operator_authorization.is_current (Option.value_exn !retained))))));
  print_endline
    "selected facts and outcome stay committed; all prescribed capacity actions run; \
     first failure/backtrace survives; dead owner lends no permission";
  [%expect
    {| selected facts and outcome stay committed; all prescribed capacity actions run; first failure/backtrace survives; dead owner lends no permission |}]
;;

let%expect_test
    "selected constructor callback retains postrelease publication failure without replay"
  =
  let module C = A.Retirement_cleanup in
  let module M = Chat_response.Moderator_manager in
  let module F = Agent_session.Callback_cleanup in
  let module S = F.Completion_snapshot in
  let completion = ref None in
  let selected = ref None in
  let published = ref None in
  let effects = ref 0 in
  let publications = ref 0 in
  let failure = Failure "selected callback publication failed after release" in
  let previous_backtraces = Stdlib.Printexc.backtrace_status () in
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous_backtraces)
    ~f:(fun () ->
      Stdlib.Printexc.record_backtrace true;
      with_owner_env
        ~state_committed:(fun state _ ->
          match !selected with
          | None -> ()
          | Some id ->
            if
              List.exists
                state.Agent_session.Session_state.moderator_executions
                ~f:(fun receipt ->
                  P.Id.Moderator_execution.equal
                    receipt.P.Moderator_execution.context.id
                    id
                  &&
                  match receipt.status with
                  | Failed _ -> true
                  | Running | Completed _ | Interrupted _ -> false)
            then (
              incr publications;
              published := Some state;
              fail_publication failure))
        (fun env callers actor backend _cancel exited ->
           let manager =
             prepared_legacy_callback_managers
               env
               ~id:"selected-constructor-publication"
               ~source:
                 "let initial_state = 0\n\
                  let on_event ctx state event = Task.pure(state + 1)"
               ()
           in
           let original = Agent_authority_tests.authentication (Atomic.make true) in
           let expected = A.state actor |> protocol_ok in
           let constructor =
             A.begin_initialization_authenticated actor ~actor:original ~expected
             |> protocol_ok
           in
           let entered, enter = Eio.Promise.create () in
           let release, resume = Eio.Promise.create () in
           let release_callback () = ignore (Eio.Promise.try_resolve resume ()) in
           let callback = ref None in
           Exn.protect
             ~finally:(fun () ->
               Eio.Cancel.protect (fun () ->
                 release_callback ();
                 Option.iter !callback ~f:(fun task ->
                   ignore (Eio.Promise.await_exn task))))
             ~f:(fun () ->
               let task =
                 Eio.Fiber.fork_promise ~sw:callers (fun () ->
                   capture (fun () ->
                     M.handle_legacy_callback_entries
                       manager
                       ~selection:(Boundary Session_start)
                       ~session_id:"fixture"
                       ~now_ms:0
                       ~history:[]
                       ~available_tools:[]
                       ~session_meta:`Null
                       ~with_admission:(fun callback ~snapshot:_ _run ->
                         (* The real Manager issues the Callback. This host body
                             exercises Actor admission/exit, not authored execution. *)
                         let foreign_captures = ref 0 in
                         let foreign_bodies = ref 0 in
                         let before = A.state actor |> protocol_ok in
                         (match
                            Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
                              Eio_main.run (fun _ ->
                                A.with_initialization_callback
                                  actor
                                  ~scope:constructor
                                  ~manager
                                  ~callback
                                  ~capture_cleanup:(fun _ -> incr foreign_captures)
                                  (fun ~scope:_ ~commit:_ ->
                                     incr foreign_bodies;
                                     Ok ())))
                          with
                          | Error error -> [%test_eq: P.Error.code] Conflict error.code
                          | Ok _ -> failwith "foreign domain acquired callback custody");
                         [%test_eq: int] 0 !foreign_captures;
                         [%test_eq: int] 0 !foreign_bodies;
                         assert_same_session_snapshot before (A.state actor |> protocol_ok);
                         match
                           A.with_initialization_callback
                             actor
                             ~scope:constructor
                             ~manager
                             ~callback
                             ~capture_cleanup:(fun receipt -> completion := Some receipt)
                             (fun ~scope ~commit:_ ->
                                let executing = A.Callback_scope.executing scope in
                                selected := Some executing.context.id;
                                let invocation =
                                  P.Invocation.create
                                    ~observer:executing.context.source
                                    ~parent_event:executing.context.id
                                    { (invocation_fixture ()).context with
                                      origin = Moderator
                                    ; provider_call_id = None
                                    ; call_entry_id = None
                                    ; parent_invocation = None
                                    ; parent_job = None
                                    }
                                  |> protocol_ok
                                in
                                let resolved =
                                  A.callback_tool_executor
                                    actor
                                    ~scope
                                    ~invocation
                                    (fun ~dispatched:_ ~capabilities:_ ->
                                       incr effects;
                                       Ok (P.Invocation.Complete `Null))
                                  |> protocol_ok
                                in
                                (match resolved.status with
                                 | Resolved (Complete `Null) -> ()
                                 | _ ->
                                   failwith "actual callback invocation did not resolve");
                                Eio.Promise.resolve enter ();
                                Eio.Promise.await release;
                                (* Actual Returned without checkpoint forces the
                                   genuine failed terminal ACK and its publication. *)
                                Ok ())
                         with
                         | Error error -> Error error.P.Error.message
                         | Ok _ -> failwith "uncommitted callback unexpectedly succeeded")))
               in
               callback := Some task;
               (match
                  Eio.Fiber.first
                    (fun () ->
                       Eio.Promise.await entered;
                       `Entered)
                    (fun () -> `Exited (Eio.Promise.await_exn task))
                with
                | `Entered -> ()
                | `Exited (Error (exn, backtrace)) ->
                  Exn.raise_with_original_backtrace exn backtrace
                | `Exited (Ok _) -> failwith "callback ended before the actual gate");
               let completion = Option.value_exn !completion in
               let initial_completion = F.Completion.snapshot completion |> protocol_ok in
               (match S.producer_exit initial_completion with
                | Not_returned -> ()
                | Returned _ -> failwith "live producer already has an exit");
               (match S.finish_call initial_completion, S.release initial_completion with
                | Not_attempted, Not_released -> ()
                | _ -> failwith "live callback manufactured finish/release evidence");
               (match
                  Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
                    F.Completion.snapshot completion)
                with
                | Error error -> [%test_eq: P.Error.code] Conflict error.code
                | Ok _ -> failwith "foreign domain read mutable completion");
               let holder =
                 A.retire_runtime_worker_with_cleanup actor ~closing:false |> protocol_ok
               in
               A.Runtime_retirement.await (C.retirement holder) |> protocol_ok;
               A.drain_callback_retirement actor |> protocol_ok;
               [%test_eq: int] 0 !publications;
               let first = C.snapshot holder |> protocol_ok in
               assert (not (C.Snapshot.equal_progress (C.Snapshot.progress first) Joined));
               assert (List.is_empty (C.Snapshot.failures first));
               let live = C.advance holder ~actor ~after:first |> protocol_ok in
               assert (C.Snapshot.equal_progress (C.Snapshot.progress live) Pending);
               assert (List.is_empty (C.Snapshot.failures live));
               [%test_eq: int] 1 !effects;
               [%test_eq: int] 0 !publications;
               (match C.advance holder ~actor ~after:first with
                | Error error -> [%test_eq: P.Error.code] Conflict error.code
                | Ok _ -> failwith "stale snapshot advanced selected cleanup");
               Job_fixtures.with_actor
                 ~environment:(env, expected.spec.workspace_instance)
                 ~start:false
                 (fun _env _sw foreign _writer _backend ->
                    match C.advance holder ~actor:foreign ~after:live with
                    | Error error -> [%test_eq: P.Error.code] Conflict error.code
                    | Ok _ -> failwith "foreign Actor advanced selected cleanup");
               (* This legacy resource is entirely in-memory. Its failed Owner
                  remains retained after the later fatal Actor publication;
                  teardown must not manufacture a successful resource close. *)
               let closes = ref 0 in
               let resource =
                 { (Runtime_lease_tests.runtime ~close:(fun () -> Int.incr closes) ()) with
                   moderator_manager = Some manager
                 }
               in
               let owner =
                 Agent_server.Runtime_owner.create
                   ~actor
                   ~initial:(Some resource)
                   ~build:(fun () -> failwith "retirement unexpectedly rebuilt runtime")
               in
               (match Agent_server.Runtime_owner.unload_and_wait owner with
                | Error error ->
                  [%test_eq: P.Error.code] Conflict error.code;
                  assert error.retryable;
                  [%test_eq: string]
                    "runtime cleanup must finish before fresh admission"
                    error.message
                | Ok () -> failwith "final live callback pass permitted retirement");
               [%test_eq: int] 0 !closes;
               assert (Agent_server.Runtime_owner.is_loaded owner);
               assert (A.Runtime_retirement.is_finished (C.retirement holder));
               A.Runtime_retirement.await (C.retirement holder) |> protocol_ok;
               (* Owner advanced this SAME holder. Preserve earlier immutable
                  snapshots, and use only its latest version for the later pass. *)
               let after_owner = C.snapshot holder |> protocol_ok in
               assert (C.Snapshot.equal_progress (C.Snapshot.progress after_owner) Pending);
               assert (List.is_empty (C.Snapshot.failures after_owner));
               [%test_eq: int] 1 !effects;
               [%test_eq: int] 0 !publications;
               release_callback ();
               (match Eio.Promise.await_exn task with
                | Error (actual, backtrace) ->
                  assert (phys_equal actual failure);
                  assert (
                    String.is_substring
                      (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                      ~substring:"fail_publication")
                | Ok _ ->
                  failwith "publication failure became an ordinary callback result");
               (match Eio.Promise.await_exn exited with
                | Error (actual, _) -> assert (phys_equal actual failure)
                | Ok () -> failwith "publication failure left Actor owner running");
               unavailable (A.state actor);
               let stored = Agent_session.Memory_backend.state backend in
               assert_same_session_snapshot (Option.value_exn !published) stored;
               let receipt =
                 List.find_exn stored.moderator_executions ~f:(fun receipt ->
                   P.Id.Moderator_execution.equal
                     receipt.context.id
                     (Option.value_exn !selected))
               in
               (match receipt.status with
                | Failed _ -> ()
                | Running | Completed _ | Interrupted _ ->
                  failwith "released callback did not retain terminal receipt");
               (match stored.invocations with
                | [ { status = Resolved (Complete `Null); _ } ] -> ()
                | _ -> failwith "actual callback invocation outcome was not retained");
               let check_negative snapshot =
                 assert (C.Snapshot.equal_progress (C.Snapshot.progress snapshot) Terminal);
                 match C.Snapshot.failures snapshot with
                 | [ Raised (actual, backtrace) ] ->
                   assert (phys_equal actual failure);
                   assert (
                     String.is_substring
                       (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                       ~substring:"fail_publication")
                 | _ -> failwith "selected cleanup lost actual publication failure"
               in
               let check_completion snapshot =
                 (match S.producer_exit snapshot with
                  | Returned (Ok ()) -> ()
                  | Not_returned | Returned (Error _) ->
                    failwith "cleanup failure replaced actual positive producer exit");
                 let raw_failure = function
                   | Error (F.Failure.Raised (actual, backtrace)) ->
                     assert (phys_equal actual failure);
                     assert (
                       String.is_substring
                         (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                         ~substring:"fail_publication");
                     backtrace
                   | Ok () | Error (Protocol _) ->
                     failwith "actual publication failure changed representation"
                 in
                 let finish =
                   match S.finish_call snapshot with
                   | Completed outcome -> raw_failure outcome
                   | Not_attempted | Awaiting_reply ->
                     failwith "lexical finish unresolved"
                 in
                 let released =
                   match S.release snapshot with
                   | Completed outcome -> raw_failure outcome
                   | Not_released | Finishing -> failwith "actual release unresolved"
                 in
                 (* The raw handler reply can append legitimate re-raise frames.
                    The releasing invocation's original trace must remain its prefix. *)
                 let released_trace = Stdlib.Printexc.raw_backtrace_to_string released in
                 assert (not (String.is_empty released_trace));
                 assert (
                   String.is_prefix
                     (Stdlib.Printexc.raw_backtrace_to_string finish)
                     ~prefix:released_trace);
                 (match S.failures snapshot with
                  | [ Raised (actual, backtrace) ] ->
                    assert (phys_equal actual failure);
                    [%test_eq: string]
                      released_trace
                      (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                  | _ -> failwith "same handler failure was lost or counted twice");
                 assert (Option.is_none (S.capture_failure snapshot))
               in
               let completed = F.Completion.snapshot completion |> protocol_ok in
               check_completion completed;
               check_completion (F.Completion.snapshot completion |> protocol_ok);
               (match S.producer_exit initial_completion with
                | Not_returned -> ()
                | Returned _ -> failwith "original live completion snapshot mutated");
               let parent =
                 A.Initialization_scope.callback_cleanup constructor
                 |> F.Initialization.snapshot
                 |> protocol_ok
               in
               (match F.Cleanup_snapshot.failures parent with
                | [ Raised (actual, _) ] -> assert (phys_equal actual failure)
                | _ -> failwith "original Init parent lost callback cleanup failure");
               let failed = C.advance holder ~actor ~after:after_owner |> protocol_ok in
               check_negative failed;
               let repeated = C.advance holder ~actor ~after:failed |> protocol_ok in
               check_negative repeated;
               check_negative failed;
               assert (List.is_empty (C.Snapshot.failures first));
               assert (C.Snapshot.equal_progress (C.Snapshot.progress live) Pending);
               assert (List.is_empty (C.Snapshot.failures live));
               [%test_eq: int] 1 !effects;
               [%test_eq: int] 1 !publications;
               [%test_eq: int] 0 !closes;
               assert (Agent_server.Runtime_owner.is_loaded owner);
               assert_same_session_snapshot
                 stored
                 (Agent_session.Memory_backend.state backend))));
  print_endline
    "original selected callback: live is unfinished; real postrelease failure stays \
     terminal; foreign Actor rejected; invocation and publication run once";
  [%expect
    {| original selected callback: live is unfinished; real postrelease failure stays terminal; foreign Actor rejected; invocation and publication run once |}]
;;

let%expect_test
    "installed callback is captured before failed claim reply and independent capture \
     failure"
  =
  let module F = Agent_session.Callback_cleanup in
  let module S = F.Completion_snapshot in
  let module M = Chat_response.Moderator_manager in
  let claim_failure = Failure "callback claim publication failed after ACK" in
  let capture_failure = Failure "independent callback receipt capture failed" in
  let armed = ref false in
  let publications = ref 0 in
  let captures = ref 0 in
  let bodies = ref 0 in
  let completion = ref None in
  let initial = ref None in
  let published = ref None in
  let previous_backtraces = Stdlib.Printexc.backtrace_status () in
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous_backtraces)
    ~f:(fun () ->
      Stdlib.Printexc.record_backtrace true;
      with_owner_env
        ~state_committed:(fun state _ ->
          if
            !armed
            && List.exists
                 state.Agent_session.Session_state.moderator_executions
                 ~f:(fun receipt ->
                   match receipt.P.Moderator_execution.status with
                   | Running -> true
                   | Completed _ | Failed _ | Interrupted _ -> false)
          then (
            incr publications;
            published := Some state;
            fail_publication claim_failure))
        (fun env _callers actor backend _cancel exited ->
           let manager =
             prepared_legacy_callback_managers
               env
               ~id:"callback-claim-publication"
               ~source:
                 "let initial_state = 0\n\
                  let on_event ctx state event = Task.pure(state + 1)"
               ()
           in
           let original = Agent_authority_tests.authentication (Atomic.make true) in
           let expected = A.state actor |> protocol_ok in
           let constructor =
             A.begin_initialization_authenticated actor ~actor:original ~expected
             |> protocol_ok
           in
           armed := true;
           let result =
             capture (fun () ->
               M.handle_legacy_callback_entries
                 manager
                 ~selection:(Boundary Session_start)
                 ~session_id:"fixture"
                 ~now_ms:0
                 ~history:[]
                 ~available_tools:[]
                 ~session_meta:`Null
                 ~with_admission:(fun callback ~snapshot:_ _run ->
                   match
                     A.with_initialization_callback
                       actor
                       ~scope:constructor
                       ~manager
                       ~callback
                       ~capture_cleanup:(fun receipt ->
                         incr captures;
                         completion := Some receipt;
                         initial := Some (F.Completion.snapshot receipt |> protocol_ok);
                         fail_publication capture_failure)
                       (fun ~scope:_ ~commit:_ ->
                          incr bodies;
                          Ok ())
                   with
                   | Error error -> Error error.P.Error.message
                   | Ok _ -> failwith "failed installed claim entered callback"))
           in
           let check_claim_exception actual backtrace =
             assert (phys_equal actual claim_failure);
             assert (
               String.is_substring
                 (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                 ~substring:"fail_publication")
           in
           (match result with
            | Error (actual, backtrace) -> check_claim_exception actual backtrace
            | Ok _ -> failwith "independent capture failure replaced original claim raise");
           [%test_eq: int] 1 !publications;
           [%test_eq: int] 1 !captures;
           [%test_eq: int] 0 !bodies;
           (match Eio.Promise.await_exn exited with
            | Error (actual, backtrace) -> check_claim_exception actual backtrace
            | Ok () -> failwith "failed claim publication left Actor alive");
           unavailable (A.state actor);
           let stored = Agent_session.Memory_backend.state backend in
           assert_same_session_snapshot (Option.value_exn !published) stored;
           (match stored.moderator_executions with
            | [ { status = Running; _ } ] -> ()
            | _ -> failwith "claim did not actually install its Running ACK");
           let completion = Option.value_exn !completion in
           let check_snapshot snapshot =
             (match S.producer_exit snapshot with
              | Returned (Error (Raised (actual, backtrace))) ->
                check_claim_exception actual backtrace
              | Not_returned | Returned (Ok ()) | Returned (Error (Protocol _)) ->
                failwith "installed producer lost original failed claim outcome");
             let refusal =
               match S.finish_call snapshot with
               | Completed (Error (Protocol error)) ->
                 [%test_eq: P.Error.code] Server_shutting_down error.code;
                 error
               | Completed _ | Not_attempted | Awaiting_reply ->
                 failwith "actual finish did not retain dead-owner refusal"
             in
             (match S.release snapshot with
              | Not_released -> ()
              | Finishing | Completed _ ->
                failwith "failed claim fabricated a released bookkeeping receipt");
             (match S.capture_failure snapshot with
              | Some (Raised (actual, backtrace)) ->
                assert (phys_equal actual capture_failure);
                assert (
                  String.is_substring
                    (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                    ~substring:"fail_publication")
              | None | Some (Protocol _) -> failwith "independent capture failure lost");
             match S.failures snapshot with
             | [ Protocol actual_refusal; Raised (actual_capture, _) ] ->
               assert (phys_equal actual_refusal refusal);
               assert (phys_equal actual_capture capture_failure)
             | _ ->
               failwith "claim primary and independent cleanup evidence were conflated"
           in
           let snapshot = F.Completion.snapshot completion |> protocol_ok in
           check_snapshot snapshot;
           check_snapshot (F.Completion.snapshot completion |> protocol_ok);
           check_snapshot snapshot;
           (match S.producer_exit (Option.value_exn !initial) with
            | Not_returned -> ()
            | Returned _ -> failwith "capture-time snapshot changed after wrapper exit");
           assert (List.is_empty (S.failures (Option.value_exn !initial)));
           let parent =
             A.Initialization_scope.callback_cleanup constructor
             |> F.Initialization.snapshot
             |> protocol_ok
           in
           assert (F.Cleanup_snapshot.pending parent);
           (match F.Cleanup_snapshot.failures parent with
            | [ Protocol refusal; Raised (actual, _) ] ->
              [%test_eq: P.Error.code] Server_shutting_down refusal.code;
              assert (phys_equal actual capture_failure)
            | _ -> failwith "original Init parent dropped independent cleanup facts");
           [%test_eq: int] 1 !publications;
           [%test_eq: int] 1 !captures;
           [%test_eq: int] 0 !bodies));
  print_endline
    "installed ACK survives failed claim reply; capture precedes native setup; original \
     primary and independent cleanup failures remain separate";
  [%expect
    {|
    +queued moderator cleanup failed: session command mailbox is closed
    installed ACK survives failed claim reply; capture precedes native setup; original primary and independent cleanup failures remain separate
    |}]
;;
