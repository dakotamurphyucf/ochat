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
