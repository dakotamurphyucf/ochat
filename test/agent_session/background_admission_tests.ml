open Core
open Fixtures
open Job_fixtures
open Background_scheduler_tests
module Capacity = Agent_server.Job_capacity

let key actor (job : J.t) =
  let state = A.state actor |> protocol_ok in
  Capacity.Key.create
    ~principal_id:state.identity.creating_principal
    ~prompt:
      (Option.value_map
         state.spec.prompt_definition_id
         ~default:"<local>"
         ~f:Agent_protocol.Id.Prompt_definition.to_string)
    ~workspace_conflict_domain:state.spec.workspace_instance.conflict_domain
    ~session_id:job.session_id
    ~kind:job.kind
    ~nested_depth:0
;;

let reserve capacity key job =
  Capacity.reserve_job capacity key ~job |> protocol_ok |> Option.value_exn
;;

let%expect_test
    "reset retires uncommitted reservations even when no job record was created"
  =
  with_capacity_scheduler
    ~reject_save:(fun _ -> false)
    (fun env actor _backend calls _registry _scheduler _start capacity request ->
       let job = new_job (B.to_json request) in
       let quota = key actor job in
       let reservation = reserve capacity quota job in
       let state = A.state actor |> protocol_ok in
       let attachment_id = (List.hd_exn state.attachments).id in
       A.stop actor ~attachment_id ~mode:Cancel |> protocol_ok |> ignore;
       let stopped = A.state actor |> protocol_ok in
       A.reset
         actor
         ~attachment_id
         ~expected_revision:stopped.counters.revision
         { keep_history = true
         ; keep_tasks = false
         ; keep_grants = false
         ; keep_labels = true
         ; workspace_instance = None
         }
       |> protocol_ok
       |> ignore;
       let released = ref None in
       until env (fun () ->
         released := Capacity.try_acquire capacity quota |> protocol_ok;
         Option.is_some !released);
       Capacity.publish reservation;
       Capacity.abort reservation;
       Capacity.release (Option.value_exn !released);
       [%test_eq: int] 0 !calls;
       print_endline "generation advance retired the unpublished reservation");
  [%expect {| generation advance retired the unpublished reservation |}]
;;

let%expect_test
    "removing a session cancels its completion-saving worker and releases capacity"
  =
  let target = ref None
  and rejections = ref 0 in
  with_capacity_scheduler
    ~reject_save:(fun next ->
      match pending_save target next with
      | false -> false
      | true ->
        incr rejections;
        true)
    (fun env actor _backend calls registry scheduler _start capacity request ->
       let job = submit ~target actor (B.to_json request) in
       until env (fun () -> !rejections >= 1);
       Registry.remove registry session_id |> Option.value_exn |> ignore;
       Capacity.close_session capacity ~session_id;
       until env (fun () -> Scheduler.running_count scheduler = 0);
       let lease =
         Capacity.try_acquire capacity (key actor job) |> protocol_ok |> Option.value_exn
       in
       Capacity.release lease;
       [%test_eq: int] 1 !calls;
       print_endline
         "removed owner cancelled pending save; effect once and capacity reclaimed");
  [%expect {| removed owner cancelled pending save; effect once and capacity reclaimed |}]
;;

let%expect_test
    "durable launch commit transfers reserved capacity without early execution or double \
     charging"
  =
  let reject_commit = ref false in
  with_capacity_scheduler
    ~reject_save:(fun _ -> !reject_commit)
    (fun env actor backend calls _registry scheduler _start capacity request ->
       let job = new_job (B.to_json request) in
       let quota = key actor job in
       let reservation = reserve capacity quota job in
       assert (Option.is_none (Capacity.try_acquire capacity quota |> protocol_ok));
       let invocation = invocation_fixture () in
       let dispatched = I.dispatch invocation |> protocol_ok in
       let resolved =
         I.resolve
           dispatched
           ~session_id
           ~generation:0
           (Pending (Job job.id, `String "accepted"))
         |> protocol_ok
       in
       let changes =
         A.Extension_change.
           [ Invocation invocation
           ; Invocation dispatched
           ; Start_job job
           ; Invocation resolved
           ]
       in
       reject_commit := true;
       assert (Result.is_error (Job_delivery_tests.commit actor changes));
       [%test_eq: int] 0 !calls;
       assert (List.is_empty (Agent_session.Memory_backend.state backend).jobs);
       Capacity.abort reservation;
       Capacity.publish reservation;
       reject_commit := false;
       let reservation = reserve capacity quota job in
       Job_delivery_tests.commit actor changes |> protocol_ok |> ignore;
       (* The durable record is visible, but the reservation is not yet published. *)
       Eio.Time.sleep (Eio.Stdenv.clock env) 0.12;
       [%test_eq: int] 0 !calls;
       (match (current backend job).status with
        | Queued -> ()
        | _ -> failwith "job ran before reservation handoff");
       Capacity.publish reservation;
       Capacity.abort reservation;
       until env (fun () -> terminal backend job && Scheduler.running_count scheduler = 0);
       [%test_eq: int] 1 !calls;
       (match (current backend job).status with
        | Succeeded -> ()
        | _ -> failwith "reserved job could not execute");
       let available =
         Capacity.try_acquire capacity quota |> protocol_ok |> Option.value_exn
       in
       Capacity.release available;
       print_endline
         "abort released capacity; stored job waited; published job used one slot and \
          one effect");
  [%expect
    {| abort released capacity; stored job waited; published job used one slot and one effect |}]
;;

let%expect_test
    "cancellation before reservation handoff retires capacity and cannot be revived"
  =
  with_capacity_scheduler
    ~reject_save:(fun _ -> false)
    (fun env actor backend calls _registry _scheduler _start capacity request ->
       let job = new_job (B.to_json request) in
       let quota = key actor job in
       let reservation = reserve capacity quota job in
       A.add_job actor job |> protocol_ok |> ignore;
       A.cancel_job_internal actor ~job_id:job.id |> protocol_ok |> ignore;
       let acquired = ref None in
       until env (fun () ->
         acquired := Capacity.try_acquire capacity quota |> protocol_ok;
         Option.is_some !acquired);
       Capacity.publish reservation;
       Capacity.abort reservation;
       Eio.Time.sleep (Eio.Stdenv.clock env) 0.06;
       [%test_eq: int] 0 !calls;
       (match (current backend job).status with
        | Cancelled -> ()
        | _ -> failwith "cancelled reservation revived");
       Capacity.release (Option.value_exn !acquired);
       let next = new_job (B.to_json request) in
       let reservation = reserve capacity quota next in
       A.add_job actor next |> protocol_ok |> ignore;
       Capacity.publish reservation;
       until env (fun () -> terminal backend next);
       [%test_eq: int] 1 !calls;
       print_endline "cancelled reservation retired; late publish harmless; next job ran");
  [%expect {| cancelled reservation retired; late publish harmless; next job ran |}]
;;

let%expect_test
    "reservation ownership and claimed cleanup cannot be bypassed by stale job snapshots"
  =
  with_capacity_scheduler
    ~reject_save:(fun _ -> false)
    (fun _env actor _backend calls _registry _scheduler _start capacity request ->
       let job = new_job (B.to_json request) in
       let quota = key actor job in
       let reservation = reserve capacity quota job in
       let wrong_generation = { job with generation = 1 } in
       assert (
         Result.is_error (Capacity.try_acquire_job capacity quota ~job:wrong_generation));
       let foreign = { job with session_id = second_session_id } in
       assert (
         Result.is_error
           (Capacity.try_acquire_job capacity (key actor foreign) ~job:foreign));
       Capacity.retire_job capacity { wrong_generation with status = Cancelled };
       Capacity.retire_job capacity { foreign with status = Cancelled };
       assert (Option.is_none (Capacity.try_acquire capacity quota |> protocol_ok));
       Capacity.publish reservation;
       let lease =
         Capacity.try_acquire_job capacity quota ~job |> protocol_ok |> Option.value_exn
       in
       assert (Option.is_none (Capacity.try_acquire_job capacity quota ~job |> protocol_ok));
       Capacity.retire_job capacity { job with status = Cancelled };
       Capacity.abort reservation;
       assert (Option.is_none (Capacity.try_acquire capacity quota |> protocol_ok));
       Capacity.release lease;
       Capacity.release lease;
       let reservation = reserve capacity quota (new_job (B.to_json request)) in
       Capacity.abort reservation;
       [%test_eq: int] 0 !calls;
       print_endline
         "foreign ownership rejected; claimed capacity retained until actual worker \
          release");
  [%expect
    {| foreign ownership rejected; claimed capacity retained until actual worker release |}]
;;
