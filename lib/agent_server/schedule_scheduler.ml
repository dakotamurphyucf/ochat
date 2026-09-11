open! Core

type t =
  { closed : bool Atomic.t
  ; mutable busy : Session_registry.entry list
  }

let failure message = Agent_protocol.Error.create Interrupted ~message ~retryable:false ()

let is_overdue schedule timestamp =
  Agent_protocol.Timestamp.compare schedule.Agent_protocol.Schedule.next_due_at timestamp
  <= 0
;;

let rec reconcile_schedule entry startup_time (schedule : Agent_protocol.Schedule.t) =
  match schedule.status with
  | Agent_protocol.Schedule.Delivering ->
    (match
       Agent_session.Session_actor.retry_schedule
         entry.Session_registry.actor
         ~schedule_id:schedule.id
         ~generation:schedule.generation
     with
     | Ok schedule -> reconcile_schedule entry startup_time schedule
     | Error error -> Error error)
  | Scheduled when is_overdue schedule startup_time ->
    (match schedule.misfire with
     | Deliver_once_immediately -> Ok ()
     | Skip_if_expired ->
       Agent_session.Session_actor.skip_schedule
         entry.Session_registry.actor
         ~schedule_id:schedule.id
         ~generation:schedule.generation
       |> Result.map ~f:(fun (_ : Agent_protocol.Schedule.t) -> ())
     | Fail ->
       Agent_session.Session_actor.fail_schedule
         entry.actor
         ~schedule_id:schedule.id
         ~generation:schedule.generation
         (failure "schedule expired while the daemon was unavailable")
       |> Result.map ~f:(fun (_ : Agent_protocol.Schedule.t) -> ()))
  | Scheduled | Delivered | Cancelled | Failed _ -> Ok ()
;;

let reconcile_entry startup_time entry =
  let open Result.Let_syntax in
  let%bind _ =
    Agent_session.Session_actor.expire_subscriptions entry.Session_registry.actor
  in
  let%bind state = Agent_session.Session_actor.state entry.actor in
  List.fold_result state.schedules ~init:() ~f:(fun () schedule ->
    reconcile_schedule entry startup_time schedule)
;;

let reconcile_recovered ~registry ~startup_time =
  Session_registry.entries registry
  |> List.fold_result ~init:() ~f:(fun () entry -> reconcile_entry startup_time entry)
;;

let fail_claim entry schedule error =
  ignore
    (Agent_session.Session_actor.fail_schedule
       entry.Session_registry.actor
       ~schedule_id:schedule.Agent_protocol.Schedule.id
       ~generation:schedule.generation
       error
     : (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result)
;;

let unload_if_stopped entry observed =
  if
    match observed with
    | Agent_protocol.Session.Stopped -> true
    | Queued_for_slot
    | Starting
    | Recovering
    | Idle
    | Running_turn _
    | Compacting _
    | Waiting_for_permission _
    | Stopping
    | Failed _ -> false
  then
    ignore
      (Runtime_owner.unload entry.Session_registry.runtime
       : (unit, Agent_protocol.Error.t) result)
;;

let drain_idle_moderator entry =
  ignore
    (Runtime_owner.drain_idle_moderator entry.Session_registry.runtime
     : (bool, Agent_protocol.Error.t) result)
;;

let deliver_claimed entry observed (schedule : Agent_protocol.Schedule.t) =
  match Runtime_owner.deliver_schedule entry.Session_registry.runtime schedule with
  | Error { code = Interrupted; _ } ->
    (* Runtime retirement can interrupt a delivery before its checkpoint commit.
       Preserve it for a later runtime/restart. A committed or cancelled schedule
       rejects retry, so this cannot undo a terminal delivery or explicit stop. *)
    ignore
      (Agent_session.Session_actor.retry_schedule
         entry.actor
         ~schedule_id:schedule.id
         ~generation:schedule.generation
       : (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result);
    unload_if_stopped entry observed
  | Error error ->
    fail_claim entry schedule error;
    unload_if_stopped entry observed
  | Ok () ->
    drain_idle_moderator entry;
    unload_if_stopped entry observed
;;

let claim entry observed (schedule : Agent_protocol.Schedule.t) =
  match
    Agent_session.Session_actor.claim_schedule
      entry.Session_registry.actor
      ~schedule_id:schedule.Agent_protocol.Schedule.id
      ~generation:schedule.generation
  with
  | Ok (Some schedule) -> deliver_claimed entry observed schedule
  | Ok None | Error _ -> ()
;;

let process_entry entry =
  match Agent_session.Session_actor.due_schedules entry.Session_registry.actor with
  | Error _ -> ()
  | Ok (observed, schedules) ->
    List.iter schedules ~f:(claim entry observed);
    drain_idle_moderator entry
;;

let dispatch_entry t sw entry =
  if not (List.mem t.busy entry ~equal:phys_equal)
  then (
    t.busy <- entry :: t.busy;
    Eio.Fiber.fork ~sw (fun () ->
      Exn.protect
        ~f:(fun () -> if not (Atomic.get t.closed) then process_entry entry)
        ~finally:(fun () ->
          t.busy <- List.filter t.busy ~f:(fun active -> not (phys_equal active entry)))))
;;

let process t sw registry =
  Session_registry.entries registry
  |> List.iter ~f:(fun entry ->
    (* Expiry must keep running while an earlier timer callback owns the runtime.
       This actor-only sweep does not invoke user code; failed saves retry on the
       next scheduler pass. *)
    ignore
      (Agent_session.Session_actor.expire_subscriptions entry.Session_registry.actor
       : (int, Agent_protocol.Error.t) result);
    dispatch_entry t sw entry)
;;

let rec run t sw clock registry =
  if not (Atomic.get t.closed)
  then (
    process t sw registry;
    Eio.Time.Mono.sleep clock 0.05;
    run t sw clock registry)
;;

let start ~sw ~clock ~registry =
  let t = { closed = Atomic.make false; busy = [] } in
  Eio.Fiber.fork ~sw (fun () -> run t sw clock registry);
  t
;;

let close t = Atomic.set t.closed true
let is_running t = not (Atomic.get t.closed)
