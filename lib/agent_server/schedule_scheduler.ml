open! Core

type sweep =
  { entry : Session_registry.entry
  ; mutable cancel : Eio.Cancel.t option
  }

type t =
  { closed : bool Atomic.t
  ; mutable busy : Session_registry.entry list
  ; mutable sweeps : sweep list
  ; idle : Eio.Condition.t
  }

let failure message = Agent_protocol.Error.create Interrupted ~message ~retryable:false ()

let is_overdue schedule timestamp =
  Agent_protocol.Timestamp.compare schedule.Agent_protocol.Schedule.next_due_at timestamp
  <= 0
;;

let reconcile_entry = Recovery_work.schedules

let reconcile_recovered ~registry ~startup_time =
  Session_registry.entries registry
  |> List.fold_result ~init:() ~f:(fun () entry -> reconcile_entry entry ~startup_time)
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

let process_entry t entry =
  match Agent_session.Session_actor.due_schedules entry.Session_registry.actor with
  | Error _ -> ()
  | Ok (observed, schedules) ->
    List.iter schedules ~f:(fun schedule ->
      if not (Atomic.get t.closed) then claim entry observed schedule);
    if not (Atomic.get t.closed) then drain_idle_moderator entry
;;

let dispatch_entry t sw entry =
  if (not (Atomic.get t.closed)) && not (List.mem t.busy entry ~equal:phys_equal)
  then (
    t.busy <- entry :: t.busy;
    Eio.Fiber.fork ~sw (fun () ->
      Exn.protect
        ~f:(fun () -> if not (Atomic.get t.closed) then process_entry t entry)
        ~finally:(fun () ->
          t.busy <- List.filter t.busy ~f:(fun active -> not (phys_equal active entry));
          if List.is_empty t.busy then Eio.Condition.broadcast t.idle)))
;;

let dispatch_recurrences t sw entry =
  if
    (not (Atomic.get t.closed))
    && not (List.exists t.sweeps ~f:(fun sweep -> phys_equal sweep.entry entry))
  then (
    let sweep = { entry; cancel = None } in
    t.sweeps <- sweep :: t.sweeps;
    let release () =
      sweep.cancel <- None;
      t.sweeps <- List.filter t.sweeps ~f:(fun active -> not (phys_equal active sweep));
      Eio.Condition.broadcast t.idle
    in
    match
      Eio.Fiber.fork ~sw (fun () ->
        Exn.protect
          ~f:(fun () ->
            try
              Eio.Cancel.sub (fun cancel ->
                sweep.cancel <- Some cancel;
                if not (Atomic.get t.closed)
                then
                  ignore
                    (Agent_session.Session_actor.sweep_recurrences entry.actor
                     : (unit, Agent_protocol.Error.t) result))
            with
            | Eio.Cancel.Cancelled _ as exn -> if not (Atomic.get t.closed) then raise exn)
          ~finally:release)
    with
    | () -> ()
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      release ();
      Exn.raise_with_original_backtrace exn backtrace)
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
    if Session_registry.execution_selected registry entry
    then (
      dispatch_recurrences t sw entry;
      dispatch_entry t sw entry))
;;

let rec run t sw clock registry =
  if not (Atomic.get t.closed)
  then (
    process t sw registry;
    Eio.Time.Mono.sleep clock 0.05;
    run t sw clock registry)
;;

let start_controlled ~enabled ~sw ~clock ~registry =
  let t =
    { closed = Atomic.make (not enabled)
    ; busy = []
    ; sweeps = []
    ; idle = Eio.Condition.create ()
    }
  in
  if enabled then Eio.Fiber.fork ~sw (fun () -> run t sw clock registry);
  t
;;

let start ~sw ~clock ~registry = start_controlled ~enabled:true ~sw ~clock ~registry

let close t =
  Atomic.set t.closed true;
  List.iter t.sweeps ~f:(fun sweep ->
    Option.iter sweep.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit))
;;

let rec await_recurrences_idle t =
  if not (List.is_empty t.sweeps)
  then (
    Eio.Condition.await_no_mutex t.idle;
    await_recurrences_idle t)
;;

let rec await_idle t =
  match t.busy, t.sweeps with
  | [], [] -> ()
  | _ ->
    Eio.Condition.await_no_mutex t.idle;
    await_idle t
;;

let is_running t = not (Atomic.get t.closed)
