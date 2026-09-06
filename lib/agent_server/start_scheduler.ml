open! Core

type activation =
  | Activated
  | Blocked of Agent_session.Quota_manager.blocking_scope
  | Retry
  | Drop

type t = { stopped : bool Atomic.t }

let ticket_of_state state =
  Option.map state.Agent_session.Session_state.spec.quota_key ~f:(fun quota_key ->
    Agent_session.Start_queue.
      { session_id = state.identity.session_id
      ; accepted_command_sequence = state.counters.revision
      ; quota_key
      ; created_at = state.identity.updated_at
      })
;;

let enqueue_state queue state =
  match ticket_of_state state with
  | None -> Ok ()
  | Some ticket -> Agent_session.Start_queue.enqueue queue ticket
;;

let seed_entry queue entry =
  let open Result.Let_syntax in
  let%bind state = Agent_session.Session_actor.state entry.Session_registry.actor in
  match state.lifecycle.observed with
  | Agent_protocol.Session.Queued_for_slot -> enqueue_state queue state
  | Stopped
  | Starting
  | Recovering
  | Idle
  | Running_turn _
  | Compacting _
  | Waiting_for_permission _
  | Stopping
  | Failed _ -> Ok ()
;;

let seed_recovered ~registry ~queue =
  Result.all_unit (List.map (Session_registry.entries registry) ~f:(seed_entry queue))
;;

let activate_actor entry capacity newly_acquired =
  let activated =
    let open Result.Let_syntax in
    let%bind () = Runtime_owner.ensure_loaded entry.Session_registry.runtime in
    Agent_session.Session_actor.activate_queued_start entry.actor
  in
  match activated with
  | Ok _ ->
    Option.iter capacity ~f:Session_capacity.runtime_ready;
    Activated
  | Error _ ->
    if newly_acquired then Option.iter capacity ~f:Session_capacity.release;
    Retry
;;

let activate_capacity entry capacity =
  match Session_capacity.try_acquire capacity ~queue_if_limited:true with
  | Acquired -> activate_actor entry (Some capacity) true
  | Already_acquired -> activate_actor entry (Some capacity) false
  | Queue_required scope -> Blocked scope
  | Rejected error -> if error.retryable then Retry else Drop
;;

let activate_entry entry =
  match Agent_session.Session_actor.state entry.Session_registry.actor with
  | Error _ -> Drop
  | Ok state ->
    (match state.lifecycle.observed, entry.capacity with
     | Queued_for_slot, None -> activate_actor entry None false
     | Queued_for_slot, Some capacity -> activate_capacity entry capacity
     | ( ( Stopped
         | Starting
         | Recovering
         | Idle
         | Running_turn _
         | Compacting _
         | Waiting_for_permission _
         | Stopping
         | Failed _ )
       , _ ) -> Drop)
;;

let process_ticket registry ticket =
  match Session_registry.find registry ticket.Agent_session.Start_queue.session_id with
  | None -> Drop
  | Some entry -> activate_entry entry
;;

let blocked_entry entry blockers =
  Option.exists entry.Session_registry.capacity ~f:(fun capacity ->
    List.exists blockers ~f:(Session_capacity.blocked_by capacity))
;;

let ticket_blocked registry blockers ticket =
  Session_registry.find registry ticket.Agent_session.Start_queue.session_id
  |> Option.exists ~f:(fun entry -> blocked_entry entry blockers)
;;

let rec process_heads registry queue blockers = function
  | [] -> Retry
  | ticket :: rest when ticket_blocked registry blockers ticket ->
    process_heads registry queue blockers rest
  | ticket :: rest ->
    let activation = process_ticket registry ticket in
    (match activation with
     | Activated ->
       ignore (Agent_session.Start_queue.complete queue ticket : bool);
       Activated
     | Drop ->
       ignore (Agent_session.Start_queue.complete queue ticket : bool);
       process_heads registry queue blockers rest
     | Retry -> process_heads registry queue blockers rest
     | Blocked scope -> process_heads registry queue (scope :: blockers) rest)
;;

let process_queue registry queue =
  process_heads registry queue [] (Agent_session.Start_queue.heads queue)
;;

let rec run t clock registry queue =
  if Atomic.get t.stopped
  then ()
  else (
    (match process_queue registry queue with
     | Retry | Blocked _ -> Eio.Time.sleep clock 0.01
     | Activated | Drop -> Eio.Fiber.yield ());
    run t clock registry queue)
;;

let start ~sw ~clock ~registry ~queue =
  let t = { stopped = Atomic.make false } in
  Eio.Fiber.fork ~sw (fun () -> run t clock registry queue);
  t
;;

let close t = Atomic.set t.stopped true
let is_running t = not (Atomic.get t.stopped)
