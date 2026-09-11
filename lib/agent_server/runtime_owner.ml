open! Core

type background_lease =
  { cancel : unit -> unit
  ; finished : unit Eio.Promise.t
  ; finish : unit Eio.Promise.u
  }

type cleanup_outcome = (unit, exn * Stdlib.Printexc.raw_backtrace) result

type unload_outcome =
  ((unit, Agent_protocol.Error.t) result, exn * Stdlib.Printexc.raw_backtrace) result

exception Cleanup_failed of Agent_protocol.Error.t

type t =
  { actor : Agent_session.Session_actor.t
  ; build : unit -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result
  ; before_unload : (closing:bool -> (unit, Agent_protocol.Error.t) result) option
  ; mutex : Eio.Mutex.t
  ; mutable runtime : Agent_session.Runtime_builder.t option
  ; mutable closed : bool
  ; mutable close_finished : bool
  ; mutable unloading : unload_outcome Eio.Promise.t option
  ; mutable background_leases : background_lease list
  }

let create_internal ~before_unload ~actor ~initial ~build =
  { actor
  ; build
  ; before_unload
  ; mutex = Eio.Mutex.create ()
  ; runtime = initial
  ; closed = false
  ; close_finished = false
  ; unloading = None
  ; background_leases = []
  }
;;

let create_with_unload ~before_unload ~actor ~initial ~build =
  create_internal ~before_unload:(Some before_unload) ~actor ~initial ~build
;;

let create ~actor ~initial ~build =
  create_internal ~before_unload:None ~actor ~initial ~build
;;

let is_loaded t = Eio.Mutex.use_ro t.mutex (fun () -> Option.is_some t.runtime)

let install t (runtime : Agent_session.Runtime_builder.t) =
  let open Result.Let_syntax in
  let%bind () =
    match runtime.automatic_turn_policy with
    | None -> Ok ()
    | Some policy ->
      Agent_session.Session_actor.enable_automatic_turn_budget t.actor policy
  in
  let%bind _ =
    Agent_session.Session_actor.change_moderator t.actor runtime.moderator_snapshot
  in
  let%map () =
    Agent_session.Session_actor.set_operation_worker t.actor (Some runtime.worker)
  in
  t.runtime <- Some runtime
;;

let closed_error () =
  Agent_protocol.Error.create
    Server_shutting_down
    ~message:"session runtime owner is closed"
    ~retryable:true
    ()
;;

(* Host admission callbacks can fail while consulting another actor or store.
   Propagate their exception after releasing the mutex, so cleanup remains usable. *)
let with_owner_lock t ~protect f =
  let outcome =
    Eio.Mutex.use_rw ~protect t.mutex (fun () ->
      match f () with
      | result -> Ok result
      | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let ensure_loaded_locked t =
  let check (runtime : Agent_session.Runtime_builder.t) =
    match runtime.check_execution with
    | None -> Ok ()
    | Some check -> check ()
  in
  match t.closed, t.runtime with
  | true, _ -> Error (closed_error ())
  | false, _ when Option.is_some t.unloading ->
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"session runtime is waiting for background cleanup"
         ~retryable:true
         ())
  | false, Some runtime -> check runtime
  | false, None ->
    (match t.build () with
     | Error _ as failure -> failure
     | Ok runtime ->
       (match install t runtime with
        | Ok () -> check runtime
        | Error _ as failure ->
          runtime.close ();
          failure))
;;

let ensure_loaded t = with_owner_lock t ~protect:true (fun () -> ensure_loaded_locked t)

let background_busy () =
  Agent_protocol.Error.create
    Conflict
    ~message:"background execution still owns the loaded runtime"
    ~retryable:true
    ()
;;

(* Remove the reference before invoking cleanup, so a failed close cannot leave
   the same runtime available for a second retirement or poison the owner mutex. *)
let retire_runtime_locked t =
  let previous = t.runtime in
  t.runtime <- None;
  match
    ignore
      (Agent_session.Session_actor.set_operation_worker t.actor None
       : (unit, Agent_protocol.Error.t) result);
    Option.iter previous ~f:(fun runtime ->
      runtime.Agent_session.Runtime_builder.close ())
  with
  | () -> Ok ()
  | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
;;

let raise_cleanup = function
  | Ok () -> ()
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let with_background_runtime t f =
  Eio.Cancel.sub (fun context ->
    let active = Atomic.make true in
    let finished, finish = Eio.Promise.create () in
    let lease =
      { finished
      ; finish
      ; cancel =
          (fun () ->
            match Atomic.get active with
            | true -> Eio.Cancel.cancel context Exit
            | false -> ())
      }
    in
    let admitted =
      with_owner_lock t ~protect:true (fun () ->
        let open Result.Let_syntax in
        let%map () = ensure_loaded_locked t in
        let runtime = Option.value_exn t.runtime in
        t.background_leases <- lease :: t.background_leases;
        runtime)
    in
    match admitted with
    | Error _ as failure ->
      Atomic.set active false;
      failure
    | Ok runtime ->
      Exn.protect
        ~finally:(fun () ->
          (* Mutex protection starts only after acquiring it. Lease cleanup must
             also survive cancellation while waiting for another owner callback. *)
          Eio.Cancel.protect (fun () ->
            Atomic.set active false;
            Exn.protect
              ~finally:(fun () -> Eio.Promise.resolve lease.finish ())
              ~f:(fun () ->
                Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
                  t.background_leases
                  <- List.filter t.background_leases ~f:(fun current ->
                       not (phys_equal current lease));
                  match t.closed, t.background_leases, t.before_unload, t.unloading with
                  | true, [], None, None -> retire_runtime_locked t
                  | _ -> Ok ())
                |> raise_cleanup)))
        ~f:(fun () ->
          Eio.Fiber.check ();
          let result = f runtime in
          Eio.Fiber.check ();
          result))
;;

let moderation_source t =
  with_background_runtime t (fun runtime ->
    let module M = Chat_response.Moderator_manager in
    match runtime.Agent_session.Runtime_builder.moderator_manager with
    | None -> Ok None
    | Some manager ->
      (match M.extension_definition manager with
       | Some _ -> Ok (M.invocation_observer manager)
       | _ ->
         Error
           (Agent_protocol.Error.create
              Invalid_state
              ~message:
                "delegation.owner_mediation_unavailable: parent requires an \
                 extensibility moderator"
              ~retryable:false
              ())))
;;

let prepare_delegated_tool t ~delegation ~event ~authorize =
  with_background_runtime t (fun runtime ->
    let module A = Agent_session.Session_actor in
    let module M = Chat_response.Moderator_manager in
    let open Result.Let_syntax in
    let unavailable () =
      Error
        (Agent_protocol.Error.create
           Invalid_state
           ~message:
             "delegation.owner_mediation_unavailable: parent policy runtime is \
              unavailable"
           ~retryable:false
           ())
    in
    match runtime.Agent_session.Runtime_builder.moderator_manager with
    | None -> unavailable ()
    | Some manager when Option.is_none (M.extension_definition manager) -> unavailable ()
    | Some manager ->
      let history = ref [] in
      let claim ~notifications ~snapshot handle =
        A.with_delegated_moderator_event
          ~notifications
          t.actor
          ~delegation
          ~event
          ~authorize
          ~snapshot
          (fun ~executing ~event ~execute ~commit ->
             let%bind state = A.state t.actor in
             let%bind entries =
               Agent_session.History_codec.all_of_protocol
                 state.conversation.canonical_history
             in
             history := entries;
             handle ~executing ~event ~execute ~commit)
      in
      let result =
        Agent_session.Moderator_event.run_delegated
          ~event
          ~claim
          ?script_tools:runtime.moderator_script_tools
          ~manager
          ~history:(fun () -> !history)
          ~available_tools:runtime.moderator_tools
          ~session_meta:`Null
          ~now:Agent_protocol.Timestamp.now
          ()
      in
      (* A committed parent intent remains the parent's responsibility even if
         child authority was lost before the policy reply could be disclosed. *)
      let%bind _ = A.apply_moderator_follow_up t.actor in
      let%bind result = result in
      (match result with
       | Some { receipt = { decision = Some decision; _ }; _ } -> Ok decision
       | None | Some _ -> unavailable ()))
;;

let unload_locked t =
  match t.background_leases, t.runtime with
  | _ :: _, _ -> Error (background_busy ())
  | [], None -> Ok ()
  | [], Some runtime ->
    let open Result.Let_syntax in
    let%map () = Agent_session.Session_actor.set_operation_worker t.actor None in
    runtime.close ();
    t.runtime <- None
;;

let unload t =
  with_owner_lock t ~protect:true (fun () ->
    match t.unloading with
    | Some _ -> Error (background_busy ())
    | None -> unload_locked t)
;;

let prepare_dependency_stop t =
  match t.before_unload with
  | None -> Ok ()
  | Some prepare -> prepare ~closing:false
;;

let unload_and_wait t =
  (* A committed stop must join cleanup before closing runtime/workspace resources.
     Waiting with the mutex held would prevent the lease finalizers from releasing. *)
  Eio.Cancel.protect (fun () ->
    let open Result.Let_syntax in
    let%bind disposition =
      with_owner_lock t ~protect:true (fun () ->
        match t.unloading with
        | Some finished -> Ok (`Join finished)
        | None ->
          let finished, finish = Eio.Promise.create () in
          t.unloading <- Some finished;
          Ok (`Retire (t.background_leases, t.closed, finish)))
    in
    let outcome =
      match disposition with
      | `Join finished -> Eio.Promise.await finished
      | `Retire (leases, closing, finish) ->
        let outcome =
          try
            let result =
              let%bind () =
                match t.before_unload with
                | None -> Ok ()
                | Some prepare -> prepare ~closing
              in
              List.iter leases ~f:(fun lease -> lease.cancel ());
              List.iter leases ~f:(fun lease -> Eio.Promise.await lease.finished);
              with_owner_lock t ~protect:true (fun () -> retire_runtime_locked t)
              |> raise_cleanup;
              Ok ()
            in
            Ok result
          with
          | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
        in
        with_owner_lock t ~protect:true (fun () ->
          t.unloading <- None;
          Eio.Promise.resolve finish outcome);
        outcome
    in
    match outcome with
    | Ok result -> result
    | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace)
;;

let with_unloaded t f =
  let result =
    (* An actor request continues after its caller cancels. Keep the owner until
       its bounded checkpoint actually returns, rather than permitting reload
       while that actor is still inspecting/deleting cache-dependent artifacts. *)
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      try
        Ok
          (match t.closed, t.unloading, t.runtime, t.background_leases with
           | false, None, None, [] -> Result.map (f ()) ~f:Option.some
           | _ -> Ok None)
      with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match result with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let retire_after_administration t =
  let previous = t.runtime in
  t.runtime <- None;
  Eio.Cancel.protect (fun () ->
    ignore
      (Agent_session.Session_actor.set_operation_worker t.actor None
       : (unit, Agent_protocol.Error.t) result);
    Option.iter previous ~f:(fun runtime ->
      ignore
        (Result.try_with runtime.Agent_session.Runtime_builder.close : (unit, exn) result)))
;;

let with_administration t f =
  let result =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
      try
        Ok
          (if t.closed
           then Error (closed_error ())
           else if Option.is_some t.unloading || not (List.is_empty t.background_leases)
           then Error (background_busy ())
           else (
             match f () with
             | Error _ as failure -> failure
             | Ok _ as result ->
               retire_after_administration t;
               result))
      with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match result with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let parse_user_content t ~id content =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match t.closed, t.runtime with
    | true, _ -> Error (closed_error ())
    | false, Some runtime -> runtime.parse_user_content ~id content
    | false, None ->
      Error
        (Agent_protocol.Error.create
           Invalid_state
           ~message:"session runtime is not loaded"
           ~retryable:true
           ()))
;;

let with_activity (runtime : Agent_session.Runtime_builder.t) f =
  match runtime.activity with
  | None -> f ()
  | Some activity ->
    (try Agent_session.Runtime_activity.run activity f with
     | Agent_session.Runtime_activity.Closed ->
       Error
         (Agent_protocol.Error.create
            Interrupted
            ~message:"generated runtime execution scope is closed"
            ~retryable:false
            ())
     | Eio.Cancel.Cancelled _ as exn ->
       (match Eio.Fiber.is_cancelled () with
        | true -> raise exn
        | false ->
          Error
            (Agent_protocol.Error.create
               Interrupted
               ~message:"generated runtime execution scope was cancelled"
               ~retryable:false
               ())))
;;

let with_cancellable_access t f =
  with_owner_lock t ~protect:false (fun () ->
    let open Result.Let_syntax in
    let%bind () = Eio.Cancel.protect (fun () -> ensure_loaded_locked t) in
    with_activity (Option.value_exn t.runtime) f)
;;

let submit_ingress t ~producer ~registration_id ~namespace ~key ~payload =
  with_cancellable_access t (fun () ->
    let module A = Agent_session.Session_actor in
    let open Result.Let_syntax in
    let%bind () = Eio.Cancel.protect (fun () -> ensure_loaded_locked t) in
    let%bind runtime, source =
      match t.runtime with
      | Some runtime when Option.is_some runtime.moderator_script_tools ->
        Option.bind
          runtime.moderator_manager
          ~f:Chat_response.Moderator_manager.invocation_observer
        |> Option.map ~f:(fun source -> runtime, source)
        |> Result.of_option
             ~error:
               (Agent_protocol.Error.invalid_request
                  "ingress requires a qualified moderator")
      | _ -> Error (Agent_protocol.Error.invalid_request "ingress runtime is unavailable")
    in
    A.with_moderator_checkpoint t.actor (fun () ->
      let%bind decision =
        A.prepare_ingress_submission
          t.actor
          ~source
          ~producer
          ~registration_id
          ~namespace
          ~key
          ~payload
      in
      match decision with
      | Duplicate receipt -> Ok receipt
      | Enqueue proposal ->
        let%bind frame = Agent_session.Ingress_submission.frame proposal in
        let%bind payload =
          Chat_response.Ingress_delivery.capture frame
          |> Chatml.Chatml_value_codec.Snapshot.of_value
          |> Result.map ~f:Chatml.Chatml_value_codec.Snapshot.to_jsonaf
          |> Result.map_error ~f:Agent_protocol.Error.invalid_request
        in
        let committed = ref None in
        let%bind _ =
          runtime.enqueue_internal_event payload ~prepare:(fun ~before ~snapshot ->
            let%map receipt =
              A.commit_ingress_submission t.actor proposal ~before ~snapshot
            in
            committed := Some receipt)
        in
        Result.of_option
          !committed
          ~error:
            (Agent_protocol.Error.invalid_request
               "ingress queue did not commit a receipt")))
;;

let deliver_schedule t (schedule : Agent_protocol.Schedule.t) =
  with_cancellable_access t (fun () ->
    let open Result.Let_syntax in
    let%bind () = Eio.Cancel.protect (fun () -> ensure_loaded_locked t) in
    match t.runtime with
    | Some runtime ->
      let%bind () =
        match schedule.ownership with
        | None -> Ok ()
        | Some ownership ->
          (match
             Option.bind
               runtime.moderator_manager
               ~f:Chat_response.Moderator_manager.invocation_observer
           with
           | Some source
             when Agent_protocol.Invocation.equal_observer source ownership.source ->
             Ok ()
           | _ ->
             Error
               (Agent_protocol.Error.create
                  Permission_denied
                  ~message:"timer belongs to a different moderator source"
                  ~retryable:false
                  ()))
      in
      let%bind payload =
        match schedule.ownership with
        | None -> Ok schedule.payload
        | Some _ ->
          let module V = Chatml.Chatml_value_codec in
          Chat_response.Schedule_delivery.capture schedule
          |> Result.bind ~f:V.Snapshot.of_value
          |> Result.map ~f:V.Snapshot.to_jsonaf
          |> Result.map_error ~f:Agent_protocol.Error.invalid_request
      in
      Agent_session.Session_actor.with_moderator_checkpoint t.actor (fun () ->
        runtime.enqueue_internal_event payload ~prepare:(fun ~before ~snapshot ->
          Agent_session.Session_actor.complete_schedule
            ~expected:before
            ~expected_schedule:schedule
            t.actor
            ~schedule_id:schedule.id
            ~generation:schedule.generation
            ~moderator_snapshot:
              (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
          |> Result.map ~f:ignore)
        |> Result.map ~f:ignore)
    | None ->
      Error
        (Agent_protocol.Error.create
           Internal_error
           ~message:"runtime load completed without an installed runtime"
           ~retryable:false
           ()))
;;

let fail_idle_moderator t failure =
  ignore
    (Agent_session.Session_actor.fail_idle_moderator t.actor failure
     : (unit, Agent_protocol.Error.t) result);
  Error failure
;;

let complete_idle_moderator t drain =
  match Agent_session.Session_actor.complete_idle_moderator t.actor drain with
  | Ok () -> Ok drain.Agent_session.Runtime_builder.remaining_events
  | Error failure -> fail_idle_moderator t failure
;;

let idle_callback_limit t =
  let open Result.Let_syntax in
  let%map state = Agent_session.Session_actor.state t.actor in
  match state.automatic_turn_budget with
  | None -> 32
  | Some budget ->
    (match
       Chat_response.Automatic_turn_policy.has_pause_condition
         budget.policy
         Pause_internal_event_drains
     with
     | true -> 0
     | false -> Int.min 32 budget.policy.budget.max_internal_event_drain)
;;

let drain_loaded_legacy_events t runtime =
  match Agent_session.Session_actor.claim_idle_moderator t.actor with
  | Error _ as failure -> failure
  | Ok None -> Ok false
  | Ok (Some history) ->
    (match runtime.Agent_session.Runtime_builder.drain_internal_events history with
     | Ok drain -> complete_idle_moderator t drain
     | Error failure -> fail_idle_moderator t failure)
;;

let drain_loaded_queued_events ~max_events t runtime manager =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  let open Result.Let_syntax in
  let history = ref [] in
  let claim ~snapshot handle =
    A.with_current_idle_queued_moderator_event_tools
      t.actor
      ~snapshot
      (fun ~executing ~retirement_reason ~event ~execute ~commit ->
         let%bind state = A.state t.actor in
         let%bind entries =
           Agent_session.History_codec.all_of_protocol
             state.conversation.canonical_history
         in
         history := entries;
         handle ~executing ~retirement_reason ~event ~execute ~commit)
  in
  let rec loop remaining handled =
    match remaining with
    | 0 -> Ok true
    | _ ->
      let%bind state = A.state t.actor in
      let blocked =
        Option.exists (M.invocation_observer manager) ~f:(fun observer ->
          Agent_session.Queued_moderator_event.has_unsettled_claim ~state ~observer)
      in
      if blocked
      then Ok handled
      else (
        let%bind outcome =
          Agent_session.Moderator_event.run_queued_idle
            ~claim
            ?script_tools:runtime.Agent_session.Runtime_builder.moderator_script_tools
            ~manager
            ~history:(fun () -> !history)
            ~available_tools:runtime.moderator_tools
            ~session_meta:`Null
            ~now:Agent_protocol.Timestamp.now
            ()
        in
        match outcome with
        | None -> Ok handled
        | Some outcome ->
          (match
             Chat_response.Runtime_semantics.should_end_session outcome.runtime_requests
           with
           | Some _ -> Ok true
           | None -> loop (remaining - 1) true))
  in
  loop max_events false
;;

let drain_loaded_idle_moderator t runtime =
  let open Result.Let_syntax in
  let%bind max_events = idle_callback_limit t in
  match max_events with
  | 0 -> Ok false
  | _ ->
    (match runtime.Agent_session.Runtime_builder.moderator_manager with
     | Some manager
       when Option.is_some (Chat_response.Moderator_manager.extension_definition manager)
       ->
       let open Result.Let_syntax in
       let%bind more = drain_loaded_queued_events ~max_events t runtime manager in
       let%map applied = Agent_session.Session_actor.apply_moderator_follow_up t.actor in
       more || applied
     | Some _ -> Eio.Cancel.protect (fun () -> drain_loaded_legacy_events t runtime)
     | None -> Ok false)
;;

let pending_observation state observer =
  List.exists state.Agent_session.Session_state.invocations ~f:(fun invocation ->
    invocation.context.generation = state.identity.generation
    && Agent_protocol.Id.Session.equal
         invocation.context.session_id
         state.identity.session_id
    &&
    match invocation.observation, invocation.status with
    | Some { observer = owner; status = Awaiting; _ }, (Resolved _ | Published _) ->
      Agent_protocol.Invocation.equal_observer owner observer
    | _ -> false)
;;

let drain_loaded_observations t runtime =
  let open Result.Let_syntax in
  let%bind max_observations = idle_callback_limit t in
  match max_observations with
  | 0 -> Ok false
  | _ ->
    (match
       Option.bind
         runtime.Agent_session.Runtime_builder.moderator_manager
         ~f:(fun manager ->
           Option.map
             (Chat_response.Moderator_manager.invocation_observer manager)
             ~f:(fun observer -> manager, observer))
     with
     | None -> Ok false
     | Some (manager, observer) ->
       let history = ref [] in
       let with_history handle =
         let%bind state = Agent_session.Session_actor.state t.actor in
         let%bind entries =
           Agent_session.History_codec.all_of_protocol
             state.conversation.canonical_history
         in
         history := entries;
         handle ()
       in
       let drain =
         match
           ( runtime.moderator_script_tools
           , Chat_response.Moderator_manager.extension_definition manager )
         with
         | Some script_tools, Some definition ->
           Agent_session.Moderator_observation.drain_idle_with_tools
             ~max_observations
             ~script_tools
             ~definition
             ~claim:(fun handle ->
               Agent_session.Session_actor.with_idle_moderator_observation_tools
                 t.actor
                 ~observer
                 (fun ~observing ~execute ~commit ->
                    with_history (fun () -> handle ~observing ~execute ~commit)))
         | _ ->
           Agent_session.Moderator_observation.drain_idle
             ~max_observations
             ~on_tool_call:(fun ~name:_ ~args:_ ->
               Ok
                 (Chat_response.Moderation.Capabilities.Tool_error
                    "invocation.unavailable"))
             ~claim:(fun handle ->
               Agent_session.Session_actor.with_idle_moderator_observation
                 t.actor
                 ~observer
                 (fun ~observing ~commit ->
                    with_history (fun () -> handle ~observing ~commit)))
       in
       let%map drain =
         drain
           ~manager
           ~history:(fun () -> !history)
           ~available_tools:runtime.moderator_tools
           ~session_meta:`Null
           ~now:Agent_protocol.Timestamp.now
           ()
       in
       drain.budget_exhausted)
;;

let drain_loaded_notifications runtime =
  match runtime.Agent_session.Runtime_builder.moderator_activation with
  | Some activation when activation.pending () -> Ok false
  | _ ->
    (match runtime.idle_notifications with
     | None -> Ok false
     | Some deliver -> deliver ())
;;

let snapshot_has_pending_events t =
  let open Result.Let_syntax in
  let%bind state = Agent_session.Session_actor.state t.actor in
  if
    Agent_protocol.Session.equal_desired_state state.lifecycle.desired Running
    && (match state.lifecycle.observed with
        | Agent_protocol.Session.Idle -> true
        | Stopped
        | Queued_for_slot
        | Starting
        | Recovering
        | Running_turn _
        | Compacting _
        | Waiting_for_permission _
        | Stopping
        | Failed _ -> false)
    && Option.is_none state.active_operation
    && (not state.halted)
    && Option.is_none state.failure
  then (
    let%bind queued =
      Agent_session.Runtime_builder.moderator_snapshot_has_queued_events state.moderator
    in
    let%bind halted =
      Agent_session.Runtime_builder.moderator_snapshot_is_halted state.moderator
    in
    let%map observer =
      Agent_session.Runtime_builder.moderator_snapshot_observer state.moderator
    in
    Option.exists t.runtime ~f:(fun runtime ->
      Option.exists runtime.moderator_activation ~f:(fun activation ->
        activation.pending ()))
    || (queued
        && (not halted)
        && not
             (Option.exists observer ~f:(fun observer ->
                Agent_session.Queued_moderator_event.has_unsettled_claim ~state ~observer))
       )
    || List.exists state.invocations ~f:Agent_session.Observation_follow_up.pending
    || List.exists
         state.moderator_executions
         ~f:Agent_session.Observation_follow_up.pending_event
    || ((not halted) && Option.exists observer ~f:(pending_observation state))
    || ((not halted) && Agent_session.Notification_delivery.has_idle_work state))
  else Ok false
;;

let drain_idle_moderator_locked t =
  let open Result.Let_syntax in
  let%bind pending = snapshot_has_pending_events t in
  if not pending
  then Ok false
  else (
    let%bind () = Eio.Cancel.protect (fun () -> ensure_loaded_locked t) in
    match t.runtime with
    | Some runtime ->
      with_activity runtime (fun () ->
        let%bind notifications = drain_loaded_notifications runtime in
        let%bind applied =
          match notifications with
          | true -> Ok true
          | false -> Agent_session.Session_actor.apply_moderator_follow_up t.actor
        in
        if applied
        then Ok true
        else (
          let%bind activated =
            match runtime.moderator_activation with
            | None -> Ok false
            | Some activation -> activation.run ()
          in
          if activated
          then Ok true
          else (
            let%bind more_observations = drain_loaded_observations t runtime in
            let%bind applied =
              Agent_session.Session_actor.apply_moderator_follow_up t.actor
            in
            if applied
            then Ok true
            else (
              let%map more_events = drain_loaded_idle_moderator t runtime in
              more_observations || more_events))))
    | None ->
      Error
        (Agent_protocol.Error.create
           Internal_error
           ~message:"runtime load completed without an installed runtime"
           ~retryable:false
           ()))
;;

let drain_idle_moderator t =
  let outcome =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
      match drain_idle_moderator_locked t with
      | result -> Ok result
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | Ok result -> result
  | Error (exn, backtrace) ->
    (* Stopping one idle session cancels its owned event sub-context. Once that
       scope has unwound, the scheduler's caller may still be live. Propagating
       that local cancellation out of its worker would fail the shared daemon
       switch and strand other requests. Caller/shutdown cancellation must still
       propagate. The event borrow has already recorded its interruption. *)
    (match Eio.Fiber.is_cancelled () with
     | true -> Exn.raise_with_original_backtrace exn backtrace
     | false ->
       Error
         (Agent_protocol.Error.create
            Interrupted
            ~message:"idle moderator execution was cancelled"
            ~retryable:false
            ()))
;;

let with_loaded_runtime t f =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let open Result.Let_syntax in
    let%bind () = ensure_loaded_locked t in
    match t.runtime with
    | Some runtime -> f runtime
    | None ->
      Error
        (Agent_protocol.Error.create
           Internal_error
           ~message:"runtime load completed without an installed runtime"
           ~retryable:false
           ()))
;;

module For_testing = struct
  let with_loaded_runtime t f = with_loaded_runtime t (fun _ -> f ())
end

let execute_model_job t ~recipe ~payload =
  let outcome =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
      match
        let open Result.Let_syntax in
        let%bind () = Eio.Cancel.protect (fun () -> ensure_loaded_locked t) in
        let runtime = Option.value_exn t.runtime in
        runtime.execute_model_job ~recipe ~payload
      with
      | result -> Ok result
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

type background_result =
  | Completed of Agent_protocol.Completion.t
  | Pending of Agent_protocol.Job.dependency

let execute_background_job t (job : Agent_protocol.Job.t) =
  with_background_runtime t (fun runtime ->
    let open Result.Let_syntax in
    let%bind executor =
      Result.of_option
        runtime.background_executor
        ~error:
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"generic background execution is not configured"
             ~retryable:false
             ())
    in
    let%bind request =
      Chat_response.Background_request.of_json ~policy:executor.policy job.payload
    in
    let%bind deadline =
      Result.try_with (fun () ->
        Time_ns.add
          (Agent_protocol.Timestamp.to_time_ns job.created_at)
          (Time_ns.Span.of_sec
             (Chat_response.Background_request.policy request).execution.wall_seconds)
        |> Agent_protocol.Timestamp.of_time_ns)
      |> Result.map_error ~f:(fun _ ->
        Agent_protocol.Error.invalid_request
          "background deadline is outside the supported timestamp range")
    in
    match Agent_protocol.Timestamp.compare deadline (executor.now ()) <= 0 with
    | true -> Ok (Completed Agent_protocol.Completion.Expired)
    | false ->
      let%bind result =
        Agent_session.Session_actor.with_job_execution
          t.actor
          ~job_id:job.id
          ~generation:job.generation
          ~attempt:job.attempt
          ~deadline:(Some deadline)
          (fun execution ->
             let job = execution.job in
             let is_halted () =
               match Agent_session.Session_actor.state t.actor with
               | Error _ -> true
               | Ok state ->
                 state.halted
                 || Option.is_some state.failure
                 || Agent_protocol.Session.equal_desired_state
                      state.lifecycle.desired
                      Stopped
                 || not
                      (List.exists state.jobs ~f:(fun current ->
                         Agent_protocol.Id.Job.equal current.id job.id
                         && Int.equal current.generation job.generation
                         && Int.equal current.attempt job.attempt
                         &&
                         match current.status with
                         | Running | Waiting_permission _ -> true
                         | _ -> false))
             in
             executor.run
               ~job
               ~deadline
               ~execute:execution.execute
               ~moderator_execute:execution.moderator_execute
               ~claim_event:execution.claim_event
               ~is_halted
               ~request)
      in
      let%bind () =
        match result.runtime_requests with
        | [] -> Ok ()
        | _ ->
          Error
            (Agent_protocol.Error.create
               Invalid_state
               ~message:"background runtime request consumption is not installed"
               ~retryable:false
               ())
      in
      (match result.pending, result.resolved.status with
       | Some target, Resolved (Complete _) ->
         (match target.invocation.status, target.invocation.context.deadline with
          | Resolved (Pending (work, _)), Some deadline ->
            let policy = Chat_response.Background_request.policy request in
            Ok
              (Pending
                 { invocation_id = target.invocation.context.id
                 ; work
                 ; deadline
                 ; completion_schema = target.completion_schema
                 ; max_output_bytes = policy.max_output_bytes
                 ; max_output_depth = policy.execution.max_depth
                 })
          | _ ->
            Error
              (Agent_protocol.Error.invalid_request
                 "background Pending target has no owned work or deadline"))
       | None, Resolved (Complete value) ->
         Ok (Completed (Agent_protocol.Completion.Succeeded value))
       | _, Resolved (Fail error) ->
         Ok (Completed (Agent_protocol.Completion.Failed error))
       | _, Resolved (Cancelled reason) ->
         Ok (Completed (Agent_protocol.Completion.Cancelled reason))
       | _ ->
         Error
           (Agent_protocol.Error.create
              Invalid_state
              ~message:"background execution did not produce a terminal outcome"
              ~retryable:false
              ())))
;;

let deliver_model_job_completion t (job : Agent_protocol.Job.t) =
  with_cancellable_access t (fun () ->
    let open Result.Let_syntax in
    let%bind () = Eio.Cancel.protect (fun () -> ensure_loaded_locked t) in
    let runtime = Option.value_exn t.runtime in
    Agent_session.Session_actor.with_moderator_checkpoint t.actor (fun () ->
      runtime.enqueue_model_job_completion job ~prepare:(fun ~before ~snapshot ->
        Agent_session.Session_actor.deliver_job
          ~expected:before
          ~expected_job:job
          t.actor
          ~job_id:job.id
          ~generation:job.generation
          ~moderator_snapshot:
            (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
        |> Result.map ~f:ignore)
      |> Result.map ~f:ignore))
;;

let deliver_moderated_background_job_completion
      t
      (runtime : Agent_session.Runtime_builder.t)
      (job : Agent_protocol.Job.t)
  =
  let open Result.Let_syntax in
  Agent_session.Session_actor.with_moderator_checkpoint t.actor (fun () ->
    let%bind state = Agent_session.Session_actor.state t.actor in
    let%bind retired =
      Agent_session.Session_actor.retire_obsolete_moderator_delivery
        t.actor
        ~revision:state.counters.revision
        ~job
    in
    match retired with
    | true -> Ok ()
    | false ->
      let%bind observer =
        Option.bind
          runtime.moderator_manager
          ~f:Chat_response.Moderator_manager.invocation_observer
        |> Result.of_option
             ~error:
               (Agent_protocol.Error.invalid_request
                  "background completion requires a qualified moderator")
      in
      let%bind frame = Agent_session.Background_job_event.frame ~state ~observer job in
      let%bind payload =
        Chat_response.Background_delivery.capture frame
        |> Chatml.Chatml_value_codec.Snapshot.of_value
        |> Result.map ~f:Chatml.Chatml_value_codec.Snapshot.to_jsonaf
        |> Result.map_error ~f:Agent_protocol.Error.invalid_request
      in
      runtime.enqueue_internal_event payload ~prepare:(fun ~before ~snapshot ->
        Agent_session.Session_actor.deliver_job
          ~expected:before
          ~expected_job:job
          t.actor
          ~job_id:job.id
          ~generation:job.generation
          ~moderator_snapshot:
            (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
        |> Result.map ~f:ignore)
      |> Result.map ~f:ignore)
;;

let deliver_background_job_completion t (job : Agent_protocol.Job.t) =
  with_cancellable_access t (fun () ->
    let open Result.Let_syntax in
    let%bind () = Eio.Cancel.protect (fun () -> ensure_loaded_locked t) in
    let runtime = Option.value_exn t.runtime in
    let%bind state = Agent_session.Session_actor.state t.actor in
    let standalone =
      match job.launch with
      | Some { owner = Invocation id; _ } ->
        List.exists state.invocations ~f:(fun invocation ->
          Agent_protocol.Id.Invocation.equal invocation.context.id id
          && Option.is_some invocation.completion_contract)
      | _ -> false
    in
    match standalone, runtime.standalone_completion with
    | true, Some deliver -> deliver job
    | true, None ->
      Error
        (Agent_protocol.Error.invalid_request "standalone completion adapter unavailable")
    | false, _ -> deliver_moderated_background_job_completion t runtime job)
;;

let close t =
  let leases, retired =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.closed <- true;
      match t.before_unload, t.unloading, t.background_leases with
      | Some _, _, _ -> [], Ok ()
      | None, None, [] -> [], retire_runtime_locked t
      | None, _, leases -> leases, Ok ())
  in
  List.iter leases ~f:(fun lease -> lease.cancel ());
  raise_cleanup retired
;;

let close_and_wait t =
  Eio.Cancel.protect (fun () ->
    close t;
    match Eio.Mutex.use_ro t.mutex (fun () -> t.close_finished) with
    | true -> ()
    | false ->
      (match unload_and_wait t with
       | Ok () ->
         Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.close_finished <- true)
       | Error error -> raise (Cleanup_failed error)))
;;
