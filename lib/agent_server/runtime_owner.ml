open! Core

type t =
  { actor : Agent_session.Session_actor.t
  ; build : unit -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result
  ; mutex : Eio.Mutex.t
  ; mutable runtime : Agent_session.Runtime_builder.t option
  ; mutable closed : bool
  }

let create ~actor ~initial ~build =
  { actor; build; mutex = Eio.Mutex.create (); runtime = initial; closed = false }
;;

let is_loaded t = Eio.Mutex.use_ro t.mutex (fun () -> Option.is_some t.runtime)

let install t (runtime : Agent_session.Runtime_builder.t) =
  let open Result.Let_syntax in
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

let ensure_loaded_locked t =
  match t.closed, t.runtime with
  | true, _ -> Error (closed_error ())
  | false, Some _ -> Ok ()
  | false, None ->
    (match t.build () with
     | Error _ as failure -> failure
     | Ok runtime ->
       (match install t runtime with
        | Ok () -> Ok ()
        | Error _ as failure ->
          runtime.close ();
          failure))
;;

let ensure_loaded t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> ensure_loaded_locked t)
;;

let unload_locked t =
  match t.runtime with
  | None -> Ok ()
  | Some runtime ->
    let open Result.Let_syntax in
    let%map () = Agent_session.Session_actor.set_operation_worker t.actor None in
    runtime.close ();
    t.runtime <- None
;;

let unload t = Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> unload_locked t)

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

let enqueue_internal_event t payload =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let open Result.Let_syntax in
    let%bind () = ensure_loaded_locked t in
    match t.runtime with
    | Some runtime -> runtime.enqueue_internal_event payload
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

let drain_loaded_legacy_events t runtime =
  match Agent_session.Session_actor.claim_idle_moderator t.actor with
  | Error _ as failure -> failure
  | Ok None -> Ok false
  | Ok (Some history) ->
    (match runtime.Agent_session.Runtime_builder.drain_internal_events history with
     | Ok drain -> complete_idle_moderator t drain
     | Error failure -> fail_idle_moderator t failure)
;;

let drain_loaded_queued_events t runtime manager =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  let open Result.Let_syntax in
  let history = ref [] in
  let claim ~snapshot handle =
    A.with_current_idle_queued_moderator_event_tools
      t.actor
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
  loop 32 false
;;

let drain_loaded_idle_moderator t runtime =
  match runtime.Agent_session.Runtime_builder.moderator_manager with
  | Some manager
    when Option.is_some (Chat_response.Moderator_manager.extension_definition manager) ->
    let open Result.Let_syntax in
    let%bind more = drain_loaded_queued_events t runtime manager in
    let%map applied = Agent_session.Session_actor.apply_moderator_follow_up t.actor in
    more || applied
  | _ -> Eio.Cancel.protect (fun () -> drain_loaded_legacy_events t runtime)
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
  match
    Option.bind runtime.Agent_session.Runtime_builder.moderator_manager ~f:(fun manager ->
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
        Agent_session.History_codec.all_of_protocol state.conversation.canonical_history
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
          ~on_tool_call:(fun ~name:_ ~args:_ ->
            Ok (Chat_response.Moderation.Capabilities.Tool_error "invocation.unavailable"))
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
    drain.budget_exhausted
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
    || ((not halted) && Option.exists observer ~f:(pending_observation state)))
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
      let%bind applied = Agent_session.Session_actor.apply_moderator_follow_up t.actor in
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
            more_observations || more_events)))
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
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
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

let enqueue_model_job_completion t job =
  with_loaded_runtime t (fun runtime -> runtime.enqueue_model_job_completion job)
;;

let close t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.closed <- true;
    ignore
      (Agent_session.Session_actor.set_operation_worker t.actor None
       : (unit, Agent_protocol.Error.t) result);
    Option.iter t.runtime ~f:(fun runtime -> runtime.close ());
    t.runtime <- None)
;;
