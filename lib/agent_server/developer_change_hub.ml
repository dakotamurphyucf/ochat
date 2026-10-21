open! Core
module P = Agent_protocol
module Change = P.Developer_wire.Change
module Repl_change = P.Developer_repl_wire.Change

type topic =
  | Command of P.Id.Developer_run.t * P.Idempotency_key.t
  | Repl of
      Developer_repl_epoch.Instance.t
      * P.Developer_repl_wire.Wire_id.Instance.t
      * P.Idempotency_key.t
  | Task of P.Id.Developer_run.t * P.Idempotency_key.t

type change =
  | Command_change of Change.t
  | Repl_change of Repl_change.t

type hint_kind =
  | Command_hint of Change.kind
  | Repl_hint of Repl_change.kind

let error code message = P.Error.create code ~message ~retryable:false ()

module Limits = struct
  type t =
    { max_connections : int
    ; max_watches_per_connection : int
    ; max_pending_hints_per_connection : int
    }

  let create
        ~max_connections
        ~max_watches_per_connection
        ~max_pending_hints_per_connection
    =
    if
      max_connections <= 0
      || max_watches_per_connection <= 0
      || max_pending_hints_per_connection <= 0
      || max_connections > 4096
      || max_watches_per_connection > 4096
      || max_pending_hints_per_connection > 4096
      || max_watches_per_connection + max_pending_hints_per_connection
         > 1_000_000 / max_connections
    then Error (error Invalid_request "invalid developer watch limits")
    else
      Ok { max_connections; max_watches_per_connection; max_pending_hints_per_connection }
  ;;
end

type watch =
  { topic : topic
  ; mutable actor : Operator_authorization.t
  ; mutable authorize_change : change -> (unit, P.Error.t) result
  ; mutable generation : int
  ; mutable live : bool
  ; mutable dirty : bool
  ; mutable dirty_queued : bool
  ; mutable dirty_revision : int
  }

type hint =
  { watch : watch
  ; generation : int
  ; dirty_revision : int
  ; kind : hint_kind
  }

type outcome = (unit, exn * Stdlib.Printexc.raw_backtrace) result

type completion =
  { done_ : outcome Eio.Promise.t
  ; resolve : outcome Eio.Promise.u
  ; mutable retire : bool
  }

type phase =
  | Open
  | Restarting of completion
  | Retiring of completion
  | Closed

type connection =
  { physical : Connection_context.t
  ; lease : Connection_context.Authoring_negotiation.t
  ; mutable watches : watch list
  ; pending : hint Queue.t
  ; dirty : watch Queue.t
  ; wake : Eio.Condition.t
  ; mutable finished : outcome Eio.Promise.t
  ; mutable finish : outcome Eio.Promise.u
  ; mutable stop : (unit -> unit) option
  ; mutable publishing : watch option
  ; mutable phase : phase
  ; mutable prefer_dirty : bool
  }

type t =
  { sw : Eio.Switch.t
  ; limits : Limits.t
  ; wire_limits : P.Developer_common.Limits.t
  ; repl_wire_limits : P.Developer_repl_wire.Limits.t option
  ; domain : int
  ; mutable connections : connection list
  ; mutable closed : bool
  }

exception Stop

let pump_key : connection Eio.Fiber.key = Eio.Fiber.create_key ()
let same_domain t = Int.equal t.domain (Stdlib.Domain.self () :> int)

let create ~sw ~limits ~wire_limits ~repl_wire_limits =
  let open Result.Let_syntax in
  let%bind run_id = P.Id.Developer_run.of_string ("dvr_" ^ String.make 92 'x') in
  let%bind key = P.Idempotency_key.of_string (String.make 256 'x') in
  let%bind () =
    List.fold_result
      [ Change.Command_changed; Change.Approval_changed; Change.Resync_required ]
      ~init:()
      ~f:(fun () kind ->
        Change.create ~limits:wire_limits ~run_id ~key ~kind |> Result.map ~f:ignore)
  in
  let%map () =
    match repl_wire_limits with
    | None -> Ok ()
    | Some limits ->
      let%bind wire_instance =
        P.Developer_repl_wire.Wire_id.Instance.of_string ("ri_" ^ String.make 93 'x')
      in
      let keys =
        [ P.Developer_repl_wire.Watch_key.Repl
            (P.Developer_repl_wire.Lookup.create ~instance:wire_instance ~key)
        ; P.Developer_repl_wire.Watch_key.Task
            (P.Developer_common.Lookup.create ~run_id ~key)
        ]
      in
      List.fold_result keys ~init:() ~f:(fun () key ->
        let%bind _ = Repl_change.create ~limits ~key ~kind:Changed in
        Repl_change.create ~limits ~key ~kind:Resync_required |> Result.map ~f:ignore)
  in
  { sw
  ; limits
  ; wire_limits
  ; repl_wire_limits
  ; domain = (Stdlib.Domain.self () :> int)
  ; connections = []
  ; closed = false
  }
;;

let same_topic left right =
  match left, right with
  | Command (left_run, left_key), Command (right_run, right_key)
  | Task (left_run, left_key), Task (right_run, right_key) ->
    P.Id.Developer_run.equal left_run right_run
    && P.Idempotency_key.equal left_key right_key
  | Repl (left_instance, left_wire, left_key), Repl (right_instance, right_wire, right_key)
    ->
    Developer_repl_epoch.Instance.equal left_instance right_instance
    && P.Developer_repl_wire.Wire_id.Instance.equal left_wire right_wire
    && P.Idempotency_key.equal left_key right_key
  | Command _, (Repl _ | Task _)
  | Repl _, (Command _ | Task _)
  | Task _, (Command _ | Repl _) -> false
;;

let same_watch watch topic = same_topic watch.topic topic

let is_resync = function
  | Command_hint Change.Resync_required | Repl_hint Repl_change.Resync_required -> true
  | Command_hint (Change.Command_changed | Change.Approval_changed)
  | Repl_hint Repl_change.Changed -> false
;;

let find_connection t physical =
  List.find t.connections ~f:(fun entry -> phys_equal entry.physical physical)
;;

let current_lease entry =
  Connection_context.current_authoring_negotiation entry.physical entry.lease
;;

let watch_feature = function
  | Command _ -> "developer.events"
  | Repl _ | Task _ -> P.Developer_repl_wire.Features.events
;;

let selected_for_watch entry watch =
  Connection_context.Authoring_negotiation.selected
    entry.lease
    (watch_feature watch.topic)
;;

let actor_current entry actor =
  Operator_authorization.is_current actor
  && Result.is_ok (Connection_context.request_actor entry.physical (Some actor))
;;

let open_entry entry =
  match entry.phase with
  | Open -> true
  | Restarting _ | Retiring _ | Closed -> false
;;

let live_hint (t : t) (entry : connection) watch ~generation =
  (not t.closed)
  && open_entry entry
  && watch.live
  && Int.equal watch.generation generation
  && current_lease entry
  && selected_for_watch entry watch
  && actor_current entry watch.actor
;;

let discard_watch_hints entry watch =
  let pending =
    Queue.to_list entry.pending
    |> List.filter ~f:(fun hint -> not (phys_equal hint.watch watch))
  in
  Queue.clear entry.pending;
  List.iter pending ~f:(Queue.enqueue entry.pending);
  let dirty =
    Queue.to_list entry.dirty
    |> List.filter ~f:(fun candidate -> not (phys_equal candidate watch))
  in
  Queue.clear entry.dirty;
  List.iter dirty ~f:(Queue.enqueue entry.dirty);
  watch.dirty_queued <- false;
  watch.dirty <- false;
  watch.dirty_revision <- watch.dirty_revision + 1
;;

let revoke_watch entry watch =
  watch.live <- false;
  watch.generation <- watch.generation + 1;
  entry.watches
  <- List.filter entry.watches ~f:(fun candidate -> not (phys_equal candidate watch));
  discard_watch_hints entry watch
;;

let mark_dirty (entry : connection) (watch : watch) =
  watch.dirty <- true;
  watch.dirty_revision <- watch.dirty_revision + 1;
  if not watch.dirty_queued
  then (
    watch.dirty_queued <- true;
    Queue.enqueue entry.dirty watch)
;;

let enqueue_matching t ~matches ~kind =
  if same_domain t && not t.closed
  then
    List.iter t.connections ~f:(fun entry ->
      if open_entry entry
      then
        List.iter entry.watches ~f:(fun watch ->
          if watch.live && matches watch.topic
          then (
            if
              watch.dirty
              || Queue.length entry.pending >= t.limits.max_pending_hints_per_connection
            then mark_dirty entry watch
            else
              Queue.enqueue
                entry.pending
                { watch
                ; generation = watch.generation
                ; dirty_revision = watch.dirty_revision
                ; kind
                };
            Eio.Condition.broadcast entry.wake)))
;;

let enqueue t ~run_id ~key ~kind =
  enqueue_matching
    t
    ~matches:(function
      | Command (watched_run, watched_key) ->
        P.Id.Developer_run.equal run_id watched_run
        && P.Idempotency_key.equal key watched_key
      | Repl _ | Task _ -> false)
    ~kind:(Command_hint kind)
;;

let enqueue_repl_submission t ~instance ~key =
  enqueue_matching
    t
    ~matches:(function
      | Repl (watched_instance, _, watched_key) ->
        Developer_repl_epoch.Instance.equal instance watched_instance
        && P.Idempotency_key.equal key watched_key
      | Command _ | Task _ -> false)
    ~kind:(Repl_hint Repl_change.Changed)
;;

let invalidate_repl_instance t ~instance =
  if same_domain t && not t.closed
  then
    List.iter t.connections ~f:(fun entry ->
      if open_entry entry
      then
        List.iter entry.watches ~f:(fun watch ->
          match watch.topic with
          | Repl (watched, _, _)
            when watch.live && Developer_repl_epoch.Instance.equal instance watched ->
            mark_dirty entry watch;
            Eio.Condition.broadcast entry.wake
          | Command _ | Repl _ | Task _ -> ()))
;;

let enqueue_task t ~run_id ~key =
  enqueue_matching
    t
    ~matches:(function
      | Task (watched_run, watched_key) ->
        P.Id.Developer_run.equal run_id watched_run
        && P.Idempotency_key.equal key watched_key
      | Command _ | Repl _ -> false)
    ~kind:(Repl_hint Repl_change.Changed)
;;

let resync_kind watch =
  match watch.topic with
  | Command _ -> Command_hint Change.Resync_required
  | Repl _ | Task _ -> Repl_hint Repl_change.Resync_required
;;

let rec next_hint entry =
  if not (open_entry entry)
  then None
  else (
    let choose_dirty () =
      match Queue.dequeue entry.dirty with
      | None -> None
      | Some watch ->
        watch.dirty_queued <- false;
        Some
          { watch
          ; generation = watch.generation
          ; dirty_revision = watch.dirty_revision
          ; kind = resync_kind watch
          }
    in
    let chosen =
      if entry.prefer_dirty
      then (
        match choose_dirty () with
        | Some hint -> Some hint
        | None -> Queue.dequeue entry.pending)
      else (
        match Queue.dequeue entry.pending with
        | Some hint -> Some hint
        | None -> choose_dirty ())
    in
    match chosen with
    | Some hint ->
      entry.prefer_dirty <- not entry.prefer_dirty;
      if hint.watch.live then Some hint else next_hint entry
    | None ->
      Eio.Condition.await_no_mutex entry.wake;
      next_hint entry)
;;

let bounded_change t watch kind =
  match watch.topic, kind with
  | Command (run_id, key), Command_hint kind ->
    Change.create ~limits:t.wire_limits ~run_id ~key ~kind
    |> Result.map ~f:(fun change ->
      Command_change change, Change.method_name, Change.to_json change)
  | Repl (_, wire_instance, key), Repl_hint kind ->
    (match t.repl_wire_limits with
     | None -> Error (error Invalid_state "REPL wire is not installed")
     | Some limits ->
       let key =
         P.Developer_repl_wire.Watch_key.Repl
           (P.Developer_repl_wire.Lookup.create ~instance:wire_instance ~key)
       in
       Repl_change.create ~limits ~key ~kind
       |> Result.map ~f:(fun change ->
         Repl_change change, Repl_change.method_name, Repl_change.to_json change))
  | Task (run_id, key), Repl_hint kind ->
    (match t.repl_wire_limits with
     | None -> Error (error Invalid_state "REPL wire is not installed")
     | Some limits ->
       let key =
         P.Developer_repl_wire.Watch_key.Task
           (P.Developer_common.Lookup.create ~run_id ~key)
       in
       Repl_change.create ~limits ~key ~kind
       |> Result.map ~f:(fun change ->
         Repl_change change, Repl_change.method_name, Repl_change.to_json change))
  | Command _, Repl_hint _ | (Repl _ | Task _), Command_hint _ ->
    Error (error Invalid_state "watch hint family changed")
;;

let rec pump t entry =
  match next_hint entry with
  | None -> ()
  | Some ({ watch; generation; dirty_revision; kind } as hint) ->
    entry.publishing <- Some watch;
    Exn.protect
      ~finally:(fun () -> entry.publishing <- None)
      ~f:(fun () ->
        if
          live_hint t entry watch ~generation
          && ((not watch.dirty) || is_resync kind)
          && (is_resync kind || Int.equal dirty_revision watch.dirty_revision)
        then (
          match bounded_change t watch hint.kind with
          | Error _ ->
            if watch.live && Int.equal watch.generation generation
            then revoke_watch entry watch
          | Ok (change, method_, params) ->
            (match watch.authorize_change change with
             | Error _ ->
               if watch.live && Int.equal watch.generation generation
               then revoke_watch entry watch
             | Ok () ->
               if live_hint t entry watch ~generation
               then (
                 Connection_context.publish_notification
                   entry.physical
                   (P.Envelope.notification ~method_ ~params ());
                 if is_resync kind && Int.equal dirty_revision watch.dirty_revision
                 then watch.dirty <- false))));
    pump t entry
;;

let remove_entry t entry =
  t.connections
  <- List.filter t.connections ~f:(fun current -> not (phys_equal current entry))
;;

let start_pump t entry =
  Eio.Switch.check t.sw;
  Eio.Fiber.fork ~sw:t.sw (fun () ->
    let outcome =
      try
        Eio.Cancel.sub (fun cancel ->
          entry.stop <- Some (fun () -> Eio.Cancel.cancel cancel Stop);
          Eio.Fiber.with_binding pump_key entry (fun () -> pump t entry));
        Ok ()
      with
      | Eio.Cancel.Cancelled Stop when not (open_entry entry) -> Ok ()
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    Eio.Cancel.protect (fun () ->
      (match outcome with
       | Ok () -> ()
       | Error (exn, backtrace) ->
         entry.phase <- Closed;
         List.iter entry.watches ~f:(fun watch ->
           watch.live <- false;
           watch.generation <- watch.generation + 1);
         entry.watches <- [];
         Queue.clear entry.pending;
         Queue.clear entry.dirty;
         Connection_context.record_developer_event_failure entry.physical exn backtrace;
         Eio.Condition.broadcast entry.wake);
      (* [finished] owns the join only. Unexpected publication failure has one
         exception custodian on the physical connection, so concurrent closes
         cannot each rethrow the same pump outcome. *)
      Eio.Promise.resolve entry.finish (Ok ());
      match outcome with
      | Ok () -> ()
      | Error _ -> remove_entry t entry))
;;

let self_pump entry =
  Option.value_map (Eio.Fiber.get pump_key) ~default:false ~f:(fun current ->
    phys_equal current entry)
;;

let new_completion () =
  let done_, resolve = Eio.Promise.create () in
  { done_; resolve; retire = false }
;;

let revoke_entry entry =
  List.iter entry.watches ~f:(fun watch ->
    watch.live <- false;
    watch.generation <- watch.generation + 1);
  entry.watches <- [];
  Queue.clear entry.pending;
  Queue.clear entry.dirty
;;

let stop_pump entry =
  Eio.Condition.broadcast entry.wake;
  Option.iter entry.stop ~f:(fun stop -> stop ())
;;

let await_outcome promise =
  match Eio.Promise.await promise with
  | Ok () -> Ok ()
  | Error (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let retire_entry t entry =
  if self_pump entry
  then Error (error Invalid_state "watch pump cannot join itself")
  else
    Eio.Cancel.protect (fun () ->
      match entry.phase with
      | Closed ->
        remove_entry t entry;
        await_outcome entry.finished
      | Retiring completion -> await_outcome completion.done_
      | Restarting completion ->
        completion.retire <- true;
        await_outcome completion.done_
      | Open ->
        let completion = new_completion () in
        entry.phase <- Retiring completion;
        revoke_entry entry;
        stop_pump entry;
        let outcome = Eio.Promise.await entry.finished in
        entry.phase <- Closed;
        remove_entry t entry;
        Eio.Promise.resolve completion.resolve outcome;
        (match outcome with
         | Ok () -> Ok ()
         | Error (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace))
;;

let restart_pump t entry =
  if self_pump entry
  then Error (error Invalid_state "watch pump cannot restart itself")
  else
    Eio.Cancel.protect (fun () ->
      match entry.phase with
      | Closed -> Error (error Server_shutting_down "watch pump has stopped")
      | Retiring completion | Restarting completion -> await_outcome completion.done_
      | Open ->
        let completion = new_completion () in
        entry.phase <- Restarting completion;
        (* No transition hint may disappear while this pump is joined. One
           bounded resync obligation per surviving key covers the interval. *)
        List.iter entry.watches ~f:(mark_dirty entry);
        stop_pump entry;
        let prior = Eio.Promise.await entry.finished in
        let outcome =
          match prior with
          | Error _ as error -> error
          | Ok ()
            when completion.retire
                 || t.closed
                 || (match entry.phase with
                     | Closed -> true
                     | Open | Restarting _ | Retiring _ -> false)
                 || not (current_lease entry) -> Ok ()
          | Ok () ->
            let finished, finish = Eio.Promise.create () in
            entry.finished <- finished;
            entry.finish <- finish;
            entry.stop <- None;
            entry.phase <- Open;
            (try
               start_pump t entry;
               Ok ()
             with
             | exn ->
               let backtrace = Stdlib.Printexc.get_raw_backtrace () in
               Eio.Promise.resolve entry.finish (Error (exn, backtrace));
               Error (exn, backtrace))
        in
        let outcome =
          match outcome, entry.phase with
          | Ok (), Closed -> Eio.Promise.await entry.finished
          | (Ok () | Error _), (Open | Restarting _ | Retiring _) -> outcome
          | Error _, Closed -> outcome
        in
        (match outcome with
         | Ok () when open_entry entry -> ()
         | Ok ()
           when match entry.phase with
                | Retiring _ -> true
                | Open | Restarting _ | Closed -> false ->
           (* A concurrent retirement owns the new pump's join. *)
           ()
         | Ok () | Error _ ->
           entry.phase <- Closed;
           revoke_entry entry;
           remove_entry t entry);
        Eio.Promise.resolve completion.resolve outcome;
        (match outcome with
         | Ok () -> Ok ()
         | Error (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace))
;;

let close_connection_and_wait t physical =
  if not (same_domain t)
  then Error (error Invalid_state "watch hub has a foreign domain")
  else (
    let entry = find_connection t physical in
    match entry with
    | Some entry when self_pump entry ->
      Error (error Invalid_state "watch pump cannot join itself")
    | None | Some _ ->
      Eio.Cancel.protect (fun () ->
        (* Terminal connection close revokes the physical feature lease before
         any yielding join, even when no watch entry was registered. *)
        Connection_context.close_authoring physical;
        let joined =
          try
            Ok
              (match entry with
               | None -> Ok ()
               | Some entry -> retire_entry t entry)
          with
          | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
        in
        match Connection_context.take_developer_event_failure physical with
        | Some (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
        | None ->
          (match joined with
           | Ok result -> result
           | Error (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace)))
;;

let close_and_wait t =
  if not (same_domain t)
  then Error (error Invalid_state "watch hub has a foreign domain")
  else if List.exists t.connections ~f:self_pump
  then Error (error Invalid_state "watch pump cannot join itself")
  else
    Eio.Cancel.protect (fun () ->
      t.closed <- true;
      let entries = t.connections in
      List.iter entries ~f:(fun entry ->
        Connection_context.close_authoring entry.physical;
        match entry.phase with
        | Restarting completion -> completion.retire <- true
        | Open | Retiring _ | Closed -> ());
      let first_failure = ref None in
      List.iter entries ~f:(fun entry ->
        try
          match retire_entry t entry with
          | Ok () -> ()
          | Error error ->
            if Option.is_none !first_failure then first_failure := Some (`Error error)
        with
        | exn ->
          if Option.is_none !first_failure
          then
            first_failure := Some (`Exception (exn, Stdlib.Printexc.get_raw_backtrace ())));
      match !first_failure with
      | None -> Ok ()
      | Some (`Error error) -> Error error
      | Some (`Exception (exn, backtrace)) ->
        Stdlib.Printexc.raise_with_backtrace exn backtrace)
;;

let rec watch_topic t ~connection ~actor ~topic ~authorize_key ~authorize_change =
  let open Result.Let_syntax in
  if t.closed || not (same_domain t)
  then Error (error Server_shutting_down "watch hub is unavailable")
  else (
    let%bind lease =
      Connection_context.authoring_negotiation connection
      |> Result.of_option
           ~error:(error Permission_denied "developer events are not negotiated")
    in
    if
      (not
         (Connection_context.Authoring_negotiation.selected lease (watch_feature topic)))
      || (not (Operator_authorization.is_current actor))
      || Result.is_error (Connection_context.request_actor connection (Some actor))
    then Error (error Permission_denied "developer watch is not current")
    else (
      let%bind () = authorize_key () in
      if
        t.closed
        || (not (Connection_context.current_authoring_negotiation connection lease))
        || (not (Operator_authorization.is_current actor))
        || Result.is_error (Connection_context.request_actor connection (Some actor))
      then Error (error Permission_denied "developer watch changed during admission")
      else (
        match find_connection t connection with
        | Some { phase = Restarting completion; _ } ->
          let%bind () = await_outcome completion.done_ in
          watch_topic t ~connection ~actor ~topic ~authorize_key ~authorize_change
        | Some { phase = Retiring completion; _ } ->
          let%bind () = await_outcome completion.done_ in
          watch_topic t ~connection ~actor ~topic ~authorize_key ~authorize_change
        | Some entry when (not (open_entry entry)) || not (current_lease entry) ->
          let%bind () = retire_entry t entry in
          watch_topic t ~connection ~actor ~topic ~authorize_key ~authorize_change
        | Some entry ->
          if
            t.closed
            || (not (Connection_context.current_authoring_negotiation connection lease))
            || (not (Operator_authorization.is_current actor))
            || Result.is_error (Connection_context.request_actor connection (Some actor))
          then Error (error Permission_denied "developer watch changed during admission")
          else (
            match List.find entry.watches ~f:(fun watch -> same_watch watch topic) with
            | Some existing ->
              existing.generation <- existing.generation + 1;
              (* The mandatory Observe after this acknowledgement covers all
                 pre-refresh transitions. Retire their hints without creating
                 a new resync; post-refresh transitions retain their own
                 generation and dirty revision, even if an old publisher yields. *)
              discard_watch_hints entry existing;
              existing.actor <- actor;
              existing.authorize_change <- authorize_change;
              Ok ()
            | None ->
              if List.length entry.watches >= t.limits.max_watches_per_connection
              then Error (error Resource_limit "developer watch limit")
              else (
                let watch =
                  { topic
                  ; actor
                  ; authorize_change
                  ; generation = 0
                  ; live = true
                  ; dirty = false
                  ; dirty_queued = false
                  ; dirty_revision = 0
                  }
                in
                entry.watches <- watch :: entry.watches;
                Ok ()))
        | None ->
          if
            t.closed
            || (not (Connection_context.current_authoring_negotiation connection lease))
            || (not (Operator_authorization.is_current actor))
            || Result.is_error (Connection_context.request_actor connection (Some actor))
          then Error (error Permission_denied "developer watch changed during admission")
          else if List.length t.connections >= t.limits.max_connections
          then Error (error Resource_limit "developer connection watch limit")
          else (
            let finished, finish = Eio.Promise.create () in
            let entry =
              { physical = connection
              ; lease
              ; watches =
                  [ { topic
                    ; actor
                    ; authorize_change
                    ; generation = 0
                    ; live = true
                    ; dirty = false
                    ; dirty_queued = false
                    ; dirty_revision = 0
                    }
                  ]
              ; pending = Queue.create ()
              ; dirty = Queue.create ()
              ; wake = Eio.Condition.create ()
              ; finished
              ; finish
              ; stop = None
              ; publishing = None
              ; phase = Open
              ; prefer_dirty = true
              }
            in
            t.connections <- entry :: t.connections;
            (try start_pump t entry with
             | exn ->
               let backtrace = Stdlib.Printexc.get_raw_backtrace () in
               entry.phase <- Closed;
               revoke_entry entry;
               remove_entry t entry;
               Eio.Promise.resolve entry.finish (Error (exn, backtrace));
               Stdlib.Printexc.raise_with_backtrace exn backtrace);
            if not (open_entry entry)
            then (
              match Eio.Promise.await entry.finished with
              | Ok () ->
                Error (error Server_shutting_down "watch pump stopped during admission")
              | Error (exn, backtrace) ->
                Stdlib.Printexc.raise_with_backtrace exn backtrace)
            else Ok ()))))
;;

let rec unwatch_topic t ~connection ~matches =
  if t.closed || not (same_domain t)
  then Error (error Server_shutting_down "watch hub is unavailable")
  else (
    match find_connection t connection with
    | None -> Ok ()
    | Some entry ->
      if self_pump entry
      then Error (error Invalid_state "watch pump cannot unwatch itself")
      else (
        match entry.phase with
        | Restarting completion | Retiring completion ->
          Result.bind (await_outcome completion.done_) ~f:(fun () ->
            unwatch_topic t ~connection ~matches)
        | Closed -> retire_entry t entry
        | Open ->
          (match List.find entry.watches ~f:(fun watch -> matches watch.topic) with
           | None -> Ok ()
           | Some watch ->
             let in_flight =
               Option.value_map entry.publishing ~default:false ~f:(fun current ->
                 phys_equal current watch)
             in
             revoke_watch entry watch;
             if List.is_empty entry.watches
             then retire_entry t entry
             else if in_flight
             then restart_pump t entry
             else Ok ())))
;;

let watch t ~connection ~actor ~run_id ~key ~authorize_key ~authorize_change =
  watch_topic
    t
    ~connection
    ~actor
    ~topic:(Command (run_id, key))
    ~authorize_key
    ~authorize_change:(function
      | Command_change change -> authorize_change change
      | Repl_change _ -> Error (error Invalid_state "watch family changed"))
;;

let watch_repl
      t
      ~connection
      ~actor
      ~instance
      ~wire_instance
      ~key
      ~authorize_key
      ~authorize_change
  =
  watch_topic
    t
    ~connection
    ~actor
    ~topic:(Repl (instance, wire_instance, key))
    ~authorize_key
    ~authorize_change:(function
      | Repl_change change -> authorize_change change
      | Command_change _ -> Error (error Invalid_state "watch family changed"))
;;

let watch_task t ~connection ~actor ~run_id ~key ~authorize_key ~authorize_change =
  watch_topic
    t
    ~connection
    ~actor
    ~topic:(Task (run_id, key))
    ~authorize_key
    ~authorize_change:(function
      | Repl_change change -> authorize_change change
      | Command_change _ -> Error (error Invalid_state "watch family changed"))
;;

let unwatch t ~connection ~run_id ~key =
  unwatch_topic t ~connection ~matches:(function
    | Command (watched_run, watched_key) ->
      P.Id.Developer_run.equal run_id watched_run
      && P.Idempotency_key.equal key watched_key
    | Repl _ | Task _ -> false)
;;

let unwatch_repl t ~connection ~wire_instance ~key =
  unwatch_topic t ~connection ~matches:(function
    | Repl (_, watched_wire, watched_key) ->
      P.Developer_repl_wire.Wire_id.Instance.equal wire_instance watched_wire
      && P.Idempotency_key.equal key watched_key
    | Command _ | Task _ -> false)
;;

let unwatch_task t ~connection ~run_id ~key =
  unwatch_topic t ~connection ~matches:(function
    | Task (watched_run, watched_key) ->
      P.Id.Developer_run.equal run_id watched_run
      && P.Idempotency_key.equal key watched_key
    | Command _ | Repl _ -> false)
;;
