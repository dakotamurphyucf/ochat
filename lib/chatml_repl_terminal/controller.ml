open! Core
module P = Agent_protocol
module W = P.Developer_repl_wire
module Client = Agent_client.Developer_repl_connection

module Input = struct
  type t =
    | Text of string
    | Paste of string
    | Enter
    | Backspace
    | Delete
    | Move of State.Editor.motion
    | History_previous
    | History_next
    | Submit
    | Cancel
    | Resize
    | Quit
  [@@deriving sexp_of]
end

module Command = struct
  type t =
    | Submit
    | Run of int
    | Type of
        { statement_index : int
        ; selector : W.Type_selector.t
        }
    | References
    | Observe
    | Cancel
    | Reset
    | Close
    | Retry
    | Abandon
    | Quit
    | Help
  [@@deriving sexp_of]

  let positive text =
    match Int.of_string_opt text with
    | Some value when value > 0 -> Some value
    | None | Some _ -> None
  ;;

  let parse line =
    let words =
      String.strip line
      |> String.split ~on:' '
      |> List.filter ~f:(fun word -> not (String.is_empty word))
    in
    match words with
    | [ ":submit" ] -> Some Submit
    | [ ":run"; index ] -> Option.map (positive index) ~f:(fun index -> Run index)
    | [ ":type"; statement_index ] ->
      Option.map (positive statement_index) ~f:(fun statement_index ->
        Type { statement_index = statement_index - 1; selector = Expression })
    | [ ":type"; statement_index; "binding"; index ] ->
      Option.both (positive statement_index) (positive index)
      |> Option.map ~f:(fun (statement_index, index) ->
        Type
          { statement_index = statement_index - 1; selector = Binding_index (index - 1) })
    | [ ":refs" ] -> Some References
    | [ ":observe" ] -> Some Observe
    | [ ":cancel" ] -> Some Cancel
    | [ ":reset" ] -> Some Reset
    | [ ":close" ] -> Some Close
    | [ ":retry" ] -> Some Retry
    | [ ":abandon" ] -> Some Abandon
    | [ ":quit" ] -> Some Quit
    | [ ":help" ] -> Some Help
    | _ -> None
  ;;
end

type completion =
  | Created of (W.View.t, P.Error.t) result
  | Submitted of
      { source : string
      ; key : P.Idempotency_key.t
      ; retried : bool
      ; result : (W.Submission.t, P.Error.t) result
      }
  | Task_started of
      { key : P.Idempotency_key.t
      ; result : (W.Task_view.t, P.Error.t) result
      }
  | Reset_done of (W.View.t, P.Error.t) result
  | Close_done of (unit, P.Error.t) result
  | Type_done of
      { instance : W.Wire_id.Instance.t
      ; epoch : W.Wire_id.Epoch.t
      ; result : (W.Type_inspection.t, P.Error.t) result
      }
  | References_done of
      { instance : W.Wire_id.Instance.t
      ; epoch : W.Wire_id.Epoch.t
      ; revision : W.Wire_id.Revision.t
      ; result : (W.Reference_page.t, P.Error.t) result
      }
  | Submission_observed of (W.Submission.t option, P.Error.t) result
  | Task_observed of (W.Task_view.t option, P.Error.t) result
  | Submission_cancelled of (W.Submission.t, P.Error.t) result
  | Task_cancelled of (W.Task_view.t, P.Error.t) result

type control_completion =
  | Submission_control of
      { lookup : W.Lookup.t
      ; result : (W.Submission.t, P.Error.t) result
      }
  | Task_control of
      { lookup : P.Developer_common.Lookup.t
      ; result : (W.Task_view.t, P.Error.t) result
      }

type revision_knowledge =
  | Known
  | Unknown of W.Watch_key.t option

let revision_unknown = function
  | Known -> false
  | Unknown _ -> true
;;

type event =
  | Input of Input.t
  | Completed of completion
  | Control_completed of control_completion
  | Notification
  | Notification_error of P.Error.t
  | Input_overflow
  | Input_closed

module Input_queue = struct
  type t =
    { wake : unit Eio.Stream.t
    ; inputs : Input.t Queue.t
    ; capacity : int
    ; mutable wake_pending : bool
    ; mutable overflow : bool
    ; mutable failure : string option
    ; mutable closed : bool
    ; mutable notification_pending : bool
    ; mutable notification_error : P.Error.t option
    ; mutable completed : completion option
    ; mutable control_completed : control_completion option
    }

  let create ~capacity =
    if capacity < 1 || capacity > 4_096
    then Error (P.Error.invalid_request "invalid terminal input queue capacity")
    else
      Ok
        { wake = Eio.Stream.create 1
        ; inputs = Queue.create ()
        ; capacity
        ; wake_pending = false
        ; overflow = false
        ; failure = None
        ; closed = false
        ; notification_pending = false
        ; notification_error = None
        ; completed = None
        ; control_completed = None
        }
  ;;

  (* Every producer runs on the terminal's Eio domain. Only one wake can be
     outstanding, so this capacity-one add has room and never waits. *)
  let signal t =
    if not t.wake_pending
    then (
      t.wake_pending <- true;
      Eio.Stream.add t.wake ())
  ;;

  let offer t input =
    if t.closed || t.overflow
    then ()
    else if Queue.length t.inputs >= t.capacity
    then (
      t.overflow <- true;
      signal t)
    else (
      Queue.enqueue t.inputs input;
      signal t)
  ;;

  let fail t ~message =
    if (not t.closed) && Option.is_none t.failure
    then (
      t.failure <- Some (String.prefix message 256);
      signal t)
  ;;

  let close t =
    if not t.closed
    then (
      t.closed <- true;
      signal t)
  ;;

  let post t event =
    (match event with
     | Completed completion -> t.completed <- Some completion
     | Control_completed completion -> t.control_completed <- Some completion
     | Notification_error error ->
       if Option.is_none t.notification_error then t.notification_error <- Some error
     | Input _ | Notification | Input_overflow | Input_closed ->
       raise_s [%sexp "invalid internal terminal queue event"]);
    signal t
  ;;

  let notify t =
    t.notification_pending <- true;
    signal t
  ;;

  let rec take t =
    if Option.is_some t.failure
    then Error (P.Error.invalid_request (Option.value_exn t.failure))
    else if t.overflow
    then Error (P.Error.invalid_request "terminal input queue overflowed")
    else if t.closed
    then Ok Input_closed
    else (
      match t.control_completed with
      | Some completed ->
        t.control_completed <- None;
        Ok (Control_completed completed)
      | None ->
        (match t.completed with
         | Some completed ->
           t.completed <- None;
           Ok (Completed completed)
         | None ->
           (match t.notification_error with
            | Some error ->
              t.notification_error <- None;
              Ok (Notification_error error)
            | None ->
              if t.notification_pending
              then (
                t.notification_pending <- false;
                Ok Notification)
              else (
                match Queue.dequeue t.inputs with
                | Some input -> Ok (Input input)
                | None ->
                  Eio.Stream.take t.wake;
                  t.wake_pending <- false;
                  take t))))
  ;;
end

module Transition = struct
  let activity ~current ~revision_unknown ~pending =
    match pending with
    | Some activity -> activity
    | None ->
      let open State in
      (match current with
       | Closed -> Closed
       | Ready | Busy _ | Uncertain _ | Needs_reset ->
         if revision_unknown then Needs_reset else Ready)
  ;;

  let task_active = function
    | W.Task_view.Terminal _ -> false
    | Reserved | Waiting_permission | Execution_started -> true
  ;;
end

let key () =
  P.Id.Attachment.create () |> P.Id.Attachment.to_string |> P.Idempotency_key.of_string
;;

let error_message error = P.Error.code_to_string error.P.Error.code ^ ": " ^ error.message

let run_on_connection ~client ~connection ~create ~initial ~input ~present =
  let open Result.Let_syntax in
  let%bind lease = Agent_client.Connection.claim_notifications connection in
  Exn.protect
    ~finally:(fun () -> Agent_client.Connection.release_notifications lease)
    ~f:(fun () ->
      Eio.Switch.run (fun sw ->
        let state = ref initial in
        let worker_active = ref false in
        let control_active = ref false in
        let notification_dirty = ref false in
        let latest_submission = ref None in
        let latest_task = ref None in
        let active_watch : W.Watch_key.t option ref = ref None in
        let pending_submit_source = ref None in
        let revision_knowledge = ref Known in
        let inflight_submit_lookup : W.Lookup.t option ref = ref None in
        let inflight_task_lookup : P.Developer_common.Lookup.t option ref = ref None in
        let show next =
          state := next;
          present next
        in
        let notice message = show (State.with_notice !state message) in
        let pending () = State.with_pending !state (Client.pending client) in
        let busy operation run_worker =
          if !worker_active || !control_active
          then (
            notice "A request or exact-key cancellation is already in progress";
            false)
          else (
            worker_active := true;
            show (State.with_activity !state (Busy operation));
            Eio.Fiber.fork_daemon ~sw (fun () ->
              let completed = run_worker () in
              Input_queue.post input (Completed completed);
              `Stop_daemon);
            true)
        in
        let current_watch ~include_abandoned =
          match include_abandoned, !revision_knowledge with
          | true, Unknown abandoned -> abandoned
          | _ ->
            (match
               ( !inflight_task_lookup
               , !inflight_submit_lookup
               , !latest_task
               , !latest_submission )
             with
             | Some lookup, _, _, _ -> Some (W.Watch_key.Task lookup)
             | None, Some lookup, _, _ -> Some (W.Watch_key.Repl lookup)
             | None, None, Some lookup, _ -> Some (W.Watch_key.Task lookup)
             | None, None, None, Some lookup -> Some (W.Watch_key.Repl lookup)
             | None, None, None, None -> None)
        in
        let observe_current ?(background = false) ?(refresh = false) () =
          if State.equal_activity (State.activity !state) Closed
          then false
          else if !worker_active || !control_active
          then (
            notification_dirty := true;
            true)
          else (
            match current_watch ~include_abandoned:true with
            | Some (W.Watch_key.Task lookup) ->
              busy Observe (fun () ->
                let old = !active_watch in
                let next = W.Watch_key.Task lookup in
                let result =
                  match old with
                  | Some (W.Watch_key.Task old_lookup)
                    when P.Id.Developer_run.equal old_lookup.run_id lookup.run_id
                         && P.Idempotency_key.equal old_lookup.key lookup.key ->
                    if refresh
                    then Client.watch_and_observe_task client lookup
                    else Client.task_observe client lookup
                  | Some old ->
                    Result.bind (Client.unwatch client old) ~f:(fun () ->
                      active_watch := None;
                      Client.watch_and_observe_task client lookup)
                  | None -> Client.watch_and_observe_task client lookup
                in
                Task_observed
                  (Result.map result ~f:(fun observation ->
                     active_watch := Some next;
                     observation)))
            | Some (W.Watch_key.Repl lookup) ->
              busy Observe (fun () ->
                let old = !active_watch in
                let next = W.Watch_key.Repl lookup in
                let result =
                  match old with
                  | Some (W.Watch_key.Repl old_lookup)
                    when W.Wire_id.Instance.equal
                           (W.Lookup.instance old_lookup)
                           (W.Lookup.instance lookup)
                         && P.Idempotency_key.equal
                              (W.Lookup.key old_lookup)
                              (W.Lookup.key lookup) ->
                    if refresh
                    then Client.watch_and_observe_repl client lookup
                    else Client.observe client lookup
                  | Some old ->
                    Result.bind (Client.unwatch client old) ~f:(fun () ->
                      active_watch := None;
                      Client.watch_and_observe_repl client lookup)
                  | None -> Client.watch_and_observe_repl client lookup
                in
                Submission_observed
                  (Result.map result ~f:(fun observation ->
                     active_watch := Some next;
                     observation)))
            | None ->
              if not background then notice "No exact submission or Task key to observe";
              false)
        in
        let after_completion () =
          if !notification_dirty
          then (
            notification_dirty := false;
            ignore (observe_current ~background:true () : bool))
        in
        let pending_activity pending =
          match
            ( pending.Client.creates
            , pending.submits
            , pending.tasks
            , pending.resets
            , pending.closes )
          with
          | _ :: _, _, _, _, _ -> Some State.(Uncertain Create)
          | [], _ :: _, _, _, _ -> Some (Uncertain Submit)
          | [], [], _ :: _, _, _ -> Some (Uncertain Run_task)
          | [], [], [], _ :: _, _ -> Some (Uncertain Reset)
          | [], [], [], [], _ :: _ -> Some (Uncertain Close)
          | [], [], [], [], [] -> None
        in
        let failure (_operation : State.keyed_operation) error =
          let client_pending = Client.pending client in
          let activity =
            Transition.activity
              ~current:(State.activity !state)
              ~revision_unknown:(revision_unknown !revision_knowledge)
              ~pending:(pending_activity client_pending)
          in
          show
            (State.with_pending !state client_pending
             |> fun state ->
             State.with_activity state activity
             |> fun state -> State.with_notice state (error_message error))
        in
        let ready snapshot =
          let pending = Client.pending client in
          let activity =
            Transition.activity
              ~current:(State.activity !state)
              ~revision_unknown:(revision_unknown !revision_knowledge)
              ~pending:(pending_activity pending)
          in
          show (State.with_activity (State.with_pending snapshot pending) activity)
        in
        let current_epoch_matches instance epoch =
          match State.view !state with
          | None -> false
          | Some view ->
            W.Wire_id.Instance.equal (W.View.instance view) instance
            && W.Wire_id.Epoch.equal (W.View.epoch view) epoch
        in
        let active_task () =
          Option.is_some !latest_task
          && Option.value_map (State.task !state) ~default:false ~f:(fun task ->
            Transition.task_active (W.Task_view.phase task))
        in
        let same_submission_lookup a b =
          W.Wire_id.Instance.equal (W.Lookup.instance a) (W.Lookup.instance b)
          && P.Idempotency_key.equal (W.Lookup.key a) (W.Lookup.key b)
        in
        let same_task_lookup
              (a : P.Developer_common.Lookup.t)
              (b : P.Developer_common.Lookup.t)
          =
          P.Id.Developer_run.equal a.run_id b.run_id
          && P.Idempotency_key.equal a.key b.key
        in
        let control_is_current = function
          | Submission_control { lookup; result } ->
            Option.value_map
              (Option.first_some !inflight_submit_lookup !latest_submission)
              ~default:false
              ~f:(same_submission_lookup lookup)
            && Option.value_map (State.view !state) ~default:false ~f:(fun view ->
              W.Wire_id.Instance.equal (W.Lookup.instance lookup) (W.View.instance view)
              &&
              match result with
              | Error _ -> true
              | Ok submission ->
                W.Wire_id.Epoch.equal (W.Submission.epoch submission) (W.View.epoch view))
          | Task_control { lookup; result } ->
            Option.value_map
              (Option.first_some !inflight_task_lookup !latest_task)
              ~default:false
              ~f:(same_task_lookup lookup)
            && Option.value_map (State.view !state) ~default:false ~f:(fun view ->
              P.Id.Developer_run.equal lookup.run_id (W.View.run_id view)
              &&
              match result with
              | Error _ -> true
              | Ok task ->
                P.Idempotency_key.equal (W.Task_view.key task) lookup.key
                && P.Id.Developer_run.equal (W.Task_view.owner task).run_id lookup.run_id)
        in
        let control_finished completion =
          control_active := false;
          if not (control_is_current completion)
          then notice "Stale cancellation response discarded"
          else (
            match completion with
            | Submission_control { result = Ok _; _ } | Task_control { result = Ok _; _ }
              ->
              notification_dirty := true;
              notice "Cancellation answered; reobserving the exact key"
            | Submission_control { result = Error error; _ }
            | Task_control { result = Error error; _ } ->
              notice ("Cancellation unresolved: " ^ error_message error))
        in
        let finish = function
          | Created (Ok view) ->
            revision_knowledge := Known;
            ready (State.with_view !state view)
          | Created (Error error) -> failure Create error
          | Submitted { source; key; retried; result = Ok submission } ->
            (match State.with_submission !state submission with
             | Error error -> failure Submit error
             | Ok state ->
               (match State.view state with
                | None -> failure Submit (P.Error.invalid_request "missing REPL view")
                | Some view ->
                  latest_task := None;
                  latest_submission
                  := Some (W.Lookup.create ~instance:(W.View.instance view) ~key);
                  inflight_submit_lookup := None;
                  pending_submit_source := None;
                  ready (State.submitted state ~source ~preserve_editor:retried);
                  ignore (observe_current () : bool)))
          | Submitted { result = Error error; _ } -> failure Submit error
          | Task_started { key; result = Ok task } ->
            (match State.view !state with
             | None -> failure Run_task (P.Error.invalid_request "missing REPL view")
             | Some view ->
               inflight_task_lookup := None;
               latest_task
               := Some
                    (P.Developer_common.Lookup.create ~run_id:(W.View.run_id view) ~key);
               ready (State.with_task !state task);
               ignore (observe_current () : bool))
          | Task_started { result = Error error; _ } -> failure Run_task error
          | Reset_done (Ok view) ->
            revision_knowledge := Known;
            latest_submission := None;
            latest_task := None;
            inflight_submit_lookup := None;
            inflight_task_lookup := None;
            ready (State.with_view !state view)
          | Reset_done (Error error) -> failure Reset error
          | Close_done (Ok ()) ->
            revision_knowledge := Known;
            latest_submission := None;
            latest_task := None;
            inflight_submit_lookup := None;
            inflight_task_lookup := None;
            show (State.with_activity (pending ()) Closed)
          | Close_done (Error error) -> failure Close error
          | Type_done { instance; epoch; result = Ok inspection }
            when current_epoch_matches instance epoch ->
            let text =
              W.Type_inspection.rendered inspection |> W.Submission.Rendered_type.text
            in
            ready (State.with_notice !state text)
          | Type_done { result = Ok _; _ } ->
            ready (State.with_notice !state "Stale type inspection discarded")
          | Type_done { result = Error error; _ } ->
            ready (State.with_notice !state (error_message error))
          | References_done { instance; epoch; revision; result = Ok page }
            when current_epoch_matches instance epoch
                 && Option.value_map
                      (State.current_revision !state)
                      ~default:false
                      ~f:(W.Wire_id.Revision.equal revision) ->
            ready (State.with_reference_page !state page)
          | References_done { result = Ok _; _ } ->
            ready (State.with_notice !state "Stale reference page discarded")
          | References_done { result = Error error; _ } ->
            ready (State.with_notice !state (error_message error))
          | Submission_observed (Ok (Some submission)) ->
            (match State.with_submission !state submission with
             | Ok state -> ready state
             | Error error -> ready (State.with_notice !state (error_message error)))
          | Submission_observed (Ok None) ->
            ready (State.with_notice !state "No authoritative submission result yet")
          | Submission_observed (Error error) ->
            ready (State.with_notice !state (error_message error))
          | Task_observed (Ok (Some task)) -> ready (State.with_task !state task)
          | Task_observed (Ok None) ->
            ready (State.with_notice !state "No authoritative Task result yet")
          | Task_observed (Error error) ->
            ready (State.with_notice !state (error_message error))
          | Submission_cancelled (Ok submission) ->
            (match State.with_submission !state submission with
             | Ok state -> ready state
             | Error error -> ready (State.with_notice !state (error_message error)))
          | Submission_cancelled (Error error) ->
            ready (State.with_notice !state (error_message error))
          | Task_cancelled (Ok task) -> ready (State.with_task !state task)
          | Task_cancelled (Error error) ->
            ready (State.with_notice !state (error_message error))
        in
        let reject message =
          notice message;
          false
        in
        let submit_source source =
          match
            State.activity !state, State.view !state, State.current_revision !state
          with
          | (Ready | Needs_reset), Some view, Some revision ->
            if State.equal_activity (State.activity !state) Needs_reset
            then reject "Current revision is unknown; reset before submitting"
            else if !control_active
            then reject "An exact-key cancellation is already in progress"
            else if active_task ()
            then reject "Cancel or await the active Task before submitting new source"
            else if String.is_empty (String.strip source)
            then reject "Source is empty"
            else (
              match key () with
              | Error error -> reject (error_message error)
              | Ok key ->
                (match
                   W.Submit.create
                     ~limits:(W.Capabilities.limits (Client.capabilities client))
                     ~instance:(W.View.instance view)
                     ~epoch:(W.View.epoch view)
                     ~expected_revision:revision
                     ~key
                     ~source
                 with
                 | Error error -> reject (error_message error)
                 | Ok request ->
                   pending_submit_source := Some source;
                   inflight_submit_lookup
                   := Some (W.Lookup.create ~instance:(W.View.instance view) ~key);
                   busy (Keyed Submit) (fun () ->
                     Submitted
                       { source
                       ; key
                       ; retried = false
                       ; result = Client.submit client request
                       })))
          | Busy _, _, _ -> reject "Finish or cancel the current request first"
          | Uncertain _, _, _ -> reject "Resolve the pending action before new work"
          | Closed, _, _ -> reject "This instance is closed"
          | Ready, _, _ | Needs_reset, _, _ -> reject "No current REPL view"
        in
        let run_task index =
          match State.activity !state, State.view !state with
          | Ready, Some view ->
            if !control_active
            then reject "An exact-key cancellation is already in progress"
            else if active_task ()
            then reject "Cancel or await the active Task before starting another"
            else (
              match State.task_reference !state ~index, key () with
              | Error error, _ | _, Error error -> reject (error_message error)
              | Ok reference, Ok key ->
                (match
                   W.Task_submit.create
                     ~limits:(W.Capabilities.limits (Client.capabilities client))
                     ~key
                     ~reference
                     ~requested_execution:None
                     ~requested_max_output_bytes:None
                     ~fresh_epoch:(Some (W.View.instance view, W.View.epoch view))
                 with
                 | Error error -> reject (error_message error)
                 | Ok request ->
                   inflight_task_lookup
                   := Some
                        (P.Developer_common.Lookup.create
                           ~run_id:(W.View.run_id view)
                           ~key);
                   busy (Keyed Run_task) (fun () ->
                     Task_started
                       { key; result = Client.task_submit client ~view request })))
          | Ready, None | Needs_reset, _ -> reject "No current Task reference"
          | Busy _, _ -> reject "Finish or cancel the current request first"
          | Uncertain _, _ -> reject "Resolve the pending action first"
          | Closed, _ -> reject "This instance is closed"
        in
        let cancel () =
          if revision_unknown !revision_knowledge
          then reject "Cancellation custody was relinquished; reset or create anew"
          else if !worker_active
          then
            if !control_active
            then reject "An exact-key cancellation is already in progress"
            else (
              match !inflight_task_lookup, !inflight_submit_lookup with
              | Some lookup, _ ->
                control_active := true;
                notice
                  "Cancelling the exact in-flight Task key; Task reply remains pending";
                Eio.Fiber.fork_daemon ~sw (fun () ->
                  Input_queue.post
                    input
                    (Control_completed
                       (Task_control
                          { lookup; result = Client.task_cancel_and_wait client lookup }));
                  `Stop_daemon);
                true
              | None, Some lookup ->
                control_active := true;
                notice "Cancelling the exact in-flight Submit key; reply remains pending";
                Eio.Fiber.fork_daemon ~sw (fun () ->
                  Input_queue.post
                    input
                    (Control_completed
                       (Submission_control
                          { lookup; result = Client.cancel_submission client lookup }));
                  `Stop_daemon);
                true
              | None, None -> reject "Current request has no cancelable key")
          else if
            Option.is_none !inflight_task_lookup
            && Option.is_none !inflight_submit_lookup
            && Option.is_some !latest_task
            && not (active_task ())
          then reject "Task is already terminal"
          else (
            match current_watch ~include_abandoned:false with
            | Some (W.Watch_key.Task lookup) ->
              busy Cancel (fun () ->
                Task_cancelled (Client.task_cancel_and_wait client lookup))
            | Some (W.Watch_key.Repl lookup) ->
              busy Cancel (fun () ->
                Submission_cancelled (Client.cancel_submission client lookup))
            | None -> reject "No exact active submission or Task to cancel")
        in
        let reset () =
          match State.activity !state, State.view !state with
          | (Ready | Needs_reset), Some view ->
            if !control_active
            then reject "An exact-key cancellation is already in progress"
            else if active_task ()
            then reject "Cancel or await the active Task before resetting"
            else (
              match key () with
              | Error error -> reject (error_message error)
              | Ok key ->
                let request =
                  W.Current_epoch.create
                    ~instance:(W.View.instance view)
                    ~epoch:(W.View.epoch view)
                    ~key
                in
                busy (Keyed Reset) (fun () ->
                  Reset_done (Client.reset_and_wait client request)))
          | _ -> reject "Reset requires an idle current instance"
        in
        let close () =
          match State.activity !state, State.view !state with
          | Ready, Some view ->
            if !control_active
            then reject "An exact-key cancellation is already in progress"
            else if active_task ()
            then reject "Cancel or await the active Task before closing"
            else (
              match key () with
              | Error error -> reject (error_message error)
              | Ok key ->
                busy (Keyed Close) (fun () ->
                  Close_done
                    (Client.close_and_wait
                       client
                       (W.Lookup.create ~instance:(W.View.instance view) ~key))))
          | _ -> reject "Close requires an idle current instance"
        in
        let current_submission view =
          match !latest_submission, State.submission !state with
          | Some lookup, Some submission
            when W.Wire_id.Instance.equal
                   (W.Lookup.instance lookup)
                   (W.View.instance view)
                 && W.Wire_id.Epoch.equal
                      (W.Submission.epoch submission)
                      (W.View.epoch view) ->
            Some
              (W.Current_epoch.create
                 ~instance:(W.View.instance view)
                 ~epoch:(W.View.epoch view)
                 ~key:(W.Lookup.key lookup))
          | Some _, Some _ | None, _ | _, None -> None
        in
        let inspect statement_index selector =
          match State.activity !state, State.view !state with
          | Ready, Some view ->
            if !control_active
            then reject "An exact-key cancellation is already in progress"
            else (
              let limits = W.Capabilities.limits (Client.capabilities client) in
              match current_submission view with
              | Some current ->
                (match
                   W.Inspect.create
                     ~limits
                     ~current
                     ~statement_index
                     ~selector
                     ~max_bytes:(Int.min 4_096 (W.Limits.max_output_bytes limits))
                 with
                 | Error error -> reject (error_message error)
                 | Ok request ->
                   busy Inspect_type (fun () ->
                     Type_done
                       { instance = W.View.instance view
                       ; epoch = W.View.epoch view
                       ; result = Client.inspect_type client request
                       }))
              | None ->
                reject "Type inspection requires a stored submission in the current epoch")
          | _ -> reject "Type inspection requires an idle current instance"
        in
        let references () =
          match State.activity !state, State.view !state with
          | Ready, Some view ->
            if !control_active
            then reject "An exact-key cancellation is already in progress"
            else (
              let limits = W.Capabilities.limits (Client.capabilities client) in
              match current_submission view with
              | None ->
                reject
                  "Reference paging requires a stored submission in the current epoch"
              | Some current ->
                (match
                   W.Reference_page_request.create
                     ~limits
                     ~current
                     ~after:
                       (Option.bind
                          (State.reference_page !state)
                          ~f:W.Reference_page.next)
                     ~limit:(Int.min 16 (W.Limits.max_page_items limits))
                 with
                 | Error error -> reject (error_message error)
                 | Ok request ->
                   busy References (fun () ->
                     References_done
                       { instance = W.View.instance view
                       ; epoch = W.View.epoch view
                       ; revision = Option.value_exn (State.current_revision !state)
                       ; result = Client.task_references_page client request
                       })))
          | _ -> reject "Reference paging requires an idle current instance"
        in
        let abandon () =
          if !worker_active || !control_active
          then reject "Wait for the in-flight request before relinquishing custody"
          else (
            let pending = Client.pending client in
            let result, abandoned =
              match
                ( pending.creates
                , pending.submits
                , pending.tasks
                , pending.resets
                , pending.closes )
              with
              | create :: _, _, _, _, _ -> Client.abandon_create client create, None
              | [], submit :: _, _, _, _ ->
                let identity = Client.Pending_submit.identity submit in
                ( Client.abandon_submit client submit
                , Some
                    (W.Watch_key.Repl
                       (W.Lookup.create ~instance:identity.instance ~key:identity.key)) )
              | [], [], task :: _, _, _ ->
                let identity = Client.Pending_task.identity task in
                ( Client.abandon_task client task
                , Some
                    (W.Watch_key.Task
                       (P.Developer_common.Lookup.create
                          ~run_id:identity.run_id
                          ~key:identity.key)) )
              | [], [], [], reset :: _, _ -> Client.abandon_reset client reset, None
              | [], [], [], [], close :: _ -> Client.abandon_close client close, None
              | [], [], [], [], [] ->
                Error (P.Error.invalid_request "No pending action to abandon"), None
            in
            match result with
            | Error error -> reject (error_message error)
            | Ok () ->
              revision_knowledge := Unknown abandoned;
              pending_submit_source := None;
              inflight_submit_lookup := None;
              inflight_task_lookup := None;
              show
                (State.with_pending !state (Client.pending client)
                 |> fun state ->
                 State.with_activity state Needs_reset
                 |> fun state ->
                 State.with_notice
                   state
                   "Pending custody relinquished; execution may have occurred. Reset or \
                    create anew.");
              true)
        in
        let retry () =
          if !worker_active || !control_active
          then reject "Finish the in-flight request before retrying"
          else (
            let pending = Client.pending client in
            match
              ( pending.creates
              , pending.submits
              , pending.tasks
              , pending.resets
              , pending.closes )
            with
            | create :: _, _, _, _, _ ->
              busy (Keyed Create) (fun () -> Created (Client.retry_create client create))
            | [], submit :: _, _, _, _ ->
              let identity = Client.Pending_submit.identity submit in
              busy (Keyed Submit) (fun () ->
                Submitted
                  { source = Option.value !pending_submit_source ~default:""
                  ; key = identity.key
                  ; retried = true
                  ; result = Client.retry_submit client submit
                  })
            | [], [], task :: _, _, _ ->
              let identity = Client.Pending_task.identity task in
              busy (Keyed Run_task) (fun () ->
                Task_started
                  { key = identity.key; result = Client.retry_task client task })
            | [], [], [], reset :: _, _ ->
              busy (Keyed Reset) (fun () -> Reset_done (Client.retry_reset client reset))
            | [], [], [], [], close :: _ ->
              busy (Keyed Close) (fun () -> Close_done (Client.retry_close client close))
            | [], [], [], [], [] -> reject "No pending action to retry")
        in
        let command = function
          | Command.Submit ->
            submit_source (State.Editor.source_before_final_line (State.editor !state))
          | Run index -> run_task index
          | Type { statement_index; selector } -> inspect statement_index selector
          | References -> references ()
          | Observe -> observe_current ~refresh:true ()
          | Cancel -> cancel ()
          | Reset -> reset ()
          | Close -> close ()
          | Retry -> retry ()
          | Abandon -> abandon ()
          | Quit -> true
          | Help ->
            notice
              ":submit :run N :type N [binding M] :refs :observe :cancel :reset :close \
               :retry :abandon :quit";
            true
        in
        let edit f =
          match State.activity !state with
          | Ready | Needs_reset | Uncertain _ ->
            (match f (State.editor !state) with
             | Ok editor ->
               (match State.with_editor !state editor with
                | Ok state -> show state
                | Error error -> notice (error_message error))
             | Error error -> notice (error_message error))
          | Busy _ | Closed ->
            notice "Finish or resolve the current action before editing"
        in
        Eio.Fiber.fork_daemon ~sw (fun () ->
          let rec reader () =
            match Agent_client.Connection.next_owned_notification lease with
            | Ok (Some envelope) ->
              (match Client.change_of_notification client envelope with
               | Ok (Some _) -> Input_queue.notify input
               | Ok None -> ()
               | Error error -> Input_queue.post input (Notification_error error));
              reader ()
            | Ok None -> ()
            | Error error -> Input_queue.post input (Notification_error error)
          in
          reader ();
          `Stop_daemon);
        ignore
          (busy (Keyed Create) (fun () -> Created (Client.create_instance client create))
           : bool);
        let rec loop () =
          match Input_queue.take input with
          | Error error -> Error error
          | Ok (Control_completed completion) ->
            control_finished completion;
            if not !worker_active then after_completion ();
            loop ()
          | Ok (Completed completion) ->
            worker_active := false;
            finish completion;
            after_completion ();
            loop ()
          | Ok Notification ->
            ignore (observe_current ~background:true () : bool);
            loop ()
          | Ok (Notification_error error) ->
            notice (error_message error);
            loop ()
          | Ok Input_overflow -> Error (P.Error.invalid_request "terminal input overflow")
          | Ok Input_closed -> Ok ()
          | Ok (Input Quit) -> Ok ()
          | Ok (Input input_event) ->
            let quit =
              match input_event with
              | Text text | Paste text ->
                edit (fun editor ->
                  State.Editor.insert editor ~limits:(State.limits !state) text);
                false
              | Enter ->
                (match Command.parse (State.Editor.final_line (State.editor !state)) with
                 | Some Quit -> true
                 | Some command_ ->
                   let accepted = command command_ in
                   if accepted
                   then (
                     match
                       State.with_editor
                         !state
                         (State.Editor.without_final_line (State.editor !state))
                     with
                     | Ok state -> show state
                     | Error error -> notice (error_message error));
                   false
                 | None ->
                   edit (fun editor ->
                     State.Editor.insert editor ~limits:(State.limits !state) "\n");
                   false)
              | Backspace ->
                edit (fun editor -> Ok (State.Editor.backspace editor));
                false
              | Delete ->
                edit (fun editor -> Ok (State.Editor.delete editor));
                false
              | Move motion ->
                edit (fun editor -> Ok (State.Editor.move editor motion));
                false
              | History_previous ->
                (match State.activity !state with
                 | Ready | Needs_reset | Uncertain _ ->
                   show (State.history_previous !state)
                 | Busy _ | Closed -> notice "History is unavailable during a request");
                false
              | History_next ->
                (match State.activity !state with
                 | Ready | Needs_reset | Uncertain _ -> show (State.history_next !state)
                 | Busy _ | Closed -> notice "History is unavailable during a request");
                false
              | Submit ->
                ignore (submit_source (State.Editor.source (State.editor !state)) : bool);
                false
              | Cancel ->
                ignore (cancel () : bool);
                false
              | Resize ->
                present !state;
                false
              | Quit -> true
            in
            if quit then Ok () else loop ()
        in
        present !state;
        loop ()))
;;

let run ~session ~create ~initial ~input ~present =
  run_on_connection
    ~client:(Client_adapter.client session)
    ~connection:(Client_adapter.connection session)
    ~create
    ~initial
    ~input
    ~present
;;

let%test_unit
    "revision uncertainty survives reads and Task admission follows terminal phase"
  =
  let open State in
  let open W.Task_view in
  let after_read =
    Transition.activity ~current:(Busy Observe) ~revision_unknown:true ~pending:None
  in
  if not (equal_activity after_read Needs_reset)
  then failwith "read cleared unknown revision";
  let pending =
    Transition.activity
      ~current:(Busy Observe)
      ~revision_unknown:true
      ~pending:(Some (Uncertain Submit))
  in
  if not (equal_activity pending (Uncertain Submit))
  then failwith "keyed pending custody lost to read";
  let after_reset =
    Transition.activity
      ~current:(Busy (Keyed Reset))
      ~revision_unknown:false
      ~pending:None
  in
  if not (equal_activity after_reset Ready)
  then failwith "authoritative reset did not restore readiness";
  if
    (not (Transition.task_active Reserved))
    || (not (Transition.task_active Execution_started))
    || Transition.task_active (Terminal { execution_started = true; outcome = Cancelled })
  then failwith "active Task gate differs from terminal receipt"
;;

let%test_unit "real portable client keeps retry, Task, and abandoned-read custody" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let must = function
      | Ok value -> value
      | Error error -> raise_s [%sexp (error : P.Error.t)]
    in
    let generator = P.Id.Generator.create ~bytes:(fun count -> String.make count 't') in
    let document bytes =
      Document_schema.Limits.create
        ~max_bytes:bytes
        ~max_depth:32
        ~max_fields:1_024
        ~max_nodes:2_048
      |> function
      | Ok value -> value
      | Error _ -> failwith "invalid fixture document limit"
    in
    let limits =
      W.Limits.create
        ~request:(document 8_192)
        ~result:(document 8_192)
        ~page:(document 4_096)
        ~response:(document 16_384)
        ~max_source_bytes:512
        ~max_reference_bytes:384
        ~max_output_bytes:512
        ~max_page_items:2
        ~max_event_bytes:256
        ~max_registry_entries:16
      |> must
    in
    let capabilities =
      W.Capabilities.create ~methods:W.Request.supported_methods ~limits |> must
    in
    let server_id = P.Id.Server.create_with generator in
    let principal_id = P.Id.Principal.create_with generator in
    let run_id = P.Id.Developer_run.create_with generator in
    let implementation =
      P.Initialize.Implementation.create ~name:"terminal-test" ~version:"1" |> must
    in
    let features =
      [ P.Developer_common.Features.run
      ; P.Developer_common.Features.events
      ; W.Features.repl
      ; W.Features.events
      ]
    in
    let principal =
      P.Principal.create
        ~id:principal_id
        ~authentication_kind:"embedded"
        ~scopes:P.Scope.Set.empty
        ~attributes:[]
      |> must
    in
    let initialized =
      P.Initialize.Response.create
        ~protocol_name:"ochat.agent"
        ~selected_version:P.Version.current
        ~implementation
        ~server_id
        ~enabled_features:features
        ~extensions:None
        ~principal
        ~limits:
          { max_request_bytes = 65_536
          ; max_event_bytes = 65_536
          ; max_page_size = 100
          ; max_attachments_per_connection = 8
          }
        ~event_retention:
          { minimum_age_ms = 1_000
          ; maximum_events = 1_024
          ; oldest_replayable_sequence = None
          }
        ~timing:
          { heartbeat_interval_ms = 1_000
          ; owner_lease_duration_ms = 1_000
          ; owner_renew_after_ms = 500
          ; disconnect_grace_default_ms = 100
          }
        ~server_time:(P.Timestamp.of_string "2026-10-20T00:00:00Z" |> must)
      |> must
    in
    let instance = W.Wire_id.Instance.of_string "ri_terminal_fixture" |> must in
    let epoch_one = W.Wire_id.Epoch.of_string "re_terminal_one" |> must in
    let epoch_two = W.Wire_id.Epoch.of_string "re_terminal_two" |> must in
    let revision_one = W.Wire_id.Revision.of_string "rv_terminal_one" |> must in
    let revision_two = W.Wire_id.Revision.of_string "rv_terminal_two" |> must in
    let revision_three = W.Wire_id.Revision.of_string "rv_terminal_three" |> must in
    let capture_fingerprint =
      Chatml_source.Digest.of_contents "terminal-capture" ~checkpoint:Fn.id
    in
    let view epoch revision =
      W.View.create ~limits ~instance ~epoch ~revision ~run_id ~capture_fingerprint
      |> must
    in
    let view_one = view epoch_one revision_one in
    let view_two = view epoch_two revision_two in
    let retained_summary =
      W.Submission.Summary.create
        ~limits
        ~outcome:Succeeded
        ~value_preview:(Some "Task")
        ~diagnostic:None
        ~expression_type:None
        ~binding_count:0
        ~task_retention:Retained
        ~text_limited:false
      |> must
    in
    let submission epoch revision ~task =
      W.Submission.create
        ~limits
        ~status:Finished
        ~details:Available
        ~completed_statements:1
        ~statement_count:1
        ~epoch
        ~revision
        ~summaries:(if task then [ retained_summary ] else [])
        ~task_reference_count:(if task then 1 else 0)
        ~task_references:(if task then [ "opaque-terminal-reference" ] else [])
      |> must
    in
    let first_submission = submission epoch_one revision_one ~task:true in
    let second_submission = submission epoch_two revision_two ~task:true in
    let third_submission = submission epoch_two revision_three ~task:false in
    let owner =
      P.Developer_common.Owner.create ~host_id:server_id ~run_id ~epoch:0 ~principal_id
      |> must
    in
    let task_terminal = ref false in
    let task_key = ref None in
    let task_view key =
      W.Task_view.create
        ~limits
        ~command_id:(P.Developer_command_id.of_string "terminal-task" |> must)
        ~owner
        ~key
        ~phase:
          (if !task_terminal
           then Terminal { execution_started = true; outcome = Succeeded None }
           else Reserved)
        ~request_fingerprint:capture_fingerprint
        ~capture_fingerprint
      |> must
    in
    let submit_count = ref 0 in
    let first_submit_started, first_submit_started_u = Eio.Promise.create () in
    let first_submit_release, first_submit_release_u = Eio.Promise.create () in
    let second_submit_started, second_submit_started_u = Eio.Promise.create () in
    let second_submit_release, second_submit_release_u = Eio.Promise.create () in
    let second_control_started, second_control_started_u = Eio.Promise.create () in
    let second_control_release, second_control_release_u = Eio.Promise.create () in
    let idle_cancel_started, idle_cancel_started_u = Eio.Promise.create () in
    let idle_cancel_release, idle_cancel_release_u = Eio.Promise.create () in
    let first_submit_key = ref None in
    let second_submit_key = ref None in
    let abandoned_submit_key = ref None in
    let abandoned_observe_keys = ref [] in
    let second_observe_keys = ref [] in
    let control_count = ref 0 in
    let observe_count = ref 0 in
    let watch_count = ref 0 in
    let inspection_count = ref 0 in
    let reference_page_count = ref 0 in
    let foreign_observation = ref false in
    let reset_count = ref 0 in
    let task_submit_count = ref 0 in
    let response value = W.Response.to_json value ~limits in
    let public_initialized =
      P.Public.Result.Non_history.of_internal (Protocol_initialize initialized)
      |> must
      |> fun value -> P.Public.Result.Non_history value
    in
    let physical =
      Agent_client.In_memory.create_with_developer_repl
        ~request:(function
          | P.Command.Protocol_initialize _ -> Ok public_initialized
          | _ -> Error (P.Error.invalid_request "unexpected public command"))
        ~request_developer:(fun ~bound:_ _ ->
          Error (P.Error.invalid_request "unexpected Developer request"))
        ~request_repl:(fun ~max_request_bytes:_ ~bound:_ request ->
          match request with
          | W.Request.Capabilities -> Ok (W.Capabilities.to_json capabilities)
          | Create _ -> response (Create view_one)
          | Submit submit ->
            Int.incr submit_count;
            (match !submit_count with
             | 1 ->
               first_submit_key := Some (W.Submit.key submit);
               Eio.Promise.resolve first_submit_started_u ();
               Eio.Promise.await first_submit_release;
               Error
                 (P.Error.create
                    Interrupted
                    ~message:"reply deliberately lost"
                    ~retryable:true
                    ())
             | 3 ->
               second_submit_key := Some (W.Submit.key submit);
               Eio.Promise.resolve second_submit_started_u ();
               Eio.Promise.await second_submit_release;
               response (Submit second_submission)
             | 4 ->
               abandoned_submit_key := Some (W.Submit.key submit);
               Error
                 (P.Error.create
                    Interrupted
                    ~message:"reply deliberately lost"
                    ~retryable:true
                    ())
             | 2 ->
               if not (String.equal (W.Submit.source submit) "Task.pure(8)")
               then Error (P.Error.invalid_request "retry changed original source")
               else response (Submit first_submission)
             | _ -> response (Submit second_submission))
          | Cancel_submission lookup ->
            Int.incr control_count;
            if
              not
                (Option.value_map
                   (if Int.equal !control_count 3
                    then !second_submit_key
                    else !first_submit_key)
                   ~default:false
                   ~f:(fun key -> P.Idempotency_key.equal key (W.Lookup.key lookup)))
            then Error (P.Error.invalid_request "control used a different Submit key")
            else (
              if Int.equal !control_count 2
              then (
                Eio.Promise.resolve idle_cancel_started_u ();
                Eio.Promise.await idle_cancel_release);
              if Int.equal !control_count 3
              then (
                Eio.Promise.resolve second_control_started_u ();
                Eio.Promise.await second_control_release);
              response
                (Cancel_submission
                   (if Int.equal !control_count 3
                    then second_submission
                    else first_submission)))
          | Watch _ ->
            Int.incr watch_count;
            response Watch
          | Unwatch _ -> response Unwatch
          | Observe lookup ->
            Int.incr observe_count;
            if Int.equal !submit_count 3
            then second_observe_keys := W.Lookup.key lookup :: !second_observe_keys;
            if Option.is_some !abandoned_submit_key
            then abandoned_observe_keys := W.Lookup.key lookup :: !abandoned_observe_keys;
            response
              (Observe
                 (Some
                    (if !foreign_observation
                     then first_submission
                     else if !reset_count = 0
                     then first_submission
                     else if !submit_count < 4
                     then second_submission
                     else third_submission)))
          | Task_submit request ->
            Int.incr task_submit_count;
            task_key := Some (W.Task_submit.key request);
            response (Task_submit (task_view (W.Task_submit.key request)))
          | Task_observe _ -> response (Task_observe (Option.map !task_key ~f:task_view))
          | Inspect_type request ->
            Int.incr inspection_count;
            let current = W.Inspect.current request in
            if
              not
                (Option.value_map !first_submit_key ~default:false ~f:(fun key ->
                   P.Idempotency_key.equal key (W.Current_epoch.key current)))
            then Error (P.Error.invalid_request "inspection lost the submission key")
            else
              response
                (Inspect_type
                   (W.Type_inspection.create
                      ~limits
                      ~revision:revision_one
                      ~binding_name:None
                      ~rendered:
                        (W.Submission.Rendered_type.create
                           ~limits
                           ~kind:Type
                           ~completeness:Complete
                           ~text:"Task Int"
                         |> must)
                    |> must))
          | Task_references_page request ->
            Int.incr reference_page_count;
            let current = W.Reference_page_request.current request in
            if
              not
                (Option.value_map !first_submit_key ~default:false ~f:(fun key ->
                   P.Idempotency_key.equal key (W.Current_epoch.key current)))
            then Error (P.Error.invalid_request "reference page lost the submission key")
            else
              response
                (Task_references_page
                   (W.Reference_page.create
                      ~limits
                      ~items:[ "opaque-terminal-reference" ]
                      ~next:None
                      ~total:1
                    |> must))
          | Reset_and_wait _ ->
            Int.incr reset_count;
            response (Reset_and_wait view_two)
          | _ -> Error (P.Error.invalid_request "unexpected REPL method"))
        ~notifications:(Eio.Stream.create 8)
        ~close:Fn.id
    in
    let initialize_request =
      P.Initialize.Request.create
        ~implementation
        ~protocol_min:P.Version.current
        ~protocol_max:P.Version.current
        ~features
        ~event_encodings:[ Json ]
        ~max_inbound_event_bytes:65_536
        ()
      |> must
    in
    (match
       Agent_client.Connection.request_without_history
         physical
         (P.Command.Protocol_initialize initialize_request)
     with
     | Ok (Protocol_initialize _) -> ()
     | Ok _ | Error _ -> failwith "terminal client Initialize failed");
    let client = Client.create physical |> must in
    let route =
      P.Developer_common.Route.local_source
        ~limits:(W.Limits.request limits)
        ~root_index:0
        ~member:(Source.Portable_path.create "source.chatml" |> Or_error.ok_exn)
      |> must
    in
    let create =
      W.Create.create
        ~limits
        ~run_id
        ~key:(P.Idempotency_key.of_string "terminal-create" |> must)
        ~route
        ~target:One_off_v1
        ~prompt:(P.Id.Prompt_definition.create_with generator)
        ~workspace:(P.Id.Workspace_definition.create_with generator)
        ~requested_execution:None
        ~requested_max_output_bytes:None
      |> must
    in
    let state_limits =
      State.Limits.create
        ~max_source_bytes:512
        ~max_source_lines:16
        ~max_history_entries:8
        ~max_history_bytes:2_048
        ~max_notice_bytes:512
      |> must
    in
    let initial = State.create ~limits:state_limits ~context:"fixture" |> must in
    let input = Input_queue.create ~capacity:32 |> must in
    let shown = ref initial in
    let await label predicate =
      match
        Eio.Time.with_timeout (Eio.Stdenv.clock env) 5. (fun () ->
          while not (predicate !shown) do
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.001
          done;
          Ok ())
      with
      | Ok () -> ()
      | Error `Timeout ->
        failwith
          ("timed out waiting for "
           ^ label
           ^ "; activity="
           ^ Sexp.to_string_hum ([%sexp_of: State.activity] (State.activity !shown))
           ^ "; notice="
           ^ Option.value (State.notice !shown) ~default:"(none)")
    in
    let command text =
      Input_queue.offer input (Text text);
      Input_queue.offer input Enter
    in
    Eio.Switch.run (fun sw ->
      let running =
        Eio.Fiber.fork_promise ~sw (fun () ->
          run_on_connection
            ~client
            ~connection:physical
            ~create
            ~initial
            ~input
            ~present:(fun state -> shown := state))
      in
      await "Create" (fun state -> Option.is_some (State.view state));
      command ":type 1";
      await "inspection requires a stored submission" (fun state ->
        Option.value_map (State.notice state) ~default:false ~f:(fun text ->
          String.is_substring text ~substring:"requires a stored submission"));
      if not (Int.equal !inspection_count 0)
      then failwith "inspection invented a submission key before Submit";
      for _ = 1 to String.length ":type 1" do
        Input_queue.offer input Backspace
      done;
      command ":refs";
      await "reference paging requires a stored submission" (fun state ->
        Option.value_map (State.notice state) ~default:false ~f:(fun text ->
          String.is_substring
            text
            ~substring:"Reference paging requires a stored submission"));
      if not (Int.equal !reference_page_count 0)
      then failwith "reference page invented a submission key before Submit";
      for _ = 1 to String.length ":refs" do
        Input_queue.offer input Backspace
      done;
      command "Task.pure(8)\n:submit";
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
        Eio.Promise.await first_submit_started);
      Input_queue.offer input Cancel;
      await "held exact-key control" (fun state ->
        Option.value_map (State.notice state) ~default:false ~f:(fun text ->
          String.is_substring text ~substring:"Cancellation answered"));
      if not (Int.equal !control_count 1)
      then failwith "held Submit did not accept exact-key control";
      if not (State.equal_activity (State.activity !shown) (Busy (Keyed Submit)))
      then failwith "control reply incorrectly completed held Submit";
      Eio.Promise.resolve first_submit_release_u ();
      await "lost Submit" (fun state ->
        State.equal_activity (State.activity state) (Uncertain Submit));
      command "draft\n:retry";
      await "exact retry" (fun state ->
        Option.is_some (State.submission state)
        && String.is_substring
             (State.Editor.source (State.editor state))
             ~substring:"draft");
      if not (Int.equal !submit_count 2)
      then failwith "retry did not retain one original Submit";
      await "initial observation settled" (fun state ->
        State.equal_activity (State.activity state) Ready);
      let before_hint_watch = !watch_count in
      let before_hint_observe = !observe_count in
      Input_queue.notify input;
      await "same-key hint reads without registering" (fun state ->
        !observe_count > before_hint_observe
        && State.equal_activity (State.activity state) Ready);
      if not (Int.equal !watch_count before_hint_watch)
      then failwith "same-key hint registered another Watch";
      command "\n:observe";
      await "explicit observation refreshes registration" (fun state ->
        !watch_count > before_hint_watch
        && State.equal_activity (State.activity state) Ready);
      command "\n:refs";
      await "reference page uses stored submission before Task" (fun state ->
        Int.equal !reference_page_count 1
        && State.equal_activity (State.activity state) Ready
        && Option.is_some (State.reference_page state));
      Input_queue.offer input (Text "\n:reset");
      await "pending reset command line" (fun state ->
        String.equal (State.Editor.final_line (State.editor state)) ":reset");
      Input_queue.offer input Cancel;
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
        Eio.Promise.await idle_cancel_started);
      Input_queue.offer input Enter;
      await "Busy rejection keeps command line" (fun state ->
        Option.value_map (State.notice state) ~default:false ~f:(fun text ->
          String.is_substring text ~substring:"Reset requires an idle")
        && String.equal (State.Editor.final_line (State.editor state)) ":reset");
      Eio.Promise.resolve idle_cancel_release_u ();
      await "cancel completed without editing" (fun state ->
        State.equal_activity (State.activity state) Ready
        && String.equal (State.Editor.final_line (State.editor state)) ":reset");
      command "\n:run 1";
      await "active Task" (fun state ->
        Option.value_map (State.task state) ~default:false ~f:(fun task ->
          Transition.task_active (W.Task_view.phase task)));
      let before_blocked = !submit_count in
      Input_queue.offer input Submit;
      await "active Task blocks Submit" (fun state ->
        Option.value_map (State.notice state) ~default:false ~f:(fun text ->
          String.is_substring text ~substring:"active Task"));
      if not (Int.equal before_blocked !submit_count)
      then failwith "active Task admitted another Submit";
      task_terminal := true;
      command ":observe";
      await "Task terminal" (fun state ->
        Option.value_map (State.task state) ~default:false ~f:(fun task ->
          not (Transition.task_active (W.Task_view.phase task))));
      await "Task observation settled" (fun state ->
        State.equal_activity (State.activity state) Ready);
      command ":type 1";
      await "inspection retains submission key after Task" (fun state ->
        Int.equal !inspection_count 1
        && State.equal_activity (State.activity state) Ready
        && Option.value_map
             (State.notice state)
             ~default:false
             ~f:(String.equal "Task Int"));
      command ":refs";
      await "reference page retains submission key after Task" (fun state ->
        Int.equal !reference_page_count 2
        && State.equal_activity (State.activity state) Ready);
      command ":reset";
      await "Reset" (fun state ->
        Option.value_map (State.view state) ~default:false ~f:(fun view ->
          W.Wire_id.Epoch.equal (W.View.epoch view) epoch_two));
      if Option.is_some (State.notice !shown) || Option.is_some (State.task !shown)
      then failwith "Reset retained the previous Task or inspection notice";
      command ":type 1";
      await "reset discards inspection custody" (fun state ->
        Option.value_map (State.notice state) ~default:false ~f:(fun text ->
          String.is_substring text ~substring:"requires a stored submission"));
      if not (Int.equal !inspection_count 1)
      then failwith "inspection reused a pre-reset submission key";
      for _ = 1 to String.length ":type 1" do
        Input_queue.offer input Backspace
      done;
      command ":refs";
      await "reset discards reference-page custody" (fun state ->
        Option.value_map (State.notice state) ~default:false ~f:(fun text ->
          String.is_substring
            text
            ~substring:"Reference paging requires a stored submission"));
      if not (Int.equal !reference_page_count 2)
      then failwith "reference page reused a pre-reset submission key";
      for _ = 1 to String.length ":refs" do
        Input_queue.offer input Backspace
      done;
      command "Task.pure(9)\n:submit";
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
        Eio.Promise.await second_submit_started);
      Input_queue.offer input Cancel;
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
        Eio.Promise.await second_control_started);
      Eio.Promise.resolve second_submit_release_u ();
      await "ordinary reply before control reply" (fun state ->
        State.equal_activity (State.activity state) Ready
        && Option.value_map (State.submission state) ~default:false ~f:(fun value ->
          W.Wire_id.Revision.equal (W.Submission.revision value) revision_two));
      Input_queue.offer input (Text "Task.pure(10)");
      await "draft while control remains active" (fun state ->
        String.equal (State.Editor.source (State.editor state)) "Task.pure(10)");
      Input_queue.offer input Submit;
      await "control blocks new Submit before lookup" (fun state ->
        Option.value_map (State.notice state) ~default:false ~f:(fun text ->
          String.is_substring text ~substring:"exact-key cancellation"));
      if not (Int.equal !submit_count 3)
      then failwith "new Submit was admitted while control reply was pending";
      command "\n:run 1";
      await "control blocks Task run before lookup" (fun state ->
        Option.value_map (State.notice state) ~default:false ~f:(fun text ->
          String.is_substring text ~substring:"exact-key cancellation")
        && String.equal (State.Editor.final_line (State.editor state)) ":run 1");
      if not (Int.equal !task_submit_count 1)
      then failwith "Task run was admitted while control reply was pending";
      Eio.Promise.resolve second_control_release_u ();
      await "held control completed" (fun state ->
        Option.value_map (State.notice state) ~default:false ~f:(fun text ->
          String.is_substring text ~substring:"Cancellation answered"));
      let before_observe = !observe_count in
      command "\n:observe";
      await "observation after rejected new work" (fun state ->
        !observe_count > before_observe
        && State.equal_activity (State.activity state) Ready);
      (match !second_submit_key with
       | Some key
         when (not (List.is_empty !second_observe_keys))
              && List.for_all !second_observe_keys ~f:(P.Idempotency_key.equal key) -> ()
       | Some _ | None -> failwith "rejected work replaced the observed Submit key");
      command "\n:submit";
      await "third lost Submit" (fun state ->
        State.equal_activity (State.activity state) (Uncertain Submit));
      command ":abandon";
      await "abandoned revision" (fun state ->
        State.equal_activity (State.activity state) Needs_reset);
      let before_cancel = !control_count in
      Input_queue.offer input Cancel;
      await "abandoned cancellation custody" (fun state ->
        Option.value_map (State.notice state) ~default:false ~f:(fun text ->
          String.is_substring text ~substring:"custody was relinquished"));
      if not (Int.equal !control_count before_cancel)
      then failwith "abandoned key was used for cancellation";
      foreign_observation := true;
      let before_observe = !observe_count in
      command ":observe";
      await "foreign read" (fun state ->
        !observe_count > before_observe
        && State.equal_activity (State.activity state) Needs_reset);
      if
        not
          (Option.value_map
             (State.current_revision !shown)
             ~default:false
             ~f:(W.Wire_id.Revision.equal revision_two))
      then failwith "foreign historical result changed the displayed revision";
      foreign_observation := false;
      let before_observe = !observe_count in
      command ":observe";
      await "historical read" (fun state ->
        !observe_count > before_observe
        && State.equal_activity (State.activity state) Needs_reset);
      (match !abandoned_submit_key with
       | Some key
         when List.length !abandoned_observe_keys >= 2
              && List.for_all !abandoned_observe_keys ~f:(P.Idempotency_key.equal key) ->
         ()
       | Some _ | None -> failwith "abandoned Observe used a different historical key");
      Input_queue.offer input Quit;
      match Eio.Promise.await running with
      | Ok (Ok ()) -> ()
      | Ok (Error error) -> raise_s [%sexp (error : P.Error.t)]
      | Error exn -> raise exn);
    Agent_client.Connection.close physical)
;;
