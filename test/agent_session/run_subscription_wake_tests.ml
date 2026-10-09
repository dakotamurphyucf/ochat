open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module Actor = A.Session_actor
module N = A.Notification_delivery

module Staged = struct
  type t =
    { subscription : P.Subscription.t
    ; delivery : P.Delivery.t
    ; source : P.Invocation.observer
    ; wake : P.Run_wake.t
    }
end

let current_capabilities () = Notification_disclosure_tests.registry [] (ref 0)

let stage_wait actor manager capabilities =
  let snapshot =
    Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
  in
  let source : P.Invocation.observer =
    { script_id = snapshot.script_id; source_sha256 = snapshot.script_source_hash }
  in
  let parent = Job_fixtures.add_claimed_job actor in
  let captured = ref None in
  Actor.with_job_execution
    actor
    ~job_id:parent.id
    ~generation:parent.generation
    ~attempt:parent.attempt
    ~deadline:None
    (fun services ->
       services.execute ~invocation:(Job_fixtures.root parent) (fun ~dispatched:root ->
         let invocation =
           P.Invocation.create
             { root.context with
               id = P.Id.Invocation.create ()
             ; parent_job = None
             ; parent_invocation = Some root.context.id
             ; tool_name = "counter"
             }
           |> protocol_ok
         in
         services.moderator_execute ~invocation (fun ~dispatched ~commit ->
           let owner = P.Job.Invocation dispatched.P.Invocation.context.id in
           let creation, pending =
             Actor.create_script_subscription
               actor
               ~owner
               ~source
               ~kind:"run-wake"
               ~lifetime_ms:1000
               ~wake:Request_turn
               ~completion_schema:None
             |> protocol_ok
           in
           let completion, terminal =
             Actor.finish_script_subscription
               actor
               ~owner
               ~source
               ~id:pending.context.id
               ~expected_epoch:pending.epoch
               (Succeeded (`String "ready"))
             |> protocol_ok
           in
           Actor.select_subscription_mutations
             actor
             ~owner
             ~source
             ~receipts:[ creation; completion ]
           |> protocol_ok;
           captured := Some terminal;
           let resolved =
             P.Invocation.resolve
               dispatched
               ~session_id:dispatched.context.session_id
               ~generation:dispatched.context.generation
               (Pending (Subscription terminal.context.id, `String "accepted"))
             |> protocol_ok
           in
           commit ~resolved ~snapshot)
         |> Result.map ~f:(fun () -> P.Invocation.Complete `Null)))
  |> protocol_ok
  |> ignore;
  let subscription = Option.value_exn !captured in
  let staged = ref None in
  Actor.with_current_moderator_event
    actor
    ~operation_id:None
    ~event:Session_start
    ~snapshot:(fun () -> Ok snapshot)
    (fun ~executing ~retirement_reason:_ ~event:_ ~execute:_ ~commit ->
       let actions =
         Actor.run_actions actor executing |> protocol_ok |> Option.value_exn
       in
       let disclosure_pins =
         Chat_response.Background_request.capability_pins capabilities |> protocol_ok
       in
       let notification, delivery =
         Actor.create_script_notification
           ~disclosure_pins
           actor
           ~owner:(Moderator_event executing.context.id)
           ~source
           ~correlation:
             { key = "run-subscription-wake"
             ; invocation_id = Some subscription.context.invocation_id
             ; work = Some (Subscription subscription.context.id)
             }
           ~completion:(Option.value_exn subscription.result)
           ~wake:Request_turn
         |> protocol_ok
       in
       Actor.select_notification_mutations
         actor
         ~owner:(Moderator_event executing.context.id)
         ~source
         ~receipts:[ notification ]
       |> protocol_ok;
       let state = Actor.state actor |> protocol_ok in
       let run = Option.value_exn state.run_state |> A.Run_state.runs |> List.hd_exn in
       let ownership = Option.value_exn delivery.context.ownership in
       let binding = Option.value_exn ownership.subscription_binding in
       let wake =
         P.Run_wake.create
           ~run_id:run.id
           ~source:run.source
           ~occurrence:
             (Subscription_delivery
                { subscription_id = binding.subscription_id
                ; epoch = binding.epoch
                ; delivery_id = delivery.context.id
                ; creator = ownership.creator
                })
         |> protocol_ok
       in
       let transaction = A.Run_action_service.transaction actions in
       let ticket = transaction.handlers.stage (Wait wake) |> Result.ok_or_failwith in
       ignore
         (transaction.prepare [ ticket ] |> Result.ok_or_failwith : P.Run_action.t option);
       staged := Some { Staged.subscription; delivery; source; wake };
       commit
         ~snapshot
         ~requests:
           { request_turn = false; request_compaction = false; end_session = None })
  |> protocol_ok
  |> ignore;
  Option.value_exn !staged
;;

let prepare state (staged : Staged.t) capabilities =
  N.prepare_idle
    ~state
    ~source:staged.source
    ~current_capabilities:capabilities
    ~policy:Chat_response.One_off_request.default_policy
    ~max_count:8
;;

let run state =
  Option.value_exn state.A.Session_state.run_state |> A.Run_state.runs |> List.hd_exn
;;

let%expect_test
    "actual subscription wake binds one admitted operation under current authority"
  =
  let started, signal_started = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let starts = ref 0 in
  let make_worker manager =
    A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
      Int.incr starts;
      if not (Eio.Promise.is_resolved started)
      then Eio.Promise.resolve signal_started input.operation.id;
      Eio.Promise.await release;
      A.Operation_worker.Completed
        { final_history = input.history
        ; moderator_snapshot =
            Some
              (Chat_response.Moderator_manager.identity_snapshot manager
               |> Result.ok_or_failwith
               |> A.Moderator_checkpoint.encode)
        ; runtime_requests = []
        })
  in
  Run_admission_tests.with_startup_actor
    ~make_worker
    (fun actor authorized reject_commit request start manager _ _ _ ->
       Exn.protect
         ~finally:(fun () ->
           Eio.Cancel.protect (fun () ->
             if not (Eio.Promise.is_resolved release)
             then Eio.Promise.resolve signal_release ()))
         ~f:(fun () ->
           ignore (start (request "subscription-wake") |> protocol_ok : P.Run_receipt.t);
           let capabilities = current_capabilities () in
           let staged = stage_wait actor manager capabilities in
           let before = Actor.state actor |> protocol_ok in
           let waiting =
             match (run before).lifecycle with
             | Waiting wake -> P.Run_wake.equal wake staged.wake
             | Admitted | Active | Terminal _ -> false
           in
           let attempt () =
             Actor.deliver_idle_notifications
               actor
               (prepare before staged capabilities |> protocol_ok)
           in
           authorized := false;
           let revoked = Result.is_error (attempt ()) in
           assert_same_session_snapshot before (Actor.state actor |> protocol_ok);
           authorized := true;
           reject_commit := true;
           let rejected_commit = Result.is_error (attempt ()) in
           reject_commit := false;
           assert_same_session_snapshot before (Actor.state actor |> protocol_ok);
           let no_early_worker = Int.equal !starts 0 in
           let accepted = attempt () |> protocol_ok in
           let operation_id = Eio.Promise.await started in
           let admitted = Actor.state actor |> protocol_ok in
           let index = Option.value_exn admitted.run_state in
           let consumed_once =
             Int.equal
               1
               (List.count (A.Run_state.intents index) ~f:(fun intent ->
                  match intent.action, intent.disposition with
                  | Wait wake, Consumed (Some id) ->
                    P.Run_wake.equal wake staged.wake
                    && P.Id.Operation.equal id operation_id
                  | (Continue | Wait _ | Finish _), (Pending | Consumed _ | Retired) ->
                    false))
           in
           let actual_owner =
             List.exists (run admitted).owned_work ~f:(fun work ->
               P.Run_work.Key.equal work.key (Operation operation_id)
               && Int.equal work.generation admitted.identity.generation)
           in
           let committed = List.hd_exn admitted.deliveries in
           let delivery_bound =
             match committed.wake_disposition with
             | Some (Accepted_wake id) -> P.Id.Operation.equal id operation_id
             | Some Pending_wake | Some (Discarded_wake _) | None -> false
           in
           let no_replay =
             not
               (Actor.deliver_idle_notifications
                  actor
                  (prepare admitted staged capabilities |> protocol_ok)
                |> protocol_ok)
           in
           assert_same_session_snapshot admitted (Actor.state actor |> protocol_ok);
           Eio.Promise.resolve signal_release ();
           let settled = await_idle actor in
           let no_late_replay =
             not
               (Actor.deliver_idle_notifications
                  actor
                  (prepare settled staged capabilities |> protocol_ok)
                |> protocol_ok)
           in
           assert_same_session_snapshot settled (Actor.state actor |> protocol_ok);
           print_s
             [%sexp
               { terminal_epoch = (Int.equal staged.subscription.epoch 1 : bool)
               ; waiting : bool
               ; revoked : bool
               ; rejected_commit : bool
               ; no_early_worker : bool
               ; accepted : bool
               ; consumed_once : bool
               ; actual_owner : bool
               ; delivery_bound : bool
               ; no_replay : bool
               ; no_late_replay : bool
               ; workers = (!starts : int)
               }]));
  [%expect
    {|
    ((terminal_epoch true) (waiting true) (revoked true) (rejected_commit true)
     (no_early_worker true) (accepted true) (consumed_once true)
     (actual_owner true) (delivery_bound true) (no_replay true)
     (no_late_replay true) (workers 1))
    |}]
;;

let%expect_test "subscription proposals cannot change the captured occurrence" =
  let starts = ref 0 in
  let make_worker manager =
    A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
      Int.incr starts;
      A.Operation_worker.Completed
        { final_history = input.history
        ; moderator_snapshot =
            Some
              (Chat_response.Moderator_manager.identity_snapshot manager
               |> Result.ok_or_failwith
               |> A.Moderator_checkpoint.encode)
        ; runtime_requests = []
        })
  in
  Run_admission_tests.with_startup_actor
    ~make_worker
    (fun actor _ _ request start manager _ _ _ ->
       ignore (start (request "subscription-immutable") |> protocol_ok : P.Run_receipt.t);
       let capabilities = current_capabilities () in
       let staged = stage_wait actor manager capabilities in
       let before = Actor.state actor |> protocol_ok in
       let context = staged.delivery.context in
       let ownership = Option.value_exn context.ownership in
       let binding = Option.value_exn ownership.subscription_binding in
       let wrong_source = { ownership.source with source_sha256 = String.make 64 'f' } in
       let wrong_binding =
         P.Delivery.Subscription_binding.create
           ~subscription_id:binding.subscription_id
           ~epoch:(binding.epoch + 1)
         |> protocol_ok
       in
       let contexts =
         [ { context with generation = context.generation + 1 }
         ; { context with ownership = Some { ownership with source = wrong_source } }
         ; { context with
             ownership = Some { ownership with subscription_binding = Some wrong_binding }
           }
         ; { context with
             ownership =
               Some
                 { ownership with
                   creator = Invocation staged.subscription.context.invocation_id
                 }
           }
         ]
       in
       let rejected =
         List.map contexts ~f:(fun context ->
           let proposal =
             P.Delivery.create ?disclosure_pins:staged.delivery.disclosure_pins context
             |> protocol_ok
           in
           let candidate = { before with deliveries = [ proposal ] } in
           let refused =
             match prepare candidate staged capabilities with
             | Error _ -> true
             | Ok plan ->
               (match Actor.deliver_idle_notifications actor plan with
                | Error _ | Ok false -> true
                | Ok true -> false)
           in
           assert_same_session_snapshot before (Actor.state actor |> protocol_ok);
           refused)
       in
       let prepared = prepare before staged capabilities |> protocol_ok in
       Actor.change_moderator actor None |> protocol_ok |> ignore;
       let replaced = Actor.state actor |> protocol_ok in
       let stale_source_rejected =
         match Actor.deliver_idle_notifications actor prepared with
         | Error _ | Ok false -> true
         | Ok true -> false
       in
       assert_same_session_snapshot replaced (Actor.state actor |> protocol_ok);
       print_s
         [%sexp
           { rejected : bool list
           ; stale_source_rejected : bool
           ; workers = (!starts : int)
           }]);
  [%expect
    {| ((rejected (true true true true)) (stale_source_rejected true) (workers 0)) |}]
;;

let%expect_test "actual pending expiry cannot become a successful subscription wake" =
  let module Setup = Subscription_transaction_tests in
  let module Clock = Extension_clock_tests in
  let wall = ref (Clock.wall 0) in
  let elapsed = ref (Clock.mono 0) in
  Job_fixtures.with_actor
    ~now:(fun () -> !wall)
    ~monotonic_now:(fun () -> !elapsed)
    (fun _ _ actor _ _ ->
       Actor.change_moderator actor (Some (Setup.encode Setup.before))
       |> protocol_ok
       |> ignore;
       let parent = Job_fixtures.add_claimed_job actor in
       let pending = Subscription_clock_tests.admit actor parent (fun () -> ()) in
       wall := Clock.wall 1000;
       elapsed := Clock.mono 1000;
       let expired = Actor.expire_subscriptions actor |> protocol_ok in
       let stale_completion_rejected = ref false in
       let stale_delivery_rejected = ref false in
       Schedule_transaction_tests.with_event
         ~snapshot:Setup.after
         actor
         parent
         (fun owner commit ->
            let before = Actor.state actor |> protocol_ok in
            (match
               Actor.finish_script_subscription
                 actor
                 ~owner
                 ~source:Setup.source
                 ~id:pending.context.id
                 ~expected_epoch:pending.epoch
                 (Succeeded (`String "late"))
             with
             | Error _ -> stale_completion_rejected := true
             | Ok (receipt, winner) ->
               stale_completion_rejected
               := Option.equal P.Completion.equal winner.result (Some Expired);
               Actor.abort_subscription_mutation actor ~owner ~receipt |> protocol_ok);
            stale_delivery_rejected
            := Result.is_error
                 (Actor.create_script_notification
                    actor
                    ~owner
                    ~source:Setup.source
                    ~correlation:
                      { key = "expired-subscription"
                      ; invocation_id = Some pending.context.invocation_id
                      ; work = Some (Subscription pending.context.id)
                      }
                    ~completion:(Succeeded (`String "late"))
                    ~wake:Request_turn);
            assert_same_session_snapshot before (Actor.state actor |> protocol_ok);
            Schedule_transaction_tests.save commit)
       |> protocol_ok
       |> ignore;
       let after = Actor.state actor |> protocol_ok in
       let winner =
         List.find_exn after.subscriptions ~f:(fun subscription ->
           P.Id.Subscription.equal subscription.context.id pending.context.id)
       in
       print_s
         [%sexp
           { expired : int
           ; stale_completion_rejected = (!stale_completion_rejected : bool)
           ; stale_delivery_rejected = (!stale_delivery_rejected : bool)
           ; expiry_retained =
               (Option.equal P.Completion.equal winner.result (Some Expired) : bool)
           ; no_delivery = (List.is_empty after.deliveries : bool)
           ; no_operation = (Option.is_none after.active_operation : bool)
           }]);
  [%expect
    {|
    ((expired 1) (stale_completion_rejected true) (stale_delivery_rejected true)
     (expiry_retained true) (no_delivery true) (no_operation true))
    |}]
;;
