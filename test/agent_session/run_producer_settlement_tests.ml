open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module Settlement = A.Run_producer_settlement
module Schedule_fixture = Run_schedule_settlement_tests

(* Actual admission supplies the retained Run/source. Following rows and their
   transitions are pure domain fixture DATA, never borrowed execution authority. *)
let with_fixture f =
  Schedule_fixture.with_admitted (fun state index run executing _schedule _work ->
    let template = invocation_fixture () in
    let invocation =
      P.Invocation.create
        ~observer:run.P.Run.source.observer
        ~parent_event:executing.P.Moderator_execution.context.id
        { template.context with
          session_id = state.A.Session_state.identity.session_id
        ; generation = state.identity.generation
        ; origin = Moderator
        }
      |> protocol_ok
    in
    let dispatched = P.Invocation.dispatch invocation |> protocol_ok in
    let subscription =
      P.Subscription.create
        { id = P.Id.Subscription.of_string "sub_owned_evidence" |> protocol_ok
        ; session_id = state.identity.session_id
        ; generation = state.identity.generation
        ; invocation_id = invocation.context.id
        ; source = Some run.P.Run.source.observer
        ; parent_job = None
        ; kind = "producer-evidence"
        ; created_at = timestamp
        ; deadline = P.Timestamp.of_string "2026-08-15T13:00:00Z" |> protocol_ok
        ; completion_schema = None
        ; wake = Request_turn
        ; ingress_capability = None
        }
      |> protocol_ok
    in
    let completed =
      P.Moderator_execution.complete
        executing
        ~checkpoint_sha256:executing.context.checkpoint_sha256
        ~requests:{ request_turn = false; request_compaction = false; end_session = None }
      |> protocol_ok
    in
    let work key =
      P.Run_work.create ~key:(Retained key) ~generation:run.source.generation
      |> protocol_ok
    in
    let delivery ?subscription:bound completion =
      let subscription_binding =
        Option.map bound ~f:(fun value ->
          P.Delivery.Subscription_binding.create
            ~subscription_id:value.P.Subscription.context.id
            ~epoch:value.epoch
          |> protocol_ok)
      in
      P.Delivery.create
        { id = P.Id.Delivery.of_string "dlv_owned_evidence" |> protocol_ok
        ; session_id = state.identity.session_id
        ; generation = state.identity.generation
        ; invocation_id =
            Option.map bound ~f:(fun value -> value.P.Subscription.context.invocation_id)
        ; work =
            Option.map bound ~f:(fun value ->
              P.Invocation.Subscription value.P.Subscription.context.id)
        ; correlation = "producer-evidence"
        ; source = Moderator
        ; completion
        ; wake = Request_turn
        ; created_at = timestamp
        ; ownership =
            Some
              { source = run.source.observer
              ; creator = Moderator_event executing.context.id
              ; subscription_binding
              }
        }
      |> protocol_ok
    in
    let resolved () =
      P.Invocation.resolve
        dispatched
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
        (Pending (Subscription subscription.context.id, `String "accepted"))
      |> protocol_ok
    in
    let prefix =
      A.Session_delta.
        [ Invocation_changed invocation
        ; Invocation_changed dispatched
        ; Subscription_changed subscription
        ]
    in
    f state index run executing completed subscription (resolved ()) prefix delivery work)
;;

let publication delivery =
  let id =
    History_entry.Id.create ~namespace:"producer-publication" ~sequence:0
    |> Result.ok_or_failwith
  in
  let entry = A.Notification_history.create ~id delivery |> protocol_ok in
  let committed =
    P.Delivery.commit ~track_wake:true delivery ~history_id:id ~now:timestamp
    |> protocol_ok
  in
  committed, A.Session_delta.Delivery_committed (committed, entry)
;;

let apply state delta =
  Settlement.prepare state ~delta ~now:timestamp
  |> protocol_ok
  |> A.Session_delta.apply state
  |> protocol_ok
;;

let run state id =
  Option.value_exn state.A.Session_state.run_state
  |> fun index -> A.Run_state.find index id |> Option.value_exn
;;

let checkpoint state (before : P.Run.t) executing ~delta ~owned_work ~action =
  A.Run_transition.prepare
    state
    ~scope:
      (A.Run_scope.create
         ~run:before
         ~execution_id:executing.P.Moderator_execution.context.id
       |> protocol_ok)
    ~executing
    ~work_delta:delta
    ~owned_work
    ~action:(Some action)
    ~now:timestamp
;;

let%expect_test
    "subscription winner and exact history publication share the callback revision"
  =
  with_fixture
    (fun
        state
         _index
         before
         executing
         completed
         subscription
         resolved
         prefix
         delivery
         work
       ->
       let armed =
         P.Subscription.arm subscription ~expected_epoch:0 ~timer_id:None ~job_id:None
         |> protocol_ok
       in
       let terminal, won =
         P.Subscription.finish
           armed
           ~expected_epoch:armed.epoch
           ~now:timestamp
           (Succeeded (`String "ready"))
         |> protocol_ok
       in
       assert won;
       let duplicate, won =
         P.Subscription.finish
           terminal
           ~expected_epoch:0
           ~now:timestamp
           (Failed { code = "late"; message = "late"; retryable = false; details = `Null })
         |> protocol_ok
       in
       assert ((not won) && P.Subscription.equal terminal duplicate);
       let notification =
         delivery ~subscription:terminal (Option.value_exn terminal.result)
       in
       let committed, commit = publication notification in
       let sub_work = work (Subscription subscription.context.id) in
       let delivery_work = work (Delivery notification.context.id) in
       let delta =
         A.Session_delta.Batch
           (prefix
            @ [ Subscription_changed armed
              ; Subscription_changed terminal
              ; Invocation_changed resolved
              ; Moderator_execution_changed completed
              ; Delivery_changed notification
              ; commit
              ])
       in
       let prepared =
         checkpoint
           state
           before
           executing
           ~delta
           ~owned_work:[ sub_work; delivery_work ]
           ~action:(Finish { terminal = Completed None; relinquish = [] })
         |> protocol_ok
       in
       let after = apply state (Batch [ delta; A.Run_transition.delta prepared ]) in
       let current = run after before.id in
       [%test_eq: int64] current.revision (Int64.succ before.revision);
       [%test_eq: int] (List.length current.terminal_work) 2;
       List.iter current.terminal_work ~f:(fun proof ->
         [%test_eq: int64] proof.revision current.revision;
         assert (P.Run_work.Terminal.equal_outcome proof.outcome Succeeded));
       assert (P.Run.Lifecycle.equal current.lifecycle (Terminal (Completed None)));
       (* Matching status/ID with a different epoch never proves this publication. *)
       let wrong =
         P.Delivery.create
           ?disclosure_pins:notification.disclosure_pins
           ?completion_projection:notification.completion_projection
           { notification.context with
             ownership =
               Option.map notification.context.ownership ~f:(fun owner ->
                 { owner with
                   subscription_binding =
                     Some
                       (P.Delivery.Subscription_binding.create
                          ~subscription_id:terminal.context.id
                          ~epoch:armed.epoch
                        |> protocol_ok)
                 })
           }
         |> protocol_ok
       in
       let wrong_committed, wrong_commit = publication wrong in
       let wrong_delta =
         A.Session_delta.Batch
           (prefix
            @ [ Subscription_changed armed
              ; Subscription_changed terminal
              ; Invocation_changed resolved
              ; Moderator_execution_changed completed
              ; Delivery_changed wrong
              ; wrong_commit
              ])
       in
       assert (
         Result.is_error
           (Settlement.capture
              state
              ~delta:wrong_delta
              ~run:before
              ~owned_work:[ sub_work; delivery_work ]));
       ignore (wrong_committed : P.Delivery.t);
       ignore (committed : P.Delivery.t));
  print_endline "first winner/epoch immutable; two producer proofs, one callback revision";
  [%expect {| first winner/epoch immutable; two producer proofs, one callback revision |}]
;;

let%expect_test "host expiry and cancellation retain distinct Subscription proof" =
  List.iter
    [ P.Completion.Expired, P.Run_work.Terminal.Limited; Cancelled "stopped", Cancelled ]
    ~f:(fun (completion, outcome) ->
      with_fixture
        (fun
            state
             index
             before
             _executing
             completed
             subscription
             resolved
             prefix
             _delivery
             work
           ->
           let state =
             A.Session_delta.apply
               state
               (Batch
                  (prefix
                   @ [ Invocation_changed resolved
                     ; Moderator_execution_changed completed
                     ]))
             |> protocol_ok
           in
           let owned_work = [ work (Subscription subscription.context.id) ] in
           let previous =
             P.Run.create
               ~id:before.P.Run.id
               ~session:before.session
               ~principal_id:before.principal_id
               ~source:before.source
               ~mode:before.mode
               ~lifecycle:Active
               ~revision:(Int64.succ before.revision)
               ~owned_work
               ~relinquished_work:[]
               ~terminal_work:[]
               ~created_at:before.created_at
               ~updated_at:timestamp
             |> protocol_ok
           in
           let index =
             A.Run_state.advance index ~run:previous ~intents:[] |> protocol_ok
           in
           let state = { state with A.Session_state.run_state = Some index } in
           A.Session_state.validate state |> protocol_ok;
           let now =
             match completion with
             | Expired -> subscription.context.deadline
             | Succeeded _ | Failed _ | Cancelled _ -> timestamp
           in
           let terminal, _ =
             P.Subscription.finish subscription ~expected_epoch:0 ~now completion
             |> protocol_ok
           in
           let delta =
             match completion with
             | Expired -> A.Session_delta.Subscription_expired terminal
             | Cancelled _ -> Subscription_cancelled terminal
             | Succeeded _ | Failed _ -> assert false
           in
           let delta = Settlement.prepare state ~delta ~now |> protocol_ok in
           let final = A.Session_delta.apply state delta |> protocol_ok in
           let current = run final previous.id in
           [%test_eq: int64] current.revision (Int64.succ previous.revision);
           [%test_eq: int] (List.length current.terminal_work) 1;
           assert (
             P.Run_work.Terminal.equal_outcome
               (List.hd_exn current.terminal_work).outcome
               outcome)));
  print_endline "host terminal facts keep Expired=Limited and Cancelled distinct";
  [%expect {| host terminal facts keep Expired=Limited and Cancelled distinct |}]
;;

let%expect_test "unrelated notification cannot finish an open subscription" =
  with_fixture
    (fun
        state
         _index
         before
         executing
         completed
         subscription
         resolved
         prefix
         delivery
         work
       ->
       let notification = delivery (Succeeded (`String "ordinary notification")) in
       let _, commit = publication notification in
       let delta =
         A.Session_delta.Batch
           (prefix
            @ [ Invocation_changed resolved
              ; Moderator_execution_changed completed
              ; Delivery_changed notification
              ; commit
              ])
       in
       let owned_work =
         [ work (Subscription subscription.context.id)
         ; work (Delivery notification.context.id)
         ]
       in
       let _, proofs =
         Settlement.capture state ~delta ~run:before ~owned_work |> protocol_ok
       in
       [%test_eq: int] (List.length proofs) 1;
       assert (
         P.Run_work.Key.equal
           (List.hd_exn proofs).work.key
           (Retained (Delivery notification.context.id)));
       assert (
         Result.is_error
           (checkpoint
              state
              before
              executing
              ~delta
              ~owned_work
              ~action:(Finish { terminal = Completed None; relinquish = [] }))));
  print_endline "publication proof does not terminalize open Subscription; Finish rejects";
  [%expect {| publication proof does not terminalize open Subscription; Finish rejects |}]
;;

let%expect_test "adverse subscription can wake; Delivery finality forbids owned reuse" =
  with_fixture
    (fun
        state
         _index
         before
         executing
         completed
         subscription
         resolved
         prefix
         delivery
         work
       ->
       let terminal, _ =
         P.Subscription.finish
           subscription
           ~expected_epoch:0
           ~now:timestamp
           (Failed
              { code = "result.failed"
              ; message = "reported failure"
              ; retryable = false
              ; details = `Null
              })
         |> protocol_ok
       in
       let notification =
         delivery ~subscription:terminal (Option.value_exn terminal.result)
       in
       let owned_work =
         [ work (Subscription subscription.context.id)
         ; work (Delivery notification.context.id)
         ]
       in
       let wake =
         P.Run_wake.create
           ~run_id:before.id
           ~source:before.source
           ~occurrence:
             (Subscription_delivery
                { subscription_id = terminal.context.id
                ; epoch = terminal.epoch
                ; delivery_id = notification.context.id
                ; creator = Moderator_event executing.context.id
                })
         |> protocol_ok
       in
       let delta =
         A.Session_delta.Batch
           (prefix
            @ [ Subscription_changed terminal
              ; Invocation_changed resolved
              ; Moderator_execution_changed completed
              ; Delivery_changed notification
              ])
       in
       let prepared =
         checkpoint state before executing ~delta ~owned_work ~action:(Wait wake)
         |> protocol_ok
       in
       let waiting = apply state (Batch [ delta; A.Run_transition.delta prepared ]) in
       let previous = run waiting before.id in
       assert (P.Run.Lifecycle.equal previous.lifecycle (Waiting wake));
       let committed, commit = publication notification in
       let published = apply waiting commit in
       let current = run published before.id in
       assert (P.Run.Lifecycle.equal current.lifecycle (Waiting wake));
       [%test_eq: int] (List.length current.terminal_work) 2;
       assert (
         Option.equal
           P.Delivery.equal_wake_disposition
           committed.wake_disposition
           (Some Pending_wake));
       let failure retryable =
         P.Delivery.fail
           notification
           { code = "publication.failed"
           ; message = "actual publication failure"
           ; retryable
           ; details = `Null
           }
         |> protocol_ok
       in
       let retryable = failure true in
       let retryable_state = apply waiting (Delivery_changed retryable) in
       let unresolved = run retryable_state before.id in
       assert (P.Run.equal unresolved previous);
       let retry = P.Delivery.retry retryable ~max_attempts:2 |> protocol_ok in
       ignore (apply retryable_state (Delivery_changed retry) : A.Session_state.t);
       let failed = failure false in
       let failed_state = apply waiting (Delivery_changed failed) in
       let retired = run failed_state before.id in
       assert (P.Run.Lifecycle.equal retired.lifecycle (Terminal Interrupted));
       assert (
         List.exists retired.terminal_work ~f:(fun proof ->
           P.Run_work.Key.equal
             proof.work.key
             (Retained (Delivery notification.context.id))
           && P.Run_work.Terminal.equal_outcome proof.outcome Failed));
       let retry = P.Delivery.retry failed ~max_attempts:2 |> protocol_ok in
       assert (
         Result.is_error
           (Settlement.prepare
              failed_state
              ~delta:(Delivery_changed retry)
              ~now:timestamp));
       (* Protocol retry still exists without immutable owned failure evidence. *)
       let restored =
         Settlement.prepare failed_state ~delta:(Created failed_state) ~now:timestamp
         |> protocol_ok
       in
       assert (
         match restored with
         | Created _ -> true
         | _ -> false);
       let reopened = { failed_state with A.Session_state.deliveries = [ retry ] } in
       assert (
         Result.is_error
           (Settlement.prepare failed_state ~delta:(Created reopened) ~now:timestamp));
       let unowned = { failed_state with A.Session_state.run_state = None } in
       ignore
         (Settlement.prepare unowned ~delta:(Delivery_changed retry) ~now:timestamp
          |> protocol_ok
          : A.Session_delta.t));
  print_endline
    "adverse result publication preserves Wait; retryable failure stays unsettled";
  print_endline
    "final awaited Delivery failure retires Wait; terminal owned ID cannot retry";
  [%expect
    {|
    adverse result publication preserves Wait; retryable failure stays unsettled
    final awaited Delivery failure retires Wait; terminal owned ID cannot retry |}]
;;

let%expect_test
    "actual Job cancellation and waited producers share one retirement revision"
  =
  Schedule_fixture.with_admitted (fun state index before executing schedule _ ->
    let queued =
      { (Job_inference_provenance_tests.job ~inference_origin:(Run before.P.Run.id) ()) with
        launch =
          Some
            { owner = Moderator_event executing.P.Moderator_execution.context.id
            ; parent_job = None
            ; nested_depth = 0
            ; moderator_source = Some before.source.observer
            }
      }
    in
    let running =
      { queued with status = Running; attempt = 1; started_at = Some timestamp }
    in
    let deadline = P.Timestamp.of_string "2026-08-15T13:00:00Z" |> protocol_ok in
    (* Pure nested call DATA: the observed child inherits the actual Job attempt
       through its unobserved script parent, never through conflicting parents. *)
    let parent =
      P.Invocation.create
        { (invocation_fixture ()).context with
          id = P.Id.Invocation.of_string "inv_job_companion_parent" |> protocol_ok
        ; session_id = state.identity.session_id
        ; generation = state.identity.generation
        ; parent_job = Some running.id
        ; deadline = Some deadline
        }
      |> protocol_ok
    in
    let parent_dispatched = P.Invocation.dispatch parent |> protocol_ok in
    let invocation =
      P.Invocation.create
        ~observer:before.source.observer
        { (invocation_fixture ()).context with
          session_id = state.identity.session_id
        ; generation = state.identity.generation
        ; parent_invocation = Some parent.context.id
        ; deadline = Some deadline
        }
      |> protocol_ok
    in
    let dispatched = P.Invocation.dispatch invocation |> protocol_ok in
    let subscription =
      P.Subscription.create
        { id = P.Id.Subscription.of_string "sub_job_companion" |> protocol_ok
        ; session_id = state.A.Session_state.identity.session_id
        ; generation = state.identity.generation
        ; invocation_id = invocation.context.id
        ; source = Some before.source.observer
        ; parent_job = Some (running.id, running.attempt)
        ; kind = "job-companion"
        ; created_at = timestamp
        ; deadline
        ; completion_schema = None
        ; wake = Request_turn
        ; ingress_capability = None
        }
      |> protocol_ok
    in
    let armed =
      P.Subscription.arm
        subscription
        ~expected_epoch:0
        ~timer_id:(Some schedule.P.Schedule.id)
        ~job_id:None
      |> protocol_ok
    in
    let unbound_schedule = schedule in
    let schedule =
      { schedule with
        ownership =
          Option.map schedule.ownership ~f:(fun owner ->
            { owner with subscription = Some (subscription.context.id, armed.epoch) })
      }
    in
    let resolved =
      P.Invocation.resolve
        dispatched
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
        (Pending (Subscription subscription.context.id, `String "accepted"))
      |> protocol_ok
    in
    let waiting =
      { running with
        status =
          Waiting_completion
            { invocation_id = invocation.context.id
            ; work = Subscription subscription.context.id
            ; deadline
            ; completion_schema = None
            ; max_output_bytes = 1024
            ; max_output_depth = 16
            }
      }
    in
    let completed =
      P.Moderator_execution.complete
        executing
        ~checkpoint_sha256:executing.context.checkpoint_sha256
        ~requests:{ request_turn = false; request_compaction = false; end_session = None }
      |> protocol_ok
    in
    let state =
      A.Session_delta.apply
        state
        (Batch
           [ Job_changed queued
           ; Job_changed running
           ; Invocation_changed parent
           ; Invocation_changed parent_dispatched
           ; Invocation_changed invocation
           ; Invocation_changed dispatched
           ; Subscription_changed subscription
           ; Schedule_changed unbound_schedule
           ; Subscription_changed armed
           ; Schedule_changed schedule
           ; Invocation_changed resolved
           ; Job_changed waiting
           ; Moderator_execution_changed completed
           ])
      |> protocol_ok
    in
    A.Job_dependency.validate
      ~invocations:state.invocations
      ~events:state.moderator_executions
      ~jobs:state.jobs
      ~subscriptions:state.subscriptions
      waiting
    |> protocol_ok;
    let work key =
      P.Run_work.create ~key:(Retained key) ~generation:before.source.generation
      |> protocol_ok
    in
    let owned_work =
      [ A.Run_job_occurrence.work waiting |> protocol_ok
      ; work (Subscription subscription.context.id)
      ; work (Schedule schedule.id)
      ]
    in
    let wake =
      P.Run_wake.create
        ~run_id:before.id
        ~source:before.source
        ~occurrence:
          (Delivered_timer
             { schedule_id = schedule.id
             ; delivery_count = 1
             ; creator = Moderator_event executing.context.id
             ; subscription = Some (subscription.context.id, armed.epoch)
             })
      |> protocol_ok
    in
    let previous =
      P.Run.create
        ~id:before.id
        ~session:before.session
        ~principal_id:before.principal_id
        ~source:before.source
        ~mode:before.mode
        ~lifecycle:(Waiting wake)
        ~revision:(Int64.succ before.revision)
        ~owned_work
        ~relinquished_work:[]
        ~terminal_work:[]
        ~created_at:before.created_at
        ~updated_at:timestamp
      |> protocol_ok
    in
    let index = A.Run_state.advance index ~run:previous ~intents:[] |> protocol_ok in
    let state = { state with run_state = Some index } in
    A.Session_state.validate state |> protocol_ok;
    let terminal =
      { waiting with
        status = Cancelled
      ; completed_at = Some timestamp
      ; result = Some (P.Completion.to_json (Cancelled "actual owning Job cancellation"))
      }
    in
    let finished, _ =
      P.Subscription.finish
        armed
        ~expected_epoch:armed.epoch
        ~now:timestamp
        (Cancelled "actual owning Job cancellation")
      |> protocol_ok
    in
    let producer_delta =
      A.Session_delta.Batch
        [ Subscription_changed finished
        ; Schedule_changed { schedule with status = Cancelled }
        ]
    in
    let run_deltas =
      A.Run_job_completion.prepare
        ~work_delta:producer_delta
        state
        ~running:waiting
        ~terminal
        ~successor:terminal
        ~outcome:Cancelled
        ~now:timestamp
      |> protocol_ok
    in
    let final =
      apply
        state
        (Batch ([ A.Session_delta.Job_changed terminal; producer_delta ] @ run_deltas))
    in
    let current = run final previous.id in
    [%test_eq: int64] current.revision (Int64.succ previous.revision);
    [%test_eq: int] (List.length current.terminal_work) 3;
    assert (P.Run.Lifecycle.equal current.lifecycle (Terminal Interrupted));
    List.iter current.terminal_work ~f:(fun proof ->
      [%test_eq: int64] proof.revision current.revision;
      assert (P.Run_work.Terminal.equal_outcome proof.outcome Cancelled)));
  print_endline
    "Job + Subscription + timer cancellation: three proofs, one exact Wait retirement";
  [%expect
    {| Job + Subscription + timer cancellation: three proofs, one exact Wait retirement |}]
;;
