open Core
open Fixtures

let%expect_test "external event delivery commits receipts before changing the live queue" =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  let module B = Agent_session.Runtime_builder in
  let module S = Session.Moderator_state.Identity_snapshot in
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let manager, _ = handoff_manager env in
      let snapshot () = M.identity_snapshot manager |> Result.ok_or_failwith in
      let initial =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let schedule () : Agent_protocol.Schedule.t =
        { id = Agent_protocol.Id.Schedule.create ()
        ; session_id
        ; generation = 0
        ; payload = `Null
        ; created_at = timestamp
        ; next_due_at = timestamp
        ; misfire = Deliver_once_immediately
        ; status = Delivering
        ; delivery_count = 0
        ; last_delivery_at = None
        ; inference_provenance = None
        ; delivery_cancellation = None
        ; ownership = None
        }
      in
      let first = schedule ()
      and second = schedule ()
      and third = schedule () in
      let job : Agent_protocol.Job.t =
        { id = Agent_protocol.Id.Job.create ()
        ; session_id
        ; generation = 0
        ; kind = Model_call
        ; payload = `Null
        ; status = Succeeded
        ; retry_policy = Never
        ; attempt = 1
        ; created_at = timestamp
        ; started_at = Some timestamp
        ; next_run_at = None
        ; completed_at = Some timestamp
        ; result = Some `Null
        ; delivery = Pending
        ; launch = None
        ; inference_origin = None
        ; progress = None
        }
      in
      let initial =
        { initial with
          moderator = Some (B.encode_moderator_snapshot (snapshot ()))
        ; schedules = [ first; second; third ]
        ; jobs = [ job ]
        ; model_job_targets = [ model_job_binding initial job ]
        }
      in
      let backend =
        Agent_session.Memory_backend.create ~event_capacity:128 ~initial_state:initial
      in
      let persistence = Agent_session.Memory_backend.persistence backend in
      let reject = ref false in
      let actor =
        A.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:initial
          ~operation_worker:None
          ~persistence:
            { archive_reference
            ; commit =
                (fun ~command_audit ~previous next ->
                  match !reject with
                  | true ->
                    reject := false;
                    Error (handoff_error "delivery save rejected")
                  | false -> persistence.commit ~command_audit ~previous next)
            }
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id = Agent_protocol.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "queue-delivery")
            ; job_results = None
            ; monotonic_now = (fun () -> Mtime.min_stamp)
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; notification_limits = Agent_session.Staged_notifications.default_limits
            ; ingress_limits = Agent_session.Staged_ingress.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      Exn.protect
        ~finally:(fun () -> A.shutdown actor)
        ~f:(fun () ->
          let same = Option.equal Jsonaf.exactly_equal in
          let current_matches () =
            same
              (Some (B.encode_moderator_snapshot (snapshot ())))
              (A.state actor |> protocol_ok).moderator
          in
          let append ?(before_commit = fun () -> ()) label save =
            A.with_moderator_checkpoint actor (fun () ->
              let event =
                Chat_response.Moderator_invocation.internal_event
                  (Chatml.Chatml_lang.VVariant ("String", [ VString label ]))
                |> Result.ok_or_failwith
              in
              M.enqueue_internal_event_entries
                manager
                ~event
                ~prepare:(fun ~before ~snapshot ->
                  before_commit ();
                  save ~before ~snapshot
                  |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message))
              |> Result.map_error ~f:handoff_error)
          in
          let save_schedule schedule ~before ~snapshot =
            A.complete_schedule
              ~expected:before
              ~expected_schedule:schedule
              actor
              ~schedule_id:schedule.Agent_protocol.Schedule.id
              ~generation:0
              ~moderator_snapshot:(Some (B.encode_moderator_snapshot snapshot))
            |> Result.map ~f:ignore
          in
          let save_job expected_job ~before ~snapshot =
            A.deliver_job
              ~expected:before
              ~expected_job
              actor
              ~job_id:job.id
              ~generation:0
              ~moderator_snapshot:(Some (B.encode_moderator_snapshot snapshot))
            |> Result.map ~f:ignore
          in
          let unchanged = snapshot () in
          assert (
            Result.is_error
              (append
                 "stale-timer"
                 (save_schedule { first with payload = `String "different payload" })));
          assert (
            Result.is_error
              (append
                 "stale-job"
                 (save_job
                    { job with attempt = 0; result = Some (`String "previous attempt") })));
          assert (Sexp.equal (S.sexp_of_t unchanged) (S.sexp_of_t (snapshot ())));
          assert (current_matches ());
          List.iter
            [ "schedule", save_schedule first; "job", save_job job ]
            ~f:(fun (label, save) ->
              let before = snapshot () in
              reject := true;
              assert (Result.is_error (append label save));
              assert (Sexp.equal (S.sexp_of_t before) (S.sexp_of_t (snapshot ())));
              assert (current_matches ());
              append label save |> protocol_ok |> ignore;
              let delivered = snapshot () in
              assert (Result.is_error (append label save));
              assert (Sexp.equal (S.sexp_of_t delivered) (S.sexp_of_t (snapshot ())));
              assert (current_matches ()));
          let entered, entered_u = Eio.Promise.create () in
          let release, release_u = Eio.Promise.create () in
          let queued, queued_u = Eio.Promise.create () in
          let first_done, first_done_u = Eio.Promise.create () in
          let second_done, second_done_u = Eio.Promise.create () in
          let second_prepared = ref false in
          Eio.Fiber.fork ~sw (fun () ->
            let result =
              append "concurrent-first" (save_schedule second) ~before_commit:(fun () ->
                Eio.Promise.resolve entered_u ();
                Eio.Promise.await release)
            in
            Eio.Promise.resolve first_done_u result);
          Eio.Promise.await entered;
          Eio.Fiber.fork ~sw (fun () ->
            Eio.Promise.resolve queued_u ();
            let result =
              append "concurrent-second" (save_schedule third) ~before_commit:(fun () ->
                second_prepared := true)
            in
            Eio.Promise.resolve second_done_u result);
          Eio.Promise.await queued;
          Eio.Fiber.yield ();
          assert (not !second_prepared);
          let responsive = (A.state actor |> protocol_ok).moderator in
          assert (same responsive (Agent_session.Memory_backend.state backend).moderator);
          Eio.Promise.resolve release_u ();
          Eio.Promise.await first_done |> protocol_ok |> ignore;
          Eio.Promise.await second_done |> protocol_ok |> ignore;
          let final = A.state actor |> protocol_ok in
          print_s
            [%sexp
              { queue_length = (List.length (snapshot ()).queued_internal_events : int)
              ; checkpoints_match = (current_matches () : bool)
              ; deliveries =
                  (List.map final.schedules ~f:(fun s -> s.delivery_count) : int list)
              ; job_delivered =
                  ((match (List.hd_exn final.jobs).delivery with
                    | Delivered _ -> true
                    | Pending | Not_required | Discarded _ -> false)
                   : bool)
              ; second_prepared = (!second_prepared : bool)
              }])));
  [%expect
    {|
    ((queue_length 4) (checkpoints_match true) (deliveries (1 1 1))
     (job_delivered true) (second_prepared true))
    |}]
;;

let%expect_test "two session event owners reject a wait cycle and release both borrows" =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  with_actor_workspace (fun env workspace_instance ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      Eio.Switch.run (fun sw ->
        let actors =
          Array.init 2 ~f:(fun index ->
            let manager, _ = handoff_manager env in
            let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
            let initial =
              actor_state ~workspace_instance ~liveness:Detached ~start_immediately:true
            in
            let initial =
              { initial with
                identity =
                  { initial.identity with
                    session_id =
                      (match index with
                       | 0 -> session_id
                       | _ -> second_session_id)
                  }
              ; inference_ledger =
                  fresh_inference_ledger
                    ~session_id:
                      (if Int.equal index 0 then session_id else second_session_id)
                    ~generation:initial.identity.generation
              ; lifecycle = { desired = Running; observed = Idle }
              ; moderator =
                  Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)
              }
            in
            let backend =
              Agent_session.Memory_backend.create
                ~event_capacity:32
                ~initial_state:initial
            in
            let actor =
              A.create
                ~sw
                ~clock:(Eio.Stdenv.clock env)
                ~mailbox_capacity:16
                ~compaction_env:None
                ~initial_state:initial
                ~operation_worker:None
                ~persistence:(Agent_session.Memory_backend.persistence backend)
                ~services:
                  { now = (fun () -> timestamp)
                  ; create_attachment_id = Agent_protocol.Id.Attachment.create
                  ; create_reclaim_token = (fun () -> "cycle-test")
                  ; job_results = None
                  ; monotonic_now = (fun () -> Mtime.min_stamp)
                  ; schedule_limits = Agent_session.Staged_schedules.default_limits
                  ; notification_limits =
                      Agent_session.Staged_notifications.default_limits
                  ; ingress_limits = Agent_session.Staged_ingress.default_limits
                  ; subscription_limits =
                      Agent_session.Staged_subscriptions.default_limits
                  ; state_committed = (fun _ _ -> ())
                  }
            in
            actor, snapshot)
        in
        let held = Array.init 2 ~f:(fun _ -> Eio.Promise.create ()) in
        let proceed = Array.init 2 ~f:(fun _ -> Eio.Promise.create ()) in
        let finished = Array.init 2 ~f:(fun _ -> Eio.Promise.create ()) in
        Exn.protect
          ~finally:(fun () ->
            Array.iter proceed ~f:(fun (promise, resolver) ->
              if Option.is_none (Eio.Promise.peek promise)
              then Eio.Promise.resolve resolver ());
            Array.iter actors ~f:(fun (actor, _) -> A.shutdown actor))
          ~f:(fun () ->
            Array.iteri actors ~f:(fun index (actor, snapshot) ->
              Eio.Fiber.fork ~sw (fun () ->
                let result = ref "missing" in
                let claimed =
                  A.with_current_moderator_event
                    actor
                    ~operation_id:None
                    ~event:Session_start
                    ~snapshot:(fun () -> Ok snapshot)
                    (fun ~executing:_ ~retirement_reason:_ ~event:_ ~execute:_ ~commit ->
                       Eio.Promise.resolve (snd held.(index)) ();
                       Eio.Promise.await (fst proceed.(index));
                       let remote, _ = actors.((index + 1) mod 2) in
                       (result
                        := match A.with_moderator_checkpoint remote (fun () -> Ok ()) with
                           | Ok () -> "acquired"
                           | Error error -> error.message);
                       commit
                         ~snapshot
                         ~requests:
                           { request_turn = false
                           ; request_compaction = false
                           ; end_session = None
                           })
                  |> protocol_ok
                in
                assert claimed;
                Eio.Promise.resolve (snd finished.(index)) !result));
            Array.iter held ~f:(fun (promise, _) -> Eio.Promise.await promise);
            (* Both mailboxes remain responsive while independent event borrows are held. *)
            Array.iter actors ~f:(fun (actor, _) ->
              let state = A.state actor |> protocol_ok in
              assert (List.length state.moderator_executions = 1));
            Array.iter proceed ~f:(fun (_, resolver) ->
              Eio.Promise.resolve resolver ();
              Eio.Fiber.yield ());
            let outcomes =
              Array.to_list finished
              |> List.map ~f:(fun (promise, _) -> Eio.Promise.await promise)
            in
            let completed =
              Array.to_list actors
              |> List.map ~f:(fun (actor, _) ->
                A.with_moderator_checkpoint actor (fun () -> Ok ()) |> protocol_ok;
                List.for_all
                  (A.state actor |> protocol_ok).moderator_executions
                  ~f:(fun receipt ->
                    match receipt.status with
                    | Completed _ -> true
                    | _ -> false))
            in
            print_s [%sexp { outcomes : string list; completed : bool list }]))));
  [%expect
    {|
    ((outcomes
      (acquired
       "moderator_wait_cycle: synchronous call would create an owner wait cycle"))
     (completed (true true)))
    |}]
;;

let%expect_test "queued events retain actor ownership through checkpoint installation" =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  let module E = Agent_protocol.Moderator_execution in
  let module S = Session.Moderator_state.Identity_snapshot in
  List.iter
    [ `Commit
    ; `Concurrent
    ; `Claim_rejected
    ; `Commit_rejected
    ; `Terminal_rejected
    ; `Stop_cancel
    ; `Bad_tail
    ]
    ~f:(fun mode ->
      let prepared = ref None
      and effects = ref 0
      and rejected = ref false in
      with_handoff_actor
        ~reject:(fun next ->
          let matches =
            List.exists
              next.Agent_session.Session_transition.state.moderator_executions
              ~f:(fun receipt ->
                match mode, receipt.status with
                | `Claim_rejected, Running
                | `Commit_rejected, Completed _
                | `Terminal_rejected, (Completed _ | Failed _) -> true
                | _ -> false)
          in
          match
            matches
            &&
            match mode with
            | `Terminal_rejected -> true
            | _ -> not !rejected
          with
          | true ->
            rejected := true;
            true
          | false -> false)
        ~make_worker:(fun env _ ->
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let manager, _, _ =
              handoff_definition
                env
                ~events:
                  {| | `Session_start ->
                    Task.bind(Runtime.emit(`String("first")), fun ignored ->
                    Task.bind(Runtime.emit(`String("tail")), fun ignored -> Task.pure(state)))
                   | `Internal_event(payload) ->
                    let ignored = state[0] <- state[0] + 1 in
                    Task.bind(Tool.call("probe", payload), fun ignored ->
                    Task.bind(Runtime.emit(`String("new")), fun ignored ->
                    Task.bind(Runtime.request_turn(), fun ignored -> Task.pure(state))))
                   | _ -> Task.pure(state) |}
            in
            M.handle_event_entries_transactional
              manager
              ~session_id:"fixture"
              ~now_ms:0
              ~history:[]
              ~available_tools:[]
              ~session_meta:`Null
              ~event:Session_start
              ~authorize:(fun () -> Ok ())
              ~on_tool_call:(fun ~name:_ ~args:_ -> assert false)
              ~prepare_event:(fun ~outcome:_ ~snapshot:_ -> Ok (M.memory_commit ignore))
            |> Result.ok_or_failwith
            |> ignore;
            let before = M.identity_snapshot manager |> Result.ok_or_failwith in
            let encoded =
              Some (Agent_session.Runtime_builder.encode_moderator_snapshot before)
            in
            caps.commit_moderator encoded |> protocol_ok;
            prepared := Some (manager, before);
            Completed
              { final_history = input.history
              ; moderator_snapshot = encoded
              ; runtime_requests = []
              }))
        (fun _env actor writer backend ->
           let initial = await_idle actor in
           let manager, before = Option.value_exn !prepared in
           let escaped = ref None in
           let run () =
             A.with_idle_queued_moderator_event
               actor
               ~snapshot:before
               (fun ~event:selected ~commit ->
                  let claimed = A.state actor |> protocol_ok in
                  assert (Option.is_none claimed.active_operation);
                  assert (
                    List.exists claimed.moderator_executions ~f:(fun receipt ->
                      match receipt.status with
                      | Running -> true
                      | _ -> false));
                  escaped := Some commit;
                  M.handle_next_event_entries_transactional
                    manager
                    ~session_id:
                      (Agent_protocol.Id.Session.to_string initial.identity.session_id)
                    ~now_ms:0
                    ~history:[]
                    ~available_tools:[]
                    ~session_meta:`Null
                    ~authorize:(fun ~event ->
                      assert (
                        Sexp.equal
                          (Session.Snapshot.sexp_of_t selected)
                          (Session.Snapshot.sexp_of_t event));
                      Ok ())
                    ~on_tool_call:(fun ~name:_ ~args ->
                      assert (Jsonaf.exactly_equal args (`String "first"));
                      incr effects;
                      assert (Result.is_error (A.change_moderator actor None));
                      (match mode with
                       | `Stop_cancel ->
                         A.stop actor ~attachment_id:writer.id ~mode:Cancel
                         |> protocol_ok
                         |> ignore;
                         Eio.Fiber.yield ()
                       | _ -> ());
                      Ok (Tool_ok `Null))
                    ~prepare_event:(fun ~outcome ~snapshot ->
                      let requests : Agent_protocol.Invocation.follow_up =
                        { request_turn =
                            Chat_response.Runtime_semantics.request_turn
                              outcome.runtime_requests
                        ; request_compaction = false
                        ; end_session = None
                        }
                      in
                      let snapshot =
                        match mode with
                        | `Bad_tail -> { snapshot with queued_internal_events = [] }
                        | _ -> snapshot
                      in
                      Ok
                        { M.persist =
                            (fun () ->
                              commit ~snapshot ~requests
                              |> Result.map_error ~f:(fun e ->
                                e.Agent_protocol.Error.message)
                              |> Result.map ~f:(fun () ->
                                (* The durable checkpoint is installed; the live manager has
                         not installed it yet. Ownership must cover this gap. *)
                                assert (Result.is_error (A.change_moderator actor None));
                                assert (
                                  Option.is_none
                                    (A.claim_idle_moderator actor |> protocol_ok));
                                ()))
                        ; install = ignore
                        })
                  |> Result.map ~f:ignore
                  |> Result.map_error ~f:handoff_error)
           in
           let safe_run () =
             try run () with
             | Eio.Cancel.Cancelled _ -> Error (handoff_error "cancelled")
           in
           let results = ref [] in
           (match mode with
            | `Concurrent ->
              Eio.Fiber.both
                (fun () ->
                   let result = safe_run () in
                   results := result :: !results)
                (fun () ->
                   let result = safe_run () in
                   results := result :: !results)
            | _ -> results := [ safe_run () ]);
           let saved = A.state actor |> protocol_ok in
           let live = M.identity_snapshot manager |> Result.ok_or_failwith in
           assert (
             Option.equal
               Jsonaf.exactly_equal
               saved.moderator
               (Some (Agent_session.Runtime_builder.encode_moderator_snapshot live)));
           assert (
             List.length saved.conversation.canonical_history
             = List.length initial.conversation.canonical_history);
           assert (Option.is_none saved.active_operation);
           assert (
             List.equal
               E.equal
               saved.moderator_executions
               (Agent_session.Memory_backend.state backend).moderator_executions);
           Option.iter !escaped ~f:(fun commit ->
             assert (
               Result.is_error
                 (commit
                    ~snapshot:live
                    ~requests:
                      { request_turn = false
                      ; request_compaction = false
                      ; end_session = None
                      })));
           (match mode with
            | `Terminal_rejected ->
              assert (
                match run () with
                | Ok false -> true
                | _ -> false);
              let restored = restore_state saved |> store_ok in
              let plan =
                Agent_session.Invocation_recovery.plan
                  ~state:restored
                  ~namespace:"queued-event-recovery"
                  ~first_sequence:0
                  ~reason:"restart"
                |> protocol_ok
              in
              let recovered =
                List.fold_result
                  plan.deltas
                  ~init:restored
                  ~f:Agent_session.Session_delta.apply
                |> protocol_ok
              in
              assert (
                List.for_all recovered.moderator_executions ~f:(fun receipt ->
                  match receipt.status with
                  | Interrupted _ -> true
                  | _ -> false));
              assert (
                Result.is_error
                  (Agent_session.Queued_moderator_event.claim
                     ~state:recovered
                     ~id:(Agent_protocol.Id.Moderator_execution.create ())
                     ~snapshot:before
                     ~now:timestamp));
              assert (!effects = 1)
            | _ -> ());
           (match mode with
            | `Commit_rejected | `Bad_tail ->
              assert (Sexp.equal (S.sexp_of_t before) (S.sexp_of_t live));
              assert (Result.is_error (run ()));
              let changed = { before with revision = before.revision + 1 } in
              A.change_moderator
                actor
                (Some (Agent_session.Runtime_builder.encode_moderator_snapshot changed))
              |> protocol_ok
              |> ignore;
              assert (
                Result.is_error
                  (A.with_idle_queued_moderator_event
                     actor
                     ~snapshot:changed
                     (fun ~event:_ ~commit:_ -> assert false)));
              assert (!effects = 1)
            | _ -> ());
           let statuses =
             List.map saved.moderator_executions ~f:(fun receipt ->
               match receipt.status with
               | Completed _ -> "completed"
               | Failed _ -> "failed"
               | Interrupted _ -> "interrupted"
               | Running -> "running")
           in
           let pending =
             List.count saved.moderator_executions ~f:(fun receipt ->
               match receipt.intent with
               | Some Pending -> true
               | _ -> false)
           in
           print_s
             [%sexp
               (mode
                : [ `Commit
                  | `Concurrent
                  | `Claim_rejected
                  | `Commit_rejected
                  | `Terminal_rejected
                  | `Stop_cancel
                  | `Bad_tail
                  ])
             , (List.count !results ~f:(function
                  | Ok true -> true
                  | _ -> false)
                : int)
             , (!effects : int)
             , (statuses : string list)
             , (pending : int)]));
  [%expect
    {|
    (Commit 1 1 (completed) 1)
    (Concurrent 1 1 (completed) 1)
    (Claim_rejected 0 0 () 0)
    (Commit_rejected 0 1 (failed) 0)
    +queued moderator cleanup failed: injected invocation save failure
    (Terminal_rejected 0 1 (running) 0)
    (Stop_cancel 0 1 (interrupted) 0)
    (Bad_tail 0 1 (failed) 0)
    |}]
;;

let%expect_test
    "failed queue retirement preserves failure and advances exactly one occurrence"
  =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  let module E = Agent_protocol.Moderator_execution in
  let module S = Session.Moderator_state.Identity_snapshot in
  let prepared = ref None
  and effects = ref 0
  and reject_retirement = ref true in
  with_handoff_actor
    ~reject:(fun next ->
      match
        !reject_retirement
        && List.exists
             next.Agent_session.Session_transition.state.moderator_executions
             ~f:(fun receipt -> Option.is_some receipt.retirement)
      with
      | true ->
        reject_retirement := false;
        true
      | false -> false)
    ~make_worker:(fun env _ ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let manager, _, _ =
          handoff_definition
            env
            ~events:
              {| | `Session_start ->
              Task.bind(Runtime.emit(`String("same")), fun ignored ->
              Task.bind(Runtime.emit(`String("same")), fun ignored -> Task.pure(state)))
             | `Internal_event(payload) ->
              let ignored = state[0] <- state[0] + 1 in
              Task.bind(Tool.call("probe", payload), fun ignored -> Task.pure(state))
             | _ -> Task.pure(state) |}
        in
        M.handle_event_entries_transactional
          manager
          ~session_id:"fixture"
          ~now_ms:0
          ~history:[]
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Session_start
          ~authorize:(fun () -> Ok ())
          ~on_tool_call:(fun ~name:_ ~args:_ -> assert false)
          ~prepare_event:(fun ~outcome:_ ~snapshot:_ -> Ok (M.memory_commit ignore))
        |> Result.ok_or_failwith
        |> ignore;
        let before = M.identity_snapshot manager |> Result.ok_or_failwith in
        let snapshot =
          Some (Agent_session.Runtime_builder.encode_moderator_snapshot before)
        in
        caps.commit_moderator snapshot |> protocol_ok;
        prepared := Some (manager, before);
        Completed
          { final_history = input.history
          ; moderator_snapshot = snapshot
          ; runtime_requests = []
          }))
    (fun _env actor writer backend ->
       let initial = await_idle actor in
       let manager, before = Option.value_exn !prepared in
       let current () = M.identity_snapshot manager |> Result.ok_or_failwith in
       let same a b = Sexp.equal (S.sexp_of_t a) (S.sexp_of_t b) in
       let execute ~fail snapshot =
         A.with_idle_queued_moderator_event actor ~snapshot (fun ~event:_ ~commit ->
           M.handle_next_event_entries_transactional
             manager
             ~session_id:"fixture"
             ~now_ms:0
             ~history:[]
             ~available_tools:[]
             ~session_meta:`Null
             ~authorize:(fun ~event ->
               assert (
                 Sexp.equal
                   (Session.Snapshot.sexp_of_t event)
                   (Session.Snapshot.sexp_of_t
                      (List.hd_exn snapshot.queued_internal_events)));
               Ok ())
             ~on_tool_call:(fun ~name:_ ~args ->
               assert (Jsonaf.exactly_equal args (`String "same"));
               incr effects;
               match fail with
               | true -> Error "effect finished but result failed"
               | false -> Ok (Tool_ok `Null))
             ~prepare_event:(fun ~outcome:_ ~snapshot ->
               Ok
                 { M.persist =
                     (fun () ->
                       commit
                         ~snapshot
                         ~requests:
                           { request_turn = false
                           ; request_compaction = false
                           ; end_session = None
                           }
                       |> Result.map_error ~f:(fun error ->
                         error.Agent_protocol.Error.message))
                 ; install = ignore
                 })
           |> Result.map ~f:ignore
           |> Result.map_error ~f:handoff_error)
       in
       assert (Result.is_error (execute ~fail:true before));
       let failed_state = A.state actor |> protocol_ok in
       let failed = List.hd_exn failed_state.moderator_executions in
       assert (same before (current ()) && !effects = 1);
       let escaped = ref None in
       let cancel_retirement = ref false in
       let retire () =
         A.with_queued_moderator_retirement
           actor
           ~id:failed.context.id
           ~snapshot:before
           ~reason:"operator discarded failed delivery"
           (fun ~event:_ ~commit ->
              escaped := Some commit;
              M.retire_queued_event_entries
                manager
                ~expected:before
                ~prepare:(fun ~snapshot ->
                  (match !cancel_retirement with
                   | true ->
                     A.stop actor ~attachment_id:writer.id ~mode:Cancel
                     |> protocol_ok
                     |> ignore;
                     Eio.Fiber.yield ()
                   | false -> ());
                  commit ~snapshot
                  |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
                  |> Result.map ~f:(fun () ->
                    assert (Result.is_error (A.change_moderator actor None));
                    fun () -> ()))
              |> Result.map_error ~f:handoff_error)
       in
       assert (Result.is_error (retire ()));
       assert (same before (current ()) && !effects = 1);
       let rejected = A.state actor |> protocol_ok in
       assert (List.equal E.equal rejected.moderator_executions [ failed ]);
       cancel_retirement := true;
       let cancelled =
         try retire () with
         | Eio.Cancel.Cancelled _ -> Error (handoff_error "cancelled")
       in
       assert (Result.is_error cancelled && same before (current ()) && !effects = 1);
       assert (
         List.equal E.equal (A.state actor |> protocol_ok).moderator_executions [ failed ]);
       cancel_retirement := false;
       let expected =
         { before with
           queued_internal_events = List.tl_exn before.queued_internal_events
         }
       in
       assert (Result.is_error ((Option.value_exn !escaped) ~snapshot:expected));
       let changed = { before with revision = before.revision + 1 } in
       assert (
         Result.is_error
           (A.with_queued_moderator_retirement
              actor
              ~id:failed.context.id
              ~snapshot:changed
              ~reason:"stale"
              (fun ~event:_ ~commit:_ -> assert false)));
       assert (
         Result.is_error
           (M.retire_queued_event_entries
              manager
              ~expected:changed
              ~prepare:(fun ~snapshot:_ -> assert false)));
       A.stop actor ~attachment_id:writer.id ~mode:Graceful |> protocol_ok |> ignore;
       assert (retire () |> protocol_ok);
       assert (same expected (current ()) && !effects = 1);
       let retired_state = A.state actor |> protocol_ok in
       let retired = List.hd_exn retired_state.moderator_executions in
       assert (E.equal_status failed.status retired.status);
       assert (Option.is_some retired.retirement);
       assert (Result.is_error (retire ()));
       let decoded = E.of_json (E.to_json retired) |> protocol_ok in
       assert (E.equal retired decoded);
       let v1 record =
         match E.to_json record with
         | `Object fields ->
           `Object
             (List.Assoc.add fields ~equal:String.equal "schema_version" (`Number "1"))
         | _ -> assert false
       in
       assert (E.equal failed (E.of_json (v1 failed) |> protocol_ok));
       assert (Result.is_error (E.of_json (v1 retired)));
       assert (
         Result.is_error
           (E.retire retired ~checkpoint_sha256:(String.make 64 'a') ~reason:"again"));
       let legacy = { failed_state with schema_version = 5 } in
       assert (Result.is_error (restore_state legacy));
       let migrated = restore_state failed_state |> store_ok in
       assert (List.equal E.equal migrated.moderator_executions [ failed ]);
       assert (
         Result.is_error
           (Agent_session.Session_state.upgrade_schema
              { retired_state with schema_version = 5 }));
       let restored = restore_state retired_state |> store_ok in
       assert_same_session_snapshot restored retired_state;
       let projection = Agent_session.Session_state.extension_status restored in
       let projection_json =
         `Array (List.map projection ~f:Agent_protocol.Extension_status.to_json)
       in
       assert (
         List.length
           (Agent_protocol.Extension_status.list_of_json projection_json |> protocol_ok)
         = 1);
       assert (
         not
           (String.is_substring (Jsonaf.to_string projection_json) ~substring:"operator"));
       A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
       assert (execute ~fail:false (current ()) |> protocol_ok);
       let final = A.state actor |> protocol_ok in
       assert (!effects = 2 && List.is_empty (current ()).queued_internal_events);
       assert (Option.is_none final.active_operation);
       assert (
         List.length final.conversation.canonical_history
         = List.length initial.conversation.canonical_history);
       assert_same_session_snapshot final (Agent_session.Memory_backend.state backend);
       let statuses =
         List.map final.moderator_executions ~f:(fun receipt ->
           (Agent_protocol.Extension_status.moderator_execution receipt).state)
         |> List.sort ~compare:String.compare
       in
       print_s
         [%sexp
           (statuses : string list)
         , (!effects : int)
         , (List.length (current ()).queued_internal_events : int)]);
  [%expect {| ((completed failed.retired) 2 0) |}]
;;

let%expect_test "event-owned native calls retain lineage and expire with their callback" =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let module M = Chat_response.Moderator_manager in
  let module C = Chat_response.Tool_capability in
  List.iter [ `Success; `Deny; `Save_fail; `Cancel ] ~f:(fun mode ->
    let calls = ref 0
    and saved = ref None
    and rejected = ref false in
    let on_call = ref (fun () -> ()) in
    let registry = native_registry calls ~raises:false ~on_call:(fun () -> !on_call ()) in
    let reference = List.hd_exn (C.references registry) in
    with_handoff_actor
      ~reject:(fun next ->
        let matches =
          List.exists
            next.Agent_session.Session_transition.state.invocations
            ~f:(fun invocation ->
              Option.is_some invocation.parent_event
              &&
              match invocation.status with
              | Resolved (Complete _) -> true
              | _ -> false)
        in
        match mode, matches, !rejected with
        | `Save_fail, true, false ->
          rejected := true;
          true
        | _ -> false)
      ~make_worker:(fun env _ ->
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let manager, _, _ =
            handoff_definition
              env
              ~capability_registry:registry
              ~declare_tool:false
              ~events:
                {| | `Session_start -> Task.bind(Runtime.emit(`Null), fun ignored -> Task.pure(state))
                        | `Internal_event(payload) -> Task.bind(Tool.call("read_file", `Object([])), fun ignored -> Task.pure(state))
                        | `Tool_observed(p) -> (match p.parent_event with
                          | `Some(id) -> (match p.parent_invocation with
                            | `None -> let ignored = state[0] <- state[0] + 100 in Task.pure(state)
                            | _ -> Task.fail("unexpected invocation parent"))
                          | _ -> Task.fail("missing event parent"))
                        | _ -> Task.pure(state) |}
          in
          M.handle_event_entries_transactional
            manager
            ~session_id:"fixture"
            ~now_ms:0
            ~history:[]
            ~available_tools:[]
            ~session_meta:`Null
            ~event:Session_start
            ~authorize:(fun () -> Ok ())
            ~on_tool_call:(fun ~name:_ ~args:_ -> assert false)
            ~prepare_event:(fun ~outcome:_ ~snapshot:_ -> Ok (M.memory_commit ignore))
          |> Result.ok_or_failwith
          |> ignore;
          let before = M.identity_snapshot manager |> Result.ok_or_failwith in
          let snapshot =
            Some (Agent_session.Runtime_builder.encode_moderator_snapshot before)
          in
          caps.commit_moderator snapshot |> protocol_ok;
          saved := Some (manager, before);
          Completed
            { final_history = input.history
            ; moderator_snapshot = snapshot
            ; runtime_requests = []
            }))
      (fun _env actor writer backend ->
         let initial = await_idle actor in
         let manager, before = Option.value_exn !saved in
         let observer = M.invocation_observer manager |> Option.value_exn in
         let escaped = ref None in
         (on_call
          := fun () ->
               match mode with
               | `Cancel ->
                 A.stop actor ~attachment_id:writer.id ~mode:Cancel
                 |> protocol_ok
                 |> ignore;
                 Eio.Fiber.yield ()
               | _ -> ());
         let run () =
           A.with_idle_queued_moderator_event_tools
             actor
             ~snapshot:before
             (fun ~executing ~retirement_reason:_ ~event:_ ~execute ~commit ->
                let native_callback = !on_call in
                (on_call
                 := fun () ->
                      assert (
                        Result.is_error
                          (commit
                             ~snapshot:before
                             ~requests:
                               { request_turn = false
                               ; request_compaction = false
                               ; end_session = None
                               }));
                      native_callback ());
                let child () =
                  I.create
                    ~parent_event:executing.context.id
                    ~observer
                    { (invocation_fixture ()).context with
                      id = Agent_protocol.Id.Invocation.create ()
                    ; origin = Moderator
                    ; session_id = executing.context.session_id
                    ; generation = executing.context.generation
                    ; input = `Object []
                    ; parent_invocation = None
                    ; parent_job = None
                    ; implementation_revision = reference.implementation_revision
                    ; capability_fingerprint = C.fingerprint registry
                    }
                  |> protocol_ok
                in
                escaped := Some (execute, child);
                let bad =
                  I.create
                    ~parent_event:(Agent_protocol.Id.Moderator_execution.create ())
                    ~observer
                    (child ()).context
                  |> protocol_ok
                in
                assert (
                  Result.is_error
                    (execute ~invocation:bad (fun ~dispatched:_ ~borrow_agent_actor:_ ->
                       assert false)));
                M.handle_next_event_entries_transactional
                  manager
                  ~session_id:"fixture"
                  ~now_ms:0
                  ~history:[]
                  ~available_tools:[]
                  ~session_meta:`Null
                  ~authorize:(fun ~event:_ -> Ok ())
                  ~on_tool_call:(fun ~name:_ ~args:_ ->
                    let result =
                      Agent_session.Native_tool_invocation.run_scoped
                        ~execute
                        ~registry:(fun () -> registry)
                        ~reference
                        ~invocation:(child ())
                        ~is_halted:(fun () -> false)
                        ~authorize:(fun _ _ ->
                          match mode with
                          | `Deny -> Error (handoff_error "denied")
                          | _ -> Ok ())
                        ~prepare_output:(fun _ -> Ok (`String "disclosed"))
                    in
                    match result with
                    | Ok { status = Resolved (Complete value); _ } -> Ok (Tool_ok value)
                    | _ -> Error "native call did not complete")
                  ~prepare_event:(fun ~outcome:_ ~snapshot ->
                    Ok
                      { M.persist =
                          (fun () ->
                            commit
                              ~snapshot
                              ~requests:
                                { request_turn = false
                                ; request_compaction = false
                                ; end_session = None
                                }
                            |> Result.map_error ~f:(fun error ->
                              error.Agent_protocol.Error.message))
                      ; install = ignore
                      })
                |> Result.map ~f:ignore
                |> Result.map_error ~f:handoff_error)
         in
         let completed =
           try Result.is_ok (run ()) with
           | Eio.Cancel.Cancelled _ -> false
         in
         let execute, child = Option.value_exn !escaped in
         assert (
           Result.is_error
             (execute ~invocation:(child ()) (fun ~dispatched:_ ~borrow_agent_actor:_ ->
                assert false)));
         let state = A.state actor |> protocol_ok in
         let native = List.hd_exn state.invocations in
         assert (
           Option.is_some native.parent_event
           && Option.is_none native.context.parent_invocation);
         assert (I.equal native (I.of_json (I.to_json native) |> protocol_ok));
         assert (
           Result.is_error
             (Agent_session.Session_state.upgrade_schema
                { state with schema_version = 6 }));
         let restored = restore_state state |> store_ok in
         assert_same_session_snapshot state restored;
         (match mode with
          | `Cancel -> ()
          | _ ->
            Agent_session.Moderator_observation.drain_idle
              ~claim:(A.with_idle_moderator_observation actor ~observer)
              ~manager
              ~history:(fun () -> [])
              ~available_tools:[]
              ~session_meta:`Null
              ~now:Agent_protocol.Timestamp.now
              ()
            |> protocol_ok
            |> ignore);
         let final = A.state actor |> protocol_ok in
         let native = List.hd_exn final.invocations in
         assert (Option.is_none final.active_operation);
         assert (
           List.length final.conversation.canonical_history
           = List.length initial.conversation.canonical_history);
         assert_same_session_snapshot final (Agent_session.Memory_backend.state backend);
         print_s
           [%sexp
             (mode : [ `Success | `Deny | `Save_fail | `Cancel ])
           , (completed : bool)
           , (!calls : int)
           , (native.status : I.status)
           , ((Option.value_exn native.observation).status : I.observation_status)]));
  [%expect
    {|
    (Success true 1 (Resolved (Complete (String disclosed))) Observed)
    (Deny false 0
     (Resolved
      (Fail
       ((code invocation.permission_denied)
        (message "Tool execution was not authorized.") (retryable false)
        (details Null))))
     Observed)
    (Save_fail false 1
     (Resolved (Cancelled "event exited before recording its native outcome"))
     Observed)
    (Cancel false 1 (Resolved (Cancelled "idle moderator cancelled")) Awaiting)
    |}]
;;

let%expect_test
    "queued event bridge scopes calls and preserves outcomes across wakeup failure"
  =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  List.iter
    [ `Many_calls; `Wakeup_failure; `Replace_binding; `Cancel; `Mismatched_head ]
    ~f:(fun mode ->
      let prepared = ref None
      and calls = ref 0
      and authorized = ref 0
      and wakeups = ref 0 in
      let on_native = ref (fun () -> ()) in
      let registry =
        ref (native_registry calls ~raises:false ~on_call:(fun () -> !on_native ()))
      in
      with_handoff_actor
        ~make_worker:(fun env _ ->
          let count =
            match mode with
            | `Many_calls -> 51
            | _ -> 1
          in
          let events =
            {| | `Session_start ->
                 Task.bind(Runtime.emit(`Null), fun ignored ->
                 Task.bind(Runtime.emit(`Null), fun ignored -> Task.pure(state)))
               | `Internal_event(payload) ->
                 let rec run = fun remaining -> match remaining with
                 | 0 -> Task.bind(Runtime.request_turn(), fun ignored -> Task.pure(state))
                 | _ -> Task.bind(Tool.call("read_file", `Object([])), fun result ->
                     let increment = match result with | `Ok(value) -> 1 | `Error(code) -> 10 in
                     let ignored = state[0] <- state[0] + increment in
                     run(remaining - 1))
                 in run(|}
            ^ Int.to_string count
            ^ {|)
               | `Tool_observed(p) ->
                 (match p.parent_event with
                  | `Some(id) -> let ignored = state[0] <- state[0] + 100 in Task.pure(state)
                  | _ -> Task.fail("missing event owner"))
               | _ -> Task.pure(state) |}
          in
          let manager, _, _ =
            handoff_definition
              env
              ~capability_registry:!registry
              ~declare_tool:false
              ~events
              ~moderator_capabilities:
                { Chat_response.Moderation.Capabilities.default with
                  on_tool_call =
                    (fun ~name:_ ~args:_ -> failwith "unscoped Tool.call fallback used")
                }
          in
          M.handle_event_entries_transactional
            manager
            ~session_id:"fixture"
            ~now_ms:0
            ~history:[]
            ~available_tools:[]
            ~session_meta:`Null
            ~event:Session_start
            ~authorize:(fun () -> Ok ())
            ~on_tool_call:(fun ~name:_ ~args:_ -> assert false)
            ~prepare_event:(fun ~outcome:_ ~snapshot:_ -> Ok (M.memory_commit ignore))
          |> Result.ok_or_failwith
          |> ignore;
          let before = M.identity_snapshot manager |> Result.ok_or_failwith in
          let snapshot =
            Some (Agent_session.Runtime_builder.encode_moderator_snapshot before)
          in
          prepared := Some manager;
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            caps.commit_moderator snapshot |> protocol_ok;
            Completed
              { final_history = input.history
              ; moderator_snapshot = snapshot
              ; runtime_requests = []
              }))
        (fun _env actor writer backend ->
           let initial = await_idle actor in
           let manager = Option.value_exn !prepared in
           (on_native
            := fun () ->
                 assert (Option.is_none (A.state actor |> protocol_ok).active_operation);
                 match mode with
                 | `Cancel ->
                   A.stop actor ~attachment_id:writer.id ~mode:Cancel
                   |> protocol_ok
                   |> ignore;
                   Eio.Fiber.yield ()
                 | _ -> ());
           let tools =
             Agent_session.Script_tool_calls.create
               ~registry:(fun () -> !registry)
               ~moderator_names:String.Set.empty
               ~now:Agent_protocol.Timestamp.now
               ~is_halted:(fun () ->
                 let state = A.state actor |> protocol_ok in
                 match state.lifecycle.desired with
                 | Running -> state.halted
                 | Stopped -> true)
               ~requires_active_moderator:(fun _ -> false)
               ~authorize:(fun _ _ ->
                 incr authorized;
                 Eio.Fiber.yield ();
                 (match mode with
                  | `Replace_binding -> registry := native_registry calls ~raises:false
                  | _ -> ());
                 Ok ())
               ~prepare_output:(fun _ -> Ok (`String "disclosed"))
               ~defer_observation:(fun child ->
                 assert (
                   Option.is_some child.parent_event
                   && Option.is_none child.context.parent_invocation);
                 incr wakeups;
                 match mode with
                 | `Wakeup_failure -> Error (handoff_error "wakeup unavailable")
                 | _ -> Ok ())
           in
           let claim ~snapshot f =
             A.with_current_idle_queued_moderator_event_tools
               actor
               ~snapshot
               (fun ~executing ~retirement_reason ~event ~execute ~commit ->
                  let event =
                    match mode with
                    | `Mismatched_head -> Session.Snapshot.String "wrong head"
                    | _ -> event
                  in
                  f ~executing ~retirement_reason ~event ~execute ~commit)
           in
           let history () =
             (A.state actor |> protocol_ok).conversation.canonical_history
             |> Agent_session.History_codec.all_of_protocol
             |> protocol_ok
           in
           let run () =
             try
               Agent_session.Moderator_event.run_queued_idle
                 ~claim
                 ~script_tools:tools
                 ~manager
                 ~history
                 ~available_tools:[]
                 ~session_meta:`Null
                 ~now:Agent_protocol.Timestamp.now
                 ()
             with
             | Eio.Cancel.Cancelled _ -> Error (handoff_error "cancelled")
           in
           let handled = ref 0 in
           (match run () with
            | Ok (Some _) -> incr handled
            | _ -> ());
           (match mode with
            | `Cancel | `Mismatched_head -> ()
            | _ ->
              assert (Option.is_some (run () |> protocol_ok));
              incr handled;
              let before_empty = A.state actor |> protocol_ok in
              assert (Option.is_none (run () |> protocol_ok));
              assert (
                Int64.equal
                  before_empty.counters.revision
                  (A.state actor |> protocol_ok).counters.revision));
           let after_events = A.state actor |> protocol_ok in
           let before_observation = !calls in
           (match mode with
            | `Cancel | `Mismatched_head -> ()
            | _ ->
              Agent_session.Moderator_observation.drain_idle
                ~max_observations:256
                ~claim:
                  (A.with_idle_moderator_observation
                     actor
                     ~observer:(M.invocation_observer manager |> Option.value_exn))
                ~manager
                ~history
                ~available_tools:[]
                ~session_meta:`Null
                ~now:Agent_protocol.Timestamp.now
                ()
              |> protocol_ok
              |> ignore);
           assert (!calls = before_observation);
           let final = A.state actor |> protocol_ok in
           List.iter final.invocations ~f:(fun invocation ->
             let event_id = Option.value_exn invocation.parent_event in
             assert (
               List.exists final.moderator_executions ~f:(fun event ->
                 Agent_protocol.Id.Moderator_execution.equal event.context.id event_id));
             match mode, invocation.status with
             | (`Many_calls | `Wakeup_failure), Resolved (Complete (`String "disclosed"))
               -> ()
             | `Replace_binding, Resolved (Fail error) ->
               assert (String.equal error.code "invocation.stale_binding")
             | `Cancel, Resolved (Cancelled _) -> ()
             | _ -> assert false);
           assert (Option.is_none final.active_operation);
           assert (
             List.equal
               Agent_protocol.History.equal_entry
               initial.conversation.canonical_history
               final.conversation.canonical_history);
           assert_same_session_snapshot final (Agent_session.Memory_backend.state backend);
           let count =
             match
               (M.identity_snapshot manager |> Result.ok_or_failwith).current_state
             with
             | Session.Snapshot.Array [ Int n ] -> n
             | _ -> assert false
           in
           let pending =
             List.count after_events.moderator_executions ~f:(fun event ->
               match event.intent with
               | Some Pending -> true
               | _ -> false)
           in
           let observed =
             List.count final.invocations ~f:(fun invocation ->
               match invocation.observation with
               | Some { status = Observed; _ } -> true
               | _ -> false)
           in
           print_s
             [%sexp
               { mode : [ `Many_calls
                        | `Wakeup_failure
                        | `Replace_binding
                        | `Cancel
                        | `Mismatched_head
                        ]
               ; handled = (!handled : int)
               ; native = (!calls : int)
               ; authorized = (!authorized : int)
               ; wakeups = (!wakeups : int)
               ; observed : int
               ; pending : int
               ; state = (count : int)
               }]));
  [%expect
    {|
    ((mode Many_calls) (handled 2) (native 102) (authorized 102) (wakeups 102)
     (observed 102) (pending 2) (state 10302))
    ((mode Wakeup_failure) (handled 2) (native 2) (authorized 2) (wakeups 2)
     (observed 2) (pending 2) (state 220))
    ((mode Replace_binding) (handled 2) (native 0) (authorized 1) (wakeups 2)
     (observed 2) (pending 2) (state 220))
    ((mode Cancel) (handled 0) (native 1) (authorized 1) (wakeups 0) (observed 0)
     (pending 0) (state 0))
    ((mode Mismatched_head) (handled 0) (native 0) (authorized 0) (wakeups 0)
     (observed 0) (pending 0) (state 0))
    |}]
;;

let%expect_test "legacy exact Manager claim installs source facts without a Run" =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  let module B = Agent_session.Runtime_builder in
  let source =
    {|type state = { count : int }
      type event = [ `Session_start | `Session_resume | `Turn_start | `Turn_end | `Queued ]
      let initial_state = { count = 0 }
      let on_event : context -> state -> event -> state task = fun ctx st ev ->
        match ev with
        | `Session_start ->
          Task.bind(Runtime.emit(`Queued), fun ignored_first ->
          Task.bind(Runtime.emit(`Queued), fun ignored_second -> Task.pure(st)))
        | `Session_resume -> Task.pure(st)
        | `Turn_start -> Task.pure(st)
        | `Turn_end -> Task.pure(st)
        | `Queued -> Task.pure({ count = st.count + 1 })|}
  in
  let managers = ref None in
  let worker_ref = ref None in
  let reject = ref false in
  let reject_claim = ref false in
  let rejected_claims = ref 0 in
  let during_worker =
    ref
      (fun
          (_ : Agent_session.Operation_worker.Input.t)
           (_ : Agent_session.Operation_worker.Capabilities.t)
         -> ())
  in
  with_handoff_actor
    ~reject:(fun next ->
      let reject_running =
        !reject_claim
        && List.exists
             next.Agent_session.Session_transition.state.moderator_executions
             ~f:(fun receipt ->
               match receipt.Agent_protocol.Moderator_execution.status with
               | Running -> true
               | Completed _ | Failed _ | Interrupted _ -> false)
      in
      if reject_running then incr rejected_claims;
      reject_running
      || (!reject
          && List.count
               next.Agent_session.Session_transition.state.moderator_executions
               ~f:(fun receipt ->
                 match receipt.Agent_protocol.Moderator_execution.status with
                 | Completed _ -> true
                 | Running | Failed _ | Interrupted _ -> false)
             > 1))
    ~make_worker:(fun env actor_ready ->
      let make_manager =
        prepared_legacy_callback_managers env ~id:"legacy-callback" ~source
      in
      let manager = make_manager () in
      managers := Some (manager, make_manager);
      M.handle_event_entries
        manager
        ~session_id:"fixture"
        ~now_ms:0
        ~history:[]
        ~available_tools:[]
        ~session_meta:`Null
        ~event:Session_start
      |> Result.ok_or_failwith
      |> ignore;
      let worker =
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
          !during_worker input caps;
          let state = A.state (Eio.Promise.await actor_ready) |> protocol_ok in
          let moderator_snapshot =
            match state.moderator with
            | Some _ as saved -> saved
            | None -> Some (B.encode_moderator_snapshot snapshot)
          in
          caps.commit_moderator moderator_snapshot |> protocol_ok;
          Completed
            { final_history = input.history; moderator_snapshot; runtime_requests = [] })
      in
      worker_ref := Some worker;
      worker)
    (fun _env actor writer backend ->
       let manager, make_manager = Option.value_exn !managers in
       let initial = await_idle actor in
       assert (Option.is_none initial.run_state);
       let worker = Option.value_exn !worker_ref in
       A.set_runtime_worker
         ~moderator_manager:manager
         actor
         ~worker:(Some worker)
         ~inference:
           (Some
              (Inference_ports.create ~config:Chat_response.Config.default ()
               |> Inference_ports.execution))
       |> protocol_ok;
       let last_scope = ref None in
       let admission_entered = ref false in
       let inspection_completed = ref false in
       let invoke
             ?(selection = M.Callback.Selection.Queue_head)
             ?inspect
             ?(operation_id = None)
             selected_manager
         =
         admission_entered := false;
         inspection_completed := false;
         M.handle_legacy_callback_entries
           selected_manager
           ~selection
           ~session_id:"fixture"
           ~now_ms:0
           ~history:[]
           ~available_tools:[]
           ~session_meta:`Null
           ~with_admission:(fun callback ~snapshot:_ run ->
             let outcome = ref None in
             let result =
               (match selection with
                | Queue_head ->
                  A.with_legacy_queued_callback actor ~callback ~operation_id
                | Boundary _ -> A.with_legacy_callback actor ~callback ~operation_id)
                 (fun ~scope ~commit ->
                    admission_entered := true;
                    last_scope := Some scope;
                    A.check_callback_current actor scope |> protocol_ok;
                    (match inspect with
                     | Some inspect -> inspect scope
                     | None ->
                       assert (Option.is_none (A.Callback_scope.origin scope));
                       assert (Result.is_error (A.callback_agent_actor actor ~scope));
                       assert (
                         Option.is_none
                           (A.run_actions actor (A.Callback_scope.executing scope)
                            |> protocol_ok));
                       List.iter
                         [ true, false; false, true ]
                         ~f:(fun (request_turn, request_compaction) ->
                           assert (
                             Result.is_error
                               (commit
                                  ~snapshot:(M.Callback.snapshot callback)
                                  ~requests:
                                    { request_turn
                                    ; request_compaction
                                    ; end_session = None
                                    }))));
                    inspection_completed := true;
                    let binding : M.callback_binding =
                      { services =
                          { model_recipes = Map.empty (module String)
                          ; after_ms = (fun ~delay_ms:_ ~payload:_ -> Error "unused")
                          ; cancel = (fun ~id:_ -> Error "unused")
                          }
                      ; authorize =
                          (fun () ->
                            A.check_callback_current actor scope
                            |> Result.map_error ~f:(fun error ->
                              error.Agent_protocol.Error.message))
                      ; prepare =
                          (fun ~outcome:_ ~snapshot ->
                            Ok
                              { M.persist =
                                  (fun () ->
                                    commit
                                      ~snapshot
                                      ~requests:
                                        { request_turn = false
                                        ; request_compaction = false
                                        ; end_session = None
                                        }
                                    |> Result.map_error ~f:(fun error ->
                                      error.Agent_protocol.Error.message))
                              ; install = ignore
                              })
                      }
                    in
                    run binding
                    |> Result.map_error ~f:handoff_error
                    |> Result.map ~f:(fun value -> outcome := Some value))
             in
             match result with
             | Ok true -> Ok (Option.value_exn !outcome)
             | Ok false -> Error "legacy physical actor unavailable"
             | Error error -> Error error.Agent_protocol.Error.message)
       in
       let other = make_manager () in
       M.enqueue_internal_event other (Chatml.Chatml_lang.VVariant ("Queued", []))
       |> Result.ok_or_failwith;
       assert (Result.is_error (invoke other));
       assert (not !admission_entered);
       assert (not !inspection_completed);
       let before = M.identity_snapshot manager |> Result.ok_or_failwith in
       assert (
         Result.is_error
           (A.with_idle_queued_moderator_event
              actor
              ~snapshot:before
              (fun ~event:_ ~commit:_ -> assert false)));
       let newer =
         { before with
           Session.Moderator_state.Identity_snapshot.current_state =
             Session.Snapshot.Record [ "count", Session.Snapshot.Int 7 ]
         }
       in
       A.change_moderator actor (Some (B.encode_moderator_snapshot newer))
       |> protocol_ok
       |> ignore;
       let changed = A.state actor |> protocol_ok in
       assert (Result.is_error (invoke manager));
       assert (not !admission_entered);
       assert (not !inspection_completed);
       let unchanged = A.state actor |> protocol_ok in
       assert (Int64.equal changed.counters.revision unchanged.counters.revision);
       assert (Option.equal Jsonaf.exactly_equal changed.moderator unchanged.moderator);
       assert (Option.is_none unchanged.run_state);
       A.change_moderator actor (Some (B.encode_moderator_snapshot before))
       |> protocol_ok
       |> ignore;
       invoke manager |> Result.ok_or_failwith |> ignore;
       let state = A.state actor |> protocol_ok in
       let index = Option.value_exn state.run_state in
       assert (List.is_empty (Agent_session.Run_state.runs index));
       assert (Int64.equal (Agent_session.Run_state.installation index).epoch 1L);
       let metadata =
         Agent_session.Moderator_checkpoint.queue state.moderator
         |> protocol_ok
         |> Option.value_exn
       in
       assert (Int64.equal (Agent_session.Moderator_queue.head_ordinal metadata) 1L);
       assert (List.length (Agent_session.Moderator_queue.entries metadata) = 1);
       assert (
         Result.is_error (A.check_callback_current actor (Option.value_exn !last_scope)));
       reject := true;
       let before_failure = M.identity_snapshot manager |> Result.ok_or_failwith in
       assert (Result.is_error (invoke manager));
       assert !admission_entered;
       assert !inspection_completed;
       let after_failure = M.identity_snapshot manager |> Result.ok_or_failwith in
       assert (
         Sexp.equal
           (Session.Moderator_state.Identity_snapshot.sexp_of_t before_failure)
           (Session.Moderator_state.Identity_snapshot.sexp_of_t after_failure));
       assert (Result.is_error (invoke manager));
       assert (not !admission_entered);
       assert (not !inspection_completed);
       let state = A.state actor |> protocol_ok in
       assert (
         List.exists state.moderator_executions ~f:(fun receipt ->
           match receipt.Agent_protocol.Moderator_execution.status with
           | Failed _ -> true
           | Running | Completed _ | Interrupted _ -> false));
       reject := false;
       let unavailable scope =
         match A.callback_agent_actor actor ~scope with
         | Error { code = Permission_denied; _ } -> ()
         | Error _ | Ok _ -> assert false
       in
       let resume inspect =
         invoke ~selection:(Boundary Session_resume) ~inspect manager
       in
       (* A genuine principal-only Start after this source is installed has one
          pending event, not historical authority. Its None is consumed once. *)
       A.stop actor ~attachment_id:writer.id ~mode:Graceful |> protocol_ok |> ignore;
       A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
       resume (fun scope ->
         assert (Option.is_none (A.callback_agent_actor actor ~scope |> protocol_ok)))
       |> Result.ok_or_failwith
       |> ignore;
       resume unavailable |> Result.ok_or_failwith |> ignore;
       let authentication live =
         let principal =
           Agent_protocol.Principal.create
             ~id:principal_id
             ~authentication_kind:"local"
             ~scopes:(Agent_protocol.Scope.Set.of_list [ Send_messages ])
             ~attributes:[]
           |> protocol_ok
         in
         Operator_authorization.guarded ~principal ~is_current:(fun () -> Atomic.get live)
       in
       let live = Atomic.make true in
       let original = authentication live in
       A.stop actor ~attachment_id:writer.id ~mode:Graceful |> protocol_ok |> ignore;
       A.start ~actor:original actor ~attachment_id:writer.id |> protocol_ok |> ignore;
       (* A rejected physical claim must neither consume Start's pending loan
          nor install a callback scope/receipt. Retry is the same real event. *)
       let before_claim = A.state actor |> protocol_ok in
       let inspected = ref false in
       reject_claim := true;
       assert (Result.is_error (resume (fun _ -> inspected := true)));
       reject_claim := false;
       assert (!rejected_claims = 1);
       assert (not !inspected);
       assert (not !admission_entered);
       assert (not !inspection_completed);
       assert_same_session_snapshot before_claim (A.state actor |> protocol_ok);
       let escaped = ref None in
       resume (fun scope ->
         let proof =
           A.callback_agent_actor actor ~scope |> protocol_ok |> Option.value_exn
         in
         assert (Operator_authorization.is_current proof);
         assert (
           Agent_protocol.Id.Principal.equal
             (Operator_authorization.principal proof).id
             (Operator_authorization.principal original).id);
         escaped := Some (scope, proof))
       |> Result.ok_or_failwith
       |> ignore;
       let scope, proof = Option.value_exn !escaped in
       assert (Operator_authorization.is_current original);
       assert (not (Operator_authorization.is_current proof));
       assert (Result.is_error (A.check_callback_current actor scope));
       resume unavailable |> Result.ok_or_failwith |> ignore;
       let run_operation ~sequence ~original ~expect_error ~after inspect =
         let inspected = ref false in
         let worker_oracle_completed = ref false in
         let before_events = (A.state actor |> protocol_ok).counters.event_sequence in
         (during_worker
          := fun input _caps ->
               let result =
                 invoke
                   ~selection:(Boundary Turn_start)
                   ~operation_id:(Some input.operation.id)
                   ~inspect:(fun scope ->
                     inspect scope;
                     inspected := true)
                   manager
               in
               assert !inspected;
               assert !inspection_completed;
               if expect_error
               then assert (Result.is_error result)
               else result |> Result.ok_or_failwith |> ignore;
               after input;
               worker_oracle_completed := true);
         let id =
           History_entry.Id.create ~namespace:"legacy_boundary" ~sequence
           |> Result.ok_or_failwith
         in
         let entry =
           Agent_session.History_codec.user_text ~id "boundary"
           |> Agent_session.History_codec.to_protocol
         in
         let admitted =
           A.submit_message_authenticated
             actor
             ~actor:original
             ~attachment_id:writer.id
             entry
           |> protocol_ok
         in
         let operation_id = Option.value_exn admitted.operation_id in
         await_idle actor |> ignore;
         (* These assertions run outside BOTH Manager and worker exception-to-
            result boundaries. Neither can convert an unfinished oracle to pass. *)
         assert !inspected;
         assert !worker_oracle_completed;
         let events =
           Agent_session.Memory_backend.events_after backend before_events |> protocol_ok
         in
         assert (
           List.exists events ~f:(fun event ->
             match
               Agent_protocol.Event.Durable.Payload.of_json
                 ~kind:event.Agent_protocol.Event.Durable.kind
                 event.payload
               |> protocol_ok
             with
             | Operation_completed { id; state = Completed; _ } ->
               Agent_protocol.Id.Operation.equal id operation_id
             | _ -> false))
       in
       run_operation ~sequence:1 ~original ~expect_error:true ~after:ignore (fun scope ->
         let proof =
           A.callback_agent_actor actor ~scope |> protocol_ok |> Option.value_exn
         in
         Atomic.set live false;
         (* The real operation is still active. Expiry, not an invented source
            or another caller's proof, denies the current callback. *)
         assert (Option.is_some (A.state actor |> protocol_ok).active_operation);
         assert (not (Operator_authorization.is_current proof));
         assert (Result.is_error (A.check_callback_current actor scope)));
       let next_original = authentication (Atomic.make true) in
       let cleared_loan = ref None in
       run_operation
         ~sequence:2
         ~original:next_original
         ~expect_error:false
         ~after:(fun input ->
           let scope, proof = Option.value_exn !cleared_loan in
           assert (Option.is_some (A.state actor |> protocol_ok).active_operation);
           (* Return already expired the loan. This clear must prevent a NEW
              callback, not be credited as the cause of the old loan's expiry. *)
           assert (not (Operator_authorization.is_current proof));
           assert (Result.is_error (A.check_callback_current actor scope));
           A.set_operation_worker actor (Some worker) |> protocol_ok;
           let before = A.state actor |> protocol_ok in
           assert (
             Result.is_error
               (invoke
                  ~selection:(Boundary Turn_end)
                  ~operation_id:(Some input.operation.id)
                  ~inspect:ignore
                  manager));
           assert (not !admission_entered);
           assert (not !inspection_completed);
           assert_same_session_snapshot before (A.state actor |> protocol_ok))
         (fun scope ->
            let proof =
              A.callback_agent_actor actor ~scope |> protocol_ok |> Option.value_exn
            in
            (* Replacement is serialized against the live borrow. An unexpected
            error must not swallow any assertion in this inspection. *)
            assert (Result.is_error (A.set_operation_worker actor (Some worker)));
            assert (Option.is_some (A.state actor |> protocol_ok).active_operation);
            assert (Operator_authorization.is_current next_original);
            assert (Operator_authorization.is_current proof);
            A.check_callback_current actor scope |> protocol_ok;
            cleared_loan := Some (scope, proof));
       print_endline
         "exact legacy source; fresh None differs from unavailable; one-event proof; \
          failed ACK preserves pending; original expiry and serialized Manager clear; \
          failed checkpoint blocks replay");
  [%expect
    {| exact legacy source; fresh None differs from unavailable; one-event proof; failed ACK preserves pending; original expiry and serialized Manager clear; failed checkpoint blocks replay |}]
;;

module Legacy_emit = struct
  module A = Agent_session.Session_actor
  module M = Chat_response.Moderator_manager
  module B = Agent_session.Runtime_builder

  let source =
    {|type state = int
      type event = [ `Session_start | `Session_resume | `First | `Second ]
      let initial_state = 0
      let on_event : context -> state -> event -> state task = fun ctx st ev ->
        match ev with
        | `Session_start -> Task.pure(st)
        | `Session_resume -> Task.bind(Runtime.emit(`First), fun ignored -> Task.pure(st))
        | `First -> Task.bind(Runtime.emit(`Second), fun ignored -> Task.pure(st + 1))
        | `Second -> Task.pure(st + 1)|}
  ;;

  let authentication live =
    let principal =
      Agent_protocol.Principal.create
        ~id:principal_id
        ~authentication_kind:"local"
        ~scopes:(Agent_protocol.Scope.Set.of_list [ Send_messages ])
        ~attributes:[]
      |> protocol_ok
    in
    Operator_authorization.guarded ~principal ~is_current:(fun () -> Atomic.get live)
  ;;

  (* Actual Prepared Manager + Actor callbacks; services intentionally provide
     no Model/provider port. Each inspection needs an oracle outside the engine. *)
  let invoke
        ?claim
        ?(requests =
          { Agent_protocol.Invocation.request_turn = false
          ; request_compaction = false
          ; end_session = None
          })
        ~actor
        ~manager
        ~selection
        ~operation_id
        ~inspect
        ()
    =
    let open Result.Let_syntax in
    let claim =
      Option.value claim ~default:(A.with_legacy_callback actor ~operation_id)
    in
    let outcome = ref None in
    let result =
      M.handle_legacy_callback_entries
        manager
        ~selection
        ~session_id:"fixture"
        ~now_ms:0
        ~history:[]
        ~available_tools:[]
        ~session_meta:`Null
        ~with_admission:(fun callback ~snapshot:_ run ->
          let%bind committed =
            claim ~callback (fun ~scope ~commit ->
              inspect scope;
              let schedules = A.callback_schedule_services actor ~scope in
              let binding : M.callback_binding =
                { services =
                    { model_recipes = Map.empty (module String)
                    ; after_ms =
                        (fun ~delay_ms ~payload ->
                          let%bind payload =
                            Chatml.Chatml_value_codec.Snapshot.of_value payload
                          in
                          schedules.after_ms
                            ~delay_ms
                            ~payload:
                              (Chatml.Chatml_value_codec.Snapshot.to_jsonaf payload))
                    ; cancel = schedules.cancel
                    }
                ; authorize =
                    (fun () ->
                      A.check_callback_current actor scope
                      |> Result.map_error ~f:(fun error ->
                        error.Agent_protocol.Error.message))
                ; prepare =
                    (fun ~outcome:_ ~snapshot ->
                      Ok
                        { M.persist =
                            (fun () ->
                              commit ~snapshot ~requests
                              |> Result.map_error ~f:(fun error ->
                                error.Agent_protocol.Error.message))
                        ; install = ignore
                        })
                }
              in
              run binding
              |> Result.map_error ~f:handoff_error
              |> Result.map ~f:(fun value -> outcome := Some value))
            |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
          in
          if committed
          then Ok (Option.value_exn !outcome)
          else Error "legacy callback physical owner unavailable")
    in
    result
  ;;

  let worker manager actor_ready during =
    Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
      during input caps;
      let state = A.state (Eio.Promise.await actor_ready) |> protocol_ok in
      let moderator_snapshot =
        match state.moderator with
        | Some _ as saved -> saved
        | None ->
          Some
            (B.encode_moderator_snapshot
               (M.identity_snapshot manager |> Result.ok_or_failwith))
      in
      caps.commit_moderator moderator_snapshot |> protocol_ok;
      Completed
        { final_history = input.history; moderator_snapshot; runtime_requests = [] })
  ;;

  let install actor manager worker =
    A.set_runtime_worker
      ~moderator_manager:manager
      actor
      ~worker:(Some worker)
      ~inference:
        (Some
           (Inference_ports.create ~config:Chat_response.Config.default ()
            |> Inference_ports.execution))
    |> protocol_ok
  ;;

  let queue state =
    Agent_session.Moderator_checkpoint.queue state.Agent_session.Session_state.moderator
    |> protocol_ok
    |> Option.value_exn
  ;;
end

let%expect_test "acknowledged legacy emits carry live custody across exact queue claims" =
  let module L = Legacy_emit in
  let module A = L.A in
  let module M = L.M in
  List.iter [ `Fresh_none; `Original; `Rejected_checkpoint ] ~f:(fun mode ->
    let manager_ref = ref None in
    let worker_ref = ref None in
    let during =
      ref
        (fun
            (_ : Agent_session.Operation_worker.Input.t)
             (_ : Agent_session.Operation_worker.Capabilities.t)
           -> ())
    in
    let reject_claim = ref false in
    let reject_checkpoint = ref false in
    let claim_rejections = ref 0 in
    let checkpoint_rejections = ref 0 in
    with_handoff_actor
      ~reject:(fun next ->
        let receipts = next.Agent_session.Session_transition.state.moderator_executions in
        if
          !reject_claim
          && List.exists receipts ~f:(fun receipt ->
            match receipt.Agent_protocol.Moderator_execution.status with
            | Running -> true
            | Completed _ | Failed _ | Interrupted _ -> false)
        then (
          incr claim_rejections;
          true)
        else if
          !reject_checkpoint
          && List.count receipts ~f:(fun receipt ->
               match receipt.Agent_protocol.Moderator_execution.status with
               | Completed _ -> true
               | Running | Failed _ | Interrupted _ -> false)
             = 2
        then (
          incr checkpoint_rejections;
          true)
        else false)
      ~make_worker:(fun env actor_ready ->
        let manager =
          prepared_legacy_callback_managers env ~id:"legacy-emit" ~source:L.source ()
        in
        manager_ref := Some manager;
        let worker =
          L.worker manager actor_ready (fun input caps -> !during input caps)
        in
        worker_ref := Some worker;
        worker)
      (fun _env actor writer backend ->
         await_idle actor |> ignore;
         let manager = Option.value_exn !manager_ref in
         L.install actor manager (Option.value_exn !worker_ref);
         let original_live = Atomic.make true in
         let original = L.authentication original_live in
         A.stop actor ~attachment_id:writer.id ~mode:Graceful |> protocol_ok |> ignore;
         A.start
           ?actor:
             (match mode with
              | `Fresh_none -> None
              | `Original | `Rejected_checkpoint -> Some original)
           actor
           ~attachment_id:writer.id
         |> protocol_ok
         |> ignore;
         let inspected = ref false in
         let check scope =
           let borrowed = A.callback_agent_actor actor ~scope |> protocol_ok in
           (match mode, borrowed with
            | `Fresh_none, None -> ()
            | (`Original | `Rejected_checkpoint), Some proof ->
              assert (Operator_authorization.is_current proof)
            | `Fresh_none, Some _ | (`Original | `Rejected_checkpoint), None ->
              assert false);
           inspected := true
         in
         L.invoke
           ~actor
           ~manager
           ~selection:(Boundary Session_resume)
           ~operation_id:None
           ~inspect:check
           ()
         |> Result.ok_or_failwith
         |> ignore;
         assert !inspected;
         let emitted = A.state actor |> protocol_ok in
         let first =
           List.hd_exn (Agent_session.Moderator_queue.entries (L.queue emitted))
         in
         let producer =
           match Agent_session.Moderator_queue.Entry.producer first with
           | Some (Event id) -> id
           | Some _ | None -> assert false
         in
         assert (
           List.exists emitted.moderator_executions ~f:(fun receipt ->
             Agent_protocol.Id.Moderator_execution.equal receipt.context.id producer
             &&
             match receipt.status with
             | Completed _ -> true
             | Running | Failed _ | Interrupted _ -> false));
         (* Reject before the Running ACK. This must preserve the live row, not
           consume it and leave the successful retry without its original loan. *)
         inspected := false;
         reject_claim := true;
         assert (
           Result.is_error
             (L.invoke
                ~actor
                ~manager
                ~selection:Queue_head
                ~operation_id:None
                ~inspect:check
                ()));
         reject_claim := false;
         assert (!claim_rejections = 1 && not !inspected);
         assert_same_session_snapshot emitted (A.state actor |> protocol_ok);
         let worker_oracle = ref false in
         (during
          := fun input _caps ->
               inspected := false;
               (reject_checkpoint
                := match mode with
                   | `Rejected_checkpoint -> true
                   | `Fresh_none | `Original -> false);
               let result =
                 L.invoke
                   ~actor
                   ~manager
                   ~selection:Queue_head
                   ~operation_id:(Some input.operation.id)
                   ~inspect:check
                   ()
               in
               assert !inspected;
               (match mode with
                | `Rejected_checkpoint ->
                  assert (Result.is_error result);
                  assert (!checkpoint_rejections = 1);
                  let state = A.state actor |> protocol_ok in
                  assert (
                    Int64.equal
                      (Agent_session.Moderator_queue.head_ordinal (L.queue state))
                      (Agent_session.Moderator_queue.Entry.ordinal first));
                  assert (
                    List.length (Agent_session.Moderator_queue.entries (L.queue state))
                    = 1);
                  let entered = ref false in
                  assert (
                    Result.is_error
                      (L.invoke
                         ~actor
                         ~manager
                         ~selection:Queue_head
                         ~operation_id:(Some input.operation.id)
                         ~inspect:(fun _ -> entered := true)
                         ()));
                  assert (not !entered)
                | `Fresh_none | `Original ->
                  result |> Result.ok_or_failwith |> ignore;
                  let state = A.state actor |> protocol_ok in
                  let second =
                    List.hd_exn (Agent_session.Moderator_queue.entries (L.queue state))
                  in
                  assert (
                    Int64.(
                      Agent_session.Moderator_queue.Entry.ordinal second
                      > Agent_session.Moderator_queue.Entry.ordinal first));
                  assert (
                    Option.equal
                      Agent_protocol.Inference_origin.equal
                      (Agent_session.Moderator_queue.Entry.origin first)
                      (Agent_session.Moderator_queue.Entry.origin second));
                  let completed = ref false in
                  let result =
                    L.invoke
                      ~actor
                      ~manager
                      ~selection:Queue_head
                      ~operation_id:(Some input.operation.id)
                      ~inspect:(fun scope ->
                        check scope;
                        (match mode with
                         | `Original ->
                           Atomic.set original_live false;
                           assert (Result.is_error (A.callback_agent_actor actor ~scope));
                           assert (Result.is_error (A.check_callback_current actor scope))
                         | `Fresh_none | `Rejected_checkpoint -> ());
                        completed := true)
                      ()
                  in
                  assert !completed;
                  (match mode with
                   | `Original -> assert (Result.is_error result)
                   | `Fresh_none ->
                     result |> Result.ok_or_failwith |> ignore;
                     assert (
                       List.is_empty
                         (Agent_session.Moderator_queue.entries
                            (L.queue (A.state actor |> protocol_ok))))
                   | `Rejected_checkpoint -> assert false));
               worker_oracle := true);
         let drainer_original = L.authentication (Atomic.make true) in
         let before_events = (A.state actor |> protocol_ok).counters.event_sequence in
         let id =
           History_entry.Id.create ~namespace:"legacy_emit" ~sequence:1
           |> Result.ok_or_failwith
         in
         let entry =
           Agent_session.History_codec.user_text ~id "drain"
           |> Agent_session.History_codec.to_protocol
         in
         let admitted =
           A.submit_message_authenticated
             actor
             ~actor:drainer_original
             ~attachment_id:writer.id
             entry
           |> protocol_ok
         in
         let operation_id = Option.value_exn admitted.operation_id in
         await_idle actor |> ignore;
         assert !worker_oracle;
         assert (
           List.exists
             (Agent_session.Memory_backend.events_after backend before_events
              |> protocol_ok)
             ~f:(fun event ->
               match
                 Agent_protocol.Event.Durable.Payload.of_json
                   ~kind:event.Agent_protocol.Event.Durable.kind
                   event.payload
                 |> protocol_ok
               with
               | Operation_completed { id; state = Completed; _ } ->
                 Agent_protocol.Id.Operation.equal id operation_id
               | _ -> false));
         assert (Operator_authorization.is_current drainer_original);
         assert (
           Bool.equal
             (Operator_authorization.is_current original)
             (match mode with
              | `Original -> false
              | `Fresh_none | `Rejected_checkpoint -> true))));
  print_endline
    "exact child chain; original or fresh None custody; claim rejection preserves loan; \
     failed checkpoint blocks replay";
  [%expect
    {| exact child chain; original or fresh None custody; claim rejection preserves loan; failed checkpoint blocks replay |}]
;;

let%expect_test "legacy publication cut restores facts without continuation custody" =
  let module L = Legacy_emit in
  let module A = L.A in
  let module M = L.M in
  let module O = Owner_termination_tests in
  let armed = ref false in
  let saved = ref None in
  let proof = ref None in
  let retained_scope = ref None in
  let inspected = ref false in
  let original = L.authentication (Atomic.make true) in
  let failure = Failure "legacy checkpoint publication cut" in
  O.with_owner_env
    ~state_committed:(fun state _ ->
      if
        !armed
        && List.exists
             state.Agent_session.Session_state.moderator_executions
             ~f:Agent_session.Observation_follow_up.pending_event
      then (
        saved := Some state;
        armed := false;
        O.fail_publication failure))
    (fun env _callers actor backend _cancel exited ->
       (* The shared owner fixture begins Pending. Complete actual initialization
         before installing the Prepared Manager; no runtime state is fabricated. *)
       let manager =
         prepared_legacy_callback_managers
           env
           ~id:"legacy-publication-cut"
           ~source:L.source
           ()
       in
       let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
       let expected = A.state actor |> protocol_ok in
       let initialization =
         A.begin_initialization_authenticated actor ~actor:original ~expected
         |> protocol_ok
       in
       A.complete_initialization
         actor
         ~scope:initialization
         ~candidate:
           { expected with moderator = Some (L.B.encode_moderator_snapshot snapshot) }
       |> protocol_ok
       |> ignore;
       A.end_initialization actor ~scope:initialization |> protocol_ok;
       let worker =
         Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
           Completed
             { final_history = input.history
             ; moderator_snapshot = Some (L.B.encode_moderator_snapshot snapshot)
             ; runtime_requests = []
             })
       in
       L.install actor manager worker;
       let writer, _ = A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok in
       A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
       A.stop actor ~attachment_id:writer.id ~mode:Graceful |> protocol_ok |> ignore;
       A.start ~actor:original actor ~attachment_id:writer.id |> protocol_ok |> ignore;
       armed := true;
       let result =
         O.capture (fun () ->
           L.invoke
             ~actor
             ~manager
             ~selection:(Boundary Session_resume)
             ~operation_id:None
             ~requests:
               { request_turn = true; request_compaction = false; end_session = None }
             ~inspect:(fun scope ->
               let borrowed =
                 A.callback_agent_actor actor ~scope |> protocol_ok |> Option.value_exn
               in
               assert (Operator_authorization.is_current borrowed);
               proof := Some borrowed;
               retained_scope := Some scope;
               inspected := true)
             ())
       in
       (* Outside both Manager and worker exception conversion. The Eio timeout
          bounds cancellable waits; the external process-group watchdog bounds a
          regression in protected cleanup or owner joining. *)
       assert !inspected;
       (match result with
        | Ok (Error _) | Error _ -> ()
        | Ok (Ok _) -> failwith "publication cut returned successful callback");
       (match Eio.Promise.await_exn exited with
        | Error (actual, _) -> assert (phys_equal actual failure)
        | Ok () -> failwith "publication cut did not terminate its owner");
       (match A.check_callback_current actor (Option.value_exn !retained_scope) with
        | Error { code = Conflict; _ } -> ()
        | Error error ->
          raise_s
            [%sexp "unexpected expired callback result", (error : Agent_protocol.Error.t)]
        | Ok () -> failwith "closed owner retained a live callback");
       O.unavailable (A.state actor);
       assert (Operator_authorization.is_current original);
       assert (not (Operator_authorization.is_current (Option.value_exn !proof)));
       let stored = Option.value_exn !saved in
       assert_same_session_snapshot stored (Agent_session.Memory_backend.state backend);
       let entries = Agent_session.Moderator_queue.entries (L.queue stored) in
       assert (List.length entries = 1);
       let producer =
         match Agent_session.Moderator_queue.Entry.producer (List.hd_exn entries) with
         | Some (Event id) -> id
         | Some _ | None -> assert false
       in
       assert (
         List.exists stored.moderator_executions ~f:(fun receipt ->
           Agent_protocol.Id.Moderator_execution.equal receipt.context.id producer
           && Agent_session.Observation_follow_up.pending_event receipt
           &&
           match receipt.status with
           | Completed _ -> true
           | Running | Failed _ | Interrupted _ -> false)));
  let stored = Option.value_exn !saved in
  (* Restore the exact saved checkpoint into a new Manager and a fresh actor.
     These durable facts do not reconstruct the dead owner's admission cells. *)
  with_actor_workspace (fun env _ ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      Eio.Switch.run (fun sw ->
        let snapshot =
          Agent_session.Moderator_checkpoint.decode stored.moderator
          |> protocol_ok
          |> Option.value_exn
        in
        let manager =
          prepared_legacy_callback_managers
            env
            ~id:"legacy-publication-cut"
            ~source:L.source
            ~snapshot
            ()
        in
        let backend =
          Agent_session.Memory_backend.create ~event_capacity:128 ~initial_state:stored
        in
        let actor =
          A.create
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~mailbox_capacity:8
            ~compaction_env:None
            ~initial_state:stored
            ~operation_worker:None
            ~persistence:(Agent_session.Memory_backend.persistence backend)
            ~services:
              { now = (fun () -> timestamp)
              ; monotonic_now = (fun () -> Mtime.min_stamp)
              ; create_attachment_id = Agent_protocol.Id.Attachment.create
              ; create_reclaim_token = (fun () -> "legacy-cut-reopen")
              ; job_results = None
              ; schedule_limits = Agent_session.Staged_schedules.default_limits
              ; notification_limits = Agent_session.Staged_notifications.default_limits
              ; ingress_limits = Agent_session.Staged_ingress.default_limits
              ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
              ; state_committed = (fun _ _ -> ())
              }
        in
        Exn.protect
          ~finally:(fun () -> A.shutdown actor)
          ~f:(fun () ->
            let worker =
              Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
                Completed
                  { final_history = input.history
                  ; moderator_snapshot = stored.moderator
                  ; runtime_requests = []
                  })
            in
            L.install actor manager worker;
            (* Restoration/installation did not execute the saved queue or intent. *)
            assert_same_session_snapshot stored (A.state actor |> protocol_ok);
            let unavailable_checked = ref false in
            let attempted =
              L.invoke
                ~actor
                ~manager
                ~selection:Queue_head
                ~operation_id:None
                ~requests:
                  { request_turn = true; request_compaction = false; end_session = None }
                ~inspect:(fun scope ->
                  (match A.callback_agent_actor actor ~scope with
                   | Error { code = Permission_denied; _ } -> ()
                   | Error _ | Ok _ -> assert false);
                  assert (
                    Option.is_none
                      (A.run_actions actor (A.Callback_scope.executing scope)
                       |> protocol_ok));
                  unavailable_checked := true)
                ()
            in
            assert !unavailable_checked;
            assert (Result.is_error attempted);
            let after = A.state actor |> protocol_ok in
            assert (Option.is_none after.active_operation);
            [%test_eq: int64]
              (Agent_session.Moderator_queue.head_ordinal (L.queue after))
              (Agent_session.Moderator_queue.head_ordinal (L.queue stored));
            assert (
              List.equal
                Agent_session.Moderator_queue.Entry.equal
                (Agent_session.Moderator_queue.entries (L.queue after))
                (Agent_session.Moderator_queue.entries (L.queue stored)));
            assert (
              List.count after.moderator_executions ~f:(fun receipt ->
                match receipt.Agent_protocol.Moderator_execution.status with
                | Completed _ -> true
                | Running | Failed _ | Interrupted _ -> false)
              = List.length stored.moderator_executions)))));
  print_endline
    "ACKed Event, queue and intent survive; closed owner releases cleanup; reopen lends \
     no custody";
  [%expect
    {|
    +queued moderator cleanup failed: session command mailbox is closed
    ACKed Event, queue and intent survive; closed owner releases cleanup; reopen lends no custody
    |}]
;;

let%expect_test
    "callback Schedule admission is immediate and preserves legacy cancellation"
  =
  let module L = Legacy_emit in
  let module A = L.A in
  let module P = Agent_protocol in
  List.iter [ false; true ] ~f:(fun authenticated ->
    let manager_ref = ref None in
    let worker_ref = ref None in
    let reject_schedule = ref false in
    let rejected_writes = ref 0 in
    with_handoff_actor
      ~reject:(fun next ->
        if
          !reject_schedule
          && List.length next.Agent_session.Session_transition.state.schedules > 1
        then (
          incr rejected_writes;
          true)
        else false)
      ~make_worker:(fun env actor_ready ->
        let manager =
          prepared_legacy_callback_managers
            env
            ~id:"callback-schedule"
            ~source:L.source
            ()
        in
        manager_ref := Some manager;
        let worker = L.worker manager actor_ready (fun _ _ -> ()) in
        worker_ref := Some worker;
        worker)
      (fun _env actor writer _backend ->
         await_idle actor |> ignore;
         let manager = Option.value_exn !manager_ref in
         L.install actor manager (Option.value_exn !worker_ref);
         let original = L.authentication (Atomic.make true) in
         A.stop actor ~attachment_id:writer.id ~mode:Graceful |> protocol_ok |> ignore;
         A.start
           ?actor:(Option.some_if authenticated original)
           actor
           ~attachment_id:writer.id
         |> protocol_ok
         |> ignore;
         (* An actual legacy target without positive source/custody remains
           cancellable. Its stored DTO does not grant the caller authority. *)
         let historical : P.Schedule.t =
           { id = P.Id.Schedule.create ()
           ; session_id
           ; generation = 0
           ; payload = `Null
           ; created_at = timestamp
           ; next_due_at = timestamp
           ; misfire = Deliver_once_immediately
           ; status = Scheduled
           ; delivery_count = 0
           ; last_delivery_at = None
           ; inference_provenance = None
           ; delivery_cancellation = None
           ; ownership = None
           }
         in
         A.add_schedule actor historical |> protocol_ok |> ignore;
         let escaped = ref None in
         let admitted = ref None in
         let inspected = ref false in
         let payload = `Object [ "legacy", `String "raw" ] in
         L.invoke
           ~actor
           ~manager
           ~selection:(Boundary Session_resume)
           ~operation_id:None
           ~inspect:(fun scope ->
             (match A.callback_agent_actor actor ~scope |> protocol_ok with
              | Some proof ->
                assert authenticated;
                assert (Operator_authorization.is_current proof)
              | None -> assert (not authenticated));
             let services = A.callback_schedule_services actor ~scope in
             let before = A.state actor |> protocol_ok in
             reject_schedule := true;
             assert (Result.is_error (services.after_ms ~delay_ms:0 ~payload));
             reject_schedule := false;
             assert (!rejected_writes = 1);
             assert_same_session_snapshot before (A.state actor |> protocol_ok);
             let id =
               services.after_ms ~delay_ms:0 ~payload
               |> Result.ok_or_failwith
               |> P.Id.Schedule.of_string
               |> protocol_ok
             in
             let immediate = A.state actor |> protocol_ok in
             let schedule =
               List.find_exn immediate.schedules ~f:(fun schedule ->
                 P.Id.Schedule.equal schedule.id id)
             in
             let provenance = Option.value_exn schedule.inference_provenance in
             assert (
               P.Run_source.equal
                 (P.Schedule_provenance.source provenance)
                 (A.Callback_scope.source scope));
             assert (
               Option.equal
                 P.Inference_origin.equal
                 (P.Schedule_provenance.origin provenance)
                 (A.Callback_scope.origin scope));
             assert (Jsonaf.exactly_equal schedule.payload payload);
             assert (Option.is_none schedule.ownership);
             assert (P.Timestamp.equal schedule.created_at schedule.next_due_at);
             assert (
               Option.equal Jsonaf.exactly_equal immediate.moderator before.moderator);
             let receipt = A.Callback_scope.executing scope in
             assert (
               List.exists immediate.moderator_executions ~f:(fun current ->
                 P.Id.Moderator_execution.equal current.context.id receipt.context.id
                 &&
                 match current.status with
                 | Running -> true
                 | Completed _ | Failed _ | Interrupted _ -> false));
             services.cancel ~id:(P.Id.Schedule.to_string historical.id)
             |> Result.ok_or_failwith;
             let cancelled =
               List.find_exn (A.state actor |> protocol_ok).schedules ~f:(fun schedule ->
                 P.Id.Schedule.equal schedule.id historical.id)
             in
             (match cancelled.status with
              | Cancelled -> ()
              | Scheduled | Delivering | Delivered | Failed _ -> assert false);
             assert (Option.is_none cancelled.inference_provenance);
             admitted := Some id;
             escaped := Some services;
             inspected := true)
           ()
         |> Result.ok_or_failwith
         |> ignore;
         (* Both oracles are outside the Manager's exception-to-error boundary. *)
         assert !inspected;
         let services = Option.value_exn !escaped in
         let before_escaped = A.state actor |> protocol_ok in
         assert (Result.is_error (services.after_ms ~delay_ms:0 ~payload));
         assert (
           Result.is_error
             (services.cancel ~id:(P.Id.Schedule.to_string (Option.value_exn !admitted))));
         assert_same_session_snapshot before_escaped (A.state actor |> protocol_ok);
         (* The exact pending lifecycle admission was consumed by the first
           callback. A second pure Boundary has unavailable custody, not None. *)
         let unavailable_checked = ref false in
         L.invoke
           ~actor
           ~manager
           ~selection:(Boundary Session_resume)
           ~operation_id:None
           ~inspect:(fun scope ->
             (match A.callback_agent_actor actor ~scope with
              | Error { code = Permission_denied; _ } -> ()
              | Error _ | Ok _ -> assert false);
             let unavailable = A.callback_schedule_services actor ~scope in
             let before = A.state actor |> protocol_ok in
             assert (Result.is_error (unavailable.after_ms ~delay_ms:0 ~payload));
             assert (
               Result.is_error
                 (unavailable.cancel
                    ~id:(P.Id.Schedule.to_string (Option.value_exn !admitted))));
             assert_same_session_snapshot before (A.state actor |> protocol_ok);
             unavailable_checked := true)
           ()
         |> Result.ok_or_failwith
         |> ignore;
         assert !unavailable_checked));
  print_endline
    "immediate ACK and original provenance; failed save leaves no timer; historical \
     cancel; escaped/unavailable reject";
  [%expect
    {| immediate ACK and original provenance; failed save leaves no timer; historical cancel; escaped/unavailable reject |}]
;;

module Legacy_checkpoint_extensions = struct
  let outer =
    `Object [ "lexeme", `Number "1.2500e+2"; "nested", `Array [ `True; `String "outer" ] ]
  ;;

  let identity = `String "identity-retained"
  let queue = `Object [ "marker", `String "queue-retained" ]

  let field json name =
    match json with
    | `Object fields -> List.Assoc.find_exn fields name ~equal:String.equal
    | _ -> failwith "checkpoint extension fixture requires an object"
  ;;

  let add json name value =
    match json with
    | `Object fields ->
      assert (not (List.Assoc.mem fields name ~equal:String.equal));
      `Object (fields @ [ name, value ])
    | _ -> failwith "checkpoint extension fixture requires an object"
  ;;

  let map_field json ~name ~f =
    let previous = field json name in
    match json with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (key, value) ->
           key, if String.equal key name then f previous else value))
    | _ -> assert false
  ;;

  let annotate raw =
    add raw "future_checkpoint" outer
    |> map_field ~name:"identity_snapshot" ~f:(fun raw ->
      add raw "future_identity" identity)
    |> map_field ~name:"queue_provenance" ~f:(fun raw -> add raw "future_queue" queue)
  ;;

  let assert_preserved raw =
    let raw = Option.value_exn raw in
    assert (Jsonaf.exactly_equal (field raw "future_checkpoint") outer);
    assert (
      Jsonaf.exactly_equal
        (field (field raw "identity_snapshot") "future_identity")
        identity);
    assert (
      Jsonaf.exactly_equal (field (field raw "queue_provenance") "future_queue") queue)
  ;;

  let assert_pair (actor, memory) =
    assert_preserved actor;
    assert_preserved memory
  ;;
end

module Legacy_schedule = struct
  module L = Legacy_emit
  module M = L.M
  module P = Agent_protocol

  let source =
    {|type state = int
      type event = [ `Session_start | `Session_resume | `Turn_start | `Turn_end | `Tick ]
      let initial_state = 0
      let on_event : context -> state -> event -> state task = fun ctx st ev ->
        match ev with
        | `Tick -> Task.pure(st + 1)
        | _ -> Task.pure(st)|}
  ;;

  let payload = Session.Snapshot.to_jsonaf (Session.Snapshot.Variant ("Tick", []))

  (* Exercise Runtime_owner's real delivery/lease path and the actual Manager
     enqueue transaction. This adapter has no provider or model implementation. *)
  let runtime manager worker =
    let base = Runtime_lease_tests.runtime ~close:(fun () -> ()) () in
    { base with
      worker
    ; moderator_manager = Some manager
    ; moderator_snapshot =
        (fun () ->
          M.identity_snapshot manager
          |> Result.ok_or_failwith
          |> L.B.encode_moderator_snapshot
          |> Option.some)
    ; enqueue_internal_event =
        (fun ?prepare payload ->
          let open Result.Let_syntax in
          let failure message = P.Error.invalid_request message in
          let%bind snapshot =
            Session.Snapshot.of_jsonaf payload |> Result.map_error ~f:failure
          in
          let%bind event =
            Session.Snapshot.to_value snapshot |> Result.map_error ~f:failure
          in
          M.enqueue_internal_event_entries
            manager
            ~event
            ~prepare:(fun ~before ~snapshot ->
              match prepare with
              | None -> Error "delivery omitted actor preparation"
              | Some prepare ->
                prepare ~before ~snapshot
                |> Result.map_error ~f:(fun error -> error.P.Error.message))
          |> Result.map_error ~f:failure
          |> Result.map ~f:(fun snapshot -> Some (L.B.encode_moderator_snapshot snapshot)))
    }
  ;;
end

let%expect_test
    "actual legacy Schedule delivery preserves facts and exact continuation custody"
  =
  let module F = Legacy_schedule in
  let module L = Legacy_emit in
  let module A = L.A in
  let module M = L.M in
  let module P = Agent_protocol in
  let module Q = Agent_session.Moderator_queue in
  let module Owner = Agent_server.Runtime_owner in
  List.iter [ `Fresh_none; `Original; `Expired; `Historical ] ~f:(fun mode ->
    let manager_ref = ref None in
    let make_manager_ref = ref None in
    let worker_ref = ref None in
    let reject_delivery = ref false in
    let rejected = ref 0 in
    with_handoff_actor
      ~reject:(fun next ->
        if
          !reject_delivery
          && List.exists
               next.Agent_session.Session_transition.state.schedules
               ~f:(fun schedule ->
                 match schedule.Agent_protocol.Schedule.status with
                 | Delivered -> true
                 | Scheduled | Delivering | Cancelled | Failed _ -> false)
        then (
          incr rejected;
          true)
        else false)
      ~make_worker:(fun env actor_ready ->
        let make =
          prepared_legacy_callback_managers env ~id:"legacy-delivery" ~source:F.source
        in
        let manager = make () in
        make_manager_ref := Some make;
        manager_ref := Some manager;
        let worker = L.worker manager actor_ready (fun _ _ -> ()) in
        worker_ref := Some worker;
        worker)
      (fun _env actor writer backend ->
         await_idle actor |> ignore;
         let manager = Option.value_exn !manager_ref in
         let worker = Option.value_exn !worker_ref in
         L.install actor manager worker;
         let original_live = Atomic.make true in
         let original = L.authentication original_live in
         A.stop actor ~attachment_id:writer.id ~mode:Graceful |> protocol_ok |> ignore;
         A.start
           ?actor:
             (match mode with
              | `Fresh_none -> None
              | `Original | `Expired | `Historical -> Some original)
           actor
           ~attachment_id:writer.id
         |> protocol_ok
         |> ignore;
         let id = ref None in
         let inspected = ref false in
         L.invoke
           ~actor
           ~manager
           ~selection:(Boundary Session_resume)
           ~operation_id:None
           ~inspect:(fun scope ->
             (match mode with
              | `Historical -> ()
              | `Fresh_none | `Original | `Expired ->
                let services = A.callback_schedule_services actor ~scope in
                id
                := Some
                     (services.after_ms ~delay_ms:0 ~payload:F.payload
                      |> Result.ok_or_failwith
                      |> P.Id.Schedule.of_string
                      |> protocol_ok));
             inspected := true)
           ()
         |> Result.ok_or_failwith
         |> ignore;
         assert !inspected;
         (match mode with
          | `Historical ->
            let timer : P.Schedule.t =
              { id = P.Id.Schedule.create ()
              ; session_id
              ; generation = 0
              ; payload = F.payload
              ; created_at = timestamp
              ; next_due_at = timestamp
              ; misfire = Deliver_once_immediately
              ; status = Scheduled
              ; delivery_count = 0
              ; last_delivery_at = None
              ; ownership = None
              ; inference_provenance = None
              ; delivery_cancellation = None
              }
            in
            A.add_schedule actor timer |> protocol_ok |> ignore;
            id := Some timer.id
          | `Fresh_none | `Original | `Expired -> ());
         let id = Option.value_exn !id in
         let claimed =
           A.claim_schedule actor ~schedule_id:id ~generation:0
           |> protocol_ok
           |> Option.value_exn
         in
         let checkpoints () =
           ( (A.state actor |> protocol_ok).moderator
           , (Agent_session.Memory_backend.state backend).moderator )
         in
         (match mode with
          | `Original ->
            let raw =
              (A.state actor |> protocol_ok).moderator
              |> Option.value_exn
              |> Legacy_checkpoint_extensions.annotate
            in
            A.change_moderator actor (Some raw) |> protocol_ok |> ignore
          | `Fresh_none | `Expired | `Historical -> ());
         let before = A.state actor |> protocol_ok in
         assert (List.is_empty (Q.entries (L.queue before)));
         let previous = M.identity_snapshot manager |> Result.ok_or_failwith in
         let after =
           { previous with
             Session.Moderator_state.Identity_snapshot.queued_internal_events =
               [ Session.Snapshot.Variant ("Tick", []) ]
           }
         in
         (* Omitting the real resource cannot downgrade either positive creator
           provenance or an already metadata-bearing historical queue. *)
         assert (
           Result.is_error
             (A.complete_schedule
                ~expected_schedule:claimed
                actor
                ~schedule_id:id
                ~generation:0
                ~moderator_snapshot:(Some (L.B.encode_moderator_snapshot after))));
         assert_same_session_snapshot before (A.state actor |> protocol_ok);
         let other = (Option.value_exn !make_manager_ref) () in
         assert (
           Result.is_error
             (A.complete_schedule
                ~moderator_manager:other
                ~expected:previous
                ~expected_schedule:claimed
                actor
                ~schedule_id:id
                ~generation:0
                ~moderator_snapshot:(Some (L.B.encode_moderator_snapshot after))));
         assert_same_session_snapshot before (A.state actor |> protocol_ok);
         (match mode with
          | `Expired -> Atomic.set original_live false
          | `Fresh_none | `Original | `Historical -> ());
         let owner =
           Owner.create
             ~actor
             ~initial:(Some (F.runtime manager worker))
             ~build:(fun () -> failwith "unexpected delivery rebuild")
         in
         Exn.protect
           ~finally:(fun () -> Owner.close_and_wait owner)
           ~f:(fun () ->
             reject_delivery := true;
             assert (Result.is_error (Owner.deliver_schedule owner claimed));
             reject_delivery := false;
             assert (!rejected = 1);
             assert_same_session_snapshot before (A.state actor |> protocol_ok);
             assert (
               Sexp.equal
                 (Session.Moderator_state.Identity_snapshot.sexp_of_t previous)
                 (Session.Moderator_state.Identity_snapshot.sexp_of_t
                    (M.identity_snapshot manager |> Result.ok_or_failwith)));
             Owner.deliver_schedule owner claimed |> protocol_ok;
             let delivered = A.state actor |> protocol_ok in
             (match mode with
              | `Original -> Legacy_checkpoint_extensions.assert_pair (checkpoints ())
              | `Fresh_none | `Expired | `Historical -> ());
             let timer =
               List.find_exn delivered.schedules ~f:(fun schedule ->
                 P.Id.Schedule.equal schedule.id id)
             in
             (match timer.status with
              | Delivered -> ()
              | Scheduled | Delivering | Cancelled | Failed _ -> assert false);
             assert (timer.delivery_count = 1);
             let entry = List.hd_exn (Q.entries (L.queue delivered)) in
             (match mode, Q.Entry.producer entry with
              | `Historical, None -> ()
              | (`Fresh_none | `Original | `Expired), Some (Schedule_delivery occurrence)
                ->
                assert (P.Id.Schedule.equal occurrence.id id);
                assert (Int.equal occurrence.generation claimed.generation);
                assert (occurrence.delivery_count = 1);
                let provenance = Option.value_exn timer.inference_provenance in
                assert (
                  P.Run_source.equal
                    (Q.source (L.queue delivered))
                    (P.Schedule_provenance.source provenance));
                assert (
                  Option.equal
                    P.Inference_origin.equal
                    (Q.Entry.origin entry)
                    (P.Schedule_provenance.origin provenance))
              | _ -> assert false);
             let entered = ref false in
             let claimed_checkpoint = ref None in
             let result =
               L.invoke
                 ~actor
                 ~manager
                 ~selection:Queue_head
                 ~operation_id:None
                 ~requests:
                   { request_turn =
                       (match mode with
                        | `Historical -> true
                        | `Fresh_none | `Original | `Expired -> false)
                   ; request_compaction = false
                   ; end_session = None
                   }
                 ~inspect:(fun scope ->
                   (match mode with
                    | `Original -> claimed_checkpoint := Some (checkpoints ())
                    | `Fresh_none | `Expired | `Historical -> ());
                   (match mode, A.callback_agent_actor actor ~scope with
                    | `Fresh_none, Ok None -> ()
                    | `Original, Ok (Some proof) ->
                      assert (Operator_authorization.is_current proof)
                    | `Historical, Error { code = Permission_denied; _ } -> ()
                    | _ -> assert false);
                   entered := true)
                 ()
             in
             match mode with
             | `Expired ->
               assert (not !entered);
               assert (Result.is_error result);
               assert_same_session_snapshot delivered (A.state actor |> protocol_ok)
             | `Historical ->
               assert !entered;
               assert (Result.is_error result);
               assert (
                 Int64.equal
                   (Q.head_ordinal (L.queue delivered))
                   (Q.head_ordinal (L.queue (A.state actor |> protocol_ok))))
             | `Fresh_none | `Original ->
               result |> Result.ok_or_failwith |> ignore;
               assert !entered;
               (match mode with
                | `Original ->
                  Legacy_checkpoint_extensions.assert_pair
                    (Option.value_exn !claimed_checkpoint);
                  Legacy_checkpoint_extensions.assert_pair (checkpoints ())
                | `Fresh_none -> ()
                | `Expired | `Historical -> assert false);
               assert (List.is_empty (Q.entries (L.queue (A.state actor |> protocol_ok)))))));
  print_endline
    "real Manager delivery ACK; current or unavailable/expired custody; rejected save \
     preserves retry; resource omission rejects";
  [%expect
    {| real Manager delivery ACK; current or unavailable/expired custody; rejected save preserves retry; resource omission rejects |}]
;;

let%expect_test
    "authenticated Actor Run attribution survives immediate legacy Schedule creation"
  =
  let module A = Agent_session in
  let module L = Legacy_emit in
  let module P = Agent_protocol in
  let rec idle actor remaining =
    if remaining = 0
    then failwith "actual Run worker did not settle within bounded actor polls";
    let state = A.Session_actor.state actor |> protocol_ok in
    match state.active_operation, state.lifecycle.observed with
    | None, Idle -> state
    | _ ->
      Eio.Fiber.yield ();
      idle actor (remaining - 1)
  in
  let actor_ready, set_actor = Eio.Promise.create () in
  let during = ref (fun (_ : A.Operation_worker.Input.t) -> ()) in
  let worker_ref = ref None in
  let timer_id = ref None in
  let callback_run = ref None in
  let worker_oracle_completed = ref false in
  Run_admission_tests.with_startup_actor
    ~make_manager:(fun env ->
      prepared_legacy_callback_managers
        env
        ~id:"legacy-run-schedule"
        ~source:Legacy_schedule.source
        ())
    ~make_worker:(fun manager ->
      let worker = L.worker manager actor_ready (fun input _ -> !during input) in
      worker_ref := Some worker;
      worker)
    (fun actor _ _ _ _ manager _ reference attachment ->
       Eio.Promise.resolve set_actor actor;
       L.install actor manager (Option.value_exn !worker_ref);
       let original = L.authentication (Atomic.make true) in
       A.Session_actor.start ~actor:original actor ~attachment_id:attachment.id
       |> protocol_ok
       |> ignore;
       idle actor 256 |> ignore;
       let before = A.Session_actor.state actor |> protocol_ok in
       let observer =
         A.Moderator_checkpoint.observer before.moderator
         |> protocol_ok
         |> Option.value_exn
       in
       let scope =
         A.Run_admission.Scope.create_authenticated
           ~actor:original
           ~observer
           ~startup_pending:(fun () -> false)
           ~authorize:(fun _ -> Ok ())
         |> protocol_ok
       in
       (* This is the authenticated Actor host seam. The daemon Run_start_service
         currently requires compiled v1 orchestration and is not exercised. *)
       (during
        := fun input ->
             L.invoke
               ~actor
               ~manager
               ~selection:(Boundary Turn_start)
               ~operation_id:(Some input.operation.id)
               ~inspect:(fun scope ->
                 let proof =
                   A.Session_actor.callback_agent_actor actor ~scope
                   |> protocol_ok
                   |> Option.value_exn
                 in
                 assert (Operator_authorization.is_current proof);
                 callback_run := A.Session_actor.Callback_scope.origin scope;
                 let services = A.Session_actor.callback_schedule_services actor ~scope in
                 timer_id
                 := Some
                      (services.after_ms ~delay_ms:0 ~payload:Legacy_schedule.payload
                       |> Result.ok_or_failwith
                       |> P.Id.Schedule.of_string
                       |> protocol_ok))
               ()
             |> Result.ok_or_failwith
             |> ignore;
             worker_oracle_completed := true);
       let request =
         P.Run_start.create
           ~session_id
           ~attachment_id:attachment.id
           ~generation:before.identity.generation
           ~expected_revision:before.counters.revision
           ~mode:Single_turn
           ~input:
             (User_submission { kind = Plain_text; text = "hello"; attachments = [] })
           ~key:(P.Idempotency_key.of_string "legacy-schedule-run" |> protocol_ok)
         |> protocol_ok
       in
       let receipt =
         A.Session_actor.admit_run
           actor
           ~scope
           ~request
           ~session:reference
           ~request_sha256:
             (P.Run_start.to_json request
              |> Jsonaf.to_string
              |> Digestif.SHA256.digest_string
              |> Digestif.SHA256.to_hex)
           ~entry:(Some actor_entry)
         |> protocol_ok
       in
       let after = idle actor 256 in
       (* Both assertions execute outside Manager and worker exception conversion. *)
       assert !worker_oracle_completed;
       assert (
         Option.equal P.Inference_origin.equal !callback_run (Some (Run receipt.run_id)));
       let run =
         A.Run_state.find (Option.value_exn after.run_state) receipt.run_id
         |> Option.value_exn
       in
       (match run.lifecycle with
        | Terminal (Completed _) -> ()
        | Admitted | Active | Waiting _
        | Terminal (Failed _ | Cancelled | Limited | Interrupted) -> assert false);
       let timer =
         List.find_exn after.schedules ~f:(fun (timer : P.Schedule.t) ->
           P.Id.Schedule.equal timer.id (Option.value_exn !timer_id))
       in
       let provenance = Option.value_exn timer.inference_provenance in
       assert (P.Run_source.equal (P.Schedule_provenance.source provenance) receipt.source);
       assert (
         Option.equal
           P.Inference_origin.equal
           (P.Schedule_provenance.origin provenance)
           (Some (Run receipt.run_id)));
       let work =
         run.owned_work
         @ run.relinquished_work
         @ List.map run.terminal_work ~f:(fun terminal ->
           terminal.P.Run_work.Terminal.work)
       in
       assert (
         not
           (List.exists work ~f:(fun work ->
              match work.P.Run_work.key with
              | Retained (Schedule _) -> true
              | Operation _
              | Retained
                  ( Job _
                  | Invocation _
                  | Subscription _
                  | Delivery _
                  | Moderator_execution _ ) -> false))));
  print_endline
    "authenticated Actor admission; real completed operation; original Run attribution \
     without Schedule work";
  [%expect
    {| authenticated Actor admission; real completed operation; original Run attribution without Schedule work |}]
;;

let%expect_test "already stopped Job loans retire only after the actual Stop ACK" =
  let module L = Legacy_emit in
  let module M = L.M in
  let module P = Agent_protocol in
  List.iter [ false; true ] ~f:(fun retry_stop ->
    with_actor_workspace (fun env workspace_instance ->
      let manager =
        prepared_legacy_callback_managers
          env
          ~id:"stopped-job"
          ~source:
            {|let initial_state = 0
              let on_event = fun ctx state event -> match event with
                | `Pre_tool_call(call) -> Task.pure(state)
                | _ -> Task.pure(state)|}
          ()
      in
      let snapshot =
        M.identity_snapshot manager
        |> Result.ok_or_failwith
        |> L.B.encode_moderator_snapshot
      in
      let reject = ref false in
      Job_fixtures.with_actor
        ~environment:(env, workspace_instance)
        ~start:false
        ~prepare_state:(fun state -> { state with moderator = Some snapshot })
        ~reject_save:(fun _ -> !reject)
        (fun _env _sw actor writer backend ->
           let worker =
             Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
               Completed
                 { final_history = input.history
                 ; moderator_snapshot = Some snapshot
                 ; runtime_requests = []
                 })
           in
           L.install actor manager worker;
           let original = L.authentication (Atomic.make true) in
           let expected = L.A.state actor |> protocol_ok in
           let request =
             P.Run_start.create
               ~session_id
               ~attachment_id:writer.id
               ~generation:expected.identity.generation
               ~expected_revision:expected.counters.revision
               ~mode:Workflow
               ~input:Authored_start
               ~key:(P.Idempotency_key.of_string "stopped-constructor-job" |> protocol_ok)
             |> protocol_ok
           in
           let request_sha256 =
             P.Run_start.to_json request
             |> Jsonaf.to_string
             |> Digestif.SHA256.digest_string
             |> Digestif.SHA256.to_hex
           in
           let preparation =
             L.A.begin_run_preparation_authenticated
               actor
               ~actor:original
               ~authorize:(fun _ -> Ok ())
               ~request
               ~request_sha256
             |> protocol_ok
             |> function
             | Agent_session.Run_preparation.Decision.Prepare preparation -> preparation
             | Retained _ -> failwith "fresh constructor unexpectedly replayed"
           in
           let job : P.Job.t =
             { id = P.Id.Job.create ()
             ; session_id
             ; generation = expected.identity.generation
             ; kind = Model_call
             ; payload = `Null
             ; status = Queued
             ; retry_policy = Never
             ; attempt = 0
             ; created_at = timestamp
             ; started_at = None
             ; next_run_at = None
             ; completed_at = None
             ; result = None
             ; delivery = Pending
             ; launch = None
             ; inference_origin = None
             ; progress = None
             }
           in
           let job = L.A.add_run_constructor_job actor ~preparation job |> protocol_ok in
           L.A.end_run_preparation actor preparation |> protocol_ok;
           let before = Agent_session.Memory_backend.state backend in
           reject := true;
           (match L.A.stop actor ~attachment_id:writer.id ~mode:Graceful with
            | Error failure ->
              [%test_eq: P.Error.code] Internal_error failure.code;
              [%test_eq: string] "injected job transition failure" failure.message
            | Ok _ ->
              failwith "already-stopped Stop skipped its required loan retirement ACK");
           reject := false;
           [%test_eq: int64]
             before.counters.revision
             (Agent_session.Memory_backend.state backend).counters.revision;
           if retry_stop
           then (
             L.A.stop actor ~attachment_id:writer.id ~mode:Graceful
             |> protocol_ok
             |> ignore;
             assert (
               Int64.(
                 before.counters.revision
                 < (Agent_session.Memory_backend.state backend).counters.revision)));
           L.A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
           let job =
             L.A.claim_job actor ~job_id:job.id ~generation:job.generation
             |> protocol_ok
             |> Option.value_exn
           in
           let observed = ref None in
           let call : Chat_response.Moderation.Tool_call.t =
             { id = "stopped-job-tool"
             ; name = "read_file"
             ; args = `Null
             ; kind = Function
             ; payload_text = "null"
             ; meta = `Null
             }
           in
           let result =
             L.A.with_job_execution
               actor
               ~job_id:job.id
               ~generation:job.generation
               ~attempt:job.attempt
               ~deadline:(Some Job_fixtures.deadline)
               (fun services ->
                  L.invoke
                    ~claim:services.claim_legacy_callback
                    ~actor
                    ~manager
                    ~selection:(Boundary (Pre_tool_call call))
                    ~operation_id:None
                    ~inspect:(fun scope ->
                      let borrowed = L.A.callback_agent_actor actor ~scope in
                      observed
                      := Some
                           ( borrowed
                           , match borrowed with
                             | Ok (Some proof) -> Operator_authorization.is_current proof
                             | Ok None | Error _ -> false ))
                    ()
                  |> Result.map_error ~f:handoff_error)
             |> protocol_ok
           in
           ignore result;
           (match retry_stop, Option.value_exn !observed with
            | false, (Ok (Some proof), true) ->
              assert (not (Operator_authorization.is_current proof))
            | true, (Error failure, false) ->
              [%test_eq: P.Error.code] Permission_denied failure.code
            | _, (Error failure, _) -> raise_s [%sexp (failure : P.Error.t)]
            | _, (Ok _, _) ->
              failwith "Stop loan retirement changed genuine/unavailable classification");
           assert (Operator_authorization.is_current original))));
  print_endline
    "failed Stop preserves genuine Job admission; acknowledged already-stopped Stop \
     leaves later callback unavailable";
  [%expect
    {| failed Stop preserves genuine Job admission; acknowledged already-stopped Stop leaves later callback unavailable |}]
;;

let%expect_test "constructor handoff preserves exact checkpoints and intended resources" =
  let module L = Legacy_emit in
  let module S = Agent_session in
  let module P = Agent_protocol in
  with_actor_workspace (fun env workspace_instance ->
    let make_manager () =
      prepared_legacy_callback_managers
        env
        ~id:"constructor-resource"
        ~source:
          {|let initial_state = 0
            let on_event = fun ctx state event -> match event with
              | `Pre_tool_call(call) -> Task.pure(state)
              | _ -> Task.pure(state)|}
        ()
    in
    let manager = make_manager () in
    let snapshot = L.M.identity_snapshot manager |> Result.ok_or_failwith in
    let bare = S.Moderator_checkpoint.encode snapshot in
    let reject = ref false in
    Job_fixtures.with_actor
      ~environment:(env, workspace_instance)
      ~start:false
      ~prepare_state:(fun state ->
        { state with runtime_initialization = Pending { fresh_history = true } })
      ~reject_save:(fun _ -> !reject)
      (fun _env _sw actor writer backend ->
         let original = L.authentication (Atomic.make true) in
         let expected = L.A.state actor |> protocol_ok in
         let scope =
           L.A.begin_initialization_authenticated actor ~actor:original ~expected
           |> protocol_ok
         in
         let job delivery : P.Job.t =
           { id = P.Id.Job.create ()
           ; session_id
           ; generation = expected.identity.generation
           ; kind = Model_call
           ; payload = `Null
           ; status = Queued
           ; retry_policy = Never
           ; attempt = 0
           ; created_at = timestamp
           ; started_at = None
           ; next_run_at = None
           ; completed_at = None
           ; result = None
           ; delivery
           ; launch = None
           ; inference_origin = None
           ; progress = None
           }
         in
         let pending =
           L.A.add_initialization_model_job actor ~scope (job Pending) |> protocol_ok
         in
         let synchronous =
           L.A.start_initialization_model_job actor ~scope (job Not_required)
           |> protocol_ok
         in
         L.A.complete_initialization_model_job
           actor
           ~scope
           ~job_id:synchronous.id
           ~generation:synchronous.generation
           ~attempt:synchronous.attempt
           (S.Runtime_builder.Model_succeeded `Null)
         |> protocol_ok
         |> ignore;
         let candidate = { expected with moderator = Some bare } in
         let before = S.Memory_backend.state backend in
         reject := true;
         (match
            L.A.complete_initialization ~moderator_manager:manager actor ~scope ~candidate
          with
          | Error failure ->
            [%test_eq: P.Error.code] Internal_error failure.code;
            [%test_eq: string] "injected job transition failure" failure.message
          | Ok _ -> failwith "constructor handoff escaped the rejected ACK");
         reject := false;
         let rejected = S.Memory_backend.state backend in
         [%test_eq: int64] before.counters.revision rejected.counters.revision;
         assert (Option.is_none rejected.moderator);
         assert (Option.is_none rejected.run_state);
         assert (Operator_authorization.is_current original);
         L.A.complete_initialization ~moderator_manager:manager actor ~scope ~candidate
         |> protocol_ok
         |> ignore;
         L.A.end_initialization actor ~scope |> protocol_ok;
         let initialized = L.A.state actor |> protocol_ok in
         let index = Option.value_exn initialized.run_state in
         assert (List.is_empty (S.Run_state.runs index));
         let source =
           S.Run_source_installation.captured
             (S.Run_state.installation index)
             ~generation:initialized.identity.generation
           |> protocol_ok
         in
         let queue = S.Moderator_queue.bootstrap ~source ~queue:[] |> protocol_ok in
         let full =
           S.Moderator_checkpoint.encode_with_queue snapshot ~queue
           |> protocol_ok
           |> function
           | `Object fields ->
             `Object
               (List.map fields ~f:(fun (name, value) ->
                  if String.equal name "queue_provenance"
                  then
                    ( name
                    , match value with
                      | `Object queue_fields ->
                        `Object (queue_fields @ [ "future_queue", `String "retained" ])
                      | _ -> assert false )
                  else name, value)
                @ [ "future_checkpoint", `String "retained" ])
           | _ -> assert false
         in
         (* This is an actual same-source host checkpoint. It adds only structural
            empty-queue facts, not a producer, original proof or Run. *)
         L.A.change_moderator ~actor:original actor (Some full) |> protocol_ok |> ignore;
         let expected = L.A.state actor |> protocol_ok in
         let request =
           P.Run_start.create
             ~session_id
             ~attachment_id:writer.id
             ~generation:expected.identity.generation
             ~expected_revision:expected.counters.revision
             ~mode:Workflow
             ~input:Authored_start
             ~key:(P.Idempotency_key.of_string "resource-handoff" |> protocol_ok)
           |> protocol_ok
         in
         let request_sha256 =
           P.Run_start.to_json request
           |> Jsonaf.to_string
           |> Digestif.SHA256.digest_string
           |> Digestif.SHA256.to_hex
         in
         let preparation =
           L.A.begin_run_preparation_authenticated
             actor
             ~actor:original
             ~authorize:(fun _ -> Ok ())
             ~request
             ~request_sha256
           |> protocol_ok
           |> function
           | S.Run_preparation.Decision.Prepare preparation -> preparation
           | Retained _ -> failwith "fresh owner unexpectedly retained"
         in
         let ready =
           L.A.add_run_constructor_job actor ~preparation (job Pending) |> protocol_ok
         in
         let checkpoint () =
           L.A.checkpoint_run_moderator
             ~moderator_manager:manager
             actor
             ~preparation
             (Some bare)
         in
         let before = S.Memory_backend.state backend in
         reject := true;
         (match checkpoint () with
          | Error failure -> [%test_eq: P.Error.code] Internal_error failure.code
          | Ok _ -> failwith "Ready handoff escaped the rejected ACK");
         reject := false;
         [%test_eq: int64]
           before.counters.revision
           (S.Memory_backend.state backend).counters.revision;
         checkpoint () |> protocol_ok |> ignore;
         let handed_off = L.A.state actor |> protocol_ok in
         assert (Option.equal Jsonaf.exactly_equal handed_off.moderator (Some full));
         checkpoint () |> protocol_ok |> ignore;
         [%test_eq: int64]
           handed_off.counters.revision
           (L.A.state actor |> protocol_ok).counters.revision;
         L.A.end_run_preparation actor preparation |> protocol_ok;
         let worker =
           S.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
             Completed
               { final_history = input.history
               ; moderator_snapshot = Some full
               ; runtime_requests = []
               })
         in
         L.install actor manager worker;
         (* The setter's intermediate absent Manager must not retire rows just
            acknowledged for this exact intended final resource. *)
         L.install actor manager worker;
         assert (
           Option.equal
             Jsonaf.exactly_equal
             (L.A.state actor |> protocol_ok).moderator
             (Some full));
         L.A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
         let claim (job : P.Job.t) =
           L.A.claim_job actor ~job_id:job.id ~generation:job.generation
           |> protocol_ok
           |> Option.value_exn
         in
         let pending = claim pending in
         let ready = claim ready in
         let inspect manager (job : P.Job.t) =
           let observed = ref None in
           let call : Chat_response.Moderation.Tool_call.t =
             { id = "constructor-resource-tool"
             ; name = "read_file"
             ; args = `Null
             ; kind = Function
             ; payload_text = "null"
             ; meta = `Null
             }
           in
           L.A.with_job_execution
             actor
             ~job_id:job.id
             ~generation:job.generation
             ~attempt:job.attempt
             ~deadline:(Some Job_fixtures.deadline)
             (fun services ->
                L.invoke
                  ~claim:services.claim_legacy_callback
                  ~actor
                  ~manager
                  ~selection:(Boundary (Pre_tool_call call))
                  ~operation_id:None
                  ~inspect:(fun scope ->
                    let borrowed = L.A.callback_agent_actor actor ~scope in
                    observed
                    := Some
                         ( borrowed
                         , match borrowed with
                           | Ok (Some proof) -> Operator_authorization.is_current proof
                           | Ok None | Error _ -> false ))
                  ()
                |> Result.map_error ~f:handoff_error)
           |> protocol_ok
           |> ignore;
           Option.value_exn !observed
         in
         List.iter [ pending; ready ] ~f:(fun job ->
           (match inspect manager job with
            | Ok (Some proof), true ->
              assert (not (Operator_authorization.is_current proof))
            | Error failure, _ -> raise_s [%sexp (failure : P.Error.t)]
            | Ok _, _ -> failwith "acknowledged constructor lost its exact live original");
           assert (
             Option.equal
               Jsonaf.exactly_equal
               (L.A.state actor |> protocol_ok).moderator
               (Some full));
           assert (
             Option.equal
               Jsonaf.exactly_equal
               (S.Memory_backend.state backend).moderator
               (Some full)));
         let replacement = make_manager () in
         L.install actor replacement worker;
         (match inspect replacement ready with
          | Error failure, false ->
            [%test_eq: P.Error.code] Permission_denied failure.code
          | Error failure, true -> raise_s [%sexp (failure : P.Error.t)]
          | Ok _, _ -> failwith "replacement resource revived the old Job loan");
         assert (Operator_authorization.is_current original)));
  print_endline
    "Pending and Ready ACK handoffs; exact checkpoint custody; intended resource \
     retained, replacement unavailable";
  [%expect
    {| Pending and Ready ACK handoffs; exact checkpoint custody; intended resource retained, replacement unavailable |}]
;;

let%expect_test "actual Model completion delivery retains exact terminal custody" =
  let module L = Legacy_emit in
  let module A = L.A in
  let module S = Agent_session in
  let module P = Agent_protocol in
  let module Owner = Agent_server.Runtime_owner in
  List.iter [ `Original; `Fresh_none; `Expired; `Historical ] ~f:(fun mode ->
    with_actor_workspace (fun env workspace_instance ->
      let manager =
        prepared_legacy_callback_managers
          env
          ~id:"model-delivery"
          ~source:
            {|let initial_state = 0
              let on_event = fun ctx state event -> Task.pure(state)|}
          ()
      in
      let snapshot = L.M.identity_snapshot manager |> Result.ok_or_failwith in
      let bare = L.B.encode_moderator_snapshot snapshot in
      let reject_delivery = ref false in
      let rejected = ref 0 in
      Job_fixtures.with_actor
        ~environment:(env, workspace_instance)
        ~start:false
        ~prepare_state:(fun state ->
          { state with runtime_initialization = Pending { fresh_history = true } })
        ~reject_save:(fun next ->
          if
            !reject_delivery
            && List.exists next.S.Session_transition.state.jobs ~f:(fun job ->
              match job.P.Job.delivery with
              | Delivered _ -> true
              | Not_required | Pending | Discarded _ -> false)
          then (
            Int.incr rejected;
            true)
          else false)
        (fun _env _sw actor writer backend ->
           let original_live = Atomic.make true in
           let original = L.authentication original_live in
           let expected = A.state actor |> protocol_ok in
           let initialization =
             (match mode with
              | `Fresh_none -> A.begin_initialization actor ~expected
              | `Original | `Expired | `Historical ->
                A.begin_initialization_authenticated actor ~actor:original ~expected)
             |> protocol_ok
           in
           let job : P.Job.t =
             { id = P.Id.Job.create ()
             ; session_id
             ; generation = expected.identity.generation
             ; kind = Model_call
             ; payload = `Object [ "recipe", `String "owner"; "payload", `Null ]
             ; status = Queued
             ; retry_policy = Never
             ; attempt = 0
             ; created_at = timestamp
             ; started_at = None
             ; next_run_at = None
             ; completed_at = None
             ; result = None
             ; delivery = Pending
             ; launch = None
             ; inference_origin = None
             ; progress = None
             }
           in
           let job =
             match mode with
             | `Historical -> job
             | `Original | `Fresh_none | `Expired ->
               A.add_initialization_model_job actor ~scope:initialization job
               |> protocol_ok
           in
           A.complete_initialization
             ~moderator_manager:manager
             actor
             ~scope:initialization
             ~candidate:{ expected with moderator = Some bare }
           |> protocol_ok
           |> ignore;
           A.end_initialization actor ~scope:initialization |> protocol_ok;
           let cancelled =
             match mode with
             | `Original ->
               let terminal = A.cancel_job_internal actor ~job_id:job.id |> protocol_ok in
               let before = A.state actor |> protocol_ok in
               (match
                  A.deliver_job
                    actor
                    ~job_id:job.id
                    ~generation:job.generation
                    ~moderator_snapshot:(Some bare)
                with
                | Error failure -> [%test_eq: P.Error.code] Conflict failure.code
                | Ok _ -> failwith "missing resource discarded a positive terminal loan");
               assert_same_session_snapshot before (A.state actor |> protocol_ok);
               assert_same_session_snapshot before (S.Memory_backend.state backend);
               Some terminal
             | `Fresh_none | `Expired | `Historical -> None
           in
           let worker =
             S.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
               Completed
                 { final_history = input.history
                 ; moderator_snapshot = Some bare
                 ; runtime_requests = []
                 })
           in
           L.install actor manager worker;
           A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
           let terminal =
             match mode, cancelled with
             | `Original, Some terminal -> terminal
             | (`Fresh_none | `Expired), None ->
               let claimed =
                 A.claim_job actor ~job_id:job.id ~generation:job.generation
                 |> protocol_ok
                 |> Option.value_exn
               in
               A.complete_job
                 actor
                 ~job_id:job.id
                 ~generation:job.generation
                 ~attempt:claimed.attempt
                 (L.B.Model_succeeded (`String "done"))
               |> protocol_ok
             | `Historical, None ->
               (* Imported terminal data has no live admission. This deliberately
                 exercises the historical data route, not execution permission. *)
               A.add_job
                 actor
                 { job with
                   status = Succeeded
                 ; attempt = 1
                 ; started_at = Some timestamp
                 ; completed_at = Some timestamp
                 ; result = Some (`String "done")
                 }
               |> protocol_ok
             | `Original, None | (`Fresh_none | `Expired | `Historical), Some _ ->
               assert false
           in
           let state = A.state actor |> protocol_ok in
           let index = Option.value_exn state.run_state in
           let source =
             S.Run_source_installation.captured
               (S.Run_state.installation index)
               ~generation:state.identity.generation
             |> protocol_ok
           in
           let metadata = S.Moderator_queue.bootstrap ~source ~queue:[] |> protocol_ok in
           let full =
             S.Moderator_checkpoint.encode_with_queue snapshot ~queue:metadata
             |> protocol_ok
           in
           let full =
             match mode with
             | `Original -> Legacy_checkpoint_extensions.annotate full
             | `Fresh_none | `Expired | `Historical -> full
           in
           A.change_moderator actor (Some full) |> protocol_ok |> ignore;
           let checkpoints () =
             ( (A.state actor |> protocol_ok).moderator
             , (S.Memory_backend.state backend).moderator )
           in
           let before = A.state actor |> protocol_ok in
           (match
              A.deliver_job
                ~expected:snapshot
                ~expected_job:terminal
                actor
                ~job_id:terminal.id
                ~generation:terminal.generation
                ~moderator_snapshot:(Some bare)
            with
            | Error failure -> [%test_eq: P.Error.code] Conflict failure.code
            | Ok _ -> failwith "metadata delivery omitted the actual Manager");
           assert_same_session_snapshot before (A.state actor |> protocol_ok);
           (match mode with
            | `Expired -> Atomic.set original_live false
            | `Original | `Fresh_none | `Historical -> ());
           let runtime = Legacy_schedule.runtime manager worker in
           let runtime =
             { runtime with
               enqueue_model_job_completion =
                 (fun ?prepare job ->
                   let open Result.Let_syntax in
                   let%bind value = L.B.model_job_event job in
                   let%bind payload =
                     Session.Snapshot.of_value value
                     |> Result.map_error ~f:P.Error.invalid_request
                   in
                   runtime.enqueue_internal_event
                     ?prepare
                     (Session.Snapshot.to_jsonaf payload))
             }
           in
           let owner =
             Owner.create ~actor ~initial:(Some runtime) ~build:(fun () ->
               failwith "unexpected Model delivery rebuild")
           in
           Exn.protect
             ~finally:(fun () -> Owner.close_and_wait owner)
             ~f:(fun () ->
               reject_delivery := true;
               assert (Result.is_error (Owner.deliver_model_job_completion owner terminal));
               reject_delivery := false;
               [%test_eq: int] 1 !rejected;
               assert_same_session_snapshot before (A.state actor |> protocol_ok);
               assert_same_session_snapshot before (S.Memory_backend.state backend);
               assert (
                 Sexp.equal
                   (Session.Moderator_state.Identity_snapshot.sexp_of_t snapshot)
                   (Session.Moderator_state.Identity_snapshot.sexp_of_t
                      (L.M.identity_snapshot manager |> Result.ok_or_failwith)));
               Owner.deliver_model_job_completion owner terminal |> protocol_ok;
               let delivered = A.state actor |> protocol_ok in
               (match mode with
                | `Original -> Legacy_checkpoint_extensions.assert_pair (checkpoints ())
                | `Fresh_none | `Expired | `Historical -> ());
               let saved =
                 List.find_exn delivered.jobs ~f:(fun current ->
                   P.Id.Job.equal current.id terminal.id)
               in
               (match saved.delivery with
                | Delivered _ -> ()
                | Not_required | Pending | Discarded _ -> assert false);
               assert (
                 Option.equal
                   P.Inference_origin.equal
                   saved.inference_origin
                   terminal.inference_origin);
               let entries = S.Moderator_queue.entries (L.queue delivered) in
               let entry = List.hd_exn entries in
               [%test_eq: int] 1 (List.length entries);
               (match mode, S.Moderator_queue.Entry.producer entry with
                | `Historical, None ->
                  assert (Option.is_none (S.Moderator_queue.Entry.origin entry))
                | (`Original | `Fresh_none | `Expired), Some (Job_completion producer) ->
                  assert (P.Id.Job.equal producer.id terminal.id);
                  [%test_eq: int] terminal.attempt producer.attempt
                | _ -> failwith "Model delivery changed original producer evidence");
               let observed = ref None in
               let claimed_checkpoint = ref None in
               let result =
                 L.invoke
                   ~actor
                   ~manager
                   ~selection:Queue_head
                   ~operation_id:None
                   ~inspect:(fun scope ->
                     (match mode with
                      | `Original -> claimed_checkpoint := Some (checkpoints ())
                      | `Fresh_none | `Expired | `Historical -> ());
                     let borrowed = A.callback_agent_actor actor ~scope in
                     observed
                     := Some
                          ( borrowed
                          , match borrowed with
                            | Ok (Some proof) -> Operator_authorization.is_current proof
                            | Ok None | Error _ -> false ))
                   ()
               in
               (match mode, !observed with
                | `Expired, None ->
                  assert (Result.is_error result);
                  assert_same_session_snapshot delivered (A.state actor |> protocol_ok)
                | `Original, Some (Ok (Some proof), true) ->
                  result |> Result.ok_or_failwith |> ignore;
                  Legacy_checkpoint_extensions.assert_pair
                    (Option.value_exn !claimed_checkpoint);
                  Legacy_checkpoint_extensions.assert_pair (checkpoints ());
                  assert (not (Operator_authorization.is_current proof))
                | `Fresh_none, Some (Ok None, false) ->
                  result |> Result.ok_or_failwith |> ignore
                | `Historical, Some (Error failure, false) ->
                  [%test_eq: P.Error.code] Permission_denied failure.code;
                  result |> Result.ok_or_failwith |> ignore
                | _, Some (Error failure, _) -> raise_s [%sexp (failure : P.Error.t)]
                | _ ->
                  failwith
                    "terminal Model continuation changed live/expired/unavailable custody");
               match mode with
               | `Original | `Fresh_none | `Historical ->
                 assert (Operator_authorization.is_current original)
               | `Expired -> assert (not (Operator_authorization.is_current original))))));
  print_endline
    "actual Model payload and ACK retry; fresh None, current original, explicit expired \
     and historical unavailable";
  [%expect
    {| actual Model payload and ACK retry; fresh None, current original, explicit expired and historical unavailable |}]
;;

let%expect_test "Prepared Async dependency delivery retains the parent's original" =
  let module L = Legacy_emit in
  let module A = L.A in
  let module S = Agent_session in
  let module P = Agent_protocol in
  let module I = P.Invocation in
  let module Owner = Agent_server.Runtime_owner in
  with_actor_workspace (fun env workspace_instance ->
    let manager =
      prepared_legacy_callback_managers
        env
        ~id:"async-dependency"
        ~source:
          {|let initial_state = 0
                  let on_event = fun ctx state event -> Task.pure(state)|}
        ()
    in
    let snapshot = L.M.identity_snapshot manager |> Result.ok_or_failwith in
    let bare = L.B.encode_moderator_snapshot snapshot in
    Job_fixtures.with_actor
      ~environment:(env, workspace_instance)
      ~start:false
      ~prepare_state:(fun state ->
        { state with runtime_initialization = Pending { fresh_history = true } })
      (fun _env _sw actor writer backend ->
         let original = L.authentication (Atomic.make true) in
         let expected = A.state actor |> protocol_ok in
         let initialization =
           A.begin_initialization_authenticated actor ~actor:original ~expected
           |> protocol_ok
         in
         let anchor : P.Job.t =
           { id = P.Id.Job.create ()
           ; session_id
           ; generation = expected.identity.generation
           ; kind = Model_call
           ; payload = `Object [ "recipe", `String "owner" ]
           ; status = Queued
           ; retry_policy = Never
           ; attempt = 0
           ; created_at = timestamp
           ; started_at = None
           ; next_run_at = None
           ; completed_at = None
           ; result = None
           ; delivery = Not_required
           ; launch = None
           ; inference_origin = None
           ; progress = None
           }
         in
         let anchor =
           A.add_initialization_model_job actor ~scope:initialization anchor
           |> protocol_ok
         in
         A.complete_initialization
           ~moderator_manager:manager
           actor
           ~scope:initialization
           ~candidate:{ expected with moderator = Some bare }
         |> protocol_ok
         |> ignore;
         A.end_initialization actor ~scope:initialization |> protocol_ok;
         let worker =
           S.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
             Completed
               { final_history = input.history
               ; moderator_snapshot = Some bare
               ; runtime_requests = []
               })
         in
         L.install actor manager worker;
         A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
         let claim (job : P.Job.t) =
           A.claim_job actor ~job_id:job.id ~generation:job.generation
           |> protocol_ok
           |> Option.value_exn
         in
         let anchor = claim anchor in
         let observer : I.observer =
           { script_id = snapshot.script_id; source_sha256 = snapshot.script_source_hash }
         in
         let registry = native_registry (ref 0) ~raises:false in
         let request =
           Background_execution_tests.capture_tool
             registry
             Chat_response.One_off_request.default_policy
         in
         let published = ref [] in
         let stage_child (parent : P.Job.t) =
           let child = ref None in
           let pending_invocation = ref None in
           let root = Job_fixtures.root parent in
           let resolved =
             A.with_job_execution
               actor
               ~job_id:parent.id
               ~generation:parent.generation
               ~attempt:parent.attempt
               ~deadline:(Some Job_fixtures.deadline)
               (fun services ->
                  services.execute
                    ~invocation:root
                    (fun ~dispatched:root ~borrow_agent_actor:_ ->
                       let nested =
                         I.create
                           ~observer
                           { root.context with
                             id = P.Id.Invocation.create ()
                           ; parent_job = None
                           ; parent_invocation = Some root.context.id
                           ; tool_name = "counter"
                           }
                         |> protocol_ok
                       in
                       let open Result.Let_syntax in
                       let%map () =
                         services.moderator_execute
                           ~invocation:nested
                           (fun ~dispatched ~borrow_agent_actor:_ ~commit ->
                              let owner = P.Job.Invocation dispatched.context.id in
                              let next =
                                A.prepare_background_job_launch actor ~owner request
                                |> protocol_ok
                              in
                              A.stage_background_job
                                actor
                                ~job:next
                                ~capacity:
                                  { publish =
                                      (fun () -> published := next.id :: !published)
                                  ; abort =
                                      (fun () ->
                                        failwith
                                          "selected dependency unexpectedly aborted")
                                  }
                              |> protocol_ok;
                              child := Some next;
                              pending_invocation := Some dispatched.context.id;
                              A.select_background_jobs actor ~owner ~ids:[ next.id ]
                              |> protocol_ok;
                              let resolved =
                                I.resolve
                                  dispatched
                                  ~session_id
                                  ~generation:parent.generation
                                  (Pending (Job next.id, `Null))
                                |> protocol_ok
                              in
                              commit ~resolved ~snapshot)
                       in
                       I.Complete `Null))
             |> protocol_ok
           in
           (match resolved.status with
            | Resolved (Complete `Null) -> ()
            | _ ->
              failwith "actual parent callback did not acknowledge its nested selection");
           let child = Option.value_exn !child in
           let saved =
             List.find_exn (S.Memory_backend.state backend).jobs ~f:(fun job ->
               P.Id.Job.equal job.id child.id)
           in
           let launch = Option.value_exn saved.launch in
           assert (Option.equal I.equal_observer launch.moderator_source (Some observer));
           assert (
             Option.equal
               (fun (id, attempt) (expected_id, expected_attempt) ->
                  P.Id.Job.equal id expected_id && Int.equal attempt expected_attempt)
               launch.parent_job
               (Some (parent.id, parent.attempt)));
           saved, Option.value_exn !pending_invocation
         in
         let parent, _ = stage_child anchor in
         let parent = claim parent in
         let child, invocation_id = stage_child parent in
         let dependency : P.Job.dependency =
           { invocation_id
           ; work = Job child.id
           ; deadline = Job_fixtures.deadline
           ; completion_schema = None
           ; max_output_bytes = 1_000_000
           ; max_output_depth = 128
           }
         in
         let waiting =
           A.defer_background_job
             actor
             ~job_id:parent.id
             ~generation:parent.generation
             ~attempt:parent.attempt
             dependency
           |> protocol_ok
         in
         (match waiting.status with
          | Waiting_completion actual -> assert (P.Job.equal_dependency dependency actual)
          | _ -> failwith "actual dependency did not enter Waiting_completion");
         let child = claim child in
         A.complete_background_job
           actor
           ~job_id:child.id
           ~generation:child.generation
           ~attempt:child.attempt
           (Succeeded `Null)
         |> protocol_ok
         |> ignore;
         let terminal =
           A.refresh_background_job
             actor
             ~job_id:parent.id
             ~generation:parent.generation
             ~attempt:parent.attempt
           |> protocol_ok
         in
         (match terminal.status with
          | Succeeded -> ()
          | _ -> failwith "actual child completion did not settle the waiting parent");
         [%test_eq: int] 2 (List.length !published);
         assert (List.mem !published parent.id ~equal:P.Id.Job.equal);
         assert (List.mem !published child.id ~equal:P.Id.Job.equal);
         let state = A.state actor |> protocol_ok in
         let index = Option.value_exn state.run_state in
         let source =
           S.Run_source_installation.captured
             (S.Run_state.installation index)
             ~generation:state.identity.generation
           |> protocol_ok
         in
         let metadata = S.Moderator_queue.bootstrap ~source ~queue:[] |> protocol_ok in
         A.change_moderator
           actor
           (Some
              (S.Moderator_checkpoint.encode_with_queue snapshot ~queue:metadata
               |> protocol_ok))
         |> protocol_ok
         |> ignore;
         let frame =
           S.Background_job_event.frame
             ~state:(A.state actor |> protocol_ok)
             ~observer
             terminal
           |> protocol_ok
         in
         let payload =
           Chat_response.Background_delivery.capture frame
           |> Session.Snapshot.of_value
           |> Result.ok_or_failwith
         in
         let owner =
           Owner.create
             ~actor
             ~initial:(Some (Legacy_schedule.runtime manager worker))
             ~build:(fun () -> failwith "unexpected Async dependency rebuild")
         in
         Exn.protect
           ~finally:(fun () -> Owner.close_and_wait owner)
           ~f:(fun () ->
             Owner.deliver_background_job_completion owner terminal |> protocol_ok;
             let delivered = A.state actor |> protocol_ok in
             let actual_snapshot =
               S.Moderator_checkpoint.decode delivered.moderator
               |> protocol_ok
               |> Option.value_exn
             in
             assert (
               List.equal
                 (fun left right ->
                    Sexp.equal
                      (Session.Snapshot.sexp_of_t left)
                      (Session.Snapshot.sexp_of_t right))
                 actual_snapshot.queued_internal_events
                 [ payload ]);
             let entry = List.hd_exn (S.Moderator_queue.entries (L.queue delivered)) in
             (match S.Moderator_queue.Entry.producer entry with
              | Some (Job_completion actual) ->
                assert (P.Id.Job.equal actual.id parent.id);
                [%test_eq: int] parent.attempt actual.attempt
              | Some _ | None ->
                failwith "dependency delivery lost its actual parent producer");
             assert (
               Option.equal
                 P.Inference_origin.equal
                 (S.Moderator_queue.Entry.origin entry)
                 terminal.inference_origin);
             let borrowed = ref None in
             L.invoke
               ~actor
               ~manager
               ~selection:Queue_head
               ~operation_id:None
               ~inspect:(fun scope ->
                 let proof = A.callback_agent_actor actor ~scope in
                 borrowed
                 := Some
                      ( proof
                      , match proof with
                        | Ok (Some proof) -> Operator_authorization.is_current proof
                        | Ok None | Error _ -> false ))
               ()
             |> Result.ok_or_failwith
             |> ignore;
             (* This oracle is outside every engine/callback exception converter.
             The child has finished: the proof must come from the parent row
             retained through Waiting_completion and terminal delivery. *)
             (match Option.value_exn !borrowed with
              | Ok (Some proof), true ->
                assert (
                  P.Id.Principal.equal
                    (Operator_authorization.principal proof).id
                    (Operator_authorization.principal original).id);
                assert (not (Operator_authorization.is_current proof))
              | Error failure, _ -> raise_s [%sexp (failure : P.Error.t)]
              | Ok _, _ ->
                failwith "deferred parent's original loan did not reach the queue");
             assert (Operator_authorization.is_current original))));
  print_endline
    "actual staged parent and child; dependency resolution; real Async frame; parent \
     original current inside queue borrow";
  [%expect
    {| actual staged parent and child; dependency resolution; real Async frame; parent original current inside queue borrow |}]
;;

let%expect_test "constructor callbacks hand off their exact emitted and timer custody" =
  let module L = Legacy_emit in
  let module A = L.A in
  let module M = L.M in
  let module S = Agent_session in
  let module P = Agent_protocol in
  let module Q = S.Moderator_queue in
  let module Owner = Agent_server.Runtime_owner in
  List.iter [ `Pending; `Ready ] ~f:(fun mode ->
    with_actor_workspace (fun env workspace_instance ->
      let manager =
        prepared_legacy_callback_managers
          env
          ~id:"constructor-callback"
          ~source:
            {|type state = int
              type event = [ `Session_start | `Session_resume | `Fresh | `Tick ]
              let initial_state = 0
              let on_event : context -> state -> event -> state task = fun ctx st ev ->
                match ev with
                | `Session_start ->
                  Task.bind(Schedule.after_ms(0, `Tick), fun timer ->
                    Task.bind(Runtime.emit(`Fresh), fun ignored -> Task.pure(st + 1)))
                | `Session_resume ->
                  Task.bind(Schedule.after_ms(0, `Tick), fun timer ->
                    Task.bind(Runtime.emit(`Fresh), fun ignored -> Task.pure(st + 1)))
                | `Fresh -> Task.pure(st + 1)
                | `Tick -> Task.pure(st + 1)|}
          ()
      in
      Job_fixtures.with_actor
        ~environment:(env, workspace_instance)
        ~start:false
        ~prepare_state:(fun state ->
          match mode with
          | `Pending ->
            { state with runtime_initialization = Pending { fresh_history = true } }
          | `Ready -> state)
        (fun _env _sw actor writer backend ->
           let original = L.authentication (Atomic.make true) in
           let expected = A.state actor |> protocol_ok in
           let scope =
             A.begin_initialization_authenticated
               ~purpose:
                 (match mode with
                  | `Pending -> Pending_initialization
                  | `Ready -> Runtime_construction)
               actor
               ~actor:original
               ~expected
             |> protocol_ok
           in
           let borrow selection ?claim () =
             let observed = ref None in
             L.invoke
               ?claim
               ~actor
               ~manager
               ~selection
               ~operation_id:None
               ~inspect:(fun scope ->
                 let proof = A.callback_agent_actor actor ~scope in
                 observed
                 := Some
                      ( scope
                      , proof
                      , match proof with
                        | Ok (Some proof) -> Operator_authorization.is_current proof
                        | Ok None | Error _ -> false ))
               ()
             |> Result.ok_or_failwith
             |> ignore;
             (* Inspect results outside the Manager's exception conversion. *)
             let callback, proof, current = Option.value_exn !observed in
             (match proof, current with
              | Ok (Some proof), true ->
                assert (not (Operator_authorization.is_current proof))
              | Error failure, _ -> raise_s [%sexp (failure : P.Error.t)]
              | Ok _, _ -> failwith "constructor original was not current inside callback");
             assert (Result.is_error (A.check_callback_current actor callback));
             assert (Operator_authorization.is_current original)
           in
           borrow
             (Boundary
                (match mode with
                 | `Pending -> Session_start
                 | `Ready -> Session_resume))
             ~claim:(A.with_initialization_callback actor ~scope ~manager)
             ();
           let emitted = A.state actor |> protocol_ok in
           assert_same_session_snapshot emitted (S.Memory_backend.state backend);
           assert (List.is_empty (S.Run_state.runs (Option.value_exn emitted.run_state)));
           let receipt = List.hd_exn emitted.moderator_executions in
           (match receipt.P.Moderator_execution.status with
            | Completed _ -> ()
            | Running | Failed _ | Interrupted _ -> assert false);
           let queue = L.queue emitted in
           let entry = List.hd_exn (Q.entries queue) in
           (match Q.Entry.producer entry with
            | Some (Event id) ->
              assert (P.Id.Moderator_execution.equal id receipt.context.id)
            | None
            | Some (Invocation _ | Job_completion _ | Schedule_delivery _ | Source_setup _)
              -> assert false);
           assert (
             Option.equal P.Inference_origin.equal (Q.Entry.origin entry) (Some Unbound));
           let timer = List.hd_exn emitted.schedules in
           let provenance = Option.value_exn timer.inference_provenance in
           assert (
             P.Run_source.equal (Q.source queue) (P.Schedule_provenance.source provenance));
           assert (
             Option.equal
               P.Inference_origin.equal
               (P.Schedule_provenance.origin provenance)
               (Some Unbound));
           let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
           let bare = Some (L.B.encode_moderator_snapshot snapshot) in
           (match mode with
            | `Pending ->
              A.complete_initialization
                ~moderator_manager:manager
                actor
                ~scope
                ~candidate:{ expected with moderator = bare }
            | `Ready ->
              A.checkpoint_runtime_construction
                ~moderator_manager:manager
                actor
                ~scope
                bare)
           |> protocol_ok
           |> ignore;
           A.end_initialization actor ~scope |> protocol_ok;
           let handed_off = A.state actor |> protocol_ok in
           assert_same_session_snapshot handed_off (S.Memory_backend.state backend);
           let entered = ref false in
           assert (
             Result.is_error
               (L.invoke
                  ~actor
                  ~manager
                  ~selection:Queue_head
                  ~operation_id:None
                  ~inspect:(fun _ -> entered := true)
                  ()));
           assert (not !entered);
           assert_same_session_snapshot handed_off (A.state actor |> protocol_ok);
           let worker_called = ref false in
           let worker =
             S.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
               worker_called := true;
               Completed
                 { final_history = input.history
                 ; moderator_snapshot = handed_off.moderator
                 ; runtime_requests = []
                 })
           in
           L.install actor manager worker;
           L.install actor manager worker;
           let before_start = (A.state actor |> protocol_ok).counters.event_sequence in
           let start_live = Atomic.make true in
           A.start ~actor:(L.authentication start_live) actor ~attachment_id:writer.id
           |> protocol_ok
           |> ignore;
           let started = await_idle actor in
           let lifecycle =
             S.Memory_backend.events_after backend before_start
             |> protocol_ok
             |> List.filter_map ~f:(fun event ->
               match
                 P.Event.Durable.Payload.of_json
                   ~kind:event.P.Event.Durable.kind
                   event.payload
                 |> protocol_ok
               with
               | Session_state_changed change -> Some change
               | _ -> None)
           in
           (match lifecycle with
            | [ { desired_state = Running; observed_state = Starting }
              ; { desired_state = Running; observed_state = Idle }
              ] -> ()
            | changes ->
              raise_s
                [%sexp
                  "Start did not acknowledge its exact lifecycle checkpoints"
                , (changes : P.Event.Durable.Payload.lifecycle_change list)]);
           assert (Option.is_none started.active_operation);
           assert (Option.is_none started.failure);
           assert (not !worker_called);
           assert_same_session_snapshot started (S.Memory_backend.state backend);
           (* A later Start is an independent admission, not the constructor's
              original authority. Revoking it must not replace the child loans. *)
           Atomic.set start_live false;
           borrow Queue_head ();
           assert (List.is_empty (Q.entries (L.queue (A.state actor |> protocol_ok))));
           let claimed =
             A.claim_schedule actor ~schedule_id:timer.id ~generation:timer.generation
             |> protocol_ok
             |> Option.value_exn
           in
           let owner =
             Owner.create
               ~actor
               ~initial:(Some (Legacy_schedule.runtime manager worker))
               ~build:(fun () -> failwith "unexpected constructor delivery rebuild")
           in
           Exn.protect
             ~finally:(fun () -> Owner.close_and_wait owner)
             ~f:(fun () ->
               Owner.deliver_schedule owner claimed |> protocol_ok;
               borrow Queue_head ();
               let settled = A.state actor |> protocol_ok in
               assert_same_session_snapshot settled (S.Memory_backend.state backend);
               assert (List.is_empty (Q.entries (L.queue settled)));
               let timer =
                 List.find_exn settled.schedules ~f:(fun saved ->
                   P.Id.Schedule.equal saved.id timer.id)
               in
               (match timer.status with
                | Delivered -> ()
                | Scheduled | Delivering | Cancelled | Failed _ -> assert false);
               assert (Operator_authorization.is_current original)))));
  print_endline
    "Pending and Ready constructor receipts; exact queue and timer handoff; original \
     live inside child callbacks after parent end and intended resource installation";
  [%expect
    {| Pending and Ready constructor receipts; exact queue and timer handoff; original live inside child callbacks after parent end and intended resource installation |}]
;;

let%expect_test "constructor claim failures preserve their exact previous basis" =
  let module L = Legacy_emit in
  let module A = L.A in
  let module S = Agent_session in
  let module P = Agent_protocol in
  List.iter [ `Rejected_ack; `Expired; `Changed_basis ] ~f:(fun mode ->
    with_actor_workspace (fun env workspace_instance ->
      let manager =
        prepared_legacy_callback_managers
          env
          ~id:"constructor-claim-failure"
          ~source:
            {|type event = [ `Session_start | `Fresh ]
              let initial_state = 0
              let on_event = fun ctx state event -> match event with
                | `Session_start ->
                  Task.bind(Runtime.emit(`Fresh), fun ignored -> Task.pure(state + 1))
                | `Fresh -> Task.pure(state + 1)|}
          ()
      in
      let reject = ref false in
      let rejections = ref 0 in
      Job_fixtures.with_actor
        ~environment:(env, workspace_instance)
        ~start:false
        ~prepare_state:(fun state ->
          { state with runtime_initialization = Pending { fresh_history = true } })
        ~reject_save:(fun _ ->
          if !reject
          then (
            incr rejections;
            true)
          else false)
        (fun _env _sw actor _writer backend ->
           let live = Atomic.make true in
           let original = L.authentication live in
           let expected = A.state actor |> protocol_ok in
           let scope =
             A.begin_initialization_authenticated actor ~actor:original ~expected
             |> protocol_ok
           in
           let snapshot = L.M.identity_snapshot manager |> Result.ok_or_failwith in
           (match mode with
            | `Rejected_ack -> reject := true
            | `Expired -> Atomic.set live false
            | `Changed_basis ->
              (* A genuine independent host write is not this scope's own ACK. *)
              A.change_moderator
                ~actor:original
                actor
                (Some (L.B.encode_moderator_snapshot snapshot))
              |> protocol_ok
              |> ignore);
           let before = A.state actor |> protocol_ok in
           let entered = ref false in
           let claim_error = ref None in
           let claim ~callback run =
             let result =
               A.with_initialization_callback actor ~scope ~manager ~callback run
             in
             Result.iter_error result ~f:(fun failure -> claim_error := Some failure);
             result
           in
           assert (
             Result.is_error
               (L.invoke
                  ~claim
                  ~actor
                  ~manager
                  ~selection:(Boundary Session_start)
                  ~operation_id:None
                  ~inspect:(fun _ -> entered := true)
                  ()));
           let failure = Option.value_exn !claim_error in
           (match mode with
            | `Rejected_ack ->
              [%test_eq: P.Error.code] Internal_error failure.code;
              [%test_eq: string] "injected job transition failure" failure.message;
              [%test_eq: int] 1 !rejections
            | `Expired -> [%test_eq: P.Error.code] Permission_denied failure.code
            | `Changed_basis ->
              [%test_eq: P.Error.code] Conflict failure.code;
              [%test_eq: string]
                "initialization basis changed while resources were prepared"
                failure.message);
           assert (not !entered);
           assert_same_session_snapshot before (A.state actor |> protocol_ok);
           assert_same_session_snapshot before (S.Memory_backend.state backend);
           assert (
             Sexp.equal
               (Session.Moderator_state.Identity_snapshot.sexp_of_t snapshot)
               (Session.Moderator_state.Identity_snapshot.sexp_of_t
                  (L.M.identity_snapshot manager |> Result.ok_or_failwith)));
           assert (List.is_empty before.schedules);
           assert (List.is_empty before.moderator_executions);
           (match mode with
            | `Expired | `Changed_basis -> ()
            | `Rejected_ack ->
              reject := false;
              let observed = ref None in
              L.invoke
                ~claim:(A.with_initialization_callback actor ~scope ~manager)
                ~actor
                ~manager
                ~selection:(Boundary Session_start)
                ~operation_id:None
                ~inspect:(fun callback ->
                  let proof = A.callback_agent_actor actor ~scope:callback in
                  observed
                  := Some
                       ( proof
                       , match proof with
                         | Ok (Some proof) -> Operator_authorization.is_current proof
                         | Ok None | Error _ -> false ))
                ()
              |> Result.ok_or_failwith
              |> ignore;
              (match Option.value_exn !observed with
               | Ok (Some proof), true ->
                 assert (not (Operator_authorization.is_current proof))
               | Error failure, _ -> raise_s [%sexp (failure : P.Error.t)]
               | Ok _, _ -> failwith "rejected claim consumed the constructor admission");
              let saved = A.state actor |> protocol_ok in
              [%test_eq: int] 1 (List.length saved.moderator_executions);
              [%test_eq: int] 1 (List.length (S.Moderator_queue.entries (L.queue saved)));
              assert_same_session_snapshot saved (S.Memory_backend.state backend));
           A.end_initialization actor ~scope |> protocol_ok)));
  print_endline
    "rejected claim ACK is retryable; expired original and unrelated basis reject before \
     effects; exact Actor, Memory and Manager state retained";
  [%expect
    {| rejected claim ACK is retryable; expired original and unrelated basis reject before effects; exact Actor, Memory and Manager state retained |}]
;;

let%expect_test
    "failed constructor checkpoint preserves facts without adopting old custody"
  =
  let module L = Legacy_emit in
  let module A = L.A in
  let module S = Agent_session in
  let module P = Agent_protocol in
  let module Q = S.Moderator_queue in
  let module Owner = Agent_server.Runtime_owner in
  with_actor_workspace (fun env workspace_instance ->
    let make_manager () =
      prepared_legacy_callback_managers
        env
        ~id:"constructor-failed-checkpoint"
        ~source:
          {|type event = [ `Session_start | `Prefix | `Fresh | `Tick ]
            let initial_state = 0
            let on_event = fun ctx state event -> match event with
              | `Session_start ->
                Task.bind(Schedule.after_ms(0, `Tick), fun timer ->
                  Task.bind(Runtime.emit(`Fresh), fun ignored -> Task.pure(state + 1)))
              | `Prefix -> Task.pure(state + 1)
              | `Fresh -> Task.pure(state + 1)
              | `Tick -> Task.pure(state + 1)|}
        ()
    in
    let manager = make_manager () in
    (* This pre-existing value has no live producer admission. A later genuine
       constructor may assign structural ordinals, never its original proof. *)
    Session.Snapshot.to_value (Session.Snapshot.Variant ("Prefix", []))
    |> Result.ok_or_failwith
    |> L.M.enqueue_internal_event manager
    |> Result.ok_or_failwith;
    let previous = L.M.identity_snapshot manager |> Result.ok_or_failwith in
    let armed = ref false in
    let rejected = ref 0 in
    Job_fixtures.with_actor
      ~environment:(env, workspace_instance)
      ~start:false
      ~prepare_state:(fun state ->
        { state with runtime_initialization = Pending { fresh_history = true } })
      ~reject_save:(fun transition ->
        if
          !armed
          && List.exists
               transition.S.Session_transition.state.moderator_executions
               ~f:(fun receipt ->
                 match receipt.P.Moderator_execution.status with
                 | Completed _ -> true
                 | Running | Failed _ | Interrupted _ -> false)
        then (
          incr rejected;
          true)
        else false)
      (fun _env _sw actor writer backend ->
         let original = L.authentication (Atomic.make true) in
         A.change_moderator
           ~actor:original
           actor
           (Some (L.B.encode_moderator_snapshot previous))
         |> protocol_ok
         |> ignore;
         let expected = A.state actor |> protocol_ok in
         let scope =
           A.begin_initialization_authenticated actor ~actor:original ~expected
           |> protocol_ok
         in
         let observed = ref None in
         let checkpoint_error = ref None in
         let claim ~callback run =
           A.with_initialization_callback
             actor
             ~scope
             ~manager
             ~callback
             (fun ~scope ~commit ->
                run ~scope ~commit:(fun ~snapshot ~requests ->
                  let result = commit ~snapshot ~requests in
                  Result.iter_error result ~f:(fun failure ->
                    checkpoint_error := Some failure);
                  result))
         in
         armed := true;
         let result =
           L.invoke
             ~claim
             ~actor
             ~manager
             ~selection:(Boundary Session_start)
             ~operation_id:None
             ~inspect:(fun callback ->
               let proof = A.callback_agent_actor actor ~scope:callback in
               observed
               := Some
                    ( proof
                    , match proof with
                      | Ok (Some proof) -> Operator_authorization.is_current proof
                      | Ok None | Error _ -> false ))
             ()
         in
         armed := false;
         assert (Result.is_error result);
         let failure = Option.value_exn !checkpoint_error in
         [%test_eq: P.Error.code] Internal_error failure.code;
         [%test_eq: string] "injected job transition failure" failure.message;
         [%test_eq: int] 1 !rejected;
         (match Option.value_exn !observed with
          | Ok (Some proof), true ->
            assert (not (Operator_authorization.is_current proof))
          | Error failure, _ -> raise_s [%sexp (failure : P.Error.t)]
          | Ok _, _ -> failwith "constructor failed before the intended checkpoint cut");
         let failed = A.state actor |> protocol_ok in
         assert_same_session_snapshot failed (S.Memory_backend.state backend);
         let receipt = List.hd_exn failed.moderator_executions in
         (match receipt.status with
          | Failed failure ->
            [%test_eq: string] "event.handler_failed" failure.code;
            [%test_eq: string]
              "queued moderator handler exited without committing its checkpoint"
              failure.message
          | Running | Completed _ | Interrupted _ ->
            raise_s
              [%sexp
                "failed constructor did not settle", (receipt : P.Moderator_execution.t)]);
         let snapshot = L.M.identity_snapshot manager |> Result.ok_or_failwith in
         assert (
           Sexp.equal
             (Session.Moderator_state.Identity_snapshot.sexp_of_t previous)
             (Session.Moderator_state.Identity_snapshot.sexp_of_t snapshot));
         let prefix = List.hd_exn (Q.entries (L.queue failed)) in
         [%test_eq: int] 1 (List.length (Q.entries (L.queue failed)));
         assert (Option.is_none (Q.Entry.producer prefix));
         assert (Option.is_none (Q.Entry.origin prefix));
         let timer = List.hd_exn failed.schedules in
         [%test_eq: int] 1 (List.length failed.schedules);
         (match timer.status with
          | Scheduled -> ()
          | Delivering | Delivered | Cancelled | Failed _ -> assert false);
         assert (Option.is_some timer.inference_provenance);
         let parent_proof =
           A.initialization_agent_actor actor ~scope |> protocol_ok |> Option.value_exn
         in
         assert (Operator_authorization.is_current parent_proof);
         (match
            A.retire_initialization_resource
              actor
              ~scope
              ~moderator_manager:(make_manager ())
          with
          | Error failure ->
            [%test_eq: P.Error.code] Conflict failure.code;
            [%test_eq: string]
              "constructor cleanup names a different Manager"
              failure.message
          | Ok () -> failwith "foreign resource cleanup was accepted");
         assert_same_session_snapshot failed (A.state actor |> protocol_ok);
         A.retire_initialization_resource actor ~scope ~moderator_manager:manager
         |> protocol_ok;
         assert (Operator_authorization.is_current parent_proof);
         A.end_initialization actor ~scope |> protocol_ok;
         assert (not (Operator_authorization.is_current parent_proof));
         (* Negative cleanup is exact and remains idempotent after owner end;
            durable receipt, prefix and timer facts are not rolled back. *)
         A.retire_initialization_resource actor ~scope ~moderator_manager:manager
         |> protocol_ok;
         assert_same_session_snapshot failed (A.state actor |> protocol_ok);
         assert_same_session_snapshot failed (S.Memory_backend.state backend);
         let later_original = L.authentication (Atomic.make true) in
         let later_expected = A.state actor |> protocol_ok in
         let later =
           A.begin_initialization_authenticated
             actor
             ~actor:later_original
             ~expected:later_expected
           |> protocol_ok
         in
         A.complete_initialization
           ~moderator_manager:manager
           actor
           ~scope:later
           ~candidate:
             { later_expected with
               moderator = Some (L.B.encode_moderator_snapshot snapshot)
             }
         |> protocol_ok
         |> ignore;
         A.end_initialization actor ~scope:later |> protocol_ok;
         let handed_off = A.state actor |> protocol_ok in
         let worker =
           S.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
             Completed
               { final_history = input.history
               ; moderator_snapshot = handed_off.moderator
               ; runtime_requests = []
               })
         in
         L.install actor manager worker;
         A.start ~actor:later_original actor ~attachment_id:writer.id
         |> protocol_ok
         |> ignore;
         await_idle actor |> ignore;
         let consume_unavailable () =
           let observed = ref None in
           L.invoke
             ~actor
             ~manager
             ~selection:Queue_head
             ~operation_id:None
             ~inspect:(fun callback ->
               observed := Some (A.callback_agent_actor actor ~scope:callback))
             ()
           |> Result.ok_or_failwith
           |> ignore;
           match Option.value_exn !observed with
           | Error failure -> [%test_eq: P.Error.code] Permission_denied failure.code
           | Ok _ ->
             failwith "later constructor resurrected historical or retired custody"
         in
         consume_unavailable ();
         assert (List.is_empty (Q.entries (L.queue (A.state actor |> protocol_ok))));
         let claimed =
           A.claim_schedule actor ~schedule_id:timer.id ~generation:timer.generation
           |> protocol_ok
           |> Option.value_exn
         in
         let owner =
           Owner.create
             ~actor
             ~initial:(Some (Legacy_schedule.runtime manager worker))
             ~build:(fun () -> failwith "unexpected failed-constructor delivery rebuild")
         in
         Exn.protect
           ~finally:(fun () -> Owner.close_and_wait owner)
           ~f:(fun () ->
             Owner.deliver_schedule owner claimed |> protocol_ok;
             let delivered = A.state actor |> protocol_ok in
             let entry = List.hd_exn (Q.entries (L.queue delivered)) in
             (match Q.Entry.producer entry with
              | Some (Schedule_delivery actual) ->
                assert (P.Id.Schedule.equal actual.id timer.id)
              | None | Some (Event _ | Invocation _ | Job_completion _ | Source_setup _)
                -> assert false);
             consume_unavailable ();
             let settled = A.state actor |> protocol_ok in
             assert_same_session_snapshot settled (S.Memory_backend.state backend);
             assert (List.is_empty (Q.entries (L.queue settled)));
             assert (Operator_authorization.is_current original);
             assert (Operator_authorization.is_current later_original))));
  print_endline
    "post-effect checkpoint rejected; truthful Failed receipt and timer survive; \
     restored prefix and exact retired resource stay unavailable after later \
     construction";
  [%expect
    {| post-effect checkpoint rejected; truthful Failed receipt and timer survive; restored prefix and exact retired resource stay unavailable after later construction |}]
;;
