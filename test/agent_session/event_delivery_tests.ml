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
        }
      in
      let initial =
        { initial with
          moderator = Some (B.encode_moderator_snapshot (snapshot ()))
        ; schedules = [ first; second; third ]
        ; jobs = [ job ]
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
            { commit =
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
                    | Pending | Not_required -> false)
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
                  ; state_committed = (fun _ _ -> ())
                  }
            in
            actor, snapshot)
        in
        Exn.protect
          ~finally:(fun () -> Array.iter actors ~f:(fun (actor, _) -> A.shutdown actor))
          ~f:(fun () ->
            let held = Array.init 2 ~f:(fun _ -> Eio.Promise.create ()) in
            let proceed = Array.init 2 ~f:(fun _ -> Eio.Promise.create ()) in
            let finished = Array.init 2 ~f:(fun _ -> Eio.Promise.create ()) in
            Array.iteri actors ~f:(fun index (actor, snapshot) ->
              Eio.Fiber.fork ~sw (fun () ->
                let result = ref "missing" in
                let claimed =
                  A.with_current_moderator_event
                    actor
                    ~operation_id:None
                    ~event:Session_start
                    ~snapshot:(fun () -> Ok snapshot)
                    (fun ~executing:_ ~event:_ ~execute:_ ~commit ->
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
              ~prepare_event:(fun ~outcome:_ ~snapshot:_ -> Ok ignore)
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
                      commit ~snapshot ~requests
                      |> Result.map_error ~f:(fun e -> e.Agent_protocol.Error.message)
                      |> Result.map ~f:(fun () ->
                        (* The durable checkpoint is installed; the live manager has
                         not installed it yet. Ownership must cover this gap. *)
                        assert (Result.is_error (A.change_moderator actor None));
                        assert (
                          Option.is_none (A.claim_idle_moderator actor |> protocol_ok));
                        fun () -> ()))
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
              let restored =
                Agent_session.Session_persistence.restore_snapshot
                  (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t saved))
                |> store_ok
              in
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
          ~prepare_event:(fun ~outcome:_ ~snapshot:_ -> Ok ignore)
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
               commit
                 ~snapshot
                 ~requests:
                   { request_turn = false
                   ; request_compaction = false
                   ; end_session = None
                   }
               |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
               |> Result.map ~f:(fun () -> ignore))
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
       let migrated =
         Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t legacy))
         |> store_ok
       in
       assert (List.equal E.equal migrated.moderator_executions [ failed ]);
       assert (
         Result.is_error
           (Agent_session.Session_state.upgrade_schema
              { retired_state with schema_version = 5 }));
       let restored =
         Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t retired_state))
         |> store_ok
       in
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
            ~prepare_event:(fun ~outcome:_ ~snapshot:_ -> Ok ignore)
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
             (fun ~executing ~event:_ ~execute ~commit ->
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
                    (execute ~invocation:bad (fun ~dispatched:_ -> assert false)));
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
                    commit
                      ~snapshot
                      ~requests:
                        { request_turn = false
                        ; request_compaction = false
                        ; end_session = None
                        }
                    |> Result.map_error ~f:(fun error ->
                      error.Agent_protocol.Error.message)
                    |> Result.map ~f:(fun () -> ignore))
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
             (execute ~invocation:(child ()) (fun ~dispatched:_ -> assert false)));
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
         let restored =
           Agent_session.Session_persistence.restore_snapshot
             (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
           |> store_ok
         in
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
            ~prepare_event:(fun ~outcome:_ ~snapshot:_ -> Ok ignore)
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
               (fun ~executing ~event ~execute ~commit ->
                  let event =
                    match mode with
                    | `Mismatched_head -> Session.Snapshot.String "wrong head"
                    | _ -> event
                  in
                  f ~executing ~event ~execute ~commit)
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
