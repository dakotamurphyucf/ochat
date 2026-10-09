open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol

let with_startup_actor
      ?events
      ?make_worker
      ?make_job_results
      ?make_persistence
      ?(after_close = fun _ _ _ -> ())
      ?(prepare = ignore)
      f
  =
  with_actor_workspace (fun env workspace_instance ->
    let result =
      Eio.Switch.run (fun sw ->
        prepare env;
        let manager, _, _ = handoff_definition ?events env in
        let snapshot =
          Chat_response.Moderator_manager.identity_snapshot manager
          |> Result.ok_or_failwith
        in
        let initial =
          actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
        in
        let initial =
          { initial with moderator = Some (A.Moderator_checkpoint.encode snapshot) }
        in
        let observer =
          A.Moderator_checkpoint.observer initial.moderator
          |> protocol_ok
          |> Option.value_exn
        in
        let reject_commit = ref false in
        let job_results =
          Option.map make_job_results ~f:(fun make -> make env sw initial)
        in
        let persistence =
          match make_persistence with
          | Some make -> make env sw initial
          | None ->
            { A.Session_actor.archive_reference
            ; commit =
                (fun ~command_audit:_ ~previous:_ _ ->
                  if !reject_commit
                  then
                    Error
                      (P.Error.create
                         Persistence_error
                         ~message:"injected run persistence failure"
                         ~retryable:true
                         ())
                  else Ok ())
            }
        in
        let actor =
          A.Session_actor.create
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~mailbox_capacity:32
            ~compaction_env:None
            ~initial_state:initial
            ~persistence
            ~operation_worker:(Option.map make_worker ~f:(fun make -> make manager))
            ~services:
              { now = (fun () -> timestamp)
              ; monotonic_now = (fun () -> Mtime.min_stamp)
              ; create_attachment_id =
                  (fun () -> P.Id.Attachment.of_string "att_run_start" |> protocol_ok)
              ; create_reclaim_token = (fun () -> "run-start-reclaim")
              ; state_committed = (fun _ _ -> ())
              ; job_results
              ; subscription_limits = A.Staged_subscriptions.default_limits
              ; schedule_limits = A.Staged_schedules.default_limits
              ; notification_limits = A.Staged_notifications.default_limits
              ; ingress_limits = A.Staged_ingress.default_limits
              }
        in
        let attachment, _ =
          A.Session_actor.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
        in
        let authorized = ref true in
        let scope =
          A.Run_admission.Scope.create
            ~principal_id
            ~observer
            ~startup_pending:(fun () -> true)
            ~authorize:(fun _ ->
              if !authorized
              then Ok ()
              else
                Error
                  (P.Error.create
                     Permission_denied
                     ~message:"run policy revoked"
                     ~retryable:false
                     ()))
          |> protocol_ok
        in
        let reference =
          P.Session_ref.create
            ~server_id:(P.Id.Server.of_string "srv_run_start" |> protocol_ok)
            ~session_id
        in
        let request key =
          let state = A.Session_actor.state actor |> protocol_ok in
          P.Run_start.create
            ~session_id
            ~attachment_id:attachment.id
            ~generation:state.identity.generation
            ~expected_revision:state.counters.revision
            ~mode:Workflow
            ~input:Authored_start
            ~key:(P.Idempotency_key.of_string key |> protocol_ok)
          |> protocol_ok
        in
        let start request =
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
            ~entry:None
        in
        Exn.protect
          ~finally:(fun () -> A.Session_actor.shutdown actor)
          ~f:(fun () ->
            f
              actor
              authorized
              reject_commit
              request
              start
              manager
              scope
              reference
              attachment))
    in
    after_close env workspace_instance result)
;;

let%expect_test "actual startup admission retries without initialization or mutation" =
  with_startup_actor (fun actor authorized _ request start _ _ _ _ ->
    let request = request "startup-once" in
    let receipt = start request |> protocol_ok in
    let accepted = A.Session_actor.state actor |> protocol_ok in
    let replay = start request |> protocol_ok in
    let replayed = A.Session_actor.state actor |> protocol_ok in
    authorized := false;
    let revoked_retry = Result.is_error (start request) in
    authorized := true;
    let rejected_initialized =
      let fresh =
        P.Run_start.create
          ~session_id
          ~attachment_id:request.attachment_id
          ~generation:replayed.identity.generation
          ~expected_revision:replayed.counters.revision
          ~mode:Workflow
          ~input:Authored_start
          ~key:(P.Idempotency_key.of_string "startup-again" |> protocol_ok)
        |> protocol_ok
      in
      Result.is_error (start fresh)
    in
    let final = A.Session_actor.state actor |> protocol_ok in
    print_s
      [%sexp
        { exact_retry = (P.Run_receipt.equal receipt replay : bool)
        ; replay_no_write =
            (Int64.equal accepted.counters.revision replayed.counters.revision : bool)
        ; revoked_retry : bool
        ; rejected_initialized : bool
        ; no_late_write =
            (Int64.equal final.counters.revision replayed.counters.revision : bool)
        ; retained_runs =
            (Option.value_map final.run_state ~default:0 ~f:(fun index ->
               List.length (A.Run_state.runs index))
             : int)
        }]);
  [%expect
    {|
    ((exact_retry true) (replay_no_write true) (revoked_retry true)
     (rejected_initialized true) (no_late_write true) (retained_runs 1))
    |}]
;;

let%expect_test "failed admission checkpoint leaves no run and same request can retry" =
  with_startup_actor (fun actor _ reject_commit request start _ _ _ _ ->
    let request = request "startup-persist" in
    reject_commit := true;
    let rejected = Result.is_error (start request) in
    let failed = A.Session_actor.state actor |> protocol_ok in
    reject_commit := false;
    ignore (start request |> protocol_ok : P.Run_receipt.t);
    let final = A.Session_actor.state actor |> protocol_ok in
    print_s
      [%sexp
        { rejected : bool
        ; no_uncommitted_run = (Option.is_none failed.run_state : bool)
        ; admitted_on_retry = (Option.is_some final.run_state : bool)
        }]);
  [%expect {| ((rejected true) (no_uncommitted_run true) (admitted_on_retry true)) |}]
;;

let%expect_test "host authority releases terminal and obsolete run closures" =
  with_startup_actor (fun actor _ _ request start _ _ _ _ ->
    ignore (start (request "authority-cleanup") |> protocol_ok : P.Run_receipt.t);
    let state = A.Session_actor.state actor |> protocol_ok in
    let index = Option.value_exn state.run_state in
    let run = List.hd_exn (A.Run_state.runs index) in
    let scope =
      A.Run_admission.Scope.create
        ~principal_id:run.principal_id
        ~observer:run.source.observer
        ~startup_pending:(fun () -> false)
        ~authorize:(fun _ -> Ok ())
      |> protocol_ok
    in
    let authorities =
      A.Run_authorities.add
        A.Run_authorities.empty
        ~run
        ~scope
        ~attachment_id:(P.Id.Attachment.of_string "att_run_start" |> protocol_ok)
      |> protocol_ok
    in
    let live =
      A.Run_authorities.retain_current
        authorities
        ~index:(Some index)
        ~generation:state.identity.generation
    in
    let terminal =
      P.Run.create
        ~id:run.id
        ~session:run.session
        ~principal_id:run.principal_id
        ~source:run.source
        ~mode:run.mode
        ~lifecycle:(Terminal Interrupted)
        ~revision:1L
        ~owned_work:run.owned_work
        ~relinquished_work:run.relinquished_work
        ~terminal_work:run.terminal_work
        ~created_at:run.created_at
        ~updated_at:timestamp
      |> protocol_ok
    in
    let receipt =
      P.Run_receipt.create
        ~run_id:run.id
        ~principal_id:run.principal_id
        ~source:run.source
        ~key:(P.Idempotency_key.of_string "authority-terminal" |> protocol_ok)
        ~request_sha256:(String.make 64 'b')
        ~kind:Terminal
        ~run_revision:1L
        ~session_revision:state.counters.revision
        ~committed_at:timestamp
      |> protocol_ok
    in
    let terminal_index =
      A.Run_state.commit index ~run:terminal ~receipt ~intent:None |> protocol_ok
    in
    let retired =
      A.Run_authorities.retain_current
        authorities
        ~index:(Some terminal_index)
        ~generation:state.identity.generation
    in
    let obsolete =
      A.Run_authorities.retain_current
        authorities
        ~index:(Some index)
        ~generation:(state.identity.generation + 1)
    in
    let absent = A.Run_authorities.retain_current authorities ~index:None ~generation:0 in
    print_s
      [%sexp
        { live_retained = (Option.is_some (A.Run_authorities.find live run.id) : bool)
        ; terminal_released =
            (Option.is_none (A.Run_authorities.find retired run.id) : bool)
        ; obsolete_released =
            (Option.is_none (A.Run_authorities.find obsolete run.id) : bool)
        ; absent_released = (Option.is_none (A.Run_authorities.find absent run.id) : bool)
        }]);
  [%expect
    {|
    ((live_retained true) (terminal_released true) (obsolete_released true)
     (absent_released true))
    |}]
;;

let%expect_test
    "actual compiled startup finish commits action and terminal without stopping session"
  =
  let events =
    {| | `Session_start ->
    Task.bind(Run.finish(`Object([
      { key = "kind"; value = `String("finish") },
      { key = "terminal"; value = `Object([{ key = "kind"; value = `String("completed") }]) },
      { key = "relinquish"; value = `Array([]) }])), fun ignored -> Task.pure(state))
    | _ -> Task.pure(state) |}
  in
  with_startup_actor ~events (fun actor _ _ request start manager _ _ _ ->
    ignore (start (request "compiled-startup-finish") |> protocol_ok : P.Run_receipt.t);
    let run () =
      A.Moderator_event.run_ordinary
        ~event:Session_start
        ~claim:
          (A.Session_actor.with_current_moderator_event
             actor
             ~operation_id:None
             ~event:Session_start)
        ~run_actions:(A.Session_actor.run_actions actor)
        ~manager
        ~history:(fun () -> [])
        ~available_tools:[]
        ~session_meta:`Null
        ~now:(fun () -> timestamp)
        ()
    in
    let executed = Option.is_some (run () |> protocol_ok) in
    let state = A.Session_actor.state actor |> protocol_ok in
    let index = Option.value_exn state.run_state in
    let row = List.hd_exn (A.Run_state.runs index) in
    let terminal =
      match row.lifecycle with
      | Terminal (Completed _) -> true
      | _ -> false
    in
    let action_receipts =
      List.count (A.Run_state.receipts index) ~f:(fun receipt ->
        P.Run_receipt.Kind.equal receipt.kind Action)
    in
    let terminal_receipts =
      List.count (A.Run_state.receipts index) ~f:(fun receipt ->
        P.Run_receipt.Kind.equal receipt.kind Terminal)
    in
    print_s
      [%sexp
        { executed : bool
        ; terminal : bool
        ; action_receipts : int
        ; terminal_receipts : int
        ; session_running =
            ((match state.lifecycle.desired with
              | Running -> true
              | Stopped -> false)
             : bool)
        ; root_not_fabricated = (Option.is_none state.active_operation : bool)
        }]);
  [%expect
    {|
    ((executed true) (terminal true) (action_receipts 1) (terminal_receipts 1)
     (session_running true) (root_not_fabricated true))
    |}]
;;

let%expect_test
    "actual root completion joins pending authored finish without fabricating success"
  =
  List.iter [ `Success; `Failure; `Cancelled ] ~f:(fun mode ->
    let callback_done, signal_callback = Eio.Promise.create () in
    let release, release_worker = Eio.Promise.create () in
    let pending_seen = ref false in
    let events =
      {| | `Turn_end ->
      Task.bind(Run.finish(`Object([
        { key = "kind"; value = `String("finish") },
        { key = "terminal"; value = `Object([{ key = "kind"; value = `String("completed") }]) },
        { key = "relinquish"; value = `Array([]) }])), fun ignored -> Task.pure(state))
      | _ -> Task.pure(state) |}
    in
    let make_worker manager =
      A.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let result =
          A.Moderator_event.run_ordinary
            ~event:Turn_end
            ~claim:(caps.with_moderator_event ~event:Turn_end)
            ~run_actions:caps.run_actions
            ~manager
            ~history:(fun () -> input.history)
            ~available_tools:[]
            ~session_meta:`Null
            ~now:(fun () -> timestamp)
            ()
        in
        Eio.Promise.resolve signal_callback result;
        Eio.Promise.await release;
        match mode with
        | `Success ->
          A.Operation_worker.Completed
            { final_history = input.history
            ; runtime_requests = []
            ; moderator_snapshot =
                Some
                  (A.Moderator_checkpoint.encode
                     (Chat_response.Moderator_manager.identity_snapshot manager
                      |> Result.ok_or_failwith))
            }
        | `Failure ->
          Failed
            (P.Error.create
               Internal_error
               ~message:"actual root failure"
               ~retryable:false
               ())
        | `Cancelled -> Cancelled { reason = "actual root cancellation" })
    in
    with_startup_actor
      ~events
      ~make_worker
      (fun actor _ _ _ _ _ scope reference attachment ->
         ignore
           (A.Session_actor.start actor ~attachment_id:attachment.id |> protocol_ok
            : P.Session.t);
         let before = A.Session_actor.state actor |> protocol_ok in
         let request =
           P.Run_start.create
             ~session_id
             ~attachment_id:attachment.id
             ~generation:before.identity.generation
             ~expected_revision:before.counters.revision
             ~mode:Workflow
             ~input:
               (User_submission { kind = Plain_text; text = "hello"; attachments = [] })
             ~key:(P.Idempotency_key.of_string "root-join" |> protocol_ok)
           |> protocol_ok
         in
         ignore
           (A.Session_actor.admit_run
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
            : P.Run_receipt.t);
         ignore
           (Eio.Promise.await callback_done |> protocol_ok
            : Chat_response.Moderation.Outcome.t option);
         let pending = A.Session_actor.state actor |> protocol_ok in
         let pending_index = Option.value_exn pending.run_state in
         let pending_run = List.hd_exn (A.Run_state.runs pending_index) in
         pending_seen
         := (match pending_run.lifecycle with
             | Active -> true
             | Admitted | Waiting _ | Terminal _ -> false)
            && List.exists (A.Run_state.intents pending_index) ~f:(fun intent ->
              match intent.disposition, intent.action with
              | Pending, Finish _ -> true
              | _ -> false)
            && Option.is_some pending.active_operation
            && not
                 (List.exists (A.Run_state.receipts pending_index) ~f:(fun receipt ->
                    P.Run_receipt.Kind.equal receipt.kind Terminal));
         Eio.Promise.resolve release_worker ();
         let rec settled () =
           let state = A.Session_actor.state actor |> protocol_ok in
           if Option.is_none state.active_operation
           then state
           else (
             Eio.Fiber.yield ();
             settled ())
         in
         let final = settled () in
         let index = Option.value_exn final.run_state in
         let run = List.hd_exn (A.Run_state.runs index) in
         let truthful =
           match mode, run.lifecycle with
           | `Success, Terminal (Completed _)
           | `Failure, Terminal (Failed _)
           | `Cancelled, Terminal Cancelled -> true
           | _ -> false
         in
         print_s
           [%sexp
             { pending_before_root = (!pending_seen : bool)
             ; truthful_terminal = (truthful : bool)
             ; terminal_receipts =
                 (List.count (A.Run_state.receipts index) ~f:(fun receipt ->
                    P.Run_receipt.Kind.equal receipt.kind Terminal)
                  : int)
             ; immutable_root_proofs = (List.length run.terminal_work : int)
             }]));
  [%expect
    {|
    ((pending_before_root true) (truthful_terminal true) (terminal_receipts 1)
     (immutable_root_proofs 1))
    ((pending_before_root true) (truthful_terminal true) (terminal_receipts 1)
     (immutable_root_proofs 1))
    ((pending_before_root true) (truthful_terminal true) (terminal_receipts 1)
     (immutable_root_proofs 1))
    |}]
;;

let%expect_test
    "actual staged callback jobs enter custody without action and can be waited \
     atomically"
  =
  List.iter [ false; true ] ~f:(fun wait ->
    with_startup_actor (fun actor _ _ request start manager _ _ _ ->
      ignore (start (request "stage-owned-job") |> protocol_ok : P.Run_receipt.t);
      let registry = native_registry (ref 0) ~raises:false in
      let request =
        Background_execution_tests.capture_tool
          registry
          { Chat_response.One_off_request.default_policy with max_output_bytes = 512 }
      in
      (* The selected native fixture returns a small fixed string. Its saved
         output contract, rather than a later default, bounds inline delivery. *)
      let staged = ref None in
      let before =
        Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
      in
      let ran =
        A.Session_actor.with_current_moderator_event
          actor
          ~operation_id:None
          ~event:Session_start
          ~snapshot:(fun () -> Ok before)
          (fun ~executing ~retirement_reason:_ ~event:_ ~execute:_ ~commit ->
             let service =
               A.Session_actor.run_actions actor executing
               |> protocol_ok
               |> Option.value_exn
             in
             let owner = P.Job.Moderator_event executing.context.id in
             let job =
               A.Session_actor.prepare_background_job_launch actor ~owner request
               |> protocol_ok
             in
             staged := Some job;
             A.Session_actor.stage_background_job
               actor
               ~job
               ~capacity:{ publish = ignore; abort = ignore }
             |> protocol_ok;
             A.Session_actor.select_background_jobs actor ~owner ~ids:[ job.id ]
             |> protocol_ok;
             let transaction = A.Run_action_service.transaction service in
             let tickets =
               if wait
               then (
                 let state = A.Session_actor.state actor |> protocol_ok in
                 let run =
                   state.run_state |> Option.value_exn |> A.Run_state.runs |> List.hd_exn
                 in
                 let wake =
                   P.Run_wake.create
                     ~run_id:run.id
                     ~source:run.source
                     ~occurrence:
                       (Job_completion { job_id = job.id; attempt = Int.succ job.attempt })
                   |> protocol_ok
                 in
                 [ transaction.handlers.stage (Wait wake) |> Result.ok_or_failwith ])
               else []
             in
             ignore
               (transaction.prepare tickets |> Result.ok_or_failwith
                : P.Run_action.t option);
             commit
               ~snapshot:before
               ~requests:
                 { request_turn = false; request_compaction = false; end_session = None })
        |> protocol_ok
      in
      let state = A.Session_actor.state actor |> protocol_ok in
      let index = Option.value_exn state.run_state in
      let run = List.hd_exn (A.Run_state.runs index) in
      let job = Option.value_exn !staged in
      let owns =
        List.exists run.owned_work ~f:(fun work ->
          match work.key with
          | Retained (Job { id; attempt }) ->
            P.Id.Job.equal id job.id && Int.equal attempt (Int.succ job.attempt)
          | _ -> false)
      in
      let expected_lifecycle =
        match wait, run.lifecycle with
        | false, Active | true, Waiting _ -> true
        | _ -> false
      in
      print_s
        [%sexp
          { actual_callback = (ran : bool)
          ; retained_owned_job = (owns : bool)
          ; staged_job_durable =
              (List.exists state.jobs ~f:(fun retained ->
                 P.Id.Job.equal retained.id job.id)
               : bool)
          ; expected_lifecycle : bool
          }]));
  [%expect
    {|
    ((actual_callback true) (retained_owned_job true) (staged_job_durable true)
     (expected_lifecycle true))
    ((actual_callback true) (retained_owned_job true) (staged_job_durable true)
     (expected_lifecycle true))
    |}]
;;

let%expect_test
    "host-owned workflow finishes after transport detach and reconnect observes receipt"
  =
  let events =
    {| | `Session_start -> Task.bind(Run.finish(`Object([
    { key = "kind"; value = `String("finish") },
    { key = "terminal"; value = `Object([{ key = "kind"; value = `String("completed") }]) },
    { key = "relinquish"; value = `Array([]) }])), fun ignored -> Task.pure(state))
    | _ -> Task.pure(state) |}
  in
  with_startup_actor ~events (fun actor _ _ request start manager _ _ attachment ->
    ignore (start (request "detached-host-run") |> protocol_ok : P.Run_receipt.t);
    A.Session_actor.detach actor attachment.id |> protocol_ok;
    let detached = A.Session_actor.state actor |> protocol_ok in
    let executed =
      A.Moderator_event.run_ordinary
        ~event:Session_start
        ~claim:
          (A.Session_actor.with_current_moderator_event
             actor
             ~operation_id:None
             ~event:Session_start)
        ~run_actions:(A.Session_actor.run_actions actor)
        ~manager
        ~history:(fun () -> [])
        ~available_tools:[]
        ~session_meta:`Null
        ~now:(fun () -> timestamp)
        ()
      |> protocol_ok
    in
    let _attachment, _token =
      A.Session_actor.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
    in
    let observed = A.Session_actor.state actor |> protocol_ok in
    let index = Option.value_exn observed.run_state in
    let terminal =
      List.exists (A.Run_state.runs index) ~f:(fun run ->
        match run.lifecycle with
        | Terminal (Completed _) -> true
        | _ -> false)
    in
    print_s
      [%sexp
        { no_transport_writer = (List.is_empty detached.attachments : bool)
        ; continued_host_execution = (Option.is_some executed : bool)
        ; terminal : bool
        ; terminal_receipts =
            (List.count (A.Run_state.receipts index) ~f:(fun receipt ->
               P.Run_receipt.Kind.equal receipt.kind Terminal)
             : int)
        }]);
  [%expect
    {|
    ((no_transport_writer true) (continued_host_execution true) (terminal true)
     (terminal_receipts 1))
    |}]
;;

let%expect_test
    "actual same-generation A to B to A replacement retires custody and restore keeps \
     epoch"
  =
  let captured_b = ref None in
  let prepare env =
    let manager, _, _ =
      handoff_definition
        env
        ~declare_tool:false
        ~events:
          "| `Session_start -> Task.bind(Runtime.emit(`String(\"source-b\")), fun \
           ignored -> Task.pure(state)) | _ -> Task.pure(state)"
    in
    captured_b
    := Some
         (Chat_response.Moderator_manager.identity_snapshot manager
          |> Result.ok_or_failwith)
  in
  with_startup_actor ~prepare (fun actor _ _ request start manager _ _ _ ->
    ignore (start (request "replace-source") |> protocol_ok : P.Run_receipt.t);
    let original = A.Session_actor.state actor |> protocol_ok in
    let original_index = Option.value_exn original.run_state in
    let initial_epoch = (A.Run_state.installation original_index).epoch in
    let snapshot_a =
      Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
    in
    let snapshot_b = Option.value_exn !captured_b in
    let install snapshot =
      let current = A.Session_actor.state actor |> protocol_ok in
      A.Session_actor.commit_extensions
        actor
        ~generation:current.identity.generation
        ~expected_revision:current.counters.revision
        [ A.Session_actor.Extension_change.Moderator_state
            (Some (A.Moderator_checkpoint.encode snapshot))
        ]
      |> protocol_ok
    in
    ignore (install snapshot_b);
    let b = A.Session_actor.state actor |> protocol_ok in
    ignore (install snapshot_a);
    let a = A.Session_actor.state actor |> protocol_ok in
    ignore (install snapshot_a);
    let restored = A.Session_actor.state actor |> protocol_ok in
    let index state = Option.value_exn state.A.Session_state.run_state in
    let epoch state = (A.Run_state.installation (index state)).epoch in
    let retired = List.hd_exn (A.Run_state.runs (index a)) in
    print_s
      [%sexp
        { same_generation =
            (Int.equal original.identity.generation a.identity.generation : bool)
        ; first_replaced = (Int64.equal (epoch b) (Int64.succ initial_epoch) : bool)
        ; second_replaced = (Int64.equal (epoch a) Int64.(initial_epoch + 2L) : bool)
        ; matching_restore_continuity = (Int64.equal (epoch restored) (epoch a) : bool)
        ; old_run_interrupted =
            ((match retired.lifecycle with
              | Terminal Interrupted -> true
              | _ -> false)
             : bool)
        ; terminal_receipts =
            (List.count
               (A.Run_state.receipts (index a))
               ~f:(fun receipt -> P.Run_receipt.Kind.equal receipt.kind Terminal)
             : int)
        }]);
  [%expect
    {|
    ((same_generation true) (first_replaced true) (second_replaced true)
     (matching_restore_continuity true) (old_run_interrupted true)
     (terminal_receipts 1))
    |}]
;;

let%expect_test
    "actual restoring-host reconciliation interrupts once without rotating source"
  =
  with_startup_actor (fun actor _ _ request start _ _ _ _ ->
    ignore (start (request "restore-run") |> protocol_ok : P.Run_receipt.t);
    let before = A.Session_actor.state actor |> protocol_ok in
    let before_index = Option.value_exn before.run_state in
    A.Session_actor.reconcile_run_recovery actor |> protocol_ok;
    let once = A.Session_actor.state actor |> protocol_ok in
    A.Session_actor.reconcile_run_recovery actor |> protocol_ok;
    let twice = A.Session_actor.state actor |> protocol_ok in
    let index = Option.value_exn twice.run_state in
    let run = List.hd_exn (A.Run_state.runs index) in
    print_s
      [%sexp
        { interrupted =
            ((match run.lifecycle with
              | Terminal Interrupted -> true
              | _ -> false)
             : bool)
        ; epoch_preserved =
            (Int64.equal
               (A.Run_state.installation before_index).epoch
               (A.Run_state.installation index).epoch
             : bool)
        ; duplicate_no_write =
            (Int64.equal once.counters.revision twice.counters.revision : bool)
        ; terminal_receipts =
            (List.count (A.Run_state.receipts index) ~f:(fun receipt ->
               P.Run_receipt.Kind.equal receipt.kind Terminal)
             : int)
        }]);
  [%expect
    {|
    ((interrupted true) (epoch_preserved true) (duplicate_no_write true)
     (terminal_receipts 1))
    |}]
;;

let%expect_test "unresolved Continue cannot be replaced by Wait or Finish" =
  with_startup_actor (fun actor _ _ request start _ _ _ _ ->
    ignore (start (request "pending-action-policy") |> protocol_ok : P.Run_receipt.t);
    let state = A.Session_actor.state actor |> protocol_ok in
    let index = Option.value_exn state.run_state in
    let previous = List.hd_exn (A.Run_state.runs index) in
    let run =
      P.Run.create
        ~id:previous.id
        ~session:previous.session
        ~principal_id:previous.principal_id
        ~source:previous.source
        ~mode:previous.mode
        ~lifecycle:Active
        ~revision:1L
        ~owned_work:[]
        ~relinquished_work:[]
        ~terminal_work:[]
        ~created_at:previous.created_at
        ~updated_at:timestamp
      |> protocol_ok
    in
    let receipt =
      P.Run_receipt.create
        ~run_id:run.id
        ~principal_id:run.principal_id
        ~source:run.source
        ~key:(P.Idempotency_key.of_string "continue-pending" |> protocol_ok)
        ~request_sha256:(String.make 64 'c')
        ~kind:Action
        ~run_revision:run.revision
        ~session_revision:(Int64.succ state.counters.revision)
        ~committed_at:timestamp
      |> protocol_ok
    in
    let intent =
      A.Run_intent.create
        ~receipt
        ~execution_id:
          (P.Id.Moderator_execution.of_string "mex_pending_continue" |> protocol_ok)
        ~action:Continue
      |> protocol_ok
    in
    let index =
      A.Run_state.commit index ~run ~receipt ~intent:(Some intent) |> protocol_ok
    in
    let finish = P.Run_action.Finish { terminal = Completed None; relinquish = [] } in
    print_s
      [%sexp
        { continue_coalesces =
            (Result.is_ok (A.Run_state.check_action index ~run_id:run.id ~action:Continue)
             : bool)
        ; unresolved_finish_rejected =
            (Result.is_error
               (A.Run_state.check_action index ~run_id:run.id ~action:finish)
             : bool)
        ; pending_receipt_unchanged =
            (P.Run_receipt.equal (List.hd_exn (A.Run_state.intents index)).receipt receipt
             : bool)
        }]);
  [%expect
    {|
    ((continue_coalesces true) (unresolved_finish_rejected true)
     (pending_receipt_unchanged true))
    |}]
;;

let%expect_test "preparation preserves original CAS through owned history reservation" =
  with_startup_actor (fun actor _ _ request _ _ scope reference _ ->
    let original = request "prepared-history" in
    let digest = String.make 64 'a' in
    let begin_ request =
      A.Session_actor.begin_run_preparation
        actor
        ~authorize:(fun _ -> Ok ())
        ~principal_id
        ~request
        ~request_sha256:digest
    in
    let preparation =
      match begin_ original |> protocol_ok with
      | A.Run_preparation.Decision.Prepare preparation -> preparation
      | Retained _ -> failwith "fresh fixture unexpectedly retained an admission"
    in
    let competing_rejected = Result.is_error (begin_ original) in
    ignore
      (A.Session_actor.reserve_run_history_block actor ~preparation ~count:2
       |> protocol_ok
       : A.History_id_source.reservation);
    let reserved = A.Session_actor.state actor |> protocol_ok in
    let outcome =
      A.Session_actor.admit_prepared_run
        actor
        ~command_audit:None
        ~preparation
        ~scope
        ~session:reference
        ~entry:None
      |> protocol_ok
    in
    let admitted_original =
      match outcome with
      | A.Run_admission_outcome.Admitted receipt ->
        P.Idempotency_key.equal receipt.key original.key
        && String.equal receipt.request_sha256 digest
      | Rejected _ | Uncertain _ -> false
    in
    let before_retry = A.Session_actor.state actor |> protocol_ok in
    let retained =
      match begin_ original |> protocol_ok with
      | A.Run_preparation.Decision.Retained _ -> true
      | Prepare _ -> false
    in
    let after_retry = A.Session_actor.state actor |> protocol_ok in
    let owned_revision_advanced =
      Int64.(reserved.counters.revision > original.expected_revision)
    in
    let replay_did_not_mutate =
      Int64.equal before_retry.counters.revision after_retry.counters.revision
    in
    print_s
      [%sexp
        { competing_rejected : bool
        ; owned_revision_advanced : bool
        ; admitted_original : bool
        ; retained : bool
        ; replay_did_not_mutate : bool
        }]);
  [%expect
    {|
    ((competing_rejected true) (owned_revision_advanced true)
     (admitted_original true) (retained true) (replay_did_not_mutate true))
  |}]
;;

let%expect_test "unrelated reservation invalidates preparation without admitting a run" =
  with_startup_actor (fun actor _ _ request _ _ scope reference _ ->
    let original = request "displaced-preparation" in
    let preparation =
      A.Session_actor.begin_run_preparation
        actor
        ~authorize:(fun _ -> Ok ())
        ~principal_id
        ~request:original
        ~request_sha256:(String.make 64 'b')
      |> protocol_ok
      |> function
      | A.Run_preparation.Decision.Prepare preparation -> preparation
      | Retained _ -> failwith "fresh fixture unexpectedly retained an admission"
    in
    ignore
      (A.Session_actor.reserve_history_block actor ~count:1 |> protocol_ok
       : A.History_id_source.reservation);
    let rejected =
      match
        A.Session_actor.admit_prepared_run
          actor
          ~command_audit:None
          ~preparation
          ~scope
          ~session:reference
          ~entry:None
        |> protocol_ok
      with
      | A.Run_admission_outcome.Rejected _ -> true
      | Admitted _ | Uncertain _ -> false
    in
    let state = A.Session_actor.state actor |> protocol_ok in
    let index = Option.value state.run_state ~default:A.Run_state.empty in
    let no_receipt =
      A.Run_state.receipt
        index
        ~principal_id
        ~key:original.key
        ~request_sha256:(String.make 64 'b')
      |> protocol_ok
      |> Option.is_none
    in
    let fresh = request "after-displaced-preparation" in
    let released =
      match
        A.Session_actor.begin_run_preparation
          actor
          ~authorize:(fun _ -> Ok ())
          ~principal_id
          ~request:fresh
          ~request_sha256:(String.make 64 'c')
        |> protocol_ok
      with
      | A.Run_preparation.Decision.Prepare next ->
        A.Session_actor.end_run_preparation actor next |> protocol_ok;
        true
      | Retained _ -> false
    in
    print_s [%sexp { rejected : bool; no_receipt : bool; released : bool }]);
  [%expect {| ((rejected true) (no_receipt true) (released true)) |}]
;;

let%expect_test
    "preparation checks policy again and distinguishes an attempted failed write"
  =
  with_startup_actor (fun actor authorized reject_commit request _ _ scope reference _ ->
    let authorize _ =
      if !authorized
      then Ok ()
      else
        Error
          (P.Error.create
             Permission_denied
             ~message:"revoked preparation"
             ~retryable:false
             ())
    in
    let prepare key digest =
      let request = request key in
      A.Session_actor.begin_run_preparation
        actor
        ~authorize
        ~principal_id
        ~request
        ~request_sha256:(String.make 64 digest)
      |> protocol_ok
      |> function
      | A.Run_preparation.Decision.Prepare preparation -> preparation
      | Retained _ -> failwith "fresh fixture unexpectedly retained an admission"
    in
    let revoked = prepare "revoked-preparation" 'd' in
    authorized := false;
    let before = A.Session_actor.state actor |> protocol_ok in
    let rejected_before_write =
      match
        A.Session_actor.admit_prepared_run
          actor
          ~command_audit:None
          ~preparation:revoked
          ~scope
          ~session:reference
          ~entry:None
        |> protocol_ok
      with
      | A.Run_admission_outcome.Rejected _ -> true
      | Admitted _ | Uncertain _ -> false
    in
    let after = A.Session_actor.state actor |> protocol_ok in
    authorized := true;
    let failed = prepare "uncertain-preparation" 'e' in
    reject_commit := true;
    let attempted_write_uncertain =
      match
        A.Session_actor.admit_prepared_run
          actor
          ~command_audit:None
          ~preparation:failed
          ~scope
          ~session:reference
          ~entry:None
        |> protocol_ok
      with
      | A.Run_admission_outcome.Uncertain _ -> true
      | Admitted _ | Rejected _ -> false
    in
    reject_commit := false;
    let next = prepare "after-uncertain-preparation" 'f' in
    A.Session_actor.end_run_preparation actor next |> protocol_ok;
    let revoked_did_not_mutate =
      Int64.equal before.counters.revision after.counters.revision
    in
    print_s
      [%sexp
        { rejected_before_write : bool
        ; revoked_did_not_mutate : bool
        ; attempted_write_uncertain : bool
        }]);
  [%expect
    {|
    ((rejected_before_write true) (revoked_did_not_mutate true)
     (attempted_write_uncertain true))
  |}]
;;

let%expect_test "foreign preparation release cannot displace the actor's actual issuer" =
  with_startup_actor (fun actor _ _ request _ _ _ _ _ ->
    let original = request "actual-preparation-owner" in
    let state = A.Session_actor.state actor |> protocol_ok in
    let digest = String.make 64 'a' in
    let actual =
      A.Session_actor.begin_run_preparation
        actor
        ~authorize:(fun _ -> Ok ())
        ~principal_id
        ~request:original
        ~request_sha256:digest
      |> protocol_ok
      |> function
      | A.Run_preparation.Decision.Prepare preparation -> preparation
      | Retained _ -> failwith "fresh fixture unexpectedly retained an admission"
    in
    let foreign =
      A.Run_preparation.create
        ~owner:(A.Run_preparation.Owner.create ())
        ~state
        ~principal_id
        ~request:original
        ~request_sha256:digest
        ~authorize:(fun _ -> Ok ())
      |> protocol_ok
    in
    let foreign_close_rejected =
      Result.is_error (A.Session_actor.end_run_preparation actor foreign)
    in
    let actual_still_live =
      Result.is_ok
        (A.Session_actor.reserve_run_history_block actor ~preparation:actual ~count:1)
    in
    A.Session_actor.end_run_preparation actor actual |> protocol_ok;
    let duplicate_close_safe =
      Result.is_ok (A.Session_actor.end_run_preparation actor actual)
    in
    print_s
      [%sexp
        { foreign_close_rejected : bool
        ; actual_still_live : bool
        ; duplicate_close_safe : bool
        }]);
  [%expect
    {|
    ((foreign_close_rejected true) (actual_still_live true)
     (duplicate_close_safe true))
  |}]
;;

let%expect_test "constructor schedule cancellation preserves admission custody" =
  with_startup_actor (fun actor _ _ request _ _ scope reference _ ->
    let original = request "constructor-schedule-cancel" in
    let preparation =
      A.Session_actor.begin_run_preparation
        actor
        ~authorize:(fun _ -> Ok ())
        ~principal_id
        ~request:original
        ~request_sha256:(String.make 64 'a')
      |> protocol_ok
      |> function
      | A.Run_preparation.Decision.Prepare preparation -> preparation
      | Retained _ -> failwith "fresh fixture unexpectedly retained an admission"
    in
    let schedule =
      P.Schedule.
        { id = P.Id.Schedule.of_string "sch_run_constructor_cancel" |> protocol_ok
        ; session_id
        ; generation = original.generation
        ; payload = `Null
        ; created_at = timestamp
        ; next_due_at = P.Timestamp.add_ms timestamp 1000 |> protocol_ok
        ; misfire = Deliver_once_immediately
        ; status = Scheduled
        ; delivery_count = 0
        ; last_delivery_at = None
        ; ownership = None
        ; delivery_cancellation = None
        }
    in
    let scheduled =
      A.Session_actor.add_run_constructor_schedule actor ~preparation schedule
      |> protocol_ok
    in
    let cancelled =
      A.Session_actor.cancel_run_constructor_schedule
        actor
        ~preparation
        ~schedule_id:scheduled.id
      |> protocol_ok
    in
    let actually_cancelled =
      match cancelled.status with
      | Cancelled -> true
      | Scheduled | Delivering | Delivered | Failed _ -> false
    in
    let admitted =
      match
        A.Session_actor.admit_prepared_run
          actor
          ~command_audit:None
          ~preparation
          ~scope
          ~session:reference
          ~entry:None
        |> protocol_ok
      with
      | A.Run_admission_outcome.Admitted _ -> true
      | Rejected _ | Uncertain _ -> false
    in
    print_s [%sexp { actually_cancelled : bool; admitted : bool }]);
  [%expect {| ((actually_cancelled true) (admitted true)) |}]
;;

let%expect_test "captured runtime installation advances only its owned accounting policy" =
  with_startup_actor (fun actor _ _ request _ _ scope reference _ ->
    let original = request "constructor-runtime-install" in
    let preparation =
      A.Session_actor.begin_run_preparation
        actor
        ~authorize:(fun _ -> Ok ())
        ~principal_id
        ~request:original
        ~request_sha256:(String.make 64 'a')
      |> protocol_ok
      |> function
      | A.Run_preparation.Decision.Prepare preparation -> preparation
      | Retained _ -> failwith "fresh fixture unexpectedly retained an admission"
    in
    let policy = Chat_response.Runtime_semantics.default_policy in
    A.Session_actor.enable_run_constructor_turn_budget actor ~preparation policy
    |> protocol_ok;
    let installed = A.Session_actor.state actor |> protocol_ok in
    A.Session_actor.enable_run_constructor_turn_budget actor ~preparation policy
    |> protocol_ok;
    ignore
      (A.Session_actor.checkpoint_run_moderator actor ~preparation installed.moderator
       |> protocol_ok
       : P.Session.t);
    let matched = A.Session_actor.state actor |> protocol_ok in
    let matching_install_did_not_mutate =
      Int64.equal installed.counters.revision matched.counters.revision
    in
    let admitted_original =
      match
        A.Session_actor.admit_prepared_run
          actor
          ~command_audit:None
          ~preparation
          ~scope
          ~session:reference
          ~entry:None
        |> protocol_ok
      with
      | A.Run_admission_outcome.Admitted _ -> true
      | Rejected _ | Uncertain _ -> false
    in
    print_s [%sexp { matching_install_did_not_mutate : bool; admitted_original : bool }]);
  [%expect {| ((matching_install_did_not_mutate true) (admitted_original true)) |}]
;;

let%expect_test "actual exact job and timer wakes consume once under current authority" =
  List.iter [ `Job; `Timer ] ~f:(fun kind ->
    with_startup_actor (fun actor authorized _ request start manager _ _ _ ->
      ignore (start (request "exact-owned-wake") |> protocol_ok : P.Run_receipt.t);
      let before =
        Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
      in
      let job = ref None in
      let timer = ref None in
      A.Session_actor.with_current_moderator_event
        actor
        ~operation_id:None
        ~event:Session_start
        ~snapshot:(fun () -> Ok before)
        (fun ~executing ~retirement_reason:_ ~event:_ ~execute:_ ~commit ->
           let service =
             A.Session_actor.run_actions actor executing
             |> protocol_ok
             |> Option.value_exn
           in
           let owner = P.Job.Moderator_event executing.context.id in
           let occurrence =
             match kind with
             | `Job ->
               let registry = native_registry (ref 0) ~raises:false in
               let request =
                 Background_execution_tests.capture_tool
                   registry
                   { Chat_response.One_off_request.default_policy with
                     max_output_bytes = 512
                   }
               in
               let staged =
                 A.Session_actor.prepare_background_job_launch actor ~owner request
                 |> protocol_ok
               in
               job := Some staged;
               A.Session_actor.stage_background_job
                 actor
                 ~job:staged
                 ~capacity:{ publish = ignore; abort = ignore }
               |> protocol_ok;
               A.Session_actor.select_background_jobs actor ~owner ~ids:[ staged.id ]
               |> protocol_ok;
               P.Run_wake.Occurrence.Job_completion
                 { job_id = staged.id; attempt = Int.succ staged.attempt }
             | `Timer ->
               let creation, staged =
                 A.Session_actor.create_script_schedule
                   actor
                   ~owner
                   ~source:executing.context.source
                   ~delay_ms:0
                   ~payload:(`String "owned timer")
                   ~misfire:Deliver_once_immediately
                 |> protocol_ok
               in
               timer := Some staged;
               A.Session_actor.select_schedule_mutations
                 actor
                 ~owner
                 ~source:executing.context.source
                 ~receipts:[ creation ]
               |> protocol_ok;
               let ownership = Option.value_exn staged.ownership in
               Delivered_timer
                 { schedule_id = staged.id
                 ; delivery_count = Int.succ staged.delivery_count
                 ; creator = ownership.creator
                 ; subscription = ownership.subscription
                 }
           in
           let state = A.Session_actor.state actor |> protocol_ok in
           let run =
             Option.value_exn state.run_state |> A.Run_state.runs |> List.hd_exn
           in
           let wake =
             P.Run_wake.create ~run_id:run.id ~source:run.source ~occurrence
             |> protocol_ok
           in
           let transaction = A.Run_action_service.transaction service in
           let ticket = transaction.handlers.stage (Wait wake) |> Result.ok_or_failwith in
           ignore
             (transaction.prepare [ ticket ] |> Result.ok_or_failwith
              : P.Run_action.t option);
           commit
             ~snapshot:before
             ~requests:
               { request_turn = false; request_compaction = false; end_session = None })
      |> protocol_ok
      |> ignore;
      let queued =
        let frame, commit_delivery =
          match kind with
          | `Job ->
            let staged = Option.value_exn !job in
            let claimed =
              A.Session_actor.claim_job
                actor
                ~job_id:staged.id
                ~generation:staged.generation
              |> protocol_ok
              |> Option.value_exn
            in
            let completed =
              A.Session_actor.complete_background_job
                actor
                ~job_id:staged.id
                ~generation:staged.generation
                ~attempt:claimed.attempt
                (Succeeded (`String "done"))
              |> protocol_ok
            in
            let state = A.Session_actor.state actor |> protocol_ok in
            let observer =
              A.Moderator_checkpoint.observer state.moderator
              |> protocol_ok
              |> Option.value_exn
            in
            let frame =
              A.Background_job_event.frame ~state ~observer completed
              |> protocol_ok
              |> Chat_response.Background_delivery.capture
            in
            ( frame
            , fun snapshot ->
                A.Session_actor.deliver_job
                  ~expected:before
                  ~expected_job:completed
                  actor
                  ~job_id:completed.id
                  ~generation:completed.generation
                  ~moderator_snapshot:(Some (A.Moderator_checkpoint.encode snapshot))
                |> protocol_ok
                |> ignore )
          | `Timer ->
            let staged = Option.value_exn !timer in
            let claimed =
              A.Session_actor.claim_schedule
                actor
                ~schedule_id:staged.id
                ~generation:staged.generation
              |> protocol_ok
              |> Option.value_exn
            in
            let frame =
              Chat_response.Schedule_delivery.capture claimed |> Result.ok_or_failwith
            in
            ( frame
            , fun snapshot ->
                A.Session_actor.complete_schedule
                  ~expected:before
                  ~expected_schedule:claimed
                  actor
                  ~schedule_id:claimed.id
                  ~generation:claimed.generation
                  ~moderator_snapshot:(Some (A.Moderator_checkpoint.encode snapshot))
                |> protocol_ok
                |> ignore )
        in
        let frame = Session.Snapshot.of_value frame |> Result.ok_or_failwith in
        let snapshot =
          { before with
            queued_internal_events = before.queued_internal_events @ [ frame ]
          }
        in
        commit_delivery snapshot;
        snapshot
      in
      let waiting = A.Session_actor.state actor |> protocol_ok in
      let run = Option.value_exn waiting.run_state |> A.Run_state.runs |> List.hd_exn in
      let wake =
        match run.lifecycle with
        | Waiting wake -> wake
        | _ -> failwith "actual wait did not persist"
      in
      let candidate, captured =
        A.Queued_moderator_event.claim
          ~state:waiting
          ~id:(P.Id.Moderator_execution.of_string "mex_wake_negative" |> protocol_ok)
          ~snapshot:queued
          ~now:timestamp
        |> protocol_ok
      in
      let matches executing wake =
        match A.Run_wake_owner.claim_matches waiting ~run ~wake ~executing with
        | Ok matches -> matches
        | Error _ -> false
      in
      let wrong_source =
        P.Moderator_execution.create
          { candidate.context with
            source = { candidate.context.source with source_sha256 = String.make 64 'f' }
          }
        |> protocol_ok
      in
      let wrong_generation =
        P.Moderator_execution.create
          { candidate.context with generation = Int.succ candidate.context.generation }
        |> protocol_ok
      in
      let wrong_occurrence =
        let occurrence =
          match wake.occurrence with
          | Job_completion { job_id; attempt } ->
            P.Run_wake.Occurrence.Job_completion { job_id; attempt = Int.succ attempt }
          | Delivered_timer { schedule_id; delivery_count; creator; subscription } ->
            Delivered_timer
              { schedule_id
              ; delivery_count = Int.succ delivery_count
              ; creator
              ; subscription
              }
          | Subscription_delivery _ -> failwith "fixture unexpectedly used subscription"
        in
        P.Run_wake.create ~run_id:run.id ~source:run.source ~occurrence |> protocol_ok
      in
      let exact_frame_matches = matches candidate wake in
      let foreign_source_rejected = not (matches wrong_source wake) in
      let foreign_generation_rejected = not (matches wrong_generation wake) in
      let wrong_occurrence_rejected = not (matches candidate wrong_occurrence) in
      let revision = waiting.counters.revision in
      let claimed_scope = ref false in
      let claim () =
        A.Session_actor.with_idle_queued_moderator_event_tools
          actor
          ~snapshot:queued
          (fun ~executing ~retirement_reason ~event:_ ~execute:_ ~commit ->
             if Option.is_some retirement_reason then failwith "valid wake retired";
             let service =
               A.Session_actor.run_actions actor executing
               |> protocol_ok
               |> Option.value_exn
             in
             claimed_scope := true;
             ignore
               ((A.Run_action_service.transaction service).prepare []
                |> Result.ok_or_failwith
                : P.Run_action.t option);
             commit
               ~snapshot:{ queued with queued_internal_events = [] }
               ~requests:
                 { request_turn = false; request_compaction = false; end_session = None })
      in
      authorized := false;
      let revoked_rejected = Result.is_error (claim ()) in
      let rejected_unchanged =
        Int64.equal
          revision
          (A.Session_actor.state actor |> protocol_ok).counters.revision
      in
      authorized := true;
      let actual_callback = claim () |> protocol_ok in
      let duplicate_rejected = Result.is_error (claim ()) in
      let state = A.Session_actor.state actor |> protocol_ok in
      let index = Option.value_exn state.run_state in
      let run = List.hd_exn (A.Run_state.runs index) in
      let duplicate_frame_retired =
        Option.is_some
          (A.Queued_moderator_event.delivery_retirement_reason
             ~state
             ~observer:candidate.context.source
             ~event:captured
             ~subscription_expired:(fun _ -> Ok false)
           |> protocol_ok)
      in
      let callback_proof_once =
        Int.equal
          1
          (List.count run.terminal_work ~f:(fun proof ->
             match proof.work.key, proof.outcome with
             | Retained (Moderator_execution _), Succeeded -> true
             | _ -> false))
      in
      let consumed_once =
        Int.equal
          1
          (List.count (A.Run_state.intents index) ~f:(fun intent ->
             match intent.action, intent.disposition with
             | Wait _, Consumed None -> true
             | _ -> false))
      in
      print_s
        [%sexp
          { exact_frame_matches : bool
          ; foreign_source_rejected : bool
          ; foreign_generation_rejected : bool
          ; wrong_occurrence_rejected : bool
          ; duplicate_frame_retired : bool
          ; revoked_rejected : bool
          ; rejected_unchanged : bool
          ; actual_callback : bool
          ; claimed_scope = (!claimed_scope : bool)
          ; duplicate_rejected : bool
          ; consumed_once : bool
          ; callback_proof_once : bool
          ; active = (P.Run.Lifecycle.equal run.lifecycle Active : bool)
          }]));
  [%expect
    {|
    ((exact_frame_matches true) (foreign_source_rejected true)
     (foreign_generation_rejected true) (wrong_occurrence_rejected true)
     (duplicate_frame_retired true) (revoked_rejected true)
     (rejected_unchanged true) (actual_callback true) (claimed_scope true)
     (duplicate_rejected true) (consumed_once true) (callback_proof_once true)
     (active true))
    ((exact_frame_matches true) (foreign_source_rejected true)
     (foreign_generation_rejected true) (wrong_occurrence_rejected true)
     (duplicate_frame_retired true) (revoked_rejected true)
     (rejected_unchanged true) (actual_callback true) (claimed_scope true)
     (duplicate_rejected true) (consumed_once true) (callback_proof_once true)
     (active true))
    |}]
;;

let%expect_test "unbounded inline job Wait rejects before staged work publication" =
  with_startup_actor (fun actor _ _ request start manager _ _ _ ->
    ignore (start (request "oversized-inline-wait") |> protocol_ok : P.Run_receipt.t);
    let registry = native_registry (ref 0) ~raises:false in
    let captured =
      Background_execution_tests.capture_tool
        registry
        Chat_response.One_off_request.default_policy
    in
    let before =
      Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
    in
    let published = ref 0 in
    let result =
      A.Session_actor.with_current_moderator_event
        actor
        ~operation_id:None
        ~event:Session_start
        ~snapshot:(fun () -> Ok before)
        (fun ~executing ~retirement_reason:_ ~event:_ ~execute:_ ~commit ->
           let service =
             A.Session_actor.run_actions actor executing
             |> protocol_ok
             |> Option.value_exn
           in
           let owner = P.Job.Moderator_event executing.context.id in
           let job =
             A.Session_actor.prepare_background_job_launch actor ~owner captured
             |> protocol_ok
           in
           A.Session_actor.stage_background_job
             actor
             ~job
             ~capacity:{ publish = (fun () -> Int.incr published); abort = ignore }
           |> protocol_ok;
           A.Session_actor.select_background_jobs actor ~owner ~ids:[ job.id ]
           |> protocol_ok;
           let state = A.Session_actor.state actor |> protocol_ok in
           let run =
             state.run_state |> Option.value_exn |> A.Run_state.runs |> List.hd_exn
           in
           let wake =
             P.Run_wake.create
               ~run_id:run.id
               ~source:run.source
               ~occurrence:
                 (Job_completion { job_id = job.id; attempt = Int.succ job.attempt })
             |> protocol_ok
           in
           let transaction = A.Run_action_service.transaction service in
           let ticket = transaction.handlers.stage (Wait wake) |> Result.ok_or_failwith in
           ignore
             (transaction.prepare [ ticket ] |> Result.ok_or_failwith
              : P.Run_action.t option);
           commit
             ~snapshot:before
             ~requests:
               { request_turn = false; request_compaction = false; end_session = None })
    in
    let state = A.Session_actor.state actor |> protocol_ok in
    let index = Option.value_exn state.run_state in
    let rejected =
      match result with
      | Error error -> P.Error.equal_code error.code Resource_limit
      | Ok _ -> false
    in
    print_s
      [%sexp
        { rejected : bool
        ; no_publication = (Int.equal !published 0 : bool)
        ; no_durable_job = (List.is_empty state.jobs : bool)
        ; no_wait_intent = (List.is_empty (A.Run_state.intents index) : bool)
        }]);
  [%expect
    {|
    ((rejected true) (no_publication true) (no_durable_job true)
     (no_wait_intent true))
    |}]
;;

let%expect_test "actual retry retains failed attempt and wakes its callback once" =
  let publisher = ref None in
  with_startup_actor
    ~make_job_results:(fun env sw initial ->
      let fixture = Job_artifact_fixtures.create env sw initial in
      publisher := Some fixture.publisher;
      fixture.publisher)
    (fun actor _ _ request start manager _ _ attachment ->
       ignore (start (request "retry-exact-owned-wake") |> protocol_ok : P.Run_receipt.t);
       let before =
         Chat_response.Moderator_manager.identity_snapshot manager
         |> Result.ok_or_failwith
       in
       let staged = ref None in
       A.Session_actor.with_current_moderator_event
         actor
         ~operation_id:None
         ~event:Session_start
         ~snapshot:(fun () -> Ok before)
         (fun ~executing ~retirement_reason:_ ~event:_ ~execute:_ ~commit ->
            let service =
              A.Session_actor.run_actions actor executing
              |> protocol_ok
              |> Option.value_exn
            in
            let owner = P.Job.Moderator_event executing.context.id in
            let captured =
              Background_execution_tests.capture_tool
                (native_registry (ref 0) ~raises:false)
                Chat_response.One_off_request.default_policy
            in
            let job =
              A.Session_actor.prepare_background_job_launch actor ~owner captured
              |> protocol_ok
            in
            staged := Some job;
            A.Session_actor.stage_background_job
              actor
              ~job
              ~capacity:{ publish = ignore; abort = ignore }
            |> protocol_ok;
            A.Session_actor.select_background_jobs actor ~owner ~ids:[ job.id ]
            |> protocol_ok;
            let run =
              (A.Session_actor.state actor |> protocol_ok).run_state
              |> Option.value_exn
              |> A.Run_state.runs
              |> List.hd_exn
            in
            let wake =
              P.Run_wake.create
                ~run_id:run.id
                ~source:run.source
                ~occurrence:(Job_completion { job_id = job.id; attempt = 1 })
              |> protocol_ok
            in
            let transaction = A.Run_action_service.transaction service in
            let ticket =
              transaction.handlers.stage (Wait wake) |> Result.ok_or_failwith
            in
            ignore
              (transaction.prepare [ ticket ] |> Result.ok_or_failwith
               : P.Run_action.t option);
            commit
              ~snapshot:before
              ~requests:
                { request_turn = false; request_compaction = false; end_session = None })
       |> protocol_ok
       |> ignore;
       let staged = Option.value_exn !staged in
       (* The existing authorized host job mutation configures retry explicitly;
          authored generic launches themselves deliberately default to Never. *)
       ignore
         (A.Session_actor.change_job
            actor
            ~attachment_id:attachment.id
            { staged with retry_policy = Safe_retry { max_attempts = 2; backoff_ms = 0 } }
          |> protocol_ok
          : P.Session.t);
       let first =
         A.Session_actor.claim_job actor ~job_id:staged.id ~generation:staged.generation
         |> protocol_ok
         |> Option.value_exn
       in
       let bounded_request =
         Background_execution_tests.capture_tool
           (native_registry (ref 0) ~raises:false)
           { Chat_response.One_off_request.default_policy with max_output_bytes = 512 }
       in
       let capacity =
         A.Run_job_capacity.capture
           { first with
             payload = Chat_response.Background_request.to_json bounded_request
           }
           ~publisher:None
         |> protocol_ok
       in
       let normalized_host_outcomes =
         List.for_all
           [ P.Completion.Succeeded (`String (String.make 2000 'x'))
           ; Failed
               { code = "fixture.host_failure"
               ; message = String.make 2000 'x'
               ; retryable = true
               ; details = `String (String.make 2000 'x')
               }
           ; Cancelled (String.make 2000 'x')
           ; Expired
           ]
           ~f:(fun original ->
             let normalized, _ =
               A.Run_job_capacity.normalize_completion capacity original |> protocol_ok
             in
             let bounded =
               String.length (Jsonaf.to_string (P.Completion.to_json normalized)) <= 515
             in
             bounded
             &&
             match original, normalized with
             | Succeeded _, Failed _ -> true
             | Failed _, Failed failure -> failure.retryable
             | Cancelled _, Cancelled _ | Expired, Expired -> true
             | _ -> false)
       in
       let failure =
         P.Completion.Failed
           { code = "fixture.retry"
           ; message = "actual failed attempt"
           ; retryable = true
           ; details = `Object [ "large", `String (String.make 2000 'x') ]
           }
       in
       let queued =
         A.Session_actor.complete_background_job
           actor
           ~job_id:first.id
           ~generation:first.generation
           ~attempt:first.attempt
           failure
         |> protocol_ok
       in
       let pending_state = A.Session_actor.state actor |> protocol_ok in
       let index = Option.value_exn pending_state.run_state in
       let round_trip state =
         A.Session_state_document.authored state
         |> fun document ->
         A.Session_state_document.encode document ~limits:document_limits
         |> document_ok
         |> A.Session_state_document.decode ~limits:document_limits
         |> document_ok
         |> A.Session_state_document.value
       in
       let recovered_pending =
         let restored = round_trip pending_state in
         A.Run_retirement.recover
           (Option.value_exn restored.run_state)
           ~session_revision:restored.counters.revision
           ~now:timestamp
         |> protocol_ok
       in
       let pending_recovery_no_replay =
         List.for_all (A.Run_state.job_deliveries recovered_pending) ~f:(fun delivery ->
           match A.Run_job_delivery.disposition delivery with
           | Retired _ -> true
           | Pending | Enqueued _ | Claimed _ -> false)
       in
       let delivery = A.Run_state.job_deliveries index |> List.hd_exn in
       let frame = A.Run_job_delivery.frame delivery in
       let exact_artifact =
         match frame.result with
         | Artifact { reference = artifact; outcome = _ } ->
           Int.equal artifact.attempt 1
           && P.Completion.equal
                (Agent_store.Job_result_store.Publisher.load
                   (Option.value_exn !publisher)
                   artifact
                 |> protocol_ok)
                failure
         | Inline _ -> false
       in
       let run = A.Run_state.runs index |> List.hd_exn in
       let waiting_first =
         match run.lifecycle with
         | Waiting wake ->
           (match wake.occurrence with
            | Job_completion { attempt; _ } -> Int.equal attempt 1
            | Delivered_timer _ | Subscription_delivery _ -> false)
         | Admitted | Active | Terminal _ -> false
       in
       let failed_first =
         List.exists run.terminal_work ~f:(fun proof ->
           match proof.work.key, proof.outcome with
           | Retained (Job { attempt = 1; _ }), Failed -> true
           | _ -> false)
       in
       let owns_second =
         List.exists run.owned_work ~f:(fun work ->
           match work.key with
           | Retained (Job { attempt = 2; _ }) -> true
           | _ -> false)
       in
       let second =
         A.Session_actor.claim_job actor ~job_id:queued.id ~generation:queued.generation
         |> protocol_ok
         |> Option.value_exn
       in
       let artifact_survives_retry =
         match frame.result with
         | Artifact { reference = artifact; outcome = _ } ->
           P.Completion.equal
             (Agent_store.Job_result_store.Publisher.load
                (Option.value_exn !publisher)
                artifact
              |> protocol_ok)
             failure
         | Inline _ -> false
       in
       let event =
         Chat_response.Background_delivery.capture frame
         |> Session.Snapshot.of_value
         |> Result.ok_or_failwith
       in
       let after =
         { before with
           queued_internal_events = before.queued_internal_events @ [ event ]
         }
       in
       A.Session_actor.enqueue_run_job_delivery
         actor
         ~delivery
         ~before
         ~after:(A.Moderator_checkpoint.encode after)
       |> protocol_ok;
       let enqueued_state = A.Session_actor.state actor |> protocol_ok |> round_trip in
       let recovered_enqueued =
         A.Run_retirement.recover
           (Option.value_exn enqueued_state.run_state)
           ~session_revision:enqueued_state.counters.revision
           ~now:timestamp
         |> protocol_ok
       in
       let enqueued_recovery_no_replay =
         List.for_all (A.Run_state.job_deliveries recovered_enqueued) ~f:(fun delivery ->
           match A.Run_job_delivery.disposition delivery with
           | Retired _ -> true
           | Pending | Enqueued _ | Claimed _ -> false)
       in
       let duplicate_enqueue_rejected =
         Result.is_error
           (A.Session_actor.enqueue_run_job_delivery
              actor
              ~delivery
              ~before
              ~after:(A.Moderator_checkpoint.encode after))
       in
       let actual_callback = ref false in
       A.Session_actor.with_idle_queued_moderator_event_tools
         actor
         ~snapshot:after
         (fun ~executing ~retirement_reason ~event:_ ~execute:_ ~commit ->
            if Option.is_some retirement_reason
            then failwith "exact retained retry frame retired";
            let service =
              A.Session_actor.run_actions actor executing
              |> protocol_ok
              |> Option.value_exn
            in
            actual_callback := true;
            ignore
              ((A.Run_action_service.transaction service).prepare []
               |> Result.ok_or_failwith
               : P.Run_action.t option);
            commit
              ~snapshot:{ after with queued_internal_events = [] }
              ~requests:
                { request_turn = false; request_compaction = false; end_session = None })
       |> protocol_ok
       |> ignore;
       let finished = A.Session_actor.state actor |> protocol_ok in
       let index = Option.value_exn finished.run_state in
       let run = A.Run_state.runs index |> List.hd_exn in
       let immutable_failed_first =
         List.exists run.terminal_work ~f:(fun proof ->
           match proof.work.key, proof.outcome with
           | Retained (Job { attempt = 1; _ }), Failed -> true
           | _ -> false)
       in
       let second_unchanged =
         List.exists finished.jobs ~f:(fun job ->
           P.Id.Job.equal job.id second.id
           && Int.equal job.attempt 2
           &&
           match job.status with
           | Running -> true
           | _ -> false)
       in
       let claimed_once =
         match
           A.Run_job_delivery.disposition (A.Run_state.job_deliveries index |> List.hd_exn)
         with
         | Claimed _ -> true
         | Pending | Enqueued _ | Retired _ -> false
       in
       let recovered =
         A.Run_retirement.recover
           index
           ~session_revision:finished.counters.revision
           ~now:timestamp
         |> protocol_ok
       in
       let recovery_retired =
         match
           A.Run_job_delivery.disposition
             (A.Run_state.job_deliveries recovered |> List.hd_exn)
         with
         | Retired _ -> true
         | Pending | Enqueued _ | Claimed _ -> false
       in
       print_s
         [%sexp
           { normalized_host_outcomes : bool
           ; exact_artifact : bool
           ; waiting_first : bool
           ; failed_first : bool
           ; owns_second : bool
           ; duplicate_enqueue_rejected : bool
           ; actual_callback = (!actual_callback : bool)
           ; immutable_failed_first : bool
           ; second_unchanged : bool
           ; artifact_survives_retry : bool
           ; claimed_once : bool
           ; pending_recovery_no_replay : bool
           ; enqueued_recovery_no_replay : bool
           ; recovery_retired : bool
           }]);
  [%expect
    {|
    ((normalized_host_outcomes true) (exact_artifact true) (waiting_first true)
     (failed_first true) (owns_second true) (duplicate_enqueue_rejected true)
     (actual_callback true) (immutable_failed_first true) (second_unchanged true)
     (artifact_survives_retry true) (claimed_once true)
     (pending_recovery_no_replay true) (enqueued_recovery_no_replay true)
     (recovery_retired true))
    |}]
;;

let stage_bounded_owned_wait actor manager =
  let before =
    Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
  in
  let staged = ref None in
  A.Session_actor.with_current_moderator_event
    actor
    ~operation_id:None
    ~event:Session_start
    ~snapshot:(fun () -> Ok before)
    (fun ~executing ~retirement_reason:_ ~event:_ ~execute:_ ~commit ->
       let captured =
         Background_execution_tests.capture_tool
           (native_registry (ref 0) ~raises:false)
           { Chat_response.One_off_request.default_policy with max_output_bytes = 512 }
       in
       let owner = P.Job.Moderator_event executing.context.id in
       let job =
         A.Session_actor.prepare_background_job_launch actor ~owner captured
         |> protocol_ok
       in
       staged := Some job;
       A.Session_actor.stage_background_job
         actor
         ~job
         ~capacity:{ publish = ignore; abort = ignore }
       |> protocol_ok;
       A.Session_actor.select_background_jobs actor ~owner ~ids:[ job.id ] |> protocol_ok;
       let run =
         (A.Session_actor.state actor |> protocol_ok).run_state
         |> Option.value_exn
         |> A.Run_state.runs
         |> List.hd_exn
       in
       let wake =
         P.Run_wake.create
           ~run_id:run.id
           ~source:run.source
           ~occurrence:(Job_completion { job_id = job.id; attempt = 1 })
         |> protocol_ok
       in
       let service =
         A.Session_actor.run_actions actor executing |> protocol_ok |> Option.value_exn
       in
       let transaction = A.Run_action_service.transaction service in
       let ticket = transaction.handlers.stage (Wait wake) |> Result.ok_or_failwith in
       ignore
         (transaction.prepare [ ticket ] |> Result.ok_or_failwith : P.Run_action.t option);
       commit
         ~snapshot:before
         ~requests:
           { request_turn = false; request_compaction = false; end_session = None })
  |> protocol_ok
  |> ignore;
  Option.value_exn !staged, before
;;

let%expect_test "owned job cancellation and interruption preserve actual custody" =
  List.iter [ `Before_claim; `Cancel_running; `Interrupt_running ] ~f:(fun control ->
    with_startup_actor (fun actor _ _ request start manager _ _ _ ->
      ignore (start (request "owned-control") |> protocol_ok : P.Run_receipt.t);
      let staged, before = stage_bounded_owned_wait actor manager in
      let actual =
        match control with
        | `Before_claim -> staged
        | `Cancel_running | `Interrupt_running ->
          A.Session_actor.claim_job actor ~job_id:staged.id ~generation:staged.generation
          |> protocol_ok
          |> Option.value_exn
      in
      let terminal =
        match control with
        | `Before_claim | `Cancel_running ->
          A.Session_actor.cancel_job_internal actor ~job_id:actual.id |> protocol_ok
        | `Interrupt_running ->
          A.Session_actor.interrupt_job
            actor
            ~job_id:actual.id
            ~generation:actual.generation
            ~attempt:actual.attempt
            ~reason:(String.make 2000 'x')
          |> protocol_ok
      in
      let state = A.Session_actor.state actor |> protocol_ok in
      let index = Option.value_exn state.run_state in
      let run = A.Run_state.runs index |> List.hd_exn in
      let truthful_outcome =
        List.exists run.terminal_work ~f:(fun (proof : P.Run_work.Terminal.t) ->
          match proof.work.key, proof.outcome, control with
          | Retained (Job { attempt = 1; _ }), Cancelled, (`Before_claim | `Cancel_running)
          | Retained (Job { attempt = 1; _ }), Interrupted, `Interrupt_running -> true
          | _ -> false)
      in
      let actual_attempt_preserved = Int.equal terminal.attempt actual.attempt in
      let no_fabricated_success =
        not
          (List.exists run.terminal_work ~f:(fun (proof : P.Run_work.Terminal.t) ->
             match proof.work.key, proof.outcome with
             | Retained (Job _), Succeeded -> true
             | _ -> false))
      in
      let finished_truthfully, no_replay =
        match control with
        | `Before_claim ->
          let frame =
            Chat_response.Background_delivery.create ~source:run.source.observer terminal
            |> Result.ok_or_failwith
          in
          let event =
            Chat_response.Background_delivery.capture frame
            |> Session.Snapshot.of_value
            |> Result.ok_or_failwith
          in
          let delivered = { terminal with delivery = Delivered run.updated_at } in
          let delivered_state = { state with jobs = [ delivered ] } in
          let guard state =
            A.Queued_moderator_event.delivery_retirement_reason
              ~state
              ~observer:run.source.observer
              ~event
              ~subscription_expired:(fun _ -> Ok false)
            |> protocol_ok
          in
          let unconfirmed =
            List.map run.terminal_work ~f:(fun (proof : P.Run_work.Terminal.t) ->
              P.Run_work.Terminal.create
                ~work:proof.work
                ~outcome:Unconfirmed
                ~revision:proof.revision
              |> protocol_ok)
          in
          let unrelated_retirement =
            P.Run.create
              ~id:run.id
              ~session:run.session
              ~principal_id:run.principal_id
              ~source:run.source
              ~mode:run.mode
              ~lifecycle:run.lifecycle
              ~revision:run.revision
              ~owned_work:run.owned_work
              ~relinquished_work:run.relinquished_work
              ~terminal_work:unconfirmed
              ~created_at:run.created_at
              ~updated_at:run.updated_at
            |> protocol_ok
          in
          let unrelated_index =
            match A.Run_state.to_jsonaf index with
            | `Object fields ->
              `Object
                (List.Assoc.add
                   fields
                   ~equal:String.equal
                   "runs"
                   (`Array [ P.Run.to_json unrelated_retirement ]))
              |> A.Run_state.of_jsonaf
              |> protocol_ok
            | _ -> failwith "run index object invariant"
          in
          let retired =
            P.Run.Lifecycle.equal run.lifecycle (Terminal Interrupted)
            && List.is_empty (A.Run_state.job_deliveries index)
            && Option.is_some (guard delivered_state)
            && Option.is_none
                 (guard { delivered_state with run_state = Some unrelated_index })
          in
          let repeated =
            A.Session_actor.cancel_job_internal actor ~job_id:terminal.id |> protocol_ok
          in
          let after = A.Session_actor.state actor |> protocol_ok in
          let not_claimed =
            Option.is_none
              (A.Session_actor.claim_job
                 actor
                 ~job_id:terminal.id
                 ~generation:terminal.generation
               |> protocol_ok)
          in
          ( retired
          , Int.equal repeated.attempt 0
            && not_claimed
            && Int64.equal state.counters.revision after.counters.revision )
        | `Cancel_running | `Interrupt_running ->
          let delivery = A.Run_state.job_deliveries index |> List.hd_exn in
          let frame = A.Run_job_delivery.frame delivery in
          let event =
            Chat_response.Background_delivery.capture frame
            |> Session.Snapshot.of_value
            |> Result.ok_or_failwith
          in
          let queued =
            { before with
              queued_internal_events = before.queued_internal_events @ [ event ]
            }
          in
          A.Session_actor.enqueue_run_job_delivery
            actor
            ~delivery
            ~before
            ~after:(A.Moderator_checkpoint.encode queued)
          |> protocol_ok;
          A.Session_actor.with_idle_queued_moderator_event_tools
            actor
            ~snapshot:queued
            (fun ~executing ~retirement_reason ~event:_ ~execute:_ ~commit ->
               if Option.is_some retirement_reason
               then failwith "actual owned control frame retired";
               let service =
                 A.Session_actor.run_actions actor executing
                 |> protocol_ok
                 |> Option.value_exn
               in
               let desired =
                 match control with
                 | `Cancel_running -> P.Run.Terminal.Cancelled
                 | `Interrupt_running -> Interrupted
                 | `Before_claim -> assert false
               in
               let transaction = A.Run_action_service.transaction service in
               let ticket =
                 transaction.handlers.stage
                   (Finish { terminal = desired; relinquish = [] })
                 |> Result.ok_or_failwith
               in
               ignore
                 (transaction.prepare [ ticket ] |> Result.ok_or_failwith
                  : P.Run_action.t option);
               commit
                 ~snapshot:{ queued with queued_internal_events = [] }
                 ~requests:
                   { request_turn = false
                   ; request_compaction = false
                   ; end_session = None
                   })
          |> protocol_ok
          |> ignore;
          let finished = A.Session_actor.state actor |> protocol_ok in
          let index = Option.value_exn finished.run_state in
          let run = A.Run_state.runs index |> List.hd_exn in
          let desired =
            match control with
            | `Cancel_running -> P.Run.Lifecycle.Terminal Cancelled
            | `Interrupt_running -> Terminal Interrupted
            | `Before_claim -> assert false
          in
          let retired = A.Run_state.job_deliveries index |> List.hd_exn in
          let empty = { queued with queued_internal_events = [] } in
          let attempt_replay =
            A.Session_actor.enqueue_run_job_delivery
              actor
              ~delivery:retired
              ~before:empty
              ~after:
                (A.Moderator_checkpoint.encode
                   { empty with queued_internal_events = [ event ] })
          in
          let repeated = A.Session_actor.state actor |> protocol_ok in
          ( P.Run.Lifecycle.equal run.lifecycle desired
          , Result.is_error attempt_replay
            && Int64.equal finished.counters.revision repeated.counters.revision )
      in
      print_s
        [%sexp
          { truthful_outcome : bool
          ; actual_attempt_preserved : bool
          ; no_fabricated_success : bool
          ; finished_truthfully : bool
          ; no_replay : bool
          }]));
  [%expect
    {|
    ((truthful_outcome true) (actual_attempt_preserved true)
     (no_fabricated_success true) (finished_truthfully true) (no_replay true))
    ((truthful_outcome true) (actual_attempt_preserved true)
     (no_fabricated_success true) (finished_truthfully true) (no_replay true))
    ((truthful_outcome true) (actual_attempt_preserved true)
     (no_fabricated_success true) (finished_truthfully true) (no_replay true))
    |}]
;;
