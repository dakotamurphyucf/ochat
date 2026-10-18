open! Core
module P = Agent_protocol
module R = Agent_store.Developer_run_store
module L = Agent_server.Developer_command_lifecycle
module F = Root_owner_tests

let () = Mirage_crypto_rng_unix.use_default ()
let ok = F.ok

let policy () =
  ( L.Disclosure_policy.create ~authorize:(fun _ _ -> true)
  , L.Control_policy.create ~authorize:(fun _ _ -> true) )
;;

let recovery () =
  L.Recovery_adapter.create ~recover:(fun ~claim:_ ~handle:_ ~window ->
    let view = R.Recovery.observe window |> ok in
    assert (List.is_empty (R.Recovery.View.candidates view));
    assert (List.is_empty (R.Recovery.View.inference_candidates view));
    Ok ())
;;

let receipt handle value =
  R.Command.Receipt.create
    value
    ~limits:(R.document_limits handle |> ok)
    ~poll:(fun () -> Eio.Fiber.yield ())
  |> ok
;;

let%test_unit "daemon owner rejects Store limits too small for its own fallback" =
  F.with_store ~max_command_terminal_bytes:1 (fun _env sw store ->
    let disclosure, control = policy () in
    assert (
      Result.is_error
        (L.create
           ~sw
           ~store
           ~max_active_commands:1
           ~disclosure
           ~control
           ~recovery:(recovery ()))))
;;

let%test_unit "Task cleanup distinguishes a retained Store Pending from absent" =
  F.with_store (fun _env sw store ->
    let run_owner = F.owner "dvr_task_pending_cleanup" 0 in
    let captured = F.capture () in
    let handle, _ =
      R.admit_epoch
        store
        ~owner:run_owner
        ~capture_fingerprint:(Chatml_developer_capture.fingerprint captured)
        ~previous:None
      |> ok
    in
    let disclosure, control = policy () in
    let lifecycle =
      L.create
        ~sw
        ~store
        ~max_active_commands:1
        ~disclosure
        ~control
        ~recovery:(recovery ())
      |> ok
    in
    let actor = Operator_authorization.trusted_local F.principal in
    let key = P.Idempotency_key.of_string "task-pending-cleanup" |> ok in
    let request_fingerprint =
      Chatml_source.Digest.of_contents "task pending" ~checkpoint:ignore
    in
    let pending =
      match
        R.Command.prepare_reservation
          handle
          ~key
          ~kind:R.Command.Receipt.Kind.Repl_task_run
          ~request_fingerprint
        |> ok
      with
      | Prepared pending -> pending
      | Unchanged _ -> failwith "expected a fresh Task Pending"
    in
    assert (Option.is_some (R.Command.pending handle |> ok));
    L.with_slot lifecycle ~run_id:run_owner.run_id ~f:(fun slot ->
      let result =
        L.reconcile_unlaunched
          lifecycle
          slot
          ~actor
          ~key
          ~kind:R.Command.Receipt.Kind.Repl_task_run
          ~request_fingerprint
          ~handle:None
      in
      (match result with
       | Error { P.Error.code = Conflict; _ } -> ()
       | Error _ | Ok _ -> failwith "Task Pending was treated as absent");
      R.Command.retire_absent_admission handle pending |> ok;
      match
        L.reconcile_unlaunched
          lifecycle
          slot
          ~actor
          ~key
          ~kind:R.Command.Receipt.Kind.Repl_task_run
          ~request_fingerprint
          ~handle:None
      with
      | Ok L.Unlaunched.No_durable_row -> Ok ()
      | Ok L.Unlaunched.Terminal_row | Error _ ->
        failwith "retired Pending was not absent")
    |> ok;
    L.close_and_wait lifecycle |> ok)
;;

let%test_unit "daemon command callback survives its submitting slot and publishes once" =
  let fault_armed = ref false in
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  F.with_store
    ~wrap_env:(fun env ->
      Developer_run_store_test_suite.Developer_run_store_tests
      .fault_after_authority_rename_env
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked
        ~before_rename:(fun () ->
          if !fault_armed
          then (
            fault_armed := false;
            raise (Core_unix.Unix_error (EIO, "injected command terminal save", "")))))
    (fun env sw store ->
       let run_owner = F.owner "dvr_command_lifecycle" 0 in
       let captured = F.capture () in
       let handle, _ =
         R.admit_epoch
           store
           ~owner:run_owner
           ~capture_fingerprint:(Chatml_developer_capture.fingerprint captured)
           ~previous:None
         |> ok
       in
       let disclosure, control = policy () in
       let joined_keys = ref [] in
       let observer_fail_key = ref None in
       let lifecycle =
         L.create_with_hook
           ~on_committed:None
           ~on_joined_terminal:
             (Some
                (fun row ->
                  let key = R.Command.Row.key row in
                  if
                    Option.value_map
                      !observer_fail_key
                      ~default:false
                      ~f:(P.Idempotency_key.equal key)
                  then failwith "injected trace observer failure";
                  joined_keys := key :: !joined_keys))
           ~sw
           ~store
           ~max_active_commands:2
           ~disclosure
           ~control
           ~recovery:(recovery ())
         |> ok
       in
       let actor = Operator_authorization.trusted_local F.principal in
       let run_id = run_owner.run_id in
       let key = P.Idempotency_key.of_string "command-lifecycle-check" |> ok in
       let second_key = P.Idempotency_key.of_string "command-lifecycle-second" |> ok in
       observer_fail_key := Some key;
       let request_fingerprint =
         Chatml_source.Digest.of_contents "command check payload" ~checkpoint:ignore
       in
       let entered, signal_entered = Eio.Promise.create () in
       let release, signal_release = Eio.Promise.create () in
       let reserved =
         L.with_slot lifecycle ~run_id ~f:(fun slot ->
           let open Result.Let_syntax in
           let%bind () = L.ensure_startup_recovered lifecycle slot in
           let%bind reserved =
             L.reserve
               lifecycle
               slot
               ~handle
               ~key
               ~kind:R.Command.Receipt.Kind.Check
               ~request_fingerprint
               ~admit:(fun () -> Operator_authorization.is_current actor)
           in
           let callback _active ~sw:_ =
             assert (Result.is_error (L.await_terminal lifecycle ~actor ~run_id ~key));
             Eio.Promise.resolve signal_entered ();
             Eio.Promise.await release;
             Ok (receipt handle (R.Command.Receipt.Check R.Command.Receipt.Check.Ready))
           in
           let%map _ =
             let open Result.Let_syntax in
             let%bind launched = L.launch lifecycle slot ~handle ~reserved ~f:callback in
             assert (
               Result.is_error (L.launch lifecycle slot ~handle ~reserved ~f:callback));
             assert (
               Result.is_error
                 (L.reserve
                    lifecycle
                    slot
                    ~handle
                    ~key:second_key
                    ~kind:R.Command.Receipt.Kind.Check
                    ~request_fingerprint
                    ~admit:(fun () -> true)));
             Ok launched
           in
           reserved)
         |> ok
       in
       Eio.Promise.await entered;
       assert (Option.is_none (R.Command.observe handle ~key:second_key |> ok));
       assert (Option.is_none (L.View.receipt reserved));
       let same =
         L.with_slot lifecycle ~run_id ~f:(fun slot ->
           L.lookup
             lifecycle
             slot
             ~actor
             ~key
             ~kind:R.Command.Receipt.Kind.Check
             ~request_fingerprint)
         |> ok
         |> Option.value_exn
       in
       assert (Option.is_none (L.View.receipt same));
       Eio.Promise.resolve signal_release ();
       let terminal = L.await_terminal lifecycle ~actor ~run_id ~key |> ok in
       (match L.View.receipt terminal with
        | Some receipt ->
          (match R.Command.Receipt.value receipt with
           | R.Command.Receipt.Check Ready -> ()
           | Check (Rejected _ | Interrupted | Cancelled) | Fixture _ | Live _ | Task _ ->
             failwith "wrong command completion")
        | None -> failwith "callback did not settle terminal");
       let retained =
         L.with_slot lifecycle ~run_id ~f:(fun slot ->
           L.lookup
             lifecycle
             slot
             ~actor
             ~key
             ~kind:R.Command.Receipt.Kind.Check
             ~request_fingerprint)
         |> ok
         |> Option.value_exn
       in
       assert (
         R.Command.Receipt.Id.equal
           (R.Command.Row.id (L.View.row retained))
           (R.Command.Row.id (L.View.row terminal)));
       L.with_slot lifecycle ~run_id ~f:(fun slot ->
         assert (
           Result.is_error
             (L.launch lifecycle slot ~handle ~reserved ~f:(fun _ ~sw:_ ->
                failwith "retired reservation relaunched")));
         Ok ())
       |> ok;
       let rejected_key = P.Idempotency_key.of_string "command-revoked-reserve" |> ok in
       L.with_slot lifecycle ~run_id ~f:(fun slot ->
         assert (
           Result.is_error
             (L.reserve
                lifecycle
                slot
                ~handle
                ~key:rejected_key
                ~kind:R.Command.Receipt.Kind.Check
                ~request_fingerprint
                ~admit:(fun () -> false)));
         Ok ())
       |> ok;
       assert (Option.is_none (R.Command.observe handle ~key:rejected_key |> ok));
       let abandoned_key = P.Idempotency_key.of_string "command-abandoned-slot" |> ok in
       let original = Failure "cancel before command launch" in
       (match
          L.with_slot lifecycle ~run_id ~f:(fun slot ->
            ignore
              (L.reserve
                 lifecycle
                 slot
                 ~handle
                 ~key:abandoned_key
                 ~kind:R.Command.Receipt.Kind.Check
                 ~request_fingerprint
                 ~admit:(fun () -> true)
               |> ok
               : L.View.t);
            raise original)
        with
        | exception actual -> assert (phys_equal actual original)
        | Ok _ | Error _ -> failwith "abandoned slot lost original failure");
       let abandoned =
         R.Command.observe handle ~key:abandoned_key |> ok |> Option.value_exn
       in
       (match R.Command.Row.phase (R.Command.View.row abandoned) with
        | R.Command.Row.Phase.Terminal { receipt; _ } ->
          (match R.Command.Receipt.value receipt with
           | R.Command.Receipt.Check Interrupted -> ()
           | Check _ | Fixture _ | Live _ | Task _ ->
             failwith "abandoned slot chose wrong receipt")
        | Reserved | Waiting_permission | Execution_started ->
          failwith "abandoned slot stayed Reserved");
       let fault_key = P.Idempotency_key.of_string "command-lifecycle-fault" |> ok in
       let fault_request =
         Chatml_source.Digest.of_contents "command fault payload" ~checkpoint:ignore
       in
       let fault_entered, signal_fault_entered = Eio.Promise.create () in
       let fault_release, signal_fault_release = Eio.Promise.create () in
       let calls = ref 0 in
       ignore
         (L.with_slot lifecycle ~run_id ~f:(fun slot ->
            let open Result.Let_syntax in
            let%bind reserved =
              L.reserve
                lifecycle
                slot
                ~handle
                ~key:fault_key
                ~kind:R.Command.Receipt.Kind.Check
                ~request_fingerprint:fault_request
                ~admit:(fun () -> true)
            in
            L.launch lifecycle slot ~handle ~reserved ~f:(fun _active ~sw:_ ->
              incr calls;
              Eio.Promise.resolve signal_fault_entered ();
              Eio.Promise.await fault_release;
              Ok (receipt handle (R.Command.Receipt.Check R.Command.Receipt.Check.Ready))))
          |> ok
          : L.View.t);
       Eio.Promise.await fault_entered;
       fault_armed := true;
       Eio.Promise.resolve signal_fault_release ();
       assert (
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
           Result.is_error (L.await_terminal lifecycle ~actor ~run_id ~key:fault_key)));
       assert (not (List.mem !joined_keys fault_key ~equal:P.Idempotency_key.equal));
       sync_blocked := true;
       assert (
         Result.is_error
           (L.with_slot lifecycle ~run_id ~f:(fun slot ->
              L.retire_run lifecycle slot ~actor)));
       assert (Int.equal !calls 1);
       sync_blocked := false;
       L.with_slot lifecycle ~run_id ~f:(fun slot -> L.retire_run lifecycle slot ~actor)
       |> ok;
       let recovered =
         L.with_slot lifecycle ~run_id ~f:(fun slot ->
           L.lookup
             lifecycle
             slot
             ~actor
             ~key:fault_key
             ~kind:R.Command.Receipt.Kind.Check
             ~request_fingerprint:fault_request)
         |> ok
         |> Option.value_exn
       in
       assert (Option.is_some (L.View.receipt recovered));
       assert (Int.equal !calls 1);
       [%test_eq: int] (List.count !joined_keys ~f:(P.Idempotency_key.equal fault_key)) 1;
       L.close_and_wait lifecycle |> ok;
       let dead_lifecycle =
         Eio.Switch.run (fun dead_sw ->
           L.create
             ~sw:dead_sw
             ~store
             ~max_active_commands:2
             ~disclosure
             ~control
             ~recovery:(recovery ())
           |> ok)
       in
       let no_fork_key = P.Idempotency_key.of_string "command-lifecycle-no-fork" |> ok in
       let fork_rejected = ref false in
       L.with_slot dead_lifecycle ~run_id ~f:(fun slot ->
         let open Result.Let_syntax in
         let%bind () = L.ensure_startup_recovered dead_lifecycle slot in
         let%bind prepared =
           L.reserve
             dead_lifecycle
             slot
             ~handle
             ~key:no_fork_key
             ~kind:R.Command.Receipt.Kind.Check
             ~request_fingerprint
             ~admit:(fun () -> true)
         in
         (match
            L.launch dead_lifecycle slot ~handle ~reserved:prepared ~f:(fun _ ~sw:_ ->
              failwith "closed daemon switch ran callback")
          with
          | exception _ -> fork_rejected := true
          | Ok _ | Error _ -> failwith "closed daemon switch accepted launch");
         Ok ())
       |> ok;
       assert !fork_rejected;
       let no_fork =
         R.Command.observe handle ~key:no_fork_key |> ok |> Option.value_exn
       in
       (match R.Command.Row.phase (R.Command.View.row no_fork) with
        | R.Command.Row.Phase.Terminal { receipt; _ } ->
          (match R.Command.Receipt.value receipt with
           | R.Command.Receipt.Check Interrupted -> ()
           | Check _ | Fixture _ | Live _ | Task _ ->
             failwith "unlaunched reservation not retired")
        | Reserved | Waiting_permission | Execution_started ->
          failwith "unlaunched reservation stayed open");
       L.close_and_wait dead_lifecycle |> ok;
       let may_read = ref false in
       let recovery_calls = ref 0 in
       let recovery_entered, signal_recovery_entered = Eio.Promise.create () in
       let disclosure = L.Disclosure_policy.create ~authorize:(fun _ _ -> !may_read) in
       let recovery =
         L.Recovery_adapter.create ~recover:(fun ~claim:_ ~handle:_ ~window ->
           incr recovery_calls;
           if Int.equal !recovery_calls 1
           then (
             Eio.Promise.resolve signal_recovery_entered ();
             Eio.Fiber.await_cancel ())
           else (
             let view = R.Recovery.observe window |> ok in
             assert (List.is_empty (R.Recovery.View.candidates view));
             assert (List.is_empty (R.Recovery.View.inference_candidates view));
             Ok ()))
       in
       let recovering =
         L.create ~sw ~store ~max_active_commands:2 ~disclosure ~control ~recovery |> ok
       in
       assert (
         Result.is_error (L.await_terminal recovering ~actor ~run_id ~key:no_fork_key));
       assert (Int.equal !recovery_calls 0);
       may_read := true;
       let cancellation, signal_cancellation = Eio.Promise.create () in
       let cancelled, signal_cancelled = Eio.Promise.create () in
       Eio.Fiber.fork ~sw (fun () ->
         let outcome =
           match
             Eio.Cancel.sub (fun cancel ->
               Eio.Promise.resolve signal_cancellation cancel;
               L.with_slot recovering ~run_id ~f:(L.ensure_startup_recovered recovering)
               |> ok)
           with
           | exception Eio.Cancel.Cancelled _ -> `Cancelled
           | () -> `Unexpected_success
         in
         Eio.Promise.resolve signal_cancelled outcome);
       let cancel = Eio.Promise.await cancellation in
       Eio.Promise.await recovery_entered;
       assert (Result.is_error (L.close_and_wait recovering));
       Eio.Cancel.cancel cancel Exit;
       (match Eio.Promise.await cancelled with
        | `Cancelled -> ()
        | `Unexpected_success -> failwith "cancelled startup returned success");
       L.with_slot recovering ~run_id ~f:(L.ensure_startup_recovered recovering) |> ok;
       assert (Int.equal !recovery_calls 2);
       L.close_and_wait recovering |> ok)
;;

let%test_unit
    "live Start is one-use under the real Root and cancellation joins one callback"
  =
  let rename_armed = ref false in
  let sync_blocked = ref false in
  let parent_sync_blocked = ref false in
  F.with_store
    ~wrap_env:(fun env ->
      Developer_run_store_test_suite.Developer_run_store_tests
      .fault_after_authority_rename_env
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked
        ~after_rename:(fun () -> sync_blocked := true))
    (fun env sw store ->
       let run_owner = F.owner "dvr_command_live" 0 in
       let captured = F.capture () in
       let handle, _ =
         R.admit_epoch
           store
           ~owner:run_owner
           ~capture_fingerprint:(Chatml_developer_capture.fingerprint captured)
           ~previous:None
         |> ok
       in
       let disclosure, _ = policy () in
       let retire_control_allowed = ref true in
       let control =
         L.Control_policy.create ~authorize:(fun _ _ -> !retire_control_allowed)
       in
       let lifecycle =
         L.create
           ~sw
           ~store
           ~max_active_commands:2
           ~disclosure
           ~control
           ~recovery:(recovery ())
         |> ok
       in
       let run_id = run_owner.run_id in
       (* This lower Start adapter fixture owns its Root before any command.
          Release the clean startup claim so the ordinary Registry.admit path
          may acquire its own; hosted Live instead transfers the retained
          finished claim after durable Start. *)
       L.with_slot lifecycle ~run_id ~f:(fun slot ->
         let open Result.Let_syntax in
         let%bind () = L.ensure_startup_recovered lifecycle slot in
         L.release_finished_epoch lifecycle slot)
       |> ok;
       let actor = Operator_authorization.trusted_local F.principal in
       let capabilities =
         Chat_response.Tool_capability.create
           ~owner:"command-lifecycle"
           ~resource_fingerprint:Digestif.SHA256.(digest_string "command" |> to_hex)
           []
         |> ok
       in
       let limits =
         Agent_server.Developer_root_owner.Limits.create
           ~max_active_runs:2
           ~max_staged_jobs:2
           ~max_native_occupancy:2
           ~max_active_workers:1
           ~max_quarantined_workers:2
           ~max_active_inference_attempts:8
           ~max_admitted_call_bindings:32
         |> F.or_error
       in
       let registry =
         Agent_server.Developer_root_owner.Registry.create
           ~sw
           ~store
           ~capacity:(F.capacity ())
           ~now:F.now
           ~limits
       in
       let root =
         Agent_server.Developer_root_owner.Registry.admit
           registry
           ~actor
           ~capture:captured
           ~registry:capabilities
           ~selected:capabilities
           ~selected_model:None
           ~job_policy:Chat_response.One_off_request.default_policy
           ~handle
           ~workspace_conflict_domain:"command-lifecycle"
         |> ok
       in
       let first_key = P.Idempotency_key.of_string "command-live-start" |> ok in
       let request_fingerprint =
         Chatml_source.Digest.of_contents "command live request" ~checkpoint:ignore
       in
       let entered = ref 0 in
       ignore
         (L.with_slot lifecycle ~run_id ~f:(fun slot ->
            let open Result.Let_syntax in
            let%bind reserved =
              L.reserve
                lifecycle
                slot
                ~handle
                ~key:first_key
                ~kind:R.Command.Receipt.Kind.Live_run
                ~request_fingerprint
                ~admit:(fun () -> true)
            in
            L.launch lifecycle slot ~handle ~reserved ~f:(fun active ~sw:_ ->
              let preparing =
                L.Active.start_preparation active ~actor ~admit:(fun () -> true) |> ok
              in
              let started = L.Active.Preparing.bind_root preparing ~root |> ok in
              assert (Result.is_error (L.Active.Preparing.bind_root preparing ~root));
              assert (L.Active.Started.issued_by_root started root);
              L.Active.Started.consume_for started ~root |> ok;
              assert (Result.is_error (L.Active.Started.consume_for started ~root));
              incr entered;
              Ok
                (receipt
                   handle
                   (R.Command.Receipt.Live (R.Command.Receipt.Live.Succeeded `Null)))))
          |> ok
          : L.View.t);
       let completed =
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
           L.await_terminal lifecycle ~actor ~run_id ~key:first_key |> ok)
       in
       assert (Int.equal !entered 1);
       assert (Option.is_some (L.View.receipt completed));
       let escaped_key = P.Idempotency_key.of_string "command-live-escaped-proof" |> ok in
       let escaped = ref None in
       ignore
         (L.with_slot lifecycle ~run_id ~f:(fun slot ->
            let open Result.Let_syntax in
            let%bind reserved =
              L.reserve
                lifecycle
                slot
                ~handle
                ~key:escaped_key
                ~kind:R.Command.Receipt.Kind.Live_run
                ~request_fingerprint
                ~admit:(fun () -> true)
            in
            L.launch lifecycle slot ~handle ~reserved ~f:(fun active ~sw:_ ->
              escaped
              := Some (L.Active.start_live active ~root ~admit:(fun () -> true) |> ok);
              Ok
                (receipt
                   handle
                   (R.Command.Receipt.Live
                      (R.Command.Receipt.Live.Failed
                         (P.Error.create
                            P.Error.Invalid_state
                            ~message:"no effect entered"
                            ~retryable:false
                            ()))))))
          |> ok
          : L.View.t);
       ignore
         (Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
            L.await_terminal lifecycle ~actor ~run_id ~key:escaped_key |> ok)
          : L.View.t);
       assert (
         Result.is_error (L.Active.Started.consume_for (Option.value_exn !escaped) ~root));
       let cancel_key = P.Idempotency_key.of_string "command-live-cancel" |> ok in
       let ready, signal_ready = Eio.Promise.create () in
       ignore
         (L.with_slot lifecycle ~run_id ~f:(fun slot ->
            let open Result.Let_syntax in
            let%bind reserved =
              L.reserve
                lifecycle
                slot
                ~handle
                ~key:cancel_key
                ~kind:R.Command.Receipt.Kind.Live_run
                ~request_fingerprint
                ~admit:(fun () -> true)
            in
            L.launch lifecycle slot ~handle ~reserved ~f:(fun _active ~sw:_ ->
              Eio.Promise.resolve signal_ready ();
              Eio.Fiber.await_cancel ()))
          |> ok
          : L.View.t);
       Eio.Promise.await ready;
       let cancelled =
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
           L.cancel_and_wait lifecycle ~actor ~run_id ~key:cancel_key |> ok)
       in
       (match L.View.receipt cancelled with
        | Some value ->
          (match R.Command.Receipt.value value with
           | R.Command.Receipt.Live R.Command.Receipt.Live.Cancelled -> ()
           | Check _ | Fixture _ | Live _ | Task _ ->
             failwith "wrong cancellation receipt")
        | None -> failwith "cancellation did not settle");
       let retire_key = P.Idempotency_key.of_string "command-live-retire-run" |> ok in
       let retire_entered, signal_retire_entered = Eio.Promise.create () in
       let permit_self_probe, signal_permit_self_probe = Eio.Promise.create () in
       let self_probe, signal_self_probe = Eio.Promise.create () in
       let retire_slot = ref None in
       ignore
         (L.with_slot lifecycle ~run_id ~f:(fun slot ->
            let open Result.Let_syntax in
            let%bind reserved =
              L.reserve
                lifecycle
                slot
                ~handle
                ~key:retire_key
                ~kind:R.Command.Receipt.Kind.Live_run
                ~request_fingerprint
                ~admit:(fun () -> true)
            in
            L.launch lifecycle slot ~handle ~reserved ~f:(fun _active ~sw:_ ->
              Eio.Promise.resolve signal_retire_entered ();
              Eio.Promise.await permit_self_probe;
              let slot = Option.value_exn !retire_slot in
              let self_denied =
                match L.retire_run lifecycle slot ~actor with
                | Error { P.Error.code = Conflict; _ } -> true
                | Ok () | Error _ -> false
              in
              Eio.Promise.resolve signal_self_probe self_denied;
              Eio.Fiber.await_cancel ()))
          |> ok
          : L.View.t);
       Eio.Promise.await retire_entered;
       L.with_slot lifecycle ~run_id ~f:(fun slot ->
         retire_slot := Some slot;
         Eio.Promise.resolve signal_permit_self_probe ();
         assert (Eio.Promise.await self_probe);
         retire_control_allowed := false;
         assert (
           match L.check_retirement lifecycle slot ~actor with
           | Error { P.Error.code = Permission_denied; _ } -> true
           | Ok () | Error _ -> false);
         assert (
           match L.retire_run lifecycle slot ~actor with
           | Error { P.Error.code = Permission_denied; _ } -> true
           | Ok () | Error _ -> false);
         (match R.Command.observe handle ~key:retire_key |> ok with
          | Some observed ->
            (match R.Command.Row.phase (R.Command.View.row observed) with
             | R.Command.Row.Phase.Terminal _ ->
               failwith "denied retirement cancelled the old callback"
             | Reserved | Waiting_permission | Execution_started -> ())
          | None -> failwith "denied retirement lost the old command");
         retire_control_allowed := true;
         L.retire_run lifecycle slot ~actor)
       |> ok;
       let retired = L.await_terminal lifecycle ~actor ~run_id ~key:retire_key |> ok in
       (match L.View.receipt retired with
        | Some value ->
          (match R.Command.Receipt.value value with
           | R.Command.Receipt.Live R.Command.Receipt.Live.Cancelled -> ()
           | Check _ | Fixture _ | Live _ | Task _ ->
             failwith "retirement chose wrong receipt")
        | None -> failwith "retirement did not join and settle its callback");
       let uncertain_key =
         P.Idempotency_key.of_string "command-live-uncertain-start" |> ok
       in
       let native_constructions = ref 0 in
       ignore
         (L.with_slot lifecycle ~run_id ~f:(fun slot ->
            let open Result.Let_syntax in
            let%bind reserved =
              L.reserve
                lifecycle
                slot
                ~handle
                ~key:uncertain_key
                ~kind:R.Command.Receipt.Kind.Live_run
                ~request_fingerprint
                ~admit:(fun () -> true)
            in
            L.launch lifecycle slot ~handle ~reserved ~f:(fun active ~sw:_ ->
              rename_armed := true;
              (match L.Active.start_preparation active ~actor ~admit:(fun () -> true) with
               | Ok _ -> incr native_constructions
               | Error _ -> ());
              assert (Int.equal !native_constructions 0);
              sync_blocked := false;
              Ok
                (receipt
                   handle
                   (R.Command.Receipt.Live
                      (R.Command.Receipt.Live.Failed
                         (P.Error.create
                            P.Error.Invalid_state
                            ~message:"start uncertain; no runtime constructed"
                            ~retryable:false
                            ()))))))
          |> ok
          : L.View.t);
       assert (
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
           Result.is_error (L.await_terminal lifecycle ~actor ~run_id ~key:uncertain_key)));
       L.with_slot lifecycle ~run_id ~f:(fun slot -> L.retire_run lifecycle slot ~actor)
       |> ok;
       let settled = L.await_terminal lifecycle ~actor ~run_id ~key:uncertain_key |> ok in
       assert (Option.is_some (L.View.receipt settled));
       assert (Int.equal !native_constructions 0);
       L.close_and_wait lifecycle |> ok;
       Agent_server.Developer_root_owner.close_and_wait root |> ok;
       Agent_server.Developer_root_owner.Registry.close_and_wait registry |> ok;
       let inactive_owner = F.owner "dvr_command_inactive_daemon" 0 in
       let inactive_handle, _ =
         R.admit_epoch
           store
           ~owner:inactive_owner
           ~capture_fingerprint:(Chatml_developer_capture.fingerprint captured)
           ~previous:None
         |> ok
       in
       let inactive_calls = ref 0 in
       let inactive_owner_lifecycle = ref None in
       let exception Inactive_daemon in
       (match
          Eio.Switch.run (fun inactive_sw ->
            let inactive =
              L.create
                ~sw:inactive_sw
                ~store
                ~max_active_commands:1
                ~disclosure
                ~control
                ~recovery:(recovery ())
              |> ok
            in
            inactive_owner_lifecycle := Some inactive;
            L.with_slot inactive ~run_id:inactive_owner.run_id ~f:(fun slot ->
              let open Result.Let_syntax in
              let%bind () = L.ensure_startup_recovered inactive slot in
              let%bind reserved =
                L.reserve
                  inactive
                  slot
                  ~handle:inactive_handle
                  ~key:(P.Idempotency_key.of_string "inactive-launch" |> ok)
                  ~kind:R.Command.Receipt.Kind.Live_run
                  ~request_fingerprint
                  ~admit:(fun () -> true)
              in
              Eio.Switch.fail inactive_sw Inactive_daemon;
              assert (
                match
                  L.launch
                    inactive
                    slot
                    ~handle:inactive_handle
                    ~reserved
                    ~f:(fun _ ~sw:_ ->
                      incr inactive_calls;
                      Error (P.Error.invalid_request "unexpected launch"))
                with
                | exception Eio.Cancel.Cancelled Inactive_daemon -> true
                | Ok _ | Error _ | (exception _) -> false);
              Ok ())
            |> ok)
        with
        | () -> failwith "inactive daemon switch returned success"
        | exception Inactive_daemon -> ());
       Option.iter !inactive_owner_lifecycle ~f:(fun inactive ->
         L.close_and_wait inactive |> ok);
       assert (Int.equal !inactive_calls 0))
;;

let%test_unit "a started Live command transfers one claim while Check remains active" =
  F.with_store (fun _env sw store ->
    let disclosure, control = policy () in
    let recoveries = ref 0 in
    let recovery =
      L.Recovery_adapter.create ~recover:(fun ~claim:_ ~handle:_ ~window ->
        incr recoveries;
        let view = R.Recovery.observe window |> ok in
        assert (List.is_empty (R.Recovery.View.candidates view));
        assert (List.is_empty (R.Recovery.View.inference_candidates view));
        Ok ())
    in
    let lifecycle =
      L.create ~sw ~store ~max_active_commands:4 ~disclosure ~control ~recovery |> ok
    in
    let capabilities =
      Chat_response.Tool_capability.create
        ~owner:"command-handoff"
        ~resource_fingerprint:Digestif.SHA256.(digest_string "handoff" |> to_hex)
        []
      |> ok
    in
    let limits =
      Agent_server.Developer_root_owner.Limits.create
        ~max_active_runs:2
        ~max_staged_jobs:2
        ~max_native_occupancy:2
        ~max_active_workers:1
        ~max_quarantined_workers:2
        ~max_active_inference_attempts:8
        ~max_admitted_call_bindings:32
      |> F.or_error
    in
    let registry =
      Agent_server.Developer_root_owner.Registry.create
        ~sw
        ~store
        ~capacity:(F.capacity ())
        ~now:F.now
        ~limits
    in
    let actor = Operator_authorization.trusted_local F.principal in
    let captured = F.capture () in
    let native_preparations = ref 0 in
    let run_case ~suffix ~reject_after_transfer =
      let run_owner = F.owner ("dvr_command_handoff_" ^ suffix) 0 in
      let run_id = run_owner.run_id in
      let check_key = P.Idempotency_key.of_string ("handoff-check-" ^ suffix) |> ok in
      let live_key = P.Idempotency_key.of_string ("handoff-live-" ^ suffix) |> ok in
      let request_fingerprint =
        Chatml_source.Digest.of_contents ("handoff-" ^ suffix) ~checkpoint:ignore
      in
      let handle =
        L.with_slot lifecycle ~run_id ~f:(fun slot ->
          let open Result.Let_syntax in
          let%bind () = L.ensure_startup_recovered lifecycle slot in
          let handle, _ =
            R.admit_epoch
              store
              ~owner:run_owner
              ~capture_fingerprint:(Chatml_developer_capture.fingerprint captured)
              ~previous:None
            |> ok
          in
          let%map () = L.ensure_admitted_claim lifecycle slot ~handle in
          handle)
        |> ok
      in
      let await_stage ~key ~stage signal =
        match
          Eio.Fiber.first
            (fun () -> `Reached (Eio.Promise.await signal))
            (fun () -> `Terminal (L.await_terminal lifecycle ~actor ~run_id ~key))
        with
        | `Reached value -> value
        | `Terminal (Ok _ | Error _) ->
          failwith ("command callback settled before " ^ stage)
      in
      let check_entered, signal_check_entered = Eio.Promise.create () in
      let release_check, signal_release_check = Eio.Promise.create () in
      ignore
        (L.with_slot lifecycle ~run_id ~f:(fun slot ->
           let open Result.Let_syntax in
           let%bind reserved =
             L.reserve
               lifecycle
               slot
               ~handle
               ~key:check_key
               ~kind:R.Command.Receipt.Kind.Check
               ~request_fingerprint
               ~admit:(fun () -> true)
           in
           L.launch lifecycle slot ~handle ~reserved ~f:(fun _ ~sw:_ ->
             Eio.Promise.resolve signal_check_entered ();
             Eio.Promise.await release_check;
             Ok (receipt handle (R.Command.Receipt.Check R.Command.Receipt.Check.Ready))))
         |> ok
         : L.View.t);
      await_stage ~key:check_key ~stage:"Check entry" check_entered;
      let started, signal_started = Eio.Promise.create () in
      let allow_admission, signal_allow_admission = Eio.Promise.create () in
      let transferred, signal_transferred = Eio.Promise.create () in
      let release_admission, signal_release_admission = Eio.Promise.create () in
      let admission_returned, signal_admission_returned = Eio.Promise.create () in
      let release_live, signal_release_live = Eio.Promise.create () in
      ignore
        (L.with_slot lifecycle ~run_id ~f:(fun slot ->
           let open Result.Let_syntax in
           let%bind reserved =
             L.reserve
               lifecycle
               slot
               ~handle
               ~key:live_key
               ~kind:R.Command.Receipt.Kind.Live_run
               ~request_fingerprint
               ~admit:(fun () -> true)
           in
           L.launch lifecycle slot ~handle ~reserved ~f:(fun active ~sw:_ ->
             let preparing =
               L.Active.start_preparation active ~actor ~admit:(fun () -> true) |> ok
             in
             incr native_preparations;
             Eio.Promise.resolve signal_started ();
             Eio.Promise.await allow_admission;
             let final_admission ~actor:_ ~capture:_ ~checkpoint =
               checkpoint ();
               Eio.Promise.await transferred;
               Eio.Promise.await release_admission;
               if reject_after_transfer
               then Error (P.Error.invalid_request "controlled post-transfer rejection")
               else Ok (fun () -> Ok ())
             in
             let admitted =
               Agent_server.Developer_root_owner.Registry.admit_recovered
                 ~final_admission
                 ~checkpoint:(fun () -> Eio.Fiber.yield ())
                 registry
                 ~actor
                 ~capture:captured
                 ~registry:capabilities
                 ~selected:capabilities
                 ~selected_model:None
                 ~job_policy:Chat_response.One_off_request.default_policy
                 ~handle
                 ~workspace_conflict_domain:"command-handoff"
                 ~transfer_finished_claim:(fun ~accept ->
                   let result = L.Active.transfer_finished_claim active ~handle ~accept in
                   if Result.is_ok result
                   then (
                     assert (
                       Result.is_error
                         (L.Active.transfer_finished_claim
                            active
                            ~handle
                            ~accept:(fun ~claim:_ ~window:_ ->
                              failwith "repeated physical transfer accepted")));
                     Eio.Promise.resolve signal_transferred ());
                   result)
             in
             Eio.Promise.resolve signal_admission_returned admitted;
             if reject_after_transfer
             then (
               assert (Result.is_error admitted);
               Eio.Promise.await release_live;
               Ok
                 (receipt
                    handle
                    (R.Command.Receipt.Live
                       (R.Command.Receipt.Live.Failed
                          (P.Error.invalid_request "controlled post-transfer rejection")))))
             else (
               let root = admitted |> ok in
               let started = L.Active.Preparing.bind_root preparing ~root |> ok in
               L.Active.Started.consume_for started ~root |> ok;
               Eio.Promise.await release_live;
               Ok
                 (receipt
                    handle
                    (R.Command.Receipt.Live (R.Command.Receipt.Live.Succeeded `Null))))))
         |> ok
         : L.View.t);
      await_stage ~key:live_key ~stage:"durable Live Start" started;
      assert (Int.equal !native_preparations (if reject_after_transfer then 2 else 1));
      let live_row = R.Command.observe handle ~key:live_key |> ok |> Option.value_exn in
      (match R.Command.Row.phase (R.Command.View.row live_row) with
       | R.Command.Row.Phase.Execution_started -> ()
       | Reserved | Waiting_permission | Terminal _ ->
         failwith "Live did not durably Start before native preparation");
      let check_row = R.Command.observe handle ~key:check_key |> ok |> Option.value_exn in
      (match R.Command.Row.phase (R.Command.View.row check_row) with
       | R.Command.Row.Phase.Reserved -> ()
       | Waiting_permission | Execution_started | Terminal _ ->
         failwith "concurrent Check did not remain live");
      Eio.Promise.resolve signal_allow_admission ();
      await_stage ~key:live_key ~stage:"claim transfer" transferred;
      assert (
        List.is_empty
          (Agent_server.Developer_root_owner.Registry.active_for_scheduler registry |> ok));
      L.with_slot lifecycle ~run_id ~f:(fun slot ->
        L.ensure_admitted_claim lifecycle slot ~handle)
      |> ok;
      Eio.Promise.resolve signal_release_admission ();
      let admitted =
        await_stage ~key:live_key ~stage:"admission result" admission_returned
      in
      if reject_after_transfer
      then (
        assert (Result.is_error admitted);
        assert (
          List.is_empty
            (Agent_server.Developer_root_owner.Registry.active_for_scheduler registry
             |> ok));
        L.with_slot lifecycle ~run_id ~f:(fun slot ->
          L.ensure_admitted_claim lifecycle slot ~handle)
        |> ok;
        assert (
          Result.is_error
            (Agent_server.Developer_root_owner.Registry.admit_recovered
               registry
               ~actor
               ~capture:captured
               ~registry:capabilities
               ~selected:capabilities
               ~selected_model:None
               ~job_policy:Chat_response.One_off_request.default_policy
               ~handle
               ~workspace_conflict_domain:"command-handoff"
               ~transfer_finished_claim:(fun ~accept:_ ->
                 failwith "second startup transfer attempted"))))
      else
        assert (
          List.length
            (Agent_server.Developer_root_owner.Registry.active_for_scheduler registry
             |> ok)
          = 1);
      let check_row = R.Command.observe handle ~key:check_key |> ok |> Option.value_exn in
      (match R.Command.Row.phase (R.Command.View.row check_row) with
       | R.Command.Row.Phase.Reserved -> ()
       | Waiting_permission | Execution_started | Terminal _ ->
         failwith "Check terminalized before its held callback was released");
      Eio.Promise.resolve signal_release_live ();
      Eio.Promise.resolve signal_release_check ();
      let terminal ~label ~key =
        match L.await_terminal lifecycle ~actor ~run_id ~key with
        | Ok view -> view
        | Error error ->
          let phase =
            match R.Command.observe handle ~key |> ok with
            | None -> "missing"
            | Some observed ->
              (match R.Command.Row.phase (R.Command.View.row observed) with
               | Reserved -> "reserved"
               | Waiting_permission -> "waiting"
               | Execution_started -> "started"
               | Terminal { receipt; _ } ->
                 "terminal "
                 ^ Sexp.to_string_hum
                     (R.Command.Receipt.sexp_of_value (R.Command.Receipt.value receipt)))
          in
          failwith
            (label
             ^ " command terminal ("
             ^ phase
             ^ "): "
             ^ P.Error.code_to_string error.code
             ^ ": "
             ^ error.message)
      in
      let check = terminal ~label:"Check" ~key:check_key in
      let live = terminal ~label:"Live" ~key:live_key in
      (match L.View.receipt check, L.View.receipt live with
       | Some check, Some live ->
         (match R.Command.Receipt.value check, R.Command.Receipt.value live with
          | R.Command.Receipt.Check Ready, R.Command.Receipt.Live (Succeeded _)
            when not reject_after_transfer -> ()
          | R.Command.Receipt.Check Ready, R.Command.Receipt.Live (Failed _)
            when reject_after_transfer -> ()
          | _ -> failwith "concurrent Check or Live chose the wrong terminal")
       | _ -> failwith "concurrent command did not terminalize");
      if reject_after_transfer
      then (
        L.with_slot lifecycle ~run_id ~f:(fun slot ->
          assert (Result.is_error (L.confirm_transferred_retired lifecycle slot ~handle));
          L.ensure_admitted_claim lifecycle slot ~handle)
        |> ok;
        Agent_server.Developer_root_owner.Registry.retire_admission registry ~handle |> ok)
      else Agent_server.Developer_root_owner.close_and_wait (admitted |> ok) |> ok;
      L.with_slot lifecycle ~run_id ~f:(fun slot ->
        L.confirm_transferred_retired lifecycle slot ~handle)
      |> ok
    in
    run_case ~suffix:"success" ~reject_after_transfer:false;
    run_case ~suffix:"rejected" ~reject_after_transfer:true;
    assert (Int.equal !recoveries 2);
    assert (Int.equal !native_preparations 2);
    L.close_and_wait lifecycle |> ok;
    Agent_server.Developer_root_owner.Registry.close_and_wait registry |> ok)
;;

let%test_unit "retirement joins a body-exited command during durable terminal settlement" =
  let body_exited = ref false in
  let pause_used = ref false in
  let settling, signal_settling = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let checkpoint () =
    if !body_exited && not !pause_used
    then (
      pause_used := true;
      Eio.Promise.resolve signal_settling ();
      Eio.Promise.await release)
  in
  F.with_store ~checkpoint (fun env sw store ->
    let owner = F.owner "dvr_body_exited_retirement" 0 in
    let run_id = owner.run_id in
    let captured = F.capture () in
    let handle, _ =
      R.admit_epoch
        store
        ~owner
        ~capture_fingerprint:(Chatml_developer_capture.fingerprint captured)
        ~previous:None
      |> ok
    in
    let disclosure, control = policy () in
    let lifecycle =
      L.create
        ~sw
        ~store
        ~max_active_commands:2
        ~disclosure
        ~control
        ~recovery:(recovery ())
      |> ok
    in
    let actor = Operator_authorization.trusted_local F.principal in
    let key = P.Idempotency_key.of_string "body-exited-retirement" |> ok in
    let request_fingerprint =
      Chatml_source.Digest.of_contents "body-exited-retirement" ~checkpoint:ignore
    in
    L.with_slot lifecycle ~run_id ~f:(fun slot ->
      let open Result.Let_syntax in
      let%bind () = L.ensure_startup_recovered lifecycle slot in
      let%bind reserved =
        L.reserve
          lifecycle
          slot
          ~handle
          ~key
          ~kind:R.Command.Receipt.Kind.Check
          ~request_fingerprint
          ~admit:(fun () -> true)
      in
      L.launch lifecycle slot ~handle ~reserved ~f:(fun _active ~sw:_ ->
        let value = receipt handle (R.Command.Receipt.Check Ready) in
        body_exited := true;
        Ok value))
    |> ok
    |> ignore;
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Promise.await settling);
    let retired, signal_retired = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve
        signal_retired
        (Result.try_with (fun () ->
           L.with_slot lifecycle ~run_id ~f:(fun slot ->
             L.retire_run lifecycle slot ~actor))));
    Exn.protect
      ~finally:(fun () ->
        if not (Eio.Promise.is_resolved release)
        then Eio.Promise.resolve signal_release ())
      ~f:(fun () ->
        Eio.Fiber.yield ();
        assert (not (Eio.Promise.is_resolved retired));
        Eio.Promise.resolve signal_release ();
        match
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
            Eio.Promise.await retired)
        with
        | Ok (Ok ()) -> ()
        | Ok (Error _) | Error _ -> failwith "retirement did not join settlement");
    let terminal = L.await_terminal lifecycle ~actor ~run_id ~key |> ok in
    (match Option.map (L.View.receipt terminal) ~f:R.Command.Receipt.value with
     | Some (Check Ready) -> ()
     | None | Some (Check _ | Fixture _ | Live _ | Task _) ->
       failwith "late retirement changed the settled Check result");
    L.close_and_wait lifecycle |> ok)
;;
