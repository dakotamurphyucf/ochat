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
       let run_id = run_owner.run_id in
       let key = P.Idempotency_key.of_string "command-lifecycle-check" |> ok in
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
             Ok launched
           in
           reserved)
         |> ok
       in
       Eio.Promise.await entered;
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
           | Check (Rejected _ | Interrupted | Cancelled) | Fixture _ | Live _ ->
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
       L.close_and_wait lifecycle |> ok)
;;

let%test_unit
    "live Start is one-use under the real Root and cancellation joins one callback"
  =
  F.with_store (fun env sw store ->
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
    let run_id = run_owner.run_id in
    L.with_slot lifecycle ~run_id ~f:(L.ensure_startup_recovered lifecycle) |> ok;
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
           let started = L.Active.start_live active ~root ~admit:(fun () -> true) |> ok in
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
           escaped := Some (L.Active.start_live active ~root ~admit:(fun () -> true) |> ok);
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
        | Check _ | Fixture _ | Live _ -> failwith "wrong cancellation receipt")
     | None -> failwith "cancellation did not settle");
    L.close_and_wait lifecycle |> ok;
    Agent_server.Developer_root_owner.close_and_wait root |> ok;
    Agent_server.Developer_root_owner.Registry.close_and_wait registry |> ok)
;;
