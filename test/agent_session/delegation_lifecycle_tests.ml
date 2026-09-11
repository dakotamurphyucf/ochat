open Core
open Fixtures
module P = Agent_protocol
module D = Agent_store.Delegation_store
module S = Agent_store.Session_store
module A = Agent_session.Session_actor
module State = Agent_session.Session_state
module Memory = Agent_session.Memory_backend
module Owner = Agent_server.Runtime_owner
module Lifecycle = Agent_server.Delegation_lifecycle

let digest = Chatmd_shell_spec.Source_ref.digest

let actor ~env ~sw ~state ~reject_save =
  let backend = Memory.create ~event_capacity:128 ~initial_state:state in
  let persistence = Memory.persistence backend in
  let actor =
    A.create
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~mailbox_capacity:32
      ~compaction_env:None
      ~initial_state:state
      ~operation_worker:None
      ~persistence:
        { commit =
            (fun ~command_audit ~previous next ->
              match reject_save next with
              | true -> Error (handoff_error "injected child stop save failure")
              | false -> persistence.commit ~command_audit ~previous next)
        }
      ~services:
        { now = (fun () -> timestamp)
        ; monotonic_now = (fun () -> Mtime.min_stamp)
        ; create_attachment_id = P.Id.Attachment.create
        ; create_reclaim_token = (fun () -> "delegated-lifecycle")
        ; job_results = None
        ; schedule_limits = Agent_session.Staged_schedules.default_limits
        ; notification_limits = Agent_session.Staged_notifications.default_limits
        ; ingress_limits = Agent_session.Staged_ingress.default_limits
        ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
        ; state_committed = (fun _ _ -> ())
        }
  in
  actor, backend
;;

let with_fixture ?(lifetime = D.Admission.Owned) f =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let store =
        S.create
          ~env
          ~sw
          ~root:(Filename.concat workspace_instance.canonical_root.native_path "store")
          ~server_id:(P.Id.Server.create ())
          ~process_start_identity:None
          ~lock_nonce:"delegation-lifecycle"
        |> store_ok
      in
      Exn.protect
        ~finally:(fun () -> S.close store |> store_ok)
        ~f:(fun () ->
          let ledger = S.delegations store in
          let reserve child_id =
            D.reserve
              ledger
              ~key:
                { parent_session_id = second_session_id
                ; parent_generation = 0
                ; principal_id
                ; idempotency_key =
                    P.Idempotency_key.of_string (P.Id.Session.to_string child_id)
                    |> protocol_ok
                }
              ~request_sha256:(digest "child creation")
              ~admission:
                { child_session_id = child_id
                ; revision_id = P.Id.Prompt_revision.create ()
                ; transaction_id = P.Id.Transaction.create ()
                ; manifest_sha256 = digest "child manifest"
                ; parent_revision_id = prompt_revision_id
                ; authority_sha256 = digest "parent authority"
                ; capability_pins = []
                ; lifetime
                ; created_at = timestamp
                }
              ~max_records:8
              ~max_bytes:1048576
            |> store_ok
            |> function
            | D.New record -> record
            | _ -> assert false
          in
          let record = reserve session_id in
          let foreign = reserve third_session_id in
          let reference = D.reference record in
          let original =
            actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
          in
          let state =
            { original with
              spec =
                { original.spec with
                  delegation = Some reference
                ; prompt_revision_id = record.admission.revision_id
                ; protocol =
                    { original.spec.protocol with
                      prompt = Generated record.admission.revision_id
                    }
                }
            ; lifecycle = { desired = Running; observed = Idle }
            }
          in
          State.validate state |> protocol_ok;
          let reject = ref false in
          let child, backend =
            actor ~env ~sw ~state ~reject_save:(fun transition ->
              !reject
              &&
              match
                transition.Agent_session.Session_transition.state.lifecycle.desired
              with
              | Stopped -> true
              | Running -> false)
          in
          let closes = ref 0 in
          let runtime =
            Owner.create
              ~actor:child
              ~initial:
                (Some (Runtime_lease_tests.runtime ~close:(fun () -> Int.incr closes) ()))
              ~build:(fun () -> failwith "unexpected child reload")
          in
          Exn.protect
            ~finally:(fun () ->
              Owner.close_and_wait runtime;
              A.shutdown child)
            ~f:(fun () ->
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                f
                  env
                  sw
                  ledger
                  record
                  foreign
                  child
                  runtime
                  backend
                  closes
                  reject
                  original)))))
;;

let%expect_test
    "private owned stop rejects foreign and independent relationships and preserves \
     failed saves"
  =
  List.iter [ `Owned; `Independent ] ~f:(fun mode ->
    let lifetime =
      match mode with
      | `Owned -> D.Admission.Owned
      | `Independent ->
        Independent { authorization_sha256 = digest "explicit independent lifetime" }
    in
    with_fixture
      ~lifetime
      (fun env _sw ledger record foreign child runtime backend closes reject _ ->
         let stop reference =
           Lifecycle.stop_owned
             ~clock:(Eio.Stdenv.clock env)
             ~delegations:ledger
             ~reference
             ~actor:child
             ~runtime
         in
         let unchanged () =
           assert_same_session_snapshot
             (A.state child |> protocol_ok)
             (Memory.state backend)
         in
         let before = A.state child |> protocol_ok in
         (match A.stop_delegated child ~reference:(D.reference foreign) ~mode:Cancel with
          | Error { code = Permission_denied; _ } -> ()
          | _ -> failwith "foreign reference stopped the child");
         assert_same_session_snapshot before (A.state child |> protocol_ok);
         match mode with
         | `Independent ->
           (match stop (D.reference record) with
            | Error { code = Permission_denied; _ } -> ()
            | _ -> failwith "owned stop reached independent child");
           assert_same_session_snapshot before (A.state child |> protocol_ok);
           [%test_eq: int] 0 !closes;
           print_endline "foreign and independent relationships leave the child unchanged"
         | `Owned ->
           reject := true;
           assert (Result.is_error (stop (D.reference record)));
           assert_same_session_snapshot before (A.state child |> protocol_ok);
           [%test_eq: int] 0 !closes;
           assert (Owner.is_loaded runtime);
           reject := false;
           ignore (D.revoke ledger record Parent_stopped |> store_ok : D.record);
           let stopped = stop (D.reference record) |> protocol_ok in
           assert (P.Session.equal_desired_state stopped.desired_state Stopped);
           let saved = A.state child |> protocol_ok in
           ignore (stop (D.reference record) |> protocol_ok : P.Session.t);
           assert_same_session_snapshot saved (A.state child |> protocol_ok);
           unchanged ();
           [%test_eq: int] 1 !closes;
           assert (List.is_empty saved.attachments);
           print_endline
             "failed save retains resources; revoked unlinked child stops without \
              attachment; retry is idempotent"));
  [%expect
    {|
    failed save retains resources; revoked unlinked child stops without attachment; retry is idempotent
    foreign and independent relationships leave the child unchanged
    |}]
;;

let%expect_test
    "parent runtime lease joins child invocation cleanup before either runtime retires"
  =
  with_fixture
    (fun env sw ledger record _ child runtime backend child_closes _ original ->
       let parent_state =
         { original with
           identity = { original.identity with session_id = second_session_id }
         ; lifecycle = { desired = Running; observed = Idle }
         }
       in
       let parent, _ = actor ~env ~sw ~state:parent_state ~reject_save:(fun _ -> false) in
       let entered, enter = Eio.Promise.create () in
       let cleaning, clean = Eio.Promise.create () in
       let release, release_cleanup = Eio.Promise.create () in
       let never, _ = Eio.Promise.create () in
       let cleaned = ref false in
       let parent_closes = ref 0 in
       let parent_runtime =
         Owner.create
           ~actor:parent
           ~initial:
             (Some
                (Runtime_lease_tests.runtime
                   ~close:(fun () ->
                     assert !cleaned;
                     [%test_eq: int] 1 !child_closes;
                     Int.incr parent_closes)
                   ()))
           ~build:(fun () -> failwith "unexpected parent reload")
       in
       Exn.protect
         ~finally:(fun () ->
           Owner.close_and_wait parent_runtime;
           A.shutdown parent)
         ~f:(fun () ->
           let worker =
             Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
               let invocation = invocation_fixture () in
               let _ =
                 caps.with_invocation ~invocation (fun ~dispatched:_ ->
                   Eio.Promise.resolve enter ();
                   Exn.protect
                     ~finally:(fun () ->
                       Eio.Cancel.protect (fun () ->
                         Eio.Promise.resolve clean ();
                         Eio.Promise.await release;
                         cleaned := true))
                     ~f:(fun () ->
                       Eio.Promise.await never;
                       Ok (P.Invocation.Complete `Null)))
                 |> protocol_ok
               in
               match completed_worker_result input caps with
               | Ok result -> Agent_session.Operation_worker.Completed result
               | Error error -> Failed error)
           in
           A.set_operation_worker child (Some worker) |> protocol_ok;
           let writer, _ =
             A.attach child ~mode:Read_write ~subscribe:false |> protocol_ok
           in
           let message =
             Agent_session.History_codec.user_text ~id:history_id "retained child input"
             |> Agent_session.History_codec.to_protocol
           in
           A.submit_message child ~attachment_id:writer.id message
           |> protocol_ok
           |> ignore;
           A.detach child writer.id |> protocol_ok;
           Eio.Promise.await entered;
           let leased, lease_ready = Eio.Promise.create () in
           let propagating, propagate = Eio.Promise.create () in
           let dependency =
             Eio.Fiber.fork_promise ~sw (fun () ->
               Result.try_with (fun () ->
                 Owner.with_background_runtime parent_runtime (fun _ ->
                   Eio.Promise.resolve lease_ready ();
                   Exn.protect
                     ~finally:(fun () ->
                       Eio.Promise.resolve propagate ();
                       Lifecycle.stop_owned
                         ~clock:(Eio.Stdenv.clock env)
                         ~delegations:ledger
                         ~reference:(D.reference record)
                         ~actor:child
                         ~runtime
                       |> protocol_ok
                       |> ignore)
                     ~f:(fun () ->
                       Eio.Promise.await never;
                       Ok ()))))
           in
           Eio.Promise.await leased;
           let ledger_locked, lock_ready = Eio.Promise.create () in
           let release_ledger, release_ledger_u = Eio.Promise.create () in
           let held_ledger =
             Eio.Fiber.fork_promise ~sw (fun () ->
               D.with_records ledger ~max_records:8 ~max_bytes:1048576 ~f:(fun _ ->
                 Eio.Promise.resolve lock_ready ();
                 Eio.Promise.await release_ledger;
                 Ok ())
               |> store_ok)
           in
           Eio.Promise.await ledger_locked;
           let parent_writer, _ =
             A.attach parent ~mode:Read_write ~subscribe:false |> protocol_ok
           in
           A.stop parent ~attachment_id:parent_writer.id ~mode:Cancel
           |> protocol_ok
           |> ignore;
           let caller_context, caller_context_u = Eio.Promise.create () in
           let stopping =
             Eio.Fiber.fork_promise ~sw (fun () ->
               Result.try_with (fun () ->
                 Eio.Cancel.sub (fun context ->
                   Eio.Promise.resolve caller_context_u context;
                   Owner.unload_and_wait parent_runtime)))
           in
           Eio.Promise.await propagating;
           [%test_eq: int] 0 !parent_closes;
           Eio.Promise.resolve release_ledger_u ();
           Eio.Promise.await_exn held_ledger;
           Eio.Promise.await cleaning;
           Eio.Cancel.cancel (Eio.Promise.await caller_context) Exit;
           [%test_eq: int] 0 !parent_closes;
           [%test_eq: int] 0 !child_closes;
           assert (Option.is_some (A.state child |> protocol_ok).active_operation);
           assert (
             Option.is_none
               (A.with_quiescent_state child ~f:(fun _ -> Ok ()) |> protocol_ok));
           Eio.Promise.resolve release_cleanup ();
           (match Eio.Promise.await_exn dependency with
            | Error (Eio.Cancel.Cancelled _) -> ()
            | _ -> failwith "dependency cancellation lost");
           (match Eio.Promise.await_exn stopping with
            | Ok (Ok ()) | Error (Eio.Cancel.Cancelled _) -> ()
            | _ -> failwith "unexpected parent stop outcome");
           [%test_eq: int] 1 !parent_closes;
           [%test_eq: int] 1 !child_closes;
           let state = A.state child |> protocol_ok in
           assert_same_session_snapshot state (Memory.state backend);
           assert (List.is_empty state.attachments);
           assert (Option.is_none state.active_operation);
           assert (
             List.exists state.conversation.canonical_history ~f:(fun entry ->
               P.History.Id.equal entry.id message.id));
           (match state.invocations with
            | [ { status = Resolved (Cancelled _); _ } ] -> ()
            | invocations ->
              raise_s
                [%sexp
                  "expected one cancelled child invocation"
                , (invocations : P.Invocation.t list)]);
           print_endline
             "cancelled stop caller cannot release parent resources before child \
              cleanup; history remains readable"));
  [%expect
    {| cancelled stop caller cannot release parent resources before child cleanup; history remains readable |}]
;;
