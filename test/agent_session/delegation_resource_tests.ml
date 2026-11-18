open Core
open Fixtures
module Owner = Agent_server.Runtime_owner

let%expect_test "stopped ancestor resources have a lifetime without loading execution" =
  Job_fixtures.with_actor (fun _env sw actor writer _backend ->
    Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
    |> protocol_ok
    |> ignore;
    let ready, ready_u = Eio.Promise.create () in
    let cleaning, cleaning_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let cleaned = ref false in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        failwith "stopped ancestor execution must not load")
    in
    let before = Agent_session.Session_actor.state actor |> protocol_ok in
    assert (Agent_protocol.Session.equal_desired_state before.lifecycle.desired Stopped);
    let borrowed =
      Eio.Fiber.fork_promise ~sw (fun () ->
        Result.try_with (fun () ->
          Owner.with_resource_lifetime owner (fun () ->
            Exn.protect
              ~finally:(fun () ->
                Eio.Cancel.protect (fun () ->
                  Eio.Promise.resolve cleaning_u ();
                  Eio.Promise.await release;
                  cleaned := true))
              ~f:(fun () ->
                Eio.Promise.resolve ready_u ();
                Eio.Promise.await never;
                Ok ()))))
    in
    Eio.Promise.await ready;
    Owner.unload_and_wait owner |> protocol_ok;
    assert (not (Owner.is_loaded owner));
    assert (not !cleaned);
    assert (
      Option.is_none
        (Owner.with_unloaded owner (fun () -> failwith "borrowed resources pruned")
         |> protocol_ok));
    (match Owner.unload owner with
     | Error { code = Conflict; _ } -> ()
     | _ -> failwith "stopped resource lifetime did not exclude reset");
    let after = Agent_session.Session_actor.state actor |> protocol_ok in
    [%test_eq: Sexp.t]
      (Agent_session.Session_state.sexp_of_t before)
      (Agent_session.Session_state.sexp_of_t after);
    let closing = Eio.Fiber.fork_promise ~sw (fun () -> Owner.close_and_wait owner) in
    Eio.Promise.await cleaning;
    assert (Option.is_none (Eio.Promise.peek closing));
    Eio.Promise.resolve release_u ();
    (match Eio.Promise.await_exn borrowed with
     | Error (Eio.Cancel.Cancelled _) -> ()
     | _ -> failwith "permanent close failed to cancel independent resources");
    Eio.Promise.await_exn closing;
    assert !cleaned;
    print_endline
      "no runtime load or actor mutation; stop preserves resources; maintenance excludes \
       them; close joins cleanup");
  [%expect
    {| no runtime load or actor mutation; stop preserves resources; maintenance excludes them; close joins cleanup |}]
;;

let%expect_test
    "resource borrowers survive ordinary stop and release the exact retired runtime"
  =
  Job_fixtures.with_actor (fun _env sw actor _writer _backend ->
    let closed = ref [] in
    let build_number = ref 0 in
    let build () =
      Int.incr build_number;
      let generation = !build_number in
      Ok
        (Runtime_lease_tests.runtime
           ~close:(fun () -> closed := generation :: !closed)
           ())
    in
    let owner = Owner.create ~actor ~initial:None ~build in
    Exn.protect
      ~finally:(fun () -> Owner.close_and_wait owner)
      ~f:(fun () ->
        let borrow () =
          let ready, ready_u = Eio.Promise.create () in
          let release, release_u = Eio.Promise.create () in
          let pending =
            Eio.Fiber.fork_promise ~sw (fun () ->
              Owner.with_delegation_resources owner (fun runtime ->
                Eio.Promise.resolve ready_u runtime;
                Eio.Promise.await release;
                assert (List.is_empty !closed);
                Ok ()))
          in
          Eio.Promise.await ready, release_u, pending
        in
        let first, release_first, first_done = borrow () in
        let second, release_second, second_done = borrow () in
        assert (phys_equal first second);
        (match Owner.unload owner with
         | Error { code = Conflict; _ } -> ()
         | _ -> failwith "reset-style unload ignored retained resources");
        let executing, executing_u = Eio.Promise.create () in
        let never, _ = Eio.Promise.create () in
        let execution_cleaned = ref false in
        let execution =
          Eio.Fiber.fork_promise ~sw (fun () ->
            Result.try_with (fun () ->
              Owner.with_background_runtime owner (fun _ ->
                Exn.protect
                  ~finally:(fun () -> execution_cleaned := true)
                  ~f:(fun () ->
                    Eio.Promise.resolve executing_u ();
                    Eio.Promise.await never;
                    Ok ()))))
        in
        Eio.Promise.await executing;
        Owner.unload_and_wait owner |> protocol_ok;
        (match Eio.Promise.await_exn execution with
         | Error (Eio.Cancel.Cancelled _) -> ()
         | _ -> failwith "ordinary stop failed to cancel execution");
        assert !execution_cleaned;
        assert (not (Owner.is_loaded owner));
        assert (List.is_empty !closed);
        assert (
          Option.is_none
            (Owner.with_unloaded owner (fun () -> failwith "borrowed resource pruned")
             |> protocol_ok));
        (match Owner.with_administration owner (fun () -> failwith "borrowed reset") with
         | Error { code = Conflict; _ } -> ()
         | _ -> failwith "administration ignored retained resources");
        Owner.with_background_runtime owner (fun fresh ->
          assert (not (phys_equal first fresh));
          Ok ())
        |> protocol_ok;
        [%test_eq: int] 2 !build_number;
        Eio.Promise.resolve release_first ();
        Eio.Promise.await_exn first_done |> protocol_ok;
        assert (List.is_empty !closed);
        Eio.Promise.resolve release_second ();
        Eio.Promise.await_exn second_done |> protocol_ok;
        [%test_eq: int list] [ 1 ] !closed;
        assert (Owner.is_loaded owner);
        Owner.unload_and_wait owner |> protocol_ok;
        [%test_eq: int list] [ 2; 1 ] !closed;
        assert (Option.is_some (Owner.with_unloaded owner (fun () -> Ok ()) |> protocol_ok));
        print_endline
          "two borrowers retained old resources; parent loaded fresh runtime; final \
           borrower closed only the old runtime; maintenance resumed"));
  [%expect
    {| two borrowers retained old resources; parent loaded fresh runtime; final borrower closed only the old runtime; maintenance resumed |}]
;;

let%expect_test "deferred resource close failure releases ownership without poisoning it" =
  Job_fixtures.with_actor (fun _env sw actor _writer _backend ->
    let ready, ready_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let old_closes = ref 0 in
    let fresh_closes = ref 0 in
    let builds = ref 0 in
    let old_succeeds = ref false in
    let owner =
      Owner.create
        ~actor
        ~initial:
          (Some
             (Runtime_lease_tests.runtime
                ~close:(fun () ->
                  Int.incr old_closes;
                  if not !old_succeeds then failwith "deferred resource close failed")
                ()))
        ~build:(fun () ->
          Int.incr builds;
          Ok (Runtime_lease_tests.runtime ~close:(fun () -> Int.incr fresh_closes) ()))
    in
    let borrower =
      Eio.Fiber.fork_promise ~sw (fun () ->
        Result.try_with (fun () ->
          Owner.with_delegation_resources owner (fun _ ->
            Eio.Promise.resolve ready_u ();
            Eio.Promise.await release;
            Ok ())))
    in
    Eio.Promise.await ready;
    Owner.unload_and_wait owner |> protocol_ok;
    Owner.ensure_loaded owner |> protocol_ok;
    let current =
      Owner.with_loaded_runtime owner (fun runtime -> Ok runtime) |> protocol_ok
    in
    Eio.Promise.resolve release_u ();
    (match Eio.Promise.await_exn borrower with
     | Error (Failure message) ->
       [%test_eq: string] "deferred resource close failed" message
     | _ -> failwith "deferred close failure was lost");
    (* The separate current runtime was admitted while the old resource was
       still borrowed. Its unchanged execution checks remain authoritative. *)
    Owner.with_background_runtime owner (fun runtime ->
      assert (phys_equal current runtime);
      Ok ())
    |> protocol_ok;
    [%test_eq: int] 1 !builds;
    [%test_eq: int] 0 !fresh_closes;
    (match Result.try_with (fun () -> Owner.unload_and_wait owner |> protocol_ok) with
     | Error (Failure message) ->
       [%test_eq: string] "deferred resource close failed" message
     | Error exn -> raise exn
     | Ok () -> failwith "old failed resource was forgotten");
    [%test_eq: int] 1 !fresh_closes;
    [%test_eq: int] 2 !old_closes;
    assert (not (Owner.is_loaded owner));
    (match Owner.ensure_loaded owner with
     | Error { code = Conflict; retryable = true; _ } -> ()
     | Ok () | Error _ -> failwith "pending cleanup admitted a replacement runtime");
    [%test_eq: int] 1 !builds;
    old_succeeds := true;
    Owner.unload_and_wait owner |> protocol_ok;
    Owner.ensure_loaded owner |> protocol_ok;
    [%test_eq: int] 2 !builds;
    Owner.close_and_wait owner;
    [%test_eq: int] 3 !old_closes;
    [%test_eq: int] 2 !fresh_closes;
    print_endline "last borrower reports close failure; fresh owner remains usable");
  [%expect {| last borrower reports close failure; fresh owner remains usable |}]
;;

let%expect_test
    "permanent close joins resource cleanup even when overlapping ordinary stop"
  =
  List.iter [ false; true ] ~f:(fun overlap ->
    Job_fixtures.with_actor (fun _env sw actor _writer _backend ->
      let ready, ready_u = Eio.Promise.create () in
      let barrier, barrier_u = Eio.Promise.create () in
      let pass_barrier, pass_barrier_u = Eio.Promise.create () in
      let cleaning, cleaning_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let cleaned = ref false in
      let closes = ref 0 in
      let owner =
        Owner.create_with_unload
          ~actor
          ~initial:
            (Some
               (Runtime_lease_tests.runtime
                  ~close:(fun () ->
                    assert !cleaned;
                    Int.incr closes)
                  ()))
          ~build:(fun () -> failwith "unexpected rebuild")
          ~before_unload:(fun ~closing ->
            if overlap && not closing
            then (
              Eio.Promise.resolve barrier_u ();
              Eio.Promise.await pass_barrier);
            Ok ())
      in
      let borrower =
        Eio.Fiber.fork_promise ~sw (fun () ->
          Result.try_with (fun () ->
            Owner.with_delegation_resources owner (fun _ ->
              Exn.protect
                ~finally:(fun () ->
                  Eio.Cancel.protect (fun () ->
                    Eio.Promise.resolve cleaning_u ();
                    Eio.Promise.await release;
                    cleaned := true))
                ~f:(fun () ->
                  Eio.Promise.resolve ready_u ();
                  Eio.Promise.await never;
                  Ok ()))))
      in
      Eio.Promise.await ready;
      let stopping =
        match overlap with
        | false -> None
        | true ->
          let stopping =
            Eio.Fiber.fork_promise ~sw (fun () -> Owner.unload_and_wait owner)
          in
          Eio.Promise.await barrier;
          Some stopping
      in
      let closing = Eio.Fiber.fork_promise ~sw (fun () -> Owner.close_and_wait owner) in
      Eio.Fiber.yield ();
      (match Owner.ensure_loaded owner with
       | Error { code = Server_shutting_down; _ } -> ()
       | _ -> failwith "closing admitted a resource load");
      if overlap then Eio.Promise.resolve pass_barrier_u ();
      Eio.Promise.await cleaning;
      [%test_eq: int] 0 !closes;
      assert (Option.is_none (Eio.Promise.peek closing));
      Eio.Promise.resolve release_u ();
      (match Eio.Promise.await_exn borrower with
       | Error (Eio.Cancel.Cancelled _) -> ()
       | _ -> failwith "permanent close did not cancel resource borrower");
      Option.iter stopping ~f:(fun result -> Eio.Promise.await_exn result |> protocol_ok);
      Eio.Promise.await_exn closing;
      Owner.close_and_wait owner;
      [%test_eq: int] 1 !closes;
      print_s [%sexp (overlap : bool), "close joined borrower cleanup and retired once"]));
  [%expect
    {|
    (false "close joined borrower cleanup and retired once")
    (true "close joined borrower cleanup and retired once") |}]
;;

let%expect_test
    "independent graph joins every resource and preserves construction error over \
     teardown"
  =
  let module A = Agent_session.Session_actor in
  let module P = Agent_protocol in
  let module D = Agent_store.Delegation_store in
  let module Store = Agent_store.Session_store in
  let module G = Agent_server.Graph_tracking in
  let module L = Agent_session.Inference_ledger in
  let module R = Agent_server.Independent_resources in
  let digest = Chatmd_shell_spec.Source_ref.digest in
  Job_fixtures.with_actor (fun env sw actor writer _ ->
    A.stop actor ~attachment_id:writer.id ~mode:Cancel |> protocol_ok |> ignore;
    let state = A.state actor |> protocol_ok in
    let store =
      Store.create
        ~env
        ~sw
        ~root:
          (Filename.concat
             state.spec.workspace_instance.canonical_root.native_path
             "tracking")
        ~server_id:(P.Id.Server.create ())
        ~process_start_identity:None
        ~lock_nonce:"independent-tracking"
      |> store_ok
    in
    let owner =
      Owner.create ~actor ~initial:None ~build:(fun () ->
        failwith "unexpected activation")
    in
    Exn.protect
      ~finally:(fun () ->
        Owner.close_and_wait owner;
        Store.close store |> store_ok)
      ~f:(fun () ->
        let context, _, prepared = Inference_tracking_tests.prepared () in
        let admission : D.Admission.t =
          { child_session_id = P.Id.Session.create ()
          ; revision_id = P.Id.Prompt_revision.create ()
          ; transaction_id = P.Id.Transaction.create ()
          ; manifest_sha256 = digest "independent child"
          ; parent_revision_id = state.spec.prompt_revision_id
          ; parent_stop_epoch = Some state.stop_epoch
          ; authority_sha256 =
              Agent_session.Delegation_authority.fingerprint state |> protocol_ok
          ; authored_tool = None
          ; goal_origin = None
          ; captured_recurrence = None
          ; capability_pins = []
          ; lifetime = Independent { authorization_sha256 = digest "fixture grant" }
          ; created_at = timestamp
          ; inference_target = Some (Inference_runtime.Context.target context)
          }
        in
        let ledger = Store.delegations store in
        let record =
          D.reserve
            ledger
            ~key:
              { parent_session_id = state.identity.session_id
              ; parent_generation = state.identity.generation
              ; principal_id
              ; idempotency_key =
                  P.Idempotency_key.of_string "tracking-construction" |> protocol_ok
              }
            ~request_sha256:(digest "tracking-construction")
            ~admission
            ~max_records:8
            ~max_bytes:1048576
          |> store_ok
          |> function
          | D.New record -> record
          | _ -> assert false
        in
        let record =
          List.fold
            [ D.Artifact_installed; Child_installed; Linked ]
            ~init:record
            ~f:(fun record stage -> D.advance ledger record stage |> store_ok)
        in
        let released = ref false in
        let construction_cleanup = R.Cleanup.create () in
        let cleanup = R.Cleanup.graph construction_cleanup in
        let failure =
          P.Error.invalid_request "original independent construction failure"
        in
        let host : R.host =
          { find = (fun _ -> Ok { state; owner })
          ; resolve =
              (fun reference ->
                D.resolve ledger reference |> Result.map_error ~f:(fun _ -> assert false))
          ; authorize = (fun _ -> Ok ())
          ; build_root =
              (fun ~sw ~register_tracking _ ->
                let graph =
                  G.create
                    actor
                    ~source:(Inference_tracking_tests.source "independent-failed-root")
                    ~upstream:
                      (Inference_tracking_tests.upstream (fun ~scope:_ ~accounting_id:_ ->
                         ()))
                  |> protocol_ok
                in
                register_tracking graph;
                Eio.Switch.on_release sw (fun () ->
                  released := true;
                  raise Exit);
                (G.identity graph).with_attempt
                  prepared
                  ~relation:Root
                  ~f:(fun ~scope:_ ~accounting_id:_ ~submission_guard:_ -> Error failure))
          ; build_generated = (fun ~sw:_ ~parent:_ _ -> failwith "unexpected descendant")
          }
        in
        let module State = Agent_session.Session_state in
        let module Authority = Agent_session.Delegation_authority in
        let parent_id = state.identity.session_id in
        let child_id = record.admission.child_session_id in
        let child_revision = record.admission.revision_id in
        Job_fixtures.with_actor
          ~environment:(env, state.spec.workspace_instance)
          ~start:false
          ~prepare_state:(fun template ->
            State.create
              ~identity:{ template.identity with session_id = child_id }
              ~spec:
                { template.spec with
                  delegation = Some (D.reference record)
                ; prompt_revision_id = child_revision
                ; protocol =
                    { template.spec.protocol with prompt = Generated child_revision }
                }
              ~initial_history:[])
          (fun _ _ child_actor _ _ ->
             let child_owner =
               Owner.create ~actor:child_actor ~initial:None ~build:(fun () ->
                 failwith "shared ancestry check activated the child")
             in
             Exn.protect
               ~finally:(fun () -> Owner.close_and_wait child_owner)
               ~f:(fun () ->
                 let child_state = A.state child_actor |> protocol_ok in
                 let authorizations = ref 0 in
                 let host =
                   { host with
                     find =
                       (fun id ->
                         if P.Id.Session.equal id child_id
                         then Ok { R.state = child_state; owner = child_owner }
                         else host.find id)
                   ; authorize =
                       (fun actual ->
                         assert (
                           D.Reference.equal (D.reference actual) (D.reference record));
                         Int.incr authorizations;
                         host.authorize actual)
                   ; build_root =
                       (fun ~sw:_ ~register_tracking:_ _ ->
                         failwith "shared ancestry check constructed root resources")
                   }
                 in
                 R.check_parent ~max_depth:2 ~host ~parent_id:child_id |> protocol_ok;
                 let source_fingerprint (ancestor : R.ancestor) =
                   if P.Id.Session.equal ancestor.state.identity.session_id child_id
                   then
                     assert (
                       phys_equal ancestor.state child_state
                       && phys_equal ancestor.owner child_owner)
                   else
                     assert (
                       phys_equal ancestor.state state && phys_equal ancestor.owner owner);
                   Authority.fingerprint ancestor.state
                 in
                 let check =
                   R.check_parent_with_source ~max_depth:2 ~parent_id:child_id
                 in
                 check ~host ~source_fingerprint |> protocol_ok;
                 let denied = { host with authorize = (fun _ -> Error failure) } in
                 (match check ~host:denied ~source_fingerprint with
                  | Error actual -> assert (phys_equal actual failure)
                  | Ok () -> failwith "source fingerprint bypassed host authorization");
                 let changed (ancestor : R.ancestor) =
                   if P.Id.Session.equal ancestor.state.identity.session_id parent_id
                   then Ok (digest "changed parent authority")
                   else source_fingerprint ancestor
                 in
                 (match check ~host ~source_fingerprint:changed with
                  | Error { code = Permission_denied; message; _ } ->
                    assert (
                      String.is_prefix message ~prefix:"delegation.authority_changed:")
                  | Error failure -> raise_s [%sexp (failure : P.Error.t)]
                  | Ok () ->
                    failwith "source fingerprint bypassed the recorded edge digest");
                 [%test_eq: int] 3 !authorizations));
        (* The real record lookup/authorization happens before cancellation.
           The cached root has no parent: find is the last host operation before
           the original Owner bracket enters its cancellation sub-context. *)
        let module CO = Agent_session.Close_observation in
        let module Diag = Agent_session.Cleanup_diagnostic in
        let module Recovery = Owner.Cleanup_recovery in
        let cancelled_cause = Failure "actual Independent pre-entry cancellation" in
        let cancelled_finds = ref 0
        and cancelled_builds = ref 0
        and cancelled_callbacks = ref 0
        and cancelled_captures = ref 0 in
        let previous_backtraces = Stdlib.Printexc.backtrace_status () in
        Exn.protect
          ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous_backtraces)
          ~f:(fun () ->
            Stdlib.Printexc.record_backtrace true;
            let check_cancelled observed =
              match CO.exit observed with
              | Raised ((Eio.Cancel.Cancelled cause as actual), trace) ->
                assert (phys_equal cause cancelled_cause);
                let selected = CO.primary observed |> Option.value_exn in
                (match Diag.failures selected with
                 | [ Agent_session.Callback_cleanup.Failure.Raised (original, producer) ]
                   ->
                   assert (phys_equal original actual);
                   let prefix = Stdlib.Printexc.raw_backtrace_to_string producer in
                   assert (not (String.is_empty prefix));
                   assert (
                     String.is_prefix
                       (Stdlib.Printexc.raw_backtrace_to_string trace)
                       ~prefix)
                 | _ -> failwith "pre-entry cancellation origin was replaced");
                selected
              | Returned _ | Raised _ ->
                failwith "pre-entry Owner cancellation was not preserved"
            in
            let no_capture =
              Eio.Cancel.sub (fun context ->
                Eio.Cancel.cancel context cancelled_cause;
                Owner.with_resource_lifetime_observed
                  owner
                  ~capture:(fun _ -> Int.incr cancelled_captures)
                  (fun () ->
                     Int.incr cancelled_callbacks;
                     CO.create (Returned (Ok ())) ~diagnostics:[] ()))
            in
            assert (
              Option.equal
                Owner.Lease_outcome.Primary_stage.equal
                (Owner.Lease_outcome.primary_stage no_capture)
                (Some Scope_exit_before_entry));
            let observed = Owner.Lease_outcome.observation no_capture in
            let selected = check_cancelled observed in
            (match
               ( Owner.Scope_outcome.primary (Owner.Lease_outcome.outcome no_capture)
               , Diag.failures selected )
             with
             | ( Raised (actual, trace)
               , [ Agent_session.Callback_cleanup.Failure.Raised (same, producer_trace) ]
               ) ->
               assert (phys_equal actual same);
               assert (phys_equal trace producer_trace)
             | _ -> failwith "actual Owner producer origin diverged");
            [%test_eq: int] 0 !cancelled_captures;
            let actual_holder = Owner.Lease_outcome.cleanup no_capture in
            let original =
              Recovery.original actual_holder |> protocol_ok |> Option.value_exn
            in
            (match Owner.Scope_outcome.Cleanup.status original with
             | Never_acquired -> ()
             | Joined | Failed _ -> failwith "cancelled pre-entry invented acquisition");
            let cancelled_host context =
              { host with
                find =
                  (fun _ ->
                    Int.incr cancelled_finds;
                    Eio.Cancel.cancel context cancelled_cause;
                    Ok { state; owner })
              ; build_root =
                  (fun ~sw:_ ~register_tracking:_ _ ->
                    Int.incr cancelled_builds;
                    failwith "cancelled Independent entered construction")
              }
            in
            let fresh = R.Cleanup.create () in
            assert (Option.is_none (R.Cleanup.snapshot fresh |> protocol_ok));
            let unentered = R.Cleanup.finish_without_entry fresh |> protocol_ok in
            assert (R.Cleanup.Snapshot.custody_joined unentered);
            assert (
              phys_equal unentered (R.Cleanup.finish_without_entry fresh |> protocol_ok));
            let empty = R.Cleanup.Snapshot.cleanup unentered |> protocol_ok in
            (match CO.exit empty with
             | Returned (Ok ()) -> ()
             | Returned (Error _) | Raised _ -> failwith "fresh no-entry cleanup failed");
            assert (List.is_empty (CO.diagnostics empty));
            let reused_validation = ref 0 in
            (match
               R.with_chain_observed
                 fresh
                 ~max_depth:8
                 ~host:
                   { host with
                     resolve =
                       (fun reference ->
                         Int.incr reused_validation;
                         host.resolve reference)
                   }
                 ~reference:(D.reference record)
                 ~f:(fun _ -> failwith "sealed fresh ancestry entered")
             with
             | _ -> failwith "sealed fresh ancestry accepted original reuse"
             | exception Invalid_argument _ -> ());
            [%test_eq: int] 0 !reused_validation;
            let ancestry = R.Cleanup.create () in
            assert (Option.is_none (R.Cleanup.snapshot ancestry |> protocol_ok));
            let cancelled =
              Eio.Cancel.sub (fun context ->
                R.with_chain_observed
                  ancestry
                  ~max_depth:8
                  ~host:(cancelled_host context)
                  ~reference:(D.reference record)
                  ~f:(fun _ ->
                    Int.incr cancelled_callbacks;
                    CO.create (Returned (Ok ())) ~diagnostics:[] ()))
            in
            let selected = check_cancelled cancelled in
            let snapshot =
              R.Cleanup.snapshot ancestry |> protocol_ok |> Option.value_exn
            in
            assert (R.Cleanup.Snapshot.custody_joined snapshot);
            (match R.Cleanup.Snapshot.fixed_controls snapshot |> protocol_ok with
             | [ control ] ->
               assert (Option.equal Diag.equal (CO.primary control) (Some selected))
             | _ -> failwith "original pre-entry cancellation control was lost");
            assert (
              List.is_empty (R.Cleanup.Snapshot.cleanup_controls snapshot |> protocol_ok));
            (match R.Cleanup.Snapshot.cleanup snapshot |> protocol_ok |> CO.exit with
             | Returned (Ok ()) -> ()
             | Returned (Error _) | Raised _ ->
               failwith "unacquired ancestry cleanup was not settled");
            let original = R.Cleanup.Snapshot.original_view snapshot in
            assert (Agent_server.Cleanup_original_view.diagnostics_complete original);
            (match Agent_server.Cleanup_original_view.diagnostics original with
             | [ actual ] -> assert (Diag.equal actual selected)
             | _ -> failwith "actual uncaptured Owner origin was lost or duplicated");
            let raw_cleanup = Agent_server.Independent_cleanup.create () in
            (match
               Eio.Cancel.sub (fun context ->
                 R.with_chain_reported
                   ~cleanup:raw_cleanup
                   ~max_depth:8
                   ~host:(cancelled_host context)
                   ~reference:(D.reference record)
                   ~f:(fun _ ->
                     Int.incr cancelled_callbacks;
                     Ok 7))
             with
             | Ok _ | Error _ -> failwith "raw cancelled chain returned normally"
             | exception Eio.Cancel.Cancelled cause ->
               assert (phys_equal cause cancelled_cause);
               assert (
                 not
                   (String.is_empty
                      (Stdlib.Printexc.raw_backtrace_to_string
                         (Stdlib.Printexc.get_raw_backtrace ())))));
            [%test_eq: Agent_server.Independent_cleanup.Report.status]
              Never_entered
              (Agent_server.Independent_cleanup.report raw_cleanup
               |> Option.value_exn
               |> Agent_server.Independent_cleanup.Report.status);
            [%test_eq: int * int * int]
              (2, 0, 0)
              (!cancelled_finds, !cancelled_builds, !cancelled_callbacks));
        let rejected = Agent_server.Independent_cleanup.create () in
        let denied = { host with authorize = (fun _ -> Error failure) } in
        (match
           R.with_chain_reported
             ~cleanup:rejected
             ~max_depth:8
             ~host:denied
             ~reference:(D.reference record)
             ~f:(fun _ -> failwith "denied ancestry escaped")
         with
         | Error actual -> assert (phys_equal actual failure)
         | Ok _ -> failwith "denied Independent ancestry was admitted");
        let rejected_report =
          Agent_server.Independent_cleanup.report rejected |> Option.value_exn
        in
        [%test_eq: Agent_server.Independent_cleanup.Report.status]
          Never_entered
          (Agent_server.Independent_cleanup.Report.status rejected_report);
        assert (not !released);
        let constructed =
          let previous_backtraces = Stdlib.Printexc.backtrace_status () in
          Exn.protect
            ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous_backtraces)
            ~f:(fun () ->
              Stdlib.Printexc.record_backtrace true;
              R.with_chain_observed
                construction_cleanup
                ~max_depth:8
                ~host
                ~reference:(D.reference record)
                ~f:(fun _ -> failwith "failed resources escaped"))
        in
        (match CO.resume constructed with
         | Error actual ->
           assert (phys_equal actual failure);
           [%test_eq: Sexp.t] (P.Error.sexp_of_t actual) (P.Error.sexp_of_t failure)
         | Ok _ -> assert false);
        let construction_snapshot =
          R.Cleanup.snapshot construction_cleanup |> protocol_ok |> Option.value_exn
        in
        let selected = CO.primary constructed |> Option.value_exn in
        (match R.Cleanup.Snapshot.fixed_controls construction_snapshot |> protocol_ok with
         | [ control ] ->
           assert (Option.equal Diag.equal (CO.primary control) (Some selected))
         | _ -> failwith "original construction control was lost");
        assert (
          List.is_empty
            (R.Cleanup.Snapshot.cleanup_controls construction_snapshot |> protocol_ok));
        let graph_cleanup =
          R.Cleanup.Snapshot.cleanup construction_snapshot |> protocol_ok
        in
        let hook_group = CO.primary graph_cleanup |> Option.value_exn in
        let construction_original =
          R.Cleanup.Snapshot.original_view construction_snapshot
        in
        assert (not (Diag.equal selected hook_group));
        (match CO.exit graph_cleanup, Diag.failures hook_group with
         | ( Raised (Exit, trace)
           , [ Agent_session.Callback_cleanup.Failure.Raised (Exit, producer_trace) ] ) ->
           assert (phys_equal trace producer_trace);
           assert (
             not
               (String.is_empty (Stdlib.Printexc.raw_backtrace_to_string producer_trace)))
         | _ -> failwith "preparation role erased the actual release hook failure");
        assert !released;
        let module Cleanup = Agent_server.Independent_cleanup in
        let original_report = Cleanup.report cleanup |> Option.value_exn in
        [%test_eq: Cleanup.Report.status] Failed (Cleanup.Report.status original_report);
        (match Cleanup.Report.failures original_report, CO.exit graph_cleanup with
         | [ Cleanup.Failure.Raised (Exit, original_trace) ], Raised (Exit, trace) ->
           assert (phys_equal original_trace trace)
         | _ -> failwith "actual Independent release failure was lost");
        assert (
          List.equal
            Diag.equal
            (Cleanup.Report.diagnostics original_report)
            [ hook_group ]);
        let original_groups =
          Agent_server.Cleanup_original_view.diagnostics construction_original
        in
        [%test_eq: int] 2 (List.length original_groups);
        assert (List.mem original_groups selected ~equal:Diag.equal);
        assert (List.mem original_groups hook_group ~equal:Diag.equal);
        (match Cleanup.retry cleanup with
         | Ok () | Error _ -> failwith "graph success erased immutable release failure"
         | exception Exit -> ());
        [%test_eq: Cleanup.Report.status] Failed (Cleanup.Report.status original_report);
        assert (
          phys_equal
            construction_snapshot
            (R.Cleanup.snapshot construction_cleanup |> protocol_ok |> Option.value_exn));
        assert (
          List.equal
            Diag.equal
            original_groups
            (Agent_server.Cleanup_original_view.diagnostics construction_original));
        assert (not (Owner.is_loaded owner));
        let row =
          (A.state actor |> protocol_ok).inference_ledger |> L.rows |> List.hd_exn
        in
        (match Inference.Observation.Attempt_record.state (L.Row.record row) with
         | Interrupted { reason = Host_interrupted; delivery = Definitely_not_submitted }
           -> ()
         | _ -> failwith "residual Prepared attempt was not reconciled after join");
        (* Use the SAME genuine Owner after its actual permanent close. The
           full admission diagnostic remains original evidence, while the
           cleanup-primary view omits only its producer-tagged preparation role. *)
        Owner.close_and_wait owner;
        let denied_builds = ref 0
        and denied_callbacks = ref 0 in
        let denied_scope =
          Owner.with_resource_lifetime_observed owner ~capture:ignore (fun () ->
            Int.incr denied_callbacks;
            CO.create (Returned (Ok ())) ~diagnostics:[] ())
        in
        assert (
          Option.equal
            Owner.Lease_outcome.Primary_stage.equal
            (Owner.Lease_outcome.primary_stage denied_scope)
            (Some Admission));
        (match Owner.Scope_outcome.primary (Owner.Lease_outcome.outcome denied_scope) with
         | Returned (Error error) ->
           (match CO.exit (Owner.Lease_outcome.observation denied_scope) with
            | Returned (Error actual) -> assert (phys_equal actual error)
            | Returned (Ok ()) | Raised _ -> failwith "closed admission changed exit")
         | Returned (Ok ()) | Raised _ -> failwith "closed Owner admitted resources");
        let denied_cleanup = R.Cleanup.create () in
        let denied =
          R.with_chain_observed
            denied_cleanup
            ~max_depth:8
            ~host:
              { host with
                build_root =
                  (fun ~sw:_ ~register_tracking:_ _ ->
                    Int.incr denied_builds;
                    failwith "denied Owner entered construction")
              }
            ~reference:(D.reference record)
            ~f:(fun _ ->
              Int.incr denied_callbacks;
              CO.create (Returned (Ok ())) ~diagnostics:[] ())
        in
        let selected = CO.primary denied |> Option.value_exn in
        (match CO.exit denied, Diag.failures selected with
         | ( Returned (Error actual)
           , [ Agent_session.Callback_cleanup.Failure.Protocol error ] ) ->
           assert (phys_equal actual error)
         | _ -> failwith "denied admission origin was lost");
        let denied_snapshot =
          R.Cleanup.snapshot denied_cleanup |> protocol_ok |> Option.value_exn
        in
        assert (R.Cleanup.Snapshot.custody_joined denied_snapshot);
        (match R.Cleanup.Snapshot.fixed_controls denied_snapshot |> protocol_ok with
         | [ original ] ->
           assert (Option.equal Diag.equal (CO.primary original) (Some selected))
         | _ -> failwith "denied admission original control was lost");
        assert (
          List.is_empty
            (R.Cleanup.Snapshot.cleanup_controls denied_snapshot |> protocol_ok));
        (match R.Cleanup.Snapshot.cleanup denied_snapshot |> protocol_ok |> CO.exit with
         | Returned (Ok ()) -> ()
         | Returned (Error _) | Raised _ ->
           failwith "denied admission invented cleanup failure");
        [%test_eq: int * int] (0, 0) (!denied_builds, !denied_callbacks);
        print_endline "original error retained; teardown ran; graph finished after join"));
  [%expect {| original error retained; teardown ran; graph finished after join |}]
;;

exception Independent_graph_primary

let%expect_test "Independent factual graph retry cannot erase actual hook failure" =
  let module A = Agent_session.Session_actor in
  let module G = Agent_server.Graph_tracking in
  let module C = Agent_server.Independent_cleanup in
  let module CO = Agent_session.Close_observation in
  let module D = Agent_session.Cleanup_diagnostic in
  let module L = Agent_session.Inference_ledger in
  let recording = Stdlib.Printexc.backtrace_status () in
  Stdlib.Printexc.record_backtrace true;
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace recording)
    ~f:(fun () ->
      List.iter [ false; true ] ~f:(fun hook_fails ->
        Inference_tracking_tests.with_tracking_actor (fun _ actor reject commits ->
          Exn.protect
            ~finally:(fun () -> reject := false)
            ~f:(fun () ->
              let cleanup = C.create () in
              let executions = ref 0
              and hooks = ref 0 in
              let _, _, prepared = Inference_tracking_tests.prepared () in
              let primary = Independent_graph_primary in
              let source_trace = ref None in
              let source_diagnostic = ref None in
              let outcome =
                C.run_observed cleanup ~f:(fun ~sw ~register_tracking ->
                  try
                    Int.incr executions;
                    let graph =
                      G.create
                        actor
                        ~source:
                          (Inference_tracking_tests.source "Independent-graph-retry")
                        ~upstream:
                          (Inference_tracking_tests.upstream
                             (fun ~scope:_ ~accounting_id:_ -> ()))
                      |> protocol_ok
                    in
                    register_tracking graph;
                    Eio.Switch.on_release sw (fun () ->
                      Int.incr hooks;
                      if hook_fails then raise Exit);
                    (G.identity graph).with_attempt
                      prepared
                      ~relation:Root
                      ~f:(fun ~scope:_ ~accounting_id:_ ~submission_guard:_ ->
                        (* Reject before actual backend commit. The original source
                 exception wins while release leaves the SAME registration
                 retained; graph finish must later persist its interruption. *)
                        reject := true;
                        try raise primary with
                        | exn ->
                          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                          source_trace := Some backtrace;
                          Exn.raise_with_original_backtrace exn backtrace)
                  with
                  | exn ->
                    let trace = Stdlib.Printexc.get_raw_backtrace () in
                    let diagnostic =
                      D.create
                        [ Agent_session.Callback_cleanup.Failure.Raised (exn, trace) ]
                    in
                    source_diagnostic := Some diagnostic;
                    CO.create
                      (CO.Raised (exn, trace))
                      ~diagnostics:[ diagnostic ]
                      ~primary:diagnostic
                      ())
              in
              (match C.Outcome.result outcome with
               | Error (C.Failure.Raised (actual, backtrace)) ->
                 assert (phys_equal actual primary);
                 let prefix =
                   Option.value_exn !source_trace
                   |> Stdlib.Printexc.raw_backtrace_to_string
                 in
                 assert (not (String.is_empty prefix));
                 assert (
                   String.is_prefix
                     (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                     ~prefix)
               | _ -> failwith "actual graph cleanup replaced source primary");
              let supplied = Option.value_exn !source_diagnostic in
              let observed = C.Outcome.observation outcome in
              assert (D.equal supplied (CO.primary observed |> Option.value_exn));
              let original = C.Outcome.cleanup outcome in
              assert (phys_equal original (C.original_report cleanup |> Option.value_exn));
              let original_groups = C.Report.diagnostics original in
              let hook_group, graph_group =
                match hook_fails, original_groups with
                | false, [ graph ] -> None, graph
                | true, [ hook; graph ] -> Some hook, graph
                | _ -> failwith "actual original graph/Switch carriers changed"
              in
              assert (not (D.equal supplied graph_group));
              assert (
                List.equal D.equal (CO.diagnostics observed) (supplied :: original_groups));
              let original_release =
                C.Report.release_report original |> Option.value_exn
              in
              assert (
                phys_equal
                  original_release
                  (Eio.Switch.Outcome.release (C.Outcome.switch_outcome outcome)));
              (match Eio.Switch.Release_report.status original_release with
               | Complete -> ()
               | Not_entered | Incomplete -> failwith "actual Switch release did not join");
              [%test_eq: int]
                (if hook_fails then 1 else 0)
                (List.length (Eio.Switch.Release_report.failures original_release));
              assert (C.equal_retry_status Retryable (C.retry_status cleanup));
              [%test_eq: C.Report.status] Failed (C.Report.status original);
              (match hook_fails, C.Report.failures original with
               | false, [ C.Failure.Protocol { code = Persistence_error; _ } ]
               | true, [ Raised (Exit, _); Protocol { code = Persistence_error; _ } ] ->
                 ()
               | _ -> failwith "actual graph/hook failure order changed");
              [%test_eq: int] 1 !executions;
              [%test_eq: int] 1 !hooks;
              let original_commits = !commits in
              let original_observed = C.Report.observation original |> protocol_ok in
              assert (
                List.equal D.equal (CO.diagnostics original_observed) original_groups);
              assert (
                Option.equal
                  D.equal
                  (CO.primary original_observed)
                  (Some (List.hd_exn original_groups)));
              (match C.Report.failures original, CO.exit original_observed with
               | C.Failure.Protocol expected :: _, Returned (Error actual) ->
                 assert (phys_equal actual expected)
               | Raised (expected, expected_trace) :: _, Raised (actual, actual_trace) ->
                 assert (phys_equal actual expected);
                 assert (phys_equal actual_trace expected_trace)
               | _ ->
                 failwith "original cleanup projection changed actual selected failure");
              (* Reading the original pair cannot run a graph retry, source,
                 release hook, or canonical commit before the explicit request. *)
              [%test_eq: int] original_commits !commits;
              [%test_eq: int] 1 !executions;
              [%test_eq: int] 1 !hooks;
              assert (phys_equal original (C.report cleanup |> Option.value_exn));
              assert (C.equal_retry_status Retryable (C.retry_status cleanup));
              (* A still-failing ACTUAL graph retry owns a new current carrier;
                 the original report and immutable Switch carrier stay unchanged. *)
              let failed_retry = C.retry_observed cleanup |> protocol_ok in
              (match hook_fails, CO.exit failed_retry with
               | false, Returned (Error { code = Persistence_error; _ })
               | true, Raised (Exit, _) -> ()
               | _ -> failwith "actual graph retry changed selected failure");
              let retried = C.report cleanup |> Option.value_exn in
              let retried_groups = C.Report.diagnostics retried in
              let retry_graph = List.last_exn retried_groups in
              assert (not (D.equal graph_group retry_graph));
              assert (
                List.equal
                  D.equal
                  retried_groups
                  (Option.to_list hook_group @ [ retry_graph ]));
              assert (List.equal D.equal (CO.diagnostics failed_retry) retried_groups);
              assert (
                D.equal
                  (CO.primary failed_retry |> Option.value_exn)
                  (List.hd_exn retried_groups));
              assert (phys_equal original (C.original_report cleanup |> Option.value_exn));
              assert (List.equal D.equal original_groups (C.Report.diagnostics original));
              reject := false;
              (match C.retry cleanup with
               | Ok () when not hook_fails -> ()
               | Ok () | Error _ ->
                 failwith "same-holder graph retry lost actual hook outcome"
               | exception Exit -> assert hook_fails);
              let repaired = C.report cleanup |> Option.value_exn in
              [%test_eq: C.Report.status]
                (if hook_fails then Failed else Joined)
                (C.Report.status repaired);
              (match hook_fails, C.Report.failures repaired with
               | false, [] | true, [ C.Failure.Raised (Exit, _) ] -> ()
               | _ -> failwith "graph repair erased immutable hook failure");
              [%test_eq: C.Report.status] Failed (C.Report.status original);
              assert (phys_equal original (C.original_report cleanup |> Option.value_exn));
              assert (List.equal D.equal original_groups (C.Report.diagnostics original));
              assert (List.equal D.equal retried_groups (C.Report.diagnostics retried));
              assert (
                List.equal
                  D.equal
                  (Option.to_list hook_group)
                  (C.Report.diagnostics repaired));
              assert (
                phys_equal
                  original_release
                  (C.Report.release_report repaired |> Option.value_exn));
              assert (
                C.equal_retry_status
                  (if hook_fails then Fixed else Joined)
                  (C.retry_status cleanup));
              let row =
                (A.state actor |> protocol_ok).inference_ledger |> L.rows |> List.hd_exn
              in
              (match Inference.Observation.Attempt_record.state (L.Row.record row) with
               | Interrupted
                   { reason = Host_interrupted; delivery = Definitely_not_submitted } ->
                 ()
               | _ -> failwith "same retained graph did not settle original Prepared row");
              let joined_commits = !commits in
              (match C.retry cleanup with
               | Ok () when not hook_fails -> ()
               | Ok () | Error _ -> failwith "repeated factual retry changed hook outcome"
               | exception Exit -> assert hook_fails);
              [%test_eq: int] joined_commits !commits;
              [%test_eq: int] 1 !executions;
              [%test_eq: int] 1 !hooks;
              print_s
                [%sexp
                  { hook_fails : bool; graph_repaired = true; source_runs = 1; hooks = 1 }]))));
  [%expect
    {|
    ((hook_fails false) (graph_repaired true) (source_runs 1) (hooks 1))
    ((hook_fails true) (graph_repaired true) (source_runs 1) (hooks 1))
    |}]
;;

exception Independent_registration_primary

let%expect_test "Independent graph registration closes before sealing and cannot escape" =
  let module G = Agent_server.Graph_tracking in
  let module C = Agent_server.Independent_cleanup in
  List.iter [ false; true ] ~f:(fun raises ->
    Inference_tracking_tests.with_tracking_actor (fun _ actor _reject commits ->
      let cleanup = C.create () in
      let late = ref None in
      let escaped = ref None in
      let rejected_while_joining = ref false in
      let reject expected f =
        match f () with
        | () -> failwith "escaped graph registration was accepted"
        | exception Invalid_argument actual -> [%test_eq: string] expected actual
      in
      let create source =
        G.create
          actor
          ~source:(Inference_tracking_tests.source source)
          ~upstream:
            (Inference_tracking_tests.upstream (fun ~scope:_ ~accounting_id:_ -> ()))
        |> protocol_ok
      in
      Exn.protect
        ~finally:(fun () ->
          Option.iter !late ~f:(fun graph ->
            G.seal graph |> protocol_ok;
            G.finish graph |> protocol_ok))
        ~f:(fun () ->
          let outcome =
            C.run cleanup ~f:(fun ~sw ~register_tracking ->
              let graph = create "Independent-accepted-registration" in
              let unregistered = create "Independent-escaped-registration" in
              late := Some unregistered;
              register_tracking graph;
              reject "Independent graph is already registered" (fun () ->
                register_tracking graph);
              let foreign =
                Stdlib.Domain.spawn (fun () ->
                  match register_tracking unregistered with
                  | () -> None
                  | exception Invalid_argument message -> Some message)
              in
              [%test_eq: string option]
                (Some "Independent cleanup belongs to another Eio domain")
                (Stdlib.Domain.join foreign);
              escaped := Some register_tracking;
              let callback_exited, callback_exited_u = Eio.Promise.create () in
              Eio.Fiber.fork ~sw (fun () ->
                Eio.Promise.await callback_exited;
                reject "Independent graph registration is closed" (fun () ->
                  register_tracking unregistered);
                rejected_while_joining := true);
              Eio.Switch.on_release sw (fun () -> assert !rejected_while_joining);
              (* Resolve does not yield. The callback exits before seal's Actor
                 RPC yields to the child, so this is a real after-callback cut. *)
              Eio.Promise.resolve callback_exited_u ();
              if raises then raise Independent_registration_primary;
              Ok ())
          in
          (match raises, C.Outcome.result outcome with
           | false, Ok ()
           | true, Error (C.Failure.Raised (Independent_registration_primary, _)) -> ()
           | _ -> failwith "registration checks changed callback primary");
          assert !rejected_while_joining;
          let completed = C.Outcome.cleanup outcome in
          [%test_eq: C.Report.status] Joined (C.Report.status completed);
          let unregistered = Option.value_exn !late in
          reject "Independent graph registration is closed" (fun () ->
            (Option.value_exn !escaped) unregistered);
          (* The rejected real graph remains outside this holder. It was never
             sealed by the bracket and must be closed by its actual local owner. *)
          (match G.finish unregistered with
           | Error { code = Conflict; _ } -> ()
           | Ok () | Error _ -> failwith "late graph entered completed bracket custody");
          let before_retry = !commits in
          C.retry cleanup |> protocol_ok;
          [%test_eq: int] before_retry !commits;
          assert (phys_equal completed (C.Outcome.cleanup outcome));
          if not raises
          then (
            let module CO = Agent_session.Close_observation in
            let module D = Agent_session.Cleanup_diagnostic in
            let recording = Stdlib.Printexc.backtrace_status () in
            Stdlib.Printexc.record_backtrace true;
            Exn.protect
              ~finally:(fun () -> Stdlib.Printexc.record_backtrace recording)
              ~f:(fun () ->
                let hook = Failure "Independent actual non-Cancelled hook" in
                let cancelled_hook = Eio.Cancel.Cancelled Exit in
                let producer_traces = ref [] in
                let order = ref [] in
                let failed_hooks = C.create () in
                let hooks_outcome =
                  C.run failed_hooks ~f:(fun ~sw ~register_tracking ->
                    let graph = create "Independent-actual-Switch-supporting-parts" in
                    register_tracking graph;
                    List.iteri [ cancelled_hook; hook ] ~f:(fun index actual ->
                      Eio.Switch.on_release sw (fun () ->
                        order := index :: !order;
                        match raise actual with
                        | _ -> assert false
                        | exception exn ->
                          let trace = Stdlib.Printexc.get_raw_backtrace () in
                          producer_traces := (actual, trace) :: !producer_traces;
                          Exn.raise_with_original_backtrace exn trace));
                    Ok ())
                in
                [%test_eq: int list] [ 1; 0 ] (List.rev !order);
                let selected, selected_trace =
                  match C.Outcome.result hooks_outcome with
                  | Error (Raised (actual, trace)) ->
                    (* Eio discards the later Cancelled hook from this raw result. *)
                    assert (phys_equal actual hook);
                    actual, trace
                  | Ok () | Error (Protocol _) ->
                    failwith "actual hook failure changed raw Independent selection"
                in
                let report = C.Outcome.cleanup hooks_outcome in
                [%test_eq: C.Report.status] Failed (C.Report.status report);
                let release = C.Report.release_report report |> Option.value_exn in
                let actual_hooks = Eio.Switch.Release_report.failures release in
                List.iter2_exn
                  (C.Report.failures report)
                  actual_hooks
                  ~f:(fun actual (expected, expected_trace) ->
                    match actual with
                    | C.Failure.Raised (exn, trace) ->
                      assert (phys_equal exn expected);
                      assert (phys_equal trace expected_trace)
                    | Protocol _ -> failwith "hook report changed raw failure kind");
                List.iter2_exn
                  actual_hooks
                  (List.rev !producer_traces)
                  ~f:(fun (actual, trace) (expected, original_trace) ->
                    assert (phys_equal actual expected);
                    let prefix = Stdlib.Printexc.raw_backtrace_to_string original_trace in
                    assert (not (String.is_empty prefix));
                    assert (
                      String.is_prefix
                        (Stdlib.Printexc.raw_backtrace_to_string trace)
                        ~prefix));
                let observed = C.Outcome.observation hooks_outcome in
                (match CO.exit observed with
                 | Raised (actual, trace) ->
                   assert (phys_equal actual selected);
                   assert (phys_equal trace selected_trace)
                 | Returned _ -> failwith "Switch diagnostics changed raw exit");
                let group = CO.primary observed |> Option.value_exn in
                assert (List.equal D.equal (CO.diagnostics observed) [ group ]);
                assert (List.equal D.equal (C.Report.diagnostics report) [ group ]);
                match D.failures group with
                | Raised (actual, trace) :: supporting ->
                  assert (phys_equal actual selected);
                  assert (phys_equal trace selected_trace);
                  (* SAME Switch carrier, separate structural report parts:
                      no aggregate parsing or exception-payload deduplication. *)
                  List.iter2_exn supporting actual_hooks ~f:(fun part (exn, trace) ->
                    match part with
                    | Raised (actual, actual_trace) ->
                      assert (phys_equal actual exn);
                      assert (phys_equal actual_trace trace)
                    | Protocol _ -> failwith "Switch supporting part lost raw hook")
                | [] | Protocol _ :: _ ->
                  failwith "Switch aggregate lost its selected diagnostic part"));
          print_s
            [%sexp { raises : bool; late_rejected = true; duplicates_rejected = true }])));
  [%expect
    {|
    ((raises false) (late_rejected true) (duplicates_rejected true))
    ((raises true) (late_rejected true) (duplicates_rejected true))
    |}]
;;

let%expect_test "Independent factual retry owns delayed graph work until completion" =
  let module G = Agent_server.Graph_tracking in
  let module C = Agent_server.Independent_cleanup in
  let module CO = Agent_session.Close_observation in
  let module D = Agent_session.Cleanup_diagnostic in
  let module A = Agent_session.Session_actor in
  let module L = Agent_session.Inference_ledger in
  let reject = ref false in
  let delay = ref None in
  let commits = ref 0 in
  Job_fixtures.with_actor
    ~start:false
    ~reject_save:(fun _ ->
      Option.iter !delay ~f:(fun (entered, release) ->
        delay := None;
        Eio.Promise.resolve entered ();
        Eio.Promise.await release);
      !reject)
    ~state_committed:(fun _ _ -> Int.incr commits)
    (fun env sw actor _writer _backend ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
         let cleanup = C.create () in
         let _, _, prepared = Inference_tracking_tests.prepared () in
         Exn.protect
           ~finally:(fun () ->
             reject := false;
             delay := None)
           ~f:(fun () ->
             let outcome =
               C.run cleanup ~f:(fun ~sw:_ ~register_tracking ->
                 assert (C.equal_retry_status Original_running (C.retry_status cleanup));
                 (match C.retry_observed cleanup with
                  | Error { code = Conflict; retryable = true; _ } -> ()
                  | Ok _ | Error _ -> failwith "running original admitted a factual retry");
                 assert (Option.is_none (C.original_report cleanup));
                 let graph =
                   G.create
                     actor
                     ~source:(Inference_tracking_tests.source "Independent-delayed-retry")
                     ~upstream:
                       (Inference_tracking_tests.upstream
                          (fun ~scope:_ ~accounting_id:_ -> ()))
                   |> protocol_ok
                 in
                 register_tracking graph;
                 (G.identity graph).with_attempt
                   prepared
                   ~relation:Root
                   ~f:(fun ~scope:_ ~accounting_id:_ ~submission_guard:_ ->
                     reject := true;
                     raise Independent_graph_primary))
             in
             (match C.Outcome.result outcome with
              | Error (C.Failure.Raised (Independent_graph_primary, _)) -> ()
              | _ -> failwith "delayed retry setup lost original primary");
             let original = C.Outcome.cleanup outcome in
             [%test_eq: C.Report.status] Failed (C.Report.status original);
             let original_groups = C.Report.diagnostics original in
             assert (phys_equal original (C.original_report cleanup |> Option.value_exn));
             assert (C.equal_retry_status Retryable (C.retry_status cleanup));
             reject := false;
             let entered, entered_u = Eio.Promise.create () in
             let release, release_u = Eio.Promise.create () in
             delay := Some (entered_u, release);
             let first =
               Eio.Fiber.fork_promise ~sw (fun () -> C.retry_observed cleanup)
             in
             Exn.protect
               ~finally:(fun () ->
                 Eio.Cancel.protect (fun () ->
                   if Option.is_none (Eio.Promise.peek release)
                   then Eio.Promise.resolve release_u ();
                   (* Always release/join the actual producer, including an
                      assertion/timeout. Its result is projected in the body;
                      this join must not replace an earlier actual failure. *)
                   ignore (Eio.Promise.await first)))
               ~f:(fun () ->
                 Eio.Promise.await entered;
                 assert (C.equal_retry_status Busy (C.retry_status cleanup));
                 (match C.retry_observed cleanup with
                  | Error { code = Conflict; message; _ } ->
                    [%test_eq: string]
                      "Independent factual retry is already running"
                      message
                  | Ok _ | Error _ ->
                    failwith "concurrent retry replayed delayed graph work");
                 assert (phys_equal original (C.report cleanup |> Option.value_exn));
                 assert (
                   phys_equal original (C.original_report cleanup |> Option.value_exn));
                 assert (
                   List.equal D.equal original_groups (C.Report.diagnostics original));
                 (match C.finish_without_entry cleanup with
                  | () -> failwith "active factual retry was marked exited"
                  | exception Invalid_argument message ->
                    [%test_eq: string] "Independent cleanup is still running" message);
                 assert (Option.is_none (Eio.Promise.peek first));
                 Eio.Promise.resolve release_u ();
                 let completed = Eio.Promise.await_exn first |> protocol_ok in
                 (match CO.exit completed with
                  | Returned (Ok ()) -> ()
                  | Returned (Error _) | Raised _ ->
                    failwith "performed factual retry did not join");
                 assert (List.is_empty (CO.diagnostics completed));
                 assert (Option.is_none (CO.primary completed));
                 assert (C.equal_retry_status Joined (C.retry_status cleanup));
                 assert (
                   phys_equal original (C.original_report cleanup |> Option.value_exn));
                 [%test_eq: C.Report.status] Failed (C.Report.status original);
                 [%test_eq: C.Report.status]
                   Joined
                   (C.report cleanup |> Option.value_exn |> C.Report.status);
                 let row =
                   (A.state actor |> protocol_ok).inference_ledger
                   |> L.rows
                   |> List.hd_exn
                 in
                 (match Inference.Observation.Attempt_record.state (L.Row.record row) with
                  | Interrupted
                      { reason = Host_interrupted; delivery = Definitely_not_submitted }
                    -> ()
                  | _ -> failwith "delayed same-graph retry lost original registration");
                 let completed_commits = !commits in
                 C.retry cleanup |> protocol_ok;
                 [%test_eq: int] completed_commits !commits;
                 print_endline
                   "one delayed retry owns actual graph; concurrent retry conflicts; \
                    immutable prior report"))));
  [%expect
    {| one delayed retry owns actual graph; concurrent retry conflicts; immutable prior report |}]
;;
