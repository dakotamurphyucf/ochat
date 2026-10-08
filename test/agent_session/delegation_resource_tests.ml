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
    let owner =
      Owner.create
        ~actor
        ~initial:
          (Some
             (Runtime_lease_tests.runtime
                ~close:(fun () ->
                  Int.incr old_closes;
                  failwith "deferred resource close failed")
                ()))
        ~build:(fun () ->
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
    Eio.Promise.resolve release_u ();
    (match Eio.Promise.await_exn borrower with
     | Error (Failure message) ->
       [%test_eq: string] "deferred resource close failed" message
     | _ -> failwith "deferred close failure was lost");
    Owner.with_background_runtime owner (fun _ -> Ok ()) |> protocol_ok;
    Owner.close_and_wait owner;
    [%test_eq: int] 1 !old_closes;
    [%test_eq: int] 1 !fresh_closes;
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
                  ~f:(fun ~scope:_ ~accounting_id:_ -> Error failure))
          ; build_generated = (fun ~sw:_ ~parent:_ _ -> failwith "unexpected descendant")
          }
        in
        (match
           R.with_chain ~max_depth:8 ~host ~reference:(D.reference record) ~f:(fun _ ->
             failwith "failed resources escaped")
         with
         | Error actual ->
           [%test_eq: Sexp.t] (P.Error.sexp_of_t actual) (P.Error.sexp_of_t failure)
         | Ok _ -> assert false);
        assert !released;
        assert (not (Owner.is_loaded owner));
        let row =
          (A.state actor |> protocol_ok).inference_ledger |> L.rows |> List.hd_exn
        in
        (match Inference.Observation.Attempt_record.state (L.Row.record row) with
         | Interrupted { reason = Host_interrupted; delivery = Definitely_not_submitted }
           -> ()
         | _ -> failwith "residual Prepared attempt was not reconciled after join");
        print_endline "original error retained; teardown ran; graph finished after join"));
  [%expect {| original error retained; teardown ran; graph finished after join |}]
;;
