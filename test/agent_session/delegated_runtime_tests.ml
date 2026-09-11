open Core
open Fixtures
module Owner = Agent_server.Runtime_owner
module Delegated = Agent_server.Delegated_runtime

let%expect_test
    "independent construction survives parent unload and joins activity before resources"
  =
  List.iter [ false; true ] ~f:(fun close_parent ->
    Job_fixtures.with_actor (fun _env sw actor _writer _backend ->
      let ready, ready_u = Eio.Promise.create () in
      let cleaning, cleaning_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let cleaned = ref false in
      let parent_closes = ref 0 in
      let child_closes = ref 0 in
      let revoked = ref 0 in
      let original =
        Runtime_lease_tests.runtime
          ~close:(fun () ->
            assert !cleaned;
            [%test_eq: int] 1 !child_closes;
            Int.incr parent_closes)
          ()
      in
      let parent =
        Owner.create ~actor ~initial:(Some original) ~build:(fun () ->
          failwith "unexpected parent resource rebuild")
      in
      let child =
        Delegated.prepare_independent
          ~sw
          ~parent
          ~on_revoked:(fun () ->
            assert !cleaned;
            Int.incr revoked;
            Ok ())
          ~build:(fun ~sw parent_runtime ->
            assert (phys_equal parent_runtime original);
            Ok
              (Runtime_lease_tests.runtime
                 ~activity:(Agent_session.Runtime_activity.create ~sw)
                 ~close:(fun () ->
                   assert !cleaned;
                   Int.incr child_closes)
                 ()))
        |> protocol_ok
      in
      Owner.unload_and_wait parent |> protocol_ok;
      assert (not (Owner.is_loaded parent));
      [%test_eq: int] 0 !parent_closes;
      let activity = Option.value_exn child.activity in
      let worker =
        Eio.Fiber.fork_promise ~sw (fun () ->
          Result.try_with (fun () ->
            Agent_session.Runtime_activity.run activity (fun () ->
              [%test_eq: int] 0 !parent_closes;
              Exn.protect
                ~finally:(fun () ->
                  Eio.Cancel.protect (fun () ->
                    Eio.Promise.resolve cleaning_u ();
                    Eio.Promise.await release;
                    cleaned := true))
                ~f:(fun () ->
                  Eio.Promise.resolve ready_u ();
                  Eio.Promise.await never))))
      in
      Eio.Promise.await ready;
      let closing =
        Eio.Fiber.fork_promise ~sw (fun () ->
          match close_parent with
          | false -> child.close ()
          | true -> Owner.close_and_wait parent)
      in
      Eio.Promise.await cleaning;
      assert (Option.is_none (Eio.Promise.peek closing));
      [%test_eq: int] 0 !parent_closes;
      [%test_eq: int] 0 !child_closes;
      Eio.Promise.resolve release_u ();
      (match Eio.Promise.await_exn worker with
       | Error (Eio.Cancel.Cancelled _) -> ()
       | _ -> failwith "child activity outlived runtime close");
      Eio.Promise.await_exn closing;
      child.close ();
      Owner.close_and_wait parent;
      [%test_eq: int] 1 !parent_closes;
      [%test_eq: int] 1 !child_closes;
      [%test_eq: int] (if close_parent then 1 else 0) !revoked;
      (match Option.value_exn child.check_execution () with
       | Error { code = Interrupted; _ } -> ()
       | _ -> failwith "closed child remained executable");
      print_s
        [%sexp
          (close_parent : bool)
        , "activity survived unload; close joined activity, child cleanup and resources"]));
  [%expect
    {|
    (false
     "activity survived unload; close joined activity, child cleanup and resources")
    (true
     "activity survived unload; close joined activity, child cleanup and resources") |}]
;;

let%expect_test
    "parent cancellation during child preparation joins cleanup and returns a typed error"
  =
  Job_fixtures.with_actor (fun _env sw actor _writer _backend ->
    let started, started_u = Eio.Promise.create () in
    let cleaning, cleaning_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let cleaned = ref false in
    let closes = ref 0 in
    let runtime =
      Runtime_lease_tests.runtime
        ~close:(fun () ->
          assert !cleaned;
          Int.incr closes)
        ()
    in
    let parent =
      Owner.create ~actor ~initial:(Some runtime) ~build:(fun () ->
        failwith "unexpected parent rebuild")
    in
    Exn.protect
      ~finally:(fun () -> Owner.close_and_wait parent)
      ~f:(fun () ->
        let caller =
          Eio.Fiber.fork_promise ~sw (fun () ->
            Delegated.prepare
              ~sw
              ~parent
              ~on_revoked:(fun () ->
                failwith "unpublished child has no lifecycle callback")
              ~build:(fun ~sw:_ _ ->
                Exn.protect
                  ~finally:(fun () ->
                    Eio.Cancel.protect (fun () ->
                      Eio.Promise.resolve cleaning_u ();
                      Eio.Promise.await release;
                      cleaned := true))
                  ~f:(fun () ->
                    Eio.Promise.resolve started_u ();
                    Eio.Promise.await never;
                    failwith "cancelled build continued")))
        in
        Eio.Promise.await started;
        let stopping =
          Eio.Fiber.fork_promise ~sw (fun () -> Owner.unload_and_wait parent)
        in
        Eio.Promise.await cleaning;
        [%test_eq: int] 0 !closes;
        assert (Owner.is_loaded parent);
        Eio.Promise.resolve release_u ();
        (match Eio.Promise.await_exn caller with
         | Error { code = Interrupted; _ } -> ()
         | _ -> failwith "preparation cancellation did not return a typed error");
        Eio.Promise.await_exn stopping |> protocol_ok;
        [%test_eq: int] 1 !closes;
        assert (not (Eio.Fiber.is_cancelled ()));
        print_endline
          "construction cleanup joined; parent retired once; caller remains live"));
  [%expect {| construction cleanup joined; parent retired once; caller remains live |}]
;;
