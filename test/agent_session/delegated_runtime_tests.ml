open Core
open Fixtures
module Owner = Agent_server.Runtime_owner
module Delegated = Agent_server.Delegated_runtime

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
