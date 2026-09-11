open Core
open Fixtures
module Activity = Agent_session.Runtime_activity

let%expect_test
    "runtime and caller cancellation join activity children and retain caller bindings"
  =
  List.iter [ `Caller; `Runtime ] ~f:(fun mode ->
    with_temp_directory (fun env _ ->
      Eio.Switch.run (fun sw ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
          let never, _ = Eio.Promise.create () in
          let ready, ready_u = Eio.Promise.create () in
          let closed = ref false in
          let owner =
            Eio.Fiber.fork_promise ~sw (fun () ->
              Exn.protect
                ~finally:(fun () -> closed := true)
                ~f:(fun () ->
                  Eio.Cancel.sub (fun context ->
                    Eio.Switch.run (fun runtime_sw ->
                      Eio.Promise.resolve ready_u (Activity.create ~sw:runtime_sw, context);
                      Eio.Promise.await never))))
          in
          let activity, owner_context = Eio.Promise.await ready in
          let key = Eio.Fiber.create_key () in
          let release_cleanup, release_cleanup_u = Eio.Promise.create () in
          let finish_second, finish_second_u = Eio.Promise.create () in
          let cleaned = ref [] in
          let entered = Array.init 2 ~f:(fun _ -> Eio.Promise.create ()) in
          let cleaning = Array.init 2 ~f:(fun _ -> Eio.Promise.create ()) in
          let caller_contexts = Array.init 2 ~f:(fun _ -> Eio.Promise.create ()) in
          let finished = Array.create ~len:2 false in
          let callers =
            Array.init 2 ~f:(fun index ->
              Eio.Fiber.fork_promise ~sw (fun () ->
                Exn.protect
                  ~finally:(fun () -> finished.(index) <- true)
                  ~f:(fun () ->
                    Eio.Cancel.sub (fun context ->
                      Eio.Promise.resolve (snd caller_contexts.(index)) context;
                      Eio.Fiber.with_binding key index (fun () ->
                        Activity.with_switch activity (fun ~sw:activity_sw ->
                          [%test_eq: int option] (Some index) (Eio.Fiber.get key);
                          Eio.Fiber.fork_daemon ~sw:activity_sw (fun () ->
                            Exn.protect
                              ~finally:(fun () ->
                                Eio.Cancel.protect (fun () ->
                                  Eio.Promise.resolve (snd cleaning.(index)) ();
                                  Eio.Promise.await release_cleanup;
                                  cleaned := index :: !cleaned))
                              ~f:(fun () ->
                                Eio.Promise.resolve (snd entered.(index)) ();
                                Eio.Promise.await never;
                                `Stop_daemon));
                          match index with
                          | 0 ->
                            Eio.Promise.await never;
                            assert false
                          | _ ->
                            Eio.Promise.await finish_second;
                            "second"))))))
          in
          Array.iter entered ~f:(fun (ready, _) -> Eio.Promise.await ready);
          (match mode with
           | `Caller ->
             Eio.Cancel.cancel (Eio.Promise.await (fst caller_contexts.(0))) Exit
           | `Runtime -> Eio.Cancel.cancel owner_context Exit);
          Eio.Promise.await (fst cleaning.(0));
          assert (not finished.(0));
          assert (not !closed);
          [%test_eq: int list] [] !cleaned;
          (match mode with
           | `Caller ->
             assert (not finished.(1));
             [%test_eq: int] 42 (Activity.run activity (fun () -> 42));
             (match
                Result.try_with (fun () ->
                  Activity.with_switch activity (fun ~sw:_ -> failwith "activity failure"))
              with
              | Error (Failure message) -> [%test_eq: string] "activity failure" message
              | _ -> failwith "activity failure was lost");
             let ran = ref false in
             (match
                Result.try_with (fun () ->
                  Eio.Cancel.sub (fun context ->
                    Eio.Cancel.cancel context Exit;
                    Activity.run activity (fun () -> ran := true)))
              with
              | Error (Eio.Cancel.Cancelled _) -> ()
              | _ -> failwith "already-cancelled caller started an activity");
             assert (not !ran)
           | `Runtime ->
             Eio.Promise.await (fst cleaning.(1));
             assert (not finished.(1)));
          Eio.Promise.resolve release_cleanup_u ();
          (match Eio.Promise.await callers.(0) with
           | Error (Eio.Cancel.Cancelled _) -> ()
           | _ -> failwith "first activity cancellation lost");
          (match mode with
           | `Caller ->
             [%test_eq: int list] [ 0 ] !cleaned;
             Eio.Promise.resolve finish_second_u ();
             [%test_eq: string] "second" (Eio.Promise.await_exn callers.(1));
             Eio.Cancel.cancel owner_context Exit
           | `Runtime ->
             (match Eio.Promise.await callers.(1) with
              | Error (Eio.Cancel.Cancelled _) -> ()
              | _ -> failwith "runtime did not cancel the second activity"));
          (match Eio.Promise.await owner with
           | Error (Eio.Cancel.Cancelled _) -> ()
           | _ -> failwith "runtime switch did not finish cancellation");
          [%test_eq: int list] [ 0; 1 ] (List.sort !cleaned ~compare:Int.compare);
          assert !closed;
          let ran = ref false in
          (match
             Result.try_with (fun () -> Activity.run activity (fun () -> ran := true))
           with
           | Error Activity.Closed -> ()
           | _ -> failwith "late work did not report a closed runtime");
          assert (not !ran);
          print_s
            [%sexp
              (mode : [ `Caller | `Runtime ])
            , "joined nested cleanup; caller scope retained; closed runtime rejects work"]))));
  [%expect
    {|
    (Caller
     "joined nested cleanup; caller scope retained; closed runtime rejects work")
    (Runtime
     "joined nested cleanup; caller scope retained; closed runtime rejects work")
    |}]
;;

let%expect_test
    "closed generated activity returns an interrupted owner request without cancelling \
     its caller"
  =
  Job_fixtures.with_actor (fun _env _sw actor _writer _backend ->
    let activity = Eio.Switch.run (fun sw -> Activity.create ~sw) in
    let runtime = Runtime_lease_tests.runtime ~activity ~close:(fun () -> ()) () in
    let owner =
      Agent_server.Runtime_owner.create ~actor ~initial:(Some runtime) ~build:(fun () ->
        failwith "unexpected runtime rebuild")
    in
    Exn.protect
      ~finally:(fun () -> Agent_server.Runtime_owner.close_and_wait owner)
      ~f:(fun () ->
        let result =
          Agent_server.Runtime_owner.submit_ingress
            owner
            ~producer:principal_id
            ~registration_id:(Agent_protocol.Id.Capability.create ())
            ~namespace:"closed"
            ~key:(Agent_protocol.Idempotency_key.of_string "closed" |> protocol_ok)
            ~payload:`Null
        in
        (match result with
         | Error { code = Interrupted; _ } -> ()
         | _ -> failwith "closed activity escaped request error handling");
        assert (not (Eio.Fiber.is_cancelled ()));
        ignore
          (Agent_session.Session_actor.state actor |> protocol_ok
           : Agent_session.Session_state.t);
        Agent_server.Runtime_owner.unload_and_wait owner |> protocol_ok;
        print_endline "owner request interrupted; caller and cleanup remain usable"));
  [%expect {| owner request interrupted; caller and cleanup remain usable |}]
;;
