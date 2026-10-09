open! Core

let%expect_test "scheduler shutdown joins the cancelled initial-start callback" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let entered, enter = Eio.Promise.create () in
      let cleaned = ref false in
      let scheduler =
        Agent_server.Start_scheduler.start
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~registry:(Agent_server.Session_registry.create ())
          ~queue:(Agent_session.Start_queue.create ())
          ~resume_initial_starts:(fun () ->
            Exn.protect
              ~finally:(fun () -> cleaned := true)
              ~f:(fun () ->
                Eio.Promise.resolve enter ();
                Eio.Fiber.await_cancel ()))
      in
      Eio.Promise.await entered;
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 1. (fun () ->
        Agent_server.Start_scheduler.close_and_wait scheduler);
      printf
        "callback-cleaned=%b stopped=%b\n"
        !cleaned
        (not (Agent_server.Start_scheduler.is_running scheduler));
      Agent_server.Start_scheduler.close_and_wait scheduler;
      let disabled =
        Agent_server.Start_scheduler.start_controlled
          ~enabled:false
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~registry:(Agent_server.Session_registry.create ())
          ~queue:(Agent_session.Start_queue.create ())
          ~resume_initial_starts:(fun () -> failwith "disabled callback executed")
      in
      Agent_server.Start_scheduler.close_and_wait disabled;
      print_endline "retry-and-disabled-finished"));
  [%expect
    {|
    callback-cleaned=true stopped=true
    retry-and-disabled-finished
    |}]
;;
