open! Core
module Fixture = Driver_tests
module D = Openai.Responses_driver

let%expect_test "callback and cleanup failures retain their original exceptions" =
  Eio_main.run (fun env ->
    List.iter [ Exit; Eio.Time.Timeout ] ~f:(fun callback_error ->
      Fixture.with_server
        env
        (fun flow _ -> Fixture.write flow Fixture.normal)
        (fun _sw endpoint ->
           let cleanup_error = Failure "resolver cleanup failed" in
           let auth ~sw _profile =
             Eio.Switch.on_release sw (fun () -> raise cleanup_error);
             D.Auth.bearer "synthetic-test-credential"
           in
           let terminals = ref 0 in
           let errors =
             try
               ignore
                 (D.run
                    (Fixture.driver env)
                    ~auth
                    ~prepared:(Fixture.prepare (Fixture.profile endpoint))
                    ~on_event:(function
                      | Diagnostic _ | Http_rejection _ -> ()
                      | Update _ | Finalized _ -> raise callback_error
                      | Terminal _ -> incr terminals)
                  : (D.Terminal.t, D.Auth.error) Result.t);
               []
             with
             | Eio.Exn.Multiple errors -> List.map errors ~f:fst
             | ex -> [ ex ]
           in
           (* Exception identity is the contract under test: the callback's exact
             exception and the independent cleanup failure must both survive. *)
           printf
             "callback:%b cleanup:%b errors:%d terminals:%d\n"
             (List.exists errors ~f:(fun ex -> phys_equal ex callback_error))
             (List.exists errors ~f:(fun ex -> phys_equal ex cleanup_error))
             (List.length errors)
             !terminals)));
  [%expect
    {|
    callback:true cleanup:true errors:2 terminals:0
    callback:true cleanup:true errors:2 terminals:0
  |}]
;;

let%expect_test "resolver timeout exceptions are distinct from the driver's deadline" =
  Eio_main.run (fun env ->
    List.iter [ false; true ] ~f:(fun in_child ->
      let events = ref 0 in
      let auth ~sw _profile =
        if in_child
        then (
          Eio.Fiber.fork ~sw (fun () -> raise Eio.Time.Timeout);
          Eio.Fiber.await_cancel ())
        else raise Eio.Time.Timeout
      in
      let propagated =
        try
          ignore
            (D.run
               (Fixture.driver env)
               ~auth
               ~prepared:
                 (Fixture.prepare (Fixture.profile "http://127.0.0.1:1/v1/responses"))
               ~on_event:(fun _ -> incr events)
             : (D.Terminal.t, D.Auth.error) Result.t);
          false
        with
        | Eio.Time.Timeout -> true
      in
      printf "child:%b propagated:%b events:%d\n" in_child propagated !events));
  [%expect
    {|
    child:false propagated:true events:0
    child:true propagated:true events:0
  |}]
;;
