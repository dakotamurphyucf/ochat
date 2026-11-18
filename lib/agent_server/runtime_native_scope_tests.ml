open! Core
module P = Agent_protocol
module Scope = Runtime_native_scope

let%expect_test "native preparation failure retains cleanup until an explicit retry" =
  let module CO = Agent_session.Close_observation in
  let module D = Agent_session.Cleanup_diagnostic in
  List.iter [ false; true ] ~f:(fun observed ->
    Eio_main.run (fun env ->
      Eio.Time.with_timeout_exn env#clock 10. (fun () ->
        Eio.Switch.run (fun sw ->
          let scope = Scope.create ~sw in
          let events = ref [] in
          let record event = events := event :: !events in
          let failure message = P.Error.create Conflict ~message ~retryable:true () in
          let expect_error expected = function
            | Error error -> [%test_eq: string] expected error.P.Error.message
            | Ok _ -> failwith "expected the original failure"
          in
          let expect_ok = function
            | Ok () -> ()
            | Error error -> raise_s [%sexp (error : P.Error.t)]
          in
          let driver_has_exited () =
            match Scope.driver_has_exited scope with
            | Ok exited -> exited
            | Error error -> raise_s [%sexp (error : P.Error.t)]
          in
          let preparation_group = ref None in
          let cleanup_group = ref None in
          let original_error = ref None in
          let cleanup_error = ref None in
          let observe group result =
            match result with
            | Ok _ -> CO.create (Returned result) ~diagnostics:[] ()
            | Error error ->
              let diagnostic = D.create [ Protocol error ] in
              assert (Option.is_none !group);
              group := Some diagnostic;
              CO.create
                (Returned result)
                ~diagnostics:[ diagnostic ]
                ~primary:diagnostic
                ()
          in
          let entered, enter = Eio.Promise.create () in
          let released, release = Eio.Promise.create () in
          let release_close () =
            if Option.is_none (Eio.Promise.peek released)
            then Eio.Promise.resolve release ()
          in
          let attempts = ref 0 in
          let prepare () =
            record "preparation failed";
            let error = failure "original preparation" in
            original_error := Some error;
            Error error
          in
          let close_attempt () =
            incr attempts;
            if !attempts = 1
            then (
              record "cleanup failed";
              if observed
              then (
                Eio.Promise.resolve enter ();
                Eio.Promise.await released);
              let error = failure "first cleanup" in
              cleanup_error := Some error;
              Error error)
            else (
              record "cleanup succeeded";
              Ok ())
          in
          let preparation =
            if observed
            then (
              let actual =
                Scope.prepare_observed scope ~f:(fun ~retain ->
                  record "entered lifetime";
                  Exn.protect
                    ~finally:(fun () -> record "left lifetime")
                    ~f:(fun () ->
                      retain
                        ~prepare:(fun () -> observe preparation_group (prepare ()))
                        ~close_attempt:(fun () ->
                          observe cleanup_group (close_attempt ()))))
              in
              let group = Option.value_exn !preparation_group in
              assert (List.equal D.equal (CO.diagnostics actual) [ group ]);
              assert (Option.equal D.equal (CO.primary actual) (Some group));
              (match CO.exit actual with
               | Returned (Error error) ->
                 assert (phys_equal error (Option.value_exn !original_error))
               | Returned (Ok _) | Raised _ ->
                 failwith "observed preparation changed the actual original failure");
              Some actual)
            else (
              Scope.prepare scope ~f:(fun ~retain ->
                record "entered lifetime";
                Exn.protect
                  ~finally:(fun () -> record "left lifetime")
                  ~f:(fun () -> retain ~prepare ~close_attempt))
              |> expect_error "original preparation";
              None)
          in
          [%test_eq: string list]
            [ "entered lifetime"; "preparation failed" ]
            (List.rev !events);
          let failed =
            if observed
            then (
              let first, joined =
                Exn.protect ~finally:release_close ~f:(fun () ->
                  let first =
                    Eio.Fiber.fork_promise ~sw (fun () -> Scope.close_observed scope)
                  in
                  Eio.Promise.await entered;
                  let joining, join = Eio.Promise.create () in
                  let joined =
                    Eio.Fiber.fork_promise ~sw (fun () ->
                      Eio.Promise.resolve join ();
                      Scope.close_observed scope)
                  in
                  Eio.Promise.await joining;
                  Eio.Fiber.yield ();
                  assert (Option.is_none (Eio.Promise.peek first));
                  assert (Option.is_none (Eio.Promise.peek joined));
                  [%test_eq: int] 1 !attempts;
                  release_close ();
                  Eio.Promise.await_exn first, Eio.Promise.await_exn joined)
              in
              let group = Option.value_exn !cleanup_group in
              List.iter [ first; joined ] ~f:(fun actual ->
                assert (List.equal D.equal (CO.diagnostics actual) [ group ]);
                assert (Option.equal D.equal (CO.primary actual) (Some group));
                match CO.exit actual with
                | Returned (Error error) ->
                  assert (phys_equal error (Option.value_exn !cleanup_error))
                | Returned (Ok ()) | Raised _ ->
                  failwith "shared native close request changed its actual failure");
              Some first)
            else (
              Scope.close scope |> expect_error "first cleanup";
              None)
          in
          [%test_eq: bool] false (driver_has_exited ());
          [%test_eq: int] 1 !attempts;
          [%test_eq: string list]
            [ "entered lifetime"; "preparation failed"; "cleanup failed" ]
            (List.rev !events);
          if observed
          then (
            let closed = Scope.close_observed scope in
            (match CO.exit closed with
             | Returned (Ok ()) -> ()
             | Returned (Error error) -> raise_s [%sexp (error : P.Error.t)]
             | Raised (exn, trace) -> Exn.raise_with_original_backtrace exn trace);
            let original = Option.value_exn !preparation_group in
            assert (List.equal D.equal (CO.diagnostics closed) [ original ]);
            assert (Option.is_none (CO.primary closed));
            let driver = Scope.driver_observation scope in
            assert (List.equal D.equal (CO.diagnostics driver) [ original ]);
            assert (Option.equal D.equal (CO.primary driver) (Some original));
            (match CO.exit driver with
             | Returned (Error error) ->
               assert (phys_equal error (Option.value_exn !original_error))
             | Returned (Ok ()) | Raised _ ->
               failwith "joined local cleanup erased original preparation failure");
            let failed = Option.value_exn failed in
            assert (
              List.equal
                D.equal
                (CO.diagnostics failed)
                [ Option.value_exn !cleanup_group ]);
            let preparation = Option.value_exn preparation in
            assert (List.equal D.equal (CO.diagnostics preparation) [ original ]))
          else Scope.close scope |> expect_ok;
          [%test_eq: bool] true (driver_has_exited ());
          Scope.driver_result scope |> expect_error "original preparation";
          Scope.close scope |> expect_ok;
          [%test_eq: int] 2 !attempts;
          assert (Result.is_error (Scope.check_open scope));
          if observed then print_s [%sexp (List.rev !events : string list)]))));
  [%expect
    {|
    ("entered lifetime" "preparation failed" "cleanup failed" "cleanup succeeded"
     "left lifetime")
    |}]
;;
