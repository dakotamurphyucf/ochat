open! Core
module Lifetime = Native_resource_lifetime
module Cleanup = Independent_cleanup

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let with_backtraces f =
  let recording = Stdlib.Printexc.backtrace_status () in
  Stdlib.Printexc.record_backtrace true;
  Exn.protect ~finally:(fun () -> Stdlib.Printexc.record_backtrace recording) ~f
;;

let%expect_test "native child joins work before classifying actual release cancellation" =
  let failed_switch_child ~env ~sw =
    let captured, captured_u = Eio.Promise.create () in
    let entered, entered_u = Eio.Promise.create () in
    let cleaning, cleaning_u = Eio.Promise.create () in
    let released, released_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let failure = Failure "642 actual failed unfinished native parent Switch" in
    let original =
      Eio.Fiber.fork_promise ~sw (fun () ->
        Eio.Switch.run_reported (fun failed ->
          Eio.Promise.resolve captured_u failed;
          Eio.Fiber.fork ~sw:failed (fun () ->
            Exn.protect
              ~finally:(fun () ->
                Eio.Cancel.protect (fun () ->
                  Eio.Promise.resolve cleaning_u ();
                  Eio.Promise.await released))
              ~f:(fun () ->
                Eio.Promise.resolve entered_u ();
                Eio.Promise.await never));
          Eio.Promise.await entered;
          raise failure))
    in
    let finish () =
      Eio.Cancel.protect (fun () ->
        if Option.is_none (Eio.Promise.peek released)
        then Eio.Promise.resolve released_u ();
        let outcome = Eio.Promise.await_exn original in
        match Eio.Switch.Outcome.result outcome with
        | Error (actual, _) -> assert (phys_equal actual failure)
        | Ok _ -> failwith "failed native parent Switch unexpectedly succeeded")
    in
    Exn.protect ~finally:finish ~f:(fun () ->
      let failed = Eio.Promise.await captured in
      Eio.Promise.await cleaning;
      assert (Option.is_none (Eio.Promise.peek original));
      let captures = ref 0 in
      Eio.Cancel.protect (fun () ->
        (* The inner timeout only bounds a missing-check regression. The calling
           parent remains healthy/protected and the supplied Switch is genuinely
           failed but unfinished behind the actual release gate. *)
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 1. (fun () ->
          match
            Lifetime.Child.create ~sw:failed ~capture:(fun _ -> Int.incr captures)
          with
          | Error _ | Ok _ -> failwith "failed Switch fabricated a native child"
          | exception Eio.Cancel.Cancelled cause -> assert (phys_equal cause failure)));
      [%test_eq: int] 0 !captures;
      assert (not (Eio.Fiber.is_cancelled ())))
  in
  let child_release_parts ~sw =
    let module CO = Agent_session.Close_observation in
    let module Diagnostic = Agent_session.Cleanup_diagnostic in
    let lifetime = Lifetime.create ~sw in
    let child =
      Lifetime.Child.create ~sw ~capture:(Lifetime.capture_child lifetime) |> protocol_ok
    in
    let ordinary = Failure "642 actual native child release" in
    let cancelled = Eio.Cancel.Cancelled (Failure "642 actual native child hook") in
    let actual_parts = ref [] in
    let hook exn () =
      try raise exn with
      | actual ->
        let trace = Stdlib.Printexc.get_raw_backtrace () in
        actual_parts := (actual, trace) :: !actual_parts;
        Exn.raise_with_original_backtrace actual trace
    in
    Eio.Switch.on_release (Lifetime.Child.switch child) (hook cancelled);
    Eio.Switch.on_release (Lifetime.Child.switch child) (hook ordinary);
    let first = Lifetime.close_local_observed lifetime in
    (match CO.exit first with
     | Raised (actual, _) -> assert (phys_equal actual ordinary)
     | Returned _ -> failwith "native child lost its selected release failure");
    let group = Option.value_exn (CO.primary first) in
    assert (List.equal Diagnostic.equal (CO.diagnostics first) [ group ]);
    let completed_parts = List.rev !actual_parts in
    [%test_eq: int] 2 (List.length completed_parts);
    (match Diagnostic.failures group with
     | Raised (selected, _) :: supporting ->
       assert (phys_equal selected ordinary);
       List.iter2_exn completed_parts supporting ~f:(fun (exn, trace) part ->
         match part with
         | Raised (actual, actual_trace) ->
           assert (phys_equal actual exn);
           let original = Stdlib.Printexc.raw_backtrace_to_string trace in
           assert (not (String.is_empty original));
           assert (
             String.is_prefix
               (Stdlib.Printexc.raw_backtrace_to_string actual_trace)
               ~prefix:original)
         | Protocol _ -> failwith "native child replaced an actual release part")
     | [] | Protocol _ :: _ -> failwith "native child lost its actual Switch boundary");
    let repeated = Lifetime.close_local_observed lifetime in
    assert (List.equal Diagnostic.equal (CO.diagnostics repeated) [ group ]);
    assert (Option.equal Diagnostic.equal (CO.primary repeated) (Some group));
    [%test_eq: int] 2 (List.length !actual_parts)
  in
  with_backtraces (fun () ->
    List.iter [ false; true ] ~f:(fun hook_fails ->
      Eio_main.run (fun env ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
          Eio.Switch.run (fun sw ->
            if not hook_fails
            then (
              failed_switch_child ~env ~sw;
              child_release_parts ~sw);
            let lifetime = Lifetime.create ~sw in
            let child =
              Lifetime.Child.create ~sw ~capture:(Lifetime.capture_child lifetime)
              |> protocol_ok
            in
            let entered, entered_u = Eio.Promise.create () in
            let never, _ = Eio.Promise.create () in
            let joined = ref false
            and releases = ref 0 in
            Eio.Fiber.fork ~sw:(Lifetime.Child.switch child) (fun () ->
              Exn.protect
                ~finally:(fun () -> joined := true)
                ~f:(fun () ->
                  Eio.Promise.resolve entered_u ();
                  Eio.Promise.await never));
            Eio.Promise.await entered;
            let hook_failure = Eio.Cancel.Cancelled Exit in
            let original_trace = ref None in
            Eio.Switch.on_release (Lifetime.Child.switch child) (fun () ->
              assert !joined;
              Int.incr releases;
              if hook_fails
              then (
                try raise hook_failure with
                | exn ->
                  let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                  original_trace := Some backtrace;
                  Exn.raise_with_original_backtrace exn backtrace));
            let close () =
              match Lifetime.close_local lifetime with
              | Ok () when not hook_fails -> ()
              | Ok () -> failwith "requested cancellation hid actual release failure"
              | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
              | exception (Eio.Cancel.Cancelled Exit as actual) ->
                assert hook_fails;
                let observed = Stdlib.Printexc.get_raw_backtrace () in
                assert (phys_equal actual hook_failure);
                let original = Option.value_exn !original_trace in
                let prefix = Stdlib.Printexc.raw_backtrace_to_string original in
                assert (not (String.is_empty prefix));
                assert (
                  String.is_prefix
                    (Stdlib.Printexc.raw_backtrace_to_string observed)
                    ~prefix)
            in
            close ();
            close ();
            assert !joined;
            [%test_eq: int] 1 !releases;
            print_s [%sexp { hook_fails : bool; joined = true; release_attempts = 1 }])))));
  [%expect
    {|
    ((hook_fails false) (joined true) (release_attempts 1))
    ((hook_fails true) (joined true) (release_attempts 1))
    |}]
;;

exception Independent_script_failure

let%expect_test "failed or cancelled Independent script retains positive actual cleanup" =
  with_backtraces (fun () ->
    List.iter [ false; true ] ~f:(fun cancelled ->
      Eio_main.run (fun env ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
          Eio.Cancel.sub (fun cancel ->
            let cleanup = Cleanup.create () in
            let joined = ref false
            and releases = ref 0 in
            let outcome =
              Cleanup.run cleanup ~f:(fun ~sw ~register_tracking:_ ->
                let entered, entered_u = Eio.Promise.create () in
                let never, _ = Eio.Promise.create () in
                Eio.Fiber.fork ~sw (fun () ->
                  Exn.protect
                    ~finally:(fun () -> joined := true)
                    ~f:(fun () ->
                      Eio.Promise.resolve entered_u ();
                      Eio.Promise.await never));
                Eio.Promise.await entered;
                Eio.Switch.on_release sw (fun () ->
                  assert !joined;
                  Int.incr releases);
                if cancelled
                then (
                  Eio.Cancel.cancel cancel Exit;
                  Eio.Fiber.check ());
                raise Independent_script_failure)
            in
            (match cancelled, Cleanup.Outcome.result outcome with
             | false, Error (Cleanup.Failure.Raised (Independent_script_failure, _))
             | true, Error (Raised (Eio.Cancel.Cancelled Exit, _)) -> ()
             | _ -> failwith "Independent source primary changed");
            let original_report = Cleanup.Outcome.cleanup outcome in
            [%test_eq: Cleanup.Report.status]
              Joined
              (Cleanup.Report.status original_report);
            assert (List.is_empty (Cleanup.Report.failures original_report));
            Cleanup.retry cleanup |> protocol_ok;
            [%test_eq: Cleanup.Report.status]
              Joined
              (Cleanup.Report.status original_report);
            assert !joined;
            [%test_eq: int] 1 !releases;
            print_s
              [%sexp { cancelled : bool; cleanup_joined = true; release_attempts = 1 }])))));
  [%expect
    {|
    ((cancelled false) (cleanup_joined true) (release_attempts 1))
    ((cancelled true) (cleanup_joined true) (release_attempts 1))
    |}]
;;
