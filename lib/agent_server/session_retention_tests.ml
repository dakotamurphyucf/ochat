open! Core
module R = Session_retention
module P = Agent_protocol

let fixture f =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      Eio.Switch.run (fun sw -> f env sw)))
;;

let%test_unit "retention coalesces before binding and keeps a new in-flight request" =
  fixture (fun env sw ->
    let retention = R.create () in
    for _ = 1 to 100 do
      R.request retention
    done;
    let entered, enter = Eio.Promise.create () in
    let released, release = Eio.Promise.create () in
    let completed, complete = Eio.Promise.create () in
    let calls = ref 0 in
    R.bind_exn retention ~sw ~clock:(Eio.Stdenv.mono_clock env) ~attempt:(fun () ->
      Int.incr calls;
      (match !calls with
       | 1 ->
         Eio.Promise.resolve enter ();
         Eio.Promise.await released
       | 2 -> Eio.Promise.resolve complete ()
       | _ -> failwith "coalesced request spawned an extra attempt");
      Ok R.Completed);
    Eio.Promise.await entered;
    for _ = 1 to 100 do
      R.request retention
    done;
    Eio.Promise.resolve release ();
    Eio.Promise.await completed;
    R.close_and_wait retention;
    [%test_result: int] !calls ~expect:2;
    R.request retention;
    R.close_and_wait retention;
    assert (Option.is_none (R.last_error retention)))
;;

let%test_unit "retention retries deferred and failed work without another snapshot" =
  fixture (fun env sw ->
    let retention = R.create () in
    let failed, failure = Eio.Promise.create () in
    let completed, complete = Eio.Promise.create () in
    let calls = ref 0 in
    R.bind_exn retention ~sw ~clock:(Eio.Stdenv.mono_clock env) ~attempt:(fun () ->
      Int.incr calls;
      match !calls with
      | 1 -> Ok R.Deferred
      | 2 ->
        Eio.Promise.resolve failure ();
        Error
          (P.Error.create Internal_error ~message:"retention fixture" ~retryable:true ())
      | 3 ->
        Eio.Promise.resolve complete ();
        Ok R.Completed
      | _ -> failwith "completed retention kept retrying");
    R.request retention;
    Eio.Promise.await failed;
    assert (
      Option.exists (R.last_error retention) ~f:(fun error ->
        P.Error.equal_code error.code Internal_error));
    Eio.Promise.await completed;
    R.close_and_wait retention;
    [%test_result: int] !calls ~expect:3;
    assert (Option.is_none (R.last_error retention)))
;;

let%test_unit "retention close joins actual work despite caller cancellation" =
  fixture (fun env sw ->
    let retention = R.create () in
    let entered, enter = Eio.Promise.create () in
    let released, release = Eio.Promise.create () in
    let closing, close = Eio.Promise.create () in
    let finished, finish = Eio.Promise.create () in
    R.bind_exn retention ~sw ~clock:(Eio.Stdenv.mono_clock env) ~attempt:(fun () ->
      Eio.Promise.resolve enter ();
      Eio.Promise.await released;
      Ok R.Completed);
    R.request retention;
    Eio.Promise.await entered;
    Eio.Fiber.fork ~sw (fun () ->
      Eio.Cancel.sub (fun context ->
        Eio.Promise.resolve close context;
        let cancelled =
          try
            R.close_and_wait retention;
            Eio.Fiber.check ();
            false
          with
          | Eio.Cancel.Cancelled _ -> true
        in
        Eio.Promise.resolve finish cancelled));
    Eio.Cancel.cancel (Eio.Promise.await closing) Exit;
    Eio.Fiber.yield ();
    assert (not (Eio.Promise.is_resolved finished));
    Eio.Promise.resolve release ();
    assert (Eio.Promise.await finished);
    R.close_and_wait retention)
;;

let%test_unit "retention terminal exception is joined and reported once" =
  fixture (fun env sw ->
    let retention = R.create () in
    let entered, enter = Eio.Promise.create () in
    let marker = Failure "exact retention fixture failure" in
    R.bind_exn retention ~sw ~clock:(Eio.Stdenv.mono_clock env) ~attempt:(fun () ->
      Eio.Promise.resolve enter ();
      raise marker);
    R.request retention;
    Eio.Promise.await entered;
    let observed =
      try
        R.close_and_wait retention;
        false
      with
      | exn -> phys_equal exn marker
    in
    assert observed;
    R.close_and_wait retention;
    R.request retention;
    let foreign_denied =
      Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
        try
          R.request retention;
          false
        with
        | Invalid_argument _ -> true)
    in
    assert foreign_denied;
    let unbound = R.create () in
    R.request unbound;
    R.close_and_wait unbound;
    R.close_and_wait unbound)
;;
