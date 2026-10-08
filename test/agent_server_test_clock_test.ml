open! Core
open Eio.Std

let pump () =
  for _ = 1 to 32 do
    Fiber.yield ()
  done
;;

let set_time clock seconds =
  let nanoseconds = Int64.(of_int seconds * 1_000_000_000L) in
  Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns nanoseconds);
  pump ()
;;

let timeout ~sw clock seconds =
  let result = ref None in
  let never, _ = Promise.create () in
  Fiber.fork ~sw (fun () ->
    result
    := Some
         (try
            Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock seconds) (fun () ->
              Promise.await never);
            false
          with
          | Eio.Time.Timeout -> true));
  pump ();
  result
;;

let logical_seconds clock =
  Eio.Time.Mono.now clock
  |> Mtime.to_uint64_ns
  |> Int64.(fun value -> value / 1_000_000_000L)
;;

let%expect_test "paused logical clock does not complete an already armed timeout" =
  Eio_main.run (fun _ ->
    Switch.run (fun sw ->
      let real = Eio_mock.Clock.Mono.make () in
      let clock, pause, _, advance =
        Agent_server_test_support.controlled_monotonic_clock real
      in
      let result = timeout ~sw clock 10. in
      set_time real 3;
      pause ();
      pump ();
      assert (not (Eio_mock.Clock.Mono.try_advance real));
      set_time real 100;
      assert (Option.is_none !result);
      [%test_eq: int64] 3L (logical_seconds clock);
      (* Explicit advancement still satisfies an existing wait while paused. *)
      advance 6.;
      pump ();
      assert (Option.is_none !result);
      advance 1.;
      pump ();
      [%test_eq: bool option] (Some true) !result));
  [%expect
    {|
    +mock time is now 3
    +mock time is now 100
    |}]
;;

let%expect_test "resuming re-arms the remaining logical timeout for all sleepers" =
  Eio_main.run (fun _ ->
    Switch.run (fun sw ->
      let real = Eio_mock.Clock.Mono.make () in
      let clock, pause, resume, _ =
        Agent_server_test_support.controlled_monotonic_clock real
      in
      let first = timeout ~sw clock 5.
      and second = timeout ~sw clock 10. in
      set_time real 2;
      pause ();
      pump ();
      assert (not (Eio_mock.Clock.Mono.try_advance real));
      set_time real 20;
      assert (Option.is_none !first && Option.is_none !second);
      resume ();
      pump ();
      set_time real 22;
      assert (Option.is_none !first && Option.is_none !second);
      set_time real 23;
      [%test_eq: bool option] (Some true) !first;
      assert (Option.is_none !second);
      set_time real 27;
      assert (Option.is_none !second);
      set_time real 28;
      [%test_eq: bool option] (Some true) !second;
      [%test_eq: int64] 10L (logical_seconds clock)));
  [%expect
    {|
    +mock time is now 2
    +mock time is now 20
    +mock time is now 22
    +mock time is now 23
    +mock time is now 27
    +mock time is now 28
    |}]
;;

let wall_wait ~sw clock deadline =
  let completed = ref false in
  Fiber.fork ~sw (fun () ->
    Eio.Time.sleep_until clock deadline;
    completed := true);
  pump ();
  completed
;;

let set_wall_time clock seconds =
  Eio_mock.Clock.set_time clock seconds;
  pump ()
;;

let%expect_test "wall clock pause and resume cancel an already armed real wait" =
  Eio_main.run (fun _ ->
    Switch.run (fun sw ->
      let real = Eio_mock.Clock.make () in
      let clock, pause, resume, _ =
        Agent_server_test_support.controlled_wall_clock real ~initial:0.
      in
      let completed = wall_wait ~sw clock 31. in
      set_wall_time real 3.;
      pause ();
      pump ();
      set_wall_time real 20.;
      resume ();
      pump ();
      set_wall_time real 31.;
      assert (not !completed);
      [%test_eq: float] 0. (Eio.Time.now clock);
      set_wall_time real 50.;
      assert (not !completed);
      set_wall_time real 51.;
      assert !completed;
      [%test_eq: float] 31. (Eio.Time.now clock)));
  [%expect
    {|
    +mock time is now 3
    +mock time is now 20
    +mock time is now 31
    +mock time is now 50
    +mock time is now 51
    |}]
;;

let%expect_test "paused wall waits block until explicit logical advancement" =
  Eio_main.run (fun _ ->
    Switch.run (fun sw ->
      let real = Eio_mock.Clock.make () in
      let clock, pause, _, advance_to =
        Agent_server_test_support.controlled_wall_clock real ~initial:0.
      in
      let completed = wall_wait ~sw clock 31. in
      pause ();
      pump ();
      assert (not (Eio_mock.Clock.try_advance real));
      set_wall_time real 100.;
      assert (not !completed);
      advance_to 30.;
      pump ();
      assert (not !completed);
      advance_to 31.;
      pump ();
      assert !completed;
      [%test_eq: float] 31. (Eio.Time.now clock)));
  [%expect {| +mock time is now 100 |}]
;;

let%expect_test "concurrent wall waits advance to deadlines without adding spans" =
  Eio_main.run (fun _ ->
    Switch.run (fun sw ->
      let real = Eio_mock.Clock.make () in
      let clock, _, _, _ =
        Agent_server_test_support.controlled_wall_clock real ~initial:0.
      in
      let first = wall_wait ~sw clock 5.
      and second = wall_wait ~sw clock 10. in
      set_wall_time real 5.;
      assert (!first && not !second);
      [%test_eq: float] 5. (Eio.Time.now clock);
      set_wall_time real 9.;
      assert (not !second);
      set_wall_time real 10.;
      assert !second;
      [%test_eq: float] 10. (Eio.Time.now clock)));
  [%expect
    {|
    +mock time is now 5
    +mock time is now 9
    +mock time is now 10
    |}]
;;

let%expect_test "a long completed real wait preserves earlier logical deadlines" =
  Eio_main.run (fun _ ->
    Switch.run (fun sw ->
      let real = Eio_mock.Clock.make () in
      let clock, _, _, _ =
        Agent_server_test_support.controlled_wall_clock real ~initial:0.
      in
      let long = wall_wait ~sw clock 31. in
      set_wall_time real 30.;
      let short = wall_wait ~sw clock 5. in
      set_wall_time real 31.;
      assert (!short && not !long);
      [%test_eq: float] 5. (Eio.Time.now clock);
      set_wall_time real 56.;
      assert (not !long);
      set_wall_time real 57.;
      assert !long;
      [%test_eq: float] 31. (Eio.Time.now clock)));
  [%expect
    {|
    +mock time is now 30
    +mock time is now 31
    +mock time is now 56
    +mock time is now 57
    |}]
;;

let%expect_test "cancelling a shorter wall wait removes its logical deadline" =
  Eio_main.run (fun _ ->
    Switch.run (fun sw ->
      let real = Eio_mock.Clock.make () in
      let clock, _, _, _ =
        Agent_server_test_support.controlled_wall_clock real ~initial:0.
      in
      let long = wall_wait ~sw clock 31. in
      set_wall_time real 30.;
      let cancel, resolver = Promise.create () in
      let cancelled = ref false in
      Fiber.fork ~sw (fun () ->
        Fiber.first
          (fun () -> Eio.Time.sleep_until clock 5.)
          (fun () -> Promise.await cancel);
        cancelled := true);
      pump ();
      Promise.resolve resolver ();
      pump ();
      assert !cancelled;
      [%test_eq: float] 0. (Eio.Time.now clock);
      set_wall_time real 31.;
      assert !long;
      [%test_eq: float] 31. (Eio.Time.now clock)));
  [%expect
    {|
    +mock time is now 30
    +mock time is now 31
    |}]
;;
