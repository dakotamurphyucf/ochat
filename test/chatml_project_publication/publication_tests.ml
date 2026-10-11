let%test_module "selected lock publication" =
  (module struct
    open Core
    module P = Project_prepare_publication
    module D = Project_prepare_deadline
    module R = Project_prepare_result
    module A = Project_prepare_accounting
    module E = Project_prepare_error
    module S = Chatml_source
    module B = Chatml_project_build
    module Fault = Crash_fault_io

    let old = lazy (Fixtures.make () |> Fixtures.compose |> Support.ok)

    let fresh =
      lazy (Fixtures.make ~outside_entry:true () |> Fixtures.compose |> Support.ok)
    ;;

    let source build = B.lock_source build
    let contents build = S.contents (source build)
    let candidate () = R.create (Lazy.force fresh) ~accounting:A.zero

    let ok = function
      | Ok value -> value
      | Error _ -> failwith "unexpected publication failure"
    ;;

    let deadline clock =
      D.create ~clock ~wall_time:(Time_ns.Span.of_sec 10.) |> Or_error.ok_exn
    ;;

    let with_directory f =
      Eio_main.run (fun env ->
        let random = Cstruct.create 16 in
        Eio.Flow.read_exact env#secure_random random;
        let suffix =
          Cstruct.to_string random
          |> String.to_list
          |> List.map ~f:(fun byte -> sprintf "%02x" (Char.to_int byte))
          |> String.concat
        in
        let path =
          Eio.Path.(Eio.Stdenv.fs env / "/tmp" / ("och204-publication-" ^ suffix))
        in
        Eio.Path.mkdir ~perm:0o700 path;
        Fun.protect
          ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree path))
          (fun () -> Eio.Path.with_open_dir path (fun directory -> f env directory)))
    ;;

    let write directory text =
      Agent_store.Durable_file.replace_in
        ~directory
        ~basename:"selected.json"
        ~durability:Flush_file_and_directory
        text
      |> ok
    ;;

    let read directory = Eio.Path.load Eio.Path.(directory / "selected.json")

    let open_owner
          ?(expected = P.Expected.Exact (source (Lazy.force old)))
          ?maximum
          directory
          sw
      =
      P.open_owned
        directory
        ~sw
        ~basename:"selected.json"
        ~expected
        ~max_lock_bytes:(Option.value maximum ~default:100_000)
      |> ok
    ;;

    let run ?(checkpoint = ignore) ?(prepare = fun () -> Ok (candidate ())) owner clock =
      P.run owner ~deadline:(deadline clock) ~clock ~checkpoint ~prepare
    ;;

    let assert_state owner expected = assert (P.equal_state (P.state owner) expected)

    let assert_problem result ~f =
      match result with
      | Error error -> assert (f (E.problem error))
      | Ok _ -> failwith "invalid publication succeeded"
    ;;

    let%expect_test
        "complete replacement is acknowledged; stale expectation stops preparation"
      =
      with_directory (fun env directory ->
        write directory (contents (Lazy.force old));
        Eio.Switch.run (fun sw ->
          let owner = open_owner directory sw in
          let completed = run owner env#mono_clock |> ok in
          assert (phys_equal (R.build completed) (Lazy.force fresh));
          assert (String.equal (read directory) (contents (Lazy.force fresh)));
          assert_state owner Available;
          printf "new lock: exact completed capture; state Available\n";
          write directory (contents (Lazy.force old));
          let called = ref false in
          run owner env#mono_clock ~prepare:(fun () ->
            called := true;
            Ok (candidate ()))
          |> assert_problem ~f:(function
            | Stale_selection -> true
            | _ -> false);
          assert (not !called);
          assert_state owner Available;
          printf "external change: Stale_selection; prepare calls 0\n"));
      [%expect
        {|
        new lock: exact completed capture; state Available
        external change: Stale_selection; prepare calls 0
        |}]
    ;;

    let%test_unit "exact ceilings, expected absence, stale reread and metadata admission" =
      with_directory (fun env directory ->
        let bytes = S.byte_length (source (Lazy.force fresh)) in
        List.iter
          [ bytes - 1; bytes ]
          ~f:(fun maximum ->
            Eio.Switch.run (fun sw ->
              let owner = open_owner directory sw ~expected:Absent ~maximum in
              let result = run owner env#mono_clock in
              if maximum < bytes
              then (
                result
                |> assert_problem ~f:(function
                  | Limit_exceeded -> true
                  | _ -> false);
                assert (not (Eio.Path.is_file Eio.Path.(directory / "selected.json"))))
              else (
                ignore (ok result : R.t);
                assert (String.equal (read directory) (contents (Lazy.force fresh))))));
        Eio.Switch.run (fun sw ->
          assert (
            Result.is_error
              (P.open_owned
                 directory
                 ~sw
                 ~basename:"../escape"
                 ~expected:Absent
                 ~max_lock_bytes:bytes));
          assert (
            Result.is_error
              (P.open_owned
                 directory
                 ~sw
                 ~basename:"selected.json"
                 ~expected:(Exact (source (Lazy.force fresh)))
                 ~max_lock_bytes:(bytes - 1)));
          let owner = open_owner directory sw in
          run owner env#mono_clock
          |> assert_problem ~f:(function
            | Stale_selection -> true
            | _ -> false);
          write directory (contents (Lazy.force old));
          run owner env#mono_clock ~prepare:(fun () ->
            write directory "external";
            Ok (candidate ()))
          |> assert_problem ~f:(function
            | Stale_selection -> true
            | _ -> false);
          assert (String.equal (read directory) "external");
          assert_state owner Available))
    ;;

    let fault_directory directory boundary reached =
      let resource, relative = directory in
      ( Fault.wrap_directory resource ~boundary ~reached ~matches:(fun path ->
          String.is_prefix path ~prefix:"selected.json.tmp-" || String.equal path ".")
      , relative )
    ;;

    let%test_unit "entered writer faults preserve full old or new lock and uncertainty" =
      List.iter
        [ Fault.After_create, false
        ; After_bytes 2, false
        ; Before_sync, false
        ; After_sync, false
        ; Before_rename, false
        ; After_rename, true
        ; Before_directory_sync, true
        ]
        ~f:(fun (boundary, renamed) ->
          with_directory (fun env directory ->
            write directory (contents (Lazy.force old));
            let hit = ref false in
            let wrapped =
              fault_directory directory boundary (fun path ->
                hit := true;
                raise (Core_unix.Unix_error (ENOSPC, "injected writer failure", path)))
            in
            Eio.Switch.run (fun sw ->
              let owner = open_owner wrapped sw in
              run owner env#mono_clock
              |> assert_problem ~f:(function
                | Publication_uncertain _ -> true
                | _ -> false);
              assert !hit;
              assert_state owner Uncertain;
              assert (
                String.equal
                  (read directory)
                  (contents (Lazy.force (if renamed then fresh else old))));
              let names = Eio.Path.read_dir directory in
              (match boundary with
               | After_create ->
                 (* open_out did not return, so the writer never admitted ownership.
                    Unknown temporaries must not be deleted automatically. *)
                 let residual =
                   List.filter names ~f:(fun name ->
                     not (String.equal name "selected.json"))
                 in
                 assert (List.length names = 2);
                 let name = List.hd_exn residual in
                 assert (
                   Option.equal
                     String.equal
                     (Agent_store.Durable_file.temporary_target name)
                     (Some "selected.json"));
                 assert (String.is_empty (Eio.Path.load Eio.Path.(directory / name)))
               | After_bytes _
               | Before_sync
               | After_sync
               | Before_rename
               | After_rename
               | Before_directory_sync ->
                 assert (List.equal String.equal names [ "selected.json" ]));
              let called = ref false in
              run owner env#mono_clock ~prepare:(fun () ->
                called := true;
                Ok (candidate ()))
              |> assert_problem ~f:(function
                | Publication_uncertain _ -> true
                | _ -> false);
              assert (not !called))))
    ;;

    let%test_unit "unexpected writer and checkpoint exceptions retain identity" =
      let exception Sentinel of unit ref in
      List.iter [ None; Some Fault.Before_sync; Some After_rename ] ~f:(fun boundary ->
        with_directory (fun env directory ->
          write directory (contents (Lazy.force old));
          let token = ref () in
          let wrapped =
            Option.value_map boundary ~default:directory ~f:(fun boundary ->
              fault_directory directory boundary (fun _ -> raise (Sentinel token)))
          in
          Eio.Switch.run (fun sw ->
            let owner = open_owner wrapped sw in
            let checkpoint () = if Option.is_none boundary then raise (Sentinel token) in
            (match run owner env#mono_clock ~checkpoint with
             | _ -> assert false
             | exception Sentinel observed -> assert (phys_equal token observed));
            assert_state owner (if Option.is_none boundary then Available else Uncertain))))
    ;;

    let%test_unit "reentrant and concurrent preparation reject without callback" =
      with_directory (fun env directory ->
        write directory (contents (Lazy.force old));
        Eio.Switch.run (fun sw ->
          let owner = open_owner directory sw in
          let entered, enter = Eio.Promise.create () in
          let released, release = Eio.Promise.create () in
          Eio.Fiber.both
            (fun () ->
               run owner env#mono_clock ~prepare:(fun () ->
                 let called = ref false in
                 run owner env#mono_clock ~prepare:(fun () ->
                   called := true;
                   Ok (candidate ()))
                 |> assert_problem ~f:(function
                   | Publication (Locked _) -> true
                   | _ -> false);
                 assert (not !called);
                 Eio.Promise.resolve enter ();
                 Eio.Promise.await released;
                 Ok (candidate ()))
               |> ok
               |> ignore)
            (fun () ->
               Eio.Promise.await entered;
               assert_state owner Busy;
               let called = ref false in
               run owner env#mono_clock ~prepare:(fun () ->
                 called := true;
                 Ok (candidate ()))
               |> assert_problem ~f:(function
                 | Publication (Locked _) -> true
                 | _ -> false);
               assert (not !called);
               Eio.Promise.resolve release ());
          assert_state owner Available))
    ;;

    let%test_unit "waiter cancellation joins; entered replacement is never acknowledged" =
      List.iter [ false; true ] ~f:(fun writing ->
        with_directory (fun env directory ->
          write directory (contents (Lazy.force old));
          let reached, reach = Eio.Promise.create () in
          let released, release = Eio.Promise.create () in
          let context, set_context = Eio.Promise.create () in
          let stopped, stop = Eio.Promise.create () in
          let token = Failure "waiter cancellation witness" in
          let joined = ref false in
          let wrapped =
            if writing
            then
              fault_directory directory After_rename (fun _ ->
                Eio.Promise.resolve reach ();
                Eio.Promise.await released;
                joined := true)
            else directory
          in
          Eio.Switch.run (fun sw ->
            let owner = open_owner wrapped sw in
            Eio.Fiber.both
              (fun () ->
                 (try
                    Eio.Cancel.sub (fun cc ->
                      Eio.Promise.resolve set_context cc;
                      run owner env#mono_clock ~prepare:(fun () ->
                        if writing
                        then Ok (candidate ())
                        else
                          Fun.protect
                            ~finally:(fun () -> joined := true)
                            (fun () ->
                               Eio.Promise.resolve reach ();
                               Eio.Promise.await released;
                               Ok (candidate ())))
                      |> ignore;
                      assert false)
                  with
                  | Eio.Cancel.Cancelled reason -> assert (phys_equal token reason));
                 assert !joined;
                 Eio.Promise.resolve stop ())
              (fun () ->
                 Eio.Promise.await reached;
                 Eio.Cancel.cancel (Eio.Promise.await context) token;
                 if writing then Eio.Promise.resolve release ();
                 Eio.Promise.await stopped);
            assert_state owner (if writing then Uncertain else Available);
            assert (
              String.equal
                (read directory)
                (contents (Lazy.force (if writing then fresh else old)))))))
    ;;

    let%test_unit "switch release joins borrowed-directory work and cannot reopen owner" =
      with_directory (fun env directory ->
        write directory (contents (Lazy.force old));
        Eio.Switch.run (fun outer ->
          let captured = ref None in
          let finished = ref false in
          let entered, enter = Eio.Promise.create () in
          let stopped, stop = Eio.Promise.create () in
          Eio.Switch.run (fun sw ->
            Eio.Switch.on_release sw (fun () ->
              assert !finished;
              assert_state (Option.value_exn !captured) Closed;
              assert (String.equal (read directory) (contents (Lazy.force old))));
            let owner = open_owner directory sw in
            captured := Some owner;
            Eio.Fiber.fork ~sw:outer (fun () ->
              (try
                 run owner env#mono_clock ~prepare:(fun () ->
                   Fun.protect
                     ~finally:(fun () -> finished := true)
                     (fun () ->
                        Eio.Promise.resolve enter ();
                        Eio.Fiber.await_cancel ()))
                 |> ignore
               with
               | Eio.Cancel.Cancelled _ -> ());
              Eio.Promise.resolve stop ());
            Eio.Promise.await entered);
          Eio.Promise.await stopped;
          let owner = Option.value_exn !captured in
          let called = ref false in
          run owner env#mono_clock ~prepare:(fun () ->
            called := true;
            Ok (candidate ()))
          |> assert_problem ~f:(function
            | Publication _ -> true
            | _ -> false);
          assert (not !called)))
    ;;

    let%test_unit "one immutable deadline; no postcommit checkpoint" =
      with_directory (fun _ directory ->
        let clock = Eio_mock.Clock.Mono.make () in
        let expired = deadline clock in
        Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 11_000_000_000L);
        Eio.Switch.run (fun sw ->
          let owner = open_owner directory sw ~expected:Absent in
          let called = ref false in
          P.run owner ~deadline:expired ~clock ~checkpoint:ignore ~prepare:(fun () ->
            called := true;
            Ok (candidate ()))
          |> assert_problem ~f:(function
            | Timed_out -> true
            | _ -> false);
          assert (not !called);
          assert (not (Eio.Path.is_file Eio.Path.(directory / "selected.json"))));
        let clock = Eio_mock.Clock.Mono.make () in
        let writing = ref false in
        let wrapped =
          fault_directory directory After_rename (fun _ ->
            writing := true;
            Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 11_000_000_000L))
        in
        Eio.Switch.run (fun sw ->
          let owner = open_owner wrapped sw ~expected:Absent in
          run owner clock ~checkpoint:(fun () -> assert (not !writing)) |> ok |> ignore;
          assert !writing;
          assert_state owner Available))
    ;;

    let%test_unit "checkpoint cancellation at the final poll never enters the writer" =
      let calls = ref 0 in
      with_directory (fun env directory ->
        write directory (contents (Lazy.force old));
        Eio.Switch.run (fun sw ->
          let owner = open_owner directory sw in
          run owner env#mono_clock ~checkpoint:(fun () -> incr calls) |> ok |> ignore));
      let last_poll = !calls in
      assert (last_poll > 0);
      with_directory (fun env directory ->
        write directory (contents (Lazy.force old));
        let writer_entered = ref false in
        let wrapped =
          fault_directory directory After_create (fun _ -> writer_entered := true)
        in
        let reason = Failure "synchronous checkpoint cancellation" in
        let observed = ref 0 in
        Eio.Switch.run (fun sw ->
          let owner = open_owner wrapped sw in
          let checkpoint () =
            incr observed;
            if !observed = last_poll
            then (
              let fiber = Effect.perform Eio.Private.Effects.Get_context in
              Eio.Cancel.cancel
                (Eio.Private.Fiber_context.cancellation_context fiber)
                reason)
          in
          (match run owner env#mono_clock ~checkpoint with
           | _ -> assert false
           | exception Eio.Cancel.Cancelled actual -> assert (phys_equal reason actual));
          assert (!observed = last_poll);
          assert (not !writer_entered);
          assert_state owner Available;
          assert (String.equal (read directory) (contents (Lazy.force old)));
          assert (
            List.equal String.equal (Eio.Path.read_dir directory) [ "selected.json" ])))
    ;;

    let%test_unit "a closing switch cannot acknowledge a protected completed writer" =
      with_directory (fun env directory ->
        write directory (contents (Lazy.force old));
        Eio.Switch.run (fun outer ->
          let entered, enter = Eio.Promise.create () in
          let released, release = Eio.Promise.create () in
          let outcome, resolve_outcome = Eio.Promise.create () in
          let wrapped =
            fault_directory directory After_rename (fun _ ->
              Eio.Promise.resolve enter ();
              Eio.Promise.await released)
          in
          let reason = Failure "owner switch closing" in
          let retained = ref None in
          (match
             Eio.Switch.run (fun sw ->
               let owner = open_owner wrapped sw in
               retained := Some owner;
               Eio.Fiber.fork ~sw:outer (fun () ->
                 Eio.Promise.resolve resolve_outcome (run owner env#mono_clock));
               Eio.Promise.await entered;
               Eio.Switch.fail sw reason;
               assert_state owner Closed;
               Eio.Promise.resolve release ())
           with
           | () -> assert false
           | exception observed -> assert (phys_equal reason observed));
          Eio.Promise.await outcome
          |> assert_problem ~f:(function
            | Publication _ -> true
            | _ -> false);
          assert_state (Option.value_exn !retained) Closed;
          assert (String.equal (read directory) (contents (Lazy.force fresh)))))
    ;;

    let%test_unit "deadline rejects invalid spans and expires at the exact endpoint" =
      let clock = Eio_mock.Clock.Mono.make () in
      assert (Result.is_error (D.create ~clock ~wall_time:Time_ns.Span.zero));
      assert (Result.is_error (D.create ~clock ~wall_time:(Time_ns.Span.of_sec (-1.))));
      let d = deadline clock in
      Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 9_999_999_999L);
      ignore (D.check d ~clock |> ok : unit);
      Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 10_000_000_000L);
      D.check d ~clock
      |> assert_problem ~f:(function
        | Timed_out -> true
        | _ -> false)
    ;;

    let%test_unit "bounded reads and preparation expiry fail before writer effects" =
      with_directory (fun env directory ->
        let old_bytes = S.byte_length (source (Lazy.force old)) in
        write directory (String.make (old_bytes + 1) 'x');
        let called = ref false in
        Eio.Switch.run (fun sw ->
          let owner = open_owner directory sw ~maximum:old_bytes in
          run owner env#mono_clock ~prepare:(fun () ->
            called := true;
            Ok (candidate ()))
          |> assert_problem ~f:(function
            | Publication (Document (Limit_exceeded _)) -> true
            | _ -> false);
          assert (not !called);
          assert_state owner Available));
      with_directory (fun _ directory ->
        write directory (contents (Lazy.force old));
        let clock = Eio_mock.Clock.Mono.make () in
        let writer_entered = ref false in
        let wrapped =
          fault_directory directory After_create (fun _ -> writer_entered := true)
        in
        Eio.Switch.run (fun sw ->
          let owner = open_owner wrapped sw in
          run owner clock ~prepare:(fun () ->
            Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 10_000_000_000L);
            Ok (candidate ()))
          |> assert_problem ~f:(function
            | Timed_out -> true
            | _ -> false);
          assert (not !writer_entered);
          assert_state owner Available;
          assert (String.equal (read directory) (contents (Lazy.force old)))))
    ;;
  end)
;;
