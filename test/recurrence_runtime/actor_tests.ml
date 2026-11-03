open! Core
open Support

let%expect_test
    "actual ACK pairs phase with durable cursor under failed save and wall rollback"
  =
  with_store (fun fixture ->
    let actor = create_actor fixture ~sw:fixture.sw () in
    Exn.protect
      ~finally:(fun () ->
        fixture.reject := false;
        A.Session_actor.shutdown actor)
      ~f:(fun () ->
        let host = open_host fixture () in
        Exn.protect
          ~finally:(fun () -> Host.close_and_wait host)
          ~f:(fun () ->
            install actor host;
            let enable =
              create
                actor
                "rate"
                (interval ~first_due:(P.Timestamp.to_time_ns (wall_at_ms 1000)) ())
            in
            let before = state actor in
            fixture.reject := true;
            fixture.elapsed_ms := 3000;
            fixture.wall := wall_at_ms (-3_600_000);
            assert (Result.is_error (A.Session_actor.sweep_recurrences actor));
            assert (equal_state before (state actor));
            fixture.reject := false;
            (* First successful plan adopts the original enable pair; the next
           bounded sweep uses elapsed time, despite the backwards wall. *)
            sweep actor;
            assert (Int64.equal (consumed actor "rate") 0L);
            sweep actor;
            let expected = Int64.of_int (!(fixture.elapsed_ms) / 1000) in
            assert (Int64.equal (consumed actor "rate") expected);
            let selected = List.hd_exn (H.ready (frontier actor "rate")) in
            assert (
              Int64.equal
                (A.Recurrence_identity.Occurrence.slot
                   (A.Recurrence_nominal.key selected))
                Int64.(expected - 1L));
            let unchanged = (state actor).counters.transaction_sequence in
            sweep actor;
            assert (Int64.equal unchanged (state actor).counters.transaction_sequence);
            call actor enable |> ignore;
            assert (Int64.equal unchanged (state actor).counters.transaction_sequence);
            mutate actor "already-enabled" (Enable (id "rate")) |> ignore;
            fixture.elapsed_ms := 4000;
            fixture.wall := wall_at_ms (-7_200_000);
            sweep actor;
            assert (Int64.equal (consumed actor "rate") 4L);
            let first_due =
              Time_ns.add
                (P.Timestamp.to_time_ns !(fixture.wall))
                (Time_ns.Span.of_int_ms 1000)
            in
            create actor "delay" (interval ~mode:Fixed_delay ~first_due ()) |> ignore;
            sweep actor;
            fixture.elapsed_ms := 5000;
            sweep actor;
            assert (Int64.equal (consumed actor "delay") 1L);
            (match A.Recurrence_cursor.position (E.cursor (entry actor "delay")) with
             | Awaiting_completion _ -> ()
             | Interval _ | Calendar _ | Exhausted ->
               failwith "fixed delay lacks genuine completion gate");
            fixture.elapsed_ms := 9000;
            sweep actor;
            assert (Int64.equal (consumed actor "delay") 1L);
            let restored = replayed fixture in
            assert (
              Document_schema.Json.equal
                (C.to_jsonaf (Option.value_exn restored.recurrence_catalog) |> protocol_ok)
                (C.to_jsonaf (catalog actor) |> protocol_ok)))));
  print_endline
    "failed write kept enable anchor; elapsed slots survived rollback/retry; actual \
     journal agrees; fixed delay stayed gated";
  [%expect
    {| failed write kept enable anchor; elapsed slots survived rollback/retry; actual journal agrees; fixed delay stayed gated |}]
;;

type cut =
  | Before_admission
  | After_ack
[@@deriving sexp_of]

let%expect_test
    "independent actor-owner death releases a surviving sweep before admission and after \
     ACK"
  =
  List.iter [ Before_admission; After_ack ] ~f:(fun cut ->
    with_store (fun fixture ->
      let entered, enter = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let armed = ref false in
      let gate () =
        if !armed
        then (
          armed := false;
          Eio.Promise.resolve enter ();
          Eio.Promise.await never)
      in
      let owner, publish_owner = Eio.Promise.create () in
      let exited, publish_exit = Eio.Promise.create () in
      Eio.Fiber.fork ~sw:fixture.sw (fun () ->
        Exn.protect
          ~finally:(fun () -> Eio.Promise.resolve publish_exit ())
          ~f:(fun () ->
            try
              Eio.Cancel.sub (fun cancel ->
                Eio.Switch.run (fun sw ->
                  let actor =
                    create_actor
                      fixture
                      ~sw
                      ~before_observe:(fun () ->
                        match cut with
                        | Before_admission -> gate ()
                        | After_ack -> ())
                      ~published:(fun _ ->
                        match cut with
                        | After_ack -> gate ()
                        | Before_admission -> ())
                      ()
                  in
                  Eio.Promise.resolve publish_owner (actor, cancel);
                  Eio.Promise.await never))
            with
            | Eio.Cancel.Cancelled _ -> ()));
      let actor, cancel = Eio.Promise.await owner in
      Exn.protect
        ~finally:(fun () ->
          if Option.is_none (Eio.Promise.peek exited) then Eio.Cancel.cancel cancel Exit;
          Eio.Cancel.protect (fun () -> Eio.Promise.await exited))
        ~f:(fun () ->
          let host = open_host fixture () in
          Exn.protect
            ~finally:(fun () -> Host.close_and_wait host)
            ~f:(fun () ->
              install actor host;
              create
                actor
                "owner"
                (interval ~first_due:(P.Timestamp.to_time_ns (wall_at_ms 1000)) ())
              |> ignore;
              let before = (state actor).counters.transaction_sequence in
              armed := true;
              let result, resolve = Eio.Promise.create () in
              Eio.Fiber.fork ~sw:fixture.sw (fun () ->
                Eio.Promise.resolve resolve (A.Session_actor.sweep_recurrences actor));
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock fixture.env) 5. (fun () ->
                Eio.Promise.await entered;
                Eio.Cancel.cancel cancel Exit;
                Eio.Promise.await exited;
                match Eio.Promise.await result with
                | Error { code = Server_shutting_down; _ } -> ()
                | result -> raise_s [%sexp (result : (unit, P.Error.t) result)]);
              (match A.Session_actor.sweep_recurrences actor with
               | Error { code = Server_shutting_down; _ } -> ()
               | result -> raise_s [%sexp (result : (unit, P.Error.t) result)]);
              let restored = replayed fixture in
              let expected =
                match cut with
                | Before_admission -> before
                | After_ack -> Int64.succ before
              in
              assert (Int64.equal expected restored.counters.transaction_sequence);
              let actual =
                A.Session_persistence.restored fixture.durable
                |> A.Session_persistence.Restored.state
              in
              assert (equal_state actual restored);
              print_s [%sexp (cut : cut), (Int64.(expected - before) : int64)]))));
  [%expect
    {|
    (Before_admission 0)
    (After_ack 1)
    |}]
;;
