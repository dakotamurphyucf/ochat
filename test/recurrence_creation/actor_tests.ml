open! Core
open Creation_support

let%expect_test
    "actual actor permits require fresh authority and elapsed grace across protected \
     moves"
  =
  List.iter [ R.Rule.Interval.Fixed_rate; Fixed_delay ] ~f:(fun mode ->
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
              create_fresh
                actor
                "live"
                (interval ~mode ~first_due:(P.Timestamp.to_time_ns (wall_at_ms 1000)) ());
              sweep actor;
              fixture.elapsed_ms := 1000;
              fixture.wall := wall_at_ms (-3_600_000);
              sweep actor;
              let nominal = selected actor "live" in
              let occurrence = N.key nominal in
              let before = state actor in
              fixture.reject := true;
              assert (Result.is_error (reserve actor "live" occurrence));
              assert (equal_state before (state actor));
              fixture.reject := false;
              let planning_only =
                A.Recurrence_planning_host.authorize
                  (Host.planning_host host)
                  ~controller:session
                |> Result.map_error ~f:(fun reason ->
                  Sexp.to_string_hum
                    (A.Recurrence_planning_status.Reason.sexp_of_t reason))
                |> Result.ok_or_failwith
              in
              assert (
                Result.is_error
                  (reserve ~actor_proof:planning_only actor "live" occurrence));
              assert (equal_state before (state actor));
              let current = ref true in
              let original =
                Operator_authorization.guarded ~principal ~is_current:(fun () -> !current)
              in
              let revoke state definition =
                let result = authorize_creation state definition in
                current := false;
                result
              in
              assert (
                Result.is_error
                  (reserve
                     ~actor_proof:original
                     ~authorize:revoke
                     actor
                     "live"
                     occurrence));
              assert (equal_state before (state actor));
              current := true;
              let permit =
                reserve ~actor_proof:original actor "live" occurrence |> protocol_ok
              in
              let original_row = row permit in
              let original_checked =
                A.Session_actor.check_recurrence_creation actor permit |> protocol_ok
              in
              A.Session_actor.Recurrence_creation.Checked.check_current original_checked
              |> protocol_ok;
              assert (
                String.equal
                  (F.digest
                     (A.Session_actor.Recurrence_creation.Checked.definition
                        original_checked))
                  (F.digest (E.current (entry actor "live"))));
              assert (List.is_empty (H.ready (frontier actor "live")));
              [%test_eq: int] (H.pending_count (frontier actor "live")) 1;
              let committed = (state actor).counters.transaction_sequence in
              let other =
                Agent_server_test_support.principal_with_id "pri_reauthorized_creation"
              in
              let newer = Operator_authorization.trusted_local other in
              let retry =
                reserve ~actor_proof:newer actor "live" occurrence |> protocol_ok
              in
              assert_same_ids original_row (row retry);
              assert (
                P.Id.Principal.equal (Reservation.principal_id (row retry)) principal.id);
              assert (Int64.equal committed (state actor).counters.transaction_sequence);
              current := false;
              assert (
                Result.is_error
                  (A.Session_actor.Recurrence_creation.Checked.check_current
                     original_checked));
              assert (
                Result.is_error (A.Session_actor.check_recurrence_creation actor permit));
              A.Session_actor.check_recurrence_creation actor retry
              |> protocol_ok
              |> ignore;
              let expires_at =
                P.Timestamp.of_time_ns
                  (Time_ns.add
                     (P.Timestamp.to_time_ns !(fixture.wall))
                     (Time_ns.Span.of_int_ms 1))
              in
              let bounded =
                Operator_authorization.bounded
                  ~principal:other
                  ~now:(fun () -> !(fixture.wall))
                  ~expires_at
                |> protocol_ok
              in
              let expiring =
                reserve ~actor_proof:bounded actor "live" occurrence |> protocol_ok
              in
              fixture.wall
              := P.Timestamp.of_time_ns
                   (Time_ns.add
                      (P.Timestamp.to_time_ns expires_at)
                      (Time_ns.Span.of_int_ms 1));
              assert (
                Result.is_error (A.Session_actor.check_recurrence_creation actor expiring));
              fixture.wall := wall_at_ms (-7_200_000);
              fixture.elapsed_ms := 61001;
              assert (
                Result.is_error (A.Session_actor.check_recurrence_creation actor retry));
              fixture.elapsed_ms := 999;
              assert (
                Result.is_error (A.Session_actor.check_recurrence_creation actor retry));
              fixture.elapsed_ms := 2000;
              let before_pause =
                A.Session_actor.check_recurrence_creation actor retry |> protocol_ok
              in
              with_store (fun foreign ->
                let other_actor = create_actor foreign ~sw:foreign.sw () in
                Exn.protect
                  ~finally:(fun () -> A.Session_actor.shutdown other_actor)
                  ~f:(fun () ->
                    assert (
                      Result.is_error
                        (A.Session_actor.check_recurrence_creation other_actor retry))));
              mutate actor "pause-protected" (Pause (id "live")) |> ignore;
              assert (
                Result.is_error
                  (A.Session_actor.Recurrence_creation.Checked.check_current before_pause));
              assert (
                Result.is_error (A.Session_actor.check_recurrence_creation actor retry));
              mutate actor "reenable-protected" (Enable (id "live")) |> ignore;
              assert (Result.is_error (reserve ~actor_proof:newer actor "live" occurrence));
              A.Session_actor.change_moderator actor None |> protocol_ok |> ignore;
              assert (
                Result.is_error (A.Session_actor.check_recurrence_creation actor retry));
              assert (
                Reservation.equal original_row (List.hd_exn (protected actor "live")));
              let journal = replayed fixture in
              assert (equal_state (state actor) journal);
              print_s [%sexp (mode : R.Rule.Interval.mode)]))));
  [%expect
    {|
    Fixed_rate
    Fixed_delay
    |}]
;;

type cut =
  | Before_admission
  | After_ack
[@@deriving sexp_of]

let%expect_test
    "actual ACK-loss cuts reopen the journal and reacquire the original protected \
     identity"
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
      let owned, own = Eio.Promise.create () in
      let exited, exit = Eio.Promise.create () in
      Eio.Fiber.fork ~sw:fixture.sw (fun () ->
        Exn.protect
          ~finally:(fun () -> Eio.Promise.resolve exit ())
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
                  Eio.Promise.resolve own (actor, cancel);
                  Eio.Promise.await never))
            with
            | Eio.Cancel.Cancelled _ -> ()));
      let actor, cancel = Eio.Promise.await owned in
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
              create_fresh
                actor
                "cut"
                (interval ~first_due:(P.Timestamp.to_time_ns !(fixture.wall)) ());
              sweep actor;
              let nominal = selected actor "cut" in
              let before = (state actor).counters.transaction_sequence in
              armed := true;
              let replied, reply = Eio.Promise.create () in
              Eio.Fiber.fork ~sw:fixture.sw (fun () ->
                Eio.Promise.resolve reply (reserve actor "cut" (N.key nominal)));
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock fixture.env) 5. (fun () ->
                Eio.Promise.await entered;
                Eio.Cancel.cancel cancel Exit;
                Eio.Promise.await exited;
                match Eio.Promise.await replied with
                | Error { code = Server_shutting_down; _ } -> ()
                | Ok _ -> failwith "owner death returned a permit"
                | Error error -> raise_s [%sexp (error : P.Error.t)]);
              let _, restored, _ = restored_journal fixture in
              let recovered = A.Session_persistence.Restored.state restored in
              let recovered_entry =
                C.find (Option.value_exn recovered.recurrence_catalog) (id "cut")
                |> Option.value_exn
              in
              let retained =
                H.protected_children (E.frontier recovered_entry |> protocol_ok)
              in
              (match cut, retained with
               | Before_admission, [] ->
                 [%test_eq: int64] recovered.counters.transaction_sequence before
               | After_ack, [ _ ] ->
                 [%test_eq: int64]
                   recovered.counters.transaction_sequence
                   (Int64.succ before)
               | Before_admission, _ :: _ | After_ack, ([] | _ :: _ :: _) -> assert false);
              let reopened = reopen_fixture fixture in
              Exn.protect
                ~finally:(fun () -> Agent_store.Commit_writer.close reopened.writer)
                ~f:(fun () ->
                  let actor = create_actor reopened ~sw:fixture.sw () in
                  Exn.protect
                    ~finally:(fun () -> A.Session_actor.shutdown actor)
                    ~f:(fun () ->
                      let permit = reserve actor "cut" (N.key nominal) |> protocol_ok in
                      (match retained with
                       | [] -> ()
                       | [ original ] -> assert_same_ids original (row permit)
                       | _ -> assert false);
                      A.Session_actor.check_recurrence_creation actor permit
                      |> protocol_ok
                      |> ignore;
                      let expected =
                        match cut with
                        | Before_admission -> Int64.succ before
                        | After_ack -> Int64.succ before
                      in
                      [%test_eq: int64]
                        (state actor).counters.transaction_sequence
                        expected;
                      print_s [%sexp (cut : cut)]))))));
  [%expect
    {|
    Before_admission
    After_ack
    |}]
;;
