open! Core
open Creation_support
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Execution = A.Recurrence_execution

let original_request actor name =
  let entry = entry actor name in
  let definition = E.current entry in
  let target, source =
    match A.Recurrence_target.view (F.target definition) with
    | Existing_session { session; source; configuration_revision = _; input = _ } ->
      session, source
    | Fresh_child _ -> failwith "expected existing target fixture"
  in
  let nominal_json =
    E.frontier entry
    |> protocol_ok
    |> H.to_document
    |> document_ok
    |> D.Document.payload
    |> fun payload ->
    match Due_support.field payload "ready" with
    | `Array [ nominal ] -> nominal
    | _ -> failwith "expected one complete saved Ready nominal"
  in
  let intent =
    Intent.create
      ~definition
      ~control_revision:(E.control_revision entry)
      ~nominal_json
      ~target
      ~source
      ~child:None
    |> protocol_ok
  in
  let attempt =
    Attempt.create
      ~intent
      ~number:0
      ~principal_id
      ~expected_revision:(state actor).counters.revision
      ~key:(P.Idempotency_key.of_string ("record:" ^ name) |> protocol_ok)
    |> protocol_ok
  in
  intent, attempt
;;

(* Current controller visibility policy for recording data. This does not admit
   a target Run, load a runtime or imply that the proposed target CAS is current. *)
let authorize_record state definition =
  let open Result.Let_syntax in
  let%bind () = authorize state in
  if P.Session_ref.equal session (I.Definition.controller (F.key definition))
  then Ok ()
  else Error (P.Error.invalid_request "fixture definition belongs to another controller")
;;

let record actor ?(actor_proof = proof) ?(authorize = authorize_record) intent attempt =
  A.Session_actor.record_recurrence_execution
    actor
    ~actor:actor_proof
    ~authorize
    ~intent
    ~attempt
;;

let assert_code expected = function
  | Error error -> assert (P.Error.equal_code expected error.P.Error.code)
  | Ok _ -> failwith "expected exact record admission fence"
;;

let%expect_test
    "actual request recording reauthorizes new work and retained historical data"
  =
  List.iter [ R.Rule.Interval.Fixed_rate; Fixed_delay ] ~f:(fun mode ->
    with_store (fun fixture ->
      let original_current = ref true in
      let revoke_on_observe = ref false in
      let observed = ref false in
      let actor =
        create_actor
          fixture
          ~sw:fixture.sw
          ~before_observe:(fun () ->
            observed := true;
            if !revoke_on_observe then original_current := false)
          ()
      in
      Exn.protect
        ~finally:(fun () -> A.Session_actor.shutdown actor)
        ~f:(fun () ->
          let host = open_host fixture () in
          Exn.protect
            ~finally:(fun () -> Host.close_and_wait host)
            ~f:(fun () ->
              install actor host;
              List.iter [ "record"; "obsolete" ] ~f:(fun name ->
                create
                  actor
                  name
                  (interval
                     ~mode
                     ~first_due:(P.Timestamp.to_time_ns (wall_at_ms 1000))
                     ())
                |> ignore);
              sweep actor;
              fixture.elapsed_ms := 1000;
              fixture.wall := wall_at_ms (-3_600_000);
              sweep actor;
              let intent, attempt = original_request actor "record" in
              let obsolete, obsolete_attempt = original_request actor "obsolete" in
              let before = state actor in
              let unchanged () = assert (equal_state before (state actor)) in
              let original =
                Operator_authorization.guarded ~principal ~is_current:(fun () ->
                  !original_current)
              in
              let reader =
                Operator_authorization.trusted_local
                  { principal with scopes = Set.remove principal.scopes Send_messages }
              in
              assert_code
                Permission_denied
                (record actor ~actor_proof:reader intent attempt);
              unchanged ();
              let other =
                Agent_server_test_support.principal_with_id "pri_record_current"
              in
              let foreign_attempt =
                Attempt.create
                  ~intent
                  ~number:0
                  ~principal_id:other.id
                  ~expected_revision:(Attempt.expected_revision attempt)
                  ~key:(P.Idempotency_key.of_string "wrong-new-principal" |> protocol_ok)
                |> protocol_ok
              in
              assert_code Conflict (record actor intent foreign_attempt);
              unchanged ();
              let revoke state definition =
                let result = authorize_record state definition in
                original_current := false;
                result
              in
              assert_code
                Permission_denied
                (record actor ~actor_proof:original ~authorize:revoke intent attempt);
              unchanged ();
              original_current := true;
              revoke_on_observe := true;
              assert_code
                Permission_denied
                (record actor ~actor_proof:original intent attempt);
              unchanged ();
              revoke_on_observe := false;
              original_current := true;
              observed := false;
              let policy_after_store state definition =
                let open Result.Let_syntax in
                let%bind () = authorize_record state definition in
                if !observed
                then
                  Error
                    (P.Error.create
                       Permission_denied
                       ~message:"current fixture policy revoked"
                       ~retryable:false
                       ())
                else Ok ()
              in
              assert_code
                Permission_denied
                (record actor ~authorize:policy_after_store intent attempt);
              assert !observed;
              unchanged ();
              let row =
                record actor ~actor_proof:original intent attempt |> protocol_ok
              in
              let committed = state actor in
              [%test_eq: int64]
                committed.counters.transaction_sequence
                Int64.(before.counters.transaction_sequence + 1L);
              assert (Intent.equal intent (Execution.intent row));
              assert (
                Attempt.equal
                  attempt
                  (Execution.Attempt.request (List.hd_exn (Execution.attempts row))));
              [%test_eq: int] (List.length (H.ready (frontier actor "record"))) 0;
              [%test_eq: int] (H.pending_count (frontier actor "record")) 1;
              assert (
                Execution.equal row (List.hd_exn (H.executions (frontier actor "record"))));
              assert (List.is_empty committed.jobs && List.is_empty committed.schedules);
              assert (
                List.is_empty (A.Run_state.runs (Option.value_exn committed.run_state)));
              mutate actor "pause-record" (Pause (id "record")) |> ignore;
              fixture.elapsed_ms := 120_000;
              let paused = state actor in
              assert (Execution.equal row (record actor intent attempt |> protocol_ok));
              assert (equal_state paused (state actor));
              original_current := false;
              assert_code
                Permission_denied
                (record actor ~actor_proof:original intent attempt);
              assert (equal_state paused (state actor));
              let writer, _ =
                A.Session_actor.attach actor ~mode:Read_write ~subscribe:false
                |> protocol_ok
              in
              A.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
              |> protocol_ok
              |> ignore;
              A.Session_actor.change_moderator actor None |> protocol_ok |> ignore;
              let stopped = state actor in
              assert (P.Session.equal_desired_state stopped.lifecycle.desired Stopped);
              let current_proof = Operator_authorization.trusted_local other in
              let current_policy state definition =
                if
                  Agent_server.Authorization.session_visible_to
                    other
                    (A.Session_state.summary state)
                then authorize_record state definition
                else
                  Error
                    (P.Error.create
                       Permission_denied
                       ~message:"current reader hidden"
                       ~retryable:false
                       ())
              in
              let replay =
                record
                  actor
                  ~actor_proof:current_proof
                  ~authorize:current_policy
                  intent
                  attempt
                |> protocol_ok
              in
              assert (Execution.equal row replay);
              assert (equal_state stopped (state actor));
              assert_code Invalid_request (record actor obsolete obsolete_attempt);
              assert (equal_state stopped (state actor));
              let restored = replayed fixture in
              assert (equal_state (state actor) restored);
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
    "owner death before request admission or after ACK reopens the exact original request"
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
                        | Before_admission -> ()
                        | After_ack -> gate ())
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
              create
                actor
                "record_cut"
                (interval ~first_due:(P.Timestamp.to_time_ns timestamp) ())
              |> ignore;
              sweep actor;
              let intent, attempt = original_request actor "record_cut" in
              let before = (state actor).counters.transaction_sequence in
              armed := true;
              let replied, reply = Eio.Promise.create () in
              Eio.Fiber.fork ~sw:fixture.sw (fun () ->
                Eio.Promise.resolve
                  reply
                  (capture_owner_call (fun () -> record actor intent attempt)));
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock fixture.env) 5. (fun () ->
                Eio.Promise.await entered;
                Eio.Cancel.cancel cancel Exit;
                Eio.Promise.await exited;
                assert_owner_cancelled (Eio.Promise.await replied));
              assert_code Server_shutting_down (record actor intent attempt);
              (* Read actual journal frames through the existing Store decoder,
                 not the dead actor's possibly installed or stale memory. *)
              let _, restored, _ = restored_journal fixture in
              let recovered = A.Session_persistence.Restored.state restored in
              let catalog = Option.value_exn recovered.recurrence_catalog in
              let recovered_entry =
                C.find catalog (id "record_cut") |> Option.value_exn
              in
              let frontier = E.frontier recovered_entry |> protocol_ok in
              let retained = H.executions frontier in
              (match cut, retained with
               | Before_admission, [] ->
                 [%test_eq: int64] recovered.counters.transaction_sequence before;
                 [%test_eq: int] (List.length (H.ready frontier)) 1
               | After_ack, [ row ] ->
                 [%test_eq: int64]
                   recovered.counters.transaction_sequence
                   (Int64.succ before);
                 assert (Intent.equal intent (Execution.intent row));
                 assert (
                   Attempt.equal
                     attempt
                     (Execution.Attempt.request (List.hd_exn (Execution.attempts row))));
                 [%test_eq: int] (List.length (H.ready frontier)) 0
               | Before_admission, _ :: _ | After_ack, ([] | _ :: _ :: _) ->
                 failwith "unexpected physical request inventory at owner-death cut");
              [%test_eq: int] (H.pending_count frontier) 1;
              let reopened = reopen_fixture fixture in
              Exn.protect
                ~finally:(fun () -> Agent_store.Commit_writer.close reopened.writer)
                ~f:(fun () ->
                  let actor = create_actor reopened ~sw:fixture.sw () in
                  Exn.protect
                    ~finally:(fun () -> A.Session_actor.shutdown actor)
                    ~f:(fun () ->
                      let row = record actor intent attempt |> protocol_ok in
                      assert (Intent.equal intent (Execution.intent row));
                      assert (
                        Attempt.equal
                          attempt
                          (Execution.Attempt.request
                             (List.hd_exn (Execution.attempts row))));
                      (match retained with
                       | [] -> ()
                       | [ saved ] -> assert (Execution.equal saved row)
                       | _ :: _ :: _ -> assert false);
                      [%test_eq: int64]
                        (state actor).counters.transaction_sequence
                        (Int64.succ before);
                      assert (
                        Execution.equal row (record actor intent attempt |> protocol_ok));
                      [%test_eq: int64]
                        (state actor).counters.transaction_sequence
                        (Int64.succ before);
                      (* Replay the complete journal from its original fixture basis; the
                         reopened actor already starts at the recovered journal tip. *)
                      assert (equal_state (state actor) (replayed fixture));
                      print_s [%sexp (cut : cut)]))))));
  [%expect
    {|
    Before_admission
    After_ack
    |}]
;;
