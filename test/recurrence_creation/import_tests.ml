open! Core
open Creation_support
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt
module Execution = A.Recurrence_execution
module Origin = A.Recurrence_run_origin
module Reference = A.Recurrence_run_reference
module Observation = A.Session_actor.Recurrence_run_observation
module Import = A.Session_actor.Recurrence_import

module Revocation = struct
  type t =
    | Controller_revokes_target
    | Target_revokes_controller
end

let publish fixture previous delta =
  let transition =
    A.Session_transition.apply ~now:timestamp previous ~delta ~payloads:[] |> protocol_ok
  in
  A.Session_persistence.commit fixture.durable ~command_audit:None ~previous transition
  |> protocol_ok;
  S.write_metadata
    ~entry:(index_entry transition.state)
    fixture.store
    fixture.handle
    (metadata transition.state)
  |> store_ok;
  transition.state
;;

(* Constructor facts, published through the real journal/Store. No callback,
   Run authority, process completion sample or provider is fabricated here. *)
let publish_admitted fixture (previous : A.Session_state.t) name intent attempt =
  let run_id = P.Id.Run.of_string ("run_import_" ^ name) |> protocol_ok in
  let run lifecycle revision =
    P.Run.create
      ~id:run_id
      ~session:(Intent.target intent)
      ~principal_id:(Attempt.principal_id attempt)
      ~source:(Intent.source intent)
      ~mode:(Intent.mode intent)
      ~lifecycle
      ~revision
      ~owned_work:[]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:timestamp
      ~updated_at:timestamp
    |> protocol_ok
  in
  let receipt ~kind ~key ~request_sha256 ~run_revision ~session_revision =
    P.Run_receipt.create
      ~run_id
      ~principal_id:(Attempt.principal_id attempt)
      ~source:(Intent.source intent)
      ~key
      ~request_sha256
      ~kind
      ~run_revision
      ~session_revision
      ~committed_at:timestamp
    |> protocol_ok
  in
  let admission =
    receipt
      ~kind:Admission
      ~key:(Attempt.key attempt)
      ~request_sha256:(Attempt.request_sha256 attempt)
      ~run_revision:0L
      ~session_revision:Int64.(previous.counters.revision + 1L)
  in
  let origin =
    Origin.create ~intent ~attempt ~run:(run Admitted 0L) ~admission |> protocol_ok
  in
  let index =
    A.Run_state.commit_recurrence_admission
      (Option.value_exn previous.run_state)
      ~run:(run Admitted 0L)
      ~receipt:admission
      ~origin
    |> protocol_ok
  in
  let admitted = publish fixture previous (Run_state_changed index) in
  admitted, index, origin, run, receipt
;;

let publish_terminal fixture previous name intent attempt =
  let admitted, index, origin, run, receipt =
    publish_admitted fixture previous name intent attempt
  in
  let outcome = P.Run.Terminal.Completed None in
  let terminal_receipt =
    receipt
      ~kind:Terminal
      ~key:(P.Idempotency_key.of_string ("import-terminal:" ^ name) |> protocol_ok)
      ~request_sha256:
        (P.Run.Terminal.to_json outcome
         |> Jsonaf.to_string
         |> Digestif.SHA256.digest_string
         |> Digestif.SHA256.to_hex)
      ~run_revision:1L
      ~session_revision:Int64.(admitted.counters.revision + 1L)
  in
  let completed = run (Terminal outcome) 1L in
  let index =
    A.Run_state.commit index ~run:completed ~receipt:terminal_receipt ~intent:None
    |> protocol_ok
  in
  let state = publish fixture admitted (Run_state_changed index) in
  let reference =
    Reference.create ~origin ~run:completed ~receipt:terminal_receipt |> protocol_ok
  in
  state, origin, reference
;;

let import
      actor
      ?(actor_proof = proof)
      ?(authorize = Record_tests.authorize_record)
      ?(automatic_policy = fun _ _ -> Ok true)
      observation
  =
  A.Session_actor.import_recurrence_execution
    actor
    ~actor:actor_proof
    ~authorize
    ~automatic_policy
    ~observation
;;

let observe actor ?(actor_proof = proof) ?(authorize = authorize) intent attempt =
  A.Session_actor.observe_recurrence_run
    actor
    ~actor:actor_proof
    ~authorize
    ~intent
    ~attempt
  |> protocol_ok
  |> Option.value_exn
;;

let%expect_test
    "actual self-target import preserves receipt time, raw duplicate and joint authority"
  =
  with_store (fun base ->
    let seed = create_actor base ~sw:base.sw () in
    let requests =
      Exn.protect
        ~finally:(fun () -> A.Session_actor.shutdown seed)
        ~f:(fun () ->
          let host = open_host base () in
          Exn.protect
            ~finally:(fun () -> Host.close_and_wait host)
            ~f:(fun () ->
              install seed host;
              let _, source = A.Goal_catalog_source.capture (state seed) |> protocol_ok in
              let target =
                A.Recurrence_target.create_existing
                  ~session
                  ~source
                  ~configuration_revision:(state seed).spec.configuration_revision
                  ~input:{ kind = Plain_text; text = "import input"; attachments = [] }
                |> protocol_ok
              in
              let policy =
                A.Recurrence_policy.create
                  ~misfire:Coalesce
                  ~grace_ms:0L
                  ~max_pending:1
                  ~max_history:1
                  ~max_calendar_steps:256
                |> protocol_ok
              in
              List.iter [ "import"; "extended" ] ~f:(fun name ->
                mutate
                  seed
                  ("create-" ^ name)
                  (Create
                     ( id name
                     , { schedule =
                           interval
                             ~mode:Fixed_delay
                             ~first_due:(P.Timestamp.to_time_ns timestamp)
                             ()
                       ; target
                       ; policy
                       } ))
                |> ignore;
                mutate seed ("enable-" ^ name) (Enable (id name)) |> ignore);
              sweep seed;
              List.map [ "import"; "extended" ] ~f:(fun name ->
                let intent, attempt = Record_tests.original_request seed name in
                Record_tests.record seed intent attempt |> protocol_ok |> ignore;
                name, intent, attempt)))
    in
    let journal = reopen_fixture base in
    Exn.protect
      ~finally:(fun () -> Agent_store.Commit_writer.close journal.writer)
      ~f:(fun () ->
        let final, proofs =
          List.fold_map
            requests
            ~init:journal.initial
            ~f:(fun previous (name, intent, attempt) ->
              let state, origin, reference =
                publish_terminal journal previous name intent attempt
              in
              state, (name, intent, attempt, origin, reference))
        in
        let _, _, _, saved_origin, canonical = List.nth_exn proofs 1 in
        let saved_raw =
          Due_support.append
            (Reference.to_json canonical)
            "future_reference"
            (`Number "2.00e1")
        in
        let saved = Reference.of_json saved_raw |> protocol_ok in
        let catalog, _ =
          C.import_execution
            (Option.value_exn final.recurrence_catalog)
            ~origin:saved_origin
            ~terminal:(Some saved)
            ~automatic:Current_policy_allows
          |> protocol_ok
        in
        publish journal final (Recurrence_catalog_changed catalog) |> ignore;
        (* Replay from the ORIGINAL journal basis, closing the current writer.
           The recovered actor has no process completion sample. *)
        let recovered = reopen_fixture { base with writer = journal.writer } in
        Exn.protect
          ~finally:(fun () -> Agent_store.Commit_writer.close recovered.writer)
          ~f:(fun () ->
            recovered.wall := wall_at_ms 30_000;
            let observed_store = ref false in
            let actor =
              create_actor
                recovered
                ~sw:base.sw
                ~before_observe:(fun () -> observed_store := true)
                ()
            in
            Exn.protect
              ~finally:(fun () -> A.Session_actor.shutdown actor)
              ~f:(fun () ->
                let _, intent, attempt, _, _ = List.hd_exn proofs in
                let before = state actor in
                List.iter
                  [ Revocation.Controller_revokes_target; Target_revokes_controller ]
                  ~f:(fun direction ->
                    let target_current = ref true in
                    let writer_current = ref true in
                    let after_target_policy = ref false in
                    let read_policy state =
                      let result = authorize state in
                      if !observed_store
                      then (
                        after_target_policy := true;
                        match direction with
                        | Target_revokes_controller -> writer_current := false
                        | Controller_revokes_target -> ());
                      result
                    in
                    observed_store := false;
                    let observation =
                      observe
                        actor
                        ~actor_proof:
                          (Operator_authorization.guarded
                             ~principal
                             ~is_current:(fun () -> !target_current))
                        ~authorize:read_policy
                        intent
                        attempt
                    in
                    (* Arm only after the real opaque observation was issued. *)
                    observed_store := false;
                    after_target_policy := false;
                    writer_current := true;
                    let write_policy state definition =
                      let result = Record_tests.authorize_record state definition in
                      if !after_target_policy
                      then (
                        match direction with
                        | Controller_revokes_target -> target_current := false
                        | Target_revokes_controller -> ());
                      result
                    in
                    let result =
                      import
                        actor
                        ~actor_proof:
                          (Operator_authorization.guarded
                             ~principal
                             ~is_current:(fun () -> !writer_current))
                        ~authorize:write_policy
                        observation
                    in
                    Record_tests.assert_code Permission_denied result;
                    assert !observed_store;
                    assert !after_target_policy;
                    assert (equal_state before (state actor));
                    assert (equal_state before (replayed base)));
                let observation = observe actor intent attempt in
                let result = import actor observation |> protocol_ok in
                (match result.settlement with
                 | Imported { next = Successor basis; _ } ->
                   assert (
                     P.Timestamp.equal
                       (A.Recurrence_delay_basis.completed_at basis)
                       timestamp)
                 | Imported { next = Evidence_only | Review_hold _; _ }
                 | Retained_exact _ | Detail_not_retained _ -> assert false);
                (match result.timing with
                 | Recovery_adoption -> ()
                 | No_successor | Live_phase | Held _ ->
                   failwith "expected recovered phase adoption");
                let imported_entry = entry actor "import" in
                let imported_frontier = E.frontier imported_entry |> protocol_ok in
                [%test_eq: int] (H.pending_count imported_frontier) 0;
                [%test_eq: int64] (H.summary imported_frontier).settled 1L;
                (match A.Recurrence_cursor.position (E.cursor imported_entry) with
                 | Interval cursor ->
                   [%test_eq: int64] (R.Cursor.Interval.slot cursor) 1L;
                   assert (
                     Time_ns.equal
                       (R.Cursor.Interval.due_at cursor)
                       (P.Timestamp.to_time_ns (wall_at_ms 1000)))
                 | Calendar _ | Awaiting_completion _ | Exhausted -> assert false);
                [%test_eq: int64]
                  (state actor).counters.transaction_sequence
                  Int64.(before.counters.transaction_sequence + 1L);
                let _, saved_intent, saved_attempt, _, _ = List.nth_exn proofs 1 in
                let saved_observation = observe actor saved_intent saved_attempt in
                assert (
                  not
                    (D.Json.equal
                       saved_raw
                       (Observation.terminal saved_observation
                        |> Option.value_exn
                        |> Reference.to_json)));
                let unchanged = state actor in
                let duplicate = import actor saved_observation |> protocol_ok in
                (match duplicate.timing with
                 | No_successor -> ()
                 | Live_phase | Recovery_adoption | Held _ -> assert false);
                (match duplicate.settlement with
                 | Retained_exact row ->
                   let terminal =
                     Execution.current_attempt row
                     |> Execution.Attempt.terminal
                     |> Option.value_exn
                   in
                   assert (Jsonaf.exactly_equal saved_raw (Reference.to_json terminal))
                 | Imported _ | Detail_not_retained _ -> assert false);
                assert (equal_state unchanged (state actor));
                let host = open_host recovered () in
                Exn.protect
                  ~finally:(fun () -> Host.close_and_wait host)
                  ~f:(fun () ->
                    install actor host;
                    sweep actor;
                    let frontier = frontier actor "import" in
                    assert (List.is_empty (H.ready frontier));
                    assert (
                      Option.equal
                        H.Issue.equal
                        (H.issue frontier)
                        (Some Delay_nominal_missed));
                    assert (Int64.equal (H.summary frontier).consumed 1L));
                assert (equal_state (state actor) (replayed base))))));
  print_endline
    "real journal/Store/Actor; exact self-target ACK; saved raw duplicate; 29s overdue; \
     both revocations";
  [%expect
    {| real journal/Store/Actor; exact self-target ACK; saved raw duplicate; 29s overdue; both revocations |}]
;;

(* A separate real session/Handle in the controller's actual Store. The state
   constructor initializes the target's own ledger; changing only identity would
   leave a foreign ledger and would not be a valid second target. *)
let with_target (base : Creation_support.t) f =
  let session_id = P.Id.Session.of_string "ses_import_distinct_target" |> protocol_ok in
  let initial =
    A.Session_state.create
      ~identity:{ base.initial.identity with session_id }
      ~spec:base.initial.spec
      ~initial_history:[]
  in
  let initial = { initial with moderator = base.initial.moderator } in
  let handle =
    S.create_session_initialized
      base.store
      ~sw:base.sw
      ~transaction_id:(P.Id.Transaction.create ())
      ~actor_lock_nonce:"import-target"
      ~initialize:(fun ~staging_directory:_ ->
        S.Initial_projection.create
          ~metadata:(metadata initial)
          ~entry:(index_entry initial))
    |> store_ok
  in
  Exn.protect
    ~finally:(fun () -> S.close_session base.store handle |> store_ok)
    ~f:(fun () ->
      let directory = Filename.concat (S.Handle.directory handle) "target-journal" in
      let journal =
        Agent_store.Journal.create
          ~env:base.env
          ~directory
          ~max_payload_length:1048576
          ~max_segment_bytes:4194304L
          ~max_segment_frames:64
        |> store_ok
      in
      let writer =
        Agent_store.Commit_writer.create
          ~sw:base.sw
          ~journal
          ~session_id
          ~next_transaction_sequence:1L
          ~previous_transaction_hash:None
          ~queue_capacity:8
        |> store_ok
      in
      Exn.protect
        ~finally:(fun () -> Agent_store.Commit_writer.close writer)
        ~f:(fun () ->
          let durable =
            A.Session_persistence.create
              ~archive:(fun _ _ -> failwith "unexpected target archive")
              ~pending_archive:None
              ~before_commit:
                (Some
                   (fun state ->
                     S.prepare_canonical_projection
                       base.store
                       handle
                       ~metadata:(metadata state)
                       ~entry:(index_entry state)
                     |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error))
              ~command_accepted:(fun _ _ -> ())
              ~writer
              ~durability:Flush
              ~limits:document_limits
              ~archive_limits:document_limits
              ~retention_preflight:None
              ~restored:(A.Session_persistence.Restored.authored initial)
              ~previous_transaction_hash:None
          in
          let target = { base with handle; initial; durable; writer; directory } in
          let index, source = A.Goal_catalog_source.capture initial |> protocol_ok in
          let initial = publish target initial (Run_state_changed index) in
          f { target with initial } source))
;;

let%expect_test
    "distinct target ACK loss recovers terminal data and historical visibility"
  =
  with_store (fun base ->
    with_target base (fun target source ->
      let target_session =
        P.Session_ref.create ~server_id ~session_id:target.initial.identity.session_id
      in
      let seed = create_actor base ~sw:base.sw () in
      let intent, attempt =
        Exn.protect
          ~finally:(fun () -> A.Session_actor.shutdown seed)
          ~f:(fun () ->
            let host = open_host base () in
            Exn.protect
              ~finally:(fun () -> Host.close_and_wait host)
              ~f:(fun () ->
                install seed host;
                let captured_target =
                  A.Recurrence_target.create_existing
                    ~session:target_session
                    ~source
                    ~configuration_revision:target.initial.spec.configuration_revision
                    ~input:
                      { kind = Plain_text; text = "distinct target"; attachments = [] }
                  |> protocol_ok
                in
                mutate
                  seed
                  "create-distinct"
                  (Create
                     ( id "distinct"
                     , { schedule =
                           interval
                             ~mode:Fixed_delay
                             ~first_due:(P.Timestamp.to_time_ns timestamp)
                             ()
                       ; target = captured_target
                       ; policy = A.Recurrence_policy.default
                       } ))
                |> ignore;
                mutate seed "enable-distinct" (Enable (id "distinct")) |> ignore;
                sweep seed;
                let intent, original = Record_tests.original_request seed "distinct" in
                let attempt =
                  Attempt.create
                    ~intent
                    ~number:0
                    ~principal_id
                    ~expected_revision:target.initial.counters.revision
                    ~key:(Attempt.key original)
                  |> protocol_ok
                in
                Record_tests.record seed intent attempt |> protocol_ok |> ignore;
                intent, attempt))
      in
      let target_state, _, _ =
        publish_terminal target target.initial "distinct" intent attempt
      in
      let target_actor =
        create_actor { target with initial = target_state } ~sw:base.sw ()
      in
      Exn.protect
        ~finally:(fun () -> A.Session_actor.shutdown target_actor)
        ~f:(fun () ->
          let observation = observe target_actor intent attempt in
          with_store (fun foreign ->
            let other = create_actor foreign ~sw:foreign.sw () in
            Exn.protect
              ~finally:(fun () -> A.Session_actor.shutdown other)
              ~f:(fun () ->
                let before = state other in
                Record_tests.assert_code Conflict (import other observation);
                assert (equal_state before (state other))));
          let controller = reopen_fixture base in
          Exn.protect
            ~finally:(fun () -> Agent_store.Commit_writer.close controller.writer)
            ~f:(fun () ->
              let entered, enter = Eio.Promise.create () in
              let never, _ = Eio.Promise.create () in
              let owned, own = Eio.Promise.create () in
              let exited, exit = Eio.Promise.create () in
              Eio.Fiber.fork ~sw:base.sw (fun () ->
                Exn.protect
                  ~finally:(fun () -> Eio.Promise.resolve exit ())
                  ~f:(fun () ->
                    try
                      Eio.Cancel.sub (fun cancel ->
                        Eio.Switch.run (fun sw ->
                          let actor =
                            create_actor
                              controller
                              ~sw
                              ~published:(fun _ ->
                                Eio.Promise.resolve enter ();
                                Eio.Promise.await never)
                              ()
                          in
                          Eio.Promise.resolve own (actor, cancel);
                          Eio.Promise.await never))
                    with
                    | Eio.Cancel.Cancelled _ -> ()));
              let actor, cancel = Eio.Promise.await owned in
              Exn.protect
                ~finally:(fun () ->
                  if Option.is_none (Eio.Promise.peek exited)
                  then Eio.Cancel.cancel cancel Exit;
                  Eio.Cancel.protect (fun () -> Eio.Promise.await exited))
                ~f:(fun () ->
                  let before = (state actor).counters.transaction_sequence in
                  let replied, reply = Eio.Promise.create () in
                  Eio.Fiber.fork ~sw:base.sw (fun () ->
                    Eio.Promise.resolve
                      reply
                      (capture_owner_call (fun () -> import actor observation)));
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock base.env) 5. (fun () ->
                    Eio.Promise.await entered;
                    Eio.Cancel.cancel cancel Exit;
                    Eio.Promise.await exited;
                    assert_owner_cancelled (Eio.Promise.await replied));
                  Record_tests.assert_code Server_shutting_down (import actor observation);
                  Observation.check_current observation |> protocol_ok;
                  let reopened =
                    reopen_fixture { base with writer = controller.writer }
                  in
                  Exn.protect
                    ~finally:(fun () -> Agent_store.Commit_writer.close reopened.writer)
                    ~f:(fun () ->
                      let actor = create_actor reopened ~sw:base.sw () in
                      Exn.protect
                        ~finally:(fun () -> A.Session_actor.shutdown actor)
                        ~f:(fun () ->
                          let retained = state actor in
                          [%test_eq: int64]
                            retained.counters.transaction_sequence
                            (Int64.succ before);
                          [%test_eq: int] (H.pending_count (frontier actor "distinct")) 0;
                          [%test_eq: int64]
                            (H.summary (frontier actor "distinct")).settled
                            1L;
                          (* This recovered classification requires no target RPC.
                             A live duplicate still authenticates the target. *)
                          let writer, _ =
                            A.Session_actor.attach actor ~mode:Read_write ~subscribe:false
                            |> protocol_ok
                          in
                          A.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
                          |> protocol_ok
                          |> ignore;
                          A.Session_actor.change_moderator actor None
                          |> protocol_ok
                          |> ignore;
                          let stopped = state actor in
                          assert (
                            P.Session.equal_desired_state
                              stopped.lifecycle.desired
                              Stopped);
                          let current =
                            Agent_server_test_support.principal_with_id
                              "pri_import_recovery"
                          in
                          let authorize_current state definition =
                            if
                              Agent_server.Authorization.session_visible_to
                                current
                                (A.Session_state.summary state)
                            then Record_tests.authorize_record state definition
                            else
                              Error
                                (P.Error.create
                                   Permission_denied
                                   ~message:"current import hidden"
                                   ~retryable:false
                                   ())
                          in
                          let result =
                            import
                              actor
                              ~actor_proof:(Operator_authorization.trusted_local current)
                              ~authorize:authorize_current
                              observation
                            |> protocol_ok
                          in
                          (match result.settlement, result.timing with
                           | Retained_exact _, No_successor -> ()
                           | _ ->
                             failwith "historical duplicate changed terminal disposition");
                          assert (equal_state stopped (state actor));
                          assert (equal_state stopped (replayed base)))))))));
  print_endline
    "separate real Handle; same-store fence; actual ACK/no reply; recovered pending0; \
     fresh historical caller";
  [%expect
    {| separate real Handle; same-store fence; actual ACK/no reply; recovered pending0; fresh historical caller |}]
;;
