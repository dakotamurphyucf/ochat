open! Core
open Agent_server_test_support
open Retry_support
module Service = Agent_server.Recurrence_run_service

let service ?(authorize = fun _ current -> authorize current) daemon =
  Service.create ~factory:(Daemon.factory daemon) ~authorize ~automatic_policy:(fun _ _ ->
    Ok true)
;;

let activate service entry occurrence =
  Service.activate
    service
    ~controller:entry
    ~controller_actor:original
    ~target:entry
    ~target_actor:original
    ~intent:occurrence.intent
    ~attempt:occurrence.previous
  |> protocol_ok
;;

let activated = function
  | Service.Outcome.Attempted attempt ->
    assert (Option.is_none (Service.Attempt.no_effects attempt));
    assert (Option.is_none (Service.Attempt.retry attempt));
    (match Service.Attempt.result attempt |> protocol_ok with
     | Activated admission -> admission
     | Retained _ | Admission_uncertain _ | Activation_failed _ ->
       failwith "public activation did not acknowledge a fresh handoff")
  | Retained _ -> failwith "fresh public request returned retained DATA"
;;

let imported_row entry occurrence ~changed (imported : Actor.Recurrence_import.t) =
  (* These interval fixtures neither rearm a fixed-delay successor nor adopt a
     completion phase. Inspect both fields of the acknowledged import result. *)
  (match imported.timing with
   | No_successor -> ()
   | Live_phase | Recovery_adoption ->
     failwith "interval reconciliation unexpectedly installed a completion phase"
   | Held error -> raise_s [%sexp "interval reconciliation is held", (error : P.Error.t)]);
  let reported =
    match imported.settlement with
    | Imported { execution; next = Evidence_only } when changed -> execution
    | Retained_exact execution when not changed -> execution
    | Detail_not_retained _ -> failwith "expected complete public execution detail"
    | Imported _ | Retained_exact _ ->
      failwith "public reconciliation changed its expected evidence disposition"
  in
  let actual = execution entry occurrence.intent in
  assert (Execution.equal actual reported);
  actual
;;

let retained entry occurrence admission = function
  | Service.Outcome.Retained result ->
    assert (P.Run_receipt.equal admission result.admission);
    result.reconciliation |> protocol_ok |> imported_row entry occurrence ~changed:false
  | Attempted _ -> failwith "retained public request acquired a new attempt"
;;

let run entry admission =
  let current = state entry in
  let index = Option.value_exn current.run_state in
  ( current
  , index
  , A.Run_state.find index admission.P.Run_receipt.run_id |> Option.value_exn )
;;

let await_completed entry admission =
  let rec await () =
    let current, index, selected = run entry admission in
    match selected.lifecycle with
    | Terminal (Completed _) -> current, index, selected
    | Terminal _ -> raise_s [%sexp "public input failed", (selected : P.Run.t)]
    | Admitted | Active | Waiting _ ->
      Eio.Fiber.yield ();
      await ()
  in
  await ()
;;

let reconcile service entry occurrence admission =
  match
    Service.reconcile
      service
      ~controller:entry
      ~controller_actor:original
      ~target:entry
      ~target_actor:original
      ~intent:occurrence.intent
      ~attempt:occurrence.previous
    |> protocol_ok
  with
  | Retained result ->
    assert (P.Run_receipt.equal admission result.admission);
    result.reconciliation |> protocol_ok |> imported_row entry occurrence ~changed:true
  | Absent -> failwith "actual public admission disappeared during reconciliation"
;;

let%expect_test "public self input retains its exact admission across replay and recovery"
  =
  with_recoverable_fixture (fun env sw root daemon policy entry runtime _ counters ->
    with_planner env sw daemon entry (fun () ->
      let occurrence = create_occurrence daemon entry entry in
      let startup = (state entry).moderator_executions in
      let ports = !(counters.inference_ports) in
      let public = service daemon in
      let admission = activate public entry occurrence |> activated in
      let final, index, selected = await_completed entry admission in
      [%test_eq: int] 1 !(counters.providers);
      [%test_eq: int] (ports + 1) !(counters.inference_ports);
      assert (Option.is_none final.active_operation);
      List.iter startup ~f:(fun previous ->
        let retained =
          List.find_exn final.moderator_executions ~f:(fun receipt ->
            P.Id.Moderator_execution.equal receipt.context.id previous.context.id)
        in
        assert (P.Moderator_execution.equal previous retained);
        match retained.status with
        | Completed _ -> ()
        | Running | Failed _ | Interrupted _ ->
          raise_s
            [%sexp
              "prior startup receipt is incomplete", (retained : P.Moderator_execution.t)]);
      let operation =
        List.filter_map selected.owned_work ~f:(fun work ->
          match work.P.Run_work.key with
          | Operation id -> Some id
          | Retained _ -> None)
        |> function
        | [ operation ] -> operation
        | _ -> failwith "public input does not retain exactly one root operation"
      in
      let input_receipts =
        List.filter final.moderator_executions ~f:(fun receipt ->
          not
            (List.exists startup ~f:(fun previous ->
               P.Id.Moderator_execution.equal previous.context.id receipt.context.id)))
      in
      List.iter input_receipts ~f:(fun receipt ->
        [%test_eq: P.Id.Operation.t option] (Some operation) receipt.context.operation_id;
        [%test_eq: int] selected.source.generation receipt.context.generation;
        assert (
          P.Invocation.equal_observer selected.source.observer receipt.context.source);
        (match receipt.context.phase with
         | Turn_start | Message_appended | Turn_end -> ()
         | Session_start
         | Session_resume
         | Pre_tool_call
         | Post_tool_response
         | Internal_event ->
           raise_s
             [%sexp
               "input replayed an unrelated phase", (receipt : P.Moderator_execution.t)]);
        match receipt.status with
        | Completed _ -> ()
        | Running | Failed _ | Interrupted _ ->
          raise_s
            [%sexp "input receipt is incomplete", (receipt : P.Moderator_execution.t)]);
      List.iter
        P.Moderator_execution.[ Turn_start; Turn_end ]
        ~f:(fun phase ->
          [%test_eq: int]
            1
            (List.count input_receipts ~f:(fun receipt ->
               P.Moderator_execution.equal_phase phase receipt.context.phase)));
      Owner.with_loaded_runtime entry.runtime (fun actual ->
        assert (phys_equal runtime actual);
        Ok ())
      |> protocol_ok;
      let origin = A.Run_state.recurrence_origin index selected.id |> Option.value_exn in
      A.Recurrence_run_origin.validate
        origin
        ~intent:occurrence.intent
        ~attempt:occurrence.previous
      |> protocol_ok;
      A.Recurrence_run_origin.validate_run origin ~run:selected ~admission |> protocol_ok;
      List.iter
        P.Run_receipt.Kind.[ Admission; Terminal ]
        ~f:(fun kind ->
          [%test_eq: int]
            1
            (List.count (A.Run_state.receipts index) ~f:(fun receipt ->
               P.Id.Run.equal receipt.run_id selected.id
               && P.Run_receipt.Kind.equal receipt.kind kind)));
      assert (
        String.is_substring
          (List.hd_exn !(counters.transcripts))
          ~substring:"539 actual retry input");
      (* Import actual terminal evidence before taking the no-write replay basis. *)
      let terminal_row = reconcile public entry occurrence admission in
      assert (
        Option.is_some
          (Execution.Attempt.terminal (Execution.current_attempt terminal_row)));
      let before = state entry in
      let frames = Existing.frames env entry in
      let counts = !(counters.inference_ports), !(counters.providers) in
      assert (
        Execution.equal
          terminal_row
          (activate public entry occurrence |> retained entry occurrence admission));
      Existing.assert_same before entry;
      [%test_eq: string list] frames (Existing.frames env entry);
      [%test_eq: int * int] counts (!(counters.inference_ports), !(counters.providers));
      let read_only =
        { principal with scopes = P.Scope.Set.of_list [ View_session_transcript ] }
      in
      List.iter
        [ principal_with_id "pri_520_other_activation_caller"; read_only ]
        ~f:(fun denied_principal ->
          (match
             Service.activate
               public
               ~controller:entry
               ~controller_actor:original
               ~target:entry
               ~target_actor:(Operator_authorization.trusted_local denied_principal)
               ~intent:occurrence.intent
               ~attempt:occurrence.previous
           with
           | Error error ->
             [%test_eq: P.Error.code] Permission_denied error.code;
             assert (not error.retryable)
           | Ok _ -> failwith "retained activation bypassed its command authority");
          Existing.assert_same before entry;
          [%test_eq: string list] frames (Existing.frames env entry);
          [%test_eq: int * int] counts (!(counters.inference_ports), !(counters.providers)));
      let now = ref (P.Timestamp.now ()) in
      let expires_at = P.Timestamp.add_ms !now 1 |> protocol_ok in
      let expired_controller =
        Operator_authorization.bounded ~principal ~now:(fun () -> !now) ~expires_at
        |> protocol_ok
      in
      now := expires_at;
      (match
         Service.activate
           public
           ~controller:entry
           ~controller_actor:expired_controller
           ~target:entry
           ~target_actor:original
           ~intent:occurrence.intent
           ~attempt:occurrence.previous
         |> protocol_ok
       with
       | Retained { admission = known; reconciliation = Error error } ->
         assert (P.Run_receipt.equal admission known);
         [%test_eq: P.Error.code] Permission_denied error.code;
         assert (not error.retryable)
       | Retained { reconciliation = Ok _; _ } | Attempted _ ->
         failwith "retained reply erased its admission or ignored expired controller");
      Existing.assert_same before entry;
      [%test_eq: string list] frames (Existing.frames env entry);
      [%test_eq: int * int] counts (!(counters.inference_ports), !(counters.providers));
      let id = before.identity.session_id in
      let old_store = Daemon.store daemon in
      Daemon.shutdown daemon |> protocol_ok;
      assert (Agent_store.Session_store.is_closed old_store);
      let reopened =
        Daemon.start
          ~sw
          ~env:(env :> Eio_unix.Stdenv.base)
          ~config:(config root root (Filename.concat root "parent.chatmd"))
          ~tool_dir:root
          ~home:root
          ~process_start_identity:None
          ~options:
            { Daemon.default_options with
              inference_policy = policy
            ; qualify_chatml_extensions = true
            }
          ()
        |> protocol_ok
      in
      protect_cleanup
        ~finally:(fun () -> Daemon.shutdown reopened |> protocol_ok)
        (fun () ->
           let recovered = Registry.load (Daemon.registry reopened) id |> protocol_ok in
           assert (not (phys_equal entry.actor recovered.actor));
           let before = Retry_rotation_tests.await_recovered recovered before in
           let frames = Existing.frames env recovered in
           let counts = !(counters.inference_ports), !(counters.providers) in
           let row =
             activate (service reopened) recovered occurrence
             |> retained recovered occurrence admission
           in
           assert (Execution.equal terminal_row row);
           Existing.assert_same before recovered;
           [%test_eq: string list] frames (Existing.frames env recovered);
           [%test_eq: int * int]
             counts
             (!(counters.inference_ports), !(counters.providers));
           [%test_eq: int] 1 !(counters.providers);
           print_endline
             "public self input hands off one exact Run; live replay writes nothing";
           print_endline
             "same key and origin recover as retained DATA without another effect")));
  [%expect
    {|
    public self input hands off one exact Run; live replay writes nothing
    same key and origin recover as retained DATA without another effect
    |}]
;;

let%expect_test "public explicit retry consumes real joined absence once" =
  with_fixture (fun env sw daemon entry _runtime _ counters ->
    with_planner env sw daemon entry (fun () ->
      let occurrence = create_occurrence daemon entry entry in
      let absence = initial_absence env daemon entry occurrence in
      let request = next_request entry occurrence in
      let sha =
        A.Run_admission_request.request_sha256
          (A.Run_admission_request.of_headless request)
        |> protocol_ok
      in
      let actual =
        Service.retry
          (service daemon)
          ~controller:entry
          ~controller_actor:original
          ~target:entry
          ~target_actor:original
          ~observation:absence
          ~intent:occurrence.intent
          ~previous:occurrence.previous
          ~request
        |> protocol_ok
      in
      assert (Option.is_none (Service.Attempt.no_effects actual));
      assert (Option.is_none (Service.Attempt.retry actual));
      let admission =
        match Service.Attempt.result actual |> protocol_ok with
        | Activated admission -> admission
        | Retained _ | Admission_uncertain _ | Activation_failed _ ->
          failwith "public explicit retry did not activate"
      in
      let _final, index, selected = await_completed entry admission in
      let row = execution entry occurrence.intent in
      let next =
        match Execution.attempts row with
        | [ old; next ] ->
          assert (Attempt.equal occurrence.previous (Execution.Attempt.request old));
          assert (Option.is_some (Execution.Attempt.retry_resolution old));
          assert (Option.is_none (Execution.Attempt.origin old));
          Execution.Attempt.request next
        | _ -> failwith "public retry appended more than one request"
      in
      [%test_eq: int] 1 (Attempt.number next);
      [%test_eq: int64] request.expected_revision (Attempt.expected_revision next);
      [%test_eq: string] sha (Attempt.request_sha256 next);
      assert (P.Idempotency_key.equal request.key (Attempt.key next));
      let origin = A.Run_state.recurrence_origin index selected.id |> Option.value_exn in
      A.Recurrence_run_origin.validate origin ~intent:occurrence.intent ~attempt:next
      |> protocol_ok;
      assert (
        Option.is_none
          (A.Run_state.receipt
             index
             ~principal_id:(Attempt.principal_id occurrence.previous)
             ~key:(Attempt.key occurrence.previous)
             ~request_sha256:(Attempt.request_sha256 occurrence.previous)
           |> protocol_ok));
      assert (Result.is_error (Actor.No_effects_observation.check_basis absence));
      [%test_eq: int] 1 !(counters.providers);
      print_endline
        "public retry ACK rotates once before activating only the frozen next request"));
  [%expect
    {| public retry ACK rotates once before activating only the frozen next request |}]
;;

let%expect_test "public policy revocation after reciprocal ACK preserves known admission" =
  with_fixture (fun env sw daemon entry _runtime _ counters ->
    with_planner env sw daemon entry (fun () ->
      let occurrence = create_occurrence daemon entry entry in
      let target_live = ref true
      and revoked = ref false in
      let controller_actor =
        Operator_authorization.guarded ~principal ~is_current:(fun () -> true)
      in
      let target_actor =
        Operator_authorization.guarded ~principal ~is_current:(fun () -> !target_live)
      in
      let ports = !(counters.inference_ports) in
      let before = state entry in
      let authorize _ (current : A.Session_state.t) =
        let result = authorize current in
        Option.iter current.recurrence_catalog ~f:(fun catalog ->
          Option.iter (Catalog.find catalog recurrence_id) ~f:(fun recurrence ->
            let frontier = Entry.frontier recurrence |> protocol_ok in
            Option.iter
              (Frontier.find_execution frontier (Intent.occurrence occurrence.intent))
              ~f:(fun row ->
                if
                  Intent.equal occurrence.intent (Execution.intent row)
                  && Option.is_some
                       (Execution.Attempt.origin (Execution.current_attempt row))
                then (
                  revoked := true;
                  target_live := false))));
        result
      in
      let public = service ~authorize daemon in
      let attempted =
        match
          Service.activate
            public
            ~controller:entry
            ~controller_actor
            ~target:entry
            ~target_actor
            ~intent:occurrence.intent
            ~attempt:occurrence.previous
          |> protocol_ok
        with
        | Attempted attempted -> attempted
        | Retained _ -> failwith "revocation fixture was not a fresh attempt"
      in
      assert !revoked;
      assert (not (Operator_authorization.is_current target_actor));
      assert (Operator_authorization.is_current controller_actor);
      let admission =
        match Service.Attempt.result attempted |> protocol_ok with
        | Activation_failed { admission; error } ->
          [%test_eq: P.Error.code] Permission_denied error.code;
          assert (not error.retryable);
          admission
        | Activated _ | Retained _ | Admission_uncertain _ ->
          failwith "public service erased or activated the revoked known admission"
      in
      assert (Option.is_none (Service.Attempt.no_effects attempted));
      assert (Option.is_none (Service.Attempt.retry attempted));
      let current, index, selected = run entry admission in
      assert (List.exists (A.Run_state.receipts index) ~f:(P.Run_receipt.equal admission));
      let origin = A.Run_state.recurrence_origin index selected.id |> Option.value_exn in
      A.Recurrence_run_origin.validate
        origin
        ~intent:occurrence.intent
        ~attempt:occurrence.previous
      |> protocol_ok;
      assert (Option.is_none current.active_operation);
      assert (List.equal P.Invocation.equal before.invocations current.invocations);
      assert (
        List.equal
          (fun left right ->
             Jsonaf.exactly_equal (P.Job.to_json left) (P.Job.to_json right))
          before.jobs
          current.jobs);
      assert (
        List.equal
          P.Moderator_execution.equal
          before.moderator_executions
          current.moderator_executions);
      [%test_eq: int] ports !(counters.inference_ports);
      [%test_eq: int] 0 !(counters.providers);
      print_endline
        "actual post-import policy revokes original; known admission survives with zero \
         effects"));
  [%expect
    {| actual post-import policy revokes original; known admission survives with zero effects |}]
;;
