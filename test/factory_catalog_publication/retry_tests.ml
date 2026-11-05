open! Core
open Agent_server_test_support
open Retry_support
module Result_attempt = Factory.Deferred_run_attempt

module Source_path = struct
  type t =
    | Direct
    | Resume_after_verification_failure
end

let run_activation (source_path : Source_path.t) =
  let entered, entered_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let release_provider () =
    if Option.is_none (Eio.Promise.peek release) then Eio.Promise.resolve release_u ()
  in
  with_fixture
    ~post_stream:(fun _ ->
      Eio.Promise.resolve entered_u ();
      Eio.Promise.await release;
      completed_response "539 retry complete")
    (fun env sw daemon entry initial_runtime revision counters ->
       protect_cleanup ~finally:release_provider (fun () ->
         with_planner env sw daemon entry (fun () ->
           let inference_ports = !(counters.inference_ports) in
           let startup_receipts current =
             List.filter current.A.Session_state.moderator_executions ~f:(fun receipt ->
               match receipt.P.Moderator_execution.context.phase with
               | Session_start | Session_resume -> true
               | Turn_start
               | Message_appended
               | Pre_tool_call
               | Post_tool_response
               | Turn_end
               | Internal_event -> false)
           in
           let startup = startup_receipts (state entry) in
           let occurrence = create_occurrence daemon entry entry in
           let source_failure =
             match source_path with
             | Direct -> None
             | Resume_after_verification_failure ->
               Some (ordinary_source_failure daemon entry revision)
           in
           let observation = initial_absence env daemon entry occurrence in
           [%test_eq: int] inference_ports !(counters.inference_ports);
           [%test_eq: int] 0 !(counters.providers);
           let original_row = execution entry occurrence.intent in
           let request = next_request entry occurrence in
           let frozen_cas = request.expected_revision in
           let frozen_sha =
             A.Run_admission_request.request_sha256
               (A.Run_admission_request.of_headless request)
             |> protocol_ok
           in
           let admission = ref None in
           let prepare_retry f =
             let begin_retry f =
               Factory.with_recurrence_retry_run_source
                 (Daemon.factory daemon)
                 entry
                 ~observation
                 ~actor:original
                 ~authorize
                 ~intent:occurrence.intent
                 ~previous:occurrence.previous
                 ~request
                 f
             in
             match source_failure with
             | None -> begin_retry f
             | Some expected ->
               let before = state entry in
               let frames = Existing.frames env entry in
               let callbacks = ref 0 in
               let failed =
                 with_changed_source revision (fun () ->
                   begin_retry (fun _ _ ->
                     Int.incr callbacks;
                     Ok ()))
                 |> protocol_ok
               in
               [%test_eq: int] 0 !callbacks;
               let error = Result_attempt.result failed |> rejected in
               [%test_eq: P.Error.code] Prompt_unavailable error.code;
               assert_error expected error;
               assert (Option.is_none (Result_attempt.no_effects failed));
               let retry = Result_attempt.retry failed |> Option.value_exn in
               let frozen = Actor.Recurrence_retry.next_attempt retry in
               Actor.Recurrence_retry.check_basis retry |> protocol_ok;
               Existing.assert_same before entry;
               [%test_eq: string list] frames (Existing.frames env entry);
               [%test_eq: int] inference_ports !(counters.inference_ports);
               [%test_eq: int] 0 !(counters.providers);
               assert (
                 Result.is_error (Actor.No_effects_observation.check_basis observation));
               Factory.resume_recurrence_retry_run_source
                 (Daemon.factory daemon)
                 entry
                 ~retry
                 ~actor:original
                 ~authorize
                 (fun resumed prepared ->
                    assert (phys_equal retry resumed);
                    let next = Actor.Recurrence_retry.next_attempt resumed in
                    assert (Attempt.equal frozen next);
                    assert (P.Id.Principal.equal principal.id (Attempt.principal_id next));
                    assert (P.Idempotency_key.equal request.key (Attempt.key next));
                    Existing.assert_same before entry;
                    [%test_eq: string list] frames (Existing.frames env entry);
                    f resumed prepared)
           in
           let attempt =
             prepare_retry (fun retry prepared ->
               let next = Actor.Recurrence_retry.next_attempt retry in
               [%test_eq: int] 1 (Attempt.number next);
               [%test_eq: int64] frozen_cas (Attempt.expected_revision next);
               [%test_eq: string] frozen_sha (Attempt.request_sha256 next);
               assert (
                 not
                   (P.Idempotency_key.equal
                      (Attempt.key next)
                      (Attempt.key occurrence.previous)));
               assert (
                 Result.is_error (Actor.No_effects_observation.check_current observation));
               let before = state entry in
               let frames = Existing.frames env entry in
               let pending =
                 Actor.admit_held_recurrence_run
                   entry.actor
                   ~binding:(Owner.Prepared.binding prepared)
                   ~intent:occurrence.intent
                   ~attempt:next
                   ~session:occurrence.session
                 |> rejected
               in
               [%test_eq: P.Error.code] Conflict pending.code;
               Existing.assert_same before entry;
               [%test_eq: string list] frames (Existing.frames env entry);
               [%test_eq: int] 0 !(counters.providers);
               [%test_eq: int] inference_ports !(counters.inference_ports);
               Owner.Prepared.check_basis prepared |> protocol_ok;
               let rotated =
                 Actor.rotate_prepared_recurrence_execution
                   entry.actor
                   ~retry
                   ~next:(Owner.Prepared.binding prepared)
                   ~actor:original
                   ~authorize:authorize_execution
                 |> protocol_ok
               in
               [%test_eq: int64]
                 (Int64.succ before.counters.revision)
                 (state entry).counters.revision;
               [%test_eq: int]
                 (List.length frames + 1)
                 (List.length (Existing.frames env entry));
               assert (Intent.equal occurrence.intent (Execution.intent rotated));
               (match Execution.attempts rotated with
                | [ previous; current ] ->
                  assert (
                    Attempt.equal occurrence.previous (Execution.Attempt.request previous));
                  assert (Option.is_some (Execution.Attempt.retry_resolution previous));
                  assert (Option.is_none (Execution.Attempt.origin previous));
                  assert (Attempt.equal next (Execution.Attempt.request current));
                  assert (Option.is_none (Execution.Attempt.retry_resolution current))
                | _ -> failwith "rotation did not append exactly one attempt");
               assert (
                 Attempt.equal
                   (Execution.Attempt.request (Execution.current_attempt original_row))
                   occurrence.previous);
               [%test_eq: int64] frozen_cas (Attempt.expected_revision next);
               [%test_eq: string] frozen_sha (Attempt.request_sha256 next);
               Owner.Prepared.check_basis prepared |> protocol_ok;
               let held =
                 match
                   Actor.admit_held_recurrence_run
                     entry.actor
                     ~binding:(Owner.Prepared.binding prepared)
                     ~intent:occurrence.intent
                     ~attempt:next
                     ~session:occurrence.session
                   |> protocol_ok
                 with
                 | Held held -> held
                 | Retained _ | Rejected _ | Uncertain _ ->
                   failwith "acknowledged retry did not admit actual Held custody"
               in
               let receipt = Actor.Held_run.admission held in
               admission := Some receipt;
               let returned =
                 consume
                   daemon
                   entry
                   entry
                   ~runtime:initial_runtime
                   prepared
                   held
                   occurrence.intent
                   next
                 |> protocol_ok
               in
               assert (P.Run_receipt.equal receipt returned);
               Ok returned)
             |> protocol_ok
           in
           let receipt = Result_attempt.result attempt |> protocol_ok in
           assert (P.Run_receipt.equal receipt (Option.value_exn !admission));
           assert (Option.is_none (Result_attempt.no_effects attempt));
           assert (Option.is_none (Result_attempt.retry attempt));
           let rec await_provider () =
             match Eio.Promise.peek entered with
             | Some () -> ()
             | None ->
               let index = Option.value_exn (state entry).run_state in
               let run = A.Run_state.find index receipt.run_id |> Option.value_exn in
               (match run.lifecycle with
                | Terminal _ ->
                  raise_s [%sexp "root ended before provider", (run : P.Run.t)]
                | Admitted | Active | Waiting _ -> ());
               Eio.Fiber.yield ();
               await_provider ()
           in
           await_provider ();
           (* Full target/source/parent CPS has exited while the actual root
              provider is still blocked. The worker must retain the handed-off Run. *)
           let running = state entry in
           (* The parser owns one fresh lexical graph. Its ports are not another
              runtime constructor; the actual installed runtime stays identical. *)
           Owner.with_loaded_runtime entry.runtime (fun runtime ->
             assert (phys_equal initial_runtime runtime);
             Ok ())
           |> protocol_ok;
           assert (
             List.equal P.Moderator_execution.equal startup (startup_receipts running));
           let operation = Option.value_exn running.active_operation in
           let work = P.Run_work.Key.Operation operation.id in
           let index = Option.value_exn running.run_state in
           let run = A.Run_state.find index receipt.run_id |> Option.value_exn in
           [%test_eq: int]
             1
             (List.count run.owned_work ~f:(fun owned ->
                P.Run_work.Key.equal owned.key work));
           [%test_eq: int] 1 !(counters.providers);
           [%test_eq: int] (inference_ports + 1) !(counters.inference_ports);
           assert (
             String.is_substring
               (List.hd_exn !(counters.transcripts))
               ~substring:"539 actual retry input");
           release_provider ();
           let rec await_terminal () =
             let current = state entry in
             let index = Option.value_exn current.run_state in
             let run = A.Run_state.find index receipt.run_id |> Option.value_exn in
             match run.lifecycle with
             | Terminal (Completed _) -> current, index, run
             | Terminal _ -> raise_s [%sexp "retry root failed", (run : P.Run.t)]
             | Admitted | Active | Waiting _ ->
               Eio.Fiber.yield ();
               await_terminal ()
           in
           let final, index, finished = await_terminal () in
           assert (Option.is_none final.active_operation);
           assert (List.equal P.Moderator_execution.equal startup (startup_receipts final));
           [%test_eq: int] 1 (List.length (A.Run_state.runs index));
           List.iter
             P.Run_receipt.Kind.[ Admission; Terminal ]
             ~f:(fun kind ->
               [%test_eq: int]
                 1
                 (List.count (A.Run_state.receipts index) ~f:(fun saved ->
                    P.Id.Run.equal saved.run_id receipt.run_id
                    && P.Run_receipt.Kind.equal saved.kind kind)));
           assert (
             List.exists finished.terminal_work ~f:(fun terminal ->
               P.Run_work.Key.equal terminal.work.key work
               && P.Run_work.Terminal.equal_outcome terminal.outcome Succeeded));
           assert (
             Option.is_none
               (A.Run_state.receipt
                  index
                  ~principal_id:(Attempt.principal_id occurrence.previous)
                  ~key:(Attempt.key occurrence.previous)
                  ~request_sha256:(Attempt.request_sha256 occurrence.previous)
                |> protocol_ok));
           [%test_eq: int] 1 !(counters.providers);
           [%test_eq: int] (inference_ports + 1) !(counters.inference_ports);
           print_endline
             "pending retry is inert; exact self-target rotation ACK appends once";
           print_endline "one real input Run completes after full source CPS exit")))
;;

let%expect_test "self-target retry rotates before one actual input activation" =
  run_activation Direct;
  [%expect
    {|
    pending retry is inert; exact self-target rotation ACK appends once
    one real input Run completes after full source CPS exit
    |}]
;;

let%expect_test "failed source resumes the same joined retry before actual activation" =
  run_activation Resume_after_verification_failure;
  [%expect
    {|
    pending retry is inert; exact self-target rotation ACK appends once
    one real input Run completes after full source CPS exit
    |}]
;;

let%expect_test "public pending retry preparation cannot enter ordinary construction" =
  with_fixture (fun env sw daemon entry _runtime _revision counters ->
    with_planner env sw daemon entry (fun () ->
      let occurrence = create_occurrence daemon entry entry in
      let observation = initial_absence env daemon entry occurrence in
      let before = state entry in
      let frames = Existing.frames env entry in
      let ports = !(counters.inference_ports) in
      (* This deliberately exercises the public host ingress without claiming
         that direct Actor issuance reserved a Factory/Owner source lifetime. *)
      let retry, deferred =
        Actor.begin_recurrence_retry_preparation
          entry.actor
          ~observation
          ~actor:original
          ~authorize
          ~intent:occurrence.intent
          ~previous:occurrence.previous
          ~request:(next_request entry occurrence)
        |> protocol_ok
      in
      let preparation = A.Run_preparation.Deferred.preparation deferred in
      let closed = ref false in
      let close () =
        if not !closed
        then
          Eio.Cancel.protect (fun () ->
            Actor.close_effect_deferred_preparation entry.actor deferred
            |> protocol_ok
            |> ignore;
            closed := true)
      in
      protect_cleanup ~finally:close (fun () ->
        let callbacks = ref 0 in
        let unchanged () =
          [%test_eq: int] 0 !callbacks;
          Existing.assert_same before entry;
          [%test_eq: string list] frames (Existing.frames env entry);
          [%test_eq: int] ports !(counters.inference_ports);
          [%test_eq: int] 0 !(counters.providers)
        in
        let deny_constructor () =
          let error =
            Owner.with_prepared_run_runtime entry.runtime ~preparation (fun _ ->
              Int.incr callbacks;
              Ok ())
            |> rejected
          in
          [%test_eq: P.Error.code] Conflict error.code;
          unchanged ()
        in
        deny_constructor ();
        Actor.Recurrence_retry.check_basis retry |> protocol_ok;
        close ();
        deny_constructor ();
        assert (Result.is_error (Actor.Recurrence_retry.check_basis retry));
        let resumed =
          Factory.resume_recurrence_retry_run_source
            (Daemon.factory daemon)
            entry
            ~retry
            ~actor:original
            ~authorize
            (fun _ _ ->
               Int.incr callbacks;
               Ok ())
          |> rejected
        in
        [%test_eq: P.Error.code] Conflict resumed.code;
        unchanged ();
        assert (Result.is_error (Actor.No_effects_observation.check_current observation));
        print_endline
          "pending and logically closed preparations cannot enter construction";
        print_endline
          "logical close cannot substitute for Factory-joined retry completion")));
  [%expect
    {|
    pending and logically closed preparations cannot enter construction
    logical close cannot substitute for Factory-joined retry completion
    |}]
;;
