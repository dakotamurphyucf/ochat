open Core
open Fixtures
open Job_fixtures
module Completion = Agent_protocol.Completion
module D = Agent_protocol.Delivery
module State = Agent_session.Session_state

let check_completion expected actual =
  match Option.equal Completion.equal expected actual with
  | true -> ()
  | false ->
    raise_s
      [%sexp
        "completion mismatch"
      , (expected : Completion.t option)
      , (actual : Completion.t option)]
;;

let commit actor changes =
  let state = A.state actor |> protocol_ok in
  A.commit_extensions
    actor
    ~generation:state.identity.generation
    ~expected_revision:state.counters.revision
    changes
;;

let acknowledge actor (job : J.t) =
  let admitted = invocation_fixture () in
  let dispatched = I.dispatch admitted |> protocol_ok in
  let resolved =
    I.resolve
      dispatched
      ~session_id
      ~generation:0
      (Pending (Job job.id, `String "accepted"))
    |> protocol_ok
  in
  commit actor [ Invocation admitted; Invocation dispatched; Invocation resolved ]
  |> protocol_ok
  |> ignore;
  resolved
;;

let delivery (job : J.t) (invocation : I.t) completion =
  D.create
    { id = Agent_protocol.Id.Delivery.create ()
    ; session_id
    ; generation = job.generation
    ; invocation_id = Some invocation.context.id
    ; work = Some (Job job.id)
    ; correlation = "job-result"
    ; source = Job_adapter
    ; completion
    ; wake = No_wake
    ; created_at = timestamp
    }
  |> protocol_ok
;;

let entry (delivery : D.t) =
  let id =
    History_entry.Id.create ~namespace:"job-notification" ~sequence:0
    |> Result.ok_or_failwith
  in
  Agent_session.Notification_history.create ~id delivery |> protocol_ok
;;

let failure : Completion.t =
  Failed
    { code = "tool.unavailable"
    ; message = "try later"
    ; retryable = true
    ; details = `Object [ "resource", `String "report"; "attempt", `Number "1" ]
    }
;;

let%expect_test
    "typed job results survive acknowledgement ordering, atomic delivery and snapshot \
     recovery"
  =
  List.iter
    [ ( "success"
      , Completion.Succeeded (`Object [ "result", `Array [ `Number "1"; `True ] ]) )
    ; "failure", failure
    ; "cancelled", Cancelled "cancelled by owner"
    ; "expired", Expired
    ; ( "interrupted"
      , Failed
          { code = "background.interrupted"
          ; message = "connection lost"
          ; retryable = false
          ; details = `Null
          } )
    ]
    ~f:(fun (label, expected) ->
      with_actor (fun _env _sw actor _writer backend ->
        let job = add_claimed_job actor in
        let resolved = acknowledge actor job in
        let terminal =
          match label with
          | "interrupted" ->
            A.interrupt_job
              actor
              ~job_id:job.id
              ~generation:job.generation
              ~attempt:job.attempt
              ~reason:"connection lost"
            |> protocol_ok
          | _ ->
            A.complete_background_job
              actor
              ~job_id:job.id
              ~generation:job.generation
              ~attempt:job.attempt
              expected
            |> protocol_ok
        in
        check_completion (Some expected) (J.terminal_completion terminal |> protocol_ok);
        let intent = delivery terminal resolved expected in
        commit actor [ Delivery intent ] |> protocol_ok |> ignore;
        let notification = entry intent in
        let published_delivery =
          D.commit intent ~history_id:notification.id ~now:timestamp |> protocol_ok
        in
        assert (
          Result.is_error (commit actor [ Publish (published_delivery, notification) ]));
        assert (
          List.is_empty
            (Agent_session.Memory_backend.state backend).conversation.canonical_history);
        let published = I.publish resolved |> protocol_ok in
        commit actor [ Invocation published; Publish (published_delivery, notification) ]
        |> protocol_ok
        |> ignore;
        commit actor [ Publish (published_delivery, notification) ]
        |> protocol_ok
        |> ignore;
        let state = Agent_session.Memory_backend.state backend in
        [%test_eq: int] 1 (List.length state.conversation.canonical_history);
        let restored =
          Agent_session.Session_persistence.restore_snapshot
            (Sexp.to_string_mach (State.sexp_of_t state))
          |> store_ok
        in
        assert (
          Completion.equal expected (List.hd_exn restored.deliveries).context.completion);
        check_completion
          (Some expected)
          (J.terminal_completion (List.hd_exn restored.jobs) |> protocol_ok);
        print_s [%sexp (label : string), (expected : Completion.t)]));
  [%expect
    {|
    (success (Succeeded (Object ((result (Array ((Number 1) True)))))))
    (failure
     (Failed
      ((code tool.unavailable) (message "try later") (retryable true)
       (details (Object ((resource (String report)) (attempt (Number 1))))))))
    (cancelled (Cancelled "cancelled by owner"))
    (expired Expired)
    (interrupted
     (Failed
      ((code background.interrupted) (message "connection lost")
       (retryable false) (details Null))))
    |}]
;;

let%expect_test
    "delivery recovery rejects missing malformed contradictory and double-wrapped job \
     results"
  =
  with_actor (fun _env _sw actor _writer backend ->
    let job = add_claimed_job actor in
    let resolved = acknowledge actor job in
    let expected = Completion.Succeeded (`String "ready") in
    let job =
      A.complete_background_job
        actor
        ~job_id:job.id
        ~generation:job.generation
        ~attempt:job.attempt
        expected
      |> protocol_ok
    in
    let intent = delivery job resolved expected in
    commit actor [ Delivery intent ] |> protocol_ok |> ignore;
    let state = Agent_session.Memory_backend.state backend in
    List.iter
      [ None
      ; Some (`String "raw")
      ; Some (Completion.to_json failure)
      ; Some (Completion.to_json (Succeeded (Completion.to_json expected)))
      ]
      ~f:(fun result ->
        let corrupt = { state with jobs = [ { job with result } ] } in
        assert (
          Result.is_error
            (Agent_session.Session_persistence.restore_snapshot
               (Sexp.to_string_mach (State.sexp_of_t corrupt)))));
    (* Model output can legitimately look like an envelope and must stay raw. *)
    let legacy = { job with kind = Model_call } in
    let legacy_completion = Completion.Succeeded (Option.value_exn job.result) in
    check_completion (Some legacy_completion) (J.terminal_completion legacy |> protocol_ok);
    let legacy_delivery = delivery legacy resolved legacy_completion in
    State.validate { state with jobs = [ legacy ]; deliveries = [ legacy_delivery ] }
    |> protocol_ok;
    print_endline
      "four corrupt deliveries rejected; envelope-shaped model output preserved");
  [%expect {| four corrupt deliveries rejected; envelope-shaped model output preserved |}]
;;

let%expect_test "a queued retry's retained failure is not a terminal delivery" =
  with_actor (fun _env _sw actor _writer _backend ->
    let first =
      add_claimed_job
        actor
        ~retry_policy:(Safe_retry { max_attempts = 2; backoff_ms = 0 })
    in
    let resolved = acknowledge actor first in
    let retry =
      A.complete_background_job
        actor
        ~job_id:first.id
        ~generation:first.generation
        ~attempt:first.attempt
        failure
      |> protocol_ok
    in
    assert (Option.is_some retry.result);
    check_completion None (J.terminal_completion retry |> protocol_ok);
    assert (Result.is_error (commit actor [ Delivery (delivery retry resolved failure) ]));
    let second =
      A.claim_job actor ~job_id:first.id ~generation:first.generation
      |> protocol_ok
      |> Option.value_exn
    in
    let expected = Completion.Succeeded (`String "recovered") in
    let terminal =
      A.complete_background_job
        actor
        ~job_id:second.id
        ~generation:second.generation
        ~attempt:second.attempt
        expected
      |> protocol_ok
    in
    commit actor [ Delivery (delivery terminal resolved expected) ]
    |> protocol_ok
    |> ignore;
    print_endline "retry failure stays private; final attempt owns terminal delivery");
  [%expect {| retry failure stays private; final attempt owns terminal delivery |}]
;;
