open Core
open Fixtures
open Job_fixtures
module Completion = Agent_protocol.Completion

let finish actor (job : J.t) outcome =
  A.complete_background_job
    actor
    ~job_id:job.id
    ~generation:job.generation
    ~attempt:job.attempt
    outcome
;;

let failure ~retryable =
  Completion.Failed
    { code = "fixture.busy"
    ; message = "Try later"
    ; retryable
    ; details = `Object [ "resource", `String "report"; "sequence", `Number "7" ]
    }
;;

let show (job : J.t) =
  print_s
    [%sexp
      (job.status : J.status)
    , (job.attempt : int)
    , (Completion.of_json (Option.value_exn job.result) |> protocol_ok : Completion.t)]
;;

let%expect_test
    "generic retries retain typed failure and reject obsolete attempt callbacks"
  =
  with_actor (fun _env _sw actor _writer backend ->
    let first =
      add_claimed_job
        actor
        ~retry_policy:(Safe_retry { max_attempts = 2; backoff_ms = 0 })
    in
    let queued = finish actor first (failure ~retryable:true) |> protocol_ok in
    show queued;
    assert (Option.is_some queued.next_run_at);
    assert (Option.is_none queued.completed_at);
    let second =
      A.claim_job actor ~job_id:first.id ~generation:first.generation
      |> protocol_ok
      |> Option.value_exn
    in
    let before = Agent_session.Memory_backend.state backend in
    reject "late completion" (finish actor first (Completion.Succeeded `Null));
    let after = Agent_session.Memory_backend.state backend in
    assert (phys_equal before after);
    show
      (finish actor second (Completion.Succeeded (`Object [ "done", `True ]))
       |> protocol_ok));
  [%expect
    {|
    (Queued 1
     (Failed
      ((code fixture.busy) (message "Try later") (retryable true)
       (details (Object ((resource (String report)) (sequence (Number 7))))))))
    ("late completion" Conflict)
    (Succeeded 2 (Succeeded (Object ((done True)))))
    |}]
;;

let%expect_test "nonretryable failures and exhausted retries preserve full diagnostics" =
  List.iter [ false; true ] ~f:(fun retryable ->
    with_actor (fun _env _sw actor _writer _backend ->
      let job =
        add_claimed_job
          actor
          ~retry_policy:
            (Safe_retry
               { max_attempts =
                   (match retryable with
                    | true -> 1
                    | false -> 3)
               ; backoff_ms = 0
               })
      in
      let result = finish actor job (failure ~retryable) |> protocol_ok in
      show result;
      assert (Option.is_some result.completed_at)));
  [%expect
    {|
    ((Failed
      ((code Internal_error) (message "Try later") (retryable false)
       (data
        (Object
         ((type (String failed)) (code (String fixture.busy))
          (message (String "Try later")) (retryable False)
          (details (Object ((resource (String report)) (sequence (Number 7))))))))))
     1
     (Failed
      ((code fixture.busy) (message "Try later") (retryable false)
       (details (Object ((resource (String report)) (sequence (Number 7))))))))
    ((Failed
      ((code Internal_error) (message "Try later") (retryable true)
       (data
        (Object
         ((type (String failed)) (code (String fixture.busy))
          (message (String "Try later")) (retryable True)
          (details (Object ((resource (String report)) (sequence (Number 7))))))))))
     1
     (Failed
      ((code fixture.busy) (message "Try later") (retryable true)
       (details (Object ((resource (String report)) (sequence (Number 7))))))))
    |}]
;;

let%expect_test "completion waits for scope release and cancellation wins late success" =
  with_actor (fun _env _sw actor _writer backend ->
    let job = add_claimed_job actor in
    with_job actor job (fun ~job ~execute:_ ->
      reject "active owner" (finish actor job (Completion.Succeeded `Null));
      Ok ())
    |> protocol_ok;
    A.cancel_job_internal actor ~job_id:job.id |> protocol_ok |> ignore;
    let before = Agent_session.Memory_backend.state backend in
    reject "cancelled job" (finish actor job (Completion.Succeeded `Null));
    assert (phys_equal before (Agent_session.Memory_backend.state backend));
    let job = add_claimed_job actor in
    finish actor job (Completion.Cancelled "tool stopped") |> protocol_ok |> show);
  [%expect
    {|
    ("active owner" Conflict)
    ("cancelled job" Already_resolved)
    (Cancelled 1 (Cancelled "tool stopped"))
    |}]
;;
