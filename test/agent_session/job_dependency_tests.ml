open Core
open Fixtures
open Job_fixtures
module Capacity = Agent_server.Job_capacity
module Completion = Agent_protocol.Completion

let%expect_test
    "waiting jobs retain results across rejected saves, cancellation and expiry"
  =
  List.iter
    [ `Success; `Retryable_failure; `Cancel; `Expired; `Rejected_defer; `Rejected_finish ]
    ~f:(fun mode ->
      let reject_save = ref false in
      let now = ref timestamp in
      with_actor
        ~reject_save:(fun _ -> !reject_save)
        ~now:(fun () -> !now)
        (fun _env _sw actor _writer backend ->
           let registry = native_registry (ref 0) ~raises:false in
           let request =
             Background_execution_tests.capture_tool
               registry
               Chat_response.One_off_request.default_policy
           in
           let parent =
             add_claimed_job
               actor
               ~payload:(Chat_response.Background_request.to_json request)
               ~retry_policy:(Safe_retry { max_attempts = 3; backoff_ms = 0 })
           in
           let deadline =
             Agent_protocol.Timestamp.to_time_ns timestamp
             |> fun at ->
             Time_ns.add at (Time_ns.Span.of_sec 1.)
             |> Agent_protocol.Timestamp.of_time_ns
           in
           let invocation = root parent in
           let invocation =
             I.create { invocation.context with deadline = Some deadline } |> protocol_ok
           in
           let capacity =
             Capacity.create
               ~limits:
                 { daemon_total = 1
                 ; per_principal = 1
                 ; per_prompt = 1
                 ; per_workspace = 1
                 ; per_session = 1
                 ; per_kind = 1
                 ; max_nested_depth = 2
                 }
           in
           let child = ref None in
           with_job actor parent (fun ~job:_ ~execute ->
             execute ~invocation (fun ~dispatched ->
               let owner = J.Invocation dispatched.context.id in
               let staged =
                 Background_transaction_tests.stage actor backend capacity request owner
               in
               child := Some staged;
               A.select_background_jobs actor ~owner ~ids:[ staged.id ] |> protocol_ok;
               Ok (I.Pending (Job staged.id, `String "acknowledgement"))))
           |> protocol_ok
           |> ignore;
           let child = Option.value_exn !child in
           let dependency =
             J.
               { invocation_id = invocation.context.id
               ; job_id = child.id
               ; deadline
               ; completion_schema = None
               ; max_output_bytes = 1_000_000
               ; max_output_depth = 128
               }
           in
           let defer () =
             A.defer_background_job
               actor
               ~job_id:parent.id
               ~generation:parent.generation
               ~attempt:parent.attempt
               dependency
           in
           (match mode with
            | `Rejected_defer ->
              reject_save := true;
              assert (Result.is_error (defer ()));
              reject_save := false;
              (match (Background_scheduler_tests.current backend parent).status with
               | Running -> ()
               | _ -> failwith "rejected wait changed the parent")
            | _ -> ());
           let waiting = defer () |> protocol_ok in
           assert (
             Result.is_error
               (A.complete_background_job
                  actor
                  ~job_id:parent.id
                  ~generation:parent.generation
                  ~attempt:parent.attempt
                  (Completion.Succeeded (`String "stale worker result"))));
           (match waiting.status with
            | Waiting_completion saved -> assert (J.equal_dependency dependency saved)
            | _ -> assert false);
           let saved = Agent_session.Memory_backend.state backend in
           let restored =
             Agent_session.Session_persistence.restore_snapshot
               (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t saved))
             |> store_ok
           in
           assert (Result.is_ok (Agent_session.Session_state.validate restored));
           let forged =
             { waiting with
               status = Waiting_completion { dependency with job_id = parent.id }
             }
           in
           assert (
             Result.is_error
               (Agent_session.Session_delta.apply saved (Job_changed forged)));
           let refresh () =
             A.refresh_background_job
               actor
               ~job_id:parent.id
               ~generation:parent.generation
               ~attempt:parent.attempt
           in
           let result =
             match mode with
             | `Cancel -> A.cancel_job_internal actor ~job_id:parent.id |> protocol_ok
             | `Expired ->
               (now
                := Agent_protocol.Timestamp.to_time_ns deadline
                   |> fun at ->
                   Time_ns.add at (Time_ns.Span.of_sec 1.)
                   |> Agent_protocol.Timestamp.of_time_ns);
               refresh () |> protocol_ok
             | `Success | `Retryable_failure | `Rejected_defer | `Rejected_finish ->
               let child =
                 A.claim_job actor ~job_id:child.id ~generation:child.generation
                 |> protocol_ok
                 |> Option.value_exn
               in
               let completion =
                 match mode with
                 | `Retryable_failure ->
                   Completion.Failed
                     { code = "fixture.retryable"
                     ; message = "eventual failure"
                     ; retryable = true
                     ; details = `Null
                     }
                 | _ -> Completion.Succeeded (`String "eventual result")
               in
               A.complete_background_job
                 actor
                 ~job_id:child.id
                 ~generation:child.generation
                 ~attempt:child.attempt
                 completion
               |> protocol_ok
               |> ignore;
               (match mode with
                | `Rejected_finish ->
                  reject_save := true;
                  assert (Result.is_error (refresh ()));
                  reject_save := false;
                  (match (Background_scheduler_tests.current backend parent).status with
                   | Waiting_completion _ -> ()
                   | _ -> failwith "rejected completion changed the wait")
                | _ -> ());
               (* A saved child result wins over a subsequent cancellation request. *)
               A.cancel_job_internal actor ~job_id:parent.id |> protocol_ok
           in
           [%test_eq: int] 1 result.attempt;
           let completion =
             J.terminal_completion result |> protocol_ok |> Option.value_exn
           in
           (match mode, completion with
            | `Cancel, Cancelled _ | `Expired, Expired ->
              (match (Background_scheduler_tests.current backend child).status with
               | Cancelled -> ()
               | _ -> failwith "owned child escaped cancellation")
            | `Retryable_failure, Failed { retryable = true; _ } -> ()
            | ( (`Success | `Rejected_defer | `Rejected_finish)
              , Succeeded (`String "eventual result") ) -> ()
            | _ -> raise_s [%sexp (completion : Completion.t)]);
           let stale =
             A.refresh_background_job
               actor
               ~job_id:parent.id
               ~generation:parent.generation
               ~attempt:(parent.attempt + 1)
           in
           assert (Result.is_error stale);
           print_s
             [%sexp
               (mode
                : [ `Success
                  | `Retryable_failure
                  | `Cancel
                  | `Expired
                  | `Rejected_defer
                  | `Rejected_finish
                  ])
             , (completion : Completion.t)]));
  [%expect
    {|
    (Success (Succeeded (String "eventual result")))
    (Retryable_failure
     (Failed
      ((code fixture.retryable) (message "eventual failure") (retryable true)
       (details Null))))
    (Cancel (Cancelled "job cancelled"))
    (Expired Expired)
    (Rejected_defer (Succeeded (String "eventual result")))
    (Rejected_finish (Succeeded (String "eventual result")))
    |}]
;;
