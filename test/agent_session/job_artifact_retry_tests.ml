open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module State = Agent_session.Session_state
module Results = Agent_store.Job_result_store

let publisher env sw initial =
  let storage = Job_artifact_fixtures.create env sw initial in
  storage.publisher, storage.session
;;

let%expect_test
    "a selected parent completion survives later child corruption but not an explicit \
     stop"
  =
  List.iter [ false; true ] ~f:(fun stop ->
    let rejected = ref false in
    let now = ref timestamp in
    let storage = ref None in
    with_actor
      ~reject_save:(fun _ -> !rejected)
      ~now:(fun () -> !now)
      ~make_job_results:(fun env sw initial ->
        let results, session = publisher env sw initial in
        storage := Some (results, session);
        Some results)
      (fun env _sw actor writer backend ->
         let results, session = Option.value_exn !storage in
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
         in
         let deadline =
           P.Timestamp.to_time_ns timestamp
           |> fun at -> Time_ns.add at (Time_ns.Span.of_sec 1.) |> P.Timestamp.of_time_ns
         in
         let invocation = root parent in
         let invocation =
           I.create { invocation.context with deadline = Some deadline } |> protocol_ok
         in
         let capacity =
           Agent_server.Job_capacity.create
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
           execute ~invocation (fun ~dispatched ~capabilities:_ ->
             let owner = J.Invocation dispatched.context.id in
             let staged =
               Background_transaction_tests.stage actor backend capacity request owner
             in
             child := Some staged;
             A.select_background_jobs actor ~owner ~ids:[ staged.id ] |> protocol_ok;
             Ok (I.Pending (Job staged.id, `String "accepted"))))
         |> protocol_ok
         |> ignore;
         let child = Option.value_exn !child in
         A.defer_background_job
           actor
           ~job_id:parent.id
           ~generation:0
           ~attempt:parent.attempt
           { invocation_id = invocation.context.id
           ; work = Job child.id
           ; deadline
           ; completion_schema = None
           ; max_output_bytes = 4096
           ; max_output_depth = 128
           }
         |> protocol_ok
         |> ignore;
         let child =
           A.claim_job actor ~job_id:child.id ~generation:0
           |> protocol_ok
           |> Option.value_exn
         in
         let completion = P.Completion.Succeeded (`String (String.make 512 'x')) in
         let child =
           A.complete_background_job
             actor
             ~job_id:child.id
             ~generation:0
             ~attempt:child.attempt
             completion
           |> protocol_ok
         in
         let refresh () =
           A.refresh_background_job
             actor
             ~job_id:parent.id
             ~generation:0
             ~attempt:parent.attempt
         in
         rejected := true;
         assert (Result.is_error (refresh ()));
         rejected := false;
         let waiting = Background_scheduler_tests.current backend parent in
         assert (
           P.Completion.equal
             completion
             (Results.Publisher.pending_completion results ~job:waiting
              |> Option.value_exn));
         let intent =
           Agent_store.Job_result_intent.list ~env ~session ~max_count:8
           |> store_ok
           |> List.hd_exn
         in
         let selected = Agent_store.Job_result_intent.reference intent in
         assert (P.Id.Job.equal selected.job_id parent.id);
         let child_ref =
           match J.terminal_result child |> protocol_ok with
           | Some (Artifact { reference; _ }) -> reference
           | _ -> failwith "expected child artifact"
         in
         let child_file =
           Filename.concat
             (Filename.concat
                (Agent_store.Session_store.Handle.directory session)
                "blobs")
             (P.Id.Blob.to_string child_ref.blob.id ^ ".blob")
         in
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           Eio.Path.(Eio.Stdenv.fs env / child_file)
           "corrupt child";
         assert (Result.is_error (Results.Publisher.load results child_ref));
         (now
          := P.Timestamp.to_time_ns deadline
             |> fun at ->
             Time_ns.add at (Time_ns.Span.of_sec 1.) |> P.Timestamp.of_time_ns);
         if stop
         then A.stop actor ~attachment_id:writer.id ~mode:Cancel |> protocol_ok |> ignore;
         let terminal = refresh () |> protocol_ok in
         let actual =
           J.terminal_completion ~load_artifact:(Results.Publisher.load results) terminal
           |> protocol_ok
           |> Option.value_exn
         in
         match stop, actual with
         | false, _ ->
           assert (P.Completion.equal actual completion);
           (match J.terminal_result terminal |> protocol_ok with
            | Some (Artifact { reference; _ }) ->
              assert (P.Id.Blob.equal reference.blob.id selected.blob.id)
            | _ -> failwith "parent reference changed");
           print_endline
             "rejected parent save retried its original result/reference after child \
              corruption and deadline"
         | true, Cancelled _ ->
           assert (
             Option.is_none (Results.Publisher.pending_completion results ~job:terminal));
           print_endline
             "explicit stop won; selected completion did not resurrect the cancelled job"
         | _ -> failwith "unexpected stopped result"));
  [%expect
    {|
    rejected parent save retried its original result/reference after child corruption and deadline
    explicit stop won; selected completion did not resurrect the cancelled job
    |}]
;;

let%expect_test
    "late completion recovery retains original time when no retry instant fits"
  =
  let now = ref timestamp in
  let reject_save = ref false in
  let storage = ref None in
  with_actor
    ~now:(fun () -> !now)
    ~reject_save:(fun _ -> !reject_save)
    ~make_job_results:(fun env sw initial ->
      let actual = Job_artifact_fixtures.create env sw initial in
      storage := Some actual;
      Some actual.publisher)
    (fun _env _sw actor _writer backend ->
       let storage = Option.value_exn !storage in
       let job =
         add_claimed_job
           actor
           ~retry_policy:(Safe_retry { max_attempts = 2; backoff_ms = 1 })
       in
       let completion = Background_completion_tests.failure ~retryable:true in
       let selected = ref None in
       let before = A.state actor |> protocol_ok in
       (* The real Publisher has durably prepared this exact failed attempt, but
          its host persistence callback has not committed a terminal Job. *)
       (match
          Results.Publisher.publish
            storage.publisher
            ~storage:Artifact
            ~jobs:before.jobs
            ~job
            ~now:timestamp
            completion
            ~persist:(fun stored ->
              selected := Some stored;
              Error
                (P.Error.create
                   Persistence_error
                   ~message:"injected prepared completion save failure"
                   ~retryable:true
                   ()))
        with
        | Error failure -> assert (P.Error.equal_code failure.code Persistence_error)
        | Ok _ -> failwith "prepared completion unexpectedly committed");
       assert (phys_equal before (A.state actor |> protocol_ok));
       assert (phys_equal before (Agent_session.Memory_backend.state backend));
       let selected =
         match Option.value_exn !selected with
         | P.Stored_completion.Artifact { reference; _ } -> reference
         | Inline _ -> failwith "expected actual prepared artifact"
       in
       let recover () =
         A.recover_background_results actor ~max_count:8 ~max_total_bytes:4096
       in
       now := P.Timestamp.of_time_ns (Time_ns.of_int63_ns_since_epoch Int63.max_value);
       assert (Result.is_error (P.Timestamp.add_ms !now 1));
       (* Using the original artifact time to decide the retry would incorrectly
          queue; recovery must use the actual late observation instead. *)
       assert (Result.is_ok (P.Timestamp.add_ms timestamp 1));
       reject_save := true;
       (match recover () with
        | Error failure ->
          assert (P.Error.equal_code failure.code Internal_error);
          [%test_eq: string] "injected job transition failure" failure.message
        | Ok () -> failwith "rejected recovery acknowledged the terminal attempt");
       assert (phys_equal before (A.state actor |> protocol_ok));
       assert (phys_equal before (Agent_session.Memory_backend.state backend));
       reject_save := false;
       recover () |> protocol_ok;
       let terminal = Background_scheduler_tests.current backend job in
       (match terminal.status with
        | Failed failure ->
          [%test_eq: string] "Try later" failure.message;
          assert failure.retryable
        | _ -> failwith "late recovered failure did not finish the attempt");
       [%test_eq: int] job.attempt terminal.attempt;
       assert (Option.is_none terminal.next_run_at);
       assert (Option.equal P.Timestamp.equal terminal.completed_at (Some timestamp));
       let actual =
         J.terminal_completion
           ~load_artifact:(Results.Publisher.load storage.publisher)
           terminal
         |> protocol_ok
         |> Option.value_exn
       in
       assert (P.Completion.equal completion actual);
       (match J.terminal_result terminal |> protocol_ok with
        | Some (Artifact { reference; _ }) ->
          assert (P.Id.Blob.equal selected.blob.id reference.blob.id)
        | _ -> failwith "recovery replaced the prepared completion reference");
       assert (
         Option.is_none
           (Results.Publisher.pending_completion storage.publisher ~job:terminal));
       let committed = A.state actor |> protocol_ok in
       recover () |> protocol_ok;
       assert (phys_equal committed (A.state actor |> protocol_ok));
       assert (phys_equal committed (Agent_session.Memory_backend.state backend));
       print_endline
         "late recovery retained exact failure, artifact and original completed_at; \
          rejected save stayed running, terminal save did not retry");
  [%expect
    {| late recovery retained exact failure, artifact and original completed_at; rejected save stayed running, terminal save did not retry |}]
;;
