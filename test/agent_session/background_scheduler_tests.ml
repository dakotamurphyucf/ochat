open Core
open Fixtures
open Job_fixtures
module B = Chat_response.Background_request
module Scheduler = Agent_server.Job_scheduler
module Owner = Agent_server.Runtime_owner
module Registry = Agent_server.Session_registry

let with_capacity_scheduler ?(retry_once = false) ~reject_save f =
  with_actor ~reject_save (fun env sw actor _writer backend ->
    let calls = ref 0 in
    let capabilities = native_registry calls ~raises:false in
    let capabilities =
      match retry_once with
      | false -> capabilities
      | true ->
        let native =
          C.find capabilities ~name:"read_file"
          |> Background_execution_tests.cap
          |> C.native_implementation
          |> Option.value_exn
        in
        let implementation =
          { native with
            run_with_progress =
              (fun ~invocation input ->
                native.run_with_progress ~invocation input |> ignore;
                let outcome =
                  match !calls with
                  | 1 ->
                    I.Fail
                      { code = "fixture.busy"
                      ; message = "try again"
                      ; retryable = true
                      ; details = `Null
                      }
                  | _ -> I.Complete `Null
                in
                Openai.Responses.Tool_output.Output.Text
                  (Jsonaf.to_string (I.outcome_to_json outcome)))
          }
        in
        C.create
          ~owner:"scheduler-fixture"
          ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
          ~result_contracts:[ "read_file", Invocation_v1 ]
          [ Chatmd_shell_spec.Source_ref.digest "structured-v1", implementation ]
        |> Background_execution_tests.cap
    in
    let script_tools = Background_execution_tests.tools (fun () -> capabilities) in
    let policy = Chat_response.One_off_request.default_policy in
    let executor : Agent_session.Runtime_builder.background_executor =
      { policy
      ; now = (fun () -> timestamp)
      ; run =
          (fun ~job
            ~deadline
            ~execute
            ~moderator_execute
            ~claim_event:_
            ~is_halted:_
            ~request ->
            Agent_session.Background_execution.run
              ~env
              ~job
              ~deadline
              ~execute
              ~moderator_execute
              ~request
              ~policy
              ~script_tools
              ~now:(fun () -> timestamp)
              ~moderate_tool:(fun _ _ -> Ok None)
              ~prepare_outcome:(fun _ -> Ok ())
              ())
      }
    in
    let runtime =
      { (Runtime_lease_tests.runtime ~script_tools ~close:ignore ()) with
        background_executor = Some executor
      }
    in
    let owner =
      Owner.create ~actor ~initial:(Some runtime) ~build:(fun () -> Ok runtime)
    in
    let registry = Registry.create () in
    let history_ids =
      Agent_session.History_id_source.create
        ~namespace:"scheduler"
        ~block_size:8
        ~reserve:(fun ~count:_ ->
          failwith "background completion must not allocate history")
      |> protocol_ok
    in
    let entry : Registry.entry =
      { actor
      ; history_ids
      ; runtime = owner
      ; durable_events =
          Agent_session.Durable_event_log.create ~capacity:128 [] |> protocol_ok
      ; capacity = None
      ; store_handle = None
      ; expire_permissions = (fun ~now:_ -> ())
      ; close = ignore
      }
    in
    Registry.add registry ~session_id entry |> protocol_ok;
    let capacity =
      Agent_server.Job_capacity.create
        ~limits:
          { daemon_total = 1
          ; per_principal = 1
          ; per_prompt = 1
          ; per_workspace = 1
          ; per_session = 1
          ; per_kind = 1
          ; max_nested_depth = 1
          }
    in
    let start () =
      Scheduler.start ~sw ~clock:(Eio.Stdenv.clock env) ~registry ~capacity
    in
    let scheduler = start () in
    Exn.protect
      ~finally:(fun () ->
        Scheduler.close scheduler;
        Owner.close_and_wait owner)
      ~f:(fun () ->
        f
          env
          actor
          backend
          calls
          registry
          scheduler
          start
          capacity
          (Background_execution_tests.capture_tool capabilities policy)))
;;

let with_scheduler ?retry_once ~reject_save f =
  with_capacity_scheduler
    ?retry_once
    ~reject_save
    (fun env actor backend calls registry scheduler start _capacity request ->
       f env actor backend calls registry scheduler start request)
;;

let new_job ?(retry_policy = J.Never) payload : J.t =
  { id = Agent_protocol.Id.Job.create ()
  ; session_id
  ; generation = 0
  ; kind = Async_tool
  ; launch = None
  ; progress = None
  ; payload
  ; status = Queued
  ; retry_policy
  ; attempt = 0
  ; created_at = timestamp
  ; started_at = None
  ; next_run_at = None
  ; completed_at = None
  ; result = None
  ; delivery = Pending
  }
;;

let submit ?target ?retry_policy actor payload =
  let job = new_job ?retry_policy payload in
  Option.iter target ~f:(fun target -> target := Some job.id);
  A.add_job actor job |> protocol_ok |> ignore;
  job
;;

let current backend (job : J.t) =
  (Agent_session.Memory_backend.state backend).jobs
  |> List.find_exn ~f:(fun current -> Agent_protocol.Id.Job.equal current.id job.id)
;;

let rec until env predicate =
  match predicate () with
  | true -> ()
  | false ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.005;
    until env predicate
;;

let terminal backend job =
  match (current backend job).status with
  | Succeeded | Failed _ | Cancelled | Interrupted _ -> true
  | Queued | Running | Waiting_permission _ | Waiting_completion _ -> false
;;

let pending_save target (transition : Agent_session.Session_transition.t) =
  List.exists transition.state.jobs ~f:(fun job ->
    Option.exists !target ~f:(Agent_protocol.Id.Job.equal job.id)
    &&
    match job.status with
    | Succeeded | Failed _ -> true
    | _ -> false)
;;

let%expect_test
    "scheduler retries only completion persistence and retains capacity until saved"
  =
  let target = ref None
  and rejected = ref 0 in
  with_scheduler
    ~reject_save:(fun next ->
      match !rejected < 3 && pending_save target next with
      | false -> false
      | true ->
        incr rejected;
        true)
    (fun env actor backend calls _registry scheduler _start request ->
       let first = submit ~target actor (B.to_json request) in
       until env (fun () -> !rejected >= 1);
       let second = submit actor (B.to_json request) in
       until env (fun () -> !rejected = 3);
       [%test_eq: int] 1 !calls;
       [%test_eq: int] 1 (Scheduler.running_count scheduler);
       (match (current backend second).status with
        | Queued -> ()
        | _ -> failwith "capacity released before persistence");
       until env (fun () -> terminal backend first && terminal backend second);
       [%test_eq: int] 2 !calls;
       List.iter [ first; second ] ~f:(fun job ->
         let job = current backend job in
         [%test_eq: int] 1 job.attempt;
         print_s
           [%sexp
             (Agent_protocol.Completion.of_json (Option.value_exn job.result)
              |> protocol_ok
              : Agent_protocol.Completion.t)]));
  [%expect
    {|
    (Succeeded (String disclosed))
    (Succeeded (String disclosed))
    |}]
;;

let%expect_test
    "failed invocation cleanup is retried before recording background completion"
  =
  let rejected = ref 0 in
  with_scheduler
    ~reject_save:(fun next ->
      match
        !rejected < 3
        && List.exists next.state.invocations ~f:(fun invocation ->
          match invocation.status with
          | Resolved _ | Published _ -> true
          | _ -> false)
      with
      | false -> false
      | true ->
        incr rejected;
        true)
    (fun env actor backend calls _registry scheduler _start request ->
       let job = submit actor (B.to_json request) in
       until env (fun () -> terminal backend job);
       until env (fun () -> Scheduler.running_count scheduler = 0);
       [%test_eq: int] 3 !rejected;
       [%test_eq: int] 1 !calls;
       (match (current backend job).status with
        | Failed _ -> ()
        | _ -> failwith "lost invocation result was not reported as failure");
       assert (
         List.for_all
           (Agent_session.Memory_backend.state backend).invocations
           ~f:(fun invocation ->
             match invocation.status with
             | Resolved _ | Published _ -> true
             | _ -> false));
       print_endline "one effect; failed cleanup persisted before terminal failure");
  [%expect {| one effect; failed cleanup persisted before terminal failure |}]
;;

let%expect_test "explicit execution retry waits for its failed completion to be saved" =
  let rejected = ref 0 in
  with_scheduler
    ~retry_once:true
    ~reject_save:(fun next ->
      match
        !rejected < 2
        && List.exists next.state.jobs ~f:(fun job ->
          match job.status, job.result with
          | Queued, Some _ -> true
          | _ -> false)
      with
      | false -> false
      | true ->
        incr rejected;
        true)
    (fun env actor backend calls _registry _scheduler _start request ->
       let job =
         submit
           ~retry_policy:(Safe_retry { max_attempts = 2; backoff_ms = 0 })
           actor
           (B.to_json request)
       in
       until env (fun () -> !rejected = 2);
       [%test_eq: int] 1 !calls;
       [%test_eq: int] 1 (current backend job).attempt;
       until env (fun () -> terminal backend job);
       [%test_eq: int] 2 !calls;
       [%test_eq: int] 2 (current backend job).attempt;
       (match (current backend job).status with
        | Succeeded -> ()
        | _ -> failwith "explicit retry did not finish");
       print_endline "two rejected saves; exactly two authorized execution attempts");
  [%expect {| two rejected saves; exactly two authorized execution attempts |}]
;;

let%expect_test
    "cancellation supersedes an unsaved completion and releases its worker capacity"
  =
  let target = ref None
  and rejected = ref 0 in
  with_scheduler
    ~reject_save:(fun next ->
      match pending_save target next with
      | false -> false
      | true ->
        incr rejected;
        true)
    (fun env actor backend calls _registry scheduler _start request ->
       let first = submit ~target actor (B.to_json request) in
       until env (fun () -> !rejected >= 2);
       A.cancel_job_internal actor ~job_id:first.id |> protocol_ok |> ignore;
       until env (fun () -> Scheduler.running_count scheduler = 0);
       let second = submit actor (B.to_json request) in
       until env (fun () -> terminal backend second);
       [%test_eq: int] 2 !calls;
       print_s
         [%sexp
           (Agent_protocol.Completion.of_json
              (Option.value_exn (current backend first).result)
            |> protocol_ok
            : Agent_protocol.Completion.t)];
       match (current backend second).status with
       | Succeeded -> ()
       | _ -> failwith "next job did not acquire released capacity");
  [%expect {| (Cancelled "job cancelled") |}]
;;

let%expect_test
    "shutdown with an unsaved completion recovers as interrupted without repeating \
     effects"
  =
  let target = ref None
  and rejected = ref 0 in
  with_scheduler
    ~reject_save:(fun next ->
      match pending_save target next with
      | false -> false
      | true ->
        incr rejected;
        true)
    (fun env actor backend calls registry scheduler start request ->
       let first = submit ~target actor (B.to_json request) in
       until env (fun () -> !rejected >= 2);
       Scheduler.close scheduler;
       until env (fun () -> Scheduler.running_count scheduler = 0);
       Scheduler.reconcile_recovered
         ~registry
         ~max_count:4096
         ~max_total_bytes:(64 * 1024 * 1024)
       |> protocol_ok;
       (match (current backend first).status with
        | Interrupted _ -> ()
        | _ -> failwith "lost completion was replayable");
       let restarted = start () in
       Exn.protect
         ~finally:(fun () -> Scheduler.close restarted)
         ~f:(fun () ->
           let second = submit actor (B.to_json request) in
           until env (fun () -> terminal backend second);
           [%test_eq: int] 2 !calls;
           print_endline "interrupted first attempt; only the new job executed"));
  [%expect {| interrupted first attempt; only the new job executed |}]
;;

let%expect_test
    "rejected admission completion retries without accumulating workers or blocking \
     cancellation"
  =
  let target = ref None
  and rejected = ref 0 in
  with_scheduler
    ~reject_save:(fun next ->
      match pending_save target next with
      | false -> false
      | true ->
        incr rejected;
        true)
    (fun env actor backend calls _registry scheduler _start request ->
       let first = submit ~target actor (`Object [ "nested_depth", `String "invalid" ]) in
       until env (fun () -> !rejected >= 1);
       let second = submit actor (B.to_json request) in
       until env (fun () -> !rejected >= 2);
       [%test_eq: int] 0 !calls;
       [%test_eq: int] 1 (Scheduler.running_count scheduler);
       (match (current backend second).status with
        | Queued -> ()
        | _ -> failwith "rejected completion lost its queue ownership");
       A.cancel_job_internal actor ~job_id:first.id |> protocol_ok |> ignore;
       until env (fun () -> terminal backend second);
       [%test_eq: int] 1 !calls;
       print_endline
         "one rejected worker; scheduler observed cancellation and resumed queue");
  [%expect {| one rejected worker; scheduler observed cancellation and resumed queue |}]
;;
