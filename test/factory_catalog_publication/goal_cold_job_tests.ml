open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Resume = Goal_resume_runtime_tests
module Manual = Goal_job_runtime_tests
module Runtime = A.Runtime_builder
module G = P.Goal

(* Same real spawned Job fixture as the qualified loaded-Hold resource law. This
   adds only genuine resource absence + cold constructor/Session_resume + actual
   model execution, and does not repeat the loaded-Hold/deadline timing matrix. *)
let model_input (job : P.Job.t) =
  let recipe =
    match Jsonaf.member "recipe" job.payload with
    | Some (`String recipe) -> recipe
    | Some _ | None -> failwith "actual captured Model Job lost its recipe"
  in
  recipe, Option.value_exn (Jsonaf.member "payload" job.payload)
;;

let%expect_test "claimed Goal Model Job constructs cold under its same actual owner" =
  let counters : Retry_support.counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let provider_states = ref [] in
  let selected = ref None in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr counters.providers;
      Option.iter !selected ~f:(fun entry ->
        provider_states := state entry :: !provider_states);
      completed_response "545 actual cold Goal Model Job")
  in
  Creation_tests.with_daemon
    ~startup_mode:Operator_only
    ~prompt_text:Manual.prompt
    ~sources:[ "held-job.chatmd", "<developer>Actual captured cold recipe.</developer>" ]
    ~policy
    (fun env _sw _root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
         Manual.with_loaded_manual_entry daemon (fun entry old_runtime ->
           selected := Some entry;
           let session, before = Goal.create_goal daemon entry in
           let admission = Resume.start_episode daemon entry original session before in
           let initial = Goal.await_input env entry counters in
           let queued = Manual.one_job initial in
           (match queued.status with
            | Queued -> ()
            | Running
            | Waiting_permission _
            | Waiting_completion _
            | Succeeded
            | Failed _
            | Cancelled
            | Interrupted _ -> failwith "actual spawned Model was not queued");
           [%test_eq: int] 0 queued.attempt;
           assert (G.State.equal (Open Enabled) (G.state (Goal.goal initial)));
           (* Unload an idle resource only. Session Stop would hold the Goal and
              correctly prevent a fresh claim. No runtime/source lease is held. *)
           Owner.unload_and_wait entry.runtime |> protocol_ok;
           assert (not (Owner.is_loaded entry.runtime));
           let cold = Resume.checkpoint entry in
           assert (G.State.equal (Open Enabled) (G.state (Goal.goal cold)));
           assert (P.Run.equal (Resume.run initial admission) (Resume.run cold admission));
           assert (
             Jsonaf.exactly_equal
               (P.Job.to_json queued)
               (P.Job.to_json (Manual.one_job cold)));
           let claimed =
             Actor.claim_job entry.actor ~job_id:queued.id ~generation:queued.generation
             |> protocol_ok
             |> Option.value_exn
           in
           [%test_eq: int] 1 claimed.attempt;
           let recipe, payload = model_input claimed in
           let outcome =
             Actor.with_job_execution
               entry.actor
               ~job_id:claimed.id
               ~generation:claimed.generation
               ~attempt:claimed.attempt
               ~deadline:None
               (fun services ->
                  (match Actor.Job_runtime_owner.attribution services.runtime_owner with
                   | Goal -> ()
                   | Ordinary -> failwith "actual claimed Job lost its Goal owner");
                  let inference =
                    Factory.model_job_inference (Daemon.factory daemon) entry claimed
                    |> protocol_ok
                  in
                  Registry.with_loaded_read
                    (Daemon.registry daemon)
                    cold.identity.session_id
                    (fun actual ->
                       assert (phys_equal actual entry);
                       Factory.with_goal_job_runtime
                         (Daemon.factory daemon)
                         entry
                         ~owner:services.runtime_owner
                         (fun runtime ->
                            assert (not (phys_equal old_runtime runtime));
                            let constructed = Resume.checkpoint entry in
                            let resume = Resume.completed_resume constructed admission in
                            (match resume.status with
                             | Completed _ -> ()
                             | Running | Failed _ | Interrupted _ ->
                               failwith "cold lifecycle did not join");
                            assert (
                              Jsonaf.exactly_equal
                                (P.Job.to_json claimed)
                                (P.Job.to_json (Manual.one_job constructed)));
                            Resume.assert_episode cold constructed admission;
                            [%test_eq: int] 1 !(counters.providers);
                            runtime.Runtime.execute_model_job
                              ~allocate_inference:services.allocate_inference
                              ~inference_context:inference.context
                              ~capture_recipe_target:inference.capture_recipe_target
                              ~recipe
                              ~payload)))
             |> protocol_ok
           in
           let outcome =
             match outcome with
             | Owner.Construction_result.Built outcome -> outcome
             | Callback_completed _ ->
               failwith "ordinary cold Model unexpectedly completed its callback"
           in
           (match outcome with
            | Runtime.Model_succeeded _ -> ()
            | Model_failed message -> failwithf "actual cold Model failed: %s" message ());
           assert (Owner.source_lane_idle entry.runtime);
           let completed =
             Actor.complete_job
               entry.actor
               ~job_id:claimed.id
               ~generation:claimed.generation
               ~attempt:claimed.attempt
               outcome
             |> protocol_ok
           in
           (match completed.status with
            | Succeeded -> ()
            | Queued
            | Running
            | Waiting_permission _
            | Waiting_completion _
            | Failed _
            | Cancelled
            | Interrupted _ -> failwith "actual Model completion was not retained");
           let final = Resume.settled_snapshot entry in
           Resume.assert_episode cold final admission;
           [%test_eq: int] 1 completed.attempt;
           [%test_eq: int] 2 !(counters.providers);
           [%test_eq: int] 2 (List.length (Manual.startup final));
           [%test_eq: int64]
             2L
             (A.Goal_accounting.admitted_attempts (Goal.accounting final));
           [%test_eq: int64] 1L (A.Goal_accounting.admitted_turns (Goal.accounting final));
           let model_provider = List.hd_exn !provider_states in
           let actual_job = Manual.one_job model_provider in
           [%test_eq: int] 1 actual_job.attempt;
           (match actual_job.status with
            | Running -> ()
            | Queued
            | Waiting_permission _
            | Waiting_completion _
            | Succeeded
            | Failed _
            | Cancelled
            | Interrupted _ -> failwith "provider escaped its actual acknowledged Job");
           assert (Option.is_none model_provider.active_operation);
           let work =
             List.find_exn (Resume.run final admission).terminal_work ~f:(fun terminal ->
               match terminal.P.Run_work.Terminal.work.key with
               | Retained (Job { id; attempt }) ->
                 P.Id.Job.equal id claimed.id && Int.equal attempt claimed.attempt
               | Operation _ | Retained _ -> false)
           in
           (match work.outcome with
            | Succeeded -> ()
            | Failed | Cancelled | Interrupted | Limited | Unconfirmed ->
              failwith "same-Run Job outcome differs");
           assert (Owner.is_loaded entry.runtime);
           print_endline
             "same claimed Goal Job owns cold Init/resume and exactly one real model \
              attempt")));
  [%expect
    {| same claimed Goal Job owns cold Init/resume and exactly one real model attempt |}]
;;
