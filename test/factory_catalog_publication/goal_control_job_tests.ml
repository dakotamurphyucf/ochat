open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Resume = Goal_resume_runtime_tests
module Manual = Goal_job_runtime_tests
module Control = Goal_control_completion_tests
module G = P.Goal

(* The actual Execute-daemon scheduler owns claim, Factory source,
   cold Init and SAME failed completion. Pause both actual host clocks only after
   observing the scheduler's real polling wait, before admitting the Job. Advance
   after unload; the existing construction-policy port then observes the actual
   claimed constructor. Cleanup resumes clocks before joining daemon schedulers. *)
let with_controlled_due_clocks (env : Eio_unix.Stdenv.base) ~pause ~resume ~advance
  : Eio_unix.Stdenv.base
  =
  let mono_clock, pause_mono, resume_mono, advance_mono =
    controlled_monotonic_clock env#mono_clock
  in
  let clock, pause_wall, resume_wall, advance_wall =
    controlled_wall_clock env#clock ~initial:(Eio.Time.now env#clock)
  in
  (pause
   := fun () ->
        pause_wall ();
        pause_mono ());
  (resume
   := fun () ->
        resume_wall ();
        resume_mono ());
  (advance
   := fun seconds ->
        advance_wall (Eio.Time.now clock +. seconds);
        advance_mono seconds);
  object
    method clock = clock
    method mono_clock = mono_clock
    method domain_mgr = env#domain_mgr
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method process_mgr = env#process_mgr
    method fs = env#fs
    method cwd = env#cwd
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;

let prompt =
  Control.prompt Ready_cold Hold_progress
  |> Control.replace_prompt
       ~pattern:"<script id=\"goal_control_completion\""
       ~with_:
         "<tool name=\"job_recipe\" agent=\"held-job.chatmd\" local/>\n\
          <script id=\"goal_control_completion\""
  |> Control.replace_prompt
       ~pattern:"| _ -> Task.pure(state)\n</script>"
       ~with_:
         {|| `Turn_start ->
  if state == 0 then
    let* child = Model.spawn("agent_prompt_v1", `Object([
      {key = "prompt"; value = `String("held-job.chatmd")},
      {key = "input"; value = `String("545_LOADED_GOAL_JOB")},
      {key = "is_local"; value = `Bool(true)}
    ])) in
    Task.pure(state + 1)
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
;;

let%expect_test "claimed cold Goal Model control completes the same actual failed attempt"
  =
  let advance = ref (fun _ -> failwith "scheduler clock is not installed") in
  let pause_clocks = ref (fun () -> failwith "scheduler clock is not installed") in
  let resume_clocks = ref (fun () -> ()) in
  let armed = ref false in
  let entered, enter = Eio.Promise.create () in
  let released, release = Eio.Promise.create () in
  let release_constructor () =
    if Option.is_none (Eio.Promise.peek released) then Eio.Promise.resolve release ()
  in
  let counters : Retry_support.counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr counters.providers;
      completed_response "one initial foreground turn before cold control")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          incr counters.inference_ports;
          if !armed
          then (
            armed := false;
            Eio.Promise.resolve enter ();
            Eio.Promise.await released);
          base.runtime_inference_ports actor)
    }
  in
  Creation_tests.with_daemon
    ~wrap_env:(fun env ->
      with_controlled_due_clocks env ~pause:pause_clocks ~resume:resume_clocks ~advance)
    ~prompt_text:prompt
    ~qualify_chatml_extensions:true
    ~sources:[ "held-job.chatmd", "<developer>Actual captured cold Model.</developer>" ]
    ~policy
    (fun env _sw _root daemon ->
       protect_cleanup
         ~finally:(fun () -> !resume_clocks ())
         (fun () ->
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
              let rec await_scheduler_wait () =
                match Daemon.For_testing.job_scheduler_wait_deadline daemon with
                | Some _ -> ()
                | None ->
                  Eio.Fiber.yield ();
                  await_scheduler_wait ()
              in
              Source.with_loaded_entry daemon (fun ~entry ~runtime:_ ~revision:_ ->
                await_scheduler_wait ();
                !pause_clocks ();
                let session, created = Goal.create_goal daemon entry in
                let admission =
                  Resume.start_episode daemon entry original session created
                in
                let initial = Goal.await_input env entry counters in
                let queued = Manual.one_job initial in
                (match queued.status with
                 | Queued -> ()
                 | _ -> failwith "actual Model Job was not queued");
                [%test_eq: int] 0 queued.attempt;
                [%test_eq: int] 1 !(counters.providers);
                Owner.unload_and_wait entry.runtime |> protocol_ok;
                let cold = Resume.checkpoint entry in
                assert (not (Owner.is_loaded entry.runtime));
                let old_account = Goal.accounting cold in
                [%test_eq: int64] 1L (A.Goal_accounting.admitted_attempts old_account);
                armed := true;
                protect_cleanup ~finally:release_constructor (fun () ->
                  !advance 0.1;
                  Eio.Promise.await entered;
                  let claimed_state = Resume.checkpoint entry in
                  let claimed = Manual.one_job claimed_state in
                  (match claimed.status with
                   | Running -> ()
                   | _ -> failwith "actual scheduler had not claimed cold Job");
                  [%test_eq: int] 1 claimed.attempt;
                  assert (P.Id.Job.equal queued.id claimed.id);
                  [%test_eq: int] queued.generation claimed.generation;
                  assert (not (Owner.is_loaded entry.runtime));
                  assert (not (Owner.source_lane_idle entry.runtime));
                  assert (G.State.equal (Open Enabled) (G.state (Goal.goal claimed_state)));
                  [%test_eq: int] 1 !(counters.providers);
                  release_constructor ();
                  let rec await_failed_completion () =
                    let current = Resume.checkpoint entry in
                    let completed = Manual.one_job current in
                    match completed.status with
                    | Failed _ when Owner.source_lane_idle entry.runtime ->
                      current, completed
                    | Queued
                    | Running
                    | Waiting_permission _
                    | Waiting_completion _
                    | Failed _ ->
                      Eio.Fiber.yield ();
                      await_failed_completion ()
                    | Succeeded | Cancelled | Interrupted _ ->
                      raise_s
                        [%sexp "control fabricated Job outcome", (completed : P.Job.t)]
                  in
                  let final, completed = await_failed_completion () in
                  assert (P.Id.Job.equal claimed.id completed.id);
                  [%test_eq: int] claimed.generation completed.generation;
                  [%test_eq: int] 1 completed.attempt;
                  if
                    not
                      (G.State.equal
                         (Open (Held (Needs_input Control.reason)))
                         (G.state (Goal.goal final)))
                  then
                    raise_s
                      [%sexp
                        "cold Job did not reach selected control"
                      , (G.state (Goal.goal final) : G.State.t)
                      , (completed : P.Job.t)
                      , (final.moderator_executions : P.Moderator_execution.t list)];
                  [%test_eq: string]
                    "progress after the exact Hold"
                    (G.Progress.to_string (G.progress (Goal.goal final)));
                  Resume.assert_episode cold final admission;
                  assert (Resume.callbacks_complete final);
                  assert (not (Owner.is_loaded entry.runtime));
                  assert (Option.is_none final.active_operation);
                  assert (A.Goal_accounting.equal old_account (Goal.accounting final));
                  [%test_eq: int] 1 !(counters.providers);
                  let proof =
                    List.find_exn
                      (Resume.run final admission).terminal_work
                      ~f:(fun proof ->
                        match proof.P.Run_work.Terminal.work.key with
                        | Retained (Job { id; attempt }) ->
                          P.Id.Job.equal id claimed.id
                          && Int.equal attempt claimed.attempt
                        | Operation _ | Retained _ -> false)
                  in
                  [%test_eq: P.Run_work.Terminal.outcome] Failed proof.outcome;
                  List.iter (Resume.run cold admission).terminal_work ~f:(fun old ->
                    assert (
                      List.mem
                        (Resume.run final admission).terminal_work
                        old
                        ~equal:P.Run_work.Terminal.equal)))))));
  print_endline
    "actual scheduler claim remains attempt1; cold selected control skips model and \
     completes Failed through its same owner; prior work/account retained";
  [%expect
    {| actual scheduler claim remains attempt1; cold selected control skips model and completes Failed through its same owner; prior work/account retained |}]
;;
