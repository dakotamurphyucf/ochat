open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Resume = Goal_resume_runtime_tests
module Manual = Goal_job_runtime_tests
module Source = Deferred_source_support
module G = P.Goal

let effect_file = "545-background.effect"
let effect_text = "545 actual background write"

let prompt =
  {|<developer>Keep the actual native Job owned by this Goal.</developer>
<tool name="append_to_file"><write path="${workspace}"/></tool>
<script id="goal_background" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Turn_start ->
  if state == 0 then
    let* job = Job.start_tool("append_to_file", `Object([
      {key = "path"; value = `String(BACKGROUND_PATH)},
      {key = "content"; value = `String("545 actual background write")}
    ])) in Task.pure(state + 1)
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
;;

let wait_context_file = "545-background-wait-context.json"

let job_wait_prompt =
  {|<developer>Wait for the exact actual Goal native Job.</developer>
<tool name="append_to_file"><write path="${workspace}"/></tool>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="goal_background" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = { job = ""; waiting = false }
let field value key = match Json.get_field(value, key) with
| `Some(found) -> found
| `None -> `Null
let read_context () =
  let* read = Tool.call("read_file", `Object([
    {key = "root"; value = `String("data")},
    {key = "file"; value = `String("545-background-wait-context.json")}
  ])) in
  match read with
  | `Error(message) -> Task.fail(message)
  | `Ok(`String(text)) ->
    let lines = String.split(text, "\n") in Task.pure(Json.parse(lines[2]))
  | `Ok(_) -> Task.fail("Job wait context is not actual text")
let on_event ctx state event = match event with
| `Turn_start ->
  if state.job == "" then
    let* job = Job.start_tool("append_to_file", `Object([
      {key = "path"; value = `String(BACKGROUND_PATH)},
      {key = "content"; value = `String("545 actual background write")}
    ])) in Task.pure({ state with job = job })
  else Task.pure(state)
| `Turn_end ->
  if state.waiting then Task.pure(state) else
  let* context = read_context() in
  let wake = `Object([
    {key = "run_id"; value = field(context, "run_id")},
    {key = "source"; value = field(context, "source")},
    {key = "occurrence"; value = `Object([
      {key = "kind"; value = `String("job_completion")},
      {key = "job_id"; value = `String(state.job)},
      {key = "attempt"; value = `Number(1.0)}
    ])}
  ]) in
  Task.bind(Run.wait(wake), fun ignored -> Task.pure({ state with waiting = true }))
| `Internal_event(_) ->
  let* context = read_context() in
  Task.bind(Goal.finish(field(context, "finish")), fun ignored -> Task.pure(state))
| _ -> Task.pure(state)
</script>|}
;;

let one_job current =
  match current.A.Session_state.jobs with
  | [ job ] ->
    [%test_eq: P.Job.kind] Async_tool job.kind;
    job
  | jobs -> raise_s [%sexp "actual spawned background occurrence", (jobs : P.Job.t list)]
;;

module Residency = struct
  type t =
    | Loaded
    | Cold
    | Job_wait
end

let with_controlled_polling (env : Eio_unix.Stdenv.base) ~pause ~resume ~clock_now
  : Eio_unix.Stdenv.base
  =
  let clock, pause_clock, resume_clock, _ =
    controlled_wall_clock env#clock ~initial:(Eio.Time.now env#clock)
  in
  pause := pause_clock;
  resume := resume_clock;
  (clock_now := fun () -> Eio.Time.now clock);
  object
    method clock = clock
    method mono_clock = env#mono_clock
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

let run (residency : Residency.t) =
  let initial_provider, initial_provider_u = Eio.Promise.create () in
  let initial_provider_released = ref false in
  let release_initial_provider () =
    if not !initial_provider_released
    then (
      initial_provider_released := true;
      Eio.Promise.resolve initial_provider_u ())
  in
  let entered, enter = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let released = ref false in
  let pause_polling = ref ignore in
  let resume_polling = ref ignore in
  let clock_now = ref (fun () -> failwith "controlled daemon clock is absent") in
  let effect_path = ref None in
  let hits = ref 0 in
  let release_effect () =
    if not !released
    then (
      released := true;
      Eio.Promise.resolve release_u ())
  in
  let counters : Retry_support.counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr counters.providers;
      (match residency with
       | Loaded | Cold -> ()
       | Job_wait -> Eio.Promise.await initial_provider);
      completed_response "545 original Goal turn")
  in
  let selected_prompt =
    match residency with
    | Loaded | Cold -> prompt
    | Job_wait -> job_wait_prompt
  in
  Creation_tests.with_daemon
    ~prompt_text:selected_prompt
    ~sources:
      (match residency with
       | Loaded | Cold -> []
       | Job_wait -> [ wait_context_file, "{}\n" ])
    ~qualify_chatml_extensions:true
    ~prepare_sources:(fun env root ->
      let path = Filename.concat root effect_file in
      effect_path := Some path;
      let source =
        String.substr_replace_all
          selected_prompt
          ~pattern:"BACKGROUND_PATH"
          ~with_:(Jsonaf.to_string (`String path))
      in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(Eio.Stdenv.fs env / root / "parent.chatmd")
        source)
    ~wrap_env:(fun env ->
      let env =
        match residency with
        | Loaded -> env
        | Cold | Job_wait ->
          with_controlled_polling
            env
            ~pause:pause_polling
            ~resume:resume_polling
            ~clock_now
      in
      Crash_fault_io.wrap
        env
        ~boundary:After_create
        ~matches:(fun path -> Option.exists !effect_path ~f:(String.equal path))
        ~reached:(fun _ ->
          Int.incr hits;
          Eio.Promise.resolve enter ();
          Eio.Promise.await release))
    ~policy
    (fun env _sw root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
         protect_cleanup
           ~finally:(fun () -> !resume_polling ())
           (fun () ->
              (match residency with
               | Loaded -> ()
               | Cold | Job_wait ->
                 (* Pause the genuine scheduler's timed wait before any entry is
                 registered. Observing a future actual loop deadline proves its
                 current pass has finished; it cannot claim our future Job yet. *)
                 !pause_polling ();
                 let rec await_scheduler_sleep () =
                   match Daemon.For_testing.job_scheduler_wait_deadline daemon with
                   | Some deadline when Float.(deadline > !clock_now ()) -> ()
                   | Some _ | None ->
                     Eio.Fiber.yield ();
                     await_scheduler_sleep ()
                 in
                 await_scheduler_sleep ());
              Source.with_loaded_entry
                daemon
                (fun ~entry ~runtime:initial_runtime ~revision:_ ->
                   protect_cleanup
                     ~finally:(fun () ->
                       release_initial_provider ();
                       release_effect ())
                     (fun () ->
                        let session, before = Goal.create_goal daemon entry in
                        let admission =
                          Resume.start_episode daemon entry original session before
                        in
                        let finish_intent =
                          match residency with
                          | Loaded | Cold -> None
                          | Job_wait ->
                            let current = Resume.checkpoint entry in
                            let run = Resume.run current admission in
                            let finish =
                              P.Goal_achievement.Intent.create
                                ~expected_goal_revision:(G.revision (Goal.goal current))
                                ~evidence:
                                  (G.Reason.of_string
                                     "Actual waited native Job and its callback completed"
                                   |> protocol_ok)
                                ~action:
                                  (Finish { terminal = Completed None; relinquish = [] })
                              |> protocol_ok
                            in
                            Eio.Path.save
                              ~create:(`Or_truncate 0o600)
                              Eio.Path.(Eio.Stdenv.fs env / root / wait_context_file)
                              (Jsonaf.to_string
                                 (`Object
                                     [ "run_id", P.Id.Run.to_json run.id
                                     ; "source", P.Run_source.to_json run.source
                                     ; "finish", P.Goal_achievement.Intent.to_json finish
                                     ])
                               ^ "\n");
                            release_initial_provider ();
                            Some finish
                        in
                        let assert_finish_revision current =
                          Option.iter finish_intent ~f:(fun intent ->
                            [%test_eq: int64]
                              (P.Goal_achievement.Intent.expected_goal_revision intent)
                              (G.revision (Goal.goal current)))
                        in
                        let cold_basis =
                          match residency with
                          | Loaded -> None
                          | Cold | Job_wait ->
                            let initial = Goal.await_input env entry counters in
                            let queued = one_job initial in
                            [%test_eq: int] 0 queued.attempt;
                            (match residency with
                             | Loaded | Cold -> ()
                             | Job_wait ->
                               (match (Resume.run initial admission).lifecycle with
                                | Waiting
                                    { occurrence = Job_completion { job_id; attempt }; _ }
                                  ->
                                  assert (P.Id.Job.equal job_id queued.id);
                                  [%test_eq: int] 1 attempt
                                | Admitted | Active | Waiting _ | Terminal _ ->
                                  failwith
                                    "actual Turn_end did not retain the exact queued Job \
                                     Wait"));
                            (match queued.status with
                             | Queued -> ()
                             | Running
                             | Waiting_permission _
                             | Waiting_completion _
                             | Succeeded
                             | Failed _
                             | Cancelled
                             | Interrupted _ ->
                               failwith
                                 "paused scheduler did not preserve actual queued Job");
                            (match queued.inference_origin with
                             | Some (Run id) ->
                               assert (P.Id.Run.equal admission.run_id id)
                             | Some Unbound | None ->
                               failwith "actual queued native Job lost its Goal Run");
                            assert (
                              G.State.equal (Open Enabled) (G.state (Goal.goal initial)));
                            [%test_eq: int] 1 (List.length (Manual.startup initial));
                            Owner.unload_and_wait entry.runtime |> protocol_ok;
                            assert (not (Owner.is_loaded entry.runtime));
                            let cold = Resume.checkpoint entry in
                            assert_finish_revision cold;
                            assert (
                              P.Run.equal
                                (Resume.run initial admission)
                                (Resume.run cold admission));
                            assert (
                              Jsonaf.exactly_equal
                                (P.Job.to_json queued)
                                (P.Job.to_json (one_job cold)));
                            assert (
                              A.Goal_accounting.equal
                                (Goal.accounting initial)
                                (Goal.accounting cold));
                            (* Release only the actual scheduler clock wait. Existing
                       claim/source/constructor/executor ports perform all work. *)
                            !resume_polling ();
                            Some cold
                        in
                        (match residency with
                         | Loaded -> Eio.Promise.await entered
                         | Cold | Job_wait ->
                           let rec await_native_effect () =
                             match Eio.Promise.peek entered with
                             | Some () -> ()
                             | None ->
                               let current = state entry in
                               let job = one_job current in
                               (match job.status with
                                | Failed _ | Cancelled | Interrupted _ ->
                                  raise_s
                                    [%sexp
                                      "cold native Job ended before its actual effect"
                                    , (job : P.Job.t)
                                    , (current.moderator_executions
                                       : P.Moderator_execution.t list)]
                                | Queued
                                | Running
                                | Waiting_permission _
                                | Waiting_completion _
                                | Succeeded ->
                                  Eio.Fiber.yield ();
                                  await_native_effect ())
                           in
                           await_native_effect ());
                        [%test_eq: int] 1 !hits;
                        let executing = Resume.checkpoint entry in
                        assert_finish_revision executing;
                        let job = one_job executing in
                        [%test_eq: int] 1 job.attempt;
                        (match job.status with
                         | Running -> ()
                         | Queued
                         | Waiting_permission _
                         | Waiting_completion _
                         | Succeeded
                         | Failed _
                         | Cancelled
                         | Interrupted _ ->
                           failwith "scheduler native effect has no actual Running Job");
                        let run = Resume.run executing admission in
                        let expected = A.Run_job_occurrence.work job |> protocol_ok in
                        assert (List.exists run.owned_work ~f:(P.Run_work.equal expected));
                        let native =
                          Resume.one_actual
                            "actual scheduler background root"
                            (List.filter executing.invocations ~f:(fun invocation ->
                               Option.exists
                                 invocation.P.Invocation.context.parent_job
                                 ~f:(P.Id.Job.equal job.id)
                               && String.equal
                                    invocation.context.tool_name
                                    "chatml.background"))
                        in
                        (match native.inference_origin with
                         | Some (Run id) -> assert (P.Id.Run.equal admission.run_id id)
                         | Some Unbound | None ->
                           failwith "native background lost actual Goal Run");
                        let append =
                          Resume.one_actual
                            "actual scheduler native target"
                            (List.filter executing.invocations ~f:(fun invocation ->
                               String.equal
                                 invocation.P.Invocation.context.tool_name
                                 "append_to_file"
                               && Option.exists
                                    invocation.context.parent_invocation
                                    ~f:(P.Id.Invocation.equal native.context.id)))
                        in
                        (match append.status with
                         | Dispatching -> ()
                         | Admitted | Resolved _ | Published _ ->
                           failwith "native effect boundary is not inside real dispatch");
                        let path = Eio.Path.(Eio.Stdenv.fs env / root / effect_file) in
                        [%test_eq: string] "" (Eio.Path.load path);
                        (match residency, cold_basis with
                         | Loaded, None ->
                           Owner.with_loaded_runtime entry.runtime (fun runtime ->
                             assert (phys_equal initial_runtime runtime);
                             Ok ())
                           |> protocol_ok
                         | (Cold | Job_wait), Some cold ->
                           (* The cold executor deliberately retains its source lease
                      across this native effect; borrowing it through ordinary
                      Owner APIs here would violate that exclusive lane. *)
                           assert (not (Owner.source_lane_idle entry.runtime));
                           assert (Option.is_none executing.active_operation);
                           let queued = one_job cold in
                           assert (P.Id.Job.equal queued.id job.id);
                           assert (Int.equal queued.generation job.generation);
                           assert (Jsonaf.exactly_equal queued.payload job.payload);
                           assert (
                             Option.equal P.Job.equal_launch queued.launch job.launch);
                           assert (
                             Option.equal
                               P.Job.Inference_origin.equal
                               queued.inference_origin
                               job.inference_origin);
                           Resume.assert_episode cold executing admission;
                           ignore
                             (Resume.completed_resume executing admission
                              : P.Moderator_execution.t);
                           [%test_eq: int] 2 (List.length (Manual.startup executing))
                         | Loaded, Some _ | (Cold | Job_wait), None ->
                           failwith "fixture lost its actual cold runtime basis");
                        release_effect ();
                        let rec await_completion () =
                          let current = Resume.checkpoint entry in
                          ignore (Resume.callbacks_complete current : bool);
                          let summary =
                            A.Inference_ledger.summary current.inference_ledger
                          in
                          let turns = P.Inference_query.Summary.turns summary in
                          if
                            Int64.(
                              turns.failed > 0L
                              || turns.cancelled > 0L
                              || turns.interrupted > 0L)
                          then
                            raise_s
                              [%sexp
                                "original Goal turn failed"
                              , (summary : P.Inference_query.Summary.t)];
                          match (one_job current).status with
                          | Succeeded
                            when Option.is_none current.active_operation
                                 && Resume.callbacks_complete current
                                 && Owner.source_lane_idle entry.runtime ->
                            let turns =
                              A.Inference_ledger.summary current.inference_ledger
                              |> P.Inference_query.Summary.turns
                            in
                            let waiter_complete =
                              match residency with
                              | Loaded | Cold -> true
                              | Job_wait ->
                                (match
                                   ( (Resume.run current admission).lifecycle
                                   , G.state (Goal.goal current) )
                                 with
                                 | Terminal (Completed None), Closed (Achieved _) -> true
                                 | _ -> false)
                            in
                            if Int64.equal turns.completed 1L && waiter_complete
                            then current
                            else (
                              Eio.Fiber.yield ();
                              await_completion ())
                          | Failed _ | Cancelled | Interrupted _ ->
                            raise_s
                              [%sexp
                                "actual scheduler background failed"
                              , (one_job current : P.Job.t)]
                          | Queued
                          | Running
                          | Waiting_permission _
                          | Waiting_completion _
                          | Succeeded ->
                            Eio.Fiber.yield ();
                            await_completion ()
                        in
                        let final = await_completion () in
                        let terminal = one_job final in
                        let run = Resume.run final admission in
                        let proof =
                          List.find run.terminal_work ~f:(fun proof ->
                            P.Run_work.equal proof.P.Run_work.Terminal.work expected)
                          |> Option.value_exn
                        in
                        [%test_eq: P.Run_work.Terminal.outcome] Succeeded proof.outcome;
                        [%test_eq: int] 1 terminal.attempt;
                        [%test_eq: string] ("\n" ^ effect_text) (Eio.Path.load path);
                        Resume.assert_episode executing final admission;
                        [%test_eq: int64]
                          1L
                          (A.Goal_accounting.admitted_attempts (Goal.accounting final));
                        [%test_eq: int64]
                          1L
                          (A.Goal_accounting.admitted_turns (Goal.accounting final));
                        [%test_eq: int] 1 !(counters.providers);
                        [%test_eq: int]
                          (match residency with
                           | Loaded -> 1
                           | Cold | Job_wait -> 2)
                          (List.length (Manual.startup final));
                        Owner.with_loaded_runtime entry.runtime (fun runtime ->
                          (match residency with
                           | Loaded -> assert (phys_equal initial_runtime runtime)
                           | Cold | Job_wait ->
                             assert (not (phys_equal initial_runtime runtime)));
                          Ok ())
                        |> protocol_ok;
                        (match residency with
                         | Loaded | Cold -> ()
                         | Job_wait ->
                           let events =
                             List.filter final.moderator_executions ~f:(fun event ->
                               P.Moderator_execution.equal_phase
                                 Internal_event
                                 event.context.phase)
                           in
                           let event =
                             Resume.one_actual "actual Job-completion callback" events
                           in
                           (match event.status with
                            | Completed _ -> ()
                            | Running | Failed _ | Interrupted _ ->
                              failwith "Job wake callback did not commit");
                           Goal_wake_runtime_tests.assert_native final admission event;
                           let wait =
                             List.filter
                               (A.Run_state.intents (Option.value_exn final.run_state))
                               ~f:(fun intent ->
                                 match intent.action, intent.disposition with
                                 | ( Wait
                                       { occurrence = Job_completion { job_id; attempt }
                                       ; _
                                       }
                                   , Consumed None ) ->
                                   P.Id.Job.equal terminal.id job_id
                                   && Int.equal terminal.attempt attempt
                                 | ( (Continue | Wait _ | Finish _)
                                   , (Pending | Consumed _ | Retired) ) -> false)
                             |> Resume.one_actual "one exact consumed Job Wait"
                           in
                           let index = Option.value_exn final.run_state in
                           let key =
                             A.Run_job_delivery.Key.of_wake
                               (match wait.action with
                                | Wait wake -> wake
                                | Continue | Finish _ -> assert false)
                             |> Option.value_exn
                           in
                           let actual_delivery =
                             A.Run_state.find_job_delivery index key |> Option.value_exn
                           in
                           (match A.Run_job_delivery.disposition actual_delivery with
                            | Retired
                                { execution_id = Some id
                                ; enqueued_at = Some _
                                ; reason = Run_terminal
                                } ->
                              assert (P.Id.Moderator_execution.equal id event.context.id)
                            | Pending | Enqueued _ | Claimed _ | Retired _ ->
                              failwith "terminal Job delivery lost its actual claim ACK");
                           assert (Option.is_none final.active_operation);
                           let before = Resume.settled_snapshot entry in
                           let wake =
                             match wait.action with
                             | Wait wake -> wake
                             | Continue | Finish _ -> assert false
                           in
                           let rec reject_replay () =
                             match
                               Factory.deliver_goal_wake
                                 (Daemon.factory daemon)
                                 entry
                                 ~wake
                             with
                             | Error error
                               when P.Error.equal_code Conflict error.code
                                    && error.retryable ->
                               Eio.Fiber.yield ();
                               reject_replay ()
                             | Error error -> raise_s [%sexp (error : P.Error.t)]
                             | Ok No_work -> ()
                             | Ok (Suspended_goal _ | Handed_off _) ->
                               failwith "completed Job wake replayed its callback"
                           in
                           reject_replay ();
                           Existing.assert_same before entry);
                        let stable = Resume.checkpoint entry in
                        let refusal =
                          match
                            Factory.execute_background_job
                              (Daemon.factory daemon)
                              entry
                              terminal
                          with
                          | Error error -> error
                          | Ok _ -> failwith "terminal background Job executed again"
                        in
                        [%test_eq: P.Error.code] Conflict refusal.code;
                        [%test_eq: bool] false refusal.retryable;
                        Existing.assert_same stable entry;
                        [%test_eq: string] ("\n" ^ effect_text) (Eio.Path.load path);
                        [%test_eq: int] 1 !hits;
                        match residency with
                        | Loaded ->
                          print_endline
                            "actual scheduler claims one Goal native Job; canonical \
                             ownership precedes one effect and factual completion"
                        | Cold ->
                          print_endline
                            "same queued Goal native Job owns cold Init/resume, one \
                             effect and factual completion"
                        | Job_wait ->
                          print_endline
                            "actual Job completion claims the same Goal Wait, finishes \
                             its callback and never replays")))))
;;

let%expect_test "actual scheduler executes one Goal native background Job" =
  run Loaded;
  [%expect
    {| actual scheduler claims one Goal native Job; canonical ownership precedes one effect and factual completion |}]
;;

let%expect_test "actual scheduler constructs cold for the same Goal native background Job"
  =
  run Cold;
  [%expect
    {| same queued Goal native Job owns cold Init/resume, one effect and factual completion |}]
;;

let%expect_test "actual native Job completion consumes its Goal Wait under the same owner"
  =
  run Job_wait;
  [%expect
    {| actual Job completion claims the same Goal Wait, finishes its callback and never replays |}]
;;
