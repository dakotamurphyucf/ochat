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
      completed_response "545 original Goal turn")
  in
  Creation_tests.with_daemon
    ~prompt_text:prompt
    ~qualify_chatml_extensions:true
    ~prepare_sources:(fun env root ->
      let path = Filename.concat root effect_file in
      effect_path := Some path;
      let source =
        String.substr_replace_all
          prompt
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
        | Cold ->
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
               | Cold ->
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
                   protect_cleanup ~finally:release_effect (fun () ->
                     let session, before = Goal.create_goal daemon entry in
                     let admission =
                       Resume.start_episode daemon entry original session before
                     in
                     let cold_basis =
                       match residency with
                       | Loaded -> None
                       | Cold ->
                         let initial = Goal.await_input env entry counters in
                         let queued = one_job initial in
                         [%test_eq: int] 0 queued.attempt;
                         (match queued.status with
                          | Queued -> ()
                          | Running
                          | Waiting_permission _
                          | Waiting_completion _
                          | Succeeded
                          | Failed _
                          | Cancelled
                          | Interrupted _ ->
                            failwith "paused scheduler did not preserve actual queued Job");
                         (match queued.inference_origin with
                          | Some (Run id) -> assert (P.Id.Run.equal admission.run_id id)
                          | Some Unbound | None ->
                            failwith "actual queued native Job lost its Goal Run");
                         assert (
                           G.State.equal (Open Enabled) (G.state (Goal.goal initial)));
                         [%test_eq: int] 1 (List.length (Manual.startup initial));
                         Owner.unload_and_wait entry.runtime |> protocol_ok;
                         assert (not (Owner.is_loaded entry.runtime));
                         let cold = Resume.checkpoint entry in
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
                      | Cold ->
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
                      | Cold, Some cold ->
                        (* The cold executor deliberately retains its source lease
                      across this native effect; borrowing it through ordinary
                      Owner APIs here would violate that exclusive lane. *)
                        assert (not (Owner.source_lane_idle entry.runtime));
                        assert (Option.is_none executing.active_operation);
                        let queued = one_job cold in
                        assert (P.Id.Job.equal queued.id job.id);
                        assert (Int.equal queued.generation job.generation);
                        assert (Jsonaf.exactly_equal queued.payload job.payload);
                        assert (Option.equal P.Job.equal_launch queued.launch job.launch);
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
                      | Loaded, Some _ | Cold, None ->
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
                         if Int64.equal turns.completed 1L
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
                        | Cold -> 2)
                       (List.length (Manual.startup final));
                     Owner.with_loaded_runtime entry.runtime (fun runtime ->
                       (match residency with
                        | Loaded -> assert (phys_equal initial_runtime runtime)
                        | Cold -> assert (not (phys_equal initial_runtime runtime)));
                       Ok ())
                     |> protocol_ok;
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
                         "same queued Goal native Job owns cold Init/resume, one effect \
                          and factual completion")))))
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
