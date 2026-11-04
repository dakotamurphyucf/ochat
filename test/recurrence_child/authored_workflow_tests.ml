open! Core
open Support
module Owner = Agent_server.Runtime_owner
module Cold = Activation_tests
module Held = Actor.Held_run
module Accounting = A.Inference_ledger
module Observation = Inference.Observation
module Res = Openai.Responses

let marker = "536_WORKFLOW_CHILD"

let prompt =
  {|<developer>Actual asynchronous authored Workflow startup.</developer>
<tool name="recipe_source" agent="workflow-recipe.chatmd" local/>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start ->
  let* child = Model.spawn("agent_prompt_v1", `Object([
    {key = "prompt"; value = `String("workflow-recipe.chatmd")},
    {key = "input"; value = `String("536_WORKFLOW_CHILD")},
    {key = "is_local"; value = `Bool(true)}
  ])) in
  Task.pure(state + 1)
| _ -> Task.pure(state)
</script>|}
;;

let startup_result child = function
  | Ok admission -> admission
  | Error error ->
    let current = state child in
    let receipts =
      List.map current.moderator_executions ~f:(fun execution ->
        execution.context.phase, execution.status)
    in
    let jobs = List.map current.jobs ~f:(fun job -> job.id, job.status) in
    raise_s
      [%sexp
        "actual Workflow Model.spawn startup"
      , (error : P.Error.t)
      , (current.lifecycle : A.Session_state.Lifecycle.t)
      , (current.runtime_initialization : A.Session_state.Runtime_initialization.t)
      , (receipts : (P.Moderator_execution.phase * P.Moderator_execution.status) list)
      , (jobs : (P.Id.Job.t * P.Job.status) list)]
;;

let selected_job (current : A.Session_state.t) =
  match
    List.filter current.jobs ~f:(fun job ->
      P.Job.equal_kind job.kind Model_call
      && Option.equal
           Jsonaf.exactly_equal
           (Jsonaf.member "payload" job.payload |> Option.bind ~f:(Jsonaf.member "input"))
           (Some (`String marker)))
  with
  | [ job ] -> job
  | jobs -> raise_s [%sexp "exact spawned Workflow child", (jobs : P.Job.t list)]
;;

let live_run (current : A.Session_state.t) admission =
  let index = Option.value_exn current.run_state in
  let run = A.Run_state.find index admission.P.Run_receipt.run_id |> Option.value_exn in
  assert (P.Run.Mode.equal run.mode Workflow);
  assert (P.Run_source.equal run.source admission.source);
  (match run.lifecycle with
   | Admitted | Active -> ()
   | Waiting _ | Terminal _ -> raise_s [%sexp "Workflow without Finish", (run : P.Run.t)]);
  let receipts =
    A.Run_state.receipts index
    |> List.filter ~f:(fun receipt -> P.Id.Run.equal receipt.run_id run.id)
  in
  [%test_eq: int]
    1
    (List.count receipts ~f:(fun receipt ->
       P.Run_receipt.Kind.equal receipt.kind Admission));
  assert (
    not
      (List.exists receipts ~f:(fun receipt ->
         P.Run_receipt.Kind.equal receipt.kind Terminal)));
  run
;;

let assert_startup_completed (current : A.Session_state.t) run =
  let startup =
    match
      List.filter current.moderator_executions ~f:(fun execution ->
        P.Moderator_execution.equal_phase execution.context.phase Session_start)
    with
    | [ execution ] -> execution
    | executions -> raise_s [%sexp (executions : P.Moderator_execution.t list)]
  in
  (match startup.status with
   | Completed _ -> ()
   | _ -> raise_s [%sexp (startup : P.Moderator_execution.t)]);
  assert (P.Invocation.equal_observer startup.context.source run.P.Run.source.observer);
  [%test_eq: int] run.source.generation startup.context.generation;
  assert (Option.is_none current.active_operation)
;;

let assert_completed_accounting current ~scope ~accounting_id =
  let row =
    Accounting.rows current.A.Session_state.inference_ledger
    |> List.find_exn ~f:(fun row ->
      let handle = Accounting.Row.handle row in
      Transcript.Scope.equal scope (Accounting.Handle.scope handle)
      && Observation.Observation_id.equal
           accounting_id
           (Accounting.Handle.accounting_id handle))
  in
  let record = Accounting.Row.record row in
  assert (Transcript.Scope.equal scope (Observation.Attempt_record.scope record));
  assert (
    Observation.Observation_id.equal
      accounting_id
      (Observation.Attempt_record.accounting_id record));
  match Observation.Attempt_record.state record with
  | Terminal terminal ->
    (match Inference.Event.Terminal.outcome terminal with
     | Completed -> ()
     | (Refused | Incomplete _ | Failed _) as outcome ->
       raise_s [%sexp (outcome : Inference.Event.Terminal.outcome)])
  | Prepared | Running | Interrupted _ ->
    failwith "spawned child accounting is not complete"
;;

let%test_unit "authored Workflow child first effect survives all startup lexical owners" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
      Mirage_crypto_rng_unix.use_default ();
      let root = T.temporary_root env in
      protect_cleanup
        ~finally:(fun () ->
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           let parent_file = Filename.concat root "parent.chatmd" in
           let target_file = Filename.concat root "workflow.chatmd" in
           List.iter
             [ ( parent_file
               , {|<developer>Actual recurrence controller.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
               )
             ; target_file, prompt
             ; ( Filename.concat root "workflow-recipe.chatmd"
               , "<developer>Return the offline Workflow result.</developer>" )
             ]
             ~f:(fun (path, contents) ->
               Eio.Path.save
                 ~create:(`Exclusive 0o600)
                 Eio.Path.(Eio.Stdenv.fs env / path)
                 contents);
           let counters : Cold.counters = { constructions = ref 0; providers = ref 0 } in
           let changed = Eio.Condition.create () in
           let progress = ref 0 in
           let notify () =
             Int.incr progress;
             Eio.Condition.broadcast changed
           in
           let admission_error = ref None in
           let lexical_closed = ref false in
           let provider_after_close = ref false in
           let admitted, admitted_u = Eio.Promise.create () in
           let release, release_u = Eio.Promise.create () in
           let released = ref false in
           let release_child () =
             if not !released
             then (
               released := true;
               Eio.Promise.resolve release_u ())
           in
           let policy =
             T.inference_policy
               ~default_model:"fixture-model"
               ~post_stream:(fun ~sw:_ ~inputs ->
                 Int.incr counters.providers;
                 provider_after_close := !lexical_closed;
                 let transcript =
                   `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
                 in
                 assert (String.is_substring transcript ~substring:marker);
                 Cold.completed_response "536-workflow-result")
           in
           let policy =
             { policy with
               runtime_inference_ports =
                 (fun actor ->
                   Result.map
                     (policy.runtime_inference_ports actor)
                     ~f:(fun (upstream : Agent_server.Graph_tracking.Upstream.t) ->
                       { upstream with
                         on_admitted =
                           (fun ~scope ~accounting_id ->
                             match
                               upstream.on_admitted ~scope ~accounting_id;
                               let job =
                                 selected_job (Actor.state actor |> protocol_ok)
                               in
                               Eio.Promise.resolve admitted_u (job, scope, accounting_id);
                               notify ();
                               Eio.Promise.await release
                             with
                             | () -> ()
                             | exception exn ->
                               let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                               admission_error := Some (exn, backtrace);
                               notify ();
                               Exn.raise_with_original_backtrace exn backtrace)
                       }))
             }
           in
           let configuration = T.config root root parent_file in
           let configured = List.hd_exn configuration.prompts in
           let configuration =
             { configuration with
               prompts =
                 configuration.prompts
                 @ [ { configured with id = "cold-activation-target"; path = target_file }
                   ]
             }
           in
           with_daemon
             ~policy
             ~on_runtime_construction:(fun () -> Int.incr counters.constructions)
             env
             root
             configuration
             ~provider_calls:counters.providers
             (fun sw daemon ->
                let connection = T.connection daemon principal in
                let handle = ref None in
                let child_ref = ref None in
                let client_error = ref None in
                let phase = ref "preparing actual child" in
                let latest = ref None in
                let await_progress name ready =
                  phase := name;
                  let rec loop () =
                    let generation = !progress in
                    Option.iter !client_error ~f:(fun error ->
                      raise_s [%sexp (error : P.Error.t)]);
                    Option.iter !admission_error ~f:(fun (exn, backtrace) ->
                      Exn.raise_with_original_backtrace exn backtrace);
                    Option.iter !child_ref ~f:(fun child ->
                      let current = state child in
                      latest := Some current;
                      List.iter current.jobs ~f:(fun job ->
                        match job.status with
                        | Failed _ | Cancelled | Interrupted _ ->
                          raise_s
                            [%sexp
                              "Workflow child failed before expected progress"
                            , (!phase : string)
                            , (job : P.Job.t)]
                        | Queued
                        | Running
                        | Waiting_permission _
                        | Waiting_completion _
                        | Succeeded -> ()));
                    match ready () with
                    | Some result -> result
                    | None ->
                      if Int.equal generation !progress
                      then Eio.Condition.await_no_mutex changed;
                      loop ()
                  in
                  loop ()
                in
                let with_progress_context f =
                  try f () with
                  | exn ->
                    let facts =
                      Option.map !latest ~f:(fun current ->
                        ( current.lifecycle
                        , current.runtime_initialization
                        , List.map current.moderator_executions ~f:(fun receipt ->
                            receipt.context.phase, receipt.status)
                        , current.jobs ))
                    in
                    Exn.reraise
                      exn
                      (Sexp.to_string_hum
                         [%sexp
                           "actual Workflow progress"
                         , (!phase : string)
                         , (facts
                            : (A.Session_state.Lifecycle.t
                              * A.Session_state.Runtime_initialization.t
                              * (P.Moderator_execution.phase
                                * P.Moderator_execution.status)
                                  list
                              * P.Job.t list)
                                option)])
                in
                protect_cleanup
                  ~finally:(fun () ->
                    release_child ();
                    Option.iter !handle ~f:Agent_client.Session_handle.close;
                    Agent_client.Connection.close connection)
                  (fun () ->
                     T.initialize connection;
                     let created, _ =
                       T.create_session ~start_immediately:true connection
                     in
                     let parent = loaded daemon created.id in
                     start_parent_settled env sw connection parent;
                     let original =
                       Operator_authorization.guarded ~principal ~is_current:(fun () ->
                         true)
                     in
                     Eio.Switch.run (fun worker_sw ->
                       let starting =
                         Eio.Fiber.fork_promise ~sw:worker_sw (fun () ->
                           protect_cleanup ~finally:notify (fun () ->
                             let result =
                               with_planner env sw daemon parent (fun () ->
                                 Cold.with_cold_held
                                   ~mode:Workflow
                                   ~catalog_target:true
                                   ~before_prepare:(fun child ->
                                     child_ref := Some child;
                                     notify ();
                                     handle
                                     := Some
                                          (Agent_client.Session_handle.attach
                                             ~sw
                                             ~clock:(Eio.Stdenv.clock env)
                                             ~connection
                                             ~session_id:(state child).identity.session_id
                                             ~mode:Read_write
                                             ~on_update:(fun _ -> notify ())
                                             ~on_error:(fun error ->
                                               client_error := Some error;
                                               notify ())
                                             ()
                                           |> protocol_ok))
                                   env
                                   daemon
                                   parent
                                   ~root_file:target_file
                                   ~counters
                                   ~original
                                   (fun child prepared held checked _intent _attempt ->
                                      let admission = Held.admission held in
                                      let returned =
                                        Owner.start_held_authored_runtime
                                          child.runtime
                                          ~prepared
                                          ~held
                                          ~checked
                                        |> startup_result child
                                      in
                                      assert (P.Run_receipt.equal returned admission);
                                      Ok (child, admission)))
                             in
                             lexical_closed := true;
                             result))
                       in
                       protect_cleanup ~finally:release_child (fun () ->
                         with_progress_context (fun () ->
                           (* Await the whole planner/source/preparation/permit CPS,
                               not merely the startup callback or first provider. *)
                           let child, admission =
                             await_progress "waiting for complete startup CPS" (fun () ->
                               Option.map (Eio.Promise.peek starting) ~f:(fun _ ->
                                 Eio.Promise.await_exn starting))
                           in
                           assert !lexical_closed;
                           let job, scope, accounting_id =
                             await_progress
                               "waiting for real child allocation ACK"
                               (fun () -> Eio.Promise.peek admitted)
                           in
                           [%test_eq: int] 0 !(counters.providers);
                           [%test_eq: int] 1 job.attempt;
                           (match job.status with
                            | Running -> ()
                            | _ -> raise_s [%sexp (job : P.Job.t)]);
                           let before = state child in
                           let run = live_run before admission in
                           assert_startup_completed before run;
                           assert (Owner.is_loaded child.runtime);
                           assert (
                             Option.equal
                               P.Inference_origin.equal
                               job.inference_origin
                               (Some (Run run.id)));
                           let work = A.Run_job_occurrence.work job |> protocol_ok in
                           assert (List.mem run.owned_work work ~equal:P.Run_work.equal);
                           assert (
                             not
                               (List.exists run.terminal_work ~f:(fun proof ->
                                  P.Run_work.equal proof.work work)));
                           phase := "waiting for actual child completion";
                           release_child ();
                           let rec await_completed () =
                             let generation = !progress in
                             Option.iter !client_error ~f:(fun error ->
                               raise_s [%sexp (error : P.Error.t)]);
                             let current = state child in
                             latest := Some current;
                             let actual = selected_job current in
                             assert (P.Id.Job.equal actual.id job.id);
                             match actual.status with
                             | Succeeded -> current, actual
                             | Queued
                             | Running
                             | Waiting_permission _
                             | Waiting_completion _ ->
                               if Int.equal generation !progress
                               then Eio.Condition.await_no_mutex changed;
                               await_completed ()
                             | Failed _ | Cancelled | Interrupted _ ->
                               raise_s [%sexp "Workflow child failed", (actual : P.Job.t)]
                           in
                           let after, completed = await_completed () in
                           [%test_eq: int] 1 !(counters.providers);
                           assert !provider_after_close;
                           [%test_eq: int] 1 completed.attempt;
                           assert (
                             Option.equal
                               Jsonaf.exactly_equal
                               (Option.bind
                                  completed.result
                                  ~f:(Jsonaf.member "final_text"))
                               (Some (`String "536-workflow-result")));
                           let run = live_run after admission in
                           assert_startup_completed after run;
                           assert (
                             Option.equal
                               P.Inference_origin.equal
                               completed.inference_origin
                               (Some (Run run.id)));
                           (match
                              List.filter run.terminal_work ~f:(fun proof ->
                                P.Run_work.equal proof.work work)
                            with
                            | [ proof ] ->
                              assert (
                                P.Run_work.Terminal.equal_outcome proof.outcome Succeeded)
                            | proofs ->
                              raise_s [%sexp (proofs : P.Run_work.Terminal.t list)]);
                           assert_completed_accounting after ~scope ~accounting_id;
                           assert (Owner.is_loaded child.runtime);
                           (* Closing the fixture may then interrupt this live
                               Workflow; no Finish or terminal receipt is invented. *)
                           child.close ()))))))))
;;
