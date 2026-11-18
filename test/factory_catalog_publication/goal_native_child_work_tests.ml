open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Manual = Goal_job_runtime_tests
module Native = Goal_native_callback_support
module Controls = Goal_native_control_tests
module G = P.Goal
module N = A.Native_tool_invocation
module Admission = A.Goal_native_admission
module C = Chat_response.Tool_capability
module M = Chat_response.Moderator_manager
module Jobs = A.Script_job_service
module Capacity = Agent_server.Job_capacity

(* The qualified extension runtime stays enabled. Existing host pause policy
   keeps idle observations/follow-ups pending, and the already qualified controlled
   polling clock parks the real JobScheduler before this entry is registered. *)
let prompt =
  Manual.prompt
  |> String.substr_replace_all
       ~pattern:"kind=\"moderator\">"
       ~with_:"kind=\"moderator\" api=\"extensibility-v1\">"
  |> String.substr_replace_all
       ~pattern:"| _ -> Task.pure(state)"
       ~with_:
         {| | `Tool_observed(_) ->
  let* result = Tool.call("goal_hold", `Object([])) in
  (match result with
   | `Error(message) -> Task.fail(message)
   | `Ok(_) -> let* child = Job.start_tool("goal_get", `Object([])) in Task.pure(state + 1))
| _ -> Task.pure(state)|}
;;

let native_call selected ~execute ~invocation =
  let reference =
    C.find selected ~name:invocation.P.Invocation.context.tool_name
    |> Result.map_error ~f:(fun error -> error.C.message)
    |> Result.ok_or_failwith
    |> C.reference
  in
  N.run_scoped
    ~execute
    ~registry:(fun () -> selected)
    ~reference
    ~invocation
    ~is_halted:(fun () -> false)
    ~authorize:(fun _ _ -> Ok ())
    ~prepare_output:(function
      | Text text -> Ok (`String text)
      | _ -> Error (P.Error.invalid_request "fixture expected disclosed Text"))
;;

let invocation
      selected
      current
      runtime
      ~name
      ~observer
      ?parent_event
      ?parent_invocation
      ()
  =
  let reference =
    C.find selected ~name
    |> Result.map_error ~f:(fun error -> error.C.message)
    |> Result.ok_or_failwith
    |> C.reference
  in
  P.Invocation.create
    ~observer
    ?parent_event
    { id = P.Id.Invocation.create ()
    ; session_id = current.A.Session_state.identity.session_id
    ; generation = current.identity.generation
    ; origin = Moderator
    ; provider_call_id = None
    ; call_entry_id = None
    ; parent_invocation
    ; parent_job = None
    ; tool_name = name
    ; implementation_revision = reference.implementation_revision
    ; capability_fingerprint = C.fingerprint selected
    ; input = `Object []
    ; created_at = runtime.A.Runtime_builder.now ()
    ; deadline = None
    }
  |> protocol_ok
;;

let jobs_service env entry selected capacity staged =
  let host : Jobs.host =
    { stage =
        (fun owner request ->
          let open Result.Let_syntax in
          let%bind job =
            Actor.prepare_background_job_launch entry.Registry.actor ~owner request
          in
          let current = state entry in
          let key =
            Capacity.Key.create
              ~principal_id:current.identity.creating_principal
              ~prompt:
                (Option.value_map
                   current.spec.prompt_definition_id
                   ~default:"<local>"
                   ~f:P.Id.Prompt_definition.to_string)
              ~workspace_conflict_domain:current.spec.workspace_instance.conflict_domain
              ~session_id:job.session_id
              ~kind:job.kind
              ~nested_depth:
                (Option.value_map job.launch ~default:0 ~f:(fun launch ->
                   launch.nested_depth))
          in
          let%bind reservation = Capacity.reserve_job capacity key ~job in
          let%bind reservation =
            Result.of_option
              reservation
              ~error:(P.Error.invalid_request "fixture capacity exhausted")
          in
          let result =
            Actor.stage_background_job
              entry.actor
              ~job
              ~capacity:
                { publish = (fun () -> Capacity.publish reservation)
                ; abort = (fun () -> Capacity.abort reservation)
                }
          in
          match result with
          | Error error ->
            Capacity.abort reservation;
            Error error
          | Ok () ->
            staged := Some job;
            Ok job)
    ; select = (fun owner ids -> Actor.select_background_jobs entry.actor ~owner ~ids)
    ; abort =
        (fun owner id -> Actor.abort_background_job entry.actor ~owner ~id |> protocol_ok)
    ; get = (fun owner id -> Actor.read_script_job entry.actor ~owner ~id)
    ; materialize =
        (fun owner expected -> Actor.read_script_job_result entry.actor ~owner ~expected)
    ; cancel = (fun owner id -> Actor.cancel_script_job entry.actor ~owner ~id)
    }
  in
  Jobs.create
    ~env
    ~policy:Chat_response.One_off_request.default_policy
    ~current_capabilities:(fun () -> selected)
    ~prepare_source:(fun ~binding:_ ~owner:_ ~request:_ ~checkpoint ->
      checkpoint ();
      Ok None)
    ~host
;;

let%expect_test "Tool_observed native Hold and staged child share the projected Run index"
  =
  let counters : Retry_support.counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr counters.providers;
      completed_response "558 genuine Goal parent")
  in
  let pause = ref ignore in
  let resume = ref ignore in
  let clock_now = ref (fun () -> failwith "controlled daemon clock absent") in
  Creation_tests.with_daemon
    ~startup_mode:Execute
    ~wrap_env:(fun env ->
      Goal_background_runtime_tests.with_controlled_polling env ~pause ~resume ~clock_now)
    ~prompt_text:prompt
    ~qualify_chatml_extensions:true
    ~sources:[ "held-job.chatmd", "<developer>Actual owned source.</developer>" ]
    ~policy
    (fun env _sw _root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         protect_cleanup
           ~finally:(fun () -> !resume ())
           (fun () ->
              !pause ();
              let rec await_scheduler_sleep () =
                match Daemon.For_testing.job_scheduler_wait_deadline daemon with
                | Some deadline when Float.(deadline > !clock_now ()) -> ()
                | Some _ | None ->
                  Eio.Fiber.yield ();
                  await_scheduler_sleep ()
              in
              await_scheduler_sleep ();
              Deferred_source_support.with_loaded_entry
                daemon
                (fun ~entry ~runtime ~revision:_ ->
                   (* Join the genuine qualified v1 startup first. Pausing idle work
                   only afterward keeps its real Session_start owner intact. *)
                   Actor.set_automatic_turn_pauses
                     entry.actor
                     [ Pause_internal_event_drains; Pause_followup_turns ]
                   |> protocol_ok;
                   let session, before = Goal.create_goal daemon entry in
                   let request =
                     A.Run_admission_request.Headless.create
                       ~session_id:before.identity.session_id
                       ~generation:before.identity.generation
                       ~expected_revision:before.counters.revision
                       ~mode:Workflow
                       ~input:
                         (User_submission
                            { kind = Plain_text; text = Manual.marker; attachments = [] })
                       ~key:(Controls.key "558-mixed-original")
                     |> protocol_ok
                     |> fun request ->
                     A.Goal_activation_request.create
                       ~goal_id:Goal.goal_id
                       ~expected_goal_revision:(G.revision (Goal.goal before))
                       ~expected_catalog_revision:
                         (A.Goal_catalog.revision (Option.value_exn before.goal_catalog))
                       ~request
                     |> protocol_ok
                   in
                   let service =
                     Agent_server.Goal_run_service.create
                       ~factory:(Daemon.factory daemon)
                       ~authorize:(fun _ current -> authorize current)
                   in
                   let admission =
                     Agent_server.Goal_run_service.start_episode
                       service
                       ~entry
                       ~actor:original
                       ~session
                       ~request
                     |> protocol_ok
                     |> Goal.activated
                   in
                   let active = Goal.await_input env entry counters in
                   let queued = Manual.one_job active in
                   let claimed =
                     Actor.claim_job
                       entry.actor
                       ~job_id:queued.id
                       ~generation:queued.generation
                     |> protocol_ok
                     |> Option.value_exn
                   in
                   let manager =
                     Option.value_exn runtime.A.Runtime_builder.moderator_manager
                   in
                   let retained = ref [] in
                   let control = ref None in
                   let selected =
                     Native.registry_many
                       entry
                       ~retained
                       ~operations:
                         [ ( Admission.Effect.Get
                           , fun admission ->
                               Actor.native_goal_state
                                 entry.actor
                                 ~admission
                                 ~authorize:Goal_native_test_support.authorize
                               |> Result.map ~f:ignore )
                         ; ( Hold
                           , fun admission ->
                               Controls.stage_control
                                 entry
                                 (Option.value_exn !control)
                                 admission )
                         ]
                   in
                   let observer = M.invocation_observer manager |> Option.value_exn in
                   let seed = ref None in
                   Actor.with_job_execution
                     entry.actor
                     ~job_id:claimed.id
                     ~generation:claimed.generation
                     ~attempt:claimed.attempt
                     ~deadline:None
                     (fun services ->
                        (* Job-owned moderator callbacks are actual tool boundaries,
                           not model Turn_end events. This is the selected Get which
                           the callback executes below and records as its observation. *)
                        services.claim_event
                          ~event:
                            (Pre_tool_call
                               { id = "558-selected-goal-get"
                               ; name = "goal_get"
                               ; args = `Object []
                               ; kind = Function
                               ; payload_text = "{}"
                               ; meta = `Null
                               })
                          ~snapshot:(fun () ->
                            M.identity_snapshot manager
                            |> Result.map_error ~f:P.Error.invalid_request)
                          (fun ~executing
                            ~policy_caller:_
                            ~model_jobs:_
                            ~allocate_inference:_
                            ~retirement_reason:_
                            ~event:_
                            ~execute
                            ~commit ->
                             let open Result.Let_syntax in
                             let native =
                               invocation
                                 selected
                                 (state entry)
                                 runtime
                                 ~name:"goal_get"
                                 ~observer
                                 ~parent_event:executing.context.id
                                 ()
                             in
                             let%bind completed =
                               native_call selected ~execute ~invocation:native
                             in
                             seed := Some completed;
                             let%bind owner = Actor.run_actions entry.actor executing in
                             let owner = Option.value_exn owner in
                             let transaction = A.Run_action_service.transaction owner in
                             let ticket =
                               transaction.handlers.stage Continue
                               |> Result.ok_or_failwith
                             in
                             let action =
                               transaction.prepare [ ticket ] |> Result.ok_or_failwith
                             in
                             let requests =
                               A.Run_action_service.compose_requests
                                 owner
                                 ~action
                                 ~requests:
                                   { request_turn = false
                                   ; request_compaction = false
                                   ; end_session = None
                                   }
                               |> Result.ok_or_failwith
                             in
                             commit
                               ~snapshot:
                                 (M.identity_snapshot manager |> Result.ok_or_failwith)
                               ~requests))
                   |> protocol_ok
                   |> ignore;
                   let before = state entry in
                   let previous = Manual.run before admission in
                   let intent =
                     List.find_exn
                       (A.Run_state.intents (Option.value_exn before.run_state))
                       ~f:(fun intent ->
                         P.Id.Run.equal intent.A.Run_intent.receipt.run_id previous.id
                         &&
                         match intent.action, intent.disposition with
                         | Continue, Pending -> true
                         | _ -> false)
                   in
                   assert (
                     Option.equal
                       P.Inference_origin.equal
                       (Some (Run previous.id))
                       (Option.value_exn !seed).inference_origin);
                   control
                   := Some
                        (Controls.control_request
                           before
                           ~key:(Controls.key "558-mixed-hold")
                           (Hold Controls.reason));
                   let staged = ref None in
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
                   let jobs = jobs_service env entry selected capacity staged in
                   let run_before = ref None in
                   let ran =
                     Actor.with_idle_moderator_observation_tools
                       entry.actor
                       ~observer
                       (fun ~observing ~capabilities:_ ~execute ~commit ->
                          assert (
                            P.Id.Invocation.equal
                              observing.context.id
                              (Option.value_exn !seed).context.id);
                          Jobs.with_scope
                            jobs
                            ~owner:(Invocation observing.context.id)
                            ~selected
                            ~error:P.Error.invalid_request
                            (fun scope ->
                               M.handle_observation_entries
                                 manager
                                 ~jobs:(Jobs.moderator_transaction scope)
                                 ~invocation:observing
                                 ~history:[]
                                 ~available_tools:[]
                                 ~session_meta:`Null
                                 ~now_ms:0
                                 ~on_tool_call:(fun ~name ~args:_ ->
                                   let invocation =
                                     invocation
                                       selected
                                       (state entry)
                                       runtime
                                       ~name
                                       ~observer
                                       ~parent_invocation:observing.context.id
                                       ()
                                   in
                                   native_call selected ~execute ~invocation
                                   |> Result.map_error ~f:(fun error ->
                                     error.P.Error.message)
                                   |> Result.bind ~f:(fun invocation ->
                                     match invocation.P.Invocation.status with
                                     | Resolved (Complete value) ->
                                       Ok
                                         (Chat_response.Moderation.Capabilities.Tool_ok
                                            value)
                                     | _ -> Error "selected Hold did not complete"))
                                 ~prepare_observation:
                                   (fun
                                     ~observed ~outcome:_ ~snapshot ->
                                   run_before := Some (state entry);
                                   Ok
                                     { M.persist =
                                         (fun () ->
                                           commit ~resolved:observed ~snapshot
                                           |> Result.map_error ~f:(fun error ->
                                             error.P.Error.message))
                                     ; install = ignore
                                     })
                               |> Result.map ~f:ignore
                               |> Result.map_error ~f:P.Error.invalid_request))
                     |> protocol_ok
                   in
                   assert ran;
                   let current = state entry in
                   let child = Option.value_exn !staged in
                   let child =
                     List.find_exn current.jobs ~f:(fun job ->
                       P.Id.Job.equal job.id child.id)
                   in
                   (match child.status with
                    | Queued -> ()
                    | _ -> raise_s [%sexp (child : P.Job.t)]);
                   assert (
                     Option.equal
                       P.Inference_origin.equal
                       child.inference_origin
                       (Some (Run previous.id)));
                   assert (
                     G.State.equal
                       (G.state (Goal.goal current))
                       (Open (Held (Needs_input Controls.reason))));
                   let index = Option.value_exn current.run_state in
                   assert (
                     List.exists (A.Run_state.intents index) ~f:(fun current ->
                       P.Run_receipt.equal current.receipt intent.receipt
                       &&
                       match current.disposition with
                       | Retired -> true
                       | Pending | Consumed _ -> false));
                   let run = Manual.run current admission in
                   assert (
                     List.exists run.owned_work ~f:(fun work ->
                       match work.P.Run_work.key with
                       | Retained (Job { id; attempt }) ->
                         P.Id.Job.equal id child.id && Int.equal attempt 1
                       | _ -> false));
                   [%test_eq: int64] Int64.(succ previous.revision) run.revision;
                   assert (
                     A.Goal_accounting.equal
                       (Goal.accounting before)
                       (Goal.accounting current));
                   [%test_eq: int] 1 !(counters.providers);
                   let checkpoint_before = Option.value_exn !run_before in
                   (* The native child has separate Invocation ACKs before this final
              selected observation checkpoint. Sample after those ACKs. *)
                   [%test_eq: int64]
                     Int64.(succ checkpoint_before.counters.revision)
                     current.counters.revision;
                   [%test_eq: int]
                     (List.length before.jobs + 1)
                     (List.length current.jobs);
                   ()))));
  print_endline
    "actual observation Hold; old Continue retired; selected child retained in same Run \
     advance";
  [%expect
    {| actual observation Hold; old Continue retired; selected child retained in same Run advance |}]
;;

module Caused_async = struct
  include Caused_async_support

  let model_marker = "597_CAUSED_ASYNC_INVOCATION_MODEL"
  let model_recipe = "597-caused-async-model.chatmd"
  let model_output = "actual caused Async Invocation Model result"

  let parent_prompt =
    {|<developer>One real caused Async Job for the actual trusted Job host.</developer>
<tool name="async_model_recipe" agent="597-caused-async-model.chatmd" local/>
<script id="parent-async-policy" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Pre_tool_call(p) ->
  if (if state == 0 then p.name == "read_file" else false) then
    let* job = Tool.spawn("async_model_probe", `Object([])) in Task.pure(state + 1)
  else Task.pure(state)
| `Tool_invoked(p) ->
  if p.context.tool_name == "async_model_probe" then
    let* result = Model.call("agent_prompt_v1", `Object([
      {key = "prompt"; value = `String("597-caused-async-model.chatmd")},
      {key = "input"; value = `String("597_CAUSED_ASYNC_INVOCATION_MODEL")},
      {key = "is_local"; value = `Bool(true)}
    ])) in
    (match result with
    | `Ok(value) ->
      let* ignored = Invocation.resolve(p.context.invocation_id, `Complete(value)) in
      Task.pure(state + 1)
    | `Refused(message) -> Task.fail(message)
    | `Error(message) -> Task.fail(message))
  else Task.pure(state)
| _ -> Task.pure(state)
</script>
<tool name="async_model_probe" type="moderator" moderator="parent-async-policy" input_schema="597-async-model-input.json" output_schema="597-async-model-output.json"/>|}
  ;;

  type model_chain =
    { job : P.Job.t
    ; invocation : P.Invocation.t
    ; background : P.Invocation.t
    }

  let model_job parent =
    List.filter parent.A.Session_state.jobs ~f:(fun job ->
      P.Job.equal_kind Model_call job.kind
      && Option.equal
           String.equal
           (Jsonaf.member "payload" job.payload
            |> Option.bind ~f:(Jsonaf.member "input")
            |> Option.bind ~f:Jsonaf.string)
           (Some model_marker))
    |> Model.only "actual Invocation-owned Model Job"
  ;;

  let observe_model (t : probe) =
    let model = model_job (state t.parent) in
    let observed = observe_attempt t model in
    let parent = state t.parent in
    let rows = Model.new_rows t.before_parent parent in
    let cause = observed.cause in
    let accounting = Option.value_exn parent.policy_canonical_usage in
    let job = List.find_exn parent.jobs ~f:(fun job -> P.Id.Job.equal job.id t.job.id) in
    let launch = Option.value_exn job.launch in
    let prior = List.hd_exn t.observed in
    assert (Cause.equal prior.cause cause);
    let previous =
      List.find_exn rows ~f:(fun row ->
        Ledger.Handle.equal prior.handle (Ledger.Row.handle row))
    in
    (match Inference.Observation.Attempt_record.state (Ledger.Row.record previous) with
     | Terminal terminal ->
       (match Inference.Event.Terminal.outcome terminal with
        | Completed -> ()
        | Refused | Incomplete _ | Failed _ -> failwith "direct Async H did not complete")
     | Prepared | Running | Interrupted _ -> failwith "direct Async H did not settle");
    assert (not (P.Id.Job.equal model.id job.id));
    assert (P.Id.Session.equal parent.identity.session_id model.session_id);
    [%test_eq: int] parent.identity.generation model.generation;
    [%test_eq: int] 1 model.attempt;
    (match model.status with
     | Running -> ()
     | Queued
     | Waiting_permission _
     | Waiting_completion _
     | Succeeded
     | Failed _
     | Cancelled
     | Interrupted _ -> failwith "Model provider lost its actual Running Job");
    let model_launch = Option.value_exn model.launch in
    let invocation =
      match P.Job.Birth_parent.view model_launch.owner with
      | Callback (Invocation id) ->
        List.find_exn parent.invocations ~f:(fun invocation ->
          P.Id.Invocation.equal invocation.context.id id)
      | Callback (Moderator_event _) | Job_attempt _ ->
        failwith "Model birth lost its actual Invocation owner"
    in
    [%test_eq: string] "async_model_probe" invocation.context.tool_name;
    assert (P.Invocation.equal_origin Script invocation.context.origin);
    assert (Option.is_none invocation.context.parent_job);
    assert (Option.is_none invocation.parent_event);
    let background =
      let id = Option.value_exn invocation.context.parent_invocation in
      List.find_exn parent.invocations ~f:(fun invocation ->
        P.Id.Invocation.equal invocation.context.id id)
    in
    [%test_eq: string] "chatml.background" background.context.tool_name;
    assert (P.Invocation.equal_origin Script background.context.origin);
    assert (Option.is_none background.context.parent_invocation);
    assert (Option.is_none background.parent_event);
    assert (Option.equal P.Id.Job.equal (Some job.id) background.context.parent_job);
    List.iter [ invocation; background ] ~f:(fun invocation ->
      assert (P.Id.Session.equal parent.identity.session_id invocation.context.session_id);
      [%test_eq: int] parent.identity.generation invocation.context.generation;
      match invocation.status with
      | Dispatching -> ()
      | Admitted | Resolved _ | Published _ ->
        failwith "Model provider lost its actual dispatched Invocation chain");
    (match model_launch.parent_job with
     | Some (id, attempt) ->
       assert (P.Id.Job.equal job.id id);
       [%test_eq: int] job.attempt attempt
     | None -> failwith "Model launch lost its actual Async ancestor");
    [%test_eq: int] (launch.nested_depth + 1) model_launch.nested_depth;
    assert (
      Option.equal
        P.Invocation.equal_observer
        model_launch.moderator_source
        (Option.map invocation.observation ~f:(fun observation -> observation.observer)));
    assert (Option.is_some model_launch.moderator_source);
    assert (
      Option.equal P.Inference_origin.equal job.inference_origin model.inference_origin);
    let _, source = A.Moderator_source_installation.capture parent |> protocol_ok in
    assert (P.Run_source.equal source (Cause.callee_source cause));
    let prefix =
      PC.Source.Native_prefix.create
        ~session_id:parent.identity.session_id
        ~generation:parent.identity.generation
        ~current_source:(Some source)
        ~events:parent.moderator_executions
        ~invocations:parent.invocations
        ~jobs:parent.jobs
        ~runs:parent.run_state
      |> protocol_ok
    in
    let members =
      [ Cause.Member.async_job job.id ~generation:job.generation
      ; Cause.Member.invocation
          background.context.id
          ~generation:background.context.generation
      ; Cause.Member.invocation
          invocation.context.id
          ~generation:invocation.context.generation
      ; Cause.Member.model_job model.id ~generation:model.generation
      ]
    in
    List.iter members ~f:(fun member ->
      let actual =
        PC.Source.cause (Some accounting) ~prefix ~member:(protocol_ok member)
        |> protocol_ok
        |> Option.value_exn
      in
      assert (Cause.equal cause actual));
    observed, { job = model; invocation; background }
  ;;
end

let%expect_test "real caused Async direct H confirms both genuine Goals before provider" =
  let module T = Caused_async in
  let model_providers = ref 0 in
  let modeled = ref None in
  T.with_claimed_async
    ~scenario:T.Invocation_model
    ~parent_prompt_text:T.parent_prompt
    ~sources:
      [ T.original_file, "real delegated read"
      ; ( T.model_recipe
        , "<developer>597 actual caused Async Invocation Model recipe.</developer>" )
      ; "597-async-model-input.json", {|{"type":"object","additionalProperties":false}|}
      ; ( "597-async-model-output.json"
        , {|{"type":"object","required":["final_text"],"properties":{"final_text":{"type":"string"}}}|}
        )
      ]
    ~model_marker:T.model_marker
    ~at_model:(fun failure subject ->
      T.Failure.guard failure (fun () ->
        Int.incr model_providers;
        assert (Option.is_none !modeled);
        modeled := Some (T.observe_model subject);
        completed_response T.model_output))
    (fun env sw daemon fixture ->
       T.run_direct sw fixture;
       [%test_eq: int] 0 !model_providers;
       let parent = fixture.probe.parent in
       T.execute_async daemon fixture ~check_completion:(function
         | Succeeded value ->
           assert (
             Option.equal
               String.equal
               (Some T.model_output)
               (Jsonaf.member "final_text" value |> Option.bind ~f:Jsonaf.string));
           assert (
             Option.equal
               String.equal
               (Some "agent_prompt_v1")
               (Jsonaf.member "recipe" value |> Option.bind ~f:Jsonaf.string));
           assert (
             Option.equal
               String.equal
               (Some T.model_recipe)
               (Jsonaf.member "prompt" value |> Option.bind ~f:Jsonaf.string));
           assert (
             Option.equal
               String.equal
               (Some T.model_recipe)
               (Jsonaf.member "session_id" value |> Option.bind ~f:Jsonaf.string));
           assert (Jsonaf.exactly_equal `True (Jsonaf.member_exn "is_local" value));
           assert (
             Jsonaf.exactly_equal `True (Jsonaf.member_exn "terminated_normally" value));
           let _, chain = Option.value_exn !modeled in
           let current = state parent in
           let model = T.model_job current in
           assert (P.Id.Job.equal chain.job.id model.id);
           [%test_eq: int] chain.job.attempt model.attempt;
           assert (Option.equal P.Job.equal_launch chain.job.launch model.launch);
           assert (Option.is_some model.completed_at);
           assert (Option.equal Jsonaf.exactly_equal (Some value) model.result);
           (match model.status with
            | Succeeded -> ()
            | Queued
            | Running
            | Waiting_permission _
            | Waiting_completion _
            | Failed _
            | Cancelled
            | Interrupted _ -> failwith "actual Model did not succeed");
           List.iter [ chain.invocation; chain.background ] ~f:(fun before ->
             let invocation =
               List.find_exn current.invocations ~f:(fun invocation ->
                 P.Id.Invocation.equal before.context.id invocation.context.id)
             in
             assert (P.Invocation.equal_context before.context invocation.context);
             match invocation.status with
             | Resolved (Complete actual) -> assert (Jsonaf.exactly_equal value actual)
             | Admitted | Dispatching | Resolved _ | Published _ ->
               failwith "actual Async Invocation chain did not resolve Model output")
         | completion -> raise_s [%sexp (completion : P.Completion.t)]);
       let final_parent = T.finish_child env fixture in
       [%test_eq: int] 1 !model_providers;
       let direct = List.hd_exn fixture.probe.observed in
       let modeled, _ = Option.value_exn !modeled in
       assert (not (T.Ledger.Handle.equal direct.handle modeled.handle));
       List.iter fixture.probe.observed ~f:(fun observed ->
         T.row final_parent observed.handle |> T.assert_completed));
  print_endline
    "actual caused Async and Invocation Model Handles confirmed both Goals before \
     providers; real Async Job completed";
  [%expect
    {| actual caused Async and Invocation Model Handles confirmed both Goals before providers; real Async Job completed |}]
;;
