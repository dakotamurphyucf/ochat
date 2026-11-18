open! Core
open Agent_server_test_support
open Retry_support
module Manual = Goal_job_runtime_tests
module Goal = Goal_service_tests
module Resume = Goal_resume_runtime_tests
module Model = Delegated_policy_model_tests
module Ledger = A.Inference_ledger
module Account = A.Goal_accounting
module PC = A.Policy_canonical_usage
module M = Chat_response.Moderator_manager
module B = A.Runtime_builder
module Observation = Inference.Observation

(* Trusted lower-host composition, as in Manual: the actual Factory/Manager and
   Actor own admission. Operator_only has no automatic Job dispatcher. This does
   not claim automatic daemon legacy-boundary routing or v1 policy authority. *)
let nested_marker = "597_LEGACY_JOB_EVENT_MODEL"

let prompt =
  {|<developer>597 actual legacy Job callback Model receiver.</developer>
<tool name="job_recipe" agent="held-job.chatmd" local/>
<script id="owner" language="chatml" kind="moderator">
let initial_state = 0
let payload text = `Object([
  {key = "prompt"; value = `String("held-job.chatmd")},
  {key = "input"; value = `String(text)},
  {key = "is_local"; value = `Bool(true)}
])
let on_event ctx state event = match event with
| `Turn_start ->
  if state == 0 then
    let* child = Model.spawn("agent_prompt_v1", payload("545_LOADED_GOAL_JOB")) in
    Task.pure(state + 1)
  else Task.pure(state)
| `Pre_tool_call(p) ->
  if state == 1 then
    let* result = Model.call("agent_prompt_v1", payload("597_LEGACY_JOB_EVENT_MODEL")) in
    let* checked = (match result with
    | `Ok(value) -> Task.pure(value)
    | `Refused(message) -> Task.fail(message)
    | `Error(message) -> Task.fail(message)) in
    Task.pure(state + 1)
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
;;

type phase =
  | Initial
  | Nested
  | Parent
[@@deriving equal]

type admitted =
  { phase : phase
  ; scope : Transcript.Scope.t
  ; accounting_id : Observation.Observation_id.t
  ; job : P.Job.t
  }

type subject =
  { entry : Registry.entry
  ; before : A.Session_state.t
  ; admission : P.Run_receipt.t
  ; parent : P.Job.t
  ; source : P.Run_source.t
  ; mutable callback : Actor.Callback_scope.t option
  }

let job_named current marker =
  List.filter current.A.Session_state.jobs ~f:(fun job ->
    P.Job.equal_kind Model_call job.kind
    && Option.equal
         Jsonaf.exactly_equal
         (Jsonaf.member "payload" job.payload |> Option.bind ~f:(Jsonaf.member "input"))
         (Some (`String marker)))
  |> Model.only "actual tagged legacy Model Job"
;;

let assert_job_identity (before : P.Job.t) (after : P.Job.t) =
  assert (P.Id.Job.equal before.id after.id);
  assert (P.Id.Session.equal before.session_id after.session_id);
  [%test_eq: int] before.generation after.generation;
  assert (P.Job.equal_kind before.kind after.kind);
  assert (Jsonaf.exactly_equal before.payload after.payload);
  assert (P.Timestamp.equal before.created_at after.created_at);
  assert (
    Jsonaf.exactly_equal
      (P.Job.retry_to_json before.retry_policy)
      (P.Job.retry_to_json after.retry_policy));
  assert (Option.equal P.Job.equal_launch before.launch after.launch);
  assert (
    Option.equal P.Inference_origin.equal before.inference_origin after.inference_origin)
;;

let assert_run_origin (t : subject) (job : P.Job.t) =
  assert (
    Option.equal
      P.Inference_origin.equal
      job.inference_origin
      (Some (Run t.admission.run_id)))
;;

let assert_running (job : P.Job.t) =
  [%test_eq: int] 1 job.attempt;
  match job.status with
  | Running -> ()
  | _ -> raise_s [%sexp "actual legacy Model is not Running1", (job : P.Job.t)]
;;

let assert_success (job : P.Job.t) text =
  [%test_eq: int] 1 job.attempt;
  (match job.status with
   | Succeeded -> ()
   | _ -> raise_s [%sexp "actual legacy Model did not succeed", (job : P.Job.t)]);
  let result = Option.value_exn job.result in
  assert (Jsonaf.exactly_equal (`String text) (Jsonaf.member_exn "final_text" result))
;;

let receipt current id =
  List.filter current.A.Session_state.moderator_executions ~f:(fun receipt ->
    P.Id.Moderator_execution.equal receipt.context.id id)
  |> Model.only "actual legacy callback receipt"
;;

let assert_callback (t : subject) scope current =
  Actor.check_callback_current t.entry.actor scope |> protocol_ok;
  let executing = Actor.Callback_scope.executing scope in
  let actual = receipt current executing.context.id in
  assert (P.Moderator_execution.equal executing actual);
  (match actual.status with
   | Running -> ()
   | _ -> failwith "legacy provider did not retain its Running Event");
  assert (P.Moderator_execution.equal_phase Pre_tool_call actual.context.phase);
  assert (Option.is_none actual.context.operation_id);
  assert (Option.is_none actual.queue_occurrence);
  assert (Option.is_none actual.delegation);
  let owner = Option.value_exn actual.context.job in
  assert (P.Id.Job.equal t.parent.id owner.job_id);
  [%test_eq: int] 1 owner.attempt;
  assert (Option.is_none owner.deadline);
  assert (P.Run_source.equal t.source (Actor.Callback_scope.source scope));
  assert (
    Option.equal
      P.Inference_origin.equal
      (Actor.Callback_scope.origin scope)
      (Some (Run t.admission.run_id)));
  let original =
    Actor.callback_agent_actor t.entry.actor ~scope |> protocol_ok |> Option.value_exn
  in
  assert (Operator_authorization.is_current original);
  assert (
    P.Id.Principal.equal
      t.admission.principal_id
      (Operator_authorization.principal original).id);
  actual
;;

let assert_unrelated_chain (t : subject) current scope nested =
  let actual = assert_callback t scope current in
  let launch = Option.value_exn nested.P.Job.launch in
  (match P.Job.Birth_parent.view launch.owner with
   | Callback (Moderator_event id) ->
     assert (P.Id.Moderator_execution.equal actual.context.id id)
   | Callback (Invocation _) | Job_attempt _ ->
     failwith "legacy nested Model did not retain its actual Event birth");
  assert (
    Option.equal
      (fun (left, n) (right, m) -> P.Id.Job.equal left right && Int.equal n m)
      launch.parent_job
      (Some (t.parent.id, 1)));
  let parent_launch = Option.value_exn t.parent.launch in
  [%test_eq: int] (parent_launch.nested_depth + 1) launch.nested_depth;
  assert (
    Option.equal
      P.Invocation.equal_observer
      launch.moderator_source
      (Some actual.context.source));
  let root_id =
    match P.Job.Birth_parent.view parent_launch.owner with
    | Callback (Moderator_event id) -> id
    | Callback (Invocation _) | Job_attempt _ ->
      failwith "parent Model lost its actual Turn_start birth"
  in
  let root = receipt current root_id in
  assert (P.Moderator_execution.equal root (receipt t.before root_id));
  assert (P.Moderator_execution.equal_phase Turn_start root.context.phase);
  assert (Option.is_none root.context.job);
  assert (Option.is_none root.queue_occurrence);
  assert (Option.is_none root.delegation);
  let _, source = A.Moderator_source_installation.capture current |> protocol_ok in
  assert (P.Run_source.equal t.source source);
  let prefix =
    PC.Source.Native_prefix.create
      ~session_id:current.identity.session_id
      ~generation:current.identity.generation
      ~current_source:(Some source)
      ~events:current.moderator_executions
      ~invocations:current.invocations
      ~jobs:current.jobs
      ~runs:current.run_state
    |> protocol_ok
  in
  let member = PC.Source.Native_prefix.member_of_job nested |> protocol_ok in
  assert (
    Option.is_none
      (PC.Source.cause current.policy_canonical_usage ~prefix ~member |> protocol_ok));
  assert (Option.is_none t.before.policy_canonical_usage);
  assert (Option.is_none current.policy_canonical_usage)
;;

let observed_handle observations phase current =
  let admitted =
    List.filter !observations ~f:(fun admitted -> equal_phase admitted.phase phase)
    |> Model.only "actual Model on_admitted observation"
  in
  let row =
    List.filter (Ledger.rows current.A.Session_state.inference_ledger) ~f:(fun row ->
      let handle = Ledger.Row.handle row in
      Transcript.Scope.equal admitted.scope (Ledger.Handle.scope handle)
      && Observation.Observation_id.equal
           admitted.accounting_id
           (Ledger.Handle.accounting_id handle))
    |> Model.only "actual acknowledged Model H"
  in
  admitted, row
;;

let assert_provider (t : subject) observations phase =
  let current = state t.entry in
  let admitted, row = observed_handle observations phase current in
  let handle = Ledger.Row.handle row in
  (match Observation.Attempt_record.state (Ledger.Row.record row) with
   | Running -> ()
   | _ -> failwith "provider preceded canonical Running acknowledgement");
  let selected =
    job_named
      current
      (match phase with
       | Nested -> nested_marker
       | Parent -> Manual.marker
       | Initial -> assert false)
  in
  assert_job_identity admitted.job selected;
  assert_running selected;
  assert_run_origin t selected;
  assert_job_identity t.parent (Manual.one_job current);
  assert_running (Manual.one_job current);
  let binding =
    List.filter
      (Account.bindings (Goal.accounting current))
      ~f:(fun binding -> Ledger.Handle.equal handle (Account.Binding.handle binding))
    |> Model.only "same physical H native Goal binding before provider"
  in
  assert (P.Id.Run.equal t.admission.run_id (Account.Binding.run_id binding));
  assert (P.Run_source.equal t.source (Account.Binding.source binding));
  let increment =
    match phase with
    | Nested -> 1L
    | Parent -> 2L
    | Initial -> assert false
  in
  [%test_eq: int64]
    Int64.(Account.admitted_attempts (Goal.accounting t.before) + increment)
    (Account.admitted_attempts (Goal.accounting current));
  [%test_eq: int64]
    (Account.admitted_turns (Goal.accounting t.before))
    (Account.admitted_turns (Goal.accounting current));
  match phase with
  | Nested -> assert_unrelated_chain t current (Option.value_exn t.callback) selected
  | Parent ->
    let _, source = A.Moderator_source_installation.capture current |> protocol_ok in
    assert (P.Run_source.equal t.source source);
    assert (Option.is_none current.policy_canonical_usage)
  | Initial -> assert false
;;

let invoke_callback (t : subject) runtime services phase =
  let open Result.Let_syntax in
  let manager = Option.value_exn runtime.B.moderator_manager in
  assert (Option.is_none (M.extension_definition manager));
  let escaped = ref None in
  let outcome = ref None in
  (* These are trusted host boundary DATA. Only the actual Manager callback and
     Actor claim below issue the Event receipt and expiring effect receiver. *)
  let call : Chat_response.Moderation.Tool_call.t =
    { id = "597-legacy-host-boundary"
    ; name = "597-legacy-boundary"
    ; args = `Object []
    ; kind = Function
    ; payload_text = "{}"
    ; meta = `Null
    }
  in
  M.handle_legacy_callback_entries
    manager
    ~selection:(Boundary (Pre_tool_call call))
    ~session_id:(P.Id.Session.to_string t.before.identity.session_id)
    ~now_ms:0
    ~history:[]
    ~available_tools:runtime.moderator_tools
    ~session_meta:`Null
    ~with_admission:(fun callback ~snapshot:_ run ->
      let%bind committed =
        services.Actor.claim_legacy_callback ~callback (fun ~scope ~commit ->
          assert (M.callback_is_current manager callback);
          t.callback <- Some scope;
          ignore (assert_callback t scope (state t.entry) : P.Moderator_execution.t);
          let recipes =
            runtime.model_recipes (Actor.callback_model_jobs t.entry.actor ~scope)
          in
          escaped := Some (Map.find_exn recipes "agent_prompt_v1");
          let schedules = Actor.callback_schedule_services t.entry.actor ~scope in
          let binding : M.callback_binding =
            { services =
                { model_recipes = recipes
                ; tool_call =
                    (fun ~name:_ ~args:_ -> Error "unused callback Tool service")
                ; after_ms =
                    (fun ~delay_ms ~payload ->
                      let%bind payload =
                        Chatml.Chatml_value_codec.Snapshot.of_value payload
                      in
                      schedules.after_ms
                        ~delay_ms
                        ~payload:(Chatml.Chatml_value_codec.Snapshot.to_jsonaf payload))
                ; cancel = schedules.cancel
                }
            ; authorize =
                (fun () ->
                  Actor.check_callback_current t.entry.actor scope
                  |> Result.map_error ~f:(fun error -> error.P.Error.message))
            ; prepare =
                (fun ~(outcome : Chat_response.Moderation.Outcome.t) ~snapshot ->
                  let requests : P.Invocation.follow_up =
                    { request_turn =
                        Chat_response.Runtime_semantics.request_turn
                          outcome.runtime_requests
                    ; request_compaction =
                        Chat_response.Runtime_semantics.request_compaction
                          outcome.runtime_requests
                    ; end_session =
                        Chat_response.Runtime_semantics.should_end_session
                          outcome.runtime_requests
                    }
                  in
                  Ok
                    { M.persist =
                        (fun () ->
                          commit
                            ~snapshot
                            ~requests
                            ~notifications:outcome.ui_notifications
                          |> Result.map_error ~f:(fun error -> error.P.Error.message))
                    ; install = ignore
                    })
            }
          in
          phase := Nested;
          run binding
          |> Result.map_error ~f:(fun message ->
            P.Error.create Invalid_state ~message ~retryable:false ())
          |> Result.map ~f:(fun value -> outcome := Some value))
        |> Result.map_error ~f:(fun error -> error.P.Error.message)
      in
      if committed
      then Ok (Option.value_exn !outcome)
      else Error "legacy callback physical owner unavailable")
  |> Result.ok_or_failwith
  |> Option.value_exn
  |> ignore;
  let scope = Option.value_exn t.callback in
  let after = state t.entry in
  let actual = receipt after (Actor.Callback_scope.executing scope).context.id in
  (match actual.status with
   | Completed _ -> ()
   | _ -> failwith "legacy callback did not ACK");
  let acknowledged =
    Actor.Callback_scope.committed_checkpoint scope |> Option.value_exn
  in
  assert (Option.equal Jsonaf.exactly_equal acknowledged after.moderator);
  let persisted =
    A.Moderator_checkpoint.decode after.moderator |> protocol_ok |> Option.value_exn
  in
  (match persisted.current_state with
   | Session.Snapshot.Int value -> [%test_eq: int] 2 value
   | _ -> failwith "legacy callback ACK lost its integer state");
  assert (not persisted.halted);
  let live = M.identity_snapshot manager |> Result.ok_or_failwith in
  (* A Manager exposes the known identity projection; the Actor ACK above owns
     the full carrier, including queue provenance. Compare both without erasing
     the durable metadata or pretending that bare encoding is the full ACK. *)
  assert (
    Jsonaf.exactly_equal
      (B.encode_moderator_snapshot persisted)
      (B.encode_moderator_snapshot live));
  let raw_queue current =
    Option.value_exn current.A.Session_state.moderator
    |> Jsonaf.member_exn "queue_provenance"
  in
  assert (Jsonaf.exactly_equal (raw_queue t.before) (raw_queue after));
  let queue =
    A.Moderator_checkpoint.queue after.moderator |> protocol_ok |> Option.value_exn
  in
  assert (P.Run_source.equal t.source (A.Moderator_queue.source queue));
  A.Moderator_queue.validate_queue queue persisted.queued_internal_events |> protocol_ok;
  assert (List.is_empty persisted.queued_internal_events);
  assert (List.is_empty (A.Moderator_queue.entries queue));
  let nested = job_named after nested_marker in
  assert_success nested "597 legacy nested result";
  assert (P.Job.equal_delivery Not_required nested.delivery);
  let denial =
    match Actor.check_callback_current t.entry.actor scope with
    | Error error -> error
    | Ok () -> failwith "escaped legacy Event receiver stayed current"
  in
  assert_running (Manual.one_job after);
  let escaped = Option.value_exn !escaped in
  let payload = Jsonaf.member_exn "payload" nested.payload in
  (match escaped.call ~payload with
   | Error message -> [%test_eq: string] denial.message message
   | Ok _ -> failwith "expired Event borrowed its live parent Job admission");
  let rejected = state t.entry in
  Existing.assert_same after t.entry;
  assert (Account.equal (Goal.accounting after) (Goal.accounting rejected));
  assert (Option.is_none rejected.policy_canonical_usage);
  Ok ()
;;

let assert_final (t : subject) observations counters baseline_providers =
  let current = state t.entry in
  let nested = job_named current nested_marker in
  let parent = Manual.one_job current in
  assert_job_identity t.parent parent;
  assert_success nested "597 legacy nested result";
  assert_success parent "597 legacy parent result";
  assert (P.Job.equal_delivery Not_required nested.delivery);
  assert (P.Job.equal_delivery Pending parent.delivery);
  [%test_eq: int] (List.length t.before.jobs + 1) (List.length current.jobs);
  [%test_eq: int]
    (List.length t.before.moderator_executions + 1)
    (List.length current.moderator_executions);
  [%test_eq: int] (baseline_providers + 2) !(counters.providers);
  [%test_eq: int] 2 (List.length !observations);
  let nested_admitted, nested_row = observed_handle observations Nested current in
  let parent_admitted, parent_row = observed_handle observations Parent current in
  assert_job_identity nested_admitted.job nested;
  assert_job_identity parent_admitted.job parent;
  let nested_h = Ledger.Row.handle nested_row in
  let parent_h = Ledger.Row.handle parent_row in
  assert (not (Ledger.Handle.equal nested_h parent_h));
  let added = Model.new_rows t.before current in
  [%test_eq: int] 2 (List.length added);
  List.iter [ nested_h; parent_h ] ~f:(fun handle ->
    assert (
      List.exists added ~f:(fun row -> Ledger.Handle.equal handle (Ledger.Row.handle row)));
    let binding =
      List.filter
        (Account.bindings (Goal.accounting current))
        ~f:(fun binding -> Ledger.Handle.equal handle (Account.Binding.handle binding))
      |> Model.only "retained same-H native Goal binding"
    in
    assert (P.Id.Run.equal t.admission.run_id (Account.Binding.run_id binding));
    assert (P.Run_source.equal t.source (Account.Binding.source binding)));
  List.iter added ~f:Caused_async_support.assert_completed;
  List.iter (Ledger.rows t.before.inference_ledger) ~f:(fun old ->
    let retained = Caused_async_support.row current (Ledger.Row.handle old) in
    assert (
      Jsonaf.exactly_equal
        (Observation.Attempt_record.to_json (Ledger.Row.record old))
        (Observation.Attempt_record.to_json (Ledger.Row.record retained))));
  [%test_eq: int64]
    Int64.(Account.admitted_attempts (Goal.accounting t.before) + 2L)
    (Account.admitted_attempts (Goal.accounting current));
  [%test_eq: int64]
    (Account.admitted_turns (Goal.accounting t.before))
    (Account.admitted_turns (Goal.accounting current));
  let run = Resume.run current t.admission in
  let is_job (work : P.Run_work.t) =
    match work.key with
    | Retained (Job _) -> true
    | _ -> false
  in
  (* Owned occurrences remain retained after their immutable terminal proof;
     ownership is not the set of unfinished work. *)
  let owned_jobs = List.filter run.owned_work ~f:is_job in
  [%test_eq: int] 2 (List.length owned_jobs);
  assert (not (List.exists run.relinquished_work ~f:is_job));
  let terminals =
    List.filter run.terminal_work ~f:(fun terminal -> is_job terminal.work)
  in
  [%test_eq: int] 2 (List.length terminals);
  List.iter [ nested; parent ] ~f:(fun job ->
    let owned =
      List.filter owned_jobs ~f:(fun work ->
        Int.equal job.generation work.P.Run_work.generation
        &&
        match work.key with
        | Retained (Job { id; attempt }) ->
          P.Id.Job.equal job.id id && Int.equal job.attempt attempt
        | _ -> false)
      |> Model.only "actual Run owned Job occurrence"
    in
    let terminal =
      List.filter terminals ~f:(fun terminal ->
        P.Run_work.equal owned terminal.P.Run_work.Terminal.work)
      |> Model.only "actual Run terminal Job attempt"
    in
    [%test_eq: P.Run_work.Terminal.outcome] Succeeded terminal.outcome);
  Resume.assert_episode t.before current t.admission;
  let _, source = A.Moderator_source_installation.capture current |> protocol_ok in
  assert (P.Run_source.equal t.source source);
  assert (Option.is_none current.policy_canonical_usage)
;;

let%expect_test
    "legacy Job callback Event propagates native Goal Model receiver and expires"
  =
  let phase = ref Initial in
  let subject = ref None in
  let observations = ref [] in
  let counters : Retry_support.counters =
    { inference_ports = ref 0; providers = ref 0; transcripts = ref [] }
  in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs ->
      Int.incr counters.providers;
      let transcript =
        `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
      in
      counters.transcripts := transcript :: !(counters.transcripts);
      match !phase with
      | Initial -> completed_response "597 legacy original episode complete"
      | (Nested | Parent) as selected ->
        assert_provider (Option.value_exn !subject) observations selected;
        let marker, result =
          match selected with
          | Nested -> nested_marker, "597 legacy nested result"
          | Parent -> Manual.marker, "597 legacy parent result"
          | Initial -> assert false
        in
        assert (String.is_substring transcript ~substring:marker);
        completed_response result)
  in
  let policy =
    { policy with
      runtime_inference_ports =
        (fun actor ->
          Int.incr counters.inference_ports;
          Result.map
            (policy.runtime_inference_ports actor)
            ~f:(fun (upstream : Agent_server.Graph_tracking.Upstream.t) ->
              { upstream with
                on_admitted =
                  (fun ~scope ~accounting_id ->
                    upstream.on_admitted ~scope ~accounting_id;
                    match !phase with
                    | Initial -> ()
                    | (Nested | Parent) as selected ->
                      let current = Actor.state actor |> protocol_ok in
                      let job =
                        job_named
                          current
                          (match selected with
                           | Nested -> nested_marker
                           | Parent -> Manual.marker
                           | Initial -> assert false)
                      in
                      assert_running job;
                      observations
                      := { phase = selected; scope; accounting_id; job } :: !observations)
              }))
    }
  in
  Creation_tests.with_daemon
    ~startup_mode:Operator_only
    ~prompt_text:prompt
    ~sources:
      [ "held-job.chatmd", "<developer>Offline genuine legacy recipe.</developer>" ]
    ~policy
    (fun env _sw _root daemon ->
       Model.within env "legacy Job callback Event Model receiver" (fun () ->
         Manual.with_loaded_manual_entry daemon (fun entry runtime ->
           let manager = Option.value_exn runtime.moderator_manager in
           assert (Option.is_none (M.extension_definition manager));
           let session, before = Goal.create_goal daemon entry in
           let admission = Resume.start_episode daemon entry original session before in
           let before = Goal.await_input env entry counters in
           let queued = Manual.one_job before in
           (match queued.status with
            | Queued -> ()
            | _ -> failwith "parent did not queue");
           [%test_eq: int] 0 queued.attempt;
           let parent =
             Actor.claim_job entry.actor ~job_id:queued.id ~generation:queued.generation
             |> protocol_ok
             |> Option.value_exn
           in
           assert_job_identity queued parent;
           assert_running parent;
           let _, source =
             A.Moderator_source_installation.capture before |> protocol_ok
           in
           assert (P.Run_source.equal source (Resume.run before admission).source);
           let t = { entry; before; admission; parent; source; callback = None } in
           assert_run_origin t parent;
           subject := Some t;
           let baseline_providers = !(counters.providers) in
           let outcome =
             Actor.with_job_execution
               entry.actor
               ~job_id:parent.id
               ~generation:parent.generation
               ~attempt:parent.attempt
               ~deadline:None
               (fun services ->
                  (match Actor.Job_runtime_owner.attribution services.runtime_owner with
                   | Goal -> ()
                   | Ordinary -> failwith "actual parent lost native Goal owner");
                  Factory.with_goal_job_runtime
                    (Daemon.factory daemon)
                    entry
                    ~owner:services.runtime_owner
                    (fun actual ->
                       assert (phys_equal actual runtime);
                       assert (
                         phys_equal manager (Option.value_exn actual.moderator_manager));
                       let open Result.Let_syntax in
                       let%bind () = invoke_callback t actual services phase in
                       [%test_eq: int] (baseline_providers + 1) !(counters.providers);
                       [%test_eq: int] 1 (List.length !observations);
                       phase := Parent;
                       let%bind inference =
                         Factory.model_job_inference (Daemon.factory daemon) entry parent
                       in
                       actual.execute_model_job
                         ~allocate_inference:services.allocate_inference
                         ~inference_context:inference.context
                         ~capture_recipe_target:inference.capture_recipe_target
                         ~recipe:
                           (Jsonaf.member_exn "recipe" parent.payload |> Jsonaf.string_exn)
                         ~payload:(Jsonaf.member_exn "payload" parent.payload))
                  |> Result.map ~f:(function
                    | Owner.Construction_result.Built outcome -> outcome
                    | Callback_completed _ ->
                      failwith
                        "legacy parent callback unexpectedly completed construction"))
             |> protocol_ok
           in
           (match outcome with
            | B.Model_succeeded _ -> ()
            | Model_failed message -> failwith message);
           Actor.complete_job
             entry.actor
             ~job_id:parent.id
             ~generation:parent.generation
             ~attempt:parent.attempt
             outcome
           |> protocol_ok
           |> ignore;
           assert_final t observations counters baseline_providers)));
  print_endline
    "actual legacy Job→Event→Model retains same-H native Goal accounting and full \
     unrelated ancestry";
  print_endline
    "expired Event recipe denies while parent Job lives; actual parent and nested work \
     succeed";
  [%expect
    {|
    actual legacy Job→Event→Model retains same-H native Goal accounting and full unrelated ancestry
    expired Event recipe denies while parent Job lives; actual parent and nested work succeed
  |}]
;;
