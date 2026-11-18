open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Direct = Descendant_turn_adapter_tests
module Goal = Goal_service_tests
module G = P.Goal
module L = A.Inference_ledger
module PC = A.Policy_canonical_usage
module Cause = A.Policy_cause_data
module Book = A.Goal_policy_contributions
module Totals = A.Goal_policy_totals
module E = P.Moderator_execution
module Failure = Delegated_policy_caller_support.Failure

let child_marker = "597_REAL_CHILD_GOAL_INPUT"
let first_marker = "597_FIRST_POLICY_MODEL"
let second_marker = "597_SECOND_POLICY_MODEL_AT_CAP"
let recipe = "597-policy-model.chatmd"

let parent_prompt =
  {|<developer>597 actual parent policy Model owner.</developer>
<tool name="policy_recipe" agent="597-policy-model.chatmd" local/>
<script id="parent-policy" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let payload text = `Object([
  {key = "prompt"; value = `String("597-policy-model.chatmd")},
  {key = "input"; value = `String(text)},
  {key = "is_local"; value = `Bool(true)}
])
let on_event ctx state event = match event with
| `Pre_tool_call(p) ->
  if p.name == "read_file" then
    let* first = Model.call("agent_prompt_v1", payload("597_FIRST_POLICY_MODEL")) in
    let* checked = (match first with
    | `Ok(value) -> Task.pure(value)
    | `Refused(message) -> Task.fail(message)
    | `Error(message) -> Task.fail(message)) in
    let* denied = Task.catch(
      (let* second = Model.call("agent_prompt_v1", payload("597_SECOND_POLICY_MODEL_AT_CAP")) in
       match second with
       | `Ok(_) -> Task.pure(false)
       | `Refused(_) -> Task.pure(true)
       | `Error(_) -> Task.pure(true)),
      fun message -> Task.pure(true)) in
    if denied then Task.pure(state + 1) else Task.fail("second policy provider ran")
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
;;

let child_prompt =
  {|<authoring_context policy="manual"/><config model="fixture-model"/>
<developer>597 genuine Goal-born child with its own actual Goal.</developer>
<tool type="inherited" name="read_file"/>
<script id="child-policy" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
;;

let checkpoint entry =
  Actor.with_moderator_checkpoint entry.Registry.actor (fun () -> Actor.state entry.actor)
  |> protocol_ok
;;

let within env phase f =
  try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. f with
  | Eio.Time.Timeout -> failwith ("597 Model trajectory timed out: " ^ phase)
;;

let only label = function
  | [ value ] -> value
  | values ->
    failwithf "%s: expected one actual value, found %d" label (List.length values) ()
;;

let attempts current =
  P.Inference_query.Summary.retained_attempts
    (L.summary current.A.Session_state.inference_ledger)
;;

let total current =
  A.Session_state.goal_policy_context current ~owner:(G.session (Goal.goal current))
  |> protocol_ok
  |> fun context -> Totals.capture context ~goal_id:Goal.goal_id |> protocol_ok
;;

let committed current = Totals.View.committed (total current) ~dimension:Attempt

let new_rows before after =
  List.filter (L.rows after.A.Session_state.inference_ledger) ~f:(fun row ->
    not
      (List.exists (L.rows before.A.Session_state.inference_ledger) ~f:(fun old ->
         L.Handle.equal (L.Row.handle row) (L.Row.handle old))))
;;

let model_jobs current =
  List.filter current.A.Session_state.jobs ~f:(fun job ->
    P.Job.equal_kind Model_call job.kind
    && Option.exists (Jsonaf.member "payload" job.payload) ~f:(fun payload ->
      Option.exists (Jsonaf.member "input" payload) ~f:(function
        | `String input ->
          String.equal input first_marker || String.equal input second_marker
        | `Null | `True | `False | `Number _ | `Object _ | `Array _ -> false)))
;;

let job_named current marker =
  model_jobs current
  |> List.filter ~f:(fun job ->
    Option.equal
      String.equal
      (Jsonaf.member "payload" job.P.Job.payload
       |> Option.bind ~f:(Jsonaf.member "input")
       |> Option.bind ~f:Jsonaf.string)
      (Some marker))
  |> only "actual policy Model Job"
;;

let cause_for accounting binding =
  PC.to_document accounting
  |> protocol_ok
  |> Document_schema.Document.payload
  |> Jsonaf.member_exn "causes"
  |> Jsonaf.list_exn
  |> List.map ~f:(fun raw -> Cause.of_json raw |> protocol_ok)
  |> List.filter ~f:(fun cause ->
    Cause.Key.equal (Cause.key cause) (PC.Binding.cause binding))
  |> only "actual caused Model binding"
;;

let subtotal current =
  Book.subtotal
    current.A.Session_state.goal_policy_contributions
    ~owner:(G.session (Goal.goal current))
    ~goal_id:Goal.goal_id
    ~catalog:current.goal_catalog
    ~accounts:current.goal_accounts
    ~canonical:current.policy_canonical_usage
    ~ledger:current.inference_ledger
    ~session_revision:current.counters.revision
  |> protocol_ok
;;

let pending_projection current =
  let raw =
    Option.value_exn current.A.Session_state.goal_policy_contributions
    |> Book.to_document
    |> protocol_ok
    |> Document_schema.Document.payload
  in
  let book =
    Jsonaf.member_exn "books" raw |> Jsonaf.list_exn |> only "remote Goal book"
  in
  let slot =
    Jsonaf.member_exn "slots" book |> Jsonaf.list_exn |> only "remote Model stream"
  in
  assert (Jsonaf.exactly_equal `Null (Jsonaf.member_exn "pending" slot));
  Jsonaf.member_exn "projection" slot |> PC.Projection.of_json |> protocol_ok
;;

module Probe = struct
  type t =
    { parent : Registry.entry
    ; child : Registry.entry
    ; before_parent : A.Session_state.t
    ; before_child : A.Session_state.t
    ; admission : P.Run_receipt.t
    ; scope : Agent_store.Delegation_goal_origin.Scope.t
    ; reference : Agent_store.Delegation_store.Reference.t
    ; ceiling : int64
    ; mutable observed : L.Handle.t option
    }

  let at_provider t =
    assert (Option.is_none t.observed);
    (* Provider I/O runs outside both Actor mailboxes. These independent reads
       observe actual ACKs, not a proposed update or a controller DTO. *)
    let parent = state t.parent in
    let child = state t.child in
    let row = new_rows t.before_parent parent |> only "physical parent policy H" in
    let handle = L.Row.handle row in
    assert (P.Id.Session.equal (L.Handle.session_id handle) parent.identity.session_id);
    assert (
      not (P.Id.Session.equal (L.Handle.session_id handle) child.identity.session_id));
    let child_row = new_rows t.before_child child |> only "physical child Goal H" in
    let child_handle = L.Row.handle child_row in
    assert (not (L.Handle.equal handle child_handle));
    (match Inference.Observation.Attempt_record.state (L.Row.record row) with
     | Running -> ()
     | Prepared | Terminal _ | Interrupted _ ->
       failwith "policy provider preceded its Running H ACK");
    let job = job_named parent first_marker in
    (match job.status with
     | Running -> ()
     | Queued
     | Waiting_permission _
     | Waiting_completion _
     | Succeeded
     | Failed _
     | Cancelled
     | Interrupted _ ->
       failwith "policy provider did not own its actual Running Model Job");
    let accounting =
      match parent.policy_canonical_usage with
      | Some accounting -> accounting
      | None -> failwith "actual parent policy provider preceded its caused canonical ACK"
    in
    let binding =
      PC.bindings accounting
      |> List.filter ~f:(fun binding ->
        Int64.equal (PC.Binding.ordinal binding) (L.Handle.ordinal handle))
      |> only "same H policy binding"
    in
    let cause = cause_for accounting binding in
    assert (P.Session_ref.equal (Cause.caller cause) (G.session (Goal.goal child)));
    assert (P.Session_ref.equal (Cause.callee cause) (G.session (Goal.goal parent)));
    assert (
      Option.equal
        P.Inference_origin.equal
        (Cause.caller_origin cause)
        (Some (Run t.admission.run_id)));
    assert (
      List.equal
        Agent_store.Delegation_store.Reference.equal
        (Cause.path cause)
        [ t.reference ]);
    let receipt =
      List.find_exn parent.moderator_executions ~f:(fun receipt ->
        P.Id.Moderator_execution.equal receipt.context.id (Cause.parent_event cause))
    in
    assert (E.equal_phase receipt.context.phase Pre_tool_call);
    assert (Cause.matches_event cause receipt);
    let targets = Cause.targets cause in
    [%test_eq: int] 2 (List.length targets);
    let for_owner owner =
      List.filter targets ~f:(fun target ->
        P.Session_ref.equal owner (Cause.Goal_target.owner target))
      |> only "exact governing Goal owner"
    in
    let inherited = for_owner (G.session (Goal.goal parent)) in
    (match Cause.Goal_target.view inherited with
     | Inherited scope ->
       assert (Agent_store.Delegation_goal_origin.Scope.equal scope t.scope)
     | Local _ ->
       failwith "actual inherited Goal descriptor was replaced with ambient native DATA");
    let local = for_owner (G.session (Goal.goal child)) in
    (match Cause.Goal_target.view local with
     | Local { goal_id; episode } ->
       assert (P.Id.Goal.equal goal_id Goal.goal_id);
       assert (P.Run_receipt.equal (P.Goal_episode.admission episode) t.admission)
     | Inherited _ ->
       failwith "actual child local Goal became a fabricated inherited Scope");
    let choices = PC.Binding.choices binding in
    [%test_eq: int] 2 (List.length choices);
    let local_choice =
      List.find_exn choices ~f:(fun choice ->
        Cause.Goal_target.equal local (PC.Choice.target choice))
    in
    assert (Cause.Coverage.equal Causal_overlay (PC.Choice.coverage local_choice));
    let root_choice =
      List.find_exn choices ~f:(fun choice ->
        Cause.Goal_target.equal inherited (PC.Choice.target choice))
    in
    let native_binding =
      List.exists
        (A.Goal_accounting.bindings (Goal.accounting parent))
        ~f:(fun binding ->
          L.Handle.equal handle (A.Goal_accounting.Binding.handle binding))
    in
    (match PC.Choice.coverage root_choice with
     | Covered_local -> assert native_binding
     | Causal_overlay -> assert (not native_binding)
     | Covered_physical -> failwith "root policy H cannot be its own physical descendant");
    let imported = pending_projection child in
    let head = PC.Projection.head imported |> Option.value_exn in
    let occurrence =
      A.Goal_descendants.Canonical.Fact.occurrence (PC.Head.canonical head)
    in
    (match A.Goal_descendant_occurrence.kind occurrence with
     | Attempt { ordinal; _ } -> [%test_eq: int64] (L.Handle.ordinal handle) ordinal
     | Host_turn _ ->
       failwith "causal confirmation did not import the actual policy Attempt");
    assert (
      P.Session_ref.equal
        (A.Goal_descendant_occurrence.Frame.session
           (A.Goal_descendant_occurrence.frame occurrence))
        (G.session (Goal.goal parent)));
    (match PC.Head.work head with
     | Model_job_attempt { id; attempt } ->
       assert (P.Id.Job.equal id job.id);
       [%test_eq: int] job.attempt attempt
     | Event _ | Invocation _ | Async_job_attempt _ ->
       failwith "Model H lost its real durable Job attempt membership");
    [%test_eq: int64] 1L (A.Goal_usage.attempts (Book.Subtotal.usage (subtotal child)));
    assert (List.is_empty (Book.Subtotal.pending (subtotal child)));
    [%test_eq: int64] t.ceiling (committed child);
    [%test_eq: int64] Int64.(committed t.before_parent + 2L) (committed parent);
    let root = total parent in
    [%test_eq: int64] 1L (A.Goal_usage.attempts (Totals.View.physical_usage root));
    [%test_eq: int64]
      (if native_binding then 0L else 1L)
      (A.Goal_usage.attempts (Totals.View.causal_usage root));
    assert (not (Totals.View.physical_unknown root));
    assert (not (Totals.View.historical_causal_unknown root));
    t.observed <- Some handle
  ;;
end

let%expect_test
    "real parent policy Model charges both genuine Goals once and refuses at cap"
  =
  let failure = Failure.create () in
  let handoff, handoff_resolver = Eio.Promise.create () in
  let release_handoff () =
    if not (Eio.Promise.is_resolved handoff) then Eio.Promise.resolve handoff_resolver ()
  in
  let selected = ref None in
  let child_calls = ref 0 in
  let first_calls = ref 0 in
  let second_calls = ref 0 in
  let emitted = ref false in
  let phase = ref "genuine native Goal birth" in
  let provider inputs =
    Failure.guard failure (fun () ->
      let text = `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string in
      if String.is_substring text ~substring:second_marker
      then (
        Int.incr second_calls;
        failwith "at-cap policy Model reached provider")
      else if String.is_substring text ~substring:first_marker
      then (
        Int.incr first_calls;
        Probe.at_provider (Option.value_exn !selected);
        completed_response "597 actual parent policy model completed")
      else if String.is_substring text ~substring:child_marker
      then (
        Int.incr child_calls;
        Eio.Promise.await handoff;
        if !emitted then failwith "at-cap child requested another provider";
        emitted := true;
        Direct.function_response
          "read_file"
          (`Object [ "root", `String "data"; "file", `String "597-effect.txt" ]))
      else completed_response "597 real parent completed native child birth")
  in
  Birth.with_native_goal_child
    ~timeout:90.
    ~parent_prompt_text:parent_prompt
    ~sources:
      [ recipe, "<developer>597 actual captured policy model recipe.</developer>"
      ; "597-effect.txt", "597 real inherited effect"
      ]
    ~inherited_tools:[ "read_file", Descendant_goal_resume_tests.parent_reader ]
    ~child_sources:[ "child.chatmd", child_prompt ]
    ~post_creation_stream:provider
    (fun env _sw daemon parent child scope reference counters ->
       Exn.protect ~finally:release_handoff ~f:(fun () ->
         Failure.restore failure (fun () ->
           try
             phase := "actual stopped child source installation";
             let born = state child in
             let providers_before_load = !(counters.providers) in
             within env !phase (fun () ->
               Owner.ensure_loaded_authenticated child.runtime ~actor:original
               |> protocol_ok);
             let installed = checkpoint child in
             assert (Owner.is_loaded child.runtime);
             assert (P.Session.equal_desired_state Stopped installed.lifecycle.desired);
             assert (Option.is_none installed.active_operation);
             assert (
               List.equal E.equal born.moderator_executions installed.moderator_executions);
             assert (Option.is_some installed.moderator);
             let _, source =
               A.Moderator_source_installation.capture installed |> protocol_ok
             in
             [%test_eq: int] installed.identity.generation source.generation;
             assert (
               Option.equal
                 Agent_store.Delegation_goal_origin.equal
                 (Option.bind born.goal_descendants ~f:A.Goal_descendants.origin)
                 (Option.bind installed.goal_descendants ~f:A.Goal_descendants.origin));
             assert (
               List.equal
                 P.History.equal_entry
                 born.conversation.canonical_history
                 installed.conversation.canonical_history);
             [%test_eq: int64] (attempts born) (attempts installed);
             [%test_eq: int] providers_before_load !(counters.providers);
             phase := "actual child lifecycle joined at running idle";
             within env !phase (fun () -> Direct.start_child daemon child);
             let idle = checkpoint child in
             assert (P.Session.equal_desired_state Running idle.lifecycle.desired);
             (match idle.lifecycle.observed with
              | Idle -> ()
              | Stopped
              | Queued_for_slot
              | Starting
              | Recovering
              | Running_turn _
              | Compacting _
              | Waiting_for_permission _
              | Stopping
              | Failed _ -> failwith "actual child startup did not reach running idle");
             assert (Option.is_none idle.active_operation);
             let _, idle_source =
               A.Moderator_source_installation.capture idle |> protocol_ok
             in
             assert (P.Run_source.equal source idle_source);
             assert (
               List.equal
                 P.History.equal_entry
                 installed.conversation.canonical_history
                 idle.conversation.canonical_history);
             [%test_eq: int64] (attempts installed) (attempts idle);
             [%test_eq: int] providers_before_load !(counters.providers);
             let before_parent = checkpoint parent in
             phase := "public local Goal creation on actual installed source";
             let session, created = Goal.create_goal daemon child in
             let ceiling = Int64.(committed created + 2L) in
             let budget =
               G.Budget.create
                 ~admitted_turns:None
                 ~admitted_attempts:(Some ceiling)
                 ~observed_tokens:None
                 ~deadline:None
               |> protocol_ok
             in
             Goal_admitted_guard_tests.command
               daemon
               child
               ~key:"597-real-child-cap"
               (Change_budget (Goal.goal_id, budget))
             |> protocol_ok
             |> ignore;
             let before_child = state child in
             let request =
               A.Run_admission_request.Headless.create
                 ~session_id:before_child.identity.session_id
                 ~generation:before_child.identity.generation
                 ~expected_revision:before_child.counters.revision
                 ~mode:Workflow
                 ~input:
                   (User_submission
                      { kind = Plain_text; text = child_marker; attachments = [] })
                 ~key:
                   (P.Idempotency_key.of_string "597-real-child-goal-episode"
                    |> protocol_ok)
               |> protocol_ok
               |> fun request ->
               A.Goal_activation_request.create
                 ~goal_id:Goal.goal_id
                 ~expected_goal_revision:(G.revision (Goal.goal before_child))
                 ~expected_catalog_revision:
                   (A.Goal_catalog.revision (Option.value_exn before_child.goal_catalog))
                 ~request
               |> protocol_ok
             in
             phase := "actual dual-governed child Goal activation";
             (* Install the provider observation before asynchronous work starts;
             the actual Run receipt is returned by the real admission. The
             provider is held at its first child response until that handoff. *)
             let admission =
               Agent_server.Goal_run_service.create
                 ~factory:(Daemon.factory daemon)
                 ~authorize:(fun _ -> authorize)
               |> fun service ->
               Agent_server.Goal_run_service.start_episode
                 service
                 ~entry:child
                 ~actor:original
                 ~session
                 ~request
               |> protocol_ok
               |> Goal.activated
             in
             selected
             := Some
                  { Probe.parent
                  ; child
                  ; before_parent
                  ; before_child
                  ; admission
                  ; scope
                  ; reference
                  ; ceiling
                  ; observed = None
                  };
             release_handoff ();
             phase := "first real policy Model plus denied second allocation";
             let final_parent, final_child =
               within env !phase (fun () ->
                 let rec await () =
                   Failure.raise_if_present failure;
                   let parent_state = state parent in
                   let child_state = state child in
                   if Int.equal !first_calls 0
                   then
                     List.iter parent_state.moderator_executions ~f:(fun receipt ->
                       let newly_claimed =
                         not
                           (List.exists
                              before_parent.moderator_executions
                              ~f:(fun before ->
                                P.Id.Moderator_execution.equal
                                  before.E.context.id
                                  receipt.E.context.id))
                       in
                       if
                         newly_claimed
                         && E.equal_phase receipt.context.phase Pre_tool_call
                       then (
                         match receipt.status with
                         | Failed _ | Interrupted _ ->
                           raise_s
                             [%sexp
                               "new actual parent policy callback ended before Model \
                                provider"
                             , (receipt.context.id : P.Id.Moderator_execution.t)
                             , (receipt.status : E.status)]
                         | Running | Completed _ -> ()));
                   let settled =
                     List.for_all parent_state.moderator_executions ~f:(fun receipt ->
                       match receipt.E.status with
                       | Running -> false
                       | Completed _ | Failed _ | Interrupted _ -> true)
                   in
                   if
                     !first_calls = 1
                     && Option.is_none child_state.active_operation
                     && settled
                   then checkpoint parent, checkpoint child
                   else (
                     Eio.Fiber.yield ();
                     await ())
                 in
                 await ())
             in
             let probe = Option.value_exn !selected in
             let handle = Option.value_exn probe.observed in
             [%test_eq: int] 1 !child_calls;
             [%test_eq: int] 1 !first_calls;
             [%test_eq: int] 0 !second_calls;
             [%test_eq: int64] Int64.(attempts before_parent + 1L) (attempts final_parent);
             [%test_eq: int64] Int64.(attempts before_child + 1L) (attempts final_child);
             let first = job_named final_parent first_marker in
             let second = job_named final_parent second_marker in
             assert (not (P.Id.Job.equal first.id second.id));
             (match first.status with
              | Succeeded -> ()
              | Queued
              | Running
              | Waiting_permission _
              | Waiting_completion _
              | Failed _
              | Cancelled
              | Interrupted _ ->
                raise_s [%sexp "first Model did not succeed", (first : P.Job.t)]);
             (match second.status with
              | Failed _ -> ()
              | Queued
              | Running
              | Waiting_permission _
              | Waiting_completion _
              | Succeeded
              | Cancelled
              | Interrupted _ ->
                raise_s
                  [%sexp "second Model did not retain its denial", (second : P.Job.t)]);
             let row =
               L.find final_parent.inference_ledger ~ordinal:(L.Handle.ordinal handle)
               |> Option.value_exn
             in
             assert (L.Handle.equal handle (L.Row.handle row));
             (match Inference.Observation.Attempt_record.state (L.Row.record row) with
              | Terminal terminal ->
                (match Inference.Event.Terminal.outcome terminal with
                 | Completed -> ()
                 | Refused | Incomplete _ | Failed _ ->
                   failwith "first policy Model terminal was not Completed")
              | Prepared | Running | Interrupted _ ->
                failwith "first actual policy H did not complete");
             [%test_eq: int64] ceiling (committed final_child);
             assert (List.is_empty (Book.Subtotal.pending (subtotal final_child)));
             [%test_eq: int64]
               1L
               (A.Goal_usage.attempts (Book.Subtotal.usage (subtotal final_child)));
             [%test_eq: int64]
               Int64.(committed before_parent + 2L)
               (committed final_parent);
             assert (
               G.State.equal
                 (Open (Held (Budget_gate Admitted_attempts)))
                 (G.state (Goal.goal final_child)));
             let first_launch = Option.value_exn first.launch in
             let second_launch = Option.value_exn second.launch in
             assert (P.Job.Birth_parent.equal first_launch.owner second_launch.owner);
             let event_id =
               match P.Job.Birth_parent.callback_owner first_launch.owner with
               | Some (Moderator_event id) -> id
               | Some (Invocation _) | None ->
                 failwith "policy Model Jobs did not retain their actual Event owner"
             in
             let receipt =
               List.filter final_parent.moderator_executions ~f:(fun receipt ->
                 P.Id.Moderator_execution.equal receipt.E.context.id event_id)
               |> only "actual parent Model callback"
             in
             assert (E.equal_phase receipt.context.phase Pre_tool_call);
             let before_snapshot =
               A.Moderator_checkpoint.decode before_parent.moderator
               |> protocol_ok
               |> Option.value_exn
             in
             let after_snapshot =
               A.Moderator_checkpoint.decode final_parent.moderator
               |> protocol_ok
               |> Option.value_exn
             in
             let source : P.Invocation.observer =
               { script_id = before_snapshot.script_id
               ; source_sha256 = before_snapshot.script_source_hash
               }
             in
             assert (String.equal before_snapshot.script_id after_snapshot.script_id);
             assert (
               String.equal
                 before_snapshot.script_source_hash
                 after_snapshot.script_source_hash);
             assert (P.Invocation.equal_observer receipt.context.source source);
             List.iter [ first_launch; second_launch ] ~f:(fun launch ->
               assert (
                 Option.equal
                   P.Invocation.equal_observer
                   launch.P.Job.moderator_source
                   (Some source)));
             assert (not after_snapshot.halted);
             (match receipt.status, receipt.decision with
              | Completed _, Some Approve -> ()
              | (Running | Failed _ | Interrupted _), _
              | Completed _, (None | Some (Reject _ | Rewrite_args _ | Redirect _)) ->
                raise_s
                  [%sexp
                    "actual parent Model callback did not commit"
                  , (receipt.context.id : P.Id.Moderator_execution.t)
                  , (receipt.status : E.status)
                  , (receipt.decision : E.Decision.t option)]);
             (match before_snapshot.current_state, after_snapshot.current_state with
              | Int before, Int after -> [%test_eq: int] (before + 1) after
              | _ -> failwith "actual parent Model callback changed state representation");
             (* This actual cap denial is prewriter. A later unrelated metadata
                ACK must remain lawful; this does not claim the separate
                Writer_prepared/known-NoWriter kernel branch was exercised. *)
             let providers_before_metadata = !(counters.providers) in
             let attachment, _ =
               Actor.attach parent.actor ~mode:Read_write ~subscribe:false |> protocol_ok
             in
             protect_cleanup
               ~finally:(fun () -> Actor.detach parent.actor attachment.id |> protocol_ok)
               (fun () ->
                  let patch =
                    P.Session_metadata.Patch.create
                      ~name:(Set "597 lawful metadata after prewriter cap denial")
                      ~set_labels:[]
                      ~remove_labels:[]
                    |> protocol_ok
                  in
                  within env "metadata ACK after prewriter cap denial" (fun () ->
                    Actor.update_metadata
                      parent.actor
                      ~attachment_id:attachment.id
                      ~expected_metadata_revision:final_parent.identity.metadata_revision
                      ~patch
                      ()
                    |> protocol_ok
                    |> ignore);
                  let after_metadata = state parent in
                  [%test_eq: int64]
                    Int64.(final_parent.identity.metadata_revision + 1L)
                    after_metadata.identity.metadata_revision;
                  [%test_eq: string option]
                    (Some "597 lawful metadata after prewriter cap denial")
                    after_metadata.identity.display_name;
                  assert (
                    Jsonaf.exactly_equal
                      (L.to_document final_parent.inference_ledger
                       |> Result.map_error ~f:(fun error ->
                         Sexp.to_string_hum (L.Error.sexp_of_t error))
                       |> Result.ok_or_failwith
                       |> Document_schema.Document.json)
                      (L.to_document after_metadata.inference_ledger
                       |> Result.map_error ~f:(fun error ->
                         Sexp.to_string_hum (L.Error.sexp_of_t error))
                       |> Result.ok_or_failwith
                       |> Document_schema.Document.json));
                  [%test_eq: int] 1 !first_calls;
                  [%test_eq: int] 0 !second_calls;
                  [%test_eq: int] providers_before_metadata !(counters.providers));
             Failure.raise_if_present failure;
             print_endline
               "actual child and parent Hs charge both Goals once; remote cap denies the \
                next policy provider"
           with
           | exn ->
             let backtrace = Stdlib.Printexc.get_raw_backtrace () in
             (try Eio.traceln "597 Model fixture phase: %s" !phase with
              | _ -> ());
             Exn.raise_with_original_backtrace exn backtrace)));
  [%expect
    {| actual child and parent Hs charge both Goals once; remote cap denies the next policy provider |}]
;;
