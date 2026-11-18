open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Direct = Descendant_turn_adapter_tests
module Subscription = Goal_subscription_runtime_tests
module G = P.Goal
module GD = A.Goal_descendants
module Origin = Agent_store.Delegation_goal_origin
module Delegations = Agent_store.Delegation_store
module C = Chat_response.Tool_capability

let tool_name = "subscription_child"
let child_file = "570-subscription-child.chatmd"

let assert_source env daemon child reference record ~prompt =
  let current = state child in
  assert (Delegations.Reference.equal reference (Option.value_exn current.spec.delegation));
  assert (Delegations.equal_stage record.Delegations.stage Linked);
  assert (Option.is_none record.revocation);
  (match record.admission.authored_tool with
   | Some source ->
     [%test_eq: string] tool_name source.name;
     assert (not (String.is_empty source.source_sha256))
   | None -> failwith "associated child did not use its real authored declaration");
  let store = Daemon.store daemon in
  let artifacts =
    Agent_store.Prompt_artifact_store.open_existing
      ~env
      ~root:
        (Agent_store.Data_root.prompt_artifacts_path
           (Agent_store.Session_store.data_root store))
    |> Existing.store_ok
  in
  let artifact =
    Agent_store.Prompt_artifact_store.load artifacts current.spec.prompt_revision_id
    |> Existing.store_ok
  in
  assert (P.Id.Prompt_revision.equal artifact.revision_id reference.revision_id);
  [%test_eq: string] record.admission.manifest_sha256 artifact.manifest_sha256;
  [%test_eq: string] prompt artifact.root_chatmd;
  assert (
    String.is_substring
      artifact.root_chatmd
      ~substring:"<tool name=\"watch\" type=\"moderator\"");
  assert (
    String.is_substring
      artifact.root_chatmd
      ~substring:
        "<tool name=\"read_file\"><read id=\"data\" path=\"${workspace}\"/></tool>");
  assert (Owner.is_loaded child.runtime);
  Owner.with_loaded_runtime child.runtime (fun runtime ->
    let check_execution = Option.value_exn runtime.A.Runtime_builder.check_execution in
    let check_ancestry = Option.value_exn runtime.check_ancestry in
    check_execution () |> protocol_ok;
    let ancestry = check_ancestry () |> protocol_ok in
    let leaf = A.Delegation_authority.Checked_ancestry.records ancestry |> List.hd_exn in
    assert (Delegations.Reference.equal reference (Delegations.reference leaf));
    assert (Delegations.equal_record record leaf);
    assert (not (A.Prompt_source_lease.is_legacy runtime.source_lease));
    (match A.Prompt_source_lease.availability runtime.source_lease with
     | Live -> ()
     | Missing_receipt | Unavailable _ | Revoked | Closed ->
       failwith "actual authored source lease is unavailable");
    let manager = Option.value_exn runtime.moderator_manager in
    assert (Option.is_some (Chat_response.Moderator_manager.extension_definition manager));
    let _, source = A.Moderator_source_installation.capture current |> protocol_ok in
    assert (
      Option.equal
        P.Invocation.equal_observer
        (Some source.observer)
        (A.Runtime_builder.moderation_source runtime |> protocol_ok));
    let native = Option.value_exn runtime.native_runtime in
    let capabilities =
      Lazy.force native.capabilities
      |> Result.map_error ~f:(fun error -> error.C.message)
      |> Result.ok_or_failwith
    in
    let selected =
      Chat_response.Background_request.rebind_capabilities
        ~pins:record.admission.capability_pins
        ~capabilities
      |> protocol_ok
    in
    List.iter [ "read_file"; "watch" ] ~f:(fun name ->
      C.find selected ~name
      |> Result.map_error ~f:(fun error -> error.C.message)
      |> Result.ok_or_failwith
      |> ignore);
    Ok ())
  |> protocol_ok
;;

(* The actual persistent wrapper starts and submits to its child. Its bootstrap
   therefore has real usage and a terminal submission; it is not a stopped seed. *)
let with_child ~phase ~chatml_runtime_policy ~prompt ~post_creation_stream ~wrap_env f =
  let first = ref true in
  (* The existing cold subscription workflow has a 60-second whole-flow budget.
     Actual authored creation adds a parent Goal and a completed child bootstrap;
     measured setup reached the ordinary notice at 29 seconds. Preserve each
     existing 30-second phase deadline while allowing the complete composition. *)
  with_recoverable_fixture
    ~timeout:90.
    ~chatml_runtime_policy
    ~sources:[ child_file, prompt; "any.json", "true"; Subscription.context_file, "{}\n" ]
    ~prompt_text:
      (Deferred_source_support.prompt_text
       ^ sprintf
           "<tool name=\"%s\" agent=\"%s\" local persistence=\"persistent\"/>"
           tool_name
           child_file)
    ~post_stream:(fun inputs ->
      if !first
      then (
        first := false;
        Direct.function_response
          tool_name
          (`Object [ "input", `String "570 complete the actual authored bootstrap" ]))
      else post_creation_stream inputs)
    ~wrap_env
    (fun env sw _root daemon _policy parent _runtime _revision counters ->
       phase := "parent Goal activates actual persistent authored wrapper";
       let session, before = Goal.create_goal daemon parent in
       let request =
         A.Run_admission_request.Headless.create
           ~session_id:before.identity.session_id
           ~generation:before.identity.generation
           ~expected_revision:before.counters.revision
           ~mode:Workflow
           ~input:
             (User_submission
                { kind = Plain_text
                ; text = "Create the real authored subscription child."
                ; attachments = []
                })
           ~key:(P.Idempotency_key.of_string "570-authored-goal-parent" |> protocol_ok)
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
           ~entry:parent
           ~actor:original
           ~session
           ~request
         |> protocol_ok
         |> Goal.activated
       in
       phase := "actual wrapper and child bootstrap publish terminal receipts";
       let rec await_created () =
         let current = state parent in
         let invocation =
           List.find current.invocations ~f:(fun invocation ->
             String.equal invocation.P.Invocation.context.tool_name tool_name)
         in
         match invocation with
         | Some { status = Published (Complete value); _ }
           when Option.is_none current.active_operation -> current, value
         | Some { status = Resolved (Fail failure) | Published (Fail failure); _ } ->
           raise_s
             [%sexp
               "authored Goal child creation failed", (failure : P.Invocation.tool_error)]
         | Some { status = Admitted | Dispatching | Resolved _ | Published _; _ } | None
           ->
           Eio.Fiber.yield ();
           await_created ()
       in
       let current, value = await_created () in
       [%test_eq: string]
         "completed"
         (Jsonaf.member_exn "status" value |> Jsonaf.string_exn);
       let child_id =
         Jsonaf.member_exn "session_id" value
         |> Jsonaf.string_exn
         |> P.Id.Session.of_string
         |> protocol_ok
       in
       let child = Registry.find (Daemon.registry daemon) child_id |> Option.value_exn in
       let born = state child in
       assert (Option.is_none born.active_operation);
       (match born.lifecycle.observed with
        | Idle -> ()
        | Stopped
        | Queued_for_slot
        | Starting
        | Recovering
        | Running_turn _
        | Compacting _
        | Waiting_for_permission _
        | Stopping
        | Failed _ -> failwith "actual authored bootstrap did not leave idle child");
       (match born.runtime_initialization with
        | Ready -> ()
        | Pending _ -> failwith "actual authored bootstrap did not initialize child");
       let receipt_id =
         Jsonaf.member_exn "receipt" value
         |> Jsonaf.member_exn "receipt_id"
         |> P.History.Id.of_json
         |> protocol_ok
       in
       let bootstrap =
         List.find_exn born.managed_submissions ~f:(fun submission ->
           P.History.Id.equal submission.A.Managed_submission.history_id receipt_id)
       in
       (match bootstrap.status with
        | Terminal (Some _, Completed) -> ()
        | Deferred | Ready | Assigned _
        | Terminal (None, _)
        | Terminal (Some _, (Failed | Cancelled | Interrupted | Invalidated)) ->
          raise_s
            [%sexp
              "authored bootstrap lacks actual completed operation"
            , (bootstrap : A.Managed_submission.t)]);
       let reference = Option.value_exn born.spec.delegation in
       assert (Delegations.Reference.equal reference bootstrap.reference);
       let record =
         Delegations.resolve
           (Agent_store.Session_store.delegations (Daemon.store daemon))
           reference
         |> Existing.store_ok
       in
       let origin = Option.value_exn record.admission.goal_origin in
       let scope =
         match Origin.scopes origin with
         | [ scope ] -> scope
         | [] | _ :: _ -> failwith "authored Goal child did not retain its one real root"
       in
       assert (P.Session_ref.equal (Origin.Scope.owner scope) session);
       assert (P.Id.Goal.equal (Origin.Scope.goal_id scope) Goal.goal_id);
       assert (
         P.Run_receipt.equal
           (P.Goal_episode.admission (Origin.Scope.creator_episode scope))
           admission);
       assert (
         Option.exists
           (GD.origin (Option.value_exn born.goal_descendants))
           ~f:(Origin.equal origin));
       assert (
         List.exists
           (GD.branches (Option.value_exn current.goal_descendants) ~goal_id:Goal.goal_id)
           ~f:(fun branch ->
             Origin.Scope.equal (GD.Branch.scope branch) scope
             && Delegations.Reference.equal (GD.Branch.reference branch) reference));
       let marks = Direct.marks child in
       [%test_eq: int64] 1L marks.turn;
       [%test_eq: int64] 1L marks.attempt;
       [%test_eq: int64] 0L marks.untracked_turns;
       [%test_eq: int64] 0L marks.untracked_attempts;
       Direct.confirmed
         parent
         child
         scope
         reference
         ~turns:marks.turn
         ~attempts:marks.attempt
       |> ignore;
       assert_source env daemon child reference record ~prompt;
       phase := "associated authored child source and bootstrap ACK verified";
       f env sw daemon parent child scope reference counters)
;;
