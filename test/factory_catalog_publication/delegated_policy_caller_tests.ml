open! Core
open Agent_server_test_support
open Retry_support
module F = Delegated_policy_caller_support
module Warm = Delegated_warm_resource_tests
module I = P.Invocation
module E = P.Moderator_execution

let one name = function
  | [ actual ] -> actual
  | _ -> failwith (name ^ " was not unique")
;;

let assert_same_installed before entry =
  let actual = Warm.loaded entry in
  assert (phys_equal before.A.Runtime_builder.worker actual.worker);
  assert (Option.equal phys_equal before.moderator_manager actual.moderator_manager);
  assert (Option.equal phys_equal before.native_runtime actual.native_runtime);
  assert (
    Option.equal phys_equal before.moderator_script_tools actual.moderator_script_tools);
  assert (
    phys_equal
      (Inference_client.Execution.context before.inference_execution)
      (Inference_client.Execution.context actual.inference_execution));
  assert (phys_equal before.parse_user_content actual.parse_user_content);
  assert (
    Option.equal
      Agent_store.Prompt_source_receipt.equal
      (A.Prompt_source_lease.receipt before.source_lease)
      (A.Prompt_source_lease.receipt actual.source_lease))
;;

let assert_parent_no_reconstruction
      (before : A.Session_state.t)
      (after : A.Session_state.t)
  =
  assert (
    P.Id.Prompt_revision.equal
      before.A.Session_state.spec.prompt_revision_id
      after.A.Session_state.spec.prompt_revision_id);
  assert (P.Run_source.equal (Warm.source before) (Warm.source after));
  assert (
    List.equal
      P.History.equal_entry
      before.conversation.canonical_history
      after.conversation.canonical_history);
  [%test_eq: int64]
    before.conversation.reserved_history_through
    after.conversation.reserved_history_through;
  [%test_eq: int64]
    before.conversation.next_history_sequence
    after.conversation.next_history_sequence
;;

let assert_native_inner (invocation : I.t) file =
  match invocation.status with
  | Resolved (Complete (`String value)) ->
    assert (String.is_substring value ~substring:("592 " ^ file))
  | Admitted | Dispatching
  | Resolved (Complete _ | Pending _ | Fail _ | Cancelled _)
  | Published _ -> failwith "actual nested/event native result was not retained"
;;

let assert_observed parent ids =
  let current = F.state parent in
  List.iter ids ~f:(fun id ->
    let invocation =
      List.find_exn current.A.Session_state.invocations ~f:(fun invocation ->
        P.Id.Invocation.equal invocation.I.context.id id)
    in
    let observation = Option.value_exn invocation.observation in
    assert (I.equal_observation_status Observed observation.status))
;;

let%expect_test
    "real child Caller reaches all three policy ports and preserves B or admitted None"
  =
  F.with_fixture (fun t ->
    let ports = !(t.counters.inference_ports) in
    let child_reference = Option.value_exn t.child_initial.spec.delegation in
    assert (
      P.Id.Session.equal
        child_reference.key.parent_session_id
        t.parent_initial.identity.session_id);
    assert (
      not
        (P.Id.Principal.equal
           (Operator_authorization.principal t.a).id
           (Operator_authorization.principal t.b).id));
    assert (Operator_authorization.is_current t.a);
    assert (Operator_authorization.is_current t.b);
    List.iter [ t.parent_initial; t.child_initial ] ~f:(fun current ->
      assert (List.is_empty current.jobs && List.is_empty current.schedules);
      Option.iter current.goal_catalog ~f:(fun catalog ->
        assert (List.is_empty (A.Goal_catalog.goals catalog)));
      Option.iter current.goal_descendants ~f:(fun descendants ->
        assert (List.is_empty (A.Goal_descendants.roots descendants));
        assert (List.is_empty (A.Goal_descendants.local_included descendants))));
    let originals =
      F.with_ambient t (fun before ->
        let direct_state, operation, direct =
          F.run_child
            t
            ~actor:(Some t.b)
            ~name:"read_file"
            ~file:"direct.txt"
            ~id:"592-direct-B"
        in
        assert (I.equal_origin direct.context.origin Model);
        assert (Option.is_none direct.context.parent_invocation);
        assert (Option.is_none direct.parent_event);
        let ending =
          Warm.completed direct_state Turn_end
          |> List.filter ~f:(fun receipt ->
            Option.exists
              receipt.E.context.operation_id
              ~f:(P.Id.Operation.equal operation)
            && List.exists direct_state.invocations ~f:(fun invocation ->
              Option.exists
                invocation.I.parent_event
                ~f:(P.Id.Moderator_execution.equal receipt.context.id)))
          |> one "actual child Turn_end owning its native call"
        in
        let event_native =
          List.filter direct_state.invocations ~f:(fun invocation ->
            Option.exists
              invocation.I.parent_event
              ~f:(P.Id.Moderator_execution.equal ending.context.id))
          |> one "actual child Event native call"
        in
        assert (I.equal_origin event_native.context.origin Moderator);
        assert (Option.is_none event_native.context.parent_invocation);
        assert_native_inner event_native "event.txt";
        let nested_state, _, outer =
          F.run_child
            t
            ~actor:(Some t.b)
            ~name:"nested_read"
            ~file:"nested.txt"
            ~id:"592-nested-B"
        in
        assert (I.equal_origin outer.context.origin Model);
        let nested_native =
          List.filter nested_state.invocations ~f:(fun invocation ->
            Option.exists
              invocation.I.context.parent_invocation
              ~f:(P.Id.Invocation.equal outer.context.id))
          |> one "actual child Native nested call"
        in
        assert (I.equal_origin nested_native.context.origin Script);
        assert (Option.is_none nested_native.parent_event);
        assert_native_inner nested_native "nested.txt";
        let originals =
          List.map [ direct; event_native; outer; nested_native ] ~f:(fun invocation ->
            let _, native = F.assert_policy_correspondence t invocation in
            native.I.context.id)
        in
        [%test_eq: int] 4 (List.length (F.policy_receipts (F.state t.parent)));
        assert_parent_no_reconstruction before (F.state t.parent);
        assert_same_installed t.parent_runtime t.parent;
        assert_same_installed t.child_runtime t.child;
        originals)
    in
    (* A's actual cancelled operation has joined. Its proof is still current;
       no Stop/source rotation manufactured or restored the B continuations. *)
    assert (Operator_authorization.is_current t.a);
    let observed =
      F.within
        t
        "actual parent observations retain B after callback and A join"
        (fun () -> F.observe_original t ~expected:(Some t.b) ~count:4)
    in
    assert (
      List.equal
        P.Id.Invocation.equal
        (List.sort originals ~compare:P.Id.Invocation.compare)
        (List.sort observed ~compare:P.Id.Invocation.compare));
    assert_observed t.parent originals;
    let admitted_none =
      F.with_ambient t (fun before ->
        let _, _, invocation =
          F.run_child
            t
            ~actor:None
            ~name:"read_file"
            ~file:"none.txt"
            ~id:"592-real-admitted-None"
        in
        let _, native = F.assert_policy_correspondence t invocation in
        [%test_eq: int] 5 (List.length (F.policy_receipts (F.state t.parent)));
        assert_parent_no_reconstruction before (F.state t.parent);
        native.I.context.id)
    in
    let observed_none =
      F.within
        t
        "actual admitted None remains distinct from ambient A and missing"
        (fun () -> F.observe_original t ~expected:None ~count:1)
    in
    assert (List.equal P.Id.Invocation.equal [ admitted_none ] observed_none);
    assert_observed t.parent [ admitted_none ];
    assert_same_installed t.parent_runtime t.parent;
    assert_same_installed t.child_runtime t.child;
    List.iter
      [ F.state t.parent; F.state t.child ]
      ~f:(fun current ->
        [%test_eq: int] 1 (List.length (Warm.completed current Session_start));
        [%test_eq: int] 0 (List.length (Warm.completed current Session_resume));
        assert (Option.is_none current.active_operation));
    [%test_eq: int] ports !(t.counters.inference_ports);
    [%test_eq: int] 6 t.provider.child_calls;
    [%test_eq: int] 2 t.provider.ambient_calls;
    [%test_eq: int] 8 !(t.counters.providers);
    assert (Operator_authorization.is_current t.b);
    assert (Operator_authorization.is_current t.a));
  print_endline
    "actual model/native/Event Callers fed exact parent claims; B and admitted None \
     outlived callbacks without ambient fallback or runtime replay";
  [%expect
    {| actual model/native/Event Callers fed exact parent claims; B and admitted None outlived callbacks without ambient fallback or runtime replay |}]
;;
