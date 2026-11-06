open! Core
open Agent_server_test_support
open Retry_support
module G = P.Goal
module Catalog = A.Goal_catalog
module Native = Goal_native_test_support

let goal_id = P.Id.Goal.of_string "goal_native_metadata" |> protocol_ok
let rolled_back_id = P.Id.Goal.of_string "goal_native_metadata_rollback" |> protocol_ok

let prompt ~v1 =
  sprintf
    {|<developer>Use selected native Goal metadata in the actual callback transaction.</developer>
<tool name="goal_get"/>
<tool name="goal_create"/>
<tool name="goal_objective"/>
<tool name="goal_progress"/>
<tool name="goal_hold"/>
<tool name="goal_abandon"/>
<script id="metadata_owner" language="chatml" kind="moderator"%s>
let initial_state = 0
let field value key = match Json.get_field(value, key) with | `Some(value) -> value | _ -> `Null
let goal_id = `String("goal_native_metadata")
let row read = match field(read, "goals") with | `Array(rows) -> if Array.length(rows) == 1 then field(rows[0], "goal") else `Null | _ -> `Null
let get_input = `Object([{key = "goal_id"; value = goal_id}])
let call name input = Task.bind(Tool.call(name, input), fun result -> match result with | `Ok(value) -> Task.pure(value) | `Error(message) -> Task.fail(message))
let create read id key = `Object([
  {key = "generation"; value = field(read, "generation")},
  {key = "source"; value = field(read, "source")},
  {key = "goal_id"; value = id},
  {key = "expected_catalog_revision"; value = field(read, "catalog_revision")},
  {key = "key"; value = `String(key)},
  {key = "objective"; value = `String("native initial objective")},
  {key = "budget"; value = `Object([
    {key = "admitted_turns"; value = `Null},
    {key = "admitted_attempts"; value = `Null},
    {key = "observed_tokens"; value = `Null},
    {key = "deadline"; value = `Null}])}])
let edit read kind key value = `Object([
  {key = "generation"; value = field(read, "generation")},
  {key = "source"; value = field(read, "source")},
  {key = "goal_id"; value = goal_id},
  {key = "expected_catalog_revision"; value = field(read, "catalog_revision")},
  {key = "expected_goal_revision"; value = field(row(read), "revision")},
  {key = "key"; value = `String(key)},
  {key = "kind"; value = `String(kind)},
  {key = "value"; value = `String(value)}])
let control read kind key = `Object([
  {key = "generation"; value = field(read, "generation")},
  {key = "goal_id"; value = goal_id},
  {key = "expected_catalog_revision"; value = field(read, "catalog_revision")},
  {key = "expected_goal_revision"; value = field(row(read), "revision")},
  {key = "key"; value = `String(key)},
  {key = "kind"; value = `String(kind)},
  {key = "reason"; value = `String("actual selected metadata control")}])
let on_event ctx state event = match event with
| `Turn_end ->
  if state == 0 then
    let* before = call("goal_get", `Object([{key = "goal_id"; value = `Null}])) in
    let create_request = create(before, goal_id, "metadata-create") in
    let* created = call("goal_create", create_request) in
    let* first = call("goal_get", get_input) in
    let* objective = call("goal_objective", (edit(first, "objective", "metadata-objective", "native revised objective"))) in
    let* second = call("goal_get", get_input) in
    let* progress = call("goal_progress", (edit(second, "progress", "metadata-progress", "native durable progress"))) in
    let* third = call("goal_get", get_input) in
    let* held = call("goal_hold", (control(third, "hold", "metadata-hold"))) in
    let* fourth = call("goal_get", get_input) in
    let* abandoned = call("goal_abandon", (control(fourth, "abandon", "metadata-abandon"))) in
    let* retry = call("goal_create", create_request) in
    let* fifth = call("goal_get", get_input) in
    let rollback = Task.bind(call("goal_create", create(fifth, `String("goal_native_metadata_rollback"), "metadata-rollback")), fun ignored -> Task.fail("actual nested rollback")) in
    let* caught = Task.catch(rollback, fun ignored -> Task.pure(())) in
    let* final = call("goal_get", `Object([{key = "goal_id"; value = `Null}])) in
    Task.pure(state + 1)
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
    (if v1 then " api=\"extensibility-v1\"" else "")
;;

let%expect_test
    "selected Goal metadata registrations reach real legacy and v1 callback services"
  =
  List.iter [ true; false ] ~f:(fun v1 ->
    let providers = ref 0 in
    let policy =
      inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
        incr providers;
        completed_response "actual selected metadata callback")
    in
    Creation_tests.with_daemon
      ~prompt_text:(prompt ~v1)
      ~qualify_chatml_extensions:true
      ~policy
      (fun env _sw _root daemon ->
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
           Deferred_source_support.with_loaded_entry
             daemon
             (fun ~entry ~runtime:_ ~revision:_ ->
                let initial = state entry in
                assert (Option.is_none initial.goal_catalog);
                Native.with_ordinary_owner daemon entry ~v1 (fun _receipt ->
                  let current = Native.await_quiet entry in
                  let catalog = Option.value_exn current.goal_catalog in
                  let goal = Catalog.find catalog goal_id |> Option.value_exn in
                  assert (Option.is_none (Catalog.find catalog rolled_back_id));
                  [%test_eq: int] 1 (List.length (Catalog.goals catalog));
                  [%test_eq: int] 5 (List.length (Catalog.receipts catalog));
                  [%test_eq: int64] 4L (G.revision goal);
                  [%test_eq: string]
                    "native revised objective"
                    (G.Objective.to_string (G.objective goal));
                  [%test_eq: string]
                    "native durable progress"
                    (G.Progress.to_string (G.progress goal));
                  let reason =
                    G.Reason.of_string "actual selected metadata control" |> protocol_ok
                  in
                  assert (G.State.equal (Closed (Abandoned reason)) (G.state goal));
                  assert (Option.is_none (G.episode goal));
                  let account =
                    A.Goal_accounts.find (Option.value_exn current.goal_accounts) goal_id
                    |> Option.value_exn
                    |> fun document ->
                    A.Goal_accounting_document.resolve
                      document
                      ~goal
                      ~ledger:current.inference_ledger
                    |> protocol_ok
                  in
                  [%test_eq: int64 * int64]
                    (0L, 0L)
                    ( A.Goal_accounting.admitted_turns account
                    , A.Goal_accounting.admitted_attempts account );
                  [%test_eq: int] 1 !providers;
                  assert (
                    List.exists current.moderator_executions ~f:(fun receipt ->
                      match
                        receipt.P.Moderator_execution.context.phase, receipt.status
                      with
                      | Turn_end, Completed _ -> true
                      | _ -> false));
                  List.iter
                    [ "goal_get"
                    ; "goal_create"
                    ; "goal_objective"
                    ; "goal_progress"
                    ; "goal_hold"
                    ; "goal_abandon"
                    ]
                    ~f:(fun name ->
                      assert (
                        List.exists current.invocations ~f:(fun invocation ->
                          String.equal invocation.P.Invocation.context.tool_name name
                          &&
                          match invocation.status with
                          | Resolved (Complete _) | Published (Complete _) -> true
                          | Admitted | Dispatching
                          | Resolved (Pending _ | Fail _ | Cancelled _)
                          | Published (Pending _ | Fail _ | Cancelled _) -> false)));
                  assert (
                    List.exists current.invocations ~f:(fun invocation ->
                      String.equal invocation.P.Invocation.context.tool_name "goal_create"
                      && (match
                            Document_schema.Json.field
                              invocation.context.input
                              ~name:"key"
                          with
                          | Value (`String "metadata-rollback") -> true
                          | Absent | Null | Value _ -> false)
                      &&
                      match invocation.status with
                      | Resolved (Complete _) | Published (Complete _) -> true
                      | Admitted | Dispatching
                      | Resolved (Pending _ | Fail _ | Cancelled _)
                      | Published (Pending _ | Fail _ | Cancelled _) -> false));
                  (* Failed nested Task retains its real native Complete staged result,
                 but neither its Goal nor its command receipt is accepted. *)
                  assert (
                    not
                      (List.exists (Catalog.receipts catalog) ~f:(fun receipt ->
                         String.equal
                           (P.Idempotency_key.to_string
                              receipt.A.Goal_command_receipt.key)
                           "metadata-rollback"))))))));
  print_endline
    "real selected metadata tools; callback projection and nested rollback; stable zero \
     account";
  [%expect
    {| real selected metadata tools; callback projection and nested rollback; stable zero account |}]
;;

let negative_prompt =
  prompt ~v1:true
  |> String.substr_replace_first ~pattern:"<tool name=\"goal_abandon\"/>" ~with_:""
  |> String.substr_replace_first
       ~pattern:"    let* third = call(\"goal_get\", get_input) in"
       ~with_:
         {|    let stale = call("goal_objective", edit(first, "objective", "metadata-stale", "must not replace objective")) in
    let* denied = Task.catch(Task.map(stale, fun ignored -> false), fun ignored -> Task.pure(true)) in
    let* checked = if denied then Task.pure(()) else Task.fail("stale selected edit unexpectedly completed") in
    let* third = call("goal_get", get_input) in|}
  |> String.substr_replace_first
       ~pattern:
         {|    let* abandoned = call("goal_abandon", (control(fourth, "abandon", "metadata-abandon"))) in|}
       ~with_:
         {|    let* missing = Tool.call("goal_abandon", control(fourth, "abandon", "metadata-abandon")) in
    let* abandoned = match missing with
      | `Error(message) -> if message == "invocation.unselected_tool" then Task.pure(`Null) else Task.fail(message)
      | `Ok(_) -> Task.fail("unselected native control unexpectedly completed") in|}
;;

let%expect_test
    "unselected Goal control and caught stale edit preserve callback projection"
  =
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr providers;
      completed_response "actual selected metadata denial")
  in
  Creation_tests.with_daemon
    ~prompt_text:negative_prompt
    ~qualify_chatml_extensions:true
    ~policy
    (fun env _sw _root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         Deferred_source_support.with_loaded_entry
           daemon
           (fun ~entry ~runtime:_ ~revision:_ ->
              Native.with_ordinary_owner daemon entry ~v1:true (fun _receipt ->
                let current = Native.await_quiet entry in
                let catalog = Option.value_exn current.goal_catalog in
                let goal = Catalog.find catalog goal_id |> Option.value_exn in
                [%test_eq: int] 1 (List.length (Catalog.goals catalog));
                [%test_eq: int] 4 (List.length (Catalog.receipts catalog));
                [%test_eq: int64] 3L (G.revision goal);
                [%test_eq: string]
                  "native revised objective"
                  (G.Objective.to_string (G.objective goal));
                [%test_eq: string]
                  "native durable progress"
                  (G.Progress.to_string (G.progress goal));
                assert (Option.is_none (Catalog.find catalog rolled_back_id));
                let reason =
                  G.Reason.of_string "actual selected metadata control" |> protocol_ok
                in
                assert (G.State.equal (Open (Held (Needs_input reason))) (G.state goal));
                assert (
                  not
                    (List.exists current.invocations ~f:(fun invocation ->
                       String.equal
                         invocation.P.Invocation.context.tool_name
                         "goal_abandon")));
                assert (
                  List.exists current.invocations ~f:(fun invocation ->
                    String.equal
                      invocation.P.Invocation.context.tool_name
                      "goal_objective"
                    && (match
                          Document_schema.Json.field invocation.context.input ~name:"key"
                        with
                        | Value (`String "metadata-stale") -> true
                        | Absent | Null | Value _ -> false)
                    &&
                    match invocation.status with
                    | Resolved (Fail _) | Published (Fail _) -> true
                    | Admitted | Dispatching
                    | Resolved (Complete _ | Pending _ | Cancelled _)
                    | Published (Complete _ | Pending _ | Cancelled _) -> false));
                assert (
                  List.exists current.moderator_executions ~f:(fun receipt ->
                    match receipt.P.Moderator_execution.context.phase, receipt.status with
                    | Turn_end, Completed _ -> true
                    | _ -> false));
                let account =
                  A.Goal_accounts.find (Option.value_exn current.goal_accounts) goal_id
                  |> Option.value_exn
                  |> fun document ->
                  A.Goal_accounting_document.resolve
                    document
                    ~goal
                    ~ledger:current.inference_ledger
                  |> protocol_ok
                in
                [%test_eq: int64 * int64]
                  (0L, 0L)
                  ( A.Goal_accounting.admitted_turns account
                  , A.Goal_accounting.admitted_attempts account );
                [%test_eq: int] 1 !providers))));
  print_endline
    "absent selection denied before invocation; stale selected edit caught; actual \
     parent ACK preserved";
  [%expect
    {| absent selection denied before invocation; stale selected edit caught; actual parent ACK preserved |}]
;;
