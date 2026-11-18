open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module G = P.Goal
module Intent = P.Goal_native_start_intent
module Request = P.Goal_native_start_request
module Driver = Agent_server.Goal_native_resume_dispatcher
module Service = Agent_server.Goal_run_service
module Catalog = A.Goal_catalog
module Accounting = A.Goal_accounting
module Ledger = A.Inference_ledger

let input_text = "557 actual native first episode user input"
let start_key = "native-first-episode"
let create_key = "native-first-create"

let evidence =
  G.Reason.of_string "Actual first native episode Turn_end evidence" |> protocol_ok
;;

module Startup_fixture = struct
  type t =
    { workspace_prompt : string option
    ; workspace_project_roots : string list
    ; prepare_sources : Eio_unix.Stdenv.base -> string -> unit
    ; goal_id : P.Id.Goal.t
    ; create_key : string
    ; start_key : string
    ; input_text : string
    }

  let inline =
    { workspace_prompt = None
    ; workspace_project_roots = []
    ; prepare_sources = (fun _ _ -> ())
    ; goal_id = Goal.goal_id
    ; create_key
    ; start_key
    ; input_text
    }
  ;;

  let checked_example =
    let example = "docs-src/examples/learning/goal-requests/" in
    let library = "docs-src/examples/libraries/goal-requests/" in
    let workspace_prompt = example ^ "agent.chatmd" in
    let prepare_sources env root =
      List.iter
        [ workspace_prompt
        ; example ^ "chatml.project.json"
        ; example ^ "moderator.chatml"
        ; library ^ "chatml.project.json"
        ; library ^ "src/goal_requests.chatml"
        ; library ^ "src/goal_requests.chatmli"
        ]
        ~f:(fun relative ->
          let contents = In_channel.read_all (Filename.concat "../.." relative) in
          let path = Eio.Path.(Eio.Stdenv.fs env / root / relative) in
          Eio.Path.mkdirs
            ~exists_ok:true
            ~perm:0o700
            (Eio.Path.split path |> Option.value_exn |> fst);
          Eio.Path.save ~create:(`Exclusive 0o600) path contents)
    in
    { workspace_prompt = Some workspace_prompt
    ; workspace_project_roots = [ library ]
    ; prepare_sources
    ; goal_id = P.Id.Goal.of_string "goal_release_review" |> protocol_ok
    ; create_key = "create-release-review"
    ; start_key = "start-release-review"
    ; input_text = "Review the release notes against accepted changes."
    }
  ;;

  let goal t (state : A.Session_state.t) =
    Catalog.find (Option.value_exn state.goal_catalog) t.goal_id |> Option.value_exn
  ;;

  let accounting t (state : A.Session_state.t) =
    let account =
      A.Goal_accounts.find (Option.value_exn state.goal_accounts) t.goal_id
      |> Option.value_exn
    in
    A.Goal_accounting_document.resolve
      account
      ~goal:(goal t state)
      ~ledger:state.inference_ledger
    |> protocol_ok
  ;;
end

module Later_callback = struct
  type t =
    | Disabled
    | Start_on_explicit_metadata
end

module Native_recurrence = struct
  let id = P.Id.Recurrence.of_string "rec_555_native_start" |> protocol_ok

  let declarations =
    {|<tool name="recurrence_list"/>
<tool name="recurrence_create"/>
<tool name="recurrence_worker" agent="recurrence-worker.chatmd" local persistence="persistent"/>|}
  ;;

  let sources =
    [ ( "recurrence-worker.chatmd"
      , {|<developer>Retained native recurrence worker.</developer>
<script id="worker" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
      )
    ]
  ;;

  let before_finish =
    let period =
      Recurrence_rules.Rule.Period.create_ms 600_000L
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum [%sexp (error : Recurrence_rules.Timing_error.t)])
      |> Result.ok_or_failwith
    in
    let schedule =
      A.Recurrence_definition.Schedule.to_json
        (Interval
           { rule = Recurrence_rules.Rule.Interval.create ~mode:Fixed_rate ~period
           ; first_due = Time_ns.of_string_with_utc_offset "2100-01-01 00:00:00Z"
           })
    in
    sprintf
      {|let* listed = call("recurrence_list", `Object([])) in
  let basis = field(listed, "basis") in
  let requested mode = `Object([
    {key = "id"; value = `String(%S)},
    {key = "basis"; value = `Object([
      {key = "generation"; value = field(basis, "generation")},
      {key = "source"; value = field(basis, "source")},
      {key = "expected_lifecycle_revision"; value = field(basis, "lifecycle_revision")},
      {key = "expected_catalog_revision"; value = field(basis, "catalog_revision")},
      {key = "expected_control_revision"; value = `Null},
      {key = "key"; value = `String("555-native-recurrence-create")}])},
    {key = "capture"; value = `Object([
      {key = "schedule"; value = Json.parse(%S)},
      {key = "policy"; value = Json.parse(%S)},
      {key = "target"; value = `Object([
        {key = "kind"; value = `String("authored_child")},
        {key = "name"; value = `String("recurrence_worker")},
        {key = "mode"; value = `String(mode)}])}])}
  ]) in
  let original = requested("workflow") in
  let* created = call("recurrence_create", original) in
  let* retry = call("recurrence_create", original) in
  let* changed = Tool.call("recurrence_create", requested("single_turn")) in
  let* rejected = match changed with
    | `Error(message) -> if message == "idempotency_conflict" then Task.pure(()) else Task.fail(message)
    | `Ok(_) -> Task.fail("changed recurrence original was accepted") in
  |}
      (P.Id.Recurrence.to_string id)
      (Jsonaf.to_string schedule)
      (A.Recurrence_policy.to_json A.Recurrence_policy.default |> Jsonaf.to_string)
  ;;

  let check (state : A.Session_state.t) ~session ~episode =
    let catalog = Option.value_exn state.recurrence_catalog in
    let entry = A.Recurrence_catalog.find catalog id |> Option.value_exn in
    let definition = A.Recurrence_entry.current entry in
    let module Binding = A.Recurrence_definition.Goal_binding in
    (match A.Recurrence_definition.goal_binding definition with
     | Binding.Unbound -> failwith "actual native Workflow recurrence lost its Goal"
     | Controller_goal binding ->
       assert (P.Session_ref.equal session (Binding.Capture.owner binding));
       assert (P.Id.Goal.equal Goal.goal_id (Binding.Capture.goal_id binding));
       assert (P.Goal_episode.equal episode (Binding.Capture.creator_episode binding)));
    assert (
      A.Recurrence_entry.equal_control
        (Paused Operator_pause)
        (A.Recurrence_entry.control entry));
    [%test_eq: int] 1 (List.length (A.Recurrence_entry.definitions entry));
    let stored =
      match A.Recurrence_catalog.receipts catalog with
      | [ receipt ] -> receipt
      | _ -> failwith "native recurrence replay appended a receipt"
    in
    assert (
      P.Id.Principal.equal
        (P.Goal_episode.admission episode).principal_id
        stored.principal_id);
    let complete, rejected =
      List.filter state.invocations ~f:(fun invocation ->
        String.equal invocation.context.tool_name "recurrence_create")
      |> List.partition_map ~f:(fun (invocation : P.Invocation.t) ->
        match invocation.status with
        | Resolved (Complete value) | Published (Complete value) -> First value
        | Resolved (Fail error) | Published (Fail error) -> Second error.code
        | _ -> failwith "native recurrence invocation did not resolve")
    in
    [%test_eq: string list] [ "idempotency_conflict" ] rejected;
    [%test_eq: int] 2 (List.length complete);
    List.iter complete ~f:(fun value ->
      let actual =
        Jsonaf.member_exn "receipt" value
        |> A.Recurrence_command_receipt.of_json
        |> protocol_ok
      in
      assert (A.Recurrence_command_receipt.equal stored actual))
  ;;
end

let prompt ~recurrence ~(later_callback : Later_callback.t) ~v1 =
  let finish_action =
    P.Run_action.to_json Goal_finish_tests.finish_action |> Jsonaf.to_string
  in
  let turn_end =
    if v1
    then
      sprintf
        {|%slet* current = call("goal_get", get_input) in
  let action = Json.parse(%S) in
  let intent = `Object([
    {key = "expected_goal_revision"; value = field(row(current), "revision")},
    {key = "evidence"; value = `String("Actual first native episode Turn_end evidence")},
    {key = "action"; value = action}
  ]) in
  Task.map(Goal.finish(intent), fun ignored -> state + 1)|}
        (if recurrence then Native_recurrence.before_finish else "")
        finish_action
    else (
      match later_callback with
      | Disabled -> "Task.pure(state)"
      | Start_on_explicit_metadata ->
        {|(match ctx.session_meta with
  | `String(encoded) ->
    let mode = match field(Json.parse(encoded), "mode") with | `String(value) -> value | _ -> "" in
    if mode == "start" then Task.map(call("goal_start", `Object([])), fun ignored -> state + 1)
    else Task.pure(state)
  | _ -> Task.pure(state))|})
  in
  sprintf
    {|<developer>Start one actual Goal episode from its genuine first authored callback.</developer>
<tool name="goal_get"/>
<tool name="goal_create"/>
<tool name="goal_start"/>
%s
<script id="first_native_goal" language="chatml" kind="moderator"%s>
let initial_state = 0
let field value key = match Json.get_field(value, key) with | `Some(value) -> value | _ -> `Null
let goal_id = `String("goal_public_episode")
let get_input = `Object([{key = "goal_id"; value = `Null}])
let row read = match field(read, "goals") with | `Array(rows) -> if Array.length(rows) == 1 then field(rows[0], "goal") else `Null | _ -> `Null
let call name input = Task.bind(Tool.call(name, input), fun result -> match result with | `Ok(value) -> Task.pure(value) | `Error(message) -> Task.fail(message))
let create read = `Object([
  {key = "generation"; value = field(read, "generation")},
  {key = "source"; value = field(read, "source")},
  {key = "goal_id"; value = goal_id},
  {key = "expected_catalog_revision"; value = field(read, "catalog_revision")},
  {key = "key"; value = `String("native-first-create")},
  {key = "objective"; value = `String("Actual native first episode")},
  {key = "budget"; value = `Object([
    {key = "admitted_turns"; value = `Null},
    {key = "admitted_attempts"; value = `Null},
    {key = "observed_tokens"; value = `Null},
    {key = "deadline"; value = `Null}])}
])
let start read = `Object([
  {key = "kind"; value = `String("goal_start_episode_v1")},
  {key = "generation"; value = field(read, "generation")},
  {key = "source"; value = field(read, "source")},
  {key = "goal_id"; value = goal_id},
  {key = "expected_goal_revision"; value = field(row(read), "revision")},
  {key = "expected_catalog_revision"; value = field(read, "catalog_revision")},
  {key = "input"; value = `Object([
    {key = "kind"; value = `String("user_submission")},
    {key = "content"; value = `Object([
      {key = "kind"; value = `String("plain_text")},
      {key = "text"; value = `String(%S)},
      {key = "attachments"; value = `Array([])}])}
  ])},
  {key = "key"; value = `String("native-first-episode")}
])
let on_event ctx state event = match event with
| `Session_start ->
  if state == 0 then (
    let* before = call("goal_get", get_input) in
    let* empty = match field(before, "goals") with
    | `Array(rows) -> if Array.length(rows) == 0 then Task.pure(()) else Task.fail("first Session_start already had a Goal")
    | _ -> Task.fail("first actual Get had no Goal array") in
    let* created = call("goal_create", create(before)) in
    let* projected = call("goal_get", get_input) in
    let* staged = call("goal_start", start(projected)) in
    Task.pure(state + 1)
  ) else Task.fail("first Session_start was replayed")
| `Turn_end ->
  %s
| _ -> Task.pure(state)
</script>|}
    (if recurrence then Native_recurrence.declarations else "")
    (if v1 then " api=\"extensibility-v1\"" else "")
    input_text
    turn_end
;;

let one = Goal_resume_runtime_tests.one_actual

let start_invocation current =
  List.filter_map current.A.Session_state.invocations ~f:(fun invocation ->
    match invocation.P.Invocation.native_goal_action with
    | Some (Start intent) -> Some (invocation, intent)
    | Some (Resume _) | None -> None)
  |> one "actual first native Start invocation"
;;

let find_start_invocation current ~id =
  let invocation =
    List.find_exn current.A.Session_state.invocations ~f:(fun invocation ->
      P.Id.Invocation.equal invocation.P.Invocation.context.id id)
  in
  match invocation.native_goal_action with
  | Some (Start intent) -> invocation, intent
  | Some (Resume _) | None -> failwith "exact Invocation has no native Start intent"
;;

let completed_callbacks current =
  List.iter current.A.Session_state.moderator_executions ~f:(fun callback ->
    match callback.P.Moderator_execution.status with
    | Completed _ | Running -> ()
    | Failed _ | Interrupted _ ->
      raise_s [%sexp "first native callback failed", (callback : P.Moderator_execution.t)])
;;

let user_input_count ?(expected = input_text) current =
  List.count current.A.Session_state.conversation.canonical_history ~f:(fun entry ->
    P.History.equal_role User entry.P.History.role
    && String.is_substring (Jsonaf.to_string entry.payload) ~substring:expected)
;;

let transactions env entry =
  Existing.journal_entries env entry
  |> List.filter ~f:(fun frame ->
    Int.equal (Agent_store.Frame.flags frame.Agent_store.Journal.frame) 0)
  |> List.map ~f:(fun frame ->
    let transaction =
      Agent_store.Transaction.decode
        (Agent_store.Frame.payload frame.Agent_store.Journal.frame)
      |> Existing.store_ok
    in
    let atoms =
      A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
      |> document_ok
      |> A.Session_delta_document.value
      |> Goal_admission_fault_tests.atoms
    in
    transaction, atoms)
;;

let same_invocation expected actual =
  P.Id.Invocation.equal expected.P.Invocation.context.id actual.P.Invocation.context.id
;;

let assert_callback_ack
      ?(created_key = Some create_key)
      env
      entry
      invocation
      accepted
      callback
  =
  (* Inspect the FIRST retained Accepted occurrence. A later publication may
     carry it again, but cannot repair a non-atomic first callback acceptance. *)
  let transaction, atoms =
    transactions env entry
    |> List.filter ~f:(fun (_, atoms) ->
      List.exists atoms ~f:(function
        | A.Session_delta.Invocation_changed actual when same_invocation invocation actual
          ->
          (match actual.native_goal_action with
           | Some (Start value) -> Intent.equal value accepted
           | Some (Resume _) | None -> false)
        | _ -> false))
    |> List.hd_exn
  in
  assert (
    List.exists atoms ~f:(function
      | A.Session_delta.Moderator_execution_changed actual ->
        P.Moderator_execution.equal actual callback
      | _ -> false));
  Option.iter created_key ~f:(fun created_key ->
    assert (
      List.exists atoms ~f:(function
        | A.Session_delta.Goal_catalog_changed catalog ->
          List.exists (Catalog.receipts catalog) ~f:(fun receipt ->
            String.equal
              created_key
              (P.Idempotency_key.to_string receipt.A.Goal_command_receipt.key))
        | _ -> false)));
  transaction
;;

let assert_admission_ack ?(goal_id = Goal.goal_id) env entry invocation admission =
  (* The FIRST Admitted occurrence must itself contain the real Run and Goal
     association; later factual publication is not a substitute for that ACK. *)
  let transaction, atoms =
    transactions env entry
    |> List.filter ~f:(fun (_, atoms) ->
      List.exists atoms ~f:(function
        | A.Session_delta.Invocation_changed actual when same_invocation invocation actual
          ->
          (match actual.native_goal_action with
           | Some (Start intent) ->
             (match Intent.phase intent with
              | Admitted value -> P.Run_receipt.equal value.admission admission
              | Staged | Accepted _ | Dispatch_claimed _ | Discarded _ | Rejected _ ->
                false)
           | Some (Resume _) | None -> false)
        | _ -> false))
    |> List.hd_exn
  in
  [%test_eq: int64] admission.P.Run_receipt.session_revision transaction.session_revision;
  assert (
    List.exists atoms ~f:(function
      | A.Session_delta.Run_state_changed index ->
        Option.is_some (A.Run_state.find index admission.run_id)
        && Option.is_some (A.Run_state.goal_link index admission.run_id)
        && List.exists (A.Run_state.receipts index) ~f:(P.Run_receipt.equal admission)
      | _ -> false));
  assert (
    List.exists atoms ~f:(function
      | A.Session_delta.Goal_catalog_changed catalog ->
        Option.exists (Catalog.find catalog goal_id) ~f:(fun goal ->
          Option.exists (G.episode goal) ~f:(fun episode ->
            P.Run_receipt.equal admission (P.Goal_episode.admission episode)))
      | _ -> false))
;;

type accepted_start =
  { before : A.Session_state.t
  ; invocation : P.Invocation.t
  ; accepted : Intent.t
  ; callback : P.Moderator_execution.t
  ; request : Request.t
  ; callback_transaction : Agent_store.Transaction.t
  ; resume_scheduler : unit -> unit
  ; advance_scheduler : float -> unit
  ; selection : Actor.Goal_native_start.Selection.t
  ; session : P.Session_ref.t
  }

(* Reuse the existing real controlled clocks. The small outer clock only
   observes sleep registration; it does not implement another clock model. *)
let observed_scheduler_clocks env ~pause ~resume ~advance ~await_poll =
  let clocked =
    Goal_control_job_tests.with_controlled_due_clocks env ~pause ~resume ~advance
  in
  let poll_generation = ref 0 in
  let deadline = ref None in
  let changed = Eio.Condition.create () in
  let module Clock = struct
    type t = unit
    type time = Mtime.t

    let now () = Eio.Time.Mono.now clocked#mono_clock

    let sleep_until () actual_deadline =
      incr poll_generation;
      deadline := Some actual_deadline;
      Eio.Condition.broadcast changed;
      Eio.Time.Mono.sleep_until clocked#mono_clock actual_deadline
    ;;
  end
  in
  (await_poll
   := fun () ->
        let prior = !poll_generation in
        !pause ();
        (* Registry is still empty and construction has joined. In this state the
       Schedule_scheduler's 50ms poll is the only server Mono sleep owner.
       Expire its old wait, then observe its NEXT genuine future wait. *)
        !advance 0.1;
        let rec await () =
          if
            !poll_generation > prior
            && Option.exists !deadline ~f:(fun value ->
              Mtime.compare value (Eio.Time.Mono.now clocked#mono_clock) > 0)
          then ()
          else (
            Eio.Condition.await_no_mutex changed;
            await ())
        in
        await ());
  let mono_clock = Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock)) in
  object
    method clock = clocked#clock
    method mono_clock = mono_clock
    method domain_mgr = clocked#domain_mgr
    method stdin = clocked#stdin
    method stdout = clocked#stdout
    method stderr = clocked#stderr
    method net = clocked#net
    method process_mgr = clocked#process_mgr
    method fs = clocked#fs
    method cwd = clocked#cwd
    method secure_random = clocked#secure_random
    method debug = clocked#debug
    method backend_id = clocked#backend_id
  end
;;

let with_accepted_start
      ?test_root
      ?(startup = Startup_fixture.inline)
      ?(wrap_env = Fn.id)
      ?configure
      ?(actor = original)
      ?(before_initial_drain = fun _env _sw _root _daemon _entry -> ())
      ?(later_callback = Later_callback.Disabled)
      ?(recurrence = false)
      ?(timeout_seconds = 30.)
      ~v1
      ~policy
      ~providers
      f
  =
  let pause = ref (fun () -> failwith "scheduler clock not installed") in
  let resume = ref (fun () -> ()) in
  let advance = ref (fun _ -> failwith "scheduler clock not installed") in
  let await_poll = ref (fun () -> failwith "scheduler clock not installed") in
  Creation_tests.with_daemon
    ?test_root
    ?configure
    ~wrap_env:(fun env ->
      observed_scheduler_clocks env ~pause ~resume ~advance ~await_poll |> wrap_env)
    ~prompt_text:(prompt ~recurrence ~later_callback ~v1)
    ?workspace_prompt:startup.workspace_prompt
    ~workspace_project_roots:startup.workspace_project_roots
    ~prepare_sources:startup.prepare_sources
    ~sources:(if recurrence then Native_recurrence.sources else [])
    ~qualify_chatml_extensions:true
    ~policy
    (fun env sw root daemon ->
       protect_cleanup
         ~finally:(fun () -> !resume ())
         (fun () ->
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) timeout_seconds (fun () ->
              let entry =
                Factory.create_session_authenticated
                  (Daemon.factory daemon)
                  ~command_audit:None
                  ~actor
                  (create_request ~start_immediately:true ())
                |> protocol_ok
              in
              protect_cleanup
                ~finally:(fun () ->
                  !resume ();
                  Agent_server.Session_registry.close_entry entry)
                (fun () ->
                   before_initial_drain env sw root daemon entry;
                   (* This is the genuine host's leased lifecycle drain. Legacy already
                completed its constructor callback; v1 has installed startup. No
                replacement Manager, invocation registry or callback issuer. *)
                   (match Owner.drain_idle_moderator entry.runtime with
                    | Ok _ -> ()
                    | Error error ->
                      let current = state entry in
                      let calls =
                        List.map current.invocations ~f:(fun invocation ->
                          ( invocation.P.Invocation.context.tool_name
                          , invocation.context.input
                          , invocation.status ))
                      in
                      raise_s
                        [%sexp
                          "actual first callback drain failed"
                        , (error : P.Error.t)
                        , (current.moderator_executions : P.Moderator_execution.t list)
                        , (calls : (string * Jsonaf.t * P.Invocation.status) list)]);
                   let before =
                     Actor.with_moderator_checkpoint entry.actor (fun () ->
                       Actor.state entry.actor)
                     |> protocol_ok
                   in
                   completed_callbacks before;
                   let invocation, accepted = start_invocation before in
                   (match Intent.phase accepted with
                    | Accepted _ -> ()
                    | Staged | Dispatch_claimed _ | Admitted _ | Discarded _ | Rejected _
                      -> failwith "first callback did not ACK its native Start");
                   (match invocation.status with
                    | Resolved (Complete _) | Published (Complete _) -> ()
                    | Admitted | Dispatching
                    | Resolved (Pending _ | Fail _ | Cancelled _)
                    | Published (Pending _ | Fail _ | Cancelled _) ->
                      failwith "first Start was not a successful actual invocation");
                   let callback =
                     List.filter before.moderator_executions ~f:(fun callback ->
                       P.Moderator_execution.equal_phase
                         Session_start
                         callback.context.phase)
                     |> one "actual first Session_start callback"
                   in
                   (match callback.status with
                    | Completed _ -> ()
                    | Running | Failed _ | Interrupted _ ->
                      failwith "initial callback was not complete");
                   assert (
                     Option.equal
                       P.Id.Moderator_execution.equal
                       (Some callback.context.id)
                       invocation.parent_event);
                   assert (Option.is_none callback.context.operation_id);
                   assert (Option.is_none before.active_operation);
                   assert (Option.is_none before.failure);
                   assert (not before.halted);
                   [%test_eq: int] 0 !providers;
                   [%test_eq: int] 0 (user_input_count before);
                   let index = Option.value_exn before.run_state in
                   assert (List.is_empty (A.Run_state.runs index));
                   let created = Startup_fixture.goal startup before in
                   assert (G.State.equal (Open (Held Source_review)) (G.state created));
                   assert (Option.is_none (G.episode created));
                   [%test_eq: int64] 0L (G.revision created);
                   assert (P.Id.Principal.equal principal.id (G.creator created));
                   assert (
                     P.Id.Principal.equal principal.id (Intent.principal_id accepted));
                   let request = Intent.request accepted in
                   assert (P.Run_source.equal (G.source created) (Request.source request));
                   let installed_source =
                     A.Run_source_installation.captured
                       (A.Run_state.installation index)
                       ~generation:before.identity.generation
                     |> protocol_ok
                   in
                   assert (P.Run_source.equal installed_source (Request.source request));
                   assert (
                     P.Moderator_observer.equal
                       (Request.source request).observer
                       callback.context.source);
                   assert (P.Id.Goal.equal startup.goal_id (Request.goal_id request));
                   [%test_eq: int] before.identity.generation (Request.generation request);
                   [%test_eq: int64]
                     (G.revision created)
                     (Request.expected_goal_revision request);
                   [%test_eq: int64]
                     (Catalog.revision (Option.value_exn before.goal_catalog))
                     (Request.expected_catalog_revision request);
                   [%test_eq: string]
                     startup.start_key
                     (P.Idempotency_key.to_string (Request.key request));
                   (match Request.input request with
                    | User_submission content ->
                      [%test_eq: string] startup.input_text content.text;
                      assert (List.is_empty content.attachments)
                    | Authored_start ->
                      failwith "native first input replayed consumed authored startup");
                   let initial_account = Startup_fixture.accounting startup before in
                   [%test_eq: int64 * int64]
                     (0L, 0L)
                     ( Accounting.admitted_turns initial_account
                     , Accounting.admitted_attempts initial_account );
                   assert (List.is_empty (Accounting.bindings initial_account));
                   let callback_transaction =
                     assert_callback_ack
                       ~created_key:(Some startup.create_key)
                       env
                       entry
                       invocation
                       accepted
                       callback
                   in
                   let selection =
                     match Actor.select_native_goal_action entry.actor |> protocol_ok with
                     | Some (Start selection) -> selection
                     | Some (Resume _) | None ->
                       failwith
                         "released callback/source did not expose its actual Start \
                          selection"
                   in
                   assert (
                     P.Id.Invocation.equal
                       invocation.context.id
                       (Actor.Goal_native_start.Selection.invocation_id selection));
                   assert (
                     Request.equal
                       request
                       (Actor.Goal_native_start.Selection.request selection));
                   assert (List.is_empty (Registry.entries (Daemon.registry daemon)));
                   !await_poll ();
                   Registry.add
                     (Daemon.registry daemon)
                     ~session_id:before.identity.session_id
                     entry
                   |> protocol_ok;
                   let session = Actor.Goal_native_start.Selection.session selection in
                   f
                     env
                     sw
                     root
                     daemon
                     entry
                     { before
                     ; invocation
                     ; accepted
                     ; callback
                     ; request
                     ; callback_transaction
                     ; resume_scheduler = !resume
                     ; advance_scheduler = !advance
                     ; selection
                     ; session
                     }))))
;;

let run ?(recurrence = false) ~v1 () =
  let providers = ref 0
  and ports = ref 0
  and transcripts = ref [] in
  let entered, enter = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let release_provider () =
    if Option.is_none (Eio.Promise.peek release) then Eio.Promise.resolve release_u ()
  in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs ->
      incr providers;
      if !providers > 1 then failwith "first native episode dispatched an extra provider";
      transcripts
      := [ `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string ];
      Eio.Promise.resolve enter ();
      Eio.Promise.await release;
      completed_response "actual first native episode completion")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          incr ports;
          base.runtime_inference_ports actor)
    }
  in
  let with_fixture = with_accepted_start ~recurrence ~v1 ~policy ~providers in
  with_fixture (fun env _sw _root daemon entry fixture ->
    protect_cleanup ~finally:release_provider (fun () ->
      let { before = _
          ; invocation
          ; accepted = _
          ; callback
          ; request
          ; callback_transaction
          ; resume_scheduler = _
          ; advance_scheduler = _
          ; selection
          ; session = _
          }
        =
        fixture
      in
      let created = Goal.goal fixture.before in
      let driver = Driver.create ~factory:(Daemon.factory daemon) in
      protect_cleanup
        ~finally:(fun () -> Driver.close driver)
        (fun () ->
           let ports_before = !ports in
           Driver.process_entry driver entry |> protocol_ok;
           Eio.Promise.await entered;
           let during = state entry in
           completed_callbacks during;
           let _, admitted = start_invocation during in
           let admission =
             match Intent.phase admitted with
             | Admitted value -> value.admission
             | Staged | Accepted _ | Dispatch_claimed _ | Discarded _ | Rejected _ ->
               failwith "provider entered without actual native admission"
           in
           assert (P.Id.Principal.equal principal.id admission.principal_id);
           assert (P.Run_source.equal (Request.source request) admission.source);
           assert (P.Idempotency_key.equal (Request.key request) admission.key);
           [%test_eq: string]
             (Request.digest request |> protocol_ok)
             admission.request_sha256;
           assert (
             Int64.(admission.session_revision > callback_transaction.session_revision));
           assert_admission_ack env entry invocation admission;
           let run_index = Option.value_exn during.run_state in
           [%test_eq: int] 1 (List.length (A.Run_state.runs run_index));
           let run = A.Run_state.find run_index admission.run_id |> Option.value_exn in
           [%test_eq: int]
             1
             (List.count (A.Run_state.receipts run_index) ~f:(fun actual ->
                P.Run_receipt.Kind.equal Admission actual.P.Run_receipt.kind
                && P.Id.Run.equal actual.run_id admission.run_id));
           let episode = G.episode (Goal.goal during) |> Option.value_exn in
           let link =
             A.Run_state.goal_link run_index admission.run_id |> Option.value_exn
           in
           assert (P.Run.Mode.equal Workflow run.mode);
           assert (P.Id.Principal.equal principal.id run.principal_id);
           assert (P.Run_source.equal (G.source created) run.source);
           assert (P.Goal_episode.matches_run episode run);
           assert (P.Run_receipt.equal admission (P.Goal_episode.admission episode));
           assert (P.Id.Goal.equal Goal.goal_id (A.Run_goal_link.goal_id link));
           assert (P.Goal_episode.equal episode (A.Run_goal_link.episode link));
           [%test_eq: string]
             (A.Goal_native_start_association.intent request
              |> protocol_ok
              |> A.Goal_episode_admission.digest)
             (A.Run_goal_link.intent_sha256 link);
           [%test_eq: int] 1 (user_input_count during);
           [%test_eq: int] 1 !providers;
           [%test_eq: int] (ports_before + 1) !ports;
           assert (String.is_substring (List.hd_exn !transcripts) ~substring:input_text);
           let operation = Option.value_exn during.active_operation in
           [%test_eq: P.Operation.kind] (Turn User_submit) operation.kind;
           assert (
             List.exists run.owned_work ~f:(fun work ->
               P.Run_work.Key.equal work.key (Operation operation.id)));
           let retained () =
             match
               Service.start_native_episode (Daemon.factory daemon) ~entry ~selection
               |> protocol_ok
             with
             | Retained actual -> assert (P.Run_receipt.equal admission actual)
             | Attempted _ ->
               failwith "historical native retry acquired a fresh source attempt"
           in
           let assert_no_effects_retry current =
             let frames = Existing.frames env entry in
             let counts = !ports, !providers in
             retained ();
             Driver.process_entry driver entry |> protocol_ok;
             Existing.assert_same current entry;
             [%test_eq: string list] frames (Existing.frames env entry);
             [%test_eq: int * int] counts (!ports, !providers)
           in
           assert_no_effects_retry during;
           (* The synchronous Driver has returned from real Service and
              source cleanup. Complete inference only afterwards, so v1
              terminal ACK retires the already-source-closed loan. *)
           release_provider ();
           let rec await_final () =
             let current =
               Actor.with_moderator_checkpoint entry.actor (fun () ->
                 Actor.state entry.actor)
               |> protocol_ok
             in
             completed_callbacks current;
             let summary =
               Ledger.summary current.inference_ledger |> P.Inference_query.Summary.turns
             in
             if
               Int64.(
                 summary.failed > 0L || summary.cancelled > 0L || summary.interrupted > 0L)
             then failwith "first native inference ended adversely";
             let terminal =
               match
                 (A.Run_state.find (Option.value_exn current.run_state) admission.run_id
                  |> Option.value_exn)
                   .lifecycle
               with
               | Terminal (Completed None) -> true
               | Terminal _ -> failwith "first native Run ended adversely"
               | Admitted | Active | Waiting _ -> false
             in
             if
               Option.is_none current.active_operation
               && Int64.equal summary.completed 1L
               && List.for_all current.moderator_executions ~f:(fun callback ->
                 match callback.P.Moderator_execution.status with
                 | Completed _ -> true
                 | Running | Failed _ | Interrupted _ -> false)
               && ((not v1) || terminal)
             then current
             else (
               Eio.Fiber.yield ();
               await_final ())
           in
           let final = await_final () in
           if recurrence
           then Native_recurrence.check final ~session:fixture.session ~episode;
           let summary = Ledger.summary final.inference_ledger in
           [%test_eq: int64] 1L (P.Inference_query.Summary.retained_attempts summary);
           [%test_eq: int64] 0L (P.Inference_query.Summary.turns summary).pending;
           let account = Goal.accounting final in
           [%test_eq: int64 * int64]
             (1L, 1L)
             (Accounting.admitted_turns account, Accounting.admitted_attempts account);
           [%test_eq: int64 * int64]
             (0L, 0L)
             (Accounting.untracked_turns account, Accounting.untracked_attempts account);
           let binding =
             Accounting.bindings account |> one "actual first native Goal account binding"
           in
           assert (P.Id.Run.equal admission.run_id (Accounting.Binding.run_id binding));
           assert (P.Run_source.equal admission.source (Accounting.Binding.source binding));
           [%test_eq: P.Id.Operation.t option]
             (Some operation.id)
             (Ledger.Handle.operation_id (Accounting.Binding.handle binding));
           [%test_eq: int] 1 (user_input_count final);
           [%test_eq: int] 1 !providers;
           [%test_eq: int]
             1
             (List.length (Catalog.receipts (Option.value_exn final.goal_catalog)));
           assert (
             List.exists
               final.moderator_executions
               ~f:(P.Moderator_execution.equal callback));
           if v1
           then (
             let achievement =
               match G.state (Goal.goal final) with
               | Closed (Achieved value) -> value
               | Open _ | Closed (Abandoned _) ->
                 failwith
                   "actual Turn_end Finish did not achieve its native first episode"
             in
             let terminal =
               Goal_finish_tests.terminal_receipt
                 (Option.value_exn final.run_state)
                 admission
             in
             assert (P.Run_receipt.equal terminal (P.Goal_achievement.receipt achievement));
             assert (
               P.Goal_episode.equal
                 episode
                 (P.Goal_episode.Settlement.episode
                    (P.Goal_achievement.settlement achievement)));
             assert (
               G.Reason.equal
                 evidence
                 (P.Goal_achievement.Request.evidence
                    (P.Goal_achievement.request achievement)));
             assert (
               P.Run_action.equal
                 Goal_finish_tests.finish_action
                 (P.Goal_achievement.Request.action
                    (P.Goal_achievement.request achievement))))
           else assert (G.State.equal (Open Enabled) (G.state (Goal.goal final)));
           assert (
             Option.is_none (Actor.select_native_goal_action entry.actor |> protocol_ok));
           (* Source scopes joined at synchronous Driver return. A later
              terminal ACK may remove the private slot. Observe the same
              actual dispatcher Selection again: factual historical
              admission must remain observable without resurrecting it.
              Original retirement itself is private and source-reviewed;
              the preceding None is not its proof. *)
           if v1
           then (
             let frames = Existing.frames env entry in
             let counts = !ports, !providers in
             Actor.observe_native_goal_start_dispatch entry.actor ~selection ~failure:None
             |> protocol_ok;
             Existing.assert_same final entry;
             [%test_eq: string list] frames (Existing.frames env entry);
             [%test_eq: int * int] counts (!ports, !providers));
           assert_no_effects_retry final)))
;;

let%expect_test
    "fresh legacy Session_start native Create and Start owns its first actual input"
  =
  run ~v1:false ();
  print_endline
    "actual fresh legacy source claim and callback ACK; one native initial \
     Run/input/account; historical retry dispatches nothing";
  [%expect
    {| actual fresh legacy source claim and callback ACK; one native initial Run/input/account; historical retry dispatches nothing |}]
;;

let%expect_test "fresh v1 native Start reaches actual Goal.finish terminal evidence" =
  run ~v1:true ~recurrence:true ();
  print_endline
    "actual fresh v1 Create-to-Start admission; one initial input/account; scoped \
     Goal.finish retains exact terminal evidence and no replay; actual native recurrence \
     freezes that Workflow and ACKs exact retry once";
  [%expect
    {| actual fresh v1 Create-to-Start admission; one initial input/account; scoped Goal.finish retains exact terminal evidence and no replay; actual native recurrence freezes that Workflow and ACKs exact retry once |}]
;;

module Example_recovery = struct
  module H = Agent_client.Session_handle
  module C = A.Prompt_catalog

  let write_revised_source env root goal_id =
    let path =
      Eio.Path.(
        Eio.Stdenv.fs env
        / root
        / "docs-src/examples/learning/goal-requests/moderator.chatml")
    in
    let source = Eio.Path.load path in
    let finish_json =
      Goal_finish_tests.finish_action |> P.Run_action.to_json |> Jsonaf.to_string
    in
    let completion =
      sprintf
        {|  | `Turn_end ->
      let* query = checked(Requests.get(%S)) in
      let* view = call("goal_get", query) in
      let* revision = goal_revision(view) in
      let intent = `Object([
        {key = "expected_goal_revision"; value = `String(revision)},
        {key = "evidence"; value = `String("An explicit operator requested the revised acceptance fixture.")},
        {key = "action"; value = Json.parse(%S)}
      ]) in
      Task.map(Goal.finish(intent), fun ignored -> staged)
|}
        (P.Id.Goal.to_string goal_id)
        finish_json
    in
    let marker = "  | _ -> Task.pure(staged)" in
    let source =
      String.substr_replace_first source ~pattern:marker ~with_:(completion ^ marker)
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) path source
  ;;

  let with_reopened
        ?(configure = fun _root config -> config)
        env
        sw
        root
        daemon
        startup
        policy
        wrap_env
        entry
        f
    =
    let session_id = (state entry).identity.session_id in
    let store = Daemon.store daemon in
    Daemon.shutdown daemon |> protocol_ok;
    assert (Agent_store.Session_store.is_closed store);
    let prompt =
      Filename.concat root (Option.value_exn startup.Startup_fixture.workspace_prompt)
    in
    let config = config root root prompt in
    let config =
      { config with
        server =
          { config.server with
            project_sources =
              Some
                { Agent_server.Project_source_configuration.local_only with
                  external_roots =
                    List.map startup.workspace_project_roots ~f:(Filename.concat root)
                }
          }
      }
    in
    let reopened =
      Daemon.start
        ~sw
        ~env:(wrap_env (env :> Eio_unix.Stdenv.base))
        ~config:(configure root config)
        ~tool_dir:root
        ~home:root
        ~process_start_identity:None
        ~options:
          { Daemon.default_options with
            qualify_chatml_extensions = true
          ; inference_policy = policy
          }
        ()
      |> protocol_ok
    in
    protect_cleanup
      ~finally:(fun () -> Daemon.shutdown reopened |> protocol_ok)
      (fun () ->
         let actual =
           Registry.load (Daemon.registry reopened) session_id |> protocol_ok
         in
         assert (not (phys_equal actual.actor entry.actor));
         f reopened actual)
  ;;

  let install_revised_source env sw daemon entry =
    let client = connection daemon principal in
    initialize client;
    let session_id = (state entry).identity.session_id in
    let handle =
      H.attach
        ~sw
        ~clock:(Eio.Stdenv.clock env)
        ~connection:client
        ~session_id
        ~mode:Read_write
        ~subscribe:false
        ()
      |> protocol_ok
    in
    protect_cleanup
      ~finally:(fun () ->
        H.close handle;
        Agent_client.Connection.close client)
      (fun () ->
         H.stop handle ~mode:Cancel |> protocol_ok |> ignore;
         Owner.unload_and_wait entry.runtime |> protocol_ok;
         let before = state entry in
         let catalog = Daemon.prompts daemon in
         let target_revision =
           match (one "packaged Goal prompt" (C.entries catalog)).availability with
           | Ready revision -> A.Prompt_revision.id revision
           | Unavailable details ->
             raise_s
               [%sexp
                 "updated authored Goal prompt is unavailable"
               , (details : A.Prompt_revision_builder.Diagnostic.t list)]
           | Disabled -> failwith "updated authored Goal prompt is disabled"
         in
         assert (
           not (P.Id.Prompt_revision.equal before.spec.prompt_revision_id target_revision));
         Agent_client.Connection.request
           client
           (Session_upgrade_prompt
              { session_id
              ; attachment_id = (H.attachment handle).id
              ; expected_revision = before.counters.revision
              ; target_revision
              ; allow_migration = true
              ; idempotency_key =
                  P.Idempotency_key.of_string "example-prompt-upgrade" |> protocol_ok
              })
         |> protocol_ok
         |> ignore;
         H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
         Owner.with_loaded_runtime entry.runtime (fun runtime ->
           Goal_native_test_support.await_activation entry runtime;
           Ok ())
         |> protocol_ok;
         assert (
           P.Id.Prompt_revision.equal
             (state entry).spec.prompt_revision_id
             target_revision))
  ;;

  let compact ?(connect = connection) env sw daemon (entry : Registry.entry) =
    let before = state entry in
    let client = connect daemon principal in
    protect_cleanup
      ~finally:(fun () -> Agent_client.Connection.close client)
      (fun () ->
         initialize client;
         let handle =
           H.attach
             ~sw
             ~clock:(Eio.Stdenv.clock env)
             ~connection:client
             ~session_id:before.identity.session_id
             ~mode:Read_write
             ~subscribe:false
             ()
           |> protocol_ok
         in
         protect_cleanup
           ~finally:(fun () -> H.close handle)
           (fun () ->
              let observed =
                H.projection handle
                |> Agent_client.Projection.snapshot
                |> P.Public.Snapshot.fields
              in
              let admitted =
                H.compact handle ~expected_revision:(Some observed.revision)
                |> protocol_ok
              in
              let operation =
                match admitted.observed_state with
                | Compacting operation -> operation
                | _ -> failwith "authenticated compaction was not admitted"
              in
              operation, Goal_native_test_support.await_quiet entry))
  ;;

  let command entry session startup mutation ~key ~source =
    let current = state entry in
    let request =
      A.Goal_command.create
        ~session_id:current.identity.session_id
        ~generation:current.identity.generation
        ~source
        ~expected_catalog_revision:
          (Catalog.revision (Option.value_exn current.goal_catalog))
        ~expected_goal_revision:(Some (G.revision (Startup_fixture.goal startup current)))
        ~key:(P.Idempotency_key.of_string key |> protocol_ok)
        ~mutation
      |> protocol_ok
    in
    Actor.goal_command entry.Registry.actor ~actor:original ~authorize ~session ~request
  ;;

  let start daemon entry session startup =
    let current = state entry in
    let request =
      A.Run_admission_request.Headless.create
        ~session_id:current.identity.session_id
        ~generation:current.identity.generation
        ~expected_revision:current.counters.revision
        ~mode:Workflow
        ~input:
          (User_submission
             { kind = Plain_text
             ; text = "Explicitly accept the revised release review."
             ; attachments = []
             })
        ~key:(P.Idempotency_key.of_string "example-revised-episode" |> protocol_ok)
      |> protocol_ok
      |> fun request ->
      A.Goal_activation_request.create
        ~goal_id:startup.Startup_fixture.goal_id
        ~expected_goal_revision:(G.revision (Startup_fixture.goal startup current))
        ~expected_catalog_revision:
          (Catalog.revision (Option.value_exn current.goal_catalog))
        ~request
      |> protocol_ok
    in
    let service =
      Service.create ~factory:(Daemon.factory daemon) ~authorize:(fun _ -> authorize)
    in
    Service.start_episode service ~entry ~actor:original ~session ~request
    |> protocol_ok
    |> Goal.activated
  ;;
end

let%expect_test "checked Goal requests example stages and admits one real Workflow" =
  let startup = Startup_fixture.checked_example in
  let providers = ref 0 in
  let summary_text = "Goal history summary from the compaction provider." in
  let second_input = "Explicitly accept the revised release review." in
  let armed = ref None in
  let reached, reached_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let release_worker () =
    if Option.is_none (Eio.Promise.peek release) then Eio.Promise.resolve release_u ()
  in
  let wrap_env env =
    Crash_fault_io.wrap
      env
      ~boundary:Before_rename
      ~matches:(fun path ->
        Option.exists !armed ~f:(fun directory ->
          String.equal directory (Filename.dirname path)
          && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
          && String.is_suffix path ~suffix:".tmp"))
      ~reached:(fun path ->
        armed := None;
        Eio.Promise.resolve reached_u path;
        Eio.Promise.await release)
  in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs ->
      Int.incr providers;
      let expected =
        match !providers with
        | 1 | 2 -> startup.input_text
        | 3 -> second_input
        | _ -> failwith "restart or adoption dispatched an unexpected provider"
      in
      let transcript =
        `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
      in
      assert (String.is_substring transcript ~substring:expected);
      (* This fixture sends a completed response with no reported token counts. *)
      completed_response
        (if Int.equal !providers 2
         then summary_text
         else "Release review awaits explicit achievement evidence"))
  in
  with_accepted_start
    ~startup
    ~wrap_env
    ~v1:true
    ~policy
    ~providers
    (fun env sw root daemon entry fixture ->
       let driver = Driver.create ~factory:(Daemon.factory daemon) in
       protect_cleanup
         ~finally:(fun () -> Driver.close driver)
         (fun () ->
            Driver.process_entry driver entry |> protocol_ok;
            let rec await_account () =
              let current =
                Actor.with_moderator_checkpoint entry.actor (fun () ->
                  Actor.state entry.actor)
                |> protocol_ok
              in
              completed_callbacks current;
              let summary =
                Ledger.summary current.inference_ledger |> P.Inference_query.Summary.turns
              in
              if
                Option.is_none current.active_operation
                && Int64.equal summary.completed 1L
              then current
              else (
                Eio.Fiber.yield ();
                await_account ())
            in
            let current = await_account () in
            let invocation, intent = start_invocation current in
            assert (same_invocation fixture.invocation invocation);
            let admission =
              match Intent.phase intent with
              | Admitted value -> value.admission
              | Staged | Accepted _ | Dispatch_claimed _ | Discarded _ | Rejected _ ->
                failwith "checked example did not admit the actual Workflow"
            in
            assert_admission_ack ~goal_id:startup.goal_id env entry invocation admission;
            let goal = Startup_fixture.goal startup current in
            let episode = G.episode goal |> Option.value_exn in
            assert (P.Run_receipt.equal (P.Goal_episode.admission episode) admission);
            assert (G.State.equal (Open Enabled) (G.state goal));
            let account = Startup_fixture.accounting startup current in
            [%test_eq: int64 * int64]
              (1L, 1L)
              (Accounting.admitted_turns account, Accounting.admitted_attempts account);
            let usage_of (snapshot : A.Session_state.t) =
              Accounting.usage
                (Startup_fixture.accounting startup snapshot)
                ~ledger:snapshot.inference_ledger
              |> Result.map_error ~f:(fun error ->
                Sexp.to_string_hum (Accounting.Error.sexp_of_t error))
              |> Result.ok_or_failwith
            in
            let usage = usage_of current in
            let input = A.Goal_usage.component usage Input in
            [%test_eq: int64]
              1L
              (A.Goal_usage.Unknown.count
                 (A.Goal_usage.Metric.unknown input)
                 Not_reported);
            assert (
              P.Inference_query.Metric.equal_sum
                (Tokens 0L)
                (A.Goal_usage.Metric.actual input));
            assert (
              P.Inference_query.Metric.equal_sum
                (Tokens 0L)
                (A.Goal_usage.Metric.estimated input));
            [%test_eq: int] 1 (user_input_count ~expected:startup.input_text current);
            let progress =
              G.Progress.of_string "Inspected the initial release notes." |> protocol_ok
            in
            Example_recovery.command
              entry
              fixture.session
              startup
              (Record_progress (startup.goal_id, progress))
              ~key:"example-before-compaction-progress"
              ~source:(Some (G.source goal))
            |> protocol_ok
            |> ignore;
            let current = state entry in
            let goal = Startup_fixture.goal startup current in
            assert (G.Progress.equal progress (G.progress goal));
            let catalog = Option.value_exn current.goal_catalog in
            let hold =
              A.Goal_command.create
                ~session_id:current.identity.session_id
                ~generation:current.identity.generation
                ~source:None
                ~expected_catalog_revision:(Catalog.revision catalog)
                ~expected_goal_revision:(Some (G.revision goal))
                ~key:(P.Idempotency_key.of_string "example-operator-hold" |> protocol_ok)
                ~mutation:(Hold (startup.goal_id, Operator_pause))
              |> protocol_ok
            in
            Actor.goal_command
              entry.actor
              ~actor:original
              ~authorize
              ~session:fixture.session
              ~request:hold
            |> protocol_ok
            |> ignore;
            let held = state entry in
            assert (
              G.State.equal
                (Open (Held Operator_pause))
                (G.state (Startup_fixture.goal startup held)));
            assert (Accounting.equal account (Startup_fixture.accounting startup held));
            Driver.process_entry driver entry |> protocol_ok;
            [%test_eq: int] 1 !providers;
            let metadata (seen : A.Session_state.t) =
              A.Goal_native_metadata_view.capture
                seen
                ~session:fixture.session
                ~goal_id:(Some startup.goal_id)
              |> protocol_ok
              |> A.Goal_native_metadata_view.to_json
              |> protocol_ok
            in
            let operation, compacted = Example_recovery.compact env sw daemon entry in
            assert (Option.is_none compacted.failure);
            [%test_eq: int]
              (held.conversation.compaction_generation + 1)
              compacted.conversation.compaction_generation;
            [%test_eq: int]
              (List.length held.conversation.compaction_archives + 1)
              (List.length compacted.conversation.compaction_archives);
            assert (Document_schema.Json.equal (metadata held) (metadata compacted));
            assert (
              Accounting.equal account (Startup_fixture.accounting startup compacted));
            [%test_eq: int] 0 (user_input_count ~expected:startup.input_text compacted);
            let archive = List.hd_exn compacted.conversation.compaction_archives in
            assert (A.Session_state.Compaction_archive.equal_kind Compaction archive.kind);
            assert (P.Id.Operation.equal operation archive.operation_id);
            [%test_eq: int]
              1
              (List.count compacted.conversation.canonical_history ~f:(fun entry ->
                 String.is_substring
                   (Jsonaf.to_string entry.P.History.payload)
                   ~substring:summary_text));
            let original =
              Accounting.bindings account |> one "Goal-owned attempt before compaction"
            in
            let compacted_rows = Ledger.rows compacted.inference_ledger in
            [%test_eq: int] 2 (List.length compacted_rows);
            let summary =
              List.filter compacted_rows ~f:(fun row ->
                not
                  (Ledger.Handle.equal
                     (Accounting.Binding.handle original)
                     (Ledger.Row.handle row)))
              |> one "separate compaction-only inference attempt"
            in
            (match
               Inference.Observation.Attempt_record.state (Ledger.Row.record summary)
             with
             | Terminal terminal
               when Inference.Event.Terminal.equal_outcome
                      (Inference.Event.Terminal.outcome terminal)
                      Completed -> ()
             | _ -> failwith "independent compaction inference did not complete");
            [%test_eq: int64]
              1L
              (P.Inference_query.Summary.turns
                 (Ledger.summary compacted.inference_ledger))
                .completed;
            [%test_eq: int] 2 !providers;
            Example_recovery.write_revised_source env root startup.goal_id;
            fixture.resume_scheduler ();
            Example_recovery.with_reopened
              env
              sw
              root
              daemon
              startup
              policy
              wrap_env
              entry
              (fun reopened loaded ->
                 let recovered = state loaded in
                 assert (
                   P.Id.Prompt_revision.equal
                     held.spec.prompt_revision_id
                     recovered.spec.prompt_revision_id);
                 let recovered_goal = Startup_fixture.goal startup recovered in
                 assert (
                   G.State.equal (Open (Held Operator_pause)) (G.state recovered_goal));
                 assert (G.Progress.equal progress (G.progress recovered_goal));
                 [%test_eq: int]
                   compacted.conversation.compaction_generation
                   recovered.conversation.compaction_generation;
                 assert (P.Run_source.equal (G.source goal) (G.source recovered_goal));
                 assert (
                   Accounting.equal account (Startup_fixture.accounting startup recovered));
                 assert (A.Goal_usage.equal usage (usage_of recovered));
                 let old_index = Option.value_exn recovered.run_state in
                 let old_run =
                   A.Run_state.find old_index admission.run_id |> Option.value_exn
                 in
                 (match old_run.lifecycle with
                  | Terminal Interrupted -> ()
                  | _ -> failwith "cold reopen did not settle the old Workflow");
                 let old_terminal =
                   Goal_finish_tests.terminal_receipt old_index admission
                 in
                 let old_link =
                   A.Run_state.goal_link old_index old_run.id |> Option.value_exn
                 in
                 assert (
                   same_invocation fixture.invocation (fst (start_invocation recovered)));
                 assert (Option.is_none recovered.active_operation);
                 assert (
                   Option.is_none
                     (Actor.select_native_goal_action loaded.actor |> protocol_ok));
                 [%test_eq: int] 2 !providers;
                 Example_recovery.install_revised_source env sw reopened loaded;
                 let changed = state loaded in
                 let _, source = A.Goal_catalog_source.capture changed |> protocol_ok in
                 assert (not (P.Run_source.equal source (G.source recovered_goal)));
                 assert (
                   Int64.(
                     source.installation_epoch
                     > (G.source recovered_goal).installation_epoch));
                 completed_callbacks changed;
                 assert (
                   List.exists changed.moderator_executions ~f:(fun callback ->
                     P.Moderator_execution.equal_phase
                       Session_start
                       callback.context.phase
                     && P.Moderator_observer.equal source.observer callback.context.source));
                 assert (
                   P.Run_source.equal
                     (G.source recovered_goal)
                     (G.source (Startup_fixture.goal startup changed)));
                 let command = Example_recovery.command loaded fixture.session startup in
                 let progress =
                   G.Progress.of_string "documented new release review" |> protocol_ok
                 in
                 (match
                    command
                      (Record_progress (startup.goal_id, progress))
                      ~key:"example-before-adoption"
                      ~source:(Some source)
                  with
                  | Error error -> [%test_eq: P.Error.code] Conflict error.code
                  | Ok _ ->
                    failwith "replacement edited the Goal before explicit adoption");
                 command
                   (Adopt_source startup.goal_id)
                   ~key:"example-adopt"
                   ~source:(Some source)
                 |> protocol_ok
                 |> ignore;
                 let adopted = state loaded in
                 let adopted_goal = Startup_fixture.goal startup adopted in
                 assert (G.State.equal (Open (Held Source_review)) (G.state adopted_goal));
                 assert (P.Run_source.equal source (G.source adopted_goal));
                 assert (
                   Accounting.equal account (Startup_fixture.accounting startup adopted));
                 assert (Option.is_none adopted.active_operation);
                 assert (
                   P.Goal_episode.equal
                     episode
                     (G.episode adopted_goal |> Option.value_exn));
                 [%test_eq: int] 2 !providers;
                 let response_directory =
                   Agent_store.Session_store.Handle.responses_directory
                     (Option.value_exn loaded.store_handle)
                 in
                 armed := Some response_directory;
                 protect_cleanup ~finally:release_worker (fun () ->
                   let completion =
                     Example_recovery.start reopened loaded fixture.session startup
                   in
                   assert (not (P.Id.Run.equal admission.run_id completion.run_id));
                   assert (P.Run_source.equal completion.source source);
                   let rec await_pending () =
                     let pending = state loaded in
                     completed_callbacks pending;
                     if Option.is_some (Eio.Promise.peek reached)
                     then pending
                     else (
                       Eio.Fiber.yield ();
                       await_pending ())
                   in
                   let pending = await_pending () in
                   let pending_goal = Startup_fixture.goal startup pending in
                   assert (G.State.equal (Open Enabled) (G.state pending_goal));
                   let requested =
                     G.achievement_request pending_goal |> Option.value_exn
                   in
                   assert (Option.is_none (G.request_settlement pending_goal));
                   let pending_index = Option.value_exn pending.run_state in
                   let second_run =
                     A.Run_state.find pending_index completion.run_id |> Option.value_exn
                   in
                   let operation = Option.value_exn pending.active_operation in
                   assert (
                     List.exists second_run.owned_work ~f:(fun work ->
                       P.Run_work.Key.equal work.key (Operation operation.id)));
                   (match second_run.lifecycle with
                    | Active -> ()
                    | _ -> failwith "new Workflow ended before the real worker barrier");
                   assert (
                     P.Run_receipt.Kind.equal
                       Action
                       (P.Goal_achievement.Request.receipt requested).kind);
                   assert (
                     not
                       (List.exists
                          (A.Run_state.receipts pending_index)
                          ~f:(fun receipt ->
                            P.Id.Run.equal receipt.run_id completion.run_id
                            && P.Run_receipt.Kind.equal Terminal receipt.kind)));
                   release_worker ();
                   let rec await_achieved () =
                     let final = state loaded in
                     completed_callbacks final;
                     match G.state (Startup_fixture.goal startup final) with
                     | Closed (Achieved actual) when Option.is_none final.active_operation
                       -> final, actual
                     | Closed (Abandoned _) ->
                       failwith "revised acceptance abandoned its Goal"
                     | Open _ | Closed (Achieved _) ->
                       Eio.Fiber.yield ();
                       await_achieved ()
                   in
                   let final, actual = await_achieved () in
                   let index = Option.value_exn final.run_state in
                   let actual_terminal =
                     Goal_finish_tests.terminal_receipt index completion
                   in
                   let final_goal = Startup_fixture.goal startup final in
                   let predecessor =
                     G.previous_episode_settlement final_goal |> Option.value_exn
                   in
                   assert (
                     P.Run_receipt.equal
                       old_terminal
                       (P.Goal_episode.Settlement.receipt predecessor));
                   assert (
                     P.Goal_achievement.Request.equal
                       requested
                       (P.Goal_achievement.request actual));
                   assert (
                     P.Run_receipt.equal
                       actual_terminal
                       (P.Goal_achievement.receipt actual));
                   assert (
                     P.Run.equal
                       old_run
                       (A.Run_state.find index old_run.id |> Option.value_exn));
                   assert (
                     A.Run_goal_link.equal
                       old_link
                       (A.Run_state.goal_link index old_run.id |> Option.value_exn));
                   assert (
                     P.Run_receipt.equal
                       old_terminal
                       (Goal_finish_tests.terminal_receipt index admission));
                   [%test_eq: int] 3 !providers;
                   [%test_eq: int] 0 (user_input_count ~expected:startup.input_text final);
                   [%test_eq: int] 1 (user_input_count ~expected:second_input final);
                   let final_account = Startup_fixture.accounting startup final in
                   [%test_eq: int64 * int64]
                     (2L, 2L)
                     ( Accounting.admitted_turns final_account
                     , Accounting.admitted_attempts final_account );
                   A.Goal_run_evidence.validate
                     ~catalog:final.goal_catalog
                     ~runs:final.run_state
                   |> protocol_ok))));
  print_endline
    "checked example stages once, compacts and reopens its held Goal, explicitly adopts, \
     and joins real achievement";
  [%expect
    {| checked example stages once, compacts and reopens its held Goal, explicitly adopts, and joins real achievement |}]
;;

let%expect_test "fresh v1 startup original expires before claim and unload retires it" =
  let live = ref true in
  let guarded = Goal_resume_tests.guarded principal live in
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr providers;
      failwith "initial original rejection dispatched a provider")
  in
  with_accepted_start
    ~actor:guarded
    ~v1:true
    ~policy
    ~providers
    ~before_initial_drain:(fun env _sw _root _daemon entry ->
      let before = state entry in
      assert (Owner.is_loaded entry.runtime);
      assert (
        A.Session_state.Runtime_initialization.equal before.runtime_initialization Ready);
      assert (List.is_empty before.moderator_executions);
      assert (List.is_empty before.invocations);
      assert (List.is_empty (A.Run_state.runs (Option.value_exn before.run_state)));
      let frames = Existing.frames env entry in
      (* The actual authenticated physical install is complete, but no ordinary
         Session_start claim has consumed its one original yet. *)
      live := false;
      (match Owner.drain_idle_moderator entry.runtime with
       | Error error ->
         [%test_eq: P.Error.code] Permission_denied error.code;
         assert (not error.retryable)
       | Ok _ -> failwith "revoked initial original allowed a startup callback");
      Existing.assert_same before entry;
      [%test_eq: string list] frames (Existing.frames env entry);
      [%test_eq: int] 0 !providers;
      [%test_eq: int] 0 (user_input_count (state entry));
      (* Actual unload joins and removes the old physical worker/Manager. A
         later explicit authenticated installation uses the existing current
         same-principal proof, rather than reviving the revoked runtime loan.
         Capture requires Unadmitted, so success also checks old-cell retirement. *)
      Owner.unload_and_wait entry.runtime |> protocol_ok;
      assert (not (Owner.is_loaded entry.runtime));
      Owner.ensure_loaded_authenticated entry.runtime ~actor:original |> protocol_ok;
      assert (Owner.is_loaded entry.runtime);
      [%test_eq: int] 0 !providers)
    (fun _env _sw _root _daemon _entry fixture ->
       (* The shared helper now runs the genuine first callback and every exact
         source/Create/Accepted ACK/zero-execution assertion unchanged. *)
       assert (P.Id.Principal.equal principal.id (Intent.principal_id fixture.accepted));
       [%test_eq: int] 0 !providers);
  print_endline
    "revoked installed first-start original denies claim without changes; joined unload \
     retires it; fresh authenticated installation ACKs once";
  [%expect
    {| revoked installed first-start original denies claim without changes; joined unload retires it; fresh authenticated installation ACKs once |}]
;;
