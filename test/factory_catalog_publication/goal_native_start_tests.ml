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

module Later_callback = struct
  type t =
    | Disabled
    | Start_on_explicit_metadata
end

let prompt ~(later_callback : Later_callback.t) ~v1 =
  let finish_action =
    P.Run_action.to_json Goal_finish_tests.finish_action |> Jsonaf.to_string
  in
  let turn_end =
    if v1
    then
      sprintf
        {|let* current = call("goal_get", get_input) in
  let action = Json.parse(%S) in
  let intent = `Object([
    {key = "expected_goal_revision"; value = field(row(current), "revision")},
    {key = "evidence"; value = `String("Actual first native episode Turn_end evidence")},
    {key = "action"; value = action}
  ]) in
  Task.map(Goal.finish(intent), fun ignored -> state + 1)|}
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

let user_input_count current =
  List.count current.A.Session_state.conversation.canonical_history ~f:(fun entry ->
    P.History.equal_role User entry.P.History.role
    && String.is_substring (Jsonaf.to_string entry.payload) ~substring:input_text)
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

let assert_admission_ack env entry invocation admission =
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
        Option.exists (Catalog.find catalog Goal.goal_id) ~f:(fun goal ->
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
      ?(wrap_env = Fn.id)
      ?(actor = original)
      ?(before_initial_drain = fun _env _sw _root _daemon _entry -> ())
      ?(later_callback = Later_callback.Disabled)
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
    ~wrap_env:(fun env ->
      observed_scheduler_clocks env ~pause ~resume ~advance ~await_poll |> wrap_env)
    ~prompt_text:(prompt ~later_callback ~v1)
    ~qualify_chatml_extensions:true
    ~policy
    (fun env sw root daemon ->
       protect_cleanup
         ~finally:(fun () -> !resume ())
         (fun () ->
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
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
                  entry.close ())
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
                   let created = Goal.goal before in
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
                   assert (P.Id.Goal.equal Goal.goal_id (Request.goal_id request));
                   [%test_eq: int] before.identity.generation (Request.generation request);
                   [%test_eq: int64]
                     (G.revision created)
                     (Request.expected_goal_revision request);
                   [%test_eq: int64]
                     (Catalog.revision (Option.value_exn before.goal_catalog))
                     (Request.expected_catalog_revision request);
                   [%test_eq: string]
                     start_key
                     (P.Idempotency_key.to_string (Request.key request));
                   (match Request.input request with
                    | User_submission content ->
                      [%test_eq: string] input_text content.text;
                      assert (List.is_empty content.attachments)
                    | Authored_start ->
                      failwith "native first input replayed consumed authored startup");
                   let initial_account = Goal.accounting before in
                   [%test_eq: int64 * int64]
                     (0L, 0L)
                     ( Accounting.admitted_turns initial_account
                     , Accounting.admitted_attempts initial_account );
                   assert (List.is_empty (Accounting.bindings initial_account));
                   let callback_transaction =
                     assert_callback_ack env entry invocation accepted callback
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
                     ; selection
                     ; session
                     }))))
;;

let run ~v1 =
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
  with_accepted_start ~v1 ~policy ~providers (fun env _sw _root daemon entry fixture ->
    protect_cleanup ~finally:release_provider (fun () ->
      let { before = _
          ; invocation
          ; accepted = _
          ; callback
          ; request
          ; callback_transaction
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
  run ~v1:false;
  print_endline
    "actual fresh legacy source claim and callback ACK; one native initial \
     Run/input/account; historical retry dispatches nothing";
  [%expect
    {| actual fresh legacy source claim and callback ACK; one native initial Run/input/account; historical retry dispatches nothing |}]
;;

let%expect_test "fresh v1 native Start reaches actual Goal.finish terminal evidence" =
  run ~v1:true;
  print_endline
    "actual fresh v1 Create-to-Start admission; one initial input/account; scoped \
     Goal.finish retains exact terminal evidence and no replay";
  [%expect
    {| actual fresh v1 Create-to-Start admission; one initial input/account; scoped Goal.finish retains exact terminal evidence and no replay |}]
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
