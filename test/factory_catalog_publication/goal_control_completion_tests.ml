open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module G = P.Goal
module Catalog = A.Goal_catalog
module Service = Agent_server.Goal_run_service
module Control = Actor.Goal_callback_control
module Resume = Goal_resume_runtime_tests

(* Legacy Pending_cold preserves actual installed history and Goal source,
   then uses the real Pending Init Session_resume before initial user input.
   Ready_cold has the same saved callback with prior Ready history. V1 retains
   actual initial Session_start after installation and joined resource retirement. *)
module Path = struct
  type t =
    | Pending_cold
    | Ready_cold
    | Installed_start
  [@@deriving sexp_of]
end

module Decision = struct
  type t =
    | Hold_progress
    | Abandon
  [@@deriving sexp_of]
end

let control_key = "goal-control-completion"
let reason = G.Reason.of_string "actual callback control completion" |> protocol_ok

let replace_prompt text ~pattern ~with_ =
  assert (String.is_substring text ~substring:pattern);
  String.substr_replace_first text ~pattern ~with_
;;

let native_tools =
  {|<tool name="goal_get"/>
<tool name="goal_hold"/>
<tool name="goal_progress"/>
<tool name="goal_abandon"/>|}
;;

let native_functions =
  {|let field value key = match Json.get_field(value, key) with | `Some(value) -> value | _ -> `Null
let row read = match field(read, "goals") with | `Array(rows) -> if Array.length(rows) == 1 then field(rows[0], "goal") else `Null | _ -> `Null
let has_goal read = match field(read, "goals") with | `Array(rows) -> Array.length(rows) == 1 | _ -> false
let goal_id = `String("goal_public_episode")
let get_input = `Object([{key = "goal_id"; value = `Null}])
let call name input = Task.bind(Tool.call(name, input), fun result -> match result with | `Ok(value) -> Task.pure(value) | `Error(message) -> Task.fail(message))
let control read kind = `Object([
  {key = "generation"; value = field(read, "generation")},
  {key = "goal_id"; value = goal_id},
  {key = "expected_catalog_revision"; value = field(read, "catalog_revision")},
  {key = "expected_goal_revision"; value = field(row(read), "revision")},
  {key = "key"; value = `String("goal-control-completion")},
  {key = "kind"; value = `String(kind)},
  {key = "reason"; value = `String("actual callback control completion")}])
let progress read = `Object([
  {key = "generation"; value = field(read, "generation")},
  {key = "source"; value = field(read, "source")},
  {key = "goal_id"; value = goal_id},
  {key = "expected_catalog_revision"; value = field(read, "catalog_revision")},
  {key = "expected_goal_revision"; value = field(row(read), "revision")},
  {key = "key"; value = `String("goal-control-later-progress")},
  {key = "kind"; value = `String("progress")},
  {key = "value"; value = `String("progress after the exact Hold")}])|}
;;

let prompt path decision =
  let event =
    match path with
    | Path.Pending_cold | Ready_cold -> "Session_resume"
    | Installed_start -> "Session_start"
  in
  let api =
    match path with
    | Path.Pending_cold | Ready_cold -> ""
    | Installed_start -> " api=\"extensibility-v1\""
  in
  let action =
    match decision with
    | Decision.Hold_progress ->
      {|let* held = call("goal_hold", control(before, "hold")) in
    let* after_hold = call("goal_get", get_input) in
    let* progressed = call("goal_progress", progress(after_hold)) in
    Task.pure(state + 1)|}
    | Abandon ->
      {|let* abandoned = call("goal_abandon", control(before, "abandon")) in
    Task.pure(state + 1)|}
  in
  sprintf
    {|<developer>Real selected native control completes its actual Goal callback.</developer>
%s
<script id="goal_control_completion" language="chatml" kind="moderator"%s>
let initial_state = 0
%s
let on_event ctx state event = match event with
| `%s ->
  let* before = call("goal_get", get_input) in
  if has_goal(before) then (
    %s
  ) else Task.fail("actual Goal callback lacks its one Goal")
| _ -> Task.pure(state)
</script>|}
    native_tools
    api
    native_functions
    event
    action
;;

let with_initial_entry daemon f =
  let entry =
    Factory.create_session_authenticated
      (Daemon.factory daemon)
      ~command_audit:None
      ~actor:original
      (create_request ~start_immediately:false ())
    |> protocol_ok
  in
  (match
     Registry.add
       (Daemon.registry daemon)
       ~session_id:(state entry).identity.session_id
       entry
   with
   | Ok () -> ()
   | Error error ->
     entry.close ();
     raise_s [%sexp (error : P.Error.t)]);
  protect_cleanup ~finally:entry.close (fun () ->
    assert (not (Owner.is_loaded entry.runtime));
    assert (
      A.Session_state.Runtime_initialization.equal
        Ready
        (state entry).runtime_initialization);
    f entry)
;;

(* Retain a genuinely installed checkpoint before capturing the Goal. The
   documented preparer and no-Init administration owner then commit Pending on
   that SAME selected state. Snapshot/source/Goal/generation remain actual; no
   first-ever Session_start or replacement observer is invented. *)
let with_pending_entry daemon f =
  with_initial_entry daemon (fun entry ->
    let session, _ = Goal.create_goal daemon entry in
    let writer, _ =
      Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
    in
    protect_cleanup
      ~finally:(fun () -> Actor.detach entry.actor writer.id |> protocol_ok)
      (fun () ->
         let expected = Resume.checkpoint entry in
         let candidate =
           Factory.prepare_administration
             (Daemon.factory daemon)
             entry
             ~previous:expected
             expected
             ~fresh_history:false
           |> protocol_ok
         in
         Owner.retire_administration
           entry.runtime
           ~validate:(fun () ->
             Actor.validate_administration_basis
               entry.actor
               ~attachment_id:writer.id
               ~expected)
           ~commit:(fun () ->
             Actor.commit_reconciled_administration
               entry.actor
               ~command_audit:None
               ~attachment_id:writer.id
               ~expected
               ~kind:Edit
               candidate)
         |> protocol_ok
         |> ignore;
         A.History_id_source.discard_reserved entry.history_ids;
         let pending = Resume.checkpoint entry in
         [%test_eq: int] expected.identity.generation pending.identity.generation;
         assert (
           A.Session_state.Runtime_initialization.equal
             (Pending { fresh_history = false })
             pending.runtime_initialization);
         assert (Option.equal Jsonaf.exactly_equal expected.moderator pending.moderator);
         assert (Option.is_some pending.moderator);
         assert (G.equal (Goal.goal expected) (Goal.goal pending));
         assert (
           A.Goal_accounting.equal (Goal.accounting expected) (Goal.accounting pending));
         assert (not (Owner.is_loaded entry.runtime)));
    f entry session (Resume.checkpoint entry))
;;

let request before path =
  let input =
    match path with
    | Path.Installed_start -> P.Run_start.Input.Authored_start
    | Pending_cold | Ready_cold ->
      User_submission
        { kind = Plain_text; text = "control must prevent this input"; attachments = [] }
  in
  let run =
    A.Run_admission_request.Headless.create
      ~session_id:before.A.Session_state.identity.session_id
      ~generation:before.identity.generation
      ~expected_revision:before.counters.revision
      ~mode:Workflow
      ~input
      ~key:(P.Idempotency_key.of_string "goal-control-initial-episode" |> protocol_ok)
    |> protocol_ok
  in
  A.Goal_activation_request.create
    ~goal_id:Goal.goal_id
    ~expected_goal_revision:(G.revision (Goal.goal before))
    ~expected_catalog_revision:(Catalog.revision (Option.value_exn before.goal_catalog))
    ~request:run
  |> protocol_ok
;;

let controlled = function
  | Service.Outcome.Attempted attempt ->
    (match Service.Attempt.result attempt |> protocol_ok with
     | Controlled { admission; control } -> admission, control
     | Admission_uncertain error | Activation_failed { error; _ } ->
       raise_s [%sexp "controlling callback activation failed", (error : P.Error.t)]
     | Activated _ -> failwith "controlling callback unexpectedly activated input"
     | Retained _ -> failwith "controlling callback unexpectedly retained"
     | Settled _ -> failwith "controlling callback unexpectedly settled")
  | Retained _ -> failwith "fresh controlled admission unexpectedly retained"
;;

let assert_control decision final control =
  let receipt = Control.receipt control in
  let goal = Goal.goal final in
  assert (P.Id.Goal.equal Goal.goal_id receipt.goal_id);
  [%test_eq: string] control_key (P.Idempotency_key.to_string receipt.key);
  assert (
    List.exists
      (Catalog.receipts (Option.value_exn final.A.Session_state.goal_catalog))
      ~f:(A.Goal_command_receipt.equal receipt));
  assert (Option.is_none (Control.waiting control));
  match decision with
  | Decision.Hold_progress ->
    assert (Control.Disposition.equal (Held reason) (Control.disposition control));
    assert (G.State.equal (Open (Held (Needs_input reason))) (G.state goal));
    [%test_eq: int64] (Int64.succ receipt.goal_revision) (G.revision goal);
    [%test_eq: string]
      "progress after the exact Hold"
      (G.Progress.to_string (G.progress goal))
  | Abandon ->
    assert (Control.Disposition.equal (Abandoned reason) (Control.disposition control));
    assert (G.State.equal (Closed (Abandoned reason)) (G.state goal));
    [%test_eq: int64] receipt.goal_revision (G.revision goal)
;;

let run path decision =
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr providers;
      failwith "control-completed callback dispatched a provider")
  in
  Creation_tests.with_daemon
    ~prompt_text:(prompt path decision)
    ~qualify_chatml_extensions:true
    ~policy
    (fun env _sw _root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         let with_entry =
           match path with
           | Path.Pending_cold -> with_pending_entry daemon
           | Installed_start ->
             fun f ->
               with_initial_entry daemon (fun entry ->
                 let session, created = Goal.create_goal daemon entry in
                 f entry session created)
           | Ready_cold ->
             fun f ->
               Source.with_loaded_entry daemon (fun ~entry ~runtime:_ ~revision:_ ->
                 let session, created = Goal.create_goal daemon entry in
                 f entry session created)
         in
         with_entry (fun entry session created ->
           (match path with
            | Pending_cold | Installed_start -> ()
            | Ready_cold -> Owner.unload_and_wait entry.runtime |> protocol_ok);
           let before = Resume.checkpoint entry in
           assert (not (Owner.is_loaded entry.runtime));
           let old_callbacks = before.moderator_executions in
           let requested = request before path in
           let public = Resume.public daemon in
           let call () =
             Service.start_episode
               public
               ~entry
               ~actor:original
               ~session
               ~request:requested
           in
           let outcome = call () |> protocol_ok in
           (match outcome with
            | Service.Outcome.Attempted attempt ->
              (match Service.Attempt.result attempt with
               | Error _ | Ok (Activation_failed _ | Admission_uncertain _) ->
                 let failed = Resume.checkpoint entry in
                 print_s
                   [%sexp
                     "failed initial control invocations"
                   , (path : Path.t)
                   , (decision : Decision.t)
                   , (failed.invocations : P.Invocation.t list)
                   , (failed.moderator_executions : P.Moderator_execution.t list)]
               | Ok (Activated _ | Controlled _ | Settled _ | Retained _) -> ())
            | Retained _ -> ());
           let admission, control = controlled outcome in
           let final = Resume.checkpoint entry in
           assert_control decision final control;
           assert (not (Owner.is_loaded entry.runtime));
           assert (Option.is_none final.active_operation);
           assert (Option.is_none final.failure);
           assert (not final.halted);
           [%test_eq: int] 0 !providers;
           (match path with
            | Pending_cold | Ready_cold ->
              assert (
                A.Session_state.Runtime_initialization.equal
                  before.runtime_initialization
                  final.runtime_initialization)
            | Installed_start ->
              assert (
                A.Session_state.Runtime_initialization.equal
                  Ready
                  final.runtime_initialization));
           List.iter old_callbacks ~f:(fun old ->
             assert (
               List.exists final.moderator_executions ~f:(P.Moderator_execution.equal old)));
           [%test_eq: int]
             (List.length old_callbacks + 1)
             (List.length final.moderator_executions);
           assert (Resume.callbacks_complete final);
           let new_callback =
             List.find_exn final.moderator_executions ~f:(fun current ->
               not (List.exists old_callbacks ~f:(P.Moderator_execution.equal current)))
           in
           let phase =
             match path with
             | Installed_start -> P.Moderator_execution.Session_start
             | Pending_cold | Ready_cold -> Session_resume
           in
           assert (P.Moderator_execution.equal_phase phase new_callback.context.phase);
           let run = Resume.run final admission in
           assert (List.is_empty (Resume.operation_work run));
           (match run.lifecycle with
            | Admitted | Active | Waiting _ -> ()
            | Terminal _ -> failwith "control cleanup manufactured a terminal Run");
           let account = Goal.accounting final in
           [%test_eq: int64 * int64]
             (0L, 0L)
             ( A.Goal_accounting.admitted_turns account
             , A.Goal_accounting.admitted_attempts account );
           [%test_eq: int] 0 (List.length (A.Goal_accounting.bindings account));
           let frames = Existing.frames env entry in
           (match call () |> protocol_ok with
            | Retained retained -> assert (P.Run_receipt.equal admission retained)
            | Attempted _ -> failwith "controlled initial replay reconstructed a runtime");
           Existing.assert_same final entry;
           [%test_eq: string list] frames (Existing.frames env entry);
           [%test_eq: int] 0 !providers;
           ignore created)))
;;

(* A stopped unbound constructor is not the selected Goal initialization
   owner. Authentication and declared native tools do not confer that custody. *)
let assert_stopped_unbound_goal_denied () =
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr providers;
      failwith "stopped unbound Goal callback dispatched a provider")
  in
  let prompt_text =
    prompt Pending_cold Hold_progress
    |> replace_prompt ~pattern:"| `Session_resume ->" ~with_:"| `Session_start ->"
  in
  Creation_tests.with_daemon
    ~prompt_text
    ~qualify_chatml_extensions:true
    ~policy
    (fun _env _sw _root daemon ->
       (match
          Factory.create_session_authenticated
            (Daemon.factory daemon)
            ~command_audit:None
            ~actor:original
            (create_request ~start_immediately:false ())
        with
        | Error error ->
          [%test_eq: P.Error.code] Invalid_state error.code;
          [%test_eq: string] "permission_denied" error.message;
          assert (not error.retryable)
        | Ok entry ->
          entry.close ();
          failwith "stopped unbound Session_start acquired native Goal authority");
       [%test_eq: int] 0 !providers)
;;

let%expect_test
    "real retained-history legacy Pending and Ready Init control joins without handoff"
  =
  assert_stopped_unbound_goal_denied ();
  List.iter [ Decision.Hold_progress; Abandon ] ~f:(run Path.Pending_cold);
  run Ready_cold Hold_progress;
  print_endline
    "stopped unbound native Goal stays denied; Pending cold Session_resume \
     Hold->Progress and Abandon retain exact control; real Ready cold Session_resume \
     keeps prior Ready; no handoff/input/provider/refund";
  [%expect
    {| stopped unbound native Goal stays denied; Pending cold Session_resume Hold->Progress and Abandon retain exact control; real Ready cold Session_resume keeps prior Ready; no handoff/input/provider/refund |}]
;;

let%expect_test "actual installed v1 Session_start control retires its unadopted resource"
  =
  run Path.Installed_start Decision.Hold_progress;
  print_endline
    "actual installed Session_start joins and retires its unadopted resource; truthful \
     Ready retained without input/provider or fabricated Run terminal";
  [%expect
    {| actual installed Session_start joins and retires its unadopted resource; truthful Ready retained without input/provider or fabricated Run terminal |}]
;;

(* This is untrusted Finish input DATA. Its revision comes from the actual
   callback's Get after Hold; the actor supplies and validates Run association,
   action receipt and terminal evidence. No synthetic proof is constructed. *)
let initial_finish_prompt =
  let finish =
    P.Goal_achievement.Intent.create
      ~expected_goal_revision:0L
      ~evidence:
        (G.Reason.of_string "Actual initial Session_start completed this Goal"
         |> protocol_ok)
      ~action:(Finish { terminal = Completed None; relinquish = [] })
    |> protocol_ok
    |> P.Goal_achievement.Intent.to_json
    |> Jsonaf.to_string
  in
  prompt Installed_start Hold_progress
  |> replace_prompt
       ~pattern:{|let* progressed = call("goal_progress", progress(after_hold)) in|}
       ~with_:
         (sprintf
            {|let template = Json.parse(%S) in
    let intent = `Object([
      {key = "expected_goal_revision"; value = field(row(after_hold), "revision")},
      {key = "evidence"; value = field(template, "evidence")},
      {key = "action"; value = field(template, "action")}
    ]) in
    let* finished = Goal.finish(intent) in|}
            finish)
;;

let%expect_test "initial installed Hold and actual Finish returns terminal settlement" =
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr providers;
      failwith "initial Hold/Finish dispatched a provider")
  in
  Creation_tests.with_daemon
    ~prompt_text:initial_finish_prompt
    ~qualify_chatml_extensions:true
    ~policy
    (fun env _sw _root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         with_initial_entry daemon (fun entry ->
           let session, before = Goal.create_goal daemon entry in
           let requested = request before Installed_start in
           let call () =
             Service.start_episode
               (Resume.public daemon)
               ~entry
               ~actor:original
               ~session
               ~request:requested
           in
           let admission, settlement =
             match call () |> protocol_ok with
             | Service.Outcome.Attempted attempt ->
               (match Service.Attempt.result attempt |> protocol_ok with
                | Settled { admission; settlement } -> admission, settlement
                | Activated _
                | Controlled _
                | Retained _
                | Admission_uncertain _
                | Activation_failed _ ->
                  failwith "initial Finish lost its terminal outcome")
             | Retained _ -> failwith "fresh initial Finish returned retained DATA"
           in
           let final = Resume.checkpoint entry in
           let goal = Goal.goal final in
           let actual = Resume.run final admission in
           let episode = G.episode goal |> Option.value_exn in
           assert (P.Run_receipt.equal admission (P.Goal_episode.admission episode));
           assert (
             P.Goal_episode.equal episode (P.Goal_episode.Settlement.episode settlement));
           P.Goal_episode.validate_terminal
             episode
             ~run:actual
             ~receipt:(P.Goal_episode.Settlement.receipt settlement)
           |> protocol_ok;
           (match actual.lifecycle, G.state goal with
            | Terminal (Completed None), Closed (Achieved achievement) ->
              assert (
                P.Goal_episode.Settlement.equal
                  settlement
                  (P.Goal_achievement.settlement achievement))
            | _ -> failwith "initial Finish lacked actual Completed/Achieved evidence");
           let index = Option.value_exn final.run_state in
           assert (
             List.exists
               (A.Run_state.receipts index)
               ~f:(P.Run_receipt.equal (P.Goal_episode.Settlement.receipt settlement)));
           let held =
             A.Goal_catalog.receipts (Option.value_exn final.goal_catalog)
             |> List.find_exn ~f:(fun receipt ->
               String.equal
                 control_key
                 (P.Idempotency_key.to_string receipt.A.Goal_command_receipt.key))
           in
           assert (P.Id.Goal.equal held.goal_id (G.id goal));
           assert (Int64.(held.goal_revision < G.revision goal));
           assert (not (Owner.is_loaded entry.runtime));
           assert (
             A.Session_state.Runtime_initialization.equal
               Ready
               final.runtime_initialization);
           assert (Resume.callbacks_complete final);
           (match final.moderator_executions with
            | [ callback ] ->
              assert (
                P.Moderator_execution.equal_phase Session_start callback.context.phase)
            | _ -> failwith "initial Finish did not complete its one actual callback");
           assert (Option.is_none final.active_operation);
           assert (List.is_empty (Resume.operation_work actual));
           assert (Option.is_none final.failure);
           [%test_eq: int * int] (0, 0) (!providers, List.length final.jobs);
           let account = Goal.accounting final in
           [%test_eq: int64 * int64]
             (0L, 0L)
             ( A.Goal_accounting.admitted_turns account
             , A.Goal_accounting.admitted_attempts account );
           assert (List.is_empty (A.Goal_accounting.bindings account));
           let frames = Existing.frames env entry in
           (match call () |> protocol_ok with
            | Retained retained -> assert (P.Run_receipt.equal admission retained)
            | Attempted _ -> failwith "initial settled replay reconstructed runtime");
           Existing.assert_same final entry;
           [%test_eq: string list] frames (Existing.frames env entry);
           [%test_eq: int] 0 !providers)));
  print_endline
    "initial actual Hold+Finish returns validated Settlement; truthful Ready and joined \
     unload; no provider/turn/refund";
  [%expect
    {| initial actual Hold+Finish returns validated Settlement; truthful Ready and joined unload; no provider/turn/refund |}]
;;
