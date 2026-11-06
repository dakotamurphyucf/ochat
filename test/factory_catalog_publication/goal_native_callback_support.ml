open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Staging = Goal_resume_tests
module G = P.Goal
module N = A.Native_tool_invocation
module Admission = A.Goal_native_admission
module C = Chat_response.Tool_capability
module M = Chat_response.Moderator_manager

(* The selected native implementation is trusted test-host wiring, exercised
   through the actual Manager Tool.call and Actor event/native owner. It neither
   replaces the Factory runtime nor supplies a native admission token. *)
let prompt ~v1 =
  sprintf
    {|<developer>Actual native Goal callback handoff fixture.</developer>
<script id="native_goal" language="chatml" kind="moderator"%s>
let initial_state = 0
let field value key = match Json.get_field(value, key) with | `Some(value) -> value | _ -> `Null
let on_event ctx state event = match ctx.session_meta with
| `String(encoded) ->
  let envelope = Json.parse(encoded) in
  let mode = match field(envelope, "mode") with | `String(value) -> value | _ -> "" in
  let name = match mode with | "status" -> "goal_get" | "progress" -> "goal_progress" | "progress-rollback" -> "goal_progress" | "progress-finish" -> "goal_progress" | "hold-wrong-effect" -> "goal_progress" | "hold-wrong-effect-catch" -> "goal_progress" | "create" -> "goal_create" | "create-rollback" -> "goal_create" | "hold" -> "goal_hold" | "hold-denied-catch" -> "goal_hold" | "hold-progress" -> "goal_hold" | "hold-turn" -> "goal_hold" | "hold-continue" -> "goal_hold" | "abandon" -> "goal_abandon" | _ -> "goal_resume" in
  let rollback = match mode with | "progress-rollback" -> true | "resume-rollback" -> true | "resume-catch" -> true | "create-rollback" -> true | _ -> false in
  let action = Task.bind(Tool.call(name, `Object([])), fun result ->
    match result with
    | `Error(message) -> Task.fail(message)
    | `Ok(_) ->
      if mode == "progress-finish" then %s
      else if mode == "hold-progress" then Task.bind(Tool.call("goal_progress", `Object([])), fun result -> match result with | `Ok(_) -> Task.pure(state + 1) | `Error(message) -> Task.fail(message))
      else if mode == "hold-turn" then Task.map(Runtime.request_turn(), fun ignored -> state + 1)
      else if mode == "hold-continue" then %s
      else if rollback then Task.fail("intentional parent rollback")
      else if mode == "multiple" then
        Task.bind(Tool.call(name, `Object([])), fun ignored -> Task.pure(state + 1))
      else Task.pure(state + 1)) in
  let caught = match mode with
  | "resume-catch" -> true
  | "hold-denied-catch" -> true
  | "hold-wrong-effect-catch" -> true
  | _ -> false in
  if caught then Task.catch(action, fun ignored -> Task.pure(state + 1)) else action
| _ -> Task.pure(state)
</script>|}
    (if v1 then " api=\"extensibility-v1\"" else "")
    (if v1
     then "Task.map(Goal.finish(field(envelope, \"finish\")), fun ignored -> state + 1)"
     else "Task.fail(\"finish fixture requires v1\")")
    (if v1
     then "Task.map(Run.continue(), fun ignored -> state + 1)"
     else "Task.fail(\"Continue fixture requires v1\")")
;;

let effect_name mode =
  if String.equal mode "status"
  then Admission.Effect.Get
  else if
    (String.equal mode "hold-wrong-effect" || String.equal mode "hold-wrong-effect-catch")
    || String.is_prefix mode ~prefix:"progress"
  then Admission.Effect.Progress
  else if String.is_prefix mode ~prefix:"create"
  then Create
  else if String.is_prefix mode ~prefix:"hold"
  then Hold
  else if String.equal mode "abandon"
  then Abandon
  else Resume
;;

let registry_many actor ~operations ~retained =
  let bindings =
    List.map operations ~f:(fun (operation_kind, run) ->
      let name = Admission.Effect.name operation_kind in
      let module Definition = struct
        type input = string

        let name = name
        let type_ = "function"
        let description = Some "Actual native Goal acceptance fixture"
        let parameters = `Object [ "type", `String "object"; "properties", `Object [] ]
        let input_of_string value = value
      end
      in
      let implementation =
        Ochat_function.create_function
          (module Definition)
          (fun _ ->
             let execute () =
               let open Result.Let_syntax in
               let%bind borrowed = N.borrow () in
               let%bind selected = N.borrowed_capabilities borrowed in
               let%bind binding =
                 C.find selected ~name
                 |> Result.map_error ~f:(fun error ->
                   P.Error.invalid_request error.C.message)
               in
               let%bind token = N.Admission.capture borrowed ~binding in
               let%bind admission =
                 Admission.create token ~binding ~effect_kind:operation_kind
               in
               retained := admission :: !retained;
               let%bind () =
                 Admission.check_now admission (state actor) ~effect_kind:operation_kind
               in
               run admission
             in
             let outcome =
               match execute () with
               | Ok () -> P.Invocation.Complete `Null
               | Error error ->
                 Fail
                   { code = P.Error.code_to_string error.code
                   ; message = error.message
                   ; retryable = error.retryable
                   ; details = error.data
                   }
             in
             Openai.Responses.Tool_output.Output.Text
               (Jsonaf.to_string (P.Invocation.outcome_to_json outcome)))
      in
      ( Chatmd_shell_spec.Source_ref.digest ("native-goal-acceptance:" ^ name)
      , implementation ))
  in
  C.create
    ~owner:"native-goal-acceptance"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "native-goal-acceptance")
    ~result_contracts:
      (List.map operations ~f:(fun (kind, _) ->
         Admission.Effect.name kind, C.Invocation_v1))
    bindings
  |> Result.map_error ~f:(fun error -> error.C.message)
  |> Result.ok_or_failwith
;;

let registry actor ~operation_kind ~run ~retained =
  registry_many actor ~operations:[ operation_kind, run ] ~retained
;;

let callback_in_runtime
      ?(additional = [])
      ~native_failure
      ~finish_intent
      entry
      runtime
      ~operation
      ~mode
      ~run
  =
  let manager = Option.value_exn runtime.A.Runtime_builder.moderator_manager in
  let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
  let retained = ref []
  and invocations = ref [] in
  let operation_kind = effect_name mode in
  let selected =
    registry_many entry ~operations:((operation_kind, run) :: additional) ~retained
  in
  let metadata =
    `String
      (Jsonaf.to_string
         (`Object
             [ "mode", `String mode; "finish", Option.value finish_intent ~default:`Null ]))
  in
  let current = state entry in
  let session_id = P.Id.Session.to_string current.identity.session_id in
  let history =
    A.History_codec.all_of_protocol current.conversation.canonical_history |> protocol_ok
  in
  let tool_call ~executing ~execute ~name ~args =
    let reference =
      C.find selected ~name
      |> Result.map_error ~f:(fun error -> error.C.message)
      |> Result.ok_or_failwith
      |> C.reference
    in
    let invocation =
      P.Invocation.create
        ~parent_event:executing.P.Moderator_execution.context.id
        ~observer:executing.P.Moderator_execution.context.source
        { id = P.Id.Invocation.create ()
        ; session_id = executing.context.session_id
        ; generation = executing.context.generation
        ; origin = Moderator
        ; provider_call_id = None
        ; call_entry_id = None
        ; parent_invocation = None
        ; parent_job = None
        ; tool_name = name
        ; implementation_revision = reference.implementation_revision
        ; capability_fingerprint = C.fingerprint selected
        ; input = args
        ; created_at = runtime.now ()
        ; deadline = None
        }
      |> protocol_ok
    in
    match
      N.run_scoped
        ~execute
        ~registry:(fun () -> selected)
        ~reference
        ~invocation
        ~is_halted:(fun () -> false)
        ~authorize:(fun _ _ -> Ok ())
        ~prepare_output:(function
          | Text text -> Ok (`String text)
          | _ -> Error (P.Error.invalid_request "fixture expected text output"))
    with
    | Ok ({ status = Resolved (Complete value); _ } as invocation) ->
      invocations := invocation :: !invocations;
      Ok (Chat_response.Moderation.Capabilities.Tool_ok value)
    | Ok invocation ->
      invocations := invocation :: !invocations;
      Error (Sexp.to_string_hum ([%sexp_of: P.Invocation.status] invocation.status))
    | Error error ->
      native_failure := Some error;
      Error error.message
  in
  let prepared
        ~run_actions
        ~commit
        ~(outcome : Chat_response.Moderation.Outcome.t)
        ~snapshot
    =
    let requests : P.Invocation.follow_up =
      { request_turn =
          Chat_response.Runtime_semantics.request_turn outcome.runtime_requests
      ; request_compaction =
          Chat_response.Runtime_semantics.request_compaction outcome.runtime_requests
      ; end_session =
          Chat_response.Runtime_semantics.should_end_session outcome.runtime_requests
      }
    in
    let open Result.Let_syntax in
    let%bind requests =
      match run_actions, outcome.run_action with
      | None, None -> Ok requests
      | None, Some _ -> Error "run action has no callback owner"
      | Some owner, action ->
        A.Run_action_service.compose_requests owner ~action ~requests
    in
    Ok
      { M.persist =
          (fun () ->
            commit ~snapshot ~requests ~notifications:outcome.ui_notifications
            |> Result.map_error ~f:(fun error -> error.P.Error.message))
      ; install = ignore
      }
  in
  let result =
    match M.extension_definition manager with
    | Some _ ->
      let event = Chat_response.Moderation.Event.Turn_end in
      Actor.with_ordinary_moderator_event
        entry.Registry.actor
        ~operation_id:(Some operation.P.Operation.id)
        ~snapshot
        ~event
        (fun
            ~executing
             ~model_jobs:_
             ~allocate_inference:_
             ~retirement_reason:_
             ~event:_
             ~execute
             ~commit
           ->
           let run_actions = Actor.run_actions entry.actor executing |> protocol_ok in
           M.handle_event_entries_transactional
             ?run_actions:(Option.map run_actions ~f:A.Run_action_service.transaction)
             manager
             ~session_id
             ~now_ms:0
             ~history
             ~available_tools:[]
             ~session_meta:metadata
             ~event
             ~authorize:(fun () -> Ok ())
             ~on_tool_call:(tool_call ~executing ~execute)
             ~prepare_event:
               (prepared ~run_actions ~commit:(fun ~snapshot ~requests ~notifications:_ ->
                  commit ~snapshot ~requests))
           |> Result.map ~f:ignore
           |> Result.map_error ~f:P.Error.invalid_request)
    | None ->
      (* This is the shipped legacy Manager entrypoint and actual Actor callback
         owner, not the ordinary v1 event adapter with a legacy-shaped script. *)
      let acknowledged = ref false in
      M.handle_legacy_callback_entries
        manager
        ~selection:(Boundary Turn_end)
        ~session_id
        ~now_ms:0
        ~history
        ~available_tools:[]
        ~session_meta:metadata
        ~with_admission:(fun callback ~snapshot:_ run ->
          let outcome = ref None in
          Actor.with_legacy_callback
            entry.actor
            ~operation_id:(Some operation.id)
            ~callback
            (fun ~scope ~commit ->
               let executing = Actor.Callback_scope.executing scope in
               let binding : M.callback_binding =
                 { services =
                     { model_recipes = String.Map.empty
                     ; tool_call =
                         tool_call
                           ~executing
                           ~execute:(Actor.callback_tool_executor entry.actor ~scope)
                     ; after_ms =
                         (fun ~delay_ms:_ ~payload:_ ->
                           Error "fixture did not select Schedule")
                     ; cancel = (fun ~id:_ -> Error "fixture did not select Schedule")
                     }
                 ; authorize =
                     (fun () ->
                       Actor.check_callback_current entry.actor scope
                       |> Result.map_error ~f:(fun error -> error.P.Error.message))
                 ; prepare = prepared ~run_actions:None ~commit
                 }
               in
               run binding
               |> Result.map ~f:(fun value -> outcome := Some value)
               |> Result.map_error ~f:P.Error.invalid_request)
          |> Result.map_error ~f:(fun error -> error.P.Error.message)
          |> Result.bind ~f:(fun committed ->
            acknowledged := committed;
            Result.of_option !outcome ~error:"legacy callback produced no joined outcome"))
      |> Result.map ~f:(fun _ -> !acknowledged)
      |> Result.map_error ~f:P.Error.invalid_request
  in
  List.iter !retained ~f:(fun admission ->
    assert (
      Result.is_error
        (Admission.check_now admission (state entry) ~effect_kind:operation_kind)));
  result, List.rev !invocations
;;

(* The actual Owner lease spans the complete callback and its factual cleanup.
   A captured runtime is compared as DATA, never treated as a resource loan. *)
let callback
      ?(additional = [])
      ?(native_failure = ref None)
      ?finish_intent
      entry
      expected_runtime
      ~operation
      ~mode
      ~run
  =
  let invocations = ref [] in
  let result =
    Owner.with_loaded_runtime entry.Registry.runtime (fun actual_runtime ->
      assert (phys_equal actual_runtime expected_runtime);
      let result, factual =
        callback_in_runtime
          ~additional
          ~native_failure
          ~finish_intent
          entry
          actual_runtime
          ~operation
          ~mode
          ~run
      in
      invocations := factual;
      result)
  in
  result, !invocations
;;
