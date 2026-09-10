open Core
open Fixtures

let%expect_test "standalone handlers retain owned native calls and canonical outcomes" =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let module Managed = Chat_response.Managed_tool_registry in
  let module C = Chat_response.Tool_capability in
  List.iter
    [ `Success
    ; `Unselected
    ; `Parent_denied
    ; `Child_denied
    ; `Child_revoked
    ; `Invalid_output
    ; `Run_limit
    ; `Custom
    ; `Pre_reject
    ; `Pre_failed
    ; `Invalid_original
    ; `Invalid_rewrite
    ; `Rewrite
    ; `Redirect
    ; `Redirect_unselected
    ; `Oversized_rewrite
    ]
    ~f:(fun mode ->
      let calls = ref 0
      and native_approvals = ref 0
      and pre_calls = ref 0 in
      let custom =
        match mode with
        | `Custom -> true
        | _ -> false
      in
      let initial = native_registry ~custom calls ~raises:false in
      let module Alias = struct
        type input = string

        let name = "alias_file"
        let description = Some "redirect target"
        let type_ = "function"
        let parameters = `Object [ "type", `String "object" ]
        let input_of_string input = input
      end
      in
      let alias =
        Ochat_function.create_function
          (module Alias)
          (fun input ->
             assert (String.equal input "{}");
             Int.incr calls;
             Openai.Responses.Tool_output.Output.Text "alias output")
      in
      let original = List.hd_exn (C.references initial) in
      let binding =
        C.resolve initial ~id:original.id ~fingerprint:original.fingerprint
        |> Result.map_error ~f:(fun error -> error.C.message)
        |> Result.ok_or_failwith
      in
      let registry =
        ref
          (C.create
             ~owner:"fixture"
             ~resource_fingerprint:
               (Chatmd_shell_spec.Source_ref.digest "standalone resources")
             [ ( original.implementation_revision
               , C.native_implementation binding |> Option.value_exn )
             ; Chatmd_shell_spec.Source_ref.digest "alias v1", alias
             ]
           |> Result.map_error ~f:(fun error -> error.C.message)
           |> Result.ok_or_failwith)
      in
      with_handoff_actor
        ~make_worker:(fun env actor_ready ->
          let dir = Eio.Stdenv.cwd env in
          let body =
            match mode with
            | `Run_limit ->
              {|let rec loop x = loop(x)
let never = loop(0)
let run ctx input = Task.pure(`Complete(`String("unreachable")))|}
            | _ ->
              {|let count = [0]
let run ctx input = Task.bind(Tool.call("read_file", |}
              ^ (match mode with
                 | `Custom -> {|`String("{}")|}
                 | `Invalid_original -> "`Null"
                 | `Rewrite -> {|`Object([{key = "before"; value = `String("rewrite")}])|}
                 | _ -> "`Object([])")
              ^ {|), fun result ->
  let ignored = count[0] <- count[0] + 1 in
  match result with
  | `Ok(value) -> |}
              ^ (match mode with
                 | `Invalid_output -> "Task.pure(`Complete(`Null))"
                 | _ ->
                   {|(match value with
    | `String(text) -> Task.pure(`Complete(`String(to_string(count[0]) ++ ":" ++ text)))
    | _ -> Task.fail("expected disclosed string"))|})
              ^ {|
  | `Error(code) -> Task.pure(`Fail({code = code; message = "native failed";
                                   retryable = false; details = `Null})))|}
          in
          let uses =
            match mode with
            | `Unselected -> ""
            | `Redirect -> {|<uses tool="read_file"/><uses tool="alias_file"/>|}
            | _ -> {|<uses tool="read_file"/>|}
          in
          let source =
            {|<script id="standalone" language="chatml" kind="tool" max_value="256KiB">|}
            ^ body
            ^ {|</script><tool name="summary" type="chatml" script="standalone"
entrypoint="run" input_schema="input.json" output_schema="output.json">|}
            ^ uses
            ^ "</tool>"
          in
          let loader =
            Source_loader.captured_filesystem
              ~root:dir
              ~sources:
                [ "input.json", {|{"type":"object"}|}
                ; "output.json", {|{"type":"string"}|}
                ]
          in
          let elements =
            Prompt.Chat_markdown.parse_chat_inputs ~dir ~source_loader:loader source
          in
          let managed =
            Managed.prepare ~env ~owner:"fixture" ~capabilities:!registry elements
            |> Result.map_error ~f:(fun errors ->
              String.concat
                ~sep:"; "
                (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
            |> Result.ok_or_failwith
          in
          registry := Managed.capabilities managed;
          let definition = Managed.definition managed in
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let script_tools =
              Agent_session.Script_tool_calls.create
                ~registry:(fun () -> !registry)
                ~moderator_names:String.Set.empty
                ~now:Agent_protocol.Timestamp.now
                ~is_halted:(fun () -> (A.state actor |> protocol_ok).halted)
                ~requires_active_moderator:(fun _ -> false)
                ~authorize:(fun child _ ->
                  assert (I.equal_origin child.context.origin Script);
                  assert (Option.is_some child.context.parent_invocation);
                  Int.incr native_approvals;
                  Eio.Fiber.yield ();
                  match mode with
                  | `Child_denied -> Error (handoff_error "private denial")
                  | `Child_revoked ->
                    registry
                    := C.select !registry ~names:[]
                       |> Result.map_error ~f:(fun e -> e.C.message)
                       |> Result.ok_or_failwith;
                    Ok ()
                  | _ -> Ok ())
                ~prepare_output:(fun _ -> Ok (`String "disclosed"))
                ~defer_observation:(fun _ -> failwith "no moderator was installed")
            in
            let dispatch =
              Agent_session.Standalone_tool_dispatch.create
                ~env
                ~definition
                ~input
                ~capabilities:caps
                ~script_tools
                ~now:Agent_protocol.Timestamp.now
                ~is_halted:(fun () -> (A.state actor |> protocol_ok).halted)
                ~execution_limits:(fun _ ->
                  { Chatml_execution.default_limits with fuel = 1000 })
                ~admit:(fun _ ->
                  match mode with
                  | `Parent_denied -> Error "private admission diagnostic"
                  | _ -> Ok ())
                ~revalidate:(fun _ ->
                  Agent_session.Script_tool_calls.validate_definition
                    script_tools
                    definition)
                ~moderate_tool:(fun _ call ->
                  Int.incr pre_calls;
                  assert (String.equal call.name "read_file");
                  (match mode with
                   | `Custom -> assert (String.equal call.payload_text "{}")
                   | _ -> ());
                  let decision
                    : (Chat_response.Moderation.Tool_moderation.t option, string) result
                    =
                    match mode with
                    | `Pre_reject -> Ok (Some (Reject "private rejection"))
                    | `Pre_failed -> Error "private pre diagnostic"
                    | `Invalid_rewrite -> Ok (Some (Rewrite_args `Null))
                    | `Rewrite -> Ok (Some (Rewrite_args (`Object [])))
                    | `Redirect | `Redirect_unselected ->
                      Ok (Some (Redirect ("alias_file", `Object [])))
                    | `Oversized_rewrite ->
                      Ok
                        (Some
                           (Rewrite_args
                              (`Object [ "huge", `String (String.make (300 * 1024) 'x') ])))
                    | _ -> Ok None
                  in
                  Result.map
                    decision
                    ~f:
                      (Option.map ~f:(fun action ->
                         { Chat_response.Moderation.Outcome.empty with
                           tool_moderation = Some action
                         })))
                ~prepare_outcome:(fun outcome ->
                  I.validate_outcome outcome
                  |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message))
                ()
            in
            let call_once call_id =
              let id =
                History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
              in
              let call =
                History_entry.create_with_id
                  ~id
                  (Chat_response.Tool_call.call_item
                     ~kind:Function
                     ~name:"summary"
                     ~payload:"{}"
                     ~call_id
                     ~id:None)
              in
              let request =
                Chat_response.In_memory_stream.Tool_dispatch.
                  { kind = Function
                  ; original_name = "summary"
                  ; original_payload = "{}"
                  ; name = "summary"
                  ; payload = "{}"
                  ; rejection = None
                  ; call
                  ; history = input.history @ [ call ]
                  ; source = None
                  ; parent_call_id = None
                  }
              in
              dispatch.validate_original ~kind:Function ~name:"summary" ~payload:"{}"
              |> Result.ok_or_failwith;
              assert (dispatch.commit_call request);
              let result =
                dispatch.run request ~authorize:(fun () -> ()) |> Option.value_exn
              in
              let id =
                History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
              in
              let output =
                History_entry.create_with_id
                  ~id
                  (Chat_response.Tool_call.output_item
                     ~kind:Function
                     ~call_id
                     ~output:result.output)
              in
              (Option.value_exn result.commit_output) output
            in
            (match mode with
             | `Success ->
               Eio.Fiber.both (fun () -> call_once "first") (fun () -> call_once "second")
             | _ -> call_once "only");
            let state = A.state actor |> protocol_ok in
            Completed
              { final_history =
                  Agent_session.History_codec.all_of_protocol
                    state.conversation.canonical_history
                  |> protocol_ok
              ; moderator_snapshot = None
              ; runtime_requests = []
              }))
        (fun _env actor _writer backend ->
           let state = await_idle actor in
           assert (Option.is_none state.failure);
           let parents, children =
             List.partition_tf state.invocations ~f:(fun invocation ->
               Option.is_none invocation.context.parent_invocation)
           in
           let outcomes =
             List.map parents ~f:(fun parent ->
               assert (Option.is_some parent.output_entry_id);
               match parent.status with
               | Published (Complete (`String value)) -> value
               | Published (Fail error) -> error.code
               | _ -> raise_s [%sexp "unexpected standalone result", (parent : I.t)])
             |> List.sort ~compare:String.compare
           in
           List.iter children ~f:(fun child ->
             assert (I.equal_origin child.context.origin Script);
             assert (Option.is_none child.context.provider_call_id);
             assert (Option.is_none child.context.call_entry_id);
             assert (Option.is_none child.output_entry_id);
             assert (Option.is_none child.observation);
             let routing = Option.value_exn child.routing in
             assert (String.equal routing.original_name "read_file");
             assert (Option.is_none routing.canonical_payload);
             match mode with
             | `Custom ->
               [%test_eq: string]
                 (Chatmd_shell_spec.Source_ref.digest "{}")
                 routing.original_payload.sha256;
               [%test_eq: int] 2 routing.final_payload.byte_length
             | `Rewrite ->
               assert (
                 not
                   (I.equal_payload_fingerprint
                      routing.original_payload
                      routing.final_payload))
             | `Redirect -> [%test_eq: string] "alias_file" child.context.tool_name
             | _ -> ());
           let saved = Agent_session.Memory_backend.state backend in
           assert (List.equal I.equal state.invocations saved.invocations);
           print_s
             [%sexp
               { mode : [ `Success
                        | `Unselected
                        | `Parent_denied
                        | `Child_denied
                        | `Child_revoked
                        | `Invalid_output
                        | `Run_limit
                        | `Custom
                        | `Pre_reject
                        | `Pre_failed
                        | `Invalid_original
                        | `Invalid_rewrite
                        | `Rewrite
                        | `Redirect
                        | `Redirect_unselected
                        | `Oversized_rewrite
                        ]
               ; outcomes : string list
               ; native_calls = (!calls : int)
               ; native_approvals = (!native_approvals : int)
               ; pre_calls = (!pre_calls : int)
               ; children = (List.length children : int)
               }]));
  [%expect
    {|
    ((mode Success) (outcomes (1:disclosed 1:disclosed)) (native_calls 2)
     (native_approvals 2) (pre_calls 2) (children 2))
    ((mode Unselected) (outcomes (invocation.unselected_tool)) (native_calls 0)
     (native_approvals 0) (pre_calls 0) (children 0))
    ((mode Parent_denied) (outcomes (invocation.permission_denied))
     (native_calls 0) (native_approvals 0) (pre_calls 0) (children 0))
    ((mode Child_denied) (outcomes (invocation.permission_denied))
     (native_calls 0) (native_approvals 1) (pre_calls 1) (children 1))
    ((mode Child_revoked) (outcomes (invocation.stale_binding)) (native_calls 0)
     (native_approvals 1) (pre_calls 1) (children 1))
    ((mode Invalid_output) (outcomes (invocation.invalid_output))
     (native_calls 1) (native_approvals 1) (pre_calls 1) (children 1))
    ((mode Run_limit) (outcomes (chatml.execution_limit)) (native_calls 0)
     (native_approvals 0) (pre_calls 0) (children 0))
    ((mode Custom) (outcomes (1:disclosed)) (native_calls 1) (native_approvals 1)
     (pre_calls 1) (children 1))
    ((mode Pre_reject) (outcomes (invocation.pre_tool_rejected)) (native_calls 0)
     (native_approvals 0) (pre_calls 1) (children 1))
    ((mode Pre_failed) (outcomes (invocation.pre_tool_failed)) (native_calls 0)
     (native_approvals 0) (pre_calls 1) (children 1))
    ((mode Invalid_original) (outcomes (invocation.invalid_input))
     (native_calls 0) (native_approvals 0) (pre_calls 0) (children 1))
    ((mode Invalid_rewrite) (outcomes (invocation.invalid_input))
     (native_calls 0) (native_approvals 0) (pre_calls 1) (children 1))
    ((mode Rewrite) (outcomes (1:disclosed)) (native_calls 1)
     (native_approvals 1) (pre_calls 1) (children 1))
    ((mode Redirect) (outcomes (1:disclosed)) (native_calls 1)
     (native_approvals 1) (pre_calls 1) (children 1))
    ((mode Redirect_unselected) (outcomes (invocation.unselected_tool))
     (native_calls 0) (native_approvals 0) (pre_calls 1) (children 1))
    ((mode Oversized_rewrite) (outcomes (invocation.invalid_input))
     (native_calls 0) (native_approvals 0) (pre_calls 1) (children 0))
    |}]
;;
