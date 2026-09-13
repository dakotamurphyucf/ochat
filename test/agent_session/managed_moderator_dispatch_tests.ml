open Core
open Fixtures
module A = Agent_session.Session_actor
module I = Agent_protocol.Invocation
module C = Chat_response.Tool_capability
module Managed = Chat_response.Managed_tool_registry
module M = Chat_response.Moderator_manager
module N = Agent_session.Native_tool_invocation
module Calls = Agent_session.Script_tool_calls
module T = Agent_session.Run_chatml_tool
module Requests = Chat_response.Runtime_request_scope

let capability result =
  result |> Result.map_error ~f:(fun error -> error.C.message) |> Result.ok_or_failwith
;;

let diagnostics result =
  result
  |> Result.map_error ~f:(fun errors ->
    String.concat ~sep:"; " (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
  |> Result.ok_or_failwith
;;

let%expect_test
    "managed moderator calls commit state and requests only with their disclosed result"
  =
  List.iter
    [ `Success
    ; `Standalone
    ; `Moderator_standalone
    ; `Denied
    ; `Revoked
    ; `Handler_fail
    ; `Save_fail
    ; `Disclosure
    ; `Reentrant
    ; `Domain_reentrant
    ; `Domain_budget
    ; `Cancel
    ]
    ~f:(fun mode ->
      let finished, finished_u = Eio.Promise.create () in
      let entered, entered_u = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let current_count = ref (fun () -> -1) in
      let native_calls = ref 0 in
      let summary = ref `Null in
      let requests = ref [] in
      let count = ref (-1) in
      let reject_once = ref true in
      let reject transition =
        match mode with
        | `Save_fail
          when !reject_once
               && List.exists
                    transition.Agent_session.Session_transition.state.invocations
                    ~f:(fun invocation ->
                      String.equal invocation.I.context.tool_name "review"
                      &&
                      match invocation.status with
                      | Resolved (Complete _) -> true
                      | _ -> false) ->
          reject_once := false;
          true
        | _ -> false
      in
      with_handoff_actor
        ~reject
        ~make_worker:(fun env actor_ready ->
          let services = ref None in
          let probe_override = ref None in
          let registration =
            T.registration
              ~env
              ~policy:Chat_response.One_off_request.default_policy
              ~services:(fun () -> Result.of_option !services ~error:"not installed")
          in
          let reader =
            native_registry native_calls ~raises:false ~on_call:(fun () ->
              match mode with
              | `Cancel ->
                Eio.Promise.resolve entered_u ();
                Eio.Promise.await never
              | _ -> ())
            |> fun registry ->
            C.find registry ~name:"read_file"
            |> capability
            |> C.native_implementation
            |> Option.value_exn
          in
          let module Probe = struct
            type input = string

            let name = "probe"
            let type_ = "function"
            let description = None
            let parameters = `True
            let input_of_string x = x
          end
          in
          let probe =
            Ochat_function.create_function
              (module Probe)
              (fun _ ->
                 incr native_calls;
                 let scope = N.borrow () |> protocol_ok in
                 match !probe_override with
                 | Some run -> run scope
                 | None ->
                   let parent = N.borrowed_invocation scope in
                   let invoke = N.moderator_executor scope |> Option.value_exn in
                   let child =
                     I.create
                       { parent.context with
                         id = Agent_protocol.Id.Invocation.create ()
                       ; origin = Script
                       ; parent_invocation = Some parent.context.id
                       ; provider_call_id = None
                       ; call_entry_id = None
                       }
                     |> protocol_ok
                   in
                   let rejected =
                     Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
                       Eio_main.run (fun domain_env ->
                         Eio.Time.Timeout.run_exn
                           (Eio.Time.Timeout.seconds
                              (Eio.Stdenv.mono_clock domain_env)
                              1.)
                           (fun () ->
                              match
                                invoke ~invocation:child (fun ~dispatched:_ ~commit:_ ->
                                  failwith "reentrant callback ran")
                              with
                              | Error error ->
                                String.is_prefix
                                  error.Agent_protocol.Error.message
                                  ~prefix:"moderator_reentrancy:"
                              | Ok () -> false)))
                   in
                   assert rejected;
                   Openai.Responses.Tool_output.Output.Text "domain reentrancy rejected")
          in
          let base =
            C.create
              ~owner:"managed-moderator"
              ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "owned fixtures")
              ~result_contracts:[ T.name, registration.result_contract ]
              [ registration.implementation_revision, registration.implementation
              ; Chatmd_shell_spec.Source_ref.digest "reader", reader
              ; Chatmd_shell_spec.Source_ref.digest "probe", probe
              ]
            |> capability
          in
          let inner =
            {|let main input = Task.bind(Tool.call("review", input), fun result -> match result with
| `Error(code) -> Task.pure(`String(code)) | `Ok(_) -> Task.fail("reentered moderator"))|}
          in
          let call =
            match mode with
            | `Reentrant ->
              {|Tool.call("run_chatml", `Object([{key = "source"; value = `String(|}
              ^ Jsonaf.to_string (`String inner)
              ^ {|)}, {key = "input"; value = `Object([])}, {key = "tools"; value = `Array([`String("review")])}]))|}
            | `Domain_reentrant -> {|Tool.call("probe", `Null)|}
            | `Domain_budget -> {|Task.pure(`Ok(`String("budget")))|}
            | `Moderator_standalone -> {|Tool.call("read-wrapper", `Object([]))|}
            | _ -> {|Tool.call("read_file", `Object([]))|}
          in
          let finish =
            match mode with
            | `Handler_fail -> {|Task.fail("after effect")|}
            | `Domain_budget ->
              {|let rec spin n = if n <= 0 then 0 else spin(n - 1) in
let ignored = spin(10000) in
Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(value)), fun ignored -> Task.pure(state))|}
            | _ ->
              {|Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(value)), fun ignored -> Task.pure(state))|}
          in
          let source =
            {|<tool name="run_chatml"/><tool name="read_file"/><tool name="probe"/>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = [0]
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let ignored = state[0] <- state[0] + 1 in
  Task.bind(|}
            ^ call
            ^ {|, fun result -> match result with
  | `Error(code) -> Task.fail(code)
  | `Ok(value) -> Task.bind(Runtime.request_compaction(), fun ignored -> |}
            ^ finish
            ^ {|))
| _ -> Task.pure(state)
</script>
<tool name="review" type="moderator" moderator="owner" input_schema="input.json" output_schema="output.json"/>|}
          in
          let dir = Eio.Stdenv.cwd env in
          let source =
            match mode with
            | `Moderator_standalone ->
              source
              ^ {|
<script id="read-wrapper" language="chatml" kind="tool">
let run ctx input = Task.bind(Tool.call("read_file", input), fun result -> match result with
| `Ok(value) -> Task.pure(`Complete(value)) | `Error(code) -> Task.fail(code))
</script>
<tool name="read-wrapper" type="chatml" script="read-wrapper" entrypoint="run" input_schema="input.json" output_schema="output.json"><uses tool="read_file"/></tool>|}
            | `Standalone ->
              source
              ^ {|
<script id="wrapper" language="chatml" kind="tool">
let run ctx input = Task.bind(Tool.call("review", input), fun result -> match result with
| `Ok(value) -> Task.pure(`Complete(value))
| `Error(code) -> Task.pure(`Fail({code = code; message = "review failed"; retryable = false; details = `Null})))
</script>
<tool name="wrapper" type="chatml" script="wrapper" entrypoint="run" input_schema="input.json" output_schema="output.json"><uses tool="review"/></tool>|}
            | _ -> source
          in
          let loader =
            Source_loader.captured_filesystem
              ~root:dir
              ~sources:[ "input.json", "true"; "output.json", {|{"type":"string"}|} ]
          in
          let elements =
            Prompt.Chat_markdown.parse_chat_inputs ~dir ~source_loader:loader source
          in
          let managed =
            Managed.prepare ~env ~owner:"managed-moderator" ~capabilities:base elements
            |> diagnostics
          in
          let registry = ref (Managed.capabilities managed) in
          let _, artifact =
            M.Registry.of_definition M.Registry.empty (Managed.definition managed)
            |> Result.ok_or_failwith
          in
          let allocator =
            History_entry.Allocator.create ~namespace:"managed-moderator" ~next_sequence:0
            |> Result.ok_or_failwith
          in
          let manager =
            M.create_entries
              ~env
              ~artifact:(Option.value_exn artifact)
              ~capabilities:Chat_response.Moderation.Capabilities.default
              ~allocator
              ()
            |> Result.ok_or_failwith
          in
          (current_count
           := fun () ->
                match
                  (M.identity_snapshot manager |> Result.ok_or_failwith).current_state
                with
                | Array [ Int n ] -> n
                | _ -> assert false);
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let script_tools =
              Calls.create
                ~registry:(fun () -> !registry)
                ~moderator_names:String.Set.empty
                ~now:Agent_protocol.Timestamp.now
                ~is_halted:(fun () -> false)
                ~requires_active_moderator:(fun _ -> false)
                ~authorize:(fun invocation _ ->
                  match invocation.I.context.tool_name, mode with
                  | "review", `Denied ->
                    Error (Agent_protocol.Error.invalid_request "denied")
                  | "review", `Revoked ->
                    registry
                    := C.select !registry ~names:[ "run_chatml"; "review"; "probe" ]
                       |> capability;
                    Ok ()
                  | _ -> Ok ())
                ~prepare_output:(function
                  | Text text ->
                    (match mode with
                     | `Disclosure
                       when String.is_prefix text ~prefix:{|{"type":"complete"|}
                            && String.is_substring text ~substring:"private output" ->
                       Ok
                         (`String (Jsonaf.to_string (I.outcome_to_json (Complete `Null))))
                     | _ -> Ok (`String text))
                  | _ -> assert false)
                ~defer_observation:(fun _ -> Ok ())
              |> fun tools ->
              Calls.with_managed_tools
                tools
                ~env
                ~definition:managed
                ~execution_limits:
                  Agent_session.Standalone_tool_dispatch.declared_execution_limits
              |> fun tools ->
              Calls.with_moderator_dispatch
                tools
                ~dispatch:
                  (Agent_session.Managed_moderator_dispatch.create
                     ~definition:managed
                     ~manager
                     ~history:(fun () -> input.history)
                     ~available_tools:[]
                     ~session_meta:`Null
                     ~now:Agent_protocol.Timestamp.now)
            in
            (match mode with
             | `Domain_budget ->
               probe_override
               := Some
                    (fun borrowed ->
                      Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
                        Eio_main.run (fun _ ->
                          let selected =
                            N.borrowed_capabilities borrowed |> protocol_ok
                          in
                          let reference =
                            C.find selected ~name:"review" |> capability |> C.reference
                          in
                          let parent = N.borrowed_invocation borrowed in
                          let invocation =
                            I.create
                              { parent.context with
                                id = Agent_protocol.Id.Invocation.create ()
                              ; parent_invocation = Some parent.context.id
                              ; origin = Script
                              ; tool_name = reference.name
                              ; implementation_revision =
                                  reference.implementation_revision
                              ; input = `Object []
                              ; provider_call_id = None
                              ; call_entry_id = None
                              }
                            |> protocol_ok
                          in
                          let resolved =
                            Agent_session.Managed_moderator_dispatch.create
                              ~definition:managed
                              ~manager
                              ~history:(fun () -> input.history)
                              ~available_tools:[]
                              ~session_meta:`Null
                              ~now:Agent_protocol.Timestamp.now
                              script_tools
                              ~execute:(N.moderator_executor borrowed |> Option.value_exn)
                              ~native_execute:(N.execute_borrowed borrowed)
                              ~selected
                              ~reference
                              ~invocation
                              ~prepare_output:(function
                                | Text text -> Ok (`String text)
                                | _ -> assert false)
                            |> protocol_ok
                          in
                          assert (
                            match resolved.status with
                            | Resolved (Fail _) -> true
                            | _ -> false);
                          Openai.Responses.Tool_output.Output.Text
                            "budget stopped handler")))
             | _ -> ());
            services
            := Some
                 T.
                   { script_tools
                   ; observer = None
                   ; now = Agent_protocol.Timestamp.now
                   ; moderate_tool = (fun _ _ -> Ok None)
                   ; prepare_outcome =
                       (fun outcome ->
                         I.validate_outcome outcome
                         |> Result.map_error ~f:(fun error ->
                           error.Agent_protocol.Error.message))
                   };
            let request =
              `Object
                [ ( "source"
                  , `String
                      {|let main input = Task.bind(Tool.call("review", input), fun result -> match result with
| `Ok(value) -> Task.pure(value) | `Error(code) -> Task.pure(`String(code)))|}
                  )
                ; "input", `Object []
                ; "tools", `Array [ `String "review" ]
                ]
            in
            let request =
              match mode with
              | `Standalone ->
                `Object
                  [ ( "source"
                    , `String
                        {|let main input = Task.bind(Tool.call("wrapper", input), fun result -> match result with
| `Ok(value) -> Task.pure(value) | `Error(code) -> Task.pure(`String(code)))|}
                    )
                  ; "input", `Object []
                  ; "tools", `Array [ `String "wrapper" ]
                  ]
              | `Domain_budget ->
                `Object
                  [ ( "source"
                    , `String
                        {|let main input = Task.bind(Tool.call("probe", input), fun result -> match result with
| `Ok(value) -> Task.pure(value) | `Error(code) -> Task.pure(`String(code)))|}
                    )
                  ; "input", `Object []
                  ; "tools", `Array [ `String "probe"; `String "review" ]
                  ; "limits", `Object [ "fuel", `Number "500" ]
                  ]
              | _ -> request
            in
            let reference = C.find !registry ~name:T.name |> capability |> C.reference in
            let invocation =
              I.create
                { (invocation_fixture ()).context with
                  id = Agent_protocol.Id.Invocation.create ()
                ; tool_name = T.name
                ; input = request
                ; implementation_revision = reference.implementation_revision
                ; capability_fingerprint = C.fingerprint !registry
                }
              |> protocol_ok
            in
            let resolved, emitted =
              Requests.collect (fun () ->
                N.run
                  ~capabilities:caps
                  ~registry:(fun () -> !registry)
                  ~reference
                  ~invocation
                  ~is_halted:(fun () -> false)
                  ~authorize:(fun _ _ -> Ok ())
                  ~prepare_output:(function
                    | Text text -> Ok (`String text)
                    | _ -> assert false)
                |> protocol_ok)
            in
            requests := emitted;
            (summary
             := match resolved.status with
                | Resolved (Complete value) -> value
                | Resolved (Fail error) -> `String error.code
                | Resolved outcome -> I.outcome_to_json outcome
                | _ -> failwith "not resolved");
            let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
            (count
             := match snapshot.current_state with
                | Array [ Int n ] -> n
                | _ -> assert false);
            let state = A.state actor |> protocol_ok in
            assert (
              Option.equal
                (fun left right ->
                   String.equal (Jsonaf.to_string left) (Jsonaf.to_string right))
                state.moderator
                (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)));
            Eio.Promise.resolve finished_u ();
            Completed
              { final_history = input.history
              ; moderator_snapshot = state.moderator
              ; runtime_requests = []
              }))
        (fun _env actor writer backend ->
           (match mode with
            | `Cancel ->
              Eio.Promise.await entered;
              A.stop actor ~attachment_id:writer.id ~mode:Cancel |> protocol_ok |> ignore
            | _ -> Eio.Promise.await finished);
           let rec settled () =
             let state = A.state actor |> protocol_ok in
             match state.active_operation with
             | None -> state
             | Some _ ->
               Eio.Fiber.yield ();
               settled ()
           in
           let state = settled () in
           (match mode with
            | `Cancel ->
              count := !current_count ();
              assert (Option.is_none state.moderator);
              assert (
                List.for_all state.invocations ~f:(fun invocation ->
                  match invocation.I.status with
                  | Resolved (Cancelled _) -> true
                  | _ -> false));
              summary := `String "cancelled"
            | _ -> ());
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           assert (Int.equal (List.length state.conversation.canonical_history) 1);
           let reviews =
             List.filter state.invocations ~f:(fun invocation ->
               String.equal invocation.I.context.tool_name "review")
           in
           assert (Int.equal (List.length reviews) 1);
           assert (
             List.for_all state.invocations ~f:(fun invocation ->
               Option.is_none invocation.output_entry_id));
           print_s
             [%sexp
               (mode
                : [ `Success
                  | `Standalone
                  | `Moderator_standalone
                  | `Denied
                  | `Revoked
                  | `Handler_fail
                  | `Save_fail
                  | `Disclosure
                  | `Reentrant
                  | `Domain_reentrant
                  | `Domain_budget
                  | `Cancel
                  ])
             , (!native_calls : int)
             , (!count : int)
             , (Jsonaf.to_string !summary : string)
             , (!requests : Chat_response.Moderation.Runtime_request.t list)]));
  [%expect
    {|
    (Success 1 1 "\"private output\"" (Request_compaction))
    (Standalone 1 1 "\"private output\"" (Request_compaction))
    (Moderator_standalone 1 1 "\"private output\"" (Request_compaction))
    (Denied 0 0 "\"invocation.permission_denied\"" ())
    (Revoked 0 0 "\"invocation.stale_binding\"" ())
    (Handler_fail 1 0 "\"invocation.handler_failed\"" ())
    (Save_fail 1 0 "\"invocation.commit_failed\"" ())
    (Disclosure 1 0 "\"invocation.invalid_output\"" ())
    (Reentrant 0 1 "\"moderator_reentrancy\"" (Request_compaction))
    (Domain_reentrant 1 1 "\"domain reentrancy rejected\"" (Request_compaction))
    (Domain_budget 1 0 "\"chatml.execution_limit\"" ())
    (Cancel 1 0 "\"cancelled\"" ())
    |}]
;;
