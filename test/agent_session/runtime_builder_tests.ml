open Core
open Fixtures

let%expect_test "captured runtime construction installs owned lifecycle and script tools" =
  let module A = Agent_session.Session_actor in
  let module B = Agent_session.Runtime_builder in
  let module M = Chat_response.Moderator_manager in
  let module C = Chat_response.Tool_capability in
  List.iter
    [ `Foreground
    ; `Idle
    ; `Denied
    ; `Revoked
    ; `Standalone
    ; `Standalone_pre_denied
    ; `Standalone_rewrite
    ; `Standalone_only
    ; `Standalone_end
    ; `Standalone_one_off
    ; `One_off_only
    ; `One_off
    ; `One_off_pre_denied
    ; `One_off_rewrite
    ; `One_off_end
    ; `One_off_recursive
    ; `One_off_managed
    ; `One_off_moderator
    ; `One_off_compile
    ; `One_off_limit
    ; `One_off_start
    ; `One_off_idle
    ; `One_off_observation
    ; `One_off_required
    ]
    ~f:(fun mode ->
      let one_off =
        match mode with
        | `One_off_only
        | `One_off
        | `One_off_pre_denied
        | `One_off_rewrite
        | `One_off_end
        | `One_off_recursive
        | `One_off_managed
        | `One_off_moderator
        | `One_off_compile
        | `One_off_limit
        | `One_off_start
        | `One_off_idle
        | `One_off_observation
        | `One_off_required -> true
        | _ -> false
      in
      let tool_name = if one_off then "run_chatml" else "counter" in
      with_actor_workspace (fun env workspace_instance ->
        Eio.Switch.run (fun sw ->
          let root =
            Eio.Path.(Eio.Stdenv.fs env / workspace_instance.canonical_root.native_path)
          in
          List.iter [ "data"; "cache"; "session" ] ~f:(fun name ->
            Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(root / name));
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            Eio.Path.(root / "data" / "value.txt")
            "approved value";
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            Eio.Path.(root / "input.json")
            {|{"type":"object","properties":{},"additionalProperties":false}|};
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            Eio.Path.(root / "output.json")
            {|{"type":"string"}|};
          let source =
            {|<tool name="read_file"><read id="data" path="${workspace}/data"/></tool>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = [0]
let read = fun () -> Tool.call("read_file", `Object([{key = "root"; value = `String("data")}, {key = "file"; value = `String("value.txt")}]))
let on_event = fun ctx state event -> match event with
| `Session_start -> Task.bind(read(), fun result -> match result with
    | `Error(code) -> Task.fail(code)
    | `Ok(value) -> let ignored = state[0] <- 10 in Task.pure(state))
| `Tool_invoked(p) -> Task.bind(read(), fun result -> match result with
    | `Error(code) -> Task.fail(code)
    | `Ok(value) -> let ignored = state[0] <- state[0] + 1 in
      Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`String(to_string(state[0])))), fun ignored -> Task.pure(state)))
| `Tool_observed(p) -> (match p.origin with
    | `Script -> let ignored = state[0] <- state[0] + 1 in Task.pure(state)
    | _ -> Task.pure(state))
| _ -> Task.pure(state)
</script>
<tool name="counter" type="moderator" moderator="owner" input_schema="input.json" output_schema="output.json"/>|}
          in
          let source =
            match mode with
            | `Standalone
            | `Standalone_pre_denied
            | `Standalone_rewrite
            | `Standalone_only
            | `Standalone_end
            | `Standalone_one_off
            | `One_off_only
            | `One_off
            | `One_off_pre_denied
            | `One_off_rewrite
            | `One_off_end
            | `One_off_recursive
            | `One_off_managed
            | `One_off_moderator
            | `One_off_compile
            | `One_off_limit
            | `One_off_start
            | `One_off_idle
            | `One_off_observation
            | `One_off_required ->
              let file =
                match mode with
                | `Standalone_rewrite | `One_off_rewrite -> "missing.txt"
                | _ -> "value.txt"
              in
              let standalone =
                {|<script id="standalone" language="chatml" kind="tool">
let count = [0]
let run ctx input = Task.bind(Tool.call("read_file", `Object([{key = "root"; value = `String("data")}, {key = "file"; value = `String("|}
                ^ file
                ^ {|")}])), fun result -> match result with
  | `Error(code) -> Task.pure(`Fail({code = code; message = "read failed"; retryable = false; details = `Null}))
  | `Ok(value) -> let ignored = count[0] <- count[0] + 1 in
    Task.pure(`Complete(`String(to_string(count[0])))))
</script>
<tool name="counter" type="chatml" script="standalone" entrypoint="run" input_schema="input.json" output_schema="output.json"><uses tool="read_file"/></tool>|}
              in
              let source =
                let standalone =
                  match mode with
                  | `Standalone_one_off ->
                    let program =
                      {|let main input = Task.bind(Tool.call("read_file", input), fun result -> match result with
| `Ok(value) -> Task.pure(value) | `Error(code) -> Task.fail(code))|}
                    in
                    let encoded_program = Jsonaf.to_string (`String program) in
                    {|<tool name="run_chatml"/>
<script id="standalone" language="chatml" kind="tool">
let run ctx input = Task.bind(Tool.call("run_chatml", `Object([
{key = "source"; value = `String(|}
                    ^ encoded_program
                    ^ {|)},
{key = "input"; value = `Object([{key = "root"; value = `String("data")}, {key = "file"; value = `String("value.txt")}])},
{key = "tools"; value = `Array([`String("read_file")])}])), fun result -> match result with
| `Ok(_) -> Task.pure(`Complete(`String("1")))
| `Error(code) -> Task.pure(`Fail({code = code; message = "one-off failed"; retryable = false; details = `Null})))
</script>
<tool name="counter" type="chatml" script="standalone" entrypoint="run" input_schema="input.json" output_schema="output.json"><uses tool="run_chatml"/><uses tool="read_file"/></tool>|}
                  | _ -> standalone
                in
                String.substr_replace_all
                  source
                  ~pattern:
                    {|<tool name="counter" type="moderator" moderator="owner" input_schema="input.json" output_schema="output.json"/>|}
                  ~with_:
                    (match mode with
                     | `One_off_managed -> {|<tool name="run_chatml"/>|} ^ standalone
                     | `One_off_moderator ->
                       {|<tool name="run_chatml"/><tool name="counter" type="moderator" moderator="owner" input_schema="input.json" output_schema="output.json"/>|}
                     | _ -> if one_off then {|<tool name="run_chatml"/>|} else standalone)
              in
              let pre =
                match mode with
                | `Standalone_pre_denied | `One_off_pre_denied ->
                  {|Task.bind(Tool.reject("blocked nested call"), fun ignored -> Task.pure(state))|}
                | `Standalone_rewrite | `One_off_rewrite ->
                  {|Task.bind(Tool.rewrite_args(`Object([{key = "root"; value = `String("data")}, {key = "file"; value = `String("value.txt")}])), fun ignored -> Task.pure(state))|}
                | `Standalone_end | `One_off_end ->
                  {|Task.bind(Runtime.end_session("nested pre ended session"), fun ignored -> Task.pure(state))|}
                | _ -> "Task.pure(state)"
              in
              String.substr_replace_all
                source
                ~pattern:"| _ -> Task.pure(state)\n</script>"
                ~with_:
                  ("| `Pre_tool_call(c) -> if c.name == \"read_file\" then "
                   ^ pre
                   ^ " else Task.pure(state)\n| _ -> Task.pure(state)\n</script>")
            | _ -> source
          in
          let source =
            match mode with
            | `Standalone_only | `One_off_only ->
              let first =
                String.substr_index_exn source ~pattern:"<script id=\"owner\""
              in
              let last =
                String.substr_index_exn source ~pos:first ~pattern:"</script>"
                + String.length "</script>"
              in
              String.prefix source first ^ String.drop_prefix source last
            | _ -> source
          in
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            Eio.Path.(root / "root.chatmd")
            (let program =
               {|let main input = Task.bind(Tool.call("read_file", input), fun result -> match result with
| `Ok(value) -> Task.pure(value) | `Error(code) -> Task.fail(code))|}
             in
             let run =
               {|Tool.call("run_chatml", `Object([{key = "source"; value = `String(|}
               ^ Jsonaf.to_string (`String program)
               ^ {|)},
{key = "input"; value = `Object([{key = "root"; value = `String("data")}, {key = "file"; value = `String("value.txt")}])},
{key = "tools"; value = `Array([`String("read_file")])}]))|}
             in
             match mode with
             | `One_off_start | `One_off_idle | `One_off_required ->
               String.substr_replace_all
                 source
                 ~pattern:
                   {|Tool.call("read_file", `Object([{key = "root"; value = `String("data")}, {key = "file"; value = `String("value.txt")}]))|}
                 ~with_:run
             | `One_off_observation ->
               String.substr_replace_all
                 source
                 ~pattern:"| `Tool_observed(p) -> (match p.origin with"
                 ~with_:
                   ("| `Tool_observed(p) -> (match p.origin with\n"
                    ^ "| `Moderator -> if state[0] == 10 then let ignored = state[0] <- \
                       20 in Task.bind("
                    ^ run
                    ^ ", fun result -> match result with | `Error(code) -> \
                       Task.fail(code) | `Ok(_) -> Task.pure(state)) else \
                       Task.pure(state)")
             | _ -> source);
          let definition =
            Agent_session.Prompt_definition.create
              ~id:prompt_id
              ~config_name:"extension-runtime"
              ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
              ~allowed_workspaces:[ workspace_id ]
              ~permission_profile:"interactive"
              ~runtime_policy:None
              ~enabled:true
              ~description:None
            |> store_ok
          in
          let artifact_store =
            Agent_store.Prompt_artifact_store.create
              ~env
              ~root:(Eio.Path.native_exn Eio.Path.(root / "artifacts"))
            |> store_ok
          in
          let revision =
            Agent_session.Prompt_revision_builder.build
              ~env
              ~artifact_store
              ~transaction_id
              ~created_at:timestamp
              definition
            |> Result.map_error ~f:(fun errors ->
              Sexp.to_string_hum
                [%sexp (errors : Agent_session.Prompt_revision_builder.Diagnostic.t list)])
            |> Result.ok_or_failwith
          in
          let initial =
            actor_state
              ~workspace_instance
              ~liveness:Process_bound
              ~start_immediately:false
          in
          let initial =
            { initial with
              spec =
                { initial.spec with
                  prompt_revision_id = Agent_session.Prompt_revision.id revision
                }
            }
          in
          let backend =
            Agent_session.Memory_backend.create ~event_capacity:128 ~initial_state:initial
          in
          let actor =
            A.create
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~mailbox_capacity:32
              ~compaction_env:None
              ~initial_state:initial
              ~operation_worker:None
              ~persistence:(Agent_session.Memory_backend.persistence backend)
              ~services:
                { now = Agent_protocol.Timestamp.now
                ; create_attachment_id = Agent_protocol.Id.Attachment.create
                ; create_reclaim_token = (fun () -> "constructed-runtime")
                ; job_results = None
                ; state_committed = (fun _ _ -> ())
                }
          in
          let requests = ref 0
          and authorized = ref 0
          and evaluations = ref 0
          and registry_ref = ref None in
          let services : B.extension_services =
            { script_tools =
                (fun native ->
                  let registry =
                    Lazy.force native.Chat_response.Agent_runtime.capabilities
                    |> Result.map_error ~f:(fun error -> error.C.message)
                    |> Result.ok_or_failwith
                  in
                  registry_ref := Some registry;
                  (match C.find registry ~name:"run_chatml" with
                   | Error _ -> ()
                   | Ok binding ->
                     let metadata = C.metadata binding in
                     let help = Option.value_exn metadata.authoring in
                     assert (
                       Chatmd_shell_spec.Authoring_metadata.equal_help
                         help
                         (Chat_response.Authoring_validation.help One_off_script)));
                  Agent_session.Script_tool_calls.create
                    ~registry:(fun () -> Option.value_exn !registry_ref)
                    ~moderator_names:
                      (match mode with
                       | `One_off_managed | `One_off_moderator -> String.Set.empty
                       | _ -> String.Set.singleton "counter")
                    ~now:Agent_protocol.Timestamp.now
                    ~is_halted:(fun () -> (A.state actor |> protocol_ok).halted)
                    ~requires_active_moderator:(fun reference ->
                      match mode with
                      | `One_off_required -> String.equal reference.C.name "read_file"
                      | _ -> false)
                    ~authorize:(fun invocation _ ->
                      match mode with
                      | `Denied ->
                        Error
                          (Agent_protocol.Error.create
                             Permission_denied
                             ~message:"denied by native policy"
                             ~retryable:false
                             ())
                      | _ ->
                        incr authorized;
                        assert (
                          String.equal
                            invocation.Agent_protocol.Invocation.context.tool_name
                            "run_chatml"
                          || Option.is_some invocation.parent_event
                          || Option.is_some invocation.context.parent_invocation);
                        Eio.Fiber.yield ();
                        Ok ())
                    ~prepare_output:(function
                      | Openai.Responses.Tool_output.Output.Text text -> Ok (`String text)
                      | output ->
                        Ok (Openai.Responses.Tool_output.Output.jsonaf_of_t output))
                    ~defer_observation:(fun _ -> Ok ()))
            ; standalone_execution_limits =
                Agent_session.Standalone_tool_dispatch.declared_execution_limits
            ; one_off_policy = Chat_response.One_off_request.default_policy
            ; authoring_validation_host = None
            ; lifecycle_started = (fun _ -> false)
            ; claim_lifecycle =
                (fun ~event ->
                  A.with_current_moderator_event actor ~operation_id:None ~event)
            ; history =
                (fun () ->
                  (A.state actor |> protocol_ok).conversation.canonical_history
                  |> Agent_session.History_codec.all_of_protocol
                  |> protocol_ok)
            }
          in
          let policy =
            match mode with
            | `Revoked ->
              permission_policy
                ~tool_default:Policy
                ~fallback:Fallback_deny
                ~evaluator:
                  (Some
                     (fun _ ->
                       incr evaluations;
                       registry_ref
                       := Some
                            (C.select (Option.value_exn !registry_ref) ~names:[]
                             |> Result.map_error ~f:(fun error -> error.C.message)
                             |> Result.ok_or_failwith);
                       Eio.Fiber.yield ();
                       Ok true))
                ~reviewer:None
            | _ ->
              permission_policy
                ~tool_default:Allow
                ~fallback:Fallback_deny
                ~evaluator:None
                ~reviewer:None
          in
          let paths : Agent_session.Runtime_paths.t =
            { tool_dir = root
            ; workspace = root
            ; prompt_dir = Agent_session.Prompt_revision.materialized_tree revision
            ; session_dir = Eio.Path.(root / "session")
            ; cache_dir = Eio.Path.(root / "cache")
            ; home = root
            }
          in
          let reservation = A.reserve_history_block actor ~count:100 |> protocol_ok in
          let payload =
            match one_off with
            | false -> "{}"
            | true ->
              let file =
                match mode with
                | `One_off_rewrite -> "missing.txt"
                | _ -> "value.txt"
              in
              let input = `Object [ "root", `String "data"; "file", `String file ] in
              let source =
                {|let main input = Task.bind(Tool.call("read_file", input), fun result -> match result with
| `Ok(_) -> Task.pure(`String("1")) | `Error(code) -> Task.fail(code))|}
              in
              let request source input tools =
                `Object
                  [ "source", `String source
                  ; "input", input
                  ; "tools", `Array (List.map tools ~f:(fun name -> `String name))
                  ]
              in
              (match mode with
               | `One_off_managed | `One_off_moderator ->
                 request
                   {|let main input = Task.bind(Tool.call("counter", input), fun result -> match result with
| `Ok(value) -> Task.pure(value) | `Error(code) -> Task.fail(code))|}
                   (`Object [])
                   [ "counter" ]
               | `One_off_compile ->
                 request "let main input = Task.pure(input + 1)" input [ "read_file" ]
               | `One_off_limit ->
                 (match request source input [ "read_file" ] with
                  | `Object fields ->
                    `Object (fields @ [ "limits", `Object [ "max_calls", `Number "101" ] ])
                  | _ -> assert false)
               | `One_off_recursive ->
                 request
                   {|let main input = Task.bind(Tool.call("run_chatml", input), fun result -> match result with
| `Ok(value) -> Task.pure(value) | `Error(code) -> Task.fail(code))|}
                   (request source input [ "read_file" ])
                   [ "run_chatml"; "read_file" ]
               | _ -> request source input [ "read_file" ])
              |> Jsonaf.to_string
          in
          let post_stream ~sw:_ ~inputs:_ =
            incr requests;
            match !requests with
            | 1 ->
              List.concat_map [ 0; 1 ] ~f:(fun index ->
                let open Openai.Responses.Response_stream in
                let item_id = "constructed-item-" ^ Int.to_string index in
                [ Output_item_added
                    { item =
                        Function_call
                          { name = tool_name
                          ; arguments = ""
                          ; call_id = "constructed-call-" ^ Int.to_string index
                          ; _type = "function_call"
                          ; id = Some item_id
                          ; status = None
                          }
                    ; output_index = index
                    ; type_ = "response.output_item.added"
                    }
                ; Function_call_arguments_done
                    { arguments = payload
                    ; item_id
                    ; output_index = index
                    ; type_ = "response.function_call_arguments.done"
                    }
                ])
              |> Stdlib.List.to_seq
            | _ -> Stdlib.Seq.empty
          in
          let build builder =
            builder
              ~sw
              ~env
              ~paths
              ~storage_paths:paths
              ~revision
              ~session_id:initial.identity.session_id
              ~history_namespace:
                (Agent_protocol.Id.Session.to_string initial.identity.session_id)
              ~next_history_sequence:(Int64.to_int_exn reservation.first_sequence)
              ~existing_history:(Some [])
              ~existing_moderator_snapshot:None
              ~moderator_reservation_size:100
              ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
              ~approval_provider:Shell_runtime.Approval_broker.None_available
              ~approval_store:(Shell_access.Approval.create_store ())
              ~permission_profile:policy
              ~model_post_stream:(Some post_stream)
              ~review_permission:(fun _ -> assert false)
              ~schedule_services:
                B.
                  { after_ms = (fun ~delay_ms:_ ~payload:_ -> failwith "unexpected timer")
                  ; cancel = (fun ~id:_ -> assert false)
                  }
              ~job_services:
                B.
                  { spawn_model = (fun ~recipe:_ ~payload:_ -> assert false)
                  ; call_model = (fun ~recipe:_ ~payload:_ ~execute:_ -> assert false)
                  }
          in
          (match mode with
           | `One_off_only ->
             assert (Result.is_error (build B.build));
             assert (Int.equal !requests 0);
             assert (Int.equal !authorized 0)
           | _ -> ());
          let runtime = build (B.build_with_extensions ~services) |> protocol_ok in
          let owner =
            Agent_server.Runtime_owner.create
              ~actor
              ~initial:(Some runtime)
              ~build:(fun () -> failwith "unexpected runtime rebuild")
          in
          Exn.protect
            ~finally:(fun () ->
              Agent_server.Runtime_owner.close owner;
              A.shutdown actor)
            ~f:(fun () ->
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                let initial_snapshot = runtime.start_moderator () |> protocol_ok in
                [%test_eq: int] 0 !authorized;
                [%test_eq: int] 0 !requests;
                A.change_moderator actor initial_snapshot |> protocol_ok |> ignore;
                A.set_operation_worker actor (Some runtime.worker) |> protocol_ok;
                assert (
                  not
                    (Agent_server.Runtime_owner.drain_idle_moderator owner |> protocol_ok));
                let names =
                  List.filter_map runtime.moderator_tools ~f:(function
                    | Openai.Responses.Request.Tool.Function tool -> Some tool.name
                    | _ -> None)
                in
                [%test_eq: string list]
                  (List.sort
                     (match mode with
                      | `Standalone_one_off -> [ tool_name; "read_file"; "run_chatml" ]
                      | `One_off_managed | `One_off_moderator ->
                        [ "counter"; "read_file"; "run_chatml" ]
                      | _ -> [ tool_name; "read_file" ])
                     ~compare:String.compare)
                  (List.sort names ~compare:String.compare);
                let writer, _ =
                  A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
                in
                A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
                (match mode with
                 | `Idle ->
                   assert (
                     Agent_server.Runtime_owner.drain_idle_moderator owner |> protocol_ok);
                   [%test_eq: int] 1 !authorized
                 | `One_off_idle | `One_off_observation ->
                   let rec drain remaining =
                     assert (remaining > 0);
                     match
                       Agent_server.Runtime_owner.drain_idle_moderator owner
                       |> protocol_ok
                     with
                     | true -> drain (remaining - 1)
                     | false -> ()
                   in
                   drain 20;
                   [%test_eq: int]
                     (match mode with
                      | `One_off_idle -> 2
                      | _ -> 3)
                     !authorized
                 | _ -> ());
                let entry =
                  Agent_session.History_codec.user_text ~id:history_id "count twice"
                  |> Agent_session.History_codec.to_protocol
                in
                A.submit_message actor ~attachment_id:writer.id entry
                |> protocol_ok
                |> ignore;
                let rec finished () =
                  let state = A.state actor |> protocol_ok in
                  match state.active_operation with
                  | None -> state
                  | Some _ ->
                    Eio.Fiber.yield ();
                    finished ()
                in
                let state = finished () in
                (match mode with
                 | `Standalone_end | `One_off_end -> assert state.halted
                 | _ -> ());
                List.iter state.invocations ~f:(fun invocation ->
                  match invocation.context.origin, invocation.status with
                  | Model, Published _ ->
                    assert (Option.is_some invocation.output_entry_id)
                  | Script, _ ->
                    assert (Option.is_some invocation.context.parent_invocation);
                    assert (Option.is_none invocation.context.provider_call_id);
                    assert (Option.is_none invocation.output_entry_id)
                  | _ -> ());
                assert_same_session_snapshot
                  state
                  (Agent_session.Memory_backend.state backend);
                (match mode with
                 | `One_off_compile | `One_off_limit ->
                   assert (
                     not
                       (List.exists state.invocations ~f:(fun invocation ->
                          Agent_protocol.Invocation.equal_origin
                            invocation.context.origin
                            Script)));
                   (match mode with
                    | `One_off_compile ->
                      let source =
                        Jsonaf.of_string payload
                        |> Jsonaf.member_exn "source"
                        |> Jsonaf.string_exn
                      in
                      List.iter state.invocations ~f:(fun invocation ->
                        match invocation.status with
                        | Published (Fail error) ->
                          let diagnostic =
                            Jsonaf.member_exn "diagnostics" error.details
                            |> Jsonaf.list_exn
                            |> List.hd_exn
                            |> Chatmd_shell_spec.Diagnostic.t_of_jsonaf
                          in
                          assert (List.equal String.equal diagnostic.path [ "source" ]);
                          assert (
                            String.equal
                              (Option.value_exn diagnostic.source).source_sha256
                              (Chatmd_shell_spec.Source_ref.digest source))
                        | _ -> ())
                    | _ -> ())
                 | `One_off_pre_denied ->
                   let rejected_reads =
                     List.count state.invocations ~f:(fun invocation ->
                       String.equal invocation.context.tool_name "read_file"
                       &&
                       match invocation.status with
                       | Resolved (Fail { code = "invocation.pre_tool_rejected"; _ }) ->
                         true
                       | _ -> false)
                   in
                   [%test_eq: int] 2 rejected_reads
                 | _ -> ());
                let count =
                  match runtime.moderator_manager with
                  | None -> 0
                  | Some manager ->
                    (match
                       (M.identity_snapshot manager |> Result.ok_or_failwith)
                         .current_state
                     with
                     | Session.Snapshot.Array [ Int n ] -> n
                     | _ -> assert false)
                in
                let published =
                  List.filter_map state.invocations ~f:(fun invocation ->
                    match invocation.status with
                    | Published (Complete (`String value)) -> Some value
                    | Published (Fail error) -> Some error.code
                    | _ -> None)
                  |> List.sort ~compare:String.compare
                in
                let failures =
                  Agent_session.Memory_backend.events_after backend 0L
                  |> protocol_ok
                  |> List.filter_map ~f:(fun event ->
                    match
                      Agent_protocol.Event.Durable.Payload.of_json
                        ~kind:event.kind
                        event.payload
                      |> protocol_ok
                    with
                    | Operation_failed { state = Failed error; _ } -> Some error.message
                    | _ -> None)
                in
                let observations =
                  List.filter_map state.invocations ~f:(fun invocation ->
                    match invocation.observation with
                    | Some { status = Observation_failed reason; _ } -> Some reason
                    | _ -> None)
                in
                assert (List.is_empty observations);
                (match mode with
                 | `Denied | `One_off_required -> [%test_eq: int] 1 (List.length failures)
                 | _ ->
                   if not (List.is_empty failures)
                   then
                     raise_s
                       [%sexp
                         "constructed runtime failed"
                       , (failures : string list)
                       , (published : string list)
                       , (state.halted : bool)]);
                print_s
                  [%sexp
                    { mode : [ `Foreground
                             | `Idle
                             | `Denied
                             | `Revoked
                             | `Standalone
                             | `Standalone_pre_denied
                             | `Standalone_rewrite
                             | `Standalone_only
                             | `Standalone_end
                             | `Standalone_one_off
                             | `One_off_only
                             | `One_off
                             | `One_off_pre_denied
                             | `One_off_rewrite
                             | `One_off_end
                             | `One_off_recursive
                             | `One_off_managed
                             | `One_off_moderator
                             | `One_off_compile
                             | `One_off_limit
                             | `One_off_start
                             | `One_off_idle
                             | `One_off_observation
                             | `One_off_required
                             ]
                    ; provider_calls = (!requests : int)
                    ; authorized_native_calls = (!authorized : int)
                    ; policy_evaluations = (!evaluations : int)
                    ; state = (count : int)
                    ; published : string list
                    ; operation_failed = (not (List.is_empty failures) : bool)
                    ; observed =
                        (List.count state.invocations ~f:(fun invocation ->
                           match invocation.observation with
                           | Some { status = Observed; _ } -> true
                           | _ -> false)
                         : int)
                    }])))));
  [%expect
    {|
    ((mode Foreground) (provider_calls 2) (authorized_native_calls 3)
     (policy_evaluations 0) (state 12) (published (11 12))
     (operation_failed false) (observed 3))
    ((mode Idle) (provider_calls 2) (authorized_native_calls 3)
     (policy_evaluations 0) (state 12) (published (11 12))
     (operation_failed false) (observed 3))
    ((mode Denied) (provider_calls 0) (authorized_native_calls 0)
     (policy_evaluations 0) (state 0) (published ()) (operation_failed true)
     (observed 0))
    ((mode Revoked) (provider_calls 2) (authorized_native_calls 1)
     (policy_evaluations 1) (state 10)
     (published (invocation.permission_denied invocation.permission_denied))
     (operation_failed false) (observed 1))
    ((mode Standalone) (provider_calls 2) (authorized_native_calls 3)
     (policy_evaluations 0) (state 12) (published (1 1)) (operation_failed false)
     (observed 3))
    ((mode Standalone_pre_denied) (provider_calls 2) (authorized_native_calls 1)
     (policy_evaluations 0) (state 12)
     (published (invocation.pre_tool_rejected invocation.pre_tool_rejected))
     (operation_failed false) (observed 3))
    ((mode Standalone_rewrite) (provider_calls 2) (authorized_native_calls 3)
     (policy_evaluations 0) (state 12) (published (1 1)) (operation_failed false)
     (observed 3))
    ((mode Standalone_only) (provider_calls 2) (authorized_native_calls 2)
     (policy_evaluations 0) (state 0) (published (1 1)) (operation_failed false)
     (observed 0))
    ((mode Standalone_end) (provider_calls 1) (authorized_native_calls 1)
     (policy_evaluations 0) (state 10)
     (published (invocation.pre_tool_rejected invocation.session_ended))
     (operation_failed false) (observed 1))
    ((mode Standalone_one_off) (provider_calls 2) (authorized_native_calls 5)
     (policy_evaluations 0) (state 14) (published (1 1)) (operation_failed false)
     (observed 5))
    ((mode One_off_only) (provider_calls 2) (authorized_native_calls 4)
     (policy_evaluations 0) (state 0) (published (1 1)) (operation_failed false)
     (observed 0))
    ((mode One_off) (provider_calls 2) (authorized_native_calls 5)
     (policy_evaluations 0) (state 12) (published (1 1)) (operation_failed false)
     (observed 3))
    ((mode One_off_pre_denied) (provider_calls 2) (authorized_native_calls 3)
     (policy_evaluations 0) (state 12)
     (published (chatml.execution_failed chatml.execution_failed))
     (operation_failed false) (observed 3))
    ((mode One_off_rewrite) (provider_calls 2) (authorized_native_calls 5)
     (policy_evaluations 0) (state 12) (published (1 1)) (operation_failed false)
     (observed 3))
    ((mode One_off_end) (provider_calls 1) (authorized_native_calls 3)
     (policy_evaluations 0) (state 10)
     (published (chatml.execution_failed chatml.execution_failed))
     (operation_failed false) (observed 1))
    ((mode One_off_recursive) (provider_calls 2) (authorized_native_calls 7)
     (policy_evaluations 0) (state 14) (published (1 1)) (operation_failed false)
     (observed 5))
    ((mode One_off_managed) (provider_calls 2) (authorized_native_calls 7)
     (policy_evaluations 0) (state 14) (published (1 1)) (operation_failed false)
     (observed 5))
    ((mode One_off_moderator) (provider_calls 2) (authorized_native_calls 7)
     (policy_evaluations 0) (state 14) (published (11 12))
     (operation_failed false) (observed 5))
    ((mode One_off_compile) (provider_calls 2) (authorized_native_calls 3)
     (policy_evaluations 0) (state 10)
     (published (chatml.type_error chatml.type_error)) (operation_failed false)
     (observed 1))
    ((mode One_off_limit) (provider_calls 2) (authorized_native_calls 3)
     (policy_evaluations 0) (state 10)
     (published (chatml.limit_escalation chatml.limit_escalation))
     (operation_failed false) (observed 1))
    ((mode One_off_start) (provider_calls 2) (authorized_native_calls 6)
     (policy_evaluations 0) (state 13) (published (1 1)) (operation_failed false)
     (observed 4))
    ((mode One_off_idle) (provider_calls 2) (authorized_native_calls 6)
     (policy_evaluations 0) (state 13) (published (1 1)) (operation_failed false)
     (observed 4))
    ((mode One_off_observation) (provider_calls 2) (authorized_native_calls 7)
     (policy_evaluations 0) (state 23) (published (1 1)) (operation_failed false)
     (observed 5))
    ((mode One_off_required) (provider_calls 0) (authorized_native_calls 1)
     (policy_evaluations 0) (state 0) (published ()) (operation_failed true)
     (observed 0))
    |}]
;;
