open! Core
module P = Agent_protocol
module S = Agent_server
module R = Agent_store.Developer_command_receipt
module RH = Runtime_host_tests
module DH = Developer_root_owner_test_suite.Developer_host_tests

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let developer_configuration ?remote_content target =
  let selected =
    S.Authoring_standard_contracts.create
      ~targets:[ Chatml.Chatml_compiler_contract.Target.One_off_v1 ]
      ~max_identity_bytes:(16 * 1024 * 1024)
      ~max_work_steps:1_000_000
      ~checkpoint:ignore
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum [%sexp (error : S.Authoring_standard_contracts.Error.t)])
    |> Result.ok_or_failwith
    |> fun contracts ->
    S.Authoring_standard_contracts.find
      contracts
      Chatml.Chatml_compiler_contract.Target.One_off_v1
    |> Option.value_exn
    |> S.Authoring_context_service.Developer.Selected_contract.of_standard_entry
    |> ok
  in
  let capture_policy =
    S.Authoring_context_service.Developer.Target_policy.create
      ~local_project:(fun _ ~root:_ ~project:_ ~selection:_ -> None)
      ~local_source:(fun _ ~root:_ ~member:_ -> None)
      ~authoring_project:(fun _ ~context:_ ~current:_ -> None)
      ~document_project:
        (fun
          _ ~context:_ ~current:_ ~parsed:_ ~declaration:_ ~build:_ ~metadata:_ -> None)
      ~document_source:(fun _ ~context:_ ~current:_ ~parsed:_ ~declaration:_ ~script:_ ->
        Some selected)
  in
  let document =
    Document_schema.Limits.create
      ~max_bytes:(1024 * 1024)
      ~max_depth:16
      ~max_fields:1024
      ~max_nodes:2048
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum [%sexp (error : Document_schema.Error.t)])
    |> Result.ok_or_failwith
  in
  let capture_limits =
    S.Authoring_context_service.Developer.Limits.create
      ~max_prepared_input_bytes:4096
      ~max_document_origin_bytes:16384
      ~max_editor_versions:8
      ~build:
        (Chatml_project_build.Limits.create
           ~max_work_steps:100_000
           ~max_input_bytes:(Some 4096)
           ~document
         |> Or_error.ok_exn)
    |> Or_error.ok_exn
  in
  let store =
    Developer_root_owner_test_suite.Root_owner_tests.store_limits
      ~max_runs:6
      ~max_job_payload_bytes:4096
      ~max_jobs_per_run:8
      ~max_inference_bytes_per_run:65536
      ~max_document_bytes:(1024 * 1024)
      ~max_work_steps:100_000
      ()
  in
  let roots =
    S.Developer_root_owner.Limits.create
      ~max_active_runs:6
      ~max_staged_jobs:4
      ~max_native_occupancy:2
      ~max_active_workers:1
      ~max_quarantined_workers:2
      ~max_active_inference_attempts:8
      ~max_admitted_call_bindings:32
    |> Or_error.ok_exn
  in
  let disclosure =
    S.Developer_result_disclosure.Limits.create ~max_result_bytes:65536 ~json:document
    |> ok
  in
  let workspace_cleanup =
    Agent_session.Workspace_cleanup.Limits.create
      ~max_entries:128
      ~max_bytes:65536
      ~max_work_steps:100_000
    |> Or_error.ok_exn
  in
  let limits =
    S.Daemon.Developer.Configuration.Limits.create
      ~store
      ~roots
      ~max_active_commands:4
      ~disclosure
      ~compilation:(Bounded Chatml_compilation.default_limits)
      ~max_dependency_depth:32
      ~execution:Chatml_execution.default_limits
      ~max_output_bytes:65536
      ~receipt:document
      ~max_agent_sources:6
      ~max_agent_source_work_steps:100_000
      ~max_native_bindings:8
      ~workspace_cleanup
      ~max_agent_cache_entries:8
    |> ok
  in
  let configuration =
    S.Daemon.Developer.Configuration.create
      ~capture_policy
      ~capture_limits
      ~limits
      ~result_disclosure:
        (S.Developer_result_disclosure.Policy.create ~authorize:(fun _ _ -> true))
      ~receipt_read:(fun _ _ -> true)
      ~command_control:(fun _ _ -> true)
      ~publisher_limits:
        (Agent_store.Developer_result_publisher.Limits.create
           ~inline_bytes:1024
           ~max_result_bytes:65536
           ~max_pending:2
         |> ok)
      ~publisher_storage:Agent_store.Developer_result_publisher.Storage.Automatic
      ~model_target:(Some target)
      ~authorize_native:(fun invocation binding ->
        let name = (Chat_response.Tool_capability.reference binding).name in
        if List.mem [ "read_file"; "summary"; "undeclared" ] name ~equal:String.equal
        then (
          assert (
            P.Invocation.equal_origin
              (P.Host_invocation.context invocation).origin
              P.Invocation.Delegated_agent);
          Ok ())
        else if
          List.mem
            [ "child"
            ; "child_tools"
            ; "deep"
            ; "model_child"
            ; "model_mixed"
            ; "model_spawn"
            ; "queued_producer"
            ]
            name
            ~equal:String.equal
        then Ok ()
        else Error (P.Error.invalid_request "unexpected native binding"))
      ~prepare_output:(function
        | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
        | _ -> Error (P.Error.invalid_request "unexpected native output"))
    |> ok
  in
  Option.value_map remote_content ~default:configuration ~f:(fun policy ->
    S.Daemon.Developer.Configuration.with_remote_content configuration policy)
;;

let open_document
      ?(member = "draft.chatmd")
      ?contents
      ?(analysis_script = "s")
      daemon
      actor
  =
  let connection =
    S.Connection_context.create_authenticated
      ~actor
      ~connection_id:P.Id.Transaction.(create () |> to_string)
      ~transport:In_memory
      ~publish_notification:(fun _ -> ())
      ~max_attachments:8
  in
  let transport =
    Agent_client.In_memory.create
      ~notifications:(Eio.Stream.create 8)
      ~request:
        (S.Dispatcher.dispatch_command (S.Daemon.dispatcher daemon) ~context:connection)
      ~close:(fun () -> S.Daemon.close_connection daemon connection)
  in
  let implementation =
    P.Initialize.Implementation.create ~name:"hosted-agent" ~version:"1" |> ok
  in
  let initialize =
    P.Initialize.Request.create
      ~implementation
      ~protocol_min:P.Version.current
      ~protocol_max:P.Version.current
      ~features:[ P.Authoring.Features.core; P.Authoring.Features.semantic ]
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(1024 * 1024)
      ()
    |> ok
  in
  (match
     Agent_client.Connection.request_without_history
       transport
       (Protocol_initialize initialize)
   with
   | Ok (Protocol_initialize _) -> ()
   | Ok _ -> failwith "unexpected initialization response"
   | Error error -> raise_s [%sexp (error : P.Error.t)]);
  let request =
    P.Authoring.Context.Open_request.create_supplied_document
      ~member
      ~default_environment:None
      ~contents:
        (Option.value
           contents
           ~default:
             {|<script id="s" language="chatml" kind="tool">let main input = Task.bind(Job.start_tool("child", input), fun first -> Task.bind(Job.start_tool("child", input), fun second -> Task.pure(`Object([{key="first"; value=`String(first)}, {key="second"; value=`String(second)}]))))</script>
<script id="tool_child" language="chatml" kind="tool">let main input = Task.bind(Job.start_tool("child_tools", input), fun id -> Task.pure(`String(id)))</script>
<script id="deep_child" language="chatml" kind="tool">let main input = Task.bind(Job.start_tool("deep", input), fun id -> Task.pure(`String(id)))</script>
<script id="model_child" language="chatml" kind="tool">let main input = Task.bind(Job.start_tool("model_child", input), fun id -> Task.pure(`String(id)))</script>|})
    |> ok
  in
  let expected =
    match
      Agent_client.Connection.request_without_history
        transport
        (Authoring (Context_open request))
    with
    | Ok (Authoring (Context_open context)) ->
      P.Authoring.Context.Expected.of_view context
    | Ok _ -> failwith "unexpected context response"
    | Error error -> raise_s [%sexp (error : P.Error.t)]
  in
  let analyze =
    P.Authoring.Analysis.Analyze.Request.create
      ~context:expected
      ~target:(P.Authoring.Analysis.Target.selected_script analysis_script |> ok)
    |> ok
  in
  (match
     Agent_client.Connection.request_without_history
       transport
       (Authoring (Analyze analyze))
   with
   | Ok (Authoring (Analyze _)) -> ()
   | Ok _ -> failwith "unexpected analysis response"
   | Error error -> raise_s [%sexp (error : P.Error.t)]);
  connection, expected
;;

let selected_tools_response ?(include_undeclared = true) () =
  let call name call_id arguments =
    `Object
      [ "type", `String "function_call"
      ; "id", `String ("item-" ^ call_id)
      ; "call_id", `String call_id
      ; "name", `String name
      ; "arguments", `String arguments
      ; "status", `String "completed"
      ]
  in
  let output =
    [ call "read_file" "read-child-file" {|{"file":"sample.txt"}|}
    ; call "summary" "managed-child-summary" {|{"message":"go"}|}
    ]
    @
    if include_undeclared
    then [ call "undeclared" "managed-child-undeclared" {|{"file":"sample.txt"}|} ]
    else []
  in
  let event =
    `Object
      [ "type", `String "response.completed"
      ; "sequence_number", `Number "1"
      ; ( "response"
        , `Object
            [ "object", `String "response"
            ; "id", `String "selected-child-tools"
            ; "status", `String "completed"
            ; "output", `Array output
            ] )
      ]
  in
  "data: " ^ Jsonaf.to_string event ^ "\n\n"
;;

let%expect_test "hosted captured Agent child uses its saved source and tracked model" =
  let requests = ref 0 in
  let model_bodies = ref [] in
  let deep_tools_sent = ref false in
  let selected_parent_submitting = ref false in
  let worker_two_submitting = ref false in
  let control_submitting = ref false in
  let first_entered, first_entered_u = Eio.Promise.create () in
  let second_entered, second_entered_u = Eio.Promise.create () in
  let selected_parent_entered, selected_parent_entered_u = Eio.Promise.create () in
  let release_first, release_first_u = Eio.Promise.create () in
  let release_selected_parent, release_selected_parent_u = Eio.Promise.create () in
  let worker_two_entered, worker_two_entered_u = Eio.Promise.create () in
  let release_worker_two, release_worker_two_u = Eio.Promise.create () in
  let first_released = ref false in
  let selected_parent_released = ref false in
  let worker_two_released = ref false in
  RH.with_fixture
    ~model_requests:requests
    ~model_function_tools:true
    ~model_image_input:true
    ~model_reasoning:true
    ~response_body:(fun number body ->
      if Int.equal number 3
      then Some (selected_tools_response ())
      else if
        String.is_substring body ~substring:"Deep level three instructions"
        && not !deep_tools_sent
      then (
        deep_tools_sent := true;
        Some (selected_tools_response ~include_undeclared:false ()))
      else None)
    ~on_model_body:(fun body ->
      model_bodies := body :: !model_bodies;
      if String.is_substring body ~substring:"Selected Model parent instructions"
      then selected_parent_submitting := true;
      if String.is_substring body ~substring:"Selected worker two instructions"
      then worker_two_submitting := true;
      if String.is_substring body ~substring:"Selected control instructions"
      then control_submitting := true)
    ~on_model_submit:(fun () ->
      if Int.equal !requests 1
      then (
        Eio.Promise.resolve first_entered_u ();
        Eio.Promise.await release_first)
      else if Int.equal !requests 2
      then Eio.Promise.resolve second_entered_u ()
      else if !selected_parent_submitting
      then (
        selected_parent_submitting := false;
        Eio.Promise.resolve selected_parent_entered_u ();
        Eio.Promise.await release_selected_parent)
      else if !worker_two_submitting
      then (
        worker_two_submitting := false;
        Eio.Promise.resolve worker_two_entered_u ();
        Eio.Promise.await release_worker_two)
      else if !control_submitting
      then (
        control_submitting := false;
        Eio.Promise.await worker_two_entered))
    (fun env sw anchor create _ ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60. (fun () ->
         Fun.protect
           ~finally:(fun () ->
             if not !first_released then Eio.Promise.resolve release_first_u ();
             if not !selected_parent_released
             then Eio.Promise.resolve release_selected_parent_u ();
             if not !worker_two_released then Eio.Promise.resolve release_worker_two_u ())
           (fun () ->
              let bootstrap = create () in
              ignore (RH.setup bootstrap : P.Method_result.t);
              let reads = ref 0 in
              ignore
                (RH.enroll bootstrap ~sw "one" "hosted-bootstrap-key" reads
                 : P.Provider_operator.Configuration_result.t);
              let target = RH.capture bootstrap in
              Provider_runtime.close bootstrap;
              let root = Eio.Path.native_exn anchor in
              let remote_listener =
                Eio.Net.listen
                  ~sw
                  ~reuse_addr:true
                  ~backlog:4
                  (Eio.Stdenv.net env)
                  (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
              in
              let remote_port =
                match Eio.Net.listening_addr remote_listener with
                | `Tcp (_, port) -> port
                | _ -> failwith "remote child fixture did not bind"
              in
              let remote_requests = ref 0 in
              Eio.Fiber.fork_daemon ~sw (fun () ->
                while true do
                  let flow, _ = Eio.Net.accept ~sw remote_listener in
                  Eio.Fiber.fork_daemon ~sw (fun () ->
                    Exn.protect
                      ~finally:(fun () -> Eio.Flow.close flow)
                      ~f:(fun () ->
                        let reader = Eio.Buf_read.of_flow flow ~max_size:1024 in
                        ignore (Eio.Buf_read.line reader : string);
                        let rec headers () =
                          if String.is_empty (Eio.Buf_read.line reader)
                          then ()
                          else headers ()
                        in
                        headers ();
                        incr remote_requests;
                        Eio.Flow.copy_string
                          "HTTP/1.1 200 OK\r\n\
                           Content-Length: 27\r\n\
                           Connection: close\r\n\
                           \r\n\
                           remote selected child bytes"
                          flow;
                        `Stop_daemon))
                done);
              let remote_origin = sprintf "http://127.0.0.1:%d" remote_port in
              let remote_policy =
                S.Daemon.Developer.Configuration.Remote_content.create
                  ~allowed_origins:[ remote_origin ]
                  ~max_response_bytes:1024
                  ~max_wall_seconds:5.
                |> Or_error.ok_exn
              in
              let prompt_dir = Filename.concat root "prompts" in
              Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / prompt_dir);
              let prompt = Filename.concat prompt_dir "prompt.chatmd" in
              let child = Filename.concat prompt_dir "child.chatmd" in
              let nested_dir = Filename.concat prompt_dir "agents" in
              Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / nested_dir);
              let tool_child = Filename.concat nested_dir "child.chatmd" in
              let deep_one = Filename.concat nested_dir "deep-one.chatmd" in
              let deep_two = Filename.concat nested_dir "deep-two.chatmd" in
              let deep_three = Filename.concat nested_dir "deep-three.chatmd" in
              let model_child = Filename.concat nested_dir "model-child.chatmd" in
              let model_grand = Filename.concat nested_dir "model-grand.chatmd" in
              let model_mixed_parent =
                Filename.concat nested_dir "model-mixed-parent.chatmd"
              in
              let model_mixed = Filename.concat nested_dir "model-mixed.chatmd" in
              let model_content = Filename.concat nested_dir "model-content.chatmd" in
              let model_last = Filename.concat nested_dir "model-last.chatmd" in
              let model_spawn_parent =
                Filename.concat nested_dir "model-spawn-parent.chatmd"
              in
              let model_worker_one =
                Filename.concat nested_dir "model-worker-one.chatmd"
              in
              let model_worker_two =
                Filename.concat nested_dir "model-worker-two.chatmd"
              in
              let model_control = Filename.concat nested_dir "model-control.chatmd" in
              let notes_dir = Filename.concat prompt_dir "notes" in
              Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / notes_dir);
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / notes_dir / "d.txt")
                "selected child original document bytes";
              let image_bytes =
                Base64.decode_exn
                  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQIHWP4z8DwHwAFgAI/ScL/nwAAAABJRU5ErkJggg=="
              in
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / notes_dir / "p.png")
                image_bytes;
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / prompt)
                {|<tool name="child" agent="child.chatmd" local source_mode="file"/>
<tool name="child_tools" agent="agents/child.chatmd" local source_mode="file"/>
<tool name="deep" agent="agents/deep-one.chatmd" local source_mode="file"/>
<tool name="model_child" agent="agents/model-child.chatmd" local source_mode="file"/>
<tool name="model_mixed" agent="agents/model-mixed-parent.chatmd" local source_mode="file"/>
<tool name="model_spawn" agent="agents/model-spawn-parent.chatmd" local source_mode="file"/>|};
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / child)
                "<developer>Original hosted child instructions</developer>";
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / tool_child)
                (String.concat
                   [ {|<developer>Call read_file and summary.</developer>
<user><doc src="../notes/d.txt" local/><img src="../notes/p.png" local/></user>
<user><doc src="|}
                   ; remote_origin
                   ; {|/asset"/></user>
<tool name="read_file"/>
<script id="owned" language="chatml" kind="tool">
let run ctx input = Task.pure(`Complete(input))
</script>
<script id="unowned" language="chatml" kind="tool">
let run ctx input = Task.bind(Tool.call("read_file", input), fun result ->
  match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(code) -> Task.pure(`Fail({code = code; message = "undeclared child call"; retryable = false; details = `Null})))
</script>
<tool name="summary" type="chatml" script="owned" entrypoint="run" input_schema="input.json" output_schema="output.json"/>
<tool name="undeclared" type="chatml" script="unowned" entrypoint="run" input_schema="input.json" output_schema="output.json"/>|}
                   ]);
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / deep_one)
                {|<developer>Deep level one instructions</developer>
<user><agent src="deep-two.chatmd" local>caller level one</agent></user>|};
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / deep_two)
                {|<developer>Deep level two instructions</developer>
<user><agent src="deep-three.chatmd" local source_mode="captured">caller level two</agent></user>|};
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / deep_three)
                {|<developer>Deep level three instructions</developer>
<user>deep selected native and managed effects</user>
<tool name="read_file"/>
<script id="owned" language="chatml" kind="tool">
let run ctx input = Task.pure(`Complete(input))
</script>
<tool name="summary" type="chatml" script="owned" entrypoint="run" input_schema="input.json" output_schema="output.json"/>|};
              let model_payload =
                `Object
                  [ "prompt", `String "model-grand.chatmd"
                  ; "input", `String "from selected Model.call"
                  ; "is_local", `True
                  ; "source_mode", `String "file"
                  ]
                |> Jsonaf.to_string
              in
              let denied_model_payload =
                `Object
                  [ "prompt", `String "../../../escape.chatmd"
                  ; "input", `String "pre-acquisition denial"
                  ; "is_local", `True
                  ; "source_mode", `String "file"
                  ]
                |> Jsonaf.to_string
              in
              let missing_model_payload =
                `Object
                  [ "prompt", `String "model-missing.chatmd"
                  ; "input", `String "post-acquisition denial"
                  ; "is_local", `True
                  ; "source_mode", `String "file"
                  ]
                |> Jsonaf.to_string
              in
              let captured_model_payload =
                `Object
                  [ "prompt", `String "model-grand.chatmd"
                  ; "input", `String "from retained selected Model.call"
                  ; "is_local", `True
                  ; "source_mode", `String "captured"
                  ]
                |> Jsonaf.to_string
              in
              let moderator_script =
                sprintf
                  {|
  type state = int
  type event = [ `Session_start | `Session_resume | `Turn_start | `Turn_end | `Item_appended(item) ]
  let initial_state = 0
  let on_event : context -> state -> event -> state task =
    fun ctx st ev ->
      match ev with
      | `Session_start ->
        Task.bind(Model.call("agent_prompt_v1", Json.parse(%S)), fun denied ->
          match denied with
          | `Error(_) ->
            Task.bind(Model.call("agent_prompt_v1", Json.parse(%S)), fun valid ->
              match valid with
              | `Ok(_) -> Task.pure(st + 1)
              | `Error(message) -> Task.fail(message)
              | `Refused(message) -> Task.fail(message))
          | `Ok(_) -> Task.fail("invalid selector was admitted")
          | `Refused(_) -> Task.fail("invalid selector was refused"))
      | `Item_appended(_) ->
        if st == 1
        then
          Task.bind(Model.call("agent_prompt_v1", Json.parse(%S)), fun retained ->
            match retained with
            | `Ok(_) ->
              Task.bind(Model.call("agent_prompt_v1", Json.parse(%S)), fun missing ->
                match missing with
                | `Error(_) ->
                  Task.bind(Model.call("agent_prompt_v1", Json.parse(%S)), fun retry ->
                    match retry with
                    | `Error(_) ->
                      Task.bind(Runtime.end_session("parent-complete"), fun ignored -> Task.pure(st))
                    | `Ok(_) -> Task.fail("uncertain read allowed retry")
                    | `Refused(_) -> Task.fail("uncertain read was refused"))
                | `Ok(_) -> Task.fail("missing File source was admitted")
                | `Refused(_) -> Task.fail("missing File source was refused"))
            | `Error(message) -> Task.fail(message)
            | `Refused(message) -> Task.fail(message))
        else Task.pure(st)
      | `Session_resume -> Task.pure(st)
      | `Turn_start -> Task.pure(st)
      | `Turn_end -> Task.pure(st)|}
                  denied_model_payload
                  model_payload
                  captured_model_payload
                  missing_model_payload
                  captured_model_payload
              in
              let check_moderator id source =
                let script : Prompt.Chat_markdown.script =
                  { id
                  ; language = "chatml"
                  ; project_selection = None
                  ; kind = "moderator"
                  ; source_ref = None
                  ; source = Inline source
                  }
                in
                Chat_response.Moderator_manager.Registry.compile_script
                  Chat_response.Moderator_manager.Registry.empty
                  script
                |> Result.ok_or_failwith
                |> ignore
              in
              check_moderator "model-child-moderator" moderator_script;
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / model_child)
                (sprintf
                   {|<config model="synthetic" reasoning_effort="high"/>
<developer>Selected Model parent instructions</developer>
<user><agent src="model-grand.chatmd" local source_mode="captured">from selected Model content</agent></user>
<script language="chatml" kind="moderator">%s</script>|}
                   moderator_script);
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / model_grand)
                "<developer>Selected Model grandchild instructions</developer>";
              let mixed_payload =
                `Object
                  [ "prompt", `String "model-mixed.chatmd"
                  ; "input", `String "enter mixed Model member"
                  ; "is_local", `True
                  ; "source_mode", `String "file"
                  ]
                |> Jsonaf.to_string
              in
              let mixed_parent_script =
                sprintf
                  {|type state = int
type event = [ `Session_start ]
let initial_state = 0
let on_event : context -> state -> event -> state task =
  fun ctx st ev ->
    match ev with
    | `Session_start ->
      Task.bind(Model.call("agent_prompt_v1", Json.parse(%S)), fun result ->
        match result with
        | `Ok(_) -> Task.bind(Runtime.end_session("mixed-complete"), fun ignored -> Task.pure(st + 1))
        | `Error(message) -> Task.fail(message)
        | `Refused(message) -> Task.fail(message))|}
                  mixed_payload
              in
              check_moderator "mixed-parent-moderator" mixed_parent_script;
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / model_mixed_parent)
                (sprintf
                   {|<config model="synthetic-mixed-parent" reasoning_effort="high"/>
<developer>Mixed saved parent instructions</developer>
<script language="chatml" kind="moderator">%s</script>|}
                   mixed_parent_script);
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / model_mixed)
                {|<config model="synthetic-mixed-outer" reasoning_effort="medium"/>
<developer>Mixed Model member instructions</developer>
<user><agent src="model-content.chatmd" local source_mode="file">mixed caller content</agent></user>|};
              let last_payload =
                `Object
                  [ "prompt", `String "model-last.chatmd"
                  ; "input", `String "from mixed Content Model.call"
                  ; "is_local", `True
                  ; "source_mode", `String "file"
                  ]
                |> Jsonaf.to_string
              in
              let content_script =
                sprintf
                  {|type state = int
type event = [ `Session_start | `Session_resume | `Turn_start | `Item_appended(item) | `Turn_end ]
let initial_state = 0
let on_event : context -> state -> event -> state task =
  fun ctx st ev ->
    match ev with
    | `Session_start ->
      Task.bind(Model.call("agent_prompt_v1", Json.parse(%S)), fun result ->
        match result with
        | `Ok(_) -> Task.pure(st + 1)
        | `Error(message) -> Task.fail(message)
        | `Refused(message) -> Task.fail(message))
    | `Session_resume -> Task.pure(st)
    | `Turn_start -> Task.pure(st)
    | `Item_appended(_) -> Task.pure(st)
    | `Turn_end -> Task.pure(st)|}
                  last_payload
              in
              check_moderator "mixed-content-moderator" content_script;
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / model_content)
                (sprintf
                   {|<config model="synthetic-mixed-content" reasoning_effort="medium"/>
<developer>Mixed Content member instructions</developer>
<script language="chatml" kind="moderator">%s</script>|}
                   content_script);
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / model_last)
                {|<config model="synthetic-mixed-leaf" reasoning_effort="low"/>
<developer>Mixed last Model member instructions</developer>|};
              let model_payload member input =
                `Object
                  [ "prompt", `String member
                  ; "input", `String input
                  ; "is_local", `True
                  ; "source_mode", `String "file"
                  ]
                |> Jsonaf.to_string
              in
              let spawn_script =
                sprintf
                  {|type state = int
type event = [ `Session_start | `Session_resume | `Turn_start | `Item_appended(item) | `Turn_end | `Model_job_succeeded(string, string, json) | `Model_job_failed(string, string, string) ]
let initial_state = 0
let on_event : context -> state -> event -> state task =
  fun ctx st ev ->
    match ev with
    | `Session_start ->
      Task.bind(Model.spawn("agent_prompt_v1", Json.parse(%S)), fun first_job -> Task.pure(1))
    | `Model_job_succeeded(_, _, _) ->
      if st == 1
      then
        Task.bind(Model.spawn("agent_prompt_v1", Json.parse(%S)), fun second_job ->
          Task.bind(Model.call("agent_prompt_v1", Json.parse(%S)), fun control ->
            match control with
            | `Ok(_) -> Task.bind(Runtime.end_session("spawn-complete"), fun ignored -> Task.pure(2))
            | `Error(message) -> Task.fail(message)
            | `Refused(message) -> Task.fail(message)))
      else Task.pure(st)
    | `Model_job_failed(_, _, message) -> Task.fail(message)
    | `Session_resume -> Task.pure(st)
    | `Turn_start -> Task.pure(st)
    | `Item_appended(_) -> Task.pure(st)
    | `Turn_end -> Task.pure(st)|}
                  (model_payload
                     "model-worker-one.chatmd"
                     "first selected spawned worker")
                  (model_payload
                     "model-worker-two.chatmd"
                     "second selected spawned worker")
                  (model_payload "model-control.chatmd" "control selected Model.call")
              in
              check_moderator "spawn-parent-moderator" spawn_script;
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / model_spawn_parent)
                (sprintf
                   {|<config model="synthetic-spawn-parent" reasoning_effort="high"/>
<developer>Selected spawn parent instructions</developer>
<script language="chatml" kind="moderator">%s</script>|}
                   spawn_script);
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / model_worker_one)
                {|<config model="synthetic-worker-one" reasoning_effort="low"/>
<developer>Selected worker one instructions</developer>|};
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / model_worker_two)
                {|<config model="synthetic-worker-two" reasoning_effort="high"/>
<developer>Selected worker two instructions</developer>|};
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / model_control)
                {|<config model="synthetic-control" reasoning_effort="medium"/>
<developer>Selected control instructions</developer>|};
              List.iter [ "input.json"; "output.json" ] ~f:(fun name ->
                Eio.Path.save
                  ~create:(`Or_truncate 0o600)
                  Eio.Path.(Eio.Stdenv.fs env / nested_dir / name)
                  {|{"type":"object"}|});
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / root / "sample.txt")
                "selected child file contents";
              let provider_anchor = Eio.Path.(anchor / "daemon-provider") in
              Eio.Path.mkdir ~perm:0o700 provider_anchor;
              let provider_factory ~sw:provider_sw ~server_id:provider_server_id =
                let runtime =
                  create
                    ~runtime_sw:provider_sw
                    ~runtime_server_id:provider_server_id
                    ~runtime_anchor:provider_anchor
                    ()
                in
                ignore (RH.setup runtime : P.Method_result.t);
                ignore
                  (RH.enroll runtime ~sw:provider_sw "one" "hosted-daemon-key" reads
                   : P.Provider_operator.Configuration_result.t);
                assert (Inference.Request.Target.equal target (RH.capture runtime));
                Ok (Provider_runtime.operator_port runtime)
              in
              let scopes = Set.add Agent_server_test_support.scopes Developer_live_run in
              let principal =
                Agent_server_test_support.principal_with_scopes "pri_operator" scopes
              in
              let actor = Operator_authorization.trusted_local principal in
              let config = Agent_server_test_support.config root root prompt in
              let options =
                { S.Daemon.default_options with
                  authoring = Some (DH.semantic_authoring_policy env)
                ; developer =
                    Some (developer_configuration ~remote_content:remote_policy target)
                ; provider_operator_factory = Some provider_factory
                }
              in
              let daemon =
                S.Daemon.start
                  ~sw
                  ~env
                  ~config
                  ~tool_dir:root
                  ~home:root
                  ~process_start_identity:None
                  ~options
                  ()
                |> ok
              in
              let session = S.Daemon.store daemon in
              let data_root =
                Agent_store.Data_root.path (Agent_store.Session_store.data_root session)
              in
              let _connection, expected = open_document daemon actor in
              let run_id = P.Id.Developer_run.create () in
              let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
              let bounds =
                Chatml_developer_capture.Json_bounds.create
                  ~max_bytes:4096
                  ~max_depth:16
                  ~max_nodes:128
                |> Or_error.ok_exn
              in
              let request =
                S.Daemon.Developer.Request.create
                  ~route:(Document_script { expected; selector })
                  ~kind:Live_run
                  ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
                  ~prompt:(Some (S.Catalog_identity.prompt_definition "restart.prompt"))
                  ~workspace:
                    (Some (S.Catalog_identity.workspace_definition "restart.workspace"))
                  ~entrypoint:"main"
                  ~input:(`Object [ "input", `String "go" ])
                  ~fixtures:None
                  ~requested_execution:None
                  ~requested_max_output_bytes:None
                  ~bounds
                  ~checkpoint:ignore
                |> ok
              in
              let key = P.Idempotency_key.of_string "hosted-captured-agent" |> ok in
              let submitted =
                S.Daemon.Developer.submit
                  daemon
                  ~actor
                  ~run_id
                  ~key
                  ~request
                  ~fresh_source:(Some (Document_script { connection = _connection }))
                |> ok
              in
              ignore (submitted : S.Daemon.Developer.View.t);
              let live =
                S.Daemon.Developer.await_terminal daemon ~actor ~run_id ~key |> ok
              in
              let first_id, second_id =
                match Option.map (S.Daemon.Developer.View.receipt live) ~f:R.value with
                | Some
                    (Live
                       (Succeeded
                          (`Object
                             [ ("first", `String first); ("second", `String second) ])))
                  -> P.Id.Job.of_string first |> ok, P.Id.Job.of_string second |> ok
                | result ->
                  raise_s
                    [%sexp "expected two saved child IDs", (result : R.value option)]
              in
              assert (not (P.Id.Job.equal first_id second_id));
              Eio.Promise.await first_entered;
              let payload ?(run = run_id) () =
                let path =
                  Eio.Path.(
                    Eio.Stdenv.fs env
                    / data_root
                    / "developer-runs"
                    / ("run_" ^ P.Id.Developer_run.to_string run ^ ".json"))
                in
                match Eio.Path.load path |> Jsonaf.parse |> Or_error.ok_exn with
                | `Object fields ->
                  (match List.Assoc.find_exn fields ~equal:String.equal "payload" with
                   | `Object fields -> fields
                   | _ -> failwith "invalid run payload")
                | _ -> failwith "invalid run envelope"
              in
              let rows ?(run = run_id) field =
                match List.Assoc.find_exn (payload ~run ()) ~equal:String.equal field with
                | `Array rows -> rows
                | _ -> failwith "invalid run rows"
              in
              let jobs ?(run = run_id) () =
                List.map (rows ~run "jobs") ~f:(fun json ->
                  P.Developer_job.of_json json
                  |> Result.map_error ~f:(fun error ->
                    Sexp.to_string_hum [%sexp (error : P.Protocol_error.t)])
                  |> Result.ok_or_failwith)
              in
              let job ?(run = run_id) id =
                List.find_exn (jobs ~run ()) ~f:(fun value ->
                  P.Id.Job.equal (P.Developer_job.id value) id)
              in
              let queued_second = job second_id in
              (match (P.Developer_job.state queued_second).status with
               | Queued -> ()
               | Running
               | Waiting_permission _
               | Waiting_completion _
               | Succeeded
               | Failed _
               | Cancelled
               | Interrupted _ -> failwith "saved second child was not queued");
              let saved_request =
                Chat_response.Background_request.of_json
                  ~policy:Chat_response.One_off_request.default_policy
                  (P.Developer_job.payload queued_second)
                |> ok
              in
              assert (
                Option.is_some
                  (Chat_response.Background_request.agent_artifact saved_request));
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / child)
                "<developer>Edited hosted child instructions</developer>";
              first_released := true;
              Eio.Promise.resolve release_first_u ();
              Eio.Promise.await second_entered;
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
                let rec await_both () =
                  match
                    ( (P.Developer_job.state (job first_id)).status
                    , (P.Developer_job.state (job second_id)).status )
                  with
                  | Succeeded, Succeeded -> ()
                  | Failed _, _
                  | _, Failed _
                  | Cancelled, _
                  | _, Cancelled
                  | Interrupted _, _
                  | _, Interrupted _ ->
                    failwith "hosted child did not complete successfully"
                  | _ ->
                    Eio.Fiber.yield ();
                    await_both ()
                in
                await_both ());
              assert (Int.equal !requests 2);
              let inference = rows "inference_attempts" in
              assert (Int.equal (List.length inference) 2);
              let inference_jobs =
                List.map inference ~f:(fun row ->
                  let fields =
                    match row with
                    | `Object fields -> fields
                    | _ -> failwith "invalid inference row"
                  in
                  let field name = List.Assoc.find_exn fields ~equal:String.equal name in
                  let job_id = P.Id.Job.of_json (field "job_id") |> ok in
                  let job_attempt =
                    match field "job_attempt" with
                    | `String value -> Int.of_string value
                    | _ -> failwith "invalid inference job attempt"
                  in
                  [%test_eq: int] (P.Developer_job.state (job job_id)).attempt job_attempt;
                  let record =
                    Inference.Observation.Attempt_record.of_json
                      (field "record")
                      ~limits:Inference.Observation.Admission.attempt
                    |> Result.map_error ~f:(fun _ -> "invalid inference attempt")
                    |> Result.ok_or_failwith
                  in
                  match Inference.Observation.Attempt_record.state record with
                  | Terminal terminal ->
                    (match Inference.Event.Terminal.outcome terminal with
                     | Completed -> job_id
                     | Refused | Incomplete _ | Failed _ ->
                       failwith "hosted inference did not complete")
                  | Prepared | Running | Interrupted _ ->
                    failwith "hosted inference attempt was not terminal")
              in
              assert (List.count inference_jobs ~f:(P.Id.Job.equal first_id) = 1);
              assert (List.count inference_jobs ~f:(P.Id.Job.equal second_id) = 1);
              assert (Int.equal (List.length !model_bodies) 2);
              List.iter !model_bodies ~f:(fun body ->
                assert (
                  String.is_substring body ~substring:"Original hosted child instructions");
                assert (
                  not
                    (String.is_substring
                       body
                       ~substring:"Edited hosted child instructions")));
              let tool_request =
                S.Daemon.Developer.Request.create
                  ~route:
                    (Document_script
                       { expected
                       ; selector =
                           P.Authoring.Analysis.Target.selected_script "tool_child" |> ok
                       })
                  ~kind:Live_run
                  ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
                  ~prompt:(Some (S.Catalog_identity.prompt_definition "restart.prompt"))
                  ~workspace:
                    (Some (S.Catalog_identity.workspace_definition "restart.workspace"))
                  ~entrypoint:"main"
                  ~input:(`Object [ "input", `String "go" ])
                  ~fixtures:None
                  ~requested_execution:None
                  ~requested_max_output_bytes:None
                  ~bounds
                  ~checkpoint:ignore
                |> ok
              in
              let tool_key =
                P.Idempotency_key.of_string "hosted-selected-child-tools" |> ok
              in
              let tool_run_id = P.Id.Developer_run.create () in
              ignore
                (S.Daemon.Developer.submit
                   daemon
                   ~actor
                   ~run_id:tool_run_id
                   ~key:tool_key
                   ~request:tool_request
                   ~fresh_source:(Some (Document_script { connection = _connection }))
                 |> ok
                 : S.Daemon.Developer.View.t);
              let tool_view =
                S.Daemon.Developer.await_terminal
                  daemon
                  ~actor
                  ~run_id:tool_run_id
                  ~key:tool_key
                |> ok
              in
              let tool_job =
                match
                  Option.map (S.Daemon.Developer.View.receipt tool_view) ~f:R.value
                with
                | Some (Live (Succeeded (`String id))) -> P.Id.Job.of_string id |> ok
                | result ->
                  raise_s [%sexp "expected selected child Job", (result : R.value option)]
              in
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
                let rec await_tool () =
                  match
                    (P.Developer_job.state (job ~run:tool_run_id tool_job)).status
                  with
                  | Succeeded -> ()
                  | Failed _ ->
                    failwithf
                      "selected child failed after %d model requests: %s"
                      !requests
                      (P.Developer_job.to_json (job ~run:tool_run_id tool_job)
                       |> Jsonaf.to_string)
                      ()
                  | Cancelled | Interrupted _ -> failwith "selected child stopped"
                  | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
                    Eio.Fiber.yield ();
                    await_tool ()
                in
                await_tool ());
              assert (Int.equal !requests 4);
              (match List.rev !model_bodies with
               | [ _first; _second; tool_request; tool_result ] ->
                 let field value name =
                   match value with
                   | `Object fields -> List.Assoc.find_exn fields ~equal:String.equal name
                   | _ -> failwith "expected provider request object"
                 in
                 let string = function
                   | `String value -> value
                   | _ -> failwith "expected provider string"
                 in
                 let array = function
                   | `Array values -> values
                   | _ -> failwith "expected provider array"
                 in
                 let request = Jsonaf.of_string tool_request in
                 let names =
                   field request "tools"
                   |> array
                   |> List.map ~f:(fun tool -> field tool "name" |> string)
                 in
                 assert (
                   List.equal String.equal names [ "read_file"; "summary"; "undeclared" ]);
                 assert (
                   String.is_substring
                     tool_request
                     ~substring:"Call read_file and summary");
                 assert (
                   String.is_substring
                     tool_request
                     ~substring:"selected child original document bytes");
                 assert (
                   String.is_substring
                     tool_request
                     ~substring:"remote selected child bytes");
                 assert (
                   String.is_substring
                     tool_request
                     ~substring:("data:image/png;base64," ^ Base64.encode_exn image_bytes));
                 assert (Int.equal !remote_requests 1);
                 let outputs =
                   field (Jsonaf.of_string tool_result) "input"
                   |> array
                   |> List.filter ~f:(fun item ->
                     match field item "type" with
                     | `String "function_call_output" -> true
                     | `String _ -> false
                     | _ -> failwith "invalid provider input type")
                 in
                 (match outputs with
                  | [ read; managed; denied ] ->
                    assert (
                      String.equal (field read "call_id" |> string) "read-child-file");
                    assert (
                      String.equal
                        (field managed "call_id" |> string)
                        "managed-child-summary");
                    assert (
                      String.equal
                        (field denied "call_id" |> string)
                        "managed-child-undeclared");
                    let read = field read "output" |> string |> Jsonaf.of_string in
                    let managed = field managed "output" |> string |> Jsonaf.of_string in
                    let denied = field denied "output" |> string |> Jsonaf.of_string in
                    assert (String.equal (field read "type" |> string) "complete");
                    assert (
                      String.is_substring
                        (field read "value" |> string)
                        ~substring:"selected child file contents");
                    if not (String.equal (field managed "type" |> string) "complete")
                    then failwithf "managed result: %s" (Jsonaf.to_string managed) ();
                    assert (
                      String.equal
                        (field (field managed "value") "message" |> string)
                        "go");
                    assert (String.equal (field denied "type" |> string) "fail");
                    assert (
                      String.equal
                        (field denied "code" |> string)
                        "invocation.handler_failed");
                    assert (
                      String.equal
                        (field denied "message" |> string)
                        "tool is not in the selected capability set")
                  | _ -> failwith "selected child returned unexpected tool results")
               | _ -> failwith "selected child model request count changed");
              let deep_request =
                S.Daemon.Developer.Request.create
                  ~route:
                    (Document_script
                       { expected
                       ; selector =
                           P.Authoring.Analysis.Target.selected_script "deep_child" |> ok
                       })
                  ~kind:Live_run
                  ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
                  ~prompt:(Some (S.Catalog_identity.prompt_definition "restart.prompt"))
                  ~workspace:
                    (Some (S.Catalog_identity.workspace_definition "restart.workspace"))
                  ~entrypoint:"main"
                  ~input:(`Object [ "input", `String "deep" ])
                  ~fixtures:None
                  ~requested_execution:None
                  ~requested_max_output_bytes:None
                  ~bounds
                  ~checkpoint:ignore
                |> ok
              in
              let deep_run_id = P.Id.Developer_run.create () in
              let deep_key = P.Idempotency_key.of_string "hosted-deep-content" |> ok in
              ignore
                (S.Daemon.Developer.submit
                   daemon
                   ~actor
                   ~run_id:deep_run_id
                   ~key:deep_key
                   ~request:deep_request
                   ~fresh_source:(Some (Document_script { connection = _connection }))
                 |> ok
                 : S.Daemon.Developer.View.t);
              let deep_view =
                S.Daemon.Developer.await_terminal
                  daemon
                  ~actor
                  ~run_id:deep_run_id
                  ~key:deep_key
                |> ok
              in
              let deep_job =
                match
                  Option.map (S.Daemon.Developer.View.receipt deep_view) ~f:R.value
                with
                | Some (Live (Succeeded (`String id))) -> P.Id.Job.of_string id |> ok
                | result ->
                  raise_s [%sexp "expected deep content Job", (result : R.value option)]
              in
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
                let rec await_deep () =
                  match
                    (P.Developer_job.state (job ~run:deep_run_id deep_job)).status
                  with
                  | Succeeded -> ()
                  | Failed _ ->
                    failwithf
                      "deep child failed after %d model requests: %s"
                      !requests
                      (P.Developer_job.to_json (job ~run:deep_run_id deep_job)
                       |> Jsonaf.to_string)
                      ()
                  | Cancelled | Interrupted _ -> failwith "deep child stopped"
                  | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
                    Eio.Fiber.yield ();
                    await_deep ()
                in
                await_deep ());
              assert !deep_tools_sent;
              let deep_bodies = List.drop (List.rev !model_bodies) 4 in
              assert (List.length deep_bodies >= 4);
              assert (
                List.exists deep_bodies ~f:(fun body ->
                  String.is_substring body ~substring:"Deep level two instructions"
                  && String.is_substring body ~substring:"caller level one"));
              assert (
                List.exists deep_bodies ~f:(fun body ->
                  String.is_substring body ~substring:"Deep level three instructions"
                  && String.is_substring body ~substring:"caller level two"));
              let field value name =
                match value with
                | `Object fields -> List.Assoc.find_exn fields ~equal:String.equal name
                | _ -> failwith "deep provider request is not an object"
              in
              let string = function
                | `String value -> value
                | _ -> failwith "deep provider field is not a string"
              in
              let deep_result =
                List.find_exn deep_bodies ~f:(fun body ->
                  String.is_substring body ~substring:"function_call_output")
                |> Jsonaf.of_string
              in
              let outputs =
                match field deep_result "input" with
                | `Array items ->
                  List.filter items ~f:(fun item ->
                    match field item "type" with
                    | `String "function_call_output" -> true
                    | `String _ -> false
                    | _ -> failwith "invalid deep provider input type")
                | _ -> failwith "deep provider input is not an array"
              in
              (match outputs with
               | [ read; managed ] ->
                 [%test_eq: string] (field read "call_id" |> string) "read-child-file";
                 [%test_eq: string]
                   (field managed "call_id" |> string)
                   "managed-child-summary";
                 let read = field read "output" |> string |> Jsonaf.of_string in
                 let managed = field managed "output" |> string |> Jsonaf.of_string in
                 [%test_eq: string] (field read "type" |> string) "complete";
                 assert (
                   String.is_substring
                     (field read "value" |> string)
                     ~substring:"selected child file contents");
                 [%test_eq: string] (field managed "type" |> string) "complete";
                 [%test_eq: string]
                   (field (field managed "value") "message" |> string)
                   "go"
               | _ -> failwith "grandchild returned unexpected native/managed results");
              let model_requests_before_denial = !requests in
              let jobs_before_denial = List.length (jobs ~run:deep_run_id ()) in
              Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / deep_two);
              let missing_key = P.Idempotency_key.of_string "hosted-deep-missing" |> ok in
              ignore
                (S.Daemon.Developer.submit
                   daemon
                   ~actor
                   ~run_id:deep_run_id
                   ~key:missing_key
                   ~request:deep_request
                   ~fresh_source:(Some (Document_script { connection = _connection }))
                 |> ok
                 : S.Daemon.Developer.View.t);
              let missing_view =
                S.Daemon.Developer.await_terminal
                  daemon
                  ~actor
                  ~run_id:deep_run_id
                  ~key:missing_key
                |> ok
              in
              (match
                 Option.map (S.Daemon.Developer.View.receipt missing_view) ~f:R.value
               with
               | Some (Live (Failed error)) ->
                 assert (P.Error.equal_code error.code P.Error.Invalid_request)
               | result ->
                 raise_s
                   [%sexp
                     "missing nested File source was not denied"
                   , (result : R.value option)]);
              [%test_eq: int] !requests model_requests_before_denial;
              [%test_eq: int] (List.length (jobs ~run:deep_run_id ())) jobs_before_denial;
              let requests_before_model = !requests in
              let model_request =
                S.Daemon.Developer.Request.create
                  ~route:
                    (Document_script
                       { expected
                       ; selector =
                           P.Authoring.Analysis.Target.selected_script "model_child" |> ok
                       })
                  ~kind:Live_run
                  ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
                  ~prompt:(Some (S.Catalog_identity.prompt_definition "restart.prompt"))
                  ~workspace:
                    (Some (S.Catalog_identity.workspace_definition "restart.workspace"))
                  ~entrypoint:"main"
                  ~input:(`Object [ "input", `String "model" ])
                  ~fixtures:None
                  ~requested_execution:None
                  ~requested_max_output_bytes:None
                  ~bounds
                  ~checkpoint:ignore
                |> ok
              in
              let model_run_id = P.Id.Developer_run.create () in
              let model_key = P.Idempotency_key.of_string "hosted-selected-model" |> ok in
              ignore
                (S.Daemon.Developer.submit
                   daemon
                   ~actor
                   ~run_id:model_run_id
                   ~key:model_key
                   ~request:model_request
                   ~fresh_source:(Some (Document_script { connection = _connection }))
                 |> ok
                 : S.Daemon.Developer.View.t);
              let model_view =
                S.Daemon.Developer.await_terminal
                  daemon
                  ~actor
                  ~run_id:model_run_id
                  ~key:model_key
                |> ok
              in
              let model_job =
                match
                  Option.map (S.Daemon.Developer.View.receipt model_view) ~f:R.value
                with
                | Some (Live (Succeeded (`String id))) -> P.Id.Job.of_string id |> ok
                | result ->
                  raise_s [%sexp "expected selected Model Job", (result : R.value option)]
              in
              (match
                 Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                   Eio.Promise.await selected_parent_entered)
               with
               | () -> ()
               | exception Eio.Time.Timeout ->
                 raise_s
                   [%sexp
                     "selected parent provider was not reached"
                   , (P.Developer_job.state (job ~run:model_run_id model_job)
                      : P.Developer_job.State.t)
                   , (!requests - requests_before_model : int)]);
              Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / model_grand);
              selected_parent_released := true;
              Eio.Promise.resolve release_selected_parent_u ();
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
                let rec await_model () =
                  match
                    (P.Developer_job.state (job ~run:model_run_id model_job)).status
                  with
                  | Succeeded -> ()
                  | Failed _ | Cancelled | Interrupted _ ->
                    raise_s
                      [%sexp
                        "selected Model child did not complete"
                      , (P.Developer_job.state (job ~run:model_run_id model_job)
                         : P.Developer_job.State.t)
                      , (!requests - requests_before_model : int)
                      , (List.length (jobs ~run:model_run_id ()) : int)]
                  | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
                    Eio.Fiber.yield ();
                    await_model ()
                in
                await_model ());
              (match (P.Developer_job.state (job ~run:model_run_id model_job)).result with
               | Some
                   (P.Developer_job.Result_value.Inline
                      (P.Completion.Succeeded (`String "model answer"))) -> ()
               | result ->
                 raise_s
                   [%sexp
                     "selected foreground End_session lost committed assistant text"
                   , (result : P.Developer_job.Result_value.t option)]);
              [%test_eq: int] !requests (requests_before_model + 4);
              let model_requests = List.take !model_bodies 4 in
              assert (Int.equal (List.length model_requests) 4);
              List.iter model_requests ~f:(fun body ->
                match Jsonaf.of_string body with
                | `Object fields ->
                  (match List.Assoc.find_exn fields ~equal:String.equal "model" with
                   | `String "synthetic" -> ()
                   | _ -> failwith "Model descendant lost selected child Config");
                  (match List.Assoc.find_exn fields ~equal:String.equal "reasoning" with
                   | `Object reasoning ->
                     (match
                        List.Assoc.find_exn reasoning ~equal:String.equal "effort"
                      with
                      | `String "high" -> ()
                      | _ -> failwith "Model descendant lost selected reasoning")
                   | _ -> failwith "selected reasoning was not a request object")
                | _ -> failwith "expected selected provider request object");
              (match List.rev model_requests with
               | [ content; file; parent; captured ] ->
                 assert (
                   String.is_substring content ~substring:"from selected Model content");
                 assert (String.is_substring file ~substring:"from selected Model.call");
                 assert (
                   String.is_substring
                     parent
                     ~substring:"Selected Model parent instructions");
                 assert (
                   String.is_substring
                     captured
                     ~substring:"from retained selected Model.call");
                 List.iter [ content; file; captured ] ~f:(fun body ->
                   assert (
                     String.is_substring
                       body
                       ~substring:"Selected Model grandchild instructions"))
               | _ -> failwith "selected Model provider sequence changed");
              [%test_eq: int] (List.length (jobs ~run:model_run_id ())) 1;
              let model_attempts = rows ~run:model_run_id "inference_attempts" in
              [%test_eq: int] (List.length model_attempts) 4;
              let completed_attempts = ref 0 in
              let interrupted_attempts = ref 0 in
              List.iter model_attempts ~f:(fun row ->
                let fields =
                  match row with
                  | `Object fields -> fields
                  | _ -> failwith "invalid selected inference row"
                in
                let field name = List.Assoc.find_exn fields ~equal:String.equal name in
                let job_id = P.Id.Job.of_json (field "job_id") |> ok in
                assert (P.Id.Job.equal job_id model_job);
                let record =
                  Inference.Observation.Attempt_record.of_json
                    (field "record")
                    ~limits:Inference.Observation.Admission.attempt
                  |> Result.map_error ~f:(fun _ -> "invalid selected inference attempt")
                  |> Result.ok_or_failwith
                in
                match Inference.Observation.Attempt_record.state record with
                | Terminal terminal ->
                  (match Inference.Event.Terminal.outcome terminal with
                   | Completed -> incr completed_attempts
                   | Refused | Incomplete _ | Failed _ ->
                     failwith "selected inference did not complete")
                | Interrupted { reason = Host_interrupted; delivery = Response_started }
                  -> incr interrupted_attempts
                | Prepared | Running | Interrupted _ ->
                  failwith "selected inference attempt retained an unexpected phase");
              [%test_eq: int] !completed_attempts 3;
              [%test_eq: int] !interrupted_attempts 1;
              let mixed_connection, mixed_expected =
                open_document
                  ~member:"selected-followup.chatmd"
                  ~contents:
                    {|<script id="model_mixed" language="chatml" kind="tool">let main input = Task.bind(Job.start_tool("model_mixed", input), fun id -> Task.pure(`String(id)))</script>
<script id="model_spawn" language="chatml" kind="tool">let main input = Task.bind(Job.start_tool("model_spawn", input), fun id -> Task.pure(`String(id)))</script>|}
                  ~analysis_script:"model_mixed"
                  daemon
                  actor
              in
              let submit_one selector_name input key_name =
                let request =
                  S.Daemon.Developer.Request.create
                    ~route:
                      (Document_script
                         { expected = mixed_expected
                         ; selector =
                             P.Authoring.Analysis.Target.selected_script selector_name
                             |> ok
                         })
                    ~kind:Live_run
                    ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
                    ~prompt:(Some (S.Catalog_identity.prompt_definition "restart.prompt"))
                    ~workspace:
                      (Some (S.Catalog_identity.workspace_definition "restart.workspace"))
                    ~entrypoint:"main"
                    ~input:(`Object [ "input", `String input ])
                    ~fixtures:None
                    ~requested_execution:None
                    ~requested_max_output_bytes:None
                    ~bounds
                    ~checkpoint:ignore
                  |> ok
                in
                let run_id = P.Id.Developer_run.create () in
                let key = P.Idempotency_key.of_string key_name |> ok in
                ignore
                  (S.Daemon.Developer.submit
                     daemon
                     ~actor
                     ~run_id
                     ~key
                     ~request
                     ~fresh_source:
                       (Some (Document_script { connection = mixed_connection }))
                   |> ok
                   : S.Daemon.Developer.View.t);
                let view =
                  S.Daemon.Developer.await_terminal daemon ~actor ~run_id ~key |> ok
                in
                let job_id =
                  match Option.map (S.Daemon.Developer.View.receipt view) ~f:R.value with
                  | Some (Live (Succeeded (`String id))) -> P.Id.Job.of_string id |> ok
                  | result ->
                    raise_s [%sexp "expected one selected Job", (result : R.value option)]
                in
                run_id, job_id
              in
              let await_succeeded ~run_id job_id =
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
                  let rec loop () =
                    match (P.Developer_job.state (job ~run:run_id job_id)).status with
                    | Succeeded -> ()
                    | Failed _ | Cancelled | Interrupted _ ->
                      raise_s
                        [%sexp
                          "selected child Job failed"
                        , (P.Developer_job.state (job ~run:run_id job_id)
                           : P.Developer_job.State.t)]
                    | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
                      Eio.Fiber.yield ();
                      loop ()
                  in
                  loop ())
              in
              let model_and_effort body =
                let fields =
                  match Jsonaf.of_string body with
                  | `Object fields -> fields
                  | _ -> failwith "selected provider request is not an object"
                in
                let find fields key =
                  List.Assoc.find_exn fields ~equal:String.equal key
                in
                let model =
                  match find fields "model" with
                  | `String value -> value
                  | _ -> failwith "selected provider model is not a string"
                in
                let effort =
                  match find fields "reasoning" with
                  | `Object fields ->
                    (match find fields "effort" with
                     | `String value -> value
                     | _ -> failwith "selected reasoning effort is not a string")
                  | _ -> failwith "selected provider reasoning is not an object"
                in
                model, effort
              in
              let requests_before_mixed = !requests in
              let mixed_run_id, mixed_job =
                submit_one "model_mixed" "mixed" "hosted-selected-mixed"
              in
              await_succeeded ~run_id:mixed_run_id mixed_job;
              [%test_eq: int] !requests (requests_before_mixed + 3);
              (match List.take !model_bodies 3 |> List.rev with
               | [ last; content; outer ] ->
                 assert (
                   String.is_substring
                     last
                     ~substring:"Mixed last Model member instructions");
                 assert (
                   String.is_substring
                     content
                     ~substring:"Mixed Content member instructions");
                 assert (
                   String.is_substring outer ~substring:"Mixed Model member instructions");
                 assert (
                   Tuple2.equal
                     ~eq1:String.equal
                     ~eq2:String.equal
                     (model_and_effort last)
                     ("synthetic-mixed-leaf", "low"));
                 assert (
                   Tuple2.equal
                     ~eq1:String.equal
                     ~eq2:String.equal
                     (model_and_effort content)
                     ("synthetic-mixed-content", "medium"));
                 assert (
                   Tuple2.equal
                     ~eq1:String.equal
                     ~eq2:String.equal
                     (model_and_effort outer)
                     ("synthetic-mixed-outer", "medium"))
               | _ -> failwith "mixed selected provider sequence changed");
              [%test_eq: int] (List.length (jobs ~run:mixed_run_id ())) 1;
              let mixed_models =
                List.map (rows ~run:mixed_run_id "inference_attempts") ~f:(fun row ->
                  let fields =
                    match row with
                    | `Object fields -> fields
                    | _ -> failwith "invalid mixed inference row"
                  in
                  let field name = List.Assoc.find_exn fields ~equal:String.equal name in
                  assert (
                    P.Id.Job.equal (P.Id.Job.of_json (field "job_id") |> ok) mixed_job);
                  let record =
                    Inference.Observation.Attempt_record.of_json
                      (field "record")
                      ~limits:Inference.Observation.Admission.attempt
                    |> Result.map_error ~f:(fun _ -> "invalid mixed inference attempt")
                    |> Result.ok_or_failwith
                  in
                  (match Inference.Observation.Attempt_record.state record with
                   | Terminal terminal ->
                     (match Inference.Event.Terminal.outcome terminal with
                      | Completed -> ()
                      | Refused | Incomplete _ | Failed _ ->
                        failwith "mixed inference did not complete")
                   | Prepared | Running | Interrupted _ ->
                     failwith "mixed inference attempt did not terminalize");
                  Inference.Observation.Configuration.model
                    (Inference.Observation.Attempt_record.configuration record))
                |> List.sort ~compare:String.compare
              in
              [%test_eq: string list]
                mixed_models
                [ "synthetic-mixed-content"
                ; "synthetic-mixed-leaf"
                ; "synthetic-mixed-outer"
                ];
              let requests_before_spawn = !requests in
              let spawn_run_id, spawn_job =
                submit_one "model_spawn" "spawn" "hosted-selected-spawn"
              in
              (match
                 Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                   Eio.Promise.await worker_two_entered)
               with
               | () -> ()
               | exception Eio.Time.Timeout ->
                 raise_s
                   [%sexp
                     "second selected spawn did not enter provider"
                   , (P.Developer_job.state (job ~run:spawn_run_id spawn_job)
                      : P.Developer_job.State.t)
                   , (!requests - requests_before_spawn : int)
                   , (List.map (List.take !model_bodies 2) ~f:(fun body ->
                        List.find_map
                          [ "Selected worker one instructions"
                          ; "Selected spawn parent instructions"
                          ; "Selected worker two instructions"
                          ; "Selected control instructions"
                          ]
                          ~f:(fun marker ->
                            if String.is_substring body ~substring:marker
                            then Some marker
                            else None))
                      : string option list)]);
              await_succeeded ~run_id:spawn_run_id spawn_job;
              worker_two_released := true;
              Eio.Promise.resolve release_worker_two_u ();
              [%test_eq: int] !requests (requests_before_spawn + 4);
              let spawn_bodies = List.take !model_bodies 4 in
              List.iter
                [ "Selected worker one instructions", ("synthetic-worker-one", "low")
                ; "Selected spawn parent instructions", ("synthetic-spawn-parent", "high")
                ; "Selected worker two instructions", ("synthetic-worker-two", "high")
                ; "Selected control instructions", ("synthetic-control", "medium")
                ]
                ~f:(fun (marker, expected) ->
                  match
                    List.filter spawn_bodies ~f:(fun body ->
                      String.is_substring body ~substring:marker)
                  with
                  | [ body ] ->
                    assert (
                      Tuple2.equal
                        ~eq1:String.equal
                        ~eq2:String.equal
                        (model_and_effort body)
                        expected)
                  | _ -> failwith "selected spawn provider marker count changed");
              [%test_eq: int] (List.length (jobs ~run:spawn_run_id ())) 1;
              let spawn_attempts = rows ~run:spawn_run_id "inference_attempts" in
              [%test_eq: int] (List.length spawn_attempts) 4;
              let spawn_observations =
                List.map spawn_attempts ~f:(fun row ->
                  let fields =
                    match row with
                    | `Object fields -> fields
                    | _ -> failwith "invalid spawned inference row"
                  in
                  let field name = List.Assoc.find_exn fields ~equal:String.equal name in
                  assert (
                    P.Id.Job.equal (P.Id.Job.of_json (field "job_id") |> ok) spawn_job);
                  let record =
                    Inference.Observation.Attempt_record.of_json
                      (field "record")
                      ~limits:Inference.Observation.Admission.attempt
                    |> Result.map_error ~f:(fun _ -> "invalid spawned inference attempt")
                    |> Result.ok_or_failwith
                  in
                  let phase =
                    match Inference.Observation.Attempt_record.state record with
                    | Terminal terminal ->
                      (match Inference.Event.Terminal.outcome terminal with
                       | Completed -> "completed"
                       | Refused -> "refused"
                       | Incomplete _ -> "incomplete"
                       | Failed _ -> "failed")
                    | Interrupted { reason; delivery } ->
                      let reason =
                        match reason with
                        | Cancelled -> "cancelled"
                        | Host_interrupted -> "host_interrupted"
                      in
                      let delivery =
                        match delivery with
                        | Definitely_not_submitted -> "not_submitted"
                        | Possibly_submitted -> "possibly_submitted"
                        | Response_started -> "response_started"
                      in
                      reason ^ "/" ^ delivery
                    | Prepared | Running ->
                      failwith "selected spawned attempt stayed live"
                  in
                  let configuration =
                    Inference.Observation.Attempt_record.configuration record
                  in
                  let effort =
                    let open Inference.Observation.Configuration in
                    List.find (settings configuration) ~f:(fun setting ->
                      Name.equal setting.name Name.Reasoning_effort)
                    |> Option.value_exn
                    |> fun setting ->
                    match setting.selection with
                    | Value (Reasoning_effort `Low) -> "low"
                    | Value (Reasoning_effort `Medium) -> "medium"
                    | Value (Reasoning_effort `High) -> "high"
                    | Value (Reasoning_effort (`None | `Minimal | `Xhigh))
                    | Value
                        ( Tokens _
                        | Boolean _
                        | Temperature _
                        | Probability _
                        | Reasoning_summary _
                        | Verbosity _
                        | Text_format _
                        | Tool_choice _
                        | Cache_retention _ )
                    | Omitted | Explicit_null | Withheld ->
                      failwith "selected spawned reasoning effort changed"
                  in
                  (Inference.Observation.Configuration.model configuration, effort), phase)
                |> List.sort ~compare:(fun ((a, _), _) ((b, _), _) -> String.compare a b)
              in
              let spawn_configurations = List.map spawn_observations ~f:fst in
              [%test_eq: (string * string) list]
                spawn_configurations
                [ "synthetic-control", "medium"
                ; "synthetic-spawn-parent", "high"
                ; "synthetic-worker-one", "low"
                ; "synthetic-worker-two", "high"
                ];
              let spawn_phases =
                List.map spawn_observations ~f:(fun ((model, _), phase) -> model, phase)
              in
              [%test_eq: (string * string) list]
                spawn_phases
                [ "synthetic-control", "completed"
                ; "synthetic-spawn-parent", "completed"
                ; "synthetic-worker-one", "completed"
                ; "synthetic-worker-two", "cancelled/possibly_submitted"
                ];
              S.Daemon.shutdown daemon |> ok;
              print_endline
                "hosted captured, content, and selected Model children used their sources")));
  [%expect {| hosted captured, content, and selected Model children used their sources |}]
;;

let%expect_test "developer child moderator uses the daemon's configured runtime policy" =
  let requests = ref 0 in
  let bodies = ref [] in
  let queued_response_sent = ref false in
  let queued_response =
    `Object
      [ "type", `String "response.completed"
      ; "sequence_number", `Number "1"
      ; ( "response"
        , `Object
            [ "object", `String "response"
            ; "id", `String "selected-queued-producer"
            ; "status", `String "completed"
            ; ( "output"
              , `Array
                  [ `Object
                      [ "type", `String "function_call"
                      ; "id", `String "item-queued-producer"
                      ; "call_id", `String "queued-producer"
                      ; "name", `String "queued_producer"
                      ; "arguments", `String {|{"input":"go"}|}
                      ; "status", `String "completed"
                      ]
                  ] )
            ] )
      ]
    |> Jsonaf.to_string
    |> fun json -> "data: " ^ json ^ "\n\n"
  in
  RH.with_fixture
    ~model_requests:requests
    ~model_function_tools:true
    ~response_body:(fun _ body ->
      if
        (not !queued_response_sent)
        && String.is_substring body ~substring:"OCH417 selected issuing child marker"
      then (
        queued_response_sent := true;
        Some queued_response)
      else None)
    ~on_model_body:(fun body -> bodies := body :: !bodies)
    (fun env sw anchor create _ ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60. (fun () ->
         let bootstrap = create () in
         ignore (RH.setup bootstrap : P.Method_result.t);
         let reads = ref 0 in
         ignore
           (RH.enroll bootstrap ~sw "one" "policy-bootstrap-key" reads
            : P.Provider_operator.Configuration_result.t);
         let target = RH.capture bootstrap in
         Provider_runtime.close bootstrap;
         let scopes = Set.add Agent_server_test_support.scopes Developer_live_run in
         let actor =
           Agent_server_test_support.principal_with_scopes "pri_operator" scopes
           |> Operator_authorization.trusted_local
         in
         let bounds =
           Chatml_developer_capture.Json_bounds.create
             ~max_bytes:4096
             ~max_depth:16
             ~max_nodes:128
           |> Or_error.ok_exn
         in
         let run_case
               ?(queued_issuer = false)
               name
               runtime_policy
               expected_status
               expected_requests
           =
           let root = Eio.Path.(anchor / name) |> Eio.Path.native_exn in
           Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / root);
           let prompt = Filename.concat root "prompt.chatmd" in
           let child = Filename.concat root "child.chatmd" in
           Eio.Path.save
             ~create:(`Or_truncate 0o600)
             Eio.Path.(Eio.Stdenv.fs env / prompt)
             {|<tool name="child" agent="child.chatmd" local source_mode="file"/>|};
           Eio.Path.save
             ~create:(`Or_truncate 0o600)
             Eio.Path.(Eio.Stdenv.fs env / child)
             (if queued_issuer
              then
                {|<developer>OCH417 selected issuing child marker</developer>
<tool name="read_file"/>
<script id="producer" language="chatml" kind="tool">
let run ctx input =
  Task.bind(Job.start_tool("read_file", `Object([{key="file"; value=`String("sample.txt")}])), fun first ->
    Task.bind(Job.start_script(`Object([
      {key="source"; value=`String("let main input = Task.pure(input)")},
      {key="tools"; value=`Array([])},
      {key="input"; value=`Null}
    ])), fun second -> Task.pure(`Complete(input))))
</script>
<tool name="queued_producer" type="chatml" script="producer" entrypoint="run" input_schema="input.json" output_schema="output.json"><uses tool="read_file"/></tool>
<script language="chatml" kind="moderator">type state = int
type event = [ `Session_start | `Session_resume | `Turn_start | `Turn_end | `Item_appended(item) ]
let initial_state = 0
let on_event : context -> state -> event -> state task = fun _ st _ -> Task.pure(st)</script>|}
              else
                {|<developer>OCH415 selected policy child marker</developer>
<script language="chatml" kind="moderator">type state = int
type event = [ `Session_start ]
let initial_state = 0
let on_event : context -> state -> event -> state task = fun _ st _ -> Task.pure(st)</script>|});
           if queued_issuer
           then (
             Eio.Path.save
               ~create:(`Or_truncate 0o600)
               Eio.Path.(Eio.Stdenv.fs env / root / "sample.txt")
               "OCH417 selected child file";
             List.iter [ "input.json"; "output.json" ] ~f:(fun name ->
               Eio.Path.save
                 ~create:(`Or_truncate 0o600)
                 Eio.Path.(Eio.Stdenv.fs env / root / name)
                 {|{"type":"object"}|}));
           let provider_anchor = Eio.Path.(anchor / (name ^ "-provider")) in
           Eio.Path.mkdir ~perm:0o700 provider_anchor;
           let provider_factory ~sw:provider_sw ~server_id:provider_server_id =
             let runtime =
               create
                 ~runtime_sw:provider_sw
                 ~runtime_server_id:provider_server_id
                 ~runtime_anchor:provider_anchor
                 ()
             in
             ignore (RH.setup runtime : P.Method_result.t);
             ignore
               (RH.enroll runtime ~sw:provider_sw "one" "policy-daemon-key" reads
                : P.Provider_operator.Configuration_result.t);
             assert (Inference.Request.Target.equal target (RH.capture runtime));
             Ok (Provider_runtime.operator_port runtime)
           in
           let options =
             { S.Daemon.default_options with
               authoring = Some (DH.semantic_authoring_policy env)
             ; developer = Some (developer_configuration target)
             ; provider_operator_factory = Some provider_factory
             ; chatml_runtime_policy = runtime_policy
             }
           in
           let config = Agent_server_test_support.config root root prompt in
           let daemon =
             S.Daemon.start
               ~sw
               ~env
               ~config
               ~tool_dir:root
               ~home:root
               ~process_start_identity:None
               ~options
               ()
             |> ok
           in
           let daemon_closed = ref false in
           Fun.protect
             ~finally:(fun () ->
               if not !daemon_closed then S.Daemon.shutdown daemon |> ok)
             (fun () ->
                let connection, expected =
                  open_document
                    ~contents:
                      {|<script id="s" language="chatml" kind="tool">let main input = Task.bind(Job.start_tool("child", input), fun id -> Task.pure(`String(id)))</script>|}
                    daemon
                    actor
                in
                let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
                let request =
                  S.Daemon.Developer.Request.create
                    ~route:(Document_script { expected; selector })
                    ~kind:Live_run
                    ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
                    ~prompt:(Some (S.Catalog_identity.prompt_definition "restart.prompt"))
                    ~workspace:
                      (Some (S.Catalog_identity.workspace_definition "restart.workspace"))
                    ~entrypoint:"main"
                    ~input:(`Object [ "input", `String "go" ])
                    ~fixtures:None
                    ~requested_execution:None
                    ~requested_max_output_bytes:None
                    ~bounds
                    ~checkpoint:ignore
                  |> ok
                in
                let run_id = P.Id.Developer_run.create () in
                let key = P.Idempotency_key.of_string ("policy-" ^ name) |> ok in
                ignore
                  (S.Daemon.Developer.submit
                     daemon
                     ~actor
                     ~run_id
                     ~key
                     ~request
                     ~fresh_source:(Some (Document_script { connection }))
                   |> ok
                   : S.Daemon.Developer.View.t);
                let view =
                  S.Daemon.Developer.await_terminal daemon ~actor ~run_id ~key |> ok
                in
                let job_id =
                  match Option.map (S.Daemon.Developer.View.receipt view) ~f:R.value with
                  | Some (Live (Succeeded (`String id))) -> P.Id.Job.of_string id |> ok
                  | result ->
                    raise_s
                      [%sexp "expected selected child Job", (result : R.value option)]
                in
                let data_root =
                  S.Daemon.store daemon
                  |> Agent_store.Session_store.data_root
                  |> Agent_store.Data_root.path
                in
                let payload () =
                  let path =
                    Eio.Path.(
                      Eio.Stdenv.fs env
                      / data_root
                      / "developer-runs"
                      / ("run_" ^ P.Id.Developer_run.to_string run_id ^ ".json"))
                  in
                  match Eio.Path.load path |> Jsonaf.parse |> Or_error.ok_exn with
                  | `Object fields ->
                    (match List.Assoc.find_exn fields ~equal:String.equal "payload" with
                     | `Object fields -> fields
                     | _ -> failwith "invalid run payload")
                  | _ -> failwith "invalid run envelope"
                in
                let jobs () =
                  let jobs =
                    match List.Assoc.find_exn (payload ()) ~equal:String.equal "jobs" with
                    | `Array rows -> rows
                    | _ -> failwith "invalid run jobs"
                  in
                  List.map jobs ~f:(fun json ->
                    P.Developer_job.of_json json
                    |> Result.map_error ~f:(fun error ->
                      Sexp.to_string_hum [%sexp (error : P.Protocol_error.t)])
                    |> Result.ok_or_failwith)
                in
                let job_status () =
                  let job =
                    List.find_exn (jobs ()) ~f:(fun job ->
                      P.Id.Job.equal (P.Developer_job.id job) job_id)
                  in
                  (P.Developer_job.state job).status
                in
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
                  let rec loop () =
                    match job_status () with
                    | Succeeded -> `Succeeded
                    | Failed _ when queued_issuer ->
                      raise_s
                        [%sexp
                          "selected queued producer failed"
                        , (List.map (jobs ()) ~f:(fun job ->
                             P.Developer_job.id job, P.Developer_job.state job)
                           : (P.Id.Job.t * P.Developer_job.State.t) list)
                        , (!requests : int)
                        , (!queued_response_sent : bool)]
                    | Failed _ -> `Failed
                    | Waiting_completion _ when queued_issuer -> `Waiting_completion
                    | Cancelled | Interrupted _ ->
                      failwith "selected policy child was interrupted"
                    | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
                      Eio.Fiber.yield ();
                      loop ()
                  in
                  [%test_eq: [ `Succeeded | `Failed | `Waiting_completion ]]
                    (loop ())
                    expected_status);
                [%test_eq: int] !requests expected_requests;
                if queued_issuer
                then (
                  let children =
                    List.filter (jobs ()) ~f:(fun job ->
                      not (P.Id.Job.equal (P.Developer_job.id job) job_id))
                  in
                  [%test_eq: int] (List.length children) 2;
                  let queued_requests =
                    List.map children ~f:(fun job ->
                      let state = P.Developer_job.state job in
                      [%test_eq: int] state.attempt 0;
                      (match state.status with
                       | Queued -> ()
                       | Running
                       | Waiting_permission _
                       | Waiting_completion _
                       | Succeeded
                       | Failed _
                       | Cancelled
                       | Interrupted _ ->
                         failwith "v3 issuer child was claimed before installation");
                      Chat_response.Background_request.of_json
                        ~policy:Chat_response.One_off_request.default_policy
                        (P.Developer_job.payload job)
                      |> ok)
                  in
                  let tool, script =
                    match
                      List.partition_tf queued_requests ~f:(fun request ->
                        match Chat_response.Background_request.target_kind request with
                        | Tool -> true
                        | Script -> false)
                    with
                    | [ tool ], [ script ] -> tool, script
                    | _ -> failwith "selected issuer did not stage Tool and Script"
                  in
                  let issuer request =
                    Chat_response.Background_request.issuer_source request
                    |> Option.value_exn
                  in
                  let issuing = issuer tool in
                  let issuer_field request =
                    match Chat_response.Background_request.to_json request with
                    | `Object fields ->
                      List.Assoc.find_exn fields ~equal:String.equal "issuer_source"
                    | _ -> failwith "invalid queued request envelope"
                  in
                  [%test_eq: string]
                    (Jsonaf.to_string (issuer_field tool))
                    (Jsonaf.to_string (issuer_field script));
                  [%test_eq: string]
                    (P.Developer_agent_artifact.root_relative_path
                       (Chat_response.Background_request.Issuer_source.artifact issuing)
                     |> Source.Portable_path.to_string)
                    "child.chatmd";
                  assert (
                    Option.is_none (Chat_response.Background_request.agent_artifact tool));
                  assert (
                    Option.is_none
                      (Chat_response.Background_request.agent_artifact script));
                  let retained =
                    List.map2_exn children queued_requests ~f:(fun job request ->
                      ( P.Developer_job.id job
                      , Chat_response.Background_request.fingerprint request ))
                  in
                  Eio.Path.save
                    ~create:(`Or_truncate 0o600)
                    Eio.Path.(Eio.Stdenv.fs env / child)
                    "<developer>Edited selected issuing child</developer>";
                  S.Daemon.shutdown daemon |> ok;
                  daemon_closed := true;
                  let after = jobs () in
                  List.iter retained ~f:(fun (id, original) ->
                    let job =
                      List.find_exn after ~f:(fun job ->
                        P.Id.Job.equal (P.Developer_job.id job) id)
                    in
                    let state = P.Developer_job.state job in
                    [%test_eq: int] state.attempt 0;
                    (match state.status with
                     | Queued -> ()
                     | Running
                     | Waiting_permission _
                     | Waiting_completion _
                     | Succeeded
                     | Failed _
                     | Cancelled
                     | Interrupted _ ->
                       failwith "v3 issuer child changed after joined shutdown");
                    let current =
                      Chat_response.Background_request.of_json
                        ~policy:Chat_response.One_off_request.default_policy
                        (P.Developer_job.payload job)
                      |> ok
                      |> Chat_response.Background_request.fingerprint
                    in
                    assert (String.equal original current));
                  [%test_eq: int] !requests expected_requests))
         in
         let default_policy = Chat_response.Runtime_semantics.default_policy in
         run_case "default" default_policy `Succeeded 1;
         assert (
           List.exists !bodies ~f:(fun body ->
             String.is_substring body ~substring:"OCH415 selected policy child marker"));
         let paused_policy =
           { default_policy with
             budget =
               { default_policy.budget with
                 pause_conditions = [ Pause_internal_event_drains ]
               }
           }
         in
         run_case "paused" paused_policy `Failed 1;
         run_case ~queued_issuer:true "queued" default_policy `Succeeded 3));
  print_endline "selected child policy and queued issuer remained source-owned";
  [%expect {| selected child policy and queued issuer remained source-owned |}]
;;
