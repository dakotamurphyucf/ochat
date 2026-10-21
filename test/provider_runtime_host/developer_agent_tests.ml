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

let developer_configuration
      ?remote_content
      ?(selected_native_profile = false)
      ?(execution = Chatml_execution.default_limits)
      ?(max_invocations_per_run = 5)
      ?(max_selected_per_commit = 2)
      ?(max_job_payload_bytes = 4096)
      ?(max_runs = 6)
      ?max_store_document_fields
      ?max_store_document_nodes
      ?(claimed_issuer = false)
      ?(on_native = fun () -> ())
      ?max_active_agent_calls
      ?observe_native_attempt
      target
  =
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
      ~authoring_project:(fun _ ~origin:_ ~context:_ ~current:_ -> None)
      ~document_project:
        (fun
          _
          ~origin:_
          ~context:_
          ~current:_
          ~parsed:_
          ~declaration:_
          ~build:_
          ~metadata:_ -> None)
      ~document_source:
        (fun
          _ ~origin:_ ~context:_ ~current:_ ~parsed:_ ~declaration:_ ~script:_ ->
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
      ~max_runs
      ~max_job_payload_bytes
      ~max_jobs_per_run:8
      ~max_invocations_per_run
      ~max_selected_per_commit
      ~max_inference_bytes_per_run:65536
      ~max_document_bytes:(1024 * 1024)
      ~max_work_steps:100_000
      ?max_document_fields:max_store_document_fields
      ?max_document_nodes:max_store_document_nodes
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
      ~execution
      ~max_output_bytes:65536
      ~receipt:document
      ~max_agent_sources:6
      ~max_agent_source_work_steps:100_000
      ~max_native_bindings:8
      ~workspace_cleanup
      ~max_agent_cache_entries:8
    |> ok
  in
  let limits =
    match max_active_agent_calls with
    | None -> limits
    | Some max_active_calls ->
      S.Daemon.Developer.Configuration.Limits.with_max_active_agent_calls
        limits
        ~max_active_calls
      |> ok
  in
  let result_disclosure =
    S.Developer_result_disclosure.Policy.create ~authorize:(fun _ _ -> true)
  in
  let publisher_limits =
    Agent_store.Developer_result_publisher.Limits.create
      ~inline_bytes:1024
      ~max_result_bytes:65536
      ~max_pending:2
    |> ok
  in
  let publisher_storage = Agent_store.Developer_result_publisher.Storage.Automatic in
  let authorize_native invocation binding =
    on_native ();
    let name = (Chat_response.Tool_capability.reference binding).name in
    Option.iter observe_native_attempt ~f:(fun observe -> observe invocation binding);
    if List.mem [ "read_file"; "summary"; "undeclared" ] name ~equal:String.equal
    then (
      let expected_origin =
        if claimed_issuer && String.equal name "read_file"
        then P.Invocation.Script
        else P.Invocation.Delegated_agent
      in
      assert (
        P.Invocation.equal_origin
          (P.Host_invocation.context invocation).origin
          expected_origin);
      Ok ())
    else if claimed_issuer && String.equal name "queued_worker"
    then (
      assert (
        P.Invocation.equal_origin
          (P.Host_invocation.context invocation).origin
          P.Invocation.Script);
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
        ; "grand"
        ]
        name
        ~equal:String.equal
    then Ok ()
    else Error (P.Error.invalid_request "unexpected native binding")
  in
  let prepare_output = function
    | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
    | _ -> Error (P.Error.invalid_request "unexpected native output")
  in
  let configuration =
    (if selected_native_profile
     then
       S.Daemon.Developer.Configuration.create_with_selected_prompt_profile
         ~capture_policy
         ~capture_limits
         ~limits
         ~result_disclosure
         ~receipt_read:(fun _ _ -> true)
         ~command_control:(fun _ _ -> true)
         ~publisher_limits
         ~publisher_storage
         ~model_target:(Some target)
         ~prepare_output
     else
       S.Daemon.Developer.Configuration.create
         ~capture_policy
         ~capture_limits
         ~limits
         ~result_disclosure
         ~receipt_read:(fun _ _ -> true)
         ~command_control:(fun _ _ -> true)
         ~publisher_limits
         ~publisher_storage
         ~model_target:(Some target)
         ~authorize_native
         ~prepare_output)
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
  let advance_deep_runtime = ref (fun (_ : string) -> ()) in
  let deep_runtime_advances = ref 0 in
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
      !advance_deep_runtime body;
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
       let elapsed_clock = Runtime_elapsed_clock.create (Eio.Stdenv.mono_clock env) in
       let mono_clock = Runtime_elapsed_clock.resource elapsed_clock in
       (* Real elapsed time continues normally. One explicit advance below models
          nested provider runtime without sleeping or pausing startup budgets. *)
       let env =
         object
           method fs = env#fs
           method cwd = env#cwd
           method stdin = env#stdin
           method stdout = env#stdout
           method stderr = env#stderr
           method net = env#net
           method domain_mgr = env#domain_mgr
           method process_mgr = env#process_mgr
           method clock = env#clock
           method mono_clock = mono_clock
           method secure_random = env#secure_random
           method debug = env#debug
           method backend_id = env#backend_id
         end
       in
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60. (fun () ->
         Fun.protect
           ~finally:(fun () ->
             (advance_deep_runtime := fun _ -> ());
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
              (* Level two has completed preparation and reached model I/O. Age
                 its waiting parent's startup clock past compile_seconds=5 while
                 remaining inside the unchanged native runtime allowance of 30s.
                 The parent's post-Agent Content projection must use runtime
                 cancellation/currentness checks rather than its startup clock. *)
              (advance_deep_runtime
               := fun body ->
                    if String.is_substring body ~substring:"Deep level two instructions"
                    then (
                      (advance_deep_runtime := fun _ -> ());
                      [%test_eq: int] !requests 7;
                      incr deep_runtime_advances;
                      let before = Eio.Time.Mono.now mono_clock in
                      Runtime_elapsed_clock.advance elapsed_clock ~seconds:6.;
                      let elapsed = Mtime.span before (Eio.Time.Mono.now mono_clock) in
                      assert (Float.(Mtime.Span.to_float_ns elapsed >= 6e9))));
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
              (advance_deep_runtime := fun _ -> ());
              [%test_eq: int] !deep_runtime_advances 1;
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
  let queued_response_hook = ref (fun () -> ()) in
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
        !queued_response_hook ();
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
         let module Queued_case = struct
           type t =
             | Ordinary
             | Claimed
             | Corrupt
             | Expired_corrupt
           [@@deriving equal]
         end
         in
         let run_case
               ?(queued_case = Queued_case.Ordinary)
               ?(selected_native_profile = false)
               name
               runtime_policy
               expected_status
               expected_requests
           =
           let claimed_issuer =
             match queued_case with
             | Queued_case.Ordinary -> false
             | Queued_case.Claimed | Queued_case.Corrupt | Queued_case.Expired_corrupt ->
               true
           in
           let expire_claims =
             Queued_case.equal queued_case Queued_case.Expired_corrupt
           in
           let corrupt_artifact =
             match queued_case with
             | Queued_case.Corrupt | Queued_case.Expired_corrupt -> true
             | Queued_case.Ordinary | Queued_case.Claimed -> false
           in
           if claimed_issuer then queued_response_sent := false;
           (queued_response_hook := fun () -> ());
           let park_entered = ref false in
           let park_observed = ref false in
           let park_last_deadline = ref None in
           let wall_clock, pause_wall, resume_wall, advance_wall_to =
             Agent_server_test_support.controlled_wall_clock
               (Eio.Stdenv.clock env)
               ~initial:(Eio.Time.now (Eio.Stdenv.clock env))
           in
           let daemon_env =
             if claimed_issuer
             then
               (object
                  method fs = env#fs
                  method cwd = env#cwd
                  method stdin = env#stdin
                  method stdout = env#stdout
                  method stderr = env#stderr
                  method net = env#net
                  method domain_mgr = env#domain_mgr
                  method process_mgr = env#process_mgr
                  method clock = wall_clock
                  method mono_clock = env#mono_clock
                  method secure_random = env#secure_random
                  method debug = env#debug
                  method backend_id = env#backend_id
                end
                 :> Eio_unix.Stdenv.base)
             else env
           in
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
             (if claimed_issuer
              then
                {|<developer>OCH417 selected issuing child marker</developer>
<tool name="read_file"/>
<script id="worker" language="chatml" kind="tool">
let run ctx input =
  Task.bind(Tool.call("read_file", `Object([{key="file"; value=`String("sample.txt")}])), fun _ -> Task.pure(`Complete(input)))
</script>
<tool name="queued_worker" type="chatml" script="worker" entrypoint="run" input_schema="input.json" output_schema="output.json"><uses tool="read_file"/></tool>
<script id="producer" language="chatml" kind="tool">
let run ctx input =
  Task.bind(Job.start_tool("read_file", `Object([{key="file"; value=`String("sample.txt")}])), fun first ->
    Task.bind(Job.start_tool("queued_worker", input), fun second ->
    Task.bind(Job.start_script(`Object([
      {key="source"; value=`String("let main input = Task.bind(Tool.call(\"read_file\", `Object([{key=\"file\"; value=`String(\"sample.txt\")}])), fun _ -> Task.bind(Job.start_tool(\"read_file\", `Object([{key=\"file\"; value=`String(\"sample.txt\")}])), fun _ -> Task.pure(input)))")},
      {key="tools"; value=`Array([`String("read_file")])},
      {key="input"; value=`String("script-claim")}
    ])), fun third -> Task.pure(`Complete(input)))))
</script>
<tool name="queued_producer" type="chatml" script="producer" entrypoint="run" input_schema="input.json" output_schema="output.json"><uses tool="read_file"/><uses tool="queued_worker"/></tool>
<script language="chatml" kind="moderator">type state = int
type event = [ `Session_start | `Session_resume | `Turn_start | `Turn_end | `Item_appended(item) ]
let initial_state = 0
let on_event : context -> state -> event -> state task = fun _ st _ -> Task.pure(st)</script>|}
              else if String.equal name "shell-no-approval"
              then
                {|<developer>OCH415 selected policy child marker</developer>
<shell_access id="direct" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false"
      child_processes="false" arbitrary_code="false" privilege_change="false">
    <write path="${workspace}"/>
  </capabilities>
  <backends merge="replace"><direct when="macos"/><direct when="linux"/></backends>
  <policy default="allow"/>
  <approvals provider="none" unavailable="deny" scopes="once"/>
  <audit format="none"/>
</shell_access>
<moderator_runtime shell_runtime="direct"/>
<script language="chatml" kind="moderator">type state = int
type event = [ `Session_start ]
let initial_state = 0
let on_event : context -> state -> event -> state task = fun _ st _ ->
  Task.bind(Process.run("/usr/bin/touch", ["selected-process-marker"]), fun output ->
    if output == "" then Task.pure(st)
    else Task.fail("selected Process output changed"))</script>|}
              else
                {|<developer>OCH415 selected policy child marker</developer>
<script language="chatml" kind="moderator">type state = int
type event = [ `Session_start ]
let initial_state = 0
let on_event : context -> state -> event -> state task = fun _ st _ -> Task.pure(st)</script>|});
           if claimed_issuer
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
           let native_admissions = ref 0 in
           let options =
             let execution =
               if claimed_issuer
               then { Chatml_execution.default_limits with fuel = 200_000 }
               else Chatml_execution.default_limits
             in
             let max_invocations_per_run = if claimed_issuer then 8 else 5 in
             let max_selected_per_commit = if claimed_issuer then 3 else 2 in
             (* Four issuer-bearing jobs use 3,686 payload bytes; the nested
                child adds 917. Keep the ordinary cases at their 4,096 cap. *)
             let max_job_payload_bytes = if claimed_issuer then 8192 else 4096 in
             { S.Daemon.default_options with
               authoring = Some (DH.semantic_authoring_policy env)
             ; developer =
                 Some
                   (developer_configuration
                      ~execution
                      ~max_invocations_per_run
                      ~max_selected_per_commit
                      ~max_job_payload_bytes
                      ~claimed_issuer
                      ~selected_native_profile
                      ~on_native:(fun () -> Int.incr native_admissions)
                      target)
             ; provider_operator_factory = Some provider_factory
             ; chatml_runtime_policy = runtime_policy
             }
           in
           let config = Agent_server_test_support.config root root prompt in
           let config =
             if String.equal name "shell-no-approval"
             then (
               let child_definition =
                 { (List.hd_exn config.prompts) with
                   id = "selected.child.grant"
                 ; path = child
                 }
               in
               let bootstrap_config =
                 { config with prompts = config.prompts @ [ child_definition ] }
               in
               let source_sha256, manifest_sha256 =
                 Eio.Switch.run (fun bootstrap_sw ->
                   let bootstrap =
                     S.Daemon.start
                       ~sw:bootstrap_sw
                       ~env:daemon_env
                       ~config:bootstrap_config
                       ~tool_dir:root
                       ~home:root
                       ~process_start_identity:None
                       ~options:{ options with developer = None }
                       ()
                     |> ok
                   in
                   Fun.protect
                     ~finally:(fun () -> S.Daemon.shutdown bootstrap |> ok)
                     (fun () ->
                        let entry =
                          Agent_session.Prompt_catalog.find_by_name
                            (S.Daemon.prompts bootstrap)
                            "selected.child.grant"
                          |> Option.value_exn
                        in
                        match entry.availability with
                        | Ready revision ->
                          let artifact =
                            Agent_session.Prompt_revision.artifact revision
                          in
                          ( artifact.root_sha256
                          , Option.value_exn artifact.shell_manifest_sha256 )
                        | Disabled | Unavailable _ ->
                          failwith "selected shell child did not compile"))
               in
               let grant =
                 S.Config.Manifest_grant.
                   { id = "selected-child-shell-grant"
                   ; prompt = "restart.prompt"
                   ; workspaces = [ "restart.workspace" ]
                   ; manifest_sha256
                   ; source_sha256
                   ; principals = [ "pri_operator" ]
                   }
               in
               { config with manifest_grants = [ grant ] })
             else config
           in
           let daemon =
             S.Daemon.start
               ~sw
               ~env:daemon_env
               ~config
               ~tool_dir:root
               ~home:root
               ~process_start_identity:None
               ~options
               ()
             |> ok
           in
           let daemon_closed = ref false in
           let body_failed = ref false in
           Fun.protect
             ~finally:(fun () ->
               resume_wall ();
               if not !daemon_closed
               then (
                 match S.Daemon.shutdown daemon with
                 | Ok () -> ()
                 | Error error ->
                   if not !body_failed then ignore (ok (Error error) : unit)))
             (fun () ->
                try
                  (if claimed_issuer
                   then (
                     queued_response_hook
                     := fun () ->
                          park_entered := true;
                          pause_wall ();
                          let paused_now = Eio.Time.now wall_clock in
                          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                            let rec await_parked () =
                              park_last_deadline
                              := S.Daemon.For_testing.job_scheduler_wait_deadline daemon;
                              match !park_last_deadline with
                              | Some deadline when Float.(deadline > paused_now) ->
                                park_observed := true
                              | None | Some _ ->
                                Eio.Fiber.yield ();
                                await_parked ()
                            in
                            await_parked ()))
                   else queued_response_hook := fun () -> ());
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
                      ~prompt:
                        (Some (S.Catalog_identity.prompt_definition "restart.prompt"))
                      ~workspace:
                        (Some
                           (S.Catalog_identity.workspace_definition "restart.workspace"))
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
                    match
                      Option.map (S.Daemon.Developer.View.receipt view) ~f:R.value
                    with
                    | Some (Live (Succeeded (`String id))) -> P.Id.Job.of_string id |> ok
                    | result ->
                      raise_s
                        [%sexp
                          "expected selected child Job"
                        , (result : R.value option)
                        , (!queued_response_sent : bool)
                        , (!park_entered : bool)
                        , (!park_observed : bool)
                        , (!park_last_deadline : float option)
                        , (!requests : int)]
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
                      match
                        List.Assoc.find_exn (payload ()) ~equal:String.equal "jobs"
                      with
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
                      | Failed _ when claimed_issuer ->
                        raise_s
                          [%sexp
                            "selected queued producer failed"
                          , (List.map (jobs ()) ~f:(fun job ->
                               P.Developer_job.id job, P.Developer_job.state job)
                             : (P.Id.Job.t * P.Developer_job.State.t) list)
                          , (!requests : int)
                          , (!queued_response_sent : bool)]
                      | Failed _ -> `Failed
                      | Cancelled | Interrupted _ ->
                        failwith "selected policy child was interrupted"
                      | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
                        Eio.Fiber.yield ();
                        loop ()
                    in
                    [%test_eq: [ `Succeeded | `Failed ]] (loop ()) expected_status);
                  [%test_eq: int] !requests expected_requests;
                  if String.equal name "shell-no-approval"
                  then
                    [%test_eq: string]
                      (Eio.Path.load
                         Eio.Path.(Eio.Stdenv.fs env / root / "selected-process-marker"))
                      "";
                  if claimed_issuer
                  then (
                    let children =
                      List.filter (jobs ()) ~f:(fun job ->
                        not (P.Id.Job.equal (P.Developer_job.id job) job_id))
                    in
                    [%test_eq: int] (List.length children) 3;
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
                    let tools, scripts =
                      List.partition_tf queued_requests ~f:(fun request ->
                        match Chat_response.Background_request.target_kind request with
                        | Tool -> true
                        | Script -> false)
                    in
                    [%test_eq: int] (List.length tools) 2;
                    [%test_eq: int] (List.length scripts) 1;
                    let tool = List.hd_exn tools in
                    let script_id =
                      List.find_map
                        (List.zip_exn children queued_requests)
                        ~f:(fun (job, request) ->
                          match Chat_response.Background_request.target_kind request with
                          | Tool -> None
                          | Script -> Some (P.Developer_job.id job))
                      |> Option.value_exn
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
                    let tool_name request =
                      match Chat_response.Background_request.to_json request with
                      | `Object fields ->
                        (match
                           List.Assoc.find_exn fields ~equal:String.equal "target"
                         with
                         | `Object target ->
                           (match
                              List.Assoc.find_exn target ~equal:String.equal "name"
                            with
                            | `String name -> name
                            | _ -> failwith "invalid claimed Tool name")
                         | _ -> failwith "invalid claimed Tool target")
                      | _ -> failwith "invalid claimed request"
                    in
                    List.iter queued_requests ~f:(fun request ->
                      [%test_eq: string]
                        (Jsonaf.to_string (issuer_field tool))
                        (Jsonaf.to_string (issuer_field request)));
                    [%test_eq: string]
                      (P.Developer_agent_artifact.root_relative_path
                         (Chat_response.Background_request.Issuer_source.artifact issuing)
                       |> Source.Portable_path.to_string)
                      "child.chatmd";
                    List.iter queued_requests ~f:(fun request ->
                      assert (
                        Option.is_none
                          (Chat_response.Background_request.agent_artifact request)));
                    let managed =
                      List.find_exn tools ~f:(fun request ->
                        String.equal (tool_name request) "queued_worker")
                    in
                    let pins =
                      match Chat_response.Background_request.to_json managed with
                      | `Object fields ->
                        (match List.Assoc.find_exn fields ~equal:String.equal "pins" with
                         | `Object pins -> pins
                         | _ -> failwith "invalid managed request pins")
                      | _ -> failwith "invalid managed request"
                    in
                    assert (
                      Option.is_some
                        (List.Assoc.find pins ~equal:String.equal "queued_worker"));
                    assert (
                      Option.is_none
                        (List.Assoc.find pins ~equal:String.equal "read_file"));
                    let retained =
                      List.map2_exn children queued_requests ~f:(fun job request ->
                        ( P.Developer_job.id job
                        , Chat_response.Background_request.fingerprint request ))
                    in
                    if corrupt_artifact
                    then (
                      let module Artifacts = Agent_store.Prompt_artifact_store in
                      let artifact_ok = function
                        | Ok value -> value
                        | Error error ->
                          raise_s [%sexp (error : Agent_store.Store_error.t)]
                      in
                      let artifacts =
                        Artifacts.open_existing
                          ~env:daemon_env
                          ~root:
                            (Agent_store.Data_root.prompt_artifacts_path
                               (S.Daemon.store daemon
                                |> Agent_store.Session_store.data_root))
                        |> artifact_ok
                      in
                      let revision_id =
                        P.Developer_agent_artifact.revision_id
                          (Chat_response.Background_request.Issuer_source.artifact
                             issuing)
                      in
                      let installed =
                        Artifacts.load artifacts revision_id |> artifact_ok
                      in
                      [%test_eq: string] installed.root_relative_path "child.chatmd";
                      let captured_root =
                        Eio.Path.(
                          Artifacts.materialized_tree artifacts revision_id
                          / installed.root_relative_path)
                      in
                      Eio.Path.unlink captured_root;
                      Eio.Path.save
                        ~create:(`Exclusive 0o600)
                        captured_root
                        "<developer>Corrupted retained issuing child</developer>";
                      assert (Result.is_error (Artifacts.load artifacts revision_id)));
                    Eio.Path.save
                      ~create:(`Or_truncate 0o600)
                      Eio.Path.(Eio.Stdenv.fs env / child)
                      "<developer>Edited selected issuing child</developer>";
                    let native_before_claim = !native_admissions in
                    let provider_before_claim = !requests in
                    if expire_claims
                    then (
                      let latest_deadline =
                        List.map2_exn children queued_requests ~f:(fun job request ->
                          let created =
                            P.Developer_job.created_at job
                            |> P.Timestamp.to_time_ns
                            |> Time_ns.to_span_since_epoch
                            |> Time_ns.Span.to_sec
                          in
                          created
                          +. (Chat_response.Background_request.policy request).execution
                               .wall_seconds)
                        |> List.reduce_exn ~f:Float.max
                      in
                      advance_wall_to (latest_deadline +. 1.));
                    resume_wall ();
                    if expire_claims
                    then
                      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
                        let rec await_expired () =
                          let current = jobs () in
                          [%test_eq: int] (List.length current) 4;
                          let children =
                            List.map retained ~f:(fun (id, original) ->
                              let job =
                                List.find_exn current ~f:(fun job ->
                                  P.Id.Job.equal (P.Developer_job.id job) id)
                              in
                              let request =
                                Chat_response.Background_request.of_json
                                  ~policy:Chat_response.One_off_request.default_policy
                                  (P.Developer_job.payload job)
                                |> ok
                              in
                              [%test_eq: string]
                                (Chat_response.Background_request.fingerprint request)
                                original;
                              job)
                          in
                          let all_expired =
                            List.for_all children ~f:(fun job ->
                              let state = P.Developer_job.state job in
                              match state.status, state.result with
                              | ( Failed _
                                , Some
                                    (P.Developer_job.Result_value.Inline
                                       P.Completion.Expired) ) ->
                                assert (state.attempt > 0);
                                true
                              | Queued, None | Running, None -> false
                              | _ ->
                                raise_s
                                  [%sexp
                                    "expired claimed child had an unexpected outcome"
                                  , (state : P.Developer_job.State.t)])
                          in
                          if not all_expired
                          then (
                            Eio.Fiber.yield ();
                            await_expired ())
                        in
                        await_expired ());
                    if Queued_case.equal queued_case Queued_case.Corrupt
                    then
                      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
                        let rec await_rejected () =
                          let current = jobs () in
                          [%test_eq: int] (List.length current) 4;
                          let all_rejected =
                            List.for_all retained ~f:(fun (id, original) ->
                              let job =
                                List.find_exn current ~f:(fun job ->
                                  P.Id.Job.equal (P.Developer_job.id job) id)
                              in
                              let request =
                                Chat_response.Background_request.of_json
                                  ~policy:Chat_response.One_off_request.default_policy
                                  (P.Developer_job.payload job)
                                |> ok
                              in
                              [%test_eq: string]
                                (Chat_response.Background_request.fingerprint request)
                                original;
                              let state = P.Developer_job.state job in
                              match state.status with
                              | Failed error ->
                                assert (
                                  P.Error.equal_code error.code P.Error.Journal_corrupt);
                                assert (state.attempt > 0);
                                true
                              | Queued | Running -> false
                              | Waiting_permission _
                              | Waiting_completion _
                              | Succeeded
                              | Cancelled
                              | Interrupted _ ->
                                raise_s
                                  [%sexp
                                    "corrupt claimed child had an unexpected outcome"
                                  , (state : P.Developer_job.State.t)])
                          in
                          if not all_rejected
                          then (
                            Eio.Fiber.yield ();
                            await_rejected ())
                        in
                        await_rejected ());
                    if Queued_case.equal queued_case Queued_case.Claimed
                    then
                      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
                        let rec await_claimed () =
                          let current = jobs () in
                          let children =
                            List.map retained ~f:(fun (id, original) ->
                              let job =
                                List.find_exn current ~f:(fun job ->
                                  P.Id.Job.equal (P.Developer_job.id job) id)
                              in
                              let request =
                                Chat_response.Background_request.of_json
                                  ~policy:Chat_response.One_off_request.default_policy
                                  (P.Developer_job.payload job)
                                |> ok
                              in
                              assert (
                                String.equal
                                  original
                                  (Chat_response.Background_request.fingerprint request));
                              job)
                          in
                          let failed =
                            List.filter children ~f:(fun job ->
                              match (P.Developer_job.state job).status with
                              | Failed _ | Cancelled | Interrupted _ -> true
                              | Queued
                              | Running
                              | Waiting_permission _
                              | Waiting_completion _
                              | Succeeded -> false)
                          in
                          if not (List.is_empty failed)
                          then
                            raise_s
                              [%sexp
                                "claimed selected child failed"
                              , (List.map failed ~f:(fun job ->
                                   let request =
                                     Chat_response.Background_request.of_json
                                       ~policy:
                                         Chat_response.One_off_request.default_policy
                                       (P.Developer_job.payload job)
                                     |> ok
                                   in
                                   ( P.Developer_job.id job
                                   , (match
                                        Chat_response.Background_request.target_kind
                                          request
                                      with
                                      | Tool -> tool_name request
                                      | Script -> "Script")
                                   , P.Developer_job.state job ))
                                 : (P.Id.Job.t * string * P.Developer_job.State.t) list)];
                          let all_succeeded =
                            List.for_all children ~f:(fun job ->
                              match (P.Developer_job.state job).status with
                              | Succeeded -> true
                              | Queued
                              | Running
                              | Waiting_permission _
                              | Waiting_completion _
                              | Failed _
                              | Cancelled
                              | Interrupted _ -> false)
                          in
                          let nested =
                            List.filter current ~f:(fun job ->
                              match
                                P.Developer_job.Launch.parent_job
                                  (P.Developer_job.launch job)
                              with
                              | Some (parent, _) -> P.Id.Job.equal parent script_id
                              | None -> false)
                          in
                          if List.length nested > 1
                          then failwith "claimed Script staged multiple nested Jobs";
                          let nested_failed =
                            List.filter nested ~f:(fun job ->
                              match (P.Developer_job.state job).status with
                              | Failed _ | Cancelled | Interrupted _ -> true
                              | Queued
                              | Running
                              | Waiting_permission _
                              | Waiting_completion _
                              | Succeeded -> false)
                          in
                          if not (List.is_empty nested_failed)
                          then
                            raise_s
                              [%sexp
                                "nested selected job failed"
                              , (List.map nested_failed ~f:P.Developer_job.state
                                 : P.Developer_job.State.t list)];
                          let nested_succeeded =
                            match nested with
                            | [ job ] ->
                              (match (P.Developer_job.state job).status with
                               | Succeeded -> true
                               | Queued
                               | Running
                               | Waiting_permission _
                               | Waiting_completion _
                               | Failed _
                               | Cancelled
                               | Interrupted _ -> false)
                            | [] -> false
                            | _ -> failwith "multiple nested selected Jobs"
                          in
                          if all_succeeded && nested_succeeded
                          then (
                            List.iter children ~f:(fun job ->
                              let state = P.Developer_job.state job in
                              assert (state.attempt > 0);
                              let result =
                                match state.result with
                                | Some
                                    (P.Developer_job.Result_value.Inline
                                       (P.Completion.Succeeded value)) -> value
                                | None | Some _ ->
                                  failwith "claimed child lost successful output"
                              in
                              let request =
                                Chat_response.Background_request.of_json
                                  ~policy:Chat_response.One_off_request.default_policy
                                  (P.Developer_job.payload job)
                                |> ok
                              in
                              match
                                Chat_response.Background_request.target_kind request
                              with
                              | Tool ->
                                (match tool_name request with
                                 | "read_file" ->
                                   assert (
                                     String.is_substring
                                       (Jsonaf.to_string result)
                                       ~substring:"OCH417 selected child file")
                                 | "queued_worker" ->
                                   (match result with
                                    | `Object [ ("input", `String "go") ] -> ()
                                    | _ -> failwith "claimed managed input changed")
                                 | _ -> failwith "unexpected claimed Tool target")
                              | Script ->
                                (match result with
                                 | `String "script-claim" -> ()
                                 | _ -> failwith "claimed Script input changed"));
                            List.iter nested ~f:(fun job ->
                              let state = P.Developer_job.state job in
                              match state.status, state.result with
                              | ( Succeeded
                                , Some
                                    (P.Developer_job.Result_value.Inline
                                       (P.Completion.Succeeded value)) ) ->
                                assert (state.attempt > 0);
                                assert (
                                  String.is_substring
                                    (Jsonaf.to_string value)
                                    ~substring:"OCH417 selected child file")
                              | _ ->
                                raise_s
                                  [%sexp
                                    "nested selected job did not finish"
                                  , (state : P.Developer_job.State.t)]))
                          else (
                            Eio.Fiber.yield ();
                            await_claimed ())
                        in
                        await_claimed ());
                    if corrupt_artifact
                    then (
                      [%test_eq: int] !native_admissions native_before_claim;
                      [%test_eq: int] !requests provider_before_claim);
                    [%test_eq: int] !requests expected_requests)
                with
                | exn ->
                  body_failed := true;
                  raise exn)
         in
         let default_policy = Chat_response.Runtime_semantics.default_policy in
         run_case ~selected_native_profile:true "default" default_policy `Succeeded 1;
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
         run_case
           ~queued_case:Queued_case.Claimed
           ~selected_native_profile:true
           "queued-claimed"
           default_policy
           `Succeeded
           3;
         run_case
           ~queued_case:Queued_case.Corrupt
           "queued-corrupt"
           default_policy
           `Succeeded
           5;
         run_case
           ~queued_case:Queued_case.Expired_corrupt
           "queued-expired"
           default_policy
           `Succeeded
           7;
         run_case "shell-no-approval" default_policy `Succeeded 8));
  print_endline "selected child policy and queued issuer remained source-owned";
  [%expect {| selected child policy and queued issuer remained source-owned |}]
;;

let%test_unit "selected ordinary Agent Tool runs through its registered child source" =
  let requests = ref 0 in
  let bodies = ref [] in
  let sent_tool_call = ref false in
  let stage_queued = ref false in
  let before_tool_call = ref (fun () -> ()) in
  let retained_grand = ref None in
  let latest_grand = ref None in
  let check_foreign_grand = ref false in
  let foreign_grand_denial = ref None in
  let retained_grand_denial implementation =
    match
      implementation.Ochat_function.run_with_progress
        ~invocation:Ochat_function.Invocation.silent
        {|{"input":"go"}|}
    with
    | exception Failure message -> Some message
    | _ -> None
  in
  let denied_retained_grand implementation ~expected =
    [%test_eq: string option] (retained_grand_denial implementation) (Some expected)
  in
  let hold_grand_response = ref false in
  let awaiting_grand_submit = ref false in
  let depth_output_waiter : (int * unit Eio.Promise.u) option ref = ref None in
  let depth_body_start = ref None in
  let depth_output_seen = ref false in
  let held_grand = ref None in
  let grand_entered, grand_entered_u = Eio.Promise.create () in
  let release_grand, release_grand_u = Eio.Promise.create () in
  let release_requested = ref false in
  let grand_released = ref false in
  let default_issuer_runtime = ref None in
  let default_grand_revisions = ref [] in
  let limited_grand_revisions = ref [] in
  let record_grand_revision revisions invocation binding =
    if String.equal (Chat_response.Tool_capability.reference binding).name "grand"
    then (
      let implementation =
        Chat_response.Tool_capability.native_implementation binding |> Option.value_exn
      in
      latest_grand := Some implementation;
      (match !retained_grand with
       | None -> retained_grand := Some implementation
       | Some original when !check_foreign_grand ->
         assert (not (phys_equal original implementation));
         let provider_calls = !requests in
         foreign_grand_denial := retained_grand_denial original;
         [%test_eq: int] !requests provider_calls;
         check_foreign_grand := false
       | Some _ -> ());
      revisions
      := (P.Host_invocation.context invocation).implementation_revision :: !revisions)
  in
  let tool_call ~stage_queued =
    let grand_call =
      `Object
        [ "type", `String "function_call"
        ; "id", `String "ordinary-agent-item"
        ; "call_id", `String "ordinary-agent-call"
        ; "name", `String "grand"
        ; "arguments", `String {|{"input":"go"}|}
        ; "status", `String "completed"
        ]
    in
    let queued_call =
      `Object
        [ "type", `String "function_call"
        ; "id", `String "queued-agent-item"
        ; "call_id", `String "queued-agent-call"
        ; "name", `String "queued_producer"
        ; "arguments", `String {|{"input":"go"}|}
        ; "status", `String "completed"
        ]
    in
    `Object
      [ "type", `String "response.completed"
      ; "sequence_number", `Number "1"
      ; ( "response"
        , `Object
            [ "object", `String "response"
            ; "id", `String "ordinary-agent-parent"
            ; "status", `String "completed"
            ; ( "output"
              , `Array
                  (if stage_queued then [ grand_call; queued_call ] else [ grand_call ]) )
            ] )
      ]
    |> Jsonaf.to_string
    |> fun json -> "data: " ^ json ^ "\n\n"
  in
  let queued_grand ~parent_job jobs =
    let parent =
      List.find_exn jobs ~f:(fun job ->
        P.Id.Job.equal (P.Developer_job.id job) parent_job)
    in
    let parent_attempt = (P.Developer_job.state parent).attempt in
    let queued =
      List.filter jobs ~f:(fun job ->
        not (P.Id.Job.equal (P.Developer_job.id job) parent_job))
    in
    let job = List.hd_exn queued in
    [%test_eq: int] (List.length queued) 1;
    let state = P.Developer_job.state job in
    [%test_eq: int] state.attempt 0;
    let launch = P.Developer_job.launch job in
    (match P.Developer_job.Launch.parent launch with
     | Invocation _ -> ()
     | Run_root -> failwith "selected grandchild lacks its real invocation parent");
    (match P.Developer_job.Launch.parent_job launch with
     | Some (id, attempt) ->
       assert (P.Id.Job.equal id parent_job);
       [%test_eq: int] attempt parent_attempt
     | None -> failwith "selected grandchild lacks its creating Job attempt");
    (match state.status with
     | Queued -> ()
     | Running
     | Waiting_permission _
     | Waiting_completion _
     | Succeeded
     | Failed _
     | Cancelled
     | Interrupted _ -> failwith "selected grandchild was claimed before observation");
    let request =
      Chat_response.Background_request.of_json
        ~policy:Chat_response.One_off_request.default_policy
        (P.Developer_job.payload job)
      |> ok
    in
    (match Chat_response.Background_request.target_kind request with
     | Tool -> ()
     | Script -> failwith "selected grandchild was not staged as Tool");
    let issuer =
      Chat_response.Background_request.issuer_source request |> Option.value_exn
    in
    let target =
      Chat_response.Background_request.agent_artifact request |> Option.value_exn
    in
    [%test_eq: string]
      (P.Developer_agent_artifact.root_relative_path
         (Chat_response.Background_request.Issuer_source.artifact issuer)
       |> Source.Portable_path.to_string)
      "child.chatmd";
    [%test_eq: string]
      (P.Developer_agent_artifact.root_relative_path target
       |> Source.Portable_path.to_string)
      "grand.chatmd";
    assert (
      not
        (P.Id.Prompt_revision.equal
           (P.Developer_agent_artifact.revision_id
              (Chat_response.Background_request.Issuer_source.artifact issuer))
           (P.Developer_agent_artifact.revision_id target)));
    ( P.Developer_job.id job
    , Chat_response.Background_request.Issuer_source.runtime_policy issuer
    , Chat_response.Background_request.Issuer_source.artifact issuer
    , target )
  in
  RH.with_fixture
    ~model_requests:requests
    ~model_function_tools:true
    ~response_body:(fun _ body ->
      if
        (not !sent_tool_call)
        && String.is_substring body ~substring:"OCH421 parent marker"
      then (
        sent_tool_call := true;
        !before_tool_call ();
        Some (tool_call ~stage_queued:!stage_queued))
      else None)
    ~on_model_body:(fun body ->
      bodies := body :: !bodies;
      (match !depth_output_waiter with
       | Some (previous_bodies, resolver)
         when List.length !bodies > previous_bodies
              && String.is_substring body ~substring:"function_call_output" ->
         depth_output_seen := true;
         depth_output_waiter := None;
         Eio.Promise.resolve resolver ()
       | Some _ | None -> ());
      if !hold_grand_response && String.is_substring body ~substring:"OCH421 grand marker"
      then (
        held_grand := !latest_grand;
        awaiting_grand_submit := true))
    ~on_model_submit:(fun () ->
      if !awaiting_grand_submit
      then (
        awaiting_grand_submit := false;
        Eio.Promise.resolve grand_entered_u ();
        Eio.Promise.await release_grand;
        grand_released := true))
    (fun env sw anchor create _ ->
       let module Phase = struct
         type t =
           | Bootstrap
           | Start_primary
           | File_success
           | File_missing
           | Captured_parent
           | Captured_grand
           | Corrupt_parent
           | Corrupt_grand
           | Held_parent
           | Held_entry
           | Shutdown_join
           | Response_release
           | Start_depth_limited
           | Depth_submit
           | Depth_command_await
           | Depth_output
           | Queued_depth_parent
         [@@deriving sexp_of]
       end
       in
       let mono_clock = Eio.Stdenv.mono_clock env in
       let started = Eio.Time.Mono.now mono_clock in
       let elapsed () =
         Mtime.span started (Eio.Time.Mono.now mono_clock)
         |> Mtime.Span.to_float_ns
         |> fun nanoseconds -> nanoseconds /. 1e9
       in
       let phase = ref Phase.Bootstrap in
       let phases = ref [ Phase.Bootstrap, 0. ] in
       let scheduler_barrier = ref None in
       let timeout_reported = ref false in
       let mark next =
         phase := next;
         phases := List.take ((next, elapsed ()) :: !phases) 32
       in
       let with_timeout_exn ~wait seconds f =
         try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) seconds f with
         | Eio.Time.Timeout as exn ->
           let backtrace = Stdlib.Printexc.get_raw_backtrace () in
           if not !timeout_reported
           then (
             timeout_reported := true;
             (* Only static fixture labels, timings, counts and flags are reported.
                Reporting must not replace the original timeout/backtrace. *)
             try
               eprintf
                 "%s\n%!"
                 (Sexp.to_string_hum
                    [%sexp
                      "selected Agent Tool fixture timeout"
                    , (wait : string)
                    , (seconds : float)
                    , (!phase : Phase.t)
                    , (elapsed () : float)
                    , (List.rev !phases : (Phase.t * float) list)
                    , (!scheduler_barrier : (Phase.t * float * bool) option)
                    , ("provider-requests", (!requests : int))
                    , ("provider-bodies", (List.length !bodies : int))
                    , ( "depth-provider-bodies"
                      , (Option.map !depth_body_start ~f:(fun start ->
                           List.length !bodies - start)
                         : int option) )
                    , ("tool-response-sent", (!sent_tool_call : bool))
                    , ("depth-output-seen", (!depth_output_seen : bool))
                    , ( "depth-native-attempts"
                      , (List.length !limited_grand_revisions : int) )
                    , ("held-response-enabled", (!hold_grand_response : bool))
                    , ("held-submit-waiting", (!awaiting_grand_submit : bool))])
             with
             | _ -> ());
           Stdlib.Printexc.raise_with_backtrace exn backtrace
       in
       with_timeout_exn ~wait:"whole-test" 45. (fun () ->
         let bootstrap = create () in
         ignore (RH.setup bootstrap : P.Method_result.t);
         let reads = ref 0 in
         ignore
           (RH.enroll bootstrap ~sw "one" "ordinary-agent-bootstrap" reads
            : P.Provider_operator.Configuration_result.t);
         let target = RH.capture bootstrap in
         Provider_runtime.close bootstrap;
         let root = Eio.Path.(anchor / "ordinary-agent") |> Eio.Path.native_exn in
         Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / root);
         let prompt = Filename.concat root "prompt.chatmd" in
         let child = Filename.concat root "child.chatmd" in
         let grand = Filename.concat root "grand.chatmd" in
         let grand_content = Filename.concat root "grand-content.chatmd" in
         let queued_child =
           {|<developer>OCH421 parent marker</developer>
<tool name="grand" agent="grand.chatmd" local source_mode="captured"/>
<script id="queue_grand" language="chatml" kind="tool">
let run ctx input =
  Task.bind(Job.start_tool("grand", input), fun _ -> Task.pure(`Complete(input)))
</script>
<tool name="queued_producer" type="chatml" script="queue_grand" entrypoint="run" input_schema="input.json" output_schema="output.json"><uses tool="grand"/></tool>|}
         in
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           Eio.Path.(Eio.Stdenv.fs env / prompt)
           {|<tool name="child" agent="child.chatmd" local source_mode="file"/>|};
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           Eio.Path.(Eio.Stdenv.fs env / child)
           {|<developer>OCH421 parent marker</developer>
<tool name="grand" agent="grand.chatmd" local source_mode="file"/>|};
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           Eio.Path.(Eio.Stdenv.fs env / grand)
           {|<developer>OCH421 grand marker</developer>
<user><agent src="grand-content.chatmd" local source_mode="captured">Tool content caller</agent></user>|};
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           Eio.Path.(Eio.Stdenv.fs env / grand_content)
           "<developer>OCH421 Tool Content marker</developer>";
         List.iter [ "input.json"; "output.json" ] ~f:(fun name ->
           Eio.Path.save
             ~create:(`Or_truncate 0o600)
             Eio.Path.(Eio.Stdenv.fs env / root / name)
             {|{"type":"object"}|});
         let provider_anchor = Eio.Path.(anchor / "ordinary-agent-provider") in
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
             (RH.enroll runtime ~sw:provider_sw "one" "ordinary-agent-key" reads
              : P.Provider_operator.Configuration_result.t);
           assert (Inference.Request.Target.equal target (RH.capture runtime));
           Ok (Provider_runtime.operator_port runtime)
         in
         let actor =
           Set.add Agent_server_test_support.scopes Developer_live_run
           |> Agent_server_test_support.principal_with_scopes "pri_operator"
           |> Operator_authorization.trusted_local
         in
         let daemon_env clock =
           object
             method fs = env#fs
             method cwd = env#cwd
             method stdin = env#stdin
             method stdout = env#stdout
             method stderr = env#stderr
             method net = env#net
             method domain_mgr = env#domain_mgr
             method process_mgr = env#process_mgr
             method clock = clock
             method mono_clock = env#mono_clock
             method secure_random = env#secure_random
             method debug = env#debug
             method backend_id = env#backend_id
           end
         in
         let pause_at_scheduler_wait daemon clock pause =
           pause ();
           let paused_now = Eio.Time.now clock in
           let barrier_phase = !phase in
           let barrier_started = elapsed () in
           scheduler_barrier := Some (barrier_phase, barrier_started, false);
           with_timeout_exn ~wait:"scheduler-barrier" 5. (fun () ->
             let rec await () =
               match S.Daemon.For_testing.job_scheduler_wait_deadline daemon with
               | Some deadline when Float.(deadline > paused_now) -> ()
               | None | Some _ ->
                 Eio.Fiber.yield ();
                 await ()
             in
             await ());
           scheduler_barrier := Some (barrier_phase, barrier_started, true)
         in
         let clock, pause_clock, resume_clock, _ =
           Agent_server_test_support.controlled_wall_clock
             (Eio.Stdenv.clock env)
             ~initial:(Eio.Time.now (Eio.Stdenv.clock env))
         in
         let options =
           { S.Daemon.default_options with
             authoring = Some (DH.semantic_authoring_policy env)
           ; developer =
               Some
                 (developer_configuration
                    ~max_runs:7
                    ~max_store_document_fields:4096
                    ~max_store_document_nodes:8192
                    ~observe_native_attempt:
                      (record_grand_revision default_grand_revisions)
                    target)
           ; provider_operator_factory = Some provider_factory
           }
         in
         let jobs daemon run_id =
           let data_root =
             S.Daemon.store daemon
             |> Agent_store.Session_store.data_root
             |> Agent_store.Data_root.path
           in
           let path =
             Eio.Path.(
               Eio.Stdenv.fs env
               / data_root
               / "developer-runs"
               / ("run_" ^ P.Id.Developer_run.to_string run_id ^ ".json"))
           in
           let rows =
             match Eio.Path.load path |> Jsonaf.parse |> Or_error.ok_exn with
             | `Object envelope ->
               (match List.Assoc.find_exn envelope ~equal:String.equal "payload" with
                | `Object payload ->
                  (match List.Assoc.find_exn payload ~equal:String.equal "jobs" with
                   | `Array jobs -> jobs
                   | _ -> failwith "invalid Agent Tool job array")
                | _ -> failwith "invalid Agent Tool run payload")
             | _ -> failwith "invalid Agent Tool run envelope"
           in
           List.map rows ~f:(fun row ->
             P.Developer_job.of_json row
             |> Result.map_error ~f:(fun error ->
               Sexp.to_string_hum [%sexp (error : P.Protocol_error.t)])
             |> Result.ok_or_failwith)
         in
         let artifacts daemon =
           let module Artifacts = Agent_store.Prompt_artifact_store in
           Artifacts.open_existing
             ~env
             ~root:
               (Agent_store.Data_root.prompt_artifacts_path
                  (S.Daemon.store daemon |> Agent_store.Session_store.data_root))
           |> function
           | Ok store -> store
           | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
         in
         mark Phase.Start_primary;
         let daemon =
           S.Daemon.start
             ~sw
             ~env:(daemon_env clock)
             ~config:(Agent_server_test_support.config root root prompt)
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
             if not !release_requested
             then (
               release_requested := true;
               Eio.Promise.resolve release_grand_u ());
             resume_clock ();
             if not !daemon_closed then S.Daemon.shutdown daemon |> ok)
           (fun () ->
              let connection, expected =
                open_document
                  ~contents:
                    {|<script id="s" language="chatml" kind="tool">let main input = Task.bind(Job.start_tool("child", input), fun id -> Task.pure(`String(id)))</script>|}
                  daemon
                  actor
              in
              let request =
                S.Daemon.Developer.Request.create
                  ~route:
                    (Document_script
                       { expected
                       ; selector = P.Authoring.Analysis.Target.selected_script "s" |> ok
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
                  ~bounds:
                    (Chatml_developer_capture.Json_bounds.create
                       ~max_bytes:4096
                       ~max_depth:16
                       ~max_nodes:128
                     |> Or_error.ok_exn)
                  ~checkpoint:ignore
                |> ok
              in
              let submit_job name =
                let run_id = P.Id.Developer_run.create () in
                let key = P.Idempotency_key.of_string name |> ok in
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
                      [%sexp
                        "expected selected Agent Tool Job", (result : R.value option)]
                in
                run_id, job_id
              in
              let job_state run_id job_id () =
                let job =
                  List.find_exn (jobs daemon run_id) ~f:(fun job ->
                    P.Id.Job.equal (P.Developer_job.id job) job_id)
                in
                P.Developer_job.state job
              in
              let await_job run_id job_id =
                with_timeout_exn ~wait:"job-terminal" 25. (fun () ->
                  let rec await () =
                    match (job_state run_id job_id ()).status with
                    | Succeeded -> ()
                    | Failed _ -> failwith "selected ordinary Agent Tool Job failed"
                    | Cancelled | Interrupted _ ->
                      failwith "selected ordinary Agent Tool Job stopped"
                    | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
                      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                      await ()
                  in
                  await ())
              in
              mark Phase.File_success;
              let run_id, job_id = submit_job "ordinary-agent-tool" in
              await_job run_id job_id;
              assert !sent_tool_call;
              let saw_grand =
                List.exists !bodies ~f:(fun body ->
                  String.is_substring body ~substring:"OCH421 grand marker")
              in
              let field value name =
                match value with
                | `Object fields -> List.Assoc.find_exn fields ~equal:String.equal name
                | _ -> failwith "ordinary Agent Tool provider field is not an object"
              in
              let string = function
                | `String value -> value
                | _ -> failwith "ordinary Agent Tool provider field is not a string"
              in
              let tool_output bodies =
                List.find_exn bodies ~f:(fun body ->
                  String.is_substring body ~substring:"function_call_output")
                |> Jsonaf.of_string
                |> fun body ->
                match field body "input" with
                | `Array items ->
                  List.find_exn items ~f:(fun item ->
                    match field item "type" with
                    | `String "function_call_output" -> true
                    | `String _ -> false
                    | _ -> failwith "invalid ordinary Agent Tool input type")
                | _ -> failwith "ordinary Agent Tool provider input is not an array"
              in
              let output = tool_output !bodies in
              [%test_eq: string] (field output "call_id" |> string) "ordinary-agent-call";
              let outcome = field output "output" |> string |> Jsonaf.of_string in
              let outcome_type = field outcome "type" |> string in
              if (not saw_grand) || not (String.equal outcome_type "complete")
              then
                raise_s
                  [%sexp
                    "ordinary Agent Tool child did not complete"
                  , (!requests : int)
                  , (saw_grand : bool)
                  , (outcome_type : string)
                  , ((match outcome_type with
                      | "fail" -> field outcome "code" |> string
                      | _ -> "")
                     : string)];
              assert saw_grand;
              assert (
                List.exists !bodies ~f:(fun body ->
                  String.is_substring body ~substring:"OCH421 Tool Content marker"));
              let original_grand = Option.value_exn !retained_grand in
              let provider_calls = !requests in
              denied_retained_grand
                original_grand
                ~expected:"selected developer native invocation scope is not active";
              [%test_eq: int] !requests provider_calls;
              (before_tool_call
               := fun () -> Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / grand));
              sent_tool_call := false;
              let previous_bodies = List.length !bodies in
              mark Phase.File_missing;
              let file_missing_run, file_missing_job =
                submit_job "ordinary-agent-tool-file-missing"
              in
              await_job file_missing_run file_missing_job;
              let file_missing_bodies =
                List.take !bodies (List.length !bodies - previous_bodies)
              in
              assert (
                not
                  (List.exists file_missing_bodies ~f:(fun body ->
                     String.is_substring body ~substring:"OCH421 grand marker")));
              let file_missing_output = tool_output file_missing_bodies in
              let file_missing_outcome =
                field file_missing_output "output" |> string |> Jsonaf.of_string
              in
              [%test_eq: string] (field file_missing_outcome "type" |> string) "fail";
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / child)
                queued_child;
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / grand)
                "<developer>OCH421 Captured original marker</developer>";
              (before_tool_call
               := fun () ->
                    Eio.Path.save
                      ~create:(`Or_truncate 0o600)
                      Eio.Path.(Eio.Stdenv.fs env / grand)
                      "<developer>OCH421 Captured edited marker</developer>";
                    pause_at_scheduler_wait daemon clock pause_clock);
              check_foreign_grand := true;
              stage_queued := true;
              sent_tool_call := false;
              let previous_bodies = List.length !bodies in
              mark Phase.Captured_parent;
              let captured_run, captured_job =
                submit_job "ordinary-agent-tool-captured"
              in
              await_job captured_run captured_job;
              let captured_bodies =
                List.take !bodies (List.length !bodies - previous_bodies)
              in
              assert (
                List.exists captured_bodies ~f:(fun body ->
                  String.is_substring body ~substring:"OCH421 Captured original marker"));
              assert (
                not
                  (List.exists captured_bodies ~f:(fun body ->
                     String.is_substring body ~substring:"OCH421 Captured edited marker")));
              let captured_output = tool_output captured_bodies in
              let captured_outcome =
                field captured_output "output" |> string |> Jsonaf.of_string
              in
              [%test_eq: string] (field captured_outcome "type" |> string) "complete";
              assert (not !check_foreign_grand);
              [%test_eq: string option]
                !foreign_grand_denial
                (Some "saved Agent source callback is no longer selected");
              let queued_grand_job, issuer_runtime, _, _ =
                queued_grand ~parent_job:captured_job (jobs daemon captured_run)
              in
              default_issuer_runtime := Some issuer_runtime;
              let previous_bodies = List.length !bodies in
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / child)
                "<developer>OCH422 edited issuer marker</developer>";
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / grand)
                "<developer>OCH422 edited target marker</developer>";
              mark Phase.Captured_grand;
              resume_clock ();
              await_job captured_run queued_grand_job;
              let claimed_bodies =
                List.take !bodies (List.length !bodies - previous_bodies)
              in
              assert (
                List.exists claimed_bodies ~f:(fun body ->
                  String.is_substring body ~substring:"OCH421 Captured original marker"));
              assert (
                not
                  (List.exists claimed_bodies ~f:(fun body ->
                     String.is_substring body ~substring:"OCH422 edited issuer marker"
                     || String.is_substring body ~substring:"OCH422 edited target marker")));
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / child)
                queued_child;
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / grand)
                "<developer>OCH422 target-corrupt original marker</developer>";
              (before_tool_call
               := fun () -> pause_at_scheduler_wait daemon clock pause_clock);
              sent_tool_call := false;
              stage_queued := true;
              mark Phase.Corrupt_parent;
              let corrupt_run, corrupt_parent =
                submit_job "ordinary-agent-tool-target-corrupt"
              in
              await_job corrupt_run corrupt_parent;
              let corrupt_job, _, issuer_artifact, target_artifact =
                queued_grand ~parent_job:corrupt_parent (jobs daemon corrupt_run)
              in
              let module Artifacts = Agent_store.Prompt_artifact_store in
              let store = artifacts daemon in
              let issuer_revision =
                P.Developer_agent_artifact.revision_id issuer_artifact
              in
              let target_revision =
                P.Developer_agent_artifact.revision_id target_artifact
              in
              let artifact_ok = function
                | Ok value -> value
                | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
              in
              let issuer_installed =
                Artifacts.load store issuer_revision |> artifact_ok
              in
              let target_installed =
                Artifacts.load store target_revision |> artifact_ok
              in
              [%test_eq: string] issuer_installed.root_relative_path "child.chatmd";
              [%test_eq: string] target_installed.root_relative_path "grand.chatmd";
              assert (
                not
                  (String.equal
                     (Eio.Path.native_exn
                        (Artifacts.materialized_tree store issuer_revision))
                     (Eio.Path.native_exn
                        (Artifacts.materialized_tree store target_revision))));
              let target_root =
                Eio.Path.(
                  Artifacts.materialized_tree store target_revision
                  / target_installed.root_relative_path)
              in
              Eio.Path.unlink target_root;
              Eio.Path.save
                ~create:(`Exclusive 0o600)
                target_root
                "<developer>OCH422 corrupt target tree</developer>";
              ignore
                (Artifacts.load store issuer_revision |> artifact_ok
                 : Artifacts.Artifact.t);
              assert (Result.is_error (Artifacts.load store target_revision));
              let provider_before_corrupt_claim = !requests in
              let native_before_corrupt_claim = List.length !default_grand_revisions in
              mark Phase.Corrupt_grand;
              resume_clock ();
              with_timeout_exn ~wait:"corrupt-target-failure" 25. (fun () ->
                let rec await_failed () =
                  let state = job_state corrupt_run corrupt_job () in
                  match state.status with
                  | Failed error ->
                    [%test_eq: P.Error.code] error.code P.Error.Invalid_state;
                    (match state.result with
                     | Some
                         (P.Developer_job.Result_value.Inline (P.Completion.Failed error))
                       ->
                       [%test_eq: string] error.code "invocation.handler_failed";
                       [%test_eq: string] error.message "Tool execution failed.";
                       assert (not error.retryable)
                     | _ ->
                       failwith "corrupt distinct Agent target lost its failure receipt");
                    [%test_eq: int] state.attempt 1
                  | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
                    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                    await_failed ()
                  | Succeeded | Cancelled | Interrupted _ ->
                    failwith "corrupt distinct Agent target did not fail at Claim"
                in
                await_failed ());
              [%test_eq: int] !requests provider_before_corrupt_claim;
              (* Native authorization observes the selected grand binding before
                 loading its separate saved Agent target. The corrupt target
                 cannot reach its provider or publish a successful result. *)
              [%test_eq: int]
                (List.length !default_grand_revisions)
                (native_before_corrupt_claim + 1);
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / child)
                queued_child;
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / grand)
                "<developer>OCH421 grand marker OCH422 held target</developer>";
              (before_tool_call
               := fun () -> pause_at_scheduler_wait daemon clock pause_clock);
              stage_queued := true;
              sent_tool_call := false;
              mark Phase.Held_parent;
              let cancelling_run, cancelling_parent =
                submit_job "ordinary-agent-tool-shutdown"
              in
              await_job cancelling_run cancelling_parent;
              let cancelling_job, _, _, held_target =
                queued_grand ~parent_job:cancelling_parent (jobs daemon cancelling_run)
              in
              assert (
                not
                  (P.Id.Prompt_revision.equal
                     target_revision
                     (P.Developer_agent_artifact.revision_id held_target)));
              hold_grand_response := true;
              mark Phase.Held_entry;
              resume_clock ();
              with_timeout_exn ~wait:"held-provider-entry" 10. (fun () ->
                Eio.Promise.await grand_entered);
              let interrupted_grand = Option.value_exn !held_grand in
              assert (not (phys_equal interrupted_grand original_grand));
              let running = job_state cancelling_run cancelling_job () in
              (match running.status with
               | Running -> assert (running.attempt > 0)
               | Queued
               | Waiting_permission _
               | Waiting_completion _
               | Succeeded
               | Failed _
               | Cancelled
               | Interrupted _ ->
                 failwith "selected Agent Tool owner was not running at shutdown");
              mark Phase.Shutdown_join;
              let shutdown_done, shutdown_done_u = Eio.Promise.create () in
              Eio.Fiber.fork ~sw (fun () ->
                S.Daemon.shutdown daemon |> ok;
                daemon_closed := true;
                Eio.Promise.resolve shutdown_done_u ());
              with_timeout_exn ~wait:"shutdown-join" 10. (fun () ->
                Eio.Promise.await shutdown_done);
              assert (not !grand_released);
              (match (job_state cancelling_run cancelling_job ()).status with
               | Cancelled -> ()
               | Queued
               | Running
               | Waiting_permission _
               | Waiting_completion _
               | Succeeded
               | Failed _
               | Interrupted _ ->
                 failwith "selected Agent Tool Job was not cancelled by joined shutdown");
              let provider_calls = !requests in
              denied_retained_grand
                interrupted_grand
                ~expected:"deferred Agent Root has been revoked";
              [%test_eq: int] !requests provider_calls;
              hold_grand_response := false;
              release_requested := true;
              mark Phase.Response_release;
              Eio.Promise.resolve release_grand_u ();
              with_timeout_exn ~wait:"response-release" 5. (fun () ->
                let rec await () =
                  if not !grand_released
                  then (
                    Eio.Fiber.yield ();
                    await ())
                in
                await ());
              [%test_eq: int] !requests provider_calls;
              match (job_state cancelling_run cancelling_job ()).status with
              | Cancelled -> ()
              | Queued
              | Running
              | Waiting_permission _
              | Waiting_completion _
              | Succeeded
              | Failed _
              | Interrupted _ ->
                failwith "cancelled Agent Tool Job changed after server release");
         stage_queued := false;
         assert (List.length !default_grand_revisions >= 2);
         let file_grand_revision = List.last_exn !default_grand_revisions in
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           Eio.Path.(Eio.Stdenv.fs env / child)
           {|<developer>OCH421 parent marker</developer>
<tool name="grand" agent="grand.chatmd" local source_mode="file"/>|};
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           Eio.Path.(Eio.Stdenv.fs env / grand)
           "<developer>OCH421 Captured original marker</developer>";
         (before_tool_call
          := fun () -> Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / grand));
         sent_tool_call := false;
         let previous_bodies = List.length !bodies in
         mark Phase.Start_depth_limited;
         let limited_clock, pause_limited, resume_limited, _ =
           Agent_server_test_support.controlled_wall_clock
             (Eio.Stdenv.clock env)
             ~initial:(Eio.Time.now (Eio.Stdenv.clock env))
         in
         let limited_options =
           { options with
             developer =
               Some
                 (developer_configuration
                    ~max_runs:7
                    ~max_store_document_fields:4096
                    ~max_store_document_nodes:8192
                    ~max_active_agent_calls:1
                    ~observe_native_attempt:
                      (record_grand_revision limited_grand_revisions)
                    target)
           }
         in
         let limited_daemon =
           S.Daemon.start
             ~sw
             ~env:(daemon_env limited_clock)
             ~config:(Agent_server_test_support.config root root prompt)
             ~tool_dir:root
             ~home:root
             ~process_start_identity:None
             ~options:limited_options
             ()
           |> ok
         in
         Fun.protect
           ~finally:(fun () ->
             resume_limited ();
             S.Daemon.shutdown limited_daemon |> ok)
           (fun () ->
              let connection, expected =
                open_document
                  ~contents:
                    {|<script id="s" language="chatml" kind="tool">let main input = Task.bind(Job.start_tool("child", input), fun id -> Task.pure(`String(id)))</script>|}
                  limited_daemon
                  actor
              in
              let request =
                S.Daemon.Developer.Request.create
                  ~route:
                    (Document_script
                       { expected
                       ; selector = P.Authoring.Analysis.Target.selected_script "s" |> ok
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
                  ~bounds:
                    (Chatml_developer_capture.Json_bounds.create
                       ~max_bytes:4096
                       ~max_depth:16
                       ~max_nodes:128
                     |> Or_error.ok_exn)
                  ~checkpoint:ignore
                |> ok
              in
              let submit_limited ?await_phase name =
                let run_id = P.Id.Developer_run.create () in
                let key = P.Idempotency_key.of_string name |> ok in
                ignore
                  (S.Daemon.Developer.submit
                     limited_daemon
                     ~actor
                     ~run_id
                     ~key
                     ~request
                     ~fresh_source:(Some (Document_script { connection }))
                   |> ok
                   : S.Daemon.Developer.View.t);
                Option.iter await_phase ~f:mark;
                let view =
                  S.Daemon.Developer.await_terminal limited_daemon ~actor ~run_id ~key
                  |> ok
                in
                let job_id =
                  match Option.map (S.Daemon.Developer.View.receipt view) ~f:R.value with
                  | Some (Live (Succeeded (`String id))) -> P.Id.Job.of_string id |> ok
                  | result ->
                    raise_s
                      [%sexp
                        "expected depth-limited selected Job", (result : R.value option)]
                in
                run_id, job_id
              in
              let depth_output, resolve_depth_output = Eio.Promise.create () in
              (* A provider body may arrive before the command receipt. Arm the
                 same fresh-output observation before submitting the command. *)
              depth_body_start := Some (List.length !bodies);
              depth_output_seen := false;
              depth_output_waiter := Some (previous_bodies, resolve_depth_output);
              Fun.protect
                ~finally:(fun () -> depth_output_waiter := None)
                (fun () ->
                   mark Phase.Depth_submit;
                   let _depth_run, _depth_job =
                     submit_limited
                       ~await_phase:Phase.Depth_command_await
                       "ordinary-agent-tool-depth"
                   in
                   mark Phase.Depth_output;
                   with_timeout_exn ~wait:"depth-output" 25. (fun () ->
                     Eio.Promise.await depth_output));
              let limited_bodies =
                List.take !bodies (List.length !bodies - previous_bodies)
              in
              assert (
                not
                  (List.exists limited_bodies ~f:(fun body ->
                     String.is_substring body ~substring:"OCH421 Captured edited marker")));
              let field value name =
                match value with
                | `Object fields -> List.Assoc.find_exn fields ~equal:String.equal name
                | _ -> failwith "limited Agent Tool provider field is not an object"
              in
              let string = function
                | `String value -> value
                | _ -> failwith "limited Agent Tool provider field is not a string"
              in
              let output =
                List.find_exn limited_bodies ~f:(fun body ->
                  String.is_substring body ~substring:"function_call_output")
                |> Jsonaf.of_string
                |> fun body ->
                match field body "input" with
                | `Array items ->
                  List.find_exn items ~f:(fun item ->
                    match field item "type" with
                    | `String "function_call_output" -> true
                    | `String _ -> false
                    | _ -> failwith "invalid limited Agent Tool input type")
                | _ -> failwith "limited Agent Tool input is not an array"
              in
              let outcome = field output "output" |> string |> Jsonaf.of_string in
              [%test_eq: string] (field outcome "type" |> string) "fail";
              [%test_eq: string]
                (field outcome "code" |> string)
                "invocation.handler_failed";
              [%test_eq: int] (List.length !limited_grand_revisions) 1;
              assert (
                not
                  (String.equal
                     file_grand_revision
                     (List.hd_exn !limited_grand_revisions)));
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / child)
                queued_child;
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / grand)
                "<developer>OCH421 Captured original marker</developer>";
              (before_tool_call
               := fun () ->
                    pause_at_scheduler_wait limited_daemon limited_clock pause_limited);
              stage_queued := true;
              sent_tool_call := false;
              mark Phase.Queued_depth_parent;
              let queued_run, queued_parent =
                submit_limited "ordinary-agent-tool-queued-depth"
              in
              with_timeout_exn ~wait:"queued-depth-parent" 25. (fun () ->
                let rec await () =
                  let parent =
                    List.find_exn (jobs limited_daemon queued_run) ~f:(fun job ->
                      P.Id.Job.equal (P.Developer_job.id job) queued_parent)
                  in
                  match (P.Developer_job.state parent).status with
                  | Succeeded -> ()
                  | Failed _ -> failwith "depth-limited queued producer failed"
                  | Cancelled | Interrupted _ ->
                    failwith "depth-limited queued producer stopped"
                  | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
                    Eio.Fiber.yield ();
                    await ()
                in
                await ());
              let _queued_grand_job, limited_issuer_runtime, _, _ =
                queued_grand ~parent_job:queued_parent (jobs limited_daemon queued_run)
              in
              assert (
                not
                  (Chatml_source.Digest.equal
                     (Option.value_exn !default_issuer_runtime)
                     limited_issuer_runtime));
              [%test_eq: int] (List.length !limited_grand_revisions) 2;
              assert (
                not
                  (String.equal
                     (List.hd_exn !default_grand_revisions)
                     (List.hd_exn !limited_grand_revisions))))))
;;
