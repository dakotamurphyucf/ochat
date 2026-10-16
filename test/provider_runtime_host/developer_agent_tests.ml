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
      ~max_job_payload_bytes:4096
      ~max_jobs_per_run:8
      ~max_inference_bytes_per_run:65536
      ~max_document_bytes:(1024 * 1024)
      ~max_work_steps:100_000
      ()
  in
  let roots =
    S.Developer_root_owner.Limits.create
      ~max_active_runs:3
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
      ~max_agent_sources:4
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
        else if List.mem [ "child"; "child_tools"; "deep" ] name ~equal:String.equal
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

let open_document daemon actor =
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
      ~member:"draft.chatmd"
      ~default_environment:None
      ~contents:
        {|<script id="s" language="chatml" kind="tool">let main input = Task.bind(Job.start_tool("child", input), fun first -> Task.bind(Job.start_tool("child", input), fun second -> Task.pure(`Object([{key="first"; value=`String(first)}, {key="second"; value=`String(second)}]))))</script>
<script id="tool_child" language="chatml" kind="tool">let main input = Task.bind(Job.start_tool("child_tools", input), fun id -> Task.pure(`String(id)))</script>
<script id="deep_child" language="chatml" kind="tool">let main input = Task.bind(Job.start_tool("deep", input), fun id -> Task.pure(`String(id)))</script>|}
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
      ~target:(P.Authoring.Analysis.Target.selected_script "s" |> ok)
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
  let first_entered, first_entered_u = Eio.Promise.create () in
  let second_entered, second_entered_u = Eio.Promise.create () in
  let release_first, release_first_u = Eio.Promise.create () in
  let first_released = ref false in
  RH.with_fixture
    ~model_requests:requests
    ~model_function_tools:true
    ~model_image_input:true
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
    ~on_model_body:(fun body -> model_bodies := body :: !model_bodies)
    ~on_model_submit:(fun () ->
      if Int.equal !requests 1
      then (
        Eio.Promise.resolve first_entered_u ();
        Eio.Promise.await release_first)
      else if Int.equal !requests 2
      then Eio.Promise.resolve second_entered_u ())
    (fun env sw anchor create _ ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60. (fun () ->
         Fun.protect
           ~finally:(fun () ->
             if not !first_released then Eio.Promise.resolve release_first_u ())
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
<tool name="deep" agent="agents/deep-one.chatmd" local source_mode="file"/>|};
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
              S.Daemon.shutdown daemon |> ok;
              print_endline
                "hosted captured child and two content levels used selected tools")));
  [%expect {| hosted captured child and two content levels used selected tools |}]
;;
