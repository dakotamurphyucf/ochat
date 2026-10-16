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

let developer_configuration target =
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
      ~max_active_runs:2
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
    ~authorize_native:(fun _ binding ->
      if String.equal (Chat_response.Tool_capability.reference binding).name "child"
      then Ok ()
      else Error (P.Error.invalid_request "unexpected native binding"))
    ~prepare_output:(function
      | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
      | _ -> Error (P.Error.invalid_request "unexpected native output"))
  |> ok
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
        {|<script id="s" language="chatml" kind="tool">let main input = Task.bind(Job.start_tool("child", input), fun first -> Task.bind(Job.start_tool("child", input), fun second -> Task.pure(`Object([{key="first"; value=`String(first)}, {key="second"; value=`String(second)}]))))</script>|}
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

let%expect_test "hosted captured Agent child uses its saved source and tracked model" =
  let requests = ref 0 in
  let model_bodies = ref [] in
  let first_entered, first_entered_u = Eio.Promise.create () in
  let second_entered, second_entered_u = Eio.Promise.create () in
  let release_first, release_first_u = Eio.Promise.create () in
  let first_released = ref false in
  RH.with_fixture
    ~model_requests:requests
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
              let prompt_dir = Filename.concat root "prompts" in
              Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / prompt_dir);
              let prompt = Filename.concat prompt_dir "prompt.chatmd" in
              let child = Filename.concat prompt_dir "child.chatmd" in
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / prompt)
                {|<tool name="child" agent="child.chatmd" local source_mode="file"/>|};
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                Eio.Path.(Eio.Stdenv.fs env / child)
                "<developer>Original hosted child instructions</developer>";
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
                ; developer = Some (developer_configuration target)
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
              let path =
                Eio.Path.(
                  Eio.Stdenv.fs env
                  / data_root
                  / "developer-runs"
                  / ("run_" ^ P.Id.Developer_run.to_string run_id ^ ".json"))
              in
              let payload () =
                match Eio.Path.load path |> Jsonaf.parse |> Or_error.ok_exn with
                | `Object fields ->
                  (match List.Assoc.find_exn fields ~equal:String.equal "payload" with
                   | `Object fields -> fields
                   | _ -> failwith "invalid run payload")
                | _ -> failwith "invalid run envelope"
              in
              let rows field =
                match List.Assoc.find_exn (payload ()) ~equal:String.equal field with
                | `Array rows -> rows
                | _ -> failwith "invalid run rows"
              in
              let jobs () =
                List.map (rows "jobs") ~f:(fun json ->
                  P.Developer_job.of_json json
                  |> Result.map_error ~f:(fun error ->
                    Sexp.to_string_hum [%sexp (error : P.Protocol_error.t)])
                  |> Result.ok_or_failwith)
              in
              let job id =
                List.find_exn (jobs ()) ~f:(fun value ->
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
              S.Daemon.shutdown daemon |> ok;
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
              print_endline
                "hosted captured child used saved instructions with tracked provider")));
  [%expect {| hosted captured child used saved instructions with tracked provider |}]
;;
