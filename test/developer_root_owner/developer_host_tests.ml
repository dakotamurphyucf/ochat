open! Core
module P = Agent_protocol
module S = Agent_server
module R = Agent_store.Developer_command_receipt

let () = Mirage_crypto_rng_unix.use_default ()

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let checked = function
  | Ok value -> value
  | Error _ -> failwith "fixture admission failed"
;;

let developer_configuration ?(tool_fixture = false) () =
  let target =
    if tool_fixture then Chatml.Chatml_compiler_contract.Target.Tool_v1 else One_off_v1
  in
  let contracts =
    S.Authoring_standard_contracts.create
      ~targets:[ target ]
      ~max_identity_bytes:(16 * 1024 * 1024)
      ~max_work_steps:1_000_000
      ~checkpoint:ignore
    |> checked
  in
  let selected =
    let entry =
      S.Authoring_standard_contracts.find contracts target |> Option.value_exn
    in
    if tool_fixture
    then (
      let schema =
        match Chatmd_shell_spec.Tool_schema.compile `True with
        | Ok schema -> schema
        | Error _ -> failwith "fixture schema unavailable"
      in
      S.Authoring_context_service.Developer.Selected_contract.of_standard_tool_entry
        entry
        ~tool_name:"developer_fixture"
        ~input_schema:schema
        ~output_schema:schema
      |> ok)
    else
      S.Authoring_context_service.Developer.Selected_contract.of_standard_entry entry
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
    |> checked
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
    Root_owner_tests.store_limits
      ~max_document_bytes:(1024 * 1024)
      ~max_work_steps:100_000
      ()
  in
  let roots =
    S.Developer_root_owner.Limits.create
      ~max_active_runs:2
      ~max_staged_jobs:2
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
    ~model_target:None
    ~authorize_native:(fun _ _ ->
      Error (P.Error.invalid_request "no native tool selected"))
    ~prepare_output:(fun _ -> Error (P.Error.invalid_request "no native output selected"))
  |> ok
;;

let semantic_authoring_policy env =
  let module Service = S.Authoring_context_service in
  let module Policy = S.Authoring_installation.Host_policy in
  let base =
    Policy.basic_supplied ~initialize_limits:S.Daemon.default_options.protocol_limits
    |> ok
  in
  let type_display =
    Chatml_type_display.Limits.create
      ~max_nodes:512
      ~max_edges:1024
      ~max_depth:16
      ~max_work_steps:100_000
      ~max_text_bytes:1024
      ~max_output_bytes:4096
    |> Or_error.ok_exn
  in
  let query =
    Chatml_authoring_query.Limits.create
      ~max_items:8
      ~max_member_depth:4
      ~max_member_edges:128
      ~max_label_bytes:4096
      ~max_work_steps:100_000
      ~type_display
    |> Or_error.ok_exn
  in
  let semantic =
    Policy.Semantic.create
      ~script_selection:None
      ~project_metadata:None
      ~project_build_limits:
        (Chatml_project_build.Limits.create
           ~max_work_steps:100_000
           ~max_input_bytes:(Some 4096)
           ~document:Document_schema.Limits.default
         |> Or_error.ok_exn)
      ~limits:
        (Service.Semantic.Limits.create
           ~max_document_bundle_files:4
           ~max_document_bundle_bytes:2048
           ~max_parser_work_steps:100_000
           ~max_diagnostic_work_steps:100_000
           ~max_selection_rows:4
           ~max_retained_selection_source_bytes:(4 * 1024 * 1024)
           ~max_retained_selection_input_bytes:(4 * 1024 * 1024)
           ~max_selection_work_steps:1_000_000
         |> Or_error.ok_exn)
      ~prefix_limits:
        (Service.Semantic.Prefix_limits.create
           ~max_document_bundle_files:4
           ~max_document_bundle_bytes:2048
           ~max_parser_work_steps:100_000
           ~max_host_work_steps:100_000
         |> Or_error.ok_exn)
      ~query_limits:
        (Service.Semantic.Query_limits.create
           ~max_host_work_steps:100_000
           ~projection:query
         |> Or_error.ok_exn)
      ~document_dir:(Eio.Stdenv.cwd env)
  in
  Policy.with_semantic base ~semantic |> ok
;;

let with_daemon ?(tool_fixture = false) f =
  Eio_main.run (fun env ->
    let root =
      Filename.concat
        "/tmp"
        ("ochat-developer-host-" ^ P.Id.Transaction.(create () |> to_string))
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / root) in
    Eio.Path.mkdir ~perm:0o700 path;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path)
      ~f:(fun () ->
        let prompt1 = Filename.concat root "prompt.chatmd" in
        let prompt2 = Filename.concat root "prompt-two.chatmd" in
        List.iter [ prompt1; prompt2 ] ~f:(fun file ->
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / file)
            "<developer>Developer host fixture.</developer>");
        Eio.Switch.run (fun sw ->
          let base = Agent_server_test_support.config root root prompt1 in
          let config =
            match base.workspaces, base.prompts with
            | [ workspace ], [ prompt ] ->
              let second_limit =
                S.Config.Workspace.
                  { prompt = "restart.second"; max_root_agents = 1; overflow = Reject }
              in
              { base with
                workspaces =
                  [ { workspace with
                      prompt_limits = second_limit :: workspace.prompt_limits
                    }
                  ]
              ; prompts =
                  { prompt with id = "restart.second"; path = prompt2 } :: base.prompts
              }
            | _ -> failwith "unexpected configured catalog fixture"
          in
          let options =
            { S.Daemon.default_options with
              authoring = Some (semantic_authoring_policy env)
            ; developer = Some (developer_configuration ~tool_fixture ())
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
          Fun.protect
            ~finally:(fun () -> S.Daemon.shutdown daemon |> ok)
            (fun () -> f daemon))))
;;

let open_document ?(source = "let main input = Task.pure(input)") daemon actor =
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
    P.Initialize.Implementation.create ~name:"developer-host" ~version:"1" |> ok
  in
  let request =
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
       (Protocol_initialize request)
   with
   | Ok (Protocol_initialize _) -> ()
   | Ok _ -> failwith "unexpected initialization response"
   | Error error -> raise_s [%sexp (error : P.Error.t)]);
  let request =
    P.Authoring.Context.Open_request.create_supplied_document
      ~member:"draft.chatmd"
      ~default_environment:None
      ~contents:
        ("<script id=\"s\" language=\"chatml\" kind=\"tool\">" ^ source ^ "</script>")
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
  connection, transport, expected
;;

let%expect_test
    "hosted Document_script keeps same Live owner through pure Check and changes on \
     Prompt selection"
  =
  with_daemon (fun daemon ->
    let scopes =
      Set.add
        (Set.add Agent_server_test_support.scopes Developer_live_run)
        Developer_check
    in
    let principal =
      Agent_server_test_support.principal_with_scopes "pri_developer_host" scopes
    in
    let actor = Operator_authorization.trusted_local principal in
    let other_actor = Operator_authorization.trusted_local principal in
    let connection, _transport, expected = open_document daemon actor in
    let run_id = P.Id.Developer_run.create () in
    let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
    let route = S.Daemon.Developer.Request.Route.Document_script { expected; selector } in
    let fresh_source =
      Some (S.Daemon.Developer.Request.Fresh_source.Document_script { connection })
    in
    let bounds =
      Chatml_developer_capture.Json_bounds.create
        ~max_bytes:4096
        ~max_depth:16
        ~max_nodes:128
      |> Or_error.ok_exn
    in
    let request ~kind ~prompt =
      S.Daemon.Developer.Request.create
        ~route
        ~kind
        ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
        ~prompt:(Option.map prompt ~f:S.Catalog_identity.prompt_definition)
        ~workspace:
          (Option.map prompt ~f:(fun _ ->
             S.Catalog_identity.workspace_definition "restart.workspace"))
        ~entrypoint:"main"
        ~input:`Null
        ~fixtures:None
        ~requested_execution:None
        ~requested_max_output_bytes:None
        ~bounds
        ~checkpoint:ignore
      |> ok
    in
    let command actor key request =
      let key = P.Idempotency_key.of_string key |> checked in
      ignore
        (S.Daemon.Developer.submit daemon ~actor ~run_id ~key ~request ~fresh_source |> ok);
      S.Daemon.Developer.await_terminal daemon ~actor ~run_id ~key |> ok
    in
    let live prompt = request ~kind:Live_run ~prompt:(Some prompt) in
    let owner = S.Daemon.Developer.View.owner in
    let live_succeeded view =
      match Option.map (S.Daemon.Developer.View.receipt view) ~f:R.value with
      | Some (Live (Succeeded `Null)) -> ()
      | result ->
        raise_s [%sexp "expected durable Live success", (result : R.value option)]
    in
    let first = command actor "developer-host-live-1" (live "restart.prompt") in
    live_succeeded first;
    let repeated = command actor "developer-host-live-2" (live "restart.prompt") in
    live_succeeded repeated;
    assert (Int.equal (owner first).epoch (owner repeated).epoch);
    let check =
      command other_actor "developer-host-check" (request ~kind:Check ~prompt:None)
    in
    (match Option.map (S.Daemon.Developer.View.receipt check) ~f:R.value with
     | Some (Check Ready) -> ()
     | result ->
       raise_s [%sexp "expected durable Check readiness", (result : R.value option)]);
    assert (Int.equal (owner first).epoch (owner check).epoch);
    let after_check = command actor "developer-host-live-3" (live "restart.prompt") in
    live_succeeded after_check;
    assert (Int.equal (owner first).epoch (owner after_check).epoch);
    let changed = command actor "developer-host-live-4" (live "restart.second") in
    live_succeeded changed;
    assert ((owner changed).epoch > (owner first).epoch);
    print_endline
      "four durable Live successes; Check Ready; same owner before changed Prompt, new \
       owner after");
  [%expect
    {| four durable Live successes; Check Ready; same owner before changed Prompt, new owner after |}]
;;

let%expect_test "hosted Tool_v1 fixture binds selected ABI and durable row" =
  with_daemon ~tool_fixture:true (fun daemon ->
    let scopes = Set.add Agent_server_test_support.scopes Developer_fixture_run in
    let principal =
      Agent_server_test_support.principal_with_scopes "pri_developer_tool" scopes
    in
    let actor = Operator_authorization.trusted_local principal in
    let source =
      {|let run ctx input =
  let* result = Tool.call("fixture", input) in
  match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(message) -> Task.fail(message)|}
    in
    let connection, _transport, expected = open_document ~source daemon actor in
    let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
    let route = S.Daemon.Developer.Request.Route.Document_script { expected; selector } in
    let fresh =
      Some (S.Daemon.Developer.Request.Fresh_source.Document_script { connection })
    in
    let bounds =
      Chatml_developer_capture.Json_bounds.create
        ~max_bytes:4096
        ~max_depth:16
        ~max_nodes:128
      |> Or_error.ok_exn
    in
    let step =
      Chatml_developer_capture.Fixture.Step.create
        ~tool_name:"fixture"
        ~input_schema:(`Object [])
        ~output_schema:(`Object [])
        ~expected_input:`Null
        ~response:(Return (`String "world"))
        ~json_bounds:bounds
        ~checkpoint:ignore
      |> Or_error.ok_exn
    in
    let fixtures =
      Chatml_developer_capture.Fixture.create
        ~steps:[ step ]
        ~max_steps:4
        ~max_bytes:8192
        ~checkpoint:ignore
      |> Or_error.ok_exn
    in
    let request target =
      S.Daemon.Developer.Request.create
        ~route
        ~kind:Fixture_run
        ~target
        ~prompt:None
        ~workspace:None
        ~entrypoint:"run"
        ~input:`Null
        ~fixtures:(Some fixtures)
        ~requested_execution:None
        ~requested_max_output_bytes:None
        ~bounds
        ~checkpoint:ignore
      |> ok
    in
    let run_id = P.Id.Developer_run.create () in
    let key = P.Idempotency_key.of_string "tool-v1-fixture" |> checked in
    let selected = request Chatml.Chatml_compiler_contract.Target.Tool_v1 in
    ignore
      (S.Daemon.Developer.submit
         daemon
         ~actor
         ~run_id
         ~key
         ~request:selected
         ~fresh_source:fresh
       |> ok);
    let terminal = S.Daemon.Developer.await_terminal daemon ~actor ~run_id ~key |> ok in
    (match Option.map (S.Daemon.Developer.View.receipt terminal) ~f:R.value with
     | Some
         (Fixture
            (Completed
               { outcome = Succeeded (`String "world")
               ; tool_selection_fingerprint = Some _
               ; matched_steps = 1
               ; _
               })) -> ()
     | other -> raise_s [%sexp "unexpected Tool_v1 fixture", (other : R.value option)]);
    let retry =
      S.Daemon.Developer.submit
        daemon
        ~actor
        ~run_id
        ~key
        ~request:selected
        ~fresh_source:None
      |> ok
    in
    assert (
      R.Id.equal (S.Daemon.Developer.View.id terminal) (S.Daemon.Developer.View.id retry));
    let wrong = request Chatml.Chatml_compiler_contract.Target.One_off_v1 in
    let changed_target_conflicts =
      Result.is_error
        (S.Daemon.Developer.submit
           daemon
           ~actor
           ~run_id
           ~key
           ~request:wrong
           ~fresh_source:None)
    in
    printf
      "tool_success=true same_row=true changed_target_conflicts=%b\n"
      changed_target_conflicts);
  [%expect {| tool_success=true same_row=true changed_target_conflicts=true |}]
;;
