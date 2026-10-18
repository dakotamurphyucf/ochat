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

let developer_configuration
      ?(tool_fixture = false)
      ?(authorize_native =
        fun _ _ -> Error (P.Error.invalid_request "no native tool selected"))
      ?(prepare_output =
        fun _ -> Error (P.Error.invalid_request "no native output selected"))
      ()
  =
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
    ~authorize_native
    ~prepare_output
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

let repl_configuration () =
  let parse =
    Chatml.Chatml_parse.Submission.Limits.create
      ~max_source_bytes:4096
      ~max_tokens:4096
      ~max_work_steps:100_000
      ~sites:
        (Chatml.Chatml_syntax_sites.Limits.create ~max_sites:4096 ~max_name_bytes:16384
         |> Or_error.ok_exn)
    |> Or_error.ok_exn
  in
  let display =
    Chatml_typechecker.Type_snapshot.Limits.create
      ~max_nodes:4096
      ~max_edges:4096
      ~max_label_bytes:16384
    |> Or_error.ok_exn
  in
  let root_copy =
    Chatml_typechecker.Repl_type_roots.Limits.create
      ~max_bindings:128
      ~max_type_roots:256
      ~max_type_nodes:4096
      ~max_variable_cells:1024
      ~max_type_edges:4096
      ~max_copy_work:100_000
      ~max_copy_depth:128
      ~max_name_work:100_000
      ~max_name_bytes:16384
    |> Or_error.ok_exn
  in
  let typing_limits =
    Chatml_repl_typing.Limits.create
      ~parse
      ~display
      ~max_display_name_bytes:4096
      ~max_statements:16
      ~root_copy
    |> Or_error.ok_exn
  in
  S.Daemon.Developer_configuration.Repl.create
    ~scope_limits:
      (Chatml_configured_lookup.Repl_scope.Limits.create
         ~max_snippets:16
         ~max_identity_bytes:4096
       |> Or_error.ok_exn)
    ~root_limits:
      (Chatml_dependency_session.Roots.Limits.create
         ~max_checked_modules:16
         ~max_edges:64
         ~max_dependency_depth:16
       |> Or_error.ok_exn)
    ~typing_limits
    ~evaluator_limits:
      (Chatml_repl_execution.Limits.create
         ~max_preview_bytes:4096
         ~max_preview_nodes:1024
         ~max_preview_depth:16
         ~max_diagnostic_bytes:1024
         ~max_retained_tasks:8
       |> Or_error.ok_exn)
    ~execution_policy:(Bounded Chatml_execution.default_limits)
    ~max_instances:2
    ~max_keys_per_instance:4
    ~max_submissions_per_epoch:8
    ~max_source_bytes:4096
    ~max_receipt_bytes:8192
    ~max_total_receipt_bytes:16384
    ~max_task_origins:4
  |> ok
;;

let with_daemon
      ?(tool_fixture = false)
      ?(repl = false)
      ?(prompt_source = "<developer>Developer host fixture.</developer>")
      ?(native_sample = false)
      ?authorize_native
      ?prepare_output
      ?timeout_seconds
      ?after_shutdown
      f
  =
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
            prompt_source);
        if native_sample
        then
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / root / "sample.txt")
            "developer host native sample";
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
          let developer =
            developer_configuration ~tool_fixture ?authorize_native ?prepare_output ()
          in
          let developer =
            if repl
            then
              S.Daemon.Developer_configuration.with_repl developer (repl_configuration ())
            else developer
          in
          let options =
            { S.Daemon.default_options with
              authoring = Some (semantic_authoring_policy env)
            ; developer = Some developer
            }
          in
          let start () =
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
          let run () =
            let daemon = start () in
            let result =
              Fun.protect
                ~finally:(fun () -> S.Daemon.shutdown daemon |> ok)
                (fun () -> f daemon)
            in
            Option.iter after_shutdown ~f:(fun replay ->
              let reopened = start () in
              Fun.protect
                ~finally:(fun () -> S.Daemon.shutdown reopened |> ok)
                (fun () -> replay result reopened));
            result
          in
          match timeout_seconds with
          | None -> run ()
          | Some seconds -> Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) seconds run)))
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

let%expect_test "hosted command receipts distinguish compilation, limits, and revocation" =
  with_daemon (fun daemon ->
    let scopes = Set.add Agent_server_test_support.scopes Developer_live_run in
    let scopes = Set.add scopes Developer_check in
    let principal =
      Agent_server_test_support.principal_with_scopes "pri_developer_receipts" scopes
    in
    let current = ref true in
    let actor =
      Operator_authorization.guarded ~principal ~is_current:(fun () -> !current)
    in
    let reconnected = Operator_authorization.trusted_local principal in
    let key text = P.Idempotency_key.of_string text |> checked in
    let bounds =
      Chatml_developer_capture.Json_bounds.create
        ~max_bytes:4096
        ~max_depth:16
        ~max_nodes:128
      |> Or_error.ok_exn
    in
    let run ?(input = `Null) ~source ~kind ~name ~requested_max_output_bytes () =
      let connection, _transport, expected = open_document ~source daemon actor in
      let route =
        S.Daemon.Developer.Request.Route.Document_script
          { expected; selector = P.Authoring.Analysis.Target.selected_script "s" |> ok }
      in
      let request =
        S.Daemon.Developer.Request.create
          ~route
          ~kind
          ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
          ~prompt:
            (if R.Kind.equal kind Live_run
             then Some (S.Catalog_identity.prompt_definition "restart.prompt")
             else None)
          ~workspace:
            (if R.Kind.equal kind Live_run
             then Some (S.Catalog_identity.workspace_definition "restart.workspace")
             else None)
          ~entrypoint:"main"
          ~input
          ~fixtures:None
          ~requested_execution:None
          ~requested_max_output_bytes
          ~bounds
          ~checkpoint:ignore
        |> ok
      in
      let run_id = P.Id.Developer_run.create () in
      let key = key name in
      ignore
        (S.Daemon.Developer.submit
           daemon
           ~actor
           ~run_id
           ~key
           ~request
           ~fresh_source:(Some (Document_script { connection }))
         |> ok);
      let terminal = S.Daemon.Developer.await_terminal daemon ~actor ~run_id ~key |> ok in
      run_id, key, request, terminal
    in
    let bad_source = "let main input = Task.pure(no_such_developer_identifier)" in
    let _check_run, _check_key, _check_request, check =
      run
        ~source:bad_source
        ~kind:Check
        ~name:"receipt-check-rejected"
        ~requested_max_output_bytes:None
        ()
    in
    (match Option.map (S.Daemon.Developer.View.receipt check) ~f:R.value with
     | Some (Check (Rejected diagnostic)) ->
       [%test_eq: string] diagnostic.code "chatml.invalid_handler"
     | result ->
       raise_s [%sexp "expected bounded Check diagnostic", (result : R.value option)]);
    let long_value = String.make 1024 'x' in
    let limited_source = "let main input = Task.pure(input)" in
    let limited_run, limited_key, limited_request, limited =
      run
        ~input:(`String long_value)
        ~source:limited_source
        ~kind:Live_run
        ~name:"receipt-live-limited"
        ~requested_max_output_bytes:(Some 128)
        ()
    in
    (match Option.map (S.Daemon.Developer.View.receipt limited) ~f:R.value with
     | Some (Live (Limited error)) -> [%test_eq: P.Error.code] error.code Resource_limit
     | result -> raise_s [%sexp "expected bounded Live limit", (result : R.value option)]);
    (match S.Daemon.Developer.View.phase limited with
     | Agent_store.Developer_command_row.Phase.Terminal { execution_started = true; _ } ->
       ()
     | _ -> failwith "bounded Live command did not enter execution");
    let _failed_run, _failed_key, _failed_request, failed =
      run
        ~source:bad_source
        ~kind:Live_run
        ~name:"receipt-live-compile"
        ~requested_max_output_bytes:None
        ()
    in
    (match Option.map (S.Daemon.Developer.View.receipt failed) ~f:R.value with
     | Some (Live (Failed error)) -> [%test_eq: P.Error.code] error.code Invalid_state
     | result ->
       raise_s
         [%sexp "expected durable Live compilation failure", (result : R.value option)]);
    (match S.Daemon.Developer.View.phase failed with
     | Agent_store.Developer_command_row.Phase.Terminal { execution_started = true; _ } ->
       ()
     | _ -> failwith "captured Live failure was not durable after Start");
    current := false;
    (match
       S.Daemon.Developer.observe daemon ~actor ~run_id:limited_run ~key:limited_key
     with
     | Error { P.Error.code = Permission_denied; _ } -> ()
     | Error error ->
       raise_s [%sexp "wrong revoked actor denial", (error.code : P.Error.code)]
     | Ok _ -> failwith "revoked actor observed developer row");
    let seen =
      S.Daemon.Developer.observe
        daemon
        ~actor:reconnected
        ~run_id:limited_run
        ~key:limited_key
      |> ok
      |> Option.value_exn
    in
    assert (
      R.Id.equal (S.Daemon.Developer.View.id limited) (S.Daemon.Developer.View.id seen));
    let retry =
      S.Daemon.Developer.submit
        daemon
        ~actor:reconnected
        ~run_id:limited_run
        ~key:limited_key
        ~request:limited_request
        ~fresh_source:None
      |> ok
    in
    assert (
      R.Id.equal (S.Daemon.Developer.View.id seen) (S.Daemon.Developer.View.id retry));
    let without_live_scope =
      Agent_server_test_support.principal_with_scopes
        "pri_developer_receipts"
        (Set.remove scopes Developer_live_run)
      |> Operator_authorization.trusted_local
    in
    (match
       S.Daemon.Developer.submit
         daemon
         ~actor:without_live_scope
         ~run_id:limited_run
         ~key:limited_key
         ~request:limited_request
         ~fresh_source:None
     with
     | Error { P.Error.code = Permission_denied; _ } -> ()
     | Error error ->
       raise_s [%sexp "wrong missing scope denial", (error.code : P.Error.code)]
     | Ok _ -> failwith "unscoped actor submitted developer command");
    print_endline
      "Check diagnostic, Live failure and limit stayed durable; revoked reader denied");
  [%expect
    {| Check diagnostic, Live failure and limit stayed durable; revoked reader denied |}]
;;

let%expect_test "a disconnected Live waiter does not cancel its native effect" =
  let entered, signal_entered = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let released = ref false in
  let authorization_attempts = ref 0 in
  let authorize_native invocation binding =
    let name = (Chat_response.Tool_capability.reference binding).name in
    [%test_eq: string] name "read_file";
    assert (
      P.Invocation.equal_origin
        (P.Host_invocation.context invocation).origin
        P.Invocation.Script);
    incr authorization_attempts;
    Eio.Promise.resolve signal_entered ();
    Eio.Promise.await release;
    Ok ()
  in
  ignore
    (Fun.protect
       ~finally:(fun () -> if not !released then Eio.Promise.resolve signal_release ())
       (fun () ->
          with_daemon
            ~prompt_source:
              {|<developer>Developer native waiter fixture.</developer>
<tool name="read_file"/>|}
            ~native_sample:true
            ~authorize_native
            ~prepare_output:(function
              | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
              | _ -> Error (P.Error.invalid_request "unexpected native output"))
            ~timeout_seconds:15.
            ~after_shutdown:(fun (run_id, key, request, receipt_id, output) reopened ->
              let scopes = Set.add Agent_server_test_support.scopes Developer_live_run in
              let principal =
                Agent_server_test_support.principal_with_scopes
                  "pri_developer_waiter"
                  scopes
              in
              let reconnected = Operator_authorization.trusted_local principal in
              let observed =
                S.Daemon.Developer.observe reopened ~actor:reconnected ~run_id ~key
                |> ok
                |> Option.value_exn
              in
              assert (R.Id.equal receipt_id (S.Daemon.Developer.View.id observed));
              (match Option.map (S.Daemon.Developer.View.receipt observed) ~f:R.value with
               | Some (Live (Succeeded (`String reopened_output))) ->
                 [%test_eq: string] reopened_output output
               | result ->
                 raise_s [%sexp "unexpected reopened receipt", (result : R.value option)]);
              let retry =
                S.Daemon.Developer.submit
                  reopened
                  ~actor:reconnected
                  ~run_id
                  ~key
                  ~request
                  ~fresh_source:None
                |> ok
              in
              assert (R.Id.equal receipt_id (S.Daemon.Developer.View.id retry));
              assert (Int.equal !authorization_attempts 1);
              print_endline
                "reopened daemon returned historical receipt without effect replay")
            (fun daemon ->
               Eio.Switch.run (fun sw ->
                 let scopes =
                   Set.add Agent_server_test_support.scopes Developer_live_run
                 in
                 let principal =
                   Agent_server_test_support.principal_with_scopes
                     "pri_developer_waiter"
                     scopes
                 in
                 let actor = Operator_authorization.trusted_local principal in
                 let source =
                   {|let main input =
  Task.bind(Tool.call("read_file", `Object([{key="file"; value=`String("sample.txt")}])), fun result ->
    match result with
    | `Ok(value) -> Task.pure(value)
    | `Error(message) -> Task.fail(message))|}
                 in
                 let connection, _transport, expected =
                   open_document ~source daemon actor
                 in
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
                     ~prompt:
                       (Some (S.Catalog_identity.prompt_definition "restart.prompt"))
                     ~workspace:
                       (Some (S.Catalog_identity.workspace_definition "restart.workspace"))
                     ~entrypoint:"main"
                     ~input:(`String "waiter-result")
                     ~fixtures:None
                     ~requested_execution:None
                     ~requested_max_output_bytes:None
                     ~bounds
                     ~checkpoint:ignore
                   |> ok
                 in
                 let run_id = P.Id.Developer_run.create () in
                 let key =
                   P.Idempotency_key.of_string "native-disconnected-waiter" |> checked
                 in
                 let submitted =
                   S.Daemon.Developer.submit
                     daemon
                     ~actor
                     ~run_id
                     ~key
                     ~request
                     ~fresh_source:(Some (Document_script { connection }))
                   |> ok
                 in
                 Eio.Promise.await entered;
                 let waiter_cancel, signal_waiter_cancel = Eio.Promise.create () in
                 let waiter_done, signal_waiter_done = Eio.Promise.create () in
                 Eio.Fiber.fork ~sw (fun () ->
                   let result =
                     match
                       Eio.Cancel.sub (fun cancel ->
                         (* Fork runs this fiber to its first suspension. Promise
                            resolution does not yield, so cancellation reaches
                            await_terminal's cooperative await path. *)
                         Eio.Promise.resolve signal_waiter_cancel cancel;
                         ignore
                           (S.Daemon.Developer.await_terminal daemon ~actor ~run_id ~key
                            |> ok))
                     with
                     | exception Eio.Cancel.Cancelled _ -> `Cancelled
                     | () -> `Unexpected_success
                   in
                   Eio.Promise.resolve signal_waiter_done result);
                 let cancel = Eio.Promise.await waiter_cancel in
                 Eio.Cancel.cancel cancel Exit;
                 (match Eio.Promise.await waiter_done with
                  | `Cancelled -> ()
                  | `Unexpected_success -> failwith "disconnected waiter returned");
                 let pending =
                   S.Daemon.Developer.observe daemon ~actor ~run_id ~key
                   |> ok
                   |> Option.value_exn
                 in
                 assert (Option.is_none (S.Daemon.Developer.View.receipt pending));
                 released := true;
                 Eio.Promise.resolve signal_release ();
                 let terminal =
                   S.Daemon.Developer.await_terminal daemon ~actor ~run_id ~key |> ok
                 in
                 let output =
                   match
                     Option.map (S.Daemon.Developer.View.receipt terminal) ~f:R.value
                   with
                   | Some (Live (Succeeded (`String output))) -> output
                   | result ->
                     raise_s
                       [%sexp "unexpected disconnected result", (result : R.value option)]
                 in
                 assert (String.is_substring output ~substring:"sample.txt:1-1:");
                 assert (
                   String.is_substring output ~substring:"developer host native sample");
                 assert (Int.equal !authorization_attempts 1);
                 assert (
                   R.Id.equal
                     (S.Daemon.Developer.View.id submitted)
                     (S.Daemon.Developer.View.id terminal));
                 print_endline "waiter cancelled; native effect completed once";
                 run_id, key, request, S.Daemon.Developer.View.id terminal, output))));
  [%expect
    {|
    waiter cancelled; native effect completed once
    reopened daemon returned historical receipt without effect replay
    |}]
;;

let%expect_test "hosted REPL retains partial work and closes at key saturation" =
  with_daemon ~repl:true (fun daemon ->
    let module Repl = S.Daemon.Developer.Repl in
    let scopes = Set.add Agent_server_test_support.scopes Developer_repl in
    let principal =
      Agent_server_test_support.principal_with_scopes "pri_developer_repl" scopes
    in
    let actor = Operator_authorization.trusted_local principal in
    let reconnected = Operator_authorization.trusted_local principal in
    let foreign =
      Operator_authorization.trusted_local
        (Agent_server_test_support.principal_with_scopes "pri_foreign_repl" scopes)
    in
    let connection, _transport, expected = open_document daemon actor in
    let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
    let route = S.Daemon.Developer.Request.Route.Document_script { expected; selector } in
    let fresh_source =
      Some (S.Daemon.Developer.Request.Fresh_source.Document_script { connection })
    in
    let original =
      Repl.Original.create
        ~route
        ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
        ~prompt:(S.Catalog_identity.prompt_definition "restart.prompt")
        ~workspace:(S.Catalog_identity.workspace_definition "restart.workspace")
        ~requested_execution:None
        ~requested_max_output_bytes:None
        ~checkpoint:ignore
      |> ok
    in
    let run_id = P.Id.Developer_run.create () in
    let key name = P.Idempotency_key.of_string name |> checked in
    let view =
      Repl.create daemon ~actor ~run_id ~key:(key "repl-create") ~original ~fresh_source
      |> ok
    in
    let replay =
      Repl.create
        daemon
        ~actor:reconnected
        ~run_id
        ~key:(key "repl-create")
        ~original
        ~fresh_source:None
      |> ok
    in
    assert (Repl.Instance.equal (Repl.View.instance view) (Repl.View.instance replay));
    let foreign_denied =
      Result.is_error
        (Repl.create
           daemon
           ~actor:foreign
           ~run_id
           ~key:(key "repl-create")
           ~original
           ~fresh_source:None)
    in
    let instance = Repl.View.instance view in
    let epoch = Repl.View.epoch view in
    let first =
      Repl.submit
        daemon
        ~actor
        ~instance
        ~epoch
        ~expected_revision:(Repl.View.revision view)
        ~key:(key "repl-good")
        ~source:"let good = 1"
      |> ok
    in
    let first_finished =
      match Repl.Submission.status first with
      | Finished -> true
      | Need_more _ | Rejected _ | Stopped _ -> false
    in
    let second =
      Repl.submit
        daemon
        ~actor
        ~instance
        ~epoch
        ~expected_revision:(Repl.Submission.revision first)
        ~key:(key "repl-failed")
        ~source:"let local = 1\nlet doomed = 1 / 0"
      |> ok
    in
    let second_stopped =
      match Repl.Submission.status second with
      | Stopped _ -> true
      | Need_more _ | Rejected _ | Finished -> false
    in
    let truthful_limited =
      match Repl.Submission.details second, Repl.Submission.statements second with
      | Output_limited, [] -> true
      | Available, [ first; failed ] ->
        (match
           ( Chatml_repl_execution.Receipt.outcome first
           , Chatml_repl_execution.Receipt.outcome failed )
         with
         | Succeeded, Failed Runtime -> true
         | _ -> false)
      | _ -> false
    in
    let retry =
      Repl.submit
        daemon
        ~actor:reconnected
        ~instance
        ~epoch
        ~expected_revision:(Repl.Submission.revision first)
        ~key:(key "repl-failed")
        ~source:"let local = 1\nlet doomed = 1 / 0"
      |> ok
    in
    assert (
      Repl.Revision.equal
        (Repl.Submission.revision second)
        (Repl.Submission.revision retry));
    let known =
      Repl.observe daemon ~actor:reconnected ~instance ~key:(key "repl-good") |> ok
    in
    let reset =
      Repl.reset_and_wait
        daemon
        ~actor
        ~instance
        ~expected_epoch:epoch
        ~key:(key "repl-reset")
      |> ok
    in
    let old_key_replay =
      Repl.submit
        daemon
        ~actor:reconnected
        ~instance
        ~epoch
        ~expected_revision:(Repl.Submission.revision first)
        ~key:(key "repl-failed")
        ~source:"let local = 1\nlet doomed = 1 / 0"
      |> ok
    in
    let changed_old_key_conflicts =
      Result.is_error
        (Repl.submit
           daemon
           ~actor:reconnected
           ~instance
           ~epoch
           ~expected_revision:(Repl.Submission.revision first)
           ~key:(key "repl-failed")
           ~source:"let local = 2")
    in
    let reset_changed_epoch = not (Repl.Epoch.equal epoch (Repl.View.epoch reset)) in
    let fresh_denied =
      Result.is_error
        (Repl.submit
           daemon
           ~actor
           ~instance
           ~epoch:(Repl.View.epoch reset)
           ~expected_revision:(Repl.View.revision reset)
           ~key:(key "repl-other")
           ~source:"1")
    in
    let close = Repl.close_and_wait daemon ~actor ~instance ~key:(key "repl-close") in
    printf
      "same_instance=true foreign_denied=%b first_finished=%b second_stopped=%b \
       truthful_limited=%b completed=%d first_observed=%b reset_changed_epoch=%b \
       old_key_replay=%b changed_old_key_conflicts=%b fresh_denied=%b close_ok=%b\n"
      foreign_denied
      first_finished
      second_stopped
      truthful_limited
      (Repl.Submission.completed_statements second)
      (Option.is_some known)
      reset_changed_epoch
      (Repl.Revision.equal
         (Repl.Submission.revision second)
         (Repl.Submission.revision old_key_replay))
      changed_old_key_conflicts
      fresh_denied
      (Result.is_ok close));
  [%expect
    {| same_instance=true foreign_denied=true first_finished=true second_stopped=true truthful_limited=true completed=1 first_observed=true reset_changed_epoch=true old_key_replay=true changed_old_key_conflicts=true fresh_denied=true close_ok=true |}]
;;
