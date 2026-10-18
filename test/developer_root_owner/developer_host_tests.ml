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
      ?(receipt_read = fun _ _ -> true)
      ?(command_control = fun _ _ -> true)
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
    ~receipt_read
    ~command_control
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

let portable_wire_limits
      ?(max_approval_display_bytes = 256)
      ?(max_approval_decision_reason_bytes = 256)
      ?(approval_page_bytes = 30_000)
      ()
  =
  let bound ~bytes ~depth ~fields ~nodes =
    Document_schema.Limits.create
      ~max_bytes:bytes
      ~max_depth:depth
      ~max_fields:fields
      ~max_nodes:nodes
    |> checked
  in
  let receipt = bound ~bytes:(1024 * 1024) ~depth:16 ~fields:1024 ~nodes:2048 in
  P.Developer_common.Limits.create
    ~request:(bound ~bytes:4096 ~depth:16 ~fields:128 ~nodes:256)
    ~receipt
    ~response:(bound ~bytes:(2 * 1024 * 1024) ~depth:20 ~fields:1200 ~nodes:2400)
    ~approval_page:(bound ~bytes:approval_page_bytes ~depth:8 ~fields:200 ~nodes:200)
    ~max_output_bytes:65536
    ~max_fixture_steps:4
    ~max_fixture_bytes:4096
    ~max_approval_display_bytes
    ~max_approval_decision_reason_bytes
    ~max_approval_page:2
    ~max_event_bytes:2048
  |> ok
;;

let open_portable_client
      ?(features =
        [ P.Developer_common.Features.run; P.Developer_common.Features.approvals ])
      ~limits
      daemon
      actor
  =
  let module C = P.Developer_common in
  let context =
    S.Connection_context.create_authenticated
      ~actor
      ~connection_id:P.Id.Transaction.(create () |> to_string)
      ~transport:In_memory
      ~publish_notification:(fun _ -> ())
      ~max_attachments:8
  in
  let transport =
    Agent_client.In_memory.create_with_developer
      ~notifications:(Eio.Stream.create 8)
      ~request:(S.Dispatcher.dispatch_command (S.Daemon.dispatcher daemon) ~context)
      ~request_developer:(fun ~bound:_ request ->
        S.Dispatcher.dispatch_developer (S.Daemon.dispatcher daemon) ~context request
        |> Result.bind ~f:(fun response ->
          P.Developer_wire.Response.to_json response ~limits))
      ~close:(fun () -> S.Daemon.close_connection daemon context)
  in
  let implementation =
    P.Initialize.Implementation.create ~name:"approval-client" ~version:"1" |> ok
  in
  let initialize =
    P.Initialize.Request.create
      ~implementation
      ~protocol_min:P.Version.current
      ~protocol_max:P.Version.current
      ~features
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
   | Ok _ -> failwith "portable approval client initialization returned another method"
   | Error error ->
     raise_s [%sexp "portable approval client initialization failed", (error : P.Error.t)]);
  Agent_client.Developer_connection.create transport |> ok, transport
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

let repl_configuration ?(max_receipt_bytes = 8192) ?(max_keys_per_instance = 4) () =
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
    ~max_keys_per_instance
    ~max_submissions_per_epoch:8
    ~max_source_bytes:4096
    ~max_receipt_bytes
    ~max_total_receipt_bytes:(Int.max 16384 (max_receipt_bytes * 2))
    ~max_task_origins:4
  |> ok
;;

let with_daemon
      ?(tool_fixture = false)
      ?(repl = false)
      ?(portable = false)
      ?(approvals = false)
      ?(repl_receipt_bytes = 8192)
      ?(repl_keys_per_instance = 4)
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
              S.Daemon.Developer_configuration.with_repl
                developer
                (repl_configuration
                   ~max_receipt_bytes:repl_receipt_bytes
                   ~max_keys_per_instance:repl_keys_per_instance
                   ())
            else developer
          in
          let developer =
            if approvals
            then (
              let limits =
                S.Daemon.Developer_configuration.Shell_approvals.Limits.create
                  ~max_pending:2
                  ~max_request_bytes:2048
                  ~max_display_bytes:256
                  ~max_decision_reason_bytes:256
                  ~wait_seconds:2.
                |> ok
              in
              S.Daemon.Developer_configuration.with_shell_approvals
                developer
                (S.Daemon.Developer_configuration.Shell_approvals.create
                   ~limits
                   ~job_read:(fun _ _ -> true)
                   ~job_respond:(fun _ _ -> true)))
            else developer
          in
          let developer =
            if portable
            then
              S.Daemon.Developer_configuration.with_portable_wire
                developer
                (S.Daemon.Developer_configuration.Portable.create
                   ~wire_limits:(portable_wire_limits ())
                   ~max_connections:4
                   ~max_watches_per_connection:4
                   ~max_pending_hints_per_connection:4
                 |> ok)
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

let open_document
      ?(source = "let main input = Task.pure(input)")
      ?(features = [ P.Authoring.Features.core; P.Authoring.Features.semantic ])
      ?(publish_notification = fun _ -> ())
      ?(developer_reply = fun _ result -> result)
      daemon
      actor
  =
  let connection =
    S.Connection_context.create_authenticated
      ~actor
      ~connection_id:P.Id.Transaction.(create () |> to_string)
      ~transport:In_memory
      ~publish_notification
      ~max_attachments:8
  in
  let transport =
    Agent_client.In_memory.create_with_developer
      ~notifications:(Eio.Stream.create 8)
      ~request:
        (S.Dispatcher.dispatch_command (S.Daemon.dispatcher daemon) ~context:connection)
      ~request_developer:(fun ~bound:_ request ->
        let result =
          S.Dispatcher.dispatch_developer
            (S.Daemon.dispatcher daemon)
            ~context:connection
            request
          |> Result.bind ~f:(fun response ->
            P.Developer_wire.Response.to_json response ~limits:(portable_wire_limits ()))
        in
        developer_reply request result)
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
      ~features
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

let%expect_test "installed Developer endpoint keeps command and approval methods distinct"
  =
  with_daemon ~portable:true ~approvals:true (fun daemon ->
    let module W = P.Developer_wire in
    let module C = P.Developer_common in
    let scopes =
      Set.add
        (Set.add Agent_server_test_support.scopes Developer_check)
        Developer_live_run
    in
    let principal =
      Agent_server_test_support.principal_with_scopes "pri_portable_developer" scopes
    in
    let actor = Operator_authorization.trusted_local principal in
    let connection, transport, expected =
      open_document
        ~features:
          [ P.Authoring.Features.core
          ; P.Authoring.Features.semantic
          ; C.Features.run
          ; C.Features.approvals
          ; C.Features.events
          ]
        daemon
        actor
    in
    let dispatch request =
      S.Dispatcher.dispatch_developer
        (S.Daemon.dispatcher daemon)
        ~context:connection
        request
      |> ok
    in
    let capabilities =
      match dispatch W.Request.Capabilities with
      | W.Response.Capabilities value -> value
      | _ -> failwith "expected installed Developer capabilities"
    in
    List.iter
      [ C.Method.Submit; Observe; Await_terminal; Approval_list; Watch; Unwatch ]
      ~f:(fun method_ -> assert (C.Capabilities.supports capabilities method_));
    let limits = C.Capabilities.limits capabilities in
    let run_id = P.Id.Developer_run.create () in
    let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
    let route =
      C.Route.document_script ~limits:(C.Limits.request limits) ~expected ~selector |> ok
    in
    let key name = P.Idempotency_key.of_string name |> checked in
    let check_key = key "portable-check" in
    let live_key = key "portable-live" in
    let submit ~key ~kind ~prompt ~workspace ~input =
      C.Submit.create
        ~limits
        ~run_id
        ~key
        ~kind
        ~route
        ~target:One_off_v1
        ~prompt
        ~workspace
        ~entrypoint:"main"
        ~input
        ~fixtures:None
        ~requested_execution:None
        ~requested_max_output_bytes:None
      |> ok
    in
    let check =
      submit ~key:check_key ~kind:Check ~prompt:None ~workspace:None ~input:`Null
    in
    let submitted =
      match dispatch (W.Request.Submit check) with
      | W.Response.Submit view -> view
      | _ -> failwith "expected Check submission"
    in
    let lookup key = C.Lookup.create ~run_id ~key in
    let terminal key =
      match dispatch (W.Request.Await_terminal (lookup key)) with
      | W.Response.Await_terminal view -> view
      | _ -> failwith "expected terminal Developer view"
    in
    let checked_view = terminal check_key in
    (match C.View.phase checked_view with
     | Terminal { receipt; _ } ->
       (match P.Developer_receipt.value receipt with
        | Check Ready -> ()
        | _ -> failwith "expected portable Check Ready")
     | _ -> failwith "expected terminal portable Check");
    (match dispatch (W.Request.Observe (lookup check_key)) with
     | W.Response.Observe (Some _) -> ()
     | _ -> failwith "expected disclosed Check observation");
    let observation = W.Request.Observe (lookup check_key) in
    let envelope_id = P.Envelope.Request_id.of_json (`Number "1") |> ok in
    (match
       S.Dispatcher.dispatch_envelope
         (S.Daemon.dispatcher daemon)
         ~context:connection
         (P.Envelope.request
            ~id:envelope_id
            ~method_:(W.Request.method_name observation)
            ~params:(W.Request.params observation)
            ())
     with
     | Ok (Some (P.Envelope.Response { outcome = Ok json; _ })) ->
       (match
          W.Response.of_method_and_json
            ~limits
            ~method_:(W.Request.method_name observation)
            json
          |> ok
        with
        | Observe (Some view) ->
          assert (
            P.Developer_command_id.equal
              (C.View.command_id view)
              (C.View.command_id checked_view))
        | _ -> failwith "envelope and local Observe differ")
     | _ -> failwith "expected Developer envelope response");
    (match dispatch (W.Request.Watch (lookup check_key)) with
     | W.Response.Watch -> ()
     | _ -> failwith "expected exact-key Watch acknowledgement");
    (match dispatch (W.Request.Unwatch (lookup check_key)) with
     | W.Response.Unwatch -> ()
     | _ -> failwith "expected exact-key Unwatch acknowledgement");
    let live =
      submit
        ~key:live_key
        ~kind:Live_run
        ~prompt:(Some (S.Catalog_identity.prompt_definition "restart.prompt"))
        ~workspace:(Some (S.Catalog_identity.workspace_definition "restart.workspace"))
        ~input:`Null
    in
    ignore (dispatch (W.Request.Submit live) : W.Response.t);
    (match C.View.phase (terminal live_key) with
     | Terminal { receipt; _ } ->
       (match P.Developer_receipt.value receipt with
        | Live (Succeeded `Null) -> ()
        | _ -> failwith "expected portable Live success")
     | _ -> failwith "expected terminal portable Live");
    (match dispatch (W.Request.Approval_list run_id) with
     | W.Response.Approval_list page ->
       assert (List.is_empty (P.Developer_approval.Page.pending page))
     | _ -> failwith "expected installed approval page");
    (match
       Agent_client.Connection.request_without_history
         transport
         (Authoring (Context_close (P.Authoring.Context.Close_request.create ~expected)))
     with
     | Ok (Authoring (Context_close _)) -> ()
     | _ -> failwith "expected Authoring context closure");
    let repeated =
      match dispatch (W.Request.Submit check) with
      | W.Response.Submit view -> view
      | _ -> failwith "expected historical Check retry"
    in
    assert (
      P.Developer_command_id.equal
        (C.View.command_id submitted)
        (C.View.command_id repeated));
    let different_input =
      submit
        ~key:check_key
        ~kind:Check
        ~prompt:None
        ~workspace:None
        ~input:(`String "changed")
    in
    (match
       S.Dispatcher.dispatch_developer
         (S.Daemon.dispatcher daemon)
         ~context:connection
         (W.Request.Submit different_input)
     with
     | Error { code = Conflict; _ } -> ()
     | Error { code; _ } ->
       raise_s [%sexp "unexpected historical conflict code", (code : P.Error.code)]
     | Ok _ -> failwith "changed payload reused historical key");
    (match
       S.Dispatcher.dispatch_developer
         (S.Daemon.dispatcher daemon)
         ~context:connection
         (W.Request.Submit
            (submit
               ~key:(key "portable-new-key")
               ~kind:Check
               ~prompt:None
               ~workspace:None
               ~input:`Null))
     with
     | Error { code = Conflict; _ } -> ()
     | Error { code; _ } ->
       raise_s [%sexp "unexpected stale source denial", (code : P.Error.code)]
     | Ok _ -> failwith "closed source recaptured under new key");
    let wide_request =
      Document_schema.Limits.create
        ~max_bytes:8192
        ~max_depth:16
        ~max_fields:128
        ~max_nodes:256
      |> checked
    in
    let wider =
      C.Limits.create
        ~request:wide_request
        ~receipt:(C.Limits.receipt limits)
        ~response:(C.Limits.response limits)
        ~approval_page:(C.Limits.approval_page limits)
        ~max_output_bytes:(C.Limits.max_output_bytes limits)
        ~max_fixture_steps:(C.Limits.max_fixture_steps limits)
        ~max_fixture_bytes:(C.Limits.max_fixture_bytes limits)
        ~max_approval_display_bytes:(C.Limits.max_approval_display_bytes limits)
        ~max_approval_decision_reason_bytes:
          (C.Limits.max_approval_decision_reason_bytes limits)
        ~max_approval_page:(C.Limits.max_approval_page limits)
        ~max_event_bytes:(C.Limits.max_event_bytes limits)
      |> ok
    in
    let oversized =
      C.Submit.create
        ~limits:wider
        ~run_id
        ~key:(key "portable-oversized")
        ~kind:Check
        ~route
        ~target:One_off_v1
        ~prompt:None
        ~workspace:None
        ~entrypoint:"main"
        ~input:(`String (String.make 5000 'x'))
        ~fixtures:None
        ~requested_execution:None
        ~requested_max_output_bytes:None
      |> ok
    in
    match
      S.Dispatcher.dispatch_developer
        (S.Daemon.dispatcher daemon)
        ~context:connection
        (W.Request.Submit oversized)
    with
    | Error { code = Resource_limit; _ } -> ()
    | Error { code; _ } ->
      raise_s [%sexp "unexpected typed request bound", (code : P.Error.code)]
    | Ok _ -> failwith "broad typed DTO bypassed installed request limit");
  let entered, signal_entered = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  let changed, signal_changed = Eio.Promise.create () in
  let released = ref false in
  let change_seen = ref false in
  let captured_change = ref None in
  let watching = ref false in
  let native_attempts = ref 0 in
  let lost_submit_response = ref false in
  ignore
    (Fun.protect
       ~finally:(fun () -> if not !released then Eio.Promise.resolve signal_release ())
       (fun () ->
          with_daemon
            ~portable:true
            ~native_sample:true
            ~prompt_source:
              {|<developer>Developer watch transition fixture.</developer>
<tool name="read_file"/>|}
            ~authorize_native:(fun _ binding ->
              [%test_eq: string]
                (Chat_response.Tool_capability.reference binding).name
                "read_file";
              incr native_attempts;
              if Int.equal !native_attempts 1 then Eio.Promise.resolve signal_entered ();
              Eio.Promise.await release;
              Ok ())
            ~prepare_output:(function
              | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
              | _ -> Error (P.Error.invalid_request "unexpected native output"))
            ~timeout_seconds:15.
            (fun daemon ->
               let module W = P.Developer_wire in
               let module C = P.Developer_common in
               let scopes = Set.add Agent_server_test_support.scopes Developer_live_run in
               let actor =
                 Agent_server_test_support.principal_with_scopes
                   "pri_portable_watch"
                   scopes
                 |> Operator_authorization.trusted_local
               in
               let run_id = P.Id.Developer_run.create () in
               let key = P.Idempotency_key.of_string "portable-watch-live" |> checked in
               let notification = function
                 | P.Envelope.Notification { method_; params }
                   when String.equal method_ W.Change.method_name ->
                   let change =
                     W.Change.of_json ~limits:(portable_wire_limits ()) params |> ok
                   in
                   if
                     !watching
                     && P.Id.Developer_run.equal change.run_id run_id
                     && P.Idempotency_key.equal change.key key
                     && (match change.kind with
                         | W.Change.Command_changed -> true
                         | W.Change.Approval_changed | W.Change.Resync_required -> false)
                     && not !change_seen
                   then (
                     change_seen := true;
                     captured_change := Some (P.Envelope.Notification { method_; params });
                     Eio.Promise.resolve signal_changed ())
                 | P.Envelope.Notification _ | Request _ | Response _ -> ()
               in
               let source =
                 {|let main input =
  Task.bind(Tool.call("read_file", `Object([{key="file"; value=`String("sample.txt")}])), fun result ->
    match result with
    | `Ok(value) -> Task.pure(value)
    | `Error(message) -> Task.fail(message))|}
               in
               let connection, transport, expected =
                 open_document
                   ~source
                   ~features:
                     [ P.Authoring.Features.core
                     ; P.Authoring.Features.semantic
                     ; C.Features.run
                     ; C.Features.events
                     ]
                   ~publish_notification:notification
                   ~developer_reply:(fun request result ->
                     match request, result with
                     | W.Request.Submit _, Ok _ when not !lost_submit_response ->
                       lost_submit_response := true;
                       Error
                         (P.Error.create
                            Interrupted
                            ~message:"the submitted response was lost"
                            ~retryable:true
                            ())
                     | _ -> result)
                   daemon
                   actor
               in
               let limits = portable_wire_limits () in
               let route =
                 C.Route.document_script
                   ~limits:(C.Limits.request limits)
                   ~expected
                   ~selector:(P.Authoring.Analysis.Target.selected_script "s" |> ok)
                 |> ok
               in
               let submit_for key =
                 C.Submit.create
                   ~limits
                   ~run_id
                   ~key
                   ~kind:Live_run
                   ~route
                   ~target:One_off_v1
                   ~prompt:(Some (S.Catalog_identity.prompt_definition "restart.prompt"))
                   ~workspace:
                     (Some (S.Catalog_identity.workspace_definition "restart.workspace"))
                   ~entrypoint:"main"
                   ~input:`Null
                   ~fixtures:None
                   ~requested_execution:None
                   ~requested_max_output_bytes:None
                 |> ok
               in
               let submit = submit_for key in
               let dispatch request =
                 S.Dispatcher.dispatch_developer
                   (S.Daemon.dispatcher daemon)
                   ~context:connection
                   request
                 |> ok
               in
               let developer = Agent_client.Developer_connection.create transport |> ok in
               (match
                  Agent_client.Developer_connection.send
                    developer
                    (W.Request.Submit submit)
                with
                | Error { code = Interrupted; _ } -> ()
                | Error error ->
                  raise_s [%sexp "unexpected lost response error", (error : P.Error.t)]
                | Ok _ -> failwith "the actual Submit response was not lost");
               assert !lost_submit_response;
               let pending =
                 Agent_client.Developer_connection.pending_submits developer
                 |> List.hd_exn
               in
               Eio.Promise.await entered;
               let lookup = C.Lookup.create ~run_id ~key in
               (match
                  Agent_client.Developer_connection.send
                    developer
                    (W.Request.Watch lookup)
                  |> ok
                with
                | W.Response.Watch -> ()
                | _ -> failwith "expected held Live Watch acknowledgement");
               watching := true;
               released := true;
               Eio.Promise.resolve signal_release ();
               Eio.Promise.await changed;
               (* The actual hint was handed to this transport callback, but not
                  consumed as a client cursor. Reobserve the exact key after
                  Watch to recover the authoritative state. *)
               (match
                  Agent_client.Developer_connection.change_of_notification
                    developer
                    (Option.value_exn !captured_change)
                  |> ok
                with
                | Some change ->
                  assert (P.Id.Developer_run.equal change.run_id run_id);
                  assert (P.Idempotency_key.equal change.key key)
                | None -> failwith "actual Developer hint was not decoded");
               (match dispatch (W.Request.Await_terminal lookup) with
                | W.Response.Await_terminal view ->
                  (match C.View.phase view with
                   | Terminal { receipt; _ } ->
                     (match P.Developer_receipt.value receipt with
                      | Live (Succeeded (`String output)) ->
                        assert (
                          String.is_substring
                            output
                            ~substring:"developer host native sample")
                      | _ -> failwith "watched Live did not succeed")
                   | _ -> failwith "watched Live did not settle");
                  let observed =
                    Agent_client.Developer_connection.observe_pending developer pending
                    |> ok
                    |> Option.value_exn
                  in
                  assert (
                    P.Developer_command_id.equal
                      (C.View.command_id observed)
                      (C.View.command_id view));
                  assert (
                    Int.equal
                      (List.length
                         (Agent_client.Developer_connection.pending_submits developer))
                      1);
                  let retried =
                    Agent_client.Developer_connection.retry_pending developer pending
                    |> ok
                  in
                  assert (
                    P.Developer_command_id.equal
                      (C.View.command_id retried)
                      (C.View.command_id view));
                  assert (
                    Chatml_source.Digest.equal
                      (C.View.capture_fingerprint retried)
                      (C.View.capture_fingerprint view))
                | _ -> failwith "expected watched Live terminal view");
               assert (
                 List.is_empty
                   (Agent_client.Developer_connection.pending_submits developer));
               assert (Int.equal !native_attempts 1);
               let reobserved =
                 Agent_client.Developer_connection.watch_and_observe developer lookup
                 |> ok
                 |> Option.value_exn
               in
               (match C.View.phase reobserved with
                | Terminal { receipt; _ } ->
                  (match P.Developer_receipt.value receipt with
                   | Live (Succeeded _) -> ()
                   | _ -> failwith "lost hint recovery did not observe Live success")
                | _ -> failwith "lost hint recovery did not reach terminal view");
               let repeat_key =
                 P.Idempotency_key.of_string "portable-watch-repeat" |> checked
               in
               let repeated = submit_for repeat_key in
               let repeated_view =
                 match
                   Agent_client.Developer_connection.send
                     developer
                     (W.Request.Submit repeated)
                   |> ok
                 with
                 | W.Response.Submit view -> view
                 | _ -> failwith "different-key Repeat returned another method"
               in
               assert (
                 not
                   (P.Developer_command_id.equal
                      (C.View.command_id repeated_view)
                      (C.View.command_id reobserved)));
               assert (
                 Chatml_source.Digest.equal
                   (C.View.capture_fingerprint repeated_view)
                   (C.View.capture_fingerprint reobserved));
               let repeated_terminal =
                 match
                   Agent_client.Developer_connection.send
                     developer
                     (W.Request.Await_terminal (C.Lookup.create ~run_id ~key:repeat_key))
                   |> ok
                 with
                 | W.Response.Await_terminal view -> view
                 | _ -> failwith "different-key Repeat did not settle"
               in
               (match C.View.phase repeated_terminal with
                | Terminal { receipt; _ } ->
                  (match P.Developer_receipt.value receipt with
                   | Live (Succeeded _) -> ()
                   | _ -> failwith "different-key Repeat failed")
                | _ -> failwith "different-key Repeat did not reach terminal view");
               assert (Int.equal !native_attempts 2);
               (match
                  Agent_client.Connection.request_without_history
                    transport
                    (Authoring
                       (Context_close (P.Authoring.Context.Close_request.create ~expected)))
                with
                | Ok (Authoring (Context_close _)) -> ()
                | _ -> failwith "current Authoring context did not close");
               let stale =
                 submit_for (P.Idempotency_key.of_string "portable-watch-stale" |> checked)
               in
               (match
                  Agent_client.Developer_connection.send
                    developer
                    (W.Request.Submit stale)
                with
                | Error { code = Conflict; _ } -> ()
                | Error error ->
                  raise_s [%sexp "stale selector denial changed", (error : P.Error.t)]
                | Ok _ -> failwith "stale selector admitted another effect");
               let foreign =
                 Agent_server_test_support.principal_with_scopes
                   "pri_portable_watch_foreign"
                   scopes
                 |> Operator_authorization.trusted_local
               in
               let foreign_client, foreign_connection =
                 open_portable_client ~features:[ C.Features.run ] ~limits daemon foreign
               in
               (match
                  Agent_client.Developer_connection.send
                    foreign_client
                    (W.Request.Observe lookup)
                with
                | Error { code = Lease_stale; _ } -> ()
                | Error error ->
                  raise_s [%sexp "foreign principal denial changed", (error : P.Error.t)]
                | Ok _ -> failwith "foreign principal observed Live result");
               Agent_client.Connection.close foreign_connection;
               assert (Int.equal !native_attempts 2);
               match dispatch (W.Request.Unwatch lookup) with
               | W.Response.Unwatch -> ()
               | _ -> failwith "expected held Live Unwatch acknowledgement")));
  with_daemon ~portable:true ~tool_fixture:true (fun daemon ->
    let module W = P.Developer_wire in
    let module C = P.Developer_common in
    let scopes = Set.add Agent_server_test_support.scopes Developer_fixture_run in
    let principal =
      Agent_server_test_support.principal_with_scopes "pri_portable_fixture" scopes
    in
    let actor = Operator_authorization.trusted_local principal in
    let source =
      {|let run ctx input =
  let* result = Tool.call("fixture", input) in
  match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(message) -> Task.fail(message)|}
    in
    let connection, _transport, expected =
      open_document
        ~source
        ~features:
          [ P.Authoring.Features.core
          ; P.Authoring.Features.semantic
          ; C.Features.run
          ; C.Features.events
          ]
        daemon
        actor
    in
    let limits = portable_wire_limits () in
    let step =
      C.Fixture.Step.create
        ~limits:(C.Limits.request limits)
        ~tool_name:"fixture"
        ~input_schema:(`Object [])
        ~output_schema:(`Object [])
        ~expected_input:`Null
        ~response:(Return (`String "world"))
      |> ok
    in
    let fixtures =
      C.Fixture.create
        ~limits:(C.Limits.request limits)
        ~max_steps:(C.Limits.max_fixture_steps limits)
        ~max_bytes:(C.Limits.max_fixture_bytes limits)
        ~steps:[ step ]
      |> ok
    in
    let run_id = P.Id.Developer_run.create () in
    let key = P.Idempotency_key.of_string "portable-fixture" |> checked in
    let route =
      C.Route.document_script
        ~limits:(C.Limits.request limits)
        ~expected
        ~selector:(P.Authoring.Analysis.Target.selected_script "s" |> ok)
      |> ok
    in
    let submitted =
      C.Submit.create
        ~limits
        ~run_id
        ~key
        ~kind:Fixture_run
        ~route
        ~target:Tool_v1
        ~prompt:None
        ~workspace:None
        ~entrypoint:"run"
        ~input:`Null
        ~fixtures:(Some fixtures)
        ~requested_execution:None
        ~requested_max_output_bytes:None
      |> ok
    in
    (match
       S.Dispatcher.dispatch_developer
         (S.Daemon.dispatcher daemon)
         ~context:connection
         (W.Request.Submit submitted)
     with
     | Ok (W.Response.Submit _) -> ()
     | _ -> failwith "expected portable Fixture submission");
    match
      S.Dispatcher.dispatch_developer
        (S.Daemon.dispatcher daemon)
        ~context:connection
        (W.Request.Await_terminal (C.Lookup.create ~run_id ~key))
    with
    | Ok (W.Response.Await_terminal view) ->
      (match C.View.phase view with
       | Terminal { receipt; _ } ->
         (match P.Developer_receipt.value receipt with
          | Fixture
              (Completed
                 { outcome = Succeeded (`String "world")
                 ; tool_selection_fingerprint = Some _
                 ; matched_steps = 1
                 ; _
                 }) -> ()
          | _ -> failwith "expected selected portable Fixture result")
       | _ -> failwith "expected terminal portable Fixture")
    | _ -> failwith "expected portable Fixture terminal");
  print_endline
    "portable Check, Fixture and Live; watched transition; approvals and envelope \
     parity; historical retry without recapture";
  [%expect
    {| portable Check, Fixture and Live; watched transition; approvals and envelope parity; historical retry without recapture |}]
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

let%expect_test "hosted REPL Task persists one terminal and joins before reset" =
  (* The retained expression's type graph has a conservative charged bound
     near 430 KiB; keep its bounded receipt visible for the issued reference. *)
  let native_calls = ref 0 in
  let native_outputs = ref 0 in
  let cancel_entered, signal_cancel_entered = Eio.Promise.create () in
  let cancel_release, signal_cancel_release = Eio.Promise.create () in
  with_daemon
    ~repl:true
    ~repl_receipt_bytes:(512 * 1024)
    ~repl_keys_per_instance:8
    ~timeout_seconds:15.
    ~prompt_source:
      "<developer>Developer REPL Task fixture.</developer>\n<tool name=\"read_file\"/>"
    ~native_sample:true
    ~authorize_native:(fun invocation binding ->
      [%test_eq: string]
        (Chat_response.Tool_capability.reference binding).name
        "read_file";
      assert (
        P.Invocation.equal_origin
          (P.Host_invocation.context invocation).origin
          P.Invocation.Script);
      incr native_calls;
      if Int.equal !native_calls 2
      then (
        Eio.Promise.resolve signal_cancel_entered ();
        Eio.Promise.await cancel_release);
      Ok ())
    ~prepare_output:(function
      | Openai.Responses.Tool_output.Output.Text value ->
        incr native_outputs;
        Ok (`String value)
      | _ -> Error (P.Error.invalid_request "unexpected native output"))
    (fun daemon ->
       let module Repl = S.Daemon.Developer.Repl in
       let scopes =
         Set.add
           (Set.add Agent_server_test_support.scopes Developer_repl)
           Developer_live_run
       in
       let principal =
         Agent_server_test_support.principal_with_scopes "pri_repl_task" scopes
       in
       let actor = Operator_authorization.trusted_local principal in
       let connection, _transport, expected = open_document daemon actor in
       let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
       let route =
         S.Daemon.Developer.Request.Route.Document_script { expected; selector }
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
         Repl.create
           daemon
           ~actor
           ~run_id
           ~key:(key "task-create")
           ~original
           ~fresh_source:
             (Some
                (S.Daemon.Developer.Request.Fresh_source.Document_script { connection }))
         |> ok
       in
       let instance = Repl.View.instance view in
       let epoch = Repl.View.epoch view in
       let submitted =
         Repl.submit
           daemon
           ~actor
           ~instance
           ~epoch
           ~expected_revision:(Repl.View.revision view)
           ~key:(key "task-inert")
           ~source:"Task.pure(8)"
         |> ok
       in
       let reference =
         match Repl.Submission.task_references submitted with
         | reference :: _ -> reference
         | [] ->
           let finished =
             match Repl.Submission.status submitted with
             | Finished -> true
             | Need_more _ | Rejected _ | Stopped _ -> false
           in
           let limited =
             match Repl.Submission.details submitted with
             | Output_limited -> true
             | Available -> false
           in
           failwith
             (Printf.sprintf
                "REPL did not retain Task: finished=%b limited=%b completed=%d \
                 statements=%d handles=%d"
                finished
                limited
                (Repl.Submission.completed_statements submitted)
                (List.length (Repl.Submission.statements submitted))
                (List.length (Repl.Submission.task_handles submitted)))
       in
       let task_original =
         Repl.Task.Original.create
           ~reference
           ~requested_execution:None
           ~requested_max_output_bytes:None
           ~checkpoint:ignore
         |> ok
       in
       let initial =
         Repl.Task.submit
           daemon
           ~actor
           ~key:(key "task-run")
           ~original:task_original
           ~fresh_epoch:(Some (instance, epoch))
         |> ok
       in
       let terminal =
         Repl.Task.await_terminal daemon ~actor ~run_id ~key:(key "task-run") |> ok
       in
       let replay =
         Repl.Task.submit
           daemon
           ~actor
           ~key:(key "task-run")
           ~original:task_original
           ~fresh_epoch:None
         |> ok
       in
       let succeeded =
         match Repl.Task.receipt terminal with
         | Some receipt ->
           (match R.value receipt with
            | R.Task (R.Task.Succeeded { value_preview = Some "8" }) -> true
            | R.Task (R.Task.Succeeded { value_preview = Some _ | None })
            | R.Task _ | R.Check _ | R.Fixture _ | R.Live _ -> false)
         | None -> false
       in
       let same_row =
         Agent_store.Developer_command_row.same_identity
           (Repl.Task.row initial)
           (Repl.Task.row replay)
       in
       let native_task =
         Repl.submit
           daemon
           ~actor
           ~instance
           ~epoch
           ~expected_revision:(Repl.Submission.revision submitted)
           ~key:(key "task-native-expression")
           ~source:
             {|Tool.call("read_file", `Object([{key="file"; value=`String("sample.txt")}]))|}
         |> ok
       in
       let native_reference =
         Repl.Submission.task_references native_task |> List.hd_exn
       in
       let native_original =
         Repl.Task.Original.create
           ~reference:native_reference
           ~requested_execution:None
           ~requested_max_output_bytes:None
           ~checkpoint:ignore
         |> ok
       in
       [%test_eq: int] !native_calls 0;
       let _native_initial =
         Repl.Task.submit
           daemon
           ~actor
           ~key:(key "task-native-run")
           ~original:native_original
           ~fresh_epoch:(Some (instance, epoch))
         |> ok
       in
       let native_terminal =
         Repl.Task.await_terminal daemon ~actor ~run_id ~key:(key "task-native-run") |> ok
       in
       let native_succeeded =
         match Repl.Task.receipt native_terminal with
         | Some receipt ->
           (match R.value receipt with
            | R.Task (R.Task.Succeeded { value_preview = Some preview }) ->
              String.is_substring preview ~substring:"Ok"
              && String.is_substring preview ~substring:"developer host native sample"
            | R.Task _ | R.Check _ | R.Fixture _ | R.Live _ -> false)
         | None -> false
       in
       let native_replay =
         Repl.Task.submit
           daemon
           ~actor
           ~key:(key "task-native-run")
           ~original:native_original
           ~fresh_epoch:None
         |> ok
       in
       let native_same_row =
         Agent_store.Developer_command_row.same_identity
           (Repl.Task.row native_terminal)
           (Repl.Task.row native_replay)
       in
       let cancellation_task =
         Repl.submit
           daemon
           ~actor
           ~instance
           ~epoch
           ~expected_revision:(Repl.Submission.revision native_task)
           ~key:(key "task-cancel-expression")
           ~source:
             {|Tool.call("read_file", `Object([{key="file"; value=`String("sample.txt")}]))|}
         |> ok
       in
       let cancellation_original =
         Repl.Task.Original.create
           ~reference:(Repl.Submission.task_references cancellation_task |> List.hd_exn)
           ~requested_execution:None
           ~requested_max_output_bytes:None
           ~checkpoint:ignore
         |> ok
       in
       let _cancellation_initial =
         Repl.Task.submit
           daemon
           ~actor
           ~key:(key "task-cancel-run")
           ~original:cancellation_original
           ~fresh_epoch:(Some (instance, epoch))
         |> ok
       in
       Eio.Promise.await cancel_entered;
       let cancelled =
         Repl.Task.cancel_and_wait daemon ~actor ~run_id ~key:(key "task-cancel-run")
         |> ok
       in
       Eio.Promise.resolve signal_cancel_release ();
       let cancelled_terminal =
         match Agent_store.Developer_command_row.phase (Repl.Task.row cancelled) with
         | Terminal _ -> true
         | Reserved | Waiting_permission | Execution_started -> false
       in
       let cancelled_without_effect =
         match Repl.Task.receipt cancelled with
         | Some receipt ->
           (match R.value receipt with
            | R.Task R.Task.Cancelled -> Int.equal !native_outputs 1
            | R.Task
                ( R.Task.Interrupted
                | R.Task.Limited _
                | R.Task.Failed _
                | R.Task.Succeeded _ )
            | R.Check _ | R.Fixture _ | R.Live _ -> false)
         | None -> false
       in
       let reset =
         Repl.reset_and_wait
           daemon
           ~actor
           ~instance
           ~expected_epoch:epoch
           ~key:(key "task-reset")
         |> ok
       in
       printf
         "task_succeeded=%b same_row=%b native_succeeded=%b native_same_row=%b \
          native_calls=%d cancel_terminal=%b cancel_without_effect=%b \
          reset_changed_epoch=%b\n"
         succeeded
         same_row
         native_succeeded
         native_same_row
         !native_calls
         cancelled_terminal
         cancelled_without_effect
         (not (Repl.Epoch.equal epoch (Repl.View.epoch reset))));
  [%expect
    {| task_succeeded=true same_row=true native_succeeded=true native_same_row=true native_calls=2 cancel_terminal=true cancel_without_effect=true reset_changed_epoch=true |}]
;;
