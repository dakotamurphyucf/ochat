let%test_module "Developer portable framing admission" =
  (module struct
    open! Core
    module P = Agent_protocol
    module D = Document_schema
    module L = P.Developer_common.Limits

    let must = function
      | Ok value -> value
      | Error _ -> failwith "expected valid portable fixture"
    ;;

    let bound ~bytes ~fields ~nodes ~depth =
      must
        (D.Limits.create
           ~max_bytes:bytes
           ~max_fields:fields
           ~max_nodes:nodes
           ~max_depth:depth)
    ;;

    let stored_receipt_limit = bound ~bytes:4096 ~fields:128 ~nodes:256 ~depth:8

    let limits =
      let request = bound ~bytes:4096 ~fields:128 ~nodes:256 ~depth:8 in
      let receipt =
        must (Developer_command_wire_receipt.required_limits ~stored:stored_receipt_limit)
      in
      let approval_page = bound ~bytes:30_000 ~fields:200 ~nodes:200 ~depth:8 in
      let response = bound ~bytes:40_000 ~fields:400 ~nodes:400 ~depth:12 in
      must
        (L.create
           ~request
           ~receipt
           ~approval_page
           ~response
           ~max_output_bytes:1024
           ~max_fixture_steps:4
           ~max_fixture_bytes:1024
           ~max_approval_display_bytes:256
           ~max_approval_decision_reason_bytes:256
           ~max_approval_page:2
           ~max_event_bytes:2048)
    ;;

    let id96 prefix = prefix ^ "_" ^ String.make (96 - String.length prefix - 1) 'a'
    let digest = Chatml_source.Digest.of_hex (String.make 64 'a') |> Or_error.ok_exn

    let%test_unit "maximal identity and escaped approval shapes fit complete responses" =
      let broker =
        must
          (Developer_approval_owner.Limits.create
             ~max_pending:2
             ~max_request_bytes:2048
             ~max_display_bytes:256
             ~max_decision_reason_bytes:256
             ~wait_seconds:1.)
      in
      let policy =
        must
          (Developer_endpoint.Policy.create
             ~wire_limits:limits
             ~max_connections:1
             ~max_watches_per_connection:2
             ~max_pending_hints_per_connection:2)
      in
      let budgets =
        must
          (Developer_command_budgets.Configuration.create
             ~compilation:(Bounded Chatml_compilation.default_limits)
             ~max_dependency_depth:32
             ~execution:Chatml_execution.default_limits
             ~max_output_bytes:1024
             ~receipt:stored_receipt_limit)
      in
      must
        (Developer_portable_admission.check
           ~policy
           ~initialize:Daemon.default_options.protocol_limits
           ~budgets
           ~approval:(Some broker)
           ~trace:None
           ~repl:None);
      let equal_receipt_limits =
        must
          (L.create
             ~request:(L.request limits)
             ~receipt:stored_receipt_limit
             ~response:(L.response limits)
             ~approval_page:(L.approval_page limits)
             ~max_output_bytes:(L.max_output_bytes limits)
             ~max_fixture_steps:(L.max_fixture_steps limits)
             ~max_fixture_bytes:(L.max_fixture_bytes limits)
             ~max_approval_display_bytes:(L.max_approval_display_bytes limits)
             ~max_approval_decision_reason_bytes:
               (L.max_approval_decision_reason_bytes limits)
             ~max_approval_page:(L.max_approval_page limits)
             ~max_event_bytes:(L.max_event_bytes limits))
      in
      let equal_receipt_policy =
        must
          (Developer_endpoint.Policy.create
             ~wire_limits:equal_receipt_limits
             ~max_connections:1
             ~max_watches_per_connection:2
             ~max_pending_hints_per_connection:2)
      in
      assert (
        Result.is_error
          (Developer_portable_admission.check
             ~policy:equal_receipt_policy
             ~initialize:Daemon.default_options.protocol_limits
             ~budgets
             ~approval:(Some broker)
             ~trace:None
             ~repl:None));
      let larger_host_budget =
        must
          (Developer_command_budgets.Configuration.create
             ~compilation:(Bounded Chatml_compilation.default_limits)
             ~max_dependency_depth:32
             ~execution:Chatml_execution.default_limits
             ~max_output_bytes:2048
             ~receipt:stored_receipt_limit)
      in
      assert (
        Result.is_error
          (Developer_portable_admission.check
             ~policy
             ~initialize:Daemon.default_options.protocol_limits
             ~budgets:larger_host_budget
             ~approval:(Some broker)
             ~trace:None
             ~repl:None));
      let command_id = must (P.Developer_command_id.of_string (String.make 256 '\001')) in
      let run_id = must (P.Id.Developer_run.of_string (id96 "dvr")) in
      let job_id = must (P.Id.Job.of_string (id96 "job")) in
      let owner =
        must
          (P.Developer_common.Owner.create
             ~host_id:(must (P.Id.Server.of_string (id96 "srv")))
             ~run_id
             ~epoch:P.Authoring.Limits.maximum_wire_integer
             ~principal_id:(must (P.Id.Principal.of_string (id96 "pri"))))
      in
      let receipt =
        must (P.Developer_receipt.create ~limits:(L.receipt limits) (Live Interrupted))
      in
      let view =
        must
          (P.Developer_common.View.create
             ~limits
             ~command_id
             ~owner
             ~key:(must (P.Idempotency_key.of_string (String.make 256 'k')))
             ~kind:Live_run
             ~phase:(Terminal { execution_started = true; receipt })
             ~request_fingerprint:digest
             ~capture_fingerprint:digest)
      in
      let bytes json =
        must (D.Json.validate_and_measure ~limits:(L.response limits) json)
      in
      let rec shape depth json =
        let children, own_fields =
          match json with
          | `Object fields -> List.map fields ~f:snd, List.length fields
          | `Array values -> values, 0
          | `Null | `True | `False | `Number _ | `String _ -> [], 0
        in
        List.fold
          children
          ~init:(1, own_fields, depth)
          ~f:(fun (nodes, fields, deepest) child ->
            let child_nodes, child_fields, child_depth = shape (depth + 1) child in
            nodes + child_nodes, fields + child_fields, Int.max deepest child_depth)
      in
      let view_json = P.Developer_common.View.to_json view in
      let receipt_json = P.Developer_receipt.to_json receipt in
      let view_bytes = bytes view_json in
      let receipt_bytes = bytes receipt_json in
      let view_nodes, view_fields, view_depth = shape 1 view_json in
      let receipt_nodes, receipt_fields, receipt_depth = shape 1 receipt_json in
      assert (view_bytes - receipt_bytes <= 4096);
      assert (view_nodes - receipt_nodes <= 32);
      assert (view_fields - receipt_fields <= 32);
      assert (view_depth - receipt_depth <= 3);
      ignore (must (P.Developer_wire.Response.to_json (Submit view) ~limits) : Jsonaf.t);
      let permission_id = must (P.Id.Permission.of_string (id96 "per")) in
      let second_permission_id =
        must (P.Id.Permission.of_string ("per_" ^ String.make 92 'b'))
      in
      let invocation_id = must (P.Id.Invocation.of_string (id96 "inv")) in
      let runtime_id = String.make 1024 '\001' in
      let redacted_display = String.make 256 '\001' in
      let pending id owner execution =
        must
          (P.Developer_approval.Pending.create
             ~limits
             ~id
             ~owner
             ~execution
             ~runtime_id
             ~manifest_sha256:(String.make 64 'a')
             ~redacted_display
             ~expires_at:(must (P.Timestamp.of_string "2026-10-18T00:00:00Z")))
      in
      let page =
        must
          (P.Developer_approval.Page.create
             ~limits
             [ pending
                 permission_id
                 (Command command_id)
                 (Native_invocation invocation_id)
             ; pending
                 second_permission_id
                 (Claimed_job
                    { job_id; attempt = P.Authoring.Limits.maximum_wire_integer })
                 (Claimed_model_job
                    { job_id; attempt = P.Authoring.Limits.maximum_wire_integer })
             ])
      in
      let page_json = P.Developer_approval.Page.to_json page in
      let page_bytes = bytes page_json in
      let page_nodes, page_fields, page_depth = shape 1 page_json in
      assert (page_bytes <= (2 * ((6 * 256) + (12 * 1024))) + 256);
      assert (page_nodes <= (2 * 32) + 16);
      assert (page_fields <= (2 * 24) + 8);
      assert (page_depth <= 8);
      let response_json =
        (must (P.Developer_wire.Response.to_json (Approval_list page) ~limits) : Jsonaf.t)
      in
      let response_nodes, response_fields, response_depth = shape 1 response_json in
      assert (bytes response_json - page_bytes <= 512);
      assert (response_nodes - page_nodes <= 8);
      assert (response_fields - page_fields <= 8);
      assert (response_depth - page_depth <= 2)
    ;;

    let repl_configuration () =
      let sites =
        Chatml.Chatml_syntax_sites.Limits.create ~max_sites:4096 ~max_name_bytes:16384
        |> Or_error.ok_exn
      in
      let parse =
        Chatml.Chatml_parse.Submission.Limits.create
          ~max_source_bytes:4096
          ~max_tokens:4096
          ~max_work_steps:100_000
          ~sites
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
      let evaluator_limits =
        Chatml_repl_execution.Limits.create
          ~max_preview_bytes:4096
          ~max_preview_nodes:1024
          ~max_preview_depth:16
          ~max_diagnostic_bytes:1024
          ~max_retained_tasks:8
        |> Or_error.ok_exn
      in
      let owner_limits =
        Developer_repl_epoch.Limits.create
          ~max_instances:2
          ~max_keys_per_instance:4
          ~max_submissions_per_epoch:8
          ~max_source_bytes:4096
          ~max_receipt_bytes:8192
          ~max_total_receipt_bytes:16384
          ~max_total_type_detail_bytes:(1024 * 1024)
          ~max_task_origins:8
          ~max_reference_page_bytes:4096
        |> Or_error.ok_exn
      in
      Developer_repl_epoch.Configuration.create
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
        ~evaluator_limits
        ~execution_policy:(Bounded Chatml_execution.default_limits)
        ~type_display_limits:
          (Chatml_type_display.Limits.create
             ~max_nodes:4096
             ~max_edges:8192
             ~max_depth:32
             ~max_work_steps:131072
             ~max_text_bytes:4096
             ~max_output_bytes:32768
           |> Or_error.ok_exn)
        ~limits:owner_limits
      |> Or_error.ok_exn
    ;;

    let%test_unit "REPL installation rejects structurally unusable pages and results" =
      let repl = repl_configuration () in
      let budgets =
        must
          (Developer_command_budgets.Configuration.create
             ~compilation:(Bounded Chatml_compilation.default_limits)
             ~max_dependency_depth:32
             ~execution:Chatml_execution.default_limits
             ~max_output_bytes:1024
             ~receipt:stored_receipt_limit)
      in
      let wire
            ?(page_items = 8)
            ?(page_bytes = 16 * 1024)
            ?(result_nodes = 8192)
            ?(response_nodes = 8300)
            ()
        =
        must
          (P.Developer_repl_wire.Limits.create
             ~request:(bound ~bytes:(128 * 1024) ~depth:24 ~fields:1024 ~nodes:2048)
             ~result:
               (bound ~bytes:(2 * 1024 * 1024) ~depth:20 ~fields:8192 ~nodes:result_nodes)
             ~page:(bound ~bytes:page_bytes ~depth:8 ~fields:128 ~nodes:128)
             ~response:
               (bound
                  ~bytes:(3 * 1024 * 1024)
                  ~depth:24
                  ~fields:8300
                  ~nodes:response_nodes)
             ~max_source_bytes:4096
             ~max_reference_bytes:384
             ~max_output_bytes:8192
             ~max_page_items:page_items
             ~max_event_bytes:4096
             ~max_registry_entries:32)
      in
      let admitted wire =
        let policy =
          must
            (Developer_endpoint.Policy.create
               ~wire_limits:limits
               ~max_connections:1
               ~max_watches_per_connection:2
               ~max_pending_hints_per_connection:2)
          |> fun policy -> must (Developer_endpoint.Policy.with_repl_wire policy wire)
        in
        Developer_portable_admission.check
          ~policy
          ~initialize:Daemon.default_options.protocol_limits
          ~budgets
          ~approval:None
          ~trace:None
          ~repl:(Some repl)
        |> Result.is_ok
      in
      assert (admitted (wire ()));
      assert (not (admitted (wire ~page_items:9 ())));
      assert (not (admitted (wire ~page_bytes:1024 ())));
      assert (not (admitted (wire ~result_nodes:4100 ())));
      assert (not (admitted (wire ~response_nodes:8192 ())))
    ;;

    let%test_unit "stored command variants project through bounded typed wire receipts" =
      let module Stored = Agent_store.Developer_command_receipt in
      let module Wire = P.Developer_receipt in
      let receipt_limit = bound ~bytes:4096 ~depth:16 ~fields:128 ~nodes:256 in
      let stored value = must (Stored.create value ~limits:receipt_limit ~poll:ignore) in
      let project value =
        stored value
        |> fun receipt ->
        must (Developer_command_wire_receipt.project receipt ~limits:receipt_limit)
        |> Wire.value
      in
      let source =
        Chatml_source.Id.create
          ~package_namespace:"developer-wire"
          ~relative_path:"entry.chatml"
        |> Or_error.ok_exn
      in
      let code = "let main input = missing" in
      let location =
        { Stored.Diagnostic.Location.source
        ; digest = Chatml_source.Digest.of_contents code ~checkpoint:ignore
        ; source_bytes = String.length code
        ; start_byte = 17
        ; end_byte = String.length code
        }
      in
      let diagnostic =
        { Stored.Diagnostic.code = "chatml.invalid_handler"
        ; message = "unknown name"
        ; location = Some location
        }
      in
      let check_location (value : Wire.Diagnostic.t) =
        match value.location with
        | None -> failwith "typed wire projection lost source attribution"
        | Some actual ->
          assert (Chatml_source.Id.equal actual.source source);
          assert (Chatml_source.Digest.equal actual.digest location.digest);
          assert (Int.equal actual.source_bytes location.source_bytes);
          assert (Int.equal actual.start_byte location.start_byte);
          assert (Int.equal actual.end_byte location.end_byte)
      in
      List.iter
        [ Stored.Fixture.Failure.Compile diagnostic
        ; Runtime diagnostic
        ; Invalid_result diagnostic
        ]
        ~f:(fun failure ->
          let completed =
            { Stored.Fixture.graph_fingerprint = digest
            ; fixture_digest = digest
            ; tool_selection_fingerprint = None
            ; matched_steps = 0
            ; logs = [ { Stored.Fixture.Log.level = Warn; message = "fixture warning" } ]
            ; outcome = Failed failure
            }
          in
          match project (Stored.Fixture (Completed completed)) with
          | Wire.Fixture (Completed value) ->
            assert (Int.equal (List.length value.logs) 1);
            assert (String.equal (List.hd_exn value.logs).message "fixture warning");
            (match value.outcome with
             | Failed (Compile value | Runtime value | Invalid_result value) ->
               check_location value
             | _ -> failwith "fixture diagnostic outcome changed")
          | _ -> failwith "fixture receipt changed family");
      let error = P.Error.invalid_request "live failure" in
      (match project (Stored.Live (Failed error)) with
       | Wire.Live (Failed actual) ->
         assert (P.Error.equal_code actual.code error.code);
         assert (String.equal actual.message error.message)
       | _ -> failwith "Live failure changed family");
      let tiny = bound ~bytes:32 ~depth:16 ~fields:128 ~nodes:256 in
      assert (
        Result.is_error
          (Developer_command_wire_receipt.project
             (stored (Stored.Check (Rejected diagnostic)))
             ~limits:tiny));
      (match
         Developer_command_wire_receipt.project
           (stored (Stored.Task Interrupted))
           ~limits:receipt_limit
       with
       | Error { code = Permission_denied; _ } -> ()
       | _ -> failwith "Task receipt entered Command wire family");
      let max_source =
        Chatml_source.Id.create
          ~package_namespace:(String.make 1024 'n')
          ~relative_path:"entry.chatml"
        |> Or_error.ok_exn
      in
      let maximal_location = { location with source = max_source } in
      let maximal_diagnostic = { diagnostic with location = Some maximal_location } in
      let maximal_value = Stored.Check (Rejected maximal_diagnostic) in
      let maximal_stored = stored maximal_value in
      let stored_bytes =
        must
          (D.Json.validate_and_measure
             ~limits:receipt_limit
             (Stored.to_json maximal_stored))
      in
      let wire_limit =
        must (Developer_command_wire_receipt.required_limits ~stored:receipt_limit)
      in
      let maximal_wire =
        must (Developer_command_wire_receipt.project maximal_stored ~limits:wire_limit)
      in
      let wire_bytes =
        must (D.Json.validate_and_measure ~limits:wire_limit (Wire.to_json maximal_wire))
      in
      assert (Int.equal (wire_bytes - stored_bytes) 362);
      let exact_stored_limit =
        bound ~bytes:stored_bytes ~depth:16 ~fields:128 ~nodes:256
      in
      ignore
        (must (Stored.create maximal_value ~limits:exact_stored_limit ~poll:ignore)
         : Stored.t);
      assert (
        Result.is_error
          (Developer_command_wire_receipt.project
             maximal_stored
             ~limits:exact_stored_limit));
      let required =
        must (Developer_command_wire_receipt.required_limits ~stored:exact_stored_limit)
      in
      assert (Int.equal (D.Limits.max_bytes required) wire_bytes);
      assert (Int.equal (D.Limits.max_depth required) 17);
      assert (Int.equal (D.Limits.max_fields required) 129);
      assert (Int.equal (D.Limits.max_nodes required) 257);
      ignore
        (must (Developer_command_wire_receipt.project maximal_stored ~limits:required)
         : Wire.t);
      let overflow = bound ~bytes:Int.max_value ~depth:16 ~fields:128 ~nodes:256 in
      assert (
        Result.is_error (Developer_command_wire_receipt.required_limits ~stored:overflow));
      let overlong_id =
        Chatml_source.Id.create
          ~package_namespace:(String.make 1025 'n')
          ~relative_path:"entry.chatml"
        |> Or_error.ok_exn
      in
      let overlong_source =
        Chatml_source.create
          overlong_id
          ~contents:code
          ~expected_digest:None
          ~max_bytes:(Some 4096)
          ~checkpoint:ignore
        |> Or_error.ok_exn
      in
      let overlong_prepared =
        Chatml_prepared_program.single_captured
          ~compiler_contract:digest
          ~source:overlong_source
          ~max_input_bytes:4096
          ~checkpoint:ignore
          ()
        |> must
      in
      assert (
        Result.is_error
          (Developer_command_wire_receipt.validate_prepared overlong_prepared))
    ;;

    let%test_unit "trace startup admits one advancing page and full RPC envelope" =
      let recorder =
        Chatml_trace.Recorder.Limits.create
          ~max_events:256
          ~max_normal_bytes:65_536
          ~max_terminal_bytes:1_024
        |> Or_error.ok_exn
      in
      let retention =
        Developer_trace_retention.Limits.create
          ~max_traces:4
          ~max_retained_bytes:(2 * 1024 * 1024)
          ~max_source_refs_per_trace:8
          ~max_provenance_bytes_per_trace:4_096
          ~max_expired_tombstones:4
          ~max_age_seconds:60.
          ~max_header_bytes:4_096
          ~recorder
        |> Or_error.ok_exn
      in
      let budgets =
        must
          (Developer_command_budgets.Configuration.create
             ~compilation:(Bounded Chatml_compilation.default_limits)
             ~max_dependency_depth:32
             ~execution:Chatml_execution.default_limits
             ~max_output_bytes:1_024
             ~receipt:stored_receipt_limit)
      in
      let trace_wire
            ?(request_bytes = 4_096)
            ?(page_bytes = 80_000)
            ?(response_bytes = 96_000)
            ?(cursor_bytes = 2_048)
            ()
        =
        must
          (P.Developer_trace_wire.Limits.create
             ~request:(bound ~bytes:request_bytes ~depth:12 ~fields:128 ~nodes:256)
             ~page:(bound ~bytes:page_bytes ~depth:12 ~fields:256 ~nodes:256)
             ~response:(bound ~bytes:response_bytes ~depth:16 ~fields:300 ~nodes:300)
             ~max_page_items:16
             ~max_cursor_bytes:cursor_bytes)
      in
      let policy wire =
        must
          (Developer_endpoint.Policy.create
             ~wire_limits:limits
             ~max_connections:1
             ~max_watches_per_connection:2
             ~max_pending_hints_per_connection:2)
        |> fun policy -> must (Developer_endpoint.Policy.with_trace_wire policy wire)
      in
      let admitted ?(initialize = Daemon.default_options.protocol_limits) wire trace =
        Developer_portable_admission.check
          ~policy:(policy wire)
          ~initialize
          ~budgets
          ~approval:None
          ~repl:None
          ~trace
        |> Result.is_ok
      in
      assert (admitted (trace_wire ()) (Some retention));
      assert (not (admitted (trace_wire ()) None));
      assert (not (admitted (trace_wire ~cursor_bytes:1 ()) (Some retention)));
      let module Trace_wire = P.Developer_trace_wire in
      let instance =
        P.Developer_repl_wire.Wire_id.Instance.of_string ("ri_" ^ String.make 93 'a')
        |> must
      in
      let epoch =
        P.Developer_repl_wire.Wire_id.Epoch.of_string ("re_" ^ String.make 93 'a') |> must
      in
      let key = P.Idempotency_key.of_string (String.make 256 'a') |> must in
      let locator = Trace_wire.Locator.Repl_statement { instance; epoch; key } in
      let fingerprint =
        Chatml_source.Digest.of_contents "trace-cursor" ~checkpoint:ignore
      in
      let cursor =
        Trace_wire.Cursor.create
          ~limits:(trace_wire ())
          ~locator
          ~execution_id:(String.make 128 'a')
          ~submission_fingerprint:fingerprint
          ~selection_fingerprint:fingerprint
          ~after_sequence:Int64.max_value
        |> must
      in
      let cursor_bytes =
        String.length (Jsonaf.to_string (Trace_wire.Cursor.to_json cursor))
      in
      let resumed =
        Trace_wire.Page_request.create
          ~limits:(trace_wire ())
          ~locator
          ~after:(Some cursor)
          ~limit:16
        |> must
      in
      let request_bytes =
        String.length (Jsonaf.to_string (Trace_wire.Page_request.to_json resumed))
      in
      assert (admitted (trace_wire ~cursor_bytes ~request_bytes ()) (Some retention));
      assert (
        not
          (admitted
             (trace_wire ~cursor_bytes:(cursor_bytes - 1) ~request_bytes ())
             (Some retention)));
      assert (
        not
          (admitted
             (trace_wire ~cursor_bytes ~request_bytes:(request_bytes - 1) ())
             (Some retention)));
      assert (not (admitted (trace_wire ~page_bytes:60_000 ()) (Some retention)));
      assert (not (admitted (trace_wire ~response_bytes:80_000 ()) (Some retention)));
      let initialize =
        { Daemon.default_options.protocol_limits with max_event_bytes = 96_511 }
      in
      assert (not (admitted ~initialize (trace_wire ()) (Some retention)))
    ;;
  end)
;;
