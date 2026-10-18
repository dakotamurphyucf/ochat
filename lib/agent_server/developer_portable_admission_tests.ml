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

    let limits =
      let request = bound ~bytes:4096 ~fields:128 ~nodes:256 ~depth:8 in
      let receipt = bound ~bytes:4096 ~fields:128 ~nodes:256 ~depth:8 in
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
             ~receipt:(L.receipt limits))
      in
      must
        (Developer_portable_admission.check
           ~policy
           ~initialize:Daemon.default_options.protocol_limits
           ~budgets
           ~approval:(Some broker)
           ~repl:None);
      let larger_host_budget =
        must
          (Developer_command_budgets.Configuration.create
             ~compilation:(Bounded Chatml_compilation.default_limits)
             ~max_dependency_depth:32
             ~execution:Chatml_execution.default_limits
             ~max_output_bytes:2048
             ~receipt:(L.receipt limits))
      in
      assert (
        Result.is_error
          (Developer_portable_admission.check
             ~policy
             ~initialize:Daemon.default_options.protocol_limits
             ~budgets:larger_host_budget
             ~approval:(Some broker)
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
             ~receipt:(L.receipt limits))
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
          ~repl:(Some repl)
        |> Result.is_ok
      in
      assert (admitted (wire ()));
      assert (not (admitted (wire ~page_items:9 ())));
      assert (not (admitted (wire ~page_bytes:1024 ())));
      assert (not (admitted (wire ~result_nodes:4100 ())));
      assert (not (admitted (wire ~response_nodes:8192 ())))
    ;;
  end)
;;
