open! Core
module P = Agent_protocol
module D = Document_schema
module Dev = P.Developer

let deterministic = P.Id.Generator.create ~bytes:(fun n -> String.make n 'x')

let must = function
  | Ok value -> value
  | Error _ -> failwith "expected valid value"
;;

let bound bytes =
  must (D.Limits.create ~max_bytes:bytes ~max_depth:32 ~max_fields:256 ~max_nodes:512)
;;

let limits_with_output ~max_output_bytes ~receipt_bytes ~response_bytes =
  must
    (Dev.Limits.create
       ~request:(bound 4096)
       ~receipt:(bound receipt_bytes)
       ~response:(bound response_bytes)
       ~approval_page:(bound 256)
       ~max_output_bytes
       ~max_fixture_steps:4
       ~max_fixture_bytes:512
       ~max_approval_display_bytes:64
       ~max_approval_decision_reason_bytes:64
       ~max_approval_page:2
       ~max_event_bytes:128)
;;

let limits ~receipt_bytes ~response_bytes =
  limits_with_output ~max_output_bytes:128 ~receipt_bytes ~response_bytes
;;

let%test_unit "command ID preserves the historical 1..256 byte domain" =
  let values =
    List.init 300 ~f:(fun length ->
      String.init length ~f:(fun index -> Char.of_int_exn (index mod 256)))
    @ [ "a\nb"; "\x00" ]
  in
  List.iter values ~f:(fun bytes ->
    let expected = (not (String.is_empty bytes)) && String.length bytes <= 256 in
    let protocol = P.Developer_command_id.of_string bytes in
    let store = Agent_store.Developer_command_receipt.Id.of_string bytes in
    assert (Bool.equal expected (Result.is_ok protocol));
    assert (Bool.equal expected (Result.is_ok store));
    match protocol, store with
    | Ok protocol, Ok store ->
      assert (String.equal bytes (P.Developer_command_id.to_string protocol));
      assert (
        String.equal bytes (Agent_store.Developer_command_receipt.Id.to_string store))
    | Error _, Error (D.Error.Invalid_field { path = [ "id" ]; _ }) -> ()
    | _ -> failwith "receipt ID error channel or bytes changed")
;;

let%test_unit
    "receipt codec preserves kind and rejects null, unknown and duplicate fields"
  =
  let limits = bound 512 in
  let ready =
    must (Dev.Receipt.create ~limits (Dev.Receipt.Check Dev.Receipt.Check.Ready))
  in
  let decoded = must (Dev.Receipt.of_json ~limits (Dev.Receipt.to_json ready)) in
  assert (Dev.Receipt.equal ready decoded);
  let invalid =
    [ `Object [ "kind", `String "check"; "state", `Null ]
    ; `Object [ "kind", `String "check"; "state", `String "ready"; "other", `True ]
    ; `Object
        [ "kind", `String "check"; "kind", `String "check"; "state", `String "ready" ]
    ; `Object [ "kind", `String "live_run"; "state", `String "ready" ]
    ]
  in
  List.iter invalid ~f:(fun json ->
    assert (Result.is_error (Dev.Receipt.of_json ~limits json)));
  let nested_error =
    match P.Error.to_json (P.Error.invalid_request "failed") with
    | `Object fields -> `Object (("unknown", `True) :: fields)
    | _ -> assert false
  in
  assert (
    Result.is_error
      (Dev.Receipt.of_json
         ~limits
         (`Object
             [ "kind", `String "live_run"
             ; "state", `String "failed"
             ; "error", nested_error
             ])));
  assert (
    Result.is_error
      (Dev.Receipt.create
         ~limits:(bound 20)
         (Dev.Receipt.Live (Dev.Receipt.Live.Succeeded (`String (String.make 100 'x'))))))
;;

let%test_unit "fixture failure constructors roundtrip and reject invalid scalars" =
  let limits = bound 4096 in
  let digest = Chatml_source.Digest.of_contents "fixture" ~checkpoint:(fun () -> ()) in
  let diagnostic =
    must
      (Dev.Receipt.Diagnostic.create
         ~code:"fixture.mismatch"
         ~message:"bounded"
         ~location:None)
  in
  let module F = Dev.Receipt.Fixture in
  let failures =
    [ F.Failure.Compile diagnostic
    ; Fixture_mismatch { expected_step = 0; actual_tool = "tool" }
    ; Fixture_missing { next_step = 1 }
    ; Unsupported_effect { operation = "network" }
    ; Runtime diagnostic
    ; Invalid_result diagnostic
    ; Pending_without_owner
    ; Admission_revoked
    ; Output_limit
    ]
  in
  let make failure selection =
    F.completed
      ~graph_fingerprint:digest
      ~fixture_digest:digest
      ~tool_selection_fingerprint:selection
      ~matched_steps:0
      ~logs:[]
      ~outcome:(F.Failed failure)
  in
  List.iter failures ~f:(fun failure ->
    let completed = must (make failure (Some digest)) in
    let receipt =
      must (Dev.Receipt.create ~limits (Dev.Receipt.Fixture (F.Completed completed)))
    in
    let decoded = must (Dev.Receipt.of_json ~limits (Dev.Receipt.to_json receipt)) in
    assert (Dev.Receipt.equal receipt decoded));
  let bad =
    [ F.Failure.Fixture_mismatch { expected_step = -1; actual_tool = "tool" }
    ; Fixture_mismatch { expected_step = 0; actual_tool = "" }
    ; Fixture_missing { next_step = -1 }
    ; Unsupported_effect { operation = "" }
    ]
  in
  List.iter bad ~f:(fun failure -> assert (Result.is_error (make failure (Some digest))));
  assert (Result.is_error (make F.Failure.Pending_without_owner None));
  let completed =
    must
      (F.completed
         ~graph_fingerprint:digest
         ~fixture_digest:digest
         ~tool_selection_fingerprint:None
         ~matched_steps:0
         ~logs:[]
         ~outcome:(F.Succeeded `Null))
  in
  let receipt =
    must (Dev.Receipt.create ~limits (Dev.Receipt.Fixture (F.Completed completed)))
  in
  assert (
    Dev.Receipt.equal
      receipt
      (must (Dev.Receipt.of_json ~limits (Dev.Receipt.to_json receipt))))
;;

let%test_unit "complete response framing has its own advertised bound" =
  let ample = limits ~receipt_bytes:256 ~response_bytes:2048 in
  let receipt =
    must
      (Dev.Receipt.create
         ~limits:(Dev.Limits.receipt ample)
         (Dev.Receipt.Live (Dev.Receipt.Live.Succeeded (`String (String.make 80 'x')))))
  in
  let owner =
    must
      (Dev.Owner.create
         ~host_id:(P.Id.Server.create_with deterministic)
         ~run_id:(P.Id.Developer_run.create_with deterministic)
         ~epoch:1
         ~principal_id:(P.Id.Principal.create_with deterministic))
  in
  let digest = Chatml_source.Digest.of_contents "capture" ~checkpoint:(fun () -> ()) in
  let view =
    must
      (Dev.View.create
         ~limits:ample
         ~command_id:(must (Dev.Command_id.of_string "c"))
         ~owner
         ~key:(must (P.Idempotency_key.of_string "k"))
         ~kind:Dev.Kind.Live_run
         ~phase:(Dev.View.Terminal { execution_started = true; receipt })
         ~request_fingerprint:digest
         ~capture_fingerprint:digest)
  in
  let maximum = 9_007_199_254_740_991 in
  let host_id = P.Id.Server.create_with deterministic in
  let run_id = P.Id.Developer_run.create_with deterministic in
  let principal_id = P.Id.Principal.create_with deterministic in
  assert (Result.is_ok (Dev.Owner.create ~host_id ~run_id ~epoch:maximum ~principal_id));
  let oversized_owner =
    match
      must
        (P.Execution_owner.developer ~host_id ~run_id ~epoch:(maximum + 1) ~principal_id)
    with
    | P.Execution_owner.Developer owner -> owner
    | Conversation _ -> assert false
  in
  assert (
    Result.is_error
      (Dev.View.create
         ~limits:ample
         ~command_id:(Dev.View.command_id view)
         ~owner:oversized_owner
         ~key:(Dev.View.key view)
         ~kind:Dev.Kind.Live_run
         ~phase:(Dev.View.Terminal { execution_started = true; receipt })
         ~request_fingerprint:digest
         ~capture_fingerprint:digest));
  let response = Dev.Wire.Response.Observe (Some view) in
  let json = must (Dev.Wire.Response.to_json response ~limits:ample) in
  let decoded =
    must
      (Dev.Wire.Response.of_method_and_json
         ~limits:ample
         ~method_:"developer.observe"
         json)
  in
  assert (String.equal (Dev.Wire.Response.method_name decoded) "developer.observe");
  assert (
    Result.is_error
      (Dev.Wire.Response.of_method_and_json
         ~limits:ample
         ~method_:"developer.submit"
         json));
  let tight = limits ~receipt_bytes:256 ~response_bytes:256 in
  assert (Result.is_error (Dev.Wire.Response.to_json response ~limits:tight));
  assert (
    Result.is_error
      (Dev.Wire.Response.of_method_and_json
         ~limits:tight
         ~method_:"developer.observe"
         json));
  let narrow_receipt =
    limits_with_output ~max_output_bytes:32 ~receipt_bytes:64 ~response_bytes:2048
  in
  assert (Result.is_error (Dev.Wire.Response.to_json response ~limits:narrow_receipt));
  assert (
    Result.is_error
      (Dev.Wire.Response.of_method_and_json
         ~limits:narrow_receipt
         ~method_:"developer.observe"
         json))
;;

let%test_unit "execution limit acceptance agrees with the installed ChatML validator" =
  let base = Chatml_execution.default_limits in
  let agreed (x : Chatml_execution.limits) =
    let portable =
      Dev.Execution_limits.create
        ~fuel:x.fuel
        ~max_tasks:x.max_tasks
        ~wall_seconds:x.wall_seconds
        ~max_value_bytes:x.max_value_bytes
        ~max_array_items:x.max_array_items
        ~max_depth:x.max_depth
        ~allocation_bytes:x.allocation_bytes
        ~max_calls:x.max_calls
        ~max_invocation_depth:x.max_invocation_depth
    in
    assert (Bool.equal (Chatml_execution.valid_limits x) (Result.is_ok portable));
    match portable with
    | Error _ -> ()
    | Ok portable ->
      let json = Dev.Execution_limits.to_json portable in
      assert (Result.is_ok (Dev.Execution_limits.of_json json))
  in
  List.iter
    [ base
    ; { base with fuel = 0 }
    ; { base with max_tasks = -1 }
    ; { base with wall_seconds = Float.nan }
    ; { base with max_value_bytes = 0 }
    ; { base with max_array_items = 0 }
    ; { base with max_depth = 0 }
    ; { base with allocation_bytes = 0 }
    ; { base with max_calls = -1 }
    ; { base with max_invocation_depth = 0 }
    ; { base with max_tasks = 0; max_calls = 0 }
    ]
    ~f:agreed;
  let largest = 9_007_199_254_740_991 in
  assert (
    Result.is_ok
      (Dev.Execution_limits.create
         ~fuel:largest
         ~max_tasks:base.max_tasks
         ~wall_seconds:base.wall_seconds
         ~max_value_bytes:base.max_value_bytes
         ~max_array_items:base.max_array_items
         ~max_depth:base.max_depth
         ~allocation_bytes:base.allocation_bytes
         ~max_calls:base.max_calls
         ~max_invocation_depth:base.max_invocation_depth));
  assert (Chatml_execution.valid_limits { base with fuel = largest + 1 });
  assert (
    Result.is_error
      (Dev.Execution_limits.create
         ~fuel:(largest + 1)
         ~max_tasks:base.max_tasks
         ~wall_seconds:base.wall_seconds
         ~max_value_bytes:base.max_value_bytes
         ~max_array_items:base.max_array_items
         ~max_depth:base.max_depth
         ~allocation_bytes:base.allocation_bytes
         ~max_calls:base.max_calls
         ~max_invocation_depth:base.max_invocation_depth))
;;

let%test_unit "approval page rejects overflow without a partial snapshot" =
  let page_bound = bound 2048 in
  let caps =
    must
      (Dev.Limits.create
         ~request:(bound 4096)
         ~receipt:(bound 256)
         ~response:(bound 4096)
         ~approval_page:page_bound
         ~max_output_bytes:128
         ~max_fixture_steps:4
         ~max_fixture_bytes:512
         ~max_approval_display_bytes:64
         ~max_approval_decision_reason_bytes:64
         ~max_approval_page:2
         ~max_event_bytes:128)
  in
  let job_id = P.Id.Job.create_with deterministic in
  let pending =
    must
      (Dev.Approval.Pending.create
         ~limits:caps
         ~id:(P.Id.Permission.create_with deterministic)
         ~owner:(Dev.Approval.Owner.Claimed_job { job_id; attempt = 1 })
         ~execution:(Dev.Approval.Execution.Claimed_model_job { job_id; attempt = 1 })
         ~runtime_id:"shell"
         ~manifest_sha256:(String.make 64 'a')
         ~redacted_display:"approval"
         ~expires_at:(P.Timestamp.now ()))
  in
  let native =
    Dev.Approval.Execution.Native_invocation (P.Id.Invocation.create_with deterministic)
  in
  let claimed = Dev.Approval.Owner.Claimed_job { job_id; attempt = 1 } in
  let command = Dev.Approval.Owner.Command (must (Dev.Command_id.of_string "command")) in
  List.iter
    [ command, native, true
    ; claimed, native, true
    ; claimed, Dev.Approval.Execution.Claimed_model_job { job_id; attempt = 1 }, true
    ; command, Dev.Approval.Execution.Claimed_model_job { job_id; attempt = 1 }, false
    ; claimed, Dev.Approval.Execution.Claimed_model_job { job_id; attempt = 2 }, false
    ]
    ~f:(fun (owner, execution, expected) ->
      let result =
        Dev.Approval.Pending.create
          ~limits:caps
          ~id:(P.Id.Permission.create_with deterministic)
          ~owner
          ~execution
          ~runtime_id:"shell"
          ~manifest_sha256:(String.make 64 'a')
          ~redacted_display:"approval"
          ~expires_at:(P.Timestamp.now ())
      in
      assert (Bool.equal expected (Result.is_ok result));
      Option.iter (Result.ok result) ~f:(fun pending ->
        assert (
          Result.is_ok
            (Dev.Approval.Pending.of_json
               ~limits:caps
               (Dev.Approval.Pending.to_json pending)))));
  assert (Result.is_ok (Dev.Approval.Page.create ~limits:caps [ pending ]));
  assert (
    Result.is_error (Dev.Approval.Page.create ~limits:caps [ pending; pending; pending ]));
  let json = `Object [ "pending", `Array [ Dev.Approval.Pending.to_json pending ] ] in
  assert (Result.is_ok (Dev.Approval.Page.of_json ~limits:caps json));
  let broader =
    must
      (Dev.Limits.create
         ~request:(bound 4096)
         ~receipt:(bound 256)
         ~response:(bound 4096)
         ~approval_page:page_bound
         ~max_output_bytes:128
         ~max_fixture_steps:4
         ~max_fixture_bytes:512
         ~max_approval_display_bytes:256
         ~max_approval_decision_reason_bytes:64
         ~max_approval_page:2
         ~max_event_bytes:128)
  in
  let long_pending =
    must
      (Dev.Approval.Pending.create
         ~limits:broader
         ~id:(P.Id.Permission.create_with deterministic)
         ~owner:(Dev.Approval.Owner.Claimed_job { job_id; attempt = 1 })
         ~execution:(Dev.Approval.Execution.Claimed_model_job { job_id; attempt = 1 })
         ~runtime_id:"shell"
         ~manifest_sha256:(String.make 64 'a')
         ~redacted_display:(String.make 100 'x')
         ~expires_at:(P.Timestamp.now ()))
  in
  let broad_page = must (Dev.Approval.Page.create ~limits:broader [ long_pending ]) in
  let response = Dev.Wire.Response.Approval_list broad_page in
  assert (Result.is_ok (Dev.Wire.Response.to_json response ~limits:broader));
  assert (Result.is_error (Dev.Wire.Response.to_json response ~limits:caps))
;;

let%test_unit "request methods and capabilities are closed and bounded" =
  let limits = limits ~receipt_bytes:256 ~response_bytes:2048 in
  let lookup =
    Dev.Lookup.create
      ~run_id:(P.Id.Developer_run.create_with deterministic)
      ~key:(must (P.Idempotency_key.of_string "retry"))
  in
  let request = Dev.Wire.Request.Observe lookup in
  let decoded =
    must
      (Dev.Wire.Request.of_method_and_params
         ~limits
         ~method_:(Dev.Wire.Request.method_name request)
         ~params:(Dev.Wire.Request.params request))
  in
  assert (String.equal (Dev.Wire.Request.method_name decoded) "developer.observe");
  assert (
    Result.is_error
      (Dev.Wire.Request.of_method_and_params
         ~limits
         ~method_:"developer.unknown"
         ~params:(`Object [])));
  assert (
    Result.is_error
      (Dev.Wire.Request.of_method_and_params
         ~limits
         ~method_:"developer.observe"
         ~params:(`Object [ "run_id", `Null; "key", `String "retry" ])));
  let caps =
    must
      (Dev.Capabilities.create ~format_version:1 ~methods:[ "developer.observe" ] ~limits)
  in
  assert (Dev.Capabilities.supports caps Dev.Method.Observe);
  assert (not (Dev.Capabilities.supports caps Dev.Method.Submit));
  assert (
    Result.is_error
      (Dev.Capabilities.create ~format_version:1 ~methods:[ "developer.unknown" ] ~limits));
  let maximum = 9_007_199_254_740_991 in
  List.iter
    [ 0; 1; maximum; maximum + 1; Int.max_value ]
    ~f:(fun nodes ->
      let nested =
        D.Limits.create ~max_bytes:4096 ~max_depth:32 ~max_fields:256 ~max_nodes:nodes
      in
      assert (Bool.equal (nodes > 0) (Result.is_ok nested));
      match nested with
      | Error _ -> ()
      | Ok request ->
        let candidate =
          Dev.Limits.create
            ~request
            ~receipt:(Dev.Limits.receipt limits)
            ~response:(Dev.Limits.response limits)
            ~approval_page:(Dev.Limits.approval_page limits)
            ~max_output_bytes:128
            ~max_fixture_steps:4
            ~max_fixture_bytes:512
            ~max_approval_display_bytes:64
            ~max_approval_decision_reason_bytes:64
            ~max_approval_page:2
            ~max_event_bytes:128
        in
        assert (Bool.equal (nodes <= maximum) (Result.is_ok candidate));
        Option.iter (Result.ok candidate) ~f:(fun admitted ->
          assert (Result.is_ok (Dev.Limits.of_json (Dev.Limits.to_json admitted)))))
;;

let%test_unit "key-scoped watch and change hints retain method and bounds" =
  let limits = limits ~receipt_bytes:256 ~response_bytes:2048 in
  let run_id = P.Id.Developer_run.create_with deterministic in
  let key = must (P.Idempotency_key.of_string "watched") in
  let lookup = Dev.Lookup.create ~run_id ~key in
  List.iter
    [ Dev.Wire.Request.Watch lookup; Dev.Wire.Request.Unwatch lookup ]
    ~f:(fun request ->
      let method_ = Dev.Wire.Request.method_name request in
      let decoded =
        must
          (Dev.Wire.Request.of_method_and_params
             ~limits
             ~method_
             ~params:(Dev.Wire.Request.params request))
      in
      assert (String.equal method_ (Dev.Wire.Request.method_name decoded));
      assert (
        Result.is_error
          (Dev.Wire.Request.of_method_and_params
             ~limits
             ~method_
             ~params:(`Object [ "run_id", P.Id.Developer_run.to_json run_id ]))));
  List.iter
    [ Dev.Wire.Change.Command_changed
    ; Dev.Wire.Change.Approval_changed
    ; Dev.Wire.Change.Resync_required
    ]
    ~f:(fun kind ->
      let change = must (Dev.Wire.Change.create ~limits ~run_id ~key ~kind) in
      let restored =
        must (Dev.Wire.Change.of_json ~limits (Dev.Wire.Change.to_json change))
      in
      assert (P.Id.Developer_run.equal restored.run_id run_id);
      assert (P.Idempotency_key.equal restored.key key);
      assert (String.equal Dev.Wire.Change.method_name "developer.changed"));
  assert (
    Result.is_error
      (Dev.Wire.Change.of_json
         ~limits
         (`Object
             [ "run_id", P.Id.Developer_run.to_json run_id
             ; "kind", `String "approval_changed"
             ])))
;;

let%test_unit "routes and submissions roundtrip under the effective request policy" =
  let limits = limits ~receipt_bytes:256 ~response_bytes:2048 in
  let request_limit = Dev.Limits.request limits in
  let path text = Source.Portable_path.create text |> Or_error.ok_exn in
  let source_id =
    Chatml_source.Id.create
      ~package_namespace:"developer-wire"
      ~relative_path:"source.chatmd"
    |> Or_error.ok_exn
  in
  let contents = "<developer>example</developer>" in
  let source =
    must
      (P.Authoring.Source.create
         ~id:source_id
         ~digest:(Chatml_source.Digest.of_contents contents ~checkpoint:Fn.id)
         ~byte_length:(String.length contents)
         ~editor_revision:None)
  in
  let context =
    must
      (P.Authoring.Context.create
         ~id:(P.Id.Authoring_context.create_with deterministic)
         ~host:(P.Id.Server.create_with deterministic)
         ~origin:
           (P.Authoring.Context.Supplied_document { source; default_environment = None })
         ~generation:0
         ~basis:(P.Authoring.Context.Document { source; selection = None })
         ~basis_fingerprint:(Chatml_source.Digest.of_contents "basis" ~checkpoint:Fn.id)
         ~editable_sources:[])
  in
  let expected = P.Authoring.Context.Expected.of_view context in
  let routes =
    [ must
        (Dev.Route.local_project
           ~limits:request_limit
           ~root_index:0
           ~dependency_root_indices:[ 1 ]
           ~project:(path "project.json")
           ~selection:
             (Chatml_project.Selection.Entry { name = "main"; environment = None }))
    ; must
        (Dev.Route.local_source
           ~limits:request_limit
           ~root_index:0
           ~member:(path "source.chatmd"))
    ; must (Dev.Route.authoring_project ~limits:request_limit ~expected)
    ; must
        (Dev.Route.document_script
           ~limits:request_limit
           ~expected
           ~selector:P.Authoring.Analysis.Target.project)
    ]
  in
  let run_id = P.Id.Developer_run.create_with deterministic in
  let key = must (P.Idempotency_key.of_string "route-roundtrip") in
  let step =
    must
      (Dev.Fixture.Step.create
         ~limits:request_limit
         ~tool_name:"echo"
         ~input_schema:`Null
         ~output_schema:`Null
         ~expected_input:`Null
         ~response:(Dev.Fixture.Response.Return `Null))
  in
  let fixture =
    must
      (Dev.Fixture.create
         ~limits:request_limit
         ~max_steps:4
         ~max_bytes:512
         ~steps:[ step ])
  in
  List.iter routes ~f:(fun route ->
    let json = Dev.Route.to_json route in
    let restored = must (Dev.Route.of_json ~limits:request_limit json) in
    assert (
      String.equal (Jsonaf.to_string json) (Jsonaf.to_string (Dev.Route.to_json restored)));
    List.iter
      [ Dev.Kind.Check, Dev.Target.One_off_v1, None, None
      ; Dev.Kind.Fixture_run, Dev.Target.Tool_v1, Some fixture, None
      ; Dev.Kind.Live_run, Dev.Target.One_off_v1, None, Some `Null
      ]
      ~f:(fun (kind, target, fixtures, live_input) ->
        let input = Option.value live_input ~default:`Null in
        let prompt, workspace =
          match kind with
          | Dev.Kind.Live_run ->
            ( Some (P.Id.Prompt_definition.create_with deterministic)
            , Some (P.Id.Workspace_definition.create_with deterministic) )
          | Check | Fixture_run -> None, None
        in
        let submit =
          must
            (Dev.Submit.create
               ~limits
               ~run_id
               ~key
               ~kind
               ~route
               ~target
               ~prompt
               ~workspace
               ~entrypoint:"main"
               ~input
               ~fixtures
               ~requested_execution:None
               ~requested_max_output_bytes:None)
        in
        let request = Dev.Wire.Request.Submit submit in
        let params = Dev.Wire.Request.params request in
        let restored =
          must
            (Dev.Wire.Request.of_method_and_params
               ~limits
               ~method_:(Dev.Wire.Request.method_name request)
               ~params)
        in
        assert (
          String.equal
            (Jsonaf.to_string params)
            (Jsonaf.to_string (Dev.Wire.Request.params restored)));
        match params with
        | `Object fields ->
          assert (
            Result.is_error
              (Dev.Submit.of_json
                 ~limits
                 (`Object (List.Assoc.remove fields "input" ~equal:String.equal))));
          assert (
            Result.is_error
              (Dev.Submit.of_json ~limits (`Object (("prompt", `Null) :: fields))))
        | _ -> assert false));
  let narrower =
    must
      (Dev.Limits.create
         ~request:request_limit
         ~receipt:(Dev.Limits.receipt limits)
         ~response:(Dev.Limits.response limits)
         ~approval_page:(Dev.Limits.approval_page limits)
         ~max_output_bytes:128
         ~max_fixture_steps:1
         ~max_fixture_bytes:100
         ~max_approval_display_bytes:64
         ~max_approval_decision_reason_bytes:64
         ~max_approval_page:2
         ~max_event_bytes:128)
  in
  assert (
    Result.is_error
      (Dev.Submit.create
         ~limits:narrower
         ~run_id
         ~key
         ~kind:Dev.Kind.Fixture_run
         ~route:(List.hd_exn routes)
         ~target:Dev.Target.Tool_v1
         ~prompt:None
         ~workspace:None
         ~entrypoint:"main"
         ~input:`Null
         ~fixtures:(Some fixture)
         ~requested_execution:None
         ~requested_max_output_bytes:None))
;;

let%test_unit "REPL Task codec preserves all phases and current nested bounds" =
  let module W = P.Developer_repl_wire in
  let wire max_output_bytes =
    must
      (W.Limits.create
         ~request:(bound 2048)
         ~result:(bound 4096)
         ~page:(bound 2048)
         ~response:(bound 8192)
         ~max_source_bytes:1024
         ~max_reference_bytes:384
         ~max_output_bytes
         ~max_page_items:4
         ~max_event_bytes:512
         ~max_registry_entries:16)
  in
  let ample = wire 512 in
  let tight = wire 256 in
  let owner =
    must
      (Dev.Owner.create
         ~host_id:(P.Id.Server.create_with deterministic)
         ~run_id:(P.Id.Developer_run.create_with deterministic)
         ~epoch:1
         ~principal_id:(P.Id.Principal.create_with deterministic))
  in
  let digest = Chatml_source.Digest.of_contents "task" ~checkpoint:Fn.id in
  let command_id = must (Dev.Command_id.of_string "task-command") in
  let key = must (P.Idempotency_key.of_string "task-key") in
  let create limits phase =
    W.Task_view.create
      ~limits
      ~command_id
      ~owner
      ~key
      ~phase
      ~request_fingerprint:digest
      ~capture_fingerprint:digest
  in
  let phases =
    [ W.Task_view.Reserved
    ; Waiting_permission
    ; Execution_started
    ; Terminal { execution_started = true; outcome = Succeeded (Some "done") }
    ; Terminal { execution_started = false; outcome = Interrupted }
    ; Terminal { execution_started = true; outcome = Cancelled }
    ; Terminal
        { execution_started = true
        ; outcome = Failed { error = P.Error.invalid_request "failed"; diagnostic = None }
        }
    ; Terminal
        { execution_started = true
        ; outcome =
            Limited { error = P.Error.invalid_request "limited"; diagnostic = None }
        }
    ]
  in
  List.iter phases ~f:(fun phase ->
    let view = must (create ample phase) in
    let decoded = must (W.Task_view.of_json ~limits:ample (W.Task_view.to_json view)) in
    assert (
      String.equal
        (Jsonaf.to_string (W.Task_view.to_json view))
        (Jsonaf.to_string (W.Task_view.to_json decoded))));
  assert (
    Result.is_error
      (create ample (Terminal { execution_started = false; outcome = Succeeded None })));
  let broad_diagnostic =
    must
      (W.Task_view.Diagnostic.create
         ~limits:ample
         ~code:"runtime"
         ~message:(String.make 300 'x')
         ~location:None)
  in
  assert (
    Result.is_error
      (create
         tight
         (Terminal
            { execution_started = true
            ; outcome =
                Limited
                  { error = P.Error.invalid_request "limited"
                  ; diagnostic = Some broad_diagnostic
                  }
            })));
  let broad_view =
    must
      (create
         ample
         (Terminal
            { execution_started = true
            ; outcome =
                Limited
                  { error = P.Error.invalid_request "limited"
                  ; diagnostic = Some broad_diagnostic
                  }
            }))
  in
  assert (
    Result.is_error (W.Task_view.of_json ~limits:tight (W.Task_view.to_json broad_view)));
  let instance = must (W.Wire_id.Instance.of_string "ri_test") in
  let epoch = must (W.Wire_id.Epoch.of_string "re_test") in
  let revision = must (W.Wire_id.Revision.of_string "rv_test") in
  let current = W.Current_epoch.create ~instance ~epoch ~key in
  List.iter [ -1; Int.max_value ] ~f:(fun index ->
    assert (
      Result.is_error
        (W.Inspect.create
           ~limits:ample
           ~current
           ~statement_index:0
           ~selector:(W.Type_selector.Binding_index index)
           ~max_bytes:256)));
  let source =
    Chatml_source.Id.create
      ~package_namespace:"repl-test"
      ~relative_path:"imported.chatml"
    |> Or_error.ok_exn
  in
  let location =
    must
      (W.Submission.Location.create
         ~limits:ample
         ~source
         ~digest
         ~start_byte:1400
         ~end_byte:1500)
  in
  assert (Int.equal (W.Submission.Location.end_byte location) 1500);
  let summary =
    must
      (W.Submission.Summary.create
         ~limits:ample
         ~outcome:Succeeded
         ~value_preview:None
         ~diagnostic:None
         ~expression_type:None
         ~binding_count:0
         ~task_retention:Not_a_task
         ~text_limited:false)
  in
  let summaries = List.init 5 ~f:(fun _ -> summary) in
  let submission =
    must
      (W.Submission.create
         ~limits:ample
         ~status:Finished
         ~details:Available
         ~completed_statements:5
         ~statement_count:5
         ~epoch
         ~revision
         ~summaries
         ~task_reference_count:0
         ~task_references:[])
  in
  ignore (must (W.Submission.of_json ~limits:ample (W.Submission.to_json submission)));
  let narrow_result =
    must (D.Limits.create ~max_bytes:4096 ~max_depth:32 ~max_fields:256 ~max_nodes:32)
  in
  let narrow_summaries =
    must
      (W.Limits.create
         ~request:(bound 2048)
         ~result:narrow_result
         ~page:(bound 2048)
         ~response:(bound 8192)
         ~max_source_bytes:1024
         ~max_reference_bytes:384
         ~max_output_bytes:512
         ~max_page_items:4
         ~max_event_bytes:512
         ~max_registry_entries:16)
  in
  assert (
    Result.is_error
      (W.Submission.create
         ~limits:narrow_summaries
         ~status:Finished
         ~details:Available
         ~completed_statements:5
         ~statement_count:5
         ~epoch
         ~revision
         ~summaries
         ~task_reference_count:0
         ~task_references:[]));
  assert (
    Result.is_error
      (W.Submission.of_json ~limits:narrow_summaries (W.Submission.to_json submission)));
  assert (
    Result.is_error
      (W.Task_submit.create
         ~limits:ample
         ~key
         ~reference:(String.make 385 'r')
         ~requested_execution:None
         ~requested_max_output_bytes:None
         ~fresh_epoch:None))
;;
