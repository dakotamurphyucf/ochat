open! Core
module P = Agent_protocol
module W = P.Authoring.Wire
module A = P.Authoring.Analysis

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let digest text = Chatml_source.Digest.of_contents text ~checkpoint:Fn.id

let source path contents =
  let id =
    Chatml_source.Id.create
      ~package_namespace:"analysis-protocol-test"
      ~relative_path:path
    |> Or_error.ok_exn
  in
  P.Authoring.Source.create
    ~id
    ~digest:(digest contents)
    ~byte_length:(String.length contents)
    ~editor_revision:None
  |> ok
;;

let document = source "draft.chatmd" "<script id=\"s\">let x = 1</script>"
let script = source "s.chatml" "let x = 1"
let generator = P.Id.Generator.create ~bytes:(fun length -> String.make length '\000')
let host = P.Id.Server.create_with generator
let context_id = P.Id.Authoring_context.create_with generator

let context =
  P.Authoring.Context.create
    ~id:context_id
    ~host
    ~origin:
      (P.Authoring.Context.Supplied_document
         { source = document; default_environment = None })
    ~generation:0
    ~basis:(P.Authoring.Context.Document { source = document; selection = None })
    ~basis_fingerprint:(digest "document-basis")
    ~editable_sources:[]
  |> ok
;;

let expected = P.Authoring.Context.Expected.of_view context
let selector = A.Target.selected_script "s" |> ok

let stamp =
  A.Target.script_stamp
    ~document_source:document
    ~parser_root_fingerprint:(digest "parser-root")
    ~script_id:"s"
    ~declaration_input_fingerprint:(digest "parser-input")
    ~declaration_input_byte_length:100
    ~declaration_start_byte:4
    ~declaration_end_byte:29
    ~selected_script_source:script
  |> ok
;;

let capture =
  A.Capture.create
    ~context:expected
    ~target:(A.Target.Script_input stamp)
    ~input_fingerprint:(digest "checked-graph")
    ~environment:"default"
    ~required_contract:(digest "required-contract")
    ~selected_contract:(digest "selected-contract")
  |> ok
;;

let snapshot = A.Snapshot_id.create "snapshot-1" |> ok
let native = A.Byte_range.create ~source:script ~start_byte:0 ~end_byte:3 |> ok
let authored = A.Byte_range.create ~source:document ~start_byte:14 ~end_byte:29 |> ok

let site =
  A.Site.create ~native ~projection:(Some (A.Site.Approximate_declaration authored)) |> ok
;;

let empty_members = A.Query.Available { A.Query.items = []; truncated = false }

let binding name selected_signature value_members =
  A.Query.Item.binding
    ~symbol:1
    ~name
    ~selected_signature
    ~value_members
    ~type_members:empty_members
  |> ok
;;

let%expect_test "selected script identity, failed truth and projected site" =
  let request = A.Analyze.Request.create ~context:expected ~target:selector |> ok in
  let wire = W.Request.Analyze request in
  let decoded =
    W.Request.of_method_and_params
      ~method_:(W.Request.method_name wire)
      ~params:(W.Request.params wire)
    |> ok
  in
  (match decoded with
   | W.Request.Analyze decoded_request ->
     assert (A.Analyze.Request.equal decoded_request request)
   | _ -> assert false);
  print_s (Sexp.List [ Atom "selected_script"; Atom (A.Target.script_id stamp) ]);
  let diagnostic =
    A.Diagnostic.create ~message:"bad type" ~site:(Some site) ~related:[] |> ok
  in
  let failed =
    A.Analyze.failed
      ~capture
      ~phase:(A.Analyze.Implementation script)
      ~syntax:[]
      ~diagnostics:[ diagnostic ]
      ~truncated_syntax:false
      ~truncated_diagnostics:false
    |> ok
  in
  let decoded = A.Analyze.of_json (A.Analyze.to_json failed) |> ok in
  (match decoded with
   | A.Analyze.Failed { diagnostics = [ diagnostic ]; _ } ->
     let site = A.Diagnostic.site diagnostic |> Option.value_exn in
     let native = A.Site.native site in
     (match A.Site.projection site with
      | Some (A.Site.Approximate_declaration projected) ->
        print_s
          (Sexp.List
             [ Atom "failed"
             ; Atom (A.Diagnostic.message diagnostic)
             ; Atom
                 (Chatml_source.Id.relative_path
                    (P.Authoring.Source.id (A.Byte_range.source native)))
             ; Atom (Int.to_string (A.Byte_range.start_byte native))
             ; Atom "approximate_declaration"
             ; Atom
                 (Chatml_source.Id.relative_path
                    (P.Authoring.Source.id (A.Byte_range.source projected)))
             ; Atom (Int.to_string (A.Byte_range.start_byte projected))
             ])
      | _ -> assert false)
   | _ -> assert false);
  let unavailable =
    A.Analyze.unavailable
      ~context:expected
      ~target:A.Target.project
      ~reason:A.Analyze.Document_basis
    |> ok
  in
  assert (Option.is_none (A.Analyze.capture unavailable));
  (match unavailable with
   | A.Analyze.Unavailable { reason = A.Analyze.Document_basis; _ } -> ()
   | _ -> assert false);
  print_s (Sexp.List [ Atom "unavailable"; Atom "document_basis" ]);
  assert (
    Result.is_error
      (A.Target.script_stamp
         ~document_source:document
         ~parser_root_fingerprint:(digest "parser-root")
         ~script_id:"s"
         ~declaration_input_fingerprint:(digest "parser-input")
         ~declaration_input_byte_length:10
         ~declaration_start_byte:4
         ~declaration_end_byte:29
         ~selected_script_source:script));
  print_s (Sexp.List [ Atom "invalid_declaration_range"; Atom "rejected" ]);
  [%expect
    {|
    (selected_script s)
    (failed "bad type" s.chatml 0 approximate_declaration draft.chatmd 14)
    (unavailable document_basis)
    (invalid_declaration_range rejected)
    |}]
;;

let%test_unit "strict semantic wire and recursive member bounds" =
  let request = A.Analyze.Request.create ~context:expected ~target:selector |> ok in
  let valid = A.Analyze.Request.to_json request in
  let variants =
    match valid with
    | `Object fields ->
      [ `Object
          (("target", `Null)
           :: List.filter fields ~f:(fun (name, _) -> not (String.equal name "target")))
      ; `Object (("unknown", `True) :: fields)
      ; `Object (("context", `Null) :: fields)
      ]
    | _ -> assert false
  in
  List.iter variants ~f:(fun value ->
    assert (Result.is_error (A.Analyze.Request.of_json value)));
  let malformed = "\255" in
  let malformed_text =
    [ Result.map (A.Target.selected_script malformed) ~f:ignore
    ; Result.map
        (A.Capture.create
           ~context:expected
           ~target:(A.Target.Script_input stamp)
           ~input_fingerprint:(digest "graph")
           ~environment:malformed
           ~required_contract:(digest "required")
           ~selected_contract:(digest "selected"))
        ~f:ignore
    ; Result.map (A.Diagnostic.create ~message:malformed ~site:None ~related:[]) ~f:ignore
    ; Result.map
        (A.Syntax_site.create ~role:A.Syntax_site.Value_binder ~name:malformed ~site)
        ~f:ignore
    ; Result.map
        (A.Query.Item.binding
           ~symbol:1
           ~name:malformed
           ~selected_signature:None
           ~value_members:empty_members
           ~type_members:empty_members)
        ~f:ignore
    ]
  in
  List.iter malformed_text ~f:(fun result -> assert (Result.is_error result));
  let display =
    A.Query.Display.create
      ~format:Chatml_display_v1
      ~kind:Signature
      ~completeness:Complete
      ~text:"int -> int"
    |> ok
  in
  let leaf = binding "x" (Some display) empty_members in
  let nested =
    binding "record" None (A.Query.Available { items = [ leaf ]; truncated = true })
  in
  let query_request =
    A.Query.Request.create
      ~evidence:
        (A.Query.Certified
           { capture; snapshot; validated_graph = digest "validated-graph" })
      ~source:script
      ~byte_offset:0
      ~kind:(A.Query.Candidates Value)
      ~max_items:4
    |> ok
  in
  let response =
    A.Query.create
      ~request:query_request
      ~payload:(Candidates_result (Selected { items = [ nested ]; truncated = false }))
    |> ok
  in
  let decoded = A.Query.of_json (A.Query.to_json response) |> ok in
  assert (A.Query.Request.equal (A.Query.request response) (A.Query.request decoded));
  (match A.Query.payload decoded with
   | Candidates_result (Selected { items = [ nested ]; _ }) ->
     (match A.Query.Item.binding_value_members nested with
      | Available { items = [ child ]; truncated = true } ->
        assert (String.equal (A.Query.Item.binding_name child) "x")
      | _ -> assert false)
   | _ -> assert false);
  let rec too_deep remaining previous =
    if remaining = 0
    then failwith "recursive member depth was not bounded"
    else (
      let next =
        A.Query.Item.binding
          ~symbol:1
          ~name:"nested"
          ~selected_signature:None
          ~value_members:(Available { items = [ previous ]; truncated = false })
          ~type_members:empty_members
      in
      match next with
      | Error _ -> ()
      | Ok next -> too_deep (remaining - 1) next)
  in
  too_deep 8 leaf
;;

let%test_unit "accepted aggregate semantic results decode under the same budget" =
  let related = A.Diagnostic.Related.create ~site ~message:"related" |> ok in
  let diagnostic =
    A.Diagnostic.create
      ~message:"diagnostic"
      ~site:(Some site)
      ~related:(List.init 32 ~f:(fun _ -> related))
    |> ok
  in
  let analysis_at count =
    let diagnostics = List.init count ~f:(fun _ -> diagnostic) in
    A.Analyze.failed
      ~capture
      ~phase:(A.Analyze.Implementation script)
      ~syntax:[]
      ~diagnostics
      ~truncated_syntax:false
      ~truncated_diagnostics:false
  in
  let complete_at count =
    A.Analyze.complete
      ~capture
      ~snapshot
      ~validated_graph:(digest "validated-graph")
      ~diagnostics:(List.init count ~f:(fun _ -> diagnostic))
      ~truncated_diagnostics:false
  in
  assert (Result.is_ok (analysis_at 0));
  assert (Result.is_error (analysis_at 256));
  assert (Result.is_ok (complete_at 0));
  assert (Result.is_error (complete_at 256));
  List.iter [ 0; 1; 8; 16; 32; 64; 128; 256 ] ~f:(fun count ->
    List.iter
      [ analysis_at count; complete_at count ]
      ~f:(function
        | Error _ -> ()
        | Ok result ->
          assert (Result.is_ok (A.Analyze.of_json (A.Analyze.to_json result)))));
  let display =
    A.Query.Display.create
      ~format:Chatml_display_v1
      ~kind:Signature
      ~completeness:Complete
      ~text:(String.make 1024 't')
    |> ok
  in
  let item = binding "member" (Some display) empty_members in
  let request =
    A.Query.Request.create
      ~evidence:
        (A.Query.Certified
           { capture; snapshot; validated_graph = digest "validated-graph" })
      ~source:script
      ~byte_offset:0
      ~kind:(A.Query.Candidates Value)
      ~max_items:512
    |> ok
  in
  let candidate_at count =
    let items = List.init count ~f:(fun _ -> item) in
    A.Query.create
      ~request
      ~payload:(Candidates_result (Selected { items; truncated = false }))
  in
  assert (Result.is_ok (candidate_at 0));
  assert (Result.is_error (candidate_at 512));
  List.iter [ 0; 1; 8; 64; 128; 256; 512 ] ~f:(fun count ->
    match candidate_at count with
    | Error _ -> ()
    | Ok result -> assert (Result.is_ok (A.Query.of_json (A.Query.to_json result))));
  let rec nested remaining current =
    if remaining = 0
    then Ok current
    else (
      let next =
        A.Query.Item.binding
          ~symbol:1
          ~name:"nested"
          ~selected_signature:None
          ~value_members:(Available { items = [ current ]; truncated = false })
          ~type_members:empty_members
      in
      Result.bind next ~f:(nested (remaining - 1)))
  in
  let nested_two = nested 2 item |> ok in
  assert (
    Result.is_ok
      (A.Query.create
         ~request
         ~payload:
           (Candidates_result (Selected { items = [ nested_two ]; truncated = false }))));
  List.iter (List.init 9 ~f:Fn.id) ~f:(fun depth ->
    match nested depth item with
    | Error _ -> ()
    | Ok binding ->
      (match
         A.Query.create
           ~request
           ~payload:
             (Candidates_result (Selected { items = [ binding ]; truncated = false }))
       with
       | Error _ -> ()
       | Ok result -> assert (Result.is_ok (A.Query.of_json (A.Query.to_json result)))));
  let empty_page = { A.Query.items = []; truncated = false } in
  List.iter [ 0; 1; 8; 64; 128; 256; 512 ] ~f:(fun count ->
    let pages = List.init count ~f:(fun _ -> empty_page) in
    match
      A.Query.create
        ~request
        ~payload:(Candidates_result (Ambiguous { items = pages; truncated = false }))
    with
    | Error _ -> ()
    | Ok result -> assert (Result.is_ok (A.Query.of_json (A.Query.to_json result))))
;;

let%test_unit "complete, provisional and each query payload round-trip" =
  let complete =
    A.Analyze.complete
      ~capture
      ~snapshot
      ~validated_graph:(digest "validated-graph")
      ~diagnostics:[]
      ~truncated_diagnostics:false
    |> ok
  in
  assert (Result.is_ok (A.Analyze.of_json (A.Analyze.to_json complete)));
  let prefix_source = source "s.chatml" "let" in
  let provisional =
    A.Prefix.create
      ~capture
      ~full_source:script
      ~prefix_source
      ~cursor_byte:3
      ~end_byte:3
      ~prefix_graph:(digest "prefix-graph")
      ~snapshot
    |> ok
  in
  let provisional_result = A.Prefix.provisional provisional in
  assert (Result.is_ok (A.Prefix.of_json (A.Prefix.to_json provisional_result)));
  let authored_request =
    A.Prefix.Authored_request.create ~source:document ~cursor_byte:20 |> ok
  in
  let selected_provisional =
    A.Prefix.create_with_authored_request
      ~capture
      ~full_source:script
      ~prefix_source
      ~cursor_byte:3
      ~end_byte:3
      ~prefix_graph:(digest "prefix-graph")
      ~snapshot
      ~authored_request
    |> ok
  in
  let selected_roundtrip =
    A.Prefix.of_json (A.Prefix.to_json (A.Prefix.provisional selected_provisional)) |> ok
  in
  (match selected_roundtrip with
   | Provisional returned ->
     assert (A.Prefix.equal_provisional selected_provisional returned)
   | Unavailable _ -> assert false);
  assert (
    Result.is_error
      (A.Prefix.within_bytes (A.Prefix.provisional selected_provisional) ~max_bytes:32));
  let open A.Query in
  let display =
    A.Query.Display.create
      ~format:Chatml_display_v1
      ~kind:Signature
      ~completeness:Complete
      ~text:"int -> int"
    |> ok
  in
  let symbol =
    A.Query.Item.symbol
      ~id:1
      ~name:"x"
      ~kind:Item.Value
      ~origin:(Item.Authored site)
      ~principal_signature:(Some display)
      ~specification_ids:[]
    |> ok
  in
  let reference =
    A.Query.Item.reference
      ~selection:site
      ~target:(Item.Symbol 1)
      ~inferred_type:(Some display)
    |> ok
  in
  let syntax =
    A.Syntax_site.create ~role:A.Syntax_site.Value_binder ~name:"x" ~site |> ok
  in
  let nested =
    binding
      "record"
      None
      (Available
         { items = [ binding "x" (Some display) empty_members ]; truncated = true })
  in
  let exports =
    A.Query.Item.exports
      ~values:{ items = [ nested ]; truncated = false }
      ~types:{ items = []; truncated = false }
    |> ok
  in
  let configured =
    A.Query.Item.configured_module
      ~name:"Config"
      ~module_key:"config"
      ~declared_at:site
      ~exports:(Available exports)
      ~shadowed_by:None
    |> ok
  in
  let page items = { A.Query.items; truncated = false } in
  let cases =
    [ A.Query.References, A.Query.References_result (Available (page [ reference ]))
    ; A.Query.Definitions, A.Query.Definitions_result (Available (page [ symbol ]))
    ; A.Query.Type_at, A.Query.Type_result (Selected display)
    ; A.Query.Candidates Value, A.Query.Candidates_result (Selected (page [ nested ]))
    ; ( A.Query.Configured_modules
      , A.Query.Configured_modules_result (Selected (page [ configured ])) )
    ; A.Query.Syntax_at, A.Query.Syntax_result (Available (page [ syntax ]))
    ]
  in
  List.iter cases ~f:(fun (kind, payload) ->
    let evidence =
      match kind with
      | A.Query.Syntax_at -> A.Query.Syntax_only capture
      | _ -> A.Query.Certified { capture; snapshot; validated_graph = digest "graph" }
    in
    let request =
      A.Query.Request.create ~evidence ~source:script ~byte_offset:1 ~kind ~max_items:4
      |> ok
    in
    let result = A.Query.create ~request ~payload |> ok in
    let decoded = A.Query.of_json (A.Query.to_json result) |> ok in
    assert (A.Query.Request.equal request (A.Query.request decoded)))
;;

let%test_unit "semantic results reject duplicate, unknown and noncanonical evidence" =
  let result =
    A.Analyze.failed
      ~capture
      ~phase:(A.Analyze.Implementation script)
      ~syntax:[]
      ~diagnostics:[]
      ~truncated_syntax:false
      ~truncated_diagnostics:false
    |> ok
  in
  let json = A.Analyze.to_json result in
  let fields =
    match json with
    | `Object fields -> fields
    | _ -> assert false
  in
  List.iter
    [ `Object (("status", `String "failed") :: fields)
    ; `Object (("future_fact", `True) :: fields)
    ]
    ~f:(fun json -> assert (Result.is_error (A.Analyze.of_json json)));
  let stamp_json = A.Target.resolved_to_json (A.Target.Script_input stamp) in
  let bad_stamp =
    match stamp_json with
    | `Object fields ->
      `Object
        (("declaration_input_byte_length", `Number "100.0")
         :: List.filter fields ~f:(fun (name, _) ->
           not (String.equal name "declaration_input_byte_length")))
    | _ -> assert false
  in
  assert (Result.is_error (A.Target.resolved_of_json bad_stamp))
;;
