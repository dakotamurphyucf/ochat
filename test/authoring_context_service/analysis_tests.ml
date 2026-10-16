open! Core
module P = Agent_protocol
module A = P.Authoring.Analysis
module Q = A.Query
module Core_owner = Agent_server.Authoring_context_service
module Testing = Agent_server.Authoring_context_service.For_testing
module Analyze = Core_owner.Semantic
module Sources = Agent_server.Authoring_source_host
module Selection = Core_owner.Semantic.Script_selection
module Build = Chatml_project_build
module Captured = Chatml_source
module Ledger = Chatml_authoring_context
module Assistance_wire = P.Authoring_assistance.Assistance

let%test_unit "compiler admission errors retain resource, selection and internal truth" =
  let module C = Chatml_compilation in
  let policy code =
    C.Analysis_error.Policy { C.code; message = "not a wire message"; diagnostic = None }
  in
  let cases =
    [ policy "chatml.source_limit", `Resource
    ; policy "chatml.entry_mismatch", `Selection
    ; policy "chatml.compiler_failed", `Internal
    ; C.Analysis_error.Analysis Chatml_semantics.Error.Work_limit, `Resource
    ; C.Analysis_error.Analysis Chatml_semantics.Error.Incompatible_contract, `Selection
    ; C.Analysis_error.Analysis Chatml_semantics.Error.Invalid_cursor, `Invalid
    ]
  in
  List.iter cases ~f:(fun (error, expected) ->
    match Testing.classify_compilation_error error, expected with
    | `Resource, `Resource
    | `Selection, `Selection
    | `Internal, `Internal
    | `Invalid, `Invalid -> ()
    | _ -> failwith "semantic admission error lost its category")
;;

let query_limits () =
  Analyze.Query_limits.create
    ~max_host_work_steps:100_000
    ~projection:
      (Chatml_authoring_query.Limits.create
         ~max_items:32
         ~max_member_depth:4
         ~max_member_edges:128
         ~max_label_bytes:4096
         ~max_work_steps:100_000
         ~type_display:
           (Chatml_type_display.Limits.create
              ~max_nodes:512
              ~max_edges:1024
              ~max_depth:16
              ~max_work_steps:100_000
              ~max_text_bytes:1024
              ~max_output_bytes:4096
            |> Or_error.ok_exn)
       |> Or_error.ok_exn)
  |> Or_error.ok_exn
;;

let ok = function
  | Ok value -> value
  | Error _ -> failwith "unexpected Analyze fixture failure"
;;

let%test_unit "failed syntax projection stops at first omitted site" =
  let collect ~max_sites units =
    let polls = ref 0 in
    let exceeded = Failure "traversed omitted syntax tail" in
    let result =
      Testing.syntax_prefix_for_testing ~max_sites units ~checkpoint:(fun () ->
        incr polls;
        if !polls > 20 then raise exceeded)
    in
    result, !polls
  in
  let long_tail = [ [ 0; 1; 2 ]; []; [ 3; 4; 5 ]; List.init 20_000 ~f:Fn.id ] in
  let (prefix, truncated), polls = collect ~max_sites:4 long_tail in
  [%test_eq: int list] prefix [ 0; 1; 2; 3 ];
  [%test_eq: bool] truncated true;
  assert (polls < 20);
  let (all, truncated), _ = collect ~max_sites:6 [ [ 0; 1; 2 ]; []; [ 3; 4; 5 ] ] in
  [%test_eq: int list] all [ 0; 1; 2; 3; 4; 5 ];
  [%test_eq: bool] truncated false
;;

let analyzer ?(script_selection = None) owner env =
  Analyze.create
    ~owner
    ~env
    ~document_dir:(Eio.Stdenv.cwd env)
    ~script_selection
    ~project_metadata:None
    ~project_build_limits:
      (Chatml_project_build.Limits.create
         ~max_work_steps:1_000_000
         ~max_input_bytes:(Some 64_000)
         ~document:Document_schema.Limits.default
       |> Or_error.ok_exn)
    ~limits:
      (Analyze.Limits.create
         ~max_document_bundle_files:4
         ~max_document_bundle_bytes:2048
         ~max_parser_work_steps:100_000
         ~max_diagnostic_work_steps:100_000
         ~max_selection_rows:32
         ~max_retained_selection_source_bytes:(16 * 1024 * 1024)
         ~max_retained_selection_input_bytes:(16 * 1024 * 1024)
         ~max_selection_work_steps:1_000_000
       |> Or_error.ok_exn)
    ~query_limits:(query_limits ())
    ~prefix_limits:
      (Analyze.Prefix_limits.create
         ~max_document_bundle_files:4
         ~max_document_bundle_bytes:2048
         ~max_parser_work_steps:100_000
         ~max_host_work_steps:100_000
       |> Or_error.ok_exn)
;;

let selected_build ?contract ?(check_identity_boundary = false) body =
  let module Fixtures = Chatml_project_build_tests.Fixtures in
  let module Support = Chatml_project_build_tests.Support in
  let fixture = Fixtures.make ?contract () in
  let base = Fixtures.compose fixture |> ok in
  let sources =
    Chatml_prepared_program.sources (Build.prepared base)
    |> List.map ~f:(fun source ->
      if Captured.Id.equal (Captured.id source) (Captured.id (Fixtures.main fixture))
      then
        Captured.create
          (Captured.id source)
          ~contents:body
          ~expected_digest:None
          ~max_bytes:None
          ~checkpoint:ignore
        |> Or_error.ok_exn
      else source)
  in
  let build =
    Build.Effective.recompose
      base
      ~sources
      ~lock_id:(Support.id ~namespace:"root" "analyze-script.lock.json")
      ~contract:(Fixtures.contract fixture)
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> ok
  in
  if check_identity_boundary
  then (
    let allocation = ref 0 in
    ignore
      (Chatml.Chatml_compiler_contract.input_bytes
         (Fixtures.contract fixture)
         ~checkpoint:ignore
         ~allocate:(fun bytes -> allocation := !allocation + bytes)
       : int);
    assert (!allocation > 1);
    let limits =
      Selection.Limits.create
        ~max_identity_bytes:(!allocation - 1)
        ~max_work_steps:1_000_000
      |> Or_error.ok_exn
    in
    match
      Selection.Selected.admit
        ~build
        ~contract:(Fixtures.contract fixture)
        ~limits
        ~checkpoint:ignore
    with
    | Error Selection.Error.Identity_limit -> ()
    | Ok _ | Error _ -> failwith "undersized actual contract admission passed");
  let selected =
    Selection.Selected.admit
      ~build
      ~contract:(Fixtures.contract fixture)
      ~limits:
        (Selection.Limits.create
           ~max_identity_bytes:(16 * 1024 * 1024)
           ~max_work_steps:1_000_000
         |> Or_error.ok_exn)
      ~checkpoint:ignore
    |> ok
  in
  build, selected
;;

let selected_assistance owner selection =
  let module Assistance = Core_owner.Assistance in
  Assistance.create
    ~owner
    ~script_selection:(Some selection)
    ~project_metadata:None
    ~selected_tools:None
    ~project_build_limits:
      (Build.Limits.create
         ~max_work_steps:1_000_000
         ~max_input_bytes:(Some 64_000)
         ~document:Document_schema.Limits.default
       |> Or_error.ok_exn)
    ~limits:
      (Assistance.Limits.create
         ~protocol_limits:
           (P.Authoring.Limits.create
              ~max_sources:4
              ~max_source_bytes:1024
              ~max_context_source_bytes:4096
              ~max_request_bytes:4096
              ~max_result_bytes:65536
              ~max_path_candidates:16
            |> ok)
         ~max_scan_work_steps:1_000_000
         ~max_assistance_work_steps:1_000_000
         ~max_contract_identity_bytes:(16 * 1024 * 1024)
         ~max_contract_work_steps:1_000_000
         ~max_projection_work_steps:1_000_000
         ~max_parser_input_bytes:1024
         ~max_imported_inputs:4
         ~max_selected_tools:16
         ~max_tool_metadata_bytes:4096
       |> Or_error.ok_exn)
  |> ok
;;

let inspect_capture owner actor connection expected =
  let state =
    Testing.capture_state owner ~actor ~connection ~expected ~checkpoint:ignore |> ok
  in
  state.parsed, state.source_matches, state.complete_rows
;;

let%expect_test "supplied Analyze retains only authentic root syntax without Build" =
  let cases =
    [ ( "root-only"
      , {|<script id="s" language="chatml" kind="tool">let x = 1</script>|}
      , true )
    ; "missing import", {|<import src="missing.chatmd"/>|}, false
    ]
  in
  List.iter cases ~f:(fun (label, contents, should_capture) ->
    Foundation_tests.with_supplied_owner
      ~contents
      (fun owner actor connection expected env ->
         let request =
           A.Analyze.Request.create
             ~context:expected
             ~target:(A.Target.selected_script "s" |> ok)
           |> ok
         in
         let result =
           match Analyze.analyze (analyzer owner env) ~actor ~connection request with
           | Ok result -> result
           | Error error ->
             let protocol = Core_owner.Error.to_protocol error in
             failwith ("Analyze error: " ^ P.Error.code_to_string protocol.code)
         in
         let reason =
           match result with
           | A.Analyze.Unavailable { reason; _ } -> reason
           | Complete _ | Failed _ -> failwith "unselected script became certified"
         in
         let capture, source_matches, rows =
           inspect_capture owner actor connection expected
         in
         [%test_eq: bool] capture should_capture;
         [%test_eq: int] rows 0;
         print_s
           [%sexp
             (label : string)
           , (reason : A.Analyze.unavailable)
           , (capture : bool)
           , (source_matches : bool)
           , (rows : int)]));
  [%expect
    {|
      (root-only No_validated_project true true 0)
      ("missing import" Incomplete_or_malformed_document false false 0) |}]
;;

let%expect_test "selected supplied script publishes certified or same-run failed facts" =
  let module Contract = Chatml.Chatml_compiler_contract in
  let module Checked = Assistance_wire.Checked_template in
  let tool_body = {|let run ctx input = Task.pure(`Complete(input))|} in
  let moderator_body =
    {|let initial_state = 0
let on_event ctx state event = Task.pure(state)|}
  in
  let native_contract =
    Contract.for_static_surface
      (Chatml.Chatml_static_surface.of_runtime_surface
         Chatml.Chatml_builtin_surface.empty
         ~checkpoint:ignore)
      ~required_bindings:[]
  in
  List.iter
    [ "valid-default", "let main input = Task.pure(input)", None, None, false, None
    ; ( "valid"
      , "let main input = Task.pure(input)"
      , None
      , None
      , true
      , Some Checked.One_off_v1 )
    ; ( "edited"
      , "let main input = Task.pure(input)"
      , Some "let main = 0"
      , None
      , true
      , Some Checked.One_off_v1 )
    ; ( "invalid"
      , "let x = 1;; let broken = true + 1"
      , None
      , None
      , true
      , Some Checked.One_off_v1 )
    ; "native", "let main = 1", None, Some native_contract, true, None
    ; ( "tool"
      , tool_body
      , None
      , Some (Contract.for_target Tool_v1 ~checkpoint:ignore)
      , true
      , Some Checked.Tool_v1 )
    ; ( "developer"
      , tool_body
      , None
      , Some (Contract.for_target Developer_tool_v1 ~checkpoint:ignore)
      , true
      , Some Checked.Developer_tool_v1 )
    ; ( "moderator"
      , moderator_body
      , None
      , Some (Contract.for_target Moderator_v1 ~checkpoint:ignore)
      , true
      , Some Checked.Moderator_v1 )
    ; ( "delegated"
      , moderator_body
      , None
      , Some (Contract.for_target Delegated_moderator_v1 ~checkpoint:ignore)
      , true
      , Some Checked.Delegated_moderator_v1 )
    ]
    ~f:(fun (label, body, selected_body, contract, checked_template, expected_target) ->
      let _, selected =
        selected_build
          ?contract
          ~check_identity_boundary:(String.equal label "moderator")
          (Option.value selected_body ~default:body)
      in
      let selection =
        Selection.Policy.create
          ~select_script:
            (fun
              ~original_actor:_
              ~actor:_
              ~current:_
              ~parsed:_
              ~declaration:_
              ~metadata:_
            -> Some selected)
      in
      let script id =
        sprintf {|<script id="%s" language="chatml" kind="tool">%s</script>|} id body
      in
      let contents = script "s" ^ if String.equal label "valid" then script "t" else "" in
      let source = ref None in
      Foundation_tests.with_supplied_owner
        ~contents
        ~result_bytes:65536
        ~analysis_policy:
          (Service_tests.analysis_policy
             ~compilation:
               { Chatml_compilation.wall_seconds = 10.; max_source_bytes = 512 * 1024 }
             ())
        ~on_context:(fun context ->
          match P.Authoring.Context.basis context with
          | Document { source = captured; _ } -> source := Some captured
          | Project _ -> failwith "script fixture gained a project")
        (fun owner actor connection expected env ->
           let semantic = analyzer ~script_selection:(Some selection) owner env in
           let request =
             A.Analyze.Request.create
               ~context:expected
               ~target:(A.Target.selected_script "s" |> ok)
             |> ok
           in
           let request =
             if checked_template
             then A.Analyze.Request.with_checked_template request
             else request
           in
           let result =
             match Analyze.analyze semantic ~actor ~connection request with
             | Ok result -> result
             | Error error ->
               failwith
                 ("Analyze fixture ("
                  ^ label
                  ^ "): "
                  ^ Jsonaf.to_string
                      (P.Error.to_json (Core_owner.Error.to_protocol error)))
           in
           let kind, diagnostic_count =
             match result with
             | A.Analyze.Complete { diagnostics; _ } ->
               "complete", List.length diagnostics
             | Failed { diagnostics; _ } -> "failed", List.length diagnostics
             | Unavailable { reason; _ } ->
               Sexp.to_string_hum (A.Analyze.sexp_of_unavailable reason), 0
           in
           (match result with
            | A.Analyze.Failed { capture; syntax; _ } ->
              let first = List.hd_exn syntax in
              let native = A.Site.native (A.Syntax_site.site first) in
              let source = A.Byte_range.source native in
              let query =
                Q.Request.create
                  ~evidence:(Q.Syntax_only capture)
                  ~source
                  ~byte_offset:(A.Byte_range.start_byte native)
                  ~kind:Q.Syntax_at
                  ~max_items:4
                |> ok
              in
              (match Analyze.query semantic ~actor ~connection query with
               | Ok answer ->
                 (match Q.payload answer with
                  | Syntax_result (Available page) when not (List.is_empty page.items) ->
                    ()
                  | _ -> failwith "failed check lost same-run parsed syntax")
               | Error error ->
                 failwith
                   ("Syntax-only query: "
                    ^ P.Error.code_to_string (Core_owner.Error.to_protocol error).code))
            | Complete _ | Unavailable _ -> ());
           let capture, sources, rows = inspect_capture owner actor connection expected in
           let authored = Option.value_exn !source in
           let cursor_byte =
             String.length {|<script id="s" language="chatml" kind="tool">|} + 4
           in
           let assistance_request =
             Assistance_wire.Request.create
               ~context:expected
               ~source:authored
               ~cursor_byte
               ~max_items:(if String.equal label "edited" then 1 else 8)
             |> ok
           in
           let checked_items, status =
             match
               Core_owner.Assistance.handle_assistance
                 (selected_assistance owner selection)
                 ~actor
                 ~connection
                 assistance_request
             with
             | Ok (Assistance result) ->
               ( List.filter (Assistance_wire.Result.items result) ~f:(fun item ->
                   match Assistance_wire.Suggestion.evidence item with
                   | Checked_template _ -> true
                   | _ -> false)
               , Assistance_wire.Result.status result )
             | Ok _ -> failwith "assistance returned another method"
             | Error error ->
               failwith
                 ("assistance fixture ("
                  ^ label
                  ^ "): "
                  ^ Jsonaf.to_string
                      (P.Error.to_json (Core_owner.Error.to_protocol error)))
           in
           let expected_checked = Option.is_some expected_target in
           [%test_eq: int] (List.length checked_items) (if expected_checked then 1 else 0);
           List.iter checked_items ~f:(fun item ->
             let example = Assistance_wire.Suggestion.insert_text item in
             let expected_example =
               match Option.value_exn expected_target with
               | Checked.One_off_v1 -> {|let main input = Task.pure(input)|}
               | Tool_v1 | Developer_tool_v1 -> tool_body
               | Moderator_v1 | Delegated_moderator_v1 -> moderator_body
             in
             [%test_eq: string] example expected_example;
             let replace = Assistance_wire.Suggestion.replace item in
             let body_start =
               String.length {|<script id="s" language="chatml" kind="tool">|}
             in
             [%test_eq: int] (Assistance_wire.Range.start_byte replace) body_start;
             [%test_eq: int]
               (Assistance_wire.Range.end_byte replace)
               (body_start + String.length body);
             assert (
               Captured.Id.equal
                 (P.Authoring.Source.id (Assistance_wire.Range.source replace))
                 (P.Authoring.Source.id authored));
             match Assistance_wire.Suggestion.evidence item with
             | Checked_template checked ->
               assert (
                 Checked.equal_target
                   (Checked.target checked)
                   (Option.value_exn expected_target));
               assert (
                 Captured.Digest.equal
                   (Checked.example_digest checked)
                   (Captured.Digest.of_contents example ~checkpoint:ignore))
             | _ -> failwith "checked item lost checked evidence");
           print_s
             [%sexp
               (label : string)
             , (kind : string)
             , (diagnostic_count : int)
             , (capture : bool)
             , (sources : bool)
             , (rows : int)
             , (List.length checked_items : int)
             , (status : Assistance_wire.Status.t)];
           if String.equal label "valid"
           then (
             let first_index =
               Testing.index_view owner ~actor ~connection ~expected ~checkpoint:ignore
               |> ok
             in
             let second_request =
               A.Analyze.Request.create
                 ~context:expected
                 ~target:(A.Target.selected_script "t" |> ok)
               |> ok
             in
             let second =
               match
                 Analyze.analyze
                   (analyzer ~script_selection:(Some selection) owner env)
                   ~actor
                   ~connection
                   second_request
               with
               | Ok result -> result
               | Error error ->
                 failwith
                   ("Analyze fixture ("
                    ^ label
                    ^ ", second): "
                    ^ Jsonaf.to_string
                        (P.Error.to_json (Core_owner.Error.to_protocol error)))
             in
             match result, second with
             | A.Analyze.Complete first, A.Analyze.Complete second ->
               let same_graph =
                 Captured.Digest.equal first.validated_graph second.validated_graph
               in
               let current_index =
                 Testing.index_view owner ~actor ~connection ~expected ~checkpoint:ignore
                 |> ok
               in
               let transitioned =
                 Testing.transition_index_to_current
                   owner
                   first_index
                   ~actor
                   ~connection
                   ~expected
                   ~checkpoint:ignore
                 |> ok
               in
               let old_evicted =
                 not
                   (Testing.row_matches
                      current_index
                      ~capture:first.capture
                      ~snapshot:first.snapshot)
               in
               let new_active =
                 Testing.row_matches
                   current_index
                   ~capture:second.capture
                   ~snapshot:second.snapshot
               in
               let old_index_retired =
                 not
                   (Testing.row_matches
                      transitioned
                      ~capture:first.capture
                      ~snapshot:first.snapshot)
               in
               print_s
                 [%sexp
                   (same_graph : bool)
                 , (old_evicted : bool)
                 , (new_active : bool)
                 , (old_index_retired : bool)];
               let published = ref false in
               (match
                  Testing.with_publication_probe
                    owner
                    ~actor
                    ~connection
                    ~expected
                    ~feature:Testing.Semantic
                    ~checkpoint:ignore
                    ~after_borrow:(fun probe ->
                      Testing.replace_head probe Testing.Parsed_capture |> ok)
                    ~final_check:(fun () -> Ok ())
                    ~publish:(fun () -> published := true)
                with
                | Error Testing.Stale_context -> ()
                | Ok _ | Error _ -> failwith "evicted capture passed final CAS");
               assert (not !published);
               (match
                  Core_owner.Assistance.handle_assistance
                    (selected_assistance owner selection)
                    ~actor
                    ~connection
                    assistance_request
                with
                | Ok (Assistance response) ->
                  assert (
                    List.for_all (Assistance_wire.Result.items response) ~f:(fun item ->
                      match Assistance_wire.Suggestion.evidence item with
                      | Checked_template _ -> false
                      | _ -> true))
                | Ok _ | Error _ ->
                  failwith "ordinary Analyze did not retire checked example")
             | _ -> failwith "second genuine declaration was not certified")));
  [%expect
    {|
      (valid-default complete 0 true true 1 0 (Truncated Item_limit))
      (valid complete 0 true true 1 1 (Truncated Item_limit))
      (true true true true)
      (edited complete 0 true true 1 1 (Truncated Item_limit))
      (invalid failed 1 true true 0 1 (Truncated Item_limit))
      (native complete 0 true true 1 0 (Truncated Item_limit))
      (tool complete 0 true true 1 1 (Truncated Item_limit))
      (developer complete 0 true true 1 1 (Truncated Item_limit))
      (moderator complete 0 true true 1 1 (Truncated Item_limit))
      (delegated complete 0 true true 1 1 (Truncated Item_limit)) |}]
;;

let%test_unit
    "developer capture uses the retained supplied script and final current target"
  =
  let contents = {|<script id="s" language="chatml" kind="tool">let main = 1</script>|} in
  Foundation_tests.with_supplied_owner
    ~contents
    (fun owner actor connection expected env ->
       let target = Chatml.Chatml_compiler_contract.Target.One_off_v1 in
       let contracts =
         Agent_server.Authoring_standard_contracts.create
           ~targets:[ target ]
           ~max_identity_bytes:(16 * 1024 * 1024)
           ~max_work_steps:1_000_000
           ~checkpoint:ignore
         |> ok
       in
       let entry =
         Agent_server.Authoring_standard_contracts.find contracts target
         |> Option.value_exn
       in
       let selected =
         Core_owner.Developer.Selected_contract.of_standard_entry entry |> ok
       in
       let allowed = ref true in
       let request_actor =
         Operator_authorization.guarded
           ~principal:(Operator_authorization.principal actor)
           ~is_current:(fun () -> !allowed)
       in
       let policy =
         Core_owner.Developer.Target_policy.create
           ~local_project:(fun _ ~root:_ ~project:_ ~selection:_ -> None)
           ~local_source:(fun _ ~root:_ ~member:_ -> None)
           ~authoring_project:(fun _ ~context:_ ~current:_ -> None)
           ~document_project:
             (fun
               _ ~context:_ ~current:_ ~parsed:_ ~declaration:_ ~build:_ ~metadata:_ ->
             None)
           ~document_source:
             (fun
               _ ~context:_ ~current:_ ~parsed:_ ~declaration:_ ~script:_ ->
             if !allowed then Some selected else None)
       in
       let limits =
         Core_owner.Developer.Limits.create
           ~max_prepared_input_bytes:4096
           ~max_document_origin_bytes:16_384
           ~max_editor_versions:8
           ~build:
             (Build.Limits.create
                ~max_work_steps:100_000
                ~max_input_bytes:(Some 4096)
                ~document:Document_schema.Limits.default
              |> Or_error.ok_exn)
         |> Or_error.ok_exn
       in
       let developer = Core_owner.Developer.create ~owner ~policy ~limits in
       let analyze =
         A.Analyze.Request.create
           ~context:expected
           ~target:(A.Target.selected_script "s" |> ok)
         |> ok
       in
       (match Analyze.analyze (analyzer owner env) ~actor ~connection analyze with
        | Ok (A.Analyze.Unavailable _) -> ()
        | Ok _ | Error _ -> failwith "source-only Analyze did not retain Parsed script");
       let request =
         Core_owner.Developer.Request.create
           ~entrypoint:"main"
           ~target:One_off_v1
           ~input:`Null
           ~fixtures:None
           ~json_bounds:
             (Chatml_developer_capture.Json_bounds.create
                ~max_bytes:4096
                ~max_depth:16
                ~max_nodes:128
              |> Or_error.ok_exn)
           ~checkpoint:ignore
         |> ok
       in
       let candidate =
         match
           Core_owner.Developer.prepare_document_script
             developer
             ~actor:request_actor
             ~connection
             ~expected
             ~script:
               (Core_owner.Developer.Document_script.create
                  ~selector:(A.Target.selected_script "s" |> ok))
             ~request
             ~checkpoint:ignore
         with
         | Ok candidate -> candidate
         | Error error -> failwith (Jsonaf.to_string (P.Error.to_json error))
       in
       let capture = Core_owner.Developer.Candidate.capture candidate in
       let check =
         Core_owner.Developer.Candidate.final_admission
           candidate
           ~actor:request_actor
           ~capture
           ~checkpoint:ignore
         |> ok
       in
       check () |> ok;
       assert (
         Result.is_error
           (Core_owner.Developer.Candidate.final_admission
              candidate
              ~actor
              ~capture
              ~checkpoint:ignore));
       allowed := false;
       match check () with
       | Error { P.Error.code = Permission_denied; _ } -> ()
       | Error _ | Ok () -> failwith "revoked developer target passed final check")
;;
