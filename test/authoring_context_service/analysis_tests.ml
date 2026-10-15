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
         ~max_work_steps:10_000
         ~max_input_bytes:(Some 4096)
         ~document:Document_schema.Limits.default
       |> Or_error.ok_exn)
    ~limits:
      (Analyze.Limits.create
         ~max_document_bundle_files:4
         ~max_document_bundle_bytes:2048
         ~max_parser_work_steps:100_000
         ~max_diagnostic_work_steps:100_000
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

let selected_build ?contract body =
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
  let selected =
    Selection.Selected.admit
      ~build
      ~contract:(Fixtures.contract fixture)
      ~limits:
        (Selection.Limits.create
           ~max_identity_bytes:(4 * 1024 * 1024)
           ~max_work_steps:100_000
         |> Or_error.ok_exn)
      ~checkpoint:ignore
    |> ok
  in
  build, selected
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
  let native_contract =
    Chatml.Chatml_compiler_contract.for_static_surface
      (Chatml.Chatml_static_surface.of_runtime_surface
         Chatml.Chatml_builtin_surface.empty
         ~checkpoint:ignore)
      ~required_bindings:[]
  in
  List.iter
    [ "valid", "let main input = Task.pure(input)", None
    ; "invalid", "let x = 1;; let broken = true + 1", None
    ; "native", "let main = 1", Some native_contract
    ]
    ~f:(fun (label, body, contract) ->
      let _, selected = selected_build ?contract body in
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
      Foundation_tests.with_supplied_owner
        ~contents
        (fun owner actor connection expected env ->
           let semantic = analyzer ~script_selection:(Some selection) owner env in
           let request =
             A.Analyze.Request.create
               ~context:expected
               ~target:(A.Target.selected_script "s" |> ok)
             |> ok
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
           print_s
             [%sexp
               (label : string)
             , (kind : string)
             , (diagnostic_count : int)
             , (capture : bool)
             , (sources : bool)
             , (rows : int)];
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
               assert (not !published)
             | _ -> failwith "second genuine declaration was not certified")));
  [%expect
    {|
      (valid complete 0 true true 1)
      (true true true true)
      (invalid failed 1 true true 0)
      (native complete 0 true true 1) |}]
;;
