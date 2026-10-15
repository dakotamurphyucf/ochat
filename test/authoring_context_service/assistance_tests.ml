open! Core
module P = Agent_protocol
module Service = Agent_server.Authoring_context_service
module Assistance = Service.Assistance
module W = P.Authoring_assistance

let query_limits () =
  Service.Semantic.Query_limits.create
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
  | Error _ -> failwith "unexpected assistance fixture admission failure"
;;

let limits ?(result_bytes = 4096) ?(max_tool_metadata_bytes = 4096) () =
  Assistance.Limits.create
    ~protocol_limits:
      (P.Authoring.Limits.create
         ~max_sources:4
         ~max_source_bytes:1024
         ~max_context_source_bytes:4096
         ~max_request_bytes:4096
         ~max_result_bytes:result_bytes
         ~max_path_candidates:16
       |> ok)
    ~max_scan_work_steps:100_000
    ~max_assistance_work_steps:100_000
    ~max_contract_identity_bytes:4096
    ~max_contract_work_steps:100_000
    ~max_projection_work_steps:100_000
    ~max_parser_input_bytes:1024
    ~max_imported_inputs:4
    ~max_selected_tools:16
    ~max_tool_metadata_bytes
  |> Or_error.ok_exn
;;

let create_service
      ?(result_bytes = 4096)
      ?(max_tool_metadata_bytes = 4096)
      ?(selected_tools = None)
      owner
  =
  Assistance.create
    ~owner
    ~script_selection:None
    ~project_metadata:None
    ~selected_tools
    ~project_build_limits:
      (Chatml_project_build.Limits.create
         ~max_work_steps:10_000
         ~max_input_bytes:(Some 4096)
         ~document:Document_schema.Limits.default
       |> Or_error.ok_exn)
    ~limits:(limits ~result_bytes ~max_tool_metadata_bytes ())
;;

let service ?result_bytes ?max_tool_metadata_bytes ?selected_tools owner =
  create_service ?result_bytes ?max_tool_metadata_bytes ?selected_tools owner |> ok
;;

let%test_unit "malformed supplied source has current lexical and structural help" =
  let captured = ref None in
  Foundation_tests.with_supplied_owner
    ~contents:"<us"
    ~on_context:(fun context ->
      match P.Authoring.Context.basis context with
      | Document { source; _ } -> captured := Some source
      | Project _ -> failwith "supplied fixture gained a project")
    (fun owner actor connection expected _env ->
       let source = Option.value_exn !captured in
       (match create_service ~result_bytes:8192 owner with
        | Error Service.Error.Invalid_request -> ()
        | Ok _ | Error _ -> failwith "assistance accepted foreign protocol limits");
       let assistance = service owner in
       let lexical_request =
         W.Lexical.Request.create ~context:expected ~source ~view:Chatmd_authored |> ok
       in
       (match Assistance.handle_lexical assistance ~actor ~connection lexical_request with
        | Ok (Lexical result) ->
          assert (
            W.Lexical.Request.equal (W.Lexical.Result.request result) lexical_request);
          assert (List.length (W.Lexical.Result.issues result) > 0)
        | Ok _ | Error _ -> failwith "current lexical source not admitted");
       let request =
         W.Assistance.Request.create ~context:expected ~source ~cursor_byte:3 ~max_items:8
         |> ok
       in
       (match Assistance.handle_assistance assistance ~actor ~connection request with
        | Ok (Assistance result) ->
          assert (
            List.exists (W.Assistance.Result.items result) ~f:(fun item ->
              match W.Assistance.Suggestion.kind item with
              | Tag -> String.equal (W.Assistance.Suggestion.label item) "user"
              | _ -> false))
        | Ok _ | Error _ -> failwith "malformed authored structural help unavailable");
       let version =
         let read =
           P.Authoring.Document.Version_request.create
             ~context:expected
             ~source_id:(P.Authoring.Source.id source)
           |> ok
         in
         match
           Service.handle_core
             owner
             ~actor
             ~connection
             (P.Authoring.Wire.Request.Document_version read)
         with
         | Ok (Document_version version) -> version
         | Ok _ | Error _ -> failwith "supplied version missing"
       in
       let replace =
         P.Authoring.Document.Replace_request.create
           ~expected:version
           ~next_editor_revision:1
           ~contents:"<use"
         |> ok
       in
       (match
          Service.handle_core
            owner
            ~actor
            ~connection
            (P.Authoring.Wire.Request.Document_replace replace)
        with
        | Ok (Document_replace _) -> ()
        | Ok _ | Error _ -> failwith "supplied edit failed");
       match Assistance.handle_assistance assistance ~actor ~connection request with
       | Error Service.Error.Stale_context -> ()
       | Ok _ | Error _ -> failwith "assistance used an old source generation")
;;

let%test_unit "effective document view requires explicit current Analyze capture" =
  let text = {|<script id="worker" language="chatml" kind="tool">let x = 1</script>|} in
  let captured = ref None in
  Foundation_tests.with_supplied_owner
    ~contents:text
    ~result_bytes:65536
    ~on_context:(fun context ->
      match P.Authoring.Context.basis context with
      | Document { source; _ } -> captured := Some source
      | Project _ -> failwith "supplied fixture gained a project")
    (fun owner actor connection expected env ->
       let source = Option.value_exn !captured in
       let assistance = service ~result_bytes:65536 owner in
       let request =
         W.Lexical.Request.create ~context:expected ~source ~view:Chatmd_effective |> ok
       in
       (match Assistance.handle_lexical assistance ~actor ~connection request with
        | Error Service.Error.Source_unavailable -> ()
        | Ok _ | Error _ -> failwith "uncaptured effective view was served");
       let analyze =
         P.Authoring.Analysis.Analyze.Request.create
           ~context:expected
           ~target:(P.Authoring.Analysis.Target.selected_script "worker" |> ok)
         |> ok
       in
       (match
          Service.Semantic.analyze
            (Analysis_tests.analyzer owner env)
            ~actor
            ~connection
            analyze
        with
        | Ok (Unavailable { reason = No_validated_project; _ }) -> ()
        | Ok (Complete _ | Failed _) | Error _ ->
          failwith "unselected script became certified or failed capture"
        | Ok (Unavailable _) -> failwith "valid root-only syntax was not captured");
       let has_capture, _, _ =
         Analysis_tests.inspect_capture owner actor connection expected
       in
       assert has_capture;
       match Assistance.handle_lexical assistance ~actor ~connection request with
       | Ok (Lexical result) ->
         assert (W.Lexical.Request.equal (W.Lexical.Result.request result) request)
       | Error error ->
         failwith
           ("captured effective view unavailable: "
            ^ P.Error.code_to_string (Service.Error.to_protocol error).code)
       | Ok _ -> failwith "captured effective view returned wrong method")
;;

let%test_unit "selected tool policy revocation during request rejects publication" =
  let text = {|<tool name="t" type="chatml"><uses tool="re|} in
  let captured = ref None in
  Foundation_tests.with_supplied_owner
    ~contents:text
    ~result_bytes:65536
    ~on_context:(fun context ->
      match P.Authoring.Context.basis context with
      | Document { source; _ } -> captured := Some source
      | Project _ -> failwith "supplied fixture gained a project")
    (fun owner actor connection expected _env ->
       let module Definition = struct
         type input = string

         let name = "read_file"
         let description = Some "selected authoring fixture"
         let type_ = "function"
         let parameters = `Object [ "type", `String "object" ]
         let input_of_string input = input
       end
       in
       let runner =
         Ochat_function.create_function
           (module Definition)
           (fun _ -> Openai.Responses.Tool_output.Output.Text "not called")
       in
       let registry =
         Chat_response.Tool_capability.create
           ~owner:"authoring test"
           ~resource_fingerprint:(String.make 64 'a')
           [ String.make 64 'b', runner ]
         |> ok
       in
       let selected =
         Assistance.Selected_tool_policy.Registry.admit
           registry
           ~limits:(limits ~result_bytes:65536 ())
           ~checkpoint:ignore
         |> ok
       in
       let active = ref true in
       let first_selection, signal = Eio.Promise.create () in
       let signal_enabled = ref false in
       let selected_tools =
         Assistance.Selected_tool_policy.create
           ~current:(fun ~original_actor:_ ~actor:_ ~context:_ ->
             if !signal_enabled
             then (
               signal_enabled := false;
               Eio.Promise.resolve signal ());
             if !active then Some selected else None)
       in
       let assistance =
         service ~result_bytes:65536 ~selected_tools:(Some selected_tools) owner
       in
       let request =
         W.Assistance.Request.create
           ~context:expected
           ~source:(Option.value_exn !captured)
           ~cursor_byte:(String.length text)
           ~max_items:8
         |> ok
       in
       let selected_item
             (response : (P.Authoring.Wire.Response.t, Service.Error.t) result)
         =
         match response with
         | Ok (Assistance result) ->
           List.exists (W.Assistance.Result.items result) ~f:(fun item ->
             match W.Assistance.Suggestion.evidence item with
             | Selected_tool _ ->
               String.equal (W.Assistance.Suggestion.insert_text item) "read_file"
             | _ -> false)
         | Ok _ | Error _ -> false
       in
       assert (
         selected_item
           (Assistance.handle_assistance assistance ~actor ~connection request));
       let narrower =
         service
           ~result_bytes:65536
           ~max_tool_metadata_bytes:1
           ~selected_tools:(Some selected_tools)
           owner
       in
       (match Assistance.handle_assistance narrower ~actor ~connection request with
        | Error Service.Error.Resource_limit -> ()
        | Ok _ | Error _ -> failwith "preadmitted tool registry exceeded host cap");
       signal_enabled := true;
       let result = ref None in
       Eio.Fiber.both
         (fun () ->
            result
            := Some (Assistance.handle_assistance assistance ~actor ~connection request))
         (fun () ->
            Eio.Promise.await first_selection;
            active := false);
       match Option.value_exn !result with
       | Error Service.Error.Selection_unavailable -> ()
       | Ok _ | Error _ -> failwith "revoked selected registry served assistance")
;;

let%test_unit "host Analyze retains imported references and decoded project metadata" =
  Eio_main.run (fun env ->
    let directory =
      Eio.Path.(env#fs / "/tmp" / P.Id.Transaction.(create () |> to_string))
    in
    Eio.Path.mkdir ~perm:0o700 directory;
    Fun.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree directory))
      (fun () ->
         let nested = Eio.Path.(directory / "nested") in
         Eio.Path.mkdir ~perm:0o700 nested;
         let root_text =
           {|<import src="nested/part.chatmd"/><script id="s" language="chatml" kind="tool" project="./chatml.project.json" environment="dev">let main input = Task.pure(input)</script><tool name="do_work" type="chatml" script="worker" entrypoint="run" input_schema="schema.json" output_schema="schema.json"/>|}
         in
         let import_text =
           {|<script id="worker" language="chatml" kind="tool">let run = fun ctx input -> Task.pure(input)</script><tool name="inside" type="chatml" script="worker" entrypoint="run" input_schema="../schema.json" output_schema="../schema.json"/>|}
         in
         let save path contents =
           Eio.Path.save ~create:(`Or_truncate 0o600) path contents
         in
         save Eio.Path.(directory / "draft.chatmd") root_text;
         save Eio.Path.(nested / "part.chatmd") import_text;
         save Eio.Path.(directory / "schema.json") {|{"type":"object"}|};
         save Eio.Path.(directory / "main.chatml") "let main input = Task.pure(input)";
         save
           Eio.Path.(directory / "chatml.project.json")
           {|{"schema_version":1,"namespace":"authoring","environments":{"dev":{"modules":{}}},"entries":{"main":{"source":"main.chatml","target":"one_off_v1"}}}|};
         let data_root =
           Agent_store.Data_root.create
             ~env
             ~path:(Eio.Path.native_exn Eio.Path.(directory / "data"))
           |> ok
         in
         Eio.Switch.run (fun sw ->
           let principal =
             P.Principal.create
               ~id:(P.Id.Principal.of_string "pri_assistance_host" |> ok)
               ~authentication_kind:"test"
               ~scopes:P.Scope.Set.empty
               ~attributes:[]
             |> ok
           in
           let actor = Operator_authorization.trusted_local principal in
           let source_owner =
             Agent_server.Project_source_host.open_owned
               { external_roots = [ Eio.Path.native_exn directory ]; git = None }
               ~sw
               ~fs:env#fs
               ~data_root
               ~clock:env#mono_clock
               ~checkpoint:ignore
               ()
             |> ok
           in
           let source_host =
             Agent_server.Authoring_source_host.create
               source_owner
               ~sw
               ~read_policy:
                 (Agent_server.Authoring_source_host.Read_policy.create
                    ~authorize:(fun _ ~root_index -> Int.equal root_index 0))
               ~document_policy:
                 Agent_server.Authoring_source_host.Document_policy.deny_all
               ~limits:
                 (Agent_server.Authoring_source_host.Limits.create
                    ~max_contexts:2
                    ~max_context_requests:24
                    ~max_manifest_entries:8
                    ~max_browse_entries:16
                    ~max_read_bytes:4096
                    ~max_context_read_bytes:16384
                  |> Or_error.ok_exn)
           in
           let selected_build =
             let selected_root =
               Agent_server.Authoring_source_host.roots
                 source_host
                 ~actor
                 ~checkpoint:ignore
               |> ok
               |> List.hd_exn
             in
             let selected_owner =
               Agent_server.Authoring_source_host.Context.open_root
                 source_host
                 ~actor
                 ~root:selected_root
                 ~dependency_roots:[]
                 ~project:(Source.Portable_path.create "chatml.project.json" |> ok)
                 ~selection:
                   (Chatml_project.Selection.Entry
                      { name = "main"; environment = Some "dev" })
                 ~contract:
                   (Chatml.Chatml_compiler_contract.for_target
                      One_off_v1
                      ~checkpoint:ignore)
               |> ok
             in
             let build =
               Agent_server.Authoring_source_host.Context.capture
                 selected_owner
                 ~actor
                 ~clock:env#mono_clock
                 ~checkpoint:ignore
               |> ok
               |> Agent_server.Authoring_source_host.Capture.build
             in
             Agent_server.Authoring_source_host.Context.close_and_wait selected_owner;
             build
           in
           let host = P.Id.Server.create () in
           let connection =
             Agent_server.Connection_context.create_authenticated
               ~actor
               ~connection_id:"assistance host"
               ~transport:In_memory
               ~publish_notification:(fun _ -> ())
               ~max_attachments:1
           in
           let protocol_limits =
             P.Authoring.Limits.create
               ~max_sources:8
               ~max_source_bytes:4096
               ~max_context_source_bytes:8192
               ~max_request_bytes:8192
               ~max_result_bytes:65536
               ~max_path_candidates:16
             |> ok
           in
           let owner =
             Service.create
               ~sw
               ~clock:env#mono_clock
               ~host
               ~sources:(Some source_host)
               ~upload:None
               ~supplied_document:None
               ~selection_policy:
                 (Service.Selection_policy.create
                    ~project_contract:(fun _ ~root:_ ~project:_ ~selection:_ -> None)
                    ~document_static:(fun _ ~requested_environment:_ -> None))
               ~context_limits:
                 (Chatml_authoring_context.Limits.create
                    ~max_contexts:2
                    ~max_sources:8
                    ~max_open_documents:1
                    ~max_document_bytes:4096
                    ~max_effective_input_bytes:8192
                    ~max_context_source_bytes:8192
                    ~max_total_source_bytes:16384
                  |> Or_error.ok_exn)
               ~analysis_policy:(Service_tests.analysis_policy ())
               ~protocol_limits
               ~limits:
                 (Service.Limits.create
                    ~max_connections:1
                    ~max_active_requests_per_connection:2
                    ~max_roots_per_connection:1
                    ~max_response_bytes:65536
                    ~max_provisional_results:8
                  |> Or_error.ok_exn)
           in
           Service.publish_initialize_and_wait
             owner
             ~connection
             ~response:
               (Service_tests.initialize_response_with_features
                  ~enabled_features:
                    [ P.Authoring.Features.core
                    ; P.Authoring.Features.semantic
                    ; P.Authoring.Features.lexical
                    ; P.Authoring.Features.assistance
                    ]
                  ~server_id:host
                  ~principal)
           |> ok;
           let call request = Service.handle_core owner ~actor ~connection request in
           let root =
             match call Roots with
             | Ok (Roots roots) -> (List.hd_exn roots.roots).id
             | Ok _ | Error _ -> failwith "host root unavailable"
           in
           let context =
             match
               call
                 (Context_open
                    (P.Authoring.Context.Open_request.create_host_document
                       ~root
                       ~member:"draft.chatmd"
                       ~default_environment:None
                     |> ok))
             with
             | Ok (Context_open context) -> context
             | Ok _ | Error _ -> failwith "host document unavailable"
           in
           let source =
             match P.Authoring.Context.basis context with
             | Document { source; _ } -> source
             | Project _ -> failwith "host document gained project basis"
           in
           let expected = P.Authoring.Context.Expected.of_view context in
           let project_metadata =
             Service.Semantic.Project_metadata.Policy.create
               ~authorize_now:
                 (fun
                   ~original_actor:_
                   ~actor:_
                   ~current:_
                   ~parsed:_
                   ~declaration:_
                   ~capture:_
                 -> true)
           in
           let analyzer =
             Service.Semantic.create
               ~owner
               ~env
               ~document_dir:(Eio.Stdenv.cwd env)
               ~script_selection:None
               ~project_metadata:(Some project_metadata)
               ~project_build_limits:
                 (Chatml_project_build.Limits.create
                    ~max_work_steps:100_000
                    ~max_input_bytes:(Some 8192)
                    ~document:Document_schema.Limits.default
                  |> Or_error.ok_exn)
               ~limits:
                 (Service.Semantic.Limits.create
                    ~max_document_bundle_files:4
                    ~max_document_bundle_bytes:8192
                    ~max_parser_work_steps:100_000
                    ~max_diagnostic_work_steps:100_000
                  |> Or_error.ok_exn)
               ~query_limits:(query_limits ())
               ~prefix_limits:
                 (Service.Semantic.Prefix_limits.create
                    ~max_document_bundle_files:4
                    ~max_document_bundle_bytes:8192
                    ~max_parser_work_steps:100_000
                    ~max_host_work_steps:100_000
                  |> Or_error.ok_exn)
           in
           let analyze =
             P.Authoring.Analysis.Analyze.Request.create
               ~context:expected
               ~target:(P.Authoring.Analysis.Target.selected_script "s" |> ok)
             |> ok
           in
           (match Service.Semantic.analyze analyzer ~actor ~connection analyze with
            | Ok (Unavailable { reason = No_validated_project; _ }) -> ()
            | Ok _ | Error _ -> failwith "metadata Analyze did not retain parsed source");
           let selected =
             Service.Semantic.Script_selection.Selected.admit
               ~build:selected_build
               ~contract:
                 (Chatml.Chatml_compiler_contract.for_target
                    One_off_v1
                    ~checkpoint:ignore)
               ~limits:
                 (Service.Semantic.Script_selection.Limits.create
                    ~max_identity_bytes:(4 * 1024 * 1024)
                    ~max_work_steps:100_000
                  |> Or_error.ok_exn)
               ~checkpoint:ignore
             |> ok
           in
           let selected_policy =
             Service.Semantic.Script_selection.Policy.create
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
           let selected_analyzer =
             Service.Semantic.create
               ~owner
               ~env
               ~document_dir:(Eio.Stdenv.cwd env)
               ~script_selection:(Some selected_policy)
               ~project_metadata:(Some project_metadata)
               ~project_build_limits:
                 (Chatml_project_build.Limits.create
                    ~max_work_steps:100_000
                    ~max_input_bytes:(Some 8192)
                    ~document:Document_schema.Limits.default
                  |> Or_error.ok_exn)
               ~limits:
                 (Service.Semantic.Limits.create
                    ~max_document_bundle_files:4
                    ~max_document_bundle_bytes:8192
                    ~max_parser_work_steps:100_000
                    ~max_diagnostic_work_steps:100_000
                  |> Or_error.ok_exn)
               ~query_limits:(query_limits ())
               ~prefix_limits:
                 (Service.Semantic.Prefix_limits.create
                    ~max_document_bundle_files:4
                    ~max_document_bundle_bytes:8192
                    ~max_parser_work_steps:100_000
                    ~max_host_work_steps:100_000
                  |> Or_error.ok_exn)
           in
           (match
              Service.Semantic.analyze selected_analyzer ~actor ~connection analyze
            with
            | Ok (Complete _) | Ok (Failed _) -> ()
            | Ok _ | Error _ -> failwith "selected host project was not admitted");
           let make_assistance max_contract_identity_bytes =
             Assistance.create
               ~owner
               ~script_selection:(Some selected_policy)
               ~project_metadata:(Some project_metadata)
               ~selected_tools:None
               ~project_build_limits:
                 (Chatml_project_build.Limits.create
                    ~max_work_steps:100_000
                    ~max_input_bytes:(Some 8192)
                    ~document:Document_schema.Limits.default
                  |> Or_error.ok_exn)
               ~limits:
                 (Assistance.Limits.create
                    ~protocol_limits
                    ~max_scan_work_steps:1_000_000
                    ~max_assistance_work_steps:1_000_000
                    ~max_contract_identity_bytes
                    ~max_contract_work_steps:1_000_000
                    ~max_projection_work_steps:1_000_000
                    ~max_parser_input_bytes:4096
                    ~max_imported_inputs:8
                    ~max_selected_tools:16
                    ~max_tool_metadata_bytes:4096
                  |> Or_error.ok_exn)
           in
           let assistance = make_assistance (4 * 1024 * 1024) |> ok in
           let query ?(offset = -1) source text marker =
             let cursor_byte =
               String.substr_index_exn text ~pattern:marker
               + String.length marker
               + offset
             in
             let request =
               W.Assistance.Request.create
                 ~context:expected
                 ~source
                 ~cursor_byte
                 ~max_items:32
               |> ok
             in
             match Assistance.handle_assistance assistance ~actor ~connection request with
             | Ok (Assistance result) -> W.Assistance.Result.items result
             | Error error ->
               failwith
                 (P.Error.to_json (Service.Error.to_protocol error) |> Jsonaf.to_string)
             | Ok _ -> failwith "host assistance returned another method"
           in
           let environment = query source root_text {|environment="dev"|} in
           assert (
             List.exists environment ~f:(fun item ->
               match W.Assistance.Suggestion.evidence item with
               | Parsed_project project ->
                 W.Assistance.Suggestion.equal_kind
                   (W.Assistance.Suggestion.kind item)
                   Environment
                 && String.equal (W.Assistance.Suggestion.insert_text item) "dev"
                 && String.equal
                      (Chatml_source.Id.relative_path (P.Authoring.Source.id project))
                      "chatml.project.json"
               | _ -> false));
           let catalog = query ~offset:0 source root_text "let main input = " in
           assert (
             List.exists catalog ~f:(fun item ->
               match W.Assistance.Suggestion.evidence item with
               | Selected_catalog _ ->
                 W.Assistance.Suggestion.equal_kind
                   (W.Assistance.Suggestion.kind item)
                   Reference
                 && String.equal (W.Assistance.Suggestion.insert_text item) "Job"
               | _ -> false));
           let tight_assistance = make_assistance 4096 |> ok in
           let script_cursor =
             String.substr_index_exn root_text ~pattern:"let main input = "
             + String.length "let main input = "
           in
           let tight_request =
             W.Assistance.Request.create
               ~context:expected
               ~source
               ~cursor_byte:script_cursor
               ~max_items:32
             |> ok
           in
           (match
              Assistance.handle_assistance
                tight_assistance
                ~actor
                ~connection
                tight_request
            with
            | Error Service.Error.Resource_limit -> ()
            | Ok _ | Error _ -> failwith "tight selected-contract budget admitted");
           let paths = query ~offset:0 source root_text {|src="nested/part.chatmd|} in
           assert (
             List.exists paths ~f:(fun item ->
               match W.Assistance.Suggestion.evidence item with
               | Host_root root_id ->
                 W.Assistance.Suggestion.equal_kind
                   (W.Assistance.Suggestion.kind item)
                   Path
                 && P.Id.Authoring_root.equal root_id root
                 && String.equal
                      (W.Assistance.Suggestion.insert_text item)
                      "nested/part.chatmd"
               | _ -> false));
           Eio.Path.unlink Eio.Path.(nested / "part.chatmd");
           let references = query source root_text {|script="worker"|} in
           assert (
             List.exists references ~f:(fun item ->
               match W.Assistance.Suggestion.evidence item with
               | Parsed_document imported ->
                 W.Assistance.Suggestion.equal_kind
                   (W.Assistance.Suggestion.kind item)
                   Reference
                 && String.equal (W.Assistance.Suggestion.insert_text item) "worker"
                 && String.equal
                      (Chatml_source.Id.relative_path (P.Authoring.Source.id imported))
                      "nested/part.chatmd"
               | _ -> false));
           let imported_source =
             P.Authoring.Source.create
               ~id:
                 (Chatml_source.Id.create
                    ~package_namespace:
                      (Chatml_source.Id.package_namespace (P.Authoring.Source.id source))
                    ~relative_path:"nested/part.chatmd"
                  |> Or_error.ok_exn)
               ~digest:(Chatml_source.Digest.of_contents import_text ~checkpoint:ignore)
               ~byte_length:(String.length import_text)
               ~editor_revision:None
             |> ok
           in
           let imported_references =
             query imported_source import_text {|script="worker"|}
           in
           assert (
             List.exists imported_references ~f:(fun item ->
               match W.Assistance.Suggestion.evidence item with
               | Parsed_document imported ->
                 W.Assistance.Suggestion.equal_kind
                   (W.Assistance.Suggestion.kind item)
                   Reference
                 && String.equal (W.Assistance.Suggestion.insert_text item) "worker"
                 && P.Authoring.Source.equal imported imported_source
               | _ -> false)))))
;;
