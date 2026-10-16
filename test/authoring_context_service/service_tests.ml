open! Core
module P = Agent_protocol
module S = Agent_server.Authoring_context_service
module C = Chatml_authoring_context
module Q = P.Authoring.Analysis.Query

let () = Mirage_crypto_rng_unix.use_default ()

let query_limits () =
  S.Semantic.Query_limits.create
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
  | Error _ -> failwith "unexpected bounded setup/result failure"
;;

let analysis_policy ?(compilation = Chatml_compilation.default_limits) () =
  let semantic =
    Chatml_semantics.Limits.create
      ~max_input_bytes:1_000_000
      ~recording:
        (Chatml_typechecker.Semantic.Limits.create
           ~max_facts:20_000
           ~max_edges:40_000
           ~max_label_bytes:400_000
           ~max_work_steps:400_000
         |> Or_error.ok_exn)
      ~type_graph:
        (Chatml_semantics.Frozen.Limits.create
           ~max_nodes:20_000
           ~max_edges:40_000
           ~max_label_bytes:400_000
         |> Or_error.ok_exn)
      ~max_published_facts:20_000
      ~max_published_edges:40_000
      ~max_published_label_bytes:400_000
      ~max_work_steps:400_000
    |> Or_error.ok_exn
  in
  Chatml_authoring_analysis.Policy.create
    ~compilation
    ~semantic
    ~max_dependency_depth:16
    ~max_results:2
    ~max_projected_diagnostics:4
    ~max_projected_sites:12
    ~max_projection_sources:8
    ~max_position_index_entries:256
    ~max_projection_source_bytes:1_000_000
  |> Or_error.ok_exn
;;

let initialize_response_with_features ~enabled_features ~server_id ~principal =
  let version = P.Version.create ~major:1 ~minor:0 |> ok in
  let implementation =
    P.Initialize.Implementation.create ~name:"authoring test" ~version:"1" |> ok
  in
  P.Initialize.Response.create
    ~protocol_name:"ochat.agent"
    ~selected_version:version
    ~implementation
    ~server_id
    ~enabled_features
    ~extensions:None
    ~principal
    ~limits:
      { max_request_bytes = 16 * 1024 * 1024
      ; max_event_bytes = 1024
      ; max_page_size = 100
      ; max_attachments_per_connection = 1
      }
    ~event_retention:
      { minimum_age_ms = 0; maximum_events = 1; oldest_replayable_sequence = None }
    ~timing:
      { heartbeat_interval_ms = 1000
      ; owner_lease_duration_ms = 10000
      ; owner_renew_after_ms = 5000
      ; disconnect_grace_default_ms = 0
      }
    ~server_time:(P.Timestamp.of_string "2026-10-13T00:00:00Z" |> ok)
  |> ok
;;

let initialize_response ~server_id ~principal =
  initialize_response_with_features
    ~enabled_features:[ P.Authoring.Features.core ]
    ~server_id
    ~principal
;;

let with_supplied_service
      ?(protocol_source_bytes = 1024)
      ?(protocol_request_bytes = 2048)
      f
  =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let principal =
        P.Principal.create
          ~id:(P.Id.Principal.of_string "pri_authoring_service" |> ok)
          ~authentication_kind:"test"
          ~scopes:P.Scope.Set.empty
          ~attributes:[]
        |> ok
      in
      let actor = Operator_authorization.trusted_local principal in
      let supplied_allowed = ref true in
      let on_authorize = ref Fn.id in
      let connection =
        Agent_server.Connection_context.create_authenticated
          ~actor
          ~connection_id:"authoring service test"
          ~transport:In_memory
          ~publish_notification:(fun _ -> ())
          ~max_attachments:1
      in
      let host = P.Id.Server.create () in
      let service =
        S.create
          ~sw
          ~clock:env#mono_clock
          ~host
          ~sources:None
          ~upload:None
          ~supplied_document:
            (Some
               (S.Supplied_document_policy.create ~authorize:(fun _ ->
                  !on_authorize ();
                  !supplied_allowed)))
          ~selection_policy:
            (S.Selection_policy.create
               ~project_contract:(fun _ ~root:_ ~project:_ ~selection:_ -> None)
               ~document_static:(fun _ ~requested_environment:_ -> None))
          ~context_limits:
            (C.Limits.create
               ~max_contexts:2
               ~max_sources:4
               ~max_open_documents:1
               ~max_document_bytes:1024
               ~max_effective_input_bytes:2048
               ~max_context_source_bytes:2048
               ~max_total_source_bytes:4096
             |> Or_error.ok_exn)
          ~analysis_policy:(analysis_policy ())
          ~protocol_limits:
            (P.Authoring.Limits.create
               ~max_sources:4
               ~max_source_bytes:protocol_source_bytes
               ~max_context_source_bytes:2048
               ~max_request_bytes:protocol_request_bytes
               ~max_result_bytes:4096
               ~max_path_candidates:16
             |> ok)
          ~limits:
            (S.Limits.create
               ~max_connections:2
               ~max_active_requests_per_connection:2
               ~max_roots_per_connection:2
               ~max_response_bytes:4096
               ~max_provisional_results:8
             |> Or_error.ok_exn)
      in
      let response = initialize_response ~server_id:host ~principal in
      S.publish_initialize_and_wait service ~connection ~response |> ok;
      f sw service host connection actor principal supplied_allowed on_authorize))
;;

let%test_unit "supplied document admission does not require native source host" =
  with_supplied_service (fun _ service _ connection actor principal supplied_allowed _ ->
    let request =
      P.Authoring.Context.Open_request.create_supplied_document
        ~member:"draft.chatmd"
        ~default_environment:None
        ~contents:"unclosed `chatml"
      |> ok
    in
    let context =
      match
        S.handle_core
          service
          ~actor
          ~connection
          (P.Authoring.Wire.Request.Context_open request)
      with
      | Ok (Context_open context) -> context
      | Ok _ | Error _ -> failwith "supplied document open failed"
    in
    let expected = P.Authoring.Context.Expected.of_view context in
    (match
       Stdlib.Domain.join
         (Stdlib.Domain.spawn (fun () ->
            S.handle_core
              service
              ~actor
              ~connection
              (P.Authoring.Wire.Request.Context_open request)))
     with
     | Error S.Error.Closed -> ()
     | Ok _ | Error _ -> failwith "foreign-domain service use was admitted");
    let source_id =
      match P.Authoring.Context.basis context with
      | Document { source; _ } -> P.Authoring.Source.id source
      | Project _ -> failwith "supplied document gained project graph"
    in
    let read =
      P.Authoring.Source_read.Read_request.create ~context:expected ~source_id |> ok
    in
    (match
       S.handle_core
         service
         ~actor
         ~connection
         (P.Authoring.Wire.Request.Source_read read)
     with
     | Ok (Source_read result) ->
       [%test_eq: string]
         (P.Authoring.Source_read.Read_result.contents result)
         "unclosed `chatml"
     | Ok _ | Error _ -> failwith "pinned supplied read failed");
    let version_request =
      P.Authoring.Document.Version_request.create ~context:expected ~source_id |> ok
    in
    let version =
      match
        S.handle_core
          service
          ~actor
          ~connection
          (P.Authoring.Wire.Request.Document_version version_request)
      with
      | Ok (Document_version version) -> version
      | Ok _ | Error _ -> failwith "supplied version failed"
    in
    let replacement =
      P.Authoring.Document.Replace_request.create
        ~expected:version
        ~next_editor_revision:1
        ~contents:"edited draft"
      |> ok
    in
    let edited =
      match
        S.handle_core
          service
          ~actor
          ~connection
          (P.Authoring.Wire.Request.Document_replace replacement)
      with
      | Ok (Document_replace result) -> result
      | Ok _ | Error _ -> failwith "supplied edit failed"
    in
    let edited_context = edited.P.Authoring.Document.Result.context in
    let edited_version = edited.version in
    assert (P.Authoring.Document.Version.overlay_open edited_version);
    [%test_eq: int]
      (P.Authoring.Context.generation edited_context)
      (P.Authoring.Context.generation context + 1);
    (match
       S.handle_core
         service
         ~actor
         ~connection
         (P.Authoring.Wire.Request.Source_read read)
     with
     | Error S.Error.Stale_context -> ()
     | Ok _ | Error _ -> failwith "old generation read admitted");
    supplied_allowed := false;
    let edited_read =
      P.Authoring.Source_read.Read_request.create
        ~context:(P.Authoring.Context.Expected.of_view edited_context)
        ~source_id
      |> ok
    in
    (match
       S.handle_core
         service
         ~actor
         ~connection
         (P.Authoring.Wire.Request.Source_read edited_read)
     with
     | Error S.Error.Unauthorized -> ()
     | Ok _ | Error _ -> failwith "revoked document grant served cached bytes");
    (match
       S.handle_core
         service
         ~actor
         ~connection
         (P.Authoring.Wire.Request.Context_close
            (P.Authoring.Context.Close_request.create
               ~expected:(P.Authoring.Context.Expected.of_view edited_context)))
     with
     | Error S.Error.Unauthorized -> ()
     | Ok _ | Error _ -> failwith "revoked grant closed current context");
    supplied_allowed := true;
    (match
       S.handle_core
         service
         ~actor
         ~connection
         (P.Authoring.Wire.Request.Source_read edited_read)
     with
     | Ok (Source_read _) -> ()
     | Ok _ | Error _ -> failwith "rejected close removed context");
    let closed =
      match
        S.handle_core
          service
          ~actor
          ~connection
          (P.Authoring.Wire.Request.Document_close
             (P.Authoring.Document.Close_request.create ~expected:edited_version))
      with
      | Ok (Document_close result) -> result
      | Ok _ | Error _ -> failwith "overlay close failed"
    in
    assert (not (P.Authoring.Document.Version.overlay_open closed.version));
    (match
       S.handle_core
         service
         ~actor
         ~connection
         (P.Authoring.Wire.Request.Source_read edited_read)
     with
     | Error S.Error.Stale_context -> ()
     | Ok _ | Error _ -> failwith "closed overlay generation remained current");
    let response =
      initialize_response ~server_id:(P.Authoring.Context.host context) ~principal
    in
    S.publish_initialize_and_wait service ~connection ~response |> ok;
    (match
       S.handle_core
         service
         ~actor
         ~connection
         (P.Authoring.Wire.Request.Source_read edited_read)
     with
     | Error S.Error.Stale_context -> ()
     | Ok _ | Error _ -> failwith "old context survived reinitialization");
    S.publish_initialize_and_wait
      service
      ~connection
      ~response:
        (initialize_response_with_features
           ~enabled_features:[]
           ~server_id:(P.Authoring.Context.host context)
           ~principal)
    |> ok;
    match
      S.handle_core
        service
        ~actor
        ~connection
        (P.Authoring.Wire.Request.Context_open request)
    with
    | Error S.Error.Unselected_feature -> ()
    | Ok _ | Error _ -> failwith "unselected core method was admitted")
;;

let%test_unit "advertised request and source ceilings bind direct admission" =
  let request =
    P.Authoring.Context.Open_request.create_supplied_document
      ~member:"draft.chatmd"
      ~default_environment:None
      ~contents:"abcde"
    |> ok
  in
  List.iter
    [ 4, 2048; 1024, 1 ]
    ~f:(fun (protocol_source_bytes, protocol_request_bytes) ->
      with_supplied_service
        ~protocol_source_bytes
        ~protocol_request_bytes
        (fun _ service _ connection actor _ _ _ ->
           match
             S.handle_core
               service
               ~actor
               ~connection
               (P.Authoring.Wire.Request.Context_open request)
           with
           | Error S.Error.Resource_limit -> ()
           | Ok _ | Error _ -> failwith "advertised limit did not bind admission"))
;;

let%test_unit "host root read is fresh while project context remains captured" =
  Eio_main.run (fun env ->
    let directory =
      Eio.Path.(env#fs / "/tmp" / P.Id.Transaction.(create () |> to_string))
    in
    Eio.Path.mkdir ~perm:0o700 directory;
    Fun.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree directory))
      (fun () ->
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           Eio.Path.(directory / "chatml.project.json")
           {|{"schema_version":1,"namespace":"authoring","entries":{"main":{"source":"main.chatml","target":"one_off_v1"}}}|};
         let script_file = Eio.Path.(directory / "main.chatml") in
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           script_file
           "let main input = Task.pure(input)\n";
         let second_project =
           {|{"schema_version":1,"namespace":"second","environments":{"other":{"modules":{}}},"entries":{"main":{"source":"second.chatml","target":"one_off_v1"}}}|}
         in
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           Eio.Path.(directory / "second.project.json")
           second_project;
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           Eio.Path.(directory / "second.chatml")
           "let main input = Task.pure(input)\n\n";
         let authored_document =
           "<script id=\"s\" language=\"chatml\" kind=\"tool\" \
            project=\"./chatml.project.json\">let main input = Task.pure(input)\n\
            </script>"
         in
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           Eio.Path.(directory / "draft.chatmd")
           authored_document;
         let data_root =
           Agent_store.Data_root.create
             ~env
             ~path:(Eio.Path.native_exn Eio.Path.(directory / "data"))
           |> ok
         in
         Eio.Switch.run (fun sw ->
           let actor =
             P.Principal.create
               ~id:(P.Id.Principal.of_string "pri_native_authoring" |> ok)
               ~authentication_kind:"test"
               ~scopes:P.Scope.Set.empty
               ~attributes:[]
             |> ok
             |> Operator_authorization.trusted_local
           in
           let principal = Operator_authorization.principal actor in
           let source_owner =
             Agent_server.Project_source_host.open_owned
               { external_roots = [ Eio.Path.native_exn directory ]; git = None }
               ~sw
               ~fs:env#fs
               ~data_root
               ~clock:env#mono_clock
               ~checkpoint:Fn.id
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
                    ~max_read_bytes:1024
                    ~max_context_read_bytes:4096
                  |> Or_error.ok_exn)
           in
           let static_root =
             Agent_server.Authoring_source_host.roots source_host ~actor ~checkpoint:Fn.id
             |> ok
             |> List.hd_exn
           in
           let static_owner =
             Agent_server.Authoring_source_host.Context.open_root
               source_host
               ~actor
               ~root:static_root
               ~dependency_roots:[]
               ~project:(Source.Portable_path.create "chatml.project.json" |> ok)
               ~selection:
                 (Chatml_project.Selection.Entry { name = "main"; environment = None })
               ~contract:
                 (Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:Fn.id)
             |> ok
           in
           let static_build =
             Agent_server.Authoring_source_host.Context.capture
               static_owner
               ~actor
               ~clock:env#mono_clock
               ~checkpoint:Fn.id
             |> ok
             |> Agent_server.Authoring_source_host.Capture.build
           in
           Agent_server.Authoring_source_host.Context.close_and_wait static_owner;
           let second_owner =
             Agent_server.Authoring_source_host.Context.open_root
               source_host
               ~actor
               ~root:static_root
               ~dependency_roots:[]
               ~project:(Source.Portable_path.create "second.project.json" |> ok)
               ~selection:
                 (Chatml_project.Selection.Entry
                    { name = "main"; environment = Some "other" })
               ~contract:
                 (Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:Fn.id)
             |> ok
           in
           let second_build =
             Agent_server.Authoring_source_host.Context.capture
               second_owner
               ~actor
               ~clock:env#mono_clock
               ~checkpoint:Fn.id
             |> ok
             |> Agent_server.Authoring_source_host.Capture.build
           in
           Agent_server.Authoring_source_host.Context.close_and_wait second_owner;
           let static_allowed = ref true in
           let project_allowed = ref true in
           let on_static = ref Fn.id in
           let host = P.Id.Server.create () in
           let connection =
             Agent_server.Connection_context.create_authenticated
               ~actor
               ~connection_id:"host-context-test"
               ~transport:In_memory
               ~publish_notification:(fun _ -> ())
               ~max_attachments:1
           in
           let service =
             S.create
               ~sw
               ~clock:env#mono_clock
               ~host
               ~sources:(Some source_host)
               ~upload:None
               ~supplied_document:None
               ~selection_policy:
                 (S.Selection_policy.create
                    ~project_contract:(fun _ ~root:_ ~project:_ ~selection:_ ->
                      if !project_allowed
                      then
                        Some
                          (Chatml.Chatml_compiler_contract.for_target
                             One_off_v1
                             ~checkpoint:Fn.id)
                      else None)
                    ~document_static:(fun _ ~requested_environment:_ ->
                      !on_static ();
                      if !static_allowed then Some static_build else None))
               ~context_limits:
                 (C.Limits.create
                    ~max_contexts:2
                    ~max_sources:8
                    ~max_open_documents:1
                    ~max_document_bytes:1024
                    ~max_effective_input_bytes:4096
                    ~max_context_source_bytes:4096
                    ~max_total_source_bytes:8192
                  |> Or_error.ok_exn)
               ~analysis_policy:(analysis_policy ())
               ~protocol_limits:
                 (P.Authoring.Limits.create
                    ~max_sources:8
                    ~max_source_bytes:4096
                    ~max_context_source_bytes:8192
                    ~max_request_bytes:4096
                    ~max_result_bytes:4096
                    ~max_path_candidates:16
                  |> ok)
               ~limits:
                 (S.Limits.create
                    ~max_connections:2
                    ~max_active_requests_per_connection:2
                    ~max_roots_per_connection:2
                    ~max_response_bytes:4096
                    ~max_provisional_results:8
                  |> Or_error.ok_exn)
           in
           S.publish_initialize_and_wait
             service
             ~connection
             ~response:
               (initialize_response_with_features
                  ~enabled_features:
                    [ P.Authoring.Features.core
                    ; P.Authoring.Features.semantic
                    ; P.Authoring.Features.assistance
                    ]
                  ~server_id:host
                  ~principal)
           |> ok;
           let call request = S.handle_core service ~actor ~connection request in
           let root =
             match call P.Authoring.Wire.Request.Roots with
             | Ok (Roots roots) -> (List.hd_exn roots.roots).id
             | Ok _ | Error _ -> failwith "root list failed"
           in
           (match
              call
                (Host_list
                   (P.Authoring.Source_read.Host_list_request.create ~root ~directory:None
                    |> ok))
            with
            | Ok (Host_list result) ->
              assert (List.mem result.members "main.chatml" ~equal:String.equal)
            | Ok _ | Error _ -> failwith "fresh host list failed");
           let selection =
             P.Authoring.Context.Selection.create ~target:(Entry "main") ~environment:None
             |> ok
           in
           let open_request =
             P.Authoring.Context.Open_request.create_host_project
               ~root
               ~dependency_roots:[]
               ~project_member:"chatml.project.json"
               ~selection
             |> ok
           in
           let context =
             match call (Context_open open_request) with
             | Ok (Context_open context) -> context
             | Ok _ | Error _ -> failwith "actual project context open failed"
           in
           let expected = P.Authoring.Context.Expected.of_view context in
           let standard =
             Agent_server.Authoring_standard_contracts.create
               ~targets:[ One_off_v1 ]
               ~max_identity_bytes:(16 * 1024 * 1024)
               ~max_work_steps:1_000_000
               ~checkpoint:ignore
             |> ok
           in
           let selected =
             Agent_server.Authoring_standard_contracts.find standard One_off_v1
             |> Option.value_exn
             |> S.Developer.Selected_contract.of_standard_entry
             |> ok
           in
           let developer =
             S.Developer.create
               ~owner:service
               ~policy:
                 (S.Developer.Target_policy.create
                    ~local_project:(fun _ ~root:_ ~project:_ ~selection:_ -> None)
                    ~local_source:(fun _ ~root:_ ~member:_ -> None)
                    ~authoring_project:(fun _ ~context:_ ~current:_ ->
                      if !project_allowed then Some selected else None)
                    ~document_project:
                      (fun
                        _
                        ~context:_
                        ~current:_
                        ~parsed:_
                        ~declaration:_
                        ~build:_
                        ~metadata:_ -> None)
                    ~document_source:
                      (fun
                        _ ~context:_ ~current:_ ~parsed:_ ~declaration:_ ~script:_ ->
                      None))
               ~limits:
                 (S.Developer.Limits.create
                    ~max_prepared_input_bytes:4096
                    ~max_document_origin_bytes:8192
                    ~max_editor_versions:4
                    ~build:
                      (Chatml_project_build.Limits.create
                         ~max_work_steps:100_000
                         ~max_input_bytes:(Some 4096)
                         ~document:Document_schema.Limits.default
                       |> Or_error.ok_exn)
                  |> Or_error.ok_exn)
           in
           let shared_selection =
             S.Developer.prepare_authoring_project_selection
               developer
               ~actor
               ~connection
               ~expected
               ~checkpoint:ignore
             |> ok
           in
           let module Selection = S.Developer.Source_selection in
           let shared_a =
             Selection.Shared.check_current shared_selection ~actor ~checkpoint:ignore
             |> ok
           in
           let shared_b =
             Selection.Shared.check_current shared_selection ~actor ~checkpoint:ignore
             |> ok
           in
           Selection.Shared.check_now shared_selection ~checked:shared_a ~actor |> ok;
           Selection.Shared.check_now shared_selection ~checked:shared_b ~actor |> ok;
           let exclusive_c =
             Selection.check_current shared_selection ~actor ~checkpoint:ignore |> ok
           in
           let exclusive_d =
             Selection.check_current shared_selection ~actor ~checkpoint:ignore |> ok
           in
           assert (
             Result.is_error
               (Selection.check_now shared_selection ~checked:exclusive_c ~actor));
           Selection.check_now shared_selection ~checked:exclusive_d ~actor |> ok;
           Selection.Shared.check_now shared_selection ~checked:shared_a ~actor |> ok;
           let foreign =
             P.Principal.create
               ~id:(P.Id.Principal.of_string "pri_foreign_authoring" |> ok)
               ~authentication_kind:"test"
               ~scopes:P.Scope.Set.empty
               ~attributes:[]
             |> ok
             |> Operator_authorization.trusted_local
           in
           assert (
             Result.is_error
               (Selection.Shared.check_now
                  shared_selection
                  ~checked:shared_a
                  ~actor:foreign));
           let analyze =
             S.Semantic.create
               ~owner:service
               ~env
               ~document_dir:(Eio.Stdenv.cwd env)
               ~script_selection:None
               ~project_metadata:None
               ~project_build_limits:
                 (Chatml_project_build.Limits.create
                    ~max_work_steps:1_000_000
                    ~max_input_bytes:(Some 64_000)
                    ~document:Document_schema.Limits.default
                  |> Or_error.ok_exn)
               ~limits:
                 (S.Semantic.Limits.create
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
                 (S.Semantic.Prefix_limits.create
                    ~max_document_bundle_files:4
                    ~max_document_bundle_bytes:2048
                    ~max_parser_work_steps:100_000
                    ~max_host_work_steps:100_000
                  |> Or_error.ok_exn)
           in
           let analysis_request =
             P.Authoring.Analysis.Analyze.Request.create
               ~context:expected
               ~target:P.Authoring.Analysis.Target.project
             |> ok
             |> P.Authoring.Analysis.Analyze.Request.with_checked_template
           in
           let complete_capture, complete_snapshot, complete_graph =
             match S.Semantic.analyze analyze ~actor ~connection analysis_request with
             | Ok (P.Authoring.Analysis.Analyze.Complete result) ->
               result.capture, result.snapshot, result.validated_graph
             | Ok result ->
               failwith
                 (Sexp.to_string_hum (P.Authoring.Analysis.Analyze.sexp_of_t result))
             | Error error ->
               let error = S.Error.to_protocol error in
               failwith (P.Error.code_to_string error.code ^ ": " ^ error.message)
           in
           let inventory =
             P.Authoring.Source_read.List_request.create
               ~context:expected
               ~package_namespace:None
               ~directory:None
             |> ok
           in
           let script_source =
             match call (Source_list inventory) with
             | Ok (Source_list result) ->
               List.find_exn
                 (P.Authoring.Source_read.List_result.sources result)
                 ~f:(fun source ->
                   String.equal
                     (Chatml_source.Id.relative_path (P.Authoring.Source.id source))
                     "main.chatml")
             | Ok _ | Error _ -> failwith "project inventory unavailable"
           in
           let script_id = P.Authoring.Source.id script_source in
           let certified_query =
             Q.Request.create
               ~evidence:
                 (Q.Certified
                    { capture = complete_capture
                    ; snapshot = complete_snapshot
                    ; validated_graph = complete_graph
                    })
               ~source:script_source
               ~byte_offset:0
               ~kind:(Q.Candidates Value)
               ~max_items:1
             |> ok
           in
           (match S.Semantic.query analyze ~actor ~connection certified_query with
            | Ok result ->
              (match Q.payload result with
               | Q.Candidates_result (Q.Selected page) when not (List.is_empty page.items)
                 -> ()
               | _ -> failwith "certified query lost current value candidates")
            | Error error ->
              failwith (P.Error.code_to_string (S.Error.to_protocol error).code));
           let wide_query =
             Q.Request.create
               ~evidence:(Q.Request.evidence certified_query)
               ~source:script_source
               ~byte_offset:0
               ~kind:(Q.Candidates Value)
               ~max_items:16
             |> ok
           in
           (match S.Semantic.query analyze ~actor ~connection wide_query with
            | Error S.Error.Resource_limit -> ()
            | Ok _ | Error _ -> failwith "wide query bypassed host response cap");
           let foreign_revision =
             P.Authoring.Source.create
               ~id:(P.Authoring.Source.id script_source)
               ~digest:(Chatml_source.Digest.of_contents "foreign" ~checkpoint:ignore)
               ~byte_length:(P.Authoring.Source.byte_length script_source)
               ~editor_revision:None
             |> ok
           in
           let stale_source_query =
             Q.Request.create
               ~evidence:(Q.Request.evidence certified_query)
               ~source:foreign_revision
               ~byte_offset:0
               ~kind:(Q.Candidates Value)
               ~max_items:1
             |> ok
           in
           (match S.Semantic.query analyze ~actor ~connection stale_source_query with
            | Error S.Error.Stale_source -> ()
            | Ok _ | Error _ -> failwith "forged source revision entered query");
           let prefix_request =
             P.Authoring.Analysis.Prefix.Request.create
               ~context:expected
               ~target:P.Authoring.Analysis.Target.project
               ~source:script_source
               ~cursor_byte:(P.Authoring.Source.byte_length script_source)
             |> ok
           in
           let provisional =
             match S.Semantic.prefix analyze ~actor ~connection prefix_request with
             | Ok (P.Authoring.Analysis.Prefix.Provisional provisional) ->
               assert (
                 P.Authoring.Source.equal
                   (P.Authoring.Analysis.Prefix.full_source provisional)
                   script_source);
               provisional
             | Ok result ->
               failwith
                 (Sexp.to_string_hum (P.Authoring.Analysis.Prefix.sexp_of_t result))
             | Error error ->
               failwith (P.Error.code_to_string (S.Error.to_protocol error).code)
           in
           let provisional_query =
             Q.Request.create
               ~evidence:(Q.Provisional provisional)
               ~source:(P.Authoring.Analysis.Prefix.prefix_source provisional)
               ~byte_offset:(P.Authoring.Analysis.Prefix.end_byte provisional)
               ~kind:(Q.Candidates Value)
               ~max_items:1
             |> ok
           in
           (match S.Semantic.query analyze ~actor ~connection provisional_query with
            | Ok result ->
              (match Q.payload result with
               | Q.Candidates_result (Q.Selected page) when not (List.is_empty page.items)
                 -> ()
               | _ -> failwith "provisional query lost current value candidates")
            | Error error ->
              let error = S.Error.to_protocol error in
              failwith (P.Error.code_to_string error.code ^ ": " ^ error.message));
           [%test_eq: int]
             (S.For_testing.provisional_count
                service
                ~actor
                ~connection
                ~expected
                ~checkpoint:ignore
              |> ok)
             1;
           let assistance =
             S.Assistance.create
               ~owner:service
               ~script_selection:None
               ~project_metadata:None
               ~selected_tools:None
               ~project_build_limits:
                 (Chatml_project_build.Limits.create
                    ~max_work_steps:1_000_000
                    ~max_input_bytes:(Some 64_000)
                    ~document:Document_schema.Limits.default
                  |> Or_error.ok_exn)
               ~limits:
                 (S.Assistance.Limits.create
                    ~protocol_limits:
                      (P.Authoring.Limits.create
                         ~max_sources:8
                         ~max_source_bytes:4096
                         ~max_context_source_bytes:8192
                         ~max_request_bytes:4096
                         ~max_result_bytes:4096
                         ~max_path_candidates:16
                       |> ok)
                    ~max_scan_work_steps:100_000
                    ~max_assistance_work_steps:100_000
                    ~max_contract_identity_bytes:(16 * 1024 * 1024)
                    ~max_contract_work_steps:1_000_000
                    ~max_projection_work_steps:100_000
                    ~max_parser_input_bytes:4096
                    ~max_imported_inputs:8
                    ~max_selected_tools:16
                    ~max_tool_metadata_bytes:4096
                  |> Or_error.ok_exn)
             |> ok
           in
           let project_assistance context source =
             let assistance_request =
               P.Authoring_assistance.Assistance.Request.create
                 ~context
                 ~source
                 ~cursor_byte:4
                 ~max_items:1
               |> ok
             in
             S.Assistance.handle_assistance
               assistance
               ~actor
               ~connection
               assistance_request
           in
           let assert_checked_project context source =
             match project_assistance context source with
             | Ok (Assistance result) ->
               (match P.Authoring_assistance.Assistance.Result.items result with
                | [ item ] ->
                  (match P.Authoring_assistance.Assistance.Suggestion.evidence item with
                   | Checked_template checked ->
                     assert (
                       P.Authoring_assistance.Assistance.Checked_template.equal_target
                         (P.Authoring_assistance.Assistance.Checked_template.target
                            checked)
                         One_off_v1);
                     [%test_eq: string]
                       (P.Authoring_assistance.Assistance.Suggestion.insert_text item)
                       {|let main input = Task.pure(input)|};
                     let replace =
                       P.Authoring_assistance.Assistance.Suggestion.replace item
                     in
                     assert (
                       P.Authoring.Source.equal
                         (P.Authoring_assistance.Assistance.Range.source replace)
                         source);
                     [%test_eq: int]
                       (P.Authoring_assistance.Assistance.Range.start_byte replace)
                       0;
                     [%test_eq: int]
                       (P.Authoring_assistance.Assistance.Range.end_byte replace)
                       (P.Authoring.Source.byte_length source)
                   | _ -> failwith "project checked template lacked evidence")
                | _ -> failwith "checked project example lost the one result slot")
             | Ok _ | Error _ -> failwith "checked project assistance failed"
           in
           assert_checked_project expected script_source;
           project_allowed := false;
           assert (
             Result.is_error
               (Selection.Shared.check_now shared_selection ~checked:shared_a ~actor));
           (match project_assistance expected script_source with
            | Error S.Error.Selection_unavailable -> ()
            | Ok _ | Error _ ->
              failwith "revoked project selection served checked example");
           project_allowed := true;
           let version =
             match
               call
                 (Document_version
                    (P.Authoring.Document.Version_request.create
                       ~context:expected
                       ~source_id:script_id
                     |> ok))
             with
             | Ok (Document_version version) -> version
             | Ok _ | Error _ -> failwith "project entry version missing"
           in
           let edited =
             match
               call
                 (Document_replace
                    (P.Authoring.Document.Replace_request.create
                       ~expected:version
                       ~next_editor_revision:1
                       ~contents:"let main value = Task.pure(value)\n"
                     |> ok))
             with
             | Ok (Document_replace result) -> result
             | Ok _ | Error _ -> failwith "project entry overlay failed"
           in
           let edited_expected = P.Authoring.Context.Expected.of_view edited.context in
           let edited_source = P.Authoring.Document.Version.source edited.version in
           (match project_assistance edited_expected edited_source with
            | Ok (Assistance result) ->
              assert (
                List.for_all
                  (P.Authoring_assistance.Assistance.Result.items result)
                  ~f:(fun item ->
                    match P.Authoring_assistance.Assistance.Suggestion.evidence item with
                    | Checked_template _ -> false
                    | _ -> true))
            | Ok _ | Error _ -> failwith "edited source used old checked example");
           (match
              S.Semantic.analyze
                analyze
                ~actor
                ~connection
                (P.Authoring.Analysis.Analyze.Request.create
                   ~context:edited_expected
                   ~target:P.Authoring.Analysis.Target.project
                 |> ok
                 |> P.Authoring.Analysis.Analyze.Request.with_checked_template)
            with
            | Ok (P.Authoring.Analysis.Analyze.Complete _) -> ()
            | Ok _ | Error _ -> failwith "edited project entry was not checked");
           assert_checked_project edited_expected edited_source;
           let restored =
             match
               call
                 (Document_close
                    (P.Authoring.Document.Close_request.create ~expected:edited.version))
             with
             | Ok (Document_close result) -> result.context
             | Ok _ | Error _ -> failwith "project entry overlay close failed"
           in
           let expected = P.Authoring.Context.Expected.of_view restored in
           let pinned =
             P.Authoring.Source_read.Read_request.create
               ~context:expected
               ~source_id:script_id
             |> ok
           in
           Eio.Path.save ~create:(`Or_truncate 0o600) script_file "let main = 2\n";
           let fresh =
             P.Authoring.Source_read.Host_read_request.create ~root ~member:"main.chatml"
             |> ok
           in
           (match call (Host_read fresh) with
            | Ok (Host_read result) ->
              [%test_eq: string]
                (P.Authoring.Source_read.Host_read_result.contents result)
                "let main = 2\n"
            | Ok _ | Error _ -> failwith "fresh host read failed");
           (match call (Source_read pinned) with
            | Ok (Source_read result) ->
              [%test_eq: string]
                (P.Authoring.Source_read.Read_result.contents result)
                "let main input = Task.pure(input)\n"
            | Ok _ | Error _ -> failwith "pinned project read changed with disk");
           let refreshed =
             match
               call
                 (Context_refresh
                    (P.Authoring.Context.Refresh_request.create
                       ~expected
                       ~policy:Reject_open_documents))
             with
             | Ok (Context_refresh context) -> context
             | Ok _ | Error _ -> failwith "explicit project refresh failed"
           in
           assert (
             Result.is_error
               (Selection.Shared.check_now shared_selection ~checked:shared_a ~actor));
           [%test_eq: int]
             (S.For_testing.provisional_count
                service
                ~actor
                ~connection
                ~expected:(P.Authoring.Context.Expected.of_view refreshed)
                ~checkpoint:ignore
              |> ok)
             0;
           (match S.Semantic.query analyze ~actor ~connection provisional_query with
            | Error S.Error.Stale_context -> ()
            | Ok _ | Error _ -> failwith "retired prefix query survived refresh");
           assert (
             not
               (P.Id.Authoring_context.equal
                  (P.Authoring.Context.id refreshed)
                  (P.Authoring.Context.id context)));
           let updated_read =
             P.Authoring.Source_read.Read_request.create
               ~context:(P.Authoring.Context.Expected.of_view refreshed)
               ~source_id:script_id
             |> ok
           in
           (match call (Source_read updated_read) with
            | Ok (Source_read result) ->
              [%test_eq: string]
                (P.Authoring.Source_read.Read_result.contents result)
                "let main = 2\n"
            | Ok _ | Error _ -> failwith "refreshed bytes unavailable");
           (match call (Source_read pinned) with
            | Error S.Error.Stale_context -> ()
            | Ok _ | Error _ -> failwith "old project capture survived refresh");
           (match
              call
                (Context_close
                   (P.Authoring.Context.Close_request.create
                      ~expected:(P.Authoring.Context.Expected.of_view refreshed)))
            with
            | Ok (Context_close _) -> ()
            | Ok _ | Error _ -> failwith "context close failed");
           (match call (Source_read updated_read) with
            | Error S.Error.Stale_context -> ()
            | Ok _ | Error _ -> failwith "closed project remained readable");
           let bounded_document =
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
             | Ok _ | Error _ -> failwith "host ChatMD document open failed"
           in
           let bounded_expected = P.Authoring.Context.Expected.of_view bounded_document in
           let bounded_analyze =
             S.Semantic.create
               ~owner:service
               ~env
               ~document_dir:(Eio.Stdenv.cwd env)
               ~script_selection:None
               ~project_metadata:
                 (Some
                    (S.Semantic.Project_metadata.Policy.create
                       ~authorize_now:
                         (fun
                           ~original_actor:_
                           ~actor:_
                           ~current:_
                           ~parsed:_
                           ~declaration:_
                           ~capture:_
                         -> true)))
               ~project_build_limits:
                 (Chatml_project_build.Limits.create
                    ~max_work_steps:100_000
                    ~max_input_bytes:(Some 4096)
                    ~document:Document_schema.Limits.default
                  |> Or_error.ok_exn)
               ~limits:
                 (S.Semantic.Limits.create
                    ~max_document_bundle_files:2
                    ~max_document_bundle_bytes:(String.length authored_document)
                    ~max_parser_work_steps:100_000
                    ~max_diagnostic_work_steps:100_000
                    ~max_selection_rows:32
                    ~max_retained_selection_source_bytes:(16 * 1024 * 1024)
                    ~max_retained_selection_input_bytes:(16 * 1024 * 1024)
                    ~max_selection_work_steps:1_000_000
                  |> Or_error.ok_exn)
               ~query_limits:(query_limits ())
               ~prefix_limits:
                 (S.Semantic.Prefix_limits.create
                    ~max_document_bundle_files:4
                    ~max_document_bundle_bytes:2048
                    ~max_parser_work_steps:100_000
                    ~max_host_work_steps:100_000
                  |> Or_error.ok_exn)
           in
           let bounded_request =
             P.Authoring.Analysis.Analyze.Request.create
               ~context:bounded_expected
               ~target:(P.Authoring.Analysis.Target.selected_script "s" |> ok)
             |> ok
           in
           (match
              S.Semantic.analyze bounded_analyze ~actor ~connection bounded_request
            with
            | Error S.Error.Resource_limit -> ()
            | Ok _ | Error _ -> failwith "project metadata budget did not reject Analyze");
           (match
              Agent_server.Authoring_context_service.For_testing.capture_state
                service
                ~actor
                ~connection
                ~expected:bounded_expected
                ~checkpoint:ignore
            with
            | Ok state when not state.parsed -> ()
            | Ok _ | Error _ ->
              failwith "rejected metadata capture published document facts");
           let selected =
             S.Semantic.Script_selection.Selected.admit
               ~build:static_build
               ~contract:
                 (Chatml.Chatml_compiler_contract.for_target
                    One_off_v1
                    ~checkpoint:ignore)
               ~limits:
                 (S.Semantic.Script_selection.Limits.create
                    ~max_identity_bytes:(4 * 1024 * 1024)
                    ~max_work_steps:100_000
                  |> Or_error.ok_exn)
               ~checkpoint:ignore
             |> ok
           in
           let selected_second =
             S.Semantic.Script_selection.Selected.admit
               ~build:second_build
               ~contract:
                 (Chatml.Chatml_compiler_contract.for_target
                    One_off_v1
                    ~checkpoint:ignore)
               ~limits:
                 (S.Semantic.Script_selection.Limits.create
                    ~max_identity_bytes:(4 * 1024 * 1024)
                    ~max_work_steps:100_000
                  |> Or_error.ok_exn)
               ~checkpoint:ignore
             |> ok
           in
           let allow_selection = ref true in
           let revoke_on_final_check = ref false in
           let selection_calls = ref 0 in
           let on_query_selection = ref Fn.id in
           let selected_policy =
             S.Semantic.Script_selection.Policy.create
               ~select_script:
                 (fun
                   ~original_actor:_
                   ~actor:_
                   ~current:_
                   ~parsed:_
                   ~declaration
                   ~metadata:_
                 ->
                 !on_query_selection ();
                 incr selection_calls;
                 let script_id = Prompt.Chat_markdown.Declaration.script_id declaration in
                 if
                   !allow_selection
                   && (not (String.equal script_id "lib:u"))
                   && ((not !revoke_on_final_check) || !selection_calls = 1)
                 then
                   Some
                     (if String.equal script_id "lib:t" then selected_second else selected)
                 else None)
           in
           let selected_analyze_with_rows
                 ?(with_script_selection = true)
                 ?(with_project_metadata = true)
                 max_selection_rows
             =
             S.Semantic.create
               ~owner:service
               ~env
               ~document_dir:(Eio.Stdenv.cwd env)
               ~script_selection:
                 (if with_script_selection then Some selected_policy else None)
               ~project_metadata:
                 (if not with_project_metadata
                  then None
                  else
                    Some
                      (S.Semantic.Project_metadata.Policy.create
                         ~authorize_now:
                           (fun
                             ~original_actor:_
                             ~actor:_
                             ~current:_
                             ~parsed:_
                             ~declaration:_
                             ~capture:_
                           -> true)))
               ~project_build_limits:
                 (Chatml_project_build.Limits.create
                    ~max_work_steps:100_000
                    ~max_input_bytes:(Some 4096)
                    ~document:Document_schema.Limits.default
                  |> Or_error.ok_exn)
               ~limits:
                 (S.Semantic.Limits.create
                    ~max_document_bundle_files:4
                    ~max_document_bundle_bytes:2048
                    ~max_parser_work_steps:100_000
                    ~max_diagnostic_work_steps:100_000
                    ~max_selection_rows
                    ~max_retained_selection_source_bytes:(16 * 1024 * 1024)
                    ~max_retained_selection_input_bytes:(16 * 1024 * 1024)
                    ~max_selection_work_steps:1_000_000
                  |> Or_error.ok_exn)
               ~query_limits:(query_limits ())
               ~prefix_limits:
                 (S.Semantic.Prefix_limits.create
                    ~max_document_bundle_files:4
                    ~max_document_bundle_bytes:2048
                    ~max_parser_work_steps:100_000
                    ~max_host_work_steps:100_000
                  |> Or_error.ok_exn)
           in
           let selected_analyze = selected_analyze_with_rows 32 in
           let selected_capture, selected_snapshot, selected_graph =
             match
               S.Semantic.analyze selected_analyze ~actor ~connection bounded_request
             with
             | Ok (P.Authoring.Analysis.Analyze.Complete result) ->
               result.capture, result.snapshot, result.validated_graph
             | Ok result ->
               failwith
                 (Sexp.to_string_hum (P.Authoring.Analysis.Analyze.sexp_of_t result))
             | Error error ->
               failwith (P.Error.code_to_string (S.Error.to_protocol error).code)
           in
           let selected_source =
             match P.Authoring.Analysis.Capture.target selected_capture with
             | P.Authoring.Analysis.Target.Script_input stamp ->
               P.Authoring.Analysis.Target.selected_script_source stamp
             | Project_input -> failwith "selected document lost script target"
           in
           let selected_query =
             Q.Request.create
               ~evidence:
                 (Q.Certified
                    { capture = selected_capture
                    ; snapshot = selected_snapshot
                    ; validated_graph = selected_graph
                    })
               ~source:selected_source
               ~byte_offset:0
               ~kind:(Q.Candidates Value)
               ~max_items:1
             |> ok
           in
           (match S.Semantic.query selected_analyze ~actor ~connection selected_query with
            | Ok result ->
              (match Q.payload result with
               | Q.Candidates_result (Q.Selected page) when not (List.is_empty page.items)
                 -> ()
               | _ -> failwith "selected document query lost value candidates")
            | Error error ->
              failwith (P.Error.code_to_string (S.Error.to_protocol error).code));
           (match
              S.Semantic.query
                (selected_analyze_with_rows ~with_project_metadata:false 32)
                ~actor
                ~connection
                selected_query
            with
            | Error S.Error.Selection_unavailable -> ()
            | Ok _ | Error _ ->
              failwith "retained project metadata lost its admission policy");
           (match
              S.Semantic.query
                (selected_analyze_with_rows ~with_script_selection:false 32)
                ~actor
                ~connection
                selected_query
            with
            | Error S.Error.Selection_unavailable -> ()
            | Ok _ | Error _ -> failwith "retained Build lost its selection policy");
           allow_selection := false;
           (match S.Semantic.query selected_analyze ~actor ~connection selected_query with
            | Error S.Error.Selection_unavailable -> ()
            | Ok _ | Error _ -> failwith "query ignored revoked script selection");
           allow_selection := true;
           selection_calls := 0;
           revoke_on_final_check := true;
           (match S.Semantic.query selected_analyze ~actor ~connection selected_query with
            | Error S.Error.Selection_unavailable -> ()
            | Ok _ | Error _ -> failwith "query ignored final selector recheck");
           revoke_on_final_check := false;
           let first_selection, wake_selection = Eio.Promise.create () in
           let signalled = ref false in
           (on_query_selection
            := fun () ->
                 if not !signalled
                 then (
                   signalled := true;
                   Eio.Promise.resolve wake_selection ()));
           let concurrent_query = ref None in
           Eio.Fiber.both
             (fun () ->
                concurrent_query
                := Some
                     (S.Semantic.query selected_analyze ~actor ~connection selected_query))
             (fun () ->
                Eio.Promise.await first_selection;
                allow_selection := false);
           on_query_selection := Fn.id;
           (match Option.value_exn !concurrent_query with
            | Error S.Error.Selection_unavailable -> ()
            | Ok _ | Error _ ->
              failwith "query served after concurrent selector revocation");
           allow_selection := true;
           let document_source =
             match P.Authoring.Context.basis bounded_document with
             | Document { source; _ } -> source
             | Project _ -> failwith "document source missing"
           in
           let authored_cursor =
             String.substr_index_exn authored_document ~pattern:"Task.pure(input)"
             + String.length "Task.pure(input)"
           in
           let document_prefix_request =
             P.Authoring.Analysis.Prefix.Request.create
               ~context:bounded_expected
               ~target:(P.Authoring.Analysis.Target.selected_script "s" |> ok)
               ~source:document_source
               ~cursor_byte:authored_cursor
             |> ok
           in
           (match
              S.Semantic.prefix
                selected_analyze
                ~actor
                ~connection
                document_prefix_request
            with
            | Ok (P.Authoring.Analysis.Prefix.Provisional provisional) ->
              let authored =
                P.Authoring.Analysis.Prefix.authored_request provisional
                |> Option.value_exn
              in
              [%test_eq: int]
                (P.Authoring.Analysis.Prefix.Authored_request.cursor_byte authored)
                authored_cursor;
              assert (
                P.Authoring.Source.equal
                  (P.Authoring.Analysis.Prefix.Authored_request.source authored)
                  document_source);
              assert (
                not
                  (Chatml_source.Id.equal
                     (P.Authoring.Source.id
                        (P.Authoring.Analysis.Prefix.full_source provisional))
                     (P.Authoring.Source.id document_source)))
            | Ok result ->
              failwith (Sexp.to_string_hum (P.Authoring.Analysis.Prefix.sexp_of_t result))
            | Error error ->
              failwith (P.Error.code_to_string (S.Error.to_protocol error).code));
           Eio.Path.save
             ~create:(`Or_truncate 0o600)
             Eio.Path.(directory / "chatml.project.json")
             {|{"schema_version":1,"namespace":"authoring","entries":{"main":{"source":"main.chatml","target":"one_off_v1"}},"capabilities":["fresh.read"]}|};
           (match
              S.Semantic.analyze selected_analyze ~actor ~connection bounded_request
            with
            | Ok
                (P.Authoring.Analysis.Analyze.Unavailable
                   { reason = No_validated_project; _ }) -> ()
            | Ok _ | Error _ ->
              failwith "stale selected Build certified fresh project declaration");
           (match
              Agent_server.Authoring_context_service.For_testing.capture_state
                service
                ~actor
                ~connection
                ~expected:bounded_expected
                ~checkpoint:ignore
            with
            | Ok state when state.parsed && state.project_metadata -> ()
            | Ok _ | Error _ -> failwith "parsed-only metadata capture did not publish");
           (match
              call
                (Context_close
                   (P.Authoring.Context.Close_request.create ~expected:bounded_expected))
            with
            | Ok (Context_close _) -> ()
            | Ok _ | Error _ -> failwith "host ChatMD document close failed");
           Eio.Path.save
             ~create:(`Or_truncate 0o600)
             Eio.Path.(directory / "chatml.project.json")
             {|{"schema_version":1,"namespace":"authoring","entries":{"main":{"source":"main.chatml","target":"one_off_v1"}}}|};
           let editable =
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
             | Ok _ | Error _ -> failwith "editable ChatMD open failed"
           in
           let editable_expected = P.Authoring.Context.Expected.of_view editable in
           let selected_target = P.Authoring.Analysis.Target.selected_script "s" |> ok in
           let baseline_capture =
             match
               S.Semantic.analyze
                 selected_analyze
                 ~actor
                 ~connection
                 (P.Authoring.Analysis.Analyze.Request.create
                    ~context:editable_expected
                    ~target:selected_target
                  |> ok)
             with
             | Ok (P.Authoring.Analysis.Analyze.Complete { capture; _ }) -> capture
             | Ok _ | Error _ -> failwith "editable ChatMD baseline was not checked"
           in
           let editable_source =
             match P.Authoring.Context.basis editable with
             | Document { source; _ } -> source
             | Project _ -> failwith "editable ChatMD source missing"
           in
           let version =
             match
               call
                 (Document_version
                    (P.Authoring.Document.Version_request.create
                       ~context:editable_expected
                       ~source_id:(P.Authoring.Source.id editable_source)
                     |> ok))
             with
             | Ok (Document_version version) -> version
             | Ok _ | Error _ -> failwith "editable ChatMD version missing"
           in
           let valid_script_body = "let x = 1;; let main input = Task.pure(input)" in
           let edited_body =
             String.substr_replace_all
               authored_document
               ~pattern:"let main input = Task.pure(input)"
               ~with_:valid_script_body
           in
           let edited =
             match
               call
                 (Document_replace
                    (P.Authoring.Document.Replace_request.create
                       ~expected:version
                       ~next_editor_revision:1
                       ~contents:edited_body
                     |> ok))
             with
             | Ok (Document_replace result) -> result
             | Ok _ | Error _ -> failwith "editable ChatMD replacement failed"
           in
           let edited_context = edited.P.Authoring.Document.Result.context in
           let edited_source =
             match P.Authoring.Context.basis edited_context with
             | Document { source; _ } -> source
             | Project _ -> failwith "edited ChatMD source missing"
           in
           assert (not (P.Authoring.Source.equal editable_source edited_source));
           (match
              S.Semantic.analyze
                selected_analyze
                ~actor
                ~connection
                (P.Authoring.Analysis.Analyze.Request.create
                   ~context:(P.Authoring.Context.Expected.of_view edited_context)
                   ~target:selected_target
                 |> ok)
            with
            | Ok
                (P.Authoring.Analysis.Analyze.Complete
                   { capture; snapshot; validated_graph; _ }) ->
              assert (
                not
                  (Chatml_source.Digest.equal
                     (P.Authoring.Analysis.Capture.input_fingerprint capture)
                     (P.Authoring.Analysis.Capture.input_fingerprint baseline_capture)));
              let compiled_source =
                match P.Authoring.Analysis.Capture.target capture with
                | P.Authoring.Analysis.Target.Script_input stamp ->
                  P.Authoring.Analysis.Target.selected_script_source stamp
                | Project_input -> failwith "edited document lost selected source"
              in
              let offset =
                String.substr_index_exn valid_script_body ~pattern:"Task.pure(input)"
                + String.length "Task.pure("
              in
              let request =
                Q.Request.create
                  ~evidence:(Q.Certified { capture; snapshot; validated_graph })
                  ~source:compiled_source
                  ~byte_offset:offset
                  ~kind:Q.References
                  ~max_items:1
                |> ok
              in
              (match S.Semantic.query selected_analyze ~actor ~connection request with
               | Ok answer ->
                 (match Q.payload answer with
                  | Q.References_result
                      (Q.Available { items = [ reference ]; truncated = false }) ->
                    let site = Q.Item.reference_selection reference in
                    let expected_authored_offset =
                      String.substr_index_exn edited_body ~pattern:"Task.pure(input)"
                      + String.length "Task.pure("
                    in
                    let native_range = P.Authoring.Analysis.Site.native site in
                    assert (
                      P.Authoring.Source.equal
                        (P.Authoring.Analysis.Byte_range.source native_range)
                        compiled_source);
                    [%test_eq: int]
                      (P.Authoring.Analysis.Byte_range.start_byte native_range)
                      offset;
                    [%test_eq: int]
                      (P.Authoring.Analysis.Byte_range.end_byte native_range)
                      (offset + String.length "input");
                    (match P.Authoring.Analysis.Site.projection site with
                     | Some (Authored_exact range) ->
                       assert (
                         P.Authoring.Source.equal
                           (P.Authoring.Analysis.Byte_range.source range)
                           edited_source);
                       [%test_eq: int]
                         (P.Authoring.Analysis.Byte_range.start_byte range)
                         expected_authored_offset;
                       [%test_eq: int]
                         (P.Authoring.Analysis.Byte_range.end_byte range)
                         (expected_authored_offset + String.length "input")
                     | None | Some (External_origin _ | Approximate_declaration _) ->
                       failwith "edited query lost exact authored selection")
                  | _ -> failwith "edited query lost selected reference")
               | Error error ->
                 failwith (P.Error.code_to_string (S.Error.to_protocol error).code))
            | Ok _ | Error _ -> failwith "edited selected body was not checked");
           let incomplete_script_body = "let x = 1;; let main input = Task.pure(inpu" in
           let incomplete_body =
             String.substr_replace_all
               edited_body
               ~pattern:valid_script_body
               ~with_:incomplete_script_body
           in
           let incomplete =
             match
               call
                 (Document_replace
                    (P.Authoring.Document.Replace_request.create
                       ~expected:edited.P.Authoring.Document.Result.version
                       ~next_editor_revision:2
                       ~contents:incomplete_body
                     |> ok))
             with
             | Ok (Document_replace result) -> result
             | Ok _ | Error _ -> failwith "incomplete ChatMD replacement failed"
           in
           let incomplete_context = incomplete.P.Authoring.Document.Result.context in
           let incomplete_source =
             match P.Authoring.Context.basis incomplete_context with
             | Document { source; _ } -> source
             | Project _ -> failwith "incomplete ChatMD source missing"
           in
           let edited_cursor =
             String.substr_index_exn incomplete_body ~pattern:"Task.pure(inpu"
             + String.length "Task.pure(inpu"
           in
           (match
              S.Semantic.prefix
                selected_analyze
                ~actor
                ~connection
                (P.Authoring.Analysis.Prefix.Request.create
                   ~context:(P.Authoring.Context.Expected.of_view incomplete_context)
                   ~target:selected_target
                   ~source:incomplete_source
                   ~cursor_byte:edited_cursor
                 |> ok)
            with
            | Ok (P.Authoring.Analysis.Prefix.Provisional provisional) ->
              let full_source = P.Authoring.Analysis.Prefix.full_source provisional in
              let exact_compiled_body = incomplete_script_body ^ "\n" in
              [%test_eq: int]
                (P.Authoring.Source.byte_length full_source)
                (String.length exact_compiled_body);
              assert (
                Chatml_source.Digest.equal
                  (P.Authoring.Source.digest full_source)
                  (Chatml_source.Digest.of_contents
                     exact_compiled_body
                     ~checkpoint:ignore));
              let authored =
                P.Authoring.Analysis.Prefix.authored_request provisional
                |> Option.value_exn
              in
              assert (
                P.Authoring.Source.equal
                  (P.Authoring.Analysis.Prefix.Authored_request.source authored)
                  incomplete_source);
              [%test_eq: int]
                (P.Authoring.Analysis.Prefix.Authored_request.cursor_byte authored)
                edited_cursor;
              let query =
                Q.Request.create
                  ~evidence:(Q.Provisional provisional)
                  ~source:(P.Authoring.Analysis.Prefix.prefix_source provisional)
                  ~byte_offset:8
                  ~kind:Q.Type_at
                  ~max_items:1
                |> ok
              in
              (match S.Semantic.query selected_analyze ~actor ~connection query with
               | Ok result ->
                 (match Q.payload result with
                  | Q.Type_result (Q.Selected display)
                    when String.equal (Q.Display.text display) "int" -> ()
                  | _ -> failwith "edited prefix query lost completed x type")
               | Error error ->
                 failwith (P.Error.code_to_string (S.Error.to_protocol error).code))
            | Ok (P.Authoring.Analysis.Prefix.Unavailable { reason; _ }) ->
              failwith
                (Sexp.to_string_hum
                   (P.Authoring.Analysis.Prefix.sexp_of_unavailable reason))
            | Error error ->
              failwith (P.Error.code_to_string (S.Error.to_protocol error).code));
           let changed_surface_body =
             String.substr_replace_all
               incomplete_body
               ~pattern:"kind=\"tool\""
               ~with_:"kind=\"moderator\""
           in
           let changed_surface =
             match
               call
                 (Document_replace
                    (P.Authoring.Document.Replace_request.create
                       ~expected:incomplete.P.Authoring.Document.Result.version
                       ~next_editor_revision:3
                       ~contents:changed_surface_body
                     |> ok))
             with
             | Ok (Document_replace result) -> result
             | Ok _ | Error _ -> failwith "surface-only ChatMD replacement failed"
           in
           let changed_surface_context =
             changed_surface.P.Authoring.Document.Result.context
           in
           let changed_surface_source =
             match P.Authoring.Context.basis changed_surface_context with
             | Document { source; _ } -> source
             | Project _ -> failwith "changed-surface source missing"
           in
           (match
              S.Semantic.prefix
                selected_analyze
                ~actor
                ~connection
                (P.Authoring.Analysis.Prefix.Request.create
                   ~context:(P.Authoring.Context.Expected.of_view changed_surface_context)
                   ~target:selected_target
                   ~source:changed_surface_source
                   ~cursor_byte:edited_cursor
                 |> ok)
            with
            | Error S.Error.Selection_unavailable -> ()
            | Ok _ | Error _ ->
              failwith "edited declaration kind reused an old selected Build");
           (match
              call
                (Context_close
                   (P.Authoring.Context.Close_request.create
                      ~expected:
                        (P.Authoring.Context.Expected.of_view changed_surface_context)))
            with
            | Ok (Context_close _) -> ()
            | Ok _ | Error _ -> failwith "edited ChatMD close failed");
           let imported_part =
             "<script id=\"s\" language=\"chatml\" kind=\"tool\" \
              project=\"./chatml.project.json\">let main input = Task.pure(input)\n\
              </script>\n\
              <script id=\"t\" language=\"chatml\" kind=\"tool\" \
              project=\"./second.project.json\" environment=\"other\">let main input = \
              Task.pure(input)\n\n\
              </script>\n\
              <script id=\"u\" language=\"chatml\" kind=\"tool\" \
              project=\"./missing.json\">let main input = Task.pure(input)\n\
              </script>"
           in
           Eio.Path.save
             ~create:(`Or_truncate 0o600)
             Eio.Path.(directory / "fragment.chatmd")
             imported_part;
           Eio.Path.save
             ~create:(`Or_truncate 0o600)
             Eio.Path.(directory / "imported.chatmd")
             {|<import src="fragment.chatmd" namespace="lib"/>|};
           let imported_context =
             match
               call
                 (Context_open
                    (P.Authoring.Context.Open_request.create_host_document
                       ~root
                       ~member:"imported.chatmd"
                       ~default_environment:None
                     |> ok))
             with
             | Ok (Context_open context) -> context
             | Ok _ | Error _ -> failwith "imported ChatMD open failed"
           in
           let imported_expected =
             P.Authoring.Context.Expected.of_view imported_context
           in
           let imported_request =
             P.Authoring.Analysis.Analyze.Request.create
               ~context:imported_expected
               ~target:(P.Authoring.Analysis.Target.selected_script "lib:s" |> ok)
             |> ok
           in
           let calls_before_cap = !selection_calls in
           (match
              S.Semantic.analyze
                (selected_analyze_with_rows 1)
                ~actor
                ~connection
                imported_request
            with
            | Error S.Error.Resource_limit -> ()
            | Ok _ | Error _ -> failwith "incomplete row inventory escaped cap");
           [%test_eq: int] !selection_calls calls_before_cap;
           (match
              S.For_testing.capture_state
                service
                ~actor
                ~connection
                ~expected:imported_expected
                ~checkpoint:ignore
            with
            | Ok state -> [%test_eq: int] state.selection_rows 0
            | Error _ -> failwith "rejected row inventory left a capture");
           let imported_capture, imported_snapshot, imported_graph =
             match
               S.Semantic.analyze selected_analyze ~actor ~connection imported_request
             with
             | Ok (P.Authoring.Analysis.Analyze.Complete complete) ->
               complete.capture, complete.snapshot, complete.validated_graph
             | Ok result ->
               failwith
                 (Sexp.to_string_hum (P.Authoring.Analysis.Analyze.sexp_of_t result))
             | Error error ->
               failwith (P.Error.code_to_string (S.Error.to_protocol error).code)
           in
           let imported_compiled_source =
             match P.Authoring.Analysis.Capture.target imported_capture with
             | P.Authoring.Analysis.Target.Script_input stamp ->
               P.Authoring.Analysis.Target.selected_script_source stamp
             | Project_input -> failwith "imported capture lost script target"
           in
           (match
              S.For_testing.capture_state
                service
                ~actor
                ~connection
                ~expected:imported_expected
                ~checkpoint:ignore
            with
            | Ok state -> [%test_eq: int] state.selection_rows 3
            | Error _ -> failwith "imported declaration rows unavailable");
           let imported_offset =
             String.substr_index_exn imported_part ~pattern:"Task.pure(input)"
             + String.length "Task.pure("
           in
           let imported_query =
             Q.Request.create
               ~evidence:
                 (Q.Certified
                    { capture = imported_capture
                    ; snapshot = imported_snapshot
                    ; validated_graph = imported_graph
                    })
               ~source:imported_compiled_source
               ~byte_offset:(String.length "let main input = Task.pure(")
               ~kind:Q.References
               ~max_items:1
             |> ok
           in
           (match S.Semantic.query selected_analyze ~actor ~connection imported_query with
            | Ok answer ->
              (match Q.payload answer with
               | Q.References_result
                   (Q.Available { items = [ reference ]; truncated = false }) ->
                 let site = Q.Item.reference_selection reference in
                 (match P.Authoring.Analysis.Site.projection site with
                  | Some (Authored_exact range) ->
                    [%test_eq: string]
                      (Chatml_source.Id.relative_path
                         (P.Authoring.Source.id
                            (P.Authoring.Analysis.Byte_range.source range)))
                      "fragment.chatmd";
                    [%test_eq: int]
                      (P.Authoring.Analysis.Byte_range.start_byte range)
                      imported_offset;
                    [%test_eq: int]
                      (P.Authoring.Analysis.Byte_range.end_byte range)
                      (imported_offset + String.length "input")
                  | None | Some (External_origin _ | Approximate_declaration _) ->
                    failwith "imported query lost parser-owned authored input")
               | _ -> failwith "imported query lost actual reference")
            | Error error ->
              failwith (P.Error.code_to_string (S.Error.to_protocol error).code));
           let second_body = "let main input = Task.pure(input)\n\n" in
           let imported_root_source =
             match P.Authoring.Context.basis imported_context with
             | Document { source; _ } -> source
             | Project _ -> failwith "imported document lost source basis"
           in
           let imported_fragment_source =
             P.Authoring.Source.create
               ~id:
                 (Chatml_source.Id.create
                    ~package_namespace:
                      (Chatml_source.Id.package_namespace
                         (P.Authoring.Source.id imported_root_source))
                    ~relative_path:"fragment.chatmd"
                  |> Or_error.ok_exn)
               ~digest:(Chatml_source.Digest.of_contents imported_part ~checkpoint:ignore)
               ~byte_length:(String.length imported_part)
               ~editor_revision:None
             |> ok
           in
           let second_cursor =
             String.substr_index_exn
               imported_part
               ~pattern:"environment=\"other\">let main input = Task.pure(input)"
             + String.length "environment=\"other\">let main input = Task.pure(input)"
           in
           Eio.Path.unlink Eio.Path.(directory / "second.project.json");
           (match
              S.Semantic.prefix
                selected_analyze
                ~actor
                ~connection
                (P.Authoring.Analysis.Prefix.Request.create
                   ~context:imported_expected
                   ~target:(P.Authoring.Analysis.Target.selected_script "lib:t" |> ok)
                   ~source:imported_fragment_source
                   ~cursor_byte:second_cursor
                 |> ok)
            with
            | Ok (P.Authoring.Analysis.Prefix.Provisional provisional) ->
              assert (
                Chatml_source.Digest.equal
                  (P.Authoring.Source.digest
                     (P.Authoring.Analysis.Prefix.full_source provisional))
                  (Chatml_source.Digest.of_contents second_body ~checkpoint:ignore));
              let authored =
                P.Authoring.Analysis.Prefix.authored_request provisional
                |> Option.value_exn
              in
              assert (
                P.Authoring.Source.equal
                  (P.Authoring.Analysis.Prefix.Authored_request.source authored)
                  imported_fragment_source)
            | Ok result ->
              failwith (Sexp.to_string_hum (P.Authoring.Analysis.Prefix.sexp_of_t result))
            | Error error ->
              failwith (P.Error.code_to_string (S.Error.to_protocol error).code));
           Eio.Path.save
             ~create:(`Or_truncate 0o600)
             Eio.Path.(directory / "second.project.json")
             second_project;
           let second_request =
             P.Authoring.Analysis.Analyze.Request.create
               ~context:imported_expected
               ~target:(P.Authoring.Analysis.Target.selected_script "lib:t" |> ok)
             |> ok
           in
           let second_capture, second_snapshot, second_graph =
             match
               S.Semantic.analyze selected_analyze ~actor ~connection second_request
             with
             | Ok (P.Authoring.Analysis.Analyze.Complete complete) ->
               complete.capture, complete.snapshot, complete.validated_graph
             | Ok (P.Authoring.Analysis.Analyze.Unavailable { reason; _ }) ->
               failwith
                 (Sexp.to_string_hum
                    (P.Authoring.Analysis.Analyze.sexp_of_unavailable reason))
             | Ok (P.Authoring.Analysis.Analyze.Failed { phase; diagnostics; _ }) ->
               failwith
                 (Sexp.to_string_hum
                    [%sexp
                      (phase : P.Authoring.Analysis.Analyze.failure_phase)
                    , (List.map diagnostics ~f:P.Authoring.Analysis.Diagnostic.message
                       : string list)])
             | Error error ->
               failwith (P.Error.code_to_string (S.Error.to_protocol error).code)
           in
           let second_source =
             match P.Authoring.Analysis.Capture.target second_capture with
             | P.Authoring.Analysis.Target.Script_input stamp ->
               P.Authoring.Analysis.Target.selected_script_source stamp
             | Project_input -> failwith "second script capture lost its target"
           in
           [%test_eq: string]
             (Chatml_source.Id.relative_path (P.Authoring.Source.id second_source))
             "second.chatml";
           assert (
             Chatml_source.Digest.equal
               (P.Authoring.Source.digest second_source)
               (Chatml_source.Digest.of_contents second_body ~checkpoint:ignore));
           assert (not (P.Authoring.Source.equal imported_compiled_source second_source));
           (match
              S.Semantic.query
                selected_analyze
                ~actor
                ~connection
                (Q.Request.create
                   ~evidence:
                     (Q.Certified
                        { capture = second_capture
                        ; snapshot = second_snapshot
                        ; validated_graph = second_graph
                        })
                   ~source:second_source
                   ~byte_offset:
                     (String.substr_index_exn second_body ~pattern:"Task.pure(input)"
                      + String.length "Task.pure(")
                   ~kind:Q.References
                   ~max_items:1
                 |> ok)
            with
            | Ok answer ->
              (match Q.payload answer with
               | Q.References_result
                   (Q.Available { items = [ reference ]; truncated = false }) ->
                 (match
                    P.Authoring.Analysis.Site.projection
                      (Q.Item.reference_selection reference)
                  with
                  | Some (Authored_exact range) ->
                    [%test_eq: int]
                      (P.Authoring.Analysis.Byte_range.start_byte range)
                      (String.substr_index_exn
                         imported_part
                         ~pattern:
                           "environment=\"other\">let main input = Task.pure(input)"
                       + String.length "environment=\"other\">let main input = Task.pure("
                      )
                  | None | Some (External_origin _ | Approximate_declaration _) ->
                    failwith "second selected project lost imported authored provenance")
               | _ -> failwith "second selected environment lost its checked reference")
            | Error _ -> failwith "second selected environment query failed");
           (match
              S.Semantic.analyze
                selected_analyze
                ~actor
                ~connection
                (P.Authoring.Analysis.Analyze.Request.create
                   ~context:imported_expected
                   ~target:(P.Authoring.Analysis.Target.selected_script "lib:u" |> ok)
                 |> ok)
            with
            | Ok
                (P.Authoring.Analysis.Analyze.Unavailable
                   { reason = No_validated_project; _ }) -> ()
            | Ok _ | Error _ ->
              failwith "unavailable third project affected valid selected rows");
           (match
              call
                (Context_close
                   (P.Authoring.Context.Close_request.create ~expected:imported_expected))
            with
            | Ok (Context_close _) -> ()
            | Ok _ | Error _ -> failwith "imported ChatMD close failed");
           let document =
             match
               call
                 (Context_open
                    (P.Authoring.Context.Open_request.create_host_document
                       ~root
                       ~member:"main.chatml"
                       ~default_environment:None
                     |> ok))
             with
             | Ok (Context_open context) -> context
             | Ok _ | Error _ -> failwith "host document static open failed"
           in
           let selected =
             match P.Authoring.Context.basis document with
             | Document { selection = Some selected; _ } -> selected
             | Document { selection = None; _ } | Project _ ->
               failwith "real static selection omitted at open"
           in
           let refreshed_document =
             match
               call
                 (Context_refresh
                    (P.Authoring.Context.Refresh_request.create
                       ~expected:(P.Authoring.Context.Expected.of_view document)
                       ~policy:Reject_open_documents))
             with
             | Ok (Context_refresh context) -> context
             | Ok _ | Error _ -> failwith "host document refresh failed"
           in
           (match P.Authoring.Context.basis refreshed_document with
            | Document { selection = Some after; _ } ->
              [%test_eq: string] after.environment selected.environment;
              assert (
                Chatml_source.Digest.equal
                  after.compiler_contract
                  selected.compiler_contract)
            | Document { selection = None; _ } | Project _ ->
              failwith "refresh lost the real static selection");
           let admitted, signal = Eio.Promise.create () in
           let finished, finish = Eio.Promise.create () in
           let signalled = ref false in
           (on_static
            := fun () ->
                 if not !signalled
                 then (
                   signalled := true;
                   Eio.Promise.resolve signal ()));
           Eio.Fiber.fork ~sw (fun () ->
             let result =
               call
                 (Context_refresh
                    (P.Authoring.Context.Refresh_request.create
                       ~expected:(P.Authoring.Context.Expected.of_view refreshed_document)
                       ~policy:Reject_open_documents))
             in
             Eio.Cancel.protect (fun () -> Eio.Promise.resolve finish result));
           Eio.Promise.await admitted;
           on_static := Fn.id;
           assert (Option.is_none (Eio.Promise.peek finished));
           static_allowed := false;
           (match Eio.Promise.await finished with
            | Error S.Error.Selection_unavailable -> ()
            | Ok _ | Error _ -> failwith "refresh silently dropped static selection");
           static_allowed := true;
           let source_id =
             match P.Authoring.Context.basis refreshed_document with
             | Document { source; _ } -> P.Authoring.Source.id source
             | Project _ -> failwith "document changed basis"
           in
           match
             call
               (Source_read
                  (P.Authoring.Source_read.Read_request.create
                     ~context:(P.Authoring.Context.Expected.of_view refreshed_document)
                     ~source_id
                   |> ok))
           with
           | Ok (Source_read _) -> ()
           | Ok _ | Error _ -> failwith "failed refresh displaced selected document")))
;;

let%test_unit "joined revocation and reinitialize cannot publish an in-flight context" =
  with_supplied_service
    (fun sw service host connection actor principal allowed on_authorize ->
       let request =
         P.Authoring.Context.Open_request.create_supplied_document
           ~member:"draft.chatmd"
           ~default_environment:None
           ~contents:(String.make 900 'x')
         |> ok
       in
       let start_open () =
         let admitted, signal = Eio.Promise.create () in
         let finished, finish = Eio.Promise.create () in
         let signalled = ref false in
         (on_authorize
          := fun () ->
               if not !signalled
               then (
                 signalled := true;
                 Eio.Promise.resolve signal ()));
         Eio.Fiber.fork ~sw (fun () ->
           let result =
             try
               `Returned
                 (S.handle_core
                    service
                    ~actor
                    ~connection
                    (P.Authoring.Wire.Request.Context_open request))
             with
             | exn -> `Raised exn
           in
           Eio.Cancel.protect (fun () -> Eio.Promise.resolve finish result));
         Eio.Promise.await admitted;
         on_authorize := Fn.id;
         assert (Option.is_none (Eio.Promise.peek finished));
         finished
       in
       let revoked = start_open () in
       allowed := false;
       (match Eio.Promise.await revoked with
        | `Returned (Error S.Error.Unauthorized) -> ()
        | `Returned _ | `Raised _ -> failwith "revoked candidate was published");
       allowed := true;
       let old_lease =
         Agent_server.Connection_context.authoring_negotiation connection
         |> Option.value_exn
       in
       let interrupted = start_open () in
       S.publish_initialize_and_wait
         service
         ~connection
         ~response:(initialize_response ~server_id:host ~principal)
       |> ok;
       (match Eio.Promise.await interrupted with
        | `Returned (Ok _) -> failwith "old lease published after reinitialize"
        | `Returned (Error _) | `Raised _ -> ());
       assert (
         not
           (Agent_server.Connection_context.current_authoring_negotiation
              connection
              old_lease));
       let foreign =
         Agent_server.Connection_context.create_authenticated
           ~actor
           ~connection_id:"foreign-lease-test"
           ~transport:In_memory
           ~publish_notification:(fun _ -> ())
           ~max_attachments:1
       in
       Agent_server.Connection_context.mark_initialized
         ~enabled_features:[ P.Authoring.Features.core ]
         foreign;
       assert (
         not
           (Agent_server.Connection_context.current_authoring_negotiation
              connection
              (Agent_server.Connection_context.authoring_negotiation foreign
               |> Option.value_exn)));
       for _ = 1 to 2 do
         match
           S.handle_core
             service
             ~actor
             ~connection
             (P.Authoring.Wire.Request.Context_open request)
         with
         | Ok (Context_open _) -> ()
         | Ok _ | Error _ -> failwith "aborted context consumed shared capacity"
       done;
       let closing = start_open () in
       S.close_connection_and_wait service connection |> ok;
       assert (Option.is_some (Eio.Promise.peek closing));
       (match Eio.Promise.await closing with
        | `Returned (Ok _) -> failwith "closed connection published pending request"
        | `Returned (Error _) | `Raised _ -> ());
       assert (
         not
           (Agent_server.Connection_context.try_mark_initialized
              ~enabled_features:[ P.Authoring.Features.core ]
              connection)))
;;
