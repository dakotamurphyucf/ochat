open! Core
module P = Agent_protocol
module S = Agent_server.Authoring_context_service
module C = Chatml_authoring_context

let () = Mirage_crypto_rng_unix.use_default ()

let ok = function
  | Ok value -> value
  | Error _ -> failwith "unexpected bounded setup/result failure"
;;

let analysis_policy () =
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
    ~compilation:Chatml_compilation.default_limits
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
                    ~max_context_requests:12
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
           let static_allowed = ref true in
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
                      Some
                        (Chatml.Chatml_compiler_contract.for_target
                           One_off_v1
                           ~checkpoint:Fn.id))
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
                    [ P.Authoring.Features.core; P.Authoring.Features.semantic ]
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
           let analyze =
             S.Semantic.create
               ~owner:service
               ~env
               ~document_dir:(Eio.Stdenv.cwd env)
               ~script_selection:None
               ~project_metadata:None
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
                  |> Or_error.ok_exn)
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
           in
           (match S.Semantic.analyze analyze ~actor ~connection analysis_request with
            | Ok (P.Authoring.Analysis.Analyze.Complete _) -> ()
            | Ok result ->
              failwith
                (Sexp.to_string_hum (P.Authoring.Analysis.Analyze.sexp_of_t result))
            | Error error ->
              failwith (P.Error.code_to_string (S.Error.to_protocol error).code));
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
           let prefix_request =
             P.Authoring.Analysis.Prefix.Request.create
               ~context:expected
               ~target:P.Authoring.Analysis.Target.project
               ~source:script_source
               ~cursor_byte:(P.Authoring.Source.byte_length script_source)
             |> ok
           in
           (match S.Semantic.prefix analyze ~actor ~connection prefix_request with
            | Ok (P.Authoring.Analysis.Prefix.Provisional provisional) ->
              assert (
                P.Authoring.Source.equal
                  (P.Authoring.Analysis.Prefix.full_source provisional)
                  script_source)
            | Ok result ->
              failwith (Sexp.to_string_hum (P.Authoring.Analysis.Prefix.sexp_of_t result))
            | Error error ->
              failwith (P.Error.code_to_string (S.Error.to_protocol error).code));
           [%test_eq: int]
             (S.For_testing.provisional_count
                service
                ~actor
                ~connection
                ~expected
                ~checkpoint:ignore
              |> ok)
             1;
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
           [%test_eq: int]
             (S.For_testing.provisional_count
                service
                ~actor
                ~connection
                ~expected:(P.Authoring.Context.Expected.of_view refreshed)
                ~checkpoint:ignore
              |> ok)
             0;
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
                  |> Or_error.ok_exn)
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
           let selected_policy =
             S.Semantic.Script_selection.Policy.create
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
           let selected_analyze =
             S.Semantic.create
               ~owner:service
               ~env
               ~document_dir:(Eio.Stdenv.cwd env)
               ~script_selection:(Some selected_policy)
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
                    ~max_document_bundle_bytes:2048
                    ~max_parser_work_steps:100_000
                    ~max_diagnostic_work_steps:100_000
                  |> Or_error.ok_exn)
               ~prefix_limits:
                 (S.Semantic.Prefix_limits.create
                    ~max_document_bundle_files:4
                    ~max_document_bundle_bytes:2048
                    ~max_parser_work_steps:100_000
                    ~max_host_work_steps:100_000
                  |> Or_error.ok_exn)
           in
           (match
              S.Semantic.analyze selected_analyze ~actor ~connection bounded_request
            with
            | Ok (P.Authoring.Analysis.Analyze.Complete _) -> ()
            | Ok result ->
              failwith
                (Sexp.to_string_hum (P.Authoring.Analysis.Analyze.sexp_of_t result))
            | Error error ->
              failwith (P.Error.code_to_string (S.Error.to_protocol error).code));
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
            | Ok (P.Authoring.Analysis.Analyze.Complete { capture; _ }) ->
              assert (
                not
                  (Chatml_source.Digest.equal
                     (P.Authoring.Analysis.Capture.input_fingerprint capture)
                     (P.Authoring.Analysis.Capture.input_fingerprint baseline_capture)))
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
                edited_cursor
            | Ok (P.Authoring.Analysis.Prefix.Unavailable { reason; _ }) ->
              failwith
                (Sexp.to_string_hum
                   (P.Authoring.Analysis.Prefix.sexp_of_unavailable reason))
            | Error error ->
              failwith (P.Error.code_to_string (S.Error.to_protocol error).code));
           (match
              call
                (Context_close
                   (P.Authoring.Context.Close_request.create
                      ~expected:(P.Authoring.Context.Expected.of_view incomplete_context)))
            with
            | Ok (Context_close _) -> ()
            | Ok _ | Error _ -> failwith "edited ChatMD close failed");
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
