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
         Eio.Path.save ~create:(`Or_truncate 0o600) script_file "let main = 1\n";
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
                  |> Or_error.ok_exn)
           in
           S.publish_initialize_and_wait
             service
             ~connection
             ~response:(initialize_response ~server_id:host ~principal)
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
           let inventory =
             P.Authoring.Source_read.List_request.create
               ~context:expected
               ~package_namespace:None
               ~directory:None
             |> ok
           in
           let script_id =
             match call (Source_list inventory) with
             | Ok (Source_list result) ->
               List.find_exn
                 (P.Authoring.Source_read.List_result.sources result)
                 ~f:(fun source ->
                   String.equal
                     (Chatml_source.Id.relative_path (P.Authoring.Source.id source))
                     "main.chatml")
               |> P.Authoring.Source.id
             | Ok _ | Error _ -> failwith "project inventory unavailable"
           in
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
                "let main = 1\n"
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
