open Core
open Support
module Authoring = Agent_server.Authoring_source_host
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Path = Source.Portable_path
module P = Agent_protocol

let actor id =
  P.Principal.create
    ~id:(P.Id.Principal.of_string id |> ok)
    ~authentication_kind:"local.authoring_test"
    ~scopes:P.Scope.Set.empty
    ~attributes:[]
  |> ok
  |> Operator_authorization.trusted_local
;;

let authoring_ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
;;

let with_source_context ?(max_context_requests = 8) f =
  with_workspace (fun env root_path data ->
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(root_path / "chatml.project.json")
      {|{"schema_version":1,"namespace":"authoring","entries":{"main":{"source":"main.chatml","target":"one_off_v1"}}}|};
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(root_path / "main.chatml")
      "let main = 1\n";
    Eio.Switch.run (fun sw ->
      let configuration =
        Configuration.{ external_roots = [ Eio.Path.native_exn root_path ]; git = None }
      in
      let sources = open_host ~configuration env data sw in
      let allowed = actor "pri_authoring_allowed" in
      let denied = actor "pri_authoring_denied" in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun actor ~root_index ->
          Int.equal root_index 0
          && P.Id.Principal.equal
               (Operator_authorization.principal actor).id
               (Operator_authorization.principal allowed).id)
      in
      let limits =
        Authoring.Limits.create
          ~max_contexts:2
          ~max_context_requests
          ~max_manifest_entries:8
          ~max_browse_entries:32
          ~max_read_bytes:1024
          ~max_context_read_bytes:2048
        |> Or_error.ok_exn
      in
      let host =
        Authoring.create
          sources
          ~sw
          ~read_policy:policy
          ~document_policy:Authoring.Document_policy.deny_all
          ~limits
      in
      let root = Authoring.roots host ~actor:allowed ~checkpoint:ignore |> authoring_ok in
      [%test_eq: int] (List.length root) 1;
      [%test_eq: int]
        (List.length
           (Authoring.roots host ~actor:denied ~checkpoint:ignore |> authoring_ok))
        0;
      let root = List.hd_exn root in
      let project = Path.create "chatml.project.json" |> Or_error.ok_exn in
      let contract =
        Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:ignore
      in
      let context =
        Authoring.Context.open_root
          host
          ~actor:allowed
          ~root
          ~dependency_roots:[]
          ~project
          ~selection:(Entry { name = "main"; environment = None })
          ~contract
        |> authoring_ok
      in
      Exn.protect
        ~finally:(fun () ->
          Authoring.Context.close_and_wait context;
          Authoring.close_and_wait host;
          Host.close_and_wait sources)
        ~f:(fun () -> f env root context allowed denied sources host data root_path)))
;;

let%test_unit "authoring context captures a local project and reads only its grant" =
  with_source_context (fun env _ context allowed denied _ _ _ _ ->
    Authoring.Context.check_current context ~actor:allowed |> authoring_ok;
    (match Authoring.Context.check_current context ~actor:denied with
     | Error Authoring.Error.Unauthorized -> ()
     | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
     | Ok () -> failwith "foreign actor reused pinned project capture");
    let capture =
      Authoring.Context.capture
        context
        ~actor:allowed
        ~clock:env#mono_clock
        ~checkpoint:ignore
      |> authoring_ok
    in
    let graph = Authoring.Capture.prepared capture in
    assert (
      Captured.Digest.equal
        (Authoring.Capture.fingerprint capture)
        (Prepared.fingerprint graph));
    assert (
      List.exists (Authoring.Capture.sources capture) ~f:(fun source ->
        String.equal (Captured.contents source) "let main = 1\n"));
    let entries =
      Authoring.Context.list_directory
        context
        ~actor:allowed
        ~directory:None
        ~checkpoint:ignore
      |> authoring_ok
    in
    assert (
      List.exists entries ~f:(fun entry ->
        String.equal (Path.to_string (Authoring.Entry.member entry)) "chatml.project.json"));
    let main = Path.create "main.chatml" |> Or_error.ok_exn in
    [%test_eq: string]
      (Authoring.Context.read_member
         context
         ~actor:allowed
         ~member:main
         ~checkpoint:ignore
       |> authoring_ok)
      "let main = 1\n";
    (match
       Authoring.Context.read_member context ~actor:denied ~member:main ~checkpoint:ignore
     with
     | Error Authoring.Error.Unauthorized -> ()
     | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
     | Ok _ -> failwith "foreign actor read admitted");
    Authoring.Context.close_and_wait context;
    match Authoring.Context.check_current context ~actor:allowed with
    | Error Authoring.Error.Closed -> ()
    | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
    | Ok () -> failwith "closed project context reused cached analysis")
;;

let analysis_policy ?(full = false) () =
  let choose ~ordinary ~full_limit = if full then full_limit else ordinary in
  Chatml_authoring_analysis.Policy.create
    ~compilation:Chatml_compilation.default_limits
    ~semantic:
      (Chatml_semantics.Limits.create
         ~max_input_bytes:(choose ~ordinary:2048 ~full_limit:1_000_000)
         ~recording:
           (Chatml_typechecker.Semantic.Limits.create
              ~max_facts:(choose ~ordinary:100 ~full_limit:20_000)
              ~max_edges:(choose ~ordinary:100 ~full_limit:40_000)
              ~max_label_bytes:(choose ~ordinary:2048 ~full_limit:400_000)
              ~max_work_steps:(choose ~ordinary:1000 ~full_limit:400_000)
            |> Or_error.ok_exn)
         ~type_graph:
           (Chatml_semantics.Frozen.Limits.create
              ~max_nodes:(choose ~ordinary:100 ~full_limit:20_000)
              ~max_edges:(choose ~ordinary:100 ~full_limit:40_000)
              ~max_label_bytes:(choose ~ordinary:2048 ~full_limit:400_000)
            |> Or_error.ok_exn)
         ~max_published_facts:(choose ~ordinary:100 ~full_limit:20_000)
         ~max_published_edges:(choose ~ordinary:100 ~full_limit:40_000)
         ~max_published_label_bytes:(choose ~ordinary:2048 ~full_limit:400_000)
         ~max_work_steps:(choose ~ordinary:1000 ~full_limit:400_000)
       |> Or_error.ok_exn)
    ~max_dependency_depth:2
    ~max_results:1
    ~max_projected_diagnostics:2
    ~max_projected_sites:2
    ~max_projection_sources:2
    ~max_position_index_entries:16
    ~max_projection_source_bytes:(choose ~ordinary:2048 ~full_limit:1_000_000)
  |> Or_error.ok_exn
;;

let%test_unit "configured grants retain physical source-host affinity" =
  with_source_context (fun env _ _ allowed denied source_a _ data root_path ->
    Eio.Switch.run (fun sw ->
      let module Access = Agent_server.Authoring_project_access_configuration in
      let module Bound = Agent_server.Authoring_configured_project_policy in
      let root_name = Eio.Path.native_exn root_path in
      let configuration = Configuration.{ external_roots = [ root_name ]; git = None } in
      let selected =
        Access.Selection.
          { root = root_name
          ; project = "chatml.project.json"
          ; request = Entry { name = "main"; environment = None }
          ; target = One_off_v1
          }
      in
      let access =
        Access.
          { read_grants =
              [ { root = root_name
                ; principal_id = (Operator_authorization.principal allowed).id
                ; authentication_kind = "local.authoring_test"
                }
              ]
          ; selections = [ selected ]
          }
      in
      let bound =
        Bound.bind
          source_a
          ~configuration:access
          ~project_sources:configuration
          ~contracts:None
          ~max_contract_identity_bytes:(16 * 1024 * 1024)
          ~max_work_steps:1_000_000
          ~checkpoint:ignore
        |> function
        | Ok bound -> bound
        | Error failure -> Error.raise_s (Bound.Error.sexp_of_t failure)
      in
      assert (Bound.issued_by bound source_a);
      (match
         Bound.bind
           source_a
           ~configuration:access
           ~project_sources:configuration
           ~contracts:None
           ~max_contract_identity_bytes:1
           ~max_work_steps:1_000_000
           ~checkpoint:ignore
       with
       | Error Bound.Error.Contract_limit -> ()
       | Ok _ | Error _ -> failwith "configured contract escaped aggregate admission");
      let limits =
        Authoring.Limits.create
          ~max_contexts:2
          ~max_context_requests:8
          ~max_manifest_entries:8
          ~max_browse_entries:32
          ~max_read_bytes:1024
          ~max_context_read_bytes:2048
        |> Or_error.ok_exn
      in
      let grant_host =
        Authoring.create
          source_a
          ~sw
          ~read_policy:(Bound.read_policy bound)
          ~document_policy:Authoring.Document_policy.deny_all
          ~limits
      in
      let roots =
        Authoring.roots grant_host ~actor:allowed ~checkpoint:ignore |> authoring_ok
      in
      [%test_eq: int] (List.length roots) 1;
      [%test_eq: int]
        (List.length
           (Authoring.roots grant_host ~actor:denied ~checkpoint:ignore |> authoring_ok))
        0;
      let other_kind =
        P.Principal.create
          ~id:(Operator_authorization.principal allowed).id
          ~authentication_kind:"http.static_bearer"
          ~scopes:P.Scope.Set.empty
          ~attributes:[]
        |> ok
        |> Operator_authorization.trusted_local
      in
      [%test_eq: int]
        (List.length
           (Authoring.roots grant_host ~actor:other_kind ~checkpoint:ignore
            |> authoring_ok))
        0;
      let root = List.hd_exn roots in
      let project = Path.create selected.project |> Or_error.ok_exn in
      let request =
        P.Authoring.Context.Selection.create ~target:(Entry "main") ~environment:None
        |> ok
      in
      assert (
        Option.is_some
          (Bound.project_contract bound allowed ~root ~project ~selection:request));
      assert (
        Option.is_none
          (Bound.project_contract bound denied ~root ~project ~selection:request));
      let session =
        Agent_store.Session_store.create
          ~env
          ~sw
          ~root:(Eio.Path.native_exn Eio.Path.(root_path / "authoring-session"))
          ~server_id:P.Id.Server.(create ())
          ~process_start_identity:None
          ~lock_nonce:"configured-authoring"
        |> function
        | Ok session -> session
        | Error failure -> Error.raise_s (Agent_store.Store_error.sexp_of_t failure)
      in
      let initialize_limits = Agent_server.Daemon.default_options.protocol_limits in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(root_path / "main.chatml")
        "let main input = Task.pure(input)\n";
      let module Installation = Agent_server.Authoring_installation in
      let module Service = Agent_server.Authoring_context_service in
      let semantic =
        Installation.Host_policy.Semantic.create
          ~script_selection:None
          ~project_metadata:None
          ~project_build_limits:
            (Chatml_project_build.Limits.create
               ~max_work_steps:1_000_000
               ~max_input_bytes:(Some 64_000)
               ~document:Document_schema.Limits.default
             |> Or_error.ok_exn)
          ~limits:
            (Service.Semantic.Limits.create
               ~max_document_bundle_files:4
               ~max_document_bundle_bytes:2048
               ~max_parser_work_steps:100_000
               ~max_diagnostic_work_steps:100_000
               ~max_selection_rows:32
               ~max_retained_selection_source_bytes:(16 * 1024 * 1024)
               ~max_retained_selection_input_bytes:(16 * 1024 * 1024)
               ~max_selection_work_steps:1_000_000
             |> Or_error.ok_exn)
          ~prefix_limits:
            (Service.Semantic.Prefix_limits.create
               ~max_document_bundle_files:4
               ~max_document_bundle_bytes:2048
               ~max_parser_work_steps:100_000
               ~max_host_work_steps:100_000
             |> Or_error.ok_exn)
          ~query_limits:
            (Service.Semantic.Query_limits.create
               ~max_host_work_steps:100_000
               ~projection:
                 (Chatml_authoring_query.Limits.create
                    ~max_items:8
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
             |> Or_error.ok_exn)
          ~document_dir:(Eio.Stdenv.cwd env)
      in
      let base_policy =
        Installation.Host_policy.create
          ~core:true
          ~source:None
          ~upload:None
          ~supplied_document:
            (Some (Service.Supplied_document_policy.create ~authorize:(fun _ -> true)))
          ~selection:
            (Service.Selection_policy.create
               ~project_contract:(fun _ ~root:_ ~project:_ ~selection:_ -> None)
               ~document_static:(fun _ ~requested_environment:_ -> None))
          ~context_limits:
            (Chatml_authoring_context.Limits.create
               ~max_contexts:2
               ~max_sources:16
               ~max_open_documents:1
               ~max_document_bytes:1024
               ~max_effective_input_bytes:64_000
               ~max_context_source_bytes:64_000
               ~max_total_source_bytes:128_000
             |> Or_error.ok_exn)
          ~analysis_policy:(analysis_policy ~full:true ())
          ~protocol_limits:
            (P.Authoring.Limits.create
               ~max_sources:4
               ~max_source_bytes:2048
               ~max_context_source_bytes:4096
               ~max_request_bytes:2048
               ~max_result_bytes:64_000
               ~max_path_candidates:16
             |> ok)
          ~service_limits:
            (Service.Limits.create
               ~max_connections:2
               ~max_active_requests_per_connection:2
               ~max_roots_per_connection:2
               ~max_response_bytes:64_000
               ~max_provisional_results:8
             |> Or_error.ok_exn)
          ~semantic:(Some semantic)
          ~assistance:None
          ~initialize_limits
        |> ok
      in
      let policy =
        Agent_server.Authoring_installation.Host_policy.with_configured_projects
          base_policy
          ~bound
          ~source_limits:limits
          ~analysis_policy:(analysis_policy ~full:true ())
        |> ok
      in
      let installed =
        Installation.create
          ~env
          ~sw
          ~clock:env#mono_clock
          ~project_sources:source_a
          ~store:session
          ~policy
          ~initialize_limits
          ~checkpoint:ignore
        |> ok
      in
      let live = ref true in
      let principal = Operator_authorization.principal allowed in
      let service_actor =
        Operator_authorization.guarded ~principal ~is_current:(fun () -> !live)
      in
      let connection =
        Agent_server.Connection_context.create_authenticated
          ~actor:service_actor
          ~connection_id:"configured project authoring"
          ~transport:In_memory
          ~publish_notification:(fun _ -> ())
          ~max_attachments:1
      in
      let server_id = Agent_store.Session_store.server_id session in
      let response =
        P.Initialize.Response.create
          ~protocol_name:"ochat.agent"
          ~selected_version:(P.Version.create ~major:1 ~minor:0 |> ok)
          ~implementation:
            (P.Initialize.Implementation.create ~name:"authoring test" ~version:"1" |> ok)
          ~server_id
          ~enabled_features:[ P.Authoring.Features.core; P.Authoring.Features.semantic ]
          ~extensions:None
          ~principal
          ~limits:initialize_limits
          ~event_retention:
            { minimum_age_ms = 0; maximum_events = 1; oldest_replayable_sequence = None }
          ~timing:
            { heartbeat_interval_ms = 1000
            ; owner_lease_duration_ms = 10000
            ; owner_renew_after_ms = 5000
            ; disconnect_grace_default_ms = 0
            }
          ~server_time:(P.Timestamp.of_string "2026-10-15T00:00:00Z" |> ok)
        |> ok
      in
      Installation.publish_initialize_and_wait installed ~connection ~response |> ok;
      let call request =
        Installation.handle installed ~actor:service_actor ~connection request
      in
      let root =
        match call P.Authoring.Wire.Request.Roots with
        | Ok (Roots roots) -> (List.hd_exn roots.roots).id
        | Ok _ | Error _ -> failwith "configured root was not installed"
      in
      let opened =
        match
          call
            (Context_open
               (P.Authoring.Context.Open_request.create_host_project
                  ~root
                  ~dependency_roots:[]
                  ~project_member:selected.project
                  ~selection:request
                |> ok))
        with
        | Ok (Context_open view) -> view
        | Ok _ -> failwith "configured project context did not open"
        | Error failure ->
          failwith ("configured project context did not open: " ^ failure.message)
      in
      let expected = P.Authoring.Context.Expected.of_view opened in
      let analyze expected =
        call
          (Analyze
             (P.Authoring.Analysis.Analyze.Request.create
                ~context:expected
                ~target:P.Authoring.Analysis.Target.project
              |> ok))
      in
      let first =
        match analyze expected with
        | Ok (Analyze (Complete complete)) -> complete.capture
        | Ok (Analyze result) ->
          failwith
            ("configured project was not checked: "
             ^ Sexp.to_string_hum (P.Authoring.Analysis.Analyze.sexp_of_t result))
        | Ok _ -> failwith "configured project response changed"
        | Error failure -> failwith ("configured project check failed: " ^ failure.message)
      in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(root_path / "main.chatml")
        "let main value = Task.pure(value)\n";
      let refreshed =
        match
          call
            (Context_refresh
               (P.Authoring.Context.Refresh_request.create
                  ~expected
                  ~policy:Reject_open_documents))
        with
        | Ok (Context_refresh view) -> view
        | Ok _ | Error _ ->
          failwith "configured project refresh did not capture disk edit"
      in
      let refreshed_expected = P.Authoring.Context.Expected.of_view refreshed in
      (match analyze expected with
       | Error failure when P.Error.equal_code failure.code P.Error.Conflict -> ()
       | Ok _ | Error _ -> failwith "previous configured context survived refresh");
      let second =
        match analyze refreshed_expected with
        | Ok (Analyze (Complete complete)) -> complete.capture
        | Ok _ | Error _ -> failwith "refreshed configured project was not checked"
      in
      assert (
        not
          (Captured.Digest.equal
             (P.Authoring.Analysis.Capture.input_fingerprint first)
             (P.Authoring.Analysis.Capture.input_fingerprint second)));
      live := false;
      (match analyze refreshed_expected with
       | Error failure when P.Error.equal_code failure.code P.Error.Permission_denied ->
         ()
       | Ok _ | Error _ -> failwith "revoked configured actor retained semantic authority");
      Installation.close_connection_and_wait installed connection |> ok;
      Installation.close_and_wait installed |> ok;
      let source_b = open_host ~configuration env data sw in
      assert (not (Bound.issued_by bound source_b));
      (match
         Agent_server.Authoring_installation.create
           ~env
           ~sw
           ~clock:env#mono_clock
           ~project_sources:source_b
           ~store:session
           ~policy
           ~initialize_limits
           ~checkpoint:ignore
       with
       | Error failure -> assert (P.Error.equal_code failure.code P.Error.Invalid_request)
       | Ok _ -> failwith "configured grants crossed physical host owners");
      Authoring.close_and_wait grant_host;
      Host.close_and_wait source_b;
      Agent_store.Session_store.close session
      |> function
      | Ok () -> ()
      | Error failure -> Error.raise_s (Agent_store.Store_error.sexp_of_t failure)))
;;

let%test_unit "shared overlay preparation uses actual host contract and a new lock" =
  with_source_context (fun env _ owner allowed _ _ _ _ _ ->
    let module Ledger = Chatml_authoring_context in
    let module Testing = Agent_server.Authoring_context_service.For_testing in
    let build =
      Authoring.Context.capture
        owner
        ~actor:allowed
        ~clock:env#mono_clock
        ~checkpoint:ignore
      |> authoring_ok
      |> Authoring.Capture.build
    in
    let identity =
      Ledger.Identity.create
        ~principal:"pri_authoring_allowed"
        ~host_incarnation:"foundation"
        ~connection_lease:"lease"
        ~nonce:"project"
      |> Or_error.ok_exn
    in
    let main =
      Prepared.sources (Chatml_project_build.prepared build)
      |> List.find_exn ~f:(fun source ->
        String.equal (Captured.Id.relative_path (Captured.id source)) "main.chatml")
    in
    let ledger =
      Ledger.create
        ~limits:
          (Ledger.Limits.create
             ~max_contexts:1
             ~max_sources:8
             ~max_open_documents:1
             ~max_document_bytes:1024
             ~max_effective_input_bytes:20_000
             ~max_context_source_bytes:20_000
             ~max_total_source_bytes:40_000
           |> Or_error.ok_exn)
      |> fun ledger ->
      Ledger.admit
        ledger
        ~identity
        ~base:build
        ~editable_sources:[ Captured.id main ]
        ~checkpoint:ignore
      |> ok
    in
    let root_label = P.Id.Authoring_root.create () in
    let selection =
      P.Authoring.Context.Selection.create ~target:(Entry "main") ~environment:None |> ok
    in
    let origin =
      P.Authoring.Context.Host_project
        { root = root_label
        ; dependency_roots = []
        ; project_member = "chatml.project.json"
        ; selection
        }
    in
    let id = P.Id.Authoring_context.create () in
    let prepared = Chatml_project_build.prepared build in
    let view =
      P.Authoring.Context.create
        ~id
        ~host:(P.Id.Server.create ())
        ~origin
        ~generation:0
        ~basis:
          (Project
             { base_graph = Prepared.fingerprint prepared
             ; selection =
                 { environment = Prepared.Environment.name (Prepared.environment prepared)
                 ; compiler_contract = Prepared.compiler_contract prepared
                 }
             })
        ~basis_fingerprint:(Prepared.fingerprint prepared)
        ~editable_sources:[]
      |> ok
    in
    let limits =
      Chatml_project_build.Limits.create
        ~max_work_steps:1_000_000
        ~max_input_bytes:(Some 100_000)
        ~document:Document_schema.Limits.default
      |> Or_error.ok_exn
    in
    let original =
      Testing.preview_project_overlay
        ~context:owner
        ~original_actor:allowed
        ~ledger
        ~identity
        ~view
        ~analysis_policy:(analysis_policy ())
        ~limits
        ~checkpoint:ignore
      |> function
      | Ok value -> value
      | Error _ -> failwith "authentic unchanged overlay was rejected"
    in
    assert (
      not
        (Captured.Id.equal
           (Captured.id (Chatml_project_build.lock_source (fst original)))
           (Captured.id (Chatml_project_build.lock_source build))));
    assert (
      Captured.Digest.equal
        (Chatml.Chatml_compiler_contract.fingerprint (snd original) ~checkpoint:ignore)
        (Prepared.compiler_contract prepared));
    let version =
      Ledger.version ledger ~identity ~source:(Captured.id main) |> Option.value_exn
    in
    let edited =
      Ledger.replace
        ledger
        ~expected:version
        ~next_revision:(Ledger.Editor_revision.create 1 |> Or_error.ok_exn)
        ~contents:"let main = 9\n"
        ~checkpoint:ignore
      |> ok
    in
    let changed =
      Testing.preview_project_overlay
        ~context:owner
        ~original_actor:allowed
        ~ledger:edited
        ~identity
        ~view
        ~analysis_policy:(analysis_policy ())
        ~limits
        ~checkpoint:ignore
      |> function
      | Ok value -> value
      | Error _ -> failwith "authentic changed overlay was rejected"
    in
    let selected_main =
      Prepared.sources (Chatml_project_build.prepared (fst changed))
      |> List.find_exn ~f:(fun source ->
        Captured.Id.equal (Captured.id source) (Captured.id main))
    in
    [%test_eq: string] (Captured.contents selected_main) "let main = 9\n")
;;

let%test_unit "host document captures exact bytes without project preparation" =
  with_source_context (fun _ root _ allowed denied _ host _ root_path ->
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(root_path / "draft.chatmd")
      "\255unfinished `chatml";
    let member = Path.create "draft.chatmd" |> Or_error.ok_exn in
    let context, captured =
      Authoring.Document_context.open_host
        host
        ~actor:allowed
        ~root
        ~member
        ~checkpoint:ignore
      |> authoring_ok
    in
    Exn.protect
      ~finally:(fun () -> Authoring.Document_context.close_and_wait context)
      ~f:(fun () ->
        [%test_eq: string]
          (Authoring.Document_capture.source captured |> Captured.contents)
          "\255unfinished `chatml";
        assert (
          Captured.Id.equal
            (Authoring.Document_context.source_id context)
            (Authoring.Document_capture.source captured |> Captured.id));
        (match
           Authoring.Document_context.refresh context ~actor:denied ~checkpoint:ignore
         with
         | Error Authoring.Error.Unauthorized -> ()
         | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
         | Ok _ -> failwith "foreign actor refreshed document");
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          Eio.Path.(root_path / "draft.chatmd")
          "";
        let refreshed =
          Authoring.Document_context.refresh context ~actor:allowed ~checkpoint:ignore
          |> authoring_ok
        in
        [%test_eq: string]
          (Authoring.Document_capture.source refreshed |> Captured.contents)
          "";
        [%test_eq: string]
          (Authoring.Document_capture.source captured |> Captured.contents)
          "\255unfinished `chatml"))
;;

let%test_unit "explicit document capture binds unsaved root and imported source bytes" =
  with_source_context (fun _ root _ allowed denied _ host _ root_path ->
    let path name = Eio.Path.(root_path / name) in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      (path "draft.chatmd")
      "<developer>native root must not win</developer>";
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      (path "part.chatmd")
      "<developer>first import</developer>";
    let member = Path.create "draft.chatmd" |> Or_error.ok_exn in
    let context, opened =
      Authoring.Document_context.open_host
        host
        ~actor:allowed
        ~root
        ~member
        ~checkpoint:ignore
      |> authoring_ok
    in
    Exn.protect
      ~finally:(fun () -> Authoring.Document_context.close_and_wait context)
      ~f:(fun () ->
        let original = Authoring.Document_capture.source opened in
        let edited =
          Captured.create
            (Captured.id original)
            ~contents:
              {|<import src="part.chatmd" namespace="part"/><developer>unsaved root</developer>|}
            ~expected_digest:None
            ~max_bytes:(Some 1024)
            ~checkpoint:ignore
          |> Or_error.ok_exn
        in
        let capture () =
          Authoring.Document_context.capture_parser_bundle
            context
            ~actor:allowed
            ~source:edited
            ~dir:root_path
            ~checkpoint:ignore
          |> authoring_ok
        in
        (match
           Authoring.Document_context.capture_parser_bundle
             context
             ~actor:denied
             ~source:edited
             ~dir:root_path
             ~checkpoint:ignore
         with
         | Error Authoring.Error.Unauthorized -> ()
         | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
         | Ok _ -> failwith "foreign actor admitted imported closure");
        let first = capture () in
        let module Parsed = Authoring.Document_context.Parsed_capture in
        [%test_eq: string]
          (Captured.contents (Parsed.source first))
          (Captured.contents edited);
        let input_sources = Parsed.input_sources first in
        [%test_eq: int] (List.length input_sources) 2;
        let source_at member =
          input_sources
          |> List.find_map ~f:(fun (input, source) ->
            if String.equal (Chatmd_parser_input.path input) member
            then Some source
            else None)
          |> Option.value_exn
        in
        assert (phys_equal (source_at "draft.chatmd") edited);
        [%test_eq: string]
          (Captured.contents (source_at "part.chatmd"))
          "<developer>first import</developer>";
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          (path "part.chatmd")
          "<developer>second import</developer>";
        [%test_eq: string]
          (Captured.contents (source_at "part.chatmd"))
          "<developer>first import</developer>";
        let second = capture () in
        let second_import =
          Parsed.input_sources second
          |> List.find_map ~f:(fun (input, source) ->
            if String.equal (Chatmd_parser_input.path input) "part.chatmd"
            then Some source
            else None)
          |> Option.value_exn
        in
        [%test_eq: string]
          (Captured.contents second_import)
          "<developer>second import</developer>";
        let reparse_with_root contents =
          let root_source =
            Captured.create
              (Captured.id edited)
              ~contents
              ~expected_digest:None
              ~max_bytes:(Some 1024)
              ~checkpoint:ignore
            |> Or_error.ok_exn
          in
          let bundle =
            Chatmd_source_bundle.create
              ~root_file:"draft.chatmd"
              ~sources:
                (Parsed.bundle first
                 |> Chatmd_source_bundle.sources
                 |> List.map ~f:(fun (member, bytes) ->
                   member, if String.equal member "draft.chatmd" then contents else bytes)
                )
              ()
            |> Result.ok_or_failwith
          in
          Parsed.reparse_frozen
            ~previous:first
            ~source:root_source
            ~bundle
            ~limits:Chatmd_source_bundle.default_limits
            ~dir:root_path
            ~checkpoint:ignore
        in
        let reparsed =
          reparse_with_root
            {|<import src="part.chatmd" namespace="part"/><developer>edited unsaved root</developer>|}
          |> authoring_ok
        in
        let reparsed_import =
          Parsed.input_sources reparsed
          |> List.find_map ~f:(fun (input, source) ->
            if String.equal (Chatmd_parser_input.path input) "part.chatmd"
            then Some source
            else None)
          |> Option.value_exn
        in
        assert (phys_equal reparsed_import (source_at "part.chatmd"));
        [%test_eq: string]
          (Captured.contents reparsed_import)
          "<developer>first import</developer>";
        (match reparse_with_root {|<import src="new.chatmd" namespace="new"/>|} with
         | Error (Authoring.Error.Document_parser Source_unavailable) -> ()
         | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
         | Ok _ -> failwith "frozen reparse acquired a newly required import");
        let missing =
          Captured.create
            (Captured.id edited)
            ~contents:{|<import src="missing.chatmd" namespace="missing"/>|}
            ~expected_digest:None
            ~max_bytes:(Some 1024)
            ~checkpoint:ignore
          |> Or_error.ok_exn
        in
        (match
           Authoring.Document_context.capture_parser_bundle
             context
             ~actor:allowed
             ~source:missing
             ~dir:root_path
             ~checkpoint:ignore
         with
         | Error (Authoring.Error.Document_parser Source_unavailable) -> ()
         | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
         | Ok _ -> failwith "missing import produced a partial capture");
        [%test_eq: string]
          (Captured.contents (source_at "part.chatmd"))
          "<developer>first import</developer>";
        let foreign_id =
          Captured.Id.create
            ~package_namespace:(Captured.Id.package_namespace (Captured.id edited))
            ~relative_path:"foreign.chatmd"
          |> Or_error.ok_exn
        in
        match
          Authoring.Document_context.capture_parser_bundle
            context
            ~actor:allowed
            ~source:(Captured.reidentify edited ~id:foreign_id)
            ~dir:root_path
            ~checkpoint:ignore
        with
        | Error Authoring.Error.Foreign_root -> ()
        | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
        | Ok _ -> failwith "foreign source label admitted as document root"))
;;

let%test_unit "declared project metadata follows the captured declaring input" =
  with_source_context
    ~max_context_requests:32
    (fun env root project_owner allowed denied source_host host data root_path ->
       let root_project_text =
         {|{"schema_version":1,"namespace":"authoring","entries":{"main":{"source":"main.chatml","target":"one_off_v1"}}}|}
       in
       let project_text =
         {|{"schema_version":1,"namespace":"imported","environments":{"default":{},"other":{}},"default_environment":"default","entries":{"main":{"source":"main.chatml","target":"one_off_v1"}}}|}
       in
       Eio.Path.mkdir ~perm:0o700 Eio.Path.(root_path / "nested");
       Eio.Path.save
         ~create:(`Or_truncate 0o600)
         Eio.Path.(root_path / "nested" / "chatml.project.json")
         project_text;
       Eio.Path.save
         ~create:(`Or_truncate 0o600)
         Eio.Path.(root_path / "nested" / "part.chatmd")
         {|<script id="lookup" language="chatml" kind="tool" project="./chatml.project.json">let main = 1</script><script id="file" language="chatml" kind="tool" project="./chatml.project.json" environment="other" src="lookup.chatml"/><script id="outside" language="chatml" kind="tool" project="./chatml.project.json" src="../outside.chatml"/>|};
       Eio.Path.save
         ~create:(`Or_truncate 0o600)
         Eio.Path.(root_path / "nested" / "lookup.chatml")
         "let main = 2\n";
       Eio.Path.save
         ~create:(`Or_truncate 0o600)
         Eio.Path.(root_path / "outside.chatml")
         "let main = 3\n";
       Eio.Path.save
         ~create:(`Or_truncate 0o600)
         Eio.Path.(root_path / "draft.chatmd")
         {|<import src="nested/part.chatmd" namespace="part"/>|};
       let member = Path.create "draft.chatmd" |> Or_error.ok_exn in
       let owner, opened =
         Authoring.Document_context.open_host
           host
           ~actor:allowed
           ~root
           ~member
           ~checkpoint:ignore
         |> authoring_ok
       in
       Exn.protect
         ~finally:(fun () -> Authoring.Document_context.close_and_wait owner)
         ~f:(fun () ->
           let module DC = Authoring.Document_context in
           let module PC = DC.Project_capture in
           let module CM = Prompt.Chat_markdown in
           let parsed =
             DC.capture_parser_bundle
               owner
               ~actor:allowed
               ~source:(Authoring.Document_capture.source opened)
               ~dir:root_path
               ~checkpoint:ignore
             |> authoring_ok
           in
           let generated = CM.Generated.parsed (DC.Parsed_capture.parsed parsed) in
           let declaration =
             List.concat_map
               (generated.parsed_root :: List.map generated.parsed_agents ~f:snd)
               ~f:CM.Parsed.declarations
             |> List.find_exn ~f:(fun declaration ->
               String.equal (CM.Declaration.script_id declaration) "part:lookup")
           in
           let external_declaration =
             List.concat_map
               (generated.parsed_root :: List.map generated.parsed_agents ~f:snd)
               ~f:CM.Parsed.declarations
             |> List.find_exn ~f:(fun declaration ->
               String.equal (CM.Declaration.script_id declaration) "part:file")
           in
           let outside_declaration =
             List.concat_map
               (generated.parsed_root :: List.map generated.parsed_agents ~f:snd)
               ~f:CM.Parsed.declarations
             |> List.find_exn ~f:(fun declaration ->
               String.equal (CM.Declaration.script_id declaration) "part:outside")
           in
           let source_only declaration =
             DC.Parsed_capture.script_source
               parsed
               ~declaration
               ~max_script_bytes:4096
               ~max_locator_bytes:1024
               ~max_path_components:16
               ~checkpoint:ignore
             |> authoring_ok
           in
           let imported_inline = source_only declaration in
           [%test_eq: string] (Captured.contents imported_inline) "let main = 1";
           let imported_external = source_only external_declaration in
           [%test_eq: string]
             (Captured.Id.relative_path (Captured.id imported_external))
             "nested/lookup.chatml";
           [%test_eq: string] (Captured.contents imported_external) "let main = 2\n";
           let capture
                 ?(parsed = parsed)
                 ?(declaration = declaration)
                 ?(schema_limits = Document_schema.Limits.default)
                 ?(max_work_steps = 100_000)
                 ()
             =
             DC.capture_project_declaration
               owner
               ~actor:allowed
               ~parsed
               ~declaration
               ~schema_limits
               ~max_locator_bytes:1024
               ~max_path_components:16
               ~max_identity_bytes:16_384
               ~max_work_steps
               ~checkpoint:ignore
           in
           let expect_limit = function
             | Error Authoring.Error.Result_limit -> ()
             | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
             | Ok _ -> failwith "declared project limit was bypassed"
           in
           let first = capture () |> authoring_ok in
           [%test_eq: string]
             (Path.to_string (PC.member first))
             "nested/chatml.project.json";
           [%test_eq: string] (Captured.contents (PC.source first)) project_text;
           [%test_eq: string] (Chatml_project.namespace (PC.project first)) "imported";
           assert (PC.matches first ~parsed ~declaration);
           assert (PC.belongs_to_root first root);
           Eio.Switch.run (fun sw ->
             let root_name = Eio.Path.native_exn root_path in
             let configuration =
               Configuration.{ external_roots = [ root_name ]; git = None }
             in
             let access =
               Agent_server.Authoring_project_access_configuration.
                 { read_grants =
                     [ { root = root_name
                       ; principal_id = (Operator_authorization.principal allowed).id
                       ; authentication_kind = "local.authoring_test"
                       }
                     ]
                 ; selections =
                     [ { root = root_name
                       ; project = "nested/chatml.project.json"
                       ; request = Library { environment = None }
                       ; target = One_off_v1
                       }
                     ]
                 }
             in
             let bind source_host =
               Agent_server.Authoring_configured_project_policy.bind
                 source_host
                 ~configuration:access
                 ~project_sources:configuration
                 ~contracts:None
                 ~max_contract_identity_bytes:(16 * 1024 * 1024)
                 ~max_work_steps:1_000_000
                 ~checkpoint:ignore
               |> function
               | Ok bound -> bound
               | Error error ->
                 Error.raise_s
                   (Agent_server.Authoring_configured_project_policy.Error.sexp_of_t
                      error)
             in
             let bound = bind source_host in
             let authorize bound actor =
               Agent_server.Authoring_configured_project_policy.project_capture_authorized
                 bound
                 ~original_actor:allowed
                 ~actor
                 first
             in
             assert (authorize bound allowed);
             assert (not (authorize bound denied));
             let foreign = open_host ~configuration env data sw in
             assert (not (authorize (bind foreign) allowed));
             Host.close_and_wait foreign);
           let contract =
             Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:ignore
           in
           let select_contract actor ~root:chosen ~project ~selection =
             if
               Operator_authorization.is_current actor
               && Int.equal (Authoring.Root.index chosen) (Authoring.Root.index root)
               && Path.equal project (PC.member first)
               && List.exists [ None; Some "other" ] ~f:(fun environment ->
                 P.Authoring.Context.Selection.equal
                   selection
                   (P.Authoring.Context.Selection.create ~target:Library ~environment
                    |> ok))
             then Some contract
             else None
           in
           let prepare ?(max_work_steps = 100_000) declaration project_capture =
             DC.prepare_selected_script
               owner
               ~actor:allowed
               ~parsed
               ~declaration
               ~project_capture
               ~select_contract
               ~clock:env#mono_clock
               ~max_script_bytes:4096
               ~max_locator_bytes:1024
               ~max_path_components:16
               ~max_work_steps
               ~checkpoint:ignore
           in
           let inline = prepare declaration first |> authoring_ok in
           assert (DC.Script_capture.matches inline ~parsed ~declaration ~project:first);
           [%test_eq: string]
             (Captured.contents (DC.Script_capture.entry_source inline))
             "let main = 1";
           let external_project =
             capture ~declaration:external_declaration () |> authoring_ok
           in
           let external_script =
             prepare external_declaration external_project |> authoring_ok
           in
           [%test_eq: string]
             (Captured.contents (DC.Script_capture.entry_source external_script))
             "let main = 2\n";
           let prepared =
             Chatml_project_build.prepared (DC.Script_capture.build external_script)
           in
           [%test_eq: string]
             (Chatml_project.Lock.environment
                (Chatml_project_build.lock (DC.Script_capture.build external_script)))
             "other";
           let selected_entry = Prepared.entry prepared in
           let selected_source =
             Prepared.find_source prepared selected_entry |> Option.value_exn
           in
           [%test_eq: string] (Captured.contents selected_source) "let main = 2\n";
           assert (
             Captured.Id.equal
               (Captured.id imported_external)
               (Captured.id selected_source));
           assert (
             Captured.Digest.equal
               (Captured.digest imported_external)
               (Captured.digest selected_source));
           let outside_project =
             capture ~declaration:outside_declaration () |> authoring_ok
           in
           Eio.Path.save
             ~create:(`Or_truncate 0o600)
             Eio.Path.(root_path / "outside.chatml")
             "let main = 999\n";
           let outside_script =
             prepare outside_declaration outside_project |> authoring_ok
           in
           let outside_prepared =
             Chatml_project_build.prepared (DC.Script_capture.build outside_script)
           in
           let outside_source =
             Prepared.find_source outside_prepared (Prepared.entry outside_prepared)
             |> Option.value_exn
           in
           [%test_eq: string] (Captured.contents outside_source) "let main = 3\n";
           [%test_eq: string]
             (Captured.contents (source_only outside_declaration))
             "let main = 3\n";
           [%test_eq: string]
             (Captured.Id.relative_path (Captured.id outside_source))
             "outside.chatml";
           (match prepare external_declaration first with
            | Error Authoring.Error.Foreign_root -> ()
            | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
            | Ok _ -> failwith "foreign declaration reused another project capture");
           (match prepare ~max_work_steps:1 declaration first with
            | Error Authoring.Error.Result_limit -> ()
            | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
            | Ok _ -> failwith "script preparation exceeded its work allowance");
           let selected_contract = ref contract in
           let selected_calls = ref 0 in
           let changed_contract =
             Chatml.Chatml_compiler_contract.for_target Moderator_v1 ~checkpoint:ignore
           in
           (match
              DC.prepare_selected_script
                owner
                ~actor:allowed
                ~parsed
                ~declaration
                ~project_capture:first
                ~select_contract:(fun actor ~root ~project ~selection ->
                  incr selected_calls;
                  Option.map
                    (select_contract actor ~root ~project ~selection)
                    ~f:(fun _ -> !selected_contract))
                ~clock:env#mono_clock
                ~max_script_bytes:4096
                ~max_locator_bytes:1024
                ~max_path_components:16
                ~max_work_steps:100_000
                ~checkpoint:(fun () ->
                  if !selected_calls >= 2 then selected_contract := changed_contract)
            with
            | Error Authoring.Error.Selected_script_unavailable -> ()
            | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
            | Ok _ -> failwith "changed compiler selection retained an old Build");
           assert (!selected_calls >= 4);
           let current = ref true in
           let actor_selection_calls = ref 0 in
           let expiring_actor =
             Operator_authorization.guarded
               ~principal:(Operator_authorization.principal allowed)
               ~is_current:(fun () -> !current)
           in
           (match
              DC.prepare_selected_script
                owner
                ~actor:expiring_actor
                ~parsed
                ~declaration
                ~project_capture:first
                ~select_contract:(fun actor ~root ~project ~selection ->
                  incr actor_selection_calls;
                  select_contract actor ~root ~project ~selection)
                ~clock:env#mono_clock
                ~max_script_bytes:4096
                ~max_locator_bytes:1024
                ~max_path_components:16
                ~max_work_steps:100_000
                ~checkpoint:(fun () ->
                  if !actor_selection_calls >= 2 then current := false)
            with
            | Error Authoring.Error.Expired_actor -> ()
            | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
            | Ok _ -> failwith "revoked request actor retained a script Build");
           assert (!actor_selection_calls >= 2);
           (match
              DC.prepare_selected_script
                owner
                ~actor:denied
                ~parsed
                ~declaration
                ~project_capture:first
                ~select_contract
                ~clock:env#mono_clock
                ~max_script_bytes:4096
                ~max_locator_bytes:1024
                ~max_path_components:16
                ~max_work_steps:100_000
                ~checkpoint:ignore
            with
            | Error Authoring.Error.Unauthorized -> ()
            | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
            | Ok _ -> failwith "foreign actor prepared the captured script");
           capture ~max_work_steps:1 () |> expect_limit;
           let read_limit =
             Document_schema.Limits.create
               ~max_bytes:8
               ~max_depth:16
               ~max_fields:16
               ~max_nodes:32
             |> ok
           in
           capture ~schema_limits:read_limit () |> expect_limit;
           let decode_limit =
             Document_schema.Limits.create
               ~max_bytes:1024
               ~max_depth:16
               ~max_fields:1
               ~max_nodes:32
             |> ok
           in
           capture ~schema_limits:decode_limit () |> expect_limit;
           let build =
             Authoring.Context.capture
               project_owner
               ~actor:allowed
               ~clock:env#mono_clock
               ~checkpoint:ignore
             |> authoring_ok
             |> Authoring.Capture.build
           in
           assert (
             not
               (PC.matches_project_contents
                  first
                  ~build
                  ~max_compare_bytes:4096
                  ~checkpoint:ignore
                |> authoring_ok));
           (match
              DC.capture_project_declaration
                owner
                ~actor:denied
                ~parsed
                ~declaration
                ~schema_limits:Document_schema.Limits.default
                ~max_locator_bytes:1024
                ~max_path_components:16
                ~max_identity_bytes:16_384
                ~max_work_steps:100_000
                ~checkpoint:ignore
            with
            | Error Authoring.Error.Unauthorized -> ()
            | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
            | Ok _ -> failwith "foreign actor captured project declaration");
           Eio.Path.save
             ~create:(`Or_truncate 0o600)
             Eio.Path.(root_path / "nested" / "chatml.project.json")
             root_project_text;
           [%test_eq: string] (Captured.contents (PC.source first)) project_text;
           let second = capture () |> authoring_ok in
           assert (not (Captured.Digest.equal (PC.identity first) (PC.identity second)));
           assert (
             PC.matches_project_contents
               second
               ~build
               ~max_compare_bytes:4096
               ~checkpoint:ignore
             |> authoring_ok);
           let reparsed =
             DC.capture_parser_bundle
               owner
               ~actor:allowed
               ~source:(Authoring.Document_capture.source opened)
               ~dir:root_path
               ~checkpoint:ignore
             |> authoring_ok
           in
           assert (not (PC.matches first ~parsed:reparsed ~declaration));
           (match capture ~parsed:reparsed () with
            | Error Authoring.Error.Foreign_root -> ()
            | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
            | Ok _ -> failwith "foreign parsed declaration admitted");
           Eio.Path.save
             ~create:(`Or_truncate 0o600)
             Eio.Path.(root_path / "nested" / "chatml.project.json")
             "{";
           match capture () with
           | Error Authoring.Error.Invalid_project_declaration -> ()
           | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
           | Ok _ -> failwith "malformed project declaration admitted"))
;;

let%test_unit
    "retained import bytes survive a root edit without reviving old parsed facts"
  =
  with_source_context (fun env root _ allowed _ _ host _ root_path ->
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(root_path / "draft.chatmd")
      {|<import src="part.chatmd" namespace="part"/><developer>base</developer>|};
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(root_path / "part.chatmd")
      "<developer>captured dependency</developer>";
    let member = Path.create "draft.chatmd" |> Or_error.ok_exn in
    let owner, opened =
      Authoring.Document_context.open_host
        host
        ~actor:allowed
        ~root
        ~member
        ~checkpoint:ignore
      |> authoring_ok
    in
    Exn.protect
      ~finally:(fun () -> Authoring.Document_context.close_and_wait owner)
      ~f:(fun () ->
        let module Ledger = Chatml_authoring_context in
        let module Testing = Agent_server.Authoring_context_service.For_testing in
        let source = Authoring.Document_capture.source opened in
        let parsed =
          Authoring.Document_context.capture_parser_bundle
            owner
            ~actor:allowed
            ~source
            ~dir:root_path
            ~checkpoint:ignore
          |> authoring_ok
        in
        let identity =
          Ledger.Identity.create
            ~principal:"pri_authoring_allowed"
            ~host_incarnation:"foundation"
            ~connection_lease:"lease"
            ~nonce:"document"
          |> Or_error.ok_exn
        in
        let ledger =
          Ledger.create
            ~limits:
              (Ledger.Limits.create
                 ~max_contexts:1
                 ~max_sources:2
                 ~max_open_documents:1
                 ~max_document_bytes:1024
                 ~max_effective_input_bytes:2048
                 ~max_context_source_bytes:2048
                 ~max_total_source_bytes:4096
               |> Or_error.ok_exn)
          |> fun ledger ->
          Ledger.admit_document
            ledger
            ~identity
            ~base:source
            ~requested_environment:None
            ~checkpoint:ignore
          |> ok
        in
        let wire_source =
          P.Authoring.Source.create
            ~id:(Captured.id source)
            ~digest:(Captured.digest source)
            ~byte_length:(Captured.byte_length source)
            ~editor_revision:None
          |> ok
        in
        let root_label = P.Id.Authoring_root.create () in
        let origin =
          P.Authoring.Context.Host_document
            { root = root_label; source = wire_source; default_environment = None }
        in
        let id = P.Id.Authoring_context.create () in
        let view =
          P.Authoring.Context.create
            ~id
            ~host:(P.Id.Server.create ())
            ~origin
            ~generation:0
            ~basis:(Document { source = wire_source; selection = None })
            ~basis_fingerprint:(Captured.digest source)
            ~editable_sources:[]
          |> ok
        in
        let retained =
          Testing.Retained_document.prepare
            ~context:owner
            ~original_actor:allowed
            ~ledger
            ~identity
            ~view
            ~capture:parsed
            ~max_sources:2
            ~max_bytes:2048
            ~checkpoint:ignore
          |> ok
        in
        let duplicate_capture =
          Authoring.Document_context.capture_parser_bundle
            owner
            ~actor:allowed
            ~source
            ~dir:root_path
            ~checkpoint:ignore
          |> authoring_ok
        in
        let duplicate =
          Testing.Retained_document.prepare
            ~context:owner
            ~original_actor:allowed
            ~ledger
            ~identity
            ~view
            ~capture:duplicate_capture
            ~max_sources:2
            ~max_bytes:2048
            ~checkpoint:ignore
          |> ok
        in
        assert (not (Testing.Retained_document.same_head retained duplicate));
        assert (
          Testing.Retained_document.matches_inputs
            retained
            ~ledger
            ~identity
            ~view
            ~checkpoint:ignore
          |> ok);
        let version =
          Ledger.version ledger ~identity ~source:(Captured.id source) |> Option.value_exn
        in
        let edited_ledger =
          Ledger.replace
            ledger
            ~expected:version
            ~next_revision:(Ledger.Editor_revision.create 1 |> Or_error.ok_exn)
            ~contents:
              {|<import src="part.chatmd" namespace="part"/><developer>unsaved</developer>|}
            ~checkpoint:ignore
          |> ok
        in
        assert (
          not
            (Testing.Retained_document.matches_inputs
               retained
               ~ledger:edited_ledger
               ~identity
               ~view
               ~checkpoint:ignore
             |> ok));
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          Eio.Path.(root_path / "part.chatmd")
          "<developer>changed host dependency</developer>";
        let recomposed =
          Testing.Retained_document.recompose_current
            retained
            ~ledger:edited_ledger
            ~identity
            ~view
            ~checkpoint:ignore
          |> ok
          |> Option.value_exn
        in
        [%test_eq: (string * string) list]
          (Chatmd_source_bundle.sources recomposed)
          [ ( "draft.chatmd"
            , {|<import src="part.chatmd" namespace="part"/><developer>unsaved</developer>|}
            )
          ; "part.chatmd", "<developer>captured dependency</developer>"
          ];
        let foreign_view =
          P.Authoring.Context.create
            ~id:(P.Id.Authoring_context.create ())
            ~host:(P.Authoring.Context.host view)
            ~origin
            ~generation:0
            ~basis:(Document { source = wire_source; selection = None })
            ~basis_fingerprint:(Captured.digest source)
            ~editable_sources:[]
          |> ok
        in
        assert (
          Option.is_none
            (Testing.Retained_document.recompose_current
               retained
               ~ledger:edited_ledger
               ~identity
               ~view:foreign_view
               ~checkpoint:ignore
             |> ok))))
;;

let%test_unit "supplied document needs its own actor policy and no read root" =
  with_source_context (fun _ _ _ allowed denied sources _ _ _ ->
    Eio.Switch.run (fun sw ->
      let limits =
        Authoring.Limits.create
          ~max_contexts:1
          ~max_context_requests:3
          ~max_manifest_entries:2
          ~max_browse_entries:2
          ~max_read_bytes:16
          ~max_context_read_bytes:16
        |> Or_error.ok_exn
      in
      let denied_host =
        Authoring.create
          sources
          ~sw
          ~read_policy:Authoring.Read_policy.deny_all
          ~document_policy:Authoring.Document_policy.deny_all
          ~limits
      in
      let member = Path.create "scratch.chatmd" |> Or_error.ok_exn in
      (match
         Authoring.Document_context.open_supplied
           denied_host
           ~actor:allowed
           ~member
           ~contents:""
           ~checkpoint:ignore
       with
       | Error Authoring.Error.Unauthorized -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "denied supplied document admitted");
      Authoring.close_and_wait denied_host;
      let policy =
        Authoring.Document_policy.create ~authorize:(fun actor ->
          P.Id.Principal.equal
            (Operator_authorization.principal actor).id
            (Operator_authorization.principal allowed).id)
      in
      let host =
        Authoring.create
          sources
          ~sw
          ~read_policy:Authoring.Read_policy.deny_all
          ~document_policy:policy
          ~limits
      in
      let context, captured =
        Authoring.Document_context.open_supplied
          host
          ~actor:allowed
          ~member
          ~contents:"\255broken"
          ~checkpoint:ignore
        |> authoring_ok
      in
      [%test_eq: string]
        (Authoring.Document_capture.source captured |> Captured.contents)
        "\255broken";
      ignore
        (Authoring.Document_context.check_current context ~actor:allowed |> authoring_ok
         : unit);
      (match Authoring.Document_context.check_current context ~actor:denied with
       | Error Authoring.Error.Unauthorized -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok () -> failwith "foreign actor read pinned document bytes");
      (match Authoring.Document_context.origin context with
       | Authoring.Document_context.Origin.Supplied -> ()
       | Host_member -> failwith "supplied document gained root origin");
      (match
         Authoring.Document_context.list_directory
           context
           ~actor:allowed
           ~directory:None
           ~checkpoint:ignore
       with
       | Error Authoring.Error.Unsupported_source_selection -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "supplied document browsed host root");
      (match
         Authoring.Document_context.refresh context ~actor:allowed ~checkpoint:ignore
       with
       | Error Authoring.Error.Unsupported_source_selection -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "supplied document refreshed from host");
      (match
         Authoring.Document_context.refresh context ~actor:denied ~checkpoint:ignore
       with
       | Error Authoring.Error.Unauthorized -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "foreign actor observed supplied origin");
      (match
         Authoring.Document_context.open_supplied
           host
           ~actor:denied
           ~member
           ~contents:""
           ~checkpoint:ignore
       with
       | Error Authoring.Error.Unauthorized -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "foreign actor supplied bytes");
      Authoring.Document_context.close_and_wait context;
      (match Authoring.Document_context.check_current context ~actor:allowed with
       | Error Authoring.Error.Closed -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok () -> failwith "closed context read pinned document bytes");
      let current = ref true in
      let original =
        Operator_authorization.guarded
          ~principal:(Operator_authorization.principal allowed)
          ~is_current:(fun () -> !current)
      in
      let retained, _ =
        Authoring.Document_context.open_supplied
          host
          ~actor:original
          ~member
          ~contents:""
          ~checkpoint:ignore
        |> authoring_ok
      in
      current := false;
      (match Authoring.Document_context.check_current retained ~actor:allowed with
       | Error Authoring.Error.Expired_actor -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok () -> failwith "expired original actor read pinned document bytes");
      Authoring.Document_context.close_and_wait retained;
      Authoring.close_and_wait host))
;;

let%test_unit "nested project work cannot close active document context" =
  with_source_context (fun _ root project allowed _ _ host _ _ ->
    let member = Path.create "main.chatml" |> Or_error.ok_exn in
    let document, _ =
      Authoring.Document_context.open_host
        host
        ~actor:allowed
        ~root
        ~member
        ~checkpoint:ignore
      |> authoring_ok
    in
    Exn.protect
      ~finally:(fun () -> Authoring.Document_context.close_and_wait document)
      ~f:(fun () ->
        let entered = ref false in
        let refreshed =
          Authoring.Document_context.refresh
            document
            ~actor:allowed
            ~checkpoint:(fun () ->
              if not !entered
              then (
                entered := true;
                ignore
                  (Authoring.Context.read_member
                     project
                     ~actor:allowed
                     ~member
                     ~checkpoint:(fun () ->
                       match Authoring.Document_context.close_and_wait document with
                       | exception Invalid_argument _ -> ()
                       | () -> failwith "nested close self-joined")
                   |> authoring_ok
                   : string)))
          |> authoring_ok
        in
        assert !entered;
        [%test_eq: string]
          (Authoring.Document_capture.source refreshed |> Captured.contents)
          "let main = 1\n"))
;;

let%test_unit "same-context reentry rejects Busy before a second source borrow" =
  with_source_context (fun env _ context allowed _ _ _ _ _ ->
    let checked = ref false in
    let checkpoint () =
      if not !checked
      then (
        checked := true;
        let member = Path.create "main.chatml" |> Or_error.ok_exn in
        match
          Authoring.Context.read_member context ~actor:allowed ~member ~checkpoint:ignore
        with
        | Error Authoring.Error.Busy -> ()
        | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
        | Ok _ -> failwith "reentrant read admitted")
    in
    ignore
      (Authoring.Context.capture context ~actor:allowed ~clock:env#mono_clock ~checkpoint
       |> authoring_ok
       : Authoring.Capture.t);
    assert !checked)
;;

let%test_unit "concurrent read rejects Busy and close joins cancelled capture" =
  with_source_context (fun env _ context allowed _ _ _ _ _ ->
    Eio.Switch.run (fun sw ->
      let entered, resolve_entered = Eio.Promise.create () in
      let entered_once = ref false in
      let worker =
        Eio.Fiber.fork_promise ~sw (fun () ->
          try
            ignore
              (Authoring.Context.capture
                 context
                 ~actor:allowed
                 ~clock:env#mono_clock
                 ~checkpoint:(fun () ->
                   if not !entered_once
                   then (
                     entered_once := true;
                     Eio.Promise.resolve resolve_entered ();
                     Eio.Fiber.await_cancel ()))
               : (Authoring.Capture.t, Authoring.Error.t) Result.t);
            false
          with
          | Eio.Cancel.Cancelled _ -> true)
      in
      Eio.Promise.await entered;
      let member = Path.create "main.chatml" |> Or_error.ok_exn in
      (match
         Authoring.Context.read_member context ~actor:allowed ~member ~checkpoint:ignore
       with
       | Error Authoring.Error.Busy -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "concurrent read admitted");
      Authoring.Context.close_and_wait context;
      assert (Eio.Promise.await_exn worker);
      match
        Authoring.Context.read_member context ~actor:allowed ~member ~checkpoint:ignore
      with
      | Error Authoring.Error.Closed -> ()
      | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
      | Ok _ -> failwith "closed context admitted read"))
;;

let%test_unit "reentrant close rejects self-join and leaves operation usable" =
  with_source_context (fun env _ context allowed _ _ _ _ _ ->
    let checked = ref false in
    let checkpoint () =
      if not !checked
      then (
        checked := true;
        (match Authoring.Context.close_and_wait context with
         | exception Invalid_argument _ -> ()
         | () -> failwith "reentrant context close admitted");
        let member = Path.create "main.chatml" |> Or_error.ok_exn in
        match
          Authoring.Context.read_member context ~actor:allowed ~member ~checkpoint:ignore
        with
        | Error Authoring.Error.Busy -> ()
        | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
        | Ok _ -> failwith "reentrant read admitted")
    in
    ignore
      (Authoring.Context.capture context ~actor:allowed ~clock:env#mono_clock ~checkpoint
       |> authoring_ok
       : Authoring.Capture.t);
    assert !checked)
;;

let%test_unit "host switch release closes retained contexts" =
  with_source_context (fun _ _ _ allowed _ sources _ _ _ ->
    let retained =
      Eio.Switch.run (fun sw ->
        let limits =
          Authoring.Limits.create
            ~max_contexts:1
            ~max_context_requests:1
            ~max_manifest_entries:8
            ~max_browse_entries:8
            ~max_read_bytes:1024
            ~max_context_read_bytes:1024
          |> Or_error.ok_exn
        in
        let policy =
          Authoring.Read_policy.create ~authorize:(fun _ ~root_index ->
            Int.equal root_index 0)
        in
        let host =
          Authoring.create
            sources
            ~sw
            ~read_policy:policy
            ~document_policy:Authoring.Document_policy.deny_all
            ~limits
        in
        let root =
          Authoring.roots host ~actor:allowed ~checkpoint:ignore
          |> authoring_ok
          |> List.hd_exn
        in
        let project = Path.create "chatml.project.json" |> Or_error.ok_exn in
        let contract =
          Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:ignore
        in
        Authoring.Context.open_root
          host
          ~actor:allowed
          ~root
          ~dependency_roots:[]
          ~project
          ~selection:(Entry { name = "main"; environment = None })
          ~contract
        |> authoring_ok)
    in
    let member = Path.create "main.chatml" |> Or_error.ok_exn in
    match
      Authoring.Context.read_member retained ~actor:allowed ~member ~checkpoint:ignore
    with
    | Error Authoring.Error.Closed -> ()
    | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
    | Ok _ -> failwith "switch-released context admitted read")
;;

let%test_unit "root enumeration rechecks grant before publication" =
  with_source_context (fun _ _ _ allowed _ sources _ _ _ ->
    Eio.Switch.run (fun sw ->
      let calls = ref 0 in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun _ ~root_index:_ ->
          Int.incr calls;
          Int.equal !calls 1)
      in
      let limits =
        Authoring.Limits.create
          ~max_contexts:1
          ~max_context_requests:1
          ~max_manifest_entries:8
          ~max_browse_entries:8
          ~max_read_bytes:1024
          ~max_context_read_bytes:1024
        |> Or_error.ok_exn
      in
      let host =
        Authoring.create
          sources
          ~sw
          ~read_policy:policy
          ~document_policy:Authoring.Document_policy.deny_all
          ~limits
      in
      match Authoring.roots host ~actor:allowed ~checkpoint:ignore with
      | Error Authoring.Error.Unauthorized -> ()
      | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
      | Ok _ -> failwith "revoked root selection published"))
;;

let%test_unit "operator policy exception preserves identity" =
  with_source_context (fun _ _ _ allowed _ sources _ _ _ ->
    Eio.Switch.run (fun sw ->
      let exception Policy_raised in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun _ ~root_index:_ ->
          raise Policy_raised)
      in
      let limits =
        Authoring.Limits.create
          ~max_contexts:1
          ~max_context_requests:1
          ~max_manifest_entries:8
          ~max_browse_entries:8
          ~max_read_bytes:1024
          ~max_context_read_bytes:1024
        |> Or_error.ok_exn
      in
      let host =
        Authoring.create
          sources
          ~sw
          ~read_policy:policy
          ~document_policy:Authoring.Document_policy.deny_all
          ~limits
      in
      match Authoring.roots host ~actor:allowed ~checkpoint:ignore with
      | exception Policy_raised -> ()
      | exception exn -> raise exn
      | Ok _ -> failwith "policy exception swallowed"
      | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)))
;;

let%test_unit "active context cannot close its host" =
  with_source_context (fun env _ context allowed _ _ host _ _ ->
    let checked = ref false in
    let checkpoint () =
      if not !checked
      then (
        checked := true;
        match Authoring.close_and_wait host with
        | exception Invalid_argument _ -> ()
        | () -> failwith "host self-join admitted")
    in
    ignore
      (Authoring.Context.capture context ~actor:allowed ~clock:env#mono_clock ~checkpoint
       |> authoring_ok
       : Authoring.Capture.t);
    assert !checked)
;;

let%test_unit "host close cancels and joins a yielding root enumeration" =
  with_source_context (fun _ _ _ allowed _ sources _ _ _ ->
    Eio.Switch.run (fun sw ->
      let limits =
        Authoring.Limits.create
          ~max_contexts:1
          ~max_context_requests:1
          ~max_manifest_entries:8
          ~max_browse_entries:8
          ~max_read_bytes:1024
          ~max_context_read_bytes:1024
        |> Or_error.ok_exn
      in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun _ ~root_index:_ -> true)
      in
      let host =
        Authoring.create
          sources
          ~sw
          ~read_policy:policy
          ~document_policy:Authoring.Document_policy.deny_all
          ~limits
      in
      let entered, resolve_entered = Eio.Promise.create () in
      let worker =
        Eio.Fiber.fork_promise ~sw (fun () ->
          try
            ignore
              (Authoring.roots host ~actor:allowed ~checkpoint:(fun () ->
                 Eio.Promise.resolve resolve_entered ();
                 Eio.Fiber.await_cancel ())
               : (Authoring.Root.t list, Authoring.Error.t) Result.t);
            false
          with
          | Eio.Cancel.Cancelled _ -> true)
      in
      Eio.Promise.await entered;
      (match Authoring.roots host ~actor:allowed ~checkpoint:ignore with
       | Error Authoring.Error.Busy -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "concurrent root enumeration admitted");
      Authoring.close_and_wait host;
      assert (Eio.Promise.await_exn worker);
      match Authoring.roots host ~actor:allowed ~checkpoint:ignore with
      | Error Authoring.Error.Closed -> ()
      | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
      | Ok _ -> failwith "closed host admitted root enumeration"))
;;

let%test_unit "root enumeration cannot self-close its host" =
  with_source_context (fun _ _ _ allowed _ sources _ _ _ ->
    Eio.Switch.run (fun sw ->
      let limits =
        Authoring.Limits.create
          ~max_contexts:1
          ~max_context_requests:1
          ~max_manifest_entries:8
          ~max_browse_entries:8
          ~max_read_bytes:1024
          ~max_context_read_bytes:1024
        |> Or_error.ok_exn
      in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun _ ~root_index:_ -> true)
      in
      let host =
        Authoring.create
          sources
          ~sw
          ~read_policy:policy
          ~document_policy:Authoring.Document_policy.deny_all
          ~limits
      in
      let checked = ref false in
      let checkpoint () =
        if not !checked
        then (
          checked := true;
          match Authoring.close_and_wait host with
          | exception Invalid_argument _ -> ()
          | () -> failwith "root enumeration self-join admitted")
      in
      let roots = Authoring.roots host ~actor:allowed ~checkpoint |> authoring_ok in
      assert !checked;
      [%test_eq: int] (List.length roots) 1))
;;

let%test_unit "nested source contexts cannot hide an outer self-join" =
  with_source_context (fun env _ outer allowed _ _ _ data root_path ->
    Eio.Switch.run (fun sw ->
      let configuration =
        Configuration.{ external_roots = [ Eio.Path.native_exn root_path ]; git = None }
      in
      let sources = open_host ~configuration env data sw in
      let limits =
        Authoring.Limits.create
          ~max_contexts:1
          ~max_context_requests:2
          ~max_manifest_entries:8
          ~max_browse_entries:8
          ~max_read_bytes:1024
          ~max_context_read_bytes:1024
        |> Or_error.ok_exn
      in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun _ ~root_index ->
          Int.equal root_index 0)
      in
      let host =
        Authoring.create
          sources
          ~sw
          ~read_policy:policy
          ~document_policy:Authoring.Document_policy.deny_all
          ~limits
      in
      let root =
        Authoring.roots host ~actor:allowed ~checkpoint:ignore
        |> authoring_ok
        |> List.hd_exn
      in
      let project = Path.create "chatml.project.json" |> Or_error.ok_exn in
      let contract =
        Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:ignore
      in
      let inner =
        Authoring.Context.open_root
          host
          ~actor:allowed
          ~root
          ~dependency_roots:[]
          ~project
          ~selection:(Entry { name = "main"; environment = None })
          ~contract
        |> authoring_ok
      in
      Exn.protect
        ~finally:(fun () ->
          Authoring.Context.close_and_wait inner;
          Authoring.close_and_wait host;
          Host.close_and_wait sources)
        ~f:(fun () ->
          let checked = ref false in
          let outer_checkpoint () =
            if not !checked
            then (
              checked := true;
              let inner_checkpoint () =
                match Authoring.Context.close_and_wait outer with
                | exception Invalid_argument _ -> ()
                | () -> failwith "nested call closed its active ancestor"
              in
              ignore
                (Authoring.Context.capture
                   inner
                   ~actor:allowed
                   ~clock:env#mono_clock
                   ~checkpoint:inner_checkpoint
                 |> authoring_ok
                 : Authoring.Capture.t))
          in
          ignore
            (Authoring.Context.capture
               outer
               ~actor:allowed
               ~clock:env#mono_clock
               ~checkpoint:outer_checkpoint
             |> authoring_ok
             : Authoring.Capture.t);
          assert !checked)))
;;

let%test_unit "owner switch release joins externally switched source work" =
  with_source_context (fun env _ _ allowed _ sources _ _ _ ->
    List.iter [ `Roots; `Capture; `Document_open ] ~f:(fun mode ->
      Eio.Switch.run (fun caller_sw ->
        let worker =
          Eio.Switch.run (fun owner_sw ->
            let limits =
              Authoring.Limits.create
                ~max_contexts:1
                ~max_context_requests:1
                ~max_manifest_entries:8
                ~max_browse_entries:8
                ~max_read_bytes:1024
                ~max_context_read_bytes:1024
              |> Or_error.ok_exn
            in
            let policy =
              Authoring.Read_policy.create ~authorize:(fun _ ~root_index ->
                Int.equal root_index 0)
            in
            let document_policy =
              match mode with
              | `Document_open ->
                Authoring.Document_policy.create ~authorize:(fun _ -> true)
              | `Roots | `Capture -> Authoring.Document_policy.deny_all
            in
            let host =
              Authoring.create
                sources
                ~sw:owner_sw
                ~read_policy:policy
                ~document_policy
                ~limits
            in
            let context =
              match mode with
              | `Roots | `Document_open -> None
              | `Capture ->
                let root =
                  Authoring.roots host ~actor:allowed ~checkpoint:ignore
                  |> authoring_ok
                  |> List.hd_exn
                in
                let project = Path.create "chatml.project.json" |> Or_error.ok_exn in
                let contract =
                  Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:ignore
                in
                Some
                  (Authoring.Context.open_root
                     host
                     ~actor:allowed
                     ~root
                     ~dependency_roots:[]
                     ~project
                     ~selection:(Entry { name = "main"; environment = None })
                     ~contract
                   |> authoring_ok)
            in
            let entered, resolve_entered = Eio.Promise.create () in
            let worker =
              Eio.Fiber.fork_promise ~sw:caller_sw (fun () ->
                try
                  let checkpoint () =
                    Eio.Promise.resolve resolve_entered ();
                    Eio.Fiber.await_cancel ()
                  in
                  (match mode, context with
                   | `Roots, None ->
                     ignore
                       (Authoring.roots host ~actor:allowed ~checkpoint
                        : (Authoring.Root.t list, Authoring.Error.t) Result.t)
                   | `Capture, Some context ->
                     ignore
                       (Authoring.Context.capture
                          context
                          ~actor:allowed
                          ~clock:env#mono_clock
                          ~checkpoint
                        : (Authoring.Capture.t, Authoring.Error.t) Result.t)
                   | `Document_open, None ->
                     let member = Path.create "draft.chatmd" |> Or_error.ok_exn in
                     ignore
                       (Authoring.Document_context.open_supplied
                          host
                          ~actor:allowed
                          ~member
                          ~contents:"draft"
                          ~checkpoint
                        : ( Authoring.Document_context.t * Authoring.Document_capture.t
                            , Authoring.Error.t )
                            Result.t)
                   | _ -> failwith "invalid lifecycle case");
                  false
                with
                | Eio.Cancel.Cancelled _ -> true)
            in
            Eio.Promise.await entered;
            worker, host)
        in
        let worker, host = worker in
        assert (Eio.Promise.await_exn worker);
        match Authoring.roots host ~actor:allowed ~checkpoint:ignore with
        | Error Authoring.Error.Closed -> ()
        | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
        | Ok _ -> failwith "switch-released host admitted more work")))
;;

let%test_unit "grant identity, actor lifetime and read budgets fail closed" =
  with_source_context (fun _ foreign_root _ allowed _ sources _ _ root_path ->
    Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root_path / "tiny.chatml") "abc";
    Eio.Switch.run (fun sw ->
      let limits =
        Authoring.Limits.create
          ~max_contexts:2
          ~max_context_requests:2
          ~max_manifest_entries:8
          ~max_browse_entries:8
          ~max_read_bytes:4
          ~max_context_read_bytes:3
        |> Or_error.ok_exn
      in
      let policy =
        Authoring.Read_policy.create ~authorize:(fun _ ~root_index ->
          Int.equal root_index 0)
      in
      let host =
        Authoring.create
          sources
          ~sw
          ~read_policy:policy
          ~document_policy:Authoring.Document_policy.deny_all
          ~limits
      in
      let root =
        Authoring.roots host ~actor:allowed ~checkpoint:ignore
        |> authoring_ok
        |> List.hd_exn
      in
      let project = Path.create "chatml.project.json" |> Or_error.ok_exn in
      let contract =
        Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:ignore
      in
      let open_context root actor =
        Authoring.Context.open_root
          host
          ~actor
          ~root
          ~dependency_roots:[]
          ~project
          ~selection:(Entry { name = "main"; environment = None })
          ~contract
      in
      (match open_context foreign_root allowed with
       | Error Authoring.Error.Foreign_root -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "foreign root admitted");
      let current = ref true in
      let original =
        Operator_authorization.guarded
          ~principal:(Operator_authorization.principal allowed)
          ~is_current:(fun () -> !current)
      in
      let stale_root =
        Authoring.roots host ~actor:original ~checkpoint:ignore
        |> authoring_ok
        |> List.hd_exn
      in
      current := false;
      (match open_context stale_root allowed with
       | Error Authoring.Error.Expired_actor -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "stale original proof admitted");
      let large = open_context root allowed |> authoring_ok in
      let aggregate = open_context root allowed |> authoring_ok in
      let main = Path.create "main.chatml" |> Or_error.ok_exn in
      let tiny = Path.create "tiny.chatml" |> Or_error.ok_exn in
      let expect_too_large context member =
        match
          Authoring.Context.read_member context ~actor:allowed ~member ~checkpoint:ignore
        with
        | Error (Authoring.Error.Local error)
          when Chatml_local_source.Error.equal_code
                 (Chatml_local_source.Error.code error)
                 Chatml_local_source.Error.Too_large -> ()
        | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
        | Ok _ -> failwith "oversized read admitted"
      in
      expect_too_large large main;
      expect_too_large large main;
      (match
         Authoring.Context.read_member
           large
           ~actor:allowed
           ~member:tiny
           ~checkpoint:ignore
       with
       | Error Authoring.Error.Result_limit -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "request cap bypassed");
      [%test_eq: string]
        (Authoring.Context.read_member
           aggregate
           ~actor:allowed
           ~member:tiny
           ~checkpoint:ignore
         |> authoring_ok)
        "abc";
      (match
         Authoring.Context.read_member
           aggregate
           ~actor:allowed
           ~member:tiny
           ~checkpoint:ignore
       with
       | Error Authoring.Error.Result_limit -> ()
       | Error error -> Error.raise_s (Authoring.Error.sexp_of_t error)
       | Ok _ -> failwith "aggregate read cap bypassed");
      Authoring.close_and_wait host))
;;
