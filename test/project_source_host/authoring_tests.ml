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

let with_source_context f =
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
          ~max_context_requests:8
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

let%test_unit "shared overlay preparation uses actual host contract and a new lock" =
  with_source_context (fun env _ owner allowed _ _ _ _ _ ->
    let module Ledger = Chatml_authoring_context in
    let module Core_owner = Agent_server.Authoring_context_service_core in
    let module Overlay = Agent_server.Authoring_project_overlay in
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
    let context =
      Agent_server.Authoring_context_activity.
        { id
        ; identity
        ; original_actor = allowed
        ; origin
        ; owner = Host_project owner
        ; view
        }
    in
    let current =
      Core_owner.Current.
        { owner = ledger
        ; domain = (Stdlib.Domain.self () :> int)
        ; context
        ; expected = P.Authoring.Context.Expected.of_view view
        ; live = ref true
        ; retained_document_capture = None
        }
    in
    let limits =
      Chatml_project_build.Limits.create
        ~max_work_steps:1_000_000
        ~max_input_bytes:(Some 100_000)
        ~document:Document_schema.Limits.default
      |> Or_error.ok_exn
    in
    let original =
      Overlay.prepare current ~limits ~checkpoint:ignore
      |> function
      | Ok value -> value
      | Error _ -> failwith "authentic unchanged overlay was rejected"
    in
    assert (
      not
        (Captured.Id.equal
           (Captured.id
              (Chatml_project_build.lock_source (Overlay.Prepared.build original)))
           (Captured.id (Chatml_project_build.lock_source build))));
    assert (
      Captured.Digest.equal
        (Chatml.Chatml_compiler_contract.fingerprint
           (Overlay.Prepared.contract original)
           ~checkpoint:ignore)
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
      Overlay.prepare { current with owner = edited } ~limits ~checkpoint:ignore
      |> function
      | Ok value -> value
      | Error _ -> failwith "authentic changed overlay was rejected"
    in
    let selected_main =
      Prepared.sources (Chatml_project_build.prepared (Overlay.Prepared.build changed))
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
        let module Core_owner = Agent_server.Authoring_context_service_core in
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
        let inputs = Ledger.document_inputs ledger ~identity ~checkpoint:ignore |> ok in
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
        let context =
          Agent_server.Authoring_context_activity.
            { id
            ; identity
            ; original_actor = allowed
            ; origin
            ; owner = Host_document owner
            ; view
            }
        in
        let retained =
          Core_owner.Retained_document_capture.prepare
            ~context
            ~inputs
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
          Core_owner.Retained_document_capture.prepare
            ~context
            ~inputs
            ~capture:duplicate_capture
            ~max_sources:2
            ~max_bytes:2048
            ~checkpoint:ignore
          |> ok
        in
        assert (
          not
            (Core_owner.Retained_document_capture.same_head
               (Some retained)
               (Some duplicate)));
        Eio.Switch.run (fun sw ->
          let module Owner = Agent_server.Authoring_context_owner in
          let principal = Operator_authorization.principal allowed in
          let connection =
            Agent_server.Connection_context.create_authenticated
              ~actor:allowed
              ~connection_id:"retained capture head"
              ~transport:In_memory
              ~publish_notification:(fun _ -> ())
              ~max_attachments:1
          in
          let analysis_policy =
            Chatml_authoring_analysis.Policy.create
              ~compilation:Chatml_compilation.default_limits
              ~semantic:
                (Chatml_semantics.Limits.create
                   ~max_input_bytes:2048
                   ~recording:
                     (Chatml_typechecker.Semantic.Limits.create
                        ~max_facts:100
                        ~max_edges:100
                        ~max_label_bytes:2048
                        ~max_work_steps:1000
                      |> Or_error.ok_exn)
                   ~type_graph:
                     (Chatml_semantics.Frozen.Limits.create
                        ~max_nodes:100
                        ~max_edges:100
                        ~max_label_bytes:2048
                      |> Or_error.ok_exn)
                   ~max_published_facts:100
                   ~max_published_edges:100
                   ~max_published_label_bytes:2048
                   ~max_work_steps:1000
                 |> Or_error.ok_exn)
              ~max_dependency_depth:2
              ~max_results:1
              ~max_projected_diagnostics:2
              ~max_projected_sites:2
              ~max_projection_sources:2
              ~max_position_index_entries:16
              ~max_projection_source_bytes:2048
            |> Or_error.ok_exn
          in
          let service =
            Core_owner.create
              ~sw
              ~clock:env#mono_clock
              ~host:(P.Authoring.Context.host view)
              ~sources:(Some host)
              ~upload:None
              ~supplied_document:None
              ~selection_policy:
                (Core_owner.Selection_policy.create
                   ~project_contract:(fun _ ~root:_ ~project:_ ~selection:_ -> None)
                   ~document_static:(fun _ ~requested_environment:_ -> None))
              ~context_limits:
                (Ledger.Limits.create
                   ~max_contexts:1
                   ~max_sources:2
                   ~max_open_documents:1
                   ~max_document_bytes:1024
                   ~max_effective_input_bytes:2048
                   ~max_context_source_bytes:2048
                   ~max_total_source_bytes:4096
                 |> Or_error.ok_exn)
              ~analysis_policy
              ~protocol_limits:
                (P.Authoring.Limits.create
                   ~max_sources:2
                   ~max_source_bytes:1024
                   ~max_context_source_bytes:2048
                   ~max_request_bytes:4096
                   ~max_result_bytes:4096
                   ~max_path_candidates:4
                 |> ok)
              ~limits:
                (Core_owner.Limits.create
                   ~max_connections:1
                   ~max_active_requests_per_connection:1
                   ~max_roots_per_connection:1
                   ~max_response_bytes:4096
                 |> Or_error.ok_exn)
          in
          let response =
            P.Initialize.Response.create
              ~protocol_name:"ochat.agent"
              ~selected_version:(P.Version.create ~major:1 ~minor:0 |> ok)
              ~implementation:
                (P.Initialize.Implementation.create ~name:"capture head" ~version:"1"
                 |> ok)
              ~server_id:(P.Authoring.Context.host view)
              ~enabled_features:
                [ P.Authoring.Features.core; P.Authoring.Features.semantic ]
              ~extensions:None
              ~principal
              ~limits:
                { max_request_bytes = 4096
                ; max_event_bytes = 1024
                ; max_page_size = 4
                ; max_attachments_per_connection = 1
                }
              ~event_retention:
                { minimum_age_ms = 0
                ; maximum_events = 1
                ; oldest_replayable_sequence = None
                }
              ~timing:
                { heartbeat_interval_ms = 1000
                ; owner_lease_duration_ms = 10000
                ; owner_renew_after_ms = 5000
                ; disconnect_grace_default_ms = 0
                }
              ~server_time:(P.Timestamp.of_string "2026-10-13T00:00:00Z" |> ok)
            |> ok
          in
          Core_owner.publish_initialize_and_wait service ~connection ~response |> ok;
          Core_owner.with_gate service (fun () ->
            let entry =
              Core_owner.find_connection service connection |> Option.value_exn
            in
            entry.contexts <- [ context ];
            service.project_cache
            <- Chatml_authoring_analysis.Project_cache.create
                 ~contexts:ledger
                 ~policy:analysis_policy;
            service.document_cache
            <- Chatmd_authoring_analysis.Document_cache.transition
                 service.document_cache
                 ~contexts:ledger;
            service.document_capture <- Some retained);
          let published = ref 0 in
          let result =
            Owner.with_current
              ~feature:Core_owner.Selected_feature.Semantic
              ~publish:(fun value ->
                incr published;
                Ok value)
              service
              ~actor:allowed
              ~connection
              ~expected:(P.Authoring.Context.Expected.of_view view)
              ~checkpoint:ignore
              ~f:(fun current ~checkpoint:_ ->
                assert (
                  Option.is_some
                    (Core_owner.Current.document_capture current ~checkpoint:ignore |> ok));
                Core_owner.with_gate service (fun () ->
                  service.document_capture <- Some duplicate);
                Ok ())
          in
          (match result with
           | Error Core_owner.Error.Stale_context -> ()
           | Ok _ | Error _ -> failwith "replaced capture head published stale result");
          [%test_eq: int] !published 0);
        let current =
          Core_owner.Current.
            { owner = ledger
            ; domain = (Stdlib.Domain.self () :> int)
            ; context
            ; expected = P.Authoring.Context.Expected.of_view view
            ; live = ref true
            ; retained_document_capture = Some retained
            }
        in
        assert (
          Option.is_some
            (Core_owner.Current.document_capture current ~checkpoint:ignore |> ok));
        assert (
          Core_owner.Retained_document_capture.matches_current retained ~context ~inputs);
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
        let edited =
          Ledger.document_inputs edited_ledger ~identity ~checkpoint:ignore |> ok
        in
        let edited_current = { current with owner = edited_ledger } in
        assert (
          Option.is_none
            (Core_owner.Current.document_capture edited_current ~checkpoint:ignore |> ok));
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          Eio.Path.(root_path / "part.chatmd")
          "<developer>changed host dependency</developer>";
        assert (
          not
            (Core_owner.Retained_document_capture.matches_current
               retained
               ~context
               ~inputs:edited));
        let recomposed =
          Core_owner.Current.frozen_document_bundle edited_current ~checkpoint:ignore
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
        let unknown_owner = { context with id = P.Id.Authoring_context.create () } in
        assert (
          not
            (Core_owner.Retained_document_capture.matches_import_owner
               retained
               ~context:unknown_owner
               ~inputs:edited));
        assert (
          Option.is_none
            (Core_owner.Current.frozen_document_bundle
               { edited_current with context = unknown_owner }
               ~checkpoint:ignore
             |> ok));
        current.live := false;
        match Core_owner.Current.frozen_document_bundle current ~checkpoint:ignore with
        | Error Core_owner.Error.Stale_context -> ()
        | Ok _ | Error _ -> failwith "expired Current retained captured imports"))
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
