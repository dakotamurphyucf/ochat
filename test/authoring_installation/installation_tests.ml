open! Core
module P = Agent_protocol
module S = Agent_server
module Q = P.Authoring.Analysis.Query
module B = P.Authoring_bundle.Bundle

let () = Mirage_crypto_rng_unix.use_default ()

let ok = function
  | Ok value -> value
  | Error failure -> raise_s [%sexp (failure : P.Error.t)]
;;

let with_daemon
      ?(options = { S.Daemon.default_options with startup_mode = Operator_only })
      ?(prepare_options = fun ~env:_ options -> options)
      ?(configure = fun ~env:_ ~root:_ config -> config)
      f
  =
  Eio_main.run (fun env ->
    let root =
      Filename.concat "/tmp" ("ochat-ai-" ^ P.Id.Transaction.(create () |> to_string))
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / root) in
    Eio.Path.mkdir ~perm:0o700 path;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path)
      ~f:(fun () ->
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(path / "prompt.chatmd")
          "<developer>Authoring installation fixture.</developer>";
        Eio.Switch.run (fun sw ->
          let config =
            Agent_server_test_support.config
              root
              root
              (Filename.concat root "prompt.chatmd")
            |> configure ~env ~root
          in
          let daemon =
            S.Daemon.start
              ~sw
              ~env
              ~config
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options:(prepare_options ~env options)
              ()
            |> ok
          in
          Fun.protect
            ~finally:(fun () -> S.Daemon.shutdown daemon |> ok)
            (fun () -> f env sw root daemon))))
;;

let connect ?actor daemon =
  let actor =
    Option.value
      actor
      ~default:
        (Operator_authorization.trusted_local (Agent_server_test_support.principal ()))
  in
  let context =
    S.Connection_context.create_authenticated
      ~actor
      ~connection_id:P.Id.Transaction.(create () |> to_string)
      ~transport:In_memory
      ~publish_notification:(fun _ -> ())
      ~max_attachments:8
  in
  let transport =
    Agent_client.In_memory.create
      ~notifications:(Eio.Stream.create 8)
      ~request:(S.Dispatcher.dispatch_command (S.Daemon.dispatcher daemon) ~context)
      ~close:(fun () -> S.Daemon.close_connection daemon context)
  in
  context, transport
;;

let initialize transport features =
  let request =
    P.Initialize.Request.create
      ~implementation:
        (P.Initialize.Implementation.create ~name:"authoring" ~version:"1" |> ok)
      ~protocol_min:P.Version.current
      ~protocol_max:P.Version.current
      ~features
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(1024 * 1024)
      ()
    |> ok
  in
  match
    Agent_client.Connection.request_without_history
      transport
      (Protocol_initialize request)
  with
  | Ok (Protocol_initialize response) -> response
  | Ok _ -> failwith "unexpected initialize result"
  | Error failure -> raise_s [%sexp (failure : P.Error.t)]
;;

let authoring transport request =
  match Agent_client.Connection.request_without_history transport (Authoring request) with
  | Ok (Authoring response) -> Ok response
  | Ok _ -> failwith "unexpected authoring result"
  | Error failure -> Error failure
;;

let upload_limits () =
  Agent_store.Authoring_upload_store.Limits.create
    ~max_active_uploads:4
    ~max_active_per_principal:2
    ~max_members:4
    ~max_manifest_bytes:65536
    ~max_member_bytes:128
    ~max_bundle_bytes:256
    ~max_chunk_bytes:64
    ~max_reserved_bytes:512
    ~max_reserved_per_principal:256
    ~max_published_records:4
    ~max_installed_sources:32
    ~max_installed_bytes_per_bundle:128
    ~max_installed_bytes:256
    ~max_installed_per_principal:256
    ~max_publication_metadata_bytes:16384
    ~max_intent_bytes:8192
    ~max_marker_bytes:2048
    ~max_work_steps:10000
    ~max_journal_records:32
    ~max_journal_bytes:16384
    ~max_idempotency_records:8
    ~max_tombstones:8
    ~max_tombstone_bytes:600000
    ~max_recovery_entries:1000
    ~max_recovery_metadata_bytes:1048576
    ~max_recovery_source_bytes:1024
    ~staging_ttl:(Time_ns.Span.of_sec 60.)
    ~receipt_ttl:(Time_ns.Span.of_sec 120.)
    ~expired_tombstone_ttl:(Time_ns.Span.of_sec 180.)
  |> Or_error.ok_exn
;;

let upload_document () =
  let contents = "<developer>Uploaded authoring source.</developer>" in
  let id =
    Chatml_source.Id.create ~package_namespace:"uploaded" ~relative_path:"entry.chatmd"
    |> Or_error.ok_exn
  in
  let source =
    P.Authoring.Source.create
      ~id
      ~digest:(Chatml_source.Digest.of_contents contents ~checkpoint:Fn.id)
      ~byte_length:(String.length contents)
      ~editor_revision:None
    |> ok
  in
  let member = B.Member.create ~source |> ok in
  let document_member =
    B.Manifest.Document_member.create
      ~source:id
      ~member:(Source.Portable_path.create "entry.chatmd" |> Or_error.ok_exn)
    |> ok
  in
  let basis =
    B.Manifest.Chatmd_document.create
      ~root_source:id
      ~document_members:[ document_member ]
      ~scripts:[]
    |> ok
  in
  let manifest =
    B.Manifest.create ~basis:(Chatmd_document basis) ~members:[ member ] |> ok
  in
  id, contents, manifest
;;

let configured_upload_access mode grants =
  S.Authoring_upload_access_configuration.
    { mode
    ; grants =
        List.map grants ~f:(fun operation ->
          Grant.
            { principal_id = (Agent_server_test_support.principal ()).id
            ; authentication_kind = "test"
            ; operation
            })
    ; selections = []
    }
;;

let publish_document ?parent transport ~key =
  let id, contents, manifest = upload_document () in
  let begin_key = P.Idempotency_key.of_string (key ^ "-begin") |> ok in
  let begin_request =
    B.Begin.create_request ?parent_context:parent ~idempotency_key:begin_key ~manifest ()
  in
  let upload_id =
    match authoring transport (Bundle_begin begin_request) |> ok with
    | Bundle_begin response -> B.Begin.upload_id response
    | _ -> failwith "configured upload did not begin"
  in
  let chunk =
    B.Chunk.create_request ~upload_id ~source:id ~byte_offset:0 ~bytes:contents |> ok
  in
  (match authoring transport (Bundle_chunk chunk) |> ok with
   | Bundle_chunk _ -> ()
   | _ -> failwith "configured upload did not accept bytes");
  let commit_key = P.Idempotency_key.of_string (key ^ "-commit") |> ok in
  let commit = B.Commit.create_request ~upload_id ~idempotency_key:commit_key in
  (match authoring transport (Bundle_commit commit) |> ok with
   | Bundle_commit response ->
     (match B.Commit.outcome response with
      | Published _ -> ()
      | Accepted_for_validation | Rejected _ ->
        failwith "configured upload did not publish")
   | _ -> failwith "configured upload returned another method");
  (match
     authoring transport (Bundle_status (B.Status.create_request ~upload_id)) |> ok
   with
   | Bundle_status status ->
     (match B.Status.state status with
      | Published _ -> ()
      | _ -> failwith "configured publication was not durable")
   | _ -> failwith "configured status returned another method");
  Option.iter parent ~f:(fun _ ->
    match authoring transport (Bundle_commit commit) |> ok with
    | Bundle_commit response ->
      (match B.Commit.outcome response with
       | Published _ -> ()
       | Accepted_for_validation | Rejected _ ->
         failwith "parent terminal retry lost its durable receipt")
    | _ -> failwith "parent terminal retry returned another method");
  commit
;;

let project_upload () =
  let module Project_fixture = Chatml_project_build_tests.Authoring_bundle_tests in
  let module Support = Chatml_project_build_tests.Support in
  let module Build = Chatml_project_build in
  let contract =
    Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:Fn.id
  in
  let declaration =
    Support.capture
      ~namespace:"uploaded"
      "chatml.project.json"
      {|{"schema_version":1,"namespace":"uploaded","source_dirs":["src"],"entries":{"main":{"source":"src/main.chatml","target":"one_off_v1"}}}|}
  in
  let main =
    Support.capture
      ~namespace:"uploaded"
      "src/main.chatml"
      "let main input = Task.pure(input)\n"
  in
  let catalog =
    Support.catalog
      declaration
      ~request:(Entry { name = "main"; environment = None })
      ~members:[ main ]
  in
  let root =
    Build.Catalog_input.create
      catalog
      ~origin:Local
      ~acquisition_key:"uploaded-project-test"
      ~dependencies:[]
  in
  let limits = Support.limits () in
  let gate, _ =
    Build.Selection_gate.create Initial_prepare ~limits ~checkpoint:Fn.id |> Support.ok
  in
  let original =
    Build.compose
      root
      ~catalogs:[]
      ~entry:Declared
      ~selection_gate:gate
      ~contract
      ~lock_id:(Support.id ~namespace:"uploaded" "original.lock.json")
      ~limits
      ~checkpoint:Fn.id
    |> Support.ok
  in
  let sources = Support.without_lock original in
  let project =
    Project_fixture.project
      original
      ~source_members:sources
      ~selection:(Project_fixture.selection (Entry "main"))
      ~entry:Declared
      ~mode:
        (Compose
           { selected_previous_lock = None
           ; generated_lock =
               Chatml_source.Id.create
                 ~package_namespace:"uploaded"
                 ~relative_path:"installed.lock.json"
               |> Or_error.ok_exn
           })
  in
  let manifest = Project_fixture.manifest project sources in
  let environment =
    Chatml_prepared_program.Environment.name
      (Chatml_prepared_program.environment (Chatml_project_build.prepared original))
  in
  manifest, sources, environment
;;

let moderator_upload () =
  let module Project_fixture = Chatml_project_build_tests.Authoring_bundle_tests in
  let module Build = Chatml_project_build in
  let module Support = Chatml_project_build_tests.Support in
  let contract =
    Chatml.Chatml_compiler_contract.for_target Moderator_v1 ~checkpoint:Fn.id
  in
  let declaration =
    Support.capture
      ~namespace:"role"
      "chatml.project.json"
      {|{"schema_version":1,"namespace":"role","source_dirs":["src"],"environments":{"default":{"modules":{"Counter":"self.Counter"}}}}|}
  in
  let counter =
    Support.capture ~namespace:"role" "src/counter.chatml" "let initial = 0"
  in
  let interface =
    Support.capture ~namespace:"role" "src/counter.chatmli" "val initial : int"
  in
  let entry =
    Support.capture
      ~namespace:"role"
      "moderator.chatml"
      "let initial_state = Counter.initial let on_event ctx state event = \
       Task.pure(state)"
  in
  let catalog =
    Support.catalog
      declaration
      ~request:(Library { environment = None })
      ~members:[ counter; interface ]
  in
  let root =
    Build.Catalog_input.create
      catalog
      ~origin:Local
      ~acquisition_key:"minimal-moderator"
      ~dependencies:[]
  in
  let limits = Support.limits () in
  let gate, _ =
    Build.Selection_gate.create Initial_prepare ~limits ~checkpoint:Fn.id |> Support.ok
  in
  let original =
    Build.compose
      root
      ~catalogs:[]
      ~entry:(Captured entry)
      ~selection_gate:gate
      ~contract
      ~lock_id:(Support.id ~namespace:"role" "moderator.lock.json")
      ~limits
      ~checkpoint:Fn.id
    |> Support.ok
  in
  let sources = Support.without_lock original in
  let entry = Project_fixture.find_entry original in
  let project =
    Project_fixture.project
      original
      ~source_members:sources
      ~selection:(Project_fixture.selection Library)
      ~entry:(Captured (Project_fixture.captured_entry entry))
      ~mode:
        (Compose
           { selected_previous_lock = None
           ; generated_lock =
               Chatml_source.Id.create
                 ~package_namespace:"role"
                 ~relative_path:"installed.lock.json"
               |> Or_error.ok_exn
           })
  in
  let environment =
    Chatml_prepared_program.Environment.name
      (Chatml_prepared_program.environment (Chatml_project_build.prepared original))
  in
  Project_fixture.manifest project sources, sources, environment, original
;;

let moderator_followup original sources previous_lock =
  let module Project_fixture = Chatml_project_build_tests.Authoring_bundle_tests in
  let module Support = Chatml_project_build_tests.Support in
  let entry = Project_fixture.find_entry original in
  let project =
    Project_fixture.project
      original
      ~source_members:sources
      ~selection:(Project_fixture.selection Library)
      ~entry:(Captured (Project_fixture.captured_entry entry))
      ~mode:
        (Compose
           { selected_previous_lock = Some previous_lock
           ; generated_lock = Support.id ~namespace:"role" "followup.lock.json"
           })
  in
  Project_fixture.manifest project sources
;;

let selected_document_project ~namespace () =
  let module Support = Chatml_project_build_tests.Support in
  let module Project_fixture = Chatml_project_build_tests.Authoring_bundle_tests in
  let module Build = Chatml_project_build in
  let contract =
    Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint:Fn.id
  in
  let declaration =
    Support.capture
      ~namespace
      "chatml.project.json"
      {|{"schema_version":1,"namespace":"selected","source_dirs":[]}|}
  in
  let entry =
    Support.capture ~namespace "entry.chatml" "let main input = Task.pure(input)\n"
  in
  let catalog =
    Support.catalog declaration ~request:(Library { environment = None }) ~members:[]
  in
  let root =
    Build.Catalog_input.create
      catalog
      ~origin:Local
      ~acquisition_key:"selected-document-parent"
      ~dependencies:[]
  in
  let limits = Support.limits () in
  let gate, _ =
    Build.Selection_gate.create Initial_prepare ~limits ~checkpoint:Fn.id |> Support.ok
  in
  let build =
    Build.compose
      root
      ~catalogs:[]
      ~entry:(Captured entry)
      ~selection_gate:gate
      ~contract
      ~lock_id:(Support.id ~namespace "original.lock.json")
      ~limits
      ~checkpoint:Fn.id
    |> Support.ok
  in
  let environment =
    Chatml_prepared_program.Environment.name
      (Chatml_prepared_program.environment (Build.prepared build))
  in
  build, contract, declaration, entry, environment
;;

let selected_document_upload
      ~dir
      ~document_source
      ~document_bytes
      ~build
      ~declaration
      ~entry
  =
  let module Project_fixture = Chatml_project_build_tests.Authoring_bundle_tests in
  let module Support = Chatml_project_build_tests.Support in
  let document =
    Chatml_source.create
      document_source
      ~contents:document_bytes
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint:Fn.id
    |> Or_error.ok_exn
  in
  let parsed =
    Prompt.Chat_markdown.parse_source_bundle
      ~dir
      (Chatmd_source_bundle.create
         ~root_file:"selected.chatmd"
         ~sources:[ "selected.chatmd", document_bytes ]
         ()
       |> Result.ok_or_failwith)
  in
  let declaration_script =
    List.hd_exn (Prompt.Chat_markdown.Parsed.declarations parsed.parsed_root)
  in
  let binding =
    Chatml_prompt_programs.Binding.of_declaration
      parsed.parsed_root
      declaration_script
      ~checkpoint:Fn.id
    |> Or_error.ok_exn
  in
  let selector =
    B.Manifest.Script.Selector.create
      ~registration_source:document_source
      ~declaration_source:document_source
      ~start_byte:(Chatml_prompt_programs.Binding.start_byte binding)
      ~end_byte:(Chatml_prompt_programs.Binding.end_byte binding)
      ~script_id:(Chatml_prompt_programs.Binding.script_id binding)
      ~body_digest:(Chatml_prompt_programs.Binding.script_digest binding)
      ~body_byte_length:
        (String.length (Prompt.Chat_markdown.Declaration.script_body declaration_script))
    |> ok
  in
  let project =
    Project_fixture.project
      build
      ~source_members:[ declaration ]
      ~selection:(Project_fixture.selection Library)
      ~entry:
        (Captured
           (B.Manifest.Project.Captured_entry.create
              ~source:(Project_fixture.metadata entry)
              ~delivery:Derived_script
            |> ok))
      ~mode:
        (Compose
           { selected_previous_lock =
               Some (Project_fixture.metadata (Chatml_project_build.lock_source build))
           ; generated_lock =
               Support.id
                 ~namespace:(Chatml_source.Id.package_namespace document_source)
                 "next.lock.json"
           })
  in
  let script =
    B.Manifest.Script.create ~selector ~choice:(Selected_project project) |> ok
  in
  let document_member source member =
    B.Manifest.Document_member.create
      ~source:(Chatml_source.id source)
      ~member:(Source.Portable_path.create member |> Or_error.ok_exn)
    |> ok
  in
  let basis =
    B.Manifest.Chatmd_document.create
      ~root_source:document_source
      ~document_members:
        [ document_member document "selected.chatmd"
        ; document_member declaration "chatml.project.json"
        ]
      ~scripts:[ script ]
    |> ok
  in
  let sources = [ document; declaration ] in
  let manifest =
    B.Manifest.create
      ~basis:(Chatmd_document basis)
      ~members:
        (List.map sources ~f:(fun source ->
           B.Member.create ~source:(Project_fixture.metadata source) |> ok))
    |> ok
  in
  manifest, sources
;;

let stage_project ?parent transport ~key ~manifest ~sources =
  let begin_key = P.Idempotency_key.of_string (key ^ "-begin") |> ok in
  let upload_id =
    match
      authoring
        transport
        (Bundle_begin
           (B.Begin.create_request
              ?parent_context:parent
              ~idempotency_key:begin_key
              ~manifest
              ()))
      |> ok
    with
    | Bundle_begin response -> B.Begin.upload_id response
    | _ -> failwith "configured Project upload did not begin"
  in
  List.iter sources ~f:(fun source ->
    let chunk =
      B.Chunk.create_request
        ~upload_id
        ~source:(Chatml_source.id source)
        ~byte_offset:0
        ~bytes:(Chatml_source.contents source)
      |> ok
    in
    match authoring transport (Bundle_chunk chunk) |> ok with
    | Bundle_chunk _ -> ()
    | _ -> failwith "configured Project member was not accepted");
  B.Commit.create_request
    ~upload_id
    ~idempotency_key:(P.Idempotency_key.of_string (key ^ "-commit") |> ok)
;;

let publish_project ?parent transport ~key ~manifest ~sources =
  let commit = stage_project ?parent transport ~key ~manifest ~sources in
  match authoring transport (Bundle_commit commit) |> ok with
  | Bundle_commit response ->
    (match B.Commit.outcome response with
     | Published receipt -> receipt
     | Accepted_for_validation | Rejected _ ->
       failwith "configured Project was not durably published")
  | _ -> failwith "configured Project commit returned another method"
;;

let%expect_test
    "default daemon installs exact supplied authoring methods and revokes \
     reinitialization"
  =
  with_daemon (fun _ _ _ daemon ->
    let _, transport = connect daemon in
    let features =
      [ P.Authoring.Features.core
      ; P.Authoring.Features.lexical
      ; P.Authoring.Features.assistance
      ; P.Authoring.Features.bundle
      ; P.Authoring.Features.semantic
      ]
    in
    let enabled = (initialize transport features).enabled_features in
    assert (List.mem enabled P.Authoring.Features.core ~equal:String.equal);
    assert (List.mem enabled P.Authoring.Features.lexical ~equal:String.equal);
    assert (List.mem enabled P.Authoring.Features.assistance ~equal:String.equal);
    assert (not (List.mem enabled P.Authoring.Features.bundle ~equal:String.equal));
    assert (not (List.mem enabled P.Authoring.Features.semantic ~equal:String.equal));
    let server_features =
      match Agent_client.Connection.request_without_history transport Server_info with
      | Ok (Server_info info) -> info.features
      | Ok _ -> failwith "unexpected server-info result"
      | Error failure -> raise_s [%sexp (failure : P.Error.t)]
    in
    assert (List.mem server_features P.Authoring.Features.core ~equal:String.equal);
    assert (List.mem server_features P.Authoring.Features.lexical ~equal:String.equal);
    assert (not (List.mem server_features P.Authoring.Features.bundle ~equal:String.equal));
    assert (
      not (List.mem server_features P.Authoring.Features.semantic ~equal:String.equal));
    let capability =
      match authoring transport P.Authoring.Wire.Request.Capabilities |> ok with
      | Capabilities value -> value
      | _ -> failwith "expected capabilities"
    in
    assert (P.Authoring.Capabilities.supports capability Document_replace);
    assert (P.Authoring.Capabilities.supports capability Lexical);
    assert (not (P.Authoring.Capabilities.supports capability Roots));
    assert (not (P.Authoring.Capabilities.supports capability Bundle_begin));
    (match authoring transport P.Authoring.Wire.Request.Roots with
     | Error failure -> assert (P.Error.equal_code failure.code Method_not_found)
     | Ok _ -> failwith "uninstalled native roots were callable");
    let request =
      P.Authoring.Context.Open_request.create_supplied_document
        ~member:"draft.chatmd"
        ~default_environment:None
        ~contents:"<script>let x = 1</script>"
      |> ok
    in
    let opened =
      match authoring transport (Context_open request) |> ok with
      | Context_open context -> context
      | _ -> failwith "expected supplied context"
    in
    let next = initialize transport [ P.Authoring.Features.bundle ] in
    assert (
      not (List.mem next.enabled_features P.Authoring.Features.bundle ~equal:String.equal));
    ignore
      (initialize transport [ P.Authoring.Features.core; P.Authoring.Features.lexical ]);
    let current_capability =
      match authoring transport P.Authoring.Wire.Request.Capabilities |> ok with
      | Capabilities value -> value
      | _ -> failwith "expected current capabilities"
    in
    assert (P.Authoring.Capabilities.supports current_capability Lexical);
    assert (not (P.Authoring.Capabilities.supports current_capability Assistance));
    let fresh =
      match authoring transport (Context_open request) |> ok with
      | Context_open context -> context
      | _ -> failwith "expected fresh supplied context"
    in
    let list context =
      authoring
        transport
        (Source_list
           (P.Authoring.Source_read.List_request.create
              ~context:(P.Authoring.Context.Expected.of_view context)
              ~package_namespace:None
              ~directory:None
            |> ok))
    in
    (match list fresh |> ok with
     | Source_list _ -> ()
     | _ -> failwith "new context source was not readable");
    (match list opened with
     | Error failure -> assert (P.Error.equal_code failure.code Conflict)
     | Ok _ -> failwith "reinitialize retained the old context");
    print_endline "installed methods, supplied context, and reinitialize revocation: ok");
  [%expect {| installed methods, supplied context, and reinitialize revocation: ok |}]
;;

let%expect_test
    "upload-only daemon advertises only bundle handlers and no position encoding"
  =
  let upload =
    S.Authoring_installation.Host_policy.Upload.create
      ~limits:(upload_limits ())
      ~grant:
        (S.Authoring_upload_host.Upload_policy.create
           ~authorize_write:Operator_authorization.is_current
           ~authorize_publish:Operator_authorization.is_current
           ~authorize_parent:None)
      ~publication:None
      ~rebind:None
  in
  let policy =
    S.Authoring_installation.Host_policy.upload_only
      ~initialize_limits:S.Daemon.default_options.protocol_limits
      ~upload
    |> ok
  in
  with_daemon
    ~options:
      { S.Daemon.default_options with
        startup_mode = Operator_only
      ; authoring = Some policy
      }
    (fun _ _ _ daemon ->
       let _, transport = connect daemon in
       let response =
         initialize transport [ P.Authoring.Features.core; P.Authoring.Features.bundle ]
       in
       assert (
         not
           (List.mem
              response.enabled_features
              P.Authoring.Features.core
              ~equal:String.equal));
       assert (
         List.mem
           response.enabled_features
           P.Authoring.Features.bundle
           ~equal:String.equal);
       let capability =
         match authoring transport P.Authoring.Wire.Request.Capabilities |> ok with
         | Capabilities value -> value
         | _ -> failwith "expected capabilities"
       in
       assert (List.is_empty capability.encodings);
       assert (P.Authoring.Capabilities.supports capability Bundle_begin);
       assert (not (P.Authoring.Capabilities.supports capability Context_open));
       assert (not (P.Authoring.Capabilities.supports capability Lexical));
       (match authoring transport P.Authoring.Wire.Request.Roots with
        | Error failure -> assert (P.Error.equal_code failure.code Method_not_found)
        | Ok _ -> failwith "upload-only daemon exposed native roots");
       print_endline "bundle only, no source positions: ok");
  [%expect {| bundle only, no source positions: ok |}]
;;

let%expect_test
    "configured upload-only daemon publishes a parentless captured document under exact \
     grants"
  =
  let access =
    configured_upload_access
      S.Authoring_upload_access_configuration.Upload_only
      [ Upload_write; Publish; Bundle_read; Rebind ]
  in
  with_daemon
    ~configure:(fun ~env:_ ~root:_ config ->
      { config with
        server = { config.server with authoring_upload_access = Some access }
      })
    (fun _ _ _ daemon ->
       let _, transport = connect daemon in
       let enabled =
         (initialize transport [ P.Authoring.Features.core; P.Authoring.Features.bundle ])
           .enabled_features
       in
       assert (List.mem enabled P.Authoring.Features.bundle ~equal:String.equal);
       assert (not (List.mem enabled P.Authoring.Features.core ~equal:String.equal));
       ignore (publish_document transport ~key:"configured-upload" : B.Commit.request);
       let foreign =
         Operator_authorization.trusted_local
           (Agent_server_test_support.principal_with_id "pri_upload_foreign")
       in
       let _, denied = connect ~actor:foreign daemon in
       ignore
         (initialize denied [ P.Authoring.Features.bundle ] : P.Initialize.Response.t);
       let _, _, manifest = upload_document () in
       let begin_request =
         B.Begin.create_request
           ~idempotency_key:(P.Idempotency_key.of_string "foreign-upload" |> ok)
           ~manifest
           ()
       in
       (match authoring denied (Bundle_begin begin_request) with
        | Error failure -> assert (P.Error.equal_code failure.code Permission_denied)
        | Ok _ -> failwith "foreign principal received configured write grant");
       print_endline "configured bundle publication and principal denial: ok");
  [%expect {| configured bundle publication and principal denial: ok |}]
;;

let%expect_test "configured Project upload cannot rebind with only a bundle-read grant" =
  let manifest, sources, environment = project_upload () in
  let base =
    configured_upload_access
      S.Authoring_upload_access_configuration.Augment_core
      [ Upload_write; Publish; Bundle_read ]
  in
  let access =
    S.Authoring_upload_access_configuration.
      { base with
        selections =
          [ Selection.
              { principal_id = (Agent_server_test_support.principal ()).id
              ; authentication_kind = "test"
              ; request =
                  S.Authoring_project_access_configuration.Selection.Entry
                    { name = "main"; environment = None }
              ; selected_environment = environment
              ; target = One_off_v1
              }
          ]
      }
  in
  with_daemon
    ~configure:(fun ~env:_ ~root:_ config ->
      { config with
        server = { config.server with authoring_upload_access = Some access }
      })
    (fun _ _ _ daemon ->
       let _, transport = connect daemon in
       ignore
         (initialize transport [ P.Authoring.Features.core; P.Authoring.Features.bundle ]
          : P.Initialize.Response.t);
       let receipt =
         publish_project transport ~key:"project-no-rebind" ~manifest ~sources
       in
       let request =
         P.Authoring.Context.Open_request.create_uploaded_project
           (B.Receipt.bundle_id receipt)
       in
       match authoring transport (Context_open request) with
       | Error failure -> assert (P.Error.equal_code failure.code Permission_denied)
       | Ok _ -> failwith "bundle read silently granted Project rebind");
  print_endline "bundle read alone cannot rebind";
  [%expect {| bundle read alone cannot rebind |}]
;;

let%expect_test "installed selected Document parent supplies only its current exact prior"
  =
  let module Service = S.Authoring_context_service in
  let module Policy = S.Authoring_installation.Host_policy in
  let selected = ref None in
  let selected_now = ref true in
  let script_selection =
    Service.Semantic.Script_selection.Policy.create
      ~select_script:
        (fun
          ~original_actor ~actor ~current:_ ~parsed:_ ~declaration ~metadata:_ ->
        if
          !selected_now
          && Operator_authorization.is_current original_actor
          && Operator_authorization.is_current actor
          && String.equal (Prompt.Chat_markdown.Declaration.script_id declaration) "s"
        then !selected
        else None)
  in
  let query_limits =
    Service.Semantic.Query_limits.create
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
    |> Or_error.ok_exn
  in
  let base =
    Policy.basic_supplied ~initialize_limits:S.Daemon.default_options.protocol_limits
    |> ok
  in
  let access =
    configured_upload_access
      S.Authoring_upload_access_configuration.Augment_core
      [ Upload_write; Publish; Bundle_read; Rebind ]
  in
  let access =
    S.Authoring_upload_access_configuration.
      { access with
        selections =
          [ Selection.
              { principal_id = (Agent_server_test_support.principal ()).id
              ; authentication_kind = "test"
              ; request =
                  S.Authoring_project_access_configuration.Selection.Library
                    { environment = None }
              ; selected_environment = "default"
              ; target = One_off_v1
              }
          ]
      }
  in
  let document_bytes =
    "<script id=\"s\" language=\"chatml\" kind=\"tool\" \
     project=\"./chatml.project.json\">let main input = Task.pure(input)\n\
     </script>"
  in
  with_daemon
    ~prepare_options:(fun ~env options ->
      let semantic =
        Policy.Semantic.create
          ~script_selection:(Some script_selection)
          ~project_metadata:None
          ~project_build_limits:
            (Chatml_project_build.Limits.create
               ~max_work_steps:100_000
               ~max_input_bytes:(Some 4096)
               ~document:Document_schema.Limits.default
             |> Or_error.ok_exn)
          ~limits:
            (Service.Semantic.Limits.create
               ~max_document_bundle_files:4
               ~max_document_bundle_bytes:2048
               ~max_parser_work_steps:100_000
               ~max_diagnostic_work_steps:100_000
               ~max_selection_rows:4
               ~max_retained_selection_source_bytes:(4 * 1024 * 1024)
               ~max_retained_selection_input_bytes:(4 * 1024 * 1024)
               ~max_selection_work_steps:1_000_000
             |> Or_error.ok_exn)
          ~prefix_limits:
            (Service.Semantic.Prefix_limits.create
               ~max_document_bundle_files:4
               ~max_document_bundle_bytes:2048
               ~max_parser_work_steps:100_000
               ~max_host_work_steps:100_000
             |> Or_error.ok_exn)
          ~query_limits
          ~document_dir:(Eio.Stdenv.cwd env)
      in
      { options with authoring = Some (Policy.with_semantic base ~semantic |> ok) })
    ~configure:(fun ~env:_ ~root:_ config ->
      { config with
        server = { config.server with authoring_upload_access = Some access }
      })
    (fun env _ _ daemon ->
       let _, transport = connect daemon in
       Fun.protect
         ~finally:(fun () -> Agent_client.Connection.close transport)
         (fun () ->
            let enabled =
              (initialize
                 transport
                 [ P.Authoring.Features.core
                 ; P.Authoring.Features.semantic
                 ; P.Authoring.Features.bundle
                 ])
                .enabled_features
            in
            assert (List.mem enabled P.Authoring.Features.semantic ~equal:String.equal);
            let opened =
              match
                authoring
                  transport
                  (Context_open
                     (P.Authoring.Context.Open_request.create_supplied_document
                        ~member:"selected.chatmd"
                        ~default_environment:None
                        ~contents:document_bytes
                      |> ok))
                |> ok
              with
              | Context_open context -> context
              | _ -> failwith "selected Document did not open"
            in
            let expected = P.Authoring.Context.Expected.of_view opened in
            ignore
              (publish_document ~parent:expected transport ~key:"document-before-analyze"
               : B.Commit.request);
            let document_source =
              match
                authoring
                  transport
                  (Source_list
                     (P.Authoring.Source_read.List_request.create
                        ~context:expected
                        ~package_namespace:None
                        ~directory:None
                      |> ok))
                |> ok
              with
              | Source_list result ->
                P.Authoring.Source.id
                  (List.find_exn
                     (P.Authoring.Source_read.List_result.sources result)
                     ~f:(fun source ->
                       String.equal
                         (Chatml_source.Id.relative_path (P.Authoring.Source.id source))
                         "selected.chatmd"))
              | _ -> failwith "selected Document source inventory unavailable"
            in
            let build, contract, declaration, entry, environment =
              selected_document_project
                ~namespace:(Chatml_source.Id.package_namespace document_source)
                ()
            in
            let admitted =
              Service.Semantic.Script_selection.Selected.admit
                ~build
                ~contract
                ~limits:
                  (Service.Semantic.Script_selection.Limits.create
                     ~max_identity_bytes:(4 * 1024 * 1024)
                     ~max_work_steps:100_000
                   |> Or_error.ok_exn)
                ~checkpoint:Fn.id
              |> function
              | Ok selected -> selected
              | Error _ -> failwith "actual selected Document Build was not admitted"
            in
            assert (String.equal environment "default");
            selected := Some admitted;
            (match
               authoring
                 transport
                 (Analyze
                    (P.Authoring.Analysis.Analyze.Request.create
                       ~context:expected
                       ~target:(P.Authoring.Analysis.Target.selected_script "s" |> ok)
                     |> ok))
               |> ok
             with
             | Analyze (Complete _) -> ()
             | _ -> failwith "selected Document did not retain a checked Build");
            let manifest, sources =
              selected_document_upload
                ~dir:(Eio.Stdenv.cwd env)
                ~document_source
                ~document_bytes
                ~build
                ~declaration
                ~entry
            in
            let selected_commit =
              stage_project
                ~parent:expected
                transport
                ~key:"selected-document-prior"
                ~manifest
                ~sources
            in
            (match authoring transport (Bundle_commit selected_commit) |> ok with
             | Bundle_commit response ->
               (match B.Commit.outcome response with
                | Published _ -> ()
                | outcome ->
                  failwith
                    ("selected Document prior was not published: "
                     ^ Sexp.to_string_hum (B.Commit.sexp_of_outcome outcome)))
             | _ -> failwith "selected Document commit response changed");
            let revoked =
              stage_project
                ~parent:expected
                transport
                ~key:"revoked-document-prior"
                ~manifest
                ~sources
            in
            selected_now := false;
            match authoring transport (Bundle_commit revoked) with
            | Error failure when P.Error.equal_code failure.code Invalid_request -> ()
            | Ok _ | Error _ ->
              failwith "revoked selected Document Build supplied prior authority"));
  print_endline "selected Document exact prior and policy revocation: ok";
  [%expect {| selected Document exact prior and policy revocation: ok |}]
;;

let%expect_test
    "installed authoring success and method rejection share local, HTTP and stdio \
     dispatch"
  =
  with_daemon (fun env sw root daemon ->
    let exercise connection =
      ignore
        (initialize connection [ P.Authoring.Features.core; P.Authoring.Features.lexical ]
         : P.Initialize.Response.t);
      let capability =
        match authoring connection P.Authoring.Wire.Request.Capabilities |> ok with
        | Capabilities value -> value
        | _ -> failwith "expected capabilities"
      in
      assert (P.Authoring.Capabilities.supports capability Context_open);
      assert (P.Authoring.Capabilities.supports capability Lexical);
      assert (not (P.Authoring.Capabilities.supports capability Roots));
      let request =
        P.Authoring.Context.Open_request.create_supplied_document
          ~member:"transport.chatmd"
          ~default_environment:None
          ~contents:"<script>let x = 1</script>"
        |> ok
      in
      (match authoring connection (Context_open request) |> ok with
       | Context_open _ -> ()
       | _ -> failwith "supplied context did not open");
      match authoring connection P.Authoring.Wire.Request.Roots with
      | Error failure -> assert (P.Error.equal_code failure.code Method_not_found)
      | Ok _ -> failwith "uninstalled roots were callable"
    in
    let _, local = connect daemon in
    Fun.protect
      ~finally:(fun () -> Agent_client.Connection.close local)
      (fun () -> exercise local);
    let principal = Agent_server_test_support.principal () in
    let http =
      Agent_server_wire_fixture.http_connector ~sw ~env ~daemon ~root ~principal ()
    in
    Fun.protect
      ~finally:(fun () -> Agent_client.Connection.close http)
      (fun () -> exercise http);
    let socket_path = Filename.concat root "authoring.sock" in
    let stdio = Agent_server_wire_fixture.connect_stdio ~sw ~env ~daemon ~socket_path in
    Fun.protect
      ~finally:(fun () -> Agent_client.Connection.close stdio)
      (fun () -> exercise stdio);
    print_endline "local, HTTP, stdio: same installed success and rejection");
  [%expect {| local, HTTP, stdio: same installed success and rejection |}]
;;

let%expect_test
    "configured native Project and document semantics use the installed grant on each \
     transport"
  =
  let local = Agent_server_test_support.principal () in
  let http_principal =
    Agent_server_test_support.principal_with_id "pri_authoring_native_http"
  in
  let moderator =
    Chatml.Chatml_compiler_contract.for_target Moderator_v1 ~checkpoint:ignore
  in
  let conversion_bytes = ref 0 in
  let conversion_steps = ref 0 in
  let moderator_bytes =
    Chatml.Chatml_compiler_contract.input_bytes
      moderator
      ~checkpoint:(fun () -> incr conversion_steps)
      ~allocate:(fun bytes -> conversion_bytes := !conversion_bytes + bytes)
  in
  assert (moderator_bytes < 512 * 1024);
  assert (!conversion_bytes < 16 * 1024 * 1024);
  assert (!conversion_steps < 1_000_000);
  let all_targets =
    Chatml.Chatml_compiler_contract.Target.
      [ One_off_v1; Tool_v1; Developer_tool_v1; Moderator_v1; Delegated_moderator_v1 ]
  in
  let all_allocated = ref 0 in
  let all_steps = ref 0 in
  List.iter all_targets ~f:(fun target ->
    let contract = Chatml.Chatml_compiler_contract.for_target target ~checkpoint:ignore in
    ignore
      (Chatml.Chatml_compiler_contract.input_bytes
         contract
         ~checkpoint:(fun () -> incr all_steps)
         ~allocate:(fun bytes -> all_allocated := !all_allocated + bytes)
       : int));
  assert (!all_allocated < 40 * 1024 * 1024);
  assert (!all_steps < 1_000_000);
  let peer_id =
    Digestif.SHA256.(
      digest_string
        ("unix-peer:v1:"
         ^ Int.to_string (Agent_transport_socket.Peer_credentials.effective_uid ()))
      |> to_hex)
    |> fun digest -> P.Id.Principal.of_string ("pri_" ^ digest) |> ok
  in
  let _, _, upload_environment, _ = moderator_upload () in
  let configure ~env ~root (config : S.Config.t) =
    let save name contents =
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(Eio.Stdenv.fs env / root / name)
        contents
    in
    save
      "chatml.project.json"
      {|{"schema_version":1,"namespace":"first","entries":{"main":{"source":"main.chatml","target":"one_off_v1"}}}|};
    save
      "second.project.json"
      {|{"schema_version":1,"namespace":"second","environments":{"other":{"modules":{}}},"entries":{"main":{"source":"second.chatml","target":"one_off_v1"}}}|};
    save
      "moderator.project.json"
      {|{"schema_version":1,"namespace":"moderator","entries":{"main":{"source":"moderator.chatml","target":"moderator_v1"}}}|};
    save
      "tool.project.json"
      {|{"schema_version":1,"namespace":"tool","entries":{"main":{"source":"tool.chatml","target":"tool_v1"}}}|};
    save
      "developer-tool.project.json"
      {|{"schema_version":1,"namespace":"developer","entries":{"main":{"source":"developer.chatml","target":"developer_tool_v1"}}}|};
    save
      "delegated.project.json"
      {|{"schema_version":1,"namespace":"delegated","entries":{"main":{"source":"delegated.chatml","target":"delegated_moderator_v1"}}}|};
    save "main.chatml" "let main input = Task.pure(input)\n";
    save "second.chatml" "let main input = Task.pure(input)\n";
    save
      "moderator.chatml"
      "let initial_state = 0\nlet on_event ctx state event = Task.pure(state)\n";
    save
      "draft.chatmd"
      "<script id=\"s\" language=\"chatml\" kind=\"tool\" \
       project=\"./chatml.project.json\">let main input = Task.pure(input)\n\
       </script>\n\
       <script id=\"t\" language=\"chatml\" kind=\"tool\" \
       project=\"./second.project.json\" environment=\"other\">let main input = \
       Task.pure(input)\n\
       </script>";
    let grant principal_id authentication_kind =
      S.Authoring_project_access_configuration.Read_grant.
        { root; principal_id; authentication_kind }
    in
    let selected
          ?(target = Chatml.Chatml_compiler_contract.Target.One_off_v1)
          project
          request
      =
      S.Authoring_project_access_configuration.Selection.
        { root; project; request; target }
    in
    let upload_grants principal_id authentication_kind =
      List.map
        S.Authoring_upload_access_configuration.Grant.
          [ Upload_write; Publish; Bundle_read; Rebind ]
        ~f:(fun operation ->
          S.Authoring_upload_access_configuration.Grant.
            { principal_id; authentication_kind; operation })
    in
    { config with
      server =
        { config.server with
          project_sources =
            Some S.Project_source_configuration.{ external_roots = [ root ]; git = None }
        ; authoring_project_access =
            Some
              S.Authoring_project_access_configuration.
                { read_grants =
                    [ grant local.id "test"
                    ; grant http_principal.id "http.static_bearer"
                    ; grant peer_id "unix.peer"
                    ]
                ; selections =
                    [ selected
                        "chatml.project.json"
                        (Entry { name = "main"; environment = None })
                    ; selected "chatml.project.json" (Library { environment = None })
                    ; selected
                        "second.project.json"
                        (Entry { name = "main"; environment = Some "other" })
                    ; selected
                        "second.project.json"
                        (Library { environment = Some "other" })
                    ; selected
                        ~target:Moderator_v1
                        "moderator.project.json"
                        (Entry { name = "main"; environment = None })
                    ; selected
                        ~target:Tool_v1
                        "tool.project.json"
                        (Entry { name = "main"; environment = None })
                    ; selected
                        ~target:Developer_tool_v1
                        "developer-tool.project.json"
                        (Entry { name = "main"; environment = None })
                    ; selected
                        ~target:Delegated_moderator_v1
                        "delegated.project.json"
                        (Entry { name = "main"; environment = None })
                    ]
                }
        ; authoring_upload_access =
            Some
              S.Authoring_upload_access_configuration.
                { mode = Augment_core
                ; grants =
                    upload_grants local.id "test"
                    @ upload_grants http_principal.id "http.static_bearer"
                    @ upload_grants peer_id "unix.peer"
                ; selections =
                    [ S.Authoring_upload_access_configuration.Selection.
                        { principal_id = local.id
                        ; authentication_kind = "test"
                        ; request =
                            S.Authoring_project_access_configuration.Selection.Library
                              { environment = None }
                        ; selected_environment = upload_environment
                        ; target = Moderator_v1
                        }
                    ]
                }
        }
    }
  in
  with_daemon ~configure (fun env sw root daemon ->
    let exercise connection ~refresh =
      let enabled =
        (initialize
           connection
           [ P.Authoring.Features.core
           ; P.Authoring.Features.semantic
           ; P.Authoring.Features.lexical
           ; P.Authoring.Features.assistance
           ; P.Authoring.Features.bundle
           ])
          .enabled_features
      in
      assert (List.mem enabled P.Authoring.Features.semantic ~equal:String.equal);
      assert (List.mem enabled P.Authoring.Features.bundle ~equal:String.equal);
      let capability =
        match authoring connection P.Authoring.Wire.Request.Capabilities |> ok with
        | Capabilities value -> value
        | _ -> failwith "configured native capabilities were absent"
      in
      assert (P.Authoring.Capabilities.supports capability Roots);
      assert (P.Authoring.Capabilities.supports capability Analyze);
      assert (P.Authoring.Capabilities.supports capability Bundle_begin);
      let source_root =
        match authoring connection P.Authoring.Wire.Request.Roots |> ok with
        | Roots roots -> (List.hd_exn roots.roots).id
        | _ -> failwith "configured native root was absent"
      in
      let project ?(checked_template = false) project_member environment =
        let selection =
          P.Authoring.Context.Selection.create ~target:(Entry "main") ~environment |> ok
        in
        let opened =
          match
            authoring
              connection
              (Context_open
                 (P.Authoring.Context.Open_request.create_host_project
                    ~root:source_root
                    ~dependency_roots:[]
                    ~project_member
                    ~selection
                  |> ok))
            |> ok
          with
          | Context_open context -> context
          | _ -> failwith "configured project context did not open"
        in
        let capture =
          match
            authoring
              connection
              (Analyze
                 (let request =
                    P.Authoring.Analysis.Analyze.Request.create
                      ~context:(P.Authoring.Context.Expected.of_view opened)
                      ~target:P.Authoring.Analysis.Target.project
                    |> ok
                  in
                  if checked_template
                  then P.Authoring.Analysis.Analyze.Request.with_checked_template request
                  else request))
            |> ok
          with
          | Analyze (Complete complete) -> complete.capture
          | Analyze result ->
            failwith
              ("configured project was not checked: "
               ^ Sexp.to_string_hum (P.Authoring.Analysis.Analyze.sexp_of_t result))
          | _ -> failwith "configured project response changed"
        in
        opened, capture
      in
      let first_context, first_capture =
        project ~checked_template:true "chatml.project.json" None
      in
      ignore (project "second.project.json" (Some "other"));
      ignore (project "moderator.project.json" None);
      if refresh
      then (
        let expected = P.Authoring.Context.Expected.of_view first_context in
        let _, other_connection = connect daemon in
        Fun.protect
          ~finally:(fun () -> Agent_client.Connection.close other_connection)
          (fun () ->
             ignore
               (initialize
                  other_connection
                  [ P.Authoring.Features.core; P.Authoring.Features.bundle ]
                : P.Initialize.Response.t);
             let _, _, foreign_manifest = upload_document () in
             let foreign_begin =
               B.Begin.create_request
                 ~parent_context:expected
                 ~idempotency_key:(P.Idempotency_key.of_string "foreign-parent" |> ok)
                 ~manifest:foreign_manifest
                 ()
             in
             match authoring other_connection (Bundle_begin foreign_begin) with
             | Error failure when P.Error.equal_code failure.code Conflict -> ()
             | Ok _ | Error _ ->
               failwith "different connection borrowed another parent's context");
        let parent_commit =
          publish_document ~parent:expected connection ~key:"native-parent"
        in
        let script_source =
          match
            authoring
              connection
              (Source_list
                 (P.Authoring.Source_read.List_request.create
                    ~context:expected
                    ~package_namespace:None
                    ~directory:None
                  |> ok))
            |> ok
          with
          | Source_list result ->
            List.find_exn
              (P.Authoring.Source_read.List_result.sources result)
              ~f:(fun source ->
                String.equal
                  (Chatml_source.Id.relative_path (P.Authoring.Source.id source))
                  "main.chatml")
          | _ -> failwith "installed project source inventory unavailable"
        in
        let prefix =
          match
            authoring
              connection
              (Prefix
                 (P.Authoring.Analysis.Prefix.Request.create
                    ~context:expected
                    ~target:P.Authoring.Analysis.Target.project
                    ~source:script_source
                    ~cursor_byte:(P.Authoring.Source.byte_length script_source)
                  |> ok))
            |> ok
          with
          | Prefix (Provisional result) -> result
          | Prefix _ | _ -> failwith "installed native Prefix unavailable"
        in
        let query =
          Q.Request.create
            ~evidence:(Q.Provisional prefix)
            ~source:(P.Authoring.Analysis.Prefix.prefix_source prefix)
            ~byte_offset:(P.Authoring.Analysis.Prefix.end_byte prefix)
            ~kind:(Q.Candidates Value)
            ~max_items:1
          |> ok
        in
        (match authoring connection (Query query) |> ok with
         | Query result ->
           (match Q.payload result with
            | Candidates_result (Selected page) when not (List.is_empty page.items) -> ()
            | _ -> failwith "installed native Query lacked selected candidates")
         | _ -> failwith "installed native Query response changed");
        let assistance_request =
          P.Authoring_assistance.Assistance.Request.create
            ~context:expected
            ~source:script_source
            ~cursor_byte:4
            ~max_items:1
          |> ok
        in
        (match authoring connection (Assistance assistance_request) |> ok with
         | Assistance result ->
           (match P.Authoring_assistance.Assistance.Result.items result with
            | [ item ] ->
              (match P.Authoring_assistance.Assistance.Suggestion.evidence item with
               | Checked_template checked ->
                 assert (
                   P.Authoring_assistance.Assistance.Checked_template.equal_target
                     (P.Authoring_assistance.Assistance.Checked_template.target checked)
                     One_off_v1)
               | _ -> failwith "installed native checked-template evidence absent")
            | _ -> failwith "installed native checked-template item absent")
         | _ -> failwith "installed native assistance response changed");
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          Eio.Path.(Eio.Stdenv.fs env / root / "main.chatml")
          "let main input = Task.pure(input)\n\n";
        let old = P.Authoring.Context.Expected.of_view first_context in
        let next =
          match
            authoring
              connection
              (Context_refresh
                 (P.Authoring.Context.Refresh_request.create
                    ~expected:old
                    ~policy:Reject_open_documents))
            |> ok
          with
          | Context_refresh context -> context
          | _ -> failwith "configured source refresh did not publish"
        in
        let next_expected = P.Authoring.Context.Expected.of_view next in
        (match authoring connection (Bundle_commit parent_commit) with
         | Error failure when P.Error.equal_code failure.code Conflict -> ()
         | Ok _ | Error _ -> failwith "persisted parent survived native context refresh");
        (match
           authoring
             connection
             (Analyze
                (P.Authoring.Analysis.Analyze.Request.create
                   ~context:old
                   ~target:P.Authoring.Analysis.Target.project
                 |> ok))
         with
         | Error failure when P.Error.equal_code failure.code Conflict -> ()
         | Ok _ | Error _ -> failwith "old configured source survived refresh");
        let second_capture =
          match
            authoring
              connection
              (Analyze
                 (P.Authoring.Analysis.Analyze.Request.create
                    ~context:next_expected
                    ~target:P.Authoring.Analysis.Target.project
                  |> ok))
            |> ok
          with
          | Analyze (Complete complete) -> complete.capture
          | _ -> failwith "refreshed source was not checked"
        in
        assert (
          not
            (Chatml_source.Digest.equal
               (P.Authoring.Analysis.Capture.input_fingerprint first_capture)
               (P.Authoring.Analysis.Capture.input_fingerprint second_capture))));
      let document =
        match
          authoring
            connection
            (Context_open
               (P.Authoring.Context.Open_request.create_host_document
                  ~root:source_root
                  ~member:"draft.chatmd"
                  ~default_environment:None
                |> ok))
          |> ok
        with
        | Context_open context -> context
        | _ -> failwith "configured host document did not open"
      in
      let context = P.Authoring.Context.Expected.of_view document in
      List.iter [ "s"; "t" ] ~f:(fun script ->
        match
          authoring
            connection
            (Analyze
               (P.Authoring.Analysis.Analyze.Request.create
                  ~context
                  ~target:(P.Authoring.Analysis.Target.selected_script script |> ok)
                |> ok))
        with
        | Ok (Analyze (Complete _)) -> ()
        | Ok (Analyze (Unavailable { reason; _ })) ->
          failwith
            ("configured document "
             ^ script
             ^ " unavailable: "
             ^ Sexp.to_string_hum
                 (P.Authoring.Analysis.Analyze.sexp_of_unavailable reason))
        | Ok (Analyze result) ->
          failwith
            ("configured document "
             ^ script
             ^ " was not checked: "
             ^ Sexp.to_string_hum (P.Authoring.Analysis.Analyze.sexp_of_t result))
        | Error failure ->
          failwith ("configured document " ^ script ^ " failed: " ^ failure.message)
        | Ok _ -> failwith "configured document response changed")
    in
    let _, local_connection = connect daemon in
    Fun.protect
      ~finally:(fun () -> Agent_client.Connection.close local_connection)
      (fun () ->
         exercise local_connection ~refresh:true;
         let manifest, sources, _, original = moderator_upload () in
         let receipt =
           publish_project local_connection ~key:"native-project" ~manifest ~sources
         in
         let opened =
           match
             authoring
               local_connection
               (Context_open
                  (P.Authoring.Context.Open_request.create_uploaded_project
                     (B.Receipt.bundle_id receipt)))
             |> ok
           with
           | Context_open context -> context
           | _ -> failwith "configured Project rebind did not open"
         in
         let uploaded_expected = P.Authoring.Context.Expected.of_view opened in
         let previous_lock =
           match
             authoring
               local_connection
               (Source_list
                  (P.Authoring.Source_read.List_request.create
                     ~context:uploaded_expected
                     ~package_namespace:None
                     ~directory:None
                   |> ok))
             |> ok
           with
           | Source_list result ->
             List.find_exn
               (P.Authoring.Source_read.List_result.sources result)
               ~f:(fun source ->
                 String.equal
                   (Chatml_source.Id.relative_path (P.Authoring.Source.id source))
                   "installed.lock.json")
           | _ -> failwith "uploaded Project lock inventory unavailable"
         in
         let followup = moderator_followup original sources previous_lock in
         ignore
           (publish_project
              ~parent:uploaded_expected
              local_connection
              ~key:"native-prior"
              ~manifest:followup
              ~sources
            : B.Receipt.t);
         let wrong_prior =
           moderator_followup
             original
             sources
             (Chatml_project_build_tests.Authoring_bundle_tests.metadata
                (Chatml_project_build.lock_source original))
         in
         let wrong_commit =
           stage_project
             ~parent:uploaded_expected
             local_connection
             ~key:"wrong-native-prior"
             ~manifest:wrong_prior
             ~sources
         in
         (match authoring local_connection (Bundle_commit wrong_commit) with
          | Error failure when P.Error.equal_code failure.code Invalid_request -> ()
          | Ok _ | Error _ -> failwith "wrong prior lock acquired parent Build authority");
         match
           authoring
             local_connection
             (Analyze
                (P.Authoring.Analysis.Analyze.Request.create
                   ~context:(P.Authoring.Context.Expected.of_view opened)
                   ~target:P.Authoring.Analysis.Target.project
                 |> ok))
           |> ok
         with
         | Analyze (Complete _) -> ()
         | Analyze (Unavailable { reason; _ }) ->
           failwith
             ("configured rebound Project unavailable: "
              ^ Sexp.to_string_hum
                  (P.Authoring.Analysis.Analyze.sexp_of_unavailable reason))
         | Analyze (Failed { phase; _ }) ->
           failwith
             ("configured rebound Project failed: "
              ^ Sexp.to_string_hum
                  (P.Authoring.Analysis.Analyze.sexp_of_failure_phase phase))
         | _ -> failwith "configured rebound Project response changed");
    let http =
      Agent_server_wire_fixture.http_connector
        ~sw
        ~env
        ~daemon
        ~root
        ~principal:http_principal
        ()
    in
    Fun.protect
      ~finally:(fun () -> Agent_client.Connection.close http)
      (fun () ->
         exercise http ~refresh:false;
         ignore (publish_document http ~key:"native-http" : B.Commit.request));
    let stdio =
      Agent_server_wire_fixture.connect_stdio
        ~sw
        ~env
        ~daemon
        ~socket_path:(Filename.concat root "native-authoring.sock")
    in
    Fun.protect
      ~finally:(fun () -> Agent_client.Connection.close stdio)
      (fun () ->
         exercise stdio ~refresh:false;
         ignore (publish_document stdio ~key:"native-stdio" : B.Commit.request));
    let current = ref true in
    let actor =
      Operator_authorization.guarded ~principal:local ~is_current:(fun () -> !current)
    in
    let _, revoked = connect ~actor daemon in
    Fun.protect
      ~finally:(fun () -> Agent_client.Connection.close revoked)
      (fun () ->
         ignore
           (initialize
              revoked
              [ P.Authoring.Features.core; P.Authoring.Features.semantic ]
            : P.Initialize.Response.t);
         let root =
           match authoring revoked P.Authoring.Wire.Request.Roots |> ok with
           | Roots roots -> (List.hd_exn roots.roots).id
           | _ -> failwith "revocation setup lost its root"
         in
         let context =
           match
             authoring
               revoked
               (Context_open
                  (P.Authoring.Context.Open_request.create_host_project
                     ~root
                     ~dependency_roots:[]
                     ~project_member:"chatml.project.json"
                     ~selection:
                       (P.Authoring.Context.Selection.create
                          ~target:(Entry "main")
                          ~environment:None
                        |> ok)
                   |> ok))
             |> ok
           with
           | Context_open context -> P.Authoring.Context.Expected.of_view context
           | _ -> failwith "revocation setup did not open project"
         in
         current := false;
         match
           authoring
             revoked
             (Analyze
                (P.Authoring.Analysis.Analyze.Request.create
                   ~context
                   ~target:P.Authoring.Analysis.Target.project
                 |> ok))
         with
         | Error failure when P.Error.equal_code failure.code Permission_denied -> ()
         | Ok _ | Error _ -> failwith "revoked actor retained configured analysis");
    print_endline "configured native Project and two-script document: local, HTTP, stdio");
  [%expect {| configured native Project and two-script document: local, HTTP, stdio |}]
;;

let%expect_test "configured native grants reject an explicitly disabled authoring owner" =
  Eio_main.run (fun env ->
    let root =
      Filename.concat "/tmp" ("ochat-ai-" ^ P.Id.Transaction.(create () |> to_string))
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / root) in
    Eio.Path.mkdir ~perm:0o700 path;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path)
      ~f:(fun () ->
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(path / "prompt.chatmd")
          "<developer>Configured authoring startup.</developer>";
        let principal = Agent_server_test_support.principal () in
        let config =
          Agent_server_test_support.config
            root
            root
            (Filename.concat root "prompt.chatmd")
        in
        let config =
          { config with
            server =
              { config.server with
                project_sources =
                  Some
                    S.Project_source_configuration.
                      { external_roots = [ root ]; git = None }
              ; authoring_project_access =
                  Some
                    S.Authoring_project_access_configuration.
                      { read_grants =
                          [ { root
                            ; principal_id = principal.id
                            ; authentication_kind = "test"
                            }
                          ]
                      ; selections =
                          [ { root
                            ; project = "chatml.project.json"
                            ; request = Entry { name = "main"; environment = None }
                            ; target = One_off_v1
                            }
                          ]
                      }
              }
          }
        in
        Eio.Switch.run (fun sw ->
          let start_config config options =
            S.Daemon.start
              ~sw
              ~env
              ~config
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options
              ()
          in
          let start options = start_config config options in
          let invalid options =
            match start options with
            | Error failure when P.Error.equal_code failure.code Invalid_request -> ()
            | Error failure -> raise_s [%sexp (failure : P.Error.t)]
            | Ok daemon ->
              S.Daemon.shutdown daemon |> ok;
              failwith "invalid configured authoring startup was admitted"
          in
          let base = { S.Daemon.default_options with startup_mode = Operator_only } in
          let duplicate_grant =
            { config with
              server =
                { config.server with
                  authoring_project_access =
                    Option.map config.server.authoring_project_access ~f:(fun access ->
                      S.Authoring_project_access_configuration.
                        { access with
                          read_grants = access.read_grants @ access.read_grants
                        })
                }
            }
          in
          (match start_config duplicate_grant base with
           | Error failure when P.Error.equal_code failure.code Invalid_request -> ()
           | Error failure -> raise_s [%sexp (failure : P.Error.t)]
           | Ok daemon ->
             S.Daemon.shutdown daemon |> ok;
             failwith "direct invalid grant reached owner startup");
          invalid { base with authoring = None };
          invalid
            { base with
              protocol_limits = { base.protocol_limits with max_request_bytes = 65536 }
            };
          let daemon = start base |> ok in
          S.Daemon.shutdown daemon |> ok)));
  print_endline "configured grant conflicts and failed-start cleanup: typed";
  [%expect {| configured grant conflicts and failed-start cleanup: typed |}]
;;
