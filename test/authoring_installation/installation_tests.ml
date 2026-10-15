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
              ~options
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

let publish_document transport ~key =
  let id, contents, manifest = upload_document () in
  let begin_key = P.Idempotency_key.of_string (key ^ "-begin") |> ok in
  let begin_request = B.Begin.create_request ~idempotency_key:begin_key ~manifest () in
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
  match
    authoring transport (Bundle_status (B.Status.create_request ~upload_id)) |> ok
  with
  | Bundle_status status ->
    (match B.Status.state status with
     | Published _ -> ()
     | _ -> failwith "configured publication was not durable")
  | _ -> failwith "configured status returned another method"
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
      {|{"schema_version":1,"namespace":"role","source_dirs":["src"]}|}
  in
  let entry =
    Support.capture
      ~namespace:"role"
      "moderator.chatml"
      "let initial_state = 0 let on_event ctx state event = Task.pure(state)"
  in
  let catalog =
    Support.catalog declaration ~request:(Library { environment = None }) ~members:[]
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
  Project_fixture.manifest project sources, sources, environment
;;

let publish_project transport ~key ~manifest ~sources =
  let begin_key = P.Idempotency_key.of_string (key ^ "-begin") |> ok in
  let upload_id =
    match
      authoring
        transport
        (Bundle_begin (B.Begin.create_request ~idempotency_key:begin_key ~manifest ()))
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
  let commit =
    B.Commit.create_request
      ~upload_id
      ~idempotency_key:(P.Idempotency_key.of_string (key ^ "-commit") |> ok)
  in
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
       publish_document transport ~key:"configured-upload";
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
  let _, _, upload_environment = moderator_upload () in
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
                    ; grant local.id "http.static_bearer"
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
                    @ upload_grants local.id "http.static_bearer"
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
         publish_document local_connection ~key:"native-local";
         let manifest, sources, _ = moderator_upload () in
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
      Agent_server_wire_fixture.http_connector ~sw ~env ~daemon ~root ~principal:local ()
    in
    Fun.protect
      ~finally:(fun () -> Agent_client.Connection.close http)
      (fun () ->
         exercise http ~refresh:false;
         publish_document http ~key:"native-http");
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
         publish_document stdio ~key:"native-stdio");
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
