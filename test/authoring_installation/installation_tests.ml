open! Core
module P = Agent_protocol
module S = Agent_server

let () = Mirage_crypto_rng_unix.use_default ()

let ok = function
  | Ok value -> value
  | Error failure -> raise_s [%sexp (failure : P.Error.t)]
;;

let with_daemon
      ?(options = { S.Daemon.default_options with startup_mode = Operator_only })
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

let connect daemon =
  let actor =
    Operator_authorization.trusted_local (Agent_server_test_support.principal ())
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
           ~authorize_upload:Operator_authorization.is_current
           ~authorize_parent:(fun _ _ -> false))
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
